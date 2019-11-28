;;; helm-eshell --- A Helm source for switching eshells -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Cash

;; Author: James N. V. Cash <james.cash@occasionallycogent.com>
;; URL: https://github.com/jamesnvc/helm-eshell
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (helm "2.8.8"))
;; Version: 1.0
;; Keywords: helm, eshell

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; License

;; GPL 3.0+

;;; Commentary:

;; This package offers a helm-source for switching between eshells via
;; helm, sorting them by how their working directory is to your
;; current active directory.

;;; Code:

(require 'helm)
(require 'helm-lib)
(require 'cl-lib)

(defun helm-eshell--buffer-dir-name (buf)
  (pwd-replace-home (buffer-local-value 'default-directory buf)))

(defun helm-eshell--get-candidates ()
  (let* ((here (expand-file-name default-directory))
         (dist2here (lambda (d)
                      (let ((prefix (compare-strings
                                     here 0 nil
                                     (expand-file-name d) 0 nil)))
                        (->> (if (numberp prefix)
                                (- (abs prefix) 1)
                              (length d))
                            (substring here 0)
                            (s-split "/")
                            length
                            (+ (if (numberp prefix) 0 2))))))
         (eshells (cl-loop for buf in (buffer-list)
                           when (string-prefix-p "*eshell*" (buffer-name buf))
                           collect (cons (helm-eshell--buffer-dir-name buf) buf) into cands
                           finally return (-> cands
                                              (sort (lambda (a b) (< (length (car a)) (length (car b)))))
                                              (sort
                                               (lambda (a b) (> (funcall dist2here (car a))
                                                           (funcall dist2here (car b))))))))
         (new-dir (if (string-blank-p helm-input)
                      default-directory
                    helm-input))
         (new-eshell (cons (concat
                            (propertize
                             " " 'display
                             (propertize "[+]" 'font-lock-face
                                         '(:background "#ff69c6" :foreground "#282a36")))
                            " "
                            (pwd-replace-home new-dir))
                           new-dir)))
    (cons new-eshell eshells)))

(defun helm-eshell--move-to-first-real-candidate ()
  (let ((sel (helm-get-selection nil nil (helm-get-current-source))))
    (unless (bufferp sel)
      (helm-next-line))))

(defun helm-eshell--horiz-split (candidate)
  (if (bufferp candidate)
      (progn
        (select-window (split-window-below))
        (switch-to-buffer candidate))
    (let ((default-directory candidate))
      (select-window (split-window-below))
      (eshell t)
      (balance-windows))))

(defun helm-eshell--vert-split (candidate)
  (if (bufferp candidate)
      (progn
        (select-window (split-window-right))
        (switch-to-buffer candidate))
    (let ((default-directory candidate))
      (select-window (split-window-right))
      (eshell t)
      (balance-windows))))

(defun helm-eshell--horiz-split-command ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-eshell--horiz-split)))

(defun helm-eshell--vert-split-command ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-eshell--vert-split)))

(defconst helm-eshell-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-s") #'helm-eshell--horiz-split-command)
    (define-key map (kbd "C-v") #'helm-eshell--vert-split-command)
    map)
  "Keymap for `helm-eshell'.")

(defconst helm-eshell--source
  (helm-build-sync-source "eshell"
    :keymap helm-eshell-map
    :candidates #'helm-eshell--get-candidates
    :action (list
             (cons
              "Switch to eshell"
              (lambda (candidate)
                (if (bufferp candidate)
                    (switch-to-buffer candidate)
                  (let ((default-directory candidate))
                    (eshell t)))))
             (cons
              "Open shell in horizontal split"
              #'helm-eshell--horiz-split)
             (cons
              "Open shell in vertical split"
              #'helm-eshell--vert-split))
    ;; make the candidates get re-generated on input, so one can
    ;; actually create an eshell in a new directory
    :volatile t
    :cleanup
    (lambda ()
      (remove-hook 'helm-after-update-hook
                   #'helm-eshell--move-to-first-real-candidate)))
  "Helm source to switch between eshells.")

;;;###autoload
(defun helm-eshell ()
  "Switch between or create eshell buffers using helm"
  (interactive)
  (add-hook 'helm-after-update-hook
            #'helm-eshell--move-to-first-real-candidate)
  (helm :sources helm-eshell--source
        :buffer "*helm eshell*"
        :prompt "eshell in: "))

(provide 'helm-eshell)
