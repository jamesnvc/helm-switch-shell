;;; helm-switch-eshell.el --- A Helm source for switching eshells -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Cash

;; Author: James N. V. Cash <james.cash@occasionallycogent.com>
;; URL: https://github.com/jamesnvc/helm-switch-eshell
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (helm "2.8.8") (dash "2.16.0") (s "1.12.0"))
;; Package-Version: 1.0
;; Keywords: matching, processes, terminals, tools

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

(require 'cl-lib)
(require 'dash)
(require 'helm)
(require 'helm-lib)
(require 's)
(require 'subr-x)

(defun helm-switch-eshell--pwd-replace-home (directory)
  "Replace $HOME in DIRECTORY with tilde character."
  (let ((home (expand-file-name (getenv "HOME"))))
    (if (string-prefix-p home directory)
        (concat "~" (substring directory (length home)))
      directory)))

(defun helm-switch-eshell--buffer-dir-name (buf)
  "Display the directory of BUF, with HOME replaced with tilde."

  (helm-switch-eshell--pwd-replace-home (buffer-local-value 'default-directory buf)))

(defun helm-switch-eshell--get-candidates ()
  "Get existing eshell buffers as well as a potential new shell location for the ‘helm-switch-eshell’ source."
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
                            (length)
                            (+ (if (numberp prefix) 0 2))))))
         (eshells (cl-loop for buf in (buffer-list)
                           when (string-prefix-p "*eshell*" (buffer-name buf))
                           collect (cons (helm-switch-eshell--buffer-dir-name buf) buf) into cands
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
                            (helm-switch-eshell--pwd-replace-home new-dir))
                           new-dir)))
    (cons new-eshell eshells)))

(defun helm-switch-eshell--move-to-first-real-candidate ()
  "Move the helm selection down to the first that's actually a buffer."
  (let ((sel (helm-get-selection nil nil (helm-get-current-source))))
    (unless (bufferp sel)
      (helm-next-line))))

(defun helm-switch-eshell--horiz-split (candidate)
  "Open CANDIDATE in a horizontal split."
  (if (bufferp candidate)
      (progn
        (select-window (split-window-below))
        (switch-to-buffer candidate))
    (let ((default-directory candidate))
      (select-window (split-window-below))
      (eshell t)
      (balance-windows))))

(defun helm-switch-eshell--vert-split (candidate)
  "Open CANDIDATE in a vertical split."
  (if (bufferp candidate)
      (progn
        (select-window (split-window-right))
        (switch-to-buffer candidate))
    (let ((default-directory candidate))
      (select-window (split-window-right))
      (eshell t)
      (balance-windows))))

(defun helm-switch-eshell--horiz-split-command ()
  "Helm command that opens eshell in a horizontal split."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-switch-eshell--horiz-split)))

(defun helm-switch-eshell--vert-split-command ()
  "Helm command that opens eshell in a vertical split."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-switch-eshell--vert-split)))

(defconst helm-switch-eshell-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-s") #'helm-switch-eshell--horiz-split-command)
    (define-key map (kbd "C-v") #'helm-switch-eshell--vert-split-command)
    map)
  "Keymap for `helm-switch-eshell'.")

(defconst helm-switch-eshell--source
  (helm-build-sync-source "eshell"
    :keymap helm-switch-eshell-map
    :candidates #'helm-switch-eshell--get-candidates
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
              #'helm-switch-eshell--horiz-split)
             (cons
              "Open shell in vertical split"
              #'helm-switch-eshell--vert-split))
    ;; make the candidates get re-generated on input, so one can
    ;; actually create an eshell in a new directory
    :volatile t
    :cleanup
    (lambda ()
      (remove-hook 'helm-after-update-hook
                   #'helm-switch-eshell--move-to-first-real-candidate)))
  "Helm source to switch between eshells.")

;;;###autoload
(defun helm-switch-eshell ()
  "Switch between or create eshell buffers using helm."
  (interactive)
  (add-hook 'helm-after-update-hook
            #'helm-switch-eshell--move-to-first-real-candidate)
  (helm :sources helm-switch-eshell--source
        :buffer "*helm eshell*"
        :prompt "eshell in: "))

(provide 'helm-switch-eshell)
;;; helm-switch-eshell.el ends here
