;;; helm-switch-shell.el --- A Helm source for switching between shell buffers -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Cash

;; Author: James N. V. Cash <james.cash@occasionallycogent.com>
;; URL: https://github.com/jamesnvc/helm-switch-shell
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

;; This package offers a helm-source for switching between shells via
;; helm, sorting them by how their working directory is to your
;; current active directory.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'helm)
(require 'helm-lib)
(require 's)
(require 'subr-x)

;; Customization

(defgroup helm-switch-shell nil
  "Group for `helm-switch-shell' customizations"
  :group 'helm)

(defcustom helm-switch-shell-new-shell-type 'eshell
  "When creating a new shell, should it be eshell (default) or shell."
  :type '(radio (const :tag "eshell" eshell)
                (const :tag "shell" shell))
  :group 'helm-switch-shell)

;; Faces

(defface helm-switch-shell-new-shell-face
  `((t (:background "#ff69c6" :foreground "#282a36")))
  "Face for the [+] indicator for creating a new shell."
  :group 'helm-switch-shell)

;; Helpers

(defun helm-switch-shell--pwd-replace-home (directory)
  "Replace $HOME in DIRECTORY with tilde character."
  (let ((home (expand-file-name (getenv "HOME"))))
    (if (string-prefix-p home directory)
        (concat "~" (substring directory (length home)))
      directory)))

(defun helm-switch-shell--buffer-dir-name (buf)
  "Display the directory of BUF, with HOME replaced with tilde."

  (helm-switch-shell--pwd-replace-home (buffer-local-value 'default-directory buf)))

(defun helm-switch-shell--create-new ()
  "Create a new shell or eshell, honouring `helm-switch-shell-new-shell-type'."
  (cl-case helm-switch-shell-new-shell-type
    (eshell (eshell t))
    (shell (shell))))

;; Switching shells

(defun helm-switch-shell--get-candidates ()
  "Get existing shell/eshell buffers as well as a potential new shell location for the ‘helm-switch-shell’ source."
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
         (shells (cl-loop for buf in (buffer-list)
                           when (or (string-prefix-p "*eshell*" (buffer-name buf))
                                    (string-prefix-p "*shell*" (buffer-name buf)))
                           collect (cons (helm-switch-shell--buffer-dir-name buf) buf) into cands
                           finally return (-> cands
                                              (sort (lambda (a b) (< (length (car a)) (length (car b)))))
                                              (sort
                                               (lambda (a b) (> (funcall dist2here (car a))
                                                           (funcall dist2here (car b))))))))
         (new-dir (if (string-blank-p helm-input)
                      default-directory
                    helm-input))
         (new-shell (cons (concat
                            (propertize
                             " " 'display
                             (propertize "[+]" 'face 'helm-switch-shell-new-shell-face))
                            " "
                            (helm-switch-shell--pwd-replace-home new-dir))
                           new-dir)))
    (cons new-shell shells)))

(defun helm-switch-shell--move-to-first-real-candidate ()
  "Move the helm selection down to the first that's actually a buffer."
  (let ((sel (helm-get-selection nil nil (helm-get-current-source))))
    (unless (bufferp sel)
      (helm-next-line))))

(defun helm-switch-shell--horiz-split (candidate)
  "Open CANDIDATE in a horizontal split."
  (if (bufferp candidate)
      (progn
        (select-window (split-window-below))
        (switch-to-buffer candidate))
    (let ((default-directory candidate)
          (display-buffer-alist '(("\\`\\*e?shell" display-buffer-same-window))))
    (select-window (split-window-below))
    (helm-switch-shell--create-new)
    (balance-windows))))

(defun helm-switch-shell--vert-split (candidate)
  "Open CANDIDATE in a vertical split."
  (if (bufferp candidate)
      (progn
        (select-window (split-window-right))
        (switch-to-buffer candidate))
    (let ((default-directory candidate)
          (display-buffer-alist '(("\\`\\*e?shell" display-buffer-same-window))))
      (select-window (split-window-right))
      (helm-switch-shell--create-new)
      (balance-windows))))

(defun helm-switch-shell--horiz-split-command ()
  "Helm command that opens shell in a horizontal split."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-switch-shell--horiz-split)))

(defun helm-switch-shell--vert-split-command ()
  "Helm command that opens shell in a vertical split."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-switch-shell--vert-split)))

(defconst helm-switch-shell-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-s") #'helm-switch-shell--horiz-split-command)
    (define-key map (kbd "C-v") #'helm-switch-shell--vert-split-command)
    map)
  "Keymap for `helm-switch-shell'.")

(defconst helm-switch-shell--source
  (helm-build-sync-source "shell-switcher"
    :keymap helm-switch-shell-map
    :candidates #'helm-switch-shell--get-candidates
    :action (list
             (cons
              "Switch to shell"
              (lambda (candidate)
                (if (bufferp candidate)
                    (switch-to-buffer candidate)
                  (let ((default-directory candidate))
                    (helm-switch-shell--create-new)))))
             (cons
              "Open shell in horizontal split"
              #'helm-switch-shell--horiz-split)
             (cons
              "Open shell in vertical split"
              #'helm-switch-shell--vert-split))
    :volatile t
    :cleanup
    (lambda ()
      (remove-hook 'helm-after-update-hook
                   #'helm-switch-shell--move-to-first-real-candidate)))
  "Helm source to switch between shells.")

;;;###autoload
(defun helm-switch-shell ()
  "Switch between or create shell/eshell buffers using helm."
  (interactive)
  (add-hook 'helm-after-update-hook
            #'helm-switch-shell--move-to-first-real-candidate)
  (helm :sources helm-switch-shell--source
        :buffer "*helm shell*"
        :prompt "shell in: "))

(provide 'helm-switch-shell)
;;; helm-switch-shell.el ends here
