;;; sniem-linked-file.el --- Hands-eased united editing method -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Hands-eased united editing method.

;;; Code:

(defcustom sniem-linked-file-list nil
  "The list used to save date of linked files."
  :type 'list
  :group 'sniem)

(defcustom sniem-linked-file-root-buffer nil
  "The root buffer."
  :type 'buffer
  :group 'sniem)

(defun sniem-linked-file-pannel (option)
  "Execute commands accroding to OPTION."
  (interactive (list (read-char sniem-linked-file-message)))
  (call-interactively (pcase option
                        (?r #'sniem-linked-file-set-root)
                        (?l #'sniem-linked-file-as-linked)
                        (?s #'sniem-linked-file-save)
                        (?o #'sniem-linked-file-open)
                        (?c #'sniem-linked-file-clean))))

(defun sniem-linked-file-open ()
  "Open linked files of current file."
  (interactive)
  (let ((linked-files (sniem-linked-file--read)))
    (unless linked-files
      (user-error "[Sniem]: Cannot find linked files for current file!"))
    (dolist (file linked-files)
      (find-file-noselect file))
    (message "[Sniem]: Have opened linked files.")))

(defun sniem-linked-file-set-root ()
  "Set the current file as the root file."
  (interactive)
  (setq sniem-linked-file-list (list (buffer-file-name))
        sniem-linked-file-root-buffer (current-buffer))
  (message "[Sniem]: Have set current buffer as root file."))

(defun sniem-linked-file-as-linked ()
  "Set current file as linked file."
  (interactive)
  (unless sniem-linked-file-list
    (user-error "[Sniem]: You haven't set the root file!"))
  (add-to-list 'sniem-linked-file-list (buffer-file-name) t #'string-equal)
  (message "[Sniem]: Have set current buffer as linked file."))

(defun sniem-linked-file-save ()
  "Save current linked list."
  (interactive)
  (sniem-linked-file--save sniem-linked-file-list)
  (setq sniem-linked-file-list nil
        sniem-linked-file-root-buffer nil)
  (message "[Sniem]: Successfully save!"))

(defun sniem-linked-file-clean ()
  "Clean current linked list."
  (interactive)
  (setq sniem-linked-file-list nil)
  (message "[Sniem]: Have cleaned linked list."))

(defun sniem-linked-file-add-ignore ()
  "Add the root conf file into .gitignore."
  (interactive)
  (let* ((file (concat (sniem-linked-file--root) ".gitignore"))
         (prev-content ""))
    (if (file-exists-p file)
        (setq prev-content (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string)))
      (make-empty-file file))
    (with-temp-file file
      (insert "SNIEM_LINKED_FILE\n" prev-content))))

(defun sniem-linked-file--save (cont)
  "Totally rewrite root conf file with CONT."
  (with-current-buffer sniem-linked-file-root-buffer
    (let* ((conf-file (sniem-linked-file--root-file))
           (prev-content ""))
      (if (file-exists-p conf-file)
          (setq prev-content (with-temp-buffer
                               (insert-file-contents conf-file)
                               (buffer-string)))
        (make-empty-file conf-file)
        (sniem-linked-file-add-ignore))
      (with-temp-file conf-file
        (insert (format "%S" cont) "\n" prev-content)))))

(defun sniem-linked-file--read ()
  "Read root conf file."
  (with-current-buffer (current-buffer)
    (let* ((conf-file (sniem-linked-file--root-file))
           (file-content (car (read-from-string
                               (replace-regexp-in-string
                                "\n"
                                ""
                                (concat "("
                                        (with-temp-buffer
                                          (insert-file-contents conf-file)
                                          (buffer-string))
                                        ")"))))))
      (when (file-exists-p conf-file)
        (alist-get (buffer-file-name) file-content nil nil #'string-equal)))))

(defun sniem-linked-file--root-file ()
  "Return current root config file."
  (concat (sniem-linked-file--root) "SNIEM_LINKED_FILE"))

(defun sniem-linked-file--root ()
  "Get the root directory of the project which includes root file."
  (let ((result default-directory)
        (get-parent (lambda (path)
                      (let ((dir (substring path 0 -1)))
                        (while (not (string-suffix-p "/" dir))
                          (setq dir (substring dir 0 -1)))
                        dir))))
    (while (not (sniem--mems ".git"
                             (directory-files result)))
      (setq result (funcall get-parent result)))
    result))

(provide 'sniem-linked-file)

;;; sniem-linked-file.el ends here
