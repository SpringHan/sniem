;;; sniem-mark-jump.el --- Simple united editing method -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Package-Requires: ((emacs) (s "20180406.808") (dash "20200524.1947"))
;; Homepage: https://github.com/SpringHan/sniem.git
;; Keywords: Editing Method


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Simple united editing method.

;;; Code:

(defcustom sniem-mark-jump-items
  '("TODO" "BUG" "NOTE")
  "The faces for `sniem-mark-jump'."
  :type 'list
  :group 'sniem)

(defcustom sniem-mark-jump-author-name nil
  "The default name for mark."
  :type 'string
  :group 'sniem)

(defcustom sniem-mark-jump-author-name-enable nil
  "If enable the author name."
  :type 'boolean
  :group 'sniem)

(defcustom sniem-mark-jump-regexp nil
  "The regexp for mark."
  :type 'string
  :group 'sniem)

(defun sniem-mark-jump-insert (type &optional name)
  "Insert the mark with TYPE."
  (interactive (list (completing-read "Enter the type: "
                                      sniem-mark-jump-items)))
  (unless (eq (face-at-point) 'font-lock-comment-face)
    (if (and (eolp)
             (not (looking-back "^[\s\t]*" (line-beginning-position) t)))
        (insert (if (memq (char-before) '(?\t ?\s))
                    " "
                  "")
                comment-start)
      (unless (= (line-beginning-position) (line-end-position))
        (sniem-open-line-previous))
      (insert (if (sniem-object-catch-lisp-mode-p)
                  (concat comment-start comment-start)
                comment-start)
              " ")))
  (insert type
          (if (or name sniem-mark-jump-author-name-enable)
              (format "(%s): " (sniem-mark-jump--get-author-name))
            ": "))
  (sniem-change-mode 'insert))

(defun sniem-mark-jump-insert-with-name (&optional name)
  "`sniem-mark-jump-insert' with name."
  (interactive "P")
  (if name
      (funcall #'sniem-mark-jump-insert
               (completing-read "Enter the type: " sniem-mark-jump-items) t)
    (call-interactively #'sniem-mark-jump-insert)))

(sniem-define-motion sniem-mark-jump-next (&optional type)
  "Jump next."
  (interactive "P")
  (when type
    (setq type (sniem-mark-jump--get-type type)))
  (sniem-mark-jump--jump t type))

(sniem-define-motion sniem-mark-jump-prev (&optional type)
  "Jump previous."
  (interactive "P")
  (when type
    (setq type (sniem-mark-jump--get-type type)))
  (sniem-mark-jump--jump nil type))

(defun sniem-mark-jump--get-type (num)
  "Get type by NUM."
  (nth (1- num) sniem-mark-jump-items))

(defun sniem-mark-jump--jump (next &optional type)
  "Jump to next/previous item."
  (let ((search-command (if next
                            're-search-forward
                          're-search-backward))
        (point (point))
        (case-fold-search nil)
        (tmp t))
    (catch 'stop
      (while tmp
        (if type
            (setq tmp (funcall search-command (concat "\\(.*\\)" type "\\(?:.*\\)") nil t))
          (setq tmp (funcall search-command sniem-mark-jump-regexp nil t)))
        (when (eq (face-at-point) 'font-lock-comment-face)
          (throw 'stop t)))
      (message "[Sniem]: The mark can not be found."))))

(defun sniem-mark-jump--get-author-name ()
  "Get the author's name."
  (if sniem-mark-jump-author-name
      sniem-mark-jump-author-name
    (unless sniem-mark-jump-author-name-enable
      (setq-local sniem-mark-jump-author-name-enable t))
    (read-string "Enter your name: ")))

(defun sniem-mark-jump-reset-regexp ()
  "Reset the regexp."
  (setq sniem-mark-jump-regexp
        (concat "\\(.*\\)\\("
                (mapconcat #'sniem--self sniem-mark-jump-items "\\|")
                "\\)\\(?:.*\\)")))

(defun sniem--self (arg)
  "Return arg itself."
  arg)

(sniem-mark-jump-reset-regexp)

;;; For user
(defun sniem-mark-jump-set-items (way item)
  "Set `sniem-mark-jump-items'.
The WAY includes:
:add - Add a new item.
:delete - Delete a item."
  (pcase way
    (:add (unless (sniem--mems item sniem-mark-jump-items)
            (setq sniem-mark-jump-items (append sniem-mark-jump-items (list item)))))
    (:delete (setq sniem-mark-jump-items (delete item sniem-mark-jump-items))))
  (sniem-mark-jump-reset-regexp))

(provide 'sniem-mark-jump)

;;; sniem-mark-jump.el ends here
