;;; sniem-mark-jump.el --- Hands-eased united editing method -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan

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

;; Hands-eased united editing method.

;;; Code:

(require 'sniem-common)
(require 'sniem-operation)

(declare-function sniem-change-mode "sniem")

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

(defun sniem-mark-jump-lisp-mode-p ()
  "Check if the current major mode belongs to Lisp mode."
  (string-match-p "\\(?:.*\\)lisp\\(?:.*\\)" (symbol-name major-mode)))

(defun sniem-mark-jump-insert (type &optional name)
  "Insert the mark with TYPE.
Optional argument NAME means enable the name."
  (interactive (list (completing-read "Enter the type: "
                                      sniem-mark-jump-items)))
  (unless (eq (face-at-point) 'font-lock-comment-face)
    (if (and (eolp)
             (not (looking-back "^[\s\t]*" (line-beginning-position) t)))
        (insert (if (not (memq (char-before) '(?\t ?\s)))
                    " "
                  "")
                comment-start)
      (unless (= (line-beginning-position) (line-end-position))
        (sniem-open-line-previous))
      (insert (if (sniem-mark-jump-lisp-mode-p)
                  (concat comment-start comment-start)
                comment-start))))
  (insert (if (string= " " (substring comment-start -1))
              ""
            " ")
          type
          (format "%s: " (if (or name sniem-mark-jump-author-name-enable)
                             (sniem-mark-jump--get-author-name)
                           "")))
  (when comment-end
    (save-mark-and-excursion
      (insert comment-end)))
  (sniem-change-mode 'insert))

(defun sniem-mark-jump-insert-with-name (&optional name)
  "`sniem-mark-jump-insert' with NAME."
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
  "Jump to NEXT/previous item.
Optional argument TYPE is the type of comment mark."
  (let ((search-command (if next
                            're-search-forward
                          're-search-backward))
        (point (point))
        (case-fold-search nil)
        (tmp t))
    (when (sniem-mark-jump--comment-face-p)
      (sniem-mark-jump--escape-comment (when next t)))
    (catch 'stop
      (while tmp
        (setq tmp (funcall search-command
                           (concat "\\(" comment-start "*\\)"
                                   (if type
                                       (concat "\\(?:.*\\)" type "\\(?:.*\\)")
                                     sniem-mark-jump-regexp))
                           nil t))
        (when (and tmp (numberp tmp))
          (goto-char (comment-beginning))
          (throw 'stop t)))
      (message "[Sniem]: The mark can not be found.")
      (goto-char point))))

(defun sniem-mark-jump--escape-comment (forward)
  "Escape current comment.
Argument FORWARD means search forward."
  (let ((motion (if forward
                    'forward-char
                  'backward-char)))
    (while (sniem-mark-jump--comment-face-p)
      (funcall motion))))

(defun sniem-mark-jump--comment-face-p ()
  "Check if the content at point has the comment face."
  (let ((face-list (get-text-property (point) 'face)))
    (when face-list
      (or (and (symbolp face-list)
               (or (eq face-list 'font-lock-comment-face)
                   (eq face-list 'font-lock-comment-delimiter-face)))
          (and (listp face-list) (memq 'font-lock-comment-face face-list))))))

(defun sniem-mark-jump--get-author-name ()
  "Get the author's name."
  (if sniem-mark-jump-author-name
      sniem-mark-jump-author-name
    (unless sniem-mark-jump-author-name-enable
      (setq-local sniem-mark-jump-author-name-enable t))
    (let ((tmp (read-string "Enter your name: ")))
      (if (string-equal tmp "")
          ;; When the name string is empty, disabling to insert author name.
          (progn
            (setq-local sniem-mark-jump-author-name-enable nil)
            "")
        tmp))))

(defun sniem-mark-jump-reset-regexp ()
  "Reset the regexp."
  (setq sniem-mark-jump-regexp
        (concat "\\(?:.*\\)\\("
                (mapconcat #'sniem-mark-jump--self sniem-mark-jump-items "\\|")
                "\\)\\(?:.*\\)")))

(defun sniem-mark-jump--self (arg)
  "Return ARG itself."
  arg)

(sniem-mark-jump-reset-regexp)

;;; For user
(defun sniem-mark-jump-set-items (way item)
  "Set `sniem-mark-jump-items'.
The WAY includes:
:add - Add a new ITEM.
:delete - Delete a item."
  (pcase way
    (:add (unless (sniem--mems item sniem-mark-jump-items)
            (setq sniem-mark-jump-items (append sniem-mark-jump-items (list item)))))
    (:delete (setq sniem-mark-jump-items (delete item sniem-mark-jump-items))))
  (sniem-mark-jump-reset-regexp))

(provide 'sniem-mark-jump)

;;; sniem-mark-jump.el ends here
