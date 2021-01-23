;;; sniem-operation.el --- Simple united edition method -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Package-Requires: ((emacs))
;; Homepage: https://github.com/SpringHan/sniem.git
;; Keywords: Edition Method


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

;; Simple united edition method

;;; Code:

(defun sniem-insert ()
  "Insert at the current point or the beginning of mark region."
  (interactive)
  (unless (eq (sniem-current-mode) 'insert)
    (when (region-active-p)
      (goto-char (region-beginning)))
    (sniem-change-mode 'insert)))

(defun sniem-insert-line ()
  "Insert at the beginning of line."
  (interactive)
  (back-to-indentation)
  (sniem-insert))

(defun sniem-append ()
  "Append at the next point or the end of mark region."
  (interactive)
  (when (region-active-p)
    (goto-char (region-beginning)))
  (forward-char)
  (sniem-insert))

(defun sniem-append-line ()
  "Append at the end of line."
  (interactive)
  (end-of-line)
  (sniem-insert))

(defun sniem--open-line (&optional above)
  "Open new line for other function."
  (when above
    (sniem-prev-line nil t))
  (sniem-end-of-line t)
  (insert "\n"))

(defun sniem-open-line ()
  "Open new line."
  (interactive)
  (sniem--open-line)
  (indent-according-to-mode)
  (sniem-insert))

(defun sniem-open-line-previous ()
  "Open new line."
  (interactive)
  (sniem--open-line t)
  (indent-according-to-mode)
  (sniem-insert))

(defun sniem-center (action)
  "Center action for sniem."
  (interactive "c[z] for center, [t] for top, [b] for buttom:")
  (pcase action
    (122 (recenter nil t))
    (116 (recenter-top-bottom 0))
    (98 (recenter-top-bottom -1))
    (t (message "[Sniem]: The key you press is not exsits for center."))))

;;; Motions

(sniem-define-motion sniem-beginning-of-line ()
  (beginning-of-line))

(sniem-define-motion sniem-end-of-line ()
  (end-of-line))

(sniem-define-motion sniem-forward-char (&optional times)
  (setq times (or times 1))
  (unless (eolp)
    (forward-char times)))

(sniem-define-motion sniem-5-forward-char ()
  (sniem-forward-char 5 t))

(sniem-define-motion sniem-backward-char (&optional times)
  (setq times (or times 1))
  (unless (bolp)
    (backward-char times)))

(sniem-define-motion sniem-5-backward-char ()
  (sniem-backward-char 5 t))

(sniem-define-motion sniem-prev-line (&optional times)
  (setq times (or times 1))
  (unless (bobp)
    (previous-line times)))

(sniem-define-motion sniem-5-prev-line ()
  (sniem-prev-line 5 t))

(sniem-define-motion sniem-next-line (&optional times)
  (setq times (or times 1))
  (unless (eobp)
    (next-line times)))

(sniem-define-motion sniem-5-next-line ()
  (sniem-next-line 5 t))

(provide 'sniem-operation)

;;; sniem-operation.el ends here
