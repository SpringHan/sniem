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
  (when (region-active-p)
    (goto-char (region-beginning)))
  (sniem-change-mode 'insert))

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

;; <TODO(SpringHan)> This function has bug. [Sun Jan 17 08:56:23 2021]
(defun sniem-end-of-line ()
  "Goto end of line."
  (interactive)
  (if sniem-on-newline-point
      (end-of-line)
    (end-of-line)
    (backward-char)))

(defun sniem-eval-last-sexp ()
  "Like `eval-last-sexp', but it can also eval in the ()."
  )

(provide 'sniem-operation)

;;; sniem-operation.el ends here
