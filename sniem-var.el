;;; sniem-var.el --- Simple united edition method -*- lexical-binding: t -*-

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

;; Simple united edition method.

;;; Code:

(defcustom sniem-space-command nil
  "The command binded on SPC."
  :type 'symbol
  :group 'sniem)

(defcustom sniem-reverse-status nil
  "The reverse status."
  :type 'symbol
  :group 'sniem)

(defcustom sniem-mode-keymap
  (let ((map (make-sparse-keymap)))
    map)
  "Mode keymap for sniem."
  :type 'keymap
  :group 'sniem)

(defcustom sniem-current-mode nil
  "Current mode for sniem."
  :type 'symbol
  :group 'sniem)

(defvar sniem-normal-mode-cursor t
  "Cursor type for normal mode.")

(defvar sniem-insert-mode-cursor 'bar
  "Cursor type for insert mode.")

(defvar sniem-motion-mode-cursor t
  "Cursor type for motion mode.")

(defvar sniem-on-newline-point nil
  "If the cursor can be on the newline point.")

(defcustom sniem-leader-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "x" 'sniem-keypad)
    (define-key map "m" 'sniem-keypad)
    (define-key map "b" 'sniem-keypad)
    (define-key map "v" 'sniem-keypad)
    (define-key map "d" 'sniem-digit-argument)
    (define-key map (kbd "SPC") 'sniem-execute-space-command)
    map)
  "Leader keymap."
  :type 'keymap
  :group 'sniem)

(defcustom sniem-normal-state-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "a" 'sniem-append)
    (define-key map "A" 'sniem-append-line)
    (define-key map "o" 'sniem-open-line)
    (define-key map "O" 'sniem-open-line-previous)
    (define-key map "s" 'sniem-eval-last-sexp)
    (define-key map "S" 'save-buffer)
    (define-key map "r" 'sniem-replace-char)
    (define-key map "R" 'sniem-replace-word)
    (define-key map "z" 'sniem-center)
    (define-key map "x" 'sniem-delete-char)
    (define-key map "X" 'execute-extended-command)
    (define-key map "c" 'sniem-change)
    (define-key map "C" 'sniem-change-in-region)
    (define-key map "d" 'sniem-delete)
    (define-key map "D" 'sniem-delete-in-region)
    (define-key map "b" 'beginning-of-line)
    (define-key map "B" 'sniem-end-of-line)
    (define-key map "m" 'sniem-mark)
    (define-key map "." 'sniem-repeat)
    (define-key map "/" 'sniem-search)
    (define-key map "w" 'sniem-next-word)
    (define-key map "W" 'sniem-prev-word)
    (define-key map "t" 'sniem-next-text)
    (define-key map "T" 'sniem-prev-text)
    (define-key map "f" 'sniem-find-forward)
    (define-key map "F" 'sniem-find-backward)
    (define-key map "p" 'sniem-paste)
    (define-key map "g" 'sniem-first-line)
    (define-key map "G" 'sniem-goto-line)
    (define-key map "y" 'sniem-yank)
    (define-key map "v" 'scroll-down-command)
    (define-key map "V" 'scroll-up-command)
    (define-key map "q" 'sniem-macro)
    (define-key map "Q" 'save-buffers-kill-terminal)
    (define-key map ";" 'keyboard-quit)
    (define-key map "'" 'sniem-end-of-mark)
    (define-key map "\"" 'sniem-beg-of-mark)
    (define-key map "<" 'sniem-goto-prev)
    (define-key map ">" 'sniem-goto-next)
    (define-key map (kbd "SPC") 'sniem-digit-argument)
    (define-key map (kbd "RET") 'sniem-object-catch)
    map)
  "Normal mode keymap."
  :type 'keymap
  :group 'sniem)

(defcustom sniem-insert-state-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") 'sniem-quit-insert)
    map)
  "Insert mode keymap."
  :type 'keymap
  :group 'sniem)

(defcustom sniem-motion-state-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") sniem-leader-keymap)
    map)
  "Motion mode keymap."
  :type 'keymap
  :group 'sniem)

(defcustom sniem-cursor-color (frame-parameter nil 'cursor-color)
  "The cursor color for the sniem fake cursor."
  :type 'string
  :group 'sniem)

(defcustom sniem-cursor-overlay nil
  "The overlay for the sniem fake cursor."
  :type 'overlay
  :group 'sniem)

(defvar sniem-normal-mode-alist
  '(fundamental-mode text-mode prog-mode conf-mode cider-repl-mode eshell-mode
    vterm-mode json-mode wdired-mode deft-mode pass-view-mode
    telega-chat-mode restclient-mode help-mode deadgrep-edit-mode mix-mode)
  "The alist of major modes that make sniem open normal mode.")

(provide 'sniem-var)

;;; sniem-var.el ends here
