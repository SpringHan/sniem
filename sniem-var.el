;;; sniem-var.el --- Simple united editing method -*- lexical-binding: t -*-

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

;; Simple united editing method.

;;; Code:

(defcustom sniem-space-command nil
  "The command binded on SPC."
  :type 'symbol
  :group 'sniem)

(defcustom sniem-initialized nil
  "If the sniem initialized."
  :type 'boolean
  :group 'sniem)

(defcustom sniem-mode-keymap
  (let ((map (make-sparse-keymap)))
    map)
  "Mode keymap for sniem."
  :type 'keymap
  :group 'sniem)

(defcustom sniem-last-point nil
  "The last point."
  :type 'number
  :group 'sniem)

(defcustom sniem-last-point-overlay nil
  "The overlay for last point."
  :type 'overlay
  :group 'sniem)

(defcustom sniem-last-point-locked nil
  "If the `sniem-last-point' is locked."
  :type 'boolean
  :group 'sniem)

(defcustom sniem-keyboard-layout nil
  "User's keyboard layout."
  :type 'symbol
  :group 'sniem)

(defcustom sniem-motion-hint-overlays nil
  "The list of all the motion hint overlays."
  :type 'list
  :group 'sniem)

(defcustom sniem-motion-hint-sit-time 1
  "The time for motion hint sit."
  :type 'number
  :group 'sniem)

(defcustom sniem-kmacro-range nil
  "The range for kmacro."
  :type 'overlay
  :group 'sniem)

(defcustom sniem-kmacro-mark-content nil
  "The content which was marked by kmacro."
  :type 'string
  :group 'sniem)

(defcustom sniem-mark-line nil
  "If in the mark line status."
  :type 'boolean
  :group 'sniem)

(defcustom sniem-delete-edit nil
  "If it's in delete edit."
  :type 'boolean
  :group 'sniem)

(defcustom sniem-change-edit nil
  "If it's in change edit."
  :type 'boolean
  :group 'sniem)

(defvar sniem-normal-mode-cursor t
  "Cursor type for normal mode.")

(defvar sniem-insert-mode-cursor 'bar
  "Cursor type for insert mode.")

(defvar sniem-motion-mode-cursor t
  "Cursor type for motion mode.")

(defvar sniem-insert-quit-key "<escape>"
  "The `sniem-quit-insert' key.")

(defcustom sniem-leader-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "x" 'sniem-keypad)
    (define-key map "m" 'sniem-keypad)
    (define-key map "b" 'sniem-keypad)
    (define-key map "v" 'sniem-keypad)
    (define-key map "c" 'sniem-keypad)
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
    (define-key map "s" 'sniem-search)
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
    (define-key map "b" 'sniem-beginning-of-line)
    (define-key map "B" 'sniem-end-of-line)
    (define-key map "m" 'sniem-mark)
    (define-key map "M" 'sniem-expand-enter-or-quit)
    (define-key map "/" 'isearch-forward)
    (define-key map "w" 'sniem-next-word)
    (define-key map "W" 'sniem-prev-word)
    (define-key map "f" 'sniem-find-forward)
    (define-key map "F" 'sniem-find-backward)
    (define-key map "p" 'sniem-paste)
    (define-key map "P" 'sniem-paste-in-region)
    (define-key map "g" 'sniem-first-line)
    (define-key map "G" 'sniem-goto-line)
    (define-key map "y" 'sniem-yank)
    (define-key map "Y" 'sniem-yank-in-region)
    (define-key map "v" 'sniem-scroll-up-command)
    (define-key map "V" 'sniem-scroll-down-command)
    (define-key map "q" 'sniem-macro)
    (define-key map "Q" 'save-buffers-kill-terminal)
    (define-key map ";" 'sniem-keyboard-quit)
    (define-key map "'" 'sniem-end-of-mark)
    (define-key map "\"" 'sniem-beg-of-mark)
    (define-key map "<" 'sniem-goto-prev)
    (define-key map ">" 'sniem-goto-next)
    (define-key map "`" 'sniem-up-down-case)
    (define-key map "1" 'digit-argument)
    (define-key map "2" 'digit-argument)
    (define-key map "3" 'digit-argument)
    (define-key map "4" 'digit-argument)
    (define-key map "5" 'digit-argument)
    (define-key map "6" 'digit-argument)
    (define-key map "7" 'digit-argument)
    (define-key map "8" 'digit-argument)
    (define-key map "9" 'digit-argument)
    (define-key map "0" 'digit-argument)
    (define-key map "-" 'kill-current-buffer)
    (define-key map "_" 'kill-buffer-and-window)
    (define-key map "." 'sniem-move-last-point)
    (define-key map "?" 'sniem-cheatsheet)
    (define-key map (kbd "SPC") 'sniem-digit-argument-or-fn)
    (define-key map (kbd "RET") 'sniem-expand-with-catch)
    (define-key map (kbd "(") 'sniem-object-catch-char)
    (define-key map (kbd "[") 'sniem-object-catch-char)
    (define-key map (kbd "{") 'sniem-object-catch-char)
    (define-key map (kbd ")") 'sniem-object-catch-parent)
    (define-key map (kbd "]") 'sniem-object-catch-parent)
    (define-key map (kbd "}") 'sniem-object-catch-parent)
    (define-key map (kbd "<C-M-return>") 'sniem-object-catch-parent-by-char)
    (define-key map (kbd "C-<return>") 'sniem-object-catch-by-char)
    (define-key map (kbd "M-<return>") 'sniem-object-catch-parent)
    (define-key map (kbd "DEL") 'sniem-backward-char)
    map)
  "Normal mode keymap."
  :type 'keymap
  :group 'sniem)

(defcustom sniem-insert-state-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<escape>") 'sniem-quit-insert)
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

(defcustom sniem-expand-state-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'sniem-expand-enter-or-quit)
    (define-key map (kbd "RET") 'sniem-object-catch)
    (define-key map "p" 'sniem-object-catch-parent)
    (define-key map "r" 'sniem-object-catch-repeat)
    (define-key map "b" 'sniem-object-catch-by-char)
    (define-key map "B" 'sniem-object-catch-parent-by-char)
    (define-key map "s" 'sniem-object-catch-char)
    (define-key map "S" 'sniem-object-catch-parent)
    (define-key map "o" 'sniem-object-catch-char)
    (define-key map "O" 'sniem-object-catch-parent)
    (define-key map "c" 'sniem-object-catch-char)
    (define-key map "C" 'sniem-object-catch-parent)
    (define-key map "q" 'sniem-object-catch-char)
    (define-key map "Q" 'sniem-object-catch-parent)
    (define-key map "a" 'sniem-object-catch-char)
    (define-key map "A" 'sniem-object-catch-parent)
    (define-key map "d" 'sniem-object-catch-char)
    (define-key map "D" 'sniem-object-catch-parent)
    (define-key map "/" 'sniem-object-catch-direction-reverse)
    (define-key map "." 'sniem-object-catch-expand)
    map)
  "Expand mode keymap."
  :type 'keymap
  :group 'sniem)

(defcustom sniem-minibuffer-keypad-state-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'sniem-minibuffer-keypad)
    (define-key map (kbd "SPC") 'sniem-minibuffer-keypad-start-or-stop)
    map)
  "The keymap for minibuffer-keypad state."
  :type 'keymap
  :group 'sniem)

(defcustom sniem-motion-hint-motion nil
  "The last hint motion."
  :type 'symbol
  :group 'sniem)

(defcustom sniem-mark-content-overlay nil
  "The mark content overlay."
  :type 'list
  :group 'sniem)

(defcustom sniem-search-result-tip nil
  "The tip of the search result."
  :type 'overlay
  :group 'sniem)

(defcustom sniem-locked-macro nil
  "The locked kmacro."
  :type 'symbol
  :group 'sniem)

(defcustom sniem-macro-file nil
  "The file to store kbd macros."
  :type 'string
  :group 'sniem)

(defcustom sniem-search-timer nil
  "The timer for searching."
  :type 'timer
  :group 'sniem)

(defcustom sniem-search-result-overlays nil
  "The overlays for search results."
  :type 'list
  :group 'sniem)

(defcustom sniem-special-clipboard nil
  "The special clipboard list."
  :type 'list
  :group 'sniem)

(defcustom sniem-minibuffer-keypad-on nil
  "If the minibuffer-keypad mode is opened."
  :type 'boolean
  :group 'sniem)

(defcustom sniem-minibuffer-keypad-prefix "C-"
  "The prefix for minibuffer-keypad."
  :type 'string
  :group 'sniem)

(defcustom sniem-minibuffer-keymap nil
  "The origin keymap for the minibuffer."
  :type 'keymap
  :group 'sniem)

(defvar sniem-normal-mode-alist
  '(fundamental-mode text-mode prog-mode conf-mode cider-repl-mode
    json-mode wdired-mode deft-mode pass-view-mode telega-chat-mode
    restclient-mode help-mode deadgrep-edit-mode mix-mode authinfo-mode)
  "The alist of major modes that make sniem open normal mode.")

(defvar sniem-insert-mode-alist '(shell-mode eshell-mode vterm-mode inferior-emacs-lisp-mode erc-mode term-mode)
  "The alist of major modes that make sniem open insert mode.")

(defvar sniem-input-method-closed nil
  "If the input method was closed when user changed to normal state.")

(defvar sniem-close-mode-alist nil
  "The modes alist for close sniem.")

(defvar sniem-center-message "[z] for center, [t] for top, [b] for buttom:"
  "The message for `sniem-center'.")

(defvar sniem-mark-message "[m] for normal, [p] for From last point, [l] for line:"
  "The message for `sniem-mark'.")

(defvar sniem-delete-message "[d] for line, [p] for From last point, [D] for Clear line contents:"
  "The message for `sniem-delete'.")

(defvar sniem-change-message "[c] for line, [p] for From last point:"
  "The message for `sniem-delete'.")

(defvar sniem-yank-message "[y] for line, [p] for From last point:"
  "The message for `sniem-yank'.")

(defvar sniem-macro-message
  "[q] for record, [e] for Eval last kbd macro, [n] for Name for it, [l] for lock or unlock macro, [.] for forcely lock macro, [c] for call macro:"
  "The message for `sniem-macro'.")

(defface sniem-motion-hint-face
  `((t (:foreground ,(frame-parameter nil 'background-color)
                    :background ,(frame-parameter nil 'foreground-color))))
  "The face for motion hint."
  :group 'sniem)

(provide 'sniem-var)

;;; sniem-var.el ends here
