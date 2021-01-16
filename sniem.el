;;; sniem.el --- Simple united edition method -*- lexical-binding: t -*-

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

(defgroup sniem nil
  "The group for sniem."
  :group 'applications)

(require 'sniem-var)
(require 'sniem-macro)


(define-minor-mode sniem-mode
  "Simple united edition method mode."
  nil nil sniem-mode-keymap
  (if sniem-mode
      (sniem--enable)
    (sniem--disable)))

(define-globalized-minor-mode global-sniem-mode
  sniem-mode sniem-initialize)

(define-minor-mode sniem-normal-mode
  "Normal mode for sniem."
  nil nil sniem-normal-state-keymap
  (when sniem-normal-mode
    (sniem-normal-mode-init)))

(define-minor-mode sniem-insert-mode
  "Insert mode for sniem."
  nil nil sniem-insert-state-keymap
  (when sniem-insert-mode
    (sniem-insert-mode-init)))

(define-minor-mode sniem-motion-mode
  "Motion mode for sniem."
  nil nil sniem-motion-state-keymap
  (when sniem-motion-mode
    (sniem-motion-mode-init)))

(defun sniem-normal-mode-init ()
  "Normal mode init."
  (sniem-insert-mode -1)
  (sniem-motion-mode -1))

(defun sniem-insert-mode-init ()
  "Insert mode init."
  (sniem-normal-mode -1)
  (sniem-motion-mode -1))

(defun sniem-motion-mode-init ()
  "Motion mode init."
  (sniem-normal-mode -1)
  (sniem-insert-mode -1))

(defun sniem--enable ()
  "Unable sniem."
  (unless sniem-space-command
    (setq sniem-space-command (key-binding (kbd "SPC"))))
  (if (apply #'derived-mode-p sniem-normal-mode-alist)
      (sniem-change-mode 'normal)
    (sniem-change-mode 'motion))
  (add-to-list 'emulation-mode-map-alists 'sniem-normal-state-keymap))

(defun sniem--disable ()
  "Disable sniem."
  (sniem-normal-mode -1)
  (sniem-insert-mode -1)
  (sniem-motion-mode -1))

;;; Interactive functions

(defun sniem-execute-space-command ()
  "Execute space command."
  (interactive)
  (call-interactively sniem-space-command))

(defun sniem-quit-insert ()
  "Quit insert mode."
  (interactive)
  (sniem-change-mode 'normal))

;; <TODO(SpringHan)> Add the function more [Sat Jan 16 22:23:54 2021]
(defun sniem-insert ()
  "Insert."
  (interactive)
  (sniem-change-mode 'insert))

(defun sniem-keypad ()
  "Execute the keypad command."
  (interactive)
  (let ((key (pcase last-input-event
               (120 "C-x ") (109 "M-") (98 "C-M-") (118 "C-")))
        tmp)
    (when (null key)
      (setq key (concat "C-" (char-to-string last-input-event))))

    (message key)
    (while (not (= 13 (setq tmp (read-char))))
      (if (= tmp 127)
          (setq key (substring key 0 -2))
        (when (= tmp 59)
          (keyboard-quit))
        (setq key (concat key
                          (cond ((= tmp 44) "C-")
                                ((= tmp 46) "M-")
                                ((= tmp 47) "C-M-")
                                (t (concat (char-to-string tmp) " "))))))
      (message key))
    (setq key (substring key 0 -1))
    (if (commandp (setq tmp (key-binding (read-kbd-macro key))))
        (call-interactively tmp)
      (message "[Evil]: '%s' is not defined." key))))

;;; Functional functions

(defun sniem-initialize ()
  "Initialize sniem."
  (unless (minibufferp)
    (sniem-mode t)))

(defun sniem-change-mode (mode)
  "Change edition mode."
  (if (eq sniem-current-mode mode)
      (message "[Sniem]: The current mode is %S!You need't change it." mode)
    (pcase mode
      ('normal (sniem-normal-mode t))
      ('insert (sniem-insert-mode t))
      ('motion (sniem-motion-mode t)))
    (sniem-cursor-change)))

(defun sniem-cursor-change ()
  "Change cursor type."
  (setq-local cursor-type (pcase (sniem-current-mode)
                            ('normal sniem-normal-mode-cursor)
                            ('insert sniem-insert-mode-cursor)
                            ('motion sniem-motion-mode-cursor))))

(defun sniem-current-mode ()
  "Get current mode."
  (cond (sniem-normal-mode 'normal)
        (sniem-insert-mode 'insert)
        (sniem-motion-mode 'motion)
        (t nil)))

(defun sniem-set-leader-key (key)
  "Set the leader key for normal mode."
  (define-key sniem-normal-state-keymap (kbd key) sniem-leader-keymap))

(defun sniem-leader-set-key (&rest keys)
  "Bind key to leader keymap.

\(fn KEY FUNC...)"
  (let (key func)
    (while keys
      (setq key (pop keys)
            func (pop keys))
      (define-key sniem-leader-keymap (kbd key) func))))

(defun sniem-normal-set-key (&rest keys)
  "Bind key to normal mode keymap.

\(fn KEY FUNC...)"
  (let (key func)
    (while keys
      (setq key (pop keys)
            func (pop keys))
      (define-key sniem-normal-state-keymap (kbd key) func))))

;;; Initialize
(sniem-set-leader-key ",")

;;; Debug
(sniem-normal-set-key
 "h" 'sniem-insert)

(provide 'sniem)

;;; sniem.el ends here
