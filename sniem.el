;;; sniem.el --- Simple united editing method -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (s "2.12.0") (dash "1.12.0"))
;; Homepage: https://github.com/SpringHan/sniem.git
;; Keywords: convenience, united-editing-method


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

(require 's)
(require 'dash)

(defgroup sniem nil
  "The group for sniem."
  :group 'applications)

(require 'sniem-var)
(require 'sniem-macro)
(require 'sniem-operation)


(define-minor-mode sniem-mode
  "Simple united editing method mode."
  nil nil sniem-mode-keymap
  (if sniem-mode
      (sniem--enable)
    (sniem--disable)))

;;;###autoload
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
  (sniem-motion-mode -1)
  (when current-input-method
    (toggle-input-method)
    (setq-local sniem-input-method-closed t)))

(defun sniem-insert-mode-init ()
  "Insert mode init."
  (sniem-normal-mode -1)
  (sniem-motion-mode -1)
  (when sniem-input-method-closed
    (toggle-input-method)
    (setq-local sniem-input-method-closed nil)))

(defun sniem-motion-mode-init ()
  "Motion mode init."
  (sniem-normal-mode -1)
  (sniem-insert-mode -1))

(defun sniem--enable ()
  "Unable sniem."
  (unless (apply #'derived-mode-p sniem-close-mode-alist)
    (unless sniem-space-command
      (setq sniem-space-command (key-binding (kbd "SPC"))))
    (cond ((apply #'derived-mode-p sniem-normal-mode-alist)
           (sniem-change-mode 'normal))
          ((apply #'derived-mode-p sniem-insert-mode-alist)
           (sniem-change-mode 'insert))
          (t (sniem-change-mode 'motion)))
    (add-to-list 'emulation-mode-map-alists 'sniem-normal-state-keymap)))

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

(defun sniem-keypad ()
  "Execute the keypad command."
  (interactive)
  (let ((key (pcase last-input-event
               (120 "C-x ") (109 "M-") (98 "C-M-") (118 "C-")))
        tmp)
    (when (null key)
      (setq key (concat "C-" (char-to-string last-input-event) " ")))

    (message key)
    (catch 'stop
      (while (setq tmp (read-char))
        (if (= tmp 127)
            (setq key (substring key 0 -2))
          (when (= tmp 59)
            (keyboard-quit))
          (setq key (concat key
                            (cond ((= tmp 32) (concat (char-to-string (read-char)) " "))
                                  ((= tmp 44) "C-")
                                  ((= tmp 46) "M-")
                                  ((= tmp 47) "C-M-")
                                  (t (concat (char-to-string tmp) " "))))))
        (message key)
        (when (commandp (setq tmp (key-binding (read-kbd-macro (substring key 0 -1)))))
          (throw 'stop nil))))
    (call-interactively tmp)))

(defun sniem-move-last-point ()
  "Move the last point to current point."
  (interactive)
  (setq-local sniem-last-point (point))
  (sniem-lock-unlock-last-point))

;;; Functional functions

(defun sniem-initialize ()
  "Initialize sniem."
  (unless (minibufferp)
    (sniem-mode t)))

(defun sniem--ele-exists-p (ele list)
  "Check if ELE is belong to the LIST."
  (catch 'exists
    (dolist (item list)
      (when (equal ele item)
        (throw 'exists t)))))

(defun sniem-change-mode (mode)
  "Change editing MODE."
  (unless (eq (sniem-current-mode) mode)
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
  "Set the leader KEY for normal mode."
  (define-key sniem-normal-state-keymap (kbd key) sniem-leader-keymap))

(defun sniem-leader-set-key (&rest keys)
  "Bind key to leader keymap.

\(fn KEY FUNC...)
Optional argument KEYS are the keys you want to add."
  (let (key func)
    (while keys
      (setq key (pop keys)
            func (pop keys))
      (define-key sniem-leader-keymap (kbd key) func))))

(defun sniem-normal-set-key (&rest keys)
  "Bind key to normal mode keymap.

\(fn KEY FUNC...)
Optional argument KEYS are the keys you want to add."
  (let (key func)
    (while keys
      (setq key (pop keys)
            func (pop keys))
      (define-key sniem-normal-state-keymap (kbd key) func))))

(defun sniem-set-keyboard-layout (layout)
  "Set the keyboard layout, then you can use the default keymap for your layout.

LAYOUT can be qwerty, colemak or dvorak."
  (pcase layout
    ('qwerty (sniem-normal-set-key
              "e" 'sniem-join
              "u" 'undo
              "k" 'sniem-prev-line
              "K" 'sniem-5-prev-line
              "j" 'sniem-next-line
              "J" 'sniem-5-next-line
              "i" 'sniem-insert
              "I" 'sniem-insert-line
              "h" 'sniem-backward-char
              "H" 'sniem-5-backward-char
              "l" 'sniem-forward-char
              "L" 'sniem-5-forward-char
              "n" 'sniem-lock-unlock-last-point
              "N" 'sniem-goto-last-point
              "t" 'sniem-next-symbol
              "T" 'sniem-prev-symbol)
             (setq sniem-keyboard-layout 'qwerty))
    ('colemak (sniem-normal-set-key
               "j" 'sniem-join
               "l" 'undo
               "u" 'sniem-prev-line
               "U" 'sniem-5-prev-line
               "e" 'sniem-next-line
               "E" 'sniem-5-next-line
               "h" 'sniem-insert
               "H" 'sniem-insert-line
               "n" 'sniem-backward-char
               "N" 'sniem-5-backward-char
               "i" 'sniem-forward-char
               "I" 'sniem-5-forward-char
               "k" 'sniem-lock-unlock-last-point
               "K" 'sniem-goto-last-point
               "t" 'sniem-next-symbol
               "T" 'sniem-prev-symbol)
              (setq sniem-keyboard-layout 'colemak))
    ((or 'dvp 'dvorak)
     (sniem-normal-set-key
      "j" 'sniem-join
      "u" 'undo
      "e" 'sniem-prev-line
      "E" 'sniem-5-prev-line
      "n" 'sniem-next-line
      "N" 'sniem-5-next-line
      "i" 'sniem-insert
      "I" 'sniem-insert-line
      "h" 'sniem-backward-char
      "H" 'sniem-5-backward-char
      "t" 'sniem-forward-char
      "T" 'sniem-5-forward-char
      "k" 'sniem-lock-unlock-last-point
      "K" 'sniem-goto-last-point
      "l" 'sniem-next-symbol
      "L" 'sniem-prev-symbol)
     (setq sniem-keyboard-layout (if (eq layout 'dvp)
                                     'dvp
                                   'dvorak)))
    (_ (user-error "[Sniem]: The %s layout is not supplied!" layout))))

(defun sniem-digit-argument-or-fn (arg)
  "The digit argument function.
Argument ARG is the `digit-argument' result."
  (interactive (list (sniem-digit-argument-get)))
  (if (listp arg)
      (eval arg)
    (prefix-command-preserve-state)
    (setq prefix-arg arg)
    (universal-argument--mode)))

(defun sniem-digit-argument-get (&optional msg)
  "A function which make you can use the middle of the keyboard.
Instead of the num keyboard.
Optional argument MSG is the message which will be outputed."
  (interactive)
  (let ((number "")
        (arg "")
        fn)
    (while (not (string= number "over"))
      (setq number (sniem-digit-argument-read-char))
      (unless (string= number "over")
        (cond ((string= number "delete")
               (setq arg (substring arg 0 -1)))
              ((setq fn (sniem-digit-argument-fn-get number))
               (setq number "over"))
              (t (setq arg (concat arg number)))))
      (message "%s%s" (if msg
                          msg
                        "C-u ")
               arg))
    (setq arg (if (string-empty-p arg)
                  nil
                (string-to-number arg)))
    (if fn
        (if arg
            `(funcall-interactively ',fn ,arg)
          `(call-interactively ',fn))
      arg)))

(defun sniem-digit-argument-fn-get (string)
  "Read the fn for `sniem-digit-argument-or-fn'.
Argument STRING is the string get from the input."
  (pcase string
    ("." 'sniem-lock-unlock-last-goto-point)
    (" " 'sniem-move-with-hint-num)
    ("/" 'sniem-object-catch-direction-reverse)
    ("," 'sniem-object-catch-repeat)
    ("p" 'sniem-pair)
    ("m" 'sniem-mark-jump-insert-with-name)
    ("<" 'sniem-mark-jump-prev)
    (">" 'sniem-mark-jump-next)))

(defun sniem-digit-argument-read-char ()
  "Read char for `sniem-digit-argument'."
  (pcase sniem-keyboard-layout
    ('colemak
     (pcase (read-char)
       (97 "1") (114 "2") (115 "3") (116 "4") (100 "5")
       (104 "6") (110 "7") (101 "8") (105 "9") (111 "0")
       (39 "-") (13 "over") (127 "delete") (59 (keyboard-quit))
       (x (char-to-string x))))
    ('qwerty
     (pcase (read-char)
       (97 "1") (115 "2") (100 "3") (102 "4") (103 "5")
       (104 "6") (106 "7") (107 "8") (108 "9") (59 "0")
       (39 "-") (13 "over") (127 "delete") (59 (keyboard-quit))
       (x (char-to-string x))))
    ('dvorak
     (pcase (read-char)
       (97 "1") (111 "2") (101 "3") (117 "4") (105 "5")
       (100 "6") (104 "7") (116 "8") (110 "9") (115 "0")
       (45 "-") (13 "over") (127 "delete") (59 (keyboard-quit))
       (x (char-to-string x))))))

(defun sniem-lock-unlock-last-point (&optional lock)
  "LOCK or unlock `sniem-last-point'."
  (interactive)
  (setq-local sniem-last-point-locked (if (and (null lock) sniem-last-point-locked)
                                          nil
                                        t))
  (sniem-show-last-point (not sniem-last-point-locked))
  (message "[Sniem]: Last point %s." (if sniem-last-point-locked
                                         "locked"
                                       "unlocked")))

(defun sniem-lock-unlock-last-goto-point (&optional lock)
  "LOCK/unlock the `sniem-last-goto-point'."
  (interactive "P")
  (if (not (region-active-p))
      (if (and (null lock) sniem-mark-content-overlay)
          (progn
            (delete-overlay sniem-mark-content-overlay)
            (setq-local sniem-mark-content-overlay nil))
        (when (and (numberp lock) (= lock 0))
          (setq lock nil))
        (setq-local sniem-last-goto-point (if (or lock (null sniem-last-goto-point))
                                              (point)
                                            nil))
        (message "[Sniem]: Goto point was %s." (if sniem-last-goto-point
                                                   "set"
                                                 "unset")))
    (when sniem-mark-content-overlay
      (delete-overlay sniem-mark-content-overlay))
    (let ((overlay (make-overlay (region-beginning) (region-end))))
      (deactivate-mark)
      (overlay-put overlay 'face 'region)
      (setq-local sniem-mark-content-overlay overlay))))

(defun sniem-show-last-point (&optional hide)
  "Show the last point.
Optional argument HIDE is t, the last point will be show."
  (let ((cursor-color
         `((t (:foreground ,(frame-parameter nil 'background-color))
              :background ,(frame-parameter nil 'cursor-color)))))
    (if (or sniem-last-point-overlay hide)
        (progn
          (delete-overlay sniem-last-point-overlay)
          (setq-local sniem-last-point-overlay nil))
      (setq-local sniem-last-point-overlay
                  (make-overlay sniem-last-point (1+ sniem-last-point) (current-buffer) t t))
      (overlay-put sniem-last-point-overlay 'face cursor-color))))

(defun sniem-motion-hint (motion)
  "Hint after MOTION."
  (let (overlay point)
    (when sniem-motion-hint-overlays
      (mapc #'delete-overlay sniem-motion-hint-overlays)
      (setq sniem-motion-hint-overlays nil))
    (save-mark-and-excursion
      (catch 'stop
        (dotimes (i 10)
          (call-interactively motion)
          (if (and point (= (point) point))
              (throw 'stop nil)
            (setq overlay (make-overlay (point) (1+ (point))))
            (overlay-put overlay 'display (format "%s%s"
                                                  (propertize (number-to-string (1+ i))
                                                              'face 'sniem-motion-hint-face)
                                                  (pcase (following-char)
                                                    ((pred (= 10)) "\n")
                                                    ((pred (= 9)) "\t")
                                                    (_ ""))))
            (setq point (point))
            (push overlay sniem-motion-hint-overlays)))))
    (sit-for sniem-motion-hint-sit-time)
    (mapc #'delete-overlay sniem-motion-hint-overlays)
    (setq sniem-motion-hint-overlays nil)
    (setq-local sniem-motion-hint-motion motion)))

(defun sniem-move-with-hint-num (num)
  "Move with NUM to eval the last `sniem-motion-hint-motion'."
  (interactive "P")
  (dotimes (_ num)
    (funcall-interactively sniem-motion-hint-motion))
  (sniem-motion-hint sniem-motion-hint-motion))

(defun sniem-set-quit-insert-key (key)
  "Set the `sniem-quit-insert' KEY."
  (define-key sniem-insert-state-keymap (kbd sniem-insert-quit-key) 'nil)
  (define-key sniem-insert-state-keymap (kbd key) 'sniem-quit-insert)
  (setq sniem-insert-quit-key key))

;;; Initialize
(sniem-set-leader-key ",")

(require 'sniem-object-catch)
(require 'sniem-cheatsheet)
(require 'sniem-mark-jump)

;;; Third-Party Settings
(advice-add 'wdired-change-to-wdired-mode :after #'sniem-normal-mode)
(advice-add 'wdired-change-to-dired-mode :after #'sniem-motion-mode)

;;; State info print support
(defun sniem-state ()
  "The function to show the current sniem state."
  (pcase (sniem-current-mode)
    ('normal (format "[N:%s%s%s]"
                     (if sniem-object-catch-forward-p ">" "<")
                     (if sniem-last-point-locked ":l" "")
                     (if sniem-last-goto-point ":L" "")))
    ('insert "[I]")
    ('motion "[M]")))
(when (featurep 'awesome-tray)
  (add-to-list 'awesome-tray-module-alist '("sniem-state" . (sniem-state awesome-tray-module-evil-face))))

(provide 'sniem)

;;; sniem.el ends here
