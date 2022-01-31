;;; sniem.el --- Hands-eased united editing method -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (s "2.12.0") (dash "1.12.0"))
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

;; Hands-eased united editing method.

;;; Code:

(require 's)
(require 'dash)

(defgroup sniem nil
  "The group for sniem."
  :group 'applications)

(require 'sniem-var)
(require 'sniem-macro)
(require 'sniem-operation)
(require 'sniem-object-catch)
(require 'sniem-cheatsheet)
(require 'sniem-mark-jump)


(define-minor-mode sniem-mode
  "Hands-eased united editing method mode."
  nil nil sniem-mode-keymap
  (if sniem-mode
      (sniem--enable)
    (sniem--disable)))

;;;###autoload
(define-globalized-minor-mode global-sniem-mode
  sniem-mode sniem-initialize)

;;;###autoload
(define-minor-mode sniem-normal-mode
  "Normal mode for sniem."
  nil nil sniem-normal-state-keymap
  (if sniem-normal-mode
      (sniem-normal-mode-init)
    (sniem-search--cancel-selection)))

;;;###autoload
(define-minor-mode sniem-insert-mode
  "Insert mode for sniem."
  nil nil sniem-insert-state-keymap
  (when sniem-insert-mode
    (sniem-insert-mode-init)))

;;;###autoload
(define-minor-mode sniem-motion-mode
  "Motion mode for sniem."
  nil nil sniem-motion-state-keymap
  (when sniem-motion-mode
    (sniem-motion-mode-init)))

;;;###autoload
(define-minor-mode sniem-expand-mode
  "Expand mode for sniem."
  nil nil sniem-expand-state-keymap
  (when sniem-expand-mode
    (sniem-expand-mode-init)))

;;;###autoload
(define-minor-mode sniem-minibuffer-keypad-mode
  nil nil sniem-minibuffer-keypad-state-keymap
  (when sniem-minibuffer-keypad-mode
    (sniem-minibuffer-keypad-mode-init)))

(defun sniem-normal-mode-init ()
  "Normal mode init."
  (sniem-insert-mode -1)
  (sniem-motion-mode -1)
  (sniem-expand-mode -1)
  (sniem-minibuffer-keypad-mode -1)
  (when current-input-method
    (toggle-input-method)
    (setq-local sniem-input-method-closed t)))

(defun sniem-insert-mode-init ()
  "Insert mode init."
  (sniem-normal-mode -1)
  (sniem-motion-mode -1)
  (sniem-expand-mode -1)
  (sniem-minibuffer-keypad-mode -1)
  (when sniem-input-method-closed
    (toggle-input-method)
    (setq-local sniem-input-method-closed nil)))

(defun sniem-motion-mode-init ()
  "Motion mode init."
  (sniem-normal-mode -1)
  (sniem-insert-mode -1)
  (sniem-expand-mode -1)
  (sniem-minibuffer-keypad-mode -1))

(defun sniem-expand-mode-init ()
  "Expand mode init."
  (sniem-normal-mode -1)
  (sniem-insert-mode -1)
  (sniem-motion-mode -1)
  (sniem-minibuffer-keypad-mode -1))

(defun sniem-minibuffer-keypad-mode-init ()
  "Minibuffer-keypad mode init."
  (sniem-normal-mode -1)
  (sniem-insert-mode -1)
  (sniem-motion-mode -1)
  (sniem-expand-mode -1))

(defun sniem--enable ()
  "Unable sniem."
  (unless (apply #'derived-mode-p sniem-close-mode-alist)
    (unless sniem-space-command
      (setq-local sniem-space-command (key-binding (kbd "SPC"))))
    (cond ((apply #'derived-mode-p sniem-normal-mode-alist)
           (sniem-change-mode 'normal))
          ((apply #'derived-mode-p sniem-insert-mode-alist)
           (sniem-change-mode 'insert))
          ((minibufferp))
          (t (sniem-change-mode 'motion)))
    (unless sniem-initialized
      (add-to-ordered-list 'emulation-mode-map-alists
                           `((sniem-minibuffer-keypad-mode . ,sniem-minibuffer-keypad-state-keymap)))
      (add-to-ordered-list 'emulation-mode-map-alists
                           `((sniem-expand-mode . ,sniem-expand-state-keymap)))
      (add-to-ordered-list 'emulation-mode-map-alists
                           `((sniem-motion-mode . ,sniem-motion-state-keymap)))
      (add-to-ordered-list 'emulation-mode-map-alists
                           `((sniem-normal-mode . ,sniem-normal-state-keymap)))
      (sniem-init-hook)
      (sniem-init-advice)
      (when (featurep 'awesome-tray)
        (defvar awesome-tray-module-alist)
        (add-to-list 'awesome-tray-module-alist '("sniem-state" . (sniem-state awesome-tray-module-evil-face))))
      (setq sniem-initialized t))))

(defun sniem--disable ()
  "Disable sniem."
  (sniem-normal-mode -1)
  (sniem-insert-mode -1)
  (sniem-motion-mode -1)
  (when sniem-initialized
    (sniem-init-hook)
    (sniem-init-advice)
    (setq sniem-initialized nil)))

;;; Interactive functions

(defun sniem-expand-with-catch ()
  "Enter expand mode with object catch."
  (interactive)
  (sniem-object-catch)
  (sniem-expand-mode t))

(defun sniem-expand-enter-or-quit ()
  "Quit expand mode."
  (interactive)
  (if sniem-expand-mode
      (progn
        (when sniem-object-catch-expand-p
          (sniem-object-catch-expand))
        (when (and sniem-object-catch-auto-backward
                   sniem-object-catch-forward-p)
          (setq-local sniem-object-catch-forward-p nil))
        (sniem-change-mode 'normal)
        (unless (eq last-input-event 32)
          (call-interactively (key-binding (read-kbd-macro (char-to-string last-input-event))))))
    (sniem-change-mode 'expand)))

(defun sniem-execute-space-command ()
  "Execute space command."
  (interactive)
  (call-interactively sniem-space-command))

(defun sniem-quit-insert ()
  "Quit insert mode."
  (interactive)
  (sniem-change-mode 'normal))

(defun sniem-keypad (&optional external-char no-convert pre-arg)
  "Execute the keypad command.
EXTERNAL-CHAR is the entrance for minibuffer-keypad mode.
NO-CONVERT means not to convert the EXTERNAL-CHAR to prefix.
PRE-ARG is the prefix arg."
  (interactive (list nil nil current-prefix-arg))
  (let* ((prefix-used-p nil)
         (key (if external-char
                  (when (and (null no-convert)
                             (memq external-char '(44 46 47))
                             (/= (sniem-keypad--convert-prefix
                                  sniem-minibuffer-keypad-prefix)
                                 external-char))
                    (setq-local sniem-minibuffer-keypad-prefix
                                (sniem-keypad--convert-prefix external-char))
                    (setq external-char t
                          prefix-used-p t))
                (pcase last-input-event
                  (109 "M-") (98 "C-M-") (118 "C-"))))
         tmp command shift)
    (unless (stringp key)
      (setq key (if external-char
                    (concat sniem-minibuffer-keypad-prefix
                            (when (numberp external-char)
                              (concat (char-to-string external-char) " ")))
                  (concat "C-" (char-to-string last-input-event) " "))))

    (message key)
    (catch 'stop
      (when (and (numberp external-char)
                 (commandp (setq command (key-binding (read-kbd-macro (substring key 0 -1))))))
        (throw 'stop nil))
      (while t
        (setq tmp (if shift
                      (progn
                        (setq shift nil)
                        (sniem-shift-convert (read-char)
                                             sniem-shift-binding-key))
                    (read-char)))
        (if (= tmp 127)
            (setq key (substring key 0 -2))
          (when (= tmp 59)
            (keyboard-quit))
          (setq key (concat key
                            (cond ((and (= tmp 44)
                                        (null prefix-used-p))
                                   (setq prefix-used-p t)
                                   "C-")
                                  ((and (= tmp 46)
                                        (null prefix-used-p))
                                   (setq prefix-used-p t)
                                   "M-")
                                  ((and (= tmp 47)
                                        (null prefix-used-p))
                                   (setq prefix-used-p t)
                                   "C-M-")
                                  ((= tmp 32)
                                   (setq prefix-used-p t)
                                   "")
                                  ((= tmp 9)
                                   (setq shift t)
                                   "")
                                  ((/= tmp 0)
                                   (when prefix-used-p
                                     (setq prefix-used-p nil))
                                   (concat (char-to-string tmp) " "))))))
        (message key)
        (when (commandp (setq command (key-binding (read-kbd-macro (substring key 0 -1)))))
          (throw 'stop nil))))
    (when pre-arg
      (setq prefix-arg pre-arg))
    (setq this-command command)
    (setq real-this-command command)
    (command-execute command 'record)))

(defun sniem-move-last-point ()
  "Move the last point to current point."
  (interactive)
  (setq-local sniem-last-point (point))
  (sniem-lock-unlock-last-point))

(defun sniem-lock-unlock-last-point (&optional lock)
  "LOCK or unlock `sniem-last-point'."
  (interactive)
  (sniem-shift-lock-motion (sniem-goto-last-point) nil
    (setq-local sniem-last-point-locked (if (and (null lock) sniem-last-point-locked)
                                            nil
                                          t))
    (sniem-show-last-point (not sniem-last-point-locked))
    (message "[Sniem]: Last point %s." (if sniem-last-point-locked
                                           "locked"
                                         "unlocked"))))

(defun sniem-keyboard-quit ()
  "Like `keyboard-quit'.
But when it's recording kmacro and there're region, deactivate mark."
  (interactive)
  (if (and (region-active-p) defining-kbd-macro)
      (deactivate-mark)
    (keyboard-quit)))

(defun sniem-special-clipboard-pop ()
  "Pop the last content in special clipboard."
  (interactive)
  (setq sniem-special-clipboard
        (delete (nth (1- (length sniem-special-clipboard))
                     sniem-special-clipboard)
                sniem-special-clipboard))
  (message "[Sniem]: Popped the special clipboard."))

(defun sniem-special-clipboard-clear ()
  "Clear the special clipboard."
  (interactive)
  (setq sniem-special-clipboard nil)
  (message "[Sniem]: Cleared the special clipboard."))

(defun sniem-minibuffer-keypad-start-or-stop ()
  "Start or stop the minibuffer-keypad mode."
  (interactive)
  (if current-input-method
      (if (and (= (char-before) 32)
               (not (= (point) (line-beginning-position))))
          (progn
            (sniem-minibuffer-keypad-mode (if sniem-minibuffer-keypad-mode
                                              -1
                                            t))
            (call-interactively (key-binding (read-kbd-macro (char-to-string 127)))))
        (self-insert-command 1 32))
    (self-insert-command 1 32)
    (let ((char (read-char)))
      (if (= 32 char)
          (progn
            (sniem-minibuffer-keypad-mode (if sniem-minibuffer-keypad-mode
                                              -1
                                            t))
            (call-interactively (key-binding (read-kbd-macro (char-to-string 127)))))
        (if (and sniem-minibuffer-keypad-mode
                 (memq char '(44 46 47)))
            (progn
              (call-interactively (key-binding (read-kbd-macro (char-to-string 127))))
              (sniem-keypad char t))
          (sniem-minibuffer-keypad))))))

(defun sniem-minibuffer-keypad ()
  "The function to insert the input key or execute the function."
  (interactive)
  (if sniem-minibuffer-keypad-mode
      (sniem-keypad last-input-event)
    (if (or (symbolp last-input-event)
            (< last-input-event 33)
            (> last-input-event 126))
        (progn
          (let (command)
            (if (commandp (setq command
                                (key-binding
                                 (vector last-input-event))))
                (let ((last-command-event last-input-event))
                  (ignore-errors
                    (call-interactively command)))
              (execute-kbd-macro (vector last-input-event)))))
      (let ((last-command-event last-input-event))
        (call-interactively #'self-insert-command)))))

;;; Functional functions

(defun sniem-initialize ()
  "Initialize sniem."
  (sniem-mode t))

(defun sniem--ele-exists-p (ele list)
  "Check if ELE is belong to the LIST."
  (catch 'exists
    (dolist (item list)
      (when (equal ele item)
        (throw 'exists t)))))

(defun sniem-cursor-change ()
  "Change cursor type."
  (setq-local cursor-type (pcase (sniem-current-mode)
                            ('normal sniem-normal-mode-cursor)
                            ('insert sniem-insert-mode-cursor)
                            ('motion sniem-motion-mode-cursor)
                            (_ cursor-type))))

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
      (when (and sniem-shift-default-lock-motions
                 (memq func '(sniem-forward-char
                              sniem-backward-char
                              sniem-prev-line
                              sniem-next-line)))
        (let ((original-key (aref (car (where-is-internal func)) 0)))
          (setq sniem-shift-default-lock-motions
                (append
                 (remq original-key sniem-shift-default-lock-motions)
                 (list (string-to-char key))))))
      (define-key sniem-normal-state-keymap (kbd key) func))))

(defun sniem-expand-set-key (&rest keys)
  "Bind key to expand mode keymap.

\(fn KEY FUNC...)
Optional argument KEYS are the keys you want to add."
  (let (key func)
    (while keys
      (setq key (pop keys)
            func (pop keys))
      (define-key sniem-expand-state-keymap (kbd key) func))))

(defun sniem-set-keyboard-layout (layout)
  "Set the keyboard layout, then you can use the default keymap for your layout.

LAYOUT can be qwerty, colemak or dvorak."
  (cond
   ((eq layout 'qwerty)
    (sniem-normal-set-key
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
   ((eq layout 'colemak)
    (sniem-normal-set-key
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
   ((or (eq layout 'dvorak)
        (eq layout 'dvp))
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
   (t (user-error "[Sniem]: The %s layout is not supplied!" layout))))

(defun sniem-current-mode ()
  "Get current mode."
  (cond (sniem-normal-mode 'normal)
        (sniem-insert-mode 'insert)
        (sniem-motion-mode 'motion)
        (sniem-expand-mode 'expand)
        (sniem-minibuffer-keypad-mode 'minibuffer-keypad)
        (t nil)))

(defun sniem-change-mode (mode)
  "Change editing MODE."
  (let ((current-mode (sniem-current-mode)))
    (unless (eq current-mode mode)
      (pcase mode
        ('normal (sniem-normal-mode t)
                 (when (eq current-mode 'insert)
                   (run-hooks 'sniem-insert-to-normal-hook)))
        ('insert (sniem-insert-mode t)
                 (when (eq current-mode 'normal)
                   (run-hooks 'sniem-normal-to-insert-hook)))
        ('motion (sniem-motion-mode t))
        ('expand (sniem-expand-mode t))
        ('minibuffer-keypad (sniem-minibuffer-keypad-mode t)))
      (sniem-cursor-change))))

(defun sniem-digit-argument-or-fn (arg)
  "The digit argument function.
Argument ARG is the `digit-argument' result."
  (interactive (list (ignore-errors (sniem-digit-argument-get))))
  (unless arg
    (setq arg '(4)))
  (if (and (listp arg)
           (> (length arg) 1))
      (eval arg)
    (prefix-command-preserve-state)
    (setq prefix-arg arg)
    (universal-argument--mode)))

(defun sniem-digit-argument-fn-get (string)
  "Read the fn for `sniem-digit-argument-or-fn'.
Argument STRING is the string get from the input."
  (pcase string
    ("." 'sniem-mark-content)
    ("k" 'sniem-unmark-content-select-it)
    ("K" 'sniem-mark-content-pop)
    (" " 'sniem-move-with-hint-num)
    ("/" 'sniem-object-catch-direction-reverse)
    ("," 'sniem-object-catch-repeat)
    ("p" 'sniem-pair)

    ("m" 'sniem-mark-jump-insert-with-name)
    ("<" 'sniem-mark-jump-prev)
    (">" 'sniem-mark-jump-next)
    ("c" 'sniem-special-clipboard-clear)
    ("x" 'sniem-special-clipboard-pop)
    ("P" (lambda ()
           (interactive)
           (funcall-interactively #'sniem-paste nil t)))
    ("y" (lambda ()
           (interactive)
           (funcall-interactively #'sniem-yank nil t)))
    ("Y" (lambda ()
           (interactive)
           (sniem-yank-in-region t)))))

(defun sniem-digit-argument-read-char ()
  "Read char for `sniem-digit-argument'."
  (pcase sniem-keyboard-layout
    ('colemak
     (pcase (read-char)
       (97 "1") (114 "2") (115 "3") (116 "4") (100 "5")
       (104 "6") (110 "7") (101 "8") (105 "9") (111 "0")
       (39 "-") (13 "over") (127 "delete") (59 nil)
       (9 (char-to-string (sniem-shift-convert (read-char) sniem-shift-binding-key)))
       (x (char-to-string x))))
    ('qwerty
     (pcase (read-char)
       (97 "1") (115 "2") (100 "3") (102 "4") (103 "5")
       (104 "6") (106 "7") (107 "8") (108 "9") (59 "0")
       (39 "-") (13 "over") (127 "delete") (92 nil)
       (9 (char-to-string (sniem-shift-convert (read-char) sniem-shift-binding-key)))
       (x (char-to-string x))))
    ('dvorak
     (pcase (read-char)
       (97 "1") (111 "2") (101 "3") (117 "4") (105 "5")
       (100 "6") (104 "7") (116 "8") (110 "9") (115 "0")
       (45 "-") (13 "over") (127 "delete") (59 nil)
       (9 (char-to-string (sniem-shift-convert (read-char) sniem-shift-binding-key)))
       (x (char-to-string x))))))

(defun sniem-mark-content (&optional mark)
  "Mark/unmark the content.
Optional Argument MARK means mark forcibly.
Optional Argument POINTS is the points of the content to mark."
  (interactive "P")
  (let* ((add-ov (lambda (ov)
                   (setq-local sniem-mark-content-overlay
                               (append (list ov) sniem-mark-content-overlay))))
         (mark-content (lambda ()
                         (if (region-active-p)
                             (progn
                               (funcall add-ov (make-overlay (region-beginning) (region-end)))
                               (deactivate-mark))
                           (funcall add-ov (make-overlay (point) (1+ (point)))))
                         (overlay-put (car sniem-mark-content-overlay) 'face 'region))))
    
    (cond ((and (listp sniem-mark-content-overlay) mark) ;Clear all the marked-contents
           (dolist (ov sniem-mark-content-overlay)
             (delete-overlay ov))
           (setq-local sniem-mark-content-overlay nil))

          ((not (sniem--list-memq sniem-mark-content-overlay ;If the content under cursor hadn't been marked, mark it.
                                  (overlays-at (point))))
           (funcall mark-content))
          
          (t (let ((ov (sniem--list-memq sniem-mark-content-overlay ;Remove the content under cursor from marked-content overlays.
                                         (overlays-at (point)))))
               (delete-overlay ov)
               (setq-local sniem-mark-content-overlay
                           (delete ov sniem-mark-content-overlay)))))))

(defun sniem-unmark-content-select-it ()
  "Unmark the marked content under cursor and select it."
  (interactive)
  (let ((ov (sniem--list-memq sniem-mark-content-overlay
                              (overlays-at (point))))
        points)
    (when ov
      (setq points (cons (overlay-start ov) (overlay-end ov)))
      (delete-overlay ov)
      (setq-local sniem-mark-content-overlay
                  (delete ov sniem-mark-content-overlay))
      (goto-char (car points))
      (push-mark (cdr points) t t))))

(defun sniem-mark-content-pop ()
  "Remove the first marked content from list."
  (interactive)
  (pop sniem-mark-content-overlay))

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

(defun sniem-set-quit-insert-key (key)
  "Set the `sniem-quit-insert' KEY."
  (define-key sniem-insert-state-keymap (kbd sniem-insert-quit-key) 'nil)
  (define-key sniem-insert-state-keymap (kbd key) 'sniem-quit-insert)
  (setq sniem-insert-quit-key key))

(defun sniem-shift (&optional arg)
  "The function to replace shift key.
ARG is the prefix-arg."
  (interactive "P")
  (let (char result)
    (while (not (setq char (sniem-shift-convert (read-char)
                                                sniem-shift-binding-key))))
    (when (numberp char)
      (when arg
        (sniem-digit-argument-or-fn arg))
      (setq result (key-binding (vector char)))
      (if (keymapp result)
          (set-transient-map result)
        (setq last-command-event char
              last-command result)
        (call-interactively result)))))

(defun sniem-shift-convert (char shift-key)
  "Convert the char if it has shift-key.
CHAR is the last input char.
SHIFT-KEY is the shift key bound by user."
  (pcase sniem-shift-times
    (2 (setq sniem-shift-times 1)
       (pcase char
         ((pred (= shift-key))
          (setq-local sniem-shift-lock
                      (if sniem-shift-lock
                          (progn
                            (remove-hook 'pre-command-hook #'sniem-shift-lock-convert t)
                            nil)
                        (add-hook 'pre-command-hook #'sniem-shift-lock-convert nil t)
                        t))
          (message "[Sniem]: Shift Lock %s in current buffer."
                   (if sniem-shift-lock
                       "opened"
                     "closed"))
          t)
         (32 t)
         (_ (when sniem-normal-mode
              (let (locked)
                (setq-local sniem-shift-lock-motions
                            (if (car sniem-shift-lock-motions)
                                '(nil)
                              (setq locked t)
                              (if sniem-shift-default-lock-motions
                                  sniem-shift-default-lock-motions
                                (setq sniem-shift-default-lock-motions
                                      (list t
                                            (aref (car (where-is-internal 'sniem-forward-char)) 0)
                                            (aref (car (where-is-internal 'sniem-backward-char)) 0)
                                            (aref (car (where-is-internal 'sniem-prev-line)) 0)
                                            (aref (car (where-is-internal 'sniem-next-line)) 0))))))
                (message "[Sniem]: Shift Motion Lock %s in current buffer."
                         (if locked
                             "opened"
                           "closed")))
              char))))
    (1 (pcase char
         ((pred (= shift-key))
          (setq sniem-shift-times 2)
          nil)
         (32 t)
         (_ (if (sniem-pair--pair-p char t)
                (if (eq sniem-keyboard-layout 'dvp)
                    (pcase char
                      (?$ ?~) (?& ?%) (91 ?7) (123 ?5) (125 ?3) (40 ?1) (?= ?9)
                      (?* ?0) (41 ?2) (?+ ?4) (93 ?6) (?! ?8) (?# ?~) (59 ?:)
                      (?, 60) (?. 62) (?/ ??) (?@ ?^) (124 ?|) (?- ?_) (39 34))
                  (pcase char
                    (?` ?~) (?1 ?!) (?2 ?@) (?3 ?#) (?4 ?$) (?5 ?%) (?6 ?^) 
                    (?7 ?&) (?8 ?*) (?9 40) (?0 41) (?- ?_) (?= ?+) (59 ?:)
                    (91 123) (93 125) (39 34) (92 124) (?, 60) (?. 62) (?/ ??)))
              (let ((result (upcase char)))
                (when (and (not (car sniem-shift-lock-motions))
                           (= result char)
                           (setq result (downcase char)))
                  (setq-local sniem-shift-lock-motions
                              (if (memq result sniem-shift-lock-motions)
                                  (delete result sniem-shift-lock-motions)
                                (append sniem-shift-lock-motions
                                        (list result)))))
                result)))))
    (_ (user-error "[Sniem]: The sniem-shift-times is error!"))))

(defun sniem-shift-lock-convert ()
  "The function to play caps_lock's role."
  (when (and sniem-insert-mode
             (characterp last-command-event)
             (or (memq this-command '(self-insert-command isearch-printing-char))
                 (eq this-command (key-binding [remap self-insert-command]))))
    (setq last-command-event
          (condition-case nil
              (let ((char (upcase last-command-event)))
                (if (eq char last-command-event)
                    (downcase char)
                  char))
            (error last-command-event)))))

;;; Initialize
(sniem-set-leader-key ",")

;;; Third-Party Settings
(defun sniem-init-hook ()
  "The inin functions."
  (let ((fn (if sniem-initialized
                'remove-hook
              'add-hook)))
    (funcall fn 'deactivate-mark-hook
             (lambda ()
               (when sniem-mark-line
                 (setq-local sniem-mark-line nil))
               (when sniem-object-catch-last-points
                 (setq-local sniem-object-catch-last-points nil))
               (when sniem-object-catch-prefix-string-p
                 (setq-local sniem-object-catch-prefix-string-p nil))))
    (funcall fn 'minibuffer-setup-hook
             (lambda ()
               (define-key (current-local-map) (kbd "SPC") #'sniem-minibuffer-keypad-start-or-stop)))))

(defun sniem-init-advice ()
  "The init function for advice."
  (if sniem-initialized
      (progn
        (advice-remove 'keyboard-quit
                       (lambda ()
                         (when sniem-kmacro-mark-content
                           (setq-local sniem-kmacro-mark-content nil))
                         (sniem-search--cancel-selection)))
        (advice-remove 'wdired-change-to-wdired-mode #'sniem-normal-mode)
        (advice-remove 'wdired-change-to-dired-mode #'sniem-motion-mode)
        (advice-remove 'org-cycle #'sniem-org-cycle-advice)
        (when (and (featurep 'yasnippet)
                   (sniem-yas--tab-used-p))
          (advice-remove 'yas-expand-from-trigger-key #'sniem-yasnippet-advice-1)))
    (advice-add 'keyboard-quit :before
                (lambda ()
                  (when sniem-kmacro-mark-content
                    (setq-local sniem-kmacro-mark-content nil))
                  (sniem-search--cancel-selection)))
    (advice-add 'wdired-change-to-wdired-mode :after #'sniem-normal-mode)
    (advice-add 'wdired-change-to-dired-mode :after #'sniem-motion-mode)
    (advice-add 'org-cycle :around #'sniem-org-cycle-advice)
    (when (and (featurep 'yasnippet)
               (sniem-yas--tab-used-p))
      (advice-add 'yas-expand-from-trigger-key :around #'sniem-yasnippet-advice-1))))

;;; Support for yasnippet
(unless (featurep 'yasnippet)
  (defun yas-expand-from-trigger-key ())
  (defvar yas-keymap))

(defun sniem-yasnippet-advice-1 (orig &optional field)
  "The yasnippet advice for `yas-expand-from-trigger-key'."
  (pcase (sniem-current-mode)
    ('insert (apply orig field))
    ('nil nil)
    (_ (call-interactively #'sniem-shift))))

(defun sniem-org-cycle-advice (orig &optional arg)
  "The advice for `org-cycle'."
  (if (or (bolp)
          sniem-insert-mode)
      (apply orig arg)
    (call-interactively #'sniem-shift)))

(defun sniem-yas--tab-used-p ()
  "Check if yasnippet used tab."
  (eq (nth 2 (alist-get 9 yas-keymap))
      'yas-next-field-or-maybe-expand))

(defun sniem-keypad--convert-prefix (prefix)
  "Convert PREFIX from string to char or from char to string."
  (let* ((prefix-string '("C-" "M-" "C-M-"))
         (prefix-char '(44 46 47))
         (from (if (stringp prefix)
                   prefix-string
                 prefix-char))
         (to (if (stringp prefix)
                 prefix-char
               prefix-string))
         index)
    (setq index (sniem--index prefix from))
    (when index
      (nth index to))))

;;; State info print support
(defun sniem-state ()
  "The function to show the current sniem state."
  (pcase (sniem-current-mode)
    ('normal (format "[N:%s%s%s]"
                     (if sniem-object-catch-forward-p ">" "<")
                     (if sniem-last-point-locked ":l" "")
                     (if sniem-mark-content-overlay
                         (format ":%d" (length sniem-mark-content-overlay))
                       "")))
    ('insert (format "[I:%s]"
                     (if sniem-shift-lock
                         "A"
                       "a")))
    ('motion "[M]")
    ('expand (format "[E:%s]"
                     (if sniem-object-catch-forward-p ">" "<")))))

(provide 'sniem)

;;; sniem.el ends here
