;;; sniem-common.el --- Simple united editing method -*- lexical-binding: t -*-

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

;; Simple united editing method

;;; Code:

(defun sniem-current-mode ()
  "Get current mode."
  (cond (sniem-normal-mode 'normal)
        (sniem-insert-mode 'insert)
        (sniem-motion-mode 'motion)
        (t nil)))

(defun sniem-change-mode (mode)
  "Change editing MODE."
  (unless (eq (sniem-current-mode) mode)
    (pcase mode
      ('normal (sniem-normal-mode t))
      ('insert (sniem-insert-mode t))
      ('motion (sniem-motion-mode t)))
    (sniem-cursor-change)))

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

(defun sniem-object-catch--get-second-char (prefix)
  "Get the second char by the PREFIX."
  (catch 'second-char
    (dolist (char-cons sniem-object-catch-global-symbol-alist)
      (when (string= prefix (car char-cons))
        (throw 'second-char (cdr-safe char-cons))))))

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

(defun sniem--mems (ele list)
  "Like memq, but use `string-equal'.
Argument ELE is the element to check.
Argument LIST is the list to check."
  (let (result)
    (catch 'stop
      (dolist (item list)
        (when (string-equal item ele)
          (setq result t)
          (throw 'stop nil))))
    result))

(provide 'sniem-common)

;;; sniem-common.el ends here
