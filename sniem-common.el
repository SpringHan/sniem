;;; sniem-common.el --- Hands-eased united editing method -*- lexical-binding: t -*-

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

;; Hands-eased united editing method

;;; Code:

(require 'sniem-var)

(declare-function sniem-digit-argument-read-char "sniem")
(declare-function sniem-digit-argument-fn-get "sniem")

(defun sniem--last-point-position ()
  "Return the last point position in the current buffer, if valid.
The position is nil when the marker is dead or belongs to another
buffer, or when an older integer value is out of range."
  (let ((position (cond
                   ((markerp sniem-last-point)
                    (when (eq (marker-buffer sniem-last-point)
                              (current-buffer))
                      (marker-position sniem-last-point)))
                   ((integerp sniem-last-point)
                    sniem-last-point))))
    (when (and position
               (<= (point-min) position)
               (<= position (point-max)))
      position)))

(defun sniem--record-last-point (position)
  "Record POSITION in the last-point trail, most recent first.
Dead, foreign, and duplicate positions are dropped so the trail stays
useful as a short history."
  (let (markers)
    (dolist (marker sniem-last-point-trail)
      (let ((entry-position (and (markerp marker)
                                 (eq (marker-buffer marker) (current-buffer))
                                 (marker-position marker))))
        (when (and entry-position
                   (<= (point-min) entry-position)
                   (<= entry-position (point-max))
                   (/= entry-position position))
          (push marker markers))))
    (setq sniem-last-point-trail
          (cons (copy-marker position) (nreverse markers)))
    (let ((size (max 1 (or sniem-last-point-trail-size 16))))
      (when (> (length sniem-last-point-trail) size)
        (setq sniem-last-point-trail
              (butlast sniem-last-point-trail
                       (- (length sniem-last-point-trail) size))))))
  sniem-last-point-trail)

(defun sniem--set-last-point (position)
  "Record POSITION as the buffer's last point.
Reuse the existing marker so overlays and any code holding it keep
tracking the same position.  The previous position is pushed onto the
last-point trail before the marker moves."
  (let ((old-position (sniem--last-point-position)))
    (setq position (max (point-min) (min position (point-max))))
    (unless (or (null old-position) (= old-position position))
      (sniem--record-last-point old-position))
    (if (and (markerp sniem-last-point)
             (eq (marker-buffer sniem-last-point) (current-buffer)))
        (move-marker sniem-last-point position)
      (setq-local sniem-last-point (copy-marker position))))
  sniem-last-point)

(defun sniem--last-point-span ()
  "Return the ordered range between the last point and point.
The range is empty when both positions are equal.  Return nil when
there is no usable last point."
  (let ((last-point (sniem--last-point-position)))
    (when last-point
      (cons (min last-point (point)) (max last-point (point))))))

(defun sniem-motion-hint (motion)
  "Hint after MOTION."
  (let (overlay point)
    (when sniem-motion-hint-remove-timer
      (cancel-timer sniem-motion-hint-remove-timer))
    (sniem--motion-hint-remove)

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
    (setq-local sniem-motion-hint-motion motion)
    (setq sniem-motion-hint-remove-timer
          (run-with-timer sniem-motion-hint-sit-time nil
                          #'sniem--motion-hint-remove))
    (sit-for sniem-motion-hint-sit-time)
    (when sniem-motion-hint-remove-timer
      (cancel-timer sniem-motion-hint-remove-timer)
      (sniem--motion-hint-remove))))

(defun sniem--motion-hint-remove ()
  "Remove motion hint overlays."
  (when sniem-motion-hint-overlays
    (mapc #'delete-overlay sniem-motion-hint-overlays)
    (setq sniem-motion-hint-overlays nil))
  (setq sniem-motion-hint-remove-timer nil))

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
        (universal-times 0)
        fn)
    (while (not (string= number "over"))
      (setq number (sniem-digit-argument-read-char))
      (unless (string= number "over")
        (cond ((string= number "delete")
               (setq arg (substring arg 0 -1)))
              ((setq fn (sniem-digit-argument-fn-get number))
               (setq number "over"))
              ((string= number "U")     ;For C-u
               (setq universal-times (1+ universal-times))
               (setq arg (concat arg "C-u ")))
              ((null number)
               (error nil))
              (t (setq arg (concat arg number)))))
      (message "%s%s" (if msg
                          msg
                        "C-u ")
               arg))
    (setq arg (if (string= "" arg)
                  nil
                (if (/= universal-times 0)
                    (list (expt 4 (1+ universal-times)))
                  (string-to-number arg))))
    (if fn
        (if arg
            `(funcall-interactively ',fn ,arg)
          `(call-interactively ',fn))
      arg)))

(defun sniem--mems (ele list &optional prefix)
  "Like memq, but use `string-equal'.
Argument ELE is the element to check.
Argument LIST is the list to check.
When PREFIX is non-nil, check if ELE is the prefix."
  (if (stringp list)
      (string= ele list)
    (catch 'stop
      (dolist (item list)
        (when (stringp item)
          (if (string-equal item ele)
              (throw 'stop t)
            (when (and prefix
                       (string-prefix-p ele item))
              (throw 'stop (list t)))))))))

(defun sniem--list-memq (list1 list2 &optional return-type)
  "Check if there are ele of LIST1 which are also in LIST2.
Optional Argument RETURN-TYPE is the type of the return value.
It can be 'index or 'ele.  Defaultly it's 'ele.

If it's true, return the value of the ele.
Otherwise nil will be return."
  (let (result ele)
    (when (and list1 list2)
      (catch 'stop
        (dotimes (i (length list1))
          (setq ele (nth i list1))
          (when (memq ele list2)
            (setq result (if (eq return-type 'index)
                             i
                           ele))
            (throw 'stop nil)))))
    result))

(defun sniem--assoc-with-list-value (value alist)
  "Get a cons with its VALUE in ALIST.
And VALUE is a list."
  (catch 'result
    (when alist
      (dolist (e alist)
        (when (memq (cdr e) value)
          (throw 'result e))))))

(defun sniem--nth-utill (start end list)
  "Get elements in LIST from START to END.
START & END can be nil."
  (let (ele)
    (unless start
      (setq start 0))
    (unless end
      (setq end (length list)))
    (catch 'stop
      (dotimes (i (length list))
        (when (>= i start)
          (if (<= i end)
              (setq ele (append ele
                                (list (nth i list))))
            (throw 'stop t)))))
    ele))

(defun sniem--index (ele list)
  "Get the index of ELE in LIST."
  (catch 'result
    (dotimes (i (length list))
      (when (equal (nth i list) ele)
        (throw 'result i)))))

(provide 'sniem-common)

;;; sniem-common.el ends here
