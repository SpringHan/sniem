;;; sniem-object-catch.el --- Simple united editing method -*- lexical-binding: t -*-

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

(require 'sniem-var)
(require 'sniem-common)
(require 'sniem-macro)

(defgroup sniem-object-catch nil
  "The group of `sniem-object-catch'."
  :group 'sniem)

(defcustom sniem-object-catch-global-symbol-alist
  '(("\"" . "\"")
    ("'" . "'")
    ("[" . "]")
    ("<" . ">")
    ("(" . ")")
    ("{" . "}"))
  "The global symbol alist."
  :type 'list
  :group 'sniem-object-catch)

(defcustom sniem-object-catch-last-points nil
  "The last point cons."
  :type 'cons
  :group 'sniem-object-catch)

(defcustom sniem-object-catch-action nil
  "The action info for the last catch."
  :type 'cons
  :group 'sniem-object-catch)

(defcustom sniem-object-catch-forward-p nil
  "The direction for catch."
  :type 'symbol
  :group 'sniem-object-catch)

(defcustom sniem-object-catch-prefix-string-p nil
  "If the prefix is string."
  :type 'boolean
  :group 'sniem-object-catch)

(sniem-define-motion sniem-object-catch (&optional char parent)
  "Catch region."
  (interactive)
  (let ((point (point)))
    (while (not (eq 'no (ignore-errors (sniem-object-catch--get char parent))))
      (if (bobp)
          (progn
            (goto-char point)
            (keyboard-quit))
        (backward-char)))
    (when sniem-object-catch-prefix-string-p
      (setq-local sniem-object-catch-prefix-string-p nil))))

(defun sniem-object-catch--get (char parent)
  "Get the object.
Argument CHAR is the prefix of pair.
Argument PARENT means get the parent pair of the content selected."
  (let ((move (if sniem-object-catch-forward-p
                  'forward-char
                'backward-char))
        prefix-point second-char second-point tmp go-on)
    (save-mark-and-excursion
      (when (region-active-p)
        (funcall move))
      ;; Get the `prefix-point'
      (if char
          (setq prefix-point
                (catch 'point-stop
                  (while t
                    (if (and (string=
                              char
                              (setq tmp
                                    (buffer-substring-no-properties (point) (1+ (point)))))
                             (sniem-object-catch-prefix-normal-p))
                        (throw 'point-stop (point))
                      (if (or (bobp) (eobp))
                          (throw 'point-stop nil)
                        (funcall move))))))
        (setq prefix-point
              (catch 'point-stop
                (while t
                  (if (and (sniem-object-catch--get-second-char
                            (setq tmp (buffer-substring-no-properties (point) (1+ (point)))))
                           (sniem-object-catch-prefix-normal-p))
                      (progn
                        (setq char tmp)
                        (throw 'point-stop (point)))
                    (if (or (bobp) (eobp))
                        (throw 'point-stop nil)
                      (funcall move)))))))
      (cond ((and (nth 3 (syntax-ppss prefix-point))
                  (/= (following-char) 34))
             (setq-local sniem-object-catch-prefix-string-p t))
            ((and sniem-object-catch-prefix-string-p
                  (null (nth 3 (syntax-ppss prefix-point))))
             (setq-local sniem-object-catch-prefix-string-p nil)))
      (if (not char)
          (message "[Sniem-Object-Catch]: Can not find a symbol in alist.")
        (setq second-char (sniem-object-catch--get-second-char char))
        (if (and (not (string= char second-char))
                 (ignore-errors
                   (= (char-before) 92)))
            (setq go-on t)
          (setq second-point (if (string= char second-char)
                                 (if (or (and (not (nth 3 (syntax-ppss)))
                                              (nth 8 (syntax-ppss)))
                                         (sniem-object-catch--face-around-eq))
                                     (sniem-object-catch-format-pointc char)
                                   (sniem-object-catch-format-point2 char prefix-point))
                               (sniem-object-catch-format-point char second-char))))
        (when (consp second-point)
          (setq prefix-point (car second-point)
                second-point (cdr second-point)))
        (if (and parent sniem-object-catch-last-points
                 (> (cdr sniem-object-catch-last-points) second-point)
                 (< prefix-point (car sniem-object-catch-last-points)))
            (setq go-on t)
          (setq-local sniem-object-catch-last-points (cons prefix-point second-point)))))
    (goto-char prefix-point)
    (push-mark second-point t t)
    (setq-local sniem-object-catch-action `(,char . ,parent))
    (unless go-on
      (setq go-on 'no))
    go-on))

(defun sniem-object-catch-by-char (char)
  "Catch region by CHAR."
  (interactive (list (char-to-string (read-char))))
  (if (sniem-object-catch--get-second-char char)
      (sniem-object-catch char)
    (message "[Sniem-Object-Catch]: %s is not defined in the symbol alist." char)))

(defun sniem-object-catch-char ()
  "Catch region by the last char."
  (interactive)
  (let ((pair (sniem-object-catch--get-last-char)))
    (sniem-object-catch pair nil)))

(defun sniem-object-catch-parent ()
  "Catch region for its parent."
  (interactive)
  (let ((pair (sniem-object-catch--get-last-char)))
    (sniem-object-catch pair t)))

(defun sniem-object-catch--get-last-char ()
  "Get the last char."
  (pcase last-input-event
    ((or 41 79 111) "(")
    ((or 83 93 115) "[")
    ((or 67 125 99) "{")
    ((or 39 113 81) "'")
    ((or 34 100 68) "\"")
    ((or 60 97 65) "<")
    (_ nil)))

(defun sniem-object-catch-parent-by-char (char)
  "Catch region for its parent by CHAR."
  (interactive (list (char-to-string (read-char))))
  (if (sniem-object-catch--get-second-char char)
      (sniem-object-catch char t)
    (message "[Sniem-Object-Catch]: %s is not defined in the symbol alist." char)))

(defun sniem-object-catch-repeat ()
  "Repeat the last catch."
  (interactive)
  (when sniem-object-catch-action
    (sniem-object-catch (car sniem-object-catch-action) (cdr sniem-object-catch-action))))

(defun sniem-object-catch-direction-reverse (&optional forward)
  "Reverse the catch direction.
Optional argument FORWARD means change the direction to forward."
  (interactive)
  (setq-local sniem-object-catch-forward-p
              (if (or forward (null sniem-object-catch-forward-p))
                  t
                nil))
  (message "[Sniem]: The object-catch-direction now is %s."
           (if sniem-object-catch-forward-p
               "forward"
             "backward")))

(defun sniem-object-catch-format-point (prefix second-char)
  "Format point with the PREFIX.
Argument SECOND-CHAR is the end char of the pair."
  (let ((times 1)
        tmp)
    (forward-char)
    (while (/= times 0)
      (setq tmp (buffer-substring-no-properties (point) (1+ (point))))
      (cond ((and (string= tmp prefix) (not (string= prefix second-char))
                  (or (and sniem-object-catch-prefix-string-p
                           (nth 3 (syntax-ppss)))
                      (and (null sniem-object-catch-prefix-string-p)
                           (null (nth 3 (syntax-ppss)))))
                  (not (= (char-before) 92)))
             (setq times (1+ times)))
            ((and (string= tmp second-char) (> times 0)
                  (not (= (char-before) 92))
                  (or (and sniem-object-catch-prefix-string-p
                           (nth 3 (syntax-ppss)))
                      (and (null sniem-object-catch-prefix-string-p)
                           (null (nth 3 (syntax-ppss))))))
             (setq times (1- times))))
      (forward-char))
    (point)))

(defun sniem-object-catch-format-point2 (pair prefix-point)
  "Format point for the PAIR with same char.
Argument PREFIX-POINT is the prefix point."
  (let ((region-forward-p (when (and (region-active-p) sniem-object-catch-forward-p)
                            (prog1 (cons (region-beginning) (region-end))
                              (deactivate-mark))))
        (face-eq-p (lambda (face1)
                     (let ((face2 (get-text-property (point) 'face)))
                       (ignore-errors
                         (or (eq face1 face2)
                             (memq face1 face2)
                             (memq face2 face1))))))
        prefix-face second-point)
    (save-mark-and-excursion
      (goto-char prefix-point)
      (setq prefix-face (face-at-point))
      (cond ((progn
               (backward-char)
               (funcall face-eq-p prefix-face))
             (setq second-point (sniem-object-catch-format-point1 pair prefix-point)
                   prefix-point (sniem-object-catch-format-point1 pair prefix-point t t)))

            ((progn
               (forward-char 2)
               (funcall face-eq-p prefix-face)) ; NOTE: This expression in here maybe have bug.
             (setq prefix-point (sniem-object-catch-format-point1 pair prefix-point nil t)
                   second-point (sniem-object-catch-format-point1 pair (point) t))))
      (when region-forward-p
        (goto-char (car region-forward-p))
        (push-mark (cdr region-forward-p)))
      (cons prefix-point (1+ second-point)))))

(defun sniem-object-catch-format-point1 (pair point &optional search prefix)
  "Format the POINT for char.
Argument PAIR is the pair."
  (save-mark-and-excursion
    (goto-char point)
    (let ((search-command (if prefix
                              'search-backward
                            'search-forward)))
      (when search
        (setq point (progn
                      (funcall search-command pair)
                      (unless prefix (backward-char))
                      (point))))
      (when (sniem-object-catch-backslash-p)
        (setq point (progn
                      (forward-char)
                      (point)))
        (while (progn
                 (setq point (funcall search-command pair))
                 (sniem-object-catch-backslash-p)))))
    point))

(defun sniem-object-catch-format-pointc (char)
  "Format the CHAR has same char in comment."
  (let (balone falone)
    (setq balone (sniem-object-catch--while-check-format char))
    (setq falone (sniem-object-catch--while-check-format char t))
    (if balone
        (cons balone (1+ (point)))
      (1+ falone))))

(defun sniem-object-catch--while-check-format (char &optional forward)
  "Check the pair which has same CHAR in a while with the direction.
When the FORWARD is non-nil, the direction is forward.
Otherwise it's backward."
  (let ((command (if forward
                     'forward-char
                   'backward-char))
        current-char alone another-point)
    (save-mark-and-excursion
      (while (and (not (sniem-object-catch--border forward))
                  (or (funcall command) t)
                  (or (nth 8 (syntax-ppss))
                      (ignore-errors
                        (= (char-before) 10))
                      (sniem-object-catch--face-around-eq)))
        (when (and (not (sniem-object-catch-backslash-p))
                   (ignore-errors
                     (setq current-char
                           (buffer-substring-no-properties (point) (1+ (point)))))
                   (string= char current-char))
          (cond ((null another-point)
                 (setq another-point (point)))
                (alone (setq alone nil))
                (t (setq alone t))))))
    (when (not alone)
      another-point)))

(defun sniem-object-catch--border (forward)
  "Check if it's border now.
FORWARD means now it's forward direction."
  (if forward
      (eobp)
    (bobp)))

(defun sniem-object-catch--face-around-eq ()
  "Check if the faces around the point are equal."
  (let ((face (face-at-point))
        lface rface)
    (save-mark-and-excursion
      (setq lface (progn
                    (ignore-errors (backward-char))
                    (face-at-point))
            rface (progn
                    (ignore-errors (forward-char))
                    (face-at-point))))
    (and (eq face lface)
         (eq face rface))))

(defun sniem-object-catch--symbol-exists-p (symbol)
  "Check if the SYMBOL is exists."
  (catch 'exists
    (let ((index 0))
      (dolist (symbol-cons sniem-object-catch-global-symbol-alist)
        (when (string= symbol (car symbol-cons))
          (throw 'exists index))
        (setq index (1+ index))))))

(defun sniem-object-catch-prefix-normal-p ()
  "Check if the current major mode belongs to Lisp mode.
The current char is not quote and the char before prefix is not backslash."
  (not (or (and (= 39 (following-char)) (sniem-object-catch-lisp-mode-p))
           (sniem-object-catch-backslash-p))))

(defun sniem-object-catch-backslash-p ()
  "Check if the char before current point is \\."
  (unless (bobp)
    (and (= 92 (char-before))
         (not (save-mark-and-excursion
                (backward-char)
                (if (bobp)
                    t
                  (= 92 (char-before))))))))

(defmacro sniem-object-catch-mode-defalist (modename &rest alist)
  "Define ALIST for major mode.
Argument MODENAME if the mode name."
  (declare (indent 1))
  `(let ((sym-alist sniem-object-catch-global-symbol-alist)
         tmp)
     (dolist (list ',alist)
       (if (setq tmp (sniem-object-catch--symbol-exists-p (car list)))
           (setf (cdr (nth tmp sym-alist)) (cdr list))
         (add-to-list 'sym-alist list)))
     (add-hook (intern (concat (symbol-name ',modename) "-hook"))
               `(lambda () (setq-local sniem-object-catch-global-symbol-alist
                                       ',sym-alist)))))

(add-hook 'deactivate-mark-hook #'(lambda ()
                                    (when sniem-object-catch-last-points
                                      (setq-local sniem-object-catch-last-points nil))
                                    (when sniem-object-catch-prefix-string-p
                                      (setq-local sniem-object-catch-prefix-string-p nil))))

(provide 'sniem-object-catch)

;;; sniem-object-catch.el ends here
