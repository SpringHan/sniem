;;; sniem-object-catch.el --- Simple united edition method -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Package-Requires: ((emacs "26.3") (evil "0"))
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

(defgroup sniem-object-catch nil
  "The group of `sniem-object-catch'."
  :group 'evil)

(defcustom sniem-object-catch-global-symbol-alist
  '(("\"" . "\"")
    ("'" . "'")
    ("[" . "]")
    ("<" . ">")
    ("(" . ")")
    ("{" . "}"))
  "The global symbol alist."
  :group 'sniem-object-catch)

(defcustom sniem-object-catch-key "RET"
  "The key for catch."
  :type 'string
  :group 'sniem-object-catch)

(defcustom sniem-object-catch-parent-key "M-RET"
  "The key for catch."
  :type 'string
  :group 'sniem-object-catch)

(defcustom sniem-object-catch-char-key "C-<return>"
  "The key for catch."
  :type 'string
  :group 'sniem-object-catch)

(defcustom sniem-object-catch-last-points nil
  "The last point cons."
  :type 'cons
  :group 'sniem-object-catch)

(defvar global-sniem-object-catch-status nil
  "The status for `global-sniem-object-catch-mode'")

(sniem-define-motion sniem-object-catch (&optional char parent)
  "Catch region."
  (interactive)
  (when
      (catch 'stop
        (let (prefix-point second-char second-point tmp)
          (save-mark-and-excursion
            (when (region-active-p)
              (backward-char))
            ;; Get the `prefix-point'
            (if char
                (setq prefix-point
                      (catch 'point-stop
                        (while t
                          (if (string=
                               char
                               (setq tmp
                                     (buffer-substring-no-properties (point) (1+ (point)))))
                              (throw 'point-stop (point))
                            (if (bobp)
                                (throw 'point-stop nil)
                              (backward-char))))))
              (setq prefix-point
                    (catch 'point-stop
                      (while t
                        (if (sniem-object-catch--get-second-char
                             (setq tmp (buffer-substring-no-properties (point) (1+ (point)))))
                            (progn
                              (setq char tmp)
                              (throw 'point-stop (point)))
                          (if (bobp)
                              (throw 'point-stop nil)
                            (backward-char)))))))

            (if (not char)
                (message "[Sniem-Object-Catch]: Can not find a symbol in alist.")
              (setq second-char (sniem-object-catch--get-second-char char)
                    second-point (sniem-object-catch-format-point char second-char))
              (if (and parent (> (cdr sniem-object-catch-last-points) second-point))
                  (throw 'stop t)
                (setq-local sniem-object-catch-last-points (cons prefix-point second-point)))))
          (goto-char prefix-point)
          (push-mark second-point t t)))
    (backward-char)
    (sniem-object-catch char t)))

(defun sniem-object-catch-by-char (char)
  "Catch region by CHAR."
  (interactive (list (char-to-string (read-char))))
  (if (sniem-object-catch--get-second-char char)
      (sniem-object-catch char)
    (message "[Sniem-Object-Catch]: %s is not defined in the symbol alist." char)))

(defun sniem-object-catch-parent ()
  "Catch region for its parent."
  (interactive)
  (sniem-object-catch nil t))

(defun sniem-object-catch-format-point (prefix second-char)
  "Format point with the PREFIX."
  (let ((times 1)
        tmp)
    (forward-char)
    (while (/= times 0)
      (setq tmp (buffer-substring-no-properties (point) (1+ (point))))
      (cond ((and (string= tmp prefix) (not (string= prefix second-char)))
             (setq times (1+ times)))
            ((and (string= tmp second-char) (> times 0))
             (setq times (1- times)))
            ((and (string= tmp second-char) (= times -1))
             (setq times 0)))
      (forward-char))
    (point)))

(defun sniem-object-catch--get-second-char (prefix)
  "Get the second char by the PREFIX."
  (catch 'second-char
    (dolist (char-cons sniem-object-catch-global-symbol-alist)
      (when (string= prefix (car char-cons))
        (throw 'second-char (cdr char-cons))))))

(defun sniem-object-catch--symbol-exists-p (symbol)
  "Check if the SYMBOL is exists."
  (catch 'exists
    (let ((index 0))
      (dolist (symbol-cons sniem-object-catch-global-symbol-alist)
        (when (string= symbol (car symbol-cons))
          (throw 'exists index))
        (setq index (1+ index))))))

(defmacro sniem-object-catch-mode-defalist (mode-name &rest alist)
  "Define alist for major mode."
  (declare (indent 1))
  `(let ((sym-alist sniem-object-catch-global-symbol-alist)
         tmp)
     (dolist (list ,alist)
       (if (setq tmp (sniem-object-catch--symbol-exists-p (car list)))
           (setf (cdr (nth tmp sym-alist)) (cdr list))
         (add-to-list 'sym-alist list)))
     (add-hook (intern (concat (symbol-name ,mode-name) "-hook"))
               `(lambda () (setq-local sniem-object-catch-global-symbol-alist
                                       ,sym-alist)))))

;;; Init
(add-hook 'emacs-lisp-mode-hook
          #'(lambda () (setq-local sniem-object-catch-global-symbol-alist
                                   (delete '("'" . "'")
                                           sniem-object-catch-global-symbol-alist))))

(provide 'sniem-object-catch)

;;; sniem-object-catch.el ends here
