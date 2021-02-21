;;; sniem-macro.el --- Simple united editing method -*- lexical-binding: t -*-

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

(defmacro sniem-define-motion (name arg docstring &rest body)
  "Define motion for sniem.
Argument NAME is the name of motion.
Argument ARG is the arg of motion.
Argument DOCSTRING is the docstring.
Optional argument BODY is the main body of motion function."
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name lambda-lsit
                           [&optional stringp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let ((inter (if (eq (car-safe (car-safe `,body)) 'interactive)
                   (pop `,body)
                 '(interactive))))
    (unless (memq '&optional `,arg)
      (setq `,arg (append `,arg '(&optional))))
    `(defun ,name (,@arg non-point-set)
       ,docstring
       ,inter
       (unless (or sniem-last-point-locked non-point-set)
         (setq-local sniem-last-point (point)))
       ,@body)))

(provide 'sniem-macro)

;;; sniem-macro.el ends here
