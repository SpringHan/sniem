;;; sniem-macro.el --- Simple united edition method -*- lexical-binding: t -*-

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

(defmacro sniem-define-motion (name arg docstring &rest body)
  "Define motion for sniem."
  (declare (indent defun)
           (doc-string 3))
  (let ((inter (if (eq (car-safe (car-safe `,body)) 'interactive)
                   (pop `,body)
                 '(interactive))))
    (unless (memq '&optional `,arg)
      (setq `,arg (append `,arg '(&optional))))
    `(defun ,name (,@arg non-point-set)
       ,docstring
       ,inter
       (unless non-point-set
         (setq-local sniem-last-point (1- (point))))
       ,@body)))

(provide 'sniem-macro)

;;; sniem-macro.el ends here
