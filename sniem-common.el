;;; sniem-common.el --- Simple united editing method -*- lexical-binding: t -*-

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

;; Simple united editing method

;;; Code:

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
