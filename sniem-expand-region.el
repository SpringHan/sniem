;;; sniem-expand-region.el --- Simple united editing method -*- lexical-binding: t -*-

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

(require 'sniem-var)

(defvar sniem-expand-region-message "[s] for symbol, [w] for word:"
  "The message hint for `sniem-expand-region'.")

(defun sniem-expand-region-string (type)
  "Expand string region with it's TYPE."
  (interactive (list (read-char sniem-expand-region-message)))
  (let* ((thing (pcase type
                  (115 'symbol)
                  (119 'word)
                  (_ (user-error "[Sniem]: The %s type is error!" type))))
         (points (bounds-of-thing-at-point thing)))
    (when points
      (goto-char (car points))
      (push-mark (cdr points) t t))))

(provide 'sniem-expand-region)

;;; sniem-expand-region.el ends here
