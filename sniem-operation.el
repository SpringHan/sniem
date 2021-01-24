;;; sniem-operation.el --- Simple united edition method -*- lexical-binding: t -*-

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

;; Simple united edition method

;;; Code:

(defun sniem-insert ()
  "Insert at the current point or the beginning of mark region."
  (interactive)
  (unless (eq (sniem-current-mode) 'insert)
    (when (region-active-p)
      (goto-char (region-beginning)))
    (sniem-change-mode 'insert)))

(defun sniem-insert-line ()
  "Insert at the beginning of line."
  (interactive)
  (back-to-indentation)
  (sniem-insert))

(defun sniem-append ()
  "Append at the next point or the end of mark region."
  (interactive)
  (when (region-active-p)
    (goto-char (region-beginning)))
  (forward-char)
  (sniem-insert))

(defun sniem-append-line ()
  "Append at the end of line."
  (interactive)
  (end-of-line)
  (sniem-insert))

(defun sniem--open-line (&optional above)
  "Open new line for other function."
  (when above
    (sniem-prev-line nil t))
  (sniem-end-of-line t)
  (insert "\n"))

(defun sniem-open-line ()
  "Open new line."
  (interactive)
  (sniem--open-line)
  (indent-according-to-mode)
  (sniem-insert))

(defun sniem-open-line-previous ()
  "Open new line."
  (interactive)
  (sniem--open-line t)
  (indent-according-to-mode)
  (sniem-insert))

(defun sniem-center (action)
  "Center action for sniem."
  (interactive (list (read-char sniem-center-message)))
  (pcase action
    (122 (recenter nil t))
    (116 (recenter-top-bottom 0))
    (98 (recenter-top-bottom -1))))

(defun sniem-mark (type)
  "Mark the object with action type."
  (interactive (list (read-char sniem-mark-message)))
  (pcase type
    (108
     (beginning-of-line)
     (push-mark (point) t t)
     (forward-line))
    (112
     (push-mark sniem-last-point t t))
    (109 (push-mark (point) t t))))

(defun sniem-up/down-case ()
  "Up or down case."
  (interactive)
  (let ((char (following-char)))
    (if (eq (upcase char) char)
        (setq char (downcase char))
      (setq char (upcase char)))
    (delete-forward-char 1)
    (insert-char char)))

(defun sniem-replace-char (char)
  "Replace the char under cursor."
  (interactive "c")
  (delete-forward-char 1)
  (insert-char char)
  (sniem-backward-char nil t))

(defun sniem-replace-word ()
  "Replace the word under cursor."
  (interactive)
  (let* ((word (thing-at-point 'word t))
         (replaced (read-string "Enter the new word: " word))
         (word-points (bounds-of-thing-at-point 'word)))
    (delete-region (car word-points) (cdr word-points))
    (insert replaced)))

(defun sniem-delete-char ()
  "Delete the char under cursor."
  (interactive)
  (kill-ring-save (point) (1+ (point)))
  (delete-forward-char 1))

(defun sniem-delete (action)
  "Delete action."
  (interactive (list (if (region-active-p)
                         t
                       (read-char sniem-delete-message))))
  (pcase action
    ((pred symbolp) (sniem-delete-region (region-beginning) (region-end)))
    (100 (sniem-delete-region (line-beginning-position) (1+ (line-end-position))))
    (68 (sniem-delete-region (line-beginning-position) (line-end-position)))
    (112 (sniem-delete-region sniem-last-point (point)))))

(defun sniem-delete-in-region ()
  "Delete in region."
  (interactive)
  (when (region-active-p)
    (when (= (point) (region-beginning))
      (sniem-end-of-mark))
    (push-mark (1+ (region-beginning)) t t)
    (goto-char (1- (region-end)))
    (sniem-delete t)))

(defun sniem-delete-region (start end)
  "Like `delete-region', but it will eval `kill-ring-save' to copy the region."
  (kill-ring-save start end)
  (delete-region start end))

(defun sniem-change (action)
  "Change contents."
  (interactive (list (if (region-active-p)
                         t
                       (read-char sniem-change-message))))
  (pcase action
    ((pred symbolp) (sniem-delete t))
    (99 (sniem-delete 68) (indent-according-to-mode))
    (112 (sniem-delete 112)))
  (sniem-insert))

(defun sniem-change-in-region ()
  "Change in region."
  (interactive)
  (when (region-active-p)
    (when (= (point) (region-beginning))
      (sniem-end-of-mark))
    (push-mark (1+ (region-beginning)) t t)
    (goto-char (1- (region-end)))
    (sniem-change t)))

(defun sniem-yank (action)
  "Yank action."
  (interactive (list (if (region-active-p)
                         t
                       (read-char sniem-yank-message))))
  (pcase action
    ((pred symbolp) (kill-ring-save (region-beginning) (region-end)))
    (121 (kill-ring-save (line-beginning-position) (line-end-position)))
    (112 (kill-ring-save sniem-last-point (point)))))

(defun sniem-join ()
  "Change LINE to one line."
  (interactive)
  (let ((lines (if (region-active-p)
                   (count-lines (region-beginning) (region-end))
                 (count-lines sniem-last-point (point))))
        (beg (if (region-active-p)
                 (region-beginning)
               (if (> sniem-last-point (point))
                   (point)
                 sniem-last-point))))
    (goto-char beg)
    (dotimes (_ lines)
      (join-line 1))))

(defun sniem-macro (action)
  "Macro action."
  (interactive (list (unless defining-kbd-macro
                       (read-char sniem-macro-message))))
  (if defining-kbd-macro
      (end-kbd-macro)
    (pcase action
      (113 (start-kbd-macro nil))
      (101 (call-last-kbd-macro))
      (110 (call-interactively (name-last-kbd-macro))))))

;;; Motions

(sniem-define-motion sniem-beginning-of-line ()
  "Beginning of line."
  (beginning-of-line))

(sniem-define-motion sniem-end-of-line ()
  "End of line."
  (end-of-line))

(sniem-define-motion sniem-forward-char (&optional times)
  "Forward char."
  (interactive "P")
  (setq times (or times 1))
  (catch 'end
    (while (/= times 0)
      (if (eolp)
          (throw 'end t)
        (forward-char)
        (setq times (1- times))))))

(sniem-define-motion sniem-5-forward-char ()
  "Eval `sniem-forward-char' 5 times."
  (sniem-forward-char 5 t))

(sniem-define-motion sniem-backward-char (&optional times)
  "Backward char."
  (interactive "P")
  (setq times (or times 1))
  (catch 'beg
    (while (/= times 0)
      (if (bolp)
          (throw 'beg t)
        (backward-char)
        (setq times (1- times))))))

(sniem-define-motion sniem-5-backward-char ()
  "Eval `sniem-backward-char' 5 times."
  (sniem-backward-char 5 t))

(sniem-define-motion sniem-prev-line (&optional times)
  "Previous line."
  (interactive "P")
  (setq times (or times 1))
  (unless (bobp)
    (previous-line times)))

(sniem-define-motion sniem-5-prev-line ()
  "Eval `sniem-prev-line' 5 times."
  (sniem-prev-line 5 t))

(sniem-define-motion sniem-next-line (&optional times)
  "Next line."
  (interactive "P")
  (setq times (or times 1))
  (unless (eobp)
    (next-line times)))

(sniem-define-motion sniem-5-next-line ()
  "Eval `sniem-next-line' 5 times."
  (sniem-next-line 5 t))

(sniem-define-motion sniem-first-line ()
  "Goto beginning of buffer."
  (goto-char (point-min)))

(sniem-define-motion sniem-goto-line (&optional n)
  "Goto line with N."
  (interactive "P")
  (if (null n)
      (with-no-warnings (end-of-buffer))
    (goto-char (point-min))
    (forward-line n)))

(sniem-define-motion sniem-scroll-up-command (&optional n)
  "Scroll up."
  (interactive "P")
  (scroll-up-command n))

(sniem-define-motion sniem-scroll-down-command (&optional n)
  "Scroll down."
  (interactive "P")
  (scroll-down-command n))

(sniem-define-motion sniem-find-forward (char)
  "Find CHAR forward."
  (interactive "c")
  (sniem-find char 'forward))

(sniem-define-motion sniem-find-backward (char)
  (interactive "c")
  (sniem-find char 'backward))

(defun sniem-find (char direct)
  "Find char."
  (let ((way (pcase direct
               ('forward 'sniem-forward-char)
               ('backward 'sniem-backward-char)
               (_ (user-error "[Sniem]: The direction for finding is error!")))))
    (funcall way nil t)
    (while (not (or (eolp) (bolp) (eq (following-char) char)))
      (funcall way nil t))))

(sniem-define-motion sniem-next-word ()
  "Move to next word."
  (sniem-word 'forward))

(sniem-define-motion sniem-prev-word ()
  "Move to previous word."
  (sniem-word 'backward))

(defun sniem-word (direct)
  "Move to word with DIRECT."
  (let ((way (pcase direct
               ('forward 'sniem-forward-char)
               ('backward 'sniem-backward-char)))
        (limit (pcase direct
                 ('forward 'eolp)
                 ('backward 'bolp)))
        (current-word (thing-at-point 'word t))
        (current-word-points (bounds-of-thing-at-point 'word)))
    (if (null current-word)
        (while (and (not (funcall limit))
                    (null current-word))
          (funcall way nil t)
          (setq current-word (thing-at-point 'word t)
                current-word-points (bounds-of-thing-at-point 'word)))
      (goto-char (pcase direct
                   ('forward (cdr current-word-points))
                   ('backward (car current-word-points))))
      (while (not (or (eolp)
                      (bolp)
                      (and (not (null (thing-at-point 'word)))
                           (not (string= current-word (thing-at-point 'word))) )))
        (funcall way nil t))
      (goto-char (car (bounds-of-thing-at-point 'word))))))

(sniem-define-motion sniem-beg-of-mark ()
  "Goto the beginning of mark."
  (when (region-active-p)
    (let ((end-point (region-end)))
      (goto-char (region-beginning))
      (push-mark end-point t t))))

(sniem-define-motion sniem-end-of-mark ()
  "Goto the end of mark."
  (when (region-active-p)
    (let ((beg-point (region-beginning)))
      (goto-char (region-end))
      (push-mark beg-point t t))))

(sniem-define-motion sniem-beg-of-word ()
  "Goto the beginning of word."
  (let ((word-points (bounds-of-thing-at-point 'word)))
    (when word-points
      (goto-char (car word-points)))))

(sniem-define-motion sniem-end-of-word ()
  "Goto the end of word."
  (let ((word-points (bounds-of-thing-at-point 'word)))
    (when word-points
      (goto-char (cdr word-points)))))

(sniem-define-motion sniem-goto-prev ()
  "Goto prev lines with `sniem-digit-argument-get'."
  (sniem-prev-line (sniem-digit-argument-get "Move up: ") t))

(sniem-define-motion sniem-goto-next ()
  "Goto next lines with `sniem-digit-argument-get'."
  (sniem-next-line (sniem-digit-argument-get "Move down: ") t))

(defun sniem-goto-last-point (&optional no-set-point)
  "Goto `sniem-last-point'."
  (interactive)
  (let ((current-point (point)))
    (goto-char sniem-last-point)
    (unless (or sniem-last-point-locked no-set-point)
      (setq-local sniem-last-point current-point))))

(provide 'sniem-operation)

;;; sniem-operation.el ends here
