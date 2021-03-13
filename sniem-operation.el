;;; sniem-operation.el --- Simple united editing method -*- lexical-binding: t -*-

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
(require 'sniem-macro)
(require 'sniem-common)

(defun sniem-insert ()
  "Insert at the current point or the beginning of mark region."
  (interactive)
  (unless (eq (sniem-current-mode) 'insert)
    (when (region-active-p)
      (goto-char (region-beginning))
      (deactivate-mark))
    (sniem-change-mode 'insert)))

(defun sniem-insert-line ()
  "Insert at the beginning of line."
  (interactive)
  (if (region-active-p)
      (goto-char (1+ (region-beginning)))
    (back-to-indentation))
  (sniem-insert))

(defun sniem-append ()
  "Append at the next point or the end of mark region."
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (deactivate-mark))
    (forward-char))
  (sniem-insert))

(defun sniem-append-line ()
  "Append at the end of line."
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (1- (region-end)))
        (deactivate-mark))
    (end-of-line))
  (sniem-insert))

(defun sniem--open-line (&optional above)
  "Open new line for other function.
Optional argument ABOVE is t, it will open line above."
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
  (if (bobp)
      (progn
        (beginning-of-line)
        (insert "\n")
        (goto-char (point-min)))
    (sniem--open-line t))
  (indent-according-to-mode)
  (sniem-insert))

(defun sniem-center (action)
  "Center ACTION for sniem."
  (interactive (list (read-char sniem-center-message)))
  (pcase action
    (122 (recenter nil t))
    (116 (recenter-top-bottom 0))
    (98 (recenter-top-bottom -1))))

(defun sniem-mark (type)
  "Mark the object with action TYPE."
  (interactive (list (read-char sniem-mark-message)))
  (pcase type
    (108
     (beginning-of-line)
     (push-mark (point) t t)
     (end-of-line)
     (setq-local sniem-mark-line t))
    (112
     (push-mark sniem-last-point t t)
     (when sniem-last-point-locked
       (sniem-lock-unlock-last-point)))
    (109 (push-mark (point) t t))
    (102 (mark-defun))
    (98 (mark-whole-buffer))
    (t (sniem-expand-region-string type))))

;;; Hook for mark
(add-hook 'deactivate-mark-hook #'(lambda ()
                                    (when sniem-mark-line
                                      (setq-local sniem-mark-line nil))))

(defun sniem-up-down-case ()
  "Up or down case."
  (interactive)
  (if (region-active-p)
      (let ((contents (buffer-substring-no-properties (region-beginning)
                                                      (region-end))))
        (delete-region (region-beginning) (region-end))
        (dolist (char (string-to-list contents))
          (insert-char (if (eq (upcase char) char)
                           (downcase char)
                         (upcase char)))))
    (let ((char (following-char)))
      (delete-char 1)
      (insert-char (if (eq (upcase char) char)
                       (downcase char)
                     (upcase char))))))

(defun sniem-replace-char (char)
  "Replace the CHAR under cursor."
  (interactive "c")
  (delete-char 1)
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
  (if (eolp)
      (delete-char -1)
    (kill-ring-save (point) (1+ (point)))
    (delete-char 1)))

(defun sniem-delete (action)
  "Delete ACTION."
  (interactive (list (if sniem-delete-edit
                         (progn
                           (setq-local sniem-delete-edit nil)
                           112)
                       (if (region-active-p)
                           t
                         (read-char sniem-delete-message)))))
  (pcase action
    ((pred symbolp) (sniem-delete-region (region-beginning) (region-end)))
    (100 (if (= (line-beginning-position) (line-end-position))
             (progn
               (if (bobp)
                   (delete-char 1)
                 (delete-char -1))
               (forward-line))
           (sniem-delete-region (line-beginning-position)
                                (if (= (line-end-position) (point-max))
                                    (line-end-position)
                                  (1+ (line-end-position)))))
         (when (eobp)
           (beginning-of-line)))
    (68 (sniem-delete-region (line-beginning-position) (line-end-position)))
    (112 (sniem-delete-region sniem-last-point (point))
         (when sniem-last-point-locked
           (sniem-lock-unlock-last-point)))
    (101 (setq-local sniem-delete-edit t)
         (sniem-lock-unlock-last-point t))))

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
  "Like `delete-region', but it will eval `kill-ring-save' to copy the region.
Argument START is the start point of the region.
Argument END is the end point of the region."
  (kill-ring-save start end)
  (delete-region start end))

(defun sniem-change (action)
  "Change contents.
Argument ACTION is the action of change."
  (interactive (list (if sniem-change-edit
                         (progn
                           (setq-local sniem-change-edit nil)
                           112)
                       (if (region-active-p)
                           t
                         (read-char sniem-change-message)))))
  (pcase action
    ((pred symbolp) (sniem-delete t) (sniem-insert))
    (99 (sniem-delete 68) (indent-according-to-mode) (sniem-insert))
    (112 (sniem-delete 112) (sniem-insert))
    (101 (setq-local sniem-change-edit t)
         (sniem-lock-unlock-last-point t))))

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
  "Yank ACTION."
  (interactive (list (if (region-active-p)
                         t
                       (read-char sniem-yank-message))))
  (pcase action
    ((pred symbolp) (kill-ring-save (region-beginning) (region-end)))
    (121 (kill-ring-save (line-beginning-position)
                         (if (= (point-max) (line-end-position))
                             (line-end-position)
                           (1+ (line-end-position)))))
    (112 (kill-ring-save sniem-last-point (point))
         (when sniem-last-point-locked
           (sniem-lock-unlock-last-point)))))

(defun sniem-yank-in-region ()
  "Yank in region."
  (interactive)
  (when (region-active-p)
    (kill-ring-save (1+ (region-beginning)) (1- (region-end)))))

(defun sniem-paste (&optional n)
  "Paste the N content in `kill-ring'."
  (interactive "P")
  (let ((i 0)
        (regionp (when (region-active-p)
                   (cons (region-beginning) (region-end)))))
    (unless n
      (when
          (catch 'n
            (while (= 0
                      (string-to-number
                       (char-to-string
                        (setq n (read-char (format "%s:%d%s"
                                                   (sniem-paste--output-contents i)
                                                   (1+ (/ i 9))
                                                   (propertize "[n]: next page, [p]: prev page or 1, [1-9]: insert content, [q]: cancel"
                                                               'face 'font-lock-comment-face)))))))
              (pcase n
                (110 (setq i (+ i 9)))
                (112 (if (>= i 9)
                         (setq i (- i 9))
                       (throw 'n t)))
                (113 (keyboard-quit)))))
        (setq n 49)))
    (setq n (string-to-number (char-to-string n)))
    (when regionp
      (goto-char (cdr regionp))
      (push-mark (car regionp) t t)
      (sniem-delete t))
    (insert (nth (if regionp
                     (+ n i)
                   (1- (+ n i)))
                 kill-ring))))

(defun sniem-paste--output-contents (n)
  "Output contents for `sniem-paste'.
Argument N is the page of the contents."
  (let (content c tmp)
    (dotimes (i 9)
      (setq c (format "%d: %s"
                      (1+ i)
                      (nth (+ i n) kill-ring))
            content (concat content
                            (progn
                              (when (setq tmp (sniem-paste--include-ln-p c))
                                (setq c tmp))
                              (while (> (length c) (frame-width))
                                (setq c (concat (substring c 0 (1- (- (length c) (frame-width)))) "...")))
                              c)
                            "\n")))
    content))

(defun sniem-paste--include-ln-p (string)
  "Check if there has \n in STRING."
  (let ((string-list (string-to-list string))
        tmp)
    (when (memq 10 string-list)
      (setq tmp (delete 10 string-list)))
    (when tmp
      (eval `(string ,@tmp)))))

(defun sniem-paste-in-region ()
  "Paste the `kill-ring' content in region."
  (interactive)
  (when (region-active-p)
    (when (= (region-beginning) (point))
      (sniem-end-of-mark))
    (push-mark (1+ (region-beginning)) t t)
    (goto-char (1- (region-end)))
    (sniem-paste)))

(defun sniem-join ()
  "Change LINE to one line."
  (interactive)
  (let ((last-point (point)))
    (if (bolp)
        (if (save-mark-and-excursion
              (forward-line -1)
              (= (line-beginning-position) (line-end-position)))
            (forward-line -1)
          (backward-char))
      (backward-char)
      (unless (or (= 10 (following-char))
                  (= 32 (following-char)))
        (user-error "[Sniem]: The current position doesn't need join!")))
    (while (or (= 10 (following-char))
               (= 32 (following-char)))
      (backward-char))
    (forward-char)
    (push-mark last-point t t)))

(defun sniem-macro (action)
  "Macro ACTION."
  (interactive (list (unless defining-kbd-macro
                       (read-char sniem-macro-message))))
  (if defining-kbd-macro
      (progn
        (end-kbd-macro)
        ;; If the `sniem-kmacro-range' is exists, call the macro to the lines
        (when sniem-kmacro-range
          (let ((region-beg
                 (save-mark-and-excursion
                   (sniem-goto-line (car sniem-kmacro-range) t)
                   (line-beginning-position)))
                (region-end
                 (save-mark-and-excursion
                   (sniem-goto-line (cdr sniem-kmacro-range) t)
                   (when (= (line-beginning-position) (line-end-position))
                     (forward-line))
                   (line-end-position))))
            (apply-macro-to-region-lines region-beg region-end)
            (setq-local sniem-kmacro-range nil))))
    
    (when (region-active-p)
      (if (= action 113)
          (if (= (line-number-at-pos (region-beginning))
                 (line-number-at-pos (region-end)))
              (setq-local sniem-kmacro-mark-content
                          (buffer-substring-no-properties (region-beginning) (region-end)))
            (setq-local sniem-kmacro-range
                        (cons (1+ (line-number-at-pos (region-beginning)))
                              (line-number-at-pos (region-end))))
            (deactivate-mark)
            (goto-char (region-beginning)))
        (setq-local sniem-kmacro-mark-content
                    (buffer-substring-no-properties (region-beginning) (region-end)))))
    (pcase action
      (113 (call-interactively #'start-kbd-macro))
      (101 (call-last-kbd-macro))
      (110 (call-interactively #'name-last-kbd-macro)))))

(advice-add 'keyboard-quit :before
            (lambda ()
              (when sniem-kmacro-mark-content
                (setq-local sniem-kmacro-mark-content nil))))

(defun sniem-pair (prefix &optional add)
  "Modify the region's pair.
Argument PREFIX is the prefix of the pair."
  (interactive (list (let ((var (read-char)))
                       (if (= var 97)
                           (list (read-char) t)
                         var))))
  (when (cdr-safe prefix)
    (setq add t
          prefix (car prefix)))
  (let ((second (unless (= 32 prefix)
                  (sniem-object-catch--get-second-char (char-to-string prefix))))
        (prefix-point (region-beginning))
        (second-point (region-end))
        (prefix-char (buffer-substring-no-properties (region-beginning) (1+ (region-beginning)))))
    (if (and (null second)
             (/= prefix 32))
        (user-error "[Sniem]: The pair is not exists in `sniem-object-catch-global-symbol-alist'!")
      (save-mark-and-excursion
        (goto-char prefix-point)
        (when (and (null add)
                   (sniem-pair--pair-p prefix-char))
          (delete-char 1))
        (unless (= prefix 32)
          (insert prefix))
        (goto-char (if (= prefix 32)
                       (1- second-point)
                     second-point))
        (if (and (null add)
                 (sniem-pair--pair-p prefix-char))
            (delete-char -1)
          (forward-char))
        (unless (= prefix 32)
          (insert second))))))

(defun sniem-pair--pair-p (char-string)
  "Check if the CHAR belongs to pair.
Argument CHAR-STRING is the string to compair."
  (let ((alpha-list '("a" "A" "b" "B" "c" "C" "d" "D" "e" "E" "f" "F" "g" "G"
                      "h" "H" "i" "I" "j" "J" "k" "K" "l" "L" "m" "M" "n" "N"
                      "o" "O" "p" "P" "q" "Q" "r" "R" "s" "S" "t" "T" "u" "U"
                      "v" "V" "w" "W" "x" "X" "y" "Y" "z" "Z" "0" "1" "2" "3"
                      "4" "5" "6" "7" "8" "9")))
    ;; Write like this because `memq' and others can not work well.
    (not (sniem--mems char-string alpha-list))))

;;; Motions

(sniem-define-motion sniem-beginning-of-line ()
  "Beginning of line."
  (beginning-of-line))

(sniem-define-motion sniem-end-of-line ()
  "End of line."
  (end-of-line))

(sniem-define-motion sniem-forward-char (&optional n)
  "Forward char."
  (interactive "P")
  (setq n (or n 1))
  (catch 'end
    (while (/= n 0)
      (if (eolp)
          (throw 'end t)
        (forward-char)
        (setq n (1- n))))))

(sniem-define-motion sniem-5-forward-char ()
  "Eval `sniem-forward-char' 5 times."
  (sniem-forward-char 5 t))

(sniem-define-motion sniem-backward-char (&optional n)
  "Backward char."
  (interactive "P")
  (setq n (or n 1))
  (catch 'beg
    (while (/= n 0)
      (if (bolp)
          (throw 'beg t)
        (backward-char)
        (setq n (1- n))))))

(sniem-define-motion sniem-5-backward-char ()
  "Eval `sniem-backward-char' 5 times."
  (sniem-backward-char 5 t))

(sniem-define-motion sniem-prev-line (&optional n)
  "Previous line."
  (interactive "P")
  (setq n (or n 1))
  (unless (bobp)
    (line-move (- 0 n)))
  (when (and (region-active-p) sniem-mark-line)
    (if (= (region-beginning) (point))
        (beginning-of-line)
      (end-of-line))))

(sniem-define-motion sniem-5-prev-line ()
  "Eval `sniem-prev-line' 5 times."
  (sniem-prev-line 5 t))

(sniem-define-motion sniem-next-line (&optional n)
  "Next line."
  (interactive "P")
  (setq n (or n 1))
  (unless (eobp)
    (line-move n))
  (when (and (region-active-p) sniem-mark-line)
    (if (= (region-beginning) (point))
        (beginning-of-line)
      (end-of-line))))

(sniem-define-motion sniem-5-next-line ()
  "Eval `sniem-next-line' 5 times."
  (sniem-next-line 5 t))

(sniem-define-motion sniem-first-line ()
  "Goto beginning of buffer."
  (goto-char (point-min))
  (when (and (region-active-p) sniem-mark-line)
    (end-of-line)))

(sniem-define-motion sniem-goto-line (&optional n)
  "Goto line with N."
  (interactive "P")
  (if (null n)
      (with-no-warnings (end-of-buffer))
    (goto-char (point-min))
    (forward-line (1- n)))
  (when (and (region-active-p) sniem-mark-line)
    (end-of-line)))

(sniem-define-motion sniem-scroll-up-command (&optional n)
  "Scroll up."
  (interactive "P")
  (scroll-up-command n)
  (when (and (region-active-p) sniem-mark-line)
    (end-of-line)))

(sniem-define-motion sniem-scroll-down-command (&optional n)
  "Scroll down."
  (interactive "P")
  (scroll-down-command n)
  (when (and (region-active-p) sniem-mark-line)
    (end-of-line)))

(sniem-define-motion sniem-find-forward (&optional n c no-hint)
  "Find CHAR forward."
  (interactive "P")
  (let ((char (if c
                  c
                (read-char))))
    (if n
        (dotimes (_ n)
          (sniem-find char 'forward))
      (sniem-find char 'forward))
    (when (region-active-p)
      (sniem-forward-char nil t))
    (unless no-hint
      (sniem-motion-hint `(lambda () (interactive)
                            (sniem-find-forward nil ,char t t))))))

(sniem-define-motion sniem-find-backward (&optional n c no-hint)
  "Find CHAR backward."
  (interactive "P")
  (let ((char (if c
                  c
                (read-char))))
    (if n
        (dotimes (_ n)
          (sniem-find char 'backward))
      (sniem-find char 'backward))
    (unless no-hint
      (sniem-motion-hint `(lambda () (interactive)
                            (sniem-find-backward nil ,char t t))))))

(defun sniem-find (char direct)
  "Find CHAR.
Argument DIRECT is the direction for find."
  (let ((current-point (point))
        (way (pcase direct
               ('forward 'sniem-forward-char)
               ('backward 'sniem-backward-char)
               (_ (user-error "[Sniem]: The direction for finding is error!")))))
    (funcall way nil t)
    (while (not (or (eolp) (bolp) (eq (following-char) char)))
      (funcall way nil t))
    (when (/= char (following-char))
      (goto-char current-point))))

(sniem-define-motion sniem-next-word (&optional n no-hint word)
  "Move to next word. If the region is active, goto the next word which is same as it."
  (interactive "P")
  (if (or (region-active-p) word sniem-kmacro-mark-content)
      (let ((word (cond (word word)
                        (sniem-kmacro-mark-content
                         (prog1 sniem-kmacro-mark-content
                           (setq-local sniem-kmacro-mark-content nil)))
                        (t (buffer-substring-no-properties (region-beginning)
                                                           (region-end))))))
        (when (= (point) (region-beginning))
          (goto-char (region-end)))
        (deactivate-mark)
        (ignore-errors (search-forward word))
        (push-mark (- (point) (length word)) t t))
    (forward-word n))
  (unless no-hint
    (sniem-motion-hint `(lambda () (interactive)
                          (sniem-next-word ,n t ,word t)))))

(sniem-define-motion sniem-prev-word (&optional n no-hint word)
  "Move to prev word. If the region is active, goto the prev word which is same as it."
  (interactive "P")
  (if (or (region-active-p) word sniem-kmacro-mark-content)
      (let ((word (cond (word word)
                        (sniem-kmacro-mark-content
                         (prog1 sniem-kmacro-mark-content
                           (setq-local sniem-kmacro-mark-content nil)))
                        (t (buffer-substring-no-properties (region-beginning)
                                                           (region-end))))))
        (when (= (point) (region-end))
          (goto-char (region-beginning)))
        (deactivate-mark)
        (search-backward word)
        (push-mark (point) t t)
        (goto-char (+ (point) (length word))))
    (backward-word n))
  (unless no-hint
    (sniem-motion-hint `(lambda () (interactive)
                          (sniem-prev-word ,n t ,word t)))))

(sniem-define-motion sniem-next-symbol (&optional n)
  "Move to next symbol."
  (interactive "P")
  (unless n
    (setq n 1))
  (forward-symbol n)
  (sniem-motion-hint `(lambda () (interactive)
                        (forward-symbol ,n))))

(sniem-define-motion sniem-prev-symbol (&optional n)
  "Move to previous symbol."
  (interactive "P")
  (unless n
    (setq n 1))
  (forward-symbol (- 0 n))
  (sniem-motion-hint `(lambda () (interactive)
                        (forward-symbol (- 0 ,n)))))

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

(sniem-define-motion sniem-goto-prev ()
  "Goto prev lines with `sniem-digit-argument-get'."
  (sniem-prev-line (sniem-digit-argument-get "Move up: ") t)
  (when (and (region-active-p) sniem-mark-line)
    (end-of-line)))

(sniem-define-motion sniem-goto-next ()
  "Goto next lines with `sniem-digit-argument-get'."
  (sniem-next-line (sniem-digit-argument-get "Move down: ") t)
  (when (and (region-active-p) sniem-mark-line)
    (end-of-line)))

(defun sniem-goto-last-point (&optional type non-point-set)
  "Goto `sniem-last-point'.
Optional argument TYPE is the type of the point to go.
Optional argument NON-POINT-SET means not change the last-point."
  (interactive "P")
  (let ((current-point (point)))
    (if type
        (goto-char sniem-last-point)
      (goto-char (if sniem-mark-content-overlay
                     (overlay-start sniem-mark-content-overlay)
                   sniem-last-point)))
    (unless (or sniem-last-point-locked non-point-set)
      (setq-local sniem-last-point current-point))))

(provide 'sniem-operation)

;;; sniem-operation.el ends here
