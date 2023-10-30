;;; sniem-operation.el --- Hands-eased united editing method -*- lexical-binding: t -*-

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

(require 'kmacro)
(require 'sniem-var)
(require 'sniem-macro)
(require 'sniem-common)

(declare-function sniem-object-catch--get-second-char "sniem-object-catch")
(declare-function sniem-current-mode "sniem")
(declare-function sniem-change-mode "sniem")
(declare-function sniem-mark-content "sniem")
(declare-function sniem-lock-unlock-last-point "sniem")
(declare-function sniem-shift--not-alpha-p "sniem")

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
    (unless (= (line-end-position) (point))
      (forward-char)))
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

(defun sniem-open-line ()
  "Open new line."
  (interactive)
  (goto-char (line-end-position))
  (insert "\n")
  (indent-according-to-mode)
  (sniem-insert))

(defun sniem-open-line-previous ()
  "Open new line."
  (interactive)
  (beginning-of-line)
  (insert "\n")
  (if (bobp)
      (goto-char (point-min))
    (forward-line -1))
  (indent-according-to-mode)
  (sniem-insert))

(defun sniem-center (action)
  "Center ACTION for sniem."
  (interactive (list (read-char sniem-center-message)))
  (pcase action
    (?z (recenter nil t))
    (?t (recenter-top-bottom 0))
    (?b (recenter-top-bottom -1))))

(defun sniem-mark (type)
  "Mark the object with action TYPE."
  (interactive (list (read-char sniem-mark-message)))
  (pcase type
    (?l
     (beginning-of-line)
     (push-mark (point) t t)
     (end-of-line)
     (setq-local sniem-mark-line t))
    (?p
     (push-mark sniem-last-point t t)
     (when sniem-last-point-locked
       (sniem-lock-unlock-last-point)))
    (?m (push-mark (point) t t))
    (?f (mark-defun))
    (?b (push-mark (point-min) t t)
        (goto-char (point-max)))
    (_
     (let* ((space-mode (when (= type 32)
                          (progn
                            (setq type (read-char sniem-mark-message))
                            t)))
            (thing (pcase type
                     (?s 'symbol)
                     (?w 'word)
                     (_ (user-error "[Sniem]: The %s type is error!" type))))
            (points (sniem-mark--bounds-of-thing-at-point
                     thing (when (region-active-p)
                             (deactivate-mark)
                             (cons (region-beginning) (region-end))))))
       (when (and points space-mode
                  (save-mark-and-excursion
                    (let (l r)
                      (goto-char (car points))
                      (when (= (char-before) 32)
                        (setq l t))
                      (goto-char (cdr points))
                      (when (= (following-char) 32)
                        (setq r t))
                      (and l r))))
         (setq points (cons (1- (car points)) (1+ (cdr points)))))
       (when points
         (goto-char (car points))
         (push-mark (cdr points) t t))))))

(defun sniem-mark--bounds-of-thing-at-point (thing &optional expand)
  "Get the bounds of theg THING a point.
THING can be `symbol' or `word'.
When EXPAND is non-nil, means expand the current selection.
And its format is like: (start-point . end-point)."
  (let ((move-command 'forward-char)
        (enter-point (point))
        (current-char (following-char))
        (split-char-p (lambda (c) (memq c '(32 9 10))))
        start-point end-point can-enter)
    (save-excursion
      (when expand
        (setq start-point (car expand)
              end-point (cdr expand)))
      (when (if (eq thing 'word)
                (not (or (sniem-pair--pair-p current-char t t)
                         (funcall split-char-p current-char)))
              (and (not (funcall split-char-p current-char))
                   (not (sniem-pair--pair-p current-char t))))
        (setq can-enter t))

      (when can-enter
        (catch 'stop
          ;; When expanding, firstly goto the end-point to expand forward
          (if expand
              (goto-char end-point)
            (funcall move-command))
          (while t
            (setq current-char (following-char))

            (when (or (sniem-pair--pair-p
                       current-char (if expand
                                        (gv-ref expand)
                                      t)
                       (eq thing 'word))
                      (funcall split-char-p current-char))
              (if (eq move-command 'forward-char)
                  (progn
                    (setq move-command 'backward-char
                          end-point (point))
                    (goto-char (if expand
                                   start-point
                                 enter-point)))
                (setq start-point (1+ (point)))
                (throw 'stop t)))

            (when (and (eobp)
                       (eq move-command 'forward-char))
              (setq move-command 'backward-char
                    end-point (point))
              (goto-char enter-point))

            (when (and (bobp)
                       (eq move-command 'backward-char))
              (setq start-point (point))
              (throw 'stop t))

            (funcall move-command)))))
    (when (and start-point end-point)
      (cons start-point end-point))))

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
                     (upcase char)))))
  (sniem-search--refresh-overlay-display))

(defun sniem-replace-char (char)
  "Replace the CHAR under cursor."
  (interactive "c")
  (delete-char 1)
  (insert-char char)
  (sniem-backward-char nil t)
  (sniem-search--modify-cancel-selection))

(defun sniem-replace-word (&optional replace-word)
  "Replace the word under cursor.
When Optional REPLACE-WORD is non-nil, replace original one with it."
  (interactive)
  (sniem-search--cancel-selection)
  (let* ((word (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'word t)))
         (word-points (if (region-active-p)
                          (cons (region-beginning) (region-end))
                        (bounds-of-thing-at-point 'word)))
         (marked-contents '("nil"))
         replace-region-p replaced ov c-ov tmp)
    (when (and (region-active-p)
               sniem-mark-content-overlay
               (listp sniem-mark-content-overlay)
               (null replace-word))
      (setq replace-region-p
            (completing-read "Enter the marked-content or nil to input word: "
                             (progn
                               (dotimes (i (length sniem-mark-content-overlay))
                                 (setq ov (nth i sniem-mark-content-overlay))
                                 (setq marked-contents
                                       (append marked-contents
                                               (list (format
                                                      "%d- %s"
                                                      (1+ i)
                                                      (buffer-substring-no-properties
                                                       (overlay-start ov) (overlay-end ov)))))))
                               marked-contents))))

    (if (and (not (string= "nil" replace-region-p))
             (null replace-word))
        (progn
          (save-mark-and-excursion
            (setq tmp (progn (string-match "^\\(.*\\)- \\(.*\\)" replace-region-p)
                             (match-string 1 replace-region-p))
                  c-ov (nth (1- (string-to-number tmp))
                            sniem-mark-content-overlay)) ;`tmp' is the number of the overlay, `c-ov' is the overlay needs to replace.
            (goto-char (overlay-start c-ov))
            (setq replaced (substring replace-region-p (+ 2 (length tmp)))) ;Set the rest of the contents for replace.
            (delete-region (overlay-start c-ov)
                           (overlay-end c-ov))
            (insert word)
            (delete-overlay c-ov)
            (setq-local sniem-mark-content-overlay
                        (append (list
                                 (make-overlay (- (point) (length word)) (point)))
                                (delete c-ov sniem-mark-content-overlay)))
            (overlay-put (car sniem-mark-content-overlay) 'face 'region))
          (delete-region (region-beginning) (region-end))
          (deactivate-mark))
      (setq replaced (or replace-word
                         (read-string "Enter the new word: " word)))
      (delete-region (car word-points) (cdr word-points)))
    (insert replaced)))

(defun sniem-delete-char ()
  "Delete the char under cursor."
  (interactive)
  (if (eolp)
      (delete-char -1)
    (unless (or (= (following-char) 32)
                (= (following-char) 10))
      (kill-ring-save (point) (1+ (point))))
    (delete-char 1))
  (sniem-search--modify-cancel-selection t))

(defun sniem-delete (action)
  "Delete ACTION."
  (interactive (list (if sniem-delete-edit
                         (progn
                           (setq-local sniem-delete-edit nil)
                           ?p)
                       (if (region-active-p)
                           t
                         (read-char sniem-delete-message)))))
  (sniem-search--modify-cancel-selection t)
  (pcase action
    ((pred symbolp)
     (sniem-delete-region
      (region-beginning)
      (if (save-mark-and-excursion
            (sniem-end-of-mark t)
            (and (not (eobp))
                 (= (point) (line-end-position))
                 sniem-mark-line))
          (1+ (region-end))
        (region-end))))
    (100 (if (= (line-beginning-position) (line-end-position))
             (progn
               (if (bobp)
                   (delete-char 1)
                 (delete-char -1))
               (when (eolp)
                 (forward-line)))
           (sniem-delete-region (line-beginning-position)
                                (if (= (line-end-position) (point-max))
                                    (line-end-position)
                                  (1+ (line-end-position)))))
         (when (eobp)
           (beginning-of-line)))
    (9 (sniem-delete-region (line-beginning-position) (line-end-position)))
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
    (99 (sniem-delete 9) (indent-according-to-mode) (sniem-insert))
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

(defun sniem-yank (action &optional special)
  "Yank ACTION.
If SPECIAL is non-nil, yank it to the special clipboard."
  (interactive (list (if (region-active-p)
                         t
                       (read-char sniem-yank-message))))
  (let ((yank-function (if special
                           #'sniem-yank--special
                         #'kill-ring-save)))
    (pcase action
      ((pred symbolp) (funcall yank-function (region-beginning) (region-end)))
      (121 (funcall yank-function (line-beginning-position)
                    (if (= (point-max) (line-end-position))
                        (line-end-position)
                      (1+ (line-end-position)))))
      (112 (funcall yank-function sniem-last-point (point))
           (when sniem-last-point-locked
             (sniem-lock-unlock-last-point))))))

(defun sniem-yank-in-region (&optional special)
  "Yank in region.
If SPECIAL is non-nil, yank it to the special clipboard."
  (interactive)
  (when (region-active-p)
    (funcall (if special
                 #'sniem-yank--special
               #'kill-ring-save)
             (1+ (region-beginning)) (1- (region-end)))))

(defun sniem-yank--special (beg end)
  "Yank content from BEG to END into special clipboard."
  (setq sniem-special-clipboard
        (append sniem-special-clipboard
                (list
                 (buffer-substring-no-properties
                  beg end))))
  (when (region-active-p)
    (deactivate-mark)))

(defun sniem-paste (&optional n special)
  "Paste the N content in `kill-ring'.
If SPECIAL is non-nil, paste from the special clipboard."
  (interactive "P")
  (let ((i 0)
        ;; For region content replace
        (regionp (when (region-active-p)
                   (cons (region-beginning) (region-end))))
        (tmp (current-kill 0)))
    ;; Append content copied from other applications into the kill-ring
    (unless (string-equal tmp (car kill-ring))
      (setq kill-ring (append (list tmp) kill-ring)))

    (unless n
      ;; Have a check for operation of page or the selection of target content.
      (while (or (null n) (stringp n))
        (setq n (char-to-string
                 (read-char (format "%s:%d%s"
                                    (sniem-paste--output-contents
                                     i
                                     special)
                                    (1+ (/ i 4))
                                    sniem-paste-message))))
        (pcase n
          ("n" (setq i (+ i 4)))
          ("p" (if (>= i 4)
                   (setq i (- i 4))
                 (setq n 1)))
          ("q" (keyboard-quit))
          (_ (setq n (string-to-number n))))))
    (sniem-search--modify-cancel-selection t) ;Exit search status
    (when regionp
      (goto-char (cdr regionp))
      (push-mark (car regionp) t t)
      (sniem-delete t))
    (insert (nth (if (and regionp
                          (null special))
                     (+ n i)
                   (1- (+ n i)))
                 ;; Select the clipboard
                 (if special
                     sniem-special-clipboard
                   kill-ring)))))

(defun sniem-paste--output-contents (n special)
  "Output contents for `sniem-paste'.
Argument N is the page of the contents.
If SPECIAL is non-nil, yank it to the special clipboard."
  (let (content c tmp)
    (dotimes (i 4)
      ;; `c' is a single content that will be show.
      (setq c (format "%d: %s"
                      (1+ i)
                      (ignore-errors    ;For the condition when the result is nil
                        (replace-regexp-in-string ;Replace `\n' to space
                         "\n"
                         "  "
                         (sniem-paste--remove-ln
                          (nth (+ i n)
                               (if special
                                   sniem-special-clipboard
                                 kill-ring)))))))
      ;; Limit the length of content that will be shown
      (when (> (length c) (frame-width))
        (setq c (concat
                 (substring c 0 (- (frame-width) 4))
                 "...")))
      (setq content (concat content c "\n")))
    content))

(defun sniem-paste--remove-ln (string)
  "Remove \n which is at the end of STRING.
Of course, the precondition is that STRING includes it."
  (if (string-suffix-p "\n" string)
      (substring string 0 -1)
    string))

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
  (sniem-search--cancel-selection)
  (let ((last-point (point)))
    (if (bolp)
        (if (save-mark-and-excursion
              (forward-line -1)
              (= (line-beginning-position) (line-end-position)))
            (forward-line -1)
          (backward-char))
      (backward-char)
      (unless (or (= 10 (following-char))
                  (= 32 (following-char))
                  (= 9 (following-char)))
        (user-error "[Sniem]: The current position doesn't need join!")))
    (while (or (= 10 (following-char))
               (= 32 (following-char))
               (= 9 (following-char)))
      (backward-char))
    (forward-char)
    (push-mark last-point t t)))

(defun sniem-macro (action)
  "Macro ACTION."
  (interactive (list (unless defining-kbd-macro
                       (read-char sniem-macro-message))))
  (catch 'stop
    (when defining-kbd-macro
      (end-kbd-macro)

      ;; If the `sniem-kmacro-range' is exists, call the macro to the lines
      (when sniem-kmacro-range
        (sniem-macro--more-line-execute t)
        (delete-overlay sniem-kmacro-range)
        (setq-local sniem-kmacro-range nil)))

    (when (region-active-p)
      (sniem-search--cancel-selection)
      (if (= action ?q)
          (if (= (line-number-at-pos (region-beginning))
                 (line-number-at-pos (region-end)))
              (setq-local sniem-kmacro-mark-content
                          (buffer-substring-no-properties (region-beginning) (region-end)))
            (setq-local sniem-kmacro-range
                        (make-overlay
                         (save-mark-and-excursion
                           (goto-char (region-beginning))
                           (forward-line)
                           (line-beginning-position))
                         (region-end)))
            (goto-char (region-beginning))
            (deactivate-mark))
        (if (= (line-number-at-pos (region-beginning))
               (line-number-at-pos (region-end)))
            (setq-local sniem-kmacro-mark-content
                        (buffer-substring-no-properties (region-beginning) (region-end)))
          (sniem-macro--more-line-execute))))

    (pcase action
      (113 (call-interactively #'start-kbd-macro))
      (101 (if sniem-locked-macro
               (call-interactively sniem-locked-macro)
             (call-last-kbd-macro)))
      (110 (call-interactively #'name-last-kbd-macro))
      (105 (let ((macro (sniem-macro--get-kbd-macros))
                 (file-content ""))
             (unless sniem-macro-file
               (setq sniem-macro-file (read-file-name "Enter the macro file you want: ")))
             (if (file-exists-p sniem-macro-file)
                 (with-temp-buffer
                   (insert-file-contents sniem-macro-file)
                   (setq file-content (buffer-string)))
               (make-empty-file sniem-macro-file))
             (with-temp-file sniem-macro-file
               (goto-char (point-min))
               (insert-kbd-macro macro)
               (insert file-content))))
      (108 (setq sniem-locked-macro
                 (if sniem-locked-macro
                     nil
                   (sniem-macro--get-kbd-macros)))
           (message "[Sniem]: %s locked macro."
                    (if sniem-locked-macro
                        "Set"
                      "Unset")))
      (46 (setq sniem-locked-macro (sniem-macro--get-kbd-macros)))
      (99 (call-interactively (sniem-macro--get-kbd-macros))))))

(defun sniem-macro--more-line-execute (&optional overlay-p)
  "More line execute macro.
When OVERLAY-P is non-nil, use `sniem-macro-range'."
  (sniem-macro--apply-to-lines (if overlay-p
                                   (overlay-start sniem-kmacro-range)
                                 (save-mark-and-excursion
                                   (sniem-beg-of-mark t)
                                   (line-beginning-position)))
                               (if overlay-p
                                   (overlay-end sniem-kmacro-range)
                                 (save-mark-and-excursion
                                   (sniem-end-of-mark t)
                                   (when (= (line-beginning-position)
                                            (line-end-position))
                                     (forward-line))
                                   (line-end-position)))
                               sniem-locked-macro)
  ;; To skip a unneccesary execution.
  (throw 'stop t))

(defun sniem-macro--apply-to-lines (top bottom &optional macro)
  "Apply the MACRO to lines from TOP to BOTTOM."
  (when (region-active-p)
    (deactivate-mark))
  (when (> top bottom)
    (let ((tmp top))
      (setq top bottom
            bottom tmp)))
  (unless macro
    (setq macro last-kbd-macro))
  (goto-char top)
  (let (top-line end-line move-line)
    (setq top-line (line-number-at-pos top)
          end-line (line-number-at-pos bottom))
    (save-mark-and-excursion
      (goto-char bottom)
      (when (= (line-beginning-position) (line-end-position))
        (setq end-line (1- end-line))))
    (setq bottom (- end-line top-line))
    (message "%S, %S" top bottom)

    (while (>= bottom 0)
      (setq move-line (make-overlay (line-beginning-position) (1+ (line-end-position))))
      (execute-kbd-macro macro)
      (goto-char (overlay-end move-line))
      (delete-overlay move-line)
      (setq bottom (1- bottom)))))

(defun sniem-macro--get-kbd-macros ()
  "Get the kbd macro from kmacros."
  (intern
   (completing-read "Enter the macro: "
                    obarray
                    #'kmacro-keyboard-macro-p
                    t)))

(defun sniem-pair (prefix &optional add)
  "Modify the region's pair.
Argument PREFIX is the prefix of the pair.
Optional Argument ADD means forced to add the pair."
  (interactive (list (let ((var (read-char sniem-pair-message)))
                       (cond ((= var 97) ;Forcibly add mode
                              (cons (read-char
                                     (if sniem-pair-message
                                         "Enter the pair:"
                                       nil))
                                    t))
                             ((= var 115) ;Add or delete space mode
                              (cons t 's))
                             (t var)))))
  (sniem-search--cancel-selection)
  (if (eq (cdr-safe prefix) 's)
      (save-mark-and-excursion
        (sniem-space))
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
            (insert second)))))))

(defun sniem-pair--pair-p (char-string &optional connector-check wordp)
  "Check if CHAR-STRING is pair.
When CONNECTOR-CHECK is t, check if the pair is a special
connector of current mode after check pair.
When CONNECTOR-CHECK is a list (pointer),
check if CHAR-STRING is expansion connector
 after normal check failed.

When WORDP is non-nil, connectors will be regarded as pair."
  (if wordp
      (sniem-shift--not-alpha-p char-string t)
    (when (characterp char-string)
      (setq char-string (char-to-string char-string)))
    (let (pairp connectorp connectors)
      (catch 'result
        (dolist (pair sniem-object-catch-global-symbol-alist)
          (when (and (stringp (car pair))
                     (or (string= (car pair) char-string)
                         (string= (cdr pair) char-string)))
            (throw 'result (setq pairp t))))
        (dolist (mode-pair (alist-get major-mode
                                      sniem-object-catch-global-symbol-alist))
          (when (or (equal (car mode-pair) char-string)
                    (equal (cdr mode-pair) char-string))
            (throw 'result (setq pairp t)))))
      (when connector-check
        (setq connectors (sniem-mark--mode-alist-get))
        (setq connectorp (sniem--mems char-string
                                      (append (alist-get 'global sniem-mark-connectors)
                                              connectors)))
        (when (and (null connectorp)
                   (consp connector-check))
          (setq connectorp (sniem--mems char-string
                                        (plist-get connectors :expand)
                                        t))
          (when (eq connectorp t)
            ;; Set the `expand' variable in `sniem-mark--bounds-of-thing-at-point'
            ;; to nil
            (setf (gv-deref connector-check) nil))))
      (or (and pairp (null connectorp))
          (and (null connectorp)
               (sniem-shift--not-alpha-p char-string t))))))

(defun sniem-mark--mode-alist-get ()
  "Try getting the connectors of current mode."
  (catch 'connectors
    (dolist (item sniem-mark-connectors)
      (when (derived-mode-p (car item))
        (throw 'connectors (cdr item))))))

(defun sniem-search (content &optional regexp-search)
  "Search the CONTENT.
When REGEXP-SEARCH is non-nil, add regexp for content automatically."
  (interactive (list (completing-read "Enter the search content: "
                                      regexp-search-ring)
                     current-prefix-arg))
  (when regexp-search
    (setq content (sniem-search--regexp-content content t)))
  (unless (ignore-errors
            (sniem-next-word nil nil content))
    (deactivate-mark)
    (sniem-search--cancel-selection)
    (unless (ignore-errors
              (sniem-prev-word nil nil content))
      (deactivate-mark)
      (sniem-search--cancel-selection)
      (message "[Sniem]: The content %S is not exsits in current buffer." content))))

(defun sniem--string-equal (string1 string2)
  "Like `string-equal', but don't care the case.
STRING1 and STRING2 are the strings to compair."
  (string-equal (downcase string1) (downcase string2)))

(defun sniem-space ()
  "Add or delete space around the marked region."
  (interactive)
  (let ((marked-content (buffer-substring-no-properties
                         (region-beginning)
                         (region-end))))
    (if (and (string-prefix-p " " marked-content)
             (string-suffix-p " " marked-content))
        (sniem-replace-word (substring marked-content 1 -1))
      (sniem-replace-word (format " %s " marked-content)))))

;;; Motions

(sniem-define-motion sniem-beginning-of-line ()
  "Beginning of line."
  (if (bolp)
      (call-interactively #'indent-for-tab-command)
    (beginning-of-line)))

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
  (let ((line (line-number-at-pos)))
    (setq n (or n 1))
    (unless (bobp)
      (sniem-line-move (- n)))
    (when (and (region-active-p) sniem-mark-line)
      (while (= (line-number-at-pos) line)
        (sniem-line-move -1))
      (if (= (region-beginning) (point))
          (beginning-of-line)
        (end-of-line)))))

(sniem-define-motion sniem-5-prev-line ()
  "Eval `sniem-prev-line' 5 times."
  (sniem-prev-line 5 t))

(sniem-define-motion sniem-next-line (&optional n)
  "Next line."
  (interactive "P")
  (let ((line (line-number-at-pos)))
    (setq n (or n 1))
    (unless (eobp)
      (sniem-line-move n))
    (when (and (region-active-p) sniem-mark-line)
      (while (= (line-number-at-pos) line)
        (sniem-line-move 1))
      (if (= (region-beginning) (point))
          (beginning-of-line)
        (end-of-line)))))

(sniem-define-motion sniem-5-next-line ()
  "Eval `sniem-next-line' 5 times."
  (sniem-next-line 5 t))

(defun sniem-line-move (n)
  "Line move N for sniem."
  (let ((target-line (+ (line-number-at-pos) n))
        (target-pos (- (point) (line-beginning-position)))
        current-pos)
    (setq current-pos (goto-line target-line))
    (if (> target-pos (- (line-end-position) (line-beginning-position)))
        (end-of-line)
      (forward-char target-pos))))

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
  "Move to next word.  If the region is active, goto the next word which is same as it."
  (interactive "P")
  (if (or (region-active-p) word sniem-kmacro-mark-content sniem-search-result-overlays)
      (let* ((word (cond (word word)
                         (sniem-kmacro-mark-content
                          (prog1 sniem-kmacro-mark-content
                            (setq-local sniem-kmacro-mark-content nil)))
                         ((consp sniem-search-result-overlays)
                          (car sniem-search-result-overlays))
                         ((region-active-p) (buffer-substring-no-properties (region-beginning)
                                                                            (region-end)))))
             tmp region-end ov)

        (when (region-active-p)
          (unless sniem-search-result-overlays
            (setq word (sniem-search--regexp-content word)))

          (goto-char (region-beginning))
          (setq region-end (region-end)))
        (if (and sniem-search-result-overlays
                 (string= (car sniem-search-result-overlays)
                          word)
                 (setq tmp (sniem--list-memq sniem-search-result-overlays
                                             (overlays-at (point))
                                             'index)))
            (ignore-errors
              (goto-char (overlay-start
                          (setq ov
                                (nth (1+ tmp) sniem-search-result-overlays))))
              (push-mark (overlay-end ov) t t)
              (sniem-search--check-result (1+ tmp)))
          (when region-end
            (goto-char region-end))
          (setq tmp (search-forward-regexp word nil t))
          (if tmp
              (progn
                (when (region-active-p)
                  (deactivate-mark))
                (setq tmp (point))
                (search-backward-regexp word)
                (push-mark tmp t t)
                (sniem-add-to-history word)
                (when (or (null sniem-search-result-overlays)
                          (not (string= (car sniem-search-result-overlays)
                                        word)))
                  (when (timerp sniem-search-timer)
                    (cancel-timer sniem-search-timer))
                  (setq-local sniem-search-timer
                              (run-with-timer
                               0 10 #'sniem-search--highlight-results
                               word (current-buffer)))))
            (user-error "[Sniem]: Cannot find the content!"))))
    (forward-word n)
    (unless no-hint
      (sniem-motion-hint `(lambda () (interactive)
                            (sniem-next-word ,n t ,word t))))))

(sniem-define-motion sniem-prev-word (&optional n no-hint word)
  "Move to prev word.  If the region is active, goto the prev word which is same as it."
  (interactive "P")
  (if (or (region-active-p) word sniem-kmacro-mark-content sniem-search-result-overlays)
      (let* ((word (cond (word word)
                         (sniem-kmacro-mark-content
                          (prog1 sniem-kmacro-mark-content
                            (setq-local sniem-kmacro-mark-content nil)))
                         ((consp sniem-search-result-overlays)
                          (car sniem-search-result-overlays))
                         ((region-active-p) (buffer-substring-no-properties (region-beginning)
                                                                            (region-end)))))
             tmp ov)

        (when (region-active-p)
          (unless sniem-search-result-overlays
            (setq word (sniem-search--regexp-content word)))

          (when (not (= (point) (region-beginning)))
            (goto-char (region-beginning))))
        (if (and sniem-search-result-overlays
                 (string= (car sniem-search-result-overlays)
                          word)
                 (setq tmp (sniem--list-memq sniem-search-result-overlays
                                             (overlays-at (point))
                                             'index)))
            (ignore-errors
              (goto-char (overlay-start
                          (setq ov
                                (nth (1- tmp) sniem-search-result-overlays))))
              (push-mark (overlay-end ov) t t)
              (sniem-search--check-result (1- tmp)))
          (setq tmp (search-backward-regexp word nil t))
          (if tmp
              (progn
                (when (region-active-p)
                  (deactivate-mark))
                (setq tmp (point))
                (push-mark (search-forward-regexp word) t t)
                (goto-char tmp)
                (sniem-add-to-history word)
                (when (or (null sniem-search-result-overlays)
                          (not (string= (car sniem-search-result-overlays)
                                        word)))
                  (when (timerp sniem-search-timer)
                    (cancel-timer sniem-search-timer))
                  (setq-local sniem-search-timer
                              (run-with-timer
                               0 10 #'sniem-search--highlight-results
                               word (current-buffer)))))
            (user-error "[Sniem]: Cannot find the content!"))))
    (backward-word n)
    (unless no-hint
      (sniem-motion-hint `(lambda () (interactive)
                            (sniem-prev-word ,n t ,word t))))))

(defun sniem-search--check-result (&optional number)
  "Check the search result.
NUMBER is the index of current overlay in all search overlays.
Or NUMBER can be 'remove, then the function'll only remove the tip.

If it's the first, print first at the end of the line.
Else if it's the last, print last, or it's the only one, print only."
  (when sniem-search-result-tip
    (delete-overlay sniem-search-result-tip)
    (setq-local sniem-search-result-tip nil))
  (unless (eq number 'remove)
    (let ((total (1- (length sniem-search-result-overlays)))
          current)
      (setq current (if (numberp number)
                        number
                      (sniem--list-memq sniem-search-result-overlays
                                        (overlays-at (point))
                                        'index)))
      (when current
        (sniem-search--add-overlay (format "[%d/%d]"
                                           current total))))))

(defun sniem-search--add-overlay (content)
  "Add the tip overlay with CONTENT."
  (let ((ov (make-overlay (line-end-position) (1+ (line-end-position)))))
    (overlay-put ov 'face 'font-lock-comment-face)
    (overlay-put ov 'display (concat content "\n"))
    (setq-local sniem-search-result-tip ov)))

(defun sniem-add-to-history (search)
  "Add the SEARCH content to the `search-ring'."
  (setq search (downcase search))
  (if (sniem--mems search regexp-search-ring)
      (unless (string-equal search (car regexp-search-ring))
        (setq regexp-search-ring (delete search regexp-search-ring))
        (add-to-history 'regexp-search-ring search regexp-search-ring-max))
    (add-to-history 'regexp-search-ring search regexp-search-ring-max)))

(defun sniem-search--highlight-results (content buffer)
  "Refresh the result for CONTENT in BUFFER."
  (let ((lint (when (or (null sniem-search-result-overlays)
                        (sniem-search--delete-search-overlays))
                t)))
    (with-current-buffer buffer
      (save-mark-and-excursion
        (deactivate-mark)
        (goto-char (point-min))
        (let (point ov)
          (while (search-forward-regexp content nil t)
            (setq point (point))
            (setq ov (make-overlay (search-backward-regexp content nil) point))
            (overlay-put ov 'face 'region)
            (goto-char point)
            (setq-local sniem-search-result-overlays
                        (append sniem-search-result-overlays
                                (list ov))))
          (when sniem-search-result-overlays
            (setq-local sniem-search-result-overlays
                        (append (list content)
                                sniem-search-result-overlays)))))
      (when lint
        (sniem-search--check-result)))))

(defun sniem-search--refresh-overlay-display ()
  "Refresh search overlays display."
  (when (consp sniem-search-result-overlays)
    (save-mark-and-excursion
      (let (ov ovs tmp)
        (dotimes (i (length sniem-search-result-overlays))
          (setq ov (nth i sniem-search-result-overlays))

          (when (overlayp ov)
            (goto-char (overlay-start ov))
            (unless (memq ov (overlays-at (setq tmp (point))))
              (delete-overlay ov)
              (setq ov (make-overlay
                        tmp (+ tmp
                               (length (car sniem-search-result-overlays)))))
              (overlay-put ov 'face 'region)
              (setq ovs (append ovs
                                (list (cons (1- i)
                                            ov)))))))
        (when ovs
          (dolist (item ovs)
            (setq-local sniem-search-result-overlays
                        (append (sniem--nth-utill
                                 0 (car item)
                                 sniem-search-result-overlays)
                                (list (cdr item))
                                (sniem--nth-utill
                                 (1+ (car item)) nil
                                 sniem-search-result-overlays)))))))))

(defun sniem-search--delete-search-overlays (&optional overlay)
  "Delete search overlays.
If OVERLAY is non-nil, only delete it."
  (when (consp sniem-search-result-overlays)
    (if (overlayp overlay)
        (progn
          (setq-local sniem-search-result-overlays
                      (delete overlay
                              sniem-search-result-overlays))
          (delete-overlay overlay)
          (sniem-search--check-result 'remove))
      (dolist (ov sniem-search-result-overlays)
        (when (overlayp ov)
          (delete-overlay ov)))
      (setq-local sniem-search-result-overlays nil))
    t))

(defun sniem-search--cancel-selection ()
  "Cancel selection."
  (sniem-search--check-result 'remove)
  (when sniem-search-timer
    (cancel-timer sniem-search-timer)
    (setq-local sniem-search-timer nil))
  (sniem-search--delete-search-overlays))

(defun sniem-search--modify-cancel-selection (&optional force)
  "Judge the modify for cancelling selection.
If FORCE is non-nil, forcibly delete the current overlay."
  (let (tmp)
    (when (and sniem-search-result-overlays
               (setq tmp
                     (sniem--list-memq sniem-search-result-overlays
                                       (overlays-at (point))))
               (or (not (string= (buffer-substring-no-properties
                                  (overlay-start tmp)
                                  (overlay-end tmp))
                                 (car sniem-search-result-overlays)))
                   force))
      (sniem-search--delete-search-overlays tmp))))

(defun sniem-search--regexp-content (content &optional force)
  "Add symbol regexp for CONTENT with conditions.
If FORCE is non-nil, add regexp for content however condition
is true."
  (if (and (string-prefix-p "\\" content)
           (string-suffix-p ">" content))
      content
    (let ((symbol-points (sniem-mark--bounds-of-thing-at-point 'symbol)))
      (format (if (or force
                      (and symbol-points
                           (region-active-p)
                           (= (- (cdr symbol-points) (car symbol-points))
                              (length content))))
                  "\\_<%s\\_>"
                "\\<%s\\>")
              content))))

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
  (sniem-goto-line (- (line-number-at-pos)
                      (sniem-digit-argument-get "Move up: "))
                   t)
  (when (and (region-active-p) sniem-mark-line)
    (end-of-line)))

(sniem-define-motion sniem-goto-next ()
  "Goto next lines with `sniem-digit-argument-get'."
  (sniem-goto-line (+ (line-number-at-pos)
                      (sniem-digit-argument-get "Move down: "))
                   t)
  (when (and (region-active-p) sniem-mark-line)
    (end-of-line)))

(defun sniem-goto-last-point (&optional type non-point-set)
  "Goto `sniem-last-point'.
Optional argument TYPE is the type of the point to go.
Optional argument NON-POINT-SET means not change the last-point."
  (interactive "P")
  (let ((current-point (point)))
    (if (or (eq 0 type) (null sniem-mark-content-overlay))
        (goto-char sniem-last-point)

      (goto-char
       (if (= 1 (length sniem-mark-content-overlay))
           (overlay-start (car sniem-mark-content-overlay)) ;Goto the first marked-content if there's only one overlay.

         (let ((current-ov (sniem--list-memq sniem-mark-content-overlay
                                             (overlays-at (point)) 'index))
               (notice (lambda (n)
                         (message "[Sniem]: Jumped to: %d" n)))
               next-ov)
           (cond ((and (numberp type)
                       (<= type (length sniem-mark-content-overlay)))
                  (funcall notice type)
                  (overlay-start (nth (1- type) sniem-mark-content-overlay)))

                 ((null current-ov)
                  (funcall notice 1)
                  (overlay-start (car sniem-mark-content-overlay)))

                 ((and (numberp type)
                       (> type (length sniem-mark-content-overlay)))
                  (user-error "[Sniem]: The number %d of marked-content can't be found!"
                              type))

                 (t (setq next-ov (nth (1+ current-ov) sniem-mark-content-overlay))
                    (if (null next-ov)
                        (progn
                          (funcall notice 1)
                          (overlay-start (car sniem-mark-content-overlay))) ;If the current overlay is the last, goto the first one.
                      (funcall notice (+ 2 current-ov))
                      (overlay-start next-ov))))))))

    (unless (or sniem-last-point-locked non-point-set)
      (setq-local sniem-last-point current-point))))

(provide 'sniem-operation)

;;; sniem-operation.el ends here
