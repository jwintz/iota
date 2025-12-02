;;; iota-box.el --- Box drawing utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: faces
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Box drawing primitives for I O T Λ (I Ø T Δ) TUI framework.
;; Provides utilities for rendering boxes with various styles
;; (single, double, rounded, heavy) and alignment options.

;;; Code:

(require 'cl-lib)

;;; Box Drawing Character Sets

(defconst iota-box-chars-single
  '(:top-left "┌" :top-right "┐"
    :bottom-left "└" :bottom-right "┘"
    :horizontal "─" :vertical "│"
    :cross "┼" :t-down "┬" :t-up "┴" :t-right "├" :t-left "┤")
  "Single-line box-drawing characters.")

(defconst iota-box-chars-double
  '(:top-left "╔" :top-right "╗"
    :bottom-left "╚" :bottom-right "╝"
    :horizontal "═" :vertical "║"
    :cross "╬" :t-down "╦" :t-up "╩" :t-right "╠" :t-left "╣")
  "Double-line box-drawing characters.")

(defconst iota-box-chars-rounded
  '(:top-left "╭" :top-right "╮"
    :bottom-left "╰" :bottom-right "╯"
    :horizontal "─" :vertical "│"
    :cross "┼" :t-down "┬" :t-up "┴" :t-right "├" :t-left "┤")
  "Rounded box-drawing characters.")

(defconst iota-box-chars-heavy
  '(:top-left "┏" :top-right "┓"
    :bottom-left "┗" :bottom-right "┛"
    :horizontal "━" :vertical "┃"
    :cross "╋" :t-down "┳" :t-up "┻" :t-right "┣" :t-left "┫")
  "Heavy box-drawing characters.")

(defconst iota-box-chars-heavy-rounded
  '(:top-left "╭" :top-right "╮"
    :bottom-left "╰" :bottom-right "╯"
    :horizontal "━" :vertical "┃"
    :cross "╋" :t-down "┳" :t-up "┻" :t-right "┣" :t-left "┫")
  "Heavy rounded box-drawing characters (rounded corners, heavy lines).")

(defconst iota-box-chars-modern-thin
  '(:top-left "┌" :top-right "┐"
    :bottom-left "└" :bottom-right "┘"
    :horizontal "─" :vertical "│"
    :cross "╪" :t-down "╤" :t-up "╧" :t-right "╞" :t-left "╡")
  "Modern thin box-drawing characters.")

(defconst iota-box-chars-modern-thick
  '(:top-left "┏" :top-right "┓"
    :bottom-left "┗" :bottom-right "┛"
    :horizontal "━" :vertical "┃"
    :cross "╋" :t-down "┳" :t-up "┻" :t-right "┣" :t-left "┫")
  "Modern thick box-drawing characters.")

(defconst iota-box-chars-ascii
  '(:top-left "+" :top-right "+"
    :bottom-left "+" :bottom-right "+"
    :horizontal "-" :vertical "|"
    :cross "+" :t-down "+" :t-up "+" :t-right "+" :t-left "+")
  "ASCII fallback box characters.")

;;; Box Style Management

(defcustom iota-box-default-style 'rounded
  "Default box drawing style.
Can be: single, double, rounded, heavy, heavy-rounded, 
modern-thin, modern-thick, or ascii."
  :type '(choice (const :tag "Single line" single)
                 (const :tag "Double line" double)
                 (const :tag "Rounded corners" rounded)
                 (const :tag "Heavy line" heavy)
                 (const :tag "Heavy rounded" heavy-rounded)
                 (const :tag "Modern thin" modern-thin)
                 (const :tag "Modern thick" modern-thick)
                 (const :tag "ASCII (compatibility)" ascii))
  :group 'iota)

(defcustom iota-box-style-fallback-chain
  '(heavy-rounded rounded heavy single ascii)
  "Fallback chain for box styles (in order of preference)."
  :type '(repeat symbol)
  :group 'iota)

(defun iota-box-get-chars (style)
  "Get box-drawing character set for STYLE.
STYLE can be: single, double, rounded, heavy, heavy-rounded,
modern-thin, modern-thick, or ascii."
  (pcase style
    ('single iota-box-chars-single)
    ('double iota-box-chars-double)
    ('rounded iota-box-chars-rounded)
    ('heavy iota-box-chars-heavy)
    ('heavy-rounded iota-box-chars-heavy-rounded)
    ('modern-thin iota-box-chars-modern-thin)
    ('modern-thick iota-box-chars-modern-thick)
    ('ascii iota-box-chars-ascii)
    (_ iota-box-chars-rounded)))

(defun iota-box-style-available-p (style)
  "Check if STYLE characters are displayable in current environment."
  (let* ((chars (iota-box-get-chars style))
         (test-chars (list (plist-get chars :top-left)
                          (plist-get chars :horizontal)
                          (plist-get chars :vertical))))
    (cl-every #'char-displayable-p 
              (mapcar (lambda (s) (string-to-char s)) test-chars))))

(defun iota-box-select-best-style (&optional preferred)
  "Select best available box style, preferring PREFERRED if available."
  (or (and preferred 
           (iota-box-style-available-p preferred)
           preferred)
      (cl-find-if #'iota-box-style-available-p
                  iota-box-style-fallback-chain)
      'ascii))

(defun iota-box-init-style ()
  "Initialize box style based on terminal capabilities."
  (let ((selected (iota-box-select-best-style)))
    (setq iota-box-default-style selected)))

(defun iota-box-char (style char-key)
  "Get specific character CHAR-KEY from STYLE char set.
CHAR-KEY can be: :top-left, :horizontal, :vertical, etc."
  (plist-get (iota-box-get-chars style) char-key))

;;; Core Box Rendering

(defun iota-box-horizontal-line (width style &optional face)
  "Render horizontal line of WIDTH using STYLE.
Optional FACE applies text properties."
  (let* ((char (iota-box-char style :horizontal))
         (line (make-string width (string-to-char char))))
    (if face
        (propertize line 'face face)
      line)))

(defun iota-box-vertical-line (height style &optional face)
  "Render vertical line of HEIGHT using STYLE.
Optional FACE applies text properties."
  (let* ((char (iota-box-char style :vertical))
         (lines (make-list height char)))
    (if face
        (mapconcat (lambda (c) (propertize c 'face face)) lines "\n")
      (mapconcat #'identity lines "\n"))))

(defun iota-box-top-border (width style &optional face dividers)
  "Render top border of WIDTH using STYLE.
Optional FACE applies text properties.
DIVIDERS is a list of 0-indexed positions to place connectors."
  (let* ((left (iota-box-char style :top-left))
         (right (iota-box-char style :top-right))
         (t-down (iota-box-char style :t-down))
         (horiz (iota-box-char style :horizontal))
         (line-len (- width 2))
         (line (make-string line-len (string-to-char horiz))))
    (when dividers
      (dolist (pos dividers)
        ;; Adjust pos for border string (subtract 1 for left border char)
        (let ((border-pos (- pos 1)))
          (when (and (>= border-pos 0) (< border-pos line-len))
            (aset line border-pos (string-to-char t-down))))))
    (let ((border (concat left line right)))
      (if face
          (propertize border 'face face)
        border))))

(defun iota-box-bottom-border (width style &optional face dividers)
  "Render bottom border of WIDTH using STYLE.
Optional FACE applies text properties.
DIVIDERS is a list of 0-indexed positions to place connectors."
  (let* ((left (iota-box-char style :bottom-left))
         (right (iota-box-char style :bottom-right))
         (t-up (iota-box-char style :t-up))
         (horiz (iota-box-char style :horizontal))
         (line-len (- width 2))
         (line (make-string line-len (string-to-char horiz))))
    (when dividers
      (dolist (pos dividers)
        ;; Adjust pos for border string (subtract 1 for left border char)
        (let ((border-pos (- pos 1)))
          (when (and (>= border-pos 0) (< border-pos line-len))
            (aset line border-pos (string-to-char t-up))))))
    (let ((border (concat left line right)))
      (if face
          (propertize border 'face face)
        border))))

(defun iota-box-content-line (content width style &optional face align)
  "Render CONTENT line within box of WIDTH using STYLE.
ALIGN can be: left, center, right (default: left)."
  (let* ((vert (iota-box-char style :vertical))
         (content-width (- width 4)) ; 2 chars for borders, 2 for padding
         (content-len (length content))
         (padding (max 0 (- content-width content-len)))
         (aligned-content
          (pcase align
            ('center
             (let* ((left-pad (/ padding 2))
                    (right-pad (- padding left-pad)))
               (concat (make-string left-pad ?\s)
                       content
                       (make-string right-pad ?\s))))
            ('right
             (concat (make-string padding ?\s) content))
            (_  ; 'left or default
             (concat content (make-string padding ?\s)))))
         (line (concat vert " " aligned-content " " vert)))
    (if face
        (propertize line 'face face)
      line)))

;;; High-Level Box Rendering

(cl-defun iota-box-render (&key content width style face align)
  "Render a box around CONTENT.

Arguments:
  :content  String or list of strings to display
  :width    Box width in characters (default: auto)
  :style    Box style (default: `iota-box-default-style')
  :face     Face to apply to box (default: nil)
  :align    Content alignment: left, center, right (default: left)

Returns: Multi-line string representing the box."
  (let* ((style (or style iota-box-default-style))
         (content-lines (if (stringp content)
                            (list content)
                          content))
         (max-content-width (apply #'max (mapcar #'length content-lines)))
         (width (or width (+ max-content-width 4))) ; +4 for borders and padding
         (width (max width 10))) ; Minimum width
    (concat
     (iota-box-top-border width style face) "\n"
     (mapconcat (lambda (line)
                  (iota-box-content-line line width style face align))
                content-lines
                "\n")
     "\n"
     (iota-box-bottom-border width style face))))

;;; Single-Line Box (for mode-line/header-line)

(cl-defun iota-box-render-single-line (&key left center right width style face compact)
  "Render single-line box for mode-line/header-line.

Arguments:
  :left     Left-aligned content (can be a list of strings for multiple segments)
  :center   Center-aligned content (can be a list of strings for multiple segments)
  :right    Right-aligned content (can be a list of strings for multiple segments)
  :width    Total width (default: window-width)
  :style    Box style (default: `iota-box-default-style')
  :face     Face to apply
  :compact  If t, render as a single line (no top/bottom borders)

Returns: String with box decorations."
  (let* ((style (or style iota-box-default-style))
         (width (or width (window-width)))
         (chars (iota-box-get-chars style))
         (vert (plist-get chars :vertical))
         (sep-char (if face (propertize vert 'face face) vert))
         (sep (concat " " sep-char " "))
         
         ;; Handle list or string inputs for segments
         (left-parts (if (listp left) left (list left)))
         (center-parts (if (listp center) center (list center)))
         (right-parts (if (listp right) right (list right)))
         
         ;; Filter out nil and empty strings
         (left-parts (cl-remove-if (lambda (s) (or (null s) (string-empty-p s))) left-parts))
         (center-parts (cl-remove-if (lambda (s) (or (null s) (string-empty-p s))) center-parts))
         (right-parts (cl-remove-if (lambda (s) (or (null s) (string-empty-p s))) right-parts))
         
         ;; Join segments with separator
         (left-content (mapconcat #'identity left-parts sep))
         (center-content (mapconcat #'identity center-parts sep))
         (right-content (mapconcat #'identity right-parts sep))
         
         (content-width (- width 4)) ; borders (2) + padding (2)
         (left-len (string-width left-content))
         (center-len (string-width center-content))
         (right-len (string-width right-content))
         (used-width (+ left-len center-len right-len))
         (remaining (max 0 (- content-width used-width)))
         
         ;; Build content line and track divider positions
         (content-parts (list))
         (dividers (list))
         (current-pos 2)) ; Start after "│ "
    
    ;; Add left content
    (when (> left-len 0)
      (push left-content content-parts)
      ;; Track positions of separators within left content
      (let ((pos 0))
        (dolist (part left-parts)
          (setq pos (+ pos (string-width part)))
          (when (not (eq part (car (last left-parts))))
            (push (+ current-pos pos 1) dividers) ; +1 for space before separator
            (setq pos (+ pos 3))))) ; +3 for " │ "
      (setq current-pos (+ current-pos left-len)))
    
     ;; Calculate spacing distribution
    ;; If we have center content, split space around it
    ;; Otherwise, all space goes between left and right
    ;; Use explicit face spec to prevent inheritance from buffer
    (let ((space-face (if face
                          ;; If we have a box face, apply similar attributes to spacing
                          `(:inherit ,face :slant normal :weight normal)
                        ;; Otherwise use mode-line with explicit attributes
                        '(:inherit mode-line :slant normal :weight normal))))
      (cond
       ;; Case 1: We have center content - distribute space around it
       ((> center-len 0)
        (let* ((left-gap (/ remaining 2))
               (right-gap (- remaining left-gap)))
          ;; Add left gap (with explicit face to prevent inheritance)
          (when (> left-gap 0)
            (push (propertize (make-string left-gap ?\s) 'face space-face) content-parts)
            (setq current-pos (+ current-pos left-gap)))
          ;; Add center content
          (push center-content content-parts)
          (let ((pos 0))
            (dolist (part center-parts)
              (setq pos (+ pos (string-width part)))
              (when (not (eq part (car (last center-parts))))
                (push (+ current-pos pos 1) dividers)
                (setq pos (+ pos 3)))))
          (setq current-pos (+ current-pos center-len))
          ;; Add right gap (with explicit face)
          (when (> right-gap 0)
            (push (propertize (make-string right-gap ?\s) 'face space-face) content-parts)
            (setq current-pos (+ current-pos right-gap)))))
       
       ;; Case 2: No center content - all space between left and right
       (t
        (when (> remaining 0)
          (push (propertize (make-string remaining ?\s) 'face space-face) content-parts)
          (setq current-pos (+ current-pos remaining))))))    ;; Add right content
    (when (> right-len 0)
      (push right-content content-parts)
      ;; Track positions of separators within right content
      (let ((pos 0))
        (dolist (part right-parts)
          (setq pos (+ pos (string-width part)))
          (when (not (eq part (car (last right-parts))))
            (push (+ current-pos pos 1) dividers)
            (setq pos (+ pos 3))))))
    
    (setq dividers (sort dividers #'<))
    (setq content-parts (nreverse content-parts))
    (let* ((left-border (if face (propertize vert 'face face) vert))
           (right-border (if face (propertize vert 'face face) vert))
           ;; Padding spaces need explicit face spec to prevent buffer face inheritance
           (padding-space (propertize " " 'face (if face
                                                    `(:inherit ,face :slant normal :weight normal)
                                                  '(:inherit mode-line :slant normal :weight normal))))
           (content-str (apply #'concat content-parts))
           ;; SAFETY: Truncate content if it exceeds available width
           ;; This prevents line overflow and broken box rendering
           (content-max-width (- width 4))  ; Account for borders and padding
           (content-str (if (> (string-width content-str) content-max-width)
                            (truncate-string-to-width content-str content-max-width nil nil "…")
                          content-str))
           ;; Pad content if it's shorter than expected (to maintain box width)
           (content-str (let ((current-width (string-width content-str)))
                          (if (< current-width content-max-width)
                              (concat content-str
                                      (propertize (make-string (- content-max-width current-width) ?\s)
                                                  'face (if face
                                                            `(:inherit ,face :slant normal :weight normal)
                                                          '(:inherit mode-line :slant normal :weight normal))))
                            content-str)))
           (content-line (concat left-border padding-space content-str padding-space right-border)))

      (if compact
          content-line
        (let ((top (iota-box-top-border width style face dividers))
              (bottom (iota-box-bottom-border width style face dividers)))
          (concat top "\n"
                  content-line
                  "\n"
                  bottom))))))

;;; Separator Lines

(defun iota-box-separator (width style &optional face)
  "Render separator line of WIDTH using STYLE."
  (let* ((left (iota-box-char style :t-right))
         (right (iota-box-char style :t-left))
         (line (iota-box-horizontal-line (- width 2) style))
         (sep (concat left line right)))
    (if face
        (propertize sep 'face face)
      sep)))

;;; Utility Functions

(defun iota-box-truncate (text max-width &optional ellipsis)
  "Truncate TEXT to MAX-WIDTH, adding ELLIPSIS if needed.
ELLIPSIS defaults to \"...\"."
  (let ((ellipsis (or ellipsis "...")))
    (if (<= (length text) max-width)
        text
      (concat (substring text 0 (- max-width (length ellipsis)))
              ellipsis))))

(defun iota-box-pad (text width &optional align)
  "Pad TEXT to WIDTH with ALIGN (left, center, right)."
  (let* ((text-len (length text))
         (padding (max 0 (- width text-len))))
    (pcase align
      ('center
       (let* ((left-pad (/ padding 2))
              (right-pad (- padding left-pad)))
         (concat (make-string left-pad ?\s)
                 text
                 (make-string right-pad ?\s))))
      ('right
       (concat (make-string padding ?\s) text))
      (_
       (concat text (make-string padding ?\s))))))

(defun iota-box--get-current-window-width ()
  "Get the width of the window displaying current buffer.
Returns the window body width minus 1 for line wrapping margin."
  (let ((win (selected-window)))
    (when win
      (max 1 (1- (window-body-width win))))))

(defun iota-box-insert-separator (&optional style face)
  "Insert a full-width separator line in the current buffer.
Uses STYLE (defaults to `iota-box-default-style') and FACE
(defaults to `iota-muted-face' for a dimmer appearance).
The separator spans the full window width and updates dynamically on resize."
  (let* ((style (or style iota-box-default-style))
         (face (or face 'iota-muted-face))
         (start (point)))
    ;; Insert a marker that will be replaced by the actual separator
    (insert-char ?\s 1)
    (insert "\n")
    ;; Create an overlay that will render the separator dynamically
    (let ((ov (make-overlay start (1+ start))))
      (overlay-put ov 'iota-separator t)
      (overlay-put ov 'iota-separator-style style)
      (overlay-put ov 'iota-separator-face face)
      (overlay-put ov 'evaporate t)
      ;; Initial render
      (iota-box--update-separator-overlay ov))))

(defun iota-box--update-separator-overlay (ov)
  "Update the display of separator overlay OV based on current window width."
  (when (overlay-buffer ov)
    (let* ((style (overlay-get ov 'iota-separator-style))
           (face (overlay-get ov 'iota-separator-face))
           (width (or (iota-box--get-current-window-width) 79))
           (sep (iota-box-horizontal-line width style face)))
      (overlay-put ov 'display sep))))

(defun iota-box--update-all-separators ()
  "Update all separator overlays in the current buffer."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'iota-separator)
      (iota-box--update-separator-overlay ov))))

(defvar-local iota-box--separator-update-timer nil
  "Timer for debouncing separator updates.")

(defun iota-box--schedule-separator-update ()
  "Schedule a separator update, debounced to avoid excessive updates."
  (when iota-box--separator-update-timer
    (cancel-timer iota-box--separator-update-timer))
  (setq iota-box--separator-update-timer
        (run-with-idle-timer 0.05 nil
                            (lambda (buf)
                              (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (iota-box--update-all-separators))))
                            (current-buffer))))

(defun iota-box--enable-dynamic-separators ()
  "Enable dynamic separator updates for the current buffer."
  (add-hook 'window-configuration-change-hook
            #'iota-box--schedule-separator-update nil t)
  (add-hook 'window-size-change-functions
            (lambda (_frame) (iota-box--schedule-separator-update)) nil t))

(defun iota-box-insert-separator-with-tees (&optional style face)
  "Insert a full-width separator with T-connectors in the current buffer.
Uses STYLE (defaults to `iota-box-default-style') and FACE
(defaults to `iota-muted-face' for a dimmer appearance).
The separator spans the full window width."
  (let* ((style (or style iota-box-default-style))
         (face (or face 'iota-muted-face))
         ;; Use window-body-width for actual text area width, minus 1 for the newline
         (width (cond
                 ((get-buffer-window (current-buffer))
                  (1- (window-body-width (get-buffer-window (current-buffer)))))
                 ((and (fboundp 'frame-width) (frame-width))
                  (1- (frame-width)))
                 (t 79)))
         (sep (iota-box-separator width style face)))
    (insert sep)
    (insert "\n")))

(provide 'iota-box)
;;; iota-box.el ends here
