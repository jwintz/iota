;;; iota-segment.el --- Segment system for I O T Λ modeline/UI -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: faces, modeline
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Segment system for I O T Λ (I Ø T Δ) modeline and UI components.
;; Provides a protocol for creating, composing, and updating segments
;; with caching, alignment, and dynamic formatting.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Segment Protocol

(cl-defstruct (iota-segment
               (:constructor iota-segment-create)
               (:copier nil))
  "A modeline/UI segment.

Slots:
  id          Symbol uniquely identifying this segment
  text        String content or function returning string
  short-text  Shorter version for constrained displays (optional)
  face        Face or function returning face
  align       Alignment: left, center, right
  priority    Display priority (higher = more important)
  min-priority Minimum priority threshold to show this segment (default 0)
  update-on   Event triggering update: always, point, timer, manual
  cache-key   Key for caching (if nil, no caching)
  help-echo   Tooltip text or function
  keymap      Keymap for mouse interactions
  visible-fn  Function determining visibility (optional)"
  id
  text
  short-text
  face
  (align 'left)
  (priority 50)
  (min-priority 0)
  (update-on 'always)
  cache-key
  help-echo
  keymap
  visible-fn)

;;; Segment Cache

(defvar iota-segment--cache (make-hash-table :test 'equal)
  "Cache for segment computations.")

(defun iota-segment-cache-get (key)
  "Get cached value for KEY."
  (gethash key iota-segment--cache))

(defun iota-segment-cache-set (key value)
  "Set cached VALUE for KEY."
  (puthash key value iota-segment--cache))

(defun iota-segment-cache-clear (&optional key)
  "Clear cache for KEY, or all if KEY is nil."
  (if key
      (remhash key iota-segment--cache)
    (clrhash iota-segment--cache)))

(defun iota-segment-cache-invalidate-pattern (pattern)
  "Invalidate all cache keys matching PATTERN (regex)."
  (maphash (lambda (key _value)
             (when (string-match-p pattern (format "%s" key))
               (remhash key iota-segment--cache)))
           iota-segment--cache))

;;; Segment Rendering

(defun iota-segment-eval (segment-or-fn)
  "Evaluate SEGMENT-OR-FN to get text.
If it's a function, call it. If it's a string, return it."
  (cond
   ((functionp segment-or-fn) (funcall segment-or-fn))
   ((stringp segment-or-fn) segment-or-fn)
   (t (format "%s" segment-or-fn))))

(defun iota-segment-render (segment)
  "Render SEGMENT to a propertized string."
  (when (and segment
             (or (null (iota-segment-visible-fn segment))
                 (funcall (iota-segment-visible-fn segment))))
    (let* ((cache-key (iota-segment-cache-key segment))
           (cached (and cache-key (iota-segment-cache-get cache-key))))
      (or cached
          (let* ((text (iota-segment-eval (iota-segment-text segment)))
                 (face (let ((f (iota-segment-face segment)))
                         (if (functionp f) (funcall f) f)))
                 (help-echo (iota-segment-help-echo segment))
                 (keymap (iota-segment-keymap segment))
                 (result text))
            ;; Apply face
            (when face
              (setq result (propertize result 'face face)))
            ;; Apply help-echo
            (when help-echo
              (setq result (propertize result 'help-echo
                                       (if (functionp help-echo)
                                           (funcall help-echo)
                                         help-echo))))
            ;; Apply keymap
            (when keymap
              (setq result (propertize result 'keymap keymap)))
            ;; Cache if key provided
            (when cache-key
              (iota-segment-cache-set cache-key result))
            result)))))

;;; Segment Composition

(defun iota-segment-compose (segments &optional separator)
  "Compose SEGMENTS into single string with optional SEPARATOR.
SEGMENTS is a list of segment structs or strings.
SEPARATOR defaults to a single space."
  (let ((separator (or separator " "))
        (rendered (delq nil (mapcar #'iota-segment-render segments))))
    (mapconcat #'identity rendered separator)))

(defun iota-segment-group (segments alignment width)
  "Group SEGMENTS by ALIGNMENT and fit within WIDTH.
Returns formatted string for the group."
  (let* ((group-segments (cl-remove-if-not
                          (lambda (seg)
                            (eq (iota-segment-align seg) alignment))
                          segments))
         (rendered (mapconcat #'iota-segment-render group-segments " ")))
    (pcase alignment
      ('center (iota-segment--center-align rendered width))
      ('right (iota-segment--right-align rendered width))
      (_ rendered)))) ; left alignment is default

(defun iota-segment--center-align (text width)
  "Center-align TEXT within WIDTH."
  (let* ((text-len (length text))
         (padding (max 0 (- width text-len)))
         (left-pad (/ padding 2))
         (right-pad (- padding left-pad)))
    (concat (make-string left-pad ?\s)
            text
            (make-string right-pad ?\s))))

(defun iota-segment--right-align (text width)
  "Right-align TEXT within WIDTH."
  (let* ((text-len (length text))
         (padding (max 0 (- width text-len))))
    (concat (make-string padding ?\s) text)))

;;; Segment Layout

(defun iota-segment-layout (segments width)
  "Layout SEGMENTS within WIDTH using left/center/right alignment.
Returns formatted string."
  (let* ((left-segs (cl-remove-if-not
                     (lambda (s) (eq (iota-segment-align s) 'left))
                     segments))
         (center-segs (cl-remove-if-not
                       (lambda (s) (eq (iota-segment-align s) 'center))
                       segments))
         (right-segs (cl-remove-if-not
                      (lambda (s) (eq (iota-segment-align s) 'right))
                      segments))
         (left-text (iota-segment-compose left-segs))
         (center-text (iota-segment-compose center-segs))
         (right-text (iota-segment-compose right-segs))
         (left-len (length left-text))
         (center-len (length center-text))
         (right-len (length right-text))
         (used (+ left-len center-len right-len))
         (remaining (max 0 (- width used)))
         (spacing (/ remaining 2))
         (left-spacing (if (> center-len 0) spacing 0))
         (right-spacing (if (> center-len 0)
                            (- remaining left-spacing)
                          remaining)))
    (concat left-text
            (make-string left-spacing ?\s)
            center-text
            (make-string right-spacing ?\s)
            right-text)))

;;; Segment Registry

(defvar iota-segment-registry (make-hash-table :test 'eq)
  "Registry of defined segments.")

(defun iota-segment-register (id segment)
  "Register SEGMENT with ID."
  (puthash id segment iota-segment-registry))

(defun iota-segment-unregister (id)
  "Unregister segment with ID."
  (remhash id iota-segment-registry))

(defun iota-segment-get (id)
  "Get registered segment with ID."
  (gethash id iota-segment-registry))

(defun iota-segment-list ()
  "List all registered segment IDs."
  (hash-table-keys iota-segment-registry))

;;; Built-in Segment Helpers

(defun iota-segment-simple (text &optional face)
  "Create simple segment with TEXT and optional FACE."
  (iota-segment-create
   :text text
   :face face))

(defun iota-segment-dynamic (fn &optional face cache-key)
  "Create dynamic segment that evaluates FN on each render.
Optional FACE and CACHE-KEY for caching expensive operations."
  (iota-segment-create
   :text fn
   :face face
   :cache-key cache-key))

(defun iota-segment-conditional (condition text &optional face)
  "Create segment that only shows when CONDITION is true.
CONDITION can be a function or a boolean."
  (iota-segment-create
   :text text
   :face face
   :visible-fn (if (functionp condition)
                   condition
                 (lambda () condition))))

(defun iota-segment-clickable (text action &optional face help)
  "Create clickable segment.
ACTION is called on mouse-1 click."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] action)
    (define-key map [header-line mouse-1] action)
    (iota-segment-create
     :text text
     :face face
     :keymap map
     :help-echo help
     :face (or face '(:foreground "#39bae6" :weight bold)))))

;;; Segment Separators

(defun iota-segment-separator (char &optional face)
  "Create separator segment with CHAR.
Useful for visual division between segments."
  (iota-segment-create
   :text (or char "│")
   :face (or face '(:inherit shadow))
   :priority 0))

(defun iota-segment-spacer (width)
  "Create spacer segment of WIDTH characters."
  (iota-segment-create
   :text (make-string width ?\s)
   :priority 0))

;;; Responsive Segment Fitting

(defun iota-segment-render-for-width (segment use-short)
  "Render SEGMENT, using short text if USE-SHORT is non-nil.
Returns the rendered string or nil if segment is not visible."
  (when (and segment
             (or (null (iota-segment-visible-fn segment))
                 (funcall (iota-segment-visible-fn segment))))
    (let* ((text-fn (if (and use-short (iota-segment-short-text segment))
                        (iota-segment-short-text segment)
                      (iota-segment-text segment)))
           (text (iota-segment-eval text-fn))
           (face (let ((f (iota-segment-face segment)))
                   (if (functionp f) (funcall f) f)))
           (help-echo (iota-segment-help-echo segment))
           (keymap (iota-segment-keymap segment))
           (result text))
      ;; Apply face
      (when face
        (setq result (propertize result 'face face)))
      ;; Apply help-echo
      (when help-echo
        (setq result (propertize result 'help-echo
                                 (if (functionp help-echo)
                                     (funcall help-echo)
                                   help-echo))))
      ;; Apply keymap
      (when keymap
        (setq result (propertize result 'keymap keymap)))
      result)))

(defun iota-segment-text-width (segment &optional use-short)
  "Calculate the display width of SEGMENT's text.
If USE-SHORT is non-nil and segment has short-text, use that instead.
Returns 0 if segment is invisible or has no text."
  (if (and segment
           (or (null (iota-segment-visible-fn segment))
               (funcall (iota-segment-visible-fn segment))))
      (let* ((text-fn (if (and use-short (iota-segment-short-text segment))
                          (iota-segment-short-text segment)
                        (iota-segment-text segment)))
             (text (iota-segment-eval text-fn)))
        (if (stringp text)
            (string-width text)
          0))
    0))

(defun iota-segment-fit-to-width (segments available-width separator-width)
  "Fit SEGMENTS into AVAILABLE-WIDTH, collapsing/removing as needed.
SEPARATOR-WIDTH is the width of the separator between segments.

Returns a list of (segment . use-short) cons cells for segments that fit.
Segments are removed by priority (lowest first) until they fit.
Before removing a segment, we try using its short-text version."
  (let* ((sorted-segments (sort (copy-sequence segments)
                                (lambda (a b)
                                  (> (iota-segment-priority a)
                                     (iota-segment-priority b)))))
         ;; Start with all segments, no collapsing
         (result (mapcar (lambda (s) (cons s nil)) sorted-segments))
         (total-width (iota-segment--calculate-total-width result separator-width)))
    
    ;; If it fits, we're done
    (when (> total-width available-width)
      ;; Try progressive fitting strategies:
      ;; 1. First, try collapsing segments (use short-text) from lowest priority
      (let ((to-collapse (reverse result))) ; lowest priority first
        (while (and to-collapse (> total-width available-width))
          (let* ((entry (car to-collapse))
                 (seg (car entry)))
            (when (iota-segment-short-text seg)
              ;; Mark this segment to use short text
              (setcdr entry t)
              (setq total-width (iota-segment--calculate-total-width result separator-width))))
          (setq to-collapse (cdr to-collapse))))
      
      ;; 2. If still doesn't fit, remove segments from lowest priority
      (while (and result (> total-width available-width))
        ;; Remove the last one (lowest priority since list is sorted by priority desc)
        (setq result (butlast result))
        (setq total-width (iota-segment--calculate-total-width result separator-width))))
    
    result))

(defun iota-segment--calculate-total-width (segment-entries separator-width)
  "Calculate total width for SEGMENT-ENTRIES with SEPARATOR-WIDTH.
SEGMENT-ENTRIES is a list of (segment . use-short) cons cells."
  (if (null segment-entries)
      0
    (let ((widths (mapcar (lambda (entry)
                            (or (iota-segment-text-width (car entry) (cdr entry)) 0))
                          segment-entries)))
      ;; Filter out zero-width segments for separator calculation
      (let ((non-zero-count (cl-count-if (lambda (w) (> w 0)) widths)))
        (+ (apply #'+ widths)
           (* (or separator-width 0) (max 0 (1- non-zero-count))))))))

(defun iota-segment-render-fitted (segment-entries)
  "Render SEGMENT-ENTRIES which are (segment . use-short) cons cells.
Returns list of rendered strings."
  (delq nil
        (mapcar (lambda (entry)
                  (iota-segment-render-for-width (car entry) (cdr entry)))
                segment-entries)))

;;; Update Triggers

(defvar iota-segment-update-hooks
  '((always . nil)
    (point . (post-command-hook))
    (timer . nil)
    (manual . nil))
  "Mapping of update-on triggers to hooks.")

(defun iota-segment-should-update-p (segment)
  "Return t if SEGMENT should update now."
  (let ((trigger (iota-segment-update-on segment)))
    (pcase trigger
      ('always t)
      ('manual nil)
      ('point t) ; Updated on post-command-hook
      ('timer t) ; Updated on timer
      (_ t))))

;;; Debouncing

(defvar iota-segment--debounce-timers (make-hash-table :test 'eq)
  "Timers for debounced segment updates.")

(defun iota-segment-debounce (id fn delay)
  "Debounce FN for segment ID with DELAY seconds.
Cancels previous timer if exists."
  (let ((timer (gethash id iota-segment--debounce-timers)))
    (when timer
      (cancel-timer timer))
    (puthash id
             (run-with-timer delay nil
                             (lambda ()
                               (funcall fn)
                               (remhash id iota-segment--debounce-timers)))
             iota-segment--debounce-timers)))

(provide 'iota-segment)
;;; iota-segment.el ends here
