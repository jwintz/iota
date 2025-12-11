;;; iota-modeline.el --- I O T Λ modeline wrapper -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: modeline, faces
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; IOTA modeline is a WRAPPER that adds TUI-style box decorations
;; around the user's existing modeline. It does NOT replace modeline
;; content - that is left to the user's preferred modeline package
;; (doom-modeline, mood-line, vanilla Emacs, etc.).
;;
;; Features:
;;   - Wraps existing mode-line-format with box decorations
;;   - Terminal overlay support for header-line position
;;   - Automatic separator lines between windows
;;   - Theme-aware face integration
;;   - Centralized update system integration
;;   - T-junction separators between segments
;;   - Right-aligned segments support (configurable count)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'iota-box)
(require 'iota-theme)
(require 'iota-update)
(require 'iota-utils)
(require 'iota-popup)
(require 'iota-separator)

;;; Faces

(defface iota-modeline-default
  '((t :inherit mode-line
       :foreground unspecified
       :background unspecified
       :slant normal
       :weight normal
       :underline nil
       :overline nil
       :strike-through nil
       :inverse-video nil
       :stipple nil
       :extend nil))
  "Default face for modeline content without a specific face.
This face explicitly sets attributes to prevent inheritance from buffer text.
The unspecified foreground/background allows mode-line colors to show through."
  :group 'iota-modeline)

;;; Customization

(defgroup iota-modeline nil
  "I O T Λ modeline configuration."
  :group 'iota
  :prefix "iota-modeline-")

(defcustom iota-modeline-position 'header
  "Position of I O T Λ modeline.
Can be: header (header-line), mode (mode-line), or both."
  :type '(choice (const :tag "Header line" header)
                 (const :tag "Mode line" mode)
                 (const :tag "Both" both))
  :group 'iota-modeline)

(defcustom iota-modeline-box-style 'rounded
  "Box drawing style for modeline.
Can be: single, double, rounded, heavy, ascii."
  :type '(choice (const :tag "Single line" single)
                 (const :tag "Double line" double)
                 (const :tag "Rounded corners" rounded)
                 (const :tag "Heavy line" heavy)
                 (const :tag "ASCII (compatibility)" ascii))
  :group 'iota-modeline)

(defcustom iota-modeline-height 1
  "Height of modeline in lines.
Currently only 1 is supported."
  :type 'integer
  :group 'iota-modeline)

(defcustom iota-modeline-show-in-inactive nil
  "Show I O T Λ modeline in inactive windows.
If nil, inactive windows use default modeline."
  :type 'boolean
  :group 'iota-modeline)

(defcustom iota-modeline-excluded-buffers '("\\*Warnings\\*" "\\*Backtrace\\*")
  "List of buffer names (strings or regexps) to exclude from IOTA modeline."
  :type '(repeat (choice string regexp))
  :group 'iota-modeline)

(defcustom iota-modeline-show-separators t
  "Show vertical separators between modeline segments.
When non-nil, segments are separated by │ characters with
T-junctions (┬/┴) in the top and bottom borders."
  :type 'boolean
  :group 'iota-modeline)

(defcustom iota-modeline-right-segment-count 2
  "Number of segments to place on the right side of the modeline.
Segments are identified by splitting mode-line content on runs of 2+ spaces.
Default is 2 (typically major-mode and VCS info). When keycast is active,
keycast segments are automatically added to the right count."
  :type 'integer
  :group 'iota-modeline)

(defcustom iota-modeline-merge-keycast t
  "Whether to merge keycast key and command segments.
When non-nil, segments with keycast faces are merged with the following
segment to keep key and command together."
  :type 'boolean
  :group 'iota-modeline)

;;; State

(defvar iota-modeline--original-mode-line-format nil
  "Saved original `mode-line-format' before IOTA wrapping.")

(defvar iota-modeline--original-header-line-format nil
  "Original header-line-format before IOTA.")

(defvar iota-modeline--current-window nil
  "The window currently being rendered.
This is dynamically bound during modeline rendering to allow
content to check if it's rendering for the selected window.")

(defvar iota-modeline--selected-window nil
  "The truly selected window at the time of rendering.
This is captured BEFORE any with-selected-window context switches,
allowing content to determine if it's in the active window.")

(defvar iota-modeline--saved-mode-line-faces nil
  "Saved mode-line face attributes for restoration.")

(defvar iota-modeline--frame-focus-state t
  "Whether the current frame has focus.
Updated by focus hooks to prevent rendering issues when terminal is unfocused.")

;;; Wrapping Logic

(defcustom iota-modeline-debug nil
  "Enable debug logging for modeline rendering."
  :type 'boolean
  :group 'iota-modeline)

(defcustom iota-modeline-debug-faces nil
  "Enable debug logging for face normalization (very verbose)."
  :type 'boolean
  :group 'iota-modeline)

(defun iota-modeline--resolve-face-color (face attr)
  "Resolve color attribute ATTR (:foreground or :background) from FACE.
FACE can be a symbol, a list of faces, or a nested plist. Returns the
first non-unspecified color found, or nil."
  (cond
   ;; Single face symbol - get attribute directly
   ((facep face)
    (let ((val (face-attribute face attr nil t)))
      (if (eq val 'unspecified) nil val)))

   ;; List of faces - try each in order
   ((and (listp face) (not (keywordp (car face))))
    (cl-some (lambda (f) (iota-modeline--resolve-face-color f attr)) face))

   ;; Plist with :inherit - resolve from inherited face
   ((and (listp face) (plist-member face :inherit))
    (let ((inherited (plist-get face :inherit)))
      (iota-modeline--resolve-face-color inherited attr)))

   ;; Otherwise
   (t nil)))

(defun iota-modeline--normalize-face (face)
  "Normalize FACE to prevent any attribute inheritance from buffer text.
Returns a face specification with explicit values for all inheritable attributes.
Critically, this ensures that foreground/background colors are resolved from
the face definition and set explicitly to prevent color inheritance."
  (when iota-modeline-debug-faces
    (message "IOTA: Normalizing face: %S" face))
  (let ((result
         (cond
          ;; List of faces - process each recursively
          ((and (listp face) (not (keywordp (car face))))
           (mapcar #'iota-modeline--normalize-face face))

          ;; Already a plist with explicit attributes - check if it's complete
          ((and (listp face) (keywordp (car face)))
           (let ((normalized (copy-sequence face)))
             ;; Ensure critical attributes that can inherit are explicitly set
             (unless (plist-member normalized :slant)
               (setq normalized (append normalized (list :slant 'normal))))
             (unless (plist-member normalized :weight)
               (setq normalized (append normalized (list :weight 'normal))))
             (unless (plist-member normalized :underline)
               (setq normalized (append normalized (list :underline nil))))
             (unless (plist-member normalized :inverse-video)
               (setq normalized (append normalized (list :inverse-video nil))))
             ;; Don't resolve foreground/background from inheritance
             ;; If the plist has :inherit, let Emacs handle color inheritance naturally
             ;; Only add explicit colors if they're already in the plist
             normalized))

          ;; Single face symbol - just set it up for inheritance
          ((facep face)
           (let* ((weight (face-attribute face :weight nil t))
                  (slant (face-attribute face :slant nil t))
                  (underline (face-attribute face :underline nil t))
                  (base-spec (list :inherit face)))
             (when iota-modeline-debug-faces
               (message "IOTA:   Face %S, preserving inheritance" face))
             ;; Only set attributes explicitly if they are unspecified
             ;; This preserves user customizations while preventing inheritance
             (when (eq slant 'unspecified)
               (setq base-spec (append base-spec (list :slant 'normal))))
             (when (eq weight 'unspecified)
               (setq base-spec (append base-spec (list :weight 'normal))))
             (when (eq underline 'unspecified)
               (setq base-spec (append base-spec (list :underline nil))))
             ;; Always prevent inverse-video inheritance
             (setq base-spec (append base-spec (list :inverse-video nil)))
             ;; Don't add explicit foreground/background - let :inherit handle colors
             base-spec))

          ;; Fallback for anything else - return default with explicit foreground
          (t
           (let ((default-fg (face-attribute 'mode-line :foreground nil t)))
             (if (and default-fg (not (eq default-fg 'unspecified)))
                 (list :inherit 'iota-modeline-default
                       :slant 'normal
                       :weight 'normal
                       :underline nil
                       :inverse-video nil
                       :foreground default-fg)
               'iota-modeline-default))))))
    (when iota-modeline-debug-faces
      (message "IOTA:   Result: %S" result))
    result))

(defun iota-modeline--dim-face-for-inactive (face)
  "Modify FACE to use inactive/dimmed appearance.
Returns a face specification that inherits from mode-line-inactive."
  (cond
   ;; List of faces - wrap each one
   ((and (listp face) (not (keywordp (car face))))
    (mapcar #'iota-modeline--dim-face-for-inactive face))
   ;; Plist with attributes - add mode-line-inactive inheritance
   ((and (listp face) (keywordp (car face)))
    (let ((dimmed (copy-sequence face)))
      ;; If it has :foreground, blend it towards mode-line-inactive foreground
      (when (plist-member dimmed :foreground)
        (let* ((fg (plist-get dimmed :foreground))
               (inactive-fg (face-attribute 'mode-line-inactive :foreground nil t)))
          (when (and fg inactive-fg
                     (not (eq fg 'unspecified))
                     (not (eq inactive-fg 'unspecified)))
            ;; Blend original color with inactive foreground
            (condition-case nil
                (let ((blended (iota-theme-color-blend fg inactive-fg 0.6)))
                  (setq dimmed (plist-put dimmed :foreground blended)))
              (error nil)))))
      dimmed))
   ;; Single face symbol - create plist that dims it
   ((facep face)
    (let* ((fg (face-attribute face :foreground nil t))
           (inactive-fg (face-attribute 'mode-line-inactive :foreground nil t)))
      (if (and fg inactive-fg
               (not (eq fg 'unspecified))
               (not (eq inactive-fg 'unspecified)))
          (condition-case nil
              (let ((blended (iota-theme-color-blend fg inactive-fg 0.6)))
                (list :inherit face :foreground blended))
            (error (list :inherit 'mode-line-inactive)))
        (list :inherit 'mode-line-inactive))))
   ;; Fallback
   (t 'mode-line-inactive)))

(defun iota-modeline--ensure-segment-face (segment &optional inactive)
  "Ensure SEGMENT has explicit face properties to prevent inheritance.
Applies face normalization to prevent any attribute inheritance from buffer text.
If INACTIVE is non-nil, dim faces for inactive window appearance."
  (if (or (null segment) (string-empty-p segment))
      segment
    (when iota-modeline-debug-faces
      (message "IOTA: Processing segment: %S" (substring-no-properties segment)))
    (let ((result (copy-sequence segment))
          (len (length segment))
          (pos 0))
      (while (< pos len)
        (let* ((next-change (or (next-single-property-change pos 'face result) len))
               (current-face (get-text-property pos 'face result))
               (segment-text (substring-no-properties result pos next-change)))
          (when iota-modeline-debug-faces
            (message "IOTA:   Text '%s' has face: %S" segment-text current-face))
          (if current-face
              ;; Normalize the face to prevent inheritance
              (let ((normalized (iota-modeline--normalize-face current-face)))
                ;; If inactive, dim the face
                (when inactive
                  (setq normalized (iota-modeline--dim-face-for-inactive normalized)))
                (put-text-property pos next-change 'face normalized result))
            ;; No face - apply our default face (or inactive face)
            (progn
              (when iota-modeline-debug-faces
                (message "IOTA:   No face found, applying default"))
              (put-text-property pos next-change 'face
                               (if inactive 'mode-line-inactive 'iota-modeline-default)
                               result)))
          (setq pos next-change)))
      result)))

(defun iota-modeline--parse-segments-preserve-faces (content &optional inactive)
  "Parse CONTENT string into segments, preserving face properties.
Splits on runs of 2+ spaces to identify logical segments.
Merges segments that match `iota-modeline-merge-patterns' with their successor.
Ensures each segment has explicit face properties to prevent inheritance.
If INACTIVE is non-nil, dim faces for inactive window appearance."
  (let* ((result nil)
         (pending nil)
         (start 0)
         (len (length content)))
    ;; Find segments by looking for runs of 2+ spaces
    (while (< start len)
      ;; Skip leading spaces
      (while (and (< start len) (eq (aref content start) ?\s))
        (setq start (1+ start)))
      (when (< start len)
        ;; Find end of segment (next run of 2+ spaces or end)
        (let ((end start)
              (space-run 0))
          (while (and (< end len) (< space-run 2))
            (if (eq (aref content end) ?\s)
                (setq space-run (1+ space-run))
              (setq space-run 0))
            (setq end (1+ end)))
          ;; Adjust end to exclude trailing spaces
          (when (>= space-run 2)
            (setq end (- end space-run)))
          ;; Extract segment with properties preserved
          (let ((seg (string-trim (substring content start end))))
            (when (> (length seg) 0)
              ;; Ensure segment has explicit face properties (pass inactive flag)
              (setq seg (iota-modeline--ensure-segment-face seg inactive))
              (if pending
                  ;; Merge with pending segment (keycast key + command)
                  (progn
                    (push (concat pending " " seg) result)
                    (setq pending nil))
                ;; Check if this segment has keycast face (should merge with next)
                (if (and iota-modeline-merge-keycast
                         (iota-modeline--segment-has-keycast-face-p seg))
                    (setq pending seg)
                  (push seg result)))))
          (setq start end))))
    ;; Handle trailing pending segment
    (when pending
      (push pending result))
    (nreverse result)))

(defun iota-modeline--parse-segments (content &optional inactive)
  "Parse CONTENT string into a list of segments.
Splits on runs of 2+ spaces to identify logical segments.
Merges segments that match `iota-modeline-merge-patterns' with their successor.
If INACTIVE is non-nil, dim faces for inactive window appearance."
  (iota-modeline--parse-segments-preserve-faces content inactive))

(defun iota-modeline--is-keycast-segment-p (segment)
  "Return t if SEGMENT appears to be a keycast segment.
Detects segments containing key-like patterns (C-x, M-x, etc.) followed by commands."
  (let ((plain (substring-no-properties segment)))
    (or
     ;; Matches patterns like "C-x b switch-to-buffer" or "M-x" or "S-<return>"
     ;; Key bindings: C-x, M-x, C-M-x, A-x, S-x, s-x (super), H-x (hyper)
     (string-match-p "^\\([CMASsH]-\\|<[^>]+>\\)" plain)
     ;; Matches common command names that appear in keycast
     ;; These are functions that are typically shown by keycast
     (string-match-p "\\b\\(self-insert-command\\|next-line\\|previous-line\\|forward-char\\|backward-char\\|delete-char\\|delete-backward-char\\|newline\\|keyboard-quit\\|save-buffer\\|find-file\\|switch-to-buffer\\)\\b" plain)
     ;; Matches segments with keycast faces
     (catch 'found
       (dotimes (i (length segment))
         (let ((face (get-text-property i 'face segment)))
           (when (iota-modeline--face-is-keycast-p face)
             (throw 'found t))))
       nil))))

(defun iota-modeline--face-is-keycast-p (face)
  "Return t if FACE is or inherits from a keycast face."
  (cond
   ((null face) nil)
   ((eq face 'keycast-key) t)
   ((eq face 'keycast-command) t)
   ((symbolp face)
    ;; Check if face inherits from keycast faces
    (let ((inherit (ignore-errors (face-attribute face :inherit nil t))))
      (and inherit (not (eq inherit 'unspecified))
           (iota-modeline--face-is-keycast-p inherit))))
   ((and (listp face) (not (keywordp (car face))))
    ;; List of faces - check each
    (cl-some #'iota-modeline--face-is-keycast-p face))
   ((and (listp face) (keywordp (car face)))
    ;; Plist - check :inherit
    (let ((inherit (plist-get face :inherit)))
      (and inherit (iota-modeline--face-is-keycast-p inherit))))
   (t nil)))

(defun iota-modeline--segment-has-keycast-face-p (segment)
  "Return t if SEGMENT contains any text with keycast face.
Used to detect keycast key segments that should be merged with command."
  (catch 'found
    (dotimes (i (length segment))
      (let ((face (get-text-property i 'face segment)))
        (when (iota-modeline--face-is-keycast-p face)
          (throw 'found t))))
    nil))

(defun iota-modeline--filter-keycast-segments (segments)
  "Filter out keycast-related segments from SEGMENTS list.
Returns segments without keycast information for inactive windows."
  (cl-remove-if #'iota-modeline--is-keycast-segment-p segments))

(defun iota-modeline--fit-segments-to-width (segments width style)
  "Fit SEGMENTS list into WIDTH, truncating as needed.
STYLE is the box style for separator width calculation.
Returns a list of segments that will fit."
  (let* ((sep-width 3)  ; " │ " = 3 chars
         (border-width 4)  ; "│ " + " │" = 4 chars
         (available (- width border-width))
         (result nil)
         (used 0))
    ;; Add segments until we run out of space
    (dolist (seg segments)
      (let* ((seg-width (string-width seg))
             (need-sep (if result sep-width 0))
             (total-need (+ seg-width need-sep)))
        (cond
         ;; Segment fits completely
         ((<= (+ used total-need) available)
          (push seg result)
          (setq used (+ used total-need)))
         ;; First segment and it's too long - truncate it
         ((null result)
          (let* ((trunc-width (- available 1))  ; Leave room for ellipsis
                 (truncated (truncate-string-to-width seg trunc-width nil nil "…")))
            (push truncated result)
            (setq used available)))
         ;; No more room - stop
         (t nil))))
    (nreverse result)))

(defun iota-modeline--calculate-segments-width (segments)
  "Calculate total width needed for SEGMENTS including separators."
  (let ((sep-width 3)  ; " │ " = 3 chars
        (total 0)
        (first t))
    (dolist (seg segments)
      (unless first
        (setq total (+ total sep-width)))
      (setq total (+ total (string-width seg)))
      (setq first nil))
    total))

(defun iota-modeline--fit-segments-to-box (left-segments right-segments width style)
  "Fit LEFT-SEGMENTS and RIGHT-SEGMENTS into box of WIDTH.
STYLE is the box style.
Returns a cons cell (left . right) with fitted segments.
Drops low-priority segments (like keycast) if content doesn't fit.
Keycast segments are identified and dropped first when space is tight."
  (ignore style)  ; Style is for API consistency but not used here
  (let* ((border-width 4)  ; "│ " + " │" = 4 chars
         (min-gap 1)       ; Minimum gap between left and right
         (available (- width border-width))
         (left-width (iota-modeline--calculate-segments-width left-segments))
         (right-width (iota-modeline--calculate-segments-width right-segments))
         (total-needed (+ left-width
                          (if (and (> left-width 0) (> right-width 0)) min-gap 0)
                          right-width)))

    ;; If everything fits, return as-is
    (if (<= total-needed available)
        (cons left-segments right-segments)

      ;; Content doesn't fit - need to drop/truncate segments
      ;; Priority order for dropping (lowest priority first):
      ;; 1. Keycast segments from right side
      ;; 2. Other segments from right side (from end)
      ;; 3. Truncate remaining segments

      (let ((new-right right-segments)
            (new-left left-segments))

        ;; First, try dropping keycast segments from right
        (when (> total-needed available)
          (setq new-right (cl-remove-if #'iota-modeline--is-keycast-segment-p new-right))
          (setq right-width (iota-modeline--calculate-segments-width new-right))
          (setq total-needed (+ left-width
                                (if (and (> left-width 0) (> right-width 0)) min-gap 0)
                                right-width)))

        ;; Then try dropping keycast segments from left
        (when (> total-needed available)
          (setq new-left (cl-remove-if #'iota-modeline--is-keycast-segment-p new-left))
          (setq left-width (iota-modeline--calculate-segments-width new-left))
          (setq total-needed (+ left-width
                                (if (and (> left-width 0) (> right-width 0)) min-gap 0)
                                right-width)))

        ;; If still doesn't fit, drop right segments one by one (from end)
        (while (and (> total-needed available) (> (length new-right) 1))
          (setq new-right (butlast new-right))
          (setq right-width (iota-modeline--calculate-segments-width new-right))
          (setq total-needed (+ left-width
                                (if (and (> left-width 0) (> right-width 0)) min-gap 0)
                                right-width)))

        ;; If still doesn't fit, drop left segments one by one (from end)
        (while (and (> total-needed available) (> (length new-left) 1))
          (setq new-left (butlast new-left))
          (setq left-width (iota-modeline--calculate-segments-width new-left))
          (setq total-needed (+ left-width
                                (if (and (> left-width 0) (> right-width 0)) min-gap 0)
                                right-width)))

        ;; If still doesn't fit, truncate remaining segments
        (when (> total-needed available)
          (let* ((left-available (max 10 (- available right-width min-gap)))
                 (truncated-left (iota-modeline--fit-segments-to-width new-left left-available style)))
            (setq new-left truncated-left)))

        (cons new-left new-right)))))

(defun iota-modeline--render-box (&optional window truly-selected-window)
  "Render complete IOTA modeline box for WINDOW.
Uses `iota-box-render-single-line' for proper T-junction handling.
Parses mode-line content into segments and distributes them between
left and right sides based on `iota-modeline-right-segment-count'.
Preserves face properties from the original modeline.
TRULY-SELECTED-WINDOW is the actual selected window for active/inactive detection."
  (let* ((win (or window (selected-window)))
         (truly-selected (or truly-selected-window (selected-window)))
         (width (iota-modeline-effective-width win))
         (style iota-modeline-box-style)
         (face (iota-theme-get-box-face win))
         ;; Render the original modeline content (preserve properties!)
         ;; CRITICAL: doom-modeline--active checks:
         ;;   (eq (doom-modeline--selected-window) doom-modeline-current-window)
         ;; where doom-modeline--selected-window calls (frame-selected-window)
         ;;
         ;; We need to make both values match for the active window:
         ;; - Set doom-modeline-current-window to the window we're rendering
         ;; - Override doom-modeline--selected-window to return the same value
         (content (let* ((is-active (eq win truly-selected))
                         ;; Save original doom-modeline state
                         (saved-current-window (and (boundp 'doom-modeline-current-window)
                                                    doom-modeline-current-window))
                         ;; For keycast: save predicate
                         (saved-keycast-predicate (and (boundp 'keycast-mode-line-window-predicate)
                                                       keycast-mode-line-window-predicate)))
                    ;; Set doom-modeline-current-window appropriately
                    ;; For active: set to win so doom-modeline--active returns t
                    ;; For inactive: set to nil so doom-modeline--active returns nil
                    (when (boundp 'doom-modeline-current-window)
                      (setq doom-modeline-current-window (if is-active win nil)))
                    ;; Override keycast predicate
                    (when (boundp 'keycast-mode-line-window-predicate)
                      (setq keycast-mode-line-window-predicate
                            (if is-active
                                (lambda () t)    ; Active: always show keycast
                              (lambda () nil)))) ; Inactive: never show keycast
                    (unwind-protect
                        ;; Use cl-letf to temporarily override doom-modeline--selected-window
                        ;; This ensures doom-modeline--active returns correct value
                        (if (and is-active (fboundp 'doom-modeline--selected-window))
                            (cl-letf (((symbol-function 'doom-modeline--selected-window)
                                       (lambda () win)))
                              (format-mode-line iota-modeline--original-mode-line-format nil win))
                          ;; For inactive windows, let doom-modeline think it's inactive
                          (format-mode-line iota-modeline--original-mode-line-format nil win))
                      ;; Restore original state
                      (when (boundp 'doom-modeline-current-window)
                        (setq doom-modeline-current-window saved-current-window))
                      (when (boundp 'keycast-mode-line-window-predicate)
                        (setq keycast-mode-line-window-predicate saved-keycast-predicate)))))
         (content (string-trim content))
         ;; Debug: check content for active vs inactive
         (_ (when nil  ; Set to t to enable debug
              (message "IOTA: Window %s (active=%s)" win is-active)
              (message "IOTA:   Content: %S" content)
              (message "IOTA:   Length: %d" (length content))))
         ;; Determine if this is an inactive window
         (is-inactive (not (eq win truly-selected)))
         ;; Parse into segments (preserves faces, dims for inactive windows)
         (all-segments (let ((segs (if iota-modeline-show-separators
                                       (iota-modeline--parse-segments content is-inactive)
                                     (list (iota-modeline--ensure-segment-face content is-inactive)))))
                         segs))
         ;; Filter out keycast segments from inactive windows
         (filtered-segments (if (and (not (eq win truly-selected))
                                     (or (bound-and-true-p keycast-mode)
                                         (bound-and-true-p keycast-mode-line-mode)))
                                (iota-modeline--filter-keycast-segments all-segments)
                              all-segments))
         ;; Adjust right-count based on how many segments were filtered
         (filtered-count (- (length all-segments) (length filtered-segments)))
         (all-segments filtered-segments)
         (_ (when nil  ; Set to t for debug
              (message "IOTA: Segments total=%d filtered=%d is-active=%s"
                       (length all-segments) filtered-count (eq win truly-selected))
              (message "IOTA:   Segments: %S" (mapcar #'substring-no-properties all-segments))))
         ;; Split into left and right segments
         ;; Left always gets at least 2 segments (buffer + position) when available
         ;; Right gets keycast + mode + vc, preserving doom-modeline order
         (total-segments (length all-segments))
         ;; Count keycast segments to adjust right-count
         (keycast-count (cl-count-if #'iota-modeline--is-keycast-segment-p all-segments))
         ;; Base right count + keycast count ensures keycast ends up on right
         (effective-right-count (+ iota-modeline-right-segment-count keycast-count))
         ;; Ensure left gets at least 2 segments (buffer + position) when available
         (min-left-count (min 2 total-segments))
         (max-right-count (max 0 (- total-segments min-left-count)))
         (right-count (min effective-right-count max-right-count))
         (left-count (- total-segments right-count))
         (left-segments (seq-take all-segments left-count))
         (right-segments (seq-drop all-segments left-count))
         ;; Fit segments to available width, dropping low-priority segments if needed
         (fitted (iota-modeline--fit-segments-to-box left-segments right-segments width style))
         (left-segments (car fitted))
         (right-segments (cdr fitted)))
    ;; Use iota-box-render-single-line with left and right
    (iota-box-render-single-line
     :left left-segments
     :center nil
     :right right-segments
     :width width
     :style style
     :face face
     :compact nil)))

(defun iota-modeline--render (&optional override-box-face window truly-selected-window)
  "Render IOTA modeline format string for WINDOW.
If WINDOW is nil, use selected window.
OVERRIDE-BOX-FACE can override the box face.
TRULY-SELECTED-WINDOW is the actual selected window for active/inactive detection.
Returns complete box: top border + content + bottom border."
  (let ((target-window (or window (selected-window))))
    ;; Check if user has hidden modeline in this window
    (if (window-parameter target-window 'iota-modeline-hidden)
        "" ; Return empty string if hidden
      (if (not (iota-modeline--should-show-p))
          "" ; Return empty string for minibuffer
        (let* ((iota-modeline--current-window target-window)
               (selected-win-value (or truly-selected-window
                                      (if (and (boundp 'iota-modeline--selected-window)
                                               iota-modeline--selected-window)
                                          iota-modeline--selected-window
                                        (selected-window))))
               (iota-modeline--selected-window selected-win-value))
          ;; Use new box rendering function
          (iota-modeline--render-box target-window selected-win-value))))))

(defun iota-modeline--top-border (&optional window)
  "Render top border for WINDOW."
  (if (not (iota-modeline--should-show-p))
      ""
    (let* ((win (or window (selected-window)))
           (width (iota-modeline-effective-width win))
           (style iota-modeline-box-style)
           (face (iota-theme-get-box-face win)))
      (iota-box-top-border width style face))))

(defun iota-modeline--bottom-border (&optional window)
  "Render bottom border for WINDOW."
  (if (not (iota-modeline--should-show-p))
      ""
    (let* ((win (or window (selected-window)))
           (width (iota-modeline-effective-width win))
           (style iota-modeline-box-style)
           (face (iota-theme-get-box-face win)))
      (iota-box-bottom-border width style face))))

(defun iota-modeline--format ()
  "Return modeline format for active window."
  '(:eval (iota-modeline--render)))

(defun iota-modeline--format-inactive ()
  "Return modeline format for inactive windows."
  (if iota-modeline-show-in-inactive
      (iota-modeline--format)
    ;; Use simple format for inactive
    '(" " mode-line-buffer-identification " "
      mode-line-position " "
      mode-name)))

;;; Overlay Management (Terminal Support)

(defvar iota-modeline--window-overlays (make-hash-table :weakness 'key)
  "Hash table mapping windows to their header-line overlays.")

(defvar iota-modeline--separator-overlays (make-hash-table :weakness 'key)
  "Hash table mapping windows to their separator-line overlays.")

(defun iota-modeline--ensure-overlay (window)
  "Ensure the simulated header overlay exists for WINDOW.
Returns nil if window or buffer is invalid."
  (when (and (window-live-p window)
             (window-buffer window)
             (buffer-live-p (window-buffer window)))
    (let ((overlay (gethash window iota-modeline--window-overlays))
          (current-buf (window-buffer window)))
      ;; Clean up invalid overlay or overlay in wrong buffer
      (when (and overlay (overlayp overlay))
        (let ((overlay-buf (overlay-buffer overlay)))
          (when (or (not overlay-buf)
                    (not (eq overlay-buf current-buf)))
            ;; Overlay is detached or in different buffer - delete and recreate
            (delete-overlay overlay)
            (remhash window iota-modeline--window-overlays)
            (setq overlay nil))))
      ;; Create new overlay if needed
      (unless overlay
        (with-current-buffer current-buf
          (setq overlay (make-overlay (point-min) (point-min) current-buf))
          (overlay-put overlay 'priority 100)
          (puthash window overlay iota-modeline--window-overlays)))
      overlay)))

(defun iota-modeline--apply-default-face (str)
  "Apply `iota-modeline-default' face to parts of STR that have no face.
This prevents inheritance from buffer text at overlay position.
Parts that already have a face get normalized to prevent attribute inheritance."
  (if (or (null str) (string-empty-p str))
      str
    (let ((result (copy-sequence str))
          (len (length str))
          (pos 0))
      (while (< pos len)
        (let* ((next-change (or (next-single-property-change pos 'face result) len))
               (current-face (get-text-property pos 'face result)))
          (if current-face
              ;; Normalize face to prevent inheritance
              (put-text-property pos next-change 'face
                               (iota-modeline--normalize-face current-face) result)
            ;; No face - apply our default face
            (put-text-property pos next-change 'face 'iota-modeline-default result))
          (setq pos next-change)))
      result)))

(defun iota-modeline--update-overlay (&optional window selected-window)
  "Update the simulated header overlay for WINDOW.
If WINDOW is nil, use selected window.
If SELECTED-WINDOW is provided, use it as the truly selected window."
  (condition-case err
      (let ((win (or window (selected-window)))
            (truly-selected (or selected-window (selected-window))))
        (when (and (window-live-p win)
                   (window-buffer win)
                   (buffer-live-p (window-buffer win)))
          (with-current-buffer (window-buffer win)
            (when (eq iota-modeline-position 'header)
              (kill-local-variable 'header-line-format)
              (let ((overlay (iota-modeline--ensure-overlay win))
                    (start (window-start win)))
                (when (and overlay (overlayp overlay))
                  ;; Ensure start is valid for current buffer
                  (unless (and start (integer-or-marker-p start)
                               (<= start (point-max)))
                    (setq start (point-min)))
                  (condition-case render-err
                      (let* ((iota-modeline--selected-window truly-selected)
                             (box (with-selected-window win
                                    (iota-modeline--render nil win truly-selected)))
                             (box-str (if (and box (stringp box) (not (string= box "")))
                                          (iota-modeline--apply-default-face
                                           (concat box "\n"))
                                        "")))
                        (move-overlay overlay start start (current-buffer))
                        (overlay-put overlay 'before-string box-str)
                        (overlay-put overlay 'window win))
                    (error
                     (message "I O T Λ: Error in render for buffer %s: %s"
                              (buffer-name) (error-message-string render-err))))))))))
    (error
     (message "I O T Λ: Error in update-overlay for window %s: %s"
              (if (windowp window) (window-buffer window) "nil")
              (error-message-string err)))))

(defun iota-modeline--remove-overlay ()
  "Remove all simulated header overlays."
  (maphash (lambda (_window overlay)
             (when (overlayp overlay)
               (delete-overlay overlay)))
           iota-modeline--window-overlays)
  (clrhash iota-modeline--window-overlays))

(defun iota-modeline--ensure-separator-overlay (window)
  "Ensure the separator overlay exists for WINDOW.
Returns nil if window or buffer is invalid."
  (when (and (window-live-p window)
             (window-buffer window)
             (buffer-live-p (window-buffer window)))
    (let ((overlay (gethash window iota-modeline--separator-overlays)))
      (when (and overlay
                 (overlayp overlay)
                 (not (overlay-buffer overlay)))
        (remhash window iota-modeline--separator-overlays)
        (setq overlay nil))
      (unless overlay
        (with-current-buffer (window-buffer window)
          (setq overlay (make-overlay (point-max) (point-max) (current-buffer)))
          (overlay-put overlay 'priority 100)
          (overlay-put overlay 'window window)
          (puthash window overlay iota-modeline--separator-overlays)))
      overlay)))

(defun iota-modeline--update-separator-overlay (window)
  "Update the separator overlay for WINDOW."
  (when (and (window-live-p window)
             (window-buffer window)
             (buffer-live-p (window-buffer window)))
    (let ((overlay (iota-modeline--ensure-separator-overlay window)))
      (when overlay
        (with-current-buffer (window-buffer window)
          (move-overlay overlay (point-max) (point-max) (current-buffer))
          (if (and (iota-modeline--should-show-p)
                   (iota-modeline--window-is-at-bottom-p window))
              (overlay-put overlay 'after-string
                          (propertize "\n" 'display
                                     (list 'space :width (iota-modeline-effective-width window))))
            (overlay-put overlay 'after-string nil)))))))

(defun iota-modeline--remove-separator-overlays ()
  "Remove all separator overlays."
  (maphash (lambda (_window overlay)
             (when (overlayp overlay)
               (delete-overlay overlay)))
           iota-modeline--separator-overlays)
  (clrhash iota-modeline--separator-overlays))

;;; Update Logic

(defun iota-modeline--update ()
  "Update modeline display."
  ;; For GUI mode, force mode-line update
  (when (display-graphic-p)
    (force-mode-line-update t))
  ;; For terminal mode or overlay-based rendering, update overlays
  (when (or (not (display-graphic-p))
            (eq iota-modeline-position 'header))
    (let ((truly-selected (selected-window)))
      (dolist (win (window-list))
        (iota-modeline--update-overlay win truly-selected)))))

(defun iota-modeline--update-windows (windows)
  "Update modeline display for specific WINDOWS only."
  (let ((truly-selected (selected-window)))
    (dolist (win windows)
      (when (window-live-p win)
        (iota-modeline--update-overlay win truly-selected)))))

(defun iota-modeline--do-update ()
  "Perform modeline update (called by centralized update system)."
  (iota-safe-call "modeline-update"
    (iota-modeline--update)))

;;; Hooks

(defun iota-modeline--post-command ()
  "Post-command hook for modeline updates.
Uses centralized update system for efficient batching."
  ;; Request update via centralized system (debounced, not immediate)
  (when (featurep 'iota-update)
    (iota-update-request :modeline)))

(defun iota-modeline--should-show-p ()
  "Return t if IOTA modeline should be shown in current buffer."
  (let ((buf-name (buffer-name)))
    (not (or (minibufferp)
             ;; Use popup detection for which-key, transient, etc.
              (and (fboundp 'iota-popup--buffer-popup-p)
                   (iota-popup--buffer-popup-p (current-buffer)))
              (derived-mode-p 'iota-screens-buffer-mode)
              (string-match-p "\\*I O T Λ splash\\*" buf-name)
             (string-match-p "\\*I O T Λ screen-[0-9]+\\*" buf-name)
             (cl-find-if (lambda (pattern) (string-match-p pattern buf-name))
                         iota-modeline-excluded-buffers)))))

(defun iota-modeline--buffer-excluded-p (buffer)
  "Return non-nil if BUFFER is in the excluded buffers list."
  (when (buffer-live-p buffer)
    (let ((buf-name (buffer-name buffer)))
      (cl-find-if (lambda (pattern) (string-match-p pattern buf-name))
                  iota-modeline-excluded-buffers))))

(defun iota-modeline--ensure-format-in-buffer (buffer)
  "Ensure BUFFER has correct IOTA modeline format."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((iota-modeline--should-show-p)
        (when (local-variable-p 'header-line-format)
          (kill-local-variable 'header-line-format))
        (when (and (local-variable-p 'mode-line-format)
                   (not (eq iota-modeline-position 'header)))
          (kill-local-variable 'mode-line-format)))
       ((string= (buffer-name) "*Completions*")
        (setq-local mode-line-format
                    '(:eval (iota-box-horizontal-line (iota-modeline-effective-width)
                                                      'single 'iota-muted-face))))
       (t
        (setq-local mode-line-format nil)
        (setq-local header-line-format nil))))))

(defun iota-modeline--buffer-list-update ()
  "Buffer-list-update hook for modeline updates."
  (dolist (window (window-list))
    (iota-modeline--ensure-format-in-buffer (window-buffer window)))
  ;; Use centralized update system
  (when (featurep 'iota-update)
    (iota-update-request :modeline)))

(defun iota-modeline--after-change-major-mode ()
  "Hook to apply modeline format after major mode change."
  (when iota-modeline-mode
    (iota-modeline--ensure-format-in-buffer (current-buffer))))

(defun iota-modeline--minibuffer-setup ()
  "Hook to disable modeline in minibuffer."
  (setq mode-line-format "")
  (setq header-line-format nil)
  ;; Update separator lines to show separator above minibuffer
  (iota-modeline--update-separator-lines))

(defun iota-modeline--minibuffer-exit ()
  "Hook for minibuffer exit - update separator lines."
  ;; Delay slightly to allow minibuffer to fully close
  (run-with-timer 0.01 nil #'iota-modeline--update-separator-lines))

(defun iota-modeline--window-scroll (win _start)
  "Window scroll hook for overlay update."
  (iota-modeline--update-overlay win))

(defvar iota-modeline--in-window-config-change nil
  "Guard against recursive window configuration changes.")

(defun iota-modeline--window-configuration-change ()
  "Hook for window configuration changes.
Updates separator lines when popups appear/disappear."
  (unless (or iota-modeline--in-window-config-change
              (bound-and-true-p iota--inhibit-updates))
    (let ((iota-modeline--in-window-config-change t))
      ;; Update separator lines for popup visibility changes
      (iota-modeline--update-separator-lines)
      (dolist (window (window-list))
        (with-current-buffer (window-buffer window)
          ;; Note: Removed kill-local-variable for header-line-format as it caused
          ;; visual flicker by momentarily exposing the default header-line.
          (when (and (not (display-graphic-p))
                     (eq iota-modeline-position 'header))
            (iota-modeline--update-overlay window)))))))

(defun iota-modeline--on-window-buffer-change (frame)
  "Hook for window buffer changes in FRAME.
This handles cases like info-mode navigation where the buffer content
changes but it's still the same window."
  (dolist (window (window-list frame 'no-minibuf))
    (when (and (not (display-graphic-p))
               (eq iota-modeline-position 'header))
      (iota-modeline--update-overlay window)))
  ;; Also update separator lines
  (iota-modeline--update-separator-lines))

;;; Window Position Detection

(defun iota-modeline--window-size-change (_frame)
  "Handle window size changes."
  (iota-modeline--update))

(defun iota-modeline--window-is-at-bottom-p (window)
  "Return t if WINDOW is at the bottom of the frame.
Ignores popup windows (which-key, transient, etc.) when determining
if there's a window below, since popups are handled separately.
Exception: If the window below is the splash screen, return t (show separator)."
  (catch 'found-window-below
    (let* ((edges1 (window-edges window))
           (bottom1 (nth 3 edges1))
           (left1 (nth 0 edges1))
           (right1 (nth 2 edges1)))
      (dolist (w (window-list nil 'no-minibuf))
        (when (not (eq window w))
          (let* ((edges2 (window-edges w))
                 (top2 (nth 1 edges2))
                 (left2 (nth 0 edges2))
                 (right2 (nth 2 edges2)))
            ;; Check if window w is directly below window
            (when (and edges1 edges2
                       (= top2 bottom1)
                       (> (min right1 right2) (max left1 left2)))
              ;; Skip popup windows - they are handled by iota-popup
              ;; Skip excluded buffers - they get separator from window above
              (when (not (or (and (fboundp 'iota-popup--window-popup-p)
                                  (iota-popup--window-popup-p w))
                             (iota-modeline--buffer-excluded-p (window-buffer w))))
                ;; Check if it's the splash/screen buffer - if so, treat current window as at bottom
                (let ((buf-name (buffer-name (window-buffer w))))
                  (if (or (string-match-p "\\*I O T Λ splash\\*" buf-name)
                          (string-match-p "\\*I O T Λ screen-[0-9]+\\*" buf-name))
                      ;; Splash/Screen below - current window should show separator
                      nil  ; Don't throw, continue and return t
                    ;; Regular window below - not at bottom
                    (throw 'found-window-below nil))))))))
      t)))

(defun iota-modeline--minibuffer-active-p ()
  "Return t if the minibuffer is currently active."
  (and (active-minibuffer-window)
       (minibufferp (window-buffer (active-minibuffer-window)))))

(defun iota-modeline--get-separator-face (window)
  "Get the appropriate face for separator line in WINDOW.
Returns inactive face when popup/minibuffer is active,
otherwise returns the window's active/inactive face."
  (let ((popup-visible (and (fboundp 'iota-popup--popup-visible-p)
                            (iota-popup--popup-visible-p)))
        (minibuffer-active (iota-modeline--minibuffer-active-p)))
    (if (or popup-visible minibuffer-active)
        'iota-inactive-box-face
      (iota-theme-get-box-face window))))

(defun iota-modeline--update-separator-lines (&optional _frame)
  "Update separator lines for windows based on position.

Separator line logic:
- Windows AT BOTTOM (adjacent to minibuffer): show separator
  This separates the window content from the minibuffer/echo area.
- Windows NOT at bottom (have window below): hide separator
  The window below has its own header-line box, no separator needed.

Face logic for bottom windows:
- When popup/minibuffer is active: use inactive face
- Otherwise: use window's active/inactive face

Note: Splash screen is handled separately by iota-splash.el."
  (let* ((popup-visible (and (fboundp 'iota-popup--popup-visible-p)
                            (iota-popup--popup-visible-p)))
         (minibuffer-active (iota-modeline--minibuffer-active-p))
         (use-inactive-face (or popup-visible minibuffer-active)))
    ;; Process each window
    (dolist (window (window-list nil 'no-minibuf))
      (let* ((buf (window-buffer window))
             (buf-name (buffer-name buf))
             ;; Skip splash/screen buffers - they handle their own separator
             (is-splash-or-screen (or (string-match-p "\\*I O T Λ splash\\*" buf-name)
                                     (string-match-p "\\*I O T Λ screen-[0-9]+\\*" buf-name))))
        (unless is-splash-or-screen
          (let ((is-bottom (iota-modeline--window-is-at-bottom-p window))
                (should-show (with-current-buffer buf
                              (iota-modeline--should-show-p))))
            (if (and should-show is-bottom)
                ;; Bottom window: show separator line via mode-line-format
                ;; Use iota-separator--render for proper width/padding with olivetti
                ;; Face is evaluated dynamically on each redisplay
                (progn
                  ;; Set mode-line-format to render separator with dynamic face
                  (with-current-buffer buf
                    (setq-local mode-line-format
                                '(:eval (iota-separator--render
                                         (selected-window)
                                         (iota-modeline--get-separator-face (selected-window))))))
                  ;; Clear window parameter to allow mode-line to show
                  (set-window-parameter window 'mode-line-format nil))
              ;; Not at bottom or shouldn't show: hide separator
              (set-window-parameter window 'mode-line-format 'none))))))))

;;; Face Management

(defun iota-modeline--on-focus-change ()
  "Handle frame focus changes."
  (let ((dominated (frame-focus-state)))
    (if dominated
        (progn
          (setq iota-modeline--frame-focus-state t)
          ;; Force redraw of all overlays with correct state
          (iota-modeline--update))
      (setq iota-modeline--frame-focus-state nil)
      ;; Force redraw to ensure consistent state
      (iota-modeline--update))))

(defun iota-modeline--save-face-attributes ()
  "Save current mode-line face attributes for later restoration."
  (setq iota-modeline--saved-mode-line-faces
        (list
         (list 'mode-line
               :background (face-attribute 'mode-line :background nil t)
               :box (face-attribute 'mode-line :box nil t)
               :underline (face-attribute 'mode-line :underline nil t)
               :overline (face-attribute 'mode-line :overline nil t))
         (list 'mode-line-inactive
               :background (face-attribute 'mode-line-inactive :background nil t)
               :box (face-attribute 'mode-line-inactive :box nil t)
               :underline (face-attribute 'mode-line-inactive :underline nil t)
               :overline (face-attribute 'mode-line-inactive :overline nil t))
         (list 'header-line
               :background (face-attribute 'header-line :background nil t)
               :box (face-attribute 'header-line :box nil t)
               :underline (face-attribute 'header-line :underline nil t)
               :overline (face-attribute 'header-line :overline nil t)))))

(defun iota-modeline--restore-face-attributes ()
  "Restore saved mode-line face attributes."
  (dolist (spec iota-modeline--saved-mode-line-faces)
    (let ((face (car spec))
          (attrs (cdr spec)))
      (condition-case nil
          (apply #'set-face-attribute face nil attrs)
        (error nil)))))

;;; Mode Setup

(defun iota-modeline--setup ()
  "Set up IOTA modeline."
  ;; Save original formats FIRST
  (unless iota-modeline--original-mode-line-format
    (setq iota-modeline--original-mode-line-format
          (default-value 'mode-line-format)))
  (unless iota-modeline--original-header-line-format
    (setq iota-modeline--original-header-line-format
          (default-value 'header-line-format)))

  ;; Save face attributes
  (iota-modeline--save-face-attributes)

  ;; Configure fringes for proper width calculation
  (iota-configure-fringes)

  ;; Remove backgrounds from modeline faces for transparency
  (set-face-attribute 'mode-line nil
                      :background 'unspecified
                      :box nil
                      :underline nil
                      :overline nil)
  (set-face-attribute 'mode-line-inactive nil
                      :background 'unspecified
                      :box nil
                      :underline nil
                      :overline nil)
  (set-face-attribute 'header-line nil
                      :background 'unspecified
                      :box nil
                      :underline nil
                      :overline nil)

  ;; Apply IOTA modeline based on position
  (pcase iota-modeline-position
    ('header
     ;; Header position uses overlays for the box, mode-line for separator
     (setq-default header-line-format nil)
     (add-hook 'window-scroll-functions #'iota-modeline--window-scroll)
     (add-hook 'window-size-change-functions #'iota-modeline--update-separator-lines)
     (iota-modeline--update-separator-lines))
    ('mode
     (setq-default header-line-format nil)
     (setq-default mode-line-format '(:eval (iota-modeline--render))))
    ('both
     (if (display-graphic-p)
         (setq-default header-line-format '(:eval (iota-modeline--render)))
       (setq-default header-line-format nil)
       (add-hook 'window-scroll-functions #'iota-modeline--window-scroll))
     (setq-default mode-line-format '(:eval (iota-modeline--render)))))

  ;; Enable transparent theme in terminal mode
  (when (and (not (display-graphic-p))
             (fboundp 'iota-theme-transparent-mode)
             (not (bound-and-true-p iota-theme-transparent-mode)))
    (iota-theme-transparent-mode 1))

  ;; Register with centralized update system
  (when (featurep 'iota-update)
    (iota-update-register-component :modeline #'iota-modeline--do-update)
    (iota-update-install-hooks))

  ;; Set up hooks
  (unless (featurep 'iota-update)
    (add-hook 'buffer-list-update-hook #'iota-modeline--buffer-list-update))

  ;; Add post-command-hook for immediate keycast updates
  (add-hook 'post-command-hook #'iota-modeline--post-command)

  ;; Add focus change function for proper rendering when terminal focus changes
  (add-function :after after-focus-change-function #'iota-modeline--on-focus-change)

  (add-hook 'after-change-major-mode-hook #'iota-modeline--after-change-major-mode)
  (add-hook 'minibuffer-setup-hook #'iota-modeline--minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'iota-modeline--minibuffer-exit)
  (add-hook 'window-configuration-change-hook #'iota-modeline--window-configuration-change)
  ;; Handle buffer content changes within same window (e.g., info-mode navigation)
  (add-hook 'window-buffer-change-functions #'iota-modeline--on-window-buffer-change)

  (unless (featurep 'iota-update)
    (add-hook 'window-size-change-functions #'iota-modeline--window-size-change))

  ;; Apply to all existing buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (iota-modeline--should-show-p)
          (progn
            (kill-local-variable 'header-line-format)
            (kill-local-variable 'mode-line-format))
        (setq-local mode-line-format nil))))

  ;; Disable mode-line in minibuffer buffers
  (dolist (buffer (buffer-list))
    (when (string-match-p "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))
      (with-current-buffer buffer
        (setq mode-line-format ""
              header-line-format nil))))

  ;; Set window parameters for minibuffer windows
  (dolist (frame (frame-list))
    (let ((minibuf-win (minibuffer-window frame)))
      (when (window-live-p minibuf-win)
        (set-window-parameter minibuf-win 'mode-line-format nil))))

  ;; Initial update
  (iota-modeline--update))

(defun iota-modeline--teardown ()
  "Tear down IOTA modeline."
  ;; Unregister from centralized update system
  (when (featurep 'iota-update)
    (iota-update-unregister-component :modeline))

  ;; Remove hooks
  (remove-hook 'buffer-list-update-hook #'iota-modeline--buffer-list-update)
  (remove-hook 'post-command-hook #'iota-modeline--post-command)
  (remove-function after-focus-change-function #'iota-modeline--on-focus-change)
  (remove-hook 'after-change-major-mode-hook #'iota-modeline--after-change-major-mode)
  (remove-hook 'minibuffer-setup-hook #'iota-modeline--minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'iota-modeline--minibuffer-exit)
  (remove-hook 'window-scroll-functions #'iota-modeline--window-scroll)
  (remove-hook 'window-size-change-functions #'iota-modeline--window-size-change)
  (remove-hook 'window-size-change-functions #'iota-modeline--update-separator-lines)
  (remove-hook 'window-configuration-change-hook #'iota-modeline--window-configuration-change)
  (remove-hook 'window-buffer-change-functions #'iota-modeline--on-window-buffer-change)

  ;; Clear window parameters
  (dolist (window (window-list nil 'no-minibuf))
    (set-window-parameter window 'mode-line-format nil))

  ;; Remove overlays
  (iota-modeline--remove-overlay)
  (iota-modeline--remove-separator-overlays)
  
  ;; Clean up separator module
  (when (featurep 'iota-separator)
    (iota-separator--remove-all))

  ;; Restore original formats
  (when iota-modeline--original-header-line-format
    (setq-default header-line-format iota-modeline--original-header-line-format))
  (when iota-modeline--original-mode-line-format
    (setq-default mode-line-format iota-modeline--original-mode-line-format))

  ;; Restore face attributes
  (iota-modeline--restore-face-attributes)

  ;; Clear saved state
  (setq iota-modeline--original-mode-line-format nil)
  (setq iota-modeline--original-header-line-format nil)
  (setq iota-modeline--saved-mode-line-faces nil)

  ;; Restore fringes
  (iota-restore-fringes)

  ;; Force update
  (force-mode-line-update t))

;;; Minor Mode

;;;###autoload
(define-minor-mode iota-modeline-mode
  "Toggle I O T Λ modeline mode.
When enabled, wraps the existing modeline with I O T Λ's
decorated TUI-style box drawing."
  :global t
  :group 'iota-modeline
  :lighter " ι"
  (if iota-modeline-mode
      (iota-modeline--setup)
    (iota-modeline--teardown)))

;;; Interactive Commands

(defun iota-modeline-refresh ()
  "Refresh I O T Λ modeline."
  (interactive)
  (iota-modeline--update))

(defun iota-modeline-toggle-position ()
  "Toggle modeline position between header and mode line."
  (interactive)
  (setq iota-modeline-position
        (pcase iota-modeline-position
          ('header 'mode)
          ('mode 'header)
          (_ 'header)))
  (when iota-modeline-mode
    (iota-modeline--teardown)
    (iota-modeline--setup))
  (message "I O T Λ modeline position: %s" iota-modeline-position))

(defun iota-modeline-cycle-style ()
  "Cycle through box styles."
  (interactive)
  (setq iota-modeline-box-style
        (pcase iota-modeline-box-style
          ('single 'double)
          ('double 'rounded)
          ('rounded 'heavy)
          ('heavy 'ascii)
          ('ascii 'single)
          (_ 'rounded)))
  (iota-modeline-refresh)
  (message "I O T Λ modeline style: %s" iota-modeline-box-style))

(defun iota-modeline-toggle-current-window ()
  "Toggle modeline box visibility in current window only.
Uses window parameters to hide/show the modeline box without affecting the separator line.
This is window-specific and does not affect other windows showing the same buffer."
  (interactive)
  (let ((hidden (window-parameter nil 'iota-modeline-hidden)))
    (if hidden
        ;; Currently hidden - show it
        (progn
          (set-window-parameter nil 'iota-modeline-hidden nil)
          (iota-modeline--update-overlay (selected-window))
          (message "Modeline shown in current window"))
      ;; Currently shown - hide it
      (progn
        (set-window-parameter nil 'iota-modeline-hidden t)
        (iota-modeline--update-overlay (selected-window))
        (message "Modeline hidden in current window")))))

(provide 'iota-modeline)
;;; iota-modeline.el ends here
