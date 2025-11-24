;;; iota-modal.el --- IOTA modal and state-aware UI theming -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: faces, modal, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Modal and state-aware theming for IOTA UI elements.
;; Provides dynamic visual feedback based on buffer state, modal editing
;; mode, and Emacs activity (recording, searching, etc.).

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'iota-theme)

;;; Configuration

(defgroup iota-modal nil
  "IOTA modal and state-aware theming configuration."
  :group 'iota
  :prefix "iota-modal-")

(defcustom iota-modal 'adaptive
  "Theme for IOTA UI elements.
Options:
  - adaptive: Dynamic colors based on state
  - spectrum: Full color spectrum animations
  - minimal: Subtle monochrome
  - custom: User-defined palette"
  :type '(choice (const :tag "Adaptive" adaptive)
                 (const :tag "Spectrum" spectrum)
                 (const :tag "Minimal" minimal)
                 (const :tag "Custom" custom))
  :group 'iota-modal)

(defcustom iota-modal-color-states
  '((normal . "#4ade80")      ; Green - normal operation
    (modified . "#ffa500")    ; Orange - buffer modified
    (read-only . "#ff6b6b")   ; Red - read-only buffer
    (inactive . "#6272a4")    ; Purple - inactive window
    (recording . "#ff79c6")   ; Pink - macro recording
    (search . "#8be9fd")      ; Cyan - isearch active
    (error . "#ff5555")       ; Bright red - error state
    (success . "#50fa7b"))    ; Bright green - success
  "State-based color palette for IOTA elements."
  :type '(alist :key-type symbol :value-type color)
  :group 'iota-modal)

(defcustom iota-modal-animate-transitions t
  "Enable smooth color transitions between states."
  :type 'boolean
  :group 'iota-modal)

(defcustom iota-modal-transition-duration 0.3
  "Duration of color transitions between states (seconds)."
  :type 'number
  :group 'iota-modal)

;;; State Management

(defvar-local iota-modal--current-state 'normal
  "Current element state for theming.
Buffer-local so each buffer can have independent state.")

(defvar-local iota-modal--cached-indicator nil
  "Cached indicator string with color for active window.
Buffer-local to avoid recalculating on every render.")

(defvar iota-modal--transition-timers nil
  "Active transition timers.")

(defvar-local iota-modal--last-update-time 0
  "Timestamp of last state update for this buffer.")

(defvar iota-modal--update-debounce 0.1
  "Minimum seconds between state updates to prevent lag.")

(defun iota-modal-get-state-color (&optional state)
  "Get color for STATE (defaults to current state)."
  (alist-get (or state iota-modal--current-state)
             iota-modal-color-states))

(defun iota-modal-detect-state ()
  "Detect current buffer state for theming.
Does not check window selection - that's handled separately during rendering."
  (cond
   (defining-kbd-macro 'recording)
   ((and (boundp 'isearch-mode) isearch-mode) 'search)
   (buffer-read-only 'read-only)
   ((buffer-modified-p) 'modified)
   (t 'normal)))

(defun iota-modal-update-state ()
  "Update current buffer state by detecting it.
Called lazily during rendering, not on every command.
Returns t if state changed."
  (let ((new-state (iota-modal-detect-state)))
    (unless (eq new-state iota-modal--current-state)
      (setq iota-modal--current-state new-state
            iota-modal--cached-indicator nil)  ; Invalidate cache
      t)))

(defun iota-modal-refresh-ui ()
  "Refresh UI elements with current theme state.
No explicit update needed - the modeline's post-command-hook handles it."
  ;; DO NOTHING - just updating the state is enough
  ;; The modeline will re-render on its own through its post-command-hook
  ;; Calling force-mode-line-update here causes double-updates and lag
  nil)

;;; Color Transitions

(defun iota-modal-transition-color (from-color to-color face-attr)
  "Smoothly transition FACE-ATTR from FROM-COLOR to TO-COLOR."
  (when-let ((timer (alist-get face-attr iota-modal--transition-timers)))
    (cancel-timer timer))
  
  (let ((steps 10)
        (step-duration (/ iota-modal-transition-duration steps))
        (current-step 0))
    (setf (alist-get face-attr iota-modal--transition-timers)
          (run-with-timer 0 step-duration
            (lambda ()
              (setq current-step (1+ current-step))
              (let* ((progress (/ (float current-step) steps))
                     (interpolated (iota-theme-color-blend 
                                   from-color to-color progress)))
                (iota-modal-set-face-color face-attr interpolated)
                (when (>= current-step steps)
                  (cancel-timer (alist-get face-attr 
                                          iota-modal--transition-timers))
                  (setf (alist-get face-attr iota-modal--transition-timers) 
                        nil))))))))

(defun iota-modal-set-face-color (face-attr color)
  "Set FACE-ATTR to COLOR on appropriate faces.
Note: Only :foreground is allowed - no :background attributes."
  (when (eq face-attr :foreground)
    (set-face-attribute 'mode-line nil face-attr color)
    (set-face-attribute 'header-line nil face-attr color)))

(defun iota-modal-pulse-on-state-change (intensity)
  "Pulse UI elements with INTENSITY on state change.
Note: Disabled - no background attributes allowed."
  ;; Pulse effect disabled to avoid background color issues
  ;; Only foreground colors are allowed per IOTA constraints
  nil)

;;; Modal Editing Integration

(defcustom iota-modal-modal-indicators
  '((normal . ("●" "#50fa7b"))    ; Green dot
    (insert . ("◆" "#8be9fd"))    ; Cyan diamond
    (visual . ("◈" "#bd93f9"))    ; Purple diamond outline
    (replace . ("▲" "#ff5555"))   ; Red triangle
    (emacs . ("○" "#f1fa8c")))    ; Yellow circle
  "Modal state indicators with (GLYPH COLOR) pairs."
  :type '(alist :key-type symbol 
                :value-type (list string color))
  :group 'iota-modal)

(defun iota-modal-get-modal-state ()
  "Detect modal editing state (Evil, Meow, Xah-Fly, etc.)."
  (cond
   ((and (boundp 'evil-state) evil-state)
    evil-state)
   ((and (boundp 'meow-mode) meow-mode 
         (boundp 'meow--current-state))
    meow--current-state)
   (t 'emacs)))

(defun iota-modal-modal-indicator ()
  "Return propertized modal state indicator.
Only returns a value for actual modal editing modes (Evil, Meow).
Returns nil for regular Emacs mode so buffer state can be shown."
  (let ((state (iota-modal-get-modal-state)))
    ;; Only show modal indicator for non-emacs states
    (when (and state (not (eq state 'emacs)))
      (when-let ((indicator (alist-get state iota-modal-modal-indicators)))
        (propertize (car indicator)
                    'face `(:foreground ,(cadr indicator))
                    'help-echo (format "Mode: %s" state))))))

;;; Modeline Segment Integration

(defun iota-modal--get-indicator-char ()
  "Get the indicator character for current state.
Fast lookup without modal mode checks."
  (pcase iota-modal--current-state
    ('normal "●")
    ('modified "●")
    ('read-only "⊗")
    ('recording "⬤")
    ('search "⌕")
    (_ "•")))

(defun iota-modal--build-active-indicator ()
  "Build colored indicator for active window.
Cached in buffer-local variable to avoid recalculation."
  (or iota-modal--cached-indicator
      (let* ((state iota-modal--current-state)
             (color (iota-modal-get-state-color state))
             (indicator (or (iota-modal-modal-indicator)
                           (iota-modal--get-indicator-char))))
        (setq iota-modal--cached-indicator
              (propertize indicator 'face `(:foreground ,color))))))

(defun iota-modal-state-segment ()
  "Create a modeline segment showing current buffer state with color.
Each buffer maintains its own state. Active windows show colored indicator,
inactive windows show simple gray character only.
State is detected lazily during render, not on every command."
  (require 'iota-segment)
  (require 'iota-theme)
  (iota-segment-create
   :id 'modal-state
   :text (lambda ()
           ;; Update state lazily when rendering (not on every command!)
           (iota-modal-update-state)
           ;; Check if this window is active using the proper function
           (if (iota-theme-window-active-p)
               ;; Active: use cached colored indicator
               (iota-modal--build-active-indicator)
             ;; Inactive: just show character with inactive face
             (propertize (iota-modal--get-indicator-char)
                        'face 'iota-inactive-modeline-face)))
   :face nil  ; Don't override the propertized face in the text
   :align 'left
   :priority 150
   :help-echo (lambda ()
                (format "Buffer state: %s" iota-modal--current-state))))

;;; Hooks

(defvar iota-modal-mode-hook nil
  "Hook run when IOTA modal mode is enabled.")

;;;###autoload
(define-minor-mode iota-modal-mode
  "Enable IOTA modal and state-aware theming.
Provides visual feedback through a colored state indicator in the modeline.

When enabled, automatically adds and displays a colored indicator showing:
  - Normal (green): Regular editing
  - Modified (orange): Buffer has unsaved changes
  - Read-only (red): Buffer is read-only
  - Recording (pink): Macro recording active
  - Search (cyan): Incremental search active
  - Inactive (purple): Window is not selected"
  :global t
  :group 'iota-modal
  (if iota-modal-mode
      (progn
        ;; NO post-command-hook needed! State is detected lazily during render
        ;; This eliminates the performance overhead of checking state on every command
        (condition-case err
            (iota-modal-add-segment)
          (error
           (message "Warning: Could not add modal segment: %s" err)))
        (message "IOTA modal mode enabled - state tracked lazily"))
    (progn
      ;; Clean up segment
      (condition-case err
          (iota-modal-remove-segment)
        (error
         (message "Warning: Could not remove modal segment: %s" err)))
      (message "IOTA modal mode disabled"))))

(defvar iota-modal--original-preset nil
  "Original modeline preset before modal mode was enabled.")

(defun iota-modal-add-segment ()
  "Add modal state indicator to current modeline configuration.
Preserves the current preset and segments."
  (interactive)
  (require 'iota-segments)

  ;; Save original preset
  (setq iota-modal--original-preset iota-modeline-segments-preset)

  ;; Switch to custom and add segment to existing segments
  (setq iota-modeline-segments-preset 'custom)

  ;; Get current segments (from previous preset or custom)
  (let ((current-segments (pcase iota-modal--original-preset
                           ('minimal (iota-segments-minimal))
                           ('standard (iota-segments-standard))
                           ('full (iota-segments-full))
                           ('custom iota-modeline-custom-segments)
                           (_ (iota-segments-standard)))))

    ;; Only add if not already present
    (unless (cl-find-if (lambda (s)
                          (and (cl-struct-p s)
                               (ignore-errors (eq (iota-segment-id s) 'modal-state))))
                        current-segments)
      (setq iota-modeline-custom-segments
            (cons (iota-modal-state-segment) current-segments))))

  (when (bound-and-true-p iota-modeline-mode)
    (iota-modeline-refresh))
  (message "Modal state indicator added to modeline"))

(defun iota-modal-remove-segment ()
  "Remove modal state indicator and restore original preset."
  (interactive)
  (when (eq iota-modeline-segments-preset 'custom)
    ;; Remove the segment
    (setq iota-modeline-custom-segments
          (cl-remove-if (lambda (seg)
                          (and (cl-struct-p seg)
                               (ignore-errors (eq (iota-segment-id seg) 'modal-state))))
                        iota-modeline-custom-segments))

    ;; Restore original preset if it wasn't custom
    (when (and iota-modal--original-preset
               (not (eq iota-modal--original-preset 'custom)))
      (setq iota-modeline-segments-preset iota-modal--original-preset)
      (setq iota-modal--original-preset nil))

    (when (bound-and-true-p iota-modeline-mode)
      (iota-modeline-refresh))
    (message "Modal state indicator removed from modeline")))

(provide 'iota-modal)
;;; iota-modal.el ends here
