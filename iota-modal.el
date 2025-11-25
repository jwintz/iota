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

(defcustom iota-modal-indicator-style 'both
  "Style for modal state indicators.
- `both': Show both glyph and label (e.g., \"● NORMAL\") (default)
- `glyph': Show only glyph (e.g., \"●\")
- `label': Show only label (e.g., \"NORMAL\")
- `meow': Use Meow's own indicator (Meow only, falls back to `both`)"
  :type '(choice (const :tag "Both glyph and label" both)
                 (const :tag "Glyph only" glyph)
                 (const :tag "Label only" label)
                 (const :tag "Meow's indicator" meow))
  :group 'iota-modal)

(defcustom iota-modal-modal-indicators
  '((normal . ("●" "#50fa7b"))    ; Green dot - Meow NORMAL
    (insert . ("◆" "#8be9fd"))    ; Cyan diamond - Meow INSERT
    (motion . ("◇" "#bd93f9"))    ; Purple diamond - Meow MOTION
    (keypad . ("◎" "#ff79c6"))    ; Pink circle - Meow KEYPAD
    (beacon . ("◉" "#ffb86c"))    ; Orange circle - Meow BEACON
    (visual . ("◈" "#bd93f9"))    ; Purple diamond outline - Evil VISUAL
    (replace . ("▲" "#ff5555"))   ; Red triangle - Evil REPLACE
    (emacs . ("○" "#6272a4")))    ; Gray circle - Emacs mode (no modal editing)
  "Modal state indicators with (GLYPH COLOR) pairs.
Supports both Evil and Meow modal editing states."
  :type '(alist :key-type symbol
                :value-type (list string color))
  :group 'iota-modal)

(defun iota-modal-meow-active-p ()
  "Return t if Meow modal editing is currently active.
Checks both global and buffer-local meow-mode."
  (or (and (boundp 'meow-mode) meow-mode)
      (and (boundp 'meow-global-mode) meow-global-mode)))

(defun iota-modal-get-modal-state ()
  "Detect modal editing state (Evil, Meow, Xah-Fly, etc.)."
  (cond
   ((and (boundp 'evil-state) evil-state)
    evil-state)
   ((iota-modal-meow-active-p)
    (if (and (boundp 'meow--current-state) meow--current-state)
        meow--current-state
      ;; Meow is active but state is nil (e.g., in meow-tutor special buffers)
      ;; Default to 'motion as a reasonable fallback
      'motion))
   (t 'emacs)))

(defun iota-modal-meow-indicator ()
  "Return Meow modeline indicator according to `iota-modal-indicator-style'.
Falls back to IOTA indicator if Meow doesn't provide one."
  (when (iota-modal-meow-active-p)
    (pcase iota-modal-indicator-style
      ('meow
       ;; Use Meow's own indicator if available
       (if (fboundp 'meow-indicator)
           (let ((meow-ind (meow-indicator)))
             (if (stringp meow-ind)
                 meow-ind
               ;; Meow indicator might return nil, fall back to both
               (iota-modal--format-meow-state meow--current-state 'both)))
         ;; Meow indicator not available, fall back to both
         (iota-modal--format-meow-state meow--current-state 'both)))
      (_
       ;; Use IOTA's indicator for Meow with specified style
       (iota-modal--format-meow-state meow--current-state iota-modal-indicator-style)))))

(defun iota-modal--format-meow-state (state style)
  "Format Meow STATE using IOTA's modal indicators with STYLE.
STYLE can be `both', `glyph', or `label'."
  (when-let ((indicator (alist-get state iota-modal-modal-indicators)))
    (let* ((glyph (car indicator))
           (color (cadr indicator))
           (label (upcase (symbol-name state)))
           (text (pcase style
                   ('both (concat glyph " " label))
                   ('glyph glyph)
                   ('label label)
                   (_ (concat glyph " " label))))) ; default to both
      (propertize text
                  'face `(:foreground ,color)
                  'help-echo (format "Meow: %s" state)))))

(defun iota-modal--format-evil-state (state style)
  "Format Evil STATE using IOTA's modal indicators with STYLE.
STYLE can be `both', `glyph', or `label'."
  (when-let ((indicator (alist-get state iota-modal-modal-indicators)))
    (let* ((glyph (car indicator))
           (color (cadr indicator))
           (label (upcase (symbol-name state)))
           (text (pcase style
                   ('both (concat glyph " " label))
                   ('glyph glyph)
                   ('label label)
                   ('meow (concat glyph " " label)) ; Fall back to both for Evil
                   (_ (concat glyph " " label))))) ; default to both
      (propertize text
                  'face `(:foreground ,color)
                  'help-echo (format "Evil: %s" state)))))

(defun iota-modal-modal-indicator ()
  "Return propertized modal state indicator.
For Meow: uses Meow's indicator or IOTA's Meow-specific indicators.
For Evil: uses IOTA's Evil indicators.
For Emacs mode: returns visual indicator showing modal editing is disabled."
  (let ((state (iota-modal-get-modal-state)))
    (cond
     ;; Meow is active - use Meow indicator
     ((iota-modal-meow-active-p)
      (iota-modal-meow-indicator))
     ;; Evil is active - use Evil indicator
     ((and (boundp 'evil-state) evil-state (not (eq state 'emacs)))
      (iota-modal--format-evil-state state iota-modal-indicator-style))
     ;; Emacs mode (no modal editing) - show distinct indicator
     (t
      (when-let ((indicator (alist-get 'emacs iota-modal-modal-indicators)))
        (let* ((glyph (car indicator))
               (color (cadr indicator))
               (text (pcase iota-modal-indicator-style
                       ('both (concat glyph " EMACS"))
                       ('glyph glyph)
                       ('label "EMACS")
                       (_ (concat glyph " EMACS"))))) ; default to both
          (propertize text
                      'face `(:foreground ,color)
                      'help-echo "Emacs mode (no modal editing)")))))))

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
Shows modal editing state (Meow/Evil) if active, otherwise buffer state.
Always recalculates to ensure state is current."
  (let* ((modal-ind (iota-modal-modal-indicator))
         (state iota-modal--current-state)
         (color (iota-modal-get-state-color state)))
    ;; Always use modal indicator if available (including 'emacs state)
    ;; This ensures visual distinction between Meow active and Emacs mode
    (or modal-ind
        (propertize (iota-modal--get-indicator-char)
                    'face `(:foreground ,color)))))

(defun iota-modal--build-inactive-indicator ()
  "Build indicator for inactive window.
Shows the same content as active but with inactive face."
  (let* ((modal-ind (iota-modal-modal-indicator)))
    (if modal-ind
        ;; Apply inactive face to the modal indicator text
        (propertize (substring-no-properties modal-ind)
                    'face 'iota-inactive-modeline-face)
      ;; Fallback to character
      (propertize (iota-modal--get-indicator-char)
                  'face 'iota-inactive-modeline-face))))

(defun iota-modal-state-segment ()
  "Create a modeline segment showing modal editing and buffer state.

Shows:
- Meow states (NORMAL, INSERT, MOTION, etc.) with Meow's indicator or IOTA's
- Evil states (normal, insert, visual, etc.) with IOTA's indicators
- Emacs mode (gray circle) when no modal editing is active
- Buffer states (modified, read-only, recording, search) when in Emacs mode

Active windows show colored indicators, inactive windows show same content in gray.
State is buffer-local and updates immediately on modal state changes."
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
             ;; Inactive: show same content but with inactive face
             (iota-modal--build-inactive-indicator)))
   :face nil  ; Don't override the propertized face in the text
   :align 'left
   :priority 150
   :help-echo (lambda ()
                (let ((modal-state (iota-modal-get-modal-state)))
                  (if (eq modal-state 'emacs)
                      (format "Emacs mode - Buffer state: %s" iota-modal--current-state)
                    (format "Modal: %s" modal-state))))))

;;; Interactive Commands

;;;###autoload
(defun iota-modal-debug-state ()
  "Debug function to check current modal state detection."
  (interactive)
  (let ((msg (format "Buffer: %s
meow-mode (buffer-local): %s
meow-global-mode: %s
meow--current-state: %s
iota-modal-meow-active-p: %s
iota-modal-get-modal-state: %s
evil-state: %s"
                     (buffer-name)
                     (if (boundp 'meow-mode) meow-mode "unbound")
                     (if (boundp 'meow-global-mode) meow-global-mode "unbound")
                     (if (boundp 'meow--current-state) meow--current-state "unbound")
                     (iota-modal-meow-active-p)
                     (iota-modal-get-modal-state)
                     (if (boundp 'evil-state) evil-state "unbound"))))
    (message "%s" msg)
    (with-current-buffer (get-buffer-create "*IOTA Modal Debug*")
      (erase-buffer)
      (insert msg)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun iota-modal-cycle-indicator-style ()
  "Cycle through modal indicator styles: both → glyph → label → meow → both."
  (interactive)
  (setq iota-modal-indicator-style
        (pcase iota-modal-indicator-style
          ('both 'glyph)
          ('glyph 'label)
          ('label 'meow)
          ('meow 'both)
          (_ 'both)))
  ;; Invalidate cached indicators to force refresh
  (iota-modal--invalidate-cache)
  (message "Modal indicator style: %s" iota-modal-indicator-style))

;;;###autoload
(defun iota-modal-toggle-meow-indicator ()
  "Toggle between Meow's own indicator and IOTA's both style.
Deprecated: Use `iota-modal-cycle-indicator-style' instead."
  (interactive)
  (setq iota-modal-indicator-style
        (if (eq iota-modal-indicator-style 'meow) 'both 'meow))
  (iota-modal--invalidate-cache)
  (message "Modal indicator style: %s" iota-modal-indicator-style))

;;; Hooks

(defun iota-modal--invalidate-cache ()
  "Force modeline update in all buffers.
Forces the modal segment to re-render immediately."
  (force-mode-line-update t))

(defvar-local iota-modal--last-modal-state nil
  "Last known modal editing state for change detection.
Buffer-local so each buffer tracks its own modal state.")

(defun iota-modal--on-meow-mode-change ()
  "Hook function called when Meow mode is toggled.
Invalidates cache to ensure modal segment updates immediately."
  (iota-modal--invalidate-cache))

(defun iota-modal--on-state-change (&optional _new-state)
  "Hook to detect modal state changes and force modeline update.
Optional NEW-STATE argument for compatibility with meow-switch-state-hook.
Runs on every command to ensure state is always current."
  (when (or (iota-modal-meow-active-p)
            (and (boundp 'evil-state) evil-state))
    (let ((current-state (iota-modal-get-modal-state)))
      (unless (eq current-state iota-modal--last-modal-state)
        (setq iota-modal--last-modal-state current-state)
        ;; Force immediate modeline update
        (force-mode-line-update t)))))

(defun iota-modal--setup-hooks ()
  "Set up hooks to track modal editing state changes."
  ;; Hook into Meow/Evil mode toggles (enable/disable)
  (when (boundp 'meow-mode-hook)
    (add-hook 'meow-mode-hook #'iota-modal--on-meow-mode-change))
  (when (boundp 'meow-global-mode-hook)
    (add-hook 'meow-global-mode-hook #'iota-modal--on-meow-mode-change))
  (when (boundp 'evil-mode-hook)
    (add-hook 'evil-mode-hook #'iota-modal--on-meow-mode-change))
  ;; Meow's state switch hook (fires immediately on state change)
  (when (boundp 'meow-switch-state-hook)
    (add-hook 'meow-switch-state-hook #'iota-modal--on-state-change))
  ;; Pre and post-command hooks to detect state changes
  (add-hook 'pre-command-hook #'iota-modal--on-state-change)
  (add-hook 'post-command-hook #'iota-modal--on-state-change))

(defun iota-modal--remove-hooks ()
  "Remove hooks that track modal editing state changes."
  (when (boundp 'meow-mode-hook)
    (remove-hook 'meow-mode-hook #'iota-modal--on-meow-mode-change))
  (when (boundp 'meow-global-mode-hook)
    (remove-hook 'meow-global-mode-hook #'iota-modal--on-meow-mode-change))
  (when (boundp 'evil-mode-hook)
    (remove-hook 'evil-mode-hook #'iota-modal--on-meow-mode-change))
  (when (boundp 'meow-switch-state-hook)
    (remove-hook 'meow-switch-state-hook #'iota-modal--on-state-change))
  (remove-hook 'pre-command-hook #'iota-modal--on-state-change)
  (remove-hook 'post-command-hook #'iota-modal--on-state-change))

(defvar iota-modal-mode-hook nil
  "Hook run when IOTA modal mode is enabled.")

;;;###autoload
(define-minor-mode iota-modal-mode
  "Enable IOTA modal and state-aware theming.
Provides visual feedback through a colored state indicator in the modeline.

Supports modal editing systems:
  - Meow: Shows both glyph and label by default (e.g., ● NORMAL)
    - ● NORMAL (green dot), ◆ INSERT (cyan diamond), ◇ MOTION (purple diamond)
    - ◎ KEYPAD (pink circle), ◉ BEACON (orange circle)
  - Evil: Shows both glyph and label (e.g., ● NORMAL)
  - Emacs mode: ○ EMACS (gray circle) — Shown when modal editing is disabled

Indicator styles (configure via `iota-modal-indicator-style'):
  - both (default): Shows glyph and label (● NORMAL)
  - glyph: Shows only glyph (●)
  - label: Shows only label (NORMAL)
  - meow: Uses Meow's own indicator (Meow only)

Buffer state indicators (when in Emacs mode):
  - Normal (green): Regular editing
  - Modified (orange): Buffer has unsaved changes
  - Read-only (red): Buffer is read-only
  - Recording (pink): Macro recording active
  - Search (cyan): Incremental search active

Cycle indicator styles with `iota-modal-cycle-indicator-style'."
  :global t
  :group 'iota-modal
  (if iota-modal-mode
      (progn
        ;; Set up hooks to track modal mode changes
        (iota-modal--setup-hooks)
        ;; NO post-command-hook needed! State is detected lazily during render
        ;; This eliminates the performance overhead of checking state on every command
        (condition-case err
            (iota-modal-add-segment)
          (error
           (message "Warning: Could not add modal segment: %s" err)))
        (message "IOTA modal mode enabled - state tracked lazily"))
    (progn
      ;; Remove hooks
      (iota-modal--remove-hooks)
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
