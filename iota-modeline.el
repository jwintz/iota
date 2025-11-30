;;; iota-modeline.el --- I O T Λ modeline implementation -*- lexical-binding: t; -*-

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
;; Main modeline implementation for I O T Λ (I Ø T Δ).
;; Provides a decorated header-line modeline with box-drawing characters,
;; dynamic segments, and theme integration.

;;; Code:

(require 'cl-lib)
(require 'iota-box)
(require 'iota-segment)
(require 'iota-segments)
(require 'iota-theme)
(require 'iota-tui)
(require 'iota-update)
(require 'iota-timers)
(require 'iota-cache)
(require 'iota-utils)

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

(defcustom iota-modeline-segments-preset 'standard
  "Preset segment configuration.
Can be: minimal, standard, full, or custom."
  :type '(choice (const :tag "Minimal" minimal)
                 (const :tag "Standard" standard)
                 (const :tag "Full" full)
                 (const :tag "Custom" custom))
  :group 'iota-modeline)

(defcustom iota-modeline-custom-segments nil
  "Custom segment list when preset is \\='custom.
Should be a list of segment structs."
  :type '(repeat sexp)
  :group 'iota-modeline)

(defcustom iota-modeline-update-debounce 0.15
  "Debounce interval for modeline updates in seconds.
Recommended range: 0.1 - 0.3. Higher values reduce CPU usage.
Note: This is now handled by the centralized update system."
  :type 'float
  :group 'iota-modeline)

(defcustom iota-modeline-show-in-inactive nil
  "Show I O T Λ modeline in inactive windows.
If nil, inactive windows use default modeline."
  :type 'boolean
  :group 'iota-modeline)

(defcustom iota-modeline-excluded-buffers '("\\*Warnings\\*")
  "List of buffer names (strings or regexps) to exclude from IOTA modeline."
  :type '(repeat (choice string regexp))
  :group 'iota-modeline)

;;; State

(defvar iota-modeline--update-timer nil
  "Timer for periodic modeline updates.")

(defvar iota-modeline--last-update-time 0
  "Time of last modeline update.")

(defvar iota-modeline--current-window nil
  "The window currently being rendered.
This is dynamically bound during modeline rendering to allow
segments to check if they're rendering for the selected window.")

(defvar iota-modeline--selected-window nil
  "The truly selected window at the time of rendering.
This is captured BEFORE any with-selected-window context switches,
allowing segments to determine if they're in the active window.")

;;; Segment Management

(defun iota-modeline--get-segments ()
  "Get current segment list based on preset."
  (pcase iota-modeline-segments-preset
    ('minimal (iota-segments-minimal))
    ('standard (iota-segments-standard))
    ('full (iota-segments-full))
    ('custom iota-modeline-custom-segments)
    (_ (iota-segments-standard))))

;;; Modeline Rendering

(defun iota-modeline--fit-segments (segments width _style)
  "Fit SEGMENTS into WIDTH for box rendering with _STYLE.
Returns three lists: (left-fitted center-fitted right-fitted).
Each fitted list contains (segment . use-short) cons cells."
  (let* ((left-segs (cl-remove-if-not
                     (lambda (s) (eq (iota-segment-align s) 'left))
                     segments))
         (center-segs (cl-remove-if-not
                       (lambda (s) (eq (iota-segment-align s) 'center))
                       segments))
         (right-segs (cl-remove-if-not
                      (lambda (s) (eq (iota-segment-align s) 'right))
                      segments))
         ;; Box borders take 4 chars (2 borders + 2 spaces), separator is 3 chars " │ "
         (content-width (- width 4))
         (separator-width 3))

    ;; Iteratively fit segments - may need multiple passes if left+right overflow
    (let ((left-fitted nil)
          (right-fitted nil)
          (center-fitted nil)
          (left-width 0)
          (right-width 0))

      ;; Initial fit - give each side full content width
      (setq left-fitted (iota-segment-fit-to-width left-segs content-width separator-width))
      (setq right-fitted (iota-segment-fit-to-width right-segs content-width separator-width))
      (setq left-width (iota-segment--calculate-total-width left-fitted separator-width))
      (setq right-width (iota-segment--calculate-total-width right-fitted separator-width))

      ;; If left + right exceeds available width, refit proportionally
      (when (> (+ left-width right-width) content-width)
        (let* ((total (+ left-width right-width))
               (left-ratio (/ (float left-width) total))
               (right-ratio (/ (float right-width) total))
               (left-budget (floor (* content-width left-ratio 0.9))) ; 90% to leave margin
               (right-budget (floor (* content-width right-ratio 0.9))))
          (setq left-fitted (iota-segment-fit-to-width left-segs left-budget separator-width))
          (setq right-fitted (iota-segment-fit-to-width right-segs right-budget separator-width))
          (setq left-width (iota-segment--calculate-total-width left-fitted separator-width))
          (setq right-width (iota-segment--calculate-total-width right-fitted separator-width))))

      ;; Fit center with remaining space
      (let ((center-available (max 0 (- content-width left-width right-width 2))))
        (setq center-fitted (iota-segment-fit-to-width center-segs center-available separator-width)))

      (list left-fitted center-fitted right-fitted))))

(defun iota-modeline--render (&optional override-box-face window)
  "Render IOTA modeline format string for WINDOW.
If WINDOW is nil, use selected window."
  (if (not (iota-modeline--should-show-p))
      "" ; Return empty string for minibuffer
    (let* ((target-window (or window (selected-window)))
           ;; Use window-body-width which excludes margins, fringes, and scroll bars
           ;; Subtract 1 to prevent line wrapping
           (raw-width (window-body-width target-window))
           (width (if raw-width (max 1 (1- raw-width)) 80))  ; Fallback to 80 if nil
           ;; Bind current window for segments to check
           (iota-modeline--current-window target-window)
           ;; Keep existing selected-window binding if already set by caller,
           ;; otherwise use (selected-window) for :eval path
           (selected-win-value (if (and (boundp 'iota-modeline--selected-window)
                                        iota-modeline--selected-window)
                                   iota-modeline--selected-window
                                 (selected-window)))
           (iota-modeline--selected-window selected-win-value)
           (segments (iota-modeline--get-segments))
           (style iota-modeline-box-style)
           (box-face (or override-box-face
                        (iota-theme-get-box-face target-window)))
           ;; Fit segments to available width
           (fitted (iota-modeline--fit-segments segments width style))
           (left-fitted (nth 0 fitted))
           (center-fitted (nth 1 fitted))
           (right-fitted (nth 2 fitted)))
      (iota-box-render-single-line
       :left (iota-segment-render-fitted left-fitted)
       :center (iota-segment-render-fitted center-fitted)
       :right (iota-segment-render-fitted right-fitted)
       :width width
       :style style
       :face box-face))))

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
    (let ((overlay (gethash window iota-modeline--window-overlays)))
      ;; Clean up invalid overlay
      (when (and overlay
                 (overlayp overlay)
                 (not (overlay-buffer overlay)))
        (remhash window iota-modeline--window-overlays)
        (setq overlay nil))
      ;; Create new overlay if needed
      (unless overlay
        (with-current-buffer (window-buffer window)
          (setq overlay (make-overlay (point-min) (point-min) (current-buffer)))
          (overlay-put overlay 'priority 100)
          (puthash window overlay iota-modeline--window-overlays)))
      overlay)))

(defun iota-modeline--update-overlay (&optional window selected-window)
  "Update the simulated header overlay for WINDOW.
If WINDOW is nil, use selected window.
If SELECTED-WINDOW is provided, use it as the truly selected window."
  (condition-case err
      (let ((win (or window (selected-window)))
            ;; Use provided selected-window or capture it now
            (truly-selected (or selected-window (selected-window))))
        (when (and (window-live-p win)
                   (window-buffer win)
                   (buffer-live-p (window-buffer win)))
          (with-current-buffer (window-buffer win)
            (when (eq iota-modeline-position 'header)
              ;; Ensure native header line is disabled locally
              (kill-local-variable 'header-line-format)

              (let ((overlay (iota-modeline--ensure-overlay win))
                    (start (window-start win)))
                ;; Only proceed if we have valid overlay and start position
                (when (and overlay (overlayp overlay)
                           start (integer-or-marker-p start))
                  (condition-case render-err
                      ;; Pass the truly selected window via dynamic binding
                      ;; MUST use let* so iota-modeline--selected-window is bound
                      ;; before box is evaluated
                      (let* ((iota-modeline--selected-window truly-selected)
                             (box (with-selected-window win
                                    (iota-modeline--render nil win))))
                        (move-overlay overlay start start (current-buffer))
                        (overlay-put overlay 'before-string
                                     (if (and box (stringp box) (not (string= box "")))
                                         (concat box "\n")
                                       ""))
                        (overlay-put overlay 'window win))
                    (error
                     ;; Log detailed error with backtrace info
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
      ;; Clean up invalid overlay
      (when (and overlay
                 (overlayp overlay)
                 (not (overlay-buffer overlay)))
        (remhash window iota-modeline--separator-overlays)
        (setq overlay nil))
      ;; Create new overlay if needed
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
              ;; Show separator for bottom windows
              (overlay-put overlay 'after-string
                          (propertize "\n" 'display
                                     (list 'space :width (window-width window))))
            ;; Hide separator for non-bottom windows
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
  ;; Capture the truly selected window once at the start
  (let ((truly-selected (selected-window)))
    (dolist (win (window-list))
      (iota-modeline--update-overlay win truly-selected))))

(defun iota-modeline--update-windows (windows)
  "Update modeline display for specific WINDOWS only.
This is more efficient than updating all windows when only
a few need to change (e.g., on window selection change)."
  (let ((truly-selected (selected-window)))
    (dolist (win windows)
      (when (window-live-p win)
        (iota-modeline--update-overlay win truly-selected)))))

(defun iota-modeline--do-update ()
  "Perform modeline update (called by centralized update system).
This is the main entry point for the iota-update system."
  (iota-safe-call "modeline-update"
    (iota-modeline--update)))

(defun iota-modeline--should-update-p ()
  "Return t if modeline should update now."
  (let ((current-time (float-time)))
    (let ((elapsed (- current-time iota-modeline--last-update-time)))
      (>= elapsed iota-modeline-update-debounce))))

(defun iota-modeline--debounced-update ()
  "Update modeline with debouncing."
  (when (iota-modeline--should-update-p)
    (setq iota-modeline--last-update-time (float-time))
    (iota-modeline--update)))

(defun iota-modeline--setup-timers ()
  "Set up periodic update timers using centralized timer registry."
  ;; Use centralized timer management for proper cleanup
  (iota-timers-cancel 'modeline-periodic)
  ;; Periodic updates now handled by iota-update system
  ;; Only register if not using centralized updates
  (unless (featurep 'iota-update)
    (setq iota-modeline--update-timer
          (iota-timers-run-with-timer 'modeline-periodic 60 60 #'iota-modeline--update))))

(defun iota-modeline--cleanup-timers ()
  "Clean up timers using centralized timer registry."
  ;; Cancel via registry
  (iota-timers-cancel 'modeline-periodic)
  ;; Also clean up legacy timer if present
  (when iota-modeline--update-timer
    (cancel-timer iota-modeline--update-timer)
    (setq iota-modeline--update-timer nil))
  ;; Cancel any debounce timers
  (cancel-function-timers #'iota-modeline--debounced-update))

;;; Hooks

(defun iota-modeline--post-command ()
  "Post-command hook for modeline updates."
  (iota-modeline--debounced-update))

(defun iota-modeline--should-show-p ()
  "Return t if IOTA modeline should be shown in current buffer."
  (let ((buf-name (buffer-name)))
    (not (or (minibufferp)
             (string-prefix-p " *which-key*" buf-name)
             (string= "*Completions*" buf-name)
             (string-match-p "\\*I O T Λ splash\\*" buf-name)
             (cl-find-if (lambda (pattern) (string-match-p pattern buf-name))
                         iota-modeline-excluded-buffers)))))

(defun iota-modeline--ensure-format-in-buffer (buffer)
  "Ensure BUFFER has correct IOTA modeline format."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((iota-modeline--should-show-p)
        (when (local-variable-p 'header-line-format)
          (kill-local-variable 'header-line-format))
        ;; Only kill mode-line-format if not in header position
        ;; In header position, mode-line is used for the separator line
        (when (and (local-variable-p 'mode-line-format)
                   (not (eq iota-modeline-position 'header)))
          (kill-local-variable 'mode-line-format)))
       ((string= (buffer-name) "*Completions*")
        ;; For completions, show a simple separator line
        (setq-local mode-line-format
                    '(:eval (iota-box-horizontal-line (1- (window-body-width))
                                                      'single 'iota-muted-face))))
       (t
        ;; For other excluded buffers, explicitly set mode-line and header-line to nil
        (setq-local mode-line-format nil)
        (setq-local header-line-format nil))))))

(defun iota-modeline--buffer-list-update ()
  "Buffer-list-update hook for modeline updates."
  ;; Ensure all visible buffers have correct modeline format
  (dolist (window (window-list))
    (iota-modeline--ensure-format-in-buffer (window-buffer window)))
  (iota-modeline--debounced-update))

(defun iota-modeline--after-change-major-mode ()
  "Hook to apply modeline format after major mode change."
  (when iota-modeline-mode
    (iota-modeline--ensure-format-in-buffer (current-buffer))))

(defun iota-modeline--minibuffer-setup ()
  "Hook to disable modeline in minibuffer."
  (setq mode-line-format "")
  (setq header-line-format nil))

(defun iota-modeline--window-scroll (win start)
  "Window scroll hook for overlay update."
  (iota-modeline--update-overlay win))

(defvar iota-modeline--in-window-config-change nil
  "Guard against recursive window configuration changes.")

(defun iota-modeline--window-configuration-change ()
  "Hook for window configuration changes."
  ;; Prevent recursive calls
  (unless iota-modeline--in-window-config-change
    (let ((iota-modeline--in-window-config-change t))
      ;; Ensure all windows have correct modeline format
      (dolist (window (window-list))
        (with-current-buffer (window-buffer window)
          ;; Only kill buffer-local variables if they differ from defaults
          (when (and (local-variable-p 'header-line-format)
                     (not (equal header-line-format (default-value 'header-line-format))))
            (kill-local-variable 'header-line-format))
          (when (and (local-variable-p 'mode-line-format)
                     (not (equal mode-line-format (default-value 'mode-line-format))))
            (kill-local-variable 'mode-line-format))
          ;; For terminal mode with overlays, ensure overlay exists
          (when (and (not (display-graphic-p))
                     (eq iota-modeline-position 'header))
            (iota-modeline--update-overlay window)))))))

;;; Mode Setup

(defvar iota-modeline--original-header-line nil
  "Original header-line-format before IOTA.")

(defvar iota-modeline--original-mode-line nil
  "Original mode-line-format before IOTA.")

(defun iota-modeline--window-size-change (frame)
  "Handle window size changes."
  (iota-modeline--update))

(defun iota-modeline--window-is-at-bottom-p (window)
  "Return t if WINDOW is at the bottom of the frame.
Ignores which-key and other temporary popup buffers when determining position."
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
                 (right2 (nth 2 edges2))
                 (buf-name (buffer-name (window-buffer w))))
            ;; Ignore which-key and similar popup buffers when determining bottom position
            (when (and edges1 edges2
                       (= top2 bottom1) ; w is below window
                       (> (min right1 right2) (max left1 left2)) ; they overlap
                       (not (string-prefix-p " *which-key*" buf-name))
                       (not (string-prefix-p "*which-key*" buf-name)))
              (throw 'found-window-below nil))))) ; not at bottom
      t))) ; at bottom

(defun iota-modeline--update-separator-lines (&optional _frame)
  "Update mode-line separator for each window based on position.
Only windows that are at the bottom of the frame get a separator line."
  ;; Clear buffer-local mode-line-format and use per-window overlays instead
  (dolist (window (window-list nil 'no-minibuf))
    (with-current-buffer (window-buffer window)
      (when (iota-modeline--should-show-p)
        ;; Ensure mode-line is set to show the actual separator
        (setq-local mode-line-format
                    '(:eval (iota-box-horizontal-line (1- (window-body-width))
                                                      iota-modeline-box-style
                                                      (iota-theme-get-box-face (selected-window))))))))

  ;; Use window parameters to control visibility per-window
  (dolist (window (window-list nil 'no-minibuf))
    (if (and (with-current-buffer (window-buffer window)
               (iota-modeline--should-show-p))
             (iota-modeline--window-is-at-bottom-p window))
        ;; Window is at bottom - ensure mode-line is visible
        (set-window-parameter window 'mode-line-format nil)
      ;; Window is not at bottom - hide mode-line
      (set-window-parameter window 'mode-line-format 'none))))

(defun iota-modeline--setup ()
  "Set up IOTA modeline."
  ;; Save original formats
  (unless iota-modeline--original-header-line
    (setq iota-modeline--original-header-line
          (default-value 'header-line-format)))
  (unless iota-modeline--original-mode-line
    (setq iota-modeline--original-mode-line
          (default-value 'mode-line-format)))
  
  ;; Apply IOTA modeline
  (pcase iota-modeline-position
    ('header
     (setq-default header-line-format nil)
     (add-hook 'window-scroll-functions #'iota-modeline--window-scroll)
     ;; Use buffer-local mode-line for separator
     (add-hook 'window-size-change-functions #'iota-modeline--update-separator-lines)
     (iota-modeline--update-separator-lines))
    ('mode
     (setq-default header-line-format nil)
     (setq-default mode-line-format (iota-modeline--format)))
    ('both
     (if (display-graphic-p)
         (setq-default header-line-format (iota-modeline--format))
       (setq-default header-line-format nil)
       (add-hook 'window-scroll-functions #'iota-modeline--window-scroll))
     (setq-default mode-line-format (iota-modeline--format))))
  
  ;; Remove backgrounds from modeline faces
  (set-face-attribute 'mode-line nil :background 'unspecified)
  (set-face-attribute 'mode-line-inactive nil :background 'unspecified)
  (set-face-attribute 'header-line nil :background 'unspecified)

  ;; Enable transparent theme in terminal mode
  (when (and (not (display-graphic-p))
             (fboundp 'iota-theme-transparent-mode)
             (not (and (boundp 'iota-theme-transparent-mode)
                       iota-theme-transparent-mode)))
    (iota-theme-transparent-mode 1))

  ;; Register with centralized update system
  (when (featurep 'iota-update)
    (iota-update-register-component :modeline #'iota-modeline--do-update)
    (iota-update-install-hooks))
  
  ;; Set up hooks (minimal set - post-command-hook removed for performance)
  ;; The centralized update system uses idle timers instead
  (unless (featurep 'iota-update)
    ;; Fallback if update system not loaded
    (add-hook 'buffer-list-update-hook #'iota-modeline--buffer-list-update))
  
  (add-hook 'after-change-major-mode-hook #'iota-modeline--after-change-major-mode)
  (add-hook 'minibuffer-setup-hook #'iota-modeline--minibuffer-setup)
  
  ;; Window hooks now handled by centralized update system
  (unless (featurep 'iota-update)
    (add-hook 'window-size-change-functions #'iota-modeline--window-size-change))

  ;; Set up timers (now uses centralized timer registry)
  (iota-modeline--setup-timers)
  
  ;; Apply to all existing buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (iota-modeline--should-show-p)
          (progn
            (kill-local-variable 'header-line-format)
            (kill-local-variable 'mode-line-format))
        ;; Explicitly disable mode-line for excluded buffers
        (setq-local mode-line-format nil))))
  
  ;; Explicitly disable mode-line in all minibuffer buffers
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
  (remove-hook 'after-change-major-mode-hook #'iota-modeline--after-change-major-mode)
  (remove-hook 'minibuffer-setup-hook #'iota-modeline--minibuffer-setup)
  (remove-hook 'window-scroll-functions #'iota-modeline--window-scroll)
  (remove-hook 'window-size-change-functions #'iota-modeline--window-size-change)
  (remove-hook 'window-size-change-functions #'iota-modeline--update-separator-lines)
  (remove-hook 'window-configuration-change-hook #'iota-modeline--window-configuration-change)

  ;; Clear window parameters
  (dolist (window (window-list nil 'no-minibuf))
    (set-window-parameter window 'mode-line-format nil))

  ;; Remove overlays
  (iota-modeline--remove-overlay)
  (iota-modeline--remove-separator-overlays)

  ;; Clean up timers
  (iota-modeline--cleanup-timers)

  ;; Restore original formats
  (when iota-modeline--original-header-line
    (setq-default header-line-format iota-modeline--original-header-line))
  (when iota-modeline--original-mode-line
    (setq-default mode-line-format iota-modeline--original-mode-line))

  ;; Clear cache
  (iota-segment-cache-clear)

  ;; Force update
  (force-mode-line-update t))

;;; Minor Mode

;;;###autoload
(define-minor-mode iota-modeline-mode
  "Toggle I O T Λ modeline mode.
When enabled, replaces the default modeline with I O T Λ's
decorated TUI-style modeline."
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
  (iota-segment-cache-clear)
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

(defun iota-modeline-cycle-preset ()
  "Cycle through segment presets."
  (interactive)
  (setq iota-modeline-segments-preset
        (pcase iota-modeline-segments-preset
          ('minimal 'standard)
          ('standard 'full)
          ('full 'minimal)
          (_ 'standard)))
  (iota-modeline-refresh)
  (message "I O T Λ modeline preset: %s" iota-modeline-segments-preset))

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

;;; Configuration Helpers

(defun iota-modeline-add-segment (segment)
  "Add SEGMENT to custom segment list."
  (setq iota-modeline-segments-preset 'custom)
  (push segment iota-modeline-custom-segments)
  (iota-modeline-refresh))

(defun iota-modeline-remove-segment (id)
  "Remove segment with ID from custom list."
  (setq iota-modeline-custom-segments
        (cl-remove id iota-modeline-custom-segments
                   :key #'iota-segment-id))
  (iota-modeline-refresh))

(provide 'iota-modeline)
;;; iota-modeline.el ends here
