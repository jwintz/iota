;;; iota-window.el --- I O T Λ window focus management -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Version: 0.1.0
;; Keywords: faces, windows, animation
;; URL: https://github.com/yourusername/iota

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Window focus management and animations for IOTA.
;; Provides smooth transitions between active and inactive window states
;; with animated face changes for visual feedback.

;;; Code:

(require 'iota-theme)
(require 'iota-animate)
(require 'iota-modeline)

;;; Configuration

(defcustom iota-window-divider-style 'plain
  "Style for window dividers.
- `plain': Thin vertical line using box-drawing character (│)
- `hidden': Invisible dividers (foreground matches background)"
  :type '(choice (const :tag "Plain vertical line" plain)
                 (const :tag "Hidden" hidden))
  :group 'iota)

(defcustom iota-window-animate-transitions nil
  "Enable animated transitions when switching windows.
When non-nil, modeline and accent faces will animate smoothly
between active and inactive states.
Note: Animations can cause lag, especially over SSH. Disabled by default."
  :type 'boolean
  :group 'iota)

(defcustom iota-window-transition-duration 0.15
  "Duration of window focus transition animations in seconds.
Shorter durations are less laggy. Only used when `iota-window-animate-transitions' is non-nil."
  :type 'float
  :group 'iota)

(defcustom iota-window-pulse-on-activate t
  "Pulse accent elements when activating a window.
Provides additional visual feedback beyond the fade animation."
  :type 'boolean
  :group 'iota)

(defcustom iota-window-animate-modeline t
  "Animate modeline face changes on window focus.
When enabled, modeline will fade between active/inactive colors."
  :type 'boolean
  :group 'iota)

(defcustom iota-window-animate-accents t
  "Animate accent face changes on window focus.
When enabled, accent elements will fade between active/inactive colors."
  :type 'boolean
  :group 'iota)

;;; State

(defvar iota-window--last-selected nil
  "Previously selected window.")

(defvar iota-window--active-animations nil
  "List of animation IDs for window transitions.")

(defvar iota-window--original-divider-settings nil
  "Storage for original window divider settings.")

(defvar iota-window--original-display-table nil
  "Storage for original display table settings.")

;;; Window Divider Configuration

(defun iota-window--configure-dividers ()
  "Configure window dividers based on `iota-window-divider-style'.
This function configures the display table character and face foreground.
Background handling is delegated to `iota-theme-transparent' for terminal compatibility."
  (message "[IOTA-WINDOW] Configuring dividers: style=%s" iota-window-divider-style)
  (message "[IOTA-WINDOW] Before: display-table=%s"
           (if standard-display-table "exists" "nil"))
  (message "[IOTA-WINDOW] Before: vertical-border face fg=%s bg=%s"
           (face-attribute 'vertical-border :foreground nil t)
           (face-attribute 'vertical-border :background nil t))
  (pcase iota-window-divider-style
    ('plain
     ;; Plain vertical bar: use box drawing vertical line for continuous appearance
     (message "[IOTA-WINDOW] Setting PLAIN style with │ character")
     (let ((table (or standard-display-table (make-display-table))))
       ;; Use │ (U+2502 BOX DRAWINGS LIGHT VERTICAL) for a thin vertical line
       (set-display-table-slot table 'vertical-border (make-glyph-code ?│ 'vertical-border))
       (setq standard-display-table table))
     ;; Set vertical-border face foreground color to inactive box color
     ;; Do NOT set background - let iota-theme-transparent handle it for terminal compatibility
     (let ((fg (face-attribute 'iota-inactive-box-face :foreground nil t)))
       (when (or (eq fg 'unspecified) (not fg))
         (setq fg "grey30"))
       (message "[IOTA-WINDOW] Setting vertical-border foreground to: %s" fg)
       (set-face-attribute 'vertical-border nil :foreground fg)
       ;; Also set window-divider faces for GUI frames
       (when (facep 'window-divider)
         (set-face-attribute 'window-divider nil :foreground fg))
       (when (facep 'window-divider-first-pixel)
         (set-face-attribute 'window-divider-first-pixel nil :foreground fg))
       (when (facep 'window-divider-last-pixel)
         (set-face-attribute 'window-divider-last-pixel nil :foreground fg))))
    ('hidden
     ;; Hidden: make divider invisible by using a space character
     ;; The key is using a space in the display table - color doesn't matter for spaces
     (message "[IOTA-WINDOW] Setting HIDDEN style with space character")
     (let ((table (or standard-display-table (make-display-table))))
       ;; Use a regular space - it's inherently invisible
       (set-display-table-slot table 'vertical-border (make-glyph-code ?\s))
       (setq standard-display-table table))
     (message "[IOTA-WINDOW] Display table slot set to space")
     ;; For the face, we still need to handle it for GUI frames and edge cases
     ;; In terminal: spaces are invisible regardless of color
     ;; In GUI: match fg/bg to make it invisible
     (let ((bg (face-attribute 'default :background nil t)))
       (message "[IOTA-WINDOW] default background = %s (type: %s)" bg (type-of bg))
       ;; Check for unspecified background (can be symbol or string)
       (when (or (eq bg 'unspecified)
                 (not bg)
                 (equal bg "unspecified-bg")
                 (and (stringp bg) (string-match-p "unspecified" bg)))
         ;; In transparent terminal, use a dark color that blends with typical dark terminals
         (message "[IOTA-WINDOW] Background is unspecified/transparent, using #000000")
         (setq bg "#000000"))
       (message "[IOTA-WINDOW] Setting vertical-border fg to: %s (to match bg)" bg)
       ;; Set face attributes - spaces won't show these anyway in terminal
       ;; but it helps with GUI frames
       (set-face-attribute 'vertical-border nil
                           :foreground bg
                           :background 'unspecified)
       (when (facep 'window-divider)
         (set-face-attribute 'window-divider nil
                             :foreground bg
                             :background 'unspecified))
       (when (facep 'window-divider-first-pixel)
         (set-face-attribute 'window-divider-first-pixel nil
                             :foreground bg
                             :background 'unspecified))
       (when (facep 'window-divider-last-pixel)
         (set-face-attribute 'window-divider-last-pixel nil
                             :foreground bg
                             :background 'unspecified)))
     ;; Force redisplay
     (force-mode-line-update t)
     (redraw-display)))
  ;; Final state
  (message "[IOTA-WINDOW] After: vertical-border face fg=%s bg=%s"
           (face-attribute 'vertical-border :foreground nil t)
           (face-attribute 'vertical-border :background nil t))
  (message "[IOTA-WINDOW] After: display-table vertical-border slot = %s"
           (when standard-display-table
             (display-table-slot standard-display-table 'vertical-border)))
  (message "[IOTA-WINDOW] Configuration complete for style: %s" iota-window-divider-style))

(defun iota-window--restore-dividers ()
  "Restore original window divider settings."
  (when iota-window--original-display-table
    (setq standard-display-table iota-window--original-display-table))
  (when iota-window--original-divider-settings
    (let ((fg (plist-get iota-window--original-divider-settings :foreground)))
      (when fg
        (set-face-attribute 'vertical-border nil :foreground fg)))))

;;; Animation Helpers

(defun iota-window--cancel-animations ()
  "Cancel all active window transition animations."
  (dolist (anim-id iota-window--active-animations)
    (iota-animate-stop anim-id))
  (setq iota-window--active-animations nil))

(defun iota-window--animate-window-modeline (window from to)
  "Animate WINDOW modeline from FROM color to TO color.
Now that modal post-command-hook is removed, animations work smoothly."
  (set-window-parameter window 'iota-animation-face-spec `(:foreground ,from))
  (iota-modeline--update-overlay window)
  (let ((anim-id (iota-animate-start
                  iota-window-transition-duration
                  (lambda (progress)
                    (let* ((color (iota-animate-color-lerp from to progress))
                           (spec `(:foreground ,color)))
                      (set-window-parameter window 'iota-animation-face-spec spec)
                      (iota-modeline--update-overlay window)))
                  #'iota-animate-ease-in-out-quad
                  (lambda ()
                    (set-window-parameter window 'iota-animation-face-spec nil)
                    (iota-modeline--update-overlay window)))))
    (when anim-id
      (add-to-list 'iota-window--active-animations anim-id))))

;;; Window Selection Hook

(defun iota-window--on-selection-change (&optional _frame)
  "Handle window selection change.
Updates window parameters for active/inactive face selection.
This is intentionally lightweight to avoid lag on window switches."
  (let ((current-window (selected-window)))
    ;; Skip minibuffer windows
    (when (and (not (minibufferp (window-buffer current-window)))
               (not (eq current-window iota-window--last-selected)))
      (let ((prev-window iota-window--last-selected))
        ;; Update window parameters for active/inactive detection
        ;; This is what iota-theme-window-active-p checks
        (dolist (win (window-list nil 'no-minibuf))
          (set-window-parameter win 'iota-active nil))
        (set-window-parameter current-window 'iota-active t)
        
        ;; Optional animations (disabled by default for performance)
        (when (and iota-animate-enabled 
                   iota-window-animate-transitions 
                   iota-window-animate-modeline)
          (iota-window--cancel-animations)
          (let ((active-color (face-attribute 'iota-active-box-face :foreground nil t))
                (inactive-color (face-attribute 'iota-inactive-box-face :foreground nil t)))
            (when (and active-color inactive-color
                       (stringp active-color) (stringp inactive-color))
              (when (and prev-window (window-live-p prev-window))
                (iota-window--animate-window-modeline 
                 prev-window active-color inactive-color))
              (iota-window--animate-window-modeline 
               current-window inactive-color active-color))))
        
        (setq iota-window--last-selected current-window)
        ;; Update only the affected windows for better performance
        ;; This avoids re-rendering all windows on every switch
        (when (fboundp 'iota-modeline--update-windows)
          (let ((windows-to-update (list current-window)))
            (when (and prev-window (window-live-p prev-window))
              (push prev-window windows-to-update))
            (iota-modeline--update-windows windows-to-update)))))))

;;; Minor Mode

(defvar iota-window--original-hooks nil
  "Storage for original hook values.")

;;;###autoload
(define-minor-mode iota-window-mode
  "Minor mode for IOTA window focus management.
When enabled, tracks window active/inactive state for visual distinction."
  :global t
  :group 'iota
  :lighter " ιWin"
  (if iota-window-mode
      (progn
        ;; Save original divider settings
        (setq iota-window--original-display-table (copy-sequence standard-display-table)
              iota-window--original-divider-settings
              (list :foreground (face-attribute 'vertical-border :foreground nil t)))
        ;; Configure window dividers
        (iota-window--configure-dividers)
        ;; Initialize window parameters
        (dolist (win (window-list nil 'no-minibuf))
          (set-window-parameter win 'iota-active
                                (eq win (selected-window))))
        ;; Enable window focus tracking
        ;; Use only window-selection-change-functions - it's efficient and sufficient
        ;; Do NOT use buffer-list-update-hook as it fires too frequently and causes lag
        (setq iota-window--last-selected (selected-window))
        (add-hook 'window-selection-change-functions
                  #'iota-window--on-selection-change)
        ;; Re-apply divider settings after theme loads (themes can override face settings)
        (advice-add 'load-theme :after #'iota-window--on-theme-load)
        ;; Force modeline update
        (force-mode-line-update t))
    ;; Disable window focus tracking
    (remove-hook 'window-selection-change-functions
                 #'iota-window--on-selection-change)
    (advice-remove 'load-theme #'iota-window--on-theme-load)
    (iota-window--cancel-animations)
    ;; Restore original divider settings
    (iota-window--restore-dividers)
    ;; Clear window parameters
    (dolist (win (window-list nil 'no-minibuf))
      (set-window-parameter win 'iota-active nil))
    (setq iota-window--last-selected nil)
    (message "IOTA window mode disabled")))

(defun iota-window--on-theme-load (&rest args)
  "Re-apply divider configuration after a theme is loaded.
Themes can override face settings, so we need to re-apply our configuration."
  (message "[IOTA-WINDOW] Theme load detected: %s" (car args))
  (message "[IOTA-WINDOW] iota-window-mode = %s" iota-window-mode)
  (when iota-window-mode
    (message "[IOTA-WINDOW] Scheduling divider reconfiguration in 0.1s...")
    ;; Small delay to let iota-theme-transparent process first
    (run-with-timer 0.1 nil
                    (lambda ()
                      (message "[IOTA-WINDOW] Timer fired, reconfiguring dividers...")
                      (iota-window--configure-dividers)))))

;;; Interactive Commands

;;;###autoload
(defun iota-window-toggle-animations ()
  "Toggle window transition animations."
  (interactive)
  (setq iota-window-animate-transitions (not iota-window-animate-transitions))
  (message "Window animations %s"
           (if iota-window-animate-transitions "enabled" "disabled")))

;;;###autoload
(defun iota-window-demo-modeline-animation ()
  "Demonstrate modeline animation by toggling active/inactive states.
This animates the modeline between active and inactive colors."
  (interactive)
  (when iota-animate-enabled
    (let ((active-color (face-attribute 'iota-active-box-face :foreground nil t))
          (inactive-color (face-attribute 'iota-inactive-box-face :foreground nil t)))
      (message "Animating modeline to inactive...")
      (iota-window--animate-window-modeline (selected-window) active-color inactive-color)
      (run-with-timer 1.0 nil
                      (lambda ()
                        (message "Animating modeline to active...")
                        (iota-window--animate-window-modeline (selected-window) inactive-color active-color))))))

;;;###autoload
(defun iota-window-pulse-current ()
  "Pulse the current window's modeline for visual feedback."
  (interactive)
  (when iota-animate-enabled
    (iota-animate-pulse 'mode-line
                        :duration 0.5
                        :intensity 0.3
                        :attribute :foreground)
    (iota-animate-pulse 'header-line
                        :duration 0.5
                        :intensity 0.3
                        :attribute :foreground)))

;;;###autoload
(defun iota-window-flash-current ()
  "Flash the current window briefly."
  (interactive)
  (when iota-animate-enabled
    (let ((original-fg (face-attribute 'mode-line :foreground nil t)))
      (when (and original-fg (not (eq original-fg 'unspecified)))
        (iota-animate-face 'mode-line :foreground
                           original-fg "#39bae6"
                           :duration 0.15
                           :finish-fn
                           (lambda ()
                             (iota-animate-face 'mode-line :foreground
                                                "#39bae6" original-fg
                                                :duration 0.15)))))))

;;;###autoload
(defun iota-window-cycle-divider-style ()
  "Cycle through window divider styles."
  (interactive)
  (setq iota-window-divider-style
        (pcase iota-window-divider-style
          ('plain 'hidden)
          ('hidden 'plain)
          (_ 'plain)))
  (when iota-window-mode
    (iota-window--configure-dividers)
    ;; Force all frames to update their display tables
    (dolist (frame (frame-list))
      (with-selected-frame frame
        (force-window-update)))
    ;; Force complete redisplay
    (redraw-display)
    (redisplay t))
  (message "Window dividers: %s (display-table: %s, vertical-border fg: %s)"
           iota-window-divider-style
           (if standard-display-table "set" "nil")
           (face-attribute 'vertical-border :foreground nil t)))

;;;###autoload
(defun iota-window-reload ()
  "Reload iota-window.el and reapply current divider style.
Useful after making changes to the window divider code."
  (interactive)
  (let ((was-enabled iota-window-mode)
        (current-style iota-window-divider-style))
    (when was-enabled
      (iota-window-mode -1))
    (unload-feature 'iota-window t)
    (require 'iota-window)
    (when was-enabled
      (setq iota-window-divider-style current-style)
      (iota-window-mode 1))
    (message "Reloaded iota-window.el (style: %s)" current-style)))

;;;###autoload
(defun iota-window-set-divider-style (style)
  "Set window divider STYLE.
STYLE can be `plain' or `hidden'."
  (interactive (list (intern (completing-read "Divider style: "
                                               '("plain" "hidden")
                                               nil t))))
  (setq iota-window-divider-style style)
  (when iota-window-mode
    (iota-window--configure-dividers)
    ;; Force all frames to update
    (dolist (frame (frame-list))
      (with-selected-frame frame
        (force-window-update)))
    (redraw-display)
    (redisplay t))
  (message "Window dividers: %s (display-table: %s, vertical-border fg: %s)"
           style
           (if standard-display-table "set" "nil")
           (face-attribute 'vertical-border :foreground nil t)))

(provide 'iota-window)

;;; iota-window.el ends here
