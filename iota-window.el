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
  ;; Ensure we have a display table
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))
  (pcase iota-window-divider-style
    ('plain
     ;; Plain vertical bar: use full-height box drawing vertical line
     ;; Use │ (U+2502 BOX DRAWINGS LIGHT VERTICAL) - same as iota-box.el uses
     ;; This creates a continuous vertical line when used on every row
     ;; Get the vertical character from iota-box for consistency
     (let ((vert-char (if (boundp 'iota-box-default-style)
                          (string-to-char (plist-get (iota-box-get-chars iota-box-default-style) :vertical))
                        ?│)))  ; U+2502 fallback
       ;; Set display table slot - just the character code
       (set-display-table-slot standard-display-table 'vertical-border vert-char))
     ;; Set vertical-border face foreground color to inactive box color
     ;; Background must remain unspecified for transparent terminals
     (let ((fg (face-attribute 'iota-inactive-box-face :foreground nil t)))
       (when (or (eq fg 'unspecified) (not fg))
         (setq fg "grey30"))
       (set-face-attribute 'vertical-border nil
                           :foreground fg
                           :background 'unspecified
                           :inherit nil)
       ;; Also set window-divider faces for GUI frames
       (when (facep 'window-divider)
         (set-face-attribute 'window-divider nil :foreground fg :background 'unspecified))
       (when (facep 'window-divider-first-pixel)
         (set-face-attribute 'window-divider-first-pixel nil :foreground fg :background 'unspecified))
       (when (facep 'window-divider-last-pixel)
         (set-face-attribute 'window-divider-last-pixel nil :foreground fg :background 'unspecified))))
    ('hidden
     ;; Hidden: make divider completely invisible
     ;; In terminal Emacs, use a space character AND set foreground to match default
     ;; Set display table to use space character
     (set-display-table-slot standard-display-table 'vertical-border ?\s)
     ;; For hidden mode, we need the foreground to match the default foreground
     ;; because with unspecified, it might inherit a visible color
     ;; Use the default foreground color so even if a character shows, it's invisible
     (let ((fg (face-attribute 'default :foreground nil t)))
       (when (or (eq fg 'unspecified) (not fg))
         (setq fg "#ffffff"))
       ;; Actually for terminal, we want to match the background to be truly invisible
       ;; But since background is unspecified (transparent), we use default foreground
       ;; which will make any character blend with text - but space shows nothing anyway
       (set-face-attribute 'vertical-border nil
                           :foreground fg
                           :background 'unspecified
                           :inherit nil)
       (when (facep 'window-divider)
         (set-face-attribute 'window-divider nil
                             :foreground fg
                             :background 'unspecified))
       (when (facep 'window-divider-first-pixel)
         (set-face-attribute 'window-divider-first-pixel nil
                             :foreground fg
                             :background 'unspecified))
       (when (facep 'window-divider-last-pixel)
         (set-face-attribute 'window-divider-last-pixel nil
                             :foreground fg
                             :background 'unspecified)))))
  ;; Apply display table to all frames
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'display-table standard-display-table))
  ;; Also update all existing buffer-local display tables
  (iota-window--update-buffer-display-tables)
  ;; Force redisplay to apply changes
  (force-mode-line-update t)
  (redraw-display))

(defun iota-window--update-buffer-display-tables ()
  "Update vertical-border slot in all buffer-local display tables.
This ensures buffers with their own display table also get the correct divider."
  (let ((vert-char (display-table-slot standard-display-table 'vertical-border)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when buffer-display-table
          (set-display-table-slot buffer-display-table 'vertical-border vert-char))))))

(defun iota-window--setup-buffer-display-table ()
  "Ensure buffer-local display table has correct vertical-border.
Called when entering a buffer to fix display tables created by other modes."
  (when (and iota-window-mode buffer-display-table)
    (let ((vert-char (display-table-slot standard-display-table 'vertical-border)))
      (set-display-table-slot buffer-display-table 'vertical-border vert-char))))

(defun iota-window--fix-current-buffer-display-table ()
  "Fix the current buffer's display table after major mode change.
This runs after modes like outline-minor-mode create their display tables."
  (when iota-window-mode
    ;; Use a short timer to run after all mode hooks complete
    (run-with-timer 0.01 nil
                    (lambda (buf)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (when buffer-display-table
                            (let ((vert-char (display-table-slot standard-display-table 'vertical-border)))
                              (set-display-table-slot buffer-display-table 'vertical-border vert-char))))))
                    (current-buffer))))

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
        ;; Fix buffer-local display tables when switching buffers
        (add-hook 'window-buffer-change-functions
                  #'iota-window--on-buffer-change)
        ;; Fix buffer-local display tables after major mode changes
        ;; (outline-minor-mode and others create display tables in mode hooks)
        (add-hook 'after-change-major-mode-hook
                  #'iota-window--fix-current-buffer-display-table)
        ;; Re-apply divider settings after theme loads (themes can override face settings)
        (advice-add 'load-theme :after #'iota-window--on-theme-load)
        ;; Force modeline update
        (force-mode-line-update t))
    ;; Disable window focus tracking
    (remove-hook 'window-selection-change-functions
                 #'iota-window--on-selection-change)
    (remove-hook 'window-buffer-change-functions
                 #'iota-window--on-buffer-change)
    (remove-hook 'after-change-major-mode-hook
                 #'iota-window--fix-current-buffer-display-table)
    (advice-remove 'load-theme #'iota-window--on-theme-load)
    (iota-window--cancel-animations)
    ;; Restore original divider settings
    (iota-window--restore-dividers)
    ;; Clear window parameters
    (dolist (win (window-list nil 'no-minibuf))
      (set-window-parameter win 'iota-active nil))
    (setq iota-window--last-selected nil)
    (message "IOTA window mode disabled")))

(defun iota-window--on-theme-load (&rest _args)
  "Re-apply divider configuration after a theme is loaded.
Themes can override face settings, so we need to re-apply our configuration."
  (when iota-window-mode
    ;; Small delay to let iota-theme-transparent process first
    (run-with-timer 0.1 nil #'iota-window--configure-dividers)))

(defun iota-window--on-buffer-change (frame-or-window)
  "Handle buffer change in FRAME-OR-WINDOW.
Fixes buffer-local display tables that don't have vertical-border set.
The hook can be called with either a frame or window depending on context."
  (when iota-window-mode
    (let ((windows (cond
                    ((framep frame-or-window) (window-list frame-or-window))
                    ((windowp frame-or-window) (list frame-or-window))
                    (t nil))))
      (dolist (win windows)
        (let ((buf (window-buffer win)))
          (when (and buf (buffer-live-p buf))
            (with-current-buffer buf
              (when buffer-display-table
                (let ((vert-char (display-table-slot standard-display-table 'vertical-border)))
                  (unless (equal (display-table-slot buffer-display-table 'vertical-border) vert-char)
                    (set-display-table-slot buffer-display-table 'vertical-border vert-char)))))))))))

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
