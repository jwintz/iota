;;; iota-window.el --- I O T Λ window focus management -*- lexical-binding: t -*-

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
- `plain': Single vertical bar spanning full line height
- `hidden': No visible dividers (uses default background)"
  :type '(choice (const :tag "Plain vertical bar" plain)
                 (const :tag "Hidden" hidden))
  :group 'iota)

(defcustom iota-window-animate-transitions t
  "Enable animated transitions when switching windows.
When non-nil, modeline and accent faces will animate smoothly
between active and inactive states."
  :type 'boolean
  :group 'iota)

(defcustom iota-window-transition-duration 0.5
  "Duration of window focus transition animations in seconds."
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
  "Configure window dividers based on `iota-window-divider-style'."
  (pcase iota-window-divider-style
    ('plain
     ;; Plain vertical bar: use a continuous vertical line character
     (let ((table (or standard-display-table (make-display-table))))
       ;; Set vertical border character to box drawing character
       (set-display-table-slot table 'vertical-border (make-glyph-code ?│ 'vertical-border))
       (setq standard-display-table table))
     ;; Set vertical-border face color to inactive box color
     (let ((fg (face-attribute 'iota-inactive-box-face :foreground nil t)))
       (when (or (eq fg 'unspecified) (not fg))
         (setq fg "grey30"))
       (set-face-attribute 'vertical-border nil 
                           :foreground fg
                           :background 'unspecified)))
    ('hidden
     ;; Hidden: set vertical border to space with transparent face
     (let ((table (or standard-display-table (make-display-table))))
       ;; Set vertical border character to space
       (set-display-table-slot table 'vertical-border (make-glyph-code ?\s 'vertical-border))
       (setq standard-display-table table))
     ;; Set vertical-border face to fully transparent
     (set-face-attribute 'vertical-border nil 
                         :foreground 'unspecified 
                         :background 'unspecified))))(defun iota-window--restore-dividers ()
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

(defun iota-window--on-selection-change ()
  "Handle window selection change.
Updates window parameters and triggers modeline updates."
  (let ((current-window (selected-window)))
    (when (not (eq current-window iota-window--last-selected))

      (iota-window--cancel-animations)

      (let ((inactive-win iota-window--last-selected)
            (active-win current-window))

        (when (and iota-animate-enabled iota-window-animate-transitions iota-window-animate-modeline)
          (let ((active-color (face-attribute 'iota-active-box-face :foreground nil t))
                (inactive-color (face-attribute 'iota-inactive-box-face :foreground nil t)))
            (when (and active-color inactive-color)
              (when (and inactive-win (window-live-p inactive-win))
                (iota-window--animate-window-modeline inactive-win active-color inactive-color))
              (iota-window--animate-window-modeline active-win inactive-color active-color))))

        (dolist (win (window-list nil 'no-minibuf))
          (set-window-parameter win 'iota-active nil))
        (set-window-parameter active-win 'iota-active t)
        (setq iota-window--last-selected active-win)))))

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
        (setq iota-window--last-selected (selected-window))
        (add-hook 'window-selection-change-functions
                  (lambda (_frame) (iota-window--on-selection-change)))
        (add-hook 'buffer-list-update-hook #'iota-window--on-selection-change)
        ;; Force modeline update
        (force-mode-line-update t)
        (message "IOTA window mode enabled"))
    ;; Disable window focus tracking
    (remove-hook 'window-selection-change-functions
                 (lambda (_frame) (iota-window--on-selection-change)))
    (remove-hook 'buffer-list-update-hook #'iota-window--on-selection-change)
    (iota-window--cancel-animations)
    ;; Restore original divider settings
    (iota-window--restore-dividers)
    ;; Clear window parameters
    (dolist (win (window-list nil 'no-minibuf))
      (set-window-parameter win 'iota-active nil))
    (setq iota-window--last-selected nil)
    (message "IOTA window mode disabled")))

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
    ;; Force window redisplay to show changes immediately
    (redraw-display))
  (message "Window dividers: %s" iota-window-divider-style))

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
    (redraw-display))
  (message "Window dividers: %s" style))

(provide 'iota-window)

;;; iota-window.el ends here
