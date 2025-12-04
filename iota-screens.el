;;; iota-screens.el --- Idle screen saver system for IOTA -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Version: 0.1.0
;; Keywords: screensaver, animation
;; URL: https://github.com/yourusername/iota

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Idle screen saver system with ASCII art animations.
;; Inspired by cmatrix and other terminal screen savers.
;;
;; Features:
;; - Automatic activation after idle timeout
;; - Multiple animation types
;; - Proper cleanup and timer management
;; - Window configuration preservation
;;
;; Usage:
;;   (iota-screens-mode 1)                  ; Enable idle detection
;;   (iota-screens-preview 'matrix)         ; Preview animation immediately
;;   (iota-screens-activate)                ; Manual activation
;;   (iota-screens-deactivate)              ; Manual deactivation

;;; Code:

(require 'iota-timers)
(require 'iota-faces)

;;; Customization

(defgroup iota-screens nil
  "Idle screen saver configuration."
  :group 'iota
  :prefix "iota-screens-")

(defcustom iota-screens-idle-timeout 300
  "Seconds of idle time before screen saver activates.
Set to nil to disable automatic activation."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'iota-screens)

(defcustom iota-screens-default-animation 'matrix
  "Default animation to use for idle screen.
Available options:
  'matrix - Matrix rain (cmatrix-style)
  'alien  - Alien-life inspired particle flow"
  :type '(choice (const :tag "Matrix Rain" matrix)
                 (const :tag "Particle Flow" alien))
  :group 'iota-screens)

;;; State Management

(defvar iota-screens--active nil
  "Whether idle screen is currently active.")

(defvar iota-screens--saved-config nil
  "Saved window configuration to restore on deactivation.")

(defvar iota-screens--buffer-name "*I O T Λ screen*"
  "Name of the idle screen buffer.")

(defvar iota-screens--current-animation nil
  "Currently active animation module.")

(defvar-local iota-screens--last-width nil
  "Last known window width for resize detection.")

(defvar-local iota-screens--last-height nil
  "Last known window height for resize detection.")

;;; Keymap

(defvar iota-screens-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'iota-screens-deactivate)
    (define-key map (kbd "Q") 'iota-screens-deactivate)
    (define-key map (kbd "ESC ESC ESC") 'iota-screens-deactivate)
    map)
  "Keymap for iota-screens buffer.
'q', 'Q', and ESC ESC ESC will exit the screen saver.")

;;; Core Functions

(defun iota-screens--check-resize ()
  "Check if window was resized and restart animation if needed."
  (when (and iota-screens--active
             (buffer-live-p (get-buffer iota-screens--buffer-name)))
    (with-current-buffer iota-screens--buffer-name
      (let ((width (window-body-width))
            (height (window-body-height)))
        (when (or (not (equal width iota-screens--last-width))
                  (not (equal height iota-screens--last-height)))
          (setq-local iota-screens--last-width width)
          (setq-local iota-screens--last-height height)
          (message "IOTA Screens: Window resized to %dx%d, restarting animation" width height)
          ;; Save animation type before stopping
          (let ((anim-type iota-screens--current-animation))
            ;; Restart animation with new dimensions
            (iota-screens--stop-animation)
            (iota-screens--start-animation anim-type)))))))

(defun iota-screens-activate ()
  "Activate idle screen saver.
Creates a full-screen buffer and starts the configured animation."
  (interactive)
  (unless iota-screens--active
    (message "IOTA Screens: Activating...")
    (setq iota-screens--active t)
    (setq iota-screens--saved-config (current-window-configuration))

    ;; Create and display screen saver buffer
    (let ((buffer (get-buffer-create iota-screens--buffer-name)))
      (switch-to-buffer buffer)
      ;; Debug window dimensions using multiple methods
      (let* ((win (selected-window))
             (body-h (window-body-height win))
             (total-h (window-total-height win))
             (pixel-h (window-pixel-height win))
             (line-h (frame-char-height))
             (calc-h (/ pixel-h line-h)))
        (message "IOTA Screens DEBUG: body-h=%d total-h=%d pixel-h=%d line-h=%d calc-h=%d"
                 body-h total-h pixel-h line-h calc-h))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; Insert initial placeholder
          (insert "IOTA Screens Initializing...\n"))

        ;; Set up buffer-local variables
        (setq-local cursor-type nil)
        (setq-local mode-line-format nil)
        (setq-local buffer-read-only t)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local iota-screens--last-width (window-body-width))
        (setq-local iota-screens--last-height (window-body-height))

        ;; Activate keymap
        (use-local-map iota-screens-mode-map)

        ;; Set window parameters
        (set-window-parameter (selected-window) 'mode-line-format 'none)

        ;; Setup cleanup hook
        (add-hook 'kill-buffer-hook #'iota-screens--cleanup nil t)))

    ;; Start selected animation
    (message "IOTA Screens: Starting animation '%s'" iota-screens-default-animation)
    (iota-screens--start-animation iota-screens-default-animation)

    ;; Install hooks
    (add-hook 'pre-command-hook #'iota-screens--on-activity)
    (add-hook 'window-size-change-functions #'iota-screens--on-resize)
    (message "IOTA Screens: Active. Press 'q' to exit.")))

(defun iota-screens-deactivate ()
  "Deactivate idle screen saver and restore previous state."
  (interactive)
  (when iota-screens--active
    (message "IOTA Screens: Deactivating...")
    ;; Stop current animation
    (iota-screens--stop-animation)

    ;; Remove hooks
    (remove-hook 'pre-command-hook #'iota-screens--on-activity)
    (remove-hook 'window-size-change-functions #'iota-screens--on-resize)

    ;; Kill buffer
    (when (get-buffer iota-screens--buffer-name)
      (kill-buffer iota-screens--buffer-name))

    ;; Restore window configuration
    (when iota-screens--saved-config
      (set-window-configuration iota-screens--saved-config)
      (setq iota-screens--saved-config nil))

    (setq iota-screens--active nil)
    (message "IOTA Screens: Deactivated")

    ;; Restart idle detection if mode is enabled
    (when iota-screens-mode
      (iota-screens--setup-idle-detection))))

(defun iota-screens--on-activity ()
  "Handle user activity - deactivate screen saver."
  (when iota-screens--active
    (iota-screens-deactivate)))

(defun iota-screens--on-resize (_frame)
  "Handle window resize for screen saver.
FRAME is ignored but required by hook signature."
  (when iota-screens--active
    (iota-screens--check-resize)))

(defun iota-screens--cleanup ()
  "Cleanup function called when screen buffer is killed."
  (iota-screens--stop-animation)
  (setq iota-screens--active nil))

;;; Animation Management

(defun iota-screens--start-animation (animation-type)
  "Start ANIMATION-TYPE in the current screen buffer."
  (setq iota-screens--current-animation animation-type)
  (pcase animation-type
    ('matrix
     (require 'iota-screens-matrix)
     (iota-screens-matrix-start))
    ('alien
     (require 'iota-screens-alien)
     (iota-screens-alien-start))
    (_ (error "Unknown animation type: %s" animation-type))))

(defun iota-screens--stop-animation ()
  "Stop current animation."
  (when iota-screens--current-animation
    ;; Cancel all screen-related timers
    (iota-timers-cancel-group 'screens)
    (setq iota-screens--current-animation nil)))

;;; Idle Detection

(defun iota-screens--setup-idle-detection ()
  "Setup idle timer to detect when to activate screen saver."
  (when iota-screens-idle-timeout
    (iota-timers-run-with-idle-timer
     'screens-detect
     iota-screens-idle-timeout
     t  ; repeat
     #'iota-screens-activate)))

;;; Minor Mode

;;;###autoload
(define-minor-mode iota-screens-mode
  "Toggle idle screen saver functionality.
When enabled, the screen saver activates after `iota-screens-idle-timeout'
seconds of idle time."
  :global t
  :group 'iota-screens
  :lighter " Screens"
  (if iota-screens-mode
      (iota-screens--setup-idle-detection)
    (progn
      (iota-timers-cancel 'screens-detect)
      (when iota-screens--active
        (iota-screens-deactivate)))))

;;; Commands

;;;###autoload
(defun iota-screens-preview (animation-type)
  "Preview ANIMATION-TYPE immediately without waiting for idle timeout.
Available animations: matrix, alien"
  (interactive
   (list (intern (completing-read "Animation: "
                                   '("matrix" "alien")
                                   nil t))))
  (let ((iota-screens-default-animation animation-type))
    (iota-screens-activate)))

(provide 'iota-screens)

;;; iota-screens.el ends here
