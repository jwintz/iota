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
;; - Dynamic modeline separator handling (matches iota-splash)
;;
;; Usage:
;;   (iota-screens-mode 1)                  ; Enable idle detection
;;   (iota-screens-preview 'matrix)         ; Preview animation immediately
;;   (iota-screens-activate)                ; Manual activation
;;   (iota-screens-deactivate)              ; Manual deactivation
;;
;;; Code:

;; Only require what's absolutely needed at load time
(require 'iota-timers)
(require 'iota-faces)

;; Declare functions from modules loaded on-demand
(declare-function iota-box-horizontal-line "iota-box")
(declare-function iota-popup--popup-visible-p "iota-popup")
(declare-function iota-popup--window-popup-p "iota-popup")

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
  'alien  - Alien-life inspired particle flow
  'life   - Conway's Game of Life
  'clock  - Digital clock
  'pipes  - 3D Pipes"
  :type '(choice (const :tag "Matrix Rain" matrix)
                 (const :tag "Particle Flow" alien)
                 (const :tag "Game of Life" life)
                 (const :tag "Digital Clock" clock)
                 (const :tag "3D Pipes" pipes))
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

;;; Major Mode

(define-derived-mode iota-screens-buffer-mode special-mode "IOScreens"
  "Major mode for IOTA screens buffer."
  (setq buffer-read-only t)
  (setq cursor-type nil))

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
    
    ;; Ensure we are excluded from modeline
    (when (boundp 'iota-modeline-excluded-buffers)
      (add-to-list 'iota-modeline-excluded-buffers (regexp-quote iota-screens--buffer-name)))

    ;; Create and display screen saver buffer
    (let ((buffer (get-buffer-create iota-screens--buffer-name)))
      (with-current-buffer buffer
        (unless (eq major-mode 'iota-screens-buffer-mode)
          (iota-screens-buffer-mode)))
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

        ;; Set up buffer-local variables (match iota-splash)
        (setq-local cursor-type nil)
        (setq-local mode-line-format nil)
        (setq-local buffer-read-only t)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local header-line-format nil)
        (setq-local truncate-lines t)  ;; Prevent line wrapping
        (setq-local visible-cursor nil)
        (setq-local iota-screens--last-width (window-body-width))
        (setq-local iota-screens--last-height (window-body-height))

        ;; Activate keymap
        (use-local-map iota-screens-mode-map)

        ;; Set window parameters (match iota-splash behavior)
        ;; Initially hide mode-line, will be shown when minibuffer activates
        (set-window-parameter (selected-window) 'mode-line-format 'none)
        (set-window-parameter (selected-window) 'header-line-format nil)
        
        ;; Hide cursor at window level (like splash)
        (internal-show-cursor (selected-window) nil)

        ;; Setup cleanup hook
        (add-hook 'kill-buffer-hook #'iota-screens--cleanup nil t)))

    ;; Start selected animation
    (message "IOTA Screens: Starting animation '%s'" iota-screens-default-animation)
    (iota-screens--start-animation iota-screens-default-animation)

    ;; Install hooks (NOT pre-command-hook - only 'q' key should exit, like splash)
    (add-hook 'post-command-hook #'iota-screens--check-and-redraw)
    (add-hook 'window-size-change-functions #'iota-screens--on-resize)
    (add-hook 'window-configuration-change-hook #'iota-screens--on-window-config-change)
    (add-hook 'minibuffer-setup-hook #'iota-screens--on-minibuffer-setup)
    (add-hook 'minibuffer-exit-hook #'iota-screens--on-minibuffer-exit)
    (add-hook 'echo-area-clear-hook #'iota-screens--on-echo-area-clear)
    
    ;; Force modeline refresh to remove overlays
    (when (fboundp 'iota-modeline-refresh)
      (iota-modeline-refresh))
      
    (message "IOTA Screens: Active. Press 'q' to exit.")))

(defun iota-screens-deactivate ()
  "Deactivate idle screen saver and restore previous state."
  (interactive)
  (when iota-screens--active
    (message "IOTA Screens: Deactivating...")
    ;; Stop current animation
    (iota-screens--stop-animation)

    ;; Remove hooks
    (remove-hook 'post-command-hook #'iota-screens--check-and-redraw)
    (remove-hook 'window-size-change-functions #'iota-screens--on-resize)
    (remove-hook 'window-configuration-change-hook #'iota-screens--on-window-config-change)
    (remove-hook 'minibuffer-setup-hook #'iota-screens--on-minibuffer-setup)
    (remove-hook 'minibuffer-exit-hook #'iota-screens--on-minibuffer-exit)
    (remove-hook 'echo-area-clear-hook #'iota-screens--on-echo-area-clear)

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

(defun iota-screens--on-resize (_frame)
  "Handle window resize for screen saver.
FRAME is ignored but required by hook signature."
  (when iota-screens--active
    (iota-screens--check-resize)
    (iota-screens--update-separator)))

(defun iota-screens--cleanup ()
  "Cleanup function called when screen buffer is killed."
  (iota-screens--stop-animation)
  (setq iota-screens--active nil)
  ;; Remove hooks
  (remove-hook 'post-command-hook #'iota-screens--check-and-redraw)
  (remove-hook 'window-configuration-change-hook #'iota-screens--on-window-config-change)
  (remove-hook 'minibuffer-setup-hook #'iota-screens--on-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'iota-screens--on-minibuffer-exit)
  (remove-hook 'echo-area-clear-hook #'iota-screens--on-echo-area-clear))

;;; Modeline/Separator Handling

(defun iota-screens--minibuffer-active-p ()
  "Return t if the minibuffer is currently active."
  (and (active-minibuffer-window)
       (minibufferp (window-buffer (active-minibuffer-window)))))

(defun iota-screens--popup-visible-p ()
  "Return t if a popup window (which-key, transient, etc.) is currently visible."
  (and (fboundp 'iota-popup--popup-visible-p)
       (iota-popup--popup-visible-p)))

(defun iota-screens--iota-buffer-p (buffer)
  "Return t if BUFFER is an iota splash or screen buffer."
  (when buffer
    (let ((name (buffer-name buffer)))
      (or (string-match-p "\\*I O T Λ splash\\*" name)
          (string-match-p "\\*I O T Λ screen\\*" name)))))

(defun iota-screens--has-iota-buffer-below-p ()
  "Return t if there is an iota splash/screen window directly below this one."
  (let ((screen-window (get-buffer-window iota-screens--buffer-name)))
    (when (window-live-p screen-window)
      (let* ((edges (window-edges screen-window))
             (bottom (nth 3 edges))
             (left (nth 0 edges))
             (right (nth 2 edges)))
        (catch 'found-iota-below
          (dolist (w (window-list nil 'no-minibuf))
            (unless (eq w screen-window)
              (let* ((w-edges (window-edges w))
                     (w-top (nth 1 w-edges))
                     (w-left (nth 0 w-edges))
                     (w-right (nth 2 w-edges)))
                ;; Check if w is directly below and overlaps horizontally
                (when (and (= w-top bottom)
                           (> (min right w-right) (max left w-left))
                           (iota-screens--iota-buffer-p (window-buffer w)))
                  (throw 'found-iota-below t)))))
          nil)))))

(defun iota-screens--window-at-bottom-p ()
  "Return t if the screen saver window is at the bottom of the frame."
  (let ((screen-window (get-buffer-window iota-screens--buffer-name)))
    (when (window-live-p screen-window)
      (let* ((edges (window-edges screen-window))
             (bottom (nth 3 edges))
             (left (nth 0 edges))
             (right (nth 2 edges)))
        (catch 'found-below
          (dolist (w (window-list nil 'no-minibuf))
            (unless (eq w screen-window)
              (let* ((w-edges (window-edges w))
                     (w-top (nth 1 w-edges))
                     (w-left (nth 0 w-edges))
                     (w-right (nth 2 w-edges)))
                ;; Check if w is below screen-window and overlaps horizontally
                (when (and (= w-top bottom)
                           (> (min right w-right) (max left w-left))
                           (not (and (fboundp 'iota-popup--window-popup-p)
                                     (iota-popup--window-popup-p w))))
                  (throw 'found-below nil)))))
          t)))))

(defun iota-screens--should-show-separator-p ()
  "Return t if separator should be shown for screen saver.
Separator is shown when:
- Minibuffer is active, OR
- Popup is visible AND screen saver is at the bottom, OR
- Another iota splash/screen buffer is directly below."
  (or (iota-screens--minibuffer-active-p)
      (and (iota-screens--popup-visible-p)
           (iota-screens--window-at-bottom-p))
      (iota-screens--has-iota-buffer-below-p)))

(defun iota-screens--get-separator-line (window)
  "Get a separator line string for WINDOW."
  (require 'iota-box)
  (let* ((width (if (window-live-p window)
                    (1- (window-body-width window))
                  79))
         (style (if (boundp 'iota-modeline-box-style)
                    iota-modeline-box-style
                  'rounded))
         (face 'iota-inactive-box-face))
    (iota-box-horizontal-line width style face)))

(defun iota-screens--update-separator ()
  "Update the separator line visibility for screen saver.
Shows separator when minibuffer or popup is active.
Matches the behavior of iota-splash--update-separator."
  (let ((buffer (get-buffer iota-screens--buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (let ((win (get-buffer-window buffer)))
        (when (window-live-p win)
          (if (iota-screens--should-show-separator-p)
              ;; Show separator line when minibuffer/popup is active
              (let ((width (1- (window-body-width win))))
                (with-current-buffer buffer
                  (setq-local mode-line-format
                              `(:eval (iota-screens--get-separator-line ,win))))
                ;; Critical: set window parameter to nil to SHOW mode-line
                (set-window-parameter win 'mode-line-format nil)
                ;; Force redisplay
                (force-mode-line-update t))
            ;; Hide separator line when not needed
            (with-current-buffer buffer
              (setq-local mode-line-format nil))
            ;; Set window parameter to 'none to HIDE mode-line
            (set-window-parameter win 'mode-line-format 'none)
            (force-mode-line-update t)))))))

(defun iota-screens--check-and-redraw ()
  "Check state and update separator."
  (when (and iota-screens--active
             (get-buffer iota-screens--buffer-name))
    (iota-screens--update-separator)))

(defun iota-screens--on-window-config-change ()
  "Handle window configuration changes."
  (when iota-screens--active
    (iota-screens--update-separator)))

(defun iota-screens--on-minibuffer-setup ()
  "Handle minibuffer setup."
  (when iota-screens--active
    (iota-screens--update-separator)))

(defun iota-screens--on-minibuffer-exit ()
  "Handle minibuffer exit."
  (when iota-screens--active
    (run-with-timer 0.01 nil #'iota-screens--update-separator)))

(defun iota-screens--on-echo-area-clear ()
  "Handle echo area clear."
  (when iota-screens--active
    (iota-screens--update-separator)))

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
    ('life
     (require 'iota-screens-life)
     (iota-screens-life-start))
    ('clock
     (require 'iota-screens-clock)
     (iota-screens-clock-start))
    ('pipes
     (require 'iota-screens-pipes)
     (iota-screens-pipes-start))
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
Available animations: matrix, alien, life, clock"
  (interactive
   (list (intern (completing-read "Animation: "
                                  '("matrix" "alien" "life" "clock" "pipes")
                                  nil t))))
  (let ((iota-screens-default-animation animation-type))
    (iota-screens-activate)))

(provide 'iota-screens)

;;; iota-screens.el ends here
