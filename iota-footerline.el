;;; iota-footerline.el --- I O T Λ footer line separator -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: modeline, minibuffer, faces
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; IOTA footerline provides a separator line that appears above the
;; minibuffer area when transient content is displayed. This creates
;; visual separation between the main editor content and:
;;   - Active minibuffer (prompts, completions)
;;   - which-key popup windows
;;   - Warning messages in the echo area
;;   - Other transient minibuffer-area content
;;
;; The footer line uses the same box-drawing style as iota-modeline
;; for visual consistency throughout the interface.
;;
;; Features:
;;   - Automatic detection of minibuffer activation
;;   - which-key popup detection
;;   - Warning buffer visibility detection
;;   - Consistent styling with modeline box decorations
;;   - Lightweight and performant

;;; Code:

(require 'iota-box)
(require 'iota-theme)

;;; Customization

(defgroup iota-footerline nil
  "I O T Λ footer line separator configuration."
  :group 'iota
  :prefix "iota-footerline-")

(defcustom iota-footerline-show-for-minibuffer t
  "Show footer separator when minibuffer is active."
  :type 'boolean
  :group 'iota-footerline)

(defcustom iota-footerline-show-for-which-key t
  "Show footer separator when which-key popup is visible."
  :type 'boolean
  :group 'iota-footerline)

(defcustom iota-footerline-show-for-warnings t
  "Show footer separator when warnings are displayed in echo area."
  :type 'boolean
  :group 'iota-footerline)

(defcustom iota-footerline-excluded-buffers nil
  "List of buffer names (strings or regexps) to exclude from footer line.
Buffers matching these patterns will not show the footer separator."
  :type '(repeat (choice string regexp))
  :group 'iota-footerline)

;;; State

(defvar iota-footerline--overlay nil
  "Overlay for footer separator line.")

(defvar iota-footerline--last-state nil
  "Last known state of footer visibility (t or nil).")

(defvar iota-footerline--warning-timer nil
  "Timer for checking warning buffer visibility.")

;;; Detection Functions

(defun iota-footerline--minibuffer-active-p ()
  "Return t if the minibuffer is currently active."
  (and iota-footerline-show-for-minibuffer
       (active-minibuffer-window)
       (minibufferp (window-buffer (active-minibuffer-window)))))

(defun iota-footerline--which-key-visible-p ()
  "Return t if a which-key window is currently visible."
  (and iota-footerline-show-for-which-key
       (cl-some (lambda (win)
                  (let ((buf-name (buffer-name (window-buffer win))))
                    (or (string-prefix-p " *which-key*" buf-name)
                        (string-prefix-p "*which-key*" buf-name))))
                (window-list))))

(defun iota-footerline--warnings-visible-p ()
  "Return t if warning messages are visible in the echo area or warnings buffer."
  (and iota-footerline-show-for-warnings
       (or
        ;; Check if *Warnings* buffer is visible in a window
        (get-buffer-window "*Warnings*")
        ;; Check if echo area contains warning messages
        (and (current-message)
             (string-match-p "Warning\\|Error" (current-message))))))

(defun iota-footerline--should-show-p ()
  "Return t if footer separator should be shown.
Checks for minibuffer activity, which-key visibility, and warnings."
  (and (not (iota-footerline--buffer-excluded-p))
       (or (iota-footerline--minibuffer-active-p)
           (iota-footerline--which-key-visible-p)
           (iota-footerline--warnings-visible-p))))

(defun iota-footerline--buffer-excluded-p ()
  "Return t if current buffer is excluded from footer line."
  (let ((buf-name (buffer-name)))
    (cl-some (lambda (pattern)
               (string-match-p pattern buf-name))
             iota-footerline-excluded-buffers)))

;;; Rendering

(defun iota-footerline--get-separator-line (width)
  "Get a separator line string of WIDTH characters.
Uses the configured box style from iota-modeline if available.
Uses the same face as the modeline box for consistency."
  (let* ((style (if (boundp 'iota-modeline-box-style)
                    iota-modeline-box-style
                  'rounded))
         (face (if (fboundp 'iota-theme-get-box-face)
                   (iota-theme-get-box-face (selected-window))
                 'iota-active-box-face)))
    (iota-box-horizontal-line width style face)))

(defun iota-footerline--ensure-overlay ()
  "Ensure the footer separator overlay exists.
Creates overlay at the end of the buffer if it doesn't exist."
  (unless (and iota-footerline--overlay
               (overlayp iota-footerline--overlay)
               (overlay-buffer iota-footerline--overlay))
    (with-current-buffer (window-buffer (selected-window))
      (setq iota-footerline--overlay
            (make-overlay (point-max) (point-max) (current-buffer)))
      (overlay-put iota-footerline--overlay 'priority 100))))

(defun iota-footerline--update ()
  "Update footer separator visibility based on current state."
  (let ((should-show (iota-footerline--should-show-p)))
    ;; Only update if state changed (avoid unnecessary redraws)
    (when (not (eq should-show iota-footerline--last-state))
      (setq iota-footerline--last-state should-show)

      (if should-show
          (iota-footerline--show)
        (iota-footerline--hide)))))

(defun iota-footerline--show ()
  "Show the footer separator line."
  ;; Apply to all normal windows (non-minibuffer, non-special)
  (dolist (win (window-list nil 'no-minibuf))
    (let ((buf (window-buffer win)))
      (when (and (buffer-live-p buf)
                 (not (string-prefix-p " " (buffer-name buf))))
        (with-current-buffer buf
          (unless (iota-footerline--buffer-excluded-p)
            (let ((width (1- (window-body-width win))))
              (setq-local mode-line-format
                          `(:propertize (:eval (iota-footerline--get-separator-line ,width))
                                        face (:underline nil :overline nil)))
              ;; Ensure window shows the mode-line
              (set-window-parameter win 'mode-line-format nil))))))))

(defun iota-footerline--hide ()
  "Hide the footer separator line."
  (dolist (win (window-list nil 'no-minibuf))
    (let ((buf (window-buffer win)))
      (when (and (buffer-live-p buf)
                 (not (string-prefix-p " " (buffer-name buf))))
        (with-current-buffer buf
          ;; Restore to default behavior or iota-modeline's separator
          (when (and (boundp 'iota-modeline-mode)
                     iota-modeline-mode
                     (fboundp 'iota-modeline--update-separator-lines))
            ;; Let iota-modeline handle the mode-line
            (kill-local-variable 'mode-line-format)
            (iota-modeline--update-separator-lines))
          ;; Update window parameter
          (when (iota-modeline--window-is-at-bottom-p win)
            (set-window-parameter win 'mode-line-format nil)))))))

;;; Hooks

(defun iota-footerline--on-minibuffer-setup ()
  "Handle minibuffer setup to show footer separator."
  (iota-footerline--update))

(defun iota-footerline--on-minibuffer-exit ()
  "Handle minibuffer exit to potentially hide footer separator."
  ;; Delay slightly to allow minibuffer to fully close
  (run-with-timer 0.01 nil #'iota-footerline--update))

(defun iota-footerline--on-window-config-change ()
  "Handle window configuration changes (e.g., which-key appearing)."
  (iota-footerline--update))

(defun iota-footerline--on-post-command ()
  "Handle post-command to check for warnings or message changes.
This is lightweight and only checks message content."
  ;; Only check if warnings feature is enabled
  (when iota-footerline-show-for-warnings
    (iota-footerline--update)))

(defun iota-footerline--check-warnings ()
  "Periodic check for warning buffer visibility.
This runs on a timer to catch warnings that appear without commands."
  (when (and iota-footerline-mode
             iota-footerline-show-for-warnings)
    (iota-footerline--update)))

;;; Setup and Teardown

(defun iota-footerline--setup ()
  "Set up footer line hooks and state."
  (setq iota-footerline--last-state nil)

  ;; Add hooks
  (add-hook 'minibuffer-setup-hook #'iota-footerline--on-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'iota-footerline--on-minibuffer-exit)
  (add-hook 'window-configuration-change-hook #'iota-footerline--on-window-config-change)
  (add-hook 'post-command-hook #'iota-footerline--on-post-command)

  ;; Start periodic warning check timer (every 2 seconds, non-intrusive)
  (setq iota-footerline--warning-timer
        (run-with-timer 2 2 #'iota-footerline--check-warnings))

  ;; Initial update
  (iota-footerline--update))

(defun iota-footerline--teardown ()
  "Tear down footer line hooks and cleanup."
  ;; Remove hooks
  (remove-hook 'minibuffer-setup-hook #'iota-footerline--on-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'iota-footerline--on-minibuffer-exit)
  (remove-hook 'window-configuration-change-hook #'iota-footerline--on-window-config-change)
  (remove-hook 'post-command-hook #'iota-footerline--on-post-command)

  ;; Cancel timer
  (when iota-footerline--warning-timer
    (cancel-timer iota-footerline--warning-timer)
    (setq iota-footerline--warning-timer nil))

  ;; Hide separator
  (setq iota-footerline--last-state nil)
  (iota-footerline--hide)

  ;; Clean up overlay
  (when (and iota-footerline--overlay
             (overlayp iota-footerline--overlay))
    (delete-overlay iota-footerline--overlay)
    (setq iota-footerline--overlay nil)))

;;; Minor Mode

;;;###autoload
(define-minor-mode iota-footerline-mode
  "Toggle I O T Λ footer line mode.
When enabled, shows a separator line above the minibuffer area
when transient content (minibuffer, which-key, warnings) is displayed."
  :global t
  :group 'iota-footerline
  :lighter " ιFoot"
  (if iota-footerline-mode
      (iota-footerline--setup)
    (iota-footerline--teardown)))

;;; Interactive Commands

(defun iota-footerline-refresh ()
  "Manually refresh footer line visibility."
  (interactive)
  (setq iota-footerline--last-state nil)
  (iota-footerline--update))

(defun iota-footerline-toggle-minibuffer ()
  "Toggle footer separator for minibuffer."
  (interactive)
  (setq iota-footerline-show-for-minibuffer
        (not iota-footerline-show-for-minibuffer))
  (iota-footerline-refresh)
  (message "Footer separator for minibuffer: %s"
           (if iota-footerline-show-for-minibuffer "enabled" "disabled")))

(defun iota-footerline-toggle-which-key ()
  "Toggle footer separator for which-key."
  (interactive)
  (setq iota-footerline-show-for-which-key
        (not iota-footerline-show-for-which-key))
  (iota-footerline-refresh)
  (message "Footer separator for which-key: %s"
           (if iota-footerline-show-for-which-key "enabled" "disabled")))

(defun iota-footerline-toggle-warnings ()
  "Toggle footer separator for warnings."
  (interactive)
  (setq iota-footerline-show-for-warnings
        (not iota-footerline-show-for-warnings))
  (iota-footerline-refresh)
  (message "Footer separator for warnings: %s"
           (if iota-footerline-show-for-warnings "enabled" "disabled")))

(provide 'iota-footerline)
;;; iota-footerline.el ends here
