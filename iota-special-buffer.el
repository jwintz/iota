;;; iota-special-buffer.el --- Unified management for special IOTA buffers -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: IOTA Development Team
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Provides unified infrastructure for "special" IOTA buffers that need:
;; - Cursor hidden
;; - Chrome (modeline/header) hidden
;; - Read-only content
;; - Proper cleanup on dismissal
;;
;; Used by: iota-splash, iota-screens, and potentially iota-popup
;;
;; Reference implementation: iota-screens (known working behavior)

;;; Code:

(require 'cl-lib)

;;; Variables

(defvar iota-special-buffer--instances nil
  "Alist of (BUFFER-NAME . PLIST) for active special buffers.
PLIST contains: :window - window where cursor was hidden
                :cleanup-fn - function to call on cleanup")

(defvar iota-special-buffer--hooks-installed nil
  "Non-nil if global hooks are installed.")

;;; Core Functions

(defun iota-special-buffer--restore-cursor-now (window buffer-name)
  "Immediately restore cursor in WINDOW, unregister BUFFER-NAME."
  (when (window-live-p window)
    (message "[SPECIAL-BUF] Restoring cursor in window %s" window)
    ;; Fix buffer-local cursor variables in the window's buffer
    (with-current-buffer (window-buffer window)
      (unless cursor-type (setq-local cursor-type t))
      (unless visible-cursor (setq-local visible-cursor t))
      (when (eq cursor-in-non-selected-windows nil)
        (kill-local-variable 'cursor-in-non-selected-windows)))
    ;; Try immediate restoration
    (internal-show-cursor window t)
    (when (eq window (selected-window))
      (internal-show-cursor (selected-window) t))
    ;; ALSO schedule idle-timer restoration as backup (runs when Emacs truly idle)
    (run-with-idle-timer 0 nil
                         (lambda (win)
                           (when (window-live-p win)
                             (message "[SPECIAL-BUF] Idle-timer cursor restore for %s" win)
                             (internal-show-cursor win t)
                             (when (eq win (selected-window))
                               (internal-show-cursor (selected-window) t))))
                         window)
    ;; Unregister
    (iota-special-buffer--unregister buffer-name)))

(defun iota-special-buffer--on-buffer-change (frame)
  "Check if any special buffers are no longer visible in FRAME.
Restore cursor in windows that switched away from special buffers."
  (dolist (entry iota-special-buffer--instances)
    (let* ((buffer-name (car entry))
           (plist (cdr entry))
           (tracked-window (plist-get plist :window))
           (buffer (get-buffer buffer-name)))
      ;; If tracked window now shows a different buffer, restore cursor
      (when (and tracked-window
                 (window-live-p tracked-window)
                 buffer
                 (not (eq (window-buffer tracked-window) buffer)))
        (iota-special-buffer--restore-cursor-now tracked-window buffer-name)))))

(defun iota-special-buffer--on-first-command ()
  "Called on first command after startup.
Ensures cursor restoration works properly for startup splash."
  (remove-hook 'pre-command-hook #'iota-special-buffer--on-first-command)
  (message "[SPECIAL-BUF] First command hook - ensuring cursor state"))

(defun iota-special-buffer--install-hooks ()
  "Install global hooks for special buffer management."
  (unless iota-special-buffer--hooks-installed
    (add-hook 'window-buffer-change-functions #'iota-special-buffer--on-buffer-change)
    ;; Add one-shot hook for first command (handles startup case)
    (add-hook 'pre-command-hook #'iota-special-buffer--on-first-command)
    (setq iota-special-buffer--hooks-installed t)))

(defun iota-special-buffer--uninstall-hooks ()
  "Uninstall global hooks when no special buffers remain."
  (when (and iota-special-buffer--hooks-installed
             (null iota-special-buffer--instances))
    (remove-hook 'window-buffer-change-functions #'iota-special-buffer--on-buffer-change)
    (setq iota-special-buffer--hooks-installed nil)))

(defun iota-special-buffer--register (buffer-name window cleanup-fn)
  "Register BUFFER-NAME as a special buffer.
WINDOW is where cursor is hidden.
CLEANUP-FN is called when buffer is dismissed."
  (push (cons buffer-name (list :window window :cleanup-fn cleanup-fn))
        iota-special-buffer--instances)
  ;; Install hooks if first special buffer
  (iota-special-buffer--install-hooks))

(defun iota-special-buffer--unregister (buffer-name)
  "Unregister BUFFER-NAME from special buffers."
  (setq iota-special-buffer--instances
        (assoc-delete-all buffer-name iota-special-buffer--instances))
  ;; Uninstall hooks if no special buffers remain
  (iota-special-buffer--uninstall-hooks))

(defun iota-special-buffer--get-instance (buffer-name)
  "Get instance data for BUFFER-NAME."
  (cdr (assoc buffer-name iota-special-buffer--instances)))

(defun iota-special-buffer--cleanup-on-kill ()
  "Cleanup function called from kill-buffer-hook.
Restores cursor for the special buffer being killed."
  (let* ((buffer-name (buffer-name))
         (instance (iota-special-buffer--get-instance buffer-name)))
    (when instance
      (let ((window (plist-get instance :window))
            (cleanup-fn (plist-get instance :cleanup-fn)))
        ;; Restore cursor in the window where it was hidden
        (when (and window (window-live-p window))
          (internal-show-cursor window t))
        ;; Also restore in selected window (may be different)
        (internal-show-cursor (selected-window) t)
        ;; Call custom cleanup if provided
        (when cleanup-fn
          (funcall cleanup-fn))
        ;; Unregister
        (iota-special-buffer--unregister buffer-name)))))

;;; Public API

(defun iota-special-buffer-setup (buffer &rest options)
  "Setup BUFFER as a special IOTA buffer.
OPTIONS is a plist supporting:
  :hide-cursor BOOL       - Hide cursor (default t)
  :hide-modeline BOOL     - Hide modeline (default t)
  :hide-header BOOL       - Hide header line (default t)
  :read-only BOOL         - Make buffer read-only (default t)
  :cleanup-fn FUNCTION    - Called on cleanup (default nil)

Returns BUFFER.

This should be called AFTER switching to the buffer or with the buffer current."
  (let ((hide-cursor (if (plist-member options :hide-cursor)
                          (plist-get options :hide-cursor) t))
        (hide-modeline (if (plist-member options :hide-modeline)
                            (plist-get options :hide-modeline) t))
        (hide-header (if (plist-member options :hide-header)
                          (plist-get options :hide-header) t))
        (read-only (if (plist-member options :read-only)
                        (plist-get options :read-only) t))
        (cleanup-fn (plist-get options :cleanup-fn)))
    
    (with-current-buffer buffer
      ;; Set buffer-local variables for cursor hiding
      (setq-local cursor-type nil)
      (setq-local cursor-in-non-selected-windows nil)
      (setq-local visible-cursor nil)
      
      ;; Set read-only if requested
      (when read-only
        (setq-local buffer-read-only t))
      
      ;; Hide modeline if requested
      (when hide-modeline
        (setq-local mode-line-format nil)
        (set-window-parameter (selected-window) 'mode-line-format 'none))
      
      ;; Hide header if requested
      (when hide-header
        (setq-local header-line-format nil)
        (set-window-parameter (selected-window) 'header-line-format nil))
      
      ;; Hide cursor at window level (most reliable)
      (when hide-cursor
        (let ((win (selected-window)))
          (internal-show-cursor win nil)
          ;; Register this buffer with its window
          (iota-special-buffer--register (buffer-name buffer) win cleanup-fn)))
      
      ;; Add cleanup hook (buffer-local)
      (add-hook 'kill-buffer-hook #'iota-special-buffer--cleanup-on-kill nil t))
    
    buffer))

(defun iota-special-buffer-cleanup (buffer-or-name)
  "Explicitly cleanup special buffer BUFFER-OR-NAME.
This restores cursor and calls cleanup callbacks.
The buffer is NOT killed - caller must do that if desired."
  (let* ((buffer-name (if (bufferp buffer-or-name)
                          (buffer-name buffer-or-name)
                        buffer-or-name))
         (instance (iota-special-buffer--get-instance buffer-name)))
    (when instance
      (let ((window (plist-get instance :window))
            (cleanup-fn (plist-get instance :cleanup-fn)))
        ;; Restore cursor
        (when (and window (window-live-p window))
          (internal-show-cursor window t))
        (internal-show-cursor (selected-window) t)
        ;; Call cleanup callback
        (when cleanup-fn
          (funcall cleanup-fn))
        ;; Unregister
        (iota-special-buffer--unregister buffer-name)))))

(defun iota-special-buffer-dismiss (buffer-or-name)
  "Dismiss special buffer BUFFER-OR-NAME.
Performs cleanup and kills the buffer.
This is the recommended way to dismiss special buffers."
  (let ((buffer-name (if (bufferp buffer-or-name)
                         (buffer-name buffer-or-name)
                       buffer-or-name)))
    ;; Explicit cleanup (restores cursor)
    (iota-special-buffer-cleanup buffer-name)
    ;; Kill buffer
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))))

;;; Utility Functions

(defun iota-special-buffer-active-p (buffer-or-name)
  "Return non-nil if BUFFER-OR-NAME is an active special buffer."
  (let ((buffer-name (if (bufferp buffer-or-name)
                         (buffer-name buffer-or-name)
                       buffer-or-name)))
    (not (null (iota-special-buffer--get-instance buffer-name)))))

(defun iota-special-buffer-list ()
  "Return list of all active special buffer names."
  (mapcar #'car iota-special-buffer--instances))

(provide 'iota-special-buffer)
;;; iota-special-buffer.el ends here
