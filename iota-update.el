;;; iota-update.el --- Centralized update management for I O T Λ -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: performance, updates
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Centralized update management to consolidate all update mechanisms
;; and prevent cascading redraws that cause performance issues.
;;
;; Key features:
;; - Single debounced timer for all UI updates
;; - Component-based dirty tracking
;; - Batched updates to minimize redraws
;; - Idle-time scheduling for expensive operations

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup iota-update nil
  "I O T Λ update management configuration."
  :group 'iota
  :prefix "iota-update-")

(defcustom iota-update-debounce 0.15
  "Global debounce interval in seconds.
Updates are batched and executed after this delay.
Higher values reduce CPU usage but make UI feel less responsive.
Recommended range: 0.1 - 0.3"
  :type 'float
  :group 'iota-update)

(defcustom iota-update-idle-delay 0.5
  "Delay in seconds before running expensive updates during idle time.
Used for operations like VCS status refresh."
  :type 'float
  :group 'iota-update)

(defcustom iota-update-enabled t
  "Whether the centralized update system is enabled.
When nil, components must manage their own updates."
  :type 'boolean
  :group 'iota-update)

(defcustom iota-update-debug nil
  "When non-nil, log update activity for debugging."
  :type 'boolean
  :group 'iota-update)

;;; State

(defvar iota-update--pending nil
  "Whether an update is currently pending.")

(defvar iota-update--timer nil
  "Single timer for all deferred updates.")

(defvar iota-update--idle-timer nil
  "Timer for idle-time expensive updates.")

(defvar iota-update--dirty-components nil
  "List of components needing update.
Components are keywords like :modeline, :splash, :window.")

(defvar iota-update--idle-tasks nil
  "List of (COMPONENT . FN) pairs to run during idle time.")

(defvar iota-update--last-flush-time 0
  "Time of the last update flush.")

(defvar iota-update--update-count 0
  "Counter for debugging update frequency.")

;;; Component Update Functions

(defvar iota-update--component-handlers (make-hash-table :test 'eq)
  "Hash table mapping component keywords to their update functions.")

(defun iota-update-register-component (component update-fn)
  "Register UPDATE-FN as the handler for COMPONENT.
COMPONENT should be a keyword like :modeline, :splash, etc.
UPDATE-FN is called with no arguments when the component needs update."
  (puthash component update-fn iota-update--component-handlers))

(defun iota-update-unregister-component (component)
  "Unregister the handler for COMPONENT."
  (remhash component iota-update--component-handlers))

;;; Core Update Logic

(defun iota-update-request (component &optional immediate)
  "Request update for COMPONENT.
COMPONENT should be a keyword like :modeline, :splash, :window.
Updates are batched and debounced unless IMMEDIATE is non-nil.

When IMMEDIATE is non-nil, schedule update with minimal delay."
  (when iota-update-enabled
    (cl-pushnew component iota-update--dirty-components)
    
    (when iota-update-debug
      (message "IOTA update requested: %s (immediate: %s)" component immediate))
    
    (if immediate
        ;; Immediate update with minimal debounce
        (progn
          (when iota-update--timer
            (cancel-timer iota-update--timer))
          (setq iota-update--pending t)
          (setq iota-update--timer
                (run-with-timer 0.01 nil #'iota-update--flush)))
      ;; Normal debounced update
      (unless iota-update--pending
        (setq iota-update--pending t)
        (when iota-update--timer
          (cancel-timer iota-update--timer))
        (setq iota-update--timer
              (run-with-timer iota-update-debounce nil #'iota-update--flush))))))

(defun iota-update-request-multiple (components)
  "Request update for multiple COMPONENTS at once.
COMPONENTS should be a list of keywords."
  (dolist (component components)
    (cl-pushnew component iota-update--dirty-components))
  (unless iota-update--pending
    (setq iota-update--pending t)
    (when iota-update--timer
      (cancel-timer iota-update--timer))
    (setq iota-update--timer
          (run-with-timer iota-update-debounce nil #'iota-update--flush))))

(defun iota-update--flush ()
  "Process all pending updates.
This is called by the debounce timer."
  (setq iota-update--pending nil)
  (setq iota-update--timer nil)
  
  (let ((components iota-update--dirty-components))
    (setq iota-update--dirty-components nil)
    (setq iota-update--last-flush-time (float-time))
    (cl-incf iota-update--update-count)
    
    (when iota-update-debug
      (message "IOTA update flush #%d: %s" iota-update--update-count components))
    
    ;; Process each dirty component
    (dolist (component components)
      (condition-case err
          (when-let ((handler (gethash component iota-update--component-handlers)))
            (funcall handler))
        (error
         (when iota-update-debug
           (message "IOTA update error for %s: %s" component (error-message-string err))))))))

;;; Idle Time Updates

(defun iota-update-schedule-idle (component fn)
  "Schedule FN to run during idle time for COMPONENT.
This is useful for expensive operations like VCS status checks.
FN will be called once after Emacs has been idle for `iota-update-idle-delay'."
  (push (cons component fn) iota-update--idle-tasks)
  (iota-update--ensure-idle-timer))

(defun iota-update--ensure-idle-timer ()
  "Ensure the idle timer is scheduled for the next idle period.
This uses a one-shot timer that reschedules itself only when there are
pending tasks, avoiding continuous activity when idle (important for SSH)."
  ;; Only schedule if we have pending tasks and no timer is already scheduled
  (when (and iota-update--idle-tasks
             (not iota-update--idle-timer))
    (setq iota-update--idle-timer
          (run-with-idle-timer iota-update-idle-delay nil
                               #'iota-update--process-idle-tasks))))

(defun iota-update--process-idle-tasks ()
  "Process pending idle tasks.
This is a one-shot timer callback; it clears the timer reference
so a new timer can be scheduled when new tasks arrive."
  ;; Clear timer reference (one-shot timer has fired)
  (setq iota-update--idle-timer nil)
  (when iota-update--idle-tasks
    (let ((tasks iota-update--idle-tasks))
      (setq iota-update--idle-tasks nil)
      (dolist (task tasks)
        (condition-case err
            (funcall (cdr task))
          (error
           (when iota-update-debug
             (message "IOTA idle task error for %s: %s" 
                      (car task) (error-message-string err)))))))))

;;; Periodic Updates

(defvar iota-update--periodic-timer nil
  "Timer for periodic updates (time, battery, etc.)")

(defcustom iota-update-periodic-interval 60
  "Interval in seconds for periodic updates.
Used for time, battery, and similar slowly-changing information."
  :type 'integer
  :group 'iota-update)

(defun iota-update-start-periodic ()
  "Start the periodic update timer."
  (when iota-update--periodic-timer
    (cancel-timer iota-update--periodic-timer))
  (setq iota-update--periodic-timer
        (run-with-timer iota-update-periodic-interval
                        iota-update-periodic-interval
                        #'iota-update--periodic-tick)))

(defun iota-update-stop-periodic ()
  "Stop the periodic update timer."
  (when iota-update--periodic-timer
    (cancel-timer iota-update--periodic-timer)
    (setq iota-update--periodic-timer nil)))

(defun iota-update--periodic-tick ()
  "Handle periodic update tick."
  ;; Request update for components that need periodic refresh
  (iota-update-request :modeline))

;;; Hook Integration

(defvar iota-update--hooks-installed nil
  "Whether update hooks are currently installed.")

(defun iota-update-install-hooks ()
  "Install efficient hooks for update triggering.
Uses minimal hooks with proper debouncing.
Note: Idle timer is now scheduled on-demand when tasks are added,
not continuously, to avoid unnecessary activity when idle (SSH-friendly)."
  (unless iota-update--hooks-installed
    ;; Window configuration changes need immediate response
    (add-hook 'window-configuration-change-hook 
              #'iota-update--on-window-change)
    
    ;; Window size changes
    (add-hook 'window-size-change-functions
              #'iota-update--on-window-size-change)
    
    ;; Note: Idle timer is now scheduled on-demand by iota-update-schedule-idle
    ;; instead of running continuously. This avoids constant activity when idle.
    
    ;; Start periodic updates
    (iota-update-start-periodic)
    
    (setq iota-update--hooks-installed t)))

(defun iota-update-remove-hooks ()
  "Remove all update hooks."
  (when iota-update--hooks-installed
    (remove-hook 'window-configuration-change-hook 
                 #'iota-update--on-window-change)
    (remove-hook 'window-size-change-functions
                 #'iota-update--on-window-size-change)
    
    ;; Stop idle timer
    (when iota-update--idle-timer
      (cancel-timer iota-update--idle-timer)
      (setq iota-update--idle-timer nil))
    
    ;; Stop periodic updates
    (iota-update-stop-periodic)
    
    ;; Cancel pending update
    (when iota-update--timer
      (cancel-timer iota-update--timer)
      (setq iota-update--timer nil))
    
    (setq iota-update--hooks-installed nil)))

(defun iota-update--on-window-change ()
  "Handle window configuration changes."
  (iota-update-request-multiple '(:modeline :window)))

(defun iota-update--on-window-size-change (_frame)
  "Handle window size changes."
  (iota-update-request-multiple '(:modeline :splash :window)))

;;; Utility Functions

(defun iota-update-force ()
  "Force immediate update of all components."
  (interactive)
  (iota-update-request-multiple '(:modeline :splash :window))
  (iota-update--flush))

(defun iota-update-stats ()
  "Display update statistics."
  (interactive)
  (message "IOTA Update Stats: %d updates, last flush: %.2fs ago, pending: %s, dirty: %s"
           iota-update--update-count
           (- (float-time) iota-update--last-flush-time)
           iota-update--pending
           iota-update--dirty-components))

(defun iota-update-reset-stats ()
  "Reset update statistics."
  (interactive)
  (setq iota-update--update-count 0)
  (message "IOTA update stats reset"))

;;; Cleanup

(defun iota-update-cleanup ()
  "Clean up all update-related state."
  (iota-update-remove-hooks)
  (setq iota-update--dirty-components nil)
  (setq iota-update--idle-tasks nil)
  (clrhash iota-update--component-handlers))

(provide 'iota-update)
;;; iota-update.el ends here
