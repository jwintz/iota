;;; iota-timers.el --- Timer management for I O T Λ -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: performance, timers
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Centralized timer management to prevent timer leaks and ensure
;; proper cleanup when modes are disabled.
;;
;; Key features:
;; - Central registry for all IOTA timers
;; - Automatic cleanup on mode disable
;; - Timer statistics and debugging
;; - Protection against orphaned timers

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup iota-timers nil
  "I O T Λ timer management configuration."
  :group 'iota
  :prefix "iota-timers-")

(defcustom iota-timers-debug nil
  "When non-nil, log timer activity for debugging."
  :type 'boolean
  :group 'iota-timers)

;;; State

(defvar iota-timers--registry (make-hash-table :test 'eq)
  "Registry of all IOTA timers.
Maps timer keys (symbols) to timer objects.")

(defvar iota-timers--stats (make-hash-table :test 'eq)
  "Statistics for timer usage.
Maps timer keys to (CREATED-COUNT . CANCELLED-COUNT).")

;;; Core Functions

(defun iota-timers-register (key timer)
  "Register TIMER under KEY for cleanup.
If a timer already exists under KEY, it is cancelled first."
  (when iota-timers-debug
    (message "IOTA timer register: %s" key))
  
  ;; Cancel existing timer if present
  (when-let ((old (gethash key iota-timers--registry)))
    (when (timerp old)
      (cancel-timer old)
      ;; Update stats
      (let ((stats (or (gethash key iota-timers--stats) (cons 0 0))))
        (puthash key (cons (car stats) (1+ (cdr stats))) iota-timers--stats))))
  
  ;; Register new timer
  (puthash key timer iota-timers--registry)
  
  ;; Update stats
  (let ((stats (or (gethash key iota-timers--stats) (cons 0 0))))
    (puthash key (cons (1+ (car stats)) (cdr stats)) iota-timers--stats))
  
  timer)

(defun iota-timers-cancel (key)
  "Cancel timer registered under KEY."
  (when iota-timers-debug
    (message "IOTA timer cancel: %s" key))
  
  (when-let ((timer (gethash key iota-timers--registry)))
    (when (timerp timer)
      (cancel-timer timer))
    (remhash key iota-timers--registry)
    ;; Update stats
    (let ((stats (or (gethash key iota-timers--stats) (cons 0 0))))
      (puthash key (cons (car stats) (1+ (cdr stats))) iota-timers--stats))
    t))

(defun iota-timers-cancel-all ()
  "Cancel all registered timers."
  (interactive)
  (when iota-timers-debug
    (message "IOTA timer cancel-all: %d timers" 
             (hash-table-count iota-timers--registry)))
  
  (maphash (lambda (key timer)
             (when (timerp timer)
               (cancel-timer timer))
             ;; Update stats
             (let ((stats (or (gethash key iota-timers--stats) (cons 0 0))))
               (puthash key (cons (car stats) (1+ (cdr stats))) iota-timers--stats)))
           iota-timers--registry)
  (clrhash iota-timers--registry))

(defun iota-timers-get (key)
  "Get timer registered under KEY, or nil if not found."
  (gethash key iota-timers--registry))

(defun iota-timers-active-p (key)
  "Return t if timer under KEY is active."
  (when-let ((timer (gethash key iota-timers--registry)))
    (and (timerp timer)
         (memq timer timer-list))))

;;; Convenience Functions

(defun iota-timers-run-with-timer (key secs repeat function &rest args)
  "Run FUNCTION with ARGS after SECS, registered under KEY.
If REPEAT is non-nil, repeat every REPEAT seconds.
Returns the timer object."
  (let ((timer (apply #'run-with-timer secs repeat function args)))
    (iota-timers-register key timer)
    timer))

(defun iota-timers-run-with-idle-timer (key secs repeat function &rest args)
  "Run FUNCTION with ARGS when idle for SECS, registered under KEY.
If REPEAT is non-nil, repeat on each idle.
Returns the timer object."
  (let ((timer (apply #'run-with-idle-timer secs repeat function args)))
    (iota-timers-register key timer)
    timer))

(defun iota-timers-run-at-time (key time repeat function &rest args)
  "Run FUNCTION with ARGS at TIME, registered under KEY.
If REPEAT is non-nil, repeat every REPEAT seconds.
Returns the timer object."
  (let ((timer (apply #'run-at-time time repeat function args)))
    (iota-timers-register key timer)
    timer))

;;; Group Management

(defun iota-timers-cancel-group (prefix)
  "Cancel all timers whose keys start with PREFIX (a symbol prefix)."
  (let ((prefix-str (symbol-name prefix))
        (cancelled 0))
    (maphash (lambda (key timer)
               (when (string-prefix-p prefix-str (symbol-name key))
                 (when (timerp timer)
                   (cancel-timer timer)
                   (cl-incf cancelled))
                 (remhash key iota-timers--registry)))
             iota-timers--registry)
    (when iota-timers-debug
      (message "IOTA timer cancel-group %s: %d timers" prefix cancelled))
    cancelled))

(defun iota-timers-list-group (prefix)
  "List all timer keys starting with PREFIX."
  (let ((prefix-str (symbol-name prefix))
        (result nil))
    (maphash (lambda (key _timer)
               (when (string-prefix-p prefix-str (symbol-name key))
                 (push key result)))
             iota-timers--registry)
    result))

;;; Statistics and Debugging

(defun iota-timers-count ()
  "Return number of registered timers."
  (hash-table-count iota-timers--registry))

(defun iota-timers-list ()
  "Return list of all registered timer keys."
  (hash-table-keys iota-timers--registry))

(defun iota-timers-stats ()
  "Display timer statistics."
  (interactive)
  (let ((total-created 0)
        (total-cancelled 0)
        (active-count 0))
    (maphash (lambda (_key stats)
               (cl-incf total-created (car stats))
               (cl-incf total-cancelled (cdr stats)))
             iota-timers--stats)
    (maphash (lambda (_key timer)
               (when (and (timerp timer) (memq timer timer-list))
                 (cl-incf active-count)))
             iota-timers--registry)
    (message "IOTA Timers: %d registered, %d active, %d total created, %d cancelled"
             (hash-table-count iota-timers--registry)
             active-count
             total-created
             total-cancelled)))

(defun iota-timers-debug-list ()
  "List all registered timers with their status."
  (interactive)
  (with-output-to-temp-buffer "*IOTA Timers*"
    (princ "IOTA Timer Registry\n")
    (princ "===================\n\n")
    (if (zerop (hash-table-count iota-timers--registry))
        (princ "No timers registered.\n")
      (maphash (lambda (key timer)
                 (let ((active (and (timerp timer) (memq timer timer-list)))
                       (stats (gethash key iota-timers--stats)))
                   (princ (format "%-30s %s (created: %d, cancelled: %d)\n"
                                  key
                                  (if active "ACTIVE" "inactive")
                                  (or (car stats) 0)
                                  (or (cdr stats) 0)))))
               iota-timers--registry))))

(defun iota-timers-reset-stats ()
  "Reset timer statistics."
  (interactive)
  (clrhash iota-timers--stats)
  (message "IOTA timer stats reset"))

;;; Cleanup

(defun iota-timers-cleanup ()
  "Clean up all timer-related state."
  (iota-timers-cancel-all)
  (clrhash iota-timers--stats))

;;; Orphan Detection

(defun iota-timers-find-orphans ()
  "Find timers in `timer-list' that might be IOTA-related but not registered.
Returns list of suspicious timers."
  (let ((registered (make-hash-table :test 'eq))
        (orphans nil))
    ;; Build set of registered timers
    (maphash (lambda (_key timer)
               (when (timerp timer)
                 (puthash timer t registered)))
             iota-timers--registry)
    ;; Check timer-list for IOTA-related unregistered timers
    (dolist (timer timer-list)
      (unless (gethash timer registered)
        (when-let ((fn (timer--function timer)))
          (when (and (symbolp fn)
                     (string-prefix-p "iota-" (symbol-name fn)))
            (push timer orphans)))))
    orphans))

(defun iota-timers-cleanup-orphans ()
  "Cancel any orphaned IOTA timers."
  (interactive)
  (let ((orphans (iota-timers-find-orphans)))
    (dolist (timer orphans)
      (cancel-timer timer))
    (message "IOTA: Cancelled %d orphaned timers" (length orphans))))

(provide 'iota-timers)
;;; iota-timers.el ends here
