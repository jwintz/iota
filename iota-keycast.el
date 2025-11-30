;;; iota-keycast.el --- Keycast integration for I O T Λ -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: keycast, modeline
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Integration with keycast to display key bindings in IOTA modeline.
;; Disables keycast's built-in mode-line display and provides the data
;; through IOTA's segment system instead.

;;; Code:

(require 'cl-lib)
(require 'iota-faces)

(declare-function iota-modeline--update "iota-modeline")
(declare-function iota-modeline-refresh "iota-modeline")
(declare-function iota-segment-id "iota-segment")
(declare-function iota-segment-keycast "iota-segments")

;;; Keycast Integration

(defvar iota-keycast--original-predicate nil
  "Original value of keycast-mode-line-window-predicate.")

(defun iota-keycast--never-show-predicate ()
  "Predicate that always returns nil to hide keycast's built-in display."
  nil)

(defun iota-keycast--disable-builtin-display ()
  "Disable keycast's built-in mode-line display.
This prevents keycast from adding its own element to mode-line-format,
allowing IOTA to display the keycast information through its segment system."
  ;; Save original predicate if not already saved
  (when (and (boundp 'keycast-mode-line-window-predicate)
             (not iota-keycast--original-predicate))
    (setq iota-keycast--original-predicate keycast-mode-line-window-predicate))

  ;; Replace predicate with one that always returns nil
  (when (boundp 'keycast-mode-line-window-predicate)
    (setq keycast-mode-line-window-predicate #'iota-keycast--never-show-predicate))

  ;; Force update to clear any existing keycast display
  (force-mode-line-update t))

(defun iota-keycast-setup ()
  "Set up keycast integration with IOTA modeline.
Call this after enabling keycast-mode-line-mode or keycast-header-line-mode."
  (interactive)
  (iota-keycast--disable-builtin-display)
  ;; Force modeline refresh
  (when (fboundp 'iota-modeline-refresh)
    (iota-modeline-refresh)))

;; Advice to disable built-in display when keycast modes are enabled
(defun iota-keycast--mode-advice (&rest _args)
  "Advice function to disable keycast's built-in display."
  (run-with-timer 0.1 nil #'iota-keycast--disable-builtin-display))

;; Advice to trigger IOTA modeline update when keycast updates
(defun iota-keycast--update-advice (&rest _args)
  "Advice function to trigger IOTA modeline update after keycast updates."
  (when (and (fboundp 'iota-modeline--update)
             (bound-and-true-p iota-modeline-mode))
    (iota-modeline--update)))

(defun iota-keycast--ensure-segment-in-custom ()
  "Ensure keycast segment is in custom segments if using custom preset."
  (when (and (boundp 'iota-modeline-segments-preset)
             (eq iota-modeline-segments-preset 'custom)
             (boundp 'iota-modeline-custom-segments))
    ;; Check if keycast segment is already present
    (unless (cl-find-if (lambda (s)
                          (and (cl-struct-p s)
                               (ignore-errors (eq (iota-segment-id s) 'keycast))))
                        iota-modeline-custom-segments)
      ;; Add keycast segment before position segments
      (require 'iota-segments)
      (let* ((keycast-seg (iota-segment-keycast))
             (pos (cl-position-if (lambda (s)
                                    (and (cl-struct-p s)
                                         (ignore-errors (memq (iota-segment-id s)
                                                             '(position position-percent major-mode)))))
                                  iota-modeline-custom-segments)))
        (if pos
            ;; Insert before position/major-mode
            (setq iota-modeline-custom-segments
                  (append (cl-subseq iota-modeline-custom-segments 0 pos)
                          (list keycast-seg)
                          (cl-subseq iota-modeline-custom-segments pos)))
          ;; Append to end if no position segment found
          (setq iota-modeline-custom-segments
                (append iota-modeline-custom-segments (list keycast-seg))))))))

;;;###autoload
(defun iota-keycast-enable-integration ()
  "Enable IOTA integration with keycast.
This disables keycast's built-in mode-line display and ensures
keycast information is shown through IOTA's segment system."
  (interactive)
  ;; Disable builtin display immediately
  (iota-keycast--disable-builtin-display)

  ;; Add advice to disable it whenever keycast modes are toggled
  (advice-add 'keycast-mode-line-mode :after #'iota-keycast--mode-advice)
  (advice-add 'keycast-header-line-mode :after #'iota-keycast--mode-advice)

  ;; Add advice to keycast--update to trigger IOTA modeline updates
  (advice-add 'keycast--update :after #'iota-keycast--update-advice)

  ;; Ensure segment is in custom segments if needed
  (iota-keycast--ensure-segment-in-custom)

  ;; Refresh modeline
  (when (fboundp 'iota-modeline-refresh)
    (iota-modeline-refresh))

  (message "I O T Λ: Keycast integration enabled"))

;;;###autoload
(defun iota-keycast-disable-integration ()
  "Disable IOTA integration with keycast.
Removes advice and allows keycast to manage its own display."
  (interactive)
  (advice-remove 'keycast-mode-line-mode #'iota-keycast--mode-advice)
  (advice-remove 'keycast-header-line-mode #'iota-keycast--mode-advice)
  (advice-remove 'keycast--update #'iota-keycast--update-advice)

  ;; Restore original predicate
  (when (and (boundp 'keycast-mode-line-window-predicate)
             iota-keycast--original-predicate)
    (setq keycast-mode-line-window-predicate iota-keycast--original-predicate)
    (setq iota-keycast--original-predicate nil))

  (force-mode-line-update t))

;; Auto-enable integration when keycast modes are active
(defun iota-keycast-check-and-integrate ()
  "Check if keycast is active and enable integration if needed."
  (when (or (bound-and-true-p keycast-mode-line-mode)
            (bound-and-true-p keycast-header-line-mode))
    (iota-keycast-enable-integration)))

;; Run integration check on load
(with-eval-after-load 'keycast
  (iota-keycast-check-and-integrate))

;;; User Commands

(defun iota-keycast-add-segment ()
  "Manually add keycast segment to current modeline configuration."
  (interactive)
  (iota-keycast--ensure-segment-in-custom)
  (when (fboundp 'iota-modeline-refresh)
    (iota-modeline-refresh))
  (message "I O T Λ: Keycast segment added to modeline"))

;;; Debug Commands

(defun iota-keycast-debug ()
  "Debug keycast integration status."
  (interactive)
  (message "=== Keycast Debug Info ===")
  (message "keycast-mode-line-mode: %s" (bound-and-true-p keycast-mode-line-mode))
  (message "keycast-header-line-mode: %s" (bound-and-true-p keycast-header-line-mode))
  (message "keycast--this-command-keys: %S (type: %s)"
           (when (boundp 'keycast--this-command-keys)
             (symbol-value 'keycast--this-command-keys))
           (when (boundp 'keycast--this-command-keys)
             (type-of (symbol-value 'keycast--this-command-keys))))
  (message "keycast--this-command-desc: %S (type: %s, symbolp: %s, eq nil: %s)"
           (when (boundp 'keycast--this-command-desc)
             (symbol-value 'keycast--this-command-desc))
           (when (boundp 'keycast--this-command-desc)
             (type-of (symbol-value 'keycast--this-command-desc)))
           (when (boundp 'keycast--this-command-desc)
             (symbolp (symbol-value 'keycast--this-command-desc)))
           (when (boundp 'keycast--this-command-desc)
             (eq (symbol-value 'keycast--this-command-desc) nil)))
  (message "this-command: %S" this-command)
  (message "last-command: %S" last-command)
  (message "keycast-mode-line-window-predicate: %s" (when (boundp 'keycast-mode-line-window-predicate)
                                                       keycast-mode-line-window-predicate))
  (message "Segment text would be: %S" (when (fboundp 'iota-segment-keycast--get-text)
                                          (iota-segment-keycast--get-text)))
  (message "iota-modeline-mode: %s" (bound-and-true-p iota-modeline-mode))
  (message "iota-modeline-segments-preset: %s" (when (boundp 'iota-modeline-segments-preset)
                                                  iota-modeline-segments-preset)))

(provide 'iota-keycast)
;;; iota-keycast.el ends here
