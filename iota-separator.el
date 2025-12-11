;;; iota-separator.el --- Separator line handling for I O T Λ -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: windows, separator
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Centralized separator line management for IOTA.
;; Handles separator lines between windows and minibuffer,
;; with proper support for:
;;   - olivetti-mode margins
;;   - popup windows (transient, which-key, etc.)
;;   - Window configuration changes
;;   - Active/inactive face switching

;;; Code:

(require 'cl-lib)
(require 'iota-box)
(require 'iota-faces)

;;; Customization

(defgroup iota-separator nil
  "I O T Λ separator line configuration."
  :group 'iota
  :prefix "iota-separator-")

(defcustom iota-separator-style 'rounded
  "Box drawing style for separator lines.
Can be: single, double, rounded, heavy, ascii."
  :type '(choice (const :tag "Single line" single)
                 (const :tag "Double line" double)
                 (const :tag "Rounded corners" rounded)
                 (const :tag "Heavy line" heavy)
                 (const :tag "ASCII (compatibility)" ascii))
  :group 'iota-separator)

;;; State

(defvar iota-separator--overlays (make-hash-table :weakness 'key)
  "Hash table mapping windows to their separator overlays.")

;;; Width Calculation

(defun iota-separator--get-window-margins (window)
  "Get the left and right margins for WINDOW.
Returns a cons cell (LEFT . RIGHT), or (0 . 0) if no margins."
  (if (window-live-p window)
      (let ((margins (window-margins window)))
        (if margins
            (cons (or (car margins) 0) (or (cdr margins) 0))
          '(0 . 0)))
    '(0 . 0)))

(defun iota-separator--effective-width (window)
  "Calculate effective width for separator line in WINDOW.
Accounts for olivetti-mode margins and window margins.
Uses actual window margins set by olivetti, not config variable."
  (when (window-live-p window)
    (let* ((body-width (window-body-width window))
           (margins (iota-separator--get-window-margins window))
           (left-margin (car margins))
           (right-margin (cdr margins))
           ;; With olivetti, body-width already excludes margins
           ;; But we also need to account for any text width setting
           (buffer (window-buffer window))
           (olivetti-active
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (and (boundp 'olivetti-mode) olivetti-mode))))
           ;; If olivetti is active and has margins, the body-width is already correct
           ;; Otherwise use full body width
           (effective-width body-width))
      ;; Subtract 1 to prevent line wrapping
      (max 10 (1- effective-width)))))

(defun iota-separator--calculate-padding (window)
  "Calculate left padding needed for olivetti centering in WINDOW.
Returns 0 if olivetti is not active or no padding needed.
Uses actual window margins set by olivetti."
  (if (window-live-p window)
      (let ((margins (iota-separator--get-window-margins window)))
        (car margins))  ; Left margin is the padding we need
    0))

;;; Separator Rendering

(defun iota-separator--get-style ()
  "Get the effective separator style.
Uses `iota-separator-style' if set, otherwise falls back to
`iota-modeline-box-style' for consistency."
  (if (and (boundp 'iota-modeline-box-style)
           iota-modeline-box-style)
      iota-modeline-box-style
    iota-separator-style))

(defun iota-separator--render (window face)
  "Render separator line for WINDOW with FACE.
Returns a string with proper padding for olivetti-mode if active."
  (when (window-live-p window)
    (let* ((width (iota-separator--effective-width window))
           (padding (iota-separator--calculate-padding window))
           (style (iota-separator--get-style))
           (line (iota-box-horizontal-line width style face))
           (padded-line (if (> padding 0)
                           (concat (make-string padding ?\ ) line)
                         line)))
      padded-line)))

;;; Overlay Management

(defun iota-separator--ensure-overlay (window)
  "Ensure separator overlay exists for WINDOW.
Returns the overlay, or nil if window/buffer is invalid."
  (when (and (window-live-p window)
             (window-buffer window)
             (buffer-live-p (window-buffer window)))
    (let ((overlay (gethash window iota-separator--overlays)))
      ;; Check if overlay is still valid
      (when (and overlay
                 (overlayp overlay)
                 (not (overlay-buffer overlay)))
        (remhash window iota-separator--overlays)
        (setq overlay nil))
      ;; Create overlay if needed
      (unless overlay
        (with-current-buffer (window-buffer window)
          (setq overlay (make-overlay (point-max) (point-max) (current-buffer)))
          (overlay-put overlay 'priority 100)
          (overlay-put overlay 'window window)
          (overlay-put overlay 'iota-separator t)
          (puthash window overlay iota-separator--overlays)))
      overlay)))

(defun iota-separator--update (window face)
  "Update separator line for WINDOW with FACE."
  (when (and (window-live-p window)
             (window-buffer window)
             (buffer-live-p (window-buffer window)))
    (let ((overlay (iota-separator--ensure-overlay window)))
      (when overlay
        (with-current-buffer (window-buffer window)
          (move-overlay overlay (point-max) (point-max) (current-buffer))
          (let ((separator-str (iota-separator--render window face)))
            (overlay-put overlay 'after-string (concat "\n" separator-str))))))))

(defun iota-separator--remove (window)
  "Remove separator overlay from WINDOW."
  (let ((overlay (gethash window iota-separator--overlays)))
    (when (and overlay (overlayp overlay))
      (delete-overlay overlay))
    (remhash window iota-separator--overlays)))

(defun iota-separator--hide (window)
  "Hide separator in WINDOW without removing the overlay."
  (let ((overlay (gethash window iota-separator--overlays)))
    (when (and overlay (overlayp overlay))
      (overlay-put overlay 'after-string nil))))

;;; Cleanup

(defun iota-separator--cleanup-stale ()
  "Remove overlays for windows that no longer exist."
  (maphash (lambda (window _overlay)
             (unless (window-live-p window)
               (iota-separator--remove window)))
           iota-separator--overlays))

(defun iota-separator--remove-all ()
  "Remove all separator overlays."
  (maphash (lambda (window _overlay)
             (iota-separator--remove window))
           iota-separator--overlays)
  (clrhash iota-separator--overlays))

;;; Public API

(defun iota-separator-update-window (window &optional face)
  "Update separator line for WINDOW with optional FACE.
If FACE is nil, uses a default face."
  (when (window-live-p window)
    (let ((face (or face 'iota-box-face)))
      (iota-separator--update window face))))

(defun iota-separator-hide-window (window)
  "Hide separator line for WINDOW."
  (when (window-live-p window)
    (iota-separator--hide window)))

(defun iota-separator-remove-window (window)
  "Remove separator line for WINDOW."
  (when (window-live-p window)
    (iota-separator--remove window)))

(defun iota-separator-refresh-all ()
  "Refresh all separator lines.
This should be called after window configuration changes."
  (iota-separator--cleanup-stale))

(provide 'iota-separator)
;;; iota-separator.el ends here
