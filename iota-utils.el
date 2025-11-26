;;; iota-utils.el --- Utility functions for I O T Λ -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: utilities
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Common utility functions and macros used throughout IOTA.
;;
;; Key features:
;; - Safe function call wrapper with error handling
;; - Safe face attribute accessor
;; - Debug logging utilities

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup iota-utils nil
  "I O T Λ utility configuration."
  :group 'iota
  :prefix "iota-")

(defcustom iota-debug-mode nil
  "When non-nil, enable debug logging and extra error details."
  :type 'boolean
  :group 'iota-utils)

;;; Error Handling

(defmacro iota-safe-call (name &rest body)
  "Execute BODY with error handling, logging errors as NAME.
Returns nil on error if `iota-debug-mode' is nil.
NAME is a string identifying the operation for error messages."
  (declare (indent 1) (debug t))
  `(condition-case err
       (progn ,@body)
     (error
      (when iota-debug-mode
        (message "IOTA error in %s: %s" ,name (error-message-string err)))
      nil)))

(defmacro iota-safe-call-default (name default &rest body)
  "Execute BODY with error handling, returning DEFAULT on error.
NAME is a string identifying the operation for error messages."
  (declare (indent 2) (debug t))
  `(condition-case err
       (progn ,@body)
     (error
      (when iota-debug-mode
        (message "IOTA error in %s: %s" ,name (error-message-string err)))
      ,default)))

;;; Face Utilities

(defun iota-face-attribute-safe (face attribute &optional fallback frame inherit)
  "Get ATTRIBUTE from FACE, returning FALLBACK if unspecified.
FRAME and INHERIT are passed to `face-attribute'.
This safely handles 'unspecified and nil values."
  (let ((value (face-attribute face attribute frame inherit)))
    (if (or (null value) 
            (eq value 'unspecified)
            (and (stringp value) (string-empty-p value)))
        fallback
      value)))

(defun iota-face-foreground-safe (face &optional fallback frame inherit)
  "Get foreground color from FACE, returning FALLBACK if unspecified."
  (iota-face-attribute-safe face :foreground fallback frame inherit))

(defun iota-face-background-safe (face &optional fallback frame inherit)
  "Get background color from FACE, returning FALLBACK if unspecified."
  (iota-face-attribute-safe face :background fallback frame inherit))

(defun iota-color-valid-p (color)
  "Return t if COLOR is a valid color specification."
  (and color
       (stringp color)
       (not (string-empty-p color))
       (condition-case nil
           (color-name-to-rgb color)
         (error nil))))

;;; Logging

(defvar iota--log-buffer-name "*IOTA Log*"
  "Name of the IOTA log buffer.")

(defun iota-log (format-string &rest args)
  "Log a message to the IOTA log buffer.
FORMAT-STRING and ARGS are passed to `format'."
  (when iota-debug-mode
    (let ((msg (apply #'format format-string args))
          (timestamp (format-time-string "%H:%M:%S.%3N")))
      (with-current-buffer (get-buffer-create iota--log-buffer-name)
        (goto-char (point-max))
        (insert (format "[%s] %s\n" timestamp msg))))))

(defun iota-log-clear ()
  "Clear the IOTA log buffer."
  (interactive)
  (when-let ((buf (get-buffer iota--log-buffer-name)))
    (with-current-buffer buf
      (erase-buffer))))

(defun iota-log-show ()
  "Show the IOTA log buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create iota--log-buffer-name)))

;;; Buffer Utilities

(defun iota-buffer-visible-p (buffer-or-name)
  "Return t if BUFFER-OR-NAME is visible in any window."
  (when-let ((buffer (get-buffer buffer-or-name)))
    (not (null (get-buffer-window buffer t)))))

(defun iota-buffer-modifiable-p (&optional buffer)
  "Return t if BUFFER (default: current) can be modified."
  (with-current-buffer (or buffer (current-buffer))
    (and (not buffer-read-only)
         (buffer-modified-p))))

;;; Window Utilities

(defun iota-window-at-bottom-p (&optional window)
  "Return t if WINDOW is at the bottom of its frame.
Ignores temporary popup buffers like which-key."
  (let ((window (or window (selected-window))))
    (catch 'found-window-below
      (let* ((edges1 (window-edges window))
             (bottom1 (nth 3 edges1))
             (left1 (nth 0 edges1))
             (right1 (nth 2 edges1)))
        (dolist (w (window-list nil 'no-minibuf))
          (when (not (eq window w))
            (let* ((edges2 (window-edges w))
                   (top2 (nth 1 edges2))
                   (left2 (nth 0 edges2))
                   (right2 (nth 2 edges2))
                   (buf-name (buffer-name (window-buffer w))))
              ;; Ignore which-key and similar popup buffers
              (when (and edges1 edges2
                         (= top2 bottom1)
                         (> (min right1 right2) (max left1 left2))
                         (not (string-prefix-p " *which-key*" buf-name))
                         (not (string-prefix-p "*which-key*" buf-name)))
                (throw 'found-window-below nil)))))
        t))))

;;; String Utilities

(defun iota-string-truncate (string max-length &optional ellipsis)
  "Truncate STRING to MAX-LENGTH, adding ELLIPSIS if truncated.
ELLIPSIS defaults to \"…\"."
  (let ((ellipsis (or ellipsis "…")))
    (if (<= (length string) max-length)
        string
      (concat (substring string 0 (- max-length (length ellipsis)))
              ellipsis))))

(defun iota-string-pad (string width &optional align padding-char)
  "Pad STRING to WIDTH with ALIGN (left, right, center).
PADDING-CHAR defaults to space."
  (let* ((padding-char (or padding-char ?\s))
         (str-len (length string))
         (padding (max 0 (- width str-len))))
    (pcase align
      ('right (concat (make-string padding padding-char) string))
      ('center (let ((left-pad (/ padding 2))
                     (right-pad (- padding (/ padding 2))))
                 (concat (make-string left-pad padding-char)
                         string
                         (make-string right-pad padding-char))))
      (_ (concat string (make-string padding padding-char))))))

;;; Version Checking

(defun iota-emacs-version>= (major &optional minor)
  "Return t if Emacs version is >= MAJOR.MINOR."
  (or (> emacs-major-version major)
      (and (= emacs-major-version major)
           (>= (or emacs-minor-version 0) (or minor 0)))))

;;; Feature Detection

(defun iota-display-supports-unicode-p ()
  "Return t if the display supports Unicode box-drawing characters."
  (char-displayable-p ?╭))

(defun iota-terminal-p ()
  "Return t if running in a terminal (not GUI)."
  (not (display-graphic-p)))

(provide 'iota-utils)
;;; iota-utils.el ends here
