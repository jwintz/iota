;;; iota-tui.el --- Core TUI library for I O T Λ -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: faces, tui
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Core TUI (Text User Interface) library for I O T Λ (I Ø T Δ).
;; Provides the high-level API for building terminal-inspired
;; interfaces in Emacs.
;;
;; This module serves as the primary entry point for TUI development,
;; combining functionality from:
;;   - iota-box:     Box drawing and borders
;;   - iota-widgets: Interactive components
;;   - iota-segment: Modeline/header segments
;;   - iota-theme:   Color management
;;   - iota-animate: Animations and transitions
;;
;; Key Features:
;;   - Modern Unicode box-drawing (7+ styles)
;;   - Animated widgets (spinners, progress, typing effects)
;;   - Layout system (columns, grids, rows)
;;   - Interactive elements (buttons, toggles, forms)
;;   - Theme-aware color management

;;; Code:

(require 'iota-box)
(require 'iota-segment)
(require 'iota-theme)
(require 'iota-widgets)

;; Forward declarations
(declare-function iota-animate-enabled "iota-animate")

;;; Customization Group

(defgroup iota nil
  "Minimal Terminal Interface for Emacs."
  :group 'faces
  :prefix "iota-")

(defcustom iota-style 'rounded
  "Default TUI component style.
Can be: single, double, rounded, heavy, heavy-rounded,
modern-thin, modern-thick, or ascii."
  :type '(choice (const :tag "Single line" single)
                 (const :tag "Double line" double)
                 (const :tag "Rounded corners" rounded)
                 (const :tag "Heavy line" heavy)
                 (const :tag "Heavy rounded" heavy-rounded)
                 (const :tag "Modern thin" modern-thin)
                 (const :tag "Modern thick" modern-thick)
                 (const :tag "ASCII (compatibility)" ascii))
  :group 'iota)

(defcustom iota-use-icons nil
  "Whether to use icons in UI elements.
Requires `all-the-icons' or `nerd-icons' package."
  :type 'boolean
  :group 'iota)

(defcustom iota-min-width 10
  "Minimum width for TUI components."
  :type 'integer
  :group 'iota)

;;; ============================================================
;;; High-Level Component API
;;; ============================================================

;;; Box Rendering

(cl-defun iota-render-box (&key content width style face align)
  "Render a box component.
This is a convenience wrapper around `iota-box-render'.

Arguments:
  :content  String or list of strings
  :width    Box width (default: auto)
  :style    Box style (default: `iota-style')
  :face     Face to apply
  :align    Content alignment (left/center/right)

Example:
  (iota-render-box :content \"Hello IOTA\"
                   :style \\='rounded
                   :align \\='center)"
  (iota-box-render
   :content content
   :width width
   :style (or style iota-style)
   :face (or face 'iota-box-face)
   :align align))

;;; Segment Rendering

(cl-defun iota-render-segments (segments &key width layout)
  "Render SEGMENTS with optional WIDTH and LAYOUT.

Arguments:
  segments  List of segment structs
  :width    Total width (default: window-width)
  :layout   Layout style: horizontal, box (default: horizontal)

Example:
  (iota-render-segments
    (list (iota-segment-simple \"Left\" \\='bold)
          (iota-segment-simple \"Right\" nil))
    :width 80)"
  (let ((width (or width (window-width))))
    (pcase layout
      ('box
       (iota-box-render-single-line
        :left (iota-segment-compose
               (cl-remove-if-not
                (lambda (s) (eq (iota-segment-align s) 'left))
                segments))
        :center (iota-segment-compose
                 (cl-remove-if-not
                  (lambda (s) (eq (iota-segment-align s) 'center))
                  segments))
        :right (iota-segment-compose
                (cl-remove-if-not
                 (lambda (s) (eq (iota-segment-align s) 'right))
                 segments))
        :width width
        :style iota-style
        :face 'iota-box-face))
      (_
       (iota-segment-layout segments width)))))

;;; ============================================================
;;; Interactive Components (Convenience Wrappers)
;;; ============================================================

(defun iota-button (label action &optional help-text)
  "Create an interactive button segment.
LABEL is the button text, ACTION is called on click.
Optional HELP-TEXT shows on hover."
  (iota-segment-clickable
   label
   action
   '(:foreground "#39bae6" :weight bold :box t)
   (or help-text (format "Click: %s" label))))

(defun iota-toggle (label state &optional on-toggle)
  "Create a toggle segment.
LABEL is the toggle text, STATE is current boolean state.
ON-TOGGLE is called when clicked."
  (let* ((indicator (if state "●" "○"))
         (text (format "%s %s" indicator label))
         (face (if state
                   '(:foreground "#51cf66" :weight bold)
                 '(:foreground "#888"))))
    (iota-segment-clickable
     text
     (or on-toggle (lambda () (interactive) (message "Toggled!")))
     face
     (format "Toggle %s (currently %s)" label (if state "ON" "OFF")))))

(defun iota-badge (text &optional type)
  "Create a badge segment.
TYPE can be: success, error, warning, info (default: info)."
  (iota-widget-badge text type))

;;; ============================================================
;;; Progress Indicators (Delegate to iota-widgets)
;;; ============================================================

(defun iota-progress-bar (value total &optional width style)
  "Create a progress bar.
VALUE is current progress, TOTAL is maximum.
Optional WIDTH (default 20) and STYLE (blocks, dots, line, etc.)."
  (iota-widget-progress-bar value total
                             :width (or width 20)
                             :style (or style 'blocks)
                             :show-percent nil))

(defun iota-progress-segment (label value total &optional width)
  "Create a progress segment with LABEL.
Shows progress bar and percentage."
  (let ((bar (iota-widget-progress-bar value total
                                        :width (or width 15)
                                        :style 'blocks
                                        :label label)))
    (iota-segment-simple bar)))

(defun iota-spinner (index &optional style)
  "Create a spinner segment.
INDEX rotates through spinner frames.
Optional STYLE: braille, dots, line, arc, etc."
  (iota-segment-simple
   (iota-widget-spinner index style)
   'iota-accent-face))

;;; ============================================================
;;; Status Indicators
;;; ============================================================

(defun iota-status-icon (status)
  "Create a status icon segment.
STATUS can be: success, error, warning, info, loading, pending."
  (let ((icon-map '((success . ("✓" iota-success-face))
                    (error   . ("✗" iota-error-face))
                    (warning . ("⚠" iota-warning-face))
                    (info    . ("●" iota-accent-face))
                    (loading . ("⋯" iota-muted-face))
                    (pending . ("○" iota-muted-face)))))
    (pcase-let ((`(,icon ,face) (or (alist-get status icon-map)
                                     '("?" default))))
      (iota-segment-simple icon face))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun iota-truncate-string (string width &optional ellipsis)
  "Truncate STRING to WIDTH with ELLIPSIS.
ELLIPSIS defaults to \"...\"."
  (iota-box-truncate string width ellipsis))

(defun iota-pad-string (string width &optional align)
  "Pad STRING to WIDTH with ALIGN (left/center/right)."
  (iota-box-pad string width align))

;;; Window Utilities

(defun iota-window-width-safe ()
  "Get window width safely, with minimum bound."
  (max iota-min-width (window-width)))

(defun iota-window-height-safe ()
  "Get window height safely, with minimum bound."
  (max 3 (window-height)))

;;; ============================================================
;;; Theme Integration
;;; ============================================================

(defun iota-get-face-color (face attribute &optional fallback)
  "Get color ATTRIBUTE from FACE with FALLBACK.
Wrapper around `iota-theme-get-color'."
  (iota-theme-get-color face attribute fallback))

(defun iota-refresh-faces ()
  "Refresh IOTA faces after theme change."
  (interactive)
  (iota-segment-cache-clear))

;; Auto-refresh on theme change
(add-hook 'iota-theme-change-hook #'iota-refresh-faces)

;;; ============================================================
;;; Version and Feature Detection
;;; ============================================================

(defun iota-version ()
  "Display I O T Λ version."
  (interactive)
  (message "I O T Λ v%s — Not one iota more than needed."
           (if (boundp 'iota-version) iota-version "0.1.0")))

(defun iota-has-icons-p ()
  "Return t if icon support is available."
  (and iota-use-icons
       (or (featurep 'all-the-icons)
           (featurep 'nerd-icons))))

(defun iota-terminal-p ()
  "Return t if running in terminal (not GUI)."
  (not (display-graphic-p)))

(defun iota-unicode-p ()
  "Return t if Unicode is supported."
  (char-displayable-p ?╭))

(defun iota-animation-p ()
  "Return t if animations are enabled."
  (and (boundp 'iota-animate-enabled)
       iota-animate-enabled))

;;; ============================================================
;;; Quick Start / Convenience Macros
;;; ============================================================

(defmacro iota-with-box (style &rest body)
  "Execute BODY with STYLE as the default box style."
  (declare (indent 1))
  `(let ((iota-style ,style))
     ,@body))

(defmacro iota-with-face (face &rest body)
  "Execute BODY with text propertized using FACE."
  (declare (indent 1))
  `(propertize (progn ,@body) 'face ,face))

(provide 'iota-tui)
