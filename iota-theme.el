;;; iota-theme.el --- Theme introspection and adaptation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: faces, themes
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Theme introspection utilities for I O T Λ (I Ø T Δ).
;; Provides functions to extract colors and properties from any theme,
;; detect light/dark themes, and create semantic faces that adapt automatically.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'iota-faces)

;;; Face Attribute Extraction

(defun iota-theme-get-attribute (face attribute &optional fallback)
  "Get ATTRIBUTE from FACE in current theme.
If ATTRIBUTE is unspecified, return FALLBACK or nil.
ATTRIBUTE can be :foreground, :background, :weight, etc."
  (let ((value (face-attribute face attribute nil t)))
    (if (or (eq value 'unspecified)
            (eq value 'unspecified-bg)
            (eq value 'unspecified-fg)
            (null value))
        fallback
      value)))

(defun iota-theme-get-color (face attribute &optional fallback)
  "Get color ATTRIBUTE from FACE.
ATTRIBUTE should be :foreground or :background.
Returns color string like \"#ffffff\" or FALLBACK if unspecified."
  (iota-theme-get-attribute face attribute fallback))

(defun iota-theme-get-foreground (face &optional fallback)
  "Get foreground color of FACE, or FALLBACK if unspecified."
  (iota-theme-get-color face :foreground fallback))

(defun iota-theme-get-background (face &optional fallback)
  "Get background color of FACE, or FALLBACK if unspecified."
  (iota-theme-get-color face :background fallback))

;;; Color Analysis

(defun iota-theme-color-luminance (color)
  "Calculate relative luminance of COLOR (0.0 = black, 1.0 = white).
COLOR should be a color name or hex string."
  (let* ((rgb (and color (color-name-to-rgb color)))
         (r (or (nth 0 rgb) 0.0))
         (g (or (nth 1 rgb) 0.0))
         (b (or (nth 2 rgb) 0.0)))
    ;; Relative luminance formula (ITU-R BT.709)
    (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))

(defun iota-theme-dark-p (&optional threshold)
  "Return t if current theme is dark.
Optional THRESHOLD (default 0.5) determines the luminance cutoff.
Lower threshold means more themes classified as dark."
  (let* ((bg (iota-theme-get-background 'default "#000000"))
         (luminance (iota-theme-color-luminance bg))
         (threshold (or threshold 0.5)))
    (< luminance threshold)))

(defun iota-theme-light-p (&optional threshold)
  "Return t if current theme is light.
See `iota-theme-dark-p' for THRESHOLD details."
  (not (iota-theme-dark-p threshold)))

;;; Color Manipulation

(defun iota-theme-color-darken (color percent)
  "Darken COLOR by PERCENT (0.0 to 1.0).
Returns a hex color string."
  (let* ((rgb (color-name-to-rgb color))
         (factor (- 1.0 percent))
         (r (* (nth 0 rgb) factor))
         (g (* (nth 1 rgb) factor))
         (b (* (nth 2 rgb) factor)))
    (color-rgb-to-hex r g b 2)))

(defun iota-theme-color-lighten (color percent)
  "Lighten COLOR by PERCENT (0.0 to 1.0).
Returns a hex color string."
  (let* ((rgb (color-name-to-rgb color))
         (factor percent)
         (r (+ (nth 0 rgb) (* (- 1.0 (nth 0 rgb)) factor)))
         (g (+ (nth 1 rgb) (* (- 1.0 (nth 1 rgb)) factor)))
         (b (+ (nth 2 rgb) (* (- 1.0 (nth 2 rgb)) factor))))
    (color-rgb-to-hex r g b 2)))

(defun iota-theme-color-blend (color1 color2 alpha)
  "Blend COLOR1 and COLOR2 with ALPHA (0.0 = color1, 1.0 = color2).
Returns a hex color string."
  (let* ((rgb1 (color-name-to-rgb color1))
         (rgb2 (color-name-to-rgb color2))
         (r (+ (* (nth 0 rgb1) (- 1.0 alpha)) (* (nth 0 rgb2) alpha)))
         (g (+ (* (nth 1 rgb1) (- 1.0 alpha)) (* (nth 1 rgb2) alpha)))
         (b (+ (* (nth 2 rgb1) (- 1.0 alpha)) (* (nth 2 rgb2) alpha))))
    (color-rgb-to-hex r g b 2)))

(defun iota-theme-color-adjust-saturation (color percent)
  "Adjust saturation of COLOR by PERCENT (-1.0 to 1.0).
Negative values desaturate, positive values saturate.
Returns a hex color string."
  (let* ((rgb (color-name-to-rgb color))
         (hsl (apply #'color-rgb-to-hsl rgb))
         (h (nth 0 hsl))
         (s (nth 1 hsl))
         (l (nth 2 hsl))
         (new-s (max 0.0 (min 1.0 (+ s (* s percent)))))
         (new-rgb (color-hsl-to-rgb h new-s l)))
    (apply #'color-rgb-to-hex (append new-rgb '(2)))))

(defun iota-theme-color-desaturate (color amount)
  "Desaturate COLOR by AMOUNT (0.0 to 1.0).
0.0 = no change, 1.0 = fully desaturated (grayscale).
Returns a hex color string."
  (iota-theme-color-adjust-saturation color (- amount)))

;;; Contrast Utilities

(defun iota-theme-contrast-ratio (color1 color2)
  "Calculate contrast ratio between COLOR1 and COLOR2.
Returns a ratio from 1.0 (no contrast) to 21.0 (maximum contrast).
WCAG AA requires 4.5:1 for normal text, 3:1 for large text."
  (let ((l1 (iota-theme-color-luminance color1))
        (l2 (iota-theme-color-luminance color2)))
    (/ (+ (max l1 l2) 0.05)
       (+ (min l1 l2) 0.05))))

(defun iota-theme-ensure-contrast (fg bg &optional min-ratio)
  "Ensure FG has sufficient contrast against BG.
If contrast ratio is below MIN-RATIO (default 4.5), adjust FG.
Returns adjusted foreground color."
  (let* ((min-ratio (or min-ratio 4.5))
         (current-ratio (iota-theme-contrast-ratio fg bg)))
    (if (>= current-ratio min-ratio)
        fg
      ;; Adjust FG to meet contrast requirement
      (if (< (iota-theme-color-luminance bg) 0.5)
          ;; Dark background: lighten foreground
          (iota-theme-color-lighten fg 0.3)
        ;; Light background: darken foreground
        (iota-theme-color-darken fg 0.3)))))

;;; Semantic Color Extraction

(defun iota-theme-get-accent-color ()
  "Get accent color from current theme.
Tries multiple sources: mode-line-highlight, region, link."
  (or (iota-theme-get-background 'mode-line-highlight nil)
      (iota-theme-get-background 'region nil)
      (iota-theme-get-foreground 'link nil)
      "#39bae6")) ; Default IOTA blue

(defun iota-theme-get-muted-color ()
  "Get muted/dimmed color from current theme.
Useful for secondary text and decorations."
  (let* ((fg (iota-theme-get-foreground 'default "#ffffff"))
         (bg (iota-theme-get-background 'default "#000000")))
    (iota-theme-color-blend fg bg 0.5)))

(defun iota-theme-get-error-color ()
  "Get error color from current theme."
  (or (iota-theme-get-foreground 'error nil)
      (iota-theme-get-foreground 'font-lock-warning-face nil)
      "#ff6b6b"))

(defun iota-theme-get-success-color ()
  "Get success color from current theme."
  (or (iota-theme-get-foreground 'success nil)
      (iota-theme-get-foreground 'font-lock-string-face nil)
      "#51cf66"))

(defun iota-theme-get-warning-color ()
  "Get warning color from current theme."
  (or (iota-theme-get-foreground 'warning nil)
      (iota-theme-get-foreground 'font-lock-builtin-face nil)
      "#ffd43b"))

;;; Active/Inactive Utilities
;; Note: Face definitions moved to iota-faces.el



(defun iota-theme-window-active-p (&optional window)
  "Return t if WINDOW is the active window.
Requires iota-window-mode to be enabled for proper detection.
If mode is disabled, always returns t (all windows appear active)."
  (if (not (bound-and-true-p iota-window-mode))
      t  ;; If mode not enabled, treat all windows as active (bright)
    (let ((win (or window (selected-window))))
      ;; Read window parameter set by iota-window-mode
      (and (windowp win) (window-parameter win 'iota-active)))))

(defun iota-theme-get-modeline-face (&optional window)
  "Return appropriate modeline face for WINDOW.
Returns `iota-active-modeline-face' for active window,
`iota-inactive-modeline-face' otherwise."
  (if (iota-theme-window-active-p window)
      'iota-active-modeline-face
    'iota-inactive-modeline-face))

(defun iota-theme-get-accent-face (&optional window)
  "Return appropriate accent face for WINDOW.
Returns `iota-active-accent-face' for active window,
`iota-inactive-accent-face' otherwise."
  (if (iota-theme-window-active-p window)
      'iota-active-accent-face
    'iota-inactive-accent-face))

(defun iota-theme-get-highlight-face (&optional window)
  "Return appropriate highlight face for WINDOW.
Returns `iota-active-highlight-face' for active window,
`iota-inactive-highlight-face' otherwise."
  (if (iota-theme-window-active-p window)
      'iota-active-highlight-face
    'iota-inactive-highlight-face))

(defun iota-theme-get-box-face (&optional window)
  "Return appropriate box face for WINDOW.
Returns `iota-active-box-face' for active window,
`iota-inactive-box-face' otherwise.
If WINDOW has `iota-animation-face-spec' parameter, return that."
  (let* ((win (or window (selected-window)))
         (spec (and (windowp win) (window-parameter win 'iota-animation-face-spec))))
    (if spec
        spec
      (if (iota-theme-window-active-p window)
          'iota-active-box-face
        'iota-inactive-box-face))))

(defun iota-theme-get-error-face (&optional window)
  "Return appropriate error face for WINDOW.
Returns `iota-active-error-face' for active window,
`iota-inactive-error-face' otherwise."
  (if (iota-theme-window-active-p window)
      'iota-active-error-face
    'iota-inactive-error-face))

(defun iota-theme-get-success-face (&optional window)
  "Return appropriate success face for WINDOW.
Returns `iota-active-success-face' for active window,
`iota-inactive-success-face' otherwise."
  (if (iota-theme-window-active-p window)
      'iota-active-success-face
    'iota-inactive-success-face))

(defun iota-theme-get-warning-face (&optional window)
  "Return appropriate warning face for WINDOW.
Returns `iota-active-warning-face' for active window,
`iota-inactive-warning-face' otherwise."
  (if (iota-theme-window-active-p window)
      'iota-active-warning-face
    'iota-inactive-warning-face))

;;; Theme Change Hook

(defvar iota-theme-change-hook nil
  "Hook run after theme changes.
Functions are called with no arguments.")

(defun iota-theme--on-change (&rest _)
  "Internal function called when theme changes."
  (run-hooks 'iota-theme-change-hook))

;; Register theme change detection
(advice-add 'load-theme :after #'iota-theme--on-change)
(advice-add 'disable-theme :after #'iota-theme--on-change)

(provide 'iota-theme)
;;; iota-theme.el ends here
