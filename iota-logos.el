;;; iota-logos.el --- I O T Λ logo rendering utilities -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Version: 0.1.0
;; Keywords: faces, logos, branding
;; URL: https://github.com/yourusername/iota

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Provides logo rendering utilities for IOTA logo variants
;; with proper brand colors according to identity guidelines.
;;
;; Logo Variants:
;; - Primary: I O T Λ (Greek-inspired, main branding)
;; - Secondary: I Ø T Δ (Geometry-inspired, technical contexts)
;; - Tertiary: ι • ο • τ • α (Minimalist, subdued contexts)

;;; Code:

(require 'iota-theme)

;;; Branding Colors

(defconst iota-brand-color-primary "#e6e6e6"
  "Primary text color from IOTA brand palette (light gray).")

(defconst iota-brand-color-accent-1 "#39bae6"
  "Accent color 1 from IOTA brand palette (cyan-blue).
Used for I O T letters in primary logo.")

(defconst iota-brand-color-accent-2 "#ffcc66"
  "Accent color 2 from IOTA brand palette (gold-yellow).
Used for Λ/Δ/α letters in logo variants.")

(defconst iota-brand-color-background "#0a0e14"
  "Background color from IOTA brand palette (deep blue-black).")

(defconst iota-brand-color-muted "#999999"
  "Muted color for subdued contexts.")

;;; Logo Rendering Functions

;;;###autoload
(defun iota-logo-primary (&optional active)
  "Return primary IOTA logo with accent colors.
Logo format: I O T Λ (Greek-inspired)

If ACTIVE is non-nil, use full brightness colors.
Otherwise, use dimmed colors for inactive context."
  (let ((accent-1 (if active iota-brand-color-accent-1 "#2a5a6a"))
        (accent-2 (if active iota-brand-color-accent-2 "#665533")))
    (concat
     (propertize "I O T " 'face `(:foreground ,accent-1 :weight ,(if active 'bold 'normal)))
     (propertize "Λ" 'face `(:foreground ,accent-2 :weight bold)))))

;;;###autoload
(defun iota-logo-secondary (&optional active)
  "Return secondary IOTA logo with accent colors.
Logo format: I Ø T Δ (Geometry-inspired)

If ACTIVE is non-nil, use full brightness colors.
Otherwise, use dimmed colors for inactive context."
  (let ((accent-1 (if active iota-brand-color-accent-1 "#2a5a6a"))
        (accent-2 (if active iota-brand-color-accent-2 "#665533")))
    (concat
     (propertize "I Ø T " 'face `(:foreground ,accent-1 :weight ,(if active 'bold 'normal)))
     (propertize "Δ" 'face `(:foreground ,accent-2 :weight bold)))))

;;;###autoload
(defun iota-logo-tertiary (&optional active)
  "Return tertiary IOTA logo with muted styling.
Logo format: ι • ο • τ • α (Minimalist)

If ACTIVE is non-nil, use normal brightness.
Otherwise, use further dimmed colors for inactive context."
  (let ((color (if active iota-brand-color-muted "#555555"))
        (weight (if active 'light 'ultralight)))
    (propertize "ι • ο • τ • α" 'face `(:foreground ,color :weight ,weight))))

;;;###autoload
(defun iota-logo-tagline ()
  "Return the primary IOTA tagline."
  (propertize "Not one iota more than needed."
              'face `(:foreground ,iota-brand-color-muted :slant italic)))

;;;###autoload
(defun iota-logo-tagline-alt ()
  "Return alternate IOTA tagline."
  (propertize "Minimal Terminal Interface for Emacs"
              'face `(:foreground ,iota-brand-color-primary)))

;;; Logo Insertion Functions

;;;###autoload
(defun iota-insert-logo-primary (&optional active)
  "Insert primary logo at point.
If ACTIVE is non-nil, use full brightness colors."
  (interactive "P")
  (insert (iota-logo-primary active)))

;;;###autoload
(defun iota-insert-logo-secondary (&optional active)
  "Insert secondary logo at point.
If ACTIVE is non-nil, use full brightness colors."
  (interactive "P")
  (insert (iota-logo-secondary active)))

;;;###autoload
(defun iota-insert-logo-tertiary (&optional active)
  "Insert tertiary logo at point.
If ACTIVE is non-nil, use normal brightness."
  (interactive "P")
  (insert (iota-logo-tertiary active)))

;;; Logo Variants for Different Contexts

;;;###autoload
(defun iota-logo-for-modeline (&optional active)
  "Return logo suitable for modeline display.
Uses tertiary logo with appropriate active/inactive styling.
If ACTIVE is non-nil, use normal brightness."
  (iota-logo-tertiary active))

;;;###autoload
(defun iota-logo-for-header ()
  "Return logo suitable for buffer headers.
Uses primary logo with full brightness."
  (iota-logo-primary t))

;;;###autoload
(defun iota-logo-for-footer ()
  "Return logo suitable for footers and credits.
Uses tertiary logo with muted styling."
  (iota-logo-tertiary nil))

;;; Message and Echo Area Functions

;;;###autoload
(defun iota-message-with-logo (format-string &rest args)
  "Display message with IOTA branding.
FORMAT-STRING and ARGS are passed to `format'."
  (message "%s %s"
           (iota-logo-tertiary t)
           (apply #'format format-string args)))

;;;###autoload
(defun iota-display-version ()
  "Display IOTA version with logo."
  (interactive)
  (iota-message-with-logo "Version %s" "0.1.0"))

;;; Branding Info

;;;###autoload
(defun iota-branding-info ()
  "Display IOTA branding information."
  (interactive)
  (with-output-to-temp-buffer "*IOTA Branding*"
    (princ (concat
            (iota-logo-primary t) "\n\n"
            "Primary Logo: I O T Λ (Greek-inspired)\n"
            "Use: Main branding, README headers, documentation\n\n"
            (iota-logo-secondary t) "\n\n"
            "Secondary Logo: I Ø T Δ (Geometry-inspired)\n"
            "Use: Technical docs, code comments, terminal splash screens\n\n"
            (iota-logo-tertiary t) "\n\n"
            "Tertiary Logo: ι • ο • τ • α (Minimalist)\n"
            "Use: Subdued contexts, footers, version tags, status indicators\n\n"
            (iota-logo-tagline) "\n\n"
            "Brand Colors:\n"
            (format "  Primary:   %s\n" iota-brand-color-primary)
            (format "  Accent 1:  %s (cyan-blue)\n" iota-brand-color-accent-1)
            (format "  Accent 2:  %s (gold-yellow)\n" iota-brand-color-accent-2)
            (format "  Background: %s (deep blue-black)\n" iota-brand-color-background)
            (format "  Muted:     %s\n" iota-brand-color-muted)))))

(provide 'iota-logos)

;;; iota-logos.el ends here
