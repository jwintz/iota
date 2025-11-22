;;; iota-faces.el --- I O T Λ face definitions -*- lexical-binding: t -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Version: 0.1.0
;; Keywords: faces
;; URL: https://github.com/yourusername/iota

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Centralized face definitions for IOTA.
;; All faces (UI, box, splash, active/inactive) are defined here.

;;; Code:

;;; Customization Group

(defgroup iota-faces nil
  "Faces for I O T Λ interface."
  :group 'iota
  :prefix "iota-")

;;; Base Faces

(defface iota-face
  '((t :inherit default))
  "Base face for I O T Λ UI elements."
  :group 'iota-faces)

(defface iota-muted-face
  '((t :inherit shadow))
  "Face for muted/secondary text."
  :group 'iota-faces)

(defface iota-vertical-border-face
  '((t :foreground "grey50"))
  "Face for vertical window borders.
This face is applied to the vertical-border face when IOTA window mode is active."
  :group 'iota-faces)

;;; Box Faces

(defface iota-box-face
  '((t :foreground "grey50"))
  "Face for TUI box decorations.
The foreground color is used for the box lines. No background."
  :group 'iota-faces)

(defface iota-active-box-face
  '((t :foreground "grey50"))
  "Face for box borders in active window.
Full brightness. No background."
  :group 'iota-faces)

(defface iota-inactive-box-face
  '((t :foreground "grey30"))
  "Face for box borders and separators in inactive window.
Dimmed to 30% brightness. No background."
  :group 'iota-faces)

;;; Accent Faces

(defface iota-accent-face
  '((t :foreground "#39bae6" :weight bold))
  "Face for accent elements (highlights, active items)."
  :group 'iota-faces)

(defface iota-active-accent-face
  '((t :foreground "#39bae6" :weight bold))
  "Face for accent elements in active window.
Full brightness accent color."
  :group 'iota-faces)

(defface iota-inactive-accent-face
  '((t :foreground "#2a5a6a" :weight normal))
  "Face for accent elements in inactive window.
Desaturated and dimmed accent color."
  :group 'iota-faces)

;;; Highlight Faces

(defface iota-active-highlight-face
  '((t :foreground "#ffcc66" :weight bold))
  "Face for highlighted elements in active window.
Uses gold-yellow accent color from branding."
  :group 'iota-faces)

(defface iota-inactive-highlight-face
  '((t :foreground "#665533" :weight normal))
  "Face for highlighted elements in inactive window.
Dimmed version of gold-yellow accent."
  :group 'iota-faces)

;;; Modeline Faces

(defface iota-active-modeline-face
  '((t :foreground "#e6e6e6" :weight normal))
  "Face for active window modeline.
Uses full brightness and saturation."
  :group 'iota-faces)

(defface iota-inactive-modeline-face
  '((t :foreground "#666666" :weight light))
  "Face for inactive window modeline.
Uses dimmed colors and reduced weight."
  :group 'iota-faces)

(defface iota-modeline-face
  '((t :foreground unspecified :background unspecified :inherit nil))
  "Face for IOTA modeline area.
Explicitly set to have no background to ensure transparency."
  :group 'iota-faces)

;;; Status Faces

(defface iota-error-face
  '((t :inherit error))
  "Face for error indicators."
  :group 'iota-faces)

(defface iota-active-error-face
  '((t :inherit error))
  "Face for error indicators in active window.
Full brightness error color."
  :group 'iota-faces)

(defface iota-inactive-error-face
  '((t :foreground "#663333"))
  "Face for error indicators in inactive window.
Dimmed error color."
  :group 'iota-faces)

(defface iota-success-face
  '((t :inherit success))
  "Face for success indicators."
  :group 'iota-faces)

(defface iota-active-success-face
  '((t :inherit success))
  "Face for success indicators in active window.
Full brightness success color."
  :group 'iota-faces)

(defface iota-inactive-success-face
  '((t :foreground "#336633"))
  "Face for success indicators in inactive window.
Dimmed success color."
  :group 'iota-faces)

(defface iota-warning-face
  '((t :inherit warning))
  "Face for warning indicators."
  :group 'iota-faces)

(defface iota-active-warning-face
  '((t :inherit warning))
  "Face for warning indicators in active window.
Full brightness warning color."
  :group 'iota-faces)

(defface iota-inactive-warning-face
  '((t :foreground "#665533"))
  "Face for warning indicators in inactive window.
Dimmed warning color."
  :group 'iota-faces)

;;; Splash Screen Faces

(defface iota-splash-logo-primary
  '((t :foreground "#39bae6" :weight normal))
  "Face for primary logo letters (I O T) in splash screen."
  :group 'iota-faces)

(defface iota-splash-logo-accent
  '((t :foreground "#ffcc66" :weight bold))
  "Face for accent logo letter (Λ) in splash screen."
  :group 'iota-faces)

(defface iota-splash-tagline
  '((t :foreground "#e6e6e6" :weight normal))
  "Face for splash screen tagline."
  :group 'iota-faces)

(defface iota-splash-tertiary
  '((t :foreground "#999999" :weight light))
  "Face for tertiary logo (ι • ο • τ • α) in splash screen."
  :group 'iota-faces)

(defface iota-splash-anim-1
  '((t :inherit iota-splash-tertiary))
  "Animation face 1 for splash screen."
  :group 'iota-faces)

(defface iota-splash-anim-2
  '((t :inherit iota-splash-tertiary))
  "Animation face 2 for splash screen."
  :group 'iota-faces)

(defface iota-splash-anim-3
  '((t :inherit iota-splash-tertiary))
  "Animation face 3 for splash screen."
  :group 'iota-faces)

(defface iota-splash-anim-4
  '((t :inherit iota-splash-tertiary))
  "Animation face 4 for splash screen."
  :group 'iota-faces)

(defface iota-splash-anim-inactive
  '((t :inherit iota-splash-tertiary))
  "Animation face for inactive elements in splash screen."
  :group 'iota-faces)

(provide 'iota-faces)

;;; iota-faces.el ends here
