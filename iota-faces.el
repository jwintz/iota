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
  '((t :inherit vertical-border))
  "Face for TUI box decorations.
The foreground color is used for the box lines. No background.
Adapts to current theme via inheritance from vertical-border."
  :group 'iota-faces)

(defface iota-active-box-face
  '((t :inherit font-lock-comment-face :underline nil :overline nil :strike-through nil))
  "Face for box borders in active window.
Inherits from font-lock-comment-face for theme-appropriate muted color.
Explicitly disables underline/overline to prevent artifacts."
  :group 'iota-faces)

(defface iota-inactive-box-face
  '((t :inherit shadow :underline nil :overline nil :strike-through nil))
  "Face for box borders in inactive window.
Inherits from shadow for dimmed appearance.
Explicitly disables underline/overline to prevent artifacts."
  :group 'iota-faces)

;;; Accent Faces

(defface iota-accent-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for accent elements (highlights, active items).
Adapts to current theme via inheritance from font-lock-constant-face."
  :group 'iota-faces)

(defface iota-highlight-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for headings and emphasized text.
Adapts to current theme via inheritance from font-lock-keyword-face."
  :group 'iota-faces)

(defface iota-active-accent-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for accent elements in active window.
Full brightness accent color.
Adapts to current theme via inheritance from font-lock-constant-face."
  :group 'iota-faces)

(defface iota-inactive-accent-face
  '((t :inherit shadow :weight normal))
  "Face for accent elements in inactive window.
Desaturated and dimmed accent color.
Adapts to current theme via inheritance from shadow."
  :group 'iota-faces)

;;; Highlight Faces

(defface iota-active-highlight-face
  '((t :inherit warning :weight bold))
  "Face for highlighted elements in active window.
Adapts to current theme via inheritance from warning face."
  :group 'iota-faces)

(defface iota-inactive-highlight-face
  '((t :inherit shadow :weight normal))
  "Face for highlighted elements in inactive window.
Dimmed appearance.
Adapts to current theme via inheritance from shadow."
  :group 'iota-faces)

;;; Modeline Faces

(defface iota-active-modeline-face
  '((t :inherit default :weight normal))
  "Face for active window modeline.
Uses full brightness and saturation.
Adapts to current theme via inheritance from default."
  :group 'iota-faces)

(defface iota-inactive-modeline-face
  '((t :inherit shadow :weight light))
  "Face for inactive window modeline.
Uses dimmed colors and reduced weight.
Adapts to current theme via inheritance from shadow."
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
  '((t :inherit font-lock-function-name-face :weight normal))
  "Face for primary logo letters (I O T) in splash screen.
Adapts to current theme via inheritance from font-lock-function-name-face."
  :group 'iota-faces)

(defface iota-splash-logo-accent
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for accent logo letter (Λ) in splash screen.
Adapts to current theme via inheritance from font-lock-keyword-face."
  :group 'iota-faces)

(defface iota-splash-tagline
  '((t :inherit default :weight normal))
  "Face for splash screen tagline.
Adapts to current theme via inheritance from default face."
  :group 'iota-faces)

(defface iota-splash-tertiary
  '((t :inherit shadow :weight light))
  "Face for tertiary logo (ι • ο • τ • α) in splash screen.
Adapts to current theme via inheritance from shadow face."
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
