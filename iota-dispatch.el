;;; iota-dispatch.el --- Transient dispatch interface for IOTA -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: config, interface
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (transient "0.4.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Unified transient dispatch interface for all IOTA features.
;; Provides keyboard-driven access to configuration and controls.
;;
;; Usage:
;;   M-x iota-dispatch
;;
;; Binding with use-package and general.el:
;;
;;   (use-package iota
;;     :load-path "~/path/to/iota"
;;     :general
;;     ("C-c i" 'iota-dispatch)  ; Main dispatch
;;     (:prefix "C-c i"
;;      "c" 'iota-config-transient
;;      "s" 'iota-screens-transient
;;      "m" 'iota-modeline-transient
;;      "d" 'iota-dimmer-transient
;;      "t" 'iota-theme-transient
;;      "p" 'iota-popup-transient
;;      "w" 'iota-window-transient
;;      "?" 'iota-splash-transient))
;;
;; Or with vanilla Emacs:
;;
;;   (global-set-key (kbd "C-c i") 'iota-dispatch)

;;; Code:

(require 'transient)
(require 'cl-lib)

;; Forward declarations
(declare-function iota-config-apply-preset "iota-config")
(declare-function iota-screens-preview "iota-screens")
(declare-function iota-screens-deactivate-all "iota-screens")
(declare-function iota-modeline-refresh "iota-modeline")
(declare-function iota-modeline-toggle-position "iota-modeline")
(declare-function iota-modeline-cycle-style "iota-modeline")
(declare-function iota-modeline--teardown "iota-modeline")
(declare-function iota-modeline--setup "iota-modeline")
(declare-function iota-dimmer-apply-preset "iota-dimmer")
(declare-function iota-dimmer-refresh "iota-dimmer")
(declare-function iota-theme-transparent-diagnose "iota-theme-transparent")
(declare-function iota-theme-transparent-remove-backgrounds "iota-theme-transparent")
(declare-function iota-popup-cycle-style "iota-popup")
(declare-function iota-window-cycle-divider-style "iota-window")
(declare-function iota-splash-screen "iota-splash")
(declare-function iota-splash-refresh-hints "iota-splash")
(declare-function iota-icon "iota-icons")
(declare-function iota-icon-get "iota-icons")
(declare-function iota-icon-get-with-text "iota-icons")
(declare-function iota-copilot-transient "iota-copilot")

;;; Helper Functions

(defun iota-dispatch--format-value (label value)
  "Format LABEL: VALUE for display."
  (format "%s %s"
          (propertize (format "%s:" label) 'face 'transient-heading)
          (propertize (format "%s" value) 'face 'transient-value)))

(defun iota-dispatch--format-toggle (label enabled)
  "Format LABEL with ENABLED state."
  (format "%s %s"
          (propertize (format "%s:" label) 'face 'transient-heading)
          (propertize (if enabled "ON" "OFF")
                      'face (if enabled 'success 'shadow))))

;;; Separator Line

(defun iota-dispatch--separator ()
  "Return a horizontal separator line spanning the window width."
  (require 'iota-utils)
  (let* ((transient-window
          ;; Find the window displaying a transient buffer
          (or (get-buffer-window " *transient*")
              (cl-find-if (lambda (w)
                            (string-match-p "transient"
                                          (buffer-name (window-buffer w))))
                          (window-list))))
         (width (if transient-window
                    (iota-modeline-effective-width transient-window)
                  ;; Fall back to frame width if no transient window found
                  (1- (frame-width)))))
    (propertize (make-string width ?─) 'face 'shadow)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Configuration Transient (from iota-config.el, enhanced)
;;; ═══════════════════════════════════════════════════════════════════════════

(transient-define-suffix iota-dispatch--config-minimal ()
  "Set minimal preset."
  :key "1"
  :description "Minimal (ASCII)"
  (interactive)
  (require 'iota-config)
  (iota-config-apply-preset 'minimal)
  (setq iota-config-preset 'minimal)
  (message "Applied preset: minimal"))

(transient-define-suffix iota-dispatch--config-standard ()
  "Set standard preset."
  :key "2"
  :description "Standard (Single)"
  (interactive)
  (require 'iota-config)
  (iota-config-apply-preset 'standard)
  (setq iota-config-preset 'standard)
  (message "Applied preset: standard"))

(transient-define-suffix iota-dispatch--config-modern ()
  "Set modern preset."
  :key "3"
  :description "Modern (Rounded)"
  (interactive)
  (require 'iota-config)
  (iota-config-apply-preset 'modern)
  (setq iota-config-preset 'modern)
  (message "Applied preset: modern"))

(transient-define-suffix iota-dispatch--config-cyberpunk ()
  "Set cyberpunk preset."
  :key "4"
  :description "Cyberpunk (Heavy)"
  (interactive)
  (require 'iota-config)
  (iota-config-apply-preset 'cyberpunk)
  (setq iota-config-preset 'cyberpunk)
  (message "Applied preset: cyberpunk"))

(transient-define-suffix iota-dispatch--toggle-transparency ()
  "Toggle transparency mode."
  :key "t"
  :description (lambda ()
                 (format "Transparency: %s"
                         (if (bound-and-true-p iota-theme-transparent-mode) "ON" "OFF")))
  (interactive)
  (require 'iota-theme-transparent)
  (if (fboundp 'iota-theme-transparent-mode)
      (iota-theme-transparent-mode (if iota-theme-transparent-mode -1 1))
    (message "Transparency mode not available")))

;;;###autoload (autoload 'iota-config-transient "iota-dispatch" nil t)
(transient-define-prefix iota-config-transient ()
  "IOTA Configuration"
  [:description
   (lambda ()
     (require 'iota-config)
     (require 'iota-box)
     (let* ((chars (iota-box-get-chars iota-box-default-style))
            (preview (format "%s%s%s%s%s"
                             (plist-get chars :top-left)
                             (plist-get chars :horizontal)
                             (plist-get chars :horizontal)
                             (plist-get chars :horizontal)
                             (plist-get chars :top-right))))
       (concat
        (propertize "I O T Λ Configuration" 'face '(:weight bold))
        "\n" (iota-dispatch--separator) "\n"
        (iota-dispatch--format-value "Preset" (symbol-name iota-config-preset)) "\n"
        (iota-dispatch--format-value "Box Style" (symbol-name iota-box-default-style)) "\n"
        (iota-dispatch--format-value "Preview" preview) "\n"
        (when (boundp 'iota-modeline-mode)
          (concat (iota-dispatch--format-toggle "Modeline" iota-modeline-mode) "\n"))
        (when (boundp 'iota-theme-transparent-mode)
          (concat (iota-dispatch--format-toggle "Transparency" iota-theme-transparent-mode) "\n"))
        (when custom-enabled-themes
          (concat (iota-dispatch--format-value "Theme"
                                                (mapconcat #'symbol-name custom-enabled-themes ", ")) "\n"))
        (iota-dispatch--separator) "\n")))

   ["Presets"
    (iota-dispatch--config-minimal)
    (iota-dispatch--config-standard)
    (iota-dispatch--config-modern)
    (iota-dispatch--config-cyberpunk)]

   ["Options"
    (iota-dispatch--toggle-transparency)]])

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Screens Transient
;;; ═══════════════════════════════════════════════════════════════════════════

(transient-define-suffix iota-dispatch--screens-matrix ()
  "Preview Matrix animation."
  :key "m"
  :description "Matrix Rain"
  (interactive)
  (require 'iota-screens)
  (iota-screens-preview 'matrix))

(transient-define-suffix iota-dispatch--screens-lava ()
  "Preview Lava animation."
  :key "a"
  :description "Lava Lamp"
  (interactive)
  (require 'iota-screens)
  (iota-screens-preview 'lava))

(transient-define-suffix iota-dispatch--screens-life ()
  "Preview Life animation."
  :key "l"
  :description "Game of Life"
  (interactive)
  (require 'iota-screens)
  (iota-screens-preview 'life))

(transient-define-suffix iota-dispatch--screens-clock ()
  "Preview Clock animation."
  :key "c"
  :description "Digital Clock"
  (interactive)
  (require 'iota-screens)
  (iota-screens-preview 'clock))

(transient-define-suffix iota-dispatch--screens-pipes ()
  "Preview Pipes animation."
  :key "p"
  :description "3D Pipes"
  (interactive)
  (require 'iota-screens)
  (iota-screens-preview 'pipes))

(transient-define-suffix iota-dispatch--screens-plasma ()
  "Preview Plasma animation."
  :key "P"
  :description "Plasma Effect"
  (interactive)
  (require 'iota-screens)
  (iota-screens-preview 'plasma))

(transient-define-suffix iota-dispatch--screens-stars ()
  "Preview Stars animation."
  :key "S"
  :description "Starfield"
  (interactive)
  (require 'iota-screens)
  (iota-screens-preview 'stars))

(transient-define-suffix iota-dispatch--screens-earth ()
  "Preview Earth animation."
  :key "e"
  :description "Earth Globe"
  (interactive)
  (require 'iota-screens)
  (iota-screens-preview 'earth))

(transient-define-suffix iota-dispatch--screens-stop ()
  "Stop all screens."
  :key "q"
  :description "Stop All Screens"
  (interactive)
  (require 'iota-screens)
  (iota-screens-deactivate-all))

(transient-define-suffix iota-dispatch--screens-toggle-mode ()
  "Toggle screens idle mode."
  :key "i"
  :description (lambda ()
                 (format "Idle Detection: %s"
                         (if (bound-and-true-p iota-screens-mode) "ON" "OFF")))
  (interactive)
  (require 'iota-screens)
  (iota-screens-mode (if iota-screens-mode -1 1)))

;;;###autoload (autoload 'iota-screens-transient "iota-dispatch" nil t)
(transient-define-prefix iota-screens-transient ()
  "IOTA Screen Savers"
  [:description
   (lambda ()
     (concat
      (propertize "I O T Λ Screens" 'face '(:weight bold))
      "\n" (iota-dispatch--separator) "\n"
      (iota-dispatch--format-toggle "Idle Mode" (bound-and-true-p iota-screens-mode)) "\n"
      (when (boundp 'iota-screens-idle-timeout)
        (concat (iota-dispatch--format-value "Timeout"
                                              (format "%ds" (or iota-screens-idle-timeout 0))) "\n"))
      (when (boundp 'iota-screens-default-animation)
        (concat (iota-dispatch--format-value "Default"
                                              (symbol-name iota-screens-default-animation)) "\n"))
      (let ((active (when (boundp 'iota-screens--instances)
                      (hash-table-count iota-screens--instances))))
        (when (and active (> active 0))
          (concat (iota-dispatch--format-value "Active" active) "\n")))
      (iota-dispatch--separator) "\n"))

   ["Animations"
    (iota-dispatch--screens-matrix)
    (iota-dispatch--screens-lava)
    (iota-dispatch--screens-life)
    (iota-dispatch--screens-clock)]

   ["More"
    (iota-dispatch--screens-pipes)
    (iota-dispatch--screens-plasma)
    (iota-dispatch--screens-stars)
    (iota-dispatch--screens-earth)]

   ["Control"
    (iota-dispatch--screens-toggle-mode)
    (iota-dispatch--screens-stop)]])

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Modeline Transient
;;; ═══════════════════════════════════════════════════════════════════════════

(transient-define-suffix iota-dispatch--modeline-toggle ()
  "Toggle modeline mode."
  :key "m"
  :description (lambda ()
                 (format "Modeline Mode: %s"
                         (if (bound-and-true-p iota-modeline-mode) "ON" "OFF")))
  (interactive)
  (require 'iota-modeline)
  (iota-modeline-mode (if iota-modeline-mode -1 1)))

(transient-define-suffix iota-dispatch--modeline-position ()
  "Toggle modeline position."
  :key "p"
  :description (lambda ()
                 (format "Position: %s"
                         (if (boundp 'iota-modeline-position)
                             (symbol-name iota-modeline-position)
                           "header")))
  (interactive)
  (require 'iota-modeline)
  (iota-modeline-toggle-position))

(transient-define-suffix iota-dispatch--modeline-style ()
  "Cycle box style."
  :key "s"
  :description (lambda ()
                 (format "Box Style: %s"
                         (if (boundp 'iota-modeline-box-style)
                             (symbol-name iota-modeline-box-style)
                           "rounded")))
  (interactive)
  (require 'iota-modeline)
  (iota-modeline-cycle-style))

(transient-define-suffix iota-dispatch--modeline-separators ()
  "Toggle segment separators."
  :key "/"
  :description (lambda ()
                 (format "Separators: %s"
                         (if (bound-and-true-p iota-modeline-show-separators) "ON" "OFF")))
  (interactive)
  (require 'iota-modeline)
  (setq iota-modeline-show-separators (not iota-modeline-show-separators))
  ;; Force a full modeline recalculation
  (force-mode-line-update t)
  ;; Deferred refresh to ensure proper rendering
  (run-with-timer 0.05 nil #'iota-modeline-refresh)
  (message "Segment separators: %s" (if iota-modeline-show-separators "enabled" "disabled")))

(transient-define-suffix iota-dispatch--modeline-inactive ()
  "Toggle show in inactive windows."
  :key "i"
  :description (lambda ()
                 (format "Show Inactive: %s"
                         (if (bound-and-true-p iota-modeline-show-in-inactive) "ON" "OFF")))
  (interactive)
  (require 'iota-modeline)
  (setq iota-modeline-show-in-inactive (not iota-modeline-show-in-inactive))
  (iota-modeline-refresh))

(transient-define-suffix iota-dispatch--modeline-refresh ()
  "Refresh modeline."
  :key "r"
  :description "Refresh"
  (interactive)
  (require 'iota-modeline)
  (iota-modeline-refresh)
  (message "Modeline refreshed"))

(transient-define-suffix iota-dispatch--modeline-toggle-window ()
  "Toggle modeline box in current window."
  :key "t"
  :description (lambda ()
                 (format "Window Modeline: %s"
                         (if (window-parameter nil 'iota-modeline-hidden)
                             "OFF" "ON")))
  (interactive)
  (require 'iota-modeline)
  (iota-modeline-toggle-current-window))

;;;###autoload (autoload 'iota-modeline-transient "iota-dispatch" nil t)
(transient-define-prefix iota-modeline-transient ()
  "IOTA Modeline"
  [:description
   (lambda ()
     (concat
      (propertize "I O T Λ Modeline" 'face '(:weight bold))
      "\n" (iota-dispatch--separator) "\n"
      (iota-dispatch--format-toggle "Mode" (bound-and-true-p iota-modeline-mode)) "\n"
      (when (boundp 'iota-modeline-position)
        (concat (iota-dispatch--format-value "Position" (symbol-name iota-modeline-position)) "\n"))
      (when (boundp 'iota-modeline-box-style)
        (concat (iota-dispatch--format-value "Style" (symbol-name iota-modeline-box-style)) "\n"))
      (iota-dispatch--separator) "\n"))

   ["Toggle"
    (iota-dispatch--modeline-toggle)
    (iota-dispatch--modeline-position)
    (iota-dispatch--modeline-style)
    (iota-dispatch--modeline-toggle-window)]

   ["Options"
    (iota-dispatch--modeline-separators)
    (iota-dispatch--modeline-inactive)
    (iota-dispatch--modeline-refresh)]])

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Dimmer Transient
;;; ═══════════════════════════════════════════════════════════════════════════

(transient-define-suffix iota-dispatch--dimmer-toggle ()
  "Toggle dimmer mode."
  :key "d"
  :description (lambda ()
                 (format "Dimmer Mode: %s"
                         (if (bound-and-true-p iota-dimmer-mode) "ON" "OFF")))
  (interactive)
  (require 'iota-dimmer)
  (iota-dimmer-mode (if iota-dimmer-mode -1 1)))

(transient-define-suffix iota-dispatch--dimmer-subtle ()
  "Apply subtle preset."
  :key "1"
  :description "Subtle"
  (interactive)
  (require 'iota-dimmer)
  (iota-dimmer-apply-preset 'subtle)
  (message "Applied dimmer preset: subtle"))

(transient-define-suffix iota-dispatch--dimmer-balanced ()
  "Apply balanced preset."
  :key "2"
  :description "Balanced"
  (interactive)
  (require 'iota-dimmer)
  (iota-dimmer-apply-preset 'balanced)
  (message "Applied dimmer preset: balanced"))

(transient-define-suffix iota-dispatch--dimmer-muted ()
  "Apply muted preset."
  :key "3"
  :description "Muted"
  (interactive)
  (require 'iota-dimmer)
  (iota-dimmer-apply-preset 'muted)
  (message "Applied dimmer preset: muted"))

(transient-define-suffix iota-dispatch--dimmer-strong ()
  "Apply strong preset."
  :key "4"
  :description "Strong"
  (interactive)
  (require 'iota-dimmer)
  (iota-dimmer-apply-preset 'strong)
  (message "Applied dimmer preset: strong"))

(transient-define-suffix iota-dispatch--dimmer-desaturated ()
  "Apply desaturated preset."
  :key "5"
  :description "Desaturated"
  (interactive)
  (require 'iota-dimmer)
  (iota-dimmer-apply-preset 'desaturated)
  (message "Applied dimmer preset: desaturated"))

(transient-define-suffix iota-dispatch--dimmer-fade-only ()
  "Apply fade-only preset."
  :key "6"
  :description "Fade Only"
  (interactive)
  (require 'iota-dimmer)
  (iota-dimmer-apply-preset 'fade-only)
  (message "Applied dimmer preset: fade-only"))

(transient-define-suffix iota-dispatch--dimmer-washed ()
  "Apply washed preset."
  :key "7"
  :description "Washed"
  (interactive)
  (require 'iota-dimmer)
  (iota-dimmer-apply-preset 'washed)
  (message "Applied dimmer preset: washed"))

(transient-define-suffix iota-dispatch--dimmer-grayscale ()
  "Apply grayscale preset."
  :key "8"
  :description "Grayscale"
  (interactive)
  (require 'iota-dimmer)
  (iota-dimmer-apply-preset 'grayscale)
  (message "Applied dimmer preset: grayscale"))

(transient-define-suffix iota-dispatch--dimmer-high-contrast ()
  "Apply high-contrast preset."
  :key "9"
  :description "High Contrast"
  (interactive)
  (require 'iota-dimmer)
  (iota-dimmer-apply-preset 'high-contrast)
  (message "Applied dimmer preset: high-contrast"))

(transient-define-suffix iota-dispatch--dimmer-increase ()
  "Increase dimming."
  :key "+"
  :description "More Dim"
  (interactive)
  (require 'iota-dimmer)
  (setq iota-dimmer-fraction (min 0.9 (+ (or iota-dimmer-fraction 0.3) 0.05)))
  (when iota-dimmer-mode (iota-dimmer-refresh))
  (message "Dimmer fraction: %.0f%%" (* 100 iota-dimmer-fraction)))

(transient-define-suffix iota-dispatch--dimmer-decrease ()
  "Decrease dimming."
  :key "-"
  :description "Less Dim"
  (interactive)
  (require 'iota-dimmer)
  (setq iota-dimmer-fraction (max 0.1 (- (or iota-dimmer-fraction 0.3) 0.05)))
  (when iota-dimmer-mode (iota-dimmer-refresh))
  (message "Dimmer fraction: %.0f%%" (* 100 iota-dimmer-fraction)))

;;;###autoload (autoload 'iota-dimmer-transient "iota-dispatch" nil t)
(transient-define-prefix iota-dimmer-transient ()
  "IOTA Dimmer"
  [:description
   (lambda ()
     (concat
      (propertize "I O T Λ Dimmer" 'face '(:weight bold))
      "\n" (iota-dispatch--separator) "\n"
      (iota-dispatch--format-toggle "Mode" (bound-and-true-p iota-dimmer-mode)) "\n"
      (when (boundp 'iota-dimmer-fraction)
        (concat (iota-dispatch--format-value "Fraction"
                                              (format "%.0f%%" (* 100 iota-dimmer-fraction))) "\n"))
      (when (boundp 'iota-dimmer-saturation-fraction)
        (concat (iota-dispatch--format-value "Saturation"
                                              (if iota-dimmer-saturation-fraction
                                                  (format "%.0f%%" (* 100 iota-dimmer-saturation-fraction))
                                                "auto")) "\n"))
      (when (boundp 'iota-dimmer-luminance-fraction)
        (concat (iota-dispatch--format-value "Luminance"
                                              (if iota-dimmer-luminance-fraction
                                                  (format "%.0f%%" (* 100 iota-dimmer-luminance-fraction))
                                                "auto")) "\n"))
      (iota-dispatch--separator) "\n"))

   ["Toggle"
    (iota-dispatch--dimmer-toggle)]

   ["Presets (Basic)"
    (iota-dispatch--dimmer-subtle)
    (iota-dispatch--dimmer-balanced)
    (iota-dispatch--dimmer-muted)
    (iota-dispatch--dimmer-strong)]

   ["Presets (Advanced)"
    (iota-dispatch--dimmer-desaturated)
    (iota-dispatch--dimmer-fade-only)
    (iota-dispatch--dimmer-washed)
    (iota-dispatch--dimmer-grayscale)
    (iota-dispatch--dimmer-high-contrast)]

   ["Adjust"
    (iota-dispatch--dimmer-increase)
    (iota-dispatch--dimmer-decrease)]])

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Theme Transient
;;; ═══════════════════════════════════════════════════════════════════════════

(transient-define-suffix iota-dispatch--theme-transparency ()
  "Toggle transparency mode."
  :key "t"
  :description (lambda ()
                 (format "Transparency: %s"
                         (if (bound-and-true-p iota-theme-transparent-mode) "ON" "OFF")))
  (interactive)
  (require 'iota-theme-transparent)
  (iota-theme-transparent-mode (if iota-theme-transparent-mode -1 1)))

(transient-define-suffix iota-dispatch--theme-diagnose ()
  "Run transparency diagnostics."
  :key "d"
  :description "Diagnose"
  (interactive)
  (require 'iota-theme-transparent)
  (iota-theme-transparent-diagnose))

(transient-define-suffix iota-dispatch--theme-reapply ()
  "Reapply transparency to current faces."
  :key "r"
  :description "Reapply"
  (interactive)
  (require 'iota-theme-transparent)
  (if (bound-and-true-p iota-theme-transparent-mode)
      (progn
        ;; Just reapply transparency without toggling mode
        (setq iota-theme-transparent--original-specs nil)
        (iota-theme-transparent-remove-backgrounds)
        (message "Transparency reapplied"))
    (message "Transparency mode is not active")))

(transient-define-suffix iota-dispatch--theme-verbose ()
  "Toggle verbose logging."
  :key "v"
  :description (lambda ()
                 (format "Verbose: %s"
                         (if (bound-and-true-p iota-theme-transparent-verbose-logging) "ON" "OFF")))
  (interactive)
  (require 'iota-theme-transparent)
  (setq iota-theme-transparent-verbose-logging
        (not iota-theme-transparent-verbose-logging))
  (message "Verbose logging: %s"
           (if iota-theme-transparent-verbose-logging "enabled" "disabled")))

;;;###autoload (autoload 'iota-theme-transient "iota-dispatch" nil t)
(transient-define-prefix iota-theme-transient ()
  "IOTA Theme & Transparency"
  [:description
   (lambda ()
     (concat
      (propertize "I O T Λ Theme" 'face '(:weight bold))
      "\n" (iota-dispatch--separator) "\n"
      (when custom-enabled-themes
        (concat (iota-dispatch--format-value "Theme"
                                              (mapconcat #'symbol-name custom-enabled-themes ", ")) "\n"))
      (iota-dispatch--format-toggle "Transparency" (bound-and-true-p iota-theme-transparent-mode)) "\n"
      (iota-dispatch--format-value "Display" (if (display-graphic-p) "GUI" "Terminal")) "\n"
      (unless (display-graphic-p)
        (concat (iota-dispatch--format-value "Colors" (or (tty-display-color-cells) "N/A")) "\n"))
      (iota-dispatch--separator) "\n"))

   ["Transparency"
    (iota-dispatch--theme-transparency)
    (iota-dispatch--theme-reapply)
    (iota-dispatch--theme-diagnose)]

   ["Debug"
    (iota-dispatch--theme-verbose)]])

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Popup Transient
;;; ═══════════════════════════════════════════════════════════════════════════

(transient-define-suffix iota-dispatch--popup-toggle ()
  "Toggle popup mode."
  :key "p"
  :description (lambda ()
                 (format "Popup Mode: %s"
                         (if (bound-and-true-p iota-popup-mode) "ON" "OFF")))
  (interactive)
  (require 'iota-popup)
  (iota-popup-mode (if iota-popup-mode -1 1)))

(transient-define-suffix iota-dispatch--popup-style ()
  "Cycle decoration style."
  :key "s"
  :description (lambda ()
                 (format "Style: %s"
                         (if (boundp 'iota-popup-decoration-style)
                             (symbol-name iota-popup-decoration-style)
                           "bottom-line")))
  (interactive)
  (require 'iota-popup)
  (iota-popup-cycle-style))

;;;###autoload (autoload 'iota-popup-transient "iota-dispatch" nil t)
(transient-define-prefix iota-popup-transient ()
  "IOTA Popup Windows"
  [:description
   (lambda ()
     (concat
      (propertize "I O T Λ Popup" 'face '(:weight bold))
      "\n" (iota-dispatch--separator) "\n"
      (iota-dispatch--format-toggle "Mode" (bound-and-true-p iota-popup-mode)) "\n"
      (when (boundp 'iota-popup-decoration-style)
        (concat (iota-dispatch--format-value "Style"
                                              (symbol-name iota-popup-decoration-style)) "\n"))
      (iota-dispatch--separator) "\n"))

   ["Options"
    (iota-dispatch--popup-toggle)
    (iota-dispatch--popup-style)]])

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Window Transient
;;; ═══════════════════════════════════════════════════════════════════════════

(transient-define-suffix iota-dispatch--window-toggle ()
  "Toggle window mode."
  :key "w"
  :description (lambda ()
                 (format "Window Mode: %s"
                         (if (bound-and-true-p iota-window-mode) "ON" "OFF")))
  (interactive)
  (require 'iota-window)
  (iota-window-mode (if iota-window-mode -1 1)))

(transient-define-suffix iota-dispatch--window-divider ()
  "Cycle divider style."
  :key "d"
  :description (lambda ()
                 (format "Dividers: %s"
                         (if (boundp 'iota-window-divider-style)
                             (symbol-name iota-window-divider-style)
                           "hidden")))
  (interactive)
  (require 'iota-window)
  (iota-window-cycle-divider-style))

(transient-define-suffix iota-dispatch--window-resize ()
  "Interactive window resize."
  :key "r"
  :description "Resize"
  :transient nil
  (interactive)
  (iota-window-resize-transient))

;;; Window Resize Transient (demonstrates iota-icons usage)

(transient-define-suffix iota-dispatch--resize-taller ()
  "Make window taller."
  :key "k"
  :description (lambda ()
                 (require 'iota-icons)
                 (iota-icon-get-with-text 'arrow-up "Taller"))
  :transient t
  (interactive)
  (enlarge-window 1))

(transient-define-suffix iota-dispatch--resize-shorter ()
  "Make window shorter."
  :key "j"
  :description (lambda ()
                 (require 'iota-icons)
                 (iota-icon-get-with-text 'arrow-down "Shorter"))
  :transient t
  (interactive)
  (shrink-window 1))

(transient-define-suffix iota-dispatch--resize-wider ()
  "Make window wider."
  :key "l"
  :description (lambda ()
                 (require 'iota-icons)
                 (iota-icon-get-with-text 'arrow-right "Wider"))
  :transient t
  (interactive)
  (enlarge-window-horizontally 1))

(transient-define-suffix iota-dispatch--resize-narrower ()
  "Make window narrower."
  :key "h"
  :description (lambda ()
                 (require 'iota-icons)
                 (iota-icon-get-with-text 'arrow-left "Narrower"))
  :transient t
  (interactive)
  (shrink-window-horizontally 1))

(transient-define-suffix iota-dispatch--resize-balance ()
  "Balance all windows."
  :key "="
  :description (lambda ()
                 (require 'iota-icons)
                 (iota-icon-get-with-text 'balance "Balance"))
  :transient t
  (interactive)
  (balance-windows))

(transient-define-suffix iota-dispatch--resize-maximize ()
  "Maximize current window."
  :key "m"
  :description (lambda ()
                 (require 'iota-icons)
                 (iota-icon-get-with-text 'maximize "Maximize"))
  :transient nil
  (interactive)
  (delete-other-windows))

;;;###autoload (autoload 'iota-window-resize-transient "iota-dispatch" nil t)
(transient-define-prefix iota-window-resize-transient ()
  "Resize windows interactively with vim-style bindings."
  [:description
   (lambda ()
     (require 'iota-icons)
     (concat
      (propertize "I O T Λ Resize" 'face '(:weight bold))
      "  "
      (propertize "(hjkl to resize, = to balance)" 'face 'shadow)
      "\n" (iota-dispatch--separator) "\n"))

   ["Vertical"
    (iota-dispatch--resize-taller)
    (iota-dispatch--resize-shorter)]

   ["Horizontal"
    (iota-dispatch--resize-wider)
    (iota-dispatch--resize-narrower)]

   ["Quick"
    (iota-dispatch--resize-balance)
    (iota-dispatch--resize-maximize)
    ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'iota-window-transient "iota-dispatch" nil t)
(transient-define-prefix iota-window-transient ()
  "IOTA Window Management"
  [:description
   (lambda ()
     (concat
      (propertize "I O T Λ Window" 'face '(:weight bold))
      "\n" (iota-dispatch--separator) "\n"
      (iota-dispatch--format-toggle "Mode" (bound-and-true-p iota-window-mode)) "\n"
      (when (boundp 'iota-window-divider-style)
        (concat (iota-dispatch--format-value "Dividers"
                                              (symbol-name iota-window-divider-style)) "\n"))
      (iota-dispatch--separator) "\n"))

   ["Options"
    (iota-dispatch--window-toggle)
    (iota-dispatch--window-divider)
    (iota-dispatch--window-resize)]])

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Splash Transient
;;; ═══════════════════════════════════════════════════════════════════════════

(transient-define-suffix iota-dispatch--splash-show ()
  "Show splash screen."
  :key "s"
  :description "Show Splash"
  :transient nil  ; Exit transient after running
  (interactive)
  (require 'iota-splash)
  ;; Use run-at-time to show splash after transient fully closes
  (run-at-time 0.1 nil (lambda () (iota-splash-screen t))))

(transient-define-suffix iota-dispatch--splash-hints ()
  "Toggle hints."
  :key "h"
  :description (lambda ()
                 (format "Hints: %s"
                         (if (bound-and-true-p iota-splash-show-hints) "ON" "OFF")))
  (interactive)
  (require 'iota-splash)
  (setq iota-splash-show-hints (not iota-splash-show-hints))
  (message "Splash hints: %s" (if iota-splash-show-hints "enabled" "disabled")))

(transient-define-suffix iota-dispatch--splash-random ()
  "Toggle random hint order."
  :key "r"
  :description (lambda ()
                 (format "Random Order: %s"
                         (if (bound-and-true-p iota-splash-hints-random) "ON" "OFF")))
  (interactive)
  (require 'iota-splash)
  (setq iota-splash-hints-random (not iota-splash-hints-random))
  (message "Random hints: %s" (if iota-splash-hints-random "enabled" "disabled")))

(transient-define-suffix iota-dispatch--splash-refresh ()
  "Refresh hints."
  :key "R"
  :description "Refresh Hints"
  (interactive)
  (require 'iota-splash)
  (iota-splash-refresh-hints)
  (message "Hints refreshed"))

;;;###autoload (autoload 'iota-splash-transient "iota-dispatch" nil t)
(transient-define-prefix iota-splash-transient ()
  "IOTA Splash Screen"
  [:description
   (lambda ()
     (concat
      (propertize "I O T Λ Splash" 'face '(:weight bold))
      "\n" (iota-dispatch--separator) "\n"
      (iota-dispatch--format-toggle "Hints" (bound-and-true-p iota-splash-show-hints)) "\n"
      (iota-dispatch--format-toggle "Random" (bound-and-true-p iota-splash-hints-random)) "\n"
      (when (boundp 'iota-splash-animation-interval)
        (concat (iota-dispatch--format-value "Animation"
                                              (if iota-splash-animation-interval
                                                  (format "%.1fs" iota-splash-animation-interval)
                                                "disabled")) "\n"))
      (iota-dispatch--separator) "\n"))

   ["Actions"
    (iota-dispatch--splash-show)
    (iota-dispatch--splash-refresh)]

   ["Options"
    (iota-dispatch--splash-hints)
    (iota-dispatch--splash-random)]])

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Package Management Transient
;;; ═══════════════════════════════════════════════════════════════════════════

(transient-define-suffix iota-dispatch--package-list ()
  "List all packages."
  :key "l"
  :description "List Packages"
  :transient nil
  (interactive)
  (list-packages))

(transient-define-suffix iota-dispatch--package-installed ()
  "List installed packages."
  :key "i"
  :description "List Installed"
  :transient nil
  (interactive)
  (list-packages)
  (package-menu-filter-by-status "installed"))

(transient-define-suffix iota-dispatch--package-upgrade ()
  "Upgrade all packages."
  :key "u"
  :description "Upgrade All"
  :transient nil
  (interactive)
  (if (fboundp 'package-upgrade-all)
      (package-upgrade-all)
    (message "package-upgrade-all not available in this Emacs version")))

;;;###autoload (autoload 'iota-package-transient "iota-dispatch" nil t)
(transient-define-prefix iota-package-transient ()
  "IOTA Package Management"
  [:description
   (lambda ()
     (concat
      (propertize "I O T Λ Packages" 'face '(:weight bold))
      "\n" (iota-dispatch--separator) "\n"))

   ["Actions"
    (iota-dispatch--package-list)
    (iota-dispatch--package-installed)
    (iota-dispatch--package-upgrade)]])

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Main Dispatch
;;; ═══════════════════════════════════════════════════════════════════════════

;;;###autoload
(transient-define-prefix iota-dispatch ()
  "I O T Λ Dispatch - Main control panel for all IOTA features.

Binding with use-package and general.el:

  (use-package iota
    :load-path \"~/path/to/iota\"
    :general
    (\"C-c i\" \\='iota-dispatch))

Or with vanilla Emacs:

  (global-set-key (kbd \"C-c i\") \\='iota-dispatch)"
  [:description
   (lambda ()
     (concat
      (propertize "ι • ο • τ • α" 'face '(:weight bold :height 1.2))
      "\n"
      (propertize "Not one iota more than needed." 'face 'shadow)
      "\n" (iota-dispatch--separator) "\n"))

   ["Configuration"
    ("c" "Config" iota-config-transient)
    ("t" "Theme" iota-theme-transient)]

   ["Display"
    ("m" "Modeline" iota-modeline-transient)
    ("d" "Dimmer" iota-dimmer-transient)]

   ["Windows"
    ("w" "Window" iota-window-transient)
    ("p" "Popup" iota-popup-transient)]

   ["Assistant"
    ("g" "Copilot" iota-copilot-transient)]

   ["Packages"
    ("l" "Packages" iota-package-transient)]

   ["Extras"
    ("s" "Screens" iota-screens-transient)
    ("?" "Splash" iota-splash-transient)]])

(provide 'iota-dispatch)
;;; iota-dispatch.el ends here
