;;; iota-demo.el --- I O T Λ demo and testing utilities -*- lexical-binding: t -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Version: 0.1.0
;; Keywords: demo, testing
;; URL: https://github.com/yourusername/iota

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Demo and testing utilities for IOTA features.
;; Provides interactive demonstrations of modeline, animations,
;; widgets, faces, and window states.

;;; Code:

(require 'iota-splash)
(require 'iota-logos)
(require 'iota-faces)
(require 'iota-animate)
(require 'iota-window)
(require 'iota-widgets)
(require 'iota-modeline)

;;; Demo Navigation State

(defvar iota-demo--demos
  '(iota-demo
    iota-demo-logos
    iota-demo-faces
    iota-demo-widgets
    iota-demo-animations
    iota-demo-window-states)
  "List of demo functions in order.")

(defvar iota-demo--current-index 0
  "Current position in demo sequence.")

;;; Helper Functions

(defun iota-demo--setup-buffer (name)
  "Create and setup demo buffer NAME."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Set up local keymap
        (use-local-map (make-sparse-keymap))
        (local-set-key (kbd "q") 'iota-demo--quit-buffer)
        (local-set-key (kbd "Q") 'iota-demo--quit-buffer)
        (local-set-key (kbd "n") 'iota-demo-next)
        (local-set-key (kbd "p") 'iota-demo-previous)
        (local-set-key (kbd "SPC") 'iota-demo-next)
        (local-set-key (kbd "DEL") 'iota-demo-previous)
        (local-set-key (kbd "<backspace>") 'iota-demo-previous)))
    buf))

(defun iota-demo--quit-buffer ()
  "Quit current demo buffer, handling read-only mode."
  (interactive)
  (let ((inhibit-read-only t))
    (when (buffer-live-p (current-buffer))
      (kill-buffer (current-buffer)))))

(defun iota-demo--show-in-current-window (buffer)
  "Display BUFFER in current window, taking over the full frame."
  (delete-other-windows)
  (switch-to-buffer buffer))

(defun iota-demo--make-readonly-with-hint (next-demo-name)
  "Make current buffer read-only and show navigation hint.
NEXT-DEMO-NAME is the name of the next demo in sequence."
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (insert "\n\n")
    (insert "═══════════════════════════════════════════════════════════════\n\n")
    (insert (propertize "Press " 'face '(:weight bold)))
    (insert (propertize "n" 'face '(:foreground "#39bae6" :weight bold)))
    (insert (propertize " or " 'face '(:weight bold)))
    (insert (propertize "SPACE" 'face '(:foreground "#39bae6" :weight bold)))
    (when next-demo-name
      (insert (propertize (format " for %s" next-demo-name) 'face '(:weight bold))))
    (insert "\n")
    (insert (propertize "Press " 'face 'iota-muted-face))
    (insert (propertize "p" 'face '(:foreground "#39bae6")))
    (insert (propertize " or " 'face 'iota-muted-face))
    (insert (propertize "BACKSPACE" 'face '(:foreground "#39bae6")))
    (insert (propertize " for previous  " 'face 'iota-muted-face))
    (insert (propertize "q" 'face '(:foreground "#39bae6")))
    (insert (propertize " to quit" 'face 'iota-muted-face)))
  (goto-char (point-min))
  (read-only-mode 1))

(defun iota-demo--find-current-index ()
  "Find index of current demo based on buffer name."
  (let ((buf-name (buffer-name)))
    (cond
     ((string= buf-name "*I O T Λ Demo*") 0)
     ((string= buf-name "*I O T Λ Logo Demo*") 1)
     ((string= buf-name "*I O T Λ Face Demo*") 2)
     ((string= buf-name "*I O T Λ Widget Demo*") 3)
     ((string= buf-name "*I O T Λ Animation Demo*") 4)
     ((or (string= buf-name "*I O T Λ Active*")
          (string= buf-name "*I O T Λ Inactive*")) 5)
     (t 0))))

(defun iota-demo-next ()
  "Go to next demo in sequence."
  (interactive)
  (let ((current-idx (iota-demo--find-current-index)))
    (setq iota-demo--current-index (mod (1+ current-idx) (length iota-demo--demos)))
    (call-interactively (nth iota-demo--current-index iota-demo--demos))))

(defun iota-demo-previous ()
  "Go to previous demo in sequence."
  (interactive)
  (let ((current-idx (iota-demo--find-current-index)))
    (setq iota-demo--current-index (mod (1- current-idx) (length iota-demo--demos)))
    (call-interactively (nth iota-demo--current-index iota-demo--demos))))

;;; Logo Display Demo

;;;###autoload
(defun iota-demo-logos ()
  "Display all IOTA logo variants with active and inactive states."
  (interactive)
  (let ((buf (iota-demo--setup-buffer "*I O T Λ Logo Demo*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (insert "\n")
        (insert (propertize "LOGO VARIANTS" 'face '(:weight bold :foreground "#39bae6" :height 1.4)))
        (insert "\n\n")
        (insert "═══════════════════════════════════════════════════════════\n\n")

        (insert (propertize "PRIMARY LOGO" 'face '(:weight bold :foreground "#39bae6")) "\n")
        (insert (propertize "Use: Main branding, README headers\n" 'face 'iota-muted-face))
        (insert "\n")
        (insert "  Active:   " (iota-logo-primary t) "\n")
        (insert "  Inactive: " (iota-logo-primary nil) "\n\n")

        (insert "───────────────────────────────────────────────────────────\n\n")

        (insert (propertize "SECONDARY LOGO" 'face '(:weight bold :foreground "#39bae6")) "\n")
        (insert (propertize "Use: Technical docs, terminal splash screens\n" 'face 'iota-muted-face))
        (insert "\n")
        (insert "  Active:   " (iota-logo-secondary t) "\n")
        (insert "  Inactive: " (iota-logo-secondary nil) "\n\n")

        (insert "───────────────────────────────────────────────────────────\n\n")

        (insert (propertize "TERTIARY LOGO" 'face '(:weight bold :foreground "#39bae6")) "\n")
        (insert (propertize "Use: Footers, version tags, status indicators\n" 'face 'iota-muted-face))
        (insert "\n")
        (insert "  Active:   " (iota-logo-tertiary t) "\n")
        (insert "  Inactive: " (iota-logo-tertiary nil) "\n\n")

        (insert "───────────────────────────────────────────────────────────\n\n")

        (insert (propertize "TAGLINES" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert "  " (iota-logo-tagline) "\n")
        (insert "  " (iota-logo-tagline-alt) "\n")

        (iota-demo--make-readonly-with-hint "Faces")))
    (iota-demo--show-in-current-window buf)))

;;; Face Demo

;;;###autoload
(defun iota-demo-faces ()
  "Display all IOTA faces with their current appearance."
  (interactive)
  (let ((buf (iota-demo--setup-buffer "*I O T Λ Face Demo*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (insert "\n")
        (insert (propertize "FACES" 'face '(:weight bold :foreground "#39bae6" :height 1.4)))
        (insert "\n\n")
        (insert "═══════════════════════════════════════════════════════════\n\n")

        (let ((faces '((iota-face "Base face for IOTA elements")
                      (iota-box-face "Box drawing characters")
                      (iota-accent-face "Accent highlights")
                      (iota-muted-face "Muted/secondary text")
                      (iota-error-face "Error indicators")
                      (iota-success-face "Success indicators")
                      (iota-warning-face "Warning indicators")
                      (iota-active-modeline-face "Active window modeline")
                      (iota-inactive-modeline-face "Inactive window modeline")
                      (iota-active-accent-face "Active window accent")
                      (iota-inactive-accent-face "Inactive window accent")
                      (iota-active-highlight-face "Active highlights")
                      (iota-inactive-highlight-face "Inactive highlights")
                      (iota-active-box-face "Active window boxes")
                      (iota-inactive-box-face "Inactive window boxes"))))
          (dolist (face-info faces)
            (let* ((face (car face-info))
                   (desc (cadr face-info))
                   (fg (face-attribute face :foreground nil t))
                   (bg (face-attribute face :background nil t))
                   (weight (face-attribute face :weight nil t))
                   (fg-display (if (or (eq fg 'unspecified) (null fg)) "inherit" fg))
                   (bg-display (if (or (eq bg 'unspecified) (null bg)) "inherit" bg))
                   (weight-display (if (or (eq weight 'unspecified) (null weight)) "normal" (symbol-name weight))))
              (insert (propertize (format "%-32s" face) 'face '(:weight bold)))
              (insert " ")
              (insert (propertize "SAMPLE TEXT" 'face face))
              (insert "\n")
              (insert (propertize (format "  %s\n" desc) 'face 'iota-muted-face))
              (insert (format "  FG: %-16s BG: %-16s Weight: %s\n\n"
                            fg-display bg-display weight-display)))))

        (iota-demo--make-readonly-with-hint "Widgets")))
    (iota-demo--show-in-current-window buf)))

;;; Widget Demo

;;;###autoload
(defun iota-demo-widgets ()
  "Demonstrate IOTA widget library."
  (interactive)
  (let ((buf (iota-demo--setup-buffer "*I O T Λ Widget Demo*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (insert "\n")
        (insert (propertize "WIDGET LIBRARY" 'face '(:weight bold :foreground "#39bae6" :height 1.4)))
        (insert "\n\n")
        (insert "═══════════════════════════════════════════════════════════\n\n")

        ;; Progress bars
        (insert (propertize "PROGRESS BARS" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert "Block style:\n")
        (insert "  " (iota-widget-progress-bar 35 100 :width 40 :style 'blocks :label "Loading") "\n\n")
        (insert "  " (iota-widget-progress-bar 70 100 :width 40 :style 'blocks :label "Progress") "\n\n")

        (insert "Line style:\n")
        (insert "  " (iota-widget-progress-bar 45 100 :width 40 :style 'line :label "Status  ") "\n\n")

        (insert "Dot style:\n")
        (insert "  " (iota-widget-progress-bar 85 100 :width 40 :style 'dots :label "Complete") "\n\n")

        (insert "───────────────────────────────────────────────────────────\n\n")

        ;; Tables
        (insert (propertize "TABLES" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert (iota-widget-table
                :headers '("Name" "Type" "Status")
                :rows '(("Alpha" "Widget" "Active")
                       ("Beta" "Component" "Testing")
                       ("Gamma" "Module" "Complete"))
                :border 'rounded))
        (insert "\n\n")

        (insert "───────────────────────────────────────────────────────────\n\n")

        ;; Badges
        (insert (propertize "BADGES" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert "  " (iota-widget-badge "Success" 'success) "  ")
        (insert (iota-widget-badge "Error" 'error) "  ")
        (insert (iota-widget-badge "Warning" 'warning) "  ")
        (insert (iota-widget-badge "Info" 'info) "\n\n")

        (insert "───────────────────────────────────────────────────────────\n\n")

        ;; Sparkline
        (insert (propertize "SPARKLINE" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert "  Metric trend: " (iota-widget-sparkline '(3 5 4 8 6 9 10 8 12 15 13)) "\n\n")

        (insert "───────────────────────────────────────────────────────────\n\n")

        ;; Banner
        (insert (propertize "BANNERS" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert (iota-widget-banner :message "Operation completed successfully!" :type 'success :style 'rounded))
        (insert "\n\n")
        (insert (iota-widget-banner :message "Warning: Please review settings" :type 'warning :style 'rounded))
        (insert "\n\n")
        (insert (iota-widget-banner :message "Error: Connection failed" :type 'error :style 'rounded))
        (insert "\n\n")

        (iota-demo--make-readonly-with-hint "Animations")))
    (iota-demo--show-in-current-window buf)))

;;; Animation Demo

;;;###autoload
(defun iota-demo-animations ()
  "Demonstrate IOTA animation capabilities."
  (interactive)
  (unless iota-animate-enabled
    (user-error "Animations are disabled. Set `iota-animate-enabled' to t"))

  (let ((buf (iota-demo--setup-buffer "*I O T Λ Animation Demo*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (insert "\n")
        (insert (propertize "ANIMATIONS" 'face '(:weight bold :foreground "#39bae6" :height 1.4)))
        (insert "\n\n")
        (insert "═══════════════════════════════════════════════════════════\n\n")

        (insert (propertize "Watch the elements below animate!\n\n" 'face '(:weight bold)))

        (insert "Box Border: ")
        (insert (propertize "█████████████████████████" 'face 'iota-box-face))
        (insert "\n\n")

        (insert "Accent:     ")
        (insert (propertize "I O T Λ  ANIMATING!" 'face 'iota-accent-face))
        (insert "\n\n")

        (insert "Success:    ")
        (insert (propertize "▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓" 'face 'iota-success-face))
        (insert "\n\n")

        (insert "Warning:    ")
        (insert (propertize "▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒" 'face 'iota-warning-face))
        (insert "\n\n")

        (insert "───────────────────────────────────────────────────────────\n\n")
        (insert (propertize "Animations will run for ~20 seconds.\n" 'face 'iota-muted-face))
        (insert (propertize "Smooth transitions with cubic easing.\n" 'face 'iota-muted-face))
        (insert (propertize "Watch the colored elements pulse and morph!\n" 'face 'iota-muted-face))
        (insert "\n")

        (iota-demo--make-readonly-with-hint "Window States")))

    (iota-demo--show-in-current-window buf)

    ;; Start animation sequence
    (run-with-timer 0.5 nil
                    (lambda ()
                      ;; 1. Pulse box face (2s)
                      (iota-animate-pulse 'iota-box-face
                                         :duration 2.0
                                         :intensity 0.6
                                         :attribute :foreground)))

    (run-with-timer 3.0 nil
                    (lambda ()
                      ;; 2. Pulse accent (2.5s)
                      (iota-animate-pulse 'iota-accent-face
                                         :duration 2.5
                                         :intensity 0.7
                                         :attribute :foreground)))

    (run-with-timer 6.0 nil
                    (lambda ()
                      ;; 3. Color morph box (4s cycle)
                      (let ((original (face-attribute 'iota-box-face :foreground nil t)))
                        (when (and original (stringp original))
                          (iota-animate-face 'iota-box-face :foreground
                                           original "#ff6b6b"
                                           :duration 2.0
                                           :easing #'iota-animate-ease-in-out-cubic
                                           :finish-fn
                                           (lambda ()
                                             (iota-animate-face 'iota-box-face :foreground
                                                              "#ff6b6b" original
                                                              :duration 2.0
                                                              :easing #'iota-animate-ease-in-out-cubic)))))))

    (run-with-timer 11.0 nil
                    (lambda ()
                      ;; 4. Pulse success (2s)
                      (iota-animate-pulse 'iota-success-face
                                         :duration 2.0
                                         :intensity 0.5
                                         :attribute :foreground)))

    (run-with-timer 14.0 nil
                    (lambda ()
                      ;; 5. Pulse warning (2s)
                      (iota-animate-pulse 'iota-warning-face
                                         :duration 2.0
                                         :intensity 0.6
                                         :attribute :foreground)))

    (run-with-timer 17.0 nil
                    (lambda ()
                      ;; 6. Color morph accent (4s cycle)
                      (let ((original (face-attribute 'iota-accent-face :foreground nil t)))
                        (when (and original (stringp original))
                          (iota-animate-face 'iota-accent-face :foreground
                                           original "#ffcc66"
                                           :duration 1.5
                                           :easing #'iota-animate-ease-in-out-cubic
                                           :finish-fn
                                           (lambda ()
                                             (iota-animate-face 'iota-accent-face :foreground
                                                              "#ffcc66" original
                                                              :duration 1.5
                                                              :easing #'iota-animate-ease-in-out-cubic)))))))

    (run-with-timer 20.5 nil
                    (lambda ()
                      ;; 7. Grand finale - window flash
                      (when (and (fboundp 'iota-window-flash-current)
                               (get-buffer-window "*I O T Λ Animation Demo*"))
                        (with-selected-window (get-buffer-window "*I O T Λ Animation Demo*")
                          (iota-window-flash-current)))
                      (message "Animation demo complete!")))))

;;; Window State Demo

;;;###autoload
(defun iota-demo-window-states ()
  "Demonstrate active/inactive window states.
Creates side-by-side windows showing visual differences."
  (interactive)
  (delete-other-windows)

  ;; Create horizontal split
  (split-window-horizontally)

  ;; Left window
  (let ((buf-left (iota-demo--setup-buffer "*I O T Λ Active*")))
    (with-current-buffer buf-left
      (let ((inhibit-read-only t))
        (insert "\n")
        (insert (propertize "ACTIVE WINDOW STATE" 'face '(:weight bold :foreground "#39bae6" :height 1.4)))
        (insert "\n\n")
        (insert "═══════════════════════════════════════════\n\n")

        (insert "This window is currently " (propertize "ACTIVE" 'face '(:weight bold :foreground "#39bae6")) ".\n\n")

        (insert (propertize "Features:" 'face '(:weight bold)) "\n")
        (insert "  • Full brightness colors\n")
        (insert "  • Bold accent: " (iota-logo-primary t) "\n")
        (insert "  • Enhanced visibility\n\n")

        (insert (propertize "Active Faces:" 'face '(:weight bold)) "\n")
        (insert "  " (propertize "Active Modeline" 'face 'iota-active-modeline-face) "\n")
        (insert "  " (propertize "Active Accent" 'face 'iota-active-accent-face) "\n")
        (insert "  " (propertize "Active Highlight" 'face 'iota-active-highlight-face) "\n\n")

        (insert "───────────────────────────────────────────\n\n")
        (insert (propertize "Switch windows with C-x o\n" 'face 'iota-muted-face))
        (insert (propertize "to see state transitions\n" 'face 'iota-muted-face))

        (iota-demo--make-readonly-with-hint nil)))
    (switch-to-buffer buf-left))

  ;; Right window
  (other-window 1)
  (let ((buf-right (iota-demo--setup-buffer "*I O T Λ Inactive*")))
    (with-current-buffer buf-right
      (let ((inhibit-read-only t))
        (insert "\n")
        (insert (propertize "INACTIVE WINDOW STATE" 'face '(:weight bold :foreground "#39bae6" :height 1.4)))
        (insert "\n\n")
        (insert "═══════════════════════════════════════════\n\n")

        (insert "When inactive, this window is " (propertize "DIMMED" 'face '(:weight bold :foreground "#666666")) ".\n\n")

        (insert (propertize "Features:" 'face '(:weight bold)) "\n")
        (insert "  • Reduced brightness\n")
        (insert "  • Muted accent: " (iota-logo-primary nil) "\n")
        (insert "  • Desaturated colors\n\n")

        (insert (propertize "Inactive Faces:" 'face '(:weight bold)) "\n")
        (insert "  " (propertize "Inactive Modeline" 'face 'iota-inactive-modeline-face) "\n")
        (insert "  " (propertize "Inactive Accent" 'face 'iota-inactive-accent-face) "\n")
        (insert "  " (propertize "Inactive Highlight" 'face 'iota-inactive-highlight-face) "\n\n")

        (insert "───────────────────────────────────────────\n\n")

        (when iota-window-animate-transitions
          (insert (propertize "✓ " 'face 'iota-success-face))
          (insert "Animations enabled\n")
          (insert (propertize "  Watch smooth transitions!\n" 'face 'iota-muted-face)))

        (iota-demo--make-readonly-with-hint nil)))
    (switch-to-buffer buf-right))

  ;; Focus left window
  (other-window 1)

  (message "Switch windows with C-x o to see active/inactive transitions. Press 'n' to return to main demo."))

;;; Main Demo

;;;###autoload
(defun iota-demo ()
  "Run comprehensive IOTA feature demonstration.
Opens in current buffer with organized sections."
  (interactive)
  (setq iota-demo--current-index 0)

  (let ((buf (iota-demo--setup-buffer "*I O T Λ Demo*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        ;; Title
        (insert "\n")
        (insert (propertize "I O T Λ  DEMONSTRATION" 'face '(:weight bold :foreground "#39bae6" :height 1.6)))
        (insert "\n\n")
        (insert (propertize (iota-logo-tagline) 'face 'iota-accent-face))
        (insert "\n\n")
        (insert "═══════════════════════════════════════════════════════════════\n\n")

        ;; Available Demos
        (insert (propertize "INTERACTIVE TOUR" 'face '(:weight bold :foreground "#39bae6" :height 1.2)) "\n\n")

        (insert (propertize "Visual Demos:" 'face '(:weight bold)) "\n\n")
        (insert "  " (propertize "1. Logos" 'face '(:foreground "#39bae6")) "\n")
        (insert "     Display all logo variants (primary, secondary, tertiary)\n\n")

        (insert "  " (propertize "2. Faces" 'face '(:foreground "#39bae6")) "\n")
        (insert "     Show all IOTA face definitions with samples\n\n")

        (insert "  " (propertize "3. Widgets" 'face '(:foreground "#39bae6")) "\n")
        (insert "     Showcase widget library (progress bars, tables, badges, etc.)\n\n")

        (insert (propertize "Interactive Demos:" 'face '(:weight bold)) "\n\n")
        (insert "  " (propertize "4. Animations" 'face '(:foreground "#39bae6")) "\n")
        (insert "     Demonstrate smooth animation effects and transitions\n\n")

        (insert "  " (propertize "5. Window States" 'face '(:foreground "#39bae6")) "\n")
        (insert "     Show active/inactive window distinction (split view)\n\n")

        (insert "═══════════════════════════════════════════════════════════════\n\n")

        ;; Configuration
        (insert (propertize "CONFIGURATION" 'face '(:weight bold :foreground "#39bae6" :height 1.2)) "\n\n")

        (insert (propertize "Enable Features:" 'face '(:weight bold)) "\n\n")
        (insert "  " (propertize "M-x iota-modeline-mode" 'face '(:foreground "#39bae6")) "\n")
        (insert "    Enable IOTA modeline with box decorations\n\n")

        (insert "  " (propertize "M-x iota-window-mode" 'face '(:foreground "#39bae6")) "\n")
        (insert "    Enable window focus animations and tracking\n\n")

        (insert (propertize "Quick Settings:" 'face '(:weight bold)) "\n\n")
        (insert "  " (propertize "M-x iota-modeline-cycle-style" 'face '(:foreground "#ffcc66")) "\n")
        (insert "    Cycle through box styles (single/double/rounded/heavy)\n\n")

        (insert "  " (propertize "M-x iota-modeline-cycle-preset" 'face '(:foreground "#ffcc66")) "\n")
        (insert "    Cycle through segment presets (minimal/standard/full)\n\n")

        (insert "  " (propertize "M-x iota-modeline-toggle-position" 'face '(:foreground "#ffcc66")) "\n")
        (insert "    Toggle between header-line and mode-line\n\n")

        (insert "═══════════════════════════════════════════════════════════════\n\n")

        ;; Status
        (insert (propertize "CURRENT STATUS" 'face '(:weight bold :foreground "#39bae6" :height 1.2)) "\n\n")

        (let ((modeline-status (if (bound-and-true-p iota-modeline-mode)
                                   (propertize "✓ Enabled" 'face 'iota-success-face)
                                 (propertize "✗ Disabled" 'face 'iota-muted-face)))
              (window-status (if (bound-and-true-p iota-window-mode)
                                (propertize "✓ Enabled" 'face 'iota-success-face)
                              (propertize "✗ Disabled" 'face 'iota-muted-face)))
              (animate-status (if iota-animate-enabled
                                 (propertize "✓ Enabled" 'face 'iota-success-face)
                               (propertize "✗ Disabled" 'face 'iota-muted-face))))
          (insert (format "  Modeline Mode:  %s\n" modeline-status))
          (insert (format "  Window Mode:    %s\n" window-status))
          (insert (format "  Animations:     %s\n\n" animate-status)))

        (when (bound-and-true-p iota-modeline-mode)
          (insert (format "  Box Style:      %s\n" iota-modeline-box-style))
          (insert (format "  Preset:         %s\n" iota-modeline-segments-preset))
          (insert (format "  Position:       %s\n\n" iota-modeline-position)))

        (insert "═══════════════════════════════════════════════════════════════\n\n")

        ;; Footer
        (insert (propertize "Version: " 'face 'iota-muted-face))
        (insert (propertize (if (boundp 'iota-version) iota-version "0.1.0")
                           'face '(:foreground "#39bae6")))
        (insert "  ")
        (insert (propertize "Emacs: " 'face 'iota-muted-face))
        (insert (propertize emacs-version 'face '(:foreground "#39bae6")))
        (insert "\n")

        (iota-demo--make-readonly-with-hint "Logos")))
    (iota-demo--show-in-current-window buf)))

(provide 'iota-demo)

;;; iota-demo.el ends here
