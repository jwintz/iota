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
(require 'iota-box)
(require 'iota-theme)

;;; Demo Face Helpers

(defun iota-demo--accent-face ()
  "Get the accent face for demo highlighting."
  'iota-accent-face)

(defun iota-demo--heading-face ()
  "Get the face for demo headings."
  'iota-highlight-face)

(defun iota-demo--success-face ()
  "Get the success face for demo."
  'iota-success-face)

(defun iota-demo--muted-face ()
  "Get the muted face for demo."
  'iota-muted-face)

;;; Demo Navigation State

(defvar iota-demo--demos
  '(iota-demo
    iota-demo-theme-system
    iota-demo-box-styles
    iota-demo-widgets
    iota-demo-widgets-advanced
    iota-demo-widgets-animated
    iota-demo-layouts
    iota-demo-logos
    iota-demo-faces
    iota-demo-animations
    iota-demo-window-states
    iota-demo-window-focus
    iota-demo-window-effects
    iota-demo-window-dividers)
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
        (local-set-key (kbd "<backspace>") 'iota-demo-previous)
        (local-set-key (kbd "r") 'iota-demo--refresh-current)
        (local-set-key (kbd "g") 'iota-demo--refresh-current))
      ;; Enable dynamic separator updates
      (when (fboundp 'iota-box--enable-dynamic-separators)
        (iota-box--enable-dynamic-separators)))
    buf))

(defun iota-demo--refresh-current ()
  "Refresh the current demo buffer."
  (interactive)
  (let ((buf-name (buffer-name))
        (demo-map '(("*I O T Λ Demo*" . iota-demo)
                   ("*I O T Λ Theme Demo*" . iota-demo-theme-system)
                   ("*I O T Λ Box Styles*" . iota-demo-box-styles)
                   ("*I O T Λ Widgets Demo*" . iota-demo-widgets)
                   ("*I O T Λ Advanced Widgets*" . iota-demo-widgets-advanced)
                   ("*I O T Λ Animated Widgets*" . iota-demo-widgets-animated)
                   ("*I O T Λ Layouts Demo*" . iota-demo-layouts)
                   ("*I O T Λ Logo Demo*" . iota-demo-logos)
                   ("*I O T Λ Face Demo*" . iota-demo-faces)
                   ("*I O T Λ Animation Demo*" . iota-demo-animations)
                   ("*I O T Λ Active*" . iota-demo-window-states)
                   ("*I O T Λ Focus Demo*" . iota-demo-window-focus)
                   ("*I O T Λ Effects Demo*" . iota-demo-window-effects)
                   ("*I O T Λ Dividers Demo*" . iota-demo-window-dividers))))
    (when-let ((demo-fn (alist-get buf-name demo-map nil nil #'string=)))
      (funcall demo-fn)
      (message "Demo refreshed"))))

(defun iota-demo--quit-buffer ()
  "Quit all demo buffers, handling read-only mode."
  (interactive)
  (let ((inhibit-read-only t)
        (demo-buffer-patterns '("^\\*I O T Λ.*\\*$"
                               "^\\*IOTA.*\\*$"
                               "^\\*.*Demo\\*$")))
    ;; Kill all buffers matching demo patterns
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (cl-some (lambda (pattern)
                           (string-match-p pattern (buffer-name buf)))
                         demo-buffer-patterns))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (kill-buffer buf)))))
    (message "All demo buffers closed")))

(defun iota-demo--show-in-current-window (buffer)
  "Display BUFFER in current window, taking over the full frame."
  (let ((win (get-buffer-window buffer)))
    (if win
        (select-window win)
      (delete-other-windows)
      (switch-to-buffer buffer))))

(defun iota-demo--make-readonly-with-hint (next-demo-name)
  "Make current buffer read-only and show navigation hint.
NEXT-DEMO-NAME is the name of the next demo in sequence."
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (insert "\n\n")
    (iota-box-insert-separator 'double)
    (insert "\n")
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
    (insert (propertize "r/g" 'face '(:foreground "#39bae6")))
    (insert (propertize " to refresh  " 'face 'iota-muted-face))
    (insert (propertize "q" 'face '(:foreground "#39bae6")))
    (insert (propertize " to quit" 'face 'iota-muted-face)))
  (goto-char (point-min))
  ;; Make buffer read-only but keep cursor visible
  (setq buffer-read-only t)
  ;; Force cursor to be visible - don't hide it
  (setq-local cursor-type t)
  (setq-local cursor-in-non-selected-windows t))

(defun iota-demo--insert-header (title &optional subtitle)
  "Insert a uniform header with TITLE and optional SUBTITLE.
Uses consistent styling across all demo buffers."
  (insert "\n")
  (insert (propertize title
                     'face '(:weight bold :foreground "#39bae6" :height 1.4)))
  (insert "\n")
  (when subtitle
    (insert "\n")
    (insert (propertize subtitle 'face 'iota-muted-face))
    (insert "\n"))
  (insert "\n")
  (iota-box-insert-separator 'double)
  (insert "\n"))

(defun iota-demo--find-current-index ()
  "Find index of current demo based on buffer name."
  (let ((buf-name (buffer-name))
        (demo-buffers '("*I O T Λ Demo*"
                       "*I O T Λ Theme Demo*"
                       "*I O T Λ Box Styles*"
                       "*I O T Λ Widgets Demo*"
                       "*I O T Λ Advanced Widgets*"
                       "*I O T Λ Animated Widgets*"
                       "*I O T Λ Layouts Demo*"
                       "*I O T Λ Logo Demo*"
                       "*I O T Λ Face Demo*"
                       "*I O T Λ Animation Demo*"
                       "*I O T Λ Active*"
                       "*I O T Λ Focus Demo*"
                       "*I O T Λ Effects Demo*"
                       "*I O T Λ Dividers Demo*")))
    (or (cl-position buf-name demo-buffers :test #'string=) 0)))

(defun iota-demo-next ()
  "Go to next demo in sequence."
  (interactive)
  (let* ((current-idx (iota-demo--find-current-index))
         (next-idx (mod (1+ current-idx) (length iota-demo--demos)))
         (current-buf (current-buffer)))
    (setq iota-demo--current-index next-idx)
    ;; Call next demo first, then kill old buffer
    (funcall (nth iota-demo--current-index iota-demo--demos))
    ;; Now kill the old buffer
    (when (and (buffer-live-p current-buf)
               (string-match-p "^\\*.*Demo\\*\\|\\*IOTA.*\\*\\|\\*I O T Λ.*\\*" 
                              (buffer-name current-buf)))
      (with-current-buffer current-buf
        (let ((inhibit-read-only t))
          (kill-buffer current-buf))))))

(defun iota-demo-previous ()
  "Go to previous demo in sequence."
  (interactive)
  (let* ((current-idx (iota-demo--find-current-index))
         (prev-idx (mod (1- current-idx) (length iota-demo--demos)))
         (current-buf (current-buffer)))
    (setq iota-demo--current-index prev-idx)
    ;; Call previous demo first, then kill old buffer
    (funcall (nth iota-demo--current-index iota-demo--demos))
    ;; Now kill the old buffer
    (when (and (buffer-live-p current-buf)
               (string-match-p "^\\*.*Demo\\*\\|\\*IOTA.*\\*\\|\\*I O T Λ.*\\*" 
                              (buffer-name current-buf)))
      (with-current-buffer current-buf
        (let ((inhibit-read-only t))
          (kill-buffer current-buf))))))

;;; Logo Display Demo

;;;###autoload
(defun iota-demo-logos ()
  "Display all IOTA logo variants with active and inactive states."
  (interactive)
  (let ((buf (iota-demo--setup-buffer "*I O T Λ Logo Demo*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (iota-demo--insert-header "LOGO VARIANTS"
                                  "IOTA branding variants for different use cases")

        (insert (propertize "PRIMARY LOGO" 'face '(:weight bold :foreground "#39bae6")) "\n")
        (insert (propertize "Use: Main branding, README headers\n" 'face 'iota-muted-face))
        (insert "\n")
        (insert "  Active:   " (iota-logo-primary t) "\n")
        (insert "  Inactive: " (iota-logo-primary nil) "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        (insert (propertize "SECONDARY LOGO" 'face '(:weight bold :foreground "#39bae6")) "\n")
        (insert (propertize "Use: Technical docs, terminal splash screens\n" 'face 'iota-muted-face))
        (insert "\n")
        (insert "  Active:   " (iota-logo-secondary t) "\n")
        (insert "  Inactive: " (iota-logo-secondary nil) "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        (insert (propertize "TERTIARY LOGO" 'face '(:weight bold :foreground "#39bae6")) "\n")
        (insert (propertize "Use: Footers, version tags, status indicators\n" 'face 'iota-muted-face))
        (insert "\n")
        (insert "  Active:   " (iota-logo-tertiary t) "\n")
        (insert "  Inactive: " (iota-logo-tertiary nil) "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

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
        (iota-demo--insert-header "FACES"
                                  "Face definitions and color samples")

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

        (iota-demo--make-readonly-with-hint "Animations")))
    (iota-demo--show-in-current-window buf)))

;;; Widget Demo

;;;###autoload
(defun iota-demo-widgets ()
  "Demonstrate IOTA widget library - basic widgets."
  (interactive)
  (let ((buf (iota-demo--setup-buffer "*I O T Λ Widgets Demo*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (iota-demo--insert-header "WIDGET LIBRARY"
                                  "Modern terminal widgets with Unicode glyphs")

        ;; Progress bars - multiple styles
        (insert (propertize "PROGRESS BARS" 'face 'iota-accent-face) "\n\n")
        
        (insert "  " (propertize "Block style:" 'face '(:weight bold)) "\n")
        (insert "  " (iota-widget-progress-bar 35 100 :width 35 :style 'blocks :label "Loading ") "\n")
        (insert "  " (iota-widget-progress-bar 70 100 :width 35 :style 'blocks :label "Progress") "\n\n")

        (insert "  " (propertize "Smooth style (sub-character precision):" 'face '(:weight bold)) "\n")
        (insert "  " (iota-widget-progress-bar 33 100 :width 35 :style 'smooth :label "Smooth  " :smooth t) "\n")
        (insert "  " (iota-widget-progress-bar 66 100 :width 35 :style 'smooth :label "Precise " :smooth t) "\n\n")

        (insert "  " (propertize "Gradient style:" 'face '(:weight bold)) "\n")
        (insert "  " (iota-widget-progress-bar 85 100 :width 35 :style 'gradient :label "Gradient"
                                               :gradient '("#ff5555" "#ffa500" "#50fa7b")) "\n\n")

        (insert "  " (propertize "Other styles:" 'face '(:weight bold)) "\n")
        (insert "  " (iota-widget-progress-bar 50 100 :width 35 :style 'line :label "Line    ") "\n")
        (insert "  " (iota-widget-progress-bar 60 100 :width 35 :style 'dots :label "Dots    ") "\n")
        (insert "  " (iota-widget-progress-bar 75 100 :width 35 :style 'braille :label "Braille ") "\n")
        (insert "  " (iota-widget-progress-bar 40 100 :width 35 :style 'modern :label "Modern  ") "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Sparklines
        (insert (propertize "SPARKLINES" 'face 'iota-accent-face) "\n\n")
        (insert "  CPU usage:    " (iota-widget-sparkline '(3 5 4 8 6 9 10 8 12 15 13 11 9 7)) "\n")
        (insert "  Memory:       " (iota-widget-sparkline '(45 46 48 50 52 55 58 60 62 65 63 61)) "\n")
        (insert "  Network I/O:  " (iota-widget-sparkline '(1 3 8 15 12 5 2 1 4 20 35 25 10)) "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Status Indicators and Badges
        (insert (propertize "STATUS INDICATORS" 'face 'iota-accent-face) "\n\n")
        (insert "  Badges: ")
        (insert (iota-widget-badge "Success" 'success) " ")
        (insert (iota-widget-badge "Error" 'error) " ")
        (insert (iota-widget-badge "Warning" 'warning) " ")
        (insert (iota-widget-badge "Info" 'info) "\n\n")
        
        (insert "  Status: ")
        (insert (iota-widget-status-indicator 'success) " Complete  ")
        (insert (iota-widget-status-indicator 'error) " Failed  ")
        (insert (iota-widget-status-indicator 'warning) " Warning  ")
        (insert (iota-widget-status-indicator 'pending) " Pending\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Chips/Tags
        (insert (propertize "CHIPS / TAGS" 'face 'iota-accent-face) "\n\n")
        (insert "  ")
        (insert (iota-widget-chip "elisp" :type 'info))
        (insert " ")
        (insert (iota-widget-chip "v0.1.0" :type 'success))
        (insert " ")
        (insert (iota-widget-chip "beta" :type 'warning))
        (insert " ")
        (insert (iota-widget-chip "deprecated" :type 'error :dismissible t))
        (insert "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Banners
        (insert (propertize "BANNERS" 'face 'iota-accent-face) "\n\n")
        (insert (iota-widget-banner :message "Operation completed successfully!" :type 'success :style 'rounded))
        (insert "\n\n")
        (insert (iota-widget-banner :message "Warning: Please review settings" :type 'warning :style 'rounded))
        (insert "\n\n")

        (iota-demo--make-readonly-with-hint "Advanced Widgets")))
    (iota-demo--show-in-current-window buf)))

;;;###autoload
(defun iota-demo-widgets-advanced ()
  "Demonstrate advanced IOTA widgets - trees, cards, tables."
  (interactive)
  (let ((buf (iota-demo--setup-buffer "*I O T Λ Advanced Widgets*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (iota-demo--insert-header "ADVANCED WIDGETS"
                                  "Trees, cards, tables, timelines, and more")

        ;; Tree Widget
        (insert (propertize "TREE VIEW" 'face 'iota-accent-face) "\n\n")
        (let ((tree (iota-tree-node-create
                     :label "iota"
                     :expanded t
                     :children
                     (list
                      (iota-tree-node-create
                       :label "core"
                       :expanded t
                       :children
                       (list (iota-tree-node-create :label "iota-box.el")
                             (iota-tree-node-create :label "iota-faces.el")
                             (iota-tree-node-create :label "iota-theme.el")))
                      (iota-tree-node-create
                       :label "widgets"
                       :expanded t
                       :children
                       (list (iota-tree-node-create :label "iota-widgets.el")
                             (iota-tree-node-create :label "iota-tui.el")))
                      (iota-tree-node-create :label "README.md")))))
          (insert (iota-widget-tree tree)))
        (insert "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Card Widget
        (insert (propertize "CARDS / PANELS" 'face 'iota-accent-face) "\n\n")
        (insert (iota-widget-card
                 :title "System Status"
                 :content '("CPU: 45%  Memory: 62%  Disk: 78%"
                           "Uptime: 5 days, 3 hours"
                           "Load average: 1.25 1.50 1.75")
                 :footer "Last updated: just now"
                 :width 45
                 :style 'rounded))
        (insert "\n\n")

        ;; Interactive Collapsible (click to toggle!)
        (insert (propertize "INTERACTIVE COLLAPSIBLE" 'face 'iota-accent-face) 
                (propertize "  (click header to toggle)" 'face 'iota-muted-face) "\n\n")
        (iota-widget-collapsible-create
         :id 'demo-collapsible-1
         :title "Click me to collapse"
         :content '("This content can be hidden by clicking the header."
                   "The widget maintains its state across redraws."
                   "Try clicking the [+]/[-] indicator!"))
        (insert "\n\n")
        (iota-widget-collapsible-create
         :id 'demo-collapsible-2
         :title "Initially collapsed"
         :content "This section starts collapsed. Click to expand!"
         :collapsed t)
        (insert "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Tables
        (insert (propertize "TABLES" 'face 'iota-accent-face) "\n\n")
        (insert (iota-widget-table
                 :headers '("Component" "Status" "Version" "Health")
                 :rows '(("iota-box" "Active" "0.1.0" "✓")
                        ("iota-widgets" "Active" "0.1.0" "✓")
                        ("iota-animate" "Active" "0.1.0" "✓")
                        ("iota-theme" "Testing" "0.1.0" "⚠"))
                 :border 'rounded))
        (insert "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Timeline
        (insert (propertize "TIMELINE" 'face 'iota-accent-face) "\n\n")
        (insert (iota-widget-timeline
                 (list
                  '(:time "09:00" :title "Project started" :status complete)
                  '(:time "10:30" :title "Core widgets implemented" :description "Box, faces, theme" :status complete)
                  '(:time "14:00" :title "Advanced widgets added" :description "Trees, cards, tables" :status complete)
                  '(:time "16:00" :title "Animation system" :status in-progress)
                  '(:time "18:00" :title "Documentation" :status pending))))
        (insert "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Breadcrumb
        (insert (propertize "BREADCRUMBS" 'face 'iota-accent-face) "\n\n")
        (insert "  Arrows: " (iota-widget-breadcrumb '("Home" "Projects" "IOTA" "Demo") :style 'arrows) "\n")
        (insert "  Dots:   " (iota-widget-breadcrumb '("Home" "Projects" "IOTA" "Demo") :style 'dots) "\n")
        (insert "  Slash:  " (iota-widget-breadcrumb '("Home" "Projects" "IOTA" "Demo") :style 'slashes) "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Tabs
        (insert (propertize "TABS" 'face 'iota-accent-face) "\n\n")
        (insert "  Boxed:     " (iota-widget-tabs '("Overview" "Code" "Settings") :style 'boxed :selected 0) "\n\n")
        (insert "  Pills:     " (iota-widget-tabs '("Overview" "Code" "Settings") :style 'pills :selected 1) "\n\n")
        (insert "  Underline: " (iota-widget-tabs '("Overview" "Code" "Settings") :style 'underline :selected 2) "\n\n")

        (iota-demo--make-readonly-with-hint "Animated Widgets")))
    (iota-demo--show-in-current-window buf)))

;;;###autoload
(defun iota-demo-widgets-animated ()
  "Demonstrate animated IOTA widgets - spinners, progress, typing."
  (interactive)
  ;; Clean up any existing animations first
  (iota-widget-cleanup-all)
  
  (let ((buf (iota-demo--setup-buffer "*I O T Λ Animated Widgets*")))
    (with-current-buffer buf
      ;; Add cleanup hook for when buffer is killed or demo changes
      (add-hook 'kill-buffer-hook #'iota-widget-cleanup-all nil t)
      
      (let ((inhibit-read-only t))
        (iota-demo--insert-header "ANIMATED WIDGETS"
                                  "Live spinners, progress bars, and text effects")

        ;; Spinner styles showcase (static preview)
        (insert (propertize "SPINNER STYLES" 'face 'iota-accent-face) "\n\n")
        (insert "  Available styles (shown as first frame):\n\n")
        (dolist (style '(braille dots line arc box circle arrows bounce grow pulse snake bar))
          (let* ((frames (alist-get style iota-widget-spinner-styles))
                 (first-frame (when frames (aref frames 0))))
            (insert (format "    %s  %s\n" 
                           (iota-box-pad (symbol-name style) 10 'left)
                           (or first-frame "?")))))
        (insert "\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Live spinner demo - starts automatically!
        (insert (propertize "LIVE SPINNERS (Auto-started)" 'face 'iota-accent-face) "\n\n")
        
        (insert "  Braille: ")
        (iota-widget-spinner-create :id 'demo-braille :style 'braille :label "Loading...")
        (insert " \n")
        
        (insert "  Dots:    ")
        (iota-widget-spinner-create :id 'demo-dots :style 'dots :label "Processing...")
        (insert " \n")
        
        (insert "  Arc:     ")
        (iota-widget-spinner-create :id 'demo-arc :style 'arc :label "Syncing...")
        (insert " \n")
        
        (insert "  Pulse:   ")
        (iota-widget-spinner-create :id 'demo-pulse :style 'pulse :label "Working...")
        (insert " \n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Indeterminate progress - auto-started
        (insert (propertize "INDETERMINATE PROGRESS (Auto-started)" 'face 'iota-accent-face) "\n\n")
        
        (insert "  Bounce: ")
        (iota-widget-progress-indeterminate :id 'demo-bounce :width 25 :style 'bounce)
        (insert " \n\n")
        
        (insert "  Snake:  ")
        (iota-widget-progress-indeterminate :id 'demo-snake :width 25 :style 'snake)
        (insert " \n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Shimmer effect - auto-started
        (insert (propertize "SHIMMER EFFECT (Auto-started)" 'face 'iota-accent-face) "\n\n")
        (insert "  ")
        (iota-widget-shimmer :id 'demo-shimmer :width 40)
        (insert " \n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Loading skeletons (static)
        (insert (propertize "LOADING SKELETONS" 'face 'iota-accent-face) "\n\n")
        (insert "  Placeholder content while loading:\n\n")
        (insert "  " (iota-widget-skeleton 45 :height 1 :style 'bars) "\n")
        (insert "  " (iota-widget-skeleton 35 :height 1 :style 'bars) "\n")
        (insert "  " (iota-widget-skeleton 40 :height 1 :style 'bars) "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Interactive section
        (insert (propertize "INTERACTIVE" 'face 'iota-accent-face) "\n\n")
        (insert "  ")
        (insert-button "[ Start Typing Animation ]"
                       'action (lambda (_)
                                 (let ((buf (current-buffer)))
                                   (with-current-buffer buf
                                     (save-excursion
                                       (goto-char (point-max))
                                       (let ((inhibit-read-only t))
                                         (insert "\n  ")
                                         (iota-widget-typing-text
                                          "Welcome to I O T Λ — Not one iota more than needed."
                                          :id 'demo-typing
                                          :speed 25
                                          :face 'iota-accent-face))))))
                       'face '(:foreground "#39bae6" :weight bold))
        (insert "\n\n  ")
        (insert-button "[ Stop All Animations ]"
                       'action (lambda (_)
                                 (iota-widget-cleanup-all)
                                 (message "All animations stopped"))
                       'face '(:foreground "#ff5555" :weight bold))
        (insert "\n\n")

        (iota-demo--make-readonly-with-hint "Layouts")))
    (iota-demo--show-in-current-window buf)))

;;;###autoload
(defun iota-demo-layouts ()
  "Demonstrate IOTA layout system - columns, grids, gauges."
  (interactive)
  (let ((buf (iota-demo--setup-buffer "*I O T Λ Layouts Demo*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (iota-demo--insert-header "LAYOUT SYSTEM"
                                  "Columns, grids, and responsive layouts")

        ;; Vertical Gauges
        (insert (propertize "VERTICAL GAUGES" 'face 'iota-accent-face) "\n\n")
        (let ((gauge-lines (iota-widget-gauge-group
                            (list '(:value 75 :total 100 :label "CPU")
                                  '(:value 62 :total 100 :label "MEM")
                                  '(:value 45 :total 100 :label "DSK")
                                  '(:value 88 :total 100 :label "NET"))
                            :spacing 3)))
          (dolist (line gauge-lines)
            (insert "  " line "\n")))
        (insert "\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Column Layout
        (insert (propertize "COLUMN LAYOUT" 'face 'iota-accent-face) "\n\n")
        (let* ((col1 (iota-widget-card :title "Column 1" 
                                        :content "Left content\nwith multiple\nlines" 
                                        :width 25))
               (col2 (iota-widget-card :title "Column 2" 
                                        :content "Middle content\ncentered" 
                                        :width 25))
               (col3 (iota-widget-card :title "Column 3" 
                                        :content "Right content\nmore text" 
                                        :width 25)))
          (insert (iota-layout-columns (list col1 col2 col3) :spacing 2)))
        (insert "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Grid Layout
        (insert (propertize "GRID LAYOUT" 'face 'iota-accent-face) "\n\n")
        (let ((items (list
                      (iota-widget-card :title "Item 1" :content "Alpha" :width 18)
                      (iota-widget-card :title "Item 2" :content "Beta" :width 18)
                      (iota-widget-card :title "Item 3" :content "Gamma" :width 18)
                      (iota-widget-card :title "Item 4" :content "Delta" :width 18)
                      (iota-widget-card :title "Item 5" :content "Epsilon" :width 18)
                      (iota-widget-card :title "Item 6" :content "Zeta" :width 18))))
          (insert (iota-layout-grid items :columns 3 :spacing 2)))
        (insert "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Code Block
        (insert (propertize "CODE BLOCKS" 'face 'iota-accent-face) "\n\n")
        (insert (iota-widget-code-block
                 "(defun hello-iota ()
  \"Display IOTA greeting.\"
  (interactive)
  (message \"I O T Λ — Not one iota more.\"))"
                 :language "elisp"
                 :line-numbers t
                 :width 50))
        (insert "\n\n")

        (iota-demo--make-readonly-with-hint "Logos")))
    (iota-demo--show-in-current-window buf)))

;;; Animation Demo

;;;###autoload
(defun iota-demo-animations ()
  "Demonstrate IOTA animation capabilities with a 5-second demo."
  (interactive)
  (unless iota-animate-enabled
    (user-error "Animations are disabled. Set `iota-animate-enabled' to t"))

  (let ((buf (iota-demo--setup-buffer "*I O T Λ Animation Demo*"))
        (progress-marker nil)
        (timer nil)
        (total-duration 5.0))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (iota-demo--insert-header "ANIMATIONS"
                                  "5-second animation showcase with live progress")

        (insert (propertize "Watch the elements below animate!\n\n" 'face '(:weight bold)))

        (insert "Box Border: ")
        (insert (propertize "█████████████████████████" 'face 'iota-box-face))
        (insert "\n\n")

        (insert "Accent:     ")
        (insert (propertize "I O T Λ  ANIMATING!" 'face 'iota-accent-face))
        (insert "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Progress bar section
        (insert (propertize "PROGRESS:\n\n" 'face '(:weight bold :foreground "#39bae6")))
        (setq progress-marker (point-marker))
        (insert (iota-widget-progress-bar 0 100 :width 50 :style 'blocks :label "Animation"))
        (insert "\n\n")

        (insert (propertize "Phase 1: Color pulse (0-2s)\n" 'face 'iota-muted-face))
        (insert (propertize "Phase 2: Color morph (2-4s)\n" 'face 'iota-muted-face))
        (insert (propertize "Phase 3: Final flash (4-5s)\n" 'face 'iota-muted-face))
        (insert "\n")

        (iota-demo--make-readonly-with-hint "Window States")))

    (iota-demo--show-in-current-window buf)

    ;; Update progress bar continuously
    (let ((start-time (float-time)))
      (setq timer
            (run-with-timer 0 0.1
                          (lambda ()
                            (let* ((elapsed (- (float-time) start-time))
                                   (progress (min 100 (* (/ elapsed total-duration) 100))))
                              (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (let ((inhibit-read-only t))
                                    (save-excursion
                                      (goto-char progress-marker)
                                      (delete-region (point) (progn (forward-line 1) (point)))
                                      (insert (iota-widget-progress-bar
                                              (truncate progress) 100
                                              :width 50 :style 'blocks :label "Animation"))
                                      (insert "\n")))))
                              (when (>= elapsed total-duration)
                                (cancel-timer timer)))))))

    ;; Phase 1: Quick pulse on box face (0-2s)
    (run-with-timer 0.2 nil
                    (lambda ()
                      (when (buffer-live-p buf)
                        (iota-animate-pulse 'iota-box-face
                                           :duration 1.5
                                           :intensity 0.7
                                           :attribute :foreground))))

    ;; Phase 2: Color morph cycle (2-4s)
    (run-with-timer 2.0 nil
                    (lambda ()
                      (when (buffer-live-p buf)
                        (let ((original (face-attribute 'iota-accent-face :foreground nil t)))
                          (when (and original (stringp original))
                            (iota-animate-face 'iota-accent-face :foreground
                                             original "#ff6b6b"
                                             :duration 1.0
                                             :easing #'iota-animate-ease-in-out-cubic
                                             :finish-fn
                                             (lambda ()
                                               (when (buffer-live-p buf)
                                                 (iota-animate-face 'iota-accent-face :foreground
                                                                  "#ff6b6b" original
                                                                  :duration 1.0
                                                                  :easing #'iota-animate-ease-in-out-cubic)))))))))

    ;; Phase 3: Final flash and completion (4-5s)
    (run-with-timer 4.0 nil
                    (lambda ()
                      (when (and (buffer-live-p buf)
                               (fboundp 'iota-window-flash-current)
                               (get-buffer-window buf))
                        (with-selected-window (get-buffer-window buf)
                          (iota-window-flash-current)))))

    (run-with-timer 5.0 nil
                    (lambda ()
                      (message "Animation demo complete!")))))

;;; Window State Demos

;;;###autoload
(defun iota-demo-window-states ()
  "Demonstrate active/inactive window states.
Creates side-by-side windows showing visual differences."
  (interactive)
  (delete-other-windows)

  ;; Create horizontal split
  (split-window-horizontally)

  ;; Left window - switch to buffer first, then populate
  (let ((buf-left (iota-demo--setup-buffer "*I O T Λ Active*")))
    (switch-to-buffer buf-left)
    (let ((inhibit-read-only t))
      (iota-demo--insert-header "ACTIVE WINDOW STATE"
                                "Visual appearance when window has focus")

      (insert "This window is currently " (propertize "ACTIVE" 'face '(:weight bold :foreground "#39bae6")) ".\n\n")

      (insert (propertize "Features:" 'face '(:weight bold)) "\n")
      (insert "  • Full brightness colors\n")
      (insert "  • Bold accent: " (iota-logo-primary t) "\n")
      (insert "  • Enhanced visibility\n\n")

      (insert (propertize "Active Faces:" 'face '(:weight bold)) "\n")
      (insert "  " (propertize "Active Modeline" 'face 'iota-active-modeline-face) "\n")
      (insert "  " (propertize "Active Accent" 'face 'iota-active-accent-face) "\n")
      (insert "  " (propertize "Active Highlight" 'face 'iota-active-highlight-face) "\n\n")

      (iota-box-insert-separator 'single)
      (insert "\n")
      (insert (propertize "Switch windows with C-x o\n" 'face 'iota-muted-face))
      (insert (propertize "to see state transitions\n" 'face 'iota-muted-face))

      (iota-demo--make-readonly-with-hint "Window Focus")))

  ;; Right window - switch to buffer first, then populate
  (other-window 1)
  (let ((buf-right (get-buffer-create "*I O T Λ Inactive*")))
    (switch-to-buffer buf-right)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Set up local keymap (same as in iota-demo--setup-buffer)
      (use-local-map (make-sparse-keymap))
      (local-set-key (kbd "q") 'iota-demo--quit-buffer)
      (local-set-key (kbd "Q") 'iota-demo--quit-buffer)
      (local-set-key (kbd "n") 'iota-demo-next)
      (local-set-key (kbd "p") 'iota-demo-previous)
      (local-set-key (kbd "SPC") 'iota-demo-next)
      (local-set-key (kbd "DEL") 'iota-demo-previous)
      (local-set-key (kbd "<backspace>") 'iota-demo-previous)
      (local-set-key (kbd "r") 'iota-demo--refresh-current)
      (local-set-key (kbd "g") 'iota-demo--refresh-current)
      ;; Enable dynamic separator updates
      (when (fboundp 'iota-box--enable-dynamic-separators)
        (iota-box--enable-dynamic-separators))

      (iota-demo--insert-header "INACTIVE WINDOW STATE"
                                "Visual appearance when window loses focus")

      (insert "When inactive, this window is " (propertize "DIMMED" 'face '(:weight bold :foreground "#666666")) ".\n\n")

      (insert (propertize "Features:" 'face '(:weight bold)) "\n")
      (insert "  • Reduced brightness\n")
      (insert "  • Muted accent: " (iota-logo-primary nil) "\n")
      (insert "  • Desaturated colors\n\n")

      (insert (propertize "Inactive Faces:" 'face '(:weight bold)) "\n")
      (insert "  " (propertize "Inactive Modeline" 'face 'iota-inactive-modeline-face) "\n")
      (insert "  " (propertize "Inactive Accent" 'face 'iota-inactive-accent-face) "\n")
      (insert "  " (propertize "Inactive Highlight" 'face 'iota-inactive-highlight-face) "\n\n")

      (iota-box-insert-separator 'single)
      (insert "\n")

      (when iota-window-animate-transitions
        (insert (propertize "✓ " 'face 'iota-success-face))
        (insert "Animations enabled\n")
        (insert (propertize "  Watch smooth transitions!\n" 'face 'iota-muted-face)))

      (goto-char (point-min))
      (read-only-mode 1)))

  ;; Focus left window
  (other-window 1)

  (message "Switch windows with C-x o to see active/inactive transitions. Press 'n' for next demo."))

;;;###autoload
(defun iota-demo-window-focus ()
  "Demonstrate window focus transition animations."
  (interactive)
  (let ((buf (iota-demo--setup-buffer "*I O T Λ Focus Demo*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (iota-demo--insert-header "WINDOW FOCUS TRANSITIONS"
                                  "Animated focus transitions between windows")

        (insert (propertize "ANIMATED TRANSITIONS" 'face '(:weight bold :foreground "#39bae6")) "\n\n")

        (let ((status (if iota-window-animate-transitions
                         (propertize "✓ Enabled" 'face 'iota-success-face)
                       (propertize "✗ Disabled" 'face 'iota-muted-face))))
          (insert (format "  Status:     %s\n" status))
          (insert (format "  Duration:   %.2fs\n\n" iota-window-transition-duration)))

        (insert (propertize "Features:" 'face '(:weight bold)) "\n")
        (insert "  • Smooth color transitions when switching windows\n")
        (insert "  • Animated modeline face changes\n")
        (insert "  • Accent element fade effects\n")
        (insert "  • Customizable transition duration\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        (insert (propertize "COMMANDS" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert "  " (propertize "M-x iota-window-toggle-animations" 'face '(:foreground "#39bae6")) "\n")
        (insert "    Toggle transition animations on/off\n\n")

        (insert "  " (propertize "M-x iota-window-demo-modeline-animation" 'face '(:foreground "#39bae6")) "\n")
        (insert "    Preview modeline color animation\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        (insert (propertize "TRY IT:" 'face '(:weight bold)) "\n")
        (insert "  1. Split this window: ")
        (insert (propertize "C-x 2" 'face '(:foreground "#ffcc66")) "\n")
        (insert "  2. Switch windows: ")
        (insert (propertize "C-x o" 'face '(:foreground "#ffcc66")) "\n")
        (insert "  3. Watch the smooth color transitions!\n\n")

        (iota-demo--make-readonly-with-hint "Window Effects")))
    (iota-demo--show-in-current-window buf)))

;;;###autoload
(defun iota-demo-window-effects ()
  "Demonstrate window visual effects (pulse and flash)."
  (interactive)
  (let ((buf (iota-demo--setup-buffer "*I O T Λ Effects Demo*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (iota-demo--insert-header "WINDOW VISUAL EFFECTS"
                                  "Pulse and flash visual effects")

        (insert (propertize "PULSE EFFECT" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert "  Subtle pulse animation on window activation\n")
        (insert "  Provides visual feedback for window focus\n\n")

        (let ((status (if iota-window-pulse-on-activate
                         (propertize "✓ Enabled" 'face 'iota-success-face)
                       (propertize "✗ Disabled" 'face 'iota-muted-face))))
          (insert (format "  Status: %s\n\n" status)))

        (insert "  " (propertize "M-x iota-window-pulse-current" 'face '(:foreground "#39bae6")) "\n")
        (insert "    Pulse the current window accent\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        (insert (propertize "FLASH EFFECT" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert "  Brief flash animation for attention\n")
        (insert "  Useful for notifications and alerts\n\n")

        (insert "  " (propertize "M-x iota-window-flash-current" 'face '(:foreground "#39bae6")) "\n")
        (insert "    Flash the current window\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        (insert (propertize "TRY THE EFFECTS:" 'face '(:weight bold)) "\n\n")
        (insert "  Click the button to see a pulse effect:\n\n  ")
        (insert-button "[ Pulse This Window ]"
                      'action (lambda (_) (when (fboundp 'iota-window-pulse-current)
                                           (iota-window-pulse-current)))
                      'follow-link t
                      'face '(:foreground "#39bae6" :weight bold))
        (insert "\n\n  ")
        (insert-button "[ Flash This Window ]"
                      'action (lambda (_) (when (fboundp 'iota-window-flash-current)
                                           (iota-window-flash-current)))
                      'follow-link t
                      'face '(:foreground "#ffcc66" :weight bold))
        (insert "\n\n")

        (iota-demo--make-readonly-with-hint "Window Dividers")))
    (iota-demo--show-in-current-window buf)))

;;;###autoload
(defun iota-demo-window-dividers ()
  "Demonstrate window divider styles."
  (interactive)
  (let ((buf (iota-demo--setup-buffer "*I O T Λ Dividers Demo*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (iota-demo--insert-header "WINDOW DIVIDERS"
                                  "Divider styles and customization")

        (insert (propertize "DIVIDER STYLES" 'face '(:weight bold :foreground "#39bae6")) "\n\n")

        (insert "  " (propertize "plain" 'face '(:weight bold)) "\n")
        (insert "    Continuous vertical line using │ character\n")
        (insert "    Matches inactive box face color\n\n")

        (insert "  " (propertize "hidden" 'face '(:weight bold)) "\n")
        (insert "    No visible dividers\n")
        (insert "    Clean, minimalist appearance\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        (insert (propertize "CURRENT SETTINGS" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert (format "  Style:  %s\n\n" iota-window-divider-style))

        (iota-box-insert-separator 'single)
        (insert "\n")

        (insert (propertize "COMMANDS" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert "  " (propertize "M-x iota-window-cycle-divider-style" 'face '(:foreground "#39bae6")) "\n")
        (insert "    Cycle through available divider styles\n\n")

        (insert "  " (propertize "M-x iota-window-set-divider-style" 'face '(:foreground "#39bae6")) "\n")
        (insert "    Set a specific divider style\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        (insert (propertize "TRY IT:" 'face '(:weight bold)) "\n")
        (insert "  1. Split this window: ")
        (insert (propertize "C-x 3" 'face '(:foreground "#ffcc66")) "\n")
        (insert "  2. Run: ")
        (insert (propertize "M-x iota-window-cycle-divider-style" 'face '(:foreground "#ffcc66")) "\n")
        (insert "  3. Watch the divider style change!\n\n")

        (iota-demo--make-readonly-with-hint "Main Menu")))
    (iota-demo--show-in-current-window buf)))

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
        (iota-box-insert-separator 'double)
        (insert "\n")

        ;; Available Demos
        (insert (propertize "AVAILABLE DEMONSTRATIONS" 'face 'iota-accent-face) "\n\n")

        (insert (propertize "Theme & Visual:" 'face '(:weight bold)) "\n\n")
        (insert "  " (propertize "1. Theme System" 'face 'iota-accent-face) "\n")
        (insert "     State-based colors, transparency system\n\n")

        (insert "  " (propertize "2. Box Styles" 'face 'iota-accent-face) "\n")
        (insert "     Unicode box-drawing characters (7+ styles)\n\n")

        (insert (propertize "Widgets:" 'face '(:weight bold)) "\n\n")
        (insert "  " (propertize "3. Basic Widgets" 'face 'iota-accent-face) "\n")
        (insert "     Progress bars, badges, sparklines, banners\n\n")

        (insert "  " (propertize "4. Advanced Widgets" 'face 'iota-accent-face) " " (propertize "NEW" 'face 'iota-success-face) "\n")
        (insert "     Trees, cards, tables, timelines, tabs\n\n")

        (insert "  " (propertize "5. Animated Widgets" 'face 'iota-accent-face) " " (propertize "NEW" 'face 'iota-success-face) "\n")
        (insert "     Live spinners, typing effects, shimmers\n\n")

        (insert "  " (propertize "6. Layout System" 'face 'iota-accent-face) " " (propertize "NEW" 'face 'iota-success-face) "\n")
        (insert "     Columns, grids, vertical gauges, code blocks\n\n")

        (insert (propertize "Branding:" 'face '(:weight bold)) "\n\n")
        (insert "  " (propertize "7. Logos" 'face 'iota-accent-face) "\n")
        (insert "     IOTA branding variants\n\n")

        (insert "  " (propertize "8. Faces" 'face 'iota-accent-face) "\n")
        (insert "     Face definitions and color samples\n\n")

        (insert (propertize "Animation:" 'face '(:weight bold)) "\n\n")
        (insert "  " (propertize "9. Animations" 'face 'iota-accent-face) "\n")
        (insert "     Smooth transitions and easing functions\n\n")

        (insert (propertize "Window System:" 'face '(:weight bold)) "\n\n")
        (insert "  " (propertize "10. Window States" 'face 'iota-accent-face) "\n")
        (insert "     Active/inactive window distinction\n\n")

        (insert "  " (propertize "11. Window Focus" 'face 'iota-accent-face) "\n")
        (insert "     Animated focus transitions\n\n")

        (insert "  " (propertize "12. Window Effects" 'face 'iota-accent-face) "\n")
        (insert "     Pulse and flash effects\n\n")

        (insert "  " (propertize "13. Window Dividers" 'face 'iota-accent-face) "\n")
        (insert "     Divider styles and customization\n\n")

        (iota-box-insert-separator 'double)
        (insert "\n")

        ;; Quick Stats
        (insert (propertize "WIDGET LIBRARY OVERVIEW" 'face 'iota-accent-face) "\n\n")
        (insert "  " (propertize "Progress:" 'face '(:weight bold)) " blocks, smooth, gradient, line, dots, braille, modern\n")
        (insert "  " (propertize "Spinners:" 'face '(:weight bold)) " braille, dots, arc, circle, bounce, grow, pulse, snake\n")
        (insert "  " (propertize "Layouts: " 'face '(:weight bold)) " columns, rows, grids, cards, panels\n")
        (insert "  " (propertize "Data:    " 'face '(:weight bold)) " tables, trees, timelines, sparklines, gauges\n")
        (insert "  " (propertize "UI:      " 'face '(:weight bold)) " tabs, breadcrumbs, badges, chips, banners\n")
        (insert "  " (propertize "Effects: " 'face '(:weight bold)) " typing, reveal, shimmer, skeletons\n\n")

        (iota-box-insert-separator 'double)
        (insert "\n")

        ;; Status
        (insert (propertize "CURRENT STATUS" 'face 'iota-accent-face) "\n\n")

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

        (iota-box-insert-separator 'double)
        (insert "\n")

        ;; Footer
        (insert (propertize "Version: " 'face 'iota-muted-face))
        (insert (propertize (if (boundp 'iota-version) iota-version "0.1.0")
                           'face 'iota-accent-face))
        (insert "  ")
        (insert (propertize "Emacs: " 'face 'iota-muted-face))
        (insert (propertize emacs-version 'face 'iota-accent-face))
        (insert "\n")

        (iota-demo--make-readonly-with-hint "Theme System")))
    (iota-demo--show-in-current-window buf)))

;;; New Feature Demos

;;;###autoload
(defun iota-demo-theme-system ()
  "Demonstrate IOTA theme system and transparency features."
  (interactive)
  (let ((buffer (iota-demo--setup-buffer "*I O T Λ Theme Demo*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (iota-demo--insert-header "THEME SYSTEM"
                                  "State-based colors and transparency system")

        ;; Current theme info
        (insert (propertize "CURRENT THEME" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert (format "  Theme:        %s\n" (if custom-enabled-themes
                                                   (car custom-enabled-themes)
                                                 "default")))
        (insert (format "  Display:      %s\n" (if (display-graphic-p) "GUI" "Terminal")))
        (insert (format "  Colors:       %s\n\n" (display-color-cells)))

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Transparency features
        (insert (propertize "TRANSPARENCY SYSTEM" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (let ((transparent-mode (and (boundp 'iota-theme-transparent-mode)
                                    iota-theme-transparent-mode)))
          (insert (format "  Status:       %s\n"
                         (if transparent-mode
                             (propertize "✓ Enabled" 'face 'iota-success-face)
                           (propertize "✗ Disabled" 'face 'iota-muted-face))))
          (insert "\n")
          (insert "  Toggle with:  ")
          (insert (propertize "M-x iota-theme-transparent-mode" 'face '(:foreground "#39bae6")))
          (insert "\n\n")

          (when transparent-mode
            (insert (propertize "  Features:" 'face '(:weight bold)) "\n")
            (insert "    • Terminal background transparency\n")
            (insert "    • Face background removal\n")
            (insert "    • Window separator adaptation\n\n")))

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; State-based colors
        (insert (propertize "STATE-BASED FACES" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert "  IOTA adapts colors based on window state:\n\n")
        (insert "  Active window:    ")
        (insert (propertize "■ " 'face 'iota-active-box-face))
        (insert (propertize "Bright, saturated colors" 'face 'iota-active-accent-face))
        (insert "\n")
        (insert "  Inactive window:  ")
        (insert (propertize "■ " 'face 'iota-inactive-box-face))
        (insert (propertize "Dimmed, muted colors" 'face 'iota-inactive-accent-face))
        (insert "\n\n")

        (iota-box-insert-separator 'single)
        (insert "\n")

        ;; Box face system
        (insert (propertize "DYNAMIC BOX FACES" 'face '(:weight bold :foreground "#39bae6")) "\n\n")
        (insert "  Box drawing characters adapt to:\n")
        (insert "    • Current theme colors\n")
        (insert "    • Window focus state\n")
        (insert "    • Terminal/GUI mode\n\n")

        (insert "  Example box rendering:\n\n  ")
        (insert (iota-box-horizontal-line 50 'rounded 'iota-box-face))
        (insert "\n\n")

        (iota-demo--make-readonly-with-hint "Box Styles")))
    (iota-demo--show-in-current-window buffer)))

;;;###autoload
(defun iota-demo-box-styles ()
  "Demonstrate all available box styles."
  (interactive)
  (let ((buffer (iota-demo--setup-buffer "*I O T Λ Box Styles*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (iota-demo--insert-header "BOX STYLES"
                                  "Unicode box-drawing characters and styles")

        (dolist (style '(single double rounded heavy heavy-rounded
                               modern-thin modern-thick ascii))
          (when (iota-box-style-available-p style)
            (insert (propertize (format "%s" (upcase (symbol-name style)))
                               'face '(:weight bold :foreground "#39bae6")))
            (insert "\n\n")
            (insert (iota-box-render :content "IOTA Box Rendering"
                                    :style style
                                    :width 40))
            (insert "\n\n")
            (iota-box-insert-separator 'single)
            (insert "\n")))
        (goto-char (point-min))
        (iota-demo--make-readonly-with-hint "Widgets")))
    (iota-demo--show-in-current-window buffer)))

;;;###autoload
(defun iota-demo-all-features ()
  "Demonstrate all IOTA features in sequence."
  (interactive)
  (iota-demo-theme-system)
  (sit-for 2)
  (iota-demo-box-styles)
  (sit-for 2)
  (iota-demo-widgets))

(provide 'iota-demo)

;;; iota-demo.el ends here
