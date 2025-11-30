;;; iota.el --- Minimal Terminal Interface for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; URL: https://github.com/jwintz/iota
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (use-package "2.4"))
;; Keywords: faces, modeline, tui, modal

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; I O T Λ brings modern CLI/TUI aesthetics to Emacs through:
;; - A decorated header-line modeline with box-drawing characters
;; - A theme-agnostic TUI library for various UI components
;; - Animation support for color transitions and visual feedback
;; - Native semantic modal editing via modalka
;;
;; Modal Editing (NEW):
;;   IOTA now includes a native semantic modal system built on modalka.
;;   Keys map directly to Emacs commands while eliminating modifier chords:
;;     n → C-n (next-line)     w → M-w (copy)
;;     p → C-p (previous-line) y → C-y (paste)
;;     SPC → C-SPC (set-mark)  W → C-w (cut)
;;
;; Quick Start:
;;   (require 'iota)
;;   (iota-modeline-mode 1)  ; Enable styled modeline
;;   (iota-modal-mode 1)     ; Enable modal editing
;;
;; Tutorial:
;;   M-x iota-tutorial       ; Interactive modal editing tutorial
;;
;; Customize:
;;   M-x customize-group RET iota RET
;;
;; Documentation:
;;   See README.md and https://github.com/jwintz/iota

;;; Code:

;; Core utilities (load first)
(require 'iota-utils)
(require 'iota-cache)
(require 'iota-timers)
(require 'iota-update)

;; UI components
(require 'iota-box)
(require 'iota-segment)
(require 'iota-segments)
(require 'iota-theme)
(require 'iota-tui)
(require 'iota-animate)
(require 'iota-modeline)
(require 'iota-widgets)
(require 'iota-splash)
(require 'iota-logos)
(require 'iota-window)
(require 'iota-demo)
(require 'iota-config)
(require 'iota-perf)

;; Optional modules
(require 'iota-theme-transparent)
(require 'iota-modal)
(require 'iota-leader)
(require 'iota-tutorial)
(require 'iota-keycast)

;;; Configuration

(defgroup iota nil
  "Minimal Terminal Interface for Emacs."
  :group 'faces
  :prefix "iota-")

;;; Version

(defconst iota-version "0.1.0"
  "I O T Λ version string.")

;;;###autoload
(defun iota-version ()
  "Display I O T Λ version information."
  (interactive)
  (message "I O T Λ v%s — Not one iota more than needed." iota-version))

;;; Setup Wizard

;;;###autoload
(defun iota-setup ()
  "Interactive setup wizard for I O T Λ."
  (interactive)
  (let ((choices '((?1 . minimal)
                   (?2 . standard)
                   (?3 . full))))
    (message "I O T Λ Setup Wizard")
    (sit-for 0.5)
    
    ;; Choose preset
    (let ((preset (cdr (assq (read-char-choice
                              (concat "Choose preset:\n"
                                      "1 - Minimal (buffer name, mode, position)\n"
                                      "2 - Standard (+ VCS info)\n"
                                      "3 - Full (+ size, time, battery)\n"
                                      "Choice: ")
                              '(?1 ?2 ?3))
                             choices))))
      (setq iota-modeline-segments-preset preset)
      (message "Preset: %s" preset)
      (sit-for 0.5))
    
    ;; Choose style
    (let ((style-choices '((?1 . single)
                          (?2 . double)
                          (?3 . rounded)
                          (?4 . heavy))))
      (let ((style (cdr (assq (read-char-choice
                               (concat "Choose box style:\n"
                                       "1 - Single line\n"
                                       "2 - Double line\n"
                                       "3 - Rounded (recommended)\n"
                                       "4 - Heavy line\n"
                                       "Choice: ")
                               '(?1 ?2 ?3 ?4))
                              style-choices))))
        (setq iota-modeline-box-style style)
        (message "Style: %s" style)
        (sit-for 0.5)))
    
    ;; Ask about modal editing
    (when (y-or-n-p "Enable modal editing (ergonomic keys without modifier chords)? ")
      (iota-modal-mode 1)
      (iota-leader-mode 1)
      (message "Modal editing enabled! Press ESC for COMMAND mode, 'i' for INSERT mode.")
      (sit-for 1))

    ;; Enable modeline
    (iota-modeline-mode 1)

    ;; Enable window mode for active/inactive distinction
    (iota-window-mode 1)

    ;; Offer tutorial
    (when (and (bound-and-true-p iota-modal-mode)
               (y-or-n-p "Would you like to take the modal editing tutorial? "))
      (iota-tutorial))

    (message "I O T Λ setup complete! Modeline and window tracking enabled.")))

;;;###autoload
(defun iota-quickstart ()
  "Enable all IOTA features with sensible defaults.
This is the easiest way to start using IOTA."
  (interactive)
  ;; Enable modeline with rounded style
  (setq iota-modeline-box-style 'rounded)
  (setq iota-modeline-segments-preset 'standard)
  (iota-modeline-mode 1)
  
  ;; Enable window mode for active/inactive distinction
  (iota-window-mode 1)
  
  ;; Enable modal editing
  (iota-modal-mode 1)
  (iota-leader-mode 1)
  
  (message "IOTA quickstart complete! Modal editing enabled. Press ESC for COMMAND mode."))

;;; Quick Commands

;;; Line Number Prevention

(defun iota-prevent-line-numbers (&rest _)
  "Prevent line numbers from being enabled.
This function overrides all line number enabling functions to do nothing."
  (interactive)
  nil)

;;;###autoload
(defun iota-disable-line-numbers ()
  "Disable all line number modes by advising their functions.
This prevents any code (including meow-tutor) from enabling line numbers."
  (interactive)
  ;; First, turn off any currently active line numbers
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (when (bound-and-true-p global-display-line-numbers-mode)
    (global-display-line-numbers-mode -1))
  (when (bound-and-true-p linum-mode)
    (linum-mode -1))

  ;; Then prevent them from being enabled again
  (advice-add 'display-line-numbers-mode :override #'iota-prevent-line-numbers)
  (advice-add 'global-display-line-numbers-mode :override #'iota-prevent-line-numbers)
  (advice-add 'linum-mode :override #'iota-prevent-line-numbers))

;;;###autoload
(defun iota-enable-line-numbers ()
  "Re-enable line number modes by removing advice.
This removes the prevention added by `iota-disable-line-numbers'."
  (interactive)
  (advice-remove 'display-line-numbers-mode #'iota-prevent-line-numbers)
  (advice-remove 'global-display-line-numbers-mode #'iota-prevent-line-numbers)
  (advice-remove 'linum-mode #'iota-prevent-line-numbers)
  (message "Line numbers re-enabled"))

(defcustom iota-prevent-line-numbers nil
  "When non-nil, prevent any code from enabling line numbers.
This is useful to prevent packages like meow-tutor from showing line numbers.
When enabled, line number modes will be permanently disabled."
  :type 'boolean
  :group 'iota
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (iota-disable-line-numbers)
           (iota-enable-line-numbers))))

;;; Integration Helpers

;;;###autoload
(defun iota-reload ()
  "Reload I O T Λ (useful during development)."
  (interactive)
  (when iota-modeline-mode
    (iota-modeline-mode -1))
  (when (bound-and-true-p iota-modal-mode)
    (iota-modal-mode -1))
  (when (bound-and-true-p iota-leader-mode)
    (iota-leader-mode -1))
  (mapc (lambda (feature)
          (when (featurep feature)
            (unload-feature feature t)))
        '(iota-tutorial iota-leader iota-modal
          iota-perf iota-config iota-demo iota-window iota-logos 
          iota-splash iota-faces iota-widgets iota-modeline 
          iota-animate iota-segments iota-segment iota-tui 
          iota-theme-transparent iota-theme 
          iota-box iota))
  (require 'iota)
  (iota-modeline-mode 1)
  (message "I O T Λ reloaded"))

;;; Hooks for Integration

(defvar iota-after-load-hook nil
  "Hook run after IOTA is loaded.")

(run-hooks 'iota-after-load-hook)

(provide 'iota)
;;; iota.el ends here
