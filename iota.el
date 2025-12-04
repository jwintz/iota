;;; iota.el --- Minimal Terminal Interface for Emacs -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; URL: https://github.com/jwintz/iota
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (use-package "2.4"))
;; Keywords: faces, modeline

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; I O T Λ brings modern CLI/TUI aesthetics to Emacs through:
;; - A decorated header-line modeline with box-drawing characters
;; - Animation support for color transitions and visual feedback
;;
;; Quick Start:
;;   (require 'iota)
;;   (iota-modeline-mode 1)  ; Enable styled modeline
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
(require 'iota-theme)
(require 'iota-animate)
(require 'iota-popup)
(require 'iota-modeline)
(require 'iota-dimmer)
(require 'iota-splash)
(require 'iota-logos)
(require 'iota-window)
(require 'iota-config)

;; Optional modules
(require 'iota-theme-transparent)

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
    
    ;; Enable modeline
    (iota-modeline-mode 1)

    ;; Enable window mode for active/inactive distinction
    (iota-window-mode 1)

    (message "I O T Λ setup complete! Modeline and window tracking enabled.")))

;;;###autoload
(defun iota-quickstart ()
  "Enable all IOTA features with sensible defaults.
This is the easiest way to start using IOTA."
  (interactive)
  ;; Enable modeline with rounded style
  (setq iota-modeline-box-style 'rounded)
  (iota-modeline-mode 1)

  ;; Enable popup handling for which-key, transient, etc.
  (iota-popup-mode 1)

  ;; Enable window mode for active/inactive distinction
  (iota-window-mode 1)

  (message "IOTA quickstart complete! Modeline, popup, and window tracking enabled."))

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

(defcustom iota-prevent-line-numbers t
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
  (when (bound-and-true-p iota-popup-mode)
    (iota-popup-mode -1))
  (mapc (lambda (feature)
          (when (featurep feature)
            (unload-feature feature t)))
        '(iota-config iota-window iota-logos
          iota-splash iota-faces iota-popup iota-modeline
          iota-animate
          iota-theme-transparent iota-theme
          iota-box iota))
  (require 'iota)
  (iota-modeline-mode 1)
  (iota-popup-mode 1)
  (message "I O T Λ reloaded"))

;;; Hooks for Integration

(defvar iota-after-load-hook nil
  "Hook run after IOTA is loaded.")

(run-hooks 'iota-after-load-hook)

;;; Automatic Activation

;; Enable IOTA modes by default when package is loaded
(iota-modeline-mode 1)
(iota-popup-mode 1)
(iota-window-mode 1)

(provide 'iota)
;;; iota.el ends here
