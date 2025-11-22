;;; iota.el --- Minimal Terminal Interface for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; URL: https://github.com/iota/iota
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))
;; Keywords: faces, modeline, tui

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
;;
;; Quick Start:
;;   (require 'iota)
;;   (iota-modeline-mode 1)
;;
;; Customize:
;;   M-x customize-group RET iota RET
;;
;; Documentation:
;;   See README.md and https://github.com/iota/iota

;;; Code:

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

;;; Quick Commands

;;; Integration Helpers

;;;###autoload
(defun iota-reload ()
  "Reload I O T Λ (useful during development)."
  (interactive)
  (when iota-modeline-mode
    (iota-modeline-mode -1))
  (mapc (lambda (feature)
          (when (featurep feature)
            (unload-feature feature t)))
        '(iota-demo iota-window iota-logos iota-splash iota-faces
          iota-widgets iota-modeline iota-animate iota-segments
          iota-segment iota-tui iota-theme iota-box iota))
  (require 'iota)
  (iota-modeline-mode 1)
  (message "I O T Λ reloaded"))

;;; Hooks for Integration

(defvar iota-after-load-hook nil
  "Hook run after IOTA is loaded.")

(run-hooks 'iota-after-load-hook)

(provide 'iota)
;;; iota.el ends here
