;;; iota-config.el --- Unified configuration interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: config
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Unified configuration interface for IOTA with presets.

;;; Code:

(require 'cl-lib)
(require 'iota-box)
;; Note: element-theme has incomplete implementation
;; (require 'iota-element-theme)
(require 'iota-theme-transparent)

;;; Configuration

(defgroup iota-config nil
  "Configuration interface for IOTA."
  :group 'iota
  :prefix "iota-config-")

(defcustom iota-config-preset 'modern
  "Configuration preset.
Options: minimal, standard, modern, cyberpunk, custom"
  :type '(choice (const :tag "Minimal" minimal)
                 (const :tag "Standard" standard)
                 (const :tag "Modern" modern)
                 (const :tag "Cyberpunk" cyberpunk)
                 (const :tag "Custom" custom))
  :group 'iota-config
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'iota-config-apply-preset)
           (iota-config-apply-preset value))))

;;; Preset Application

(defun iota-config-apply-preset (preset)
  "Apply configuration PRESET."
  (pcase preset
    ('minimal
     (setq iota-box-default-style 'ascii))

    ('standard
     (setq iota-box-default-style (iota-box-select-best-style 'single)))

    ('modern
     (setq iota-box-default-style (iota-box-select-best-style 'rounded)))

    ('cyberpunk
     (setq iota-box-default-style (iota-box-select-best-style 'heavy)))

    (_ ; custom - do nothing
     nil))

  ;; Apply changes if modeline is active
  (when (and (boundp 'iota-modeline-mode) iota-modeline-mode)
    (setq iota-modeline-box-style iota-box-default-style)
    (force-mode-line-update t))

  ;; Enable transparency mode in terminal
  (when (and (fboundp 'iota-theme-transparent-mode)
             (not (display-graphic-p)))
    (unless iota-theme-transparent-mode
      (iota-theme-transparent-mode 1)))

  (message "IOTA: Applied '%s' preset (box style: %s)" preset iota-box-default-style))

;;; Interactive Configuration

(defun iota-config-choose-preset ()
  "Interactively choose and apply a configuration preset."
  (interactive)
  (let ((presets '(("Minimal - ASCII only, maximum compatibility" . minimal)
                  ("Standard - Single-line box, square corners" . standard)
                  ("Modern - Rounded corners, smooth style" . modern)
                  ("Cyberpunk - Heavy lines, bold appearance" . cyberpunk)
                  ("Custom - Manual configuration" . custom))))
    (let* ((choice (completing-read "Choose IOTA preset: " 
                                   (mapcar #'car presets)))
           (preset (alist-get choice presets nil nil #'string=)))
      (when preset
        (customize-set-variable 'iota-config-preset preset)
        (iota-config-apply-preset preset)
        (force-mode-line-update t)))))

(defun iota-config-info ()
  "Display current IOTA configuration."
  (interactive)
  (let ((buf (get-buffer-create "*IOTA Configuration*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== IOTA Configuration ===\n\n")

      ;; Preset and Style
      (insert (format "Preset: %s\n" iota-config-preset))
      (insert (format "Box Style: %s\n" iota-box-default-style))

      ;; Show actual box characters being used
      (let* ((chars (iota-box-get-chars iota-box-default-style))
             (tl (plist-get chars :top-left))
             (tr (plist-get chars :top-right))
             (bl (plist-get chars :bottom-left))
             (br (plist-get chars :bottom-right))
             (h (plist-get chars :horizontal))
             (v (plist-get chars :vertical)))
        (insert (format "\nCurrent Box Characters:\n  %s%s%s%s  %s  Corners\n  %s   %s  Sides\n  %s%s%s%s\n"
                       tl h h tr tl v v bl h h br)))

      ;; Modeline state
      (when (boundp 'iota-modeline-mode)
        (insert (format "\nModeline Mode: %s\n"
                       (if iota-modeline-mode "enabled" "disabled")))
        (when iota-modeline-mode
          (insert (format "Modeline Box Style: %s\n"
                         (if (boundp 'iota-modeline-box-style)
                             iota-modeline-box-style
                             "not set")))))

      ;; Transparency state
      (when (boundp 'iota-theme-transparent-mode)
        (insert (format "\nTransparency Mode: %s\n"
                       (if iota-theme-transparent-mode "enabled" "disabled")))
        (when (boundp 'iota-theme-transparent--active)
          (insert (format "Transparency Active: %s\n"
                         (if iota-theme-transparent--active "yes" "no")))))

      ;; Current theme
      (when custom-enabled-themes
        (insert (format "\nEnabled Themes: %s\n"
                       (mapconcat #'symbol-name custom-enabled-themes ", "))))

      ;; Terminal capabilities
      (insert (format "\nTerminal Type: %s\n" (or (getenv "TERM") "unknown")))
      (insert (format "Display Graphic: %s\n" (if (display-graphic-p) "yes" "no")))
      (insert (format "Color Cells: %s\n" (or (tty-display-color-cells) "N/A")))
      (when (not (display-graphic-p))
        (insert (format "True Color Support: %s\n"
                       (if (or (getenv "COLORTERM")
                              (string-match-p "truecolor\\|24bit"
                                            (or (getenv "TERM") "")))
                           "yes" "no")))))
    (display-buffer buf)))

;;; Wizard

(defun iota-config-wizard ()
  "Interactive configuration wizard."
  (interactive)
  (message "IOTA configuration wizard: Use M-x iota-config-choose-preset"))

(provide 'iota-config)
;;; iota-config.el ends here
