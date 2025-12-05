;;; iota-config.el --- Unified configuration interface -*- no-byte-compile: t; lexical-binding: t; -*-

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
;; Uses transient for popup interface when available.

;;; Code:

(require 'cl-lib)
(require 'iota-box)
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
      (iota-theme-transparent-mode 1))))

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

;;; Transient Interface (provided by iota-dispatch.el)
;; The transient interface is defined in iota-dispatch.el as iota-config-transient
;; to avoid duplication. This file provides the fallback buffer interface.

;;; Fallback Buffer Interface

(defun iota-config--info-buffer ()
  "Display current IOTA configuration in a buffer (fallback)."
  (let ((buf (get-buffer-create "*IOTA Configuration*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)

        ;; Title
        (insert (propertize "I O T Λ Configuration" 'face '(:weight bold :height 1.2)))

        ;; Preset and Style section
        (insert "\n\n")
        (insert (propertize "Preset & Style" 'face '(:weight bold)))
        (insert "\n\n")
        (insert (format "  Preset:     %s\n"
                       (propertize (format "%s" iota-config-preset) 'face 'iota-accent-face)))
        (insert (format "  Box Style:  %s\n"
                       (propertize (format "%s" iota-box-default-style) 'face 'iota-accent-face)))

        ;; Show actual box characters being used
        (let* ((chars (iota-box-get-chars iota-box-default-style))
               (tl (plist-get chars :top-left))
               (tr (plist-get chars :top-right))
               (bl (plist-get chars :bottom-left))
               (br (plist-get chars :bottom-right))
               (h (plist-get chars :horizontal))
               (v (plist-get chars :vertical)))
          (insert "\n  Box Characters:\n")
          (insert (format "    Corners:  %s %s %s %s  (TL TR BL BR)\n" tl tr bl br))
          (insert (format "    Lines:    %s %s        (H V)\n" h v)))

        ;; Page break before Modeline section
        (insert "\n\f\n")

        ;; Modeline state section
        (when (boundp 'iota-modeline-mode)
          (insert (propertize "Modeline" 'face '(:weight bold)))
          (insert "\n\n")
          (insert (format "  Mode:       %s\n"
                         (propertize (if iota-modeline-mode "enabled" "disabled")
                                    'face (if iota-modeline-mode 'success 'iota-muted-face))))
          (when iota-modeline-mode
            (insert (format "  Box Style:  %s\n"
                           (propertize (format "%s" (if (boundp 'iota-modeline-box-style)
                                                       iota-modeline-box-style
                                                     "not set"))
                                      'face 'iota-accent-face)))))

        ;; Page break before Transparency section
        (insert "\n\f\n")

        ;; Transparency state section
        (when (boundp 'iota-theme-transparent-mode)
          (insert (propertize "Transparency" 'face '(:weight bold)))
          (insert "\n\n")
          (insert (format "  Mode:       %s\n"
                         (propertize (if iota-theme-transparent-mode "enabled" "disabled")
                                    'face (if iota-theme-transparent-mode 'success 'iota-muted-face))))
          (when (boundp 'iota-theme-transparent--active)
            (insert (format "  Active:     %s\n"
                           (propertize (if iota-theme-transparent--active "yes" "no")
                                      'face (if iota-theme-transparent--active 'success 'iota-muted-face))))))

        ;; Page break before Themes section
        (insert "\n\f\n")

        ;; Current theme section
        (when custom-enabled-themes
          (insert (propertize "Themes" 'face '(:weight bold)))
          (insert "\n\n")
          (insert (format "  Enabled:    %s\n"
                         (propertize (mapconcat #'symbol-name custom-enabled-themes ", ")
                                    'face 'iota-accent-face))))

        ;; Page break before Terminal section
        (insert "\n\f\n")

        ;; Terminal capabilities section
        (insert (propertize "Terminal" 'face '(:weight bold)))
        (insert "\n\n")
        (insert (format "  Type:       %s\n" (or (getenv "TERM") "unknown")))
        (insert (format "  Graphic:    %s\n" (if (display-graphic-p) "yes" "no")))
        (insert (format "  Colors:     %s\n" (or (tty-display-color-cells) "N/A")))
        (when (not (display-graphic-p))
          (insert (format "  True Color: %s\n"
                         (if (or (getenv "COLORTERM")
                                (string-match-p "truecolor\\|24bit"
                                              (or (getenv "TERM") "")))
                             "yes" "no"))))
        
        ;; Add hint for keybindings
        (insert "\n\n")
        (insert (propertize "Press 'q' to close, '1-4' to change preset\n" 'face 'shadow))))

    ;; Open in current window and position cursor at beginning
    (switch-to-buffer buf)
    (goto-char (point-min))
    ;; Make buffer read-only and add keybindings
    (setq buffer-read-only t)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "q") 'quit-window)
      (define-key map (kbd "1") (lambda () (interactive) (iota-config-apply-preset 'minimal) (setq iota-config-preset 'minimal) (iota-config--info-buffer)))
      (define-key map (kbd "2") (lambda () (interactive) (iota-config-apply-preset 'standard) (setq iota-config-preset 'standard) (iota-config--info-buffer)))
      (define-key map (kbd "3") (lambda () (interactive) (iota-config-apply-preset 'modern) (setq iota-config-preset 'modern) (iota-config--info-buffer)))
      (define-key map (kbd "4") (lambda () (interactive) (iota-config-apply-preset 'cyberpunk) (setq iota-config-preset 'cyberpunk) (iota-config--info-buffer)))
      (use-local-map map))
    ;; Enable page-break-lines-mode if available
    (when (fboundp 'page-break-lines-mode)
      (page-break-lines-mode 1))))

;;; Main Entry Point

(defun iota-config-info ()
  "Display current IOTA configuration.
Uses transient popup if available, otherwise falls back to buffer."
  (interactive)
  (if (and (featurep 'transient)
           (fboundp 'iota-config-transient))
      (iota-config-transient)
    (iota-config--info-buffer)))

;;; Wizard

(defun iota-config-wizard ()
  "Interactive configuration wizard."
  (interactive)
  (iota-config-info))

(provide 'iota-config)
;;; iota-config.el ends here

