;;; iota-theme-transparent.el --- Transparent theme handling -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: faces, theme
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Automatic theme transparency for terminal Emacs.
;; Removes background colors from theme-defined faces by modifying theme specs.
;; Does NOT set user specs - only modifies what themes explicitly define.
;; This preserves theme attributes and inheritance correctly.
;;
;; DEBUGGING:
;;
;; To see what happens when loading a theme, enable verbose logging:
;;
;;   M-x iota-theme-transparent-toggle-logging
;;
;; Or use the test function to load a theme with logging:
;;
;;   M-x iota-theme-transparent-test-theme-load RET wombat
;;
;; This will show detailed logs in *Messages* including:
;; - When the advice is triggered
;; - Which faces are being processed
;; - Which faces are modified vs. skipped
;; - Why faces are skipped (excluded or no background)
;;
;; You can also set the variable directly:
;;
;;   (setq iota-theme-transparent-verbose-logging t)

;;; Code:

(require 'cl-lib)
(require 'iota-faces)

;;; Configuration

(defgroup iota-theme-transparent nil
  "IOTA theme transparency configuration."
  :group 'iota
  :prefix "iota-theme-transparent-")

(defcustom iota-theme-transparent-in-terminal t
  "Automatically remove background colors in terminal Emacs."
  :type 'boolean
  :group 'iota-theme-transparent)

(defcustom iota-theme-transparent-verbose-logging nil
  "Enable verbose logging for transparency operations.
When non-nil, log detailed messages about theme loading and face processing."
  :type 'boolean
  :group 'iota-theme-transparent)

(defcustom iota-theme-transparent-excluded-faces
  '(tab-bar tab-line fringe
    highlight region lazy-highlight
    vertico-current completions-highlight
    corfu-current corfu-default
    iota-accent-face iota-success-face
    iota-error-face iota-warning-face)
  "Faces to preserve background colors (UI elements).
These faces need backgrounds for visibility and selection indication.
Note: mode-line faces are NOT excluded because IOTA renders its own modeline
      using box-drawing characters, so the built-in mode-line background must
      be transparent to avoid showing through the minibuffer separator.
Note: iota-box-face and iota-inactive-box-face are NOT excluded - they should be transparent.
All other faces including default, borders, dividers will have backgrounds removed."
  :type '(repeat symbol)
  :group 'iota-theme-transparent)

(defcustom iota-theme-transparent-critical-faces
  '(iota-box-face iota-inactive-box-face iota-active-box-face
    vertical-border window-divider window-divider-first-pixel window-divider-last-pixel
    internal-border child-frame-border)
  "Faces that MUST have transparent backgrounds for IOTA to work correctly.
These faces will have backgrounds removed even if the theme doesn't define them."
  :type '(repeat symbol)
  :group 'iota-theme-transparent)

(defcustom iota-theme-transparent-preserve-regions
  '((minibuffer . nil)  ; NO backgrounds allowed
    (modeline . nil)    ; NO backgrounds allowed
    (header . nil)      ; NO backgrounds allowed
    (fringe . t)        ; Fringe can have background
    (hl-line . nil)
    (region . nil))
  "Control which regions preserve backgrounds in transparency mode.
Note: IOTA faces should NOT have backgrounds - only foreground colors."
  :type '(alist :key-type symbol :value-type boolean)
  :group 'iota-theme-transparent)

;;; State

(defvar iota-theme-transparent--original-specs nil
  "Alist storing original face specs before transparency.")

(defvar iota-theme-transparent--active nil
  "Whether transparency mode is currently active.")

;;; Logging

(defmacro iota-theme-transparent--log (format-string &rest args)
  "Log message with FORMAT-STRING and ARGS if verbose logging is enabled."
  `(when iota-theme-transparent-verbose-logging
     (message (concat "[IOTA-TRANSPARENCY] " ,format-string) ,@args)))

;;; Terminal Detection

(defun iota-theme-transparent-should-apply-p ()
  "Determine if transparent theming should be applied."
  (let ((should-apply (and iota-theme-transparent-in-terminal
                          (not (display-graphic-p)))))
    (iota-theme-transparent--log "Should apply transparency? %s (in-terminal: %s, graphic: %s)"
                                 should-apply
                                 iota-theme-transparent-in-terminal
                                 (display-graphic-p))
    should-apply))

(defun iota-theme-transparent-terminal-supports-truecolor-p ()
  "Check if terminal supports 24-bit true color."
  (or (getenv "COLORTERM")
      (string-match-p "truecolor\\|24bit" 
                     (or (getenv "TERM") ""))))

(defun iota-theme-transparent-terminal-info ()
  "Return terminal capability information."
  (when (not (display-graphic-p))
    (list :type (getenv "TERM")
          :colorterm (getenv "COLORTERM")
          :truecolor (iota-theme-transparent-terminal-supports-truecolor-p)
          :colors (tty-display-color-cells))))

;;; Face Classification

(defun iota-theme-transparent-classify-face (face)
  "Classify FACE into UI element category."
  (let ((name (symbol-name face)))
    (cond
     ((string-match-p "mode-?line" name) 'modeline)
     ((string-match-p "header" name) 'header)
     ((string-match-p "minibuffer" name) 'minibuffer)
     ((string-match-p "fringe" name) 'fringe)
     ((string-match-p "hl-line\\|highlight" name) 'hl-line)
     ((string-match-p "region" name) 'region)
     (t 'syntax))))

(defun iota-theme-transparent-should-preserve-face-p (face)
  "Determine if FACE should preserve background."
  (let ((category (iota-theme-transparent-classify-face face)))
    (or (memq face iota-theme-transparent-excluded-faces)
        (alist-get category iota-theme-transparent-preserve-regions))))

;;; Transparency Application

(defun iota-theme-transparent-remove-backgrounds ()
  "Remove background colors from all faces except excluded ones.
Only modifies theme specs - does not set user specs."
  (interactive)
  (iota-theme-transparent--log "=== Starting background removal ===")

  (if (not (iota-theme-transparent-should-apply-p))
      (iota-theme-transparent--log "Skipping: conditions not met")
    (let ((faces-modified 0)
          (faces-checked 0)
          (faces-skipped-excluded 0)
          (faces-skipped-no-bg 0))
      (iota-theme-transparent--log "Total faces to check: %d" (length (face-list)))
      (dolist (face (face-list))
        (setq faces-checked (1+ faces-checked))
        (if (iota-theme-transparent-should-preserve-face-p face)
            (setq faces-skipped-excluded (1+ faces-skipped-excluded))
          ;; Check if face has a background - check theme specs first, then current attributes
          (let* ((theme-specs (get face 'theme-face))
                 ;; Get actual background value from theme specs (not just t/nil)
                 (theme-bg (and theme-specs
                               (cl-some (lambda (spec)
                                         (let ((face-spec (cadr spec)))
                                           (cl-some (lambda (display-spec)
                                                     (plist-get (cdr display-spec) :background))
                                                   face-spec)))
                                       theme-specs)))
                 (current-bg (face-attribute face :background nil 'default))
                 ;; Use theme bg if present and valid, otherwise current bg
                 (bg (if (and theme-bg
                             (not (eq theme-bg 'unspecified))
                             (not (equal theme-bg "unspecified-bg")))
                         theme-bg
                       current-bg)))
            (if (or (not bg)
                    (eq bg 'unspecified)
                    (equal bg "unspecified-bg")
                    (eq bg 'unspecified-bg))
                (setq faces-skipped-no-bg (1+ faces-skipped-no-bg))
              ;; Face has a background, remove it
              ;; Special logging for problem faces
              (when (memq face '(vertical-border window-divider window-divider-first-pixel
                                window-divider-last-pixel fringe internal-border
                                iota-box-face iota-inactive-box-face))
                (iota-theme-transparent--log "  *** TRACKED FACE: %s (bg: %s)" face bg))

              ;; Store original spec if not already stored
              (unless (alist-get face iota-theme-transparent--original-specs)
                (setf (alist-get face iota-theme-transparent--original-specs)
                      (list :background bg)))

              ;; Remove background - ONLY modify theme specs (don't set user specs)
              (condition-case err
                  (let ((theme-face-specs (get face 'theme-face)))

                    (if theme-face-specs
                        ;; Face has theme specs - modify them to remove :background
                        (progn
                          (when (memq face '(vertical-border window-divider window-divider-first-pixel
                                            window-divider-last-pixel fringe internal-border default
                                            iota-box-face iota-inactive-box-face))
                            (iota-theme-transparent--log "    %s: modifying %d theme-face specs" face (length theme-face-specs)))
                          (let ((modified-specs '()))
                            (dolist (spec theme-face-specs)
                              (let* ((theme-name (car spec))
                                     (face-spec (cadr spec)))
                                (let ((new-face-spec
                                       (mapcar
                                        (lambda (display-spec)
                                          ;; Handle two input formats but ALWAYS output flat format:
                                          ;; Input 1: (DISPLAY :foreground "..." ...)  [modus, doric]
                                          ;; Input 2: (DISPLAY (:foreground "...") DOC)  [wombat, etc]
                                          ;; Output: (DISPLAY :foreground "..." ...)  [standard format]
                                          (let* ((display (car display-spec))
                                                 (rest (cdr display-spec))
                                                 (second (car rest))
                                                 ;; Detect format and extract plist
                                                 (attrs (if (keywordp second)
                                                            rest              ; Flat plist
                                                          (car rest)))        ; Wrapped plist
                                                 (new-attrs '()))
                                            ;; Process attributes, removing :background, :underline, :overline, :box
                                            (while attrs
                                              (let ((key (car attrs))
                                                    (val (cadr attrs)))
                                                (unless (memq key '(:background :underline :overline :box))
                                                  (push key new-attrs)
                                                  (push val new-attrs))
                                                (setq attrs (cddr attrs))))
                                            ;; Always output flat format: (display . new-attrs)
                                            (cons display (nreverse new-attrs))))
                                        face-spec)))
                                  (push (list theme-name new-face-spec) modified-specs))))
                            (put face 'theme-face (nreverse modified-specs))
                            (face-spec-recalc face (selected-frame))
                            (setq faces-modified (1+ faces-modified))))

                      ;; No theme specs - check if it's a critical face
                      (if (memq face iota-theme-transparent-critical-faces)
                          ;; Critical face - must remove background for IOTA to work
                          (progn
                            (when (memq face '(vertical-border window-divider window-divider-first-pixel
                                              window-divider-last-pixel fringe internal-border default
                                              iota-box-face iota-inactive-box-face))
                              (iota-theme-transparent--log "    %s: CRITICAL face, removing background directly" face))
                            ;; Use set-face-background to remove background without full user spec
                            (set-face-background face 'unspecified)
                            (setq faces-modified (1+ faces-modified)))
                        ;; Non-critical face - skip it (preserve defaults/inheritance)
                        (when (memq face '(vertical-border window-divider window-divider-first-pixel
                                          window-divider-last-pixel fringe internal-border default
                                          iota-box-face iota-inactive-box-face))
                          (iota-theme-transparent--log "    %s: no theme specs, skipping (preserving defaults/inheritance)" face)))))
                (error
                 (iota-theme-transparent--log "    ERROR: Failed to remove background from %s: %s" face err)
                 (message "I O T Λ: Error removing background from face %s: %s" face err)))))))
      (setq iota-theme-transparent--active t)
      (iota-theme-transparent--log "=== Background removal complete ===")
      (iota-theme-transparent--log "Summary: checked=%d modified=%d excluded=%d no-bg=%d"
                                   faces-checked faces-modified faces-skipped-excluded faces-skipped-no-bg))))

(defun iota-theme-transparent-restore-backgrounds ()
  "Restore original background colors."
  (interactive)
  (dolist (spec iota-theme-transparent--original-specs)
    (let ((face (car spec))
          (attrs (cdr spec)))
      (when-let ((bg (plist-get attrs :background)))
        (set-face-attribute face nil :background bg))))
  (setq iota-theme-transparent--original-specs nil)
  (setq iota-theme-transparent--active nil))

(defun iota-theme-transparent-advice (theme &optional no-confirm no-enable &rest args)
  "Advice for `load-theme' to apply transparency after theme loads.
THEME is the theme being loaded, NO-CONFIRM and NO-ENABLE are load-theme args."
  (iota-theme-transparent--log "")
  (iota-theme-transparent--log "====== THEME LOAD: %s ======" theme)
  (iota-theme-transparent--log "  transparency-mode: %s"
                               (if (boundp 'iota-theme-transparent-mode)
                                   iota-theme-transparent-mode
                                 "not defined"))

  (if (not (iota-theme-transparent-should-apply-p))
      (iota-theme-transparent--log "  Skipping: should-apply-p returned nil")
    (if no-enable
        (iota-theme-transparent--log "  Skipping: theme not being enabled (no-enable=%s)" no-enable)
      ;; Clear previous specs to allow re-application
      (iota-theme-transparent--log "  Cleared %d previously stored specs"
                                   (length iota-theme-transparent--original-specs))
      (setq iota-theme-transparent--original-specs nil)

      ;; Process faces synchronously (no delay)
      (iota-theme-transparent--log "  Processing theme transparency synchronously...")
      (iota-theme-transparent-remove-backgrounds))))

;;; Mode

(defun iota-theme-transparent-enable ()
  "Enable automatic theme transparency."
  (interactive)
  (iota-theme-transparent--log ">>> Enabling transparency mode <<<")
  (iota-theme-transparent--log "  Adding advice to load-theme")
  (advice-add 'load-theme :after #'iota-theme-transparent-advice)
  (iota-theme-transparent--log "  Applying transparency to current faces")
  (iota-theme-transparent-remove-backgrounds)
  (iota-theme-transparent--log ">>> Transparency mode enabled <<<"))

(defun iota-theme-transparent-disable ()
  "Disable automatic theme transparency."
  (interactive)
  (iota-theme-transparent--log ">>> Disabling transparency mode <<<")
  (iota-theme-transparent--log "  Removing advice from load-theme")
  (advice-remove 'load-theme #'iota-theme-transparent-advice)
  (iota-theme-transparent--log "  Restoring original backgrounds")
  (iota-theme-transparent-restore-backgrounds)
  (iota-theme-transparent--log ">>> Transparency mode disabled <<<"))

;;;###autoload
(define-minor-mode iota-theme-transparent-mode
  "Toggle automatic theme transparency in terminal."
  :global t
  :group 'iota-theme-transparent
  (if iota-theme-transparent-mode
      (iota-theme-transparent-enable)
    (iota-theme-transparent-disable)))

;;; Diagnostics

(defun iota-theme-transparent-diagnose ()
  "Display terminal transparency diagnostics."
  (interactive)
  (let* ((info (iota-theme-transparent-terminal-info))
         (should-apply (iota-theme-transparent-should-apply-p))
         (faces-with-bg 0))
    ;; Count faces that still have backgrounds
    (dolist (face (face-list))
      (unless (iota-theme-transparent-should-preserve-face-p face)
        (let ((bg (face-attribute face :background nil 'default)))
          (when (and bg
                     (not (eq bg 'unspecified))
                     (not (equal bg "unspecified-bg")))
            (setq faces-with-bg (1+ faces-with-bg))))))
    (message "Terminal: %s | Colors: %s | TrueColor: %s | Active: %s | Should Apply: %s | Faces with BG: %d"
             (plist-get info :type)
             (plist-get info :colors)
             (plist-get info :truecolor)
             iota-theme-transparent--active
             should-apply
             faces-with-bg)))

(defun iota-theme-transparent-list-faces-with-backgrounds ()
  "List all faces that currently have background colors."
  (interactive)
  (let ((faces-with-bg '()))
    (dolist (face (face-list))
      (let ((bg (face-attribute face :background nil 'default)))
        (when (and bg
                   (not (eq bg 'unspecified))
                   (not (equal bg "unspecified-bg")))
          (push (cons face bg) faces-with-bg))))
    (with-current-buffer (get-buffer-create "*Faces With Backgrounds*")
      (erase-buffer)
      (insert (format "=== Faces With Background Colors (%d total) ===\n\n"
                     (length faces-with-bg)))
      (dolist (item (sort faces-with-bg (lambda (a b) (string< (symbol-name (car a))
                                                                (symbol-name (car b))))))
        (let* ((face (car item))
               (bg (cdr item))
               (excluded (iota-theme-transparent-should-preserve-face-p face)))
          (insert (format "%-40s %s %s\n"
                         (symbol-name face)
                         bg
                         (if excluded "[EXCLUDED]" "")))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun iota-theme-transparent-toggle-logging ()
  "Toggle verbose logging for transparency operations."
  (interactive)
  (setq iota-theme-transparent-verbose-logging
        (not iota-theme-transparent-verbose-logging))
  (message "IOTA transparency logging: %s"
           (if iota-theme-transparent-verbose-logging "ENABLED" "DISABLED")))

(defun iota-theme-transparent-test-theme-load (theme)
  "Load THEME with verbose logging enabled to debug transparency.
This is a diagnostic function to help understand what happens during theme loading."
  (interactive
   (list (intern (completing-read "Load theme: "
                                  (mapcar #'symbol-name
                                         (custom-available-themes))))))
  (let ((original-logging iota-theme-transparent-verbose-logging))
    (setq iota-theme-transparent-verbose-logging t)
    (message "[IOTA-TRANSPARENCY] ====== Starting test theme load: %s ======" theme)
    (load-theme theme t)
    (run-with-timer 0.5 nil
                    (lambda ()
                      (message "[IOTA-TRANSPARENCY] ====== Theme load test complete ======")
                      (setq iota-theme-transparent-verbose-logging original-logging)))))

(defun iota-theme-transparent-check-default-face ()
  "Check if default face has proper colors after theme load."
  (interactive)
  (let* ((fg (face-attribute 'default :foreground nil 'default))
         (bg (face-attribute 'default :background nil 'default))
         (theme-spec (get 'default 'theme-face))
         (user-spec (get 'default 'face-defface-spec)))
    (with-current-buffer (get-buffer-create "*IOTA Theme Debug*")
      (erase-buffer)
      (insert "=== Default Face Debug Info ===\n\n")
      (insert (format "Foreground: %s\n" fg))
      (insert (format "Background: %s\n" bg))
      (insert (format "\nHas theme-spec: %s\n" (if theme-spec "YES" "NO")))
      (when theme-spec
        (insert (format "Theme specs: %S\n" theme-spec)))
      (insert (format "\nHas user-spec: %s\n" (if user-spec "YES" "NO")))
      (when user-spec
        (insert (format "User spec: %S\n" user-spec)))
      (insert (format "\nAll attributes:\n%S\n" (face-all-attributes 'default)))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'iota-theme-transparent)
;;; iota-theme-transparent.el ends here
