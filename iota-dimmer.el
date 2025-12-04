;;; iota-dimmer.el --- I O T Λ inactive window dimming -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: faces, windows, dimming
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; IOTA dimmer provides automatic dimming of inactive window contents.
;; Unlike other dimmer packages, this one:
;;   - Preserves syntax highlighting by adjusting each face's colors
;;   - Is compatible with modern packages (vertico, doom-modeline, which-key, transient)
;;   - Respects IOTA's theme transparency handling
;;   - Does not affect modeline boxes or separator lines (handled by iota-modeline)
;;   - Does not affect popup windows (handled by iota-popup)
;;
;; The dimming is achieved by reducing saturation and luminance in HSL color space
;; for each face, preserving the relative color relationships (syntax highlighting).

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'face-remap)

;;; Debug Logging

(defvar iota-dimmer-debug nil
  "When non-nil, log dimmer debug messages.")

(defun iota-dimmer--log (format-string &rest args)
  "Log a debug message if `iota-dimmer-debug' is non-nil.
FORMAT-STRING and ARGS are passed to `message'."
  (when iota-dimmer-debug
    (apply #'message (concat "[iota-dimmer] " format-string) args)))

;;; Customization

(defgroup iota-dimmer nil
  "I O T Λ inactive window dimming configuration."
  :group 'iota
  :prefix "iota-dimmer-")

(defcustom iota-dimmer-fraction 0.40
  "Amount to dim inactive windows.
Value between 0.0 (no dimming) and 1.0 (maximum dimming).
Higher values = more dimming (colors blend more toward background)."
  :type '(number :tag "Dimming fraction (0.0-1.0)")
  :group 'iota-dimmer)

(defcustom iota-dimmer-excluded-buffers
  '("^ \\*Minibuf"
    "^ \\*Echo Area"
    "^\\*which-key\\*"
    "^ \\*which-key\\*"
    "^\\*transient\\*"
    "^ \\*transient\\*"
    "^\\*Completions\\*"
    "^\\*I O T Λ splash\\*")
  "List of buffer name patterns to exclude from dimming.
Buffers matching these patterns will never be dimmed."
  :type '(repeat regexp)
  :group 'iota-dimmer)

(defcustom iota-dimmer-excluded-modes
  '(minibuffer-mode
    minibuffer-inactive-mode)
  "List of major modes to exclude from dimming."
  :type '(repeat symbol)
  :group 'iota-dimmer)

(defcustom iota-dimmer-watch-frame-focus t
  "Whether to dim all windows when frame loses focus."
  :type 'boolean
  :group 'iota-dimmer)

;;; Internal State

(defvar iota-dimmer--face-remaps (make-hash-table :test 'eq :weakness 'key)
  "Hash table mapping buffers to their face remap cookies.
Each value is an alist of (face . cookie) pairs.")

(defvar iota-dimmer--dimmed-color-cache (make-hash-table :test 'equal)
  "Cache of computed dimmed colors.
Keys are (color . fraction) pairs, values are dimmed colors.")

(defvar iota-dimmer--last-selected-window nil
  "The last selected window, used to detect window changes.")

(defvar-local iota-dimmer--buffer-dimmed nil
  "Non-nil if this buffer is currently dimmed.")

;;; Color Manipulation

(defun iota-dimmer--get-background-color ()
  "Get the default background color.
Handles terminal-mode 'unspecified-bg' by falling back to black."
  (let ((bg (face-background 'default nil t)))
    (cond
     ((null bg) "#000000")
     ((string-match-p "unspecified" bg) "#000000")
     (t bg))))

(defun iota-dimmer--dim-color (color)
  "Compute a dimmed version of COLOR.
Reduces saturation and shifts luminance toward background.
Returns a hex color string."
  (when (and color (stringp color)
             (not (string= color "unspecified"))
             (not (string-match-p "unspecified" color)))
    (let* ((cache-key (cons color iota-dimmer-fraction))
           (cached (gethash cache-key iota-dimmer--dimmed-color-cache)))
      (or cached
          (condition-case nil
              (let* ((bg (iota-dimmer--get-background-color))
                     (rgb (color-name-to-rgb color))
                     (bg-rgb (color-name-to-rgb bg)))
                (when (and rgb bg-rgb)
                  (let* ((hsl (apply #'color-rgb-to-hsl rgb))
                         (h (nth 0 hsl))
                         (s (nth 1 hsl))
                         (l (nth 2 hsl))
                         (bg-hsl (apply #'color-rgb-to-hsl bg-rgb))
                         (bg-l (nth 2 bg-hsl))
                         ;; Reduce saturation
                         (new-s (* s (- 1.0 (* iota-dimmer-fraction 0.6))))
                         ;; Blend luminance toward background
                         (new-l (+ l (* (- bg-l l) (* iota-dimmer-fraction 0.5))))
                         ;; Clamp
                         (new-s (max 0.0 (min 1.0 new-s)))
                         (new-l (max 0.0 (min 1.0 new-l)))
                         ;; Convert back
                         (new-rgb (color-hsl-to-rgb h new-s new-l))
                         (result (apply #'color-rgb-to-hex (append new-rgb '(2)))))
                    (puthash cache-key result iota-dimmer--dimmed-color-cache)
                    result)))
            (error nil))))))

;;; Face Handling

(defun iota-dimmer--get-face-foreground (face)
  "Get the foreground color of FACE, resolving inheritance."
  (condition-case nil
      (let ((fg (face-foreground face nil t)))
        (when (and fg
                   (stringp fg)
                   (not (string= fg "unspecified"))
                   (not (string-match-p "unspecified" fg)))
          fg))
    (error nil)))

(defun iota-dimmer--face-should-dim-p (face)
  "Return non-nil if FACE should be dimmed."
  (let ((name (symbol-name face)))
    (not (or (string-prefix-p "iota-" name)
             (string-prefix-p "mode-line" name)
             (string-prefix-p "header-line" name)
             (string-prefix-p "minibuffer" name)
             (string-prefix-p "fringe" name)
             (string-prefix-p "vertical-border" name)
             (string-prefix-p "window-divider" name)
             (string-prefix-p "cursor" name)
             (string-match-p "tooltip" name)
             (string-match-p "^menu" name)
             (string-match-p "^tool-bar" name)))))

(defun iota-dimmer--create-dimmed-face-spec (face)
  "Create a face specification for a dimmed version of FACE.
Returns nil if face cannot be dimmed."
  (let ((fg (iota-dimmer--get-face-foreground face)))
    (when fg
      (let ((dimmed-fg (iota-dimmer--dim-color fg)))
        (when dimmed-fg
          `(:foreground ,dimmed-fg))))))

;;; Buffer Dimming

(defun iota-dimmer--buffer-excluded-p (buffer)
  "Return non-nil if BUFFER should be excluded from dimming."
  (when (buffer-live-p buffer)
    (let ((name (buffer-name buffer)))
      (or
       (cl-some (lambda (pattern)
                  (string-match-p pattern name))
                iota-dimmer-excluded-buffers)
       (with-current-buffer buffer
         (memq major-mode iota-dimmer-excluded-modes))
       (and (fboundp 'iota-popup--buffer-popup-p)
            (iota-popup--buffer-popup-p buffer))))))

(defun iota-dimmer--dim-buffer (buffer)
  "Apply dimming to BUFFER by remapping face foreground colors."
  (when (and (buffer-live-p buffer)
             (not (iota-dimmer--buffer-excluded-p buffer)))
    (with-current-buffer buffer
      (unless iota-dimmer--buffer-dimmed
        (iota-dimmer--log "Dimming buffer: %s" (buffer-name buffer))
        (let ((remaps nil)
              (count 0))
          ;; Remap each face that has a foreground color
          (dolist (face (face-list))
            (when (iota-dimmer--face-should-dim-p face)
              (let ((spec (iota-dimmer--create-dimmed-face-spec face)))
                (when spec
                  (condition-case nil
                      (let ((cookie (face-remap-add-relative face spec)))
                        (push (cons face cookie) remaps)
                        (cl-incf count))
                    (error nil))))))
          (iota-dimmer--log "  Remapped %d faces" count)
          (puthash buffer remaps iota-dimmer--face-remaps)
          (setq iota-dimmer--buffer-dimmed t))))))

(defun iota-dimmer--undim-buffer (buffer)
  "Remove dimming from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when iota-dimmer--buffer-dimmed
        (iota-dimmer--log "Undimming buffer: %s" (buffer-name buffer))
        (let ((remaps (gethash buffer iota-dimmer--face-remaps)))
          (dolist (remap remaps)
            (condition-case nil
                (face-remap-remove-relative (cdr remap))
              (error nil))))
        (remhash buffer iota-dimmer--face-remaps)
        (setq iota-dimmer--buffer-dimmed nil)))))

;;; Window Management

(defun iota-dimmer--window-should-dim-p (window)
  "Return non-nil if WINDOW should be dimmed."
  (and (window-live-p window)
       (not (eq window (selected-window)))
       (not (window-minibuffer-p window))
       (not (and (fboundp 'iota-popup--window-popup-p)
                 (iota-popup--window-popup-p window)))
       (not (iota-dimmer--buffer-excluded-p (window-buffer window)))))

(defun iota-dimmer--buffer-visible-in-selected-window-p (buffer)
  "Return non-nil if BUFFER is displayed in the selected window."
  (eq buffer (window-buffer (selected-window))))

(defun iota-dimmer--update ()
  "Update dimming state for all windows."
  (iota-dimmer--log "update called")
  (let ((selected (selected-window))
        (frame-focused (frame-focus-state)))
    (iota-dimmer--log "  selected: %s, frame-focused: %s"
                      selected frame-focused)
    ;; First pass: collect which buffers should be dimmed/undimmed
    (let ((buffers-to-dim nil)
          (buffers-to-undim nil))
      (dolist (window (window-list nil 'no-minibuf))
        (let ((buffer (window-buffer window)))
          (cond
           ;; Selected window's buffer - always undim
           ((eq window selected)
            (push buffer buffers-to-undim))
           ;; Frame not focused - dim all (if watch-frame-focus)
           ((and iota-dimmer-watch-frame-focus (not frame-focused))
            (unless (iota-dimmer--buffer-excluded-p buffer)
              (push buffer buffers-to-dim)))
           ;; Inactive window - dim if should
           ((iota-dimmer--window-should-dim-p window)
            (push buffer buffers-to-dim))
           ;; Otherwise undim
           (t
            (push buffer buffers-to-undim)))))
      ;; Apply changes
      (dolist (buffer (delete-dups buffers-to-undim))
        ;; Only undim if buffer is not also shown in a dimmed window
        (unless (and (memq buffer buffers-to-dim)
                     (not (iota-dimmer--buffer-visible-in-selected-window-p buffer)))
          (iota-dimmer--undim-buffer buffer)))
      (dolist (buffer (delete-dups buffers-to-dim))
        ;; Only dim if buffer is not in selected window
        (unless (iota-dimmer--buffer-visible-in-selected-window-p buffer)
          (iota-dimmer--dim-buffer buffer))))))

(defun iota-dimmer--on-window-change (&optional _frame)
  "Handle window selection changes."
  (iota-dimmer--log "on-window-change called, mode=%s" iota-dimmer-mode)
  (when iota-dimmer-mode
    (let ((current (selected-window)))
      (unless (eq current iota-dimmer--last-selected-window)
        (iota-dimmer--log "  window changed: %s -> %s"
                          iota-dimmer--last-selected-window current)
        ;; Undim newly selected window's buffer
        (when (window-live-p current)
          (iota-dimmer--undim-buffer (window-buffer current)))
        ;; Dim previously selected window's buffer (if not visible in current)
        (when (and iota-dimmer--last-selected-window
                   (window-live-p iota-dimmer--last-selected-window))
          (let ((prev-buf (window-buffer iota-dimmer--last-selected-window)))
            (unless (iota-dimmer--buffer-visible-in-selected-window-p prev-buf)
              (when (iota-dimmer--window-should-dim-p iota-dimmer--last-selected-window)
                (iota-dimmer--dim-buffer prev-buf)))))
        (setq iota-dimmer--last-selected-window current)))))

(defun iota-dimmer--on-focus-change ()
  "Handle frame focus changes."
  (when (and iota-dimmer-mode iota-dimmer-watch-frame-focus)
    (iota-dimmer--update)))

(defun iota-dimmer--on-buffer-change (&optional _frame)
  "Handle buffer changes in windows."
  (when iota-dimmer-mode
    (iota-dimmer--update)))

;;; Cache Management

(defun iota-dimmer--clear-cache ()
  "Clear the dimmed color cache."
  (clrhash iota-dimmer--dimmed-color-cache))

(defun iota-dimmer--on-theme-change (&rest _)
  "Handle theme changes."
  (when iota-dimmer-mode
    (iota-dimmer--clear-cache)
    (iota-dimmer--undim-all)
    (run-with-timer 0.1 nil #'iota-dimmer--update)))

(defun iota-dimmer--undim-all ()
  "Remove dimming from all buffers."
  (maphash (lambda (buffer _)
             (when (buffer-live-p buffer)
               (iota-dimmer--undim-buffer buffer)))
           iota-dimmer--face-remaps)
  (clrhash iota-dimmer--face-remaps))

;;; Setup and Teardown

(defun iota-dimmer--setup ()
  "Set up dimmer mode."
  (iota-dimmer--log "setup called")
  (setq iota-dimmer--last-selected-window (selected-window))
  
  ;; Clear caches
  (iota-dimmer--clear-cache)
  
  ;; Add hooks
  (add-hook 'window-selection-change-functions #'iota-dimmer--on-window-change)
  (add-hook 'window-buffer-change-functions #'iota-dimmer--on-buffer-change)
  (add-hook 'window-configuration-change-hook #'iota-dimmer--update)
  (add-function :after after-focus-change-function #'iota-dimmer--on-focus-change)
  
  ;; Handle theme changes
  (advice-add 'load-theme :after #'iota-dimmer--on-theme-change)
  (advice-add 'enable-theme :after #'iota-dimmer--on-theme-change)
  (advice-add 'disable-theme :after #'iota-dimmer--on-theme-change)
  
  ;; Initial update
  (iota-dimmer--log "calling initial update")
  (iota-dimmer--update)
  (iota-dimmer--log "setup complete"))

(defun iota-dimmer--teardown ()
  "Tear down dimmer mode."
  (iota-dimmer--log "teardown called")
  ;; Remove all dimming
  (iota-dimmer--undim-all)
  
  ;; Remove hooks
  (remove-hook 'window-selection-change-functions #'iota-dimmer--on-window-change)
  (remove-hook 'window-buffer-change-functions #'iota-dimmer--on-buffer-change)
  (remove-hook 'window-configuration-change-hook #'iota-dimmer--update)
  (remove-function after-focus-change-function #'iota-dimmer--on-focus-change)
  
  ;; Remove advice
  (advice-remove 'load-theme #'iota-dimmer--on-theme-change)
  (advice-remove 'enable-theme #'iota-dimmer--on-theme-change)
  (advice-remove 'disable-theme #'iota-dimmer--on-theme-change)
  
  ;; Clear state
  (iota-dimmer--clear-cache)
  (setq iota-dimmer--last-selected-window nil))

;;; Mode Definition

;;;###autoload
(define-minor-mode iota-dimmer-mode
  "Toggle I O T Λ inactive window dimming.
When enabled, inactive windows have their content dimmed by reducing
saturation and luminance of each face's foreground color, preserving
syntax highlighting."
  :global t
  :group 'iota-dimmer
  :lighter " ιDim"
  (if iota-dimmer-mode
      (iota-dimmer--setup)
    (iota-dimmer--teardown)))

;;; Interactive Commands

(defun iota-dimmer-refresh ()
  "Manually refresh dimming state."
  (interactive)
  (iota-dimmer--clear-cache)
  (iota-dimmer--undim-all)
  (iota-dimmer--update))

(defun iota-dimmer-set-fraction (fraction)
  "Set dimming FRACTION interactively."
  (interactive "nDimming fraction (0.0-1.0): ")
  (setq iota-dimmer-fraction (max 0.0 (min 1.0 fraction)))
  (iota-dimmer-refresh)
  (message "Dimming fraction set to %.2f" iota-dimmer-fraction))

(defun iota-dimmer-toggle-debug ()
  "Toggle debug logging for iota-dimmer."
  (interactive)
  (setq iota-dimmer-debug (not iota-dimmer-debug))
  (message "iota-dimmer debug logging: %s" (if iota-dimmer-debug "ON" "OFF")))

(defun iota-dimmer-diagnose ()
  "Print diagnostic information about dimmer state."
  (interactive)
  (let ((buf (get-buffer-create "*iota-dimmer-diagnose*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== IOTA Dimmer Diagnostics ===\n\n")
      (insert (format "Mode enabled: %s\n" iota-dimmer-mode))
      (insert (format "Dim fraction: %.2f\n" iota-dimmer-fraction))
      (insert (format "Background color: %s\n" (iota-dimmer--get-background-color)))
      (insert (format "Last selected window: %s\n" iota-dimmer--last-selected-window))
      (insert (format "Current selected window: %s\n" (selected-window)))
      (insert (format "Frame focused: %s\n" (frame-focus-state)))
      (insert (format "Color cache size: %d\n" (hash-table-count iota-dimmer--dimmed-color-cache)))
      (insert (format "Dimmed buffers: %d\n" (hash-table-count iota-dimmer--face-remaps)))
      (insert "\n=== Windows ===\n")
      (dolist (window (window-list nil 'no-minibuf))
        (let ((buffer (window-buffer window)))
          (insert (format "\nWindow: %s\n" window))
          (insert (format "  Buffer: %s\n" (buffer-name buffer)))
          (insert (format "  Should dim: %s\n" (iota-dimmer--window-should-dim-p window)))
          (insert (format "  Buffer dimmed: %s\n"
                          (buffer-local-value 'iota-dimmer--buffer-dimmed buffer)))
          (insert (format "  Face remaps: %d\n"
                          (length (gethash buffer iota-dimmer--face-remaps))))))
      (insert "\n=== Sample dimmed colors ===\n")
      (let ((test-colors '("#ff0000" "#00ff00" "#0000ff" "#ffff00" "#ff00ff" "#00ffff")))
        (dolist (color test-colors)
          (insert (format "  %s -> %s\n" color (iota-dimmer--dim-color color))))))
    (display-buffer buf)))

(provide 'iota-dimmer)
;;; iota-dimmer.el ends here
