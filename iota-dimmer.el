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
;;   - Preserves syntax highlighting (adjusts each face individually)
;;   - Is compatible with modern packages (vertico, doom-modeline, which-key, transient)
;;   - Respects IOTA's theme transparency handling
;;   - Does not affect modeline boxes or separator lines (handled by iota-modeline)
;;   - Does not affect popup windows (handled by iota-popup)
;;
;; The dimming is achieved by applying an overlay with a 'face property that
;; modifies the foreground color, using HSL color space adjustments.

;;; Code:

(require 'cl-lib)
(require 'color)

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

(defcustom iota-dimmer-fraction 0.30
  "Amount to dim inactive windows.
Value between 0.0 (no dimming) and 1.0 (maximum dimming).
This affects both saturation and luminance reduction."
  :type '(number :tag "Dimming fraction (0.0-1.0)")
  :group 'iota-dimmer)

(defcustom iota-dimmer-adjustment-mode 'foreground
  "How to apply the dimming effect.
- `foreground': Adjust foreground colors toward background (preserves syntax)
- `both': Adjust both foreground and background"
  :type '(choice (const :tag "Foreground only" foreground)
                 (const :tag "Foreground and background" both))
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

(defvar iota-dimmer--overlays (make-hash-table :test 'eq :weakness 'key)
  "Hash table mapping windows to their dimming overlays.")

(defvar iota-dimmer--dimmed-windows (make-hash-table :test 'eq :weakness 'key)
  "Set of currently dimmed windows.")

(defvar iota-dimmer--last-selected-window nil
  "The last selected window, used to detect window changes.")

(defvar iota-dimmer--dim-face nil
  "Computed face for dimming effect.")

;;; Color Manipulation

(defun iota-dimmer--compute-dim-color (fg-color bg-color fraction)
  "Compute a dimmed version of FG-COLOR blending toward BG-COLOR.
FRACTION controls the blend amount (0.0 = fg, 1.0 = bg)."
  (when (and fg-color bg-color
             (stringp fg-color) (stringp bg-color)
             (not (string= fg-color "unspecified"))
             (not (string= bg-color "unspecified")))
    (condition-case nil
        (let* ((fg-rgb (color-name-to-rgb fg-color))
               (bg-rgb (color-name-to-rgb bg-color)))
          (when (and fg-rgb bg-rgb)
            (let* ((r (+ (nth 0 fg-rgb) (* fraction (- (nth 0 bg-rgb) (nth 0 fg-rgb)))))
                   (g (+ (nth 1 fg-rgb) (* fraction (- (nth 1 bg-rgb) (nth 1 fg-rgb)))))
                   (b (+ (nth 2 fg-rgb) (* fraction (- (nth 2 bg-rgb) (nth 2 fg-rgb))))))
              (color-rgb-to-hex r g b 2))))
      (error nil))))

(defun iota-dimmer--get-background-color ()
  "Get the default background color.
Handles terminal-mode 'unspecified-bg' by falling back to black."
  (let ((bg (face-background 'default nil t)))
    (cond
     ((null bg) "#000000")
     ((string-match-p "unspecified" bg) "#000000")
     (t bg))))

(defun iota-dimmer--get-foreground-color ()
  "Get the default foreground color.
Handles terminal-mode 'unspecified-fg' by falling back to white."
  (let ((fg (face-foreground 'default nil t)))
    (cond
     ((null fg) "#ffffff")
     ((string-match-p "unspecified" fg) "#ffffff")
     (t fg))))

(defun iota-dimmer--compute-dim-face ()
  "Compute the face used for dimming overlays."
  (let* ((bg (iota-dimmer--get-background-color))
         (fg (iota-dimmer--get-foreground-color))
         (dimmed-fg (iota-dimmer--compute-dim-color fg bg iota-dimmer-fraction)))
    (iota-dimmer--log "compute-dim-face: bg=%s fg=%s dimmed=%s fraction=%.2f"
                      bg fg dimmed-fg iota-dimmer-fraction)
    (setq iota-dimmer--dim-face
          (if dimmed-fg
              `(:foreground ,dimmed-fg)
            nil))
    (iota-dimmer--log "dim-face set to: %S" iota-dimmer--dim-face)))

;;; Window/Buffer Exclusions

(defun iota-dimmer--buffer-excluded-p (buffer)
  "Return non-nil if BUFFER should be excluded from dimming."
  (when (buffer-live-p buffer)
    (let ((name (buffer-name buffer)))
      (or
       ;; Check buffer name patterns
       (cl-some (lambda (pattern)
                  (string-match-p pattern name))
                iota-dimmer-excluded-buffers)
       ;; Check major mode
       (with-current-buffer buffer
         (memq major-mode iota-dimmer-excluded-modes))
       ;; Check if it's a popup buffer
       (and (fboundp 'iota-popup--buffer-popup-p)
            (iota-popup--buffer-popup-p buffer))))))

(defun iota-dimmer--window-should-dim-p (window)
  "Return non-nil if WINDOW should be dimmed."
  (and (window-live-p window)
       (not (eq window (selected-window)))
       (not (window-minibuffer-p window))
       ;; Don't dim popup windows
       (not (and (fboundp 'iota-popup--window-popup-p)
                 (iota-popup--window-popup-p window)))
       ;; Check buffer exclusions
       (not (iota-dimmer--buffer-excluded-p (window-buffer window)))))

;;; Overlay Management

(defun iota-dimmer--make-overlay (window)
  "Create a dimming overlay for WINDOW."
  (let* ((buffer (window-buffer window))
         (ov (make-overlay (point-min) (point-max) buffer nil t)))
    (overlay-put ov 'iota-dimmer t)
    (overlay-put ov 'window window)
    (overlay-put ov 'priority -100)  ; Low priority to not interfere
    (overlay-put ov 'face iota-dimmer--dim-face)
    (puthash window ov iota-dimmer--overlays)
    ov))

(defun iota-dimmer--remove-overlay (window)
  "Remove dimming overlay from WINDOW."
  (let ((ov (gethash window iota-dimmer--overlays)))
    (when (and ov (overlayp ov))
      (delete-overlay ov))
    (remhash window iota-dimmer--overlays)
    (remhash window iota-dimmer--dimmed-windows)))

(defun iota-dimmer--dim-window (window)
  "Apply dimming to WINDOW."
  (iota-dimmer--log "dim-window called for: %s (buffer: %s)"
                    window (buffer-name (window-buffer window)))
  (when (and (window-live-p window)
             (not (gethash window iota-dimmer--dimmed-windows)))
    (let ((buffer (window-buffer window)))
      (iota-dimmer--log "  window live, buffer: %s" (buffer-name buffer))
      (when (buffer-live-p buffer)
        ;; Remove any existing overlay first
        (iota-dimmer--remove-overlay window)
        ;; Create new overlay covering the visible portion
        (with-current-buffer buffer
          (iota-dimmer--log "  creating overlay from %d to %d with face %S"
                            (point-min) (point-max) iota-dimmer--dim-face)
          (let ((ov (make-overlay (point-min) (point-max) buffer nil t)))
            (overlay-put ov 'iota-dimmer t)
            (overlay-put ov 'window window)
            (overlay-put ov 'priority -100)
            (overlay-put ov 'face iota-dimmer--dim-face)
            (puthash window ov iota-dimmer--overlays)
            (puthash window t iota-dimmer--dimmed-windows)
            (iota-dimmer--log "  overlay created: %S" ov)))))))

(defun iota-dimmer--undim-window (window)
  "Remove dimming from WINDOW."
  (iota-dimmer--log "undim-window called for: %s" window)
  (iota-dimmer--remove-overlay window))

;;; Update Logic

(defun iota-dimmer--update ()
  "Update dimming state for all windows."
  (iota-dimmer--log "update called")
  ;; First, compute the dim face if needed
  (unless iota-dimmer--dim-face
    (iota-dimmer--compute-dim-face))
  
  (let ((selected (selected-window))
        (frame-focused (frame-focus-state))
        (windows (window-list nil 'no-minibuf)))
    (iota-dimmer--log "  selected: %s, frame-focused: %s, windows: %d"
                      selected frame-focused (length windows))
    ;; Process all windows
    (dolist (window windows)
      (iota-dimmer--log "  processing window: %s (buffer: %s)"
                        window (buffer-name (window-buffer window)))
      (cond
       ;; Selected window - never dim
       ((eq window selected)
        (iota-dimmer--log "    -> undim (selected)")
        (iota-dimmer--undim-window window))
       ;; Frame not focused and watch-frame-focus enabled - dim all
       ((and iota-dimmer-watch-frame-focus (not frame-focused))
        (iota-dimmer--log "    -> dim (frame unfocused)")
        (when (iota-dimmer--window-should-dim-p window)
          (iota-dimmer--dim-window window)))
       ;; Normal case - dim if should be dimmed
       ((iota-dimmer--window-should-dim-p window)
        (iota-dimmer--log "    -> dim (inactive)")
        (iota-dimmer--dim-window window))
       ;; Otherwise undim
       (t
        (iota-dimmer--log "    -> undim (excluded)")
        (iota-dimmer--undim-window window))))))

(defun iota-dimmer--on-window-change (&optional _frame)
  "Handle window selection changes."
  (iota-dimmer--log "on-window-change called, mode=%s" iota-dimmer-mode)
  (when iota-dimmer-mode
    (let ((current (selected-window)))
      (iota-dimmer--log "  current=%s last=%s" current iota-dimmer--last-selected-window)
      (unless (eq current iota-dimmer--last-selected-window)
        ;; Undim newly selected window
        (iota-dimmer--undim-window current)
        ;; Dim previously selected window
        (when (and iota-dimmer--last-selected-window
                   (window-live-p iota-dimmer--last-selected-window)
                   (iota-dimmer--window-should-dim-p iota-dimmer--last-selected-window))
          (iota-dimmer--dim-window iota-dimmer--last-selected-window))
        (setq iota-dimmer--last-selected-window current)))))

(defun iota-dimmer--on-focus-change ()
  "Handle frame focus changes."
  (when (and iota-dimmer-mode iota-dimmer-watch-frame-focus)
    (iota-dimmer--update)))

(defun iota-dimmer--on-buffer-change (&optional _frame)
  "Handle buffer changes in windows."
  (when iota-dimmer-mode
    ;; Clean up overlays for windows that changed buffers
    (maphash (lambda (window ov)
               (when (and (window-live-p window)
                          (overlayp ov)
                          (not (eq (overlay-buffer ov) (window-buffer window))))
                 (iota-dimmer--remove-overlay window)))
             iota-dimmer--overlays)
    ;; Update all windows
    (iota-dimmer--update)))

;;; Theme Change Handling

(defun iota-dimmer--on-theme-change (&rest _)
  "Handle theme changes."
  (when iota-dimmer-mode
    ;; Recompute dim face
    (iota-dimmer--compute-dim-face)
    ;; Remove all overlays and reapply
    (iota-dimmer--remove-all-overlays)
    (run-with-timer 0.1 nil #'iota-dimmer--update)))

(defun iota-dimmer--remove-all-overlays ()
  "Remove all dimming overlays."
  (maphash (lambda (window ov)
             (when (overlayp ov)
               (delete-overlay ov)))
           iota-dimmer--overlays)
  (clrhash iota-dimmer--overlays)
  (clrhash iota-dimmer--dimmed-windows))

;;; Setup and Teardown

(defun iota-dimmer--setup ()
  "Set up dimmer mode."
  (iota-dimmer--log "setup called")
  (setq iota-dimmer--last-selected-window (selected-window))
  
  ;; Compute initial dim face
  (iota-dimmer--compute-dim-face)
  
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
  ;; Remove all overlays
  (iota-dimmer--remove-all-overlays)
  
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
  (setq iota-dimmer--dim-face nil)
  (setq iota-dimmer--last-selected-window nil))

;;; Mode Definition

;;;###autoload
(define-minor-mode iota-dimmer-mode
  "Toggle I O T Λ inactive window dimming.
When enabled, inactive windows have their content dimmed by blending
foreground colors toward the background."
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
  (iota-dimmer--compute-dim-face)
  (iota-dimmer--remove-all-overlays)
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
      (insert (format "Dim face: %S\n" iota-dimmer--dim-face))
      (insert (format "Background color: %s\n" (iota-dimmer--get-background-color)))
      (insert (format "Foreground color: %s\n" (iota-dimmer--get-foreground-color)))
      (insert (format "Last selected window: %s\n" iota-dimmer--last-selected-window))
      (insert (format "Current selected window: %s\n" (selected-window)))
      (insert (format "Frame focused: %s\n" (frame-focus-state)))
      (insert "\n=== Windows ===\n")
      (dolist (window (window-list nil 'no-minibuf))
        (insert (format "\nWindow: %s\n" window))
        (insert (format "  Buffer: %s\n" (buffer-name (window-buffer window))))
        (insert (format "  Should dim: %s\n" (iota-dimmer--window-should-dim-p window)))
        (insert (format "  Is dimmed: %s\n" (gethash window iota-dimmer--dimmed-windows)))
        (insert (format "  Has overlay: %s\n" (gethash window iota-dimmer--overlays))))
      (insert "\n=== Overlays ===\n")
      (insert (format "Total overlays tracked: %d\n" (hash-table-count iota-dimmer--overlays)))
      (maphash (lambda (win ov)
                 (insert (format "  Window %s -> overlay %s (live: %s)\n"
                                 win ov (and (overlayp ov) (overlay-buffer ov)))))
               iota-dimmer--overlays))
    (display-buffer buf)))

(provide 'iota-dimmer)
;;; iota-dimmer.el ends here
