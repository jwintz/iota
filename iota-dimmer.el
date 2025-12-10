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

;;; Customization

(defgroup iota-dimmer nil
  "I O T Λ inactive window dimming configuration."
  :group 'iota
  :prefix "iota-dimmer-")

(defcustom iota-dimmer-fraction 0.40
  "Amount to dim inactive windows.
Value between 0.0 (no dimming) and 1.0 (maximum dimming).
This is the base value used when saturation/luminance fractions are nil."
  :type '(number :tag "Dimming fraction (0.0-1.0)")
  :group 'iota-dimmer)

(defcustom iota-dimmer-saturation-fraction nil
  "Saturation reduction fraction for dimming.
Value between 0.0 (keep original saturation) and 1.0 (fully desaturated/gray).
When nil, uses `iota-dimmer-fraction' * 0.6 as default."
  :type '(choice (const :tag "Use default (fraction * 0.6)" nil)
                 (number :tag "Saturation fraction (0.0-1.0)"))
  :group 'iota-dimmer)

(defcustom iota-dimmer-luminance-fraction nil
  "Luminance blend fraction for dimming.
Value between 0.0 (keep original brightness) and 1.0 (blend fully to background).
When nil, uses `iota-dimmer-fraction' * 0.5 as default."
  :type '(choice (const :tag "Use default (fraction * 0.5)" nil)
                 (number :tag "Luminance fraction (0.0-1.0)"))
  :group 'iota-dimmer)

;; Presets documentation:
;;
;; | Effect                    | saturation | luminance | Description                          |
;; |---------------------------|------------|-----------|--------------------------------------|
;; | Subtle dim                | 0.15       | 0.10      | Barely noticeable, gentle fade       |
;; | Balanced (default)        | nil        | nil       | Good balance of desaturation + fade  |
;; | Desaturated only          | 0.50       | 0.00      | Gray out colors, keep brightness     |
;; | Fade only                 | 0.00       | 0.40      | Keep colors vivid, fade to background|
;; | Washed out                | 0.60       | 0.30      | Pastel/washed look                   |
;; | Strong dim                | 0.50       | 0.50      | Very noticeable dimming              |
;; | Grayscale fade            | 0.80       | 0.40      | Nearly grayscale, faded              |
;; | Muted colors              | 0.40       | 0.15      | Less vibrant, slightly darker        |
;; | High contrast preserve    | 0.20       | 0.50      | Keep colors, fade brightness more    |

(defcustom iota-dimmer-excluded-buffers
  '("^ \\*Minibuf"
    "^ \\*Echo Area"
    "^\\*which-key\\*"
    "^ \\*which-key\\*"
    "^\\*transient\\*"
    "^ \\*transient\\*"
    "^\\*Completions\\*")
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

(defvar iota-dimmer--refresh-in-progress nil
  "Non-nil when a refresh operation is in progress.
Prevents recursive updates.")

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
    (let* ((sat-frac (or iota-dimmer-saturation-fraction
                         (* iota-dimmer-fraction 0.6)))
           (lum-frac (or iota-dimmer-luminance-fraction
                         (* iota-dimmer-fraction 0.5)))
           (cache-key (list color sat-frac lum-frac))
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
                         ;; Reduce saturation: 0 = keep, 1 = fully gray
                         (new-s (* s (- 1.0 sat-frac)))
                         ;; Blend luminance toward background: 0 = keep, 1 = match bg
                         (new-l (+ l (* (- bg-l l) lum-frac)))
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
    (not (or
          ;; Exclude iota UI faces (modeline, box, etc.) but NOT screen faces
          (and (string-prefix-p "iota-" name)
               (not (string-prefix-p "iota-screens-" name)))
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
        (let ((remaps nil))
          ;; Remap each face that has a foreground color
          (dolist (face (face-list))
            (when (iota-dimmer--face-should-dim-p face)
              (let ((spec (iota-dimmer--create-dimmed-face-spec face)))
                (when spec
                  (condition-case nil
                      (let ((cookie (face-remap-add-relative face spec)))
                        (push (cons face cookie) remaps))
                    (error nil))))))
          (puthash buffer remaps iota-dimmer--face-remaps)
          (setq iota-dimmer--buffer-dimmed t))))))

(defun iota-dimmer--undim-buffer (buffer)
  "Remove dimming from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when iota-dimmer--buffer-dimmed
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
  (unless (or iota-dimmer--refresh-in-progress
              (bound-and-true-p iota--inhibit-updates))
    (let ((selected (selected-window))
          (frame-focused (frame-focus-state)))
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
            (iota-dimmer--dim-buffer buffer)))))))

(defun iota-dimmer--on-window-change (&optional _frame)
  "Handle window selection changes."
  (when iota-dimmer-mode
    (let ((current (selected-window)))
      (unless (eq current iota-dimmer--last-selected-window)
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
  (iota-dimmer--update))

(defun iota-dimmer--teardown ()
  "Tear down dimmer mode."
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
  (let ((iota-dimmer--refresh-in-progress t))
    (iota-dimmer--clear-cache)
    (iota-dimmer--undim-all)
    (iota-dimmer--update)
    ;; Force complete redisplay of all frames
    (redraw-display)))

(defun iota-dimmer-set-fraction (fraction)
  "Set dimming FRACTION interactively."
  (interactive "nDimming fraction (0.0-1.0): ")
  (setq iota-dimmer-fraction (max 0.0 (min 1.0 fraction)))
  (iota-dimmer-refresh)
  (message "Dimming fraction set to %.2f" iota-dimmer-fraction))

(defun iota-dimmer-apply-preset (preset)
  "Apply a dimming PRESET.
PRESET can be a string or symbol.
Available presets:
  subtle          - Barely noticeable (sat: 0.15, lum: 0.10)
  balanced        - Default balance (uses fraction defaults)
  desaturated     - Gray out, keep brightness (sat: 0.50, lum: 0.00)
  fade-only       - Keep colors, fade brightness (sat: 0.00, lum: 0.40)
  washed          - Pastel/washed look (sat: 0.60, lum: 0.30)
  strong          - Very noticeable (sat: 0.50, lum: 0.50)
  grayscale       - Nearly gray, faded (sat: 0.80, lum: 0.40)
  muted           - Less vibrant (sat: 0.40, lum: 0.15)
  high-contrast   - Keep colors, fade more (sat: 0.20, lum: 0.50)"
  (interactive
   (list (completing-read "Preset: "
                          '("subtle" "balanced" "desaturated" "fade-only"
                            "washed" "strong" "grayscale" "muted" "high-contrast")
                          nil t)))
  ;; Convert symbol to string for compatibility with transient
  (when (symbolp preset)
    (setq preset (symbol-name preset)))
  (pcase preset
    ("subtle"        (setq iota-dimmer-saturation-fraction 0.15
                           iota-dimmer-luminance-fraction 0.10))
    ("balanced"      (setq iota-dimmer-saturation-fraction nil
                           iota-dimmer-luminance-fraction nil))
    ("desaturated"   (setq iota-dimmer-saturation-fraction 0.50
                           iota-dimmer-luminance-fraction 0.00))
    ("fade-only"     (setq iota-dimmer-saturation-fraction 0.00
                           iota-dimmer-luminance-fraction 0.40))
    ("washed"        (setq iota-dimmer-saturation-fraction 0.60
                           iota-dimmer-luminance-fraction 0.30))
    ("strong"        (setq iota-dimmer-saturation-fraction 0.50
                           iota-dimmer-luminance-fraction 0.50))
    ("grayscale"     (setq iota-dimmer-saturation-fraction 0.80
                           iota-dimmer-luminance-fraction 0.40))
    ("muted"         (setq iota-dimmer-saturation-fraction 0.40
                           iota-dimmer-luminance-fraction 0.15))
    ("high-contrast" (setq iota-dimmer-saturation-fraction 0.20
                           iota-dimmer-luminance-fraction 0.50)))
  (iota-dimmer-refresh)
  (message "Applied '%s' preset (sat: %s, lum: %s)"
           preset
           (or iota-dimmer-saturation-fraction "default")
           (or iota-dimmer-luminance-fraction "default")))

(provide 'iota-dimmer)
;;; iota-dimmer.el ends here
