;;; iota-modal.el --- Native semantic modal editing for IOTA -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: faces, modal, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (modalka "0.1.5"))

;;; Commentary:

;; ι • ο • τ • α
;; Native Semantic Modal Editing for IOTA
;;
;; This file is part of I O T Λ.
;;
;; The Iota modal system provides ergonomic modal editing while preserving
;; Emacs semantics. Built exclusively on modalka, Iota creates a native
;; translation layer where:
;;
;;   n → C-n (next-line)
;;   p → C-p (previous-line)
;;   w → M-w (copy/kill-ring-save)
;;   y → C-y (paste/yank)
;;   SPC → C-SPC (set-mark)
;;
;; This eliminates RSI-inducing modifier chords while maintaining muscle
;; memory for long-time Emacs users.
;;
;; Visual Feedback:
;;   - COMMAND mode: Box cursor (█), colored modeline indicator
;;   - INSERT mode: Bar cursor (|), neutral modeline indicator
;;
;; Usage:
;;   (require 'iota-modal)
;;   (iota-modal-mode 1)
;;
;; Toggle modes:
;;   ESC - Enter COMMAND mode
;;   i   - Enter INSERT mode (in COMMAND mode)

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'iota-theme)

;;; Dependencies

;; Initialize package system
(require 'package)
(require 'vc-git nil t)

;; Ensure MELPA is available
(unless (assoc 'melpa package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Initialize packages if needed
(unless package--initialized
  (package-initialize))

;; Refresh package contents if modalka not available
(unless (package-installed-p 'modalka)
  (package-refresh-contents))

;; Use use-package for dependency management
(require 'use-package)

(use-package modalka
  :ensure t
  :demand t)

;; Forward declarations for modalka
(defvar modalka-mode)
(defvar modalka-mode-map)
(defvar modalka-cursor-type)
(declare-function modalka-mode "modalka")
(declare-function modalka-define-kbd "modalka")

;; Forward declarations for iota modules
(defvar iota-modeline-segments-preset)
(defvar iota-modeline-custom-segments)
(declare-function iota-modeline-refresh "iota-modeline")
(declare-function iota-segment-create "iota-segment")
(declare-function iota-segment-id "iota-segment")
(declare-function iota-segments-minimal "iota-segments")
(declare-function iota-segments-standard "iota-segments")
(declare-function iota-segments-full "iota-segments")

;;; Configuration

(defgroup iota-modal nil
  "IOTA native semantic modal configuration."
  :group 'iota
  :prefix "iota-modal-")

(defcustom iota-modal-command-cursor-type 'box
  "Cursor type for COMMAND mode."
  :type '(choice (const :tag "Box" box)
                 (const :tag "Hollow" hollow)
                 (const :tag "Bar" bar)
                 (const :tag "Hbar" hbar))
  :group 'iota-modal)

(defcustom iota-modal-insert-cursor-type '(bar . 2)
  "Cursor type for INSERT mode."
  :type '(choice (const :tag "Box" box)
                 (const :tag "Hollow" hollow)
                 (const :tag "Bar" bar)
                 (const :tag "Hbar" hbar)
                 (cons :tag "Bar with width" (const bar) integer))
  :group 'iota-modal)

(defcustom iota-modal-command-color nil
  "Color for COMMAND mode indicator.
When nil (default), uses `iota-theme-get-success-color' for theme integration."
  :type '(choice (const :tag "Auto (from theme)" nil)
                 (color :tag "Custom color"))
  :group 'iota-modal)

(defcustom iota-modal-insert-color nil
  "Color for INSERT mode indicator.
When nil (default), uses `iota-theme-get-muted-color' for theme integration."
  :type '(choice (const :tag "Auto (from theme)" nil)
                 (color :tag "Custom color"))
  :group 'iota-modal)

(defcustom iota-modal-indicator-style 'both
  "Style for modal state indicators.
- `both': Show both glyph and label (e.g., \"● COMMAND\")
- `glyph': Show only glyph (e.g., \"●\")
- `label': Show only label (e.g., \"COMMAND\")"
  :type '(choice (const :tag "Both glyph and label" both)
                 (const :tag "Glyph only" glyph)
                 (const :tag "Label only" label))
  :group 'iota-modal)

(defcustom iota-modal-excluded-modes
  '(dired-mode
    magit-mode magit-status-mode magit-log-mode magit-diff-mode
    magit-revision-mode magit-refs-mode magit-stash-mode
    Info-mode help-mode
    minibuffer-mode minibuffer-inactive-mode
    term-mode vterm-mode eshell-mode shell-mode
    org-agenda-mode
    treemacs-mode
    which-key-mode)
  "List of major modes where modalka should not be activated."
  :type '(repeat symbol)
  :group 'iota-modal)

;;; State Variables

(defvar-local iota-modal--current-state 'insert
  "Current modal state: `command' or `insert'.
Buffer-local so each buffer tracks its own state.")

(defvar iota-modal--original-cursor-type nil
  "Original cursor-type before modal editing was enabled.")

(defvar iota-modal--original-cursor-color nil
  "Original cursor color before modal editing was enabled.")

;;; Cursor Management

(defun iota-modal--save-cursor-state ()
  "Save the original cursor state before modal editing."
  (unless iota-modal--original-cursor-type
    (setq iota-modal--original-cursor-type
          (or (default-value 'cursor-type) 'bar)))
  (unless iota-modal--original-cursor-color
    (setq iota-modal--original-cursor-color
          (face-attribute 'cursor :background nil 'default))))

(defun iota-modal--restore-cursor-state ()
  "Restore cursor to its original state before modal editing."
  (when iota-modal--original-cursor-type
    (setq-default cursor-type iota-modal--original-cursor-type)
    (setq cursor-type iota-modal--original-cursor-type))
  (when iota-modal--original-cursor-color
    (set-face-attribute 'cursor nil :background iota-modal--original-cursor-color))
  ;; Terminal cursor reset
  (unless (display-graphic-p)
    (send-string-to-terminal "\e[5 q"))
  (ignore-errors (redraw-frame)))

(defun iota-modal--restore-cursor-on-exit ()
  "Restore terminal cursor shape when Emacs exits.
This ensures the terminal is left in a clean state."
  (unless (display-graphic-p)
    ;; Reset to default cursor (usually blinking bar or block depending on terminal)
    (send-string-to-terminal "\e[0 q")))

(defun iota-modal--update-cursor ()
  "Update cursor based on current modal state."
  (if (bound-and-true-p modalka-mode)
      (progn
        (setq cursor-type iota-modal-command-cursor-type)
        (unless (display-graphic-p)
          (send-string-to-terminal "\e[1 q"))) ; Block cursor
    (progn
      (setq cursor-type iota-modal-insert-cursor-type)
      (unless (display-graphic-p)
        (send-string-to-terminal "\e[5 q"))))) ; Bar cursor

;;; Modeline Indicator

(defun iota-modal--get-command-color ()
  "Get the color for COMMAND mode indicator.
Uses custom color if set, otherwise derives from theme."
  (or iota-modal-command-color
      (and (fboundp 'iota-theme-get-success-color)
           (iota-theme-get-success-color))
      "#50fa7b"))  ; Fallback

(defun iota-modal--get-insert-color ()
  "Get the color for INSERT mode indicator.
Uses custom color if set, otherwise derives from theme."
  (or iota-modal-insert-color
      (and (fboundp 'iota-theme-get-muted-color)
           (iota-theme-get-muted-color))
      "#6272a4"))  ; Fallback

(defun iota-modal--format-indicator (state style)
  "Format modal STATE indicator using STYLE.
STATE is `command' or `insert'.
STYLE can be `both', `glyph', or `label'."
  (let* ((is-command (eq state 'command))
         (glyph (if is-command "●" "○"))
         (label (if is-command "COMMAND" "INSERT"))
         (color (if is-command
                    (iota-modal--get-command-color)
                  (iota-modal--get-insert-color)))
         (text (pcase style
                 ('both (concat glyph " " label))
                 ('glyph glyph)
                 ('label label)
                 (_ (concat glyph " " label)))))
    (propertize text
                'face `(:foreground ,color :weight bold)
                'help-echo (format "Iota: %s mode (ESC to toggle)" label))))

(defun iota-modal--build-indicator ()
  "Build the modal state indicator for the modeline."
  (let ((state (if (bound-and-true-p modalka-mode) 'command 'insert)))
    (setq iota-modal--current-state state)
    (if (iota-theme-window-active-p)
        (iota-modal--format-indicator state iota-modal-indicator-style)
      ;; Inactive window: show same content but dimmed
      (propertize (substring-no-properties
                   (iota-modal--format-indicator state iota-modal-indicator-style))
                  'face 'iota-inactive-modeline-face))))

(defun iota-modal--build-short-indicator ()
  "Build the short modal state indicator (glyph only) for the modeline."
  (let ((state (if (bound-and-true-p modalka-mode) 'command 'insert)))
    (iota-modal--format-indicator state 'glyph)))

(defun iota-modal-state-segment ()
  "Create a modeline segment showing modal state.

Shows:
- ● COMMAND (green) when modalka is active
- ○ INSERT (gray) when modalka is inactive

Active windows show colored indicators, inactive windows show dimmed."
  (require 'iota-segment)
  (iota-segment-create
   :id 'modal-state
   :text #'iota-modal--build-indicator
   :short-text #'iota-modal--build-short-indicator
   :face nil ; Don't override the propertized face
   :align 'left
   :priority 150
   :help-echo (lambda ()
                (format "Iota modal state: %s\nPress ESC to toggle"
                        iota-modal--current-state))))

;;; Key Translations

(defun iota-modal--setup-keys ()
  "Set up modalka key translations for Iota semantic bindings.
This implements the Iota Semantic Specification from the architecture document."

  ;; === Navigation Layer (Section 3.1) ===
  ;; Character/Line movement - use direct bindings for reliability
  (define-key modalka-mode-map (kbd "n") #'next-line)
  (define-key modalka-mode-map (kbd "p") #'previous-line)
  (define-key modalka-mode-map (kbd "f") #'forward-char)
  (define-key modalka-mode-map (kbd "b") #'backward-char)
  (define-key modalka-mode-map (kbd "a") #'move-beginning-of-line)
  (define-key modalka-mode-map (kbd "e") #'move-end-of-line)

  ;; Word movement
  (define-key modalka-mode-map (kbd "F") #'forward-word)
  (define-key modalka-mode-map (kbd "B") #'backward-word)

  ;; Scrolling
  (define-key modalka-mode-map (kbd "v") #'scroll-up-command)    ; Page Down
  (define-key modalka-mode-map (kbd "V") #'scroll-down-command)  ; Page Up

  ;; Buffer boundaries
  (define-key modalka-mode-map (kbd "<") #'beginning-of-buffer)
  (define-key modalka-mode-map (kbd ">") #'end-of-buffer)

  ;; Paragraph motions
  (define-key modalka-mode-map (kbd "{") #'backward-paragraph)
  (define-key modalka-mode-map (kbd "}") #'forward-paragraph)

  ;; Sexp motions (balanced expressions)
  (define-key modalka-mode-map (kbd "[") #'backward-sexp)
  (define-key modalka-mode-map (kbd "]") #'forward-sexp)

  ;; Recentering
  (define-key modalka-mode-map (kbd "l") #'recenter-top-bottom)

  ;; Buffer management
  (define-key modalka-mode-map (kbd "q") #'kill-current-buffer)

  ;; === Editing Layer (Section 3.2) ===
  ;; Copy/Paste/Cut - THE CORE REQUIREMENT
  (define-key modalka-mode-map (kbd "w") #'kill-ring-save)    ; COPY
  (define-key modalka-mode-map (kbd "y") #'yank)              ; PASTE
  (define-key modalka-mode-map (kbd "W") #'kill-region)       ; CUT
  (define-key modalka-mode-map (kbd "Y") #'yank-pop)

  ;; Killing and deleting
  (define-key modalka-mode-map (kbd "k") #'kill-line)
  (define-key modalka-mode-map (kbd "d") #'delete-char)
  (define-key modalka-mode-map (kbd "D") #'kill-word)

  ;; Undo/Redo
  (define-key modalka-mode-map (kbd "u") #'undo)
  (define-key modalka-mode-map (kbd "U") #'undo-redo)

  ;; Completion
  (define-key modalka-mode-map (kbd "/") #'dabbrev-expand)

  ;; Mark/Selection
  (define-key modalka-mode-map (kbd "SPC") #'set-mark-command)

  ;; Transpose
  (define-key modalka-mode-map (kbd "t") #'transpose-chars)

  ;; === Search (Section 7.2) ===
  (define-key modalka-mode-map (kbd "s") #'isearch-forward)
  (define-key modalka-mode-map (kbd "r") #'isearch-backward)

  ;; === Prefix Delegation (Section 3.3) ===
  ;; For x prefix, we need to respect user's C-x bindings including those
  ;; made with :bind* (which use override-global-map).
  ;; We create a command that simulates C-x and then reads the next key.
  (define-key modalka-mode-map (kbd "x") #'iota-modal--simulate-C-x)
  (define-key modalka-mode-map (kbd "X") #'execute-extended-command)  ; M-x
  ;; Note: 'c' is reserved for the leader key (iota-leader.el)
  ;; C-c prefix is accessible via leader: c c
  ;; Help prefix: simulate C-h
  (define-key modalka-mode-map (kbd "h") #'iota-modal--simulate-C-h)
  (define-key modalka-mode-map (kbd "g") #'keyboard-quit)

  ;; === Arrow Keys Passthrough ===
  ;; Arrow keys must work exactly as normal
  (define-key modalka-mode-map (kbd "<up>") #'previous-line)
  (define-key modalka-mode-map (kbd "<down>") #'next-line)
  (define-key modalka-mode-map (kbd "<left>") #'backward-char)
  (define-key modalka-mode-map (kbd "<right>") #'forward-char)
  (define-key modalka-mode-map (kbd "<home>") #'move-beginning-of-line)
  (define-key modalka-mode-map (kbd "<end>") #'move-end-of-line)
  (define-key modalka-mode-map (kbd "<prior>") #'scroll-down-command)  ; Page Up
  (define-key modalka-mode-map (kbd "<next>") #'scroll-up-command)    ; Page Down

  ;; === Mode Switching ===
  ;; Enter insert mode with 'i'
  (define-key modalka-mode-map (kbd "i") #'iota-modal-enter-insert-mode)

  ;; o/O for open line
  (define-key modalka-mode-map (kbd "o") #'iota-modal--open-line-below)
  (define-key modalka-mode-map (kbd "O") #'iota-modal--open-line-above)

  ;; === Escape Hatch ===
  ;; Bind <escape> in modalka-mode-map for COMMAND mode (does keyboard-quit)
  (define-key modalka-mode-map (kbd "<escape>") #'keyboard-quit))

(defun iota-modal-enter-command-mode ()
  "Enter COMMAND mode (activate modalka)."
  (interactive)
  ;; Allow explicit entry to command mode (don't check should-activate-p)
  ;; Only skip minibuffer
  (unless (minibufferp)
    (unless (bound-and-true-p modalka-mode)
      (modalka-mode 1)
      (message "COMMAND mode"))))

(defun iota-modal-enter-insert-mode ()
  "Enter INSERT mode (deactivate modalka)."
  (interactive)
  (when (bound-and-true-p modalka-mode)
    (modalka-mode -1)
    (message "INSERT mode")))

(defun iota-modal--open-line-below ()
  "Open a new line below and enter insert mode."
  (interactive)
  (move-end-of-line nil)
  (newline)
  (iota-modal-enter-insert-mode))

(defun iota-modal--open-line-above ()
  "Open a new line above and enter insert mode."
  (interactive)
  (move-beginning-of-line nil)
  (open-line 1)
  (iota-modal-enter-insert-mode))

;;; Prefix Key Simulation
;; These functions simulate C-x, C-c, C-h prefixes by reading the next key
;; and looking up the binding through the normal key-binding mechanism.
;; This ensures user bindings (including :bind* / override-global-map) work.

(defvar iota-modal--x-convenience-map
  (let ((map (make-sparse-keymap)))
    ;; Convenience mappings: x <key> -> C-x C-<key>
    ;; These are the most common C-x C-* commands accessed without Control
    (define-key map (kbd "f") #'find-file)              ; C-x C-f
    (define-key map (kbd "s") #'save-buffer)            ; C-x C-s
    (define-key map (kbd "w") #'write-file)             ; C-x C-w
    (define-key map (kbd "e") #'eval-last-sexp)         ; C-x C-e
    (define-key map (kbd "c") #'save-buffers-kill-terminal)  ; C-x C-c
    map)
  "Convenience keymap for common C-x C-* commands.")

(defun iota-modal--simulate-prefix (prefix-key prefix-name &optional convenience-map)
  "Simulate PREFIX-KEY (like \"C-x\") and execute the resulting command.
PREFIX-NAME is used for the prompt.
CONVENIENCE-MAP is an optional keymap checked first for convenience bindings.
This function continues reading keys until a command is found, supporting
multi-key sequences like C-c v v."
  (let* ((keys (read-key-sequence-vector (concat prefix-name "-")))
         ;; First check convenience map for single-key shortcuts
         (convenience-cmd (and convenience-map
                               (lookup-key convenience-map keys)))
         ;; Build the full key sequence with prefix
         (full-key (vconcat (kbd prefix-key) keys))
         (cmd (or (and (commandp convenience-cmd) convenience-cmd)
                  (key-binding full-key))))
    ;; If the binding is a keymap, continue reading keys
    (while (keymapp cmd)
      (let ((next-keys (read-key-sequence-vector 
                        (concat prefix-name " " (key-description keys) "-"))))
        (setq keys (vconcat keys next-keys))
        (setq full-key (vconcat (kbd prefix-key) keys))
        (setq cmd (key-binding full-key))))
    (if cmd
        (if (commandp cmd)
            (call-interactively cmd)
          (error "%s %s is not a command" prefix-name (key-description keys)))
      (message "%s %s is undefined" prefix-name (key-description keys)))))

(defun iota-modal--simulate-C-x ()
  "Simulate C-x prefix and execute the following command.
Common commands like `f' (find-file), `s' (save) work without Control.
All other bindings including user customizations (like windmove) work too."
  (interactive)
  (iota-modal--simulate-prefix "C-x" "x" iota-modal--x-convenience-map))

(defun iota-modal--simulate-C-c ()
  "Simulate C-c prefix and execute the following command."
  (interactive)
  (iota-modal--simulate-prefix "C-c" "c"))

(defun iota-modal--simulate-C-h ()
  "Simulate C-h prefix and execute the following command."
  (interactive)
  (iota-modal--simulate-prefix "C-h" "h"))

;;; Debug Logging

(defvar iota-modal-debug nil
  "When non-nil, log keystrokes and mode changes for debugging.")

(defun iota-modal--log-keystroke ()
  "Log the current keystroke for debugging."
  (when iota-modal-debug
    (let ((keys (this-command-keys-vector))
          (cmd this-command))
      (message "IOTA DEBUG: keys=%S cmd=%S modalka=%s"
               keys cmd (if (bound-and-true-p modalka-mode) "ON" "OFF")))))

(defun iota-modal-toggle-debug ()
  "Toggle keystroke debugging."
  (interactive)
  (setq iota-modal-debug (not iota-modal-debug))
  (if iota-modal-debug
      (progn
        (add-hook 'pre-command-hook #'iota-modal--log-keystroke)
        (message "IOTA keystroke debugging ENABLED"))
    (remove-hook 'pre-command-hook #'iota-modal--log-keystroke)
    (message "IOTA keystroke debugging DISABLED")))

;;; Mode Activation Hooks

(defun iota-modal--should-activate-p ()
  "Return t if modalka should be activated in current buffer."
  (and (not (minibufferp))
       (not (member major-mode iota-modal-excluded-modes))
       (not (derived-mode-p 'special-mode))
       (not (string-prefix-p " *" (buffer-name)))
       ;; Allow splash screen, exclude other * buffers
       (or (string-match-p "I O T Λ splash" (buffer-name))
           (not (string-prefix-p "*" (buffer-name))))))

(defun iota-modal--maybe-activate ()
  "Activate modalka in current buffer if appropriate.
Called from text-mode-hook and prog-mode-hook."
  (when (iota-modal--should-activate-p)
    (modalka-mode 1)))

;;; Hooks for State Changes

(defun iota-modal--on-mode-change ()
  "Hook function called when modalka-mode is toggled.
Updates cursor and forces modeline refresh."
  (iota-modal--update-cursor)
  ;; Update the modal state variable
  (setq iota-modal--current-state (if (bound-and-true-p modalka-mode) 'command 'insert))
  ;; Force modeline update via multiple methods for reliability
  (force-mode-line-update t)
  ;; Also trigger iota-modeline refresh if available
  (when (fboundp 'iota-modeline-refresh)
    (iota-modeline-refresh))
  ;; Request update via centralized system if available
  (when (fboundp 'iota-update-request)
    (iota-update-request :modeline t)))

(defun iota-modal--on-window-selection-change (frame)
  "Update cursor when window selection changes in FRAME.
This ensures cursor reflects the modal state of the newly selected buffer."
  (iota-modal--update-cursor))

(defun iota-modal--setup-hooks ()
  "Set up hooks for modal state tracking."
  ;; Track modalka state changes
  (add-hook 'modalka-mode-hook #'iota-modal--on-mode-change)
  ;; Activate in text and programming modes (whitelist approach)
  (add-hook 'text-mode-hook #'iota-modal--maybe-activate)
  (add-hook 'prog-mode-hook #'iota-modal--maybe-activate)
  ;; Update cursor when switching windows/buffers
  (add-hook 'window-selection-change-functions #'iota-modal--on-window-selection-change)
  ;; Restore cursor on Emacs exit
  (add-hook 'kill-emacs-hook #'iota-modal--restore-cursor-on-exit))

(defun iota-modal--remove-hooks ()
  "Remove all iota-modal hooks."
  (remove-hook 'modalka-mode-hook #'iota-modal--on-mode-change)
  (remove-hook 'text-mode-hook #'iota-modal--maybe-activate)
  (remove-hook 'prog-mode-hook #'iota-modal--maybe-activate)
  (remove-hook 'window-selection-change-functions #'iota-modal--on-window-selection-change)
  (remove-hook 'kill-emacs-hook #'iota-modal--restore-cursor-on-exit))

;;; Segment Management

(defvar iota-modal--original-preset nil
  "Original modeline preset before modal mode was enabled.")

(defun iota-modal-add-segment ()
  "Add modal state indicator to current modeline configuration."
  (interactive)
  (require 'iota-segments)

  ;; Save original preset
  (setq iota-modal--original-preset iota-modeline-segments-preset)

  ;; Switch to custom and add segment
  (setq iota-modeline-segments-preset 'custom)

  (let ((current-segments (pcase iota-modal--original-preset
                            ('minimal (iota-segments-minimal))
                            ('standard (iota-segments-standard))
                            ('full (iota-segments-full))
                            ('custom iota-modeline-custom-segments)
                            (_ (iota-segments-standard)))))

    ;; Only add if not already present
    (unless (cl-find-if (lambda (s)
                          (and (cl-struct-p s)
                               (ignore-errors (eq (iota-segment-id s) 'modal-state))))
                        current-segments)
      (setq iota-modeline-custom-segments
            (cons (iota-modal-state-segment) current-segments))))

  (when (bound-and-true-p iota-modeline-mode)
    (iota-modeline-refresh)))

(defun iota-modal-remove-segment ()
  "Remove modal state indicator and restore original preset."
  (interactive)
  (when (eq iota-modeline-segments-preset 'custom)
    (setq iota-modeline-custom-segments
          (cl-remove-if (lambda (seg)
                          (and (cl-struct-p seg)
                               (ignore-errors (eq (iota-segment-id seg) 'modal-state))))
                        iota-modeline-custom-segments))

    (when (and iota-modal--original-preset
               (not (eq iota-modal--original-preset 'custom)))
      (setq iota-modeline-segments-preset iota-modal--original-preset)
      (setq iota-modal--original-preset nil))

    (when (bound-and-true-p iota-modeline-mode)
      (iota-modeline-refresh))))

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode iota-modal-mode
  "Enable Iota native semantic modal editing.

Iota provides ergonomic modal editing while preserving Emacs semantics.
Built on modalka, it maps single keys to Emacs commands:

Navigation:
  n/p     → C-n/C-p (next/previous line)
  f/b     → C-f/C-b (forward/backward char)
  F/B     → M-f/M-b (forward/backward word)
  a/e     → C-a/C-e (beginning/end of line)
  v/V     → C-v/M-v (page down/up)
  </> → M-</M-> (beginning/end of buffer)

Editing:
  w       → M-w (copy/kill-ring-save)
  y       → C-y (paste/yank)
  W       → C-w (cut/kill-region)
  k       → C-k (kill-line)
  d       → C-d (delete-char)
  /       → C-/ (undo)
  SPC     → C-SPC (set-mark)

Search:
  s       → C-s (isearch-forward)
  r       → C-r (isearch-backward)

Prefix Delegation:
  x       → C-x (command prefix)
  c       → Leader key (iota commands menu)
  h       → C-h (help prefix)

Mode Switching:
  i       → Enter INSERT mode
  ESC     → Enter COMMAND mode

Visual Feedback:
  COMMAND mode: Box cursor (█), green indicator
  INSERT mode:  Bar cursor (|), gray indicator"
  :global t
  :group 'iota-modal
  :lighter " ι"
  (if iota-modal-mode
      (progn
        ;; Save original cursor state
        (iota-modal--save-cursor-state)
        ;; Configure cursor types for modalka
        (setq modalka-cursor-type iota-modal-command-cursor-type)
        (setq-default cursor-type iota-modal-insert-cursor-type)
        ;; Set up key translations
        (iota-modal--setup-keys)
        ;; Set up hooks
        (iota-modal--setup-hooks)
        ;; ESC binding for mode switching (works in GUI Emacs)
        (global-set-key (kbd "<escape>") #'iota-modal-enter-command-mode)
        (global-set-key [escape] #'iota-modal-enter-command-mode)
        ;; C-] for terminal (ESC doesn't work reliably in terminals)
        (global-set-key (kbd "C-]") #'iota-modal-enter-command-mode)
        ;; Add modeline segment
        (condition-case err
            (iota-modal-add-segment)
          (error (message "Warning: Could not add modal segment: %s" err)))
        ;; Load and set up leader key
        (when (require 'iota-leader nil t)
          (when (fboundp 'iota-leader-mode)
            (iota-leader-mode 1)))
        ;; Activate in existing buffers
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (and (iota-modal--should-activate-p)
                       (or (derived-mode-p 'text-mode)
                           (derived-mode-p 'prog-mode)))
              (modalka-mode 1)))))
    (progn
      ;; Remove global ESC and C-] bindings
      (global-unset-key (kbd "<escape>"))
      (global-unset-key [escape])
      (global-unset-key (kbd "C-]"))
      ;; Disable leader mode
      (when (fboundp 'iota-leader-mode)
        (iota-leader-mode -1))
      ;; Remove hooks
      (iota-modal--remove-hooks)
      ;; Deactivate modalka in all buffers
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (bound-and-true-p modalka-mode)
            (modalka-mode -1))))
      ;; Remove modeline segment
      (condition-case err
          (iota-modal-remove-segment)
        (error (message "Warning: Could not remove modal segment: %s" err)))
      ;; Restore cursor
      (iota-modal--restore-cursor-state)
      (message "Iota modal mode disabled"))))

;;; Interactive Commands

;;;###autoload
(defun iota-modal-toggle ()
  "Toggle between COMMAND and INSERT mode."
  (interactive)
  (if (bound-and-true-p modalka-mode)
      (modalka-mode -1)
    (modalka-mode 1)))

;;;###autoload
(defun iota-modal-cycle-indicator-style ()
  "Cycle through modal indicator styles: both → glyph → label → both."
  (interactive)
  (setq iota-modal-indicator-style
        (pcase iota-modal-indicator-style
          ('both 'glyph)
          ('glyph 'label)
          ('label 'both)
          (_ 'both)))
  (force-mode-line-update t)
  (message "Modal indicator style: %s" iota-modal-indicator-style))

;;;###autoload
(defun iota-modal-which-key-show ()
  "Display available commands in current modal state."
  (interactive)
  (if (bound-and-true-p modalka-mode)
      (message "COMMAND mode - Keys: n/p(line) f/b(char) w(copy) y(paste) W(cut) SPC(mark) x(C-x) i(insert)")
    (message "INSERT mode - Press ESC to enter COMMAND mode")))

(provide 'iota-modal)
;;; iota-modal.el ends here
