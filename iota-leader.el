;;; iota-leader.el --- Leader key framework for IOTA -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: convenience, modal
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Leader Key Framework for IOTA
;;
;; This file is part of I O T Λ.
;;
;; Implements a hierarchical leader key system.
;; The 'c' key serves as the global leader prefix in COMMAND mode.
;;
;; The Space (SPC) key is reserved for set-mark-command (C-SPC) in Iota
;; to maintain Emacs semantics.
;;
;; C-c Prefix Delegation:
;;   All leader keys (except 'i' for iota) delegate to your C-c bindings.
;;   This ensures full compatibility with your init.el configuration.
;;
;;   c <key>  →  C-c <key>
;;
;;   For example, if you have:
;;     (general-define-key :prefix "C-c v" "v" 'magit-status)
;;   Then in COMMAND mode:
;;     c v v  →  magit-status
;;
;; Iota-specific commands:
;;   c i  - Iota commands (demo, setup, config, etc.)
;;   c c  - Full C-c simulation (for any C-c command)
;;
;; Usage:
;;   (require 'iota-leader)
;;   (iota-leader-mode 1)

;;; Code:

(require 'cl-lib)

;;; Dependencies

;; Initialize package system
(require 'package)

;; Forward declarations
(defvar which-key-idle-delay)
(defvar which-key-popup-type)
(defvar which-key-side-window-location)
(defvar which-key-separator)
(defvar which-key-key-face)
(defvar modalka-mode-map)
(declare-function which-key-add-keymap-based-replacements "which-key")

;; Forward declarations for iota modules
(declare-function iota-demo "iota-demo")
(declare-function iota-setup "iota")
(declare-function iota-config-choose-preset "iota-config")
(declare-function iota-modeline-mode "iota-modeline")
(declare-function iota-window-mode "iota-window")
(declare-function iota-tutorial "iota-tutorial")
(declare-function iota-version "iota")
(declare-function iota-reload "iota")
(declare-function iota-modal-cycle-indicator-style "iota-modal")

;;; Configuration

(defgroup iota-leader nil
  "IOTA leader key configuration."
  :group 'iota
  :prefix "iota-leader-")

(defcustom iota-leader-key "c"
  "Leader key for IOTA commands in COMMAND mode.
'c' is chosen because:
1. SPC is reserved for set-mark-command (C-SPC)
2. 'c' is accessible on home row
3. Mnemonically suggests 'command' or 'control'
4. C-c prefix is available via 'c c' in the leader menu"
  :type 'string
  :group 'iota-leader)

(defcustom iota-leader-major-mode-key "c m"
  "Leader key for major-mode specific commands."
  :type 'string
  :group 'iota-leader)

;;; Leader Keymap Setup

(defvar iota-leader-map (make-sparse-keymap)
  "Keymap for iota leader key bindings.")

(defun iota-leader--setup-keymap ()
  "Set up the leader keymap in modalka-mode-map."
  (when (boundp 'modalka-mode-map)
    (define-key modalka-mode-map (kbd iota-leader-key) iota-leader-map)))

;;; Leader Key Definitions

(defun iota-leader--simulate-C-c-b ()
  "Simulate C-c b prefix."
  (interactive)
  (iota-leader--do-simulate-C-c-prefix "b"))

(defun iota-leader--simulate-C-c-e ()
  "Simulate C-c e prefix."
  (interactive)
  (iota-leader--do-simulate-C-c-prefix "e"))

(defun iota-leader--simulate-C-c-f ()
  "Simulate C-c f prefix."
  (interactive)
  (iota-leader--do-simulate-C-c-prefix "f"))

(defun iota-leader--simulate-C-c-h ()
  "Simulate C-c h prefix."
  (interactive)
  (iota-leader--do-simulate-C-c-prefix "h"))

(defun iota-leader--simulate-C-c-m ()
  "Simulate C-c m prefix."
  (interactive)
  (iota-leader--do-simulate-C-c-prefix "m"))

(defun iota-leader--simulate-C-c-n ()
  "Simulate C-c n prefix."
  (interactive)
  (iota-leader--do-simulate-C-c-prefix "n"))

(defun iota-leader--simulate-C-c-p ()
  "Simulate C-c p prefix."
  (interactive)
  (iota-leader--do-simulate-C-c-prefix "p"))

(defun iota-leader--simulate-C-c-q ()
  "Simulate C-c q prefix."
  (interactive)
  (iota-leader--do-simulate-C-c-prefix "q"))

(defun iota-leader--simulate-C-c-s ()
  "Simulate C-c s prefix."
  (interactive)
  (iota-leader--do-simulate-C-c-prefix "s"))

(defun iota-leader--simulate-C-c-t ()
  "Simulate C-c t prefix."
  (interactive)
  (iota-leader--do-simulate-C-c-prefix "t"))

(defun iota-leader--simulate-C-c-v ()
  "Simulate C-c v prefix."
  (interactive)
  (iota-leader--do-simulate-C-c-prefix "v"))

(defun iota-leader--simulate-C-c-w ()
  "Simulate C-c w prefix."
  (interactive)
  (iota-leader--do-simulate-C-c-prefix "w"))

(defun iota-leader--do-simulate-C-c-prefix (key)
  "Simulate C-c KEY prefix and execute the resulting command."
  (let* ((prefix-key (concat "C-c " key))
         (next-key (read-event (concat "C-c " key "-")))
         (keys (vector next-key))
         (full-key (vconcat (kbd prefix-key) keys))
         (cmd (key-binding full-key)))
    ;; If the binding is a keymap, continue reading keys
    (while (keymapp cmd)
      (let ((next (read-event (concat "C-c " key " " (key-description keys) "-"))))
        (setq keys (vconcat keys (vector next)))
        (setq full-key (vconcat (kbd prefix-key) keys))
        (setq cmd (key-binding full-key))))
    (if cmd
        (if (commandp cmd)
            (progn
              ;; Set this-command so keycast shows the actual command
              (setq this-command cmd)
              (call-interactively cmd))
          (error "C-c %s %s is not a command" key (key-description keys)))
      (message "C-c %s %s is undefined" key (key-description keys)))))

(defun iota-leader--define-keys ()
  "Define all leader key bindings.

The leader key delegates to C-c prefixes for compatibility with user
configurations. Only Iota-specific commands are defined directly."

  ;; Create sub-keymaps for iota-specific categories
  (let ((iota-map (make-sparse-keymap)))

    ;; === C-c Prefix Delegations ===
    ;; These delegate to the user's C-c bindings for full compatibility
    (define-key iota-leader-map (kbd "b") #'iota-leader--simulate-C-c-b)
    (define-key iota-leader-map (kbd "e") #'iota-leader--simulate-C-c-e)
    (define-key iota-leader-map (kbd "f") #'iota-leader--simulate-C-c-f)
    (define-key iota-leader-map (kbd "h") #'iota-leader--simulate-C-c-h)
    (define-key iota-leader-map (kbd "m") #'iota-leader--simulate-C-c-m)
    (define-key iota-leader-map (kbd "n") #'iota-leader--simulate-C-c-n)
    (define-key iota-leader-map (kbd "p") #'iota-leader--simulate-C-c-p)
    (define-key iota-leader-map (kbd "q") #'iota-leader--simulate-C-c-q)
    (define-key iota-leader-map (kbd "s") #'iota-leader--simulate-C-c-s)
    (define-key iota-leader-map (kbd "t") #'iota-leader--simulate-C-c-t)
    (define-key iota-leader-map (kbd "v") #'iota-leader--simulate-C-c-v)
    (define-key iota-leader-map (kbd "w") #'iota-leader--simulate-C-c-w)

    ;; === Iota (c i) - Only Iota-specific commands ===
    (when (fboundp 'iota-demo)
      (define-key iota-map (kbd "d") #'iota-demo))
    (when (fboundp 'iota-setup)
      (define-key iota-map (kbd "s") #'iota-setup))
    (when (fboundp 'iota-config-choose-preset)
      (define-key iota-map (kbd "c") #'iota-config-choose-preset))
    (when (fboundp 'iota-modeline-mode)
      (define-key iota-map (kbd "m") #'iota-modeline-mode))
    (when (fboundp 'iota-window-mode)
      (define-key iota-map (kbd "w") #'iota-window-mode))
    (when (fboundp 'iota-tutorial)
      (define-key iota-map (kbd "t") #'iota-tutorial))
    (when (fboundp 'iota-version)
      (define-key iota-map (kbd "v") #'iota-version))
    (when (fboundp 'iota-reload)
      (define-key iota-map (kbd "r") #'iota-reload))
    (define-key iota-leader-map (kbd "i") iota-map)

    ;; === C-c Prefix (c c) - Full C-c simulation ===
    (when (fboundp 'iota-modal--simulate-C-c)
      (define-key iota-leader-map (kbd "c") #'iota-modal--simulate-C-c))

    ;; Set up which-key descriptions if available
    (iota-leader--setup-which-key-descriptions)))

;;; Which-Key Integration

(defun iota-leader--setup-which-key-descriptions ()
  "Set up which-key descriptions for leader key bindings."
  (when (featurep 'which-key)
    ;; Add descriptions for the leader key categories
    (which-key-add-keymap-based-replacements iota-leader-map
      ;; Iota-specific
      "i" "iota"
      "c" "C-c prefix"
      ;; C-c prefix delegations (a-z)
      "b" "C-c b"
      "e" "C-c e"
      "f" "C-c f"
      "h" "C-c h"
      "m" "C-c m"
      "n" "C-c n"
      "p" "C-c p"
      "q" "C-c q"
      "s" "C-c s"
      "t" "C-c t"
      "v" "C-c v"
      "w" "C-c w")))

(defun iota-leader--setup-which-key ()
  "Configure which-key for better leader key display."
  (when (featurep 'which-key)
    ;; Shorten the delay for leader keys
    (setq which-key-idle-delay 0.3)
    ;; Use a popup at the bottom
    (setq which-key-popup-type 'side-window)
    (setq which-key-side-window-location 'bottom)
    ;; Separator between key and command
    (setq which-key-separator " → ")
    ;; Add some color
    (setq which-key-key-face 'font-lock-keyword-face)))

;;; Minor Mode

;;;###autoload
(define-minor-mode iota-leader-mode
  "Enable Iota leader key framework.

Provides a hierarchical menu of commands accessible via the 'c' key
in COMMAND mode (when modalka is active).

Leader Key: c

C-c Prefix Delegation:
  All keys delegate to your C-c bindings:
  c <key>  →  C-c <key>

  Examples:
  c v v  - C-c v v (magit-status if configured)
  c p f  - C-c p f (project-find-file)
  c e m  - C-c e m (your edit commands)
  c n n  - C-c n n (denote)
  c t l  - C-c t l (load-theme)

Iota-specific:
  c i  - Iota commands (demo, setup, config, modeline, etc.)
  c c  - Full C-c simulation (any C-c command)

Press the leader key and wait for which-key popup (if installed)."
  :global t
  :group 'iota-leader
  :lighter nil
  (if iota-leader-mode
      (progn
        ;; Set up the leader keymap in modalka-mode-map
        (iota-leader--setup-keymap)
        ;; Define all key bindings
        (iota-leader--define-keys)
        ;; Configure which-key
        (iota-leader--setup-which-key))))

;;; Utility Commands

;;;###autoload
(defun iota-leader-show-menu ()
  "Show the leader key menu manually."
  (interactive)
  (if (featurep 'which-key)
      (which-key-show-keymap 'modalka-mode-map)
    (message "Install which-key for interactive menu: M-x package-install RET which-key RET")))

(provide 'iota-leader)
;;; iota-leader.el ends here
