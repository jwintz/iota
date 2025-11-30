;;; iota-leader.el --- Leader key framework for IOTA -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: convenience, modal
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (general "0.1"))

;;; Commentary:

;; ι • ο • τ • α
;; Leader Key Framework for IOTA
;;
;; This file is part of I O T Λ.
;;
;; Implements a hierarchical leader key system using general.el.
;; The comma (,) key serves as the global leader prefix in COMMAND mode.
;;
;; The Space (SPC) key is reserved for set-mark-command (C-SPC) in Iota
;; to maintain Emacs semantics, so comma is used instead.
;;
;; Leader Hierarchy (Section 5.3):
;;   , f  - Files (find-file, save, recentf)
;;   , b  - Buffers (switch, kill, list)
;;   , w  - Windows (split, delete, maximize)
;;   , p  - Projects (project-find-file, switch)
;;   , g  - Git (magit-status, log)
;;   , h  - Help (describe-function/variable/key)
;;   , q  - Quit (save-buffers-kill, restart)
;;
;; Usage:
;;   (require 'iota-leader)
;;   (iota-leader-mode 1)

;;; Code:

(require 'cl-lib)

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

;; Refresh package contents if general not available
(unless (package-installed-p 'general)
  (package-refresh-contents))

;; Use use-package for dependency management
(require 'use-package)

(use-package general
  :ensure t
  :demand t)

;; Forward declarations for general.el
(declare-function general-create-definer "general")
(defvar which-key-idle-delay)
(defvar which-key-popup-type)
(defvar which-key-side-window-location)
(defvar which-key-separator)
(defvar which-key-key-face)

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

(defcustom iota-leader-key ","
  "Leader key for IOTA commands in COMMAND mode.
Comma is chosen because:
1. SPC is reserved for set-mark-command (C-SPC)
2. Comma is unshifted and accessible on home row
3. Rarely used as a prefix in standard Emacs"
  :type 'string
  :group 'iota-leader)

(defcustom iota-leader-major-mode-key ", m"
  "Leader key for major-mode specific commands."
  :type 'string
  :group 'iota-leader)

;;; General.el Setup

;; The definer will be set up at runtime
(defvar iota-leader-def nil
  "The general.el definer for iota leader keys.")

(defvar iota-local-leader-def nil
  "The general.el definer for iota local leader keys.")

(defun iota-leader--setup-general ()
  "Set up general.el with iota leader configuration."
  (when (featurep 'general)
    ;; Create the main leader definer
    (general-create-definer iota-leader-def
      :prefix iota-leader-key
      :keymaps 'modalka-mode-map)

    ;; Create local leader definer for major-mode bindings
    (general-create-definer iota-local-leader-def
      :prefix iota-leader-major-mode-key
      :keymaps 'modalka-mode-map)))

;;; Leader Key Definitions

(defun iota-leader--define-keys ()
  "Define all leader key bindings.
Implements the hierarchy from Section 5.3 of the architecture document."

  (when (and (featurep 'general) (fboundp 'iota-leader-def))

    ;; === Top Level ===
    (iota-leader-def
      ""  '(nil :which-key "Iota Leader")

      ;; === Files (, f) ===
      "f"  '(:ignore t :which-key "files")
      "ff" '(find-file :which-key "find file")
      "fr" '(recentf-open-files :which-key "recent files")
      "fs" '(save-buffer :which-key "save")
      "fS" '(write-file :which-key "save as")
      "fD" '(delete-file :which-key "delete")
      "fR" '(rename-file :which-key "rename")
      "fc" '(copy-file :which-key "copy")

      ;; === Buffers (, b) ===
      "b"  '(:ignore t :which-key "buffers")
      "bb" '(switch-to-buffer :which-key "switch buffer")
      "bk" '(kill-buffer :which-key "kill buffer")
      "bK" '(kill-buffer-and-window :which-key "kill buffer & window")
      "bl" '(list-buffers :which-key "list buffers")
      "bi" '(ibuffer :which-key "ibuffer")
      "bn" '(next-buffer :which-key "next buffer")
      "bp" '(previous-buffer :which-key "previous buffer")
      "br" '(revert-buffer :which-key "revert buffer")
      "bs" '(save-some-buffers :which-key "save all")

      ;; === Windows (, w) ===
      "w"  '(:ignore t :which-key "windows")
      "w/" '(split-window-right :which-key "split right")
      "w-" '(split-window-below :which-key "split below")
      "wd" '(delete-window :which-key "delete window")
      "wD" '(delete-other-windows :which-key "delete others")
      "wm" '(delete-other-windows :which-key "maximize")
      "wh" '(windmove-left :which-key "move left")
      "wl" '(windmove-right :which-key "move right")
      "wk" '(windmove-up :which-key "move up")
      "wj" '(windmove-down :which-key "move down")
      "w=" '(balance-windows :which-key "balance")
      "wo" '(other-window :which-key "other window")

      ;; === Projects (, p) ===
      "p"  '(:ignore t :which-key "projects")
      "pf" '(project-find-file :which-key "find file")
      "pp" '(project-switch-project :which-key "switch project")
      "pb" '(project-switch-to-buffer :which-key "project buffer")
      "pd" '(project-dired :which-key "project dired")
      "ps" '(project-shell :which-key "project shell")
      "pk" '(project-kill-buffers :which-key "kill project buffers")

      ;; === Magit (, v) ===
      "v"  '(:ignore t :which-key "magit")
      "vv" '(magit-status :which-key "status")
      "vl" '(magit-log-current :which-key "log")
      "vb" '(magit-blame :which-key "blame")
      "vd" '(magit-diff :which-key "diff")
      "vf" '(magit-log-buffer-file :which-key "file log")
      "vs" '(magit-stage-file :which-key "stage file")
      "vc" '(magit-commit :which-key "commit")
      "vp" '(magit-push :which-key "push")
      "vP" '(magit-pull :which-key "pull")
      "vF" '(magit-fetch :which-key "fetch")
      "vB" '(magit-branch :which-key "branch")

      ;; === Help (, h) ===
      "h"  '(:ignore t :which-key "help")
      "hf" '(describe-function :which-key "describe function")
      "hv" '(describe-variable :which-key "describe variable")
      "hk" '(describe-key :which-key "describe key")
      "hm" '(describe-mode :which-key "describe mode")
      "hb" '(describe-bindings :which-key "describe bindings")
      "hi" '(info :which-key "info")
      "ha" '(apropos :which-key "apropos")
      "hw" '(where-is :which-key "where is")
      "hc" '(describe-char :which-key "describe char")
      "ht" '(iota-tutorial :which-key "iota tutorial")

      ;; === Quit (, q) ===
      "q"  '(:ignore t :which-key "quit")
      "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
      "qQ" '(kill-emacs :which-key "kill emacs (no save)")
      "qr" '(restart-emacs :which-key "restart emacs")

      ;; === Search (, s) ===
      "s"  '(:ignore t :which-key "search")
      "ss" '(isearch-forward :which-key "search forward")
      "sr" '(isearch-backward :which-key "search backward")
      "sR" '(query-replace :which-key "replace")
      "sg" '(grep :which-key "grep")
      "so" '(occur :which-key "occur")

      ;; === Toggle (, t) ===
      "t"  '(:ignore t :which-key "toggle")
      "tl" '(display-line-numbers-mode :which-key "line numbers")
      "tw" '(whitespace-mode :which-key "whitespace")
      "tf" '(auto-fill-mode :which-key "auto-fill")
      "tv" '(visual-line-mode :which-key "visual line")
      "ti" '(iota-modal-cycle-indicator-style :which-key "modal indicator")

      ;; === Iota (, i) ===
      "i"  '(:ignore t :which-key "iota")
      "id" '(iota-demo :which-key "demo")
      "is" '(iota-setup :which-key "setup wizard")
      "ic" '(iota-config-choose-preset :which-key "choose preset")
      "im" '(iota-modeline-mode :which-key "toggle modeline")
      "iw" '(iota-window-mode :which-key "toggle window mode")
      "it" '(iota-tutorial :which-key "tutorial")
      "iv" '(iota-version :which-key "version")
      "ir" '(iota-reload :which-key "reload")

      ;; === Major Mode (, m) ===
      "m"  '(:ignore t :which-key "major mode"))))

;;; Magit Prefix Map (C-c v)

(defvar iota-vc-map (make-sparse-keymap)
  "Keymap for Magit commands, bound to C-c v.")

(defun iota-leader--setup-vc-map ()
  "Set up the C-c v prefix map for Magit commands.

This mirrors the structure of project.el's C-c p prefix,
providing consistent access to version control commands."
  ;; Define Magit commands in the prefix map
  (define-key iota-vc-map (kbd "v") #'magit-status)        ; status
  (define-key iota-vc-map (kbd "l") #'magit-log-current)   ; log
  (define-key iota-vc-map (kbd "L") #'magit-log-all)       ; log all
  (define-key iota-vc-map (kbd "b") #'magit-blame)         ; blame
  (define-key iota-vc-map (kbd "d") #'magit-diff)          ; diff
  (define-key iota-vc-map (kbd "D") #'magit-diff-buffer-file) ; diff file
  (define-key iota-vc-map (kbd "s") #'magit-stage-file)    ; stage
  (define-key iota-vc-map (kbd "c") #'magit-commit)        ; commit
  (define-key iota-vc-map (kbd "p") #'magit-push)          ; push
  (define-key iota-vc-map (kbd "P") #'magit-pull)          ; pull
  (define-key iota-vc-map (kbd "f") #'magit-fetch)         ; fetch
  (define-key iota-vc-map (kbd "B") #'magit-branch)        ; branch
  (define-key iota-vc-map (kbd "r") #'magit-rebase)        ; rebase
  (define-key iota-vc-map (kbd "z") #'magit-stash)         ; stash
  ;; Bind the map to C-c v
  (global-set-key (kbd "C-c v") iota-vc-map))

;;; Which-Key Integration

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

Provides a hierarchical menu of commands accessible via the comma key
in COMMAND mode (when modalka is active).

Leader Key: , (comma)

Categories:
  , f  - Files (find, save, delete, rename)
  , b  - Buffers (switch, kill, list, revert)
  , w  - Windows (split, delete, move, balance)
  , p  - Projects (find-file, switch, buffer)
  , v  - Magit (status, log, blame, diff, commit)
  , h  - Help (describe function/variable/key)
  , s  - Search (forward, backward, replace, grep)
  , t  - Toggle (line numbers, whitespace, etc.)
  , i  - Iota (demo, setup, config)
  , q  - Quit (save & quit, restart)
  , m  - Major mode specific commands

Also sets up C-c v as a prefix for Magit commands (similar to C-c p for projects).

Press the leader key and wait for which-key popup (if installed)."
  :global t
  :group 'iota-leader
  :lighter nil
  (if iota-leader-mode
      (progn
        ;; Set up general.el
        (iota-leader--setup-general)
        ;; Define all key bindings
        (iota-leader--define-keys)
        ;; Set up C-c v prefix for VC
        (iota-leader--setup-vc-map)
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
