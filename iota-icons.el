;;; iota-icons.el --- Icon support with fallback for I O T Λ -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: icons, faces
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Icon support with automatic fallback when nerd-icons is unavailable.
;; Provides a consistent API for icons throughout IOTA components.
;;
;; Key features:
;; - Automatic detection of nerd-icons availability
;; - Graceful fallback to Unicode or ASCII alternatives
;; - Predefined icon sets for common UI elements
;; - Support for all nerd-icons families (codicon, devicon, faicon, etc.)
;;
;; Usage:
;;   (iota-icon 'codicon "nf-cod-file" "📄")      ; Specific icon
;;   (iota-icon-get 'file)                        ; Predefined icon
;;   (iota-icon-buffer-type)                      ; Context-aware buffer icon

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup iota-icons nil
  "I O T Λ icon configuration."
  :group 'iota
  :prefix "iota-icons-")

(defcustom iota-icons-prefer-nerd t
  "When non-nil, prefer nerd-icons over fallbacks when available."
  :type 'boolean
  :group 'iota-icons)

(defcustom iota-icons-fallback-style 'unicode
  "Fallback style when nerd-icons is unavailable.
Options: `unicode' for Unicode symbols, `ascii' for ASCII only."
  :type '(choice (const :tag "Unicode symbols" unicode)
                 (const :tag "ASCII only" ascii))
  :group 'iota-icons)

;;; State

(defvar iota-icons--available-p nil
  "Cached result of nerd-icons availability check.")

(defvar iota-icons--checked-p nil
  "Whether we've checked for nerd-icons availability.")

;;; Availability Detection

(defun iota-icons-available-p ()
  "Return t if nerd-icons is available and working."
  (unless iota-icons--checked-p
    (setq iota-icons--available-p
          (and iota-icons-prefer-nerd
               (featurep 'nerd-icons)
               ;; Verify the font is actually installed
               (condition-case nil
                   (and (fboundp 'nerd-icons-codicon)
                        (stringp (nerd-icons-codicon "nf-cod-file")))
                 (error nil))))
    (setq iota-icons--checked-p t))
  iota-icons--available-p)

(defun iota-icons-refresh ()
  "Refresh nerd-icons availability check.
Call this after installing nerd-icons or its fonts."
  (interactive)
  (setq iota-icons--checked-p nil)
  (iota-icons-available-p)
  (message "I O T Λ Icons: nerd-icons %s"
           (if iota-icons--available-p "available" "not available")))

;;; Core Icon Functions

(defun iota-icon (family icon-name fallback &optional face)
  "Get icon from nerd-icons FAMILY with ICON-NAME, or FALLBACK if unavailable.

FAMILY is a symbol corresponding to a nerd-icons function:
  - `codicon'   → nerd-icons-codicon (VS Code icons)
  - `devicon'   → nerd-icons-devicon (Developer icons)
  - `faicon'    → nerd-icons-faicon (Font Awesome)
  - `flicon'    → nerd-icons-flicon (File icons)
  - `mdicon'    → nerd-icons-mdicon (Material Design)
  - `octicon'   → nerd-icons-octicon (GitHub Octicons)
  - `pomicon'   → nerd-icons-pomicon (Pomodoro)
  - `powerline' → nerd-icons-powerline
  - `sucicon'   → nerd-icons-sucicon (Seti UI)
  - `wicon'     → nerd-icons-wicon (Weather)

ICON-NAME is the nerd-fonts icon identifier (e.g., \"nf-cod-file\").
FALLBACK is the string to use if nerd-icons is unavailable.
FACE is an optional face to apply to the result.

Example:
  (iota-icon \\='codicon \"nf-cod-arrow_up\" \"↑\")"
  (let ((result
         (if (iota-icons-available-p)
             (let ((fn (intern (format "nerd-icons-%s" family))))
               (if (fboundp fn)
                   (condition-case nil
                       (funcall fn icon-name)
                     (error fallback))
                 fallback))
           fallback)))
    (if face
        (propertize result 'face face)
      result)))

(defun iota-icon-with-text (family icon-name fallback text &optional separator face)
  "Get icon with TEXT appended.

FAMILY, ICON-NAME, and FALLBACK are as in `iota-icon'.
TEXT is the label to append after the icon.
SEPARATOR is the string between icon and text (default: single space).
FACE is an optional face to apply to the icon (text keeps its own face).

Example:
  (iota-icon-with-text \\='codicon \"nf-cod-arrow_up\" \"↑\" \"Taller\")"
  (let ((icon (iota-icon family icon-name fallback face))
        (sep (or separator " ")))
    (concat icon sep text)))

;;; Predefined Icon Registry

(defconst iota-icons-registry
  '(;; File/Buffer icons
    (file          . (codicon "nf-cod-file"          "📄" "F"))
    (file-code     . (codicon "nf-cod-file_code"     "📝" "C"))
    (file-text     . (codicon "nf-cod-file_text"     "📄" "T"))
    (folder        . (codicon "nf-cod-folder"        "📁" "D"))
    (folder-open   . (codicon "nf-cod-folder_opened" "📂" "D"))
    
    ;; Buffer state icons
    (modified      . (codicon "nf-cod-circle_filled" "●"  "*"))
    (saved         . (codicon "nf-cod-check"         "✓"  "-"))
    (readonly      . (codicon "nf-cod-lock"          "🔒" "R"))
    (unsaved       . (codicon "nf-cod-primitive_dot" "○"  "o"))
    
    ;; Arrows/Navigation
    (arrow-up      . (codicon "nf-cod-arrow_up"      "↑"  "^"))
    (arrow-down    . (codicon "nf-cod-arrow_down"    "↓"  "v"))
    (arrow-left    . (codicon "nf-cod-arrow_left"    "←"  "<"))
    (arrow-right   . (codicon "nf-cod-arrow_right"   "→"  ">"))
    (chevron-up    . (codicon "nf-cod-chevron_up"    "⌃"  "^"))
    (chevron-down  . (codicon "nf-cod-chevron_down"  "⌄"  "v"))
    (chevron-left  . (codicon "nf-cod-chevron_left"  "‹"  "<"))
    (chevron-right . (codicon "nf-cod-chevron_right" "›"  ">"))
    
    ;; Window/Layout
    (window        . (codicon "nf-cod-window"        "⧉"  "W"))
    (split-h       . (codicon "nf-cod-split_horizontal" "⬓" "H"))
    (split-v       . (codicon "nf-cod-split_vertical"   "⬒" "V"))
    (maximize      . (codicon "nf-cod-screen_full"   "⛶"  "M"))
    (minimize      . (codicon "nf-cod-screen_normal" "⊡"  "m"))
    (balance       . (mdicon  "nf-md-view_grid_outline" "⊞" "="))
    (close         . (codicon "nf-cod-close"         "✕"  "x"))
    
    ;; UI Elements
    (menu          . (codicon "nf-cod-menu"          "☰"  "="))
    (settings      . (codicon "nf-cod-settings_gear" "⚙"  "@"))
    (search        . (codicon "nf-cod-search"        "🔍" "/"))
    (info          . (codicon "nf-cod-info"          "ℹ"  "i"))
    (warning       . (codicon "nf-cod-warning"       "⚠"  "!"))
    (error         . (codicon "nf-cod-error"         "✖"  "E"))
    (success       . (codicon "nf-cod-pass_filled"   "✔"  "S"))
    (question      . (codicon "nf-cod-question"      "?"  "?"))
    
    ;; Version Control
    (git           . (devicon "nf-dev-git"           "⎇"  "G"))
    (git-branch    . (codicon "nf-cod-git_branch"    "⎇"  "B"))
    (git-commit    . (codicon "nf-cod-git_commit"    "◉"  "C"))
    (git-compare   . (codicon "nf-cod-git_compare"   "◊"  "D"))
    (git-merge     . (codicon "nf-cod-git_merge"     "⌥"  "M"))
    
    ;; Mode indicators
    (emacs         . (sucicon "nf-seti-elisp"        "ε"  "E"))
    (terminal      . (codicon "nf-cod-terminal"      "⌨"  "T"))
    (debug         . (codicon "nf-cod-debug"         "🐛" "D"))
    (test          . (codicon "nf-cod-beaker"        "⚗"  "T"))
    
    ;; Misc
    (clock         . (codicon "nf-cod-clock"         "⏱"  "T"))
    (calendar      . (codicon "nf-cod-calendar"      "📅" "C"))
    (bookmark      . (codicon "nf-cod-bookmark"      "🔖" "B"))
    (star          . (codicon "nf-cod-star_full"     "★"  "*"))
    (heart         . (codicon "nf-cod-heart"         "♥"  "<3"))
    (refresh       . (codicon "nf-cod-refresh"       "↻"  "R"))
    (sync          . (codicon "nf-cod-sync"          "⟳"  "S"))
    (pin           . (codicon "nf-cod-pin"           "📌" "P"))
    (link          . (codicon "nf-cod-link"          "🔗" "L"))
    (unlink        . (codicon "nf-cod-link_external" "↗"  "^"))
    (key           . (codicon "nf-cod-key"           "🔑" "K"))
    (keyboard      . (codicon "nf-cod-keyboard"      "⌨"  "K"))
    (play          . (codicon "nf-cod-play"          "▶"  ">"))
    (pause         . (codicon "nf-cod-debug_pause"   "⏸"  "="))
    (stop          . (codicon "nf-cod-debug_stop"    "■"  "S"))
    (record        . (codicon "nf-cod-record"        "⏺"  "O")))
  "Registry of predefined icons.
Each entry is (KEY . (FAMILY ICON-NAME UNICODE-FALLBACK ASCII-FALLBACK)).")

(defun iota-icon-get (key &optional face)
  "Get predefined icon by KEY.

KEY is a symbol from `iota-icons-registry'.
FACE is an optional face to apply.

Example:
  (iota-icon-get \\='modified)
  (iota-icon-get \\='arrow-up \\='bold)"
  (if-let* ((entry (alist-get key iota-icons-registry)))
      (let* ((family (nth 0 entry))
             (icon-name (nth 1 entry))
             (unicode-fallback (nth 2 entry))
             (ascii-fallback (nth 3 entry))
             (fallback (if (eq iota-icons-fallback-style 'ascii)
                           ascii-fallback
                         unicode-fallback)))
        (iota-icon family icon-name fallback face))
    ;; Unknown key - return empty or placeholder
    (if (eq iota-icons-fallback-style 'ascii)
        "?"
      "□")))

(defun iota-icon-get-with-text (key text &optional separator face)
  "Get predefined icon by KEY with TEXT appended.

KEY is a symbol from `iota-icons-registry'.
TEXT is the label to append.
SEPARATOR is the string between icon and text (default: single space).
FACE is an optional face to apply to the icon.

Example:
  (iota-icon-get-with-text \\='arrow-up \"Taller\")"
  (concat (iota-icon-get key face)
          (or separator " ")
          text))

;;; Context-Aware Icons

(defun iota-icon-buffer-state (&optional buffer)
  "Get icon representing BUFFER's modification state.
Returns icon for modified, readonly, or saved state."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     (buffer-read-only (iota-icon-get 'readonly))
     ((buffer-modified-p) (iota-icon-get 'modified))
     (t (iota-icon-get 'saved)))))

(defun iota-icon-for-mode (&optional mode)
  "Get icon appropriate for major MODE.
MODE defaults to `major-mode'."
  (let ((m (or mode major-mode)))
    (cond
     ;; Emacs Lisp
     ((memq m '(emacs-lisp-mode lisp-interaction-mode))
      (iota-icon 'sucicon "nf-seti-elisp" "λ"))
     ;; Common Lisp / Scheme
     ((memq m '(lisp-mode scheme-mode))
      (iota-icon 'devicon "nf-dev-scheme" "λ"))
     ;; Python
     ((eq m 'python-mode)
      (iota-icon 'devicon "nf-dev-python" "🐍"))
     ;; JavaScript/TypeScript
     ((memq m '(js-mode js2-mode javascript-mode))
      (iota-icon 'devicon "nf-dev-javascript" "JS"))
     ((memq m '(typescript-mode tsx-mode))
      (iota-icon 'devicon "nf-dev-typescript" "TS"))
     ;; Web
     ((memq m '(html-mode web-mode))
      (iota-icon 'devicon "nf-dev-html5" "🌐"))
     ((eq m 'css-mode)
      (iota-icon 'devicon "nf-dev-css3" "🎨"))
     ;; Shell
     ((memq m '(sh-mode shell-mode bash-mode))
      (iota-icon 'codicon "nf-cod-terminal_bash" "💲"))
     ;; Markdown/Org
     ((memq m '(markdown-mode gfm-mode))
      (iota-icon 'devicon "nf-dev-markdown" "📝"))
     ((eq m 'org-mode)
      (iota-icon 'sucicon "nf-seti-org" "📋"))
     ;; C/C++
     ((eq m 'c-mode)
      (iota-icon 'devicon "nf-dev-c" "C"))
     ((eq m 'c++-mode)
      (iota-icon 'devicon "nf-dev-cplusplus" "C++"))
     ;; Rust
     ((eq m 'rust-mode)
      (iota-icon 'devicon "nf-dev-rust" "🦀"))
     ;; Go
     ((eq m 'go-mode)
      (iota-icon 'devicon "nf-dev-go" "Go"))
     ;; Ruby
     ((eq m 'ruby-mode)
      (iota-icon 'devicon "nf-dev-ruby" "💎"))
     ;; Java
     ((eq m 'java-mode)
      (iota-icon 'devicon "nf-dev-java" "☕"))
     ;; JSON/YAML
     ((memq m '(json-mode jsonc-mode))
      (iota-icon 'codicon "nf-cod-json" "{}"))
     ((eq m 'yaml-mode)
      (iota-icon 'sucicon "nf-seti-yaml" "📜"))
     ;; Git
     ((memq m '(magit-mode magit-status-mode magit-log-mode))
      (iota-icon 'devicon "nf-dev-git" "⎇"))
     ;; Dired
     ((eq m 'dired-mode)
      (iota-icon 'codicon "nf-cod-folder_opened" "📂"))
     ;; Term/VTerm
     ((memq m '(term-mode vterm-mode eat-mode))
      (iota-icon 'codicon "nf-cod-terminal" "⌨"))
     ;; Help/Info
     ((memq m '(help-mode helpful-mode Info-mode))
      (iota-icon 'codicon "nf-cod-info" "ℹ"))
     ;; Default
     (t (iota-icon-get 'file)))))

;;; Transient Integration Helpers

(defun iota-icon-desc (key text)
  "Create a description function for transient using icon KEY and TEXT.
Returns a lambda suitable for transient :description.

Example in transient-define-prefix:
  (\"k\" (:description (iota-icon-desc \\='arrow-up \"Taller\"))
   enlarge-window :transient t)"
  (lambda () (iota-icon-get-with-text key text)))

;;; Diagnostics

(defun iota-icons-diagnose ()
  "Display diagnostic information about icon support."
  (interactive)
  (with-output-to-temp-buffer "*IOTA Icons Diagnose*"
    (princ "I O T Λ Icons Diagnostics\n")
    (princ "=========================\n\n")
    (princ (format "nerd-icons feature loaded: %s\n"
                   (if (featurep 'nerd-icons) "YES" "NO")))
    (princ (format "nerd-icons available:      %s\n"
                   (if (iota-icons-available-p) "YES" "NO")))
    (princ (format "Fallback style:            %s\n\n"
                   iota-icons-fallback-style))
    
    (princ "Sample Icons:\n")
    (princ "─────────────────────────────────────────\n")
    (dolist (key '(file modified readonly arrow-up arrow-down
                   window balance git emacs terminal))
      (princ (format "  %-15s → %s\n"
                     key (iota-icon-get key))))
    
    (princ "\nMode Icons:\n")
    (princ "─────────────────────────────────────────\n")
    (dolist (mode '(emacs-lisp-mode python-mode org-mode dired-mode))
      (princ (format "  %-20s → %s\n"
                     mode
                     (iota-icon-for-mode mode))))
    
    (when (not (iota-icons-available-p))
      (princ "\n⚠ Note: nerd-icons is not available.\n")
      (princ "  Install with: M-x package-install RET nerd-icons RET\n")
      (princ "  Then run: M-x nerd-icons-install-fonts\n"))))

(provide 'iota-icons)
;;; iota-icons.el ends here
