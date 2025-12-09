;;; iota-modes.el --- Mode-specific enhancements for IOTA -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: faces, matching
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Mode-specific visual enhancements coordinator.
;; Loads and manages per-major-mode enhancement modules:
;;
;;   - iota-modes-markdown: Code block decorations
;;   - iota-modes-info: Header and text reflow
;;
;; Each module follows the naming convention iota-modes-[major].el

;;; Code:

(require 'iota-modes-markdown)
(require 'iota-modes-info)

;;; Configuration

(defgroup iota-modes nil
  "IOTA major mode enhancements."
  :group 'iota
  :prefix "iota-modes-")

;;; Face Updates (coordinated)

(defun iota-modes-update-faces ()
  "Update mode-specific faces based on current theme colors.
Called automatically when the theme changes."
  (iota-modes-markdown-update-faces))

(add-hook 'iota-theme-change-hook #'iota-modes-update-faces)

;;; Global Mode

;;;###autoload
(define-minor-mode iota-modes-mode
  "Toggle IOTA major mode enhancements.

When enabled, provides visual enhancements to major modes:

Markdown Mode (iota-modes-markdown):
- Draw box borders around code blocks using overlays
- Shows language identifier in the top border

Info Mode (iota-modes-info):
- Header overlay with current node title
- Navigation hints (prev/up/next/quit)
- `iota-modes-info-reflow-node' command to reflow text to window width

General:
- Theme agnostic: Works with any theme
- Transparency support: Respects terminal transparency
- Decorations are purely visual and do not modify buffer content
- Cursor position, line numbers, and navigation remain unaffected."
  :global t
  :group 'iota-modes
  (if iota-modes-mode
      (progn
        ;; Enable all mode enhancements
        (iota-modes-markdown-enable)
        (iota-modes-info-enable)
        (iota-modes-update-faces)
        ;; Apply to existing buffers
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (cond
             ((derived-mode-p 'markdown-mode)
              (iota-modes-markdown-setup-buffer))
             ((derived-mode-p 'Info-mode)
              (iota-modes-info-setup-buffer))))))
    ;; Disable all mode enhancements
    (iota-modes-markdown-disable)
    (iota-modes-info-disable)))

(provide 'iota-modes)
;;; iota-modes.el ends here
