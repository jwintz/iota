;;; iota-modes-markdown.el --- Markdown mode enhancements for IOTA -*- no-byte-compile: t; lexical-binding: t; -*-

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
;; Markdown mode visual enhancements:
;; - Draw box borders around code blocks
;; - Shows language identifier in the top border
;; - Uses overlays to avoid modifying buffer content
;; - Does NOT interfere with cursor movement or line numbers

;;; Code:

(require 'iota-faces)
(require 'iota-theme)
(require 'rx)

;;; Configuration

(defgroup iota-modes-markdown nil
  "IOTA markdown mode enhancements."
  :group 'iota-modes
  :prefix "iota-modes-markdown-")

(defcustom iota-modes-markdown-box-code-blocks t
  "When non-nil, draw decorative borders around code blocks in markdown-mode."
  :type 'boolean
  :group 'iota-modes-markdown)

;;; State

(defvar-local iota-modes-markdown--overlays nil
  "List of overlays created by iota-modes-markdown in the current buffer.")

(defvar-local iota-modes-markdown--update-timer nil
  "Timer for debounced overlay updates.")

;;; Face Updates

(defun iota-modes-markdown-update-faces ()
  "Update markdown-specific faces based on current theme colors.
Called automatically when the theme changes."
  ;; Ensure backgrounds are unspecified to allow transparency
  (when (facep 'markdown-code-face)
    (set-face-attribute 'markdown-code-face nil 
                        :background 'unspecified 
                        :extend t))
  (when (facep 'markdown-pre-face)
    (set-face-attribute 'markdown-pre-face nil 
                        :background 'unspecified 
                        :extend t))
  (when (facep 'markdown-language-keyword-face)
    (set-face-attribute 'markdown-language-keyword-face nil 
                        :background 'unspecified)))

;;; Overlay Management

(defun iota-modes-markdown--clear-overlays ()
  "Remove all markdown overlays from the current buffer."
  (dolist (ov iota-modes-markdown--overlays)
    (when (overlayp ov)
      (delete-overlay ov)))
  (setq iota-modes-markdown--overlays nil)
  ;; Also remove any orphaned overlays with our property
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'iota-modes-markdown)
      (delete-overlay ov))))

;;; Box Drawing Helpers

(defun iota-modes-markdown--get-box-width ()
  "Get the current box width based on window.
Subtracts 1 to prevent line wrapping."
  (max 10 (1- (if (get-buffer-window)
                  (window-body-width (get-buffer-window))
                (frame-width)))))

;;; Markdown Box Rendering

(defconst iota-modes-markdown--fence-regex
  (rx bol (zero-or-more space)
      (group (or (seq (>= 3 ?`) (zero-or-more any))
                 (seq (>= 3 ?~) (zero-or-more any))))
      eol)
  "Regex to match markdown code fence lines (both opening and closing).")

(defun iota-modes-markdown--extract-language (fence-text)
  "Extract language identifier from FENCE-TEXT.
Returns the language string or nil if not found."
  (when (string-match (rx (or (>= 3 ?`) (>= 3 ?~))
                          (zero-or-more space)
                          (group (one-or-more (any alnum "-_+"))))
                      fence-text)
    (match-string 1 fence-text)))

(defun iota-modes-markdown--make-top-border (width language)
  "Generate top border with WIDTH and optional LANGUAGE identifier.
Simple horizontal line with optional language label."
  (let ((horiz "─"))
    (if language
        ;; Line with language: ── elisp ────────────────────
        (let* ((lang-display (concat horiz horiz " " language " "))
               (lang-len (length lang-display))
               (remaining (max 0 (- width lang-len)))
               (line (make-string remaining (string-to-char horiz))))
          (propertize (concat lang-display line) 'face 'iota-muted-face))
      ;; Simple line: ──────────────────────────────────────
      (propertize (make-string width (string-to-char horiz)) 'face 'iota-muted-face))))

(defun iota-modes-markdown--make-bottom-border (width)
  "Generate bottom border for markdown code block with WIDTH.
Simple horizontal line."
  (propertize (make-string width ?─) 'face 'iota-muted-face))

(defun iota-modes-markdown--find-fence-pairs ()
  "Find all fence pairs in the buffer and return as list of positions.
Each pair is (start-beg start-end end-beg end-end language) representing
the start and end fence lines of a code block, plus the language."
  (let ((pairs nil)
        (stack nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward iota-modes-markdown--fence-regex nil t)
        ;; Use line positions at current point (end of match) for accuracy
        (let* ((line-beg (line-beginning-position))
               (line-end (line-end-position))
               (fence-text (match-string 1))
               ;; Determine fence type (backtick or tilde) and length
               (fence-char (string-to-char fence-text))
               (fence-len (length (replace-regexp-in-string 
                                   (rx (not (any "`~"))) "" fence-text)))
               (language (iota-modes-markdown--extract-language fence-text)))
          ;; Stack element: (fence-char fence-len line-beg line-end language)
          (if (and stack
                   ;; Check if this is a closing fence for the top of stack
                   (eq fence-char (nth 0 (car stack)))
                   (>= fence-len (nth 1 (car stack)))
                   ;; Closing fence should only have the fence chars and whitespace
                   (string-match-p (rx bol (zero-or-more space)
                                       (or (>= 3 ?`) (>= 3 ?~))
                                       (zero-or-more space) eol)
                                   (buffer-substring-no-properties 
                                    line-beg line-end)))
              ;; Found closing fence
              (let ((opening (pop stack)))
                (push (list (nth 2 opening) ; start line start
                            (nth 3 opening) ; start line end
                            line-beg        ; end line start
                            line-end        ; end line end
                            (nth 4 opening)); language
                      pairs))
            ;; Opening fence - push to stack with language
            (push (list fence-char fence-len line-beg line-end language) stack)))))
    (nreverse pairs)))

(defun iota-modes-markdown--apply-fence-overlays (start-beg start-end end-beg end-end language)
  "Apply box overlays for a single code block.
START-BEG/START-END are the opening fence positions.
END-BEG/END-END are the closing fence positions.
LANGUAGE is the optional language identifier."
  (let ((width (iota-modes-markdown--get-box-width)))
    ;; Top border: replace fence line with box border using display property
    (let ((ov (make-overlay start-beg start-end nil t nil)))
      (overlay-put ov 'iota-modes-markdown t)
      (overlay-put ov 'iota-modes-markdown-type 'top-border)
      (overlay-put ov 'iota-modes-markdown-language language)
      (overlay-put ov 'display (iota-modes-markdown--make-top-border width language))
      (overlay-put ov 'evaporate t)
      (push ov iota-modes-markdown--overlays))
    
    ;; Bottom border: replace fence line with box border using display property
    (let ((ov (make-overlay end-beg end-end nil t nil)))
      (overlay-put ov 'iota-modes-markdown t)
      (overlay-put ov 'iota-modes-markdown-type 'bottom-border)
      (overlay-put ov 'display (iota-modes-markdown--make-bottom-border width))
      (overlay-put ov 'evaporate t)
      (push ov iota-modes-markdown--overlays))))

(defun iota-modes-markdown--apply-boxes ()
  "Apply box decorations to all code blocks in the current markdown buffer.
Uses overlays to avoid modifying buffer content."
  (when (and iota-modes-markdown-box-code-blocks
             (derived-mode-p 'markdown-mode))
    (iota-modes-markdown--clear-overlays)
    (let ((pairs (iota-modes-markdown--find-fence-pairs)))
      (dolist (pair pairs)
        (iota-modes-markdown--apply-fence-overlays 
         (nth 0 pair) (nth 1 pair) (nth 2 pair) (nth 3 pair) (nth 4 pair))))))

(defun iota-modes-markdown--update-overlay-widths ()
  "Update width-dependent overlays when window size changes."
  (when iota-modes-markdown--overlays
    (let ((width (iota-modes-markdown--get-box-width)))
      (dolist (ov iota-modes-markdown--overlays)
        (when (overlayp ov)
          (let ((type (overlay-get ov 'iota-modes-markdown-type)))
            (pcase type
              ('top-border
               (let ((lang (overlay-get ov 'iota-modes-markdown-language)))
                 (overlay-put ov 'display
                              (iota-modes-markdown--make-top-border width lang))))
              ('bottom-border
               (overlay-put ov 'display
                            (iota-modes-markdown--make-bottom-border width))))))))))

(defun iota-modes-markdown--schedule-update ()
  "Schedule a debounced overlay update."
  (when iota-modes-markdown--update-timer
    (cancel-timer iota-modes-markdown--update-timer))
  (setq iota-modes-markdown--update-timer
        (run-with-idle-timer
         0.1 nil
         (lambda (buf)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (iota-modes-markdown--apply-boxes)
               (setq iota-modes-markdown--update-timer nil))))
         (current-buffer))))

;;; Buffer Change Handling

(defun iota-modes-markdown--on-change (_beg _end _len)
  "Handle buffer changes."
  (when (derived-mode-p 'markdown-mode)
    (iota-modes-markdown--schedule-update)))

;;; Window Resize Handling

(defun iota-modes-markdown--handle-window-change ()
  "Handle window configuration changes to update overlay widths."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and iota-modes-markdown--overlays
                 (derived-mode-p 'markdown-mode))
        (iota-modes-markdown--update-overlay-widths)))))

;;; Setup with markdown-mode

(with-eval-after-load 'markdown-mode
  (setq markdown-fontify-code-blocks-natively t))

;;; Public API

(defun iota-modes-markdown-setup-buffer ()
  "Apply IOTA enhancements to the current markdown-mode buffer."
  ;; Update faces (this doesn't modify buffer)
  (iota-modes-markdown-update-faces)
  
  ;; Apply decorations
  (iota-modes-markdown--apply-boxes)
  
  ;; Track buffer changes for updates
  (add-hook 'after-change-functions #'iota-modes-markdown--on-change nil t)
  
  ;; Track window resizes
  (add-hook 'window-configuration-change-hook
            #'iota-modes-markdown--handle-window-change nil t))

(defun iota-modes-markdown-cleanup-buffer ()
  "Remove IOTA enhancements from the current markdown-mode buffer."
  (when iota-modes-markdown--update-timer
    (cancel-timer iota-modes-markdown--update-timer)
    (setq iota-modes-markdown--update-timer nil))
  (remove-hook 'after-change-functions #'iota-modes-markdown--on-change t)
  (remove-hook 'window-configuration-change-hook
               #'iota-modes-markdown--handle-window-change t)
  (iota-modes-markdown--clear-overlays))

(defun iota-modes-markdown-enable ()
  "Enable markdown mode enhancements."
  (add-hook 'markdown-mode-hook #'iota-modes-markdown-setup-buffer))

(defun iota-modes-markdown-disable ()
  "Disable markdown mode enhancements."
  (remove-hook 'markdown-mode-hook #'iota-modes-markdown-setup-buffer)
  ;; Clean up existing buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'markdown-mode)
        (iota-modes-markdown-cleanup-buffer))))
  ;; Reset faces
  (when (featurep 'markdown-mode)
    (when (facep 'markdown-code-face)
      (set-face-attribute 'markdown-code-face nil :background 'unspecified :box nil))
    (when (facep 'markdown-pre-face)
      (set-face-attribute 'markdown-pre-face nil :background 'unspecified :box nil))
    (when (facep 'markdown-language-keyword-face)
      (set-face-attribute 'markdown-language-keyword-face nil :background 'unspecified))))

(provide 'iota-modes-markdown)
;;; iota-modes-markdown.el ends here
