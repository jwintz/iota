;;; iota-modes-info.el --- Info mode enhancements for IOTA -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: help, info
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Info mode enhancements:
;; - Text reflow commands for better readability
;; - Justification support

;;; Code:

(require 'cl-lib)

;;; Configuration

(defgroup iota-modes-info nil
  "IOTA Info mode enhancements."
  :group 'iota-modes
  :prefix "iota-modes-info-")

;;; Text Reflow Commands

;;;###autoload
(defun iota-modes-info-unfill-paragraph ()
  "Join a multi-line paragraph into a single line.
Useful for preparing text for re-flowing."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))

;;;###autoload
(defun iota-modes-info-reflow-paragraph ()
  "Reflow the current paragraph to window width.
Note: This modifies buffer content. Use `g' to revert the Info node."
  (interactive)
  (when (derived-mode-p 'Info-mode)
    (let ((inhibit-read-only t)
          (fill-column (- (window-body-width) 2)))
      (save-excursion
        ;; First unfill
        (let ((fill-column most-positive-fixnum))
          (fill-paragraph nil))
        ;; Then refill to window width
        (fill-paragraph nil))
      (message "Paragraph reflowed to %d columns" fill-column))))

;;;###autoload
(defun iota-modes-info-reflow-node ()
  "Reflow the current Info node to window width.
Skips lines that look like list items or menu entries to preserve formatting.

Note: This modifies buffer content. Use `g' to revert the Info node."
  (interactive)
  (when (derived-mode-p 'Info-mode)
    (let ((inhibit-read-only t)
          (fill-column (- (window-body-width) 2))
          (count 0)
          (max-iterations 10000)  ; Safety bound
          (iterations 0))
      (save-excursion
        (goto-char (point-min))
        ;; Skip header lines (usually first few lines with navigation)
        (forward-line 3)
        (while (and (not (eobp))
                    (< iterations max-iterations))
          (cl-incf iterations)
          (let ((line-start (point)))
            ;; Skip lines that look like:
            ;; - Empty lines
            ;; - List items (starting with * or -)
            ;; - Menu entries (starting with *)
            ;; - Indented code/examples
            (cond
             ((looking-at "^\\s-*$")           ; Empty line
              (forward-line 1))
             ((looking-at "^\\s-*[*-]\\s-")    ; List item
              (forward-line 1))
             ((looking-at "^\\*")              ; Menu entry
              (forward-line 1))
             ((looking-at "^   ")              ; Indented (code)
              (forward-line 1))
             (t
              ;; Reflow this paragraph
              (let ((para-start (point)))
                (forward-paragraph)
                (let ((para-end (point)))
                  (when (> para-end para-start)
                    (save-restriction
                      (narrow-to-region para-start para-end)
                      (goto-char (point-min))
                      ;; Unfill then refill
                      (let ((fill-column most-positive-fixnum))
                        (fill-region (point-min) (point-max)))
                      (fill-region (point-min) (point-max)))
                    (cl-incf count))
                  ;; Ensure we advance
                  (goto-char para-end)))))
            ;; Ensure we always advance at least one line if nothing happened
            (when (= (point) line-start)
              (forward-line 1)))))
      (message "Reflowed %d paragraphs to %d columns" count fill-column))))

;;; Public API

(defun iota-modes-info-setup-buffer ()
  "Apply IOTA enhancements to the current Info buffer.
Currently a no-op as Info mode uses standard modeline."
  nil)

(defun iota-modes-info-cleanup-buffer ()
  "Remove IOTA enhancements from the current Info buffer."
  nil)

(defun iota-modes-info-enable ()
  "Enable Info mode enhancements."
  (add-hook 'Info-mode-hook #'iota-modes-info-setup-buffer))

(defun iota-modes-info-disable ()
  "Disable Info mode enhancements."
  (remove-hook 'Info-mode-hook #'iota-modes-info-setup-buffer))

(provide 'iota-modes-info)
;;; iota-modes-info.el ends here
