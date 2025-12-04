;;; iota-popup.el --- I O T Λ popup window handling -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: windows, popup, transient
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; IOTA popup provides handling for popup windows (which-key, transient, etc.)
;; that appear at the bottom of the frame above the minibuffer.
;;
;; When such popups appear, they occupy space between the regular windows and
;; the minibuffer area. This module:
;;   - Detects when popup windows are visible
;;   - Applies TUI box decorations (top/bottom lines) to popup windows
;;   - Updates the minibuffer separator line appropriately
;;   - Uses the inactive face style for popup decorations (consistent muted look)
;;
;; Supported popup types:
;;   - which-key popup windows
;;   - transient popup windows
;;   - Embark action windows
;;   - Other bottom-positioned temporary buffers

;;; Code:

(require 'cl-lib)
(require 'iota-box)
(require 'iota-faces)

;;; Customization

(defgroup iota-popup nil
  "I O T Λ popup window handling configuration."
  :group 'iota
  :prefix "iota-popup-")

(defcustom iota-popup-buffer-patterns
  '(;; which-key
    "\\` \\*which-key\\*"
    "\\`\\*which-key\\*"
    ;; transient
    "\\` \\*transient\\*"
    "\\`\\*transient\\*"
    "\\`\\*Transient-Posframe\\*"
    ;; Embark
    "\\`\\*Embark Actions\\*"
    "\\`\\*Embark Collect\\*"
    ;; Consult
    "\\`\\*consult-.*\\*"
    ;; General completions
    "\\`\\*Completions\\*")
  "List of regexp patterns that match popup buffer names.
These buffers will receive TUI box decorations when they appear
at the bottom of the frame."
  :type '(repeat regexp)
  :group 'iota-popup)

(defcustom iota-popup-decoration-style 'bottom-line
  "Style of decoration for popup windows.
- `bottom-line': Draw only a line at the bottom of the popup
- `none': No decorations (separator line from window above is sufficient)"
  :type '(choice (const :tag "Bottom line only" bottom-line)
                 (const :tag "No decorations" none))
  :group 'iota-popup)

(defcustom iota-popup-use-inactive-face t
  "Use inactive face for popup decorations.
When non-nil, popup window decorations use `iota-inactive-box-face',
creating a visual distinction from the active window's modeline."
  :type 'boolean
  :group 'iota-popup)

;;; State

(defvar iota-popup--overlays (make-hash-table :weakness 'key)
  "Hash table mapping popup windows to their decoration overlays.
Keys are windows, values are plists with :top and :bottom overlays.")

(defvar iota-popup--active-popups nil
  "List of currently visible popup windows.")

;;; Detection Functions

(defun iota-popup--buffer-popup-p (buffer)
  "Return non-nil if BUFFER is a popup buffer.
Checks against `iota-popup-buffer-patterns'."
  (when (buffer-live-p buffer)
    (let ((name (buffer-name buffer)))
      (cl-some (lambda (pattern)
                 (string-match-p pattern name))
               iota-popup-buffer-patterns))))

(defun iota-popup--window-popup-p (window)
  "Return non-nil if WINDOW is displaying a popup buffer."
  (and (window-live-p window)
       (iota-popup--buffer-popup-p (window-buffer window))))

(defun iota-popup--get-popup-windows ()
  "Return a list of all visible popup windows."
  (cl-remove-if-not #'iota-popup--window-popup-p
                    (window-list nil 'no-minibuf)))

(defun iota-popup--popup-visible-p ()
  "Return non-nil if any popup window is currently visible."
  (cl-some #'iota-popup--window-popup-p
           (window-list nil 'no-minibuf)))

(defun iota-popup--window-at-bottom-p (window)
  "Return non-nil if WINDOW is at the bottom of the frame.
A window is at bottom if no other non-popup, non-minibuffer window is below it."
  (when (window-live-p window)
    (let* ((edges (window-edges window))
           (bottom (nth 3 edges))
           (left (nth 0 edges))
           (right (nth 2 edges)))
      (catch 'found-below
        (dolist (w (window-list nil 'no-minibuf))
          (unless (or (eq w window)
                      (iota-popup--window-popup-p w))
            (let* ((w-edges (window-edges w))
                   (w-top (nth 1 w-edges))
                   (w-left (nth 0 w-edges))
                   (w-right (nth 2 w-edges)))
              ;; Check if w is below window and overlaps horizontally
              (when (and (= w-top bottom)
                         (> (min right w-right) (max left w-left)))
                (throw 'found-below nil)))))
        t))))

(defun iota-popup--get-bottom-windows ()
  "Return list of regular windows at the bottom of the frame.
Excludes popup windows and minibuffer."
  (cl-remove-if-not
   (lambda (w)
     (and (not (iota-popup--window-popup-p w))
          (iota-popup--window-at-bottom-p w)))
   (window-list nil 'no-minibuf)))

(defun iota-popup--popup-between-windows-and-minibuffer-p ()
  "Return non-nil if a popup window sits between regular windows and minibuffer.
This is the typical case for which-key and transient popups."
  (let ((popups (iota-popup--get-popup-windows)))
    (and popups
         ;; Check if any popup window is positioned below regular bottom windows
         (cl-some (lambda (popup)
                    (let* ((popup-edges (window-edges popup))
                           (popup-top (nth 1 popup-edges)))
                      (cl-some (lambda (bottom-win)
                                 (let* ((bottom-edges (window-edges bottom-win))
                                        (bottom-bottom (nth 3 bottom-edges)))
                                   (= popup-top bottom-bottom)))
                               (iota-popup--get-bottom-windows))))
                  popups))))

;;; Decoration Functions

(defun iota-popup--get-decoration-face ()
  "Return the face to use for popup decorations."
  (if iota-popup-use-inactive-face
      'iota-inactive-box-face
    'iota-active-box-face))

(defun iota-popup--get-box-style ()
  "Return the box style to use for popup decorations."
  (if (boundp 'iota-modeline-box-style)
      iota-modeline-box-style
    'rounded))

(defun iota-popup--make-top-decoration (width)
  "Create top decoration string for popup of WIDTH characters."
  (let ((style (iota-popup--get-box-style))
        (face (iota-popup--get-decoration-face)))
    (iota-box-horizontal-line width style face)))

(defun iota-popup--make-bottom-decoration (width)
  "Create bottom decoration string for popup of WIDTH characters."
  (let ((style (iota-popup--get-box-style))
        (face (iota-popup--get-decoration-face)))
    (iota-box-horizontal-line width style face)))

(defun iota-popup--ensure-top-overlay (window)
  "Ensure WINDOW has a top decoration overlay.
Returns the overlay."
  (let* ((overlays (gethash window iota-popup--overlays))
         (top-ov (plist-get overlays :top)))
    (when (and top-ov (not (overlay-buffer top-ov)))
      (setq top-ov nil))
    (unless top-ov
      (with-current-buffer (window-buffer window)
        (setq top-ov (make-overlay (point-min) (point-min)))
        (overlay-put top-ov 'priority 200)
        (overlay-put top-ov 'window window)
        (puthash window (plist-put overlays :top top-ov) iota-popup--overlays)))
    top-ov))

(defun iota-popup--ensure-bottom-overlay (window)
  "Ensure WINDOW has a bottom decoration overlay.
Returns the overlay."
  (let* ((overlays (gethash window iota-popup--overlays))
         (bottom-ov (plist-get overlays :bottom)))
    (when (and bottom-ov (not (overlay-buffer bottom-ov)))
      (setq bottom-ov nil))
    (unless bottom-ov
      (with-current-buffer (window-buffer window)
        (setq bottom-ov (make-overlay (point-max) (point-max)))
        (overlay-put bottom-ov 'priority 200)
        (overlay-put bottom-ov 'window window)
        (puthash window (plist-put overlays :bottom bottom-ov) iota-popup--overlays)))
    bottom-ov))

(defun iota-popup--update-decorations (window)
  "Update decorations for popup WINDOW.
Only draws bottom line since the window above provides the top separator."
  (when (and (window-live-p window)
             (iota-popup--window-popup-p window))
    (let* ((width (1- (window-body-width window)))
           (style iota-popup-decoration-style))
      (with-current-buffer (window-buffer window)
        ;; Bottom decoration only (top comes from window above's separator)
        (when (eq style 'bottom-line)
          (let ((bottom-ov (iota-popup--ensure-bottom-overlay window))
                (bottom-str (iota-popup--make-bottom-decoration width)))
            (move-overlay bottom-ov (point-max) (point-max))
            (overlay-put bottom-ov 'after-string (concat "\n" bottom-str))))
        ;; Set mode-line-format to nil for popup windows
        (setq-local mode-line-format nil)
        (set-window-parameter window 'mode-line-format 'none)))))

(defun iota-popup--remove-decorations (window)
  "Remove decorations from WINDOW."
  (let ((overlays (gethash window iota-popup--overlays)))
    (when overlays
      (let ((top-ov (plist-get overlays :top))
            (bottom-ov (plist-get overlays :bottom)))
        (when (and top-ov (overlayp top-ov))
          (delete-overlay top-ov))
        (when (and bottom-ov (overlayp bottom-ov))
          (delete-overlay bottom-ov)))
      (remhash window iota-popup--overlays))))

(defun iota-popup--cleanup-stale-overlays ()
  "Remove overlays for windows that no longer exist or are not popups."
  (maphash (lambda (window _overlays)
             (unless (and (window-live-p window)
                          (iota-popup--window-popup-p window))
               (iota-popup--remove-decorations window)))
           iota-popup--overlays))

;;; Update Logic

(defun iota-popup--update-all ()
  "Update decorations for all popup windows."
  ;; Cleanup stale overlays
  (iota-popup--cleanup-stale-overlays)
  ;; Update active popups
  (let ((popups (iota-popup--get-popup-windows)))
    (setq iota-popup--active-popups popups)
    (dolist (popup popups)
      (iota-popup--update-decorations popup))))

(defun iota-popup--on-window-configuration-change ()
  "Handle window configuration changes."
  (when iota-popup-mode
    (iota-popup--update-all)))

;;; Mode Definition

(defvar iota-popup--hooks-installed nil
  "Non-nil when hooks are installed.")

(defun iota-popup--setup ()
  "Set up popup handling."
  (unless iota-popup--hooks-installed
    (add-hook 'window-configuration-change-hook
              #'iota-popup--on-window-configuration-change)
    (setq iota-popup--hooks-installed t))
  ;; Initial update
  (iota-popup--update-all))

(defun iota-popup--teardown ()
  "Tear down popup handling."
  (when iota-popup--hooks-installed
    (remove-hook 'window-configuration-change-hook
                 #'iota-popup--on-window-configuration-change)
    (setq iota-popup--hooks-installed nil))
  ;; Clean up all overlays
  (maphash (lambda (window _)
             (iota-popup--remove-decorations window))
           iota-popup--overlays)
  (clrhash iota-popup--overlays)
  (setq iota-popup--active-popups nil))

;;;###autoload
(define-minor-mode iota-popup-mode
  "Toggle I O T Λ popup window handling.
When enabled, popup windows (which-key, transient, etc.) receive
TUI box decorations for visual consistency with the IOTA interface."
  :global t
  :group 'iota-popup
  :lighter " ιPop"
  (if iota-popup-mode
      (iota-popup--setup)
    (iota-popup--teardown)))

;;; Interactive Commands

(defun iota-popup-refresh ()
  "Manually refresh popup window decorations."
  (interactive)
  (iota-popup--update-all))

(defun iota-popup-cycle-style ()
  "Cycle through popup decoration styles."
  (interactive)
  (setq iota-popup-decoration-style
        (pcase iota-popup-decoration-style
          ('bottom-line 'none)
          ('none 'bottom-line)
          (_ 'bottom-line)))
  (iota-popup--update-all)
  (message "Popup decoration style: %s" iota-popup-decoration-style))

(provide 'iota-popup)
;;; iota-popup.el ends here
