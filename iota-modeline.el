;;; iota-modeline.el --- I O T Λ modeline wrapper -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: modeline, faces
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; IOTA modeline is a WRAPPER that adds TUI-style box decorations
;; around the user's existing modeline. It does NOT replace modeline
;; content - that is left to the user's preferred modeline package
;; (doom-modeline, mood-line, vanilla Emacs, etc.).
;;
;; Features:
;;   - Wraps existing mode-line-format with box decorations
;;   - Terminal overlay support for header-line position
;;   - Automatic separator lines between windows
;;   - Theme-aware face integration
;;   - Centralized update system integration

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'iota-box)
(require 'iota-theme)
(require 'iota-update)
(require 'iota-timers)
(require 'iota-utils)

;;; Customization

(defgroup iota-modeline nil
  "I O T Λ modeline configuration."
  :group 'iota
  :prefix "iota-modeline-")

(defcustom iota-modeline-position 'header
  "Position of I O T Λ modeline.
Can be: header (header-line), mode (mode-line), or both."
  :type '(choice (const :tag "Header line" header)
                 (const :tag "Mode line" mode)
                 (const :tag "Both" both))
  :group 'iota-modeline)

(defcustom iota-modeline-box-style 'rounded
  "Box drawing style for modeline.
Can be: single, double, rounded, heavy, ascii."
  :type '(choice (const :tag "Single line" single)
                 (const :tag "Double line" double)
                 (const :tag "Rounded corners" rounded)
                 (const :tag "Heavy line" heavy)
                 (const :tag "ASCII (compatibility)" ascii))
  :group 'iota-modeline)

(defcustom iota-modeline-height 1
  "Height of modeline in lines.
Currently only 1 is supported."
  :type 'integer
  :group 'iota-modeline)

(defcustom iota-modeline-update-debounce 0.15
  "Debounce interval for modeline updates in seconds.
Recommended range: 0.1 - 0.3. Higher values reduce CPU usage.
Note: This is now handled by the centralized update system."
  :type 'float
  :group 'iota-modeline)

(defcustom iota-modeline-show-in-inactive nil
  "Show I O T Λ modeline in inactive windows.
If nil, inactive windows use default modeline."
  :type 'boolean
  :group 'iota-modeline)

(defcustom iota-modeline-excluded-buffers '("\\*Warnings\\*")
  "List of buffer names (strings or regexps) to exclude from IOTA modeline."
  :type '(repeat (choice string regexp))
  :group 'iota-modeline)

(defcustom iota-modeline-show-separators t
  "Show vertical separators between modeline segments.
When non-nil, segments are separated by │ characters with
T-junctions (┬/┴) in the top and bottom borders."
  :type 'boolean
  :group 'iota-modeline)

;;; State

(defvar iota-modeline--original-mode-line-format nil
  "Saved original `mode-line-format' before IOTA wrapping.")

(defvar iota-modeline--original-header-line-format nil
  "Original header-line-format before IOTA.")

(defvar iota-modeline--update-timer nil
  "Timer for periodic modeline updates.")

(defvar iota-modeline--last-update-time 0
  "Time of last modeline update.")

(defvar iota-modeline--current-window nil
  "The window currently being rendered.
This is dynamically bound during modeline rendering to allow
content to check if it's rendering for the selected window.")

(defvar iota-modeline--selected-window nil
  "The truly selected window at the time of rendering.
This is captured BEFORE any with-selected-window context switches,
allowing content to determine if it's in the active window.")

(defvar iota-modeline--saved-mode-line-faces nil
  "Saved mode-line face attributes for restoration.")

;;; Wrapping Logic

(defun iota-modeline--parse-segments (content)
  "Parse CONTENT string into a list of segments.
Splits on runs of 2+ spaces to identify logical segments."
  (let ((segments (split-string content "  +" t)))
    ;; Trim each segment
    (mapcar #'string-trim segments)))

(defun iota-modeline--fit-segments-to-width (segments width style)
  "Fit SEGMENTS list into WIDTH, truncating as needed.
STYLE is the box style for separator width calculation.
Returns a list of segments that will fit."
  (let* ((sep-width 3)  ; " │ " = 3 chars
         (border-width 4)  ; "│ " + " │" = 4 chars
         (available (- width border-width))
         (result nil)
         (used 0))
    ;; Add segments until we run out of space
    (dolist (seg segments)
      (let* ((seg-width (string-width seg))
             (need-sep (if result sep-width 0))
             (total-need (+ seg-width need-sep)))
        (cond
         ;; Segment fits completely
         ((<= (+ used total-need) available)
          (push seg result)
          (setq used (+ used total-need)))
         ;; First segment and it's too long - truncate it
         ((null result)
          (let* ((trunc-width (- available 1))  ; Leave room for ellipsis
                 (truncated (truncate-string-to-width seg trunc-width nil nil "…")))
            (push truncated result)
            (setq used available)))
         ;; No more room - stop
         (t nil))))
    (nreverse result)))

(defun iota-modeline--render-box (&optional window)
  "Render complete IOTA modeline box for WINDOW.
Uses `iota-box-render-single-line' for proper T-junction handling."
  (let* ((win (or window (selected-window)))
         (width (max 10 (1- (window-body-width win))))
         (style iota-modeline-box-style)
         (face (iota-theme-get-box-face win))
         ;; Render the original modeline content
         (content (format-mode-line iota-modeline--original-mode-line-format nil win))
         ;; Remove ALL text properties
         (content (substring-no-properties content))
         (content (string-trim content))
         ;; Parse into segments if separators enabled
         (segments (if iota-modeline-show-separators
                       (iota-modeline--parse-segments content)
                     (list content)))
         ;; Fit segments to available width
         (fitted-segments (iota-modeline--fit-segments-to-width segments width style)))
    ;; Use iota-box-render-single-line for proper box with T-junctions
    (iota-box-render-single-line
     :left fitted-segments
     :center nil
     :right nil
     :width width
     :style style
     :face face
     :compact nil)))

(defun iota-modeline--render (&optional override-box-face window)
  "Render IOTA modeline format string for WINDOW.
If WINDOW is nil, use selected window.
OVERRIDE-BOX-FACE can override the box face.
Returns complete box: top border + content + bottom border."
  (if (not (iota-modeline--should-show-p))
      "" ; Return empty string for minibuffer
    (let* ((target-window (or window (selected-window)))
           (iota-modeline--current-window target-window)
           (selected-win-value (if (and (boundp 'iota-modeline--selected-window)
                                        iota-modeline--selected-window)
                                   iota-modeline--selected-window
                                 (selected-window)))
           (iota-modeline--selected-window selected-win-value))
      ;; Use new box rendering function
      (iota-modeline--render-box target-window))))

(defun iota-modeline--top-border (&optional window)
  "Render top border for WINDOW."
  (if (not (iota-modeline--should-show-p))
      ""
    (let* ((win (or window (selected-window)))
           (width (max 10 (1- (window-body-width win))))
           (style iota-modeline-box-style)
           (face (iota-theme-get-box-face win)))
      (iota-box-top-border width style face))))

(defun iota-modeline--bottom-border (&optional window)
  "Render bottom border for WINDOW."
  (if (not (iota-modeline--should-show-p))
      ""
    (let* ((win (or window (selected-window)))
           (width (max 10 (1- (window-body-width win))))
           (style iota-modeline-box-style)
           (face (iota-theme-get-box-face win)))
      (iota-box-bottom-border width style face))))

(defun iota-modeline--format ()
  "Return modeline format for active window."
  '(:eval (iota-modeline--render)))

(defun iota-modeline--format-inactive ()
  "Return modeline format for inactive windows."
  (if iota-modeline-show-in-inactive
      (iota-modeline--format)
    ;; Use simple format for inactive
    '(" " mode-line-buffer-identification " "
      mode-line-position " "
      mode-name)))

;;; Overlay Management (Terminal Support)

(defvar iota-modeline--window-overlays (make-hash-table :weakness 'key)
  "Hash table mapping windows to their header-line overlays.")

(defvar iota-modeline--separator-overlays (make-hash-table :weakness 'key)
  "Hash table mapping windows to their separator-line overlays.")

(defun iota-modeline--ensure-overlay (window)
  "Ensure the simulated header overlay exists for WINDOW.
Returns nil if window or buffer is invalid."
  (when (and (window-live-p window)
             (window-buffer window)
             (buffer-live-p (window-buffer window)))
    (let ((overlay (gethash window iota-modeline--window-overlays)))
      ;; Clean up invalid overlay
      (when (and overlay
                 (overlayp overlay)
                 (not (overlay-buffer overlay)))
        (remhash window iota-modeline--window-overlays)
        (setq overlay nil))
      ;; Create new overlay if needed
      (unless overlay
        (with-current-buffer (window-buffer window)
          (setq overlay (make-overlay (point-min) (point-min) (current-buffer)))
          (overlay-put overlay 'priority 100)
          (puthash window overlay iota-modeline--window-overlays)))
      overlay)))

(defun iota-modeline--update-overlay (&optional window selected-window)
  "Update the simulated header overlay for WINDOW.
If WINDOW is nil, use selected window.
If SELECTED-WINDOW is provided, use it as the truly selected window."
  (condition-case err
      (let ((win (or window (selected-window)))
            (truly-selected (or selected-window (selected-window))))
        (when (and (window-live-p win)
                   (window-buffer win)
                   (buffer-live-p (window-buffer win)))
          (with-current-buffer (window-buffer win)
            (when (eq iota-modeline-position 'header)
              (kill-local-variable 'header-line-format)
              (let ((overlay (iota-modeline--ensure-overlay win))
                    (start (window-start win)))
                (when (and overlay (overlayp overlay)
                           start (integer-or-marker-p start))
                  (condition-case render-err
                      (let* ((iota-modeline--selected-window truly-selected)
                             ;; Render complete box (top + content + bottom)
                             (box (with-selected-window win
                                    (iota-modeline--render nil win))))
                        (move-overlay overlay start start (current-buffer))
                        (overlay-put overlay 'before-string
                                     (if (and box (stringp box) (not (string= box "")))
                                         (concat box "\n")
                                       ""))
                        (overlay-put overlay 'window win))
                    (error
                     (message "I O T Λ: Error in render for buffer %s: %s"
                              (buffer-name) (error-message-string render-err))))))))))
    (error
     (message "I O T Λ: Error in update-overlay for window %s: %s"
              (if (windowp window) (window-buffer window) "nil")
              (error-message-string err)))))

(defun iota-modeline--remove-overlay ()
  "Remove all simulated header overlays."
  (maphash (lambda (_window overlay)
             (when (overlayp overlay)
               (delete-overlay overlay)))
           iota-modeline--window-overlays)
  (clrhash iota-modeline--window-overlays))

(defun iota-modeline--ensure-separator-overlay (window)
  "Ensure the separator overlay exists for WINDOW.
Returns nil if window or buffer is invalid."
  (when (and (window-live-p window)
             (window-buffer window)
             (buffer-live-p (window-buffer window)))
    (let ((overlay (gethash window iota-modeline--separator-overlays)))
      (when (and overlay
                 (overlayp overlay)
                 (not (overlay-buffer overlay)))
        (remhash window iota-modeline--separator-overlays)
        (setq overlay nil))
      (unless overlay
        (with-current-buffer (window-buffer window)
          (setq overlay (make-overlay (point-max) (point-max) (current-buffer)))
          (overlay-put overlay 'priority 100)
          (overlay-put overlay 'window window)
          (puthash window overlay iota-modeline--separator-overlays)))
      overlay)))

(defun iota-modeline--update-separator-overlay (window)
  "Update the separator overlay for WINDOW."
  (when (and (window-live-p window)
             (window-buffer window)
             (buffer-live-p (window-buffer window)))
    (let ((overlay (iota-modeline--ensure-separator-overlay window)))
      (when overlay
        (with-current-buffer (window-buffer window)
          (move-overlay overlay (point-max) (point-max) (current-buffer))
          (if (and (iota-modeline--should-show-p)
                   (iota-modeline--window-is-at-bottom-p window))
              (overlay-put overlay 'after-string
                          (propertize "\n" 'display
                                     (list 'space :width (window-width window))))
            (overlay-put overlay 'after-string nil)))))))

(defun iota-modeline--remove-separator-overlays ()
  "Remove all separator overlays."
  (maphash (lambda (_window overlay)
             (when (overlayp overlay)
               (delete-overlay overlay)))
           iota-modeline--separator-overlays)
  (clrhash iota-modeline--separator-overlays))

;;; Update Logic

(defun iota-modeline--update ()
  "Update modeline display."
  ;; For GUI mode, force mode-line update
  (when (display-graphic-p)
    (force-mode-line-update t))
  ;; For terminal mode or overlay-based rendering, update overlays
  (when (or (not (display-graphic-p))
            (eq iota-modeline-position 'header))
    (let ((truly-selected (selected-window)))
      (dolist (win (window-list))
        (iota-modeline--update-overlay win truly-selected)))))

(defun iota-modeline--update-windows (windows)
  "Update modeline display for specific WINDOWS only."
  (let ((truly-selected (selected-window)))
    (dolist (win windows)
      (when (window-live-p win)
        (iota-modeline--update-overlay win truly-selected)))))

(defun iota-modeline--do-update ()
  "Perform modeline update (called by centralized update system)."
  (iota-safe-call "modeline-update"
    (iota-modeline--update)))

(defun iota-modeline--should-update-p ()
  "Return t if modeline should update now."
  (let ((current-time (float-time)))
    (>= (- current-time iota-modeline--last-update-time)
        iota-modeline-update-debounce)))

(defun iota-modeline--debounced-update ()
  "Update modeline with debouncing."
  (when (iota-modeline--should-update-p)
    (setq iota-modeline--last-update-time (float-time))
    (iota-modeline--update)))

(defun iota-modeline--setup-timers ()
  "Set up periodic update timers using centralized timer registry."
  (iota-timers-cancel 'modeline-periodic)
  (unless (featurep 'iota-update)
    (setq iota-modeline--update-timer
          (iota-timers-run-with-timer 'modeline-periodic 60 60 #'iota-modeline--update))))

(defun iota-modeline--cleanup-timers ()
  "Clean up timers using centralized timer registry."
  (iota-timers-cancel 'modeline-periodic)
  (when iota-modeline--update-timer
    (cancel-timer iota-modeline--update-timer)
    (setq iota-modeline--update-timer nil))
  (cancel-function-timers #'iota-modeline--debounced-update))

;;; Hooks

(defun iota-modeline--post-command ()
  "Post-command hook for modeline updates."
  (iota-modeline--debounced-update))

(defun iota-modeline--should-show-p ()
  "Return t if IOTA modeline should be shown in current buffer."
  (let ((buf-name (buffer-name)))
    (not (or (minibufferp)
             (string-prefix-p " *which-key*" buf-name)
             (string= "*Completions*" buf-name)
             (string-match-p "\\*I O T Λ splash\\*" buf-name)
             (cl-find-if (lambda (pattern) (string-match-p pattern buf-name))
                         iota-modeline-excluded-buffers)))))

(defun iota-modeline--ensure-format-in-buffer (buffer)
  "Ensure BUFFER has correct IOTA modeline format."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((iota-modeline--should-show-p)
        (when (local-variable-p 'header-line-format)
          (kill-local-variable 'header-line-format))
        (when (and (local-variable-p 'mode-line-format)
                   (not (eq iota-modeline-position 'header)))
          (kill-local-variable 'mode-line-format)))
       ((string= (buffer-name) "*Completions*")
        (setq-local mode-line-format
                    '(:eval (iota-box-horizontal-line (1- (window-body-width))
                                                      'single 'iota-muted-face))))
       (t
        (setq-local mode-line-format nil)
        (setq-local header-line-format nil))))))

(defun iota-modeline--buffer-list-update ()
  "Buffer-list-update hook for modeline updates."
  (dolist (window (window-list))
    (iota-modeline--ensure-format-in-buffer (window-buffer window)))
  (iota-modeline--debounced-update))

(defun iota-modeline--after-change-major-mode ()
  "Hook to apply modeline format after major mode change."
  (when iota-modeline-mode
    (iota-modeline--ensure-format-in-buffer (current-buffer))))

(defun iota-modeline--minibuffer-setup ()
  "Hook to disable modeline in minibuffer."
  (setq mode-line-format "")
  (setq header-line-format nil))

(defun iota-modeline--window-scroll (win _start)
  "Window scroll hook for overlay update."
  (iota-modeline--update-overlay win))

(defvar iota-modeline--in-window-config-change nil
  "Guard against recursive window configuration changes.")

(defun iota-modeline--window-configuration-change ()
  "Hook for window configuration changes."
  (unless iota-modeline--in-window-config-change
    (let ((iota-modeline--in-window-config-change t))
      (dolist (window (window-list))
        (with-current-buffer (window-buffer window)
          (when (and (local-variable-p 'header-line-format)
                     (not (equal header-line-format (default-value 'header-line-format))))
            (kill-local-variable 'header-line-format))
          (when (and (local-variable-p 'mode-line-format)
                     (not (equal mode-line-format (default-value 'mode-line-format))))
            (kill-local-variable 'mode-line-format))
          (when (and (not (display-graphic-p))
                     (eq iota-modeline-position 'header))
            (iota-modeline--update-overlay window)))))))

;;; Window Position Detection

(defun iota-modeline--window-size-change (_frame)
  "Handle window size changes."
  (iota-modeline--update))

(defun iota-modeline--window-is-at-bottom-p (window)
  "Return t if WINDOW is at the bottom of the frame."
  (catch 'found-window-below
    (let* ((edges1 (window-edges window))
           (bottom1 (nth 3 edges1))
           (left1 (nth 0 edges1))
           (right1 (nth 2 edges1)))
      (dolist (w (window-list nil 'no-minibuf))
        (when (not (eq window w))
          (let* ((edges2 (window-edges w))
                 (top2 (nth 1 edges2))
                 (left2 (nth 0 edges2))
                 (right2 (nth 2 edges2))
                 (buf-name (buffer-name (window-buffer w))))
            (when (and edges1 edges2
                       (= top2 bottom1)
                       (> (min right1 right2) (max left1 left2))
                       (not (string-prefix-p " *which-key*" buf-name))
                       (not (string-prefix-p "*which-key*" buf-name)))
              (throw 'found-window-below nil)))))
      t)))

(defun iota-modeline--update-separator-lines (&optional _frame)
  "Update mode-line separator for each window based on position.
Shows horizontal line separator (no corners) between stacked windows."
  (dolist (window (window-list nil 'no-minibuf))
    (with-current-buffer (window-buffer window)
      (when (iota-modeline--should-show-p)
        ;; Use horizontal line (no corners) as separator
        (setq-local mode-line-format
                    '(:eval (iota-box-horizontal-line (1- (window-body-width))
                                                      iota-modeline-box-style
                                                      (iota-theme-get-box-face (selected-window))))))))
  (dolist (window (window-list nil 'no-minibuf))
    (if (and (with-current-buffer (window-buffer window)
               (iota-modeline--should-show-p))
             (iota-modeline--window-is-at-bottom-p window))
        (set-window-parameter window 'mode-line-format nil)
      (set-window-parameter window 'mode-line-format 'none))))

;;; Face Management

(defun iota-modeline--save-face-attributes ()
  "Save current mode-line face attributes for later restoration."
  (setq iota-modeline--saved-mode-line-faces
        (list
         (list 'mode-line
               :background (face-attribute 'mode-line :background nil t)
               :box (face-attribute 'mode-line :box nil t)
               :underline (face-attribute 'mode-line :underline nil t)
               :overline (face-attribute 'mode-line :overline nil t))
         (list 'mode-line-inactive
               :background (face-attribute 'mode-line-inactive :background nil t)
               :box (face-attribute 'mode-line-inactive :box nil t)
               :underline (face-attribute 'mode-line-inactive :underline nil t)
               :overline (face-attribute 'mode-line-inactive :overline nil t))
         (list 'header-line
               :background (face-attribute 'header-line :background nil t)
               :box (face-attribute 'header-line :box nil t)
               :underline (face-attribute 'header-line :underline nil t)
               :overline (face-attribute 'header-line :overline nil t)))))

(defun iota-modeline--restore-face-attributes ()
  "Restore saved mode-line face attributes."
  (dolist (spec iota-modeline--saved-mode-line-faces)
    (let ((face (car spec))
          (attrs (cdr spec)))
      (condition-case nil
          (apply #'set-face-attribute face nil attrs)
        (error nil)))))

;;; Mode Setup

(defun iota-modeline--setup ()
  "Set up IOTA modeline."
  ;; Save original formats FIRST
  (unless iota-modeline--original-mode-line-format
    (setq iota-modeline--original-mode-line-format
          (default-value 'mode-line-format)))
  (unless iota-modeline--original-header-line-format
    (setq iota-modeline--original-header-line-format
          (default-value 'header-line-format)))
  
  ;; Save face attributes
  (iota-modeline--save-face-attributes)
  
  ;; Remove backgrounds from modeline faces for transparency
  (set-face-attribute 'mode-line nil 
                      :background 'unspecified 
                      :box nil
                      :underline nil
                      :overline nil)
  (set-face-attribute 'mode-line-inactive nil 
                      :background 'unspecified 
                      :box nil
                      :underline nil
                      :overline nil)
  (set-face-attribute 'header-line nil 
                      :background 'unspecified 
                      :box nil
                      :underline nil
                      :overline nil)
  
  ;; Apply IOTA modeline based on position
  (pcase iota-modeline-position
    ('header
     ;; Header position uses overlays for the box, mode-line for separator
     (setq-default header-line-format nil)
     (add-hook 'window-scroll-functions #'iota-modeline--window-scroll)
     (add-hook 'window-size-change-functions #'iota-modeline--update-separator-lines)
     (iota-modeline--update-separator-lines))
    ('mode
     (setq-default header-line-format nil)
     (setq-default mode-line-format '(:eval (iota-modeline--render))))
    ('both
     (if (display-graphic-p)
         (setq-default header-line-format '(:eval (iota-modeline--render)))
       (setq-default header-line-format nil)
       (add-hook 'window-scroll-functions #'iota-modeline--window-scroll))
     (setq-default mode-line-format '(:eval (iota-modeline--render)))))

  ;; Enable transparent theme in terminal mode
  (when (and (not (display-graphic-p))
             (fboundp 'iota-theme-transparent-mode)
             (not (bound-and-true-p iota-theme-transparent-mode)))
    (iota-theme-transparent-mode 1))

  ;; Register with centralized update system
  (when (featurep 'iota-update)
    (iota-update-register-component :modeline #'iota-modeline--do-update)
    (iota-update-install-hooks))
  
  ;; Set up hooks
  (unless (featurep 'iota-update)
    (add-hook 'buffer-list-update-hook #'iota-modeline--buffer-list-update))
  
  (add-hook 'after-change-major-mode-hook #'iota-modeline--after-change-major-mode)
  (add-hook 'minibuffer-setup-hook #'iota-modeline--minibuffer-setup)
  
  (unless (featurep 'iota-update)
    (add-hook 'window-size-change-functions #'iota-modeline--window-size-change))

  ;; Set up timers
  (iota-modeline--setup-timers)
  
  ;; Apply to all existing buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (iota-modeline--should-show-p)
          (progn
            (kill-local-variable 'header-line-format)
            (kill-local-variable 'mode-line-format))
        (setq-local mode-line-format nil))))
  
  ;; Disable mode-line in minibuffer buffers
  (dolist (buffer (buffer-list))
    (when (string-match-p "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))
      (with-current-buffer buffer
        (setq mode-line-format ""
              header-line-format nil))))

  ;; Set window parameters for minibuffer windows
  (dolist (frame (frame-list))
    (let ((minibuf-win (minibuffer-window frame)))
      (when (window-live-p minibuf-win)
        (set-window-parameter minibuf-win 'mode-line-format nil))))
  
  ;; Initial update
  (iota-modeline--update))

(defun iota-modeline--teardown ()
  "Tear down IOTA modeline."
  ;; Unregister from centralized update system
  (when (featurep 'iota-update)
    (iota-update-unregister-component :modeline))
  
  ;; Remove hooks
  (remove-hook 'buffer-list-update-hook #'iota-modeline--buffer-list-update)
  (remove-hook 'after-change-major-mode-hook #'iota-modeline--after-change-major-mode)
  (remove-hook 'minibuffer-setup-hook #'iota-modeline--minibuffer-setup)
  (remove-hook 'window-scroll-functions #'iota-modeline--window-scroll)
  (remove-hook 'window-size-change-functions #'iota-modeline--window-size-change)
  (remove-hook 'window-size-change-functions #'iota-modeline--update-separator-lines)
  (remove-hook 'window-configuration-change-hook #'iota-modeline--window-configuration-change)

  ;; Clear window parameters
  (dolist (window (window-list nil 'no-minibuf))
    (set-window-parameter window 'mode-line-format nil))

  ;; Remove overlays
  (iota-modeline--remove-overlay)
  (iota-modeline--remove-separator-overlays)

  ;; Clean up timers
  (iota-modeline--cleanup-timers)

  ;; Restore original formats
  (when iota-modeline--original-header-line-format
    (setq-default header-line-format iota-modeline--original-header-line-format))
  (when iota-modeline--original-mode-line-format
    (setq-default mode-line-format iota-modeline--original-mode-line-format))

  ;; Restore face attributes
  (iota-modeline--restore-face-attributes)
  
  ;; Clear saved state
  (setq iota-modeline--original-mode-line-format nil)
  (setq iota-modeline--original-header-line-format nil)
  (setq iota-modeline--saved-mode-line-faces nil)

  ;; Force update
  (force-mode-line-update t))

;;; Minor Mode

;;;###autoload
(define-minor-mode iota-modeline-mode
  "Toggle I O T Λ modeline mode.
When enabled, wraps the existing modeline with I O T Λ's
decorated TUI-style box drawing."
  :global t
  :group 'iota-modeline
  :lighter " ι"
  (if iota-modeline-mode
      (iota-modeline--setup)
    (iota-modeline--teardown)))

;;; Interactive Commands

(defun iota-modeline-refresh ()
  "Refresh I O T Λ modeline."
  (interactive)
  (iota-modeline--update))

(defun iota-modeline-toggle-position ()
  "Toggle modeline position between header and mode line."
  (interactive)
  (setq iota-modeline-position
        (pcase iota-modeline-position
          ('header 'mode)
          ('mode 'header)
          (_ 'header)))
  (when iota-modeline-mode
    (iota-modeline--teardown)
    (iota-modeline--setup))
  (message "I O T Λ modeline position: %s" iota-modeline-position))

(defun iota-modeline-cycle-style ()
  "Cycle through box styles."
  (interactive)
  (setq iota-modeline-box-style
        (pcase iota-modeline-box-style
          ('single 'double)
          ('double 'rounded)
          ('rounded 'heavy)
          ('heavy 'ascii)
          ('ascii 'single)
          (_ 'rounded)))
  (iota-modeline-refresh)
  (message "I O T Λ modeline style: %s" iota-modeline-box-style))

(provide 'iota-modeline)
;;; iota-modeline.el ends here
