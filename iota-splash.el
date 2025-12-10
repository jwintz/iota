;;; iota-splash.el --- I O T Λ splash screen -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Version: 0.1.0
;; Keywords: faces, splash, startup, minimal
;; URL: https://github.com/jwintz/iota

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Provides the IOTA splash screen displaying logo variants
;; with proper branding colors and centered layout.

;;; Code:

;; Only require what's absolutely needed at load time
(require 'iota-faces)  ; Defines faces used in splash
(require 'iota-utils)  ; For iota-modeline-effective-width
(require 'iota-special-buffer)  ; Unified special buffer management
(require 'color)       ; For color-lighten-name

;; Declare functions from modules loaded on-demand
(declare-function iota-timers-run-with-timer "iota-timers")
(declare-function iota-timers-cancel "iota-timers")
(declare-function iota-box-horizontal-line "iota-box")
(declare-function iota-theme-get-box-face "iota-theme")
(declare-function iota-popup--popup-visible-p "iota-popup")

(defcustom iota-splash-buffer-name "*I O T Λ splash*"
  "Buffer name for IOTA splash screen."
  :type 'string
  :group 'iota)

(defcustom iota-splash-show-hints t
  "Show animated hints about IOTA features in splash screen.
When non-nil, display rotating hints about IOTA functionality."
  :type 'boolean
  :group 'iota)

(defcustom iota-splash-animation-interval 0.1
  "Interval in seconds between animation frames.
Set to nil to disable logo animation entirely."
  :type '(choice (const :tag "Disabled" nil)
                 (float :tag "Interval in seconds"))
  :group 'iota)

(defcustom iota-splash-hints-random nil
  "When non-nil, show hints in random order instead of sequential."
  :type 'boolean
  :group 'iota)

(defcustom iota-splash-hint-prefixes '("C-c e" "C-c v" "C-c f" "C-c g" "C-c p" "C-c s" "C-c t" "C-c w" "C-c b" "C-c h")
  "List of key prefixes to scan for bound commands.
These prefixes are used to discover commands bound via general.el or other methods."
  :type '(repeat string)
  :group 'iota)

(defvar iota-splash--animation-step 0
  "Current step of the animation cycle.")

(defvar iota-splash--hint-index 0
  "Current index of the hint being displayed.")

(defvar-local iota-splash--last-window-height nil
  "Last known window height for splash screen.")

(defvar-local iota-splash--last-window-width nil
  "Last known window width for splash screen.")

(defvar-local iota-splash--anim-markers nil
  "Markers for animated character positions: (marker1 marker2 marker3 marker4).")

(defvar iota-splash--hints nil
  "Dynamically generated hints from commands.
Populated by `iota-splash-generate-hints' when available.")

(defvar iota-splash--hint-timer nil
  "Legacy hint rotation timer (deprecated, use iota-timers registry).")

(defvar iota-splash--animation-timer nil
  "Legacy animation timer (deprecated, use iota-timers registry).")

(defvar iota-splash-debug nil
  "When non-nil, log debug messages for splash screen operations.
Set to t to enable logging, or a buffer name to log to that buffer.")

(defvar iota-splash--in-update nil
  "Guard variable to prevent re-entrant hook calls.
Set to t while update functions are running to break feedback loops.")

(defvar iota-splash--separator-visible nil
  "Current state of separator visibility.
Used to avoid unnecessary force-window-update calls.")

(defvar iota-splash--resize-timer nil
  "Timer for checking window resize.")

(defun iota-splash--log (format-string &rest args)
  "Log a debug message if `iota-splash-debug' is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when iota-splash-debug
    (let ((msg (apply #'format (concat "[splash] " format-string) args)))
      (if (stringp iota-splash-debug)
          (with-current-buffer (get-buffer-create iota-splash-debug)
            (goto-char (point-max))
            (insert msg "\n"))
        (message "%s" msg)))))

(defun iota-splash--extract-keymap-commands (keymap)
  "Extract all command symbols from KEYMAP recursively.
Returns a list of unique command symbols."
  (let ((commands nil))
    (when (keymapp keymap)
      (map-keymap
       (lambda (_key binding)
         (cond
          ;; Direct command
          ((commandp binding)
           (push binding commands))
          ;; Nested keymap
          ((keymapp binding)
           (setq commands (append commands (iota-splash--extract-keymap-commands binding))))
          ;; cons cell (for extended menu items, etc)
          ((and (consp binding)
                (commandp (cdr binding)))
           (push (cdr binding) commands))))
       keymap))
    (delete-dups commands)))

(defun iota-splash--extract-general-commands ()
  "Extract command symbols from general.el keybindings.
Returns a list of unique command symbols bound via general.el."
  (when (featurep 'general)
    (let ((commands nil))
      ;; Method 1: Try general-keybindings variable (nested alist)
      (when (boundp 'general-keybindings)
        (dolist (entry general-keybindings)
          ;; Try different possible structures
          (let ((data (cdr entry)))
            (cond
             ;; If data is a list of bindings
             ((listp data)
              (dolist (item data)
                (let ((binding-data (if (consp item) (cdr item) item)))
                  (cond
                   ;; Direct symbol
                   ((and (symbolp binding-data) (commandp binding-data))
                    (push binding-data commands))
                   ;; Quoted symbol
                   ((and (consp binding-data)
                         (eq (car binding-data) 'quote)
                         (symbolp (cadr binding-data))
                         (commandp (cadr binding-data)))
                    (push (cadr binding-data) commands))
                   ;; Nested list
                   ((listp binding-data)
                    (dolist (nested-item binding-data)
                      (when (consp nested-item)
                        (let ((def (cdr nested-item)))
                          (cond
                           ((and (symbolp def) (commandp def))
                            (push def commands))
                           ((and (consp def)
                                 (eq (car def) 'quote)
                                 (symbolp (cadr def))
                                 (commandp (cadr def)))
                            (push (cadr def) commands)))))))))))
             ;; If data itself is a keymap
             ((keymapp data)
              (setq commands (append commands (iota-splash--extract-keymap-commands data))))))))

      ;; Method 2: Extract from configured prefix keymaps
      (dolist (prefix iota-splash-hint-prefixes)
        (condition-case nil
            (let ((binding (key-binding (kbd prefix))))
              (when (keymapp binding)
                (setq commands (append commands
                                       (iota-splash--extract-keymap-commands binding)))))
          (error nil)))

      ;; Return unique commands
      (delete-dups commands))))

(defun iota-splash--get-command-description (command)
  "Get a description for COMMAND, preferring marginalia if available."
  (let ((doc nil))
    ;; Try marginalia first if available
    (when (and (featurep 'marginalia)
               (fboundp 'marginalia-annotate-command))
      (let ((annotation (marginalia-annotate-command (symbol-name command))))
        (when (and annotation (stringp annotation))
          ;; Clean up marginalia annotation (remove properties and extra whitespace)
          (setq doc (substring-no-properties annotation))
          ;; Collapse multiple spaces into single space
          (setq doc (replace-regexp-in-string "[ \t]+" " " doc))
          ;; Trim whitespace
          (setq doc (string-trim doc)))))
    ;; Fall back to first line of documentation
    (unless doc
      (when-let ((docstring (documentation command)))
        (setq doc (car (split-string docstring "\n")))))
    ;; Clean up and return
    (when doc
      (string-trim doc))))

(defun iota-splash-generate-hints ()
  "Generate hints dynamically from IOTA and general.el commands.
Returns a list of hint strings with command descriptions."
  (let ((hints nil)
        (commands nil))

    ;; 1. Collect all IOTA commands
    (mapatoms
     (lambda (sym)
       (when (and (commandp sym)
                  (string-prefix-p "iota-" (symbol-name sym))
                  ;; Exclude internal/private commands
                  (not (string-match-p "--" (symbol-name sym)))
                  ;; Must have documentation
                  (documentation sym))
         (push sym commands))))

    ;; 2. Collect commands from general.el bindings
    (when (featurep 'general)
      (let ((general-cmds (iota-splash--extract-general-commands)))
        (setq commands (append commands general-cmds))
        ;; Remove duplicates
        (setq commands (delete-dups commands))))

    ;; 3. Generate hints for all collected commands
    (dolist (cmd commands)
      (when-let ((desc (iota-splash--get-command-description cmd)))
        (let ((hint (format "M-x %s — %s" (symbol-name cmd) desc)))
          (push hint hints))))

    ;; Return sorted list
    (when hints
      (sort hints #'string<))))

(defun iota-splash-refresh-hints ()
  "Refresh the hints list.
Call this after loading new IOTA modules or changing general.el bindings."
  (interactive)
  (let ((hints (iota-splash-generate-hints)))
    (setq iota-splash--hints hints)
    (setq iota-splash--hint-index 0)  ; Reset index
    ;; Redraw splash screen if visible
    (when (get-buffer iota-splash-buffer-name)
      (iota-splash--redraw-buffer))))

;; Backward compatibility alias
(defalias 'iota-splash-refresh-dynamic-hints 'iota-splash-refresh-hints
  "Deprecated alias for `iota-splash-refresh-hints'.")

(defun iota-splash--get-hints ()
  "Get the current list of hints to display."
  iota-splash--hints)

(defun iota-splash-hints-debug ()
  "Show debug info about hint configuration."
  (interactive)
  (let ((hints (iota-splash--get-hints)))
    (message "Hints: random=%s | total=%d current-idx=%d | current: %s"
             iota-splash-hints-random
             (length hints)
             iota-splash--hint-index
             (truncate-string-to-width (or (nth iota-splash--hint-index hints) "nil") 40))))

(defconst iota-splash--base-colors
  '("#39bae6" "#7ac5ed" "#b0ddf5" "#d9e3fa" "#ffcc66" "#ffe6a3" "#fffbe0"
    "#ffe6a3" "#ffcc66" "#d9e3fa" "#b0ddf5" "#7ac5ed" "#39bae6")
  "Seamless loop palette: cyan→pale-blue→white-yellow→yellow→white-yellow→pale-blue→cyan.")

(defun iota-splash--hex-to-rgb (hex)
  "Convert HEX color string to (R G B) list with values 0.0-1.0."
  (let ((hex (if (string-prefix-p "#" hex) (substring hex 1) hex)))
    (list (/ (string-to-number (substring hex 0 2) 16) 255.0)
          (/ (string-to-number (substring hex 2 4) 16) 255.0)
          (/ (string-to-number (substring hex 4 6) 16) 255.0))))

(defun iota-splash--interpolate-color (color1 color2 ratio)
  "Interpolate between COLOR1 and COLOR2 at RATIO (0.0 to 1.0).
Returns a hex color string."
  (let* ((rgb1 (iota-splash--hex-to-rgb color1))
         (rgb2 (iota-splash--hex-to-rgb color2))
         (r (+ (* (nth 0 rgb1) (- 1.0 ratio)) (* (nth 0 rgb2) ratio)))
         (g (+ (* (nth 1 rgb1) (- 1.0 ratio)) (* (nth 1 rgb2) ratio)))
         (b (+ (* (nth 2 rgb1) (- 1.0 ratio)) (* (nth 2 rgb2) ratio))))
    (color-rgb-to-hex r g b 2)))

(defun iota-splash--generate-smooth-palette (base-colors steps-between)
  "Generate a smooth palette by interpolating STEPS-BETWEEN colors between each pair in BASE-COLORS.
Colors form a seamless loop when first and last are identical."
  (let ((result nil)
        (len (length base-colors)))
    (dotimes (i len)
      (let ((color1 (nth i base-colors))
            (color2 (nth (% (1+ i) len) base-colors)))
        ;; Skip the last color since it equals the first (loop closure)
        (unless (= i (1- len))
          (push color1 result)
          (dotimes (step steps-between)
            (let ((ratio (/ (float (1+ step)) (float (1+ steps-between)))))
              (push (iota-splash--interpolate-color color1 color2 ratio) result))))))
    (nreverse result)))

(defconst iota-splash--animation-colors
  (iota-splash--generate-smooth-palette iota-splash--base-colors 10)
  "A smooth palette of interpolated colors for the gradient animation.")

(defun iota-splash--insert-tertiary ()
  "Insert the tertiary logo. Animation markers created AFTER centering."
  ;; Just insert the logo text - markers will be set after center-region
  (insert (propertize "ι" 'face `(:foreground ,(nth 0 iota-splash--animation-colors))))
  (insert " • ")
  (insert (propertize "ο" 'face `(:foreground ,(nth 2 iota-splash--animation-colors))))
  (insert " • ")
  (insert (propertize "τ" 'face `(:foreground ,(nth 4 iota-splash--animation-colors))))
  (insert " • ")
  (insert (propertize "α" 'face `(:foreground ,(nth 6 iota-splash--animation-colors))))
  (insert "\n\n"))

(defun iota-splash--fix-animation-markers (start)
  "Create animation markers by finding Greek letters after START.
Must be called AFTER center-region to get correct positions."
  (save-excursion
    (goto-char start)
    (let (m1 m2 m3 m4)
      ;; Find ι
      (when (re-search-forward "ι" nil t)
        (setq m1 (copy-marker (match-beginning 0) t)))
      ;; Find ο
      (when (re-search-forward "ο" nil t)
        (setq m2 (copy-marker (match-beginning 0) t)))
      ;; Find τ
      (when (re-search-forward "τ" nil t)
        (setq m3 (copy-marker (match-beginning 0) t)))
      ;; Find α
      (when (re-search-forward "α" nil t)
        (setq m4 (copy-marker (match-beginning 0) t)))
      (setq iota-splash--anim-markers (list m1 m2 m3 m4)))))

(defun iota-splash--insert-footer ()
  "Insert the footer tagline."
  (insert "Not one iota more than needed.")
  (insert "\n"))

(defun iota-splash--insert-init-time ()
  "Insert the Emacs init time at the bottom of the splash screen."
  (insert "\n")  ; Empty line before init time
  (let ((init-time (emacs-init-time "%f")))  ; Get raw seconds as float string
    (when init-time
      (let* ((seconds (string-to-number init-time))
             (time-str (format "%.2f" seconds)))
        (insert (propertize "Loaded in " 'face 'shadow))
        (insert (propertize time-str 'face 'iota-splash-logo-primary))
        (insert (propertize " seconds" 'face 'shadow))
        (insert "\n")))))

(defun iota-splash--get-current-hint ()
  "Get the current hint text."
  (let* ((hints (iota-splash--get-hints))
         (hint (nth (mod iota-splash--hint-index (length hints)) hints)))
    hint))

(defvar-local iota-splash--hint-start nil
  "Marker for the start of hint text region.")

(defvar-local iota-splash--hint-end nil
  "Marker for the end of hint text region.")

(defun iota-splash--insert-hints ()
  "Insert hints section if enabled."
  (when iota-splash-show-hints
    (let ((hint (iota-splash--get-current-hint)))
      (insert "\n\n\n\n")  ; Add spacing to push hints lower
      ;; Start marker: moves after insertion (t)
      (setq iota-splash--hint-start (copy-marker (point) nil))
      (insert (propertize hint 'face 'iota-splash-tertiary))
      ;; End marker: doesn't move on insertion (nil)
      (setq iota-splash--hint-end (copy-marker (point) t))
      (insert "\n"))))

(defun iota-splash--rotate-hint ()
  "Rotate to the next hint and update ONLY the hint text in-place."
  (let ((buf (get-buffer iota-splash-buffer-name)))
    ;; Early exit if buffer doesn't exist
    (unless (and buf (buffer-live-p buf))
      (iota-splash--stop-hint-rotation)
      (cl-return-from iota-splash--rotate-hint))
    (with-current-buffer buf
      (let ((hints (iota-splash--get-hints)))
        (when hints
          (setq iota-splash--hint-index
                (if iota-splash-hints-random
                    (random (length hints))
                  (mod (1+ iota-splash--hint-index) (length hints)))))
        ;; Update hint text in-place without full redraw
        (when (and iota-splash--hint-start 
                   iota-splash--hint-end
                   (marker-position iota-splash--hint-start)
                   (marker-position iota-splash--hint-end))
          (let ((inhibit-read-only t)
                (new-hint (iota-splash--get-current-hint))
                (start-pos (marker-position iota-splash--hint-start)))
            (delete-region iota-splash--hint-start iota-splash--hint-end)
            (goto-char start-pos)
            (let ((hint-start (point)))
              (insert (propertize new-hint 'face 'iota-splash-tertiary))
              ;; Center the hint line
              (let ((fill-column (window-width (get-buffer-window (current-buffer)))))
                (center-line))
              ;; Update end marker to new position after centering
              (set-marker iota-splash--hint-end (point)))))))))

(defun iota-splash--start-hint-rotation ()
  "Start the hint rotation timer."
  (when (and iota-splash-show-hints
             (not iota-splash--hint-timer))
    (setq iota-splash--hint-index 0)
    ;; Use centralized timer registry (load on demand)
    (require 'iota-timers)
    (iota-timers-run-with-timer 'splash-hints 5 5 #'iota-splash--rotate-hint)))

(defun iota-splash--stop-hint-rotation ()
  "Stop the hint rotation timer."
  ;; Use centralized timer registry
  (when (featurep 'iota-timers)
    (iota-timers-cancel 'splash-hints))
  ;; Also clean up legacy timer if present
  (when (and iota-splash--hint-timer (timerp iota-splash--hint-timer))
    (cancel-timer iota-splash--hint-timer)
    (setq iota-splash--hint-timer nil)))

(defvar iota-splash--redraw-pending nil
  "Whether a redraw is pending (for debouncing).")

(defvar iota-splash--last-redraw-time 0
  "Time of last redraw (for throttling).")

(defcustom iota-splash-redraw-debounce 0.1
  "Minimum interval between splash screen redraws in seconds."
  :type 'float
  :group 'iota)

(defun iota-splash--redraw-buffer ()
  "Redraw the splash screen buffer content.
This regenerates the entire buffer with proper padding and centering.
Redraws are debounced to prevent performance issues."
  (let ((now (float-time)))
    ;; Throttle redraws
    (when (> (- now iota-splash--last-redraw-time) iota-splash-redraw-debounce)
      (setq iota-splash--last-redraw-time now)
      (iota-splash--do-redraw))))

(defun iota-splash--do-redraw ()
  "Actually perform the splash screen redraw."
  (iota-splash--log "do-redraw called")
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        ;; Prioritize selected window for layout calculations
        (let ((window (if (eq (window-buffer (selected-window)) buffer)
                          (selected-window)
                        (get-buffer-window buffer))))
          (when window
            (let ((current-height (window-body-height window))
                  (current-width (window-width window)))
              (when (and current-height current-width)
                (iota-splash--log "do-redraw: height=%d width=%d" current-height current-width)
                (setq iota-splash--last-window-height current-height)
                (setq iota-splash--last-window-width current-width)
                (let ((inhibit-read-only t)
                      ;; Inhibit redisplay during buffer regeneration
                      (inhibit-redisplay t))
                  ;; Regenerate the entire buffer with new padding
                  (erase-buffer)
                  
                  ;; RE-ASSERT mode-line/header-line hiding to prevent leaks
                  (setq-local mode-line-format nil)
                  (setq-local header-line-format nil)
                  
                  (let* ((content-lines (+ 6  ; logo + footer + empty line + init-time
                                           (if iota-splash-show-hints 5 0)))
                         ;; Use floor to ensure consistent padding calculation
                         (padding (max 0 (floor (/ (- current-height content-lines) 2.0)))))
                    (dotimes (_ padding) (insert "\n")))
                  (let ((logo-start (point)))
                    (iota-splash--insert-tertiary)
                    (iota-splash--insert-footer)
                    (iota-splash--insert-init-time)
                    ;; Center BEFORE saving marker positions (center-region modifies text)
                    (let ((fill-column (window-width window)))
                      (center-region logo-start (point)))
                    ;; NOW find and save the animated character positions after centering
                    (iota-splash--fix-animation-markers logo-start))

                  (let ((hints-start (point)))
                    (iota-splash--insert-hints)
                    (let ((fill-column (window-width window)))
                      (center-region hints-start (point))))

                  ;; Ensure cursor remains hidden after redraw
                  (setq-local cursor-type nil)
                  (setq-local cursor-in-non-selected-windows nil)
                  (setq-local visible-cursor nil)
                  ;; Move point to beginning
                  (goto-char (point-min))
                  ;; Hide cursor at window level (most reliable method)
                  (internal-show-cursor window nil))))))))))

(defun iota-splash--redraw-on-resize ()
  "Redraw splash screen if SELECTED window height or width changed.
We prioritize the selected window to avoid flicker loops when multiple
windows have different sizes."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        ;; Prefer selected window if it displays this buffer, otherwise get any
        (let ((window (if (eq (window-buffer (selected-window)) buffer)
                          (selected-window)
                        (get-buffer-window buffer))))
          (when window
            (let ((current-height (window-body-height window))
                  (current-width (window-width window)))
              (when (and current-height current-width
                         (or (not (equal current-height iota-splash--last-window-height))
                             (not (equal current-width iota-splash--last-window-width))))
                (iota-splash--redraw-buffer)))))))))

(defun iota-splash--check-and-redraw ()
  "Check if splash screen window height changed and redraw if needed.
Also updates separator line visibility based on minibuffer/which-key state.
This is a lightweight check that only acts when splash is visible."
  (let ((buf (get-buffer iota-splash-buffer-name)))
    (when (and buf (get-buffer-window buf))
      (iota-splash--update-separator)
      (iota-splash--redraw-on-resize))))

(defun iota-splash--minibuffer-active-p ()
  "Return t if the minibuffer is currently active."
  (and (active-minibuffer-window)
       (minibufferp (window-buffer (active-minibuffer-window)))))

(defun iota-splash--popup-visible-p ()
  "Return t if a popup window (which-key, transient, etc.) is currently visible.
Uses iota-popup for detection."
  (and (fboundp 'iota-popup--popup-visible-p)
       (iota-popup--popup-visible-p)))

(defun iota-splash--iota-buffer-p (buffer)
  "Return t if BUFFER is an iota splash or screen buffer."
  (when buffer
    (let ((name (buffer-name buffer)))
      (or (string-match-p "\\*I O T Λ splash\\*" name)
          (string-match-p "\\*I O T Λ screen-[0-9]+\\*" name)))))

(defun iota-splash--has-iota-buffer-below-p (window)
  "Return t if there is an iota splash/screen window directly below WINDOW."
  (when (window-live-p window)
    (let* ((edges (window-edges window))
           (bottom (nth 3 edges))
           (left (nth 0 edges))
           (right (nth 2 edges))
           (frame (window-frame window)))
      (catch 'found-iota-below
        (dolist (w (window-list frame 'no-minibuf))
          (unless (eq w window)
            (let* ((w-edges (window-edges w))
                   (w-top (nth 1 w-edges))
                   (w-left (nth 0 w-edges))
                   (w-right (nth 2 w-edges)))
              ;; Check if w is directly below and overlaps horizontally
              (when (and (= w-top bottom)
                         (> (min right w-right) (max left w-left))
                         (iota-splash--iota-buffer-p (window-buffer w)))
                (throw 'found-iota-below t)))))
        nil))))

(defun iota-splash--window-at-bottom-p (window)
  "Return t if WINDOW is at the bottom of the frame.
A window is at bottom if no other non-popup, non-minibuffer window is below it."
  (when (window-live-p window)
    (let* ((edges (window-edges window))
           (bottom (nth 3 edges))
           (left (nth 0 edges))
           (right (nth 2 edges))
           (frame (window-frame window)))
      (catch 'found-below
        (dolist (w (window-list frame 'no-minibuf))
          (unless (eq w window)
            (let* ((w-edges (window-edges w))
                   (w-top (nth 1 w-edges))
                   (w-left (nth 0 w-edges))
                   (w-right (nth 2 w-edges)))
              ;; Check if w is below window and overlaps horizontally
              ;; Skip popup windows as they are handled separately
              (when (and (= w-top bottom)
                         (> (min right w-right) (max left w-left))
                         (not (and (fboundp 'iota-popup--window-popup-p)
                                   (iota-popup--window-popup-p w))))
                (throw 'found-below nil)))))
        t))))

(defun iota-splash--should-show-separator-p (window)
  "Return t if separator should be shown for WINDOW in splash screen.
Separator is shown when:
- Minibuffer is active, OR
- Popup is visible AND splash screen is at the bottom, OR
- Another iota splash/screen buffer is directly below."
  (or (iota-splash--minibuffer-active-p)
      (and (iota-splash--popup-visible-p)
           (iota-splash--window-at-bottom-p window))
      (iota-splash--has-iota-buffer-below-p window)))

(defun iota-splash--get-separator-line (window)
  "Get a separator line string for WINDOW.
Uses the configured box style from iota-modeline if available.
Uses inactive face since popup/minibuffer is active when this is called."
  (require 'iota-box)  ; Load on demand
  (let* ((width (if (window-live-p window)
                    (iota-modeline-effective-width window)
                  79))
         (style (if (boundp 'iota-modeline-box-style)
                    iota-modeline-box-style
                  'rounded))
         ;; Always use inactive face for splash separator
         ;; (it's only shown when popup/minibuffer is active)
         (face 'iota-inactive-box-face))
    (iota-box-horizontal-line width style face)))

(defun iota-splash--update-separator ()
  "Update the separator line visibility for splash screen windows.
Shows separator when minibuffer or popup is active, or if split.
Iterates over ALL windows displaying the splash buffer."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      ;; Iterate over ALL windows displaying the splash buffer
      (dolist (win (get-buffer-window-list buffer nil t))
        (when (window-live-p win)
          (let ((should-show (iota-splash--should-show-separator-p win)))
            (if should-show
                ;; Show separator line (override buffer value with window parameter)
                (set-window-parameter win 'mode-line-format
                                    `(:eval (iota-splash--get-separator-line ,win)))
              ;; Hide separator line
              (set-window-parameter win 'mode-line-format 'none))))))))

(defun iota-splash--check-resize ()
  "Check if splash screen needs redraw due to resize.
Called periodically by idle timer.
Prioritizes SELECTED window to avoid multi-window flicker loops."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      ;; Use same prioritization as redraw logic
      (let ((win (if (eq (window-buffer (selected-window)) buffer)
                     (selected-window)
                   (get-buffer-window buffer))))
        (when (window-live-p win)
          (with-current-buffer buffer
            (let ((current-height (window-body-height win))
                  (current-width (window-width win)))
              (when (and current-height current-width
                         (or (not (equal current-height iota-splash--last-window-height))
                             (not (equal current-width iota-splash--last-window-width))))
                (iota-splash--log "resize detected: %dx%d -> %dx%d"
                                  iota-splash--last-window-width iota-splash--last-window-height
                                  current-width current-height)
                (iota-splash--redraw-buffer)))))))))

(defun iota-splash--start-resize-timer ()
  "Start the resize check timer."
  (iota-splash--stop-resize-timer)
  (setq iota-splash--resize-timer
        (run-with-idle-timer 0.2 t #'iota-splash--check-resize)))

(defun iota-splash--stop-resize-timer ()
  "Stop the resize check timer."
  (when (timerp iota-splash--resize-timer)
    (cancel-timer iota-splash--resize-timer)
    (setq iota-splash--resize-timer nil)))

(defun iota-splash--on-minibuffer-setup ()
  "Handle minibuffer setup to show separator and redraw splash screen."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer
               (buffer-live-p buffer)
               (get-buffer-window buffer))
      ;; Show separator line when minibuffer is active
      (iota-splash--update-separator)
      ;; Force redraw when minibuffer opens
      (iota-splash--redraw-buffer))))

(defun iota-splash--on-minibuffer-exit ()
  "Handle minibuffer exit to hide separator and redraw splash screen."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer
               (buffer-live-p buffer)
               (get-buffer-window buffer))
      ;; Schedule separator update after minibuffer is fully closed
      (run-with-timer 0.01 nil #'iota-splash--update-separator)
      ;; Force redraw when minibuffer closes
      (run-with-timer 0.02 nil #'iota-splash--redraw-buffer))))

(defun iota-splash--on-echo-area-clear ()
  "Handle echo area clear to redraw splash screen."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer
               (buffer-live-p buffer)
               (get-buffer-window buffer))
      ;; Update separator and redraw
      (iota-splash--update-separator)
      (iota-splash--redraw-buffer))))



(defun iota-splash--cleanup-hooks ()
  "Remove all splash screen hooks and reset state."
  ;; Reset state variables
  (setq iota-splash--separator-visible nil)
  (setq iota-splash--in-update nil)
  ;; Stop resize timer
  (iota-splash--stop-resize-timer)
  ;; Remove hooks
  (remove-hook 'minibuffer-setup-hook #'iota-splash--on-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'iota-splash--on-minibuffer-exit)
  ;; Legacy hooks (remove if present from older sessions)
  (remove-hook 'window-configuration-change-hook #'iota-splash--on-window-config-change)
  (remove-hook 'window-size-change-functions #'iota-splash--on-resize)
  (remove-hook 'post-command-hook #'iota-splash--check-and-redraw)
  (remove-hook 'buffer-list-update-hook #'iota-splash--check-buffer-switch)
  (remove-hook 'echo-area-clear-hook #'iota-splash--on-echo-area-clear))


(defun iota-splash--animate-step ()
  "Execute one step of the color animation.
Updates text properties at marker positions - buffer-local, no global redisplay."
  (let ((buf (get-buffer iota-splash-buffer-name)))
    ;; Early exit if buffer doesn't exist or not visible
    (unless (and buf (buffer-live-p buf))
      (iota-splash--stop-animation)
      (cl-return-from iota-splash--animate-step))
    (with-current-buffer buf
      (when (and iota-splash--anim-markers
                 (cl-every #'marker-position iota-splash--anim-markers))
        (let* ((inhibit-read-only t)
               ;; Inhibit redisplay during property updates
               (inhibit-redisplay t)
               (num-colors (length iota-splash--animation-colors))
               (colors (list
                        (nth (% (+ iota-splash--animation-step 0) num-colors) iota-splash--animation-colors)
                        (nth (% (+ iota-splash--animation-step 2) num-colors) iota-splash--animation-colors)
                        (nth (% (+ iota-splash--animation-step 4) num-colors) iota-splash--animation-colors)
                        (nth (% (+ iota-splash--animation-step 6) num-colors) iota-splash--animation-colors))))
          ;; Update text properties at marker positions
          (cl-loop for marker in iota-splash--anim-markers
                   for color in colors
                   for pos = (marker-position marker)
                   when pos do
                   (put-text-property pos (1+ pos) 'face `(:foreground ,color)))
          (setq iota-splash--animation-step (1+ iota-splash--animation-step)))))))

(defun iota-splash--start-animation ()
  "Start the splash screen animation timer.
Respects `iota-splash-animation-interval' setting."
  (setq iota-splash--animation-step 0)
  (when iota-splash-animation-interval
    ;; Use centralized timer registry (load on demand)
    (require 'iota-timers)
    (iota-timers-cancel 'splash-animation)
    (iota-timers-run-with-timer 'splash-animation 0 iota-splash-animation-interval
                                 #'iota-splash--animate-step)))

(defun iota-splash--stop-animation ()
  "Stop the splash screen animation timer."
  ;; Use centralized timer registry
  (when (featurep 'iota-timers)
    (iota-timers-cancel 'splash-animation))
  ;; Also clean up legacy timer if present
  (when (and iota-splash--animation-timer (timerp iota-splash--animation-timer))
    (cancel-timer iota-splash--animation-timer)
    (setq iota-splash--animation-timer nil)))

(defun iota-splash--file-buffers-exist-p ()
  "Return t if any buffer is visiting a file.
This indicates Emacs was likely started with file arguments."
  (cl-some (lambda (buf)
             (buffer-file-name buf))
           (buffer-list)))

(defun iota-splash-quit ()
  "Quit the splash screen and clean up all timers and hooks."
  (interactive)
  ;; Stop all timers first
  (iota-splash--stop-animation)
  (iota-splash--stop-hint-rotation)
  ;; Clean up hooks
  (iota-splash--cleanup-hooks)
  ;; Use unified special buffer dismissal (handles cursor restoration)
  (iota-special-buffer-dismiss iota-splash-buffer-name))

(defun iota-splash-screen (&optional force)
  "Display IOTA splash screen with branding.
If FORCE is non-nil, display even if files are open.
Does not display if Emacs was opened with file arguments (unless FORCE is t)."
  (interactive "P")
  ;; Skip splash screen if files were opened, unless forced or interactive
  (unless (and (not force)
               (not (called-interactively-p 'any))
               (iota-splash--file-buffers-exist-p))
    ;; Auto-generate hints if not already done
    (when (and iota-splash-show-hints
               (null iota-splash--hints))
      (setq iota-splash--hints (iota-splash-generate-hints)))
    (setq inhibit-startup-screen t)
    (let ((buffer (get-buffer-create iota-splash-buffer-name)))
      ;; First, switch to the buffer to establish window context
      (switch-to-buffer buffer)

      ;; Setup as special buffer (hides cursor, sets buffer-local vars)
      (iota-special-buffer-setup buffer
        :cleanup-fn (lambda ()
                      (iota-splash--stop-animation)
                      (iota-splash--stop-hint-rotation)
                      (iota-splash--cleanup-hooks)))

      ;; Now, populate and configure the buffer in the correct window
      (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Store initial window dimensions
        (setq iota-splash--last-window-height (window-body-height))
        (setq iota-splash--last-window-width (window-width))

        ;; Layout Calculations (now in correct window context)
        (let* ((content-lines (+ 6  ; logo + footer + empty line + init-time
                                 (if iota-splash-show-hints 5 0)))
               ;; Use floor to ensure consistent padding calculation
               (padding (max 0 (floor (/ (- (window-body-height) content-lines) 2.0)))))
          (dotimes (_ padding) (insert "\n")))

        ;; Insert Content
        (let ((logo-start (point)))
          (iota-splash--insert-tertiary)
          (iota-splash--insert-footer)
          (iota-splash--insert-init-time)
          ;; Center logo, tagline, and init time
          (let ((fill-column (window-width)))
            (center-region logo-start (point)))
          ;; Create animation markers AFTER centering
          (iota-splash--fix-animation-markers logo-start))

        (let ((hints-start (point)))
          (iota-splash--insert-hints)
          ;; Center hints only
          (let ((fill-column (window-width)))
            (center-region hints-start (point))))

        ;; Finalize Buffer State
        (read-only-mode 1)
        ;; Use a minimal keymap that doesn't inherit global C-c bindings
        (use-local-map (let ((map (make-sparse-keymap)))
                         (define-key map (kbd "q") 'iota-splash-quit)
                         (define-key map (kbd "RET") 'iota-splash-quit)
                         map))

        ;; Hide both mode-line and header-line in splash screen
        (setq-local mode-line-format nil)
        (setq-local header-line-format nil)

        ;; Disable hl-line in splash buffer
        (setq-local global-hl-line-mode nil)
        (setq-local hl-line-mode nil)
        ;; Move point to beginning for aesthetic consistency
        (goto-char (point-min))
        ;; Make the buffer truly read-only
        (setq buffer-read-only t))) ; closes inner let and with-current-buffer

      ;; Set window parameters after buffer is displayed
      ;; Initially hide mode-line, will be shown when minibuffer activates
      (set-window-parameter (selected-window) 'mode-line-format 'none)
      (set-window-parameter (selected-window) 'header-line-format nil)

      ;; Clear echo area and set minibuffer window properties
      (message nil)
      (let ((minibuf-win (minibuffer-window)))
        (when (window-live-p minibuf-win)
          (set-window-parameter minibuf-win 'mode-line-format nil)
          (with-selected-window minibuf-win
            (setq-local mode-line-format nil))))

      ;; Add hooks for layout/interaction
      ;; Use idle timer for resize detection (avoids hook cascade issues)
      (iota-splash--start-resize-timer)
      ;; Minibuffer hooks for separator line
      (add-hook 'minibuffer-setup-hook #'iota-splash--on-minibuffer-setup)
      (add-hook 'minibuffer-exit-hook #'iota-splash--on-minibuffer-exit)

      ;; Start animations and hint rotation
      (iota-splash--start-animation)
      (iota-splash--start-hint-rotation)
      ;; Return the buffer
      buffer)))

(provide 'iota-splash)

;;; iota-splash.el ends here
