;;; iota-splash.el --- I O T Λ splash screen -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Version: 0.1.0
;; Keywords: faces, splash, startup, minimal
;; URL: https://github.com/yourusername/iota

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

(defvar iota-splash--animation-timer nil
  "Timer for splash screen animation.")

(defvar iota-splash--animation-step 0
  "Current step of the animation cycle.")

(defvar iota-splash--hint-index 0
  "Current index of the hint being displayed.")

(defvar iota-splash--hint-timer nil
  "Timer for rotating hints.")

(defvar-local iota-splash--last-window-height nil
  "Last known window height for splash screen.")

(defvar iota-splash--hints nil
  "Dynamically generated hints from commands.
Populated by `iota-splash-generate-hints' when available.")

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
  '("#39bae6" "#52c1e9" "#6ac8ec" "#81cfef" "#99d6f2" "#b0ddf5" "#c8e4f8" "#d9e3fa" "#ffcc66" "#ffd57a" "#ffde8f" "#ffe6a3" "#ffedb8" "#fff6cc" "#fffbe0")
  "Base palette of colors for the gradient animation.")

(defun iota-splash--interpolate-color (color1 color2 ratio)
  "Interpolate between COLOR1 and COLOR2 at RATIO (0.0 to 1.0).
Returns a hex color string."
  (let* ((rgb1 (color-name-to-rgb color1))
         (rgb2 (color-name-to-rgb color2))
         (r (+ (* (nth 0 rgb1) (- 1.0 ratio)) (* (nth 0 rgb2) ratio)))
         (g (+ (* (nth 1 rgb1) (- 1.0 ratio)) (* (nth 1 rgb2) ratio)))
         (b (+ (* (nth 2 rgb1) (- 1.0 ratio)) (* (nth 2 rgb2) ratio))))
    (color-rgb-to-hex r g b 2)))

(defun iota-splash--generate-smooth-palette (base-colors steps-between)
  "Generate a smooth palette by interpolating STEPS-BETWEEN colors between each pair in BASE-COLORS."
  (let ((result nil))
    (dotimes (i (1- (length base-colors)))
      (let ((color1 (nth i base-colors))
            (color2 (nth (1+ i) base-colors)))
        (push color1 result)
        (dotimes (step steps-between)
          (let ((ratio (/ (float (1+ step)) (float (1+ steps-between)))))
            (push (iota-splash--interpolate-color color1 color2 ratio) result)))))
    ;; Add the last color
    (push (car (last base-colors)) result)
    (nreverse result)))

(defconst iota-splash--animation-colors
  (iota-splash--generate-smooth-palette iota-splash--base-colors 10)
  "A smooth palette of interpolated colors for the gradient animation.")

(defun iota-splash--insert-tertiary ()
  "Insert the tertiary logo with animation-ready faces."
  (insert (propertize "ι" 'face 'iota-splash-anim-1))
  (insert " • ")  ; Use default (brightest) face for dots
  (insert (propertize "ο" 'face 'iota-splash-anim-2))
  (insert " • ")  ; Use default (brightest) face for dots
  (insert (propertize "τ" 'face 'iota-splash-anim-3))
  (insert " • ")  ; Use default (brightest) face for dots
  (insert (propertize "α" 'face 'iota-splash-anim-4))
  (insert "\n\n"))

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

(defun iota-splash--insert-hints ()
  "Insert hints section if enabled."
  (when iota-splash-show-hints
    (let ((hint (iota-splash--get-current-hint)))
      (insert "\n\n\n\n")  ; Add spacing to push hints lower
      (insert (propertize hint 'face 'iota-splash-tertiary))
      (insert "\n"))))

(defun iota-splash--rotate-hint ()
  "Rotate to the next hint and update the splash screen buffer."
  (let ((hints (iota-splash--get-hints)))
    (when hints
      (setq iota-splash--hint-index
            (if iota-splash-hints-random
                (random (length hints))
              (mod (1+ iota-splash--hint-index) (length hints))))))
  ;; Use the centralized redraw function
  (iota-splash--redraw-buffer))

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
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let ((window (get-buffer-window buffer)))
          (when window
            (let ((current-height (window-body-height window)))
              (when current-height
                (setq iota-splash--last-window-height current-height)
                (let ((inhibit-read-only t))
                  ;; Regenerate the entire buffer with new padding
                  (erase-buffer)
                  (let* ((content-lines (+ 6  ; logo + footer + empty line + init-time
                                           (if iota-splash-show-hints 5 0)))
                         (padding (max 0 (/ (- current-height content-lines) 2))))
                    (dotimes (_ padding) (insert "\n")))
                  (let ((logo-start (point)))
                    (iota-splash--insert-tertiary)
                    (iota-splash--insert-footer)
                    (iota-splash--insert-init-time)
                    (let ((fill-column (window-width window)))
                      (center-region logo-start (point))))

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
  "Redraw splash screen if window height changed."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let ((window (get-buffer-window buffer)))
          (when window
            (let ((current-height (window-body-height window)))
              (when (and current-height
                         (not (equal current-height iota-splash--last-window-height)))
                (iota-splash--redraw-buffer)))))))))

(defun iota-splash--check-and-redraw ()
  "Check if splash screen window height changed and redraw if needed.
Also updates separator line visibility based on minibuffer/which-key state."
  (when (get-buffer iota-splash-buffer-name)
    (iota-splash--update-separator)
    (iota-splash--redraw-on-resize)))

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

(defun iota-splash--has-iota-buffer-below-p ()
  "Return t if there is an iota splash/screen window directly below this one."
  (let ((splash-window (get-buffer-window iota-splash-buffer-name)))
    (when (window-live-p splash-window)
      (let* ((edges (window-edges splash-window))
             (bottom (nth 3 edges))
             (left (nth 0 edges))
             (right (nth 2 edges)))
        (catch 'found-iota-below
          (dolist (w (window-list nil 'no-minibuf))
            (unless (eq w splash-window)
              (let* ((w-edges (window-edges w))
                     (w-top (nth 1 w-edges))
                     (w-left (nth 0 w-edges))
                     (w-right (nth 2 w-edges)))
                ;; Check if w is directly below and overlaps horizontally
                (when (and (= w-top bottom)
                           (> (min right w-right) (max left w-left))
                           (iota-splash--iota-buffer-p (window-buffer w)))
                  (throw 'found-iota-below t)))))
          nil)))))

(defun iota-splash--window-at-bottom-p ()
  "Return t if the splash screen window is at the bottom of the frame.
A window is at bottom if no other non-popup, non-minibuffer window is below it."
  (let ((splash-window (get-buffer-window iota-splash-buffer-name)))
    (when (window-live-p splash-window)
      (let* ((edges (window-edges splash-window))
             (bottom (nth 3 edges))
             (left (nth 0 edges))
             (right (nth 2 edges)))
        (catch 'found-below
          (dolist (w (window-list nil 'no-minibuf))
            (unless (eq w splash-window)
              (let* ((w-edges (window-edges w))
                     (w-top (nth 1 w-edges))
                     (w-left (nth 0 w-edges))
                     (w-right (nth 2 w-edges)))
                ;; Check if w is below splash-window and overlaps horizontally
                ;; Skip popup windows as they are handled separately
                (when (and (= w-top bottom)
                           (> (min right w-right) (max left w-left))
                           (not (and (fboundp 'iota-popup--window-popup-p)
                                     (iota-popup--window-popup-p w))))
                  (throw 'found-below nil)))))
          t)))))

(defun iota-splash--should-show-separator-p ()
  "Return t if separator should be shown for splash screen.
Separator is shown when:
- Minibuffer is active, OR
- Popup is visible AND splash screen is at the bottom, OR
- Another iota splash/screen buffer is directly below."
  (or (iota-splash--minibuffer-active-p)
      (and (iota-splash--popup-visible-p)
           (iota-splash--window-at-bottom-p))
      (iota-splash--has-iota-buffer-below-p)))

(defun iota-splash--get-separator-line (window)
  "Get a separator line string for WINDOW.
Uses the configured box style from iota-modeline if available.
Uses inactive face since popup/minibuffer is active when this is called."
  (require 'iota-box)  ; Load on demand
  (let* ((width (if (window-live-p window)
                    (1- (window-body-width window))
                  79))
         (style (if (boundp 'iota-modeline-box-style)
                    iota-modeline-box-style
                  'rounded))
         ;; Always use inactive face for splash separator
         ;; (it's only shown when popup/minibuffer is active)
         (face 'iota-inactive-box-face))
    (iota-box-horizontal-line width style face)))

(defun iota-splash--update-separator ()
  "Update the separator line visibility for splash screen.
Shows separator when minibuffer or popup is active."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (let ((win (get-buffer-window buffer)))
        (when (window-live-p win)
          (if (iota-splash--should-show-separator-p)
              ;; Show separator line when minibuffer/popup is active
              (let ((width (1- (window-body-width win))))
                (with-current-buffer buffer
                  (setq-local mode-line-format
                              `(:eval (iota-splash--get-separator-line ,win))))
                ;; Critical: set window parameter to nil to SHOW mode-line
                (set-window-parameter win 'mode-line-format nil)
                ;; Force redisplay
                (force-mode-line-update t))
            ;; Hide separator line when not needed
            (with-current-buffer buffer
              (setq-local mode-line-format nil))
            ;; Set window parameter to 'none to HIDE mode-line
            (set-window-parameter win 'mode-line-format 'none)
            (force-mode-line-update t)))))))

(defun iota-splash--on-window-config-change ()
  "Handle window configuration changes (e.g., popup appearing).
This forces a redraw to ensure the splash screen stays centered."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer
               (buffer-live-p buffer)
               (get-buffer-window buffer))
      ;; Update separator line visibility
      (iota-splash--update-separator)
      ;; Force redraw on any window configuration change
      (iota-splash--redraw-buffer))))

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

(defvar iota-splash--cursor-hidden nil
  "Track cursor visibility state to avoid redundant calls.")

(defun iota-splash--restore-cursor ()
  "Restore cursor visibility when leaving splash screen."
  (when iota-splash--cursor-hidden
    (let ((win (selected-window)))
      (when (window-live-p win)
        (internal-show-cursor win t)
        (setq iota-splash--cursor-hidden nil)))))

(defun iota-splash--check-buffer-switch ()
  "Check buffer and manage cursor visibility accordingly.
Only updates cursor state when it actually changes to reduce flickering."
  (let ((splash-buf (get-buffer iota-splash-buffer-name))
        (current-buf (current-buffer))
        (win (selected-window)))
    (when (window-live-p win)
      (let ((should-hide (and splash-buf (eq current-buf splash-buf))))
        ;; Only call internal-show-cursor when state actually changes
        (when (not (eq should-hide iota-splash--cursor-hidden))
          (internal-show-cursor win (not should-hide))
          (setq iota-splash--cursor-hidden should-hide))))))

(defun iota-splash--cleanup-hooks ()
  "Remove all splash screen hooks and restore cursor."
  (remove-hook 'post-command-hook #'iota-splash--check-and-redraw)
  (remove-hook 'window-configuration-change-hook #'iota-splash--on-window-config-change)
  (remove-hook 'minibuffer-setup-hook #'iota-splash--on-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'iota-splash--on-minibuffer-exit)
  (remove-hook 'buffer-list-update-hook #'iota-splash--check-buffer-switch)
  (remove-hook 'echo-area-clear-hook #'iota-splash--on-echo-area-clear)
  ;; Restore cursor when cleaning up
  (iota-splash--restore-cursor))


(defun iota-splash--animate-step ()
  "Execute one step of the color animation."
  (let* ((num-colors (length iota-splash--animation-colors))
         (color1 (nth (% (+ iota-splash--animation-step 0) num-colors) iota-splash--animation-colors))
         (color2 (nth (% (+ iota-splash--animation-step 2) num-colors) iota-splash--animation-colors))
         (color3 (nth (% (+ iota-splash--animation-step 4) num-colors) iota-splash--animation-colors))
         (color4 (nth (% (+ iota-splash--animation-step 6) num-colors) iota-splash--animation-colors)))
    (set-face-foreground 'iota-splash-anim-1 color1)
    (set-face-foreground 'iota-splash-anim-2 color2)
    (set-face-foreground 'iota-splash-anim-3 color3)
    (set-face-foreground 'iota-splash-anim-4 color4)
    (setq iota-splash--animation-step (1+ iota-splash--animation-step))))

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
  ;; Restore cursor visibility in the window before killing buffer
  (when-let ((win (get-buffer-window iota-splash-buffer-name)))
    (internal-show-cursor win t))
  ;; Kill the buffer (not just bury it)
  (when (get-buffer iota-splash-buffer-name)
    (kill-buffer iota-splash-buffer-name))
  ;; Restore cursor in the new current window
  (internal-show-cursor (selected-window) t))

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
      ;; Now, populate and configure the buffer in the correct window
      (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Store initial window height
        (setq iota-splash--last-window-height (window-body-height))

        ;; Hide cursor IMMEDIATELY (before any content insertion)
        (setq-local cursor-type nil)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local visible-cursor nil)
        (internal-show-cursor (selected-window) nil)

        ;; Add hooks to stop timers and cleanup when buffer is killed
        (add-hook 'kill-buffer-hook #'iota-splash--stop-animation nil t)
        (add-hook 'kill-buffer-hook #'iota-splash--stop-hint-rotation nil t)
        (add-hook 'kill-buffer-hook #'iota-splash--cleanup-hooks nil t)

        ;; Layout Calculations (now in correct window context)
        (let* ((content-lines (+ 6  ; logo + footer + empty line + init-time
                                 (if iota-splash-show-hints 5 0)))
               (padding (max 0 (/ (- (window-body-height) content-lines) 2))))
          (dotimes (_ padding) (insert "\n")))

        ;; Insert Content
        (let ((logo-start (point)))
          (iota-splash--insert-tertiary)
          (iota-splash--insert-footer)
          (iota-splash--insert-init-time)
          ;; Center logo, tagline, and init time
          (let ((fill-column (window-width)))
            (center-region logo-start (point))))

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

        ;; Hide cursor in splash screen (multiple methods for robustness)
        (setq-local cursor-type nil)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local visible-cursor nil)
        ;; Additional cursor hiding methods
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

      ;; Hide cursor at window level (more reliable than buffer-local)
      (internal-show-cursor (selected-window) nil)

      ;; Clear echo area and set minibuffer window properties
      (message nil)
      (let ((minibuf-win (minibuffer-window)))
        (when (window-live-p minibuf-win)
          (set-window-parameter minibuf-win 'mode-line-format nil)
          (with-selected-window minibuf-win
            (setq-local mode-line-format nil))))

      ;; Add hooks to check for window size and configuration changes
      (add-hook 'post-command-hook #'iota-splash--check-and-redraw)
      (add-hook 'window-configuration-change-hook #'iota-splash--on-window-config-change)
      (add-hook 'minibuffer-setup-hook #'iota-splash--on-minibuffer-setup)
      (add-hook 'minibuffer-exit-hook #'iota-splash--on-minibuffer-exit)
      (add-hook 'buffer-list-update-hook #'iota-splash--check-buffer-switch)
      (add-hook 'echo-area-clear-hook #'iota-splash--on-echo-area-clear)

      ;; Start animations and hint rotation
      (iota-splash--start-animation)
      (iota-splash--start-hint-rotation)
      ;; Return the buffer
      buffer)))

(provide 'iota-splash)

;;; iota-splash.el ends here
