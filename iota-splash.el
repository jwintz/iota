;;; iota-splash.el --- I O T Λ splash screen -*- lexical-binding: t -*-

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

(defcustom iota-splash-buffer-name "*I O T Λ splash*"
  "Buffer name for IOTA splash screen."
  :type 'string
  :group 'iota)

(defcustom iota-splash-show-key-bindings nil
  "Show key bindings in splash screen when project.el is available.
When non-nil and project.el is loaded, display helpful key bindings
for project management."
  :type 'boolean
  :group 'iota)

(defcustom iota-splash-show-hints nil
  "Show animated hints about IOTA features in splash screen.
When non-nil, display rotating hints about IOTA functionality."
  :type 'boolean
  :group 'iota)

(defcustom iota-splash-project-prefix nil
  "Project prefix key sequence to display in splash screen.
When nil, automatically detect the prefix by searching for project-prefix-map.
Set this to a string like \"C-c p\" to override automatic detection."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Custom prefix"))
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

(defconst iota-splash--hints
  '("Enable modal editing with M-x iota-modal-mode (ESC for COMMAND, i for INSERT)"
    "Learn modal keys with M-x iota-tutorial - an interactive guide"
    "Use comma (,) as leader key in COMMAND mode: , f for files, , b for buffers"
    "Press M-x iota-modeline-cycle-style to change box styles"
    "Use M-x iota-modeline-toggle-position to switch between header and mode line"
    "Try M-x iota-modeline-cycle-preset to explore segment presets"
    "Modeline segments automatically adapt to narrow windows"
    "Enable terminal transparency with iota-theme-transparent-in-terminal"
    "Run M-x iota-demo to explore all I O T Λ widgets and features"
    "I O T Λ animations use 30fps for smooth visual feedback"
    "I O T Λ automatically adapts to your theme's colors"
    "Create custom segments with iota-segment-simple or iota-segment-dynamic"
    "I O T Λ works in both terminal and GUI Emacs")
  "List of hints about I O T Λ features.")

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

(defun iota-splash--project-available-p ()
  "Return t if project.el is available."
  (featurep 'project))

(defun iota-splash--find-prefix-for-command (command)
  "Find the prefix key sequence for COMMAND by removing its last key."
  (let ((keys (where-is-internal command nil t)))
    (when keys
      (let ((full-key (key-description keys)))
        ;; Remove the last key to get prefix (e.g., "C-c p f" -> "C-c p")
        (cond
         ((string-match "\\`\\(.*[^ ]\\) [^ ]\\'" full-key)
          (match-string 1 full-key))
         ((and (> (length full-key) 2)
               (eq (aref full-key (- (length full-key) 2)) ?\s))
          (substring full-key 0 (- (length full-key) 2)))
         (t nil))))))

(defun iota-splash--search-keymap-for-prefix (keymap target-map)
  "Search KEYMAP for a key sequence bound to TARGET-MAP.
Returns the key description if found, nil otherwise."
  (let ((result nil))
    (map-keymap
     (lambda (key binding)
       (when (and (not result) (eq binding target-map))
         (setq result (key-description (vector key)))))
     keymap)
    result))

(defun iota-splash--binding-activates-keymap-p (binding target-map)
  "Check if BINDING would activate TARGET-MAP.
This handles both direct keymap bindings and autoload wrappers."
  (or
   ;; Direct keymap binding
   (eq binding target-map)
   ;; Check if it's a symbol whose function cell is the keymap
   (and (symbolp binding)
        (eq (symbol-function binding) target-map))
   ;; Check if calling the binding would return the keymap
   ;; (for use-package autoload wrappers)
   (and (or (functionp binding) (symbolp binding))
        (condition-case nil
            (eq (symbol-value (intern-soft "project-prefix-map")) target-map)
          (error nil)))))

(defun iota-splash--find-keymap-binding (target-map)
  "Find the key sequence that TARGET-MAP is bound to in global-map.
Returns a string like \"C-c p\" or nil if not found.
Handles both direct keymap bindings and use-package autoload wrappers."
  (catch 'found
    ;; Search through common prefix keys (prioritize C-c for custom bindings)
    (dolist (prefix-key '("C-c" "C-x" "C-z" "s-p" "M-p"))
      (let ((prefix-map (ignore-errors (lookup-key global-map (kbd prefix-key)))))
        (when (keymapp prefix-map)
          ;; Check all possible single character suffixes
          (catch 'found-suffix
            (map-keymap
             (lambda (key binding)
               (let* ((key-desc (key-description (vector key)))
                      (full-key (concat prefix-key " " key-desc))
                      ;; For autoload bindings, check what's actually at that key
                      (actual-binding (lookup-key global-map (kbd full-key)))
                      ;; Check if this key leads to project commands
                      (leads-to-project (and (or (keymapp actual-binding)
                                                 (functionp binding))
                                             ;; Check if common project commands are under this prefix
                                             (let ((test-key (concat full-key " f")))
                                               (eq (lookup-key global-map (kbd test-key) t)
                                                   'project-find-file))))
                      (is-match (or (eq binding target-map)
                                   (eq actual-binding target-map)
                                   leads-to-project
                                   ;; Check for use-package autoload pattern
                                   (and (symbolp binding)
                                        (string-match-p "project" (symbol-name binding))))))
                 (when is-match
                   (throw 'found full-key))))
             prefix-map)))))

    ;; Also try direct single-key bindings (less common but possible)
    (let ((suffix (iota-splash--search-keymap-for-prefix global-map target-map)))
      (when suffix
        (throw 'found suffix)))
    nil))

(defun iota-splash--get-project-prefix ()
  "Get the project prefix key sequence as a string.
Tries multiple methods to detect the actual binding."
  (or
   ;; First, check if user has set a manual override
   iota-splash-project-prefix

   ;; Search for project-prefix-map binding first (most authoritative)
   ;; This detects when user has bound the entire keymap with :bind-keymap
   (when (boundp 'project-prefix-map)
     (iota-splash--find-keymap-binding project-prefix-map))

   ;; If keymap binding not found, try to find prefix from individual commands
   (or (iota-splash--find-prefix-for-command 'project-find-file)
       (iota-splash--find-prefix-for-command 'project-switch-project)
       (iota-splash--find-prefix-for-command 'project-find-regexp))

   ;; Default fallback
   "C-x p"))

(defun iota-splash--insert-key-bindings (&optional window)
  "Insert key bindings section if project.el is available.
WINDOW is the window to use for width calculations (defaults to selected window)."
  (when (and iota-splash-show-key-bindings
             (iota-splash--project-available-p))
    (insert "\n\n\n\n")  ; More vertical space before Quick Actions

    ;; Center the "Quick Actions in [CWD]" header
    (let ((header-start (point))
          (cwd (abbreviate-file-name default-directory)))
      (let ((cwd-str (if (> (length cwd) 40)
                         (concat "..." (substring cwd -37))
                       cwd)))
        (insert (propertize "Quick Actions" 'face 'iota-splash-logo-accent))
        (insert (propertize " in " 'face 'default))
        (insert (propertize cwd-str 'face 'iota-splash-logo-primary)))
      (insert "\n")
      (let ((fill-column (window-width (or window (selected-window)))))
        (center-region header-start (point))))

    (insert "\n")

    (let* ((prefix (iota-splash--get-project-prefix))
           (project-bindings `((,(concat prefix " f") "Find file in project")
                               (,(concat prefix " p") "Switch project")
                               (,(concat prefix " d") "Dired in project root")
                               (,(concat prefix " g") "Search project (grep)")))
           (magit-bindings '(("C-c v v" "Magit status")
                             ("C-c v l" "Magit log")
                             ("C-c v d" "Magit diff")
                             ("C-c v c" "Magit commit")))
           (all-bindings (append project-bindings magit-bindings))
           (max-key-width (apply 'max (mapcar (lambda (b) (length (car b))) all-bindings)))
           ;; Calculate the width of the longest line
           (max-line-width (apply 'max
                                  (mapcar (lambda (b)
                                            (+ max-key-width 2 (length (cadr b))))
                                          all-bindings)))
           ;; Calculate left margin to center the block
           (left-margin (max 0 (/ (- (window-width (or window (selected-window))) max-line-width) 2))))
      ;; Insert project bindings
      (dolist (binding project-bindings)
        (let* ((key (car binding))
               (desc (cadr binding))
               (key-padding (make-string (- max-key-width (length key)) ?\s)))
          (insert (make-string left-margin ?\s))
          (insert (propertize key 'face 'iota-splash-logo-primary))
          (insert key-padding)
          (insert "  ")
          (insert desc)
          (insert "\n")))
      ;; Empty line between groups
      (insert "\n")
      ;; Insert magit bindings
      (dolist (binding magit-bindings)
        (let* ((key (car binding))
               (desc (cadr binding))
               (key-padding (make-string (- max-key-width (length key)) ?\s)))
          (insert (make-string left-margin ?\s))
          (insert (propertize key 'face 'iota-splash-logo-primary))
          (insert key-padding)
          (insert "  ")
          (insert desc)
          (insert "\n"))))))

(defun iota-splash--get-current-hint ()
  "Get the current hint text."
  (nth iota-splash--hint-index iota-splash--hints))

(defun iota-splash--insert-hints ()
  "Insert hints section if enabled."
  (when iota-splash-show-hints
    (insert "\n\n\n\n")  ; Add spacing to push hints lower
    (insert (propertize (iota-splash--get-current-hint) 'face 'iota-splash-tertiary))
    (insert "\n")))

(defun iota-splash--rotate-hint ()
  "Rotate to the next hint and update the splash screen buffer."
  (setq iota-splash--hint-index
        (mod (1+ iota-splash--hint-index) (length iota-splash--hints)))
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
                                           (if (and iota-splash-show-key-bindings
                                                    (iota-splash--project-available-p))
                                               16 0)  ; 8 bindings + 1 separator + header + spacing
                                           (if iota-splash-show-hints 5 0)))
                         (padding (max 0 (/ (- current-height content-lines) 2))))
                    (dotimes (_ padding) (insert "\n")))
                  (let ((logo-start (point)))
                    (iota-splash--insert-tertiary)
                    (iota-splash--insert-footer)
                    (iota-splash--insert-init-time)
                    (let ((fill-column (window-width window)))
                      (center-region logo-start (point))))

                  (iota-splash--insert-key-bindings window)

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

(defun iota-splash--which-key-visible-p ()
  "Return t if a which-key window is currently visible."
  (cl-some (lambda (win)
             (let ((buf-name (buffer-name (window-buffer win))))
               (or (string-prefix-p " *which-key*" buf-name)
                   (string-prefix-p "*which-key*" buf-name))))
           (window-list)))

(defun iota-splash--get-separator-line (window)
  "Get a separator line string for WINDOW.
Uses the configured box style from iota-modeline if available.
Uses the same face as the modeline box for consistency."
  (require 'iota-box)  ; Load on demand
  (let* ((width (1- (window-body-width window)))
         (style (if (boundp 'iota-modeline-box-style)
                    iota-modeline-box-style
                  'rounded))
         ;; Use same face as modeline box for consistency
         (face (if (fboundp 'iota-theme-get-box-face)
                   (iota-theme-get-box-face window)
                 'iota-active-box-face)))
    (iota-box-horizontal-line width style face)))

(defun iota-splash--update-separator ()
  "Update the separator line visibility for splash screen.
Shows separator when minibuffer or which-key is active."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (let ((win (get-buffer-window buffer)))
        (when (window-live-p win)
          (if (or (iota-splash--minibuffer-active-p)
                  (iota-splash--which-key-visible-p))
              ;; Show separator line when minibuffer/which-key is active
              ;; Use :propertize to override mode-line face and remove underline
              (with-current-buffer buffer
                (setq-local mode-line-format
                            '(:propertize (:eval (iota-splash--get-separator-line (selected-window)))
                                          face (:underline nil :overline nil))))
            ;; Hide separator line when not needed
            (with-current-buffer buffer
              (setq-local mode-line-format nil)))
          ;; Update window parameter as well
          (if (or (iota-splash--minibuffer-active-p)
                  (iota-splash--which-key-visible-p))
              (set-window-parameter win 'mode-line-format nil)
            (set-window-parameter win 'mode-line-format 'none)))))))

(defun iota-splash--on-window-config-change ()
  "Handle window configuration changes (e.g., which-key appearing).
This forces a redraw to ensure the splash screen stays centered."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer
               (buffer-live-p buffer)
               (get-buffer-window buffer))
      ;; Update separator line visibility
      (iota-splash--update-separator)
      ;; Force redraw on any window configuration change
      (iota-splash--redraw-buffer)
      ;; Ensure cursor stays hidden after window config changes
      (let ((win (get-buffer-window buffer)))
        (when win
          (internal-show-cursor win nil))))))

(defun iota-splash--on-minibuffer-setup ()
  "Handle minibuffer setup to show separator and redraw splash screen."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer
               (buffer-live-p buffer)
               (get-buffer-window buffer))
      ;; Show separator line when minibuffer is active
      (iota-splash--update-separator)
      ;; Force redraw when minibuffer opens
      (iota-splash--redraw-buffer)
      ;; Ensure cursor stays hidden
      (let ((win (get-buffer-window buffer)))
        (when win
          (internal-show-cursor win nil))))))

(defun iota-splash--on-minibuffer-exit ()
  "Handle minibuffer exit to hide separator and redraw splash screen."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer
               (buffer-live-p buffer)
               (get-buffer-window buffer))
      ;; Schedule separator update after minibuffer is fully closed
      (run-with-timer 0.01 nil #'iota-splash--update-separator)
      ;; Force redraw when minibuffer closes
      (run-with-timer 0.02 nil #'iota-splash--redraw-buffer)
      ;; Ensure cursor stays hidden
      (let ((win (get-buffer-window buffer)))
        (when win
          (internal-show-cursor win nil))))))

(defun iota-splash--on-echo-area-clear ()
  "Handle echo area clear to redraw splash screen."
  (let ((buffer (get-buffer iota-splash-buffer-name)))
    (when (and buffer
               (buffer-live-p buffer)
               (get-buffer-window buffer))
      ;; Update separator and redraw
      (iota-splash--update-separator)
      (iota-splash--redraw-buffer)
      ;; Ensure cursor stays hidden
      (let ((win (get-buffer-window buffer)))
        (when win
          (internal-show-cursor win nil))))))

(defun iota-splash--restore-cursor ()
  "Restore cursor visibility when leaving splash screen."
  (let ((win (selected-window)))
    (when (window-live-p win)
      (internal-show-cursor win t))))

(defun iota-splash--check-buffer-switch ()
  "Check buffer and manage cursor visibility accordingly."
  (let ((splash-buf (get-buffer iota-splash-buffer-name))
        (current-buf (current-buffer))
        (win (selected-window)))
    (when (window-live-p win)
      (if (and splash-buf (eq current-buf splash-buf))
          ;; In splash buffer - hide cursor
          (internal-show-cursor win nil)
        ;; Not in splash buffer - show cursor
        (internal-show-cursor win t)))))

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
  "Start the splash screen animation timer."
  (setq iota-splash--animation-step 0)
  ;; Use centralized timer registry (load on demand)
  (require 'iota-timers)
  (iota-timers-cancel 'splash-animation)
  (iota-timers-run-with-timer 'splash-animation 0 0.1 #'iota-splash--animate-step))

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

(defun iota-splash-screen ()
  "Display IOTA splash screen with branding.
Does not display if Emacs was opened with file arguments."
  (interactive)
  ;; Skip splash screen if files were opened
  (unless (and (not (called-interactively-p 'any))
               (iota-splash--file-buffers-exist-p))
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

        ;; Add hooks to stop timers and cleanup when buffer is killed
        (add-hook 'kill-buffer-hook #'iota-splash--stop-animation nil t)
        (add-hook 'kill-buffer-hook #'iota-splash--stop-hint-rotation nil t)
        (add-hook 'kill-buffer-hook #'iota-splash--cleanup-hooks nil t)

        ;; Layout Calculations (now in correct window context)
        (let* ((content-lines (+ 6  ; logo + footer + empty line + init-time
                                 (if (and iota-splash-show-key-bindings
                                          (iota-splash--project-available-p))
                                     16 0)  ; 8 bindings + 1 separator + header + spacing
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

        (iota-splash--insert-key-bindings)

        (let ((hints-start (point)))
          (iota-splash--insert-hints)
          ;; Center hints only
          (let ((fill-column (window-width)))
            (center-region hints-start (point))))

        ;; Finalize Buffer State
        (read-only-mode 1)
        (local-set-key (kbd "q") 'quit-window)
        (local-set-key (kbd "RET") 'quit-window)

        ;; Enable modalka for command mode in splash screen
        ;; This allows x f, x b, etc. to work
        (when (and (boundp 'iota-modal-mode) iota-modal-mode
                   (fboundp 'modalka-mode))
          (modalka-mode 1))

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
