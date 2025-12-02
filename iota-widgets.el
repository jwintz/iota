;;; iota-widgets.el --- Widget library for I O T Λ -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: widgets, tui
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Widget library for I O T Λ (I Ø T Δ) TUI framework.
;; Provides modern terminal widgets: progress bars, spinners, trees,
;; tabs, cards, gauges, tables, forms, and interactive elements.
;;
;; All widgets support:
;; - Unicode box-drawing and block characters
;; - Animation via iota-animate framework
;; - Emacs widget.el integration
;; - Theming via iota faces

;;; Code:

(require 'cl-lib)
(require 'iota-box)
(require 'iota-faces)
(require 'widget)
(require 'wid-edit)

;; Forward declarations to avoid circular dependencies
(declare-function iota-animate-start "iota-animate")
(declare-function iota-animate-stop "iota-animate")
(declare-function iota-animate-lerp "iota-animate")
(defvar iota-animate-enabled)

;;; Customization Group

(defgroup iota-widgets nil
  "IOTA TUI widget library."
  :group 'iota
  :prefix "iota-widget-")

;;; Unicode Glyph Constants

(defconst iota-widget-blocks
  '((full      . "█")
    (dark      . "▓")
    (medium    . "▒")
    (light     . "░")
    (empty     . " ")
    ;; Vertical eighths (bottom to top)
    (v-1/8     . "▁")
    (v-2/8     . "▂")
    (v-3/8     . "▃")
    (v-4/8     . "▄")
    (v-5/8     . "▅")
    (v-6/8     . "▆")
    (v-7/8     . "▇")
    (v-full    . "█")
    ;; Horizontal eighths (left to right)
    (h-1/8     . "▏")
    (h-2/8     . "▎")
    (h-3/8     . "▍")
    (h-4/8     . "▌")
    (h-5/8     . "▋")
    (h-6/8     . "▊")
    (h-7/8     . "▉")
    (h-full    . "█")
    ;; Half blocks
    (left      . "▌")
    (right     . "▐")
    (top       . "▀")
    (bottom    . "▄")
    ;; Quadrants
    (quad-tl   . "▘")
    (quad-tr   . "▝")
    (quad-bl   . "▖")
    (quad-br   . "▗")
    (quad-v    . "▚")
    (quad-h    . "▞"))
  "Unicode block characters for widget rendering.")

(defconst iota-widget-arrows
  '((right     . "❯")
    (left      . "❮")
    (up        . "▲")
    (down      . "▼")
    (right-sm  . "▸")
    (left-sm   . "◂")
    (up-sm     . "▴")
    (down-sm   . "▾")
    (expand    . "▶")
    (collapse  . "▼")
    (bullet    . "●")
    (circle    . "○")
    (dot       . "•")
    (diamond   . "◆")
    (diamond-o . "◇")
    (check     . "✓")
    (cross     . "✗")
    (star      . "★")
    (star-o    . "☆"))
  "Unicode arrow and indicator characters.")

(defconst iota-widget-braille
  '((empty     . "⠀")
    (dot-1     . "⠁")
    (dot-2     . "⠂")
    (dot-12    . "⠃")
    (full      . "⣿")
    (left      . "⡇")
    (right     . "⢸")
    (top       . "⠛")
    (bottom    . "⣤"))
  "Braille characters for fine-grained graphics.")

;;; Progress Bars

(defcustom iota-widget-progress-styles
  '((blocks    "█" "░" "▓")     ; Filled, empty, partial
    (gradient  "█" "░" "▓")     ; Same but uses color gradient
    (shaded    "█" " " "▒")     ; With shading
    (smooth    "█" "░" ("▏" "▎" "▍" "▌" "▋" "▊" "▉")) ; Sub-character precision
    (circles   "●" "○" "◐")
    (squares   "■" "□" "▪")
    (arrows    "▶" "▷" "▸")
    (diamonds  "◆" "◇" "◈")
    (braille   "⣿" "⠀" "⣤")
    (line      "━" "┄" "╌")
    (line-thin "─" "┄" "╌")
    (dots      "•" "·" "∙")
    (pipes     "┃" "│" "╏")
    (modern    "█" "▁" "▄"))
  "Progress bar style definitions.
Each style is (name filled empty partial-or-list).
If partial is a list, it provides sub-character precision."
  :type '(alist :key-type symbol
                :value-type (list string string (choice string (repeat string))))
  :group 'iota-widgets)

(cl-defun iota-widget-progress-bar (value total &key width style label show-percent
                                          gradient smooth caps)
  "Render a progress bar with modern terminal aesthetics.

Arguments:
  value          Current progress value
  total          Maximum value
  :width         Width of bar (default 40)
  :style         Style: blocks, gradient, shaded, smooth, circles, etc.
  :label         Optional label to show before bar
  :show-percent  Show percentage (default t)
  :gradient      List of colors for gradient effect (e.g., \\='(\"#ff0000\" \"#00ff00\"))
  :smooth        Use sub-character precision for smooth animation
  :caps          Add end caps: rounded, square, arrow, none (default none)

Returns: Propertized string representation of progress bar."
  (let* ((width (or width 40))
         (style (or style 'blocks))
         (show-percent (if (null show-percent) t show-percent))
         (chars (or (alist-get style iota-widget-progress-styles)
                    (alist-get 'blocks iota-widget-progress-styles)))
         (filled-char (nth 0 chars))
         (empty-char (nth 1 chars))
         (partial-chars (nth 2 chars))
         (percent (if (zerop total) 0 (/ (* value 100.0) total)))
         (progress-ratio (/ (float value) (max 1 total)))
         (exact-filled (* width progress-ratio))
         (filled-width (floor exact-filled))
         (partial-amount (- exact-filled filled-width))
         (empty-width (max 0 (- width filled-width (if (and smooth (> partial-amount 0)) 1 0))))
         ;; Caps
         (left-cap (pcase caps
                     ('rounded "╭")
                     ('square "┌")
                     ('arrow "")
                     (_ "")))
         (right-cap (pcase caps
                      ('rounded "╮")
                      ('square "┐")
                      ('arrow "▶")
                      (_ ""))))
    (concat
     (when label 
       (propertize (concat label " ") 'face 'iota-muted-face))
     left-cap
     (cond
      ;; Gradient mode - each character gets a different color
      (gradient
       (iota-widget--progress-gradient filled-width empty-width
                                        filled-char empty-char gradient))
      ;; Smooth mode - sub-character precision
      ((and smooth (listp partial-chars))
       (iota-widget--progress-smooth filled-width partial-amount empty-width
                                      filled-char empty-char partial-chars))
      ;; Standard mode
      (t
       (concat
        (propertize (make-string filled-width (string-to-char filled-char))
                    'face 'iota-accent-face)
        (propertize (make-string empty-width (string-to-char empty-char))
                    'face 'iota-muted-face))))
     right-cap
     (when show-percent
       (propertize (format " %3.0f%%" percent) 'face 'iota-muted-face)))))

(defun iota-widget--progress-gradient (filled empty fill-char empty-char colors)
  "Render gradient progress bar with COLORS across FILLED width."
  (let* ((num-colors (length colors))
         (filled-str
          (if (zerop filled)
              ""
            (mapconcat
             (lambda (i)
               (let* ((color-idx (floor (* (/ (float i) (max 1 (1- filled))) 
                                           (1- num-colors))))
                      (color (nth (min color-idx (1- num-colors)) colors)))
                 (propertize fill-char 'face `(:foreground ,color))))
             (number-sequence 0 (1- filled))
             ""))))
    (concat filled-str
            (propertize (make-string empty (string-to-char empty-char))
                        'face 'iota-muted-face))))

(defun iota-widget--progress-smooth (filled partial-amount empty filled-char empty-char partial-chars)
  "Render smooth progress bar with sub-character precision."
  (let* ((num-partials (length partial-chars))
         (partial-idx (floor (* partial-amount num-partials)))
         (partial-char (if (and (> partial-amount 0) (< partial-idx num-partials))
                           (nth partial-idx partial-chars)
                         "")))
    (concat
     (propertize (make-string filled (string-to-char filled-char))
                 'face 'iota-accent-face)
     (when (> (length partial-char) 0)
       (propertize partial-char 'face 'iota-accent-face))
     (propertize (make-string empty (string-to-char empty-char))
                 'face 'iota-muted-face))))

;;; Animated Progress Bar

(defvar-local iota-widget--progress-overlays nil
  "Alist of (id . overlay) for animated progress bars.")

(defvar iota-widget--progress-animations nil
  "Alist of (id . animation-id) for progress animations.")

(cl-defun iota-widget-progress-bar-animated (&key id from to duration width style
                                                   label on-complete)
  "Create an animated progress bar.

Arguments:
  :id          Unique identifier for this progress bar
  :from        Starting value (default 0)
  :to          Target value
  :duration    Animation duration in seconds (default 1.0)
  :width       Bar width (default 40)
  :style       Progress bar style
  :label       Label text
  :on-complete Callback when animation completes

Returns: Overlay marker for the progress bar."
  (require 'iota-animate)
  (let* ((id (or id (gensym "progress-")))
         (from (or from 0))
         (to (or to 100))
         (duration (or duration 1.0))
         (marker (point-marker))
         (ov (make-overlay (point) (1+ (point)))))
    ;; Initialize overlay
    (overlay-put ov 'iota-widget 'progress-bar)
    (overlay-put ov 'iota-widget-id id)
    (overlay-put ov 'display (iota-widget-progress-bar from to
                                                        :width width
                                                        :style style
                                                        :label label))
    ;; Store overlay reference
    (push (cons id ov) iota-widget--progress-overlays)
    ;; Start animation
    (when (and (boundp 'iota-animate-enabled) iota-animate-enabled)
      (let ((anim-id (iota-animate-start
                      duration
                      (lambda (progress)
                        (when (overlay-buffer ov)
                          (let ((current (+ from (* progress (- to from)))))
                            (overlay-put ov 'display
                                         (iota-widget-progress-bar current to
                                                                    :width width
                                                                    :style style
                                                                    :label label)))))
                      nil
                      (lambda ()
                        (when on-complete (funcall on-complete))))))
        (push (cons id anim-id) iota-widget--progress-animations)))
    marker))

;;; Indeterminate Progress (Spinner/Throbber)

(defcustom iota-widget-spinner-styles
  '((braille   . ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
    (dots      . ["⠁" "⠂" "⠄" "⡀" "⢀" "⠠" "⠐" "⠈"])
    (line      . ["─" "\\" "│" "/"])
    (arc       . ["◜" "◠" "◝" "◞" "◡" "◟"])
    (box       . ["◰" "◳" "◲" "◱"])
    (circle    . ["◐" "◓" "◑" "◒"])
    (arrows    . ["←" "↖" "↑" "↗" "→" "↘" "↓" "↙"])
    (bounce    . ["⠁" "⠂" "⠄" "⠂"])
    (grow      . ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" "▇" "▆" "▅" "▄" "▃" "▂"])
    (pulse     . ["█" "▓" "▒" "░" "▒" "▓"])
    (snake     . ["⠏" "⠛" "⠹" "⢸" "⣰" "⣤" "⣆" "⡇"])
    (moon      . ["🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"])
    (clock     . ["🕐" "🕑" "🕒" "🕓" "🕔" "🕕" "🕖" "🕗" "🕘" "🕙" "🕚" "🕛"])
    (bar       . ["▏" "▎" "▍" "▌" "▋" "▊" "▉" "█" "▉" "▊" "▋" "▌" "▍" "▎"]))
  "Spinner animation frame definitions."
  :type '(alist :key-type symbol :value-type vector)
  :group 'iota-widgets)

(defvar iota-widget--spinner-timers nil
  "Alist of (id . timer) for active spinners.")

(defvar iota-widget--spinner-overlays nil
  "Alist of (id . overlay) for spinner displays.")

(cl-defun iota-widget-spinner-create (&key id style label interval face)
  "Create an animated spinner widget.

Arguments:
  :id       Unique identifier
  :style    Spinner style (braille, dots, line, arc, box, circle, etc.)
  :label    Text to show after spinner
  :interval Animation interval in seconds (default 0.1)
  :face     Face for spinner character

Returns: Marker at spinner position."
  (let* ((id (or id (gensym "spinner-")))
         (style (or style 'braille))
         (interval (or interval 0.1))
         (face (or face 'iota-accent-face))
         (frames (or (alist-get style iota-widget-spinner-styles)
                     (alist-get 'braille iota-widget-spinner-styles)))
         (frame-idx 0)
         (start-pos (point))
         ov marker)
    ;; Insert placeholder character for the overlay
    (insert " ")
    (setq marker (copy-marker start-pos))
    (setq ov (make-overlay start-pos (point)))
    ;; Initialize overlay
    (overlay-put ov 'iota-widget 'spinner)
    (overlay-put ov 'iota-widget-id id)
    (overlay-put ov 'evaporate t)
    ;; Initial display
    (overlay-put ov 'display
                 (concat (propertize (aref frames 0) 'face face)
                         (when label (concat " " label))))
    ;; Store overlay
    (push (cons id ov) iota-widget--spinner-overlays)
    ;; Start animation timer
    (let ((timer (run-with-timer
                  0 interval
                  (lambda ()
                    (when (and ov (overlayp ov) (overlay-buffer ov))
                      (setq frame-idx (mod (1+ frame-idx) (length frames)))
                      (overlay-put ov 'display
                                   (concat (propertize (aref frames frame-idx) 'face face)
                                           (when label (concat " " label)))))))))
      (push (cons id timer) iota-widget--spinner-timers))
    marker))

(defun iota-widget-spinner-stop (id)
  "Stop spinner with ID."
  (when-let ((timer (alist-get id iota-widget--spinner-timers)))
    (cancel-timer timer)
    (setq iota-widget--spinner-timers
          (assq-delete-all id iota-widget--spinner-timers)))
  (when-let ((ov (alist-get id iota-widget--spinner-overlays)))
    (when (overlayp ov)
      (delete-overlay ov))
    (setq iota-widget--spinner-overlays
          (assq-delete-all id iota-widget--spinner-overlays))))

(defun iota-widget-spinner-stop-all ()
  "Stop all active spinners."
  (dolist (entry iota-widget--spinner-timers)
    (cancel-timer (cdr entry)))
  (dolist (entry iota-widget--spinner-overlays)
    (when (overlayp (cdr entry))
      (delete-overlay (cdr entry))))
  (setq iota-widget--spinner-timers nil
        iota-widget--spinner-overlays nil))

;;; Indeterminate Progress Bar (Marquee/Throbber)

(cl-defun iota-widget-progress-indeterminate (&key id width style label interval)
  "Create an indeterminate (marquee) progress bar.

Arguments:
  :id       Unique identifier
  :width    Width of the bar (default 20)
  :style    Style: bounce, snake, pulse (default bounce)
  :label    Label text
  :interval Animation interval (default 0.1)

Returns: Marker at progress bar position."
  (let* ((id (or id (gensym "indeterminate-")))
         (width (or width 20))
         (style (or style 'bounce))
         (interval (or interval 0.1))
         (start-pos (point))
         ov marker
         (pos 0)
         (direction 1)
         (segment-width 4))
    ;; Insert placeholder
    (insert " ")
    (setq marker (copy-marker start-pos))
    (setq ov (make-overlay start-pos (point)))
    ;; Initialize overlay
    (overlay-put ov 'iota-widget 'progress-indeterminate)
    (overlay-put ov 'iota-widget-id id)
    (overlay-put ov 'evaporate t)
    ;; Store overlay
    (push (cons id ov) iota-widget--spinner-overlays)
    ;; Animation timer
    (let ((timer (run-with-timer
                  0 interval
                  (lambda ()
                    (when (and ov (overlayp ov) (overlay-buffer ov))
                      (let* ((bar (make-string width ?░))
                             (end-pos (min (+ pos segment-width) width)))
                        ;; Draw moving segment
                        (dotimes (i (- end-pos pos))
                          (when (< (+ pos i) width)
                            (aset bar (+ pos i) ?█)))
                        ;; Update position
                        (pcase style
                          ('bounce
                           (setq pos (+ pos direction))
                           (when (or (>= pos (- width segment-width)) (<= pos 0))
                             (setq direction (- direction))))
                          ('snake
                           (setq pos (mod (1+ pos) width)))
                          ('pulse
                           (setq pos (mod (1+ pos) width))))
                        (overlay-put ov 'display
                                     (concat (when label (concat label " "))
                                             (propertize bar 'face 'iota-muted-face)))))))))
      (push (cons id timer) iota-widget--spinner-timers))
    marker))

;;; Vertical Gauge Widget

(cl-defun iota-widget-gauge-vertical (value total &key height label show-value
                                             style colors position)
  "Render a vertical gauge/bar.

Arguments:
  value       Current value
  total       Maximum value
  :height     Height in lines (default 8)
  :label      Label below gauge
  :show-value Show numeric value (default t)
  :style      Style: blocks, gradient, thermometer
  :colors     List of colors from bottom to top
  :position   Value position: top, bottom, inside (default top)

Returns: List of strings (one per line, top to bottom)."
  (let* ((height (or height 8))
         (show-value (if (null show-value) t show-value))
         (style (or style 'blocks))
         (percent (if (zerop total) 0 (/ (float value) total)))
         (filled-height (round (* height percent)))
         (lines '()))
    ;; Value display at top
    (when (and show-value (eq position 'top))
      (push (propertize (format "%3d%%" (round (* percent 100)))
                        'face 'iota-accent-face)
            lines))
    ;; Top cap
    (push (propertize "╭─╮" 'face 'iota-box-face) lines)
    ;; Gauge body
    (dotimes (i height)
      (let* ((row-from-bottom (- height i 1))
             (filled (< row-from-bottom filled-height))
             (char (if filled "█" " "))
             (face (cond
                    ((and filled colors)
                     (let* ((color-idx (floor (* (/ (float row-from-bottom) height)
                                                  (length colors))))
                            (color (nth (min color-idx (1- (length colors))) colors)))
                       `(:foreground ,color)))
                    (filled 'iota-accent-face)
                    (t 'iota-muted-face))))
        (push (concat (propertize "│" 'face 'iota-box-face)
                      (propertize char 'face face)
                      (propertize "│" 'face 'iota-box-face))
              lines)))
    ;; Bottom cap
    (push (propertize "╰─╯" 'face 'iota-box-face) lines)
    ;; Label
    (when label
      (push (propertize (iota-box-pad label 3 'center) 'face 'iota-muted-face) lines))
    ;; Value at bottom
    (when (and show-value (or (null position) (eq position 'bottom)))
      (push (propertize (format "%3d%%" (round (* percent 100)))
                        'face 'iota-accent-face)
            lines))
    (nreverse lines)))

(defun iota-widget-gauge-vertical-insert (value total &rest args)
  "Insert a vertical gauge at point.
See `iota-widget-gauge-vertical' for arguments."
  (let ((lines (apply #'iota-widget-gauge-vertical value total args)))
    (dolist (line lines)
      (insert line "\n"))))

;;; Multiple Gauges Side by Side

(cl-defun iota-widget-gauge-group (gauges &key spacing)
  "Render multiple vertical gauges side by side.

GAUGES is a list of plists with :value :total :label keys.
SPACING is space between gauges (default 2).

Returns: List of strings (one per line)."
  (let* ((spacing (or spacing 2))
         (spacer (make-string spacing ?\s))
         (rendered (mapcar (lambda (g)
                             (iota-widget-gauge-vertical
                              (plist-get g :value)
                              (plist-get g :total)
                              :label (plist-get g :label)
                              :height (or (plist-get g :height) 8)
                              :colors (plist-get g :colors)))
                           gauges))
         (max-lines (apply #'max (mapcar #'length rendered)))
         (lines '()))
    ;; Pad all gauge outputs to same height
    (setq rendered
          (mapcar (lambda (g)
                    (let ((pad (- max-lines (length g))))
                      (append (make-list pad "   ") g)))
                  rendered))
    ;; Combine horizontally
    (dotimes (i max-lines)
      (push (mapconcat (lambda (g) (or (nth i g) "   "))
                       rendered
                       spacer)
            lines))
    (nreverse lines)))

;;; Tree Widget

(defconst iota-widget-tree-chars
  '((branch    . "├── ")
    (last      . "└── ")
    (vertical  . "│   ")
    (space     . "    ")
    (expanded  . "[-] ")
    (collapsed . "[+] ")
    (leaf      . "    ")
    (file      . "")
    (folder    . "")
    (folder-open . ""))
  "Characters for tree rendering.")

(cl-defstruct (iota-tree-node
               (:constructor iota-tree-node-create)
               (:copier nil))
  "Tree node structure."
  label
  children
  expanded
  icon
  face
  data)

(cl-defun iota-widget-tree (root &key indent-guides show-icons)
  "Render a tree structure.

ROOT is an `iota-tree-node' or nested alist.
:indent-guides  Show vertical indent guides (default t)
:show-icons     Show node icons (default nil, uses ASCII)

Returns: Multi-line string."
  (let ((indent-guides (if (null indent-guides) t indent-guides))
        (lines '()))
    ;; Render root node first (without branch char)
    (let* ((is-struct (iota-tree-node-p root))
           (label (if is-struct (iota-tree-node-label root) (car root)))
           (children (if is-struct (iota-tree-node-children root) (cdr root)))
           (expanded (if is-struct (iota-tree-node-expanded root) t))
           (expand-icon (when children
                          (if expanded "[-] " "[+] ")))
           (face (when is-struct (iota-tree-node-face root))))
      (push (concat (or expand-icon "    ")
                    (if face (propertize label 'face face) label))
            lines)
      ;; Render children
      (when (and children expanded)
        (iota-widget--tree-children children lines "" indent-guides show-icons)))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun iota-widget--tree-children (children lines prefix indent-guides show-icons)
  "Render tree CHILDREN into LINES with PREFIX."
  (let ((num-children (length children)))
    (cl-loop for child in children
             for i from 0
             for is-last = (= i (1- num-children))
             do (let* ((is-struct (iota-tree-node-p child))
                       (label (if is-struct (iota-tree-node-label child) (car child)))
                       (grandchildren (if is-struct (iota-tree-node-children child) (cdr child)))
                       ;; Filter out non-list grandchildren
                       (grandchildren (when (and (listp grandchildren)
                                                 (or (null grandchildren)
                                                     (consp (car grandchildren))))
                                        grandchildren))
                       (expanded (if is-struct (iota-tree-node-expanded child) t))
                       (expand-icon (if grandchildren
                                        (if expanded "[-] " "[+] ")
                                      "    "))
                       (branch (if is-last "└── " "├── "))
                       (face (when is-struct (iota-tree-node-face child))))
                  ;; Render this node
                  (push (concat (propertize prefix 'face 'iota-muted-face)
                                (propertize branch 'face 'iota-box-face)
                                expand-icon
                                (if face (propertize label 'face face) label))
                        lines)
                  ;; Render grandchildren if expanded
                  (when (and grandchildren expanded)
                    (let ((new-prefix (concat prefix
                                              (if is-last
                                                  (if indent-guides "    " "    ")
                                                (if indent-guides "│   " "    ")))))
                      (iota-widget--tree-children grandchildren lines new-prefix
                                                   indent-guides show-icons)))))))

(defun iota-widget-tree-from-alist (alist)
  "Convert nested ALIST to tree node structure.
ALIST format: ((label . children) ...) or ((label) ...) for leaves."
  (mapcar (lambda (item)
            (if (consp (cdr item))
                (iota-tree-node-create
                 :label (car item)
                 :children (iota-widget-tree-from-alist (cdr item))
                 :expanded t)
              (iota-tree-node-create
               :label (car item)
               :expanded t)))
          alist))

;;; Tabbed Content Widget

(defvar-local iota-widget--tabs-state nil
  "State for tabbed widgets in buffer: ((id . selected-index) ...).")

(cl-defun iota-widget-tabs (tabs &key id selected style on-select)
  "Render a tab bar.

TABS is a list of (label . content) pairs or just labels.
:id        Unique identifier for this tab bar
:selected  Index of selected tab (default 0)
:style     Tab style: boxed, underline, pills, minimal (default boxed)
:on-select Callback when tab is selected (receives index)

Returns: String with tab bar."
  (let* ((id (or id 'default))
         (selected (or selected
                       (alist-get id iota-widget--tabs-state)
                       0))
         (style (or style 'boxed))
         (num-tabs (length tabs))
         (parts '()))
    ;; Store state
    (setf (alist-get id iota-widget--tabs-state) selected)
    ;; Build tab bar
    (cl-loop for tab in tabs
             for i from 0
             do (let* ((label (if (consp tab) (car tab) tab))
                       (is-selected (= i selected))
                       (tab-str
                        (pcase style
                          ('boxed
                           (if is-selected
                               (concat "┃ " (propertize label 'face '(:weight bold :foreground "#39bae6")) " ┃")
                             (concat "│ " (propertize label 'face 'iota-muted-face) " │")))
                          ('underline
                           (if is-selected
                               (concat (propertize label 'face '(:weight bold :underline t)) " ")
                             (concat (propertize label 'face 'iota-muted-face) " ")))
                          ('pills
                           (if is-selected
                               (propertize (concat " " label " ") 
                                           'face '(:weight bold :background "#39bae6" :foreground "#000"))
                             (propertize (concat " " label " ")
                                         'face '(:background "#333" :foreground "#888"))))
                          ('minimal
                           (if is-selected
                               (propertize label 'face '(:weight bold :foreground "#39bae6"))
                             (propertize label 'face 'iota-muted-face))))))
                  (push tab-str parts)))
    ;; Separator between tabs
    (let ((sep (pcase style
                 ('boxed "")
                 ('underline " │ ")
                 ('pills " ")
                 ('minimal " │ "))))
      (mapconcat #'identity (nreverse parts) sep))))

(cl-defun iota-widget-tabbed-content (tabs &key id style width)
  "Render tabs with content panel below.

TABS is a list of (label . content) pairs.
Returns: Multi-line string with tabs and content."
  (let* ((id (or id 'default))
         (selected (or (alist-get id iota-widget--tabs-state) 0))
         (width (or width 60))
         (tab-bar (iota-widget-tabs tabs :id id :style style))
         (content (cdr (nth selected tabs)))
         (content-str (if (stringp content) content (format "%s" content))))
    (concat
     tab-bar "\n"
     (iota-box-render :content content-str
                      :width width
                      :style 'rounded))))

;;; Card/Panel Widget

(cl-defun iota-widget-card (&key title content footer width style
                                  title-face border-face collapsible collapsed)
  "Render a card/panel widget.

Arguments:
  :title       Card title (appears in header)
  :content     Card content (string or list of strings)
  :footer      Optional footer content
  :width       Card width (default auto)
  :style       Border style (default rounded)
  :title-face  Face for title
  :border-face Face for borders
  :collapsible If t, show collapse indicator
  :collapsed   If t, hide content

Returns: Multi-line string."
  (let* ((style (or style 'rounded))
         (chars (iota-box-get-chars style))
         (title-face (or title-face '(:weight bold :foreground "#39bae6")))
         (border-face (or border-face 'iota-box-face))
         (content-lines (cond
                         ((null content) nil)
                         ((stringp content) (split-string content "\n"))
                         (t content)))
         (footer-lines (cond
                        ((null footer) nil)
                        ((stringp footer) (split-string footer "\n"))
                        (t footer)))
         (max-content-width (if content-lines
                                (apply #'max (mapcar #'length content-lines))
                              0))
         (max-footer-width (if footer-lines
                               (apply #'max (mapcar #'length footer-lines))
                             0))
         ;; Title with collapse icon if needed  
         (collapse-icon (if collapsible (if collapsed "▶ " "▼ ") ""))
         (full-title (concat collapse-icon (or title "")))
         (title-width (length full-title))
         ;; Calculate total width
         (content-width (max title-width max-content-width max-footer-width))
         (width (or width (+ content-width 4)))
         (inner-width (- width 2))  ;; Width between left and right borders
         (horiz (plist-get chars :horizontal))
         (vert (plist-get chars :vertical))
         (result '()))
    ;; Top border with title
    (if (and title (> (length title) 0))
        (let* ((right-horiz (max 0 (- inner-width title-width 1))))
          (push (concat
                 (propertize (plist-get chars :top-left) 'face border-face)
                 (propertize (make-string 1 (string-to-char horiz)) 'face border-face)
                 (propertize full-title 'face title-face)
                 (propertize (make-string right-horiz (string-to-char horiz)) 'face border-face)
                 (propertize (plist-get chars :top-right) 'face border-face))
                result))
      ;; No title - plain top border
      (push (concat
             (propertize (plist-get chars :top-left) 'face border-face)
             (propertize (make-string inner-width (string-to-char horiz)) 'face border-face)
             (propertize (plist-get chars :top-right) 'face border-face))
            result))
    ;; Content (unless collapsed)
    (unless collapsed
      (dolist (line (or content-lines '("")))
        (let ((padded (iota-box-pad line (- inner-width 2) 'left)))
          (push (concat (propertize vert 'face border-face)
                        " " padded " "
                        (propertize vert 'face border-face))
                result)))
      ;; Footer separator and content
      (when footer-lines
        (push (concat (propertize (plist-get chars :t-right) 'face border-face)
                      (propertize (make-string inner-width (string-to-char horiz)) 'face border-face)
                      (propertize (plist-get chars :t-left) 'face border-face))
              result)
        (dolist (line footer-lines)
          (let ((padded (iota-box-pad line (- inner-width 2) 'left)))
            (push (concat (propertize vert 'face border-face)
                          " " padded " "
                          (propertize vert 'face border-face))
                  result)))))
    ;; Bottom border
    (push (concat
           (propertize (plist-get chars :bottom-left) 'face border-face)
           (propertize (make-string inner-width (string-to-char horiz)) 'face border-face)
           (propertize (plist-get chars :bottom-right) 'face border-face))
          result)
    (mapconcat #'identity (nreverse result) "\n")))

;;; Interactive Collapsible Widget

(defvar iota-widget--collapsible-state (make-hash-table :test 'equal)
  "Hash table storing collapsed state for collapsible widgets by ID.")

(cl-defun iota-widget-collapsible-insert (&key id title content width style)
  "Insert an interactive collapsible widget at point.

Arguments:
  :id      Unique identifier (required)
  :title   Section title
  :content Content string or list of strings
  :width   Widget width (default auto)
  :style   Border style (default rounded)

The widget is interactive - click or press RET on the header to toggle."
  (let* ((id (or id (error "Collapsible widget requires :id")))
         (collapsed (gethash id iota-widget--collapsible-state nil))
         (start (point)))
    ;; Insert the widget
    (iota-widget--collapsible-render id title content width style collapsed)
    ;; Store the region for redraw
    (put-text-property start (point) 'iota-collapsible-id id)
    (put-text-property start (point) 'iota-collapsible-start start)))

(defun iota-widget--collapsible-render (id title content width style collapsed)
  "Render collapsible widget with ID, TITLE, CONTENT, WIDTH, STYLE, COLLAPSED state."
  (let* ((style (or style 'rounded))
         (chars (iota-box-get-chars style))
         (content-lines (cond
                         ((null content) nil)
                         ((stringp content) (split-string content "\n"))
                         (t content)))
         (max-content-width (if content-lines
                                (apply #'max (cons 0 (mapcar #'length content-lines)))
                              0))
         (collapse-icon (if collapsed "[+] " "[-] "))
         (full-title (concat collapse-icon (or title "")))
         (title-width (length full-title))
         ;; Width must accommodate both title and content
         (content-width (max title-width max-content-width))
         (width (or width (+ content-width 4)))
         (inner-width (- width 2))
         (horiz (plist-get chars :horizontal))
         (vert (plist-get chars :vertical))
         (title-face '(:weight bold :foreground "#39bae6"))
         (border-face 'iota-box-face)
         (header-start (point)))
    ;; Top border with clickable title
    (let* ((right-horiz (max 0 (- inner-width title-width 1)))
           (header-line (concat
                         (propertize (plist-get chars :top-left) 'face border-face)
                         (propertize (make-string 1 (string-to-char horiz)) 'face border-face)
                         (propertize full-title 'face title-face)
                         (propertize (make-string right-horiz (string-to-char horiz)) 'face border-face)
                         (propertize (plist-get chars :top-right) 'face border-face))))
      (insert-text-button header-line
                          'action (lambda (_btn)
                                    (iota-widget-collapsible-toggle id))
                          'follow-link t
                          'help-echo "Click to expand/collapse"
                          'face nil  ;; Don't override faces
                          'mouse-face '(:background "#3a3a3a")))
    (insert "\n")
    ;; Content (unless collapsed)
    (unless collapsed
      (dolist (line (or content-lines '("")))
        (let ((padded (iota-box-pad line (- inner-width 2) 'left)))
          (insert (propertize vert 'face border-face)
                  " " padded " "
                  (propertize vert 'face border-face)
                  "\n"))))
    ;; Bottom border
    (insert (propertize (plist-get chars :bottom-left) 'face border-face)
            (propertize (make-string inner-width (string-to-char horiz)) 'face border-face)
            (propertize (plist-get chars :bottom-right) 'face border-face))))

(defun iota-widget-collapsible-toggle (id)
  "Toggle the collapsed state of widget with ID."
  (interactive)
  (let ((current-state (gethash id iota-widget--collapsible-state nil)))
    (puthash id (not current-state) iota-widget--collapsible-state)
    ;; Find and redraw the widget
    (save-excursion
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found) (< (point) (point-max)))
          (let ((widget-id (get-text-property (point) 'iota-collapsible-id)))
            (if (equal widget-id id)
                (setq found t)
              (goto-char (or (next-single-property-change (point) 'iota-collapsible-id)
                             (point-max))))))
        (when found
          (iota-widget--collapsible-redraw id))))))

(defun iota-widget--collapsible-redraw (id)
  "Redraw the collapsible widget with ID at current position."
  (let* ((start (point))
         (end start)
         (props nil)
         (inhibit-read-only t))
    ;; Find the extent of this widget
    (while (and (< end (point-max))
                (equal (get-text-property end 'iota-collapsible-id) id))
      (setq end (or (next-single-property-change end 'iota-collapsible-id)
                    (point-max))))
    ;; Get widget properties before deleting
    (setq props (text-properties-at start))
    ;; Extract widget parameters (we need to store these)
    (let ((title (get-text-property start 'iota-collapsible-title))
          (content (get-text-property start 'iota-collapsible-content))
          (width (get-text-property start 'iota-collapsible-width))
          (style (get-text-property start 'iota-collapsible-style))
          (collapsed (gethash id iota-widget--collapsible-state nil)))
      ;; Delete old widget
      (delete-region start end)
      ;; Insert new widget
      (iota-widget--collapsible-render id title content width style collapsed)
      ;; Restore properties
      (put-text-property start (point) 'iota-collapsible-id id)
      (put-text-property start (point) 'iota-collapsible-title title)
      (put-text-property start (point) 'iota-collapsible-content content)
      (put-text-property start (point) 'iota-collapsible-width width)
      (put-text-property start (point) 'iota-collapsible-style style))))

(cl-defun iota-widget-collapsible-create (&key id title content width style collapsed)
  "Insert an interactive collapsible widget at point.

Arguments:
  :id        Unique identifier (required)
  :title     Section title
  :content   Content string or list of strings
  :width     Widget width (default auto)
  :style     Border style (default rounded)
  :collapsed Initial state (default nil = expanded)

The widget is interactive - click or press RET on the header to toggle.
Returns: marker at widget start."
  (let* ((id (or id (error "Collapsible widget requires :id")))
         (start (point)))
    ;; Set initial state
    (puthash id collapsed iota-widget--collapsible-state)
    ;; Render the widget
    (iota-widget--collapsible-render id title content width style collapsed)
    ;; Store properties for redraw
    (put-text-property start (point) 'iota-collapsible-id id)
    (put-text-property start (point) 'iota-collapsible-title title)
    (put-text-property start (point) 'iota-collapsible-content content)
    (put-text-property start (point) 'iota-collapsible-width width)
    (put-text-property start (point) 'iota-collapsible-style style)
    (copy-marker start)))

;;; Breadcrumb Widget

(cl-defun iota-widget-breadcrumb (items &key separator style)
  "Render a breadcrumb trail.

ITEMS is a list of strings or (label . action) pairs.
:separator  Separator between items (default \" ❯ \")
:style      Style: arrows, slashes, dots (default arrows)

Returns: Propertized string."
  (let* ((style (or style 'arrows))
         (separator (or separator
                        (pcase style
                          ('arrows " ❯ ")
                          ('slashes " / ")
                          ('dots " • ")
                          (_ " › ")))))
    (mapconcat
     (lambda (item)
       (let ((label (if (consp item) (car item) item)))
         (propertize label 'face 'iota-accent-face)))
     items
     (propertize separator 'face 'iota-muted-face))))

;;; Timeline Widget

(cl-defun iota-widget-timeline (events &key style show-time)
  "Render a vertical timeline.

EVENTS is a list of plists with :time :title :description :status keys.
:style     Timeline style: dots, line, detailed
:show-time Show timestamps (default t)

Returns: Multi-line string."
  (let* ((style (or style 'dots))
         (show-time (if (null show-time) t show-time))
         (lines '())
         (num-events (length events))
         (chars (pcase style
                  ('dots '("●" "│" "○"))
                  ('line '("◆" "│" "◇"))
                  ('detailed '("●" "┃" "○")))))
    (cl-loop for event in events
             for i from 0
             do (let* ((time (plist-get event :time))
                       (title (plist-get event :title))
                       (desc (plist-get event :description))
                       (status (plist-get event :status))
                       (is-last (= i (1- num-events)))
                       (dot (if (eq status 'pending)
                                (nth 2 chars)
                              (nth 0 chars)))
                       (dot-face (pcase status
                                   ('complete 'iota-success-face)
                                   ('error 'iota-error-face)
                                   ('pending 'iota-muted-face)
                                   (_ 'iota-accent-face))))
                  ;; Main line with dot and title
                  (push (concat
                         (when show-time
                           (propertize (or time "     ") 'face 'iota-muted-face))
                         (when show-time " ")
                         (propertize dot 'face dot-face)
                         " "
                         (propertize title 'face '(:weight bold)))
                        lines)
                  ;; Description line
                  (when desc
                    (push (concat
                           (when show-time "      ")
                           (propertize (nth 1 chars) 'face 'iota-muted-face)
                           " "
                           (propertize desc 'face 'iota-muted-face))
                          lines))
                  ;; Connector line (unless last)
                  (unless is-last
                    (push (concat
                           (when show-time "      ")
                           (propertize (nth 1 chars) 'face 'iota-muted-face))
                          lines))))
    (mapconcat #'identity (nreverse lines) "\n")))

;;; Chip/Tag Widget

(cl-defun iota-widget-chip (text &key type dismissible icon)
  "Render a chip/tag.

TEXT is the chip content.
:type        Type: default, success, error, warning, info
:dismissible Show dismiss icon
:icon        Icon to show before text

Returns: Propertized string."
  (let* ((face (pcase type
                 ('success '(:background "#143d1f" :foreground "#50fa7b"))
                 ('error '(:background "#3d1414" :foreground "#ff5555"))
                 ('warning '(:background "#3d3114" :foreground "#ffa500"))
                 ('info '(:background "#14283d" :foreground "#8be9fd"))
                 (_ '(:background "#333" :foreground "#aaa"))))
         (dismiss (when dismissible " ✕"))
         (icon-str (when icon (concat icon " "))))
    (propertize (concat " " (or icon-str "") text (or dismiss "") " ")
                'face face)))

(defun iota-widget-chip-group (chips &optional separator)
  "Render multiple chips with SEPARATOR (default space)."
  (mapconcat #'identity chips (or separator " ")))

;;; Code Block Widget

(cl-defun iota-widget-code-block (code &key language line-numbers width style)
  "Render a code block with optional line numbers.

CODE is the code string.
:language     Language name for header
:line-numbers Show line numbers (default nil)
:width        Block width (default auto)
:style        Border style

Returns: Multi-line string."
  (let* ((style (or style 'rounded))
         (lines (split-string code "\n"))
         (num-lines (length lines))
         (line-num-width (if line-numbers
                             (length (number-to-string num-lines))
                           0))
         ;; Line number prefix: "N │ " = digits + space + bar + space
         (line-num-prefix-width (if line-numbers (+ line-num-width 3) 0))
         (max-line-len (apply #'max (cons 0 (mapcar #'length lines))))
         ;; Total width: │ + space + [line-num-prefix] + code + space + │
         ;; inner-width (between │ and │) = space + line-num-prefix + code + space
         (inner-width (+ 2 line-num-prefix-width max-line-len))
         (width (or width (+ inner-width 2)))  ;; add 2 for the │ borders
         (chars (iota-box-get-chars style))
         (horiz (plist-get chars :horizontal))
         (vert (plist-get chars :vertical))
         (result '()))
    ;; Top border with language
    (if language
        (let* ((header (concat " " language " "))
               (header-len (length header))
               (right-len (max 0 (- inner-width header-len 1))))
          (push (concat (plist-get chars :top-left)
                        (make-string 1 (string-to-char horiz))
                        (propertize header 'face '(:weight bold))
                        (make-string right-len (string-to-char horiz))
                        (plist-get chars :top-right))
                result))
      (push (concat (plist-get chars :top-left)
                    (make-string inner-width (string-to-char horiz))
                    (plist-get chars :top-right))
            result))
    ;; Code lines
    (cl-loop for line in lines
             for i from 1
             do (let* ((line-num-str (when line-numbers
                                       (concat (iota-box-pad (number-to-string i) line-num-width 'right)
                                               " │ ")))
                       (line-num (when line-num-str
                                   (propertize line-num-str 'face 'iota-muted-face)))
                       (code-padded (iota-box-pad line max-line-len 'left)))
                  (push (concat vert " "
                                (or line-num "")
                                code-padded
                                " " vert)
                        result)))
    ;; Bottom border
    (push (concat (plist-get chars :bottom-left)
                  (make-string inner-width (string-to-char horiz))
                  (plist-get chars :bottom-right))
          result)
    (mapconcat #'identity (nreverse result) "\n")))

;;; Tables

(cl-defun iota-widget-table (&key headers rows border style)
  "Render a table.

Arguments:
  :headers List of column header strings
  :rows    List of row lists (each row is list of cell strings)
  :border  Border style: single, double, rounded (default single)
  :style   Additional styling options

Returns: Multi-line string table."
  (let* ((border (or border 'single))
         (chars (iota-box-get-chars border))
         (col-widths (make-vector (length headers) 0)))
    ;; Calculate column widths
    (dotimes (i (length headers))
      (aset col-widths i (length (nth i headers))))
    (dolist (row rows)
      (dotimes (i (length row))
        (when (< i (length col-widths))
          (aset col-widths i (max (aref col-widths i)
                                  (length (nth i row)))))))
    
    ;; Build table
    (let* ((horiz (plist-get chars :horizontal))
           (vert (plist-get chars :vertical))
           (top-left (plist-get chars :top-left))
           (top-right (plist-get chars :top-right))
           (bottom-left (plist-get chars :bottom-left))
           (bottom-right (plist-get chars :bottom-right))
           (t-down (plist-get chars :t-down))
           (t-up (plist-get chars :t-up))
           (cross (plist-get chars :cross))
           (result '()))
      
      ;; Top border
      (push (concat top-left
                    (mapconcat (lambda (w)
                                 (make-string (+ w 2) (string-to-char horiz)))
                               col-widths
                               t-down)
                    top-right)
            result)
      
      ;; Headers
      (push (concat vert " "
                    (mapconcat (lambda (i)
                                 (let ((header (nth i headers))
                                       (width (aref col-widths i)))
                                   (iota-box-pad header width 'left)))
                               (number-sequence 0 (1- (length headers)))
                               (concat " " vert " "))
                    " " vert)
            result)
      
      ;; Header separator
      (push (concat (plist-get chars :t-right)
                    (mapconcat (lambda (w)
                                 (make-string (+ w 2) (string-to-char horiz)))
                               col-widths
                               cross)
                    (plist-get chars :t-left))
            result)
      
      ;; Rows
      (dolist (row rows)
        (push (concat vert " "
                      (mapconcat (lambda (i)
                                   (let ((cell (or (nth i row) ""))
                                         (width (aref col-widths i)))
                                     (iota-box-pad cell width 'left)))
                                 (number-sequence 0 (1- (length headers)))
                                 (concat " " vert " "))
                      " " vert)
              result))
      
      ;; Bottom border
      (push (concat bottom-left
                    (mapconcat (lambda (w)
                                 (make-string (+ w 2) (string-to-char horiz)))
                               col-widths
                               t-up)
                    bottom-right)
            result)
      
      (mapconcat #'identity (nreverse result) "\n"))))

;;; Dialogs

(cl-defun iota-widget-dialog (&key title message buttons style)
  "Display a dialog box (non-blocking, returns buffer).

Arguments:
  :title   Dialog title
  :message Dialog message (can be multi-line)
  :buttons List of (label . action) pairs
  :style   Box style (default rounded)

Returns: Buffer containing dialog."
  (let* ((style (or style 'rounded))
         (title (or title "Dialog"))
         (message (or message "Message"))
         (buttons (or buttons '(("OK" . t))))
         (message-lines (split-string message "\n"))
         (max-width (apply #'max
                           (length title)
                           (mapcar #'length message-lines)))
         (button-line (mapconcat (lambda (btn) (format "[ %s ]" (car btn)))
                                 buttons
                                 " "))
         (width (max max-width (length button-line)))
         (buf (get-buffer-create "*IOTA Dialog*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (iota-box-render
               :content (append (list (propertize title 'face '(:weight bold)))
                               message-lines
                               (list "" button-line))
               :width (+ width 8)
               :style style
               :align 'center))
      (read-only-mode 1)
      (goto-char (point-min)))
    buf))

(defun iota-widget-confirm (prompt)
  "Show confirmation dialog and wait for response.
Returns t for yes, nil for no."
  (let ((response (read-char-choice
                   (format "%s (y/n): " prompt)
                   '(?y ?n ?Y ?N))))
    (member response '(?y ?Y))))

;;; Menus

(cl-defun iota-widget-menu (&key title items style)
  "Create a menu (returns buffer).

Arguments:
  :title Dialog title
  :items List of (label . action) pairs
  :style Box style

Returns: Buffer with menu."
  (let* ((style (or style 'rounded))
         (title (or title "Menu"))
         (max-width (apply #'max
                           (length title)
                           (mapcar (lambda (item) (length (car item))) items)))
         (buf (get-buffer-create "*IOTA Menu*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (iota-box-render
               :content (cons (propertize title 'face '(:weight bold))
                             (cl-loop for (label . action) in items
                                      for i from 1
                                      collect (format "%d. %s" i label)))
               :width (+ max-width 10)
               :style style
               :align 'left))
      (read-only-mode 1)
      (goto-char (point-min)))
    buf))

;;; Forms

(cl-defun iota-widget-form (&key title fields style)
  "Create a form (returns buffer).

Arguments:
  :title Form title
  :fields List of (label . default-value) pairs
  :style Box style

Returns: Buffer with form."
  (let* ((style (or style 'rounded))
         (title (or title "Form"))
         (max-label-width (apply #'max (mapcar (lambda (f) (length (car f))) fields)))
         (buf (get-buffer-create "*IOTA Form*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (propertize title 'face '(:weight bold :foreground "#39bae6")))
      (insert "\n\n")
      (dolist (field fields)
        (let ((label (car field))
              (default (cdr field)))
          (insert (concat (iota-box-pad label max-label-width 'left)
                          ": "
                          (or default "")
                          "\n"))))
      (goto-char (point-min))
      (read-only-mode 1))
    buf))

;;; Status Banners

(cl-defun iota-widget-banner (&key message type style)
  "Create a status banner.

Arguments:
  :message Banner message
  :type    Banner type: success, error, warning, info
  :style   Box style

Returns: String representation."
  (let* ((style (or style 'rounded))
         (type (or type 'info))
         (message (or message "Status"))
         (icon (pcase type
                 ('success "✓")
                 ('error "✗")
                 ('warning "⚠")
                 (_ "●")))
         (face (pcase type
                 ('success 'iota-success-face)
                 ('error 'iota-error-face)
                 ('warning 'iota-warning-face)
                 (_ 'iota-accent-face)))
         (text (format "%s %s" icon message)))
    (iota-box-render
     :content (propertize text 'face face)
     :width (+ (length text) 8)
     :style style
     :align 'center)))

;;; Sparklines

(defun iota-widget-sparkline (values &optional height)
  "Create a sparkline graph from VALUES.
HEIGHT determines the number of levels (default 8)."
  (let* ((height (or height 8))
         (blocks ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"])
         (min-val (apply #'min values))
         (max-val (apply #'max values))
         (range (- max-val min-val))
         (range (if (= range 0) 1 range)))
    (mapconcat
     (lambda (val)
       (let* ((normalized (/ (- val min-val) range))
              (index (min (1- height) (floor (* normalized height)))))
         (aref blocks index)))
     values
     "")))

;;; Spinners

(defvar iota-widget-spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧"]
  "Spinner animation frames.")

(defvar iota-widget--spinner-index 0
  "Current spinner frame index.")

(defvar iota-widget--spinner-timer nil
  "Timer for spinner animation.")

(defun iota-widget-spinner-start (buffer-or-marker)
  "Start spinner animation at BUFFER-OR-MARKER."
  (unless iota-widget--spinner-timer
    (setq iota-widget--spinner-index 0)
    (setq iota-widget--spinner-timer
          (run-with-timer 0 0.1
                          (lambda ()
                            (when (buffer-live-p (if (markerp buffer-or-marker)
                                                     (marker-buffer buffer-or-marker)
                                                   buffer-or-marker))
                              (save-excursion
                                (with-current-buffer (if (markerp buffer-or-marker)
                                                        (marker-buffer buffer-or-marker)
                                                      buffer-or-marker)
                                  (let ((inhibit-read-only t))
                                    (goto-char (if (markerp buffer-or-marker)
                                                  buffer-or-marker
                                                (point-min)))
                                    (delete-char 1)
                                    (insert (aref iota-widget-spinner-frames
                                                 iota-widget--spinner-index)))))
                              (setq iota-widget--spinner-index
                                    (mod (1+ iota-widget--spinner-index)
                                         (length iota-widget-spinner-frames)))))))))

(defun iota-widget-spinner-stop ()
  "Stop spinner animation."
  (when iota-widget--spinner-timer
    (cancel-timer iota-widget--spinner-timer)
    (setq iota-widget--spinner-timer nil)))

;;; Badges and Status Indicators

(defun iota-widget-badge (text &optional type)
  "Create a badge with TEXT.
TYPE can be: success, error, warning, info."
  (let ((face (pcase type
                ('success 'iota-success-face)
                ('error 'iota-error-face)
                ('warning 'iota-warning-face)
                (_ 'iota-accent-face))))
    (propertize (format "[ %s ]" text)
                'face `(:inherit ,face :weight bold))))

(defun iota-widget-status-indicator (status)
  "Render status indicator for STATUS (success, warning, error, info)."
  (let ((indicators
         '((success . ("✓" "#50fa7b"))
           (warning . ("⚠" "#ffa500"))
           (error   . ("✗" "#ff5555"))
           (info    . ("ℹ" "#8be9fd"))
           (pending . ("○" "#6272a4")))))
    (when-let ((indicator (alist-get status indicators)))
      (propertize (car indicator)
                 'face `(:foreground ,(cadr indicator))
                 'help-echo (format "Status: %s" status)))))

(defun iota-widget-spinner (frame &optional style)
  "Return spinner character for animation FRAME.
STYLE options: braille, dots, line, arc, box"
  (let ((frames
         (pcase (or style 'braille)
           ('braille '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
           ('dots    '("⠁" "⠂" "⠄" "⡀" "⢀" "⠠" "⠐" "⠈"))
           ('line    '("─" "/" "│" "\\"))
           ('arc     '("◜" "◠" "◝" "◞" "◡" "◟"))
           ('box     '("◰" "◳" "◲" "◱")))))
    (nth (mod frame (length frames)) frames)))

(defun iota-widget-bullet ()
  "Return best available bullet point glyph."
  (cond
   ((char-displayable-p ?●) "●")
   ((char-displayable-p ?■) "■")
   (t "•")))

;;; ============================================================
;;; Text Animation Widgets
;;; ============================================================

;;; Typing Effect

(defvar iota-widget--typing-timers nil
  "Alist of (id . timer) for active typing animations.")

(defvar iota-widget--typing-overlays nil
  "Alist of (id . overlay) for typing animations.")

(cl-defun iota-widget-typing-text (text &key id speed cursor on-complete face)
  "Create a typing animation effect.

Arguments:
  text         Text to type out
  :id          Unique identifier
  :speed       Characters per second (default 30)
  :cursor      Cursor character (default \"▌\")
  :on-complete Callback when done
  :face        Face for text

Returns: Marker at animation position."
  (let* ((id (or id (gensym "typing-")))
         (speed (or speed 30))
         (cursor (or cursor "▌"))
         (face (or face 'default))
         (interval (/ 1.0 speed))
         (start-pos (point))
         ov marker
         (text-len (length text))
         (char-idx 0))
    ;; Insert placeholder
    (insert " ")
    (setq marker (copy-marker start-pos))
    (setq ov (make-overlay start-pos (point)))
    ;; Initialize
    (overlay-put ov 'iota-widget 'typing)
    (overlay-put ov 'iota-widget-id id)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'display (propertize cursor 'face 'iota-accent-face))
    ;; Store
    (push (cons id ov) iota-widget--typing-overlays)
    ;; Animation timer
    (let ((timer (run-with-timer
                  0 interval
                  (lambda ()
                    (when (and ov (overlayp ov) (overlay-buffer ov))
                      (if (< char-idx text-len)
                          (progn
                            (cl-incf char-idx)
                            (overlay-put ov 'display
                                         (concat
                                          (propertize (substring text 0 char-idx) 'face face)
                                          (propertize cursor 'face 'iota-accent-face))))
                        ;; Done - show final text without cursor
                        (overlay-put ov 'display (propertize text 'face face))
                        (iota-widget-typing-stop id)
                        (when on-complete (funcall on-complete))))))))
      (push (cons id timer) iota-widget--typing-timers))
    marker))

(defun iota-widget-typing-stop (id)
  "Stop typing animation with ID."
  (when-let ((timer (alist-get id iota-widget--typing-timers)))
    (cancel-timer timer)
    (setq iota-widget--typing-timers
          (assq-delete-all id iota-widget--typing-timers))))

;;; Scroll Reveal Animation

(cl-defun iota-widget-reveal-lines (lines &key id interval delay face)
  "Reveal lines one by one with animation.

Arguments:
  lines     List of strings to reveal
  :id       Unique identifier  
  :interval Time between lines (default 0.15)
  :delay    Initial delay (default 0)
  :face     Face for text

Returns: Marker at animation position."
  (let* ((id (or id (gensym "reveal-")))
         (interval (or interval 0.15))
         (delay (or delay 0))
         (face (or face 'default))
         (marker (point-marker))
         (ov (make-overlay (point) (1+ (point))))
         (num-lines (length lines))
         (line-idx 0))
    ;; Initialize
    (overlay-put ov 'iota-widget 'reveal)
    (overlay-put ov 'iota-widget-id id)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'display "")
    ;; Store
    (push (cons id ov) iota-widget--typing-overlays)
    ;; Animation timer (with initial delay)
    (let ((timer (run-with-timer
                  delay interval
                  (lambda ()
                    (when (overlay-buffer ov)
                      (if (< line-idx num-lines)
                          (progn
                            (cl-incf line-idx)
                            (overlay-put ov 'display
                                         (propertize
                                          (mapconcat #'identity
                                                     (cl-subseq lines 0 line-idx)
                                                     "\n")
                                          'face face)))
                        ;; Done
                        (iota-widget-typing-stop id)))))))
      (push (cons id timer) iota-widget--typing-timers))
    marker))

;;; Shimmer/Loading Skeleton

(cl-defun iota-widget-skeleton (width &key height style)
  "Render a loading skeleton placeholder.

Arguments:
  width   Width of skeleton
  :height Number of lines (default 1)
  :style  Style: bars, blocks, dots

Returns: Multi-line string."
  (let* ((height (or height 1))
         (style (or style 'bars))
         (char (pcase style
                 ('bars "░")
                 ('blocks "▒")
                 ('dots "·")
                 (_ "░")))
         (line (propertize (make-string width (string-to-char char))
                           'face 'iota-muted-face)))
    (mapconcat #'identity (make-list height line) "\n")))

(defvar iota-widget--shimmer-timers nil
  "Active shimmer animation timers.")

(cl-defun iota-widget-shimmer (&key id width interval)
  "Create an animated shimmer effect.

Arguments:
  :id       Unique identifier
  :width    Width of shimmer (default 20)
  :interval Animation speed (default 0.1)

Returns: Marker at shimmer position."
  (let* ((id (or id (gensym "shimmer-")))
         (width (or width 20))
         (interval (or interval 0.1))
         (start-pos (point))
         ov marker
         (pos 0)
         (chars ["░" "▒" "▓" "█" "▓" "▒" "░"]))
    ;; Insert placeholder
    (insert " ")
    (setq marker (copy-marker start-pos))
    (setq ov (make-overlay start-pos (point)))
    (overlay-put ov 'iota-widget 'shimmer)
    (overlay-put ov 'evaporate t)
    (push (cons id ov) iota-widget--typing-overlays)
    (let ((timer (run-with-timer
                  0 interval
                  (lambda ()
                    (when (and ov (overlayp ov) (overlay-buffer ov))
                      (let ((display ""))
                        (dotimes (i width)
                          (let* ((dist (abs (- i pos)))
                                 (char-idx (min dist (1- (length chars))))
                                 (char (aref chars char-idx)))
                            (setq display (concat display char))))
                        (overlay-put ov 'display
                                     (propertize display 'face 'iota-muted-face))
                        (setq pos (mod (1+ pos) (+ width 6)))))))))
      (push (cons id timer) iota-widget--shimmer-timers))
    marker))

;;; ============================================================
;;; Layout System
;;; ============================================================

(cl-defun iota-layout-columns (columns &key spacing widths align)
  "Render multiple columns side by side.

Arguments:
  columns   List of content strings (one per column)
  :spacing  Space between columns (default 2)
  :widths   List of column widths (default: auto from content)
  :align    Alignment: left, center, right (default left)

Returns: Multi-line string."
  (let* ((spacing (or spacing 2))
         (spacer (make-string spacing ?\s))
         ;; Split each column into lines
         (column-lines (mapcar (lambda (col)
                                 (if (stringp col)
                                     (split-string col "\n")
                                   col))
                               columns))
         ;; Calculate widths from the actual max line length in each column
         (widths (or widths
                     (mapcar (lambda (lines)
                               (if lines
                                   (apply #'max (mapcar #'length lines))
                                 0))
                             column-lines)))
         ;; Find max height
         (max-height (apply #'max (mapcar #'length column-lines)))
         (result '()))
    ;; Build output line by line
    (dotimes (i max-height)
      (let ((line-parts '()))
        (cl-loop for col-lines in column-lines
                 for width in widths
                 do (let* ((line (or (nth i col-lines) ""))
                           ;; Truncate if too long, then pad
                           (truncated (if (> (length line) width)
                                          (substring line 0 width)
                                        line))
                           (padded (concat truncated
                                           (make-string (max 0 (- width (length truncated))) ?\s))))
                      (push padded line-parts)))
        (push (mapconcat #'identity (nreverse line-parts) spacer) result)))
    (mapconcat #'identity (nreverse result) "\n")))

(cl-defun iota-layout-rows (rows &key spacing)
  "Stack multiple rows vertically.

Arguments:
  rows     List of content strings
  :spacing Blank lines between rows (default 1)

Returns: Multi-line string."
  (let ((spacing (or spacing 1))
        (spacer (make-string spacing ?\n)))
    (mapconcat #'identity rows spacer)))

(cl-defun iota-layout-grid (items &key columns spacing cell-width cell-height)
  "Render items in a grid layout.

Arguments:
  items       List of content strings
  :columns    Number of columns (default 3)
  :spacing    Space between cells (default 2)
  :cell-width Width of each cell (default auto)
  :cell-height Height of each cell (default auto)

Returns: Multi-line string."
  (let* ((columns (or columns 3))
         (spacing (or spacing 2))
         ;; Calculate cell dimensions
         (cell-width (or cell-width
                         (apply #'max
                                (mapcar (lambda (item)
                                          (apply #'max
                                                 (mapcar #'length
                                                         (split-string item "\n"))))
                                        items))))
         (cell-height (or cell-height
                          (apply #'max
                                 (mapcar (lambda (item)
                                           (length (split-string item "\n")))
                                         items))))
         ;; Pad all items to same size
         (padded-items (mapcar
                        (lambda (item)
                          (let* ((lines (split-string item "\n"))
                                 (height (length lines))
                                 (v-pad (- cell-height height)))
                            (mapconcat
                             (lambda (line)
                               (iota-box-pad line cell-width 'left))
                             (append lines (make-list v-pad ""))
                             "\n")))
                        items))
         ;; Group into rows
         (rows '())
         (current-row '()))
    (dolist (item padded-items)
      (push item current-row)
      (when (= (length current-row) columns)
        (push (nreverse current-row) rows)
        (setq current-row nil)))
    ;; Handle incomplete last row
    (when current-row
      (while (< (length current-row) columns)
        (push (make-string cell-width ?\s) current-row))
      (push (nreverse current-row) rows))
    ;; Render grid
    (mapconcat
     (lambda (row)
       (iota-layout-columns row :spacing spacing :widths (make-list columns cell-width)))
     (nreverse rows)
     "\n\n")))

(cl-defun iota-layout-center (content &key width)
  "Center content horizontally.

Arguments:
  content  String to center
  :width   Total width (default window-width)

Returns: Centered string."
  (let* ((width (or width (window-width)))
         (lines (split-string content "\n")))
    (mapconcat
     (lambda (line)
       (iota-box-pad line width 'center))
     lines
     "\n")))

;;; ============================================================
;;; Emacs widget.el Integration
;;; ============================================================

;; Define IOTA button type
(define-button-type 'iota-button
  'face '(:foreground "#39bae6" :weight bold)
  'mouse-face '(:foreground "#39bae6" :underline t)
  'help-echo "Click to activate"
  'follow-link t)

(defun iota-widget-insert-button (label action &optional help-text)
  "Insert an IOTA-styled button.

LABEL is the button text.
ACTION is called when clicked.
HELP-TEXT is shown on hover."
  (insert-button (concat "[ " label " ]")
                 'type 'iota-button
                 'action (lambda (_) (funcall action))
                 'help-echo (or help-text label)))

;; Define IOTA widget types for widget.el
(define-widget 'iota-text 'text
  "IOTA styled text input."
  :format "%{%t%}: %v"
  :sample-face 'iota-accent-face
  :valid-regexp ".*")

(define-widget 'iota-toggle 'toggle
  "IOTA styled toggle."
  :format "%{%t%} %[%v%]"
  :on "●"
  :off "○")

(define-widget 'iota-choice 'menu-choice
  "IOTA styled choice menu."
  :format "%{%t%}: %[%v%]"
  :button-prefix "❯ "
  :button-suffix "")

;; Create a form with widget.el integration
(cl-defun iota-widget-create-form (fields &key title on-submit style)
  "Create an interactive form using widget.el.

FIELDS is a list of plists with :name :type :label :default keys.
:type can be: text, toggle, choice, number

Returns: Buffer with form."
  (let ((buf (get-buffer-create "*IOTA Form*")))
    (with-current-buffer buf
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)
      ;; Title
      (when title
        (widget-insert (propertize title 'face '(:weight bold :foreground "#39bae6" :height 1.2)))
        (widget-insert "\n")
        (widget-insert (iota-box-horizontal-line (length title) 'single 'iota-muted-face))
        (widget-insert "\n\n"))
      ;; Fields
      (let ((widgets '()))
        (dolist (field fields)
          (let* ((name (plist-get field :name))
                 (type (or (plist-get field :type) 'text))
                 (label (or (plist-get field :label) (symbol-name name)))
                 (default (plist-get field :default))
                 (choices (plist-get field :choices))
                 (w (pcase type
                      ('text
                       (widget-create 'editable-field
                                      :format (concat (propertize label 'face 'iota-accent-face)
                                                      ": %v")
                                      :size 30
                                      :value (or default "")))
                      ('toggle
                       (widget-create 'toggle
                                      :format (concat (propertize label 'face 'iota-accent-face)
                                                      " %[%v%]")
                                      :on (propertize "● ON" 'face 'iota-success-face)
                                      :off (propertize "○ OFF" 'face 'iota-muted-face)
                                      :value (or default nil)))
                      ('choice
                       (apply #'widget-create 'menu-choice
                              :format (concat (propertize label 'face 'iota-accent-face)
                                              ": %[%v%]")
                              :value (or default (car choices))
                              (mapcar (lambda (c)
                                        `(item :tag ,c :value ,c))
                                      choices)))
                      ('number
                       (widget-create 'number
                                      :format (concat (propertize label 'face 'iota-accent-face)
                                                      ": %v")
                                      :size 10
                                      :value (or default 0))))))
            (push (cons name w) widgets)
            (widget-insert "\n")))
        (widget-insert "\n")
        ;; Submit button
        (when on-submit
          (widget-create 'push-button
                         :notify (lambda (&rest _)
                                   (let ((values '()))
                                     (dolist (w widgets)
                                       (push (cons (car w) (widget-value (cdr w))) values))
                                     (funcall on-submit values)))
                         (propertize " Submit " 'face '(:weight bold))))
        (widget-insert "  ")
        ;; Cancel button
        (widget-create 'push-button
                       :notify (lambda (&rest _) (kill-buffer))
                       (propertize " Cancel " 'face 'iota-muted-face)))
      (widget-setup)
      (goto-char (point-min))
      (widget-forward 1))
    (switch-to-buffer buf)
    buf))

;;; ============================================================  
;;; Utility Functions
;;; ============================================================

(defun iota-widget-cleanup-all ()
  "Stop all widget animations and clean up resources."
  (interactive)
  (iota-widget-spinner-stop-all)
  ;; Clean up typing animations
  (dolist (entry iota-widget--typing-timers)
    (cancel-timer (cdr entry)))
  (setq iota-widget--typing-timers nil)
  ;; Clean up shimmer animations
  (dolist (entry iota-widget--shimmer-timers)
    (cancel-timer (cdr entry)))
  (setq iota-widget--shimmer-timers nil)
  ;; Clean up overlays
  (dolist (entry iota-widget--typing-overlays)
    (when (overlayp (cdr entry))
      (delete-overlay (cdr entry))))
  (setq iota-widget--typing-overlays nil)
  (message "IOTA widgets cleaned up"))

(provide 'iota-widgets)
;;; iota-widgets.el ends here
