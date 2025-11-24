;;; iota-widgets.el --- Widget library for I O T Λ -*- lexical-binding: t; -*-

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
;; Provides popups, dialogs, forms, tables, and other interactive elements.

;;; Code:

(require 'cl-lib)
(require 'iota-box)
(require 'iota-tui)

;;; Progress Bars

(defcustom iota-widget-progress-styles
  '((blocks    "█" "░" "▓")     ; Filled, empty, partial
    (circles   "●" "○" "◐")
    (squares   "■" "□" "▪")
    (arrows    "▶" "▷" "▸")
    (diamonds  "◆" "◇" "◈")
    (braille   "⣿" "⠀" "⣤")
    (line      "─" "┄" "╌")
    (dots      "•" "·" "∙"))
  "Progress bar style definitions."
  :type '(alist :key-type symbol
                :value-type (list string string string))
  :group 'iota-widgets)

(cl-defun iota-widget-progress-bar (value total &key width style label show-percent gradient)
  "Render a progress bar.

Arguments:
  value          Current progress value
  total          Maximum value
  :width         Width of bar (default 40)
  :style         Style: blocks, circles, squares, arrows, diamonds, braille, line, dots
  :label         Optional label to show before bar
  :show-percent  Show percentage (default t)
  :gradient      List of colors for gradient effect

Returns: String representation of progress bar."
  (let* ((width (or width 40))
         (style (or style 'blocks))
         (show-percent (if (null show-percent) t show-percent))
         (chars (or (alist-get style iota-widget-progress-styles)
                   (alist-get 'blocks iota-widget-progress-styles)))
         (filled-char (nth 0 chars))
         (empty-char (nth 1 chars))
         (percent (/ (* value 100.0) total))
         (filled-width (floor (* width (/ percent 100.0))))
         (empty-width (- width filled-width)))
    (concat
     (when label (concat label " "))
     (if gradient
         (iota-widget-progress-gradient filled-width empty-width 
                                       filled-char empty-char gradient)
       (concat
        (make-string filled-width (string-to-char filled-char))
        (make-string empty-width (string-to-char empty-char))))
     (when show-percent
       (format " %.0f%%" percent)))))

(defun iota-widget-progress-gradient (filled empty fill-char empty-char colors)
  "Render gradient progress bar."
  (let ((gradient-steps (min filled (length colors))))
    (concat
     (mapconcat
      (lambda (i)
        (propertize (make-string 1 (string-to-char fill-char))
                   'face `(:foreground ,(nth (min i (1- (length colors))) 
                                             colors))))
      (number-sequence 0 (1- filled))
      "")
     (make-string empty (string-to-char empty-char)))))

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
          (insert (format "%-*s: %s\n"
                         max-label-width
                         label
                         (or default "")))))
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

(provide 'iota-widgets)
;;; iota-widgets.el ends here
