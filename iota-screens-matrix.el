;;; iota-screens-matrix.el --- Matrix rain animation for iota-screens -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Version: 0.1.0
;; Keywords: screensaver, animation
;; URL: https://github.com/yourusername/iota

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Matrix rain animation (cmatrix-style) for iota-screens.
;; Features falling columns of characters with color trails.

;;; Code:

(require 'iota-screens)
(require 'iota-timers)
(require 'iota-faces)

;;; Configuration

(defcustom iota-screens-matrix-speed 0.08
  "Update interval in seconds for matrix rain animation.
Lower values = faster animation. Recommended: 0.05-0.15"
  :type 'float
  :group 'iota-screens)

(defcustom iota-screens-matrix-chars
  "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Characters to use in matrix rain.
Default uses half-width katakana and alphanumeric characters."
  :type 'string
  :group 'iota-screens)

(defcustom iota-screens-matrix-density 0.3
  "Density of matrix columns (0.0-1.0).
1.0 = every column has rain, 0.5 = half the columns."
  :type 'float
  :group 'iota-screens)

(defcustom iota-screens-matrix-min-speed 1
  "Minimum speed for falling columns (rows per frame)."
  :type 'integer
  :group 'iota-screens)

(defcustom iota-screens-matrix-max-speed 3
  "Maximum speed for falling columns (rows per frame)."
  :type 'integer
  :group 'iota-screens)

(defcustom iota-screens-matrix-min-length 5
  "Minimum length of rain trails."
  :type 'integer
  :group 'iota-screens)

(defcustom iota-screens-matrix-max-length 20
  "Maximum length of rain trails."
  :type 'integer
  :group 'iota-screens)

;;; Animation State

(defvar-local iota-screens-matrix--columns nil
  "List of column states. Each element is (x-pos y-pos speed length).")

(defvar-local iota-screens-matrix--frame 0
  "Current animation frame number.")

(defvar-local iota-screens-matrix--grid nil
  "2D grid storing characters and their ages for fade effect.
Structure: hash-table with key (x . y) -> (char . age)")

;;; Core Animation Functions

(defun iota-screens-matrix-start ()
  "Start matrix rain animation in current buffer."
  (message "Matrix: Starting animation...")
  (let* ((buf (get-buffer iota-screens--buffer-name))
         (win (get-buffer-window buf)))
    ;; Debug: what window do we have?
    (message "Matrix DEBUG: buf=%s win=%s selected-win=%s"
             buf win (selected-window))
    (message "Matrix DEBUG: win-buffer=%s win-total-h=%s win-body-h=%s win-pixel-h=%s"
             (when win (window-buffer win))
             (when win (window-total-height win))
             (when win (window-body-height win))
             (when win (window-pixel-height win)))
    (message "Matrix DEBUG: frame-height=%s frame-char-height=%s"
             (frame-height)
             (frame-char-height))
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             ;; Use selected-window if it's showing our buffer, else the found window
             (actual-win (if (eq (window-buffer (selected-window)) buf)
                             (selected-window)
                           win))
             (width (window-body-width actual-win))
             (height (window-body-height actual-win)))
        (message "Matrix DEBUG: actual-win=%s actual-height=%s" actual-win height)
        (erase-buffer)
        (message "Matrix: Buffer cleared, dimensions: %dx%d" width height)
        ;; Initialize state
        (setq-local iota-screens-matrix--columns nil)
        (setq-local iota-screens-matrix--frame 0)
        (setq-local iota-screens-matrix--grid (make-hash-table :test 'equal))
        ;; Initialize columns with correct dimensions
        (iota-screens-matrix--init-columns width height)
        (message "Matrix: Initialized %d columns" (length iota-screens-matrix--columns))
        ;; Start animation timer
        (iota-timers-run-with-timer
         'screens-matrix-animate
         0
         iota-screens-matrix-speed
         #'iota-screens-matrix--animate-step)
        (message "Matrix: Timer started (interval: %.2f)" iota-screens-matrix-speed)))))

(defun iota-screens-matrix--init-columns (width height)
  "Initialize column states for matrix rain.
WIDTH and HEIGHT are the buffer dimensions."
  (let ((num-cols (floor (* width iota-screens-matrix-density))))
    (setq iota-screens-matrix--columns nil)
    (dotimes (_ num-cols)
      (push (iota-screens-matrix--make-column width height)
            iota-screens-matrix--columns))))

(defun iota-screens-matrix--make-column (width height)
  "Create a new column with random parameters.
WIDTH and HEIGHT are the buffer dimensions."
  (list
   (random width)                                              ; x-position
   ;; Start some columns above screen, some within screen for immediate fill
   (- (random (* height 2)) height)                            ; y-position (staggered)
   (+ iota-screens-matrix-min-speed
      (random (- iota-screens-matrix-max-speed
                 iota-screens-matrix-min-speed)))              ; speed
   (+ iota-screens-matrix-min-length
      (random (- iota-screens-matrix-max-length
                 iota-screens-matrix-min-length)))))           ; trail length

(defun iota-screens-matrix--random-char ()
  "Get a random character from matrix charset."
  (let ((len (length iota-screens-matrix-chars)))
    (aref iota-screens-matrix-chars (random len))))

(defun iota-screens-matrix--animate-step ()
  "Execute one animation frame."
  (condition-case err
      (let ((win (get-buffer-window iota-screens--buffer-name)))
        (when (and (buffer-live-p (get-buffer iota-screens--buffer-name))
                   win)
          (with-current-buffer iota-screens--buffer-name
            (let* ((inhibit-read-only t)
                   ;; Get dimensions from the window displaying the buffer
                   (width (window-body-width win))
                   (height (window-body-height win)))

              ;; Debug output every 50 frames
              (when (= (mod iota-screens-matrix--frame 50) 0)
                (let ((char-count (hash-table-count iota-screens-matrix--grid))
                      (min-y height)
                      (max-y -1))
                  (maphash (lambda (pos _data)
                             (let ((y (cdr pos)))
                               (setq min-y (min min-y y))
                               (setq max-y (max max-y y))))
                           iota-screens-matrix--grid)
                  (message "Matrix: Frame %d, %d chars, Y range: %d-%d (height=%d, win-height=%d)"
                           iota-screens-matrix--frame char-count min-y max-y height
                           (window-body-height win))))

              ;; Age all existing characters
              (maphash (lambda (pos data)
                         (let ((char (car data))
                               (age (cdr data)))
                           (puthash pos (cons char (1+ age)) iota-screens-matrix--grid)))
                       iota-screens-matrix--grid)

              ;; Update column positions and add new characters
              (setq iota-screens-matrix--columns
                    (mapcar (lambda (col)
                              (cl-destructuring-bind (x y speed length) col
                                (let ((new-y (+ y speed)))
                                  ;; Add head character
                                  (when (and (>= new-y 0) (< new-y height))
                                    (puthash (cons x new-y)
                                             (cons (iota-screens-matrix--random-char) 0)
                                             iota-screens-matrix--grid))
                                  ;; Remove old tail
                                  (let ((tail-y (- new-y length)))
                                    (when (>= tail-y 0)
                                      (remhash (cons x tail-y) iota-screens-matrix--grid)))
                                  ;; Reset column if completely off screen
                                  (if (> new-y (+ height length))
                                      (iota-screens-matrix--make-column width height)
                                    (list x new-y speed length)))))
                            iota-screens-matrix--columns))

              ;; Render the grid
              (iota-screens-matrix--render-grid width height)

              ;; Increment frame counter
              (cl-incf iota-screens-matrix--frame)))))
    (error
     (message "Matrix animation error: %s" err)
     (iota-timers-cancel 'screens-matrix-animate))))

(defun iota-screens-matrix--render-grid (width height)
  "Render the character grid to buffer.
WIDTH and HEIGHT are buffer dimensions."
  (erase-buffer)

  ;; Create blank grid - use width-1 to prevent wrapping
  (dotimes (y height)
    (insert (make-string (1- width) ?\s))
    (when (< y (1- height))
      (insert "\n")))

  ;; Debug every 50 frames: check buffer state
  (when (= (mod iota-screens-matrix--frame 50) 0)
    (let ((line-count (count-lines (point-min) (point-max)))
          (buf-size (buffer-size)))
      (message "Matrix render: height=%d, lines=%d, buf-size=%d, expected=%d"
               height line-count buf-size
               (+ (* height (1- width)) (1- height)))))

  ;; Draw characters with appropriate faces
  (maphash (lambda (pos data)
             (cl-destructuring-bind (x . y) pos
               (cl-destructuring-bind (char . age) data
                  ;; Use (1- width) because lines only have width-1 characters
                  (when (and (>= x 0) (< x (1- width))
                            (>= y 0) (< y height))
                    (let ((face (cond
                                ((= age 0) 'iota-screens-matrix-head)
                                ((< age 2) 'iota-screens-matrix-bright)
                                ((< age 5) 'iota-screens-matrix-medium)
                                (t 'iota-screens-matrix-dim))))
                      (iota-screens-matrix--put-char x y char face (1- width)))))))
           iota-screens-matrix--grid)
  
  ;; Ensure we're viewing from the top of the buffer
  (goto-char (point-min))
  (set-window-start (get-buffer-window (current-buffer)) (point-min)))

(defun iota-screens-matrix--put-char (x y char face line-width)
  "Put CHAR at position X,Y with FACE.
LINE-WIDTH is the actual width of each line (characters per line)."
  (save-excursion
    (goto-char (point-min))
    (let ((lines-moved (forward-line y)))
      ;; Debug: report failures for high Y values
      (when (and (> y 20) (= (mod iota-screens-matrix--frame 100) 0) (< (random 100) 5))
        (message "put-char debug: y=%d, forward-line returned %d, now at line %d, point=%d, max=%d"
                 y lines-moved (line-number-at-pos) (point) (point-max)))
      (when (zerop lines-moved)
        (when (< x line-width)
          (move-to-column x)
          (when (and (not (eobp))
                     (not (eolp)))
            (let ((inhibit-read-only t))
              (delete-char 1)
              (insert (propertize (char-to-string char) 'face face)))))))))

(provide 'iota-screens-matrix)

;;; iota-screens-matrix.el ends here
