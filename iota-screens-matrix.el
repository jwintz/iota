;;; iota-screens-matrix.el --- Matrix rain animation for iota-screens -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Version: 0.1.0
;; Keywords: screensaver, animation
;; URL: https://github.com/jwintz/iota

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Matrix rain animation (cmatrix-style) for iota-screens.
;; Based on the algorithm from cmatrix by Chris Allegretta and Abishek V Ashok.
;; Features falling columns of characters with color trails and glitch effects.

;;; Code:

(require 'iota-screens)
(require 'iota-timers)
(require 'iota-faces)

;;; Configuration

(defcustom iota-screens-matrix-speed 0.05
  "Update interval in seconds for matrix rain animation.
Lower values = faster animation."
  :type 'float
  :group 'iota-screens)

(defcustom iota-screens-matrix-chars
  "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ0123456789"
  "Characters to use in matrix rain."
  :type 'string
  :group 'iota-screens)

;;; Animation State

;; Grid stores character value at each position
;; -1 = empty (black), 0 = head (white), positive = char value
(defvar-local iota-screens-matrix--grid nil
  "Vector of vectors: (aref (aref grid row) col) -> char value or -1.")

;; Track the length each column should grow to
(defvar-local iota-screens-matrix--lengths nil
  "Vector of target lengths for each column.")

;; Track remaining spaces before next column starts
(defvar-local iota-screens-matrix--spaces nil
  "Vector of space counts before next column drop.")

(defvar-local iota-screens-matrix--width 0
  "Current grid width (columns).")

(defvar-local iota-screens-matrix--height 0
  "Current grid height (rows).")

;;; Core Animation Functions

(defun iota-screens-matrix--random-char ()
  "Get a random character from matrix charset."
  (aref iota-screens-matrix-chars
        (random (length iota-screens-matrix-chars))))

(defun iota-screens-matrix--init-grid (width height)
  "Initialize the grid to WIDTH x HEIGHT."
  (setq iota-screens-matrix--width width
        iota-screens-matrix--height height)
  ;; Create grid: height rows, each row is a vector of width cells
  ;; Each cell is -1 (empty) initially
  (setq iota-screens-matrix--grid (make-vector height nil))
  (dotimes (row height)
    (aset iota-screens-matrix--grid row (make-vector width -1)))
  ;; Initialize per-column state
  (setq iota-screens-matrix--lengths (make-vector width 0))
  (setq iota-screens-matrix--spaces (make-vector width 0))
  ;; Set initial random spaces and lengths
  (dotimes (col width)
    (aset iota-screens-matrix--spaces col (random height))
    (aset iota-screens-matrix--lengths col (+ 3 (random (max 1 (- height 3)))))))

(defun iota-screens-matrix--update-column (col)
  "Update a single column COL using cmatrix-style algorithm."
  (let* ((height iota-screens-matrix--height)
         (grid iota-screens-matrix--grid)
         (row 0)
         (firstcoldone nil))
    
    ;; Skip over empty cells at the top
    (while (and (< row height)
                (= (aref (aref grid row) col) -1))
      (setq row (1+ row)))
    
    ;; If entire column is empty, maybe start a new drop
    (if (>= row height)
        ;; Column is empty - check if we should start a new one
        (if (> (aref iota-screens-matrix--spaces col) 0)
            ;; Still waiting
            (aset iota-screens-matrix--spaces col 
                  (1- (aref iota-screens-matrix--spaces col)))
          ;; Start new column at top
          (aset (aref grid 0) col (iota-screens-matrix--random-char))
          ;; Reset spaces and length for next time
          (aset iota-screens-matrix--spaces col (1+ (random height)))
          (aset iota-screens-matrix--lengths col (+ 3 (random (max 1 (- height 3))))))
      
      ;; Column has content - process each segment
      (setq row 0)
      (while (< row height)
        ;; Skip empty cells
        (while (and (< row height)
                    (= (aref (aref grid row) col) -1))
          (setq row (1+ row)))
        
        (when (< row height)
          ;; Found start of a segment
          (let ((segment-start row)
                (segment-length 0))
            ;; Count segment length and apply glitch effect
            (while (and (< row height)
                        (/= (aref (aref grid row) col) -1))
              ;; Random glitch: change character occasionally
              (when (< (random 100) 3)
                (aset (aref grid row) col (iota-screens-matrix--random-char)))
              (setq segment-length (1+ segment-length))
              (setq row (1+ row)))
            
            ;; Now row points to first empty cell after segment (or end)
            ;; Extend the segment downward if there's room
            (if (< row height)
                (progn
                  ;; Add new head
                  (aset (aref grid row) col (iota-screens-matrix--random-char))
                  ;; If segment is longer than target, remove from top
                  (when (or (> segment-length (aref iota-screens-matrix--lengths col))
                            firstcoldone)
                    (aset (aref grid segment-start) col -1)
                    (aset (aref grid 0) col -1)))
              ;; Segment hit bottom - clear top cell
              (aset (aref grid segment-start) col -1))
            
            (setq firstcoldone t)
            (setq row (1+ row))))))))

(defun iota-screens-matrix--update-state ()
  "Update all columns."
  (dotimes (col iota-screens-matrix--width)
    (iota-screens-matrix--update-column col)))

(defun iota-screens-matrix--render ()
  "Render the grid to buffer."
  (let ((inhibit-read-only t)
        (width iota-screens-matrix--width)
        (height iota-screens-matrix--height)
        (grid iota-screens-matrix--grid)
        (render-width (1- iota-screens-matrix--width)))  ;; Prevent line wrapping
    (erase-buffer)
    (dotimes (row height)
      (let ((row-data (aref grid row))
            (line (make-string render-width ?\s)))
        ;; Build the line - only render up to render-width
        (dotimes (col render-width)
          (let ((val (aref row-data col)))
            (unless (= val -1)
              (aset line col (if (characterp val) val ?*)))))
        ;; Now apply faces
        (dotimes (col render-width)
          (let ((val (aref row-data col)))
            (unless (= val -1)
              ;; Determine if this is the head (bottom of segment)
              (let* ((is-head (or (>= (1+ row) height)
                                  (= (aref (aref grid (1+ row)) col) -1)))
                     (face (if is-head
                               'iota-screens-matrix-head
                             ;; Varying brightness based on position in segment
                             (let ((dist-from-bottom 0)
                                   (check-row (1+ row)))
                               (while (and (< check-row height)
                                           (/= (aref (aref grid check-row) col) -1))
                                 (setq dist-from-bottom (1+ dist-from-bottom))
                                 (setq check-row (1+ check-row)))
                               (cond
                                ((< dist-from-bottom 2) 'iota-screens-matrix-bright)
                                ((< dist-from-bottom 5) 'iota-screens-matrix-medium)
                                (t 'iota-screens-matrix-dim))))))
                (put-text-property col (1+ col) 'face face line)))))
        ;; Insert line
        (insert line)
        (when (< row (1- height))
          (insert "\n"))))
    (goto-char (point-min))))

(defun iota-screens-matrix--animate-step (buffer-name)
  "Perform one animation frame for BUFFER-NAME."
  (condition-case err
      (let ((buf (get-buffer buffer-name)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (let ((win (get-buffer-window buf)))
              (when win
                ;; Check for resize
                (let ((w (window-body-width win))
                      (h (window-body-height win)))
                  (when (or (/= w iota-screens-matrix--width)
                            (/= h iota-screens-matrix--height)
                            (null iota-screens-matrix--grid))
                    (iota-screens-matrix--init-grid w h)))
                ;; Update and render
                (iota-screens-matrix--update-state)
                (iota-screens-matrix--render))))))
    (error 
     (message "Matrix animation error: %s" err))))

(defun iota-screens-matrix-start (&optional instance-id)
  "Start matrix rain animation in current buffer.
INSTANCE-ID is used to create unique timer keys for multi-screen support."
  (let* ((buffer-name iota-screens--buffer-name)
         (buf (get-buffer buffer-name))
         (win (get-buffer-window buf)))
    (when (and buf win)
      (with-current-buffer buf
        (let ((width (window-body-width win))
              (height (window-body-height win))
              (timer-key (if instance-id
                             (intern (format "screens-%d-matrix-animate" instance-id))
                           'screens-matrix-animate)))
          (iota-screens-matrix--init-grid width height)
          ;; Initial render
          (iota-screens-matrix--render)
          ;; Start timer - pass buffer-name to the callback
          (iota-timers-run-with-timer
           timer-key
           0
           iota-screens-matrix-speed
           #'iota-screens-matrix--animate-step
           buffer-name))))))

(provide 'iota-screens-matrix)
;;; iota-screens-matrix.el ends here

