;;; iota-screens-pipes.el --- 3D Pipes animation for iota-screens -*- lexical-binding: t; -*-

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
;; Classic 3D Pipes screensaver.
;; Pipes grow across the screen, changing direction and color.

;;; Code:

(require 'iota-screens)
(require 'iota-timers)
(require 'iota-faces)
(require 'cl-lib)

;;; Configuration

(defcustom iota-screens-pipes-speed 0.05
  "Update interval in seconds."
  :type 'float
  :group 'iota-screens)

(defcustom iota-screens-pipes-count 5
  "Number of concurrent pipes."
  :type 'integer
  :group 'iota-screens)

;;; State

(cl-defstruct iota-pipe
  x y           ; Current position
  dx dy         ; Current direction (-1, 0, 1)
  face          ; Face to use
  stuck)        ; Boolean, true if pipe cannot move

(defvar-local iota-screens-pipes--grid nil "Grid of pipe characters.")
(defvar-local iota-screens-pipes--face-grid nil "Grid of faces for characters.")
(defvar-local iota-screens-pipes--width 0 "Grid width.")
(defvar-local iota-screens-pipes--height 0 "Grid height.")
(defvar-local iota-screens-pipes--pipes nil "List of active pipes.")

;;; Pipe faces for variety
(defconst iota-screens-pipes--faces
  '(iota-screens-matrix-bright
    iota-screens-matrix-medium
    iota-screens-matrix-head
    iota-screens-alien-bright
    iota-screens-alien-medium)
  "List of faces to use for different pipes.")

;;; Characters - use box drawing characters

(defun iota-screens-pipes--get-char (from-dx from-dy to-dx to-dy)
  "Get pipe character for direction change FROM-DX,FROM-DY to TO-DX,TO-DY."
  (cond
   ;; Straight horizontal
   ((and (= from-dy 0) (= to-dy 0) (/= from-dx 0)) ?━)
   ;; Straight vertical
   ((and (= from-dx 0) (= to-dx 0) (/= from-dy 0)) ?┃)
   ;; Corners: entering horizontally, leaving vertically
   ((and (= from-dx 1) (= to-dy -1)) ?┛)   ; right -> up
   ((and (= from-dx 1) (= to-dy 1)) ?┓)    ; right -> down
   ((and (= from-dx -1) (= to-dy -1)) ?┗)  ; left -> up
   ((and (= from-dx -1) (= to-dy 1)) ?┏)   ; left -> down
   ;; Corners: entering vertically, leaving horizontally
   ((and (= from-dy -1) (= to-dx 1)) ?┗)   ; up -> right
   ((and (= from-dy -1) (= to-dx -1)) ?┛)  ; up -> left
   ((and (= from-dy 1) (= to-dx 1)) ?┏)    ; down -> right
   ((and (= from-dy 1) (= to-dx -1)) ?┓)   ; down -> left
   ;; Default: just a plus/cross for other cases
   (t ?╋)))

;;; Logic

(defun iota-screens-pipes--init-grid (w h)
  "Initialize grid to W x H."
  (setq iota-screens-pipes--width w
        iota-screens-pipes--height h
        iota-screens-pipes--grid (make-vector (* w h) nil)
        iota-screens-pipes--face-grid (make-vector (* w h) nil)
        iota-screens-pipes--pipes nil)
  ;; Spawn initial pipes
  (dotimes (_ iota-screens-pipes-count)
    (push (iota-screens-pipes--spawn-pipe) iota-screens-pipes--pipes)))

(defun iota-screens-pipes--spawn-pipe ()
  "Create a new pipe at a random position."
  (let* ((x (random iota-screens-pipes--width))
         (y (random iota-screens-pipes--height))
         (face (nth (random (length iota-screens-pipes--faces))
                    iota-screens-pipes--faces))
         ;; Start with a random direction
         (dirs '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
         (dir (nth (random 4) dirs)))
    (make-iota-pipe :x x :y y 
                    :dx (car dir) :dy (cdr dir) 
                    :face face :stuck nil)))

(defun iota-screens-pipes--is-free (x y)
  "Return t if position X,Y is free."
  (and (>= x 0) (< x iota-screens-pipes--width)
       (>= y 0) (< y iota-screens-pipes--height)
       (null (aref iota-screens-pipes--grid 
                   (+ x (* y iota-screens-pipes--width))))))

(defun iota-screens-pipes--update-pipe (pipe)
  "Update PIPE position and draw segment."
  (unless (iota-pipe-stuck pipe)
    (let* ((x (iota-pipe-x pipe))
           (y (iota-pipe-y pipe))
           (dx (iota-pipe-dx pipe))
           (dy (iota-pipe-dy pipe))
           (face (iota-pipe-face pipe)))
      
      ;; Possible next directions (prioritize current direction, then turns)
      (let* ((possible-dirs
              (cond
               ;; Moving horizontally
               ((/= dx 0)
                (list (cons dx 0)    ; continue
                      (cons 0 1)     ; turn down
                      (cons 0 -1)))  ; turn up
               ;; Moving vertically
               ((/= dy 0)
                (list (cons 0 dy)    ; continue
                      (cons 1 0)     ; turn right
                      (cons -1 0)))  ; turn left
               ;; Initial state (no direction yet)
               (t '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))))
             ;; Shuffle to add randomness
             (shuffled (sort (copy-sequence possible-dirs)
                             (lambda (_ _) (= 0 (random 3)))))
             (found-dir nil))
        
        ;; Find first free direction
        (dolist (dir shuffled)
          (unless found-dir
            (let ((nx (+ x (car dir)))
                  (ny (+ y (cdr dir))))
              (when (iota-screens-pipes--is-free nx ny)
                (setq found-dir dir)))))
        
        (if found-dir
            (let* ((new-dx (car found-dir))
                   (new-dy (cdr found-dir))
                   (char (iota-screens-pipes--get-char dx dy new-dx new-dy))
                   (idx (+ x (* y iota-screens-pipes--width))))
              ;; Draw character at current position
              (aset iota-screens-pipes--grid idx char)
              (aset iota-screens-pipes--face-grid idx face)
              ;; Move to new position
              (setf (iota-pipe-x pipe) (+ x new-dx))
              (setf (iota-pipe-y pipe) (+ y new-dy))
              (setf (iota-pipe-dx pipe) new-dx)
              (setf (iota-pipe-dy pipe) new-dy))
          ;; No valid direction - pipe is stuck
          (setf (iota-pipe-stuck pipe) t))))))

(defun iota-screens-pipes--render ()
  "Render the grid to buffer."
  (let ((inhibit-read-only t)
        (width iota-screens-pipes--width)
        (height iota-screens-pipes--height)
        (render-width (1- iota-screens-pipes--width)))  ;; Prevent line wrapping
    (erase-buffer)
    (dotimes (y height)
      (let ((line (make-string render-width ?\s)))
        (dotimes (x render-width)
          (let* ((idx (+ x (* y width)))
                 (char (aref iota-screens-pipes--grid idx))
                 (face (aref iota-screens-pipes--face-grid idx)))
            (when char
              (aset line x char)
              (when face
                (put-text-property x (1+ x) 'face face line)))))
        (insert line)
        (when (< y (1- height))
          (insert "\n"))))
    (goto-char (point-min))))

(defun iota-screens-pipes--animate-step (buffer-name)
  "Perform one animation frame for BUFFER-NAME."
  (condition-case err
      (let ((buf (get-buffer buffer-name)))
        (when (buffer-live-p buf)
          (let ((win (get-buffer-window buf)))
            (when win
              (with-current-buffer buf
                (let ((w (window-body-width win))
                      (h (window-body-height win)))
                  ;; Init or resize, or all pipes stuck -> reset
                  (when (or (null iota-screens-pipes--grid)
                            (/= w iota-screens-pipes--width)
                            (/= h iota-screens-pipes--height)
                            (cl-every #'iota-pipe-stuck iota-screens-pipes--pipes))
                    (iota-screens-pipes--init-grid w h))
                  
                  ;; Update all pipes
                  (dolist (p iota-screens-pipes--pipes)
                    (iota-screens-pipes--update-pipe p))
                  
                  ;; Render
                  (iota-screens-pipes--render)))))))
    (error
     (message "Pipes animation error: %s" err))))

(defun iota-screens-pipes-start (&optional instance-id)
  "Start pipes animation.
INSTANCE-ID is used to create unique timer keys for multi-screen support."
  (let ((buffer-name iota-screens--buffer-name))
    (when-let ((win (get-buffer-window buffer-name)))
      (with-current-buffer (get-buffer buffer-name)
        (let ((timer-key (if instance-id
                             (intern (format "screens-%d-pipes-animate" instance-id))
                           'screens-pipes-animate)))
          (iota-screens-pipes--init-grid
           (window-body-width win)
           (window-body-height win))
          ;; Start timer - pass buffer-name to the callback
          (iota-timers-run-with-timer
           timer-key
           0
           iota-screens-pipes-speed
           #'iota-screens-pipes--animate-step
           buffer-name))))))

(provide 'iota-screens-pipes)
;;; iota-screens-pipes.el ends here

