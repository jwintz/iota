;;; iota-screens-alien.el --- Alien-life inspired particle flow animation -*- lexical-binding: t; -*-

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
;; Particle flow animation inspired by alien-life.py and similar
;; generative art. Uses a sine/cosine-based flow field to create
;; organic particle motion rendered as ASCII art.

;;; Code:

(require 'iota-screens)
(require 'iota-timers)
(require 'iota-faces)

;;; Configuration

(defcustom iota-screens-alien-speed 0.08
  "Update interval in seconds for alien animation."
  :type 'float
  :group 'iota-screens)

(defcustom iota-screens-alien-num-particles 3000
  "Number of particles in the flow field."
  :type 'integer
  :group 'iota-screens)

(defcustom iota-screens-alien-chars " .·:∘○◉●"
  "Characters representing particle density from low to high."
  :type 'string
  :group 'iota-screens)

;;; Animation State

(defvar-local iota-screens-alien--particles nil
  "List of particles. Each particle is (x y).")

(defvar-local iota-screens-alien--frame 0
  "Current animation frame number.")

(defvar-local iota-screens-alien--width 80
  "Buffer width in characters.")

(defvar-local iota-screens-alien--height 24
  "Buffer height in characters.")

;;; Flow Field Functions

(defun iota-screens-alien--flow-field (x y time)
  "Calculate flow field vector at position X,Y and TIME.
Returns (dx . dy) velocity vector."
  (let* ((scale 0.03)
         (t-slow (* time 0.01))
         ;; Create swirling motion
         (angle (+ (* (sin (* x scale)) 2.0)
                   (* (cos (* y scale)) 2.0)
                   (* (sin (* (+ x y) scale 0.5)) 1.5)
                   t-slow))
         (dx (* (cos angle) 0.8))
         (dy (* (sin angle) 0.8)))
    (cons dx dy)))

;;; Particle Functions

(defun iota-screens-alien--init-particles ()
  "Initialize particles with random positions."
  (setq iota-screens-alien--particles nil)
  (let ((w (float iota-screens-alien--width))
        (h (float iota-screens-alien--height)))
    (dotimes (_ iota-screens-alien-num-particles)
      (push (list (* (/ (random 10000) 10000.0) w)
                  (* (/ (random 10000) 10000.0) h))
            iota-screens-alien--particles))))

(defun iota-screens-alien--update-particles ()
  "Update all particle positions based on flow field."
  (let ((w (float iota-screens-alien--width))
        (h (float iota-screens-alien--height))
        (time iota-screens-alien--frame))
    (setq iota-screens-alien--particles
          (mapcar
           (lambda (particle)
             (let* ((x (car particle))
                    (y (cadr particle))
                    (flow (iota-screens-alien--flow-field x y time))
                    (new-x (+ x (car flow)))
                    (new-y (+ y (cdr flow))))
               ;; Wrap around edges
               (list (mod (+ new-x w) w)
                     (mod (+ new-y h) h))))
           iota-screens-alien--particles))))

;;; Rendering Functions

;; Use ASCII characters by default to avoid encoding issues
;; Can be customized to use Unicode if terminal supports it
(defconst iota-screens-alien--density-chars
  [?\s ?. ?: ?o ?O ?0 ?@ ?#]
  "Vector of ASCII characters for density levels (low to high).")

(defun iota-screens-alien--render ()
  "Render particles to buffer as density-based ASCII art."
  (let* ((width iota-screens-alien--width)
         (height iota-screens-alien--height)
         (grid (make-vector (* width height) 0))
         (max-density 1)
         (render-width (max 1 (1- width))))  ;; Prevent line wrapping, ensure at least 1
    
    ;; Count particles per cell
    (dolist (particle iota-screens-alien--particles)
      (let* ((x (floor (car particle)))
             (y (floor (cadr particle))))
        (when (and (>= x 0) (< x width)
                   (>= y 0) (< y height))
          (let* ((idx (+ x (* y width)))
                 (new-val (1+ (aref grid idx))))
            (aset grid idx new-val)
            (when (> new-val max-density)
              (setq max-density new-val))))))
    
    ;; Render grid
    (let ((inhibit-read-only t)
          (char-count (length iota-screens-alien--density-chars)))
      (erase-buffer)
      (dotimes (y height)
        (let ((line (make-string render-width ?\s)))
          (dotimes (x render-width)
            (let* ((idx (+ x (* y width)))
                   (density (aref grid idx)))
              (when (> density 0)
                (let* ((normalized (min 1.0 (/ (float density) (float max-density))))
                       ;; Map normalized value to character index
                       (char-idx (min (1- char-count)
                                      (floor (* normalized (1- char-count)))))
                       (char (aref iota-screens-alien--density-chars char-idx))
                       (face (cond
                              ((< normalized 0.3) 'iota-screens-alien-dim)
                              ((< normalized 0.6) 'iota-screens-alien-medium)
                              (t 'iota-screens-alien-bright))))
                  (aset line x char)
                  (put-text-property x (1+ x) 'face face line)))))
          (insert line)
          (when (< y (1- height))
            (insert "\n"))))
      (goto-char (point-min)))))

;;; Animation Loop

(defun iota-screens-alien--animate-step (buffer-name)
  "Execute one animation frame for BUFFER-NAME."
  (condition-case err
      (let ((buf (get-buffer buffer-name)))
        (when (buffer-live-p buf)
          (let ((win (get-buffer-window buf)))
            (when win
              (with-current-buffer buf
                (let ((w (window-body-width win))
                      (h (window-body-height win)))
                  ;; Handle resize
                  (when (or (/= w iota-screens-alien--width)
                            (/= h iota-screens-alien--height)
                            (null iota-screens-alien--particles))
                    (setq iota-screens-alien--width w
                          iota-screens-alien--height h)
                    (iota-screens-alien--init-particles))
                  
                  ;; Update and render
                  (iota-screens-alien--update-particles)
                  (iota-screens-alien--render)
                  (cl-incf iota-screens-alien--frame)))))))
    (error
     (message "Alien animation error: %s" err))))

;;; Entry Point

(defun iota-screens-alien-start (&optional instance-id)
  "Start particle flow animation in current buffer.
INSTANCE-ID is used to create unique timer keys for multi-screen support."
  (let ((buffer-name iota-screens--buffer-name))
    (when-let ((win (get-buffer-window buffer-name)))
      (with-current-buffer (get-buffer buffer-name)
        (let ((width (window-body-width win))
              (height (window-body-height win))
              (timer-key (if instance-id
                             (intern (format "screens-%d-alien-animate" instance-id))
                           'screens-alien-animate)))
          (setq iota-screens-alien--width width
                iota-screens-alien--height height
                iota-screens-alien--frame 0)
          (iota-screens-alien--init-particles)
          ;; Initial render
          (iota-screens-alien--render)
          ;; Start animation timer - pass buffer-name to the callback
          (iota-timers-run-with-timer
           timer-key
           0
           iota-screens-alien-speed
           #'iota-screens-alien--animate-step
           buffer-name))))))

(provide 'iota-screens-alien)

;;; iota-screens-alien.el ends here

