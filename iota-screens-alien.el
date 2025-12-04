;;; iota-screens-alien.el --- Alien-life inspired particle flow animation -*- lexical-binding: t; -*-

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
;; Particle flow animation inspired by alien-life.py and similar
;; generative art. Uses a sine/cosine-based flow field to create
;; organic particle motion rendered as ASCII art.
;;
;; Design:
;; - Particles flow according to a time-varying flow field
;; - Flow field uses layered sine/cosine waves for organic motion
;; - Particles are rendered as density-based ASCII characters
;; - Uses circular boundary to contain particles
;; - Much simpler than original (no Perlin noise) but similar aesthetic

;;; Code:

(require 'iota-screens)
(require 'iota-timers)
(require 'iota-faces)

;;; Configuration

(defcustom iota-screens-alien-speed 0.08
  "Update interval in seconds for alien animation.
Lower values = faster animation. Recommended: 0.05-0.15"
  :type 'float
  :group 'iota-screens)

(defcustom iota-screens-alien-num-particles 2000
  "Number of particles in the flow field.
More particles = denser appearance but slower performance.
Original alien-life uses 40000; we use fewer for terminal performance."
  :type 'integer
  :group 'iota-screens)

(defcustom iota-screens-alien-field-scale 0.08
  "Scale factor for flow field (affects wave size).
Smaller = larger waves, larger = smaller waves."
  :type 'float
  :group 'iota-screens)

(defcustom iota-screens-alien-particle-speed 1.5
  "Speed multiplier for particle movement."
  :type 'float
  :group 'iota-screens)

(defcustom iota-screens-alien-chars " .·:∘○◉●"
  "Characters representing particle density from low to high.
First character = empty, last = highest density."
  :type 'string
  :group 'iota-screens)

(defcustom iota-screens-alien-boundary-type 'circle
  "Type of boundary for particle containment.
Options: 'circle (like original alien-life), 'rect, 'none"
  :type '(choice (const :tag "Circle" circle)
                 (const :tag "Rectangle" rect)
                 (const :tag "None (wrap)" none))
  :group 'iota-screens)

;;; Animation State

(defvar-local iota-screens-alien--particles nil
  "List of particles. Each particle is (x y vx vy) in normalized coordinates.")

(defvar-local iota-screens-alien--frame 0
  "Current animation frame number.")

(defvar-local iota-screens-alien--width 80
  "Buffer width in characters.")

(defvar-local iota-screens-alien--height 24
  "Buffer height in characters.")

;;; Flow Field Functions

(defun iota-screens-alien--flow-field (x y time)
  "Calculate flow field vector at position X,Y and TIME.
Returns (dx . dy) velocity vector.
Uses cycling time like original alien-life (cos_t, sin_t as 3rd/4th dimensions)."
  (let* ((scale iota-screens-alien-field-scale)
         ;; Cycle time like original: 2*pi*time creates full rotation
         (t-cycle (* time 0.004))  ; Slower cycling for smoother motion
         (cos-t (* 1.5 (cos (* 2 float-pi t-cycle))))
         (sin-t (* 1.5 (sin (* 2 float-pi t-cycle))))
         ;; Pseudo 4D noise using combined sine/cosine with cycling time
         ;; Layer 1: Primary flow influenced by position and cyclic time
         (noise1 (+ (* (sin (* x scale)) 2.0)
                    (* (cos (* y scale)) 2.0)
                    cos-t))
         ;; Layer 2: Secondary flow (offset position, different frequency)
         (noise2 (+ (* (sin (* (+ x 100) scale 1.5)) 1.5)
                    (* (cos (* (+ y 100) scale 1.5)) 1.5)
                    sin-t))
         ;; Combine for dx and dy
         (dx (+ (* (cos noise1) 0.7)
                (* (sin noise2) 0.3)))
         (dy (+ (* (sin noise1) 0.7)
                (* (cos noise2) 0.3))))
    (cons dx dy)))

;;; Particle Functions

(defun iota-screens-alien--init-particles ()
  "Initialize particles with random positions within boundary."
  (setq iota-screens-alien--particles nil)
  (dotimes (_ iota-screens-alien-num-particles)
    (let* ((angle (* (random 1000) 0.001 (* 2 float-pi)))
           ;; For circle: sqrt for uniform disk distribution, scale to 0.47 radius (leaves margin)
           (radius (if (eq iota-screens-alien-boundary-type 'circle)
                       (* (sqrt (/ (float (random 1000)) 1000.0)) 0.47)
                     0))
           (x (if (eq iota-screens-alien-boundary-type 'circle)
                  (+ 0.5 (* radius (cos angle)))
                (* (random 1000) 0.001)))
           (y (if (eq iota-screens-alien-boundary-type 'circle)
                  (+ 0.5 (* radius (sin angle)))
                (* (random 1000) 0.001))))
      (push (list x y 0.0 0.0) iota-screens-alien--particles))))

(defun iota-screens-alien--update-particle (particle)
  "Update single PARTICLE position based on flow field.
Returns updated particle (x y vx vy)."
  (cl-destructuring-bind (x y vx vy) particle
    (let* ((flow (iota-screens-alien--flow-field
                  (* x iota-screens-alien--width)
                  (* y iota-screens-alien--height)
                  iota-screens-alien--frame))
           (dx (* (car flow) iota-screens-alien-particle-speed 0.01))
           (dy (* (cdr flow) iota-screens-alien-particle-speed 0.01))
           (new-x (+ x dx))
           (new-y (+ y dy)))

      ;; Apply boundary conditions
      (pcase iota-screens-alien-boundary-type
        ('circle
         ;; Keep particles within circle - softer boundary
         (let* ((cx 0.5)
               (cy 0.5)
               (dist (sqrt (+ (* (- new-x cx) (- new-x cx))
                            (* (- new-y cy) (- new-y cy))))))
           (if (> dist 0.48)
               ;; Gently push back toward center
               (let ((angle (atan (- new-y cy) (- new-x cx)))
                     (push-strength 0.02))
                 (list (- new-x (* push-strength (cos angle)))
                      (- new-y (* push-strength (sin angle)))
                      dx dy))
             (list new-x new-y dx dy))))
        ('rect
         ;; Wrap at edges
         (list (mod (+ new-x 1.0) 1.0)
              (mod (+ new-y 1.0) 1.0)
              dx dy))
        ('none
         ;; No boundaries, just wrap
         (list (mod (+ new-x 1.0) 1.0)
              (mod (+ new-y 1.0) 1.0)
              dx dy))))))

(defun iota-screens-alien--update-particles ()
  "Update all particle positions."
  (setq iota-screens-alien--particles
        (mapcar #'iota-screens-alien--update-particle
                iota-screens-alien--particles)))

;;; Rendering Functions

(defun iota-screens-alien--render ()
  "Render particles to buffer as density-based ASCII art."
  (let* ((width iota-screens-alien--width)
         (height iota-screens-alien--height)
         (grid (make-hash-table :test 'equal))
         (max-density 0))

    ;; Count particles per cell
    (dolist (particle iota-screens-alien--particles)
      (cl-destructuring-bind (x y _vx _vy) particle
        (let* ((cx (floor (* x width)))
              (cy (floor (* y height))))
          (when (and (>= cx 0) (< cx width)
                    (>= cy 0) (< cy height))
            (let ((key (cons cx cy)))
              (puthash key (1+ (gethash key grid 0)) grid)
              (setq max-density (max max-density (gethash key grid))))))))

    ;; Debug every 50 frames
    (when (= (mod iota-screens-alien--frame 50) 0)
      (message "Alien render: %d particles, grid has %d cells, max-density=%d, dims=%dx%d"
              (length iota-screens-alien--particles)
              (hash-table-count grid)
              max-density
              width height))

    ;; Render grid - use width-1 to prevent wrapping
    (erase-buffer)
    (dotimes (y height)
      (dotimes (x (1- width))
        (let* ((density (gethash (cons x y) grid 0))
               (char-face (iota-screens-alien--density-to-char-face density max-density)))
          (insert (propertize (char-to-string (car char-face))
                            'face (cdr char-face)))))
      (when (< y (1- height))
        (insert "\n")))
    
    ;; Ensure we're viewing from the top of the buffer
    (goto-char (point-min))
    (set-window-start (get-buffer-window (current-buffer)) (point-min))))

(defun iota-screens-alien--density-to-char-face (density max-density)
  "Convert DENSITY to character and face.
MAX-DENSITY is used for normalization.
Returns (char . face)."
  (if (= density 0)
      (cons ?\s nil)
    (let* ((normalized (if (> max-density 0)
                          (/ (float density) max-density)
                        0))
           (char-count (length iota-screens-alien-chars))
           (char-idx (min (1- char-count)
                         (floor (* normalized char-count))))
           (char (aref iota-screens-alien-chars char-idx))
           (face (cond
                  ((< normalized 0.3) 'iota-screens-alien-dim)
                  ((< normalized 0.6) 'iota-screens-alien-medium)
                  (t 'iota-screens-alien-bright))))
      (cons char face))))

;;; Animation Loop

(defun iota-screens-alien--animate-step ()
  "Execute one animation frame."
  (condition-case err
      (let ((win (get-buffer-window iota-screens--buffer-name)))
        (when (and (buffer-live-p (get-buffer iota-screens--buffer-name))
                   win)
          (with-current-buffer iota-screens--buffer-name
            (let ((inhibit-read-only t))
              ;; Debug output every 50 frames
              (when (= (mod iota-screens-alien--frame 50) 0)
                (message "Alien: Frame %d" iota-screens-alien--frame))

              ;; Update dimensions from the window displaying the buffer
              (setq iota-screens-alien--width (window-body-width win))
              (setq iota-screens-alien--height (window-body-height win))

              ;; Update particle positions
              (iota-screens-alien--update-particles)

              ;; Render
              (iota-screens-alien--render)

              (cl-incf iota-screens-alien--frame)))))
    (error
     (message "Alien animation error: %s" err)
     (iota-timers-cancel 'screens-alien-animate))))

;;; Entry Point

(defun iota-screens-alien-start ()
  "Start particle flow animation in current buffer."
  (message "Alien: Starting animation...")
  (let ((win (get-buffer-window iota-screens--buffer-name)))
    (with-current-buffer (get-buffer iota-screens--buffer-name)
      (let ((inhibit-read-only t)
            (width (window-body-width win))
            (height (window-body-height win)))
        (erase-buffer)
        (message "Alien: Buffer cleared, dimensions: %dx%d" width height)
        ;; Initialize state
        (setq-local iota-screens-alien--frame 0)
        (setq-local iota-screens-alien--width width)
        (setq-local iota-screens-alien--height height)
        (iota-screens-alien--init-particles)
        (message "Alien: Initialized %d particles" (length iota-screens-alien--particles))
        ;; Start animation timer
        (iota-timers-run-with-timer
         'screens-alien-animate
         0
         iota-screens-alien-speed
         #'iota-screens-alien--animate-step)
        (message "Alien: Timer started (interval: %.2f)" iota-screens-alien-speed)))))

(provide 'iota-screens-alien)

;;; iota-screens-alien.el ends here
