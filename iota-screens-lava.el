;;; iota-screens-lava.el --- Lava lamp metaball animation -*- lexical-binding: t; -*-

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
;; Lava lamp simulation using metaballs algorithm.
;; Inspired by lavat (https://github.com/AngelJumbo/lavat).
;; Features smooth, organic blob motion rendered as ASCII art.

;;; Code:

(require 'iota-screens)
(require 'iota-timers)
(require 'iota-faces)

;;; Configuration

(defcustom iota-screens-lava-speed 0.05
  "Update interval in seconds for lava lamp animation."
  :type 'float
  :group 'iota-screens)

(defcustom iota-screens-lava-num-balls 5
  "Number of metaballs in the simulation.
Lower values improve performance."
  :type 'integer
  :group 'iota-screens)

(defcustom iota-screens-lava-radius 5.0
  "Base radius of metaballs (1-10 scale)."
  :type 'float
  :group 'iota-screens)

(defcustom iota-screens-lava-threshold 1.0
  "Threshold for metaball field rendering.
Lower values create larger blobs."
  :type 'float
  :group 'iota-screens)

;;; Faces

(defface iota-screens-lava-core
  '((t :foreground "#FF6B6B" :weight bold))
  "Face for the core of lava blobs (highest intensity)."
  :group 'iota-screens)

(defface iota-screens-lava-inner
  '((t :foreground "#FF8E72"))
  "Face for the inner area of lava blobs."
  :group 'iota-screens)

(defface iota-screens-lava-outer
  '((t :foreground "#FFA07A"))
  "Face for the outer area of lava blobs."
  :group 'iota-screens)

(defface iota-screens-lava-rim
  '((t :foreground "#4A3B47"))
  "Face for the rim of lava blobs."
  :group 'iota-screens)

;;; Animation State

(defvar-local iota-screens-lava--balls nil
  "List of metaballs. Each ball is (x y vx vy radius).")

(defvar-local iota-screens-lava--width 80
  "Buffer width in characters.")

(defvar-local iota-screens-lava--height 24
  "Buffer height in characters.")

(defvar-local iota-screens-lava--frame 0
  "Current animation frame number.")

;;; Density characters for ASCII rendering
(defconst iota-screens-lava--chars
  [?\s ?. ?: ?∘ ?○ ?◎ ?● ?█]
  "Characters representing density levels from low to high.")

;;; Metaball Functions

(defun iota-screens-lava--init-balls ()
  "Initialize metaballs with random positions and velocities."
  (setq iota-screens-lava--balls nil)
  (let ((w (float iota-screens-lava--width))
        (h (float iota-screens-lava--height)))
    (dotimes (_ iota-screens-lava-num-balls)
      (let* ((x (+ 5.0 (* (/ (random 10000) 10000.0) (- w 10.0))))
             (y (+ 5.0 (* (/ (random 10000) 10000.0) (- h 10.0))))
             ;; Slow velocities for smooth motion
             (vx (* (- (/ (random 10000) 5000.0) 1.0) 0.3))
             (vy (* (- (/ (random 10000) 5000.0) 1.0) 0.3))
             ;; Slight radius variation
             (radius (+ iota-screens-lava-radius
                        (* (- (/ (random 10000) 5000.0) 1.0) 1.0))))
        (push (list x y vx vy radius) iota-screens-lava--balls)))))

(defun iota-screens-lava--update-balls ()
  "Update metaball positions with bounce physics."
  (let ((w (float iota-screens-lava--width))
        (h (float iota-screens-lava--height)))
    (setq iota-screens-lava--balls
          (mapcar
           (lambda (ball)
             (let* ((x (nth 0 ball))
                    (y (nth 1 ball))
                    (vx (nth 2 ball))
                    (vy (nth 3 ball))
                    (r (nth 4 ball))
                    ;; Apply velocity
                    (new-x (+ x vx))
                    (new-y (+ y vy))
                    (new-vx vx)
                    (new-vy vy))
               ;; Bounce off walls with some damping
               (when (or (< new-x r) (> new-x (- w r)))
                 (setq new-vx (* -0.95 new-vx))
                 (setq new-x (max r (min (- w r) new-x))))
               (when (or (< new-y r) (> new-y (- h r)))
                 (setq new-vy (* -0.95 new-vy))
                 (setq new-y (max r (min (- h r) new-y))))
               ;; Add slight buoyancy (lava rises and falls)
               (let ((buoyancy (* 0.002 (sin (* iota-screens-lava--frame 0.02)))))
                 (setq new-vy (+ new-vy buoyancy)))
               (list new-x new-y new-vx new-vy r)))
           iota-screens-lava--balls))))

(defun iota-screens-lava--field-value (x y)
  "Calculate the metaball field value at position X, Y.
Higher values indicate closer to blob centers."
  (let ((total 0.0))
    (dolist (ball iota-screens-lava--balls)
      (let* ((bx (nth 0 ball))
             (by (nth 1 ball))
             (r (nth 4 ball))
             ;; Account for terminal character aspect ratio (chars are taller than wide)
             (dx (* (- x bx) 2.0))  ; Scale x distance
             (dy (- y by))
             (dist-sq (+ (* dx dx) (* dy dy)))
             ;; Metaball contribution: r²/d²
             (contribution (if (< dist-sq 0.1)
                               100.0  ; Prevent division by zero
                             (/ (* r r) dist-sq))))
        (setq total (+ total contribution))))
    total))

;;; Rendering

(defcustom iota-screens-lava-max-width 80
  "Maximum effective render width to prevent performance issues.
Field values are only calculated at this resolution and replicated."
  :type 'integer
  :group 'iota-screens)

(defcustom iota-screens-lava-max-height 30
  "Maximum effective render height to prevent performance issues.
Field values are only calculated at this resolution and replicated."
  :type 'integer
  :group 'iota-screens)

(defun iota-screens-lava--render ()
  "Render the metaball field to buffer.
Uses grid-based rendering with limited resolution for consistent performance."
  (let* ((width iota-screens-lava--width)
         (height iota-screens-lava--height)
         (render-width (max 1 (1- width)))
         ;; Calculate grid dimensions - limit to max resolution
         (grid-width (min render-width iota-screens-lava-max-width))
         (grid-height (min height iota-screens-lava-max-height))
         ;; Scale factors for mapping grid to screen
         (x-scale (/ (float render-width) grid-width))
         (y-scale (/ (float height) grid-height))
         (inhibit-read-only t)
         (char-count (length iota-screens-lava--chars))
         (threshold iota-screens-lava-threshold)
         ;; Pre-compute grid of field values (the expensive part, now limited)
         (grid (make-vector grid-height nil)))
    
    ;; Step 1: Compute field values on reduced grid
    (dotimes (gy grid-height)
      (let ((row (make-vector grid-width nil))
            (screen-y (* gy y-scale)))
        (dotimes (gx grid-width)
          (let* ((screen-x (* gx x-scale))
                 (field (iota-screens-lava--field-value screen-x screen-y)))
            (aset row gx field)))
        (aset grid gy row)))
    
    ;; Step 2: Render to buffer using precomputed grid
    (erase-buffer)
    (dotimes (y height)
      (let ((line (make-string render-width ?\s))
            (gy (min (1- grid-height) (floor (/ y y-scale)))))
        (dotimes (x render-width)
          (let* ((gx (min (1- grid-width) (floor (/ x x-scale))))
                 (field (aref (aref grid gy) gx))
                 (normalized (min 1.0 (/ field (* threshold 4.0)))))
            (when (> field (* threshold 0.3))
              (let* ((char-idx (min (1- char-count)
                                    (floor (* normalized (1- char-count)))))
                     (char (aref iota-screens-lava--chars char-idx))
                     (face (cond
                            ((> normalized 0.8) 'iota-screens-lava-core)
                            ((> normalized 0.6) 'iota-screens-lava-inner)
                            ((> normalized 0.4) 'iota-screens-lava-outer)
                            (t 'iota-screens-lava-rim))))
                (aset line x char)
                (put-text-property x (1+ x) 'face face line)))))
        (insert line)
        (when (< y (1- height))
          (insert "\n"))))
    (goto-char (point-min))))

;;; Animation Loop

(defun iota-screens-lava--animate-step (buffer-name)
  "Execute one animation frame for BUFFER-NAME."
  (condition-case err
      (let ((buf (get-buffer buffer-name)))
        (when (buffer-live-p buf)
          (let ((win (get-buffer-window buf)))
            (when win
              (with-current-buffer buf
                (let ((w (window-body-width win))
                      (h (window-body-height win)))
                  ;; Handle resize - scale existing balls instead of reinitializing
                  (when (or (/= w iota-screens-lava--width)
                            (/= h iota-screens-lava--height))
                    (let ((x-scale (if (> iota-screens-lava--width 0)
                                       (/ (float w) iota-screens-lava--width)
                                     1.0))
                          (y-scale (if (> iota-screens-lava--height 0)
                                       (/ (float h) iota-screens-lava--height)
                                     1.0)))
                      ;; Scale ball positions to new dimensions
                      (when iota-screens-lava--balls
                        (setq iota-screens-lava--balls
                              (mapcar (lambda (ball)
                                        (list (* (nth 0 ball) x-scale)
                                              (* (nth 1 ball) y-scale)
                                              (nth 2 ball)
                                              (nth 3 ball)
                                              (nth 4 ball)))
                                      iota-screens-lava--balls))))
                    (setq iota-screens-lava--width w
                          iota-screens-lava--height h))
                  ;; Initialize if no balls yet
                  (when (null iota-screens-lava--balls)
                    (iota-screens-lava--init-balls))
                  ;; Update and render
                  (iota-screens-lava--update-balls)
                  (iota-screens-lava--render)
                  (cl-incf iota-screens-lava--frame)))))))
    (error
     (message "Lavat animation error: %s" err))))

;;; Entry Point

(defun iota-screens-lava-start (&optional instance-id)
  "Start lava lamp animation in current buffer.
INSTANCE-ID is used to create unique timer keys for multi-screen support."
  (let ((buffer-name iota-screens--buffer-name))
    (when-let ((win (get-buffer-window buffer-name)))
      (with-current-buffer (get-buffer buffer-name)
        (let ((width (window-body-width win))
              (height (window-body-height win))
              (timer-key (if instance-id
                             (intern (format "screens-%d-lavat-animate" instance-id))
                           'screens-lavat-animate)))
          (setq iota-screens-lava--width width
                iota-screens-lava--height height
                iota-screens-lava--frame 0)
          (iota-screens-lava--init-balls)
          ;; Initial render
          (iota-screens-lava--render)
          ;; Start animation timer
          (iota-timers-run-with-timer
           timer-key
           0
           iota-screens-lava-speed
           #'iota-screens-lava--animate-step
           buffer-name))))))

(provide 'iota-screens-lava)

;;; iota-screens-lava.el ends here
