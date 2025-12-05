;;; iota-screens-life.el --- Conway's Game of Life animation for iota-screens -*- lexical-binding: t; -*-

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
;; Conway's Game of Life implementation for iota-screens.
;; A zero-player game that simulates cellular evolution.

;;; Code:

(require 'iota-screens)
(require 'iota-timers)
(require 'iota-faces)

;;; Configuration

(defcustom iota-screens-life-speed 0.1
  "Update interval in seconds for Life animation."
  :type 'float
  :group 'iota-screens)

(defcustom iota-screens-life-char "●"
  "Character to use for live cells."
  :type 'string
  :group 'iota-screens)

(defcustom iota-screens-life-density 0.3
  "Initial density of live cells (0.0-1.0)."
  :type 'float
  :group 'iota-screens)

;;; Animation State

(defvar-local iota-screens-life--grid nil
  "Current generation grid (vector of vectors).")

(defvar-local iota-screens-life--next-grid nil
  "Next generation grid buffer.")

(defvar-local iota-screens-life--width 0
  "Grid width.")

(defvar-local iota-screens-life--height 0
  "Grid height.")

(defvar-local iota-screens-life--frame 0
  "Generation counter.")

(defvar-local iota-screens-life--stagnation-counter 0
  "Counter to detect stagnant states.")

(defvar-local iota-screens-life--last-population 0
  "Population of previous generation.")

;;; Core Logic

(defun iota-screens-life--init-grid (width height)
  "Initialize grid with random state."
  (setq iota-screens-life--width width)
  (setq iota-screens-life--height height)
  (setq iota-screens-life--grid (make-vector height nil))
  (setq iota-screens-life--next-grid (make-vector height nil))
  
  (dotimes (y height)
    (let ((row (make-vector width nil))
          (next-row (make-vector width nil)))
      (dotimes (x width)
        (aset row x (< (random 1000) (* iota-screens-life-density 1000))))
      (aset iota-screens-life--grid y row)
      (aset iota-screens-life--next-grid y next-row))))

(defun iota-screens-life--count-neighbors (x y)
  "Count live neighbors for cell at X,Y."
  (let ((count 0)
        (w iota-screens-life--width)
        (h iota-screens-life--height))
    (dolist (dy '(-1 0 1))
      (dolist (dx '(-1 0 1))
        (unless (and (= dx 0) (= dy 0))
          ;; Wrap around edges (toroidal)
          (let ((nx (mod (+ x dx) w))
                (ny (mod (+ y dy) h)))
            (when (aref (aref iota-screens-life--grid ny) nx)
              (setq count (1+ count)))))))
    count))

(defun iota-screens-life--evolve ()
  "Compute next generation."
  (let ((population 0)
        (changed nil))
    
    (dotimes (y iota-screens-life--height)
      (let ((row (aref iota-screens-life--grid y))
            (next-row (aref iota-screens-life--next-grid y)))
        (dotimes (x iota-screens-life--width)
          (let* ((alive (aref row x))
                 (neighbors (iota-screens-life--count-neighbors x y))
                 (next-alive (if alive
                                 (and (>= neighbors 2) (<= neighbors 3))
                               (= neighbors 3))))
            (aset next-row x next-alive)
            (when next-alive (setq population (1+ population)))
            (when (not (eq alive next-alive)) (setq changed t))))))

    ;; Swap grids
    (let ((temp iota-screens-life--grid))
      (setq iota-screens-life--grid iota-screens-life--next-grid)
      (setq iota-screens-life--next-grid temp))

    ;; Check stagnation
    (if (or (not changed)
            (= population iota-screens-life--last-population))
        (setq iota-screens-life--stagnation-counter (1+ iota-screens-life--stagnation-counter))
      (setq iota-screens-life--stagnation-counter 0))
    
    (setq iota-screens-life--last-population population)
    
    ;; Reset if stagnant or empty
    (when (or (> iota-screens-life--stagnation-counter 100)
              (< population 10))
      (iota-screens-life--init-grid iota-screens-life--width iota-screens-life--height)
      (setq iota-screens-life--stagnation-counter 0))))

(defun iota-screens-life--render ()
  "Render grid to buffer."
  (let ((render-width (1- iota-screens-life--width)))  ;; Prevent line wrapping
    (erase-buffer)
    (dotimes (y iota-screens-life--height)
      (let ((row (aref iota-screens-life--grid y))
            (line-str (make-string render-width ?\s)))
        (dotimes (x render-width)
          (when (aref row x)
            (aset line-str x (string-to-char iota-screens-life-char))
            (put-text-property x (1+ x) 'face 'iota-screens-matrix-bright line-str)))
        (insert line-str)
        (when (< y (1- iota-screens-life--height))
          (insert "\n"))))
    (goto-char (point-min))
    (set-window-start (get-buffer-window (current-buffer)) (point-min))))

(defun iota-screens-life--animate-step (buffer-name)
  "Perform one animation frame for BUFFER-NAME."
  (condition-case err
      (let ((buf (get-buffer buffer-name)))
        (when (buffer-live-p buf)
          (let ((win (get-buffer-window buf)))
            (when win
              (with-current-buffer buf
                (let ((inhibit-read-only t)
                      (w (window-body-width win))
                      (h (window-body-height win)))
                  
                  ;; Handle resize
                  (when (or (not (= w iota-screens-life--width))
                            (not (= h iota-screens-life--height)))
                    (iota-screens-life--init-grid w h))
                  
                  (iota-screens-life--evolve)
                  (iota-screens-life--render)
                  (setq iota-screens-life--frame (1+ iota-screens-life--frame))))))))
    (error (message "Life animation error: %s" err))))

;;; Entry Point

(defun iota-screens-life-start (&optional instance-id)
  "Start Game of Life in current buffer.
INSTANCE-ID is used to create unique timer keys for multi-screen support."
  (let ((buffer-name iota-screens--buffer-name))
    (when-let ((win (get-buffer-window buffer-name)))
      (with-current-buffer (get-buffer buffer-name)
        (let ((width (window-body-width win))
              (height (window-body-height win))
              (inhibit-read-only t)
              (timer-key (if instance-id
                             (intern (format "screens-%d-life-animate" instance-id))
                           'screens-life-animate)))
          (erase-buffer)
          (iota-screens-life--init-grid width height)
          (iota-screens-life--render)
          
          ;; Start timer - pass buffer-name to the callback
          (iota-timers-run-with-timer
           timer-key
           0
           iota-screens-life-speed
           #'iota-screens-life--animate-step
           buffer-name))))))

(provide 'iota-screens-life)
;;; iota-screens-life.el ends here

