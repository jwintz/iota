;;; iota-screens-earth.el --- Rotating Earth screen animation -*- lexical-binding: t; -*-

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
;; Rotating ASCII Earth/globe screen animation.
;; Inspired by ascsaver's globe animation.

;;; Code:

(require 'iota-screens)
(require 'iota-timers)
(require 'iota-faces)

;;; Configuration

(defcustom iota-screens-earth-speed 0.1
  "Update interval in seconds for earth animation."
  :type 'float
  :group 'iota-screens)

;;; Faces

(defface iota-screens-earth-land
  '((t :foreground "#A3BE8C"))
  "Face for land masses."
  :group 'iota-screens)

(defface iota-screens-earth-water
  '((t :foreground "#5E81AC"))
  "Face for ocean/water."
  :group 'iota-screens)

(defface iota-screens-earth-ice
  '((t :foreground "#ECEFF4"))
  "Face for ice/poles."
  :group 'iota-screens)

(defface iota-screens-earth-dark
  '((t :foreground "#3B4252"))
  "Face for dark side of earth."
  :group 'iota-screens)

;;; Animation State

(defvar-local iota-screens-earth--width 80
  "Buffer width in characters.")

(defvar-local iota-screens-earth--height 24
  "Buffer height in characters.")

(defvar-local iota-screens-earth--frame 0
  "Current animation frame number.")

;;; Earth Map Data (simplified continental shapes)
;; 1 = land, 0 = water, 2 = ice
;; This is a simplified 36x18 map of Earth (10° per cell)
(defconst iota-screens-earth--map
  [;; Row 0: North Pole
   [2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2]
   ;; Row 1: 80°N - Arctic
   [2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2]
   ;; Row 2: 70°N - Northern Canada/Siberia
   [0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0]
   ;; Row 3: 60°N - Alaska/Canada/Scandinavia/Russia
   [0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0]
   ;; Row 4: 50°N - US/UK/Europe/Russia
   [0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0]
   ;; Row 5: 40°N - US/Europe/China
   [0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0]
   ;; Row 6: 30°N - Mexico/Africa/India
   [0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0]
   ;; Row 7: 20°N - Caribbean/Sahara/India
   [0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0]
   ;; Row 8: 10°N - Central America/Africa
   [0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0]
   ;; Row 9: Equator
   [0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 1 1 0 0 0 0 0]
   ;; Row 10: 10°S - South America/Africa
   [0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0]
   ;; Row 11: 20°S - Brazil/Africa/Indonesia
   [0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0]
   ;; Row 12: 30°S - Argentina/South Africa/Australia
   [0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0]
   ;; Row 13: 40°S - Chile/New Zealand
   [0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0]
   ;; Row 14: 50°S - Patagonia
   [0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   ;; Row 15: 60°S - Southern Ocean
   [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
   ;; Row 16: 70°S - Antarctica
   [2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2]
   ;; Row 17: South Pole
   [2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2]]
  "Simplified Earth map data.")

;;; Rendering

(defun iota-screens-earth--get-map-value (lat-idx lon-idx rotation)
  "Get map value at LAT-IDX, LON-IDX with ROTATION offset."
  (let* ((map-width 36)
         (map-height 18)
         (rotated-lon (mod (+ lon-idx rotation) map-width)))
    (if (and (>= lat-idx 0) (< lat-idx map-height)
             (>= rotated-lon 0) (< rotated-lon map-width))
        (aref (aref iota-screens-earth--map lat-idx) rotated-lon)
      0)))

(defun iota-screens-earth--render ()
  "Render the rotating Earth."
  (let* ((width iota-screens-earth--width)
         (height iota-screens-earth--height)
         (render-width (max 1 (1- width)))
         (center-x (/ render-width 2))
         (center-y (/ height 2))
         (radius (min (- center-x 2) (- center-y 1)))
         (rotation (mod iota-screens-earth--frame 36))
         (inhibit-read-only t))
    (erase-buffer)
    (dotimes (y height)
      (let ((line (make-string render-width ?\s)))
        (dotimes (x render-width)
          (let* ((dx (- x center-x))
                 (dy (* 2.0 (- y center-y)))  ; Adjust for character aspect ratio
                 (dist (sqrt (+ (* dx dx) (* dy dy)))))
            (when (<= dist radius)
              ;; Map screen coords to sphere coords (latitude/longitude)
              (let* ((sphere-x (/ (float dx) radius))
                     (sphere-y (/ (float dy) (float (* 2 radius))))
                     ;; Calculate longitude index (0-35)
                     (lon-idx (floor (* (+ sphere-x 1.0) 18)))
                     ;; Calculate latitude index (0-17)
                     (lat-idx (floor (* (+ sphere-y 1.0) 9)))
                     ;; Check if on visible side (front of sphere)
                     (depth (sqrt (max 0 (- 1.0 (* sphere-x sphere-x) (* sphere-y sphere-y)))))
                     (visible (> depth 0.1))
                     (map-val (if visible
                                  (iota-screens-earth--get-map-value lat-idx lon-idx rotation)
                                0))
                     (char (cond
                            ((not visible) ?.)
                            ((= map-val 2) ?#)   ; Ice
                            ((= map-val 1) ?@)   ; Land
                            (t ?~)))             ; Water
                     (face (cond
                            ((not visible) 'iota-screens-earth-dark)
                            ((= map-val 2) 'iota-screens-earth-ice)
                            ((= map-val 1) 'iota-screens-earth-land)
                            (t 'iota-screens-earth-water))))
                (aset line x char)
                (put-text-property x (1+ x) 'face face line)))))
        (insert line)
        (when (< y (1- height))
          (insert "\n"))))
    (goto-char (point-min))))

;;; Animation Loop

(defun iota-screens-earth--animate-step (buffer-name)
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
                  (when (or (/= w iota-screens-earth--width)
                            (/= h iota-screens-earth--height))
                    (setq iota-screens-earth--width w
                          iota-screens-earth--height h))
                  ;; Render
                  (iota-screens-earth--render)
                  (cl-incf iota-screens-earth--frame)))))))
    (error
     (message "Earth animation error: %s" err))))

;;; Entry Point

(defun iota-screens-earth-start (&optional instance-id)
  "Start earth animation in current buffer.
INSTANCE-ID is used to create unique timer keys for multi-screen support."
  (let ((buffer-name iota-screens--buffer-name))
    (when-let ((win (get-buffer-window buffer-name)))
      (with-current-buffer (get-buffer buffer-name)
        (let ((width (window-body-width win))
              (height (window-body-height win))
              (timer-key (if instance-id
                             (intern (format "screens-%d-earth-animate" instance-id))
                           'screens-earth-animate)))
          (setq iota-screens-earth--width width
                iota-screens-earth--height height
                iota-screens-earth--frame 0)
          ;; Initial render
          (iota-screens-earth--render)
          ;; Start animation timer
          (iota-timers-run-with-timer
           timer-key
           0
           iota-screens-earth-speed
           #'iota-screens-earth--animate-step
           buffer-name))))))

(provide 'iota-screens-earth)

;;; iota-screens-earth.el ends here
