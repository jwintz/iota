;;; iota-screens-stars.el --- Starfield screen animation -*- lexical-binding: t; -*-

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
;; Starfield effect screen animation - parallax star movement.

;;; Code:

(require 'iota-screens)
(require 'iota-timers)
(require 'iota-faces)
(require 'iota-icons)

;;; Configuration

(defcustom iota-screens-stars-speed 0.08
  "Update interval in seconds for starfield animation."
  :type 'float
  :group 'iota-screens)

(defcustom iota-screens-stars-count 100
  "Number of stars in the starfield."
  :type 'integer
  :group 'iota-screens)

;;; Faces

(defface iota-screens-stars-bright
  '((t :foreground "#ECEFF4"))
  "Face for bright stars."
  :group 'iota-screens)

(defface iota-screens-stars-medium
  '((t :foreground "#D8DEE9"))
  "Face for medium stars."
  :group 'iota-screens)

(defface iota-screens-stars-dim
  '((t :foreground "#4C566A"))
  "Face for dim stars."
  :group 'iota-screens)

;;; Animation State

(defvar-local iota-screens-stars--width 80
  "Buffer width in characters.")

(defvar-local iota-screens-stars--height 24
  "Buffer height in characters.")

(defvar-local iota-screens-stars--frame 0
  "Current animation frame number.")

(defvar-local iota-screens-stars--stars nil
  "List of star positions.")

;;; Starfield Effect

(defun iota-screens-stars--init ()
  "Initialize stars for starfield effect."
  (setq iota-screens-stars--stars nil)
  (dotimes (_ iota-screens-stars-count)
    (push (list (random iota-screens-stars--width)
                (random iota-screens-stars--height)
                (1+ (random 3))  ; speed/brightness layer
                (random 4))      ; twinkle phase
          iota-screens-stars--stars)))

(defun iota-screens-stars--update ()
  "Update star positions."
  (setq iota-screens-stars--stars
        (mapcar
         (lambda (star)
           (let* ((x (nth 0 star))
                  (y (nth 1 star))
                  (speed (nth 2 star))
                  (phase (nth 3 star))
                  (new-x (- x speed)))
             (if (< new-x 0)
                 (list (1- iota-screens-stars--width)
                       (random iota-screens-stars--height)
                       speed
                       (random 4))
               (list new-x y speed phase))))
         iota-screens-stars--stars)))

(defun iota-screens-stars--render ()
  "Render starfield effect."
  (let* ((width iota-screens-stars--width)
         (height iota-screens-stars--height)
         (render-width (max 1 (1- width)))
         (inhibit-read-only t)
         (frame iota-screens-stars--frame)
         ;; Build a sparse grid of star positions
         (grid (make-hash-table :test 'equal)))
    ;; Initialize stars if needed
    (unless iota-screens-stars--stars
      (iota-screens-stars--init))
    (iota-screens-stars--update)
    
    ;; Place stars in grid
    (dolist (star iota-screens-stars--stars)
      (let* ((x (floor (nth 0 star)))
             (y (nth 1 star))
             (speed (nth 2 star))
             (phase (nth 3 star))
             ;; Twinkle effect
             (twinkle (mod (+ frame phase) 4))
             ;; Use iota-icons for stars (nerd-icons when available)
             (icon-str (pcase speed
                         (1 (if (= twinkle 0)
                                (iota-icon 'mdicon "nf-md-circle_small" ".")
                              ","))
                         (2 (if (= twinkle 0)
                                (iota-icon 'codicon "nf-cod-star_empty" "*")
                              "+"))
                         (3 (if (= twinkle 0)
                                (iota-icon 'codicon "nf-cod-star_full" "★")
                              (iota-icon 'codicon "nf-cod-star_empty" "☆")))))
             (face (pcase speed
                     (1 'iota-screens-stars-dim)
                     (2 'iota-screens-stars-medium)
                     (3 'iota-screens-stars-bright))))
        (when (and (>= x 0) (< x render-width)
                   (>= y 0) (< y height))
          (puthash (cons x y) (cons icon-str face) grid))))
    
    ;; Render grid
    (erase-buffer)
    (dotimes (y height)
      (dotimes (x render-width)
        (let ((cell (gethash (cons x y) grid)))
          (if cell
              (insert (propertize (car cell) 'face (cdr cell)))
            (insert " "))))
      (when (< y (1- height))
        (insert "\n")))
    (goto-char (point-min))))

;;; Animation Loop

(defun iota-screens-stars--animate-step (buffer-name)
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
                  (when (or (/= w iota-screens-stars--width)
                            (/= h iota-screens-stars--height))
                    (setq iota-screens-stars--width w
                          iota-screens-stars--height h)
                    (iota-screens-stars--init))
                  ;; Render
                  (iota-screens-stars--render)
                  (cl-incf iota-screens-stars--frame)))))))
    (error
     (message "Stars animation error: %s" err))))

;;; Entry Point

(defun iota-screens-stars-start (&optional instance-id)
  "Start starfield animation in current buffer.
INSTANCE-ID is used to create unique timer keys for multi-screen support."
  (let ((buffer-name iota-screens--buffer-name))
    (when-let ((win (get-buffer-window buffer-name)))
      (with-current-buffer (get-buffer buffer-name)
        (let ((width (window-body-width win))
              (height (window-body-height win))
              (timer-key (if instance-id
                             (intern (format "screens-%d-stars-animate" instance-id))
                           'screens-stars-animate)))
          (setq iota-screens-stars--width width
                iota-screens-stars--height height
                iota-screens-stars--frame 0)
          (iota-screens-stars--init)
          ;; Initial render
          (iota-screens-stars--render)
          ;; Start animation timer
          (iota-timers-run-with-timer
           timer-key
           0
           iota-screens-stars-speed
           #'iota-screens-stars--animate-step
           buffer-name))))))

(provide 'iota-screens-stars)

;;; iota-screens-stars.el ends here
