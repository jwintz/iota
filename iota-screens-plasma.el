;;; iota-screens-plasma.el --- Plasma effect screen animation -*- lexical-binding: t; -*-

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
;; Plasma effect screen animation - organic sinusoidal patterns.

;;; Code:

(require 'iota-screens)
(require 'iota-timers)
(require 'iota-faces)

;;; Configuration

(defcustom iota-screens-plasma-speed 0.08
  "Update interval in seconds for plasma animation."
  :type 'float
  :group 'iota-screens)

;;; Faces

(defface iota-screens-plasma-bright
  '((t :foreground "#88C0D0"))
  "Face for bright plasma elements."
  :group 'iota-screens)

(defface iota-screens-plasma-medium
  '((t :foreground "#5E81AC"))
  "Face for medium intensity plasma elements."
  :group 'iota-screens)

(defface iota-screens-plasma-dim
  '((t :foreground "#4C566A"))
  "Face for dim plasma elements."
  :group 'iota-screens)

(defface iota-screens-plasma-accent
  '((t :foreground "#B48EAD"))
  "Face for accent plasma elements."
  :group 'iota-screens)

;;; Animation State

(defvar-local iota-screens-plasma--width 80
  "Buffer width in characters.")

(defvar-local iota-screens-plasma--height 24
  "Buffer height in characters.")

(defvar-local iota-screens-plasma--frame 0
  "Current animation frame number.")

;;; Character set

(defconst iota-screens-plasma--chars
  " .'`^\",:;Il!i><~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"
  "Characters for plasma effect.")

;;; Plasma Effect

(defun iota-screens-plasma--value (x y time)
  "Calculate plasma value at X, Y position at TIME.
Returns a value between 0 and 1."
  (let* ((scale 0.05)
         (t-slow (* time 0.1))
         ;; Combine multiple sine waves for organic pattern
         (v1 (sin (+ (* x scale) t-slow)))
         (v2 (sin (+ (* y scale 1.2) (* t-slow 0.7))))
         (v3 (sin (+ (* (+ x y) scale 0.5) (* t-slow 0.3))))
         (v4 (sin (* (sqrt (+ (* (- x 40) (- x 40))
                               (* (- y 12) (- y 12))))
                     scale)))
         ;; Combine and normalize to 0-1
         (combined (+ v1 v2 v3 v4)))
    (/ (+ combined 4.0) 8.0)))

(defun iota-screens-plasma--render ()
  "Render plasma effect."
  (let* ((width iota-screens-plasma--width)
         (height iota-screens-plasma--height)
         (render-width (max 1 (1- width)))
         (time iota-screens-plasma--frame)
         (char-count (length iota-screens-plasma--chars))
         (inhibit-read-only t))
    (erase-buffer)
    (dotimes (y height)
      (let ((line (make-string render-width ?\s)))
        (dotimes (x render-width)
          (let* ((value (iota-screens-plasma--value x y time))
                 (char-idx (floor (* value (1- char-count))))
                 (char (aref iota-screens-plasma--chars char-idx))
                 (face (cond
                        ((> value 0.75) 'iota-screens-plasma-bright)
                        ((> value 0.5) 'iota-screens-plasma-medium)
                        ((> value 0.25) 'iota-screens-plasma-dim)
                        (t 'iota-screens-plasma-accent))))
            (aset line x char)
            (put-text-property x (1+ x) 'face face line)))
        (insert line)
        (when (< y (1- height))
          (insert "\n"))))
    (goto-char (point-min))))

;;; Animation Loop

(defun iota-screens-plasma--animate-step (buffer-name)
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
                  (when (or (/= w iota-screens-plasma--width)
                            (/= h iota-screens-plasma--height))
                    (setq iota-screens-plasma--width w
                          iota-screens-plasma--height h))
                  ;; Render
                  (iota-screens-plasma--render)
                  (cl-incf iota-screens-plasma--frame)))))))
    (error
     (message "Plasma animation error: %s" err))))

;;; Entry Point

(defun iota-screens-plasma-start (&optional instance-id)
  "Start plasma animation in current buffer.
INSTANCE-ID is used to create unique timer keys for multi-screen support."
  (let ((buffer-name iota-screens--buffer-name))
    (when-let ((win (get-buffer-window buffer-name)))
      (with-current-buffer (get-buffer buffer-name)
        (let ((width (window-body-width win))
              (height (window-body-height win))
              (timer-key (if instance-id
                             (intern (format "screens-%d-plasma-animate" instance-id))
                           'screens-plasma-animate)))
          (setq iota-screens-plasma--width width
                iota-screens-plasma--height height
                iota-screens-plasma--frame 0)
          ;; Initial render
          (iota-screens-plasma--render)
          ;; Start animation timer
          (iota-timers-run-with-timer
           timer-key
           0
           iota-screens-plasma-speed
           #'iota-screens-plasma--animate-step
           buffer-name))))))

(provide 'iota-screens-plasma)

;;; iota-screens-plasma.el ends here
