;;; iota-screens-clock.el --- Digital clock screen for iota-screens -*- lexical-binding: t; -*-

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
;; Digital clock screensaver.
;; Displays current time with large ASCII digits.

;;; Code:

(require 'iota-screens)
(require 'iota-timers)
(require 'iota-faces)

;;; Configuration

(defcustom iota-screens-clock-update-interval 1.0
  "Update interval in seconds."
  :type 'float
  :group 'iota-screens)

;;; ASCII Fonts

(defconst iota-screens-clock--digits
  ;; 0
  '("  ████  "
    " ██  ██ "
    " ██  ██ "
    " ██  ██ "
    "  ████  ")
  ;; 1
  ;; ... vector of lists/strings ...
  ;; Ideally we use a vector of shapes.
  )
;; Let's try a simpler block font that is easier to maintain inline
;; 5 lines high

(defun iota-screens-clock--get-digit (char)
  (pcase char
    (?0 '("  ████  " " ██  ██ " " ██  ██ " " ██  ██ " "  ████  "))
    (?1 '("   ██   " "  ███   " "   ██   " "   ██   " "  ████  "))
    (?2 '("  ████  " "     ██ " "  ████  " " ██     " " ██████ "))
    (?3 '("  ████  " "     ██ " "   ███  " "     ██ " "  ████  "))
    (?4 '(" ██  ██ " " ██  ██ " " ██████ " "     ██ " "     ██ "))
    (?5 '(" ██████ " " ██     " " █████  " "     ██ " " █████  "))
    (?6 '("  ████  " " ██     " " █████  " " ██  ██ " "  ████  "))
    (?7 '(" ██████ " "     ██ " "    ██  " "   ██   " "   ██   "))
    (?8 '("  ████  " " ██  ██ " "  ████  " " ██  ██ " "  ████  "))
    (?9 '("  ████  " " ██  ██ " "  █████ " "     ██ " "  ████  "))
    (?: '("        " "   ██   " "        " "   ██   " "        "))
    (_  '("        " "        " "        " "        " "        "))))

;;; Core Logic

(defun iota-screens-clock--render-time (buffer-name)
  "Render current time to BUFFER-NAME."
  (let ((buf (get-buffer buffer-name)))
    (when (buffer-live-p buf)
      (let ((win (get-buffer-window buf)))
        (when win
          (let* ((time-str (format-time-string "%H:%M:%S"))
                 (lines (make-vector 5 "")))
            (with-current-buffer buf
              (let ((inhibit-read-only t)
                    (w (window-body-width win))
                    (h (window-body-height win)))
                (erase-buffer)
                
                ;; Build the clock string lines
                (dotimes (i (length time-str))
                  (let* ((char (aref time-str i))
                         (digit-lines (iota-screens-clock--get-digit char)))
                    (dotimes (j 5)
                      (aset lines j (concat (aref lines j) (nth j digit-lines) " ")))))
                
                ;; Center vertically
                (let ((padding-top (/ (- h 5) 2)))
                  (dotimes (_ (max 0 padding-top))
                    (insert "\n")))
                
                ;; Center horizontally
                (dotimes (j 5)
                  (let ((line (aref lines j)))
                    (let ((padding-left (/ (- w (length line)) 2)))
                      (insert (make-string (max 0 padding-left) ?\s))
                      ;; Propertize
                      (insert (propertize line 'face 'iota-screens-matrix-head))
                      (insert "\n"))))
                
                (goto-char (point-min))
                (set-window-start win (point-min))))))))))

(defun iota-screens-clock--animate-step (buffer-name)
  "Update clock for BUFFER-NAME."
  (condition-case err
      (iota-screens-clock--render-time buffer-name)
    (error (message "Clock animation error: %s" err))))

;;; Entry Point

(defun iota-screens-clock-start (&optional instance-id)
  "Start clock screen.
INSTANCE-ID is used to create unique timer keys for multi-screen support."
  (let ((buffer-name iota-screens--buffer-name))
    (iota-screens-clock--render-time buffer-name)
    (let ((timer-key (if instance-id
                         (intern (format "screens-%d-clock-animate" instance-id))
                       'screens-clock-animate)))
      ;; Start timer - pass buffer-name to the callback
      (iota-timers-run-with-timer
       timer-key
       0
       iota-screens-clock-update-interval
       #'iota-screens-clock--animate-step
       buffer-name))))

(provide 'iota-screens-clock)
;;; iota-screens-clock.el ends here

