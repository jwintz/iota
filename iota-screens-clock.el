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

(defun iota-screens-clock--render-time ()
  "Render current time."
  (let* ((time-str (format-time-string "%H:%M:%S"))
         (lines (make-vector 5 ""))
         (win (get-buffer-window iota-screens--buffer-name)))
    
    (when win
      (with-current-buffer (get-buffer iota-screens--buffer-name)
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
          (set-window-start win (point-min)))))))

(defun iota-screens-clock--animate-step ()
  "Update clock."
  (condition-case nil
      (iota-screens-clock--render-time)
    (error (iota-timers-cancel 'screens-clock-animate))))

;;; Entry Point

(defun iota-screens-clock-start ()
  "Start clock screen."
  (message "Clock: Starting...")
  (iota-screens-clock--render-time)
  (iota-timers-run-with-timer
   'screens-clock-animate
   0
   iota-screens-clock-update-interval
   #'iota-screens-clock--animate-step))

(provide 'iota-screens-clock)
;;; iota-screens-clock.el ends here
