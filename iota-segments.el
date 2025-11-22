;;; iota-segments.el --- Built-in segments for I O T Λ modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: modeline
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Built-in segment library for I O T Λ (I Ø T Δ) modeline.
;; Provides common segments: buffer name, position, mode, time, etc.

;;; Code:

(require 'cl-lib)
(require 'iota-segment)
(require 'iota-theme)

;;; Buffer Information Segments

(defun iota-segment-buffer-name ()
  "Create buffer name segment."
  (iota-segment-create
   :id 'buffer-name
   :text (lambda ()
           (let ((name (buffer-name)))
             (if (buffer-modified-p)
                 (concat (propertize "●" 'face (iota-theme-get-accent-face)) " " name)
               name)))
   :face #'iota-theme-get-modeline-face
   :align 'left
   :priority 100
   :help-echo (lambda ()
                (format "%s\n%s"
                        (buffer-file-name)
                        (if (buffer-modified-p) "Modified" "Saved")))))

(defun iota-segment-buffer-size ()
  "Create buffer size segment."
  (iota-segment-create
   :id 'buffer-size
   :text (lambda ()
           (let ((size (buffer-size)))
             (cond
              ((> size 1048576) (format "%.1fM" (/ size 1048576.0)))
              ((> size 1024) (format "%.1fK" (/ size 1024.0)))
              (t (format "%dB" size)))))
   :face #'iota-theme-get-modeline-face
   :align 'left
   :priority 30
   :update-on 'timer
   :help-echo "Buffer size"))

(defun iota-segment-buffer-encoding ()
  "Create buffer encoding segment."
  (iota-segment-create
   :id 'buffer-encoding
   :text (lambda ()
           (let ((coding (symbol-name buffer-file-coding-system)))
             (upcase (replace-regexp-in-string "-.*$" "" coding))))
   :face #'iota-theme-get-modeline-face
   :align 'right
   :priority 20
   :visible-fn (lambda () buffer-file-coding-system)
   :help-echo "Buffer encoding"))

;;; Position Segments

(defun iota-segment-position ()
  "Create line:column position segment."
  (iota-segment-create
   :id 'position
   :text (lambda ()
           (format "%d:%d"
                   (line-number-at-pos)
                   (current-column)))
   :face #'iota-theme-get-modeline-face
   :align 'right
   :priority 80
   :update-on 'point
   :help-echo "Line:Column"))

(defun iota-segment-position-percent ()
  "Create position percentage segment."
  (iota-segment-create
   :id 'position-percent
   :text (lambda ()
           (format "%d%%"
                   (if (= (point-max) (point-min))
                       0
                     (/ (* 100 (- (point) (point-min)))
                        (- (point-max) (point-min))))))
   :face #'iota-theme-get-modeline-face
   :align 'right
   :priority 70
   :update-on 'point
   :help-echo "Position in buffer"))

(defun iota-segment-region-info ()
  "Create region info segment (only visible when region active)."
  (iota-segment-create
   :id 'region-info
   :text (lambda ()
           (let ((lines (count-lines (region-beginning) (region-end)))
                 (chars (- (region-end) (region-beginning))))
             (format "<%d:%d>" lines chars)))
   :face #'iota-theme-get-highlight-face
   :align 'right
   :priority 90
   :visible-fn (lambda () (use-region-p))
   :help-echo "Selection: lines:characters"))

;;; Mode Segments

(defun iota-segment-major-mode ()
  "Create major mode segment."
  (iota-segment-create
   :id 'major-mode
   :text (lambda ()
           (let ((name (format-mode-line mode-name)))
             (if (stringp name)
                 (substring-no-properties name)
               (format "%s" name))))
   :face (lambda () (list :inherit (iota-theme-get-modeline-face) :weight 'bold))
   :align 'right
   :priority 60
   :help-echo (lambda ()
                (format "%s\n%s"
                        (symbol-name major-mode)
                        (documentation major-mode)))))

(defun iota-segment-minor-modes ()
  "Create minor modes segment."
  (iota-segment-create
   :id 'minor-modes
   :text (lambda ()
           (let ((modes (delq nil
                              (mapcar (lambda (mode)
                                        (when (and (boundp mode) (symbol-value mode))
                                          (substring (symbol-name mode) 0 -5)))
                                      minor-mode-list))))
             (if modes
                 (concat "[" (mapconcat #'identity (cl-subseq modes 0 (min 3 (length modes))) ",") "]")
               "")))
   :face #'iota-theme-get-modeline-face
   :align 'right
   :priority 40
   :visible-fn (lambda () minor-mode-list)
   :help-echo "Active minor modes"))

;;; VCS Segments

(defun iota-segment-vcs-branch ()
  "Create VCS branch segment."
  (iota-segment-create
   :id 'vcs-branch
   :text (lambda ()
           (when-let ((branch (vc-git-mode-line-string (buffer-file-name))))
             (replace-regexp-in-string "Git[-:]" "" branch)))
   :face #'iota-theme-get-accent-face
   :align 'left
   :priority 50
   :update-on 'timer
   :visible-fn (lambda () (and vc-mode (buffer-file-name)))
   :help-echo "VCS branch"))

(defun iota-segment-vcs-status ()
  "Create VCS status segment."
  (iota-segment-create
   :id 'vcs-status
   :text (lambda ()
           (cond
            ((and (buffer-file-name)
                  (vc-git-working-revision (buffer-file-name)))
             "●")
            (t "")))
   :face (lambda ()
           (if (buffer-modified-p)
               (iota-theme-get-warning-face)
             (iota-theme-get-success-face)))
   :align 'left
   :priority 55
   :visible-fn (lambda () (and vc-mode (buffer-file-name)))
   :help-echo "VCS status"))

;;; Time/Date Segments

(defun iota-segment-time ()
  "Create time segment."
  (iota-segment-create
   :id 'time
   :text (lambda () (format-time-string "%H:%M"))
   :face #'iota-theme-get-modeline-face
   :align 'right
   :priority 10
   :update-on 'timer
   :help-echo (lambda () (format-time-string "%Y-%m-%d %H:%M:%S"))))

(defun iota-segment-date ()
  "Create date segment."
  (iota-segment-create
   :id 'date
   :text (lambda () (format-time-string "%Y-%m-%d"))
   :face #'iota-theme-get-modeline-face
   :align 'right
   :priority 5
   :update-on 'timer
   :help-echo "Current date"))

;;; System Segments

(defun iota-segment-battery ()
  "Create battery status segment."
  (iota-segment-create
   :id 'battery
   :text (lambda ()
           (when (and (require 'battery nil t)
                      (functionp 'battery-format))
             (let* ((data (funcall battery-status-function))
                    (percent (battery-format "%p" data)))
               (if (string-match "^[0-9]+" percent)
                   (format "%s%%" (match-string 0 percent))
                 ""))))
   :face (lambda ()
           (when (and (require 'battery nil t)
                      (functionp 'battery-format))
             (let* ((data (funcall battery-status-function))
                    (percent (string-to-number (battery-format "%p" data))))
               (cond
                ((< percent 20) (iota-theme-get-error-face))
                ((< percent 50) (iota-theme-get-warning-face))
                (t (iota-theme-get-success-face))))))
   :align 'right
   :priority 15
   :update-on 'timer
   :visible-fn (lambda () (and (require 'battery nil t)
                               (functionp 'battery-format)
                               battery-status-function))
   :help-echo "Battery level"))

;;; Flycheck/Flymake Segments

(defun iota-segment-flycheck ()
  "Create Flycheck error count segment."
  (iota-segment-create
   :id 'flycheck
   :text (lambda ()
           (when (bound-and-true-p flycheck-mode)
             (let ((counts (flycheck-count-errors flycheck-current-errors)))
               (format "✗%d ⚠%d"
                       (or (cdr (assq 'error counts)) 0)
                       (or (cdr (assq 'warning counts)) 0)))))
   :face (lambda ()
           (when (bound-and-true-p flycheck-mode)
             (let ((counts (flycheck-count-errors flycheck-current-errors)))
               (if (> (or (cdr (assq 'error counts)) 0) 0)
                   (iota-theme-get-error-face)
                 (iota-theme-get-warning-face)))))
   :align 'right
   :priority 65
   :visible-fn (lambda () (bound-and-true-p flycheck-mode))
   :help-echo "Flycheck errors and warnings"))

;;; Segment Collections

(defun iota-segments-minimal ()
  "Return minimal segment set."
  (list (iota-segment-buffer-name)
        (iota-segment-major-mode)
        (iota-segment-position)))

(defun iota-segments-standard ()
  "Return standard segment set."
  (list (iota-segment-buffer-name)
        (iota-segment-vcs-branch)
        (iota-segment-vcs-status)
        (iota-segment-major-mode)
        (iota-segment-position)
        (iota-segment-position-percent)))

(defun iota-segments-full ()
  "Return full segment set."
  (list (iota-segment-buffer-name)
        (iota-segment-vcs-branch)
        (iota-segment-vcs-status)
        (iota-segment-buffer-size)
        (iota-segment-major-mode)
        (iota-segment-region-info)
        (iota-segment-position)
        (iota-segment-position-percent)
        (iota-segment-time)))

(provide 'iota-segments)
;;; iota-segments.el ends here
