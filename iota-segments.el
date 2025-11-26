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
(require 'iota-cache)
(require 'iota-update)
(require 'iota-utils)

(defvar battery-status-function)
(defvar flycheck-current-errors)
(declare-function flycheck-count-errors "flycheck")
(declare-function vc-git-mode-line-string "vc-git")
(declare-function vc-git-working-revision "vc-git")

;;; Buffer Information Segments

(defun iota-segment-buffer-name ()
  "Create buffer name segment."
  (iota-segment-create
   :id 'buffer-name
   :text (lambda () (buffer-name))
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

;; VCS Status Caching
(defvar-local iota-segment--vcs-status-cache nil
  "Cached VCS status for this buffer.")

(defvar-local iota-segment--vcs-cache-time 0
  "Time when VCS cache was last updated.")

(defcustom iota-segment-vcs-cache-duration 10
  "How long to cache VCS status in seconds.
Longer values improve performance but may show stale data.
Recommended: 5-30 seconds."
  :type 'number
  :group 'iota)

(defun iota-segment--invalidate-vcs-cache ()
  "Invalidate VCS status cache for current buffer.
Uses unified cache system."
  ;; Clear unified cache VCS entries for this directory
  (when-let ((dir (vc-git-root (or (buffer-file-name) default-directory))))
    (iota-cache-invalidate (format "^vcs:git-status:%s" (regexp-quote dir))))
  ;; Also clear legacy cache
  (setq iota-segment--vcs-status-cache nil
        iota-segment--vcs-cache-time 0))

(defun iota-segment--git-status-indicators ()
  "Get Git status indicators (*, +, ?, ⇡, ⇣, etc.) with caching."
  (let ((current-time (float-time)))
    ;; Return cached value if still fresh
    (if (and iota-segment--vcs-status-cache
             (< (- current-time iota-segment--vcs-cache-time)
                iota-segment-vcs-cache-duration))
        iota-segment--vcs-status-cache
      ;; Otherwise compute and cache
      (when-let ((default-directory (vc-git-root (or (buffer-file-name) default-directory))))
        (let ((status "")
              (upstream nil)
              (ahead 0)
              (behind 0))
          ;; Get upstream tracking info
          (with-temp-buffer
            (when (zerop (call-process "git" nil t nil "rev-parse" "--abbrev-ref" "@{upstream}"))
              (setq upstream (string-trim (buffer-string)))))

          ;; Get ahead/behind counts if we have upstream
          (when upstream
            (with-temp-buffer
              (when (zerop (call-process "git" nil t nil "rev-list" "--left-right" "--count"
                                         (concat "HEAD..." upstream)))
                (goto-char (point-min))
                (when (looking-at "\\([0-9]+\\)\t\\([0-9]+\\)")
                  (setq ahead (string-to-number (match-string 1)))
              (setq behind (string-to-number (match-string 2)))))))

      ;; Check for unstaged, staged, and untracked files
      (with-temp-buffer
        (call-process "git" nil t nil "status" "--porcelain")
        (goto-char (point-min))
        (let ((has-unstaged nil)
              (has-staged nil)
              (has-untracked nil))
          (while (not (eobp))
            (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
              (when (string-match "^\\(.\\)\\(.\\)" line)
                (let ((x (match-string 1 line))
                      (y (match-string 2 line)))
                  ;; Staged changes (index)
                  (when (string-match "[MADRC]" x)
                    (setq has-staged t))
                  ;; Unstaged changes (working tree)
                  (when (string-match "[MD]" y)
                    (setq has-unstaged t))
                  ;; Untracked files
                  (when (string= "??" (concat x y))
                    (setq has-untracked t)))))
            (forward-line))

          ;; Build status string
          (when has-staged (setq status (concat status "+")))
          (when has-unstaged (setq status (concat status "*")))
          (when has-untracked (setq status (concat status "?")))))

          ;; Add ahead/behind indicators
          (cond
           ((and (> ahead 0) (> behind 0))
            (setq status (concat status "⇕")))
           ((> ahead 0)
            (setq status (concat status "⇡" (if (> ahead 1) (number-to-string ahead) ""))))
           ((> behind 0)
            (setq status (concat status "⇣" (if (> behind 1) (number-to-string behind) "")))))

          ;; Cache the result
          (setq iota-segment--vcs-status-cache status
                iota-segment--vcs-cache-time current-time)
          status)))))

(defun iota-segment-vcs ()
  "Create comprehensive VCS segment with branch and status.
Format: ⎇ main*+? where:
  ⎇ = branch symbol
  * = unstaged changes
  + = staged changes
  ? = untracked files
  ⇡N = ahead of remote by N commits
  ⇣N = behind remote by N commits
  ⇕ = diverged from remote"
  (iota-segment-create
   :id 'vcs
   :text (lambda ()
           (when-let ((file (buffer-file-name)))
             (when (vc-git-registered file)
               (let* ((branch-raw (vc-git-mode-line-string file))
                      (branch (when branch-raw
                               (replace-regexp-in-string "Git[-:]" "" branch-raw)))
                      (status (iota-segment--git-status-indicators)))
                 (concat "⎇ " branch status)))))
   :face #'iota-theme-get-accent-face
   :align 'left
   :priority 50
   :update-on 'timer
   :visible-fn (lambda () (and vc-mode (buffer-file-name)))
   :help-echo (lambda ()
                (concat "VCS: " (or (ignore-errors
                                     (vc-git-mode-line-string (buffer-file-name)))
                                   "")
                       "\n* = unstaged, + = staged, ? = untracked"
                       "\n⇡ = ahead, ⇣ = behind, ⇕ = diverged"))))

;; Legacy segments kept for compatibility
(defun iota-segment-vcs-branch ()
  "Create VCS branch segment (legacy - use iota-segment-vcs instead)."
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
  "Create VCS status segment (legacy - use iota-segment-vcs instead)."
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
        (iota-segment-vcs)
        (iota-segment-major-mode)
        (iota-segment-position)
        (iota-segment-position-percent)))

(defun iota-segments-full ()
  "Return full segment set."
  (list (iota-segment-buffer-name)
        (iota-segment-vcs)
        (iota-segment-buffer-size)
        (iota-segment-major-mode)
        (iota-segment-region-info)
        (iota-segment-position)
        (iota-segment-position-percent)
        (iota-segment-time)))

;;; VCS Cache Invalidation Hooks

(defun iota-segment--setup-vcs-hooks ()
  "Set up hooks to invalidate VCS cache at appropriate times."
  ;; Invalidate on save - status changed
  (add-hook 'after-save-hook #'iota-segment--invalidate-vcs-cache nil t)
  ;; Invalidate on VC operations
  (add-hook 'vc-checkin-hook #'iota-segment--invalidate-vcs-cache nil t)
  (add-hook 'vc-after-revert-hook #'iota-segment--invalidate-vcs-cache nil t))

;; Set up hooks when visiting files
(add-hook 'find-file-hook #'iota-segment--setup-vcs-hooks)

(provide 'iota-segments)
;;; iota-segments.el ends here
