;;; iota-perf.el --- Performance monitoring -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: performance
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Performance monitoring and benchmarking for IOTA.

;;; Code:

(require 'cl-lib)

;;; Configuration

(defgroup iota-perf nil
  "Performance monitoring for IOTA."
  :group 'iota
  :prefix "iota-perf-")

(defcustom iota-perf-enabled nil
  "Enable performance monitoring.
When enabled, records timing data for operations."
  :type 'boolean
  :group 'iota-perf)

(defcustom iota-perf-warning-threshold 0.01
  "Threshold in seconds to warn about slow operations."
  :type 'number
  :group 'iota-perf)

;;; State

(defvar iota-perf-timings nil
  "Performance timing data.")

(defvar iota-perf-max-entries 100
  "Maximum number of timing entries to keep.")

;;; Measurement

(defmacro iota-perf-measure (name &rest body)
  "Measure execution time of BODY, storing as NAME."
  `(if (not iota-perf-enabled)
       (progn ,@body)
     (let ((start-time (current-time)))
       (prog1 (progn ,@body)
         (let ((elapsed (float-time (time-subtract (current-time) start-time))))
           (push (cons ,name elapsed) iota-perf-timings)
           ;; Trim old entries
           (when (> (length iota-perf-timings) iota-perf-max-entries)
             (setq iota-perf-timings (cl-subseq iota-perf-timings 0 iota-perf-max-entries)))
           (when (> elapsed iota-perf-warning-threshold)
             (message "IOTA-PERF: %s took %.3fms" ,name (* elapsed 1000))))))))

;;; Reporting

(defun iota-perf-report ()
  "Display performance report."
  (interactive)
  (if (null iota-perf-timings)
      (message "No performance data collected. Enable with `iota-perf-enable'")
    (with-current-buffer (get-buffer-create "*IOTA Performance*")
      (erase-buffer)
      (insert "=== IOTA Performance Report ===\n\n")
      (insert (format "Total measurements: %d\n" (length iota-perf-timings)))
      (insert (format "Warning threshold: %.3f ms\n\n" 
                     (* iota-perf-warning-threshold 1000)))
      
      ;; Group by operation name
      (let ((grouped (make-hash-table :test 'equal)))
        (dolist (timing iota-perf-timings)
          (let ((name (car timing))
                (time (cdr timing)))
            (push time (gethash name grouped))))
        
        ;; Calculate statistics for each operation
        (insert "Operation Statistics:\n")
        (insert (format "%-30s %8s %8s %8s %8s\n" 
                       "Operation" "Count" "Avg(ms)" "Max(ms)" "Total(ms)"))
        (insert (make-string 70 ?-) "\n")
        
        (maphash
         (lambda (name times)
           (let* ((count (length times))
                  (total (* 1000 (apply #'+ times)))
                  (avg (/ total count))
                  (max (* 1000 (apply #'max times))))
             (insert (format "%-30s %8d %8.2f %8.2f %8.2f\n"
                           name count avg max total))))
         grouped))
      
      (insert "\n\nRecent measurements (newest first):\n")
      (dolist (timing (cl-subseq iota-perf-timings 0 (min 20 (length iota-perf-timings))))
        (insert (format "%-30s: %6.3f ms\n" 
                       (car timing)
                       (* (cdr timing) 1000))))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun iota-perf-reset ()
  "Reset performance metrics."
  (interactive)
  (setq iota-perf-timings nil)
  (message "IOTA: Performance data cleared"))

(defun iota-perf-enable ()
  "Enable performance monitoring."
  (interactive)
  (setq iota-perf-enabled t)
  (message "IOTA: Performance monitoring enabled"))

(defun iota-perf-disable ()
  "Disable performance monitoring."
  (interactive)
  (setq iota-perf-enabled nil)
  (message "IOTA: Performance monitoring disabled"))

;;; Benchmarks

(defun iota-perf-benchmark-modeline ()
  "Benchmark modeline update performance."
  (interactive)
  (let ((iterations 100)
        (timings '()))
    (dotimes (i iterations)
      (let ((start (current-time)))
        (force-mode-line-update)
        (redisplay t)
        (push (float-time (time-subtract (current-time) start)) timings)))
    (let ((avg (* 1000 (/ (apply #'+ timings) iterations)))
          (max (* 1000 (apply #'max timings)))
          (min (* 1000 (apply #'min timings))))
      (message "Modeline update (%d iterations): avg=%.2fms min=%.2fms max=%.2fms" 
               iterations avg min max))))

(defun iota-perf-benchmark-box ()
  "Benchmark box rendering performance."
  (interactive)
  (let ((result (benchmark-run 1000
                  (iota-box-render :content "Test content"
                                  :style 'rounded
                                  :width 40))))
    (message "Box rendering (1000 iterations): %.3fs total, %.3fms avg"
             (car result)
             (* (car result) 1))))

(provide 'iota-perf)
;;; iota-perf.el ends here
