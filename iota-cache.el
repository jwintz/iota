;;; iota-cache.el --- Unified caching system for I O T Λ -*- no-byte-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: performance, caching
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Unified caching system with TTL (time-to-live) support.
;; Replaces multiple ad-hoc caches throughout the codebase.
;;
;; Key features:
;; - TTL-based expiration
;; - Lazy computation with compute-fn
;; - Pattern-based invalidation
;; - Size limits and LRU eviction (optional)
;; - Statistics and debugging

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup iota-cache nil
  "I O T Λ cache configuration."
  :group 'iota
  :prefix "iota-cache-")

(defcustom iota-cache-default-ttl 60.0
  "Default time-to-live in seconds for cached values."
  :type 'float
  :group 'iota-cache)

(defcustom iota-cache-vcs-ttl 10.0
  "Time-to-live in seconds for VCS-related cache entries.
VCS operations are expensive, so a longer TTL helps performance."
  :type 'float
  :group 'iota-cache)

(defcustom iota-cache-segment-ttl 5.0
  "Time-to-live in seconds for segment rendering cache."
  :type 'float
  :group 'iota-cache)

(defcustom iota-cache-max-size nil
  "Maximum number of cache entries.
When exceeded, oldest entries are evicted.
nil means unlimited."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Max entries"))
  :group 'iota-cache)

(defcustom iota-cache-debug nil
  "When non-nil, log cache activity for debugging."
  :type 'boolean
  :group 'iota-cache)

;;; State

(defvar iota-cache--store (make-hash-table :test 'equal)
  "Central cache store.
Maps keys to (VALUE . TIMESTAMP) pairs.")

(defvar iota-cache--access-times (make-hash-table :test 'equal)
  "Last access time for each cache entry (for LRU eviction).")

(defvar iota-cache--stats
  (list :hits 0 :misses 0 :evictions 0 :invalidations 0)
  "Cache statistics.")

;;; Core Functions

(cl-defun iota-cache-get (key &key (ttl iota-cache-default-ttl) compute-fn)
  "Get cached value for KEY.

Arguments:
  KEY        Cache key (any hashable value)
  :ttl       Time-to-live in seconds (default: `iota-cache-default-ttl')
  :compute-fn  Function called to compute value on cache miss

If the cached value exists and is not expired, return it.
If expired or missing and COMPUTE-FN is provided, compute and cache the value.
Returns nil if no value and no compute-fn."
  (let* ((now (float-time))
         (entry (gethash key iota-cache--store))
         (value (car entry))
         (timestamp (cdr entry))
         (expired (or (null entry)
                      (> (- now timestamp) ttl))))
    
    ;; Update access time for LRU
    (puthash key now iota-cache--access-times)
    
    (if (and (not expired) entry)
        ;; Cache hit
        (progn
          (cl-incf (plist-get iota-cache--stats :hits))
          (when iota-cache-debug
            (message "IOTA cache hit: %s" key))
          value)
      ;; Cache miss or expired
      (cl-incf (plist-get iota-cache--stats :misses))
      (when iota-cache-debug
        (message "IOTA cache miss: %s (expired: %s)" key (and entry expired)))
      (when compute-fn
        (let ((computed-value (funcall compute-fn)))
          (iota-cache-set key computed-value)
          computed-value)))))

(defun iota-cache-set (key value)
  "Set cached VALUE for KEY.
Returns VALUE."
  (let ((now (float-time)))
    ;; Check size limit and evict if needed
    (when (and iota-cache-max-size
               (>= (hash-table-count iota-cache--store) iota-cache-max-size))
      (iota-cache--evict-lru))
    
    (puthash key (cons value now) iota-cache--store)
    (puthash key now iota-cache--access-times)
    
    (when iota-cache-debug
      (message "IOTA cache set: %s" key))
    
    value))

(defun iota-cache-remove (key)
  "Remove KEY from cache."
  (remhash key iota-cache--store)
  (remhash key iota-cache--access-times)
  (cl-incf (plist-get iota-cache--stats :invalidations)))

(defun iota-cache-clear ()
  "Clear all cache entries."
  (interactive)
  (let ((count (hash-table-count iota-cache--store)))
    (clrhash iota-cache--store)
    (clrhash iota-cache--access-times)
    (cl-incf (plist-get iota-cache--stats :invalidations) count)
    (when iota-cache-debug
      (message "IOTA cache cleared: %d entries" count))))

;;; Pattern-based Invalidation

(defun iota-cache-invalidate (pattern)
  "Invalidate all cache entries with keys matching PATTERN.
PATTERN is a regular expression string."
  (let ((count 0))
    (maphash (lambda (key _value)
               (when (string-match-p pattern (format "%s" key))
                 (remhash key iota-cache--store)
                 (remhash key iota-cache--access-times)
                 (cl-incf count)))
             iota-cache--store)
    (cl-incf (plist-get iota-cache--stats :invalidations) count)
    (when iota-cache-debug
      (message "IOTA cache invalidate '%s': %d entries" pattern count))
    count))

(defun iota-cache-invalidate-prefix (prefix)
  "Invalidate all cache entries with keys starting with PREFIX."
  (iota-cache-invalidate (concat "^" (regexp-quote (format "%s" prefix)))))

;;; LRU Eviction

(defun iota-cache--evict-lru ()
  "Evict the least recently used cache entry."
  (let ((oldest-key nil)
        (oldest-time most-positive-fixnum))
    (maphash (lambda (key time)
               (when (< time oldest-time)
                 (setq oldest-key key
                       oldest-time time)))
             iota-cache--access-times)
    (when oldest-key
      (remhash oldest-key iota-cache--store)
      (remhash oldest-key iota-cache--access-times)
      (cl-incf (plist-get iota-cache--stats :evictions))
      (when iota-cache-debug
        (message "IOTA cache evict LRU: %s" oldest-key)))))

(defun iota-cache-evict-expired ()
  "Evict all expired entries using default TTL."
  (interactive)
  (let ((now (float-time))
        (count 0))
    (maphash (lambda (key entry)
               (when (> (- now (cdr entry)) iota-cache-default-ttl)
                 (remhash key iota-cache--store)
                 (remhash key iota-cache--access-times)
                 (cl-incf count)))
             iota-cache--store)
    (cl-incf (plist-get iota-cache--stats :evictions) count)
    (when (> count 0)
      (message "IOTA cache: evicted %d expired entries" count))))

;;; Specialized Caches

(defun iota-cache-get-vcs (key compute-fn)
  "Get VCS-related cached value for KEY.
Uses `iota-cache-vcs-ttl' for expiration.
COMPUTE-FN is called to compute value on cache miss."
  (iota-cache-get (format "vcs:%s" key)
                  :ttl iota-cache-vcs-ttl
                  :compute-fn compute-fn))

(defun iota-cache-set-vcs (key value)
  "Set VCS-related cached VALUE for KEY."
  (iota-cache-set (format "vcs:%s" key) value))

(defun iota-cache-invalidate-vcs ()
  "Invalidate all VCS-related cache entries."
  (iota-cache-invalidate-prefix "vcs:"))

(defun iota-cache-get-segment (key compute-fn)
  "Get segment cached value for KEY.
Uses `iota-cache-segment-ttl' for expiration.
COMPUTE-FN is called to compute value on cache miss."
  (iota-cache-get (format "segment:%s" key)
                  :ttl iota-cache-segment-ttl
                  :compute-fn compute-fn))

(defun iota-cache-set-segment (key value)
  "Set segment cached VALUE for KEY."
  (iota-cache-set (format "segment:%s" key) value))

(defun iota-cache-invalidate-segments ()
  "Invalidate all segment cache entries."
  (iota-cache-invalidate-prefix "segment:"))

;;; Statistics

(defun iota-cache-count ()
  "Return number of cached entries."
  (hash-table-count iota-cache--store))

(defun iota-cache-stats ()
  "Display cache statistics."
  (interactive)
  (let* ((hits (plist-get iota-cache--stats :hits))
         (misses (plist-get iota-cache--stats :misses))
         (total (+ hits misses))
         (hit-rate (if (> total 0) (* 100.0 (/ (float hits) total)) 0)))
    (message "IOTA Cache: %d entries, %d hits, %d misses (%.1f%% hit rate), %d evictions, %d invalidations"
             (hash-table-count iota-cache--store)
             hits
             misses
             hit-rate
             (plist-get iota-cache--stats :evictions)
             (plist-get iota-cache--stats :invalidations))))

(defun iota-cache-reset-stats ()
  "Reset cache statistics."
  (interactive)
  (setq iota-cache--stats
        (list :hits 0 :misses 0 :evictions 0 :invalidations 0))
  (message "IOTA cache stats reset"))

(defun iota-cache-debug-list ()
  "List all cache entries with their age."
  (interactive)
  (let ((now (float-time)))
    (with-output-to-temp-buffer "*IOTA Cache*"
      (princ "IOTA Cache Contents\n")
      (princ "===================\n\n")
      (if (zerop (hash-table-count iota-cache--store))
          (princ "Cache is empty.\n")
        (let ((entries nil))
          (maphash (lambda (key entry)
                     (push (list key (car entry) (- now (cdr entry))) entries))
                   iota-cache--store)
          ;; Sort by age
          (setq entries (sort entries (lambda (a b) (< (nth 2 a) (nth 2 b)))))
          (dolist (entry entries)
            (princ (format "%-50s %.1fs ago\n"
                           (truncate-string-to-width (format "%s" (car entry)) 50)
                           (nth 2 entry)))))))))

;;; Cleanup

(defun iota-cache-cleanup ()
  "Clean up all cache-related state."
  (iota-cache-clear)
  (iota-cache-reset-stats))

(provide 'iota-cache)
;;; iota-cache.el ends here
