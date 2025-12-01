;;; iota-tests.el --- Test suite for IOTA -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: tests
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Test suite for IOTA functionality.

;;; Code:

(require 'ert)
(require 'iota)
(require 'iota-theme-transparent)
(require 'iota-box)
(require 'iota-widgets)
(require 'iota-update)
(require 'iota-timers)
(require 'iota-cache)
(require 'iota-utils)

;;; Theme System Tests

(ert-deftest iota-test-transparent-should-apply ()
  "Test transparency application logic."
  (let ((iota-theme-transparent-in-terminal t)
        (iota-modeline-mode t))
    ;; Mock display-graphic-p to return nil (terminal)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil)))
      (should (iota-theme-transparent-should-apply-p)))))

;;; Box Rendering Tests

(ert-deftest iota-test-box-style-selection ()
  "Test box style fallback chain."
  (should (memq (iota-box-select-best-style) 
               '(heavy-rounded rounded heavy single ascii))))

(ert-deftest iota-test-box-rendering ()
  "Test basic box rendering."
  (let ((box (iota-box-render :content "Test" 
                             :style 'ascii
                             :width 20)))
    (should (stringp box))
    (should (string-match-p "Test" box))))

(ert-deftest iota-test-box-style-availability ()
  "Test box style availability detection."
  ;; ASCII should always be available
  (should (iota-box-style-available-p 'ascii))
  ;; Test should return boolean
  (should (booleanp (iota-box-style-available-p 'rounded))))

(ert-deftest iota-test-box-chars ()
  "Test box character set retrieval."
  (dolist (style '(single double rounded heavy heavy-rounded 
                         modern-thin modern-thick ascii))
    (let ((chars (iota-box-get-chars style)))
      (should (plist-get chars :top-left))
      (should (plist-get chars :horizontal))
      (should (plist-get chars :vertical)))))

;;; Widget Tests

(ert-deftest iota-test-progress-bar ()
  "Test progress bar rendering."
  (let ((bar (iota-widget-progress-bar 50 100 :width 20)))
    (should (stringp bar))
    (should (> (length bar) 0))))

(ert-deftest iota-test-progress-bar-modern ()
  "Test modern progress bar rendering."
  (when (fboundp 'iota-widget-progress-bar-modern)
    (let ((bar (iota-widget-progress-bar-modern 75 100 
                 :width 20 :show-percent t)))
      (should (stringp bar))
      (should (string-match-p "[0-9]+\\.[0-9]+%" bar)))))

(ert-deftest iota-test-status-indicator ()
  "Test status indicator rendering."
  (when (fboundp 'iota-widget-status-indicator)
    (dolist (status '(success warning error info pending))
      (should (stringp (iota-widget-status-indicator status))))))

(ert-deftest iota-test-badge ()
  "Test badge widget."
  (let ((badge (iota-widget-badge "TEST" 'success)))
    (should (stringp badge))
    (should (string-match-p "TEST" badge))))

(ert-deftest iota-test-badge-modern ()
  "Test modern badge widget."
  (when (fboundp 'iota-widget-badge-modern)
    (let ((badge (iota-widget-badge-modern "TEST" :color "#39bae6")))
      (should (stringp badge))
      (should (string-match-p "TEST" badge)))))

(ert-deftest iota-test-spinner ()
  "Test spinner animation frames."
  (when (fboundp 'iota-widget-spinner)
    (dolist (style '(braille dots line arc box))
      (dotimes (i 8)
        (should (stringp (iota-widget-spinner i style)))))))

(ert-deftest iota-test-bullet ()
  "Test bullet glyph selection."
  (when (fboundp 'iota-widget-bullet)
    (should (stringp (iota-widget-bullet)))))

;;; Configuration Tests

(ert-deftest iota-test-config-presets ()
  "Test configuration preset definitions."
  (when (fboundp 'iota-config-apply-preset)
    ;; Test that presets apply without errors
    (dolist (preset '(minimal standard modern cyberpunk))
      (should (progn (iota-config-apply-preset preset) t)))))

(ert-deftest iota-test-config-preset-variable ()
  "Test configuration preset variable."
  (should (boundp 'iota-config-preset))
  (should (memq iota-config-preset 
               '(minimal standard modern cyberpunk custom))))

;;; Performance Tests

(ert-deftest iota-test-perf-measure-macro ()
  "Test performance measurement macro."
  (when (fboundp 'iota-perf-measure)
    (let ((iota-perf-enabled t)
          (result nil))
      (setq result (iota-perf-measure "test-operation"
                     (+ 1 1)))
      (should (= result 2)))))

;;; Integration Tests

(ert-deftest iota-test-module-loading ()
  "Test that all modules load correctly."
  (should (featurep 'iota))
  (should (featurep 'iota-box))
  (should (featurep 'iota-theme))
  (should (featurep 'iota-theme-transparent))
  (should (featurep 'iota-widgets))
  (should (featurep 'iota-config))
  (should (featurep 'iota-perf))
  ;; New modules
  (should (featurep 'iota-update))
  (should (featurep 'iota-timers))
  (should (featurep 'iota-cache))
  (should (featurep 'iota-utils)))

(ert-deftest iota-test-version ()
  "Test version information."
  (should (boundp 'iota-version))
  (should (stringp iota-version)))

;;; Performance Tests

(ert-deftest iota-test-modeline-render-performance ()
  "Ensure modeline renders in reasonable time."
  (skip-unless (featurep 'iota-modeline))
  (let ((start (float-time))
        (iterations 50))
    (dotimes (_ iterations)
      (iota-modeline--render))
    (let ((elapsed (- (float-time) start)))
      ;; Should complete 50 renders in under 1 second
      (should (< elapsed 1.0))
      (message "IOTA modeline render: %.3fms avg" (* 1000 (/ elapsed iterations))))))

(ert-deftest iota-test-no-timer-leaks ()
  "Ensure no timer leaks after mode toggle."
  (skip-unless (featurep 'iota-modeline))
  ;; Clean up any existing IOTA timers
  (iota-timers-cancel-all)
  (let ((initial-timer-count (iota-timers-count)))
    ;; Enable and disable modeline mode
    (iota-modeline-mode 1)
    (iota-modeline-mode -1)
    ;; Allow for some tolerance (update system may have idle timer)
    (should (<= (iota-timers-count) (+ initial-timer-count 2)))))

(ert-deftest iota-test-timer-registry-basic ()
  "Test basic timer registry operations."
  (let ((initial-count (iota-timers-count)))
    ;; Register a timer
    (iota-timers-run-with-timer 'test-timer 1 nil (lambda () nil))
    (should (= (iota-timers-count) (1+ initial-count)))
    (should (iota-timers-active-p 'test-timer))
    ;; Cancel it
    (iota-timers-cancel 'test-timer)
    (should (= (iota-timers-count) initial-count))
    (should (not (iota-timers-active-p 'test-timer)))))

(ert-deftest iota-test-timer-registry-replacement ()
  "Test that registering a timer with same key cancels the old one."
  (let ((call-count 0))
    ;; Register first timer
    (iota-timers-run-with-timer 'test-replace 0.01 nil 
                                (lambda () (cl-incf call-count)))
    ;; Immediately replace with another
    (iota-timers-run-with-timer 'test-replace 10 nil 
                                (lambda () (cl-incf call-count 100)))
    ;; Only one timer should be registered
    (should (= (length (iota-timers-list-group 'test-replace)) 1))
    ;; Clean up
    (iota-timers-cancel 'test-replace)))

;;; Cache Tests

(ert-deftest iota-test-cache-basic ()
  "Test basic cache operations."
  ;; Clear cache first
  (iota-cache-clear)
  (should (= (iota-cache-count) 0))
  ;; Set a value
  (iota-cache-set "test-key" "test-value")
  (should (= (iota-cache-count) 1))
  ;; Get it back
  (should (equal (iota-cache-get "test-key") "test-value"))
  ;; Clear
  (iota-cache-clear)
  (should (= (iota-cache-count) 0)))

(ert-deftest iota-test-cache-ttl ()
  "Test cache TTL expiration."
  (iota-cache-clear)
  ;; Set with very short TTL
  (iota-cache-set "ttl-test" "value")
  ;; Should be available immediately
  (should (equal (iota-cache-get "ttl-test" :ttl 60) "value"))
  ;; With TTL of 0, should be expired
  (should (null (iota-cache-get "ttl-test" :ttl 0)))
  (iota-cache-clear))

(ert-deftest iota-test-cache-compute-fn ()
  "Test cache compute-fn for lazy evaluation."
  (iota-cache-clear)
  (let ((compute-count 0))
    ;; First call should compute
    (iota-cache-get "compute-test" 
                    :ttl 60
                    :compute-fn (lambda () 
                                  (cl-incf compute-count)
                                  "computed-value"))
    (should (= compute-count 1))
    ;; Second call should use cache
    (iota-cache-get "compute-test" 
                    :ttl 60
                    :compute-fn (lambda () 
                                  (cl-incf compute-count)
                                  "computed-value"))
    (should (= compute-count 1)))
  (iota-cache-clear))

(ert-deftest iota-test-cache-invalidate-pattern ()
  "Test pattern-based cache invalidation."
  (iota-cache-clear)
  (iota-cache-set "vcs:repo1" "value1")
  (iota-cache-set "vcs:repo2" "value2")
  (iota-cache-set "segment:mode" "value3")
  (should (= (iota-cache-count) 3))
  ;; Invalidate vcs entries only
  (iota-cache-invalidate "^vcs:")
  (should (= (iota-cache-count) 1))
  (should (equal (iota-cache-get "segment:mode") "value3"))
  (iota-cache-clear))

;;; Update System Tests

(ert-deftest iota-test-update-request ()
  "Test update request debouncing."
  (let ((iota-update-enabled t)
        (iota-update-debounce 0.05))
    ;; Request multiple updates
    (iota-update-request :modeline)
    (iota-update-request :modeline)
    (iota-update-request :modeline)
    ;; Should have pending update
    (should iota-update--pending)
    ;; Wait for debounce
    (sleep-for 0.1)
    ;; Should have flushed
    (should (not iota-update--pending))))

(ert-deftest iota-test-update-component-registration ()
  "Test component registration with update system."
  (let ((called nil))
    ;; Register a test component
    (iota-update-register-component :test-component 
                                    (lambda () (setq called t)))
    ;; Request update and flush
    (iota-update-request :test-component t)
    (sleep-for 0.05)
    ;; Should have called our handler
    (should called)
    ;; Unregister
    (iota-update-unregister-component :test-component)))

;;; Utility Tests

(ert-deftest iota-test-safe-call ()
  "Test iota-safe-call error handling."
  (let ((iota-debug-mode nil))
    ;; Should return nil on error
    (should (null (iota-safe-call "test" (error "test error"))))
    ;; Should return value on success
    (should (equal (iota-safe-call "test" (+ 1 2)) 3))))

(ert-deftest iota-test-face-attribute-safe ()
  "Test safe face attribute accessor."
  ;; Should return fallback for unspecified
  (should (equal (iota-face-attribute-safe 'default :box "fallback") "fallback"))
  ;; Should return actual value when present
  (let ((fg (iota-face-attribute-safe 'default :foreground "fallback")))
    (should (or (stringp fg) (equal fg "fallback")))))

(ert-deftest iota-test-color-valid-p ()
  "Test color validation."
  (should (iota-color-valid-p "#ff0000"))
  (should (iota-color-valid-p "red"))
  (should (not (iota-color-valid-p nil)))
  (should (not (iota-color-valid-p "")))
  (should (not (iota-color-valid-p "not-a-color-at-all-xyz"))))

;;; Test Runner

;;;###autoload
(defun iota-run-tests ()
  "Run all IOTA tests."
  (interactive)
  (ert-run-tests-interactively "^iota-test-"))

(provide 'iota-tests)
;;; iota-tests.el ends here
