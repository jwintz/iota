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
(require 'iota-element-theme)
(require 'iota-theme-transparent)
(require 'iota-box)
(require 'iota-widgets)

;;; Theme System Tests

(ert-deftest iota-test-state-detection ()
  "Test state detection logic."
  (with-temp-buffer
    ;; State detection checks window, so in temp buffer it may be 'inactive
    (should (memq (iota-element-detect-state) '(normal inactive)))
    (set-buffer-modified-p t)
    (should (memq (iota-element-detect-state) '(modified inactive)))
    (read-only-mode 1)
    (should (memq (iota-element-detect-state) '(read-only inactive)))))

(ert-deftest iota-test-transparent-should-apply ()
  "Test transparency application logic."
  (let ((iota-theme-transparent-in-terminal t)
        (iota-modeline-mode t))
    ;; Mock display-graphic-p to return nil (terminal)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil)))
      (should (iota-theme-transparent-should-apply-p)))))

(ert-deftest iota-test-element-color-states ()
  "Test element color state definitions."
  (should (alist-get 'normal iota-element-color-states))
  (should (alist-get 'modified iota-element-color-states))
  (should (alist-get 'error iota-element-color-states))
  (should (stringp (alist-get 'normal iota-element-color-states))))

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
  (should (featurep 'iota-element-theme))
  (should (featurep 'iota-theme-transparent))
  (should (featurep 'iota-widgets))
  (should (featurep 'iota-config))
  (should (featurep 'iota-perf)))

(ert-deftest iota-test-version ()
  "Test version information."
  (should (boundp 'iota-version))
  (should (stringp iota-version)))

;;; Test Runner

;;;###autoload
(defun iota-run-tests ()
  "Run all IOTA tests."
  (interactive)
  (ert-run-tests-interactively "^iota-test-"))

(provide 'iota-tests)
;;; iota-tests.el ends here
