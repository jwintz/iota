;;; iota-copilot.el --- GitHub Copilot Usage Tracking -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I O T A)
;; Keywords: tools, github, copilot
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (transient "0.4.0"))

;;; Commentary:

;; GitHub Copilot Usage Tracking for IOTA
;;
;; This module provides a transient interface for checking GitHub Copilot
;; premium request usage for Pro+ subscribers.
;;
;; Requires: gh CLI (https://cli.github.com) authenticated via `gh auth login`
;;
;; Usage:
;;   M-x iota-copilot-transient
;;   M-x iota-copilot-fetch-usage

;;; Code:

(require 'json)
(require 'transient)
(require 'cl-lib)

;; Forward declarations
(declare-function iota-dispatch--separator "iota-dispatch")
(declare-function iota-dispatch--format-value "iota-dispatch")
(declare-function nerd-icons-mdicon "nerd-icons")

;;; Customization

(defgroup iota-copilot nil
  "GitHub Copilot integration for IOTA."
  :group 'iota
  :prefix "iota-copilot-")

(defcustom iota-copilot-username nil
  "GitHub username for API requests.
If nil, will detect from `gh api user`."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Username"))
  :group 'iota-copilot)

(defcustom iota-copilot-monthly-allowance 1500
  "Monthly premium request allowance for your plan.
Pro+ includes 1500 premium requests per month."
  :type 'integer
  :group 'iota-copilot)

(defcustom iota-copilot-progress-bar-width 20
  "Width of the progress bar in characters."
  :type 'integer
  :group 'iota-copilot)

(defcustom iota-copilot-cache-file
  (locate-user-emacs-file "iota-cache/copilot-usage.el")
  "File to persist usage cache across Emacs restarts."
  :type 'file
  :group 'iota-copilot)

;;; Internal State

(defvar iota-copilot--cache nil
  "Cached usage data from last fetch.")

(defvar iota-copilot--cache-time nil
  "Time of last cache update (float-time).")

(defconst iota-copilot--cache-ttl 300
  "Cache time-to-live in seconds (5 minutes).")

;;; Cache Persistence

(defun iota-copilot--save-cache ()
  "Save cache to file with timestamp."
  (when (and iota-copilot--cache iota-copilot--cache-time)
    (let ((dir (file-name-directory iota-copilot-cache-file)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (with-temp-file iota-copilot-cache-file
        (insert ";;; iota-copilot cache -*- lexical-binding: t -*-\n")
        (insert (format ";; Saved: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
        (prin1 `(setq iota-copilot--cache-time ,iota-copilot--cache-time
                      iota-copilot--cache ',iota-copilot--cache)
               (current-buffer))))))

(defun iota-copilot--load-cache ()
  "Load cache from file if exists and not expired."
  (when (file-exists-p iota-copilot-cache-file)
    (condition-case nil
        (load iota-copilot-cache-file nil t t)
      (error nil))))

;; Load cache on startup
(iota-copilot--load-cache)

;;; Progress Bar

(defun iota-copilot--nerd-available-p ()
  "Check if nerd-icons is available."
  (featurep 'nerd-icons))

(defun iota-copilot--days-in-month (&optional time)
  "Return the number of days in the month for TIME (default: now)."
  (let* ((decoded (decode-time (or time (current-time))))
         (month (nth 4 decoded))
         (year (nth 5 decoded)))
    (pcase month
      ((or 1 3 5 7 8 10 12) 31)
      ((or 4 6 9 11) 30)
      (2 (if (or (zerop (mod year 400))
                 (and (zerop (mod year 4))
                      (not (zerop (mod year 100)))))
             29 28)))))

(defun iota-copilot--current-day ()
  "Return current day of month."
  (string-to-number (format-time-string "%d")))

(defun iota-copilot--expected-usage-pct ()
  "Return expected usage percentage based on day of month."
  (let ((day (iota-copilot--current-day))
        (days-in-month (iota-copilot--days-in-month)))
    (* 100.0 (/ (float day) days-in-month))))

(defun iota-copilot--usage-status (current-pct expected-pct)
  "Return status symbol based on CURRENT-PCT vs EXPECTED-PCT.
Returns `error', `warning', or `success'."
  (let ((diff (- current-pct expected-pct)))
    (cond
     ((> diff 1.0) 'error)
     ((> diff -1.0) 'warning)
     (t 'success))))

(defun iota-copilot--progress-bar (pct &optional face width)
  "Render a thin progress bar at PCT percent.
FACE is applied to filled portion. WIDTH defaults to `iota-copilot-progress-bar-width'."
  (let* ((w (or width iota-copilot-progress-bar-width))
         (pct-clamped (min 100 (max 0 pct)))
         (filled-chars (round (* w (/ pct-clamped 100.0))))
         ;; Thin bar characters
         (bar-filled (if (iota-copilot--nerd-available-p) "━" "="))
         (bar-empty (if (iota-copilot--nerd-available-p) "─" "-"))
         (filled-str (make-string filled-chars (string-to-char bar-filled)))
         (empty-str (make-string (- w filled-chars) (string-to-char bar-empty))))
    (concat (if face (propertize filled-str 'face face) filled-str)
            (propertize empty-str 'face 'shadow))))

(defun iota-copilot--format-bar-line (label pct requests allowance face)
  "Format a progress bar line with LABEL, PCT, REQUESTS/ALLOWANCE and FACE."
  (let ((bar (iota-copilot--progress-bar pct face)))
    (format "%s %s %5.1f%% (%.0f/%d)"
            (propertize (format "%-10s" label) 'face 'transient-heading)
            bar
            pct
            requests
            allowance)))

;;; gh CLI Integration

(defun iota-copilot--gh-available-p ()
  "Check if gh CLI is available and authenticated."
  (zerop (call-process "gh" nil nil nil "auth" "status")))

(defun iota-copilot--gh-api-sync (endpoint)
  "Call GitHub API ENDPOINT via gh CLI synchronously.
Returns parsed JSON or nil on error."
  (with-temp-buffer
    (let ((exit-code (call-process "gh" nil t nil "api" endpoint)))
      (if (zerop exit-code)
          (progn
            (goto-char (point-min))
            (condition-case nil
                (json-parse-buffer :object-type 'plist :array-type 'list)
              (error nil)))
        nil))))

;;; Username Detection

(defun iota-copilot--get-username ()
  "Get GitHub username from config or gh CLI."
  (or iota-copilot-username
      (let ((user-data (iota-copilot--gh-api-sync "/user")))
        (when user-data
          (plist-get user-data :login)))))

;;; Usage Data

(defun iota-copilot--parse-month (date-string)
  "Extract year-month from DATE-STRING like 2025-12-01T08:59:46Z."
  (when (and date-string (string-match "^\\([0-9]\\{4\\}-[0-9]\\{2\\}\\)" date-string))
    (match-string 1 date-string)))

(defun iota-copilot--current-month ()
  "Return current year-month as YYYY-MM."
  (format-time-string "%Y-%m"))

(defun iota-copilot--get-month-usage ()
  "Return (requests . cost) for current month from cache."
  (when iota-copilot--cache
    (let ((usage-items (plist-get iota-copilot--cache :usageItems))
          (current-month (iota-copilot--current-month))
          (month-requests 0)
          (month-cost 0.0))
      (dolist (item usage-items)
        (when-let ((prod (plist-get item :product)))
          (when (and (string-match-p "copilot" (downcase prod))
                     (string-prefix-p current-month (or (plist-get item :date) "")))
            (setq month-requests (+ month-requests (or (plist-get item :quantity) 0)))
            (setq month-cost (+ month-cost (or (plist-get item :grossAmount) 0))))))
      (cons month-requests month-cost))))

(defun iota-copilot-fetch-usage (&optional force)
  "Fetch GitHub Copilot usage data.
If FORCE is non-nil, bypass cache."
  (interactive "P")
  (unless (iota-copilot--gh-available-p)
    (user-error "gh CLI not authenticated. Run: gh auth login"))

  ;; Check cache (in-memory)
  (when (and (not force)
             iota-copilot--cache
             iota-copilot--cache-time
             (< (- (float-time) iota-copilot--cache-time) iota-copilot--cache-ttl))
    (when (called-interactively-p 'any)
      (iota-copilot--display-usage iota-copilot--cache))
    (cl-return-from iota-copilot-fetch-usage iota-copilot--cache))

  (let ((username (iota-copilot--get-username)))
    (unless username
      (user-error "Could not detect GitHub username"))
    (message "Fetching Copilot usage for %s..." username)

    (let ((data (iota-copilot--gh-api-sync
                 (format "/users/%s/settings/billing/usage" username))))
      (if data
          (progn
            (setq iota-copilot--cache data)
            (setq iota-copilot--cache-time (float-time))
            (iota-copilot--save-cache)
            (when (called-interactively-p 'any)
              (iota-copilot--display-usage data))
            (message "Copilot usage data fetched")
            data)
        (message "Failed to fetch Copilot usage")
        nil))))

;;; Display Buffer

(defun iota-copilot--display-usage (data)
  "Display usage DATA in a dedicated buffer."
  (let ((buf (get-buffer-create "*Copilot Usage*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (iota-copilot--insert-usage-report data)
        (goto-char (point-min)))
      (special-mode)
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (iota-copilot-fetch-usage t))))
    (display-buffer buf)))

(defun iota-copilot--insert-usage-report (data)
  "Insert formatted usage report from DATA into current buffer."
  (let ((usage-items (plist-get data :usageItems))
        (current-month (iota-copilot--current-month))
        (monthly-data (make-hash-table :test 'equal)))

    (insert (propertize "Copilot Premium Requests\n" 'face '(:weight bold :height 1.3)))
    (insert (make-string 50 ?─) "\n\n")

    (if (not usage-items)
        (insert (propertize "No Copilot usage data found.\n" 'face 'shadow))

      ;; Group by month
      (dolist (item usage-items)
        (let* ((product (plist-get item :product))
               (date (plist-get item :date))
               (month (iota-copilot--parse-month date))
               (quantity (or (plist-get item :quantity) 0))
               (gross-amount (or (plist-get item :grossAmount) 0)))
          (when (and product (string-match-p "copilot" (downcase product)) month)
            (let ((existing (gethash month monthly-data '(0 . 0.0))))
              (puthash month
                       (cons (+ (car existing) quantity)
                             (+ (cdr existing) gross-amount))
                       monthly-data)))))

      ;; Current month
      (let* ((current-data (gethash current-month monthly-data))
             (requests (if current-data (car current-data) 0))
             (current-pct (* 100.0 (/ requests (float iota-copilot-monthly-allowance))))
             (expected-pct (iota-copilot--expected-usage-pct))
             (expected-requests (* iota-copilot-monthly-allowance (/ expected-pct 100.0)))
             (status (iota-copilot--usage-status current-pct expected-pct))
             (usage-face (pcase status ('error 'error) ('warning 'warning) ('success 'success))))

        (insert (propertize "This Month\n" 'face '(:weight bold)))
        (insert (iota-copilot--format-bar-line "Usage" current-pct requests
                                                iota-copilot-monthly-allowance usage-face) "\n")
        (insert (iota-copilot--format-bar-line "Expected" expected-pct expected-requests
                                                iota-copilot-monthly-allowance 'shadow) "\n")
        (when current-data
          (insert (format "\nValue: $%.2f" (cdr current-data)))
          (when (= (cdr current-data) 0.0)
            (insert (propertize " (included in plan)" 'face 'shadow)))
          (insert "\n")))

      ;; Monthly history
      (insert "\n" (make-string 50 ?─) "\n")
      (insert (propertize "Monthly History\n" 'face '(:weight bold)))
      (let ((months (sort (hash-table-keys monthly-data) #'string>)))
        (dolist (month months)
          (let ((mdata (gethash month monthly-data)))
            (insert (format "  %s: %.0f requests" month (car mdata)))
            (when (> (cdr mdata) 0)
              (insert (format " ($%.2f)" (cdr mdata))))
            (insert "\n"))))

      ;; Total
      (let ((total-requests 0) (total-cost 0.0))
        (maphash (lambda (_k v)
                   (setq total-requests (+ total-requests (car v)))
                   (setq total-cost (+ total-cost (cdr v))))
                 monthly-data)
        (insert "\n" (make-string 50 ?─) "\n")
        (insert (propertize "All Time\n" 'face '(:weight bold)))
        (insert (format "  Total: %.0f requests ($%.2f)\n" total-requests total-cost))))

    ;; Footer
    (insert "\n" (make-string 50 ?─) "\n")
    (insert (propertize (format "Updated: %s\n"
                                (if iota-copilot--cache-time
                                    (format-time-string "%Y-%m-%d %H:%M:%S"
                                                        (seconds-to-time iota-copilot--cache-time))
                                  "never"))
                        'face 'shadow))
    (insert (propertize "Press 'g' to refresh\n" 'face 'shadow))))

;;; Transient Helpers

(defun iota-copilot--separator ()
  "Return a horizontal separator line."
  (if (fboundp 'iota-dispatch--separator)
      (iota-dispatch--separator)
    (propertize (make-string 40 ?─) 'face 'shadow)))

(defun iota-copilot--format-value (label value)
  "Format LABEL: VALUE for display."
  (if (fboundp 'iota-dispatch--format-value)
      (iota-dispatch--format-value label value)
    (format "%s %s"
            (propertize (format "%s:" label) 'face 'transient-heading)
            (propertize (format "%s" value) 'face 'transient-value))))

(defun iota-copilot--cache-age ()
  "Return human-readable cache age."
  (if iota-copilot--cache-time
      (let ((age (- (float-time) iota-copilot--cache-time)))
        (cond
         ((< age 60) (format "%ds ago" (truncate age)))
         ((< age 3600) (format "%dm ago" (truncate (/ age 60))))
         ((< age 86400) (format "%dh ago" (truncate (/ age 3600))))
         (t (format "%dd ago" (truncate (/ age 86400))))))
    "never"))

(defun iota-copilot--transient-bars ()
  "Return the two progress bar lines for transient display."
  (if-let ((usage (iota-copilot--get-month-usage)))
      (let* ((requests (car usage))
             (current-pct (* 100.0 (/ requests (float iota-copilot-monthly-allowance))))
             (expected-pct (iota-copilot--expected-usage-pct))
             (expected-requests (* iota-copilot-monthly-allowance (/ expected-pct 100.0)))
             (status (iota-copilot--usage-status current-pct expected-pct))
             (usage-face (pcase status ('error 'error) ('warning 'warning) ('success 'success))))
        (concat
         (iota-copilot--format-bar-line "Usage" current-pct requests
                                         iota-copilot-monthly-allowance usage-face) "\n"
         (iota-copilot--format-bar-line "Expected" expected-pct expected-requests
                                         iota-copilot-monthly-allowance 'shadow)))
    "Not fetched — press 'f' to fetch"))

;;; Transient Suffixes

(transient-define-suffix iota-copilot--fetch-suffix ()
  "Fetch usage data."
  :key "f"
  :description "Fetch"
  :transient t
  (interactive)
  (iota-copilot-fetch-usage nil))

(transient-define-suffix iota-copilot--refresh-suffix ()
  "Force refresh (bypass cache)."
  :key "r"
  :description "Refresh"
  :transient t
  (interactive)
  (iota-copilot-fetch-usage t))

(transient-define-suffix iota-copilot--details-suffix ()
  "Show detailed buffer."
  :key "s"
  :description "Details"
  :transient nil
  (interactive)
  (unless iota-copilot--cache
    (iota-copilot-fetch-usage nil))
  (when iota-copilot--cache
    (iota-copilot--display-usage iota-copilot--cache)))

(transient-define-suffix iota-copilot--web-suffix ()
  "Open GitHub billing."
  :key "w"
  :description "Web"
  :transient nil
  (interactive)
  (browse-url "https://github.com/settings/billing"))

;;;###autoload (autoload 'iota-copilot-transient "iota-copilot" nil t)
(transient-define-prefix iota-copilot-transient ()
  "GitHub Copilot Usage Tracking."
  [:description
   (lambda ()
     (concat
      (propertize "I O T Λ Copilot" 'face '(:weight bold))
      "\n" (iota-copilot--separator) "\n"
      (iota-copilot--transient-bars) "\n"
      (iota-copilot--format-value "Cache" (iota-copilot--cache-age)) "\n"
      (iota-copilot--separator) "\n"))

   ["Actions"
    (iota-copilot--fetch-suffix)
    (iota-copilot--refresh-suffix)
    (iota-copilot--details-suffix)
    (iota-copilot--web-suffix)]])

(provide 'iota-copilot)
;;; iota-copilot.el ends here
