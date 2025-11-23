;;; iota-animate.el --- Animation framework for I O T Λ -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julien Wintz
;; Author: Julien Wintz
;; Maintainer: Julien Wintz
;; Package: IOTA (I Ø T Δ)
;; Keywords: faces, animation
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; ι • ο • τ • α
;; Minimal Terminal Interface for Emacs
;;
;; This file is part of I O T Λ.
;;
;; Animation framework for I O T Λ (I Ø T Δ).
;; Provides smooth color transitions, pulsing effects, and visual feedback
;; using timer-based frame interpolation.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'iota-theme)

;;; Configuration

(defcustom iota-animate-enabled t
  "Enable animations in I O T Λ.
 Set to nil to disable all animations for performance."
  :type 'boolean
  :group 'iota)

(defcustom iota-animate-fps 30
  "Target frames per second for animations.
Higher values = smoother but more CPU usage."
  :type 'integer
  :group 'iota)

(defcustom iota-animate-duration 0.5
  "Default animation duration in seconds."
  :type 'float
  :group 'iota)

;;; Animation State

(defvar iota-animate--active-animations nil
  "List of currently active animations.")

(defvar iota-animate--animation-id 0
  "Counter for animation IDs.")

(cl-defstruct (iota-animation
               (:constructor iota-animation--create)
               (:copier nil))
  "Animation state structure.

Slots:
  id          Unique animation ID
  timer       Timer object
  start-time  Animation start time
  duration    Total duration in seconds
  easing      Easing function
  update-fn   Function called each frame with progress (0.0-1.0)
  finish-fn   Function called when animation completes
  cancel-fn   Function called if animation is cancelled"
  id
  timer
  start-time
  duration
  easing
  update-fn
  finish-fn
  cancel-fn)

;;; Color Interpolation

(defun iota-animate-lerp (a b time)
  "Linear interpolation between A and B at TIME (0.0-1.0)."
  (+ a (* time (- b a))))

(defun iota-animate-color-lerp (color1 color2 time)
  "Interpolate between COLOR1 and COLOR2 at TIME (0.0-1.0).
Returns a hex color string."
  (condition-case nil
      (let* ((rgb1 (color-name-to-rgb color1))
             (rgb2 (color-name-to-rgb color2))
             (r (iota-animate-lerp (nth 0 rgb1) (nth 0 rgb2) time))
             (g (iota-animate-lerp (nth 1 rgb1) (nth 1 rgb2) time))
             (b (iota-animate-lerp (nth 2 rgb1) (nth 2 rgb2) time)))
        (color-rgb-to-hex r g b 2))
    (error color2)))

(defun iota-animate-hsl-lerp (color1 color2 time)
  "Interpolate between COLOR1 and COLOR2 in HSL space at TIME.
Often produces more natural color transitions than RGB."
  (let* ((rgb1 (color-name-to-rgb color1))
         (rgb2 (color-name-to-rgb color2))
         (hsl1 (apply #'color-rgb-to-hsl rgb1))
         (hsl2 (apply #'color-rgb-to-hsl rgb2))
         (h (iota-animate-lerp (nth 0 hsl1) (nth 0 hsl2) time))
         (s (iota-animate-lerp (nth 1 hsl1) (nth 1 hsl2) time))
         (l (iota-animate-lerp (nth 2 hsl1) (nth 2 hsl2) time))
         (rgb (color-hsl-to-rgb h s l)))
    (apply #'color-rgb-to-hex (append rgb '(2)))))

;;; Easing Functions

(defun iota-animate-ease-linear (time)
  "Linear easing (no acceleration)."
  time)

(defun iota-animate-ease-in-quad (time)
  "Quadratic ease-in (accelerate from zero)."
  (* time time))

(defun iota-animate-ease-out-quad (time)
  "Quadratic ease-out (decelerate to zero)."
  (- (* time (- time 2))))

(defun iota-animate-ease-in-out-quad (time)
  "Quadratic ease-in-out (accelerate then decelerate)."
  (if (< time 0.5)
      (* 2 time time)
    (+ (* -2 time time) (* 4 time) -1)))

(defun iota-animate-ease-in-cubic (time)
  "Cubic ease-in."
  (* time time time))

(defun iota-animate-ease-out-cubic (time)
  "Cubic ease-out."
  (let ((f (- time 1)))
    (+ (* f f f) 1)))

(defun iota-animate-ease-in-out-cubic (time)
  "Cubic ease-in-out."
  (if (< time 0.5)
      (* 4 time time time)
    (let ((f (- (* 2 time) 2)))
      (+ (* 0.5 f f f) 1))))

(defun iota-animate-ease-elastic (time)
  "Elastic easing (bouncy effect)."
  (cond
   ((<= time 0) 0)
   ((>= time 1) 1)
   (t (* (expt 2 (* -10 time))
         (sin (* (- (* time 10) 0.75) (/ (* 2 float-pi) 3)))
         0.5
         0.5))))

(defun iota-animate-ease-bounce (time)
  "Bounce easing."
  (cond
   ((< time (/ 1 2.75))
    (* 7.5625 time time))
   ((< time (/ 2 2.75))
    (let ((t2 (- time (/ 1.5 2.75))))
      (+ (* 7.5625 t2 t2) 0.75)))
   ((< time (/ 2.5 2.75))
    (let ((t2 (- time (/ 2.25 2.75))))
      (+ (* 7.5625 t2 t2) 0.9375)))
   (t
    (let ((t2 (- time (/ 2.625 2.75))))
      (+ (* 7.5625 t2 t2) 0.984375)))))

;;; Animation Engine

(defun iota-animate--tick (animation)
  "Execute one frame of ANIMATION."
  (condition-case err
      (let* ((current-time (float-time))
             (elapsed (- current-time (iota-animation-start-time animation)))
             (duration (iota-animation-duration animation))
             (raw-progress (min 1.0 (/ elapsed duration)))
             (easing-fn (iota-animation-easing animation))
             (progress (funcall easing-fn raw-progress))
             (update-fn (iota-animation-update-fn animation)))
        ;; Call update function with eased progress
        (funcall update-fn progress)
        ;; Check if animation is complete
        (when (>= raw-progress 1.0)
          (iota-animate-stop (iota-animation-id animation))
          (when-let ((finish-fn (iota-animation-finish-fn animation)))
            (funcall finish-fn))))
    (error
     ;; On error, stop the animation and log
     (message "IOTA animation error: %S" err)
     (iota-animate-stop (iota-animation-id animation)))))

(defun iota-animate-start (duration update-fn &optional easing finish-fn cancel-fn)
  "Start a new animation.

Arguments:
  DURATION   Animation duration in seconds
  UPDATE-FN  Function called each frame with progress (0.0-1.0)
  EASING     Easing function (default: ease-in-out-quad)
  FINISH-FN  Function called when animation completes
  CANCEL-FN  Function called if animation is cancelled

Returns: Animation ID (use with `iota-animate-stop')."
  (if (not iota-animate-enabled)
      ;; If animations disabled, call update-fn with 1.0 and finish
      (progn
        (funcall update-fn 1.0)
        (when finish-fn (funcall finish-fn))
        nil)
    (let* ((id (cl-incf iota-animate--animation-id))
           (easing (or easing #'iota-animate-ease-in-out-quad))
           (interval (/ 1.0 iota-animate-fps))
           (animation (iota-animation--create
                       :id id
                       :start-time (float-time)
                       :duration duration
                       :easing easing
                       :update-fn update-fn
                       :finish-fn finish-fn
                       :cancel-fn cancel-fn))
           (timer nil))
      ;; Create timer that looks up animation by ID to avoid closure issues
      (setq timer (run-with-timer 0 interval
                                  (lambda ()
                                    (let ((anim (cl-find id iota-animate--active-animations
                                                         :key #'iota-animation-id)))
                                      (when anim
                                        (iota-animate--tick anim))))))
      (setf (iota-animation-timer animation) timer)
      ;; Register animation
      (push animation iota-animate--active-animations)
      id)))

(defun iota-animate-stop (id)
  "Stop animation with ID."
  (when-let ((animation (cl-find id iota-animate--active-animations
                                  :key #'iota-animation-id)))
    ;; Cancel timer
    (when-let ((timer (iota-animation-timer animation)))
      (cancel-timer timer))
    ;; Remove from active list
    (setq iota-animate--active-animations
          (cl-remove id iota-animate--active-animations
                     :key #'iota-animation-id))
    t))

(defun iota-animate-stop-all ()
  "Stop all active animations."
  (interactive)
  (dolist (animation iota-animate--active-animations)
    (when-let ((timer (iota-animation-timer animation)))
      (cancel-timer timer))
    (when-let ((cancel-fn (iota-animation-cancel-fn animation)))
      (funcall cancel-fn)))
  (setq iota-animate--active-animations nil))

;;; High-Level Animation Functions

(cl-defun iota-animate-face (face attribute from to
                                   &key (duration iota-animate-duration)
                                   (easing #'iota-animate-ease-in-out-quad)
                                   finish-fn)
  "Animate FACE ATTRIBUTE from FROM to TO.

ATTRIBUTE can be :foreground, :background, etc.
FROM and TO should be color strings.

Example:
  (iota-animate-face \\='mode-line :background \"#000\" \"#39bae6\")"
  ;; Validate colors before starting animation
  (unless (and (stringp from) (stringp to))
    (error "iota-animate-face: FROM and TO must be color strings"))
  
  ;; Try to convert to RGB to validate - more lenient than color-defined-p
  (unless (condition-case nil (color-name-to-rgb from) (error nil))
    (error "iota-animate-face: Invalid FROM color: %s" from))
  (unless (condition-case nil (color-name-to-rgb to) (error nil))
    (error "iota-animate-face: Invalid TO color: %s" to))
  
  (let ((original-value (face-attribute face attribute nil t)))
    (iota-animate-start
     duration
     (lambda (progress)
       (let ((color (iota-animate-color-lerp from to progress)))
         (set-face-attribute face nil attribute color)))
     easing
     (lambda ()
       (set-face-attribute face nil attribute to)
       (when finish-fn (funcall finish-fn)))
     (lambda ()
       ;; On cancel, restore original
       (set-face-attribute face nil attribute original-value)))))

(cl-defun iota-animate-pulse (face &key
                                    (duration 0.5)
                                    (intensity 0.3)
                                    (attribute :background))
  "Pulse FACE by animating ATTRIBUTE.
INTENSITY controls how much brighter (0.0-1.0).

Example:
  (iota-animate-pulse \\='mode-line-highlight :intensity 0.3)"
  (let ((original (face-attribute face attribute nil t)))
    ;; Only animate if color is valid (not unspecified or nil)
    (if (and original
             (not (eq original 'unspecified))
             (stringp original)
             (condition-case nil (color-name-to-rgb original) (error nil)))
        (let ((bright (iota-theme-color-lighten original intensity)))
          (iota-animate-face face attribute original bright
                             :duration (/ duration 2)
                             :easing #'iota-animate-ease-in-out-quad
                             :finish-fn
                             (lambda ()
                               (iota-animate-face face attribute bright original
                                                  :duration (/ duration 2)))))
      nil)))

(cl-defun iota-animate-fade-in (face &key
                                     (duration 0.3)
                                     (from nil))
  "Fade in FACE from transparent/dim to current colors.
FROM can specify starting color (default: background)."
  (let* ((fg (face-attribute face :foreground nil t))
         (bg (face-attribute face :background nil t))
         (from-color (or from bg "#000000")))
    (iota-animate-face face :foreground from-color fg
                       :duration duration
                       :easing #'iota-animate-ease-out-quad)))

(cl-defun iota-animate-fade-out (face &key
                                      (duration 0.3)
                                      (to nil))
  "Fade out FACE to transparent/dim colors.
TO can specify target color (default: background)."
  (let* ((fg (face-attribute face :foreground nil t))
         (bg (face-attribute face :background nil t))
         (to-color (or to bg "#000000")))
    (iota-animate-face face :foreground fg to-color
                       :duration duration
                       :easing #'iota-animate-ease-in-quad)))

;;; Numeric Value Animation

(cl-defun iota-animate-value (from to update-fn
                                    &key (duration iota-animate-duration)
                                    (easing #'iota-animate-ease-in-out-quad)
                                    finish-fn)
  "Animate numeric value from FROM to TO.
UPDATE-FN is called each frame with current value.

Example:
  (iota-animate-value 0 100
    (lambda (val) (message \"Progress: %.0f%%\" val))
    :duration 2.0)"
  (iota-animate-start
   duration
   (lambda (progress)
     (let ((value (iota-animate-lerp from to progress)))
       (funcall update-fn value)))
   easing
   (lambda ()
     (funcall update-fn to)
     (when finish-fn (funcall finish-fn)))))

;;; Utility Functions

(defun iota-animate-active-p (id)
  "Return t if animation with ID is active."
  (and (cl-find id iota-animate--active-animations
                :key #'iota-animation-id)
       t))

(defun iota-animate-count ()
  "Return number of active animations."
  (length iota-animate--active-animations))

(provide 'iota-animate)
;;; iota-animate.el ends here
