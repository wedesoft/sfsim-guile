#!/usr/bin/env guile
!#
(use-modules (glut) (gl) (gl low-level) (glu) (srfi srfi-1) (srfi srfi-26))

(define cx 200)
(define cy 240)
(define ox 320)
(define oy 240)
(define vx 0)
(define l 250)
(define m 20)

(define dt 0.08)

(define main-window #f)

(define (euler)
  (let* [(f  (- 320 cx))
         (ax (/ f m))]
    (set! vx (+ vx (* ax dt)))
    (set! cx (+ cx (* vx dt)))))

(define (on-idle)
  (euler)
  (post-redisplay))

(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (glPointSize 5)
  (gl-begin (begin-mode points)
    (gl-color 1 0 0)
    (gl-vertex cx cy)
    (gl-color 0 1 0)
    (gl-vertex ox oy))
  (swap-buffers))

(define (on-reshape width height)
  (pk 'reshape! width height)
  (gl-viewport 0 0 width height)
  (set-gl-matrix-mode (matrix-mode projection))
  (gl-load-identity)
  (gl-ortho 0 width height 0 -1 +1))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "spring"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback     on-idle)
(set-gl-clear-color 0 0 0 1)
(set-gl-matrix-mode (matrix-mode modelview))
(gl-load-identity)
(glut-main-loop)
