#!/usr/bin/env guile
!#
(use-modules (glut) (gl) (gl low-level) (glu) (srfi srfi-1) (srfi srfi-26) (sfsim linear-algebra))

(define p '(220 140))
(define o 0)
(define v '(0 0))
(define w 0.0)
(define l 100)
(define m 1)

(define dt 0.08)

(define main-window #f)

(define (euler)
  (let* [(f '(0 3))
         (a (* f (/ 1 m)))]
    (set! p (+ p (* v dt)))
    (set! o (+ o (* w dt)))
    (set! v (+ v (* a dt)))))

(define (on-idle)
  (euler)
  (post-redisplay))

(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (gl-begin (begin-mode lines)
    (gl-color 1 1 1)
    (gl-vertex (- (car p) (* (cos o) 0.5 l)) (- (cadr p) (* (sin o) 0.5 l)))
    (gl-vertex (+ (car p) (* (cos o) 0.5 l)) (+ (cadr p) (* (sin o) 0.5 l))))
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
