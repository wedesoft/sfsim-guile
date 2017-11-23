#!/usr/bin/env guile
!#
(use-modules (system foreign)
             (rnrs bytevectors)
             (srfi srfi-1)
             (srfi srfi-26)
             (glut) (gl)
             (ssim quaternion))


(define glut (dynamic-link "libglut"))
(define glut-wire-cube (pointer->procedure void (dynamic-func "glutWireCube" glut) (list double)))

(define (sqr x) (* x x))

(define main-window #f)
(define q (quaternion-rotation 0 '(1 0 0)))
(define qs (quaternion-rotation 0.01 '(0 0.6 0.8)))

(define m 1)
(define w 1)
(define h 0.25)
(define d 0.5)
(define Iw (* (/ m 12) (+ (sqr h) (sqr d)))); -> cuboid
(define Ih (* (/ m 12) (+ (sqr w) (sqr d))))
(define Id (* (/ m 12) (+ (sqr w) (sqr h))))
(define I (list Iw Ih Id))

(define (on-reshape width height)
  (let [(aspect (/ width height))]
    (pk 'reshape! width height)
    (gl-viewport 0 0 width height)
    (set-gl-matrix-mode (matrix-mode projection))
    (gl-load-identity)
    (gl-ortho (- aspect) aspect -1 1 -1 +1)))

(define (on-display)
  (pk 'on-display)
  (let* [(b (make-bytevector (* 4 4 4)))
         (mat (rotation-matrix q))
         (hom (concatenate (append (map (cut append <> '(0)) mat) '((0 0 0 1)))))]
    (for-each (lambda (i) (bytevector-ieee-single-native-set! b (* i 4) (list-ref hom i))) (iota (length hom)))
    (gl-clear (clear-buffer-mask color-buffer))
    (gl-color 0 1 0)
    (set-gl-matrix-mode (matrix-mode modelview))
    (gl-load-matrix b)
    (gl-scale w h d)
    (glut-wire-cube 0.5)
    (swap-buffers)))

(define (on-idle)
  (set! q (* q qs))
  (post-redisplay))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "ssim"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback on-idle)
(glut-main-loop)
