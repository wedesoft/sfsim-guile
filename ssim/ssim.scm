#!/usr/bin/env guile
!#
(use-modules (system foreign)
             (rnrs bytevectors)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26)
             (glut)
             (gl)
             (gl low-level)
             (ssim physics)
             (ssim quaternion))


(define glut (dynamic-link "libglut"))
(define glut-wire-cube (pointer->procedure void (dynamic-func "glutWireCube" glut) (list double)))

(define (sqr x) (* x x))

(define time #f)
(define main-window #f)

(define q (quaternion-rotation 0.3 '(0.6 0.0 0.8)))
(define impulse '(0 0.3 0))

(define m 1)
(define w 1)
(define h 0.25)
(define d 0.5)
(define inertia (cuboid-inertia m w h d))

(define w2 (/ w 2))
(define h2 (/ h 2))
(define d2 (/ d 2))

(define corners
  (list (list (- w2) (- h2) (- d2))
        (list (+ w2) (- h2) (- d2))
        (list (- w2) (+ h2) (- d2))
        (list (+ w2) (+ h2) (- d2))
        (list (- w2) (- h2) (+ d2))
        (list (+ w2) (- h2) (+ d2))
        (list (- w2) (+ h2) (+ d2))
        (list (+ w2) (+ h2) (+ d2))))

(define (on-reshape width height)
  (let* [(aspect (/ width height))
         (h      1.0)
         (w      (* aspect h))]
    (gl-viewport 0 0 width height)
    (set-gl-matrix-mode (matrix-mode projection))
    (gl-load-identity)
    (gl-ortho (- w) w (- h) h -1 +1)))

(define (on-display)
  (let* [(b   (make-bytevector (* 4 4 4)))
         (mat (rotation-matrix q))
         (hom (concatenate (append (map (cut append <> '(0)) mat) '((0 0 0 1)))))]
    (for-each (lambda (i) (bytevector-ieee-single-native-set! b (* i 4) (list-ref hom i))) (iota (length hom)))
    (gl-clear (clear-buffer-mask color-buffer))
    (set-gl-matrix-mode (matrix-mode modelview))
    (gl-load-matrix b #:transpose #t)
    (gl-color 1 0 0)
    (glPointSize 5)
    (gl-begin (begin-mode points)
      (for-each (cut apply gl-vertex <>) corners))
    (gl-scale w h d)
    (gl-color 0 1 0)
    (glut-wire-cube 1.0)
    (swap-buffers)))

(define (omega q)
  (let [(rotated-impulse  (rotate-vector (quaternion-conjugate q) impulse))]
    (map / rotated-impulse inertia)))

(define (dq q dt)
  (* q (apply make-quaternion 0 (map (cut / <> 2) (omega q)))))

(define (on-idle)
  (set! q (quaternion-normalize (runge-kutta q (elapsed time #t) dq)))
  (post-redisplay))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "ssim"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback on-idle)
(set! time (clock))
(glut-main-loop)
