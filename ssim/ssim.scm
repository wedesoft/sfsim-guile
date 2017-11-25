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
             (ssim linear-algebra)
             (ssim physics)
             (ssim quaternion))


(define glut (dynamic-link "libglut"))
(define glut-wire-cube (pointer->procedure void (dynamic-func "glutWireCube" glut) (list double)))

(define (sqr x) (* x x))

(define time #f)
(define main-window #f)

(define state '(0 0 0 0 0 0))
(define (position state) (take state 3))
(define (speed state) (drop state 3))
(define g '(0 -0.1 0))
(define q (quaternion-rotation 0.0 '(1.0 0.0 0.0)))
(define angular-momentum '(0.01 0.1 0))

(define m 1)
(define w 1)
(define h 0.5)
(define d 0.15)
(define inertia (cuboid-inertia m w h d))

(define ground -1.0)

(define speed-scale 0.3)

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

(define (argop op fun lst)
  (let* [(vals  (map fun lst))
         (opval (apply op vals))]
    (list-ref (reverse lst) (1- (length (member opval vals))))))
(define (argmin fun lst) (argop min fun lst))
(define (argmax fun lst) (argop max fun lst))

(define (omega q)
  (let [(rotated-momentum  (rotate-vector (quaternion-conjugate q) angular-momentum))]
    (rotate-vector q (map / rotated-momentum inertia))))

(define (dstate state dt)
  (append (speed state) g))

(define (dq q dt)
  (* (apply make-quaternion 0 (* 0.5 (omega q))) q))

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
         (hom (concatenate (append (map append mat (map list (position state))) '((0 0 0 1)))))]
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
    (gl-load-identity)
    (gl-color 0 0 1)
    (gl-begin (begin-mode lines)
      (for-each (lambda (p)
        (apply gl-vertex p)
        (apply gl-vertex (+ p (* speed-scale (+ (cross-product (omega q) p) (speed state))))))
        (map (lambda (corner) (+ (rotate-vector q corner) (position state))) corners)))
    (swap-buffers)))

(define (on-idle)
  (let [(dt (elapsed time #t))]
    (set! state (runge-kutta state dt dstate))
    (set! q (quaternion-normalize (runge-kutta q dt dq)))
    (let* [(outer     (map (lambda (corner) (+ (rotate-vector q corner) (position state))) corners))
           (collision (argmin cadr outer))]
      (if (<= (cadr collision) ground)
        (set! state '(0 0 0 0 0 0))))
    (post-redisplay)))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "ssim"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback on-idle)
(set! time (clock))
(glut-main-loop)
