#!/usr/bin/env guile
!#
(use-modules (system foreign)
             (rnrs bytevectors)
             (ice-9 optargs)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26)
             (glut)
             (gl)
             (gl low-level)
             (sfsim linear-algebra)
             (sfsim physics)
             (sfsim quaternion)
             (sfsim state)
             (sfsim util))


(define glut (dynamic-link "libglut"))
(define glut-wire-cube (pointer->procedure void (dynamic-func "glutWireCube" glut) (list double)))

(define time #f)
(define main-window #f)

(define state (make-state '(-0.5 0.7 0) '(0 0 0) (quaternion-rotation 0 '(1 0 0)) '(0.01 0.06 0.04)))
(define g '(0 -2.0 0))

(define m 1)
(define w 1)
(define h 0.5)
(define d 0.15)
(define inertia (inertia-body (cuboid-inertia m w h d)))

(define loss 0.6)
(define mu 0.6)

(define ground -0.99)
(define dtmax 0.025)
(define epsilon (* 0.5 (abs (cadr g)) (* dtmax dtmax)))
(define ve (sqrt (* 2 (- (cadr g)) epsilon)))
(define max-depth 3)

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

(define (on-reshape width height)
  (let* [(aspect (/ width height))
         (h      1.0)
         (w      (* aspect h))]
    (gl-viewport 0 0 width height)
    (set-gl-matrix-mode (matrix-mode projection))
    (gl-load-identity)
    (gl-ortho (- w) w (- h) h -100 +100)))

(define (on-display)
  (let* [(b   (make-bytevector (* 4 4 4)))
         (mat (rotation-matrix (orientation state)))
         (hom (concatenate (homogeneous-matrix mat (position state))))]
    (for-each (lambda (i v) (bytevector-ieee-single-native-set! b (* i 4) v)) (iota (length hom)) hom)
    (gl-clear (clear-buffer-mask color-buffer))
    (set-gl-matrix-mode (matrix-mode modelview))
    (gl-load-matrix b #:transpose #t)
    (gl-scale w h d)
    (gl-color 0 1 0)
    (glut-wire-cube 1.0)
    (swap-buffers)))

(define (collision contact state)
  (let* [(radius           (- (particle-position state contact) (position state)))
         (relative-speed   (particle-speed inertia state contact))
         (normal           '(0 1 0))
         (speed-delta      (deflect relative-speed normal loss mu ve))
         (direction         (normalize speed-delta))
         (impulse           (/ (norm speed-delta)
                               (+ (/ 1 m) (inner-product direction (cross-product (dot (inverse (inertia (orientation state)))
                                                                                  (cross-product radius direction)) radius)))))
         (impulse-vector    (* impulse direction))]
    (if (< (inner-product normal relative-speed) 0)
      (make-state (position state)
                  (+ (speed state) (* (/ 1 m) impulse-vector))
                  (quaternion-normalize (orientation state))
                  (+ (angular-momentum state) (cross-product radius impulse-vector)))
      state)))

(define (depth state corner)
  (- ground (cadr (particle-position state corner))))

(define (candidate state)
  (argmax (cut depth state <>) corners))

(define (collisions state)
  (let [(contacts (filter (lambda (corner) (>= (depth state corner) (* -2 epsilon))) corners))]
    (fold collision state contacts)))

(define* (timestep state dt #:optional (recursion 1))
  (let [(update (runge-kutta state dt (state-change inertia g)))]
    (let* [(contact (candidate update))
           (d       (depth update contact))]
      (if (>= d (* -2 epsilon))
        (if (or (<= d (- epsilon)) (>= recursion max-depth))
          (collisions update)
          (timestep (timestep state (/ dt 2) (1+ recursion)) (/ dt 2) (1+ recursion)))
        update))))

(define (on-idle)
  (let [(dt (min dtmax (elapsed time #t)))]
    (set! state (timestep state dt))
    (post-redisplay)))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "sfsim"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback on-idle)
(set! time (clock))
(glut-main-loop)
