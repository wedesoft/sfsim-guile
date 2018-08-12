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

(define state1 (make-state '(0 0.6 0) '(0 -0.1 0) (quaternion-rotation 0 '(1 0 0)) '(0.005 0.02 0.01)))
(define state2 (make-state '(0 -0.6 0) '(0 0.1 0) (quaternion-rotation 0 '(1 0 0)) '(0.015 0.005 0.01)))
(define g '(0 -0.5 0))

(define m 1)
(define w 0.5)
(define h 0.25)
(define d 0.1)
(define inertia (inertia-body (cuboid-inertia m w h d)))

(define loss 0.6)
(define mu 0.6)

(define ground -0.99)
(define dtmax 0.025)
(define epsilon (* 0.5 (abs (cadr g)) (* dtmax dtmax)))
(define ve (sqrt (* 2 (abs (cadr g)) epsilon)))
(define max-depth 10)

(define g '(0 0 0))
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

(define (show state)
  (let* [(b   (make-bytevector (* 4 4 4)))
         (mat (rotation-matrix (orientation state)))
         (hom (concatenate (homogeneous-matrix mat (position state))))]
    (for-each (lambda (i v) (bytevector-ieee-single-native-set! b (* i 4) v)) (iota (length hom)) hom)
    (set-gl-matrix-mode (matrix-mode modelview))
    (gl-load-matrix b #:transpose #t)
    (gl-scale w h d)
    (gl-color 0 1 0)
    (glut-wire-cube 1.0))
)

(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (show state1)
  (show state2)
  (swap-buffers))

(define (collision closest state1 state2)
  (let* [(radius1        (- (car closest) (position state1)))
         (radius2        (- (cdr closest) (position state2)))
         (s1             (+ (speed state1) (cross-product (angular-velocity inertia (orientation state1) (angular-momentum state1)) radius1)))
         (s2             (+ (speed state2) (cross-product (angular-velocity inertia (orientation state2) (angular-momentum state2)) radius2)))
         (relative-speed (- s1 s2))
         (normal         (normalize (- (car closest) (cdr closest))))
         (speed-delta    (deflect relative-speed normal loss mu ve))
         (direction      (normalize speed-delta))
         (impulse        (/ (norm speed-delta)
                            (+ (/ 1 m) (/ 1 m) (inner-product direction (cross-product (dot (inverse (inertia (orientation state1)))
                                                                                       (cross-product radius1 direction)) radius1)))))
         (impulse-vector (* impulse direction))]
    (if (< (inner-product normal relative-speed) 0)
     (cons (make-state (position state1)
                  (+ (speed state1) (* (/ 1 m) impulse-vector))
                  (quaternion-normalize (orientation state1))
                  (+ (angular-momentum state1) (cross-product radius1 impulse-vector)))
          (make-state (position state2)
                  (+ (speed state2) (* (/ 1 m) (- impulse-vector)))
                  (quaternion-normalize (orientation state2))
                  (+ (angular-momentum state2) (cross-product radius2 (- impulse-vector))))
    )
     (cons state1 state2)
     )
    )
)

(define (depth state corner)
  (- ground (cadr (particle-position state corner))))

(define (candidate state)
  (argmax (cut depth state <>) corners))

(define* (timestep state1 state2 dt #:optional (recursion 1))
  (let [(update1 (runge-kutta state1 dt (state-change inertia g)))
        (update2 (runge-kutta state2 dt (state-change inertia g)))]
    (let* [(closest (gjk-algorithm (map (cut particle-position update1 <>) corners)
                                   (map (cut particle-position update2 <>) corners)))
           (d       (- (norm (-(car closest) (cdr closest)))))]
      (if (>= d (* -2 epsilon))
        (if (or (<= d (- epsilon)) (>= recursion max-depth))
          (begin (format #t "depth ~a recursion ~a~&" d recursion) (collision closest update1 update2))
          (let [(update (timestep state1 state2 (/ dt 2) (1+ recursion)))]
            (timestep (car update) (cdr update) (/ dt 2) (1+ recursion))))
        (cons update1 update2)))))

(define (on-idle)
  (let [(dt (min dtmax (elapsed time #t)))]
    (let [(update (timestep state1 state2 dt))]
      (set! state1 (car update))
      (set! state2 (cdr update)))
    (post-redisplay)))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "sfsim"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback on-idle)
(set! time (clock))
(glut-main-loop)
