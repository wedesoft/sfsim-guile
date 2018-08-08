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
             (sfsim util))


(define glut (dynamic-link "libglut"))
(define glut-wire-cube (pointer->procedure void (dynamic-func "glutWireCube" glut) (list double)))

(define time #f)
(define main-window #f)

(define state (list '(0 0.7 0)
                    '(0 0 0)
                    (quaternion-rotation 0 '(1 0 0))
                    '(0.01 0.05 0.04)))
(define (position         state) (car    state))
(define (speed            state) (cadr   state))
(define (orientation      state) (caddr  state))
(define (angular-momentum state) (cadddr state))
(define g '(0 -0.4 0))

(define m 1)
(define w 1)
(define h 0.5)
(define d 0.15)
(define inertia (inertia-body (cuboid-inertia m w h d)))

(define loss 0.4)
(define mu 0.4)

(define ground -0.99)
(define dtmax 0.05)
(define epsilon (* 0.5 (abs (cadr g)) (* dtmax dtmax)))
(define ve (sqrt (* 2 (- (cadr g)) epsilon)))
(define max-depth 4)

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

(define (particle-pos state corner) (particle-position (position state) (orientation state) corner))

(define (particle-vel state corner) (particle-speed inertia (orientation state) (speed state) (angular-momentum state) corner))

(define (dstate state dt)
  (let [(v (speed state))
        (q (orientation state))
        (l (angular-momentum state))]
    (list v
          g
          (* (vector->quaternion (* 0.5 (angular-velocity inertia q l))) q)
          '(0 0 0))))

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
      (for-each (lambda (corner)
        (let [(pos (particle-pos state corner))
              (vel (particle-vel state corner))]
          (apply gl-vertex pos)
          (apply gl-vertex (+ pos (* speed-scale vel)))))
        corners))
    (swap-buffers)))

(define (collision contact state)
  (let* [(r      (- (particle-pos state contact) (position state)))
         (n      '(0 1 0))
         (d      (- ground (height state contact)))
         (v      (particle-vel state contact))
         (vrel   (inner-product n v))
         (vtan   (- v (* vrel n)))
         (vtnorm (norm vtan))
         (vtgt   (if (>= vrel (- ve)) (- ve vrel) (* (- loss 2) vrel)))
         (vfr    (min vtnorm (* mu vtgt)))
         (vtgt2  (sqrt (+ (* vfr vfr) (* vtgt vtgt))))
         (dir    (* (+ (* vfr (* vtan (/ -1 (max vtnorm 1e-6)))) (* vtgt n)) (/ 1 vtgt2)))
         (j      (/ vtgt2
                   (+ (/ 1 m) (inner-product dir (cross-product (dot (inverse (inertia (orientation state)))
                                                                     (cross-product r dir)) r)))))
         (J      (* j dir))]
    (if (< vrel 0)
      (list (position state)
            (+ (speed state) (* (/ 1 m) J))
            (quaternion-normalize (orientation state))
            (+ (angular-momentum state) (cross-product r J)))
      state)))

(define (height state corner)
  (cadr (particle-position (position state) (orientation state) corner)))

(define (depth state corner)
  (- ground (height state corner)))

(define (candidate state)
  (argmax (cut depth state <>) corners))

(define (collisions state)
  (let [(contacts (filter (lambda (corner) (>= (depth state corner) (- epsilon))) corners))]
    (fold collision state contacts)))

(define* (timestep state dt #:optional (recursion 1))
  (let [(update (runge-kutta state dt dstate))]
    (let* [(contact (candidate update))
           (d       (depth update contact))]
      (if (>= d (- epsilon))
        (if (or (<= d epsilon) (>= recursion max-depth))
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
