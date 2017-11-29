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
             (ssim quaternion)
             (ssim util))


(define glut (dynamic-link "libglut"))
(define glut-wire-cube (pointer->procedure void (dynamic-func "glutWireCube" glut) (list double)))

(define time #f)
(define main-window #f)

(define state (list '(0 0.7 0)
                    '(0 0 0)
                    (quaternion-rotation 0 '(1 0 0))
                    '(0.01 0.1 0)))
(define (position         state) (car    state))
(define (speed            state) (cadr   state))
(define (orientation      state) (caddr  state))
(define (angular-momentum state) (cadddr state))
(define g '(0 -0.2 0))

(define m 1)
(define w 1)
(define h 0.5)
(define d 0.15)
(define inertia-body (cuboid-inertia m w h d))

(define loss 0.2)

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

(define (inertia state)
  (rotate-matrix (orientation state) inertia-body))

(define (omega state)
  (dot (inverse (inertia state)) (angular-momentum state)))

(define (circular-motion state r)
  (cross-product (omega state) r))

(define (corner-speed state r)
  (+ (circular-motion state r) (speed state)))

(define (dstate state dt)
  (list (speed state)
        g
        (* (vector->quaternion (* 0.5 (omega state))) (orientation state))
        '(0 0 0)))

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
         (mat (rotation-matrix (orientation state)))
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
      (for-each (lambda (r)
        (apply gl-vertex (+ r (position state)))
        (apply gl-vertex (+ r (position state) (* speed-scale (corner-speed state r)))))
        (map (cut rotate-vector (orientation state) <>) corners)))
    (swap-buffers)))


;(format #t "vrel : ~a~&" vrel)
;(format #t "vrel':  ~a~&" (inner-product n (corner-speed state r)))

(define (collision contact)
  (let* [(r    (- contact (position state)))
         (n    '(0 1 0))
         (v    (corner-speed state r))
         (vrel (inner-product n v))
         (j    (/ (* (- loss 2) vrel)
                  (+ (/ 1 m) (inner-product n (cross-product (dot (inverse (inertia state)) (cross-product r n)) r)))))
         (J    (* j n))]
    (if (< vrel 0)
      (list (position state)
            (+ (speed state) (* (/ 1 m) J))
            (quaternion-normalize (orientation state))
            (+ (angular-momentum state) (cross-product r J)))
      state)))

(define (on-idle)
  (let [(dt (elapsed time #t))]
    (set! state (runge-kutta state dt dstate))
    (let* [(outer   (map (lambda (corner) (+ (rotate-vector (orientation state) corner) (position state))) corners))
           (contact (argmin cadr outer))]
      (if (<= (cadr contact) ground)
        (set! state (collision contact))))
    (post-redisplay)))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "ssim"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback on-idle)
(set! time (clock))
(glut-main-loop)
