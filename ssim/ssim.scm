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

(define (sqr x) (* x x))

(define time #f)
(define main-window #f)

(define state '(0 0.7 0 0 0 0))
(define (position state) (take state 3))
(define (speed state) (drop state 3))
(define g '(0 -0.2 0))
(define q (quaternion-rotation 0.0 '(1.0 0.0 0.0)))
(define angular-momentum '(0.01 0.1 0.0))

(define m 1)
(define w 1)
(define h 0.5)
(define d 0.15)
(define inertia (cuboid-inertia m w h d))

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
      (for-each (lambda (r)
        (apply gl-vertex (+ r (position state)))
        (apply gl-vertex (+ r (position state) (* speed-scale (+ (cross-product (omega q) r) (speed state))))))
        (map (cut rotate-vector q <>) corners)))
    (swap-buffers)))

(define (on-idle)
  (let [(dt (elapsed time #t))]
    (set! state (runge-kutta state dt dstate))
    (set! q (quaternion-normalize (runge-kutta q dt dq)))
    (let* [(outer     (map (lambda (corner) (+ (rotate-vector q corner) (position state))) corners))
           (collision (argmin cadr outer))]
      (if (<= (cadr collision) ground)
        (let* [(r    (- collision (position state)))
               (n    '(0 1 0))
               (v    (+ (cross-product (omega q) r) (speed state)))
               (vrel (inner-product n v))
               (j    (/ (* (- loss 2) vrel) (+ (/ 1 m) (inner-product n (cross-product (rotate-vector q (map / (rotate-vector (quaternion-conjugate q) (cross-product r n)) inertia)) r)))))
               (J    (* j n))]
          (if (< vrel 0)
            (begin
              (format #t "vrel : ~a~&" vrel)
              (set! state (append (position state) (+ (speed state) (* (/ 1 m) J))))
              (set! angular-momentum (+ angular-momentum (cross-product r J)))
              (format #t "vrel':  ~a~&" (inner-product n (+ (cross-product (omega q) r) (speed state)))))))))
    (post-redisplay)))

; rotated inertia

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "ssim"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback on-idle)
(set! time (clock))
(glut-main-loop)
