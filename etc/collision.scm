#!/usr/bin/env guile
!#
(use-modules (glut) (gl) (gl low-level) (ice-9 match) (srfi srfi-1) (srfi srfi-26))

(define w 0.5)
(define h 0.1)
(define d 0.25)

(define w2 (/ w 2))
(define h2 (/ h 2))
(define d2 (/ d 2))

(define vertices
  (list (list (- w2) (- h2) (- d2))
        (list (+ w2) (- h2) (- d2))
        (list (- w2) (+ h2) (- d2))
        (list (+ w2) (+ h2) (- d2))
        (list (- w2) (- h2) (+ d2))
        (list (+ w2) (- h2) (+ d2))
        (list (- w2) (+ h2) (+ d2))
        (list (+ w2) (+ h2) (+ d2))))

(define edges '((0 1) (2 3) (4 5) (6 7) (0 2) (1 3) (4 6) (5 7) (0 4) (1 5) (2 6) (3 7)))

(define faces '((0 2 3 1) (4 5 7 6) (0 1 5 4) (2 6 7 3) (0 4 6 2) (1 3 7 5)))

(define make-plane list)
(define plane-point car)
(define plane-normal cadr)

(define (cross-product a b)
  (match-let [((a1 a2 a3) a)
              ((b1 b2 b3) b)]
    (list (- (* a2 b3) (* a3 b2))
          (- (* a3 b1) (* a1 b3))
          (- (* a1 b2) (* a2 b1)))))

(define (face-normal vertices face)
  (cross-product (map - (list-ref vertices (cadr face)) (list-ref vertices (car face)))
                 (map - (list-ref vertices (last face)) (list-ref vertices (car face)))))

(define (voronoi-vertex-edge vertices vertex edge)
  (let [(a vertex)
        (b (if (eqv? vertex (car edge)) (cadr edge) (car edge)))]
    (make-plane (list-ref vertices a) (map - (list-ref vertices b) (list-ref vertices a)))))

(define (voronoi-edge-face vertices face edge)
  (cross-product (face-normal vertices face) (map - (list-ref vertices (cadr edge)) (list-ref vertices (car edge)))))
; TODO: invert edge if necessary

(define main-window #f)

(define (on-reshape width height)
  (let* [(aspect (/ width height))
         (h      1.0)
         (w      (* aspect h))]
    (gl-viewport 0 0 width height)
    (set-gl-matrix-mode (matrix-mode projection))
    (gl-load-identity)
    (gl-ortho (- w) w (- h) h -100 +100)))

(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (gl-begin (begin-mode lines)
    (gl-color 1 0 0)
    (for-each
      (lambda (edge)
        (apply gl-vertex (list-ref vertices (car edge)))
        (apply gl-vertex (list-ref vertices (cadr edge))))
      edges))
  (swap-buffers))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "sfsim"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(glut-main-loop)
