#!/usr/bin/env guile
!#
(use-modules (glut) (gl) (gl low-level) (ice-9 match) (srfi srfi-1) (srfi srfi-26))

(define w 0.5)
(define h 0.1)
(define d 0.25)

(define w2 (/ w 2))
(define h2 (/ h 2))
(define d2 (/ d 2))

(define alpha 0.0)
(define beta 0.0)
(define gamma 0.0)

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

(define (vertex? v) (number? v))
(define (edge? e) (and (list? e) (eqv? (length e) 2)))
(define (face? f) (and (list? f) (eqv? (length f) 4)))

(define (rotate-z gamma)
  (list (list (cos gamma) (- (sin gamma)) 0)
        (list (sin gamma) (cos gamma) 0)
        (list 0 0 1)))

(define (rotate-y beta)
  (list (list (cos beta) 0 (- (sin beta)))
        (list 0 1 0)
        (list (sin beta) 0 (cos beta))))

(define (rotate-x alpha)
  (list (list 1 0 0)
        (list 0 (cos alpha) (- (sin alpha)))
        (list 0 (sin alpha) (cos alpha))))

(define (dot matrix point) (map (lambda (row) (reduce + 0 (map (lambda (m p) (* m p)) row point))) matrix))

(define (rotate matrix vertices) (map (lambda (point) (dot matrix point)) vertices))

(define (index-of a b) (list-index (cut eqv? a <>) b))

(define (swap-edge edge) (list (cadr edge) (car edge)))

(define (face-edge face edge)
  (let [(i (index-of (car edge) face))
        (j (index-of (cadr edge) face))]
    (if (or (eqv? (1+ i) j) (eqv? (- i 3) j)) edge (swap-edge edge))))

(define (edge-vector vertices edge) (map - (list-ref vertices (cadr edge)) (list-ref vertices (car edge))))

(define (vertex-edge vertex edge) (if (eqv? (car edge) vertex) edge (swap-edge edge)))

(define (cross-product a b)
  (match-let [((a1 a2 a3) a)
              ((b1 b2 b3) b)]
    (list (- (* a2 b3) (* a3 b2))
          (- (* a3 b1) (* a1 b3))
          (- (* a1 b2) (* a2 b1)))))

(define (face-normal vertices face)
  (cross-product (map - (list-ref vertices (cadr face)) (list-ref vertices (car face)))
                 (map - (list-ref vertices (last face)) (list-ref vertices (car face)))))

(define make-plane list)
(define plane-point car)
(define plane-normal cadr)

(define (negative-plane plane) (make-plane (plane-point plane) (map - (plane-normal plane))))

(define (plane-distance plane point) (reduce + 0 (map * (map - point (plane-point plane)) (plane-normal plane))))

(define (voronoi-vertex-edge vertices vertex edge)
  (let [(ordered (vertex-edge vertex edge))] (make-plane (list-ref vertices vertex) (edge-vector vertices ordered))))

(define (adjacent-edges vertex) (filter (lambda (edge) (member vertex edge)) edges))

(define (adjacent-faces edge) (filter (lambda (face) (and (member (car edge) face) (member (cadr edge) face))) faces))

(define (voronoi-vertex vertices vertex)
  (map (lambda (edge) (negative-plane (voronoi-vertex-edge vertices vertex edge))) (adjacent-edges vertex)))

(define (in-voronoi-vertex vertices vertex point)
  (every (lambda (plane) (positive? (plane-distance plane point))) (voronoi-vertex vertices vertex)))

(define (voronoi-face-edge vertices face edge)
  (let [(ordered (face-edge face edge))]
    (make-plane (list-ref vertices (car ordered)) (cross-product (edge-vector vertices ordered) (face-normal vertices face)))))

(define (voronoi-edge vertices edge)
  (append (map (cut voronoi-vertex-edge vertices <> edge) edge) (map (cut voronoi-face-edge vertices <> edge) (adjacent-faces edge))))

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
  (let [(rotated (rotate (rotate-z gamma) (rotate (rotate-y beta) (rotate (rotate-x alpha) vertices))))]
    (gl-clear (clear-buffer-mask color-buffer))
    (gl-begin (begin-mode lines)
      (gl-color 1 0 0)
      (for-each
        (lambda (edge)
          (apply gl-vertex (list-ref rotated (car edge)))
          (apply gl-vertex (list-ref rotated (cadr edge))))
        edges)
      (gl-color 0 0 1)
      (for-each
        (lambda (i)
          (if (in-voronoi-vertex rotated i '(-1 -1 0))
            (begin
              (apply gl-vertex (list-ref rotated i))
              (gl-vertex -1 -1 0))))
        (iota 8)))
    (swap-buffers)))

(define (on-idle)
  (set! alpha (+ alpha 0.02))
  (set! beta (+ beta 0.01))
  (set! gamma (+ gamma 0.005))
  (post-redisplay))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "sfsim"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback on-idle)
(glut-main-loop)
