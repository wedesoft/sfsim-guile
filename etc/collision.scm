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

(define coordinates
  (list (list (- w2) (- h2) (- d2))
        (list (+ w2) (- h2) (- d2))
        (list (- w2) (+ h2) (- d2))
        (list (+ w2) (+ h2) (- d2))
        (list (- w2) (- h2) (+ d2))
        (list (+ w2) (- h2) (+ d2))
        (list (- w2) (+ h2) (+ d2))
        (list (+ w2) (+ h2) (+ d2))))

(define vertices '(0 1 2 3 4 5 6 7))
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

(define (rotate matrix coordinates) (map (lambda (point) (dot matrix point)) coordinates))

(define (index-of a b) (list-index (cut eqv? a <>) b))

(define (flip-edge edge) (reverse edge))

(define (order-edge-for-face face edge)
  (let [(i (index-of (car edge) face)) (j (index-of (cadr edge) face))]
    (if (or (eqv? (1+ i) j) (eqv? (- i 3) j)) edge (flip-edge edge))))

(define (order-edge-for-vertex vertex edge) (if (eqv? (car edge) vertex) edge (flip-edge edge)))

(define (edge-vector coordinates edge) (map - (list-ref coordinates (cadr edge)) (list-ref coordinates (car edge))))

(define (cross-product a b)
  (match-let [((a1 a2 a3) a) ((b1 b2 b3) b)]
    (list (- (* a2 b3) (* a3 b2)) (- (* a3 b1) (* a1 b3)) (- (* a1 b2) (* a2 b1)))))

(define (face-normal coordinates face)
  (cross-product (map - (list-ref coordinates (cadr face)) (list-ref coordinates (car face)))
                 (map - (list-ref coordinates (last face)) (list-ref coordinates (car face)))))

(define make-plane list)
(define plane-point car)
(define plane-normal cadr)

(define (edges-adjacent-to-vertex vertex) (filter (lambda (edge) (member vertex edge)) edges))

(define (faces-adjacent-to-edge edge) (filter (lambda (face) (and (member (car edge) face) (member (cadr edge) face))) faces))

(define (edges-adjacent-to-face face) (map list face (append (cdr face) (list (car face)))))

(define (negative-plane plane) (make-plane (plane-point plane) (map - (plane-normal plane))))

(define (plane-distance plane point) (reduce + 0 (map * (map - point (plane-point plane)) (plane-normal plane))))

(define (voronoi-vertex-edge coordinates vertex edge)
  (let [(ordered (order-edge-for-vertex vertex edge))]
    (make-plane (list-ref coordinates vertex) (edge-vector coordinates ordered))))

(define (voronoi-face-edge coordinates face edge)
  (let [(ordered (order-edge-for-face face edge))]
    (make-plane (list-ref coordinates (car ordered))
                (cross-product (edge-vector coordinates ordered) (face-normal coordinates face)))))

(define (voronoi-vertex coordinates vertex)
  (map (lambda (edge) (cons edge (negative-plane (voronoi-vertex-edge coordinates vertex edge)))) (edges-adjacent-to-vertex vertex)))

(define (voronoi-edge coordinates edge)
  (append (map (lambda (vertex) (cons vertex (voronoi-vertex-edge coordinates vertex edge))) edge)
          (map (lambda (face) (cons face (voronoi-face-edge coordinates face edge))) (faces-adjacent-to-edge edge))))

(define (voronoi-face coordinates face)
  (cons (cons (random 8) (make-plane (list-ref coordinates (car face)) (face-normal coordinates face)))
        (map (lambda (edge) (cons edge (negative-plane (voronoi-face-edge coordinates face edge)))) (edges-adjacent-to-face face))))

(define (voronoi coordinates feature)
  ((cond ((vertex? feature) voronoi-vertex)
         ((edge?   feature) voronoi-edge  )
         ((face?   feature) voronoi-face  )) coordinates feature))

(define (out-of-voronoi candidates point)
  (any (lambda (candidate) (if (negative? (plane-distance (cdr candidate) point)) (car candidate) #f)) candidates))

(define (inner-prod a b) (reduce + 0 (map * a b)))

(define (norm2 vec) (inner-prod vec vec))

(define (vertex-closest coordinates vertex point) (list-ref coordinates vertex))

(define (edge-closest coordinates edge point)
  (let* [(base  (list-ref coordinates (car edge)))
         (vec   (edge-vector coordinates edge))
         (d     (/ (inner-prod (map - point base) vec) (norm2 vec)))]
    (map + (map (cut * d <>) vec) base)))

(define (face-closest coordinates face point)
  (let* [(base  (list-ref coordinates (car face)))
         (vec   (face-normal coordinates face))
         (d     (/ (inner-prod (map - point base) vec) (norm2 vec)))]
    (map - point (map (cut * d <>) vec))))

(define (feature-closest coordinates feature point)
  ((cond ((vertex? feature) vertex-closest)
         ((edge?   feature) edge-closest  )
         ((face?   feature) face-closest  )) coordinates feature point))

(define (closest coordinates start point)
  (let [(next (out-of-voronoi (voronoi coordinates start) point))]
    (if next
      (closest coordinates next point)
      (feature-closest coordinates start point))))

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
  (let [(rotated (rotate (rotate-z gamma) (rotate (rotate-y beta) (rotate (rotate-x alpha) coordinates))))]
    (gl-clear (clear-buffer-mask color-buffer))
    (gl-begin (begin-mode lines)
      (gl-color 1 0 0)
      (for-each
        (lambda (edge)
          (apply gl-vertex (list-ref rotated (car edge)))
          (apply gl-vertex (list-ref rotated (cadr edge))))
        edges)
      (gl-color 0 0 1)
      (gl-vertex -1 -1 0)
      (apply gl-vertex (closest rotated (random 8) '(-1 -1 0))))
    (swap-buffers)))

(define (on-idle)
  (set! alpha (+ alpha 0.021))
  (set! beta (+ beta 0.01))
  (set! gamma (+ gamma 0.0052))
  (post-redisplay))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "sfsim"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback on-idle)
(glut-main-loop)
