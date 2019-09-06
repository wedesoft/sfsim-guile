#!/usr/bin/env guile
!#
(use-modules (glut) (gl) (gl low-level) (ice-9 match) (ice-9 format) (srfi srfi-1) (srfi srfi-26))

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

(define (translate vec coordinates) (map (lambda (point) (map + point vec)) coordinates))

(define (dot matrix point) (map (lambda (row) (reduce + 0 (map (lambda (m p) (* m p)) row point))) matrix))

(define (rotate matrix coordinates) (map (lambda (point) (dot matrix point)) coordinates))

(define (index-of a b) (list-index (cut = a <>) b))

(define (flip-edge edge) (reverse edge))

(define (order-edge-for-face face edge)
  (let [(i (index-of (car edge) face)) (j (index-of (cadr edge) face))]
    (if (or (eqv? (1+ i) j) (eqv? (- i 3) j)) edge (flip-edge edge))))

(define (order-edge-for-vertex vertex edge) (if (eqv? (car edge) vertex) edge (flip-edge edge)))

(define (edge-head coordinates edge) (list-ref coordinates (cadr edge)))

(define (edge-tail coordinates edge) (list-ref coordinates (car edge)))

(define (edge-vector coordinates edge) (map - (edge-head coordinates edge) (edge-tail coordinates edge)))

(define (edge-point coordinates edge l)
  (map + (edge-tail coordinates edge) (map (cut * l <>) (edge-vector coordinates edge))))

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
  (map (lambda (edge) (cons (order-edge-for-vertex vertex edge) (negative-plane (voronoi-vertex-edge coordinates vertex edge))))
       (edges-adjacent-to-vertex vertex)))

(define (voronoi-edge coordinates edge)
  (append (map (lambda (vertex) (cons vertex (voronoi-vertex-edge coordinates vertex edge))) edge)
          (map (lambda (face) (cons face (voronoi-face-edge coordinates face edge))) (faces-adjacent-to-edge edge))))

(define (voronoi-face coordinates face)
  (cons (cons (random 8) (make-plane (list-ref coordinates (car face)) (face-normal coordinates face)))
        (map (lambda (edge) (cons (order-edge-for-face edge) (negative-plane (voronoi-face-edge coordinates face edge))))
             (edges-adjacent-to-face face))))

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

(define (closest-point coordinates start point)
  (let [(next (out-of-voronoi (voronoi coordinates start) point))]
    (if next
      (closest-point coordinates next point)
      (feature-closest coordinates start point))))

(define (swap pair) (cons (cdr pair) (car pair)))

(define (clip-edge-voronoi coordinates edge feature)
  (let [(dt (plane-distance (cdr feature) (edge-tail coordinates edge)))
        (dh (plane-distance (cdr feature) (edge-head coordinates edge)))]
    (if (>= dt 0)
      (if (>= dh 0)
        (list -1 2 #f)
        (let [(l (/ dt (- dt dh)))] (list 0 l (car feature))))
      (if (< dh 0)
        (list 1 0 (car feature))
        (let [(l (/ dt (- dt dh)))] (list l 1 (car feature)))))))

(define (extremum e key lst)
  (let* [(keys (map key lst))
         (m    (apply e keys))]
    (list-ref lst (index-of m keys))))

(define (clip-edge-vertex object1 edge1 object2 vertex2)
  (let* [(clip (map (cut clip-edge-voronoi object1 edge1 <>) (voronoi-vertex object2 vertex2)))
         (lower-bound (extremum max car clip))
         (upper-bound (extremum min cadr clip))]
    (if (and (eqv? (car lower-bound) -1) (eqv? (cadr upper-bound) 2))
      (cons (car edge1) vertex2)
      (if (and (caddr lower-bound)
               (positive? (inner-prod (edge-vector object1 edge1)
                                      (map - (edge-point object1 edge1 (car lower-bound)) (list-ref object2 vertex2)))))
        (edge-edge-test object1 edge1 object2 (caddr lower-bound))
        (if (and (caddr upper-bound)
                 (negative? (inner-prod (edge-vector object1 edge1)
                                        (map - (edge-point object1 edge1 (cadr upper-bound)) (list-ref object2 vertex2)))))
          (edge-edge-test object1 edge1 object2 (caddr upper-bound))
          (cons (car edge1) vertex2))))))

(define (edge-vertex-test object1 edge1 object2 vertex2)
  (let* [(candidates1 (voronoi-edge object1 edge1))
         (feature1 (out-of-voronoi candidates1 (list-ref object2 vertex2)))]
    (if feature1
      (if (vertex? feature1)
        (vertex-vertex-test object1 feature1 object2 vertex2)
        (face-vertex-test object1 feature1 object2 vertex2))
      (clip-edge-vertex object1 edge1 object2 vertex2))))

(define (vertex-edge-test object1 vertex1 object2 edge2)
  (swap (edge-vertex-test object2 edge2 object1 vertex1)))

(define (vertex-vertex-test object1 vertex1 object2 vertex2)
  (let* [(candidates1 (voronoi-vertex object1 vertex1))
         (edge1 (out-of-voronoi candidates1 (list-ref object2 vertex2)))]
    (if edge1
      (edge-vertex-test object1 edge1 object2 vertex2)
      (let* [(candidates2 (voronoi-vertex object2 vertex2))
             (edge2 (out-of-voronoi candidates2 (list-ref object1 vertex1)))]
        (if edge2
          (vertex-edge-test object1 vertex1 object2 edge2)
          (cons vertex1 vertex2))))))

(define (edge-edge-test object1 edge1 object2 edge2)
  (cons (car edge1) (car edge2))); TODO: implement this

(define (face-vertex-test object1 face1 object2 vertex2)
  (cons (car face1) vertex2)); TODO: implement this test

(define (closest-points object1 feature1 object2 feature2)
  (cons (list-ref object1 feature1) (list-ref object2 feature2)))

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
  (let [(object1 (translate '(-0.5 0 0) (rotate (rotate-z gamma) (rotate (rotate-y beta) (rotate (rotate-x alpha) coordinates)))))
        (object2 (translate '(+0.5 0 0) (rotate (rotate-x gamma) (rotate (rotate-y beta) (rotate (rotate-z alpha) coordinates)))))]
    (gl-clear (clear-buffer-mask color-buffer))
    (gl-begin (begin-mode lines)
      (gl-color 1 0 0)
      (for-each
        (lambda (object)
          (for-each
           (lambda (edge)
             (apply gl-vertex (list-ref object (car edge)))
             (apply gl-vertex (list-ref object (cadr edge))))
           edges))
        (list object1 object2))
      (let* [(feature-pair (vertex-vertex-test object1 (random 8) object2 (random 8)))
             (points (closest-points object1 (car feature-pair) object2 (cdr feature-pair)))]; TOOD: compute closest points
        (gl-color 0 0 1)
        (apply gl-vertex (car points))
        (apply gl-vertex (cdr points))))
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
