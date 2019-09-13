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

(define (edge-tail coordinates edge) (list-ref coordinates (car edge)))

(define (edge-head coordinates edge) (list-ref coordinates (cadr edge)))

(define (edge-vector coordinates edge) (map - (edge-head coordinates edge) (edge-tail coordinates edge)))

(define (edge-point coordinates edge l)
  (map + (edge-tail coordinates edge) (map (cut * l <>) (edge-vector coordinates edge))))

(define (inner-prod a b) (reduce + 0 (map * a b)))

(define (norm a) (sqrt (inner-prod a a)))

(define (cross-product a b)
  (match-let [((a1 a2 a3) a) ((b1 b2 b3) b)]
    (list (- (* a2 b3) (* a3 b2)) (- (* a3 b1) (* a1 b3)) (- (* a1 b2) (* a2 b1)))))

(define (normalize vec) (let [(n (norm vec))] (map (cut / <> n) vec)))

(define (face-normal coordinates face); TODO: normalise
  (normalize (cross-product (map - (list-ref coordinates (cadr face)) (list-ref coordinates (car face)))
                            (map - (list-ref coordinates (last face)) (list-ref coordinates (car face))))))

(define make-plane list)
(define plane-point car)
(define plane-normal cadr)

(define (face-plane coordinates face) (make-plane (list-ref coordinates (car face)) (face-normal coordinates face)))

; https://math.stackexchange.com/questions/1414285/location-of-shortest-distance-between-two-skew-lines-in-3d
(define (point-between-lines object1 edge1 object2 edge2)
  (let* [(p1 (edge-tail object1 edge1))
         (d1 (edge-vector object1 edge1))
         (p2 (edge-tail object2 edge2))
         (d2 (edge-vector object2 edge2))
         (n (cross-product d1 d2))
         (n1 (cross-product d1 n))
         (n2 (cross-product d2 n))
         (f1 (/ (inner-prod (map - p2 p1) n2) (inner-prod d1 n2)))
         (f2 (/ (inner-prod (map - p2 p1) n1) (inner-prod d2 n1)))
         (c1 (map + p1 (map (cut * <> f1) d1)))
         (c2 (map + p2 (map (cut * <> f1) d2)))]
    (map (lambda (a b) (/ (+ a b) 2)) c1 c2)))

(define (center coordinates) (map (cut / <> (length coordinates)) (apply map + coordinates)))

(define (edges-plane object1 edge1 vertices1 object2 edge2 vertices2)
  (let* [(p1 (edge-tail object1 edge1))
         (p2 (edge-tail object2 edge2))
         (n (cross-product (edge-vector object1 edge1) (edge-vector object2 edge2)))
         (plane1 (make-plane p1 n))
         (plane2 (make-plane p2 n))]
    (let [(e1 (elevation max plane1 object1 vertices1))
          (e2 (elevation min plane2 object2 vertices2))]
      (if (and (>= e2 -1e-6) (<= e1 1e-6))
        (cons plane1 plane2)
        (let [(e1 (elevation min plane1 object1 vertices1))
              (e2 (elevation max plane2 object2 vertices2))]
          (if (and (>= e1 -1e-6) (<= e2 1e-6))
            (cons (negative-plane plane1) (negative-plane plane2))
            #f))))))

(define (edges-adjacent-to-vertex vertex) (filter (lambda (edge) (member vertex edge)) edges))

(define (faces-adjacent-to-edge edge) (filter (lambda (face) (and (member (car edge) face) (member (cadr edge) face))) faces))

(define (edges-adjacent-to-face face) (map list face (append (cdr face) (list (car face)))))

(define (negative-plane plane) (make-plane (plane-point plane) (map - (plane-normal plane))))

(define (plane-distance plane point) (reduce + 0 (map * (map - point (plane-point plane)) (plane-normal plane))))

(define (argop op fun lst)
  (let* [(vals  (map fun lst))
         (opval (apply op vals))]
    (list-ref (reverse lst) (1- (length (member opval vals))))))

(define (argmin fun lst) (argop min fun lst))

(define (argmax fun lst) (argop max fun lst))

(define (elevation reduce plane object2 vertices2)
  (apply reduce (map (lambda (vertex) (plane-distance plane (list-ref object2 vertex))) vertices2)))

(define (combine set1 set2)
  (list (append-map (lambda (x) (make-list (length set2) x)) set1) (apply append (make-list (length set1) set2))))

(define (separation planes)
  (if (not planes) -1 (plane-distance (car planes) (plane-point (cdr planes)))))

(define (best-edge-pair object1 edges1 vertices1 object2 edges2 vertices2)
  (let* [(edge-pair (argmax
                      (lambda (pair)
                        (match-let [((edge1 . edge2) pair)]
                          (separation (edges-plane object1 edge1 vertices1 object2 edge2 vertices2))))
                      (apply map cons (combine edges1 edges2))))
        (dist (separation (edges-plane object1 (car edge-pair) vertices1 object2 (cdr edge-pair) vertices2)))]
    (cons edge-pair dist)))

(define (best-face object1 faces1 object2 vertices2)
  (let* [(face (argmax (lambda (face) (elevation min (face-plane object1 face) object2 vertices2)) faces1))
         (vertex (argmin (lambda (vertex) (plane-distance (face-plane object1 face) (list-ref object2 vertex))) vertices2))
         (dist (plane-distance (face-plane object1 face) (list-ref object2 vertex)))]
    (cons (cons face vertex) dist)))

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
  (let [(object1 (translate '(-0.2 0 0) (rotate (rotate-z gamma) (rotate (rotate-y beta) (rotate (rotate-x alpha) coordinates)))))
        (object2 (translate '(+0.2 0 0) (rotate (rotate-x alpha) (rotate (rotate-y beta) (rotate (rotate-z gamma) coordinates)))))]
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
      (gl-color 0 1 0)
      (match-let [((face-point1 . dist1) (best-face object1 faces object2 vertices))
                  ((face-point2 . dist2) (best-face object2 faces object1 vertices))
                  ((edges . dist3) (best-edge-pair object1 edges vertices object2 edges vertices))]
        (if (and (positive? dist1) (>= dist1 dist2) (>= dist1 dist3))
          (for-each
            (lambda (edge)
              (apply gl-vertex (list-ref object1 (car edge)))
              (apply gl-vertex (list-ref object1 (cadr edge))))
              (edges-adjacent-to-face (car face-point1))))
        (if (and (positive? dist2) (> dist2 dist1) (>= dist2 dist3))
          (for-each
            (lambda (edge)
              (apply gl-vertex (list-ref object2 (car edge)))
              (apply gl-vertex (list-ref object2 (cadr edge))))
            (edges-adjacent-to-face (car face-point2))))
        (if (and (positive? dist3) (> dist3 dist1) (> dist3 dist2))
          (begin
            (apply gl-vertex (list-ref object1 (car (car edges))))
            (apply gl-vertex (list-ref object1 (cadr (car edges))))
            (apply gl-vertex (list-ref object2 (car (cdr edges))))
            (apply gl-vertex (list-ref object2 (cadr (cdr edges))))))
        (format #t "~a~&" (max dist1 dist2 dist3))))
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
