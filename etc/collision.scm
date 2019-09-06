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

(define (face-normal coordinates face); TODO: normalise
  (cross-product (map - (list-ref coordinates (cadr face)) (list-ref coordinates (car face)))
                 (map - (list-ref coordinates (last face)) (list-ref coordinates (car face)))))

(define make-plane list)
(define plane-point car)
(define plane-normal cadr)

(define (face-plane coordinates face) (make-plane (list-ref coordinates (car face)) (face-normal coordinates face)))

(define (edges-adjacent-to-vertex vertex) (filter (lambda (edge) (member vertex edge)) edges))

(define (faces-adjacent-to-edge edge) (filter (lambda (face) (and (member (car edge) face) (member (cadr edge) face))) faces))

(define (edges-adjacent-to-face face) (map list face (append (cdr face) (list (car face)))))

(define (negative-plane plane) (make-plane (plane-point plane) (map - (plane-normal plane))))

(define (plane-distance plane point) (reduce + 0 (map * (map - point (plane-point plane)) (plane-normal plane))))

(define (inner-prod a b) (reduce + 0 (map * a b)))

(define (argop op fun lst)
  (let* [(vals  (map fun lst))
         (opval (apply op vals))]
    (list-ref (reverse lst) (1- (length (member opval vals))))))

(define (argmin fun lst) (argop min fun lst))

(define (argmax fun lst) (argop max fun lst))

(define (min-elevation object1 face1 object2 vertices2)
  (apply min (map (lambda (vertex) (plane-distance (face-plane object1 face1) (list-ref object2 vertex))) vertices2)))

(define (best-face object1 faces1 object2 vertices2)
  (let* [(face (argmax (lambda (face) (min-elevation object1 face object2 vertices2)) faces1))
         (dist (min-elevation object1 face object2 vertices2))]
    (cons face dist)))

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
        (object2 (translate '(+0.2 0 0) (rotate (rotate-x gamma) (rotate (rotate-y beta) (rotate (rotate-z alpha) coordinates)))))]
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
      (match-let [((face . dist) (best-face object1 faces object2 vertices))]
        (gl-color 0 1 0)
        (if (positive? dist)
          (for-each
            (lambda (edge)
              (apply gl-vertex (list-ref object1 (car edge)))
              (apply gl-vertex (list-ref object1 (cadr edge))))
            (edges-adjacent-to-face face))))
      (match-let [((face . dist) (best-face object2 faces object1 vertices))]
        (gl-color 0 1 0)
        (if (positive? dist)
          (for-each
            (lambda (edge)
              (apply gl-vertex (list-ref object2 (car edge)))
              (apply gl-vertex (list-ref object2 (cadr edge))))
            (edges-adjacent-to-face face)))))
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
