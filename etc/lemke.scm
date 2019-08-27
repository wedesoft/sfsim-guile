#!/usr/bin/env guile
!#
(use-modules (srfi srfi-1))

(define (dim m) (length m))
(define (neg m) (map (lambda (row-vector) (map - row-vector)) m))
(define (z0 n) (make-list n -1))
(define (id n) (map (lambda (j) (map (lambda (i) (if (eqv? i j) 1 0)) (iota n))) (iota n)))
(define (hstack . args) (apply map append args))
(define (vec v) (map list v))
(define (argmin fun lst)
  (let* [(vals (map fun lst))
         (filtered (filter identity vals))]
    (if (null? filtered) #f (- (length lst) (length (member (apply min filtered) vals))))))

(define (scale row-vector column) (let [(factor (list-ref row-vector column))] (map (lambda (x) (/ x factor)) row-vector)))
(define (project row-vector pivot-vector column)
  (let [(factor (list-ref row-vector column))]
    (map (lambda (a b) (- a (* b factor))) row-vector pivot-vector)))
(define (pivot tableau row column)
  (let [(pivot-vector (scale (list-ref tableau row) column))]
    (map (lambda (row-vector j) (if (eqv? j row) pivot-vector (project row-vector pivot-vector column))) tableau (iota (dim tableau)))))
(define (update-basis basis row column) (map (lambda (var index) (if (eqv? index row) column var)) basis (iota (dim basis))))
(define (complement basis row)
  (let [(element (list-ref basis row))]
    (if (eqv? element (* 2 (dim basis))) #f (if (< element (dim basis)) (+ element (dim basis)) (- element (dim basis))))))
(define (best-ratio tableau column)
  (argmin (lambda (row-vector) (let [(c (list-ref row-vector column))] (if (positive? c) (/ (last row-vector) c) #f))) tableau))

(define (lemke-iterate tableau basis column row)
  (let [(tableau (pivot tableau row column))
        (compl (complement basis row))
        (basis (update-basis basis row column))]
    (if (not compl)
      (list tableau basis)
      (let [(row (best-ratio tableau compl))]
        (if row (lemke-iterate tableau basis compl row) #f)))))

(define (lemke tableau) (lemke-iterate tableau (iota (dim tableau)) (* 2 (dim tableau)) (argmin last tableau)))

(define (lcp m q)
  (let [(solution (lemke (hstack (id (dim m)) (neg m) (vec (z0 (dim m))) (vec q))))]
    (if solution
      (let* [(tableau (car solution))
             (basis (cadr solution))
             (lookup (map cons basis (map last tableau)))]
        (map (lambda (idx) (or (assq-ref lookup idx) 0)) (iota (* 2 (dim tableau)))))
      #f)))

(define m '(( 1 -1 -1 -1)
            (-1  1 -1 -1)
            ( 1  1  2  0)
            ( 1  1  0  2)))
(define q '(3 5 -9 -5))
(lcp m q)

(define m '((1 0 0)
            (2 1 0)
            (2 2 1)))
(define q '(-8 -12 -14))
(lcp m q)

(define m '((-1  0 -3)
            ( 1 -2 -5)
            (-2 -1 -2)))
(define q '(-3 -2 -1))
(lcp m q)

(define m '((1 2 0)
            (0 1 2)
            (2 0 1)))
(define q '(-1 -1 -1))
(lcp m q); TODO: prevent cycling
