#!/usr/bin/env guile
!#
(use-modules (srfi srfi-1))

(define m '(( 1 -1 -1 -1)
            (-1  1 -1 -1)
            ( 1  1  2  0)
            ( 1  1  0  2)))
(define q '(3 5 -9 -5))

(define (dim m) (length m))
(define (neg m) (map (lambda (row-vector) (map - row-vector)) m))
(define (z0 n) (make-list n -1))
(define (id n) (map (lambda (j) (map (lambda (i) (if (eqv? i j) 1 0)) (iota n))) (iota n)))
(define (hstack . args) (apply map append args))
(define (vec v) (map list v))
(define (argmin fun lst) (let [(vals (map fun lst))] (- (length lst) (length (member (apply min vals) vals)))))

(define (scale row-vector column) (let [(factor (list-ref row-vector column))] (map (lambda (x) (/ x factor)) row-vector)))
(define (project row-vector pivot-vector column)
  (let [(factor (list-ref row-vector column))]
    (map (lambda (a b) (- a (* b factor))) row-vector pivot-vector)))
(define (pivot tableau row column)
  (let [(pivot-vector (scale (list-ref tableau row) column))]
    (map (lambda (row-vector j) (if (eqv? j row) pivot-vector (project row-vector pivot-vector column))) tableau (iota (dim tableau)))))
(define (update-basis basis row column) (map (lambda (var index) (if (eqv? index row) column var)) basis (iota (dim basis))))

(define tableau (hstack (id (dim m)) (neg m) (vec (z0 (dim m))) (vec q)))
(define basis (iota (dim m)))

(define row (argmin last tableau))
(define column (* 2 (dim m)))
(define tableaut (pivot tableau row column))
(define basis (update-basis basis row column))


