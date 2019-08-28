#!/usr/bin/env guile
!#
(use-modules (glut) (gl) (gl low-level))

(define w 0.5)
(define h 0.1)
(define d 0.25)

(define w2 (/ w 2))
(define h2 (/ h 2))
(define d2 (/ d 2))

(define (bit-complement a b bit)
  (and (zero? (logand a bit)) (not (zero? (logand b bit))) (eqv? (logand a (lognot bit)) (logand b (lognot bit)))))

(define vertices
  (list (list (- w2) (- h2) (- d2))
        (list (+ w2) (- h2) (- d2))
        (list (- w2) (+ h2) (- d2))
        (list (+ w2) (+ h2) (- d2))
        (list (- w2) (- h2) (+ d2))
        (list (+ w2) (- h2) (+ d2))
        (list (- w2) (+ h2) (+ d2))
        (list (+ w2) (+ h2) (+ d2))))

(define edges
  (filter
    (lambda (edge) (or (bit-complement (car edge) (cdr edge) 1)
                       (bit-complement (car edge) (cdr edge) 2)
                       (bit-complement (car edge) (cdr edge) 4)))
    (map (lambda (idx) (cons (modulo idx 8) (floor (/ idx 8)))) (iota (* 8 8)))))

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
        (apply gl-vertex (list-ref vertices (cdr edge))))
      edges))
  (swap-buffers))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "sfsim"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(glut-main-loop)
