#!/usr/bin/env guile
!#
(use-modules (glut) (gl) (gl low-level) (srfi srfi-26))

(define w 0.5)
(define h 0.1)
(define d 0.25)

(define w2 (/ w 2))
(define h2 (/ h 2))
(define d2 (/ d 2))

(define (bit-complement a b bit)
  (and (zero? (logand a bit)) (not (zero? (logand b bit))) (eqv? (logand a (lognot bit)) (logand b (lognot bit)))))

(define (vertex m n) (logand (ash m (- (* 3 n))) 7))

(define (edge-bits m a b)
  (and (eqv? (logand a (lognot m))
             (logand b (lognot m)))
       (< a b)))

(define (face-bits m a b c d)
  (and (eqv? (logand a m)
             (logand b m)
             (logand c m)
             (logand d m))
       (< a b)
       (< b c)
       (< c d)))

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
    (lambda (edge) (or (apply edge-bits 1 edge)
                       (apply edge-bits 2 edge)
                       (apply edge-bits 4 edge)))
    (map (lambda (idx) (map (cut vertex idx <>) (iota 2))) (iota (* 8 8)))))

(define faces
  (filter
    (lambda (face) (or (apply face-bits 1 face)
                       (apply face-bits 2 face)
                       (apply face-bits 4 face)))
    (map (lambda (idx) (map (cut vertex idx <>) (iota 4))) (iota (* 8 8 8 8)))))

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
