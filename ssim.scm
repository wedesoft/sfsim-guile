#!/usr/bin/env guile
!#
(use-modules (system foreign) (glut) (gl))


(define glut (dynamic-link "libglut"))
(define glut-wire-cube (pointer->procedure void (dynamic-func "glutWireCube" glut) (list double)))

(define (on-reshape width height)
  (pk 'reshape! width height)
  (gl-viewport 0 0 width height)
  (set-gl-matrix-mode (matrix-mode projection))
  (gl-load-identity)
  (gl-ortho -1 1 -1 1 -1 +1))

(define (on-display)
  (pk 'on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (gl-color 0 1 0)
  (glut-wire-cube 0.5)
  (swap-buffers))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(define main-window (make-window "ssim"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(glut-main-loop)
