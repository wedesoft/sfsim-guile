#!/usr/bin/env guile
!#
(use-modules (glut) (gl))


(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (swap-buffers))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(define main-window (make-window "ssim"))
(set-display-callback on-display)
(glut-main-loop)
