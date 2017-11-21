(define-module (ssimlib quaternion)
  #:use-module (ice-9 optargs)
  #:export (make-quaternion))


(define* (make-quaternion #:optional (a 0) (b 0) (c 0) (d 0)) (list a b c d))
