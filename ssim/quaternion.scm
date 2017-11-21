(define-module (ssim quaternion)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:export (<quaternion>
            make-quaternion jmag-part kmag-part)
  #:re-export (real-part imag-part))


(define-class <quaternion> ()
  (real-part #:init-keyword #:real-part #:getter real-part)
  (imag-part #:init-keyword #:imag-part #:getter imag-part)
  (jmag-part #:init-keyword #:jmag-part #:getter jmag-part)
  (kmag-part #:init-keyword #:kmag-part #:getter kmag-part))

(define* (make-quaternion #:optional (a 0) (b 0) (c 0) (d 0))
  (if (and (zero? c) (zero? d))
    (if (zero? b) a (make-rectangular a b))
    (make <quaternion> #:real-part a #:imag-part b #:jmag-part c #:kmag-part d)))

(define-method (jmag-part (self <number>)) 0)
(define-method (kmag-part (self <number>)) 0)

(define-method (write (self <quaternion>) port)
  (format port "~f~@fi~@fj~@fk" (real-part self) (imag-part self) (jmag-part self) (kmag-part self)))

(define-method (equal? (a <quaternion>) (b <quaternion>))
  (and (eqv? (real-part a) (real-part b))
       (eqv? (imag-part a) (imag-part b))
       (eqv? (jmag-part a) (jmag-part b))
       (eqv? (kmag-part a) (kmag-part b))))
