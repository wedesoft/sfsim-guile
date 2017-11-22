(define-module (ssim quaternion)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:export (<quaternion>
            make-quaternion jmag-part kmag-part quaternion-rotation)
  #:re-export (real-part imag-part))


(define-class <quaternion> ()
  (real-part #:init-keyword #:real-part #:getter real-part)
  (imag-part #:init-keyword #:imag-part #:getter imag-part)
  (jmag-part #:init-keyword #:jmag-part #:getter jmag-part)
  (kmag-part #:init-keyword #:kmag-part #:getter kmag-part))

(define (make-quaternion2 a b c d)
  (make <quaternion> #:real-part a #:imag-part b #:jmag-part c #:kmag-part d))

(define* (make-quaternion #:optional (a 0) (b 0) (c 0) (d 0))
  (if (and (zero? c) (zero? d))
    (if (zero? b) a (make-rectangular a b))
    (make-quaternion2 a b c d)))

(define-method (jmag-part (self <number>)) 0)
(define-method (kmag-part (self <number>)) 0)

(define-method (write (self <quaternion>) port)
  (format port "~f~@fi~@fj~@fk" (real-part self) (imag-part self) (jmag-part self) (kmag-part self)))

(define-method (equal? (a <quaternion>) (b <quaternion>))
  (and (eqv? (real-part a) (real-part b))
       (eqv? (imag-part a) (imag-part b))
       (eqv? (jmag-part a) (jmag-part b))
       (eqv? (kmag-part a) (kmag-part b))))

(define-syntax-rule (quaternion-binary-op op)
  (begin
    (define-method (op (a <real>) (b <quaternion>)) (op (make-quaternion2 a 0 0 0) b))
    (define-method (op (a <quaternion>) (b <real>)) (op a (make-quaternion2 b 0 0 0)))
    (define-method (op (a <complex>) (b <quaternion>)) (op (make-quaternion2 (real-part a) (imag-part a) 0 0) b))
    (define-method (op (a <quaternion>) (b <complex>)) (op a (make-quaternion2 (real-part b) (imag-part b) 0 0)))))

(define-method (- (a <quaternion>))
  (make-quaternion (- (real-part a)) (- (imag-part a)) (- (jmag-part a)) (- (kmag-part a))))

(define-method (+ (a <quaternion>) (b <quaternion>))
  (make-quaternion (+ (real-part a) (real-part b))
                   (+ (imag-part a) (imag-part b))
                   (+ (jmag-part a) (jmag-part b))
                   (+ (kmag-part a) (kmag-part b))))
(quaternion-binary-op +)

(define-method (- (a <quaternion>) (b <quaternion>))
  (make-quaternion (- (real-part a) (real-part b))
                   (- (imag-part a) (imag-part b))
                   (- (jmag-part a) (jmag-part b))
                   (- (kmag-part a) (kmag-part b))))

(define-method (* (a <quaternion>) (b <quaternion>))
  (make-quaternion
    (- (* (real-part a) (real-part b))
       (* (imag-part a) (imag-part b))
       (* (jmag-part a) (jmag-part b))
       (* (kmag-part a) (kmag-part b)))
    (- (+ (* (real-part a) (imag-part b))
          (* (imag-part a) (real-part b))
          (* (jmag-part a) (kmag-part b)))
       (* (kmag-part a) (jmag-part b)))
    (+ (- (* (real-part a) (jmag-part b))
          (* (imag-part a) (kmag-part b)))
       (* (jmag-part a) (real-part b))
       (* (kmag-part a) (imag-part b)))
    (+ (- (+ (* (real-part a) (kmag-part b))
             (* (imag-part a) (jmag-part b)))
          (* (jmag-part a) (imag-part b)))
       (* (kmag-part a) (real-part b)))))

(define (quaternion-rotation theta axis)
  (cos (/ theta 2)))

(quaternion-binary-op *)
