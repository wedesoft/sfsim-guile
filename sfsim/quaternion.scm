(define-module (sfsim quaternion)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (sfsim linear-algebra)
  #:export (<quaternion>
            make-quaternion jmag-part kmag-part quaternion-rotation quaternion-norm
            quaternion-normalize quaternion-conjugate vector->quaternion quaternion->vector
            rotate-vector rotate-matrix rotation-matrix sinc)
  #:re-export (real-part imag-part exp))


(define-class <quaternion> ()
  (real-part #:init-keyword #:real-part #:getter real-part)
  (imag-part #:init-keyword #:imag-part #:getter imag-part)
  (jmag-part #:init-keyword #:jmag-part #:getter jmag-part)
  (kmag-part #:init-keyword #:kmag-part #:getter kmag-part))

(define (make-quaternion2 a b c d)
  (make <quaternion> #:real-part a #:imag-part b #:jmag-part c #:kmag-part d))

(define* (make-quaternion #:optional (a 0) (b 0) (c 0) (d 0))
  "Construct a quaternion"
  (if (and (zero? c) (zero? d))
    (make-rectangular a b)
    (make-quaternion2 a b c d)))

(define-method (jmag-part (self <number>)) 0)
(define-method (kmag-part (self <number>)) 0)

(define (components self)
  (map (cut <> self) (list real-part imag-part jmag-part kmag-part)))

(define-method (write (self <quaternion>) port)
  (apply format port "~f~@fi~@fj~@fk" (components self)))

(define-syntax-rule (quaternion-binary-op op)
  (begin
    (define-method (op (a <real>      ) (b <quaternion>)) (op (make-quaternion2 a 0 0 0) b))
    (define-method (op (a <quaternion>) (b <real>      )) (op a (make-quaternion2 b 0 0 0)))
    (define-method (op (a <complex>   ) (b <quaternion>)) (op (make-quaternion2 (real-part a) (imag-part a) 0 0) b))
    (define-method (op (a <quaternion>) (b <complex>   )) (op a (make-quaternion2 (real-part b) (imag-part b) 0 0)))))

(define-method (equal? (a <quaternion>) (b <quaternion>))
  "Compare quaternions"
  (and-map (cut apply equal? <>) (map list (components a) (components b))))

(define-method (= (a <quaternion>) (b <quaternion>))
  "Numerical equality of quaternions"
  (and-map (cut apply = <>) (map list (components a) (components b))))
(quaternion-binary-op =)

(define-method (- (a <quaternion>))
  "Unary negation of quaternion"
  (apply make-quaternion (map - (components a) )))

(define-method (+ (a <quaternion>) (b <quaternion>))
  "Add two quaternions"
  (apply make-quaternion (map + (components a) (components b))))
(quaternion-binary-op +)

(define-method (- (a <quaternion>) (b <quaternion>))
  "Subtract a quaternion from another"
  (apply make-quaternion (map - (components a) (components b))))

(define-method (* (a <quaternion>) (b <quaternion>))
  "Multiply two quaternions"
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
(quaternion-binary-op *)

(define (quaternion-norm2 self)
  (let [(v (components self))]
    (inner-product v v)))

(define (quaternion-norm self)
  "Compute norm of quaternion"
  (norm (components self)))

(define (quaternion-normalize self)
  "Normalize the quaternion"
  (* (/ 1 (quaternion-norm self)) self))

(define (quaternion-conjugate2 self)
  (make-quaternion (real-part self) (- (imag-part self)) (- (jmag-part self)) (- (kmag-part self))))

(define (quaternion-conjugate self)
  "Determine multiplicative inverse of quaternion"
  (* (quaternion-conjugate2 self) (/ 1 (quaternion-norm2 self))))

(define (vector->quaternion vec)
  "Convert a vector to a quaternion in order to rotate it"
  (apply make-quaternion 0 vec))

(define (quaternion->vector self)
  "Convert a rotation result back to a vector"
  (cdr (components self)))

(define (rotate-vector self vec)
  "Perform vector rotation with rotation represented by a quaternion"
  (quaternion->vector (* self (vector->quaternion vec) (quaternion-conjugate self))))

(define (rotate-matrix self mat)
  "Rotate a matrix using quaternion rotation"
  (let [(rot (rotation-matrix self))]
    (dot (dot rot mat) (transpose rot))))

(define (rotation-matrix self)
  "Convert rotation quaternion to a rotation matrix"
  (transpose (map (cut rotate-vector self <>) '((1 0 0) (0 1 0) (0 0 1)))))

(define (sinc x)
  "Compute sin(x) divided by x"
  (if (zero? x) 1 (/ (sin x) x)))

(define-method (exp (self <quaternion>))
  (let* [(scale         (exp (real-part self)))
         (axis          (quaternion->vector self))
         (rotation      (norm axis))
         (cos-rotation  (cos rotation))
         (sinc-rotation (sinc rotation))]
    (apply make-quaternion (* scale cos-rotation) (* scale sinc-rotation axis))))

(define (quaternion-rotation theta axis)
  "Use quaternion to reprsent a rotation"
  (exp (vector->quaternion (* (/ theta 2) axis))))
