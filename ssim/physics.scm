(define-module (ssim physics)
  #:export (cuboid-inertia))


(define (sqr x) (* x x))

(define (cuboid-inertia mass width height depth)
  "Determine diagonal elements of a cuboid's inertial matrix"
  (list (* (/ mass 12) (+ (sqr height) (sqr depth )))
        (* (/ mass 12) (+ (sqr width ) (sqr depth )))
        (* (/ mass 12) (+ (sqr width ) (sqr height)))))
