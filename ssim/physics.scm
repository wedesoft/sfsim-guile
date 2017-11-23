(define-module (ssim physics)
  #:use-module (srfi srfi-19)
  #:export (clock elapsed cuboid-inertia))


(define (clock)
  "Get current time with high precision"
  (current-time))

(define* (elapsed reference #:optional (reset #f))
  "Return time elapsed and optionally reset the clock"
  (let [(difference (time-difference (current-time) reference))]
    (if reset (add-duration! reference difference))
    (+ (time-second difference) (* 1e-9 (time-nanosecond difference)))))

(define (sqr x) (* x x))

(define (cuboid-inertia mass width height depth)
  "Determine diagonal elements of a cuboid's inertial matrix"
  (list (* (/ mass 12) (+ (sqr height) (sqr depth )))
        (* (/ mass 12) (+ (sqr width ) (sqr depth )))
        (* (/ mass 12) (+ (sqr width ) (sqr height)))))
