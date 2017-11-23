(define-module (ssim physics)
  #:use-module (srfi srfi-19)
  #:export (clock elapsed cuboid-inertia runge-kutta))


(define (clock)
  "Get current time with high precision"
  (current-time))

(define* (elapsed reference #:optional (reset #f))
  "Return time elapsed and optionally reset the clock"
  (let [(difference (time-difference (current-time) reference))]
    (if reset (add-duration! reference difference))
    (+ (time-second difference) (* 1e-9 (time-nanosecond difference)))))

(define (runge-kutta y0 dt dy)
  "4th order Runge-Kutta method"
  (let* [(dt2 (/ dt 2))
         (k1  (dy y0                0  ))
         (k2  (dy (+ y0 (* k1 dt2)) dt2))
         (k3  (dy (+ y0 (* k2 dt2)) dt2))
         (k4  (dy (+ y0 (* k3 dt )) dt ))]
    (+ y0 (* (/ dt 6) (+ k1 (* 2 k2) (* 2 k3) k4)))))

(define (sqr x) (* x x))

(define (cuboid-inertia mass width height depth)
  "Determine diagonal elements of a cuboid's inertial matrix"
  (list (* (/ mass 12) (+ (sqr height) (sqr depth )))
        (* (/ mass 12) (+ (sqr width ) (sqr depth )))
        (* (/ mass 12) (+ (sqr width ) (sqr height)))))
