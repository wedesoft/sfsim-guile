(define-module (sfsim physics)
  #:use-module (oop goops)
  #:use-module (ice-9 curried-definitions)
  #:use-module (srfi srfi-19)
  #:use-module (sfsim linear-algebra)
  #:use-module (sfsim quaternion)
  #:export (clock elapsed cuboid-inertia runge-kutta inertia-body angular-velocity
            particle-position particle-speed deflect))


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
    (+ y0 (* (+ k1 (* k2 2) (* k3 2) k4) (/ dt 6)))))

(define (cuboid-inertia mass width height depth)
  "Determine diagonal elements of a cuboid's inertial matrix"
  (diagonal (list (* (/ mass 12) (+ (expt height 2) (expt depth  2)))
                  (* (/ mass 12) (+ (expt width  2) (expt depth  2)))
                  (* (/ mass 12) (+ (expt width  2) (expt height 2))))))

(define ((inertia-body mat) orientation)
  "Rotate inertia matrix MAT into world frame"
  (rotate-matrix orientation mat))

(define (angular-velocity inertia orientation angular-momentum)
  "Angular velocity determined using the angular momentum and the (rotated) inertia tensor"
  (dot (inverse (inertia orientation)) angular-momentum))

(define-method (particle-position center orientation radius-vector)
  "Determine position of a rigid body's particle"
  (+ center (rotate-vector orientation radius-vector)))

(define-method (particle-speed inertia orientation body-velocity angular-momentum radius-vector)
  "Determine speed of a rigid body's particle"
  (+ body-velocity
     (cross-product (angular-velocity inertia orientation angular-momentum)
                    (rotate-vector orientation radius-vector))))

(define (deflect relative-speed normal loss friction micro-speed)
  (let* [(normal-speed     (inner-product normal relative-speed))
         (tangential-speed (orthogonal-component normal relative-speed))
         (normal-target    (if (>= normal-speed (- micro-speed)) (- micro-speed normal-speed) (* (- loss 2) normal-speed)))
         (friction-target  (* friction normal-target))]
    (- (* normal-target normal) (* friction-target (normalize tangential-speed)))))
