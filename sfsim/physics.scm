(define-module (sfsim physics)
  #:use-module (oop goops)
  #:use-module (ice-9 curried-definitions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (sfsim util)
  #:use-module (sfsim linear-algebra)
  #:use-module (sfsim quaternion)
  #:export (<spring> <state> <lander>
            clock elapsed cuboid-inertia runge-kutta inertia-body angular-velocity
            particle-position particle-positions particle-speed deflect support-point center-of-gravity
            closest-simplex-points gjk-algorithm collision-impulse
            make-spring position speed spring-change apply-linear-impulse apply-rotational-impulse
            make-state orientation linear-momentum angular-momentum state-change collision
            make-lander state gears lander-change)
  #:re-export (+ *))


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

(define-method (particle-speed mass inertia center orientation linear-momentum angular-momentum particle-pos)
  "Determine speed of a rigid body's particle"
  (+ (* (/ 1 mass) linear-momentum) (cross-product (angular-velocity inertia orientation angular-momentum) (- particle-pos center))))

(define (support-point direction points)
  "Get outermost point of POINTS in given DIRECTION."
  (argmax (cut inner-product direction <>) points))

(define (center-of-gravity points)
  "Compute average of given points"
  (* (reduce + #f points) (/ 1 (length points))))

(define (closest-simplex-points simplex-a simplex-b)
  "Determine closest point pair of two simplices"
  (let* [(observation   (- (car simplex-a) (car simplex-b)))
         (design-matrix (- observation (transpose (- (cdr simplex-a) (cdr simplex-b)))))
         (factors       (least-squares design-matrix observation))]
      (if (and (every positive? factors) (< (reduce + 0 factors) 1))
        (cons (cons (fold + (car simplex-a) (map * factors (map (cut - <> (car simplex-a)) (cdr simplex-a))))
                    (fold + (car simplex-b) (map * factors (map (cut - <> (car simplex-b)) (cdr simplex-b)))))
              (cons simplex-a simplex-b))
        (argmin (lambda (result) (norm (- (caar result) (cdar result))))
                (map closest-simplex-points (leave-one-out simplex-a) (leave-one-out simplex-b))))))

(define (gjk-algorithm convex-a convex-b)
  "Get pair of closest points of two convex hulls each defined by a set of points"
  (let [(simplex-a '())
        (simplex-b '())
        (closest (cons (center-of-gravity convex-a) (center-of-gravity convex-b)))]
    (while #t
      (let* [(direction  (- (car closest) (cdr closest)))
             (candidates (cons (support-point (- direction) convex-a) (support-point direction convex-b)))]
        (if (>= (+ (inner-product direction (- direction)) 1e-9) (inner-product (- (car candidates) (cdr candidates)) (- direction)))
          (break closest))
        (let [(result (closest-simplex-points (cons (car candidates) simplex-a) (cons (cdr candidates) simplex-b)))]
          (set! closest (car result))
          (set! simplex-a (cadr result))
          (set! simplex-b (cddr result)))))))

(define (deflect relative-speed normal loss friction micro-speed)
  "Determine speed change necessary to deflect particle. If the particle is very slow, a lossless micro-collision is computed instead."
  (let* [(normal-speed     (inner-product normal relative-speed))
         (tangential-speed (orthogonal-component normal relative-speed))
         (normal-target    (if (>= normal-speed (- micro-speed)) (- micro-speed normal-speed) (* (- loss 2) normal-speed)))
         (friction-target  (* friction normal-target))]
    (- (* normal-target normal) (* friction-target (normalize tangential-speed)))))

(define (collision-impulse speed-change mass-a mass-b inertia-a inertia-b orientation-a orientation-b radius-a radius-b)
  "Compute impulse of a collision of two objects"
  (let* [(direction (normalize speed-change))
         (impulse   (/ (norm speed-change)
                       (+ (/ 1 mass-a)
                          (/ 1 mass-b)
                          (inner-product direction (cross-product (dot (inverse (inertia-a orientation-a))
                                                                  (cross-product radius-a direction)) radius-a))
                          (inner-product direction (cross-product (dot (inverse (inertia-b orientation-b))
                                                                  (cross-product radius-b direction)) radius-b)))))]
    (* impulse direction)))

(define-class <spring> (<object>)
              (position #:init-keyword #:position #:getter position)
              (speed    #:init-keyword #:speed    #:getter speed   ))

(define (make-spring position speed)
  (make <spring> #:position position #:speed speed))

(define (spring-force spring strength damping)
  "Determine force of spring-damper system depending on its position and speed"
  (- (+ (* (position spring) strength) (* (speed spring) damping))))

(define ((spring-change strength damping mass) spring dt)
  "Derivative of spring-damper system's state"
  (make-spring (speed spring) (/ (spring-force spring strength damping) mass)))

(define-method (* (spring <spring>) (scalar <real>))
  (make-spring (* (position spring) scalar)
               (* (speed spring) scalar)))

(define-method (+ (spring <spring>) (dspring <spring>))
  (make-spring (+ (position spring) (position dspring))
               (+ (speed spring) (speed dspring))))

(define (apply-linear-impulse linear-impulse impulse)
  "Apply speed change"
  (+ linear-impulse impulse))

(define (apply-rotational-impulse momentum radius impulse)
  "Apply angular momentum change "
  (+ momentum (cross-product radius impulse)))

(define-class <state> (<object>)
              (position         #:init-keyword #:position         #:getter position        )
              (linear-momentum  #:init-keyword #:linear-momentum  #:getter linear-momentum )
              (orientation      #:init-keyword #:orientation      #:getter orientation     )
              (angular-momentum #:init-keyword #:angular-momentum #:getter angular-momentum))

(define (make-state position linear-momentum orientation angular-momentum)
  (make <state>
        #:position         position
        #:linear-momentum  linear-momentum
        #:orientation      orientation
        #:angular-momentum angular-momentum))

(define-method (* (state <state>) (scalar <real>))
  (make-state (* (position         state) scalar)
              (* (linear-momentum  state) scalar)
              (* (orientation      state) scalar)
              (* (angular-momentum state) scalar)))

(define-method (+ (state <state>) (dstate <state>))
  (make-state (+ (position         state) (position         dstate))
              (+ (linear-momentum  state) (linear-momentum  dstate))
              (+ (orientation      state) (orientation      dstate))
              (+ (angular-momentum state) (angular-momentum dstate))))

(define-method (particle-position state corner)
  "Rotated and translated position of particle"
  (particle-position (position state) (orientation state) corner))

(define ((particle-positions corners) state)
  (map (cut particle-position state <>) corners))

(define-method (particle-speed mass inertia state particle-pos)
  "Speed of particle taking into account rotation of object"
  (particle-speed mass inertia (position state) (orientation state) (linear-momentum state) (angular-momentum state) particle-pos))

(define ((state-change mass inertia acceleration) state dt)
  (make-state (* (/ 1 mass) (linear-momentum state))
              (* acceleration mass)
              (* (vector->quaternion (* 0.5 (angular-velocity inertia (orientation state) (angular-momentum state))))
                 (orientation state))
              '(0 0 0)))

(define (state-impulse state mass radius impulse)
  "Apply impulse to an object"
  (make-state (position state)
              (apply-linear-impulse (linear-momentum state) impulse)
              (quaternion-normalize (orientation state))
              (apply-rotational-impulse (angular-momentum state) radius impulse)))

(define (collision state-a state-b mass-a mass-b inertia-a inertia-b closest loss friction micro-speed)
  "Simulate a rigid-body collision"
  (let* [(speed-a        (particle-speed mass-a inertia-a state-a (car closest)))
         (speed-b        (particle-speed mass-b inertia-b state-b (cdr closest)))
         (relative-speed (- speed-a speed-b))
         (normal         (normalize (- (car closest) (cdr closest))))
         (speed-delta    (deflect relative-speed normal loss friction micro-speed))
         (radius-a       (- (car closest) (position state-a)))
         (radius-b       (- (cdr closest) (position state-b)))
         (impulse-vector (collision-impulse speed-delta mass-a mass-b inertia-a inertia-b
                                            (orientation state-a) (orientation state-b) radius-a radius-b))]
    (cons (state-impulse state-a mass-a radius-a impulse-vector)
          (state-impulse state-b mass-b radius-b (- impulse-vector)))))

(define-class <lander> (<object>)
              (state #:init-keyword #:state #:getter state)
              (gears #:init-keyword #:gears #:getter gears))

(define (make-lander state . gears)
  (make <lander> #:state state #:gears gears))

(define ((lander-change mass inertia acceleration strength damping gear-mass) self dt)
  (apply make-lander ((state-change mass inertia acceleration) (state self) dt)
                     (map (cut (spring-change strength damping gear-mass) <> dt) (gears self))))

(define-method (* (self <lander>) (scalar <real>))
  (apply make-lander (* (state self) scalar) (map (cut * <> scalar) (gears self))))

(define-method (+ (lander <lander>) (dlander <lander>))
  (apply make-lander (+ (state lander) (state dlander)) (map + (gears lander) (gears dlander))))
