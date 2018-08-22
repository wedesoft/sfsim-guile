(define-module (sfsim state)
  #:use-module (oop goops)
  #:use-module (ice-9 curried-definitions)
  #:use-module (sfsim physics)
  #:use-module (sfsim quaternion)
  #:use-module (sfsim linear-algebra)
  #:export (make-state orientation linear-momentum angular-momentum <state>
            state-change collision)
  #:re-export (+ * particle-position particle-speed position speed))


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
