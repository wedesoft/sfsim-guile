(define-module (sfsim state)
  #:use-module (oop goops)
  #:use-module (ice-9 curried-definitions)
  #:use-module (sfsim physics)
  #:use-module (sfsim quaternion)
  #:use-module (sfsim linear-algebra)
  #:export (make-state position speed orientation angular-momentum <state>
            state-change collision)
  #:re-export (+ * particle-position particle-speed))


(define-class <state> (<object>)
              (position         #:init-keyword #:position         #:getter position        )
              (speed            #:init-keyword #:speed            #:getter speed           )
              (orientation      #:init-keyword #:orientation      #:getter orientation     )
              (angular-momentum #:init-keyword #:angular-momentum #:getter angular-momentum))

(define (make-state position speed orientation angular-momentum)
  (make <state>
        #:position         position
        #:speed            speed
        #:orientation      orientation
        #:angular-momentum angular-momentum))

(define-method (* (state <state>) (scalar <real>))
  (make-state (* (position         state) scalar)
              (* (speed            state) scalar)
              (* (orientation      state) scalar)
              (* (angular-momentum state) scalar)))

(define-method (+ (state <state>) (dstate <state>))
  (make-state (+ (position         state) (position         dstate))
              (+ (speed            state) (speed            dstate))
              (+ (orientation      state) (orientation      dstate))
              (+ (angular-momentum state) (angular-momentum dstate))))

(define-method (particle-position state corner)
  "Rotated and translated position of particle"
  (particle-position (position state) (orientation state) corner))

(define-method (particle-speed inertia state particle-pos)
  "Speed of particle taking into account rotation of object"
  (particle-speed inertia (position state) (orientation state) (speed state) (angular-momentum state) particle-pos))

(define ((state-change inertia acceleration) state dt)
  (make-state (speed state)
              acceleration
              (* (vector->quaternion (* 0.5 (angular-velocity inertia (orientation state) (angular-momentum state))))
                 (orientation state))
              '(0 0 0)))

(define (collision state-a state-b mass-a mass-b inertia-a inertia-b closest loss friction micro-speed)
  "Simulate a rigid-body collision"
  (let* [(speed-a        (particle-speed inertia-a state-a (car closest)))
         (speed-b        (particle-speed inertia-b state-b (cdr closest)))
         (relative-speed (- speed-a speed-b))
         (normal         (normalize (- (car closest) (cdr closest))))
         (speed-delta    (deflect relative-speed normal loss friction micro-speed))
         (radius-a       (- (car closest) (position state-a)))
         (radius-b       (- (cdr closest) (position state-b)))
         (impulse-vector (collision-impulse speed-delta mass-a mass-b inertia-a inertia-b
                                            (orientation state-a) (orientation state-b) radius-a radius-b))]
    (cons (make-state (position state-a)
                      (+ (speed state-a) (* (/ 1 mass-a) impulse-vector))
                      (quaternion-normalize (orientation state-a))
                      (+ (angular-momentum state-a) (cross-product radius-a impulse-vector)))
          (make-state (position state-b)
                      (- (speed state-b) (* (/ 1 mass-b) impulse-vector))
                      (quaternion-normalize (orientation state-b))
                      (- (angular-momentum state-b) (cross-product radius-b impulse-vector))))))
