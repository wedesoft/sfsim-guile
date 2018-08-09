(use-modules (srfi srfi-64)
             (sfsim linear-algebra)
             (sfsim physics)
             (sfsim quaternion)
             (sfsim state))


(test-begin "sfsim state")

(define s (make-state '(2 3 5) '(0.2 0.3 0.5) 1.0 '(0.1 0.2 0.3)))
(test-group "state struct"
  (test-equal "position of state"
    '(2 3 5) (position s))
  (test-equal "position of state"
    '(0.2 0.3 0.5) (speed s))
  (test-equal "quaternion orientation of state"
    1.0 (orientation s))
  (test-equal "angular momentum of state"
    '(0.1 0.2 0.3) (angular-momentum s)))

(test-group "scalar multiplication"
  (test-equal "multiply position"
    '(4 6 10) (position (* s 2)))
  (test-equal "multiply speed"
    '(0.4 0.6 1.0) (speed (* s 2)))
  (test-equal "multiply orientation"
    2.0 (orientation (* s 2)))
  (test-equal "multiply angular momentum"
    '(0.2 0.4 0.6) (angular-momentum (* s 2))))

(define s2 (make-state '(3 5 7) '(0.3 0.5 0.7) 0.1 '(0.5 0.3 0.4)))
(test-group "add states"
  (test-equal "add position"
    '(5 8 12) (position (+ s s2)))
  (test-equal "add speed"
    '(0.5 0.8 1.2) (speed (+ s s2)))
  (test-equal "add orientation"
    1.1 (orientation (+ s s2)))
  (test-equal "angular momentum"
    '(0.6 0.5 0.7) (angular-momentum (+ s s2))))

(define s1 (make-state '(2 3 5) '(0 0 0) 1.0 '(0 0 0)))
(define s2 (make-state '(0 0 0) '(0 0 0) 1.0 '(1 0 0)))
(define inertia (inertia-body '((1 0 0) (0 1 0) (0 0 1))))
(test-group "particle states"
  (test-equal "position of particle"
    '(3.0 5.0 8.0) (particle-position s1 '(1 2 3)))
  (test-equal "speed of particle"
    '(0.0 0.0 1.0) (particle-speed inertia s2 '(0 1 0))))

(define s (make-state '(2 3 5) '(0.2 0.3 0.5) 1.0 '(0.1 0.2 0.3)))
(define s2 (make-state '(2 3 5) '(0.2 0.3 0.5) -1.0 '(0.1 0.2 0.3)))
(define inertia (inertia-body '((1 0 0) (0 1 0) (0 0 1))))
(define inertia2 (inertia-body '((0.5 0 0) (0 0.5 0) (0 0 0.5))))
(define acceleration '(0 -10 0))
(define torque '(0 -10 0))
(test-group "time derivative of state"
  (test-equal "derivative of position is speed"
    '(0.2 0.3 0.5) (position ((state-change inertia acceleration) s 0)))
  (test-equal "derivative of speed is acceleration"
    acceleration (speed ((state-change inertia acceleration) s 0)))
  (test-equal "derivative of orientation requires computation of angular speed"
    0.05 (imag-part (orientation ((state-change inertia acceleration) s 0))))
  (test-equal "derivative of orientation takes into account inertia"
    0.1 (imag-part (orientation ((state-change inertia2 acceleration) s 0))))
  (test-equal "derivative of orientation takes into account orientation"
    -0.05 (imag-part (orientation ((state-change inertia acceleration) s2 0))))
  (test-equal "derivative of rotational impulse is zero"
    '(0 0 0) (angular-momentum ((state-change inertia acceleration) s 0))))

(test-end "sfsim state")