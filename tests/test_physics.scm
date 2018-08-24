(use-modules (srfi srfi-64)
             (sfsim physics)
             (sfsim quaternion))


(define pi (* 2 (acos 0)))


(test-begin "sfsim physics")

(test-group "clock"
  (test-assert "Clock starts with zero"
    (let [(t (clock))] (>= (elapsed t) 0)))
  (test-assert "Check clock has advanced after 1 second"
    (let [(t (clock))] (sleep 1) (>= (elapsed t) 1)))
  (test-assert "Check clock has advanced after 100 milliseconds"
    (let [(t (clock))] (usleep 100000) (>= (elapsed t) 0.1)))
  (test-assert "Check clock has not advanced too much after 100 milliseconds"
    (let [(t (clock))] (usleep 100000) (< (elapsed t) 0.2)))
  (test-assert "Check time with reset"
    (let [(t (clock))] (sleep 1) (elapsed t #t) (< (elapsed t) 1)))
  (test-assert "Check time without reset"
    (let [(t (clock))] (sleep 1) (elapsed t) (>= (elapsed t) 1))))

(test-group "Runge-Kutta method"
  (test-equal "Constant function"
    42 (runge-kutta 42 1 (lambda (y dt) 0)))
  (test-equal "Linear function"
    47 (runge-kutta 42 1 (lambda (y dt) 5)))
  (test-equal "Different time step"
    52 (runge-kutta 42 2 (lambda (y dt) 5)))
  (test-equal "Quadratic function"
    43 (runge-kutta 42 1 (lambda (y dt) (* 2 dt))))
  (test-equal "Quadratic function with different time step"
    46 (runge-kutta 42 2 (lambda (y dt) (* 2 dt))))
  (test-equal "Quadratic function with different time step"
    43 (runge-kutta 42 1 (lambda (y dt) (* 3 dt dt))))
  (test-approximate "Approximate exponential growth"
    (exp 1) (runge-kutta 1 1 (lambda (y dt) y)) 1e-2))

(test-group "inertia of cuboid"
  (test-equal "Inertia of unit cube"
    '((1 0 0) (0 1 0) (0 0 1)) (cuboid-inertia 6 1 1 1))
  (test-equal "Inertia of heavier unit cube"
    '((2 0 0) (0 2 0) (0 0 2)) (cuboid-inertia 12 1 1 1))
  (test-equal "Inertia of cuboid"
    '((34 0 0) (0 29 0) (0 0 13)) (cuboid-inertia 12 2 3 5)))

(define (round-vector v) (map (compose inexact->exact round) v))
(define (round-matrix m) (map round-vector m))

(test-group "inertia"
  (let [(inertia (inertia-body '((3 2 1) (2 4 2) (1 2 5))))]
    (test-equal "Use specified inertia matrix"
      '((3 2 1) (2 4 2) (1 2 5)) (round-matrix (inertia (quaternion-rotation 0 '(1 0 0)))))
    (test-equal "Apply specified rotation to inertia matrix"
      '((3 -1 2) (-1 5 -2) (2 -2 4)) (round-matrix (inertia (quaternion-rotation (/ pi 2) '(1 0 0)))))))

(test-group "angular speed"
  (let [(inertia (inertia-body '((1 0 0) (0 2 0) (0 0 4))))]
    (test-equal "Angular speed should be zero if angular momentum is zero"
      '(0.0 0.0 0.0) (angular-velocity inertia (quaternion-rotation 0 '(1 0 0)) '(0 0 0)))
    (test-equal "Scale angular momentum using inverse inertia matrix"
      '(2.0 2.0 1.5) (angular-velocity inertia (quaternion-rotation 0 '(1 0 0)) '(2 4 6)))
    (test-equal "Scale angular momentum using inverse of rotated inertia matrix"
      '(2 1 3) (round-vector (angular-velocity inertia (quaternion-rotation (/ pi 2) '(1 0 0)) '(2 3 6))))))

(test-group "position of a particle"
  (test-equal "Position of center particle equals body's center of gravity"
    '(2 3 5) (particle-position '(2 3 5) (quaternion-rotation 0 '(1 0 0)) '(0 0 0)))
  (test-equal "Position of particle of an object which is aligned with the world coordinate system"
    '(3.0 5.0 8.0) (particle-position '(2 3 5) (quaternion-rotation 0 '(1 0 0)) '(1 2 3)))
  (test-equal "Position of particle of a rotated object"
    '(3.0 1.0 2.0) (particle-position '(2 3 5) (quaternion-rotation pi '(1 0 0)) '(1 2 3))))

(test-group "speed of a particle"
  (let [(inertia (inertia-body '((1 0 0) (0 1 0) (0 0 1))))]
    (test-equal "Speed of particle on non-rotating object"
      '(1.0 2.0 3.0) (particle-speed 1 inertia '(0 0 0) (quaternion-rotation 0 '(1 0 0)) '(1 2 3) '(0 0 0) '(0 1 0)))
    (test-equal "Speed of particle on heavier object"
      '(0.1 0.2 0.3) (particle-speed 10 inertia '(0 0 0) (quaternion-rotation 0 '(1 0 0)) '(1 2 3) '(0 0 0) '(0 1 0)))
    (test-equal "Speed of particle on rotating object"
      '(0.0 0.0 1.0) (particle-speed 1 inertia '(0 0 0) (quaternion-rotation 0 '(1 0 0)) '(0 0 0) '(1 0 0) '(0 1 0)))
    (test-equal "Direction of speed vector depends on center of object"
      '(0 0 2) (round-vector (particle-speed 1 inertia '(0 -1 0) (quaternion-rotation 0 '(1 0 0)) '(0 0 0) '(1 0 0) '(0 1 0)))))
  (let [(inertia (inertia-body '((2 0 0) (0 0.5 0) (0 0 0.5))))]
    (test-equal "Heavier object rotates more slowly"
      '(0.0 0.0 0.5) (particle-speed 1 inertia '(0 0 0) (quaternion-rotation 0 '(1 0 0)) '(0 0 0) '(1 0 0) '(0 1 0)))
    (test-equal "Speed of rotation depends on orientation of inertia tensor"
      '(0 0 2) (round-vector (particle-speed 1 inertia '(0 0 0) (quaternion-rotation (/ pi 2) '(0 1 0)) '(0 0 0) '(1 0 0) '(0 1 0))))))

(test-group "support point"
  (test-equal "First point is outermost point in given direction"
    '(2 3 5) (support-point '(-1 0 0) '((2 3 5) (3 5 7))))
  (test-equal "Last point is outermost point in given direction"
    '(3 5 7) (support-point '(1 0 0) '((2 3 5) (3 5 7)))))

(test-group "center of gravity"
  (test-equal "Center of one point is point"
    '(2 3 5) (center-of-gravity '((2 3 5))))
  (test-equal "Center of two points is average of points"
    '(5/2 4 6) (center-of-gravity '((2 3 5) (3 5 7)))))

(test-group "closest point pair and simplex pair"
  (test-equal "Closest point pair of zero-dimensional simplex is trivial"
    '((2 3 5) . (3 5 7)) (car (closest-simplex-points '((2 3 5)) '((3 5 7)))))
  (test-equal "Return zero-dimensional simplex"
    '(((2 3 5)) . ((3 5 7))) (cdr (closest-simplex-points '((2 3 5)) '((3 5 7)))))
  (test-equal "Closest point on a line"
    '((0 0 1) . (0 0 -1)) (car (closest-simplex-points '((-1 0 1) (1 0 1)) '((0 0 -1) (0 0 -1)))))
  (test-equal "Return simplex"
    '(((-1 0 1) (1 0 1)) . ((0 -1 -1) (0 1 -1)))
    (cdr (closest-simplex-points '((-1 0 1) (1 0 1)) '((0 -1 -1) (0 1 -1)))))
  (test-equal "Closest point of shifted line in first argument"
    '((0 0 1) . (0 0 -1)) (car (closest-simplex-points '((-1 0 1) (3 0 1)) '((0 0 -1) (0 0 -1)))))
  (test-equal "Closest point of shifted line in second argument"
    '((0 0 1) . (0 0 -1)) (car (closest-simplex-points '((0 0 1) (0 0 1)) '((-1 0 -1) (3 0 -1)))))
  (test-equal "Limit closest point to starting point of line segment"
    '((2 0 1) . (0 0 -1)) (car (closest-simplex-points '((2 0 1) (3 0 1)) '((0 0 -1) (0 0 -1)))))
  (test-equal "Limit closest point to ending point of line segment"
    '((-2 0 1) . (0 0 -1)) (car (closest-simplex-points '((-3 0 1) (-2 0 1)) '((0 0 -1) (0 0 -1))))))

(test-group "Gilbert-Johnson-Keerthi algorithm"
  (test-equal "Two points are the closest two points"
    '((-1 0 0) . (1 0 0)) (gjk-algorithm '((-1 0 0)) '((1 0 0))))
  (test-equal "Closest points of two lines"
    '((0 0 1) . (0 0 -1)) (gjk-algorithm '((-1 0 1) (1 0 1)) '((0 -1 -1) (0 1 -1))))
  (test-equal "Closest point of triangle and point"
    '((0 0 1) . (0 0 -1)) (gjk-algorithm '((2 -1 1) (-3 -1 1) (0 4 1)) '((0 0 -1))))
  (test-assert "No infinite loop when handling floating-point inaccuracies"
    (gjk-algorithm '((2.0 -1.0 1.0) (-3.0 -1.0 1.0) (0.0 4.0 1.0)) '((0.0 0.0 -1.0)))))

(test-group "deflection of particle"
  (test-equal "Zero speed"
    '(0 0 0) (deflect '(0 0 0) '(1 0 0) 0 0 0))
  (test-equal "Inverting speed vector"
    '(6 0 0) (deflect '(-3 0 0) '(1 0 0) 0 0 0))
  (test-equal "Lossless reflection of speed"
    '(6 0 0) (deflect '(-3 5 0) '(1 0 0) 0 0 0))
  (test-equal "Reflect speed with loss"
    '(4.5 0.0 0.0) (deflect '(-3 5 0) '(1 0 0) 0.5 0 0))
  (test-equal "Reflect speed with friction"
    '(6.0 -3.0 -0.0) (deflect '(-3 5 0) '(1 0 0) 0 0.5 0))
  (test-equal "Lossless micro-collision"
    '(0.325 0.0 0.0) (deflect '(-0.125 0 0) '(1 0 0) 0.5 0.5 0.2)))

(define inertia (inertia-body '((1 0 0) (0 1 0) (0 0 1))))
(test-group "impulse acting on particle"
  (test-equal "Collision with planetary object"
    '(1.0 0.0 0.0) (collision-impulse '(1.0 0.0 0.0) 1.0 5.9742e+24 inertia inertia 1 1 '(1 0 0) '(-1 0 0)))
  (test-equal "Collision of heavier object"
    '(10.0 0.0 0.0) (collision-impulse '(1.0 0.0 0.0) 10.0 5.9742e+24 inertia inertia 1 1 '(1 0 0) '(-1 0 0)))
  (test-equal "Collision of objects of equal mass"
    '(0.5 0.0 0.0) (collision-impulse '(1.0 0.0 0.0) 1.0 1.0 inertia inertia 1 1 '(1 0 0) '(-1 0 0)))
  (test-equal "Rotational component in first object"
    '(0.5 0.0 0.0) (collision-impulse '(1.0 0.0 0.0) 1.0 5.9742e+24 inertia inertia 1 1 '(0 1 0) '(-1 0 0)))
  (test-equal "Rotational component in second object"
    '(0.5 0.0 0.0) (collision-impulse '(1.0 0.0 0.0) 5.9742e+24 1.0 inertia inertia 1 1 '(1 0 0) '(0 -1 0))))

(test-group "spring"
  (test-equal "elongation of spring"
    3 (position (make-spring 3 2)))
  (test-equal "speed of spring"
    2 (speed (make-spring 3 2)))
  (test-equal "position change of spring is speed"
    2 (position ((spring-change 12 6 2) (make-spring 3 2) 0)))
  (test-equal "speed change depends on elongation of spring"
    -30 (speed ((spring-change 12 6 2) (make-spring 5 0) 0)))
  (test-equal "speed change depends on damping of spring"
    -3 (speed ((spring-change 12 6 2) (make-spring 0 1) 0))))

(test-group "apply impulses"
  (test-equal "applying an impulse increases momentum"
    11 (apply-linear-impulse 5 6))
  (test-equal "applying an impulse to an momentum vector"
    '(11) (apply-linear-impulse '(5) '(6)))
  (test-equal "rotational impulse with no effect"
    '(2 3 5) (apply-rotational-impulse '(2 3 5) '(1 0 0) '(1 0 0)))
  (test-equal "rotational impulse with lever"
    '(2 3 6) (apply-rotational-impulse '(2 3 5) '(1 0 0) '(0 1 0))))

(define s (make-spring 2 3))
(test-group "scalar multiplication for spring"
  (test-equal "multiply position"
    10 (position (* s 5)))
  (test-equal "multiply speed"
    15 (speed (* s 5))))

(define s2 (make-spring 5 7))
(test-group "add springs"
  (test-equal "add positions"
    7 (position (+ s s2)))
  (test-equal "add speeds"
    10 (speed (+ s s2))))

(define s (make-state '(2 3 5) '(0.2 0.3 0.5) 1.0 '(0.1 0.2 0.3)))
(test-group "state struct"
  (test-equal "position of state"
    '(2 3 5) (position s))
  (test-equal "linear momentum of state"
    '(0.2 0.3 0.5) (linear-momentum s))
  (test-equal "quaternion orientation of state"
    1.0 (orientation s))
  (test-equal "angular momentum of state"
    '(0.1 0.2 0.3) (angular-momentum s)))

(test-group "scalar multiplication for state"
  (test-equal "multiply position"
    '(4 6 10) (position (* s 2)))
  (test-equal "multiply linear momentum"
    '(0.4 0.6 1.0) (linear-momentum (* s 2)))
  (test-equal "multiply orientation"
    2.0 (orientation (* s 2)))
  (test-equal "multiply angular momentum"
    '(0.2 0.4 0.6) (angular-momentum (* s 2))))

(define s2 (make-state '(3 5 7) '(0.3 0.5 0.7) 0.1 '(0.5 0.3 0.4)))
(test-group "add states"
  (test-equal "add position"
    '(5 8 12) (position (+ s s2)))
  (test-equal "add linear-momentum"
    '(0.5 0.8 1.2) (linear-momentum (+ s s2)))
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
    '(0.0 0.0 1.0) (particle-speed 1 inertia s2 '(0 1 0)))
  (test-equal "position of multiple particles"
    '((3.0 5.0 8.0) (4.0 6.0 10.0)) ((particle-positions '((1 2 3) (2 3 5))) s1)))

(define s (make-state '(2 3 5) '(0.2 0.3 0.5) 1.0 '(0.1 0.2 0.3)))
(define s2 (make-state '(2 3 5) '(0.2 0.3 0.5) -1.0 '(0.1 0.2 0.3)))
(define inertia (inertia-body '((1 0 0) (0 1 0) (0 0 1))))
(define inertia2 (inertia-body '((0.5 0 0) (0 0.5 0) (0 0 0.5))))
(define force_ '(0 -10 0))
(define torque '(0 -10 0))
(define moment '(2 3 5))
(test-group "time derivative of state"
  (test-equal "derivative of position is linear momentum divided by mass"
    '(0.1 0.15 0.25) (position ((state-change 2 inertia force_ moment) s 0)))
  (test-equal "derivative of linear momentum is force"
    force_ (linear-momentum ((state-change 2 inertia force_ moment) s 0)))
  (test-equal "derivative of orientation requires computation of angular speed"
    0.05 (imag-part (orientation ((state-change 2 inertia force_ moment) s 0))))
  (test-equal "derivative of orientation takes into account inertia"
    0.1 (imag-part (orientation ((state-change 2 inertia2 force_ moment) s 0))))
  (test-equal "derivative of orientation takes into account orientation"
    -0.05 (imag-part (orientation ((state-change 2 inertia force_ moment) s2 0))))
  (test-equal "derivative of rotational impulse is rotational momentum"
    '(2 3 5) (angular-momentum ((state-change 2 inertia force_ moment) s 0))))

(define s1 (make-state '(2 3 5) '(0 0  1) 1 '(0 0 0)))
(define s2 (make-state '(2 3 8) '(0 0 -1) 1 '(0 0 0)))
(define s3 (make-state '(2 3 5) '(0 0  1) 1 '(0 -1 0)))
(define s4 (make-state '(2 3 8) '(0 0 -1) 1 '(0 1 0)))
(define light 1.0)
(define heavy 5.9742e+24)
(define inertia-a(inertia-body '((1 0 0) (0 1 0) (0 0 1))))
(define inertia-b (inertia-body '((0.5 0 0) (0 0.5 0) (0 0 0.5))))
(define ca '((2 3 6) . (2 3 7)))
(define cb '((3 3 6) . (3 3 7)))
(test-group "simulation of collision (friction and micro-collision parameters not tested)"
  (test-equal "Position of first object is preserved"
    '(2 3 5) (position (car (collision s1 s2 light light inertia-a inertia-b ca 0.0 0.0 0.1))))
  (test-equal "Position of second object is preserved"
    '(2 3 8) (position (cdr (collision s1 s2 light light inertia-a inertia-b ca 0.0 0.0 0.1))))
  (test-equal "deflect first object"
    '(0.0 0.0 -1.0) (linear-momentum (car (collision s1 s2 light heavy inertia-a inertia-b ca 0.0 0.0 0.1))))
  (test-equal "stop first object"
    '(0.0 0.0 0.0) (linear-momentum (car (collision s1 s2 light heavy inertia-a inertia-b ca 1.0 0.0 0.1))))
  (test-equal "Deflect second object"
    '(0.0 0.0 1.0) (linear-momentum (cdr (collision s1 s2 heavy light inertia-a inertia-b ca 0.0 0.0 0.1))))
  (test-equal "Spin first object"
    '(0.0 0.5 0.0) (angular-momentum (car (collision s1 s2 light heavy inertia-a inertia-b cb 0.0 0.0 0.1))))
  (test-equal "Spin second object"
    '(0.0 -0.5 0.0) (angular-momentum (cdr (collision s1 s2 heavy light inertia-a inertia-b cb 0.0 0.0 0.1))))
  (test-equal "Change spin of first object"
    '(0.0 1.0 0.0) (angular-momentum (car (collision s3 s4 light heavy inertia-a inertia-b cb 0.0 0.0 0.1))))
  (test-equal "Change spin of second object"
    '(0.0 -1.0 0.0) (angular-momentum (cdr (collision s3 s4 heavy light inertia-a inertia-b cb 0.0 0.0 0.1)))))

(define l (make-lander (make-state '(2 3 5) '(0 0 1.0) 1 '(0 0 0)) (make-spring 3 2)))
(define inertia (inertia-body '((0.5 0 0) (0 0.5 0) (0 0 0.5))))
(test-group "structure for lander"
  (test-equal "Position of main body"
    '(2 3 5) (position (state l)))
  (test-eqv "Position of gear"
    3 (position (car (gears l))))
  (test-equal "state change of main body"
    '(0 0 0.5) (position (state ((lander-change 2 inertia '(0 0 0) 12 6 2 '(0 0 0)) l 0))))
  (test-eqv "state change of gears"
    2 (position (car (gears ((lander-change 2 inertia '(0 0 0) 12 6 2 '(0 0 0)) l 0))))))

(define l (make-lander (make-state '(2 3 5) '(0 0 1.0) 1 '(0 0 0)) (make-spring 3 2)))
(test-group "scalar multiplication of lander state"
  (test-equal "multiply position of main body"
    '(4 6 10) (position (state (* l 2))))
  (test-equal "multiply position of gear"
    6 (position (car (gears (* l 2))))))

(define l1 (make-lander (make-state '(2 3 5) '(0 0 1.0) 1 '(0 0 0)) (make-spring 3 2)))
(define l2 (make-lander (make-state '(3 5 7) '(0 0 1.0) 1 '(0 0 0)) (make-spring 7 5)))
(test-group "add lander states"
  (test-equal "add positions of main bodies"
    '(5 8 12) (position (state (+ l1 l2))))
  (test-equal "add posoitions of gears"
    10 (position (car (gears (+ l1 l2))))))

(define l (make-lander (make-state '(2 3 5) '(0 0 1.0) 1 '(0 0 0)) (make-spring 3 2)))
(define l2 (make-lander (make-state '(2 3 5) '(0 0 1.0) +i '(0 0 0)) (make-spring 3 2)))
(define inertia (inertia-body '((0.5 0 0) (0 0.5 0) (0 0 0.5))))
(test-group "gears of lander"
  (test-equal "gear exerts force on lander"
    '(0 48 0) (linear-momentum (state ((lander-change 3 inertia '(0 0 0) 12 6 2 '(0 0 0)) l 0))))
  (test-equal "add external forces"
    '(2 48 3) (linear-momentum (state ((lander-change 3 inertia '(2 0 3) 12 6 2 '(0 0 0)) l 0))))
  (test-equal "rotate gear forces correctly"
    '(0.0 -48.0 0.0) (linear-momentum (state ((lander-change 3 inertia '(0 0 0) 12 6 2 '(0 0 0)) l2 0))))
  (test-equal "introduce rotational moment"
    '(0 0 0) (angular-momentum (state ((lander-change 3 inertia '(0 0 0) 12 6 2 '(0 0 0)) l2 0)))))

(test-end "sfsim physics")
