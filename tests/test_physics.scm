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
    2 (position ((spring-change 12 6 2) (make-spring 3 2))))
  (test-equal "speed change depends on elongation of spring"
    -30 (speed ((spring-change 12 6 2) (make-spring 5 0))))
  (test-equal "speed change depends on damping of spring"
    -3 (speed ((spring-change 12 6 2) (make-spring 0 1)))))

(test-group "apply impulses"
  (test-equal "applying an impulse increases momentum"
    11 (apply-linear-impulse 5 6))
  (test-equal "applying an impulse to an momentum vector"
    '(11) (apply-linear-impulse '(5) '(6)))
  (test-equal "rotational impulse with no effect"
    '(2 3 5) (apply-rotational-impulse '(2 3 5) '(1 0 0) '(1 0 0)))
  (test-equal "rotational impulse with lever"
    '(2 3 6) (apply-rotational-impulse '(2 3 5) '(1 0 0) '(0 1 0))))

(test-end "sfsim physics")
