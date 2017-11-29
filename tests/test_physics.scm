(use-modules (srfi srfi-64)
             (sfsim physics)
             (sfsim quaternion))


(define pi (* 2 (acos 0)))


(test-begin "sfsim physics")
(test-begin "clock")
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
    (let [(t (clock))] (sleep 1) (elapsed t) (>= (elapsed t) 1)))
(test-end "clock")

(test-begin "Runge-Kutta method")
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
    (exp 1) (runge-kutta 1 1 (lambda (y dt) y)) 1e-2)
(test-end "Runge-Kutta method")

(test-begin "inertia of cuboid")
  (test-equal "Inertia of unit cube"
    '((1 0 0) (0 1 0) (0 0 1)) (cuboid-inertia 6 1 1 1))
  (test-equal "Inertia of heavier unit cube"
    '((2 0 0) (0 2 0) (0 0 2)) (cuboid-inertia 12 1 1 1))
  (test-equal "Inertia of cuboid"
    '((34 0 0) (0 29 0) (0 0 13)) (cuboid-inertia 12 2 3 5))
(test-end "inertia of cuboid")

(define (round-vector v) (map (compose inexact->exact round) v))
(define (round-matrix m) (map round-vector m))

(test-begin "inertia")
  (let [(inertia (inertia-body '((3 2 1) (2 4 2) (1 2 5))))]
    (test-equal "use specified inertia matrix"
      '((3 2 1) (2 4 2) (1 2 5)) (round-matrix (inertia (quaternion-rotation 0 '(1 0 0)))))
    (test-equal "use specified inertia matrix"
      '((3 -1 2) (-1 5 -2) (2 -2 4)) (round-matrix (inertia (quaternion-rotation (/ pi 2) '(1 0 0))))))
(test-end "inertia")
(test-end "sfsim physics")
