(use-modules (srfi srfi-64)
             (ssim physics))


(test-begin "ssim physics")
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

(test-begin "inertia of cuboid")
  (test-equal "Inertia of unit cube"
    '(1 1 1) (cuboid-inertia 6 1 1 1))
  (test-equal "Inertia of heavier unit cube"
    '(2 2 2) (cuboid-inertia 12 1 1 1))
  (test-equal "Inertia of cuboid"
    '(34 29 13) (cuboid-inertia 12 2 3 5))
(test-end "inertia of cuboid")
(test-end "ssim physics")
