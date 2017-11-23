(use-modules (srfi srfi-64)
             (ssim physics))


(test-begin "ssim physics")
(test-begin "inertia of cuboid")
  (test-equal "Inertia of unit cube"
    '(1 1 1) (cuboid-inertia 6 1 1 1))
  (test-equal "Inertia of heavier unit cube"
    '(2 2 2) (cuboid-inertia 12 1 1 1))
  (test-equal "Inertia of cuboid"
    '(34 29 13) (cuboid-inertia 12 2 3 5))
(test-end "inertia of cuboid")
(test-end "ssim physics")
