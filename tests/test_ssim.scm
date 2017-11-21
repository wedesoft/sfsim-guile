(use-modules (srfi srfi-64)
             (ssimlib quaternion))


(test-begin "ssim")
(test-begin "quaternion")
(test-equal "Create quaternion"
  '(0 0 0 0) (make-quaternion))
(test-equal "Initialise quaternion with values"
  '(2 3 5 7) (make-quaternion 2 3 5 7))
(test-end "quaternion")
(test-end "ssim")
