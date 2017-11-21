(use-modules (srfi srfi-64)
             (ssimlib quaternion))


(test-begin "ssim")
(test-begin "quaternion")
(test-equal "Create quaternion"
  '(0 0 0 0) (make-quaternion))
(test-end "quaternion")
(test-end "ssim")
