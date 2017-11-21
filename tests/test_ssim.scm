(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ssim quaternion))


(test-begin "ssim")
(test-begin "quaternion")
(test-equal "Create quaternion"
  '(0 0 0 0) (map (cut <> (make-quaternion)) (list real-part imag-part jmag-part kmag-part)))
(test-equal "Initialise quaternion with values"
  '(2 3 5 7) (map (cut <> (make-quaternion 2 3 5 7)) (list real-part imag-part jmag-part kmag-part)))
(test-equal "Display quaternion"
  "2.0+3.0i+5.0j+7.0k" (call-with-output-string (lambda (port) (write (make-quaternion 2 3 5 7) port))))
(test-equal "Display negative components"
  "-2.0-3.0i-5.0j-7.0k" (call-with-output-string (lambda (port) (write (make-quaternion -2 -3 -5 -7) port))))
(test-end "quaternion")
(test-end "ssim")
