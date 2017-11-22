(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ssim quaternion))


(define pi (* 2 (acos 0)))

(test-begin "ssim quaternion")
(test-begin "quaternion objects")
(test-equal "Create quaternion"
  '(0 0 0 0) (map (cut <> (make-quaternion)) (list real-part imag-part jmag-part kmag-part)))
(test-equal "Initialise quaternion with values"
  '(2 3 5 7) (map (cut <> (make-quaternion 2 3 5 7)) (list real-part imag-part jmag-part kmag-part)))
(test-equal "Display quaternion"
  "2.0+3.0i+5.0j+7.0k" (call-with-output-string (lambda (port) (write (make-quaternion 2 3 5 7) port))))
(test-equal "Display negative components"
  "-2.0-3.0i-5.0j-7.0k" (call-with-output-string (lambda (port) (write (make-quaternion -2 -3 -5 -7) port))))
(test-equal "Equal quaternions"
  (make-quaternion 2 3 5 7) (make-quaternion 2 3 5 7))
(test-end "quaternion objects")

(test-begin "comparisons")
(let [(i (make-quaternion 0 1 0 0))
      (j (make-quaternion 0 0 1 0))
      (k (make-quaternion 0 0 0 1))]
  (for-each (lambda (b)
    (test-assert (format #f "Compare zero with ~a" b)
      (not (equal? (make-quaternion 0 0 0 0) b))))
    (list 1 i j k))
  (test-assert "Compare zero quaternion with scalar"
    (equal? 0 (make-quaternion 0 0 0 0)))
  (test-assert "Compare quaternion with complex number"
    (equal? 3+5i (make-quaternion 3 5 0 0))))
(test-end "comparisons")

(test-begin "unary operations")
  (test-equal "Negate quaternion"
    (make-quaternion -2 -3 -5 -7) (- (make-quaternion 2 3 5 7)))
(test-end "unary operations")

(test-begin "binary operations")
(let [(i (make-quaternion 0 1 0 0))
      (j (make-quaternion 0 0 1 0))
      (k (make-quaternion 0 0 0 1))]
  (test-equal "Sum of quaternions"
    (make-quaternion 5 8 12 18) (+ (make-quaternion 2 3 5 7) (make-quaternion 3 5 7 11)))
  (test-equal "Sum of quaternion and real number"
    (make-quaternion 5 3 5 7) (+ (make-quaternion 2 3 5 7) 3))
  (test-equal "Sum of real number and quaternion"
    (make-quaternion 5 3 5 7) (+ 3 (make-quaternion 2 3 5 7)))
  (test-equal "Difference of quaternions"
    (make-quaternion -1 -2 -2 -4) (- (make-quaternion 2 3 5 7) (make-quaternion 3 5 7 11)))
  (let [(i2 (make-quaternion 0 2 0 0))
        (j2 (make-quaternion 0 0 2 0))
        (k2 (make-quaternion 0 0 0 2))]
    (for-each (lambda (a results)
      (for-each (lambda (b expected)
          (test-assert (format #f "Multiplying ~a with ~a is expected to be ~a" a b expected)
            (zero? (- (* a b) expected)))
          (format #t "# ~a~&" expected))
        (list 2 i j k) results))
      (list 2 i j k)
      (list (list 4 i2 j2 k2)
            (list i2 -1 k (- j))
            (list j2 (- k) -1 i)
            (list k2 j (- i) -1)))))
(test-end "binary operations")

(test-begin "rotation")
(test-approximate "no rotation"
  1.0 (quaternion-rotation 0 '(0 0 1)) 1e-6)
(test-approximate "no rotation"
  -1.0 (quaternion-rotation (* 2 pi) '(0 0 1)) 1e-6)
(test-end "rotation")
(test-end "ssim quaternion")
