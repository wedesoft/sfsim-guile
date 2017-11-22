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
  (test-equal "Map to complex if possible"
    2.0+3.0i (make-quaternion 2.0 3.0 0.0 0.0))
  (test-equal "Map to scalar if possible"
    1 (make-quaternion 1 0 0 0))
(test-end "quaternion objects")

(test-begin "equality")
  (test-equal "Equal quaternions"
    (make-quaternion 2 3 5 7) (make-quaternion 2 3 5 7))
  (let [(a (make-quaternion 0 1 1 1))
        (b (make-quaternion 1 0 1 1))
        (c (make-quaternion 1 1 0 1))
        (d (make-quaternion 1 1 1 0))]
    (for-each (lambda (q)
      (test-assert (format #f "Compare 1+i+j+k with ~a" q)
        (not (equal? (make-quaternion 1 1 1 1) q))))
      (list a b c d))
    (test-equal "Positive comparison of quaternion"
      (make-quaternion 2 3 5 7) (make-quaternion 2 3 5 7))
    (test-assert "Compare different component types"
      (not (equal? (make-quaternion 3.0 5 7 11) (make-quaternion 3 5 7 11)))))
(test-end "equality")

(test-begin "comparison with =")
  (test-assert "zero quaternion = 0"
    (= (make-quaternion 2 3 5 7) (make-quaternion 2 3 5 7)))
  (let [(i (make-quaternion 0 1 0 0))
        (j (make-quaternion 0 0 1 0))
        (k (make-quaternion 0 0 0 1))]
    (for-each (lambda (b)
      (test-assert (format #f "zero != ~a" b)
        (not (= (make-quaternion 0 0 0 0) b))))
      (list 1 i j k)))
  (test-assert "use = for comparing components"
    (= (make-quaternion 2 3 5 7) (make-quaternion 2.0 3.0 5.0 7.0)))
(test-end "comparison with =")

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
  (test-equal "Sum of quaternion and complex number"
    (make-quaternion 5.0 8.0 5 7) (+ (make-quaternion 2 3 5 7) 3+5i))
  (test-equal "Sum of complex number and quaternion"
    (make-quaternion 5.0 8.0 5 7) (+ 3+5i (make-quaternion 2 3 5 7)))
  (test-equal "Difference of quaternions"
    (make-quaternion -1 -2 -2 -4) (- (make-quaternion 2 3 5 7) (make-quaternion 3 5 7 11)))
  (let [(i2 (make-quaternion 0 2 0 0))
        (j2 (make-quaternion 0 0 2 0))
        (k2 (make-quaternion 0 0 0 2))]
    (for-each (lambda (a results)
      (for-each (lambda (b expected)
          (test-assert (format #f "Multiplying ~a with ~a is expected to be ~a" a b expected)
            (= (* a b) expected))
          (format #t "# ~a~&" expected))
        (list 2 i j k) results))
      (list 2 i j k)
      (list (list 4 i2 j2 k2)
            (list i2 -1 k (- j))
            (list j2 (- k) -1 i)
            (list k2 j (- i) -1)))))
(test-end "binary operations")

(test-begin "norm")
  (test-equal "Norm of unit quaternion"
    1.0 (quaternion-norm (make-quaternion 0.216 0.288 0.48 0.8)))
  (test-equal "Norm of larger quaternion"
    2.0 (quaternion-norm (make-quaternion 0.432 0.576 0.96 1.6)))
(test-end "norm")

(test-begin "conjugate")
  (test-equal "Conjugate of unit quaternion"
    (make-quaternion 0.216 -0.288 -0.48 -0.8) (quaternion-conjugate (make-quaternion 0.216 0.288 0.48 0.8)))
  (test-equal "Conjugate of larger quaternion"
    (make-quaternion 0.108 -0.144 -0.24 -0.4) (quaternion-conjugate (make-quaternion 0.432 0.576 0.96 1.6)))
(test-end "conjugate")

(test-begin "rotation")
  (test-approximate "no rotation"
    1.0 (quaternion-rotation 0 '(0 0 1)) 1e-6)
  (test-approximate "no rotation"
    -1.0 (quaternion-rotation (* 2 pi) '(0 0 1)) 1e-6)
  (let [(q (quaternion-rotation pi '(0.36 0.48 0.8)))]
    (test-approximate "imaginary part of 180° rotation quaternion"
      0.36 (imag-part q) 1e-6)
    (test-approximate "kmaginary part of 180° rotation quaternion"
      0.8 (kmag-part q) 1e-6))
  (let [(q (quaternion-rotation (/ pi 3) '(0.36 0.48 0.8)))]
    (test-approximate "imaginary part of 60° rotation quaternion"
      (* 0.5 0.36) (imag-part q) 1e-6))
  (test-equal "Convert vector to quaternion"
    (make-quaternion 0 2 3 5) (vector->quaternion '(2 3 5)))
  (test-equal "Convert quaternion to vector"
    '(2 3 5) (quaternion->vector (make-quaternion 0 2 3 5)))
  (test-equal "Zero rotation"
    '(2.0 4.0 8.0) (rotate-vector (quaternion-rotation 0 '(1 0 0)) '(2.0 4.0 8.0)))
  (test-equal "Rotate vector around x-axis"
    '(2.0 -8.0 4.0) (map round (rotate-vector (quaternion-rotation (/ pi 2) '(1 0 0)) '(2.0 4.0 8.0))))
(test-end "rotation")
(test-end "ssim quaternion")
