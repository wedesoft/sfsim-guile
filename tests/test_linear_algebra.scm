(use-modules (srfi srfi-64)
             (ssim linear-algebra))


(test-begin "ssim linear-algebra")

(test-begin "vector operations")
  (test-equal "add two vectors"
    '(7 10) (+ '(2 3) '(5 7)))
  (test-equal "subtract a vector from another"
    '(2 3) (- '(7 10) '(5 7)))
  (test-equal "multiply vector with scalar"
    '(10 15) (* '(2 3) 5))
  (test-equal "multiply scalar with vector"
    '(10 15) (* 5 '(2 3)))
(test-end "vector operations")

(test-equal "create diagonal matrix"
  '((2 0 0) (0 3 0) (0 0 5)) (diagonal '(2 3 5)))

(test-begin "inner product")
  (test-equal "inner product of one-dimensional vectors"
    6 (inner-product '(2) '(3)))
  (test-equal "inner product of two-dimensional vectors"
    31 (inner-product '(2 3) '(5 7)))
(test-end "inner product")

(test-begin "cross product")
  (test-equal "cross product of zero vectors"
    '(0 0 0) (cross-product '(1 0 0) '(0 0 0)))
  (test-equal "cross product of first and second basis vector"
    '(0 0 1) (cross-product '(1 0 0) '(0 1 0)))
  (test-equal "cross product of second and first basis vector"
    '(0 0 -1) (cross-product '(0 1 0) '(1 0 0)))
  (test-equal "cross product of second and third basis vector"
    '(1 0 0) (cross-product '(0 1 0) '(0 0 1)))
  (test-equal "cross product of third and second basis vector"
    '(-1 0 0) (cross-product '(0 0 1) '(0 1 0)))
  (test-equal "cross product of third and first basis vector"
    '(0 1 0) (cross-product '(0 0 1) '(1 0 0)))
  (test-equal "cross product of first and third basis vector"
    '(0 -1 0) (cross-product '(1 0 0) '(0 0 1)))
(test-end "cross product")

(test-begin "norm")
  (test-equal "Norm of 1D vector"
    3 (norm '(3)))
  (test-equal "Norm of 2D vector"
    5 (norm '(3 4)))
(test-end "norm")

(test-begin "permutations")
  (test-equal "permutations of single element array"
    '((0)) (permutations '(0)))
  (test-equal "permutations of array with two elements"
    '((0 1) (1 0)) (permutations '(0 1)))
  (test-equal "permutations of array with three elements"
    '((0 1 2) (0 2 1) (1 2 0) (1 0 2) (2 0 1) (2 1 0)) (permutations '(0 1 2)))
(test-end "permutations")

(test-begin "determinant")
  (test-equal "Determinant of 2D diagonal matrix"
    6 (determinant '((2 0) (0 3))))
  (test-equal "Determinant of 2D matrix"
    -1 (determinant '((2 3) (5 7))))
  (test-equal "Determinant of 3D matrix"
    3 (determinant '((0 1 2) (3 2 1) (1 1 0))))
(test-end "determinant")

(test-begin "inverse")
  (test-equal "Invert diagonal matrix"
    '((1/2 0 0) (0 1/4 0) (0 0 1/8)) (inverse '((2 0 0) (0 4 0) (0 0 8))))
(test-end "inverse")

(test-begin "dot")
  (test-equal "Matrix-vector product"
    '(6 15 35) (dot (diagonal '(2 3 5)) '(3 5 7)))
(test-end "dot")

(test-end "ssim linear-algebra")
