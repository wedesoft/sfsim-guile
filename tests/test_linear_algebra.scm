(use-modules (srfi srfi-64)
             (sfsim linear-algebra))


(test-begin "sfsim linear-algebra")

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

(test-begin "diagonal matrix")
  (test-equal "create 2D diagonal matrix"
    '((2 0) (0 3)) (diagonal '(2 3)))
  (test-equal "create 3D diagonal matrix"
    '((2 0 0) (0 3 0) (0 0 5)) (diagonal '(2 3 5)))
(test-end "diagonal matrix")

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

(test-begin "submatrix")
  (test-equal "Empty submatrix"
     '() (submatrix '((42)) 0 0))
  (test-equal "First submatrix of 2D matrix"
     '((1 2)) (submatrix '((1 2 3) (4 5 6)) 1 2))
  (test-equal "Last submatrix of 2D matrix"
     '((5 6)) (submatrix '((1 2 3) (4 5 6)) 0 0))
(test-end "submatrix")

(test-begin "inverse")
  (test-equal "Invert diagonal matrix"
    '((1/2 0 0) (0 1/4 0) (0 0 1/8)) (inverse '((2 0 0) (0 4 0) (0 0 8))))
  (test-equal "Invert 2D matrix"
    '((-7 3) (5 -2)) (inverse '((2 3) (5 7))))
(test-end "inverse")

(test-begin "dot")
  (test-equal "Matrix-vector product"
    '(6 15 35) (dot (diagonal '(2 3 5)) '(3 5 7)))
  (test-equal "Matrix-matrix product"
    '((3 0) (0 2)) (dot '((1 0) (0 2)) '((3 0) (0 1))))
(test-end "dot")

(test-begin "transpose")
  (test-equal "One element matrix transpose"
    '((42)) (transpose '((42))))
  (test-equal "Transpose 2x3 matrix"
    '((1 4) (2 5) (3 6)) (transpose '((1 2 3) (4 5 6))))
(test-end "transpose")

(test-begin "homogeneous-matrix")
  (test-equal "Create homogeneous matrix from 3x3 matrix"
    '((1 2 3 0) (4 5 6 0) (7 8 9 0) (0 0 0 1)) (homogeneous-matrix '((1 2 3) (4 5 6) (7 8 9))))
  (test-equal "Create homogeneous matrix from 3x3 matrix and translation vector"
    '((1 2 3 -1) (4 5 6 -2) (7 8 9 -3) (0 0 0 1)) (homogeneous-matrix '((1 2 3) (4 5 6) (7 8 9)) '(-1 -2 -3)))
(test-end "homogeneous-matrix")

(test-end "sfsim linear-algebra")
