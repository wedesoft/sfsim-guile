(define-module (sfsim linear-algebra)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (cross-product inner-product norm diagonal dot permutations determinant submatrix inverse transpose
            homogeneous-matrix orthogonal-component)
  #:re-export (+ - *))


(define-method (+ (a <list>) (b <list>))
  "Add two vectors."
  (map + a b))

(define-method (- (a <list>) (b <list>))
  "Subtract a vector from another."
  (map - a b))

(define-method (* (a <list>) (b <number>))
  "Multiply a vector with a scalar."
  (map (cut * <> b) a))

(define-method (* (a <number>) (b <list>))
  "Multiply a scalar with a vector."
  (map (cut * <> a) b))

(define (inner-product a b)
  "Compute inner product of two vectors."
  (reduce + 0 (map * a b)))

(define (cross-product a b)
  "Determine cross-product of two 3D vectors."
  (list (- (* (cadr  a) (caddr b)) (* (caddr a) (cadr  b)))
        (- (* (caddr a) (car   b)) (* (car   a) (caddr b)))
        (- (* (car   a) (cadr  b)) (* (cadr  a) (car   b)))))

(define (norm v)
  "Return norm of a vector."
  (sqrt (inner-product v v)))

(define (permutations lst)
  "Return all permutations of list LST. The permutations are ordered so that every alternate permutation is even."
  (if (zero? (length lst))
    '(())
    (concatenate
      (map
        (lambda (item index)
          (let [(remaining (delete item lst))
                (order     (if (even? index) identity reverse))]
            (map (cut cons item <>) (permutations (order remaining)))))
        lst
        (iota (length lst))))))

(define (determinant mat)
  "Compute determinant of a matrix"
  (let* [(n       (length mat))
         (indices (iota n))
         (perms   (permutations indices))]
    (reduce + 0
      (map
        (lambda (perm k)
          (* (reduce * 1 (map (lambda (j i) (list-ref (list-ref mat j) i)) indices perm)) (if (even? k) 1 -1)))
         perms
         (iota (length perms))))))

(define (diagonal lst)
  "Create diagonal matrix"
  (map (lambda (item i) (append (make-list i 0) (list item) (make-list (- (length lst) i 1) 0))) lst (iota (length lst))))

(define (submatrix mat row column)
  "Return submatrix with specified ROW and COLUMN removed."
  (let [(rows    (delete row    (iota (length mat))))
        (columns (delete column (iota (length (car mat)))))]
    (map (lambda (j) (map (lambda (i) (list-ref (list-ref mat j) i)) columns)) rows)))

(define (inverse mat)
  "Compute inverse of matrix"
  (let [(det     (determinant mat))
        (indices (iota (length mat)))
        (sgn     (lambda (v j i) (if (eq? (even? j) (even? i)) v (- v))))]
    (map (lambda (j) (map (lambda (i) (sgn (/ (determinant (submatrix mat i j)) det) j i)) indices)) indices)))

(define (dot mat vec)
  "Multiply a matrix with another matrix or a vector"
  (map (cut inner-product <> vec) mat))

(define (transpose mat)
  "Transpose a matrix"
  (map (lambda (i) (map (cut list-ref <> i) mat)) (iota (length (car mat)))))

(define* (homogeneous-matrix rotation #:optional (translation '(0 0 0)))
  "Convert rotation and optional translation to a homogeneous matrix"
  (append (map append rotation (map list translation)) '((0 0 0 1))))

(define (orthogonal-component normal vec)
  "Get component of VEC orthogonal to normal"
  (- vec (* (inner-product normal vec) normal)))
