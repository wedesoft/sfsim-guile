(define-module (ssim linear-algebra)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (cross-product inner-product norm diagonal inverse dot permutations determinant)
  #:re-export (+ - *))


(define-method (+ (a <list>) (b <list>))
  (map + a b))

(define-method (- (a <list>) (b <list>))
  (map - a b))

(define-method (* (a <list>) (b <number>))
  (map (cut * <> b) a))

(define-method (* (a <number>) (b <list>))
  (map (cut * <> a) b))

(define (inner-product a b)
  (reduce + 0 (map * a b)))

(define (cross-product a b)
  (list (- (* (cadr  a) (caddr b)) (* (caddr a) (cadr  b)))
        (- (* (caddr a) (car   b)) (* (car   a) (caddr b)))
        (- (* (car   a) (cadr  b)) (* (cadr  a) (car   b)))))

(define (norm v)
  (sqrt (inner-product v v)))

(define (permutations lst)
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
  (let* [(n       (length mat))
         (indices (iota n))
         (perms   (permutations indices))]
    (reduce + 0
      (map
        (lambda (perm k)
          (* (reduce * 1 (map (lambda (j i) (list-ref (list-ref mat j) i)) indices perm)) (if (even? k) 1 -1)))
         perms
         (iota (length perms))))))

(define (diagonal v)
  (list (list (car v) 0 0) (list 0 (cadr v) 0) (list 0 0 (caddr v))))

(define (inverse matrix)
  (diagonal (map (lambda (i) (/ 1 (list-ref (list-ref matrix i) i))) (iota 3))))

(define (dot matrix vec)
  (map (cut inner-product <> vec) matrix))
