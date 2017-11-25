(define-module (ssim linear-algebra)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:export (cross-product)
  #:re-export (+ - *))


(define-method (+ (a <list>) (b <list>))
  (map + a b))

(define-method (- (a <list>) (b <list>))
  (map - a b))

(define-method (* (a <list>) (b <number>))
  (map (cut * <> b) a))

(define-method (* (a <number>) (b <list>))
  (map (cut * <> a) b))

(define (cross-product a b)
  (list (- (* (cadr  a) (caddr b)) (* (caddr a) (cadr  b)))
        (- (* (caddr a) (car   b)) (* (car   a) (caddr b)))
        (- (* (car   a) (cadr  b)) (* (cadr  a) (car   b)))))
