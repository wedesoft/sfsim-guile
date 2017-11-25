(define-module (ssim linear-algebra)
  #:export (cross-product))


(define (cross-product a b)
  (list (- (* (cadr  a) (caddr b)) (* (caddr a) (cadr  b)))
        (- (* (caddr a) (car   b)) (* (car   a) (caddr b)))
        (- (* (car   a) (cadr  b)) (* (cadr  a) (car   b)))))
