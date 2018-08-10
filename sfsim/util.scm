(define-module (sfsim util)
  #:use-module (srfi srfi-1)
  #:export (argmin argmax leave-one-out))


(define (argop op fun lst)
  (let* [(vals  (map fun lst))
         (opval (apply op vals))]
    (list-ref (reverse lst) (1- (length (member opval vals))))))

(define (argmin fun lst) (argop min fun lst))

(define (argmax fun lst) (argop max fun lst))

(define (leave-one-out lst)
  (map (lambda (i) (append (take lst i) (drop lst (1+ i)))) (iota (length lst))))
