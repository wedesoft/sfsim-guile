(use-modules (sfsim util))


(test-begin "sfsim util")

(test-group "argmin and argmax"
  (test-equal "Get element with minimum of argument"
    '(a . 1) (argmin cdr '((c . 3) (a . 1) (b . 2))))
  (test-equal "Get element with maximum of argument"
    '(c . 3) (argmax cdr '((c . 3) (a . 1) (b . 2)))))

(test-equal "Removing one item from a set of three"
  '((3 5) (2 5) (2 3)) (leave-one-out '(2 3 5)))

(test-end "sfsim util")

