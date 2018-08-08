(use-modules (sfsim util))


(test-begin "sfsim util")

(test-group "argmin and argmax"
  (test-equal "Get element with minimum of argument"
    '(a . 1) (argmin cdr '((c . 3) (a . 1) (b . 2))))
  (test-equal "Get element with maximum of argument"
    '(c . 3) (argmax cdr '((c . 3) (a . 1) (b . 2)))))

(test-end "sfsim util")

