(define-module (sfsim state)
  #:use-module (oop goops)
  #:use-module (sfsim physics)
  #:use-module (sfsim linear-algebra)
  #:export (make-state position speed orientation angular-momentum <state>)
  #:re-export (+ * particle-position))


(define-class <state> (<object>)
              (position         #:init-keyword #:position         #:getter position        )
              (speed            #:init-keyword #:speed            #:getter speed           )
              (orientation      #:init-keyword #:orientation      #:getter orientation     )
              (angular-momentum #:init-keyword #:angular-momentum #:getter angular-momentum))

(define (make-state position speed orientation angular-momentum)
  (make <state> #:position position #:speed speed #:orientation orientation #:angular-momentum angular-momentum))

(define-method (* (state <state>) (scalar <real>))
  (make-state (* (position         state) scalar)
              (* (speed            state) scalar)
              (* (orientation      state) scalar)
              (* (angular-momentum state) scalar)))

(define-method (+ (state <state>) (dstate <state>))
  (make-state (+ (position         state) (position         dstate))
              (+ (speed            state) (speed            dstate))
              (+ (orientation      state) (orientation      dstate))
              (+ (angular-momentum state) (angular-momentum dstate))))

(define-method (particle-position state corner)
  (particle-position (position state) (orientation state) corner))
