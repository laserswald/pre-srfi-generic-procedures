(import (scheme base)
        (scheme load)
        (chibi test)
        (methods)
        (types))

(define silly-method
  (make-method (list list?) (lambda (x) (display x))))

(test-assert (method? silly-method))
(test-assert (method-matches? silly-method '(())))
(test-assert (not (method-matches? silly-method '())))

(predicate-specialize! list? object?)

(define other-silly-method
  (make-method (list object?) values))

(test-assert (method-resolution<? silly-method other-silly-method))
(test-assert (not (method-resolution<? other-silly-method silly-method)))

