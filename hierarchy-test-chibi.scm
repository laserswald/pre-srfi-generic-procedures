
(import (scheme base)
        (scheme load)
        (chibi test)
        (hierarchy))

(define (object? obj) #t)

(test-group "add predicate specializations"
  (begin
   (predicate-specialize! list? object?)
   (predicate-specialize! null? list?)
   (test-assert (predicate-specializes? list? object?))
   (test-assert (predicate-specializes? null? list?))
   (test-assert (predicate-specializes? null? object?))))

