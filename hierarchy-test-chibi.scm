
(import (scheme base)
        (chibi test)
        (hierarchy))

(test-group "add predicate specializations"
  (begin
   (derive! list? object?)
   (derive! null? list?)
   (test-assert (is-a? list? object?))
   (test-assert (is-a? null? list?))
   (test-assert (is-a? null? object?))))

