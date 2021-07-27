
(define-library (lazr generic)

  (export make-generic
          generic?
          define-generic
          define-method)

  (import (scheme base)
          (scheme mapping hash)
          (lazr types))

  (include "method.scm")
  (include "generic.scm"))
