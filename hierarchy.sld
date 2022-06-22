
(define-library (hierarchy)
  (export hierarchy?
          make-hierarchy          
          current-hierarchy
          derive!
          derives?
          is-a?
          descendants
          ancestors
          test-hierarchy
          object?)

  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (scheme comparator)
          (scheme mapping hash)
          (scheme set)
          (scheme list)
          (srfi 26)
          (srfi 64))

  (include "hierarchy.scm"))

