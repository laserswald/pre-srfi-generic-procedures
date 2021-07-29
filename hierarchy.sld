
(define-library (hierarchy)
  (export predicate-specialize! 
          predicate-specializes?
          object?)
  (import (scheme base)
          (scheme write)
          (scheme comparator)
          (scheme mapping)
          (scheme set)
          (scheme list)
          (srfi 26))
  (include "hierarchy.scm"))

