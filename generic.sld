 (define-library (generic)

  (export define-generic 
          define-method

          make-generic
          generic-add!
          generic-methods
          generic-metadata
          display-generic-metadata
          
          test-method-storage
          method-storage-lookup)

  (import (scheme base)
          (scheme write)
          (scheme cxr)
          (scheme show)
          (scheme sort)
          (srfi 229)
          (srfi 64)
          (hierarchy)
          (methods))

  (include "generic.scm"))
