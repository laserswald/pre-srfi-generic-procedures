(import (scheme base)
        (scheme write)
        (scheme show)
        (prefix (gauche base) gauche:)

        (generic)
        (hierarchy)
        
        (srfi 64))

(test-begin "generics")

(test-method-storage)
(test-hierarchy)

(test-group "single-argument"
  (define-generic sequence?)
  (define-method (sequence? (vector? v)) #t)
  (define-method (sequence? (list? l)) #t)

  (test-assert "vectors should be considered sequences" 
               (sequence? (vector 1 2 3)))

  (test-assert "lists should be considered sequences" 
               (sequence? (list 1 2 3))))

(define-generic join)

(define-method (join (number? a) (number? b))
  (+ a b))

(define-method (join (string? a) (string? b)) 
  (string-append a b))

(test-group "two-arguments"
  (test-equal 3 (join 1 2))
  (test-equal "alphabeta" (join "alpha" "beta")))

(derive! integer? number?)
(define-generic join*)

(define-method (join* (integer? a) (integer? b))
  (+ a b 1))

(define-method (join* (number? a) (number? b))
  (+ a b))

(test-group "overriding"
  (test-equal 4 (join* 1 2))
  (test-equal 3.0 (join* 1.5 1.5)))

(test-assert (procedure? join*))

(test-end "generics")

