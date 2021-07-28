;;;
;;; An entry in the predicate ordering.
;;;
(define-record-type <hierarchy>
  (make-hierarchy predicate specializations)
  hierarchy?
  (predicate hierarchy-predicate)
  (specializations hierarchy-specializations hierarchy-set-specializations!))

(define (display-order hierarchy)
  (set-for-each (lambda (sub)
                  (display "\t")
                  (display sub)
                  (newline))
                (hierarchy-specializations hierarchy))
  (newline))

(define (hierarchy-add-specialization! hierarchy sub)
  (hierarchy-set-specializations!
   hierarchy
   (set-adjoin (hierarchy-specializations hierarchy) sub)))

;;;
;;; The global predicate specialization registry.
;;;
(define *predicate-ordering* 
  (mapping (make-default-comparator)))

(define (display-ordering)
  (mapping-for-each (lambda (k v)
                      (display k)
                      (newline)
                      (display-order v))
                    *predicate-ordering*))
                               

;;;
;;; Look up the ordering for a predicate in the global ordering.
;;;
(define (predicate-ordering-ref predicate)
  (mapping-ref *predicate-ordering* predicate))

(define (predicate-ordering-adjoin! hierarchy)
  (set! *predicate-ordering*
        (mapping-adjoin *predicate-ordering* 
                        (hierarchy-predicate hierarchy)
                        hierarchy)))

;;;
;;; Is this predicate registered in the ordering?
;;;
(define (predicate-ordered? predicate)
  (mapping-contains? *predicate-ordering* predicate))

(define (make-predicate-ordering predicate children)
  (make-new-hierarchy predicate
                            (list->set (make-default-comparator) children)))

(define (object? obj) #t)

(define (predicate-specialize! sub super)
  (unless (predicate-ordered? sub)
    (predicate-ordering-adjoin!
     (make-predicate-ordering sub '())))
  (unless (predicate-ordered? super)
    (predicate-ordering-adjoin!
     (make-predicate-ordering super '())))
  (hierarchy-add-specialization! 
   (predicate-ordering-ref super)
   sub))

(define (predicate-immediate-specializations predicate)
  (hierarchy-specializations 
   (predicate-ordering-ref predicate)))

(define (predicate-specializes? sub super)
  (if (predicate-ordered? super)
    (let ((specializations (hierarchy-specializations (predicate-ordering-ref super))))
      (if (set-contains? specializations sub)
        #t 
        (set-any? (cut predicate-specializes? <> sub) specializations)))
    (error "Predicate is not ordered" super)))

