;;;
;;; An entry in the predicate ordering.
;;;
(define-record-type <predicate-order>
  (make-new-predicate-order predicate specializations)
  predicate-order?
  (predicate predicate-order-predicate)
  (specializations predicate-order-specializations predicate-order-set-specializations!))

(define (display-order predicate-order)
  (set-for-each (lambda (sub)
                  (display "\t")
                  (display sub)
                  (newline))
                (predicate-order-specializations predicate-order))
  (newline))

(define (predicate-order-add-specialization! predicate-order sub)
  (predicate-order-set-specializations!
   predicate-order
   (set-adjoin (predicate-order-specializations predicate-order) sub)))

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

(define (predicate-ordering-adjoin! predicate-order)
  (set! *predicate-ordering*
        (mapping-adjoin *predicate-ordering* 
                        (predicate-order-predicate predicate-order)
                        predicate-order)))

;;;
;;; Is this predicate registered in the ordering?
;;;
(define (predicate-ordered? predicate)
  (mapping-contains? *predicate-ordering* predicate))

(define (make-predicate-ordering predicate children)
  (make-new-predicate-order predicate
                            (list->set (make-default-comparator) children)))

(define (object? obj) #t)

(define (predicate-specialize! sub super)
  (unless (predicate-ordered? sub)
    (predicate-ordering-adjoin!
     (make-predicate-ordering sub '())))
  (unless (predicate-ordered? super)
    (predicate-ordering-adjoin!
     (make-predicate-ordering super '())))
  (predicate-order-add-specialization! 
   (predicate-ordering-ref super)
   sub))

(define (predicate-immediate-specializations predicate)
  (predicate-order-specializations 
   (predicate-ordering-ref predicate)))

(define (predicate-specializes? sub super)
  (if (predicate-ordered? super)
    (let ((specializations (predicate-order-specializations (predicate-ordering-ref super))))
      (if (set-contains? specializations sub)
        #t 
        (set-any? (cut predicate-specializes? <> sub) specializations)))
    (error "Predicate is not ordered" super)))

