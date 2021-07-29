;;;
;;; An entry in the predicate ordering.
;;;
(define-record-type <hierarchy-entry>
  (%make-hierarchy-entry predicate specializations)
  hierarchy-entry?
  ;; procedure?
  (predicate hierarchy-entry-predicate) 
  ;; set?
  (specializations hierarchy-entry-specializations
                   hierarchy-entry-set-specializations!))

(define (hierarchy-entry predicate . specializations)
  (%make-hierarchy-entry predicate
                         (list->set default-comparator specializations)))

(define (display-order hierarchy)
  (set-for-each (lambda (sub)
                  (display "\t")
                  (display sub)
                  (newline))
                (hierarchy-specializations hierarchy))
  (newline))

(define (hierarchy-entry-add-specialization! entry sub)
  (hierarchy-entry-set-specializations!
   entry
   (set-adjoin (hierarchy-entry-specializations entry) sub)))

(define-record-type <hierarchy>
  (%make-hierarchy entries)
  hierarchy?
  (entries hierarchy-entries hierarchy-set-entries!))

(define (make-hierarchy)
  (%make-hierarchy (mapping default-comparator)))

;;;
;;; The global predicate specialization registry.
;;;
(define current-hierarchy (make-parameter (make-hierarchy))))

(define (display-current-hierarchy)
  (mapping-for-each (lambda (k v)
                      (display k)
                      (newline)
                      (display-order v))
                    (hierarchy-entries (current-hierarchy)))

;;;
;;; Look up the ordering for a predicate in the global ordering.
;;;
(define hierarchy-ref
  (case-lambda 
   ((p) (hierarchy-ref (current-hierarchy) p))
   ((p hier)
    (mapping-ref (hierarchy-entries hier) p))))

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

