
(define (object? obj) #t)

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
                         (list->set (make-default-comparator) specializations)))

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

;;;
;;;
;;;
(define-record-type <hierarchy>
  (%make-hierarchy entries)
  hierarchy?
  (entries hierarchy-entries 
           hierarchy-set-entries!))

(define (make-hierarchy)
  (%make-hierarchy (hashmap (make-default-comparator))))

(define (hierarchy-ref hier p)
  (hashmap-ref (hierarchy-entries hier) p))

(define (hierarchy-add! hierarchy entry)
  (hierarchy-set-entries!
   hierarchy
   (hashmap-adjoin (hierarchy-entries hierarchy)
                   (hierarchy-entry-predicate entry)
                   entry)))

;;; Is this predicate registered in the ordering?
(define (hierarchy-contains? hier predicate)
  (hashmap-contains? (hierarchy-entries hier)
                     predicate))

(define (hierarchy-for-each proc hier)
  (hashmap-for-each proc (hierarchy-entries hier)))

(define (hierarchy-adjoin! hier pred . specializations)
  (if (hierarchy-contains? hier pred)
    (let ((entry (hierarchy-ref hier pred)))
      (for-each (lambda (specialization)
                  (hierarchy-entry-add-specialization! entry
                                                       specialization))
                specializations))
    (hierarchy-add! hier (apply hierarchy-entry pred specializations))))

;;;
;;; The global predicate specialization registry.
;;;

(define current-hierarchy 
  (make-parameter (make-hierarchy)))

(define (display-current-hierarchy)
  (hierarchy-for-each (lambda (k v)
                        (display k)
                        (newline)
                        (display-order v))
                      (current-hierarchy)))


(define derive! 
  (case-lambda
   ((sub super)
    (derive! sub super (current-hierarchy)))
   ((sub super hier)
    (hierarchy-adjoin! hier sub)
    (hierarchy-adjoin! hier super sub))))

(define (predicate-immediate-specializations hierarchy predicate)
  (hierarchy-entry-specializations
   (hierarchy-ref hierarchy predicate)))

(define is-a?
  (case-lambda 
   ((sup sub)
    (is-a? sup sub (current-hierarchy)))
   ((sup sub hier)
    (unless (hierarchy-contains? hier sup)
      (error "Predicate is not ordered" sup))
    (let ((specializations (hierarchy-entry-specializations 
                            (hierarchy-ref hier sup))))
      (or (set-contains? specializations sub)
          (set-any? (cut is-a? <> sub hier) specializations))))))


(define (test-hierarchy)
  (test-begin "derive")
  (derive! list? object?)
  (test-assert "list is an object" (is-a? object? list?))
  (derive! null? list?)
  (test-assert "null is a list" (is-a? list? null?))
  (test-assert "null is an object" (is-a? object? null?))
  (derive! pair? list?)
  (test-assert "pairs are lists" (is-a? list? null?))
  (test-assert "pairs are objects" (is-a? object? null?))
  (test-end "derive"))
