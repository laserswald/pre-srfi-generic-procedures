;;;
;;; Methods.
;;;

(define-library (methods)

  (export make-method
          method?
          method-body
          method-predicates
          method-matches?
          method-resolution<?)

  (import (scheme base)
          (types))

  (begin
   (define-record-type <method>
     (make-method predicates body)
     method?
     (predicates method-predicates)
     (body method-body))

   (define (method-matches? m args)
     (let loop ((ps (method-predicates m)) (as args))
       (cond ((and (null? as) (null? ps)) #t)
             ((or (null? as) (null? ps))  #f)
             ((not ((car ps) (car as)))   #f)
             (else
              (loop (cdr ps) (cdr as))))))

   (define (method-resolution<? this that)
     (let loop ((this-predicates (method-predicates this))
                (that-predicates (method-predicates that)))
       (if (and (null? this-predicates)
                (null? that-predicates))
         #t
         (let ((current-this (car this-predicates))
               (current-that (car that-predicates)))
           (cond ((or (equal? current-this current-that)
                      (predicate-specializes? current-this current-that))
                  (loop (cdr this-predicates)
                        (cdr that-predicates)))
                 (else #f))))))))

