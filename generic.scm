
;;; The list of other methods that can be 
(define next-methods (make-parameter #f))

(define (call-next-method . args)
  (let ((nms (next-methods)))
    (if (not nms)
      (error "call-next-method: not within a method context" args)
      (parameterize ((next-methods (cdr nms)))
        (apply (car nms) args))))))

;;;
;;; Generic procedures.
;;;

;;; Here, we define special directive values. If a generic procedure is called with such a value, 
;;; it won't call any methods and instead be manipulated by itself.
(define insert-method-tag (list 'add-method-tag))

(define (make-generic name)
  (define (is-add-method-directive? args)
    (and (eq? (car args) add-method-tag)
         (list? (cdar args))
         (procedure? (cddar args))))

  (let ((name name)
        (methods (make-trie method-comparator)))

    (define (insert-method predicates args)
      (let ((method (make-method predicates args)))
        (set! methods (insert-after (cut method-resolution<? <> method)
                                    method
                                    methods))))
    
    (define (select-applicable-methods predicates args))

    (lambda args
      (if (is-add-method-directive? args)
        (insert-method (cdar args) (cddar args))
        (parameterize (())
          (apply (find-matching-method methods args) args)))))))

(define (generic-add! g preds proc)
  (g add-method-tag preds proc))

(define-syntax define-generic
  (syntax-rules ()
    ((define-generic name)
     (define name (make-generic 'name)))))

(define-syntax define-method
  (syntax-rules ()
    ((define-method (name (type param) ...) . body)
     (generic-add! name
                   (list type ...)
                   (lambda (param ...)
                     body)))))

