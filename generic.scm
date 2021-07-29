
(define (insert-on pred x l)
  (let-values ((before after) (span pred l))
    (append before (list x) after)))

(define next-method
  (make-parameter #f))


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

    (lambda args
      (if (is-add-method-directive? args)
        (insert-method (cdar args) (cddar args))
        (let (())
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
    ((define-method (name (param type) ...) . body)
     (generic-add! name
                   (list type ...)
                   (lambda (param ...)
                     body)))))


(define (object? obj) #t)

(define-generic initialize)
(define-method (initialize (obj object?)) obj)

