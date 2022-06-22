;;;
;;; Method storage system.
;;;

(define-record-type <method-storage-node>
    (make-method-storage-node predicate children method)
    method-storage-node?
  (predicate method-storage-node-predicate)
  (children method-storage-node-children method-storage-node-set-children!)
  (method method-storage-node-method method-storage-node-set-method!))

(define (method-storage-node->list msn)
  (list 
   'method-storage
   'predicate (method-storage-node-predicate msn)
   'children  (map method-storage-node->list
                   (method-storage-node-children msn))
   'method    (method-storage-node-method msn)))

(define (make-root-method-storage-node)
  (make-method-storage-node #f '() #f))

;;
;; Add a child node to a method storage node.
;;
(define (method-storage-node-add-child! msn child)
  (method-storage-node-set-children! 
   msn 
   (cons child (method-storage-node-children msn))))

;; 
;; Return all the methods that match the arguments.
;;
(define (method-storage-lookup msn args)
  (if (null? args)

    (if (method-storage-node-method msn)
      (list (method-storage-node-method msn))
      '())

    (let loop ((children (method-storage-node-children msn))
               (arg (car args))
               (matches '()))

      (define current-predicate
        (and (pair? children)
             (method-storage-node-predicate (car children))))

      (cond

       ; We are done
       ((null? children) matches)

       ; Grab children of this node that match the next predicate
       ((current-predicate arg)
        (loop (cdr children) 
              arg
              (append matches
                      (method-storage-lookup (car children)
                                             (cdr args)))))

       (else 
        (loop (cdr children) arg matches))))))

;;
;; Insert a new method into the storage tree.
;;
(define (method-storage-insert! msn preds method)
  (if (null? preds)
    (method-storage-node-set-method! msn method)
    (let ((found (or (member (car preds)
                             (method-storage-node-children msn)
                             (lambda (p ms)
                               (equal? p (method-storage-node-predicate ms))))
                     (let ((node (make-method-storage-node (car preds) '() #f)))
                       (method-storage-node-add-child! msn node)
                       node))))
      (method-storage-insert! found (cdr preds) method))))

(define (test-method-storage)
  (test-begin "method-storage")
  (let ((ms (make-root-method-storage-node)))
    (test-assert (method-storage-node? ms))
    (test-assert (null? (method-storage-lookup ms '())))

    (method-storage-insert! ms '() values)
    (test-assert (not (null? (method-storage-lookup ms '()))))
    (test-assert (null? (method-storage-lookup ms (list 'symbol))))

    (method-storage-insert! ms (list symbol?) values)
    (test-assert (not (null? (method-storage-lookup ms (list 'asdf)))))
    (method-storage-insert! ms (list number?) values)
    (test-assert (not (null? (method-storage-lookup ms (list 3))))))
  (test-end "method-storage"))

;;;
;;; Next methods
;;;

;;; The list of other methods that can be 
(define next-methods (make-parameter #f))

(define (call-next-method . args)
  (let ((nms (next-methods)))
    (if (not nms)
      (error "call-next-method: not within a method context" args)
      (parameterize ((next-methods (cdr nms)))
        (apply (car nms) args)))))

;;
;; Apply a method with a parameter of the next method
;;

(define (apply-method-chain method-chain . args)
  (let ((methods method-chain))
    (call-with-current-continuation
     (lambda (escape)
       (apply (car method-chain) args)))))

;;;
;;; Generic procedures.
;;;

(define-record-type <generic-metadata>
    (make-generic-metadata name method-storage default-method)
    generic-metadata?
  (name generic-metadata-name)
  (method-storage generic-metadata-method-storage)
  (default-method generic-metadata-default-method))

(define (generic-metadata->list gm)
  (list (cons 'name (generic-metadata-name gm))
        (cons 'method-storage (method-storage-node->list
                               (generic-metadata-method-storage gm)))
        (cons 'default-method (generic-metadata-default-method gm))))
  

(define (display-generic-metadata md)
  (generic-metadata->list md))

(define (make-generic name hierarchy default)
  (let* ((method-storage (make-root-method-storage-node))
         (metadata       (make-generic-metadata name method-storage default)))
    (lambda/tag metadata
        args
      (let ((matching-methods (method-storage-lookup method-storage args)))
        (if (null? matching-methods)
          (apply (generic-metadata-default-method metadata) args)
          (apply (method-body 
                  (car (list-sort method-resolution<? matching-methods)))
                 args))))))

(define (generic? obj)
  (and (procedure? obj)
       (procedure/tag? obj)
       (generic-metadata? (procedure-tag obj))))

(define (generic-metadata generic)
  (unless (generic? generic)
          (error "Not a generic:" generic))
  (procedure-tag generic))

(define (generic-add! generic predicates implementation)
  (unless (generic? generic)
          (error "Not a generic:" generic))
  (let ((metadata (generic-metadata generic)))
    (method-storage-insert! (generic-metadata-method-storage metadata)
                            predicates
                            (make-method predicates implementation))))

(define (generic-methods generic)
  (generic-metadata-method-storage
   (generic-metadata generic)))

(define-syntax define-generic
  (syntax-rules ()

    ((_ name)
     (define-generic name
       (lambda args
         (error (string-append 
                 (symbol->string 'name)
                 ": unsupported argument combination")
                args))))

    ((_ name default)
     (define-generic name (current-hierarchy) default))

    ((_ name hierarchy default)
     (define name (make-generic 'name hierarchy default)))))

(define-syntax define-method
  (syntax-rules ()
    ((define-method (name (type param) ...) . body)
     (generic-add! name
                   (list type ...)
                   (lambda (param ...) . body)))))

