
(define-syntax define-role
  (syntax-rules ()
    ((define-role name (required-method ...))
     (begin 
      (define-generic name)
      (define-method (name (any? x)) #f)
      (define-generic required-method)...))))

(define-syntax define-fulfillment
  (syntax-rules ()
    ((define-fulfillment (role type?) ((method args) body) ...)
     (begin
      (define-method (role (type? x)) #t)
      (define-method (method args) body) ...
      (derive! type? role)))))

(define-fulfillment (refable? list?)
  ((ref (list? l) (any? a))
   (list-ref l a)))

(define-fulfillment (refable? vector?)
  ((ref (vector? v) (any? a))
   (vector-ref v a)))

(define-fulfillment (refable? string?)
  ((ref (string? s) (any? a))
   (string-ref s a)))

(define-role setable?
  (set-ref!))

(define-role indexable?
  (size element set-element!))

(define-role seq?
  (head tail empty?))

(define-fulfillment (indexable? list?)
  ((size (list? l)) 
   (length l))
  ((element (list? l) (integer? i))
   (list-ref l i))
  ((set-element! (list? l) (integer? i) (any? x))
   (list-set! l i x)))

(define-fulfillment (seq? list?) 
  ((head (list? l))    (car l))
  ((tail (list? l))    (cdr l))
  ((empty? (list? l))  (null? l)))

(define-generic adjoin)

(define-method (adjoin (list? l) (any? x))
  (cons x l))

(define-method (adjoin (flexvector? fv) (any? x))
  (flexvector-append! fv x)
  fv)

(define-generic swap-indexes!)
(define-method (swap-indexes! (indexable? ix)
                              (integer? a) 
                              (integer? b))
  (let ((tmp (elt ix a)))
    (set-elt! ix a (elt ix b))
    (set-elt! ix b tmp)))
