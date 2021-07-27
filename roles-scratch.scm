
(define-typeclass <functor>
  (fmap proc fctr))

(define-instance (<functor> <list> fmap) map)
(define-instance (<functor> <vector> fmap) vector-map)

(define-typeclass (<applicative> <functor>))
  
