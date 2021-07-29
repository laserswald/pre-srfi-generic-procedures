
(define-library (trie)

(export make-trie
        trie
        trie?
        trie-empty?
        trie-ref
        trie-insert!)

(import (scheme base)
        (srfi 146 hash))

(begin

(define-record-type <trie-node>
  (make-trie-node path value children)
  trie-node?
  (path trie-node-path trie-node-set-path!)
  (value trie-node-value trie-node-set-value!)
  (children trie-node-children trie-node-set-children!))

(define (trie-node-empty? node)
  (and (eqv? #f (trie-node-value node))
       (hashmap-empty? (trie-node-children node))))

(define (trie-node-has-child? node path)
  (hashmap-contains? (trie-node-children node) (car path)))

(define (trie-node-child node path)
  (hashmap-ref (trie-node-children node) (car path)))

(define (trie-node-ref node path)
  (if (null? path)
    (trie-node-value node)
    (if (trie-node-has-child? node path)
      (trie-node-ref (trie-node-child node path) (cdr path))
      #f)))

(define (trie-node-insert! node comparator path value)
  (if (null? path)
    (if (eqv? (trie-node-value node) #f)
      (trie-node-set-value! node value)
      (error "trie-node-insert!: already an entry" value))
    (begin
      (unless (trie-node-has-child? node path)
        (trie-node-set-children! 
         node
         (hashmap-set! (trie-node-children node)
                       (car path)
                       (make-trie-node (car path) #f (hashmap comparator)))))
      (trie-node-insert! (trie-node-child node path)
                         comparator
                         (cdr path)
                         value))))

(define-record-type <trie>
  (make-trie comparator root)
  trie?
  (comparator trie-comparator)
  (root trie-root trie-set-root!))

(define (trie comparator)
  (make-trie comparator (make-trie-node '() #f (hashmap comparator))))

(define (trie-empty? trie)
  (trie-node-empty? (trie-root trie)))

(define (trie-ref trie path)
  (trie-node-ref (trie-root trie) path))

(define (trie-insert! trie path value)
  (trie-node-insert! (trie-root trie) (trie-comparator trie) path value))

))
