
(define-record-type <trie>
  (%make-trie comparator value children)
  trie?
  (comparator trie-comparator)
  (value trie-value trie-set-value!)
  (children trie-children trie-set-children!))

(define (make-trie comparator value children)
  (%make-trie comparator
              value
              (make-hashmap comparator)))
