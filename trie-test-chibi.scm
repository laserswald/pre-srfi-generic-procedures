
(import (scheme base)
        (scheme write)
        (scheme comparator)
        (chibi test)
        (trie))

(define my-trie (trie default-comparator))

;; make-trie
;; trie

;; trie?
(test-assert (trie? my-trie))

;; trie-insert!
;; trie-empty?
(test-assert (trie-empty? my-trie))
(trie-insert! my-trie '(asdf) 'jkl)
(test-assert (not (trie-empty? my-trie)))

;; trie-ref
(test-assert (eqv? 'jkl (trie-ref my-trie '(asdf))))
(test-assert (eqv? #f (trie-ref my-trie '(as df))))

(trie-insert! my-trie '(as df) 'j)
(test-assert (eqv? 'j (trie-ref my-trie '(as df))))
(trie-insert! my-trie '(as dj) 'k)
(test-assert (eqv? 'k (trie-ref my-trie '(as dj))))



