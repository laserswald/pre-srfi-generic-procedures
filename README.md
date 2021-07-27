
Predicate-based generic procedures
==================================

Author
------

Ben Davenport-Ray

Abstract
--------

This SRFI defines mechanisms for creating procedures which change their behavior depending on one or more predicates. This allows for ad-hoc polymorphism based on type, a specific value, range of values, or any other constraint that can be expressed using a predicate.

Issues
------

- Should a global parameter for a default hierarchy exist?
- What should we actually call the parent-child relation?
- When a predicate function is re-defined, should we re-define all relations as well?

Rationale
---------

The author makes no claims about the rationality of this draft.

Specification
-------------

### Hierarchies

In order to have a useful ordering for methods, there must be an
established ordering of predicates which designate types, which we will call a _predicate hierarchy_, or just a _hierarchy_ for short. 

A hierarchy is a disjoint type.

We say a predicate specializes another predicate when methods that are defined on
the first predicate should be given higher priority than methods defined
on the second.

`make-predicate-hierarchy`

`derive! <pred> <parent-pred> [<hierarchy>]`

Ensure that _pred_ is registered to specialize _subpred_. It is an error if _pred_ already specializes _subpred_.

`specializes? <pred> <other> -> boolean?`

Returns `#t` if _pred_ subsumes _other_, and `#f` otherwise.

`subsumptions <pred> -> list?`

Returns a list of all predicates that have been registered to be subsumed in _pred_. If no subsumptions of _pred_ are registered, the empty list is returned.

`(define-subsume ((<name> ... <subsumed>) ...args) ...body)`

Creates a procedure with the argument list _args_ and the body _body_,
binds it to _name_, and establishes that the newly defined procedure
subsumes the procedures named in _subsumed_.

If a procedure of the same name already exists, all predicates that have
been defined as subsuming the previous definition of name are defined
as subsuming the new definition as well.

### Generic procedures

`make-generic <symbol> [<hierarchy>]`

Define a new generic function with the name _symbol_. If no hierarchy is given, then the return value of `(current-hierarchy)` is used. 

`generic?`

Returns `#t` if the object is a generic function, and `#f` otherwise.

`generic-specialized-on? <generic> <predicate>...`

`generic-has-specialization? <generic> <predicate>...`

`generic-add-method! <generic> <predicates>`








Example
-------

```scheme

;; elt - retrieve an object from a container by integer index
(define-generic (elt container index)
  (error "elt: no such method for" (list container index))

;; Sequences are defined by having a specialization for elt. 
(define (sequence? obj)
  (generic-accepts? elt (list obj #f))
  
(define (refable? obj)
  (and (generic-accepts? ref      (list obj #f))
       (generic-accepts? set-ref! (list obj #f #f)))

(define-method (elt (coll list?) idx) 
  (list-ref coll idx))
(subsume! sequence? list?)

(define-method (elt (coll vector?) idx)     (vector-ref coll idx))
(subsume! sequence? vector?)

(define-method (elt (coll bytevector?) idx) (bytevector-u8-ref coll idx))
(subsume! sequence? bytevector?)

(define-method (head (seq sequence?)) 
  (elt seq 0)) 
  
(define-record-type <one-hot-vector>
  ())
```


