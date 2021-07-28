
Predicate-based generic procedures
==================================

by Ben Davenport-Ray, Others

Abstract
--------

This SRFI defines mechanisms for creating procedures which change their behavior depending on the values of the parameters, distinguished by one or more predicates. This allows for ad-hoc polymorphism based on type, a specific value, range of values, or any other constraint that can be expressed using a predicate.

Issues
------

- Should a pre-defined default hierarchy exist?
	- I think it might be a good idea, if we can agree on it. If not, we don't have to provide it.
	- Seperate hierarchy instances may help this problem of agreeing on hierarchies. If you don't like a particular hierarchy, you are free to define your own, but you have to do some of the work.
- What should we actually call the parent-child relation?
	- Subsumption sounds extra fancy, but it's a bit weird, since for me it sounds like if a predicate subsumes another it replaces it.
	- Parent or Child is fine, but we don't want to take these concepts for ourselves, right?
- When a predicate function is re-defined, should we re-define all relations as well?
	- That may not be too bad, depending on how much we cache the relations.

Rationale
---------

One of the percieved issues with Scheme is it's inability to create generic algorithms that take a wide variety of data as arguments: for example, conceptually `list-ref`, `vector-ref`, and `string-ref` are the same concept, that of looking up a value in a datum by index. 

_The author makes no claims about the rationality of this draft._

Specification
-------------

### Hierarchies

In order to have a useful ordering for methods, there must be an
established ordering of predicates which designate types, which we will call a _predicate hierarchy_, or just a _hierarchy_ for short. 

A hierarchy is a disjoint type.

We say a predicate specializes another predicate when methods that are defined on
the first predicate should be given higher priority than methods defined
on the second.

##### `(current-hierarchy) -> hierarchy?`

##### `(with-hierarchy <hierarchy> <body>) -> ...`

Evaluates the contents of `body` where the current hierarchy (as determined by `current-hierarchy`) is set to the given `hierarchy`.

##### `(make-predicate-hierarchy) -> hierarchy?`

Create a new predicate hierarchy. 

##### `(derive! <pred> <parent-pred> [<hierarchy>])`

Ensure that _pred_ is registered to specialize _subpred_. It is an error if _pred_ already specializes _subpred_.

##### `(derived? <pred> <other> [<hierarchy>]) -> boolean?`

Returns `#t` if _pred_ subsumes _other_, and `#f` otherwise.

##### `(derivations <pred> [<hierarchy>]) -> list?`

Returns a list of all predicates that have been registered to be subsumed in _pred_. If no subsumptions of _pred_ are registered, the empty list is returned.

#### Syntax

##### `(define-subsume ((<name> ... <subsumed>) ...args) ...body)`

Creates a procedure with the argument list _args_ and the body _body_,
binds it to _name_, and establishes that the newly defined procedure
subsumes the procedures named in _subsumed_.

If a procedure of the same name already exists, all predicates that have
been defined as subsuming the previous definition of name are defined
as subsuming the new definition as well.

### Methods

##### `(make-method [<hierarchy>] <predicates> <body>) -> method?`

> What is a good order here? 

##### `(method-hierarchy <method>) -> hierarchy?`

##### `(method-specializers <method>) -> list?`

##### `(method-body <method>) -> procedure?`

### Generic procedures

##### `(make-generic <symbol> [<hierarchy>]) -> generic?`

Define a new generic function with the name _symbol_. If no hierarchy is given, then the return value of `(current-hierarchy)` is used. 

##### `(generic? <object>) -> boolean?`

Returns `#t` if the object is a generic function, and `#f` otherwise.

##### `(generic-apply <generic> <list-of-args>) -> ...`

Like `apply`, but calls a `generic` instead.

##### `(generic-specialized-on? <generic> <object>...) -> boolean?`

Returns `#t` if the generic function has an explicit specialization for the objects, and `#f` otherwise.

##### `(generic-has-specialization? <generic> <predicate>...) -> boolean?`

Returns `#t` if the generic function has an explicit specialization that specializes using the specific predicates, and `#f` otherwise.

##### `(generic-add-method! <generic> <predicates> <body>)`

#### Syntax

##### `(define-generic <name> [<hierarchy>])`

Defines a new generic function with the name and using the given hierarchy. 

##### `(define-method (name parameters...) body ...)`

Defines a new specialization for the generic function `name`. Each parameter may have a specializer if the 

### Ancillary procedures

##### `(object? . xs) -> boolean?` 

Returns `#t` if an object is a datum. Useful for having a "base" type in a predicate hierarchy.

## Interaction with other SRFIs

Examples
--------

```scheme
;; elt - retrieve an object from a container by integer index
(define-generic elt)

(define-method (elt (list? l) idx) (list-ref l idx))
(define-method (elt (vector? v) idx) (vector-ref v idx))
(define-method (elt (bytevector? bv) idx) (bytevector-u8-ref bv idx))
```




