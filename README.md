
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
	- Seperate hierarchy instances may help this problem of agreeing on hierarchies. If you don't like a particular hierarchy, you are free to define your own. Copying branches of the hierarchy should be easy with the right introspection primitives.

- What should we actually call the parent-child relation?
	- Subsumption sounds extra fancy, but it's a bit weird, since for me it sounds like if a predicate subsumes another it replaces it.
	- Parent or Child is fine, but we don't want to take these concepts for ourselves, right?

- When a predicate function is re-defined, should we re-define all relations as well?
	- Maybe, but probably with convenience syntax.

- What about `call-next-method`?
	- Definitely unsure how to implement this. Maybe we can do some continuation nonsense in order to resolve this?

- Naming conventions?

Rationale
---------

One of the percieved issues with Scheme is it's inability to create generic algorithms that take a wide variety of data as arguments: for example, conceptually `list-ref`, `vector-ref`, and `string-ref` are the same concept, that of looking up a value in a datum by index. 

_The author makes no claims about the rationality of this draft._

Specification
-------------

### Hierarchies

In order to have a useful ordering for methods, there must be an
established ordering of predicates which designate types, which we will call a _predicate hierarchy_, or just a _hierarchy_ for short. 

We say a predicate specializes another predicate when methods that are defined on
the first predicate should be given higher priority than methods defined
on the second.

A hierarchy is a disjoint type.

#### Constructors and recognizers

##### `(hierarchy? <object>) -> boolean?`

Returns `#t` if the object is a predicate hierarchy, `#f` otherwise.

##### `(make-hierarchy) -> hierarchy?`

Create a new predicate hierarchy. 

#### Hierarchy operations

##### `(derive! <pred> <parent-pred> [<hierarchy>])`

Ensure that _pred_ is registered to specialize _subpred_. It is an error if _pred_ already specializes _subpred_.

##### `(derived? <pred> <other> [<hierarchy>]) -> boolean?`

Returns `#t` if _pred_ subsumes _other_, and `#f` otherwise.

##### `(derivations <pred> [<hierarchy>]) -> list?`

Returns a list of all predicates that have been registered to be subsumed in _pred_. If no subsumptions of _pred_ are registered, the empty list is returned.

#### The current hierarchy

##### `(current-hierarchy) -> hierarchy?`

A parameter that holds the current hierarchy. W

##### `(with-hierarchy <hierarchy> <body>) -> ...`

Evaluates the contents of `body` where the current hierarchy (as determined by `current-hierarchy`) is set to the given `hierarchy`.

#### Syntax

##### `(define-subsume ((<name> ... <subsumed>) ...args) ...body)`

Creates a procedure with the argument list _args_ and the body _body_,
binds it to _name_, and establishes that the newly defined procedure
subsumes the procedures named in _subsumed_.

If a procedure of the same name already exists, all predicates that have
been defined as subsuming the previous definition of name are defined
as subsuming the new definition as well.

### Methods

> Should `method?` be a specialization of `procedure?`?

##### `(make-method [<hierarchy>] <predicates> <body>) -> method?`

> What is a good order here? 

##### `(method-hierarchy <method>) -> hierarchy?`

##### `(method-specializers <method>) -> list?`

##### `(method-body <method>) -> procedure?`

### Generic procedures

> Should `generic?` be a specialization of `procedure?`?

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

##### `(generic-add-method! <generic> <method>)`

Adds a new method to the generic.

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

A possible hierarchy:

```scheme
(define standard-hierarchy (make-hierarchy))

;;;; The core recognizers.
(derive! boolean? object? standard-hierarchy)
(derive! char? object? standard-hierarchy)
(derive! procedure? object? standard-hierarchy)
(derive! string? object? standard-hierarchy)
(derive! symbol? object? standard-hierarchy)
(derive! vector? object? standard-hierarchy)
(derive! bytevector? object? standard-hierarchy)
(derive! port? object? standard-hierarchy)

(derive! input-port? port? standard-hierarchy)
(derive! output-port? port? standard-hierarchy)

;;;; Pair and null should be preferred over list. This is usually what you want.
(derive! list? object? standard-hierarchy)
(derive! pair? list? standard-hierarchy)
(derive! null? list? standard-hierarchy)

;;;; Adding generics and methods under procedures
(derive! generic? procedure? standard-hierarchy)
(derive! method? procedure? standard-hierarchy)
(derive! hierarchy? object? standard-hierarchy)
```
