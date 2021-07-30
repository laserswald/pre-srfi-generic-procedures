
Predicate-based generic procedures
==================================

by Ben Davenport-Ray, Others

Abstract
--------

This SRFI defines mechanisms for creating procedures which change their
behavior depending on the values of the parameters, distinguished by one
or more predicates. This allows for ad-hoc polymorphism based on type,
a specific value, range of values, or any other constraint that can be
expressed using a predicate.

Issues
------

- Should a pre-defined default hierarchy exist?
	- I think it might be a good idea, if we can agree on it. If not, we don't have to provide it.
	- Seperate hierarchy instances may help this problem of agreeing on hierarchies. If you don't like a particular hierarchy, you are free to define your own. Copying branches of the hierarchy should be easy with the right introspection primitives.

- What should we actually call the parent-child relation?
	- Subsumption sounds extra fancy, but it's a bit weird, since for me it sounds like if a predicate subsumes another it replaces it.
	- Parent or Child is fine, but we don't want to take these concepts for ourselves, right?

- When a predicate function is re-defined, should we re-define all relations as well?
	- Not automatically, but we should define the primitives that allow this, at least.

- Naming conventions?

Rationale
---------

One of the percieved issues with Scheme is it's lack of built-in
polymorphic procedurs; that is, the ability to call a single function
that symbolizes a single operation on a wide variety of types, with
differing effects depending on the value in question.

Polymorphism is a powerful design tool, and there are several ways to
tackle it. This SRFI specifies a version of the multiple dispatch style
of polymorphic operations, with inspiration taken from Common Lisp's CLOS,
Clojure's Multimethods, and others.

### Differences from other generic function systems

Unlike other programming languages, Scheme does not explicitly have a
'type' construct. Instead, Scheme uses predicates to differentiate the
type of values. In the same way, this SRFI uses predicates in order to
differentiate values for each method. This allows for flexibility, but
also comes with a cost: which predicate should be preferred over another?

We solve this problem by adding a new object, a predicate hierarchy,
to solve the ambiguity. Predicates may be registered with the hierarchy
in order to clarify which method should be preferred if a value matches
the constraints of more than one method. For convenience, there is a 
parameter defined that will point to a default hierarchy, and most operations
involving hierarchies will default to this one if a specific hierarchy is not
specified.

This stands in contrast to Common Lisp's CLOS, which not only define
the mechanisms for generic operations, but also a system of classes that
define aggregate types. However, R7RS already has a way to define
new aggregates using `define-record-type`, and such a system of classes
is not, strictly speaking, necessary to gain utility from the generic
function idea. Adding the functionality for classes is left to a future SRFI.

### Terminology

We say a predicate specializes another predicate when methods that are
defined on the first predicate should be given higher priority than
methods defined on the second.

_The author makes no claims about the rationality of this draft._

Specification
-------------

### Hierarchies

A hierarchy is a disjoint type.

#### Constructors and recognizers

##### `(make-hierarchy) -> hierarchy?`

Create a new predicate hierarchy. 

##### `(hierarchy? <object>) -> boolean?`

Returns `#t` if the object is a predicate hierarchy, `#f` otherwise.

#### Hierarchy operations

For each of these operations that take an optional hierarchy, if the hierarchy is not specified, then the hierarchy affected is the value returned by `(current-hierarchy)`.

##### `(derive! <pred> <parent-pred> [<hierarchy>])`

Ensure that _pred_ is registered to specialize _subpred_. It is an error if _pred_ already specializes _subpred_.

##### `(derived? <pred> <other> [<hierarchy>]) -> boolean?`

Returns `#t` if _pred_ subsumes _other_, and `#f` otherwise.

##### `(derivations <pred> [<hierarchy>]) -> list?`

Returns a list of all predicates that have been registered to be subsumed in _pred_. If no subsumptions of _pred_ are registered, the empty list is returned.

#### The current hierarchy

##### `(current-hierarchy) -> hierarchy?`

A parameter that holds the current hierarchy.

##### `(with-hierarchy <hierarchy> <body>) -> ...`

Evaluates the contents of `body` where the current hierarchy (as determined by `current-hierarchy`) is set to the given `hierarchy`.

### Methods

A method is a group of specializing predicates, a reference to the
hierarchy used to order them, and the procedure that is invoked when the
method is matched by the invocation of a generic procedure. 

##### `(make-method <predicates> <body> [<hierarchy>]) -> method?`

Creates and returns a new method with the predicates, body and
hierarchy. 

If the hierarchy is not specified, it defaults to the value
returned from `(current-hierarchy)`.  It is an error to use predicates
for specialization that have not been registered in the given hierarchy.

##### `(method? <object>) -> boolean?`

Returns `#t` if the object is a method, and `#f` otherwise.

##### `(method-hierarchy <method>) -> hierarchy?`

Returns the hierarchy that the method was defined under.

##### `(method-specializers <method>) -> list?`

Returns the list of specializing procedures for the method.

##### `(method-body <method>) -> procedure?`

Returns the procedure that will be called when the method is matched.

##### `(method-resolution<? <method> ...) -> boolean?`

### Generic procedures

##### `(make-generic <symbol> [<hierarchy>]) -> generic?`

Define a new generic function with the name _symbol_. If no hierarchy is given, then the return value of `(current-hierarchy)` is used. 

##### `(generic? <object>) -> boolean?`

Returns `#t` if the object is a generic function, and `#f` otherwise.

##### `(generic-specialized-on? <generic> <object>...) -> boolean?`

Returns `#t` if the generic function has an explicit specialization for the objects, and `#f` otherwise.

##### `(generic-has-specialization? <generic> <predicate>...) -> boolean?`

Returns `#t` if the generic function has an explicit specialization that specializes using the specific predicates, and `#f` otherwise.

##### `(generic-add-method! <generic> <method>)`

Adds a new method to the generic.

##### `(generic-remove-method! <generic> <method>)`

Removes the method from the generic. If the method was not registered with the generic, this function does nothing.

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
