# Primitives for Composing Comparators

## Specification

### `(make-wrapper-comparator type-test unwrap contents-comparator)` (Procedure)

Returns a comparator which compares values satisfying the predicate `type-test` by first calling the given `unwrap` procedure on them, then comparing the output of that procedure with the given `contents-comparator.` The hash function of the wrapper comparator returns the same value as the `contents-comparator` run on the unwrapped value.

### `(make-composed-comparator type-test comparator ...)` (Procedure)

Returns a comparator which compares values satisfying the given predicate `type-test` by comparing them with each of the given comparators in turn, left to right, and returning the result of the first non-equal comparison. If all the given comparators consider two values equal, the composed comparator also considers them equal. The hash function of the composed comparator hashes together the results of all the given comparators in an implementation-defined way.

### `(compose-comparator type-test (unwrap comparator) ...)` (Syntax)

Expands to a form which returns a comparator which compares values satisfying the given predicate `type-test` by running in turn, left to right, wrapper comparators made out of the given `unwrap` and `comparator`, according to the rules for `make-composed-comparator`. `comparator` may be omitted from each form, in which case the SRFI 128 default comparator is used.

This is equivalent to using the procedural forms `make-composed-comparator` and `make-wrapper-comparator` together, but can be slightly more efficient because it only needs to run the given `type-test` predicate once, whereas composing a number of `make-wrapper-comparator`s would run each wrapper comparator’s type test once for each comparator in the composed comparator.

## Examples

`make-pair-comparator` from SRFI 128 can be implemented in terms of this library as follows:

```scheme
(define (make-pair-comparator car-comparator cdr-comparator)
  (compose-comparator pair? (car car-comparator) (cdr cdr-comparator)))
```

If one has defined a date record type consisting of year, month, and day fields such as:

```scheme
(define-record-type <date>
  (make-date year month day)
  date?
  (year date-year)
  (month date-month)
  (day date-day))
```

these can be correctly compared by a comparator defined by:

```scheme
(compose-comparator date?
  (date-year)  ;; Equivalent to (date-year (make-default-comparator))
  (date-month)
  (date-day))
```

More advanced use is also possible, such as with SRFI 209 enums:

```scheme
(define flavours (make-enum-type '(vanilla chocolate strawberry)))
(define toppings (make-enum-type '(golden-syrup
                                   strawberry-syrup
                                   chocolate-sprinkles
                                   rainbow-sprinkles
                                   fudge)))

(define flavour-comparator (make-enum-comparator flavours))
(define topping-comparator (make-enum-comparator toppings))

(define-record-type <ice-cream>
  (make-ice-cream flavour toppings price)
  ice-cream?
  (flavour ice-cream-flavour)
  (toppings ice-cream-toppings)
  (price ice-cream-price))

;; e.g.
(make-ice-cream (enum-name->enum 'strawberry)
                (map enum-name->enum '(golden-syrup chocolate-sprinkles))
                4.50)

;; Sorts ice creams by price, then by flavour, then by their list of toppings.
(define ice-cream-comparator
  (compose-comparator ice-cream?
    (ice-cream-price)
    (ice-cream-flavour flavour-comparator)
    (ice-cream-toppings 
      (make-list-comparator
        topping-comparator
        (lambda (x) (and (list? x) (all enum? x)))
        null? car cdr))))
```

(This perhaps shows that `make-list-comparator` could also do with some convenience wrapper for when the last three arguments are known to be `null?`, `car`, and `cdr`.)
