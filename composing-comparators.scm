(define (make-wrapper-comparator type-test unwrap contents-comparator)
  (make-comparator
   type-test
   (lambda (a b)
     ((comparator-equality-predicate contents-comparator)
      (unwrap a)
      (unwrap b)))
   (if (comparator-ordering-predicate contents-comparator)
       (lambda (a b)
         ((comparator-ordering-predicate contents-comparator)
          (unwrap a)
          (unwrap b)))
       #f)
  (if (comparator-hash-function contents-comparator)
      (lambda (x)
        ((comparator-hash-function contents-comparator) x))
      #f)))

(define (make-composed-comparator type-test . comparators)
  (make-comparator
   type-test
   (if (every comparator-equality-predicate comparators)
       (lambda (a b)
         (every (lambda (cmp)
                  ((comparator-equality-predicate cmp) a b))
                comparators))
       #f)
   (if (every comparator-ordering-predicate comparators)
       (lambda (a b)
         (let ((gen (list->generator comparators)))
           (let loop ((cmp (gen)))
             (cond ((eof-object? cmp) #f)
                   (((comparator-ordering-predicate cmp) a b) #t)
                   (((comparator-equality-predicate cmp) a b) (loop (gen)))
                   (else #f)))))
       #f)
   (if (every comparator-hash-function comparators)
       (lambda (x)
         (generator-fold bitwise-xor
                         0
                         (gmap (lambda (cmp)
                                 ((comparator-hash-function cmp) x))
                               (list->generator comparators))))
       #f)))

(define-syntax compose-comparator
  (syntax-rules ()
    ((_ type-test (unwrap . more) ...)
     (make-composed-comparator
      type-test
      (let-values (((unwrap cmp) (compose-comparator-form unwrap . more)))
        (make-wrapper-comparator
         (comparator-type-test-predicate cmp)
         unwrap
         cmp)) ...))))

(define-syntax compose-comparator-form
  ;; Using this submacro enables enforcement of the correct form with
  ;; moderately more useful syntax errors than doing it the SRFI 9
  ;; way, at least within the limited bounds of what one can do for
  ;; that in syntax-rules.
  (syntax-rules ()
    ((_ unwrap) (compose-comparator-form unwrap (make-default-comparator)))
    ((_ unwrap cmp)
     (values
      unwrap cmp))))

(define (comparison-procedures comparator)
  (values
   (lambda args (apply <? comparator args))
   (lambda args (apply <=? comparator args))
   (lambda args (apply =? comparator args))
   (lambda args (apply >=? comparator args))
   (lambda args (apply >? comparator args))))
