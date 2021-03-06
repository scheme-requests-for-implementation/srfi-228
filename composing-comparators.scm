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

(define (make-product-comparator . comparators)
  (let* ((type-tests
          (delete-duplicates
           (map comparator-type-test-predicate comparators)
           eq?))
         (type-test
          (lambda (val)
            (every (lambda (test) (test val)) type-tests))))
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
           (let loop ((cmps comparators))
             (cond ((null? cmps) #f)
                   (((comparator-ordering-predicate (car cmps)) a b) #t)
                   (((comparator-equality-predicate (car cmps)) a b) (loop (cdr cmps)))
                   (else #f))))
         #f)
     (if (every comparator-hash-function comparators)
         (lambda (x)
           (fold bitwise-xor
                 0
                 (map (lambda (cmp)
                        ((comparator-hash-function cmp) x))
                      comparators)))
         #f))))

(define (comparator-index comparators val)
  (list-index
   (lambda (cmp)
     ((comparator-type-test-predicate cmp) val))
   comparators))

(define (make-sum-comparator . comparators)
  (make-comparator
   (lambda (x)
     (any
      (lambda (cmp)
        ((comparator-type-test-predicate cmp) x))
      comparators))
   (if (every comparator-equality-predicate comparators)
       (lambda (a b)
         (let ((a-cmp-idx (comparator-index comparators a))
               (b-cmp-idx (comparator-index comparators b)))
           (if (not (= a-cmp-idx b-cmp-idx))
               #f
               (let ((cmp (list-ref comparators a-cmp-idx)))
                 ((comparator-equality-predicate cmp) a b)))))
       #f)
   (if (every comparator-ordering-predicate comparators)
       (lambda (a b)
         (let ((a-cmp-idx (comparator-index comparators a))
               (b-cmp-idx (comparator-index comparators b)))
           (cond ((< a-cmp-idx b-cmp-idx) #t)
                 ((> a-cmp-idx b-cmp-idx) #f)
                 (else
                  (let ((cmp (list-ref comparators a-cmp-idx)))
                    ((comparator-ordering-predicate cmp) a b))))))
       #f)
   (if (every comparator-hash-function comparators)
       (lambda (x)
         (let ((cmp (find (lambda (cmp) ((comparator-type-test-predicate cmp) x))
                          comparators)))
           ((comparator-hash-function cmp) x)))
       #f)))

(define (comparison-procedures comparator)
  (values
   (lambda args (apply <? comparator args))
   (lambda args (apply <=? comparator args))
   (lambda args (apply =? comparator args))
   (lambda args (apply >=? comparator args))
   (lambda args (apply >? comparator args))))
