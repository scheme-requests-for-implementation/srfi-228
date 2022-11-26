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
  (if (null? comparators)
      one-comparator
      (let* ((type-tests
              (delete-duplicates
               (map comparator-type-test-predicate comparators)
               eq?))
             (type-test
              (lambda (val)
                (every (lambda (test) (test val)) type-tests))))
        (make-comparator
         type-test
         (lambda (a b)
           (every (lambda (cmp)
                    ((comparator-equality-predicate cmp) a b))
                  comparators))
         (if (every comparator-ordered? comparators)
             (lambda (a b)
               (let loop ((cmps comparators))
                 (cond ((null? cmps) #f)
                       (((comparator-ordering-predicate (car cmps)) a b) #t)
                       (((comparator-equality-predicate (car cmps)) a b) (loop (cdr cmps)))
                       (else #f))))
             #f)
         (if (every comparator-hashable? comparators)
             (lambda (x)
               (fold bitwise-xor
                     0
                     (map (lambda (cmp)
                            ((comparator-hash-function cmp) x))
                          comparators)))
             #f)))))

(define (%sum-comparator-for comparators a b)
  (define (type-tests-for? x)
    (lambda (cmp) ((comparator-type-test-predicate cmp) x)))
  (let ((a-cmp (find-tail (type-tests-for? a) comparators)))
    (if (and a-cmp ((comparator-type-test-predicate (car a-cmp)) b))
        a-cmp
        (let ((b-cmp (find-tail (type-tests-for? b) comparators)))
          (if (and b-cmp ((comparator-type-test-predicate (car b-cmp)) a))
              b-cmp
              #f)))))

(define (make-sum-comparator . comparators)
  (if (null? comparators)
      zero-comparator
      (make-comparator
       (lambda (x)
         (any
          (lambda (cmp)
            ((comparator-type-test-predicate cmp) x))
          comparators))
       (lambda (a b)
         (let ((cmp (%sum-comparator-for comparators a b)))
           (if cmp
               ((comparator-equality-predicate (car cmp)) a b)
               #f)))
       (if (every comparator-ordered? comparators)
           (lambda (a b)
             (let ((cmp (%sum-comparator-for comparators a b)))
               (if cmp
                   ((comparator-ordering-predicate (car cmp)) a b)
                   (let ((a-cmp (%sum-comparator-for comparators a a))
                         (b-cmp (%sum-comparator-for comparators b b)))
                     (>= (length a-cmp) (length b-cmp))))))
           #f)
       (if (every comparator-hashable? comparators)
           (lambda (x)
             (let ((cmp (%sum-comparator-for comparators x x)))
               ((comparator-hash-function (car cmp)) x)))
           #f))))

(define one-comparator
  (make-comparator
   (lambda (x) #t)
   (lambda (a b) #t)
   (lambda (a b) #f)
   (lambda (x) 0)))

(define zero-comparator
  (make-comparator
   (lambda (x) #f)
   (lambda (a b) (error "can't compare" a b))
   (lambda (a b) (error "can't compare" a b))
   (lambda (x) (error "can't hash" x))))
