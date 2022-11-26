(define-library (srfi 228)
  (import (scheme base)
	  (srfi 1)
          (srfi 128)
	  (srfi 151))
  (export make-wrapper-comparator
          make-product-comparator
          make-sum-comparator

          one-comparator
          zero-comparator)
  (include "srfi-228.scm"))
