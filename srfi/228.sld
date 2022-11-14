(define-library (srfi 228)
  (import (srfi 1)
          (srfi 128))
  (export make-wrapper-comparator
          make-product-comparator
          make-sum-comparator
          comparison-procedures)
  (include "srfi-228.scm"))
