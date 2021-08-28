(define-library (srfi 228)
  (import (srfi 1)
          (srfi 128)
          (srfi 151)
          (srfi 158))
  (export make-wrapper-comparator
          make-composed-comparator
          compose-comparator
          comparison-procedures)

  (include "composing-comparators.scm"))
