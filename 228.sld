(define-library (srfi 228) ;; SRFI number reserved for, but not yet formally
                           ;; assigned to this library.
  (import (srfi 1)
          (srfi 128)
          (srfi 151)
          (srfi 158))
  (export make-wrapper-comparator
          make-composed-comparator
          compose-comparator)

  (include "composing-comparators.scm"))
