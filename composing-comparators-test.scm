(import (chibi test))

(define-record-type <date>
  (make-date year month day)
  date?
  (year date-year)
  (month date-month)
  (day date-day))



