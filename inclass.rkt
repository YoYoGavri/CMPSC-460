#lang plait

(define (gt10 x) (> x 10))

;(filter gt10 '(1 1 22 33 54 1 1 5))

(define (me-filter func lst out)
  (if (empty? lst)
      out
      (if (func (first lst))
          (me-filter func (rest lst) (append out (list (first lst))))
          (me-filter func (rest lst) out))))

(trace gt10)

(me-filter gt10 '(1 2 3 11 13 15) empty)