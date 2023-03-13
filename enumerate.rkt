#lang racket
 (require list-utils)


(define (val alist) 
  display (enumerate alist))

(val '(1 7 3 8 3 6)) 