#lang plait

(define-type Tree 
  (leaf [val : Number]) 
  (node [val : Number] 
        [left : Tree] 
        [right : Tree]))

;Part 1 - Super Jank, couldn't add 3 values, had to do 2 + 1
(define (sum tree)
  (cond [(leaf? tree) (leaf-val tree)]
        [(node? tree) (+(+ (node-val tree) (sum (node-left tree))) (sum (node-right tree)))]))

(test (sum (node 5 (leaf 6) (leaf 7))) 18)
(test (sum (node 2 (leaf 7) (leaf 4))) 13)
(test (sum (node 4 (leaf 27) (leaf 8))) 39)

;Part 2 - Pain
(define (negate tree)
  (cond [(leaf? tree) (leaf (* (leaf-val tree) -1))]
        [(node? tree) (node (* (node-val tree) -1) (negate (node-left tree)) (negate (node-right tree)))]))

(test (negate (node 5 (leaf 6) (leaf 7))) (node -5 (leaf -6) (leaf -7)))

;Part 3 - (define (pain) "Very much pain, yes")
(define (contains? tree number)
  (cond
    [(leaf? tree) (= (leaf-val tree) number)]
    [(node? tree) (or (= (node-val tree) number) (contains? (node-left tree) number) (contains? (node-right tree) number))]))

(test (contains? (node 5 (leaf 6) (leaf 7)) 5) #t)
(test (contains? (node 5 (leaf 6) (leaf 7)) 6) #t)
(test (contains? (node 5 (leaf 6) (leaf 7)) 7) #t)


;Part 4 - Uh...It works I think?
(define (big-leaves? tree)
    (bigger-leaves? tree 0))

(define (bigger-leaves? tree sum)
  (cond [(leaf? tree) (> (leaf-val tree) sum)]
        [(node? tree) (and (bigger-leaves? (node-left tree) (+ sum (node-val tree)))
                           (bigger-leaves? (node-right tree) (+ sum (node-val tree))))]))

(test (big-leaves? (node 5 (leaf 6) (leaf 7))) #t)
(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) #f)

;Part 5 - Go Birds
(define (positive-trees? lst)
  (cond [(empty? lst) #t]
        [(leaf? (first lst)) (and (> (sum (first lst)) 0) (positive-trees? (rest lst)))]
        [(node? (first lst)) (and (> (sum (first lst)) 0) (positive-trees? (rest lst)))]))

(test (positive-trees? (cons (leaf 6) empty)) #t)
(test (positive-trees? (cons (leaf -6) empty)) #f)
(test (positive-trees? (cons (node 1 (leaf 6) (leaf -6)) empty)) #t)
(test (positive-trees? (cons (node 1 (leaf 6) (leaf -6)) (cons (node 0 (leaf 0) (leaf 1))  empty))) #t)
(test (positive-trees? (cons (node -1 (leaf 6) (leaf -6)) (cons (node 0 (leaf 0) (leaf 1)) empty))) #f)