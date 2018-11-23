;; Exercise 2.39
;; 对树里的所有元素取平方
#lang racket

;; 对树里所有元素取平方
(define (square-tree t)
  (cond ((null? t) null)
        ((pair? t) (cons (square-tree (car t)) (square-tree (cdr t))))
        (else (square t))))

;; 平方
(define (square x)
  (* x x))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))


