;; Exercise 2.31
;; 对树里的所有元素采取某种操作
#lang racket

;; map
(define (tree-map proc t)
  (cond ((null? t) null)
        ((pair? t) (cons (tree-map proc (car t)) (tree-map proc (cdr t))))
        (else (proc t))))

(define t
  (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(tree-map (lambda (x) (* x x)) t)
