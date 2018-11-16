;; Exercise 2.28
;; 从左到右，返回一棵树的所有叶节点
#lang racket

;; 返回叶节点
(define (fringe tree)
  (cond ((null? tree) null)
        ((pair? tree) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (list tree))))

(define x (list (list 1 2) (list 3 4)))
(fringe (list x x))
