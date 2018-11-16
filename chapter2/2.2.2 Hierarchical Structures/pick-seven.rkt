;; Exercise 2.25
;; 获取list中的7
#lang racket

(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;; 获取7
(car (cdr (car (cdr (cdr l1)))))
(car (car l2))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)
