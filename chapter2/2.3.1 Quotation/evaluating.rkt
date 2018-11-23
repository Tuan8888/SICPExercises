;; Exercise 2.53
;; 计算表达式
#lang racket

(list 'a 'b 'c)

(list (list 'george))

(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))

(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

(car (list (list 1 2) (list 3 4)))
