;; Exercise 2.4
;; cons的另一种定义方法
#lang racket

;; cons
(define (cons x y)
  (lambda (m) (m x y)))

;; selector
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

(define c (cons 3 4))
(car c)
(cdr c)
