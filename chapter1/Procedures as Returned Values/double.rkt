;; Exercise 1.41
;; 执行两次某函数
#lang racket

;; 执行两次输入函数
(define (double f)
  (lambda(x) (f (f x))))

;; inc
(define (inc x)
  (+ x 1))

(((double (double double)) inc) 5)
