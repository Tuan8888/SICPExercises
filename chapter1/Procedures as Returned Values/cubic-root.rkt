;; Exercise 1.40
;; 牛顿法求解立方方程
#lang racket
(require "newtons-method.rkt")

;; 求解立方方程
(define (cubic-root a b c)
  ;; 定义cubic函数
  (define (cubic a b c)
    (lambda(x)
      (+ (* x x x)
         (* a x x)
         (* b x)
         c)))
  ;; 求解
  (newtons-method (cubic a b c) 1))
