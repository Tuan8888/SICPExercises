;; Exercise 1.40
;; 牛顿法求解立方根
#lang racket
(require "newtons-method.rkt")

(define (cubic-root a b c)
  (define (cubic a b c)
    (+ (* x x x)n
       (* a x x)
       (* b x)
       c))
  (newtons-method (cubic a b c) 1))

(cubic-root 1 1 1)
