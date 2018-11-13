;; Exercise 1.42
;; 依次执行函数
#lang racket
(provide (all-defined-out))

;; 返回复合函数
(define (compose f g)
  (lambda(x) (f (g x))))

;; inc
(define (inc x)
  (+ x 1))

;; square
(define (square x)
  (* x x))
