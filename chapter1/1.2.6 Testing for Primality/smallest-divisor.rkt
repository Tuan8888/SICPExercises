;; 求输入的最小因子
#lang racket
(provide (all-defined-out))
 
;; 平方
(define (square x)
  (* x x))

;; 求最小因子
(define (smallest-divisor n)
  (find-divisor n 2))

;; 迭代
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

;; 判断是否可以除尽
(define (divides? a b)
  (= (remainder b a) 0))


