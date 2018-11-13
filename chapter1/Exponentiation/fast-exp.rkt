;; Exercise 1.16
#lang racket
(provide (all-defined-out))

;; 迭代求幂
(define (exp-iter a b n)
  (if (= n 0) 
      (if (even n) (exp-iter a (square b) (/ n 2))
          (exp-iter (* a b) b (- n 1)))))

;; 快速求幂
(define (fast-exp b n)
  (exp-iter 1 b n))

;; 判断是否为偶数
(define (even n)
  (= (remainder n 2) 0))

;; 平方
(define (square x)
  (* x x))
