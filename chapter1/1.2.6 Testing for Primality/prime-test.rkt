;; Exercise 1.21
;; 检测一个数是否为质数
#lang racket
(provide (all-defined-out))

;; 求最小因子
(define (expmod base n m)
  (cond ((= n 0) 1)
        ((odd? n) (remainder (* base (expmod base (- n 1) m)) m))  
        (else (remainder (square (expmod base (/ n 2) m)) m))))

(define (square x)
  (* x x))

(define (odd? n)
  (= (remainder n 2) 1))

(define (prime? n)
  (= (SmallestDivisor n) n))
