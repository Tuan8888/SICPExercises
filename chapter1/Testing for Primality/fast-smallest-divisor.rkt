;; Exercise 1.23
;; 改进的求最小因子函数
;; 当不能被2整除，跳过所有偶数因子的尝试
#lang racket
(provide (all-defined-out))

;; 迭代搜索最小因子
(define (fast-divisor-iter n divisor)
  (cond ((> (* divisor divisor) n) n)
        ((divides? n divisor) divisor)
        (else (fast-divisor-iter n (next divisor)))))

;; 改进的求解最小因子函数
(define (fast-smallest-divisor n)
  (fast-divisor-iter n 2))

;; 判断是否可以除尽
(define (divides? a b)
  (= (remainder a b) 0))

(define (next divisor)
  (if (= divisor 2)
      3
      (+ divisor 2)))

