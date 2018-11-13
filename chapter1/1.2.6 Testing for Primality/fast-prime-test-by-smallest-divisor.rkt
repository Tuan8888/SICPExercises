;; Exercise 1.23
;; 改进版通过求解最小因数，判断是否是质数
#lang racket
(provide (all-defined-out))
(require "fast-smallest-divisor.rkt")

;; 最小因数等于本身，证明为质数
(define (fast-prime-test-by-smallest-divisor x)
  (= (fast-smallest-divisor x) x))


