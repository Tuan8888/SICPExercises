;; 通过求解最小因数，判断是否是质数
#lang racket
(provide (all-defined-out))
(require "smallest-divisor.rkt")

;; 最小因数等于本身，证明为质数
(define (prime-test-by-smallest-divisor x)
  (and (> x 1) (= (smallest-divisor x) x)))


