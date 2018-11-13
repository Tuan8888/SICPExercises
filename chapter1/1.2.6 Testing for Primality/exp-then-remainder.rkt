;; Exercise 1.25
;; 计算幂然后取模
#lang racket
(require "../Exponentiation/fast-exp.rkt")

;; 先计算完幂，最后取模一次
(define (expmod base exp m)
  (remainder (fast-exp base exp) m))

(expmod 5 3 8)
(expmod 100000000 10000000000 7)
