;; Exercise 1.33
;; 所有与n互质且小于n的正整数累乘
#lang racket
(require "filtered-accumulate.rkt")

;; 欧几里得算法
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; n以内，与n互质的正整数乘积
(define (prime-to-n-accumulate n)
  (define (filter? x)
    (= (gcd x n) 1))
  (define (next x)
    (+ x 1))
  (filtered-accumulate * 1 identity 1 next n filter?))

(prime-to-n-accumulate 13)
