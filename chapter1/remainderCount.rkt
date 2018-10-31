#lang racket
(define (remainderCount a b)
  (display "yes\n")
  (remainder a b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainderCount a b))))

(gcd 206 40)
