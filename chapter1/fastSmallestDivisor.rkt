#lang racket
(provide (all-defined-out))
(define (fastSmallestDivisor n)
  (findDivisorIter n 2))

(define (findDivisorIter n divisor)
  (cond ((> (* divisor divisor) n) n)
        ((isDivisor? n divisor) divisor)
        (else (findDivisorIter n (next divisor)))))

(define (isDivisor? n divisor)
  (= (remainder n divisor) 0))

(define (next divisor)
  (if (= divisor 2)
      3
      (+ divisor 2)))


