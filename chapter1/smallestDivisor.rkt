#lang racket
(define (smallestDivisor n)
  (findDivisorIter n 2))

(define (findDivisorIter n divisor)
  (cond ((> (* divisor divisor) n) n)
        ((isDivisor? n divisor) divisor)
        (else (findDivisorIter n (+ divisor 1)))))

(define (isDivisor? n divisor)
  (= (remainder n divisor) 0))

(smallestDivisor 199)
(smallestDivisor 1999)
(smallestDivisor 19999)

