#lang racket
(require "fastSmallestDivisor.rkt")

(define (timed-prime-test n number)
  (start-prime-test n (current-inexact-milliseconds) number))

(define (start-prime-test n start-time number)
  (cond ((= number 0) (report-prime (- (current-inexact-milliseconds) start-time)))
        ((prime? n)
         (display n)
         (display " ")
         (start-prime-test (+ n 1) start-time (- number 1)))
        (else (start-prime-test (+ n 1) start-time number))))

(define (report-prime elapsed-time)
  (display "*** ")
  (display elapsed-time)
  (newline))

(define (searchForPrimes startNumber number)
  (timed-prime-test startNumber number))      

(define (expmod base n m)
  (cond ((= n 0) 1)
        ((odd? n) (remainder (* base (expmod base (- n 1) m)) m))  
        (else (remainder (square (expmod base (/ n 2) m)) m))))

(define (square x)
  (* x x))

(define (odd? n)
  (= (remainder n 2) 1))

(define (prime? n)
  (= (fastSmallestDivisor n) n))

(searchForPrimes 10000000000 3)
(searchForPrimes 100000000000 3)
(searchForPrimes 1000000000000 3)
(searchForPrimes 10000000000000 3)
(searchForPrimes 100000000000000 3)
