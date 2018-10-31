#lang racket
(require "primeTest.rkt")

(define (timed-prime-test n number)
  (start-prime-test n (current-inexact-milliseconds) number))

(define (start-prime-test n start-time number)
  (cond ((= number 0) (report-prime (- (current-inexact-milliseconds) start-time)))
        ((prime? n)         (start-prime-test (+ n 1) start-time (- number 1)))
        (else (start-prime-test (+ n 1) start-time number))))

(define (report-prime elapsed-time)
  (display "*** ")
  (display elapsed-time)
  (newline))

(define (searchForPrimes startNumber number)
  (timed-prime-test startNumber number))      

(searchForPrimes 1000000 3000)
(searchForPrimes 100000000 3000)
