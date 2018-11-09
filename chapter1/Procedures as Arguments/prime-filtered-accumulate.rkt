;; Exercise 1.33
;; 素数累加
#lang racket
(require (file "../Testing for Primality/prime-test-by-smallest-divisor.rkt"))
(require "filtered-accumulate.rkt")

(define (prime-accumulate a b)
  (define (next x)
    (+ x 1))
  (filtered-accumulate + 0 identity a next b prime-test-by-smallest-divisor))

(prime-accumulate 1 10)
