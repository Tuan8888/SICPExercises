#lang racket
(provide (all-defined-out))

(define (expmod base n m)
  (cond ((= n 0) 1)
        ((odd? n) (remainder (* base (expmod base (- n 1) m)) m))  
        (else (remainder (square (expmod base (/ n 2) m)) m))))

(define (square x)
  (* x x))

(define (odd? n)
  (= (remainder n 2) 1))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 10))
