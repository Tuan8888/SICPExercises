#lang racket
(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (even x)
  (= (remainder x 2) 0))
  
(define (multIter a b)
  (cond ((= b 0) 0)
        ((even b) (multIter (double a) (halve b)))
        (else (+ a (multIter a (- b 1))))))

(define (mult a b)
  (multIter a b))

(mult 3 4)
(mult 3 0)
(mult 99 1)
