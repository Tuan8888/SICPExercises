#lang racket
(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (even x)
  (= (remainder x 2) 0))

(define (fastMult a b)
  (cond ((= b 0) 0)
        ((even b) (double (fastMult a (halve b))))
        (else (+ a (fastMult a (- b 1))))))
         
(fastMult 5 0)
(fastMult 3 3)
(fastMult 4 9)
(fastMult 5 7)
