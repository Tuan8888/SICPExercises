#lang racket
(define (dec a)
  (- a 1))
(define (inc a)
  (+ a 1))

(define (add a b)
  (if (= a 0)
      b
      (inc (add (dec a) b))))

(add 4 8)

(define (+ a b)
  (- a b))
(+ 3 3)
