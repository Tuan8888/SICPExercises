#lang racket
(define (dec a)
  (- a 1))
(define (inc a)
  (+ a 1))

(define (add a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(add 3 4)
