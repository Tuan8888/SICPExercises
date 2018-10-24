#lang racket
(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-iter guess x)
  (begin
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 2130895720398745908273049857902374590723904570237409572394750937252394892817059712093847098129034709823904812093875091620934)
