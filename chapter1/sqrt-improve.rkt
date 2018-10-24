#lang racket
(define (square x)
  (* x x))

(define (good-enough? old-guess guess)
  (< (abs (- old-guess guess)) (* guess 0.001)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-iter old-guess guess x)
  (begin
    (if (good-enough? old-guess guess)
        guess
        (sqrt-iter guess (improve guess x) x))))

(define (sqrt x)
  (sqrt-iter 0.0 1.0 x))

(sqrt 2130895720398745908273049857902374590723904570237409572394750937252394892817059712093847098129034709823904812093875091620934213089572039874590827304985790237459072390457023740957239475093725)
