;; Exercise 1.8
;; 改进的牛顿迭代法
#lang racket

;; 平方
(define (square x)
  (* x x))

;; 检测猜测值精度，当猜测值和上一次猜测值相比改进小于0.1%时停止迭代
(define (good-enough? old-guess guess)
  (< (abs (- old-guess guess)) (* guess 0.001)))

;; 改进猜测值
(define (improve guess x)
  (average guess (/ x guess)))

;; 平均值
(define (average x y)
  (/ (+ x y) 2))

;; 牛顿迭代法
(define (sqrt-iter old-guess guess x)
  (begin
    (if (good-enough? old-guess guess)
        guess
        (sqrt-iter guess (improve guess x) x))))

;; 改进的求平方根
(define (better-sqrt x)
  (sqrt-iter 0.0 1.0 x))

(better-sqrt 2130895720398745908273049857902374590723904570237409572394750937252394892817059712093847098129034709823904812093875091620934213089572039874590827304985790237459072390457023740957239475093725)
