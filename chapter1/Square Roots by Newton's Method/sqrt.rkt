;; 牛顿法求平方根
#lang racket

;; 求平方
(define (square x)
  (* x x))

;; 检测猜测值精度
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; 牛顿法改进猜测值
(define (improve guess x)
  (average guess (/ x guess)))

;; 求平均值
(define (average x y)
  (/ (+ x y) 2))

;; 不断改进猜测值直到精度满足需求
(define (sqrt-iter guess x)
  (begin
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))))

;; 牛顿法求平方根
(define (sqrt x)
  (sqrt-iter 1.0 x))
