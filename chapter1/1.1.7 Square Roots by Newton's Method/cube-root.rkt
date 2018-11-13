;; Exercise 1.8
#lang racket

;; 立方
(define (cube x)
  (* x x x))

;; 猜测值精度检测
(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

;; 迭代改进猜测值
(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

;; 牛顿迭代法
(define (cube-root-iter guess x)
  (begin
    (if (good-enough? guess x)
        guess
        (cube-root-iter (improve guess x) x))))

;; 求立方根
(define (cube-root x)
  (cube-root-iter 1.0 x))

(cube-root 729)
