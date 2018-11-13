;; 牛顿法求解方程根
#lang racket
(require (file "../Procedures as General Methods/fixed-point.rkt"))
(provide (all-defined-out))

;; 定义dx
(define dx 0.00001)

;; 求解导函数
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

;; 牛顿法求解的函数
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

;; 牛顿法求解方程，利用fixed-point
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
