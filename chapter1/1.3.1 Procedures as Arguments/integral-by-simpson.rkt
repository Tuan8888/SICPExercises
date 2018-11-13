;; Exercise 1.29
;; Simpson's Rule计算integral
#lang racket
(require "cube.rkt")
(require "sum.rkt")

;; Simpson's Rule计算integral
(define (integral-by-simpson f a b n)
  ;; 定义h
  (define h (/ (- b a) n))
  ;; 定义h/3
  (define h-divide-3 (/ h 3))
  ;; 以k作为自变量，比用a方便
  (define (next k)
    (+ k 1))
  ;; k的每项计算
  (define (term k)
    (define yk (* h-divide-3 (f (+ a (* k h))))) 
    (cond ((or (= k 0) (= k n)) yk)
          ((even? k) (* 2 yk))
          (else (* 4 yk))))
  ;; 调用sigma函数
  (sum term 0 next n))

;; 判断是否为偶数
(define (even? x)
  (= (remainder x 2) 0))

(integral-by-simpson cube 0 1 100)
(integral-by-simpson cube 0 1 1000)
