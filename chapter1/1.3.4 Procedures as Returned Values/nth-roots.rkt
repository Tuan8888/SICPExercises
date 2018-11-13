;; Exercise 1.45
;; 求解n次方根
#lang racket
(require (file "../Procedures as General Methods/fixed-point.rkt"))
(require (file "../Exponentiation/fast-exp.rkt"))
(require "repeated.rkt")

;; average damp
(define (average-damp f)
  (lambda(x) (/ (+ (f x) x) 2)))

;; 根据n，求解需要进行多少次average dump才能使fixed-point收敛
(define (average-times n)
  ;; 迭代求解log2(n)，p为计数器
  (define (iter res p)
    (if (> n res)
        (iter (* 2 res) (+ p 1))
        p))
  (iter 1 0))

;; 使用fixed-point进行n次方根求解
(define (nth-roots x n)
  (define (f y)
    (/ x (fast-exp y (- n 1))))
  (fixed-point ((repeated average-damp (average-times n)) f) 1.0))

(nth-roots 256 8)
