;; Exercise 1.44
;; smooth
#lang racket
(require "repeated.rkt")

;; 定义dx
(define dx 0.00001)

;; smooth
(define (smooth f)
  (define (avg a b c)
    (/ (+ a b c) 3))
  (lambda(x) (avg (f (- x dx)) (f x) (f (+ x dx)))))

;; n次smooth
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (square x)
  (* x x))

((n-fold-smooth square 10) 5)
