;; Exercise 2.2
;; segment
#lang racket
(require "point.rkt")
(provide (all-defined-out))

;; selector
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

;; make
(define (make-segment start end)
  (cons start end))

;; 平均数
(define (avg x y)
  (/ (+ x y) 2.0))

;; 求中点
(define (midpoint-segment s)
  (make-point (avg (x-point (start-segment s)) (x-point (end-segment s))) (avg (y-point(start-segment s)) (y-point (end-segment s)))))

;; 平方
(define (square x)
  (* x x))

;; 求线段长度
(define (length s)
  (sqrt (+ (square (- (x-point (start-segment s)) (x-point (end-segment s))))
           (square (- (y-point (start-segment s)) (y-point (end-segment s)))))))
