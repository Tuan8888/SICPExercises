;; Exercise 2.3
;; 矩形测试
#lang racket
(require "point.rkt")
(require "segment.rkt")
(require "rectangle-calculate.rkt")
;; 可以替换矩形底层实现
(require "rectangle-by-point.rkt")
;; (require "rectangle-by-segment.rkt")

;; 定义四个点
(define p1 (make-point 3 4))
(define p2 (make-point 5 6))
(define p3 (make-point 7 4))
(define p4 (make-point 5 2))

;; 定义两条边
(define s1 (make-segment p1 p2))
(define s2 (make-segment p2 p3))

;; 定义矩形
(define r (make-rectangle p1 p2 p3 p4))
;; (define r (make-rectangle s1 s2))

;; 计算周长和面积
(perimeter r)
(area r)
