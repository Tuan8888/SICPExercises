;; Exercise 2.3
;; 矩形的相关计算
#lang racket
(require "rectangle-by-segment.rkt")
(provide (all-defined-out))

;; 计算周长
(define (perimeter rectangle)
  (* 2 (+ (width rectangle) (height rectangle))))

;; 计算面积
(define (area rectangle)
  (* (width rectangle) (height rectangle))) 
