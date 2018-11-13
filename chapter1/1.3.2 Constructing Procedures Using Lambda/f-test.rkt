;; Exercise 1.34
;; 测试(f f)
#lang racket

(define (f g)
  (g 2))

(f f)
