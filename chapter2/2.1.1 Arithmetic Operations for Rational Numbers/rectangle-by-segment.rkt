;; Exercise 2.3
;; 通过宽和高定义矩形
#lang racket
(provide (all-defined-out))
(require "segment.rkt")

;; make
(define (make-rectangle s1 s2) (cons s1 s2))

;; selector
(define (width r)
  (length (car r)))
(define (height r)
  (length (cdr r)))
