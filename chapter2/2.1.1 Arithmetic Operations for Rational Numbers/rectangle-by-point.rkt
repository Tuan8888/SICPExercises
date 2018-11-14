;; Exercise 2.3
;; 通过4个点定义矩形
#lang racket
(require "point.rkt")
(require "segment.rkt")
(provide (all-defined-out))

;; make
(define (make-rectangle p1 p2 p3 p4)
  (cons (cons p1 p2) (cons p3 p4)))

;; selector
(define (width r)
  (length (make-segment (car (car r)) (cdr (car r)))))
(define (height r)
  (length (make-segment (cdr (car r)) (cdr (cdr r)))))
