;; Exercise 2.48
;; segment
#lang racket
(require "vector.rkt")
(provide (all-defined-out))

;; make
(define (make-segment v1 v2)
  (cons v1 v2))

;; selector
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))
(define s (make-segment v1 v2))
(start-segment s)
(end-segment s)
