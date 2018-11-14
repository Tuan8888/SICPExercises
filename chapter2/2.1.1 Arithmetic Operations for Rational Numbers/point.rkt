;; Exercise 2.2
;; point
#lang racket
(provide (all-defined-out))

;; selector
(define (x-point x) (car x))
(define (y-point x) (cdr x))

;; make
(define (make-point x y) (cons x y))

;; print
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

