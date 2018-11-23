;; Exercise 2.47
;; frame
#lang racket
(require "vector.rkt")
(require "segment.rkt")

;; make
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

;; selector
(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (cdr (cdr f)))


