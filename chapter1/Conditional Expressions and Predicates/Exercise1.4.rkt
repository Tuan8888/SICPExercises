;; Exercise 1.4
#lang racket

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 3 -9)
(a-plus-abs-b 3 9)
