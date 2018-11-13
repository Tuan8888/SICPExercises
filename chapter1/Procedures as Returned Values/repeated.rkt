;; Exercise 1.43
;; 重复执行某函数
#lang racket
(require "compose.rkt")
(provide (all-defined-out))

;; 执行n次f
(define (repeated f n)
  (define (repeated-iter n res)
    (if (= n 0)
        res
        (repeated-iter (- n 1) (compose f res))))
  (repeated-iter n identity))

