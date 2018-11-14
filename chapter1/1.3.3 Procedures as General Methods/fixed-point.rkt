;; fixed-point计算
#lang racket
(provide (all-defined-out))

;; 设定精度
(define tolerance 0.00001)

;; fixed-point计算
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))