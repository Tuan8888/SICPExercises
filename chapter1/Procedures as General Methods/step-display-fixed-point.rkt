;; Exercise 1.36
;; 利用fixed-point计算x^x = 1000，并打印中间结果
#lang racket

;; 设定精度
(define tolerance 0.00001)

;; fixed-point计算
(define (step-display-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(step-display-fixed-point (lambda (x) (/ (log 1000) (log x))) 4)
