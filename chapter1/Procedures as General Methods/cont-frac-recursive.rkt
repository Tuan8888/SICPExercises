;; Exercise 1.37
;; 递归，近似计算无限连续小数
#lang racket

;; 计算无限连续小数
(define (cont-frac-recursive d n k)
  (define (cont-frac-recursive-iter i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (cont-frac-recursive-iter (+ i 1))))))
  (cont-frac-recursive-iter 1))

(cont-frac-recursive (lambda (i) 1.0)
                     (lambda (i) 1.0)
                     11)
