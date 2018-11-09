;; Exercise 1.37
;; 迭代，近似计算无限连续小数
#lang racket
(provide (all-defined-out))

(define (cont-frac-iterative d n k)
  (define (cont-frac-iterative-iter i res)
    (if (= i 0)
        res
        (cont-frac-iterative-iter (- i 1)
                                  (/ (n i)
                                     (+ (d i) res)))))
  (cont-frac-iterative-iter k 0))

