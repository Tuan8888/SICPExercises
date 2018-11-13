;; Exercise 1.12
;; 求pascal三角中的数值
#lang racket

;; 求第row行第col列的pascal三角数值
(define (pascal row col)
  (cond ((or (= col 1) (= col row)) 1)
        (else (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1))))))

(pascal 3 2)
(pascal 5 2)
(pascal 5 4)
