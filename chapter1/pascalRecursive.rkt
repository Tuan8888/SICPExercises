#lang racket
(define (pascal row col)
  (cond ((or (= col 1) (= col row)) 1)
        (else (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1))))))

(pascal 1 1)
(pascal 2 2)
(pascal 5 4)
