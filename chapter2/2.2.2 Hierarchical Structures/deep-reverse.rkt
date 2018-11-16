;; Exercise 2.27
;; 深度翻转list
#lang racket

;; 深度翻转list
(define (deep-reverse l)
  (define (iter remain result)
    (if (null? remain)
        result
        (iter (cdr remain) (cons (deep-reverse (car remain)) result))))
  (if (pair? l)
      (iter l null)
      l))

(define x (list (list 1 2) (list 3 4) 1 2 3))

(deep-reverse x)
