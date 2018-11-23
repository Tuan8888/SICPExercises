;; Exercise 2.32
;; 生成一个list的所有子list
#lang racket

;; 子list生成
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (l) (cons (car s) l)) rest)))))

(define s (list 1 2 3))
(subsets s)


