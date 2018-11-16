;; Exercise 2.23
;; for循环
#lang racket

;; 对l中的每个元素执行f
(define (for-each f l)
  (cond ((not (null? l))
         (f (car l))
         (for-each f (cdr l)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
