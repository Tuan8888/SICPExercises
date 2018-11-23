;; Exercise 2.54
;; 判断list是否相等
#lang racket

;; 判断两个list是否完全相等
(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2) (not (eq? (car l1) (car l2)))) #f)
        (else (equal? (cdr l1) (cdr l2)))))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

