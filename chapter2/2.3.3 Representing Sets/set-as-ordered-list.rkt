;; Exercise 2.61
;; 有序列表构成的集合
#lang racket

;; 向集合增加元素
(define (adjoin-set x set1)
  (cond ((null? set1) (cons x null))
        ((= (car set1) x) set1)
        ((> (car set1) x) (cons x set1))
        (else (cons (car set1) (adjoin-set x (cdr set1))))))

;; 取交集
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                 (else (cons x2 (union-set set1 (cdr set2)))))))))

(define set1 (list 2 3 5 8))
(define set2 (list 1 3 7 9))

(union-set set1 set2)

        
