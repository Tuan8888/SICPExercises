;; Exercise 2.60
;; 允许重复的列表构成的集合
#lang racket

;; 判断集合是否包含某个元素
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; 在集合中加入元素
(define (adjoin-set x set)
  (cons x set))

;; 取交集 
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set (cdr set1) (cons (car set1) set2)))))

;; 取并集
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) null)
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


