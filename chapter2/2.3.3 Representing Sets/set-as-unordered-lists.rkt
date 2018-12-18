;; Exercise 2.59
;; 无序列表构成的集合
#lang racket

;; 判断集合中是否存在某个元素
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; 在集合中加入元素
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; 取交集
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; 取并集
(define (union-set set1 set2)
  (cond ((or (null? set1) (null? set2)) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define s1 (list 1 2 3))
(define s2 (list 3 5 7 9))

(union-set s1 s2)
