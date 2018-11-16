;; Exercise 2.17~2.18 2.21
;; list
#lang racket
(provide (all-defined-out))

;; 返回list最后一个元素
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

;; 反转list
(define (reverse l)
  (define (reverse-iter remain result)
    (if (null? remain)
        result
        (reverse-iter (cdr remain) (cons (car remain) result))))
  (reverse-iter l null))

;; list索引
(define (list-ref l n)
  (if (= n 0)
      (car l)
      (list-ref (cdr l) (- n 1))))

;; 映射
(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

;; 对队列中的每个数取平方，递归版
(define (square-list-recursive items)
  (if (null? items)
      null
      (cons (* (car items) (car items)) (square-list-recursive (cdr items)))))

;; 对队列中的每个数取平方，map版
(define (square-list-map items)
  (map (lambda(x) (* x x)) items))

(square-list-recursive (list 1 2 3 4))
(square-list-map (list 1 2 3 4))
         
