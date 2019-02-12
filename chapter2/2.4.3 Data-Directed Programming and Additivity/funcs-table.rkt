;; 通用函数表
#lang racket
(provide (all-defined-out))

;; 保存通用函数
(define funcs-table (make-hash))

;; put
(define (put op type-tag func)
  (hash-set*! funcs-table (list op type-tag) func))

;; get
(define (get op type-tag)
  (if (hash-has-key? funcs-table (list op type-tag))
      (hash-ref funcs-table (list op type-tag))
      null))

