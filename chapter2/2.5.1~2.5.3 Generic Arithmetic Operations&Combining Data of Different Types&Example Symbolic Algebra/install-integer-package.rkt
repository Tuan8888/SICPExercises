;; 整数包
#lang racket
(require (file "../2.4.3 Data-Directed Programming and Additivity/funcs-table.rkt"))
(require "tag-datum.rkt")
(provide (all-defined-out))

;; 安装整数包
(define (install-integer-package)
  ;; internal procedures
  (define (make-integer x)
    x)
  ;; 升级为有理数
  (define (raise-integer x)
    ((get 'make 'rational) x 1))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'integer x))
  (put 'make 'integer
       (lambda (x) (tag (make-integer x))))
  (put 'raise '(integer)
       raise-integer)
  ;; 判断是否相等
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  'done)

  ;; 构造函数
(define (make-integer x)
  ((get 'make 'integer) x))
