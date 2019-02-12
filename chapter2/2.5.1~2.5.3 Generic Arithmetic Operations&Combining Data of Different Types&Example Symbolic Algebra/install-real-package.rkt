;; 实数包
#lang racket
(require (file "../2.4.3 Data-Directed Programming and Additivity/funcs-table.rkt"))
(require "tag-datum.rkt")
(require "install-complex-package.rkt")
(provide (all-defined-out))

;; 安装实数包
(define (install-real-package)
  ;; internal procedures
  (define (make-real x)
    x)
  ;; 升级为复数
  (define (raise-real x)
    ((get 'make-from-real-imag 'complex) x 0))
  ;; 降级为有理数
  (define (project-real x)
    ((get 'make 'rational) (* x 100000000) 100000000))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'real x))
  (put 'make 'real
       (lambda (x) (tag (make-real x))))
  (put 'raise '(real)
       raise-real)
  ;; 判断是否相等
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put 'project '(real) project-real)  
  'done)

  ;; 构造函数
(define (make-real x)
  ((get 'make 'real) x))
