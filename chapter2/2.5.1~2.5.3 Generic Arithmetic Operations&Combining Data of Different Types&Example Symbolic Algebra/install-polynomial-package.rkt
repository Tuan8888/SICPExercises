;; 多项式包
#lang racket
(require (file "../2.4.3 Data-Directed Programming and Additivity/funcs-table.rkt"))
(require "tag-datum.rkt")
(require "apply-generic.rkt")
(require "install-polynomial-parse-package.rkt")
(require "install-polynomial-dense-package.rkt")
(provide (all-defined-out))

(define (install-polynomial-package)
  ;; internal procedures
  ;; 多项式加法
  (define (add-poly p1 p2) (apply-generic 'add p1 p2))
  ;; 多项式乘法
  (define (mul-poly p1 p2) (apply-generic 'mul p1 p2))
  ;; 多项式减法
  (define (sub-poly p1 p2) (apply-generic 'sub p1 p2))
  ;; 多项式除法
  (define (div-poly p1 p2) (apply-generic 'div p1 p2))
  ;; interface to rest of the system
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'div '(polynomial polynomial) 
       (lambda (p1 p2) (map tag (div-poly p1 p2))))
  'done)

(define (tag p) (attach-tag 'polynomial p))
;; 构造函数
(define (make-polynomial-from-dense variable term-list)
  (tag ((get 'make 'polynomial-dense) variable term-list)))
(define (make-polynomial-from-parse variable term-list)
  (tag ((get 'make 'polynomial-parse) variable term-list)))

(install-polynomial-package)

               
