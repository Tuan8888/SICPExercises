;; Exercise 2.86
;; 新直角坐标复数包，支持任意数字类型
#lang racket
(require (file "../2.4.3 Data-Directed Programming and Additivity/funcs-table.rkt"))
(require "tag-datum.rkt")
(require "apply-generic.rkt")
(provide (all-defined-out))

;; 安装新直角坐标复数包
(define (install-new-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cadr z))
  (define (make-from-real-imag x y) (list x y))
  (define (magnitude z)
    (apply-generic 'square
                   (apply-generic 'add
                                  (apply-generic 'square (real-part z))
                                  (apply-generic 'square (imag-part z)))))
  (define (angle z)
    (apply-generic 'atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons
     (apply-generic 'mul r (apply-generic 'cos a))
     (apply-generic 'mul r (apply-generic 'sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; 构造函数
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
