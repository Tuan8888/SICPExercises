;; 新极坐标复数包，支持任意数字类型
#lang racket
(require (file "../2.4.3 Data-Directed Programming and Additivity/funcs-table.rkt"))
(require "tag-datum.rkt")
(require "apply-generic.rkt")
(provide (all-defined-out))

;; 安装新极坐标复数包
(define (install-new-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cadr z))
  (define (make-from-mag-ang r a) (list r a))
  (define (real-part z)
    (apply-generic 'mul (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (apply-generic 'mul (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (apply-generic 'sqrt (apply-generic 'add (* x x) (* y y)))
          (apply-generic 'atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; 构造函数
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
