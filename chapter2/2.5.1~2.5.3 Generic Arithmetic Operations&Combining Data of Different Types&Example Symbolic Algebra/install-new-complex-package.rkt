;; Exercise 2.86
;; 新复数包，实部和虚部可以用任意数字类型表示
#lang racket
(require (file "../2.4.3 Data-Directed Programming and Additivity/funcs-table.rkt"))
(require "tag-datum.rkt")
(require "install-new-rectangular-package.rkt")
(require "install-new-polar-package.rkt")
(require "apply-generic.rkt")
(provide (all-defined-out))

;; 安装复数子包
(install-new-rectangular-package)
(install-new-polar-package)

;; select
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; 安装新复数包
(define (install-new-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (apply-generic 'add (real-part z1) (real-part z2))
     (apply-generic 'add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (apply-generic 'sub (real-part z1) (real-part z2))
                         (apply-generic 'sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (apply-generic 'mul (magnitude z1) (magnitude z2))
                       (apply-generic 'add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (apply-generic 'div (magnitude z1) (magnitude z2))
                       (apply-generic 'sub (angle z1) (angle z2))))
  ;; 判断复数是否相等
  (define (equ? z1 z2)
    (and (apply-generic 'equ? (real-part z1) (real-part z2)) (apply-generic 'equ? (imag-part z1) (imag-part z2))))
  ;; 判断复数是否为零
  (define (complex-zero? c)
    (and (apply-generic 'zero? (real-part c)) (apply-generic 'zero? (imag-part c))))
  ;; 降级为实数
  (define (project-complex c)
    ((get 'make 'real) (real-part c)))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ?)
  (put 'zero? '(complex) complex-zero?)
  (put 'project '(complex) project-complex)
  'done)

;; 构造函数
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
