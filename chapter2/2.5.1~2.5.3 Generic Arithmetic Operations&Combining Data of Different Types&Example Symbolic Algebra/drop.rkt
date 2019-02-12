;; Exercise 2.85
;; 数据类型降级
#lang racket
(require "apply-generic.rkt")
(require "tag-datum.rkt")
(provide (all-defined-out))

;; 数据降级
(define (apply-drop arg)
  (if (equal? (type-tag arg) 'integer)
      arg
      ;; 尝试降级
      (let ((projected-arg (apply-generic 'project arg)))
        ;; 判断降级后数据，raise后是否等于原数据
        (if (apply-generic 'equ? arg (apply-generic 'raise projected-arg))
            (apply-drop projected-arg)
            arg))))
