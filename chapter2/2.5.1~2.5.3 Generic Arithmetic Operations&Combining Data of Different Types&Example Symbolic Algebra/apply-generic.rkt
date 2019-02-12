;; Exercise 2.81 2.84
;; 使用泛型函数
#lang racket
(require "tag-datum.rkt")
(require (file "../2.4.3 Data-Directed Programming and Additivity/funcs-table.rkt"))
(provide (all-defined-out))

;; 模拟类型塔
(define levels (hash 'integer 3 'rational 2 'real 1 'complex 0))

;; 带raise的多输入泛型操作
(define (apply-generic-with-raise op . args)
  ;; raise到指定类型
  (define (raise-to type)
    (lambda(arg)
      (define (iter cur)
        (if (equal? type (type-tag cur))
            cur
            (let ((proc (get 'raise (list (type-tag cur)))))
              (if (null? proc)
                  (error "Cannot raise")
                  (iter (proc (contents cur)))))))
      (iter arg)))
  ;; 找到输入参数中最高级类型
  (define (highest-type args)
    (define (iter remains higher)
      (if (null? remains)
          (type-tag higher)
          (let ((arg (car remains)))
            (if (> (hash-ref levels (type-tag arg)) (hash-ref levels (type-tag higher)))
                (iter (cdr remains) arg)
                (iter (cdr remains) arg)))))
    (iter (cdr args) (car args)))
;; 找到参数中最高级类型，把所有参数转换为该类型，寻找对应通用函数
  (let ((changed-proc (raise-to (highest-type args))))
    (let ((changed-args (map changed-proc args)))
      (let ((proc (get op (map type-tag changed-args))))
        (if (null? proc)
            (error "No method for these types")
            (apply proc (map contents changed-args)))))))

;; 多输入泛型操作
(define (apply-generic op . args)
  ;; 尝试转换arg为type类型
  (define (change-type type)
    (lambda(arg)
      (let ((change-proc (get-coercion (type-tag arg) type)))
        (if (null? change-proc)
             arg
             (change-proc arg)))))
  ;; origin-types表示最初输入参数的类型list
  (define (iter origin-types changed-args)
    ;; 查找对应输入类型的函数
    (let ((type-tags (map type-tag changed-args)))
      (let ((proc (get op type-tags)))
        (if (not (null? proc))
            ;; 如果找到对应输入类型的函数
            (apply proc (map contents changed-args))
            ;; 如果没有找到，转换为origin-types中的首元素类型
            (if (null? origin-types)
                ;; 已经尝试了所有类型，仍未找到，报错
                (error "No method for these types" (list op args))
                ;; 转换参数类型，递归调用
                (iter (cdr origin-types) (map (change-type (car origin-types)) args)))))))
  ;; 调用
  (iter (map type-tag args) args))
          
;; 获取转换函数
(define (get-coercion type1 type2)
  (get 'coercion (list type1 type2)))
