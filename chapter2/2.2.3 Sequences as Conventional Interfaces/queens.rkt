;; Exercise 2.42
;; N皇后
#lang racket
(require "prime-sum-pairs.rkt")
(require "sequence-operations.rkt")

;; N皇后
;; 当前棋盘的可能状态用一组list表示，每个list表示一种皇后放置方法，每个list对应下标存放的数值表示这一列皇后放在哪一行
(define (queens board-size)
  (define (queen-cols k) 
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; 空棋盘
(define empty-board null)

;; 放入新的一列，new-row表示新放入的第k列皇后处于哪一行
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

;; 判断第k列的皇后位置是否合法
(define (safe? k positions)
  ;; 判断皇后所处行是否有重复，是否会在对角线碰撞
  (and (unique-first positions)
       ;; 判断皇后是否在对角线碰撞
       (not-crash? positions)))
       
;; 判断list中首元素是否是不重复
(define (unique-first l)
  (= (list-item-num l (car l)) 1))

;; 获取list中某个给定值出现了几次
(define (list-item-num l item)
  (length (filter (lambda(x) (= x item)) l)))

;; 判断新皇后是否会和其他皇后碰撞
(define (not-crash? l)
  ;; 沿着对角线逐步检查是否相撞
  ;; cur表示会与新皇后相撞的位置
  ;; direction为±1，表示两个对角线方向
  (define (check remain cur direction)
    (if (null? remain)
        #t
        (and (not (= (car remain) cur))
             (check (cdr remain) (+ cur direction) direction))))
  (and (check (cdr l) (+ (car l) 1) 1)
       (check (cdr l) (+ (car l) -1) -1)))

(queens 6)












