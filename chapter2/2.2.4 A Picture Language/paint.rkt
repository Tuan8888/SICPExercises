;; Exercise 2.44~2.45 2.49~2.52
;; 画画操作
;; 本章的代码只是抽象的，不能实际运行
#lang racket

;; 在画作上方画两幅更小的画作
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; split，d1表示小画和大画的组合方式，d2表示两幅小画的组合方式
(define (split d1 d2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split d1 d2) painter (- n 1))))
          (d1 painter (d2 smaller smaller))))))

;; 画出外框
(define (draw-outline frame)
  ;; 四个端点
  (let ((v1 (make-vect 0 1))
        (v2 (make-vect 0 0))
        (v3 (make-vect 1 0))
        (v4 (make-vect 1 1)))
    (segments->painter (list (make-segment v1 v2)
                             (make-segment v2 v3)
                             (make-segment v3 v4)
                             (make-segment v4 v1)))))

;; 画X
(define (draw-x frame)
  ;; 四个端点
  (let ((v1 (make-vect 0 1))
        (v2 (make-vect 0 0))
        (v3 (make-vect 1 0))
        (v4 (make-vect 1 1)))
    (segments->painter (list (make-segment v1 v3)
                             (make-segment v2 v4)))))

;; 画菱形
(define (draw-diamond frame)
  ;; 四个中点
  (let ((v1 (make-vect 0.0 0.5))
        (v2 (make-vect 0.5 1.0))
        (v3 (make-vect 1.0 0.5))
        (v4 (make-vect 0.5 0.0)))
    (segments->painter (list (make-segment v1 v2)
                             (make-segment v2 v3)
                             (make-segment v3 v4)
                             (make-segment v4 v1)))))

;; 画挥手的小人
(define wave-painter-segments
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
         (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.4))
         (make-segment (make-vect 0.4 0.4) (make-vect 0.3 0.5))
         (make-segment (make-vect 0.3 0.5) (make-vect 0.1 0.3))
         (make-segment (make-vect 0.1 0.3) (make-vect 0.0 0.6))
         (make-segment (make-vect 0.0 0.8) (make-vect 0.1 0.5))
         (make-segment (make-vect 0.1 0.5) (make-vect 0.3 0.6))
         (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.6))
         (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
         (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1.0))
         (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.8))
         (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.6))
         (make-segment (make-vect 0.6 0.6) (make-vect 0.8 0.6))
         (make-segment (make-vect 0.8 0.6) (make-vect 1.0 0.4))
         (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.4))
         (make-segment (make-vect 0.6 0.4) (make-vect 0.8 0.0))
         (make-segment (make-vect 0.7 0.0) (make-vect 0.5 0.3))
         (make-segment (make-vect 0.5 0.3) (make-vect 0.3 0.0)))))

;; 水平翻转
(define (flip-horiz painter)
  ((transform-painter painter
                      (make-vect 1.0 0.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
   painter))

;; 顺时针旋转180度
(define (rotate180 painter)
  ((transform-painter painter
                      (make-vect 1 1)
                      (make-vect 0 1)
                      (make-vect 1 0))
   painter))

;; 顺时针旋转270度
(define (rotate270 painter)
  ((transform-painter painter
                      (make-vect 0 1)
                      (make-vect 0 0)
                      (make-vect 1 1))
   painter))

;; below
(define (below painter-bottom painter-top)
  (let ((paint-top
         (transform-painter painter-top
                            (make-vect 0.0 0.5)
                            (make-vect 1.0 0.5)
                            (make-vect 0.0 1.0)))
        (paint-below
         (transform-painter painter-bottom
                            (make-vect 0.0 0.0)
                            (make-vect 1.0 0.0)
                            (make-vect 0.0 0.5))))
    (lambda (frame)
      (paint-top frame)
      (paint-bottom frame))))

;; beside实现below
(define (below-by-biside painter-bottom painter-top)
  (rotate270 (beside (rotate90 painter-bottom)
                      (rotate90 painter-up))))

;; corner split
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))

;; square limit
(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))



  
