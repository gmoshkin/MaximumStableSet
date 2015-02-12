#lang racket/base

(require plot)
(require racket/class)
(require racket/draw)
(require racket/list)

(require racket/math)

;###############################################################################
; statistics -- список из трёх списков (max-fit avg-fit min-fit) содержащих
; максимальные, средние и минимаьлные соостветственно значения целевой функции
; на каждой итерации алгоритма
; функция рисует граффик в файл Statistics.jpeg
;###############################################################################
(define (draw-statistics statistics)
  (let*
    ((iterations-num (length (car statistics)))
     (iterations (range iterations-num))
     (max-fit (map list iterations (car statistics)))
     (avg-fit (map list iterations (cadr statistics)))
     (min-fit (map list iterations (caddr statistics))))
    (plot (list (lines max-fit #:color 1 #:label "Maximum fitness")
                (lines avg-fit #:color 2 #:label "Average fitness")
                (lines min-fit #:color 3 #:label "Minimum fitness"))
          #:title "Statistics"
          #:x-label "Algorith iteration"
          #:y-label "Fitness"
          #:out-file "Statistics.jpeg"
          #:out-kind 'jpeg)))

;###############################################################################
; graph -- список рёбер графа, где вершина -- пара (список?) индексов вершин
; vertices -- список индексов вершин
; mss -- список индексов вершин, которые входят в доминирующее множество
; функция вызывает функцию draw-graph проведя подготовку и сохраняет результат
; в файл Result.jpeg
;###############################################################################
(define (draw-result graph vertices mss)
  (let*
    ((image-width 400)
     (image-height 400)
     (margin-x 10)
     (margin-y 10)
     (vertices-vect (apply vector vertices))
     (coordinates (Peter-Eades-algorithm graph vertices))
     (min-x (foldl (lambda (coordinate min-tmp) (min (car coordinates) min-tmp))
                   (caar coordinates)
                   (cdr coordinates)))
     (max-x (foldl (lambda (coordinate max-tmp) (max (car coordinates) max-tmp))
                   (caar coordinates)
                   (cdr coordinates)))
     (min-y (foldl (lambda (coordinate min-tmp) (min (cadr coordinates) min-tmp))
                   (cadar coordinates)
                   (cdr coordinates)))
     (max-y (foldl (lambda (coordinate max-tmp) (max (cadr coordinates) max-tmp))
                   (cadar coordinates)
                   (cdr coordinates)))
     (scale-x (/ (- image-width (* 2 margin-x)) (- max-x min-x)))
     (scale-y (/ (- image-height (* 2 margin-y)) (- max-y min-y)))
     (origin-x (- margin-x (* scale-x min-x)))
     (origin-y (- margin-y (* scale-y min-y)))
     (bitmap (make-bitmap image-width image-height))
     (dc (new bitmap-dc% [bitmap bitmap])))
    (send dc set-transformation (vector (send dc get-initial-matrix)
                                        origin-x origin-y scale-x scale-y 0))
    (draw-graph dc graph coordinates mss)
    (send bitmap save-file "Result.jpeg" 'jpeg)))

;###############################################################################
; dc -- объект типа контекст рисования
; graph -- список рёбер графа, где вершина -- пара (список?) индексов вершин
; coordinates -- список координат вершин
; mss -- список индексов вершин, которые входят в доминирующее множество
; функция рисует графф
;###############################################################################
(define (draw-graph dc graph coordinates mss)
  (let
    ((coordinates-vect (apply vector coordinates)))
    (send dc set-smoothing 'smoothed)
    (send dc set-pen "black" 1 'solid)
    (map (lambda (edge)
           (let
             ((v1-coord (vector-ref coordinates-vect (car edge)))
              (v2-coord (vector-ref coordinates-vect (cdr edge))))
             (send dc draw-lines (list v1-coord v2-coord)))) graph)
    (send dc set-brush "blue" 'solid)
    (map (lambda (coord)
           (send dc draw-ellipse
                 (- (car coord) 3) (- (cdr coord) 3) 6 6)) coordinates)
    (send dc set-pen "black" 1 'transparent)
    (send dc set-brush "red" 'solid)
    (map (lambda (vertice)
           (let
             ((coord (vector-ref coordinates-vect vertice)))
             (send dc draw-ellipse
                   (- (car coord) 3) (- (cdr coord) 3) 6 6))) mss)))

;###############################################################################
; graph -- список рёбер графа, где вершина -- пара (список?) индексов вершин
; vertices -- список индексов вершин
; функция высчитывает координаты вершин по алгоритму неизвестного автора и
; возвращает список пар координат вершин графа
;###############################################################################
(define (Peter-Eades-algorithm graph vertices)
  'TODO)

;###############################################################################
(define bitmap (make-bitmap 300 300))
(define dc (new bitmap-dc% [bitmap bitmap]))
(send dc set-background "white")
(send dc clear)
(define graph (list (cons 1 2) (cons 2 3) (cons 3 0)))
(define coords (list (cons 50 50) (cons 100 100) (cons 100 50) (cons 50 100)))
(define mss (list 1 2))
(draw-graph dc graph coords mss)
(send bitmap save-file "Test.png" 'png)
(display (send dc get-transformation))
