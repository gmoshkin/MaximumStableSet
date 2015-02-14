#lang racket/base

(require plot)
(require racket/class)
(require racket/draw)
(require racket/list)

(require racket/math)

; константы:
(define c-spring 2.0)
(define c-repulsion 1.0)
(define nat-length 1.0)
(define delta 0.01)
(define iterations-num 100)
(define vertice-color "blue")
(define vertice-radius 6)
(define vertice-form 'circle)
(define mss-color "red")
(define edge-color "black")
(define edge-kind 'dot)
(define edge-thickness 2)
(define background-color "white")
(define image-width 700)
(define image-height 700)
(define margin-x 50)
(define margin-y 50)

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
    (
     (vertices-vect (apply vector vertices))
     (coordinates (Peter-Eades-algorithm graph vertices))
     (dbg (displayln coordinates))
     (min-x (foldl (lambda (coordinate min-tmp) (min (car coordinate) min-tmp))
                   (caar coordinates)
                   (cdr coordinates)))
     (max-x (foldl (lambda (coordinate max-tmp) (max (car coordinate) max-tmp))
                   (caar coordinates)
                   (cdr coordinates)))
     (min-y (foldl (lambda (coordinate min-tmp) (min (cdr coordinate) min-tmp))
                   (cdar coordinates)
                   (cdr coordinates)))
     (max-y (foldl (lambda (coordinate max-tmp) (max (cdr coordinate) max-tmp))
                   (cdar coordinates)
                   (cdr coordinates)))
     (scale-x (/ (- image-width (* 2 margin-x)) (- max-x min-x)))
     (scale-y (/ (- image-height (* 2 margin-y)) (- max-y min-y)))
     (origin-x (- margin-x (* scale-x min-x)))
     (origin-y (- margin-y (* scale-y min-y)))
     (bitmap (make-bitmap image-width image-height))
     (dc (new bitmap-dc% [bitmap bitmap])))
    ; (send dc set-transformation (vector (send dc get-initial-matrix)
                                        ; origin-x origin-y scale-x scale-y 0))
    (draw-graph dc graph coordinates mss origin-x origin-y scale-x scale-y
                (/ image-width 100))
    (send bitmap save-file "Result.jpeg" 'jpeg)))

;###############################################################################
; dc -- объект типа контекст рисования
; graph -- список рёбер графа, где ребро -- пара (список?) индексов вершин
; coordinates -- список координат вершин
; mss -- список индексов вершин, которые входят в доминирующее множество
; функция рисует графф
;###############################################################################
(define (draw-graph dc graph coordinates mss origin-x origin-y scale-x scale-y r)
  (let
    ((coordinates-vect (apply vector coordinates)))
    (send dc set-background background-color)
    (send dc clear)
    (send dc set-smoothing 'smoothed)
    (send dc set-pen edge-color edge-thickness 'solid)
    (map (lambda (edge)
           (let*
             ((v1-coord (vector-ref coordinates-vect (car edge)))
              (x1 (+ (* scale-x (car v1-coord)) origin-x))
              (y1 (+ (* scale-y (cdr v1-coord)) origin-y))
              (v2-coord (vector-ref coordinates-vect (cdr edge)))
              (x2 (+ (* scale-x (car v2-coord)) origin-x))
              (y2 (+ (* scale-y (cdr v2-coord)) origin-y)))
             (send dc draw-lines (list (cons x1 y1) (cons x2 y2))))) graph)
    (send dc set-pen "black" 1 'solid)
    (send dc set-brush vertice-color 'solid)
    (map (lambda (coord)
           (let
             ((x (+ (* scale-x (car coord)) origin-x))
              (y (+ (* scale-y (cdr coord)) origin-y)))
             (draw-vertice dc x y vertice-radius))) coordinates)
    (send dc set-pen "black" 1 'transparent)
    (send dc set-brush mss-color 'solid)
    (map (lambda (vertice)
           (let*
             ((coord (vector-ref coordinates-vect vertice))
              (x (+ (* scale-x (car coord)) origin-x))
              (y (+ (* scale-y (cdr coord)) origin-y)))
             (draw-vertice dc x y vertice-radius))) mss)))

;###############################################################################
; dc -- объект типа контекст рисования
; x, y -- координаты центра вершины
; r -- радиус (или другой параметр) фигуры, изображающей вершину
; функция рисует вершину (форма зависит от констант)
;###############################################################################
(define (draw-vertice dc x y r)
  (let
    ((2pi/3 (/ (* 2 pi) 3)))
    (cond ((eq? vertice-form 'square)
          (send dc draw-rectangle (- x r) (- y r) (* 2 r) (* 2 r)))
         ((eq? vertice-form 'traingle)
          (send dc draw-lines (list (cons (+ x (* (* 2 r) (cos 2pi/3)))
                                          (+ y (* (* 2 r) (sin 2pi/3))))
                                    (cons (+ x (* (* 2 r) (cos (* 2 2pi/3))))
                                          (+ y (* (* 2 r) (sin (* 2 2pi/3)))))
                                    (cons (+ x (* (* 2 r) (cos (* 2 pi))))
                                          (+ y (* (* 2 r) (sin (* 2 pi)))))
                                    (cons (+ x (* (* 2 r) (cos 2pi/3)))
                                          (+ y (* (* 2 r) (sin 2pi/3)))))))
         ((eq? vertice-form 'circle)
          (send dc draw-ellipse
                (- x r) (- y r) (* r 2) (* r 2))))))

;###############################################################################
; graph -- список рёбер графа, где ребро -- пара (список?) индексов вершин
; vertices -- список индексов вершин
; функция высчитывает координаты вершин по алгоритму неизвестного автора и
; возвращает список пар координат вершин графа
;###############################################################################
(define (Peter-Eades-algorithm graph vertices)
  (define (cycle coordinates n)
    (displayln coordinates)
    (if (= n 0)
      coordinates
      (let
        ((coordinates-vect (apply vector coordinates)))
        (cycle
          (map
            (lambda (v)
              (let ((force (force v graph vertices coordinates-vect))
                    (coord (vector-ref coordinates-vect v)))
                (cons (+ (car coord) (* delta (car force)))
                      (+ (cdr coord) (* delta (cdr force))))))
            vertices) (- n 1)))))
  (cycle (initialize (length vertices)) 10))

;###############################################################################
; вычисляет силу, действующую на вершину со стороны остальных вершин
;###############################################################################
(define (force vertice graph vertices coordinates-vect)
  (let
    ((coord (vector-ref coordinates-vect vertice)))
    (define (force v)
      (let* ((coord2 (vector-ref coordinates-vect v))
             (x2 (car coord2)) (x1 (car coord))
             (y2 (cdr coord2)) (y1 (cdr coord))
             (x-dir (- x1 x2)) (y-dir (- y1 y2))
             (dist (sqrt (+ (sqr x-dir) (sqr y-dir)))))
        (if (adjacent? v vertice graph)
          (let ((force (* c-spring (log (/ dist nat-length)))))
            (cons (- (* (/ x-dir dist) force)) (- (* (/ y-dir dist) force))))
          (let ((force (/ c-repulsion (sqr dist))))
            (cons (+ (* (/ x-dir dist) force)) (+ (* (/ y-dir dist) force)))))))
    (foldl (lambda (v f)
             (let ((force (force v)))
               (cons (+ (car force) (car f)) (+ (cdr force) (cdr f)))))
           (cons 0 0)
           (filter (lambda (v) (not (equal? v vertice))) vertices))))

;###############################################################################
; проверяет, есть ли в графе ребро соединяющее две вершины
;###############################################################################
(define (adjacent? v1 v2 graph)
  (cond
    ((null? graph)
     #f)
    ((or (equal? (cons v1 v2) (car graph)) (equal? (cons v2 v1) (car graph)))
     #t)
    (else
      (adjacent? v1 v2 (cdr graph)))))

;###############################################################################
; vertices-num -- количество вершин графа
; функция возвращает список случайных пар координат
;###############################################################################
; (define (initialize vertices-num)
  ; (list (cons 10 10) (cons 50 10) (cons 50 50) (cons 10 55)))
(define (initialize vertices-num)
  (define (helper coordinates n)
    (if (= n 0)
      coordinates
      (helper (cons (cons (random 300) (random 300)) coordinates) (- n 1))))
  (helper '() vertices-num))

;###############################################################################
; (define bitmap (make-bitmap 300 300))
; (define dc (new bitmap-dc% [bitmap bitmap]))
; (send dc set-background "white")
; (send dc clear)
; (send dc draw-rectangle (+ (* )10) 10 10 10)
; (send dc set-transformation (vector (send dc get-initial-matrix) 0 0 1 1 0))
; (send dc draw-rectangle 10 10 10 10)
; (send bitmap save-file "Test.png" 'png)
(define graph (list
                (cons 1 2) (cons 2 3) (cons 3 0)
                (cons 6 2) (cons 4 6) (cons 5 7)
                (cons 3 2) (cons 8 9) (cons 8 6)))
(define vertices '(0 1 2 3 4 5 6 7 8 9))
(define coords (initialize 10))
(define v-coords (apply vector coords))
(define mss (list 1 2))
(draw-result graph vertices mss)

