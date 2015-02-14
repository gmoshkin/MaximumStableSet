#lang racket/base

(require plot)
(require racket/class)
(require racket/draw)
(require racket/list)

(require racket/math)

; call example: (main '((a b) (b c) (c d) (a d)) 2)

(define (main graph K)

  ; consts
  (define POP-SIZE 4) ; population size

  ; return list of nodes
  ; TODO: make our own implementation of used racket/list functions 
  (define (get-nodes graph)
    (remove-duplicates (flatten graph)))

  ; useful data
  (define nodes (get-nodes graph))
  (define nodes-number (length nodes))

  ; generates POP-SIZE random population, each vector of nodes-number size
  (define (random-population)
    ; generate binary vector of nodes-number length
    (define (gen-bin-vector lst)
      (if (= (length lst) nodes-number )
        lst
        (gen-bin-vector (cons (= 1 (random 2)) lst))))

    (define (gen-population lst)
      (if (= (length lst) POP-SIZE )
        lst
        (gen-population (cons (gen-bin-vector '()) lst))))

    (gen-population '()))

  ; calculate fitness function of solution
  (define (ff solution)
    (let*
      ((coverage-vec (get-coverage-vec solution))
       (coverage (sum coverage-vec))
       (vertexes-taken (sum (binary-vec-to-int solution))))
      (+
        (/ coverage nodes-number)
        (/ (+ 1 (* nodes-number vertexes-taken))))))

  ; returns the list of coverages (0 or 1) for each vertex in the solution 
  (define (get-coverage-vec solution)
    (define solution-with-names (zip solution nodes))
    (define taken-vertexes (solution-to-vertex-names solution))

    ; check coverage (0 or 1) for a not-taken vertex
    (define (check-coverage vertex)
      ; check for existence (0 or 1) an edge between two vertices in graph
      (define (exist-edge-between v1 v2)
        ; check (#t or #f) that given edge connects two given vertices
        (define (this-edge-connects-these-vertices edge v1 v2)
          (or
            (and (eqv? v1 (car edge)) (eqv? v2 (cadr edge)))
            (and (eqv? v1 (cadr edge)) (eqv? v2 (car edge)))))

        (sgn (sum (map
          (lambda (edge)
            (if (this-edge-connects-these-vertices edge v1 v2)
              1
              0))
          graph))))

      (sgn (sum (map
        (lambda (taken-vertex)
          (if (= 1 (exist-edge-between vertex taken-vertex))
            1
            0))
        taken-vertexes))))

    (map
      (lambda (x)
        (if (car x)
          1
          (check-coverage (cdr x))))
      solution-with-names))

  ; convert genetic solution to vertex names
  (define (solution-to-vertex-names solution)
    (define solution-with-names (zip solution nodes))
    (map
      (lambda (x) (cdr x))  
      (filter
        (lambda (x) (car x))
        solution-with-names)))

   ; compare two (solution . solution's fitness) pairs
  (define (sol-f> sf1 sf2)
    (> (cdr sf1) (cdr sf2)))

  ; one cycle - one population
  (define (main-cycle population-with-ffs generation-numb)
    (let*
      ((n 2))
      population-with-ffs))
  
  ; initialize population, calculate their fitness functions, sort them, and go to the main cycle
  (main-cycle
    (sort
      (map (lambda (x) (cons x (ff x))) (random-population))
      ; (map (lambda (x) (cons x (ff x))) '((#t #f #f #f)))
      sol-f>)
    0))


;#######################################################
; Визуализация
;#######################################################
; константы:
(define c-spring 2.0)
(define c-repulsion 1.0)
(define nat-length 1.0)
(define delta 0.1)
(define iterations-num 100)

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
    (send dc set-background "white")
    (send dc clear)
    (send dc set-smoothing 'smoothed)
    (send dc set-pen "black" 1 'solid)
    (map (lambda (edge)
           (let*
             ((v1-coord (vector-ref coordinates-vect (car edge)))
              (x1 (- (* scale-x (car v1-coord)) origin-x))
              (y1 (- (* scale-y (cdr v1-coord)) origin-y))
              (v2-coord (vector-ref coordinates-vect (cdr edge)))
              (x2 (- (* scale-x (car v2-coord)) origin-x))
              (y2 (- (* scale-y (cdr v2-coord)) origin-y)))
             (send dc draw-lines (list (cons x1 y1) (cons x2 y2))))) graph)
    (send dc set-brush "blue" 'solid)
    (map (lambda (coord)
           (let
             ((x (- (* scale-x (car coord)) origin-x))
              (y (- (* scale-y (cdr coord)) origin-y)))
             (send dc draw-ellipse
                  (- x r) (- y r) (* r 2) (* r 2)))) coordinates)
    (send dc set-pen "black" 1 'transparent)
    (send dc set-brush "red" 'solid)
    (map (lambda (vertice)
           (let*
             ((coord (vector-ref coordinates-vect vertice))
              (x (- (* scale-x (car coord)) origin-x))
              (y (- (* scale-y (cdr coord)) origin-y)))
             (send dc draw-ellipse
                   (- x r) (- y r) (* r 2) (* r 2)))) mss)))

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
        (if (adjecent? v vertice graph)
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
(define (adjecent? v1 v2 graph)
  (cond
    ((null? graph)
     #f)
    ((or (equal? (cons v1 v2) (car graph)) (equal? (cons v2 v1) (car graph)))
     #t)
    (else
      (adjecent? v1 v2 (cdr graph)))))

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

;###############################################################################
; Просто полезные функции
;###############################################################################

; sum of a list
(define (sum lst) (foldl + 0 lst))

; convert binary list to a list of integers
(define (binary-vec-to-int bin-lst)
    (map
      (lambda (x)
        (if x
          1
          0))
      bin-lst))

(define (zip lst1 lst2) 
  (map cons lst1 lst2))

;###############################################################################

(define graph (list
                (cons 1 2) (cons 2 3) (cons 3 0)
                (cons 6 2) (cons 4 6) (cons 5 7)
                (cons 3 2) (cons 8 9) (cons 8 6)))
(define vertices '(0 1 2 3 4 5 6 7 8 9))
; (define coords (initialize 10))
; (define v-coords (apply vector coords))
(define mss (list 1 2))
; (draw-result graph vertices mss)
; (send bitmap save-file "Test.png" 'png)

(main '((a b) (b c) (c d) (a d) (a e)) 2)
