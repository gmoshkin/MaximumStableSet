#lang racket/base

(require plot)
(require racket/class)
(require racket/draw)


;###############################################################################
; Генерация тестов
;###############################################################################

;###############################################################################
; Генетический алгоритм
;###############################################################################
(define (main graph K)
  (let ((result (maximum-stable-set graph)))
    (if (<= (car result) K)
      (list #t (car result) (cdr result))
      #f)))

; Возвращает пару (|W| . W)
(define (maximum-stable-set graph)

  ;###################### констатнты ############################
  (define pop-size 100) ; размер популяции, должен быть четным
  (define const-fitness-duration 15)
  (define generation-numb-max 20000)
  (define crossover-prob 0.8)
  (define mutate-prob 0.1)

  ; список имен вершин графа
  (define (get-vertices graph)
    (distinct (flatten-once graph)))

  ;###################### вспомогательные данные ############################
  (define vertices (get-vertices graph))
  (define vertices-number (length vertices))

  ;###################### инициализация ############################
  ; сгенерировать случайную популяцию размера pop-size, каждый член которой состоит из vertices-number элементов
  (define (random-population)
    ; сгенерировать бинарный список длины vertices-number
    (define (gen-bin-vector lst)
      (if (= (length lst) vertices-number )
        lst
        (gen-bin-vector (cons (= 1 (random 2)) lst))))

    (define (gen-population lst)
      (if (= (length lst) pop-size )
        lst
        (gen-population (cons (gen-bin-vector '()) lst))))

    (gen-population '()))

  ;###################### вычисление оценочной функции ############################
  ; вычислить оценочную функцию данного кандидата на решение
  (define (ff solution)
    (let*
      ((coverage-vec (get-coverage-vec solution))
       (coverage (sum coverage-vec))
       (vertexes-taken (sum (binary-vec-to-int solution))))
      (+
        (* 2 (/ coverage vertices-number))
        (/ (+ 1 (* vertices-number vertexes-taken))))))

  ; возвращает список покрытия (0 или 1, покрывается или нет каждая вершина в данном решении) 
  (define (get-coverage-vec solution)
    (define solution-with-names (zip solution vertices))
    (define taken-vertexes (solution-to-vertex-names solution))

    ; проверить покрытие для вершины, которая в данном решении не является доминатной
    (define (check-coverage vertex)
      (sgn (sum (map
        (lambda (taken-vertex)
          (if (adjacent? vertex taken-vertex graph)
            1
            0))
        taken-vertexes))))

    (map
      (lambda (x)
        (if (car x)
          1
          (check-coverage (cdr x))))
      solution-with-names))

  ; сконвертировать решение генетического алгоритма (бинарный список) в список имен вершин
  (define (solution-to-vertex-names solution)
    (define solution-with-names (zip solution vertices))
    (map
      (lambda (x) (cdr x))  
      (filter
        (lambda (x) (car x))
        solution-with-names)))

  ;###################### выбор родителей ############################
  ; выбрать из population-with-ffs n родителей методом рулетки
  (define (select-parents population-with-ffs n)
    ; запустить рулетку один раз
    (define (select-parent)
      (define rand (/ (random 1000001) 1000000))

      (define (normalize-pairs)
        (let ((sum (foldl + 0 (map (lambda (x) (cdr x)) population-with-ffs))))
          (map
            (lambda (x) (cons (car x) (/ (cdr x) sum)))
            population-with-ffs)))

      (define (select-el-rec pairs outcome checked)
        (if (null? pairs)
          '()
          (let ((next (+ checked (cdar pairs))))
            (if (<= outcome next)
              (caar pairs)
              (select-el-rec (cdr pairs) outcome next)))))

      (select-el-rec (normalize-pairs) rand 0)
    )

    (define (select-parents-cycle parents)
      (if (= (length parents) n )
        parents
        (select-parents-cycle (cons (select-parent) parents)))
    )

    (select-parents-cycle '()))

  ;###################### породить новое поколение ############################
  (define (give-birth parents)
    (define (crossover pair)
        (if (< crossover-prob (random))
          (list (car pair) (cdr pair))
          (let ( (sep (quotient vertices-number 2)) )
            (let
              ((head1 (take-els (car pair) sep))
                (head2 (take-els (cdr pair) sep))
                (tail1 (list-tail (car pair) sep))
                (tail2 (list-tail (cdr pair) sep)))
              (list (append head1 tail2) (append head2 tail1))))))

    ; сделать кроссовер с вероятностью crossover-prob
    (flatten-once (map crossover parents)))

  ;###################### мутация ############################
    (define (mutate solution)
      (map
        (lambda (x)
          (if (< (random) mutate-prob)
            (not x)
            x))
        solution))

   ; сравнить две пары (solution . solution's fitness)
  (define (sol-f> sf1 sf2)
    (> (cdr sf1) (cdr sf2)))

  ;###################### главный цикл ############################
  ; одна итерация цикла соответствует одному поколению
  (define (maximum-stable-set-cycle population-with-ffs generation-numb best-fitness best-fitness-duration fitnesses-history)
    (let*
      ((best (car population-with-ffs))
       (new-best-fitness (cdr best))
       (new-duration 
         (if (= new-best-fitness best-fitness) 
           (+ 1 best-fitness-duration)
           1))
       ; данные для визуализации процесса приближения
       (new-avg-fitness (cdr (list-ref population-with-ffs (- (length population-with-ffs) 1))))
       (new-worst-fitness (cdr (list-ref population-with-ffs (/ (length population-with-ffs) 2))))
       (new-best-fitnesses-lst (cons new-best-fitness (car fitnesses-history)))
       (new-avg-fitnesses-lst (cons new-avg-fitness (cadr fitnesses-history)))
       (new-worst-fitnesses-lst (cons new-worst-fitness (caddr fitnesses-history)))
       (new-fitnesses-history (list new-best-fitnesses-lst new-avg-fitnesses-lst new-worst-fitnesses-lst)))
      (if (or (= new-duration const-fitness-duration) (= generation-numb generation-numb-max))
        ; условие конца алгоритма выполнено
        (let*
          ((answer-vertices (solution-to-vertex-names (car best)))
           (answer-size (length answer-vertices)))
          (draw-statistics (map (lambda (x) (reverse x)) new-fitnesses-history))
          ; (draw-result graph vertices answer-vertices)
          (cons
            answer-size
            answer-vertices))
        ; условие конца алгоритма еще не выполнено
        (let*
          ((mothers (select-parents population-with-ffs (/ pop-size 2)))
            (fathers (select-parents population-with-ffs (/ pop-size 2)))
            (new-generation (give-birth (zip fathers mothers)))
            (new-generation-mutated (map mutate new-generation))
            (new-population-with-ffs (map (lambda (x) (cons x (ff x))) new-generation-mutated))
            (new-old-populations-together (append new-population-with-ffs population-with-ffs))
            (new-old-populations-together-sorted (sort new-old-populations-together sol-f>))
            (strongest-guys (take-els new-old-populations-together-sorted pop-size)))
          (maximum-stable-set-cycle
            strongest-guys
            (+ 1 generation-numb)
            new-best-fitness
            new-duration
            new-fitnesses-history)))))

  ; инициализация первой популяции, вычисление их оценочных функций, сортировка и переход в главный цикл
  (maximum-stable-set-cycle
    (sort
      (map (lambda (x) (cons x (ff x))) (random-population))
      sol-f>)
    1 0 0 '(() () ())))


;###############################################################################
; Визуализация
;###############################################################################
; константы:
(define pi 3.141592653)
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
(define image-width 500)
(define image-height 500)
(define margin-x 50)
(define margin-y 50)

;###############################################################################
; statistics -- список из трёх списков (max-fit avg-fit min-fit) содержащих
; максимальные, средние и минимаьлные соостветственно значения целевой функции
; на каждой итерации алгоритма
; функция рисует график в файл Statistics.jpeg
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
          #:legend-anchor 'bottom-right
          #:out-file "Statistics.jpeg"
          #:out-kind 'jpeg)))

;###############################################################################
; graph -- список рёбер графа, где ребро -- список вершин
; vertices -- список вершин
; mss -- список вершин, которые входят в доминирующее множество
; функция вызывает функцию draw-graph проведя подготовку и сохраняет результат
; в файл Result.jpeg
;###############################################################################
(define (draw-result src-graph src-vertices src-mss)
  (let*
    ((vertices-indexes (zip vertices (range (length src-vertices))))
     (graph (map (lambda (edge)
                   (list (vertex->index (car edge) vertices-indexes)
                         (vertex->index (cadr edge) vertices-indexes)))
                 src-graph))
     (mss (map (lambda (vertex) (vertex->index vertex vertices-indexes))
               src-mss))
     (vertices (map cdr vertices-indexes))
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
; vertex -- врешина
; vertices -- список пар (вершина . индекс)
; функция заменяет вершину на её индекс
;###############################################################################
(define (vertex->index vertex vertices)
  (cond ((null? vertices)
         #f)
        ((eqv? (caar vertices) vertex)
         (cdar vertices))
        (else
          (vertex->index vertex (cdr vertices)))))

;###############################################################################
; dc -- объект типа контекст рисования
; graph -- список рёбер графа, где ребро -- список индексов вершин
; coordinates -- список координат вершин
; mss -- список индексов вершин, которые входят в доминирующее множество
; функция рисует граф
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
              (v2-coord (vector-ref coordinates-vect (cadr edge)))
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
    (map (lambda (vertex)
           (let*
             ((coord (vector-ref coordinates-vect vertex))
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
    (cond ((eqv? vertice-form 'square)
          (send dc draw-rectangle (- x r) (- y r) (* 2 r) (* 2 r)))
         ((eqv? vertice-form 'traingle)
          (send dc draw-lines (list (cons (+ x (* (* 2 r) (cos 2pi/3)))
                                          (+ y (* (* 2 r) (sin 2pi/3))))
                                    (cons (+ x (* (* 2 r) (cos (* 2 2pi/3))))
                                          (+ y (* (* 2 r) (sin (* 2 2pi/3)))))
                                    (cons (+ x (* (* 2 r) (cos (* 2 pi))))
                                          (+ y (* (* 2 r) (sin (* 2 pi)))))
                                    (cons (+ x (* (* 2 r) (cos 2pi/3)))
                                          (+ y (* (* 2 r) (sin 2pi/3)))))))
         ((eqv? vertice-form 'circle)
          (send dc draw-ellipse
                (- x r) (- y r) (* r 2) (* r 2))))))

;###############################################################################
; graph -- список рёбер графа, где ребро -- список индексов вершин
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
; vertex -- вершина
; graph -- список рёбер графа, где ребро -- список индексов вершин
; vertices -- список вершин графа
; coordinates-vect -- вектор координат вершин
; вычисляет силу, действующую на вершину со стороны остальных вершин
;###############################################################################
(define (force vertex graph vertices coordinates-vect)
  (let
    ((coord (vector-ref coordinates-vect vertex)))
    (define (force v)
      (let* ((coord2 (vector-ref coordinates-vect v))
             (x2 (car coord2)) (x1 (car coord))
             (y2 (cdr coord2)) (y1 (cdr coord))
             (x-dir (- x1 x2)) (y-dir (- y1 y2))
             (dist (sqrt (+ (sqr x-dir) (sqr y-dir)))))
        (if (adjacent? v vertex graph)
          (let ((force (* c-spring (log (/ dist nat-length)))))
            (cons (- (* (/ x-dir dist) force)) (- (* (/ y-dir dist) force))))
          (let ((force (/ c-repulsion (sqr dist))))
            (cons (+ (* (/ x-dir dist) force)) (+ (* (/ y-dir dist) force)))))))
    (foldl (lambda (v f)
             (let ((force (force v)))
               (cons (+ (car force) (car f)) (+ (cdr force) (cdr f)))))
           (cons 0 0)
           (filter (lambda (v) (not (equal? v vertex))) vertices))))

;###############################################################################
; проверяет, есть ли в графе ребро соединяющее две вершины
;###############################################################################
(define (adjacent? v1 v2 graph)
  (cond
    ((null? graph)
     #f)
    ((or (equal? (list v1 v2) (car graph)) (equal? (list v2 v1) (car graph)))
     #t)
    (else
      (adjacent? v1 v2 (cdr graph)))))

;###############################################################################
; vertices-num -- количество вершин графа
; функция возвращает список случайных пар координат
;###############################################################################
(define (initialize vertices-num)
  (define (helper coordinates n)
    (if (= n 0)
      coordinates
      (helper (cons (cons (random 300) (random 300)) coordinates) (- n 1))))
  (helper '() vertices-num))

;###############################################################################
; Просто полезные функции
;###############################################################################

; сумма элементов списка
(define (sum lst) (foldl + 0 lst))

; сконвертировать бинарный список в список из целых чисел
(define (binary-vec-to-int bin-lst)
    (map
      (lambda (x)
        (if x
          1
          0))
      bin-lst))

(define (zip lst1 lst2)
  (map cons lst1 lst2))

(define (flatten-once lst)
  (apply append lst))

; взять N первых элементов списка
(define (take-els lst N)
  (define (take-cycle lst res n)
    (if (= n N)
      res
      (take-cycle (cdr lst) (cons (car lst) res) (+ n 1))))
  (reverse (take-cycle lst '() 0)))

(define (range n)
  (define (helper lst m)
    (if (= m 0)
      (cons 0 lst)
      (helper (cons m lst) (- m 1))))
  (helper '() (- n 1)))

(define (sqr n)
  (* n n))

(define (distinct l)
  (cond
    ((null? l) '())
    ((member (car l) (cdr l))
      (distinct (cdr l)))
    (else
      (cons (car l) (distinct (cdr l))))))

(define (sgn n)
  (cond
    ((negative? n) -1)
    ((positive? n)  1)
    (else 0)))

;###############################################################################

; (main '((a b) (b c) (b f) (c d) (a d) (a e)) 2)
; (define graph '((a b) (b c) (b f) (c d) (a d) (a e)))
; (define vertices (distinct (flatten-once graph)))
; (define mss '(a b))
; (draw-result graph vertices mss)
