#lang racket/base

(require plot)
(require racket/class)
(require racket/draw)

; call example: (main'((a b) (b c) (b f) (c d) (a d) (a e)) 2)

(define (main graph K)
  (let ((result (maximum-stable-set graph)))
    (if (<= (car result) K)
      (list #t (car result) (cdr result))
      #f)))

; returns pair (|W| . W)
(define (maximum-stable-set graph)

  ;###################### consts ############################
  (define pop-size 100) ; population size, must be even
  (define const-fitness-duration 15)
  (define generation-numb-max 20000)
  (define crossover-prob 0.8)
  (define mutate-prob 0.1)

  ; return list of nodes
  (define (get-nodes graph)
    (distinct (flatten-once graph)))

  ;###################### useful data ############################
  (define nodes (get-nodes graph))
  (define nodes-number (length nodes))

  ;###################### initialization ############################
  ; generates pop-size random population, each vector of nodes-number size
  (define (random-population)
    ; generate binary vector of nodes-number length
    (define (gen-bin-vector lst)
      (if (= (length lst) nodes-number )
        lst
        (gen-bin-vector (cons (= 1 (random 2)) lst))))

    (define (gen-population lst)
      (if (= (length lst) pop-size )
        lst
        (gen-population (cons (gen-bin-vector '()) lst))))

    (gen-population '()))

  ;###################### fitness function calculation ############################
  ; calculate fitness function of solution
  (define (ff solution)
    (let*
      ((coverage-vec (get-coverage-vec solution))
       (coverage (sum coverage-vec))
       (vertexes-taken (sum (binary-vec-to-int solution))))
      (+
        (* 2 (/ coverage nodes-number))
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

  ;###################### choosing parents ############################
  ; select n parents with a roulette wheel
  (define (select-parents population-with-ffs n)
    ; run a roulette wheel
    (define (select-parent)
      (define rand (/ (random 1000001) 1000000))

      (define (normalize-pairs)
        (let ((sum (foldl + 0 (map (lambda (x) (cdr x)) population-with-ffs))))
          (map
            (lambda (x) (cons (car x) (/ (cdr x) sum)))
            population-with-ffs)))

      (define (select-el-rec pairs outcome checked)
        (if (null? pairs)
          '() ;should never be here
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

  ;###################### give birth to a new generation ############################
  (define (give-birth parents)
    (define (crossover pair)
        (if (< crossover-prob (random))
          (list (car pair) (cdr pair))
          (let ( (sep (quotient nodes-number 2)) )
            (let
              ((head1 (take-els (car pair) sep))
                (head2 (take-els (cdr pair) sep))
                (tail1 (list-tail (car pair) sep))
                (tail2 (list-tail (cdr pair) sep)))
              (list (append head1 tail2) (append head2 tail1))))))

    ; make a crossover with crossover-prob probability
    (flatten-once (map crossover parents)))

  ;###################### mutation ############################
    (define (mutate solution)
      (map
        (lambda (x)
          (if (< (random) mutate-prob)
            (not x)
            x))
        solution))

   ; compare two (solution . solution's fitness) pairs
  (define (sol-f> sf1 sf2)
    (> (cdr sf1) (cdr sf2)))

  ;###################### main cycle ############################
  ; one cycle - one population
  (define (maximum-stable-set-cycle population-with-ffs generation-numb best-fitness best-fitness-duration fitnesses-history)
    (let*
      ((best (car population-with-ffs))
       (new-best-fitness (cdr best))
       (new-duration 
         (if (= new-best-fitness best-fitness) 
           (+ 1 best-fitness-duration)
           1))
       ; for process rendering
       (new-avg-fitness (cdr (list-ref population-with-ffs (- (length population-with-ffs) 1))))
       (new-worst-fitness (cdr (list-ref population-with-ffs (/ (length population-with-ffs) 2))))
       (new-best-fitnesses-lst (cons new-best-fitness (car fitnesses-history)))
       (new-avg-fitnesses-lst (cons new-avg-fitness (cadr fitnesses-history)))
       (new-worst-fitnesses-lst (cons new-worst-fitness (caddr fitnesses-history)))
       (new-fitnesses-history (list new-best-fitnesses-lst new-avg-fitnesses-lst new-worst-fitnesses-lst)))
      (if (or (= new-duration const-fitness-duration) (= generation-numb generation-numb-max))
        ; the end of the process
        (let*
          ((answer-vertices (solution-to-vertex-names (car best)))
           (answer-size (length answer-vertices)))
          (draw-statistics (map (lambda (x) (reverse x)) new-fitnesses-history))
          (cons
            answer-size
            answer-vertices))
        ; continue
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
  
  ; initialize population, calculate their fitness functions, sort them, and go to the main cycle
  (maximum-stable-set-cycle
    (sort
      (map (lambda (x) (cons x (ff x))) (random-population))
      sol-f>)
    1 0 0 '(() () ())))


;#######################################################
; Визуализация
;#######################################################
; константы:
; визуализация:
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
(define image-width 700)
(define image-height 700)
(define margin-x 50)
(define margin-y 50)

;###############################################################################
; Генерация тестов
;###############################################################################
;###############################################################################
; описание функции
;###############################################################################
(define (generate-test vertices-num dominators-num)
  'TODO)

;###############################################################################
; Визуализация
;###############################################################################
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

(define (flatten-once lst)
  (apply append lst))

;take N first elements from lst
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

(main'((a b) (b c) (b f) (c d) (a d) (a e)) 2)
