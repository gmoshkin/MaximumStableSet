#lang racket/base

(require plot)
(require racket/gui)
(require racket/list)

(require racket/math)

;###############################################################################
; statistics -- список из трёх списков (max-fit avg-fit min-fit) содержащих
; максимальные, средние и минимаьлные соостветственно значения целевой функции
; на каждой итерации алгоритма
;###############################################################################
(define (draw-statistics statistics)
  (let
    ((iterations-num (length (car statistics)))
     (iterations (range iterations-num))
     (max-fit (map list (car statistics) iterations))
     (avg-fit (map list (cadr statistics) iterations))
     (min-fit (map list (caddr statistics) iterations)))
    (plot (list (lines max-fit #:color 1 #:label "Maximum fitness")
                (lines avg-fit #:color 2 #:label "Average fitness")
                (lines min-fit #:color 3 #:label "Minimum fitness"))
          #:title "Statistics"
          #:out-file "Statistics.jpeg"
          #:out-kind 'jpeg)))
