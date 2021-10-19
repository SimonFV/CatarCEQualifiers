#lang racket


; FUNCIONES AUXILIARES

#|; Consulta si un elemento pertenece a una lista
(define (miembro a m_list)
  (cond ((list? m_list)
         (cond ((null? m_list) #f)
               ((equal? (car m_list) a) #t)
               (else (miembro a (cdr m_list)))))
        (else "not a list")))|#

; Elimina un elemento de una lista
(define (delete a m_list)
  (cond [(list? m_list)
         (cond [(null? m_list) '()]
               [else (cond [(equal? a (car m_list))
                            (delete a (cdr m_list))]
                           [else (cons (car m_list) (delete a (cdr m_list)))])])]
        [else "not a list"]))



; FUNCIONES RELACIONADAS AL ALGORITMO GENTEICO

; Retorna una lista con jugadores aleatorios para ser utilizados como poblacion inicial.
; Parametro: Tamaño de la poblacion
(define (first_gen size_population)
  (get_players 0 size_population '()))

; Funcion auxiliar que devuelve la lista de jugadores inicial
(define (get_players n size_population list_players)
  (cond [(= n size_population) list_players]
        [else (get_players (+ n 1) size_population (cons (get_player_stats 0 '()) list_players))]))

; Funcion auxiliar que crea los jugadores iniciales con estadisticas aleatorias
(define (get_player_stats p stats_list)
  (cond [(= p 5) stats_list]
        [(>= p 3) (get_player_stats (+ p 1) (cons (random 100) stats_list))]
        [else (get_player_stats (+ p 1) (cons (random 5) stats_list))]))



; Retorna el jugador con la mejor suma de estadisticas
; Parametros: Lista de jugadores, posicion de la bola
(define (best list_players pos_ball)
  (cond [(equal? '() (cdr list_players)) (car list_players)]
        [else (cond [(is_better? (car list_players) (cadr list_players) pos_ball)
                     (best [cons (car list_players) (cddr list_players) pos_ball])]
                    [else (best [cdr list_players] pos_ball)])]))

; Compara dos jugadores y devuelve el mejor
(define (is_better? first_p second_p pos_ball)
  (cond [(> (suma first_p pos_ball) (suma second_p pos_ball)) #t]
        [else #f]))

; Devuelve la suma de los puntos de estadisticas de un jugador
(define (suma player pos_ball)
  (+ [close_to_ball (car player) (cadr player) pos_ball] [caddr player] [cadddr player]
     [cadddr (cdr player)]))

; Retorna una calificacion de la distancia entre el jugador y el balon. Entre mas cerca mejor.
(define (close_to_ball p_x p_y pos_ball)
  (/ (- 100 (sqrt (+ (expt (- p_x (car pos_ball)) 2) (expt (- p_y (cadr pos_ball)) 2)))) 2))



; Reproduce dos individuos y retorna una lista con n hijos
; Parametros: Lista de padres, cantidad de hijos, lista de hijos (vacia)
(define (reproduce parents n children)
  (cond [(= n 0) children]
        [else (reproduce parents (- n 1)
                         (cons (new_child (reverse (car parents)) (reverse (cadr parents)) 4 '()) children))]))

; Crea un hijo seleccionando atributos de forma aleatoria de los padres
(define (new_child parent1 parent2 n child)
  (cond [(= 1 n)
         (cond [(= 0 (random 2))
                (cons (cadr parent1) (cons (car parent1) child))]
               [else
                (cons (cadr parent2) (cons (car parent2) child))])]
        [else (cond [(= 0 (random 2))
                     (new_child (cdr parent1) (cdr parent2) (- n 1) (cons (car parent1) child))]
                    [else
                     (new_child (cdr parent1) (cdr parent2) (- n 1) (cons (car parent2) child))])]))



; Muta las caracteristicas de una lista de jugadores y los retorna
; Las estadisticas normales cambian en un punto (negativo o positivo) y la posicion en 5 puntos.
; Parametro: Lista de jugadores
(define (mutate players)
  (cond [(equal? (cdr players) '()) (cons (mutate_player (car players) 0) '())]
        [else (cons (mutate_player (car players) 0) (mutate (cdr players)))]))

; Muta las caracteristicas de un jugador.
(define (mutate_player player n)
  (cond [(equal? (cdr player) '())
         (cons (rnd_op (car player) 1 (random 3)) '())]
        [else (cond [(or (= n 0) (= n 1))
                     (cons (rnd_op (car player) 5 (random 3)) (mutate_player (cdr player) (+ n 1)))]
                    [else
                     (cons (rnd_op (car player) 1 (random 3)) (mutate_player (cdr player) (+ n 1)))])]))

; Opera de forma aleatoria (suma/resta/ninguno) un valor con un numero dado y retorna el resultado
(define (rnd_op base value operator)
  (cond [(= 0 operator) (+ base value)]
        [(= 1 operator) (- base value)]
        [else base]))


;(new_child (reverse '(23 24 1 2 3)) (reverse'(32 32 4 5 6)) 4 '())
;(reproduce '((20 22 1 2 3) (30 33 4 5 6)) 5 '())
; (mutate '((22 22 3 4 5)))