#lang racket

; Retorna una lista con jugadores aleatorios para ser utilizados como poblacion inicial.
(define (first_gen size_poll)
  (get_players 0 size_poll '()))

(define (get_players n size_poll list_players)
  (cond [(= n size_poll) list_players]
        (else [get_players (+ n 1) size_poll (cons (get_player_stats 0 '()) list_players)])))

(define (get_player_stats p stats_list)
  (cond [(= p 4) stats_list]
        (else [get_player_stats (+ p 1) (cons (random 5) stats_list)])))

(first_gen 30)