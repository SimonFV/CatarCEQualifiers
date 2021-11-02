#lang racket

; Paquetes necesarios
(require racket/gui)
(require dyoo-while-loop)
(require (file "genetics.rkt"))


; Constantes
(define MIN_X 0)
(define MAX_X 1200)
(define MIN_Y 100)
(define MAX_Y 600)
(define CENTER_X 600)
(define CENTER_Y 350)
(define LEFT_LIMIT 200)
(define RIGHT_LIMIT 800)
(define LEFT_KEEPER 80)
(define RIGHT_KEEPER 1120)

(define def_limits (list MIN_X LEFT_LIMIT MIN_Y MAX_Y))

; Flags



; Colores
(define no-pen (new pen% [style 'transparent]))
(define white-pen (new pen% [color "white"] [width 2]))
(define gray-brush (new brush% [color "gray"]))
(define black-brush (new brush% [color "black"]))
(define blue-brush (new brush% [color "blue"]))
(define red-brush(new brush% [color "red"]))
(define green-brush (new brush% [color "green"]))
(define white-brush (new brush% [color "white"]))


; ELEMENTOS

; Equipos. Estructura '((posX posY velocidad fuerza habilidad)...(...))
(define team_blue_keep (first_gen 1))
(define team_blue_def (first_gen 4))
(define team_blue_mid (first_gen 4))
(define team_blue_atck (first_gen 2))

(define team_red_keep (first_gen 4))
(define team_red_def (first_gen 4))
(define team_red_mid (first_gen 4))
(define team_red_atck (first_gen 2))

; Lista temporal que alberga los jugadores de las nuevas generaciones
(define new_players '())
; Azul
(define selected_players_blue_keep '())
(define selected_players_blue_def '())
(define selected_players_blue_mid '())
(define selected_players_blue_atck '())
; Rojo
(define selected_players_red_keep '())
(define selected_players_red_def '())
(define selected_players_red_mid '())
(define selected_players_red_atck '())


; Balon
(define ball (list CENTER_X CENTER_Y))
(define ball_speed '(0 0))

; Marcador
(define goals '(0 0))



; INTERFAZ

; Frame de la ventana
(define frame (new frame% [label "Frame"] [width 1200] [height 600]))

;Contenedor de la cancha y el marcador
(define container(new vertical-pane%[parent frame]))

;Contenedor del marcador
(define scoreboard(new horizontal-pane%[parent container]))

#|
;Marcador del equipo azul
(define blue_score(new message% [parent scoreboard]
                       [label "0"]
                       [font (make-font #:size 32 #:family 'roman #:weight 'bold)]
                       [horiz-margin 289]))

;Marcador del equipo rojo
(define red_score(new message% [parent scoreboard]
                      [label "0"]
                      [font(make-font #:size 32 #:family 'roman #:weight 'bold)]
                      [horiz-margin 289]))|#

; Canvas donde se dibujan todos los elementos
(define game_field (new canvas% [parent frame]
                        [min-width 1200]
                        [min-height 600]
                        [paint-callback
                         (lambda (c dc)
                           ; Limpia la escena
                           (send dc clear)
                           (send dc set-pen no-pen)

                           ; Dibuja la cancha
                           (send dc set-brush green-brush)
                           (send dc draw-rectangle 0 100 1200 500)

                           ;Dibuja el marcador
                           (send dc set-brush black-brush)
                           (send dc draw-rectangle 0 0 1200 100)
                           (send dc set-font (make-font #:size 32 #:family 'roman #:weight 'bold))
                           (send dc set-text-foreground "blue")
                           (send dc draw-text (~a "Blue Team: " (car goals)) 200 25)
                           (send dc set-text-foreground "red")
                           (send dc draw-text (~a "Read Team: " (cadr goals)) 800 25)

                           ; Dibuja la bola
                           (send dc set-brush white-brush)
                           (send dc draw-ellipse (car ball) (cadr ball) 30 30)

                           ; Dibuja los jugadores
                           ; Azul
                           (send dc set-brush blue-brush)
                           (for ([player team_blue_keep])
                             (send dc draw-ellipse (car player) (cadr player) 30 30))
                           (for ([player team_blue_def])
                             (send dc draw-ellipse (car player) (cadr player) 30 30))
                           (for ([player team_blue_mid])
                             (send dc draw-ellipse (car player) (cadr player) 30 30))
                           (for ([player team_blue_atck])
                             (send dc draw-ellipse (car player) (cadr player) 30 30))

                           ; Rojo
                           (send dc set-brush red-brush)
                           (for ([player team_red_keep])
                             (send dc draw-ellipse (car player) (cadr player) 30 30))
                           (for ([player team_red_def])
                             (send dc draw-ellipse (car player) (cadr player) 30 30))
                           (for ([player team_red_mid])
                             (send dc draw-ellipse (car player) (cadr player) 30 30))
                           (for ([player team_red_atck])
                             (send dc draw-ellipse (car player) (cadr player) 30 30))

                           )]))

; Activa la ventana
(send frame show #t)


; OPERACIONES DE CONTROL

; Calcula hacia donde debe moverse el jugador para llegar a su destino
(define (route current end speed)
  (cond [(< current (- end speed)) (+ (* 1 speed) current)]
        [(> current (+ end speed)) (- current (* 1 speed))]
        [else end]))

; Actualiza la lista de los jugadores para que se dirijan hacia el destino
(define (update_pos old_player new_player)
  (list (route (car old_player) (car new_player) (caddr new_player))
        (route (cadr old_player) (cadr new_player) (caddr new_player))
        (caddr new_player) (cadddr new_player) (cadddr (cdr new_player))))




; Cambia la velocidad inicial del balon que empiece a animarse como si se pateara
(define (kick player direction)
  (set! ball_speed [list
                    (* (cadddr player) direction 3)
                    (round (/ (random -20 20) (+ 1 (cadddr (cdr player))) ))]))

; Revisa si hay algun jugador tocando con el balon y lo patea
(define (close_enough player direction)
  (cond [(and (< (abs (- (car player) (car ball))) 30) (< (abs (- (cadr player) (cadr ball))) 30))
         (kick player direction)]))

; Cambia la posicion del balon en base a su velocidad actual
(define (kicked)
  (begin
    (set! ball (list (+ (car ball) (car ball_speed)) (+ (cadr ball) (cadr ball_speed))) )
    (cond [(not (= 0 (car ball_speed)))
           (set! ball_speed (list
                             (round (- (car ball_speed) (/ (car ball_speed) (abs (car ball_speed))) ))
                             (cadr ball_speed)))])
    (cond [(not (= 0 (cadr ball_speed)))
           (set! ball_speed (list
                             (car ball_speed)
                             (round (- (car ball_speed) (/ (cadr ball_speed) (abs (cadr ball_speed))) )) ))])))

; Cambia la direccion de la velocidad si el balon sale del campo
; O lo coloca en el centro si es un gol
(define (ball_out)
  (cond [(< (car ball) MIN_X)
         (begin (set! goals (list (car goals) (+ 1 (cadr goals))))
                (set! ball (list CENTER_X CENTER_Y))
                (set! ball_speed '(0 0)))]
        [(> (car ball) MAX_X)
         (begin (set! goals (list (+ 1 (car goals)) (cadr goals)))
                (set! ball (list CENTER_X CENTER_Y))
                (set! ball_speed '(0 0)))]
        [(< (cadr ball) MIN_Y)
         (begin
           (set! ball (list (car ball) CENTER_Y))
           (set! ball_speed (list (car ball_speed) (* -1 (cadr ball_speed))))
           )]
        [(> (cadr ball) MAX_Y)
         (begin
           (set! ball (list (car ball) CENTER_Y))
           (set! ball_speed (list (car ball_speed) (* -1 (cadr ball_speed)))))]
        ))


; Comprueba si los jugadores terminaron su animacion
(define (finished_anim)
  (cond [(and
          (equal? team_blue_keep selected_players_blue_keep)
          (equal? team_blue_def selected_players_blue_def)
          (equal? team_blue_mid selected_players_blue_mid)
          (equal? team_blue_atck selected_players_blue_atck)
          (equal? team_red_keep selected_players_red_keep)
          (equal? team_red_def selected_players_red_def)
          (equal? team_red_mid selected_players_red_mid)
          (equal? team_red_atck selected_players_red_atck))
         #t]
        [else #f]))


; Loop principal de las animaciones
(while #t
       ; Aplica los algoritmos geneticos para crear nuevas poblaciones

       ; AZUL
       ; Portero
       (set! new_players (mutate team_blue_keep))
       (set! selected_players_blue_keep (best_players team_blue_keep new_players ball LEFT_KEEPER))
       ; Defensas
       (set! new_players (mutate team_blue_def))
       (set! selected_players_blue_def (best_players team_blue_def new_players ball LEFT_LIMIT))
       ; Medios
       (set! new_players (mutate team_blue_mid))
       (set! selected_players_blue_mid (best_players team_blue_mid new_players ball CENTER_X))
       ; Delanteros
       (set! new_players (mutate team_blue_atck))
       (set! selected_players_blue_atck (best_players team_blue_atck new_players ball RIGHT_LIMIT))

       ; ROJO
       ; Portero
       (set! new_players (mutate team_red_keep))
       (set! selected_players_red_keep (best_players team_red_keep new_players ball RIGHT_KEEPER))
       ; Defensas
       (set! new_players (mutate team_red_def))
       (set! selected_players_red_def (best_players team_red_def new_players ball RIGHT_LIMIT))
       ; Medios
       (set! new_players (mutate team_red_mid))
       (set! selected_players_red_mid (best_players team_red_mid new_players ball CENTER_X))
       ; Delanteros
       (set! new_players (mutate team_red_atck))
       (set! selected_players_red_atck (best_players team_red_atck new_players ball LEFT_LIMIT))

       (while (not (finished_anim))
              (begin
                ; Llama a la animacion de la cancha
                (send game_field on-paint)

                ; Consulta si alguien puede patear el balon y lo mueve
                ; Azul
                (for ([player team_blue_keep])
                  (close_enough player 1))
                (for ([player team_blue_def])
                  (close_enough player 1))
                (for ([player team_blue_mid])
                  (close_enough player 1))
                (for ([player team_blue_atck])
                  (close_enough player 1))
                ; Rojo
                (for ([player team_red_keep])
                  (close_enough player -1))
                (for ([player team_red_def])
                  (close_enough player -1))
                (for ([player team_red_mid])
                  (close_enough player -1))
                (for ([player team_red_atck])
                  (close_enough player -1))


                ; Mueve los jugadores
                ; Azul
                (set! team_blue_keep (map (lambda (x y) (update_pos x y)) team_blue_keep selected_players_blue_keep))
                (set! team_blue_def (map (lambda (x y) (update_pos x y)) team_blue_def selected_players_blue_def))
                (set! team_blue_mid (map (lambda (x y) (update_pos x y)) team_blue_mid selected_players_blue_mid))
                (set! team_blue_atck (map (lambda (x y) (update_pos x y)) team_blue_atck selected_players_blue_atck))
                ; Rojo
                (set! team_red_keep (map (lambda (x y) (update_pos x y)) team_red_keep selected_players_red_keep))
                (set! team_red_def (map (lambda (x y) (update_pos x y)) team_red_def selected_players_red_def))
                (set! team_red_mid (map (lambda (x y) (update_pos x y)) team_red_mid selected_players_red_mid))
                (set! team_red_atck (map (lambda (x y) (update_pos x y)) team_red_atck selected_players_red_atck))

                (sleep/yield 0.03)
                ; Cambia la posicion del balon si fue pateado
                (kicked)
                (ball_out)))


       (sleep/yield 0.03)
       (cond [(or (= 3 (car goals)) (= 3 (cadr goals))) (break)])

       )


