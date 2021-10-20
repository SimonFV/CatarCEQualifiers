#lang racket

; Paquetes necesarios
(require racket/gui)
(require dyoo-while-loop)

; Colores
(define no-pen (new pen% [style 'transparent]))
(define white-pen (new pen% [color "white"] [width 2]))
(define gray-brush (new brush% [color "gray"]))
(define black-brush (new brush% [color "black"]))
(define blue-brush (new brush% [color "blue"]))
(define red-brush(new brush% [color "red"]))
(define green-brush (new brush% [color "green"]))
(define white-brush (new brush% [color "white"]))


; OBJETOS

; Jugador
(define team1 '((50 50) (200 300) (150 150) (100 300)))

; Balon
(define ball '((178 103) (178 103) (178 103) (178 103)))


; INTERFAZ

; Frame de la ventana
(define frame (new frame% [label "Frame"] [width 1200] [height 600]))

;Contenedor de la cancha y el marcador
(define container(new vertical-pane%[parent frame]))

;Contenedor del marcador
(define scoreboard(new horizontal-pane%[parent container]))

;Marcador del equipo azul
(define blue_score(new message% [parent scoreboard]
                       [label "0"]
                       [font (make-font #:size 32 #:family 'roman #:weight 'bold)]
                       [horiz-margin 289]))

;Marcador del equipo rojo
(define red_score(new message% [parent scoreboard]
                       [label "0"]
                       [font(make-font #:size 32 #:family 'roman #:weight 'bold)]
                       [horiz-margin 289]))

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
                           (send dc draw-text "Blue Team" 200 25)
                           (send dc set-text-foreground "red")                         
                           (send dc draw-text "Red Team" 800 25)

                           ; Dibuja la bola
                           (send dc set-brush white-brush)
                           (send dc draw-ellipse (caar ball) (cadar ball) 10 10)

                           ; Dibuja los jugadores
                           (send dc set-brush blue-brush)
                           (for ([player team1])
                             (send dc draw-ellipse (car player) (cadr player) 10 10))

                           )]))

; Activa la ventana
(send frame show #t)


; OPERACIONES DE CONTROL

; Calcula hacia donde debe moverse el jugador para llegar a su destino
(define (route current end)
  (cond [(< current end) (+ 1 current)]
        [(> current end) (- current 1)]
        [else current]))

(define (update_pos old_player new_player)
  (list (route (car old_player) (car new_player)) (route (cadr old_player) (cadr new_player))))

; Loop principal de las animaciones
(while #t
       (send game_field on-paint)
       (set! team1 (map update_pos team1 ball))

       (sleep/yield 0.03))


