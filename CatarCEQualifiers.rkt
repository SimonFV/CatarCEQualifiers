#lang racket
;paquetes necesarios
(require racket/gui)
(require 2htdp/image)

; Se crea la ventana donde se mostrara el juego
(define MainWindow(new frame% [label "CatarCEQualifiers"]
                       [min-width 1800]
                       [min-height 800]
                       [stretchable-width #f]
                       [stretchable-height #f]
                       [style '(fullscreen-button)]
                       [alignment'(left center)]))

; Se hace un canvas que maneja todos los eventos que sucedan en el MainWindow
(new canvas% [parent MainWindow]
                [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "blue")
                (send dc draw-text "Don't Panic!" 0 0))])


(send MainWindow show #t)