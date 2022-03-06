#lang scheme
(require (lib "graphics.ss" "graphics")) (open-graphics)


(define i 0)
(define j 0)
(define width 1000)
(define height 800)
(define ladoCuadro 19)
(define espaciado 20)
(define ventana (open-viewport "Cubo Rubik" width height))

(define (dibujarMatriz n)
  (define espaciadoCaras (+ ladoCuadro (* n espaciado) 11))
  (limpiarVentana)
  
  ;(dibujarCara n (- (/ width 2) (/ espaciadoCaras 2)) (- (/ height 2) (/ espaciadoCaras 2)))

  (for([i (in-range -1 3 1)])
    (dibujarCara n (- (/ width 2) (/ espaciadoCaras 2)) (+ (- (/ height 2) (/ espaciadoCaras 2)) (* i espaciadoCaras) )); dibuja las cara superior, frontal, inferior y trasera en ese orden (de arriba  a abajo)
    )
  
  (dibujarCara n (- (/ width 2) (/ espaciadoCaras 2) espaciadoCaras) (- (/ height 2) (/ espaciadoCaras 2))); dibuja la cara izquierda
  (dibujarCara n (- (/ width 2) (/ espaciadoCaras 2) (- 0 espaciadoCaras)) (- (/ height 2) (/ espaciadoCaras 2))); dibuja la cara derecha
  )


(define (dibujarCara n posx posy)

  (for([i (in-range 0 n 1)])
    ((draw-solid-rectangle ventana)(make-posn (+ posx (* i espaciado)) posy) ladoCuadro ladoCuadro "black")
    (for([j (in-range 1 n 1)])
      ((draw-solid-rectangle ventana)(make-posn (+ posx (* i espaciado)) (+ posy (* j espaciado))) ladoCuadro ladoCuadro "black")
      )
    )
  )

(define (limpiarVentana)
  ((clear-viewport ventana))
  ((draw-solid-rectangle ventana)(make-posn 0 0) width height "gray")
  )

 
(dibujarMatriz 6) 