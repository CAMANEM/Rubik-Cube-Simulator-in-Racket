#lang scheme
(require (lib "graphics.ss" "graphics")) (open-graphics)
(require "Rubik_Cube_Simulation.rkt")

(define i 0)
(define j 0)
(define width 1000)
(define height 800)
(define ladoCuadro 17)
(define espaciado 20)
(define ventana (open-viewport "Cubo Rubik" width height))

(define (dibujarMatriz n matriz)
  (define espaciadoCaras (+ ladoCuadro (* n espaciado) 11))
  (limpiarVentana)
  
  ;(dibujarCara n (- (/ width 2) (/ espaciadoCaras 2)) (- (/ height 2) (/ espaciadoCaras 2)))

  (for([i (in-range -1 3 1)])
    (dibujarCara n (- (/ width 2) (/ espaciadoCaras 2)) (+ (- (/ height 2) (/ espaciadoCaras 2)) (* i espaciadoCaras))  (getFace matriz (+ i 1) 0)); dibuja las cara superior, frontal, inferior y trasera en ese orden (de arriba  a abajo)
    )
  
  (dibujarCara n (- (/ width 2) (/ espaciadoCaras 2) espaciadoCaras) (- (/ height 2) (/ espaciadoCaras 2)) (getFace matriz 4 0)); dibuja la cara izquierda
  (dibujarCara n (- (/ width 2) (/ espaciadoCaras 2) (- 0 espaciadoCaras)) (- (/ height 2) (/ espaciadoCaras 2)) (getFace matriz 5 0)); dibuja la cara derecha
  )


(define (dibujarCara n posx posy cara)

  (for([i (in-range 0 n 1)])
    (for([j (in-range 0 n 1)])
      ((draw-solid-rectangle ventana)(make-posn (+ posx (* i espaciado)) (+ posy (* j espaciado))) ladoCuadro ladoCuadro (getColumn (getRow cara j 0) i 0))
      )
    )
  )

(define (limpiarVentana)
  ((clear-viewport ventana))
  ((draw-solid-rectangle ventana)(make-posn 0 0) width height "gray")
  )

(define (get x lista)
  (cond ((equal? 1 x)(car lista))
        (else (get (= x 1) (cdr lista)))))


(dibujarMatriz 6 (RS 6 '() '("C1A"))) 

;(for([i(in-range 2 7 1)])
;  (dibujarMatriz i)
;  (sleep 1)
;  )




