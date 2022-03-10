#lang scheme
(require (lib "graphics.ss" "graphics")) (open-graphics)
(require plot racket/class racket/gui/base)
(require "Rubik_Cube_Simulation.rkt")
(require"TP.rkt")

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
  ((draw-solid-rectangle ventana)(make-posn 0 0) width height "black")
  )

(define (get x lista)
  (cond ((equal? 1 x)(car lista))
        (else (get (= x 1) (cdr lista)))))


(define (drawingCycle X Cube Movs) 
  (cond ((null? Movs) (print "drawing finished")) 
        (else (sleep/yield 3) (dibujarMatriz X (moveCube X Cube Movs)) (drawingCycle X (moveCube X Cube Movs) (cdr Movs))) 
  ) 
)

(define (validateCube X Cube)
  (cond ((and (equal? (length Cube) 6)  (equal? (length (car Cube) ) X) (equal? (length (caar Cube)) X)) #true)
        (else #false)
        )
  )


(define (RS X Cube Movs)
  (cond ((null? Cube) (RS X (buildCube X '("green" "red" "blue" "orange" "yellow" "white")) Movs) )
    ((filter X Movs) (dibujarMatriz X Cube) (drawingCycle X Cube Movs))
    (else (print "found Error in validation")(close-viewport ventana))
  )
)

;(RS 2 '( (("green" "green") ("green" "green")) (("red" "red") ("red" "red")) (("blue" "blue") ("blue" "blue")) (("orange" "orange") ("orange" "orange")) (("yellow" "yellow") ("yellow" "yellow"))
;                                               (("white" "white") ("white" "white"))) '("C2A" "F1D"))

(RS 3 '() '("F1D" "C3B" "C3B"))


;;Agregar validacion (and (equal? (length Cube) 6) (equal? (lenght (car Cube)) X) 

;(sleep 2)
;(dibujarMatriz 3 (RS 6 '() '("C1A"))) 
;(for([i(in-range 2 7 1)])
;  (dibujarMatriz i)
;  (sleep 1)
;  )



