#lang scheme
(require (lib "graphics.ss" "graphics")) (open-graphics)
(require plot racket/class racket/gui/base)
(require "Rubik_Cube_Simulation.rkt")
(require"TP.rkt")

(define i 0)
(define j 0)
(define width 1000)
(define height 800)
(define sideSquare 17)
(define division 20)
(define ventana (open-viewport "Cubo Rubik" width height))

(define (drawCube n matriz)
  (define faceDivision (+ sideSquare (* n division) 11))
  (clearWindow)
  

  (for([i (in-range -1 3 1)])
    (drawFace n (- (/ width 2) (/ faceDivision 2)) (+ (- (/ height 2) (/ faceDivision 2)) (* i faceDivision))  (getFace matriz (+ i 1) 0)); drwa faces over, front, under and rear in this order (up to down)
    )
  
  (drawFace n (- (/ width 2) (/ faceDivision 2) faceDivision) (- (/ height 2) (/ faceDivision 2)) (getFace matriz 4 0)); draw left face
  (drawFace n (- (/ width 2) (/ faceDivision 2) (- 0 faceDivision)) (- (/ height 2) (/ faceDivision 2)) (getFace matriz 5 0)); draw right face
  )


(define (drawFace n posx posy cara)

  (for([i (in-range 0 n 1)])
    (for([j (in-range 0 n 1)])
      ((draw-solid-rectangle ventana)(make-posn (+ posx (* i division)) (+ posy (* j division))) sideSquare sideSquare (getColumn (getRow cara j 0) i 0))
      )
    )
  )

(define (clearWindow)
  ((clear-viewport ventana))
  ((draw-solid-rectangle ventana)(make-posn 0 0) width height "black")
  )

(define (drawingCycle X Cube Movs) 
  (cond ((null? Movs) (print "drawing finished")) 
        (else (sleep/yield 3) (drawCube X (moveCube X Cube Movs)) (drawingCycle X (moveCube X Cube Movs) (cdr Movs))) 
  ) 
)

(define (validateCube X Cube)
  (cond ((and (equal? (length Cube) 6)  (equal? (length (car Cube) ) X) (equal? (length (caar Cube)) X)) #true)
        (else #false)
        )
  )

(define (RS X Cube Movs)
  (cond ((null? Cube) (RS X (buildCube X '("green" "red" "blue" "orange" "yellow" "white")) Movs) )
    ((and (user_filter X Cube) (filter X Movs)) (drawCube X Cube) (drawingCycle X Cube Movs))
    (else (print "found Error in validation")(close-viewport ventana))
  )
)

;(RS 2 '((("green" "green") ("green" "green")) (("red" "red") ("red" "red")) (("blue" "blue") ("blue" "blue")) (("orange" "orange") ("orange" "orange")) (("yellow" "yellow") ("yellow" "yellow"))
;                                               (("white" "white") ("white" "white"))) '("C2A" "F1D"))

(RS 3 '() '("F1D" "C3B" "C3B"))

