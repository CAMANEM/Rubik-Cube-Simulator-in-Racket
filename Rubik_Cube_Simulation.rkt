#lang racket/base

(provide buildCube)
(provide moveCube)
(provide getFace)
(provide getValue)
(provide getColumn)
(provide getRow)

;; Rubik's cube simulation


;; ( ([b b  ]) () () () () ())


;; Comprueba si un valor se encuentra en la lista:

(define (miembro? valor lista)
  (cond ((null? lista) #f)
        ((equal? valor (car lista)) #t)
        (else
         (miembro? valor (cdr lista)))))

;; creates a row

(define (generateRow X j color)
  (cond ((equal? X j) (append '())) 
    (else (append (list color) (generateRow X (+ 1 j) color)))
  )
)

;(generateRow 2 0 'b)

;; creates a cube's face

(define (generateFace X i color)
  (cond ((equal? X i) (append '()) )
    (else (append (list(generateRow X 0 color)) (generateFace X (+ 1 i) color)))
  ) 
)

;; creates a matrix of X if a empty list is given

(define (buildCube X colors)
  (cond ((null? colors) append '())
    (else (append (list(generateFace X 0 (car colors))) (buildCube X (cdr colors))))
  )
)



;; Get a value by position
;; (getValue '(((v v) (v v)) ((r r) (r r)) ((az az) (az az)) ((n n) (n n)) ((am am) (am am)) ((b b) (b b))) 0 0 0)

(define (getValue Cube face i j)
  (getColumn (getRow (getFace Cube face 0) i 0) j 0)
)


;; gets the face called

(define (getFace Cube face counter)
  (cond ((equal? face counter) (car Cube) )
    (else (getFace (cdr Cube) face (+ 1 counter) ))
  )
)

;; gets the row called

(define (getRow face i counter) 
  (cond ((equal? i counter) (car face))
    (else (getRow (cdr face) i (+ 1 counter) ))
  )
)


;; gets the value of the column called

(define (getColumn row j counter)
  (cond ((equal? j counter) (car row))
    (else (getColumn (cdr row) j (+ 1 counter)))
  )
)


;; Controller of the Cube movements

(define (moveCube X Cube Movs)
  (cond ((null? Movs) Cube)
    ((and (equal? (substring (car Movs) 0 1) "C") (equal? (substring (car Movs) 2 3) "B") ) (moveColumn X Cube (- (string->number (substring (car Movs)1 2)) 1) 0 '(3 0 1 2) "B")) 
    ((and (equal? (substring (car Movs) 0 1) "C") (equal? (substring (car Movs) 2 3) "A") ) (moveColumn X Cube (- (string->number (substring (car Movs)1 2)) 1) 0 '(1 2 3 0) "A")) 
    ((and (equal? (substring (car Movs) 0 1) "F") (equal? (substring (car Movs) 2 3) "D") ) (moveRow X Cube (- (string->number (substring (car Movs)1 2)) 1) 0 '(4 5 3 1) "D")) 
    ((and (equal? (substring (car Movs) 0 1) "F") (equal? (substring (car Movs) 2 3) "I") ) (moveRow X Cube (- (string->number (substring (car Movs)1 2)) 1) 0 '(5 4 1 3) "I")) 
    (else ("No manejado"))
  )
)


;; Do a row movement

(define (moveRow X originalCube new_i actual_face faces_moved dir)
  (cond ((equal? 6 actual_face) (append '() ))
        ((and (equal? new_i 0) (equal? actual_face 0) (equal? dir "I")) (append (list (rotateClockwise X originalCube actual_face 0)) (moveRow  X originalCube new_i (+ 1 actual_face) faces_moved dir)))
        ((and (equal? new_i 0) (equal? actual_face 0) (equal? dir "D")) (append (list (rotateCounterClockwise X originalCube actual_face (- X 1))) (moveRow  X originalCube new_i (+ 1 actual_face) faces_moved dir)))
        ((and (equal? new_i (- X 1)) (equal? actual_face 2) (equal? dir "D")) (append (list (rotateClockwise X originalCube actual_face 0)) (moveRow  X originalCube new_i (+ 1 actual_face) faces_moved dir)))
        ((and (equal? new_i (- X 1)) (equal? actual_face 2) (equal? dir "I")) (append (list (rotateCounterClockwise X originalCube actual_face (- X 1))) (moveRow  X originalCube new_i (+ 1 actual_face) faces_moved dir)))
    ((miembro? actual_face '(4 5 3 1)) (append (list (generateChangedFace_R X originalCube 0 new_i actual_face faces_moved))  (moveRow  X originalCube new_i (+ 1 actual_face) (cdr faces_moved) dir)) )
    (else (append (list (generateChangedFace_R X originalCube 0 new_i actual_face faces_moved))  (moveRow  X originalCube new_i (+ 1 actual_face) faces_moved dir)))
  )
)


;; creates a changed cube's face by row movement

(define (generateChangedFace_R X originalCube i new_i actual_face faces_moved)
  (cond ((equal? X i) (append '()) )
    ((miembro? actual_face '(4 5 3 1)) (append (list(generateChangedRow_R X originalCube i 0 new_i actual_face (car faces_moved))) (generateChangedFace_R X originalCube (+ 1 i) new_i actual_face faces_moved)) ) ;; 7 para que no suceda
    (else (append (list (generateChangedRow_R X originalCube i 0 7 actual_face faces_moved)) (generateChangedFace_R X originalCube (+ 1 i) new_i actual_face faces_moved) ))
  )
)



;; creates a row by row movement

(define (generateChangedRow_R X originalCube i j new_i face new_face)
  (cond ((equal? X j) (append '()))
  ((equal? i new_i) (append (list (getValue originalCube new_face i j)) (generateChangedRow_R X originalCube i (+ 1 j) new_i face new_face) ))
  (else (append (list (getValue originalCube face i j)) (generateChangedRow_R X originalCube i (+ 1 j) new_i face new_face)))
  )
)
;;(generateChangedRow_R 2 '(((v v) (v v)) ((r r) (r r)) ((az az) (az az)) ((n n) (n n)) ((am am) (am am)) ((b b) (b b))) 0 0 0 0 4)

;;(generateChangedFace_R 2 '(((v v) (v v)) ((r r) (r r)) ((az az) (az az)) ((n n) (n n)) ((am am) (am am)) ((b b) (b b))) 0 0 1 '(4 5 3 1))


;; Do a Column movement

(define (moveColumn X originalCube new_j  actual_face faces_moved dir)
  (cond ((equal? 6 actual_face) (append '() ))
        ((and (equal? new_j 0) (equal? actual_face 4) (equal? dir "B")) (append (list (rotateClockwise X originalCube actual_face 0)) (moveColumn  X originalCube new_j (+ 1 actual_face) faces_moved dir)))
        ((and (equal? new_j 0) (equal? actual_face 4) (equal? dir "A")) (append (list (rotateCounterClockwise X originalCube actual_face (- X 1))) (moveColumn  X originalCube new_j (+ 1 actual_face) faces_moved dir)))
        ((and (equal? new_j (- X 1)) (equal? actual_face 5) (equal? dir "A")) (append (list (rotateClockwise X originalCube actual_face 0)) (moveColumn  X originalCube new_j (+ 1 actual_face) faces_moved dir)) )
        ((and (equal? new_j (- X 1)) (equal? actual_face 5) (equal? dir "B")) (append (list (rotateCounterClockwise X originalCube actual_face (- X 1))) (moveColumn  X originalCube new_j (+ 1 actual_face) faces_moved dir)) )
    ((null? faces_moved) (append (list (generateChangedFace X originalCube 0 new_j actual_face faces_moved))  (moveColumn  X originalCube new_j (+ 1 actual_face) faces_moved dir)))
    (else (append (list (generateChangedFace X originalCube 0 new_j actual_face faces_moved))  (moveColumn  X originalCube new_j (+ 1 actual_face) (cdr faces_moved) dir)))
  )
)


;; creates a changed cube's face by column movement

(define (generateChangedFace X originalCube i new_j actual_face faces_moved)
  (cond ((equal? X i) (append '()) )
    ((null? faces_moved)  (append (list (generateChangedRow X originalCube i 0 7 actual_face faces_moved)) (generateChangedFace X originalCube (+ 1 i) 7 actual_face faces_moved) )) ;; 7 para que no suceda
    (else (append (list(generateChangedRow X originalCube i 0 new_j actual_face (car faces_moved))) (generateChangedFace X originalCube (+ 1 i) new_j actual_face faces_moved)))
  )
)

;; creates a by row a column movement

(define (generateChangedRow X originalCube i j new_j face new_face)
  (cond ((equal? X j) (append '()))
    ((equal? j new_j) (append (list (getValue originalCube new_face i j)) (generateChangedRow X originalCube i (+ 1 j) new_j face new_face) ))
    (else (append (list (getValue originalCube face i j)) (generateChangedRow X originalCube i (+ 1 j) new_j face new_face)))
  )
)

;; ------------ BEGIN ROTATIONS ----------

; rotatate a cube face clockwise
;; X: cube size
;; Cube: the original cube
;; n_face: index of face to rotate
;; j: index that should start at 0 

(define (rotateClockwise X Cube n_face j)
  (cond ((equal? X j) (append '()))
        (else (append (list (generateRowClockwise X Cube n_face (- X 1) j)) (rotateClockwise X Cube n_face (+ j 1))))
  )
)

;; build the rotated row clockwise

(define (generateRowClockwise X Cube n_face i j)
  (cond ((equal? i -1) (append '()))
        (else (append (list (getValue Cube n_face i j)) (generateRowClockwise X Cube n_face (- i 1) j)))
        )
  )


;; rotate a cube face counterclockwise
;; X: cube size
;; Cube: original cube
;; n_face: index of the face to rotate
;; j: indez that should start at X-1

(define (rotateCounterClockwise X Cube n_face j)
  (cond ((equal? j -1) (append '()))
        (else (append (list (generateRowCounterClockwise X Cube n_face 0 j)) (rotateCounterClockwise X Cube n_face (- j 1))))
  )
)


;; builds the rotate row counterclockwise
;; X: cube size
;; Cube: original cube
;; n_face: index of the face to rotate

(define (generateRowCounterClockwise X Cube n_face i j)
  (cond ((equal? X i) (append '()))
        (else (append (list (getValue Cube n_face i j)) (generateRowCounterClockwise X Cube n_face (+ i 1) j)))
        )
  )

;(rotateCounterClockwise 2 '((("green" "green") ("green" "green")) (("red" "red") ("red" "red")) (("blue" "blue") ("blue" "blue")) (("orange" "orange") ("orange" "orange")) (("yellow" "yellow") ("yellow" "yellow"))
;                                                  (("red" "white") ("red" "white"))) 5 1)


;(rotateClockwise 2 '((("green" "green") ("green" "green")) (("red" "red") ("red" "red")) (("blue" "blue") ("blue" "blue")) (("orange" "orange") ("orange" "orange")) (("yellow" "yellow") ("yellow" "yellow"))
;                                                   (("red" "white") ("red" "white"))) 5 0)


;; --------- END ROTATIONS ---------


;; arriba 1 2 3 0 4 5
;; abajo 3 0 1 2 4 5

;; izquierda 0 5 2 4 1 3
;; derecha 0 4 2 5 3 1

;;cuidado con las rotaciones laterales!!!



  
;(RS 3 '() '("F1I"))
;(getValue '(((green green green) (green green green) (green green green))
;  ((white white white) (red red red) (red red red))
;  ((blue blue blue) (blue blue blue) (blue blue blue))
;  ((yellow yellow yellow) (orange orange orange) (orange orange orange))
;  ((red red red) (yellow yellow yellow) (yellow yellow yellow))
;  ((orange orange orange) (white white white) (white white white))) 0 0 0)





;; '(((v v) (v v)) ((r r) (r r)) ((az az) (az az)) ((n n) (n n)) ((am am) (am am)) ((b b) (b b)))
