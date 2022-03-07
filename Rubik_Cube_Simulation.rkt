#lang racket/base
(provide RS)
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
    ((equal? (car Movs) "C1B") (moveColumn X Cube 0 0 '(3 0 1 2))) ;; El primer 0 se debe cambiar por la columna especificada
    ((equal? (car Movs) "C1A") (moveColumn X Cube 0 0 '(1 2 3 0))) ;; El primer 0 se debe cambiar por la columna especificada
    ((equal? (car Movs) "F1D") (moveRow X Cube 0 0 '(4 5 3 1))) ;; El primer 0 se debe cambiar por la columna especificada
    ((equal? (car Movs) "F1I") (moveRow X Cube 0 0 '(5 4 1 3))) ;; El primer 0 se debe cambiar por la columna especificada
    (else ("No manejado"))
  )
)


;; Do a row movement

(define (moveRow X originalCube new_i actual_face faces_moved)
  (cond ((equal? 6 actual_face) (append '() ))
    ((miembro? actual_face '(4 5 3 1)) (append (list (generateChangedFace_R X originalCube 0 new_i actual_face faces_moved))  (moveRow  X originalCube new_i (+ 1 actual_face) (cdr faces_moved))) )
    (else (append (list (generateChangedFace_R X originalCube 0 new_i actual_face faces_moved))  (moveRow  X originalCube new_i (+ 1 actual_face) faces_moved)))
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

(define (moveColumn X originalCube new_j  actual_face faces_moved)
  (cond ((equal? 6 actual_face) (append '() ))
    ((null? faces_moved) (append (list (generateChangedFace X originalCube 0 new_j actual_face faces_moved))  (moveColumn  X originalCube new_j (+ 1 actual_face) faces_moved)))
    (else (append (list (generateChangedFace X originalCube 0 new_j actual_face faces_moved))  (moveColumn  X originalCube new_j (+ 1 actual_face) (cdr faces_moved))))
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


;; main

(define (RS X Cube Movs)
  (cond ((null? Cube) (RS X (buildCube X '("green" "red" "blue" "orange" "yellow" "white")) Movs) )
    (else (moveCube X Cube Movs))))
  
(RS 3 '() '("F1I"))
(getValue '(((green green green) (green green green) (green green green))
  ((white white white) (red red red) (red red red))
  ((blue blue blue) (blue blue blue) (blue blue blue))
  ((yellow yellow yellow) (orange orange orange) (orange orange orange))
  ((red red red) (yellow yellow yellow) (yellow yellow yellow))
  ((orange orange orange) (white white white) (white white white))) 0 0 0)





;; '(((v v) (v v)) ((r r) (r r)) ((az az) (az az)) ((n n) (n n)) ((am am) (am am)) ((b b) (b b)))
