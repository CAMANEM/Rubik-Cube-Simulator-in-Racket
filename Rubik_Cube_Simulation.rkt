#lang racket/base

(provide buildCube)
(provide moveCube)
(provide getFace)
(provide getValue)
(provide getColumn)
(provide getRow)



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
;; X: cube size
;; originalCube: previous cube
;; new_i: index of row affected by the movement
;; actual_face: index of the creating face
;; faces_moved: list in order of index with the affected faces tha have to be used to rewrite
;; dir: direction of the movement (A, B, D or I)

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
;; X: cube size
;; originalCube: previous cube
;; new_i: index of row affected by the movement
;; actual_face: index of the creating face
;; faces_moved: list in order of index with the affected faces tha have to be used to rewrite

(define (generateChangedFace_R X originalCube i new_i actual_face faces_moved)
  (cond ((equal? X i) (append '()) )
    ((miembro? actual_face '(4 5 3 1)) (append (list(generateChangedRow_R X originalCube i 0 new_i actual_face (car faces_moved))) (generateChangedFace_R X originalCube (+ 1 i) new_i actual_face faces_moved)) ) ;; 7 para que no suceda
    (else (append (list (generateChangedRow_R X originalCube i 0 7 actual_face faces_moved)) (generateChangedFace_R X originalCube (+ 1 i) new_i actual_face faces_moved) ))
  )
)



;; creates a row by row movement
;; X: cube size
;; originalCube: previous cube
;; new_i: index of row affected by the movement
;; face: index of the creating face

(define (generateChangedRow_R X originalCube i j new_i face new_face)
  (cond ((equal? X j) (append '()))
  ((equal? i new_i) (append (list (getValue originalCube new_face i j)) (generateChangedRow_R X originalCube i (+ 1 j) new_i face new_face) ))
  (else (append (list (getValue originalCube face i j)) (generateChangedRow_R X originalCube i (+ 1 j) new_i face new_face)))
  )
)
;;(generateChangedRow_R 2 '(((v v) (v v)) ((r r) (r r)) ((az az) (az az)) ((n n) (n n)) ((am am) (am am)) ((b b) (b b))) 0 0 0 0 4)

;;(generateChangedFace_R 2 '(((v v) (v v)) ((r r) (r r)) ((az az) (az az)) ((n n) (n n)) ((am am) (am am)) ((b b) (b b))) 0 0 1 '(4 5 3 1))


;; Do a Column movement
;; X: cube size
;; originalCube: previous cube
;; new_j: index of column affected by the movement
;; actual_face: index of the creating face
;; faces_moved: list in order of index with the affected faces tha have to be used to rewrite
;; dir: direction of the movement (A, B, D or I)

(define (moveColumn X originalCube new_j  actual_face faces_moved dir)
  (cond ((equal? 6 actual_face) (append '() ))
        ((and (equal? new_j 0) (equal? actual_face 4) (equal? dir "B")) (append (list (rotateClockwise X originalCube actual_face 0)) (moveColumn  X originalCube new_j (+ 1 actual_face) faces_moved dir)))
        ((and (equal? new_j 0) (equal? actual_face 4) (equal? dir "A")) (append (list (rotateCounterClockwise X originalCube actual_face (- X 1))) (moveColumn  X originalCube new_j (+ 1 actual_face) faces_moved dir)))
        ((and (equal? new_j (- X 1)) (equal? actual_face 5) (equal? dir "A")) (append (list (rotateClockwise X originalCube actual_face 0)) (moveColumn  X originalCube new_j (+ 1 actual_face) faces_moved dir)) )
        ((and (equal? new_j (- X 1)) (equal? actual_face 5) (equal? dir "B")) (append (list (rotateCounterClockwise X originalCube actual_face (- X 1))) (moveColumn  X originalCube new_j (+ 1 actual_face) faces_moved dir)) )
    ((null? faces_moved) (append (list (generateChangedFace X originalCube 0 new_j actual_face faces_moved (- X 1)))  (moveColumn  X originalCube new_j (+ 1 actual_face) faces_moved dir)))
    (else (append (list (generateChangedFace X originalCube 0 new_j actual_face faces_moved (- X 1)))  (moveColumn  X originalCube new_j (+ 1 actual_face) (cdr faces_moved) dir)))
  )
)


;; creates a changed cube's face by column movement
;; X: cube size
;; originalCube: previous cube
;; new_j: index of column affected by the movement
;; actual_face: index of the creating face
;; faces_moved: list in order of index with the affected faces tha have to be used to rewrite

(define (generateChangedFace X originalCube i new_j actual_face faces_moved i_invert)
  (cond ((equal? X i) (append '()) )
    ((null? faces_moved)  (append (list (generateChangedRow X originalCube i 0 7 actual_face faces_moved i_invert)) (generateChangedFace X originalCube (+ 1 i) 7 actual_face faces_moved (- i_invert 1)) )) ;; 7 para que no suceda
    (else (append (list(generateChangedRow X originalCube i 0 new_j actual_face (car faces_moved) i_invert)) (generateChangedFace X originalCube (+ 1 i) new_j actual_face faces_moved (- i_invert 1))))
  )
)

;; creates a row by a column movement
;; X: cube size
;; originalCube: previous cube
;; new_j: index of column affected by the movement
;; face: index of the creating face
;; new_face: index of the creating face

(define (generateChangedRow X originalCube i j new_j face new_face i_invert)
  (cond ((equal? X j) (append '()))
    ;((and (equal? face 3) (equal? j new_j)) (append (list (getValue originalCube new_face i_invert j)) (generateChangedRow X originalCube i (+ 1 j) new_j face new_face i_invert) ));parche 
    ((equal? j new_j) (append (list (getValue originalCube new_face i j)) (generateChangedRow X originalCube i (+ 1 j) new_j face new_face i_invert) ))
    (else (append (list (getValue originalCube face i j)) (generateChangedRow X originalCube i (+ 1 j) new_j face new_face i_invert)))
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
;; X: cube size
;; Cube: original cube
;; n_face: index of the face to rotate

(define (generateRowClockwise X Cube n_face i j)
  (cond ((equal? i -1) (append '()))
        (else (append (list (getValue Cube n_face i j)) (generateRowClockwise X Cube n_face (- i 1) j)))
        )
  )


;; rotate a cube face counterclockwise
;; X: cube size
;; Cube: original cube
;; n_face: index of the face to rotate
;; j: index that should start at X-1

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



;; --------- END ROTATIONS ---------

