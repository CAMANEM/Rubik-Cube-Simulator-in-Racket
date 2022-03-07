#lang scheme

; It checks if the list provited isn`t null and calls "counter"
(define (filter n x)
  (cond ((and (not (null? x)) (counter x) (size n x) (format x)) displayln "XD")
        (else (displayln "Radio Check"))))

; It checks if every istruction have 3 characters and if they come in a string
(define (counter x)
  (cond ((null? x) #t)
        (( and (string? (first x)) (equal? 3 (string-length (first x)))) (counter (cdr x)))
        (else #f)))

; It checks if the n value provided by the player is correct in the structions
(define (size n x)
  (cond ((null? x) #t)
        ((equal? (number->string n) (substring (first x) 1 2)) (size n (cdr x)))
        (else #f)))

; It checks if the format in all the istructions is correctly writted
(define (format x)
  (cond ((null? x) #t)
        ((and (equal? "F" (substring (first x) 0 1)) (or (equal? "D" (substring (first x) 2 3)) (equal? "I" (substring (first x) 2 3)))) (format (cdr x)))
        ((and (equal? "C" (substring (first x) 0 1)) (or (equal? "A" (substring (first x) 2 3)) (equal? "B" (substring (first x) 2 3)))) (format (cdr x)))
        (else #f)))


(define (Taste x)
  (displayln (substring (first x) 0 1)))