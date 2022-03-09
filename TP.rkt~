#lang scheme

; It checks if the list provited isn`t null and calls "counter", "size", "format"
; n = among of i an j
; x = instructions
(define (filter n x)
  (cond ((and (not (null? x)) (number? n) (counter x) (>= 6 n) (<= 2 n) (size n x) (format x)) #t)
        (else #f)))

; It checks if every instruction have 3 characters and if they come in a string
(define (counter x)
  (cond ((null? x) #t)
        (( and (string? (first x)) (equal? 3 (string-length (first x)))) (counter (cdr x)))
        (else #f)))

; It checks if the n value provided by the player is correct in the instructions
(define (size n x)
  (cond ((null? x) #t)
        ((and (middle x)(>= n (string->number(substring (first x) 1 2)))(<= 1 (string->number(substring (first x) 1 2)))) (size n (cdr x)))
        (else #f)))

; It checks if the format in all the instructions is correctly writted
(define (format x)
  (cond ((null? x) #t)
        ((and (equal? "F" (substring (first x) 0 1)) (or (equal? "D" (substring (first x) 2 3)) (equal? "I" (substring (first x) 2 3)))) (format (cdr x)))
        ((and (equal? "C" (substring (first x) 0 1)) (or (equal? "A" (substring (first x) 2 3)) (equal? "B" (substring (first x) 2 3)))) (format (cdr x)))
        (else #f)))

; It checks if the caracter in the middle of the instruction is truly a number
(define (middle x)
  (cond ((string->number (substring (first x) 1 2)) #t)
         (else #f)))