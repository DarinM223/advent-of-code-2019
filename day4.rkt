#lang racket/base

(require racket/function racket/list threading)

(define input "178416-676461")
(define nums (range 178416 676461))

; group-by identity = group in Haskell
; Use group to bunch up equal numbers into groups.
; Example: (group-by identity '(1 2 2 3 4 3)) => '((1) (2 2) (3 3) (4))
(define (check-number num)
  (and (equal? num (sort num char<?))
       (not (= (length num) (length (group-by identity num))))))

(define (count-nums check)
  (~>> nums
       (map (compose string->list number->string))
       (count check)))

(define part1 (count-nums check-number))

; curry curries from left to right
; curryr curries from right to left
; Examples:
; ((curry > 2) 1) => #t
; ((curryr > 1) 2) => #t
(define (check-number2 num)
  (and (check-number num)
       (> (count (compose (curry = 2) length)
                 (group-by identity num))
          0)))

(define part2 (count-nums check-number2))

part1
part2
