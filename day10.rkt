#lang racket/base

(require (only-in rnrs/base-6 mod)
         racket/file
         racket/function
         racket/list
         racket/match
         racket/math
         racket/string
         threading)

(define filename "./resources/day10/input")
(define file-input (string-split (file->string filename) "\n"))

(define (asteroids input)
  ; in-indexed is like enumerate() for Racket
  ; except that the second value is the index
  (for*/list ([(line row) (in-indexed input)]
              [(ch col) (in-indexed line)]
              #:when (char=? #\# ch))
    ; Use complex numbers to store points.
    ; Racket has a bunch of angle functions for these.
    (+ col (* row +i))))

(define file-asteroids (asteroids file-input))

(define (num-visible point asteroids)
  (length
   (remove-duplicates
    (for/list ([p asteroids]
               #:unless (= p point))
      (angle (- p point))))))

(define (most-visible asteroids)
  (define min-point (argmax (curryr num-visible asteroids) asteroids))
  (values min-point (num-visible min-point asteroids)))

(define-values (best-point part1) (most-visible file-asteroids))

(define (radians point)
  (define diff (- point best-point))
  (mod (+ (atan (imag-part diff) (real-part diff))
          (/ pi 2))
       (* 2 pi)))

(define point-200
  (~> (remove best-point file-asteroids)
      (sort < #:key magnitude)
      ; You aren't gonna need the second round of asteroids
      ; so just remove the farther away astroids with duplicate angles.
      (remove-duplicates (λ (a b) (= (radians a) (radians b))))
      (sort < #:key radians)
      (list-ref (sub1 200))))

(define part2 (+ (* (real-part point-200) 100) (imag-part point-200)))

part1
part2
