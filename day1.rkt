#lang racket/base

(require racket/file)
(require racket/string)
(require threading)

(define lines (filter (λ (s) (> (string-length (string-trim s)) 0))
                      (file->lines "./resources/day1/input")))
(define nums (map (λ (s) (string->number s)) lines))

(define (fuel mass)
  (- (floor (/ mass 3)) 2))

(define part1
  (~>> nums
      (map (λ (num) (fuel num)))
      (foldl + 0)))

(define (fuel2 mass)
  (define needed-fuel (fuel mass))
  (if (<= (fuel needed-fuel) 0)
      needed-fuel
      (+ needed-fuel (fuel2 needed-fuel))))

(define part2
  (~>> nums
       (map (λ (num) (fuel2 num)))
       (foldl + 0)))

part1
part2
