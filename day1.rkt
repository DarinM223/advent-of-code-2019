#lang racket/base

(require racket/file racket/string)

(define lines (filter (λ (s) (> (string-length (string-trim s)) 0))
                      (file->lines "./resources/day1/input")))
(define nums (map string->number lines))

(define (fuel mass)
  (- (floor (/ mass 3)) 2))

(define part1 (foldl + 0 (map fuel nums)))

(define (fuel2 mass)
  (define needed-fuel (fuel mass))
  (if (<= (fuel needed-fuel) 0)
      needed-fuel
      (+ needed-fuel (fuel2 needed-fuel))))

(define part2 (foldl + 0 (map fuel2 nums)))

part1
part2
