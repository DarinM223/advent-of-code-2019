#lang racket/base

(require threading "intcode.rkt")

(define program (list->program (read-input "./resources/day2/input")))

(define (run-program a b)
  (~> program
      (hash-set 1 a)
      (hash-set 2 b)
      (run read displayln)
      (hash-ref 0)))

(define part1 (run-program 12 2))

(define (part2)
  (for* ([i (in-range 0 99)]
         [j (in-range 0 99)])
    (when (= (run-program i j) 19690720)
      (printf "Result: ~a\n" (+ (* 100 i) j)))))

part1
(part2)
