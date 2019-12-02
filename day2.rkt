#lang racket/base

(require racket/base racket/file racket/match racket/string threading)

(define lines (~>> "./resources/day2/input"
                   (file->lines)
                   (filter (λ (l) (> (string-length (string-trim l)) 0)))))
(define input (map string->number (string-split (car lines) ",")))

(define (list->program input)
  (for/hash ([i (in-range 0 (length input))]
             [v (in-list input)])
    (values i v)))

(define program (list->program input))

(define (run program)
  (define (run-iter i program)
    (define op (hash-ref program i))
    (match op
      [1
       (let* ([a-idx (hash-ref program (+ i 1))]
              [b-idx (hash-ref program (+ i 2))]
              [c-idx (hash-ref program (+ i 3))]
              [a-value (hash-ref program a-idx)]
              [b-value (hash-ref program b-idx)])
         (run-iter (+ i 4) (hash-set program c-idx (+ a-value b-value))))]
      [2
       (let* ([a-idx (hash-ref program (+ i 1))]
              [b-idx (hash-ref program (+ i 2))]
              [c-idx (hash-ref program (+ i 3))]
              [a-value (hash-ref program a-idx)]
              [b-value (hash-ref program b-idx)])
         (run-iter (+ i 4) (hash-set program c-idx (* a-value b-value))))]
      [_ program]))
  (run-iter 0 program))

(define (run-program a b)
  (~> program
      (hash-set 1 a)
      (hash-set 2 b)
      (run)
      (hash-ref 0)))

(define part1 (run-program 12 2))

(define (part2)
  (for* ([i (in-range 0 99)]
         [j (in-range 0 99)])
    (when (= (run-program i j) 19690720)
      (printf "Result: ~a\n" (+ (* 100 i) j)))))

part1
(part2)
