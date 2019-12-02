#lang racket/base

(require racket/file racket/match racket/string threading)

(define input
  (map string->number
       (~> "./resources/day2/input"
           (file->string)
           (string-trim)
           (string-split ","))))

(define (list->program input)
  (for/hash ([i (in-range 0 (length input))]
             [v (in-list input)])
    (values i v)))

(define program (list->program input))

(define (run program)
  (define (handle-op i program f)
    (let ([a (hash-ref program (hash-ref program (+ i 1)))]
          [b (hash-ref program (hash-ref program (+ i 2)))]
          [c (hash-ref program (+ i 3))])
      (run-iter (+ i 4) (hash-set program c (f a b)))))
  (define (run-iter i program)
    (match (hash-ref program i)
      [1 (handle-op i program +)]
      [2 (handle-op i program *)]
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
