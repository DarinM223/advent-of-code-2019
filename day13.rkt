#lang racket/base

(require racket/async-channel
         racket/function
         racket/list
         racket/match
         (prefix-in intcode: "intcode.rkt"))

(define filename "./resources/day13/input")
(define program (intcode:list->program (intcode:read-input filename)))

(define output
  (let* ([results '()]
         [write-value (λ (e) (set! results (cons e results)))])
    (intcode:run program read write-value)
    (reverse results)))

(define result
  (let loop ([grid (hash)]
             [l output])
    (if (< (length l) 3)
        grid
        (match-let ([(list from-left from-top tile) (take l 3)])
          (loop (hash-set grid (list from-left from-top) tile)
                (drop l 3))))))

(define part1 (count (curry = 2) (hash-values result)))

(define (part2 program)
  (define in-chan (make-async-channel))
  (define out-chan (make-async-channel))
  (define results-chan (make-async-channel))
  (define (read-input) (async-channel-get in-chan))
  (define (write-output e) (async-channel-put out-chan e))

  ; Thread that runs program.
  (thread
   (λ () (intcode:run program read-input write-output)))

  ; Thread that listens for padx and ballx signals.
  (thread
   (λ ()
     (let loop ()
       (define in1 (async-channel-get out-chan))
       (define in2 (async-channel-get out-chan))
       (define in3 (async-channel-get out-chan))
       (match (list in1 in2 in3)
         [`(-1 0 ,score) (displayln score)]
         [`(,x ,y 3) (async-channel-put results-chan (list 'padx x))]
         [`(,x ,y 4) (async-channel-put results-chan (list 'ballx x))]
         [_ (void)])
       (loop))))

  ; Thread that feeds input based on padx and ballx.
  (thread
   (λ ()
     (let loop ([padx 0] [ballx 0])
       (match (sync/timeout 0.005 results-chan)
         [(list 'padx x) (loop x ballx)]
         [(list 'ballx x) (loop padx x)]
         [#f
          (async-channel-put in-chan (cond
                                       [(> ballx padx) 1]
                                       [(= ballx padx) 0]
                                       [else -1]))
          (loop padx ballx)])))))

(printf "Part1: ~a~%" part1)

(define play-program (hash-set program 0 2))
(part2 play-program) ; Answer: 14538
