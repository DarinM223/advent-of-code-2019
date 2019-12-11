#lang racket/base

(require racket/async-channel
         racket/function
         racket/list
         racket/match
         (prefix-in intcode: "intcode.rkt"))

(define filename "./resources/day11/input2")
(define program (intcode:list->program (intcode:read-input filename)))

(define (rotate90 direction turn-direction)
  (match (list direction turn-direction)
    [(list 'up 'left) 'left]
    [(list 'up 'right) 'right]
    [(list 'right 'left) 'up]
    [(list 'right 'right) 'down]
    [(list 'left 'left) 'down]
    [(list 'left 'right) 'up]
    [(list 'down 'left) 'right]
    [(list 'down 'right) 'left]))

(define (move pos direction)
  (match-define (list x y) pos)
  (match direction
    ['left (list (- x 1) y)]
    ['right (list (+ x 1) y)]
    ['up (list x (- y 1))]
    ['down (list x (+ y 1))]))

(define (run-paint program init-color-num)
  (define in-channel (make-async-channel))
  (define out-channel (make-async-channel))
  (thread
   (λ ()
     (define (read) (async-channel-get in-channel))
     (define (write e) (async-channel-put out-channel e))
     (match (intcode:run program read write)
       [(hash-table ('halt #t)) (void)])))

  (define (get-color grid pos)
    (if (hash-has-key? grid pos)
        (hash-ref grid pos)
        0))

  (define grid (make-hash))
  (define num-painted (make-hash))
  (define pos '(0 0))
  (define direction 'up)

  (with-handlers ([exn:fail? (λ (e) (values grid num-painted))])
    (let loop ([initial #t])
      (define color (if initial init-color-num (get-color grid pos)))
      (async-channel-put in-channel color)
      ; sync/timeout takes in an event and a channel is actually an event.
      (define paint-color (sync/timeout 0.5 out-channel))
      (when (eq? paint-color #f) (error "Timeout"))
      (hash-set! num-painted pos
                 (if (hash-has-key? num-painted pos)
                     (add1 (hash-ref num-painted pos))
                     1))
      (hash-set! grid pos paint-color)
      (define turn-direction
        (if (= (async-channel-get out-channel) 0) 'left 'right))
      (set! direction (rotate90 direction turn-direction))
      (set! pos (move pos direction))
      (loop #f))))

(define (print-grid grid)
  (define min-x (argmin identity (map car (hash-keys grid))))
  (define max-x (argmax identity (map car (hash-keys grid))))
  (define min-y (argmin identity (map cadr (hash-keys grid))))
  (define max-y (argmax identity (map cadr (hash-keys grid))))
  (define (num-to-char n) (if (= n 0) #\. #\#))
  (for ([y (range min-y (+ max-y 1))])
    (for ([x (range min-x (+ max-x 1))])
      (define ch (num-to-char
                  (if (hash-has-key? grid (list x y))
                      (hash-ref grid (list x y))
                      0)))
      (printf "~a" ch))
    (printf "~%")))

(match-define-values (_ num-painted) (run-paint program 0))
(define part1 (count (curryr > 0) (hash-values num-painted)))

(match-define-values (grid _) (run-paint program 1))
(define (part2) (print-grid grid))

part1
(part2)
