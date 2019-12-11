#lang racket/base

(require racket/async-channel
         racket/function
         racket/list
         racket/match
         "intcode.rkt")

(define program (list->program (read-input "./resources/day7/input")))

(define (simulate program input1 input2)
  (define l '())
  (define (read-fn)
    (define count-read 0)
    (λ ()
      (match count-read
        [0 (set! count-read (+ count-read 1))
           input1]
        [1 (set! count-read (+ count-read 1))
           input2]
        [_ (error "Too many inputs")])))
  (define (write-fn) (λ (a) (set! l (cons a l))))
  (run program (read-fn) (write-fn))
  l)

(define (run-stages program inputs)
  (for/fold ([input2 0])
            ([input1 inputs])
    (car (simulate program input1 input2))))

(define (find-best program run inputs)
  (argmax identity (for/list ([input inputs])
                     (run program input))))

(define part1 (find-best program run-stages (in-permutations (range 0 5))))

(define (run-async program inputs)
  (define (last-amplifier i) (= i (- (length inputs) 1)))
  (define channels (for/vector ([_ inputs]) (make-async-channel)))
  (define last-output 0)

  (define workers
    (for/list ([(phase i) (in-indexed inputs)])
      (thread
       (λ ()
         (define in-channel (vector-ref channels i))
         (async-channel-put in-channel phase)
         (when (= i 0) (async-channel-put in-channel 0))

         (define (read-fn) (async-channel-get in-channel))
         (define (write-fn e)
           (if (last-amplifier i)
               (begin
                 (set! last-output e)
                 (async-channel-put (vector-ref channels 0) e))
               (async-channel-put (vector-ref channels (+ i 1)) e)))
         (let loop ([program program])
           (match (run program read-fn write-fn)
             [(hash-table ('halt #t)) (void)]
             [updated (loop updated)]))))))
  (for-each thread-wait workers)
  last-output)

(define part2 (find-best program run-async (in-permutations (range 5 10))))

part1
part2
