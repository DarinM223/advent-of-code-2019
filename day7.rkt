#lang racket/base

(require data/queue racket/file racket/function racket/match racket/set racket/string threading)
(require racket/list)
(require errortrace)

(define input
  (map string->number
       (~> "./resources/day7/input"
           (file->string)
           (string-trim)
           (string-split ","))))

(define (list->program input)
  (for/hash ([i (in-range 0 (length input))]
             [v (in-list input)])
    (values i v)))

(define program (list->program input))

(define (decode-op opnum)
  (define op (modulo opnum 100))
  (define rest (quotient opnum 100))
  (define params
    (match op
      [1 3]
      [2 3]
      [3 1]
      [4 1]
      [5 2]
      [6 2]
      [7 3]
      [8 3]
      [_ 0]))
  (for/fold ([n rest]
             [l '()]
             #:result (values op (reverse l)))
            ([i params])
    (values (quotient n 10) (cons (modulo n 10) l))))

(define (my-lt? a b) (if (< a b) 1 0))
(define (my-eq? a b) (if (= a b) 1 0))

(define (run program read write-value)
  (define (load-value param value program)
    (match param
      [0 (hash-ref program value)]
      [1 value]
      [_ (error (format "Invalid parameter: ~a\n" param))]))
  (define (handle-op params i program f)
    (let ([a (load-value (list-ref params 0) (hash-ref program (+ i 1)) program)]
          [b (load-value (list-ref params 1) (hash-ref program (+ i 2)) program)]
          [c (hash-ref program (+ i 3))])
      (run-iter (+ i 4) (hash-set program c (f a b)))))
  (define (jump-condition params i program f)
    (let ([a (load-value (list-ref params 0) (hash-ref program (+ i 1)) program)]
          [b (load-value (list-ref params 1) (hash-ref program (+ i 2)) program)])
      (if (f a)
          (run-iter b program)
          (run-iter (+ i 3) program))))
  (define (run-iter i program)
    (define-values (op params) (decode-op (hash-ref program i)))
    (match op
      [1 (handle-op params i program +)]
      [2 (handle-op params i program *)]
      [3
       (let ([index (hash-ref program (+ i 1))]
             [value (read)])
         (run-iter (+ i 2) (hash-set program index value)))]
      [4
       (let ([value (load-value (list-ref params 0)
                                (hash-ref program (+ i 1)) program)])
         (write-value value)
         (run-iter (+ i 2) program))]
      [5 (jump-condition params i program (compose not (curryr = 0)))]
      [6 (jump-condition params i program (curryr = 0))]
      [7 (handle-op params i program my-lt?)]
      [8 (handle-op params i program my-eq?)]
      [99 'halt]
      [_ program]))
  (run-iter 0 program))

(define (simulate program input1 input2)
  (define count-read 0)
  (define l '())
  (define (read-fn)
    (λ ()
      (match count-read
        [0 (set! count-read (+ count-read 1))
           input1]
        [1 (set! count-read (+ count-read 1))
           input2]
        [_ (error "Too many inputs")])))
  (define (write-fn) (λ (a) (set! l (cons a l))))
  (values (run program (read-fn) (write-fn)) l))

(define (run-stages program inputs)
  (for/fold ([input2 0])
            ([input1 inputs])
    (let-values ([(_ result) (simulate program input1 input2)])
      (car result))))

(define (max-thruster program run-stages)
  (for/fold ([sum 0]
             [input '(0 0 0 0 0)]
             #:result sum)
            ([l (in-permutations (range 0 5))])
    (let ((new-sum (run-stages program l)))
      (if (> new-sum sum)
          (values new-sum l)
          (values sum input)))))

(max-thruster program run-stages)
