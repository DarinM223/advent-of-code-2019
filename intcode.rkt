#lang racket/base

(require racket/file racket/function racket/match racket/string threading)

(provide read-input
         list->program
         decode-op
         run)

(define (read-input filename)
  (map string->number
       (~> filename
           (file->string)
           (string-trim)
           (string-split ","))))

(define (list->program input)
  (~> (for/hash ([(v i) (in-indexed input)])
        (values i v))
      (hash-set 'relative-base 0)
      (hash-set 'halt #f)))

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
      [9 1]
      [_ 0]))
  (for/fold ([n rest]
             [l '()]
             #:result (values op (reverse l)))
            ([i params])
    (values (quotient n 10) (cons (modulo n 10) l))))

(define (my-lt? a b) (if (< a b) 1 0))
(define (my-eq? a b) (if (= a b) 1 0))

(define (run program read write-value)
  (define (get-memory program i)
    (if (hash-has-key? program i)
        (hash-ref program i)
        0))
  (define (load-value program param pc)
    (define value (get-memory program pc))
    (match param
      [0 (get-memory program value)]
      [1 value]
      [2 (get-memory program (+ (hash-ref program 'relative-base) value))]
      [_ (error (format "Invalid parameter: ~a\n" param))]))
  (define (save-value program param pc value)
    (define index (get-memory program pc))
    (match param
      [0 (hash-set program index value)]
      [1 (error "Writing to immediate mode")]
      [2 (hash-set program (+ (hash-ref program 'relative-base) index) value)]))
  (define (handle-op params pc program f)
    (define a (load-value program (list-ref params 0) (+ pc 1)))
    (define b (load-value program (list-ref params 1) (+ pc 2)))
    (run-iter (+ pc 4)
              (save-value program (list-ref params 2) (+ pc 3) (f a b))))
  (define (jump-condition params pc program f)
    (define a (load-value program (list-ref params 0) (+ pc 1)))
    (define b (load-value program (list-ref params 1) (+ pc 2)))
    (if (f a)
        (run-iter b program)
        (run-iter (+ pc 3) program)))
  (define (run-iter pc program)
    (define-values (op params) (decode-op (get-memory program pc)))
    (match op
      [1 (handle-op params pc program +)]
      [2 (handle-op params pc program *)]
      [3 (run-iter (+ pc 2)
                   (save-value program (list-ref params 0) (+ pc 1) (read)))]
      [4
       (write-value (load-value program (list-ref params 0) (+ pc 1)))
       (run-iter (+ pc 2) program)]
      [5 (jump-condition params pc program (compose not (curryr = 0)))]
      [6 (jump-condition params pc program (curryr = 0))]
      [7 (handle-op params pc program my-lt?)]
      [8 (handle-op params pc program my-eq?)]
      [9 (run-iter (+ pc 2)
                   (hash-set program 'relative-base
                             (+ (hash-ref program 'relative-base)
                                (load-value program (list-ref params 0) (+ pc 1)))))]
      [99 (hash-set program 'halt #t)]
      [_ program]))
  (run-iter 0 program))
