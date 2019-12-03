#lang racket/base

(require racket/list racket/file racket/match racket/string racket/set threading)

(define filename "./resources/day3/input")
(define file-input (file->string filename))
(define wires (string-split file-input "\n"))

(define (path-points path x y)
  (define amount (string->number (substring path 1)))
  (match (string-ref path 0)
    [#\L (values (- x amount) y (for/list ([i amount]) (list (- x i) y)))]
    [#\R (values (+ x amount) y (for/list ([i amount]) (list (+ x i) y)))]
    [#\D (values x (- y amount) (for/list ([i amount]) (list x (- y i))))]
    [#\U (values x (+ y amount) (for/list ([i amount]) (list x (+ y i))))]))

(define (wire-points wire)
  (define-values (x2 y2 result2)
    (for/fold ([x 0] [y 0] [result '()])
              ([path wire])
      (let-values ([(x y new-results) (path-points path x y)])
        (values x y (foldl cons result new-results)))))
  ; Ugly, but can't find a structure with O(1) append in Racket.
  (reverse result2))

(define (build-grid wire wire-num grid)
  (for/fold ([grid grid])
            ([point wire])
    (if (hash-has-key? grid point)
        (hash-set grid point (set-add (hash-ref grid point) wire-num))
        (hash-set grid point (set wire-num)))))

(define (manhattan-dist p1 p2)
  (match-let ([(list p1x p1y) p1]
              [(list p2x p2y) p2])
    (+ (abs (- p2x p1x)) (abs (- p2y p1y)))))

(define wire1 (wire-points (string-split (car wires) ",")))
(define wire2 (wire-points (string-split (cadr wires) ",")))
(define grid (~>> (hash) (build-grid wire1 1) (build-grid wire2 2)))
(define points
  (for/list ([(k v) grid]
             #:when (> (length (set->list v)) 1))
    k))
(define dists (map (λ (dist) (manhattan-dist dist '(0 0))) points))

(define part1
  (for/fold ([shortest 100000])
            ([dist dists]
             [point points])
    (if (and (> shortest dist) (not (= dist 0)))
        dist
        shortest)))

(define (wires-min-steps wire1 wire2)
  ; let loop works better than for/fold here because
  ; it allows early return.
  (let loop ([p1old (hash)]
             [p2old (hash)]
             [steps 0]
             [wire1 wire1]
             [wire2 wire2])
    (let ([p1 (car wire1)] [p2 (car wire2)])
      (cond
        [(and (equal? p1 p2)
              (not (equal? p1 '(0 0))))
         (* steps 2)]
        [(hash-has-key? p1old p2)
         (+ steps (hash-ref p1old p2))]
        [(hash-has-key? p2old p1)
         (+ steps (hash-ref p2old p1))]
        [else
         (loop (hash-set p1old p1 steps)
               (hash-set p2old p2 steps)
               (+ steps 1)
               (cdr wire1)
               (cdr wire2))]))))

(define part2 (wires-min-steps wire1 wire2))

part1
part2
