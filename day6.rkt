#lang racket/base

(require data/queue racket/file racket/function racket/match racket/set racket/string threading)

(define file-name "./resources/day6/input")
(define file-input (string-split (file->string file-name) "\n"))
(define edges (map (curryr string-split ")") file-input))

(define (add-hash h a b)
  (if (hash-has-key? h a)
      (hash-set h a (set-add (hash-ref h a) b))
      (hash-set h a (set b))))

(define (build-graph edges)
  (for/fold ([h (hash)])
            ([edge edges])
    (add-hash h (cadr edge) (car edge))))

(define (node-walk curr graph)
  (if (hash-has-key? graph curr)
      (add1 (for/sum ([edge (hash-ref graph curr)])
              (node-walk edge graph)))
      1))

(define (total-orbits graph)
  (for/sum ([(k v) graph])
    (sub1 (node-walk k graph))))

(define graph (build-graph edges))
(define part1 (total-orbits graph))

(define (build-graph2 edges)
  (for/fold ([h (hash)])
            ([edge edges])
    (match-let ([(list a b) edge])
      (~> h (add-hash a b) (add-hash b a)))))

; Yuck
(define (bfs start end graph)
  (define flag (mutable-set start))
  (define pred (make-hash '((start -1))))
  (define q (make-queue))
  (enqueue! q start)
  (let loop ()
    (when (not (queue-empty? q))
      (let ([v (dequeue! q)])
        (when (and (not (eq? v end)) (hash-has-key? graph v))
          (for ([w (hash-ref graph v)])
            (when (not (set-member? flag w))
              (set-add! flag w)
              (hash-set! pred w v)
              (enqueue! q w)
              (loop)))))))
  (let trace-backwards ([curr end] [acc 0])
    (if (hash-has-key? pred curr)
        (trace-backwards (hash-ref pred curr) (+ acc 1))
        acc)))

(define graph2 (build-graph2 edges))
(define part2 (- (bfs "YOU" "SAN" graph2) 3))

part1
part2
