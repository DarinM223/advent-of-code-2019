#lang racket

(require racket/generator threading)

(define filename "./resources/day12/input")
(define (parse-line line)
  (for/list ([binding line])
    (match-define (list _ n) (string-split binding "="))
    (string->number n)))

(define moons
  (map (compose parse-line
                (curryr string-split ",")
                (curryr string-trim ">")
                (curryr string-trim "<"))
       (string-split (string-trim (file->string filename)) "\n")))

(define (unique-pairs lst)
  (for*/list ([i (length lst)]
              [j (range (+ i 1) (length lst))])
    (list i j)))

(define (add-points p1 p2)
  (for/list ([p1c p1] [p2c p2])
    (+ p1c p2c)))

(define (apply-gravity moons vels)
  (for/fold ([vels vels])
            ([pair (unique-pairs moons)])
    (match-define (list i1 i2) pair)
    (define-values (v1-change v2-change)
      (for/lists (v1 v2)
                 ([coord1 (list-ref moons i1)]
                  [coord2 (list-ref moons i2)])
        (cond
          [(< coord1 coord2) (values 1 -1)]
          [(> coord1 coord2) (values -1 1)]
          [else (values 0 0)])))
    (~> vels
        (list-update i1 (curryr add-points v1-change))
        (list-update i2 (curryr add-points v2-change)))))

(define (step moons)
  (generator ()
   (let loop ([moons moons]
              [vels (for/list ([_ (length moons)]) '(0 0 0))])
     (define new-vels (apply-gravity moons vels))
     (define new-moons
       (for/list ([moon moons] [vel new-vels])
         (add-points moon vel)))
     (yield new-moons new-vels)
     (loop new-moons new-vels))))

(define (total-energy moons vels)
  (for/sum ([moon moons] [vel vels])
    (define potential (foldl + 0 (map abs moon)))
    (define kinetic (foldl + 0 (map abs vel)))
    (* potential kinetic)))

(define-values (updated-moons updated-vels)
  (let ([g (step moons)])
    (for/fold ([moons moons] [vels '()])
              ([i 1000])
      (g))))
(define part1 (total-energy updated-moons updated-vels))

(define (get-repeats g)
  (let loop ([step 0]
             [seen (list (set) (set) (set))]
             [repeats (list null null null)])
    (if (= (count null? repeats) 0)
        repeats
        (let*-values ([(moons vels) (g)]
                      [(seen repeats)
                       (for/fold ([seen seen] [repeats repeats])
                                 ([i 3])
                         (define m-axis (map (curryr list-ref i) moons))
                         (define v-axis (map (curryr list-ref i) vels))
                         (define key (append m-axis v-axis))
                         (if (set-member? (list-ref seen i) key)
                             (values seen (if (null? (list-ref repeats i))
                                              (list-set repeats i step)
                                              repeats))
                             (values (list-update seen i (curryr set-add key))
                                     repeats)))])
          (loop (+ step 1) seen repeats)))))
(define repeats (get-repeats (step moons)))

(define (steps-repeats repeats)
  (foldl lcm (car repeats) (cdr repeats)))

(define part2 (steps-repeats repeats))

part1
part2
