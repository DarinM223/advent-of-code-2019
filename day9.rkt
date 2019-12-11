#lang racket/base

(require "intcode.rkt")

(define program (list->program (read-input "./resources/day9/input")))

; Input 1 to run part 1
; Input 2 to run part 2
(define day9 (run program read displayln))
