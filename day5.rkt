#lang racket/base

(require "intcode.rkt")

(define program (list->program (read-input "./resources/day5/input")))

; Input 1 into command line to run part 1
; Input 5 into command line to run part 2
(define result (run program read displayln)) ; Evaluate this and the answer is in the console output.
