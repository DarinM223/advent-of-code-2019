(ql:quickload :iterate)
(use-package :iterate)

(declaim (optimize (speed 3) (safety 0)))

(defun read-input (stream)
  (remove-if-not #'integerp
                 (iter (for ch = (read-char stream nil))
                       (while ch)
                       (collect (digit-char-p ch)))))

(defparameter *input*
  (with-open-file (stream #P"./resources/day16/input")
    (read-input stream)))

(defparameter *base-sequence* #(0 1 0 -1))

(defun calc-pos (base pos nums)
  (iter (with repeat = pos)
        (with curr-idx = 0)
        (for n in-vector nums with-index i)
        (when (= repeat 0)
          (setf repeat (+ pos 1) curr-idx (+ curr-idx 1)))
        (setf repeat (- repeat 1))
        (summing (* (aref base (mod curr-idx 4)) n) into sum)
        (finally (return (mod (abs sum) 10)))))

(defun fft (nums result)
  (iter (for n in-vector nums with-index i)
        (setf (aref result i) (calc-pos *base-sequence* i nums))))

(defun list-to-array (lst len)
  (iter (with array = (make-array len))
        (for n in-sequence lst with-index i)
        (setf (aref array i) n)
        (finally (return array))))

(defun list-to-num (lst)
  (iter (for n in lst)
        (for i first n then (+ (* i 10) n))
        (finally (return i))))

(defun run-fft (nums-list num-phases)
  (let* ((result-len (length nums-list))
         (nums-array (list-to-array nums-list result-len)))
    (iter (repeat num-phases)
          (for input first nums-array then result)
          (for result = (make-array result-len))
          (fft input result)
          (finally (return result)))))

(defvar *part1-nums* (run-fft *input* 100))

(defvar *part1*
  (list-to-num (iter (for i below 8) (collect (aref *part1-nums* i)))))

(defvar *repeated-input* (iter (repeat 10000) (appending *input*)))
(defvar *offset* (iter (repeat 7) (for n in *repeated-input*) (collect n)))
(defvar *offset-num* (iter (for num in *offset*)
                           (for n first num then (+ (* n 10) num))
                           (finally (return n))))
(defun run-phase (num-list)
  (iter (for l first nil then (cons (mod (abs s) 10) l))
        (for n in (reverse num-list))
        (summing n into s)
        (finally (return l))))

(defvar *result*
  (iter (repeat (+ 100 1))
        (for input first (nthcdr *offset-num* *repeated-input*) then (run-phase input))
        (finally (return input))))

(defvar *part2*
  (list-to-num (iter (repeat 8) (for n in *result*) (collect n))))

*part1*
*part2*
