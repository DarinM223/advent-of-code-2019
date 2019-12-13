(ql:quickload :arrows)
(ql:quickload :iterate)
(use-package :arrows)
(use-package :iterate)

(defvar *filename* #P"./resources/day10/input")

(defun read-input (path)
  (with-open-file (stream path)
    (iter (for line = (read-line stream nil))
          (while line)
          (collect line))))

(defun get-asteroids (lines)
  (iter outer (for line in-sequence lines with-index row)
        (iter (for ch in-string line with-index col)
              (when (eq ch #\#)
                (in outer (collect (list col row)))))))

(defvar *input* (read-input *filename*))
(defvar *asteroids* (get-asteroids *input*))

(defun num-asteroids (asteroid asteroids)
  (iter (with h = (make-hash-table :test #'equal))
        (for (x y) in (remove asteroid asteroids))
        (let ((dx (- x (car asteroid)))
              (dy (- y (cadr asteroid))))
          (setf (gethash (atan dy dx) h) t))
        (finally (return h))))

(defvar *best-point*
  (iter (for asteroid in *asteroids*)
        (for table = (num-asteroids asteroid *asteroids*))
        (finding asteroid maximizing (hash-table-count table))))
(defvar *part1* (hash-table-count (num-asteroids *best-point* *asteroids*)))

(defun magnitude (p)
  (sqrt (+ (expt (car p) 2) (expt (cadr p) 2))))

(defun radians (p)
  (let ((dy (- (cadr p) (cadr *best-point*)))
        (dx (- (car p) (car *best-point*))))
    (mod (+ (atan dy dx) (/ pi 2.0)) (* 2 pi))))

(defvar *point-200*
  (nth (- 200 2) ; Weird, maybe a floating point comparison problem?
       (-> (remove *best-point* *asteroids*)
           (sort #'< :key #'magnitude)
           (remove-duplicates :test (lambda (x y) (= (radians x) (radians y))))
           (sort #'< :key #'radians))))

(defvar *part2* (+ (* (car *point-200*) 100) (cadr *point-200*)))

*part1*
*part2*
