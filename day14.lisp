(ql:quickload :iterate)
(use-package :iterate)

(defvar *filename* #P"./resources/day14/input")

(defun parse-materials (materials)
  (let ((parsed (mapcar
                  (lambda (x) (uiop:split-string (string-trim " " x)))
                  (uiop:split-string (string-trim " " materials) :separator ","))))
    (iter (for (num material) in parsed)
          (collect (list (parse-integer num) material)))))

(defun read-input (path)
  (with-open-file (stream path)
    (iter (for line = (read-line stream nil))
          (while line)
          (for (left right) = (remove-if (lambda (s) (= (length s) 0))
                                         (uiop:split-string line :separator "=>")))
          (collect (list (parse-materials left)
                         (car (parse-materials right)))))))

(defvar *input* (read-input *filename*))

(defun acc-reqs (amount reqs f)
  (iter (for (num req) in reqs)
        (collect (list (funcall f num amount) req))))

(defun make-material-map (input)
  (iter (with hash = (make-hash-table :test #'equal))
        (for (materials (rnum rmaterial)) in input)
        (setf (gethash rmaterial hash) (list rnum materials))
        (finally (return hash))))

(defvar *material-map* (make-material-map *input*))

(defun calc-material (material need material-map extra)
  (multiple-value-bind (r ok) (gethash material material-map)
    (let ((produced (car r))
          (reqs (cadr r)))
      (if (not ok)
          need
          (let* ((have (or (gethash material extra) 0))
                 (actual-need (max 0 (- need have)))
                 (times (ceiling actual-need produced))
                 (material-extra (if (= (mod actual-need produced) 0)
                                     0
                                     (- produced (mod actual-need produced)))))
            (setf (gethash material extra) (- have (- need actual-need)))
            (iter (for (req-amount req) in reqs)
                  (summing (calc-material req (* times req-amount) material-map extra) into total)
                  (finally
                   (setf (gethash material extra)
                         (+ (or (gethash material extra) 0) material-extra))
                   (return total))))))))

(defun calc (material need)
  (calc-material material need *material-map* (make-hash-table :test #'equal)))

(defvar *part1* (calc "FUEL" 1))

(defvar *available-ore* 1000000000000)

(defvar *upper-bound*
  (iter (for i first 1 then (* i 2))
        (while (< (calc "FUEL" i) *available-ore*))
        (finally (return i))))

(defun binary-search (lower-bound upper-bound f target)
  (iter (with result = 0)
        (for mid = (truncate (+ lower-bound upper-bound) 2))
        (while (<= lower-bound upper-bound))
        (cond
          ((> (funcall f mid) target) (setf upper-bound (- mid 1)))
          ((< (funcall f mid) target) (setf lower-bound (+ mid 1)))
          (t (setf result mid)))
        (finally (return mid))))

(defvar *part2*
  (binary-search (/ *upper-bound* 2)
                 *upper-bound*
                 (lambda (i) (calc "FUEL" i))
                 *available-ore*))

*part1*
*part2*
