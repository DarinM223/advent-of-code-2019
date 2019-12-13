(ql:quickload :iterate)
(use-package :iterate)

(defun read-chunk (stream size)
  (iter (repeat size)
        (for ch = (read-char stream nil))
        (while ch)
        (collect ch)))

(defmacro-clause (FOR var IN-CHUNKS stream WITH-SIZE chunk-size)
  "Over chunks of a specific size in a stream"
  `(progn
     (for ,var = (read-chunk ,stream ,chunk-size))
     (while (= (length chunk) ,chunk-size))))

(defun count-ch (l test)
  (iter (for ch in l) (counting (eq ch test))))

(defun update-layer (layer chunk)
  (iter (for i in layer)
        (for j in chunk)
        (if (eq i #\2) (collect j) (collect i))))

(defun print-char (ch)
  (ecase ch
    (#\0 #\ )
    (#\1 #\1)))

(defun day8 (width height)
  (let ((chunk-size (* width height)))
    (with-open-file (stream #P"./resources/day8/input")
      (let ((part1 (iter (for chunk in-chunks stream with-size chunk-size)
                         (finding chunk minimizing (count-ch chunk #\0) into least-zeros)
                         (finally (return (* (count-ch least-zeros #\1)
                                             (count-ch least-zeros #\2)))))))
        (format t "Part 1: ~a~%" part1)))
    (with-open-file (stream #P"./resources/day8/input")
      (let ((part2 (iter (for chunk in-chunks stream with-size chunk-size)
                         (for layer first chunk then (update-layer layer chunk))
                         (finally (return layer)))))
        (format t "Part 2: ~%")
        (iter (for counter from 1)
              (for i in part2)
              (if (= (mod counter 25) 0)
                  (format t "~a~%" (print-char i))
                  (format t "~a" (print-char i))))))))

(day8 25 6)
