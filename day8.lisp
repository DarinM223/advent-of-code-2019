(defun read-chunk (stream size)
  (loop for _ below size
        for ch = (read-char stream nil)
        while ch
        collect ch))

(defun count-ch (l test)
  (loop for ch in l count (eq ch test)))

(defun update-layer (layer chunk)
  (loop for i in layer
        for j in chunk
        if (eq i #\2) collect j
        else collect i))

(defun print-char (ch)
  (ecase ch
    (#\0 #\ )
    (#\1 #\1)))

(defun day8 (width height)
  (let ((chunk-size (* width height)))
    (with-open-file (stream #P"./resources/day8/input")
      (let* ((init-zeros (loop for i from 0 below chunk-size collect #\0))
             (part1 (loop with least-zeros = init-zeros
                          for chunk = (read-chunk stream chunk-size)
                          while (= (length chunk) chunk-size)
                          when (< (count-ch chunk #\0)
                                  (count-ch least-zeros #\0))
                            do (setf least-zeros chunk)
                          finally (return (* (count-ch least-zeros #\1)
                                             (count-ch least-zeros #\2))))))
        (format t "Part 1: ~a~%" part1)))
    (with-open-file (stream #P"./resources/day8/input")
      (let ((part2 (loop for chunk = (read-chunk stream chunk-size)
                         while (= (length chunk) chunk-size)
                         for layer = chunk then (update-layer layer chunk)
                         finally (return layer))))
        (format t "Part 2: ~%")
        (loop for counter from 1
              for i in part2
              if (= (mod counter 25) 0) do (format t "~a~%" (print-char i))
              else do (format t "~a" (print-char i)))))))

(day8 25 6)
