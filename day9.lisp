(ql:quickload :trivia)

(defvar *input*
  (with-open-file (stream #P"./resources/day9/input")
    (loop for line = (read-line stream nil)
          while line
          append (mapcar #'parse-integer (uiop:split-string line :separator ",")))))

(defun list->program (input)
  (loop with h = (make-hash-table)
        for value in input
        for i from 0
        do (setf (gethash i h) value)
        finally (return h)))

(defun decode-op (opnum)
  (let* ((op (mod opnum 100))
         (rest (floor (/ opnum 100)))
         (num-params (trivia:match op
                       (1 3)
                       (2 3)
                       (3 1)
                       (4 1)
                       (5 2)
                       (6 2)
                       (7 3)
                       (8 3)
                       (9 1)
                       (_ 0))))
    (loop for i = rest then (floor (/ i 10))
          for param = (mod i 10)
          for _ below num-params
          collecting param into params
          finally (return (values op params)))))

(define-condition program-halt (error)
  ((message :initarg :message :accessor program-halt-message :initform nil))
  (:report (lambda (condition stream)
             (format stream "The program halted: ~a"
                     (program-halt-message condition)))))

(define-condition unknown-opcode (error)
  ((opcode :initarg :opcode :accessor unknown-opcode-opcode :initform -1)
   (message :initarg :message :accessor unknown-opcode-message :initform nil))
  (:report (lambda (condition stream)
             (format stream "Unknown opcode ~a: ~a"
                     (unknown-opcode-opcode condition)
                     (unknown-opcode-message condition)))))

(define-condition write-immediate (error)
  ((message :initarg :message :accessor write-immediate-message :initform nil))
  (:report (lambda (condition stream)
             (format stream "Writing to immediate mode: ~a"
                     (write-immediate-message condition)))))

(defun run (program read-input write-output)
  (let ((relative-base 0))
    (labels ((getmemory (i) (or (gethash i program) 0))
             (load-value (param value)
               (ecase param
                 (0 (getmemory value))
                 (1 value)
                 (2 (getmemory (+ relative-base value)))))
             (save-value (param i value)
               (ecase param
                 (0 (setf (gethash i program) value))
                 (1 (error 'write-immediate
                           :message (format nil "i: ~a value: ~a" i value)))
                 (2 (setf (gethash (+ relative-base i) program) value))))
             (handle-op (pc params f)
               (let ((a (load-value (nth 0 params) (getmemory (+ pc 1))))
                     (b (load-value (nth 1 params) (getmemory (+ pc 2))))
                     (c (getmemory (+ pc 3))))
                 (save-value (nth 2 params) c (funcall f a b))
                 (+ pc 4)))
             (jump-condition (pc params f)
               (let ((a (load-value (nth 0 params) (getmemory (+ pc 1))))
                     (b (load-value (nth 1 params) (getmemory (+ pc 2)))))
                 (if (funcall f a) b (+ pc 3))))
             (run-op (pc op params)
               (trivia:match op
                 (1 (handle-op pc params #'+))
                 (2 (handle-op pc params #'*))
                 (3 (let ((index (getmemory (+ pc 1)))
                          (value (funcall read-input)))
                      (save-value (nth 0 params) index value)
                      (+ pc 2)))
                 (4 (let ((value (load-value (nth 0 params)
                                             (getmemory (+ pc 1)))))
                      (funcall write-output value)
                      (+ pc 2)))
                 (5 (jump-condition pc params (lambda (i) (not (eq i 0)))))
                 (6 (jump-condition pc params (lambda (i) (eq i 0))))
                 (7 (handle-op pc params (lambda (a b) (if (< a b) 1 0))))
                 (8 (handle-op pc params (lambda (a b) (if (= a b) 1 0))))
                 (9 (incf relative-base
                          (load-value (nth 0 params) (getmemory (+ pc 1))))
                    (+ pc 2))
                 (99 (error 'program-halt :message "Got opcode 99"))
                 (_ (error 'unknown-opcode :opcode op)))))
      (loop with pc = 0
            for (op params) = (multiple-value-list (decode-op (getmemory pc)))
            do (setf pc (run-op pc op params))))))

(defun my-write (e) (format t "~a~%" e))

; Input 1 to run part 1
; Input 2 to run part 2
(run (list->program *input*) #'read #'my-write)
