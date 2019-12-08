(ql:quickload :trivia)
(ql:quickload :lparallel)

(defvar *input*
  (with-open-file (stream #P"./resources/day7/input")
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

(defun run (program read-input write-output)
  (labels ((load-value (param value)
             (ecase param
               (0 (gethash value program))
               (1 value)))
           (handle-op (pc params f)
             (let ((a (load-value (nth 0 params) (gethash (+ pc 1) program)))
                   (b (load-value (nth 1 params) (gethash (+ pc 2) program)))
                   (c (gethash (+ pc 3) program)))
               (setf (gethash c program) (funcall f a b))
               (+ pc 4)))
           (jump-condition (pc params f)
             (let ((a (load-value (nth 0 params) (gethash (+ pc 1) program)))
                   (b (load-value (nth 1 params) (gethash (+ pc 2) program))))
               (if (funcall f a) b (+ pc 3))))
           (run-op (program pc op params)
             (trivia:match op
               (1 (handle-op pc params #'+))
               (2 (handle-op pc params #'*))
               (3 (let ((index (gethash (+ pc 1) program))
                        (value (funcall read-input)))
                    (setf (gethash index program) value)
                    (+ pc 2)))
               (4 (let ((value (load-value (nth 0 params)
                                           (gethash (+ pc 1) program))))
                    (funcall write-output value)
                    (+ pc 2)))
               (5 (jump-condition pc params (lambda (i) (not (eq i 0)))))
               (6 (jump-condition pc params (lambda (i) (eq i 0))))
               (7 (handle-op pc params (lambda (a b) (if (< a b) 1 0))))
               (8 (handle-op pc params (lambda (a b) (if (= a b) 1 0))))
               (99 (error 'program-halt :message "Got opcode 99"))
               (_ (error 'unknown-opcode :opcode op)))))
    (loop with pc = 0
          for (op params) = (multiple-value-list (decode-op (gethash pc program)))
          do (setf pc (run-op program pc op params)))))

(defun init-queue (index phase)
  (let ((queue (lparallel.queue:make-queue)))
    (lparallel.queue:push-queue phase queue)
    (when (= index 0)
      (lparallel.queue:push-queue 0 queue))
    queue))

(defun run-parallel (program-input inputs)
  (let ((channel (lparallel:make-channel))
        (input-queues (loop for index from 0
                            for phase in inputs
                            collect (init-queue index phase)))
        (last-output 0))
    (labels ((last-amplifier (i) (= i (- (length inputs) 1)))
             (read-input (queue)
               (lambda () (lparallel.queue:pop-queue queue)))
             (write-output (i queue)
               (lambda (e)
                 (when (last-amplifier i) (setf last-output e))
                 (lparallel.queue:push-queue e queue))))
      (lparallel:task-handler-bind ((program-halt (lambda (e)
                                                    (declare (ignore e))
                                                    (invoke-restart 'last-output))))
        (dotimes (i (length inputs))
          (lparallel:submit-task
           channel
           (lambda (i)
             (format t "Starting thread: ~a~%" i)
             (let* ((program (list->program program-input))
                    (input-queue (nth i input-queues))
                    (output-queue (if (last-amplifier i)
                                      (nth 0 input-queues)
                                      (nth (+ i 1) input-queues))))
               (restart-case (run program
                                  (read-input input-queue)
                                  (write-output i output-queue))
                 (last-output () last-output))))
           i)))
      (loop for _ in inputs
            maximizing (lparallel:receive-result channel)))))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
                 append (mapcar (lambda (l) (cons element l))
                                (all-permutations (remove element list)))))))

(defun part2 (input)
  (let ((result nil))
    (setf lparallel:*kernel* (lparallel:make-kernel 8))
    (setf result (loop for inputs in (all-permutations '(5 6 7 8 9))
                       maximizing (run-parallel input inputs)))
    (lparallel:end-kernel :wait t)
    result))

(part2 *input*)
