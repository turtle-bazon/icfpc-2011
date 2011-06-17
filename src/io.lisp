(in-package :icfpc)

(defun name->func (name)
  (symbol-function (intern (concatenate 'string (string-upcase (string name)) "-CARD"))))

(defun func->name (func)
  (let ((str (string (nth-value 2 (function-lambda-expression func)))))
    (string-downcase (subseq str 0 (position #\- str)))))

(defun read-opp-move (s)
  (let ((v1 (read s))
	(v2 (read s))
	(v3 (read s)))
    (ecase v1
      (1 (list :left (name->func v2) v3))
      (2 (list :right(name->func v3) v2)))))

(defun write-my-move (s move)
  (let ((v1 (first move))
	(v2 (second move))
	(v3 (third move)))
    (ecase v1
      (:left  (format s "~A~%~A~%~A~%" 1 (func->name v2) v3))
      (:right (format s "~A~%~A~%~A~%" 2 v3 (func->name v2))))
    (values)))

(defun game-loop (s player-id)
  (let ((counter 10000)
	(prev-opp-move
	 (ecase player-id
	   (0 nil)
	   (1 (read-opp-move s)))))
    (loop
      (if (minusp counter) (return) (decf counter))
      (let ((my-move (make-move prev-opp-move)))
	(apply #'imitate-my-move my-move) ; think of a rollback here
	(write-my-move s my-move))
      (apply #'imitate-opp-move (setf prev-opp-move (read-opp-move s))))))