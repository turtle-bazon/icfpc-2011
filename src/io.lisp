(in-package :icfpc)

(defun name->func (name)
  (symbol-function (intern (concatenate 'string (string-upcase (string name)) "-CARD") :icfpc)))

(defun func->name (func)
  (let* ((str  (string (nth-value 2 (function-lambda-expression func))))
	 (name (subseq str 0 (position #\- str))))
    (if (= (length name) 1)
	(string-upcase name)
	(string-downcase name))))

(defun read-opp-move ()
  (let ((v1 (parse-integer (read-line)))
	(v2 (read-line))
	(v3 (read-line)))
    (ecase v1
      (1 (list :left (name->func v2) (parse-integer v3)))
      (2 (list :right(name->func v3) (parse-integer v2))))))

(defun write-my-move (move)
  (let ((v1 (first move))
	(v2 (second move))
	(v3 (third move)))
    (ecase v1
      (:left  (format t "~A~%~A~%~A~%" 1 (func->name v2) v3))
      (:right (format t "~A~%~A~%~A~%" 2 v3 (func->name v2))))
    (values)))

(defun auto-apply ()
  (let ((*auto-apply-flag* t))
    (dotimes (i 256)
      (setf *call-count* 0)
      (when (= (my-vitality i) -1)
	(card-call (my-field i) #'i-card)
	(setf (my-field i) #'i-card)
	(setf (my-vitality i) 0)))))

(defun auto-my-apply ()
  (auto-apply))

(defun auto-opp-apply ()
  (let ((*player1* *player2*)
	(*player2* *player1*))
    (auto-apply)))

(defun game-loop (player-id)
  (let ((prev-opp-move
	 (ecase player-id
	   (0 nil)
	   (1 (read-opp-move)))))
    (loop
      (when (or (all-slots-dead-p *player1*)
		(all-slots-dead-p *player2*))
	(return))

      (auto-my-apply)
      (write-my-move (make-move prev-opp-move))
      (auto-opp-apply)
      (apply #'imitate-opp-move (setf prev-opp-move (read-opp-move))))

    (let ((my-alive (slots-alive-num *player1*))
	  (opp-alive (slots-alive-num *player2*)))
      (cond ((> my-alive opp-alive) :win)
	    ((< my-alive opp-alive) :lose)
	    ((= my-alive opp-alive) :tie)))))

(defun run (argv)
  (game-loop (parse-integer (second argv))))
