(in-package :icfpc)

(defun make-move (opp-move)
  (funcall #'make-move-1 opp-move))

(defun dumb-move ()
  (list :left #'i-card 0))

#|(defun dumb-make-move (opp-move)
  (declare (ignore opp-move))
  (dumb-move))|#

(defparameter *storage-slots* '(0 1 2 3))
(defparameter *current-attack-queue* nil)
(defparameter *current-kill-queue* nil)
(defparameter *prev-kill-state* nil)
(defparameter *current-protect-queue* nil)
(defparameter *init-power* 4096)
(defparameter *current-kill-state* :init)

(defun imitate-restart ()
  (setf *player1* (make-player)
	*player2* (make-player)
	*current-attack-queue* nil
	*current-kill-queue* nil
	*prev-kill-state* nil
	*current-protect-queue* nil
	*init-power* 4096
	*current-kill-state* :init)
  t)

(defun attack-optimal-source (min-power)
  (let ((pos (position-if #'(lambda (s) (> (slot-vitality s) min-power)) (subseq (player-slots *player1*) 4))))
    (if (null pos) nil (+ pos 4))))

(defun attack-optimal-delta ()
  (if (nth-value 1 (opp-vitality (- 255 (my-field 2))))
      0 1))

(defun perform-attack ()
  (unless *current-attack-queue*
    (setf *current-attack-queue*
	  (let ((s (attack-optimal-source *init-power*)))
	    (if s (attack-queue-1st-2nd-slot 3 s)
		(let ((s1 (position-if #'(lambda (s) (and (plusp (slot-vitality s))
							  (< (slot-vitality s) *init-power*)))
				       (subseq (player-slots *player1*) 4))))
		  (multiple-heal 11 s1))))))
  (let ((move (car *current-attack-queue*)))
    (setf *current-attack-queue*
	  (let ((res (apply #'imitate-my-move move)))
	    (if (eq res :error)
		(progn ;(print `(:error ,move) *error-output*)
		       nil)
		(cdr *current-attack-queue*))))
    (values move (null *current-attack-queue*))))

(defun prepare-kill-slot ()
  (unless *current-kill-queue*
    (setf *current-kill-queue*
	  (append (write-number 1 *init-power*)
		  (write-number 2 0))))
  (let ((move (car *current-kill-queue*)))
    (setf *current-kill-queue*
	  (let ((res (apply #'imitate-my-move move)))
	    (if (eq res :error)
		(progn ;(print `(:error ,move) *error-output*)
		       nil)
		(cdr *current-kill-queue*))))
    (values move (null *current-kill-queue*))))

(defun next-attack-params ()
  (unless *current-kill-queue*
    (setf *current-kill-queue*
	  (append (loop repeat (attack-optimal-delta)
			collect `(:left ,#'succ-card 2)))))
  (if *current-kill-queue*
      (let ((move (car *current-kill-queue*)))
	(setf *current-kill-queue*
	      (unless (eq (apply #'imitate-my-move move) :error)
		(cdr *current-kill-queue*)))
	(values move (null *current-kill-queue*)))
      (progn (setf *current-kill-state* :attack)
	     (make-move-1 nil))))

(defun perform-protect (slot)
  (unless *current-protect-queue*
    (setf *current-protect-queue*
	  (append (protect (if (alive-p 3) 3
			       (first (remove-if-not #'alive-p (loop for i from 0 to 255 collect i))))
			   slot)
		  `((:left ,#'put-card ,slot)))))
  (let ((move (car *current-protect-queue*)))
    (setf *current-protect-queue*
	  (unless (eq (apply #'imitate-my-move move) :error)
	    (cdr *current-protect-queue*)))
    (values move (null *current-protect-queue*))))

(defun make-move-1 (opp-move) (declare (ignore opp-move))
  (let ((protect-aims (remove nil (mapcar #'(lambda (x) (if (alive-p x) nil x)) (loop for i from 4 to 255 collect i)))))
    (when (and protect-aims (not *current-protect-queue*))
      (setf *prev-kill-state* *current-kill-state*
	    *current-kill-state* :protect))
    (ecase *current-kill-state*
      (:init
	 (multiple-value-bind (move finish)
	     (prepare-kill-slot)
	   (when finish (setf *current-kill-state* :attack))
	   move))
      (:protect
	 (multiple-value-bind (move finish)
	     (perform-protect (car protect-aims))
	   (when finish
	     (setf *current-kill-state* *prev-kill-state*))
	   move))
      (:attack
	 (multiple-value-bind (move finish)
	     (perform-attack)
	   (when finish
	     (setf *current-kill-state* :next))
	   move))
      (:next
	 (multiple-value-bind (move finish)
	     (next-attack-params)
	   (when finish
	     (setf *current-kill-state* :attack))
	   move)))))
