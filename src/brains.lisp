(in-package :icfpc)

(defun make-move (opp-move)
  (funcall #'make-move-1 opp-move))

(defun dumb-move ()
  (list :left #'i-card 0))

#|(defun dumb-make-move (opp-move)
  (declare (ignore opp-move))
  (dumb-move))|#


(defparameter *current-attack-queue* nil)
(defparameter *current-kill-queue* nil)
(defparameter *init-power* 1000)
(defparameter *current-kill-state* :init)

(defun imitate-restart ()
  (setf *player1* (make-player)
	*player2* (make-player)
	*current-attack-queue* nil
	*current-kill-queue* nil
	*init-power* 1000
	*current-kill-state* :init)
  t)

(defun attack-optimal-source (min-power)
  (+ (position-if #'(lambda (s) (> (slot-vitality s) min-power)) (subseq (player-slots *player1*) 4)) 4))

(defun perform-attack ()
  (unless *current-attack-queue*
    (setf *current-attack-queue*
	  (attack-queue-1st-2nd-slot 3 (attack-optimal-source *init-power*)))
    (print `(:perform (:command (attack-queue-1st-2nd-slot 3 ,(attack-optimal-source *init-power*)))
		      (:my-1 ,(my-field 1)) (:my-2 ,(my-field 2)))
	   *error-output*))
  (let ((move (car *current-attack-queue*)))
    (setf *current-attack-queue*
	  (let ((res (apply #'imitate-my-move move)))
	    (if (eq res :error)
		(progn (print `(:error ,move) *error-output*) nil)
		(cdr *current-attack-queue*))))
    (values move (null *current-attack-queue*))))

(defun prepare-kill-slot ()
  (unless *current-kill-queue*
    (setf *current-kill-queue*
	  (append (write-number 1 *init-power*)
		  (write-number 2 (opp-least-alive)))))
  (let ((move (car *current-kill-queue*)))
    (setf *current-kill-queue*
	  (let ((res (apply #'imitate-my-move move)))
	    (if (eq res :error)
		(progn (print `(:error ,move) *error-output*) nil)
		(cdr *current-kill-queue*))))
    (values move (null *current-kill-queue*))))

(defun next-attack-params ()
  (unless *current-kill-queue*
    (setf *current-kill-queue*
	  (append (loop repeat (- (opp-least-alive) (my-field 2))
			collect `(:left ,#'succ-card 2)))))
  (if *current-kill-queue*
      (let ((move (car *current-kill-queue*)))
	(setf *current-kill-queue*
	      (unless (eq (apply #'imitate-my-move move) :error)
		(cdr *current-kill-queue*)))
	(values move (null *current-kill-queue*)))
      (progn (setf *current-kill-state* :attack)
	     (make-move-1 nil))))

(defun make-move-1 (opp-move) (declare (ignore opp-move))
  (ecase *current-kill-state*
    (:init
       (multiple-value-bind (move finish)
	   (prepare-kill-slot)
	 (when finish (setf *current-kill-state* :attack))
	 move))
    (:attack (print :attack *error-output*)
       (multiple-value-bind (move finish)
	   (perform-attack)
	 (when finish
	   (setf *current-kill-state* :next))
	 move))
    (:next (print :next *error-output*)
       (multiple-value-bind (move finish)
	   (next-attack-params)
	 (when finish
	   (setf *current-kill-state* :attack))
	 move))))
