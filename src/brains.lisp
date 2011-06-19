(in-package :icfpc)

(defun make-move (opp-move)
  (funcall #'make-move-1 opp-move))

(defun dumb-move ()
  (list :left #'i-card 0))

#|(defun dumb-make-move (opp-move)
  (declare (ignore opp-move))
  (dumb-move))|#

(defparameter *current-attack-queue* nil)


(defun imitate-restart ()
  (setf *player1* (make-player)
	*player2* (make-player)
	*current-attack-queue* nil)
  t)

(defun attack-optimal-source ()
  (position-if #'(lambda (s) (plusp (slot-vitality s))) (player-slots *player1*)))

(defun make-move-1 (opp-move)
  (declare (ignore opp-move))
  (unless *current-attack-queue*
    (let* ((opp (opp-least-alive))
	   (my  (attack-optimal-source))
	   (stor 1))
      (setf *current-attack-queue*
	    (append (attack-queue stor my (- 255 opp)
				  (* 0.5 (min (my-vitality my)
					      (opp-vitality opp))))
		    (list my opp)))))

  (let ((move (car *current-attack-queue*)))
    (setf *current-attack-queue*
	  (unless (eq (apply #'imitate-my-move move) :error)
	    (cdr *current-attack-queue*)))
    (when (integerp (car *current-attack-queue*))
      (let ((my (first *current-attack-queue*))
	    (opp (second *current-attack-queue*)))
	(format *error-output* "My: vit(~A)=~A; Opp: vit(~A)=~A~%"
		my (my-vitality my) opp (opp-vitality opp)))
      (setf *current-attack-queue* nil))
    move))
