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
  (+ (position-if #'(lambda (s) (> (slot-vitality s) 2000)) (subseq (player-slots *player1*) 2)) 2))

(defun make-move-1 (opp-move)
  (declare (ignore opp-move))
  (unless *current-attack-queue*
    (let* ((opp (opp-least-alive))
	   (my  (attack-optimal-source))
	   (stor 1))
      (setf *current-attack-queue*
	    (attack-queue stor my (- 255 opp) 2000))))
  (let ((move (car *current-attack-queue*)))
    (setf *current-attack-queue*
	  (unless (eq (apply #'imitate-my-move move) :error)
	    (cdr *current-attack-queue*)))
    move))
