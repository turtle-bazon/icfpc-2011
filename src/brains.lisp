(in-package :icfpc)

(defun make-move (opp-move)
  (funcall #'dumb-make-move opp-move))

(defun dumb-make-move (opp-move)
  (declare (ignore opp-move))
  (list :left #'i-card 0))
