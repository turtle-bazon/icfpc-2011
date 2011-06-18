(in-package :icfpc)

(defun make-attack-queue-0-0-1 (my-fn)
  "Attacks 0-th slot using our 0-th with attack value 1 (attack 0 0 1)"
  (let ((my (delay (funcall my-fn))))
    `((:left  #'attack-card ,my)
      (:right #'zero-card   ,my)
      (:right #'zero-card   ,my)
      (:left  #'K-card      ,my)
      (:left  #'S-card      ,my)
      (:right #'succ-card   ,my)
      (:right #'zero-card   ,my))))
