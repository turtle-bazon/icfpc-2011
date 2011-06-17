(in-package :icfpc-2011)

(defgeneric apply-card (card slot-no))

(defmethod apply-card ((card :I) slot-no)
  (my-field slot-no))

(defmethod apply-card ((card :zero) slot-no)
  0)

(defmethod apply-card ((card :succ) slot-no)
  (with-my-integer-field (n slot-no)
    (if (= n *max-field*) n (1+ n))))

(defmethod apply-card ((card :dbl) slot-no)
  (with-my-integer-field (n slot-no)
    (min *max-field* (* 2 n))))

(defmethod apply-card ((card :get) slot-no)
  (if (and (correct-slot-p slot-no)
	   (nth-value 1 (get-vitality slot-no)))
      (my-field slot-no)
      :error)


(defmethod apply-card ((card :put) slot-no)
  #'identity)

;;; !!!!!!!!
(defun s-comb (f)
  (lambda (g)
    (lambda (x)
      (let ((h (if (not (functionp g)) :error(funcall f x)))
	    (y (if (not (functionp g)) :error (funcall g x))))
	(let ((val (funcall h y)))
	  (if (not (functionp val)) :error val))))))

(defun k-comb (x)
  (lambda (y)
    (declare (ignore y))
    x))

(defmethod apply-card ((card :inc) slot-no)
  (if (not (correct-slot-p slot-no))
      :error
      (let ((v (my-vitality slot-no)))
	(when (and (plusp v) (< v *max-field*))
	  (incf (my-vitality slot-no))))))

(defmethod apply-card ((card :dec) slot-no)
  (if (not (correct-slot-p slot-no))
      :error
      (let ((v (opp-vitality (- 255 slot-no))))
	(when (plusp v)
	  (decf (opp-vitality (- 255 slot-no)))))))

(defmethod apply-card ((card :dec) slot-no)
  (if (not (correct-slot-p slot-no))
      :error
      (lambda (j)
	(if (not (correct-slot-p j))
	    :error
	    (lambda (n)
	      (if (or (not (integerp n))
		      (> n (my-vitality slot-no)))
		  :error
		  (progn (decf (my-vitality slot-no) n)
			 (when (nth-value 1 (opp-vitality (- 255 j)))
			   (when (minusp (decf (opp-vitality (- 255 j)) (floor (* n 9/10))))
			     (setf (opp-vitality (- 255 j)) 0)))
			 #'identity)))))))
