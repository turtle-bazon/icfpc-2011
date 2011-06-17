(in-package :icfpc-2011)

(deftype slot-no ()
  '(integer 0 255))

(deftype vitality ()
  '(integer -1 65535))

;; Внутриигровой кондишен.
(define-condition card-condition () ())
;; Закончились ходы.
(define-condition rounds-limit () ())

(defvar *call-count*)

(defun card-call (card-fun arg)
  (incf *call-count*)
  (when (<= 1000 *call-count*)
    (signal 'rounds-limit))
  (unless (typep card-fun 'function)
    (signal 'card-condition))
  (funcall card-fun arg))

(defun i-card (x)
  x)

(defmethod apply-card (x)
  (declare (ignore x))
  0)

(defun succ-card (n)
  (unless (typep n 'vitality)
    (signal 'card-condition))
  (if (= n *max-field*) n (1+ n)))

(defun dbl-card (n)
  (unless (typep n 'vitality)
    (signal 'card-condition))
  (min *max-field* (* 2 n)))

(defun get-card (i)
  (unless (and (typep i 'slot-no)
	       (nth-value 1 (get-vitality i)))
    (signal 'card-condtion))
  (my-field i))

(defun put-card (x)
  (declare (ignore x))
  #'identity)

(defun s-card (f)
  (lambda (g)
    (lambda (x)
      (let ((h (card-call f x))
	    (y (card-call g x)))
	(card-call h y)))))

(defun k-card (x)
  (lambda (y)
    (declare (ignore y))
    x))

(defun inc-card (i)
  (unless (typep i 'slot-no)
    (signal 'card-condtion))
  (let ((v (my-vitality i)))
    (when (and (plusp v) (< v *max-field*))
      (incf (my-vitality i)))))

(defun dec-card (i)
  (unless (typep i 'slot-no)
    (signal 'card-condtion))
  (let ((v (opp-vitality (- 255 i))))
    (when (plusp v)
      (decf (opp-vitality (- 255 i))))))

#+nil
(defun attack-card (i)
  (lambda (j)
    (lambda (n)
      (unless (and (typep i 'legal-slot-no)
		   (typep j 'legal-slot-no)
		   (or (not (integerp n))
		       (> n (my-vitality i))))
	(signal card-condtion))
      (decf (my-vitality slot-no) n)
      (when (nth-value 1 (opp-vitality (- 255 j)))
	(when (minusp (decf (opp-vitality (- 255 j)) (floor (* n 9/10))))
	  (setf (opp-vitality (- 255 j)) 0)))
      #'identity)))
