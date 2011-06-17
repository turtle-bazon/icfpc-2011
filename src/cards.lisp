(in-package :ltg)

(defparameter *start-vitality* 10000)

(defstruct slot
  (field #'identity)
  (vitality *start-vitality*))
  

(defparameter *n-slots* 256)
(defparameter *proponent-slots* (make-array *n-slots*))
(defparameter *opponent-slots* (make-array *n-slots*))
(defparameter *max-field* 65535)

(defun i-card (x)
  x)

(defun zero-card (x)
  (declare (ignore x))
  0)

(defun succ-card (n)
  (assert (typep n 'integer))
  (if (= n *max-field*)
      n
      (1+ n)))

(defun dbl-card (n)
  (min *max-field* (* 2 n)))

(defun alive-p (slot)
  (plusp (slot-vitality slot)))

(defun get-card (i)
  (let ((slot (aref *proponent-slots* i)))
    (assert (alive-p slot))
    (slot-field slot)))

(defun put-card (x)
  (declare (ignore x))
  #'identity)

(defun s-card (f)
  (lambda (g)
    (lambda (x)
      (let ((h (funcall f x))
	    (y (funcall g x)))
	(funcall h y)))))

(defun k-card (x)
  (lambda (y)
    (declare (ignore y))
    x))

(defun inc-card (i)
  (let ((v (slot-vitality (aref *proponent-slots* i))))
    (when (and (plusp v) (< v *max-field*))
      (incf (slot-vitality (aref *proponent-slots* i))))))

(defun dec-card (i)
  (when (plusp (aref *opponent-slots* (- 255 i)))
    (decf (aref *opponent-slots* (- 255 i)))))

(defun decf-vitality (slot delta &optional set-to-0-if-neg)
  (let ((new-vitality (- (slot-vitality slot) delta)))
    (when (and (not set-to-0-if-neg) (minusp new-vitality))
      (error "Negative vitality after decrease."))
    (setf (slot-vitality slot) (if set-to-0-if-neg
				   (max 0 new-vitality)
				   new-vitality))))

(defun attack-card (i)
  (lambda (j)
    (lambda (n)
      (decf-vitality (aref *proponent-slots* i) n)
      (when (alive-p (aref *opponent-slots* (- 255 j)))
	(decf-vitality (aref *opponent-slots* (- 255 j))
		       (floor (* n 9/10)) t))
      #'identity)))
