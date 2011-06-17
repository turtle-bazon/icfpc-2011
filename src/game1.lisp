(in-package :icfpc-2011)

(defstruct player
  (slots (make-array 256))
  (cards *all-cards*))

(defstruct slot
  (field #'identity)
  (vitality 10000))

(defparameter *player1* (make-player))
(defparameter *player2* (make-player))
(defparameter *max-field* 65535)

;; slots

(defun correct-slot-p (slot-no)
  (and (plusp slot-no)
       (< slot-no 256)))

;; fields

(defun %get-field (slot-no player)
  (let ((val (slot-field (svref player slot-no))))
    (values val
	    (typecase val
	      (function t)
	      (integer nil)
	      (error "incorrect field")))))

(defun %set-field (slot-no player new)
  (if (typep new `(or function (integer 0 ,*max-field*)))
      (setf (slot-field (svref player slot-no)) new)
      (error "setting incorrect field")))

(defun my-field  (slot-no) (%get-field slot-no *player1*))
(defun opp-field (slot-no) (%get-field slot-no *player2*))
(defun (setf my-field) (new slot-no) (%set-field slot-no *player1* new))
(defun (setf opp-field) (new slot-no) (%set-field slot-no *player2* new))


(defmacro %with-integer-field ((field slot-no player) body)
  `(multiple-value-bind (n function-p)
       (%get-field ,slot-no ,player)
     (if function-p
	 :error
	 (let ((,field n))
	   ,@body))))

(defmacro %with-function-field ((field slot-no player) body)
  `(multiple-value-bind (n function-p)
       (%get-field ,slot-no ,player)
     (if function-p
	 (let ((,field n))
	   ,@body)
	 :error)))

(defmacro with-my-integer-field ((field slot-no) &body body)
  (%with-integer-field field slot-no *player1* body))

(defmacro with-opp-integer-field ((field slot-no) &body body)
  (%with-integer-field field slot-no *player2* body))

(defmacro with-my-function-field ((field slot-no) &body body)
  (%with-function-field field slot-no *player1* body))

(defmacro with-opp-function-field ((field slot-no) &body body)
  (%with-function-field field slot-no *player2* body))

;; vitality

(defun %get-vitality (slot-no player)
  (let ((val (slot-vitality (svref player slot-no))))
    (values val
	    (cond ((or (< val -1) (> val *max-field*)) (error "incorrect vitality"))
		  ((plusp val) t)
		  (t nil)))))

(defun %set-vitality (slot-no player new)
  (if (typep vitality '(integer -1))
      (setf (slot-vitality (svref player slot-no)) new)
      (error "incorrect vitality")))

(defun my-vitality  (slot-no) (%get-vitality slot-no *player1*))
(defun opp-vitality (slot-no) (%get-vitality slot-no *player2*))
(defun (setf my-vitality) (new slot-no) (%set-vitality slot-no *player1* new))
(defun (setf opp-vitality) (new slot-no) (%set-vitality slot-no *player2* new))

(defun left-apply (card slot-no)
  ;; insert error check
  (apply-card card slot-no))

(defun right-apply (card slot-no)
  ;; insert error check
  (funcall (my-field slot-no) card))

(defun imitate-move (side card slot-no)
  (ecase side
    (:left  (left-apply card slot-no)
    (:right (right-apply card slot-no)))))
