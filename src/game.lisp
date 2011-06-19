(in-package :icfpc)

(defstruct slot
  (field #'i-card)
  (vitality 10000))

(deftype slot-no ()
  '(integer 0 255))

(deftype field ()
  '(or (integer 0 65535) function))

(deftype vitality ()
  '(integer -1 65535))

(defstruct player
  (slots (let ((arr (make-array 256))) (map-into arr #'(lambda (x) (declare (ignore x)) (make-slot)) arr))))
;  (cards *all-cards*))

(defparameter *player1* (make-player))
(defparameter *player2* (make-player))
(defparameter *auto-count* 0)
(defparameter *move-count* 0)
(defparameter *game-count* 0)
(defparameter *auto-apply-flag* nil)

(defun %get-field (slot-no player)
  (let ((val (slot-field (svref (player-slots player) slot-no))))
    (values val
	    (typecase val
	      (function t)
	      (integer nil)
	      (t (error "incorrect field"))))))

(defun %set-field (slot-no player new)
  (if (typep new 'field)
      (setf (slot-field (svref (player-slots player) slot-no)) new)
      (error "setting incorrect field")))

(defun my-field  (slot-no) (%get-field slot-no *player1*))
(defun opp-field (slot-no) (%get-field slot-no *player2*))
(defun (setf my-field) (new slot-no) (%set-field slot-no *player1* new))
(defun (setf opp-field) (new slot-no) (%set-field slot-no *player2* new))

(defun %get-vitality (slot-no player)
  (let ((val (slot-vitality (svref (player-slots player) slot-no))))
    (assert (typep val 'vitality) nil "incorrect vitality")
    (values val (plusp val))))

(defun %set-vitality (slot-no player new)
  (if (typep new 'vitality)
      (setf (slot-vitality (svref (player-slots player) slot-no)) new)
      (error "incorrect vitality")))

(defun my-vitality  (slot-no) (%get-vitality slot-no *player1*))
(defun opp-vitality (slot-no) (%get-vitality slot-no *player2*))
(defun (setf my-vitality) (new slot-no) (%set-vitality slot-no *player1* new))
(defun (setf opp-vitality) (new slot-no) (%set-vitality slot-no *player2* new))

(defun alive-p (slot-no) (nth-value 1 (my-vitality slot-no)))

(defun zero-card (x) (declare (ignore x))
  (error "zero-card was called somehow %)"))

(defun card-function-p (card)
  (and (not (eq card #'zero-card))
       (functionp card)))

(defun card-call (card-fun arg)
  (unless (eq card-fun #'zero-card)
    (when *auto-apply-flag* (incf *auto-count*))
    (incf *call-count*))
  (unless (or (and *auto-apply-flag* (> 1000 *auto-count*))
	      (> 1000 *call-count*))
    (normal-error))
  (unless (functionp card-fun) (normal-error))
  (if (eq card-fun #'zero-card)
      0
      (funcall card-fun (if (eq arg #'zero-card) 0 arg))))

(defun left-apply (card slot-no)
  (unless (card-function-p card) (normal-error))
  (card-call card (my-field slot-no)))

(defun right-apply (card slot-no)
  (card-call (my-field slot-no) card))

(define-condition normal-error () ())

(defun normal-error ()
  (signal 'normal-error))

(defun imitate-my-move (side card slot-no)
  (let ((result
	 (with-simple-restart (normal-error-restart "")
	   (handler-bind ((normal-error (lambda (c) (declare (ignore c)) (invoke-restart 'normal-error-restart))))
	     (setf *call-count* 0)
	     (if (and (functionp card)
		      (alive-p slot-no))
		 (setf (my-field slot-no)
		       (ecase side
			 (:left  (left-apply card slot-no))
			 (:right (right-apply card slot-no))))
		 (normal-error))))))
    (or result
	(progn (setf (my-field slot-no) #'i-card)
	       :error))))

(defun imitate-opp-move (side card slot-no)
  (let ((*player1* *player2*)
	(*player2* *player1*))
    (imitate-my-move side card slot-no)))

(defun slots-alive-num (player)
  (count-if #'(lambda (s) (plusp (slot-vitality s))) (player-slots player)))

(defun all-slots-dead-p (player)
  (zerop (slots-alive-num player)))
