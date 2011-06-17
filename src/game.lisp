(in-package :icfpc)

(defstruct slot
  (field #'identity)
  (vitality 10000))

(deftype slot-no ()
  '(integer 0 255))

(deftype field ()
  '(or integer function))

(deftype vitality ()
  '(integer -1 65535))

(defstruct player
  (slots (let ((arr (make-array 256))) (map-into arr #'(lambda (x) (declare (ignore x)) (make-slot)) arr))))
;  (cards *all-cards*))

(defparameter *player1* (make-player))
(defparameter *player2* (make-player))
(defparameter *max-field* 65535)

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

;; Внутриигровой кондишен.
(define-condition card-condition () ())
;; Закончились ходы.
(define-condition rounds-limit () ())

(defparameter *call-count* 0)

(defun card-call (card-fun arg)
  (incf *call-count*)
  (when (<= 1000 *call-count*)
    (signal 'rounds-limit))
  (unless (typep card-fun 'function)
    (signal 'card-condition))
  (funcall card-fun arg))

(defun left-apply (card slot-no)
  (card-call card (my-field slot-no)))

(defun right-apply (card slot-no)
  (card-call (my-field slot-no) card))

(defun imitate-move (side card slot-no)
  (if (and (functionp card)
	   (nth-value 1 (my-vitality slot-no)))
      (ecase side
	(:left  (left-apply card slot-no))
	(:right (right-apply card slot-no)))
      (signal 'card-condition)))
