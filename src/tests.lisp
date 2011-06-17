;;;; -*- mode: lisp -*-

(in-package :icfpc)

(lift:deftestsuite test-cards () ())

(lift:addtest
    test-identity-card
  (let ((value (random 65535)))
    (lift:ensure-same (i-card value)
		      value)))

(lift:addtest
    test-zero-card
  (let ((value (random 65535)))
    (lift:ensure-same (zero-card value)
		      0)))

(lift:addtest
    test-succ-card-1
  (lift:ensure-same (succ-card 0)
		    1))

(lift:addtest
    test-succ-card-2
  (lift:ensure-same (succ-card 65535)
		    65535))

(lift:addtest
    test-dbl-card-1
  (lift:ensure-same (dbl-card 0)
		    0))

(lift:addtest
    test-dbl-card-2
  (loop for n from 0 to 32767
     do (lift:ensure-same (dbl-card n)
			  (* n 2))))

(lift:addtest
    test-dbl-card-3
  (lift:ensure-same (dbl-card 32768)
		    65535))

(lift:addtest
    test-dbl-card-4
  (lift:ensure-error (dbl-card -1)))

(lift:addtest
    test-get-card-1
  (setf (my-field 0) 1)
  (setf (my-vitality 0) 10000)
  (lift:ensure-same (get-card 0)
		    1))

(lift:addtest
    test-get-card-2
  (setf (my-field 0) #'i-card)
  (setf (my-vitality 0) 10000)
  (lift:ensure-same (get-card 0)
		    #'i-card))

(lift:addtest
    test-get-card-3
  (setf (my-field 0) 1)
  (setf (my-vitality 0) 0)
  (lift:ensure-error (get-card 0)))

(lift:addtest
    test-get-card-4
  (setf (my-field 0) 1)
  (setf (my-vitality 0) -1)
  (lift:ensure-error (get-card 0)))

(lift:addtest
    test-get-card-5
  (lift:ensure-error (get-card -1))
  (lift:ensure-error (get-card 256)))

(lift:addtest
    test-put-card
  (loop for i from 0 to 10
       do (lift:ensure-same (put-card (random 65535))
			    #'i-card)))

(lift:addtest
    test-s-card-1
  (let* ((g-fun (s-card (random 65535)))
	 (x-fun (funcall g-fun #'inc-card)))
    (lift:ensure-error (funcall x-fun (random 65535)))))

(lift:addtest
    test-s-card-2
  (let* ((g-fun (s-card #'inc-card))
	 (x-fun (funcall g-fun (random 65535))))
    (lift:ensure-error (funcall x-fun (random 65535)))))
;; 
;;; run-tests
;;;
(defun run-all-tests ()
  (loop for test-suite in '(test-cards)
     do (lift:describe-test-result (lift:run-tests :suite test-suite
						   :report-pathname nil) t)))
