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
  (let ((slot (random 255)))
    (setf (my-field slot) 1)
    (setf (my-vitality slot) 10000)
    (lift:ensure-same (get-card slot)
		      1)))

(lift:addtest
    test-get-card-2
  (let ((slot (random 255)))
    (setf (my-field slot) #'i-card)
    (setf (my-vitality slot) 10000)
    (lift:ensure-same (get-card slot)
		      #'i-card)))

(lift:addtest
    test-get-card-3
  (let ((slot (random 255)))
    (setf (my-field slot) 1)
    (setf (my-vitality slot) 0)
    (lift:ensure-error (get-card slot))))

(lift:addtest
    test-get-card-4
  (let ((slot (random 255)))
    (setf (my-field slot) 1)
    (setf (my-vitality slot) -1)
    (lift:ensure-error (get-card slot))))

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
  (let* ((g-fun (s-card #'i-card))
	 (x-fun (funcall g-fun (random 65535))))
    (lift:ensure-error (funcall x-fun (random 65535)))))

(lift:addtest
    test-s-card-3
  (let* ((g-fun (s-card #'i-card))
	 (x-fun (funcall g-fun #'i-card)))
    (lift:ensure-error (funcall x-fun (random 65535)))
    (lift:ensure-no-warning (funcall x-fun #'i-card))))

(lift:addtest
    test-k-card
  (loop for i from 1 to 10
     for n = (random 65535)
     do (let ((y-fun (k-card n)))
	  (lift:ensure-same (funcall y-fun (random 65535))
			    n)
	  (lift:ensure-same (funcall y-fun #'i-card)
			    n))))

(lift:addtest
    test-inc-card-1
  (loop for v from 1 to 65534
     for slot = (random 255)
     do (progn
	  (setf (my-vitality slot) v)
	  (lift:ensure-same (inc-card slot) #'i-card)
	  (lift:ensure-same (my-vitality slot)
			    (+ v 1)))))

(lift:addtest
    test-inc-card-2
  (let ((slot (random 255)))
    (setf (my-vitality slot) 0)
    (lift:ensure-same (inc-card slot) #'i-card)
    (lift:ensure-same (my-vitality slot)
		      0)
    (setf (my-vitality slot) 65535)
    (lift:ensure-same (inc-card slot) #'i-card)
    (lift:ensure-same (my-vitality slot)
		      65535)))

(lift:addtest
    test-inc-card-3
  (lift:ensure-error (inc-card -1))
  (lift:ensure-error (inc-card 256)))

(lift:addtest
    test-dec-card-1
  (loop for v from 1 to 65535
     for slot = (random 255)
     do (progn
	  (setf (opp-vitality slot) v)
	  (lift:ensure-same (dec-card slot) #'i-card)
	  (lift:ensure-same (opp-vitality slot)
			    (- v 1)))))

(lift:addtest
    test-dec-card-2
  (let ((slot (random 255)))
    (setf (opp-vitality slot) 0)
    (lift:ensure-same (dec-card slot) #'i-card)
    (lift:ensure-same (opp-vitality slot)
		      0)
    (setf (my-vitality slot) -1)
    (lift:ensure-same (dec-card slot) #'i-card)
    (lift:ensure-same (opp-vitality slot)
		      -1)))

(lift:addtest
    test-dec-card-3
  (lift:ensure-error (dec-card -1))
  (lift:ensure-error (dec-card 256)))

;; 
;;; run-tests
;;;
(defun run-all-tests ()
  (loop for test-suite in '(test-cards)
     do (lift:describe-test-result (lift:run-tests :suite test-suite
						   :report-pathname nil) t)))
