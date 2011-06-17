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

;; 
;;; run-tests
;;;
(defun run-all-tests ()
  (loop for test-suite in '(test-cards)
     do (lift:describe-test-result (lift:run-tests :suite test-suite
						   :report-pathname nil) t)))
