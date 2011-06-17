;;;; -*- mode: lisp -*-

(in-package :icfpc)

(lift:deftestsuite test-cards () ())

(lift:addtest
    test-identity-card
  (lift:ensure-same (i-card 1)
		    1))

;; 
;;; run-tests
;;;
(defun run-all-tests ()
  (loop for test-suite in '(test-cards)
     do (lift:describe-test-result (lift:run-tests :suite test-suite
						   :report-pathname nil) t)))
