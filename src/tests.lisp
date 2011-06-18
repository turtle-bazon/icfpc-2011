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
  (lift:ensure-condition normal-error (dbl-card #'i-card)))

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
    (lift:ensure-condition normal-error (get-card slot))))

(lift:addtest
    test-get-card-4
  (let ((slot (random 255)))
    (setf (my-field slot) 1)
    (setf (my-vitality slot) -1)
    (lift:ensure-condition normal-error (get-card slot))))

(lift:addtest
    test-get-card-5
  (lift:ensure-condition normal-error (get-card -1))
  (lift:ensure-condition normal-error (get-card 256)))

(lift:addtest
    test-put-card
  (loop for i from 0 to 10
       do (lift:ensure-same (put-card (random 65535))
			    #'i-card)))

(lift:addtest
    test-s-card-1
  (let* ((g-fun (s-card (random 65535)))
	 (x-fun (funcall g-fun #'inc-card)))
    (lift:ensure-condition normal-error (funcall x-fun (random 65535)))))

(lift:addtest
    test-s-card-2
  (let* ((g-fun (s-card #'i-card))
	 (x-fun (funcall g-fun (random 65535))))
    (lift:ensure-condition normal-error (funcall x-fun (random 65535)))))

(lift:addtest
    test-s-card-3
  (let* ((g-fun (s-card #'i-card))
	 (x-fun (funcall g-fun #'i-card)))
    (lift:ensure-condition normal-error (funcall x-fun (random 65535)))
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
  (lift:ensure-condition normal-error (inc-card -1))
  (lift:ensure-condition normal-error (inc-card 256)))

(lift:addtest
    test-dec-card-1
  (loop for v from 1 to 65535
     for i = (random 255)
     do (progn
	  (setf (opp-vitality (- 255 i)) v)
	  (lift:ensure-same (dec-card i)
			    #'i-card)
	  (lift:ensure-same (opp-vitality (- 255 i))
			    (- v 1)))))

(lift:addtest
    test-dec-card-2
  (let ((i (random 255)))
    (setf (opp-vitality (- 255 i)) 0)
    (lift:ensure-same (dec-card i)
		      #'i-card)
    (lift:ensure-same (opp-vitality (- 255 i))
		      0)
    (setf (opp-vitality (- 255 i)) -1)
    (lift:ensure-same (dec-card i)
		      #'i-card)
    (lift:ensure-same (opp-vitality (- 255 i))
		      -1)))

(lift:addtest
    test-dec-card-3
  (lift:ensure-condition normal-error (dec-card -1))
  (lift:ensure-condition normal-error (dec-card 256)))

(lift:addtest
    test-attack-card-1
  (let ((i (random 255))
	(j (random 255)))
    (let* ((j-fun (attack-card i))
	   (n-fun (funcall j-fun j)))
      (setf (my-vitality i) 89)
      (setf (opp-vitality (- 255 j)) 99)
      (lift:ensure-same (funcall n-fun 10) #'i-card)
      (lift:ensure-same (my-vitality i)
			79)
      (lift:ensure-same (opp-vitality (- 255 j))
			90))))

(lift:addtest
    test-attack-card-2
  (let* ((i -1)
	 (j (random 255))
	 (j-fun (attack-card i))
	 (n-fun (funcall j-fun j)))
    (lift:ensure-condition normal-error (funcall n-fun (random 255)))))

(lift:addtest
    test-attack-card-3
  (let* ((i 256)
	 (j (random 255))
	 (j-fun (attack-card i))
	 (n-fun (funcall j-fun j)))
    (lift:ensure-condition normal-error (funcall n-fun (random 255)))))

(lift:addtest
    test-attack-card-4
  (let* ((i (random 255))
	 (j -1)
	 (j-fun (attack-card i))
	 (n-fun (funcall j-fun j)))
    (lift:ensure-condition normal-error (funcall n-fun (random 255)))))

(lift:addtest
    test-attack-card-5
  (let* ((i (random 255))
	 (j 256)
	 (j-fun (attack-card i))
	 (n-fun (funcall j-fun j)))
    (lift:ensure-condition normal-error (funcall n-fun (random 255)))))

(lift:addtest
    test-attack-card-6
  (loop for i from 0 to 255
     do (loop for j from 0 to 255
	   do (let* ((j-fun (attack-card i))
		     (n-fun (funcall j-fun j)))
		(lift:ensure-condition normal-error (funcall n-fun #'i-card))))))

(lift:addtest
    test-attack-card-7
  (let ((i (random 255))
	(j (random 255)))
    (let* ((j-fun (attack-card i))
	   (n-fun (funcall j-fun j)))
      (setf (my-vitality i) 9)
      (lift:ensure-condition normal-error (funcall n-fun 10)))))

(lift:addtest
    test-attack-card-8
  (let ((i (random 255))
	(j (random 255)))
    (let* ((j-fun (attack-card i))
	   (n-fun (funcall j-fun j)))
      (setf (my-vitality i) 100)
      (setf (opp-vitality (- 255 j)) 0)
      (lift:ensure-same (funcall n-fun 10) #'i-card)
      (lift:ensure-same (my-vitality i)
			90)
      (lift:ensure-same (opp-vitality (- 255 j))
			0))))

(lift:addtest
    test-attack-card-9
  (let ((i (random 255))
	(j (random 255)))
    (let* ((j-fun (attack-card i))
	   (n-fun (funcall j-fun j)))
      (setf (my-vitality i) 100)
      (setf (opp-vitality (- 255 j)) -1)
      (lift:ensure-same (funcall n-fun 10) #'i-card)
      (lift:ensure-same (my-vitality i)
			90)
      (lift:ensure-same (opp-vitality (- 255 j))
			-1))))

(lift:addtest
    test-attack-card-10
  (let ((i (random 255))
	(j (random 255)))
    (let* ((j-fun (attack-card i))
	   (n-fun (funcall j-fun j)))
      (setf (my-vitality i) 99)
      (setf (opp-vitality (- 255 j)) 9)
      (lift:ensure-same (funcall n-fun 10) #'i-card)
      (lift:ensure-same (my-vitality i)
			89)
      (lift:ensure-same (opp-vitality (- 255 j))
			0))))

(lift:addtest
    test-help-card-1
  (let* ((i -1)
	 (j (random 255))
	 (j-fun (help-card i))
	 (n-fun (funcall j-fun j)))
    (lift:ensure-condition normal-error (funcall n-fun (random 255)))))

(lift:addtest
    test-help-card-2
  (let* ((i 256)
	 (j (random 255))
	 (j-fun (help-card i))
	 (n-fun (funcall j-fun j)))
    (lift:ensure-condition normal-error (funcall n-fun (random 255)))))

(lift:addtest
    test-help-card-3
  (let* ((i (random 255))
	 (j -1)
	 (j-fun (help-card i))
	 (n-fun (funcall j-fun j)))
    (lift:ensure-condition normal-error (funcall n-fun (random 255)))))

(lift:addtest
    test-help-card-4
  (let* ((i (random 255))
	 (j 256)
	 (j-fun (help-card i))
	 (n-fun (funcall j-fun j)))
    (lift:ensure-condition normal-error (funcall n-fun (random 255)))))

(lift:addtest
    test-help-card-5
  (let ((i (random 255))
	(j (random 255)))
    (let* ((j-fun (help-card i))
	   (n-fun (funcall j-fun j)))
      (setf (my-vitality i) 200)
      (setf (my-vitality j) 102)
      (lift:ensure-same (funcall n-fun 99) #'i-card)
      (lift:ensure-same (my-vitality i)
			101)
      (lift:ensure-same (my-vitality j)
			210))))

(lift:addtest
    test-help-card-6
  (let ((i (random 255))
	(j (random 255)))
    (let* ((j-fun (help-card i))
	   (n-fun (funcall j-fun j)))
      (setf (my-vitality i) 200)
      (lift:ensure-condition normal-error (funcall n-fun 210))
      (lift:ensure-condition normal-error (funcall n-fun :QWERTY)))))

(lift:addtest
    test-help-card-7
  (let ((i (random 255))
	(j (random 255)))
    (let* ((j-fun (help-card i))
	   (n-fun (funcall j-fun j)))
      (setf (my-vitality i) 100)
      (setf (my-vitality j) 0)
      (lift:ensure-same (funcall n-fun 10) #'i-card)
      (lift:ensure-same (my-vitality i)
			90)
      (lift:ensure-same (my-vitality j)
			0))))

(lift:addtest
    test-help-card-8
  (let ((i (random 255))
	(j (random 255)))
    (let* ((j-fun (help-card i))
	   (n-fun (funcall j-fun j)))
      (setf (my-vitality i) 100)
      (setf (my-vitality j) -1)
      (lift:ensure-same (funcall n-fun 10) #'i-card)
      (lift:ensure-same (my-vitality i)
			90)
      (lift:ensure-same (my-vitality j)
			-1))))

(lift:addtest
    test-help-card-9
  (let ((i (random 255))
	(j (random 255)))
    (let* ((j-fun (help-card i))
	   (n-fun (funcall j-fun j)))
      (setf (my-vitality i) 200)
      (setf (my-vitality j) 65525)
      (lift:ensure-same (funcall n-fun 10) #'i-card)
      (lift:ensure-same (my-vitality i)
			190)
      (lift:ensure-same (my-vitality j)
			65535))))

(lift:addtest
    test-copy-card-1
  (let ((i (random 255))
	(v1 (random 65535))
	(v2 #'dbl-card))
    (setf (opp-field i) v1)
    (lift:ensure-same (copy-card i)
		      v1)
    (setf (opp-field i) v2)
    (lift:ensure-same (copy-card i)
		      v2)))

(lift:addtest
    test-copy-card-2
  (lift:ensure-condition normal-error (copy-card -1))
  (lift:ensure-condition normal-error (copy-card 256)))

(lift:addtest
    test-revive-card-1
  (let ((i (random 255)))
    (setf (my-vitality i) -1)
    (lift:ensure-same (revive-card i) #'i-card)
    (lift:ensure-same (my-vitality i)
		      1)
    (setf (my-vitality i) 0)
    (lift:ensure-same (revive-card i) #'i-card)
    (lift:ensure-same (my-vitality i)
		      1)))

(lift:addtest
    test-revive-card-2
  (let ((i (random 255)))
    (setf (my-vitality i) 10)
    (lift:ensure-same (revive-card i) #'i-card)
    (lift:ensure-same (my-vitality i)
		      10)))

(lift:addtest
    test-revive-card-3
  (lift:ensure-condition normal-error (revive-card -1))
  (lift:ensure-condition normal-error (revive-card 256)))

(lift:addtest
    test-zombie-card-1
  (let* ((i (random 255))
	 (x-fun (zombie-card i))
	 (v1 (random 65535))
	 (v2 #'dbl-card))
    (setf (opp-vitality (- 255 i)) 0)
    (lift:ensure-same (funcall x-fun v1)
		      #'i-card)
    (lift:ensure-same (opp-field (- 255 i))
		      v1)
    (lift:ensure-same (opp-vitality (- 255 i))
		      -1)
    (lift:ensure-same (funcall x-fun v2)
		      #'i-card)
    (lift:ensure-same (opp-field (- 255 i))
		      v2)
    (lift:ensure-same (opp-vitality (- 255 i))
		      -1)
    (setf (opp-vitality (- 255 i)) -1)
    (setf (opp-field (- 255 i)) #'i-card)
    (lift:ensure-same (funcall x-fun v1)
		      #'i-card)
    (lift:ensure-same (opp-field (- 255 i))
		      v1)
    (lift:ensure-same (opp-vitality (- 255 i))
		      -1)
    (lift:ensure-same (funcall x-fun v2)
		      #'i-card)
    (lift:ensure-same (opp-field (- 255 i))
		      v2)
    (lift:ensure-same (opp-vitality (- 255 i))
		      -1)))

(lift:addtest
    test-zombie-card-2
  (let* ((i (random 255))
	 (x-fun (zombie-card i))
	 (v1 (random 65535))
	 (v2 #'dbl-card))
    (setf (opp-vitality (- 255 i)) 10)
    (lift:ensure-condition normal-error (funcall x-fun v1))
    (lift:ensure-condition normal-error (funcall x-fun v2))))

(lift:addtest
    test-zombie-card-3
  (let* ((i -1)
	 (x-fun (zombie-card i))
	 (v1 (random 65535))
	 (v2 #'dbl-card))
    (lift:ensure-condition normal-error (funcall x-fun v1))
    (lift:ensure-condition normal-error (funcall x-fun v2))))

(lift:addtest
    test-zombie-card-4
  (let* ((i 256)
	 (x-fun (zombie-card i))
	 (v1 (random 65535))
	 (v2 #'dbl-card))
    (lift:ensure-condition normal-error (funcall x-fun v1))
    (lift:ensure-condition normal-error (funcall x-fun v2))))

;; 
;;; run-tests
;;;
(defun run-all-tests ()
  (loop for test-suite in '(test-cards)
     do (lift:describe-test-result (lift:run-tests :suite test-suite
						   :report-pathname nil) t)))
