(in-package :icfpc)

;;; TODO: rewrite, use dbl and binary (4 = dbl(dbl(1))
;;; done. puts strictly 'n' into slot, if strict is t
;;; else puts a number more than 'n'
(defun write-number (slot n &optional (strict t))
  "Writes n to slot"
  (append `((:left  ,#'put-card  ,slot)
	    (:right ,#'zero-card ,slot))
	  (unless (zerop n)
	    (let ((d (floor (log n 2))))
	      (unless (or (<= (my-vitality slot) (expt 2 (1+ d)))
			  strict)
		(incf d))
	      (append `((:left ,#'succ-card ,slot))
		      (loop repeat d
			    collect `(:left ,#'dbl-card ,slot))
		      (when (or (<= (my-vitality slot) (expt 2 (1+ d)))
				strict)
			(loop repeat (rem n (expt 2 d))
			      collect `(:left ,#'succ-card ,slot))))))))

(defun b-combinator (storage b c)
  "B combinator: Babc = S(Ka)bc = a(bc)"
  `((:left  ,#'k-card   ,storage)
    (:left  ,#'s-card   ,storage)
    (:right ,b          ,storage)
    (:right ,c          ,storage)))

(defun b2-combinator (storage b c d)
  "B2 combinator: B2 a b c = S(K(S(Ka)b))cd = a(b(cd))"
  `((:left  ,#'k-card   ,storage)
    (:left  ,#'s-card   ,storage)
    (:right ,b          ,storage)
    (:left  ,#'k-card   ,storage)
    (:right ,#'s-card   ,storage)
    (:right ,c          ,storage)
    (:right ,d          ,storage)))

(defun attack-queue (storage i j n)
  "Function is written to storage; it attacks j-th opponent's slot using i-th our with given value n"
  (unless (/= storage 0) (normal-error))
  (append `((:left  ,#'put-card    ,storage)
	    (:right ,#'attack-card ,storage))
	  (write-number 0 i)
	  (b-combinator storage #'get-card #'zero-card)
	  (write-number 0 j)
	  (b-combinator storage #'get-card #'zero-card)
	  (write-number 0 n nil) ;; equal to or more than n
	  (b-combinator storage #'get-card #'zero-card)))

;; has to be tested
(defun y-combinator (storage slot-f)
  "Y f = f (Y f)
   Y = S S K (S (K (S S (S (S S K)))) K)"
  (unless (/= storage 0) (normal-error))
  (append `((:left  ,#'put-card   0)
	    (:left  ,#'put-card   1)
	    (:right ,#'s-card     0)
	    (:right ,#'s-card     0)
	    (:right ,#'k-card     0)
	    (:left  ,#'s-card     0)	; 0 -> s(ssk)
	    (:right ,#'s-card     1)
	    (:left  ,#'s-card     1))	; 1 -> ss
	  (b-combinator 1 #'get-card #'zero-card) ; 1 -> ss(s(ssk))
	  `((:left  ,#'k-card     1)
	    (:left  ,#'s-card     1)
	    (:right ,#'k-card     1)
	    (:left  ,#'k-card     1)	; 1 -> k(s(k(ss(s(ssk)))) k)
	    (:left  ,#'put-card   ,storage)	; storage -> I
	    (:right ,#'s-card     ,storage)
	    (:left  ,#'s-card     ,storage))
	  (b2-combinator storage #'get-card #'succ-card #'zero-card)
	  (write-number 0 slot-f)
	  (b-combinator 2 #'get-card #'zero-card)))

;; has to be tested
(defun w-combinator (storage slot-x slot-y)
  "W x y = x y y
   W = S S (K (S K K))"
  (unless (/= storage 0) (normal-error))
  (append `((:left  ,#'put-card   0)
	    (:left  ,#'put-card   1)
	    (:right ,#'s-card     0)
	    (:right ,#'k-card     0)
	    (:right ,#'k-card     0)
	    (:left  ,#'k-card     0)	; 0 -> k(skk)
	    (:right ,#'s-card     1)
	    (:left  ,#'s-card     1))	; 1 -> ss
	  (b-combinator 1 #'get-card #'zero-card) ; 1 -> ss(k(skk))
	  (write-number 0 slot-x)
	  (b-combinator 1 #'get-card #'zero-card) ; 1 -> W x
	  (write-number 0 slot-y)
	  (b-combinator 1 #'get-card #'zero-card)))

;;; doesn't work, has to be rewritten - ?
;(defun infinite-attack (storage i j n)
;  "Y (attack i j n)"
;  (append (attack-queue storage i j n)
;	  (y-combinator storage)))
