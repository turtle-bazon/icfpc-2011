(in-package :icfpc)

;; (defun make-attack-queue-0-0-1 (my-fn)
;;   "Attacks 0-th slot using our 0-th with attack value 1 (attack 0 0 1)"
;;   (let ((my (delay (funcall my-fn))))
;;     `((:right #'attack-card ,my)     ; 2 0 attack
;;       (:right #'zero-card   ,my)     ; 2 0 zero
;;       (:right #'zero-card   ,my)     ; 2 0 zero
;;       (:left  #'K-card      ,my)     ; 1 K 0
;;       (:left  #'S-card      ,my)     ; 1 S 0
;;       (:right #'succ-card   ,my)     ; 2 0 succ
;;       (:right #'zero-card   ,my))))  ; 2 0 zero

;; (defun make-attack-queue (storage)
;;   "Attacks 0-th slot using our 0-th slot with attack value given in 0-th slot"
;;   (list (:right #'attack-card storage)	 ; 2 x attack
;; 	(:right #'zero-card   storage)	 ; 2 x zero
;; 	(:right #'zero-card   storage)	 ; 2 x zero
;; 	(:left  #'K-card      storage)	 ; 1 K x
;; 	(:left  #'S-card      storage)	 ; 1 S x
;; 	(:right #'get-card    storage)	 ; 2 x succ
;; 	(:right #'zero-card   storage))) ; 2 x zero

(defun write-number (slot n)
  "Writes n to slot"
  (append `((:left  #'put-card  ,slot)
	    (:right #'zero-card ,slot))
	  (loop repeat n
	     collect `(:left #'succ-card ,slot))))

(defun b-combinator (storage b c)
  "B combinator: Babc = S(Ka)bc = a(bc)"
  `((:left  #'k-card    ,storage)
    (:left  #'s-card    ,storage)
    (:right ,b          ,storage)
    (:right ,c          ,storage)))

(defun b2-combinator (storage b c d)
  "B2 combinator: B2 a b c = S(K(S(Ka)b))cd = a(b(cd))"
  `((:left  #'k-card    ,storage)
    (:left  #'s-card    ,storage)
    (:right ,b          ,storage)
    (:left  #'k-card    ,storage)
    (:right #'s-card    ,storage)
    (:right ,c          ,storage)
    (:right ,d          ,storage)))

(defun attack-queue (storage i j n)
  "Function is written to storage; it attacks j-th opponent's slot using i-th our with given value n"
  (unless (/= storage 0) (normal-error))
  (append `((:left  #'put-card    ,storage)
	    (:right #'attack-card ,storage))
	  (write-number 0 i)
	  (b-combinator storage #'get-card #'zero-card)
	  (write-number 0 j)
	  (b-combinator storage #'get-card #'zero-card)
	  (write-number 0 n)
	  (b-combinator storage #'get-card #'zero-card)))

(defun y-combinator (storage)
  "Y f = f (Y f)
   Y = S S K (S (K (S S (S (S S K)))) K)"
  (unless (/= storage 0) (normal-error))
  (append `((:left  #'put-card   0)
	    (:left  #'put-card   1)
	    (:right #'s-card     0)
	    (:right #'s-card     0)
	    (:right #'k-card     0)
	    (:left  #'s-card     0)	; 0 -> s(ssk)
	    (:right #'s-card     1)
	    (:left  #'s-card     1))	; 1 -> ss
	  (b-combinator 1 #'get-card #'zero-card) ; 1 -> ss(s(ssk))
	  `((:left  #'k-card     1)
	    (:left  #'s-card     1)
	    (:right #'k-card     1)
	    (:left  #'k-card     1)	; 1 -> k(s(k(ss(s(ssk)))) k)
	    (:left  #'put-card   ,storage)	; storage -> I
	    (:right #'s-card     ,storage)
	    (:left  #'s-card     ,storage))
	  (b2-combinator storage #'get-card #'succ-card #'zero-card)))

(defun infinite-attack (storage)