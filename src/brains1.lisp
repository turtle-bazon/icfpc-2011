(in-package :icfpc)

(defun make-attack-queue-0-0-1 (my-fn)
  "Attacks 0-th slot using our 0-th with attack value 1 (attack 0 0 1)"
  (let ((my (delay (funcall my-fn))))
    `((:right #'attack-card ,my)     ; 2 0 attack
      (:right #'zero-card   ,my)     ; 2 0 zero
      (:right #'zero-card   ,my)     ; 2 0 zero
      (:left  #'K-card      ,my)     ; 1 K 0
      (:left  #'S-card      ,my)     ; 1 S 0
      (:right #'succ-card   ,my)     ; 2 0 succ
      (:right #'zero-card   ,my))))  ; 2 0 zero

(defun make-attack-queue (storage)
  "Attacks 0-th slot using our 0-th slot with attack value given in 0-th slot"
  (list (:right #'attack-card storage)	 ; 2 x attack
	(:right #'zero-card   storage)	 ; 2 x zero
	(:right #'zero-card   storage)	 ; 2 x zero
	(:left  #'K-card      storage)	 ; 1 K x
	(:left  #'S-card      storage)	 ; 1 S x
	(:right #'get-card    storage)	 ; 2 x succ
	(:right #'zero-card   storage))) ; 2 x zero