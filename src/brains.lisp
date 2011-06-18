(in-package :icfpc)

(defun make-move (opp-move)
  (funcall #'make-move-1 opp-move))

(defun dumb-move ()
  (list :left #'i-card 0))

#|(defun dumb-make-move (opp-move)
  (declare (ignore opp-move))
  (dumb-move))|#

(defun %most-alive (player)
  (let ((max-val -2)
	(max-id -1))
    (dotimes (i 256)
      (multiple-value-bind (v p)
	  (%get-vitality i player)
	(when (and p (> v max-val))
	  (setf max-val v
		max-id i))))
    (values max-id max-val)))

(defun %least-alive (player)
  (let ((min-val most-positive-fixnum)
	(min-id -1))
    (dotimes (i 256)
      (multiple-value-bind (v p)
	  (%get-vitality i player)
	(when (and p (> v min-val))
	  (setf min-val v
		min-id i))))
    (values min-id min-val)))

(defun my-most-alive () (%most-alive *player1*))
(defun opp-most-alive () (%most-alive *player2*))
(defun my-least-alive () (%least-alive *player1*))
(defun opp-least-alive () (%least-alive *player2*))

;; lazy eval

(defstruct promise
  (expr (lambda () NIL) :type function)
  (done nil :type boolean)
  (value nil))

(defmacro delay (form)
  `(make-promise
    :expr (lambda () ,form)))

(defun do-promise (promise)
  (with-slots (expr done value) promise
    (setf value (multiple-value-list (funcall expr)))
    (setf done t)
    (apply #'values value)))

(defun force (promise)
  (cond ((not (eq (type-of promise) 'promise)) promise)
        ((promise-done promise) (apply #'values (promise-value promise)))
        (t (do-promise promise))))

;; queues

(defun make-attack-queue (my-fn opp-fn source-fn)
  (let ((my     (delay (funcall my-fn)))
	(opp    (delay (funcall opp-fn)))
	(source (delay (funcall source-fn))))
    `((:left  #'attack-card ,my)
      (:right ,opp          ,my)
      (:right ,source       ,my))))

(defun shift-queue (queue)
  (values (mapcar #'force (car queue))
	  (cdr queue)))


(defun make-move-1 (opp-move)
  (declare (ignore opp-move))
  (let ((aim (opp-least-alive)))
    (print aim *error-output*)
    (if (typep aim 'slot-no)
	(list :left #'attack-card aim)
	(dumb-move))))
