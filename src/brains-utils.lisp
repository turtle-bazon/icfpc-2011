(in-package :icfpc)

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
	(min-id 0))
    (dotimes (i 256)
      (multiple-value-bind (v p)
	  (%get-vitality i player)
	(when (and p (< v min-val))
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

(defun imitate-queue (queue)
  (map nil #'(lambda (x) (apply #'imitate-my-move x)) queue))
