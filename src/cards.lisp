(in-package :icfpc-2011)

(deftype slot-no ()
  '(integer 0 255))

(deftype vitality ()
  '(integer -1 65535))

;; Внутриигровой кондишен.
(define-condition card-condition () ())
;; Закончились ходы.
(define-condition rounds-limit () ())

(defvar *call-count*)

(defun card-call (card-fun arg)
  (incf *call-count*)
  (when (<= 1000 *call-count*)
    (signal 'rounds-limit))
  (unless (typep card-fun 'function)
    (signal 'card-condition))
  (funcall card-fun arg))

(defun i-card (x)
  "Тождественная функция возвращает аргумент"
  x)

(defun zero-card (x)
  "Возвращает константу 0"
  (declare (ignore x))
  0)

(defun succ-card (n)
  "Увеличивает аргумент на 1"
  (unless (typep n 'vitality)
    (signal 'card-condition))
  (if (= n *max-field*) n (1+ n)))

(defun dbl-card (n)
  "Удваивает аргумент и возвращает результат, но не более максимума"
  (unless (typep n 'vitality)
    (signal 'card-condition))
  (min *max-field* (* 2 n)))

(defun get-card (i)
  "Возвращает значение поля field в i-ом слоте защищающегося игрока"
  (unless (and (typep i 'slot-no)
	       (nth-value 1 (my-vitality i)))
    (signal 'card-condtion))
  (my-field i))

(defun put-card (x)
  "Возвращаяет тождественную функцию"
  (declare (ignore x))
  #'identity)

(defun s-card (f)
  "S-кобминатор"
  (lambda (g)
    (lambda (x)
      (let ((h (card-call f x))
	    (y (card-call g x)))
	(card-call h y)))))

(defun k-card (x)
  "K-комбинатор"
  (lambda (y)
    (declare (ignore y))
    x))

(defun inc-card (i)
  "Увеличивает значение здоровья i-го слота на 1, но не более максимума"
  (unless (typep i 'slot-no)
    (signal 'card-condtion))
  (let ((v (my-vitality i)))
    (when (and (plusp v) (< v *max-field*))
      (incf (my-vitality i)))))

(defun dec-card (i)
  "Ументшает значение здоровья i-го слота на 1, но результат не меньше 0"
  (unless (typep i 'slot-no)
    (signal 'card-condtion))
  (let ((v (opp-vitality (- 255 i))))
    (when (plusp v)
      (decf (opp-vitality (- 255 i))))))

(defun attack-card (i)
  "Вычитает из здоровья i-го слота защищаегося n,
вычитает из здоровья j-го слота оппонетна n * 9/10, но результат не меньше 0,
возвращает тождественную фукнцию"
  (lambda (j)
    (lambda (n)
      (unless (and (typep i 'legal-slot-no)
		   (typep j 'legal-slot-no)
		   (or (not (integerp n))
		       (> n (my-vitality i))))
	(signal 'card-condtion))
      (decf (my-vitality i) n)
      (when (nth-value 1 (opp-vitality (- 255 j)))
	(when (minusp (decf (opp-vitality (- 255 j)) (floor (* n 9/10))))
	  (setf (opp-vitality (- 255 j)) 0)))
      #'identity)))

(defun help-card (i)
  "Вычитает из здоровья i-го слота защищаегося n,
лечит j-ый слот защищиегося на n * 11/10, но не больше максимума,
возвращает тождественную фукнцию"
  (lambda (j)
    (lambda (n)
      (unless (and (typep i 'legal-slot-no)
		   (typep j 'legal-slot-no)
		   (or (not (integerp n))
		       (> n (my-vitality i))))
	(signal 'card-condtion))
      (decf (my-vitality i) n)
      (when (plusp (my-vitality j))
	(when (> (incf (my-vitality j) (floor (* n 11/10))) *max-field*)
	  (setf (my-vitality j) *max-field*)))
      #'identity)))

(defun copy-card (i)
  "Возвращаяет значение field i-го слота защищающегося"
  (unless (typep i 'slot-no)
    (signal 'card-condtion))
  (my-field i))

(defun revive-card (i)
  "Оживлает i-ый слот защищающегося, здоровье оживлённого равно 1.
Если i-ый слот жив, не делает ничего. Возвращает тождественную фукнцию"
  (unless (typep i 'slot-no)
    (signal 'card-condtion))
  (when (<= (my-vitality i) 0)
    (setf (my-vitality i) 1))
  #'identity)

(defun zombie-card (i)
  "Если i-ый слот мёртв (здовье 0 или -1), устанавливает его здоровье в -1,
в поле field кладёт x."
  (unless (typep i 'slot-no)
    (signal 'card-condtion))
  (lambda (x)
    (setf (opp-field i) x
	  (opp-vitality i) -1)
    #'identity))
