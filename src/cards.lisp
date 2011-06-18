(in-package :icfpc)

(defparameter *max-field* 65535)

(defun i-card (x)
  "Тождественная функция возвращает аргумент"
  x)

(defun zero-card (x)
  "Возвращает константу 0"
  (declare (ignore x))
  0)

(defun succ-card (n)
  "Увеличивает аргумент на 1"
  (unless (typep n 'vitality) (normal-error))
  (if (= n *max-field*) n (1+ n)))

(defun dbl-card (n)
  "Удваивает аргумент и возвращает результат, но не более максимума"
  (unless (typep n 'vitality) (normal-error))
  (min *max-field* (* 2 n)))

(defun get-card (i)
  "Возвращает значение поля field в i-ом слоте защищающегося игрока"
  (unless (and (typep i 'slot-no)
	       (alive-p i))
    (normal-error))
  (my-field i))

(defun put-card (x)
  "Возвращаяет тождественную функцию"
  (declare (ignore x))
  #'i-card)

(defun s-card (f)
  "S-кобминатор"
  (lambda (g)
    (lambda (x)
      (let ((h (card-call f x))
	    (y (card-call g x)))
	(card-call h y))))) ;; also check 'z' to be a function?

(defun k-card (x)
  "K-комбинатор"
  (lambda (y)
    (declare (ignore y))
    x))

(defun inc-card (i)
  "Увеличивает значение здоровья i-го слота на 1, но не более максимума"
  (unless (typep i 'slot-no) (normal-error))
  (let ((v (my-vitality i)))
    (when (and (plusp v) (< v *max-field*))
      (incf (my-vitality i))))
  #'i-card)

(defun dec-card (i)
  "Ументшает значение здоровья i-го слота на 1, но результат не меньше 0"
  (unless (typep i 'slot-no) (normal-error))
  (let ((v (opp-vitality (- 255 i))))
    (when (plusp v)
      (decf (opp-vitality (- 255 i)))))
  #'i-card)

(defun attack-card (i)
  "Вычитает из здоровья i-го слота защищаегося n,
вычитает из здоровья j-го слота оппонетна n * 9/10, но результат не меньше 0,
возвращает тождественную фукнцию"
  (lambda (j)
    (lambda (n)
      (unless (and (typep i 'slot-no)
		   (typep j 'slot-no)
		   (typep n 'integer)
		   (<= n (my-vitality i)))
	(normal-error))
      (decf (my-vitality i) n)
      (when (nth-value 1 (opp-vitality (- 255 j)))
	(when (minusp (decf (opp-vitality (- 255 j)) (floor (* n 9/10))))
	  (setf (opp-vitality (- 255 j)) 0)))
      #'i-card)))

(defun help-card (i)
  "Вычитает из здоровья i-го слота защищаегося n,
лечит j-ый слот защищиегося на n * 11/10, но не больше максимума,
возвращает тождественную фукнцию"
  (lambda (j)
    (lambda (n)
      (unless (and (typep i 'slot-no)
		   (typep j 'slot-no)
		   (typep n 'integer)
		   (<= n (my-vitality i)))
	(normal-error))
      (decf (my-vitality i) n)
      (when (plusp (my-vitality j))
	(setf (my-vitality j)
	      (min *max-field*
		   (+ (my-vitality j) (floor (* n 11/10))))))
      #'i-card)))

(defun copy-card (i)
  "Возвращаяет значение field i-го слота защищающегося"
  (unless (typep i 'slot-no)
    (normal-error))
  (opp-field i))

(defun revive-card (i)
  "Оживлает i-ый слот защищающегося, здоровье оживлённого равно 1.
Если i-ый слот жив, не делает ничего. Возвращает тождественную фукнцию"
  (unless (typep i 'slot-no) (normal-error))
  (when (<= (my-vitality i) 0)
    (setf (my-vitality i) 1))
  #'i-card)

(defun zombie-card (i)
  "Если i-ый слот мёртв (здовье 0 или -1), устанавливает его здоровье в -1,
в поле field кладёт x."
  (lambda (x)
      (unless (and (typep i 'slot-no)
		   (not (nth-value 1 (opp-vitality (- 255 i)))))
	(normal-error))
    (setf (opp-field (- 255 i)) x
	  (opp-vitality (- 255 i)) -1)
    #'i-card))
