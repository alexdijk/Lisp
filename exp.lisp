;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A system to calculate the top and x1 and x2 of a
;; quadratic equation
;;
(defun axDiscr (a b c)
  (- (expt b 2) (* 4 a c)))

(defun axX1X2 (a b c)
  (let ((D (axDiscr a b c)))
    (cond ((< D 0) (print "no solutions"))
	  ((= D 0) (let ((x (/ b (* 2 a))))
		     (print x)))
	  ((> D 0) (let ((x1 (/ (- (* b -1) (sqrt D)) (* 2 a)))
			 (x2 (/ (+ (* b -1) (sqrt D)) (* 2 a))))
                     (format t "x1:~,2f, x2:~,2f~%" x1 x2))))))

(defun axX0Y0 (a b c)
  (let ((zx (/ (* -1 b) (* 2 a))))
    (let ((zy (+ (* (expt zx 2) a) (* zx b) c)))
      (format t "x0, y0: (~,2f,~,2f) " zx zy))))

(defun axQuad (a b c)
  (axX0Y0 a b c)
  (axX1X2 a b c))
