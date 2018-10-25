(defun delta (a b c)
  (- (expt b 2) (* 4 a c)))

(defun abc (a b c)
  (let ((D (delta a b c)))
    (cond ((< D 0) (print "no solutions"))
	  ((= D 0) (let ((x (/ b (* 2 a))))
		     (print x)))
	  (t (let ((x1 (print "x1"))
		   (x2 (print "x2")))
	       )))))


