(defparameter *peg-rack* '((peg-a (1 2 3))
                           (peg-b ())
                           (peg-c ())))

(defparameter *nm* 0)

(defun move-disk (src dst)
  (setf *nm* (1+ *nm*))
  (push (pop (cadr (assoc src *peg-rack*))) (cadr (assoc dst *peg-rack*)))
  (print-towers))

(defun get-lst (peg)
  (cadr (assoc peg *peg-rack*)))

(defun get-len (peg)
  (length (get-lst peg)))

(defun print-towers ()
  (format t "~10:a ~10:a ~10:a~%"
          (get-lst 'peg-a)
          (get-lst 'peg-b)
          (get-lst 'peg-c)))

(defun move-tower (disk src dst aux)
  (if (equalp disk 1)
      (move-disk src dst)
      (progn
        (move-tower (decf disk) src aux dst)
        (move-disk src dst)
        (move-tower (decf disk) aux dst src))))

(defun solve ()
  (print-towers)
  (time (move-tower (get-len 'peg-a) 'peg-a 'peg-c 'peg-b))
  (format t "Number of moves: ~D~%" *nm*))

