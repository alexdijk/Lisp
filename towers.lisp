;;
;; solution to tower of hanoi problem with variable number of disks
;;
(defparameter *peg-rack* '((peg-a ())
                           (peg-b ())
                           (peg-c ())))

(defun fill-rack (n)
  (loop as i from n downto 1
        do (push i (cadr (assoc 'peg-a *peg-rack*)))))

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
  (format t "~20:a ~20:a ~20:a~%"
          (get-lst 'peg-a)
          (get-lst 'peg-b)
          (get-lst 'peg-c)))

(defun move-tower (disk src dst aux)
  (if (equalp disk 1)
      (move-disk src dst)
      (progn
        (move-tower (1- disk) src aux dst)
        (move-disk src dst)
        (move-tower (1- disk) aux dst src))))

(defun solve (n)
  (fill-rack n)
  (print-towers)
  (move-tower (get-len 'peg-a) 'peg-a 'peg-c 'peg-b)
  (format t "~%# of moves: ~D~%" *nm*))
