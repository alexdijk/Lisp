; simulation of the machi koro game

(defun throw-dice (n)
  (if (eql n 1)
      (1+ (random 6))
      (+ (1+ (random 6)) (1+ (random 6)))))

(defun prodrnd ()
    (loop for i from 1 to 100
          do (format t "~D " (throw-dice 2))))

