(defun find-jumps (prev i g seed p)
  (let ((lst nil))
    (do ((n 0 (+ n 1)))
	((eql n i) (reverse lst))
      (setq prev (car (push (mod (+ (* prev g) seed) p) lst))))))

