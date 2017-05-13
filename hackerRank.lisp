(defun solve-me-first-old (y)
  (let ((head (car y)) (tail (cdr y)))
    (if (> (length tail) 0)
	(+ head (solve-me-first tail))
	head)))

(defmacro string-to-list (str)
  `(read-from-string (concatenate 'string "(" ,str ")")))


;;beautiful word


(defvar vowels '(#\a #\e #\i #\o #\u #\y))

(defun are-equal (a b)
  (if (char= a b)
      t
      (if (and
	   (find a vowels)
	   (find b vowels))
	  t nil)))

(defun solve-me-first (y)
  (let ((truthval t))
    (do ((str y (cdr str)))
	((= (length str) 1) truthval)
      (if (are-equal (car str) (car (cdr str))) (setq truthval nil)))))


(defun string-to-char-list (str)
  (let ((output nil))
    (with-input-from-string (tmp str)
      (do ((c (read-char tmp) (read-char tmp nil 'the-end)))
	  ((not (characterp c)) (reverse output))
	(push c output)))))

(defvar input (string-to-char-list (read-line)))

(if (solve-me-first input) (format t "Yes") (format t "No"))




