(defun build (a b)
  (if (> (length a) 500)
      (concatenate 'string a b)
      (build (concatenate 'string a b) (concatenate 'string b a))))

(defun get-pos(a)
  (char *binary-string* a))

(defvar *binary-string* (build "0" "1"))
(defvar *num-lines* (parse-integer (read-line)))

(do ((line-index *num-lines* (1- line-index)))
	((= 0 line-index) line-index)
      (format t "~C~%" (get-pos (parse-integer (read-line)))))
