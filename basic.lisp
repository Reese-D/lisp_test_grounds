(defun hello-world ()
  (format t "hello, world"))

(defun make-cd(title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)
(defun add-record (cd) (push cd *db*))



(defun dump-db-oneliner ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun dump-db-twoline ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))



(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))


(defun custom-int-parse (int)
  (or (parse-integer int :junk-allowed t) 0))
  
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (custom-int-parse (prompt-read "Artist"))
   (parse-integer (prompt-read "Rating") :junk-allowed t)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;; print is important here vs format, as it standardizes it for reading back in later
(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))
