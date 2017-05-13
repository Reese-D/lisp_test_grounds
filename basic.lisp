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

;;clobbers db so anything that hasn't been saved to disc will be overwritten
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;;lambda example
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))
(defun select-by-type (type name)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd type) name)) *db*))

;;first class function example
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

(select (artist-selector "bang"))

;;key paramters and supplied-p parameters
(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db* 
	(mapcar ;apply lambda to each item in db
	 #'(lambda (row)
	     (when (funcall selector-fn row) ;(selector-fn row)
	       (if title (setf (getf row :title) title)) ;if not nil change value
	       (if artist (setf (getf row :artist) artist))
	       (if rating (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row)
	 *db*)))

;;macros
(defmacro backwards (expr) (reverse expr))
(backwards ("hello, world" t format))

(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))

(defun make-comparison-expr-alternate (field value)
  `(equal (getf cd ,field) ,value))

(make-comparison-expr :rating 2)


(defun make-comparisons-list (fields)
  (loop while fields
       collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where-macro (&rest clauses) ;;&rest means you can supply an arbitrary number of arguments whereas &key makes them optional and named
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

;; `(and ,(list 1 2 3)) --> (AND (1 2 3))
;; `(and ,@(list 1 2 3)) --> (AND 1 2 3)

;;view macros with macroexpand-1
(macroexpand-1 '(where-macro :title "Give Us a Break" :ripped t))


