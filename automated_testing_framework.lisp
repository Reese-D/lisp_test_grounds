(defun simple-test+- ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= ( + 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= ( - 1 3) -2) '(= (- 1 3) -2)))

;;refactor
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun simple-test2+- ()
  (report-result (= ( + 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= ( - 1 3) -2) '(= (- 1 3) -2)))


;;macro refactor
(defmacro check (form)
  `(report-result ,form ',form))

(defun simple-test3+- ()
  (check (= (+ 1 2) 3))
  (check (= (- 1 3) -2)))


;;further refactor
(defmacro check2 (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun simple-test+-4 ()
  (check2
   (= (+ 1 2) 3)
   (= (- 1 3) -2)))

;;expanding on the automation idea
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check3 (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun simple-test+-5 ()
  (check3
    (= (+ 1 2) 3)
    (= (- 1 3) -2)))



;; just an idea
(defmacro string-to-list (str)
  `(read-from-string (concatenate 'string "(" ,str ")")))
