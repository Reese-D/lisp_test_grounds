;;; This file is built to create a portable pathname system across various lisp implementations
;;; it follows along with the 'learning common lisp' book


(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

;; determines if the given path is a directory pathname (in contrast to file path)
;; eg. "/home/reese" is a file path but "/home/reese/" is a directory pathname
(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

;;converts a file/directory pathname to a directory pathname
(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't convert wild pathnames"))
    (if (not (directory-pathname-p name))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name nil
	 :type nil
	 :defaults pathname)
	pathname)))

;; clisp specific workaround for portability
(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))



;; a simple attempt to list directories, but this doesn't account for
;; implementation specifics well enough, only works for cmu lispworks and sbcl

;; (defun list-directory (dirname)
;;   (when (wild-pathname-p dirname)
;;     (error "Can only list concrete directory names"))
;;   (directory (directory-wildcard dirname)))


(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "can only list concrete directory names"))
  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+opencml
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks opencml allegro clisp)
    (error "list-directory is not implemented or supported for this common lisp variant")))


;; the following function will only be compiled in clisp
#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))


;; a replacement for probe-file with increased compatability
(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
	(probe-file (pathname-as-file pathname)))
      (ignore-errors
	(let ((directory-form (pathname-as-directory pathname)))
	  (when (ext:probe-directory directory-form)
	    directory-form))))

  #-(or sbcl cmu lispworks openmcll allegro clisp)
  (error "file-exists-p not implemented in this common lisp variant"))



;;generally a clisp-only function, but it's useful so we'll make it available to all versions

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
	(let* ((directory (pathname-directory pathname))
	       (name-and-type (pathname (first (last directory)))))
	  (make-pathname
	   :directory (butlast directory)
	   :name (pathname-name name-and-type)
	   :type (pathname-type name-and-type)
	   :defaults pathname))
	pathname)))


(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
	 (cond
	   ((directory-pathname-p name)
	    (when (and directories (funcall test name))
	      (funcall fn name))
	    (dolist (x (list-directory name)) (walk x)))
	   ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))
