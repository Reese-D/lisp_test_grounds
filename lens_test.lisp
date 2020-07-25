(defvar example-plist '(:first 1
			:inner (:second 2
				:inner (:third 3
					:inner (:fourth 4 :tacks "final tacks")
					:tacks "some more tacks")
				:tacks "this")
			:tacks "tacky"))

;;repeat into a list
(defun repeat (item times)
  (if (eq times 0) '()
      (cons item (repeat item (- times 1)))))

;; (defun expand (token)
;;   (if (eq (type-of token) 'cons) (eval token)
;;       token))

;;functional plist search and get
(defun get-search (list &rest tokens)
  (let ((current (getf list (car tokens))))
    (if (eq (cdr tokens) nil)
        current
        (if (eq tokens nil) '()
            (apply #'get-search (append (list current) (cdr tokens)))))))

(defun set-copy (list key value)
  (if (eq (car list) nil) nil
      (let ((next (set-copy (cdr list) key value)))
	(if (eq (car list) key)
            (append (list key value) (cdr next))
            (cons (car list) next)))))

;;;Example usage

(get-search example-plist :inner :inner :inner)

(set-copy example-plist :inner "me now")

;;set first inner to second inner
(set-copy example-plist :inner (get-search example-plist :inner :inner))





;;------------------------------------------------------------------------------------------
;; Lenses (A stands for All, S stands for Sub)
;;------------------------------------------------------------------------------------------

;;Lets create an object to lense over

(defvar company '(:name "text co"
		  :location (:name "brazil"
			     :department ((:name "hr")
					  (:name "spoons")
					  (:name "construction"))
			     :address (:name "nickel st" :city "holland" :state "california"))
		  :owner (:name "townsley"
			  :address (:name "dimes st" :city "burrowsville" :state "wyoming"))))

;;Lens get interface:
;; (get (a) s)

;;Lens set interface:
;;(set (a s) s)

;;Create some for the :location tag
(defun g-location (A)
  (getf A :location))

(defun s-location (A S)
  (set-copy A :location S))


;;actually these look very macro-able, lets make a macro to make these functions for us

;;lets make a helper function to make the name system first
;;(hyphen-tags '(:company :g :example))
;;would return the string "COMPANY-G-LETTER"
(defun hyphen-tags (tags)
  (if (eq (car (cdr tags)) nil) (string (car tags))
      (concatenate 'string
		   (concatenate 'string (string (car tags)) "-")
		   (hyphen-tags (cdr tags)))))

;;our macros
(defmacro lens-getter (child-tag)
  `(defun ,(read-from-string (hyphen-tags (list :g child-tag))) (A)
     (getf A ,child-tag)))

(defmacro lens-setter (child-tag)
  `(defun ,(read-from-string (hyphen-tags (list :s child-tag))) (A S)
     (set-copy A ,child-tag S)))

(defmacro create-lense (C)
  `(values-list (list (lens-getter ,C) (lens-setter ,C))))


;;lets define a new more lenses now
(create-lense :department) ;;now we can get department out of a location object, and set it too
(create-lense :location) ;;redefing g-location and s-location
(create-lense :name)
;;lets make a function to compose any 2 lenses
(defmacro compose-g-lens (A B obj)
  `(,B (,A ,obj)))

;;we could make a macro to compose a list instead of just 2
;;we could also create a function that creates these based off of the company structure automatically, which is kind of neat.

;;all of these are identical now
(g-department (g-location company))
(getf (getf company :location) :department)
(get-search company :location :department)
(compose-g-lens g-location g-department company)

(defun location-department-names (comp)
  (map 'list #'g-name (compose-g-lens g-location g-department company)))

;;now we've created a specific lens to get the department names in a given company
(location-department-names company)

;;get-search example
(map 'list #'g-name (get-search company :location :department ))

(g-name '(:NAME "hr"))

(map 'int (lambda (x) (+ 1 x)) '(1 2 3))
;;a better idea is to name the lense just off the thing you're getting and not to care about the parent at all



;; a function to get any top level keys from a list
(defun get-keys (plist)
  (if (not (eq (type-of plist) 'CONS)) (values)
      (remove-if-not (lambda (x) (eq (type-of x) 'KEYWORD)) plist))) 


;; ;;create all lenses 1 level deep
;; (defmacro create-lense-1-deep (&rest keys)
;;   (case (length keys)
;;     (1  `(multiple-value-list (create-lense ,(car keys))))
;;     (otherwise  `(concatenate 'list
;;                               (create-lense-1-deep ,@(cdr keys))
;;                               (multiple-value-list (create-lense ,(car keys)))))))
        

;; ;;create lenses for all levels
;; (defun get-all-keys (plist)
;;   (let ((keys (get-keys plist)))
;;     (if (eq keys nil) (values)
;;         (cons
;;                      (cons (car keys) (get-all-keys (getf plist (car keys))))
;;                      (get-all-keys (cddr plist))))))




;;the plist is effectively an m-ary tree structure
;;so this will apply a function (a -> b) depth first
(defun apply-depth-first (func tree)
  (if (atom tree) (funcall func tree)
      (map 'list #'apply-depth-first func tree)))

;;some tests
(defun id (x)
  x)

(defun print-if-key (obj)
  (if (eq (type-of obj) 'KEYWORD)
      obj
      nil))
        

(apply-depth-first #'id company)

(remove-if-not (lambda (x) (not (eq nil x))) (apply-depth-first #'print-if-key company))

;;currying should help for the function we need to pass in
(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;lets try to create a list of depth-first searches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;first lets modify the function to track depth
(defun apply-depth-first (func tree &optional (depth 0))
  (if (atom tree) (funcall func tree depth)
      (map 'list #'apply-depth-first func tree depth)))


;;now we can easily build lists of branches depth and breadth wise

;; (let ((prior-depth nil)
;;       (lists nil))
;;   (lambda (node depth)


(defun take-last (n list)
  (subseq list (- (length list) n) (length list)))

(defun depth-first-closure ()
  (let ((prior-depth 0)
        (lists nil))
    (lambda (node depth) ;lexical closure
      (progn
        (if (>= depth prior-depth)
            (setq lists (cons (cons node (car lists)) (cdr lists)))
            ;;TODO fix this, it needs to take int account how many we backtraced, not just create a whole new list
            (setq lists
                  (cons (cons node (take-last
                                    (- prior-depth depth)
                                    (car lists)))
                        lists)))
        (setq prior-depth depth)
        lists))))

(defparameter fun (depth-first-closure))
(funcall fun 0 1)

  
          
          
      
              
