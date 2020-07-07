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
      (let ((next (lens-set (cdr list) key value)))
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
		  :address (:name "townsley" :city "burrowsville" :state "wyoming")))
			     
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

;;lets make a function to compose any 2 lenses
(defmacro compose-g-lens (A B obj)
  `(,B (,A ,obj)))

;;all of these are identical now
(g-department (g-location company))
(getf (getf company :location) :department)
(get-search company :location :department)
(compose-g-lens g-location g-department company)
  

;;some random stuff
(s-department (g-location company) "new department")



;;a better idea is to name the lense just off the thing you're getting and not to care about the parent at all
