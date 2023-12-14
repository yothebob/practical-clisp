;;; core
(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd)
  (push cd *db*))


;;; adding cds

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(dump-db)

;;; improving user interface

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
	(if (not (y-or-n-p "Another [y.n]: "))(return))))

;;; query database

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf :artist) artist))))

;;; the better lisp way to do it
(defun select (select-fn)
  (remove-if-not select-fn *db*))

;;usage :
(select #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")))

;;; that is pretty gross so try this:
(defun artist-selector (artist)
  (lambda (cd) (equal (getf :artist) artist)))

(select (artist-selector "Dixie Chicks"))

(defun where (&key artist title rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title (setf (row :title) title))
	       (if artist (setf (row :artist) artist))
	       (if rating (setf (row :rating) rating))
	       (if ripped-p (setf (row :ripped) ripped)))
	 row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

;;; save
(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;;; load
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))



(select :artist "Dixie Chicks")

;; macro demo
(defmacro backwards (expr) (reverse expr))
;; running (backwards ("hello world" t format)) ;; (which would normally cause an error) returns this
;; "hello world"
;; it evaluates the argument and macro all at once.

(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))

;; output
;; CL-USER> (make-comparison-expr :rating 10)
;; (EQUAL (GETF CD :RATING) 10)
;; CL-USER> (make-comparison-expr :title "Give Us a Break")
;; (EQUAL (GETF CD :TITLE) "Give Us a Break")

;; you can also use a back quote, it also stops evaluation like a forward quote
;; However, in a back-quoted expression, any subexpression that's preceded by a comma is evaluated. Notice the effect of the comma in the second expression:

;; `(1 2 (+ 1 2))        ==> (1 2 (+ 1 2))
;; `(1 2 ,(+ 1 2))       ==> (1 2 3)

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
	collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))

;; You can see the difference between , and ,@ in the following two expressions:

;; `(and ,(list 1 2 3))   ==> (AND (1 2 3))
;; `(and ,@(list 1 2 3))  ==> (AND 1 2 3)
;; You can also use ,@ to splice into the middle of a list.

;; `(and ,@(list 1 2 3) 4) ==> (AND 1 2 3 4)
