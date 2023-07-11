;; Before you can implement this API in a library that will run correctly on multiple Common Lisp implementations, I need to show you the mechanism for writing implementation-specific code.

;; While most of the code you write can be "portable" in the sense that it will run the same on any conforming Common Lisp implementation, you may occasionally need to rely on implementation-specific functionality or to write slightly different bits of code for different implementations. To allow you to do so without totally destroying the portability of your code, Common Lisp provides a mechanism, called read-time conditionalization, that allows you to conditionally include code based on various features such as what implementation it's being run in.

;; The mechanism consists of a variable *FEATURES* and two extra bits of syntax understood by the Lisp reader. *FEATURES* is a list of symbols; each symbol represents a "feature" that's present in the implementation or on the underlying platform. These symbols are then used in feature expressions that evaluate to true or false depending on whether the symbols in the expression are present in *FEATURES*. The simplest feature expression is a single symbol; the expression is true if the symbol is in *FEATURES* and false if it isn't. Other feature expressions are boolean expressions built out of NOT, AND, and OR operators. For instance, if you wanted to conditionalize some code to be included only if the features foo and bar were present, you could write the feature expression (and foo bar).

;; The reader uses feature expressions in conjunction with two bits of syntax, #+ and #-. When the reader sees either of these bits of syntax, it first reads a feature expression and then evaluates it as I just described. When a feature expression following a #+ is true, the reader reads the next expression normally. Otherwise it skips the next expression, treating it as whitespace. #- works the same way except it reads the form if the feature expression is false and skips it if it's true.

;; The initial value of *FEATURES* is implementation dependent, and what functionality is implied by the presence of any given symbol is likewise defined by the implementation. However, all implementations include at least one symbol that indicates what implementation it is. For instance, Allegro Common Lisp includes the symbol :allegro, CLISP includes :clisp, SBCL includes :sbcl, and CMUCL includes :cmu. To avoid dependencies on packages that may or may not exist in different implementations, the symbols in *FEATURES* are usually keywords, and the reader binds *PACKAGE* to the KEYWORD package while reading feature expressions. Thus, a name with no package qualification will be read as a keyword symbol. So, you could write a function that behaves slightly differently in each of the implementations just mentioned like this:


(defun foo ()
  #+allegro (do-one-thing)
  #+sbcl (do-another-thing)
  #+clisp (something-else)
  #+cmu (yet-another-version)
  #-(or allegro sbcl clisp cmu) (error "Not implemented"))

;; In Allegro that code will be read as if it had been written like this:

;; (defun foo ()
;;   (do-one-thing))
;; while in SBCL the reader will read this:

;; (defun foo ()
;;   (do-another-thing))
;; while in an implementation other than one of the ones specifically conditionalized, it will read this:

;; (defun foo ()
;;   (error "Not implemented"))

(directory (make-pathname :name :wild :type :wild :defaults "/home/bbrodrick/"))

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-pp (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-pp name))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
				(list (file-namestring pathname)))
			    :name nil
			    :type nil
			    :defaults pathname)
	 pathname)))
