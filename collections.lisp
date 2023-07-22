;; vectors
(vector 1 2 3)
#(1 2 3)

;; but you should use these on ones you plan to modify
(vector 1 2 3)

;; make-array
;; make-array needs size and a list
(make-array 5 :initial-element nil)

;; if you want to make-array with a resizable vector use
(make-array 5 :fill-pointer 0)

;; to add element to end of resizable vector push vector push
;; the func vector-pop returns most recently added item, decrementing the fill pointer 
(defparameter *x* (make-array 5 :fill-pointer 0));; 5 is max length
(vector-push 'a *x*)
(vector-push 'b *x*)
(vector-push 'c *x*)
(vector-pop *x*)
(vector-pop *x*)
(vector-pop *x*)

;; to make an array that can add more then the starting amount of elements, you can use :adjustable
(make-array 5 :fill-pointer 0 :adjustable t)

;; with an adjustable vector, use vector-push-extend
(vector-push-extend 'a *x*)

;; to make a string type vector
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character) ;; => ""

;; bit vectors (read/print syntax #*00001111) 
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'bit) ;; => #*

;; more vector funcs
(defparameter *x* (vector 1 2 3))

(length *x*)
(elt *x* 0)
(elt *x* 1)
(elt *x* 2)
(elt *x* 3) ;; error

elt can also be used to update a vector index value

(setf (elt *x* 0) 9) ;; does not work with setq

;; | name       | required argmuents          | returns                                                |
;; |------------+-----------------------------+--------------------------------------------------------|
;; | count      | item and sequence           | number of times item appears in sequence               |
;; | find       | item and sequence           | item or nil                                            |
;; | position   | item and sequence           | index into sequence or nil                             |
;; | remove     | item and sequence           | sequence with instances of item removed                |
;; | substitute | new item, item and sequence | sequence with instances of item replaced with new item |

(count 1 #(1 2 3 1 2))
(remove 1 #(1 2 3 1 2))
(remove 1 '(1 2 3 1 2))
(remove #\a "foobarbaz")
(substitute 10 1 #(1 2 3 1 2))
(substitute 10 1 '(1 2 3 1 2))
(substitute #\x #\b "foobarbaz")
(find 1 #(1 2 3 2 1))
(find 10 #(1 2 3 2 1))
(position 1 #(1 2 3 2 1))
(position 5 #(1 2 3 2 1))

;; with these functions you can modify their outputs
;; you can use :test if provided it will compare item to each element overriding the equality test
;; kinda like a map or apply
(count "foo" #("foo" "bar" "baz") :test #'string=) 
(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first) ;; this one still returns the same thing

;; you can provide bounding indices :start :end , nil or not used will do 0 to index length
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first)
(find 'b #((a 10) (b 20) (a 30) (b 40)) :key #'first :end 1)
(find 'b #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t)

(remove #\a "foobarbaz" :count 1)
(remove #\a "foobarbaz" :count 1 :from-end t)

(defparameter *v* #((a 10) (b 20) (a 30) (b 40)))
(defun verbose-first (x) (format t "Looking at ~s~%" x) (first x))
(count 'a *v* :key #'verbose-first)
(count 'a *v* :key #'verbose-first :from-end t)

;; | argument  | meaning                                                                                                               | default |
;; |-----------+-----------------------------------------------------------------------------------------------------------------------+---------|
;; | :test     | two argument function used to compare item (or value extracted by :key function )                                     | eql     |
;; | :key      | One argument function to extract key value from acual sequence element. nil means use element as is.                  | nil     |
;; | :start    | Starting index (inclusive) of subsequence.                                                                            | 0       |
;; | :end      | Ending index (exclusive) of subsequence. Nil indicated end of sequence                                                | nil     |
;; | :from-end | if true, the seqauence will be traversed in reverse order from end to start.                                          | nil     |
;; | :count    | Number indicating ther number of elements to remove or substitute or nil to indicate all (REMOVE and SUBSTITUTE only) | nil     |


(count-if #'evenp #(1 2 3 4 5))
(count-if-not #'evenp #(1 2 3 4 5))

(position-if #'digit-char-p "abcd0001")

(remove-if-not #'(lambda (x) (char= (elt x 1) #\o))
	       #("foo" "bar" "baz" "foom"))

(concatenate 'vector #(1 2 3) #(4 5 6))
(concatenate 'list #(1 2 3) #(4 5 6))
(concatenate 'string "abc" #(#\d #\e #\f))

(sort (vector "foo" "bar" "baz") #'string<)


;; stable-sort is garenteed to not sort the argument, just return a sorted copy
(stable-sort (vector "foo" "bar" "baz") #'string<) 

;; so you need override your variable if you want to sort it. like so
(setf my-sequence (sort my-sequence #'string<))

;; merge and sort
(merge 'vector #(1 3 5) #(2 4 6) #'<) 
(merge 'list #(1 3 5) #(2 4 6) #'<)

;; subseq extracts a subsequence starting at an index and continuing to an end index or the end
(subseq "foobarbaz" 3)
(subseq "foobarbaz" 3 6)

(defparameter *x* (copy-seq "foobarbaz"))

(setf (subseq *x* 3 6) "xxx")  ; subsequence and new value are same length
;; *x* ==> "fooxxxbaz"

(setf (subseq *x* 3 6) "abcd") ; new value too long, extra character ignored.
;; *x* ==> "fooabcbaz"

(setf (subseq *x* 3 6) "xx")   ; new value too short, only two characters changed
;; *x* ==> "fooxxcbaz"

(position #\b "foobarbaz")
(search "bar" "foobarbaz")

(mismatch "foobarbaz" "foom")

(mismatch "foozbar" "bar" :from-end t)

(every #'evenp #(1 2 3 4 5));;    ==> NIL
(some #'evenp #(1 2 3 4 5));;     ==> T
(notany #'evenp #(1 2 3 4 5));;   ==> NIL
(notevery #'evenp #(1 2 3 4 5));; ==> T

;; These calls compare elements of two sequences pairwise:

(every #'> #(1 2 3 4) #(5 4 3 2));;    ==> NIL
(some #'> #(1 2 3 4) #(5 4 3 2));;     ==> T
(notany #'> #(1 2 3 4) #(5 4 3 2));;   ==> NIL
(notevery #'> #(1 2 3 4) #(5 4 3 2));; ==> T

(map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6)) ;; ==> #(10 18 24 28 30)

;;map-into is like map but puts the result into the passed argument vrs returning a new sequence
(map-into *x* #'+ a b c)

(reduce #'+ #(1 2 3 4 5 6 7 8 9 10));; ==> 55

;; hash tables
(defparameter *h* (make-hash-table))

(gethash 'foo *h*);; ==> NIL

(setf (gethash 'foo *h*) 'quux)

(gethash 'foo *h*);; ==> QUUX


;; I'll discuss multiple return values in greater detail in Chapter 20, but for now I'll give you a sneak preview of how to use the MULTIPLE-VALUE-BIND macro to take advantage of GETHASH's extra return value. MULTIPLE-VALUE-BIND creates variable bindings like LET does, filling them with the multiple values returned by a form.

;; The following function shows how you might use MULTIPLE-VALUE-BIND; the variables it binds are value and present:

(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
      (format nil "Value ~a actually present." value)
      (format nil "Value ~a because key not found." value))))

(setf (gethash 'bar *h*) nil) ; provide an explicit value of NIL

(show-value 'foo *h*);; ==> "Value QUUX actually present."
(show-value 'bar *h*);; ==> "Value NIL actually present."
(show-value 'baz *h*);; ==> "Value NIL because key not found."

;; hash table iteration
(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *h*)

(loop for k being the hash-keys in *h* using (hash-value v)
  do (format t "~a => ~a~%" k v))







