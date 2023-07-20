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

