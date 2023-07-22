;; The key to understanding lists is to understand that they're largely an illusion built on top of objects that are instances of a more primitive data type. Those simpler objects are pairs of values called cons cells, after the function CONS used to create them.

;; CONS takes two arguments and returns a new cons cell containing the two values.2 These values can be references to any kind of object. Unless the second value is NIL or another cons cell, a cons is printed as the two values in parentheses separated by a dot, a so-called dotted pair.

(cons 1 2);; ==> (1 . 2)

;; The two values in a cons cell are called the CAR and the CDR after the names of the functions used to access them. At the dawn of time, these names were mnemonic, at least to the folks implementing the first Lisp on an IBM 704. But even then they were just lifted from the assembly mnemonics used to implement the operations. However, it's not all bad that these names are somewhat meaningless--when considering individual cons cells, it's best to think of them simply as an arbitrary pair of values without any particular semantics. Thus:

(car (cons 1 2));; ==> 1
(cdr (cons 1 2));; ==> 2

;; Both CAR and CDR are also SETFable places--given an existing cons cell, it's possible to assign a new value to either of its values.

(defparameter *cons* (cons 1 2))
*cons*;;                 ==> (1 . 2)
(setf (car *cons*) 10);; ==> 10
*cons*;;                 ==> (10 . 2)
(setf (cdr *cons*) 20);; ==> 20
*cons*;;                 ==> (10 . 20)

;; Because the values in a cons cell can be references to any kind of object, you can build larger structures out of cons cells by linking them together. Lists are built by linking together cons cells in a chain. The elements of the list are held in the CARs of the cons cells while the links to subsequent cons cells are held in the CDRs. The last cell in the chain has a CDR of NIL, which--as I mentioned in Chapter 4--represents the empty list as well as the boolean value false.

;; This arrangement is by no means unique to Lisp; it's called a singly linked list. However, few languages outside the Lisp family provide such extensive support for this humble data type.

;; So when I say a particular value is a list, what I really mean is it's either NIL or a reference to a cons cell. The CAR of the cons cell is the first item of the list, and the CDR is a reference to another list, that is, another cons cell or NIL, containing the remaining elements. The Lisp printer understands this convention and prints such chains of cons cells as parenthesized lists rather than as dotted pairs.

(cons 1 nil)                   ;;==> (1)
(cons 1 (cons 2 nil))          ;;==> (1 2)
(cons 1 (cons 2 (cons 3 nil))) ;;==> (1 2 3)

;; When talking about structures built out of cons cells, a few diagrams can be a big help. Box-and-arrow diagrams represent cons cells as a pair of boxes like this:
 _ _
| | |
|_|_|

;; The box on the left represents the CAR, and the box on the right is the CDR. The values stored in a particular cons cell are either drawn in the appropriate box or represented by an arrow from the box to a representation of the referenced value.4 For instance, the list (1 2 3), which consists of three cons cells linked together by their CDRs, would be diagrammed like this:

;; However, most of the time you work with lists you won't have to deal with individual cons cells--the functions that create and manipulate lists take care of that for you. For example, the LIST function builds a cons cells under the covers for you and links them together; the following LIST expressions are equivalent to the previous CONS expressions:

(list 1)     ;;==> (1)
(list 1 2)   ;;==> (1 2)
(list 1 2 3) ;;==> (1 2 3)

;; Similarly, when you're thinking in terms of lists, you don't have to use the meaningless names CAR and CDR; FIRST and REST are synonyms for CAR and CDR that you should use when you're dealing with cons cells as lists.

(defparameter *list* (list 1 2 3 4))
(first *list*)        ;;==> 1
(rest *list*)         ;;==> (2 3 4)
(first (rest *list*)) ;;==> 2

;; Because cons cells can hold any kind of values, so can lists. And a single list can hold objects of different types.

(list "foo" (list 1 2) 10) ;;==> ("foo" (1 2) 10)

;; destructive operations aka non-functional

(defparameter *list-1* (list 1 2))
(defparameter *list-2* (list 3 4))
(defparameter *list-3* (append *list-1* *list-2*))

;; After evaluating these forms, you have three lists, but *list-3* and *list-2* share structure just like the lists in the previous diagram.

*list-1*                  ;;==> (1 2)
*list-2*                  ;;==> (3 4)
*list-3*                  ;;==> (1 2 3 4)

;; Now consider what happens when you modify *list-2*.

(setf (first *list-2*) 0) ;; ==> 0
*list-2*                  ;; ==> (0 4)     ; as expected
*list-3*                  ;; ==> (1 2 0 4) ; maybe not what you wanted


;; | Function  | Description                                                                                                                                                                              |
;; |-----------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
;; | LAST      | Returns the last cons cell in a list. With an integer, argument returns the last n cons cells.                                                                                           |
;; | BUTLAST   | Returns a copy of the list, excluding the last cons cell. With an integer argument, excludes the last n cells.                                                                           |
;; | NBUTLAST  | The recycling version of BUTLAST; may modify and return the argument list but has no reliable side effects.                                                                              |
;; | LDIFF     | Returns a copy of a list up to a given cons cell.                                                                                                                                        |
;; | TAILP     | Returns true if a given object is a cons cell that's part of the structure of a list.                                                                                                    |
;; | LIST      | *	Builds a list to hold all but the last of its arguments and then makes the last argument the CDR of the last cell in the list. In other words, a cross between LIST and APPEND. |
;; | MAKE      | -LIST	Builds an n item list. The initial elements of the list are NIL or the value specified with the :initial-element keyword argument.                                          |
;; | REVAPPEND | Combination of REVERSE and APPEND; reverses first argument as with REVERSE and then appends the second argument.                                                                         |
;; | NRECONC   | Recycling version of REVAPPEND; reverses first argument as if by NREVERSE and then appends the second argument. No reliable side effects.                                                |
;; | CONSP     | Predicate to test whether an object is a cons cell.                                                                                                                                      |
;; | ATOM      | Predicate to test whether an object is not a cons cell.                                                                                                                                  |
;; | LISTP     | Predicate to test whether an object is either a cons cell or NIL.                                                                                                                        |
;; | NULL      | Predicate to test whether an object is NIL. Functionally equivalent to NOT but stylistically preferable when testing for an empty list as opposed to boolean false.                      |

;; mapcar like map but no return type needed, allways returns list
(mapcar #'(lambda (x) (* 2 x)) (list 1 2 3)) ;;==> (2 4 6)
(mapcar #'+ (list 1 2 3) (list 10 20 30)) ;;==> (11 22 33)

