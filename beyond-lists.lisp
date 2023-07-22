;; Where a cons cell in the original referenced an atomic value, the corresponding cons cell in the copy will reference the same value. Thus, the only objects referenced in common by the original tree and the copy produced by COPY-TREE are the numbers 1-6, and the symbol NIL.

;; Another function that walks both the CARs and the CDRs of a tree of cons cells is TREE-EQUAL, which compares two trees, considering them equal if the tree structure is the same shape and if the leaves are EQL (or if they satisfy the test supplied with the :test keyword argument).

;; Some other tree-centric functions are the tree analogs to the SUBSTITUTE and NSUBSTITUTE sequence functions and their -IF and -IF-NOT variants. The function SUBST, like SUBSTITUTE, takes a new item, an old item, and a tree (as opposed to a sequence), along with :key and :test keyword arguments, and it returns a new tree with the same shape as the original tree but with all instances of the old item replaced with the new item. For example:

(subst 10 1 '(1 2 (3 2 1) ((1 1) (2 2)))) ;;==> (10 2 (3 2 10) ((10 10) (2 2)))

;;sets
;; CL-USER> (defparameter *set* ())
;; *SET*
;; CL-USER> (adjoin 1 *set*)
;; (1)
;; CL-USER> *set*
;; NIL
;; CL-USER> (setf *set* (adjoin 1 *set*))
;; (1)
;; CL-USER> (pushnew 2 *set*)
;; (2 1)
;; CL-USER> *set*
;; (2 1)
;; CL-USER> (pushnew 2 *set*)
;; (2 1)

;; You can test whether a given item is in a set with MEMBER and the related functions MEMBER-IF and MEMBER-IF-NOT. These functions are similar to the sequence functions FIND, FIND-IF, and FIND-IF-NOT except they can be used only with lists. And instead of returning the item when it's present, they return the cons cell containing the item--in other words, the sublist starting with the desired item. When the desired item isn't present in the list, all three functions return NIL.

;; The remaining set-theoretic functions provide bulk operations: INTERSECTION, UNION, SET-DIFFERENCE, and SET-EXCLUSIVE-OR. Each of these functions takes two lists and :key and :test keyword arguments and returns a new list representing the set resulting from performing the appropriate set-theoretic operation on the two lists: INTERSECTION returns a list containing all the elements found in both arguments. UNION returns a list containing one instance of each unique element from the two arguments.3 SET-DIFFERENCE returns a list containing all the elements from the first argument that don't appear in the second argument. And SET-EXCLUSIVE-OR returns a list containing those elements appearing in only one or the other of the two argument lists but not in both. Each of these functions also has a recycling counterpart whose name is the same except with an N prefix.

;; Finally, the function SUBSETP takes two lists and the usual :key and :test keyword arguments and returns true if the first list is a subset of the second--if every element in the first list is also present in the second list. The order of the elements in the lists doesn't matter.

;; CL-USER> (subsetp '(3 2 1) '(1 2 3 4))
;; T
;; CL-USER> (subsetp '(1 2 3 4) '(3 2 1))
;; NIL


;; Alists and Plists

(assoc 'a '((a . 1) (b . 2) (c . 3))) ;;==> (A . 1)
(assoc 'c '((a . 1) (b . 2) (c . 3))) ;;==> (C . 3)
(assoc 'd '((a . 1) (b . 2) (c . 3))) ;;==> NIL

(cdr (assoc 'a '((a . 1) (b . 2) (c . 3)))) ;;==> 1

;; By default the key given is compared to the keys in the alist using EQL, but you can change that with the standard combination of :key and :test keyword arguments. For instance, if you wanted to use string keys, you might write this:

;; CL-USER> (assoc "a" '(("a" . 1) ("b" . 2) ("c" . 3)) :test #'string=)
;; ("a" . 1)

;; Without specifying :test to be STRING=, that ASSOC would probably return NIL because two strings with the same contents aren't necessarily EQL.

;; CL-USER> (assoc "a" '(("a" . 1) ("b" . 2) ("c" . 3)))
;; NIL

;; Because ASSOC searches the list by scanning from the front of the list, one key/value pair in an alist can shadow other pairs with the same key later in the list.

;; CL-USER> (assoc 'a '((a . 10) (a . 1) (b . 2) (c . 3)))
;; (A . 10)

;; You can add a pair to the front of an alist with CONS like this:

;; (cons (cons 'new-key 'new-value) alist)

;; However, as a convenience, Common Lisp provides the function ACONS, which lets you write this:

;; (acons 'new-key 'new-value alist)

;; Like CONS, ACONS is a function and thus can't modify the place holding the alist it's passed. If you want to modify an alist, you need to write either this:

;; (setf alist (acons 'new-key 'new-value alist))
;; or this:

;; (push (cons 'new-key 'new-value) alist)

;; Obviously, the time it takes to search an alist with ASSOC is a function of how deep in the list the matching pair is found. In the worst case, determining that no pair matches requires ASSOC to scan every element of the alist. However, since the basic mechanism for alists is so lightweight, for small tables an alist can outperform a hash table. Also, alists give you more flexibility in how you do the lookup. I already mentioned that ASSOC takes :key and :test keyword arguments. When those don't suit your needs, you may be able to use the ASSOC-IF and ASSOC-IF-NOT functions, which return the first key/value pair whose CAR satisfies (or not, in the case of ASSOC-IF-NOT) the test function passed in the place of a specific item. And three functions--RASSOC, RASSOC-IF, and RASSOC-IF-NOT--work just like the corresponding ASSOC functions except they use the value in the CDR of each element as the key, performing a reverse lookup.

;; The function COPY-ALIST is similar to COPY-TREE except, instead of copying the whole tree structure, it copies only the cons cells that make up the list structure, plus the cons cells directly referenced from the CARs of those cells. In other words, the original alist and the copy will both contain the same objects as the keys and values, even if those keys or values happen to be made up of cons cells.

;; Finally, you can build an alist from two separate lists of keys and values with the function PAIRLIS. The resulting alist may contain the pairs either in the same order as the original lists or in reverse order. For example, you may get this result:

;; CL-USER> (pairlis '(a b c) '(1 2 3))
;; ((C . 3) (B . 2) (A . 1))
;; Or you could just as well get this:

;; CL-USER> (pairlis '(a b c) '(1 2 3))
;; ((A . 1) (B . 2) (C . 3))
;; The other kind of lookup table is the property list, or plist, which you used to represent the rows in the database in Chapter 3. Structurally a plist is just a regular list with the keys and values as alternating values. For instance, a plist mapping A, B, and C, to 1, 2, and 3 is simply the list (A 1 B 2 C 3). In boxes-and-arrows form, it looks like this:



;; However, plists are less flexible than alists. In fact, plists support only one fundamental lookup operation, the function GETF, which takes a plist and a key and returns the associated value or NIL if the key isn't found. GETF also takes an optional third argument, which will be returned in place of NIL if the key isn't found.

;; Unlike ASSOC, which uses EQL as its default test and allows a different test function to be supplied with a :test argument, GETF always uses EQ to test whether the provided key matches the keys in the plist. Consequently, you should never use numbers or characters as keys in a plist; as you saw in Chapter 4, the behavior of EQ for those types is essentially undefined. Practically speaking, the keys in a plist are almost always symbols, which makes sense since plists were first invented to implement symbolic "properties," arbitrary mappings between names and values.

;; You can use SETF with GETF to set the value associated with a given key. SETF also treats GETF a bit specially in that the first argument to GETF is treated as the place to modify. Thus, you can use SETF of GETF to add a new key/value pair to an existing plist.

;; CL-USER> (defparameter *plist* ())
;; *PLIST*
;; CL-USER> *plist*
;; NIL
;; CL-USER> (setf (getf *plist* :a) 1)
;; 1
;; CL-USER> *plist*
;; (:A 1)
;; CL-USER> (setf (getf *plist* :a) 2)
;; 2
;; CL-USER> *plist*
;; (:A 2)
;; To remove a key/value pair from a plist, you use the macro REMF, which sets the place given as its first argument to a plist containing all the key/value pairs except the one specified. It returns true if the given key was actually found.

;; CL-USER> (remf *plist* :a)
;; T
;; CL-USER> *plist*
;; NIL
;; Like GETF, REMF always uses EQ to compare the given key to the keys in the plist.

;; Since plists are often used in situations where you want to extract several properties from the same plist, Common Lisp provides a function, GET-PROPERTIES, that makes it more efficient to extract multiple values from a single plist. It takes a plist and a list of keys to search for and returns, as multiple values, the first key found, the corresponding value, and the head of the list starting with the found key. This allows you to process a property list, extracting the desired properties, without continually rescanning from the front of the list. For instance, the following function efficiently processes--using the hypothetical function process-property--all the key/value pairs in a plist for a given list of keys:

;; (defun process-properties (plist keys)
;;   (loop while plist do
;;        (multiple-value-bind (key value tail) (get-properties plist keys)
;;          (when key (process-property key value))
;;          (setf plist (cddr tail)))))
;; The last special thing about plists is the relationship they have with symbols: every symbol object has an associated plist that can be used to store information about the symbol. The plist can be obtained via the function SYMBOL-PLIST. However, you rarely care about the whole plist; more often you'll use the functions GET, which takes a symbol and a key and is shorthand for a GETF of the same key in the symbols SYMBOL-PLIST.

;; (get 'symbol 'key) === (getf (symbol-plist 'symbol) 'key)
;; Like GETF, GET is SETFable, so you can attach arbitrary information to a symbol like this:

;; (setf (get 'some-symbol 'my-key) "information")
;; To remove a property from a symbol's plist, you can use either REMF of SYMBOL-PLIST or the convenience function REMPROP.4

;; (remprop 'symbol 'key) === (remf (symbol-plist 'symbol key))
;; Being able to attach arbitrary information to names is quite handy when doing any kind of symbolic programming. For instance, one of the macros you'll write in Chapter 24 will attach information to names that other instances of the same macros will extract and use when generating their expansions.

;; DESTRUCTURING-BIND
;; One last tool for slicing and dicing lists that I need to cover since you'll need it in later chapters is the DESTRUCTURING-BIND macro. This macro provides a way to destructure arbitrary lists, similar to the way macro parameter lists can take apart their argument list. The basic skeleton of a DESTRUCTURING-BIND is as follows:

;; (destructuring-bind (parameter*) list
;;   body-form*)
;; The parameter list can include any of the types of parameters supported in macro parameter lists such as &optional, &rest, and &key parameters.5 And, as in macro parameter lists, any parameter can be replaced with a nested destructuring parameter list, which takes apart the list that would otherwise have been bound to the replaced parameter. The list form is evaluated once and should return a list, which is then destructured and the appropriate values are bound to the variables in the parameter list. Then the body-forms are evaluated in order with those bindings in effect. Some simple examples follow:

;; (destructuring-bind (x y z) (list 1 2 3)
;;   (list :x x :y y :z z)) ==> (:X 1 :Y 2 :Z 3)

;; (destructuring-bind (x y z) (list 1 (list 2 20) 3)
;;   (list :x x :y y :z z)) ==> (:X 1 :Y (2 20) :Z 3)

;; (destructuring-bind (x (y1 y2) z) (list 1 (list 2 20) 3)
;;   (list :x x :y1 y1 :y2 y2 :z z)) ==> (:X 1 :Y1 2 :Y2 20 :Z 3)

;; (destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2 20) 3)
;;   (list :x x :y1 y1 :y2 y2 :z z)) ==> (:X 1 :Y1 2 :Y2 20 :Z 3)

;; (destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) 3)
;;   (list :x x :y1 y1 :y2 y2 :z z)) ==> (:X 1 :Y1 2 :Y2 NIL :Z 3)

;; (destructuring-bind (&key x y z) (list :x 1 :y 2 :z 3)
;;   (list :x x :y y :z z)) ==> (:X 1 :Y 2 :Z 3)

;; (destructuring-bind (&key x y z) (list :z 1 :y 2 :x 3)
;;   (list :x x :y y :z z)) ==> (:X 3 :Y 2 :Z 1)
;; One kind of parameter you can use with DESTRUCTURING-BIND and also in macro parameter lists, though I didn't mention it in Chapter 8, is a &whole parameter. If specified, it must be the first parameter in a parameter list, and it's bound to the whole list form.6 After a &whole parameter, other parameters can appear as usual and will extract specific parts of the list just as they would if the &whole parameter weren't there. An example of using &whole with DESTRUCTURING-BIND looks like this:

;; (destructuring-bind (&whole whole &key x y z) (list :z 1 :y 2 :x 3)
;;   (list :x x :y y :z z :whole whole))
;; ==> (:X 3 :Y 2 :Z 1 :WHOLE (:Z 1 :Y 2 :X 3))
;; You'll use a &whole parameter in one of the macros that's part of the HTML generation library you'll develop in Chapter 31. However, I have a few more topics to cover before you can get to that. After two chapters on the rather Lispy topic of cons cells, you can now turn to the more prosaic matter of how to deal with files and filenames.

