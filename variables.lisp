;; lisp has "local" or "global" variables

;; setf is kinda the standard
 ;; Then I'll discuss Common Lisp's general-purpose assignment operator, SETF, which is used to assign new values to variables and just about every other place that can hold a value.

;; Another binding form is a variant of LET, LET*. The difference is that in a LET, the variable names can be used only in the body of the LET--the part of the LET after the variables list--but in a LET*, the initial value forms for each variable can refer to variables introduced earlier in the variables list. Thus, you can write the following:

;; (let* ((x 10)
;;        (y (+ x 10)))
;;   (list x y))
;; but not this:

;; (let ((x 10)
;;       (y (+ x 10)))
;;   (list x y))
;; However, you could achieve the same result with nested LETs.

;; (let ((x 10))
;;   (let ((y (+ x 10)))
;;     (list x y)))

;; The key thing to understand about closures is that it's the binding, not the value of the variable, that's captured. Thus, a closure can not only access the value of the variables it closes over but can also assign new values that will persist between calls to the closure. For instance, you can capture the closure created by the previous expression in a global variable like this:

;; (defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))
;; Then each time you invoke it, the value of count will increase by one.

;; CL-USER> (funcall *fn*)
;; 1
;; CL-USER> (funcall *fn*)
;; 2
;; CL-USER> (funcall *fn*)
;; 3

;; Common Lisp provides two ways to create global variables: DEFVAR and DEFPARAMETER. Both forms take a variable name, an initial value, and an optional documentation string. After it has been DEFVARed or DEFPARAMETERed, the name can be used anywhere to refer to the current binding of the global variable. As you've seen in previous chapters, global variables are conventionally named with names that start and end with *. You'll see later in this section why it's quite important to follow that naming convention. Examples of DEFVAR and DEFPARAMETER look like this:

;; (defvar *count* 0
;;   "Count of widgets made so far.")

;; (defparameter *gap-tolerance* 0.001
;;   "Tolerance to be allowed in widget gaps.")
;; The difference between the two forms is that DEFPARAMETER always assigns the initial value to the named variable while DEFVAR does so only if the variable is undefined. A DEFVAR form can also be used with no initial value to define a global variable without giving it a value. Such a variable is said to be unbound.

;; SETF can also assign to multiple places in sequence. For instance, instead of the following:

;; (setf x 1)
;; (setf y 2)
;; you can write this:

;; (setf x 1 y 2)

;; while you could increment a number with SETF, like this:

;; (setf x (+ x 1))
;; or decrement it with this:

;; (setf x (- x 1))
;; it's a bit tedious, compared to the C-style ++x and --x. Instead, you can use the macros INCF and DECF, which increment and decrement a place by a certain amount that defaults to 1.

;; (incf x)    === (setf x (+ x 1))
;; (decf x)    === (setf x (- x 1))
;; (incf x 10) === (setf x (+ x 10))

;; The macro PUSH, which you used in the mini-database to add elements to the *db* variable, is another modify macro. You'll take a closer look at how it and its counterparts POP and PUSHNEW work in Chapter 12 when I talk about how lists are represented in Lisp.

;; Finally, two slightly esoteric but useful modify macros are ROTATEF and SHIFTF. ROTATEF rotates values between places. For instance, if you have two variables, a and b, this call:

;; (rotatef a b)
;; swaps the values of the two variables and returns NIL. Since a and b are variables and you don't have to worry about side effects, the previous ROTATEF expression is equivalent to this:

;; (let ((tmp a)) (setf a b b tmp) nil)
;; With other kinds of places, the equivalent expression using SETF would be quite a bit more complex.

;; SHIFTF is similar except instead of rotating values it shifts them to the left--the last argument provides a value that's moved to the second-to-last argument while the rest of the values are moved one to the left. The original value of the first argument is simply returned. Thus, the following:

;; (shiftf a b 10)
;; is equivalent--again, since you don't have to worry about side effects--to this:

;; (let ((tmp a)) (setf a b b 10) tmp)
;; Both ROTATEF and SHIFTF can be used with any number of arguments and, like all modify macros, are guaranteed to evaluate them exactly once, in left to right order.
