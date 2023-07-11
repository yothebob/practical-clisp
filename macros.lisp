(dotimes (i 10)
  (print i))

(dolist (i (list 1 2 3 4))
  (return i))

;;; loops 
(loop
  (when (> (get-universal-time) *some-future-date*)
    (return))
  (format t "Waiting~%")
  (sleep 60))

;;; do loop
(do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))

;;; same function but with loop
  (loop for i from 1 to 10 collecting i)

;;; loop examples
(loop for x from 1 to 10 summing (expt x 2)) ;; ==> 385

;; eleventh Fibonacci number
(loop for i below 10
      and a = 0 then b
      and b = 1 then (+ b a)
      finally (return  a))


;;; defmacro

;; basic defmacro skeleton
  (defmacro name (parameter*)
    "doc string."
    body-form*)


;;; writing macro steps
 ;; 1. write a simple call to the macro and the code it should expand into. or vice versa.
 ;; 2. write code that generates the handwritten expansion from the arguments in the sample call.
 ;; 3. make sure the macro abstraction doesn't "Leak".


 ;; making a simple macro do-primes

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

  (defmacro do-primes (var-and-range &rest body)
    (let ((var (first var-and-range))
	  (start (second var-and-range))
	  (end (third var-and-range)))
	  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
	       ((> ,var ,end))
	     ,@body)))

  (do-primes (p 0 19) (print p))

  ;; streamlined and to lisp standards
  (defmacro do-primes ((var start end) &body body)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var ))))
	 ((> ,var ,end))
	 ,@body))
  
  (do-primes (p 0 19) (print p))

;; For simple macros like do-primes, the special backquote syntax is perfect. To review, a backquoted expression is similar to a quoted expression except you can "unquote" particular subexpressions by preceding them with a comma, possibly followed by an at (@) sign. Without an at sign, the comma causes the value of the subexpression to be included as is. With an at sign, the value--which must be a list--is "spliced" into the enclosing list.

;; Another useful way to think about the backquote syntax is as a particularly concise way of writing code that generates lists. This way of thinking about it has the benefit of being pretty much exactly what's happening under the covers--when the reader reads a backquoted expression, it translates it into code that generates the appropriate list structure. For instance,
;;; `(,a b) might be read as (list a 'b).

;; | backquote syntax     | equivalent list-building code            | res           |
;; |----------------------+------------------------------------------+---------------|
;; | `(a (+ 1 2) c)       | (list 'a '(+ 1 2) 'c)                    | (a (+ 1 2) c) |
;; | `(a ,(+ 1 2) c)      | (list 'a (+ 1 2) 'c)                     | (a 3 c)       |
;; | `(a (list 1 2) c)    | (list 'a '(list 1 2) 'c)                 | (a (1 2) c)   |
;; | `(a  ,@(list 1 2) c) | (append (list 'a) (list 1 2 ) (list 'c)) | (a 1 2 c)     |

  ;; you can check macro directly by looking at the expansion of a particular call. The function MACROEXPAND-1 takes any Lisp expression as an argument and returns the result of doing one level of macro expansion.
  ;; Because MACROEXPAND-1 is a function, to pass it a literal macro form you must quote it. You can use it to see the expansion of the previous call.

  (macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))
;; in emacs you can C-c RET and it will print the res in a seperate buffer.
  
 

  
  (print "hello")
  (lambda ()
