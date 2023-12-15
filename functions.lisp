
(+ 1 2)
(quote (+ 1 2)) ; these are the same
'(+ 1 2) ; these are the same


(defun test-params (a &optional b c)
  (list a b c))

(test-params 1)
(test-params 1 2 3)
(test-params 1 4)

(defun test-params-preset (a &optional b (c 10))
  (list a b c))

(test-params-preset 1)
(test-params-preset 1 2)
(test-params-preset 1 2 3)

(defun make-rectangle (width &optional (height width))
  (list width height))

(make-rectangle 1)
(make-rectangle 1 5)

(defun test-param-supplied (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

(test-param-supplied 1 2)
(test-param-supplied 1 2 3)

(defun test-rest (&rest values)
  (list values))

(test-rest)
(test-rest 1)
(test-rest 1 2 3 4 5)

(defun test-params (a &key b c)
  (list a b c))

(test-params 1)
(test-params 1 :c 3)
(test-params 1 4) ;; will not work, b & c need keys

(defun test-additional-keys (&key (a 0) (b 0 b-supplied-p))
  (list a b b-supplied-p))

(test-additional-keys :a 0)
(test-additional-keys :b 1)
(test-additional-keys :a 1 :b 2)

(defun test-additional-keys-alias (&key ((:apple a) 0) ((:bravo b) 0 b-supplied-p))
  (list a b b-supplied-p))

(test-additional-keys-alias :apple 1)
(test-additional-keys-alias :bravo 1)

(defun test-rest-and-key (&rest rest &key a b c)
  (list rest a b c))

(test-rest-and-key :a 1)

;;; mixing different params needs to be in order
(defun param-them-all (a &optional b &key z)
  (list a b z))

(param-them-all 1 2 :z 2) ;;; works
(param-them-all 1 :z 2 2) ;;; does not work

;;; return mid function 
(defun testing-mid-return (a &key (b 0 b-supplied-p))
  (if (equal b-supplied-p t)
      (return-from testing-mid-return "b was supplied"))
  (list a b b-supplied-p))

(testing-mid-return 1)
(testing-mid-return 1 :b 2)

;;; functions as data aka higher order functions
(defun foo (x) (* x 2))
(function foo) ;;; same as #'
#'foo

(funcall #'foo 2) ;;; === (foo 2)

;;; applicable use of funcall
(defun plot (fn min max step)
  (loop for i from min to max by step do
    (loop repeat (funcall fn i) do (format t "*"))
    (format t "~%")))

(plot #'exp 0 4 1/2)

;;; if the numbers are not known you could do something like...
(plot (first plot-data) (second plot-data) (third plot-data) (forth plot-data))

;;; but that is unnessary, use apply
(apply #'plot plot-data)

;;; you can also do
(apply #'plot #'exp plot-data)
