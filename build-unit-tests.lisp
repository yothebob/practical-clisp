(defun test-+ ()
  (= (+ 1 2)))
(test-+)

(defun test-+ ()
  (and (= (+ 1 2))
       (= (+ 1 2))
       ))

(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  )

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6)))

(defmacro check (form)
  `(report-result ,form `,form))

(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6)))

;; even better

(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))

;; This definition uses a common macro idiom of wrapping a PROGN around a series of forms in order to turn them into a single form. Notice also how you can use ,@ to splice in the result of an expression that returns a list of expressions that are themselves generated with a backquote template.

;; now you can

(defun test-+ ()
  (check
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)))


(defmacro combine-results (&body forms)
  (with-gensyms (result)
  `(let ((,result t))
     ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
,result)))

;; now update check

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))


;; better result reporting
(defun test-*()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(defun test-arithmetic()
  (combine-results
    (test-+)
    (test-*)))

;;; use dynamic variable to report what check failed

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
  (let ((*test-name* (append *test-name* (list ',name))))
    ,@body)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loopfor f in forms collect `(report-result ,f `,f))))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(deftest test-arithmetic ()
    (combine-results
      (test-*)
      (test-+)))

(deftest test-math () (test-arithmetic))

(test-math)
