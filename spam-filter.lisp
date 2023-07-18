(defpackage :com.gigamonkeys.spam
  ;; (:use :common-lisp :com.gigamonkeys.pathnames)
  (:use :common-lisp :cl-ppcre))

;; (ql:quickload :cl-ppcre)

(defparameter *max-ham-score* .4)
(defparameter *min-ham-score* .6)

(defclass word-feature ()
  ((word
    :initarg :word
    :accessor word
    :initform (error "Must supply :word")
    :documentation "The word this feature represents.")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "Number of spams we have seen this feature in.")
   (ham-count
    :initarg :ham-count
    :accessor :ham-count
    :initform 0
    :documentation "Number of hams we have seen this feature in.")))

(defvar *feature-database* (make-hash-table :test #'equal))

(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal)))

(defun intern-feature (word)
  "Takes a WORD and returns the appropiate feature, creating if neccessary."
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
	    (make-instance 'word-feature :word word))))

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))


(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

;; (extract-features "this is a freekin test")

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))


(defun increment-count (feature type)
  (ecase ))

(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))


(defun classification (score)
  (cond
    ((<= score *max-ham-score* .4) 'ham)
    ((>= score *min-ham-score* .4) 'spam)
    (t 'unsure)))




(defun classify (text)
  (classification (score (extract-features text))))




 
