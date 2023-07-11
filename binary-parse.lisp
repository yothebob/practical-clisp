;;; manual way

(defun read-u2 (in)
  (+ (* (read-byte in) 256) (read-byte in)))

;;; Thus, you can extract the least significant octet of an integer like this
(ldb (byte 8 0) #xabcd) ;; ==> 205 ; 205 is #xcd
;; To get the next octet, you'd use a byte specified bits of an integer stored in a setf
(ldb (byte 8 8) #xabcd) ;; ==> 171 ; 171 is #xab

;; you can use ldb with setf to set the specified bits of an integer stored in a setf place.

(defvar *num* 0)
(setf (ldb (byte 8 0) *num*) 128)
(setf (ldb (byte 8 8) *num*) 255)

;;; better way
(defun read-u2 (in)
  (let ((u2 0))
    (setf (ldb (byte 8 8) u2) (read-byte in))
    (setf (ldb (byte 8 0) u2) (read-byte in))
    u2))

;;; to write 16-bit

(defun write-u2 (out value)
  (write-byte (ldb (byte 8 8) value) out)
  (write-byte (ldb (byte 8 0) value) out))

;; null terminated ASCII
(defconstant +null+ (code-char 0))

(defun read-null-termined-ascii (in)
  "read a STRING froma a IN bytes stream, finishing when you see the null char."
  (with-output-to-string (s)
    (loop for char = (code-char (read-byte in))
	  until (char= char +null+) do (write-char char s))))

(defun write-null-terminated-ascii (string out)
  "Write a STRING as bytes to a OUT stream, teminating the string with a null char."
  (loop for char across string
	do (write-byte (char-code char) out))
  (write-byte (char-code +null+) out))


