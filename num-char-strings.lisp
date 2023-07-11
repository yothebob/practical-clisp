

;; (= 1 1)                        ==> T
;; (= 10 20/2)                    ==> T
;; (= 1 1.0 #c(1.0 0.0) #c(1 0))  ==> T
;; The /= function, conversely, returns true only if all its arguments are different values.

;; (/= 1 1)        ==> NIL
;; (/= 1 2)        ==> T
;; (/= 1 2 3)      ==> T
;; (/= 1 2 3 1)    ==> NIL
;; (/= 1 2 3 1.0)  ==> NIL
;; The functions <, >, <=, and >= order rationals and floating-point numbers (in other words, the real numbers.) Like = and /=, these functions can be called with more than two arguments, in which case each argument is compared to the argument to its right.

;; (< 2 3)       ==> T
;; (> 2 3)       ==> NIL
;; (> 3 2)       ==> T
;; (< 2 3 4)     ==> T
;; (< 2 3 3)     ==> NIL
;; (<= 2 3 3)    ==> T
;; (<= 2 3 3 4)  ==> T
;; (<= 2 3 4 3)  ==> NIL
;; To pick out the smallest or largest of several numbers, you can use the function MIN or MAX, which takes any number of real number arguments and returns the minimum or maximum value.

;; (max 10 11)    ==> 11
;; (min -12 -10)  ==> -12
;; (max -1 2 -3)  ==> 2


;; | numeric analog | Case-sensitive | Case-insensitive  |
;; |----------------+----------------+-------------------|
;; | =              | CHAR=          | CHAR-EQUAL        |
;; | /=             | CHAR/=         | CHAR-NOT-EQUAL    |
;; | <              | CHAR<          | CHAR-LESSP        |
;; | >              | CHAR>          | CHAR-GREATERP     |
;; | <=             | CHAR<=         | CHAR-NOT-GREATERP |
;; | >=             | CHAR>=         | CHAR-NOT-LESSP    |


;; | numeric analog | Case-sensitive | Case-insensitive    |
;; |----------------+----------------+---------------------|
;; | =              | STRING=        | STRING-EQUAL        |
;; | /=             | STRING/=       | STRING-NOT-EQUAL    |
;; | <              | STRING<        | STRING-LESSP        |
;; | >              | STRING>        | STRING-GREATERP     |
;; | <=             | STRING<=       | STRING-NOT-GREATERP |
;; | >=             | STRING>=       | STRING-NOT-LESSP    |


(string= "foobarbaz" "quuxbarfoo" :start1 3 :end1 6 :start2 4 :end2 7)

;; The comparators that return true when their arguments differ--that is, all of them except STRING= and STRING-EQUAL--return the index in the first string where the mismatch was detected.

(string/= "lisp" "lissome") ;;==> 3

;; If the first string is a prefix of the second, the return value will be the length of the first string, that is, one greater than the largest valid index into the string.

(string< "lisp" "lisper") ;; ==> 4
