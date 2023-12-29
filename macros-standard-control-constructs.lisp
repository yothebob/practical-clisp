;; commonly used macros

;; when and unless

;; The lisp if allow if true do this otherwise do that. This is good but if's only allow one s-expression per side. so if you want to do one thing and then another thing if true, then it all needs to be wrapped in a progn.

;; (if t
;;     (progn
;;       (do-one-thing)
;;       (do-another-thing))
;;     (do-else))

;; This is where WHEN comes in:

;; (when t (do-one-thing) (do-another-thing))

;; But if it wasn't built into the standard library, you could define WHEN yourself with a macro such as this, using the backquote notation I discussed in Chapter 3:3

;; (defmacro when (condition &rest body)
;;   `(if ,condition (progn ,@body)))

;; The opposite of WHEN is UNLESS:

;; its defmacro looks like:

;; (defmacro unless (condition &rest body)
;;   `(if (not ,condition) (progn ,@body)))

;; COND is a vary useful macro, allowing nested conditional logic

;; Each element of the body represents one branch of the conditional and consists of a list containing a condition form and zero or more forms to be evaluated if that branch is chosen. The conditions are evaluated in the order the branches appear in the body until one of them evaluates to true. At that point, the remaining forms in that branch are evaluated, and the value of the last form in the branch is returned as the value of the COND as a whole. If the branch contains no forms after the condition, the value of the condition is returned instead. By convention, the branch representing the final else clause in an if/else-if chain is written with a condition of T. Any non-NIL value will work, but a T serves as a useful landmark when reading the code. Thus, you can write the previous nested IF expression using COND like this:

;; (cond (a (do-x))
;;       (b (do-y))
;;       (t (do-z)))

;; AND, OR, NOT

