;;;;
;;;; Version 1.00
;;;;
;;;; Featuring:
;;;;
;;;;	* True subroutines via kom-queues
;;;;	* Improved error handling
;;;;	* Improved speed
;;;;	* Reduced bug/code-size ratio
;;;;
;;;; (None of the above is yet implemented).
;;;;

;;;; As now, this file only contains some experimental definitions.

(defmacro lyskom-check-error (object error-handlers &rest body)
  "Check wether a service returned successfully or not.
Args: OBJECT ERROR-HANDLERS &rest BODY
OBJECT is the result from the call.
ERROR-HANDLERS is a list of e-lists, similar to a cond-clause.
An e-list is a list whose car is either an error-number or a
list of error-numbers. If OBJECT is the result of an error, and
the number is present on (car e-list), (cdr e-list) is executed as a progn.
If car of the last e-list is t, cdr of that e-list will be executed
for any error not listed.
BODY is executed if OBJECT doesn't represent an error."
  (list 'let (list (list '__object__ object))
	(list 'if '(eq (car-safe __object__ 'err-info))
	      (list 'let '((__err__ (err-info->errno __object__)))
		    (list 'cond
			  (mapcar 'lyskom-check-error-calc-clauses
				  error-handlers)))
	      body)))

(defun lyskom-check-error-calc-clauses (clause)
  "Compile a cond-clause for lyskom-check-error.
This function is used when the macro lyskom-check-error is evaluated."
  (cond
   ((eq (car clause) t)
    clause)
   ((consp (car clause))
    (cons (cons 'or (mapcar (function (lambda (x) (list 'eq '__err__ x)))
			    (car clause)))
	  (cdr clause)))
   (t
    (cons (list 'eq '__err__ (car clause)) (cdr clause)))))
	 