;;;;;
;;;;; $Id: lyskom-1.00.el,v 38.0 1994-01-06 01:48:50 linus Exp $
;;;;; Copyright (C) 1991  Lysator Academic Computer Association.
;;;;;
;;;;; This file is part of the LysKOM server.
;;;;; 
;;;;; LysKOM is free software; you can redistribute it and/or modify it
;;;;; under the terms of the GNU General Public License as published by 
;;;;; the Free Software Foundation; either version 1, or (at your option) 
;;;;; any later version.
;;;;; 
;;;;; LysKOM is distributed in the hope that it will be useful, but WITHOUT
;;;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;;;; for more details.
;;;;; 
;;;;; You should have received a copy of the GNU General Public License
;;;;; along with LysKOM; see the file COPYING.  If not, write to
;;;;; Lysator, c/o ISY, Linkoping University, S-581 83 Linkoping, SWEDEN,
;;;;; or the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, 
;;;;; MA 02139, USA.
;;;;;
;;;;; Please mail bug reports to bug-lyskom@lysator.liu.se. 
;;;;;
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
	 