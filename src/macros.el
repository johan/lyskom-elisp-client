;;;;;
;;;;; $Id: macros.el,v 44.8 1997-07-02 11:12:33 petli Exp $
;;;;; Copyright (C) 1991, 1996  Lysator Academic Computer Association.
;;;;;
;;;;; This file is part of the LysKOM server.
;;;;; 
;;;;; LysKOM is free software; you can redistribute it and/or modify it
;;;;; under the terms of the GNU General Public License as published by 
;;;;; the Free Software Foundation; either version 2, or (at your option) 
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
;;;; ================================================================
;;;; ================================================================
;;;;
;;;; File: macros.el
;;;;
;;;; This file contains the macros which must be loaded before lyskom can
;;;; be compiled.
;;;;

(setq lyskom-clientversion-long
      (concat lyskom-clientversion-long
	      "$Id: macros.el,v 44.8 1997-07-02 11:12:33 petli Exp $\n"))




;;; ======================================================================
;;; lyskom-traverse - traverse a sequence.
;;;

(defmacro lyskom-traverse (atom sequence &rest body)
  "Bind ATOM to each element in SEQUENCE and execute BODY.
Value returned is always nil."
  (list 'let* (list '(__i__ 0)
		    (list '__sequence__ sequence)
		    '(__len__ (length __sequence__))
		    atom)
	(list 'if '(listp __sequence__)
	      (append (list 'while '__sequence__
			    (list 'setq atom '(car __sequence__)))
		      body
		      (list '(setq __sequence__ (cdr __sequence__))))
	      (append (list 'while '(< __i__ __len__)
			    (list 'setq atom '(aref __sequence__ __i__)))
		      body
		      (list '(setq __i__ (1+ __i__)))))))


(put 'lyskom-traverse 'edebug-form-spec
     '(sexp form body))


;;; ======================================================================
;;; lyskom-save-excursion Does not save point and mark.
;;;

(defmacro lyskom-save-excursion (&rest forms)
  "Save-excursion without saving point and mark."
  (list 'let (list '(__buffer__ (current-buffer)))
	(list 'unwind-protect
	      (cons 'progn
		    forms)
	      '(set-buffer __buffer__))))

(put 'lyskom-save-excursion 'edebug-form-spec t)

;;; ======================================================================
;;; Some useful macros to make the code more readable.
;;;

(defmacro char-in-string (char string)
  "Return t if the character CHAR is member of STRING. Otherwise return nil."
  (list 'null
	(list 'not
	      (list 'string-match
		    (list 'regexp-quote
			  (list 'char-to-string char))
		    string))))

(defmacro ++ (var)
  "Increment the variable VAR and return the value."
  (list 'setq var (list '1+ var)))

(defmacro -- (var)
  "Decrement the variable VAR and return the value."
  (list 'setq var (list '1- var)))

(defmacro when (expr &rest body)
  "Execute BODY if EXPR evaluates to non-nil"
  (list 'if expr (cons 'progn body)))

(put 'when lisp-indent-function 1)
(put 'when 'edebug-form-spec t)

(defmacro unless (expr &rest body)
  "Execute BODY if EXPR evaluates to non-nil"
  (append (list 'if expr nil) body))

(put 'unless lisp-indent-function 1)
(put 'unless 'edebug-form-spec t)


;;; ======================================================================
;;; Multiple blocking read from server
;;;

(defvar lyskom-multiple-blocking-return nil
  "Return from blocking-do-multiple")

(defun lyskom-blocking-do-multiple (call-list)
  (save-excursion
    (set-buffer (or lyskom-buffer
                    (process-buffer lyskom-proc)))
    ;; If this happens, we're in trouble
    (if lyskom-is-parsing
	(lyskom-really-serious-bug))
    
    (let ((lyskom-multiple-blocking-return 'not-yet-gotten))
      (lyskom-collect 'blocking)
      (while call-list
	(apply (intern-soft (concat "initiate-"
				    (symbol-name (car (car call-list)))))
	       'blocking nil
	       (cdr (car call-list)))
	(setq call-list (cdr call-list)))
      (lyskom-use 'blocking 'lyskom-blocking-do-multiple-1)
      (while (and (eq lyskom-multiple-blocking-return 'not-yet-gotten)
		  (memq (process-status lyskom-proc) '(open run))
		  (not lyskom-quit-flag))
	(accept-process-output lyskom-proc
			       lyskom-apo-timeout-s lyskom-apo-timeout-ms))
      (if lyskom-quit-flag
	  (progn
	    (setq lyskom-quit-flag nil)
	    (lyskom-insert-before-prompt (lyskom-get-string 'interrupted))
	    (signal 'quit nil)))
      lyskom-multiple-blocking-return)))

(defun lyskom-blocking-do-multiple-1 (&rest data)
  (setq lyskom-multiple-blocking-return data))

(defmacro blocking-do-multiple (bind-list &rest body)
  "Bind variables according to BIND-LIST and then eval BODY.
The value of the last form in BODY is returned.
Each element in BIND-LIST is a list (SYMBOL FORM) which binds SYMBOL to
the result of the server call FORM, which is the same as used in blocking-do.
All the forms in BIND-LIST are evaluated before and symbols are bound."
  (let ((bindsym 'multiple-bind-sym)
	(index 0))
    (` (let (((, bindsym)
	      (lyskom-blocking-do-multiple
	       (list (,@ (mapcar (function (lambda (x) 
					     (` (list '(, (car (car (cdr x))))
						      (,@ (cdr (car (cdr x))))))))
			   bind-list))))))
	 (let ((,@ (mapcar (function 
			    (lambda (bpat)
			      (prog1
				  (` ((, (car bpat))
				      (elt (, bindsym) (, index))))
				(setq index (1+ index)))))
			   bind-list)))
	   (,@ body))))))

(put 'blocking-do-multiple 'edebug-form-spec
     '(sexp body))

(put 'blocking-do-multiple 'lisp-indent-function 1)


;;; ======================================================================
;;; These macros do magic things to the compiler to avoid gratuitous
;;; compiler warnings.
;;;

;; (eval-when-compile (defvar lyskom-expected-unresolved-functions nil))
(defvar lyskom-expected-unresolved-functions nil)

(defmacro lyskom-external-function (fn)
  (` (eval-when-compile
       (setq lyskom-expected-unresolved-functions
             (cons (quote (, fn))
                   lyskom-expected-unresolved-functions)))))

(defmacro lyskom-end-of-compilation ()
  (` 
   (eval-when-compile
     (progn
       (if (and (boundp 'byte-compile-unresolved-functions)
                (consp (car-safe byte-compile-unresolved-functions))
                (symbolp (car-safe (car-safe 
                                    byte-compile-unresolved-functions))))
           (mapcar (function (lambda (x)
                               (setq byte-compile-unresolved-functions
                                     (delq
                                      (assq x
                                            byte-compile-unresolved-functions)
                                      byte-compile-unresolved-functions))))
                   lyskom-expected-unresolved-functions))
       (if lyskom-compatibility-definitions
           (message "Compatibility definitions: %s"
                    (mapconcat '(lambda (sym)
                                  (symbol-name sym))
                               lyskom-compatibility-definitions
                               ", ")))))))

;;; ================================================================
;;;         Faces

(defmacro lyskom-make-face (name &rest body)
  (` (if (memq (, name) (face-list))
	 nil
       (,@ body))))

(put 'lyskom-make-face 'lisp-indent-function 1)

(provide 'lyskom-macros)


;;; ============================================================
;;; Keymap handling
;;;

(defmacro lyskom-use-local-map (keymap)
  "Use keymap KEYMAP as local map in this buffer. KEYMAP is made local in
the current buffer, and its value is copied from the LysKOM buffer."
  (` (progn (make-local-variable (quote (, keymap)))
            (setq (, keymap)
                  (lyskom-default-value (quote (, keymap))))
            (use-local-map (, keymap)))))

;;; ============================================================
;;; Local variables
;;;

(defmacro lyskom-setq-default (name value)
  (` (lyskom-set-default (quote (, name))
                         (, value))))


;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; end: 
