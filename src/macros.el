;;;;;
;;;;; $Id: macros.el,v 43.2 1996-08-27 15:15:42 byers Exp $
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
	      "$Id: macros.el,v 43.2 1996-08-27 15:15:42 byers Exp $\n"))


;;; ======================================================================
;;; Use lyskom-provide to supply a definition that is only to be used
;;; if no definition already exists. The definition will be evaluated at
;;; both compile and run time.
;;;
;;; lyskom-provide-macros behaves like defmacro
;;; lyskom-provide-function behaves like defun
;;; lyskom-provide-subst behaves like defsubst
;;;

(defmacro lyskom-provide (definer name rest)
  (` (progn (eval-when-compile 
              (if (not (fboundp (quote (, name))))
                  (message "Compatibility %S for %S"
                           (quote (, definer))
                           (quote (, name)))))
            (eval-and-compile 
              (if (not (fboundp (quote (, name))))
                  ((, definer) (, name) (,@ rest)))))))

(defmacro lyskom-provide-macro (name &rest rest)
  (` (lyskom-provide defmacro (, name) (, rest))))

(defmacro lyskom-provide-function (name &rest rest)
  (` (lyskom-provide defun (, name) (, rest))))


(defmacro lyskom-provide-subst (name &rest rest)
  (` (lyskom-provide defsubst (, name) (, rest))))


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
;;; LysKOM user commands
;;; The new, blocking commands have a very similar structure
;;;
;;;  (defun kom-cmd (args)
;;;    "Documentation"
;;;    (interactive "...")
;;;    (lyskom-start-of-command 'kom-cmd)
;;;    (unwind-protect
;;;        (progn ...)
;;;      (lyskom-end-of-command)))
;;;
;;; This can now be written as
;;;
;;; (def-kom-command kom-cmd (args)
;;;   "Documentation"
;;;   (interactive "...")
;;;   ...)

(defmacro def-kom-command (cmd args doc interactive-decl &rest forms)
  (list 'defun cmd args doc interactive-decl
	(list 'lyskom-start-of-command (list 'quote cmd))
	(list 'unwind-protect
	      (cons 'progn
		    forms)
	      (list 'lyskom-end-of-command))))


;;(def-edebug-spec def-kom-command
;;  (&define name lambda-list
;;                [&optional stringp]   ; Match the doc string, if present.
;;                ("interactive" interactive)
;;                def-body))
(put 'def-kom-command 'edebug-form-spec
     '(&define name lambda-list
	       [&optional stringp]	; Match the doc string, if present.
	       ("interactive" interactive)
	       def-body))


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


;;; ======================================================================
;;; Multiple blocking read from server
;;;

(defvar lyskom-multiple-blocking-return nil
  "Return from blocking-do-multiple")

(defun lyskom-blocking-do-multiple (call-list)
  (save-excursion
    (set-buffer (process-buffer (or lyskom-proc
				    lyskom-blocking-process)))
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
	(accept-process-output nil lyskom-apo-timeout-s lyskom-apo-timeout-ms))
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



;;; ============================================================
;;; General compatibility macros and functions
;;;
;;; These are functions that may not be available in every version of
;;; GNU Emacs 19. Replacement functions are defined here.
;;;
;;; Compatibility for other Emascs version should be placed in
;;; external compatibility files so as not to sully compilation with
;;; too many compiler warnings.
;;;

(lyskom-provide-macro byte-code-function-p (obj)
  (` (compiled-function-p (, obj))))

(lyskom-provide-subst internal-facep (x)
  (and (vectorp x) (= (length x) 8) (eq (aref x 0) 'face)))

(lyskom-provide-function facep (x)
  "Return t if X is a face name or an internal face vector."
  (and (or (and (fboundp 'internal-facep)
                (internal-facep x))
           (and (symbolp x) 
                (boundp 'global-face-data)
                (assq x global-face-data)))
       t))


(lyskom-provide-macro save-selected-window (&rest body)
  "Execute BODY, then select the window that was selected before BODY."
  (list 'let
        '((save-selected-window-window (selected-window)))
        (list 'unwind-protect
              (cons 'progn body)
              (list 'select-window 'save-selected-window-window))))

(if (not (fboundp 'frame-width))
    (fset 'frame-width 'screen-width))

(lyskom-provide-function match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

;;; ======================================================================
;;; These macros do magic things to the compiler to avoid gratuitous
;;; compiler warnings.
;;;

(eval-when-compile (defvar lyskom-expected-unresolved-functions nil))

(defmacro lyskom-external-function (fn)
  (` (eval-when-compile
       (setq lyskom-expected-unresolved-functions
             (cons (quote (, fn))
                   lyskom-expected-unresolved-functions)))))

(defmacro lyskom-end-of-compilation ()
  (` 
   (eval-when-compile
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
                 lyskom-expected-unresolved-functions)))))


;;; ======================================================================
;;; Definition of map-keymap that hopefully works like the one in XEmacs
;;; except that the sort-first argument is ignored.
;;;

(lyskom-provide-function map-keymap (fn keymap &optional sort-first)
  (let ((lis nil)
        (r 0))
    (cond ((vectorp keymap)
           (while (< r (length keymap))
             (if (aref keymap r)
                 (funcall fn r (aref keymap r)))
             (setq r (1+ r))))
          (t (mapcar (function 
                      (lambda (x)
                        (funcall fn (car x) (cdr x))))
                     (cdr keymap))))))


;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-provide-macro 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-provide-function 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-provide-subst 'lisp-indent-hook 2)
;;; end: 



