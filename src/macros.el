;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: macros.el,v 44.33 2003-08-17 12:54:52 byers Exp $
;;;;; Copyright (C) 1991-2002  Lysator Academic Computer Association.
;;;;;
;;;;; This file is part of the LysKOM Emacs LISP client.
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
	      "$Id: macros.el,v 44.33 2003-08-17 12:54:52 byers Exp $\n"))

;;;
;;; Require parts of the widget package. We do this to avoid generating
;;; errors later on. This sucks. 
;;;

(require 'custom)
(require 'widget)

;;; ======================================================================
;;; lyskom-traverse - traverse a sequence.
;;;

(defmacro lyskom-traverse (atom sequence &rest body)
  "Bind ATOM to each element in SEQUENCE and execute BODY.
Value returned is always nil."
  `(let* ((__i__ 0)
          (__sequence__ ,sequence)
          (__len__ (or (listp __sequence__ )
                       (length __sequence__)))
          (,atom nil)
          (__result__ nil))
     (setq __result__ __result__)       ; Get rid of compiler warnings
     (if (listp __sequence__)
         (while __sequence__
           (setq ,atom (car __sequence__))
           ,@body
           (setq __sequence__ (cdr __sequence__)))
       (while (< __i__ __len__)
         (setq ,atom (aref __sequence__ __i__))
         ,@body
         (setq __i__ (1+ __i__))))
     __result__))
  

(defmacro lyskom-traverse-break (&optional result)
  "Break a current lyskom-traverse"
  `(progn (setq __len__ 0)
          (setq __sequence__ nil)
          (setq __result__ (or ,result __result__))))

(defmacro lyskom-traverse-aux (atom sequence &rest body)
  "Bind ATOM to each element in SEQUENCE and execute BODY.
Value returned is always nil."
  (let ((seq (make-symbol "aux-items")))
    (` (let (((, seq) (, sequence))
             ((, atom) nil))
         (while (, seq)
           (setq (, atom) (car (, seq)))
           (if (not (aux-item-flags->deleted
                     (aux-item->flags (, atom))))
               (progn (,@ body)))
           (setq (, seq) (cdr (, seq))))))))


(put 'lyskom-traverse-aux 'edebug-form-spec
     '(sexp form body))

(put 'lyskom-traverse-aux 'lisp-indent-hook 2)

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
(put 'lyskom-save-excursion 'lisp-indent-hook 0)

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

(eval-and-compile
  (if (fboundp 'when)
      nil
    (defmacro when (expr &rest body)
      "Execute BODY if EXPR evaluates to non-nil"
      (list 'if expr (cons 'progn body)))
    (put 'when lisp-indent-function 1)
    (put 'when 'edebug-form-spec t)))

(eval-and-compile
  (if (fboundp 'unless)
      nil
    (defmacro unless (expr &rest body)
      "Execute BODY if EXPR evaluates to non-nil"
      (append (list 'if expr nil) body))
    (put 'unless lisp-indent-function 1)
    (put 'unless 'edebug-form-spec t)))


;;; ======================================================================
;;; Multiple blocking read from server
;;;

(defmacro blocking-do-multiple (bind-list &rest body)
  "Bind variables according to BIND-LIST and then eval BODY.
The value of the last form in BODY is returned.
Each element in BIND-LIST is a list (SYMBOL FORM) which binds SYMBOL to
the result of the server call FORM, which is the same as used in blocking-do.
All the forms in BIND-LIST are evaluated before any symbols are bound."
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
;;; Some commands generat async messages we don't really want
;;;

(defmacro lyskom-ignoring-async (async &rest body)
  `(let ((lyskom-ignoring-async-list 
          (cons (list ,@async) lyskom-ignoring-async-list)))
     ,@body))

(put 'lyskom-ignoring-async 'edebug-form-spec
     '(sexp body))

(put 'lyskom-ignoring-async 'lisp-indent-function 1)


;;; ======================================================================
;;; These macros do magic things to the compiler to avoid gratuitous
;;; compiler warnings.
;;;

(eval-and-compile (defvar lyskom-expected-unresolved-functions nil))
(eval-and-compile (defvar lyskom-expected-unresolved-functions-temp nil))

(defmacro lyskom-external-function (fn)
  `(eval-when-compile
       (setq lyskom-expected-unresolved-functions
             (cons ',fn lyskom-expected-unresolved-functions))))

(defmacro lyskom-with-external-functions (fns &rest body)
  "Ignore warnings about unresolved functions FNS when compiling BODY."
  `(progn (eval-when-compile
            (and (boundp 'byte-compile-unresolved-functions)
                 (setq lyskom-expected-unresolved-functions-temp
                       (cons (delq nil
                                   (mapcar (lambda (fn)
                                             (if (assq fn byte-compile-unresolved-functions)
                                                 nil
                                               fn))
                                           ',fns))
                             lyskom-expected-unresolved-functions-temp))))
          ,@body
          (eval-when-compile
            (and (boundp 'byte-compile-unresolved-functions)
                 (mapcar (lambda (fn) (and fn (setq byte-compile-unresolved-functions
                                                    (delq (assq fn byte-compile-unresolved-functions)
                                                          byte-compile-unresolved-functions))))
                         (car lyskom-expected-unresolved-functions-temp)))
            (setq lyskom-expected-unresolved-functions-temp
                  (cdr lyskom-expected-unresolved-functions-temp)))))


(defmacro lyskom-end-of-compilation ()
  (` 
   (eval-when-compile
     (progn
       (if (and (boundp 'byte-compile-unresolved-functions)
                (consp (car-safe byte-compile-unresolved-functions))
                (symbolp (car-safe (car-safe 
                                    byte-compile-unresolved-functions))))
           (progn
             (mapcar (function (lambda (x)
                                 (setq byte-compile-unresolved-functions
                                       (delq
                                        (assq x
                                              byte-compile-unresolved-functions)
                                        byte-compile-unresolved-functions))))
                     lyskom-expected-unresolved-functions)
             ))
       (if lyskom-compatibility-definitions
           (message "Compatibility definitions: %s"
                    (mapconcat '(lambda (sym)
                                  (symbol-name sym))
                               lyskom-compatibility-definitions
                               ", ")))))))

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

(defmacro lyskom-try-define-key (map seq fn)
  `(condition-case nil
       (define-key ,map ,seq ,fn)
     (error nil)))


;;; ============================================================
;;; Widget gunk
;;;

(defmacro lyskom-widget-wrapper (fn file)
  `(eval-and-compile
     (if (not (fboundp ',fn))
         (autoload ',fn ,file))))

(lyskom-widget-wrapper define-widget "widget")
(lyskom-widget-wrapper widget-at "wid-edit")
(lyskom-widget-wrapper widget-value "wid-edit")
(lyskom-widget-wrapper widget-button-click "wid-edit")
(lyskom-widget-wrapper widget-setup "wid-edit")
(lyskom-widget-wrapper widget-value-set "wid-edit")
(lyskom-widget-wrapper widget-insert "wid-edit")
(lyskom-widget-wrapper widget-create "wid-edit")
(lyskom-widget-wrapper widget-get "wid-edit")
(lyskom-widget-wrapper widget-put "wid-edit")

;;; ============================================================
;;; Signal gunk
;;;

(defmacro lyskom-ignore-errors (&rest forms)
  (` (condition-case nil
         (progn (,@ forms))
       (error nil))))

(put 'ignore-errors 'edebug-form-spec
     '(sexp form body))

(defun lyskom-assert-error (&rest args)
  (save-excursion
    (save-match-data
      (if (not (and (boundp 'lyskom-buffer)
                    (bufferp lyskom-buffer)))
          (let ((debug-on-error t))
            (apply 'error args))
        (set-buffer lyskom-buffer)
        (lyskom-save-backtrace)
        (apply 'message args)
        (lyskom-insert-before-prompt (concat (apply 'format args) "\n"))
        (lyskom-insert-before-prompt (format "%s\n" (car (car lyskom-backtrace-list))))
        (lyskom-insert-before-prompt (format "%s\n" (elt (car lyskom-backtrace-list) 1)))))))

(eval-and-compile
  (if lyskom-debug-compile
      (defsubst lyskom-assert (check &rest args)
        (or check (if args (apply 'lyskom-assert-error args)
                    (lyskom-assert-error "Assertion failed"))))
    (defsubst lyskom-assert (check &rest args) nil)))

;;; ============================================================
;;; Local variables
;;;

(defmacro lyskom-setq-default (name value)
  (` (lyskom-set-default (quote (, name))
                         (, value))))


(eval-and-compile (provide 'lyskom-macros))

;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-save-excursion 'lisp-indent-hook 0)
;;; eval: (put 'lyskom-ignore-errors 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-with-external-functions 'lisp-indent-hook 1)
;;; end: 
