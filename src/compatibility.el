;;;;; -*- emacs-lisp -*-
;;;;; $Id: compatibility.el,v 44.1 1996-10-24 09:47:39 byers Exp $
;;;;; Copyright (C) 1996  Lysator Academic Computer Association.
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
;;;; File: compatibility.el
;;;;
;;;; This file contains functions that may not exist in all supported
;;;; versions of Gnu Emacs. XEmacs-specific and Emacs 18-specific code
;;;; should go in some other file.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: compatibility.el,v 44.1 1996-10-24 09:47:39 byers Exp $\n"))


;;; ======================================================================
;;; Use lyskom-provide to supply a definition that is only to be used
;;; if no definition already exists. The definition will be evaluated at
;;; both compile and run time.
;;;
;;; lyskom-provide-macros behaves like defmacro
;;; lyskom-provide-function behaves like defun
;;; lyskom-provide-subst behaves like defsubst
;;;

(eval-and-compile
  (defvar lyskom-compatibility-definitions nil
    "Functions defined or redefined because they are incompatible with
LysKOM"))

;;; ============================================================
;;; lyskom-compatibility-forms
;;; lyskom-compatibility-definition
;;;


(defmacro lyskom-compatibility-forms (predicate &rest forms)
  "If PREDICATE is nil, evaluate FORMS at compile and run time"
  (` (eval-and-compile
       (if (not (, predicate))
           (progn (,@ forms))))))


(defmacro lyskom-compatibility-definition (predicate definition)
  "If PREDICATE is nil, evaluate DEFINITION at compile and run time.
Definition should be a function definition of some kind, with syntax 
similar to defun or defmacro.

To simply define a function if it is not already defined, used one
of the lyskom-provide-* functions instead."
  (` (progn (eval-when-compile
              (if (not (, predicate))
                  (message "Compatibility %S for %S"
                           (quote (, (car definition)))
                           (quote (, (car (cdr definition))))))
              (eval-and-compile
                (if (not (, predicate))
                    (progn
                      (, definition)
                      (setq lyskom-compatibility-definitions
                            (cons (quote (, (car (cdr definition))))
                                  lyskom-compatibility-definitions)))))))))


;;; ============================================================
;;; lyskom-provide
;;; lyskom-provide-macro
;;; lyskom-provide-function
;;; lyskom-provide-subst
;;;
;;; Define functions if they are not already defined
;;;

(defmacro lyskom-provide (definer name rest)
  (` (progn (eval-when-compile 
              (if (not (fboundp (quote (, name))))
                  (message "Compatibility %S for %S"
                           (quote (, definer))
                           (quote (, name)))))
            (eval-and-compile 
              (if (not (fboundp (quote (, name))))
                  (progn
                    (setq lyskom-compatibility-definitions
                          (cons (quote (, name)) 
                                lyskom-compatibility-definitions))
                    ((, definer) (, name) (,@ rest))))))))

(defmacro lyskom-provide-macro (name &rest rest)
  "If NAME is not already defined, define it as a macro."
  (` (lyskom-provide defmacro (, name) (, rest))))

(defmacro lyskom-provide-function (name &rest rest)
  "If NAME is not already defined, define it as a function."
  (` (lyskom-provide defun (, name) (, rest))))

(defmacro lyskom-provide-subst (name &rest rest)
  "If NAME is not already defined, define it as a defsubst."
  (` (lyskom-provide defsubst (, name) (, rest))))


;;; ======================================================================
;;; ======================================================================
;;; ======================================================================

;;;
;;; Dummy definitions
;;;

(defun add-hook-org (x y z))
(defun remove-hook-org (x y))

;;;
;;; Real functions
;;;

(lyskom-provide-function rassoc (key list)
  "Return non-nil if KEY is `equal' to the cdr of an element of LIST.
The value is actually the element of LIST whose cdr is KEY."
  (catch 'rassoc
    (while list
      (if (equal key (cdr (car list)))
          (throw 'rassoc (car list))
        (setq list (cdr list))))
    (throw 'rassoc nil)))


(lyskom-provide-function rassq (key list)
  "Return non-nil if KEY is `eq' to the cdr of an element of LIST.
The value is actually the element of LIST whose cdr is KEY."
  (catch 'rassq
    (while list
      (if (eq key (cdr (car list)))
          (throw 'rassq (car list))
        (setq list (cdr list))))
    (throw 'rassq nil)))


;;;
;;; Functions for limited simulation of local hooks
;;;

(lyskom-compatibility-forms (fboundp 'make-local-hook)
    (fset 'add-hook-org (symbol-function 'add-hook))
  (fset 'remove-hook-org (symbol-function 'remove-hook)))

(lyskom-compatibility-definition (fboundp 'make-local-hook)
    (defun add-hook (hook function &optional append local)
      (add-hook-org hook function append)))

(lyskom-compatibility-definition (fboundp 'make-local-hook)
 (defun remove-hook (hook function &optional local)
   (remove-hook-org hook function)))

(lyskom-provide-function make-local-hook (hook)
  "Poor man's definition of make-local-hook. Better'n nothing..."
  (make-variable-buffer-local hook))



(lyskom-provide-function buffer-live-p (buf)
   (and (bufferp buf) (buffer-name buf)))                         

(lyskom-provide-function get-buffer-window-list (buffer
                                                 &optional minibuf frame)
  "Return windows currently displaying BUFFER, or nil if none.
See `walk-windows' for the meaning of MINIBUF and FRAME."
  (let ((buffer (if (bufferp buffer) buffer (get-buffer buffer))) windows)
    (walk-windows (function (lambda (window)
			      (if (eq (window-buffer window) buffer)
				  (setq windows (cons window windows)))))
		  minibuf frame)
    windows))

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

(lyskom-compatibility-forms (fboundp 'frame-width)
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


(lyskom-provide-function set-keymap-parent (keymap new-parent)
   (let ((tail keymap))
     (while (and tail (cdr tail) (not (eq (car (cdr tail)) 'keymap)))
       (setq tail (cdr tail)))
     (if tail
         (setcdr tail new-parent))))


;;; Local Variables:
;;; eval: (put 'lyskom-provide-macro 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-provide-function 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-provide-subst 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-compatibility-forms 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-compatibility-definition 'lisp-indent-hook 2)
;;; end:
