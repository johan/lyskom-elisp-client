;;;;;
;;;;; $Id: language.el,v 44.4 1996-10-20 02:56:51 davidk Exp $
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
;;;; File: language.el
;;;; Author: Niels M�ller
;;;;
;;;;

(require 'lyskom-vars "vars")

;;(defconst lyskom-category-properties		    
;;  '((command . lyskom-command)			    
;;    (menu    . lyskom-menu)			    
;;    (message . lyskom-message))			    
;;  "Property names for saving different categories.")

(defvar lyskom-language-symbols nil
  "Symbols with language data bound to them")

(defvar lyskom-language-categories nil
  "Categories used")

(defvar lyskom-language-vars nil
  "A list of all language-dependent variables.")

(defun lyskom-language-var-internal (var language val)
  "Defines a language-local variable value."
  (or (memq var lyskom-language-vars)
      (setq lyskom-language-vars
	    (cons var lyskom-language-vars)))
  (let* ((alist (get var 'lyskom-language-var))
	 (entry (assq language alist)))
    (if entry
	(setcdr entry val)
      (put var 'lyskom-language-var (cons (cons language val) alist)))))

(defmacro lyskom-language-var (var language val)
  (list 'lyskom-language-var-internal
	(list 'quote var)
	(list 'quote language)
	(list 'quote val)))

(put 'lyskom-language-var 'lisp-indent-function 2)

(defun lyskom-language-strings (category language alist)
  "Associates names to symbols.

CATEGORY and LANGUAGE determines what kind of association to
create. ALIST is a mapping from symbols to strings."
  ;; Record category
  (or (memq category lyskom-language-categories)
      (setq lyskom-language-categories
	    (cons category lyskom-language-categories)))
  (mapcar (function (lambda (pair)
		      (let* ((symbol (car pair))
			     (string (cdr pair))
			     (llist (get symbol category))
			     (entry (assq language llist)))
			;; Record symbol
			(or (memq symbol lyskom-language-symbols)
			    (setq lyskom-language-symbols
				  (cons symbol lyskom-language-symbols)))
			(if entry
			    (setcdr entry string)
			  (put symbol category
			       (cons (cons language string) llist))))))
	  alist))

(put 'lyskom-language-strings 'lisp-indent-function 2)

(defsubst lyskom-get-string-internal (symbol category)
    (cdr (assq lyskom-language (get symbol category))))

(defsubst lyskom-get-string-error (function symbol category)
  (signal 'lyskom-internal-error
	  (list function (list symbol category ": string not found"))))

(defun lyskom-get-string (symbol &optional category)
  "Returns string assiciated with SYMBOL"
    (or (lyskom-get-string-internal symbol (or category 'lyskom-message))
        (lyskom-get-string-error 'lyskom-get-string
                                 symbol
                                 (or category 'lyskom-message))))


(defun lyskom-get-strings (symbols &optional category)
  "Returns an alist of (symbol . string) pairs

according to CATEGORY and lyskom-language. Kind of inverse to
lyskom-define-language."
  (mapcar (function (lambda (symbol)
		      (cons symbol (lyskom-get-string symbol category))))
	  symbols))

(defun lyskom-get-menu-string (symbol)
  "Returns the name of a menu(item)

Looks for the 'lyskom-menu category, or 'lyskom-command
if 'lyskom-menu is not found."
    (or (lyskom-get-string-internal symbol 'lyskom-menu)
        (lyskom-get-string-internal symbol 'lyskom-command)
        (lyskom-get-string-error 'lyskom-get-menu-string symbol 'lyskom-menu)))

;;(defun nisse-filter (pred list)
;;  (cond ((null list) nil)
;;	  ((funcall pred (car list))
;;	   (cons (car list) (nisse-filter pred (cdr list))))
;;	  (t (nisse-filter pred (cdr list)))))
    
(defun lyskom-string-check-category (category)
  "Returns list of names for the cetegory, and their supported languages"
  (delq nil (mapcar (function
		     (lambda (symbol)
		       (let ((info (get symbol category)))
			 (if info (cons symbol (mapcar 'car info))))))
		    lyskom-language-symbols)))

			      
(provide 'lyskom-language)

;;; language.el ends here
