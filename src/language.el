;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: language.el,v 44.21 2000-08-31 12:29:46 byers Exp $
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
;;;; Author: Niels Möller
;;;;
;;;;

(require 'lyskom-vars "vars")

;;; Variables

;;(defvar lyskom-language-symbols nil
;;  "Symbols with language data bound to them")

(defvar lyskom-languages nil
  "A alist of defined languages.
Each entry is a pair (SYMBOL . (NAME NAME ...)) where symbol is the symbol
used for identification, and the NAMEs are names of the language.")

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

(defun lyskom-set-language-vars (language)
  (mapcar
   (function
    (lambda (var)
      (if (or (not (symbol-value var))
	      (get var 'lyskom-language-force))
	  (set var (eval (cdr (assq language
				    (get var 'lyskom-language-var))))))))
   lyskom-language-vars))

;;; Keymaps

(defvar lyskom-language-keymaps nil
  "A list of all language-dependent variables.")

(defun lyskom-language-keymap-internal (keymap language langmap)
  "Defines a language-local variable value."
  ;; If the "real" keymap has no value, set it to an empty keymap
  (if (eval keymap)
      nil
    (set keymap (make-sparse-keymap)))
  ;; Add it to the list of keymaps
  (or (memq keymap lyskom-language-keymaps)
      (setq lyskom-language-keymaps
	    (cons keymap lyskom-language-keymaps)))
  ;; Modify the property list
  (let* ((alist (get keymap 'lyskom-language-keymap))
	 (entry (assq language alist)))
    (if entry
	(setcdr entry langmap)
      (put keymap 'lyskom-language-keymap
	   (cons (cons language langmap) alist)))))

(defmacro lyskom-language-keymap (keymap language langmap)
  (list 'lyskom-language-keymap-internal
	(list 'quote keymap)
	(list 'quote language)
	(list 'quote langmap)))

(put 'lyskom-language-keymap 'lisp-indent-function 2)

(defun lyskom-set-language-keymaps (language)
  (mapcar
   (function
    (lambda (map)
      (set-keymap-parent (symbol-value map)
                         (eval (cdr (assq language
                                          (get map
                                               'lyskom-language-keymap)))))))
   lyskom-language-keymaps))

;(defun lyskom-set-language-keymaps (language)
;  (mapcar
;   (function
;    (lambda (map)
;      (setcdr (symbol-value map)
;	      (eval (cdr (assq language
;			       (get map 'lyskom-language-keymap)))))))
;   lyskom-language-keymaps))

;;; String catalogs

(defun lyskom-language-strings-internal (category language alist)
  "Associates names to symbols.

CATEGORY and LANGUAGE determines what kind of association to
create. ALIST is a mapping from symbols to strings."
  ;; Record category
  (or (memq category lyskom-language-categories)
      (setq lyskom-language-categories
	    (cons category lyskom-language-categories)))
  (let ((record (get category 'lyskom-language-symbols)))
    (mapcar (function (lambda (pair)
			(let* ((symbol (car pair))
			       (string (cdr pair))
			       (llist (get symbol category))
			       (entry (assq language llist)))
			  ;; Record symbol
			  (or (memq symbol record)
			      (setq record
				    (cons symbol record)))
			  (if entry
			      (setcdr entry string)
			    (put symbol category
				 (cons (cons language string) llist))))))
	    alist)
    (put category 'lyskom-language-symbols record)))

(defmacro lyskom-language-strings (category language alist)
  (list 'lyskom-language-strings-internal
	(list 'quote category)
	(list 'quote language)
	alist))

(defun lyskom-language-missing-string-internal (category string languages)
  (let ((old-missing (assq 'lyskom-missing-languages (get string category))))
    (if old-missing
        (setcdr old-missing (append languages (cdr old-missing)))
      (put string category (cons (cons 'lyskom-missing-languages languages)
                                 (get string category))))))

(defun lyskom-language-ending-mismatch-internal (category string l1 l2)
  (let ((old-mismatch (assq 'lyskom-ending-mismatch (get string category))))
    (if old-mismatch
        (setcdr old-mismatch (append (list (cons l1 l2) (cons l2 l1))
                                     (cdr old-mismatch)))
      (put string category (cons (cons 'lyskom-ending-mismatch 
                                       (list (cons l1 l2) (cons l2 l1)))
                                 (get string category))))))

(defmacro lyskom-language-missing-string (category string &rest languages)
  `(lyskom-language-missing-string-internal ',category ',string ',languages))

(defmacro lyskom-language-ending-mismatch (category string l1 l2)
  `(lyskom-language-ending-mismatch-internal ',category ',string ',l1 ',l2))

(put 'lyskom-language-strings 'lisp-indent-function 2)

(defsubst lyskom-tell-string (key)
  "Retrieve the phrase indexed by the key from the kom-tell-phrases
assoc list."
  (condition-case nil
      (lyskom-get-string key 'kom-tell-phrases)
    (lyskom-internal-error (message "Bad kom-tell-phrases: missing %s" key)
                           "")))

(defsubst lyskom-get-string-internal (symbol category)
    (cdr (assq lyskom-language (get symbol category))))

(defsubst lyskom-get-string-error (function symbol category)
  (signal 'lyskom-internal-error
	  (list function (list symbol category ": string not found"))))

(defun lyskom-get-string (symbol &optional category)
  "Returns string associated with SYMBOL"
    (or (lyskom-get-string-internal symbol (or category 'lyskom-message))
        (lyskom-get-string-error 'lyskom-get-string
                                 symbol
                                 (or category 'lyskom-message))))

(defun lyskom-get-string-sol (symbol &optional category)
  "Returns string associated with SYMBOL
If kom-long-lines is set, return the long form of the string, if it exists."
  (or  (and kom-long-lines
            (lyskom-get-string-internal (intern (concat (symbol-name symbol)
                                                        "-long"))
                                        (or category 'lyskom-message)))
       (lyskom-get-string-internal symbol
                                   (or category 'lyskom-message))
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
  (encode-coding-string 
    (or (lyskom-get-string-internal symbol 'lyskom-menu)
        (lyskom-get-string-internal symbol 'lyskom-command)
        (lyskom-get-string-error 'lyskom-get-menu-string symbol 'lyskom-menu))
    'iso-8859-1))

(defun lyskom-string-check-category (category)
  "Returns list of names for the category, and their supported languages"
  (mapcar (function
	   (lambda (symbol)
	     (let ((info (get symbol category)))
	       (if info (cons symbol (mapcar 'car info))))))
	  (get category 'lyskom-language-symbols)))


(defun lyskom-define-language (language coding &rest names)
  (let ((match (assq language lyskom-languages)))
    (if match
	(setcdr match names)
      (setq lyskom-languages (cons (cons language names) lyskom-languages))))
  (unless (and lyskom-language
               kom-default-language)
    (setq lyskom-language language)
    (setq kom-default-language language))
  (put language 'lyskom-language-coding coding))

(defun lyskom-language-coding (language)
  (or (get language 'lyskom-language-coding)
      'raw-text))

(defun lyskom-language-name (language)
  "Return the name of language code LANGUAGE in the current language."
  (save-excursion
    (when lyskom-buffer (set-buffer lyskom-buffer))
    (or (cdr (assq language lyskom-language-codes))
        (lyskom-format (cdr (assq '-- lyskom-language-codes))
                       (symbol-name language)))))

(defun lyskom-set-language (language)
  "Set the current language to LANGUAGE."
  (cond ((not (assq language lyskom-languages))
         (lyskom-format-insert-before-prompt 'language-not-loaded
                                             (lyskom-language-name language))
         nil)
        (t (setq lyskom-language language)
           (lyskom-set-language-vars language)
           (lyskom-set-language-keymaps language)
           (lyskom-update-menus)
           (lyskom-update-prompt t)
           (lyskom-update-command-completion)
           t)))

			      
(eval-and-compile (provide 'lyskom-language))

;;; language.el ends here
