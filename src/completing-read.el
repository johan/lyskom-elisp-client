;;;;;
;;;;; $Id: completing-read.el,v 38.4 1996-01-13 06:50:25 davidk Exp $
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
;;;; ================================================================
;;;; ================================================================
;;;;
;;;; File: completing-read.el
;;;;
;;;; This file implements functions for reading a conference name
;;;; or a person name with completion and other help.
;;;;

;; Overview of `exported' functions from this file:
;; lyskom-read-conf-no        returns conf-no
;; lyskom-read-conf-stat      returns conf-stat
;; lyskom-read-conf-name      returns name


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: completing-read.el,v 38.4 1996-01-13 06:50:25 davidk Exp $\n"))


;;; Author: Linus Tolke


;;; Completing-function

(defvar lyskom-name-hist nil)

(defvar lyskom-minibuffer-local-completion-map
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    (define-key map " " nil)
    map)
  "Keymap used for reading LysKOM names.")

(defvar lyskom-minibuffer-local-must-match-map
  (let ((map (copy-keymap minibuffer-local-must-match-map)))
    (define-key map " " nil)
    map)
  "Keymap used for reading LysKOM names.")

(defun lyskom-read-conf-no (prompt type &optional empty initial)
  "Returns the conf-no of a conf or person read by lyskom-read-conf-name.
The question is prompted with PROMPT.
Only the conferences of TYPE are allowed.
The TYPE allows for subsets of the entire Lyskom-name space:
* all
* confs only conferences
* pers only persons
* logins only persons that are logged in right now.
If EMPTY is non-nil then the empty string is allowed (returns 0).
INITIAL is the initial contents of the input field."
  (let (read)
    (while (and (string= (setq read
			       (lyskom-read-conf-name prompt type t initial))
			 "")
		(not empty)))
    (if (string= read "")
	0
      (lyskom-read-conf-name-internal read type 'conf-no))))


(defun lyskom-read-conf-stat (prompt type &optional empty initial)
  "Exactly the same as lyskom-read-conf-no but returns the conf-stat if possible.
Arguments: PROMPT TYPE EMPTY INITIAL
The TYPE allows for subsets of the entire Lyskom-name space:
* all
* confs only conferences
* pers only persons
* logins only persons that are logged in right now.

If EMPTY is non-nil then the empty string is allowed (returns nil)."
  (let ((no (lyskom-read-conf-no prompt type empty initial)))
    (if (zerop no)
	nil
      (blocking-do 'get-conf-stat no))))

(defun lyskom-read-conf-name (prompt type 
				     &optional mustmatch 
				     initial)
  "Read a LysKOM name, prompting with PROMPT.
The TYPE allows for subsets of the entire Lyskom-name space:
* all
* confs only conferences
* pers only persons
* logins only persons that are logged in right now.
The third argument MUSTMATCH makes the function always return the conf-no and 
never the read string.
The fourth argument INITIAL is the initial contents of the input-buffer.

Returns the name."
  (let* ((completion-ignore-case t)
	 ; When lyskom-read-conf-name-internal is called the current-buffer
	 ; is the minibuffer and the buffer-local variable lyskom-proc is not
	 ; correct. Then the variable lyskom-blocking-process must be set
	 ; instead. It is not buffer-local but scopes the let.
	 (lyskom-blocking-process lyskom-proc)
	 (minibuffer-local-completion-map 
	  lyskom-minibuffer-local-completion-map)
	 (minibuffer-local-must-match-map 
	  lyskom-minibuffer-local-must-match-map))
    (condition-case error
	(completing-read prompt 
			 'lyskom-read-conf-name-internal
			 type
			 mustmatch
			 initial
			 'lyskom-name-hist)
      (wrong-number-of-arguments ; This is for emacs 18.
       (completing-read prompt 'lyskom-read-conf-name-internal
			type mustmatch)))
    ))


(defun lyskom-read-conf-name-internal-verify-type (cs predicate logins)
  "Returns true if CONF-STAT is of the correct type.

For types se documentation of lyskom-read-conf-name-internal.
Logins is a list of conf-nos (only significant when PREDICATE is logins)."
  (or (eq predicate 'all)
      (and (eq predicate 'confs)
	   (not (conf-type->letterbox 
		 (conf-stat->conf-type cs))))
      (and (eq predicate 'pers)
	   (conf-type->letterbox
	    (conf-stat->conf-type cs)))
      (and (eq predicate 'logins)
	   (memq (conf-stat->conf-no cs) logins))))


;; +++ Where should this be put?
(defsubst listify-vector (vector)
  "Turn VECTOR into a list"
  (append vector nil))

(defun lyskom-read-conf-name-internal (string predicate all)
  "The \"try-completion\" for the lyskom-read name.
STRING is the string to be matched.
PREDICATE is one of:
* all
* confs only conferences
* pers only persons
* logins only persons that are logged in right now.
If third argument ALL is t then we are called from all-completions.
If third argument ALL is nil then we are called from try-completion.
If third argument ALL is 'conf-no then we are called from lyskom name
to conf-no translator."
  (let* ((alllogins (and (string= string "")
			 (eq predicate 'logins)))
	 (list (if (not alllogins)
		   (blocking-do 'lookup-name string)))
	 (nos (listify-vector (conf-list->conf-nos list)))
	 (parlist (if (memq predicate '(pers confs))
		      (let ((nos nos)
			    (typs (listify-vector (conf-list->conf-types list)))
			    res)
			(while nos
			  (setq res (cons (cons (car nos) (car typs)) res))
			  (setq nos (cdr nos)
				typs (cdr typs)))
			res)))
	 (logins (and (eq predicate 'logins)
		      (mapcar
		       (function (lambda (ele)
				   (who-info->pers-no ele)))
		       (listify-vector (blocking-do 'who-is-on)))))
	 (mappedlist (cond
		      (alllogins
		       logins)
		      ((eq predicate 'all)
		       nos)
		      ((eq predicate 'confs)
		       (apply 'append 
			(mapcar (function 
				 (lambda (par)
				   (and (not (conf-type->letterbox (cdr par)))
					(list (car par)))))
				parlist)))
		      ((eq predicate 'pers)
		       (apply 'append
			(mapcar (function
				 (lambda (par)
				   (and (conf-type->letterbox (cdr par))
					(list (car par)))))
				parlist)))
		      ((eq predicate 'logins)
		       (let ((nos (sort nos '<))
			     ;; We need logins later on
			     (lis (sort (copy-sequence logins) '<))
			     res)
			 (while (and nos
				     lis)
			   (if (= (car nos) (car lis))
			       (setq res (cons (car nos) res)))
			   (if (> (car nos)
				  (car lis))
			       (setq lis (cdr lis))
			     (setq nos (cdr nos))))
			 res)))))
    (cond
     ((eq all 'conf-no)
      (cond
       ((= (length mappedlist) 1)
	(car mappedlist))
       (t (let ((found nil))
	    (while (and (not found) mappedlist)
	      (if (string= string
			   (conf-stat->name (blocking-do 'get-conf-stat
							 (car mappedlist))))
		  (setq found (car mappedlist)))
	      (setq mappedlist (cdr mappedlist)))
	    (cond
	     (found)
	     ((string-match (lyskom-get-string 'person-or-conf-no-regexp)
			    string)
	      (let* ((no (string-to-int (substring string
						   (match-beginning 1)
						   (match-end 1))))
		     (cs (blocking-do 'get-conf-stat no)))
		(if (lyskom-read-conf-name-internal-verify-type
		     cs predicate logins)
		    no))))))))

     ((eq all 'lambda)
      (or (= (length mappedlist) 1)
	  (let ((found nil))
	    (while (and (not found)
			mappedlist)
	      (if (string= string 
			   (conf-stat->name (blocking-do 'get-conf-stat 
							 (car mappedlist))))
		  (setq found t))
	      (setq mappedlist (cdr mappedlist)))
	    (cond
	     (found)
	     ((string-match (lyskom-get-string 'person-or-conf-no-regexp)
			    string)
	      (let* ((no (string-to-int (substring string
						   (match-beginning 1)
						   (match-end 1))))
		     (cs (blocking-do 'get-conf-stat no)))
		(if (lyskom-read-conf-name-internal-verify-type
		     cs predicate logins)
		    string)))))))
     (all
      (let ((names (mapcar (function (lambda (no)
				       (conf-stat->name 
					(blocking-do 'get-conf-stat no))))
			   mappedlist)))
	(if (and (string-match (lyskom-get-string 'person-or-conf-no-regexp)
			       string)
		 (let* ((no (string-to-int (substring string
						      (match-beginning 1)
						      (match-end 1))))
			(cs (blocking-do 'get-conf-stat no)))
		   (lyskom-read-conf-name-internal-verify-type cs 
							       predicate
							       logins)))
	    (cons string names)
	  names)))
	    
     ((= (length mappedlist) 0)
      (if (string-match (lyskom-get-string 'person-or-conf-no-regexp)
			string)
	  (let* ((no (string-to-int (substring string
					       (match-beginning 1)
					       (match-end 1))))
		 (cs (blocking-do 'get-conf-stat no)))
	    (if (lyskom-read-conf-name-internal-verify-type
		 cs predicate logins)
		t))))

     (t					; Some matches, maybe exact?
      (let ((strings (mapcar (function
			      (lambda (no)
				(list (conf-stat->name 
				       (blocking-do 'get-conf-stat no)))))
			     mappedlist)))
	(if (and (= (length strings) 1)
		 (string= string (car (car strings))))
	    t				; Exact
	(lyskom-try-complete-partials string strings)))))))


(defun lyskom-try-complete-partials (string alist)
  "Returns the longest string matching STRING.
Where every word matches the corresponding word in the car part of ALIST.
parst matching ([^)]) in string and alist are disgarded."
  (let* ((a-whitespace "\\([ \t]\\|([^)]*)\\)+")
	 (string (let ((initwhite (concat "\\`" a-whitespace)))
		   (if (string-match initwhite string)
		       (substring string (match-end 0))
		     string)))
	 (endfirstword (string-match a-whitespace string))
	 (firstword (substring string 0 endfirstword))
	 (reststring (and endfirstword
			  (substring string (match-end 0))))
	 (words (let ((res (try-completion firstword alist)))
		  (cond
		   ((eq res t) string)
		   (res)
		   (t string))))	;+++ Buggfix. Inget error om []\->{}|
	 (endfirstwords (string-match a-whitespace words))
	 (firstwords (substring words 0 endfirstwords))
	 (restlist (mapcar
		    (function
		     (lambda (part)
		       (cond
			((string-match a-whitespace
				       (car part))
			 (list (substring (car part) (match-end 0))))
			((list "")))))
		    alist)))
    (if	(= (length reststring) 0)
	words
      (concat (if (> (length firstwords) (length firstword))
		  firstwords
		firstword)
	      " " (lyskom-try-complete-partials reststring
						restlist)))))	 

					  


