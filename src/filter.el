;;;;;
;;;;; $Id: filter.el,v 38.6 1996-03-04 15:13:09 byers Exp $
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
;;;; File: filter.el
;;;;
;;;; Contains the support functions for text filtering.
;;;;


;;;============================================================
;;;
;;; Utility functions.
;;;
;;; These should be shared in LysKOM
;;;

(defun copy-tree (l)
  "Recursively copy the list L"
  (cond ((atom l) l)
        (t (cons (copy-tree (car l))
                 (copy-tree (cdr l))))))

(defun functionp (fn)
  "Return t if fn is callable"
  (or (compiled-function-p fn)
      (and (listp fn)
           (eq 'lambda (car fn)))
      (and (symbolp fn)
           (symbol-function fn))))

;;;============================================================
;;;
;;; Filter pattern accessors
;;;
;;; Filters are lists with the following structure
;;;
;;;   (filter [ PATTERNS ATTRIBUTES FUNCTION])
;;;
;;; Where PATTERNS is a list of filter patterns and ATTRIBUTES is
;;; an association list. FUNCTION is a lisp function implementing
;;; the filter.
;;;
;;; The following attributes are reserved
;;;
;;; action -- What action to take when the pattern matches. Currently
;;;           one of dontshow, skip or skip-comments.
;;; expire -- When the filter expires. Not used.
;;;

(defun make-filter (&optional p a)
  "Creates and returns a filter structure.
Optional P and A initialize pattern and attributes, respectively."
  (list 'filter (vector p a
			(lyskom-create-compile-filter-function p))))

(defun copy-filter (f)
  "Create a copy of the filter F"
  (make-filter (copy-tree (filter->pattern f))
               (copy-tree (filter->attribute-list f))))

(defun filter-p (f)
  "Returns T if f looks like a filter"
  (and (listp f)
       (eq 'filter (car f))))

(defun filter->pattern (f)
  "Extract the patterns part of a filter F"
  (elt (elt f 1) 0))

(defun filter->attribute-list (f)
  "Extract the attribute list of F."
  (elt (elt f 1) 1))

(defun filter->attribute (f a)
  "From filter F, extract the value of attribute A. 
Returns nil if no such attribute is present."
  (cdr (assq a (filter->attribute-list f))))

(defun set-filter->pattern (f p)
  "Set the patterns part of F to P."
  (aset (elt f 1) 0 p))

(defun set-filter->attribute-list (f l)
  "Set the attribute list of filter F to L"
  (aset (elt f 1) 1 l))

(defun set-filter->attribute (f a v)
  "Set the value in filter F of attribute A to V."
  (let ((x (assq a (filter->attribute-list f))))
    (if x
        (setcdr x v)
      (set-filter->attribute-list f
       (cons (cons a v) (filter->attribute-list f))))))

(defun filter->function (f)
  "Get the function for filter F"
  (elt (elt f 1) 2))

(defun set-filter->function (f fn)
  "Set the function for filter F to FN"
  (aset (elt f 1) 2 fn))


;;;============================================================
;;;
;;;


(defvar lyskom-filter-hack nil)

(defun lyskom-filter-text-p (text-no)
  (if (null lyskom-filter-list)
      nil
    (progn
      (setq lyskom-filter-hack 'invalid-value)
      (initiate-get-text-stat 'filter 'lyskom-filter-text-p-2 text-no)
      ;;
      ;; Block until done
      ;;
      (while (eq lyskom-filter-hack 'invalid-value)
        (accept-process-output))
      lyskom-filter-hack)))
    
(defun lyskom-filter-text-p-2 (text-stat)
  (if (null text-stat)
      nil
    (progn
      ;;
      ;; Collect information from the server
      ;;
      (lyskom-collect 'filter)
      
      ;;
      ;; Get the conf-stat of the author of the text
      ;; Get the text body
      ;;
      
      (initiate-get-conf-stat 'filter nil (text-stat->author text-stat))
      (initiate-get-text 'filter nil (text-stat->text-no text-stat))
      
      ;;
      ;; Get the conf-stat of the recipients
      ;;
      
      (lyskom-traverse
          misc
          (text-stat->misc-info-list text-stat)
        (let ((type (misc-info->type misc)))
          (if (or (eq type 'RECPT) (eq type 'CC-RECPT))
              (initiate-get-conf-stat 'filter 
                                      nil
                                      (misc-info->recipient-no misc)))))
      
      ;;
      ;; Use the results
      ;;
      
      (lyskom-use 'filter 'lyskom-filter-text-p-3 text-stat))))
    
(defun lyskom-filter-text-p-3 (author text &rest data)
  (if (or (null text)
	  (null author))
      nil
    (let (subject recipient-list text-stat)
      
      ;;
      ;; Extract the subject
      ;;
      
      (cond ((string-match "\n" (text->text-mass text))
	     (setq subject 
		   (substring (text->text-mass text) 0 (match-beginning 0))))
	    (t (setq subject "")))
      
      ;;
      ;; Extract the text-stat
      ;; Shorten the list (quick'n'dirty)
      ;;
      
      (setq text-stat (elt data (- (length data) 1)))
      (if (= (length data) 1)
	  (setq data nil)
	(rplacd (nthcdr (- (length data) 2) data) nil))
      
      ;;
      ;; Do the checking
      ;;
      
      (setq lyskom-filter-hack
	    (lyskom-check-filter-list text-stat
				      author
				      data
				      subject
				      (text->text-mass text)
				      lyskom-filter-list)))))

(defun lyskom-check-filter-list (text-stat
                                 author
                                 recipient-list
                                 subject
                                 text
                                 filter-list)
  (let (tmp)
    (while filter-list
      (if (functionp (filter->function (car filter-list)))
          (setq tmp (funcall (filter->function (car filter-list))
                             (car filter-list)
                             author
                             recipient-list
                             subject
                             text-stat
                             text)))
        (if tmp
            (setq filter-list nil)
          (setq filter-list (cdr filter-list))))
    tmp))




;;;========================================
;;;  The filter compiler.
;;;


(defmacro lyskom-filter-is-member (testfn arg list selector)
  (` (let (found
           (objlist (, list)))
       (while (and objlist (not found))
         (and ((, testfn) (, arg) ((, selector) (car objlist)))
           (setq found t))
      (setq objlist (cdr objlist)))
    found)))


(defun lyskom-create-compile-filter-function (pattern)
  (if (null pattern)
      (byte-compile '(lambda (x y) nil)))
  (byte-compile (lyskom-create-filter-function pattern)))


(defun lyskom-create-filter-function (pattern)
  (` (lambda (filter author recipient-list subject text-stat text)
       (, (cons 'and (lyskom-create-filter-function-body pattern))))))

(defun lyskom-create-filter-function-body (pattern)
  (let (inverse tmp)
    (cond
     ;;
     ;; End of pattern
     ;; 
     
     ((null pattern) '((filter->attribute filter 'action)))

     ;;
     ;; Bad pattern
     ;;
     
     ((or (not (listp pattern))
          (not (listp (car pattern)))) 
      (lyskom-error 
       "%s"
       (lyskom-get-string 'filter-error-specification)))
     
     ;;
     ;; Assume valid pattern
     ;;
     
     (t
      (let ((key (car (car pattern)))
            (args (cdr (car pattern)))
            (form nil))
        (if (eq key 'not)
            (if (not (listp args))
                (lyskom-error "%s" (lyskom-get-string 'filter-error-bad-not))
              (setq key (car args) args (cdr args) inverse t)))

        (setq form
              (cond 
               ((eq key 'author)
                (lyskom-filter-check-args 'stringp args)
                (` (string-match (, (regexp-quote args))
                                 (conf-stat->name author))))

               ((eq key 'author-re)
                (lyskom-filter-check-args 'regexpp args)
                (` (string-match (, args)
                                 (conf-stat->name author))))

               ((eq key 'author-no)
                (lyskom-filter-check-args 'integerp args)
                (` (= (, args) (conf-stat->conf-no author))))

               ((eq key 'recipient)
                (lyskom-filter-check-args 'stringp args)
                (` (lyskom-filter-is-member
                    string-match
                    (, (regexp-quote args))
                    recipient-list
                    conf-stat->name)))
               ((eq key 'recipient-re)
                (lyskom-filter-check-args 'regexpp args)
                (` (lyskom-filter-is-member
                    string-match
                    (,  args)
                    recipient-list
                    conf-stat->name)))
               ((eq key 'recipient-no)
                (lyskom-filter-check-args 'integerp args)
                (` (lyskom-filter-is-member
                    =
                    (,  args)
                    recipient-list
                    conf-stat->conf-no)))

               ((eq key 'subject)
                (lyskom-filter-check-args 'stringp args)
                (` (string-match (, (regexp-quote args))
                                 subject)))
               ((eq key 'subject-re)
                (lyskom-filter-check-args 'regexpp args)
                (` (string-match (, args)
                                 subject)))

               ((eq key 'text)
                (lyskom-filter-check-args 'stringp args)
                (` (string-match (, (regexp-quote args))
                                 text)))
               ((eq key 'text-re)
                (lyskom-filter-check-args 'regexpp args)
                (` (string-match (, args)
                                 text)))
               (t (lyskom-error 
                   (lyskom-get-string 'filter-error-unknown-key)
                   key
                   ))))
        
        (if inverse (setq form (list 'not form)))
        (cons form (lyskom-create-filter-function-body (cdr pattern))))))))

      

(defun lyskom-filter-check-args (fn arg)
  (if (not (funcall fn arg))
      (lyskom-error (lyskom-get-string 'filter-error-key-arg)
                    fn arg)))

(defun regexpp (re)
  (let ((result t))
    (save-match-data
      (condition-case nil
          (string-match re "")
        (error (setq result nil))))
    result))




;;;========================================
;;;  I don't actually remember what these are for
;;;

(defun lyskom-filter-prompt (text-no prompt)
  (setq lyskom-filter-hack t)
  (let ((text-stat (blocking-do 'get-text-stat text-no))
	(subject nil))
    (if text-stat
	(blocking-do-multiple 
	 ((text (get-text text-no))
	  (conf-stat (get-conf-stat 
		      (text-stat->author text-stat))))
	 (if text
	     (progn
	       (setq subject
		     (if (string-match "\n" (text->text-mass text))
			 (substring (text->text-mass text) 
				    0
				    (match-beginning 0))
		       (text->text-mass text)))
	       (lyskom-format-insert prompt
				     text-stat
				     subject
				     (or conf-stat
					 (text-stat->author text-stat)))
	       (lyskom-scroll))))))
  (setq lyskom-filter-hack nil))



;;;========================================
;;; User functions and support functions
;;;

(defun lyskom-add-filter (filter)
  "Add the filter FILTER to the LysKOM filtering mechanism."
  (if (filter->attribute filter 'expire)
      (setq kom-session-filter-list (cons filter
                                          kom-session-filter-list))
    (progn
      (setq kom-permanent-filter-list (cons filter kom-permanent-filter-list))
      (lyskom-save-options (current-buffer)
                           (lyskom-get-string 'filter-edit-saving)
                           (lyskom-get-string 'filter-edit-saving-done)
                           (lyskom-get-string 'filter-edit-saving-error))))
  (setq lyskom-filter-list
        (cons filter lyskom-filter-list)))


(defun lyskom-filter-read-action ()
  "Read a filter action from the minibuffer, returning its symbol"
  (let ((completion-ignore-case t))
    (car
     (rassoc
      (completing-read (lyskom-get-string 'filter-action)
                       (lyskom-reverse-pairs 
                        (if kom-emacs-knows-iso-8859-1
                            lyskom-filter-actions
                          lyskom-swascii-filter-actions))
                       nil
                       nil
                       (cdr (car (if kom-emacs-knows-iso-8859-1
                                     lyskom-filter-actions
                                   lyskom-swascii-filter-actions)))
                       t)
      (if kom-emacs-knows-iso-8859-1
          lyskom-filter-actions
        lyskom-swascii-filter-actions)))))


(defun lyskom-filter-read-permanent ()
  "Ask the user is a filter is permanent and return t in this case.
Otherwise return nil."
  (lyskom-j-or-n-p (lyskom-get-string 'filter-permanent)))



;;;========================================
;;; Filtrera ärende --- Filter subject
;;; 

(defun kom-filter-subject (&optional subject)
  "Interactively filter a subject. Optional SUBJECT is subject to filter."
  (interactive)
  (lyskom-start-of-command 'kom-filter-subject)
  (unwind-protect
      (if (/= 0 lyskom-current-conf)
	  (let ((conf-stat (blocking-do 'get-conf-stat lyskom-current-conf)))
	    (let (conf perm filter action)
	      (if (null subject)
		  (setq subject lyskom-current-subject))
	      (setq subject 
		    (read-from-minibuffer (lyskom-get-string 'filter-subject)
					  subject))
	      (setq filter (cons (cons 'subject subject) filter))
	      (setq conf (lyskom-read-conf-no
			  (lyskom-get-string 'filter-in-conf)
			  'all
			  t
			  (or (and (conf-stat->conf-no conf-stat)
				   (conf-stat->name conf-stat))
			      "")
			  t))
	      (if (/= conf 0)
		  (setq filter (cons (cons 'recipient-no conf) filter)))
	      (setq action (lyskom-filter-read-action))
	      (setq perm (lyskom-filter-read-permanent))
	      
	      (lyskom-add-filter
	       (make-filter filter
			    (list (cons 'action action)
				  (cons 'expire (not perm))))))))
    (lyskom-end-of-command)))
  

     
;;;========================================
;;; Filtrera författare --- Filter author
;;;

(defun kom-filter-author ()
  "Interactively filter an author."
  (interactive)
  (lyskom-start-of-command 'kom-filter-author)
  (unwind-protect
      (let (auth-stat author conf filter action permanent)
	(blocking-do-multiple ((text-stat (get-text-stat 
					   (or lyskom-current-text 0)))
			       (conf-stat (get-conf-stat
					   lyskom-current-conf)))
           (if text-stat
	       (setq auth-stat (blocking-do 'get-conf-stat
					    (text-stat->author text-stat))))
	   (setq author 
		 (lyskom-read-conf-no (lyskom-get-string 'filter-author)
				      'pers
				      t
				      (or (and auth-stat
					       (conf-stat->name auth-stat))
					  "")
				      t))
	   (if (/= author 0)
	       (setq filter (cons (cons 'author-no author) filter)))
	   (setq conf (lyskom-read-conf-no
		       (lyskom-get-string 'filter-in-conf)
		       'all
		       t
		       (or 
			(and conf-stat
			     (conf-stat->name conf-stat))
			"")
		       t))
	   (if (/= conf 0)
	       (setq filter (cons (cons 'recipient-no conf) filter)))
	   (setq action (lyskom-filter-read-action))
	   (setq permanent (lyskom-filter-read-permanent))
	   (lyskom-add-filter
	    (make-filter filter
			 (list (cons 'action action)
			       (cons 'expire (not permanent)))))))
    (lyskom-end-of-command)))




;;;============================================================
;;;
;;; Superhoppa
;;;

(defun kom-super-jump ()
  "Skip all texts and comments that share the subject and recipient of 
the current text"
  (interactive)
  (lyskom-start-of-command 'kom-super-jump)
  (unwind-protect
      (if (zerop lyskom-current-conf)
          (progn (lyskom-insert-string 'no-in-conf)
                 (lyskom-end-of-command))
        (if (or (null lyskom-current-text)
                (null lyskom-current-subject))
            (progn (lyskom-insert-string 'have-to-read)
                   (lyskom-end-of-command))
	  (let ((conf-stat (blocking-do 'get-conf-stat lyskom-current-conf)))
	    (if conf-stat
		(progn
		  (lyskom-add-filter
		   (make-filter
		    (list (cons 'subject lyskom-current-subject)
			  (cons 'recipient-no (conf-stat->conf-no conf-stat)))
		    (list (cons 'action 'skip-tree)
			  (cons 'expire t))))
		  (lyskom-format-insert 'super-jump
					lyskom-current-subject
					conf-stat))))))
    (lyskom-end-of-command)))




;;;============================================================
;;;
;;; Filtrera text
;;;

(defun kom-filter-text (&optional text)
  "Interactively filter on text contents. Optional TEXT is subject to filter."
  (interactive)
  (lyskom-start-of-command 'kom-filter-text)
  (unwind-protect
      (if (/= 0 lyskom-current-conf)
	  (let ((conf-stat (blocking-do 'get-conf-stat lyskom-current-conf))
		(conf nil)
		(action nil)
		(perm nil)
		(filter nil))
	    (if conf-stat
		(progn
		  (setq text 
			(read-from-minibuffer (lyskom-get-string 
					       'filter-which-text)
					      (or text "")))
		  (setq filter (cons (cons 'text text) filter))
		  (setq conf (lyskom-read-conf-no
			      (lyskom-get-string 'filter-in-conf)
			      'all t
			      (or (and (conf-stat->conf-no conf-stat)
				       (conf-stat->name conf-stat))
				  "")
			      t))
		  (if (/= conf 0)
		      (setq filter (cons (cons 'recipient-no conf) filter)))
		  (setq action (lyskom-filter-read-action))
		  (setq perm (lyskom-filter-read-permanent))
		  (lyskom-add-filter
		   (make-filter filter
				(list (cons 'action action)
				      (cons 'expire (not perm)))))))))


    (lyskom-end-of-command)))

  




;;;============================================================
;;; Lista filter                         (kom-list-filters)
;;;
;;; Author: David Byers
;;; Calls internal functions in filter-edit mode. This may or
;;; may not be a good idea, but it works...

(defun kom-list-filters ()
  "Display all filters"
  (interactive)
  (lyskom-start-of-command 'kom-view-filters)
  (save-excursion
    (unwind-protect
        (let ((filters lyskom-filter-list)
              (filter nil))
          (goto-char (point-max))
          (if (null filters)
              (lyskom-insert (lyskom-get-string 'no-filters))
            (progn
              (lyskom-insert (lyskom-get-string 'view-filters-header))
              (while filters
                (goto-char (point-max))
                (lyskom-format-filter-pattern (car filters))
                (setq filters (cdr filters)))
              (lyskom-insert (lyskom-get-string 'view-filters-footer)))))
      (progn (lyskom-insert "\n"))))
  (lyskom-end-of-command))

