;;;;;
;;;;; $Id: filter.el,v 38.1 1995-03-01 17:55:51 byers Exp $
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
;;;; File: ignore.el
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

;;;============================================================
;;;
;;; Filter pattern accessors
;;;
;;; Filters are lists with the following structure
;;;
;;;   (filter [ PATTERNS ATTRIBUTES])
;;;
;;; Where PATTERNS is a list of filter patterns and ATTRIBUTES is
;;; an association list.
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
  (list 'filter (vector p a)))

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
      (filter->attribute lyskom-filter-hack 'action))))
    
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
    (rplacd (nthcdr (- (length data) 2) data) nil)

    ;;
    ;; Do the checking
    ;;
    (setq lyskom-filter-hack
          (lyskom-traverse-filter-list text-stat
                                       author
                                       data
                                       subject
                                       (text->text-mass text)
                                       lyskom-filter-list))))

(defun lyskom-traverse-filter-list (text-stat
                                    author
                                    recipient-list
                                    subject
                                    text
                                    filter-list)
  (cond 
   ;;
   ;; End of filter list
   ;;
   ((null filter-list) nil)

   ;;
   ;; Error in filter list
   ;;
   ((not (listp filter-list))
    (lyskom-bad-filter-list filter-list))

   ;;
   ;; Not a filter at head of list
   ;;
   ((not (filter-p (car filter-list)))
    (lyskom-bad-filter-list filter-list))

   ;;
   ;; Assume valid list and filter at head
   ;;
   (t (or
       (lyskom-traverse-filter-pattern text-stat
                                       author
                                       recipient-list
                                       subject
                                       text
                                       (filter->pattern
                                        (car filter-list))
                                       (car filter-list))
       (lyskom-traverse-filter-list text-stat
                                    author
                                    recipient-list
                                    subject
                                    text
                                    (cdr filter-list))))))


(defun lyskom-traverse-filter-pattern (text-stat
                                       author
                                       recipient-list
                                       subject
                                       text
                                       pat
                                       filter)
  (let (inverse tmp)
    (cond 
     ;;
     ;; End of pattern
     ;;
     ((null pat) filter)

     ;;
     ;; Bad pattern
     ;;
     ((or (not (listp pat))
          (not (listp (car pat))))
      (lyskom-bad-filter-list pat))

     ;;
     ;; Assume valid pattern
     ;;
     (t 
      ;;
      ;; Key is the pattern key
      ;; Args is the pattern arguments
      ;;
      (let ((key (car (car pat)))
            (args (cdr (car pat))))
        ;;
        ;; If key is NOT, rebind key and args and note the negation is 
        ;; in effect.
        ;;
        (if (eq key 'not)
            (if (not (listp args))
                (progn
                  (lyskom-bad-filter-list pat)
                  (setq key 'dummy)
                  (setq args nil)
                  (setq inverse nil))
              (progn 
                (setq key (car args))
                (setq args (cdr args))
                (setq inverse t))))
        (and
         (progn 
           (setq tmp
                 (cond
                  ((or (eq key 'author) (eq key 'author-re))
                   (if (not (null author))
                       (if (not (stringp args))
                           (progn (setq inverse nil)
                                  (lyskom-bad-filter-list pat))
                         (string-match 
                          (if (eq key 'author-re)
                              args 
                            (regexp-quote args))
                          (conf-stat->name author)))))
                  ((eq key 'author-no)
                   (if (not (null author))
                       (if (not (numberp args))
                           (progn (setq inverse nil)
                                  (lyskom-bad-filter-list pat))
                         (= (conf-stat->conf-no author) args))))
                  ((or (eq key 'recipient)
                       (eq key 'recipient-re))
                   (if (not (null recipient-list))
                       (if (not (stringp args))
                           (progn (setq inverse nil)
                                  (lyskom-bad-filter-list pat))
                         (lyskom-filter-match-recipients recipient-list
                                                         (if (eq key
                                                                 'recipient-re)
                                                             args
                                                           (regexp-quote args))
                                                         'name))))
                  ((eq key 'recipient-no)
                   (if (not (null recipient-list))
                       (if (not (numberp args))
                           (progn (setq inverse nil)
                                  (lyskom-bad-filter-list pat))
                         (lyskom-filter-match-recipients recipient-list
                                                         args
                                                         'number))))
                  ((or (eq key 'subject)
                       (eq key 'subject-re))
                   (if (not (stringp args))
                       (progn (setq inverse nil)
                              (lyskom-bad-filter-list pat))
                     (string-match (if (eq key 'subject-re)
                                       args
                                     (regexp-quote args)) 
                                   subject)))
                  ((or (eq key 'text)
                       (eq key 'text-re))
                   (if (not (stringp args))
                       (progn (setq inverse nil)
                              (lyskom-bad-filter-list pat))
                     (string-match (if (eq key 'text-re)
                                       args
                                     (regexp-quote args))
                                   text)))
                  ((eq key 'dummy)
                   (setq inverse nil)
                   nil)
                  (t (setq inverse nil)
                     (lyskom-bad-filter-list pat))))
           (if inverse (not tmp) tmp))
         (lyskom-traverse-filter-pattern text-stat
                                         author
                                         recipient-list
                                         subject
                                         text
                                         (cdr pat)
                                         filter)))))))



(defun lyskom-filter-match-recipients (recipient-list pat key)
  (cond ((null recipient-list) nil)
        ((eq key 'name)
         (or 
          (string-match pat (conf-stat->name (car recipient-list)))
          (lyskom-filter-match-recipients (cdr recipient-list) pat key)))
        ((eq key 'number)
         (or 
          (= pat (conf-stat->conf-no (car recipient-list)))
          (lyskom-filter-match-recipients (cdr recipient-list)
                                          pat
                                          key)))))

(defun lyskom-bad-filter-list (arg)
  (lyskom-message (lyskom-get-string 'bad-filter-list)
                  arg)
  (sit-for 1)
  nil)


(defun lyskom-filter-prompt (text-no prompt)
  (setq lyskom-filter-hack t)
  (lyskom-collect 'filter)
  (initiate-get-text-stat 'filter nil text-no)
  (initiate-get-text 'filter nil text-no)
  (lyskom-use 'filter 'lyskom-filter-prompt-2 prompt)
  (while lyskom-filter-hack
    (accept-process-output)))


(defun lyskom-filter-prompt-2 (text-stat text prompt)
  (if (and text-stat text)
      (initiate-get-conf-stat 'filter 
                              'lyskom-filter-prompt-3
                              (text-stat->author text-stat)
                              text-stat text prompt)))

(defun lyskom-filter-prompt-3 (conf-stat text-stat text prompt)
  (let (author subject string start)
    (if conf-stat
        (setq author (conf-stat->name conf-stat))
      (setq author (lyskom-format 'person-does-not-exist 
                                  (text-stat->author text-stat))))
    (setq subject
          (progn (string-match "\n" (text->text-mass text))
                 (substring (text->text-mass text) 0 (match-beginning 0))))
    (setq string
          (format (lyskom-get-string prompt)
                  (text-stat->text-no text-stat)
                  subject
                  author))
    (if (null conf-stat)
	(lyskom-insert string)
;;        (lyskom-insert-with-button string 
;;                                    (int-to-string 
;;                                     (text-stat->text-no text-stat))
;;                                    'lyskom-button-view-text
;;                                    (text-stat->text-no text-stat))
      (lyskom-insert string))
;;      (lyskom-insert-with-button string 
;;                                  (int-to-string 
;;                                   (text-stat->text-no text-stat))
;;                                  'lyskom-button-view-text
;;                                  (text-stat->text-no text-stat)
;;                                  (regexp-quote author)
;;                                  'lyskom-button-view-pres
;;                                  (conf-stat->presentation conf-stat)))
    (setq lyskom-filter-hack nil)))
                              


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
			      "")))
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
					  "")))
	   (if (/= author 0)
	       (setq filter (cons (cons 'author-no author) filter)))
	   (setq conf (lyskom-read-conf-no
		       (lyskom-get-string 'filter-in-conf)
		       'all
		       t
		       (or 
			(and conf-stat
			     (conf-stat->name conf-stat))
			"")))
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
		  (lyskom-insert
		   (lyskom-format 'super-jump
				  lyskom-current-subject
				  (conf-stat->name conf-stat))))))))
    (lyskom-end-of-command)))




;;;============================================================
;;;
;;; Filtrera text
;;;

(defun kom-filter-text (&optional text)
  "Interactively filter a subject. Optional TEXT is subject to filter."
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
			(read-from-minibuffer (lyskom-get-string 'filter-text)
					      (or text "")))
		  (setq filter (cons (cons 'text text) filter))
		  (setq conf (lyskom-read-conf-no
			      (lyskom-get-string 'filter-in-conf)
			      'all t
			      (or (and (conf-stat->conf-no conf-stat)
				       (conf-stat->name conf-stat))
				  "")))
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
                (lyskom-format-filter-pattern (car filters))
                (setq filters (cdr filters)))
              (lyskom-insert (lyskom-get-string 'view-filters-footer)))))
      (progn (lyskom-insert "\n")
	     (lyskom-end-of-command)))))

