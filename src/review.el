;;;;;
;;;;; $Id: review.el,v 39.1 1996-03-18 15:43:24 byers Exp $
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
;;;; File: review.el
;;;;
;;;; This file contains functions to review articles in different ways.
;;;; Both the review commands themselves, the functions called by them
;;;; and the underlying functions are here.
;;;;
;;;; Most, if not all, of these functions are written by Linus Tolke.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: review.el,v 39.1 1996-03-18 15:43:24 byers Exp $\n"))



(defun lyskom-intersection (a b)
  "Returns as a list the intersection of list A and list B.
The order of the list a is kept."
  (let ((list (list)))
    (while a
      (if (memq (car a) b)
	  (setq list (cons (car a) list)))
      (setq a (cdr a)))
    (nreverse list)))

(defun lyskom-remove-zeroes (a)
  "Returns a copy of list where all zeroes are removed."
  (delq 0 (copy-sequence a)))


;;; ================================================================
;;;              ]terse av, till - Review by X to Conference Y.

;;; Author: Linus Tolke



(def-kom-command kom-review-by-to (&optional count)
  "Reviews all articles of author that is written to conference recipient.
If return is given instead of an author then all authors to that conference is
shown. If return is given instead of conference then all conferences for that
person is chosen.
If a positive numeric argument is given then only the last COUNT articles are 
chosen. If the argument is negative then the first -COUNT articles are chosen.
If the argument is zero the all articles are chosen.
No argument is equivalent to COUNT 1.
The defaults for this command is the conference that you are in."
  (interactive "p")
  (lyskom-tell-internat 'kom-tell-review)
  (let* ((info (progn
		 (if (and (listp count)
			  (integerp (car count))
			  (null (cdr count)))
		     (setq count (car count)))
		 (cond
		  ((zerop count) 
		   (setq count nil)
		   (lyskom-get-string 'everybody))
		  ((> count 0)
		   (lyskom-format 'latest-n count))
		  ((< count 0)
		   (lyskom-format 'first-n (- count))))))
	 (by (lyskom-read-conf-no (lyskom-format 'info-by-whom info)
				  'pers 'empty nil t))
	 (to (lyskom-read-conf-no (lyskom-format 'info-to-conf info)
				  'all 
				  ;; If person is not given we must give
				  ;; conf
				  (not (zerop by))
				  (if (zerop lyskom-current-conf)
				      ""
				    (conf-stat->name
				     (blocking-do 'get-conf-stat
						  lyskom-current-conf)))
				  t)))

    ;; Since we fetch everything anyway we don't need to do this.  If
    ;; we later choose to fetch all in small chunks we will have to do
    ;; this then.
    (if (not (zerop to))
	(cache-del-conf-stat to))
    (if (not (zerop by)) 
	(cache-del-pers-stat by))
    (let* ((info-by (if (zerop by) 
			(lyskom-get-string 'anybody)
		      (blocking-do 'get-conf-stat by)))
	   (info-to (if (zerop to)
			(lyskom-get-string 'all-confs)
		      (blocking-do 'get-conf-stat to))))
      (lyskom-format-insert 'review-info-by-to
			    info
			    info-by
			    info-to))

    (let ((list (lyskom-get-texts-by-to by to count)))
      (if list
	  (read-list-enter-read-info (lyskom-create-read-info
				      'REVIEW
				      nil
				      (lyskom-get-current-priority)
				      (lyskom-create-text-list list)
				      nil t)
				     lyskom-reading-list t)
      	(lyskom-insert-string 'no-such-text)))))

	
(defun lyskom-get-texts-by-to (by to count)
  "Get a list of COUNT text numbers written by BY in conference TO."

  ;; THIS FUNCTION DOES NOT WORK AT THE MOMENT
  
  ;; Now we have 
  ;; - the person number in by
  ;; - the conf number in to
  ;; - the number of interesting texts in count (if negative, then 
  ;;   count from the beginning.
  ;;
  ;; What we have to do is fetch and merge the list of texts until we 
  ;; have found count texts.
  
  ;; Lets do the very simple thing: 
  ;; - Fetch the whole lists. If its to slow, fix it!
  ;;   (the calls wont get very big, at least not during get-map
  ;;    because the initiate-get-map itself splits the call) (sure?)
  
;;;  I'll finish this some day /davidk
;;;  In the meantime we'll use the old, inefficient code
;;;  
;;;  (let* ((texts nil)
;;;	 (exhausted nil)
;;;	 (persstat (if (zerop by) nil (blocking-do 'get-pers-stat by)))
;;;	 (confstat (if (zerop to) nil (blocking-do 'get-conf-stat to)))
;;;	 (phigh (pers-stat->no-of-created-texts persstat))
;;;	 (plow (pers-stat->first-created-text persstat))
;;;	 (pmark (if persstat (if (< count 0) plow phigh)))
;;;	 (chigh (conf-stat->no-of-texts confstat))
;;;	 (clow (conf-stat->first-local-no confstat))
;;;	 (cmark (if confstat (if (< count 0) clow chigh))))
    
;;;    (while (and (not exhausted)
;;;		(not (zerop count)))
;;;      (let* ((found-by (or (zerop by)
;;;			   (lyskom-remove-zeroes
;;;			    (listify-vector
;;;			     (map->text-nos
;;;			      (if (< count 0)
;;;				  (blocking-do 'get-created-texts by
;;;					       pmark (+ pmark 10))
;;;				(blocking-do 'get-created-texts by
;;;					     (- pmark 10) pmark)))))))
;;;	     (found-to (or (zerop to)
;;;			   (lyskom-remove-zeroes
;;;			    (listify-vector
;;;			     (map->text-nos
;;;			      (if (< count 0)
;;;				  (blocking-do 'get-map to
;;;					       cmark (+ cmark 10))
;;;				(blocking-do 'get-map to
;;;					     (- cmark 10) cmark)))))))
;;;	     (list (cond
;;;		    ((zerop by) found-to)
;;;		    ((zerop to) found-by)
;;;		    ;; This will get them in the correct order
;;;		    (t (lyskom-intersection found-by found-to)))))
		     
;;;	;; Cut out the part we want, the beginning or the end...
;;;	(cond
;;;	 ((> count 0)
;;;	  (while (> (length list) count)
;;;	    (setq list (cdr list)))
;;;	  (setq count (- count (length list))
;;;		pmark (- pmark 10)
;;;		cmark (- cmark 10)
;;;		texts (nconc texts list)))
;;;	 ((< count 0)
;;;	  (setq list (nfirst (- count) list))
;;;	  (setq count (+ count (length list))
;;;		pmark (+ pmark 10)
;;;		cmark (+ cmark 10)
;;;		texts (nconc list texts))))
;;;	(setq exhausted (and (or (< pmark plow) (< phigh pmark))
;;;			     (or (< cmark clow) (< chigh cmark))))))
;;;    texts)

  (let* ((found-by (or (zerop by)
		       (lyskom-remove-zeroes
			(append
			 (map->text-nos
			  (blocking-do 'get-created-texts by
				       0 lyskom-max-int)) nil))))
	 (found-to (or (zerop to)
		       (lyskom-remove-zeroes
			(append
			 (map->text-nos
			  (blocking-do 'get-map to
				       0 lyskom-max-int)) nil))))
	 (list (cond
		((zerop by) found-to)
		((zerop to) found-by)
		;; This will get them in the correct order
		(t (lyskom-intersection found-by found-to)))))
		     
    (if list
	(progn
	  ;; Cut out the part we want, the beginning or the end...
	  (cond
	   ((> count 0)
	    (while (> (length list) count)
	      (setq list (cdr list))))
	   ((< count 0)
	    (setq list (nfirst (- count) list))))
	  (read-list-enter-read-info
	   (lyskom-create-read-info
	    'REVIEW
	    nil
	    (lyskom-get-current-priority)
	    (lyskom-create-text-list list)
	    nil t)
	   lyskom-reading-list t))
      (lyskom-insert-string 'no-such-text))))
	


(defun kom-review-backward ()
  "Toggles the reviewing order.
If reading forward then starts reading backward and the other way round."
  (interactive)
  (lyskom-start-of-command 'kom-review-backward)
  (cond
   ((and (not (read-list-isempty lyskom-reading-list))
	 (or (eq (read-info->type (read-list->first lyskom-reading-list))
		 'REVIEW)
	     (eq (read-info->type (read-list->first lyskom-reading-list))
		 'REVIEW-MARK)))
    (let* ((info (read-list->first lyskom-reading-list))
	   (list (read-info->text-list info))
	   (texts (cdr list))
	   (forward (read-info->forward info)))
      (setcdr list (nreverse (cdr list)))
      (set-read-info->forward info (not forward))
      (lyskom-format-insert 'you-review 
			    (lyskom-get-string (if (not forward)
						   'forward
						 'backward)))))
   (t
    (lyskom-insert-string 'illegal-command)))
  (lyskom-end-of-command)) 


;;; ================================================================
;;;                   ]terse tr{det - review tree

;;; Author: Linus Tolke


(defun kom-review-tree (&optional text-no)
  "Review all comments to this text.
Descends recursively in the comment-tree without marking the texts as read.
The tree is forgotten when a kom-go-to-next-conf command is issued.
If optional prefix argument TEXT-NO is present view tree from that text 
instead. In this case the text TEXT-NO is first shown." 
  (interactive (list
		(cond
		 ((null current-prefix-arg)
		  lyskom-current-text)
		 ((integerp current-prefix-arg)
		  current-prefix-arg)
		 (t
		  (signal 'lyskom-internat-error '(kom-review-tree))))))
  (lyskom-start-of-command 'kom-review-tree)
  (unwind-protect
      (progn
	(lyskom-tell-internat 'kom-tell-review)
	(if text-no
	    (let ((ts (blocking-do 'get-text-stat text-no)))
	      (lyskom-follow-comments ts
				      nil 'review
				      (lyskom-get-current-priority)
				      t))
	  (lyskom-insert-string 'read-text-first)))
    (lyskom-end-of-command)))


(defun kom-find-root (&optional text-no)
  "Finds the root text of the tree containing the text in lyskom-current-text."
  (interactive)
  (lyskom-start-of-command 'kom-find-root)
  (unwind-protect
      (progn
	(lyskom-tell-internat 'kom-tell-review)
	(cond
	 (lyskom-current-text 
	  (let* ((ts (blocking-do 'get-text-stat (or text-no 
						     lyskom-current-text)))
		 (r (lyskom-find-root ts ts)))
	    (if r
		(lyskom-view-text r)
	      (signal 'lyskom-internal-error "Could not find root"))
	    )
	  )
	 (t
	  (lyskom-insert-string 'read-text-first))))
    (lyskom-end-of-command)))


(def-kom-command kom-find-root-review ()
  "Finds the root text of the tree containing the text in lyskom-current-text and
reviews the whole tree in deep-first order."
  (interactive)
  (lyskom-tell-internat 'kom-tell-review)
  (cond
   (lyskom-current-text
    (lyskom-review-tree
     (lyskom-find-root (blocking-do 'get-text-stat lyskom-current-text)
		       nil)))
   (t
    (lyskom-insert-string 'read-text-first))))


(defun lyskom-find-root (text-stat old-text-stat)
  "Finds the root text of the tree containing the text TEXT-STAT.
Args: TEXT-STAT OLD-TEXT-STAT THENDO
If TEXT-STAT is nil and OLD-TEXT-STAT contains a text-stat
then this means that the parent of the text in OLD-TEXT-STAT is not readable
and the text in OLD-TEXT-STAT is to be used instead.
If both TEXT-STAT and OLD-TEXT-STAT is nil then this means we are not allowed
to read the text we are trying to find the root of. This just returns with a
message."
  (let* ((ts text-stat)
	 (misclist (and ts (text-stat->misc-info-list ts)))
	 (res nil))
    (cond
     (ts				;+++ Smartare errorhantering hit.
      (setq res 'noparents)
      (while misclist
	(let* ((type (misc-info->type (car misclist)))
	       (comm (eq type 'COMM-TO))
	       (yes (or comm
			(eq type 'FOOTN-TO)))
	       (parent-no (if comm
			      (misc-info->comm-to (car misclist))
			    (misc-info->footn-to (car misclist)))))
	  (cond
	   (yes
	    (let* ((pts (blocking-do 'get-text-stat parent-no)))
	      (if (setq res (lyskom-find-root pts ts))
		  (setq misclist nil))))
	   (t
	    (setq misclist (cdr misclist))))))
      (if (eq res 'noparents) (text-stat->text-no ts) res))
     (old-text-stat
      (text-stat->text-no old-text-stat))
     (t
      nil))))


(defun lyskom-review-tree (text)
  "Takes a TEXT as an arg, shows the text and the tree of all comments.
Does a lyskom-end-of-command.
Text is a text-no."
  (cond
   ((integerp text)
    (lyskom-view-text text nil t nil (lyskom-get-current-priority) t))
   (t
    (signal 'lyskom-internal-error
	    (list 'lyskom-review-tree
		  "Called with incorrect argument."
		  text)))))


;;; ================================================================
;;;                     ]terse n{sta -  review next

;;; Author: Linus Tolke


(defun kom-review-next ()
  "Resumes an interupted review by moving all review and review-tree entries in
the lyskom-reading-list to the beginning. i.e by moving all other types to the
end."
  (interactive)
  (lyskom-start-of-command 'kom-review-next)
  (lyskom-tell-internat 'kom-tell-review)
  (let ((len (read-list-length lyskom-reading-list))
	(finished nil))
    (while (and (not finished)
		(> len 1))
      (let ((type (read-info->type (read-list->first lyskom-reading-list))))
	(if (and (not (eq type 'REVIEW))
		 (not (eq type 'REVIEW-TREE))
		 (not (eq type 'REVIEW-MARK)))
	    (read-list-rotate lyskom-reading-list)
	  (setq finished t)))
      (setq len (1- len))))
  (lyskom-end-of-command))


;;; ================================================================
;;;                    ]terse stacken - Review stack

;;; Author: Linus


(defun kom-review-stack ()
  "Displays the review-stack."
  (interactive)
  (lyskom-start-of-command 'kom-review-stack)
  (mapcar
   (function
    (lambda (info)
      (let ((un (length (cdr (read-info->text-list info))))
	    (type (read-info->type info))
	    (cto (read-info->comm-to info)))
	(cond
	 ((eq type 'REVIEW)
	  (lyskom-format-insert 'review-n-texts un))
	 ((eq type 'REVIEW-TREE)
	  ; +++ Hmmm. Pluralformer. Besv{rligt!
	  (if (= un 1)
	      (lyskom-format-insert 'review-one-comment cto)
	    (lyskom-format-insert 'review-many-comments cto un)))
	 ((eq type 'REVIEW-MARK)
	  (lyskom-format-insert 'review-marked un))))))
   (read-list->all-entries lyskom-reading-list))
  (lyskom-end-of-command))


;;; ================================================================
;;;                      ]terse hoppa - review clear

;;; Author: Linus Tolke


(defun kom-review-clear ()
  "Deletes all review-types from the lyskom-reading-list and lyskom-to-do-list."
  (interactive)
  (lyskom-start-of-command 'kom-review-clear)
  (if (not (read-list-isempty lyskom-reading-list))
      (while (or (eq (read-info->type (read-list->first lyskom-reading-list))
		     'REVIEW)
		 (eq (read-info->type (read-list->first lyskom-reading-list))
		     'REVIEW-TREE)
		 (eq (read-info->type (read-list->first lyskom-reading-list))
		     'REVIEW-MARK))
	(set-read-list-del-first lyskom-reading-list)))
  (if (not (read-list-isempty lyskom-to-do-list))
      (while (or (eq (read-info->type (read-list->first lyskom-to-do-list))
		     'REVIEW)
		 (eq (read-info->type (read-list->first lyskom-to-do-list))
		     'REVIEW-TREE)
		 (eq (read-info->type (read-list->first lyskom-to-do-list))
		     'REVIEW-MARK))
	(set-read-list-del-first lyskom-to-do-list)))
  (lyskom-end-of-command))


;;; ================================================================
;;;          ]terse det kommenterade - View commented text

;;; Author: Inge Wallin


(defun kom-review-comments ()
  "View the comments to this text.
If the current text has comments in (footnotes in) some texts then the first
text is shown and a REVIEW list is built to shown the other ones."
  (interactive)
  (lyskom-start-of-command 'kom-review-comments)
  (lyskom-tell-internat 'kom-tell-review)
  (initiate-get-text-stat 'read 'lyskom-review-comments
			  lyskom-current-text))


(defun lyskom-review-comments (text-stat)
  "Handles the return from the initiate-get-text-stat, displays and builds list."
  (let* ((misc-info-list (and text-stat
			      (text-stat->misc-info-list text-stat)))
	 (misc-infos (and misc-info-list
			  (append (lyskom-misc-infos-from-list 
				   'COMM-IN misc-info-list)
				  (lyskom-misc-infos-from-list 
				   'FOOTN-IN misc-info-list))))
	 (text-nos (and misc-infos
			(mapcar
			 (function
			  (lambda (misc-info)
			    (if (equal (misc-info->type misc-info)
				       'COMM-IN)
				(misc-info->comm-in misc-info)
			      (misc-info->footn-in misc-info))))
			 misc-infos))))
    (if text-nos
	(progn
	  (lyskom-format-insert 'review-text-no (car text-nos))
	  (if (cdr text-nos)
	      (read-list-enter-read-info
	       (lyskom-create-read-info
		'REVIEW nil (lyskom-get-current-priority)
		(lyskom-create-text-list (cdr text-nos))
		lyskom-current-text)
	       lyskom-reading-list t))
	  (lyskom-view-text (car text-nos)))
      (lyskom-insert-string 'no-such-text)))
  (lyskom-run 'main 'lyskom-end-of-command))


;;; ================================================================
;;;          ]terse igen - kom-review-last-normally-read
;;;
;;; Author: Linus Tolke


(defun kom-review-last-normally-read (no)
  "Reviews the NO last normally read texts."
  (interactive 
   (list 
    (lyskom-read-number (lyskom-get-string 'read-normally-read) 1)))
  (lyskom-start-of-command 'kom-review-last-normally-read)
  (lyskom-tell-internat 'kom-tell-review)
  (let* ((text-nos (reverse (nfirst no lyskom-normally-read-texts))))
    (if text-nos
	(progn
	  (lyskom-format-insert 'review-text-no (car text-nos))
	  (if (cdr text-nos)
	      (read-list-enter-read-info
	       (lyskom-create-read-info
		'REVIEW nil (lyskom-get-current-priority)
		(lyskom-create-text-list (cdr text-nos))
		lyskom-current-text)
	       lyskom-reading-list t))
	  (lyskom-view-text (car text-nos)))
      (lyskom-format-insert 'no-such-text)))
  (lyskom-end-of-command))
      
(defun nfirst (n list)
  "Return a list of the N first elements of LIST."
  (if (or (<= n 0) (not list))
      nil
    (cons (car list) (nfirst (1- n) (cdr list)))))
