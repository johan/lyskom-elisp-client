;;;;;
;;;;; $Id: review.el,v 36.1 1993-04-26 19:37:59 linus Exp $
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
	      "$Id: review.el,v 36.1 1993-04-26 19:37:59 linus Exp $\n"))


;;; ================================================================
;;;              ]terse av, till - Review by X to Conference Y.

;;; Author: Linus Tolke


(defun kom-review-by-to (&optional count)
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
  (lyskom-start-of-command 'kom-review-by-to)
  (if (and (listp count)
	   (integerp (car count))
	   (null (cdr count)))
      (setq count (car count)))
  (let ((info (cond
	       ((zerop count) 
		(setq count nil)
		(lyskom-get-string 'everybody))
	       ((> count 0)
		(lyskom-format 'latest-n count))
	       ((< count 0)
		(lyskom-format 'first-n count)))))
    (lyskom-completing-read 'review 'lyskom-review-by-to-get-default-for-conf
			    (lyskom-format 'info-by-whom info)
			    'person 'empty "" count info)))


(defun lyskom-review-by-to-get-default-for-conf (pers-no count info)
  "Collects the PERS-NO from lyskom-completing-read and calls for conf-stat.
This conf-stat is needed as the default for the conference."
  (if (zerop lyskom-current-conf)
      (lyskom-review-by-to-person nil pers-no count info)
    (initiate-get-conf-stat 'review 'lyskom-review-by-to-person
			    lyskom-current-conf
			    pers-no count info)))


(defun lyskom-review-by-to-person (conf-stat pers-no count info)
  "Collects the info from the get-conf-stat that gets the default.
Then procedes to read the conf-no for the requested conf.
Args: DEFAULT-CONF-STAT, PERS-NO, COUNT and INFO."
  (lyskom-completing-read 'review 'lyskom-review-by-to-conf
			  (lyskom-format 'info-to-conf info)
			  nil 'empty 
			  (if conf-stat ;+++ annan felhantering.
			      (conf-stat->name conf-stat)
			    "")
			  count info pers-no))

			  
(defun lyskom-review-by-to-conf (conf-no count info pers-no)
  "Has all info about users request of review.
Gets the conf-stat of the conf CONF-NO, sends the COUNT and INFO together with
the fetched pers-stat for the author PERS-NO."
  (lyskom-collect-ignore-err 'main)
  (if (not (zerop conf-no)) 
      (progn
	(cache-del-conf-stat conf-no)
	(initiate-get-conf-stat 'main nil conf-no)))
  (if (not (zerop pers-no)) 
      (progn
	(cache-del-pers-stat pers-no)
	(initiate-get-pers-stat 'main nil pers-no)
	(initiate-get-conf-stat 'main nil pers-no)))
  (lyskom-list-use 'main 'lyskom-review-get-maps 
		   (and (not (zerop conf-no))
			conf-no)
		   (and (not (zerop pers-no))
			pers-no)
		   count info))


(defun lyskom-review-get-maps (data conf-no pers-no count info)
  "Has the conf-stat of the conf and pers-stat of the person in the first arg.
Checks for errors then gets maps and calls next function."
  (let* ((conf-stat (and conf-no
			 (lyskom-conf-stat-p (car data))
			 (prog1
			     (car data)
			   (setq data (cdr data)))))
	 (pers-stat (and pers-no
			 (lyskom-pers-stat-p (car data))
			 (prog1
			     (car data)
			   (setq data (cdr data)))))
	 (pers-conf-stat  (and pers-no
			       (lyskom-conf-stat-p (car data))
			       (car data)))
	 (info (lyskom-format 'info-by-to info 
			      (if pers-stat
				  (conf-stat->name pers-conf-stat)
				(lyskom-get-string 'anybody))
			      (if conf-stat
				  (conf-stat->name conf-stat)
				(lyskom-get-string 'all-confs))))
	 start-conf start-pers)

    (if (cond
	 ((not (eq (not conf-no) (not conf-stat)))
	  (lyskom-insert-string 'no-get-conf)
	  t)
	 ((not (eq (not pers-no) (not pers-stat)))
	  (lyskom-insert-string 'no'get'pers)
	  t)
	 ((and (not conf-stat)
	       (not pers-stat))
	  (lyskom-format-insert 'no-review-info info)
	  t))
	(lyskom-end-of-command)
      (lyskom-format-insert 'review-info info)
      (let ((from-end (and count (> count 0))))
	(lyskom-collect 'main)
	(if conf-stat
	    (let* ((max (1- (+ (conf-stat->no-of-texts conf-stat)
			       (conf-stat->first-local-no conf-stat))))
		   (end (if from-end
			    max
			  lyskom-fetch-map-nos))
		   (start (max 0 (1+ (- end lyskom-fetch-map-nos))))
		   (nom (1+ (- end start))))
	      (if (or (and from-end (= start 0))
		      (and (not from-end)
			   (= max end)))
		  (setq start-conf 'all)
		(setq start-conf start))
	      (initiate-get-map 'main nil conf-no start nom)))

	(if pers-stat
	    (let* ((max (1- (+ (pers-stat->no-of-created-texts pers-stat)
			       (pers-stat->first-created-text pers-stat))))
		   (end (if from-end 
			    max
			  lyskom-fetch-map-nos))
		   (start (max 0 (1+ (- end lyskom-fetch-map-nos))))
		   (nom (1+ (- end start))))
	      (if (or (and from-end (= start 0))
		      (and (not from-end)
			   (= max end)))
		  (setq start-pers 'all)
		(setq start-pers start))
	      (initiate-get-created-texts 'main nil pers-no start nom))))
      (lyskom-list-use 'main 'lyskom-review-by-to 
		       nil start-conf nil start-pers
		       conf-stat pers-stat count info))))


(defun lyskom-review-by-to (data texts-in-conf start-conf 
				 texts-written start-pers
				 conf-stat pers-stat count info)
  "Review texts by author to conference. Recursively fetches more maps.
Args: DATA, TEXTS-IN-CONF, START-CONF, TEXTS-WRITTEN, START-PERS,
      CONF-STAT, PERS-STAT, COUNT, INFO
DATA is a list of a conf-map if CONF-STAT is non-nil and 
a pers-map if PERS-STAT is non-nil.
Starts reviewing the texts both in conf-map and author-map. 
CONF-STAT and PERS-STAT are used to determine the start and stop of the maps.
TEXTS-IN-CONF and START-CONF is the list of text-nos and the starting local-no
associated with the conf.
TEXTS-WRITTEN and START-PERS is the list of text-nos and the starting local-no
associated with the pers.
If forth argument count is non-nil then the articles are restricted to the last
COUNT ones if COUNT >= 0 and the first -COUNT ones if COUNT < 0.
If COUNT is nil then all articles are chosen.
The INFO is a preformatted type to help telling."
  (let* ((conf-map (if (and conf-stat
			    (not (eq start-conf 'done)))
		       (prog1
			 (if (lyskom-map-p (car data))
			     (car data))
			 (setq data (cdr data)))))
	 (pers-map (if (and pers-stat
			    (not (eq start-pers 'done)))
		       (prog1
			   (car data)
			 (setq data (cdr data)))))
       	 (l1 (if conf-map
		 (skip-first-zeros
		  (sort (append (map->text-nos conf-map) texts-in-conf nil)
			'<))
	       texts-in-conf))
	 (texts-in-conf l1)		;Dont sort l1
	 (l2 (if pers-map
		 (skip-first-zeros
		  (sort (append (map->text-nos pers-map) texts-written nil)
			'<))
	       texts-written))
	 (texts-written l2)		;Dont sort l2
	 (start-conf (if (and count (> count 0))
			 start-conf
		       (if (and (map->first-local conf-map)
				(numberp start-conf))
			   (max start-conf (map->first-local conf-map))
			 start-conf)))
	 (start-pers (if (and count (> count 0))
			 start-pers
		       (if (and (map->first-local pers-map)
				(numberp start-pers))
			   (max start-pers (map->first-local pers-map))
			 start-pers)))
	 (list nil))
    (cond
     ((not pers-stat)
      (setq list (append (reverse l1) nil)))
     ((not conf-stat)
      (setq list (append (reverse l2) nil)))
     (t
      (while (and l1 l2)			;Borde kunna optimeras.
	(cond
	 ((< (car l1) (car l2))
	  (setq l1 (cdr l1)))
	 ((> (car l1) (car l2))
	  (setq l2 (cdr l2)))
	 ((= (car l1) (car l2))
	  (setq list (cons (car l1) list))
	  (setq l1 (cdr l1))
	  (setq l2 (cdr l2)))))))
    (cond
     ((or (and (or (eq start-conf 'all)
		   (eq start-conf 'done)
		   (not conf-stat))
	       (or (eq start-pers 'all)
		   (eq start-pers 'done)
		   (not pers-stat)))
	  (and count
	       (>= (length list) count)
	       (>= (length list) (- count))))
					;Were done strip of the part we want.
      (setq list (sort list '>))
      (cond
       ((and count (< count 0))
	(while (> (length list) (- count))
	  (setq list (cdr list))))
       ((and count (>= count 0))
	(setq list (nreverse list))
	(while (> (length list) count)
	  (setq list (cdr list)))
	(setq list (nreverse list))))
	
      (if list
	  (read-list-enter-read-info
	   (lyskom-create-read-info
	    'REVIEW
	    nil
	    (lyskom-get-current-priority)
	    (lyskom-create-text-list (nreverse list))
	    nil t)
	   lyskom-reading-list t)
	(lyskom-insert-string 'no-such-text))
      (lyskom-end-of-command))
     (t					;Read new maps
      (lyskom-collect 'main)
      (let ((from-end (and count (> count 0))))
	(if (or (eq start-conf 'all)
		(and (numberp start-conf)
		     (< start-conf (map->first-local conf-map))))
	    (setq start-conf 'done))
	(if (and conf-stat
		 (not (eq start-conf 'done)))
	    (let* ((end (if from-end
			    start-conf
			  (+ start-conf (length (map->text-nos conf-map))
			     lyskom-fetch-map-nos)))
		   (start (max 0 (- end lyskom-fetch-map-nos)))
		   (nom (- end start)))
	      (if (or (= start 0)
		      (>= end (1- (+ (conf-stat->no-of-texts conf-stat)
				     (conf-stat->first-local-no conf-stat)))))
		  (setq start-conf 'all)
		(setq start-conf start))
	      (initiate-get-map 'main nil (conf-stat->conf-no conf-stat)
				start nom)))
	(if (or (eq start-pers 'all)
		(and (numberp start-pers)
		     (< start-pers (map->first-local pers-map))))
	    (setq start-pers 'done))
	(if (and pers-stat
		 (not (eq start-pers 'done)))
	    (let* ((end (if from-end
			    start-pers
			  (+ start-pers (length (map->text-nos pers-map))
			     lyskom-fetch-map-nos)))
		   (start (max 0 (- end lyskom-fetch-map-nos)))
		   (nom (- end start)))
	      (if (or (= start 0)
		      (>= end (1-
			       (+ (pers-stat->no-of-created-texts pers-stat)
				  (pers-stat->first-created-text pers-stat)))))
		  (setq start-pers 'all)
		(setq start-pers start))
	      (initiate-get-created-texts 'main nil
					  (pers-stat->pers-no pers-stat)
					  start nom))))
      (lyskom-list-use 'main 'lyskom-review-by-to
		       texts-in-conf start-conf
		       texts-written start-pers
		       conf-stat pers-stat count info)))))
	


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
      (lyskom-insert
       (lyskom-format 'you-review (lyskom-get-string (if (not forward)
							 'forward
						       'backward))))))
   (t
    (lyskom-insert-string 'illegal-command)))
  (lyskom-end-of-command)) 



;;; +++This function should perhaps be moved somewhere???    

(defun skip-first-zeros (list)
  "Returns the list beginning at the first non-0 element in LIST."
  (while (eq (car list) 0)		;(car nil) -> nil
    (setq list (cdr list)))
  list)


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
  (if text-no
      (progn
	(initiate-get-text-stat 'main 'lyskom-follow-comments text-no
				nil 'review
				(lyskom-get-current-priority)
				t)
	(lyskom-run 'mail 'lyskom-end-of-command))
    (lyskom-insert-string 'read-text-first)
    (lyskom-end-of-command)))


(defun kom-find-root ()
  "Finds the root text of the tree containing the text in lyskom-current-text."
  (interactive)
  (lyskom-start-of-command 'kom-find-root)
  (cond
   (lyskom-current-text 
    (lyskom-collect 'review)
    (initiate-get-text-stat 'review nil lyskom-current-text)
    (initiate-get-text-stat 'review nil lyskom-current-text)
    (lyskom-use 'review 'lyskom-find-root 'kom-find-root-2))
   (t
    (lyskom-insert-string 'read-text-first)
    (lyskom-end-of-command))))


(defun kom-find-root-2 (text-no)
  "Runs lyskom-view-text with the text-no that lyskom-find-root produced
and then runs lyskom-end-of-command."
  (lyskom-view-text 'main text-no)
  (lyskom-run 'main 'lyskom-end-of-command))


(defun kom-find-root-review ()
  "Finds the root text of the tree containing the text in lyskom-current-text and
reviews the whole tree in deep-first order."
  (interactive)
  (lyskom-start-of-command 'kom-find-root-review)
  (cond
   (lyskom-current-text
    (lyskom-collect 'review)
    (initiate-get-text-stat 'review nil lyskom-current-text)
    (initiate-get-text-stat 'review nil lyskom-current-text)
    (lyskom-use 'review 'lyskom-find-root 'lyskom-review-tree))
   (t
    (lyskom-insert-string 'read-text-first)
    (lyskom-end-of-command))))


(defun lyskom-find-root (text-stat old-text-stat thendo)
  "Finds the root text of the tree containing the text TEXT-STAT.
Args: TEXT-STAT OLD-TEXT-STAT THENDO
If TEXT-STAT is nil and OLD-TEXT-STAT contains a text-stat
then this means that the parent of the text in OLD-TEXT-STAT is not readable
and the text in OLD-TEXT-STAT is to be used instead.
If both TEXT-STAT and OLD-TEXT-STAT is nil then this means we are not allowed
to read the text we are trying to find the root of. This just returns with a
message.
If the third argument THENDO is non-nil then call the function with the text-no
of the root text as argument."
  (let* ((ts text-stat)
	 (misclist (and ts (text-stat->misc-info-list ts)))
	 (todo t))
    (cond
     (ts				;+++ Smartare errorhantering hit.
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
	    (initiate-get-text-stat 'main 'lyskom-find-root
				    parent-no ts thendo)
	    (setq todo nil)
	    (setq misclist nil))))
	(setq misclist (cdr misclist)))
      (if todo (apply thendo (list (text-stat->text-no ts)))))
     (old-text-stat
      (apply thendo (list (text-stat->text-no old-text-stat))))
     (t
      (lyskom-insert-string 'cannot-get-last-text)
      (lyskom-end-of-command)))))


(defun lyskom-review-tree (text)
  "Takes a TEXT as an arg, shows the text and the tree of all comments.
Does a lyskom-end-of-command.
Text is a text-no or a text-stat."
  (cond
   ((integerp text)
    (lyskom-view-text 'main text nil t nil (lyskom-get-current-priority) t))
   (t 
    (lyskom-view-text 'main (text-stat->text-no text-stat)
		      nil t nil (lyskom-get-current-priority) t)))
  (lyskom-run 'main 'lyskom-end-of-command))


;;; ================================================================
;;;                     ]terse n{sta -  review next

;;; Author: Linus Tolke


(defun kom-review-next ()
  "Resumes an interupted review by moving all review and review-tree entries in
the lyskom-reading-list to the beginning. i.e by moving all other types to the
end."
  (interactive)
  (lyskom-start-of-command 'kom-review-next)
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
  (lyskom-tell-internat 'kom-tell-read)
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
	  (lyskom-view-text 'main (car text-nos)))
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
	  (lyskom-view-text 'main (car text-nos)))
      (lyskom-format-insert 'no-such-text)))
  (lyskom-run 'main 'lyskom-end-of-command))
      
(defun nfirst (n list)
  "Return a list of the N first elements of LIST."
  (if (or (<= n 0) (not list))
      nil
    (cons (car list) (nfirst (1- n) (cdr list)))))
