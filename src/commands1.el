;;;;;
;;;;; $Id: commands1.el,v 35.13 1992-01-13 20:44:57 ceder Exp $
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
;;;; File: commands1.el
;;;;
;;;; This file contains the code for some of the high level commands.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: commands1.el,v 35.13 1992-01-13 20:44:57 ceder Exp $\n"))


;;; ================================================================
;;;                  F} uppmuntran - Get appreciation 

;;; Author: Inge Wallin


(defun kom-get-appreciation ()
  "Give the user a little light in the dark"
  (interactive)
  (lyskom-start-of-command 'kom-get-appreciation)
  (lyskom-insert-string 'appreciation)
  (lyskom-end-of-command))


;;; ================================================================
;;;                      F} Sk{ll - Get abuse

;;; Author: Inge Wallin


(defun kom-get-abuse ()
  "Give the user a little verbal abuse."
  (interactive)
  (lyskom-start-of-command 'kom-get-abuse)
  (lyskom-insert-string 'abuse)
  (lyskom-end-of-command))


;;; ================================================================
;;;            Utpl}na - Delete a person or a conference

;;; Author: Inge Wallin


(defun kom-delete-conf ()
  "Delete a person or a conference."
  (interactive)
  (lyskom-start-of-command 'kom-delete-conf)
  (lyskom-completing-read 
   'main 'lyskom-delete-conf
   (lyskom-get-string 'what-conf-to-delete)
   nil nil ""))


(defun lyskom-delete-conf (conf)
  "Deletes the conference CONF.
CONF can be either a conf-no or a conf-stat."
  (cond					;+++ error handling should be better
   ((lyskom-conf-stat-p conf)
    (if (ja-or-nej-p
	 (lyskom-format 'confirm-delete-pers-or-conf
			(if (conf-type->letterbox (conf-stat->conf-type 
						   conf))
			    (lyskom-get-string 'the-pers)
			  (lyskom-get-string 'the-conf))
			(conf-stat->name conf)))
	(initiate-delete-conf 'main 'lyskom-delete-conf-2 
			      (conf-stat->conf-no conf) conf) 
      (lyskom-insert-string 'deletion-not-confirmed)
      (lyskom-end-of-command)))
   ((numberp conf)			;+++ Should be lyskom-conf-no-p
    (initiate-get-conf-stat 'main 'lyskom-delete-conf conf))
   (t
    (lyskom-insert-string 'somebody-else-deleted-that-conf)
    (lyskom-end-of-command))))


(defun lyskom-delete-conf-2 (answer conf-stat)
  "Handles the answer from the server after deletion of a conf."
  (if answer 
     (lyskom-format-insert 'conf-is-deleted
			   (conf-stat->name conf-stat))
    (lyskom-format-insert 'you-could-not-delete
			  (conf-stat->name conf-stat)))
  (lyskom-end-of-command))


;;; ================================================================
;;;                Radera (text) - Delete a text

;;; Author: Inge Wallin


(defun kom-delete-text (text-no-arg)
  "Delete a text. Argument: TEXT-NO"
  (interactive "P")
  (lyskom-start-of-command 'kom-delete-text)
  (let ((text-no (cond ((null text-no-arg) 0)
		       ((integerp text-no-arg) text-no-arg)
		       ((listp text-no-arg) (car text-no-arg))
		       (t 0))))
    (if (equal text-no 0)
	(setq text-no 
	      (lyskom-read-number (lyskom-get-string 'what-text-to-delete)
				  lyskom-current-text)))
    (lyskom-format-insert 'deleting-text text-no)
    (initiate-delete-text 'main 'lyskom-handle-command-answer text-no)))


;;; ================================================================
;;;        ]terse presentation - Review the presentation
;;;               for a person or a conference

;;; Author: Inge Wallin


(defun kom-review-presentation ()
  "Review the presentation for a person or a conference."
  (interactive)
  (lyskom-start-of-command 'kom-review-presentation)
  (lyskom-completing-read-conf-stat
   'main 'lyskom-review-presentation
   (lyskom-get-string 'presentation-for-whom)
   nil nil ""))


(defun lyskom-review-presentation (conf-stat)
  "Shows the presentation of CONF-STAT."
  (if (null conf-stat)
      (lyskom-insert-string 'somebody-deleted-that-conf)
    (lyskom-format-insert 'review-presentation-of
			   (conf-stat->name conf-stat))
    (if (/= (conf-stat->presentation conf-stat) 0)
	(lyskom-view-text 'main (conf-stat->presentation conf-stat))
      (lyskom-format-insert 'has-no-presentation
			    (conf-stat->name conf-stat))))
  (lyskom-run 'main 'lyskom-end-of-command))


;;; ================================================================
;;;          ]terse det kommenterade - View commented text

;;; Author: Inge Wallin


(defun kom-view-commented-text ()
  "View the commented text.
If the current text is comment to (footnote to) several text then the first
text is shown and a REVIEW list is built to shown the other ones."
  (interactive)
  (lyskom-start-of-command 'kom-view-commented-text)
  (if lyskom-current-text
      (progn
	(lyskom-tell-internat 'kom-tell-read)
	(initiate-get-text-stat 'read 'lyskom-view-commented-text
				lyskom-current-text))
    (lyskom-insert-string 'have-to-read)
    (lyskom-end-of-command)))


(defun lyskom-view-commented-text (text-stat)
  "Handles the return from the initiate-get-text-stat, displays and builds list."
  (let* ((misc-info-list (and text-stat
			      (text-stat->misc-info-list text-stat)))
	 (misc-infos (and misc-info-list
			  (append (lyskom-misc-infos-from-list 'COMM-TO
							       misc-info-list)
				  (lyskom-misc-infos-from-list 'FOOTN-TO
							       misc-info-list))))
	 (text-nos (and misc-infos
			(mapcar
			 (function
			  (lambda (misc-info)
			    (if (equal (misc-info->type misc-info)
				       'COMM-TO)
				(misc-info->comm-to misc-info)
			      (misc-info->footn-to misc-info))))
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
      (lyskom-insert-string 'no-comment-to)))
  (lyskom-run 'main 'lyskom-end-of-command))


(defun lyskom-misc-infos-from-list (type list)
  "Get all the misc-infos from the misc-info-list LIST with the same type
as TYPE. If no such misc-info, return NIL"
  (cond ((null list) nil)
	((equal type (misc-info->type (car list))) 
	 (cons (car list)
	       (lyskom-misc-infos-from-list type (cdr list))))
	(t (lyskom-misc-infos-from-list type (cdr list)))))


;;; ================================================================
;;;                         Brev - Send letter

;;; Author: Inge Wallin


(defun kom-send-letter ()
  "Send a personal letter to a person."
  (interactive)
  (lyskom-start-of-command 'kom-send-letter)
  (lyskom-tell-internat 'kom-tell-write-letter)
  (lyskom-completing-read-conf-stat 'main 'lyskom-send-letter-2
				    (lyskom-get-string 'who-letter-to)
				    nil nil ""))

			  
(defun lyskom-send-letter-2 (conf-stat)
  "Send a letter to the person or conference with conf-stat CONF-STAT.
If the conference has a set motd, show it and confirm that the user
still wants to send the letter."
  (if (zerop (conf-stat->msg-of-day conf-stat))
      (lyskom-do-send-letter (conf-stat->conf-no conf-stat))
    (progn
      (recenter 0)
      (lyskom-format-insert 'has-motd (conf-stat->name conf-stat))
      (lyskom-view-text 'main (conf-stat->msg-of-day conf-stat))
      (lyskom-run 'main 'lyskom-send-letter-3
		  (conf-stat->conf-no conf-stat)))))


(defun lyskom-send-letter-3 (conf-no)
  "Ask for confirmation if the recipient of the letter has a motd."
  (if (j-or-n-p (lyskom-get-string 'motd-persist-q))
      (lyskom-do-send-letter conf-no)
    (lyskom-end-of-command)))


(defun lyskom-do-send-letter (conf-no)
  "Asks for subject for the letter to be written and starts the editing."
  (if (= conf-no lyskom-pers-no)
      (lyskom-edit-text lyskom-proc
			(lyskom-create-misc-list 'recpt conf-no)
			"" "")
    (lyskom-edit-text lyskom-proc
		      (lyskom-create-misc-list 'recpt conf-no
					       'recpt lyskom-pers-no)
		      "" "")))


;;; ================================================================
;;;           Bli medlem i m|te - Become a member of a conference
;;;             Addera medlem - Add somebody else as a member

;;; Author: ???


;; Add another person

(defun kom-add-member ()
  "Add a person as a member of a conference. Ask for the name of the person."
  (interactive)
  (lyskom-start-of-command 'kom-add-member)
  (lyskom-completing-read 'main 'lyskom-add-member
			  (lyskom-get-string 'who-to-add)
			  'person nil ""))


(defun lyskom-add-member (pers-no)
  "Ask which conference the person should be added to."
  (lyskom-completing-read 'main 'lyskom-add-member-2
			  (lyskom-get-string 'where-to-add)
			  nil nil ""
			  pers-no 'lyskom-end-of-command))


;; Add self

(defun kom-add-self ()
  "Add this person as a member of a conference."
  (interactive)
  (lyskom-start-of-command 'kom-add-self)
  (lyskom-completing-read 'main 'lyskom-add-member-2
			  (lyskom-get-string 'where-to-add-self)
			  nil nil ""
			  lyskom-pers-no
			  'lyskom-end-of-command))


;;; NOTE: This function is also called from lyskom-go-to-conf-handler
;;;       and from lyskom-create-conf-handler.

(defun lyskom-add-member-2 (conf-no pers-no &optional thendo &rest data)
  "Fetch info to be able to add a person to a conf.
Get the conf-stat CONF-NO for the conference and the conf-stat and pers-stat 
for person PERS-NO and send them into lyskom-add-member-3.
Let lyskom-add-member-3 call the optional function THENDO with the arguments
DATA."
  (lyskom-collect 'main)
  (initiate-get-conf-stat 'main nil conf-no)
  (initiate-get-conf-stat 'main nil pers-no)
  (initiate-get-pers-stat 'main nil pers-no)
  (lyskom-use 'main 'lyskom-add-member-3 thendo data))


(defun lyskom-add-member-3 (conf-conf-stat pers-conf-stat pers-stat
					   &optional thendo data)
  "Add a member to a conference.
Args: CONF-CONF-STAT PERS-CONF-STAT PERS-STAT &optional THENDO DATA
CONF-CONF-STAT: the conf-stat of the conference the person is being added to
PERS-CONF-STAT: the conf-stat of the person being added.
PERS-STAT: the pers-stat of the person being added.
If optional argument THENDO (and data) then call THENDO with the argments if
successful.
If THENDO is nil then execute lyskom-end-of-command."
  (if (or (null conf-conf-stat)
	  (null pers-conf-stat))
      (lyskom-end-of-command)
    (let ((priority
	   (if (/= lyskom-pers-no (conf-stat->conf-no pers-conf-stat))
	       100			; When adding someone else
	     (if (and (numberp kom-membership-default-priority)
		      (< kom-membership-default-priority 256)
		      (>= kom-membership-default-priority 0))
		 kom-membership-default-priority
	       (lyskom-read-num-range
		0 255 (lyskom-get-string 'priority-q)))))
	  (where
	   (if (/= lyskom-pers-no (conf-stat->conf-no pers-conf-stat))
	       1			; When adding someone else
	     (cond
	      ((and (numberp kom-membership-default-placement)
		    (>= kom-membership-default-placement 0))
	       kom-membership-default-placement)
	      ((eq kom-membership-default-placement 'first)
	       0)
	      ((eq kom-membership-default-placement 'last)
	       (length lyskom-membership))
	      (t
	       (lyskom-read-num-range
		0 (pers-stat->no-of-confs pers-stat)
		(lyskom-format 'where-on-list-q
			       (length lyskom-membership))))))))

      (lyskom-insert (if (= (conf-stat->conf-no pers-conf-stat)
			    lyskom-pers-no)
			 (lyskom-format 'member-in-conf
					(conf-stat->name conf-conf-stat))
		       (lyskom-format 'add-member-in
				      (conf-stat->name pers-conf-stat)
				      (conf-stat->name conf-conf-stat))))
      (initiate-add-member 'main 'lyskom-add-member-answer
			   (conf-stat->conf-no conf-conf-stat)
			   (conf-stat->conf-no pers-conf-stat)
			   priority where
			   conf-conf-stat pers-conf-stat thendo data))))


(defun lyskom-add-member-answer (answer conf-conf-stat pers-conf-stat
					&optional thendo data)
  "Handle the result from an attempt to add a member to a conference."
  (if (null answer)
      (lyskom-handle-command-answer answer)
    (lyskom-insert-string 'done)
    (cache-del-pers-stat (conf-stat->conf-no pers-conf-stat)) ;+++Borde {ndra i cachen i st{llet.
    (cache-del-conf-stat (conf-stat->conf-no conf-conf-stat)) ;+++Borde {ndra i cachen i st{llet.
    (if (= (conf-stat->conf-no pers-conf-stat)
	   lyskom-pers-no)
	(initiate-query-read-texts 'main 'lyskom-add-membership
				   lyskom-pers-no 
				   (conf-stat->conf-no conf-conf-stat)
				   conf-conf-stat thendo data)
      (if thendo
	  (apply 'lyskom-run 'main thendo data)))))


(defun lyskom-add-membership (membership conf-stat &optional thendo data)
  "Adds MEMBERSHIP to the sorted list of memberships.
Args: MEMBERSHIP CONF-STAT THENDO DATA
Then if non-nil do THENDO with arguments DATA.
Also adds to lyskom-to-do-list."
  (if membership
      (progn
	(setq lyskom-membership (sort (cons membership lyskom-membership)
				      'lyskom-membership-<))
	(initiate-get-map 'main 'lyskom-add-membership-2
			  (conf-stat->conf-no conf-stat)
			  (max (1+ (membership->last-text-read membership))
			       (conf-stat->first-local-no conf-stat))
			  (conf-stat->no-of-texts conf-stat)
			  membership
			  conf-stat
			  thendo data))
    (lyskom-insert-string 'conf-does-not-exist)
    (if thendo
	(apply 'lyskom-run 'main thendo data))))


(defun lyskom-add-membership-2 (map membership conf-stat thendo data)
  "Adds info to lyskom-to-do-list."
  (if map
      (let ((texts (skip-first-zeros
		    (sort (append (map->text-nos map) nil)
			  '<))))
	(if texts
	    (read-list-enter-read-info (lyskom-create-read-info
					'CONF conf-stat
					(membership->priority membership)
					(lyskom-create-text-list
					 texts)
					nil nil)
					lyskom-to-do-list))))
  (if thendo
      (apply 'lyskom-run 'main thendo data)))



;;; ================================================================
;;;     Uttr{d - Subtract yourself as a member from a conference
;;;     Uteslut medlem - Subtract somebody else as a member

;;; Author: Inge Wallin


;; Subtract another person

(defun kom-sub-member ()
  "Subtract a person as a member from a conference. Ask for
the name of the person."
  (interactive)
  (lyskom-start-of-command 'kom-sub-member)
  (lyskom-completing-read 'main 'lyskom-sub-member
			  (lyskom-get-string 'who-to-exclude)
			  'person nil ""))


(defun lyskom-sub-member (pers-no)
  "Ask which conference the person should be subtracted from."
  (lyskom-completing-read 'main 'lyskom-sub-member-2
			  (lyskom-get-string 'where-from-exclude)
			  nil nil ""
			  pers-no))


;; Subtract self

(defun kom-sub-self ()
  "Subtract this person as a member from a conference."
  (interactive)
  (lyskom-start-of-command 'kom-sub-self)
  (lyskom-completing-read 'main 'lyskom-sub-member-2
			  (lyskom-get-string 'leave-what-conf)
			  nil nil ""
			  lyskom-pers-no))


(defun lyskom-sub-member-2 (conf-no pers-no)
  "Get the conf-stat for both the person and the conference and
send them into lyskom-sub-member-3"
  (lyskom-collect 'main)
  (initiate-get-conf-stat 'main nil conf-no)
  (initiate-get-conf-stat 'main nil pers-no)
  (lyskom-use 'main 'lyskom-sub-member-3))


(defun lyskom-sub-member-3 (conf-conf-stat pers-conf-stat)
  "Does the deletion of a person from a conf if all needed info could be fetched.
If either the person or the confs conf-stat could not be fetched it tells the
user so instead."
  (cond ((null pers-conf-stat)
	 (lyskom-insert-string 'error-fetching-person)
	 (lyskom-end-of-command))
	((null conf-conf-stat)
	 (lyskom-insert-string 'error-fetching-conf)
	 (lyskom-end-of-command))
	(t
	 (lyskom-insert (if (= (conf-stat->conf-no pers-conf-stat)
			       lyskom-pers-no)
			    (lyskom-format 'unsubscribe-to
					   (conf-stat->name conf-conf-stat))
			  (lyskom-format 'exclude-from
					 (conf-stat->name pers-conf-stat)
					 (conf-stat->name conf-conf-stat))))
	 (initiate-sub-member 'main 'lyskom-sub-member-answer
			      (conf-stat->conf-no conf-conf-stat) 
			      (conf-stat->conf-no pers-conf-stat)
			      conf-conf-stat pers-conf-stat)
	 (if (= (conf-stat->conf-no pers-conf-stat) lyskom-pers-no)
	     (initiate-get-membership 'main 'lyskom-set-membership
				      (conf-stat->conf-no
				       pers-conf-stat))))))


(defun lyskom-sub-member-answer (answer conf-conf-stat pers-conf-stat)
  "Handles the answer after the unsubscribe call to the server."
  (if (not answer)
      (lyskom-insert
       (lyskom-format 'unsubscribe-failed
	       (if (= (conf-stat->conf-no pers-conf-stat) lyskom-pers-no)
		   (lyskom-get-string 'You)
		 (conf-stat->name pers-conf-stat))
	       (conf-stat->name conf-conf-stat)))
    (lyskom-insert-string 'done)
    (if (and (= (conf-stat->conf-no pers-conf-stat)
		lyskom-pers-no)
	     (= (conf-stat->conf-no conf-conf-stat)
		lyskom-current-conf))
	(progn
	  (set-read-list-empty lyskom-reading-list)
	  (setq lyskom-current-conf 0)))
    (if (= (conf-stat->conf-no pers-conf-stat)
	   lyskom-pers-no)
	(setq lyskom-last-conf-received (1- lyskom-last-conf-received)))
    (read-list-delete-read-info (conf-stat->conf-no conf-conf-stat)
				lyskom-to-do-list))
  (lyskom-run 'main 'lyskom-end-of-command))



;;; ================================================================
;;;                 Skapa m|te - Create a conference

;;; Author: ???


(defun kom-create-conf (conf-name)
  "Create a conference."
  (interactive (list (lyskom-read-string (lyskom-get-string 'name-of-conf))))
  (let* ((open (j-or-n-p (lyskom-get-string 'anyone-member)))
	 (secret (if (not open)
		     (j-or-n-p (lyskom-get-string 'secret-conf))))
	 (orig (j-or-n-p (lyskom-get-string 'comments-allowed))))
    (lyskom-start-of-command 'kom-create-conf)
    (initiate-create-conf 'main 'lyskom-create-conf-handler
			  conf-name (lyskom-create-conf-type
				     (not open)
				     (not orig)
				     secret
				     nil)
			  conf-name)))


(defun lyskom-create-conf-handler (conf-no conf-name)
  "Deal with the answer from an attempt to create a conference.
Add the person creating and execute lyskom-end-of-command."
  (if (null conf-no)
      (lyskom-format-insert 'could-not-create-conf
			    conf-name
			    lyskom-errno)
    (progn
      (lyskom-format-insert 'created-conf-no-name 
			    conf-no
			    conf-name)
      (lyskom-scroll)
      (lyskom-add-member-2 conf-no lyskom-pers-no
			   'lyskom-create-conf-handler-2 conf-no conf-name))))


(defun lyskom-create-conf-handler-2 (conf-no conf-name)
  "Starts editing a presentation for the newly created conference.
This does lyskom-end-of-command"
  (lyskom-tell-internat 'kom-tell-conf-pres)
  (lyskom-edit-text lyskom-proc
		    (lyskom-create-misc-list
		     'recpt
		     (server-info->conf-pres-conf lyskom-server-info))
		    conf-name ""
		    'lyskom-set-presentation conf-no)
  (lyskom-end-of-command))


(defun lyskom-set-presentation (text-no conf-no)
  "Set presentation of a conference. Args: text-no conf-no."
  (initiate-set-presentation 'background nil conf-no text-no)
  (cache-del-conf-stat conf-no)) ;+++Borde {ndra i cachen i st{llet.
					;+++ Kan tas bort n{r det existerar 
					;asynkrona meddelanden som talar om att
					;n}got {r {ndrat.
    

(defun lyskom-set-conf-motd (text-no conf-no)
  "Set motd of a conference. Args: text-no conf-no."
  (initiate-set-conf-motd 'background nil conf-no text-no)
  (cache-del-conf-stat conf-no)) ;+++Borde {ndra i cachen i st{llet.
					;+++ Kan tas bort n{r det existerar 
					;asynkrona meddelanden som talar om att
					;n}got {r {ndrat.
    

;;; ================================================================
;;;                  Kommentera - write comment

;;; Author: ???


(defun kom-write-comment (&optional text-no)
  "Write a comment to a text.
If optional arg TEXT-NO is present write a comment to that text instead."
  (interactive (list 
		(cond
		 ((null current-prefix-arg)
		  lyskom-current-text)
		 ((integerp current-prefix-arg)
		  current-prefix-arg)
		 ((and (listp current-prefix-arg) 
		       (integerp (car current-prefix-arg)) 
		       (null (cdr current-prefix-arg)))
		  (car current-prefix-arg))
		 (t
		  (signal 'lyskom-internal-error '(kom-write-comment))))))
  (lyskom-start-of-command (concat 
			    (car (cdr (assoc 'kom-write-comment
					     lyskom-commands)))
			    (if text-no 
				(format " (%d)" text-no)
			      "")))
  (if text-no
      (progn
	(lyskom-collect 'main)
	(initiate-get-text-stat 'main nil text-no)
	(initiate-get-text 'main nil text-no)
	(lyskom-use 'main 'lyskom-write-comment-soon text-no 'comment))
    (lyskom-insert-string 'confusion-what-to-comment)
    (lyskom-end-of-command)))


(defun kom-write-footnote (&optional text-no)
  "Write a footnote to a text.
If optional arg TEXT-NO is present write a footnote to that text instead."
  (interactive (list 
		(cond
		 ((null current-prefix-arg)
		  lyskom-current-text)
		 ((integerp current-prefix-arg)
		  current-prefix-arg)
		 ((and (listp current-prefix-arg) 
		       (integerp (car current-prefix-arg)) 
		       (null (cdr current-prefix-arg)))
		  (car current-prefix-arg))
		 (t
		  (signal 'lyskom-internal-error '(kom-write-comment))))))
  (lyskom-start-of-command 'kom-write-footnote)
  (if text-no
      (progn
	(lyskom-collect 'main)
	(initiate-get-text-stat 'main nil text-no)
	(initiate-get-text 'main nil text-no)
	(lyskom-use 'main 'lyskom-write-comment-soon text-no 'footnote))
    (lyskom-insert-string 'confusion-what-to-footnote)
    (lyskom-end-of-command)))


(defun kom-comment-previous ()
  "Write a comment to previously viewed text."
  (interactive)
  (lyskom-start-of-command 'kom-comment-previous)
  (if lyskom-previous-text
      (progn
	(lyskom-collect 'main)
	(initiate-get-text-stat 'main nil lyskom-previous-text)
	(initiate-get-text 'main nil lyskom-previous-text)
	(lyskom-use 'main 'lyskom-write-comment-soon 
		    lyskom-previous-text 'comment))
    (lyskom-insert-string 'confusion-what-to-comment)
    (lyskom-end-of-command)))


(defun lyskom-write-comment-soon (text-stat text text-no type)
  "Write a comment to the text with TEXT-STAT, TEXT and, TEXT-NO.
TYPE is either 'comment or 'footnote."
  (cond
   ;; Text not found?
   ((or (null text-stat)
	(null text))
    (lyskom-format-insert 'cant-read-textno text-no)
    (lyskom-end-of-command))
   ;; Give header.
   ((string-match "\n" (text->text-mass text))
    (lyskom-write-comment text-stat
			  (substring (text->text-mass text)
				     0 (match-beginning 0))
			  type))
   ;; The commented text had no header.
   (t (lyskom-write-comment text-stat "" type))))


(defun lyskom-write-comment (text-stat subject type)
  "Write a comment to the text associated with TEXT-STAT.
The default subject is SUBJECT. TYPE is either 'comment or 'footnote."
  (if (null text-stat)
      (progn
	(lyskom-insert-string 'confusion-what-to-comment)
	(lyskom-end-of-command))
    (progn
      (lyskom-tell-internat (if (eq type 'comment)
				'kom-tell-write-comment
			      'kom-tell-write-footnote))
      (lyskom-collect 'edit)
      (lyskom-traverse
       misc-info (text-stat->misc-info-list text-stat)
       (cond
	((eq 'RECPT (misc-info->type misc-info))
	 (initiate-get-conf-stat 'edit nil (misc-info->recipient-no 
					    misc-info)))))
      (lyskom-list-use 'edit 'lyskom-comment-recipients lyskom-proc text-stat
		       subject type))))


(defun lyskom-comment-recipients (data lyskom-proc text-stat subject type)
  "Compute recipients to a comment to a text.
Args: DATA, LYSKOM-PROC TEXT-STAT SUBJECT TYPE.
DATA is a list of all the recipients that should receive this text.
If DATA contains more than one conference the user is asked (using y-or-n-p)
if all conferences really should receive the text.
The call is continued to the lyskom-edit-text."

  (condition-case x
      ;; Catch any quits and run lyskom-end-of-command.
      (progn
	;; Filter multiple recipients through y-or-n-p.
	(if (and kom-confirm-multiple-recipients (> (length data) 1)
		 (not (and (= (length data) 2)
			   (or (= lyskom-pers-no (conf-stat->conf-no
						  (car data)))
			       (= lyskom-pers-no (conf-stat->conf-no
						  (car (cdr data))))))))
	    (let ((new-data nil))
	      (while data
		(if (j-or-n-p (lyskom-format 'comment-keep-recpt-p
					     (conf-stat->name (car data))))
		    (setq new-data (cons (car data) new-data)))
		(setq data (cdr data)))
	      (setq data (nreverse new-data))))	    

	(let* ((member nil)
	       (recver (lyskom-create-misc-list
			(cond 
			 ((eq type 'comment) 'comm-to)
			 ((eq type 'footnote) 'footn-to)
			 (t (signal 'lyskom-internal-error
				    (list "Unknown comment type" type))))
			(text-stat->text-no text-stat)))
	       (recpts nil))
	  (while data
	    (let ((conf-stat (car data)))
	      (if (memq (conf-stat->comm-conf conf-stat) recpts)
		  nil
		(setq recver
		      (append recver
			      (list
			       (cons 'recpt
				     (conf-stat->comm-conf conf-stat)))))
		(if (lyskom-member-p (conf-stat->conf-no conf-stat))
		    (setq member t))
		(setq recpts (cons (conf-stat->comm-conf conf-stat) recpts))))
	    (setq data (cdr data)))
	  ;; Add the user to the list of recipients if he isn't a member in
	  ;; any of the recipients.
	  (if (not member)
	      (setq recver (append recver
				   (list (cons 'recpt lyskom-pers-no)))))
	  (lyskom-edit-text lyskom-proc 
			    recver
			    subject "")))

    (quit (lyskom-end-of-command)
	  (ding))))


;;; ================================================================
;;;                Personligt svar - personal answer

;;; Author: ???


(defun kom-private-answer (&optional text-no)
  "Write a private answer to the current text.
If optional arg TEXT-NO is present write a private answer to
that text instead."
  (interactive (list
		(cond
		 ((null current-prefix-arg)
		  lyskom-current-text)
		 ((integerp current-prefix-arg)
		  current-prefix-arg)
		 ((and (listp current-prefix-arg) 
		       (integerp (car current-prefix-arg)) 
		       (null (cdr current-prefix-arg)))
		  (car current-prefix-arg))
		 (t
		  (signal 'lyskom-internal-error '(kom-private-answer))))))
  (if text-no
      (progn
	(lyskom-collect 'main)
	(initiate-get-text-stat 'main nil text-no)
	(initiate-get-text 'main nil text-no)
	(lyskom-use 'main 'lyskom-private-answer-soon))
    (lyskom-start-of-command 'kom-private-answer)
    (lyskom-insert-string 'confusion-who-to-reply-to)
    (lyskom-end-of-command)))


(defun lyskom-private-answer-soon (text-stat text)
  "Write a private answer to TEXT-STAT, TEXT."
  (if (string-match "\n" (text->text-mass text))
      (lyskom-private-answer text-stat
			     (substring (text->text-mass text)
					0 (match-beginning 0)))
    (lyskom-private-answer text-stat "")))


(defun lyskom-private-answer (text-stat subject)
  "Write a private answer. Args: TEXT-STAT SUBJECT."
  (lyskom-start-of-command 'kom-private-answer)
  (if (null text-stat)
      (progn
	(lyskom-insert-string 'confusion-what-to-answer-to)
	(lyskom-end-of-command))
    (progn
      (lyskom-tell-internat 'kom-tell-write-reply)
      (lyskom-edit-text lyskom-proc
			(lyskom-create-misc-list
			 'comm-to (text-stat->text-no text-stat)
			 'recpt (text-stat->author text-stat)
			 'recpt lyskom-pers-no)
			subject ""))))


;;; ================================================================
;;;                         Sluta - quit

;;; Author: ???


(defun kom-quit (&optional arg)
  "Quit session. Kill process and buffer-local variables.
If optional argument is non-nil then dont ask for confirmation."
  (interactive "P")
  (lyskom-start-of-command 'kom-quit t)
  (cond
   ((or arg (ja-or-nej-p (lyskom-get-string 'really-quit)))
    (initiate-logout 'main nil)
    (set-process-sentinel lyskom-proc nil)
    (delete-process lyskom-proc)
    (setq lyskom-proc)
    (lyskom-clear-vars)
    (lyskom-insert-string 'session-ended)
    (lyskom-scroll)
    (run-hooks 'kom-quit-hook))
   (t (lyskom-end-of-command))))

  

;;; ================================================================
;;;     	[ndra presentation - Change presentation 
;;;	         S{tta lapp p} d|rren - Change conf-motd

;;; Author: Inge Wallin
;;; Changed by Linus Tolke


(defun kom-change-presentation ()
  "Change presentation for a person or a conference."
  (interactive)
  (lyskom-start-of-command 'kom-change-presentation)
  (lyskom-completing-read
   'main 'lyskom-change-presentation-or-motd
   (lyskom-get-string 'what-to-change-pres-you)
   nil 'empty "" 'pres))


(defun kom-change-conf-motd ()
  "Change motd for a person or a conference."
  (interactive)
  (lyskom-start-of-command 'kom-change-conf-motd)
  (lyskom-completing-read
   'main 'lyskom-change-presentation-or-motd
   (lyskom-get-string 'who-to-put-motd-for)
   nil 'empty "" 'motd))


(defun lyskom-change-presentation-or-motd (conf-no type)
  "Change the presentation or motd of CONF-NO.
TYPE is either 'pres or 'motd, depending on what should be changed."
  (if (zerop conf-no)
      (setq conf-no lyskom-pers-no))
  (initiate-get-conf-stat 'main 'lyskom-change-pres-or-motd-2 
			  conf-no type))


(defun lyskom-change-pres-or-motd-2 (conf-stat type)
  "Change the presentation or motd of CONF-STAT.
TYPE is either 'pres or 'motd, depending on what should be changed."
  (cond
   ((null conf-stat)			;+++ annan felhantering
      (lyskom-insert-string 'cant-get-conf-stat)
      (lyskom-end-of-command))
   ((or lyskom-is-administrator
	(lyskom-member-p (conf-stat->supervisor conf-stat))
	(= lyskom-pers-no (conf-stat->conf-no conf-stat)))
      (initiate-get-text 'main 'lyskom-change-pres-or-motd-3
			 (cond
			  ((eq type 'pres)
			   (conf-stat->presentation conf-stat))
			  ((eq type 'motd)
			   (conf-stat->msg-of-day conf-stat)))
			 conf-stat type))
   (t
      (lyskom-format-insert 'not-supervisor-for
			    (conf-stat->name conf-stat))
      (lyskom-end-of-command))))


(defun lyskom-change-pres-or-motd-3 (text-mass conf-stat type)
  "Edit the presentation or motd for CONF-STAT.
Args: TEXT-MASS CONF-STAT TYPE.
TEXT-MASS is the old presentation or motd, or nil. If nil and writing a
presentationen an empty presentation is entered.
TYPE is either 'pres or 'motd, depending on what should be changed."
  (lyskom-edit-text
   lyskom-proc
   (lyskom-create-misc-list
    'recpt (cond
	    ((eq type 'motd)
	     (server-info->motd-conf lyskom-server-info))
	    ((eq type 'pres)
	     (if (conf-type->letterbox (conf-stat->conf-type conf-stat))
		 (server-info->pers-pres-conf lyskom-server-info)
	       (server-info->conf-pres-conf lyskom-server-info)))))
   (conf-stat->name conf-stat)
   (if (and text-mass
	    (string-match "\n" (text->text-mass text-mass)))
       (substring (text->text-mass text-mass) (match-end 0))
     (if (eq type 'pres)
	 (lyskom-get-string 'presentation-form)
       ""))
   (cond
    ((eq type 'pres) 'lyskom-set-presentation)
    ((eq type 'motd) 'lyskom-set-conf-motd))
   (conf-stat->conf-no conf-stat)))
   

;;; ================================================================
;;;           Ta bort lapp p} d|rren - delete conf-motd

;;; Author: Linus Tolke (& Inge Wallin)


(defun kom-unset-conf-motd ()
  "Removes motd for a person or a conference."
  (interactive)
  (lyskom-start-of-command 'kom-unset-conf-motd)
  (lyskom-completing-read
   'main 'lyskom-unset-conf-motd
   (lyskom-get-string 'who-to-remove-motd-for)
   nil nil ""))


(defun lyskom-unset-conf-motd (conf)
  "Removes motd for person or conference CONF which can be conf-no or 
conf-stat."
  (cond
   ((null conf)				;+++ annan felhantering.
    (lyskom-insert-string 'cant-get-conf-stat)
    (lyskom-end-of-command))
   ((numberp conf)
    (initiate-get-conf-stat 'main 'lyskom-unset-conf-motd conf))
   ((or lyskom-is-administrator
	(lyskom-member-p (conf-stat->supervisor conf)))
    (lyskom-set-conf-motd 0 (conf-stat->conf-no conf))
    (lyskom-end-of-command))
   (t
    (lyskom-format-insert 'not-supervisor-for
			  (conf-stat->name conf))
    (lyskom-end-of-command))))
  

;;; ================================================================
;;;               G} till M|te - Go to a conference.

;;; Author: ???


(defun kom-go-to-conf ()
  "Select a certain conference.
The user is prompted for the name of the conference.
If s/he was already reading a conference that conference will be put
back on lyskom-to-do-list."
  (interactive)
  (lyskom-start-of-command 'kom-go-to-conf)
  (lyskom-completing-read 'main 'lyskom-go-to-conf-handler
			  (lyskom-get-string 'go-to-conf-p) nil nil ""))


(defun lyskom-go-to-conf-handler (conf &optional pers-conf-stat)
  "Go to the conference in CONF. CONF can be conf-no of conf-stat.
Allowed conferences are conferences and the mailboxes you are member of."
  (cond
   ((numberp conf)
    (if (= conf lyskom-current-conf)
	(lyskom-end-of-command)
      (lyskom-collect 'main)
      (initiate-get-conf-stat 'main nil conf)
      (initiate-get-conf-stat 'main nil lyskom-pers-no)
      (lyskom-use 'main 'lyskom-go-to-conf-handler)))
   ((lyskom-conf-stat-p conf)
    (let ((membership (lyskom-member-p
		       (conf-stat->conf-no conf))))
      (lyskom-format-insert 'go-to-conf
			    (conf-stat->name conf))
      (cond
       (membership
	(lyskom-do-go-to-conf conf membership))
       ((conf-type->letterbox (conf-stat->conf-type conf))
	(lyskom-format-insert 'cant-go-to-his-mailbox
			      (conf-stat->name conf))
	(lyskom-end-of-command))
       (t
	(progn
	  (lyskom-format-insert 'not-member-of-conf
				(conf-stat->name conf))
	  (lyskom-scroll)
	  (if (j-or-n-p (lyskom-get-string 'want-become-member))
	      (lyskom-add-member-2 (conf-stat->conf-no conf)
				   lyskom-pers-no
				   'lyskom-fixup-and-go-to-conf
				   (conf-stat->conf-no conf))
	    (progn
	      (lyskom-insert-string 'no-ok)
	      (lyskom-end-of-command))))))))))


(defun lyskom-fixup-and-go-to-conf (conf-no)
  "Prefetches and after lyskom-member-in-conf and then goes to CONF-NO."
  (initiate-get-conf-stat 'main 'lyskom-do-go-to-conf conf-no
			  (lyskom-member-p conf-no)))

 
(defun lyskom-do-go-to-conf (conf-stat membership)
  "Go to a conference. Args: CONF-STAT MEMBERSHIP.
 Put a read-info of type CONF first on lyskom-reading-list.
Args: CONF-STAT MEMBERSHIP"
  (let ((priority (lyskom-get-current-priority)))
    (lyskom-maybe-move-unread nil)
    (if conf-stat
	(lyskom-set-mode-line conf-stat))
    (cond
     ((not conf-stat)			;+++ Use another error-check.
      (lyskom-insert-string 'someone-deleted-that-conf)
      (lyskom-end-of-command))

     ((let ((r 0)
	    (len (read-list-length lyskom-to-do-list))
	    (list (read-list->all-entries lyskom-to-do-list))
	    (found nil))
	(while (and (not found)
		    (< r len))
	  (if (and (read-info->conf-stat (read-list->nth lyskom-to-do-list r))
		   (= (conf-stat->conf-no conf-stat)
		      (conf-stat->conf-no 
		       (read-info->conf-stat 
			(read-list->nth lyskom-to-do-list r)))))
	      (setq found t)
	    (++ r)))
	(if found
	    (let ((list (read-list->nth lyskom-to-do-list r)))
	      (read-list-enter-first list lyskom-reading-list)
	      (read-list-delete-read-info (conf-stat->conf-no conf-stat)
					  lyskom-to-do-list)
	      (read-list-enter-first list lyskom-to-do-list)))
	found)
      (set-read-info->priority (read-list->first lyskom-reading-list)
			       priority)
      (lyskom-enter-conf conf-stat (read-list->first lyskom-reading-list))
      (lyskom-end-of-command))

     (t
      (lyskom-go-to-empty-conf conf-stat)))))


(defun lyskom-go-to-conf-handle-map (map membership conf-stat priority)
  "Add info about unread texts in a conf to the lyskom-reading-list.
Args: MAP MEMBERSHIP CONF-STAT PRIORITY.
MAP is the mapping from local to global text-nos for (at least) all
texts after membership->last-text-read. MEMBERSHIP is info about the
user's membership in the conference.
PRIORITY is the smallest priority the conference can be read with."
  (let ((unread (lyskom-list-unread map membership)))
    (if unread
	(progn
	  (read-list-delete-read-info (conf-stat->conf-no conf-stat)
				      lyskom-to-do-list)
	  (set-read-list-empty lyskom-reading-list)
	  (read-list-enter-first (lyskom-create-read-info
				  'CONF conf-stat
				  (membership->priority membership)
				  (lyskom-create-text-list unread))
				 lyskom-reading-list)
	  (read-list-enter-first (read-list->first lyskom-reading-list)
				 lyskom-to-do-list)
	  (if (> priority (read-info->priority
			   (read-list->first lyskom-reading-list)))
;+++Priorities
	      (set-read-info->priority (read-list->first lyskom-reading-list)
				       priority))
	  (lyskom-enter-conf conf-stat (read-list->first lyskom-reading-list))
	  (lyskom-end-of-command))
      (lyskom-go-to-empty-conf conf-stat))))


(defun lyskom-go-to-empty-conf (conf-stat)
  "Go to a conference with no unseen messages. Args: CONF-STAT."
  (initiate-pepsi 'main nil (conf-stat->conf-no conf-stat))
  (setq lyskom-current-conf (conf-stat->conf-no conf-stat))
  (lyskom-format-insert 'conf-all-read (conf-stat->name conf-stat))
  (lyskom-end-of-command))


(defun lyskom-get-current-priority ()
  "Return the current priority level."
  (or (read-info->priority (read-list->first 
			    lyskom-reading-list))
      (read-info->priority (read-list->first
			    lyskom-to-do-list))
      -1))


;;; ================================================================
;;;                 Skriva inl{gg - write text

;;; Author: ???


(defun kom-write-text ()
  "write a text."
  (interactive)
  (lyskom-start-of-command 'kom-write-text)
  (if (zerop lyskom-current-conf)
      (progn
	(lyskom-insert-string 'no-in-conf)
	(lyskom-end-of-command))
    (lyskom-tell-internat 'kom-tell-write-text)
    (lyskom-edit-text lyskom-proc
		      (lyskom-create-misc-list 'recpt
					       lyskom-current-conf)
		      "" "")))


;;; ================================================================
;;;                 Lista Personer - List persons

;;; Author: ceder


(defun kom-list-persons (match)
  "List all persons whose name matches MATCH (a string)."
  (interactive (list (lyskom-read-string 
		      (lyskom-get-string 'search-for-pers))))
  (lyskom-start-of-command 'kom-list-persons)
  (initiate-lookup-name 'main 'lyskom-list-pers-handler match))


(defun lyskom-list-pers-handler (conf-list)
  "Get names of all persons on conf-list and display them."
  (let ((list-of-pers-nos (lyskom-extract-persons conf-list)))
    (lyskom-traverse
     conf-no list-of-pers-nos
     (initiate-get-conf-stat 'main 'lyskom-list-pers-print
			     conf-no))
    (lyskom-run 'main 'lyskom-end-of-command)))


(defun lyskom-list-pers-print (conf-stat)
  "Print name of the person in CONF-STAT for kom-list-persons."
   (if conf-stat
       (lyskom-insert (concat (format "%4d " (conf-stat->conf-no conf-stat))
			      (conf-stat->name conf-stat)
			      "\n"))))


;;; ================================================================
;;;              Lista M|ten - List conferences

;;; Author: ceder


(defun kom-list-conferences (match)
  "List all conferences whose name matches MATCH (a string).
Those that you are not a member in will be marked with an asterisk."
  (interactive (list (lyskom-read-string
		      (lyskom-get-string 'search-for-conf))))
  (lyskom-start-of-command 'kom-list-conferences)
  (initiate-lookup-name 'main 'lyskom-list-conferences match))


(defun lyskom-list-conferences (conf-list)
  "Get names of all conferences on conf-list and display them."
  (let ((list-of-conf-nos (lyskom-extract-confs conf-list)))
    (lyskom-traverse conf-no list-of-conf-nos
		     (initiate-get-conf-stat 'main 'lyskom-list-conf-print
					     conf-no)))
  (lyskom-run 'main 'lyskom-end-of-command))


(defun lyskom-list-conf-print (conf-stat)
  "Print a line of info about conf-stat.
If you are not member in the conference it will be flagged with an asterisk."
  (if (not conf-stat)
      nil
    (lyskom-insert (concat (format "%4d %c "
				   (conf-stat->conf-no conf-stat)
				   (if (lyskom-member-p
					(conf-stat->conf-no conf-stat))
				       32
				     ?*))
			   (conf-stat->name conf-stat)
			   "\n"))))

;;; ================================================================
;;;                 [ndra namn - Change name

;;; Author: Inge Wallin
;;; Changed by: Peter Eriksson(?)
;;; Changed again: Inge Wallin


(defun kom-change-name ()
  "Change the name of a person or conference."
  (interactive)
  (lyskom-start-of-command 'kom-change-name)
  (lyskom-completing-read-conf-stat 'main 'lyskom-change-name
				    (lyskom-get-string 'name-to-be-changed)
				    nil nil ""))


(defun lyskom-change-name (conf-stat)
  "Change the name of the conf whose conf-stat is CONF-STAT."
  (if (null conf-stat)
      (progn
	(lyskom-insert-string 'no-such-conf-or-pers)
	(lyskom-end-of-command))
    (progn
      (lyskom-insert (conf-stat->name conf-stat))
      (lyskom-scroll)
      (lyskom-tell-internat 'kom-tell-change-name)
      (setq name (read-from-minibuffer (lyskom-get-string 'new-name)))
      (initiate-change-name 'main 'lyskom-change-name-2
			    (conf-stat->conf-no conf-stat)
			    name name))))


(defun lyskom-change-name-2 (answer name)
  "Tell the user wether the name change was ok"
  (if answer
      (lyskom-format-insert 'change-name-done name)
    (lyskom-format-insert 'change-name-nope name))
  (lyskom-end-of-command))


;;; ================================================================
;;;                [ndra organisat|r - Change supervisor

;;; Author: Inge Wallin


(defun kom-change-supervisor ()
  "Change the supervisor of a person or conference."
  (interactive)
  (lyskom-start-of-command 'kom-change-supervisor)
  (lyskom-completing-read-conf-stat 'main 'lyskom-change-supervisor
				    (lyskom-get-string 'who-to-change-supervisor-for)
				    nil nil ""))


(defun lyskom-change-supervisor (conf-stat)
 "Ask who the new supervisor of the conf whose conf-stat is CONF-STAT will be."
  (if (null conf-stat)
      (progn
	(lyskom-insert-string 'no-such-conf-or-pers)
	(lyskom-end-of-command))
    (progn
      (lyskom-tell-internat 'kom-tell-change-supervisor)
      (lyskom-completing-read-conf-stat 'main 'lyskom-change-supervisor-2
					(lyskom-get-string 'new-supervisor)
					nil nil ""
					conf-stat))))


(defun lyskom-change-supervisor-2 (supervisor supervisee)
  "Let SUPERVISOR be the new supervisor of SUPERVISEE.
Both arguments are conf-statuses."
  (lyskom-format-insert 'change-supervisor-from-to
			(conf-stat->name supervisee)
			(conf-stat->name supervisor))
  (initiate-set-supervisor 'main 'lyskom-change-supervisor-3
			   (conf-stat->conf-no supervisee) 
			   (conf-stat->conf-no supervisor)
			   supervisee))


(defun lyskom-change-supervisor-3 (answer supervisee)
  "Tell the user wether the supervisor change was ok.
Args: ANSWER SUPERVISEE.
ANSWER is the answer from the initiate function and SUPERVISEE is the conf-stat
of the conference with new supervisor."
  (if answer
      (progn
	(lyskom-insert-string 'done)
	(cache-del-conf-stat (conf-stat->conf-no supervisee)))
    (lyskom-insert
     (lyskom-format 'change-supervisor-nope
	     (conf-stat->name supervisee))))
  (lyskom-end-of-command))


;;; ================================================================
;;;         Markera och Avmarkera - Mark and Unmark a text

;;; Author: Inge Wallin


(defun kom-mark-text (text-no-arg)
  "Mark a text. If the argument TEXT-NO-ARG is non-nil, the user has used
a prefix command argument."
  (interactive "P")
  (lyskom-start-of-command 'kom-mark-text)
  (lyskom-mark-text text-no-arg (lyskom-get-string 'text-to-mark) 1))


(defun kom-unmark-text (text-no-arg)
  "Unmark a text. If the argument TEXT-NO-ARG is non-nil, the user has used
a prefix command argument."
  (interactive "P")
  (lyskom-start-of-command 'kom-unmark-text)
  (lyskom-mark-text text-no-arg (lyskom-get-string 'text-to-unmark) 0))


(defun lyskom-mark-text (text-no-arg prompt mark)
  "Get the number of the text that is to be marked and do the marking.
Arguments: TEXT-NO-ARG: an argument as it is gotten from (interactive P)
PROMPT: A string that is used when prompting for a number.
MARK:   A number that is used as the mark."
  (let ((text-no (cond ((integerp text-no-arg) text-no-arg)
		       ((and text-no-arg
			     (listp text-no-arg))
			(car text-no-arg))
		       (t lyskom-current-text))))
    (setq text-no (lyskom-read-number prompt text-no))
    (if (not (eq mark 0))
	(setq mark
	      (or kom-default-mark
		  (lyskom-read-num-range
		   1 255 (lyskom-get-string 'what-mark) t))))
    (lyskom-insert (if (equal mark 0)
		       (lyskom-format 'unmarking-textno text-no)
		     (lyskom-format 'marking-textno text-no)))
    (initiate-mark-text 'main 'lyskom-mark-text-answer
			text-no mark text-no mark)))


(defun lyskom-mark-text-answer (answer text-no mark)
  "Handles the answer from the server after a marking or an unmarking."
  (if answer
      (progn
	(lyskom-insert-string 'done)
	(if (= mark 0)
	    (cache-del-marked-text text-no)
	  (cache-add-marked-text text-no mark)))
    (lyskom-insert-string 'nope))
  (lyskom-end-of-command))


;;; ================================================================
;;;          ]terse alla markerade - Review marked texts

;;; Author: Inge Wallin


(defun kom-review-marked-texts ()
  "Review marked texts with a certain mark."
  (interactive)
  (lyskom-start-of-command 'kom-review-marked-texts)
  (lyskom-review-marked-texts 
   (lyskom-read-num-range 
    1 255 (lyskom-get-string 'what-mark-to-view) t)))


(defun kom-review-all-marked-texts ()
  "Review all marked texts"
  (interactive)
  (lyskom-start-of-command 'kom-review-all-marked-texts)
  (lyskom-review-marked-texts 0))


(defun lyskom-review-marked-texts (mark-no)
  "Review all marked texts with the mark equal to MARK-NO. 
If MARK-NO == 0, review all marked texts."
  (let ((mark-list (cache-get-marked-texts))
	(text-list nil))
    (while (not (null mark-list))
      (let ((mark (car mark-list)))		
	(if (and mark
		 (or (eq mark-no 0)
		     (eq mark-no (mark->mark-type mark))))
	    (setq text-list (cons (mark->text-no mark)
				  text-list))))
      (setq mark-list (cdr mark-list)))
    (if (equal (length text-list) 0)
	(lyskom-insert (if (eq mark-no 0)
			   (lyskom-get-string 'no-marked-texts)
			 (lyskom-format 'no-marked-texts-mark mark-no)))
      (let ((read-info (lyskom-create-read-info
			'REVIEW-MARK nil 
			(lyskom-get-current-priority)
			(lyskom-create-text-list text-list) 
			nil t)))
	(read-list-enter-read-info read-info lyskom-reading-list t)
	(read-list-enter-read-info read-info lyskom-to-do-list t))))
  (lyskom-end-of-command))


;;; ================================================================
;;;                 [ndra L|senord - Change password

;;; Author: Inge Wallin


(defun kom-change-password ()
  "Change the password for a person."
  (interactive)
  (lyskom-start-of-command 'kom-change-password)
  (lyskom-completing-read 'main 'lyskom-change-password
			  (lyskom-get-string 'whos-passwd)
			  'person 'empty ""))


(defun lyskom-change-password (pers-no)
  "Get a PERS-NO from kom-change-password and ask for the new password.
If PERS-NO == 0, use the variable lyskom-pers-no."
  (let ((old-pw (silent-read (lyskom-get-string 'old-passwd)))
	(new-pw1 (silent-read (lyskom-get-string 'new-passwd)))
	(new-pw2 (silent-read (lyskom-get-string 'new-passwd-again))))

    (if (string= new-pw1 new-pw2)
	(progn
	  (lyskom-insert-string 'changing-passwd)
	  (initiate-set-passwd 'main 'lyskom-handle-command-answer
			       (if (zerop pers-no)
				   lyskom-pers-no
				 pers-no)
			       old-pw new-pw1))
      (lyskom-insert-string 'retype-dont-match)
      (lyskom-end-of-command))))


;;; ================================================================
;;;               (Se) Tiden - display time and date.


(defun kom-display-time ()
  "Ask server about time and date."
  (interactive)
  (lyskom-start-of-command 'kom-display-time)
  (initiate-get-time 'main 'lyskom-display-time)
  (lyskom-run 'main 'lyskom-end-of-command))


(defun lyskom-display-time (time)
  "Display time, formatted for the command kom-display-time.
args: TIME."
  (lyskom-format-insert 'time-is
			(+ (time->year time) 1900)
			(1+ (time->mon  time))
			(time->mday time)
			(time->hour time)
			(time->min  time)
			(time->sec  time)
			; Kult:
			(if (and (= (time->hour time)
				    (+ (/ (time->sec time) 10)
				       (* (% (time->sec time) 10) 10)))
				 (= (/ (time->min time) 10)
				    (% (time->min time) 10)))
			    (lyskom-get-string 'palindrome)
			  "")))


;;; ================================================================
;;;                Vilka ({r inloggade) - Who is on?

;;; Author: ???


(defun kom-who-is-on ()
  "Display a list of all connected users."
  (interactive)
  (lyskom-start-of-command 'kom-who-is-on)
  (initiate-who-is-on 'who-is-on 'lyskom-print-who-is-on))


(defun lyskom-print-who-is-on (who-info-list)
  "Print a list of all on who-info-list."
  (lyskom-insert
   (lyskom-return-who-info-line "     "
				(lyskom-get-string 'lyskom-name)
				(lyskom-get-string 'is-in-conf)))
  (if kom-show-where-and-what
      (lyskom-insert
       (lyskom-return-who-info-line "     "
				    (lyskom-get-string 'from-machine)
				    (lyskom-get-string 'is-doing))))

  (lyskom-insert
   (concat (make-string (- (lyskom-window-width) 2) ?-) "\n"))

  (let ((who-list (sort (append who-info-list nil)
			(function (lambda (who1 who2)
				    (< (who-info->connection who1)
				       (who-info->connection who2)))))))
    (lyskom-traverse
     who-info who-list
     (lyskom-collect 'who-is-on)
     (initiate-get-conf-stat 'who-is-on nil
			     (who-info->pers-no who-info))
     (initiate-get-conf-stat 'who-is-on nil
			     (who-info->working-conf who-info))
     (lyskom-use 'who-is-on  'lyskom-print-who-info who-info lyskom-session-no))
    (lyskom-run 'who-is-on 'lyskom-insert (concat
					   (make-string
					    (- (lyskom-window-width) 2) ?-)
					   "\n"))
    (lyskom-run 'who-is-on 'lyskom-insert
		(lyskom-format 'total-users
			(length who-list))))
  (lyskom-run 'who-is-on 'lyskom-run 'main 'lyskom-end-of-command))


(defun lyskom-print-who-info (conf-stat working who-info my-session-no)
  "Print a line about a user. Args: CONF-STAT WORKING WHO-INFO MY-SESSION-NO.
CONF-STAT refer to the user.
WORKING is the conf-stat of his current working conference.
WHO-INFO is the who-info.
MY-SESSION-NO is the session number of the running session."
  (lyskom-insert
   (lyskom-return-who-info-line (format "%4d%s" 
					(who-info->connection who-info)
					(if (= my-session-no
					       (who-info->connection who-info))
					    "*"
					  " "))
				(conf-stat->name conf-stat)
				(cond
				 ((conf-stat->name working))
				 (t (lyskom-get-string
				     'not-present-anywhere)))))
  (cond
   (kom-show-where-and-what
    (lyskom-insert
     (lyskom-return-who-info-line "     "
				  (lyskom-return-username who-info)
				  (concat "(" 
					  (who-info->doing-what who-info)
					  ")"))))))

		 
(defun lyskom-fix-str (len str)
  "Pad STR with space to LEN. Args: LEN STR."
  (while (< (length str) len)
    (setq str
	  (concat str
		  "                                                        ")))
  (substring str 0 len))


(defun lyskom-return-who-info-line (prefix string1 string2)
  "Return a formatted line (with reference to the current window width."
  (concat
   prefix
   (lyskom-fix-str (/ (* 37 (- (lyskom-window-width) 7)) 73)
		   string1)
   " "
   (lyskom-fix-str (/ (* 36 (- (lyskom-window-width) 7)) 73)
		   string2)
   "\n"))

    
(defun lyskom-window-width ()
  "Returns the width of the lyskom-window of the screen-width if not displayed."
  (let ((win (get-buffer-window (current-buffer))))
    (if win
	(window-width win)
      (screen-width))))


(defun lyskom-return-username (who-info)
  "Takes the username from the WHO-INFO and returns it on a better format."
  (let* ((username (who-info->username who-info))
	 (type (or 
		(string-match "\\([^%@.]+\\)%\\(.+\\)@\\([^%@.]+\\)" username)
		(string-match "\\([^%@.]+\\)@\\([^%@.]+\\)" username))))
    (if type
	(let ((name (substring username 0 (match-end 1)))
	      (sent (if (match-beginning 3)
			(substring username
				   (match-beginning 2)
				   (match-end 2))))
	      (gott (if (match-beginning 3)
			(substring username
				   (match-beginning 3)
				   (match-end 3))
		      (substring username
				 (match-beginning 2)
				 (match-end 2))))
	      (rest (substring username (match-end 0))))
	  (if (or (not sent)
		  (string= (downcase sent) (downcase gott))
		  (string= (downcase sent)
			   (downcase (concat gott rest))))
	      (concat name "@" gott rest)
	    (concat name "@" sent " (" gott rest ")")))
      username)))
  
	

;;; ================================================================
;;;                     Hoppa -  Jump over comments

;;; Author: Linus Tolke Y

;; Hoppa |ver alla inl{gg som {r kommentarer till detta inl{gg (recursivt)


(defun kom-jump (&optional text-no)
  "Jumps all comments to the current text. Descends recursively in comment tree.
If optional arg TEXT-NO is present then jump all comments to that text instead."
  (interactive (list
		(cond
		 ((null current-prefix-arg)
		  lyskom-current-text)
		 ((integerp current-prefix-arg)
		  current-prefix-arg)
		 ((and (listp current-prefix-arg)
		       (integerp (car current-prefix-arg))
		       (null (cdr current-prefix-arg)))
		  (car current-prefix-arg))
		 (t
		  (signal 'lyskom-internal-error '(kom-jump))))))
  (if text-no
      (progn
	(lyskom-start-of-command 'kom-jump)
	(initiate-get-text-stat 'main 'lyskom-jump text-no t)
	(lyskom-run 'main 'lyskom-end-of-command))
    (lyskom-start-of-command 'kom-jump)
    (lyskom-insert-string 'have-to-read)
    (lyskom-end-of-command)))


(defun lyskom-jump (text-stat mark-as-read)
  "Jump past TEXT-STAT and all comments to it.
Remove TEXT-STAT from all internal tables in the client.
If MARK-AS-READ is non-nil, also mark TEXT-STAT and all comments (and
footnotes) to it as read in the server."
  (cond
   (text-stat				;+++ annan errorhantering.
    ;; Should check that we are a member of at least one of
    ;; the recipients, and stop otherwise.
    (if mark-as-read
	(lyskom-mark-as-read text-stat))
    (lyskom-is-read (text-stat->text-no text-stat))
    (lyskom-traverse misc (text-stat->misc-info-list text-stat)
		     (cond
		      ((or (eq (misc-info->type misc) 'COMM-IN)
			   (eq (misc-info->type misc) 'FOOTN-IN))
		       (initiate-get-text-stat 'main
					       'lyskom-jump
					       (if (eq (misc-info->type misc)
						       'COMM-IN)
						   (misc-info->comm-in misc)
						 (misc-info->footn-in misc))
					       mark-as-read)))))))


;;; ================================================================
;;;                 Addera mottagare - Add recipient
;;;             Subtrahera mottagare - Subtract recipient

;;; Author: Inge Wallin


(defun kom-add-recipient (text-no-arg)
  "Add a recipient to a text. If the argument TEXT-NO-ARG is non-nil, 
the user has used a prefix command argument."
  (interactive "P")
  (lyskom-start-of-command 'kom-add-recipient)
  (lyskom-add-sub-recipient text-no-arg 
			    (lyskom-get-string 'text-to-add-recipient)
			    t))

(defun kom-add-copy (text-no-arg)
  "Add a recipient to a text. If the argument TEXT-NO-ARG is non-nil, 
the user has used a prefix command argument."
  (interactive "P")
  (lyskom-start-of-command 'kom-add-copy)
  (lyskom-add-sub-recipient text-no-arg 
			    (lyskom-get-string 'text-to-add-copy)
			    'copy))


(defun kom-sub-recipient (text-no-arg)
  "Subtract a recipient from a text. If the argument TEXT-NO-ARG is non-nil, 
the user has used a prefix command argument."
  (interactive "P")
  (lyskom-start-of-command 'kom-sub-recipient)
  (lyskom-add-sub-recipient text-no-arg
			    (lyskom-get-string 'text-to-delete-recipient)
			    nil))


(defun lyskom-add-sub-recipient (text-no-arg prompt do-add)
  "Get the number of the text that is to be added or subtracted a recipient
to/from 
Arguments: TEXT-NO-ARG: an argument as it is gotten from (interactive P)
PROMPT: A string that is used when prompting for a number.
DO-ADD: NIL if a recipient should be subtracted.
        Otherwise a recipient is added"
  (let ((text-no (cond ((null text-no-arg) lyskom-current-text)
		       (t text-no-arg))))
    (setq text-no (lyskom-read-number prompt text-no))
    (lyskom-completing-read-conf-stat
     'main 'lyskom-add-sub-recipient-2
     (cond
      ((eq do-add 'copy)
       (lyskom-get-string 'who-to-add-copy-q))
      (do-add (lyskom-get-string 'who-to-add-q))
      (t (lyskom-get-string 'who-to-sub-q)))
     nil nil "" text-no do-add)))


(defun lyskom-add-sub-recipient-2 (conf-stat text-no do-add)
  "Tell what we do and do the call to the server."
  (if do-add
      (progn
	(lyskom-format-insert (if (eq do-add 'copy)
				  'adding-name-as-copy
				'adding-name-as-recipient)
			      (conf-stat->name conf-stat)
			      text-no)
	(initiate-add-recipient 
	 'main 'lyskom-handle-command-answer
	 text-no (conf-stat->conf-no conf-stat) (if (eq do-add 'copy)
						    'cc-recpt
						  'recpt)))
    (lyskom-format-insert 'remove-name-as-recipient
			  (conf-stat->name conf-stat)
			  text-no)
    (initiate-sub-recipient 
     'main 'lyskom-handle-command-answer
     text-no (conf-stat->conf-no conf-stat)))
  (cache-del-text-stat text-no))


;;; ================================================================
;;;                 Addera kommentar - Add comment
;;;             Subtrahera kommentar - Subtract comment

;;; Author: Lars Willf|r

(defun kom-add-comment (text-no-arg)
  "Add a text as a comment to a text. If the argument TEXT-NO-ARG is non-nil,
the user has used a prefix command argument."
  (interactive "P")
  (lyskom-start-of-command 'kom-add-comment)
  (lyskom-add-sub-comment text-no-arg 
			  (lyskom-get-string 'text-to-add-comment-to)
			  t))


(defun kom-sub-comment (text-no-arg)
  "Subtract a recipient from a text. If the argument TEXT-NO-ARG is non-nil,
the user has used a prefix command argument."
  (interactive "P")
  (lyskom-start-of-command 'kom-sub-comment)
  (lyskom-add-sub-comment text-no-arg
			  (lyskom-get-string 'text-to-delete-comment-from)
			  nil))


(defun lyskom-add-sub-comment (text-no-arg prompt do-add)
  "Get the number of the text that is going to have a comment added to it or
subtracted from it
Arguments: TEXT-NO-ARG: an argument as it is gotten from (interactive P)
PROMPT: A string that is used when prompting for a number.
DO-ADD: NIL if a comment should be subtracted.
        Otherwise a comment is added"
  (let ((text-no (if (null text-no-arg)
		     lyskom-current-text
		   text-no-arg))
	(comment-text-no))
    (setq text-no (lyskom-read-number prompt text-no))
    (setq comment-text-no
	  (lyskom-read-number
	   (if do-add
	       (lyskom-get-string 'text-to-add-q)
	     (lyskom-get-string 'text-to-remove-q))
	   (if (= text-no lyskom-current-text)
	       nil
	     lyskom-current-text)))
    (lyskom-insert 
     (if do-add
	 (lyskom-format 'add-comment-to comment-text-no text-no)
       (lyskom-format 'sub-comment-to comment-text-no text-no)))
    (if do-add
	(initiate-add-comment 'main
			      'lyskom-handle-command-answer
			      comment-text-no
			      text-no)
      (initiate-sub-comment 'main
			    'lyskom-handle-command-answer
			    comment-text-no
			    text-no))
    (cache-del-text-stat text-no)
    (cache-del-text-stat comment-text-no)))


;;; ================================================================
