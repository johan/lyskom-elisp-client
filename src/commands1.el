;;;;;
;;;;; $Id: commands1.el,v 44.9 1996-10-08 12:25:48 nisse Exp $
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
;;;; File: commands1.el
;;;;
;;;; This file contains the code for some of the high level commands.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: commands1.el,v 44.9 1996-10-08 12:25:48 nisse Exp $\n"))


;;; ================================================================
;;;                  F} uppmuntran - Get appreciation 

;;; Author: Inge Wallin


(def-kom-command kom-get-appreciation ()
  "Give the user a little light in the dark"
  (interactive)
  (lyskom-insert-string 'appreciation))
  

;;; ================================================================
;;;                      F} Sk{ll - Get abuse

;;; Author: Inge Wallin


(def-kom-command kom-get-abuse ()
  "Give the user a little verbal abuse."
  (interactive)
  (lyskom-insert-string 'abuse))


;;; ================================================================
;;;            Utpl}na - Delete a person or a conference

;;; Author: Inge Wallin


(def-kom-command kom-delete-conf ()
  "Delete a person or a conference."
  (interactive)
  (let ((conf-stat 
	 (lyskom-read-conf-stat (lyskom-get-string 'what-conf-to-delete)
				'(all) nil nil t)))
    (if conf-stat
	(if (lyskom-ja-or-nej-p
	     (lyskom-format 'confirm-delete-pers-or-conf
			    (if (conf-type->letterbox (conf-stat->conf-type 
						       conf-stat))
				(lyskom-get-string 'the-pers)
			      (lyskom-get-string 'the-conf))
			    (conf-stat->name conf-stat)))
	    (if (blocking-do 'delete-conf
			     (conf-stat->conf-no conf-stat))
		(lyskom-format-insert 'conf-is-deleted
				      (conf-stat->name conf-stat))
	      (lyskom-format-insert 'you-could-not-delete
				    conf-stat))
	  (lyskom-insert-string 'deletion-not-confirmed))
      (lyskom-insert-string 'somebody-else-deleted-that-conf))))


;;; ================================================================
;;;                Radera (text) - Delete a text

;;; Author: Inge Wallin


(def-kom-command kom-delete-text (text-no-arg)
  "Delete a text. Argument: TEXT-NO"
  (interactive "P")
  (let ((text-no (cond ((null text-no-arg) 0)
		       ((integerp text-no-arg) text-no-arg)
		       ((listp text-no-arg) (car text-no-arg))
		       (t 0))))
    (if (zerop text-no)
	(setq text-no 
	      (lyskom-read-number (lyskom-get-string 'what-text-to-delete)
				  lyskom-current-text)))
    (lyskom-format-insert 'deleting-text text-no)
    (lyskom-report-command-answer (blocking-do 'delete-text text-no))))


;;; ================================================================
;;;        ]terse presentation - Review the presentation
;;;               for a person or a conference

;;; Author: Inge Wallin


(defun kom-review-presentation (&optional who)
  "Review the presentation for a person or a conference."
  (interactive)
  (lyskom-start-of-command 'kom-review-presentation)
  (let ((end-of-command-taken-care-of))
    (unwind-protect
        (let ((conf-stat 
               (if who
                   (blocking-do 'get-conf-stat who)
                 (lyskom-read-conf-stat 
                          (lyskom-get-string 'presentation-for-whom)
                          '(all)
                          nil "" t))))
          (if (null conf-stat)
              (lyskom-insert-string 'somebody-deleted-that-conf)
            (lyskom-format-insert 'review-presentation-of
                                  conf-stat)
            (if (/= (conf-stat->presentation conf-stat) 0)
                (lyskom-view-text (conf-stat->presentation conf-stat))
              (lyskom-format-insert 'has-no-presentation
                                    conf-stat))))
      (if end-of-command-taken-care-of
          nil
        (lyskom-end-of-command)))))



;;; ================================================================
;;;          ]terse det kommenterade - View commented text

;;; Author: Inge Wallin
;;; Modified by: David K}gedal

(def-kom-command kom-view-commented-text ()
  "View the commented text.
If the current text is comment to (footnote to) several text then the first
text is shown and a REVIEW list is built to shown the other ones."
  (interactive)
  (if lyskom-current-text
      (progn
	(lyskom-tell-internat 'kom-tell-read)
	(lyskom-view-commented-text
	 (blocking-do 'get-text-stat lyskom-current-text)))
    (lyskom-insert-string 'have-to-read)))


(def-kom-command kom-view-previous-commented-text ()
 "View the text the previous text commented.
If the previously viewed text is a comment to (footnote to) several
texts then the first text is shown and a REVIEW list is built to show
the other ones."
	      (interactive)
	      (if lyskom-previous-text
		  (progn
		    (lyskom-tell-internat 'kom-tell-read)
		    (lyskom-view-commented-text
		     (blocking-do 'get-text-stat lyskom-previous-text)))
		(lyskom-insert-string 'confusion-what-to-view)))

(defun lyskom-view-commented-text (text-stat)
  "Handles the return from the initiate-get-text-stat, displays and builds list."
  (let* ((misc-info-list (and text-stat
                              (text-stat->misc-info-list text-stat)))
         (misc-infos (and misc-info-list
                          (append (lyskom-misc-infos-from-list
                                   'COMM-TO misc-info-list)
                                  (lyskom-misc-infos-from-list
                                   'FOOTN-TO misc-info-list))))
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
          (lyskom-format-insert 'review-text-no 
                                (car text-nos))
          (if (cdr text-nos)
              (read-list-enter-read-info
               (lyskom-create-read-info
                'REVIEW nil (lyskom-get-current-priority)
                (lyskom-create-text-list (cdr text-nos))
                lyskom-current-text)
               lyskom-reading-list t))
          (lyskom-view-text (car text-nos)))
      (lyskom-insert-string 'no-comment-to))))


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
;;; Rewritten using read-conf-no by Linus Tolke (4=>1)

(def-kom-command kom-send-letter (&optional pers-no)
  "Send a personal letter to a person or a conference."
  (interactive)
  (condition-case nil
      (progn
        (lyskom-tell-internat 'kom-tell-write-letter)
	;; If there was a motd, which is now removed we have to
	;; refetch the conf-stat to know that.
        (let* ((tono (or pers-no
                         (lyskom-read-conf-no
                          (lyskom-get-string 'who-letter-to)
                          '(all) nil nil t)))
               (conf-stat (blocking-do 'get-conf-stat tono)))
	  (cache-del-conf-stat tono)
          (if (if (zerop (conf-stat->msg-of-day conf-stat))
                  t
                (progn
                  (recenter 1)
                  (lyskom-format-insert 'has-motd 
                                        conf-stat)
                  (lyskom-view-text (conf-stat->msg-of-day conf-stat))
                  (if (lyskom-j-or-n-p (lyskom-get-string 'motd-persist-q))
                      t
                    nil)))
              (if (= tono lyskom-pers-no)
                  (lyskom-edit-text lyskom-proc
                                    (lyskom-create-misc-list 'recpt tono)
                                    "" "")
                (lyskom-edit-text lyskom-proc
                                  (lyskom-create-misc-list 'recpt tono
                                                           'recpt lyskom-pers-no)
                                  "" "")))))
    (quit (signal 'quit "Quitting in letter"))))


;;; ================================================================
;;;           Bli medlem i m|te - Become a member of a conference
;;;             Addera medlem - Add somebody else as a member

;;; Author: ???
;;; Rewritten by: David K}gedal


;; Add another person

(def-kom-command kom-add-member ()
  "Add a person as a member of a conference.
Ask for the name of the person, the conference to add him/her to."
  (interactive)
  (let* ((who (lyskom-read-conf-stat (lyskom-get-string 'who-to-add)
				     '(pers) nil nil t))
	 (whereto (lyskom-read-conf-stat (lyskom-get-string 'where-to-add)
					 '(all) nil nil t))
	 (pers-stat (blocking-do 'get-pers-stat (conf-stat->conf-no who))))
    (lyskom-add-member-answer (lyskom-try-add-member whereto who pers-stat)
			      whereto who)))



;; Add self

(def-kom-command kom-add-self (&optional conf)
  "Add this person as a member of a conference."
  (interactive)
  (let ((whereto (if conf (blocking-do 'get-conf-stat conf)
                   (lyskom-read-conf-stat 
                    (lyskom-get-string 'where-to-add-self)
                    '(all) nil "" t)))
        (who (blocking-do 'get-conf-stat lyskom-pers-no))
        (pers-stat (blocking-do 'get-pers-stat lyskom-pers-no)))
    (lyskom-add-member-answer (lyskom-try-add-member whereto who pers-stat)
                              whereto who)))



;;; NOTE: This function is also called from lyskom-go-to-conf-handler
;;;       and from lyskom-create-conf-handler.

(defun lyskom-add-member-by-no (conf-no pers-no &optional thendo &rest data)
  "Fetch info to be able to add a person to a conf.
Get the conf-stat CONF-NO for the conference and the conf-stat and pers-stat 
for person PERS-NO and send them into lyskom-try-add-member."
  (blocking-do-multiple ((whereto (get-conf-stat conf-no))
                         (who (get-conf-stat pers-no))
                         (pers-stat (get-pers-stat pers-no)))
    (let ((result (lyskom-try-add-member whereto who pers-stat)))
      (lyskom-add-member-answer result whereto who)
      (if thendo
          (apply thendo data))
      result)))


(defun lyskom-try-add-member (conf-conf-stat pers-conf-stat pers-stat)
  "Add a member to a conference.
Args: CONF-CONF-STAT PERS-CONF-STAT PERS-STAT
CONF-CONF-STAT: the conf-stat of the conference the person is being added to
PERS-CONF-STAT: the conf-stat of the person being added.
PERS-STAT: the pers-stat of the person being added.

Returns t if it was possible, otherwise nil."
  (if (or (null conf-conf-stat)
	  (null pers-conf-stat))
      nil				; We have some problem here.
    (let ((priority
	   (if (/= lyskom-pers-no (conf-stat->conf-no pers-conf-stat))
	       100			; When adding someone else
	     (if (and (numberp kom-membership-default-priority)
		      (< kom-membership-default-priority 256)
		      (> kom-membership-default-priority 0))
		 kom-membership-default-priority
	       (lyskom-read-num-range
		1 255 (lyskom-get-string 'priority-q)))))
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

      (if (= (conf-stat->conf-no pers-conf-stat)
	     lyskom-pers-no)
	  (lyskom-format-insert 'member-in-conf
				conf-conf-stat)
	(lyskom-format-insert 'add-member-in
			      pers-conf-stat
			      conf-conf-stat))
      (blocking-do 'add-member 
		   (conf-stat->conf-no conf-conf-stat)
		   (conf-stat->conf-no pers-conf-stat)
		   priority where))))


(defun lyskom-add-member-answer (answer conf-conf-stat pers-conf-stat)
  "Handle the result from an attempt to add a member to a conference."
  (if (null answer)
      (progn
	(lyskom-insert-string 'nope)
	(if (conf-type->rd_prot (conf-stat->conf-type conf-conf-stat))
	    ;; The conference is protected. Tell the user to contact
	    (let ((supervisorconf (blocking-do
				   'get-conf-stat
				   (conf-stat->supervisor conf-conf-stat))))
	      (if supervisorconf
		  (lyskom-format-insert 'is-read-protected-contact-supervisor
					conf-conf-stat
					supervisorconf)
		(lyskom-format-insert 'cant-find-supervisor
				      conf-conf-stat)))
	  (lyskom-format-insert 'error-code
				(lyskom-get-error-text lyskom-errno)
				lyskom-errno)))

    (lyskom-insert-string 'done)
    ;;+++Borde {ndra i cachen i st{llet.
    (cache-del-pers-stat (conf-stat->conf-no pers-conf-stat))
    ;;+++Borde {ndra i cachen i st{llet.
    (cache-del-conf-stat (conf-stat->conf-no conf-conf-stat))
    (if (= (conf-stat->conf-no pers-conf-stat)
	   lyskom-pers-no)
	(progn				; Adding myself
	  (lyskom-add-membership
	   (blocking-do 'query-read-texts
			lyskom-pers-no 
			(conf-stat->conf-no conf-conf-stat))
	   conf-conf-stat)))))



(defun lyskom-add-membership (membership conf-stat)
  "Adds MEMBERSHIP to the sorted list of memberships.
Args: MEMBERSHIP CONF-STAT THENDO DATA
Also adds to lyskom-to-do-list."
  ;;; +++ What should this do if the priority is lower than
  ;;; kom-session-priority?
  (if membership
      (progn
	(setq lyskom-membership (sort (cons membership lyskom-membership)
				      'lyskom-membership-<))
	;; (let ((map (blocking-do
	;; 	       'get-map
	;; 	       (conf-stat->conf-no conf-stat)
	;; 	       (max (1+ (membership->last-text-read membership))
	;; 		    (conf-stat->first-local-no conf-stat))
	;; 	       (conf-stat->no-of-texts conf-stat))))
	;;   (if map
	;; 	 (let ((texts (skip-first-zeros
	;; 		       (sort (listify-vector (map->text-nos map))
	;; 			     '<))))
	;; 	   (if texts
	;; 	       (read-list-enter-read-info
	;; 		(lyskom-create-read-info
	;; 		 'CONF conf-stat
	;; 		 (membership->priority membership)
	;; 		 (lyskom-create-text-list
	;; 		  texts)
	;; 		 nil nil)
	;; 		lyskom-to-do-list))))	  
	;;   )
	(lyskom-prefetch-map-using-conf-stat
	 conf-stat
	 (1+ (membership->last-text-read membership))
	 membership))
    (lyskom-insert-string 'conf-does-not-exist))
  )



;;; ================================================================
;;;     Uttr{d - Subtract yourself as a member from a conference
;;;     Uteslut medlem - Subtract somebody else as a member

;;; Author: David Byers
;;; Based on code by Inge Wallin


;; Subtract another person

(def-kom-command kom-sub-member ()
  "Subtract a person as a member from a conference. Ask for the name
of the person."
  (interactive)
  (lyskom-sub-member
   (lyskom-read-conf-stat (lyskom-get-string 'who-to-exclude)
                          '(pers) nil "" t)
   (lyskom-read-conf-stat (lyskom-get-string 'where-from-exclude) 
			  '(all) nil "" t)))


(def-kom-command kom-sub-self (&optional conf)
  "Subtract this person as a member from a conference."
  (interactive)
  (lyskom-sub-member 
   (blocking-do 'get-conf-stat lyskom-pers-no)
   (if conf (blocking-do 'get-conf-stat conf)
     (lyskom-read-conf-stat (lyskom-get-string 'leave-what-conf)
                            '(all) nil 
                            (let ((ccn 
                                   (if (or (zerop lyskom-current-conf))
                                       ""
                                     (conf-stat->name
                                      (blocking-do 'get-conf-stat
                                                   lyskom-current-conf)))))
                              (if ccn
                                  (cons ccn 0)
                                "")) t))))

(defun lyskom-sub-member (pers conf)
  "Remove the person indicated by PERS as a member of CONF."
  (let ((reply nil)
	(self (= (conf-stat->conf-no pers) lyskom-pers-no)))
    (cond ((null pers) (lyskom-insert-string 'error-fetching-person))
	  ((null conf) (lyskom-insert-string 'error-fetching-conf))
	  (t
	   (if self
	       (lyskom-format-insert 'unsubscribe-to conf)
	     (lyskom-format-insert 'exclude-from pers conf))

	   (setq reply (blocking-do 'sub-member
				    (conf-stat->conf-no conf)
				    (conf-stat->conf-no pers)))
	   (if self
	       (lyskom-set-membership (blocking-do 'get-membership
						   lyskom-pers-no)))
	   (if (not reply)
	       (lyskom-format-insert 'unsubscribe-failed
				     (if self
					 (lyskom-get-string 'You)
				       (conf-stat->name pers))
				     (conf-stat->name conf))
	     (lyskom-insert-string 'done)
	     (if (and self
		      (= (conf-stat->conf-no conf)
			 lyskom-current-conf))
		 (progn 
		   (set-read-list-empty lyskom-reading-list)
		   (setq lyskom-current-conf 0)))
	     (read-list-delete-read-info (conf-stat->conf-no conf)
					 lyskom-to-do-list))))))
	   

	 

;;; ================================================================
;;;                 Skapa m|te - Create a conference

;;; Author: ???


(def-kom-command kom-create-conf (&optional name)
  "Create a conference."
  (interactive)
  (let* ((conf-name (or name
			(lyskom-read-string
			 (lyskom-get-string 'name-of-conf))))
	 (open (j-or-n-p (lyskom-get-string 'anyone-member)))
	 (secret (if (not open)
		     (j-or-n-p (lyskom-get-string 'secret-conf))))
	 (orig (j-or-n-p (lyskom-get-string 'comments-allowed)))
         (anarchy (j-or-n-p (lyskom-get-string 'anonymous-allowed)))
	 (conf-no (blocking-do 'create-conf 
			       conf-name
			       (lyskom-create-conf-type (not open) 
							(not orig)
							secret
							nil
                                                        anarchy
                                                        nil
                                                        nil
                                                        nil))))
    (if (null conf-no)
	(progn
	  (lyskom-format-insert 'could-not-create-conf
				conf-name)
	  (lyskom-format-insert 'error-code
				(lyskom-get-error-text lyskom-errno)
				lyskom-errno))
      (progn
	(let ((conf-stat (blocking-do 'get-conf-stat conf-no)))
	  (lyskom-format-insert 'created-conf-no-name 
				(or conf-stat conf-no)
				(or conf-stat conf-name)
				(if conf-stat
				    (lyskom-default-button 'conf conf-stat)
				  nil)))
	(lyskom-scroll)
	(lyskom-add-member-by-no conf-no lyskom-pers-no
				 (if secret
				     nil ; Don't write a presentation
				   'lyskom-create-conf-handler-2)
				 conf-no conf-name)))))


(defun lyskom-create-conf-handler-2 (conf-no conf-name)
  "Starts editing a presentation for the newly created conference.
This does lyskom-end-of-command"
  (lyskom-tell-internat 'kom-tell-conf-pres)
  (let ((conf (blocking-do 'get-conf-stat conf-no)))
    (if conf
        (lyskom-dispatch-edit-text lyskom-proc
                                   (lyskom-create-misc-list
                                    'recpt
                                    (server-info->conf-pres-conf 
                                     lyskom-server-info))
                                   conf-name ""
                                   'lyskom-set-presentation conf-no))))


(defun lyskom-set-presentation (text-no conf-no)
  "Set presentation of a conference. Args: text-no conf-no."
  (initiate-set-presentation 'background nil conf-no text-no)
  (cache-del-conf-stat conf-no))	;+++Borde {ndra i cachen i st{llet.
					;+++ Kan tas bort n{r det existerar 
					;asynkrona meddelanden som talar om att
					;n}got {r {ndrat.
    

(defun lyskom-set-conf-motd (text-no conf-no)
  "Set motd of a conference. Args: text-no conf-no."
  (initiate-set-conf-motd 'background nil conf-no text-no)
  (cache-del-conf-stat conf-no))	;+++Borde {ndra i cachen i st{llet.
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
		 ((listp current-prefix-arg) 
		  (lyskom-read-number (lyskom-get-string 'what-comment-no)))
		 (t
		  (signal 'lyskom-internal-error '(kom-write-comment))))))
  (lyskom-start-of-command (concat 
			    (lyskom-get-string 'kom-write-comment
					       'command)
			    (if text-no 
				(format " (%d)" text-no)
			      "")))
  (unwind-protect
      (if text-no
          (lyskom-write-comment-soon
           (blocking-do 'get-text-stat text-no)
           (blocking-do 'get-text text-no)
           text-no
           'comment)
        (lyskom-insert-string 'confusion-what-to-comment))
    (lyskom-end-of-command)))


(def-kom-command kom-write-footnote (&optional text-no)
  "Write a footnote to a text.
If optional arg TEXT-NO is present write a footnote to that text instead."
  (interactive (list 
		(cond
		 ((null current-prefix-arg)
		  lyskom-current-text)
		 ((integerp current-prefix-arg)
		  current-prefix-arg)
		 ((listp current-prefix-arg) 
		  (lyskom-read-number (lyskom-get-string 'what-comment-no)))
		 (t
		  (signal 'lyskom-internal-error '(kom-write-comment))))))
  (if text-no
      (lyskom-write-comment-soon
       (blocking-do 'get-text-stat text-no)
       (blocking-do 'get-text text-no)
       text-no 'footnote)
    (lyskom-insert-string 'confusion-what-to-footnote)))


(def-kom-command kom-comment-previous ()
  "Write a comment to previously viewed text."
  (interactive)
  (if lyskom-previous-text
      (lyskom-write-comment-soon 
       (blocking-do 'get-text-stat lyskom-previous-text)
       (blocking-do 'get-text lyskom-previous-text)
       lyskom-previous-text 'comment)
    (lyskom-insert-string 'confusion-what-to-comment)))


(defun lyskom-write-comment-soon (text-stat text text-no type)
  "Write a comment to the text with TEXT-STAT, TEXT and, TEXT-NO.
TYPE is either 'comment or 'footnote."
  (cond
   ;; Text not found?
   ((or (null text-stat)
	(null text))
    (lyskom-format-insert 'cant-read-textno text-no))
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
        (lyskom-insert-string 'confusion-what-to-comment))
    (let ((ccrep))
      (lyskom-tell-internat (if (eq type 'comment)
                                'kom-tell-write-comment
                              'kom-tell-write-footnote))
      (let (data)
        (mapcar 
         (function
          (lambda (misc-info)
            (cond
             ((eq 'RECPT (misc-info->type misc-info))
              (setq data 
                    (cons (blocking-do 'get-conf-stat 
                                       (misc-info->recipient-no misc-info))
                          data)))
             ((and (eq type 'footnote)
                   (eq 'CC-RECPT (misc-info->type misc-info)))
              (setq ccrep (cons (misc-info->recipient-no misc-info) 
                                ccrep))
              (setq data (cons (blocking-do 'get-conf-stat
                                            (misc-info->recipient-no misc-info))
                               data))))))
         (text-stat->misc-info-list text-stat))
        (lyskom-comment-recipients data lyskom-proc text-stat
                                   subject type ccrep)))))


(defun lyskom-comment-recipients (data lyskom-proc text-stat
				       subject type ccrep)
  "Compute recipients to a comment to a text.
Args: DATA, LYSKOM-PROC TEXT-STAT SUBJECT TYPE CCREP.
DATA is a list of all the recipients that should receive this text.
If DATA contains more than one conference the user is asked (using y-or-n-p)
if all conferences really should receive the text.
The call is continued to the lyskom-edit-text.
TYPE is info whether this is going to be a comment of footnote.
CCREP is a list of all recipients that are going to be cc-recipients."

  (condition-case nil
      ;; Catch any quits
      (progn
	;; Filter multiple recipients through y-or-n-p.
	(if (and (eq kom-confirm-multiple-recipients 'before)
                 (> (length data) 1)
		 (not (and (= (length data) 2)
			   (or (= lyskom-pers-no (conf-stat->conf-no
						  (car data)))
			       (= lyskom-pers-no (conf-stat->conf-no
						  (car (cdr data))))))))
	    (let ((new-data nil))
	      (while data
		(if (lyskom-j-or-n-p (lyskom-format 'comment-keep-recpt-p
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
			       (cons (if (memq (conf-stat->conf-no conf-stat)
					       ccrep)
					 'cc-recpt
				       'recpt)
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

    (quit (signal 'quit "quit in lyskom-comment-recipients"))))


;;; ================================================================
;;;                Personligt svar - personal answer

;;; Author: ???
;;; Rewritten using blocking-do by: Linus Tolke


(def-kom-command kom-private-answer (&optional text-no)
  "Write a private answer to the current text.
If optional arg TEXT-NO is present write a private answer to
that text instead."
  (interactive (list
		(cond
		 ((null current-prefix-arg)
		  lyskom-current-text)
		 ((integerp current-prefix-arg)
		  current-prefix-arg)
		 ((listp current-prefix-arg) 
		  (lyskom-read-number (lyskom-get-string 'what-private-no)))
		 (t
		  (signal 'lyskom-internal-error '(kom-private-answer))))))
  (if text-no
      (lyskom-private-answer-soon
       (blocking-do 'get-text-stat text-no)
       (blocking-do 'get-text text-no)
       text-no)
    (lyskom-insert-string 'confusion-who-to-reply-to)))


(defun lyskom-private-answer-soon (text-stat text text-no)
  "Write a private answer to TEXT-STAT, TEXT."
  (if (and text-stat text)
      (if (string-match "\n" (text->text-mass text))
	  (lyskom-private-answer text-stat
				 (substring (text->text-mass text)
					    0 (match-beginning 0)))
	(lyskom-private-answer text-stat ""))
    (lyskom-format-insert 'no-such-text-no text-no)))


(defun lyskom-private-answer (text-stat subject)
  "Write a private answer. Args: TEXT-STAT SUBJECT."
  (if (null text-stat)
      (progn
	(lyskom-insert-string 'confusion-what-to-answer-to))
    (progn
      (lyskom-tell-internat 'kom-tell-write-reply)
      (lyskom-edit-text lyskom-proc
			(lyskom-create-misc-list
			 'comm-to (text-stat->text-no text-stat)
			 'recpt (text-stat->author text-stat)
			 'recpt lyskom-pers-no)
			subject ""))))


;;; ================================================================
;;;    Personligt svar p} f|reg}ende - kom-private-answer-previous

;;; Author: ceder
;;; Rewritten using blocking-do by: Linus Tolke

(def-kom-command kom-private-answer-previous ()
  "Write a private answer to previously viewed text."
  (interactive)
  (if lyskom-previous-text
      (lyskom-private-answer-soon-prev
       (blocking-do 'get-text-stat lyskom-previous-text)
       (blocking-do 'get-text lyskom-previous-text))
    (lyskom-insert-string 'confusion-who-to-reply-to)))

(defun lyskom-private-answer-soon-prev (text-stat text)
  "Write a private answer to TEXT-STAT, TEXT."
  (if (string-match "\n" (text->text-mass text))
      (lyskom-private-answer text-stat
			     (substring (text->text-mass text)
					0 (match-beginning 0)))
    (lyskom-private-answer text-stat "")))


;;; ================================================================
;;;                         Sluta - quit

;;; Author: ???


(defun kom-quit (&optional arg)
  "Quit session. Kill process and buffer-local variables.
If optional argument is non-nil then dont ask for confirmation."
  (interactive "P")
  (lyskom-start-of-command 'kom-quit t)
  (let ((do-end-of-command t))
    (unwind-protect
        (setq do-end-of-command
              (cond
               ((and (lyskom-count-down-edits)
                     (display-buffer (car lyskom-list-of-edit-buffers))
                     (not (lyskom-ja-or-nej-p
                           (lyskom-get-string 'quit-in-spite-of-unsent))))
                t)
               ((or arg (lyskom-ja-or-nej-p (lyskom-get-string 'really-quit)))
                (lyskom-quit) nil)
               (t t)))
      (if do-end-of-command (lyskom-end-of-command)))))


(defun lyskom-quit ()
  "Quit a session. Kill process and buffer-local variables.
Don't ask for confirmation."
    (initiate-logout 'main nil)
    (setq lyskom-sessions-with-unread
	  (delq lyskom-buffer lyskom-sessions-with-unread))
    (setq lyskom-sessions-with-unread-letters
	  (delq lyskom-buffer lyskom-sessions-with-unread-letters))
    (set-process-sentinel lyskom-proc nil)
    (delete-process lyskom-proc)
    (setq lyskom-proc)
    (lyskom-insert-string 'session-ended)
    (lyskom-scroll)
    (setq mode-line-process (lyskom-get-string 'mode-line-down))
    (run-hooks 'kom-quit-hook))
  

;;; ================================================================
;;;     	[ndra presentation - Change presentation 
;;;	         S{tta lapp p} d|rren - Change conf-motd

;;; Author: Inge Wallin
;;; Changed by Linus Tolke


(def-kom-command kom-change-presentation ()
  "Change presentation for a person or a conference."
  (interactive)
  (lyskom-change-pres-or-motd-2
   (let ((no (lyskom-read-conf-no 
	      (lyskom-get-string 'what-to-change-pres-you)
	      '(all) t nil t)))
     (if (zerop no)
	 (setq no lyskom-pers-no))
     (blocking-do 'get-conf-stat no))
   'pres))


(def-kom-command kom-change-conf-motd ()
  "Change motd for a person or a conference."
  (interactive)
  (lyskom-change-pres-or-motd-2
   (let ((no (lyskom-read-conf-no (lyskom-get-string 'who-to-put-motd-for)
				  '(all) t nil t)))
     (if (zerop no)
	 (setq no lyskom-pers-no))
     (blocking-do 'get-conf-stat no))
   'motd))


(defun lyskom-change-pres-or-motd-2 (conf-stat type)
  "Change the presentation or motd of CONF-STAT.
TYPE is either 'pres or 'motd, depending on what should be changed."
  (cond
   ((null conf-stat)			;+++ annan felhantering
    (lyskom-insert-string 'cant-get-conf-stat))
   ((or lyskom-is-administrator
	(lyskom-member-p (conf-stat->supervisor conf-stat))
	(= lyskom-pers-no (conf-stat->conf-no conf-stat)))
    (lyskom-dispatch-edit-text
     lyskom-proc
     (lyskom-create-misc-list 'recpt
			      (cond
			       ((eq type 'motd)
				(server-info->motd-conf lyskom-server-info))
			       ((eq type 'pres)
				(if (conf-type->letterbox
				     (conf-stat->conf-type conf-stat))
				    (server-info->pers-pres-conf 
				     lyskom-server-info)
				  (server-info->conf-pres-conf
				   lyskom-server-info)))))
     (conf-stat->name conf-stat)
     (let ((text-mass (blocking-do 'get-text 
				   (cond
				    ((eq type 'pres)
				     (conf-stat->presentation conf-stat))
				    ((eq type 'motd)
				     (conf-stat->msg-of-day conf-stat))))))
       (if (and text-mass
		(string-match "\n" (text->text-mass text-mass)))
	   (substring (text->text-mass text-mass) (match-end 0))
	 (if (and (eq type 'pres)
		  (conf-type->letterbox (conf-stat->conf-type conf-stat)))
	     (lyskom-get-string 'presentation-form)
	   "")))
     (cond
      ((eq type 'pres) 'lyskom-set-presentation)
      ((eq type 'motd) 'lyskom-set-conf-motd))
     (conf-stat->conf-no conf-stat)))
   (t
    (lyskom-format-insert 'not-supervisor-for
			  conf-stat))))


   

;;; ================================================================
;;;           Ta bort lapp p} d|rren - delete conf-motd

;;; Author: Linus Tolke (& Inge Wallin)


(def-kom-command kom-unset-conf-motd ()
  "Removes motd for a person or a conference."
  (interactive)
  (let ((conf-stat (or (lyskom-read-conf-stat
			(lyskom-get-string 'who-to-remove-motd-for)
			'(all) t nil t)
		       (blocking-do 'get-conf-stat lyskom-pers-no))))
    (cond
     ((null conf-stat)
      (lyskom-insert-string 'cant-get-conf-stat))
     ((or lyskom-is-administrator
	  (lyskom-member-p (conf-stat->supervisor conf-stat)))
      ;; This works like a dispatch. No error handling.
      (lyskom-set-conf-motd 0 (conf-stat->conf-no conf-stat)))
     (t
      (lyskom-format-insert 'not-supervisor-for
			    conf-stat)))))
  

;;; ================================================================
;;;               G} till M|te - Go to a conference.

;;; Author: ???

(def-kom-command kom-go-to-conf (&optional conf-no)
  "Select a certain conference.
The user is prompted for the name of the conference.
If s/he was already reading a conference that conference will be put
back on lyskom-to-do-list."
  (interactive)
  (let ((conf (if conf-no
                  (blocking-do 'get-conf-stat conf-no)
                (lyskom-read-conf-stat
                 (lyskom-get-string 'go-to-conf-p)
                 '(all) nil "" t))))
    (lyskom-go-to-conf conf)))


(defun lyskom-go-to-conf (conf)
  "Go to the conference in CONF. CONF can be conf-no of conf-stat.
Allowed conferences are conferences and the mailboxes you are 
member of."
  (if (numberp conf) (setq conf (blocking-do 'get-conf-stat conf)))
  (let ((membership (lyskom-member-p
		     (conf-stat->conf-no conf))))
    (lyskom-format-insert 'go-to-conf
			  conf)
    (cond
     (membership
      (lyskom-do-go-to-conf conf membership))
     ((conf-type->letterbox (conf-stat->conf-type conf))
      (lyskom-format-insert 'cant-go-to-his-mailbox
			    conf))
     (t
      (progn
	(lyskom-format-insert 'not-member-of-conf
			      conf)
	(lyskom-scroll)
	(if (lyskom-j-or-n-p (lyskom-get-string 'want-become-member))
	    (if (lyskom-add-member-by-no (conf-stat->conf-no conf)
					 lyskom-pers-no)
		(lyskom-do-go-to-conf conf
				      (lyskom-member-p (conf-stat->conf-no conf)))
	      (lyskom-insert-string 'nope))
	  (lyskom-insert-string 'no-ok)))))))
  

;; Dead function /davidk 960217
;;(defun lyskom-fixup-and-go-to-conf (conf-no)
;;  "Prefetches and after lyskom-member-in-conf and then goes to CONF-NO."
;;  (lyskom-do-go-to-conf (blocking-do 'get-conf-stat conf-no)
;;			(lyskom-member-p conf-no)))

 
(defun lyskom-do-go-to-conf (conf-stat membership)
  "Go to a conference. Args: CONF-STAT MEMBERSHIP.
 Put a read-info of type CONF first on lyskom-reading-list.
Args: CONF-STAT MEMBERSHIP"
  (let ((priority (lyskom-get-current-priority)))
    (lyskom-maybe-move-unread nil)
    (if conf-stat
	(lyskom-set-mode-line conf-stat))
    (cond
     ((let ((r 0)
	    (len (read-list-length lyskom-to-do-list)) 
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
      (lyskom-enter-conf conf-stat (read-list->first lyskom-reading-list)))

     (t
      (lyskom-go-to-empty-conf conf-stat)))))



(defun lyskom-go-to-empty-conf (conf-stat)
  "Go to a conference with no unseen messages. Args: CONF-STAT."
  (blocking-do 'pepsi (conf-stat->conf-no conf-stat))
  (setq lyskom-current-conf (conf-stat->conf-no conf-stat))
  (lyskom-format-insert 'conf-all-read 
			conf-stat))



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


(def-kom-command kom-write-text ()
  "write a text."
  (interactive)
;  (lyskom-start-of-command 'kom-write-text)
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
;;; Rewritten: linus

(def-kom-command kom-list-persons (match)
  "List all persons whose name matches MATCH (a string)."
  (interactive (list (lyskom-read-string 
		      (lyskom-get-string 'search-for-pers))))
  (mapcar
   (function (lambda (no)
	       (lyskom-list-pers-print no)))
   (lyskom-extract-persons (blocking-do 'lookup-name match))))


(defun lyskom-list-pers-print (conf-no)
  "Print name of the person CONF-NO for kom-list-persons."
  (lyskom-format-insert "%[%#1@%4#2:p %#3P%]\n"
			(lyskom-default-button 'pers conf-no)
			conf-no
			conf-no))



;;; ================================================================
;;;              Lista M|ten - List conferences

;;; Author: ceder
;;; Rewritten: linus

(def-kom-command kom-list-conferences (match)
  "List all conferences whose name matches MATCH (a string).
Those that you are not a member in will be marked with an asterisk."
  (interactive (list (lyskom-read-string
		      (lyskom-get-string 'search-for-conf))))
  (mapcar
   (function (lambda (no)
	       (lyskom-list-conf-print no)))
   (lyskom-extract-confs (blocking-do 'lookup-name match))))


(defun lyskom-list-conf-print (conf-no)
  "Print a line of info about CONF-NO.
If you are not member in the conference it will be flagged with an asterisk."
  (lyskom-format-insert "%[%#1@%4#2:m %#3c %#4M%]\n"
			(lyskom-default-button 'conf conf-no)
			conf-no
			(if (lyskom-member-p conf-no)
			    32 ?*)
			conf-no))

;;; ================================================================
;;;                Lista med regexpar - List regexp

(def-kom-command kom-list-re (regexp)
  "List all persons and conferences whose name matches REGEXP."
  (interactive (list (lyskom-read-string
		      (lyskom-get-string 'search-re))))
  (lyskom-format-insert 'matching-regexp regexp)
  (let ((conf-list (blocking-do 're-z-lookup regexp 1 1)))
    (mapcar
     (function (lambda (czi)
		 (lyskom-format-insert
		  "%[%#1@%4#2:m %#3c %#4:M%]\n"
		  (lyskom-default-button
		   'conf (conf-z-info->conf-no czi))
		  (conf-z-info->conf-no czi)
		  (if (conf-type->letterbox 
		       (conf-z-info->conf-type czi))
		      ?P ?M)
		  (conf-z-info->name czi))))
     (conf-z-info-list->conf-z-infos conf-list))))


;;; ================================================================
;;;                 [ndra namn - Change name

;;; Author: Inge Wallin
;;; Changed by: Peter Eriksson(?)
;;; Changed again: Inge Wallin
;;; Rewritten: linus


(def-kom-command kom-change-name ()
  "Change the name of a person or conference."
  (interactive)
  (let ((conf-stat (lyskom-read-conf-stat 
		    (lyskom-get-string 'name-to-be-changed)
		    '(all) nil nil t)))
    (if (null conf-stat)
	(lyskom-insert-string 'no-such-conf-or-pers)
      (let (name)
	(lyskom-format-insert 'about-to-change-name-from
			      conf-stat)
	(lyskom-scroll)
	(lyskom-tell-internat 'kom-tell-change-name)
	(setq name (lyskom-read-string (lyskom-get-string 'new-name)
				       (conf-stat->name conf-stat)))
	(if (blocking-do 'change-name (conf-stat->conf-no conf-stat) name)
	    (lyskom-format-insert 'change-name-done name
				  (lyskom-default-button 'conf conf-stat))
	  (lyskom-format-insert 'change-name-nope name 
				(lyskom-get-error-text lyskom-errno)
				lyskom-errno))))))
	    


;;; ================================================================
;;;                [ndra organisat|r - Change supervisor

;;; Author: Inge Wallin
;;; Rewritten: linus

(def-kom-command kom-change-supervisor ()
  "Change the supervisor of a person or conference."
  (interactive)
  (let ((supervisee (lyskom-read-conf-stat
		     (lyskom-get-string 'who-to-change-supervisor-for)
		     '(all) nil nil t)))
    (if (null supervisee)
	(lyskom-insert-string 'no-such-conf-or-pers)
      (lyskom-tell-internat 'kom-tell-change-supervisor)
      (let ((supervisor (lyskom-read-conf-stat
			 (lyskom-get-string 'new-supervisor)
			 '(all) nil nil t)))
	(lyskom-format-insert 'change-supervisor-from-to
			      supervisee
			      supervisor)
	(if (blocking-do 'set-supervisor 
			 (conf-stat->conf-no supervisee) 
			 (conf-stat->conf-no supervisor))
	    (progn
	      (lyskom-insert-string 'done)
	      (cache-del-conf-stat (conf-stat->conf-no supervisee)))
	  (lyskom-format-insert
	   'change-supervisor-nope
	   supervisee))))))


;;; ================================================================
;;;         Markera och Avmarkera - Mark and Unmark a text

;;; Author: Inge Wallin
;;; [ndrad av: Linus Tolke

(def-kom-command kom-mark-text (text-no-arg)
  "Mark a text. If the argument TEXT-NO-ARG is non-nil, the user has used
a prefix command argument."
  (interactive "P")
  (lyskom-mark-text text-no-arg (lyskom-get-string 'text-to-mark) 1))


(def-kom-command kom-unmark-text (text-no-arg)
  "Unmark a text. If the argument TEXT-NO-ARG is non-nil, the user has used
a prefix command argument."
  (interactive "P")
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
    (if prompt
        (setq text-no (lyskom-read-number prompt text-no)))
    (if (not (eq mark 0))
        (setq mark
              (or kom-default-mark
                  (lyskom-read-num-range
                   1 255 (lyskom-get-string 'what-mark) t))))
    (if (equal mark 0)
        (lyskom-format-insert 'unmarking-textno 
                              text-no)
      (lyskom-format-insert 'marking-textno 
                            text-no))
    
    (if (blocking-do 'mark-text text-no mark)
        (progn
          (lyskom-insert-string 'done)	  
          (if (= mark 0)
              (cache-del-marked-text text-no)
            (cache-add-marked-text text-no mark)))
      (lyskom-insert-string 'nope))     ;+++ lyskom-errno?
    (cache-del-text-stat text-no)))


;;; ================================================================
;;;          ]terse alla markerade - Review marked texts

;;; Author: Inge Wallin


(def-kom-command kom-review-marked-texts ()
  "Review marked texts with a certain mark."
  (interactive)
  (lyskom-review-marked-texts 
   (lyskom-read-num-range 
    1 255 (lyskom-get-string 'what-mark-to-view) t)))


(def-kom-command kom-review-all-marked-texts ()
  "Review all marked texts"
  (interactive)
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
	(read-list-enter-read-info read-info lyskom-to-do-list t)))))


;;; ================================================================
;;;                 [ndra L|senord - Change password

;;; Author: Inge Wallin


(def-kom-command kom-change-password ()
  "Change the password for a person."
  (interactive)
  (let ((pers-no (lyskom-read-conf-no (lyskom-get-string 'whos-passwd)
				      '(pers) t "" t))
	(old-pw (silent-read (lyskom-get-string 'old-passwd)))
	(new-pw1 (silent-read (lyskom-get-string 'new-passwd)))
	(new-pw2 (silent-read (lyskom-get-string 'new-passwd-again))))

    (if (string= new-pw1 new-pw2)
	(progn
	  (lyskom-insert-string 'changing-passwd)
	  (lyskom-report-command-answer 
	   (blocking-do 'set-passwd (if (zerop pers-no)
					lyskom-pers-no
				      pers-no)
			old-pw new-pw1)))
      (lyskom-insert-string 'retype-dont-match))))



;;; ================================================================
;;;               (Se) Tiden - display time and date.

(defconst lyskom-times 
  '(((nil 12 24 nil nil nil) . xmaseve)
    ((nil 12 25 nil nil nil) . xmasday)
    ((nil  1  1 nil nil nil) . newyearday)
    ((nil 12 31  11 nil nil) . newyearevelate)
    ((nil 12 31 nil nil nil) . newyeareve)
    ((nil  4 30 nil nil nil) . cgdag)
    ((nil  6  6 nil nil nil) . sixjune)
    ((nil  8 15 nil nil nil) . holdnose)
    ((nil  3 29 nil nil nil) . lysbday)
))


(defun lyskom-format-time (time)
  "Return TIME as a formatted string."
  (lyskom-format 'time-format-exact
		 (+ (time->year time) 1900)
		 (1+ (time->mon  time))
		 (time->mday time)
		 (time->hour time)
		 (time->min  time)
		 (time->sec  time)
		 (elt (lyskom-get-string 'weekdays)
		      (time->wday time))))


(def-kom-command kom-display-time ()
  "Ask server about time and date."
  (interactive)
  (let ((time (blocking-do 'get-time)))
    (lyskom-format-insert 'time-is
			  (lyskom-format-time time)
			  ;; Kult:
			  (if (and (= (time->hour time)
				      (+ (/ (time->sec time) 10)
					 (* (% (time->sec time) 10) 10)))
				   (= (/ (time->min time) 10)
				      (% (time->min time) 10)))
			      (lyskom-get-string 'palindrome)
			    ""))
    ;; Mera kult
    (mapcar (function 
             (lambda (el)
               (let ((when (car el))
                     (event (cdr el)))
                 (if (and (or (null (elt when 0))
                              (= (+ (time->year time) 1900) (elt when 0)))
                          (or (null (elt when 1))
                              (= (1+ (time->mon time)) (elt when 1)))
                          (or (null (elt when 2))
                              (= (time->mday time) (elt when 2)))
                          (or (null (elt when 3))
                              (= (time->hour time) (elt when 3)))
                          (or (null (elt when 4))
                              (= (time->min time) (elt when 4)))
                          (or (null (elt when 5))
                              (= (time->sec time) (elt when 5))))
                     (condition-case nil
			 (progn
			   (lyskom-insert " ")
			   (lyskom-format-insert event
						 (+ (time->year time) 1900)
						 (1+ (time->mon  time))
						 (time->mday time)
						 (time->hour time)
						 (time->min  time)
						 (time->sec  time)))
		       (error nil))))))
            lyskom-times))
  (lyskom-insert "\n"))



;;; ================================================================
;;;                Vilka ({r inloggade) - Who is on?

;;; Author: ???
;;; Rewritten by: David K}gedal


(put 'lyskom-no-users 'error-conditions
     '(error lyskom-error lyskom-no-users))

(def-kom-command kom-who-is-on (&optional arg)
  "Display a list of all connected users.
The prefix arg controls the idle limit of the sessions showed. If the
prefix is negativ, invisible sessions are also shown.

If the prefix is 0, all visible sessions are shown."
  (interactive "P")
  (condition-case nil
      (if lyskom-dynamic-session-info-flag
	  (lyskom-who-is-on-9 arg)
	(lyskom-who-is-on-8))
    (lyskom-no-users
     (lyskom-insert (lyskom-get-string 'null-who-info)))))


(defun lyskom-who-is-on-8 ()
  "Display a list of all connected users.
Uses Protocol A version 8 calls"
  (let* ((who-info-list (blocking-do 'who-is-on))
	 (who-list (sort (listify-vector who-info-list)
			 (function (lambda (who1 who2)
				     (< (who-info->connection who1)
					(who-info->connection who2))))))
	 (total-users (length who-list))
	 (session-width (1+ (length (int-to-string
				     (who-info->connection
				      (nth (1- total-users) who-list))))))
	 (format-string-1 (lyskom-info-line-format-string
			   session-width "P" "M"))
	 (format-string-2 (lyskom-info-line-format-string
			   session-width "s" "s"))
	 (lyskom-default-conf-string 'not-present-anywhere))
    (lyskom-format-insert format-string-2
			  ""
			  (lyskom-get-string 'lyskom-name)
			  (lyskom-get-string 'is-in-conf))
    (if kom-show-where-and-what
	(lyskom-format-insert format-string-2
			      ""
			      (lyskom-get-string 'from-machine)
			      (lyskom-get-string 'is-doing)))
    
    (lyskom-insert
     (concat (make-string (- (lyskom-window-width) 2) ?-) "\n"))
    
    (while who-list
      (let* ((who-info (car who-list))
	     (session-no (int-to-string (who-info->connection who-info)))
	     (my-session (if (= lyskom-session-no
				(who-info->connection who-info))
			     "*"
			   " ")))
	(lyskom-format-insert
	 format-string-1
	 (concat session-no my-session)
	 (who-info->pers-no who-info)
	 (or (who-info->working-conf who-info)
	     (lyskom-get-string 'not-present-anywhere)))
	(if kom-show-where-and-what
	    (lyskom-format-insert
	     format-string-2
	     ""
	     (lyskom-return-username who-info)
	     (concat "(" (who-info->doing-what who-info) ")"))))
      (setq who-list (cdr who-list)))
      
      (lyskom-insert (concat (make-string (- (lyskom-window-width) 2) ?-)
			     "\n"))
      (lyskom-insert (lyskom-format 'total-visible-users total-users))))


(defun lyskom-who-is-on-9 (arg)
  "Display a list of all connected users.
Uses Protocol A version 9 calls"
  (let* ((wants-invisibles (or (and (numberp arg) (< arg 0))
                               (and (symbolp arg) (eq '- arg))))
	 (idle-hide (if (numberp arg) (abs arg) 
                      (cond ((numberp kom-idle-hide) kom-idle-hide)
                            ((eq '- arg) 0)
                            (kom-idle-hide 30)
                            (t 0))))
	 (who-info-list (blocking-do 'who-is-on-dynamic
				     't wants-invisibles (* idle-hide 60)))
	 (who-list (sort (listify-vector who-info-list)
			 (function
			  (lambda (who1 who2)
			    (< (dynamic-session-info->session who1)
			       (dynamic-session-info->session who2))))))
	 (total-users (length who-list))
	 (session-width (if (null who-list)
			    (signal 'lyskom-no-users nil)
			  (1+ (length (int-to-string
				       (dynamic-session-info->session
					(nth (1- total-users) who-list)))))))
	 (format-string-1 (lyskom-info-line-format-string
			   session-width "P" "M"))
	 (format-string-2 (lyskom-info-line-format-string
			   session-width "D" "s"))
	 (lyskom-default-conf-string 'not-present-anywhere))

    (if (zerop idle-hide)
	(lyskom-insert (lyskom-get-string 'who-is-active-all))
      (lyskom-format-insert 'who-is-active-last-minutes idle-hide))

    (if wants-invisibles
	(lyskom-insert (lyskom-get-string 'showing-invisibles)))
			  
    (lyskom-format-insert format-string-2
			  ""
			  (lyskom-get-string 'lyskom-name)
			  (lyskom-get-string 'is-in-conf))
    (if kom-show-where-and-what
	(lyskom-format-insert format-string-2
			      ""
			      (lyskom-get-string 'from-machine)
			      (lyskom-get-string 'is-doing)))
    
    (lyskom-insert
     (concat (make-string (- (lyskom-window-width) 2) ?-) "\n"))
    
    (while who-list
      (let* ((who-info (car who-list))
	     (session-no (dynamic-session-info->session who-info))
	     (session-no-s (int-to-string session-no))
	     (my-session (if (= lyskom-session-no session-no)
			     "*"
			   " ")))
	(lyskom-format-insert
	 format-string-1
	 (concat session-no-s my-session)
	 (dynamic-session-info->person who-info)
	 (or (dynamic-session-info->working-conference who-info)
	     (lyskom-get-string 'not-present-anywhere)))
	(if kom-show-where-and-what
	    (let* (static defer-info username)
	      (cond (kom-deferred-printing
		     (setq static (cache-get-static-session-info session-no))
		     (if static
			 (setq username
			       (lyskom-combine-username
				(static-session-info->username static)
				(static-session-info->ident-user static)
				(static-session-info->hostname static)))
		       (setq defer-info
			     (lyskom-create-defer-info
			      'get-static-session-info
			      session-no
			      'lyskom-insert-deferred-session-info
			      (make-marker)
			      (length lyskom-defer-indicator)
			      "%#1s"))
		       (setq username defer-info)))
		    (t
		     (setq static
			   (blocking-do 'get-static-session-info session-no))
		     (setq username (lyskom-combine-username
				     (static-session-info->username static)
				     (static-session-info->ident-user static)
				     (static-session-info->hostname static)))))
	      (lyskom-format-insert
	       format-string-2
	       ""
	       username
	       (concat "(" (dynamic-session-info->what-am-i-doing who-info)
		       ")"))))
	(setq who-list (cdr who-list))))
    
    (lyskom-insert (concat (make-string (- (lyskom-window-width) 2) ?-)
			   "\n"))
    (lyskom-insert (lyskom-format
		    (cond ((and wants-invisibles (zerop idle-hide))
			   'total-users)
			  (wants-invisibles
			   'total-active-users)
			  ((zerop idle-hide)
			   'total-visible-users)
			  (t
			   'total-visible-active-users))
		    total-users))))

(defun lyskom-insert-deferred-session-info (session-info defer-info)
  (if session-info
      (lyskom-replace-deferred defer-info
			       (lyskom-combine-username
				(static-session-info->username session-info)
				(static-session-info->ident-user session-info)
				(static-session-info->hostname session-info)))
    (lyskom-replace-deferred defer-info "")))

;;; =====================================================================
;;;                 Lista klienter - List clients
;;; Author: David Kågedal

		 
(def-kom-command kom-list-clients ()
  "Display a list of all connected users."
  (interactive)
  (let* ((who-info-list (blocking-do 'who-is-on))
	 (who-list (sort (listify-vector who-info-list)
			 (function (lambda (who1 who2)
				     (< (who-info->connection who1)
					(who-info->connection who2))))))
	 (total-users (length who-list))
	 (s-width (1+ (length (int-to-string
			       (who-info->connection
				(nth (1- total-users) who-list))))))
	 (format-string (lyskom-info-line-format-string
			 s-width "P" (if kom-deferred-printing "D" "s"))))
    (lyskom-format-insert format-string
			  ""
			  (lyskom-get-string 'lyskom-name)
			  (lyskom-get-string 'lyskom-client))
    (lyskom-insert
     (concat (make-string (- (lyskom-window-width) 2) ?-) "\n"))

    (while who-list
      (let* ((who-info (car who-list))
	     (session-no (int-to-string (who-info->connection who-info)))
	     (my-session (if (= lyskom-session-no
				(who-info->connection who-info))
			     "*"
			   " "))
	     (client (if kom-deferred-printing
			 (lyskom-create-defer-info
			  'get-client-name
			  (who-info->connection who-info)
			  'lyskom-deferred-client-1
			  nil nil nil	; Filled in later
			  (who-info->connection who-info))
		       (blocking-do-multiple
			   ((name (get-client-name
				   (who-info->connection who-info)))
			    (version (get-client-version
				      (who-info->connection who-info))))
			 (concat name " " version)))))
	(lyskom-format-insert
	 format-string
	 (concat session-no my-session)
	 (who-info->pers-no who-info)
	 client))
      (setq who-list (cdr who-list)))

    (lyskom-insert (concat (make-string (- (lyskom-window-width) 2) ?-)
			   "\n"))
    (lyskom-insert (lyskom-format 'total-visible-users total-users))))


(defun lyskom-deferred-client-1 (name defer-info)
  (initiate-get-client-version 'deferred
			       'lyskom-deferred-client-2
			       (defer-info->data defer-info)
			       defer-info
			       name))

(defun lyskom-deferred-client-2 (version defer-info name)
  (lyskom-replace-deferred defer-info (if (zerop (length name))
					  "-"
					(concat name " " version))))



(defun lyskom-info-line-format-string (prefixlen type1 type2)
  "Return a format string suitable for inserting who-info lines etc."
  (let* ((plen (or prefixlen 7))
	 (adj1 (+ plen 2))
         (adj2 (+ adj1 1)))
    (concat "%" (int-to-string plen) "#1s"
	    "%=-"
	    (int-to-string (/ (* 37 (- (lyskom-window-width) adj1)) 73))
	    "#2" type1
	    " %=-"
	    (int-to-string (/ (* 37 (- (lyskom-window-width) adj2)) 73))
	    "#3" type2
	    "\n")))
    
(defun lyskom-window-width ()
  "Returns the width of the lyskom-window or the screen-width if not displayed."
  (let ((win (get-buffer-window (current-buffer))))
    (cond
     (win (window-width win))
     (t (frame-width)))))


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


(defun lyskom-combine-username (username identname hostname)
  "Return a description of from where a user is logged in."
  ;; Ignore ident info for now
  (if (string-match "\\(.*\\)%\\(.*\\)" username)
      (let ((user (substring username (match-beginning 1) (match-end 1)))
	    (uhost (substring username (match-beginning 2) (match-end 2))))
	(if (string= uhost hostname)
	    (concat user "@" hostname)
	  (concat username "@" hostname)))
    (concat username "@" hostname)))


;;; ================================================================
;;;            Status (för) Session - Status (for a) session
;;;
;;; Author: David Byers

(def-kom-command kom-status-session (&optional arg)
  "Show status for all sessions a person has. Asks for person name.
Optional argument ARG should be a list of sessions to get information
about or a single session number."
  (interactive "P")
  (let ((sessions (or (cond ((listp arg) arg)
                            ((numberp arg) (list arg)))
                      (lyskom-read-session-no 
                       (lyskom-get-string 'status-for-session))))
	who-info)
    (cond ((null sessions)
           (lyskom-insert-string 'no-such-session-r))
          ((and (numberp (car sessions))
                (<= (car sessions) 0))
           (lyskom-format-insert
            (lyskom-get-string 'person-not-logged-in-r)
            (- (car sessions))))
          (t
           (if lyskom-dynamic-session-info-flag
               (progn
                 (setq who-info (listify-vector
                                 (blocking-do 'who-is-on-dynamic t t 0)))
                 (mapcar (function
                          (lambda (x) (lyskom-status-session-9 x 
                                                               who-info)))
                         sessions))
             (setq who-info (listify-vector (blocking-do 'who-is-on)))
             (mapcar (function 
                      (lambda (x) (lyskom-status-session-8 x who-info)))
                     sessions))))))


(defun lyskom-status-session-8 (sid who-info-list)
  "Show session status for session SID. WHO-INFO is a list of
WHO-INFOS that are potential sessions."
  (while who-info-list
    (if (eq sid (who-info->connection (car who-info-list)))
	(let* ((info (car who-info-list))
	       (client (if kom-deferred-printing
			   (lyskom-create-defer-info
			    'get-client-name
			    (who-info->connection info)
			    'lyskom-deferred-client-1
			    nil nil nil
			    (who-info->connection info))
			 (blocking-do-multiple
			     ((name (get-client-name
				     (who-info->connection info)))
			      (version (get-client-version
					(who-info->connection info))))
			   (concat name " " version)))))
	  (lyskom-format-insert
	   (lyskom-get-string 'session-status)
	   (who-info->connection info)
	   (who-info->pers-no info)
	   (lyskom-return-username info)
	   (if (not (eq (who-info->working-conf info) 0))
	       (who-info->working-conf info)
	     (lyskom-get-string 'not-present-anywhere))
	   (let ((string (if (string-match "^\\(.*[^.]\\)\\.*$"
					   (who-info->doing-what info))
			     (match-string 1 (who-info->doing-what info))
			   (who-info->doing-what info))))
	     (if (string= string "")
		 (lyskom-get-string 'unknown-doing-what)
	       string))
	   client
	   (if (not (eq (who-info->working-conf info) 0))
	       (lyskom-get-string 'doing-where-conn)
	     (lyskom-get-string 'doing-nowhere-conn)))))
    (setq who-info-list (cdr who-info-list))))


(defun lyskom-status-session-9 (sid who-info-list)
  "Show session status for session SID. WHO-INFO is a list of
WHO-INFOS that are potential sessions."
  (let ((static (blocking-do 'get-static-session-info sid)))
    (while who-info-list
      (if (eq sid (dynamic-session-info->session (car who-info-list)))
	  (let* ((info (car who-info-list))
		 (client (if kom-deferred-printing
			     (lyskom-create-defer-info
			      'get-client-name
			      (dynamic-session-info->session info)
			      'lyskom-deferred-client-1
			      nil nil nil
			      (dynamic-session-info->session info))
			   (blocking-do-multiple
			       ((name (get-client-name
				       (dynamic-session-info->session info)))
				(version
				 (get-client-version
				  (dynamic-session-info->session info))))
			     (concat name " " version)))))
	    (lyskom-format-insert
	     (lyskom-get-string 'session-status-9)
	     (dynamic-session-info->session info)
	     (dynamic-session-info->person info)
	     (lyskom-combine-username (static-session-info->username static)
				      (static-session-info->ident-user static)
				      (static-session-info->hostname static))
	     (if (not (eq (dynamic-session-info->working-conference info) 0))
		 (dynamic-session-info->working-conference info)
	       (lyskom-get-string 'not-present-anywhere))
	     (let ((string (if (string-match
				"^\\(.*[^.]\\)\\.*$"
				(dynamic-session-info->what-am-i-doing info))
			       (match-string
				1 (dynamic-session-info->what-am-i-doing info))
			     (dynamic-session-info->what-am-i-doing info))))
	       (if (string= string "")
		   (lyskom-get-string 'unknown-doing-what)
		 string))
	     client
	     (if (not (eq (dynamic-session-info->working-conference info) 0))
		 (lyskom-get-string 'doing-where-conn)
	       (lyskom-get-string 'doing-nowhere-conn))
	     (lyskom-format-time
	      (static-session-info->connection-time static))
             (cond ((eq (/ (dynamic-session-info->idle-time info) 60) 0)
                    (lyskom-get-string 'session-is-active))
                   ((not (session-flags->user_active_used
                          (dynamic-session-info->flags info)))
                    "\n")
                   (t
                    (lyskom-format (lyskom-get-string 'session-status-inactive)
                                   (lyskom-format-secs
                                    (dynamic-session-info->idle-time info))))))
	    (if (session-flags->invisible (dynamic-session-info->flags info))
		(lyskom-insert (lyskom-get-string 'session-is-invisible)))))
      (setq who-info-list (cdr who-info-list)))))


(defun lyskom-format-secs-aux (string num x1 x2 one many)
  (cond ((<= num 0) string)
        ((= num 1) (if (string= "" string)
                       (concat string (lyskom-get-string one))
                     (concat string (if (and (= x1 0)
                                             (= x2 0))
                                        (format " %s " 
                                                (lyskom-get-string 'and))
                                      ", ")
                             (lyskom-get-string one))))
        (t (if (string= "" string)
               (concat string (format "%d %s" num
                                      (lyskom-get-string many)))
             (concat string (if (and (= x1 0)
                                     (= x2 0))
                                (format " %s "
                                        (lyskom-get-string 'and))
                              ", ")
                     (format "%d %s"
                             num
                             (lyskom-get-string many)))))))

(defun lyskom-format-secs (time)
  "Format the number of seconds in TIME as a human-readable string."
  (let (;; (secs (% time 60))
        (mins (% (/ time 60) 60))
        (hrs  (% (/ time 3600) 24))
        (days (/ time 86400))
        (string ""))
    (setq string (lyskom-format-secs-aux string days hrs mins 'one-day 'days))
    (setq string (lyskom-format-secs-aux string hrs  mins 0 'one-hour 'hours))
    (setq string (lyskom-format-secs-aux string mins 0 0 'one-minute 'minutes))))
	

;;; ================================================================
;;;                     Hoppa -  Jump over comments

;;; Author: Linus Tolke Y

;; Hoppa |ver alla inl{gg som {r kommentarer till detta inl{gg (recursivt)


(defun kom-jump (&optional text-no)
  "Jumps all comments to the current text. Descends recursively in comment tree.
The three is truncated if we encounter an older text.
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


(defun lyskom-jump (text-stat mark-as-read &optional sync)
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
    (lyskom-traverse 
	misc
	(text-stat->misc-info-list text-stat)
      (cond
       ((and (or (eq (misc-info->type misc) 'COMM-IN)
		 (eq (misc-info->type misc) 'FOOTN-IN))
	     (> (if (eq (misc-info->type misc)
			'COMM-IN)
		    (misc-info->comm-in misc)
		  (misc-info->footn-in misc))
		(text-stat->text-no text-stat)))
	(let ((comment (if (eq (misc-info->type misc)
			       'COMM-IN)
			   (misc-info->comm-in misc)
			 (misc-info->footn-in misc))))
	  (if sync
	      (lyskom-jump (blocking-do 'get-text-stat comment)
			   mark-as-read sync)
	    (initiate-get-text-stat 'main
				    'lyskom-jump
				    comment
				    mark-as-read)))))))))


;;; ================================================================
;;;                 Addera mottagare - Add recipient
;;;             Subtrahera mottagare - Subtract recipient

;;; Author: David Byers & David K}gedal
;;; Based on code by Inge Wallin


(def-kom-command kom-add-recipient (text-no-arg)
  "Add a recipient to a text. If the argument TEXT-NO-ARG is non-nil,
the user has used a prefix command argument."
  (interactive "P")
  (let ((conf (blocking-do 'get-conf-stat lyskom-last-added-rcpt)))
	(lyskom-add-sub-recipient text-no-arg
                              (lyskom-get-string 'text-to-add-recipient)
                              'add-rcpt
                              conf)))

(def-kom-command kom-add-copy (text-no-arg)
  "Add a cc recipient to a text. If the argument TEXT-NO-ARG is non-nil,
the user has used a prefix command argument."
  (interactive "P")
  (let ((conf (blocking-do 'get-conf-stat lyskom-last-added-ccrcpt)))
	(lyskom-add-sub-recipient text-no-arg
                              (lyskom-get-string 'text-to-add-copy)
                              'add-copy
                              conf)))

(def-kom-command kom-sub-recipient (text-no-arg)
  "Subtract a recipient from a text. If the argument TEXT-NO-ARG is non-nil, 
the user has used a prefix command argument."
  (interactive "P")
  (let ((conf (blocking-do 'get-conf-stat lyskom-current-conf)))
    (lyskom-add-sub-recipient text-no-arg
			      (lyskom-get-string 'text-to-delete-recipient)
			      'sub
			      conf)))

(def-kom-command kom-move-text (text-no-arg)
  "Subtract a recipient from a text and add another.
If the argument TEXT-NO-ARG is non-nil, the user has used a prefix
command argument."
  (interactive "P")
  (blocking-do-multiple ((default-from (get-conf-stat lyskom-current-conf))
			 (default-to (get-conf-stat lyskom-last-added-rcpt)))
    (lyskom-add-sub-recipient text-no-arg
			      (lyskom-get-string 'text-to-move)
			      'move
			      default-to
			      default-from)))

(defun lyskom-add-sub-recipient (text-no-arg
				 prompt
				 action
				 conf
				 &optional conf2)
  (let* ((text-no (lyskom-read-number prompt 
				      (or text-no-arg lyskom-current-text)))
	 (text-stat (blocking-do 'get-text-stat text-no))
	 (was-read (lyskom-text-read-p text-stat))

	 ;; Only for moving
	 (conf-to-move-from (if (eq action 'move)
				(lyskom-read-conf-stat
				 (lyskom-get-string 'who-to-move-from-q)
				 '(all)
				 nil
				 (if conf2 (conf-stat->name conf2) "")
                                 t)))

	 (conf-to-add-to (lyskom-read-conf-stat
			  (lyskom-get-string
			   (cond ((eq action 'add-rcpt) 'who-to-add-q)
				 ((eq action 'add-copy) 'who-to-add-copy-q)
				 ((eq action 'sub) 'who-to-sub-q)
				 ((eq action 'move) 'who-to-move-to-q)
				 (t (lyskom-error "internal error"))))
			  '(all)
			  nil
			  (if conf (conf-stat->name conf) "")
                          t))
	 (result nil))
    (setq result
	  (cond ((eq action 'add-rcpt)
		 (lyskom-format-insert 'adding-name-as-recipient 
				       conf-to-add-to
				       text-stat)
		 (setq lyskom-last-added-rcpt
		       (conf-stat->conf-no conf-to-add-to))
		 (blocking-do 'add-recipient
			      text-no 
			      (conf-stat->conf-no conf-to-add-to)
			      'recpt))

		((eq action 'add-copy)
		 (lyskom-format-insert 'adding-name-as-copy
				       conf-to-add-to
				       text-stat)
		 (setq lyskom-last-added-ccrcpt
		       (conf-stat->conf-no conf-to-add-to))
		 (blocking-do 'add-recipient
			      text-no
			      (conf-stat->conf-no conf-to-add-to)
			      'cc-recpt))

		((eq action 'sub)
		 (lyskom-format-insert 'remove-name-as-recipient
				       conf-to-add-to
				       text-stat)
		 (blocking-do 'sub-recipient
			      text-no
			      (conf-stat->conf-no conf-to-add-to)))

		((eq action 'move)
		 (lyskom-format-insert 'moving-name
				       conf-to-move-from
				       conf-to-add-to
				       text-stat)
		 (setq lyskom-last-added-rcpt
		       (conf-stat->conf-no conf-to-add-to))
		 (blocking-do-multiple
		     ((add (add-recipient
			    text-no 
			    (conf-stat->conf-no conf-to-add-to)
			    'recpt))
		      (sub (sub-recipient
			    text-no
			    (conf-stat->conf-no conf-to-move-from))))
		   (and add sub)))
			      
		
		(t (lyskom-error "internal error"))))
    (cache-del-text-stat text-no)
    (if was-read (lyskom-mark-as-read (blocking-do 'get-text-stat text-no)))
    (lyskom-report-command-answer result)))





;;; ================================================================
;;;                 Addera kommentar - Add comment
;;;             Subtrahera kommentar - Subtract comment

;;; Author: David Byers
;;; Heavily based on code by Lars Willf|r

(def-kom-command kom-add-comment (text-no-arg)
  "Add a text as a comment to another text."
  (interactive "P")
  (lyskom-add-sub-comment text-no-arg
			  (lyskom-get-string 'text-to-add-comment-to)
			  t))

(def-kom-command kom-sub-comment (text-no-arg)
  "Remove a comment from a text."
  (interactive "P")
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
  (let* ((text-no (lyskom-read-number prompt
				      (or text-no-arg lyskom-current-text)))
	 (comment-text-no  (lyskom-read-number
			    (lyskom-get-string
			     (if do-add 'text-to-add-q 'text-to-remove-q))
			    (if (eq text-no lyskom-current-text)
				nil
			      lyskom-current-text))))
    (lyskom-format-insert (if do-add 'add-comment-to 'sub-comment-to)
			  comment-text-no
			  text-no)
    (cache-del-text-stat text-no)
    (cache-del-text-stat comment-text-no)
    (lyskom-report-command-answer 
     (blocking-do (if do-add 'add-comment 'sub-comment)
		  comment-text-no
		  text-no))))
    



;;; ================================================================
;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; end: 
