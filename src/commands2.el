;;;;;
;;;;; $Id: commands2.el,v 36.9 1993-08-11 09:48:35 linus Exp $
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
;;;; File: commands2.el
;;;;
;;;; This file contains the code for some high level commands.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: commands2.el,v 36.9 1993-08-11 09:48:35 linus Exp $\n"))


;;; ================================================================
;;;              Lista medlemsskap - List membership

;;; Author: Linus Tolke
;;; Rewritten by ceder


(defun kom-membership ()
  "Show memberships last visited, priority, unread and name."
  (interactive)
  (lyskom-start-of-command 'kom-membership)
  (let ((buffer (get-buffer-create (concat (buffer-name (current-buffer))
					   "-membership"))))
    (save-window-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert (lyskom-get-string 'your-memberships))
      (insert (lyskom-get-string 'memberships-header)))
    (display-buffer buffer)
    (lyskom-traverse
     x lyskom-membership
     (initiate-get-conf-stat 'membership 'lyskom-memb-received-1
			     (membership->conf-no x)
			     x buffer)))
  (lyskom-end-of-command))

(defun lyskom-memb-received-1 (conf-stat membership buffer)
  "Part of kom-membership.
Get maps for the conference CONF-STAT. MEMBERSHIP is the users
membership in that conference. Call lyskom-memb-received with
the resulting MAP, CONF-STAT, MEMBERSHIP and BUFFER.
Args: CONF-STAT MEMBERSHIP BUFFER."
  (if (/= (conf-stat->conf-no conf-stat)
	  (membership->conf-no membership))
      (signal 'lyskom-internal-error '("lyskom-memb-received-1")))
  (let ((first-wanted (1+ (membership->last-text-read membership)))
	(last-existing  (+ (conf-stat->first-local-no conf-stat)
			   (conf-stat->no-of-texts conf-stat)
			   -1)))
    (if (> first-wanted last-existing)
	(lyskom-run 'membership 'lyskom-memb-received
		    nil conf-stat membership buffer)
      (initiate-get-map 'membership 'lyskom-memb-received
			(membership->conf-no membership)
			first-wanted
			(+ 1 last-existing
			   (- first-wanted))
			conf-stat membership buffer))))

(defun lyskom-memb-received (map conf-stat membership buffer)
  "Args: MAP CONF-STAT MEMBERSHIP BUFFER.
Prints membership in a conferences.
MAP may be nil if there are no new texts."
  (save-window-excursion
    (set-buffer buffer)
    (goto-char (point-max))
    (insert (concat (lyskom-return-time
		     (membership->last-time-read membership))
		    (format "   %d\t%d\t"
			    (membership->priority membership)
			    (if map
				(length (lyskom-list-unread map membership))
			      0))
		    (conf-stat->name conf-stat)
		    "\n"))))



;;; ================================================================
;;;          Status (f|r) M|te - Status for a conference

;;; Author: ceder (with some help by Linus)
;;; much enhanced by Inge Wallin (lyskom-status-conf-2 and beyond)


(defun kom-status-conf (&optional conf-no)
  "Prints conference status.
If argument CONF-NO is existing and non-nil then this conference is used. 
otherwise: the conference is read with lyskom-completing-read."
  (interactive)
  (lyskom-start-of-command 'kom-status-conf)
  (if conf-no
      (lyskom-status-conf conf-no)
    (lyskom-completing-read 'main 'lyskom-status-conf-1
			    (lyskom-get-string 'conf-for-status)
			    nil nil "")))


(defun lyskom-status-conf-1 (conf-no)
  "Invalidate the cache entry for CONF-NO and print status.
Calls initiate-get-conf-stat and prints using lyskom-status-conf."
  (cache-del-conf-stat conf-no)
  (initiate-get-conf-stat 'main 'lyskom-status-conf
			  conf-no))


(defun lyskom-status-conf (conf-stat)
  "Print status about CONF-STAT."
  (if (null conf-stat)
      (progn
	(lyskom-insert-string 'no-such-conf)
	(lyskom-end-of-command))
    (let* ((type (conf-stat->conf-type conf-stat))
	   (box (conf-type->letterbox type))
	   (ori (conf-type->original type))
	   (pro (conf-type->rd_prot type))
	   (sec (conf-type->secret type)))
      (lyskom-format-insert 'status-record
			    (conf-stat->name conf-stat)
			    (format "%d" (conf-stat->conf-no conf-stat))
			    (cond
			     ((or box ori pro sec)
			      (concat "("
				      (if box (lyskom-get-string 'Mailbox) "")
				      (if (and box (or sec ori pro)) ", " "")
				      (if sec (lyskom-get-string
					       'Protected) "")
				      (if (and sec (or ori pro)) ", " "")
				      (if ori (lyskom-get-string
					       'no-comments) "")
				      (if (and ori pro) ", " "")
				      (if pro (lyskom-get-string 'closed) "")
				      ")"))
			     (t ""))))
    (lyskom-format-insert 'created-by
			  (format "%25d" (conf-stat->creator conf-stat))
			  (let ((creator (cache-get-conf-stat
					  (conf-stat->creator conf-stat))))
			    (if creator
				(concat
				 (if (> (length (conf-stat->name creator))
					(- (lyskom-window-width) 46))
				     "\n("
				   "(")
					(conf-stat->name creator)")")
			      "")))
    (lyskom-format-insert 'created-at
			  (lyskom-return-time
			   (conf-stat->creation-time conf-stat)))
    (lyskom-format-insert 'members
			  (conf-stat->no-of-members conf-stat))
    (lyskom-format-insert 'garb-nice
			  (conf-stat->garb-nice conf-stat))
    (lyskom-format-insert 'lowest-local-no
			  (conf-stat->first-local-no conf-stat))
    (lyskom-format-insert 'highest-local-no
			  (1- (+ (conf-stat->no-of-texts conf-stat)
				 (conf-stat->first-local-no conf-stat))))
    (lyskom-format-insert 'last-text-time
			  (lyskom-return-time
			   (conf-stat->last-written conf-stat)))
    (lyskom-format-insert 'no-of-motd
			  (conf-stat->msg-of-day conf-stat))
    (lyskom-format-insert 'superconf-is-no-name
			  (conf-stat->super-conf conf-stat)
			  (let ((super-conf (cache-get-conf-stat
					     (conf-stat->super-conf conf-stat))))
			    (if super-conf
				(concat
				 (if (> (length (conf-stat->name super-conf))
					(- (lyskom-window-width) 46))
				     "\n("
				   "(")
				 (conf-stat->name super-conf)")")
			      "")))
    (lyskom-format-insert 'permitted-submitters-no-name
			  (conf-stat->permitted-submitters conf-stat)
			  (let ((permitted-submitters
				 (cache-get-conf-stat
				  (conf-stat->permitted-submitters
				   conf-stat))))
			    (cond
			     (permitted-submitters
			      (concat
			       (if (> (length (conf-stat->name permitted-submitters))
				      (- (lyskom-window-width) 46))
				   "\n("
				 "(")
			       (conf-stat->name permitted-submitters)
			       ")"))
			     ((zerop (conf-stat->permitted-submitters conf-stat))
			      (lyskom-get-string 'Everybody))
			     (t ""))))
    (lyskom-format-insert 'supervisor-is-no-name
			  (conf-stat->supervisor conf-stat)
			  (let ((supervisor (cache-get-conf-stat
					     (conf-stat->supervisor conf-stat))))
			    (if supervisor
				(concat (if (> (length (conf-stat->name supervisor))
					       (- (lyskom-window-width) 46))
					    "\n(" 
					  "(")
					(conf-stat->name supervisor)")")
			      "")))
    (lyskom-format-insert 'presentation-no
			  (conf-stat->presentation conf-stat))

    (if (zerop (conf-stat->msg-of-day conf-stat))
	nil
      (lyskom-run 'main 'lyskom-insert
		  (lyskom-format 'conf-has-motd
			  (conf-stat->name conf-stat)))
      (lyskom-view-text 'main (conf-stat->msg-of-day conf-stat)))

  (lyskom-run 'main 'lyskom-status-conf-2 conf-stat)))


(defun lyskom-status-conf-2 (conf-stat)
  "Show all members of CONF-STAT if the user so wishes."
  (lyskom-scroll)
  (if (null (j-or-n-p (lyskom-get-string 'show-members-list-also-q)))
      (lyskom-end-of-command)
    (initiate-get-members 'main 'lyskom-status-conf-3
			  (conf-stat->conf-no conf-stat)
			  0 lyskom-max-int conf-stat)))


(defun lyskom-status-conf-3 (member-list conf-stat)
  "Receive a list of members in a conference in MEMBER-LIST and call
lyskom-status-conf-4 to show their names. The conference CONF-STAT
is the one we are interested in."
  (lyskom-format-insert 'conf-has-these-members
			(conf-stat->name conf-stat))
  (lyskom-insert-string 'member-list-header)
  (lyskom-traverse 
   member (conf-no-list->conf-nos member-list)
   (lyskom-collect 'main)
   (initiate-get-conf-stat 'main nil member)
   (initiate-query-read-texts 'main nil
			      member (conf-stat->conf-no conf-stat))
   (lyskom-use 'main 'lyskom-status-conf-4 conf-stat))
  (lyskom-run 'main 'lyskom-end-of-command))


(defun lyskom-status-conf-4 (member-conf-stat membership conf-stat)
  "Print a row describing the membership of MEMBER-CONF-STAT 
 (described by MEMBERSHIP) in CONF-STAT."
  (if (or (null member-conf-stat)
	  (null membership))
      (lyskom-insert-string 'secret-membership)
    (lyskom-print-date-and-time (membership->last-time-read membership))
    (let ((unread (- (+ (conf-stat->first-local-no conf-stat)
			(conf-stat->no-of-texts conf-stat))
		     (membership->last-text-read membership)
		     (length (membership->read-texts
			      membership))
		     1)))
      (lyskom-insert (concat (if (zerop unread)
				 "         "
			       (format "%7d  " unread))
			     (conf-stat->name member-conf-stat)
			     "\n")))))


;;; ================================================================
;;;            Status (f|r) Person - status for a person

;;; Author: ceder
;;; Heavily enhanced: Inge Wallin (lyskom-status-pers-3 and beyond)


(defun kom-status-person (&optional pers-no)
  "Prints status for a person."
  (interactive)
  (lyskom-start-of-command 'kom-status-person)
  (if pers-no
      (lyskom-status-pers pers-no)
    (lyskom-completing-read 'main 'lyskom-status-pers
			    (lyskom-get-string 'pers-for-status)
			    'person nil "")))


(defun lyskom-status-pers (pers-no)
  "Print status about PERS-NO."
  (cache-del-conf-stat pers-no)
  (lyskom-collect 'main)
  (initiate-get-pers-stat 'main nil pers-no)
  (initiate-get-conf-stat 'main nil pers-no)
  (lyskom-use 'main 'lyskom-status-pers-2))


(defun lyskom-status-pers-2 (pers-stat conf-stat)
  "Print status about PERS-STAT. The name is in CONF-STAT"
  (cond
   ((null pers-stat)
    (lyskom-insert-string 'no-such-pers)
    (lyskom-end-of-command))
   (t
    (lyskom-format-insert 'pers-status-record
			  (conf-stat->name conf-stat)
			  (conf-stat->conf-no conf-stat))
    (lyskom-format-insert 'created-time
			  (lyskom-return-time
			   (conf-stat->creation-time conf-stat)))

    (lyskom-format-insert 'created-confs
			  (pers-stat->created-confs pers-stat))
    (lyskom-format-insert 'created-persons
			  (pers-stat->created-persons pers-stat))
    (lyskom-format-insert 'created-texts
			  (1- (+ (pers-stat->no-of-created-texts pers-stat)
				 (pers-stat->first-created-text pers-stat))))
    (lyskom-format-insert 'created-lines
			  (pers-stat->created-lines pers-stat))
    (lyskom-format-insert 'created-chars
			  (pers-stat->created-bytes pers-stat))

    (lyskom-format-insert 'no-of-sessions
			  (pers-stat->sessions pers-stat))
    (if (zerop (pers-stat->total-time-present pers-stat))
	nil
      (lyskom-format-insert 'present-time-d-h-m-s
			    (/ (pers-stat->total-time-present pers-stat)
			       (* 24 3600))
			    (% (/ (pers-stat->total-time-present pers-stat)
				  3600) 24)
			    (% (/ (pers-stat->total-time-present pers-stat)
				  60) 60)
			    (% (pers-stat->total-time-present pers-stat)
			       60)))
    (lyskom-format-insert 'last-log-in
			  (lyskom-return-time
			   (pers-stat->last-login pers-stat)))
    (lyskom-format-insert 'user-name
			  (pers-stat->username pers-stat))
    (lyskom-format-insert 'read-texts
			  (pers-stat->read-texts pers-stat))
    (lyskom-format-insert 'time-for-last-letter
			  (lyskom-return-time
			   (conf-stat->last-written conf-stat)))

    (lyskom-format-insert 'superconf
			  (conf-stat->super-conf conf-stat)
			  (let ((super-conf 
				 (cache-get-conf-stat
				  (conf-stat->super-conf conf-stat))))
			    (if super-conf
				(concat
				 (if (> (length (conf-stat->name super-conf))
					(- (lyskom-window-width) 46))
				     "\n("
				   "(")
				 (conf-stat->name super-conf)")")
			      "")))
    (if (not (zerop (conf-stat->supervisor conf-stat)))
	(lyskom-format-insert 'supervisor
			      (conf-stat->supervisor conf-stat)
			      (let ((supervisor
				     (cache-get-conf-stat
				      (conf-stat->supervisor conf-stat))))
				(if supervisor
				    (concat
				     (if (> (length (conf-stat->name supervisor))
					    (- (lyskom-window-width) 46))
					 "\n(" 
				       "(")
				     (conf-stat->name supervisor)")")
				  ""))))
    (lyskom-format-insert 'member-of-confs
			  (pers-stat->no-of-confs pers-stat))
    (lyskom-format-insert 'presentation
			  (conf-stat->presentation conf-stat))))

  (if (not (zerop (conf-stat->msg-of-day conf-stat)))
      (progn
	(lyskom-run 'main 'lyskom-format-insert
		    'has-motd (conf-stat->name conf-stat))
	(lyskom-view-text 'main (conf-stat->msg-of-day conf-stat))))

  (lyskom-run 'main 'lyskom-status-pers-3 conf-stat))


(defun lyskom-status-pers-3 (conf-stat)
  "Show all conferences CONF-STAT is a member of if the user so wishes."
  (lyskom-scroll)
  (if (null (j-or-n-p (lyskom-get-string 'show-membership-list-also-q)))
      (lyskom-end-of-command)
    (initiate-get-membership 'main 'lyskom-status-pers-4
			     (conf-stat->conf-no conf-stat)
			     conf-stat)))


(defun lyskom-status-pers-4 (membership-list conf-stat)
  "Receive an array of memberships and print them using lyskom-status-pers-5.
Args: MEMBERSHIP-LIST CONF-STAT."
  (if (null membership-list)
      (lyskom-format-insert 'not-allowed-see-confs
			    (conf-stat->name conf-stat))
    (lyskom-format-insert 'is-member-of
			  (conf-stat->name conf-stat))
    (lyskom-insert-string 'membership-list-header)
    (setq lyskom-count-var 0)
    (lyskom-traverse
     membership membership-list
     (let ((cs (cache-get-conf-stat (membership->conf-no membership))))
       (and cs
	    (lyskom-time-greater (membership->last-time-read membership)
				 (conf-stat->last-written conf-stat))
	    (cache-del-conf-stat (membership->conf-no membership))))
     (initiate-get-conf-stat 'main 'lyskom-status-pers-5 
			     (membership->conf-no membership)
			     membership conf-stat)))
  (lyskom-run 'main 'lyskom-status-pers-6 conf-stat))


(defun lyskom-status-pers-5 (conf-stat membership member-conf-stat)
  "Print a row describing the membership of MEMBER-CONF-STAT 
 (described by MEMBERSHIP) in CONF-STAT."
  (if (or (null conf-stat)
	  (null membership))
      (lyskom-insert-string 'secret-membership)
    (lyskom-print-date-and-time (membership->last-time-read membership))
    (let ((unread (- (+ (conf-stat->first-local-no conf-stat)
			(conf-stat->no-of-texts conf-stat))
		     (membership->last-text-read membership)
		     (length (membership->read-texts
			      membership))
		     1)))
      (lyskom-insert (concat 
		      (if (zerop unread)
			  "       "
			(format "%6d " unread))
		      (if (= (conf-stat->conf-no member-conf-stat)
			     (conf-stat->supervisor conf-stat))
			  "O "
			"  ")
		      (conf-stat->name conf-stat)
		      "\n"))
      (setq lyskom-count-var (+ lyskom-count-var unread)))))


(defun lyskom-status-pers-6 (conf-stat)
  "Print the total number of unread texts for the person CONF-STAT."
  (lyskom-format-insert 'his-total-unread 
			(conf-stat->name conf-stat)
			lyskom-count-var)
  (lyskom-run 'main 'lyskom-end-of-command))



;;; ================================================================
;;;              Skicka meddelande - Send message

;;; Author: Inge Wallin
;;; Rewritten to use lyskom-read-conf-no by Linus Tolke


(defun kom-send-message ()
  "Send a message to one of the users in KOM right now."
  (interactive)
  (lyskom-start-of-command 'kom-send-message)
  (condition-case error
      (lyskom-send-message 
       (lyskom-read-conf-no (lyskom-get-string 'who-to-send-message-to)
			    'logins t))
    (quit (lyskom-end-of-command)
	  (signal 'quit "Quitting in kom-send-message"))))
  

(defun kom-send-alarm ()
  "Send a message to all of the users in KOM right now."
  (interactive)
  (lyskom-start-of-command 'kom-send-alarm)
  (lyskom-send-message 0))


(defun lyskom-send-message (pers-no)
  "Send a message to the person with the number CONF-NO.  CONF-NO == 0 
means send the message to everybody."
  (let ((string (lyskom-read-string (lyskom-get-string 'message-prompt))))
    (lyskom-collect 'main)
    (initiate-send-message 'main nil pers-no string)
    (initiate-get-conf-stat 'main nil pers-no)
    (lyskom-use 'main 'lyskom-send-message-2 string)))


(defun lyskom-send-message-2 (reply to-conf-stat string)
  "Receive the reply from send-message and report on it."
  (if reply
      (lyskom-handle-as-personal-message
       (if to-conf-stat
	   (lyskom-format 'message-sent-to-user
			  string (conf-stat->name to-conf-stat))
	 (lyskom-format 'message-sent-to-all string))
       lyskom-pers-no)
    (lyskom-insert-before-prompt
     (lyskom-get-string 'message-nope))) ;+++ lyskom-errno
  (lyskom-end-of-command))



;;; ================================================================
;;;       Endast l{sa senaste - Set unread articles in a conf. 
;;;            (Skip or re-read articles).

;;; Author: Linus Tolke


(defun kom-set-unread ()
  "Set number of unread articles in current conference."
  (interactive)
  (lyskom-start-of-command 'kom-set-unread)
  (cond
   ((zerop lyskom-current-conf)
    (lyskom-insert-string 'not-present-anywhere)
    (lyskom-end-of-command))
   (t
    (initiate-get-conf-stat 'main 'lyskom-set-unread
			    lyskom-current-conf))))


(defun lyskom-set-unread (conf-stat)
  "Ask user how many unread texts he wants to have in CONF-STAT."
  (cond
   ((null conf-stat)			;+++ annan errorhantering
    (lyskom-insert "Error!\n")		;+++ Hrrrmmmmffff????
    (lyskom-end-of-command))
   (t
    (let ((n (lyskom-read-num-range
	      0 (conf-stat->no-of-texts conf-stat)
	      (lyskom-format 'only-last
			     (conf-stat->no-of-texts conf-stat)
			     (conf-stat->name conf-stat)))))

      (lyskom-halt 'main)
      (initiate-set-unread 'set-unread 'lyskom-set-unread-2
			   (conf-stat->conf-no conf-stat) n)
      (lyskom-run 'set-unread 'lyskom-resume 'main)))))


(defun lyskom-set-unread-2 (flg)
  "Re-fetch everything if there was success."
  (if (null flg)
      (progn
	(lyskom-insert-string 'only-error)
	(lyskom-end-of-command))
    (lyskom-refetch)))


;;; ================================================================
;;;                 Lista Nyheter - List News

;;; Author:   Linus Tolke
;;; Rehacked: Inge Wallin


(defun kom-list-news (num)
  "Runs lyskom-start-of-command and then gets all conferences using 
lyskom-prefetch-all-confs."
  (interactive "P")
  (lyskom-start-of-command 'kom-list-news)
  (lyskom-prefetch-all-confs (cond
			      ((numberp num) num)
			      ((and (listp num)
				    (numberp (car num))) (car num))
			      (t nil)) 'lyskom-list-news))


(defun lyskom-list-news (num-arg)
  "Print the number of unread articles to the user."
  (interactive)
  (let ((sum 0))
    (mapcar
     (function
      (lambda (info)
	(let ((un (length (cdr (read-info->text-list info))))
	      (name (conf-stat->name (read-info->conf-stat info))))
	  (cond
	   ((eq (read-info->type info) 'CONF)
	    (if (or (not num-arg)
		    (>= (-- num-arg) 0))
		(lyskom-insert 
		 (if (/= un 1)
		     (lyskom-format 'you-have-unreads un name)
		   (lyskom-format 'you-have-an-unread name))))
	    (setq sum (+ sum un)))))))
     (read-list->all-entries lyskom-to-do-list))
    (if (= 0 sum)
	(lyskom-insert-string 'you-have-read-everything)
      (lyskom-insert 
       (if (/= sum 1)
	   (lyskom-format 'total-unreads 
		   sum
)
	 (format (lyskom-get-string 'total-unread))))))
  (lyskom-end-of-command))


;;; ================================================================
;;; 			V{nta - Idle wait


(defun kom-busy-wait (arg)
  "Sets the kom-session in wait-mode.
The wait-mode is interrupted when a text in a conference with higher priority
than that of the next text to be read.
If you want another priority to break that the ones higher that the next text 
to be read, give the priority as a prefix argument.
When a text is received the new text is displayed."
  (interactive "P")
  (lyskom-start-of-command 'kom-busy-wait)
  (if (not (read-list-isempty lyskom-reading-list))
      (set-read-list-empty lyskom-reading-list))
  (let ((waitfor (or (cond
		      ((integerp arg) arg)
		      ((listp arg) (car arg)))
		     (read-info->priority
		      (read-list->first lyskom-to-do-list))
		     -2)))
    (lyskom-tell-internat 'kom-tell-wait)
    (if (= waitfor -2)
	(lyskom-insert-string 'waiting-for-anything)
      (lyskom-format-insert 'waiting-higher-than waitfor))
    (lyskom-scroll)
    (setq lyskom-is-waiting
	  (list '>
		'(or (read-info->priority
		      (read-list->first lyskom-reading-list))
		     (read-info->priority
		      (read-list->first lyskom-to-do-list))
		     257)
		waitfor))))


;;; ================================================================
;;;                    Lista nyheter - List unread

;;; Author: Linus Tolke


(defun kom-list-news-old ()
  "List the amount of unread texts in all conferences.
Lista nyheter"
  (interactive)
  (lyskom-start-of-command 'kom-list-news)
  (lyskom-halt 'main)
  (setq lyskom-count-var 0)
  (mapcar (function
	   (lambda (memb-ship)
	     (initiate-get-conf-stat 'membership
				     'lyskom-list-unread-maybe-get-map
				     (membership->conf-no memb-ship)
				     memb-ship)))
	  lyskom-membership)
  (lyskom-run 'membership 'lyskom-list-unread-summary))


(defun lyskom-list-unread-maybe-get-map (conf-stat memb-ship)
  "If a map is needed to calculate unread then send for one.
Else call lyskom-list-unread-print with nil instead of map.
Args: CONF-STAT MEMB-SHIP."
  (lyskom-halt 'membership)
  (if (not (lyskom-time-greater (membership->last-time-read memb-ship)
				(conf-stat->last-written conf-stat)))
      (lyskom-run 'membership-2
		  'lyskom-list-unread-print nil conf-stat memb-ship)
    (initiate-get-map 'membership-2 'lyskom-list-unread-print
		      (membership->conf-no memb-ship)
		      (1+ (membership->last-text-read memb-ship))
		      (+ (conf-stat->no-of-texts conf-stat)
			 (conf-stat->first-local-no conf-stat)
			 (- (membership->last-text-read memb-ship)))
		      conf-stat
		      memb-ship)))


(defun lyskom-list-unread-print (map conf-stat memb-ship)
  "Prints how many unread a conference has. And adds to lyskom-count-var.
MAP is a map och articles for that conference.
CONF-STAT is the conference status for that conference and
MEMBERSHIP is the membership for the user in that conference."
  (let ((antal
	 (if map
	     (length (lyskom-list-unread map memb-ship))
	   0)))
    (if (> antal 0)
	(lyskom-insert 
	 (if (= antal 1)
	     (lyskom-format 'you-have-an-unread (conf-stat->name conf-stat))
	   (lyskom-format 'you-have-unreads
			  antal (conf-stat->name conf-stat)))))
    (setq lyskom-count-var (+ lyskom-count-var antal))
    (lyskom-resume 'membership)))


(defun lyskom-list-unread-summary ()
  "Sums up the kom-list-unread command and allows main to start again."
  (if (> lyskom-count-var 0)
      (lyskom-insert
       (if (= lyskom-count-var 1)
	   (lyskom-get-string 'total-unread)
	 (lyskom-format 'total-unreads lyskom-count-var)))
    (lyskom-insert-string 'you-have-read-everything))
  (lyskom-resume 'main)
  (lyskom-end-of-command))


(defun lyskom-time-greater (time1 time2)
  "Returns t if TIME2 is before TIME1 chronologically."
  (cond
   ((< (time->year time2) (time->year time1)))
   ((< (time->mon time2) (time->mon time1)))
   ((< (time->mday time2) (time->mday time1)))
   ((< (time->hour time2) (time->hour time1)))
   ((< (time->min time2) (time->min time1)))
   ((< (time->sec time2) (time->sec time1)))
   (t nil)))


;;; ================================================================
;;;               Lista {rende - list summary

;;; Author: Linus Tolke


(defun kom-list-summary ()
  "List a summary of the unread in the current conf.
The summary contains the date, number of lines, author and subject of the text
on one line."
  (interactive)
  (lyskom-start-of-command 'kom-list-summary)
  (if (read-list-isempty lyskom-reading-list)
      (progn 
	(lyskom-insert-string 'have-to-be-in-conf-with-unread)
	(lyskom-end-of-command))
    (initiate-get-time 'main 'lyskom-list-summary-all 
		       (text-list->texts 
			(read-info->text-list 
			 (let ((list (read-list->all-entries lyskom-reading-list))
			       (len (read-list-length lyskom-reading-list))
			       (r 0))
			   (while (< r len)
			     (let ((type (read-info->type 
					  (read-list->nth lyskom-reading-list
							  r))))
			       (if (or (eq type 'CONF)
				       (eq type 'REVIEW-MARK)
				       (eq type 'REVIEW))
				   (setq len 0)
				 (++ r))))
			   (read-list->nth lyskom-reading-list r)))))))


(defun lyskom-list-summary-all (time texts)
  "Handles the returned time from kom-list-summary.
Then starts fetching all text-stats and text to list them."
  (if time				;+++ annan felhantering
      (progn
	(lyskom-insert (format "%-7s%-6s%5s%s%s\n"
			       (lyskom-get-string 'Texts)
			       (lyskom-get-string 'Date)
			       (lyskom-get-string 'Lines)
			       (lyskom-fix-str (/ (- (lyskom-window-width) 21)
						  3)
					       (lyskom-get-string 'Author))
			       (lyskom-get-string 'Subject)))
	(lyskom-traverse
	 text-no texts
	 (lyskom-collect 'main)
	 (initiate-get-text-stat 'main nil text-no)
	 (initiate-get-text      'main nil text-no)
	 (lyskom-use 'main 'lyskom-list-summary text-no 
		     (time->year time) (time->yday time))
	 (sit-for 0))))
  (lyskom-run 'main 'lyskom-end-of-command))


(defun lyskom-list-summary (text-stat text text-no year day)
  "Handle the info, fetch the author and print it.
Args: TEXT-STAT TEXT TEXT-NO YEAR DAY.
The year and day is there to be able to choose format on the day.
Format is 23:29 if the text is written today. Otherwise 04-01."
  (if (not (and text-stat text))	;+++ B{ttre felhantering.
      (lyskom-format-insert 'could-not-read text-no)
    (let* ((lines (text-stat->no-of-lines text-stat))
	   (txt (text->text-mass text))
	   (eos (string-match (regexp-quote "\n") txt))
	   (subject (substring txt 0 eos))
	   ;; length of the number %%%%%% :7
	   ;; length for time is: 6
	   (time (text-stat->creation-time text-stat))
	   (time (if (and (= year (time->year time))
			  (= day (time->yday time)))
		     (format "%02d:%02d" (time->hour time)
			     		 (time->min time))
		   (format "%02d-%02d" (1+ (time->mon time))
			   	       (time->mday time))))
	   ;; length for lines is: 4
	   ;; We split the rest between author and subject
	   (namelen (/ (- (lyskom-window-width) 21) 3))
	   (subjlen (/ (* (- (lyskom-window-width) 21) 2) 3)))
      (lyskom-insert (lyskom-fix-str 7 (format "%d" text-no)))
      (lyskom-insert time)
      (lyskom-insert (format "%4d  " lines))
      (lyskom-halt 'main)
      (lyskom-queue-print-name 'summary (text-stat->author text-stat)
			       t namelen)
      (lyskom-run 'summary 'lyskom-insert (concat 
					   "  "
					   (lyskom-fix-str subjlen subject)
					   "\n"))
      (lyskom-run 'summary 'lyskom-resume 'main))))



;;; ================================================================
;;;      kom-display-who-buffer - Visa vilka-listan

;;; Author: Linus Tolke


(defun kom-display-who-buffer ()
  "Make the who-buffer appear on the screen as a temp buffer."
  (interactive)
  (lyskom-start-of-command 'kom-display-who-buffer)
  (let ((win (selected-window))
	(who (display-buffer lyskom-who-info-buffer)))
    (unwind-protect
	(progn
	  (select-window who)
	  (if (numberp kom-who-buffer-size-when-displaying)
	      (enlarge-window (- kom-who-buffer-size-when-displaying 
				 (window-height who)))))
      (select-window win)
      (lyskom-end-of-command))))



;;; ================================================================
;;;          Hj{lp vid del av kommando - Help function

;;; Author: Linus Tolke


(defun lyskom-help ()
  "Prints a short list of alternatives when you don't know what you can do."
  (interactive)
  (let* ((tohere (cond
		  ((stringp (this-command-keys))
		   (substring (this-command-keys) 0 -1))
		  (t			;This is the case in the lucid-emacs.
		   (let* ((tck (this-command-keys))
			  (newvec (make-vector (1- (length tck)) nil))
			  (r 0))
		     (while (< r (length newvec))
		       (aset newvec r (aref tck r))
		       (++ r))
		     newvec))))
	 (binding (key-binding tohere))
	 (keymap (cond
		  ((and (symbolp binding)
			(fboundp binding))
		   (symbol-function binding))
		  (t binding)))
	 (keylis (cond
		  ((fboundp 'map-keymap)
		   (and keymap
			(let (list)
			  (map-keymap
			   (function
			    (lambda (event function)
			      (setq list (cons (cons event function) list))))
			   keymap t)
			  (nreverse list))))
		  ((vectorp keymap)
		   (let ((lis nil)
			 (r 0))
		     (while (< r (length keymap))
		       (if (aref keymap r)
			   (setq lis (cons (cons r (aref keymap r))
					   lis)))
		       (++ r))
		     (nreverse lis)))
		  (t
		   (cdr keymap))))
	 (text (format "\n%s: \n%s\n"
		       (mapconcat 'single-key-description tohere " ")
		       (mapconcat
			(function
			 (lambda (arg)
			   (format "%s - %s" 
				   (if (fboundp 'map-keymap)
				       (if (symbolp (car arg))
					   (format "%s" (car arg))
					 (format "%c" (car arg)))
				     (format "%c" (car arg)))
				   (or (lyskom-command-name (cdr arg))
				       (and (keymapp (cdr arg))
					    (lyskom-get-string 'multiple-choice))
				       (cdr arg)))))
			keylis
			"\n")))
	 next-char)
    (if (eq major-mode 'lyskom-mode)
	(progn
	  (lyskom-insert text)
	  (lyskom-end-of-command))
      (with-output-to-temp-buffer "*Help*"
	(princ text)))))
;    (setq next-char (read-char))
;    (cond
;     ((commandp (key-binding (concat tohere (char-to-string next-char))))
;      (command-execute (concat tohere (char-to-string next-char))))
;     (t (lyskom-message (lyskom-get-string 'does-not-exist))))


;;; ================================================================
;;;           Skapa bugg-rapport - Compile bugg-report

;;; Author: Linus Tolke


(defun kom-bug-report ()
  "This command should make it easier to include the correct info in a buggreport"
  (interactive)
  (let* ((curbuf (current-buffer))
	 (old-buf (condition-case ()
			    debugger-old-buffer
			  (void-variable (current-buffer))))
	 (repname "*lyskom-bugreport*"))
    (lyskom-message "%s" (lyskom-get-string 'buggreport-compilestart))
    (set-buffer old-buf)
    (cond
     ((eq old-buf (process-buffer lyskom-proc)))
     ((save-excursion
	(set-buffer (process-buffer lyskom-proc))
	(set-buffer lyskom-unparsed-buffer)
	(eq old-buf (current-buffer)))
      (set-buffer (process-buffer lyskom-proc)))
     (t
      (error "I dont know what buffer you are running lyskom in (%s)?" 
	     old-buf)))
    (with-output-to-temp-buffer repname
      (princ (lyskom-get-string 'buggreport-description))
      (princ (lyskom-get-string 'buggreport-internals))
      (princ (lyskom-get-string 'buggreport-command-keys))
      (terpri)
      (princ (key-description (recent-keys)))
      (terpri)
      (princ (lyskom-get-string 'buggreport-version))
      (print lyskom-clientversion)
      (princ (lyskom-get-string 'buggreport-emacs-version))
      (print (emacs-version))
      (princ (lyskom-get-string 'buggreport-system-id))
      (print system-type)
      (princ (lyskom-get-string 'buggreport-ctl-arrow-doc))
      (print (condition-case error
		 (documentation-property 'ctl-arrow 'variable-documentation)
	       (error)))

      (princ (lyskom-get-string 'buggreport-unparsed))
      (print (save-excursion
	       (set-buffer lyskom-unparsed-buffer)
	       (goto-char (point-min))
	       (forward-line 10)
	       (buffer-substring (point-min) (point))))
      (if (condition-case ()
	      debugger-old-buffer
	    (void-variable nil))
	  (princ (lyskom-format 'buggreport-backtrace
				(save-excursion
				  (set-buffer curbuf)
				  (buffer-substring (point-min) 
						    (point-max))))))
      (if lyskom-debug-communications-to-buffer
	  (progn
	    (princ (lyskom-get-string 'buggreport-communications))
	    (print (save-excursion
		     (set-buffer lyskom-debug-communications-to-buffer-buffer)
		     (buffer-substring (point-min) (point-max))))))
      (princ (lyskom-get-string 'buggreport-all-kom-variables))
      (mapatoms
       (function
	(lambda (symbol)
	  (and (boundp symbol)
	       (string-match "^\\(kom-\\|lyskom-\\)" (symbol-name symbol))
	       (not (string-match "-cache$\\|^kom-dict$\\|^lyskom-strings$\
\\|-map$\\|^lyskom-commands$\\|^lyskom-swascii"
				  (symbol-name symbol)))
	       (progn
		 (terpri)
		 (princ (symbol-name symbol))
		 (princ ":")
		 (print (symbol-value symbol))))))))
    
    (save-excursion
      (set-buffer repname)
      (goto-char (point-min))
      (replace-regexp "byte-code(\".*\""
		      (lyskom-get-string 'buggreport-instead-of-byte-comp)))
    (lyskom-message "%s" (lyskom-get-string 'buggreport-compileend))))

(fset 'kom-compile-bug-report (symbol-function 'kom-bug-report))


; ;;; ================================================================
; ;;;          Status f|r en session - Status session
; 
; ;;; Author: ceder
;
; 
; (defun kom-status-session ()
;   "Show some status about this session."
;   (interactive)
;   (lyskom-start-of-command 'kom-status-session)
;   (lyskom-collect 'ss)
;   (initiate-get-session-info 'ss nil)
;   (initiate-get-conf-stat 'ss nil lyskom-current-pers-no)
;   (initiate-get-conf-stat 'ss nil lyskom-current-conf)
;   (lyskom-use 'ss 'lyskom-print-status-session)
;   (lyskom-run 'ss 'lyskom-end-of-comand))
;
; 
; (defun lyskom-print-status-session (session-info person working)
;   "Print a short message in the *lyskom* buffer.
; Args: SESSION-INFO PERSON WORKING.
; PERSON is conf-stat of the user, or nil if he hasn't logged in.
; WORKING is conf-stat of current conference, or nil."
;   (cond
;    ((null person)
;     (lyskom-insert-string 'not-logged-in))
;    ((null working)
;     (lyskom-insert
;      (lyskom-format 'name-is-not-in-conf (conf-stat->name person))))
;    (t
;     (lyskom-insert
;      (lyskom-format 'name-is-in-conf (conf-stat->name person)
; 	     (conf-stat->name working)))))
;   (lyskom-insert
;    (lyskom-format 'connected-during
;     (session-info->connect-time session-info))))
; 
; ;ceder (Per Cederqvist) {r n{rvarande i
; ;PC/AT/386-(,icke-)kompatibla med/utan (MS,PC)DOS - 69 ol{sta.
; ;Totalt 86 ol{sta i 12 m|ten.   N{rvarotid: 0:12:03


;;; ================================================================
;;;      [ndra livsl{ngd - Set lifespan of texts in a conference

;;; Author: Inge Wallin


(defun kom-set-garb-nice ()
  "Set the garb-nice value for a conference."
  (interactive)
  (lyskom-start-of-command 'kom-set-garb-nice)
  (lyskom-completing-read-conf-stat 
   'main 'lyskom-set-garb-nice
   (lyskom-get-string 'conf-to-set-garb-nice-q)
   nil nil ""))


(defun lyskom-set-garb-nice (conf-stat)
  "Set the garb-nice value for CONF-STAT."
  (if (not conf-stat)
      (progn
	(lyskom-insert-string 'somebody-deleted-that-conf)
	(lyskom-end-of-command))
    (let ((garb-nice (lyskom-read-number
		      (lyskom-get-string 'new-garb-nice-q))))
      (lyskom-format-insert 'garb-nice-for-is
			    (conf-stat->name conf-stat)
			    garb-nice)
      (initiate-set-garb-nice 'main 'lyskom-set-garb-nice-2
			      (conf-stat->conf-no conf-stat) garb-nice
			      conf-stat))))


(defun lyskom-set-garb-nice-2 (answer conf-stat)
  "Get the answer from lyskom-set-garb-nice and report the result."
  (if (not answer)
      (lyskom-insert-string 'nope)	;+++lyskom-errno
    (lyskom-insert-string 'done)
    (cache-del-conf-stat (conf-stat->conf-no conf-stat)))
  (lyskom-end-of-command))


;;; ================================================================
;;;       S{tt till}tna f|rfattare - set-permitted-submitters

;;; Author: Linus Tolke

(defun kom-set-permitted-submitters ()
  "Set the permitted submitters of a conference."
  (interactive)
  (lyskom-start-of-command 'kom-set-permitted-submitters)
  (lyskom-completing-read-conf-stat
   'main 'lyskom-set-conf-for-conf
   (lyskom-get-string 'conf-to-set-permitted-submitters-q)
   nil nil "" 'permitted-submitters))


;;; ================================================================
;;;             [ndra superm|te - Set super conference 

;;; Author: Inge Wallin


(defun kom-set-super-conf ()
  "Set the super conference for a conference."
  (interactive)
  (lyskom-start-of-command 'kom-set-super-conf)
  (lyskom-completing-read-conf-stat 
   'main 'lyskom-set-conf-for-conf
   (lyskom-get-string 'conf-to-set-super-conf-q)
   nil nil "" 'super-conf))


(defun lyskom-set-conf-for-conf (conf-stat type)
  "Set the super conference of permitted submitters value for CONF-STAT.
Which it is depends on the value of the TYPE parameter."
  (if (not conf-stat)
      (progn
	(lyskom-insert-string 'somebody-deleted-that-conf)
	(lyskom-end-of-command))
    (lyskom-completing-read-conf-stat
     'main 'lyskom-set-conf-for-conf-2
     (lyskom-format (if (eq type 'super-conf)
			'new-super-conf-q
		      'new-permitted-submitters-q)
		    (conf-stat->name conf-stat))
     nil (if (eq type 'permitted-submitters)
	     'empty) "" conf-stat type)))


(defun lyskom-set-conf-for-conf-2 (super-conf conf-stat type)
  "Set the super conference or permitted submitters for CONF-STAT to SUPER-CONF.
Args: SUPER-CONF CONF_STAT TYPE"
  (if (and (eq type 'permitted-submitters)
	   (eq super-conf 0))		;Allowing all to write there.
      (progn
	(lyskom-format-insert 'permitted-submitters-removed-for-conf 
			      (conf-stat->name conf-stat))
	(initiate-set-permitted-submitters 'main 'lyskom-set-conf-for-conf-3
					   (conf-stat->conf-no conf-stat) 0
					   conf-stat type))
    (lyskom-format-insert (if (eq type 'super-conf)
			      'super-conf-for-is
			    'submitters-conf-for-is)
			  (conf-stat->name conf-stat)
			  (conf-stat->name super-conf))
    (if (eq type 'super-conf)
	(initiate-set-super-conf 'main 'lyskom-set-conf-for-conf-3
				 (conf-stat->conf-no conf-stat) 
				 (conf-stat->conf-no super-conf) 
				 conf-stat type)
      (initiate-set-permitted-submitters 'main 'lyskom-set-conf-for-conf-3
					 (conf-stat->conf-no conf-stat) 
					 (conf-stat->conf-no super-conf) 
					 conf-stat type))))


(defun lyskom-set-conf-for-conf-3 (answer conf-stat type)
  "Get the answer from lyskom-set-conf-for-conf and report the result."
  (if (not answer)
      (lyskom-insert-string 'nope)	;+++ lyskom-errno
    (lyskom-insert-string 'done)
    (cache-del-conf-stat (conf-stat->conf-no conf-stat)))
  (lyskom-end-of-command))


;;; ================================================================
;;;                  St{ng av servern - Shutdown

;;; Author: Inge Wallin


(defun kom-shutdown-server ()
  "Shutdown the LysKOM server."
  (interactive)
  (lyskom-start-of-command 'kom-shutdown-server)
  (if (ja-or-nej-p (lyskom-get-string 'really-shutdown))
      (progn
	(lyskom-insert-string 'closing-server)
	(initiate-shutdown 'main 'lyskom-handle-command-answer 0))
    (lyskom-end-of-command)))


;;; ================================================================
;;;       \verg} till adm.mod - Enable administrator capabilities
;;;     \verg} till normalmod - Disable administrator capabilities

;;; Author: Inge Wallin


(defun kom-enable-adm-caps ()
  "Enable the LysKOM adminstrator commands for the current user."
  (interactive)
  (lyskom-start-of-command 'kom-enable-adm-caps)
  (initiate-enable 'main 'lyskom-enable-adm-caps 255 
		   (lyskom-get-string 'administrator) t))


(defun kom-disable-adm-caps ()
  "Disable the LysKOM adminstrator commands for the current user."
  (interactive)
  (lyskom-start-of-command 'kom-disable-adm-caps)
  (initiate-enable 'main 'lyskom-enable-adm-caps 0 
		   (lyskom-get-string 'no-longer-administrator) nil))


(defun lyskom-enable-adm-caps (answer string is-administrator)
  "Tell the user if the call succeded."
  (if answer
      (progn
	(lyskom-format-insert 'you-are-now string)
	(setq lyskom-is-administrator is-administrator))
    (lyskom-insert-string 'nope))	;+++ lyskom-errno
  (lyskom-end-of-command))


;;; ================================================================
;;;         S{tt loginmeddelande - Set message of the day

;;; Author: Inge Wallin


(defun kom-set-motd ()
  "Set the message of the day for LysKOM."
  (interactive)
  (lyskom-start-of-command 'kom-set-motd)
  (if (server-info->motd-of-lyskom lyskom-server-info)
      (initiate-get-text 'main 'lyskom-set-motd
			 (server-info->motd-of-lyskom lyskom-server-info))
    (lyskom-set-motd nil)))


(defun lyskom-set-motd (old-motd-text)
  "Set the message of the day for LysKOM. 
Use OLD-MOTD-TEXT as the default text if non-nil."
  
  (lyskom-edit-text
   lyskom-proc
   (lyskom-create-misc-list)
   (if (and old-motd-text
	    (string-match "\n" (text->text-mass old-motd-text)))
       (substring (text->text-mass old-motd-text) 0 (1- (match-end 0)))
     "")
   (if (and old-motd-text
	    (string-match "\n" (text->text-mass old-motd-text)))
       (substring (text->text-mass old-motd-text) (match-end 0))
     "")
   'lyskom-set-motd-2))


(defun lyskom-set-motd-2 (text-no)
  "Set motd of LysKOM to the newly created text TEXT-NO."
  (lyskom-insert-before-prompt
   (lyskom-format 'setting-motd text-no))
  (initiate-set-motd-of-lyskom 'background 'lyskom-set-motd-3
			       text-no text-no))


(defun lyskom-set-motd-3 (result text-no)
  "Handle the return from the initiate-set-motd-of-lyskom call."
  (if result
      (progn
	(lyskom-insert-before-prompt
	 (lyskom-get-string (if (zerop text-no)
				'removed-motd 
			      'set-motd-success)))
	(set-server-info->motd-of-lyskom lyskom-server-info text-no))
    (lyskom-insert-before-prompt
     (lyskom-get-string 'set-motd-failed))))


;;; ================================================================
;;;       Ta bort loginmeddelande - Remove message of the day

;;; Author: Inge Wallin


(defun kom-remove-motd ()
  "Remove the message of the day for LysKOM."
  (interactive)
  (lyskom-start-of-command 'kom-remove-motd)
  (lyskom-insert-string 'removing-motd)
  (initiate-set-motd-of-lyskom 'background 'lyskom-set-motd-3
			       0 0)
  (lyskom-end-of-command))

;;; ================================================================
;;;                  Kasta ut - force logout

;;; Author: Inge Wallin


(defun kom-force-logout ()
  "Force another user to log out."
  (interactive)
  (lyskom-start-of-command 'kom-force-logout)
  (let ((session (lyskom-read-number (lyskom-get-string 'who-to-throw-out))))
    (lyskom-format-insert 'throwing-out session)
    (initiate-disconnect 'main 'lyskom-handle-command-answer
			 session)))


;;; ================================================================
;;;                  Skjut upp l{sning - postpone

;;; Author: Per Cederqvist


(defun kom-postpone (today)
  "Postpone the reading of all but the last TODAY articles in the
current conference to another session."
  (interactive (list
		(cond
		 ((null current-prefix-arg)
		  (lyskom-read-number
		   (lyskom-get-string 'postpone-prompt)
		   17))
		 (t (prefix-numeric-value current-prefix-arg)))))

  (lyskom-start-of-command 'kom-postpone)

  (let ((len (read-list-length lyskom-reading-list))
	(finished nil))
    (while (and (not finished)
		(> len 0))
      (let ((type (read-info->type (read-list->first lyskom-reading-list))))
	(cond 
	 ((or (eq type 'REVIEW)
	      (eq type 'REVIEW-TREE)
	      (eq type 'REVIEW-MARK))
	  (read-list-rotate lyskom-reading-list))
	 ((or (eq type 'COMM-IN)
	      (eq type 'FOOTN-IN))
	  (set-read-list-del-first lyskom-reading-list))
	 ((eq type 'CONF)
	  (let* ((rlist (read-info->text-list
			 (read-list->first lyskom-reading-list)))
		 (cell (nthcdr (max (- (length rlist) today) 1)
			       rlist)))
	    (setcdr rlist cell))
	  (setq finished t))
	 (t
	  (signal 'lyskom-internal-error '("lyskom-remove-comment-chains")))))
      (-- len)))

  ;; Delete the 'CONF entry if we selected 0 entries.
  (cond
   ((zerop today)
    (read-list-delete-text nil lyskom-reading-list)
    (read-list-delete-text nil lyskom-to-do-list)))

  (lyskom-end-of-command))



;;; ================================================================
;;;              Begrav lyskom-sessionen - kom-bury

;;; Author: Linus Tolke

(defun kom-bury () 
  "Puts the kom-session in the background."
  (interactive)
  (let ((session-name (buffer-name (current-buffer)))
	(buffer (current-buffer)))
    (if lyskom-debug-communications-to-buffer
	(bury-buffer lyskom-debug-communications-to-buffer))
    (if lyskom-who-info-buffer
	(bury-buffer lyskom-who-info-buffer))
    (bury-buffer)
    (while (and (string-match (regexp-quote session-name) (buffer-name (current-buffer)))
		(not (eq buffer (current-buffer))))
      (bury-buffer))))


(defun kom-next-kom ()
  "Pop up the next lyskom-session."
  (interactive)
  (lyskom-tell-internat 'kom-tell-next-lyskom)
  (let ((buffers (buffer-list)))
    (while (and buffers
		(or (eq (car buffers) (current-buffer))
		    (not (save-excursion
			   (set-buffer (car buffers))
			   (and (boundp 'lyskom-proc)
				lyskom-proc
				(processp lyskom-proc)
				(memq (process-status lyskom-proc) '(run open))
				(eq (current-buffer) (process-buffer lyskom-proc)))))))
      (setq buffers (cdr buffers)))
    (if buffers
	(progn
	  (kom-bury)
	  (switch-to-buffer (car buffers))))))

