;;;;;
;;;;; $Id: commands2.el,v 38.17 1996-02-17 15:36:02 byers Exp $
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
	      "$Id: commands2.el,v 38.17 1996-02-17 15:36:02 byers Exp $\n"))


;;; ================================================================
;;;              Lista medlemsskap - List membership

;;; Author: Linus Tolke
;;; Rewritten by ceder


;; This functions is left in its "asynchronous way".
(def-kom-command kom-membership ()
  "Show memberships last visited, priority, unread and name."
  (interactive)
  (let ((buf (current-buffer))
	(buffer (get-buffer-create (concat (buffer-name (current-buffer))
					   "-membership"))))
    (save-window-excursion
      (set-buffer buffer)
      (make-local-variable 'lyskom-buffer)
      (local-set-key [mouse-2] 'kom-mouse-2)
      (setq lyskom-buffer buf)
      (erase-buffer)
      (insert (lyskom-get-string 'your-memberships))
      (insert (lyskom-get-string 'memberships-header)))
    (display-buffer buffer)
    (lyskom-traverse
     x lyskom-membership
     (initiate-get-conf-stat 'membership 'lyskom-memb-received-1
			     (membership->conf-no x)
			     x buffer))))

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
    (lyskom-format-insert 'memberships-line
			  (lyskom-return-date-and-time  (membership->last-time-read
						membership))
			  (membership->priority membership)
			  (if map
			      (length (lyskom-list-unread map membership))
			    0)
			  conf-stat)))


;;; ================================================================
;;;          Status (f|r) M|te - Status for a conference

;;; Author: ceder (with some help by Linus)
;;; much enhanced by Inge Wallin (lyskom-status-conf-2 and beyond)


(def-kom-command kom-status-conf (&optional conf-no)
  "Prints conference status.
If argument CONF-NO is existing and non-nil then this conference is used. 
otherwise: the conference is read with lyskom-completing-read."
  (interactive)
  (let ((conf-no
	 (or conf-no
	     (lyskom-read-conf-no (lyskom-get-string 'conf-for-status)
				  'all)))
	conf-stat)
    (cache-del-conf-stat conf-no)
    (setq conf-stat (blocking-do 'get-conf-stat conf-no))
    (if (null conf-stat)
	(lyskom-insert-string 'no-such-conf)
      (let* ((type (conf-stat->conf-type conf-stat))
	     (box (conf-type->letterbox type))
	     (ori (conf-type->original type))
	     (pro (conf-type->rd_prot type))
	     (sec (conf-type->secret type)))
	(lyskom-format-insert 'status-record
			      conf-stat
			      conf-stat
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
      (let ((creator (or (blocking-do 'get-conf-stat
				      (conf-stat->creator conf-stat))
			 (conf-stat->creator conf-stat))))

	(lyskom-format-insert 'created-by
			      creator
			      creator
			      (lyskom-default-button 'conf
						     creator)
			      (if (and 
				   (lyskom-conf-stat-p creator)
				   (> (length (conf-stat->name creator))
				      (- (lyskom-window-width) 46)))
				  "\n"
				"")))
      (lyskom-format-insert 'created-at
			    (lyskom-return-date-and-time
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
			    (lyskom-return-date-and-time
			     (conf-stat->last-written conf-stat)))
      (lyskom-format-insert 'no-of-motd
			    (conf-stat->msg-of-day conf-stat))
      (let ((superconf 
	     (or (blocking-do 'get-conf-stat
			      (conf-stat->super-conf conf-stat))
		 (conf-stat->super-conf conf-stat))))
	(lyskom-format-insert 'superconf-is-no-name
			      superconf
			      superconf
			      (lyskom-default-button 'conf
						     superconf)
			      (if (and 
				   (lyskom-conf-stat-p superconf)
				   (> (length (conf-stat->name superconf))
				      (- (lyskom-window-width) 46)))
				  "\n"
				"")))
      (let ((permitted-submitters
	     (or (blocking-do 'get-conf-stat 
			      (conf-stat->permitted-submitters conf-stat))
		 (conf-stat->permitted-submitters conf-stat))))
	(lyskom-format-insert 'permitted-submitters-no-name
			      permitted-submitters
			      (if (zerop (conf-stat->permitted-submitters
					  conf-stat))
				  (lyskom-get-string 'Everybody)
				permitted-submitters)
			      (lyskom-default-button 'conf
						     permitted-submitters)
			      (cond
			       ((zerop (conf-stat->permitted-submitters
					conf-stat))
				"")
			       ((lyskom-conf-stat-p permitted-submitters)
				(if (> (length 
					(conf-stat->name 
					 permitted-submitters))
				       (- (lyskom-window-width) 46))
				    "\n"
				  ""))
			       (t permitted-submitters))))
      (let ((supervisor (or (blocking-do 'get-conf-stat
					 (conf-stat->supervisor conf-stat))
			    (conf-stat->supervisor conf-stat))))
	(lyskom-format-insert 'supervisor-is-no-name
			      supervisor
			      supervisor
			      (lyskom-default-button 'conf
						     supervisor)
			      (if (and 
				   (lyskom-conf-stat-p supervisor)
				   (> (length (conf-stat->name supervisor))
				      (- (lyskom-window-width) 46)))
				  "\n"
				"")))
      (lyskom-format-insert 'presentation-no
			    (conf-stat->presentation conf-stat))

      (if (zerop (conf-stat->msg-of-day conf-stat))
	  nil
	(lyskom-format-insert 'conf-has-motd conf-stat)
	(lyskom-view-text (conf-stat->msg-of-day conf-stat)))

					; Show all members of CONF-STAT if the user so wishes."
      (lyskom-scroll)
      (if (null (lyskom-j-or-n-p (lyskom-get-string 'show-members-list-also-q)))
	  nil
	(let ((member-list (blocking-do 'get-members
					(conf-stat->conf-no conf-stat)
					0 lyskom-max-int)))

	  (lyskom-format-insert 'conf-has-these-members
				conf-stat)
	  (lyskom-insert-string 'member-list-header)
	  (lyskom-traverse 
	      member (conf-no-list->conf-nos member-list)
	    (let ((member-conf-stat (blocking-do 'get-conf-stat member))
		  (membership (blocking-do 'query-read-texts 
					   member
					   (conf-stat->conf-no conf-stat))))
	      ;; Print a row describing the membership of MEMBER-CONF-STAT 
	      ;; (described by MEMBERSHIP) in CONF-STAT.
	      (if (or (null member-conf-stat)
		      (null membership))
		  (lyskom-insert-string 'secret-membership)
		(lyskom-insert 
		 (format "%17s"
			 (lyskom-return-date-and-time
			  (membership->last-time-read membership))))
		(let ((unread (- (+ (conf-stat->first-local-no conf-stat)
				    (conf-stat->no-of-texts conf-stat))
				 (membership->last-text-read membership)
				 (length (membership->read-texts
					  membership))
				 1)))
		  (lyskom-format-insert 'conf-membership-line
					(if (zerop unread)
					    "         "
					  (format "%7d  " unread))
					member-conf-stat))))))))))


;;; ================================================================
;;;            Status (f|r) Person - status for a person

;;; Author: ceder
;;; Heavily enhanced: Inge Wallin (lyskom-status-pers-3 and beyond)


(def-kom-command kom-status-person (&optional pers-no)
  "Prints status for a person."
  (interactive)
  (let ((pers-no
         (or pers-no
             (lyskom-read-conf-no (lyskom-get-string 'pers-for-status)
                                  'pers "")))
	conf-stat
	pers-stat)
    (cache-del-conf-stat pers-no)
    (cache-del-pers-stat pers-no)
    (setq pers-stat (blocking-do 'get-pers-stat pers-no))
    (setq conf-stat (blocking-do 'get-conf-stat pers-no))

    ;; "Print status about PERS-STAT. The name is in CONF-STAT"
    (if (or (null pers-stat)
	    (null conf-stat))
	(lyskom-insert-string 'no-such-pers)
      (lyskom-format-insert 'pers-status-record
			    conf-stat
			    conf-stat)
      (lyskom-format-insert 'created-time
			    (lyskom-return-date-and-time
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
			    (lyskom-return-date-and-time
			     (pers-stat->last-login pers-stat)))
      (lyskom-format-insert 'user-name
			    (pers-stat->username pers-stat))
      (lyskom-format-insert 'read-texts
			    (pers-stat->read-texts pers-stat))
      (if (= (pers-stat->pers-no pers-stat) lyskom-pers-no)
          (lyskom-format-insert 'marked-texts
                            (pers-stat->no-of-marks pers-stat)))
      (lyskom-format-insert 'time-for-last-letter
			    (lyskom-return-date-and-time
			     (conf-stat->last-written conf-stat)))

      (let ((superconf 
	     (or (blocking-do 'get-conf-stat
			      (conf-stat->super-conf conf-stat))
		 (conf-stat->super-conf conf-stat))))
	(lyskom-format-insert 'superconf
			      superconf
			      superconf
			      (lyskom-default-button 'conf
						     superconf)
			      (if (and
				   (lyskom-conf-stat-p superconf)
				   (> (length (conf-stat->name superconf))
				      (- (lyskom-window-width) 46)))
				  "\n"
				"")))
      (if (not (zerop (conf-stat->supervisor conf-stat)))
	  (let ((supervisor 
		 (or (blocking-do 'get-conf-stat
				  (conf-stat->supervisor conf-stat))
		     (conf-stat->supervisor conf-stat))))
	    (lyskom-format-insert 'supervisor
				  supervisor
				  supervisor
				  (lyskom-default-button 'conf
							 supervisor)
				  (if (and 
				       (lyskom-conf-stat-p supervisor)
				       (> (length (conf-stat->name
						   supervisor))
					  (- (lyskom-window-width) 46)))
				      "\n"
				    ""))))
      (lyskom-format-insert 'member-of-confs
			    (pers-stat->no-of-confs pers-stat))
      (lyskom-format-insert 'presentation
			    (conf-stat->presentation conf-stat))

      (if (not (zerop (conf-stat->msg-of-day conf-stat)))
	  (progn
	    (lyskom-format-insert 'has-motd conf-stat)
	    (lyskom-view-text (conf-stat->msg-of-day conf-stat))))

      ;; "Show all conferences CONF-STAT is a member of if the user so wishes."
      (lyskom-scroll)
      (if (null (lyskom-j-or-n-p (lyskom-get-string
				  'show-membership-list-also-q))) 
	  nil
	(let ((membership-list 
	       (blocking-do 'get-membership
			    (conf-stat->conf-no conf-stat)))
	      (lyskom-count-var 0))

	  ;; "Receive an array of memberships and print them using
	  ;; lyskom-status-pers-5.
	  (if (null membership-list)
	      (lyskom-format-insert 'not-allowed-see-confs conf-stat)
	    (lyskom-format-insert 'is-member-of conf-stat)
	    (lyskom-insert-string 'membership-list-header)
	    (setq lyskom-count-var 0)
	    (lyskom-traverse
		membership membership-list
	      (let ((cs (cache-get-conf-stat
			 (membership->conf-no membership))))
		(and cs
		     (lyskom-time-greater
		      (membership->last-time-read membership)
		      (conf-stat->last-written conf-stat))
		     (cache-del-conf-stat (membership->conf-no membership))))
		  
	      ;; "Print a row describing the membership of
	      ;; MEMBER-CONF-STAT
	      (let ((member-conf-stat
		     (blocking-do 'get-conf-stat 
				  (membership->conf-no membership))))
		(if (or (null member-conf-stat)
			(null membership))
		    (lyskom-insert-string 'secret-membership)
          (lyskom-insert 
           (format "%17s"
                   (lyskom-return-date-and-time
                    (membership->last-time-read membership))))
		  (let ((unread (- (+ (conf-stat->first-local-no
				       member-conf-stat)
				      (conf-stat->no-of-texts
				       member-conf-stat))
				   (membership->last-text-read membership)
				   (length (membership->read-texts
					    membership))
				   1)))
		    (lyskom-format-insert
		     'pers-membership-line
		     (if (zerop unread) "       " (format "%6d " unread))
		     (if (= (conf-stat->conf-no conf-stat)
			    (conf-stat->supervisor member-conf-stat))
			 (lyskom-get-string 'is-supervisor-mark)
		       "  ")
		     member-conf-stat)
		    (setq lyskom-count-var (+ lyskom-count-var unread)))))))

	  ;; "Print the total number of unread texts for the person CONF-STAT."
	  (lyskom-format-insert 'his-total-unread 
				conf-stat
				lyskom-count-var))))))



;;; ================================================================
;;;              Skicka meddelande - Send message

;;; Author: Inge Wallin
;;; Rewritten to use lyskom-read-conf-no by Linus Tolke
;;; Modified to use default recipient by David Byers


(def-kom-command kom-send-message (&optional who message)
  "Send a message to one of the users in KOM right now."
  (interactive)
  (lyskom-send-message 
   (or who
       (lyskom-read-conf-no
        (format (lyskom-get-string 'who-to-send-message-to)
                (lyskom-get-string 'everybody))
        'all t
        ;; Initial string:
        (cond
         ((eq kom-default-message-recipient 'everybody) nil)

         ((and (eq kom-default-message-recipient 'group)
               lyskom-last-group-message-recipient)
          (if (string-match "^19" emacs-version)
              (cons lyskom-last-group-message-recipient 0)))

	 ((or (and (eq kom-default-message-recipient 'group)
		   (null lyskom-last-group-message-recipient))
	      (and (eq kom-default-message-recipient 'sender)
		   lyskom-last-personal-message-sender))
          (if (string-match "^19" emacs-version)
              (cons lyskom-last-personal-message-sender 0)
            lyskom-last-personal-message-sender))

         (t 
	  (if lyskom-last-personal-message-sender
	      (if (string-match "^19" emacs-version)
		  (cons lyskom-last-personal-messsage-sender 0)
		lyskom-last-personal-message-sender)
	    "")))))
   message))
  

(def-kom-command kom-send-alarm (&optional message)
  "Send a message to all of the users in KOM right now."
  (interactive)
  (lyskom-send-message 0 message))


(defun lyskom-send-message (pers-no message)
  "Send a message to the person with the number CONF-NO.  CONF-NO == 0 
means send the message to everybody."
  (let* ((string (or message
                     (lyskom-read-string (lyskom-get-string 'message-prompt))))
         (reply (blocking-do 'send-message pers-no string))
         (to-conf-stat (if (zerop pers-no)
                           nil
                         (blocking-do 'get-conf-stat pers-no))))
    (if reply
        (lyskom-handle-as-personal-message
         (if to-conf-stat
             (lyskom-format 'message-sent-to-user
                            string to-conf-stat)
           (lyskom-format 'message-sent-to-all string))
         lyskom-pers-no
         lyskom-filter-outgoing-messages)
      (lyskom-format-insert-before-prompt 'message-nope 
                                          (or to-conf-stat
                                              (lyskom-get-string 'everybody))
                                          string)) ;+++ lyskom-errno
    ))



;;; ================================================================
;;;       Endast l{sa senaste - Set unread articles in a conf. 
;;;            (Skip or re-read articles).

;;; Author: Linus Tolke


(def-kom-command kom-set-unread (&optional arg conf-no)
  "Set number of unread articles in current conference."
  (interactive "P")
  (if (and (zerop lyskom-current-conf) (null conf-no))
      (lyskom-insert-string 'not-present-anywhere)
    (let ((conf-stat (blocking-do 'get-conf-stat (or conf-no 
                                                     lyskom-current-conf))))
      (if (null conf-stat)              ;+++ annan errorhantering
          (lyskom-insert "Error!\n")	;+++ Hrrrmmmmffff????
        (let* ((narg (prefix-numeric-value arg))
               (n (if (and arg
                           (<= 0 narg)
                           (<= narg (conf-stat->no-of-texts conf-stat)))
                      narg
                    (lyskom-read-num-range 
                     0 (conf-stat->no-of-texts conf-stat)
                     (lyskom-format 'only-last
                                    (conf-stat->no-of-texts conf-stat)
                                    (conf-stat->name conf-stat)))))
               (result (blocking-do 'set-unread
                                    (conf-stat->conf-no conf-stat) n)))
          (if (null result)
              (lyskom-insert-string 'only-error)
            (lyskom-refetch)))))))



;;; ================================================================
;;;                 Lista Nyheter - List News

;;; Author:   Linus Tolke
;;; Rehacked: Inge Wallin


(defvar lyskom-special-conf-name "\\`Inl.gg .t mig\\'"
  "Regexp to match conf names that are special.")

(def-kom-command kom-list-news (&optional num)
  "Print the number of unread articles to the user."
  (interactive "P")
  (lyskom-prefetch-all-confs)
  (let ((num-arg (cond
                  ((numberp num) num)
                  ((and (listp num)
                        (numberp (car num))) (car num))
                  (t nil)))
        (sum 0))
    (mapcar
     (function
      (lambda (info)
        (let ((un (length (cdr (read-info->text-list info))))
              (name (conf-stat->name (read-info->conf-stat info)))
              (conf-stat (read-info->conf-stat info)))
          (cond
           ((eq (read-info->type info) 'CONF)
            (if (or (not num-arg)
                    (>= (-- num-arg) 0))
                (lyskom-insert 
                 (if (and (boundp 'lyskom-special-conf-name)
                          (stringp lyskom-special-conf-name)
                          (string-match lyskom-special-conf-name name))
                     (if (/= un 1)
                         (lyskom-format 'you-have-unreads-special un conf-stat)
                       (lyskom-format 'you-have-an-unread-special conf-stat))
                   (if (/= un 1)
                       (lyskom-format 'you-have-unreads un conf-stat)
                     (lyskom-format 'you-have-an-unread conf-stat)))))
            (setq sum (+ sum un)))))))
     (read-list->all-entries lyskom-to-do-list))
    (if (= 0 sum)
        (lyskom-insert-string 'you-have-read-everything)
      (lyskom-insert 
       (if (/= sum 1)
           (lyskom-format 'total-unreads 
                          sum)
         (format (lyskom-get-string 'total-unread)))))))


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
    (lyskom-tell-server kom-mercial)
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


(def-kom-command kom-list-summary ()
  "List a summary of the unread in the current conf.
The summary contains the date, number of lines, author and subject of the text
on one line."
  (interactive)
  (if (read-list-isempty lyskom-reading-list)
      (lyskom-insert-string 'have-to-be-in-conf-with-unread)
	
    (lyskom-list-summary
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

;; This function is commented out untile we might implement marks in a
;; new way. But it works as it is.

;;(def-kom-command kom-list-marks (&optional mark)
;;  "List a summary of marked texts with mark MARK."
;;  (interactive (list (or (and current-prefix-arg
;;			      (prefix-numeric-value current-prefix-arg))
;;			 (lyskom-read-num-range
;;			  1 255
;;			  (lyskom-get-string 'what-mark-to-list)))))
;;  (let ((texts (delq nil
;;		     (mapcar (function
;;			      (lambda (x) (and (= (elt (cdr x) 1) mark)
;;					       (elt (cdr x) 0))))
;;			     (blocking-do 'get-marks)))))
;;    (lyskom-list-summary texts)
;;    (lyskom-format-insert 'you-have-marks (length texts) mark)))


(defun lyskom-list-summary (texts)
  "List a summary of the texts in TEXTS.
The summary contains the date, number of lines, author and subject of the text
on one line."
  (let ((time (blocking-do 'get-time)))

      ;; Start fetching all text-stats and text to list them.
      (lyskom-insert (format "%-8s%-6s%5s%s%s\n"
			     (lyskom-get-string 'Texts)
			     (lyskom-get-string 'Date)
			     (lyskom-get-string 'Lines)
			     (lyskom-fix-str (/ (- (lyskom-window-width) 21)
						3)
					     (lyskom-get-string 'Author))
			     (lyskom-get-string 'Subject)))
      (lyskom-traverse
	  text-no texts
	(let ((text-stat (blocking-do 'get-text-stat text-no))
	      (text (blocking-do 'get-text text-no))
	      ;; We could do som optimization here. 
	      ;; We really don't need the whole text.
	      )
	  (lyskom-print-summary-line text-stat text text-no 
				     (time->year time) (time->yday time))))))


(defun lyskom-print-summary-line (text-stat text text-no year day)
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
	   (subjlen (/ (* (- (lyskom-window-width) 21) 2) 3))
	   (author-name (lyskom-format "%#1:M" (text-stat->author text-stat))))
      (lyskom-format-insert 'summary-line
			    text-no
			    time
			    lines
			    (lyskom-default-button 'conf
						   (text-stat->author
						    text-stat))
			    (lyskom-fix-str namelen author-name)
			    (lyskom-default-button 'text
						   text-no)
			    (lyskom-fix-str subjlen subject))
			    
;;;      (lyskom-insert (lyskom-fix-str 7 (format "%d" text-no)))
;;;      (lyskom-insert time)
;;;      (lyskom-insert (format "%4d  " lines))
;;;      (lyskom-halt 'main)
;;;      ;;; +++ Should be a function that only takes number
;;;      (lyskom-queue-print-name-2 
;;;       (blocking-do 'get-conf-stat (text-stat->author text-stat))
;;;       (text-stat->author text-stat)
;;;       t namelen)
;;;
;;;      (lyskom-insert (concat "  "
;;;			     (lyskom-fix-str subjlen subject)
;;;			     "\n"))
)))




;;; ================================================================
;;;      kom-display-who-buffer - Visa vilka-listan

;;; Author: Linus Tolke


(def-kom-command kom-display-who-buffer ()
  "Make the who-buffer appear on the screen as a temp buffer."
  (interactive)
  (let ((win (selected-window))
	(who (display-buffer lyskom-who-info-buffer)))
    (unwind-protect
	(progn
	  (select-window who)
	  (if (numberp kom-who-buffer-size-when-displaying)
	      (enlarge-window (- kom-who-buffer-size-when-displaying 
				 (window-height who)))))
      (select-window win))))



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
     ((condition-case error
	  (eq old-buf (process-buffer lyskom-proc))
	(error nil)))
     ((condition-case error
	  (save-excursion
	    (set-buffer (process-buffer lyskom-proc))
	    (set-buffer lyskom-unparsed-buffer)
	    (eq old-buf (current-buffer)))
	(error nil))
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




;;; ================================================================
;;;      [ndra livsl{ngd - Set lifespan of texts in a conference

;;; Author: Inge Wallin


(def-kom-command kom-set-garb-nice ()
  "Set the garb-nice value for a conference."
  (interactive)
  (let ((conf-stat (lyskom-read-conf-stat
		    (lyskom-get-string 'conf-to-set-garb-nice-q)
		    'all)))
    (if (not conf-stat)
	(lyskom-insert-string 'somebody-deleted-that-conf)
      (let ((garb-nice (lyskom-read-number
			(lyskom-get-string 'new-garb-nice-q))))
	(lyskom-format-insert 'garb-nice-for-is
			      conf-stat
			      garb-nice)
	(if (not (blocking-do 'set-garb-nice
			      (conf-stat->conf-no conf-stat) 
			      garb-nice))
	    (lyskom-insert-string 'nope) ;+++lyskom-errno
	  (lyskom-insert-string 'done)
	  (cache-del-conf-stat (conf-stat->conf-no conf-stat)))))))


;;; ================================================================
;;;       S{tt till}tna f|rfattare - set-permitted-submitters

;;; Author: Linus Tolke

(def-kom-command kom-set-permitted-submitters ()
  "Set the permitted submitters of a conference."
  (interactive)
  (let ((conf-stat (lyskom-read-conf-stat 
		    (lyskom-get-string 'conf-to-set-permitted-submitters-q)
		    'all)))

    (if (not conf-stat)
	(lyskom-insert-string 'somebody-deleted-that-conf)
      (let ((new-conf (lyskom-read-conf-stat
		       (lyskom-format 'new-permitted-submitters-q
				      (conf-stat->name conf-stat))
		       'all 
		       'empty)))
	(if (eq new-conf nil)
	    (lyskom-format-insert 'permitted-submitters-removed-for-conf 
				  conf-stat)
	  (lyskom-format-insert 'submitters-conf-for-is
				conf-stat
				new-conf))
	(if (not (blocking-do 'set-permitted-submitters
			      (conf-stat->conf-no conf-stat) 
			      (if (eq new-conf nil) ;Allowing all to write there
				  0
				(conf-stat->conf-no new-conf))))
	    (lyskom-insert-string 'nope) ;+++ lyskom-errno
	  (lyskom-insert-string 'done)
	  (cache-del-conf-stat (conf-stat->conf-no conf-stat)))))))


;;; ================================================================
;;;             [ndra superm|te - Set super conference 

;;; Author: Inge Wallin


(def-kom-command kom-set-super-conf ()
  "Set the super conference for a conference."
  (interactive)
  (let ((conf-stat (lyskom-read-conf-stat 
		    (lyskom-get-string 'conf-to-set-super-conf-q)
		    'all)))
    (if (not conf-stat)
	(lyskom-insert-string 'somebody-deleted-that-conf)
      (let ((new-conf (lyskom-read-conf-stat
		       (lyskom-format 'new-super-conf-q
				      (conf-stat->name conf-stat))
		       'all)))
      
	;; Set the super conference for conf-stat to new-conf.
	(lyskom-format-insert 'super-conf-for-is
			      conf-stat
			      new-conf)
	(if (not (blocking-do 'set-super-conf 
			      (conf-stat->conf-no conf-stat) 
			      (conf-stat->conf-no new-conf)))
	    (lyskom-insert-string 'nope) ;+++ lyskom-errno
	  (lyskom-insert-string 'done)
	  (cache-del-conf-stat (conf-stat->conf-no conf-stat)))))))




;;; ================================================================
;;;                  St{ng av servern - Shutdown

;;; Author: Inge Wallin


(defun kom-shutdown-server ()
  "Shutdown the LysKOM server."
  (interactive)
  (lyskom-start-of-command 'kom-shutdown-server)
  (if (lyskom-ja-or-nej-p (lyskom-get-string 'really-shutdown))
      (progn
	(lyskom-insert-string 'closing-server)
	(initiate-shutdown 'main 'lyskom-handle-command-answer 0))
    (lyskom-end-of-command)))


;;; ================================================================
;;;       \verg} till adm.mod - Enable administrator capabilities
;;;     \verg} till normalmod - Disable administrator capabilities

;;; Author: Inge Wallin


(def-kom-command kom-enable-adm-caps ()
  "Enable the LysKOM adminstrator commands for the current user."
  (interactive)
  (lyskom-enable-adm-caps (blocking-do 'enable 255)
			  (lyskom-get-string 'administrator)
			  t))
  

(def-kom-command kom-disable-adm-caps ()
  "Disable the LysKOM adminstrator commands for the current user."
  (interactive)
  (lyskom-enable-adm-caps (blocking-do 'enable 0)
			  (lyskom-get-string 'no-longer-administrator)
			  nil))

(defun lyskom-enable-adm-caps (answer string is-administrator)
  "Tell the user if the call succeded."
  (if answer
      (progn
	(lyskom-format-insert 'you-are-now string)
	(setq lyskom-is-administrator is-administrator))
    (lyskom-insert-string 'nope)))	;+++ lyskom-errno


;;; ================================================================
;;;         S{tt loginmeddelande - Set message of the day

;;; Author: Inge Wallin


;; This function relies on lyskom-edit-text calling lyskom-end-of-command
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


(def-kom-command kom-remove-motd ()
  "Remove the message of the day for LysKOM."
  (interactive)
  (lyskom-insert-string 'removing-motd)
  (initiate-set-motd-of-lyskom 'background 'lyskom-set-motd-3
			       0 0))

;;; ================================================================
;;;                  Kasta ut - force logout

;;; Author: Inge Wallin


(def-kom-command kom-force-logout ()
  "Force another user to log out."
  (interactive)
  (let ((session (car-safe (lyskom-read-session-no
                            (lyskom-get-string 'who-to-throw-out)
                            nil nil t))))
    (if session
        (progn
          (lyskom-format-insert 'throwing-out session)
          (lyskom-report-command-answer
           (blocking-do 'disconnect session))))))


;;; ================================================================
;;;                  Skjut upp l{sning - postpone

;;; Author: Per Cederqvist


(def-kom-command kom-postpone (today)
  "Postpone the reading of all but the last TODAY articles in the
current conference to another session."
  (interactive (list
		(cond
		 ((null current-prefix-arg)
		  (lyskom-read-number
		   (lyskom-get-string 'postpone-prompt)
		   17))
		 (t (prefix-numeric-value current-prefix-arg)))))

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
    (read-list-delete-text nil lyskom-to-do-list))))



;;; ================================================================
;;;              S{tt l{sniv} - Sess session priority

;;; Author: David K}gedal

(def-kom-command kom-set-session-priority (priority)
  "Set the priority level of the current session.
This sets the variable kom-session-priority and refetches all
membership info."
  (interactive "P")
  (let ((pri (or priority
		 (lyskom-read-number (lyskom-get-string
				      'set-session-priority)
				     100))))
    (setq lyskom-session-priority pri)
    (lyskom-refetch)))



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
    (while (and (string-match (regexp-quote session-name)
			      (buffer-name (current-buffer)))
		(not (eq buffer (current-buffer))))
      (bury-buffer))))


(defun kom-next-kom ()
  "Pop up the next lyskom-session."
  (interactive)
  (and (boundp 'lyskom-proc)
       lyskom-proc
       (processp lyskom-proc)
       (lyskom-tell-internat 'kom-tell-next-lyskom))
  (let ((buffers (buffer-list)))
    (while (and buffers
		(or (eq (car buffers) (current-buffer))
		    (not (save-excursion
			   (set-buffer (car buffers))
			   (and (boundp 'lyskom-proc)
				lyskom-proc
				(processp lyskom-proc)
				(memq (process-status lyskom-proc) '(run open))
				(eq (current-buffer)
				    (process-buffer lyskom-proc)))))))
      (setq buffers (cdr buffers)))
    (if buffers
	(progn
	  (kom-bury)
	  (switch-to-buffer (car buffers))))))

;;;============================================================
;;;  Visa user-arean                    (kom-show-user-area)
;;;
;;;  Author: David Byers

(defun kom-show-user-area ()
  "Get and display the user area of the current person"
  (interactive)
  (lyskom-start-of-command 'kom-show-user-area)
  (let ((pers-stat (blocking-do 'get-pers-stat lyskom-pers-no)))
    (lyskom-view-text (pers-stat->user-area pers-stat)
		      nil nil nil nil nil)
    (lyskom-run 'main 'lyskom-end-of-command)))


;;;============================================================
;;;   Ändra mötestyp                    (kom-change-conf-type)
;;;
;;;   Author: Tomas Abrahamsson & David Byers

(def-kom-command kom-change-conf-type ()
  "Change type of a conference"
  (interactive)
  (let* ((conf-no (lyskom-read-conf-no
		   (lyskom-get-string 'what-conf-to-change)
		   'confs nil ""))
	 (open (j-or-n-p (lyskom-get-string 'anyone-member)))
	 (secret (if (not open)
		     (j-or-n-p (lyskom-get-string 'secret-conf))))
	 (orig (j-or-n-p (lyskom-get-string 'comments-allowed))))
    (cache-del-conf-stat conf-no)
    (if (not (blocking-do 'set-conf-type
	       conf-no
	       (lyskom-create-conf-type (not open)
					(not orig)
					secret
					nil)))
	(progn (lyskom-insert-string 'nope)
	       (lyskom-format-insert 'error-code
				     (lyskom-get-error-text lyskom-errno)
				     lyskom-errno)))))


