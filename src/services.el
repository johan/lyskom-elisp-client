;;;;;
;;;;; $Id: services.el,v 38.8 1996-01-19 18:50:08 byers Exp $
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
;;;; This file contains functions for sending requests to the server
;;;; and parsing the result.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: services.el,v 38.8 1996-01-19 18:50:08 byers Exp $\n"))


;;; ================================================================
;;;                     Requests for services


(defun initiate-login (kom-queue handler pers-no password &rest data)
  "Log in on server.
Args: KOM-QUEUE HANDLER PERS-NO PASSWORD &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 0 pers-no password)))


(defun initiate-login-new (kom-queue handler pers-no password status 
				     &rest data)
  "Log in on server.
Args: KOM-QUEUE HANDLER PERS-NO PASSWORD STATUS &rest DATA.
Status is 0 for visible login and 1 for invisible login."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 62 pers-no
						       password status)))


(defun initiate-logout (kom-queue handler &rest data)
  "Log out from server.
Args: KOM-QUEUE HANDLER &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 1)))


(defun initiate-pepsi (kom-queue handler conf-no &rest data)
  "Change working conference.
Args: KOM-QUEUE HANDLER CONF-NO &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 2 conf-no)))

(defun initiate-change-name (kom-queue handler
				       conf-no new-name
				       &rest data)
  "Change the name of a conference.
Args: KOM-QUEUE HANDLER CONF-NO NEW-NAME &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 3 conf-no new-name)))


(defun initiate-change-what-i-am-doing (kom-queue handler what &rest data)
  "Tell server what you are doing.
Args: KOM-QUEUE HANDLER WHAT &rest DATA.
WHAT is a string."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 4 what)))
  

(defun initiate-create-person (kom-queue handler name password &rest data)
  "Create a new person.
Args: KOM-QUEUE HANDLER NAME PASSWORD &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
  (lyskom-send-packet kom-queue (lyskom-format-objects 5 name password)))

;;; Call 6 is get-person-stat-old, and is obsoleted by call 49.

(defun initiate-set-priv-bits (kom-queue handler pers-no priv-bits &rest data)
  "Set priv-bits of a person.
Args: KOM-QUEUE HANDLER PERS-NO PRIV-BITS &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 7 pers-no priv-bits)))


(defun initiate-set-passwd (kom-queue handler
				      pers-no old-pw new-pw
				      &rest data)
  "Set the password of a person.
Args: KOM-QUEUE HANDLER PERS-NO OLD-PW NEW-PW &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 8 pers-no old-pw new-pw)))


;;; 
;;; This function has a ridiculous name!  It ought to be called
;;; get-membership.  Unfortunately this name is already taken
;;; by another call.
;;;
(defun initiate-query-read-texts (kom-queue handler
					    pers-no conf-no &rest data)
  "Get a membership struct describing the membership of PERS-NO in CONF-NO.
Args: KOM-QUEUE HANDLER PERS-NO CONF-NO &rest DATA"
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-membership)
  (lyskom-send-packet kom-queue 
		      (lyskom-format-objects 9 pers-no conf-no)))


(defun initiate-create-conf (kom-queue handler
				       conf-name conf-type &rest data)
  "Add a member to a conference.
Args: KOM-QUEUE HANDLER CONF-NAME CONF-TYPE &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 10 conf-name conf-type)))


(defun initiate-delete-conf (kom-queue handler conf-no &rest data)
  "Delete a conference.
Args: KOM-QUEUE HANDLER CONF-NO &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 11 conf-no)))



(defun initiate-lookup-name (kom-queue handler name &rest data)
  "See what conferences match NAME.
Args: KOM-QUEUE HANDLER NAME &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data
	       'lyskom-parse-conf-list)
  (lyskom-send-packet kom-queue (lyskom-format-objects 12 name)))
					    

;;; Call 13 is get-conf-stat-old, which is obsoleted by 50.


(defun initiate-add-member (kom-queue handler
				      conf-no pers-no priority where
				      &rest data)
  "Add a member to a conference.
Args: KOM-QUEUE HANDLER CONF-NO PERS-NO PRIORITY WHERE &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 14 conf-no pers-no 
					        priority where)))


(defun initiate-sub-member (kom-queue handler
				      conf-no pers-no
				      &rest data)
  "Subtract a member from a conference.
Args: KOM-QUEUE HANDLER CONF-NO PERS-NO  &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		     (lyskom-format-objects 15 conf-no pers-no)))


(defun initiate-set-presentation (kom-queue handler
					    conf-no text-no
					    &rest data)
  "Set presentation of a conference.
Args: KOM-QUEUE HANDLER CONF-NO TEXT-NO &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 16 conf-no text-no)))


(defun initiate-set-conf-motd (kom-queue handler conf-no text-no &rest data)
  "Set motd of a conference.
Args: KOM-QUEUE HANDLER CONF-NO TEXT-NO &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 17 conf-no text-no)))


(defun initiate-set-user-area (kom-queue handler pers-no text-no &rest data)
  "Set user-area of a person.
Args: KOM-QUEUE HANDLER PERS-NO TEXT-NO &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 57 pers-no text-no)))


(defun initiate-set-supervisor (kom-queue handler conf-no admin
					  &rest data)
  "Set supervisor of a conference.
Args: KOM-QUEUE HANDLER CONF-NO ADMIN &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 18 conf-no admin)))


(defun initiate-set-permitted-submitters (kom-queue handler conf-no perm-sub
						    &rest data)
  "Set permitted submitters of a conference.
Args: KOM-QUEUE HANDLER CONF-NO PERM-SUB &rest DATA.
PERM-SUB is a conference number. All members in that conference might
write texts in CONF-NO. If PERM-SUB is zero everyone is allowed to
write texts in CONF-NO."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 19 conf-no perm-sub)))


(defun initiate-set-super-conf (kom-queue handler conf-no super-conf
					  &rest data)
  "Set superconference of a conference.
Args: KOM-QUEUE HANDLER CONF-NO SUPER-CONF &rest DATA.
Unauthorized attempts to write texts to CONF-NO will bounce to SUPER-CONF."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue 
		      (lyskom-format-objects 20 conf-no super-conf)))


(defun initiate-set-conf-type (kom-queue handler conf-no conf-type &rest data)
  "Set type of a conference.
Args: KOM-QUEUE HANDLER CONF-NO CONF-TYPE &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 21 conf-no conf-type)))


(defun initiate-set-garb-nice (kom-queue handler conf-no garb-nice &rest data)
  "Set garb-nice of a conference.
Args: KOM-QUEUE HANDLER CONF-NO GARB-NICE &rest DATA.
Texts in CONF-NO will live approximately GARB-NICE days."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 22 conf-no garb-nice)))


(defun initiate-get-marks (kom-queue handler &rest data)
  "Get all marked texts.
Args: KOM-QUEUE HANDLER &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-mark-list)
  (lyskom-send-packet kom-queue (lyskom-format-objects 23)))


(defun initiate-mark-text (kom-queue handler
				     text-no mark-type	
				     &rest data)
  "Mark a text.
Args: KOM-QUEUE HANDLER TEXT-NO MARK-TYPE &rest DATA.
MARK-TYPE is currently a number, but this should maybe be
changed (internally in the elisp-klient) to something similar to
a conf-type (with several bits that are 't' or 'nil' that is)."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 24 text-no mark-type)))


(defun initiate-get-text (kom-queue handler text-no &rest data)
  "Get text from LysKOM server.
Args: KOM-QUEUE HANDLER TEXT-NO &rest DATA."
  (let ((text (cache-get-text text-no)))
    (cond
     ((null text)			;Cached info?
      (lyskom-call kom-queue		;No, ask the server.
		   lyskom-ref-no
		   handler data
		   'lyskom-parse-text text-no)
      ;(princ text-no (get-buffer-create "text"))+++
      ;(terpri (get-buffer-create "text"))
      (lyskom-send-packet kom-queue (lyskom-format-objects 25 text-no
							   0 lyskom-max-int)))
     (t
	;Cached info. 
      (lyskom-call-add kom-queue 'PARSED text handler data)
      (lyskom-check-call kom-queue))))) ;This might call the handler.


(defun initiate-get-text-stat (kom-queue handler text-no &rest data)
  "Get text-stat from LysKOM server.
Args: KOM-QUEUE HANDLER TEXT-NO &rest DATA."
  (let ((text-stat (cache-get-text-stat text-no)))
    (cond
     ((null text-stat)			;Cached info?
      (lyskom-call kom-queue		;No, ask the server.
		   lyskom-ref-no
		   handler data
		   'lyskom-parse-text-stat text-no)
      ;(princ text-no (get-buffer-create "text-stat"))+++
      ;(terpri (get-buffer-create "text-stat"))
      (lyskom-send-packet kom-queue (lyskom-format-objects 26 text-no)))
     (t
	;Cached info. 
      (lyskom-call-add kom-queue 'PARSED text-stat handler data)
      (lyskom-check-call kom-queue))))) ;This might call the handler.


(defun initiate-mark-as-read (kom-queue handler conf-no text-list &rest data)
  "Mark all texts in TEXT-LIST as read in CONF-NO. Args: CONF-NO TEXT-LIST."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 27 conf-no 
					     (cons 'LIST text-list))))


(defun initiate-create-text (kom-queue handler message misc-list &rest data)
  "Create a new text.
Args: KOM-QUEUE HANDLER MESSAGE MISC-LIST &rest DATA.
MESSAGE is a string. MISC-LIST should be created by lyskom-create-misc-list."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 28 message misc-list)))

(defun initiate-create-anonymous-text (kom-queue handler message 
						 misc-list &rest data)
  "Create a new anonymous text.
Args: KOM-QUEUE HANDLER MESSAGE MISC-LIST &rest DATA.
MESSAGE is a string. MISC-LIST should be created by lyskom-create-misc-list."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 59 message misc-list)))
				     
(defun initiate-delete-text (kom-queue handler text-no &rest data)
  "Delete a text.
Args: KOM-QUEUE HANDLER TEXT-NO &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 29 text-no)))


(defun initiate-add-recipient (kom-queue handler
					 text-no conf-no type
					 &rest data)
  "Add a recipient to a text.
Args: KOM-QUEUE HANDLER TEXT-NO CONF-NO TYPE &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 30 text-no conf-no
				     (if (eq type 'recpt)
					 0
				       1))))

(defun initiate-sub-recipient (kom-queue handler
					 text-no conf-no 
					 &rest data)
  "Subtract a recipient from a text.
Args: KOM-QUEUE HANDLER TEXT-NO CONF-NO &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 31 text-no conf-no)))


(defun initiate-add-comment (kom-queue handler
					 comment-text-no text-no
					 &rest data)
  "Add a comment to a text.
Args: KOM-QUEUE HANDLER COMMENT-TEXT-NO TEXT-NO &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 32 comment-text-no text-no)))

				       

(defun initiate-sub-comment (kom-queue handler
					 comment-text-no text-no
					 &rest data)
  "Subtract a comment from a text.
Args: KOM-QUEUE HANDLER COMMENT-TEXT-NO TEXT-NO &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 33 comment-text-no text-no)))


(defun initiate-get-map (kom-queue handler conf-no first-local
				   no-of-texts &rest data-list)
  "Get mapping from local to global text-nos for CONF-NO from server.
Args: KOM-QUEUE HANDLER CONF-NO FIRST-LOCAL NO-OF-TEXTS DATA-LIST.
Use initiate-get-map instead. This function has severe performance losses
with big maps."
  (lyskom-call kom-queue lyskom-ref-no handler data-list
	       'lyskom-parse-map)
  (lyskom-send-packet kom-queue 
		      (lyskom-format-objects 34 conf-no
					     first-local no-of-texts)))

(defun z-initiate-get-map  (kom-queue handler conf-no first-local
					     no-of-texts &rest data)
  "Get mapping from local to global text-nos for CONF-NO from server.
Args: KOM-QUEUE HANDLER CONF-NO FIRST-LOCAL NO-OF-TEXTS &rest DATA.
This function will automatically split fetching of big maps to small
chunks of lyskom-fetch-map-nos texts/chunk if KOM-QUEUE is not already
used to collect a result. This currently gives a big performance gain.
Unfortunately it is impossible (or at least very hard) to do the same
thing when a collect is in progress. This will of course be fixed in
protocol B."
  (cond
   ((lyskom-kom-queue-collect-p kom-queue)
    ;; Use oldstyle single big map. Sorry.
    (initiate-get-map kom-queue handler conf-no
		      first-local no-of-texts data))
   (t
    ;; You win.
    (lyskom-collect-ignore-err kom-queue)
    (while (> no-of-texts 0)
      (initiate-get-map kom-queue nil conf-no
				  first-local lyskom-fetch-map-nos data)
      (setq first-local (+ lyskom-fetch-map-nos first-local))
      (setq no-of-texts (- no-of-texts lyskom-fetch-map-nos)))
    (lyskom-list-use kom-queue 'lyskom-receive-get-map handler data))))

(defun lyskom-receive-get-map (map-list handler data-list)
  "Receive a list of maps.
Args: MAP-LIST HANDLER DATA-LIST."
  (apply handler (apply 'lyskom-map-concat map-list) data-list))
    

(defun initiate-get-time (kom-queue handler &rest data)
  "Get time from server.
Args: KOM-QUEUE HANDLER &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data
	       'lyskom-parse-time)
  (lyskom-send-packet kom-queue (lyskom-format-objects 35)))


(defun initiate-get-server-info (kom-queue handler &rest data)
  "Get info about the server"
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-server-info)
  (lyskom-send-packet kom-queue (lyskom-format-objects 36)))


(defun initiate-add-footnote (kom-queue handler
					footnote-text-no text-no
					&rest data)
  "Add a footnote to a text.
Args: KOM-QUEUE HANDLER FOOTNOTE-TEXT-NO TEXT-NO &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 37 footnote-text-no text-no)))


(defun initiate-sub-footnote (kom-queue handler
					footnote-text-no text-no
					&rest data)
  "Subtract a footnote from a text.
Args: KOM-QUEUE HANDLER FOOTNOTE-TEXT-NO TEXT-NO &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 38 footnote-text-no text-no)))


;;; Call 39, who-is-on-old, is obsoleted by call 51.


(defun initiate-set-unread (kom-queue handler conf-no no-of-unread &rest data)
  "Set number of unread texts in a certain conference.
Args: KOM-QUEUE HANDLER CONF-NO NO-OF-UNREAD &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 40 conf-no no-of-unread)))


(defun initiate-set-motd-of-lyskom (kom-queue handler text-no &rest data)
  "Set message of the day of LysKOM.
Args: KOM-QUEUE HANDLER TEXT-NO &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 41 text-no)))


(defun initiate-enable (kom-queue handler level &rest data)
  "Set security level.
Args: KOM-QUEUE HANDLER LEVEL &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 42 level)))

;;; Call 43 is sync. That should not be done too often, so no
;;; function is written... Use 'kill -SIGUSR1' instead.

;;; Call 44 is shutdown. Use 'kill -HUP' instead.

(defun initiate-shutdown (kom-queue handler parameter &rest data)
  "Shutdown the server.
Args: KOM-QUEUE HANDLER PARAMETER &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 44 parameter)))


(defun initiate-broadcast (kom-queue handler message &rest data)
  "Send a broadcast message to all logged in users.
Args: KOM-QUEUE HANDLER MESSAGE &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 45 message)))



(defun initiate-get-membership (kom-queue handler pers-no &rest data)
  "Get membership-list for PERS-NO from server.
Args: KOM-QUEUE HANDLER PERS-NO &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data
	       'lyskom-parse-membership-list)
  (lyskom-send-packet kom-queue (lyskom-format-objects 46 pers-no
						0 lyskom-max-int ;all confs.
						1))) ;want read texts.


(defun initiate-get-part-of-membership (kom-queue handler pers-no first length
						  &rest data)
  "Get membership-list for PERS-NO from server.
Args: KOM-QUEUE HANDLER PERS-NO FIRST-IN-LIST LENGHT &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data
	       'lyskom-parse-membership-list)
  (lyskom-send-packet kom-queue (lyskom-format-objects 46 pers-no
						first length ;all confs.
						1)))


(defun initiate-get-created-texts (kom-queue handler pers-no first-local
					     no-of-texts &rest data)
  "Get a part of the list of created texts for a person.
Args: KOM-QUEUE HANDLER PERS-NO FIRST-LOCAL NO-OF-TEXTS &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data
	       'lyskom-parse-map)
  (lyskom-send-packet kom-queue (lyskom-format-objects 47 pers-no
					       first-local no-of-texts)))


(defun initiate-get-members (kom-queue handler conf-no first-local
				       no-of-members &rest data)
  "Get a part of the list of members in a conference.
Args: KOM-QUEUE HANDLER CONF-NO FIRST-LOCAL NO-OF-MEMBERS &rest DATA.
Returns a conf-no-list."
  (lyskom-call kom-queue lyskom-ref-no handler data
	       'lyskom-parse-conf-no-list)
  (lyskom-send-packet kom-queue
		      (lyskom-format-objects 48 conf-no
					     first-local no-of-members)))


(defun initiate-get-pers-stat (kom-queue handler pers-no &rest data)
  "Get status for person PERS-NO.
Args: KOM-QUEUE HANDLER PERS-NO &rest DATA."
  (let ((pers-stat (cache-get-pers-stat pers-no)))
    (cond
     ((null pers-stat)			;Cached info?
      (lyskom-call kom-queue		;No, ask the server.
		   lyskom-ref-no
		   handler data
		   'lyskom-parse-pers-stat pers-no)
      ;(princ pers-no (get-buffer-create "pers-stat")) +++
      ;(terpri (get-buffer-create "pers-stat"))
      (lyskom-send-packet kom-queue (lyskom-format-objects 49 pers-no)))
     (t
	;Cached info. 
      (lyskom-call-add kom-queue 'PARSED pers-stat handler data)
      (lyskom-check-call kom-queue)))))

(defun initiate-get-conf-stat (kom-queue handler conf-no &rest data)
  "Get conf-stat from LysKOM server.
Args: KOM-QUEUE HANDLER CONF-NO &rest DATA."
  (let ((conf-stat (cache-get-conf-stat conf-no)))
    (cond
     ((zerop conf-no)			;No real user.
      (lyskom-call-add kom-queue 'PARSED nil handler data)
      (lyskom-check-call kom-queue))
     ((null conf-stat)			;Cached info?
      (lyskom-call kom-queue		;No, ask the server.
		   lyskom-ref-no
		   handler data
		   'lyskom-parse-conf-stat conf-no)
      ;(princ conf-no (get-buffer-create "conf-stat")) +++
      ;(terpri (get-buffer-create "conf-stat"))
      (lyskom-send-packet kom-queue (lyskom-format-objects 50 conf-no)))
     (t
	;Cached info. 
      (lyskom-call-add kom-queue 'PARSED conf-stat handler data)
      (lyskom-check-call kom-queue))))) ;This might call the handler.


(defun initiate-get-uconf-stat (kom-queue handler conf-no &rest data)
  "Get an uconf-sstat from LysKOM server.
Args: KOM-QUEUE HANDLER CONF-NO &rest DATA."
  (let ((conf-stat (cache-get-uconf-stat conf-no)))
    (cond ((zerop conf-no)
	   (lyskom-call-add kom-queue 'PARSED nil handler data)
	   (lyskom-check-call kom-queue))
	  ((null conf-stat)
	   (lyskom-call kom-queue
			lyskom-ref-no
			handler data
			'lyskom-parse-uconf-stat conf-no)
	   (lyskom-send-packet kom-queue (lyskom-format-objects 78 conf-no)))
	  (t
	   (lyskom-call-add kom-queue 'PARSED conf-stat handler data)
	   (lyskom-check-call kom-queue)))))


(defun initiate-who-is-on (kom-queue handler &rest data)
  "Ask server who is on.
Args: KOM-QUEUE HANDLER &rest DATA"
  (lyskom-call kom-queue lyskom-ref-no handler data
	       'lyskom-parse-who-info-list)
  (lyskom-send-packet kom-queue (lyskom-format-objects 51)))


(defun initiate-get-unread-confs (kom-queue handler pers-no &rest data)
  "Return a list of confs that may have unread texts.
Args: KOM-QUEUE HANDLER PERS-NO &rest DATA.
PERS-NO is the number of the person whos confs we are checking."
  (lyskom-call kom-queue lyskom-ref-no handler data
	       'lyskom-parse-conf-no-list)
  (lyskom-send-packet kom-queue (lyskom-format-objects 52 pers-no)))


(defun initiate-send-message (kom-queue handler recipient message &rest data)
  "Send a message to one or all logged in users.
Args: KOM-QUEUE HANDLER RECIPIENT MESSAGE &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 53 recipient message)))


(defun initiate-get-session-info (kom-queue handler session-no &rest data)
  "Ask server about info about a session.
Args: KOM-QUEUE HANDLER SESSION-NO &rest DATA"
  (lyskom-call kom-queue lyskom-ref-no handler data
	       'lyskom-parse-session-info)
  (lyskom-send-packet kom-queue (lyskom-format-objects 54 session-no)))


(defun initiate-disconnect (kom-queue handler session-no &rest data)
  "Disconnect a session.
Args: KOM-QUEUE HANDLER SESSION &rest DATA"
  (lyskom-call kom-queue lyskom-ref-no handler data
	       'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 55 session-no)))


(defun initiate-who-am-i (kom-queue handler &rest data)
  "Ask the server which connection we are using.
Args: KOM-QUEUE HANDLER &rest DATA."
  (lyskom-call kom-queue lyskom-ref-no handler data
	       'lyskom-parse-num)
  (lyskom-send-packet kom-queue (lyskom-format-objects 56)))

(defun initiate-set-last-read (kom-queue handler conf-no text-no &rest data)
  "Tell the server to set the highest unread article in conference CONF-NO
to TEXT-NO
Args: KOM-QUEUE HANDLER CONF-NO TEXT-NO &rest DATA"
  (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
  (lyskom-send-packet kom-queue (lyskom-format-objects 77 conf-no text-no)))



;;; ================================================================


;; Blocking reading from server:

(defvar lyskom-blocking-return nil
  "Return from blocking-do.")

(defvar lyskom-blocking-process nil
  "The process the where the lyskom-session is.
If blocking-do is called from a buffer that is no well-connected to the 
lyskom-session, i.e. the lyskom-proc variable is not set, then this variable
has to be set for the blocking-do to be able to know what process to send
questions to. This is the case when called from the minibuffer when 
completing.")

(defun blocking-return (retval)
  "Sets blocking variable."
  (setq lyskom-blocking-return retval))

(defun blocking-do (command &rest data)
  "Does the COMMAND agains the lyskom-server and returns the result.
COMMAND is one lyskom-command (like the initiate-* but the initiate- is 
stripped.
The cache is consulted when command is get-conf-stat, get-pers-stat
or get-text-stat."
  (save-excursion
    (set-buffer (process-buffer (or lyskom-proc
				    lyskom-blocking-process)))
    (let ((lyskom-blocking-return 'not-yet-gotten))
      (apply (intern-soft (concat "initiate-"
				  (symbol-name command)))
	     'blocking 'blocking-return
	     data)
      (while (and (eq lyskom-blocking-return 'not-yet-gotten)
		  (not lyskom-quit-flag))
	(accept-process-output))
      (if lyskom-quit-flag (error (lyskom-get-string 'interrupted)))
      (setq lyskom-quit-flag nil)
      lyskom-blocking-return)))


