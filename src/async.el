;;;;;
;;;;; $Id: async.el,v 35.6 1991-09-28 19:34:38 ceder Exp $
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
;;;; File: async.el
;;;;
;;;; These functions implement a nice service that give the user
;;;; continuous messages about what other users are doing and what
;;;; is happening inside the lyskom server.
;;;;
;;;; Author: Linus Tolke
;;;; Entry:  Inge Wallin
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: async.el,v 35.6 1991-09-28 19:34:38 ceder Exp $\n"))


(defun lyskom-parse-async (tokens buffer)
  "Parse an asynchronous message from the server.
The message consists of TOKENS tokens. Unknown messages are skipped.
Actions are taken to perform the various tasks that is required on reciept of
an asynchronous message.
If variable kom-presence-messages is non-nil or some minibuffer editing is 
going on then nothing is printed on the message area.
This function is called with the lyskom-unparsed-buffer as current-buffer.
All calls using the lyskom-variables have to be made using the buffer BUFFER.

Be careful when editing this. All parsing is done with the buffer this
function is called with as the current-buffer, while all calls from
this function shall be with current-buffer the BUFFER."
  (let ((msg-no (lyskom-parse-num)))
    (cond
     ((eq msg-no 0)			; New text
      (let* ((text-no (lyskom-parse-num))
	     (text-stat (lyskom-parse-text-stat text-no)))
	(lyskom-save-excursion
	  (set-buffer buffer)
	  (lyskom-async-new-text text-stat)))) ;

     ((eq msg-no 1)			; Logout (obsolete)
;      (let ((pno (lyskom-parse-num)))
;	(lyskom-save-excursion
;	  (set-buffer buffer)
;	  (if (and (not (zerop lyskom-pers-no))
;		   (or kom-presence-messages
;		       kom-presence-messages-in-buffer))
;	      (initiate-get-conf-stat 'follow 
;				      'lyskom-show-logged-out-person
;				      pno))
;	  (if (not (zerop lyskom-pers-no))
;	      (initiate-who-is-on 'who-buffer 'cache-set-who-info-list))))
      )

     ((eq msg-no 2)			; Login, obsolete.
      (lyskom-skip-tokens tokens))

     ((eq msg-no 3)			; Conference deleted
      (lyskom-skip-tokens tokens))

     ((eq msg-no 4)			; Conference created
      (lyskom-skip-tokens tokens))

     ((eq msg-no 5)             ; A person or conference has changed name.
      (let ((conf-no (lyskom-parse-num))
	    (old-name (lyskom-parse-string))
	    (new-name (lyskom-parse-string)))
	(lyskom-save-excursion
	  (set-buffer buffer)
	  (cache-del-conf-stat conf-no) ;+++Borde {ndra i cachen i st{llet.
	  (cond
	   ((lyskom-is-in-minibuffer))
	   (kom-presence-messages
	    (lyskom-message (lyskom-format 'name-has-changed-to-name
					   old-name new-name))))
	  (cond
	   (kom-presence-messages-in-buffer
	    (lyskom-insert-before-prompt
	     (lyskom-format 'name-has-changed-to-name-r old-name new-name )))))))

     ((eq msg-no 6)			;i_am_on - something is moving
      (let ((info (lyskom-parse-who-info)))
	(lyskom-save-excursion
	  (set-buffer buffer)
	  (if (zerop lyskom-pers-no)
	      nil
	    (if (and (/= (who-info->pers-no info) 0)
		     (/= (who-info->pers-no info) lyskom-pers-no))
					;Don't show myself.
		(initiate-get-conf-stat 'follow
					'lyskom-show-changed-person
					(who-info->pers-no info)
					(who-info->working-conf info)
					(who-info->doing-what info)))
	    (if (/= (who-info->working-conf info) 0)
		(initiate-get-conf-stat 'void nil
					(who-info->working-conf info)))
	    (cache-add-who-info info)))))

     ((eq msg-no 7)			; Database is syncing.
      (lyskom-save-excursion
	(set-buffer buffer)
	(if (and (not (lyskom-is-in-minibuffer))
		 kom-presence-messages)
	    (lyskom-message (lyskom-get-string 'database-sync)))
	(setq mode-line-process ": saving")
	(set-buffer-modified-p (buffer-modified-p))
	(sit-for 0)
	(if (not lyskom-pending-calls)
	    (initiate-get-time 'async nil))))

     ((eq msg-no 8)			; Forced leave conference
      (lyskom-skip-tokens tokens))

     ((eq msg-no 9)			; A person has logged in
      (let ((pers-no (lyskom-parse-num))
	    (session-no (lyskom-parse-num)))
	(lyskom-save-excursion
	  (set-buffer buffer)
	  (if (and (not (zerop lyskom-pers-no))
		   (/= pers-no lyskom-pers-no))
					; Don't show myself.
	      (initiate-get-conf-stat 'follow
				      (if kom-presence-messages
					  'lyskom-show-logged-in-person
					nil)
				      pers-no))
;	  (if (not (zerop lyskom-pers-no))
;	      (initiate-who-is-on 'who-buffer 'cache-set-who-info-list))
	  (if (not (zerop lyskom-pers-no))
	      (initiate-get-session-info 'who-buffer 'cache-add-session-info
					 session-no))
	  )))

; msg-no 10 is the old broadcast message. No longer used.

     ((eq msg-no 11)
      (lyskom-save-excursion
       (set-buffer buffer)
       (lyskom-insert-before-prompt (lyskom-get-string 'lyskom-is-full))))

     ((eq msg-no 12)			; Message to the user (or everybody)
      (let ((recipient (lyskom-parse-num))
	    (sender (lyskom-parse-num))
	    (message (lyskom-parse-string)))
	(lyskom-save-excursion
	 (set-buffer buffer)
	 (initiate-get-conf-stat 'follow
				 'lyskom-show-personal-message sender
				 recipient
				 message))))

     ((eq msg-no 13)			; New logout
      (let ((pers-no (lyskom-parse-num))
	    (session-no (lyskom-parse-num)))
	(lyskom-save-excursion
	  (set-buffer buffer)
	  (if (and (not (zerop lyskom-pers-no))
		   (or kom-presence-messages
		       kom-presence-messages-in-buffer))
	      (initiate-get-conf-stat 'follow 
				      'lyskom-show-logged-out-person
				      pers-no
				      session-no))
	  (if (not (zerop lyskom-pers-no))
	      (cache-del-who-info session-no)))))

     (t
      (lyskom-skip-tokens tokens)))))


(defun lyskom-show-logged-in-person (conf-stat)
  "Visa p} kommandoraden vem som loggat in."
  (cond
   ((lyskom-is-in-minibuffer))
   (kom-presence-messages
    (lyskom-message (lyskom-format 'has-entered
			    (conf-stat->name conf-stat)))))
  (cond
   (kom-presence-messages-in-buffer
    (lyskom-insert-before-prompt
     (lyskom-format 'has-entered-r
		    (conf-stat->name conf-stat))))))


(defun lyskom-show-logged-out-person (conf-stat session-no)
  "Visa p} kommandoraden vem som loggat ut."
  (cond
   ((lyskom-is-in-minibuffer))
   (kom-presence-messages
    (lyskom-message (lyskom-format 'has-left (conf-stat->name conf-stat)))))
  (cond
   (kom-presence-messages-in-buffer
    (lyskom-insert-before-prompt
     (lyskom-format 'has-left-r
	     (conf-stat->name conf-stat))))))


(defun lyskom-show-changed-person (personconfstat conf-num doing)
  "Tells the user what another person is doing."
  (if personconfstat			;+++ Annan felhantering
      (progn
	(cond
	 ((lyskom-is-in-minibuffer))
	 ((and kom-presence-messages
	       (or (= 0 conf-num)
		   (eq conf-num lyskom-current-conf))
	       (/= 0 (length doing)))
	  (lyskom-message "%s %s" (conf-stat->name personconfstat) 
		   (let ((string
			  (concat (char-to-string (downcase
						   (string-to-char doing)))
				  (substring doing 1))))
		     (if kom-emacs-knows-iso-8859-1
			 string
		       (iso-8859-1-to-swascii string))))))
	(cond
	 ((and (eq kom-presence-messages-in-buffer t)
	       (or (= 0 conf-num)
		   (eq conf-num lyskom-current-conf))
	       (/= 0 (length doing)))
	  (lyskom-insert-before-prompt
	   (concat (conf-stat->name personconfstat)
		   " "
		   (concat (char-to-string (downcase (string-to-char doing)))
			   (substring doing 1)) 
		   "\n")))))))


(defun lyskom-is-in-minibuffer ()
  "Returns non-nil if I am using the minibuffer for some reading."
  (not (zerop (minibuffer-depth))))


(defun lyskom-show-personal-message (sender recipient message)
  "Insert a personal message into the lyskom buffer.
Args: SENDER: conf-stat for the person issuing the broadcast message or a
	      string that is the sender.
      RECIPIENT: 0 if this message is for everybody, otherwise the pers-no 
                 of the user.
      MESSAGE: A string containing the message."
  (lyskom-insert-before-prompt
   "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  (lyskom-insert-before-prompt 
   (if (= recipient 0)
       (lyskom-format 'message-broadcast
		      (cond
		       ((stringp sender) sender)
		       (sender (conf-stat->name sender))
		       (t (lyskom-get-string 'unknown)))
		      message
		      (substring (current-time-string) 11 19))
     (lyskom-format 'message-from
		      (cond
		       ((stringp sender) sender)
		       (sender (conf-stat->name sender))
		       (t (lyskom-get-string 'unknown)))
		      message
		      (substring (current-time-string) 11 19))))
  (lyskom-insert-before-prompt
   "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  (beep))


;;; ================================================================
;;;            Functions for dealing with a new text


(defun lyskom-default-new-text-hook (text-stat)
  "Print a message if the user was waiting. Change the prompt. run hooks."
  (if (and (not lyskom-no-prompt)	           ;There is a prompt
	   (not lyskom-dont-change-prompt) 	   ;We shall change it
	   (not lyskom-executing-command)) 	   ;We have time to do it.
      ;; Alter the prompt.
      (let ((buffer-read-only nil))
	(lyskom-save-excursion
	  (goto-char (point-max))
	  (beginning-of-line)
	  (delete-region (point) (point-max)))
	(setq lyskom-no-prompt t)))

  (let ((no-message nil))
    (run-hooks 'lyskom-new-text-hook)
  
    (if (and (not no-message)
	     lyskom-is-waiting
	     (not (lyskom-is-in-minibuffer)))
	(lyskom-message (lyskom-format 'text-is-created
				(text-stat->text-no text-stat))))))

  
(defun lyskom-async-new-text (text-stat)
  "Take care of a message that a new text has been created."
  (cache-del-pers-stat (text-stat->author text-stat)) ;+++Borde {ndra i cachen i st{llet.
  
  (lyskom-traverse
   misc-info (text-stat->misc-info-list text-stat)
   (let ((type (misc-info->type misc-info)))
     (cond
      ((or (eq type 'RECPT)
	   (eq type 'CC-RECPT))
       ;; add on lyskom-reading-list and lyskom-to-do-list if
       ;; this recipient is a recipient that has been checked.
       (initiate-get-conf-stat 'async 'lyskom-add-new-text
			       (misc-info->recipient-no misc-info)
			       (text-stat->text-no text-stat)
			       (misc-info->local-no misc-info)))
      ((eq type 'COMM-TO)
       (cache-del-text-stat (misc-info->comm-to misc-info)))
      ((eq type 'FOOTN-TO)
       (cache-del-text-stat (misc-info->footn-to misc-info)))
      (t
       (signal 'lyskom-internal-error
	       (list 'lyskom-async-new-text
		     "Unexpected misc-info in new text "
		     type))))))

  ;; Give a message if the user is waiting.
  (lyskom-run 'async 'lyskom-default-new-text-hook text-stat)

  (lyskom-run 'async 'lyskom-prefetch-and-print-prompt))


(defun lyskom-add-new-text (recipient text-no local-no)
  "RECIPIENT is a conf-stat and recipient of TEXT-NO.
Args: RECIPIENT TEXT-NO LOCAL-NO.
LOCAL-NO is the texts local number in RECIPIENT. This info is used
to update the no-of-texts field in the cache.

Also add this info in lyskom-to-do-list if info about RECIPIENT as been
fetched. Does not try to print prompt or do any prefetch. That will be
done after all the confs has been handled."

  ;; Update the cache.

  (set-conf-stat->no-of-texts
   recipient
   (max (conf-stat->no-of-texts recipient)
	(+ local-no -1
	   (- (conf-stat->first-local-no recipient)))))

  ;; Update the read-lists.

  (if (and (lyskom-conf-fetched-p (conf-stat->conf-no recipient))
	   (not (read-list-enter-text text-no recipient lyskom-to-do-list)))
      ;; If we have already read all texts in the conference...
      (let ((info (lyskom-create-read-info
		   'CONF
		   recipient
		   (membership->priority
		    (lyskom-member-p (conf-stat->conf-no recipient)))
		   (lyskom-create-text-list (list text-no)))))
	(read-list-enter-read-info info lyskom-to-do-list)
	(if (= lyskom-current-conf (conf-stat->conf-no recipient))
	    (read-list-enter-first info lyskom-reading-list))))

  (lyskom-set-mode-line))
