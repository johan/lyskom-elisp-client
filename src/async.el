;;;;;
;;;;; $Id: async.el,v 36.3 1993-05-05 03:10:31 linus Exp $
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
	      "$Id: async.el,v 36.3 1993-05-05 03:10:31 linus Exp $\n"))


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
;nej:	  (if (not (zerop lyskom-pers-no))
;nej:	      (initiate-who-is-on 'who-buffer 'cache-set-who-info-list))))
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
	  (if (= conf-no lyskom-pers-no)
	      (lyskom-insert-before-prompt 
	       (lyskom-format 'you-changed-name-to new-name)))
	  (cache-del-conf-stat conf-no) ;+++Borde {ndra i cachen i st{llet.
	  (cond
	   ((lyskom-is-in-minibuffer))
	   (kom-presence-messages
	    (lyskom-message "%s" (lyskom-format 'name-has-changed-to-name
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
; This fetches the conf-stats once to much.
;	    (if (and (/= (who-info->pers-no info) 0)
;		     (/= (who-info->pers-no info) lyskom-pers-no))
;					;Don't show myself.
;		(initiate-get-conf-stat 'follow
;					'lyskom-show-changed-person
;					(who-info->pers-no info)
;					(who-info->working-conf info)
;					(who-info->doing-what info)))
;	    (if (/= (who-info->working-conf info) 0)
;		(initiate-get-conf-stat 'void nil
;					(who-info->working-conf info)))
	    (cache-add-who-info info)))))

     ((eq msg-no 7)			; Database is syncing.
      (lyskom-save-excursion
	(set-buffer buffer)
	(if (and (not (lyskom-is-in-minibuffer))
		 kom-presence-messages)
	    (lyskom-message "%s" (lyskom-get-string 'database-sync)))
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
				      'lyskom-show-logged-in-person
				      pers-no))
	  (if (and (not (zerop lyskom-pers-no))
		   lyskom-who-info-buffer-is-on)
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
	 (cond
	  ((string= message "	")
	   (initiate-send-message 'follow nil sender
				  (format "emacs-version: %s\nclient-version: %s"
					  (emacs-version)
					  lyskom-clientversion)))
	  (t (initiate-get-conf-stat 'async
				     'lyskom-show-personal-message sender
				     recipient
				     message))))))

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
	      (lyskom-run 'who-buffer 'cache-del-who-info session-no)))))

     (t
      (lyskom-skip-tokens tokens)))))


(defun lyskom-show-logged-in-person (conf-stat)
  "Visa p} kommandoraden vem som loggat in."
  (cond
   ((lyskom-is-in-minibuffer))
   (kom-presence-messages
    (lyskom-message "%s" (lyskom-format 'has-entered
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
    (lyskom-message "%s" (lyskom-format 'has-left
					(conf-stat->name conf-stat)))))
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
  (lyskom-insert-personal-message sender recipient message)
  (run-hooks 'lyskom-personal-message-hook))


(defun lyskom-insert-personal-message (sender recipient message)
  "Insert a personal message in the current buffer.
Arguments: SENDER RECIPIENT MESSAGE.
SENDER is a conf-stat (possibly nil) or a string.
RECIPIENT is 0 if the message is public, otherwise the pers-no of the user.
MESSAGE is a string containing the message.
INSERT-FUNCTION is a function that given a string inserts it into the
current buffer."
  (lyskom-handle-as-personal-message
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
		    (substring (current-time-string) 11 19)))
   (conf-stat->conf-no sender)))

  
(defun lyskom-handle-as-personal-message (string from)
  "Insert STRING as a personal message and beep if not from me and supposed to.
The buffer, is chosen according to the kom-show-personal-messages-in-buffer
variable value.
The text is converted, before insertion."
  (lyskom-save-excursion
   (cond
    ((eq kom-show-personal-messages-in-buffer t)
     (lyskom-insert-before-prompt string))
    ((null kom-show-personal-messages-in-buffer))
    (t
     (set-buffer (get-buffer-create kom-show-personal-messages-in-buffer))
     (goto-char (point-max))
     (insert (if kom-emacs-knows-iso-8859-1
		 string
	       (iso-8859-1-to-swascii string)))))
   (if kom-pop-personal-messages
       (display-buffer (current-buffer))))
  (if (and kom-ding-on-personal-messages
	   (/= lyskom-pers-no from))
      (beep)))
  

;;; ================================================================
;;;            Functions for dealing with a new text


(defun lyskom-default-new-text-hook (text-stat)
  "Print a message if the user was waiting. Change the prompt. run hooks."
  (if (and (not lyskom-dont-change-prompt) ;We shall change it
	   (not lyskom-executing-command)) ;We have time to do it.
      ;; Alter the prompt.
      (let ((buffer-read-only nil))
	(lyskom-save-excursion
	 (goto-char (point-max))
	 (beginning-of-line)
	 (delete-region (point) (point-max)))
	(lyskom-run 'async 'lyskom-print-prompt)))

  (let ((no-message nil))
    (run-hooks 'lyskom-new-text-hook)
  
    (if (and (not no-message)
	     lyskom-is-waiting
	     (not (lyskom-is-in-minibuffer)))
	(lyskom-message "%s" (lyskom-format 'text-is-created
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

  ;; Give a message if the user is waiting. Update the prompt.
  (lyskom-run 'async 'lyskom-default-new-text-hook text-stat))


(defun lyskom-conf-fetched-p (conf-no)
  "Return t if CONF-NO has been prefetched."
  (let ((n (1- (length lyskom-membership)))
	(result nil))
    (while (and (not result)
		(>= n 0))
      (if (= (membership->conf-no (elt lyskom-membership n))
	     conf-no)
	  (setq result t))
      (-- n))
    result))


(defun lyskom-add-new-text (recipient text-no local-no)
  "RECIPIENT is a conf-stat and recipient of TEXT-NO.
Args: RECIPIENT TEXT-NO LOCAL-NO.
LOCAL-NO is the texts local number in RECIPIENT. This info is used
to update the no-of-texts field in the cache.

Also add this info in lyskom-to-do-list if info about RECIPIENT as been
fetched. Does not try to print prompt or do any prefetch. That will be
done after all the confs has been handled.

If recipient is nil this means we are crossposting to a protected conference.
In that case, just discard this call."

  (cond
   (recipient				;+++ Annan felhantering.
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

    (lyskom-set-mode-line))))
