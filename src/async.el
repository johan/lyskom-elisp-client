;;;;;
;;;;; $Id: async.el,v 44.70 2010-05-13 18:14:09 byers Exp $
;;;;; Copyright (C) 1991-2002  Lysator Academic Computer Association.
;;;;;
;;;;; This file is part of the LysKOM Emacs LISP client.
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
	      "$Id: async.el,v 44.70 2010-05-13 18:14:09 byers Exp $\n"))


(defun lyskom-is-ignoring-async (buffer message &rest args)
  (save-current-buffer
    (set-buffer buffer)
    (let ((tmp (assq message lyskom-ignoring-async-list)))
      (and tmp (equal args (cdr tmp))))))

(defun lyskom-parse-async (tokens buffer)
  "Parse an asynchronous message from the server.
The message consists of TOKENS tokens. Unknown messages are skipped.
Actions are taken to perform the various tasks that is required on reciept of
an asynchronous message.

If variable `kom-presence-messages-in-echo-area' is non-nil or some
minibuffer editing is going on then nothing is printed on the message
area. This function is called with the lyskom-unparsed-buffer as
current-buffer. All calls using the lyskom-variables have to be made
using the buffer BUFFER.

Be careful when editing this. All parsing is done with the buffer this
function is called with as the current-buffer, while all calls from
this function shall be with current-buffer the BUFFER."
  (let ((msg-no (lyskom-parse-num)))
    (cond
     ((or (eq msg-no 0)
          (eq msg-no 15)) ; New text
      (let* ((text-no (lyskom-parse-num))
	     (text-stat (if (eq msg-no 0)
                            (lyskom-parse-text-stat-old text-no)
                          (lyskom-parse-text-stat text-no))))
	(lyskom-save-excursion
	 (set-buffer buffer)
	 (lyskom-async-new-text text-stat)))) ;

     ((eq msg-no 1)			; Logout (obsolete)
      (lyskom-skip-tokens tokens))

     ((eq msg-no 2)			; Login, obsolete.
      (lyskom-skip-tokens tokens))

     ((eq msg-no 3)			; Conference deleted
      (lyskom-skip-tokens tokens))

     ((eq msg-no 4)			; Conference created
      (lyskom-skip-tokens tokens))

     ((eq msg-no 5)			; A person or conference has
					; changed name.
      (let ((conf-no (lyskom-parse-num))
	    (old-name (lyskom-parse-string))
	    (new-name (lyskom-parse-string)))
	(lyskom-save-excursion
	 (set-buffer buffer)
	 (if (and lyskom-pers-no (= conf-no lyskom-pers-no))
	     (lyskom-format-insert-before-prompt 
	      'you-changed-name-to
	      new-name
	      (lyskom-default-button 'conf conf-no)))
	 (let ((cached-stat (cache-get-conf-stat conf-no))
               (cached-ustat (cache-get-uconf-stat conf-no)))
	   (when cached-stat
             (set-conf-stat->name cached-stat new-name))
	   (when cached-ustat
             (set-uconf-stat->name cached-ustat new-name)))
	 (cond
	  ((lyskom-is-in-minibuffer))
          ((lyskom-show-presence conf-no kom-presence-messages-in-echo-area)
	   (lyskom-message "%s" (lyskom-format 'name-has-changed-to-name
					       old-name new-name))))
	 (cond
	  ((lyskom-show-presence conf-no kom-presence-messages-in-buffer)
	   (lyskom-format-insert-before-prompt
	    'name-has-changed-to-name-r 
	    old-name 
	    new-name
	    (lyskom-default-button 'conf conf-no)
	    (and kom-text-properties
		 (list 'face kom-presence-face)))))

         ;; Update in the mship-edit buffer

         (lp--maybe-update-entry-for-conf conf-no))))

     ((eq msg-no 6)			;i_am_on - something is moving
      (lyskom-parse-who-info))

     ((eq msg-no 7)			; Database is syncing.
      (lyskom-save-excursion
       (set-buffer buffer)
       ;; I removed the test for kom-presence-messages /david
       (if (and (not (lyskom-is-in-minibuffer))
                kom-show-sync-messages)
	   (lyskom-message "%s" (lyskom-get-string 'database-sync)))
       (setq mode-line-process (lyskom-get-string 'mode-line-saving))
       (setq lyskom-is-saving t)
       ;; I guess the following two lines could be replaced by
       ;; force-mode-line-update in a modern emacs.
       (set-buffer-modified-p (buffer-modified-p))
       (sit-for 0)
       (if (not lyskom-pending-calls)
	   (initiate-get-time 'async nil))))
     
     ((eq msg-no 8)			; Forced leave conference
      (let ((conf-no (lyskom-parse-num)))
        (unless (lyskom-is-ignoring-async buffer 8 conf-no)
          (lyskom-save-excursion
              (set-buffer buffer)
              (initiate-get-conf-stat 'follow
                                      'lyskom-async-forced-leave-conf
                                      conf-no
                                      conf-no)))))
     
     ((eq msg-no 9)			; A person has logged in
      (let ((pers-no (lyskom-parse-num))
	    (session-no (lyskom-parse-num)))
	(lyskom-save-excursion
	  (set-buffer buffer)
	  (if (and lyskom-pers-no
		   (not (zerop lyskom-pers-no)))
	      (let ((c (make-collector)))
		(initiate-get-conf-stat 'follow 'collector-push pers-no c)
		(initiate-get-static-session-info 'follow 'collector-push 
						  session-no c)
		(lyskom-run 'follow (lambda (c)
				      (lyskom-show-logged-in-person 
				       (elt (collector->value c) 1)
				       (elt (collector->value c) 0)))
			    c))))))

     ;; msg-no 10 is the old broadcast message. No longer used.
     
     ((eq msg-no 11)
      (lyskom-save-excursion
       (set-buffer buffer)
       (lyskom-insert-before-prompt (lyskom-get-string-sol 'lyskom-is-full))
       ))
     
     
     ((eq msg-no 12)			; Message to the user (or everybody)
      (let ((recipient (lyskom-parse-num))
	    (sender (lyskom-parse-num))
	    (message (lyskom-parse-string)))
	(lyskom-save-excursion
	 (set-buffer buffer)
	 (if (zerop recipient)
	     (initiate-get-conf-stat 'async
				     'lyskom-handle-personal-message
				     sender
				     0
				     message)
	   (lyskom-collect 'async)
	   (initiate-get-conf-stat 'async nil sender)
	   (initiate-get-conf-stat 'async nil recipient)
	   (lyskom-use 'async 'lyskom-handle-personal-message message)))))
     
     ((eq msg-no 13)			; New logout
      (let ((pers-no (lyskom-parse-num))
	    (session-no (lyskom-parse-num)))
	(lyskom-save-excursion
	 (set-buffer buffer)
	 (if (and lyskom-pers-no
		  (not (zerop lyskom-pers-no))
		  (or (lyskom-show-presence pers-no kom-presence-messages-in-echo-area)
		      (lyskom-show-presence pers-no kom-presence-messages-in-buffer)))
	     (initiate-get-conf-stat 'follow
				     'lyskom-show-logged-out-person
				     pers-no session-no)))))

     ((eq msg-no 14)                    ; Deleted text
      (let* ((text-no (lyskom-parse-num))
            (text-stat (lyskom-parse-text-stat text-no)))
        (lyskom-save-excursion
         (set-buffer buffer)
         (lyskom-async-deleted-text text-stat))))

     ((eq msg-no 16)                    ; New recipient
      (let ((text-no (lyskom-parse-num))
            (conf-no (lyskom-parse-num))
            (misc-type (lyskom-parse-num)))
        (cond ((eq misc-type 0) (setq misc-type 'RECPT))
              ((eq misc-type 1) (setq misc-type 'CC-RECPT))
              ((eq misc-type 15) (setq misc-type 'BCC-RECPT)))
        (when (symbolp misc-type)
          (lyskom-save-excursion
            (set-buffer buffer)
            (cache-del-text-stat text-no)
            (cache-del-conf-stat conf-no)
            (initiate-get-text-stat 'follow
                                    'lyskom-async-new-recipient
                                    text-no
                                    text-no conf-no misc-type)
            ))))

     ((eq msg-no 17)                    ; Deleted recipient
      (let ((text-no (lyskom-parse-num))
            (conf-no (lyskom-parse-num))
            (misc-type (lyskom-parse-num)))
        (lyskom-save-excursion
         (set-buffer buffer)
          (cache-del-conf-stat conf-no)
          (cache-del-text-stat text-no)

	  (let ((membership (lyskom-try-get-membership conf-no)))
	    (when (and membership
		       (lyskom-visible-membership membership))
	      (read-list-delete-text text-no lyskom-to-do-list)
	      (read-list-delete-text text-no lyskom-reading-list)))
          (lyskom-ignore misc-type)
         )))

     ((eq msg-no 18)                    ; New membership
      (let ((pers-no (lyskom-parse-num))
            (conf-no (lyskom-parse-num)))
        (unless (lyskom-is-ignoring-async buffer 18 pers-no conf-no)
          (lyskom-save-excursion
              (set-buffer buffer)
              (cache-del-pers-stat pers-no)
              (cache-del-conf-stat conf-no)
              (when (eq pers-no lyskom-pers-no)
                (lyskom-collect 'follow)
                (initiate-get-conf-stat 'follow nil conf-no)
                (initiate-query-read-texts 'follow nil pers-no conf-no t 0)
                (lyskom-use 'follow 'lyskom-async-new-membership pers-no conf-no))
            ))))

     ((eq msg-no 19)                    ; async-new-user-area
      (let* ((pers-no (lyskom-parse-num))
             (old-user-area (lyskom-parse-num))
             (new-user-area (lyskom-parse-num)))
        (lyskom-save-excursion
          (set-buffer buffer)
          (when (and (eq pers-no lyskom-pers-no)
                     (not (eq lyskom-current-user-area new-user-area)))
            (initiate-get-pers-stat 'follow 
                                    'lyskom-async-new-user-area
                                    pers-no
                                    old-user-area
                                    new-user-area)))))

     ((eq msg-no 20)                    ; async-new-presentation
      (let* ((conf-no (lyskom-parse-num))
             (old-pres (lyskom-parse-num))
             (new-pres (lyskom-parse-num)))
        (lyskom-ignore old-pres)
        (lyskom-save-excursion
          (set-buffer buffer)
          (when (cache-get-conf-stat conf-no)
            (set-conf-stat->presentation (cache-get-conf-stat conf-no)
                                         new-pres)
            (lp--maybe-update-entry-for-conf conf-no)))))

     ((eq msg-no 21)                    ; async-new-motd
      (let* ((conf-no (lyskom-parse-num))
             (old-motd (lyskom-parse-num))
             (new-motd (lyskom-parse-num)))
        (lyskom-save-excursion
          (set-buffer buffer)
          (lyskom-ignore old-motd)
          (when (cache-get-conf-stat conf-no)
            (set-conf-stat->msg-of-day (cache-get-conf-stat conf-no)
                                       new-motd)))))

     ((eq msg-no 22)                    ; async-text-aux-changes
      (let* ((text-no (lyskom-parse-num))
             (deleted-aux (lyskom-parse-aux-item-list))
             (added-aux (lyskom-parse-aux-item-list))
             (text-stat nil))
        (lyskom-save-excursion
          (set-buffer buffer)
          (setq text-stat (cache-get-text-stat text-no))
          (when text-stat
            (set-text-stat->aux-items text-stat
                                      (lyskom-aux-item-modify-list
                                       (text-stat->aux-items text-stat)
                                       deleted-aux
                                       added-aux))))))
     (t
      (lyskom-skip-tokens tokens)))))

(defun lyskom-async-new-user-area (pers-stat old-user-area new-user-area)
  (when pers-stat
    (let ((need-reread (and (eq lyskom-pers-no (pers-stat->pers-no pers-stat))
                            (not (eq lyskom-current-user-area new-user-area)))))
  ;; Update the cache
  (set-pers-stat->user-area pers-stat new-user-area)
  ;; Re-read
  (when (and need-reread
             new-user-area
             (not (zerop new-user-area)))
    (initiate-get-text 'follow (lambda (text)
                                 (when text
                                   (lyskom-format-insert-before-prompt 'reading-settings-from-server)
                                   (setq lyskom-saved-unknown-variables
                                    (lyskom-read-options-eval text))))
                       new-user-area)))))


(defun lyskom-async-forced-leave-conf (conf-stat conf-no)
  (if conf-stat
      (lyskom-format-insert-before-prompt 'no-longer-member conf-stat)
    (lyskom-format-insert-before-prompt 'no-longer-member-n conf-no))
  (lyskom-remove-membership conf-no)
  (when (eq conf-no lyskom-current-conf)
    (lyskom-leave-current-conf))
  (read-list-delete-read-info conf-no lyskom-to-do-list)
  (lyskom-update-prompt))

(defun lyskom-async-new-membership (conf-conf-stat
                                    membership
                                    pers-no 
                                    conf-no)
  ;; Are we already members?

  (when membership
    (let ((cur-mship (lyskom-try-get-membership conf-no t))
          (mship-type (membership->type membership)))
      (unless cur-mship
        (lyskom-format-insert-before-prompt 
         (cond ((membership-type->invitation mship-type)
                'have-become-invited-member)
               ((membership-type->passive mship-type)
                'have-become-passive-member)
               (t 'have-become-member))
         conf-conf-stat))

    (cond ((membership-type->passive (membership->type membership))
           (lyskom-replace-membership membership)
           (when (eq conf-no lyskom-current-conf)
             (lyskom-leave-current-conf))
           (read-list-delete-read-info conf-no lyskom-to-do-list)
           (lyskom-update-prompt))

          ;; Already a member. Perhaps the priority changed.
          ;; Update the cache. The reading list is probably also
          ;; not quite correct since the priority might have changed

          (cur-mship
           (lyskom-replace-membership membership)
           (lyskom-sort-to-do-list))

          ;; Not a member. Completely new. Deal with it.
          (t (lyskom-add-membership membership conf-no))))))
    

(defun lyskom-show-presence (num flag)
  "Returns non-nil if presence messages for NUM should be displayed
according to the value of FLAG."
  (cond ((null flag) nil)
        ((eq flag 'friends) (memq num kom-friends))
        ((eq flag 'morons) (memq num kom-morons))
        ((eq flag 'friends-and-morons) (or (memq num kom-friends)
                                           (memq num kom-morons)))
        ((listp flag) (lyskom-indirect-assq num flag))
        (t t)))


(defun lyskom-show-logged-in-person (conf-stat session-info)
  "Visa p} kommandoraden vem som loggat in."
  (let ((server (lyskom-session-nickname))
	(login-when-date (let ((kom-print-relative-dates nil))
			   (lyskom-format-time 'date)))
	(login-when-time (let ((kom-print-relative-dates nil))
			   (lyskom-format-time 'time))))
    (cond
     ((lyskom-is-in-minibuffer))
     ((lyskom-show-presence (conf-stat->conf-no conf-stat)
                            kom-presence-messages-in-echo-area)
      (lyskom-message
       "%s"
       (lyskom-format 'has-entered
                      (or conf-stat
                          (lyskom-get-string 'unknown-person))
                      server
                      ))))

    (cond
     ((lyskom-show-presence (conf-stat->conf-no conf-stat)
                            kom-presence-messages-in-buffer)
      (lyskom-format-insert-before-prompt 
       'has-entered-r
       (or conf-stat 
	   (lyskom-get-string
	    'unknown-person))
       (and kom-text-properties
	    (list 'face kom-presence-face))
       server
       login-when-date
       login-when-time
       (and session-info
	    nil
	    (lyskom-combine-username 
	     (static-session-info->username session-info)
	     (static-session-info->ident-user session-info)
	     (static-session-info->hostname session-info))))))))


(defun lyskom-show-logged-out-person (conf-stat session-no)
  "Visa p} kommandoraden vem som loggat ut."
  (let ((server (lyskom-session-nickname))
	(logout-when-date (let ((kom-print-relative-dates nil))
			    (lyskom-format-time 'date)))
	(logout-when-time (let ((kom-print-relative-dates nil))
			    (lyskom-format-time 'time))))
  (cond
   ((lyskom-is-in-minibuffer))
   ((lyskom-show-presence (conf-stat->conf-no conf-stat) 
                          kom-presence-messages-in-echo-area)
    (lyskom-message
     "%s"
     (lyskom-format 'has-left (or conf-stat
				  (lyskom-get-string 'unknown-person))
                    server))))
  (cond
   ((lyskom-show-presence (conf-stat->conf-no conf-stat)
                          kom-presence-messages-in-buffer)
    (lyskom-format-insert-before-prompt 'has-left-r 
					(or conf-stat
					    (lyskom-get-string
					     'unknown-person))
					(and kom-text-properties
					     (list 'face kom-presence-face))
					server
					logout-when-date
					logout-when-time)))))



(defun lyskom-is-in-minibuffer ()
  "Returns non-nil if I am using the minibuffer for some reading."
  (or lyskom-inhibit-minibuffer-messages
      cursor-in-echo-area
      (not (zerop (minibuffer-depth)))))


 (defun lyskom-show-personal-message (sender recipient message 
                                            &optional when nobeep)
  "Insert a personal message into the lyskom buffer.
Args: SENDER: conf-stat for the person sending the message.
      RECIPIENT: 0 if this message is for everybody, otherwise the conf-stat
                 of the recipient.
      MESSAGE: A string containing the message.
      WHEN: Optional time of arrival. A lyskom time structure.
      NOBEEP: True means don't beep. No matter what."
  (lyskom-insert-personal-message sender recipient message when nobeep)
  (setq lyskom-last-personal-message-sender 
        (if (stringp sender) sender (conf-stat->conf-no sender)))
  (setq lyskom-last-group-message-recipient 
        (if (and recipient
                 (not (eq 0 recipient))
                 (not (eq (conf-stat->conf-no recipient) lyskom-pers-no)))
            (conf-stat->conf-no recipient)
          nil))
  (run-hooks 'kom-personal-message-hook))


(defun lyskom-insert-personal-message (sender recipient message
                                              &optional when nobeep)
  "Insert a personal message in the current buffer.
Arguments: SENDER RECIPIENT MESSAGE.
SENDER is a conf-stat (possibly nil).
RECIPIENT is 0 if the message is public, otherwise the conf-stat of the
recipient.
MESSAGE is a string containing the message.
WHEN, if given, is the time when the message arrived. It must be a lyskom
time structure.
Non-nil NOBEEP means don't beep."
  (lyskom-handle-as-personal-message
   (lyskom-format-as-personal-message sender recipient message when nobeep)
   (conf-stat->conf-no sender) 
   nil))

(defun lyskom-format-as-personal-message (sender 
                                          recipient
                                          message
                                          &optional when nobeep)
  "Formats a personal message, returning it as a string.
Arguments: SENDER RECIPIENT MESSAGE.
SENDER is a conf-stat (possibly nil).
RECIPIENT is 0 if the message is public, otherwise the conf-stat of the
recipient.
MESSAGE is a string containing the message.
WHEN, if given, is the time when the message arrived. It must be a lyskom
time structure.
Non-nil NOBEEP means don't beep."
  (let ((lyskom-last-text-format-flags nil)
        (now (lyskom-current-client-time)))
    (when (null when) (setq when (lyskom-current-client-time)))
    (if (or kom-show-personal-message-date
            (not (eq (time->mday when) (time->mday now)))
            (not (eq (time->mon when) (time->mon now)))
            (not (eq (time->year when) (time->year now))))
        (setq when (let ((kom-print-relative-dates nil))
                     (lyskom-format-time 'date-and-time when)))
      (setq when (lyskom-format-time 'time when)))

    (setq nobeep (or nobeep (and kom-ansaphone-on
                                 kom-silent-ansaphone)))

    (cond ((or (null recipient)		; Have been seen to be nil when
                                        ; listing recorded
                                        ; messages. Should it be?
                                        ; /davidk
	       (eq recipient 0))	; Public message
           (if (not nobeep) (lyskom-beep kom-ding-on-common-messages sender))
           (lyskom-format (lyskom-get-string-sol 'message-broadcast)
                          (cond
                           ((stringp sender) sender)
                           (sender sender)
                           (t (lyskom-get-string 'unknown)))
                          message
                          when
                          (when kom-async-highlight-dashed-lines
                            `(face ,(or kom-async-dashed-lines-face
                                        lyskom-default-async-dashed-lines-face)))
                          (when kom-async-highlight-text-body
                            `(face ,(or kom-async-text-body-face
                                        lyskom-default-async-text-body-face)))))
          ((= (conf-stat->conf-no recipient) lyskom-pers-no) ; Private
           (if (not nobeep) (lyskom-beep kom-ding-on-personal-messages sender))
           (lyskom-format (lyskom-get-string-sol 'message-from)
                          (cond
                           ((stringp sender) sender)
                           (sender sender)
                           (t (lyskom-get-string 'unknown)))
                          message
                          when
                          (when kom-async-highlight-dashed-lines
                            `(face ,(or kom-async-dashed-lines-face
                                        lyskom-default-async-dashed-lines-face)))
                          (when kom-async-highlight-text-body
                            `(face ,(or kom-async-text-body-face
                                        lyskom-default-async-text-body-face)))))
          (t                            ; Group message
           (if (not nobeep) (lyskom-beep kom-ding-on-group-messages recipient))
           (lyskom-format (lyskom-get-string-sol 'message-from-to)
                          message
                          (cond
                           ((stringp sender) sender)
                           (sender  sender)
                           (t (lyskom-get-string 'unknown)))
                          (cond
                           ((stringp recipient) recipient)
                           (recipient recipient)
                           (t (lyskom-get-string 'unknown)))
                          when
                          (when kom-async-highlight-dashed-lines
                            `(face ,(or kom-async-dashed-lines-face
                                        lyskom-default-async-dashed-lines-face)))
                          (when kom-async-highlight-text-body
                            `(face ,(or kom-async-text-body-face
                                        lyskom-default-async-text-body-face))))))))


  
(defun lyskom-handle-as-personal-message (string from &optional filter)
  "Insert STRING as a personal message and beep if not from me and
supposed to. The buffer, is chosen according to the
kom-show-personal-messages-in-buffer variable value. The text is
converted, before insertion."
  (if (and filter
           (or
            (eq 0 (string-match "^Remote-command: [0-9]+ [0-9]+\n" string))
            (eq 0 (string-match "^Auto-reply:\n" string))))
      nil
    (let ((pop kom-pop-personal-messages))
      (lyskom-save-excursion
       (cond
	((eq kom-show-personal-messages-in-buffer t)
	 (lyskom-insert-before-prompt string)
	 (if pop (display-buffer (current-buffer))))
	((null kom-show-personal-messages-in-buffer))
	(t
         (let ((inhibit-read-only t))
           (let ((message-buffer
                  (car (lyskom-buffers-of-category 'personal-messages))))
             (if message-buffer
                 (set-buffer message-buffer)
               (set-buffer (lyskom-get-buffer-create 
                            'personal-messages 
                            kom-show-personal-messages-in-buffer t))
               (lyskom-view-mode)
               (setq buffer-read-only t)))
           (goto-char (point-max))
           (lyskom-insert string )
           (if pop
               (save-selected-window
                 (select-window (lyskom-display-buffer (current-buffer)
                                                       (not (eq pop t))))
                 (goto-char (point-max))
                 (recenter -1))))))))))
  


;;; ================================================================
;;; New recipient

;;; The text stat and might have been cached and thus invalid. Check
;;; for this. The conf-stat for the conf-no is almost certainly 
;;; invalid.

(defun lyskom-async-new-recipient (text-stat text-no conf-no misc-type)
  "Handle a new recipient message"

  ;; Check if we are added. A new letter!
  (when (and (eq  conf-no lyskom-pers-no)
             (not (eq (text-stat->author text-stat) lyskom-pers-no)))
    (lyskom-beep kom-ding-on-new-letter))

  ;; If the text is read in another conference, mark it as read here too
  ;; unless the new recipient is the mailbox
  (if (and kom-mark-read-texts-as-read-in-new-recipient
           (lyskom-text-read-at-least-once-p text-stat t)
           (not (eq conf-no lyskom-pers-no)))
      (lyskom-traverse misc-info (text-stat->misc-info-list text-stat)
        (when (and (memq (misc-info->type misc-info) lyskom-recpt-types-list)
                   (eq conf-no (misc-info->recipient-no misc-info)))
          (initiate-mark-as-read 'follow
                                 nil
                                 conf-no 
                                 (list (misc-info->local-no misc-info)))))

    ;; Text is previously unread or in the mailbox
    (let ((local-no nil))
      (lyskom-traverse misc-info (text-stat->misc-info-list text-stat)
        (when (and (eq (misc-info->type misc-info) misc-type)
                   (eq (misc-info->recipient-no misc-info) conf-no))
          (setq local-no (misc-info->local-no misc-info))))
      (when local-no
        (initiate-get-conf-stat 'async 'lyskom-add-new-text
                                conf-no
                                text-no
                                local-no)
        (lyskom-prefetch-text-all text-no)
        (lyskom-run 'async 'lyskom-default-new-recipient-hook text-stat)
        (lyskom-run 'async 'lyskom-prefetch-and-print-prompt)))))

(defun lyskom-default-new-recipient-hook (text-stat)
  (when (and (not lyskom-dont-change-prompt) ;We shall change it
             (not lyskom-executing-command)) ;We have time to do it.
    (lyskom-update-prompt))
  (run-hooks 'kom-new-recipient-hook))



;;; ================================================================
;;;            Functions for dealing with a new or deleted text


(defun lyskom-default-new-text-hook (text-stat)
  "Print a message if the user was waiting. Change the prompt. run hooks."
  (if (and (not lyskom-dont-change-prompt) ;We shall change it
	   (not lyskom-executing-command)) ;We have time to do it.
      (lyskom-update-prompt))

  (let ((no-message nil))
    (run-hooks 'kom-new-text-hook)
  
    (if (and (not no-message)
	     lyskom-is-waiting
	     (not (lyskom-is-in-minibuffer)))
	(lyskom-message "%s" (lyskom-format 'text-is-created
					    (text-stat->text-no text-stat))))))

(defun lyskom-default-deleted-text-hook (text-stat)
  "Update the prompt. Run hooks"
  (if (and (not lyskom-dont-change-prompt) ;We shall change it
	   (not lyskom-executing-command)) ;We have time to do it.
      (lyskom-update-prompt))
  (run-hooks 'kom-deleted-text-hook))

(defun lyskom-async-new-text (text-stat)
  "Take care of a message that a new text has been created."
  (cache-del-pers-stat (text-stat->author text-stat)) ;+++Borde {ndra i cachen i st{llet.
  
  (lyskom-traverse
      misc-info (text-stat->misc-info-list text-stat)
    (let ((type (misc-info->type misc-info)))
      (cond
       ((memq type lyskom-recpt-types-list)
	;; add on lyskom-reading-list and lyskom-to-do-list if
	;; this recipient is a recipient that has been checked.
	(if (and (eq  (misc-info->recipient-no misc-info)
                      lyskom-pers-no)
                 (not (eq (text-stat->author text-stat)
                          lyskom-pers-no)))
            (lyskom-beep kom-ding-on-new-letter))
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

  (lyskom-prefetch-text-stat-all text-stat)

  ;; Give a message if the user is waiting. Update the prompt.
  (lyskom-run 'async 'lyskom-default-new-text-hook text-stat)

  (lyskom-run 'async 'lyskom-prefetch-and-print-prompt))


(defun lyskom-async-deleted-text (text-stat)
  "Take care of a message that a text has been deleted."
      (cache-del-pers-stat (text-stat->author text-stat))
      (lyskom-traverse
          misc-info (text-stat->misc-info-list text-stat)
        (let ((type (misc-info->type misc-info)))
          (cond ((memq type lyskom-recpt-types-list)
                 (initiate-get-conf-stat 'async 'lyskom-delete-old-text
                                         (misc-info->recipient-no misc-info)
                                         (text-stat->text-no text-stat)
                                         (misc-info->local-no misc-info)))
                ((eq type 'COMM-TO)
                 (cache-del-text-stat (misc-info->comm-to misc-info)))
                ((eq type 'FOOTN-TO)
                 (cache-del-text-stat (misc-info->footn-to misc-info))))))
      (lyskom-run 'async 'lyskom-default-deleted-text-hook text-stat)
      (lyskom-run 'async 'lyskom-prefetch-and-print-prompt))

(defun lyskom-delete-old-text (recipient text-no local-no)
  "RECIPIENT is a conf-stat and previous recipient of TEXT-NO.
This call is used in response to a deleted text message"
  (when recipient

    ;; Update the cache

    (cache-del-text-stat text-no)
    (cache-del-text text-no)
    (set-conf-stat->no-of-texts 
     recipient
     (min (conf-stat->no-of-texts recipient)
          (min (conf-stat->no-of-texts recipient)
               (1- (conf-stat->no-of-texts recipient)))))

    ;; Update the read lists

    (let ((membership (lyskom-try-get-membership
                       (conf-stat->conf-no recipient))))
      (when (and membership
                 (lyskom-visible-membership membership))
        (read-list-delete-text text-no lyskom-to-do-list)
        (read-list-delete-text text-no lyskom-reading-list)))

    (lyskom-set-mode-line)))
                 

    

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

    ;; Prefetch thoughts:
    ;; We need a way to check if a conferences is fetched.
    ;; davidk /960924
    
    (let ((membership (lyskom-try-get-membership
		       (conf-stat->conf-no recipient))))
      (if (and membership
	       ;; (lyskom-conf-fetched-p (conf-stat->conf-no recipient))
	       (lyskom-visible-membership membership)
	       (not (read-list-enter-text text-no recipient
					  lyskom-to-do-list)))
	  ;; If we have already read all texts in the conference or the
	  ;; text has not been prefetched
	  (let ((info (lyskom-create-read-info
		       'CONF
		       recipient
		       (membership->priority membership)
		       (lyskom-create-text-list (list text-no)))))
	    (read-list-enter-read-info info lyskom-to-do-list)
	    (if (= lyskom-current-conf (conf-stat->conf-no recipient))
		(read-list-enter-read-info info lyskom-reading-list)))

        ;; We don't have the membership yet. Treat it as an unread conf
        ;; and prefetch it. This might result in two prefetches of the
        ;; same conference, but the prefetch should be able to deal with
        ;; that.
        (unless membership
          (lyskom-prefetch-one-membership (conf-stat->conf-no recipient)
                                          lyskom-pers-no))))

    (lyskom-set-mode-line))))


;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; end: 
