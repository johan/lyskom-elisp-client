;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: commands2.el,v 44.225 2007-07-11 20:48:42 byers Exp $
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
;;;; File: commands2.el
;;;;
;;;; This file contains the code for some high level commands.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
              "$Id: commands2.el,v 44.225 2007-07-11 20:48:42 byers Exp $\n"))

(eval-when-compile
  (require 'lyskom-command "command"))


;;; ================================================================
;;;              Lista medlemsskap - List membership

(def-kom-command kom-membership ()
  "Alias for `kom-prioritize'."
  (interactive)
  (lyskom-prioritize))


;;; ================================================================
;;;          Status (f|r) M|te - Status for a conference

;;; Author: ceder (with some help by Linus)
;;; much enhanced by Inge Wallin (lyskom-status-conf-2 and beyond)


(defun lyskom-conf-type-marker (conf-stat)
  "Return a pretty string describing the type of CONF-STAT."
  (let* ((type (conf-stat->conf-type conf-stat))
	 (box (conf-type->letterbox type))
	 (ori (conf-type->original type))
	 (pro (conf-type->rd_prot type))
	 (sec (conf-type->secret type)))
    (cond
     ((or box ori pro sec)
      (concat
       " ("
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

(def-kom-command kom-status-conf (&optional conf-no)
  "Print information about a conference.
The information listed may be taken from the client's cache and
therefore slightly out of date.

See `kom-extended-status-information'.

Lisp documentation:
If argument CONF-NO is existing and non-nil then this conference is used. 
otherwise: the conference is read with lyskom-completing-read."
  (interactive)
  (let ((conf-no
         (or conf-no
             (lyskom-read-conf-no 'conf-for-status '(all) nil nil t)))
        (kom-print-seconds-in-time-strings nil)
        (kom-extended-status-information (lyskom-extended-status-override 'conf)))
    (cache-del-conf-stat conf-no)
    (cache-del-uconf-stat conf-no)
    (blocking-do-multiple ((conf-stat (get-conf-stat conf-no))
                           (uconf-stat (get-uconf-stat conf-no)))
      (if (null conf-stat)
          (lyskom-insert-string 'no-such-conf)
        
	(lyskom-format-insert 'status-record
			      conf-stat
			      (lyskom-conf-type-marker conf-stat))
        (let ((creator (conf-stat->creator conf-stat)))
          (lyskom-format-insert 'created-by
                                creator
                                creator
                                (if (and 
                                     (lyskom-conf-stat-p creator)
                                     (> (lyskom-string-width (conf-stat->name creator))
                                        (- (lyskom-window-width) 46)))
                                    "\n"
                                  "")))
        (lyskom-format-insert 'created-at
                              (lyskom-format-time
                               'date-and-time
                               (conf-stat->creation-time conf-stat)))
        (lyskom-format-insert 'members
                              (conf-stat->no-of-members conf-stat))
        (lyskom-format-insert 'conf-allows-secret-members
                              (lyskom-get-string 
                               (if (conf-type->forbid-secret 
                                    (uconf-stat->conf-type uconf-stat))
                                   'secret-members-not-permitted
                                 'secret-members-permitted)))
        (lyskom-format-insert 'conf-allows-anon-texts
                              (lyskom-get-string 
                               (if (conf-type->anarchy
                                    (uconf-stat->conf-type uconf-stat))
                                   'anon-texts-permitted
                                 'anon-texts-not-permitted))) 
        (lyskom-format-insert 'garb-nice
                              (conf-stat->garb-nice conf-stat))
        (lyskom-format-insert 'keep-commented
                              (conf-stat->keep-commented conf-stat))
        (lyskom-format-insert 'lowest-local-no
                              (conf-stat->first-local-no conf-stat))
        (lyskom-format-insert 'highest-local-no
                              (1- (+ (conf-stat->no-of-texts conf-stat)
                                     (conf-stat->first-local-no conf-stat))))
        (lyskom-format-insert 'last-text-time
                              (lyskom-format-time
                               'date-and-time
                               (conf-stat->last-written conf-stat)))
        (lyskom-format-insert 'no-of-motd
                              (conf-stat->msg-of-day conf-stat))
        (let ((superconf (conf-stat->super-conf conf-stat)))
          (lyskom-format-insert 'superconf-is-no-name
                                superconf
                                superconf
                                (if (and 
                                     (lyskom-conf-stat-p superconf)
                                     (> (lyskom-string-width (conf-stat->name superconf))
                                        (- (lyskom-window-width) 46)))
                                    "\n"
                                  "")))
        (let ((permitted-submitters (conf-stat->permitted-submitters conf-stat)))
          (lyskom-format-insert 'permitted-submitters-no-name
                                permitted-submitters
                                (if (zerop permitted-submitters)
                                    (lyskom-get-string 'Everybody)
                                  permitted-submitters)
                                ""))
        (let ((supervisor (conf-stat->supervisor conf-stat)))
          (lyskom-format-insert 'supervisor-is-no-name
                                supervisor
                                supervisor
                                ""))
        (lyskom-format-insert 'presentation-no
                              (conf-stat->presentation conf-stat))

        (if (zerop (conf-stat->msg-of-day conf-stat))
            nil
          (lyskom-format-insert 'conf-has-motd conf-stat)
          (lyskom-view-text (conf-stat->msg-of-day conf-stat)))

        ;; Show aux items

        (lyskom-traverse-aux item
            (conf-stat->aux-items conf-stat)
          (if (lyskom-aux-item-definition-field item 'status-print)
              (lyskom-aux-item-call item 'status-print item conf-stat)
            (lyskom-format-insert 'status-aux-item
                                  (format "%d/%d" 
                                          (aux-item->aux-no item)
                                          (aux-item->tag item))
                                  (aux-item->creator item)
                                  (lyskom-aux-item-terminating-button item conf-stat))
            ))

        (let ((mship (lyskom-try-get-membership (conf-stat->conf-no conf-stat) t)))
          (when mship
            (lyskom-format-insert 'conf-mship-priority
                                  (membership->priority mship)
                                  (lyskom-return-membership-type
                                   (membership->type mship)))))

        ;; Show all members of CONF-STAT if the user so wishes."
        (lyskom-scroll)
        (if (lyskom-j-or-n-p
             (lyskom-get-string 'show-members-list-also-q))
            (let ((member-list (blocking-do 'get-members
                                            (conf-stat->conf-no conf-stat)
                                            0 lyskom-max-int)))

              (if (null member-list)
                  (lyskom-format-insert 'conf-has-no-members conf-stat)
                (lyskom-format-insert 'conf-has-these-members conf-stat)
                (if (lyskom-j-or-n-p (lyskom-get-string 'show-membership-info-q))
                    (progn
                      (lyskom-insert-string 'member-list-header)
                      (lyskom-traverse 
                       member (member-list->members member-list)
                       (let ((membership (blocking-do
                                          'query-read-texts 
                                          (member->pers-no member)
                                          (conf-stat->conf-no conf-stat)
                                          t
                                          0)))
                         ;; Print a row describing the membership of MEMBER
                         ;; (described by MEMBERSHIP) in CONF-STAT.
                         (if (or (null membership))
                             (lyskom-insert-string 'secret-membership)
                           (lyskom-format-insert 
                            "%#1@%-17#2s"
                            (if (membership-type->passive
                                 (member->membership-type member))
                                `(face ,kom-dim-face)
                              nil)
                            (lyskom-format-time
                             'date-and-time
                             (membership->last-time-read membership)))

                           (let ((unread (- (+ (conf-stat->first-local-no
                                                conf-stat)
                                               (conf-stat->no-of-texts conf-stat))
                                            (membership->last-text-read membership)
                                            (length (membership->read-texts
                                                     membership))
                                            1)))
                             (lyskom-format-insert 'conf-membership-line
                                                   (if (zerop unread)
                                                       "           "
                                                     (format "%9d  " unread))
                                                   (member->pers-no member)
                                                   (lyskom-return-membership-type (member->membership-type member))
                                                   (if (membership-type->passive
                                                        (member->membership-type member))
                                                       `(face ,kom-dim-face)
                                                     nil)
                                                   )
                             (when (and (member->created-by member)
                                        (not (zerop (member->created-by member)))
                                        (not (eq (member->pers-no member)
                                                 (member->created-by member))))
                               (lyskom-format-insert 'conf-membership-line-2
                                                     (lyskom-format-time
                                                      'date-and-time
                                                      (member->created-at member))
                                                     (member->created-by member)))
                             )))))

                  ;; Don't show membership info
                  (lyskom-insert "\n")
                  (lyskom-traverse
                   member (member-list->members member-list)
                   (lyskom-format-insert "  %#1P\n" 
                                         (member->pers-no member)))))))))))


;;; ================================================================
;;;            Status (f|r) Person - status for a person

;;; Author: ceder
;;; Heavily enhanced: Inge Wallin (lyskom-status-pers-3 and beyond)


(def-kom-command kom-status-person (&optional text-or-pers-no)
  "Show information about a person. 
If a prefix argument is given, the status of the author of that text
will be shown.

See `kom-extended-status-information'.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (and current-prefix-arg ; only peek at textno:s when prefixed!
		    (list (lyskom-read-text-no-prefix-arg
			   'text-to-see-author-status-of))))
  (let ((pers-no
         (or (when (interactive-p)
	       (text-stat->author (blocking-do 'get-text-stat text-or-pers-no)))
	     text-or-pers-no
             (lyskom-read-conf-no 'pers-for-status '(pers) nil nil t)))
        (kom-print-seconds-in-time-strings nil)
        (kom-extended-status-information (lyskom-extended-status-override 'pers))
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
                            conf-stat)
      (lyskom-format-insert 'created-time
                            (lyskom-format-time
                             'date-and-time
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
      (let ((time (pers-stat->total-time-present pers-stat)))
        (unless (zerop time) ;; Why not let it print "0 d 00:00:00"?
          (lyskom-format-insert 'present-time-d-h-m-s
                                (floor time (* 24 3600))
                                (mod (floor time 3600) 24)
                                (mod (floor time 60) 60)
                                (round (mod time 60)))))
      (lyskom-format-insert 'last-log-in
                            (lyskom-format-time
                             'date-and-time
                             (pers-stat->last-login pers-stat)))
      (lyskom-format-insert 'user-name
                            (pers-stat->username pers-stat))
      (lyskom-format-insert 'read-texts
                            (pers-stat->read-texts pers-stat))
      (if (= (pers-stat->pers-no pers-stat) lyskom-pers-no)
          (lyskom-format-insert 'marked-texts
                                (pers-stat->no-of-marks pers-stat)))
      (lyskom-format-insert 'time-for-last-letter
                            (lyskom-format-time
                             'date-and-time
                             (conf-stat->last-written conf-stat)))

      (let* ((a (lyskom-format 'pers-has-privileges ""))
             (b (concat "\n" (make-string (lyskom-string-width a) ?\ ))))
        (lyskom-format-insert 'pers-has-privileges
                              (lyskom-privilege-string (pers-stat->privileges pers-stat)
                                                       'pers-has-privileges-2
                                                       b)))
      (let ((superconf (conf-stat->super-conf conf-stat)))
        (lyskom-format-insert 'superconf
                              superconf
                              superconf
                              ""))
      (if (not (zerop (conf-stat->supervisor conf-stat)))
          (let ((supervisor (conf-stat->supervisor conf-stat)))
            (lyskom-format-insert 'supervisor
                                  supervisor
                                  supervisor
                                  "")))
      (lyskom-format-insert 'member-of-confs
                            (pers-stat->no-of-confs pers-stat))
      (lyskom-format-insert 'presentation
                            (conf-stat->presentation conf-stat))

      ;; Show aux items

      (lyskom-traverse-aux item
          (conf-stat->aux-items conf-stat)
        (lyskom-aux-item-call item 'status-print item conf-stat))

      ;; Show motd

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
              (deferred-mships nil)
              (lyskom-count-var 0)
              (lyskom-passive-count-var 0))

          (if (null membership-list)
              (lyskom-format-insert 'not-allowed-see-confs conf-stat)
            (lyskom-format-insert 'is-member-of conf-stat)
            (lyskom-insert-string 'membership-list-header)
            (setq lyskom-count-var 0)
            (setq lyskom-passive-count-var 0)
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
             (if (membership-type->passive 
                  (membership->type membership))
                 (setq deferred-mships 
                       (cons membership deferred-mships))
               (setq lyskom-count-var
                     (+ lyskom-count-var
                        (lyskom-status-pers-list-one-membership conf-stat membership)))))
            (lyskom-traverse membership (nreverse deferred-mships)
              (setq lyskom-passive-count-var
                     (+ lyskom-passive-count-var
                        (lyskom-status-pers-list-one-membership conf-stat membership)))))

          ;; "Print the total number of unread texts for the person CONF-STAT."
          (lyskom-format-insert 'his-total-unread 
                                conf-stat
                                lyskom-count-var
                                lyskom-passive-count-var))))))

(defun lyskom-status-pers-list-one-membership (conf-stat membership)
  (let ((member-conf-stat
         (blocking-do 'get-conf-stat 
                      (membership->conf-no membership))))
    (if (or (null member-conf-stat)
            (null membership))
        (lyskom-insert-string 'secret-membership)
      (lyskom-format-insert
       "%#1@%-17#2s"
       (if (membership-type->passive
            (membership->type membership))
           `(face ,kom-dim-face)
         nil)
       (lyskom-format-time
        'date-and-time
        (membership->last-time-read membership)))
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
         (if (zerop unread) "          " (format "%9d " unread))
         (if (= (conf-stat->conf-no conf-stat)
                (conf-stat->supervisor member-conf-stat))
             (lyskom-get-string 'is-supervisor-mark)
           "  ")
         member-conf-stat
         (lyskom-return-membership-type
          (membership->type membership))
         (if (membership-type->passive
              (membership->type membership))
             `(face ,kom-dim-face)
           nil)
         )
        (when (and (membership->created-by membership)
                   (not (zerop (membership->created-by membership)))
                   (not (eq (conf-stat->conf-no conf-stat)
                            (membership->created-by membership))))
          (lyskom-format-insert 'pers-membership-line-2
                                (lyskom-format-time
                                 'date-and-time
                                 (membership->created-at membership))
                                (membership->created-by membership)))
        unread))))



;;; ================================================================
;;;              Skicka meddelande - Send message

;;; Author: Inge Wallin
;;; Rewritten to use lyskom-read-conf-no by Linus Tolke
;;; Modified to use default recipient by David Byers


(defun lyskom-default-conference-for-send-message (&rest args)
  (let ((tmp (cond
              ((eq kom-default-message-recipient 'everybody) nil)
              ((and (eq kom-default-message-recipient 'group)
                    lyskom-last-group-message-recipient)
               lyskom-last-group-message-recipient)
              ((or (and (eq kom-default-message-recipient 'group)
                        (null lyskom-last-group-message-recipient))
                   (and (eq kom-default-message-recipient 'sender)
                        lyskom-last-personal-message-sender))
               lyskom-last-personal-message-sender)
              ((and (eq kom-default-message-recipient 'last-recipient)
                    lyskom-last-message-recipient
                    (not (eq 0 lyskom-last-message-recipient))
                    lyskom-last-message-recipient))
              (t
               (if lyskom-last-personal-message-sender
                   lyskom-last-personal-message-sender
                 nil)))))
    (and tmp (list tmp))))

(def-kom-command kom-moronify (&optional whom)
  "Add person to `kom-morons' list."
  (interactive)
  (let ((moron (or whom
		   (lyskom-read-conf-no 'moronify-whom '(pers) nil nil t))))
    (when (not (memq moron kom-morons))
      (setq kom-morons (cons moron kom-morons))
      (lyskom-save-options (current-buffer)
			   (lyskom-get-string 'moronify-saving)
			   (lyskom-get-string 'moronify-saving-done)
			   (lyskom-get-string 'moronify-saving-error)))))

(def-kom-command kom-befriend (&optional whom)
  "Add person to `kom-friends' list."
  (interactive)
  (let ((friend (or whom
		    (lyskom-read-conf-no 'befriend-whom '(pers) nil nil t))))
    (when (not (memq friend kom-friends))
      (setq kom-friends (cons friend kom-friends))
      (lyskom-save-options (current-buffer)
			   (lyskom-get-string 'befriend-saving)
			   (lyskom-get-string 'befriend-saving-done)
			   (lyskom-get-string 'befriend-saving-error)))))

(def-kom-command kom-send-message (&optional who message)
  "Send a message to another user or all members of a conference.
Messages sent with this command are not texts and are not stored
in the database. If you don't know if you should use this or write
a text, write a text \(see `kom-write-text' instead). Remember that
messages of this type are intrusive, yet may not be read by all
users.

Runs `kom-send-message-setup-hook' when entering the minibuffer.

See `kom-default-message-recipient'."
  (interactive)
  (lyskom-interactive-send-message who message nil))


(def-kom-command kom-send-alarm (&optional message) 
  "Send a message to all of the users in LysKOM.
Don't use this command unless what you have to say is really important to
everyone who is logged on.

Runs `kom-send-message-setup-hook' when entering the minibuffer."
  (interactive)
  (lyskom-interactive-send-message nil message t))


(defun lyskom-interactive-send-message (who message alarm-ok)
  "Implementation of kom-send-message and kom-send-alarm."
  (let* ((target (or who
                     (lyskom-read-conf-no
                      (list 'who-to-send-message-to
                            (lyskom-get-string (if alarm-ok 'everybody 'nobody)))
                      (if kom-permissive-completion '(all) '(login conf))
                      t nil t))))
    (cond ((and (zerop target)
                (not alarm-ok))
           (lyskom-format-insert 'message-use-alarm-instead
                                 (lyskom-command-name 'kom-send-alarm)))
          ((not (zerop target))
           (setq lyskom-last-message-recipient target)
           (lyskom-format-insert 'message-recipient-info target)
           (lyskom-send-message target message))

          (t (lyskom-format-insert 'message-all-info 
                                   `(face ,kom-warning-face)
                                   'kom-send-message
                                   (max 20 (- (window-width) 8)))
             (lyskom-beep t)
             (lyskom-send-message target message)))))


(defvar lyskom-message-recipient)
(defvar lyskom-message-string)


(defun lyskom-send-message-minibuffer-setup-hook ()
  (unwind-protect
      (progn
        (run-hooks 'kom-send-message-setup-hook))
    (remove-hook 'minibuffer-setup-hook 
                 'lyskom-send-message-minibuffer-setup-hook)))

(defun lyskom-send-message-minibuffer-exit-hook ()
  (unwind-protect
      (progn
        (run-hooks 'kom-send-message-exit-hook))
    (remove-hook 'minibuffer-exit-hook
                 'lyskom-send-message-minibuffer-exit-hook)))

(defun lyskom-send-message (pers-no
                            message &optional dontshow)
  "Send a message to the person with the number PERS-NO.  PERS-NO == 0
means send the message to everybody. MESSAGE is the message to
send. If DONTSHOW is non-nil, don't display the sent message."
  (let* ((lyskom-message-string nil)
         (reply nil)
         (lyskom-message-recipient nil)
         (lyskom-last-text-format-flags nil))

    (add-hook 'minibuffer-setup-hook
              'lyskom-send-message-minibuffer-setup-hook)
    (add-hook 'minibuffer-exit-hook
              'lyskom-send-message-minibuffer-exit-hook)
    (setq lyskom-message-string 
          (or message
              (lyskom-read-string (lyskom-format 'message-prompt pers-no)
                                  nil
                                  'lyskom-message-history)))
    (setq lyskom-message-recipient (if (zerop pers-no)
                                       nil
                                     (blocking-do 'get-conf-stat 
                                                  pers-no)))

    (run-hooks 'kom-send-message-hook)
    (if lyskom-message-string
        (progn
          (setq reply (blocking-do 'send-message pers-no
                                   lyskom-message-string))

          (if reply
              (if (not dontshow)
                  (lyskom-handle-as-personal-message
                   (lyskom-format
                    (if lyskom-message-recipient
                        (lyskom-get-string-sol 'message-sent-to-user)
                      (lyskom-get-string-sol 'message-sent-to-all))
                    lyskom-message-string 
                    lyskom-message-recipient
                    (when kom-async-highlight-dashed-lines
                      `(face ,(or kom-async-dashed-lines-face
                                  lyskom-default-async-dashed-lines-face)))
                    (when kom-async-highlight-text-body
                      `(face ,(or kom-async-text-body-face
                                  lyskom-default-async-text-body-face)))
		    (let ((kom-print-relative-dates nil))
		      (lyskom-format-time 'date-and-time)))
                   lyskom-pers-no
                   kom-filter-outgoing-messages))
            (lyskom-format-insert-before-prompt
             'message-nope 
             (or lyskom-message-recipient
                 (lyskom-get-string 'everybody))
             lyskom-message-string
             (lyskom-format 'error-code
                            (lyskom-get-error-text lyskom-errno)
                            lyskom-errno
                            lyskom-err-stat))))
      (lyskom-insert-string 'interrupted))                   ;+++ lyskom-errno
    ))

(defun lyskom-send-message-trim-newlines ()
  (when (stringp lyskom-message-string)
    (let ((size (length lyskom-message-string)))
      (while (and (> size 0)
                  (eq ?\n (aref lyskom-message-string (1- size))))
        (setq size (1- size)))
      (cond ((and (eq size 0)
                  (not (lyskom-j-or-n-p (lyskom-get-string 
                                         'send-empty-message-p))))
             (setq lyskom-message-string nil))
            ((eq size 0)
             (setq lyskom-message-string ""))
            (t (setq lyskom-message-string (substring lyskom-message-string 
                                                      0 size)))))))

(lyskom-with-external-functions (resize-minibuffer-setup 
				 resize-minibuffer-mode)

  (defun lyskom-send-message-turn-off-resize-on-exit ()
    (resize-minibuffer-mode -1)
    (remove-hook 'kom-send-message-exit-hook
                 'lyskom-send-message-turn-off-resize-on-exit))

  ;; USER-HOOK: lyskom-send-message-resize-minibuffer
  (defvar resize-minibuffer-mode)

  (defun lyskom-send-message-resize-minibuffer ()
    "Temporarily turn on resizing of minibuffer"
    (unless resize-minibuffer-mode
      (resize-minibuffer-mode 1)
      (resize-minibuffer-setup)
      (add-hook 'kom-send-message-exit-hook
                'kom-send-message-turn-off-resize-on-exit)))

  )


;; USER-HOOK: lyskom-send-message-auto-fill
(defun lyskom-send-message-auto-fill ()
  "Temporarily turn on auto fill in minibuffer"
  (setq fill-column 78)                 ;+++ Ta bort?
  (auto-fill-mode 1))



;;; ================================================================
;;;       Endast l{sa senaste - Set unread articles in a conf. 
;;;            (Skip or re-read articles).

;;; Author: Linus Tolke
;;; Rehacked: David K}gedal


(def-kom-command kom-set-unread (&optional arg conf-no)
  "Set number of unread articles in current conference."
  (interactive "P")
  (setq conf-no (or conf-no lyskom-current-conf))
  (when conf-no (cache-del-conf-stat conf-no))
  (if (or (null conf-no) (zerop conf-no))
      (progn
        (lyskom-insert-string 'not-present-anywhere)
        (lyskom-insert-string "\n"))
    (let ((conf-stat (blocking-do 'get-conf-stat conf-no)))
      (if (null conf-stat)
          (lyskom-insert 'somebody-deleted-that-conf)
        (let* ((narg (prefix-numeric-value arg))
               (n (if (and arg
                           (<= 0 narg)
                           (<= narg (conf-stat->no-of-texts conf-stat)))
                      narg
                    (lyskom-read-num-range-or-date 0 (conf-stat->no-of-texts conf-stat)
                                                   (lyskom-format 'only-last
                                                                  (conf-stat->name conf-stat)))))
               (membership nil))
          (cond ((listp n)
                 (lyskom-format-insert 'set-unread-date 
                                       (elt n 0)
                                       (car (rassq (elt n 1) lyskom-month-names))
                                       (elt n 2))
                 (let* ((target-date (lyskom-create-time 0 0 0 (elt n 2) (elt n 1) (elt n 0) 0 0 nil))
                        (text (lyskom-find-text-by-date conf-stat target-date)))
                   (when text
                     (blocking-do 'set-last-read 
                                  (conf-stat->conf-no conf-stat)
                                  (car text)))))
                ((numberp n) 
                 (lyskom-format-insert 'set-unread-n n)
                 (blocking-do 'set-unread conf-no n)))
          (setq membership (blocking-do 'query-read-texts lyskom-pers-no conf-no t 0))
          (lyskom-replace-membership membership)
          (if (= conf-no lyskom-current-conf)
              (set-read-list-empty lyskom-reading-list))
          (read-list-delete-read-info conf-no lyskom-to-do-list)
          (if (= conf-no lyskom-current-conf)
              (progn (lyskom-fetch-start-of-map conf-stat membership)
                     (lyskom-go-to-conf lyskom-current-conf t))
            (lyskom-prefetch-map conf-no membership))
          )))))



(def-kom-command kom-list-sessions ()
  "List current LysKOM sessions and unread messages for each session.

See `kom-session-nickname' for a setting that affects display."
  (interactive)
  (let ((total-letters 0)
	(total-texts 0)
	(total-confs 0)
	(buflist (buffer-list))
	(session-list nil))
    (lyskom-save-excursion
     (lyskom-traverse buf buflist
       (when (lyskom-buffer-p buf)
	 (set-buffer buf)
	 (let ((letters 0)
	       (texts 0)
	       (confs 0))
	   (lyskom-traverse entry (lyskom-list-news)
	     (unless (or (not entry) ; Happens sometimes
			 (zerop (car entry))) ; Ignore confs with 0 unread
	       (setq texts (+ (car entry)
			      texts))
	       (setq confs (1+ confs))
	       (when (= (conf-stat->conf-no (cdr entry))
			lyskom-pers-no)
		 (setq letters (+ (car entry)
				  letters)))))

	   (setq total-texts (+ texts 
				total-texts))
	   (setq total-letters (+ letters
				total-letters))
	   (setq total-confs (+ confs
				total-confs))

	   (setq session-list (append (list (list (lyskom-session-nickname) 
						  (lyskom-format "%#1P" lyskom-pers-no)
						  texts 
						  letters 
						  confs
						  kom-server-priority))
				      session-list))))))

    (setq session-list
	  (sort session-list (lambda (s1 s2)
			       (if (= (nth 5 s1) ; Same priority?
				      (nth 5 s2))
				   (string< (nth 0 s1) 
					    (nth 0 s2)) ; Sort on name
				 (< (nth 5 s1) ; Different priority - sort on that
				    (nth 5 s2))))))

    (lyskom-traverse session session-list
      (if (zerop (nth 2 session))
	(lyskom-format-insert 'session-list-no-unread-in 
			      (nth 0 session))
	(lyskom-format-insert 'session-list-unreads-in-confs
			      (nth 0 session)
			      (nth 3 session)
			      (nth 2 session)
			      (nth 4 session))))))


;;; ================================================================
;;;                 Lista Nyheter - List News

;;; Author:   Linus Tolke
;;; Rehacked: Inge Wallin, Johan Sundström


(defvar lyskom-special-conf-name "\\`\\(Inl.gg .t mig\\|NL:\\)\\'"
  "Regexp to match conf names that are special.")


(defvar lyskom-iter-list-news-total-confs)
(defvar lyskom-iter-list-news-mship-confs)
(defvar lyskom-iter-list-news-shown-unreads)
(defvar lyskom-iter-list-news-shown-confs)
(defvar lyskom-iter-list-news-total-unreads)
(defvar lyskom-iter-list-news-total-confs)

(defun lyskom-iter-list-news (unreads conf-stat at-least at-most)
  "Callback function used to show the number of unread messages of a
conference. It heavily relies on (and destructively modifies) its
environment."
  (when (and (or (not at-least)
                 (>= unreads at-least))	; unreads within lower bound
             (or (not at-most)
                 (<= unreads at-most)))	; unreads within upper bound
    (when lyskom-iter-list-news-mship-confs			; remember all read conferences?
      (setq lyskom-iter-list-news-mship-confs
            (delq (conf-stat->conf-no conf-stat) 
                  lyskom-iter-list-news-mship-confs)))
    (cond
     ((and (boundp 'lyskom-special-conf-name)
           (stringp lyskom-special-conf-name)
           (string-match lyskom-special-conf-name
                         (conf-stat->name conf-stat)))
      (lyskom-format-insert 'you-have-unreads-special unreads conf-stat))
     (t (lyskom-format-insert 'you-have-unreads unreads conf-stat)))
    (setq lyskom-iter-list-news-shown-unreads 
          (+ lyskom-iter-list-news-shown-unreads unreads)
          lyskom-iter-list-news-shown-confs
          (1+ lyskom-iter-list-news-shown-confs)))
  (setq lyskom-iter-list-news-total-unreads 
        (+ lyskom-iter-list-news-total-unreads unreads)
        lyskom-iter-list-news-total-confs
        (1+ lyskom-iter-list-news-total-confs)))


(def-kom-command kom-list-news (&optional num)
  "Print the number of unread texts in each conference.

If the prefix argument is zero, show all conferences, including those
with no unread textts. With a positiv prefix argument, only show
conferences with at least that many unread texts. With a negative
prefix argument, only show conferences with no more than that many
unread texts.

See `kom-allow-incompleteness'."
  (interactive "P")
  (let ((num-arg (cond
                  ((numberp num) num)
                  ((and (listp num)
                        (numberp (car num))) (car num))
                  (t nil)))
        (lyskom-iter-list-news-mship-confs nil)
        (at-least 1)
        (at-most nil)
        (lyskom-iter-list-news-shown-unreads 0)
        (lyskom-iter-list-news-total-unreads 0)
        (lyskom-iter-list-news-shown-confs 0)
        (lyskom-iter-list-news-total-confs 0))
    (when num-arg
      (cond
       ((= num-arg 0)
        (lyskom-traverse-membership el
          (when (not (membership-type->passive (membership->type el)))
            (setq lyskom-iter-list-news-mship-confs
                  (cons (membership->conf-no el)
                        lyskom-iter-list-news-mship-confs))))
        (setq at-least nil
              lyskom-iter-list-news-mship-confs (nreverse lyskom-iter-list-news-mship-confs)))

       ((> num-arg 0)
        (lyskom-format-insert 'list-unread-with-n-unread
                              (setq at-least num-arg)))
       ((< num-arg 0)
        (lyskom-format-insert 'list-unread-with-at-most-n-unread
                              (setq at-most (- num-arg))))))

    ;; The following variables are bound in the callback
    ;; lyskom-iter-list-news-total-unreads
    ;; lyskom-iter-list-news-shown-unreads
    ;; lyskom-iter-list-news-total-confs
    ;; lyskom-iter-list-news-shown-confs
    ;; lyskom-iter-list-news-mship-confs

    (lyskom-list-news 'lyskom-iter-list-news (list at-least at-most))
    (when lyskom-iter-list-news-mship-confs ; then list all read conferences too
      (lyskom-traverse conf-no lyskom-iter-list-news-mship-confs
        (lyskom-format-insert 'you-have-no-unreads conf-no)))

    (if (= 0 lyskom-iter-list-news-total-unreads)
        (lyskom-insert-string 'you-have-read-everything)
      (if (= 0 lyskom-iter-list-news-shown-unreads)
          (lyskom-insert-string 'no-unreads-shown)
        (lyskom-insert "\n")) ; separate the shown list from the summary message
      (when (and
             (> lyskom-iter-list-news-shown-unreads 0)
             (< lyskom-iter-list-news-shown-unreads
                lyskom-iter-list-news-total-unreads))
        (lyskom-format-insert 'shown-unreads
                              lyskom-iter-list-news-shown-unreads
                              lyskom-iter-list-news-shown-confs))
      (lyskom-format-insert 'total-unreads
                            lyskom-iter-list-news-total-unreads 
                            lyskom-iter-list-news-total-confs))))


(defun lyskom-list-news (&optional callback &optional callback-args)
  "With no arguments, returns a list of tuples (unread . conf-stat).
When called with a CALLBACK function, this function is called
iteratively as the list is built up. This function should take the two
arguments `number-of-unread-messages-in-conference' and `conf-stat'
and optionally any other arguments sent in the list CALLBACK-ARGS, and
its return value will form the elements of the list returned from
lyskom-list-news. The callback will only be fed conferences with at
least one unread message in them."

  (unless kom-allow-incompleteness
    (sit-for 0)
    (lyskom-prefetch-all-confs))

  (mapcar
   (function
    (lambda (info)
      (let ((unreads (length (text-list->texts (read-info->text-list info))))
            (conf-stat (read-info->conf-stat info)))
        (when (eq (read-info->type info) 'CONF)
          (if callback
              (apply callback unreads conf-stat callback-args)
            (cons unreads conf-stat))))))
   (read-list->all-entries lyskom-to-do-list)))



;;; ================================================================
;;; 			V{nta - Idle wait


(defun kom-busy-wait (arg)
  "Wait for new texts.
Waiting is interrupted when a text in a conference with higher priority
than that of the next text to be read. If you want another priority to
break that the ones higher that the next text to be read, give the 
priority as a prefix argument. When a text is received the new text
is displayed.

This command is semi-obsolete and may be removed in a future version
of the client.

See `kom-ding-on-wait-done'."
  (interactive "P")
  (lyskom-start-of-command 'kom-busy-wait)
  (unwind-protect
      (let ((waitfor (or (cond
                          ((integerp arg) arg)
                          ((listp arg) (car arg)))
                         (read-info->priority
                          (read-list->first lyskom-to-do-list))
                         -2)))
        (lyskom-tell-server kom-mercial)
        (if (not (read-list-isempty lyskom-reading-list))
            (set-read-list-empty lyskom-reading-list))
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
                    waitfor))
        (while lyskom-is-waiting
          ;; This is a bit trial-and-error stuff at the momemt.
          ;; o How to make personal messages appear *fast*
          ;; o How to enable C-g with a quick response
          (sit-for 0)
          (accept-process-output nil 1)
          (sit-for 0)
          (if lyskom-quit-flag
              (signal 'quit nil))))
    (lyskom-end-of-command))
  ;; We are done waiting
  (lyskom-beep kom-ding-on-wait-done)
  (if (read-list-isempty lyskom-reading-list)
      (kom-go-to-next-conf))
  (kom-next-command))



(defun lyskom-time-greater (time1 time2)
  "Returns t if TIME2 is before TIME1 chronologically."
  (cond
   ((< (time->year time2) (time->year time1)))
   ((> (time->year time2) (time->year time1)) nil)
   ((< (time->mon time2) (time->mon time1)))
   ((> (time->mon time2) (time->mon time1)) nil)
   ((< (time->mday time2) (time->mday time1)))
   ((> (time->mday time2) (time->mday time1)) nil)
   ((< (time->hour time2) (time->hour time1)))
   ((> (time->hour time2) (time->hour time1)) nil)
   ((< (time->min time2) (time->min time1)))
   ((> (time->min time2) (time->min time1)) nil)
   ((< (time->sec time2) (time->sec time1)))
   ((> (time->sec time2) (time->sec time1)) nil)
   (t nil)))


;;; ================================================================
;;;               Lista {rende - list summary

;;; Author: Linus Tolke


(def-kom-command kom-list-summary (prefix)
  "List a summary of the unread texts in the current conference.
The summary contains the date, number of lines, author and subject
of the text on one line.

with a prefix argument, list each unique subject only once."
  (interactive "P")
  (if (read-list-isempty lyskom-reading-list)
      (lyskom-insert-string 'have-to-be-in-conf-with-unread)
    (lyskom-list-summary nil prefix)))

(defun lyskom-list-summary (conf-no &optional unique)
  "List a summary of unread texts in conference CONF-NO.
If CONF-NO is nil, list the first text-list element in lyskom-reading-list.
If UNIQUE is non-nil, list only the first text with a particular subject.

The summary contains the date, number of lines, author and subject of 
the text on one line."
  (let* ((read-info nil)
         (read-list (if conf-no lyskom-to-do-list lyskom-reading-list))
         (len (read-list-length read-list))
         (r 0))
    (while (< r len)
      (if (or (and conf-no 
                   (eq (read-info->type (read-list->nth read-list r)) 'CONF)
                   (eq conf-no (conf-stat->conf-no 
                                (read-info->conf-stat
                                 (read-list->nth read-list r)))))
              (and (null conf-no)
                   (memq (read-info->type (read-list->nth read-list r))
                         '(CONF REVIEW-MARK REVIEW REVIEW-TREE REVIEW-FAQ REVIEW-FAQ-TREE))))
          (setq len 0)
        (setq r (1+ r))))
    (setq read-info (read-list->nth read-list r))
    (when read-info
      (lyskom-list-text-summary 
        (copy-sequence (text-list->texts (read-info->text-list read-info)))
        '(text-no " " written " " lines " " comments " " author " " subject)
        (if unique :unique :comment-order)))))


;;; ============================================================
;;; kom-list-marks                      Lista markeringar
;;; Author: David Byers
;;; Modified by: Joel Rosdahl

(def-kom-command kom-list-marks (&optional which-mark)
  "List texts marked with a particular mark. Use `kom-mark-text' to
mark texts and `kom-unmark-text' to unmark them. A numeric prefix
argument indicated the mark to list. With no prefix argument, you
will be prompted for the mark."
  (interactive "P")
  (when (not (numberp which-mark))
    (setq which-mark (lyskom-read-mark-type
                      (lyskom-get-string 'list-which-mark)
                      t)))

  ;; Start fetching all text-stats and text to list them.

  (lyskom-list-text-summary
    (sort (listify-vector (blocking-do 'get-marks))
          (lambda (a b) (< (mark->mark-type a)
                           (mark->mark-type b))))
    '(mark-type " " text-no " " written " " lines " " mark-count " " author " " subject)
    :filter (lambda (mark which-mark)
              (or (null which-mark)
                  (eq (mark->mark-type mark) which-mark)))
    :filter-args (list which-mark)))


;;; ============================================================
;;;     kom-who-am-i - Vem är jag
;;;
;;; Author: David Byers

(def-kom-command kom-who-am-i ()
  "Show your name and information about your sessions."
  (interactive)
  (if (and lyskom-current-conf
           (not (zerop lyskom-current-conf)))
      (lyskom-format-insert 'who-i-am-present 
                            lyskom-pers-no
                            lyskom-current-conf)
    (lyskom-format-insert 'who-i-am-not-present lyskom-pers-no))

  (lyskom-format-insert 
   'who-i-am-server
   lyskom-server-name
   (version-info->software-version lyskom-server-version-info))

  (lyskom-format-insert 'who-i-am-client
                        lyskom-clientversion
                        lyskom-mule-compiled)

  (lyskom-format-insert 'who-i-am-emacs
                        (emacs-version)
                        enable-multibyte-characters))




;;; ================================================================
;;;          Hj{lp vid del av kommando - Help function

;;; Author: Linus Tolke


(defun lyskom-help (&optional only-kom)
  "Prints a short list of alternatives when you don't know what you can do."
  (interactive)
  (let* ((tohere (cond
                  ((stringp (this-command-keys))
                   (substring (this-command-keys) 0 -1))
                  (t                    ;This is the case in the lucid-emacs.
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
                  (t (if (eq binding lyskom-mode-map)
                         (keymap-parent binding)
                       binding))))
         (keylis (lyskom-help-get-keylist keymap))
         (keydes (mapconcat 'single-key-description tohere " "))
         (text (lyskom-format
                (if (string= keydes "")
                    "\n%#2s\n"
                  "\n%#1s:\n%#2s\n")
                keydes
                (mapconcat
                 'identity
                 (delq nil (mapcar
                            (lambda (arg)
                              (if (or (null (cdr arg)) 
                                      (eq (cdr arg) 'undefined)
                                      (and only-kom
                                           (not (or (lyskom-command-name (cdr arg))
                                                    (keymapp (cdr arg))))))
                                  nil
                                (format "%s - %s" 
                                        (if (fboundp 'key-description)
                                            (if (not (vectorp (car arg)))
                                                (key-description (vector (car arg)))
                                              (key-description (car arg)))
                                          (cond ((symbolp (car arg))
                                                 (format "%s" (car arg)))
                                                ((lyskom-characterp (car arg))
                                                 (format "%c" (car arg)))
                                                (t (format "%S" (car arg)))))
                                        (or (lyskom-command-name (cdr arg))
                                            (and (keymapp (cdr arg))
                                                 (lyskom-get-string
                                                  'multiple-choice))
                                            (cdr arg)))))
                            keylis))
                 "\n")))
         ;; next-char
         )
    (when (interactive-p)
      (if (eq major-mode 'lyskom-mode)
          (progn
            (lyskom-insert text)
            (lyskom-end-of-command))
        (with-output-to-temp-buffer "*Help*"
          (princ text))))
    text))


(defun lyskom-help-get-keylist (keymap)
  (and keymap
       (let (keylist)
         (lyskom-map-keymap
          (lambda (event function)
            (setq keylist (cons (cons event function) keylist)))
          keymap t)
         (nreverse keylist))))


;;; ================================================================
;;;      [ndra livsl{ngd - Set lifespan of texts in a conference

;;; Author: Inge Wallin


(def-kom-command kom-set-garb-nice ()
  "Set the garbage collection time for a conference. Texts in a
conference will eventually be deleted automatically \(this process is
called garbage collection). This can only happen when a text is older
than the garbage collection time of all its recipients."
  (interactive)
  (let ((conf-stat (lyskom-read-conf-stat 'conf-to-set-garb-nice-q
                                          '(all) nil nil t)))
    (if (not conf-stat)
        (lyskom-insert-string 'somebody-deleted-that-conf)
      (let ((garb-nice (lyskom-read-number 'new-garb-nice-q
                                           (conf-stat->garb-nice conf-stat)))
            (keep-commented
             (lyskom-read-number 'new-keep-commented-q
                                 (conf-stat->keep-commented conf-stat))))
        (lyskom-format-insert 'garb-nice-for-is conf-stat garb-nice)
        (if (not (blocking-do 'set-garb-nice
                              (conf-stat->conf-no conf-stat) 
                              garb-nice))
            (lyskom-insert-string 'nope) ;+++lyskom-errno
          (lyskom-insert-string 'done)
          (cache-del-conf-stat (conf-stat->conf-no conf-stat))

          (sit-for 0)
          (lyskom-format-insert 'keep-commented-for-is
                                conf-stat
                                keep-commented)
          (if (not (blocking-do 'set-keep-commented
                                (conf-stat->conf-no conf-stat) 
                                keep-commented))
              (lyskom-insert-string 'nope) ;+++lyskom-errno
            (lyskom-insert-string 'done)
            (cache-del-conf-stat (conf-stat->conf-no conf-stat))))))))


;;; ================================================================
;;;       S{tt till}tna f|rfattare - set-permitted-submitters

;;; Author: Linus Tolke

(def-kom-command kom-set-permitted-submitters ()
  "Set the permitted submitters of a conference. 
The permitted submitters of a conference is another conference. Only
members of the permitted submitters may submit texts to the conference."
  (interactive)
  (let ((conf-stat (lyskom-read-conf-stat 'conf-to-set-permitted-submitters-q
                                          '(all) nil nil t)))

    (if (not conf-stat)
        (lyskom-insert-string 'somebody-deleted-that-conf)
      (let ((new-conf (lyskom-read-conf-stat
                       `(new-permitted-submitters-q 
                         ,(conf-stat->name conf-stat))
                       '(all) t nil t)))
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
  "Set the super conference for a conference.
If a conference is set to only accept new texts, and not comments, any
comments submitted to the conference will be sent to the super
conference instead."
  (interactive)
  (let ((conf-stat (lyskom-read-conf-stat 'conf-to-set-super-conf-q
                                          '(all) nil nil t)))
    (if (not conf-stat)
        (lyskom-insert-string 'somebody-deleted-that-conf)
      (let ((new-conf (lyskom-read-conf-stat
                       `(new-super-conf-q ,(conf-stat->name conf-stat))
                       '(all) nil nil t)))

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
;;;                  Spara databasen - Save database
;;;

(def-kom-command kom-sync-database ()
  "Save the LysKOM database.
You can only run this command if you have administrative rights.

See `kom-enable-adm-caps'."
  (interactive)
  (if (and (>= (version-info->protocol-version lyskom-server-version-info) 8)
           (lyskom-ja-or-nej-p (lyskom-get-string 'really-sync)))
      (progn (lyskom-insert-string 'syncing-server)
             (lyskom-report-command-answer (blocking-do 'sync)))
    (setq lyskom-errno 12)
    (lyskom-report-command-answer nil)))


;;; ================================================================
;;;                  St{ng av servern - Shutdown

;;; Author: Inge Wallin

(def-kom-command kom-shutdown-server ()
  "Shutdown the LysKOM server.
You can only run this command if you have administrative rights.

See `kom-enable-adm-caps'."
  (interactive)
  (if (lyskom-ja-or-nej-p (lyskom-get-string 'really-shutdown))
      (progn
        (lyskom-insert-string 'closing-server)
        (lyskom-report-command-answer (blocking-do 'shutdown 0)))))


;;; ================================================================
;;;       \verg} till adm.mod - Enable administrator capabilities
;;;     \verg} till normalmod - Disable administrator capabilities

;;; Author: Inge Wallin


(def-kom-command kom-enable-adm-caps ()
  "Enable the adminstrator commands for the current user.
You can use this command even if you don't have administrative rights
on the system, but you still won't be able to use privileged commands.

Use `kom-disable-adm-caps' to return to normal mode."
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


(def-kom-command kom-set-motd ()
  "Set the notice for the server.
You can only use this command if you have administrative rights.

See `kom-enable-adm-caps'."
  (interactive)
  (let* ((old-motd-text-stat (and (server-info->motd-of-lyskom lyskom-server-info)
                                 (blocking-do 'get-text-stat (server-info->motd-of-lyskom lyskom-server-info))))
         (old-motd-text (and (server-info->motd-of-lyskom lyskom-server-info)
                             (blocking-do 'get-text (server-info->motd-of-lyskom lyskom-server-info))))
         (str (and old-motd-text 
                   old-motd-text-stat
                   (text->decoded-text-mass old-motd-text old-motd-text-stat)))
         (recpt (if old-motd-text-stat
                    (lyskom-get-recipients-from-misc-list
                     (text-stat->misc-info-list old-motd-text-stat))
                  (apply 'nconc (mapcar (lambda (x) (list 'RECPT x)) 
                                        (and lyskom-server-info
                                             (server-info->kom-news-conf lyskom-server-info)
                                             (not (eq 0 (server-info->kom-news-conf lyskom-server-info)))
                                             (list (server-info->kom-news-conf lyskom-server-info))))))))

    (lyskom-edit-text
     lyskom-proc
     (apply 'lyskom-create-misc-list recpt)
     (if (and str (string-match "\n" str))
         (substring str 0 (1- (match-end 0)))
       "")
     (if (and str (string-match "\n" str))
         (substring str (match-end 0))
       "")
     'lyskom-set-motd-2)))


;; Should really fix lyskom-edit text instead of the ugly IGNORE

(defun lyskom-set-motd-2 (text-no ignore)
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
  "Remove the notice for the LysKOM server.
You can only use this command if you have administrative rights.

See `kom-enable-adm-caps'."
  (interactive)
  (lyskom-insert-string 'removing-motd)
  (initiate-set-motd-of-lyskom 'background 'lyskom-set-motd-3
                               0 0))

;;; ================================================================
;;;                  Kasta ut - force logout

;;; Author: Inge Wallin


(def-kom-command kom-force-logout ()
  "Force another session to log out.
You can log out any sessions logged on as users you are the
supervisor of. With administrative rights you can log out any
user."
  (interactive)
  (let ((session (car-safe (lyskom-read-session-no 'who-to-throw-out
                                                   nil nil t))))
    (cond ((> session 0)
           (lyskom-format-insert 'throwing-out session)
           (lyskom-report-command-answer
            (blocking-do 'disconnect session)))
          ((< session 0)
           (lyskom-format-insert 'person-not-logged-in-r (- session) nil))
          (t nil))))

;;; ================================================================
;;;                  Skjut upp l{sning - postpone

;;; Author: Per Cederqvist


(def-kom-command kom-postpone (today)
  "Postpone the reading of all but the last N texts in the current
conference. The postponed texts will be removed from the read list for
this session but return in your next session or when you do a
`kom-recover'.

A numeric prefix argument is the number of texts to read now. Without
a prefix argument this command will prompt for the number of texts."
  (interactive (list
                (cond
                 ((null current-prefix-arg)
                  (lyskom-read-number 'postpone-prompt kom-postpone-default))
                 (t (prefix-numeric-value current-prefix-arg)))))

  (let ((len (read-list-length lyskom-reading-list))
        (finished nil))
    (while (and (not finished)
                (> len 0))
      (let ((type (read-info->type (read-list->first lyskom-reading-list))))
        (cond 
         ((memq type lyskom-review-types-list)
          (read-list-rotate lyskom-reading-list))
         ((memq type lyskom-comment-types-list)
          (set-read-list-del-first lyskom-reading-list))
         ((eq type 'CONF)
	  (text-list->trim-head
	   (read-info->text-list (read-list->first lyskom-reading-list))
	   today)
          (setq finished t))
         ((eq type 'RE-EDIT-TEXT))
         ((eq type 'PRI-SESSION))
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
  "Set the priority level of the current session and refetch all
memberships. Conferences whose priorities are lower than the session
priority will not be included when reading. This is useful to separate
conferences to read while at work from conferences to not read at
work.

A numeric prefix argument specifies the new session priority. Without
prefix argument, you will be prompted for a priority.

See `kom-default-session-priority'."
  (interactive "P")
  (let ((pri (or priority
                 (lyskom-read-num-range 0
                                255
                                (lyskom-get-string 'set-session-priority)
                                t
                                (or kom-default-session-priority 100)))))
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
    (bury-buffer)
    (while (and (string-match (regexp-quote session-name)
                              (buffer-name (current-buffer)))
                (not (eq buffer (current-buffer))))
      (bury-buffer))))


(defun lyskom-buffer-p (buf &optional may-be-dead)
  "Returns non-nil if BUF is an active LysKOM buffer
If optional second argument MAY-BE-DEAD is non-nil, this function returns t
whether the session is alive or not. Otherwise it checks that the session
is alive."
  (when (buffer-live-p buf)
    (save-excursion
      (set-buffer buf)
      (and (eq major-mode 'lyskom-mode)
           (boundp 'lyskom-proc)
           lyskom-proc
           (processp lyskom-proc)
           (or (and may-be-dead
                    (memq (process-status lyskom-proc) '(run open closed)))
               (memq (process-status lyskom-proc) '(run open)))))))


(defun lyskom-next-kom (buffer-list-name direction)
  "Internal version of kom-next-kom
BUFFER-LIST-NAME is the list of buffers to rotate through. It must be a 
name since this function may modify the list.
DIRECTION should be one of 'forward or 'backward and is the direction to
rotate through the buffer list.
Return-value: 'no-session if there is no suitable session to switch to
              'same-session if the current buffer is the only suitable one
              nil if everything went well"

  ;; Clean the buffer lists 

  (lyskom-clean-all-buffer-lists)

  (let ((current (lyskom-buffer-root-ancestor (current-buffer)))
        (buffer-list (symbol-value buffer-list-name))
        (result nil))

    ;; Figure out the start buffer. The target buffer will be the
    ;; following buffer in lyskom-buffer-list

    (cond
     ((null buffer-list) (setq result 'no-session
                               current nil))

     ;; If we are in a lyskom buffer that is in lyskom-buffer-list
     ;; switch from the current buffer. If unlisted, list it.
     ((lyskom-buffer-p current) 
      (unless (memq current buffer-list)
        (setq buffer-list (cons current buffer-list))))

     ;; If we are in a non-LysKOM buffer, the start buffer is the
     ;; last one in the list
     (t (setq current (car buffer-list))))

    ;; Rotate the buffer list so the target buffer is first

    (when current
      (cond ((eq direction 'forward)
             (setq buffer-list 
                   (lyskom-rotate-list buffer-list
                                       (car (cdr (memq current buffer-list))))))
            (t
             (setq buffer-list
                   (lyskom-rotate-list buffer-list
                                       (or (car (lyskom-preceding-cons 
                                                 buffer-list
                                                 current))
                                           (car (last buffer-list)))))))
      (set buffer-list-name buffer-list)

      ;; If the current buffer and the target buffer are the same
      ;; do nothing. Otherwise, flip around the buffers

      (if (eq (current-buffer) (car buffer-list))
          (setq result 'same-session)
        (lyskom-switch-to-kom-buffer (car buffer-list) current)
        (setq result nil)))
    result))

(def-kom-emacs-command kom-next-kom ()
  "Pop up the next LysKOM-session.
This command can be run from any buffer to go to the next available
LysKOM session.

See `kom-previous-kom' and `kom-next-unread-kom' for related commands."
  (interactive)
  (let ((result (lyskom-next-kom 'lyskom-buffer-list 'forward)))
    (cond ((eq result 'no-session)
           (error (lyskom-get-string 'no-lyskom-session)))
          ((eq result 'same-session)
           (if kom-next-kom-running-as-kom-command
               (lyskom-insert-before-prompt (lyskom-get-string
                                             'no-other-lyskom-r))
             (error (lyskom-get-string 'no-lyskom-session))))
          (t nil))))

(def-kom-emacs-command kom-previous-kom ()
  "Pop up the previous LysKOM-session.
This command can be run from any buffer to go to the next available
LysKOM session.

See `kom-next-kom' and `kom-next-unread-kom' for related commands."
  (interactive)
  (let ((result (lyskom-next-kom 'lyskom-buffer-list 'backward)))
    (cond ((eq result 'no-session)
           (error (lyskom-get-string 'no-lyskom-session)))
          ((eq result 'same-session)
           (if kom-previous-kom-running-as-kom-command
               (lyskom-insert-before-prompt (lyskom-get-string
                                             'no-other-lyskom-r))
             (error (lyskom-get-string 'no-lyskom-session))))
          (t nil))))


(def-kom-emacs-command kom-next-unread-kom ()
  "Pop up the next LysKOM-session with unread texts.
This command can be run from any buffer to go to the next available
LysKOM session.

See `kom-next-kom' and `kom-previous-kom' for related commands."
  (interactive)
  (let ((result (lyskom-next-kom 'lyskom-sessions-with-unread 'forward)))
    (cond ((eq result 'no-session)
           (if kom-next-unread-kom-running-as-kom-command
               (lyskom-insert-before-prompt (lyskom-get-string 
                                             'no-unread-lyskom-r))
             (error (lyskom-get-string 'no-unread-lyskom))))
          ((eq result 'same-session)
           (if kom-next-unread-kom-running-as-kom-command
               (lyskom-insert-before-prompt
                (lyskom-get-string
                 ;; Are there are any other sessions at all?
                 (if (= 1 (length lyskom-buffer-list))
                     'no-other-lyskom-r
                   'no-other-unread-lyskom-r)))
             (error (lyskom-get-string 'no-lyskom-session))))
          (t nil))))

(defun kom-modeline-next-unread-kom ()
  "Pop up the previous unread lyskom session, if there is one"
  (interactive)
  (lyskom-next-kom 'lyskom-sessions-with-unread 'forward))

(defun kom-modeline-select-unread-kom (event)
  "Pop up a menu of sessions with unreads"
  (interactive "@e")
  (when lyskom-sessions-with-unread
    (let ((unreads
           (mapcar (lambda (buffer)
                     (save-excursion
                       (set-buffer buffer)
                       (vector
                        (lyskom-format "%#1P, %#2s%#3?b%[ (%#4s)%]%[%]"
                                       lyskom-pers-no
                                       (lyskom-session-nickname)
                                       (memq buffer lyskom-sessions-with-unread-letters)
                                       (lyskom-get-string 'unread-letters))
                        (list 'lyskom-switch-to-kom-buffer buffer)
                        :active t)))
                   lyskom-sessions-with-unread)))
      (popup-menu
       (cons (lyskom-get-string 'sessions-with-unreads) unreads)
       event))))



(defun lyskom-switch-to-kom-buffer (buffer &optional current)
  (when buffer
    (when (and kom-bury-buffers
               (or (null current)
                   (eq current (current-buffer))))
      (kom-bury))

    ;; Switch to the new buffer
    (switch-to-buffer buffer)))






;;; ============================================================
;;; Var finns kommandot                 (kom-where-is)
;;; Author: David Byers

(def-kom-emacs-command kom-where-is (cmd)
  "Show on which key a LysKOM command is. Prompts for a command name
and displays all key bindings for the command. This is similar to the
Emacs function `where-is', but reads localized LysKOM command names,
not function names."
  (interactive (list (lyskom-read-extended-command)))
  (let ((w (where-is-internal cmd))
        (msg nil))
    (cond ((null cmd)
           (setq msg (lyskom-format (lyskom-get-string 'where-is-doesnt-exist)
                                    (lyskom-command-name cmd))))
          ((null w)
           (setq msg (lyskom-format (lyskom-get-string 'where-is-on-no-key)
                                    (lyskom-command-name cmd))))
          (t (setq msg (lyskom-format (lyskom-get-string 'where-is-on-key)
                                      (lyskom-command-name cmd)
                                      (mapconcat
                                       (lambda (x) 
                                         (format "`%s'" (key-description x)))
                                       w ", ")))))
    (if kom-where-is-running-as-kom-command
        (lyskom-insert-before-prompt (concat msg "\n"))
      (message msg))))

;;;============================================================
;;;  Visa user-arean                    (kom-show-user-area)
;;;
;;;  Author: David Byers

(def-kom-command kom-show-user-area ()
  "Get and display the user area of the current person. The user area
is a regular text used to store all settings. The format of this text
is documented in the LysKOM protocol specification. This command is
primarily intended for use by developers."
  (interactive)
  (let ((pers-stat (blocking-do 'get-pers-stat lyskom-pers-no)))
    (lyskom-view-text (pers-stat->user-area pers-stat)
                      nil nil nil nil nil)
    (lyskom-wait-queue 'main)))

(def-kom-command kom-delete-user-area ()
  "Remove the user area of the current person. The user area
is where all settings are stored. Deleting it means all settings
are lost. Do not delete the user area unless you're sure you really
want to."
  (interactive)
  (lyskom-format-insert 'delete-user-area-warning
			`(face ,kom-warning-face))
  (when (lyskom-ja-or-nej-p (lyskom-get-string 'delete-user-area-confirm))
    (let* ((pers-stat (blocking-do 'get-pers-stat lyskom-pers-no))
	   (user-area (pers-stat->user-area pers-stat)))
      (lyskom-insert 'deleting-user-area)
      (lyskom-report-command-answer (blocking-do 'delete-text user-area))
      (lyskom-insert 'removing-user-area)
      (lyskom-report-command-answer (blocking-do 'set-user-area lyskom-pers-no 0))
      )))
  


;;;============================================================
;;; Bli anonym

(def-kom-command kom-become-anonymous ()
  "Become pseudo-anonymous. 

When this mode is in effect, your movements in LysKOM will not be
reported as usual, and when writing texts you will have the option of
sending them anonymously. Depending on what you do, it may still be
possible to deduce that you are the author of an anonymous text.

To summarize, texts you create while in this mode should be difficult
to trace back to you, if you are careful. It is not foolproof.

Use `kom-become-nonanonymous' to return to normal mode."
  (interactive)
  (if lyskom-is-anonymous
      (lyskom-insert 'you-are-already-anonymous)
;    (initiate-pepsi 'main nil 0)
    (setq lyskom-is-anonymous t)
    (lyskom-tell-server kom-mercial)
    (lyskom-insert 'you-are-anonymous)
    (lyskom-update-prompt t)))

(def-kom-command kom-become-nonanonymous ()
  "Leave pseudo-anonymous mode.

See `kom-become-anonymous' for information on anonymous mode."
  (interactive)
  (if lyskom-is-anonymous
      (progn (when (and lyskom-current-conf 
                        (not (zerop lyskom-current-conf)))
               (initiate-pepsi 'main nil lyskom-current-conf))
             (setq lyskom-is-anonymous nil)
             (lyskom-update-prompt t)
             (lyskom-insert 'you-are-nonanonymous))
    (lyskom-insert 'you-are-already-nonanonymous)))

;;;============================================================
;;;   Ändra mötestyp                    (kom-change-conf-type)
;;;
;;;   Author: Tomas Abrahamsson & David Byers

(def-kom-command kom-change-conf-type ()
  "Change type of a conference.
Using this command you can set all flags of a conference, with
the exception of the letterbox flag (which cannot be modified)."
  (interactive)
  (let* ((uconf-stat (lyskom-read-uconf-stat 'what-conf-to-change
                                             '(conf pers) nil "" t))
         (type (uconf-stat->conf-type uconf-stat))
         (box (conf-type->letterbox type))
         (ori (conf-type->original type))
         (pro (conf-type->rd_prot type))
         (sec (conf-type->secret type))
         (ano (conf-type->anarchy type))
         (ope (conf-type->forbid-secret type)))
    (lyskom-format-insert 
     'change-type-prompt
     uconf-stat
     (mapconcat 'identity
                (delq nil
                      (list (and box (lyskom-get-string 'Mailbox))
                            (and sec (lyskom-get-string 'Protected))
                            (and ori (lyskom-get-string 'no-comments))
                            (and pro (lyskom-get-string 'closed))
                            (and ano (lyskom-get-string 'allow-anon))
                            (and (not ope) (lyskom-get-string 'allow-secret))))
                ", "))
    (let* ((open (lyskom-j-or-n-p (lyskom-get-string 'anyone-member)))
           (secret (if (not open)
                       (lyskom-j-or-n-p (lyskom-get-string 'secret-conf))))
           (orig (lyskom-j-or-n-p (lyskom-get-string 'comments-allowed)))
           (anarchy (lyskom-j-or-n-p (lyskom-get-string 'anonymous-allowed)))
           (secmem (and (lyskom-have-feature long-conf-types)
                        (not (lyskom-j-or-n-p (lyskom-get-string 'secret-members-allowed))))))
      (cache-del-conf-stat (uconf-stat->conf-no uconf-stat))
      (cache-del-uconf-stat (uconf-stat->conf-no uconf-stat))
      (if (not (blocking-do 
                'set-conf-type
                (uconf-stat->conf-no uconf-stat)
                (lyskom-create-conf-type (not open)
                                         (not orig)
                                         secret
                                         (conf-type->letterbox
                                          (uconf-stat->conf-type  uconf-stat))
                                         anarchy
                                         (if (lyskom-have-feature long-conf-types)
                                             secmem
                                           (conf-type->forbid-secret
                                            (uconf-stat->conf-type uconf-stat)))
                                         (conf-type->rsv2
                                          (uconf-stat->conf-type uconf-stat))
                                         (conf-type->rsv3
                                          (uconf-stat->conf-type uconf-stat)))))
          (progn (lyskom-insert-string 'nope)
                 (lyskom-insert-error))
        (lyskom-insert-string 'done)))))



;;; ============================================================
;;; Ändra språk
;;;

(defun kom-change-global-language ()
  "Use kom-change-language instead."
  (interactive)
  (kom-change-language t))

(defun kom-change-local-language ()
  "Use kom-change-language instead"
  (interactive)
  (kom-change-language))

(def-kom-command kom-change-language (&optional global)
  "Change the current language in the current LysKOM session.
With a prefix argument, also make changes that would affect all 
sessions, such as key bindings.

The selected language is not saved between sessions. To permanently
set and save language settings. use `kom-customize' instead.

See `kom-default-language'.

Lisp documentation:
The optional argument GLOBAL indicates that the change should have a
global effect, including changes to key binding."
  (interactive "P")
  (let* ((completion-ignore-case t)
         (table (lyskom-available-language-list))
         (language (lyskom-completing-read
                    (lyskom-get-string 'which-language)
                    (lyskom-maybe-frob-completion-table table)
                    nil
                    t
                    nil
                    'lyskom-language-history)))
    (when (lyskom-string-assoc language table)
      (lyskom-set-language (cdr (lyskom-string-assoc language table)) 'local)
      (when global
        (lyskom-set-language (cdr (lyskom-string-assoc language table)) 'global))

      (lyskom-format-insert 
       'language-set-to 
       (lyskom-language-name (cdr (lyskom-string-assoc language table)))))))



(defun lyskom-available-language-list ()
  "Return an alist suitable for completing read of available language names."
  (let ((tmp 
         (mapcar
          (function
           (lambda (el)
             (cons (car el) (eval (cdr el)))))
          (get 'lyskom-language-codes 'lyskom-language-var)))
        (codes (mapcar 'car lyskom-languages))
        (result nil))
    (mapcar 
     (function
      (lambda (code)
        (mapcar 
         (function
          (lambda (codelist)
            (when (assq code codelist)
              (setq result
                    (cons (cons (cdr (assq code codelist))
                                code)
                          result)))))
         tmp)))
     codes)
    result))


;;; ============================================================
;;; Beräkna

(lyskom-with-external-functions (calc-eval)

  (def-kom-command kom-calculate (&optional exprx)
    "Calculate a mathematical expression.
This function requires the `calc' package to be installed, and is
really only a simple interface to the basic functionality of calc."
    (interactive)
    (when (lyskom-try-require 'calc 
                              (lyskom-get-string 'need-library))
      (let* ((expr (or exprx
                       (lyskom-read-from-minibuffer 
                        (lyskom-get-string 'calc-expression)
                        nil nil nil 'lyskom-expression-history)))
             (result (calc-eval expr)))
        (cond ((stringp result)
               (lyskom-format-insert-before-prompt
                "%#1s = \n    %#2s\n" expr result))
              (t (lyskom-format-insert-before-prompt
                  "%#1s = \n%#2s^ %#3s\n"
                  expr
                  (make-string (car result) ?\ )
                  (car (cdr result)))))))))

;;; ============================================================
;;; Ändra namn

(def-kom-command kom-set-personal-label ()
  "This command is obsolete, broken and doesn't work. Don't use it.
Sets a personal label on an object of some kind."
  (interactive)
  (let* ((completions (list (cons (lyskom-get-string 'Conference) 'conf)
                            (cons (lyskom-get-string 'Person) 'pers)
                            (cons (lyskom-get-string 'Text) 'text)))
         (completion-ignore-case t)
         (type (cdr (lyskom-string-assoc
                     (completing-read (lyskom-get-string 'label-what-kind)
                                      (lyskom-maybe-frob-completion-table 
                                       completions)
                                      nil t)
                     completions)))
         (objno nil)
         (label nil)
         (object nil)
         (secret nil)
         (aux nil))
    (cond ((eq type 'text)
           (setq objno (lyskom-read-number 'label-what-text))
           (setq object (blocking-do 'get-text-stat objno))
           (when (null object)
             (lyskom-error 'no-such-text-no objno))
           (setq aux (text-stat-find-aux object 10 lyskom-pers-no))
           (setq secret (not (lyskom-j-or-n-p 
                              (lyskom-get-string 'label-secret))))
           (setq label
                  (lyskom-read-from-minibuffer 
                   (lyskom-get-string 'label-what-label)))
           (blocking-do 
            'modify-text-info
            objno
            (mapcar 'aux-item->aux-no aux)
            (list
             (lyskom-create-aux-item 0 10 0 0 
                              (lyskom-create-aux-item-flags
                               nil nil secret nil nil nil nil nil)
                              0 label)))
           (cache-del-text-stat objno))

          ((memq type '(conf pers))
           (setq object
                 (lyskom-read-conf-stat (if (eq type 'pers)
                                            'label-what-pers
                                          'label-what-conf)
                                        (if (eq type 'pers)
                                            '(pers)
                                          '(all))
                                         nil nil t))
           (setq objno (conf-stat->conf-no object))
           (setq aux (conf-stat-find-aux object 10 lyskom-pers-no))
           (setq secret (not (lyskom-j-or-n-p
                              (lyskom-get-string 'label-secret))))
           (setq label
                 (lyskom-read-from-minibuffer 
                  (lyskom-get-string 'label-what-label)))
           (blocking-do 
            'modify-conf-info
            objno
            (mapcar 'aux-item->aux-no aux)
            (list
             (lyskom-create-aux-item 0 10 0 0 
                              (lyskom-create-aux-item-flags
                               nil nil secret nil nil nil nil nil)
                              0 label)))
           (cache-del-conf-stat objno)))))


(def-kom-command kom-fast-reply (text-no)
  "Add a remark to a text.
Note that remarks may not be seen by all users. Users are not notified
when remarks are created, and not all clients support remarks.

See `kom-agree' for a variant of this command.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-fast-reply-no)))
  (if text-no
      (progn (lyskom-format-insert 'fast-replying text-no)
             (lyskom-fast-reply text-no
                                (lyskom-read-string 
                                 (lyskom-get-string 'fast-reply-prompt)
                                 nil
                                 'lyskom-fast-reply-history)))
    (lyskom-insert-string 'confusion-what-to-reply-to)))



(defun lyskom-default-agree-string (&optional text)
  (unless text (setq text kom-agree-text))
  (cond ((null text) (lyskom-get-string 'default-agree-string))
        ((stringp text) text)
        ((functionp text) (funcall text))
        ((listp text) (lyskom-default-agree-string
                       (elt text (random (length kom-agree-text)))))))

(def-kom-command kom-agree (text-no)
  "Add a predefined remark to a text.
Note that remarks may not be seen by all users. Users are not notified
when remarks are created, and not all clients support remarks.

The remark to add is defined by `kom-agree-text'.

See `kom-fast-reply' for a general variant of this command.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-agree-no)))
  (if text-no
      (progn (lyskom-format-insert 'agreeing text-no)
             (lyskom-fast-reply text-no
                                (lyskom-read-string
                                 (lyskom-get-string 'agree-prompt)
                                 (lyskom-default-agree-string)
                                 'lyskom-fast-reply-history)))
    (lyskom-insert-string 'confusion-what-to-agree-to)))


(defun lyskom-fast-reply (text-no message)
  "To text TEXT-NO add MESSAGE as a fast reply."
  (cache-del-text-stat text-no)
  (if (lyskom-check-fast-reply-message message)
      (lyskom-report-command-answer
       (blocking-do 'modify-text-info
                    text-no
                    nil
                    (list (lyskom-create-aux-item 0 2 0 0 
                                                  (lyskom-create-aux-item-flags
                                                   nil nil nil nil nil nil nil nil)
                                                  0 message))))
    (lyskom-insert 'nope)
    (lyskom-insert 'fast-reply-too-long)))

(defun lyskom-check-fast-reply-message (message)
  "Return non-nil if MESSAGE is a valid fast reply."
  (not (string-match "\n" message)))


;;; ============================================================
;;; Various aux-item stuff

(def-kom-command kom-add-no-comments (&optional text-no)
  "Add a request for no comments to the selected text. 

Such a request is advisory; clients may ignore them. Be restrictive
with requests for no comments as other users may find them annoying or
even insulting \(they can be seen as the LysKOM equivalent of telling
someone to shut up and get out after making them listen to you).

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-no-comments-no)))
  (let ((text-stat (blocking-do 'get-text-stat text-no)))

    ;; Make sure there is a text there in the first place

    (if (null text-stat)
        (lyskom-format-insert 'no-such-text-no text-no)

      ;; Make sure that the text doesn't already have this kind of item
      ;; created by the same person

      (if (lyskom-match-aux-items (text-stat->aux-items text-stat) 
                                  (lambda (el) 
                                    (and (eq (aux-item->tag el) 4)
                                         (eq (aux-item->creator el)
                                             lyskom-pers-no))))
          (lyskom-format-insert 'already-no-comments text-no)

        ;; If the author of the text is not the current user, ask if the
        ;; user wants to try anyway (it might work...)

        (if (or (eq (text-stat->author text-stat) lyskom-pers-no)
                (lyskom-j-or-n-p 'not-author-try-anyway-p))
            (progn (lyskom-format-insert 'adding-no-comments
                                         text-no)
                   (lyskom-report-command-answer
                    (blocking-do 'modify-text-info
                                 text-no
                                 nil
                                 (list
                                  (lyskom-create-aux-item 
                                   0 4 nil nil
                                   (lyskom-create-aux-item-flags
                                    nil nil nil nil nil nil nil nil) 0 ""))))
                   (cache-del-text-stat text-no)))))))


(def-kom-command kom-add-private-answer (text-no)
  "Add a request for private replies only to the selected text.
Note that such requests are advisory; clients may ignore them.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-private-answer-no)))
  (if text-no
      (let ((text-stat (blocking-do 'get-text-stat text-no)))

        ;; Make sure there is a text there in the first place

        (if (null text-stat)
            (lyskom-format-insert 'no-such-text-no text-no)

          ;; Make sure that the text doesn't already have this kind of item
          ;; created by the same person

          (if (lyskom-match-aux-items (text-stat->aux-items text-stat) 
                                      (lambda (el) 
                                        (and (eq (aux-item->tag el) 5)
                                             (eq (aux-item->creator el)
                                                 lyskom-pers-no))))
              (lyskom-format-insert 'already-private-answer text-no)

            ;; If the author of the text is not the current user, ask if the
            ;; user wants to try anyway (it might work...)

            (if (or (eq (text-stat->author text-stat) lyskom-pers-no)
                    (lyskom-j-or-n-p 'not-author-try-anyway-p))
                (progn (lyskom-format-insert 'adding-private-answer
                                             text-no)
                       (lyskom-report-command-answer
                        (blocking-do 'modify-text-info
                                     text-no
                                     nil
                                     (list
                                      (lyskom-create-aux-item 
                                       0 5 nil nil
                                       (lyskom-create-aux-item-flags
                                        nil nil nil nil nil nil nil nil) 0 ""))))
                       (cache-del-text-stat text-no))))))
    (lyskom-insert 'confusion-what-to-comment)))

(def-kom-command kom-add-request-confirm (text-no)
  "Add confirmation request to the selected text. 
Note that such requests are advisory; clients may ignore them. Use
confirmation requests very sparingly. If unmotivated use of them
becomes widespread, then they will be ignored even when they are used
appropriately.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-request-confirm-no)))
  (if text-no
      (let ((text-stat (blocking-do 'get-text-stat text-no)))

        ;; Make sure there is a text there in the first place

        (if (null text-stat)
            (lyskom-format-insert 'no-such-text-no text-no)

          ;; Make sure that the text doesn't already have this kind of item
          ;; created by the same person

          (if (lyskom-match-aux-items (text-stat->aux-items text-stat) 
                                      (lambda (el) 
                                        (and (eq (aux-item->tag el) 6)
                                             (eq (aux-item->creator el)
                                                 lyskom-pers-no))))
              (lyskom-format-insert 'already-request-confirm text-no)

            ;; If the author of the text is not the current user, ask if the
            ;; user wants to try anyway (it might work...)

            (if (or (eq (text-stat->author text-stat) lyskom-pers-no)
                    (lyskom-j-or-n-p 'not-author-try-anyway-p))
                (progn (lyskom-format-insert 'adding-request-confirm
                                             text-no)
                       (lyskom-report-command-answer
                        (blocking-do 'modify-text-info
                                     text-no
                                     nil
                                     (list
                                      (lyskom-create-aux-item 
                                       0 6 nil nil
                                       (lyskom-create-aux-item-flags
                                        nil nil nil nil nil nil nil nil) 0 ""))))
                       (cache-del-text-stat text-no))))))
    (lyskom-insert 'confusion-what-to-request-confirmation)))

(def-kom-command kom-review-mail-headers (text-no)
  "Show all mail headers of an imported message. Mail headers are also
shown when you use the `kom-review-noconversion' command.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 
                      'review-mail-headers-to-what)))
  (if text-no
      (let* ((text-stat (blocking-do 'get-text-stat text-no))
             (headers (and text-stat (lyskom-get-aux-item (text-stat->aux-items text-stat) 24)))
             (lyskom-transforming-external-text t))
        (cond ((null text-stat) (lyskom-format-insert 'no-such-text-no text-no))
              ((null headers) (lyskom-format-insert 'no-mail-headers text-no))
              (t (lyskom-format-insert 'mail-headers-for text-no)
                 (mapcar (lambda (el) 
                           (let ((kom-autowrap nil))
                             (lyskom-format-insert "%#1t" (aux-item->data el))
                             (lyskom-insert "\n")))
                         headers))))
    (lyskom-insert 'confusion-what-to-review-mail-headers)))


;;; ============================================================
;;; Keep-alive
;;; Author: ceder, byers


(def-kom-var lyskom-keep-alive-timers nil
  "List of all active keep alive timers"
  local)

(defun lyskom-keep-alive-callback (buffer)
  (condition-case nil
    (save-excursion (set-buffer buffer)
                    (if (eq (process-status lyskom-proc) 'open)
                        (initiate-get-time 'keep nil)
                      (lyskom-stop-keep-alive)))
    (error (lyskom-stop-keep-alive))))

(def-kom-command kom-keep-alive ()
  "Keep the LysKOM session alive by sending a request every once in a
while. The variable `kom-keep-alive-interval' controls the frequency
of the request. This command may be useful for users with brain-dead
broadband access that disconnects when the line has been inactive for
too long.

Use `kom-stop-keep-alive' to turn off this mode."
  (interactive)
  (set-buffer lyskom-buffer)
  (lyskom-stop-keep-alive)
  (lyskom-keep-alive)
  (unless (eq (current-buffer) lyskom-buffer)
    (lyskom-message 'start-keep-alive kom-keep-alive-interval))
  (lyskom-format-insert 'start-keep-alive kom-keep-alive-interval)
  (lyskom-insert "\n"))

(defun lyskom-keep-alive ()
  (setq lyskom-keep-alive-timers
        (cons
         (add-timeout kom-keep-alive-interval
                      'lyskom-keep-alive-callback
                      (current-buffer)
                      kom-keep-alive-interval)
         lyskom-keep-alive-timers)))

(def-kom-command kom-stop-keep-alive ()
  "Stop sending periodic requests to keep the session alive.

See `kom-keep-alive' for more information."
  (interactive)
  (lyskom-stop-keep-alive)
  (unless (eq (current-buffer) lyskom-buffer)
    (lyskom-message "%#1s" 'stop-keep-alive))
  (lyskom-insert 'stop-keep-alive)
  (lyskom-insert "\n"))


(defun lyskom-stop-keep-alive ()
  (mapcar 'disable-timeout lyskom-keep-alive-timers)
  (setq lyskom-keep-alive-timers nil))



;;; ========================================================================
;;;   Check (if Person is a) member (of Conference)
;;;   Quickly finds out whether a person is a member of a given conference

;;; Author: Johan Sundström

(def-kom-command kom-is-person-member-of-conference (&optional pers-no conf-no)
  "Check if a particular person is a member of a particular conference."
  (interactive)
  (let* ((pers-no
          (or pers-no
              (lyskom-read-conf-no 'pers-to-check-mship-for
                                   '(pers) nil nil t)))
         (conf-stat
          (if conf-no 
              (blocking-do 'get-conf-stat conf-no)
            (lyskom-read-conf-stat 'conf-to-check-mship-of
                                   '(all) nil nil t)))
	 (mship (lyskom-is-member (conf-stat->conf-no conf-stat) pers-no)))
    (if (null mship)
        (lyskom-format-insert 'pers-is-not-member-of-conf pers-no (conf-stat->conf-no conf-stat))
      (if (membership-type->passive (membership->type mship))
          (progn
            (lyskom-format-insert 'pers-is-passive-member-of-conf
                                  pers-no conf-stat)
            (lyskom-format-insert 'pers-will-receive-async
                                  (membership-type->message-flag
                                   (membership->type mship))))
        (lyskom-format-insert 'pers-is-member-of-conf pers-no conf-stat)
        (lyskom-format-insert 'pers-will-receive-async
                              (membership-type->message-flag
                               (membership->type mship)))
        (lyskom-format-insert 'pers-mship-priority
                              (membership->priority mship))
        (when kom-deferred-printing
          (lyskom-format-insert 
           'pers-is-member-of-conf-2
           (lyskom-format-time
            'date-and-time
            (membership->last-time-read mship))
           (lyskom-create-defer-info
            'query-read-texts
            (list pers-no (conf-stat->conf-no conf-stat) t 0)
            (lambda (membership defer-info)
              (if (null membership)
                  (lyskom-replace-deferred 
                   defer-info (lyskom-get-string 'Unknown-number))
                (let ((conf-stat (defer-info->data defer-info)))
                  (lyskom-replace-deferred defer-info 
                                           (number-to-string 
                                            (- (+ (conf-stat->first-local-no conf-stat)
                                                  (conf-stat->no-of-texts conf-stat))
                                               (membership->last-text-read membership)
                                               (length (membership->read-texts membership))
                                               1))))))
            nil nil "%#1s" conf-stat))))
      )))

(def-kom-command kom-will-person-read-text (pers-no text-no)
  "Check if a particular person is a member of any recipient of a text..
If a prefix argument is given, that text will be checked.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-conf-no 'pers-to-check-will-read-for
                                          '(all) nil nil t)
                     (lyskom-read-text-no-prefix-arg 'text-to-check-will-read-for)))
  (let* ((text-stat (blocking-do 'get-text-stat text-no))
         (recipients (and text-stat (lyskom-text-recipients text-stat)))
         (result nil))
    (lyskom-traverse rcpt recipients
      (let ((mship (lyskom-is-member rcpt pers-no)))
        (when mship
          (if (membership-type->passive (membership->type mship))
              (setq result 'passive)
            (setq result t)
            (lyskom-traverse-break)))))

    (cond ((null result)
           (lyskom-format-insert 'pers-is-not-member-of-rcpt
                                 pers-no text-stat))
          ((eq result 'passive)
           (lyskom-format-insert 'pers-is-passive-member-of-rcpt
                                 pers-no text-stat))
          (t (lyskom-format-insert 'pers-is-member-of-rcpt
                                   pers-no text-stat)))))


;;; ================================================================
;;; Help

(def-kom-command kom-help (&optional section)
  "Run the built-in help system."
  (interactive)
  (let* ((alternatives (delq nil
                             (mapcar (lambda (section)
                                       (unless (string= "" (elt section 1))
                                         (cons (elt section 1) 
                                               (elt section 0))))
                                     lyskom-help-data)))
         (completion-ignore-case t)
         (section section))
    (while (null section)
      (setq section
            (lyskom-string-assoc
             (lyskom-completing-read (lyskom-get-string 'help-with-what)
                                     alternatives
                                     nil
                                     t
                                     nil
                                     'lyskom-help-history)
             alternatives)))
    (lyskom-format-insert 'help-for (car section))
    (lyskom-help-show-section (cdr section))))


(def-kom-command kom-make-review-mark-as-read ()
  "Makes all review commands mark texts as read. Overrides the value
of the configurable variable `kom-review-marks-texts-as-read' in the
current buffer."
  (interactive)
  (setq kom-review-marks-texts-as-read t))

(def-kom-command kom-make-review-not-mark-as-read ()
  "Makes all review commands not mark texts as read. Overrides the
value of the configurable variable `kom-review-marks-texts-as-read' in
the current buffer."
  (interactive)
  (setq kom-review-marks-texts-as-read nil))

;;; ================================================================
;;;             Jämför två texter - Compare two texts
;;; Run ediff-buffers on two texts.

;;; Author: Per Cederqvist


(defun lyskom-create-text-buffer (text-no text text-stat)
  "Create and return a buffer containing TEXT."
  (let ((buf (generate-new-buffer (format "%d" text-no))))
    (set-buffer buf)
    (insert (text->decoded-text-mass text text-stat))
    ;; Add a terminating newline.  Ediff works better that way, and it
    ;; should not harm any other applications of this function.
    (insert "\n")
    (set-buffer-modified-p nil)
    buf))

(defvar diff-command)
(defvar ediff-diff-program)
(def-kom-command kom-compare-texts (old new)
  "Show differences between two texts, OLD and NEW.

When called interactively, it will prompt for the NEW text first,
defaulting to the last viewed texts.  The OLD text number will default
to the first text that NEW is a comment or footnote to.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg') for the NEW text."
  (interactive
   (let* ((n (lyskom-read-text-no-prefix-arg 'diff-what-text-new))
	  (new-stat (blocking-do 'get-text-stat n))
	  (o (lyskom-read-number
	      'diff-what-text-old
	      (if (null new-stat)
		  (lyskom-error (lyskom-get-string 'no-such-text-no n))
		(car (lyskom-text-stat-commented-texts new-stat))))))
     (list o n)))
  (blocking-do-multiple ((old-text (get-text old))
			 (new-text (get-text new))
			 (old-text-stat (get-text-stat old))
			 (new-text-stat (get-text-stat new)))
    (cond
     ((or (null old-text) (null old-text-stat))
      (lyskom-format-insert 'no-such-text-no old))
     ((or (null new-text) (null new-text-stat))
      (lyskom-format-insert 'no-such-text-no new))
     (t
      (condition-case nil
	  (ediff-buffers
	   (lyskom-create-text-buffer old old-text old-text-stat)
	   (lyskom-create-text-buffer new new-text new-text-stat))
	(file-error (lyskom-error (lyskom-get-string 'external-program-missing)
				  (cond ((boundp 'ediff-diff-program)
					 ediff-diff-program)
					((boundp 'diff-command) diff-command)
					(t "diff (gissningsvis)")))))))))

;;; ================================================================
;;;             Se diff - View diff
;;; Run diff on two texts and insert the result.

;;; Author: Per Cederqvist


(defun lyskom-create-temp-file (text-no text text-stat)
  "Create a temporary file containing TEXT, and return its file name."
  (let ((file (make-temp-file (format "kom-diff-%d." text-no)))
	(buf (lyskom-create-text-buffer text-no text text-stat)))
    (write-region (point-min) (point-max) file)
    (kill-buffer buf)
    file))

(defvar diff-command)

(def-kom-command kom-diff-texts (old new &optional switches)
  "Show differences between two texts, OLD and NEW.

When called interactively, it will prompt for the NEW text first,
defaulting to the last viewed texts. The OLD text number will default
to the first text that NEW is a comment or footnote to.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg') for the NEW text."
  (interactive
   (let* ((n (lyskom-read-text-no-prefix-arg 'diff-what-text-new))
	  (new-stat (blocking-do 'get-text-stat n))
	  (o (lyskom-read-number
	      'diff-what-text-old
	      (if (null new-stat)
		  (lyskom-error (lyskom-get-string 'no-such-text-no n))
		(car (lyskom-text-stat-commented-texts new-stat))))))
     (list
      o n
      (if current-prefix-arg
	  (list (read-string "Diff switches: "
			     (if (stringp diff-switches)
				 diff-switches
			       (mapconcat 'identity diff-switches " "))))
	nil))))

  (blocking-do-multiple ((old-text (get-text old))
			 (new-text (get-text new))
			 (old-text-stat (get-text-stat old))
			 (new-text-stat (get-text-stat new)))

    (cond
     ((or (null old-text) (null old-text-stat))
      (lyskom-format-insert 'no-such-text-no old))
     ((or (null new-text) (null new-text-stat))
      (lyskom-format-insert 'no-such-text-no new))
     (t
      (let* ((buf (current-buffer))
	     (oldfile (lyskom-create-temp-file old old-text old-text-stat))
	     (newfile (lyskom-create-temp-file new new-text new-text-stat))
	     (args (list "-L" (format "%d\t%s" old
				      (lyskom-format-time
				       'timeformat-yyyy-mm-dd-hh-mm-ss
				       (text-stat->creation-time
					old-text-stat)))
			 "-L" (format "%d\t%s" new
				      (lyskom-format-time
				       'timeformat-yyyy-mm-dd-hh-mm-ss
				       (text-stat->creation-time
					new-text-stat)))
			 oldfile newfile)))

	(if switches
	    (setq args (append
			(lyskom-split-string (if (consp switches)
                                                 (mapconcat 'identity switches " ")
                                               switches))
			args))
	  (if diff-switches
	      (setq args (append
			  (lyskom-split-string (if (consp diff-switches)
                                                   (mapconcat 'identity diff-switches
                                                              " ")
                                                 diff-switches))
			  args))))
	    

	(set-buffer buf)
	(lyskom-insert
	 (with-temp-buffer
	   (apply 'call-process (if (boundp 'diff-command)
				    diff-command
				  "diff")
		  nil t nil args)
	   (buffer-string)))
	(delete-file oldfile)
	(delete-file newfile))))))
      
    

;;; ================================================================
;;;             Skapa aux-item

(def-kom-command kom-create-aux-item ()
  "Creates arbitrary aux-items.
This command is primarily targeted at advanced users and developers
who want access to aux-item-based features before they are implemented
properly in the client."
  (interactive)
  (let* ((completions
          (mapcar (lambda (x) (cons (lyskom-get-string x) x))
                  '(server conference text)))
         (completion-ignore-case t)
         (object-type 
          (cdr (lyskom-string-assoc 
                (lyskom-completing-read 
                 (lyskom-get-string 'what-kind-to-add-aux-to) 
                 completions nil t)
                completions)))
         (object-id (cond ((eq object-type 'server) nil)
                           ((eq object-type 'conference)
                            (lyskom-read-conf-no 'which-conf-to-add-aux-to
                                                 '(pers conf) nil nil t))
                           ((eq object-type 'text)
                            (lyskom-read-number 'which-text-to-add-aux-to 
                                                (if (and lyskom-current-text
                                                         (not (zerop lyskom-current-text)))
                                                    lyskom-current-text
                                                  nil)))))
         (tag (lyskom-read-number 'which-aux-item-tag))
         (inherit (lyskom-j-or-n-p 'which-aux-item-inherit))
         (secret (lyskom-j-or-n-p 'which-aux-item-secret))
         (anonymous (lyskom-j-or-n-p 'which-aux-item-anonymous))
         (rsv1 (lyskom-j-or-n-p 'which-aux-item-rsv1))
         (rsv2 (lyskom-j-or-n-p 'which-aux-item-rsv2))
         (rsv3 (lyskom-j-or-n-p 'which-aux-item-rsv3))
         (rsv4 (lyskom-j-or-n-p 'which-aux-item-rsv4))
         (inherit-limit (lyskom-read-number 'which-aux-item-inherit-limit))
         (data (lyskom-read-string (lyskom-get-string 'which-aux-item-data)))
         (flags (lyskom-create-aux-item-flags nil inherit secret anonymous
                                              rsv1 rsv2 rsv3 rsv4))
         (item (lyskom-create-aux-item 0 tag 0 0 flags inherit-limit data)))
    
    (lyskom-report-command-answer 
     (cond ((eq object-type 'server) 
            (blocking-do 'modify-server-info nil (list item)))
           ((eq object-type 'conference)
            (progn (cache-del-conf-stat object-id)
                   (blocking-do 'modify-conf-info object-id nil (list item))))
           ((eq object-type 'text)
            (progn (cache-del-text-stat object-id)
                   (blocking-do 'modify-text-info object-id nil (list item))))))
    (when (eq object-type 'server)
      (setq lyskom-server-info (blocking-do 'get-server-info)))))


;;; ================================================================
;;; Status för LysKOM
;;;
;;; Skriv ut:
;;; * Serverns kanoniska namn (canonical-name eller ud kom-server-alist)
;;; * Serverns DNS-namn/IP och port
;;; * Serverns programvara och version
;;; * Högsta existerande inläggsnummer
;;; * Antal sessioner
;;; * Serverns tid
;;;

(def-kom-command kom-status-server ()
  "Show status information for the LysKOM server."
  (interactive)
  (blocking-do-multiple ((server-info (get-server-info))
                         (server-version (get-version-info))
                         (server-time (get-time))
                         (highest-text (find-previous-text-no lyskom-max-int))
                         (first-text (find-next-text-no 0))
                         (session-info (who-is-on-dynamic  t t 0))
                         (stats-desc (get-stats-description))
                         (text-stats (get-stats "texts"))
                         (conf-stats (get-stats "confs"))
                         (pers-stats (get-stats "persons"))
                         (boottime-info (get-boottime-info)))
    (setq lyskom-server-info (blocking-do 'get-server-info))
    (setq lyskom-server-version-info (blocking-do 'get-version-info))

    (let* ((aux-items (server-info->aux-item-list lyskom-server-info))
           (canonical-name-aux (car (lyskom-get-aux-item aux-items 31)))
           (invisible-sessions 0)
           (anonymous-sessions 0)
           (active-sessions 0)
           (inactive-sessions 0)
           (unknown-activity-sessions 0)
           (total-sessions (length session-info))
           (idle-hide (* 60 (if (numberp kom-idle-hide) kom-idle-hide 30)))
           (kom-extended-status-information
            (lyskom-extended-status-override 'server))
           momentary-index periodic-index)

      (setq aux-items (delq canonical-name-aux aux-items))

      ;; Compute the index into statistics that represents the
      ;; momentary value (when equals zero) and the periodic
      ;; value (when is at least five minutes).

      (when stats-desc
        (let ((min-time
               (apply 'min (listify-vector (stats-description->when
                                            stats-desc))))
              (max-time
               (apply 'max (listify-vector (stats-description->when
                                            stats-desc))))
              (i 0)
              (min-diff lyskom-max-int)
              max-time-index)
          (lyskom-traverse val (stats-description->when stats-desc)
            (when (eq val min-time) (setq momentary-index i))
            (when (eq val max-time) (setq max-time-index i))
            (when (and (>= val 300)
                       (< (- val 300) min-diff))
              (setq min-diff (- val 300)
                    periodic-index i))
            (setq i (1+ i)))
          (unless periodic-index (setq periodic-index max-time-index))))

      ;; ----------------------------------------
      ;; Compute session statistics
      (lyskom-traverse session session-info

        ;; Record anonymity
        (when (zerop (dynamic-session-info->person session))
          (setq anonymous-sessions (1+ anonymous-sessions)))

        ;; Record activity
        (if (session-flags->user_active_used (dynamic-session-info->flags session))
            (if (> (dynamic-session-info->idle-time session) idle-hide)
                (setq inactive-sessions (1+ inactive-sessions))
              (setq active-sessions (1+ active-sessions)))
          (setq unknown-activity-sessions (1+ unknown-activity-sessions)))

        ;; Record invisibility
        (when (session-flags->invisible (dynamic-session-info->flags session))
          (setq invisible-sessions (1+ invisible-sessions)))
        )

      ;; ----------------------------------------
      ;; Print header

      (lyskom-format-insert 'server-status-header
			    (lyskom-session-nickname)
			    lyskom-server-name
			    lyskom-server-port)

      ;; ----------------------------------------
      ;; Print software name and version

      (lyskom-format-insert 'server-status-version
                            (version-info->server-software server-version)
                            (version-info->software-version server-version))
      (lyskom-format-insert 'server-status-protocol
                            (version-info->protocol-version server-version))

      ;; ----------------------------------------
      ;; Print canonical name, if we have one

      (when canonical-name-aux
        (let ((canonical-name nil)
              (canonical-port nil))
          (if (string-match ":" (aux-item->data canonical-name-aux))
              (setq canonical-name (substring (aux-item->data canonical-name-aux) 0 (match-beginning 0))
                    canonical-port (substring (aux-item->data canonical-name-aux) (1+ (match-beginning 0))))
            (setq canonical-name (aux-item->data canonical-name-aux)))
          (lyskom-format-insert 'server-status-server canonical-name canonical-port)))

      ;; ----------------------------------------
      ;; Print start time and such

      (when boottime-info
        (lyskom-format-insert 'server-status-boot-time
                            (let ((kom-print-relative-dates nil))
                              (lyskom-format-time 
                               'date-and-time
                               (static-server-info->boot-time
                                boottime-info))))

        (lyskom-format-insert
         'server-status-save-time
         (let ((kom-print-relative-dates nil))
           (lyskom-format-time 
            'date-and-time
            (static-server-info->save-time
             boottime-info)))
         (cond ((equal "clean" (static-server-info->db-status boottime-info)) nil)
               (t (condition-case nil
                      (lyskom-get-string (intern (concat "db-status-" (static-server-info->db-status boottime-info))))
                    (error (static-server-info->db-status boottime-info))))))
         )

      ;; ----------------------------------------
      ;; Print time
      (lyskom-format-insert 'server-status-time
                            (let ((kom-print-relative-dates nil))
                              (lyskom-format-time 'date-and-time server-time)))

      ;; ----------------------------------------
      ;; Print session statistics

      (lyskom-format-insert 'server-status-sessions
                            total-sessions
                            active-sessions
                            inactive-sessions
                            unknown-activity-sessions
                            invisible-sessions
                            anonymous-sessions
                            (/ idle-hide 60))

      ;; ----------------------------------------
      ;; Print stats for confs and persons

      (when pers-stats
        (lyskom-format-insert 'server-status-pers
                              (stats->average
                               (elt pers-stats momentary-index))
                              (lyskom-format-time-units
                               (stats->ascent-rate
                                (elt pers-stats periodic-index)))
                              (static-server-info->existing-persons
                               boottime-info)))

      (when conf-stats
        (lyskom-format-insert 'server-status-confs
                              (stats->average
                               (elt conf-stats momentary-index))
                              (lyskom-format-time-units
                               (stats->ascent-rate
                                (elt conf-stats periodic-index)))
                              (static-server-info->existing-confs
                               boottime-info)))


      ;; ----------------------------------------
      ;; Print info on text numbers

      (when text-stats
        (lyskom-format-insert 'server-status-texts
                              (stats->average
                               (elt text-stats momentary-index))
                              (lyskom-format-time-units
                               (stats->ascent-rate
                                (elt text-stats periodic-index)))
                              (static-server-info->existing-texts
                               boottime-info)))
      (lyskom-format-insert 'server-status-first-text first-text)
      (lyskom-format-insert 'server-status-last-text 
                            highest-text
                            (when boottime-info
                              (static-server-info->highest-text-no
                               boottime-info)))


      ;; ----------------------------------------
      ;; Print remaining aux-items
      (lyskom-traverse-aux item aux-items
        (if (lyskom-aux-item-definition-field item 'status-print)
            (lyskom-aux-item-call item 'status-print item 'server)
          (lyskom-format-insert 'status-aux-item
                                (format "%d/%d" 
                                        (aux-item->aux-no item)
                                        (aux-item->tag item))
                                (aux-item->creator item)
                                (lyskom-aux-item-terminating-button item 'server))
          ))

      ;; ----------------------------------------
      ;; Print statistics

      (when (lyskom-extended-status-information 'raw-server-stats)
        (let* ((stats (lyskom-get-server-stats))
               (what (server-stats->what stats))
               (maxlen (and stats (apply 'max (mapcar 'length what))))
               (fmt (and maxlen (format "%%=%d#1s " maxlen))))
          (when stats
            (lyskom-format-insert 'status-server-stats)
            (lyskom-insert (lyskom-format fmt ""))
            (lyskom-traverse period (server-stats->when stats)
              (lyskom-format-insert "  %=8#1s" 
                                    (if (eq 0 period)
                                        (lyskom-get-string 'current-average)
                                      (lyskom-format-units period 
                                                           '((604800 . "w")
                                                             (86400 . "d")
                                                             (3600 . "h")
                                                             (60 . "m"))
                                                           "s"))))
            (lyskom-insert "\n")
            (lyskom-traverse item (server-stats->values stats)
              (let ((start (point-max))
                    (inhibit-read-only t))
                (lyskom-format-insert fmt (car item))
                (lyskom-traverse val (cdr item)
                  (lyskom-format-insert "  %=8.2.7#1f" (stats->average val)))
                (lyskom-format-insert "\n")
                (add-text-properties start (point-max) `(face ,kom-mark-face)))

              (lyskom-format-insert fmt "")
              (lyskom-insert " ")
              (let ((index 0))
                (lyskom-traverse val (cdr item)
                  (if (eq (elt (server-stats->when stats) index) 0)
                      (lyskom-insert "          ")
                    (lyskom-format-insert " %=8.2.7#1f+"
                                          (stats->ascent-rate val)))
                  (setq index (1+ index))))
              (lyskom-format-insert "\n")

              (lyskom-format-insert fmt "")
              (lyskom-insert " ")
              (let ((index 0))
                (lyskom-traverse val (cdr item)
                  (if (eq (elt (server-stats->when stats) index) 0)
                      (lyskom-insert "          ")
                    (lyskom-format-insert " %=8.2.7#1f-"
                                          (stats->descent-rate val)))
                  (setq index (1+ index))))
              (lyskom-format-insert "\n")
              ))))


      ;; ----------------------------------------
      ;; Print MOTD (if there is one)
      (when (not (zerop (server-info->motd-of-lyskom server-info)))
        (lyskom-insert 'server-status-has-motd)
        (lyskom-view-text (server-info->motd-of-lyskom server-info)))

)))


;;; ================================================================
;;; Rekommendera möte

(def-kom-command kom-recommend-conference ()
  "Recommend a conference to new LysKOM users.
Thie command can only be used if you have administrative rights
to the LysKOM server."
  (interactive)
  (let* ((conf-stat (lyskom-read-conf-stat 'recommend-which-conf
                                           '(conf) nil nil t))
         (priority (and (lyskom-j-or-n-p 'recommend-set-priority-q)
                        (lyskom-read-num-range 0 255 'priority-q)))
         (mship-type (and priority
                          (lyskom-j-or-n-p 'recommend-set-mship-type-q)
                          (lyskom-read-membership-type)))

         (aux-item (lyskom-create-aux-item 
                    0 29 nil nil
                    (lyskom-create-aux-item-flags nil nil nil nil
                                                  nil nil nil nil)
                    0
                    (mapconcat 'lyskom-format-object
                               (delq nil
                                     (list (conf-stat->conf-no conf-stat)
                                           priority
                                           mship-type))
                               " "))))
    (lyskom-format-insert 'recommending-conf
                          conf-stat
                          priority
                          (and mship-type
                               (lyskom-return-membership-type mship-type)))
    (lyskom-report-command-answer (blocking-do 'modify-server-info
                                               nil
                                               (list aux-item)))))


;;; ================================================================
;;; Add send-comments-to
;;;

(def-kom-command kom-redirect-comments ()
  "Add extended information to your letterbox that causes some other
conference to be added as a recipient to comments to your texts when
someone posts them to a conference you are not a member of.

Note that this is advisory only; clients may ignore your redirection."
  (interactive)
  (let* ((conf-stat (lyskom-read-conf-stat 'redirect-for-whom
                                           '(pers) nil nil t))
         (redirect-to (lyskom-read-conf-stat 'redirect-to-which-conf
                                             '(pers conf) nil nil t))
         (old-redirect (car (lyskom-get-aux-item 
                             (conf-stat->aux-items conf-stat)
                             33))))
    (lyskom-format-insert 'redirecting-comments-to
                          conf-stat redirect-to old-redirect)
    (lyskom-report-command-answer
     (blocking-do 'modify-conf-info
                  (conf-stat->conf-no conf-stat)
                  (and old-redirect (list (aux-item->aux-no old-redirect)))
                  (list (lyskom-create-aux-item 
                         0
                         33
                         nil nil
                         (lyskom-create-aux-item-flags
                          nil nil nil nil 
                          nil nil nil nil)
                         0
                         (format "%d" (conf-stat->conf-no redirect-to)))))
     nil
     '((illegal-aux-item . kom-redirect-comments-e48)
       (aux-item-permission . kom-redirect-comments-e48))))
  )


;;; ================================================================
;;; Kamikaze functions!

(def-kom-command kom-join-all-conferences ()
  "Join all conferences.

You want to be really careful doing this. It will take a while and
is probably not what you really want to do." 
  (interactive)
  (let* ((conf-nos (lyskom-get-all-conferences t))
         (confirm-each (or (lyskom-j-or-n-p 'confirm-each-join)
                           (not (lyskom-j-or-n-p 
                                 (lyskom-format 'no-confirm-each-sure
                                                (length conf-nos))))))
         (mship-type (unless (lyskom-j-or-n-p 'confirm-each-msg)
                       (lyskom-create-membership-type
                        nil nil nil (lyskom-j-or-n-p 'receive-each-msg)
                        nil nil nil nil)))

         (no-of-unread (lyskom-read-num-range-or-date 
                        0 
                        lyskom-max-int
                        (lyskom-format 'initial-unread)
                        nil
                        t
                        nil))
         (kom-membership-default-priority
          (lyskom-read-num-range 0 255 
                                 (lyskom-get-string 'priority-q)
                                 nil nil)))
    (while conf-nos
      (unless (or (lyskom-get-membership (car conf-nos) t)
                  (and confirm-each
                       (not (lyskom-j-or-n-p (lyskom-format 'confirm-join 
                                                            (car conf-nos))))))
        (lyskom-add-member-by-no (car conf-nos) lyskom-pers-no 
                                 no-of-unread mship-type)
        (sit-for 0))
      (setq conf-nos (cdr conf-nos)))))



(def-kom-command kom-leave-all-conferences ()
  "Leave all conferences.

This command will ignore certain conferences. It will not leave your
letterbox, secret or closed conferences without confirmation. Note
that this command could take a very long time to complete."
  (interactive)
  (let* ((auto-regular nil)
         (auto-secret nil)
         (auto-closed nil)
         (confs nil))
    (lyskom-message (lyskom-get-string 'getting-all-confs))
    (let* ((xlist (lyskom-get-all-conferences))
           (count (length xlist))
           (index 0))
      (setq confs (delq nil
                        (mapcar (lambda (conf-no)
                                  (setq index (1+ index))
                                  (lyskom-message
                                   (lyskom-format 'getting-all-confs-progress index count))
                                  (when (or (lyskom-try-get-membership conf-no t)
                                            (lyskom-is-member conf-no lyskom-pers-no))
                                    conf-no))
                                xlist)))
      (lyskom-message (lyskom-get-string 'getting-all-confs-done)))

    (while confs
      (let* ((conf (car confs))
             (conf-stat (blocking-do 'get-conf-stat conf))
             (conf-type (conf-stat->conf-type conf-stat)))
        (setq lyskom-last-viewed (point-max))
        (lyskom-format-insert 'unsubscribe-to-2
                              conf-stat
                              (lyskom-conf-type-marker conf-stat))
        (when (cond 
               ;; Won't auto-unsub letterbox
               ((= (conf-stat->conf-no conf-stat) lyskom-pers-no)
                (lyskom-insert (lyskom-get-string
                                'unsub-all-skipping-letterbox))
                nil)

               ;; Won't unsub if we are supervisor
               ((lyskom-i-am-supervisor conf-stat t)
                (lyskom-insert-string 'unsub-all-skipping-supervised)
                nil)

               ;; Check secret conferences
               ((conf-type->secret conf-type)
                (if auto-secret
                    t
                  (let ((ans (lyskom-a-or-b-or-c-p (lyskom-format 'unsub-secret-conf-q conf-stat)
                                                   '(abc-yes abc-no unsub-all-secret)
                                                   'abc-no)))
                    (cond ((eq ans 'abc-no)
                           (lyskom-insert-string 'cancelled) nil)
                          ((eq ans 'abc-yes) t)
                          ((eq ans 'unsub-all-secret)
                           (setq auto-secret t)
                           t)))))

               ;; Skip closed only if doit is 
               ((conf-type->rd_prot conf-type)
                (if auto-closed
                    t
                  (let ((ans (lyskom-a-or-b-or-c-p (lyskom-format 'unsub-closed-conf-q conf-stat)
                                                   '(abc-yes abc-no unsub-all-closed)
                                                   'abc-no)))
                    (cond ((eq ans 'abc-no) 
                           (lyskom-insert-string 'cancelled) nil)
                          ((eq ans 'abc-yes) t)
                          ((eq ans 'unsub-all-closed)
                           (setq auto-closed t)
                           t)))))

               ;; Regular conference
               (t 
                (if auto-regular
                    t
                  (let ((ans (lyskom-a-or-b-or-c-p (lyskom-format 'unsub-open-conf-q conf-stat)
                                                   '(abc-yes abc-no unsub-all-open)
                                                   'abc-no)))
                    (cond ((eq ans 'abc-no) 
                           (lyskom-insert-string 'cancelled) nil)
                          ((eq ans 'abc-yes) t)
                          ((eq ans 'unsub-all-open)
                           (setq auto-regular t)
                           t))))))


          ;; How's that for a really long condition? Lots of
          ;; cool side effects and user interactions. Don't
          ;; you just love functional programming?

          ;; Now we've either printed a message or we've
          ;; decided to unsubscribe.

          (lyskom-sub-member (blocking-do 'get-conf-stat lyskom-pers-no)
                             conf-stat
                             t)

          ))
      (sit-for 0)
      (setq confs (cdr confs)))
    ))


(def-kom-command kom-limit-import (conf-stat)
  "Specify restrictions on importing e-mail. Note that such restrictions
are advisory; clients may ignore them."
  (interactive (list (lyskom-read-conf-stat 'limit-import-to-conf
                                            '(conf pers) nil nil t)))
  (when conf-stat
    (let* ((what (lyskom-a-or-b-or-c-p 'limit-import-of-what
                                       '(abc-spam abc-html abc-everything)
                                       'abc-spam))
           (data (cond ((eq what 'abc-spam) "spam")
                       ((eq what 'abc-html) "html")
                       ((eq what 'abc-everything) "all"))))
      (when (or (lyskom-is-supervisor (conf-stat->conf-no conf-stat)
                                      lyskom-pers-no)
                (lyskom-j-or-n-p 'limit-import-not-super))
        (when data
          (lyskom-format-insert 'limiting-import
                                (substring (lyskom-get-string what) 1)
                                conf-stat)
          (lyskom-report-command-answer
           (blocking-do 'modify-conf-info
                        (conf-stat->conf-no conf-stat)
                        nil
                        (list
                         (lyskom-create-aux-item
                          0 35 nil nil
                          (lyskom-create-aux-item-flags nil nil nil nil 
                                                        nil nil nil nil)
                          0 data))))
          (cache-del-conf-stat (conf-stat->conf-no conf-stat)))))))


(def-kom-command kom-change-message-flag (uconf-stat)
  "Specify whether to receive group messages to a particular conference."
  (interactive (list (lyskom-read-uconf-stat 'set-message-flag-for-conf
                                            '(membership) nil
                                            lyskom-current-conf t)))
  (when uconf-stat
    (let* ((mship (lyskom-get-membership (uconf-stat->conf-no uconf-stat) t)))
      (if (null mship)
          (lyskom-format-insert 'not-member-of-conf uconf-stat)
        (set-membership-type->message-flag
         (membership->type mship)
         (lyskom-j-or-n-p (lyskom-format 'set-message-flag-to-what
                                         uconf-stat)))
        (lyskom-format-insert 'setting-message-flag
                              (membership-type->message-flag
                               (membership->type mship))
                              uconf-stat)
        (lyskom-report-command-answer
         (blocking-do 'set-membership-type
                      lyskom-pers-no
                      (uconf-stat->conf-no uconf-stat)
                      (membership->type mship)))
        (lp--update-buffer (uconf-stat->conf-no uconf-stat))))))

(def-kom-command kom-list-new-conferences (arg)
  "List conferences created since the last time this command
was given. With prefix argument, prompt for a date to start at."
  (interactive "P")
  (lyskom-list-new-conferences 'lyskom-last-known-conf-no
                               'conferences
                               (lambda (el)
                                 (not (conf-type->letterbox
                                       (conf-stat->conf-type el))))
                               arg))

(def-kom-command kom-list-new-persons (arg)
  "List persons created since the last time this command
was given. With prefix argument, prompt for a date to start at."
  (interactive "P")
  (lyskom-list-new-conferences 'lyskom-last-known-pers-no
                               'persons
                               (lambda (el)
                                 (conf-type->letterbox
                                  (conf-stat->conf-type el)))
                               arg))

(defun lyskom-list-new-conferences (varsym obj filter &optional list-from-date)
  "List conferences created since a particular point in history.

VARSYM is the name of a variable that should be bound to a cons of the 
last conference number displayed and the date when it was displayed 
\(lyskom-time structure).

OBJ is the type of object. It should be a symbol that can be passed
to lyskom-get-string to get a text representation of the object type.

FILTER is a function called on each conference. It should return
non-nil for conferences that should be displayed.

If optional LIST-FROM-DATE is non-nil, prompt for a date and list
all conferences from that date forward."
  (interactive)
  (let* ((last-conf-no (blocking-do 'first-unused-conf-no))
         (var (or (and (null list-from-date) 
                       (symbol-value varsym))
                  (let* ((n (lyskom-read-date 
                             (lyskom-format 'list-confs-from-date
                                            (lyskom-get-string obj))
                             t))
                         (date (and n (lyskom-create-time 0 0 0 (elt n 2)
                                                          (elt n 1) (elt n 0)
                                                          0 0 nil)))
                         (conf (and n (lyskom-find-conf-by-date date))))
                    (cond 

                     ;; No date -- we want it all!
                     ((null n) (cons 1 nil))

                     ;; Date but no conf -- no confs to list!
                     ((null conf) (cons last-conf-no date))

                     ;; Date and conf -- list from this conf
                     (t (cons (conf-stat->conf-no conf) date))))))
         (conf-no (car var))
         (time-string (condition-case nil
                          (and (cdr var) (lyskom-format-time
                                          'date-and-time (cdr var)))
                        (error nil))))

    (if (null last-conf-no)
        (lyskom-format-insert 'no-support-in-server))

    ;; There's a serious bug here: we can't cancel!
    ;; That *has* to be fixed.

    (let ((count (cons 0 (make-marker)))
          (calls nil))
      (condition-case nil
          (progn 
            (while (< conf-no last-conf-no)
              (setq calls 
                    (cons
                     (initiate-get-conf-stat 
                      'main
                      (lambda (conf filter count time-string)
                        (when (and conf (funcall filter conf))
                          (when (eq (car count) 0)
                            (lyskom-format-insert 'new-conferences-since 
                                                  time-string (lyskom-get-string obj)))
                          (rplaca count (1+ (car count)))
                          (lyskom-format-insert "%5#1m %#2c %#1M\n"
                                                conf
                                                (lyskom-list-conf-membership-char
                                                 (conf-stat->conf-no conf)))
                          (set-marker (cdr count) (point))))
                      conf-no filter count time-string)
                     calls))
              (setq conf-no (1+ conf-no)))
            (lyskom-wait-queue 'main))
        (quit 
         (lyskom-message (lyskom-get-string 'canceling-command))
         (lyskom-cancel-calls calls)
         (signal 'quit nil)))
      (when (marker-position (cdr count))
        (goto-char (cdr count))
	(set-marker (cdr count) nil))

      (cond ((eq 0 (car count))
             (lyskom-format-insert 'no-new-conferences 
                                   time-string (lyskom-get-string obj))) 

            (t 
               (when (or kom-auto-confirm-new-conferences
                         (lyskom-j-or-n-p (lyskom-format 'mark-confs-as-known
                                                         (lyskom-get-string obj)
                                                         (car count))))
                 (set varsym (cons conf-no (lyskom-current-client-time)))
                 (lyskom-save-options lyskom-buffer
                                      nil
                                      nil
                                      nil)))))))




(def-kom-command kom-change-privileges (&optional set-all)
  "Change privileges for a user.
Using this command you can change all privileges for a user."
  (interactive "P")
  (let* ((uconf-stat (lyskom-read-uconf-stat 'what-pers-privs-to-change
                                             '(pers) nil "" t))
         (pers-stat (blocking-do 'get-pers-stat
                                 (uconf-stat->conf-no uconf-stat)))
         (privs (pers-stat->privileges pers-stat)))

    (lyskom-format-insert
     'change-pers-privs-prompt
     uconf-stat
     (lyskom-privilege-string privs nil "\n    "))

    (let* ((wheel (lyskom-j-or-n-p (lyskom-get-string 'set-wheel-priv-q)))
           (admin (lyskom-j-or-n-p (lyskom-get-string 'set-admin-priv-q)))
           (statistic (lyskom-j-or-n-p (lyskom-get-string 'set-statistic-priv-q)))
           (create-pers (lyskom-j-or-n-p (lyskom-get-string 'set-create-pers-priv-q)))
           (create-conf (lyskom-j-or-n-p (lyskom-get-string 'set-create-conf-priv-q)))
           (change-name (lyskom-j-or-n-p (lyskom-get-string 'set-change-name-priv-q)))
           (flg7 (if set-all (lyskom-j-or-n-p (lyskom-get-string 'set-flg7-priv-q)) (privs->flg7 privs)))
           (flg8 (if set-all (lyskom-j-or-n-p (lyskom-get-string 'set-flg8-priv-q)) (privs->flg8 privs)))
           (flg9 (if set-all (lyskom-j-or-n-p (lyskom-get-string 'set-flg9-priv-q)) (privs->flg9 privs)))
           (flg10 (if set-all (lyskom-j-or-n-p (lyskom-get-string 'set-flg10-priv-q)) (privs->flg10 privs)))
           (flg11 (if set-all (lyskom-j-or-n-p (lyskom-get-string 'set-flg11-priv-q)) (privs->flg11 privs)))
           (flg12 (if set-all (lyskom-j-or-n-p (lyskom-get-string 'set-flg12-priv-q)) (privs->flg12 privs)))
           (flg13 (if set-all (lyskom-j-or-n-p (lyskom-get-string 'set-flg13-priv-q)) (privs->flg13 privs)))
           (flg14 (if set-all (lyskom-j-or-n-p (lyskom-get-string 'set-flg14-priv-q)) (privs->flg14 privs)))
           (flg15 (if set-all (lyskom-j-or-n-p (lyskom-get-string 'set-flg15-priv-q)) (privs->flg15 privs)))
           (flg16 (if set-all (lyskom-j-or-n-p (lyskom-get-string 'set-flg16-priv-q)) (privs->flg16 privs)))
           (new-privs (lyskom-create-privs wheel admin statistic create-pers 
                                           create-conf change-name flg7 flg8
                                           flg9 flg10 flg11 flg12
                                           flg13 flg14 flg15 flg16)))

      (if (not (blocking-do 
                'set-priv-bits
                (pers-stat->pers-no pers-stat)
                new-privs))
          (progn (lyskom-insert-string 'nope)
                 (lyskom-insert-error))
        (when (setq pers-stat 
                    (cache-get-pers-stat (pers-stat->pers-no pers-stat)))
          (set-pers-stat->privileges pers-stat new-privs))
        (lyskom-insert-string 'done)))))




;;; ================================================================
;;; Temporary function for when we moved kom-extended-command from a
;;; to x.

(defun kom-obsolete-extended-command-binding ()
  "Temporary function for when we moved kom-extended-command from a"
  (interactive)
  (lyskom-format-insert-before-prompt "\
%#1@----------------------------------------------------------------
Tangenten för inmatning av kommandon har flyttat från a till x.

Använd x istället för a om du vill ange LysKOM-kommandon vid
namn. För att undvika att du kör oönskade LysKOM-kommandon så
ignoreras all inmatning tills du trycker enter nästa gång. 

Tryck på enter för att fortsätta.
----------------------------------------------------------------
"
                                      `(face ,kom-warning-face))
  ;; Beep both visibly and audibly, if possible.  We *want* to be annoying.
  (let ((visible-bell t))
    (ding))
  (let ((visible-bell nil))
    (ding))
  (read-from-minibuffer 
   (lyskom-format "%#1@Tryck return eller enter för att gå vidare: "
                  `(face ,kom-warning-face))))


(defun kom-obsolete-who-is-on-in-conference ()
  "Temporary function for when we moved kom-who-is-on-in-conference from l v to 
v m"
  (interactive)
  (lyskom-insert-before-prompt "Kommandot \"Vilka (är inloggade i) möte\" är flyttat till v m\n"))


(defun kom-apropos (re do-all)
  "List LysKOM-related symbols whose name or documentation matches a regexp.
With prefix argument, also list symbols that are not part of the semi-stable
interface (i.e. symbols whose name starts with \"lyskom\").

This command is intended primarily for developers and advanced users."
  (interactive "sLysKOM-apropos (regexp): \nP")
  (message "Searching for %s..." re)
  (let ((result nil)
        (case-fold-search t))
    (mapatoms
     (lambda (atom)
       (when (and (or (boundp atom) (fboundp atom))
                  (or (and do-all (string-match "^lyskom-" (symbol-name atom)))
                      (string-match "^kom-" (symbol-name atom)))
                  (or (string-match re (symbol-name atom))
                      (and (fboundp atom)
                           (string-match re (or (documentation atom) "")))
                      (and (boundp atom)
                           (string-match re (or
                                             (documentation-property 
                                              atom 'variable-documentation)
                                             "")))))
         (setq result (cons atom result)))))
    (setq result 
          (sort result (lambda (a b)
                         (string-lessp (symbol-name a)
                                       (symbol-name b)))))
    (message "Searching for %s...formatting..." re)
    (if result
        (let ((buffer (get-buffer-create "*LysKOM-Apropos*")))
          (save-window-excursion
            (let ((inhibit-read-only t))
              (set-buffer buffer)
              (help-mode)
              (lyskom-view-mode)
              (erase-buffer)
              (insert "\
In this buffer, go to the name of the symbol and type * to
get full documentation.

")
              (lyskom-traverse el result
                (lyskom-apropos-insert el))
              (toggle-read-only 1))
            (message "Searching for %s...formatting...done" re)
            (display-buffer buffer)
            (goto-char (point-min))))
      (message "Nothing found that matches \"%s\"" re))))


(defun lyskom-apropos-insert (sym)
  (let ((start (point)))
    (insert (symbol-name sym))
    (add-text-properties start (point) 
                         `(face bold 
                           mouse-face highlight
                           lyskom-button t
                           lyskom-button-text ,(symbol-name sym)
                           lyskom-button-type func
                           lyskom-buffer ,(current-buffer)
                           lyskom-button-arg (lyskom-apropos-item ,sym)))
    (insert "\n")
    (lyskom-apropos-insert-docstring 'func
                                     sym
                                     (when (fboundp sym)
                                       (documentation sym)))
    (lyskom-apropos-insert-docstring 'var
                                     sym 
                                     (when (boundp sym)
                                       (documentation-property 
                                        sym 'variable-documentation)))
    (insert "\n")))

(defun lyskom-apropos-insert-docstring (type sym doc)
  (when doc
    (insert "    ")
    (let ((start (point)))
      (cond ((eq type 'func) (insert "Func: "))
            ((eq type 'var)  (insert "Var:  ")))
      (when (not (eq start (point)))
        (add-text-properties start (point)
                             `(face italic
                                    mouse-face highlight
                                    lyskom-button t
                                    lyskom-button-text ""
                                    lyskom-button-type func
                                    lyskom-buffer ,(current-buffer)
                                    lyskom-button-arg (lyskom-apropos-item 
                                                       (,sym ,type)))))

      (insert
       (lyskom-truncate-string-to-width
        (if (string-match "\\(\r\\|\n\\)" doc)
            (substring doc 0 (string-match "\\(\r\\|\n\\)" doc))
          doc)
        (- (window-width) 12)))
      (insert "\n"))))


(defun lyskom-apropos-item (args)
  (let ((sym (elt args 0))
        (type (elt args 1)))
    (cond ((eq type 'func) (describe-function sym))
          ((eq type 'var)  (describe-variable sym))
          (t  (cond ((fboundp sym)
                     (describe-function sym))
                    ((boundp sym)
                     (describe-variable sym)))))))
