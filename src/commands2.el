;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: commands2.el,v 44.131 2002-05-21 22:05:42 byers Exp $
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
              "$Id: commands2.el,v 44.131 2002-05-21 22:05:42 byers Exp $\n"))

(eval-when-compile
  (require 'lyskom-command "command"))


;;; ================================================================
;;;              Lista medlemsskap - List membership

;;; Author: Linus Tolke
;;; Rewritten by ceder


;; This functions is left in its "asynchronous way".
(def-kom-command kom-membership ()
  "Show memberships last visited, priority, unread and name."
  (interactive)
  (let ((buffer (lyskom-get-buffer-create 'list-membership
                                          (concat (buffer-name 
                                                   (current-buffer))
                                                  "-membership")
                                          t)))
    (save-window-excursion
      (set-buffer buffer)
      (lyskom-view-mode)
      (lyskom-add-hook 'lyskom-new-membership-list-hook
                       'lyskom-update-membership-when-changed t)
      (setq truncate-lines t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (lyskom-get-string 'your-memberships))
        (insert (lyskom-get-string 'memberships-header))))
    (save-selected-window
      (lyskom-display-buffer buffer))
    (lyskom-update-membership-buffer)))

(defun lyskom-update-membership-buffer ()
  (let ((buf (car (lyskom-buffers-of-category 'list-membership))))
    (when (buffer-live-p buf)
      (let ((inhibit-read-only t))
        (save-excursion (set-buffer buf)
                        (erase-buffer))
        (lyskom-traverse x lyskom-membership
          (initiate-get-conf-stat 'memberhsip 'lyskom-memb-received-1
                                  (membership->conf-no x)
                                  x buf))))))

(defun lyskom-update-membership-when-changed ()
  (let ((buffer (car (lyskom-buffers-of-category 'list-membership))))
    (if (buffer-live-p buffer)
        (save-excursion (set-buffer buffer)
                        (lyskom-update-membership-buffer))
      (lyskom-remove-hook 
       'lyskom-new-membership-list-hook
       'lyskom-update-membership-when-changed))))



(defun lyskom-memb-received-1 (conf-stat membership buffer)
  "Part of kom-membership.
Get maps for the conference CONF-STAT. MEMBERSHIP is the users
membership in that conference. Call lyskom-memb-received with
the resulting MAP, CONF-STAT, MEMBERSHIP and BUFFER.
Args: CONF-STAT MEMBERSHIP BUFFER."
  (if (/= (conf-stat->conf-no conf-stat)
          (membership->conf-no membership))
      (signal 'lyskom-internal-error '("lyskom-memb-received-1")))
  (let* ((first-wanted (1+ (membership->last-text-read membership)))
         (last-existing  (+ (conf-stat->first-local-no conf-stat)
                            (conf-stat->no-of-texts conf-stat)
                            -1)))
    (if (> first-wanted last-existing)
        (lyskom-run 'membership 'lyskom-memb-received
                    nil conf-stat membership buffer)
      (if (> (- last-existing first-wanted) 50)
          (lyskom-run 'membership 'lyskom-memb-received
                      (- last-existing first-wanted)
                      conf-stat membership buffer)
        (initiate-get-map 'membership 'lyskom-memb-received
                          (membership->conf-no membership)
                          first-wanted
                          (+ 1 last-existing
                             (- first-wanted))
                          conf-stat membership buffer)))))

(defun lyskom-memb-received (map conf-stat membership buffer)
  "Args: MAP CONF-STAT MEMBERSHIP BUFFER.
Prints membership in a conferences.
MAP may be nil if there are no new texts."
  (save-window-excursion
    (set-buffer buffer)
    (goto-char (point-max))
    (let ((lyskom-executing-command 'kom-membership)
          (lyskom-current-command 'kom-membership)
          (inhibit-read-only t)
          (kom-print-seconds-in-time-strings nil))
      (lyskom-format-insert 'memberships-line
                            (lyskom-format-time 'date-and-time
                                                (membership->last-time-read
                                                 membership))
                            (membership->priority membership)
                            (cond
                             ((null map) 0)
                             ((numberp map) map)
                             ((listp map)
                              (length (lyskom-list-unread map membership)))
                             (t (signal
                                 'lyskom-internal-error
                                 '("Erroneous map in lyskom-memb-received"))))
                            conf-stat))))


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
                                  '(all) nil nil t)))
        (kom-print-seconds-in-time-strings nil))
    (cache-del-conf-stat conf-no)
    (cache-del-uconf-stat conf-no)
    (blocking-do-multiple ((conf-stat (get-conf-stat conf-no))
                           (uconf-stat (get-uconf-stat conf-no)))
      (if (null conf-stat)
          (lyskom-insert-string 'no-such-conf)
        (let* ((type (uconf-stat->conf-type uconf-stat))
               (box (conf-type->letterbox type))
               (ori (conf-type->original type))
               (pro (conf-type->rd_prot type))
               (sec (conf-type->secret type)))
          (lyskom-format-insert 'status-record
                                conf-stat
                                (cond
                                 ((or box ori pro sec)
                                  (concat
                                   "("
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
                                          (conf-stat->conf-no conf-stat))))
                         ;; Print a row describing the membership of MEMBER
                         ;; (described by MEMBERSHIP) in CONF-STAT.
                         (if (or (null membership))
                             (lyskom-insert-string 'secret-membership)
                           (lyskom-insert 
                            (format "%17s"
                                    (lyskom-format-time
                                     'date-and-time
                                     (membership->last-time-read membership))))
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
  "Prints status for a person. If a prefix argument is given, the status of the
author of that text will be shown."
  (interactive (and current-prefix-arg ; only peek at textno:s when prefixed!
		    (list (lyskom-read-text-no-prefix-arg
			   'text-to-see-author-status-of))))
  (let ((pers-no
         (or (when (interactive-p)
	       (text-stat->author (blocking-do 'get-text-stat text-or-pers-no)))
	     text-or-pers-no
             (lyskom-read-conf-no (lyskom-get-string 'pers-for-status)
                                  '(pers) nil "" t)))
        (kom-print-seconds-in-time-strings nil)
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

      (if (not (zerop (conf-stat->msg-of-day conf-stat)))
          (progn
            (lyskom-format-insert 'has-motd conf-stat)
            (lyskom-view-text (conf-stat->msg-of-day conf-stat))))

      ;; Show aux items

      (lyskom-traverse-aux item
          (conf-stat->aux-items conf-stat)
        (lyskom-aux-item-call item 'status-print item conf-stat))

      ;; "Show all conferences CONF-STAT is a member of if the user so wishes."
      (lyskom-scroll)
      (if (null (lyskom-j-or-n-p (lyskom-get-string
                                  'show-membership-list-also-q))) 
          nil
        (let ((membership-list 
               (blocking-do 'get-membership
                            (conf-stat->conf-no conf-stat)))
              (lyskom-count-var 0))

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
                          (lyskom-format-time
                           'date-and-time
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
                    (if (zerop unread) "          " (format "%9d " unread))
                    (if (= (conf-stat->conf-no conf-stat)
                           (conf-stat->supervisor member-conf-stat))
                        (lyskom-get-string 'is-supervisor-mark)
                      "  ")
                    member-conf-stat
                    (lyskom-return-membership-type (membership->type membership))
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
  (let* ((tmp nil)
         (target (or who
                     (lyskom-read-conf-no
                      (format (lyskom-get-string 'who-to-send-message-to)
                              (lyskom-get-string 'nobody))
                      (if kom-permissive-completion
                          '(all)
                        '(login conf))
                      ;; Initial string:
                      t
                      (cond
                       ((eq kom-default-message-recipient 'everybody) nil)

                       ((and (eq kom-default-message-recipient 'group)
                             lyskom-last-group-message-recipient)
                        (cons lyskom-last-group-message-recipient 0))

                       ((or (and (eq kom-default-message-recipient 'group)
                                 (null lyskom-last-group-message-recipient))
                            (and (eq kom-default-message-recipient 'sender)
                                 lyskom-last-personal-message-sender))
                        (cons lyskom-last-personal-message-sender 0))

                       ((and (eq kom-default-message-recipient 'last-recipient)
                             lyskom-last-message-recipient
                             (not (eq 0 lyskom-last-message-recipient))
                             (setq tmp (blocking-do 'get-uconf-stat lyskom-last-message-recipient)))
                        (cons (conf-stat->name tmp) 0))

                       (t 
                        (if lyskom-last-personal-message-sender
                            (cons lyskom-last-personal-message-sender 0)
                          "")))
                      t))))
    (if (zerop target)
        (lyskom-format-insert 'message-use-alarm-instead
                              (lyskom-command-name 'kom-send-alarm))
      (setq lyskom-last-message-recipient target)
      (lyskom-format-insert 'message-recipient-info target)
      (lyskom-send-message target message))))


(def-kom-command kom-send-alarm (&optional message)
  "Send a message to all of the users in KOM right now."
  (interactive)
  (lyskom-insert (lyskom-get-string 'message-all-info))
  (lyskom-send-message 0 message))


(defvar lyskom-message-recipient)
(defvar lyskom-message-string)


(defun lyskom-send-message-minibuffer-setup-hook ()
  (unwind-protect
      (progn
        (run-hooks 'lyskom-send-message-setup-hook)
        (run-hooks 'kom-send-message-setup-hook))
    (remove-hook 'minibuffer-setup-hook 
                 'lyskom-send-message-minibuffer-setup-hook)))

(defun lyskom-send-message-minibuffer-exit-hook ()
  (unwind-protect
      (progn
        (run-hooks 'lyskom-send-message-exit-hook)
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
              (lyskom-read-string (lyskom-get-string 'message-prompt)
                                  nil
                                  'lyskom-message-history)))
    (setq lyskom-message-recipient (if (zerop pers-no)
                                       nil
                                     (blocking-do 'get-conf-stat 
                                                  pers-no)))

    (run-hooks 'lyskom-send-message-hook)
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
                    '(face kom-dashed-lines-face)
                    '(face kom-text-body-face))
                   lyskom-pers-no
                   kom-filter-outgoing-messages))
            (lyskom-format-insert-before-prompt
             'message-nope 
             (or lyskom-message-recipient
                 (lyskom-get-string 'everybody))
             lyskom-message-string)))
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

(lyskom-external-function resize-minibuffer-setup)

(defun lyskom-send-message-turn-off-resize-on-exit ()
  (resize-minibuffer-mode -1)
  (remove-hook 'kom-send-message-exit-hook
               'lyskom-send-message-turn-off-resize-on-exit))

(defvar resize-minibuffer-mode)
(defun lyskom-send-message-resize-minibuffer ()
  "Temporarily turn on resizing of minibuffer"
  (unless resize-minibuffer-mode
    (resize-minibuffer-mode 1)
    (resize-minibuffer-setup)
    (add-hook 'kom-send-message-exit-hook
              'kom-send-message-turn-off-resize-on-exit)))


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
  (if (or (null conf-no) (zerop conf-no))
      (progn
        (lyskom-insert-string 'not-present-anywhere)
        (lyskom-insert-string "\n"))
    (let ((conf-stat (blocking-do 'get-conf-stat conf-no)))
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
               (result (blocking-do 'set-unread conf-no n))
               (membership (blocking-do 'query-read-texts
                                        lyskom-pers-no
                                        conf-no)))
          (lyskom-ignore result)
          (lyskom-replace-membership membership)
          (if (= conf-no lyskom-current-conf)
              (set-read-list-empty lyskom-reading-list))
          (read-list-delete-read-info conf-no lyskom-to-do-list)
          (if (= conf-no lyskom-current-conf)
              (progn (lyskom-fetch-start-of-map conf-stat membership)
                     (lyskom-go-to-conf lyskom-current-conf t))
            (lyskom-prefetch-map conf-no membership))
          )))))



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
  "Print the number of unread articles to the user. A prefix argument
of zero lists all conferences, a positive prefix arguments sets a
lower bound on the number of unread messages, whereas a negative
prefix argument sets an upper bound on the number of unread messages."
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
        (setq at-least nil
              lyskom-iter-list-news-mship-confs 
              (delq nil
                    (mapcar (lambda (el)
                              (when (not (membership-type->passive
                                          (membership->type el)))
                                (membership->conf-no el)))
                            lyskom-membership))))
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
      (let ((unreads (length (cdr (read-info->text-list info))))
            (conf-stat (read-info->conf-stat info)))
        (when (eq (read-info->type info) 'CONF)
          (if callback
              (apply callback unreads conf-stat callback-args)
            (cons unreads conf-stat))))))
   (read-list->all-entries lyskom-to-do-list)))



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
  "List a summary of the unread in the current conf.
The summary contains the date, number of lines, author and subject of the text
on one line."
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
                         '(CONF REVIEW-MARK REVIEW REVIEW-FAQ))))
          (setq len 0)
        (setq r (1+ r))))
    (setq read-info (read-list->nth read-list r))
    (when read-info
      (lyskom-do-list-summary
       (text-list->texts (read-info->text-list read-info))
       unique))
    ))

(defun lyskom-do-list-summary (texts &optional unique)
  "List a summary of the texts in TEXTS.
The summary contains the date, number of lines, author and subject of the text
on one line."
  (let ((time (lyskom-current-server-time))
        (unique-subjects nil))

    ;; The header.
    (lyskom-format-insert (lyskom-construct-summary-format-string t nil)
                          (lyskom-get-string 'Texts)
                          (lyskom-get-string 'Written)
                          (lyskom-get-string 'Lines)
                          (lyskom-get-string 'Author)
                          (lyskom-get-string 'Subject))

    ;; The summary lines.
    (lyskom-traverse
        text-no texts
      (let ((text-stat (blocking-do 'get-text-stat text-no))
            (text (blocking-do 'get-text text-no))
            ;; We could do som optimization here. 
            ;; We really don't need the whole text.
            )
        (when (or (not unique)
                  (let* ((txt (text->decoded-text-mass text text-stat))
                         (eos (string-match (regexp-quote "\n") txt))
                         (subject (substring txt 0 eos)))
                    (and (not (lyskom-string-member subject unique-subjects))
                         (setq unique-subjects (cons subject unique-subjects)))))
          (lyskom-print-summary-line (lyskom-construct-summary-format-string nil nil)
                                     text-stat text text-no 
                                     (time->year time)
                                     (time->mon time)
                                     (time->mday time)))
        (sit-for 0)))))


(defun lyskom-print-summary-line (format-string text-stat text text-no year mon mday)
  "Handle the info, fetch the author and print it.
Args: FORMAT-STRING TEXT-STAT TEXT TEXT-NO YEAR MON MDAY.

The year, mon and mday are there to be able to choose format on the
day.  Format is HH:MM(:SS) if the text is written today, otherwise
YYYY-MM-DD."
  (if (not (and text-stat text))	;+++ B{ttre felhantering.
      (lyskom-format-insert 'could-not-read text-no)
    (let* ((lines (text-stat->no-of-lines text-stat))
           (txt (text->decoded-text-mass text text-stat))
           (eos (string-match (regexp-quote "\n") txt))
           (subject (substring txt 0 eos))
           (mx-date (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 21)))
           (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 17)))
           (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 16)))
           (time (or (lyskom-mx-date-to-time mx-date)
                     (text-stat->creation-time text-stat)))
           (time (if (and (= year (time->year time))
                          (= mon (time->mon time))
                          (= mday (time->mday time)))
                     (lyskom-format-time 'time time)
                   (lyskom-format-time 'timeformat-yyyy-mm-dd time))))
      (lyskom-format-insert format-string
                            text-no
                            time
                            lines
                            (if (or mx-from mx-author)
                                (lyskom-format-mx-author mx-from mx-author)
                              (text-stat->author text-stat))
                            (lyskom-default-button 'text
                                                   text-no)
                            subject))))


(defun lyskom-max-text-no-width ()
  "Ask the server about the maximum text number width."
  (length (int-to-string (blocking-do 'find-previous-text-no
                                      lyskom-max-int))))


(defun lyskom-construct-summary-format-string (header include-mark-field)
  "Returns a format string suitable for printing text summaries.  If
HEADER is non-nil, a header format string is contructed, otherwise a
format string for a summary row.  If (and only if) INCLUDE-MARK-FIELD
is non-nil, a field where the mark string can be put will be
included."
  (let* ((mark-field-width (when include-mark-field
                             (max (length (lyskom-get-string 'mark-type))
                                  (lyskom-max-mark-width))))
         (text-no-width (lyskom-max-text-no-width))
         (text-no-field-width (max (length (lyskom-get-string 'Texts))
                                   text-no-width))
         (written-field-width (max (length (lyskom-get-string 'Written))
                                   (length (lyskom-format-time 'time))
                                   (length (lyskom-format-time
                                            'timeformat-yyyy-mm-dd))))
         (lines-field-width (max (length (lyskom-get-string 'Lines))
                                 5))
         (author-and-subject-fields-width
          (max (+ (length (lyskom-get-string 'Author))
                  1
                  (length (lyskom-get-string 'Subject)))
               (- (lyskom-window-width)
                  (+ (if include-mark-field
                         (1+ mark-field-width)
                       0)
                     text-no-field-width
                     1
                     written-field-width
                     1
                     lines-field-width
                     1)
                  1)))
         (author-field-width (max (length (lyskom-get-string 'Author))
                                  (/ author-and-subject-fields-width 3)))
         (subject-field-width (- author-and-subject-fields-width
                                 author-field-width
                                 1)))
    (if header
        (concat (if include-mark-field
                    (concat "%-" (int-to-string mark-field-width) "#6s"
                            " ")
                  "")
                "%-" (int-to-string text-no-field-width) "#1s"
                " "
                "%-" (int-to-string written-field-width) "#2s"
                " "
                "%-" (int-to-string lines-field-width) "#3s"
                " "
                "%-" (int-to-string author-field-width) "#4s"
                " "
                "%#5s"
                "\n")
      (concat (if include-mark-field
                  (concat "%-" (int-to-string mark-field-width) "#7s"
                          " ")
                "")
              "%-" (int-to-string text-no-field-width) "#1n"
              " "
              "%-" (int-to-string written-field-width) "#2s"
              " "
              "%" (int-to-string lines-field-width) "#3d"
              " "
              "%=-" (int-to-string (1- author-field-width)) "#4P"
              "  "
              "%[%#5@%=-" (int-to-string subject-field-width) "#6r%]"
              "\n"))))

;;; ============================================================
;;; kom-list-marks                      Lista markeringar
;;; Author: David Byers
;;; Modified by: Joel Rosdahl

(def-kom-command kom-list-marks (&optional which-mark)
  "List texts marked with a particular mark number."
  (interactive "P")
  (when (not (numberp which-mark))
    (setq which-mark (lyskom-read-mark-type
                      (lyskom-get-string 'list-which-mark)
                      t)))
  (let ((marks
         (sort (listify-vector (blocking-do 'get-marks))
               (lambda (a b) (< (mark->mark-type a) (mark->mark-type b)))))
        (time (lyskom-current-server-time)))

    ;; Start fetching all text-stats and text to list them.
    (lyskom-format-insert (lyskom-construct-summary-format-string t t)
                          (lyskom-get-string 'Texts)
                          (lyskom-get-string 'Written)
                          (lyskom-get-string 'Lines)
                          (lyskom-get-string 'Author)
                          (lyskom-get-string 'Subject)
                          (lyskom-get-string 'mark-type))
    (lyskom-traverse
        mark marks
      (when (or (null which-mark)
                (eq (mark->mark-type mark) which-mark))
        (let* ((text-no (mark->text-no mark))
               (text-stat (blocking-do 'get-text-stat text-no))
               (text (blocking-do 'get-text text-no))
            ;; We could do som optimization here. 
            ;; We really don't need the whole text.
            )
          (lyskom-print-mark-summary-line (lyskom-construct-summary-format-string nil t)
                                          mark text-stat text text-no 
                                          (time->year time) (time->yday time))
          (sit-for 0))))
    ))

(defun lyskom-print-mark-summary-line (format-string mark text-stat text text-no year day)
  "Handle the info, fetch the author and print it.
Args: FORMAT-STRING TEXT-STAT TEXT TEXT-NO YEAR DAY.

The year and day is there to be able to choose format on the day.
Format is HH:MM(:SS) if the text is written today, otherwise
YYYY-MM-DD."
  (if (not (and text-stat text))	;+++ B{ttre felhantering.
      (lyskom-format-insert 'could-not-read text-no)
    (let* ((lines (text-stat->no-of-lines text-stat))
           (txt (text->decoded-text-mass text text-stat))
           (eos (string-match (regexp-quote "\n") txt))
           (subject (substring txt 0 eos))
           (time (text-stat->creation-time text-stat))
           (time (if (and (= year (time->year time))
                          (= day (time->yday time)))
                     (lyskom-format-time 'time time)
                   (lyskom-format-time 'timeformat-yyyy-mm-dd time))))
      (lyskom-format-insert format-string
                            text-no
                            time
                            lines
                            (text-stat->author text-stat)
                            (lyskom-default-button 'text
                                                   text-no)
                            subject
                            (mark->symbolic-mark-type mark)))))

(defun lyskom-symbolic-mark-type-string (mark-no)
  "Return a symbolic name string for the mark-type of MARK."
  (or (car-safe (rassoc mark-no kom-symbolic-marks-alist))
      (int-to-string mark-no)))

(defun lyskom-max-mark-width ()
  "Return the maximum width of the user's symbolic mark strings."
  (max 3
       (apply 'max
              (mapcar (function (lambda (x) (length (car x))))
                      kom-symbolic-marks-alist))))

;;; ============================================================
;;;     kom-who-am-i - Vem är jag
;;;
;;; Author: David Byers

(def-kom-command kom-who-am-i ()
  "Show my name"
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
;;;      kom-display-who-buffer - Visa vilka-listan

;;; Author: Linus Tolke


;;(def-kom-command kom-display-who-buffer ()
;;  "Make the who-buffer appear on the screen as a temp buffer."
;;  (interactive)
;;  (let ((win (selected-window))
;;	(who (display-buffer lyskom-who-info-buffer)))
;;    (unwind-protect
;;	(progn
;;	  (select-window who)
;;	  (if (numberp kom-who-buffer-size-when-displaying)
;;	      (enlarge-window (- kom-who-buffer-size-when-displaying 
;;				 (window-height who)))))
;;      (select-window win))))



;;; ================================================================
;;;          Hj{lp vid del av kommando - Help function

;;; Author: Linus Tolke


(defun lyskom-help ()
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
                  (t binding)))
         (keylis (lyskom-help-get-keylist keymap))
         (text (format "\n%s: \n%s\n"
                       (mapconcat 'single-key-description tohere " ")
                       (mapconcat
                        (function
                         (lambda (arg)
                           (format "%s - %s" 
                                   (if (fboundp 'key-description)
                                       (if (not (vectorp (car arg)))
                                           (key-description (vector (car arg)))
                                         (key-description (car arg)))
                                     (cond ((symbolp (car arg))
                                            (format "%s" (car arg)))
                                           ((characterp (car arg))
                                            (format "%c" (car arg)))
                                           (t (format "%S" (car arg)))))
                                   (or (lyskom-command-name (cdr arg))
                                       (and (keymapp (cdr arg))
                                            (lyskom-get-string
                                             'multiple-choice))
                                       (cdr arg)))))
                        keylis
                        "\n")))
         ;; next-char
         )
    (if (eq major-mode 'lyskom-mode)
        (progn
          (lyskom-insert text)
          (lyskom-end-of-command))
      (with-output-to-temp-buffer "*Help*"
        (princ text)))))


(defun lyskom-help-get-keylist (keymap)
  (cond
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




;    (setq next-char (read-char))
;    (cond
;     ((commandp (key-binding (concat tohere (char-to-string next-char))))
;      (command-execute (concat tohere (char-to-string next-char))))
;     (t (lyskom-message "%s" (lyskom-get-string 'does-not-exist))))



;;; ================================================================
;;;      [ndra livsl{ngd - Set lifespan of texts in a conference

;;; Author: Inge Wallin


(def-kom-command kom-set-garb-nice ()
  "Set the garb-nice value for a conference."
  (interactive)
  (let ((conf-stat (lyskom-read-conf-stat
                    (lyskom-get-string 'conf-to-set-garb-nice-q)
                    '(all) nil nil t)))
    (if (not conf-stat)
        (lyskom-insert-string 'somebody-deleted-that-conf)
      (let ((garb-nice (lyskom-read-number 'new-garb-nice-q)))
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
                    '(all) nil nil t)))

    (if (not conf-stat)
        (lyskom-insert-string 'somebody-deleted-that-conf)
      (let ((new-conf (lyskom-read-conf-stat
                       (lyskom-format 'new-permitted-submitters-q
                                      (conf-stat->name conf-stat))
                       '(all) 
                       t nil t)))
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
                    '(all) nil nil t)))
    (if (not conf-stat)
        (lyskom-insert-string 'somebody-deleted-that-conf)
      (let ((new-conf (lyskom-read-conf-stat
                       (lyskom-format 'new-super-conf-q
                                      (conf-stat->name conf-stat))
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
  "Save the LysKOM database."
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
  "Shutdown the LysKOM server."
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


(def-kom-command kom-set-motd ()
  "Set the message of the day for LysKOM."
  (interactive)
  (if (server-info->motd-of-lyskom lyskom-server-info)
      (progn (lyskom-collect 'main)
             (initiate-get-text-stat 'main nil
                         (server-info->motd-of-lyskom lyskom-server-info)) 
             (initiate-get-text 'main nil
                         (server-info->motd-of-lyskom lyskom-server-info))
             (lyskom-use 'main 'lyskom-set-motd))
    (lyskom-set-motd nil nil)))


(defun lyskom-set-motd (old-motd-text-stat old-motd-text)
  "Set the message of the day for LysKOM. 
Use OLD-MOTD-TEXT as the default text if non-nil."

  (let ((str (and old-motd-text 
                  old-motd-text-stat
                  (text->decoded-text-mass old-motd-text old-motd-text-stat))))
    (lyskom-edit-text
     lyskom-proc
     (lyskom-create-misc-list)
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
  "Postpone the reading of all but the last TODAY articles in the
current conference to another session."
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
          (let* ((rlist (read-info->text-list
                         (read-list->first lyskom-reading-list)))
                 (cell (nthcdr (max (- (length rlist) today) 1)
                               rlist)))
            (setcdr rlist cell))
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
  "Set the priority level of the current session.
This sets the variable kom-session-priority and refetches all
membership info."
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
;;;    (if lyskom-debug-communications-to-buffer
;;;	(bury-buffer lyskom-debug-communications-to-buffer-buffer))
    (if lyskom-who-info-buffer
        (bury-buffer lyskom-who-info-buffer))
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

;;;(defun lyskom-update-lyskom-buffer-list ()
;;;  (mapcar (function
;;;	   (lambda (buf)
;;;	     (if (and (lyskom-buffer-p buf)
;;;		      (not (memq buf lyskom-buffer-list)))
;;;		 ;; This is a LysKOM buffer that we haven't seen yet --
;;;		 ;; If it is the current buffer, add it at the start
;;;		 ;; of lyskom-buffer-list, otherwise add it to the end
;;;		 (if (eq buf (current-buffer))
;;;		     (setq lyskom-buffer-list (cons buf
;;;						    lyskom-buffer-list))
;;;		   (setq lyskom-buffer-list
;;;			 (nconc lyskom-buffer-list (list buf)))))))
;;;	  (buffer-list))
;;;  (mapcar (function
;;;	   (lambda (buf)
;;;	     (if buf
;;;		 (setq lyskom-buffer-list
;;;		       (delete buf lyskom-buffer-list)))))
;;;	  (mapcar (function
;;;		   (lambda (buf)
;;;		     (if (lyskom-buffer-p buf) nil buf)))
;;;		  lyskom-buffer-list)))


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
  "Pop up the next lyskom-session."
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
  "Pop up the previous lyskom-session."
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
  "Pop up the previous lyskom-session."
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
                                       (or (cdr (assoc lyskom-server-name
                                                       kom-server-aliases))
                                           lyskom-server-name)
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
    (when (and kom-bury-buffers (eq current (current-buffer)))
      (kom-bury))

    ;; Switch to the new buffer
    (switch-to-buffer buffer)))






;;; ============================================================
;;; Var finns kommandot                 (kom-where-is)
;;; Author: David Byers

(def-kom-emacs-command kom-where-is (cmd)
  "Show on which key a LysKOM command is"
  (interactive
   (list (lyskom-read-extended-command)))
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

(defun kom-show-user-area ()
  "Get and display the user area of the current person"
  (interactive)
  (lyskom-start-of-command 'kom-show-user-area)
  (let ((pers-stat (blocking-do 'get-pers-stat lyskom-pers-no)))
    (lyskom-view-text (pers-stat->user-area pers-stat)
                      nil nil nil nil nil)
    (lyskom-run 'main 'lyskom-end-of-command)))


;;;============================================================
;;; Bli anonym

(def-kom-command kom-become-anonymous ()
  "Become pseudo-anonymous"
  (interactive)
  (if lyskom-is-anonymous
      (lyskom-insert 'you-are-already-anonymous)
;    (initiate-pepsi 'main nil 0)
    (setq lyskom-is-anonymous t)
    (lyskom-tell-server kom-mercial)
    (lyskom-insert 'you-are-anonymous)
    (lyskom-update-prompt t)))

(def-kom-command kom-become-nonanonymous ()
  "Leave pseudo-anonymous mode"
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
  "Change type of a conference"
  (interactive)
  (let* ((uconf-stat (lyskom-read-uconf-stat
                      (lyskom-get-string 'what-conf-to-change)
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
                            (and ope (lyskom-get-string 'allow-secret))))
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

(def-kom-command kom-change-language (&optional global)
  "Change the current language in LysKOM

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
      (lyskom-set-language (cdr (lyskom-string-assoc language table)) 
                           (if global 'global 'local)))))



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

(lyskom-external-function calc-eval)
(def-kom-command kom-calculate (&optional exprx)
  "Calculate optional arg EXPRX, or prompt the user for an expression."
  (interactive)
  (when (lyskom-try-require 'calc 
                            (lyskom-get-string 'need-library))
    (let* ((expr (or exprx
                     (lyskom-with-lyskom-minibuffer
                      (lyskom-read-from-minibuffer 
                       (lyskom-get-string 'calc-expression)
                       nil nil nil 'lyskom-expression-history))))
           (result (calc-eval expr)))
      (cond ((stringp result)
             (lyskom-format-insert-before-prompt
              "%#1s = \n    %#2s\n" expr result))
            (t (lyskom-format-insert-before-prompt
                "%#1s = \n%#2s^ %#3s\n"
                expr
                (make-string (car result) ?\ )
                (car (cdr result))))))))

;;; ============================================================
;;; Ändra namn

(def-kom-command kom-set-personal-label ()
  "Set a personal label on an object of some kind"
  (interactive)
  (let* ((completions (list (cons (lyskom-get-string 'conference) 'conf)
                            (cons (lyskom-get-string 'person) 'pers)
                            (cons (lyskom-get-string 'text) 'text)))
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
                 (lyskom-with-lyskom-minibuffer
                  (lyskom-read-from-minibuffer 
                   (lyskom-get-string 'label-what-label))))
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
                 (lyskom-read-conf-stat (lyskom-get-string 
                                         (if (eq type 'pers)
                                             'label-what-pers
                                           'label-what-conf))
                                         (if (eq type 'pers)
                                             '(pers)
                                           '(all))
                                         nil nil t))
           (setq objno (conf-stat->conf-no object))
           (setq aux (conf-stat-find-aux object 10 lyskom-pers-no))
           (setq secret (not (lyskom-j-or-n-p
                              (lyskom-get-string 'label-secret))))
           (setq label
                 (lyskom-with-lyskom-minibuffer
                  (lyskom-read-from-minibuffer 
                   (lyskom-get-string 'label-what-label))))
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
  "Add a remark (fast reply) to a text."
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
  "Convenience function to add agreement."
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
  (lyskom-report-command-answer
   (blocking-do 'modify-text-info
                text-no
                nil
                (list (lyskom-create-aux-item 0 2 0 0 
                                              (lyskom-create-aux-item-flags
                                               nil nil nil nil nil nil nil nil)
                                              0 message)))))


;;; ============================================================
;;; Various aux-item stuff

(def-kom-command kom-add-no-comments (&optional text-no)
  "Add a don't comment me please aux-item to a text."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-no-comments-no nil
                                                     'last-seen-written)))
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
  "Add a private answer only please aux-item to a text."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-private-answer-no nil
                                                     'last-seen-written)))
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
  "Add confirmation request aux-item to a text."
  (interactive (list (lyskom-read-text-no-prefix-arg
                      'what-request-confirm-no nil 'last-seen-written)))
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
  "Review the mail headers of an imported message"
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
  "Keep the LysKOM session alive by sending a request every once in a while.
The variable kom-keep-alive-interval controls the frequency of the request."
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
  "Stop sending periodic requests to keep the session alive."
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
  "Find out whether PERS-NO is a member of conference CONF-NO."
  (interactive)
  (let* ((pers-no
          (or pers-no
              (lyskom-read-conf-no (lyskom-get-string 'pers-to-check-mship-for)
                                   '(all) nil nil t)))
         (conf-no
          (or conf-no
              (lyskom-read-conf-no (lyskom-get-string 'conf-to-check-mship-of)
                                   '(all) nil nil t)))
	 (mship (lyskom-is-member conf-no pers-no)))
    (if mship
	(if (membership-type->passive (membership->type mship))
	    (lyskom-format-insert 'pers-is-passive-member-of-conf
				  pers-no conf-no)
	  (lyskom-format-insert 'pers-is-member-of-conf pers-no conf-no))
      (lyskom-format-insert 'pers-is-not-member-of-conf pers-no conf-no))))

(def-kom-command kom-help ()
  "Get some help with LysKOM"
  (interactive)
  (let* ((completion-ignore-case t)
         (table (mapcar (lambda (x)
                          (cons (lyskom-get-string (car x) 'lyskom-help-strings)
                                (car x)))
                        lyskom-help-categories))
         (category
          (lyskom-completing-read (lyskom-get-string 'help-with-what)
                                  table
                                  nil
                                  t
                                  nil
                                  'lyskom-help-history)))
    (lyskom-format-insert "Hjälp för %#1s\n\n" (car (lyskom-string-assoc category table)))
    (lyskom-display-help-category 
     (cdr (lyskom-string-assoc category table)))))

(defun lyskom-display-help-category (category &optional flags)
  "Display help category CATEGORY."
  (let ((cat-name (lyskom-get-string category 'lyskom-help-strings)))
    (lyskom-format-insert "%#1@%[%#2s%]\n" '(face bold) cat-name)
    (lyskom-insert (make-string (length cat-name) ?=))
    (lyskom-insert "\n")
    (unless (memq 'summary flags) (lyskom-insert "\n"))
    (mapcar (lambda (x) (lyskom-display-help-item x flags))
            (cdr (assq category lyskom-help-categories))
            )
    (when (memq 'summary flags) (lyskom-insert "\n"))))

(defun lyskom-display-help-item (item &optional flags)
  "Display help item ITEM."
  (let ((type (elt item 0)))
    (cond ((eq type 'command)
           (let* ((command (elt item 1))
                  (command-name (lyskom-get-string command 'lyskom-command)))
             (lyskom-format-insert "%#1@%[%#2s%]\n"
                                   '(face italic)
                                   command-name)
             (unless (memq 'summary flags)
               (lyskom-format-insert "%#1s\n%#2s\n\n"
                                     (make-string (length command-name) ?-)
                                     (lyskom-get-string command
                                                        'lyskom-help-strings))))
           )

          ((eq type 'category) 
           (let ((category (elt item 1))
                 (flags (and (> (length item) 2)
                             (elt item 2))))
             (lyskom-display-help-category category flags))))))

(def-kom-command kom-make-review-mark-as-read ()
  "Makes all review commands mark texts as read. Overrides the value of the
configurable variable `kom-review-marks-texts-as-read' in the current buffer."
  (interactive)
  (setq kom-review-marks-texts-as-read t))

(def-kom-command kom-make-review-not-mark-as-read ()
  "Makes all review commands not mark texts as read. Overrides the value of the
configurable variable `kom-review-marks-texts-as-read' in the current buffer."
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
(def-kom-command kom-compare-texts (old new)
  "Show differences between text OLD and NEW.
When called interactively, it will prompt for the NEW text first,
defaulting to the last viewed texts.  The OLD text number will default
to the first text that NEW is a comment or footnote to."
  (interactive
   (let* ((n (lyskom-read-text-no-prefix-arg 'diff-what-text-new t))
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
    (condition-case nil
        (ediff-buffers
         (lyskom-create-text-buffer old old-text old-text-stat)
         (lyskom-create-text-buffer new new-text new-text-stat))
      (file-error (lyskom-error (lyskom-get-string 'external-program-missing)
                                (if (boundp 'diff-command)
                                    diff-command
                                  "diff"))))))

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
  "Show differences between text OLD and NEW.
When called interactively, it will prompt for the NEW text first,
defaulting to the last viewed texts.  The OLD text number will default
to the first text that NEW is a comment or footnote to."
  (interactive
   (let* ((n (lyskom-read-text-no-prefix-arg 'diff-what-text-new t))
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
    (let* ((buf (current-buffer))
	   (oldfile (lyskom-create-temp-file old old-text old-text-stat))
	   (newfile (lyskom-create-temp-file new new-text new-text-stat))
	   (args (list "-L" (format "%d\t%s" old
				    (lyskom-format-time
				     'timeformat-yyyy-mm-dd-hh-mm-ss
				     (text-stat->creation-time old-text-stat)))
		       "-L" (format "%d\t%s" new
				    (lyskom-format-time
				     'timeformat-yyyy-mm-dd-hh-mm-ss
				     (text-stat->creation-time new-text-stat)))
		       oldfile newfile)))

      (if switches
	  (setq args (append
		      (split-string (if (consp switches)
					(mapconcat 'identity switches " ")
				      switches))
			     args))
	(if diff-switches
	  (setq args (append
		      (split-string (if (consp diff-switches)
					(mapconcat 'identity diff-switches " ")
				      diff-switches))
		      args))))
	    

      (set-buffer buf)
      (let ((buffer-read-only nil))
	(apply 'call-process (if (boundp 'diff-command)
                                 diff-command
                               "diff")
               nil buf nil args))
      (delete-file oldfile)
      (delete-file newfile))))
      
    

;;; ================================================================
;;;             Skapa aux-item

(def-kom-command kom-create-aux-item ()
  "Create arbitrary aux-items"
  (interactive)
  (let* ((completions
          (mapcar (lambda (x) (cons (lyskom-get-string x) x))
                  '(server conference text)))
         (completion-ignore-case t)
         (object-type 
          (cdr (lyskom-string-assoc 
                (lyskom-completing-read (lyskom-get-string 'what-kind-to-add-aux-to)
                                        completions
                                        nil t)
                completions)))
         (object-id (cond ((eq object-type 'server) nil)
                           ((eq object-type 'conference)
                            (lyskom-read-conf-no 'which-conf-to-add-aux-to '(pers conf)))
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
  "Show status information for the LysKOM server"
  (interactive)
  (blocking-do-multiple ((server-info (get-server-info))
                         (server-version (get-version-info))
                         (server-time (get-time))
                         (highest-text (find-previous-text-no lyskom-max-int))
                         (first-text (find-next-text-no 0))
                         (session-info (who-is-on-dynamic  t t 0)))
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
           (idle-hide (* 60 (if (numberp kom-idle-hide) kom-idle-hide 30))))

      (setq aux-items (delq canonical-name-aux aux-items))

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
                            (cond ((cdr (lyskom-string-assoc lyskom-server-name kom-server-aliases)))
                                  ((cdr (lyskom-string-rassoc lyskom-server-name kom-server-aliases)))
                                  (t lyskom-server-name))
                            (cond ((car (lyskom-string-rassoc lyskom-server-name kom-server-aliases)))
                                  (t lyskom-server-name))
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
      ;; Print info on text numbers
      (lyskom-format-insert 'server-status-first-text first-text)
      (lyskom-format-insert 'server-status-last-text highest-text)

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
      ;; Print MOTD (if there is one)
      (when (not (zerop (server-info->motd-of-lyskom server-info)))
        (lyskom-insert 'server-status-has-motd)
        (lyskom-view-text (server-info->motd-of-lyskom server-info)))

)))


;;; ================================================================
;;; Rekommendera möte

(def-kom-command kom-recommend-conference ()
  "Recommend a conference to new LysKOM users"
  (interactive)
  (let* ((conf-stat (lyskom-read-conf-stat 'recommend-which-conf
                                           '(conf)
                                           nil
                                           nil
                                           t))
         (priority (and (lyskom-j-or-n-p 'recommend-set-priority-q)
                        (lyskom-read-num-range 0 255 'priority-q)))
         (mship-type (and priority
                          (lyskom-j-or-n-p 'recommend-set-mship-type-q)
                          (lyskom-read-membership-type)))

         (aux-item (lyskom-create-aux-item 
                    0
                    29
                    nil
                    nil
                    (lyskom-create-aux-item-flags nil
                                                  nil
                                                  nil
                                                  nil
                                                  nil
                                                  nil
                                                  nil
                                                  nil)
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
someone posts them in a conference you are not a member of.

Note that this functionality depends on the client. Not all clients
will honor this redirection."
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
                                      '(face kom-warning-face))
  ;; Beep both visibly and audibly, if possible.  We *want* to be annoying.
  (let ((visible-bell t))
    (ding))
  (let ((visible-bell nil))
    (ding))
  (read-from-minibuffer 
   (lyskom-format "%#1@Tryck return eller enter för att gå vidare: " '(face kom-warning-face))))


(defun kom-obsolete-who-is-on-in-conference ()
  "Temporary function for when we moved kom-who-is-on-in-conference from l v to 
v m"
  (interactive)
  (lyskom-insert-before-prompt "Kommandot \"Vilka (är inloggade i) möte\" är flyttat till v m\n"))

