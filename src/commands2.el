;;;;;
;;;;; $Id: commands2.el,v 44.23 1997-12-04 20:39:34 byers Exp $
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
;;;; File: commands2.el
;;;;
;;;; This file contains the code for some high level commands.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: commands2.el,v 44.23 1997-12-04 20:39:34 byers Exp $\n"))

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
          (inhibit-read-only t))
      (lyskom-format-insert 'memberships-line
                            (lyskom-return-date-and-time
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
      (let ((superconf (conf-stat->super-conf conf-stat)))
	(lyskom-format-insert 'superconf-is-no-name
			      superconf
			      superconf
			      (if (and 
				   (lyskom-conf-stat-p superconf)
				   (> (length (conf-stat->name superconf))
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

      ;; Show all members of CONF-STAT if the user so wishes."
      (lyskom-scroll)
      (if (lyskom-j-or-n-p
	   (lyskom-get-string 'show-members-list-also-q))
	  (let ((member-list (blocking-do 'get-members
					  (conf-stat->conf-no conf-stat)
					  0 lyskom-max-int)))
	    
	    (lyskom-format-insert 'conf-has-these-members
				  conf-stat)
	    (if (lyskom-j-or-n-p (lyskom-get-string 'show-membership-info-q))
		
		(progn
		  (lyskom-insert-string 'member-list-header)
		  (lyskom-traverse 
		   member (conf-no-list->conf-nos member-list)
		   (let ((membership (blocking-do
				      'query-read-texts 
				      member
				      (conf-stat->conf-no conf-stat))))
		     ;; Print a row describing the membership of MEMBER
		     ;; (described by MEMBERSHIP) in CONF-STAT.
		     (if (or (null membership))
			 (lyskom-insert-string 'secret-membership)
		       (lyskom-insert 
			(format "%17s"
				(lyskom-return-date-and-time
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
						   "         "
						 (format "%7d  " unread))
					       member))))))
	      
	      ;; Don't show membership info
	      (lyskom-insert "\n")
	      (lyskom-traverse
	       member (conf-no-list->conf-nos member-list)
	       (lyskom-format-insert "  %#1P\n" member))))))))


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
                                  '(pers) nil "" t)))
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
  (let ((target (or who
		    (lyskom-read-conf-no
		     (format (lyskom-get-string 'who-to-send-message-to)
			     (lyskom-get-string 'everybody))
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

		      (t 
		       (if lyskom-last-personal-message-sender
			   (cons lyskom-last-personal-message-sender 0)
			 "")))
                     t))))
    (if (zerop target)
	(lyskom-insert (lyskom-get-string 'message-all-info))
      (lyskom-format-insert 'message-recipient-info target))
    (lyskom-send-message target message)))
  

(def-kom-command kom-send-alarm (&optional message)
  "Send a message to all of the users in KOM right now."
  (interactive)
  (lyskom-send-message 0 message))


(defvar lyskom-message-recipient)
(defvar lyskom-message-string)


(defun lyskom-send-message-minibuffer-setup-hook ()
  (unwind-protect
      (run-hooks 'lyskom-send-message-setup-hook)
    (remove-hook 'minibuffer-setup-hook 
                 'lyskom-send-message-minibuffer-setup-hook)))

(defun lyskom-send-message-minibuffer-exit-hook ()
  (unwind-protect
      (run-hooks 'lyskom-send-message-exit-hook)
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
    (if lyskom-message-string
        (progn
          (setq reply (blocking-do 'send-message pers-no
                                   lyskom-message-string))

          (if reply
              (if (not dontshow)
                  (lyskom-handle-as-personal-message
                   (if lyskom-message-recipient
                       (lyskom-format 'message-sent-to-user
                                      lyskom-message-string 
                                      lyskom-message-recipient)
                     (lyskom-format 'message-sent-to-all
                                    lyskom-message-string))
                   lyskom-pers-no
                   lyskom-filter-outgoing-messages))
            (lyskom-format-insert-before-prompt
             'message-nope 
             (or lyskom-message-recipient
                 (lyskom-get-string 'everybody))
             lyskom-message-string)))
      (lyskom-insert-string 'interrupted))                   ;+++ lyskom-errno
    ))

(defun lyskom-send-message-trim-newlines ()
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
                                                    0 size))))))

(lyskom-external-function lyskom-resize-minibuffer-mode)
(lyskom-external-function resize-minibuffer-setup)

(defun lyskom-send-message-turn-off-resize-on-exit ()
  (resize-minibuffer-mode -1)
  (remove-hook 'lyskom-send-message-exit-hook
               'lyskom-send-message-turn-off-resize-on-exit))
               
(defun lyskom-send-message-resize-minibuffer ()
  "Temporarily turn on resizing of minibuffer"
  (let ((tmp nil))
    (if (not resize-minibuffer-mode)
        (progn
          (if (not (memq 'resize-minibuffer-setup minibuffer-setup-hook))
              (setq tmp t))
          (resize-minibuffer-mode 0)
          (if tmp (resize-minibuffer-setup))
          (add-hook 'lyskom-send-message-exit-hook
                    'lyskom-send-message-turn-off-resize-on-exit)))))


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
  (if (and (zerop lyskom-current-conf) (null conf-no))
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
					conf-no))
	       )
          (ignore result)
	  (lyskom-replace-membership membership lyskom-membership)
	  (if (= conf-no lyskom-current-conf)
	      (set-read-list-empty lyskom-reading-list))
	  (read-list-delete-read-info conf-no lyskom-to-do-list)
	  (lyskom-prefetch-map conf-no membership)
	  )))))



;;; ================================================================
;;;                 Lista Nyheter - List News

;;; Author:   Linus Tolke
;;; Rehacked: Inge Wallin


(defvar lyskom-special-conf-name "\\`Inl.gg .t mig\\'"
  "Regexp to match conf names that are special.")

(def-kom-command kom-list-news (&optional num)
  "Print the number of unread articles to the user."
  (interactive "P")

  (unless kom-allow-incompleteness
    (sit-for 0)
    (lyskom-prefetch-all-confs))

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
	(let ((len (read-list-length lyskom-reading-list))
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
  (let ((time (blocking-do 'get-time))
	(author-width (/ (- (lyskom-window-width) 22) 3)))
    
    ;; Start fetching all text-stats and text to list them.
    (lyskom-format-insert (concat "%-8#1s%-6#2s%-4#3s %-"
				  (int-to-string author-width)
				  "#4s  %#5s\n")
			  (lyskom-get-string 'Texts)
			  (lyskom-get-string 'Date)
			  (lyskom-get-string 'Lines)
			  (lyskom-get-string 'Author)
			  (lyskom-get-string 'Subject))
    (lyskom-traverse
	text-no texts
      (let ((text-stat (blocking-do 'get-text-stat text-no))
	    (text (blocking-do 'get-text text-no))
	    ;; We could do som optimization here. 
	    ;; We really don't need the whole text.
	    )
	(lyskom-print-summary-line text-stat text text-no 
				   (time->year time) (time->yday time))
	(sit-for 0)))))


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
	   ;; length of the number %%%%%% :8
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
	   (namelen (/ (- (lyskom-window-width) 22) 3))
	   (subjlen (/ (* (- (lyskom-window-width) 22) 2) 3))
	   (format-string (concat "%=-8#1n%#2s%4#3d  %=-"
				  (int-to-string namelen)
				  "#4P  %[%#5@%=-"
				  (int-to-string subjlen)
				  "#6r%]\n")))
      (lyskom-format-insert format-string
			    text-no
			    time
			    lines
			    (text-stat->author text-stat)
			    (lyskom-default-button 'text
						   text-no)
			    subject))))



;;; ============================================================
;;; kom-list-marks                      Lista markeringar
;;; Author: David Byers

(def-kom-command kom-list-marks (&optional which-mark)
  (interactive "P")
  (when (not (numberp which-mark))
    (setq which-mark (lyskom-read-num-range 
                      0 255
                      (lyskom-get-string 'list-which-mark) nil nil nil t)))
  (let ((marks
         (sort (listify-vector (blocking-do 'get-marks))
               (lambda (a b) (< (mark->mark-type a) (mark->mark-type b))))))
  (let ((time (blocking-do 'get-time))
	(author-width (/ (- (lyskom-window-width) 26) 3)))
    
    ;; Start fetching all text-stats and text to list them.
    (lyskom-format-insert (concat "%-4#6s%-8#1s%-6#2s%-4#3s %-"
				  (int-to-string author-width)
				  "#4s  %#5s\n")
			  (lyskom-get-string 'Texts)
			  (lyskom-get-string 'Date)
			  (lyskom-get-string 'Lines)
			  (lyskom-get-string 'Author)
			  (lyskom-get-string 'Subject)
                          (lyskom-get-string 'mark-type))
;    (lyskom-insert (make-string (1- (window-width)) ?-))
;    (lyskom-insert "\n")
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
          (lyskom-print-mark-summary-line mark text-stat text text-no 
                                     (time->year time) (time->yday time))
          (sit-for 0))))
;    (lyskom-insert (make-string (1- (window-width)) ?-))
;    (lyskom-insert "\n")
    )))
                           
(defun lyskom-print-mark-summary-line (mark text-stat text text-no year day)
  "Handle the info, fetch the author and print it.
Args: TEXT-STAT TEXT TEXT-NO YEAR DAY.
The year and day is there to be able to choose format on the day.
Format is 23:29 if the text is written today. Otherwise 04-01."
  (lyskom-format-insert "%3#1d " (mark->mark-type mark))
  (if (not (and text-stat text))	;+++ B{ttre felhantering.
      (lyskom-format-insert 'could-not-read text-no)
    (let* ((lines (text-stat->no-of-lines text-stat))
	   (txt (text->text-mass text))
	   (eos (string-match (regexp-quote "\n") txt))
	   (subject (substring txt 0 eos))
	   ;; length of the number %%%%%% :8
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
	   (namelen (/ (- (lyskom-window-width) 26) 3))
	   (subjlen (/ (* (- (lyskom-window-width) 26) 2) 3))
	   (format-string (concat "%=-8#1n%#2s%4#3d  %=-"
				  (int-to-string namelen)
				  "#4P  %[%#5@%=-"
				  (int-to-string subjlen)
				  "#6r%]\n")))
      (lyskom-format-insert format-string
			    text-no
			    time
			    lines
			    (text-stat->author text-stat)
			    (lyskom-default-button 'text
						   text-no)
			    subject))))


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
                        lyskom-clientversion)

  (lyskom-format-insert 'who-i-am-emacs
                        (emacs-version)))
                        


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
;;;           Skapa bugg-rapport - Compile bugg-report

;;; Author: Linus Tolke


(defun kom-bug-report ()
  "This command should make it easier to include the correct info in a buggreport"
  (interactive)
  (let* ((curbuf (current-buffer))
         (old-buf (if (boundp 'debugger-old-buffer)
                      (symbol-value 'debugger-old-buffer)
                    (current-buffer)))
         (repname "*lyskom-bugreport*"))
    (lyskom-message "%s" (lyskom-get-string 'buggreport-compilestart))
    (set-buffer old-buf)
    (cond
     ((condition-case nil
          (eq old-buf (process-buffer lyskom-proc))
        (error nil)))
     ((condition-case nil
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
      (print (condition-case nil
                 (documentation-property 'ctl-arrow 'variable-documentation)
               (error)))

      (princ (lyskom-get-string 'buggreport-unparsed))
      (print (save-excursion
               (set-buffer lyskom-unparsed-buffer)
               (goto-char (point-min))
               (forward-line 10)
               (buffer-substring (point-min) (point))))
      (if (and (boundp 'debugger-old-buffer) 
               (symbol-value 'debugger-old-buffer))
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
\\|-map$\\|^lyskom-commands$"
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
		    '(all) nil nil t)))
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
		 (lyskom-read-num-range 0
                                255
                                (lyskom-get-string 'set-session-priority)
                                t
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
;;;    (if lyskom-debug-communications-to-buffer
;;;	(bury-buffer lyskom-debug-communications-to-buffer-buffer))
    (if lyskom-who-info-buffer
	(bury-buffer lyskom-who-info-buffer))
    (bury-buffer)
    (while (and (string-match (regexp-quote session-name)
			      (buffer-name (current-buffer)))
		(not (eq buffer (current-buffer))))
      (bury-buffer))))


(defun lyskom-buffer-p (buf)
  ;; Returns non-nil if BUF is an active LysKOM buffer
  (if (and buf (bufferp buf) (buffer-name buf))
      (save-excursion
	(set-buffer buf)
	(and (eq major-mode 'lyskom-mode)
	     (boundp 'lyskom-proc)
	     lyskom-proc
	     (processp lyskom-proc)
	     (memq (process-status lyskom-proc) '(run open))))))

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



(defun lyskom-next-kom ()
  "Internal version of kom-next-kom"
  (if lyskom-buffer-list
      (progn
	(if (lyskom-buffer-p (car lyskom-buffer-list))
	    ;; If there is an "active" lyskom buffer, send it to the
	    ;; back of the list.
	    (progn
	      ;; If the "active" lyskom buffer is the current buffer,
	      ;; and kom-bury-buffers is non-nil, bury it.
	      (if (and kom-bury-buffers
		       (eq (car lyskom-buffer-list)
			   (current-buffer)))
		  (kom-bury))
	      (setq lyskom-buffer-list
		    (nconc (cdr lyskom-buffer-list)
			   (list (car lyskom-buffer-list)))))
	  ;; The "active" lyskom buffer is dead, so we remove it from
	  ;; some lists.
	  (setq lyskom-sessions-with-unread
		(delq (car lyskom-buffer-list)
		      lyskom-sessions-with-unread))
	  (setq lyskom-sessions-with-unread-letters
		(delq (car lyskom-buffer-list)
		      lyskom-sessions-with-unread-letters))
	  (setq lyskom-buffer-list (cdr lyskom-buffer-list)))
	;; Don't switch to dead sessions.
	(if (lyskom-buffer-p (car lyskom-buffer-list))
	    (switch-to-buffer (car lyskom-buffer-list))
	  (lyskom-next-kom)))
    (error "No active LysKOM buffers")))

;; (defun lyskom-next-kom ()
  "Internal version of kom-next-kom"

  ;; Find the lyskom-buffer to go from, to the next
  ;;    If we are in a lyskom buffer that is in lyskom-buffer-list
  ;;                    switch from the current buffer.
  ;;    If we are in a lyskom buffer that is unlisted, 
  ;;                    put this buffer in the list and switch from it
  ;;    If we are in another buffer, switch from the last listed buffer

  ;;    If we are switching from nil, then the target buffer is
  ;;                    the first buffer in lyskom-buffer-list.
  ;;    If we are switching from a buffer, then the target buffer is the
  ;;                    first in the rest of the list, or the first buffer
  ;;                    if we are at the end of the list
  ;;    Splice the list as follows:
  ;;    Put the rest of the list, after the current buffer in front of the
  ;;                    buffer list, and remove it from the back. Now the
  ;;                    buffer we switched from is at the back and the one
  ;;                    we want to switch to is at the front.
  ;;    Iterate over the buffer list and switch to the first buffer that
  ;;                    is active. Remove any buffers that are inactive.
  ;;    If the new target buffer after this is the one we were switching
  ;;                    from, then signal an error
;;)


(defun lyskom-previous-kom ()
  "Internal version of kom-previous-kom"
  (if (> (length lyskom-buffer-list) 1)
      (let (lastbuf
	    (last-but-one lyskom-buffer-list))
	(while (cdr (cdr last-but-one))
	  (setq last-but-one (cdr last-but-one)))
	(setq lastbuf (car (cdr last-but-one)))
	(if (lyskom-buffer-p (car lyskom-buffer-list))
	    ;; If there is an "active" lyskom buffer, send it to the
	    ;; back of the list.
	    (progn
	      ;; If the "active" lyskom buffer is the current buffer,
	      ;; and kom-bury-buffers is non-nil, bury it.
	      (if (and kom-bury-buffers
		       (eq (car lyskom-buffer-list)
			   (current-buffer)))
		  (kom-bury))
	      (setq lyskom-buffer-list (cons lastbuf lyskom-buffer-list))
	      (rplacd last-but-one nil))
	  ;; The "active" lyskom buffer is dead, so we remove it from
	  ;; some lists.
	  (setq lyskom-sessions-with-unread
		(delq (cdr last-but-one)
		      lyskom-sessions-with-unread))
	  (setq lyskom-sessions-with-unread-letters
		(delq (cdr last-but-one)
		      lyskom-sessions-with-unread-letters))
	  (rplacd last-but-one nil))
	(if (lyskom-buffer-p (car last-but-one))
	    (switch-to-buffer lastbuf)
	  (lyskom-previous-kom)))
    (if (null lyskom-buffer-list)
        (error
         (lyskom-get-string 'no-lyskom-session)))))



(def-kom-emacs-command kom-next-kom ()
  "Pop up the next lyskom-session."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (if (lyskom-buffer-p (current-buffer))
        (lyskom-tell-internat 'kom-tell-next-lyskom))
    (lyskom-next-kom)
    (when (eq (current-buffer) start-buffer)
      (if kom-next-kom-running-as-kom-command
          (lyskom-insert-before-prompt (lyskom-get-string 'no-other-lyskom-r))
        (error (lyskom-get-string 'no-lyskom-session))))))


(def-kom-emacs-command kom-previous-kom ()
  "Pop up the previous lyskom-session."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (if (lyskom-buffer-p (current-buffer))
        (lyskom-tell-internat 'kom-tell-next-lyskom))
    (lyskom-previous-kom)
    (when (eq (current-buffer) start-buffer)
      (if kom-previous-kom-running-as-kom-command
          (lyskom-insert-before-prompt (lyskom-get-string 'no-other-lyskom-r))
        (error 'no-lyskom-session)))))


(def-kom-emacs-command kom-next-unread-kom ()
  "Pop up the next LysKOM session with unread texts in."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (if (not (lyskom-buffer-p (current-buffer)))
        (lyskom-next-kom))
    (let ((thisbuf (current-buffer)))
      (lyskom-next-kom)
      (while (and (not (eq thisbuf (current-buffer)))
                  (not (memq lyskom-buffer lyskom-sessions-with-unread)))
        (lyskom-next-kom)))
    (when (eq start-buffer (current-buffer))
        (if kom-next-unread-kom-running-as-kom-command
            (lyskom-insert-before-prompt 
             (lyskom-get-string 'no-unread-lyskom-r))
          (error (lyskom-get-string 'no-unread-lyskom))))))


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
  (let* ((conf-stat (lyskom-read-conf-stat
                     (lyskom-get-string 'what-conf-to-change)
                     '(conf pers) nil "" t))
	 (type (conf-stat->conf-type conf-stat))
	 (box (conf-type->letterbox type))
	 (ori (conf-type->original type))
	 (pro (conf-type->rd_prot type))
	 (sec (conf-type->secret type)))
    (lyskom-format-insert 'change-type-prompt
			  conf-stat
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
			   (t "")))
    (let* ((open (lyskom-j-or-n-p (lyskom-get-string 'anyone-member)))
	   (secret (if (not open)
		       (lyskom-j-or-n-p (lyskom-get-string 'secret-conf))))
	   (orig (lyskom-j-or-n-p (lyskom-get-string 'comments-allowed)))
	   (anarchy (lyskom-j-or-n-p (lyskom-get-string 'anonymous-allowed))))
      (cache-del-conf-stat (conf-stat->conf-no conf-stat))
      (if (not (blocking-do 
		'set-conf-type
		(conf-stat->conf-no conf-stat)
		(lyskom-create-conf-type (not open)
					 (not orig)
					 secret
					 (conf-type->letterbox
					  (conf-stat->conf-type  conf-stat))
					 anarchy
					 (conf-type->rsv1
					  (conf-stat->conf-type conf-stat))
					 (conf-type->rsv2
					  (conf-stat->conf-type conf-stat))
					 (conf-type->rsv3
					  (conf-stat->conf-type conf-stat)))))
	  (progn (lyskom-insert-string 'nope)
		 (lyskom-format-insert 'error-code
				       (lyskom-get-error-text lyskom-errno)
				       lyskom-errno))))))


;;; ============================================================
;;; Ändra språk
;;;

(def-kom-command kom-change-language ()
  "Change the current language in LysKOM"
  (interactive)
  (let* ((completion-ignore-case t)
         (table (lyskom-available-language-list))
         (language (completing-read
                   (lyskom-get-string 'which-language)
                   table
                   nil
                   t
                   nil
                   'lyskom-language-history)))
    (when (lyskom-string-assoc language table)
      (lyskom-set-language (cdr (lyskom-string-assoc language table))))))



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
                    (cons (cons (cdr (assq code codelist)) code)
                          result)))))
         tmp)))
     codes)
    result))


;;; ============================================================
;;; Beräkna

(def-kom-command kom-calculate (&optional exprx)
  "Calculate optional arg EXPRX, or prompt the user for an expression."
  (interactive)
  (when (lyskom-try-require 'calc 
                            (lyskom-get-string 'need-library))
    (let* ((expr (or exprx
                     (lyskom-with-lyskom-minibuffer
                      (read-from-minibuffer 
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
                                      completions nil t)
                     completions)))
         (objno nil)
         (label nil)
         (object nil)
         (secret nil)
         (aux nil))
    (cond ((eq type 'text)
           (setq objno
                 (lyskom-read-number (lyskom-get-string 'label-what-text)))
           (setq object (blocking-do 'get-text-stat objno))
           (when (null object)
             (lyskom-error 'no-such-text-no objno))
           (setq aux (text-stat-find-aux object 10 lyskom-pers-no))
           (setq secret (not (lyskom-j-or-n-p 
                              (lyskom-get-string 'label-secret))))
           (setq label
                 (lyskom-with-lyskom-minibuffer
                  (read-from-minibuffer 
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

          ((or (eq type 'conf)
               (eq type 'pers))
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
                  (read-from-minibuffer 
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
