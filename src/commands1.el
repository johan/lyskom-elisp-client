;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: commands1.el,v 44.202 2003-11-17 21:03:48 byers Exp $
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
;;;; File: commands1.el
;;;;
;;;; This file contains the code for some of the high level commands.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: commands1.el,v 44.202 2003-11-17 21:03:48 byers Exp $\n"))

(eval-when-compile
  (require 'lyskom-command "command"))


;;; ================================================================
;;;                  F} uppmuntran - Get appreciation 

;;; Author: Inge Wallin


(def-kom-command kom-get-appreciation ()
  "Provides a little light in the dark."
  (interactive)
  (lyskom-insert-string 'appreciation))
  

;;; ================================================================
;;;                      F} Sk{ll - Get abuse

;;; Author: Inge Wallin


(def-kom-command kom-get-abuse ()
  "Provides a bit of verbal abuse."
  (interactive)
  (lyskom-insert-string 'abuse))


;;; ================================================================
;;;            Utpl}na - Delete a person or a conference

;;; Author: Inge Wallin


(def-kom-command kom-delete-conf ()
  "Deletes a person or a conference. In order to delete a conference
you must be the supervisor of the conference. Once a conference is
deleted, it can never be restored, and texts with no recipients other
than the deleted conference will become inaccessible, and will
eventually be permanently deleted."
  (interactive)
  (let ((conf-stat 
	 (lyskom-read-conf-stat 'what-conf-to-delete '(all) nil nil t)))
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
		(progn
		  (lyskom-format-insert 'conf-is-deleted
					(conf-stat->name conf-stat))
		  (when (= (conf-stat->conf-no conf-stat)
			   lyskom-pers-no)
		    (lyskom-insert (lyskom-get-string
				    'you-have-deleted-yourself))
		    (setq lyskom-pers-no nil
			  lyskom-membership nil
			  lyskom-to-do-list (lyskom-create-read-list)
			  lyskom-reading-list (lyskom-create-read-list)
			  lyskom-pending-commands (cons
						   'kom-start-anew
						   lyskom-pending-commands))))
	      (lyskom-format-insert 'you-could-not-delete
				    conf-stat))
	  (lyskom-insert-string 'deletion-not-confirmed))
      (lyskom-insert-string 'somebody-else-deleted-that-conf))))


;;; ================================================================
;;;                Radera (text) - Delete a text

;;; Author: Inge Wallin


(def-kom-command kom-delete-text (text-no)
  "Permanently deletes a text. In order to delete a text you must
be the author of the text, or the supervisor of the author.

It is a very bad idea to delete texts that have comments. By deleting
a text with comments, the comments lost their context. This can lead
to fragmented discussion trees, where it is impossible to understand
what a comment means, or indeed what the whole discussion is about.

Instead of deleting a text, consider moving it to a conference created
specifically for storing texts that are no longer interesting. Several
LysKOM systems have such conferences, so ask around.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-text-to-delete)))
  (if text-no
      (let* ((text-stat (blocking-do 'get-text-stat text-no))
             (num-marks (and text-stat (text-stat->no-of-marks text-stat)))
             (num-comments (and text-stat (length (lyskom-text-comments text-stat))))
             (is-marked-by-me (cache-text-is-marked text-no)))

        (if (null text-stat)
            (lyskom-report-command-answer nil)
          (when (and (or (eq 0 num-marks)
                         (lyskom-j-or-n-p 
                          (lyskom-format 'delete-marked-text
                                         (if (> num-marks 0)
                                             (if is-marked-by-me
                                                 (if (= num-marks 1)
                                                     (lyskom-get-string 'delete-marked-by-you)
                                                   (lyskom-format 'delete-marked-by-you-and-others
                                                                  (1- num-marks)))
                                               (lyskom-format 'delete-marked-by-several
                                                              num-marks))))))
                     (or (eq 0 num-comments)
                         (progn (lyskom-format-insert "%#1@%#2t\n" 
                                                      `(face ,kom-warning-face)
                                                      (lyskom-get-string 'delete-commented-text-help))
                                (lyskom-beep t)
                                (and (lyskom-j-or-n-p
                                      (lyskom-get-string 'delete-commented-text))
                                     (lyskom-j-or-n-p
                                      (lyskom-get-string 'really-delete-commented-text))))))
            (lyskom-format-insert 'deleting-text text-no)
            (when (lyskom-report-command-answer 
                   (blocking-do 'delete-text text-no))
              (when is-marked-by-me
                (lyskom-unmark-text text-no))))))
    (lyskom-insert 'confusion-what-to-delete)))



;;; ================================================================
;;;        ]terse presentation - Review the presentation
;;;               for a person or a conference

;;; Author: Inge Wallin


(def-kom-command kom-review-presentation (&optional text-or-conf-no)
  "Reviews the presentation for a person or a conference. If you give
a prefix argument it is treated as a text number prefix argument (see
`lyskom-read-text-no-prefix-arg'), and this command shows the
presentation of the author of that text.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (and current-prefix-arg ; only peek at textno:s when prefixed!
		    (list (lyskom-read-text-no-prefix-arg
			   'text-to-see-author-of))))
  (let ((conf-stat
         (if text-or-conf-no
             (blocking-do 'get-conf-stat
                          (if (interactive-p)
                              (text-stat->author
                               (blocking-do 'get-text-stat text-or-conf-no))
                            text-or-conf-no))
           (lyskom-read-conf-stat 'presentation-for-whom '(all)
                                  nil nil t))))
    (lyskom-review-presentation conf-stat)))

(def-kom-command kom-unread-presentation (&optional text-or-conf-no)
  "Marks the presentation for a person or a conference as unread. If
you give a prefix argument it is treated as a text number prefix
argument (see `lyskom-read-text-no-prefix-arg'), and this command
unreads the presentation of the author of that text.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (and current-prefix-arg ; only peek at textno:s when prefixed!
		    (list (lyskom-read-text-no-prefix-arg
			   'text-to-unread-author-of))))
  (let ((conf-stat
         (if text-or-conf-no
             (blocking-do 'get-conf-stat
                          (if (interactive-p)
                              (text-stat->author
                               (blocking-do 'get-text-stat text-or-conf-no))
                            text-or-conf-no))
           (lyskom-read-conf-stat 'unread-presentation-for-whom
                                  '(all) nil nil t))))
    (if (zerop (conf-stat->presentation conf-stat))
        (lyskom-format-insert 'has-no-presentation conf-stat)
      (lyskom-format-insert 'marking-text-unread
                            (conf-stat->presentation conf-stat))
      (lyskom-report-command-answer
       (lyskom-mark-unread (conf-stat->presentation conf-stat))))))

(defun lyskom-review-presentation (conf-stat)
  "Review the presentation of conference CONF-STAT."
  (if (null conf-stat)
      (lyskom-insert-string 'somebody-deleted-that-conf)
    (lyskom-format-insert 'review-presentation-of
                          conf-stat)
    (if (/= (conf-stat->presentation conf-stat) 0)
        (lyskom-view-text (conf-stat->presentation conf-stat))
      (lyskom-format-insert 'has-no-presentation
                            conf-stat))
    (lyskom-traverse faq (lyskom-get-aux-item 
                          (conf-stat->aux-items conf-stat)
                          14)
      (lyskom-print-comment-like-aux faq conf-stat))))  

(defun lyskom-print-comment-like-aux (item object)
  (let* ((text-no (string-to-int (aux-item->data item)))
         (text-stat nil)
         (text nil))
    (unless kom-deferred-printing
      (blocking-do-multiple ((x-text (get-text text-no))
                             (x-text-stat (get-text-stat text-no)))
        (setq text-stat x-text-stat text x-text)))

    (cond ((or text-stat (not kom-deferred-printing))
           (lyskom-insert-comment-like-aux item text-no text-stat text object))
          (t (let ((defer-info (lyskom-create-defer-info
                                'get-text-stat
                                text-no
                                'lyskom-insert-deferred-comment-like-aux
                                (point-max-marker)
                                (length lyskom-defer-indicator)
                                nil     ; Filled in later
                                (list item object text-no))))
               (lyskom-format-insert "%#1s\n" lyskom-defer-indicator)
               (lyskom-defer-insertion defer-info))))))

(defun lyskom-insert-comment-like-aux (item text-no text-stat text object)
  (let* ((author (if text-stat (text-stat->author text-stat) nil))
         (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat)
                                            17)))
         (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat)
                                              16)))
         (formats (lyskom-aux-item-definition-field item 'text-header-line))
         (subject (lyskom-print-faq-format-subject text text-stat text-no))
         content-type
         )
    (if (and mx-from
             (setq content-type
                   (car (lyskom-get-aux-item (text-stat->aux-items 
                                              text-stat) 1))))
        (progn (string-match "^\\(\\S-+\\)" (aux-item->data content-type))
               (setq content-type (format "(%s) "
                                          (aux-item->data content-type))))
      (setq content-type ""))
    (setq author (or (lyskom-format-mx-author mx-from mx-author) author))

    (lyskom-format-insert (cond ((not (listp formats)) formats)
                                ((<= (length formats) 1) (car formats))
                                (author (elt formats 1))
                                (t (elt formats 0)))
                          text-no
                          author
                          content-type
                          (lyskom-aux-item-terminating-button item
                                                              object)
                          subject)
    (lyskom-insert "\n")))

;;; FIXME: This contains code that duplicates the code in 
;;; lyskom-insert-deferred-header-comm. That's a BAD THING.

(defun lyskom-insert-deferred-comment-like-aux (text-stat defer-info)
  (initiate-get-text 'deferred
                     'lyskom-insert-deferred-comment-like-aux-2
                     (elt (defer-info->data defer-info) 2)
                     text-stat
                     defer-info))

(defun lyskom-insert-deferred-comment-like-aux-2 (text text-stat defer-info)
  (let* ((author (if text-stat (text-stat->author text-stat) nil))
         (item (elt (defer-info->data defer-info) 0))
         (object (elt (defer-info->data defer-info) 1))
         (text-no (elt (defer-info->data defer-info) 2))
         (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat)
                                            17)))
         (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat)
                                              16)))
         (formats (lyskom-aux-item-definition-field item 'text-header-line))
         (subject (lyskom-print-faq-format-subject text text-stat text-no))
         content-type
         )
    (if (and mx-from
             (setq content-type
                   (car (lyskom-get-aux-item (text-stat->aux-items 
                                              text-stat) 1))))
        (progn (string-match "^\\(\\S-+\\)" (aux-item->data content-type))
               (setq content-type (format "(%s) "
                                          (aux-item->data content-type))))
      (setq content-type ""))
    (setq author (or (lyskom-format-mx-author mx-from mx-author) author))

    (set-defer-info->format defer-info 
                            (cond ((not (listp formats)) formats)
                                  ((<= (length formats) 1) (car formats))
                                  (author (elt formats 1))
                                  (t (elt formats 0))))
    (lyskom-replace-deferred defer-info
                             text-no
                             author
                             content-type
                             (lyskom-aux-item-terminating-button item
                                                                 object)
                             subject)))






;;; ================================================================
;;;          Återse det kommenterade - View commented text

;;; Author: Inge Wallin
;;; Modified by: David Kågedal, Johan Sundström

(def-kom-command kom-view-commented-text (text-no)
  "Views the text that the selected text is a comment to. If the
current text is comment or footnote to several texts then the first
text is shown and the remaining texts are reviewed.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg').

See `kom-review-uses-cache'."
  (interactive (list (lyskom-read-text-no-prefix-arg 'review-commented-q)))
  (if text-no
      (progn
	(lyskom-tell-internat 'kom-tell-read)
        (unless kom-review-uses-cache
          (cache-del-text-stat text-no))
	(lyskom-view-commented-text
	 (blocking-do 'get-text-stat text-no)))
    (lyskom-insert-string 'confusion-what-to-view)))

(def-kom-command kom-unread-commented-text (text-no)
  "MArkes the texts that the selected text is a comment to as unread.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'unread-commented-q)))
  (if text-no
      (lyskom-unread-commented-text 
       (blocking-do 'get-text-stat text-no))
    (lyskom-insert-string 'confusion-what-to-mark-unread)))


(def-kom-command kom-view-previous-commented-text (text-no)
  "Views the text that the selected text is a comment to. If the
current text is comment or footnote to several texts then the first
text is shown and the remaining texts are reviewed.

Without a prefix argument this will display the text that the
next-to-last text in the buffer is a comment to. With a prefix
argument this command is identical to `kom-view-commented-text'.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')

See `kom-review-uses-cache'."
  (interactive (list (lyskom-read-text-no-prefix-arg 'review-commented-q nil
                                                     lyskom-previous-text)))
  (cond (text-no
         (lyskom-tell-internat 'kom-tell-read)
        (unless kom-review-uses-cache
          (cache-del-text-stat text-no))
         (lyskom-view-commented-text
          (blocking-do 'get-text-stat text-no)))
        (t (lyskom-insert-string 'confusion-what-to-view))))


(def-kom-command kom-unread-previous-commented-text (text-no)
  "Marks the texts that the selected text is a comment to as unread.

Without a prefix argument this will display the text that the
next-to-last text in the buffer is a comment to. With a prefix
argument this command is identical to `kom-unread-commented-text'.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')"
  (interactive (list (lyskom-read-text-no-prefix-arg 'review-commented-q nil
                                                     lyskom-previous-text)))
  (if text-no
      (lyskom-unread-commented-text
       (blocking-do 'get-text-stat text-no))
    (lyskom-insert-string 'confusion-what-to-mark-unread)))


(defun lyskom-text-stat-commented-texts (text-stat)
  "Return a list of the text-nos that TEXT-STAT is a comment or footnote to."
  (let* ((misc-info-list (and text-stat
                              (text-stat->misc-info-list text-stat)))
         (misc-infos (and misc-info-list
                          (append (lyskom-misc-infos-from-list
                                   'COMM-TO misc-info-list)
                                  (lyskom-misc-infos-from-list
                                   'FOOTN-TO misc-info-list)))))
    (and misc-infos
	 (mapcar
	  (function
	   (lambda (misc-info)
	     (if (equal (misc-info->type misc-info)
			'COMM-TO)
		 (misc-info->comm-to misc-info)
	       (misc-info->footn-to misc-info))))
	  misc-infos))))


(defun lyskom-view-commented-text (text-stat)
  "Handles the return from the initiate-get-text-stat, displays and builds list."
  (let ((text-nos (lyskom-text-stat-commented-texts text-stat)))
    (if text-nos
        (progn
          (lyskom-format-insert 'review-text-no 
                                (car text-nos))
          (if (cdr text-nos)
              (read-list-enter-read-info
               (lyskom-create-read-info
                'REVIEW nil (lyskom-review-get-priority)
                (lyskom-create-text-list (cdr text-nos))
                lyskom-current-text)
               lyskom-reading-list t))
          (unless kom-review-uses-cache
            (cache-del-text-stat (car text-nos)))
          (lyskom-view-text (car text-nos)
			    nil nil nil 
			    nil nil nil
			    t))
      (lyskom-insert-string 'no-comment-to))))

(defun lyskom-unread-commented-text (text-stat)
  "Marks the texts that TEXT-STAT is a comment to as unread."
  (let ((text-nos (lyskom-text-stat-commented-texts text-stat)))
    (if text-nos
        (lyskom-traverse text-no text-nos
          (lyskom-format-insert 'marking-text-unread text-no)
          (lyskom-report-command-answer (lyskom-mark-unread text-no)))
      (lyskom-insert-string 'no-comment-to-to-unread))))


(defun lyskom-misc-infos-from-list (type list)
  "Get all the misc-infos from the misc-info-list LIST with the same type
as TYPE. If no such misc-info, return NIL"
  (let ((result nil))
    (while list
      (when (equal type (misc-info->type (car list)))
        (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (nreverse result)))


;;; ================================================================
;;;                         Brev - Send letter
;;; Author: Inge Wallin

(def-kom-command kom-send-letter (&optional pers-no)
  "Send a personal letter to a person. This command is similar to
`kom-write-text', but intended specifically for sending personal
messages.

Several settings affect writing texts in general.
`kom-write-texts-in-window' controls which window is used to write the
comment; `kom-confirm-multiple-recipients' affects how multiple
recipients are handled."
  (interactive)
  (lyskom-tell-internat 'kom-tell-write-letter)
  (lyskom-write-text pers-no 'who-letter-to))


;;; ================================================================
;;;           Bli medlem i m|te - Become a member of a conference
;;;             Addera medlem - Add somebody else as a member

;;; Author: ???
;;; Rewritten by: David K}gedal


;; Add another person

(def-kom-command kom-add-member ()
  "Invite a person as a member of a conference. Depending on the
server configuration this command may either create an invitation or
simply make the person a member of the conference without asking them
for confirmation."
  (interactive)
  (let* ((who (lyskom-read-conf-stat 'who-to-add '(pers) nil nil t))
	 (whereto (lyskom-read-conf-stat 'where-to-add '(all) nil nil t))
	 (pers-stat (blocking-do 'get-pers-stat (conf-stat->conf-no who))))
    (lyskom-add-member-answer (lyskom-try-add-member whereto who 
                                                     pers-stat 
                                                     nil
                                                     nil
                                                     t)
			      whereto who)))



;; Add self

(def-kom-command kom-add-self (&optional conf)
  "Become a member of a conference. The default priority of the
membership is determined by `kom-membership-default-priority' and its
placement by `kom-membership-default-placement' You can also use this
command to change the priority of an existing membership and to
re-join a conference of which you are a passive member.

See `kom-membership-default-priority' and 
`kom-membership-default-placement'"
  (interactive)
  (let* ((whereto (if conf (blocking-do 'get-conf-stat conf)
                    (lyskom-read-conf-stat 'where-to-add-self
                                           '(all) nil nil t)))
         (mship (lyskom-is-member (conf-stat->conf-no whereto)
                                  lyskom-pers-no)))

    (cond ((and mship (membership-type->passive
                       (membership->type mship)))
              (set-membership-type->passive (membership->type mship) nil)
              (set-membership-type->message-flag (membership->type mship)
                                                 (lyskom-j-or-n-p
                                                  (lyskom-format 'set-message-flag-q whereto)))
              (blocking-do 'set-membership-type
                           lyskom-pers-no
                           (conf-stat->conf-no whereto)
                           (membership->type mship))
              (lyskom-format-insert 'activate-mship-done whereto)
              (lp--update-buffer (membership->conf-no mship))
              (lyskom-fetch-start-of-map whereto mship))

          ((and mship (not (membership-type->invitation
                            (membership->type mship))))
           (lyskom-format-insert 'you-already-member whereto))

          (t 
           (let* ((who (blocking-do 'get-conf-stat lyskom-pers-no))
                  (pers-stat (blocking-do 'get-pers-stat lyskom-pers-no))
                  (mship (lyskom-get-membership (conf-stat->conf-no whereto) t))
                  (no-of-unread
                   (unless (and mship (not (membership-type->passive
                                            (membership->type mship))))
                     (lyskom-read-num-range-or-date 
                      0 
                      (conf-stat->no-of-texts whereto)
                      (lyskom-format 'initial-unread)
                      nil
                      t
                      nil)))
                  (kom-membership-default-priority
                   (if (and mship (membership-type->passive (membership->type mship)))
                       (membership->priority mship)
                     kom-membership-default-priority)))

             (lyskom-add-member-answer (lyskom-try-add-member whereto
                                                              who
                                                              pers-stat
                                                              nil
                                                              nil
                                                              t
                                                              nil
                                                              no-of-unread)
                                       whereto who
                                       no-of-unread
                                       ))))))


(def-kom-command kom-change-priority (&optional conf)
  "Changes the priority of a single membership. If you want to change
the priority of several memberships, use `kom-prioritize' instead."
  (interactive)
  (let* ((conf-stat (if conf (blocking-do 'get-conf-stat conf)
                      (lyskom-read-conf-stat  'change-priority-for-q
                                              '(all) nil nil t)))
         (mship (lyskom-get-membership (conf-stat->conf-no conf-stat) t))
	 (kom-membership-default-priority nil))
    (blocking-do-multiple ((who (get-conf-stat lyskom-pers-no))
                           (pers-stat (get-pers-stat lyskom-pers-no)))
      (cond ((and (null mship) conf-stat)
             (lyskom-format-insert 'not-member-of-conf conf-stat))
	    ((null conf-stat)
	     (lyskom-format-insert 'no-such-conf))
            (t (lyskom-add-member-answer
                (lyskom-try-add-member conf-stat who pers-stat nil
                                       'change-priority-for t)
                conf-stat who))))))


;;; NOTE: This function is also called from lyskom-go-to-conf-handler
;;;       and from lyskom-create-conf-handler.

(defun lyskom-add-member-by-no (conf-no pers-no &optional no-of-unread thendo &rest data)
  "Fetch info to be able to add a person to a conf.
Get the conf-stat CONF-NO for the conference and the conf-stat and pers-stat 
for person PERS-NO and send them into lyskom-try-add-member."
  (blocking-do-multiple ((whereto (get-conf-stat conf-no))
                         (who (get-conf-stat pers-no))
                         (pers-stat (get-pers-stat pers-no)))
    (let ((result (lyskom-try-add-member whereto who pers-stat 
                                         nil nil t nil no-of-unread)))
      (lyskom-add-member-answer result whereto who no-of-unread)
      (if thendo
          (apply thendo data))
      (car result))))


(defun lyskom-try-add-member (conf-conf-stat 
                              pers-conf-stat 
                              pers-stat
                              membership-type
                              &optional message-string
                              need-extra-information
                              default-priority
                              no-of-unread)
  "Add a member to a conference.
Args: CONF-CONF-STAT PERS-CONF-STAT PERS-STAT
CONF-CONF-STAT: the conf-stat of the conference the person is being added to
PERS-CONF-STAT: the conf-stat of the person being added.
PERS-STAT: the pers-stat of the person being added.

Optional MESSAGE-STRING is the message to print before making server call.
Returns t if it was possible, otherwise nil.

If optional NEED-EXTRA-INFORMATION is non-nil, the return value will be
a list where the first element is the result of add-member and the second
is the position where the membership was placed.

If optional USE-PRIORITY is non-nil then use that as the priority.
"
  (cond ((null no-of-unread))

        ((numberp no-of-unread)
         (setq no-of-unread (lyskom-format 'member-in-conf-with-unread
                                           no-of-unread)))
        ((listp no-of-unread)
                 (setq no-of-unread
                       (lyskom-format 'member-in-conf-with-unread-date
                                      (elt no-of-unread 0)
                                      (car (rassq (elt no-of-unread 1)
                                                  lyskom-month-names))
                                      (elt no-of-unread 2)))))

  (if (or (null conf-conf-stat)
	  (null pers-conf-stat))
      nil				; We have some problem here.
    (let ((priority
           (if (/= lyskom-pers-no (conf-stat->conf-no pers-conf-stat))
               (lyskom-read-num-range 0 255
                                      (lyskom-get-string 'other-priority-q)
                                      nil 100)
             (if (and (numberp kom-membership-default-priority)
                      (< kom-membership-default-priority 256)
                      (>= kom-membership-default-priority 0))
                 kom-membership-default-priority
               (lyskom-read-num-range 0 255 (lyskom-get-string 'priority-q) nil default-priority))))
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

      ;;
      ;; Adding ourselves. Adjust where so the membership
      ;; list remains sorted. Find the closest position to
      ;; where at which we can put the membership and keep
      ;; the membership lsit sorted.
      ;;

      (when (eq lyskom-pers-no (conf-stat->conf-no pers-conf-stat))
        (let ((mship-list lyskom-membership)
              (mship nil)
              (index 0)
              (found nil))
          (while mship-list
            (setq mship (car mship-list)
                  mship-list (cdr mship-list))
            (cond ((> (membership->priority mship) priority))
                  ((< (membership->priority mship) priority) 
                   (setq where index mship-list nil found t))
                  ((and (= (membership->priority mship) priority)
                        (= index where))
                   (setq mship-list nil found t))
                  ((and (= (membership->priority mship) priority)
                        (> index where))
                   (setq where index mship-list nil found t)))
            (setq index (1+ index))
            (unless found (setq where (1+ index))))))

      (when (null membership-type)
        (setq membership-type 
              (lyskom-create-membership-type nil nil nil nil
                                             nil nil nil nil)))

      (if message-string
          (lyskom-format-insert message-string
                                pers-conf-stat
                                conf-conf-stat)
        (if (= (conf-stat->conf-no pers-conf-stat)
               lyskom-pers-no)
            (lyskom-format-insert 'member-in-conf
                                  conf-conf-stat
                                  no-of-unread)
          (lyskom-format-insert 'add-member-in
                                pers-conf-stat
                                conf-conf-stat)))
      (lyskom-ignoring-async (18 lyskom-pers-no
                                 (conf-stat->conf-no conf-conf-stat))
        (let ((res (blocking-do 'add-member 
                                (conf-stat->conf-no conf-conf-stat)
                                (conf-stat->conf-no pers-conf-stat)
                                priority where
                                membership-type)))
          (if need-extra-information
              (list res where)
            res))))))


(defun lyskom-add-member-answer (answer 
                                 conf-conf-stat
                                 pers-conf-stat
                                 &optional no-of-unread)
  "Handle the result from an attempt to add a member to a conference."
  (let ((pos (if (consp answer) (elt answer 1) nil))
        (answer (if (consp answer) (elt answer 0) answer)))
    (if (null answer)
        (progn
          (lyskom-insert-string 'nope)
          (let* ((errno lyskom-errno)
                 (err-stat lyskom-err-stat)
                 (is-supervisor (lyskom-is-supervisor (conf-stat->conf-no conf-conf-stat)
                                                      lyskom-pers-no))
                 (is-member (lyskom-is-member (conf-stat->conf-no conf-conf-stat)
                                              (conf-stat->conf-no pers-conf-stat)))
                 (rd-prot (conf-type->rd_prot (conf-stat->conf-type conf-conf-stat))))


            (cond (is-member
                   (lyskom-format-insert 'add-already-member 
                                         pers-conf-stat
                                         conf-conf-stat))
                  ((and rd-prot is-supervisor)
                   (lyskom-insert-error errno err-stat))

                  (rd-prot (let ((supervisorconf (blocking-do
                                                  'get-conf-stat
                                                  (conf-stat->supervisor conf-conf-stat))))
                             (if supervisorconf
                                 (lyskom-format-insert 'is-read-protected-contact-supervisor
                                                       conf-conf-stat
                                                       supervisorconf)
                               (lyskom-format-insert 'cant-find-supervisor
                                                     conf-conf-stat))))

                  (t (lyskom-insert-error errno err-stat)))))

      (when (and (eq (conf-stat->conf-no pers-conf-stat)
                     lyskom-pers-no)
                 (lyskom-j-or-n-p
                  (lyskom-format 'set-message-flag-q conf-conf-stat)))
        (blocking-do 'set-membership-type
                     (conf-stat->conf-no pers-conf-stat)
                     (conf-stat->conf-no conf-conf-stat)
                     (lyskom-create-membership-type nil nil nil t
                                                    nil nil nil nil)))

      (cache-del-pers-stat (conf-stat->conf-no pers-conf-stat))
      (cache-del-conf-stat (conf-stat->conf-no conf-conf-stat))
      (when (= (conf-stat->conf-no pers-conf-stat)
             lyskom-pers-no)
        (when no-of-unread
          (cond ((listp no-of-unread)
                 (let* ((target-date (lyskom-create-time 0 0 0 (elt no-of-unread 2) (elt no-of-unread 1) (elt no-of-unread 0) 0 0 nil))
                        (text (lyskom-find-text-by-date conf-conf-stat target-date)))
                   (when text
                     (blocking-do 'set-last-read 
                                  (conf-stat->conf-no conf-conf-stat)
                                  (car text)))))
                ((numberp no-of-unread) 
                 (blocking-do 'set-unread (conf-stat->conf-no conf-conf-stat)
                              no-of-unread))))
        (let ((mship (blocking-do 'query-read-texts
                                  lyskom-pers-no 
                                  (conf-stat->conf-no conf-conf-stat)
                                  t 0)))
          (if (< (membership->priority mship) lyskom-session-priority)
              (lyskom-format-insert-before-prompt
               'member-in-conf-with-low-priority
               conf-conf-stat)
            (unless (membership->position mship)
              (set-membership->position mship pos))
            (if (lyskom-try-get-membership (conf-stat->conf-no conf-conf-stat) t)
                (progn (lyskom-replace-membership mship)
                       (lyskom-fetch-start-of-map conf-conf-stat mship))
              (lyskom-add-membership mship
                                     conf-conf-stat
                                     t)))))
      (lyskom-insert-string 'done))))



(defun lyskom-add-membership (membership conf-no-or-stat &optional blocking)
  "Adds MEMBERSHIP to the sorted list of memberships.
If BLOCKING is non-nil, block while reading the conference map until at 
least one text has been seen. CONF-NO-OR-STAT is the conf-no to add 
unless BLOCKING is t, in which cast it is the conf-stat."
  (if membership
      (progn
	(lyskom-insert-membership membership)
        (if blocking
            (lyskom-fetch-start-of-map conf-no-or-stat membership)
          (lyskom-prefetch-map conf-no-or-stat membership))
        (lyskom-run-hook-with-args 'lyskom-add-membership-hook
                                   membership))
    (lyskom-insert-string 'conf-does-not-exist)))



;;; ================================================================
;;;     Uttr{d - Subtract yourself as a member from a conference
;;;     Uteslut medlem - Subtract somebody else as a member

;;; Author: David Byers
;;; Based on code by Inge Wallin


;; Subtract another person

(def-kom-command kom-sub-member ()
  "Remove a member from a conference. To do this you need to be the
supervisor of the conference or of the member being removed."
  (interactive)
  (lyskom-sub-member
   (lyskom-read-conf-stat 'who-to-exclude '(pers) nil nil t)
   (lyskom-read-conf-stat 'where-from-exclude '(all) nil nil t)))


(def-kom-command kom-sub-self (&optional conf-no)
  "Resign your membership of a conference. If
`kom-unsubscribe-makes-passive' is set, them resigning once converts
your membership to a passive membership and resigning twice removes
the membership completely (if the server supports passive
memberships).

See `kom-unsubscribe-makes-passive'."
  (interactive)
  (let* ((me (blocking-do 'get-conf-stat lyskom-pers-no))
	 (conf (if conf-no (blocking-do 'get-conf-stat conf-no)
                 (lyskom-read-conf-stat 'leave-what-conf
                                        '(membership) nil nil t))))
    (lyskom-sub-member me conf)))

(defun lyskom-sub-member (pers conf &optional have-message)
  "Remove the person indicated by PERS as a member of CONF."
  (let* ((reply nil)
         (self (= (conf-stat->conf-no pers) lyskom-pers-no))
         (mship (and self 
                     kom-unsubscribe-makes-passive
                     (lyskom-get-membership (conf-stat->conf-no conf))))
         (passivate (and mship
                         (lyskom-have-call 102) ; set-membership-type
                         (not (membership-type->passive
                               (membership->type mship))))))

    (cond ((null pers) (lyskom-insert-string 'error-fetching-person))
	  ((null conf) (lyskom-insert-string 'error-fetching-conf))
          (passivate 
           (lyskom-prefetch-cancel-prefetch-map (conf-stat->conf-no conf))
           (unless have-message
             (lyskom-format-insert 'unsubscribe-to conf))
           (set-membership-type->passive (membership->type mship) t)
           (set-membership-type->message-flag (membership->type mship) nil)
           (setq reply (blocking-do 'set-membership-type
                                    (conf-stat->conf-no pers)
                                    (conf-stat->conf-no conf)
                                    (membership->type mship)))
           (if (not reply)
               (lyskom-format-insert 'unsubscribe-failed
                                     (lyskom-get-string 'You)
                                     conf)
             (lyskom-insert-string 'done)
             (lyskom-format-insert 'passivate-done conf)
             (when (= (conf-stat->conf-no conf) lyskom-current-conf)
               (lyskom-leave-current-conf))
	     (read-list-delete-read-info (conf-stat->conf-no conf)
					 lyskom-to-do-list)
             (lp--update-buffer (conf-stat->conf-no conf))))
	  (t
           (when self
             (lyskom-prefetch-cancel-prefetch-map (conf-stat->conf-no conf)))

	   (if self
	       (lyskom-format-insert 'unsubscribe-to conf)
	     (lyskom-format-insert 'exclude-from pers conf))

           (lyskom-ignoring-async (8 (conf-stat->conf-no conf))
             (setq reply (blocking-do 'sub-member
                                      (conf-stat->conf-no conf)
                                      (conf-stat->conf-no pers))))
	   (if self
	       (lyskom-remove-membership (conf-stat->conf-no conf)))
	   (if (not reply)
	       (lyskom-format-insert 'unsubscribe-failed
				     (if self
					 (lyskom-get-string 'You)
				       (conf-stat->name pers))
				     conf)
	     (lyskom-insert-string 'done)
	     (when (and self (= (conf-stat->conf-no conf)
                              lyskom-current-conf))
               (lyskom-leave-current-conf))
	     (read-list-delete-read-info (conf-stat->conf-no conf)
					 lyskom-to-do-list))))))
	   

	 

;;; ================================================================
;;;                 Skapa m|te - Create a conference

;;; Author: ???


(def-kom-command kom-create-conf (&optional name)
  "Create a new conference. Use this command when you want to create a
new conference. It will ask for all required information.

Before creating a conference, check that there isn't already one
covering the intended topics. There may be a conference for discussing
converence creation where you can ask. If there isn't, there is
probably a general Q&A conference where you can ask."
  (interactive)
  (let* ((conf-name (or name
			(lyskom-read-string
			 (lyskom-get-string 'name-of-conf))))
	 (open (j-or-n-p (lyskom-get-string 'anyone-member)))
	 (secret (if (not open)
		     (j-or-n-p (lyskom-get-string 'secret-conf))))
	 (orig (j-or-n-p (lyskom-get-string 'comments-allowed)))
         (anarchy (j-or-n-p (lyskom-get-string 'anonymous-allowed)))
         (secmem (and (lyskom-have-feature long-conf-types)
                      (not (lyskom-j-or-n-p (lyskom-get-string 'secret-members-allowed)))))
	 (life-expectancy (lyskom-read-number 'new-garb-nice-q))
	 (keep-commented (lyskom-read-number 'new-keep-commented-q))
	 (conf-no (blocking-do 'create-conf
			       conf-name
			       (lyskom-create-conf-type (not open)
							(not orig)
							secret
							nil
                                                        anarchy
                                                        secmem
                                                        nil
                                                        nil)
                               nil)))
    (if (null conf-no)
	(progn
	  (lyskom-format-insert 'could-not-create-conf
				conf-name)
	  (lyskom-insert-error))
      (progn
	(let ((conf-stat (blocking-do 'get-conf-stat conf-no)))
	  (lyskom-format-insert 'created-conf-no-name 
				(or conf-stat conf-no)
				(or conf-stat conf-name)
				(if conf-stat
				    (lyskom-default-button 'conf conf-stat)
				  nil)))
	(lyskom-scroll)
	(lyskom-add-member-by-no conf-no 
                                 lyskom-pers-no
                                 nil
				 (if secret
				     nil ; Don't write a presentation
				   'lyskom-create-conf-handler-2)
				 conf-no conf-name)
	(blocking-do 'set-garb-nice conf-no life-expectancy)
	(blocking-do 'set-keep-commented conf-no keep-commented)))))


(defun lyskom-create-conf-handler-2 (conf-no conf-name)
  "Starts editing a presentation for the newly created conference.
This does lyskom-end-of-command"
  (lyskom-tell-internat 'kom-tell-conf-pres)
  (let ((conf (blocking-do 'get-conf-stat conf-no)))
    (if conf
        (lyskom-dispatch-edit-text lyskom-proc
                                   (lyskom-create-misc-list
                                    'RECPT
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
;;; FIXME: Does not use def-kom-command


(defun kom-write-comment (text-no)
  "Write a comment to the selected text.

Several settings affect writing texts in general.
`kom-write-texts-in-window' controls which window is used to write the
comment; `kom-confirm-multiple-recipients' affects how multiple
recipients are handled.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list 
                (let ((lyskom-current-command 'kom-write-comment))
                  (lyskom-read-text-no-prefix-arg 'what-comment-no))))
  (lyskom-start-of-command (concat 
			    (lyskom-command-name 'kom-write-comment)
			    (if text-no 
				(lyskom-format " (%#1n)" text-no)
			      "")))
  (unwind-protect
      (if text-no
          (progn
            (lyskom-nag-about-presentation)
            (blocking-do-multiple ((text (get-text text-no))
                                   (text-stat (get-text-stat text-no)))
              (when (or (null (text-stat-find-aux text-stat 4))
                        (lyskom-j-or-n-p 
                         (lyskom-get-string 'no-comments-q)))
                (if (and (text-stat-find-aux text-stat 5)
                         (lyskom-j-or-n-p
                          (lyskom-get-string 'private-answer-q)))
                    (lyskom-private-answer-soon
                     text-stat
                     text
                     text-no)
                  (lyskom-write-comment-soon text-stat
                                             text
                                             text-no
                                             'comment)))))
        (lyskom-insert-string 'confusion-who-to-reply-to))
    (lyskom-end-of-command)))


(def-kom-command kom-write-footnote (text-no)
  "Write a footnote to the selected text. In order to write a footnote
you must be the author, or superviser of the author, of the selected
text.

Several settings affect writing texts in general.
`kom-write-texts-in-window' controls which window is used to write the
comment; `kom-confirm-multiple-recipients' affects how multiple
recipients are handled.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg'). Without a prefix argument this
command will write a footnote to the most recently read or written
applicable text."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-footnote-no nil
                                                     'last-seen-written)))
  (if text-no
      (lyskom-write-comment-soon
       (blocking-do 'get-text-stat text-no)
       (blocking-do 'get-text text-no)
       text-no 'footnote)
    (lyskom-insert-string 'confusion-what-to-footnote)))


(def-kom-command kom-comment-previous (text-no)
  "Write a comment to the selected text. Without a prefix argument
the selected text is the next-to-last text in the buffer.

Several settings affect writing texts in general.
`kom-write-texts-in-window' controls which window is used to write the
comment; `kom-confirm-multiple-recipients' affects how multiple
recipients are handled.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-comment-no nil
                                                     lyskom-previous-text)))
  (if text-no
      (blocking-do-multiple ((text-stat (get-text-stat text-no))
                             (text (get-text text-no)))
        (when (or (null text-stat)
                  (null text)
                  (null (text-stat-find-aux text-stat 4))
                  (lyskom-j-or-n-p 
                   (lyskom-get-string 'no-comments-q)))
          (if (and (text-stat-find-aux text-stat 5)
                   (lyskom-j-or-n-p
                    (lyskom-get-string 'private-answer-q)))
              (lyskom-private-answer-soon
               text-stat
               text
               text-no)
            (lyskom-write-comment-soon
             text-stat
             text
             text-no
             'comment))))
    (lyskom-insert-string 'confusion-what-to-comment)))


(defun lyskom-write-comment-soon (text-stat text text-no type)
  "Write a comment to the text with TEXT-STAT, TEXT and, TEXT-NO.
TYPE is either 'comment or 'footnote."
  (let ((str (and text-stat text (text->decoded-text-mass text text-stat))))
    (cond
     ;; Text not found?
     ((or (null text-stat)
          (null text))
      (lyskom-format-insert 'cant-read-textno text-no))
     ;; Give header.
     ((string-match "\n" str)
      (lyskom-write-comment text-stat
                            (substring str 0 (match-beginning 0))
                            type))
     ;; The commented text had no header.
     (t (lyskom-write-comment text-stat "" type)))))


(defun lyskom-write-comment (text-stat subject type)
  "Write a comment to the text associated with TEXT-STAT.
The default subject is SUBJECT. TYPE is either 'comment or 'footnote."
  (if (null text-stat)
      (progn
        (lyskom-insert-string 'confusion-what-to-comment))
    (let ((ccrep nil)
          (bccrep nil))
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
                               data)))
             ((and (eq type 'footnote)
                   (eq 'BCC-RECPT (misc-info->type misc-info)))
              (setq bccrep (cons (misc-info->recipient-no misc-info) 
                                 bccrep))
              (setq data (cons (blocking-do 'get-conf-stat
                                            (misc-info->recipient-no misc-info))
                               data))))))
         (text-stat->misc-info-list text-stat))
        (lyskom-comment-recipients (nreverse data)
                                   lyskom-proc 
                                   text-stat
                                   subject 
                                   type 
                                   (nreverse ccrep) 
                                   (nreverse bccrep))))))


(defun lyskom-comment-recipients (data lyskom-proc text-stat
				       subject type ccrep bccrep)
  "Compute recipients to a comment to a text.
Args: DATA, LYSKOM-PROC TEXT-STAT SUBJECT TYPE CCREP BCCREP.
DATA is a list of all the recipients that should receive this text.
If DATA contains more than one conference the user is asked (using y-or-n-p)
if all conferences really should receive the text.
The call is continued to the lyskom-edit-text.
TYPE is info whether this is going to be a comment of footnote.
CCREP is a list of all recipients that are going to be cc-recipients.
BCCREP is a list of all recipient that are going to be bcc-recipients."

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
			 ((eq type 'comment) 'COMM-TO)
			 ((eq type 'footnote) 'FOOTN-TO)
			 (t (signal 'lyskom-internal-error
				    (list "Unknown comment type" type))))
			(text-stat->text-no text-stat)))
	       (recpts nil))
	  (while data
	    (let* ((conf-stat (car data))
                   (confno (conf-stat->conf-no conf-stat))
                   (commno (if (eq type 'footnote)
                               confno
                             (conf-stat->comm-conf conf-stat))))
	      (if (memq commno recpts)
		  nil
		(setq recver
		      (append recver
			      (list
			       (cons (cond
                                      ((memq confno ccrep) 'CC-RECPT)
                                      ((memq confno bccrep) 'BCC-RECPT)
                                      (t 'RECPT))
                                     commno))))
		(if (lyskom-get-membership commno)
		    (setq member t))
		(setq recpts (cons commno recpts))))
	    (setq data (cdr data)))
	  ;; Add the user to the list of recipients if he isn't a member in
	  ;; any of the recipients.
	  (when (not member)
	      (setq recver (append recver
				   (list (cons 'RECPT
                                               (or (lyskom-get-send-comments-to lyskom-pers-no)
                                                   lyskom-pers-no))))))
	  (lyskom-edit-text lyskom-proc 
			    recver
			    subject "")))

    (quit (signal 'quit nil))))


;;; ================================================================
;;;                Personligt svar - personal answer
;;;
;;; Author: ???
;;; Rewritten using blocking-do by: Linus Tolke


(def-kom-command kom-private-answer (text-no)
  "Write a private answer to the selected text.

Several settings affect writing texts in general.
`kom-write-texts-in-window' controls which window is used to write the
comment; `kom-confirm-multiple-recipients' affects how multiple
recipients are handled.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-private-no)))
  (if text-no
      (blocking-do-multiple ((text-stat (get-text-stat text-no))
                             (text (get-text text-no)))
        (when (or (null (text-stat-find-aux text-stat 4))
                  (lyskom-j-or-n-p 
                   (lyskom-get-string 'no-comments-q)))
          (lyskom-private-answer-soon text-stat text text-no)))
    (lyskom-insert-string 'confusion-who-to-reply-to)))


(defun lyskom-private-answer-soon (text-stat text text-no)
  "Write a private answer to TEXT-STAT, TEXT."
  (if (and text-stat text)
      (let ((str (text->decoded-text-mass text text-stat)))
        (if (string-match "\n" str)
            (lyskom-private-answer text-stat
                                   (substring str
                                              0 (match-beginning 0)))
          (lyskom-private-answer text-stat "")))
    (lyskom-format-insert 'no-such-text-no text-no)))


(defun lyskom-private-answer (text-stat subject)
  "Write a private answer. Args: TEXT-STAT SUBJECT."
  (cond ((null text-stat)
         (lyskom-insert-string 'confusion-what-to-answer-to))
        ((= (text-stat->author text-stat) 0)
         (lyskom-insert-string 'personal-comment-to-anonymous))
        (t
         (lyskom-tell-internat 'kom-tell-write-reply)
         (cache-del-conf-stat (text-stat->author text-stat))
         (let* ((conf-stat (blocking-do 'get-conf-stat
                                        (text-stat->author text-stat))))
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

               (lyskom-edit-text lyskom-proc
                                 (if (= (text-stat->author text-stat) lyskom-pers-no)
                                     (lyskom-create-misc-list
                                      'COMM-TO (text-stat->text-no text-stat)
                                      'RECPT (text-stat->author text-stat))
                                   (lyskom-create-misc-list
                                    'COMM-TO (text-stat->text-no text-stat)
                                    'RECPT (text-stat->author text-stat)
                                    'RECPT lyskom-pers-no))
                                 subject ""))))))


;;; ================================================================
;;;    Personligt svar p} f|reg}ende - kom-private-answer-previous
;;;
;;; Author: ceder
;;; Rewritten using blocking-do by: Linus Tolke

(def-kom-command kom-private-answer-previous (text-no)
  "Write a private answer to the selected text. Without a prefix
argument the next-to-last text in the buffer will be selected.


Several settings affect writing texts in general.
`kom-write-texts-in-window' controls which window is used to write the
comment; `kom-confirm-multiple-recipients' affects how multiple
recipients are handled.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-private-no nil
                                                     lyskom-previous-text)))
  (if text-no
      (blocking-do-multiple ((text-stat (get-text-stat text-no))
                             (text (get-text text-no)))
        (when (or (null (text-stat-find-aux text-stat 4))
                  (lyskom-j-or-n-p 
                   (lyskom-get-string 'no-comments-q)))
          (lyskom-private-answer-soon text-stat text text-no)))
    (lyskom-insert-string 'confusion-who-to-reply-to)))


;;; ================================================================
;;;                         Sluta - quit

;;; Author: ???


(defun kom-quit (&optional arg)
  "Exit LysKOM. With prefix argument, don't ask for confirmation."
  (interactive "P")
  (lyskom-start-of-command 'kom-quit t)
  (let ((do-end-of-command t))
    (unwind-protect
        (setq do-end-of-command
              (cond
               ((and (lyskom-buffers-of-category 'write-texts)
                     (display-buffer 
                      (car (lyskom-buffers-of-category 'write-texts)))
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
    (lyskom-remove-unread-buffer lyskom-buffer)
    (set-process-sentinel lyskom-proc nil)
    (delete-process lyskom-proc)
    (lyskom-insert-string (lyskom-get-string-sol 'session-ended))
    (lyskom-scroll)
    (setq mode-line-process (lyskom-get-string 'mode-line-down))
    (run-hooks 'kom-quit-hook)

    (when (and (boundp 'lyskom-ssh-proxy)
               lyskom-ssh-proxy)
      (let* ((numleft (1- (get lyskom-ssh-proxy 'num-connected)))
             (procname (symbol-name lyskom-ssh-proxy))
             (bufname (concat " *" procname "*"))
             (proc (get-process procname)))
        (put lyskom-ssh-proxy 'num-connected numleft)
        (when (and (<= numleft 0) proc)
          (string-match "^ssh<\\([^:]*\\)" procname)
          (lyskom-message 
           "%s"
           (lyskom-format 'ssh-closing
                          (or (match-string 1 procname) 
                              (lyskom-get-string 'ssh-unknown-host))))
          (delete-process proc)
          (when (get-buffer bufname)
            (save-excursion
              (set-buffer bufname)
              (goto-char (point-max))
              (insert "\n--- closed connection ---\n")))))))
  

;;; ================================================================
;;;     	[ndra presentation - Change presentation 
;;;	         S{tta lapp p} d|rren - Change conf-motd

;;; Author: Inge Wallin
;;; Changed by Linus Tolke


(def-kom-command kom-change-presentation ()
  "Change presentation for a person or a conference. If the person
or conference doesn't have a presentation, a new presentation will 
be created."
  (interactive)
  (lyskom-change-pres-or-motd-2
   (let ((no (lyskom-read-conf-no 'what-to-change-pres-you '(all) nil nil t)))
     (blocking-do 'get-conf-stat no))
   'pres))



(def-kom-command kom-change-conf-motd ()
  "Change the notice for a person or a conference. If the person
or conference doesn't have a notice, a new notice till be created."
  (interactive)
  (lyskom-change-pres-or-motd-2
   (let ((no (lyskom-read-conf-no 'who-to-put-motd-for '(all) nil nil t)))
     (blocking-do 'get-conf-stat no))
   'motd))


(defun lyskom-get-recipients-from-misc-list (misc-list)
  "Return a misc-info-list containing only the recipients."
  (let* ((info (car misc-list))
	 (type (misc-info->type info)))
    (cond ((null misc-list) '())
	  ((memq type lyskom-recpt-types-list)
	   (append (list (intern (symbol-name type))
			 (misc-info->recipient-no info))
		   (lyskom-get-recipients-from-misc-list
		    (cdr misc-list))))
	  (t
	   (lyskom-get-recipients-from-misc-list
	    (cdr misc-list))))))


(defun lyskom-change-pres-or-motd-2 (conf-stat type)
  "Change the presentation or motd of CONF-STAT.
TYPE is either 'pres or 'motd, depending on what should be changed."
  (cond
   ((null conf-stat)			;+++ annan felhantering
    (lyskom-insert-string 'cant-get-conf-stat))
   ((or lyskom-is-administrator
	(lyskom-get-membership (conf-stat->supervisor conf-stat) t)
	(= lyskom-pers-no (conf-stat->conf-no conf-stat)))
    (blocking-do-multiple ((text-stat (get-text-stat
                                       (conf-stat->presentation conf-stat)))
                           (text-mass (get-text 
                                       (cond
                                        ((eq type 'pres)
                                         (conf-stat->presentation conf-stat))
                                        ((eq type 'motd)
                                         (conf-stat->msg-of-day conf-stat))))))
      (let ((str (and text-mass (text->decoded-text-mass
                                 text-mass text-stat))))
        (lyskom-dispatch-edit-text
         lyskom-proc
         (apply
          'lyskom-create-misc-list
          (if (and (eq type 'pres)
                   (not (or (zerop (conf-stat->presentation conf-stat))
                            (null text-stat))))
              (append
               (lyskom-get-recipients-from-misc-list
                (text-stat->misc-info-list text-stat))
               (list 'COMM-TO
                     (conf-stat->presentation conf-stat)))
            (list 'RECPT
                  (cond
                   ((eq type 'motd)
                    (server-info->motd-conf lyskom-server-info))
                   ((eq type 'pres)
                    (if (conf-type->letterbox
                         (conf-stat->conf-type conf-stat))
                        (server-info->pers-pres-conf 
                         lyskom-server-info)
                      (server-info->conf-pres-conf
                       lyskom-server-info)))))))
         (conf-stat->name conf-stat)
         (if (and text-mass (string-match "\n" str))
             (substring str (match-end 0))
           (if (and (eq type 'pres)
                    (conf-type->letterbox (conf-stat->conf-type conf-stat)))
               (lyskom-get-string 'presentation-form)
             ""))
         (cond
          ((eq type 'pres) 'lyskom-set-presentation)
          ((eq type 'motd) 'lyskom-set-conf-motd))
         (conf-stat->conf-no conf-stat)))))
   (t
    (lyskom-format-insert 'not-supervisor-for
			  conf-stat))))


(def-kom-command kom-set-presentation (arg)
  "Set the presentation of a conference or person to the selected text.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive "P")
   (let ((conf-no (lyskom-read-conf-no 'what-to-set-pres-you '(all) nil nil t))
         (text-no (lyskom-read-text-no-prefix-arg 'what-text-to-set-as-pres-no t
                                                  lyskom-previous-text)))
     (lyskom-set-pres-or-motd-2
      conf-no
      text-no
      'pres)))

(def-kom-command kom-set-motd-text (arg)
  "Set the notice for a person or conference to the selected text.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive "P")
   (let ((conf-no (lyskom-read-conf-no 'what-to-set-motd-you '(all) nil nil t))
         (text-no (lyskom-read-text-no-prefix-arg
                   'what-text-to-set-as-motd-no t
                   lyskom-previous-text)))
     (lyskom-set-pres-or-motd-2 conf-no text-no 'motd)))

(defun lyskom-set-pres-or-motd-2 (conf-no text-no what)
  (let ((set-pres (eq what 'pres)))
    (blocking-do-multiple ((conf-stat (get-conf-stat conf-no))
                           (text-stat (get-text-stat text-no)))
      (cond ((null conf-stat)
             (lyskom-format-insert 'no-such-conf-or-pers)) 
            ((null text-stat)
             (lyskom-format-insert 'no-such-text-no text-no))
            (t
             (let ((existing (if set-pres
                                 (conf-stat->presentation conf-stat)
                               (conf-stat->msg-of-day conf-stat))))
               (when (or (zerop existing)
                         (progn (lyskom-view-text existing)
                                (lyskom-j-or-n-p 
                                 (lyskom-get-string (if set-pres
                                                        'conf-already-has-pres
                                                      'conf-already-has-motd)))))
                 (cache-del-conf-stat conf-no)
                 (cache-del-text-stat text-no)
                 (lyskom-format-insert (if set-pres
                                           'setting-conf-pres
                                         'setting-conf-motd)
                                       conf-stat text-no)
                 (lyskom-report-command-answer 
                  (cond ((eq what 'pres)
                         (blocking-do 'set-presentation conf-no text-no))
                        ((eq what 'motd)
                         (blocking-do 'set-conf-motd conf-no text-no)))))))))))

(def-kom-command kom-remove-presentation ()
  "Removes the presentation for a person or a conference. Please don't
remove a presentation without adding a new one. This can be accomplished
with the `kom-change-presentation' command."
  (interactive)
  (let ((conf-stat (lyskom-read-conf-stat 'who-to-remove-pres-for
                                          '(all) nil nil t)))
    (cond ((null conf-stat)
           (lyskom-insert-string 'cant-get-conf-stat))
          ((zerop (conf-stat->presentation conf-stat))
           (lyskom-format-insert 'has-no-presentation conf-stat))
          (t
           (lyskom-format-insert 'removing-pres-for-conf conf-stat 
                                 (conf-stat->presentation conf-stat))
           (lyskom-report-command-answer
            (blocking-do 'set-presentation (conf-stat->conf-no conf-stat) 0))))))

;;; ================================================================
;;;           Ta bort lapp p} d|rren - delete conf-motd

;;; Author: Linus Tolke (& Inge Wallin)


(def-kom-command kom-unset-conf-motd ()
  "Removes the notice for a person or conference."
  (interactive)
  (let ((conf-stat (or (lyskom-read-conf-stat 'who-to-remove-motd-for
                                              '(all) nil nil t)
		       (blocking-do 'get-conf-stat lyskom-pers-no))))
    (cond
     ((null conf-stat)
      (lyskom-insert-string 'cant-get-conf-stat))
     ((or lyskom-is-administrator
	  (lyskom-get-membership (conf-stat->supervisor conf-stat) t))
      ;; This works like a dispatch. No error handling.
      (lyskom-set-conf-motd 0 (conf-stat->conf-no conf-stat)))
     (t
      (lyskom-format-insert 'not-supervisor-for
			    conf-stat)))))
  

;;; ================================================================
;;;               G} till M|te - Go to a conference.

;;; Author: ???

(def-kom-command kom-go-to-conf (&optional conf-no)
  "Go to a conference and start reading texts there.

Changing conferences runs `kom-change-conf-hook' and
`kom-after-change-conf-hook'."
  (interactive)
  (let ((conf (if conf-no
                  (blocking-do 'get-conf-stat conf-no)
                (lyskom-read-conf-stat 'go-to-conf-p '(all) nil nil t))))
    (when (lyskom-check-go-to-conf conf)
      (lyskom-go-to-conf conf))))


(defun lyskom-go-to-conf (conf &optional no-prompt)
  "Go to the conference in CONF. CONF can be conf-no of conf-stat.
Allowed conferences are conferences and the mailboxes you are 
member of.
If NO-PROMPT is non-nil, don't print message that we have gone to conf."
  (if (numberp conf) (setq conf (blocking-do 'get-conf-stat conf)))
  (let ((membership (lyskom-get-membership (conf-stat->conf-no conf) t)))
    (unless no-prompt
      (lyskom-format-insert 'go-to-conf conf))

    ;; FIXME: DEBUG+++
    (let ((lyskom-inhibit-prefetch t))

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
              (let ((no-of-unread
                     (lyskom-read-num-range-or-date 
                      0 
                      (conf-stat->no-of-texts conf)
                      (lyskom-format 'initial-unread)
                      nil
                      t
                      nil)))
                (if (lyskom-add-member-by-no (conf-stat->conf-no conf)
                                             lyskom-pers-no no-of-unread)
                    (lyskom-do-go-to-conf conf
                                          (lyskom-get-membership
                                           (conf-stat->conf-no conf) t))
                  (lyskom-report-command-answer nil)))
	    (lyskom-insert-string 'no-ok))))))

    ;; DEBUG+++
    (lyskom-continue-prefetch)
    ))
  

(defun lyskom-do-go-to-conf (conf-stat membership)
  "Go to a conference. Args: CONF-STAT MEMBERSHIP.
 Put a read-info of type CONF first on lyskom-reading-list.
Args: CONF-STAT MEMBERSHIP"
  (let ((priority (lyskom-get-current-priority)))
    (lyskom-maybe-move-unread nil)
    (if conf-stat
	(lyskom-set-mode-line conf-stat))
    (let ((r 0)
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

      (cond (found
             (let ((read-info (read-list->nth lyskom-to-do-list r)))
               (read-list-enter-first read-info lyskom-reading-list)
               (read-list-delete-read-info (conf-stat->conf-no conf-stat)
                                           lyskom-to-do-list)
               (read-list-enter-first read-info lyskom-to-do-list)
               (set-read-info->priority read-info priority)
               (lyskom-enter-conf conf-stat read-info)))
	    (t
	     (lyskom-go-to-empty-conf conf-stat))))))



(defun lyskom-go-to-empty-conf (conf-stat)
  "Go to a conference with no unseen messages. Args: CONF-STAT."
  (unless lyskom-is-anonymous
    (blocking-do 'pepsi (conf-stat->conf-no conf-stat)))
  (lyskom-run-hook-with-args 'lyskom-change-conf-hook 
                      lyskom-current-conf
                      (conf-stat->conf-no conf-stat))
  (lyskom-run-hook-with-args 'kom-change-conf-hook 
                      lyskom-current-conf
                      (conf-stat->conf-no conf-stat))
  (setq lyskom-current-conf (conf-stat->conf-no conf-stat))
  (lyskom-enter-conf-print-unread conf-stat 0)
  (lyskom-run-hook-with-args 'lyskom-after-change-conf-hook 
                      lyskom-current-conf
                      (conf-stat->conf-no conf-stat))
  (lyskom-run-hook-with-args 'kom-after-change-conf-hook 
                      lyskom-current-conf
                      (conf-stat->conf-no conf-stat)))


(defun lyskom-get-current-priority ()
  "Return the current priority level."
  (or (and (read-list->first lyskom-reading-list)
           (read-info->priority (read-list->first lyskom-reading-list)))
      (and (read-list->first lyskom-to-do-list)
           (read-info->priority (read-list->first lyskom-to-do-list)))
      -1))


;;; ================================================================
;;;                 Skriva inl{gg - write text

;;; Author: ???


(def-kom-command kom-write-text (&optional arg)
  "Start writing a new text that isn't a comment to any existing text.
With a prefix argument don't ask for a recipient.

Several settings affect writing texts in general.
`kom-write-texts-in-window' controls which window is used to write the
comment; `kom-confirm-multiple-recipients' affects how multiple
recipients are handled."
  (interactive "P")
  (lyskom-tell-internat 'kom-tell-write-text)
  (lyskom-write-text arg 'who-send-text-to))

(defun lyskom-write-text (arg prompt)
  "Start writing a new text."
  (lyskom-nag-about-presentation)
  (let* ((tono (if (and arg lyskom-current-conf (not (zerop lyskom-current-conf)))
                   lyskom-current-conf
                 (lyskom-read-conf-no prompt '(pers conf) nil nil t)))
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
                              (lyskom-create-misc-list 'RECPT tono)
                              "" "")
          (lyskom-edit-text lyskom-proc
                            (if (lyskom-get-membership tono)
                                (lyskom-create-misc-list 'RECPT tono)
                              (lyskom-create-misc-list 
                               'RECPT tono
                               'RECPT lyskom-pers-no))
                            "" "")))))


;;; ================================================================
;;;                 Lista Personer - List persons
;;;
;;; Author: ceder
;;; Rewritten: linus

(def-kom-command kom-list-persons (match)
  "List all persons whose names start with a particular string.
For more flexible listing, use kom-list-re instead."
  (interactive (list (lyskom-read-string
		      (lyskom-get-string 'search-for-pers))))
  (let ((result (blocking-do 'lookup-z-name match 1 0)))
    (if result 
        (if (conf-z-info-list->conf-z-infos result)
            (lyskom-traverse info (conf-z-info-list->conf-z-infos result)
              (lyskom-list-pers-print info))
          (lyskom-format-insert 'no-matching-perss match))
      (lyskom-insert (lyskom-current-error)))))


(defun lyskom-list-pers-print (conf-z)
  "Print name of the person CONF-NO for kom-list-persons."
  (lyskom-format-insert "%[%#1@%5#2:p %#2P%]\n"
			(lyskom-default-button 'pers (conf-z-info->conf-no conf-z))
			conf-z))



;;; ================================================================
;;;              Lista M|ten - List conferences

;;; Author: ceder
;;; Rewritten: linus

(def-kom-command kom-list-conferences (&optional match)
  "List all conferences whose names start with a particular string.
Those that you are not a member in will be marked with an asterisk.
For more flexible searching, use `kom-list-re'."
  (interactive)

  (unless kom-allow-incompleteness
    (sit-for 0)
    (lyskom-prefetch-all-confs))

  (setq match (or match
                  (lyskom-read-string
                   (lyskom-get-string 'search-for-conf))))
  (let ((result (blocking-do 'lookup-z-name match 0 1)))
    (if result 
        (if (conf-z-info-list->conf-z-infos result)
            (progn 
              (lyskom-traverse info (conf-z-info-list->conf-z-infos result)
                (lyskom-list-conf-print info))
              (lyskom-insert 'conf-list-legend))
          (lyskom-format-insert 'no-matching-confs match))
      (lyskom-insert (lyskom-current-error)))))


(def-kom-command kom-list-created-conferences (arg)
  "List all conferences created by a particular person. With a prefix
argument, also list persons created by that person.

See `kom-allow-incompleteness'."
  (interactive "P")

  (unless kom-allow-incompleteness
    (sit-for 0)
    (lyskom-prefetch-all-confs))

  (blocking-do 'get-uconf-stat lyskom-pers-no)
  (let ((pers (lyskom-read-conf-stat
                  (if arg 'list-pers-confs-created-by 'list-confs-created-by)
                  '(all) nil nil t)))
    (lyskom-format-insert 'listing-confs-created-by (conf-stat->conf-no pers))
    (lyskom-message (lyskom-get-string (if arg 'getting-all-pers-confs 'getting-all-confs)))
    (let ((result (blocking-do 'lookup-z-name "" (if arg 1 0) 1)))
      (lyskom-message (lyskom-get-string (if arg 'getting-all-pers-confs-done 'getting-all-confs-done)))
      (if result
          (if (conf-z-info-list->conf-z-infos result)
              (let ((counter (vector nil
                                     1
                                     (length (conf-z-info-list->conf-z-infos result))
                                     0))
                    (calls nil)
                    (was-at-max (= (save-excursion (end-of-line) (point)) (point-max))))
                (condition-case arg
                    (progn
                      (lyskom-traverse conf-z (conf-z-info-list->conf-z-infos result)
                        (setq calls (cons
                                     (initiate-get-conf-stat 'main
                                                             'lyskom-list-created-conferences-2
                                                             (conf-z-info->conf-no conf-z)
                                                             counter
                                                             (conf-stat->conf-no pers)
                                                             arg
                                                             )
                                     calls)))
                      (lyskom-wait-queue 'main)
                      (if (eq 0 (elt counter 3))
                          (lyskom-format-insert 'no-created-confs pers)
                        (let ((window (get-buffer-window (current-buffer))))
                          (if (and window was-at-max)
                              (if (pos-visible-in-window-p (point-max) window)
                                  (goto-char (point-max))
                                (and kom-continuous-scrolling (lyskom-scroll)))))))
                  (quit (aset counter 0 t)
                        (lyskom-cancel-call 'main calls)
                        (signal 'quit nil))))
            (lyskom-insert (lyskom-get-string (if arg 'no-pers-confs-exist 'no-confs-exist))))
        (lyskom-format-insert (lyskom-current-error))))))

(defun lyskom-list-conf-membership-char (conf-no)
  (if lyskom-membership-is-read
      (cond ((lyskom-get-membership conf-no) ?\ )
            ((lyskom-get-membership conf-no t) ?-)
            (t ?*))
    (cond ((lyskom-try-get-membership conf-no) ?\ )
          ((lyskom-try-get-membership conf-no t) ?-)
          (t ?\?))))

(defun lyskom-list-created-conferences-2 (cs counter pers-no arg)
  (unless (elt counter 0)
    (aset counter 1 (1+ (elt counter 1)))
    (lyskom-message (lyskom-format (if arg 'finding-created-pers-confs 'finding-created-confs)
                                   (elt counter 1)
                                   (elt counter 2)))
    (when (and cs (memq pers-no (list (conf-stat->creator cs)
					(conf-stat->supervisor cs)
					(conf-stat->super-conf cs))))
      (aset counter 3 (1+ (elt counter 3)))
      (lyskom-format-insert "%5#2m %5#6d %#3c %4#4s %#5s %#2M\n"
                            (lyskom-default-button 'conf (conf-stat->conf-no cs))
                            cs
                            (lyskom-list-conf-membership-char (conf-stat->conf-no cs))
                            (concat (if (eq pers-no (conf-stat->creator cs)) (lyskom-get-string 'created-conf-letter) " ")
                                    (if (eq pers-no (conf-stat->supervisor cs)) (lyskom-get-string 'supervisor-conf-letter) " ")
                                    (if (and (conf-type->original (conf-stat->conf-type cs))
                                             (eq pers-no (conf-stat->super-conf cs)))
                                        (lyskom-get-string 'superconf-conf-letter) " "))
                            (cond ((conf-type->secret (conf-stat->conf-type cs)) (lyskom-get-string 'secret-conf-letter))
                                  ((conf-type->rd_prot (conf-stat->conf-type cs)) (lyskom-get-string 'protected-conf-letter))
                                  (t " "))
                            (conf-stat->garb-nice cs))
      (sit-for 0))))


(defun lyskom-list-conf-print (conf-z)
  "Print a line of info about CONF-NO.
If you are not member in the conference it will be flagged with an asterisk."
  (lyskom-format-insert "%5#1m %#2c %#1M\n"
			conf-z
                        (lyskom-list-conf-membership-char (conf-z-info->conf-no conf-z))))

;;; ================================================================
;;;                Lista med regexpar - List regexp


(defun lyskom-make-re-case-insensitive (re)
  "Convert the regexp RE to a case insensitive regexp."
  (unless lyskom-char-classes
    (setq lyskom-char-classes 
	  (lyskom-compute-char-classes lyskom-collate-table)))
  (let ((res nil)
	(input (listify-vector re))
	val)
    (while input
      (cond

       ;; Copy "[]" character sets literally.
       ((eq (car input) ?\[)
	(setq res (cons "[" res))
	(setq input (cdr input))
	(when input			;Handle "[]asdf]" properly.
	  (setq res (cons (make-string 1 (car input)) res))
	  (setq input (cdr input)))
	(while (and input (not (eq (car input) ?\])))
	  (setq res (cons (make-string 1 (car input)) res))
	  (setq input (cdr input)))
	(when input			;Don't forget the terminating "]".
	  (setq res (cons (make-string 1 (car input)) res))
	  (setq input (cdr input))))

       ;; Copy backslashed sequences literally.
       ((eq (car input) ?\\)
	(setq res (cons "\\" res))
	(setq input (cdr input))
	(when input
	  (setq res (cons (make-string 1 (car input)) res))
	  (setq input (cdr input))))

       ;; Copy special characters literally.
       ((memq (car input) '(?\( ?\) ?\| ?+ ?\* ?\?))
	(setq res (cons (make-string 1 (car input)) res))
	(setq input (cdr input)))

       ;; Create "[]" character sets for equivalent characters.
       ((setq val (cdr-safe (assoc (car input) lyskom-char-classes)))
	(setq res (cons "[" res))
	(if (member "]" val)		;"]" must come first.
	    (setq res (cons "]" res)))
	(while val
	  (cond
	   ((string= "]" (car val)))		;already handled
	   ((string= "-" (car val))
	    (setq res (cons "---" res)))
	   (t
	    (setq res (cons (car val) res))))
	  (setq val (cdr val)))
	(setq res (cons "]" res))
	(setq input (cdr input)))

       ;; Copy other characters literally.
       (t
	(setq res (cons (make-string 1 (car input)) res))
	(setq input (cdr input)))))

    (apply 'concat (nreverse res))))


(def-kom-command kom-list-re (regexp &optional case-sensitive what)
  "List all persons and conferences whose names match a particular
regular expression. Regular expressions are case insensitive. You 
will be asked whether to look for persons, conferences, or both.

Lisp documentation:
By default, the regexp will be converted so that the match is
performed in a case insensitive way.  If the optional argument
CASE-SENSITIVE is true, that conversion will not be performed.

If the optional argument WHAT is 'person, only persons will be listed.
If it is 'conf, only conferences will be listed."
  (interactive (list (lyskom-read-string
		      (lyskom-get-string 'search-re))
		     current-prefix-arg
		     (let ((sel (lyskom-a-or-b-or-c-p 'search-re-for-what
                                                      '(search-re-confs
                                                        search-re-persons
                                                        search-re-all)
                                                      'search-re-all)))
                       (cond
			((eq sel 'search-re-all) nil)
			((eq sel 'search-re-persons) 'pers)
			((eq sel 'search-re-confs) 'conf)
			(t nil)))))
  (unless case-sensitive
    (setq regexp (lyskom-make-re-case-insensitive regexp)))
  (lyskom-format-insert  (cond
			  ((eq what 'pers) 'matching-regexp-perss)
			  ((eq what 'conf) 'matching-regexp-confs)
			  (t 'matching-regexp))
			 regexp)
  (let ((conf-list (blocking-do 're-z-lookup regexp
				(if (eq what 'conf) 0 1)
				(if (eq what 'pers) 0 1))))
    (if conf-list
        (if (conf-z-info-list->conf-z-infos conf-list)
            (lyskom-traverse czi (conf-z-info-list->conf-z-infos conf-list)
              (lyskom-format-insert
               "%[%#1@%5#2:m %#3c %#2:M%]\n"
               (lyskom-default-button
                'conf (conf-z-info->conf-no czi))
               czi
               (if (conf-type->letterbox 
                    (conf-z-info->conf-type czi))
                   ?P ?M)
               ))
          (lyskom-format-insert (cond
				 ((eq what 'pers) 'no-matching-perss)
				 ((eq what 'conf) 'no-matching-confs)
				 (t 'no-matching-anys))
				regexp))
      (lyskom-format-insert (lyskom-current-error)))))


;;; ================================================================
;;;                 [ndra namn - Change name

;;; Author: Inge Wallin
;;; Changed by: Peter Eriksson(?)
;;; Changed again: Inge Wallin
;;; Rewritten: linus


(def-kom-command kom-change-name ()
  "Change the name of a person or conference. In order to change the
name of a conference or person, you need to be the supervisor of that
conference or person."
  (interactive)
  (let ((conf-stat (lyskom-read-conf-stat 'name-to-be-changed
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
            (progn
              (lyskom-format-insert 'change-name-done name
                                    (lyskom-default-button 'conf conf-stat))
              (cache-del-conf-stat (conf-stat->conf-no conf-stat)))
	  (lyskom-format-insert 'change-name-nope name 
				(lyskom-get-error-text lyskom-errno)
				lyskom-errno))))))

;;; ================================================================
;;;                 [ndra parentes - Change parenthesis

;;; Author: Per Cederqvist (template stolen from kom-change-name)

(def-kom-command kom-change-parenthesis ()
  "Change parenthsized contents in the  name of a person or conference.
In some LysKOM communities it is popular to put a witticism enclosed in
parenthesis at the end of one's name. This command is intended to make
it easy to change this information."
  (interactive)
  (let ((conf-stat (lyskom-read-conf-stat 'name-to-be-changed
                                          '(all) nil nil t)))
    (if (null conf-stat)
	(lyskom-insert-string 'no-such-conf-or-pers)
      (cond 
       ((string-match ".*(.*).*(.*)" (conf-stat->name conf-stat))
        (lyskom-insert-string 'too-many-parens-in-name))
       ((string-match "^\\(.*\\)(\\(.*\\))\\(.*\\)$" (conf-stat->name conf-stat))
        (let* ((pre-paren (match-string 1 (conf-stat->name conf-stat)))
               (post-paren (match-string 3 (conf-stat->name conf-stat)))
               (old-paren (match-string 2 (conf-stat->name conf-stat)))
               (paren (lyskom-read-string (lyskom-get-string 'new-paren)
                                          old-paren))
               (name (concat pre-paren "(" paren ")" post-paren)))
          (if (blocking-do 'change-name (conf-stat->conf-no conf-stat) name)
              (progn
                (lyskom-format-insert 'change-name-done name
                                      (lyskom-default-button 'conf conf-stat))
                (cache-del-conf-stat (conf-stat->conf-no conf-stat)))
            (lyskom-format-insert 'change-name-nope name
                                  (lyskom-get-error-text lyskom-errno)
                                  lyskom-errno))))
       (t (lyskom-insert-string 'no-paren-in-name))))))
	    


;;; ================================================================
;;;                [ndra organisat|r - Change supervisor

;;; Author: Inge Wallin
;;; Rewritten: linus

(def-kom-command kom-change-supervisor ()
  "Change the supervisor of a person or conference. You need to be the
supervisor of the person or conference to perform this operation."
  (interactive)
  (let ((supervisee (lyskom-read-conf-stat 'who-to-change-supervisor-for
                                           '(all) nil nil t)))
    (if (null supervisee)
	(lyskom-insert-string 'no-such-conf-or-pers)
      (lyskom-tell-internat 'kom-tell-change-supervisor)
      (let ((supervisor (lyskom-read-conf-stat 'new-supervisor
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
;;; Modified by: Linus Tolke, Johan Sundström, Joel Rosdahl

(def-kom-command kom-mark-text (&optional text-no)
  "Mark a text.
The settings `kom-symbolic-marks-alist' and `kom-default-mark' control
what mark to set."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-mark)))
  (if text-no
      (lyskom-mark-text text-no)
    (lyskom-insert 'confusion-what-to-mark)))


(def-kom-command kom-unmark-text (&optional text-no)
  "Unmark a text that was previously marked."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-unmark)))
  (if text-no
      (lyskom-unmark-text text-no)
    (lyskom-insert 'confusion-what-to-unmark)))


(defun lyskom-unmark-text (text-no)
  "Do the actual unmarking of the text TEXT-NO."
  (lyskom-format-insert 'unmarking-textno text-no)

  (if (blocking-do 'unmark-text text-no)
      (progn
	(lyskom-insert-string 'done)
	(cache-del-marked-text text-no))
    (lyskom-insert-string 'nope))     ;+++ lyskom-errno?
  (cache-del-text-stat text-no))


(defun lyskom-mark-text (text-no &optional mark)
  "Mark TEXT-NO using kom-default-mark (if non-nil) or prompt
the user for what mark to use."
  (let ((mark
	 (or mark
	     kom-default-mark
	     (lyskom-read-mark-type (lyskom-get-string 'what-mark) nil t))))
    (lyskom-format-insert 'marking-textno text-no)

    (if (blocking-do 'mark-text text-no mark)
        (progn
          (lyskom-insert-string 'done)
          (cache-add-marked-text text-no mark))
      (lyskom-insert-string 'nope))     ;+++ lyskom-errno?
    (cache-del-text-stat text-no)))

(defun lyskom-read-mark-type (prompt &optional nildefault create-nonexistent)
  "Ask user about symbolic mark type and return the (integer)
mark-type.  Prompt with PROMPT.  If NILDEFAULT is non-nil, nil is
returned if the user enters the empty string, otherwise the user is
prompted again. If CREATE-NONEXISTENT is t, the user is asked whether
the symbolic mark association should be created if it doesn't already
exist."
  (let ((mark-type nil) ; nil: not yet set, 'default: default chosen
        (completion-ignore-case t)
        (completions kom-symbolic-marks-alist))
    (while (and (not (eq mark-type 'default))
                (or (not (integerp mark-type))
                    (< mark-type 0)
                    (> mark-type 255)))
      (let* ((mark (lyskom-completing-read
                    prompt
                    (lyskom-maybe-frob-completion-table completions)))
             (mark-assoc (lyskom-string-assoc mark completions)))
        (when (null mark)
          (setq mark ""))
        (cond
         ;; Default completion.
         ((and nildefault (null mark))
          (setq mark-type 'default))

         ((null mark))

         ;; Correct completion.
         (mark-assoc
          (setq mark-type (cdr mark-assoc)))

         ;; Incorrect completion, integer entered.
         ((string-match "\\`\\s-*[0-9]+\\s-*\\'" mark)
          (setq mark-type (string-to-int mark)))

         ;; Incorrect completion; create new symbolic mark type.
         ((and create-nonexistent
               (not (string= mark ""))
               (lyskom-j-or-n-p
                (lyskom-format (lyskom-get-string
                                'want-to-create-symbolic-mark)
                               mark)))
          (let ((new-mark-type (lyskom-allocate-mark-type)))
            (if (not new-mark-type)
                (lyskom-insert 'no-mark-types-left)
              (lyskom-format-insert 'creating-symbolic-mark-type
                                    mark
                                    new-mark-type)
              (setq kom-symbolic-marks-alist
                    (cons (cons mark new-mark-type)
                          kom-symbolic-marks-alist))
              (lyskom-save-options
               (current-buffer)
               (lyskom-get-string 'saving-settings)
               (lyskom-get-string 'saving-settings-done)
               (lyskom-get-string 'could-not-save-options))
              (setq mark-type new-mark-type))))

         ;; Incorrect completion.
         (t (lyskom-insert 'erroneous-mark)))))

    (if (eq mark-type 'default)
        nil
      mark-type)))


(defun lyskom-allocate-mark-type ()
  "Returns the first mark type available that is neither named nor used.
If no such type existed, the least used non-named mark type is returned.
If no such existed either, nil is returned."
  (setq kom-symbolic-marks-alist
        (sort kom-symbolic-marks-alist
              (function (lambda (x y) (< (cdr x) (cdr y))))))
  (let ((i 0)
        (list kom-symbolic-marks-alist)
	(used (lyskom-get-least-used-mark-types-alist))
        (found nil))
    (while (and (not found)
                (< i 256))
      (when (not (assq i used)) ; mark type i not presently used?
	(when list
	  (if (= i (cdr (car list))) ; already named?
	      (setq list (cdr list))
	    (setq found t)))) ; neither used nor named!
      (when (not found)
	(++ i)))
    (if (and found (< i 256))
        i
      ; no unused and unnamed mark type available; fall back
      ; to the least used not-yet-named type, if available:
      (when (and used
		 (< (length used) 256)
		 (< (length list) 256))
	(cdr (car used))))))


(defun lyskom-get-least-used-mark-types-alist ()
  "Returns an alist from mark type to number of such marks, ordered by
increasing number of marks per mark type (and, when equal, by mark type)."
  (let ((mark-list (cache-get-marked-texts))
	(cnt-alist nil)) ; the number of texts marked by each mark type
    ;; Count the number of texts marked per mark type:
    (while (not (null mark-list))
      (let ((mark (car mark-list)))
        (when mark
          (let* ((type (mark->mark-type mark))
                 (tcnt (assq type cnt-alist)))
            (when tcnt (setq tcnt (cdr tcnt)))
            (setq cnt-alist
                  (lyskom-set-alist
                   cnt-alist type (if (null tcnt) 1 (1+ tcnt)))))))
      (setq mark-list (cdr mark-list)))

    ;; Sort the list, least-used, lowest number of mark type first:
    (sort cnt-alist
	  (function (lambda (x y)
		      (cond
		       ((< (cdr x) (cdr y)) t)
		       ((= (cdr x) (cdr y)) (< (car x) (car y)))
		       (t nil)))))))



;;; ================================================================
;;;          ]terse alla markerade - Review marked texts

;;; Author: Inge Wallin
;;; Modified by: Joel Rosdahl

(def-kom-command kom-review-marked-texts ()
  "Review all texts you have marked with a particular mark."
  (interactive)
  (lyskom-review-marked-texts
   (lyskom-read-mark-type (lyskom-get-string 'what-mark-to-view) t)))


(def-kom-command kom-review-all-marked-texts ()
  "Review all texts you have marked, regardless of mark."
  (interactive)
  (lyskom-review-marked-texts nil))

(def-kom-command kom-unread-marked-texts ()
  "Mark all texts you have marked with a particular mark as unread."
  (interactive)
  (lyskom-unread-marked-texts
   (lyskom-read-mark-type (lyskom-get-string 'what-mark-to-unread) t)))


(def-kom-command kom-unread-all-marked-texts ()
  "Mark all texts you have marked, regardless of mark, as unread."
  (interactive)
  (lyskom-unread-marked-texts nil))
  

(defun lyskom-unread-marked-texts (mark-no)
  "Mark all marked texts with the mark equal to MARK-NO unread. 
If MARK-NO is nil, review all marked texts."
  (let ((text-list (lyskom-get-marked-texts mark-no)))
    (if (null text-list)
	(lyskom-insert (if (null mark-no)
			   (lyskom-get-string 'no-marked-texts)
			 (lyskom-format 'no-marked-texts-mark
                                        (lyskom-symbolic-mark-type-string
                                         mark-no))))
      (lyskom-traverse text-no text-list
        (lyskom-format-insert 'marking-text-unread text-no)
        (lyskom-report-command-answer
         (lyskom-mark-unread text-no))))))
  

(defun lyskom-review-marked-texts (mark-no)
  "Review all marked texts with the mark equal to MARK-NO. 
If MARK-NO is nil, review all marked texts."
  (let ((text-list (lyskom-get-marked-texts mark-no)))
    (if (null text-list)
	(lyskom-insert (if (null mark-no)
			   (lyskom-get-string 'no-marked-texts)
			 (lyskom-format 'no-marked-texts-mark
                                        (lyskom-symbolic-mark-type-string
                                         mark-no))))
      (let ((read-info (lyskom-create-read-info
			'REVIEW-MARK nil 
			(lyskom-review-get-priority)
			(lyskom-create-text-list text-list)
			nil t)))
	(read-list-enter-read-info read-info lyskom-reading-list t)
	(read-list-enter-read-info read-info lyskom-to-do-list t)))))

(defun lyskom-get-marked-texts (mark-no)
  "Return a list of all texts marked with MARK-NO."
  (let ((text-list nil))
    (lyskom-traverse mark (cache-get-marked-texts)
      (if (and mark
               (or (null mark-no)
                   (eq mark-no (mark->mark-type mark))))
          (setq text-list (cons (mark->text-no mark)
                                text-list))))
    text-list))


;;; ================================================================
;;;                 [ndra L|senord - Change password

;;; Author: Inge Wallin


(def-kom-command kom-change-password ()
  "Change the password for a person. To change the password for a
person you need either the old password for the person, or have
administrative privileges enabled."
  (interactive)
  (let ((pers-no (lyskom-read-conf-no 'whos-passwd '(pers) nil nil t))
	(old-pw (silent-read (lyskom-get-string 'old-passwd)))
	(new-pw1 (silent-read (lyskom-get-string 'new-passwd)))
	(new-pw2 (silent-read (lyskom-get-string 'new-passwd-again))))

    (if (lyskom-string= new-pw1 new-pw2)
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
  '(((nil  1  1 nil nil nil) . newyearday)
    ((nil  1  6 nil nil nil) . 13dayxmas)
    ((nil  1 13 nil nil nil) . 20dayxmas)
    ((nil  2  2 nil nil nil) . kyndeldag)
    ((nil  2 29 nil nil nil) . skottdag)
    ((nil  3  8 nil nil nil) . intwomday)
    ((nil  3 25 nil nil nil) . mariebdag)
    ((nil  3 29 nil nil nil) . lysbday)
    ((nil  4 30 nil nil nil) . cgdag)
    ((nil  6  6 nil nil nil) . sixjune)
    ((nil  6 24 nil nil nil) . johannesdday)
    ((nil  8 15 nil nil nil) . holdnose)
    ((nil 10 24 nil nil nil) . fnday)
    ((nil 11  1 nil nil nil) . allhelgonadag)
    ((nil 12 13 nil nil nil) . luciaday)
    ((nil 12 24 nil nil nil) . xmaseve)
    ((nil 12 25 nil nil nil) . xmasday)
    ((nil 12 28 nil nil nil) . varnlosdag)
    ((nil 12 31 nil nil nil) . newyeareve)
    ((nil 12 31  23 nil nil) . newyearevelate)
))


(lyskom-with-external-functions (calendar-iso-from-absolute
                                 calendar-absolute-from-gregorian)
  (def-kom-command kom-display-time ()
    "Display the current date and time, according to the server. If
`kom-show-week-number', include the ISO week number. Display of
today's name is controlled by `kom-show-namedays'."
    (interactive)
    (let ((time (lyskom-current-server-time))
          (lyskom-last-text-format-flags nil)
          (weekno nil))
      (lyskom-format-insert
       (if kom-show-week-number
           (condition-case nil
               (progn (require 'calendar)
                      (require 'cal-iso)
                      (setq weekno
                            (car (calendar-iso-from-absolute
                                  (calendar-absolute-from-gregorian 
                                   (list (time->mon time)
                                         (time->mday time)
                                         (time->year time))))))
                      'time-is-week)
             (error 'time-is))
         'time-is)
       (lyskom-format-time 'timeformat-day-yyyy-mm-dd-hh-mm-ss time)
       ;; Kult:
       (if (and (= (time->hour time)
                   (+ (/ (time->sec time) 10)
                      (* (% (time->sec time) 10) 10)))
                (= (/ (time->min time) 10)
                   (% (time->min time) 10)))
           (lyskom-get-string 'palindrome)
         "")
       weekno)
      ;; Mera kult
      (mapcar (function 
               (lambda (el)
                 (let ((when (car el))
                       (event (cdr el)))
                   (if (and (or (null (elt when 0))
                                (= (time->year time) (elt when 0)))
                            (or (null (elt when 1))
                                (= (time->mon time) (elt when 1)))
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
                             (lyskom-format-insert 
                              "%#1t"
                              (lyskom-format event
                                             (time->year time)
                                             (time->mon  time)
                                             (time->mday time)
                                             (time->hour time)
                                             (time->min  time)
                                             (time->sec  time))))
                         (error nil))))))
              lyskom-times)

      (when kom-show-namedays
        (let ((tmp (lyskom-nameday time)))
          (when tmp
            (lyskom-insert "\n")
            (lyskom-insert tmp)))))
    (lyskom-insert "\n"))

)




;;; ================================================================
;;;                Vilka ({r inloggade) - Who is on?
;;;
;;; Author: ???
;;; Rewritten by: David K}gedal


(put 'lyskom-no-users 'error-conditions
     '(error lyskom-error lyskom-no-users))

(def-kom-command kom-who-is-on (&optional arg)
  "Display a list of active connected users.

With a positive prefix argument ARG, list sessions who have been
active in the last ARG minutes. With a positive zero prefix argument
\(i.e. \"0\", not \"-0\"), list all visible sessions. With a negative
nonzero prefix argument ARG, list both visible and invisible sessions
who have been active in the last -ARG minutes. With a negative zero
prefix argument (C-u -), list all sessions.

Several variables affect display. See `kom-show-where-and-what', 
`kom-show-since-and-when' and `kom-idle-hide' for more information."
  (interactive "P")
  (condition-case nil
      (if (lyskom-have-feature dynamic-session-info)
	  (lyskom-who-is-on-9 arg)
	(lyskom-who-is-on-8))
    (lyskom-no-users
     (lyskom-insert (lyskom-get-string 'null-who-info)))))

;;; ================================================================
;;;                Vilka ({r inloggade i) möte - Who is on in a conference?

;;; Author: petli

(def-kom-command kom-who-is-on-in-conference (&optional arg)
  "Display a list of all connected users who are members of a
particular cnoference.

With a positive prefix argument ARG, list sessions who have been
active in the last ARG minutes. With a positive zero prefix argument
\(i.e. \"0\", not \"-0\"), list all visible sessions. With a negative
nonzero prefix argument ARG, list both visible and invisible sessions
who have been active in the last -ARG minutes. With a negative zero
prefix argument \(C-u -), list all sessions.

Several variables affect display. See `kom-show-where-and-what',
`kom-show-since-and-when' and `kom-idle-hide' for more information."
  (interactive "P")
  (let ((conf-stat 
	 (lyskom-read-conf-stat 'who-is-on-in-what-conference
                                '(all) nil nil t)))
    (condition-case nil
	(if (lyskom-have-feature dynamic-session-info)
	    (lyskom-who-is-on-9 arg conf-stat)
	  (lyskom-who-is-on-8 conf-stat))
      (lyskom-no-users
       (lyskom-insert (lyskom-get-string 'null-who-info))))))

;;; ================================================================
;;;                Vilka ({r n{rvarande i) möte - Who is present in a conference?

;;; Author: Christer Ekholm
;;; Copied from kom-who-is-on-in-conference by petli

(def-kom-command kom-who-is-present-in-conference (&optional arg)
  "Display a list of all connected users currently present in a
particular conference.

With a positive prefix argument ARG, list sessions who have been
active in the last ARG minutes. With a positive zero prefix argument
\(i.e. \"0\", not \"-0\"), list all visible sessions. With a negative
nonzero prefix argument ARG, list both visible and invisible sessions
who have been active in the last -ARG minutes. With a negative zero
prefix argument \(C-u -), list all sessions.

Several variables affect display. See `kom-show-where-and-what',
`kom-show-since-and-when' and `kom-idle-hide' for more information."
  (interactive "P")
  (let ((conf-stat 
	 (lyskom-read-conf-stat 'who-is-present-in-what-conference
				'(all) nil nil t)))
    (condition-case nil
	(if (lyskom-have-feature dynamic-session-info)
	    (lyskom-who-is-on-9 arg conf-stat t)
	  (lyskom-who-is-on-8 conf-stat t))
      (lyskom-no-users
       (lyskom-insert (lyskom-get-string 'null-who-info))))))

;;; ================================================================
;;;                Vilka vänner (är inloggade) - What friends are on

;;; Author: Ulrik Haugen

(def-kom-command kom-who-is-on-and-friend (&optional arg)
  "Display a list of all connected users in `kom-friends'.

With a positive prefix argument ARG, list sessions who have been
active in the last ARG minutes. With a positive zero prefix argument
\(i.e. \"0\", not \"-0\"), list all visible sessions. With a negative
nonzero prefix argument ARG, list both visible and invisible sessions
who have been active in the last -ARG minutes. With a negative zero
prefix argument \(C-u -), list all sessions.

Several variables affect display. See `kom-show-where-and-what',
`kom-show-since-and-when' and `kom-idle-hide' for more information."
  (interactive "P")
  (condition-case nil
      (if (lyskom-have-feature dynamic-session-info)
	  (lyskom-who-is-on-9 arg nil nil t)
	(lyskom-who-is-on-8 nil nil t))
    (lyskom-no-users
     (lyskom-insert (lyskom-get-string 'null-who-info)))))

(defun lyskom-who-is-on-8 (&optional conf-stat show-present-only
				     show-friends-only)
  "Display a list of all connected users.
Uses Protocol A version 8 calls"
  (let* ((who-info-list (blocking-do 'who-is-on))
	 (who-list (sort
		    (funcall (if show-friends-only
				 #'lyskom-select-friends-from-who-list
			       #'identity)
			     (cond (show-present-only
				    (lyskom-who-is-present-check-membership-8
				     who-info-list conf-stat))
				   (conf-stat
				    (lyskom-who-is-on-check-membership-8
				     who-info-list conf-stat))
				   (t
				    (listify-vector who-info-list))))
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
	 (lyskom-default-conf-string 'not-present-anywhere)
         (lyskom-default-pers-string 'unknown-person))

    (cond (show-friends-only
           (lyskom-format-insert 'who-is-friend))
          (show-present-only
	   (lyskom-format-insert 'who-is-active-and-present conf-stat))
	  (conf-stat
	   (lyskom-format-insert 'who-is-active-and-member conf-stat)))
	  
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
     (concat (make-string (- (lyskom-window-width) 1) ?-) "\n"))
    
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
	     (concat "(" (or (string-replace-match "\n" (who-info->doing-what who-info) " " t t)
                             (who-info->doing-what who-info)")")))))
      (setq who-list (cdr who-list)))
      
      (lyskom-insert (concat (make-string (- (lyskom-window-width) 1) ?-)
			     "\n"))
      (lyskom-insert (lyskom-format 'total-visible-users total-users
                                    (lyskom-format-time
                                     'timeformat-day-yyyy-mm-dd-hh-mm-ss)))))


(defun lyskom-who-is-on-9 (arg &optional conf-stat show-present-only
			       show-friends-only)
  "Display a list of all connected users.
Uses Protocol A version 9 calls"
  (let* ((wants-invisibles (or (and (numberp arg) (< arg 0))
                               (and (symbolp arg) (eq '- arg))))
	 (idle-hide (if (numberp arg) (abs arg) 
                      (cond ((eq '- arg) 0)
                            ((numberp kom-idle-hide) kom-idle-hide)
                            (kom-idle-hide 30)
                            (t 0))))
	 (who-info-list (blocking-do 'who-is-on-dynamic
				     't wants-invisibles (* idle-hide 60)))
	 (who-list (sort 
		    (funcall (if show-friends-only
				 #'lyskom-select-friends-from-who-list
			       #'identity)
			     (cond (show-present-only
				    (lyskom-who-is-present-check-membership-9
				     who-info-list conf-stat))
				   (conf-stat
				    (lyskom-who-is-on-check-membership-9
				     who-info-list conf-stat))
				   (t
				    (listify-vector who-info-list))))
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
	 (format-string-3 (lyskom-info-line-format-string
			   session-width "D" "s"))
	 (lyskom-default-conf-string 'not-present-anywhere)
         (lyskom-default-pers-string 'unknown-person))

    (if (zerop idle-hide)
	(lyskom-insert (lyskom-get-string 'who-is-active-all))
      (lyskom-format-insert 'who-is-active-last-minutes idle-hide))

    (if wants-invisibles
	(lyskom-insert (lyskom-get-string 'showing-invisibles)))

    (cond (show-friends-only
           (lyskom-format-insert 'who-is-friend))
          (show-present-only
	   (lyskom-format-insert 'who-is-active-and-present conf-stat))
	  (conf-stat
	   (lyskom-format-insert 'who-is-active-and-member conf-stat)))
			  
    (lyskom-format-insert format-string-2
			  ""
			  (lyskom-get-string 'lyskom-name)
			  (lyskom-get-string 'is-in-conf))
    (if kom-show-where-and-what
	(lyskom-format-insert format-string-2
			      ""
			      (lyskom-get-string 'from-machine)
			      (lyskom-get-string 'is-doing)))

    (if kom-show-since-and-when
	(lyskom-format-insert format-string-3
			      ""
			      (lyskom-get-string 'connection-time)
			      (lyskom-get-string 'active-last)))
    
    (lyskom-insert
     (concat (make-string (- (lyskom-window-width) 1) ?-) "\n"))
    
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
	       (concat "(" (or (string-replace-match "\n" (dynamic-session-info->what-am-i-doing who-info) " " t t)
                               (dynamic-session-info->what-am-i-doing who-info))
		       ")"))))
	(if kom-show-since-and-when
	    (let ((active 
                   (if (session-flags->user_active_used 
                        (dynamic-session-info->flags who-info))
                       (if (< (dynamic-session-info->idle-time who-info) 60)
                           (lyskom-get-string 'active)
                         (lyskom-format-secs
                          (dynamic-session-info->idle-time who-info)))
                     (lyskom-get-string 'Unknown2))
                     )
		  defer-info
		  static
		  since)
	      (cond (kom-deferred-printing
		     (setq static (cache-get-static-session-info session-no))
		     (if static
			 (setq since
			       (upcase-initials
				(lyskom-format-time
                                 'timeformat-day-yyyy-mm-dd-hh-mm-ss
				 (static-session-info->connection-time static))))
		       (setq defer-info
			     (lyskom-create-defer-info
			      'get-static-session-info
			      session-no
			      'lyskom-insert-deferred-session-info-since
			      (make-marker)
			      (length lyskom-defer-indicator)
			      "%#1s"))
		       (setq since defer-info)))
		    (t
		     (setq static
			   (blocking-do 'get-static-session-info session-no))
		     (setq since (upcase-initials
				  (lyskom-format-time
                                 'timeformat-day-yyyy-mm-dd-hh-mm-ss
				   (static-session-info->connection-time static))))))
	      (lyskom-format-insert
	       format-string-3
	       ""
	       since
	       active)))
	
	(setq who-list (cdr who-list))))
    
    (lyskom-insert (concat (make-string (- (lyskom-window-width) 1) ?-)
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
		    total-users
                    (lyskom-format-time
                     'timeformat-day-yyyy-mm-dd-hh-mm-ss)))))

(defun lyskom-who-is-on-check-membership-8 (who-info-list conf-stat)
  "Returns a list of those in WHO-INFO-LIST which is member in CONF-STAT."
  (let ((members (blocking-do 'get-members (conf-stat->conf-no conf-stat)
                              0 (conf-stat->no-of-members conf-stat)))
	(len (length who-info-list))
	(i 0)
	(res nil)
        (mship nil))
    (while (< i len)
      (setq mship (lyskom-member-list-find-member
                   (who-info->pers-no (aref who-info-list i))
                   members))
      (when (and mship
               (not (membership-type->passive 
                     (member->membership-type mship))))
	  (setq res (cons (aref who-info-list i) res)))
      (setq i (1+ i)))
    res))

(defun lyskom-who-is-on-check-membership-9 (who-info-list conf-stat)
  "Returns a list of those in WHO-INFO-LIST which is member in CONF-STAT."
  (let ((members (blocking-do 'get-members (conf-stat->conf-no conf-stat)
			      0 (conf-stat->no-of-members conf-stat)))
	(len (length who-info-list))
	(i 0)
	(res nil)
        (mship nil))
    (while (< i len)
      (setq mship (lyskom-member-list-find-member
                   (dynamic-session-info->person (aref who-info-list i))
                   members))
      (when (and mship
               (not (membership-type->passive 
                     (member->membership-type mship))))
	  (setq res (cons (aref who-info-list i) res)))
      (setq i (1+ i)))
    res))

(defun lyskom-who-is-present-check-membership-8 (who-info-list conf-stat)
  "Returns a list of those in WHO-INFO-LIST which is present in CONF-STAT."
  (let ((len (length who-info-list))
	(i 0)
	(res nil))
    (while (< i len)
      (if (eq (who-info->working-conf (aref who-info-list i))
	      (conf-stat->conf-no conf-stat))
	  (setq res (cons (aref who-info-list i) res)))
      (setq i (1+ i)))
    res))

(defun lyskom-who-is-present-check-membership-9 (who-info-list conf-stat)
  "Returns a list of those in WHO-INFO-LIST which is present in CONF-STAT."
  (let ((len (length who-info-list))
	(i 0)
	(res nil))
    (while (< i len)
      (if (eq (dynamic-session-info->working-conference (aref who-info-list i))
	      (conf-stat->conf-no conf-stat))
	  (setq res (cons (aref who-info-list i) res)))
      (setq i (1+ i)))
    res))

(defun lyskom-select-friends-from-who-list (who-list)
  "Returns a list of friends in WHO-LIST"
  (let ((result nil)
        (extended-friends (cons lyskom-pers-no kom-friends)))
    (while (not (null who-list))
      (when (memq (dynamic-session-info->person (car who-list))
		  extended-friends)
	(setq result (cons (car who-list) result)))
      (setq who-list (cdr who-list)))
    result))

(defun lyskom-insert-deferred-session-info (session-info defer-info)
  (if session-info
      (lyskom-replace-deferred defer-info
			       (lyskom-combine-username
				(static-session-info->username session-info)
				(static-session-info->ident-user session-info)
				(static-session-info->hostname session-info)))
    (lyskom-replace-deferred defer-info "")))

(defun lyskom-insert-deferred-session-info-since (session-info defer-info)
  (if session-info
      (lyskom-replace-deferred defer-info
			       (upcase-initials
				(lyskom-format-time
                                 'timeformat-day-yyyy-mm-dd-hh-mm-ss
				 (static-session-info->connection-time session-info))))
    (lyskom-replace-deferred defer-info "")))

;;; =====================================================================
;;;                 Lista klienter - List clients
;;; Author: David Kågedal
;;; Modified: Daivd Byers

		 
(def-kom-command kom-list-clients (prefix)
  "Display a list of all connected sessions, including current logon
and client software.

With a positive prefix argument ARG, list sessions who have been
active in the last ARG minutes. With a positive zero prefix argument
\(i.e. \"0\", not \"-0\"), list all visible sessions. With a negative
nonzero prefix argument ARG, list both visible and invisible sessions
who have been active in the last -ARG minutes. With a negative zero
prefix argument \(C-u -), list all sessions."
  (interactive "P")
  (let* ((want-invisible (if prefix t nil))
         (who-info-list (blocking-do 'who-is-on-dynamic t want-invisible nil))
         (who-list (sort (listify-vector who-info-list)
                         (function
                          (lambda (who1 who2)
                            (< (dynamic-session-info->session who1)
                               (dynamic-session-info->session who2))))))
	 (total-users (length who-list))
	 (s-width (1+ (length (int-to-string
			       (dynamic-session-info->session
				(nth (1- total-users) who-list))))))
	 (format-string (lyskom-info-line-format-string
			 s-width "P" (if kom-deferred-printing "D" "s")))
         (collect (make-collector)))
    (lyskom-format-insert format-string
			  ""
			  (lyskom-get-string 'lyskom-name)
			  (lyskom-get-string 'lyskom-client))
    (lyskom-insert
     (concat (make-string (- (lyskom-window-width) 2) ?-) "\n"))

    (while who-list
      (let* ((who-info (car who-list))
	     (session-no (int-to-string (dynamic-session-info->session who-info)))
	     (my-session (if (= lyskom-session-no
				(dynamic-session-info->session who-info))
			     "*"
			   " "))
	     (client (if kom-deferred-printing
			 (lyskom-create-defer-info
			  'get-client-name
			  (dynamic-session-info->session who-info)
			  'lyskom-deferred-client-1
			  nil nil nil	; Filled in later
			  (list (dynamic-session-info->session who-info)
                                collect))
		       (blocking-do-multiple
			   ((name (get-client-name
				   (dynamic-session-info->session who-info)))
			    (version (get-client-version
				      (dynamic-session-info->session who-info))))
                         (lyskom-list-clients-collect name version collect)
			 (concat name " " version)))))
	(lyskom-format-insert
	 format-string
	 (concat session-no my-session)
	 (dynamic-session-info->person who-info)
	 client))
      (setq who-list (cdr who-list)))

    (lyskom-insert (concat (make-string (- (lyskom-window-width) 2) ?-)
			   "\n"))
    (lyskom-insert (lyskom-format (if want-invisible
                                      'total-users
                                    'total-visible-users) total-users
                                    (lyskom-format-time
                                     'timeformat-day-yyyy-mm-dd-hh-mm-ss)))
    (lyskom-format-insert "%#1D\n"
                          (lyskom-create-defer-info
                           'get-time nil
                           'lyskom-list-clients-statistics-1
                           nil nil nil collect))
))


(defun lyskom-list-clients-collect (client version collect)
  "Collect client statistics"
  (let* ((name (concat client " " version))
         (el (assoc name (collector->value collect))))
    (if el
        (setcdr el (1+ (cdr el)))
      (set-collector->value collect (cons (cons name 1)
                                          (collector->value collect))))))

(defun lyskom-list-clients-statistics-1 (time arg defer-info)
  (initiate-get-time 'deferred
                     'lyskom-list-clients-statistics-2 
                     defer-info))

(defun lyskom-list-clients-statistics-2 (time defer-info)
  (lyskom-replace-deferred 
   defer-info
   (concat "\n"
           (lyskom-get-string 'client-statistics)
           (mapconcat (lambda (el)
                        (lyskom-format 'client-statistics-line
                                       (if (equal (car el) " ")
                                           (lyskom-get-string 'Unknown)
                                         (car el))
                                       (cdr el)))
                      (nreverse
                       (sort (collector->value (defer-info->data defer-info))
                             (lambda (a b)
                               (string-lessp (car a) (car b)))))
                      "\n")
           "\n")))
                                        


(defun lyskom-deferred-client-1 (name defer-info)
  (let* ((data (defer-info->data defer-info))
         (session (if (consp data) (elt data 0) data))
         (collector (if (consp data) (elt data 1) nil)))
    (initiate-get-client-version 'deferred
                                 'lyskom-deferred-client-2
                                 session
                                 defer-info
                                 name
                                 collector
                                 )))

(defun lyskom-deferred-client-2 (version defer-info name collect)
  (when collect (lyskom-list-clients-collect name version collect))
  (lyskom-replace-deferred defer-info (if (zerop (length name))
					  "-"
					(concat name " " version))))



(defun lyskom-info-line-format-string (prefixlen type1 type2)
  "Return a format string suitable for inserting who-info lines etc."
  (let* ((plen (or prefixlen 7))
         (width (- (lyskom-window-width) plen 2)))
    (concat "%" (int-to-string plen) "#1s"
	    "%=-"
	    (int-to-string (/ width 2))
	    "#2" type1
	    " %=-"
	    (int-to-string (+ (/ width 2) (% width 2)))
	    "#3" type2
	    "\n")))
    
(defun lyskom-window-width ()
  "Returns the width of the lyskom-window or the screen-width if not displayed."
  (let ((win (get-buffer-window (current-buffer))))
    (cond
     (win (window-width win))
     (t (lyskom-frame-width)))))


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
		  (lyskom-string= (downcase sent) (downcase gott))
		  (lyskom-string= (downcase sent)
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
	(if (lyskom-string= uhost hostname)
	    (concat user "@" hostname)
	  (concat username "@" hostname)))
    (concat username "@" hostname)))


;;; ================================================================
;;;            Status (för) Session - Status (for a) session
;;;
;;; Author: David Byers

(def-kom-command kom-status-session (&optional text-or-session-no)
  "Show status for all sessions a person has.

Lisp documentation:
Optional argument should be a list of sessions to get information
about or a single session number. When used interactively, the prefix
argument is used to find a text whose author's status is shown."
  (interactive (and current-prefix-arg ; only peek at textno:s when prefixed!
                    (list (lyskom-read-text-no-prefix-arg
                           'text-to-see-author-status-of))))
  (let ((sessions (or (when (and text-or-session-no (interactive-p))
			(lyskom-session-from-conf
			 (text-stat->author
			  (blocking-do 'get-text-stat text-or-session-no))))
		      (cond
		       ((listp text-or-session-no) text-or-session-no)
		       ((numberp text-or-session-no) (list text-or-session-no)))
                      (lyskom-read-session-no 'status-for-session)))
	who-info)
    (cond ((null sessions)
           (lyskom-insert-string 'no-such-session-r))
          ((and (numberp (car sessions))
                (<= (car sessions) 0))
	   (let ((pers-no (- (car sessions))))
	     (lyskom-format-insert
	      (lyskom-get-string 'person-not-logged-in-r)
	      pers-no
	      (lyskom-format-time 'date-and-time
				  (pers-stat->last-login
				   (blocking-do 'get-pers-stat pers-no))))))
          (t
           (if (lyskom-have-feature dynamic-session-info)
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
             (setq string (or (string-replace-match "\n" string " " t t) string))
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
               (setq string (or (string-replace-match "\n" string " " t t ) string))
	       (if (string= string "")
		   (lyskom-get-string 'unknown-doing-what)
		 string))
	     client
	     (if (not (eq (dynamic-session-info->working-conference info) 0))
		 (lyskom-get-string 'doing-where-conn)
	       (lyskom-get-string 'doing-nowhere-conn))
	     (lyskom-format-time
              'timeformat-day-yyyy-mm-dd-hh-mm-ss
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
  "Skips all comments to the selected text. Descends recursively in
the comment tree, marking all texts found as read. 

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'jump-from-text)))
  (cond ((and (null current-prefix-arg)
              (or (eq 'REVIEW-TREE (read-info->type (read-list->first lyskom-reading-list)))
                  (eq 'REVIEW-FAQ-TREE (read-info->type (read-list->first lyskom-reading-list)))))
         (lyskom-start-of-command 'kom-jump)
         (lyskom-format-insert 'jumping-from-text-review)
         (unwind-protect
             (progn 
               (set-read-list-del-first lyskom-reading-list)
               (set-read-list-del-first lyskom-to-do-list))
           (lyskom-end-of-command)))
        (text-no
         (lyskom-start-of-command 'kom-jump)
         (lyskom-format-insert 'jumping-from-text text-no)
         (initiate-get-text-stat 'main 'lyskom-jump text-no t)
         (lyskom-run 'main 'lyskom-end-of-command))
        (t (lyskom-start-of-command 'kom-jump)
           (lyskom-insert-string 'have-to-read)
           (lyskom-end-of-command))))


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
       ((and (memq (misc-info->type misc) lyskom-comment-types-list)
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

;;; Author: David Byers

(def-kom-command kom-add-recipient (text-no)
  "Add a recipient to a text.
If the recipient already exists, this will convert the current recipient
type to a regular copy recipient, if possible.

See `kom-confirm-add-recipients'.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-add-recipient)))
  (lyskom-add-helper text-no 
                     'who-to-add-q
                     'adding-name-as-recipient
                     'RECPT))

(def-kom-command kom-add-copy (text-no)
  "Add a carbon copy recipient to a text.
If the recipient already exists, this will convert the current recipient
type to a carbon copy recipient, if possible.

See `kom-confirm-add-recipients'.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-add-copy)))
  (lyskom-add-helper text-no 
                     'who-to-add-copy-q
                     'adding-name-as-copy
                     'CC-RECPT))

(def-kom-command kom-add-bcc (text-no)
  "Add a recipient to a text.
If the recipient already exists, this will convert the current recipient
type to a BCC recipient, if possible.

See `kom-confirm-add-recipients'.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-add-bcc)))
  (lyskom-add-helper text-no 
                     'who-to-add-bcc-q
                     'adding-name-as-copy
                     'BCC-RECPT))

(defun lyskom-add-helper (text-no who-prompt doing-prompt type)
  (let* ((text-stat (blocking-do 'get-text-stat text-no))
         (attachments (lyskom-attachments-for-sub text-no))
         (footnotes (lyskom-footnotes-for-sub text-no))
         (move-footnotes nil)
         (move-attachments nil)
         (target (lyskom-read-conf-stat who-prompt '(all) nil nil t)))

    (when (and target text-no)

      (when (and (eq type 'RECPT)
                 kom-confirm-add-recipients
                 (not (lyskom-j-or-n-p
                       (lyskom-format 'really-add-as-recpt-q target))))
        (setq type 'CC-RECPT doing-prompt 'adding-name-as-copy))

      (setq move-footnotes (and footnotes (lyskom-j-or-n-p 'add-footnotes-too-q)))
      (setq move-attachments (and attachments (lyskom-j-or-n-p 'add-attachments-too-q)))
      (lyskom-traverse text (append (list (cons text-no text-stat))
                                    (and move-footnotes footnotes)
                                    (and move-attachments attachments))
        (lyskom-format-insert doing-prompt target (car text))
        (lyskom-move-recipient (car text) nil target type)))))

(defun lyskom-footnotes-for-sub (text-no)
  "Return the list of footnotes to remove recipient from."
  (let ((result nil)
        (worklist (list text-no)))
    (while worklist
      (setq text-no (car worklist))
      (setq worklist (cdr worklist))
      (unless (assq text-no result)
        (let ((text-stat (blocking-do 'get-text-stat text-no)))
          (setq result (cons (cons text-no text-stat) result)
                worklist (nconc (mapcar 'misc-info->footn-in
                                        (lyskom-misc-infos-from-list
                                         'FOOTN-IN (text-stat->misc-info-list
                                                    text-stat)))
                                worklist)))))
    (cdr (nreverse result))))

(defun lyskom-attachments-for-sub (text-no)
  "Return the list of attachments to remove recipient from."
  (let ((result nil)
        (worklist (list text-no)))
    (while worklist
      (setq text-no (car worklist))
      (setq worklist (cdr worklist))
      (unless (assq text-no result)
        (let ((text-stat (blocking-do 'get-text-stat text-no)))
          (setq result (cons (cons text-no text-stat) result)
                worklist (nconc (lyskom-get-text-attachments text-stat)
                                worklist)))))
    (cdr (nreverse result))))



(def-kom-command kom-sub-recipient (text-no)
  "Remove a recipient from the selected text. Take care not to remove
the last recipient, since this will make the text unreadable.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-delete-recipient)))
  (let* ((text-stat (blocking-do 'get-text-stat text-no))
         (attachments (lyskom-attachments-for-sub text-no))
         (footnotes (lyskom-footnotes-for-sub text-no))
         (move-footnotes nil)
         (move-attachments nil))
    (if text-stat
        (let ((recipients
               (sort (lyskom-text-recipients text-stat t)
                     (lambda (a b)
                       (> (length (memq (cdr a) lyskom-recpt-types-list))
                          (length (memq (cdr b) lyskom-recpt-types-list)))))))
          (if recipients
              (let* ((source (lyskom-read-conf-stat
                              'who-to-sub-q
                              (list (cons 'restrict (mapcar 'car recipients)))
                              nil
                              nil
                              t)))
                (when source
                  (setq move-footnotes (and footnotes (lyskom-j-or-n-p 'sub-footnotes-too-q)))
                  (setq move-attachments (and attachments (lyskom-j-or-n-p 'sub-attachments-too-q)))

                  (lyskom-traverse text (append (list (cons text-no text-stat))
                                                (and move-footnotes footnotes)
                                                (and move-attachments attachments))
                    (lyskom-format-insert 'remove-name-as-recipient source (cdr text))
                    (if (memq (conf-stat->conf-no source) (lyskom-text-recipients (cdr text)))
                        (lyskom-move-recipient (car text) source nil nil)
                      (lyskom-insert 'move-text-not-recipient)))))
            (lyskom-format-insert 'text-has-no-recipients-r text-no)))
      (lyskom-format-insert 'no-such-text-no text-no))))


(def-kom-command kom-move-text (text-no)
  "Move the selected text from one conference to another.
If you want to move an entire comment tree, use `kom-move-text-tree' 
instead.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg').

Lisp documentation:
CONTINUATION is the function that does the actual moving. It is called
with three arguments: source, target and text-stat, where source is the
recipient to remove and target the recipient to add to text-stat."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-move)))
  (let* ((text-stat (blocking-do 'get-text-stat text-no))
         (attachments (lyskom-attachments-for-sub text-no))
         (footnotes (lyskom-footnotes-for-sub text-no))
         (move-footnotes nil)
         (move-attachments nil))
    (if (null text-stat)
        (lyskom-format-insert 'no-such-text-no text-no)
      (let* ((recipients (sort (lyskom-text-recipients text-stat t)
                               (lambda (a b)
                                 (> (length (memq (cdr a) lyskom-recpt-types-list))
                                    (length (memq (cdr b) lyskom-recpt-types-list)))))))
        (if (null recipients)
            (lyskom-format-insert 'text-has-no-recipients-r text-no)
          (let ((source (lyskom-read-conf-stat 
                         'who-to-move-from-q
                         (list (cons 'restrict (mapcar 'car recipients)))
                         nil nil t))
                (target (lyskom-read-conf-stat 'who-to-move-to-q '(all)
                                               nil nil t)))
            (when (and source target)
              (setq move-footnotes (and footnotes (lyskom-j-or-n-p 'move-footnotes-too-q)))
              (setq move-attachments (and attachments (lyskom-j-or-n-p 'move-attachments-too-q)))

              (lyskom-traverse text (append (list (cons text-no text-stat))
                                            (and move-footnotes footnotes)
                                            (and move-attachments attachments))
                (lyskom-format-insert 'moving-name source target (cdr text))
                (if (memq (conf-stat->conf-no source) (lyskom-text-recipients (cdr text)))
                    (lyskom-move-recipient (car text) source target 'RECPT)
                  (lyskom-insert 'move-text-not-recipient))
                ))))))))


(def-kom-command kom-move-text-tree (text-no)
  "Move the selected text and all comments below it from one
conference to another. This command can also be used to remove or add
a recipient. Confirmation is required for each single move.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-tree-to-move)))
  (let ((root-text-stat (blocking-do 'get-text-stat text-no)))
    (if root-text-stat

        ;; Set up default recipients for from and to.

        (let* ((recipients (sort (lyskom-text-recipients root-text-stat t)
                               (lambda (a b)
                                 (> (length (memq (cdr a) lyskom-recpt-types-list))
                                    (length (memq (cdr b) lyskom-recpt-types-list)))))))

          ;; Check that the text has recipients at all.

          (if (null recipients)
              (lyskom-format-insert 'text-has-no-recipients-r text-no)

            ;; Read the conference we are moving the tree away from.

            (let ((source (lyskom-read-conf-stat 
                           'who-to-move-from-q
                           (list (cons 'restrict (mapcar 'car recipients)))
                           nil nil t))
                  (to-do (list (text-stat->text-no root-text-stat)))
                  (done nil))


              ;; Now loop over the tree

              (while to-do
                (let* ((text-to-move (car to-do))
                       (text-stat (blocking-do 'get-text-stat text-to-move))
                       (text-to-move-recipients
                        (lyskom-text-recipients text-stat)))
                  (setq to-do (cdr to-do))

                  ;; Check that this text is a candidate for moving
                  ;; If not, put it in done to fool the next control
                  ;; structure.

                  (unless (memq (conf-stat->conf-no source)
                                text-to-move-recipients)
                    (lyskom-format-insert 'moving-already-moved 
                                          text-to-move source)
                    (setq done (cons text-to-move done)))

                  ;; Use memoing to ensure that we don't loop.

                  (unless (memq text-to-move done)
                    (setq done (cons text-to-move done))

                    ;; Check that the text has the source as a
                    ;; recipient. If not, skip to the next in the
                    ;; tree (and don't traverse downwards).

                    ;; Show the text that we are planning on moving
                    ;; Truncate it. This may be a bad idea, but it
                    ;; kind of makes sense since we don't have the
                    ;; opportunity to scroll the buffer.

                    (let ((kom-truncate-show-lines 
                           (if (numberp kom-truncate-show-lines)
                               (min kom-truncate-show-lines 10)
                             10))
                          (kom-truncate-threshold 
                           (if (numberp kom-truncate-threshold)
                               (min kom-truncate-threshold 10)
                             10)))
                      (lyskom-view-text text-to-move nil nil nil
                                        nil nil nil t)
                      (setq lyskom-last-viewed (point-max)))


                    ;; Ask the user what to do

                    (let* ((completion-ignore-case t)
                           (action
                            (cdr
                             (lyskom-string-assoc
                              (lyskom-completing-read (lyskom-get-string 'moving-tree-what-action-q)
                                                      lyskom-move-tree-actions
                                                      nil
                                                      t
                                                      nil
                                                      nil
                                                      (car (rassoc 'move lyskom-move-tree-actions)))
                              lyskom-move-tree-actions))))
                      (cond

                       ;; Move the text.

                       ((eq action 'move)
                        (condition-case nil
                            (let ((target
                                   (lyskom-read-conf-stat
                                    (if (> (length text-to-move-recipients) 1)
                                        'who-to-move-to-or-sub-q
                                      'who-to-move-to-q)
                                    '(all)
                                    (> (length text-to-move-recipients) 1)
                                    nil t)))
                              (if target
                                  (lyskom-format-insert 'moving-name source target text-stat)
                                (lyskom-format-insert 'remove-name-as-recipient
                                                      source
                                                      (text-stat->text-no text-stat)))
                              (lyskom-move-recipient text-to-move source target 'RECPT)
                              (setq to-do (nconc (lyskom-text-comments text-stat) to-do))
                              )
                          (quit (setq to-do (cons text-to-move to-do))))
                        )


                       ((eq action 'skip)
                        (setq to-do (nconc (lyskom-text-comments text-stat) to-do))
                        )

                       ((eq action 'quit)
                        (setq to-do nil))

                       ((eq action 'jump))))))))))
      (lyskom-format-insert 'no-such-text-no text-no)
      )))



(defun lyskom-move-recipient (text-no source target type)
  "Remove TEXT-NO from SOURCE and add it to TARGET as TYPE.
This is the internal function for moving texts around. SOURCE or TARGET
may be nil. TYPE is ignored if TARGET is nil.

Calls lyskom-report-command-answer to report the result, to callers
must have printed something without a newline at the end of the buffer."
  (let ((text-stat (blocking-do 'get-text-stat text-no)))
    (if text-stat
        (let* ((was-read (lyskom-text-read-p text-stat))
               (add-result (if target
                               (blocking-do 'add-recipient
                                            text-no
                                            (conf-stat->conf-no target)
                                            type)
                             t))
               (add-errno lyskom-errno)
               (sub-result (if (and source add-result)
                               (blocking-do 'sub-recipient
                                            text-no
                                            (conf-stat->conf-no source)) 
                             t))
               (sub-errno lyskom-errno))

          (when (null add-result)

            ;; Can't add to target. Explain why. We have not removed from the
            ;; source conference, so no need to add it back

            (cond ((eq add-errno 27) ; already-recipient
                   (lyskom-insert-string 'nope)
                   (lyskom-format-insert 'error-already-recipient text-stat target)
                   )
                  ((eq add-errno 33) ; recipient-limit
                   (lyskom-insert-string 'nope)
                   (lyskom-format-insert 'error-recipient-limit text-stat)
                   )
                  ((eq add-errno 12) ; permission-denied
                   (lyskom-insert-string 'nope)
                   (lyskom-format-insert 'error-permission-denied-add-recpt text-stat target)
                   )
                  ((eq add-errno 11) ; access-denied
                   (lyskom-insert-string 'nope)
                   (lyskom-format-insert 'error-access-denied-add-recpt text-stat target)
                   )
                  (t (lyskom-report-command-answer nil add-errno))) 
            )

          (when (null sub-result)
            ;; Can't sub from souce. Explain why.
            (when target
              (blocking-do 'sub-recipient text-no (conf-stat->conf-no source)))
            (cond ((eq sub-errno 30) ;not-recipient
                   (lyskom-insert-string 'nope)
                   (lyskom-format-insert 'error-not-recipient text-stat source)
                   )
                  ((eq sub-errno 12) ; permission-denied
                   (lyskom-insert-string 'nope)
                   (lyskom-format-insert 'error-permission-denied-sub-recpt
                                         text-stat
                                         source)
                   )
                  (t (lyskom-report-command-answer nil sub-errno)))
            )

          (when (and add-result sub-result)
            (lyskom-report-command-answer t))

          ;; If the text was read prior to the move, it is afterwards too.
          (cache-del-text-stat text-no)
          (when (and add-result target was-read)
            (lyskom-mark-as-read (blocking-do 'get-text-stat text-no)))


          (and add-result sub-result))
      (lyskom-format-insert 'no-such-text-no text-no)
      nil)))




;;; ================================================================
;;;                 Addera kommentar - Add comment
;;;             Subtrahera kommentar - Subtract comment

;;; Author: David Byers
;;; Heavily based on code by Lars Willf|r

(def-kom-command kom-add-comment (text-no-arg)
  "Add one text as a comment to another text.
This command is used to connect a text to another one after both have
been created. If you want to write a new comment, use `kom-write-comment'
instead.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-add-comment-to)))
  (lyskom-add-sub-comment text-no-arg
			  t))

(def-kom-command kom-sub-comment (text-no-arg)
  "Remove a comment from a text.
Be careful removing comments. If others have written comments to the
comment being removed, the context of those comments may become
hard to understand.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-delete-comment-from)))
  (if (lyskom-misc-infos-from-list 
       'COMM-IN (text-stat->misc-info-list
                 (blocking-do 'get-text-stat text-no-arg)))
      (lyskom-add-sub-comment text-no-arg nil)
    (lyskom-format-insert 'text-has-no-comments text-no-arg)))

(defun lyskom-add-sub-comment (text-no do-add)
  "Get the number of the text that is going to have a comment added to it or
subtracted from it
Arguments: TEXT-NO-ARG: an argument as it is gotten from (interactive P)
DO-ADD: NIL if a comment should be subtracted.
        Otherwise a comment is added"
  (if text-no
    (let* ((completions (unless do-add
                          (let ((text-stat (blocking-do 'get-text-stat text-no)))
                            (when text-stat
                              (mapcar 'misc-info->comm-in
                                      (lyskom-misc-infos-from-list 'COMM-IN (text-stat->misc-info-list text-stat)))))))
           (comment-text-no (lyskom-read-number
                             (lyskom-format (if do-add 'text-to-add-q 'text-to-remove-q) text-no)
                             (cond (do-add
                                    (if (eq text-no lyskom-current-text)
                                        nil
                                      lyskom-current-text))
                                   ((eq (length completions) 1)
                                    (car completions))
                                   ((memq lyskom-current-text completions)
                                    lyskom-current-text)
                                   (t nil))
                             nil
                             nil
                             completions
                             )))
      (lyskom-format-insert (if do-add 'add-comment-to 'sub-comment-to)
                            comment-text-no
                            text-no)
      (cache-del-text-stat text-no)
      (cache-del-text-stat comment-text-no)
      (lyskom-report-command-answer 
       (blocking-do (if do-add 'add-comment 'sub-comment)
                    comment-text-no
                    text-no)))
    (lyskom-format-insert (if do-add 
                              'confusion-what-to-add-comment-to
                            'confusion-what-to-sub-comment-from))))

(def-kom-command kom-add-footnote (text-no-arg)
  "Add a text as a footnote to another text. This command is used to
add a text as a footnote to another text after both have been created.
If you want to write a new footnote, use `kom-write-footnote' instead.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-add-footnote-to)))
  (lyskom-add-sub-footnote text-no-arg
			  t))

(def-kom-command kom-sub-footnote (text-no-arg)
  "Remove a footnote from a text.

Be careful removing comments. If others have written comments to the
comment being removed, the context of those comments may become
hard to understand.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-delete-footnote-from)))
  (if (lyskom-misc-infos-from-list 
       'FOOTN-IN (text-stat->misc-info-list
                 (blocking-do 'get-text-stat text-no-arg)))
      (lyskom-add-sub-footnote text-no-arg nil)
    (lyskom-format-insert 'text-has-no-footnotes text-no-arg)))


(defun lyskom-add-sub-footnote (text-no do-add)
  "Get the number of the text that is going to have a footnote added to it or
subtracted from it
Arguments: TEXT-NO-ARG: an argument as it is gotten from (interactive P)
DO-ADD: NIL if a footnote should be subtracted.
        Otherwise a footnote is added"
  (if text-no
      (let* ((completions (unless do-add
                          (let ((text-stat (blocking-do 'get-text-stat text-no)))
                            (when text-stat
                              (mapcar 'misc-info->footn-in
                                      (lyskom-misc-infos-from-list 'FOOTN-IN (text-stat->misc-info-list text-stat)))))))
             (footnote-text-no  (lyskom-read-number
                                 (lyskom-format
                                  (if do-add 'text-to-add-footn-q 'text-to-remove-footn-q)
                                  text-no)
                                 (cond (do-add
                                        (if (eq text-no lyskom-current-text)
                                            nil
                                          lyskom-current-text))
                                       ((eq (length completions) 1)
                                        (car completions))
                                       ((memq lyskom-current-text completions)
                                        lyskom-current-text)
                                       (t nil))
                                 nil
                                 nil
                                 completions
                                 )))
        (lyskom-format-insert (if do-add 'add-footnote-to 'sub-footnote-to)
                              footnote-text-no
                              text-no)
        (cache-del-text-stat text-no)
        (cache-del-text-stat footnote-text-no)
        (lyskom-report-command-answer 
         (blocking-do (if do-add 'add-footnote 'sub-footnote)
                      footnote-text-no
                      text-no)))
    (lyskom-insert (if do-add 
                       'confusion-what-to-add-footnote-to
                     'confusion-what-to-sub-footnote-from))))

;;; ================================================================
;;;                 Addera referens - Add cross reference

;;; Author: Joel Rosdahl

(def-kom-command kom-add-cross-reference (text-no-arg)
  "Add a cross reference to a text.
The reference can be to another text, a conference or a person.

This command accepts text number prefix arguments (see
`lyskom-read-text-no-prefix-arg')."
  (interactive "P")
  (lyskom-add-cross-reference text-no-arg
                              (lyskom-get-string
                               'text-to-add-cross-reference-to)))

(defun lyskom-add-cross-reference (text-no-arg prompt)
  "Get the number of the text that is going to have a cross reference
added to it, ask the user about cross reference type and value, and
add the cross reference.

Arguments:
  TEXT-NO-ARG: An argument as gotten from (interactive \"P\").
  PROMPT: A string that is used when prompting for a text number."
  (let ((text-no (let ((current-prefix-arg text-no-arg))
                   (lyskom-read-text-no-prefix-arg prompt
                                                   nil
                                                   lyskom-current-text)))
        (aux-item (lyskom-read-cross-reference-and-get-aux-item)))
    (when (and text-no aux-item)
      (cache-del-text-stat text-no)
      (lyskom-insert 'adding-cross-reference)
      (lyskom-report-command-answer
       (blocking-do 'modify-text-info
                    text-no
                    nil
                    (list aux-item))))))

(defun lyskom-read-cross-reference-and-get-aux-item ()
  "Query user about cross reference type and value, and return the
corresponding aux-item."
  (let* ((completions (list (cons (lyskom-get-string 'Conference) 'conf)
                            (cons (lyskom-get-string 'Person) 'pers)
                            (cons (lyskom-get-string 'Text) 'text)))
         (completion-ignore-case t)
         (type (cdr (lyskom-string-assoc
                     (lyskom-completing-read
                      (lyskom-get-string 'xref-type)
                      (lyskom-maybe-frob-completion-table
                       completions)
                      nil
                      t)
                     completions)))
         (obj nil)
         (char nil))
    (cond
     ((eq type 'text)
      (let ((prompt 'which-text-to-xref))
        (while (null obj)
          (setq obj (text-stat->text-no
                     (blocking-do 'get-text-stat
                                  (lyskom-read-number prompt))))
          (setq prompt 'which-text-to-xref-err )))
      (setq char "T"))
     ((eq type 'conf)
      (while (null obj)
        (setq obj (lyskom-read-conf-no 'which-conf-to-xref '(conf) nil nil t)))
      (setq char "C"))
     ((eq type 'pers)
      (while (null obj)
        (setq obj (lyskom-read-conf-no 'which-pers-to-xref '(pers) nil nil t)))
      (setq char "P")))

    (when obj
      (lyskom-create-aux-item 0 3 0 0
                              (lyskom-create-aux-item-flags
                               nil nil nil nil nil nil nil nil)
                              0
                              (format "%s%d" char obj)))))


(defun lyskom-read-link ()
  "Query user about link type and value, and return the corresponding
link as a string."
  (let* ((completions (list (cons (lyskom-get-string 'Conference) 'conf)
                            (cons (lyskom-get-string 'Person) 'pers)
                            (cons (lyskom-get-string 'Text) 'text)))
         (completion-ignore-case t)
         (type (cdr (lyskom-string-assoc
                     (lyskom-completing-read
                      (lyskom-get-string 'link-type)
                      (lyskom-maybe-frob-completion-table
                       completions)
                      nil
                      t)
                     completions)))
         (obj nil))
    (cond
     ((eq type 'text)
      (let ((prompt 'which-text-to-link))
        (while (null obj)
          (setq obj (blocking-do 'get-text-stat
                                 (lyskom-read-number prompt)))
          (setq prompt 'which-text-to-link-err )))
      (let* ((text-no (text-stat->text-no obj))
             (text (blocking-do 'get-text text-no))
	     (txt (text->decoded-text-mass text obj))
	     (eos (string-match (regexp-quote "\n") txt))
	     (subject (substring txt 0 eos)))
	(format "<text %d: %s>" text-no subject)))
     
     ((eq type 'conf)
      (while (null obj)
        (setq obj (lyskom-read-conf-stat 'which-conf-to-link '(conf) nil nil t)))
      (format "<möte %d: %s>" (conf-stat->conf-no obj)
			   (conf-stat->name obj)))

     ((eq type 'pers)
      (while (null obj)
        (setq obj (lyskom-read-conf-stat 'which-pers-to-link '(pers) nil nil t)))
      (format "<person %d: %s>" (conf-stat->conf-no obj)
			   (conf-stat->name obj))))))


;;; ================================================================
;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; end: 
