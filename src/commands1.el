;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: commands1.el,v 44.119 2001-08-16 18:28:05 teddy Exp $
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
;;;; File: commands1.el
;;;;
;;;; This file contains the code for some of the high level commands.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: commands1.el,v 44.119 2001-08-16 18:28:05 teddy Exp $\n"))

(eval-when-compile
  (require 'lyskom-command "command"))


;;; ================================================================
;;;                  F} uppmuntran - Get appreciation 

;;; Author: Inge Wallin


(def-kom-command kom-get-appreciation ()
  "Give the user a little light in the dark"
  (interactive)
  (lyskom-insert-string 'appreciation))
  

;;; ================================================================
;;;                      F} Sk{ll - Get abuse

;;; Author: Inge Wallin


(def-kom-command kom-get-abuse ()
  "Give the user a little verbal abuse."
  (interactive)
  (lyskom-insert-string 'abuse))


;;; ================================================================
;;;            Utpl}na - Delete a person or a conference

;;; Author: Inge Wallin


(def-kom-command kom-delete-conf ()
  "Delete a person or a conference."
  (interactive)
  (let ((conf-stat 
	 (lyskom-read-conf-stat (lyskom-get-string 'what-conf-to-delete)
				'(all) nil nil t)))
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
  "Delete a text. Argument: TEXT-NO"
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-text-to-delete)))
  (if text-no
      (let* ((do-delete t)
             (text-stat (blocking-do 'get-text-stat text-no))
             (num-marks (text-stat->no-of-marks text-stat))
             (is-marked-by-me (cache-text-is-marked text-no)))
        (cond ((null text-stat) 
               (lyskom-report-command-answer nil)
               (setq do-delete nil))

              ((> (text-stat->no-of-marks text-stat) 0)
               (setq do-delete
                     (lyskom-j-or-n-p 
                      (lyskom-format 'delete-marked-text
                                     (if (> num-marks 0)
                                         (if is-marked-by-me
                                             (if (= num-marks 1)
                                                 (lyskom-get-string 'delete-marked-by-you)
                                               (lyskom-format 'delete-marked-by-you-and-others
                                                              (1- num-marks)))
                                           (lyskom-format 'delete-marked-by-several
                                                          num-marks))))))))
        (when do-delete
          (lyskom-format-insert 'deleting-text text-no)
          (when (lyskom-report-command-answer 
                 (blocking-do 'delete-text text-no))
            (when is-marked-by-me
              (lyskom-unmark-text text-no)))))
    (lyskom-insert 'confusion-what-to-delete)))



;;; ================================================================
;;;        ]terse presentation - Review the presentation
;;;               for a person or a conference

;;; Author: Inge Wallin


(def-kom-command kom-review-presentation (&optional who)
  "Review the presentation for a person or a conference."
  (interactive)
  (let ((conf-stat 
         (if who
             (blocking-do 'get-conf-stat who)
           (lyskom-read-conf-stat 
            (lyskom-get-string 'presentation-for-whom)
            '(all)
            nil "" t))))
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
        (lyskom-print-comment-like-aux faq conf-stat)))))

(defun lyskom-print-comment-like-aux (item object)
  (let* ((text-no (string-to-int (aux-item->data item)))
         (text-stat (if kom-deferred-printing
                        (cache-get-text-stat text-no)
                      (blocking-do 'get-text-stat text-no))))
    (cond ((or text-stat (not kom-deferred-printing))
           (lyskom-insert-comment-like-aux item text-no text-stat object))
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

(defun lyskom-insert-comment-like-aux (item text-no text-stat object)
  (let* ((author (if text-stat (text-stat->author text-stat) nil))
         (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat)
                                            17)))
         (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat)
                                              16)))
         (formats (lyskom-aux-item-definition-field item 'text-header-line))
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
                                                              object))
    (lyskom-insert "\n")))

;;; FIXME: This contains code that duplicates the code in 
;;; lyskom-insert-deferred-header-comm. That's a BAD THING.

(defun lyskom-insert-deferred-comment-like-aux (text-stat defer-info)
  (let* ((author (if text-stat (text-stat->author text-stat) nil))
         (item (elt (defer-info->data defer-info) 0))
         (object (elt (defer-info->data defer-info) 1))
         (text-no (elt (defer-info->data defer-info) 2))
         (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat)
                                            17)))
         (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat)
                                              16)))
         (formats (lyskom-aux-item-definition-field item 'text-header-line))
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
                                                                 object))))






;;; ================================================================
;;;          Återse det kommenterade - View commented text

;;; Author: Inge Wallin
;;; Modified by: David Kågedal, Johan Sundström

(def-kom-command kom-view-commented-text (text-no)
  "View the commented text.
If the current text is comment to (footnote to) several text then the first
text is shown and a REVIEW list is built to shown the other ones. If the
optional arg TEXT-NO is present review the text that text commented instead."
  (interactive (list (lyskom-read-text-no-prefix-arg 'review-commented-q)))
  (if text-no
      (progn
	(lyskom-tell-internat 'kom-tell-read)
        (unless kom-review-uses-cache
          (cache-del-text-stat text-no))
	(lyskom-view-commented-text
	 (blocking-do 'get-text-stat text-no)))
    (lyskom-insert-string 'confusion-what-to-view)))


(def-kom-command kom-view-previous-commented-text (text-no)
  "View the text the previous text commented.
If the previously viewed text is a comment to (footnote to) several
texts then the first text is shown and a REVIEW list is built to show
the other ones."
  (interactive (list (lyskom-read-text-no-prefix-arg 'review-commented-q nil
                                                     lyskom-previous-text)))
  (cond (text-no
         (lyskom-tell-internat 'kom-tell-read)
        (unless kom-review-uses-cache
          (cache-del-text-stat text-no))
         (lyskom-view-commented-text
          (blocking-do 'get-text-stat lyskom-previous-text)))
        (t (lyskom-insert-string 'confusion-what-to-view))))

(defun lyskom-view-commented-text (text-stat)
  "Handles the return from the initiate-get-text-stat, displays and builds list."
  (let* ((misc-info-list (and text-stat
                              (text-stat->misc-info-list text-stat)))
         (misc-infos (and misc-info-list
                          (append (lyskom-misc-infos-from-list
                                   'COMM-TO misc-info-list)
                                  (lyskom-misc-infos-from-list
                                   'FOOTN-TO misc-info-list))))
         (text-nos (and misc-infos
                        (mapcar
                         (function
                          (lambda (misc-info)
                            (if (equal (misc-info->type misc-info)
                                       'COMM-TO)
                                (misc-info->comm-to misc-info)
                              (misc-info->footn-to misc-info))))
                         misc-infos))))
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


(defun lyskom-misc-infos-from-list (type list)
  "Get all the misc-infos from the misc-info-list LIST with the same type
as TYPE. If no such misc-info, return NIL"
  (cond ((null list) nil)
	((equal type (misc-info->type (car list))) 
	 (cons (car list)
	       (lyskom-misc-infos-from-list type (cdr list))))
	(t (lyskom-misc-infos-from-list type (cdr list)))))


;;; ================================================================
;;;                         Brev - Send letter

;;; Author: Inge Wallin
;;; Rewritten using read-conf-no by Linus Tolke (4=>1)

(def-kom-command kom-send-letter (&optional pers-no)
  "Send a personal letter to a person or a conference."
  (interactive)
  (condition-case nil
      (progn
        (lyskom-tell-internat 'kom-tell-write-letter)
	;; If there was a motd, which is now removed we have to
	;; refetch the conf-stat to know that.
        (let* ((tono (or pers-no
                         (lyskom-read-conf-no
                          (lyskom-get-string 'who-letter-to)
                          '(all) nil nil t)))
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
                                    (lyskom-create-misc-list 'recpt tono)
                                    "" "")
                (lyskom-edit-text lyskom-proc
                                  (if (lyskom-get-membership tono)
                                      (lyskom-create-misc-list 'recpt tono)
                                      (lyskom-create-misc-list 
                                       'recpt tono
                                       'recpt lyskom-pers-no))
                                  "" "")))))
    (quit (signal 'quit nil))))


;;; ================================================================
;;;           Bli medlem i m|te - Become a member of a conference
;;;             Addera medlem - Add somebody else as a member

;;; Author: ???
;;; Rewritten by: David K}gedal


;; Add another person

(def-kom-command kom-add-member ()
  "Add a person as a member of a conference.
Ask for the name of the person, the conference to add him/her to."
  (interactive)
  (let* ((who (lyskom-read-conf-stat (lyskom-get-string 'who-to-add)
				     '(pers) nil nil t))
	 (whereto (lyskom-read-conf-stat (lyskom-get-string 'where-to-add)
					 '(all) nil nil t))
	 (pers-stat (blocking-do 'get-pers-stat (conf-stat->conf-no who))))
    (lyskom-add-member-answer (lyskom-try-add-member whereto who 
                                                     pers-stat nil nil t)
			      whereto who)))



;; Add self

(def-kom-command kom-add-self (&optional conf)
  "Add this person as a member of a conference."
  (interactive)
  (let* ((whereto (if conf (blocking-do 'get-conf-stat conf)
                    (lyskom-read-conf-stat 
                     (lyskom-get-string 'where-to-add-self)
                     '(all) nil "" t)))
         (who (blocking-do 'get-conf-stat lyskom-pers-no))
         (pers-stat (blocking-do 'get-pers-stat lyskom-pers-no))
         (mship (lyskom-get-membership (conf-stat->conf-no whereto) t)))
  
    ;; Fake kom-membership-default-priority if this is a passive membership
    ;; This will suppress the normal "which priority" question. Ugly hack.

    (let ((kom-membership-default-priority
           (if (and mship (membership-type->passive (membership->type mship)))
               (membership->priority mship)
             kom-membership-default-priority)))
      (lyskom-add-member-answer (lyskom-try-add-member whereto who pers-stat nil nil t)
                                whereto who))))


(def-kom-command kom-change-priority (&optional conf)
  "Change the priority of a conference."
  (interactive)
  (let* ((conf-stat (if conf (blocking-do 'get-conf-stat conf)
                      (lyskom-read-conf-stat 
                       (lyskom-get-string 'change-priority-for-q)
                       '(all) nil "" t)))
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

(defun lyskom-add-member-by-no (conf-no pers-no &optional thendo &rest data)
  "Fetch info to be able to add a person to a conf.
Get the conf-stat CONF-NO for the conference and the conf-stat and pers-stat 
for person PERS-NO and send them into lyskom-try-add-member."
  (blocking-do-multiple ((whereto (get-conf-stat conf-no))
                         (who (get-conf-stat pers-no))
                         (pers-stat (get-pers-stat pers-no)))
    (let ((result (lyskom-try-add-member whereto who pers-stat nil nil t)))
      (lyskom-add-member-answer result whereto who)
      (if thendo
          (apply thendo data))
      (car result))))


(defun lyskom-try-add-member (conf-conf-stat 
                              pers-conf-stat 
                              pers-stat
                              membership-type
                              &optional message-string
                              need-extra-information)
  "Add a member to a conference.
Args: CONF-CONF-STAT PERS-CONF-STAT PERS-STAT
CONF-CONF-STAT: the conf-stat of the conference the person is being added to
PERS-CONF-STAT: the conf-stat of the person being added.
PERS-STAT: the pers-stat of the person being added.

Optional MESSAGE-STRING is the message to print before making server call.
Returns t if it was possible, otherwise nil.

If optional NEED-EXTRA-INFORMATION is non-nil, the return value will be
a list where the first element is the result of add-member and the second
is the position where the membership was placed."
  (if (or (null conf-conf-stat)
	  (null pers-conf-stat))
      nil				; We have some problem here.
    (let ((priority
	   (if (/= lyskom-pers-no (conf-stat->conf-no pers-conf-stat))
	       (lyskom-read-num-range 0 255
                                      (lyskom-get-string 'priority-q)
                                      nil 100)
	     (if (and (numberp kom-membership-default-priority)
		      (< kom-membership-default-priority 256)
		      (>= kom-membership-default-priority 0))
		 kom-membership-default-priority
	       (lyskom-read-num-range 0 255 (lyskom-get-string 'priority-q)))))
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
                                  conf-conf-stat)
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


(defun lyskom-add-member-answer (answer conf-conf-stat
                                        pers-conf-stat)
  "Handle the result from an attempt to add a member to a conference."
  (let ((pos (if (consp answer) (elt answer 1) nil))
        (answer (if (consp answer) (elt answer 0) answer)))
    (if (null answer)
        (progn
          (lyskom-insert-string 'nope)
          (let* ((errno lyskom-errno)
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
                   (lyskom-format-insert 'error-code (lyskom-get-error-text errno)))

                  (rd-prot (let ((supervisorconf (blocking-do
                                                  'get-conf-stat
                                                  (conf-stat->supervisor conf-conf-stat))))
                             (if supervisorconf
                                 (lyskom-format-insert 'is-read-protected-contact-supervisor
                                                       conf-conf-stat
                                                       supervisorconf)
                               (lyskom-format-insert 'cant-find-supervisor
                                                     conf-conf-stat))))

                  (t (lyskom-format-insert 'error-code
                                           (lyskom-get-error-text lyskom-errno)
                                           lyskom-errno)))))

      ;;+++Borde {ndra i cachen i st{llet.
      (cache-del-pers-stat (conf-stat->conf-no pers-conf-stat))
      ;;+++Borde {ndra i cachen i st{llet.
      (cache-del-conf-stat (conf-stat->conf-no conf-conf-stat))
      (if (= (conf-stat->conf-no pers-conf-stat)
             lyskom-pers-no)
          (let ((mship (blocking-do 'query-read-texts
                                    lyskom-pers-no 
                                    (conf-stat->conf-no conf-conf-stat))))
            (unless (membership->position mship)
              (set-membership->position mship pos))
            (lyskom-add-membership mship
                                   conf-conf-stat
                                   t)))
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
  "Subtract a person as a member from a conference. Ask for the name
of the person."
  (interactive)
  (lyskom-sub-member
   (lyskom-read-conf-stat (lyskom-get-string 'who-to-exclude)
                          '(pers) nil "" t)
   (lyskom-read-conf-stat (lyskom-get-string 'where-from-exclude) 
			  '(all) nil "" t)))


(def-kom-command kom-sub-self (&optional conf)
  "Subtract this person as a member from a conference."
  (interactive)
  (lyskom-sub-member 
   (blocking-do 'get-conf-stat lyskom-pers-no)
   (if conf (blocking-do 'get-conf-stat conf)
     (lyskom-read-conf-stat (lyskom-get-string 'leave-what-conf)
                            '(all) nil 
                            (let ((ccn 
                                   (if (or (null lyskom-current-conf)
                                           (zerop lyskom-current-conf))
                                       ""
                                     (conf-stat->name
                                      (blocking-do 'get-conf-stat
                                                   lyskom-current-conf)))))
                              (if ccn
                                  (cons ccn 0)
                                "")) t))))

(defun lyskom-sub-member (pers conf)
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
           (lyskom-format-insert 'unsubscribe-to conf)
           (set-membership-type->passive (membership->type mship) t)
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
					 lyskom-to-do-list)))
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
  "Create a conference."
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
	  (lyskom-format-insert 'error-code
				(lyskom-get-error-text lyskom-errno)
				lyskom-errno))
      (progn
	(let ((conf-stat (blocking-do 'get-conf-stat conf-no)))
	  (lyskom-format-insert 'created-conf-no-name 
				(or conf-stat conf-no)
				(or conf-stat conf-name)
				(if conf-stat
				    (lyskom-default-button 'conf conf-stat)
				  nil)))
	(lyskom-scroll)
	(lyskom-add-member-by-no conf-no lyskom-pers-no
				 (if secret
				     nil ; Don't write a presentation
				   'lyskom-create-conf-handler-2)
				 conf-no conf-name)))))


(defun lyskom-create-conf-handler-2 (conf-no conf-name)
  "Starts editing a presentation for the newly created conference.
This does lyskom-end-of-command"
  (lyskom-tell-internat 'kom-tell-conf-pres)
  (let ((conf (blocking-do 'get-conf-stat conf-no)))
    (if conf
        (lyskom-dispatch-edit-text lyskom-proc
                                   (lyskom-create-misc-list
                                    'recpt
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
  "Write a comment to a text.
If optional arg TEXT-NO is present write a comment to that text instead."
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
                                         'comment))))
        (lyskom-insert-string 'confusion-who-to-reply-to))
    (lyskom-end-of-command)))


(def-kom-command kom-write-footnote (text-no)
  "Write a footnote to a text.
If optional arg TEXT-NO is present write a footnote to that text instead."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-footnote-no nil
                                                     'last-seen-written)))
  (if text-no
      (lyskom-write-comment-soon
       (blocking-do 'get-text-stat text-no)
       (blocking-do 'get-text text-no)
       text-no 'footnote)
    (lyskom-insert-string 'confusion-what-to-footnote)))


(def-kom-command kom-comment-previous (text-no)
  "Write a comment to previously viewed text."
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
			 ((eq type 'comment) 'comm-to)
			 ((eq type 'footnote) 'footn-to)
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
                                      ((memq confno ccrep) 'cc-recpt)
                                      ((memq confno bccrep) 'bcc-recpt)
                                      (t 'recpt))
                                     commno))))
		(if (lyskom-get-membership commno)
		    (setq member t))
		(setq recpts (cons commno recpts))))
	    (setq data (cdr data)))
	  ;; Add the user to the list of recipients if he isn't a member in
	  ;; any of the recipients.
	  (if (not member)
	      (setq recver (append recver
				   (list (cons 'recpt lyskom-pers-no)))))
	  (lyskom-edit-text lyskom-proc 
			    recver
			    subject "")))

    (quit (signal 'quit nil))))


;;; ================================================================
;;;                Personligt svar - personal answer

;;; Author: ???
;;; Rewritten using blocking-do by: Linus Tolke


(def-kom-command kom-private-answer (text-no)
  "Write a private answer to the current text.
If optional arg TEXT-NO is present write a private answer to
that text instead."
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
  (if (null text-stat)
      (progn
	(lyskom-insert-string 'confusion-what-to-answer-to))
    (progn
      (lyskom-tell-internat 'kom-tell-write-reply)
      (lyskom-edit-text lyskom-proc
			(lyskom-create-misc-list
			 'comm-to (text-stat->text-no text-stat)
			 'recpt (text-stat->author text-stat)
			 'recpt lyskom-pers-no)
			subject ""))))


;;; ================================================================
;;;    Personligt svar p} f|reg}ende - kom-private-answer-previous

;;; Author: ceder
;;; Rewritten using blocking-do by: Linus Tolke

(def-kom-command kom-private-answer-previous (text-no)
  "Write a private answer to previously viewed text."
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


(defun lyskom-private-answer-soon-prev (text-stat text)
  "Write a private answer to TEXT-STAT, TEXT."
  (let ((str (text->decoded-text-mass text text-stat)))
  (if (string-match "\n" str)
      (lyskom-private-answer text-stat
			     (substring str 0 (match-beginning 0)))
    (lyskom-private-answer text-stat ""))))


;;; ================================================================
;;;                         Sluta - quit

;;; Author: ???


(defun kom-quit (&optional arg)
  "Quit session. Kill process and buffer-local variables.
If optional argument is non-nil then dont ask for confirmation."
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
    (run-hooks 'kom-quit-hook))
  

;;; ================================================================
;;;     	[ndra presentation - Change presentation 
;;;	         S{tta lapp p} d|rren - Change conf-motd

;;; Author: Inge Wallin
;;; Changed by Linus Tolke


(def-kom-command kom-change-presentation ()
  "Change presentation for a person or a conference."
  (interactive)
  (lyskom-change-pres-or-motd-2
   (let ((no (lyskom-read-conf-no 
	      (lyskom-get-string 'what-to-change-pres-you)
	      '(all) t nil t)))
     (if (zerop no)
	 (setq no lyskom-pers-no))
     (blocking-do 'get-conf-stat no))
   'pres))

(def-kom-command kom-change-conf-faq ()
  "Change a FAQ for a conference."
  (interactive)
  (let* ((conf-no (lyskom-read-conf-no
                   (lyskom-get-string 'what-to-change-faq-you)
                   '(conf) 
                   nil
                   (cons (if lyskom-current-conf
                             (let ((tmp (blocking-do 'get-uconf-stat lyskom-current-conf)))
                               (if tmp (uconf-stat->name tmp) ""))
                           "") 0)
                   t))
         (conf-stat (when conf-no       ; Need this to make sure the conf-stat is up-to-date!
                      (cache-del-conf-stat conf-no)
                      (blocking-do 'get-conf-stat conf-no)))
         (faq-list (when conf-stat
                     (let ((tmp nil))
                       (lyskom-traverse-aux item 
                           (conf-stat->aux-items conf-stat)
                         (progn
                           (when (eq (aux-item->tag item) 14)
                             (setq tmp (cons (cons (aux-item->data item) (aux-item->aux-no item)) tmp)))))
                       tmp)))
         (text-no-aux (cond ((eq (length faq-list) 1) 
                             (car faq-list))
                            ((> (length faq-list) 1)
                             (lyskom-string-assoc
                              (lyskom-completing-read 
                               (lyskom-get-string 'text-to-change-as-faq)
                               (lyskom-maybe-frob-completion-table 
                                faq-list)
                               nil t)
                              faq-list)))))
    (cond ((null conf-stat)
           (lyskom-insert (lyskom-get-string 'conf-does-not-exist)))
;          ((null text-no)
;           (lyskom-format-insert 'conf-has-no-faq conf-stat))
          (t (lyskom-change-conf-faq conf-stat 
                                     (if text-no-aux (string-to-int (car text-no-aux)))
                                     (if text-no-aux (cdr text-no-aux)))))))

(defun lyskom-change-conf-faq (conf-stat text-no aux-no)
  "Interactively edit the FAQ for CONF-STAT in TEXT-NO."
  (cond ((null conf-stat)
         (lyskom-insert-string 'cant-get-conf-stat))
        ((or lyskom-is-administrator
             (lyskom-is-supervisor (conf-stat->conf-no conf-stat) lyskom-pers-no))
         (blocking-do-multiple ((text-stat (get-text-stat text-no))
                                (text-mass (get-text text-no)))
           (let* ((str (and text-mass (text->decoded-text-mass text-mass text-stat)))
                  (subject (if (and str (string-match "\n" str))
                               (substring str 0 (match-beginning 0))
                             ""
                               ))
                  (body (if (and str (string-match "\n" str))
                            (substring str (match-end 0))
                          (or str ""))))
             (lyskom-dispatch-edit-text
              lyskom-proc
              (apply 'lyskom-create-misc-list
                     (if (and text-stat text-mass)
                         (append (lyskom-get-recipients-from-misc-list
                                  (text-stat->misc-info-list text-stat))
                                 (list 'comm-to (text-stat->text-no text-stat)))
                       (list 'recpt (conf-stat->conf-no conf-stat))))
              subject
              body
              'lyskom-change-conf-faq-2
              conf-stat
              (text-stat->text-no text-stat)
              aux-no))))
        (t (lyskom-format-insert 'not-supervisor-for conf-stat))))

(defun lyskom-change-conf-faq-2 (text-no conf-stat old-text-no old-aux-no)
  (cache-del-conf-stat (conf-stat->conf-no conf-stat))
  (initiate-modify-conf-info
   'background
   (lambda (retval conf-stat old-text-no text-no)
     (if retval
         (lyskom-format-insert-before-prompt 'changed-faq-for-conf-done conf-stat old-text-no text-no)
       (lyskom-format-insert-before-prompt 'changed-faq-for-conf-failed conf-stat 
                                           old-text-no text-no
                                           (lyskom-current-error))))
   (conf-stat->conf-no conf-stat)
   (when old-aux-no (list old-aux-no))
   (list (lyskom-create-aux-item 
          0 14 0 0
          (lyskom-create-aux-item-flags nil nil nil nil
                                        nil nil nil nil)
          0
          (int-to-string text-no)))
   conf-stat
   old-text-no
   text-no))
                

(def-kom-command kom-change-conf-motd ()
  "Change motd for a person or a conference."
  (interactive)
  (lyskom-change-pres-or-motd-2
   (let ((no (lyskom-read-conf-no (lyskom-get-string 'who-to-put-motd-for)
				  '(all) t nil t)))
     (if (zerop no)
	 (setq no lyskom-pers-no))
     (blocking-do 'get-conf-stat no))
   'motd))


(defun lyskom-get-recipients-from-misc-list (misc-list)
  "Return a misc-info-list containing only the recipients."
  (let* ((info (car misc-list))
	 (type (misc-info->type info)))
    (cond ((null misc-list) '())
	  ((memq type lyskom-recpt-types-list)
	   (append (list (intern (downcase (symbol-name type)))
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
               (list 'comm-to
                     (conf-stat->presentation conf-stat)))
            (list 'recpt
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


   

;;; ================================================================
;;;           Ta bort lapp p} d|rren - delete conf-motd

;;; Author: Linus Tolke (& Inge Wallin)


(def-kom-command kom-unset-conf-motd ()
  "Removes motd for a person or a conference."
  (interactive)
  (let ((conf-stat (or (lyskom-read-conf-stat
			(lyskom-get-string 'who-to-remove-motd-for)
			'(all) t nil t)
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
  "Select a certain conference.
The user is prompted for the name of the conference.
If s/he was already reading a conference that conference will be put
back on lyskom-to-do-list."
  (interactive)
  (let ((conf (if conf-no
                  (blocking-do 'get-conf-stat conf-no)
                (lyskom-read-conf-stat
                 (lyskom-get-string 'go-to-conf-p)
                 '(all) nil "" t))))
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
	      (if (lyskom-add-member-by-no (conf-stat->conf-no conf)
					   lyskom-pers-no)
		  (lyskom-do-go-to-conf conf
					(lyskom-get-membership
					 (conf-stat->conf-no conf) t))
		(lyskom-insert-string 'nope))
	    (lyskom-insert-string 'no-ok))))))

    ;; DEBUG+++
    (lyskom-continue-prefetch)
    ))
  

;; Dead function /davidk 960217
;;(defun lyskom-fixup-and-go-to-conf (conf-no)
;;  "Prefetches and after lyskom-member-in-conf and then goes to CONF-NO."
;;  (lyskom-do-go-to-conf (blocking-do 'get-conf-stat conf-no)
;;			(lyskom-member-p conf-no)))

 
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
  (setq lyskom-current-conf (conf-stat->conf-no conf-stat))
  (lyskom-format-insert 'conf-all-read 
			conf-stat)
  (lyskom-run-hook-with-args 'lyskom-after-change-conf-hook 
                      lyskom-current-conf
                      (conf-stat->conf-no conf-stat)))



;;(def-kom-var kom-iåm-conf-no 6
;;  "*Conf-no of IÅM."
;;local)

;;(defun kom-change-to-iåm-hook (old new)
;;  (cond ((eq new kom-iåm-conf-no)
;;         (make-local-variable kom-iåm-saved-variables)
;;         (setq kom-iåm-saved-variables
;;               (list kom-check-commented-author-membership
;;                     kom-check-for-new-comments
;;                     kom-confirm-multiple-recipients))
;;         (setq kom-check-commented-author-membership nil
;;               kom-check-for-new-comments nil
;;               kom-confirm-multiple-recipients nil))
;;        (t (when kom-iåm-saved-variables
;;             (setq kom-check-commented-author-membership 
;;                   (elt kom-iåm-saved-variables 0)
;;                   kom-check-for-new-comments
;;                   (elt kom-iåm-saved-variables 1)
;;                   kom-confirm-multiple-recipients
;;                   (elt kom-iåm-saved-variables 2))))))

         

(defun lyskom-get-current-priority ()
  "Return the current priority level."
  (or (read-info->priority (read-list->first 
			    lyskom-reading-list))
      (read-info->priority (read-list->first
			    lyskom-to-do-list))
      -1))


;;; ================================================================
;;;                 Skriva inl{gg - write text

;;; Author: ???


(def-kom-command kom-write-text (&optional arg)
  "write a text."
  (interactive "P")
  (let ((recpt nil))
    (cond ((consp arg) (setq recpt
                         (lyskom-read-conf-no
                          (lyskom-get-string 'who-send-text-to)
                          '(all) nil nil t)))
          ((numberp arg) (setq recpt arg))
          (t (setq recpt lyskom-current-conf)))
    (if (or (null recpt)
            (zerop recpt))
	(lyskom-insert-string 'no-in-conf)
      (lyskom-tell-internat 'kom-tell-write-text)
      (lyskom-edit-text lyskom-proc
                        (lyskom-create-misc-list 'recpt
                                                 recpt)
                        "" ""))))


;;; ================================================================
;;;                 Lista Personer - List persons

;;; Author: ceder
;;; Rewritten: linus

(def-kom-command kom-list-persons (match)
  "List all conferences whose name matches MATCH (a string).
Those that you are not a member in will be marked with an asterisk."
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
  (lyskom-format-insert "%[%#1@%4#2:p %#2P%]\n"
			(lyskom-default-button 'pers (conf-z-info->conf-no conf-z))
			conf-z))



;;; ================================================================
;;;              Lista M|ten - List conferences

;;; Author: ceder
;;; Rewritten: linus

(def-kom-command kom-list-conferences (&optional match)
  "List all conferences whose name matches MATCH (a string).
Those that you are not a member in will be marked with an asterisk."
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
            (lyskom-traverse info (conf-z-info-list->conf-z-infos result)
              (lyskom-list-conf-print info))
          (lyskom-format-insert 'no-matching-confs match))
      (lyskom-insert (lyskom-current-error)))))


(def-kom-command kom-list-created-conferences (arg)
  "List all conferences created by some person."
  (interactive "P")

  (unless kom-allow-incompleteness
    (sit-for 0)
    (lyskom-prefetch-all-confs))

  (blocking-do 'get-uconf-stat lyskom-pers-no)
  (let ((pers (lyskom-read-conf-stat
                  (if arg 'list-pers-confs-created-by 'list-confs-created-by)
                  '(all) 
                  nil
                  (if (cache-get-uconf-stat lyskom-pers-no)
                      (cons (conf-stat->name (cache-get-uconf-stat lyskom-pers-no)) 0)1
                      nil)
                  t)))
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
      (lyskom-format-insert "%[%#1@%4#2:m %#3c %4#4s %#5s %#2M%]\n"
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
                                  (t " ")))
      (sit-for 0))))


(defun lyskom-list-conf-print (conf-z)
  "Print a line of info about CONF-NO.
If you are not member in the conference it will be flagged with an asterisk."
  (lyskom-format-insert "%[%#1@%4#2:m %#3c %#2M%]\n"
			(lyskom-default-button 'conf (conf-z-info->conf-no conf-z))
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


(def-kom-command kom-list-re (regexp &optional case-insensitive)
  "List all persons and conferences whose name matches REGEXP.
If the optional argument CASE-INSENSITIVE is true, the regexp will be
converted so that the search is case insensitive."
  (interactive (list (lyskom-read-string
		      (lyskom-get-string 'search-re))
		     current-prefix-arg))
  (if case-insensitive
      (setq regexp (lyskom-make-re-case-insensitive regexp)))
  (lyskom-format-insert 'matching-regexp regexp)
  (let ((conf-list (blocking-do 're-z-lookup regexp 1 1)))
    (if conf-list
        (if (conf-z-info-list->conf-z-infos conf-list)
            (lyskom-traverse czi (conf-z-info-list->conf-z-infos conf-list)
              (lyskom-format-insert
               "%[%#1@%4#2:m %#3c %#2:M%]\n"
               (lyskom-default-button
                'conf (conf-z-info->conf-no czi))
               czi
               (if (conf-type->letterbox 
                    (conf-z-info->conf-type czi))
                   ?P ?M)
               ))
          (lyskom-format-insert 'no-matching-anys regexp))
      (lyskom-format-insert (lyskom-current-error)))))


;;; ================================================================
;;;                 [ndra namn - Change name

;;; Author: Inge Wallin
;;; Changed by: Peter Eriksson(?)
;;; Changed again: Inge Wallin
;;; Rewritten: linus


(def-kom-command kom-change-name ()
  "Change the name of a person or conference."
  (interactive)
  (let ((conf-stat (lyskom-read-conf-stat 
		    (lyskom-get-string 'name-to-be-changed)
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
  "Change the name of a person or conference."
  (interactive)
  (let ((conf-stat (lyskom-read-conf-stat 
		    (lyskom-get-string 'name-to-be-changed)
		    '(all)
                    nil
                    (cons
                     (conf-stat->name (blocking-do 'get-conf-stat lyskom-pers-no))
                     0)
                    t)))
    (if (null conf-stat)
	(lyskom-insert-string 'no-such-conf-or-pers)
      (if (string-match "^\\(.*\\)(\\(.*\\))\\(.*\\)$" (conf-stat->name conf-stat))
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
				    lyskom-errno)))
	(lyskom-insert-string 'no-paren-in-name)))))
	    


;;; ================================================================
;;;                [ndra organisat|r - Change supervisor

;;; Author: Inge Wallin
;;; Rewritten: linus

(def-kom-command kom-change-supervisor ()
  "Change the supervisor of a person or conference."
  (interactive)
  (let ((supervisee (lyskom-read-conf-stat
		     (lyskom-get-string 'who-to-change-supervisor-for)
		     '(all) nil nil t)))
    (if (null supervisee)
	(lyskom-insert-string 'no-such-conf-or-pers)
      (lyskom-tell-internat 'kom-tell-change-supervisor)
      (let ((supervisor (lyskom-read-conf-stat
			 (lyskom-get-string 'new-supervisor)
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
  "Mark the text TEXT-NO."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-mark)))
  (if text-no
      (lyskom-mark-text text-no)
    (lyskom-insert 'confusion-what-to-mark)))


(def-kom-command kom-unmark-text (&optional text-no)
  "Unmark the text TEXT-NO."
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
        (cond
         ;; Default completion.
         ((and nildefault
               (stringp mark)
               (string= mark ""))
          (setq mark-type 'default))

         ;; Correct completion.
         (mark-assoc
          (setq mark-type (cdr mark-assoc)))

         ;; Incorrect completion, integer entered.
         ((string-match "\\`[0-9]+\\'" mark)
          (setq mark-type (string-to-int mark)))

         ;; Incorrect completion; create new symbolic mark type.
         ((and create-nonexistent
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
         (t
          (lyskom-insert 'erroneous-mark)))))

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
      (let* ((mark (car mark-list))
	     (type (mark->mark-type mark))
	     (tcnt (assq type cnt-alist)))
	(when tcnt (setq tcnt (cdr tcnt)))
	(setq cnt-alist
	      (lyskom-set-alist
	       cnt-alist type (if (null tcnt) 1 (1+ tcnt)))))
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
  "Review marked texts with a certain mark."
  (interactive)
  (lyskom-review-marked-texts
   (lyskom-read-mark-type (lyskom-get-string 'what-mark-to-view) t)))


(def-kom-command kom-review-all-marked-texts ()
  "Review all marked texts"
  (interactive)
  (lyskom-review-marked-texts nil))


(defun lyskom-review-marked-texts (mark-no)
  "Review all marked texts with the mark equal to MARK-NO. 
If MARK-NO is nil, review all marked texts."
  (let ((mark-list (cache-get-marked-texts))
	(text-list nil))
    (while (not (null mark-list))
      (let ((mark (car mark-list)))
	(if (and mark
		 (or (null mark-no)
		     (eq mark-no (mark->mark-type mark))))
	    (setq text-list (cons (mark->text-no mark)
				  text-list))))
      (setq mark-list (cdr mark-list)))
    (if (eq (length text-list) 0)
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


;;; ================================================================
;;;                 [ndra L|senord - Change password

;;; Author: Inge Wallin


(def-kom-command kom-change-password ()
  "Change the password for a person."
  (interactive)
  (let ((pers-no (lyskom-read-conf-no (lyskom-get-string 'whos-passwd)
				      '(pers) t "" t))
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
    ((nil 12 24 nil nil nil) . xmaseve)
    ((nil 12 25 nil nil nil) . xmasday)
    ((nil 12 28 nil nil nil) . varnlosdag)
    ((nil 12 31 nil nil nil) . newyeareve)
    ((nil 12 31  23 nil nil) . newyearevelate)
))


(lyskom-external-function calendar-iso-from-absolute)
(lyskom-external-function calendar-absolute-from-gregorian)

(def-kom-command kom-display-time ()
  "Ask server about time and date."
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
;;;
;;; +++ FIXME specialhack för svenska. Borde det generaliseras?
;;;   
    (when (and (eq lyskom-language 'sv) kom-show-namedays)
      (let ((tmp (lyskom-nameday time)))
        (when tmp
          (lyskom-insert "\n")
          (lyskom-insert tmp)))))
  (lyskom-insert "\n"))


;(def-kom-command kom-display-calendar ()
;  "Nothing yet"
;  (interactive)
;  (let* ((time (lyskom-current-server-time))
;         (nameday (lyskom-nameday time))
;         (special (lyskom-special-date time)))
;    ))
    
(defvar lyskom-nameday-alist
  '((1 . ((1 . ())
          (2 . ("Svea"))
          (3 . ("Alfred" "Alfrida"))
          (4 . ("Rut"))
          (5 . ("Hanna" "Hannele"))
          (6 . ("Kasper" "Melker" "Baltsar"))
          (7 . ("August" "Augusta"))
          (8 . ("Erland"))
          (9 . ("Gunnar" "Gunder"))
          (10 . ("Sigurd" "Sigbritt"))
          (11 . ("Jan" "Jannike"))
          (12 . ("Frideborg" "Fridolf"))
          (13 . ("Knut"))
          (14 . ("Felix" "Felicia"))
          (15 . ("Laura" "Lorentz"))
          (16 . ("Hjalmar" "Helmer"))
          (17 . ("Anton" "Tony"))
          (18 . ("Hilda" "Hildur"))
          (19 . ("Henrik"))
          (20 . ("Fabian" "Sebastian"))
          (21 . ("Agnes" "Agneta"))
          (22 . ("Vincent" "Viktor"))
          (23 . ("Frej" "Freja"))
          (24 . ("Erika"))
          (25 . ("Paul" "Pål"))
          (26 . ("Bodil" "Boel"))
          (27 . ("Göte" "Göta"))
          (28 . ("Karl" "Karla"))
          (29 . ("Diana"))
          (30 . ("Gunilla" "Gunhild"))
          (31 . ("Ivar" "Joar"))))

    (2 . ((1 . ("Max" "Maximilian"))
          (2 . ())
          (3 . ("Disa" "Hjördis"))
          (4 . ("Ansgar" "Anselm"))
          (5 . ("Agata" "Agda"))
          (6 . ("Dorotea" "Doris"))
          (7 . ("Rikard" "Dick"))
          (8 . ("Berta" "Bert"))
          (9 . ("Fanny" "Franciska"))
          (10 . ("Iris"))
          (11 . ("Yngve" "Inge"))
          (12 . ("Evelina" "Evy"))
          (13 . ("Agne" "Ove"))
          (14 . ("Valentin"))
          (15 . ("Sigfrid"))
          (16 . ("Julia" "Julius"))
          (17 . ("Alexandra" "Sandra"))
          (18 . ("Frida" "Fritiof"))
          (19 . ("Gabriella" "Ella"))
          (20 . ("Vivianne"))
          (21 . ("Hilding"))
          (22 . ("Pia"))
          (23 . ("Torsten" "Torun"))
          (24 . ("Mattias" "Mats"))
          (25 . ("Sigvard" "Sivert"))
          (26 . ("Torgny" "Torkel"))
          (27 . ("Lage"))
          (28 . ("Maria"))))

    (3 . ((1 . ("Albin" "Elvira"))
          (2 . ("Ernst" "Erna"))
          (3 . ("Gunborg" "Gunvor"))
          (4 . ("Adrian" "Adriana"))
          (5 . ("Tora" "Tove"))
          (6 . ("Ebba" "Ebbe"))
          (7 . ("Camilla"))
          (8 . ("Siv"))
          (9 . ("Torbjörn" "Torleif"))
          (10 . ("Edla" "Ada"))
          (11 . ("Edvin" "Egon"))
          (12 . ("Viktoria"))
          (13 . ("Greger"))
          (14 . ("Matilda" "Maud"))
          (15 . ("Kristoffer" "Christel"))
          (16 . ("Herbert" "Gilbert"))
          (17 . ("Gertrud"))
          (18 . ("Edvard" "Edmund"))
          (19 . ("Josef" "Josefina"))
          (20 . ("Joakim" "Kim"))
          (21 . ("Bengt"))
          (22 . ("Kennet" "Kent"))
          (23 . ("Gerda" "Gerd"))
          (24 . ("Gabriel" "Rafael"))
          (25 . ())
          (26 . ("Emanuel"))
          (27 . ("Rudolf" "Ralf"))
          (28 . ("Malkolm" "Morgan"))
          (29 . ("Jonas" "Jens"))
          (30 . ("Holger" "Holmfrid"))
          (31 . ("Ester"))))

    (4 . ((1 . ("Harald" "Hervor"))
          (2 . ("Gudmund" "Ingesund"))
          (3 . ("Ferdinand" "Nanna"))
          (4 . ("Marianne" "Marlene"))
          (5 . ("Irene" "Irja"))
          (6 . ("Vilhelm" "Helmi"))
          (7 . ("Irma" "Irmelin"))
          (8 . ("Nadja" "Tanja"))
          (9 . ("Otto" "Ottilia"))
          (10 . ("Ingvar" "Ingvor"))
          (11 . ("Ulf" "Ylva"))
          (12 . ("Liv"))
          (13 . ("Artur" "Douglas"))
          (14 . ("Tiburtius"))
          (15 . ("Olivia" "Oliver"))
          (16 . ("Patrik" "Patricia"))
          (17 . ("Elias" "Elis"))
          (18 . ("Valdemar" "Volmar"))
          (19 . ("Olaus" "Ola"))
          (20 . ("Amalia" "Amelie"))
          (21 . ("Anneli" "Annika"))
          (22 . ("Allan" "Glenn"))
          (23 . ("Georg" "Göran"))
          (24 . ("Vega"))
          (25 . ("Markus"))
          (26 . ("Teresia" "Terese"))
          (27 . ("Engelbrekt"))
          (28 . ("Ture" "Tyra"))
          (29 . ("Tyko"))
          (30 . ("Mariana"))))

    (5 . ((1 . ("Valborg"))
          (2 . ("Filip" "Filippa"))
          (3 . ("John" "Jane"))
          (4 . ("Monika" "Mona"))
          (5 . ("Gotthard" "Erhard"))
          (6 . ("Marit" "Rita"))
          (7 . ("Carina" "Carita"))
          (8 . ("Åke"))
          (9 . ("Reidar" "Reidun"))
          (10 . ("Esbjörn" "Styrbjörn"))
          (11 . ("Märta" "Märit"))
          (12 . ("Charlotta" "Lotta"))
          (13 . ("Linnea" "Linn"))
          (14 . ("Halvard" "Halvar"))
          (15 . ("Sofia" "Sonja"))
          (16 . ("Ronald" "Ronny"))
          (17 . ("Rebecka" "Ruben"))
          (18 . ("Erik"))
          (19 . ("Maj" "Majken"))
          (20 . ("Karolina" "Carola"))
          (21 . ("Konstantin" "Conny"))
          (22 . ("Hemming" "Henning"))
          (23 . ("Desideria" "Desiree"))
          (24 . ("Ivan" "Vanja"))
          (25 . ("Urban"))
          (26 . ("Vilhelmina" "Vilma"))
          (27 . ("Beda" "Blenda"))
          (28 . ("Ingeborg" "Borghild"))
          (29 . ("Yvonne" "Jeanette"))
          (30 . ("Vera" "Veronika"))
          (31 . ("Petronella" "Pernilla"))))

    (6 . ((1 . ("Gun" "Gunnel"))
          (2 . ("Rutger" "Roger"))
          (3 . ("Ingemar" "Gudmar"))
          (4 . ("Solbritt" "Solveig"))
          (5 . ("Bo"))
          (6 . ("Gustav" "Gösta"))
          (7 . ("Robert" "Robin"))
          (8 . ("Eivor" "Majvor"))
          (9 . ("Börje" "Birger"))
          (10 . ("Svante" "Boris"))
          (11 . ("Bertil" "Berthold"))
          (12 . ("Eskil"))
          (13 . ("Aina" "Aino"))
          (14 . ("Håkan" "Hakon"))
          (15 . ("Margit" "Margot"))
          (16 . ("Axel" "Axelina"))
          (17 . ("Torborg" "Torvald"))
          (18 . ("Björn" "Bjarne"))
          (19 . ("Germund" "Görel"))
          (20 . ("Linda"))
          (21 . ("Alf" "Alvar"))
          (22 . ("Paulina" "Paula"))
          (23 . ("Adolf" "Alice"))
          (24 . ())
          (25 . ("David" "Salomon"))
          (26 . ("Rakel" "Lea"))
          (27 . ("Selma" "Fingal"))
          (28 . ("Leo"))
          (29 . ("Peter" "Petra"))
          (30 . ("Elof" "Leif"))))

    (7 . ((1 . ("Aron" "Mirjam"))
          (2 . ("Rosa" "Rosita"))
          (3 . ("Aurora"))
          (4 . ("Ulrika" "Ulla"))
          (5 . ("Laila" "Ritva"))
          (6 . ("Esaias" "Jessika"))
          (7 . ("Klas"))
          (8 . ("Kjell"))
          (9 . ("Jörgen" "Örjan"))
          (10 . ("Andre" "Andrea"))
          (11 . ("Eleonora" "Ellinor"))
          (12 . ("Herman" "Hermine"))
          (13 . ("Joel" "Judit"))
          (14 . ("Folke"))
          (15 . ("Ragnhild" "Ragnvald"))
          (16 . ("Reinhold" "Reine"))
          (17 . ("Bruno"))
          (18 . ("Fredrik" "Fritz"))
          (19 . ("Sara"))
          (20 . ("Margareta" "Greta"))
          (21 . ("Johanna"))
          (22 . ("Magdalena" "Madeleine"))
          (23 . ("Emma"))
          (24 . ("Kristina" "Kerstin"))
          (25 . ("Jakob"))
          (26 . ("Jesper"))
          (27 . ("Marta"))
          (28 . ("Botvid" "Seved"))
          (29 . ("Olof"))
          (30 . ("Algot"))
          (31 . ("Helena" "Elin"))))

    (8 . ((1 . ("Per"))
          (2 . ("Karin" "Kajsa"))
          (3 . ("Tage"))
          (4 . ("Arne" "Arnold"))
          (5 . ("Ulrik" "Alrik"))
          (6 . ("Alfons" "Inez"))
          (7 . ("Dennis" "Denise"))
          (8 . ("Silvia" "Sylvia"))
          (9 . ("Roland"))
          (10 . ("Lars"))
          (11 . ("Susanna"))
          (12 . ("Klara"))
          (13 . ("Kaj"))
          (14 . ("Uno"))
          (15 . ("Stella" "Estelle"))
          (16 . ("Brynolf"))
          (17 . ("Verner" "Valter"))
          (18 . ("Ellen" "Lena"))
          (19 . ("Magnus" "Måns"))
          (20 . ("Bernhard" "Bernt"))
          (21 . ("Jon" "Jonna"))
          (22 . ("Henrietta" "Henrika"))
          (23 . ("Signe" "Signhild"))
          (24 . ("Bartolomeus"))
          (25 . ("Lovisa" "Louise"))
          (26 . ("Östen"))
          (27 . ("Rolf" "Raoul"))
          (28 . ("Gurli" "Leila"))
          (29 . ("Hans" "Hampus"))
          (30 . ("Albert" "Albertina"))
          (31 . ("Arvid" "Vidar"))))

    (9 . ((1 . ("Samuel"))
          (2 . ("Justus" "Justina"))
          (3 . ("Alfhild" "Alva"))
          (4 . ("Gisela"))
          (5 . ("Adela" "Heidi"))
          (6 . ("Lilian" "Lilly"))
          (7 . ("Regina" "Roy"))
          (8 . ("Alma" "Hulda"))
          (9 . ("Anita" "Annette"))
          (10 . ("Tord" "Turid"))
          (11 . ("Dagny" "Helny"))
          (12 . ("Åsa" "Åslög"))
          (13 . ("Sture"))
          (14 . ("Ida"))
          (15 . ("Sigrid" "Siri"))
          (16 . ("Dag" "Daga"))
          (17 . ("Hildegard" "Magnhild"))
          (18 . ("Orvar"))
          (19 . ("Fredrika"))
          (20 . ("Elise" "Lisa"))
          (21 . ("Matteus"))
          (22 . ("Maurits" "Moritz"))
          (23 . ("Tekla" "Tea"))
          (24 . ("Gerhard" "Gert"))
          (25 . ("Tryggve"))
          (26 . ("Enar" "Einar"))
          (27 . ("Dagmar" "Rigmor"))
          (28 . ("Lennart" "Leonard"))
          (29 . ("Mikael" "Mikaela"))
          (30 . ("Helge"))))

    (10 . ((1 . ("Ragnar" "Ragna"))
           (2 . ("Ludvig" "Love"))
           (3 . ("Evald" "Osvald"))
           (4 . ("Frans" "Frank"))
           (5 . ("Bror"))
           (6 . ("Jenny" "Jennifer"))
           (7 . ("Birgitta" "Britta"))
           (8 . ("Nils"))
           (9 . ("Ingrid" "Inger"))
           (10 . ("Harry" "Harriet"))
           (11 . ("Erling" "Jarl"))
           (12 . ("Valfrid" "Manfred"))
           (13 . ("Berit" "Birgit"))
           (14 . ("Stellan"))
           (15 . ("Hedvig" "Hillevi"))
           (16 . ("Finn"))
           (17 . ("Antonia" "Toini"))
           (18 . ("Lukas"))
           (19 . ("Tore" "Tor"))
           (20 . ("Sibylla"))
           (21 . ("Ursula" "Yrsa"))
           (22 . ("Marika" "Marita"))
           (23 . ("Severin" "Sören"))
           (24 . ("Evert" "Eilert"))
           (25 . ("Inga" "Ingalill"))
           (26 . ("Amanda" "Rasmus"))
           (27 . ("Sabina"))
           (28 . ("Simon" "Simone"))
           (29 . ("Viola"))
           (30 . ("Elsa" "Isabella"))
           (31 . ("Edit" "Edgar"))))

    (11 . ((1 . ())
           (2 . ("Tobias"))
           (3 . ("Hubert" "Hugo"))
           (4 . ("Sverker"))
           (5 . ("Eugen" "Eugenia"))
           (6 . ("Gustav" "Adolf"))
           (7 . ("Ingegerd" "Ingela"))
           (8 . ("Vendela"))
           (9 . ("Teodor" "Teodora"))
           (10 . ("Martin" "Martina"))
           (11 . ("Mårten"))
           (12 . ("Konrad" "Kurt"))
           (13 . ("Kristian" "Krister"))
           (14 . ("Emil" "Emilia"))
           (15 . ("Leopold"))
           (16 . ("Vibeke" "Viveka"))
           (17 . ("Naemi" "Naima"))
           (18 . ("Lillemor" "Moa"))
           (19 . ("Elisabet" "Lisbet"))
           (20 . ("Pontus" "Marina"))
           (21 . ("Helga" "Olga"))
           (22 . ("Cecilia" "Sissela"))
           (23 . ("Klemens"))
           (24 . ("Gudrun" "Rune"))
           (25 . ("Katarina" "Katja"))
           (26 . ("Linus"))
           (27 . ("Astrid" "Asta"))
           (28 . ("Malte"))
           (29 . ("Sune"))
           (30 . ("Andreas" "Anders"))))

    (12 . ((1 . ("Oskar" "Ossian"))
           (2 . ("Beata" "Beatrice"))
           (3 . ("Lydia"))
           (4 . ("Barbara" "Barbro"))
           (5 . ("Sven"))
           (6 . ("Nikolaus" "Niklas"))
           (7 . ("Angela" "Angelika"))
           (8 . ("Virginia"))
           (9 . ("Anna"))
           (10 . ("Malin" "Malena"))
           (11 . ("Daniel" "Daniela"))
           (12 . ("Alexander" "Alexis"))
           (13 . ("Lucia"))
           (14 . ("Sten" "Sixten"))
           (15 . ("Gottfrid"))
           (16 . ("Assar"))
           (17 . ("Stig"))
           (18 . ("Abraham"))
           (19 . ("Isak"))
           (20 . ("Israel" "Moses"))
           (21 . ("Tomas"))
           (22 . ("Natanael" "Jonatan"))
           (23 . ("Adam"))
           (24 . ("Eva"))
           (25 . ())
           (26 . ("Stefan" "Staffan"))
           (27 . ("Johannes" "Johan"))
           (28 . ("Benjamin"))
           (29 . ("Natalia" "Natalie"))
           (30 . ("Abel" "Set"))
           (31 . ("Sylvester"))))))

(defun lyskom-nameday (&optional now)
  (let* ((time (or now (lyskom-current-server-time)))
         (mlist (cdr (assq (time->mon time) lyskom-nameday-alist)))
         (dlist (cdr (assq (time->mday time) mlist))))
    (cond ((null dlist) nil)
          ((eq 1 (length dlist))
           (lyskom-format "%#1s har namnsdag i dag." (car dlist)))
          ((eq 2 (length dlist))
           (lyskom-format "%#1s och %#2s har namnsdag i dag."
                          (elt dlist 0) (elt dlist 1)))
          (t (format "%s och %s har namnsdag i dag."
                     (mapconcat 'identity (lyskom-butlast dlist 1) ", ")
                     (elt dlist (1- (length dlist))))))))


;;; ================================================================
;;;                Vilka ({r inloggade) - Who is on?

;;; Author: ???
;;; Rewritten by: David K}gedal


(put 'lyskom-no-users 'error-conditions
     '(error lyskom-error lyskom-no-users))

(def-kom-command kom-who-is-on (&optional arg)
  "Display a list of all connected users.
The prefix arg controls the idle limit of the sessions showed. If the
prefix is negative, invisible sessions are also shown.

If the prefix is 0, all visible sessions are shown."
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
  "Display a list of all connected users in CONF.
The prefix arg controls the idle limit of the sessions showed. If the
prefix is negative, invisible sessions are also shown.

If the prefix is 0, all visible sessions are shown."
  (interactive "P")
  (let ((conf-stat 
	 (lyskom-read-conf-stat (lyskom-get-string 'who-is-on-in-what-conference)
				'(all) nil 
				(let ((ccn 
				       (if (or (null lyskom-current-conf)
					       (zerop lyskom-current-conf))
					   ""
					 (conf-stat->name
					  (blocking-do 'get-conf-stat
						       lyskom-current-conf)))))
				  (if ccn
				      (cons ccn 0)
				    "")) t)))
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
  "Display a list of all connected users currently present in CONF.
The prefix arg controls the idle limit of the sessions showed. If the
prefix is negative, invisible sessions are also shown.

If the prefix is 0, all visible sessions are shown."
  (interactive "P")
  (let ((conf-stat 
	 (lyskom-read-conf-stat (lyskom-get-string 'who-is-present-in-what-conference)
				'(all) nil 
				(let ((ccn 
				       (if (or (null lyskom-current-conf)
					       (zerop lyskom-current-conf))
					   ""
					 (conf-stat->name
					  (blocking-do 'get-conf-stat
						       lyskom-current-conf)))))
				  (if ccn
				      (cons ccn 0)
				    "")) t)))
    (condition-case nil
	(if (lyskom-have-feature dynamic-session-info)
	    (lyskom-who-is-on-9 arg conf-stat t)
	  (lyskom-who-is-on-8 conf-stat t))
      (lyskom-no-users
       (lyskom-insert (lyskom-get-string 'null-who-info))))))

(defun lyskom-who-is-on-8 (&optional conf-stat show-present-only)
  "Display a list of all connected users.
Uses Protocol A version 8 calls"
  (let* ((who-info-list (blocking-do 'who-is-on))
	 (who-list (sort (cond (show-present-only
				(lyskom-who-is-present-check-membership-8 who-info-list conf-stat))
			       (conf-stat
				(lyskom-who-is-on-check-membership-8 who-info-list conf-stat))
			       (t
				(listify-vector who-info-list)))
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

    (cond (show-present-only
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
	     (concat "(" (who-info->doing-what who-info) ")"))))
      (setq who-list (cdr who-list)))
      
      (lyskom-insert (concat (make-string (- (lyskom-window-width) 1) ?-)
			     "\n"))
      (lyskom-insert (lyskom-format 'total-visible-users total-users
                                    (lyskom-format-time
                                     'timeformat-day-yyyy-mm-dd-hh-mm-ss)))))


(defun lyskom-who-is-on-9 (arg &optional conf-stat show-present-only)
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
	 (who-list (sort (cond (show-present-only
				(lyskom-who-is-present-check-membership-9 who-info-list conf-stat))
			       (conf-stat
				(lyskom-who-is-on-check-membership-9 who-info-list conf-stat))
			       (t
				(listify-vector who-info-list)))
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

    (cond (show-present-only
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
	       (concat "(" (dynamic-session-info->what-am-i-doing who-info)
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
	(res nil))
    (while (< i len)
      (if (lyskom-member-list-find-member
           (who-info->pers-no (aref who-info-list i))
           members)
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

(defun lyskom-who-is-on-check-membership-9 (who-info-list conf-stat)
  "Returns a list of those in WHO-INFO-LIST which is member in CONF-STAT."
  (let ((members (blocking-do 'get-members (conf-stat->conf-no conf-stat)
			      0 (conf-stat->no-of-members conf-stat)))
	(len (length who-info-list))
	(i 0)
	(res nil))
    (while (< i len)
      (if (lyskom-member-list-find-member 
           (dynamic-session-info->person (aref who-info-list i))
           members)

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
  "Display a list of all connected users."
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
     (t (frame-width)))))


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

(def-kom-command kom-status-session (&optional arg)
  "Show status for all sessions a person has. Asks for person name.
Optional argument ARG should be a list of sessions to get information
about or a single session number."
  (interactive "P")
  (let ((sessions (or (cond ((listp arg) arg)
                            ((numberp arg) (list arg)))
                      (lyskom-read-session-no 
                       (lyskom-get-string 'status-for-session))))
	who-info)
    (cond ((null sessions)
           (lyskom-insert-string 'no-such-session-r))
          ((and (numberp (car sessions))
                (<= (car sessions) 0))
           (lyskom-format-insert
            (lyskom-get-string 'person-not-logged-in-r)
            (- (car sessions))))
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
  "Jumps all comments to the current text. Descends recursively in comment tree.
The three is truncated if we encounter an older text.
If optional arg TEXT-NO is present then jump all comments to that text instead."
  (interactive (list
		(cond
		 ((null current-prefix-arg)
		  lyskom-current-text)
		 ((integerp current-prefix-arg)
		  current-prefix-arg)
		 ((and (listp current-prefix-arg)
		       (integerp (car current-prefix-arg))
		       (null (cdr current-prefix-arg)))
		  (car current-prefix-arg))
		 (t
		  (signal 'lyskom-internal-error '(kom-jump))))))
  (cond ((and (null current-prefix-arg)
              (eq 'REVIEW-TREE (read-info->type
                                (read-list->first
                                 lyskom-reading-list))))
         (lyskom-start-of-command 'kom-jump)
         (unwind-protect
             (progn 
               (set-read-list-del-first lyskom-reading-list)
               (set-read-list-del-first lyskom-to-do-list))
           (lyskom-end-of-command)))
        (text-no
         (lyskom-start-of-command 'kom-jump)
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
  "Add a recipient to a text."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-add-recipient)))
  (lyskom-add-helper text-no 
                     'lyskom-last-added-rcpt 
                     'who-to-add-q
                     'adding-name-as-recipient
                     'recpt))

(def-kom-command kom-add-copy (text-no)
  "Add a recipient to a text."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-add-copy)))
  (lyskom-add-helper text-no 
                     'lyskom-last-added-ccrcpt 
                     'who-to-add-copy-q
                     'adding-name-as-copy
                     'cc-recpt))

(def-kom-command kom-add-bcc (text-no)
  "Add a recipient to a text."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-add-copy)))
  (lyskom-add-helper text-no 
                     'lyskom-last-added-bccrcpt
                     'who-to-add-bcc-q
                     'adding-name-as-copy
                     'bcc-recpt))

(defun lyskom-add-helper (text-no last-variable who-prompt doing-prompt type)
  (let* ((conf (blocking-do 'get-conf-stat (lyskom-default-value last-variable)))
         (target (lyskom-read-conf-stat
                  (lyskom-get-string who-prompt)
                  '(all) 
                  nil 
                  (cons (if conf (conf-stat->name conf) "") 0)
                  t)))

    (when (and target text-no)

      (when (and (eq type 'recpt)
                 kom-confirm-add-recipients
                 (not (lyskom-j-or-n-p (lyskom-format 'really-add-as-recpt-q
                                                      target))))
        (setq type 'cc-recpt 
              doing-prompt 'adding-name-as-copy))

      (lyskom-set-default last-variable (conf-stat->conf-no target))
      (lyskom-format-insert doing-prompt target text-no)
      (lyskom-move-recipient text-no nil target type))))

(defun lyskom-default-recpt-for-sub (recipients)
  "Select the default recipient for removal from RECIPIENTS.
The value of RECIPIENTS should be the result of a call to 
\(lyskom-text-recipients text-no t\)."
  (let ((last-sub (lyskom-default-value 'lyskom-last-sub-rcpt)))
    (cond ((and (assq last-sub recipients)
                (blocking-do 'get-conf-stat last-sub)))
          ((and (assq lyskom-current-conf recipients)
                (blocking-do 'get-conf-stat lyskom-current-conf)))
          ((and (rassq 'RECPT recipients)
                (blocking-do 'get-conf-stat (car (rassq 'RECPT recipients)))))
          ((and (car recipients)
                (blocking-do 'get-conf-stat (car (car recipients))))))))


(def-kom-command kom-sub-recipient (text-no)
  "Remove a recipient from text TEXT-NO."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-delete-recipient)))
  (let ((text-stat (blocking-do 'get-text-stat text-no)))
    (if text-stat
        (let ((recipients (lyskom-text-recipients text-stat t)))
          (if recipients
              (let* ((conf (lyskom-default-recpt-for-sub recipients))
                     (source (lyskom-read-conf-stat
                              (lyskom-get-string 'who-to-sub-q)
                              (list (cons 'restrict (mapcar 'car recipients)))
                              nil
                              (cons (if conf (conf-stat->name conf) "") 0)
                              t)))
                (when source
                  (lyskom-format-insert 'remove-name-as-recipient source text-no)
                  (lyskom-set-default 'lyskom-last-sub-rcpt (conf-stat->conf-no source))
                  (lyskom-move-recipient text-no source nil nil)
                  ))
            (lyskom-format-insert 'text-has-no-recipients-r text-no)))
      (lyskom-format-insert 'no-such-text-no text-no))))

(def-kom-command kom-move-text (text-no)
  "Move text TEXT-NO from one conference to another."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-move)))
  (let ((text-stat (blocking-do 'get-text-stat text-no)))
    (if text-stat
        (let* ((recipients (lyskom-text-recipients text-stat t))
               (default-from (lyskom-default-recpt-for-sub recipients))
               (default-to (blocking-do 'get-conf-stat
                                        (or (lyskom-default-value 'lyskom-last-added-rcpt)
                                            lyskom-current-conf))))
          (if (null default-from)
              (lyskom-format-insert 'text-has-no-recipients-r text-no)
            (let ((source (lyskom-read-conf-stat 'who-to-move-from-q
                                                 (list
                                                  (cons 'restrict
                                                        (mapcar 'car
                                                                recipients)))
                                                 nil
                                                 (cons (if default-from
                                                           (conf-stat->name default-from)
                                                         "") 0)
                                                 t))
                  (target (lyskom-read-conf-stat 'who-to-move-to-q
                                                 '(all)
                                                 nil
                                                 (cons (if default-to
                                                           (conf-stat->name default-to)
                                                         "") 0)
                                                 t)))
              (when (and source target)
                (lyskom-format-insert 'moving-name source target text-stat)
                (lyskom-set-default 'lyskom-last-added-rcpt (conf-stat->conf-no target))
                (lyskom-set-default 'lyskom-last-sub-rcpt (conf-stat->conf-no source))
                (lyskom-move-recipient text-no source target 'recpt)))))
      (lyskom-format-insert 'no-such-text-no text-no))))
         



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
  "Add a text as a comment to another text."
  (interactive "P")
  (lyskom-add-sub-comment text-no-arg
			  (lyskom-get-string 'text-to-add-comment-to)
			  t))

(def-kom-command kom-sub-comment (text-no-arg)
  "Remove a comment from a text."
  (interactive "P")
  (lyskom-add-sub-comment text-no-arg
			  (lyskom-get-string 'text-to-delete-comment-from)
			  nil))

(defun lyskom-add-sub-comment (text-no-arg prompt do-add)
  "Get the number of the text that is going to have a comment added to it or
subtracted from it
Arguments: TEXT-NO-ARG: an argument as it is gotten from (interactive P)
PROMPT: A string that is used when prompting for a number.
DO-ADD: NIL if a comment should be subtracted.
        Otherwise a comment is added"
  (let ((text-no (let ((current-prefix-arg text-no-arg))
                    (lyskom-read-text-no-prefix-arg prompt nil lyskom-current-text))))
    (if text-no
        (let* ((comment-text-no  (lyskom-read-number
                                  (lyskom-get-string
                                   (if do-add 'text-to-add-q 'text-to-remove-q))
                                  (if (eq text-no lyskom-current-text)
                                      nil
                                    lyskom-current-text))))
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
                              'confusion-what-to-sub-comment-from)))))

(def-kom-command kom-add-footnote (text-no-arg)
  "Add a text as a footnote to another text."
  (interactive "P")
  (lyskom-add-sub-footnote text-no-arg
			  (lyskom-get-string 'text-to-add-footnote-to)
			  t))

(def-kom-command kom-sub-footnote (text-no-arg)
  "Remove a footnote from a text."
  (interactive "P")
  (lyskom-add-sub-footnote text-no-arg
			  (lyskom-get-string 'text-to-delete-footnote-from)
			  nil))

(defun lyskom-add-sub-footnote (text-no-arg prompt do-add)
  "Get the number of the text that is going to have a footnote added to it or
subtracted from it
Arguments: TEXT-NO-ARG: an argument as it is gotten from (interactive P)
PROMPT: A string that is used when prompting for a number.
DO-ADD: NIL if a footnote should be subtracted.
        Otherwise a footnote is added"
  (let ((text-no (let ((current-prefix-arg text-no-arg))
                    (lyskom-read-text-no-prefix-arg prompt nil lyskom-current-text))))
    (if text-no
        (let* ((footnote-text-no  (lyskom-read-number
                                   (lyskom-get-string
                                    (if do-add 
                                        'text-to-add-footn-q 
                                      'text-to-remove-footn-q))
                                   (if (eq text-no lyskom-current-text)
                                       nil
                                     lyskom-current-text))))
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
                       'confusion-what-to-sub-footnote-from)))))

;;; ================================================================
;;;                 Addera referens - Add cross reference

;;; Author: Joel Rosdahl

(def-kom-command kom-add-cross-reference (text-no-arg)
  "Add a cross reference to a text."
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
  (let* ((completions (list (cons (lyskom-get-string 'conference) 'conf)
                            (cons (lyskom-get-string 'person) 'pers)
                            (cons (lyskom-get-string 'text) 'text)))
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
         (prompt nil)
         (char nil))
    (cond
     ((eq type 'text)
      (setq prompt (lyskom-get-string 'which-text-to-xref))
      (while (null obj)
        (setq obj (text-stat->text-no
                   (blocking-do 'get-text-stat
                                (lyskom-read-number prompt))))
        (setq prompt (lyskom-get-string 'which-text-to-xref-err )))
      (setq char "T"))
     ((eq type 'conf)
      (setq prompt (lyskom-get-string 'which-conf-to-xref))
      (while (null obj)
        (setq obj (lyskom-read-conf-no prompt '(conf) nil nil t)))
      (setq char "C"))
     ((eq type 'pers)
      (setq prompt (lyskom-get-string 'which-pers-to-xref))
      (while (null obj)
        (setq obj (lyskom-read-conf-no prompt '(pers) nil nil t)))
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
  (let* ((completions (list (cons (lyskom-get-string 'conference) 'conf)
                            (cons (lyskom-get-string 'person) 'pers)
                            (cons (lyskom-get-string 'text) 'text)))
         (completion-ignore-case t)
         (type (cdr (lyskom-string-assoc
                     (lyskom-completing-read
                      (lyskom-get-string 'link-type)
                      (lyskom-maybe-frob-completion-table
                       completions)
                      nil
                      t)
                     completions)))
         (obj nil)
         (prompt nil))
    (cond
     ((eq type 'text)
      (setq prompt (lyskom-get-string 'which-text-to-link))
      (while (null obj)
        (setq obj (blocking-do 'get-text-stat
			       (lyskom-read-number prompt)))
        (setq prompt (lyskom-get-string 'which-text-to-link-err )))
      (let* ((text-no (text-stat->text-no obj))
             (text (blocking-do 'get-text text-no))
	     (txt (text->decoded-text-mass text obj))
	     (eos (string-match (regexp-quote "\n") txt))
	     (subject (substring txt 0 eos)))
	(format "<text %d: %s>" text-no subject)))
     
     ((eq type 'conf)
      (setq prompt (lyskom-get-string 'which-conf-to-link))
      (while (null obj)
        (setq obj (lyskom-read-conf-stat prompt '(conf) nil nil t)))
      (format "<möte %d: %s>" (conf-stat->conf-no obj)
			   (conf-stat->name obj)))

     ((eq type 'pers)
      (setq prompt (lyskom-get-string 'which-pers-to-link))
      (while (null obj)
        (setq obj (lyskom-read-conf-stat prompt '(pers) nil nil t)))
      (format "<person %d: %s>" (conf-stat->conf-no obj)
			   (conf-stat->name obj))))))


;;; ================================================================
;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; end: 
