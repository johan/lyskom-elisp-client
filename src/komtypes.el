;;;;;
;;;;; $Id: komtypes.el,v 38.0 1994-01-06 01:58:08 linus Exp $
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
;;;; This file contains primitives for the different data types
;;;; in the lyskom system. All types here have their origin in
;;;; the server. Compare the file clienttypes.el.
;;;;
;;;; Author: ceder
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: komtypes.el,v 38.0 1994-01-06 01:58:08 linus Exp $\n"))


;;; ================================================================
;;;                            conf-no-list


;;; Constructor:

(defun lyskom-create-conf-no-list (conf-nos)
  "Create a conf-no-list from all parameters."
  (cons
   'CONF-NO-LIST
   (vector conf-nos)))


;;; Selector:

(defun conf-no-list->conf-nos (conf-no-list)
  "Get conf-nos from conf-no-list."
  (elt (cdr conf-no-list) 0))


;;; Modifier:

(defun set-conf-no-list->conf-nos (conf-no-list newval)
  "Set conf-nos in conf-no-list to NEWVAL."
  (aset (cdr conf-no-list) 0 newval))


;;; Predicate:

(defun lyskom-conf-no-list-p (object)
  "Return t if OBJECT is a conf-no-list."
  (eq (car-safe object) 'CONF-NO-LIST))


;;; Special functions

(defun lyskom-conf-no-list-member (conf-no conf-no-list)
  "Returns non-nil if CONF-NO is a member of CONF-NO-LIST.
CONF-NO is a conf-no and CONF-NO-LIST is a conf-no-list."
  (if (= (length (conf-no-list->conf-nos conf-no-list)) 0)
      nil
    (let* ((r 0)
	   (list (conf-no-list->conf-nos conf-no-list))
	   (len (length list))
	   (yes nil))
      (while (and (not yes)
		  (< r len))
	(if (= conf-no (elt list r))
	    (setq yes t)
	  (setq r (1+ r))))
      yes)))


;;; ================================================================
;;;                            conf-stat

;;; Constructor:

(defun lyskom-create-conf-stat (conf-no
				name
				conf-type
				creation-time
				last-written
				creator
				presentation
				supervisor
				permitted-submitters
				super-conf
				msg-of-day
				garb-nice
				no-of-members
				first-local-no
				no-of-texts)
  "Create a conf-stat from all parameters."
  (cons
   'CONF-STAT
   (vector conf-no name conf-type creation-time last-written 
	   creator presentation supervisor permitted-submitters 
	   super-conf msg-of-day garb-nice no-of-members first-local-no 
	   no-of-texts )))


;;; Selectors:

(defun conf-stat->conf-no (conf-stat)
  "Get conf-no from conf-stat."
  (elt (cdr conf-stat) 0))

(defun conf-stat->name (conf-stat)
  "Get name from conf-stat."
  (elt (cdr conf-stat) 1))

(defun conf-stat->conf-type (conf-stat)
  "Get conf-type from conf-stat."
  (elt (cdr conf-stat) 2))

(defun conf-stat->creation-time (conf-stat)
  "Get creation-time from conf-stat."
  (elt (cdr conf-stat) 3))

(defun conf-stat->last-written (conf-stat)
  "Get last-written from conf-stat."
  (elt (cdr conf-stat) 4))

(defun conf-stat->creator (conf-stat)
  "Get creator from conf-stat."
  (elt (cdr conf-stat) 5))

(defun conf-stat->presentation (conf-stat)
  "Get presentation from conf-stat."
  (elt (cdr conf-stat) 6))

(defun conf-stat->supervisor (conf-stat)
  "Get supervisor from conf-stat."
  (elt (cdr conf-stat) 7))

(defun conf-stat->permitted-submitters (conf-stat)
  "Get permitted-submitters from conf-stat."
  (elt (cdr conf-stat) 8))

(defun conf-stat->super-conf (conf-stat)
  "Get super-conf from conf-stat."
  (elt (cdr conf-stat) 9))

(defun conf-stat->msg-of-day (conf-stat)
  "Get msg-of-day from conf-stat."
  (elt (cdr conf-stat) 10))

(defun conf-stat->garb-nice (conf-stat)
  "Get garb-nice from conf-stat."
  (elt (cdr conf-stat) 11))

(defun conf-stat->no-of-members (conf-stat)
  "Get no-of-members from conf-stat."
  (elt (cdr conf-stat) 12))

(defun conf-stat->first-local-no (conf-stat)
  "Get first-local-no from conf-stat."
  (elt (cdr conf-stat) 13))

(defun conf-stat->no-of-texts (conf-stat)
  "Get no-of-texts from conf-stat."
  (elt (cdr conf-stat) 14))


;;; Modifiers:

(defun set-conf-stat->conf-no (conf-stat newval)
  "Set conf-no in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 0 newval))

(defun set-conf-stat->name (conf-stat newval)
  "Set name in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 1 newval))

(defun set-conf-stat->conf-type (conf-stat newval)
  "Set conf-type in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 2 newval))

(defun set-conf-stat->creation-time (conf-stat newval)
  "Set creation-time in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 3 newval))

(defun set-conf-stat->last-written (conf-stat newval)
  "Set last-written in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 4 newval))

(defun set-conf-stat->creator (conf-stat newval)
  "Set creator in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 5 newval))

(defun set-conf-stat->presentation (conf-stat newval)
  "Set presentation in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 6 newval))

(defun set-conf-stat->supervisor (conf-stat newval)
  "Set supervisor in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 7 newval))

(defun set-conf-stat->permitted-submitters (conf-stat newval)
  "Set permitted-submitters in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 8 newval))

(defun set-conf-stat->super-conf (conf-stat newval)
  "Set super-conf in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 9 newval))

(defun set-conf-stat->msg-of-day (conf-stat newval)
  "Set msg-of-day in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 10 newval))

(defun set-conf-stat->garb-nice (conf-stat newval)
  "Set garb-nice in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 11 newval))

(defun set-conf-stat->no-of-members (conf-stat newval)
  "Set no-of-members in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 12 newval))

(defun set-conf-stat->first-local-no (conf-stat newval)
  "Set first-local-no in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 13 newval))

(defun set-conf-stat->no-of-texts (conf-stat newval)
  "Set no-of-texts in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 14 newval))



;;; Predicate:

(defun lyskom-conf-stat-p (object)
  "Return t if OBJECT is a conf-stat."
  (eq (car-safe object) 'CONF-STAT))


				
				
;;;; ================================================================
;;;; This field is just simulation of a field in the conf-stat
;;;; that not yet exist.

(defun conf-stat->comm-conf (conf-stat)
  (if (and (conf-type->original (conf-stat->conf-type conf-stat))
	   (not (zerop (conf-stat->super-conf conf-stat))))
      (conf-stat->super-conf conf-stat)
    (conf-stat->conf-no conf-stat)))


;;; ================================================================
;;;                         Conf-list


;;; Constructor:

(defun lyskom-create-conf-list (conf-nos conf-types)
  "Create a conf-list from CONF-NOS and CONF-TYPES.
CONF-NOS is a vector of numbers. CONF-TYPES is a vector of conf-type.
Both vectors should be of the same length."
  (cons 'CONF-LIST (cons conf-nos conf-types)))


;;; Selectors:

(defun conf-list->conf-nos (conf-list)
  "Get the conf-nos part of CONF-LIST"
  (car (cdr conf-list)))

(defun conf-list->conf-types (conf-list)
  "Get the conf-types part of CONF-LIST"
  (cdr (cdr conf-list)))


;;; Predicate:

(defun conf-list-p (object)
  "Return true if OBJECT is a conf-list"
  (eq 'CONF-LIST (car-safe object)))


;;; Special functions:

(defun lyskom-conf-list-length (conf-list)
  "Return the length of CONF-LIST"
  (length (conf-list->conf-nos conf-list)))


;;; ================================================================
;;;                             pers-stat


;;; Constructor:

(defun lyskom-create-pers-stat (pers-no
				username
				privileges
				flags
				last-login
				user-area
				total-time-present
				sessions
				created-lines
				created-bytes
				read-texts
				no-of-text-fetches
				created-persons
				created-confs
				first-created-text
				no-of-created-texts
				no-of-marks
				no-of-confs)
  "Create a pers-stat from all parameters."
  (cons
   'PERS-STAT
   (vector pers-no username privileges flags last-login user-area 
	   total-time-present sessions created-lines created-bytes 
	   read-texts no-of-text-fetches created-persons created-confs 
	   first-created-text no-of-created-texts no-of-marks 
	   no-of-confs )))


;;; Selectors:

(defun pers-stat->pers-no (pers-stat)
  "Get pers-no from pers-stat."
  (elt (cdr pers-stat) 0))

(defun pers-stat->username (pers-stat)
  "Get username from pers-stat."
  (elt (cdr pers-stat) 1))

(defun pers-stat->privileges (pers-stat)
  "Get privileges from pers-stat."
  (elt (cdr pers-stat) 2))

(defun pers-stat->flags (pers-stat)
  "Get flags from pers-stat."
  (elt (cdr pers-stat) 3))

(defun pers-stat->last-login (pers-stat)
  "Get last-login from pers-stat."
  (elt (cdr pers-stat) 4))

(defun pers-stat->user-area (pers-stat)
  "Get user-area from pers-stat."
  (elt (cdr pers-stat) 5))

(defun pers-stat->total-time-present (pers-stat)
  "Get total-time-present from pers-stat."
  (elt (cdr pers-stat) 6))

(defun pers-stat->sessions (pers-stat)
  "Get sessions from pers-stat."
  (elt (cdr pers-stat) 7))

(defun pers-stat->created-lines (pers-stat)
  "Get created-lines from pers-stat."
  (elt (cdr pers-stat) 8))

(defun pers-stat->created-bytes (pers-stat)
  "Get created-bytes from pers-stat."
  (elt (cdr pers-stat) 9))

(defun pers-stat->read-texts (pers-stat)
  "Get read-texts from pers-stat."
  (elt (cdr pers-stat) 10))

(defun pers-stat->no-of-text-fetches (pers-stat)
  "Get no-of-text-fetches from pers-stat."
  (elt (cdr pers-stat) 11))

(defun pers-stat->created-persons (pers-stat)
  "Get created-persons from pers-stat."
  (elt (cdr pers-stat) 12))

(defun pers-stat->created-confs (pers-stat)
  "Get created-confs from pers-stat."
  (elt (cdr pers-stat) 13))

(defun pers-stat->first-created-text (pers-stat)
  "Get first-created-text from pers-stat."
  (elt (cdr pers-stat) 14))

(defun pers-stat->no-of-created-texts (pers-stat)
  "Get no-of-created-texts from pers-stat."
  (elt (cdr pers-stat) 15))

(defun pers-stat->no-of-marks (pers-stat)
  "Get no-of-marks from pers-stat."
  (elt (cdr pers-stat) 16))

(defun pers-stat->no-of-confs (pers-stat)
  "Get no-of-confs from pers-stat."
  (elt (cdr pers-stat) 17))


;;; Modifiers:

(defun set-pers-stat->pers-no (pers-stat newval)
  "Set pers-no in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 0 newval))

(defun set-pers-stat->username (pers-stat newval)
  "Set username in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 1 newval))

(defun set-pers-stat->privileges (pers-stat newval)
  "Set privileges in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 2 newval))

(defun set-pers-stat->flags (pers-stat newval)
  "Set flags in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 3 newval))

(defun set-pers-stat->last-login (pers-stat newval)
  "Set last-login in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 4 newval))

(defun set-pers-stat->user-area (pers-stat newval)
  "Set user-area in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 5 newval))

(defun set-pers-stat->total-time-present (pers-stat newval)
  "Set total-time-present in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 6 newval))

(defun set-pers-stat->sessions (pers-stat newval)
  "Set sessions in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 7 newval))

(defun set-pers-stat->created-lines (pers-stat newval)
  "Set created-lines in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 8 newval))

(defun set-pers-stat->created-bytes (pers-stat newval)
  "Set created-bytes in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 9 newval))

(defun set-pers-stat->read-texts (pers-stat newval)
  "Set read-texts in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 10 newval))

(defun set-pers-stat->no-of-text-fetches (pers-stat newval)
  "Set no-of-text-fetches in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 11 newval))

(defun set-pers-stat->created-persons (pers-stat newval)
  "Set created-persons in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 12 newval))

(defun set-pers-stat->created-confs (pers-stat newval)
  "Set created-confs in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 13 newval))

(defun set-pers-stat->first-created-text (pers-stat newval)
  "Set first-created-text in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 14 newval))

(defun set-pers-stat->no-of-created-texts (pers-stat newval)
  "Set no-of-created-texts in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 15 newval))

(defun set-pers-stat->no-of-marks (pers-stat newval)
  "Set no-of-marks in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 16 newval))

(defun set-pers-stat->no-of-confs (pers-stat newval)
  "Set no-of-confs in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 17 newval))


;;; Predicate:

(defun lyskom-pers-stat-p (object)
  "Return t if OBJECT is a pers-stat."
  (eq (car-safe object) 'PERS-STAT))


;;; ================================================================
;;;                           text-stat


;;; Constructor:

(defun lyskom-create-text-stat (text-no
				creation-time
				author
				no-of-lines
				no-of-chars
				no-of-marks
				misc-info-list)
  "Create a text-stat from all parameters."
  (cons
   'TEXT-STAT
   (vector text-no creation-time author no-of-lines no-of-chars 
	   no-of-marks misc-info-list )))


;;; Selectors:

(defun text-stat->text-no (text-stat)
  "Get text-no from text-stat."
  (elt (cdr text-stat) 0))

(defun text-stat->creation-time (text-stat)
  "Get creation-time from text-stat."
  (elt (cdr text-stat) 1))

(defun text-stat->author (text-stat)
  "Get author from text-stat."
  (elt (cdr text-stat) 2))

(defun text-stat->no-of-lines (text-stat)
  "Get no-of-lines from text-stat."
  (elt (cdr text-stat) 3))

(defun text-stat->no-of-chars (text-stat)
  "Get no-of-chars from text-stat."
  (elt (cdr text-stat) 4))

(defun text-stat->no-of-marks (text-stat)
  "Get no-of-marks from text-stat."
  (elt (cdr text-stat) 5))

(defun text-stat->misc-info-list (text-stat)
  "Get misc-info-list from text-stat."
  (elt (cdr text-stat) 6))


;;; Modifiers:

(defun set-text-stat->text-no (text-stat newval)
  "Set text-no in text-stat to NEWVAL."
  (aset (cdr text-stat) 0 newval))

(defun set-text-stat->creation-time (text-stat newval)
  "Set creation-time in text-stat to NEWVAL."
  (aset (cdr text-stat) 1 newval))

(defun set-text-stat->author (text-stat newval)
  "Set author in text-stat to NEWVAL."
  (aset (cdr text-stat) 2 newval))

(defun set-text-stat->no-of-lines (text-stat newval)
  "Set no-of-lines in text-stat to NEWVAL."
  (aset (cdr text-stat) 3 newval))

(defun set-text-stat->no-of-chars (text-stat newval)
  "Set no-of-chars in text-stat to NEWVAL."
  (aset (cdr text-stat) 4 newval))

(defun set-text-stat->no-of-marks (text-stat newval)
  "Set no-of-marks in text-stat to NEWVAL."
  (aset (cdr text-stat) 5 newval))


(defun set-text-stat->misc-info-list (text-stat newval)
  "Set misc-info-list in text-stat to NEWVAL."
  (aset (cdr text-stat) 6 newval))


;;; Predicate:

(defun lyskom-text-stat-p (object)
  "Return t if OBJECT is a text-stat."
  (eq (car-safe object) 'TEXT-STAT))


;;; ================================================================
;;;                            text


;;; Constructor:

(defun lyskom-create-text (text-no
			   text-mass)
  "Create a text from all parameters."
  (cons
   'TEXT
   (vector text-no text-mass )))


;;; Selectors:

(defun text->text-no (text)
  "Get text-no from text."
  (elt (cdr text) 0))

(defun text->text-mass (text)
  "Get text-mass from text."
  (elt (cdr text) 1))


;;; Modifiers:

(defun set-text->text-no (text newval)
  "Set text-no in text to NEWVAL."
  (aset (cdr text) 0 newval))

(defun set-text->text-mass (text newval)
  "Set text-mass in text to NEWVAL."
  (aset (cdr text) 1 newval))


;;; Predicate:

(defun lyskom-text-p (object)
  "Return t if OBJECT is a text."
  (eq (car-safe object) 'TEXT))
				   

;;; ================================================================
;;;                          misc-info


;;; Constructors:

(defun lyskom-create-empty-misc-info ()
  "Create an empty misc-info."
  (lyskom-create-misc-info nil nil nil nil nil nil nil nil nil nil))

(defun lyskom-create-misc-info (type
				recipient-no
				local-no
				rec-time
				comm-to
				comm-in
				footn-to
				footn-in
				sender
				sent-at)
  "Create a misc-info from all parameters.
TYPE is one of RECPT, CC-RECPT COMM-TO COMM-IN FOOTN-TO or FOOTN-IN."
  (cons
   'MISC-INFO
   (vector type recipient-no local-no rec-time comm-to comm-in 
	   footn-to footn-in sender sent-at )))


;;; Selectors:

(defun misc-info->type (misc-info)
  "Get type from misc-info."
  (elt (cdr misc-info) 0))

(defun misc-info->recipient-no (misc-info)
  "Get recipient-no from misc-info."
  (elt (cdr misc-info) 1))

(defun misc-info->local-no (misc-info)
  "Get local-no from misc-info."
  (elt (cdr misc-info) 2))

(defun misc-info->rec-time (misc-info)
  "Get rec-time from misc-info."
  (elt (cdr misc-info) 3))

(defun misc-info->comm-to (misc-info)
  "Get comm-to from misc-info."
  (elt (cdr misc-info) 4))

(defun misc-info->comm-in (misc-info)
  "Get comm-in from misc-info."
  (elt (cdr misc-info) 5))

(defun misc-info->footn-to (misc-info)
  "Get footn-to from misc-info."
  (elt (cdr misc-info) 6))

(defun misc-info->footn-in (misc-info)
  "Get footn-in from misc-info."
  (elt (cdr misc-info) 7))

(defun misc-info->sender (misc-info)
  "Get sender from misc-info."
  (elt (cdr misc-info) 8))

(defun misc-info->sent-at (misc-info)
  "Get sent-at from misc-info."
  (elt (cdr misc-info) 9))


;;; Modifiers:

(defun set-misc-info->type (misc-info newval)
  "Set type in misc-info to NEWVAL."
  (aset (cdr misc-info) 0 newval))

(defun set-misc-info->recipient-no (misc-info newval)
  "Set recipient-no in misc-info to NEWVAL."
  (aset (cdr misc-info) 1 newval))

(defun set-misc-info->local-no (misc-info newval)
  "Set local-no in misc-info to NEWVAL."
  (aset (cdr misc-info) 2 newval))

(defun set-misc-info->rec-time (misc-info newval)
  "Set rec-time in misc-info to NEWVAL."
  (aset (cdr misc-info) 3 newval))

(defun set-misc-info->comm-to (misc-info newval)
  "Set comm-to in misc-info to NEWVAL."
  (aset (cdr misc-info) 4 newval))

(defun set-misc-info->comm-in (misc-info newval)
  "Set comm-in in misc-info to NEWVAL."
  (aset (cdr misc-info) 5 newval))

(defun set-misc-info->footn-to (misc-info newval)
  "Set footn-to in misc-info to NEWVAL."
  (aset (cdr misc-info) 6 newval))

(defun set-misc-info->footn-in (misc-info newval)
  "Set footn-in in misc-info to NEWVAL."
  (aset (cdr misc-info) 7 newval))

(defun set-misc-info->sender (misc-info newval)
  "Set sender in misc-info to NEWVAL."
  (aset (cdr misc-info) 8 newval))

(defun set-misc-info->sent-at (misc-info newval)
  "Set sent-at in misc-info to NEWVAL."
  (aset (cdr misc-info) 9 newval))


;;; Predicate:

(defun lyskom-misc-info-p (object)
  "Return t if OBJECT is a misc-info."
  (eq (car-safe object) 'MISC-INFO))


;;; ================================================================
;;;                                time


;;; Constructor:

(defun lyskom-create-time (sec
			   min
			   hour
			   mday
			   mon
			   year
			   wday
			   yday
			   isdst)
  "Create a time from all parameters."
  (cons
   'TIME
   (vector sec min hour mday mon year wday yday isdst )))


;;; Selectors:

(defun time->sec (time)
  "Get sec from time."
  (elt (cdr time) 0))

(defun time->min (time)
  "Get min from time."
  (elt (cdr time) 1))

(defun time->hour (time)
  "Get hour from time."
  (elt (cdr time) 2))

(defun time->mday (time)
  "Get mday from time."
  (elt (cdr time) 3))

(defun time->mon (time)
  "Get mon from time."
  (elt (cdr time) 4))

(defun time->year (time)
  "Get year from time."
  (elt (cdr time) 5))

(defun time->wday (time)
  "Get wday from time."
  (elt (cdr time) 6))

(defun time->yday (time)
  "Get yday from time."
  (elt (cdr time) 7))

(defun time->isdst (time)
  "Get isdst from time."
  (elt (cdr time) 8))


;;; Predicate:

(defun lyskom-time-p (object)
  "Return t if OBJECT is a time."
  (eq (car-safe object) 'TIME))


;;; ================================================================
;;;                               privs


;;; Constructor:

(defun lyskom-create-privs (wheel
			    admin
			    statistic
			    create_pers
			    create_conf
			    change_name
			    flg7
			    flg8
			    flg9
			    flg10
			    flg11
			    flg12
			    flg13
			    flg14
			    flg15
			    flg16)
  "Create a privs from all parameters."
  (cons
   'PRIVS
   (vector wheel admin statistic create_pers create_conf change_name 
	   flg7 flg8 flg9 flg10 flg11 flg12 flg13 flg14 flg15 
	   flg16 )))


;;; Selectors:

(defun privs->wheel (privs)
  "Get wheel from privs."
  (elt (cdr privs) 0))

(defun privs->admin (privs)
  "Get admin from privs."
  (elt (cdr privs) 1))

(defun privs->statistic (privs)
  "Get statistic from privs."
  (elt (cdr privs) 2))

(defun privs->create_pers (privs)
  "Get create_pers from privs."
  (elt (cdr privs) 3))

(defun privs->create_conf (privs)
  "Get create_conf from privs."
  (elt (cdr privs) 4))

(defun privs->change_name (privs)
  "Get change_name from privs."
  (elt (cdr privs) 5))

(defun privs->flg7 (privs)
  "Get flg7 from privs."
  (elt (cdr privs) 6))

(defun privs->flg8 (privs)
  "Get flg8 from privs."
  (elt (cdr privs) 7))

(defun privs->flg9 (privs)
  "Get flg9 from privs."
  (elt (cdr privs) 8))

(defun privs->flg10 (privs)
  "Get flg10 from privs."
  (elt (cdr privs) 9))

(defun privs->flg11 (privs)
  "Get flg11 from privs."
  (elt (cdr privs) 10))

(defun privs->flg12 (privs)
  "Get flg12 from privs."
  (elt (cdr privs) 11))

(defun privs->flg13 (privs)
  "Get flg13 from privs."
  (elt (cdr privs) 12))

(defun privs->flg14 (privs)
  "Get flg14 from privs."
  (elt (cdr privs) 13))

(defun privs->flg15 (privs)
  "Get flg15 from privs."
  (elt (cdr privs) 14))

(defun privs->flg16 (privs)
  "Get flg16 from privs."
  (elt (cdr privs) 15))


;;; Modifiers:

(defun set-privs->wheel (privs newval)
  "Set wheel in privs to NEWVAL."
  (aset (cdr privs) 0 newval))

(defun set-privs->admin (privs newval)
  "Set admin in privs to NEWVAL."
  (aset (cdr privs) 1 newval))

(defun set-privs->statistic (privs newval)
  "Set statistic in privs to NEWVAL."
  (aset (cdr privs) 2 newval))

(defun set-privs->create_pers (privs newval)
  "Set create_pers in privs to NEWVAL."
  (aset (cdr privs) 3 newval))

(defun set-privs->create_conf (privs newval)
  "Set create_conf in privs to NEWVAL."
  (aset (cdr privs) 4 newval))

(defun set-privs->change_name (privs newval)
  "Set change_name in privs to NEWVAL."
  (aset (cdr privs) 5 newval))

(defun set-privs->flg7 (privs newval)
  "Set flg7 in privs to NEWVAL."
  (aset (cdr privs) 6 newval))

(defun set-privs->flg8 (privs newval)
  "Set flg8 in privs to NEWVAL."
  (aset (cdr privs) 7 newval))

(defun set-privs->flg9 (privs newval)
  "Set flg9 in privs to NEWVAL."
  (aset (cdr privs) 8 newval))

(defun set-privs->flg10 (privs newval)
  "Set flg10 in privs to NEWVAL."
  (aset (cdr privs) 9 newval))

(defun set-privs->flg11 (privs newval)
  "Set flg11 in privs to NEWVAL."
  (aset (cdr privs) 10 newval))

(defun set-privs->flg12 (privs newval)
  "Set flg12 in privs to NEWVAL."
  (aset (cdr privs) 11 newval))

(defun set-privs->flg13 (privs newval)
  "Set flg13 in privs to NEWVAL."
  (aset (cdr privs) 12 newval))

(defun set-privs->flg14 (privs newval)
  "Set flg14 in privs to NEWVAL."
  (aset (cdr privs) 13 newval))

(defun set-privs->flg15 (privs newval)
  "Set flg15 in privs to NEWVAL."
  (aset (cdr privs) 14 newval))

(defun set-privs->flg16 (privs newval)
  "Set flg16 in privs to NEWVAL."
  (aset (cdr privs) 15 newval))


;;; Predicate:

(defun lyskom-privs-p (object)
  "Return t if OBJECT is a privs."
  (eq (car-safe object) 'PRIVS))


;;; ================================================================
;;;                            flags


;;; Constructor:

(defun lyskom-create-flags (unread_is_secret
			    flg2
			    flg3
			    flg4
			    flg5
			    flg6
			    flg7
			    flg8)
  "Create a flags from all parameters."
  (cons
   'FLAGS
   (vector unread_is_secret flg2 flg3 flg4 flg5 flg6 flg7 flg8 
	   )))


;;; Selectors:

(defun flags->unread_is_secret (flags)
  "Get unread_is_secret from flags."
  (elt (cdr flags) 0))

(defun flags->flg2 (flags)
  "Get flg2 from flags."
  (elt (cdr flags) 1))

(defun flags->flg3 (flags)
  "Get flg3 from flags."
  (elt (cdr flags) 2))

(defun flags->flg4 (flags)
  "Get flg4 from flags."
  (elt (cdr flags) 3))

(defun flags->flg5 (flags)
  "Get flg5 from flags."
  (elt (cdr flags) 4))

(defun flags->flg6 (flags)
  "Get flg6 from flags."
  (elt (cdr flags) 5))

(defun flags->flg7 (flags)
  "Get flg7 from flags."
  (elt (cdr flags) 6))

(defun flags->flg8 (flags)
  "Get flg8 from flags."
  (elt (cdr flags) 7))


;;; Modifiers:

(defun set-flags->unread_is_secret (flags newval)
  "Set unread_is_secret in flags to NEWVAL."
  (aset (cdr flags) 0 newval))

(defun set-flags->flg2 (flags newval)
  "Set flg2 in flags to NEWVAL."
  (aset (cdr flags) 1 newval))

(defun set-flags->flg3 (flags newval)
  "Set flg3 in flags to NEWVAL."
  (aset (cdr flags) 2 newval))

(defun set-flags->flg4 (flags newval)
  "Set flg4 in flags to NEWVAL."
  (aset (cdr flags) 3 newval))

(defun set-flags->flg5 (flags newval)
  "Set flg5 in flags to NEWVAL."
  (aset (cdr flags) 4 newval))

(defun set-flags->flg6 (flags newval)
  "Set flg6 in flags to NEWVAL."
  (aset (cdr flags) 5 newval))

(defun set-flags->flg7 (flags newval)
  "Set flg7 in flags to NEWVAL."
  (aset (cdr flags) 6 newval))

(defun set-flags->flg8 (flags newval)
  "Set flg8 in flags to NEWVAL."
  (aset (cdr flags) 7 newval))



;;; Predicate:

(defun lyskom-flags-p (object)
  "Return t if OBJECT is a flags."
  (eq (car-safe object) 'FLAGS))


;;; ================================================================
;;;                             membership


;;; Constructor:

(defun lyskom-create-membership (last-time-read
				 conf-no
				 priority
				 last-text-read
				 read-texts)
  "Create a membership from all parameters."
  (cons
   'MEMBERSHIP
   (vector last-time-read conf-no priority last-text-read read-texts 
	   )))


;;; Selectors:

(defun membership->last-time-read (membership)
  "Get last-time-read from membership."
  (elt (cdr membership) 0))

(defun membership->conf-no (membership)
  "Get conf-no from membership."
  (elt (cdr membership) 1))

(defun membership->priority (membership)
  "Get priority from membership."
  (elt (cdr membership) 2))

(defun membership->last-text-read (membership)
  "Get last-text-read from membership."
  (elt (cdr membership) 3))

(defun membership->read-texts (membership)
  "Get read-texts from membership."
  (elt (cdr membership) 4))


;;; Modifiers:

(defun set-membership->last-time-read (membership newval)
  "Set last-time-read in membership to NEWVAL."
  (aset (cdr membership) 0 newval))

(defun set-membership->conf-no (membership newval)
  "Set conf-no in membership to NEWVAL."
  (aset (cdr membership) 1 newval))

(defun set-membership->priority (membership newval)
  "Set priority in membership to NEWVAL."
  (aset (cdr membership) 2 newval))

(defun set-membership->last-text-read (membership newval)
  "Set last-text-read in membership to NEWVAL."
  (aset (cdr membership) 3 newval))

(defun set-membership->read-texts (membership newval)
  "Set read-texts in membership to NEWVAL."
  (aset (cdr membership) 4 newval))


;;; Predicate:

(defun lyskom-membership-p (object)
  "Return t if OBJECT is a membership."
  (eq (car-safe object) 'MEMBERSHIP))


;;; ================================================================
;;;                               map


;;; Constructor:

(defun lyskom-create-map (first-local text-nos)
  "Create a map from all parameters."
  (cons
   'MAP
   (vector first-local text-nos )))


;;; Selectors:

(defun map->first-local (map)
  "Get first-local from map."
  (elt (cdr map) 0))

(defun map->text-nos (map)
  "Get text-nos from map."
  (elt (cdr map) 1))


;;; Modifiers:

(defun set-map->first-local (map newval)
  "Set first-local in map to NEWVAL."
  (aset (cdr map) 0 newval))

(defun set-map->text-nos (map newval)
  "Set text-nos in map to NEWVAL."
  (aset (cdr map) 1 newval))


;;; Predicate:

(defun lyskom-map-p (object)
  "Return t if OBJECT is a map."
  (eq (car-safe object) 'MAP))

			  
;;; Concat:

(defun lyskom-map-concat (&rest maps)
  "Take any number of MAPS and return a new map which is the sum of the maps.
Args: &rest MAPS.
The MAPS must be consecutive. No gaps or overlaps are currently allowed."
  (if (null maps)
      (lyskom-create-map 1 [])
    (let* ((first (map->first-local (car maps)))
	   (high (+ first (length (map->text-nos (car maps)))))
	   (maplist (list (map->text-nos (car maps))))
	   (maps (cdr maps)))
      (while maps
	(if (/= (map->first-local (car maps))
		high)
	    (signal 'lyskom-internal-error '("lyskom-map-concat")))
	(setq maplist (nconc maplist (list (map->text-nos (car maps)))))
	(setq high (+ high (length (map->text-nos (car maps)))))
	(setq maps (cdr maps)))
      (lyskom-create-map first (apply 'vconcat maplist)))))


;;; ================================================================
;;;                            mark


;;; Constructor:

(defun lyskom-create-mark (text-no
			   mark-type)
  "Create a mark from all parameters."
  (cons
   'MARK
   (vector text-no mark-type )))


;;; Selectors:

(defun mark->text-no (mark)
  "Get text-no from mark."
  (elt (cdr mark) 0))

(defun mark->mark-type (mark)
  "Get mark-type from mark."
  (elt (cdr mark) 1))


;;; Modifiers:

(defun set-mark->text-no (mark newval)
  "Set text-no in mark to NEWVAL."
  (aset (cdr mark) 0 newval))

(defun set-mark->mark-type (mark newval)
  "Set mark-type in mark to NEWVAL."
  (aset (cdr mark) 1 newval))


;;; Predicate:

(defun lyskom-mark-p (object)
  "Return t if OBJECT is a mark."
  (eq (car-safe object) 'MARK))


;;; ================================================================
;;;                           who-info


;;; Constructor:

(defun lyskom-create-who-info (pers-no
			       working-conf
			       connection
			       doing-what
			       username)
  "Create a who-info from all parameters."
  (cons
   'WHO-INFO
   (vector pers-no working-conf connection doing-what username 
	   )))


;;; Selectors:

(defun who-info->pers-no (who-info)
  "Get pers-no from who-info."
  (elt (cdr who-info) 0))

(defun who-info->working-conf (who-info)
  "Get working-conf from who-info."
  (elt (cdr who-info) 1))

(defun who-info->connection (who-info)
  "Get connection from who-info."
  (elt (cdr who-info) 2))

(defun who-info->doing-what (who-info)
  "Get doing-what from who-info."
  (elt (cdr who-info) 3))

(defun who-info->username (who-info)
  "Get username from who-info."
  (elt (cdr who-info) 4))


;;; Modifiers:

(defun set-who-info->pers-no (who-info newval)
  "Set pers-no in who-info to NEWVAL."
  (aset (cdr who-info) 0 newval))

(defun set-who-info->working-conf (who-info newval)
  "Set working-conf in who-info to NEWVAL."
  (aset (cdr who-info) 1 newval))

(defun set-who-info->connection (who-info newval)
  "Set connection in who-info to NEWVAL."
  (aset (cdr who-info) 2 newval))

(defun set-who-info->doing-what (who-info newval)
  "Set doing-what in who-info to NEWVAL."
  (aset (cdr who-info) 3 newval))

(defun set-who-info->username (who-info newval)
  "Set username in who-info to NEWVAL."
  (aset (cdr who-info) 4 newval))


;;; Predicate:

(defun lyskom-who-info-p (object)
  "Return t if OBJECT is a who-info."
  (eq (car-safe object) 'WHO-INFO))

		       
;;; ================================================================
;;;                         session-info


;;; Constructor:

(defun lyskom-create-session-info (pers-no
				   working-conf
				   connection
				   doing
				   username
				   idletime
				   connect-time)
  "Create a session-info from all parameters."
  (cons
   'SESSION-INFO
   (vector pers-no working-conf connection doing username idletime 
	   connect-time )))


;;; Selectors:

(defun session-info->pers-no (session-info)
  "Get pers-no from session-info."
  (elt (cdr session-info) 0))

(defun session-info->working-conf (session-info)
  "Get working-conf from session-info."
  (elt (cdr session-info) 1))

(defun session-info->connection (session-info)
  "Get connection from session-info."
  (elt (cdr session-info) 2))

(defun session-info->doing (session-info)
  "Get doing from session-info."
  (elt (cdr session-info) 3))

(defun session-info->username (session-info)
  "Get username from session-info."
  (elt (cdr session-info) 4))

(defun session-info->idletime (session-info)
  "Get idletime from session-info."
  (elt (cdr session-info) 5))

(defun session-info->connect-time (session-info)
  "Get connect-time from session-info."
  (elt (cdr session-info) 6))


;;; Modifiers:

(defun set-session-info->pers-no (session-info newval)
  "Set pers-no in session-info to NEWVAL."
  (aset (cdr session-info) 0 newval))

(defun set-session-info->working-conf (session-info newval)
  "Set working-conf in session-info to NEWVAL."
  (aset (cdr session-info) 1 newval))

(defun set-session-info->connection (session-info newval)
  "Set connection in session-info to NEWVAL."
  (aset (cdr session-info) 2 newval))

(defun set-session-info->doing (session-info newval)
  "Set doing in session-info to NEWVAL."
  (aset (cdr session-info) 3 newval))

(defun set-session-info->username (session-info newval)
  "Set username in session-info to NEWVAL."
  (aset (cdr session-info) 4 newval))

(defun set-session-info->idletime (session-info newval)
  "Set idletime in session-info to NEWVAL."
  (aset (cdr session-info) 5 newval))

(defun set-session-info->connect-time (session-info newval)
  "Set connect-time in session-info to NEWVAL."
  (aset (cdr session-info) 6 newval))



;;; Predicate:

(defun lyskom-session-info-p (object)
  "Return t if OBJECT is a session-info."
  (eq (car-safe object) 'SESSION-INFO))


				   
;;; ================================================================
;;;                          conf-type.


;;; Constructor:

(defun lyskom-create-conf-type (rd_prot original secret letterbox)
  "Create a conf-type object. Args: RD_PROT ORIGINAL SECRET LETTERBOX."
  (list 'CONF-TYPE 
	rd_prot 
	original 
	secret 
	letterbox
	))


;;;Selectors:

(defun conf-type->rd_prot (conf-type)
  "Get rd_prot from conf-type."
  (elt (cdr conf-type) 0))

(defun conf-type->original (conf-type)
  "Get original from conf-type."
  (elt (cdr conf-type) 1))

(defun conf-type->secret (conf-type)
  "Get secret from conf-type."
  (elt (cdr conf-type) 2))

(defun conf-type->letterbox (conf-type)
  "Get letterbox from conf-type."
  (elt (cdr conf-type) 3))


;;; ================================================================
;;;                            text-list


;;; Constructor:

(defun lyskom-create-text-list (texts)
  "Create a text-list from all parameters."
  (cons 'TEXT-LIST texts))


;;; Selector:

(defun text-list->texts (text-list)
  "Get texts from text-list."
  (cdr text-list))


;;; Modifier:

(defun set-text-list->texts (text-list newval)
  "Set texts in TEXT-LIST to NEWVAL."
  (setcdr text-list newval))


;;; Predicate:

(defun lyskom-text-list-p (object)
  "Return t if OBJECT is a text-list."
  (eq (car-safe object) 'TEXT-LIST))


				   
;;; ================================================================
;;;                          server-info


;;; Constructor:

(defun lyskom-create-server-info (version
				  conf-pres-conf
				  pers-pres-conf
				  motd-conf
				  kom-news-conf
				  motd-of-lyskom)
  "Create a server-info from all parameters."
  (cons
   'SERVER-INFO
   (vector version conf-pres-conf pers-pres-conf motd-conf kom-news-conf 
	   motd-of-lyskom )))


;;; Selectors:

(defun server-info->version (server-info)
  "Get version from server-info."
  (elt (cdr server-info) 0))

(defun server-info->conf-pres-conf (server-info)
  "Get conf-pres-conf from server-info."
  (elt (cdr server-info) 1))

(defun server-info->pers-pres-conf (server-info)
  "Get pers-pres-conf from server-info."
  (elt (cdr server-info) 2))

(defun server-info->motd-conf (server-info)
  "Get motd-conf from server-info."
  (elt (cdr server-info) 3))

(defun server-info->kom-news-conf (server-info)
  "Get kom-news-conf from server-info."
  (elt (cdr server-info) 4))

(defun server-info->motd-of-lyskom (server-info)
  "Get motd-of-lyskom from server-info."
  (elt (cdr server-info) 5))


;;; Modifiers:

(defun set-server-info->version (server-info newval)
  "Set version in server-info to NEWVAL."
  (aset (cdr server-info) 0 newval))

(defun set-server-info->conf-pres-conf (server-info newval)
  "Set conf-pres-conf in server-info to NEWVAL."
  (aset (cdr server-info) 1 newval))

(defun set-server-info->pers-pres-conf (server-info newval)
  "Set pers-pres-conf in server-info to NEWVAL."
  (aset (cdr server-info) 2 newval))

(defun set-server-info->motd-conf (server-info newval)
  "Set motd-conf in server-info to NEWVAL."
  (aset (cdr server-info) 3 newval))

(defun set-server-info->kom-news-conf (server-info newval)
  "Set kom-news-conf in server-info to NEWVAL."
  (aset (cdr server-info) 4 newval))

(defun set-server-info->motd-of-lyskom (server-info newval)
  "Set motd-of-lyskom in server-info to NEWVAL."
  (aset (cdr server-info) 5 newval))


;;; Predicate:

(defun lyskom-server-info-p (object)
  "Return t if OBJECT is a server-info."
  (eq (car-safe object) 'SERVER-INFO))


;;; ================================================================
