;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: komtypes.el,v 44.9 1999-06-29 14:21:14 byers Exp $
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
;;;; This file contains primitives for the different data types
;;;; in the lyskom system. All types here have their origin in
;;;; the server. Compare the file clienttypes.el.
;;;;
;;;; Author: ceder
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: komtypes.el,v 44.9 1999-06-29 14:21:14 byers Exp $\n"))


;;; ============================================================
;;; Black magic...

(defmacro def-komtype (type &rest args)
  (let ((typename (symbol-name type))
	(n 0)
        (tmp nil))
    ;; Constructor
    (append
     (list 'progn
           (list 'defsubst
                 (intern (concat "lyskom-create-" typename))
                 args
                 (concat "Create a `" typename "' from arguments.\n"
                         "Args: " (upcase (mapconcat
                                           'symbol-name args " ")) "\n"
                         "Automatically created with def-komtype.")
                 (list 'cons
                       (list 'quote (intern (upcase typename)))
                       (cons 'vector args)))
           ;; Identifier
           (list 'defsubst
                 (intern (concat "lyskom-" typename "-p"))
                 (list type)
                 (concat "Return `t' if " (upcase typename)
                         " is a " typename ".\n"
                         "Args: " (upcase typename) "\n"
                         "Automatically created with def-komtype.")
                 (list 'and
                       (list 'consp type)
                       (list 'eq (list 'car type)
                             (list 'quote (intern (upcase typename)))))))
    ;; Selectors/Modifiers
     (progn
       (while args
         (let ((argname (symbol-name (car args))))
           ;; Selctor
           (setq tmp (cons
                      (list 'defsubst
                            (intern (concat typename "->" argname))
                            (list type)
                            "Automatically created with def-komtype."
                            (list 'aref (list 'cdr type) n))
                      tmp))
           ;; Modifier
           (setq tmp (cons
                      (list 'defsubst
                            (intern (concat "set-" typename "->" argname))
                            (list type (car args))
                            "Automatically created with def-komtype."
                            (list 'aset (list 'cdr type) n (car args)))
                      tmp))
           (setq n (1+ n)
                 args (cdr args))))
       tmp))))



;;; ================================================================
;;;                            conf-no-list


;;; Constructor:

(defsubst lyskom-create-conf-no-list (conf-nos)
  "Create a conf-no-list from all parameters."
  (cons
   'CONF-NO-LIST
   (vector
    (cond ((vectorp conf-nos) (append conf-nos nil))
          (t conf-nos)))))



;;; Selector:

(defsubst conf-no-list->conf-nos (conf-no-list)
  "Get conf-nos from conf-no-list."
  (elt (cdr conf-no-list) 0))


;;; Modifier:

(defsubst set-conf-no-list->conf-nos (conf-no-list newval)
  "Set conf-nos in conf-no-list to NEWVAL."
  (aset (cdr conf-no-list) 0 newval))


;;; Predicate:

(defsubst lyskom-conf-no-list-p (object)
  "Return t if OBJECT is a conf-no-list."
  (eq (car-safe object) 'CONF-NO-LIST))


;;; Special functions

(defsubst lyskom-conf-no-list-member (conf-no conf-no-list)
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
;;;                            uconf-stat

;;; Constructor:

(def-komtype uconf-stat 
  conf-no name conf-type highest-local-no nice)

;;; ================================================================
;;;                            conf-stat

;;; Constructor:

(defsubst lyskom-create-conf-stat (conf-no
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
                                keep-commented
				no-of-members
				first-local-no
				no-of-texts
                                &optional expire
                                aux-items)
  "Create a conf-stat from all parameters."
  (cons
   'CONF-STAT
   (vector conf-no name conf-type creation-time last-written 
	   creator presentation supervisor permitted-submitters 
	   super-conf msg-of-day garb-nice no-of-members first-local-no 
	   no-of-texts (or expire 0) aux-items)))


;;; Selectors:

(defsubst conf-stat->conf-no (conf-stat)
  "Get conf-no from conf-stat."
  (elt (cdr conf-stat) 0))

(defsubst conf-stat->name (conf-stat)
  "Get name from conf-stat."
  (elt (cdr conf-stat) 1))

(defsubst conf-stat->conf-type (conf-stat)
  "Get conf-type from conf-stat."
  (elt (cdr conf-stat) 2))

(defsubst conf-stat->creation-time (conf-stat)
  "Get creation-time from conf-stat."
  (elt (cdr conf-stat) 3))

(defsubst conf-stat->last-written (conf-stat)
  "Get last-written from conf-stat."
  (elt (cdr conf-stat) 4))

(defsubst conf-stat->creator (conf-stat)
  "Get creator from conf-stat."
  (elt (cdr conf-stat) 5))

(defsubst conf-stat->presentation (conf-stat)
  "Get presentation from conf-stat."
  (elt (cdr conf-stat) 6))

(defsubst conf-stat->supervisor (conf-stat)
  "Get supervisor from conf-stat."
  (elt (cdr conf-stat) 7))

(defsubst conf-stat->permitted-submitters (conf-stat)
  "Get permitted-submitters from conf-stat."
  (elt (cdr conf-stat) 8))

(defsubst conf-stat->super-conf (conf-stat)
  "Get super-conf from conf-stat."
  (elt (cdr conf-stat) 9))

(defsubst conf-stat->msg-of-day (conf-stat)
  "Get msg-of-day from conf-stat."
  (elt (cdr conf-stat) 10))

(defsubst conf-stat->garb-nice (conf-stat)
  "Get garb-nice from conf-stat."
  (elt (cdr conf-stat) 11))

(defsubst conf-stat->no-of-members (conf-stat)
  "Get no-of-members from conf-stat."
  (elt (cdr conf-stat) 12))

(defsubst conf-stat->first-local-no (conf-stat)
  "Get first-local-no from conf-stat."
  (elt (cdr conf-stat) 13))

(defsubst conf-stat->no-of-texts (conf-stat)
  "Get no-of-texts from conf-stat."
  (elt (cdr conf-stat) 14))

(defsubst conf-stat->expire (conf-stat)
  "Get expire from conf-stat."
  (elt (cdr conf-stat) 15))

(defsubst conf-stat->aux-items (conf-stat)
  "Get aux-items from conf-stat."
  (elt (cdr conf-stat) 16))


;;; Modifiers:

(defsubst set-conf-stat->conf-no (conf-stat newval)
  "Set conf-no in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 0 newval))

(defsubst set-conf-stat->name (conf-stat newval)
  "Set name in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 1 newval))

(defsubst set-conf-stat->conf-type (conf-stat newval)
  "Set conf-type in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 2 newval))

(defsubst set-conf-stat->creation-time (conf-stat newval)
  "Set creation-time in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 3 newval))

(defsubst set-conf-stat->last-written (conf-stat newval)
  "Set last-written in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 4 newval))

(defsubst set-conf-stat->creator (conf-stat newval)
  "Set creator in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 5 newval))

(defsubst set-conf-stat->presentation (conf-stat newval)
  "Set presentation in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 6 newval))

(defsubst set-conf-stat->supervisor (conf-stat newval)
  "Set supervisor in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 7 newval))

(defsubst set-conf-stat->permitted-submitters (conf-stat newval)
  "Set permitted-submitters in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 8 newval))

(defsubst set-conf-stat->super-conf (conf-stat newval)
  "Set super-conf in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 9 newval))

(defsubst set-conf-stat->msg-of-day (conf-stat newval)
  "Set msg-of-day in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 10 newval))

(defsubst set-conf-stat->garb-nice (conf-stat newval)
  "Set garb-nice in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 11 newval))

(defsubst set-conf-stat->no-of-members (conf-stat newval)
  "Set no-of-members in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 12 newval))

(defsubst set-conf-stat->first-local-no (conf-stat newval)
  "Set first-local-no in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 13 newval))

(defsubst set-conf-stat->no-of-texts (conf-stat newval)
  "Set no-of-texts in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 14 newval))

(defsubst set-conf-stat->expire (conf-stat newval)
  "Set expire in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 15 newval))

(defsubst set-conf-stat->aux-items (conf-stat newval)
  "Set aux-items in conf-stat to NEWVAL."
  (aset (cdr conf-stat) 16 newval))



;;; Predicate:

(defsubst lyskom-conf-stat-p (object)
  "Return t if OBJECT is a conf-stat."
  (eq (car-safe object) 'CONF-STAT))


				
				
;;; ================================================================
;;;                         Conf-list


;;; Constructor:

(defsubst lyskom-create-conf-list (conf-nos conf-types)
  "Create a conf-list from CONF-NOS and CONF-TYPES.
CONF-NOS is a vector of numbers. CONF-TYPES is a vector of conf-type.
Both vectors should be of the same length."
  (cons 'CONF-LIST (cons conf-nos conf-types)))


;;; Selectors:

(defsubst conf-list->conf-nos (conf-list)
  "Get the conf-nos part of CONF-LIST"
  (car (cdr conf-list)))

(defsubst conf-list->conf-types (conf-list)
  "Get the conf-types part of CONF-LIST"
  (cdr (cdr conf-list)))


;;; Predicate:

(defsubst conf-list-p (object)
  "Return true if OBJECT is a conf-list"
  (eq 'CONF-LIST (car-safe object)))


;;; Special functions:

(defsubst lyskom-conf-list-length (conf-list)
  "Return the length of CONF-LIST"
  (length (conf-list->conf-nos conf-list)))


;;; ================================================================
;;;                             pers-stat


;;; Constructor:

(defsubst lyskom-create-pers-stat (pers-no
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

(defsubst pers-stat->pers-no (pers-stat)
  "Get pers-no from pers-stat."
  (elt (cdr pers-stat) 0))

(defsubst pers-stat->username (pers-stat)
  "Get username from pers-stat."
  (elt (cdr pers-stat) 1))

(defsubst pers-stat->privileges (pers-stat)
  "Get privileges from pers-stat."
  (elt (cdr pers-stat) 2))

(defsubst pers-stat->flags (pers-stat)
  "Get flags from pers-stat."
  (elt (cdr pers-stat) 3))

(defsubst pers-stat->last-login (pers-stat)
  "Get last-login from pers-stat."
  (elt (cdr pers-stat) 4))

(defsubst pers-stat->user-area (pers-stat)
  "Get user-area from pers-stat."
  (elt (cdr pers-stat) 5))

(defsubst pers-stat->total-time-present (pers-stat)
  "Get total-time-present from pers-stat."
  (elt (cdr pers-stat) 6))

(defsubst pers-stat->sessions (pers-stat)
  "Get sessions from pers-stat."
  (elt (cdr pers-stat) 7))

(defsubst pers-stat->created-lines (pers-stat)
  "Get created-lines from pers-stat."
  (elt (cdr pers-stat) 8))

(defsubst pers-stat->created-bytes (pers-stat)
  "Get created-bytes from pers-stat."
  (elt (cdr pers-stat) 9))

(defsubst pers-stat->read-texts (pers-stat)
  "Get read-texts from pers-stat."
  (elt (cdr pers-stat) 10))

(defsubst pers-stat->no-of-text-fetches (pers-stat)
  "Get no-of-text-fetches from pers-stat."
  (elt (cdr pers-stat) 11))

(defsubst pers-stat->created-persons (pers-stat)
  "Get created-persons from pers-stat."
  (elt (cdr pers-stat) 12))

(defsubst pers-stat->created-confs (pers-stat)
  "Get created-confs from pers-stat."
  (elt (cdr pers-stat) 13))

(defsubst pers-stat->first-created-text (pers-stat)
  "Get first-created-text from pers-stat."
  (elt (cdr pers-stat) 14))

(defsubst pers-stat->no-of-created-texts (pers-stat)
  "Get no-of-created-texts from pers-stat."
  (elt (cdr pers-stat) 15))

(defsubst pers-stat->no-of-marks (pers-stat)
  "Get no-of-marks from pers-stat."
  (elt (cdr pers-stat) 16))

(defsubst pers-stat->no-of-confs (pers-stat)
  "Get no-of-confs from pers-stat."
  (elt (cdr pers-stat) 17))


;;; Modifiers:

(defsubst set-pers-stat->pers-no (pers-stat newval)
  "Set pers-no in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 0 newval))

(defsubst set-pers-stat->username (pers-stat newval)
  "Set username in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 1 newval))

(defsubst set-pers-stat->privileges (pers-stat newval)
  "Set privileges in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 2 newval))

(defsubst set-pers-stat->flags (pers-stat newval)
  "Set flags in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 3 newval))

(defsubst set-pers-stat->last-login (pers-stat newval)
  "Set last-login in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 4 newval))

(defsubst set-pers-stat->user-area (pers-stat newval)
  "Set user-area in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 5 newval))

(defsubst set-pers-stat->total-time-present (pers-stat newval)
  "Set total-time-present in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 6 newval))

(defsubst set-pers-stat->sessions (pers-stat newval)
  "Set sessions in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 7 newval))

(defsubst set-pers-stat->created-lines (pers-stat newval)
  "Set created-lines in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 8 newval))

(defsubst set-pers-stat->created-bytes (pers-stat newval)
  "Set created-bytes in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 9 newval))

(defsubst set-pers-stat->read-texts (pers-stat newval)
  "Set read-texts in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 10 newval))

(defsubst set-pers-stat->no-of-text-fetches (pers-stat newval)
  "Set no-of-text-fetches in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 11 newval))

(defsubst set-pers-stat->created-persons (pers-stat newval)
  "Set created-persons in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 12 newval))

(defsubst set-pers-stat->created-confs (pers-stat newval)
  "Set created-confs in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 13 newval))

(defsubst set-pers-stat->first-created-text (pers-stat newval)
  "Set first-created-text in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 14 newval))

(defsubst set-pers-stat->no-of-created-texts (pers-stat newval)
  "Set no-of-created-texts in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 15 newval))

(defsubst set-pers-stat->no-of-marks (pers-stat newval)
  "Set no-of-marks in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 16 newval))

(defsubst set-pers-stat->no-of-confs (pers-stat newval)
  "Set no-of-confs in pers-stat to NEWVAL."
  (aset (cdr pers-stat) 17 newval))


;;; Predicate:

(defsubst lyskom-pers-stat-p (object)
  "Return t if OBJECT is a pers-stat."
  (eq (car-safe object) 'PERS-STAT))


;;; ================================================================
;;;                           text-stat


;;; Constructor:

(defsubst lyskom-create-text-stat (text-no
				creation-time
				author
				no-of-lines
				no-of-chars
				no-of-marks
				misc-info-list
                                &optional aux-items)
  "Create a text-stat from all parameters."
  (cons
   'TEXT-STAT
   (vector text-no creation-time author no-of-lines no-of-chars 
	   no-of-marks misc-info-list aux-items)))


;;; Selectors:

(defsubst text-stat->text-no (text-stat)
  "Get text-no from text-stat."
  (elt (cdr text-stat) 0))

(defsubst text-stat->creation-time (text-stat)
  "Get creation-time from text-stat."
  (elt (cdr text-stat) 1))

(defsubst text-stat->author (text-stat)
  "Get author from text-stat."
  (elt (cdr text-stat) 2))

(defsubst text-stat->no-of-lines (text-stat)
  "Get no-of-lines from text-stat."
  (elt (cdr text-stat) 3))

(defsubst text-stat->no-of-chars (text-stat)
  "Get no-of-chars from text-stat."
  (elt (cdr text-stat) 4))

(defsubst text-stat->no-of-marks (text-stat)
  "Get no-of-marks from text-stat."
  (elt (cdr text-stat) 5))

(defsubst text-stat->misc-info-list (text-stat)
  "Get misc-info-list from text-stat."
  (elt (cdr text-stat) 6))

(defsubst text-stat->aux-items (text-stat)
  "Get aux-items from text-stat."
  (elt (cdr text-stat) 7))


;;; Modifiers:

(defsubst set-text-stat->text-no (text-stat newval)
  "Set text-no in text-stat to NEWVAL."
  (aset (cdr text-stat) 0 newval))

(defsubst set-text-stat->creation-time (text-stat newval)
  "Set creation-time in text-stat to NEWVAL."
  (aset (cdr text-stat) 1 newval))

(defsubst set-text-stat->author (text-stat newval)
  "Set author in text-stat to NEWVAL."
  (aset (cdr text-stat) 2 newval))

(defsubst set-text-stat->no-of-lines (text-stat newval)
  "Set no-of-lines in text-stat to NEWVAL."
  (aset (cdr text-stat) 3 newval))

(defsubst set-text-stat->no-of-chars (text-stat newval)
  "Set no-of-chars in text-stat to NEWVAL."
  (aset (cdr text-stat) 4 newval))

(defsubst set-text-stat->no-of-marks (text-stat newval)
  "Set no-of-marks in text-stat to NEWVAL."
  (aset (cdr text-stat) 5 newval))

(defsubst set-text-stat->misc-info-list (text-stat newval)
  "Set misc-info-list in text-stat to NEWVAL."
  (aset (cdr text-stat) 6 newval))

(defsubst set-text-stat->aux-items (text-stat newval)
  "Set aux-items in text-stat to NEWVAL."
  (aset (cdr text-stat) 7 newval))


;;; Predicate:

(defsubst lyskom-text-stat-p (object)
  "Return t if OBJECT is a text-stat."
  (eq (car-safe object) 'TEXT-STAT))

;;; ================================================================
;;;                            text


;;; Constructor:

(defsubst lyskom-create-text (text-no
			   text-mass)
  "Create a text from all parameters."
  (cons
   'TEXT
   (vector text-no text-mass )))


;;; Selectors:

(defsubst text->text-no (text)
  "Get text-no from text."
  (elt (cdr text) 0))

(defsubst text->text-mass (text)
  "Get text-mass from text."
  (elt (cdr text) 1))


;;; Modifiers:

(defsubst set-text->text-no (text newval)
  "Set text-no in text to NEWVAL."
  (aset (cdr text) 0 newval))

(defsubst set-text->text-mass (text newval)
  "Set text-mass in text to NEWVAL."
  (aset (cdr text) 1 newval))


;;; Predicate:

(defsubst lyskom-text-p (object)
  "Return t if OBJECT is a text."
  (eq (car-safe object) 'TEXT))
				   

;;; ================================================================
;;;                          misc-info


;;; Constructors:

(defsubst lyskom-create-misc-info (type
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
TYPE is one of RECPT, CC-RECPT, BCC-RECPT, COMM-TO, COMM-IN, 
FOOTN-TO or FOOTN-IN."
  (cons
   'MISC-INFO
   (vector type recipient-no local-no rec-time comm-to comm-in 
	   footn-to footn-in sender sent-at )))

(defsubst lyskom-create-empty-misc-info ()
  "Create an empty misc-info."
  (lyskom-create-misc-info nil nil nil nil nil nil nil nil nil nil))


;;; Selectors:

(defsubst misc-info->type (misc-info)
  "Get type from misc-info."
  (elt (cdr misc-info) 0))

(defsubst misc-info->recipient-no (misc-info)
  "Get recipient-no from misc-info."
  (elt (cdr misc-info) 1))

(defsubst misc-info->local-no (misc-info)
  "Get local-no from misc-info."
  (elt (cdr misc-info) 2))

(defsubst misc-info->rec-time (misc-info)
  "Get rec-time from misc-info."
  (elt (cdr misc-info) 3))

(defsubst misc-info->comm-to (misc-info)
  "Get comm-to from misc-info."
  (elt (cdr misc-info) 4))

(defsubst misc-info->comm-in (misc-info)
  "Get comm-in from misc-info."
  (elt (cdr misc-info) 5))

(defsubst misc-info->footn-to (misc-info)
  "Get footn-to from misc-info."
  (elt (cdr misc-info) 6))

(defsubst misc-info->footn-in (misc-info)
  "Get footn-in from misc-info."
  (elt (cdr misc-info) 7))

(defsubst misc-info->sender (misc-info)
  "Get sender from misc-info."
  (elt (cdr misc-info) 8))

(defsubst misc-info->sent-at (misc-info)
  "Get sent-at from misc-info."
  (elt (cdr misc-info) 9))


;;; Modifiers:

(defsubst set-misc-info->type (misc-info newval)
  "Set type in misc-info to NEWVAL."
  (aset (cdr misc-info) 0 newval))

(defsubst set-misc-info->recipient-no (misc-info newval)
  "Set recipient-no in misc-info to NEWVAL."
  (aset (cdr misc-info) 1 newval))

(defsubst set-misc-info->local-no (misc-info newval)
  "Set local-no in misc-info to NEWVAL."
  (aset (cdr misc-info) 2 newval))

(defsubst set-misc-info->rec-time (misc-info newval)
  "Set rec-time in misc-info to NEWVAL."
  (aset (cdr misc-info) 3 newval))

(defsubst set-misc-info->comm-to (misc-info newval)
  "Set comm-to in misc-info to NEWVAL."
  (aset (cdr misc-info) 4 newval))

(defsubst set-misc-info->comm-in (misc-info newval)
  "Set comm-in in misc-info to NEWVAL."
  (aset (cdr misc-info) 5 newval))

(defsubst set-misc-info->footn-to (misc-info newval)
  "Set footn-to in misc-info to NEWVAL."
  (aset (cdr misc-info) 6 newval))

(defsubst set-misc-info->footn-in (misc-info newval)
  "Set footn-in in misc-info to NEWVAL."
  (aset (cdr misc-info) 7 newval))

(defsubst set-misc-info->sender (misc-info newval)
  "Set sender in misc-info to NEWVAL."
  (aset (cdr misc-info) 8 newval))

(defsubst set-misc-info->sent-at (misc-info newval)
  "Set sent-at in misc-info to NEWVAL."
  (aset (cdr misc-info) 9 newval))


;;; Predicate:

(defsubst lyskom-misc-info-p (object)
  "Return t if OBJECT is a misc-info."
  (eq (car-safe object) 'MISC-INFO))


;;; ================================================================
;;;                                time


;;; Constructor:

(defsubst lyskom-create-time (sec
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

(defsubst time->sec (time)
  "Get sec from time."
  (elt (cdr time) 0))

(defsubst time->min (time)
  "Get min from time."
  (elt (cdr time) 1))

(defsubst time->hour (time)
  "Get hour from time."
  (elt (cdr time) 2))

(defsubst time->mday (time)
  "Get mday from time."
  (elt (cdr time) 3))

(defsubst time->mon (time)
  "Get mon from time."
  (elt (cdr time) 4))

(defsubst time->year (time)
  "Get year from time."
  (elt (cdr time) 5))

(defsubst time->wday (time)
  "Get wday from time."
  (elt (cdr time) 6))

(defsubst time->yday (time)
  "Get yday from time."
  (elt (cdr time) 7))

(defsubst time->isdst (time)
  "Get isdst from time."
  (elt (cdr time) 8))


;;; Predicate:

(defsubst lyskom-time-p (object)
  "Return t if OBJECT is a time."
  (eq (car-safe object) 'TIME))


;;; ================================================================
;;;                               privs


;;; Constructor:

(defsubst lyskom-create-privs (wheel
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

(defsubst privs->wheel (privs)
  "Get wheel from privs."
  (elt (cdr privs) 0))

(defsubst privs->admin (privs)
  "Get admin from privs."
  (elt (cdr privs) 1))

(defsubst privs->statistic (privs)
  "Get statistic from privs."
  (elt (cdr privs) 2))

(defsubst privs->create_pers (privs)
  "Get create_pers from privs."
  (elt (cdr privs) 3))

(defsubst privs->create_conf (privs)
  "Get create_conf from privs."
  (elt (cdr privs) 4))

(defsubst privs->change_name (privs)
  "Get change_name from privs."
  (elt (cdr privs) 5))

(defsubst privs->flg7 (privs)
  "Get flg7 from privs."
  (elt (cdr privs) 6))

(defsubst privs->flg8 (privs)
  "Get flg8 from privs."
  (elt (cdr privs) 7))

(defsubst privs->flg9 (privs)
  "Get flg9 from privs."
  (elt (cdr privs) 8))

(defsubst privs->flg10 (privs)
  "Get flg10 from privs."
  (elt (cdr privs) 9))

(defsubst privs->flg11 (privs)
  "Get flg11 from privs."
  (elt (cdr privs) 10))

(defsubst privs->flg12 (privs)
  "Get flg12 from privs."
  (elt (cdr privs) 11))

(defsubst privs->flg13 (privs)
  "Get flg13 from privs."
  (elt (cdr privs) 12))

(defsubst privs->flg14 (privs)
  "Get flg14 from privs."
  (elt (cdr privs) 13))

(defsubst privs->flg15 (privs)
  "Get flg15 from privs."
  (elt (cdr privs) 14))

(defsubst privs->flg16 (privs)
  "Get flg16 from privs."
  (elt (cdr privs) 15))


;;; Modifiers:

(defsubst set-privs->wheel (privs newval)
  "Set wheel in privs to NEWVAL."
  (aset (cdr privs) 0 newval))

(defsubst set-privs->admin (privs newval)
  "Set admin in privs to NEWVAL."
  (aset (cdr privs) 1 newval))

(defsubst set-privs->statistic (privs newval)
  "Set statistic in privs to NEWVAL."
  (aset (cdr privs) 2 newval))

(defsubst set-privs->create_pers (privs newval)
  "Set create_pers in privs to NEWVAL."
  (aset (cdr privs) 3 newval))

(defsubst set-privs->create_conf (privs newval)
  "Set create_conf in privs to NEWVAL."
  (aset (cdr privs) 4 newval))

(defsubst set-privs->change_name (privs newval)
  "Set change_name in privs to NEWVAL."
  (aset (cdr privs) 5 newval))

(defsubst set-privs->flg7 (privs newval)
  "Set flg7 in privs to NEWVAL."
  (aset (cdr privs) 6 newval))

(defsubst set-privs->flg8 (privs newval)
  "Set flg8 in privs to NEWVAL."
  (aset (cdr privs) 7 newval))

(defsubst set-privs->flg9 (privs newval)
  "Set flg9 in privs to NEWVAL."
  (aset (cdr privs) 8 newval))

(defsubst set-privs->flg10 (privs newval)
  "Set flg10 in privs to NEWVAL."
  (aset (cdr privs) 9 newval))

(defsubst set-privs->flg11 (privs newval)
  "Set flg11 in privs to NEWVAL."
  (aset (cdr privs) 10 newval))

(defsubst set-privs->flg12 (privs newval)
  "Set flg12 in privs to NEWVAL."
  (aset (cdr privs) 11 newval))

(defsubst set-privs->flg13 (privs newval)
  "Set flg13 in privs to NEWVAL."
  (aset (cdr privs) 12 newval))

(defsubst set-privs->flg14 (privs newval)
  "Set flg14 in privs to NEWVAL."
  (aset (cdr privs) 13 newval))

(defsubst set-privs->flg15 (privs newval)
  "Set flg15 in privs to NEWVAL."
  (aset (cdr privs) 14 newval))

(defsubst set-privs->flg16 (privs newval)
  "Set flg16 in privs to NEWVAL."
  (aset (cdr privs) 15 newval))


;;; Predicate:

(defsubst lyskom-privs-p (object)
  "Return t if OBJECT is a privs."
  (eq (car-safe object) 'PRIVS))


;;; ================================================================
;;;                            flags



(def-komtype session-flags
  invisible user_active_used user_absent
  reserved3 reserved4 reserved5 reserved6 reserved7)

(def-komtype dynamic-session-info
  session person working-conference idle-time flags what-am-i-doing)

(def-komtype static-session-info
  username hostname ident-user connection-time)

;;; ================================================================
;;;                            flags


;;; Constructor:

(defsubst lyskom-create-flags (unread_is_secret
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
   (vector unread_is_secret flg2 flg3 flg4 flg5 flg6 flg7 flg8)))


;;; Selectors:

(defsubst flags->unread_is_secret (flags)
  "Get unread_is_secret from flags."
  (elt (cdr flags) 0))

(defsubst flags->flg2 (flags)
  "Get flg2 from flags."
  (elt (cdr flags) 1))

(defsubst flags->flg3 (flags)
  "Get flg3 from flags."
  (elt (cdr flags) 2))

(defsubst flags->flg4 (flags)
  "Get flg4 from flags."
  (elt (cdr flags) 3))

(defsubst flags->flg5 (flags)
  "Get flg5 from flags."
  (elt (cdr flags) 4))

(defsubst flags->flg6 (flags)
  "Get flg6 from flags."
  (elt (cdr flags) 5))

(defsubst flags->flg7 (flags)
  "Get flg7 from flags."
  (elt (cdr flags) 6))

(defsubst flags->flg8 (flags)
  "Get flg8 from flags."
  (elt (cdr flags) 7))


;;; Modifiers:

(defsubst set-flags->unread_is_secret (flags newval)
  "Set unread_is_secret in flags to NEWVAL."
  (aset (cdr flags) 0 newval))

(defsubst set-flags->flg2 (flags newval)
  "Set flg2 in flags to NEWVAL."
  (aset (cdr flags) 1 newval))

(defsubst set-flags->flg3 (flags newval)
  "Set flg3 in flags to NEWVAL."
  (aset (cdr flags) 2 newval))

(defsubst set-flags->flg4 (flags newval)
  "Set flg4 in flags to NEWVAL."
  (aset (cdr flags) 3 newval))

(defsubst set-flags->flg5 (flags newval)
  "Set flg5 in flags to NEWVAL."
  (aset (cdr flags) 4 newval))

(defsubst set-flags->flg6 (flags newval)
  "Set flg6 in flags to NEWVAL."
  (aset (cdr flags) 5 newval))

(defsubst set-flags->flg7 (flags newval)
  "Set flg7 in flags to NEWVAL."
  (aset (cdr flags) 6 newval))

(defsubst set-flags->flg8 (flags newval)
  "Set flg8 in flags to NEWVAL."
  (aset (cdr flags) 7 newval))



;;; Predicate:

(defsubst lyskom-flags-p (object)
  "Return t if OBJECT is a flags."
  (eq (car-safe object) 'FLAGS))


;;; ================================================================
;;;                             membership


;;; Constructor:

(def-komtype member-list members)

(def-komtype member pers-no created-by created-at membership-type)

(def-komtype membership-type invitation passive secret rsv1 
  rsv2 rsv3 rsv4 rsv5)

(defsubst lyskom-create-membership ( position
                                     last-time-read
                                     conf-no
                                     priority
                                     last-text-read
                                     read-texts
                                     created-by
                                     created-at
                                     type
                                     )
  "Create a membership from all parameters."
  (cons
   'MEMBERSHIP
   (vector last-time-read conf-no priority last-text-read read-texts 
           created-by created-at type position
	   )))


;;; Selectors:

(defsubst membership->last-time-read (membership)
  "Get last-time-read from membership."
  (elt (cdr membership) 0))

(defsubst membership->conf-no (membership)
  "Get conf-no from membership."
  (elt (cdr membership) 1))

(defsubst membership->priority (membership)
  "Get priority from membership."
  (elt (cdr membership) 2))

(defsubst membership->last-text-read (membership)
  "Get last-text-read from membership."
  (elt (cdr membership) 3))

(defsubst membership->read-texts (membership)
  "Get read-texts from membership."
  (elt (cdr membership) 4))

(defsubst membership->created-by (membership)
  "Get created-by from membership"
  (elt (cdr membership) 5))

(defsubst membership->created-at (membership)
  "Get created-by from membership"
  (elt (cdr membership) 6))

(defsubst membership->type (membership)
  "Get type from membership"
  (elt (cdr membership) 7))

(defsubst membership->position (membership)
  "Get position from membership, if known"
  (elt (cdr membership) 8))



;;; Modifiers:

(defsubst set-membership->last-time-read (membership newval)
  "Set last-time-read in membership to NEWVAL."
  (aset (cdr membership) 0 newval))

(defsubst set-membership->conf-no (membership newval)
  "Set conf-no in membership to NEWVAL."
  (aset (cdr membership) 1 newval))

(defsubst set-membership->priority (membership newval)
  "Set priority in membership to NEWVAL."
  (aset (cdr membership) 2 newval))

(defsubst set-membership->last-text-read (membership newval)
  "Set last-text-read in membership to NEWVAL."
  (aset (cdr membership) 3 newval))

(defsubst set-membership->read-texts (membership newval)
  "Set read-texts in membership to NEWVAL."
  (aset (cdr membership) 4 newval))

(defsubst set-membership->created-by (membership newval)
  "Set created-by in membership to NEWVAL."
  (aset (cdr membership) 5 newval))

(defsubst set-membership->created-at (membership newval)
  "Set type in membership to NEWVAL."
  (aset (cdr membership) 6 newval))

(defsubst set-membership->type (membership newval)
  "Set type in membership to NEWVAL."
  (aset (cdr membership) 7 newval))

(defsubst set-membership->position (membership newval)
  "Set position in membership to NEWVAL."
  (aset (cdr membership) 8 newval))


;;; Predicate:

(defsubst lyskom-membership-p (object)
  "Return t if OBJECT is a membership."
  (eq (car-safe object) 'MEMBERSHIP))

;;; Special stuff

(defun lyskom-member-list-find-member (person members)
  (lyskom-traverse member (member-list->members members)
    (when (eq person (member->pers-no member))
      (lyskom-traverse-break member))))



;;; ================================================================
;;;                               map


;;; Constructor:

(defsubst lyskom-create-map (first-local text-nos)
  "Create a map from all parameters."
  (cons
   'MAP
   (vector first-local text-nos )))


;;; Selectors:

(defsubst map->first-local (map)
  "Get first-local from map."
  (elt (cdr map) 0))

(defsubst map->text-nos (map)
  "Get text-nos from map."
  (elt (cdr map) 1))


;;; Modifiers:

(defsubst set-map->first-local (map newval)
  "Set first-local in map to NEWVAL."
  (aset (cdr map) 0 newval))

(defsubst set-map->text-nos (map newval)
  "Set text-nos in map to NEWVAL."
  (aset (cdr map) 1 newval))


;;; Predicate:

(defsubst lyskom-map-p (object)
  "Return t if OBJECT is a map."
  (eq (car-safe object) 'MAP))

			  
;;; Concat:

(defsubst lyskom-map-concat (&rest maps)
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


;;; ============================================================
;;; Local to global mapping
;;; - Sparse map
;;; - Text mapping

(def-komtype sparse-map mapping)
(def-komtype text-mapping have-more type mapping)


;;; ================================================================
;;;                            mark


;;; Constructor:

(defsubst lyskom-create-mark (text-no
			   mark-type)
  "Create a mark from all parameters."
  (cons
   'MARK
   (vector text-no mark-type )))


;;; Selectors:

(defsubst mark->text-no (mark)
  "Get text-no from mark."
  (elt (cdr mark) 0))

(defsubst mark->mark-type (mark)
  "Get mark-type from mark."
  (elt (cdr mark) 1))


;;; Modifiers:

(defsubst set-mark->text-no (mark newval)
  "Set text-no in mark to NEWVAL."
  (aset (cdr mark) 0 newval))

(defsubst set-mark->mark-type (mark newval)
  "Set mark-type in mark to NEWVAL."
  (aset (cdr mark) 1 newval))


;;; Predicate:

(defsubst lyskom-mark-p (object)
  "Return t if OBJECT is a mark."
  (eq (car-safe object) 'MARK))


;;; ================================================================
;;;                           who-info


;;; Constructor:

(defsubst lyskom-create-who-info (pers-no
			       working-conf
			       connection
			       doing-what
			       username
                               &optional hostname ident-user)
  "Create a who-info from all parameters."
  (cons
   'WHO-INFO
   (vector pers-no working-conf connection doing-what 
           username hostname ident-user
	   )))


;;; Selectors:

(defsubst who-info->pers-no (who-info)
  "Get pers-no from who-info."
  (elt (cdr who-info) 0))

(defsubst who-info->working-conf (who-info)
  "Get working-conf from who-info."
  (elt (cdr who-info) 1))

(defsubst who-info->connection (who-info)
  "Get connection from who-info."
  (elt (cdr who-info) 2))

(defsubst who-info->doing-what (who-info)
  "Get doing-what from who-info."
  (elt (cdr who-info) 3))

(defsubst who-info->username (who-info)
  "Get username from who-info."
  (elt (cdr who-info) 4))

(defsubst who-info->hostname (who-info)
  "Get hostname from who-info."
  (elt (cdr who-info) 5))

(defsubst who-info->ident-user (who-info)
  "Get ident-user from who-info."
  (elt (cdr who-info) 6))


;;; Modifiers:

(defsubst set-who-info->pers-no (who-info newval)
  "Set pers-no in who-info to NEWVAL."
  (aset (cdr who-info) 0 newval))

(defsubst set-who-info->working-conf (who-info newval)
  "Set working-conf in who-info to NEWVAL."
  (aset (cdr who-info) 1 newval))

(defsubst set-who-info->connection (who-info newval)
  "Set connection in who-info to NEWVAL."
  (aset (cdr who-info) 2 newval))

(defsubst set-who-info->doing-what (who-info newval)
  "Set doing-what in who-info to NEWVAL."
  (aset (cdr who-info) 3 newval))

(defsubst set-who-info->username (who-info newval)
  "Set username in who-info to NEWVAL."
  (aset (cdr who-info) 4 newval))

(defsubst set-who-info->hostname (who-info newval)
  "Set hostname in who-info to NEWVAL."
  (aset (cdr who-info) 5 newval))

(defsubst set-who-info->ident-user (who-info newval)
  "Set ident-user in who-info to NEWVAL."
  (aset (cdr who-info) 6 newval))


;;; Predicate:

(defsubst lyskom-who-info-p (object)
  "Return t if OBJECT is a who-info."
  (eq (car-safe object) 'WHO-INFO))

		       
;;; ================================================================
;;;                         session-info


;;; Constructor:

(defsubst lyskom-create-session-info (pers-no
				   working-conf
				   connection
				   doing
				   username
                                   hostname
                                   ident-user
				   idletime
				   connect-time)
  "Create a session-info from all parameters."
  (cons
   'SESSION-INFO
   (vector pers-no working-conf connection doing username idletime 
	   connect-time hostname ident-user)))


;;; Selectors:

(defsubst session-info->pers-no (session-info)
  "Get pers-no from session-info."
  (elt (cdr session-info) 0))

(defsubst session-info->working-conf (session-info)
  "Get working-conf from session-info."
  (elt (cdr session-info) 1))

(defsubst session-info->connection (session-info)
  "Get connection from session-info."
  (elt (cdr session-info) 2))

(defsubst session-info->doing (session-info)
  "Get doing from session-info."
  (elt (cdr session-info) 3))

(defsubst session-info->username (session-info)
  "Get username from session-info."
  (elt (cdr session-info) 4))

(defsubst session-info->idletime (session-info)
  "Get idletime from session-info."
  (elt (cdr session-info) 5))

(defsubst session-info->connect-time (session-info)
  "Get connect-time from session-info."
  (elt (cdr session-info) 6))

(defsubst session-info->hostname (session-info)
  "Get hostname from session-info."
  (elt (cdr session-info) 7))

(defsubst session-info->ident-user (session-info)
  "Get connect-time from session-info."
  (elt (cdr session-info) 8))


;;; Modifiers:

(defsubst set-session-info->pers-no (session-info newval)
  "Set pers-no in session-info to NEWVAL."
  (aset (cdr session-info) 0 newval))

(defsubst set-session-info->working-conf (session-info newval)
  "Set working-conf in session-info to NEWVAL."
  (aset (cdr session-info) 1 newval))

(defsubst set-session-info->connection (session-info newval)
  "Set connection in session-info to NEWVAL."
  (aset (cdr session-info) 2 newval))

(defsubst set-session-info->doing (session-info newval)
  "Set doing in session-info to NEWVAL."
  (aset (cdr session-info) 3 newval))

(defsubst set-session-info->username (session-info newval)
  "Set username in session-info to NEWVAL."
  (aset (cdr session-info) 4 newval))

(defsubst set-session-info->idletime (session-info newval)
  "Set idletime in session-info to NEWVAL."
  (aset (cdr session-info) 5 newval))

(defsubst set-session-info->connect-time (session-info newval)
  "Set connect-time in session-info to NEWVAL."
  (aset (cdr session-info) 6 newval))

(defsubst set-session-info->hostname (session-info newval)
  "Set hostname in session-info to NEWVAL."
  (aset (cdr session-info) 7 newval))

(defsubst set-session-info->ident-user (session-info newval)
  "Set ident-user in session-info to NEWVAL."
  (aset (cdr session-info) 8 newval))



;;; Predicate:

(defsubst lyskom-session-info-p (object)
  "Return t if OBJECT is a session-info."
  (eq (car-safe object) 'SESSION-INFO))


				   
;;; ================================================================
;;;                          conf-type.


;;; Constructor:

(defsubst lyskom-create-conf-type (rd_prot original secret letterbox 
                                           &optional anarchy
                                           forbid-secret rsv2 rsv3)
  "Create a conf-type object. Args: RD_PROT ORIGINAL SECRET LETTERBOX."
  (list 'CONF-TYPE 
	rd_prot 
	original 
	secret 
	letterbox
        anarchy
        forbid-secret
        rsv2
        rsv3
	))


;;;Selectors:

(defsubst conf-type->rd_prot (conf-type)
  "Get rd_prot from conf-type."
  (elt (cdr conf-type) 0))

(defsubst conf-type->original (conf-type)
  "Get original from conf-type."
  (elt (cdr conf-type) 1))

(defsubst conf-type->secret (conf-type)
  "Get secret from conf-type."
  (elt (cdr conf-type) 2))

(defsubst conf-type->letterbox (conf-type)
  "Get letterbox from conf-type."
  (elt (cdr conf-type) 3))

(defsubst conf-type->anarchy (conf-type)
  "Get anarchy from conf-type."
  (elt (cdr conf-type) 4))

(defsubst conf-type->forbid-secret (conf-type)
  "Get reserved bit from conf-type."
  (elt (cdr conf-type) 5))

(defsubst conf-type->rsv2 (conf-type)
  "Get reserved bit from conf-type."
  (elt (cdr conf-type) 5))

(defsubst conf-type->rsv3 (conf-type)
  "Get reserved bit from conf-type."
  (elt (cdr conf-type) 5))


;;; ================================================================
;;;                            text-list


;;; Constructor:

(defsubst lyskom-create-text-list (texts)
  "Create a text-list from all parameters."
  (cons 'TEXT-LIST texts))


;;; Selector:

(defsubst text-list->texts (text-list)
  "Get texts from text-list."
  (cdr text-list))

(defsubst text-list->empty (text-list)
  "Return t if TEXT-LIST is empty."
  (null (cdr text-list)))

(defsubst text-list->length (text-list)
  "Return the length of TEXT-LIST."
  (length (cdr text-list)))


;;; Modifier:

(defsubst set-text-list->texts (text-list newval)
  "Set texts in TEXT-LIST to NEWVAL."
  (setcdr text-list newval))

(defsubst text-list->delq (text-list no)
  "Remove text NO from TEXT-LIST."
  (setcdr text-list (delq no (cdr text-list))))

(defsubst text-list->append (text-list texts)
  "Destructively append TEXTS to the end of TEXT-LIST."
  (setcdr text-list (nconc (cdr text-list) texts)))

;;; Predicate:

(defsubst lyskom-text-list-p (object)
  "Return t if OBJECT is a text-list."
  (eq (car-safe object) 'TEXT-LIST))


;;; ================================================================
;;;                         version-info


;;; Constructor:

(defsubst lyskom-create-version-info (protocol-version
                                      server-software
                                      software-version)
  "Create a version-info from all parameters."
  (cons 'VERSION-INFO
        (vector protocol-version server-software software-version)))

;;; Selectors:

(defsubst version-info->protocol-version (version-info)
  "Get protocol version from version-info."
  (elt (cdr version-info) 0))

(defsubst version-info->server-software (version-info)
  "Get server software name from version-info."
  (elt (cdr version-info) 1))

(defsubst version-info->software-version (version-info)
  "Get server software version from version-info."
  (elt (cdr version-info) 2))

				   
;;; ================================================================
;;;                          server-info


;;; Constructor:

(defsubst lyskom-create-server-info (version
				  conf-pres-conf
				  pers-pres-conf
				  motd-conf
				  kom-news-conf
				  motd-of-lyskom
                                  &optional aux-items)
  "Create a server-info from all parameters."
  (cons
   'SERVER-INFO
   (vector version conf-pres-conf pers-pres-conf motd-conf kom-news-conf 
	   motd-of-lyskom aux-items )))


;;; Selectors:

(defsubst server-info->version (server-info)
  "Get version from server-info."
  (elt (cdr server-info) 0))

(defsubst server-info->conf-pres-conf (server-info)
  "Get conf-pres-conf from server-info."
  (elt (cdr server-info) 1))

(defsubst server-info->pers-pres-conf (server-info)
  "Get pers-pres-conf from server-info."
  (elt (cdr server-info) 2))

(defsubst server-info->motd-conf (server-info)
  "Get motd-conf from server-info."
  (elt (cdr server-info) 3))

(defsubst server-info->kom-news-conf (server-info)
  "Get kom-news-conf from server-info."
  (elt (cdr server-info) 4))

(defsubst server-info->motd-of-lyskom (server-info)
  "Get motd-of-lyskom from server-info."
  (elt (cdr server-info) 5))

(defsubst server-info->aux-item-list (server-info)
  "Get motd-of-lyskom from server-info."
  (elt (cdr server-info) 6))


;;; Modifiers:

(defsubst set-server-info->version (server-info newval)
  "Set version in server-info to NEWVAL."
  (aset (cdr server-info) 0 newval))

(defsubst set-server-info->conf-pres-conf (server-info newval)
  "Set conf-pres-conf in server-info to NEWVAL."
  (aset (cdr server-info) 1 newval))

(defsubst set-server-info->pers-pres-conf (server-info newval)
  "Set pers-pres-conf in server-info to NEWVAL."
  (aset (cdr server-info) 2 newval))

(defsubst set-server-info->motd-conf (server-info newval)
  "Set motd-conf in server-info to NEWVAL."
  (aset (cdr server-info) 3 newval))

(defsubst set-server-info->kom-news-conf (server-info newval)
  "Set kom-news-conf in server-info to NEWVAL."
  (aset (cdr server-info) 4 newval))

(defsubst set-server-info->motd-of-lyskom (server-info newval)
  "Set motd-of-lyskom in server-info to NEWVAL."
  (aset (cdr server-info) 5 newval))

(defsubst set-server-info->aux-item-list (server-info newval)
  "Set motd-of-lyskom in server-info to NEWVAL."
  (aset (cdr server-info) 6 newval))


;;; Predicate:

(defsubst lyskom-server-info-p (object)
  "Return t if OBJECT is a server-info."
  (eq (car-safe object) 'SERVER-INFO))


;;; ================================================================
;;;                            conf-z-info-list


;;; Constructor:

(defun lyskom-create-conf-z-info-list (conf-z-infos)
  "Create a conf-z-info-list from all parameters."
  (cons
   'CONF-Z-INFO-LIST
   (vector conf-z-infos)))


;;; Selector:

(defun conf-z-info-list->conf-z-infos (conf-z-info-list)
  "Get conf-z-infos from conf-z-info-list."
  (elt (cdr conf-z-info-list) 0))


;;; Modifier:

(defun set-conf-z-info-list->conf-z-infos (conf-z-info-list newval)
  "Set conf-z-infos in conf-z-info-list to NEWVAL."
  (aset (cdr conf-z-info-list) 0 newval))


;;; Predicate:

(defun lyskom-conf-z-info-list-p (object)
  "Return t if OBJECT is a conf-z-info-list."
  (eq (car-safe object) 'CONF-Z-INFO-LIST))

;;; ================================================================
;;;                            conf-z-info

;;; Constructor:

(defun lyskom-create-conf-z-info (name
				  conf-type
				  conf-no)
  "Create a conf-z-info from all parameters."
  (cons
   'CONF-Z-INFO
   (vector name conf-type conf-no)))


;;; Selectors:

(defun conf-z-info->name (conf-z-info)
  "Get name from conf-z-info."
  (elt (cdr conf-z-info) 0))

(defun conf-z-info->conf-type (conf-z-info)
  "Get conf-type from conf-z-info."
  (elt (cdr conf-z-info) 1))

(defun conf-z-info->conf-no (conf-z-info)
  "Get conf-no from conf-z-info."
  (elt (cdr conf-z-info) 2))


;;; Modifiers:

(defun set-conf-z-info->name (conf-z-info newval)
  "Set name in conf-z-info to NEWVAL."
  (aset (cdr conf-z-info) 0 newval))

(defun set-conf-z-info->conf-type (conf-z-info newval)
  "Set conf-type in conf-z-info to NEWVAL."
  (aset (cdr conf-z-info) 1 newval))

(defun set-conf-z-info->conf-no (conf-z-info newval)
  "Set conf-no in conf-z-info to NEWVAL."
  (aset (cdr conf-z-info) 2 newval))


;;; Predicate:

(defun lyskom-conf-z-info-p (object)
  "Return t if OBJECT is a conf-z-info."
  (eq (car-safe object) 'CONF-Z-INFO))


;;; ================================================================
;;;                              aux-item

(def-komtype aux-item-flags deleted inherit secret anonymous
  reserved1 reserved2 reserved3 reserved4)

(def-komtype aux-item aux-no 
                       tag
                       creator
                       sent-at
                       flags
                       inherit-limit
                       data)


;;;; ================================================================
;;;; This field is just simulation of a field in the conf-stat
;;;; that not yet exist.

(defsubst conf-stat->comm-conf (conf-stat)
  (if (and (conf-type->original (conf-stat->conf-type conf-stat))
	   (not (zerop (conf-stat->super-conf conf-stat))))
      (conf-stat->super-conf conf-stat)
    (conf-stat->conf-no conf-stat)))


;;; ================================================================



;;; Utilities

(defun text-stat-find-aux (text-stat tag &optional person)
  "Return a list containing the aux items in TEXT-STAT with tag TAG.
If PERSON is non-nil return only those items created by person.
Args: TEXT-STAT TAG PERSON"
  (let ((result nil)
        (items (text-stat->aux-items text-stat)))
    (while items
      (when (and (eq tag (aux-item->tag (car items)))
                 (not (aux-item-flags->deleted
                       (aux-item->flags (car items))))
                 (or (null person)
                     (eq person (aux-item->creator (car items)))))
        (setq result (cons (car items) result)))
      (setq items (cdr items)))
    (nreverse result)))

(defun conf-stat-find-aux (conf-stat tag &optional person)
  "Return a list containing the aux items in CONF-STAT with tag TAG.
If PERSON is non-nil return only those items created by person.
Args: CONF-STAT TAG PERSON"
  (let ((result nil)
        (items (conf-stat->aux-items conf-stat)))
    (while items
      (when (and (eq tag (aux-item->tag (car items)))
                 (not (aux-item-flags->deleted
                       (aux-item->flags (car items))))
                 (or (null person)
                     (eq person (aux-item->creator (car items)))))
        (setq result (cons (car items) result)))
      (setq items (cdr items)))
    (nreverse result)))

(defun lyskom-is-recipient (text-stat conf-no)
  "Return non-nil if TEXT-STAT has CONF-NO as a recipient."
  (let ((result nil))
    (lyskom-traverse misc
        (text-stat->misc-info-list text-stat)
      (when (and (or (eq (misc-info->type misc) 'RECPT)
                     (eq (misc-info->type misc) 'CC-RECPT)
                     (eq (misc-info->type misc) 'BCC-RECPT))
                 (eq (misc-info->recipient-no misc) conf-no))
        (setq result t)))
    result))



(provide 'lyskom-types)
