;;;;;
;;;;; $Id: reading.el,v 44.3 1997-02-07 18:08:04 byers Exp $
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
;;;; File: reading.el
;;;;
;;;; This file contains functions that manage membership and reading
;;;; lists, namely lyskom-membersip lyskom-reading-list and
;;;; lyskom-to-do-list. These are called both from prefetch and from
;;;; startup procedures.
;;;; 

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: reading.el,v 44.3 1997-02-07 18:08:04 byers Exp $\n"))


(defun lyskom-enter-map-in-to-do-list (map conf-stat membership)
  "Takes a MAP and enters all its listed text-nos in the conference CONF-STAT.
This works by modifying the lyskom-to-do-list which in some cases
also means modifying the lyskom-reading-list. The zero text-nos are skipped."
  (let ((list (lyskom-list-unread map membership)))
    (if (null list)
	nil
      (read-list-enter-read-info
       (lyskom-create-read-info 
	'CONF conf-stat
	(membership->priority 
	 (lyskom-try-get-membership (conf-stat->conf-no conf-stat)))
	(lyskom-create-text-list
	 list))
       lyskom-to-do-list))))


(defun lyskom-add-memberships-to-membership (memberships)
  "Adds a newly fetched MEMBERSHIP-PART to the list in lyskom-membership.
If an item of the membership is already read and entered in the
lyskom-membership list then this item is not entered."
  (let ((list (listify-vector memberships)))
    (while list
      (if (memq (membership->conf-no (car list))
		(mapcar (function membership->conf-no) lyskom-membership))
	  nil
	(setq lyskom-membership (append lyskom-membership (list (car list)))))
      (setq list (cdr list)))))


(defun lyskom-insert-membership (membership membership-list)
  "Add MEMBERSHIP into MEMBERSHIP-LIST, sorted by priority."
  (setq lyskom-membership (sort (cons membership lyskom-membership)
				'lyskom-membership-<)))  


(defun lyskom-replace-membership (membership membership-list)
  "Find the membership for the same conference as MEMBERSHIP, and
replaceit with MEMBERSHIP into MEMBERSHIP-LIST."
  (let ((conf-no (membership->conf-no membership))
	(list lyskom-membership))
    (while list
      (if (= conf-no (membership->conf-no (car list)))
	  (progn
	    (setcar list membership)
	    (setq list nil))
	(setq list (cdr list)))))
  (lyskom-run-hook-with-args 'lyskom-replace-membership-hook
                             membership
                             membership-list))

(defun lyskom-remove-membership (conf-no membership-list)
  "Remove the membership for CONF-NO from MEMBERSHIP-LIST."
  (let ((list lyskom-membership))
    (while list
      (if (= conf-no (membership->conf-no (car list)))
	  (progn
	    (setcar list nil)
	    (setq list nil))
	(setq list (cdr list)))))
  (setq lyskom-membership (delq nil lyskom-membership))
  (lyskom-run-hook-with-args 'lyskom-remove-membership-hook
                             conf-no membership-list))
  
