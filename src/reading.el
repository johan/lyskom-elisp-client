;;;;;
;;;;; $Id: reading.el,v 44.1 1996-09-29 15:18:50 davidk Exp $
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
	      "$Id: reading.el,v 44.1 1996-09-29 15:18:50 davidk Exp $\n"))


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
	 (lyskom-member-p (conf-stat->conf-no conf-stat)))
	(lyskom-create-text-list
	 list))
       lyskom-to-do-list))))


(defun lyskom-add-membership-to-membership (membership)
  "Adds a newly fetched MEMBERSHIP-PART to the list in lyskom-membership.
If an item of the membership is already read and entered in the
lyskom-membership list then this item is not entered and the variable
lyskom-membership-is-read is decreased by 1."
  (let ((list (listify-vector membership)))
    (while list
      (if (memq (membership->conf-no (car list))
		(mapcar (function membership->conf-no) lyskom-membership))
	  (if (numberp lyskom-membership-is-read)
	      (-- lyskom-membership-is-read))
	(setq lyskom-membership (append lyskom-membership (list (car list)))))
      (setq list (cdr list)))
    (setq list (listify-vector membership))))
