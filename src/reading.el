;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: reading.el,v 44.17 2004-07-11 23:01:04 byers Exp $
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
;;;; File: reading.el
;;;;
;;;; This file contains functions that manage membership and reading
;;;; lists, namely lyskom-membersip lyskom-reading-list and
;;;; lyskom-to-do-list. These are called both from prefetch and from
;;;; startup procedures.
;;;; 

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: reading.el,v 44.17 2004-07-11 23:01:04 byers Exp $\n"))


(defun lyskom-enter-map-in-to-do-list (map conf-stat membership)
  "Takes a MAP and enters all its listed text-nos in the conference CONF-STAT.
This works by modifying the lyskom-to-do-list which in some cases
also means modifying the lyskom-reading-list. The zero text-nos are skipped."
  (let ((list (lyskom-list-unread map membership))
        (mship (lyskom-try-get-membership (conf-stat->conf-no conf-stat))))
    (when (and list mship)
      (read-list-enter-read-info
       (lyskom-create-read-info 
	'CONF conf-stat
	(membership->priority 
	 (lyskom-try-get-membership (conf-stat->conf-no conf-stat)))
	(lyskom-create-text-list
	 list))
       lyskom-to-do-list))))


(defun lyskom-sort-to-do-list ()
  "Sort lyskom-to-do-list in order of membership priorities. 
The priorities for CONF elements are updated to match the membership
priorities. Elements that are not of type CONF appear first on the list
within their priority. This may not be totally accurate, but it's a
reasonable guess."
  (let ((todo (read-list->all-entries lyskom-to-do-list))
        (info nil))

    ;; Update the priorities in the read list

    (while todo
      (setq info (car todo))
      (setq todo (cdr todo))
      (when (eq (read-info->type info) 'CONF)
        (let ((mship 
               (lyskom-get-membership 
                (conf-stat->conf-no (read-info->conf-stat info)) t)))
          (when mship
            (set-read-info->priority info
                                     (membership->priority mship))))))

    ;; Sort the todo list

    (setq lyskom-to-do-list (cons 'READ-LIST
                                  (sort (read-list->all-entries lyskom-to-do-list)
                                        'lyskom-read-info-<)))
    (lyskom-update-prompt)))

(defun lyskom-read-info-< (a b)
  (cond ((< (read-info->priority a) (read-info->priority b)) nil)
        ((> (read-info->priority a) (read-info->priority b)) t)

        ;; Both are confs of equal priority; check position in mship
        ((and (eq (read-info->type a) 'CONF) 
              (eq (read-info->type b) 'CONF))
         (< (lyskom-membership-position 
             (conf-stat->conf-no (read-info->conf-stat a)))
            (lyskom-membership-position 
             (conf-stat->conf-no (read-info->conf-stat b)))))

        ;; A is a CONF and B is not; B is greater.
        ((eq (read-info->type a) 'CONF) nil)

        ;; Both are not CONF, so A is not less than B
        (t t)))



;;; ================================================================
;;; Fundamental membership cache functions

;;; (require 'lyskom-avltree)

(def-kom-var lyskom-mship-cache nil
  "Membership cache. Do not alter directly."
  local)

(defsubst lyskom-mship-cache-index () (aref lyskom-mship-cache 0))
(defsubst lyskom-mship-cache-data  () (aref lyskom-mship-cache 1))

(defun lyskom-mship-cache-create ()
  "Initialize the membership cache to empty."
  (vector (make-hash-table :size 300 :test 'eq) 
          (lyskom-avltree-create 'lyskom-membership-<)))

(defun lyskom-mship-cache-get (conf-no)
  "Get the membership for CONF-NO from the membership cache."
  (gethash conf-no (lyskom-mship-cache-index)))

(defun lyskom-mship-cache-put (mship)
  "Add MSHIP to the membership cache."
  (lyskom-avltree-enter (lyskom-mship-cache-data) mship)
  (puthash (membership->conf-no mship) mship (lyskom-mship-cache-index)))

(defun lyskom-mship-cache-del (conf-no)
  "Delete CONF-NO from the membership cache."
  (let ((mship (lyskom-mship-cache-get conf-no)))
    (when mship
      (lyskom-avltree-delete (lyskom-mship-cache-data) mship)
      (remhash conf-no (lyskom-mship-cache-index)))))

(defun lyskom-update-membership-positions ()
  "Update the position field of all memberships."
  (let ((num 0))
    (lyskom-avltree-traverse (lambda (mship)
                               (set-membership->position mship num)
                               (setq num (1+ num)))
                             (lyskom-mship-cache-data))))

(defun lyskom-add-memberships-to-membership (memberships)
  "Adds a newly fetched MEMBERSHIP-PART to the list in lyskom-membership.
If an item of the membership is already read and entered in the
lyskom-membership list then this item is not entered."
  (lyskom-with-lyskom-buffer
    (lyskom-traverse mship memberships
      (unless (lyskom-mship-cache-get (membership->conf-no mship))
        (lyskom-mship-cache-put mship)))))

(defun lyskom-insert-membership (mship)
  "Add MSHIP into lyskom-membership, sorted by priority."
  (lyskom-with-lyskom-buffer
    (lyskom-mship-cache-put mship)
    (lyskom-update-membership-positions)
    (lp--update-buffer (membership->conf-no mship))))

(defun lyskom-replace-membership (mship)
  "Find the membership for the same conference as MSHIP, and
replace it with MSHIP into lyskom-membership."
  (lyskom-with-lyskom-buffer
    (when (lyskom-mship-cache-get (membership->conf-no mship))
      (lyskom-mship-cache-del (membership->conf-no mship)))
    (lyskom-mship-cache-put mship)
    (lyskom-update-membership-positions)
    (lp--update-buffer (membership->conf-no mship))))

(defun lyskom-remove-membership (conf-no)
  "Remove the membership for CONF-NO from lyskom-membership."
  (lyskom-with-lyskom-buffer
    (lyskom-mship-cache-del conf-no)
    (lyskom-update-membership-positions)
    (lp--update-buffer conf-no)))

(defun lyskom-membership-position (conf-no)
  "Return the position of the membership for CONF-NO."
  (lyskom-with-lyskom-buffer
    (membership->position (lyskom-get-membership conf-no t))))


(defun lyskom-init-membership ()
  "Initialize membership cache and information."
  (setq lyskom-mship-cache
        (lyskom-mship-cache-create)))

(defun lyskom-membership-length ()
  "Return the size of the membership list."
  (lyskom-avltree-size (lyskom-mship-cache-data)))

(defun lyskom-membership-< (a b)
  "Retuns t if A has a higher priority than B. A and B are memberships."
  (cond ((> (membership->priority a)
            (membership->priority b)) t)
        ((and (= (membership->priority a)
                 (membership->priority b))
              (numberp (membership->position a))
              (numberp (membership->position b)))
         (< (membership->position a)
            (membership->position b)))
        (t nil)))

(defun lyskom-get-membership (conf-no &optional want-passive)
  "Return membership for conference CONF-NO.
If optional WANT-PASSIVE is non-nil, also return passive memberships.

If the membership has not been cached, a blocking call is made to the
server to attempt to get it, so this function should not be used from
a callback function."
  (lyskom-with-lyskom-buffer
    (or (lyskom-try-get-membership conf-no want-passive)
        (and (not (lyskom-membership-is-read))
             (let ((mship (blocking-do 'query-read-texts lyskom-pers-no conf-no t 0)))
               (when (and mship (lyskom-visible-membership mship))
                 (lyskom-add-membership mship conf-no))
               (when (or want-passive
                         (not (membership-type->passive (membership->type mship))))
                 mship))))))


(defun lyskom-try-get-membership (conf-no &optional want-passive)
  "Return membership for conference CONF-NO.
If optional WANT-PASSIVE is non-nil, also return passive memberships.

This call does not block. If the membership has not been cached, this
call will return nil." 
  (lyskom-with-lyskom-buffer
    (let ((mship (lyskom-mship-cache-get conf-no)))
      (when (or want-passive (not (membership-type->passive (membership->type mship))))
        mship))))

