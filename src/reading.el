;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: reading.el,v 44.14 2002-02-24 20:23:27 joel Exp $
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
	      "$Id: reading.el,v 44.14 2002-02-24 20:23:27 joel Exp $\n"))


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


(defun lyskom-sort-membership ()
  "Sort the internal membership list."
  (setq lyskom-membership (sort lyskom-membership 'lyskom-membership-<))
  (lyskom-update-membership-positions))

(defun lyskom-update-membership-positions ()
  "Update all the position fields in the memberships in the membership list."
  (let ((mship lyskom-membership)
        (num 0))
    (while mship 
      (set-membership->position (car mship) num)
      (setq num (1+ num) mship (cdr mship)))
  ;; FIXME: If something changed, tell the server.
    (lyskom-sort-to-do-list)))

(defun lyskom-add-memberships-to-membership (memberships)
  "Adds a newly fetched MEMBERSHIP-PART to the list in lyskom-membership.
If an item of the membership is already read and entered in the
lyskom-membership list then this item is not entered."
  (save-excursion
    (set-buffer lyskom-buffer)
    (let ((list (listify-vector memberships)))
      (while list
        ;; If membership is already added or passive, don't add it
        (if (memq (membership->conf-no (car list))
                  (mapcar (function membership->conf-no) lyskom-membership))
            nil
          (setq lyskom-membership (append lyskom-membership (list (car list)))))
        (setq list (cdr list))))))

(defun lyskom-insert-memberships-in-membership (memberships)
  (save-excursion
    (set-buffer lyskom-buffer)
    (let ((list (listify-vector memberships)))
      (while list
        ;; If membership is already added or passive, don't add it
        (if (memq (membership->conf-no (car list))
                  (mapcar (function membership->conf-no) lyskom-membership))
            nil
          (setq lyskom-membership (cons (car list) lyskom-membership)))
        (setq list (cdr list))))
    (lyskom-sort-membership)))


(defun lyskom-do-insert-membership (membership)
  (if (membership->position membership)
      (setq lyskom-membership
            (cond ((elt lyskom-membership (membership->position membership))
                   (lyskom-insert-in-list 
                    membership
                    lyskom-membership
                    (elt lyskom-membership
                         (membership->position membership))))
                  ((>= (membership->position membership) 0)
                   (nconc lyskom-membership (list membership)))
                  (t (cons membership lyskom-membership))))
    (let ((mship-list lyskom-membership)
          (found nil))
      (while mship-list
        (when (<= (membership->priority (car mship-list))
                  (membership->priority membership))
          (setq lyskom-membership
                (lyskom-insert-in-list membership
                                       lyskom-membership
                                       (car mship-list))
                mship-list nil
                found t))
        (setq mship-list (cdr mship-list)))
      (unless found (setq lyskom-membership
                          (nconc lyskom-membership (list membership)))))))

(defun lyskom-insert-membership (membership)
  "Add MEMBERSHIP into lyskom-membership, sorted by priority."
  (save-excursion
    (set-buffer lyskom-buffer)
    (lyskom-do-insert-membership membership)
    (lyskom-update-membership-positions)))

(defun lyskom-replace-membership (membership)
  "Find the membership for the same conference as MEMBERSHIP, and
replace it with MEMBERSHIP into lyskom-membership."
  (save-excursion
    (set-buffer lyskom-buffer)
    (when (lyskom-try-get-membership (membership->conf-no membership) t)
      (lyskom-do-remove-membership (membership->conf-no membership))
      (lyskom-do-insert-membership membership)
      (lyskom-run-hook-with-args 'lyskom-replace-membership-hook
                                 membership
                                 lyskom-membership))))

(defun lyskom-do-remove-membership (conf-no)
  "Remove the membership for CONF-NO from lyskom-membership."
  (let ((list lyskom-membership))
    (while list
      (if (= conf-no (membership->conf-no (car list)))
	  (progn
	    (setcar list nil)
	    (setq list nil))
	(setq list (cdr list)))))
  (setq lyskom-membership (delq nil lyskom-membership)))

(defun lyskom-remove-membership (conf-no)
  "Remove the membership for CONF-NO from lyskom-membership."
  (save-excursion
    (set-buffer lyskom-buffer)
    (lyskom-do-remove-membership conf-no)
    (lyskom-run-hook-with-args 'lyskom-remove-membership-hook
                               conf-no lyskom-membership)))
  
(defun lyskom-membership-position (conf-no)
  "Return the position of the membership for CONF-NO."
  (save-excursion
    (set-buffer lyskom-buffer)
    (let ((mship (lyskom-get-membership conf-no t)))
      (or (membership->position mship)
          (- (length (memq mship lyskom-membership))
             (length lyskom-membership))))))


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
