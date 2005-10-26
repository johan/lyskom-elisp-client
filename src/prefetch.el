;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: prefetch.el,v 44.33 2005-10-26 08:54:28 _cvs_pont_lyskomelisp Exp $
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
;;;; File: prefetch.el
;;;;
;;;; The (new) prefetch system.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: prefetch.el,v 44.33 2005-10-26 08:54:28 _cvs_pont_lyskomelisp Exp $\n"))

(def-kom-var lyskom-prefetch-queue nil
  "A queue where all prefetch requests are entered."
  local)

(defvar lyskom-inhibit-prefetch nil
  "When non-nil, disables the prefetch.")

(def-kom-var lyskom-pending-prefetch 0
  "Variable counting the number of unfinished prefetch requests."
  local)

(defun lyskom-setup-prefetch ()
  "Set up the prefetch system"
  (setq lyskom-prefetch-queue (lyskom-queue-create))
  (setq lyskom-pending-prefetch 0)
  (setq lyskom-inhibit-prefetch nil)
  (setq lyskom-membership-is-read 0))

(defun lyskom-reset-prefetch ()
  "Reset the prefetch system"
  (lyskom-setup-prefetch))

(defsubst lyskom-membership-is-read ()
  "Return t if the while membership list has been fetched, and nil otherwise."
  (eq lyskom-membership-is-read 't))


(defun lyskom-fetch-start-of-map (conf-stat membership)
  "Block fetching map for MEMBERSHIP until we see a text.
Start the prefetch for the remainder of the map."
  (let* ((first-local (1+ (membership->last-text-read membership)))
         (map (blocking-do 'local-to-global
                           (membership->conf-no membership)
                           first-local
                           lyskom-fetch-map-nos)))
    (when map
      (lyskom-enter-map-in-to-do-list map conf-stat membership)
      (when (text-mapping->later-texts-exist map)
        (lyskom-prefetch-map-using-conf-stat conf-stat
                                             (text-mapping->range-end map)
                                             membership)))))

(defun lyskom-prefetch-conf (conf-no)
  "Prefetch the conf-stat for the conference with number CONF-NO."
  (lyskom-queue-enter lyskom-prefetch-queue 
                      `(initiate-get-conf-stat prefetch
                                               lyskom-prefetch-handler 
                                               ,conf-no))
  (lyskom-continue-prefetch))

(defun lyskom-prefetch-pers (pers-no)
  "Prefetch the pers-stat for person with number PERS-NO."
  (lyskom-queue-enter lyskom-prefetch-queue
                      `(initiate-get-pers-stat prefetch
                                               lyskom-prefetch-handler
                                               ,pers-no))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-text (text-no)
  "Prefetch the text-stat for the text with number TEXT-NO."
  (lyskom-queue-enter lyskom-prefetch-queue
                      `(initiate-get-text-stat prefetch
                                               lyskom-prefetch-handler
                                               ,text-no))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-textmass (text-no)
  "Prefetch the text mass for the text with number TEXT-NO."
  (lyskom-queue-enter lyskom-prefetch-queue
                      `(initiate-get-text prefetch
                                          lyskom-prefetch-handler
                                          ,text-no))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-textauth (text-no)
  "Prefetch the text stat and the author of text number TEXT-NO."
  (lyskom-queue-enter lyskom-prefetch-queue
                      `(initiate-get-text-stat prefetch
                                               lyskom-prefetch-textauth-handler
                                               ,text-no))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-text-all (text-no)
  "Prefetch all info about the text with number TEXT-NO."
  (lyskom-queue-enter lyskom-prefetch-queue
                      `(initiate-get-text-stat prefetch
                                               lyskom-prefetch-text-all-handler
                                               ,text-no))
  (lyskom-continue-prefetch))

(defun lyskom-prefetch-text-stat-all (text-stat)
  "Prefetch all info about the text with text stat TEXT-STAT."
  (lyskom-prefetch-text-all-handler text-stat))

(defun lyskom-prefetch-texttree (text-no &optional only-new)
  "Prefetch all info about the text with number TEXT-NO and descends recursively.

If ONLY-NEW is non-nil and the text-stat in question is already
prefetched the prefetch is not done."
  (unless (and only-new (cache-get-text-stat text-no))
    (lyskom-queue-enter lyskom-prefetch-queue
                        `(initiate-get-text-stat prefetch
                                                 lyskom-prefetch-texttree-handler
                                                 ,text-no)))
  (lyskom-continue-prefetch))

(defun lyskom-prefetch-membership (pers-no)
  "Prefetch memberships for PERS-NO."
  (lyskom-queue-enter lyskom-prefetch-queue 
                      (list (lambda (pers-no)
                              (if (numberp lyskom-membership-is-read)
                                  (initiate-get-part-of-membership
                                   'prefetch 'lyskom-prefetch-membership-handler
                                   pers-no lyskom-membership-is-read
                                   lyskom-fetch-membership-length
                                   pers-no)
                                (lyskom-prefetch-handler)))
                            pers-no))
  (lyskom-continue-prefetch))

(defun lyskom-prefetch-one-membership (conf-no pers-no)
  "Prefetch the membership in CONF-NO for PERS-NO."
  (lyskom-queue-enter lyskom-prefetch-queue 
                      `(initiate-query-read-texts prefetch
                                                  lyskom-prefetch-read-texts-handler
                                                  ,pers-no
                                                  ,conf-no
                                                  t
                                                  ,lyskom-max-int
                                                  ,conf-no))
  (lyskom-continue-prefetch))

(defun lyskom-prefetch-map (conf-no membership)
  "Prefetches a map for conf CONF-NO."
  (lyskom-prefetch-map-from conf-no
                            (1+ (membership->last-text-read membership))
                            membership))

(defun lyskom-prefetch-map-from (conf-no first-local membership)
  "Prefetches a map for conf CONFNO starting att FIRST-LOCAL."
  (lyskom-queue-enter lyskom-prefetch-queue
                      `(initiate-get-conf-stat prefetch
                                               lyskom-prefetch-confstatformap-handler
                                               ,conf-no
                                               ,first-local
                                               ,membership))
  (lyskom-continue-prefetch))

(defun lyskom-prefetch-map-using-conf-stat (conf-stat first-local membership)
  "Prefetches a map for conf CONFSTAT starting att FIRST-LOCAL."
  (lyskom-queue-enter lyskom-prefetch-queue
                      `(initiate-local-to-global prefetch
                                                 lyskom-prefetch-map-handler
                                                 ,(conf-stat->conf-no conf-stat)
                                                 ,first-local
                                                 ,lyskom-fetch-map-nos
                                                 ,conf-stat
                                                 ,first-local
                                                 ,membership))
  (lyskom-continue-prefetch))

(defun lyskom-prefetch-texts (texts)
  "Prefetches a list of texts."
  (when texts
    (lyskom-queue-enter lyskom-prefetch-queue
                        `(initiate-get-text-stat prefetch
                                                 lyskom-prefetch-texts-handler
                                                 ,(car texts)
                                                 ,(cdr texts))))
  (lyskom-continue-prefetch))

(defun lyskom-prefetch-cancel-prefetch-map (conf-no &optional queue))


;;; ================================================================
;;; Internal prefetch functions

(defun lyskom-stop-prefetch ()
  "Stop the prefetch process temporarily."
  )

(defun lyskom-start-prefetch ()
  "Start the whole prefetch process"
  (lyskom-continue-prefetch))

(defun lyskom-continue-prefetch ()
  "Called after each prefetch is finished and also when the whole prefetch
process is started. Used to keep prefetch going."
  (unless lyskom-inhibit-prefetch
    (let ((lyskom-inhibit-prefetch t))
      (while (and (< lyskom-pending-prefetch lyskom-prefetch-limit)
                  (lyskom-prefetch-one-item)
                  (++ lyskom-pending-prefetch))))))

(defun lyskom-prefetch-one-item ()
  "Get the first element of the prefetch data structure and fetch it.
Return t if an element was prefetched, otherwise return nil."
  (let ((item (lyskom-queue-delete-first lyskom-prefetch-queue)))
    (cond ((null item) nil)
          ((functionp (car item)) (apply (car item) (cdr item)) t)
          (t (signal 'lyskom-internal-error
                     (list "unknown prefetch key" (car item)))))))



;;; ================================================================
;;; Handlers
;;;

(defun lyskom-prefetch-textauth-handler (text-stat)
  "Prefetch the conf-stat of the author of the text TEXT-STAT."
  (-- lyskom-pending-prefetch)
  (lyskom-stop-prefetch)
  (when text-stat
    (lyskom-prefetch-conf (text-stat->author text-stat)))
  (lyskom-start-prefetch))

(defun lyskom-prefetch-text-all-handler (text-stat)
  "Prefetch all info neccessary to write the text with text-stat TEXT-STAT."
  (-- lyskom-pending-prefetch)
  (lyskom-stop-prefetch)
  (lyskom-prefetch-conf (text-stat->author text-stat))
  (lyskom-prefetch-textmass (text-stat->text-no text-stat))
  (lyskom-traverse misc (text-stat->misc-info-list text-stat)
   (let ((type (misc-info->type misc)))
     (cond
      ((memq type lyskom-recpt-types-list)
       (lyskom-prefetch-conf (misc-info->recipient-no misc)))
      ((eq type 'COMM-IN)
       (lyskom-prefetch-textauth (misc-info->comm-in misc)))
      ((eq type 'FOOTN-IN)
       (lyskom-prefetch-textauth (misc-info->footn-in misc)))
      ((eq type 'COMM-TO)
       (lyskom-prefetch-textauth (misc-info->comm-to misc)))
      ((eq type 'FOOTN-TO)
       (lyskom-prefetch-textauth (misc-info->footn-to misc)))
      (t nil))))
  (lyskom-start-prefetch))



(defun lyskom-prefetch-texttree-handler (text-stat)
  "Prefetch all info neccessary to write the text with text-stat TEXT-STAT.
Then prefetch all info (texttree) of comments."
  (-- lyskom-pending-prefetch)
  (when text-stat
    (lyskom-stop-prefetch)
    (lyskom-prefetch-conf (text-stat->author text-stat))
    (lyskom-prefetch-textmass (text-stat->text-no text-stat))
    (lyskom-traverse misc (text-stat->misc-info-list text-stat)
     (let ((type (misc-info->type misc)))
       (cond
	((memq type lyskom-recpt-types-list)
	 (lyskom-prefetch-conf (misc-info->recipient-no misc)))
	((eq type 'COMM-IN)
	 (lyskom-prefetch-texttree (misc-info->comm-in misc) t))
	((eq type 'FOOTN-IN)
	 (lyskom-prefetch-texttree (misc-info->footn-in misc) t))
	((eq type 'COMM-TO)
	 (lyskom-prefetch-textauth (misc-info->comm-to misc)))
	((eq type 'FOOTN-TO)
	 (lyskom-prefetch-textauth (misc-info->footn-to misc)))
	(t nil)))))
  (lyskom-start-prefetch))


(defun lyskom-prefetch-read-texts-handler (membership pers-no)
  (-- lyskom-pending-prefetch)
  (lyskom-stop-prefetch)
  (when membership
    (let ((old-mship (lyskom-try-get-membership
                      (membership->conf-no membership) t)))
      (if old-mship
          (set-membership->read-texts old-mship
                                      (membership->read-texts membership))
        (lyskom-add-memberships-to-membership (list membership)))
      (when (and (lyskom-visible-membership membership)
                 (lyskom-prefetch-map (membership->conf-no membership)
                                      membership))))
    (lyskom-start-prefetch)))


(defun lyskom-prefetch-membership-handler (memberships pers-no)
  "Handle the return of the membership prefetch call."
  (-- lyskom-pending-prefetch)
  (lyskom-stop-prefetch)
  (lyskom-add-memberships-to-membership memberships)
  (if (and (numberp lyskom-membership-is-read)
           (< (length memberships) lyskom-fetch-membership-length))
        (setq lyskom-membership-is-read t)
    (setq lyskom-membership-is-read (+ lyskom-membership-is-read
                                       lyskom-fetch-membership-length))
    (lyskom-prefetch-membership pers-no))
  (lyskom-start-prefetch))


(defun lyskom-prefetch-confstatformap-handler (conf-stat first-local membership)
  "Now that we have the conf-stat we can fetch the map."
  (-- lyskom-pending-prefetch)
  (lyskom-stop-prefetch)
  (lyskom-prefetch-map-using-conf-stat conf-stat first-local membership)
  (lyskom-start-prefetch))


(defun lyskom-prefetch-map-handler (map conf-stat first-local membership)
  "Handle the return of the membership prefetch call.
Maps are `cached' in lyskom-to-do-list."
  (-- lyskom-pending-prefetch)
  (lyskom-stop-prefetch)
  (let ((next-start (and map (text-mapping->range-end map))))
    (when map
      (when (text-mapping->later-texts-exist map)
	(lyskom-prefetch-map-using-conf-stat conf-stat
                                             next-start
                                             membership))
      (lyskom-enter-map-in-to-do-list map conf-stat membership)))
  (lyskom-start-prefetch)
  (lyskom-update-prompt)
  (lyskom-set-mode-line))


(defun lyskom-prefetch-texts-handler (text-stat texts queue)
  "Prefetch all info neccessary to write the text with text-stat TEXT-STAT."
  (-- lyskom-pending-prefetch)
  (lyskom-stop-prefetch)
  (lyskom-prefetch-conf (text-stat->author text-stat))
  (lyskom-prefetch-textmass (text-stat->text-no text-stat))
  (lyskom-traverse misc (text-stat->misc-info-list text-stat)
    (let ((type (misc-info->type misc)))
      (cond ((memq type lyskom-recpt-types-list)
             (lyskom-prefetch-conf (misc-info->recipient-no misc)))
            ((eq type 'COMM-IN)
             (lyskom-prefetch-textauth (misc-info->comm-in misc)))
            ((eq type 'FOOTN-IN)
             (lyskom-prefetch-textauth (misc-info->footn-in misc)))
            ((eq type 'COMM-TO)
             (lyskom-prefetch-textauth (misc-info->comm-to misc)))
            ((eq type 'FOOTN-TO)
             (lyskom-prefetch-textauth (misc-info->footn-to misc)))
            (t nil))))
  (lyskom-prefetch-texts texts)
  (lyskom-start-prefetch))


(defun lyskom-prefetch-handler (&rest data)
  "Handler called after each simple prefetch request is done."
  (-- lyskom-pending-prefetch)
  (lyskom-continue-prefetch))
