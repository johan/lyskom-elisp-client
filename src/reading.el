;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: reading.el,v 44.20 2004-07-18 14:58:05 byers Exp $
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
	      "$Id: reading.el,v 44.20 2004-07-18 14:58:05 byers Exp $\n"))


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

(def-komtype mship-list-node (prev next data))
(def-komtype membership-list ((head :automatic nil)
                              (tail :automatic nil)
                              (size :automatic 0)))
;;; (def-komtype smship (id priority position))


;;; ----------------------------------------------------------------
;;; INSERTION FUNCTIONS
;;;
;;; There are three functions for inserting memberships into a list.
;;;
;;; Use lyskom-membership-list-insert when inserting memberships in
;;; random order. It uses a heuristic based on the priority of the
;;; membership to determine whether to search from the front of back
;;; of the list.
;;;
;;; Use lyskom-membership-list-append when you know that the position
;;; of the membership is towards the end of the list.
;;;
;;; Use lyskom-membership-list-prepend when you know that the position
;;; of the membership is towards the front of the list.
;;;
;;;
;;; The heuristic is optimized for randomly inserting memberships. If
;;; the list is near-full and a single membership is to be inserted,
;;; other heuristics might perform better. If the approximate position
;;; of the membership is already known, this heuristic is not the best.
;;;

(defun lyskom-membership-list-compare-next (mship next &optional after)
  (and next
       (or (> (membership->priority (mship-list-node->data next))
              (membership->priority mship))
           (and (= (membership->priority (mship-list-node->data next))
                   (membership->priority mship))
                (membership->position mship)
                (if after
                    (>= (membership->position mship)
                        (membership->position (mship-list-node->data next)))
                  (> (membership->position mship)
                     (membership->position (mship-list-node->data next))))))))

(defun lyskom-membership-list-compare-prev (mship prev)
  (and prev
       (or (< (membership->priority (mship-list-node->data prev))
              (membership->priority mship))
           (and (= (membership->priority (mship-list-node->data prev))
                   (membership->priority mship))
                (membership->position mship)
                (<= (membership->position mship)
                    (membership->position (mship-list-node->data prev)))))))  


(defun lyskom-membership-list-insert (mship-list mship)
  "Insert a new membership MSHP into MSHIP-LIST."
  (if (and (membership-list->head mship-list)
           (membership-list->tail mship-list)
           (< (- (membership->priority mship)
                 (membership->priority (mship-list-node->data (membership-list->head mship-list))))
              (- (membership->priority (mship-list-node->data (membership-list->tail mship-list)))
                 (membership->priority mship))))
      (lyskom-membership-list-append mship-list mship)
    (lyskom-membership-list-prepend mship-list mship)))


(defun lyskom-membership-list-prepend (mship-list mship)
  "Insert new membership MSHIP into MSHIP-LIST."
  (let ((cur (membership-list->head mship-list))
        (prev nil))

    ;; Search for the element in the list at which we want to insert
    ;; the new membership.

    (while (lyskom-membership-list-compare-next mship cur)
      (setq prev cur cur (mship-list-node->next cur)))

    (let ((new (lyskom-create-mship-list-node prev cur mship)))

      ;; Set the position of the membership
      ;;
      ;; If it already has a position that is between the positions of cur and prev
      ;; then we do not alter that position. If it has some other position or no
      ;; position, set its position to the position of cur (which is appropriate
      ;; when adding a new membership). We do this even if there is room between
      ;; prev and cur since such a hole probably indicates that we haven't gotten
      ;; the entire membership from the server -- once we have all memberships there
      ;; shouldn't be any holes left.

      (if (or (null (membership->position mship))
              (<= (membership->position mship)
                  (if prev (membership->position (mship-list-node->data prev)) -1))
              (>= (membership->position mship)
                  (if cur (membership->position (mship-list-node->data cur)) lyskom-max-int)))
          (set-membership->position mship
                                (cond (cur (membership->position (mship-list-node->data cur)))
                                      (prev (1+ (membership->position (mship-list-node->data prev))))
                                      (t 0))))

      ;; If cur is nil, then we want to insert at the end of the list
      ;; If prev is nil, then we want to insert at the beginning of the list
      ;; If both are nil, the list is empty and we are inserting the first element

      (if prev
          (set-mship-list-node->next prev new)
        (set-membership-list->head mship-list new))
      (if cur
          (set-mship-list-node->prev cur new)
        (set-membership-list->tail mship-list new))

      (setq prev new)

      ;; If the position we chose for the new element collides with the position of
      ;; the element following it, we adjust the positions of following elements
      ;; until all elements again have unique positions.

      (while (and cur (eq (membership->position (mship-list-node->data prev))
                          (membership->position (mship-list-node->data cur))))
        (set-membership->position (mship-list-node->data cur)
                              (1+ (membership->position (mship-list-node->data cur))))
        (setq prev cur cur (mship-list-node->next cur)))

      (set-membership-list->size mship-list (1+ (membership-list->size mship-list)))
      new)))


(defun lyskom-membership-list-append (mship-list mship)
  "Like lyskom-insert-membership, but searches from the end of the list"
  (let ((cur (membership-list->tail mship-list))
        (prev nil))

    (while (lyskom-membership-list-compare-prev mship cur)
      (setq prev cur cur (mship-list-node->prev cur)))

    (let ((new (lyskom-create-mship-list-node cur prev mship)))

      ;; Set the position of the membership
      ;;
      ;; If it already has a position that is available and between the
      ;; positions of the current and previous elements, then use that.
      ;; If not, use the position of the previous element (after it in
      ;; the list). If there isn't one, use one plus the position of the
      ;; last element in the list. If the list is empty, set position to
      ;; zero.

      (if (or (null (membership->position mship))
              (<= (membership->position mship)
                  (if cur (membership->position (mship-list-node->data cur)) -1))
              (>= (membership->position mship)
                  (if prev (membership->position (mship-list-node->data prev)) lyskom-max-int)))
          (set-membership->position mship
                                (cond (prev (membership->position (mship-list-node->data prev)))
                                      (cur (1+ (membership->position (mship-list-node->data cur))))
                                      (t 0))))

      ;; If cur is nil, then we want to insert at the end of the list
      ;; If prev is nil, then we want to insert at the beginning of the list
      ;; If both are nil, the list is empty and we are inserting the first element

      (if cur
          (set-mship-list-node->next cur new)
        (set-membership-list->head mship-list new))
      (if prev
          (set-mship-list-node->prev prev new)
        (set-membership-list->tail mship-list new))

      ;; Set up for scanning back to the end of the list to adjust positions
      ;; of elements that are after the newly inserted element.

      (setq prev new cur (mship-list-node->next new))

      ;; If the position we chose for the new element collides with the position of
      ;; the element following it, we adjust the positions of following elements
      ;; until all elements again have unique positions.

      (while (and cur (eq (membership->position (mship-list-node->data prev))
                          (membership->position (mship-list-node->data cur))))
        (set-membership->position (mship-list-node->data cur)
                              (1+ (membership->position (mship-list-node->data cur))))
        (setq prev cur cur (mship-list-node->next cur)))

      (set-membership-list->size mship-list (1+ (membership-list->size mship-list)))
      new)))


(defun lyskom-membership-list-delete (mship-list node)
  "Remove NODE from MSHIP-LIST"
  (if (mship-list-node->next node)
      (set-mship-list-node->prev (mship-list-node->next node)
                                 (mship-list-node->prev node))
    (set-membership-list->tail mship-list
                               (mship-list-node->prev node)))

  (if (mship-list-node->prev node)
      (set-mship-list-node->next (mship-list-node->prev node)
                                 (mship-list-node->next node))
    (set-membership-list->head mship-list (mship-list-node->next node)))

  (setq node (mship-list-node->next node))
  (while node
    (set-membership->position (mship-list-node->data node)
                              (1- (membership->position (mship-list-node->data node))))
    (setq node (mship-list-node->next node)))

  (set-membership-list->size mship-list (1- (membership-list->size mship-list))))

(defun lyskom-membership-list-move (mship-list node)
  "Move the node NODE in MSHIP-LIST to its new position."
  (let* ((prev (mship-list-node->prev node))
         (next (mship-list-node->next node))
         (mship (mship-list-node->data node))
         (new-pos nil))

    (cond
     ((lyskom-membership-list-compare-next mship next t) ; Move right
      (setq prev nil)
      (while (lyskom-membership-list-compare-next mship next t)
        (setq new-pos (membership->position (mship-list-node->data next)))
        (set-membership->position (mship-list-node->data next) (1- new-pos))
        (setq prev next next (mship-list-node->next next)))
      (set-membership->position mship new-pos))

     ((lyskom-membership-list-compare-prev mship prev) ; Move left
      (setq next nil)
      (while (lyskom-membership-list-compare-prev mship prev)
        (setq new-pos (membership->position (mship-list-node->data prev)))
        (set-membership->position (mship-list-node->data prev) (1+ new-pos))
        (setq next prev prev (mship-list-node->prev prev)))
      (set-membership->position mship new-pos))

     (t                                 ; Stay in the same place
      (setq prev (mship-list-node->prev node))
      (setq next (mship-list-node->next node))
      (setq new-pos (membership->position mship))))


    ;; Remove the node from its current position

    (if (mship-list-node->prev node)
        (set-mship-list-node->next (mship-list-node->prev node)
                                   (mship-list-node->next node))
      (set-membership-list->head mship-list (mship-list-node->next node)))
    (if (mship-list-node->next node)
        (set-mship-list-node->prev (mship-list-node->next node)
                                   (mship-list-node->prev node))
      (set-membership-list->tail mship-list (mship-list-node->prev node)))

    ;; Splice it into the list at the new position

    (set-mship-list-node->prev node prev)
    (set-mship-list-node->next node next)

    (if prev 
        (set-mship-list-node->next prev node)
      (set-membership-list->head mship-list node))

    (if next
        (set-mship-list-node->prev next node)
      (set-membership-list->tail mship-list node)))
  )



;;; ================================================================
;;; The following functions are concerned with managing the cache

(def-kom-var lyskom-mship-cache nil
  "Membership cache. Do not alter directly."
  local)

(defsubst lyskom-mship-cache-index () (aref lyskom-mship-cache 0))
(defsubst lyskom-mship-cache-data  () (aref lyskom-mship-cache 1))

(defun lyskom-mship-cache-create ()
  "Initialize the membership cache to empty."
  (vector (lyskom-make-hash-table :size 300 :test 'eq) 
          (lyskom-create-membership-list)))

(defun lyskom-mship-cache-get (conf-no)
  "Get the membership for CONF-NO from the membership cache."
  (lyskom-gethash conf-no (lyskom-mship-cache-index)))

(defun lyskom-mship-cache-put (mship)
  "Add MSHIP to the membership cache."
  (lyskom-puthash (membership->conf-no mship)
                  (lyskom-membership-list-insert (lyskom-mship-cache-data) mship)
                  (lyskom-mship-cache-index)))

(defun lyskom-mship-cache-append (mship)
  "Add MSHIP to the membership cache."
  (lyskom-puthash (membership->conf-no mship)
                  (lyskom-membership-list-append (lyskom-mship-cache-data) mship)
                  (lyskom-mship-cache-index)))

(defun lyskom-mship-cache-del (conf-no)
  "Delete CONF-NO from the membership cache."
  (let ((node (lyskom-mship-cache-get conf-no)))
    (when node
      (lyskom-membership-list-delete (lyskom-mship-cache-data) node)
      (lyskom-remhash conf-no (lyskom-mship-cache-index)))))

(defun lyskom-add-memberships-to-membership (memberships)
  "Adds newly fetched MEMBERSHIPS to the membership list."
  (lyskom-with-lyskom-buffer
    (lyskom-traverse mship memberships
      (unless (lyskom-mship-cache-get (membership->conf-no mship))
        (lyskom-mship-cache-append mship)))))

(defun lyskom-insert-membership (mship)
  "Add MSHIP into lyskom-membership, sorted by priority."
  (lyskom-with-lyskom-buffer
    (lyskom-mship-cache-put mship)
    (lp--update-buffer (membership->conf-no mship))))


(defun lyskom-replace-membership (mship)
  "Replace the membership MSHIP."
  (lyskom-with-lyskom-buffer
    (let ((node (lyskom-mship-cache-get (membership->conf-no mship))))
      (if node
          (lyskom-membership-list-move (lyskom-mship-cache-data) node)
        (lyskom-mship-cache-put mship)))
    (lp--update-buffer (membership->conf-no mship))))

(defun lyskom-remove-membership (conf-no)
  "Remove the membership for CONF-NO from lyskom-membership."
  (lyskom-with-lyskom-buffer
    (lyskom-mship-cache-del conf-no)
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
  (membership-list->size (lyskom-mship-cache-data)))

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
      (when (and mship
                 (or want-passive
                     (not (membership-type->passive 
                           (membership->type
                            (mship-list-node->data mship))))))
        (mship-list-node->data mship)))))






;;; ================================================================
;;; Testing


;;;(defun lps (l)
;;;  (let ((cur (membership-list->head l))
;;;        res)
;;;    (while cur
;;;      (setq res (cons (cons (smship->id (mship-list-node->data cur))
;;;                            (smship->position (mship-list-node->data cur)))
;;;                      res)
;;;            cur (mship-list-node->next cur)))
;;;    (nreverse res)))
;;;
;;;
;;;
;;;(progn (setq l (lyskom-create-membership-list))
;;;(setq a (lyskom-create-smship 'a 255 0))
;;;(setq b (lyskom-create-smship 'b 100 1))
;;;(setq c (lyskom-create-smship 'c 100 2))
;;;(setq d (lyskom-create-smship 'd 100 3))
;;;(setq e (lyskom-create-smship 'e  50 4))
;;;(setq f (lyskom-create-smship 'f  50 5))
;;;(setq g (lyskom-create-smship 'g  20 6))
;;;(setq h (lyskom-create-smship 'h   1 7)))
;;;
;;;(setq x (lyskom-create-smship 'x 100 nil))
;;;
;;;(mapcar (lambda (el) (lyskom-membership-list-insert l el)) (list c g f b d a x))

;; (let ((mx 0)
;;       (times nil))
;;   (while (<= mx 500)
;; ;    (garbage-collect)
;;     (message "%d" mx)
;;     (let ((enter-time nil)
;;           (exit-time nil)
;;           (l (lyskom-create-membership-list))
;;           (i 0)
;;           (x 0)
;;           (y 0))
;;       (garbage-collect)
;;       (setq mx (+ mx 10))
;;       (setq enter-time (current-time))
;;       (while (< i mx)
;;         (let* ((val (random mx))
;;                (el (lyskom-create-smship i val (- mx val))))
;;           (lyskom-membership-list-insert l el))
;;         (setq i (1+ i)))
;;       (setq exit-time (current-time))
;;       (princ (format "%05d %.8f %.8f %4d %4d\n"
;;                      mx 
;;                      (elp-elapsed-time enter-time exit-time)
;;                      (/ (elp-elapsed-time enter-time exit-time) mx)
;;                      x
;;                      y))
;;       (sit-for 0)
;;       (setq times (cons (cons mx
;;                               (elp-elapsed-time enter-time exit-time)) times))
;; ))
;;   times)


;; (defun lyskom-map-membership-list (fn list)
;;   (let ((cur (membership-list->tail list))
;;         (res nil))
;;     (while cur
;;       (setq res (cons (funcall fn (mship-list-node->data cur)) res)
;;             cur (mship-list-node->prev cur)))
;;     res))
