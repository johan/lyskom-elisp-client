;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: komtypes.el,v 44.23 2003-01-05 21:37:07 byers Exp $
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
;;;; This file contains primitives for the different data types
;;;; in the lyskom system. All types here have their origin in
;;;; the server. Compare the file clienttypes.el.
;;;;
;;;; Author: ceder
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: komtypes.el,v 44.23 2003-01-05 21:37:07 byers Exp $\n"))


;;; ============================================================
;;; Black magic...

(defmacro def-komtype (type args &optional flags)
  "Define a new type named TYPE with fields ARGS and documentation DOC.
TYPE is the name of the new type; a symbol.
ARGS is a list of arguments. Each element can be a symbol or a cons whose
car is the name of the field and the cdr is a property list for the field.

If optional FLAGS is :nil-safe, then calling accessors on nil object
will return nil and not signal an error.

The special symbol &optional indicates start of fields that will be
optional in the constructor argument list.

Legal fiels properties are:

:default val        In the constructor, if the value supplied for the 
                    field is nil, use VAL instead.
:read-only t        Do not create a mutator. Note that t must be specified.
:automatic val      Do not include field in constructor. Use VAL for
                    the initial field value.
:filter code        Use CODE as a filter for the field value supplied to
                    the constructor.

Only one of :automatic, :filter or :default may be supplied.
"
  (let ((accessors nil)
        (mutators nil)
        (predicate nil)
        (constructor nil)
        (access-method (if (eq ':nil-safe flags) 'elt 'aref))
        (type-sym (intern (upcase (symbol-name type)))))

    ;; Create constructor

    (setq constructor
          `(defsubst ,(intern (format "lyskom-create-%S" type))
             ,(delq nil (mapcar (lambda (arg)
                                  (unless (plist-member (cdr-safe arg) ':automatic)
                                    (or (car-safe arg) arg)))
                                args))
             ,(format "Create a `%S' from arguments.
Automatically created with def-komtype." type)
             (cons ',type-sym
                   (vector
                    ,@(delq '&optional
                            (mapcar 
                             (lambda (arg)
                               (cond ((plist-member (cdr-safe arg) ':automatic)
                                      (plist-get (cdr-safe arg) ':automatic))
                                     ((plist-member (cdr-safe arg) ':filter)
                                      (plist-get (cdr-safe arg) ':filter))
                                     ((plist-get (cdr-safe arg) ':default)
                                      `(or ,(car arg) ,(plist-get (cdr arg) :default)))
                                     (t (or (car-safe arg) arg))
                                     ))
                             args))))))

    ;; Create predicate

    (setq predicate
        `(defsubst ,(intern (format "lyskom-%S-p" type)) (obj)
           ,(format "Return non-nil if OBJ is a %S
Automatically created with def-komtype" type)
           (and (consp obj) (eq (car obj) ',type-sym))))

    ;; Create accessors and mutators

    (let ((field-index 0))
      (mapcar
       (lambda (arg)
         (unless (eq arg '&optional)
           (let ((field (or (car-safe arg) arg)))
             (setq accessors
                   (cons `(defsubst ,(intern (format "%S->%S" type field)) (obj)
                            ,(format "Return field `%s' from OBJ." field)
                            (,access-method (cdr obj) ,field-index))
                         accessors))
             (unless (plist-get (cdr-safe arg) ':read-only)
               (setq mutators
                     (cons `(defsubst ,(intern (format "set-%S->%S" type field)) (obj val)
                              ,(format "Set field `%s' of OBJ to VAL." field)
                              (aset (cdr obj) ,field-index val))
                           mutators))))
           (setq field-index (1+ field-index))))
       args))

    ;; Return the forms

    `(progn ,constructor
            ,predicate
            ,@accessors
            ,@mutators)))




;;; ================================================================
;;;                            conf-no-list


;;; Constructor:

(def-komtype conf-no-list 
  ((conf-nos :filter (cond ((vectorp conf-nos) (append conf-nos nil))
                           (t conf-nos))
             :read-only t))
  :nil-safe)


;;; ================================================================
;;;                            uconf-stat

(def-komtype uconf-stat 
  ((conf-no          :read-only t)
   name
   (conf-type        :read-only t)
   (highest-local-no :read-only t)
   (nice             :read-only t))
  :nil-safe)

;;; ================================================================
;;;                            conf-stat

;;; Constructor:

(def-komtype conf-stat 
  ((conf-no              :read-only t)
   name
   (conf-type            :read-only t)
   (creation-time        :read-only t)
   (last-written         :read-only t)
   (creator              :read-only t)
   (presentation         :read-only t)
   (supervisor           :read-only t)
   (permitted-submitters :read-only t)
   (super-conf           :read-only t)
   (msg-of-day           :read-only t)
   (garb-nice            :read-only t)
   (keep-commented       :read-only t)
   (no-of-members        :read-only t)
   (first-local-no       :read-only t)
   no-of-texts
   &optional
   (expire               :read-only t :default 0)
   (aux-items            :read-only t))
  :nil-safe)


;;; ================================================================
;;;                          conf-type.

(def-komtype conf-type
  ((rd_prot         :read-only t)
   (original        :read-only t)
   (secret          :read-only t)
   (letterbox       :read-only t)
   &optional 
   (anarchy         :read-only t)
   (forbid-secret   :read-only t)
   (rsv2            :read-only t)
   (rsv3            :read-only t))
  :nil-safe)



;;;; This field is just simulation of a field in the conf-stat
;;;; that not yet exist.

(defsubst conf-stat->comm-conf (conf-stat)
  (if (and (conf-type->original (conf-stat->conf-type conf-stat))
	   (not (zerop (conf-stat->super-conf conf-stat))))
      (conf-stat->super-conf conf-stat)
    (conf-stat->conf-no conf-stat)))




;;; ================================================================
;;;                         Conf-list


(def-komtype conf-list
  ((conf-nos    :read-only t) 
   (conf-types  :read-only t))
  :nil-safe)


;;; ================================================================
;;;                             pers-stat


;;; Constructor:

(def-komtype pers-stat 
  ((pers-no                 :read-only t) 
   (username                :read-only t)
   (privileges              :read-only t)
   (flags                   :read-only t)
   (last-login              :read-only t)
   (user-area               :read-only t)
   (total-time-present      :read-only t)
   (sessions                :read-only t)
   (created-lines           :read-only t)
   (created-bytes           :read-only t)
   (read-texts              :read-only t)
   (no-of-text-fetches      :read-only t)
   (created-persons         :read-only t)
   (created-confs           :read-only t)
   (first-created-text      :read-only t)
   (no-of-created-texts     :read-only t)
   (no-of-marks             :read-only t)
   (no-of-confs             :read-only t))
  :nil-safe)


;;; ================================================================
;;;                           text-stat


;;; Constructor:

(def-komtype text-stat 
  ((text-no             :read-only t)
   (creation-time       :read-only t)
   (author              :read-only t)
   (no-of-lines         :read-only t)
   (no-of-chars         :read-only t)
   (no-of-marks         :read-only t)
   (misc-info-list      :read-only t)
   &optional 
   (aux-items           :read-only t))
  :nil-safe)

;;; ================================================================
;;;                              aux-item

(def-komtype aux-item-flags
  ((deleted     :read-only t)
   inherit
   secret
   anonymous
   dont-garb
   (reserved2   :read-only t)
   (reserved3   :read-only t)
   (reserved4   :read-only t))
  :nil-safe)

(def-komtype aux-item 
  ((aux-no          :read-only t)
   (tag             :read-only t)
   (creator         :read-only t)
   (sent-at         :read-only t)
   (flags           :read-only t)
   (inherit-limit   :read-only t)
   data)
  :nil-safe)



;;; ================================================================
;;;                            text

(def-komtype text
  ((text-no         :read-only t)
   text-mass)
  :nil-safe)

;;; Utilities

(defun text->decoded-text-mass (text text-stat)
  "Get the text mass of a text after decoding according to its content type"
  (save-match-data 
    (let* ((str (text->text-mass text))
           (item (lyskom-get-aux-item
                  (text-stat->aux-items text-stat) 1))
           (content-type (and (car item)
                              (lyskom-mime-decode-content-type
                               (aux-item->data (car item))))))
      (if (cdr content-type)
          (lyskom-mime-decode-string str (cdr content-type))
        str))))

				   

;;; ================================================================
;;;                          misc-info


(def-komtype misc-info 
  (type
   recipient-no
   local-no
   rec-time
   comm-to
   comm-in
   footn-to
   footn-in
   sender
   sent-at)
  :nil-safe)

(defsubst lyskom-create-empty-misc-info ()
  "Create an empty misc-info."
  (lyskom-create-misc-info nil nil nil nil nil nil nil nil nil nil))



;;; ================================================================
;;;                                time

(def-komtype time 
  ((sec         :read-only t)
   (min         :read-only t)
   (hour        :read-only t)
   (mday        :read-only t)
   (mon         :read-only t)
   (year        :read-only t)
   (wday        :read-only t)
   (yday        :read-only t)
   (isdst       :read-only t)
   &optional
   (tzhr        :read-only t)
   (tzmin       :read-only t))
  :nil-safe)


;;; ================================================================
;;;                               privs

(def-komtype privs 
  ((wheel       :read-only t)
   (admin       :read-only t)
   (statistic   :read-only t)
   (create_pers :read-only t)
   (create_conf :read-only t)
   (change_name :read-only t)
   (flg7        :read-only t)
   (flg8        :read-only t)
   (flg9        :read-only t)
   (flg10       :read-only t)
   (flg11       :read-only t)
   (flg12       :read-only t)
   (flg13       :read-only t)
   (flg14       :read-only t)
   (flg15       :read-only t)
   (flg16       :read-only t))
  :nil-safe)


;;; ================================================================
;;;                            flags

(def-komtype session-flags
  ((invisible           :read-only t)
   (user_active_used    :read-only t)
   (user_absent         :read-only t)
   (reserved3           :read-only t)
   (reserved4           :read-only t)
   (reserved5           :read-only t)
   (reserved6           :read-only t)
   (reserved7           :read-only t))
  :nil-safe)

(def-komtype dynamic-session-info
  ((session             :read-only t)
   (person              :read-only t)
   (working-conference  :read-only t)
   (idle-time           :read-only t)
   (flags               :read-only t)
   (what-am-i-doing     :read-only t))
  :nil-safe)

(def-komtype static-session-info
  ((username            :read-only t)
   (hostname            :read-only t)
   (ident-user          :read-only t)
   (connection-time     :read-only t))
  :nil-safe)

;;; ================================================================
;;;                            flags

(def-komtype flags
  ((unread_is_secret    :read-only t)
   (flg2                :read-only t)
   (flg3                :read-only t)
   (flg4                :read-only t)
   (flg5                :read-only t)
   (flg6                :read-only t)
   (flg7                :read-only t)
   (flg8                :read-only t))
  :nil-safe)


;;; ================================================================
;;;                             membership


;;; Constructor:

(def-komtype member-list 
  ((members     :read-only t))
  :nil-safe)

(def-komtype member
  ((pers-no             :read-only t)
   (created-by          :read-only t)
   (created-at          :read-only t)
   (membership-type     :read-only t))
  :nil-safe)

(def-komtype membership-type
  (invitation
   passive
   secret
   (rsv1                :read-only t)
   (rsv2                :read-only t)
   (rsv3                :read-only t)
   (rsv4                :read-only t)
   (rsv5                :read-only t))
  :nil-safe)

(def-komtype membership 
  (position 
   (last-time-read      :read-only t)
   (conf-no             :read-only t)
   priority 
   (last-text-read      :read-only t)
   read-texts
   (created-by          :read-only t)
   (created-at          :read-only t)
   (type                :read-only t))
  :nil-safe)

;;; Special stuff

(defun lyskom-member-list-find-member (person members)
  (when members
    (lyskom-traverse member (member-list->members members)
      (when (eq person (member->pers-no member))
        (lyskom-traverse-break member)))))



;;; ================================================================
;;;                               map


(def-komtype map 
  ((first-local         :read-only t)
   (text-nos            :read-only t))
  :nil-safe)

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


;;; ================================================================
;;; Text-Mapping support

(def-komtype text-mapping
  ((range-begin         :read-only t)
   (range-end           :read-only t)
   (size                :read-only t)
   (later-texts-exist   :read-only t)
   (type                :read-only t)
   (block               :read-only t)))

(defsubst lyskom-create-text-pair (local global) (cons local global))
;;UNUSED: text-pair->local-number
(defsubst text-pair->local-number (pair) (car pair))
;;UNUSED: text-pair->global-number
(defsubst text-pair->global-number (pair) (cdr pair))

;;UNUSED: text-mapping->local-to-global
(defun text-mapping->local-to-global (map local)
  (cond ((or (< local (text-mapping->range-begin map))
             (> local (text-mapping->range-end map))) nil)
        ((eq (text-mapping->type map) 'sparse)
         (cdr (assq local (text-mapping->block map))))

        ((eq (text-mapping->type map) 'dense)
         (let ((result (aref (text-mapping->block map)
                             (- local (text-mapping->range-begin map)))))
           (and (not (zerop result)) result)))))

(defun text-mapping->global-numbers (map)
  (cond ((eq (text-mapping->type map) 'sparse)
         (mapcar 'cdr (text-mapping->block map)))
        ((eq (text-mapping->type map) 'dense)
         (let ((result nil))
           (lyskom-traverse el (map->text-nos (text-mapping->block map))
             (unless (zerop el) (setq result (cons el result))))
           (nreverse result)))))

(defun text-mapping->global-to-local (map global)
  (cond ((eq (text-mapping->type map) 'sparse)
         (cdr (rassq global (text-mapping->block map))))
        ((eq (text-mapping->type map) 'dense)
         (let ((i (text-mapping->range-begin map))
               (result nil))
           (while (< i (text-mapping->range-end map))
             (if (eq (aref (map->text-nos (text-mapping->block map)) 
                           (- i (text-mapping->range-begin map)))
                     global)
                 (setq result i i (text-mapping->range-end map))
               (setq i (1+ i))))
           result))))

;;; ================================================================
;;;                            mark

(def-komtype mark 
  ((text-no     :read-only t)
   (mark-type   :read-only t))
  :nil-safe)

;;; Utilities

(defun mark->symbolic-mark-type (mark)
  (lyskom-symbolic-mark-type-string (mark->mark-type mark)))

;;; ================================================================
;;;                           who-info

(def-komtype who-info 
  ((pers-no         :read-only t)
   (working-conf    :read-only t)
   (connection      :read-only t)
   (doing-what      :read-only t)
   (username        :read-only t)
   &optional
   (hostname        :read-only t)
   (ident-user      :read-only t))
  :nil-safe)


;;; ================================================================
;;;                         session-info

(def-komtype session-info 
  ((pers-no         :read-only t)
   (working-conf    :read-only t)
   (connection      :read-only t)
   (doing           :read-only t)
   (username        :read-only t)
   (hostname        :read-only t)
   (ident-user      :read-only t)
   (idletime        :read-only t)
   (connect-time    :read-only t))
  :nil-safe)



;;; ================================================================
;;;                            text-list

;;; Constructor:

(def-komtype text-list (texts)
  :nil-safe)

(defsubst text-list->empty (text-list)
  "Return t if TEXT-LIST is empty."
  (null (text-list->texts text-list)))

(defsubst text-list->length (text-list)
  "Return the length of TEXT-LIST."
  (length (text-list->texts text-list)))

(defsubst text-list->delq (text-list no)
  "Remove text NO from TEXT-LIST."
  (set-text-list->texts text-list
                        (delq no (text-list->texts text-list))))

(defsubst text-list->append (text-list texts)
  "Destructively append TEXTS to the end of TEXT-LIST."
  (set-text-list->texts text-list
                        (nconc (text-list->texts text-list)
                               texts)))


;;; ================================================================
;;;                         version-info


;;; Constructor:

(def-komtype version-info 
  ((protocol-version    :read-only t)
   (server-software     :read-only t)
   (software-version    :read-only t))
  :nil-safe)


;;; ================================================================
;;;                          server-info

(def-komtype server-info 
  ((version         :read-only t)
   (conf-pres-conf  :read-only t)
   (pers-pres-conf  :read-only t)
   (motd-conf       :read-only t)
   (kom-news-conf   :read-only t)
   motd-of-lyskom
   &optional
   (aux-item-list   :read-only t))
  :nil-safe)


;;; ================================================================
;;;                            conf-z-info-list

(def-komtype conf-z-info-list
  ((conf-z-infos :read-only t))
  :nil-safe)


;;; ================================================================
;;;                            conf-z-info

;;; Constructor:

(def-komtype conf-z-info 
  ((name            :read-only t)
   (conf-type       :read-only t)
   (conf-no         :read-only t))
  :nil-safe)


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
      (when (and (memq (misc-info->type misc) lyskom-recpt-types-list)
                 (eq (misc-info->recipient-no misc) conf-no))
        (setq result t)))
    result))



(provide 'lyskom-types)
