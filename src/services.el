;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: services.el,v 44.49 2004-10-28 18:51:55 byers Exp $
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
;;;; This file contains functions for sending requests to the server
;;;; and parsing the result.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: services.el,v 44.49 2004-10-28 18:51:55 byers Exp $\n"))


;;; ================================================================
;;; Macro for defining services

;;; (defmacro def-kom-service (name args &rest body)
;;;   "Create an initiate call. NAME and ARGS are the name and arguments for the call.
;;; If BODY starts with a string, that is the documentation string.
;;; If BODY consists of (call N PARSER), generate a simple call
;;; for RPC number N, and parse the result with PARSER."
;;;   (let ((function-name (intern (concat "initiate-" (symbol-name name))))
;;;         (doc-string nil)
;;;         (auto-call nil)
;;;         (buffer-save (intern (format "initiate-%s-saved-buffer" name))))
;;;     (when (stringp (car body))
;;;       (setq doc-string (car body))
;;;       (setq body (cdr body)))
;;; 
;;;     (when (and (listp (car body))
;;;                (eq 'call (car (car body))))
;;;       (setq auto-call (car body))
;;;       (setq body (cdr body)))
;;; 
;;;     (` (defun (, function-name) (kom-queue handler (,@ args) &rest data)
;;;          (, 
;;;           (or doc-string 
;;;               (format "Initiate %S on server\nArgs: KOM-QUEUE HANDLER %s &rest DATA"  
;;;                       name (mapconcat 
;;;                             (function
;;;                              (lambda (x)
;;;                                (format "%s" (upcase (symbol-name x)))))
;;;                             args
;;;                             " "))))
;;;          (let (((, buffer-save) (current-buffer)))
;;;            (unwind-protect
;;;                (progn
;;;                  (and (not lyskom-output-queues) (set-buffer lyskom-buffer))
;;;                  (,@ (if auto-call
;;;                          (` ((lyskom-call kom-queue lyskom-ref-no handler data (quote (, (elt auto-call 2))))
;;;                              (lyskom-send-packet kom-queue (lyskom-format-objects (, (elt auto-call 1)) (,@ args)))))
;;;                        body)))
;;;              (set-buffer (, buffer-save))))))))

(defmacro lyskom-server-call (&rest body)
  "Macro to protect initiate-somethings 
from being called in the wrong buffer."
  (` (let ((initiate-something-saved-buffer (current-buffer)))
       (unwind-protect
           (prog2 (or lyskom-output-queues (set-buffer lyskom-buffer))
               lyskom-ref-no
             (,@ body))
         (set-buffer initiate-something-saved-buffer)))))

(put 'lyskom-server-call 'lisp-indent-function 0)
(put 'lyskom-server-call 'edebug-form-spec t)
     

;;; ================================================================
;;;                     Requests for services


(defun initiate-login-old (kom-queue handler pers-no password &rest data)
  "Log in on server.
Args: KOM-QUEUE HANDLER PERS-NO PASSWORD &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 0 pers-no password))))


(defun initiate-login (kom-queue handler pers-no password status 
                                 &rest data)
  "Log in on server.
Args: KOM-QUEUE HANDLER PERS-NO PASSWORD STATUS &rest DATA.
Status is 0 for visible login and 1 for invisible login."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 62 pers-no
                                                         password status))))


(defun initiate-logout (kom-queue handler &rest data)
  "Log out from server.
Args: KOM-QUEUE HANDLER &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 1))))


(defun initiate-pepsi (kom-queue handler conf-no &rest data)
  "Change working conference.
Args: KOM-QUEUE HANDLER CONF-NO &rest DATA."
  (lyskom-server-call
    (let ((mship (lyskom-try-get-membership conf-no t)))
      (when mship
        (set-membership->last-time-read mship (lyskom-current-client-time))))
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 2 conf-no))))

(defun initiate-change-name (kom-queue handler
				       conf-no new-name
				       &rest data)
  "Change the name of a conference.
Args: KOM-QUEUE HANDLER CONF-NO NEW-NAME &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 3 conf-no new-name))))


(defun initiate-change-what-i-am-doing (kom-queue handler what &rest data)
  "Tell server what you are doing.
Args: KOM-QUEUE HANDLER WHAT &rest DATA.
WHAT is a string."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 4 what))))
  

(defun initiate-create-person (kom-queue handler name 
                                         password pers-flags
                                         aux-items 
                                             &rest data)
  "Create a new person.
Args: KOM-QUEUE HANDLER NAME PASSWORD &rest DATA."
  (lyskom-server-call
   (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
   (if (lyskom-have-call 89)
       (lyskom-send-packet kom-queue (lyskom-format-objects 89
                                                            name
                                                            password
                                                            pers-flags
                                                            (cons 'LIST 
                                                                  aux-items)))
     (lyskom-send-packet kom-queue (lyskom-format-objects 5 name password)))))


;;; Call 6 is get-person-stat-old, and is obsoleted by call 49.

(defun initiate-set-priv-bits (kom-queue handler pers-no priv-bits &rest data)
  "Set priv-bits of a person.
Args: KOM-QUEUE HANDLER PERS-NO PRIV-BITS &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 7 pers-no priv-bits))))


(defun initiate-set-passwd (kom-queue handler
				      pers-no old-pw new-pw
				      &rest data)
  "Set the password of a person.
Args: KOM-QUEUE HANDLER PERS-NO OLD-PW NEW-PW &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 8 pers-no old-pw new-pw))))


;;; 
;;; This function has a ridiculous name!  It ought to be called
;;; get-membership.  Unfortunately this name is already taken
;;; by another call.
;;;
(defun initiate-query-read-texts (kom-queue handler
					    pers-no 
                                            conf-no
                                            want-read-ranges
                                            max-ranges
                                            &rest data)
  "Get a membership struct describing the membership of PERS-NO in CONF-NO.
Args: KOM-QUEUE HANDLER PERS-NO CONF-NO &rest DATA"
  (lyskom-server-call
    (cond ((lyskom-have-call 107)
           (lyskom-call kom-queue lyskom-ref-no handler data 
                        'lyskom-parse-membership-11)
           (lyskom-send-packet kom-queue
                               (lyskom-format-objects 107 pers-no conf-no
                                                      want-read-ranges
                                                      max-ranges)))
          ((lyskom-have-call 98)
           (lyskom-call kom-queue lyskom-ref-no handler data 
                        'lyskom-parse-membership-10)
           (lyskom-send-packet kom-queue
                               (lyskom-format-objects 98 pers-no conf-no)))
          (t
           (lyskom-call kom-queue lyskom-ref-no handler data 
                        'lyskom-parse-membership-old)
           (lyskom-send-packet kom-queue
                               (lyskom-format-objects 9 pers-no conf-no))))))


(defun initiate-create-conf (kom-queue handler
				       conf-name conf-type
                                       aux-items &rest data)
  "Add a member to a conference.
Args: KOM-QUEUE HANDLER CONF-NAME CONF-TYPE &rest DATA."
  (lyskom-server-call
   (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
   (if (lyskom-have-call 88)
       (lyskom-send-packet kom-queue
                           (lyskom-format-objects 88 conf-name conf-type
                                                  (cons 'LIST aux-items)))
     (lyskom-send-packet kom-queue
                         (lyskom-format-objects 10 conf-name conf-type)))))


(defun initiate-delete-conf (kom-queue handler conf-no &rest data)
  "Delete a conference.
Args: KOM-QUEUE HANDLER CONF-NO &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 11 conf-no))))



(defun initiate-lookup-name (kom-queue handler name &rest data)
  "See what conferences match NAME.
Args: KOM-QUEUE HANDLER NAME &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data
                 'lyskom-parse-conf-list)
    (lyskom-send-packet kom-queue (lyskom-format-objects 12 name))))
					    

;;; Call 13 is get-conf-stat-old, which is obsoleted by 50.


(defun initiate-add-member (kom-queue handler
				      conf-no 
                                      pers-no
                                      priority
                                      where
                                      type
				      &rest data)
  "Add a member to a conference.
Args: KOM-QUEUE HANDLER CONF-NO PERS-NO PRIORITY WHERE &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (if (lyskom-have-call 100)
        (lyskom-send-packet kom-queue
                            (lyskom-format-objects 100 conf-no pers-no 
                                                   priority where type))
      (lyskom-send-packet kom-queue
                          (lyskom-format-objects 14 conf-no pers-no 
                                                 priority where)))))


(defun initiate-sub-member (kom-queue handler
				      conf-no pers-no
				      &rest data)
  "Subtract a member from a conference.
Args: KOM-QUEUE HANDLER CONF-NO PERS-NO  &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 15 conf-no pers-no))))


(defun initiate-set-presentation (kom-queue handler
					    conf-no text-no
					    &rest data)
  "Set presentation of a conference.
Args: KOM-QUEUE HANDLER CONF-NO TEXT-NO &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 16 conf-no text-no))))


(defun initiate-set-conf-motd (kom-queue handler conf-no text-no &rest data)
  "Set motd of a conference.
Args: KOM-QUEUE HANDLER CONF-NO TEXT-NO &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 17 conf-no text-no))))


(defun initiate-set-supervisor (kom-queue handler conf-no admin
					  &rest data)
  "Set supervisor of a conference.
Args: KOM-QUEUE HANDLER CONF-NO ADMIN &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 18 conf-no admin))))


(defun initiate-set-permitted-submitters (kom-queue handler conf-no perm-sub
						    &rest data)
  "Set permitted submitters of a conference.
Args: KOM-QUEUE HANDLER CONF-NO PERM-SUB &rest DATA.
PERM-SUB is a conference number. All members in that conference might
write texts in CONF-NO. If PERM-SUB is zero everyone is allowed to
write texts in CONF-NO."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 19 conf-no perm-sub))))


(defun initiate-set-super-conf (kom-queue handler conf-no super-conf
					  &rest data)
  "Set superconference of a conference.
Args: KOM-QUEUE HANDLER CONF-NO SUPER-CONF &rest DATA.
Unauthorized attempts to write texts to CONF-NO will bounce to SUPER-CONF."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue 
                        (lyskom-format-objects 20 conf-no super-conf))))


(defun initiate-set-conf-type (kom-queue handler conf-no conf-type &rest data)
  "Set type of a conference.
Args: KOM-QUEUE HANDLER CONF-NO CONF-TYPE &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 21 conf-no conf-type))))


(defun initiate-set-garb-nice (kom-queue handler conf-no garb-nice &rest data)
  "Set garb-nice of a conference.
Args: KOM-QUEUE HANDLER CONF-NO GARB-NICE &rest DATA.
Texts in CONF-NO will live approximately GARB-NICE days."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 22 conf-no garb-nice))))


(defun initiate-get-marks (kom-queue handler &rest data)
  "Get all marked texts.
Args: KOM-QUEUE HANDLER &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-mark-list)
    (lyskom-send-packet kom-queue (lyskom-format-objects 23))))


(defun initiate-mark-text (kom-queue handler
                                     text-no mark-type
                                     &rest data)
  "Mark a text.
Args: KOM-QUEUE HANDLER TEXT-NO MARK-TYPE &rest DATA.
MARK-TYPE is currently a number, but this should maybe be
changed (internally in the elisp-klient) to something similar to
a conf-type (with several bits that are 't' or 'nil' that is)."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 
                         (if (lyskom-have-call 72) 72 24) 
                         text-no mark-type))))

(defun initiate-unmark-text (kom-queue handler
                                     text-no
                                     &rest data)
  "Unmark a text.
Args: KOM-QUEUE HANDLER TEXT-NO &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (if (lyskom-have-call 73)
        (lyskom-send-packet kom-queue (lyskom-format-objects 73 text-no)))
      (lyskom-send-packet kom-queue (lyskom-format-objects 24 text-no 0))))


(defun initiate-get-text (kom-queue handler text-no &rest data)
  "Get text from LysKOM server.
Args: KOM-QUEUE HANDLER TEXT-NO &rest DATA."
  (lyskom-server-call
    (let ((text (cache-get-text text-no)))
      (cond
       ((null text)			;Cached info?
        (lyskom-call kom-queue		;No, ask the server.
                     lyskom-ref-no
                     handler data
                     'lyskom-parse-text text-no)
                                        ;(princ text-no (get-buffer-create "text"))+++
                                        ;(terpri (get-buffer-create "text"))
        (lyskom-send-packet kom-queue (lyskom-format-objects 25 text-no
                                                             0 lyskom-max-int)))
       (t
                                        ;Cached info. 
        (lyskom-call-add kom-queue 'PARSED (lyskom-ref-no) text handler data)
        (lyskom-check-call kom-queue))))))


(defun initiate-get-text-stat (kom-queue handler text-no &rest data)
  "Get text-stat from LysKOM server.
Args: KOM-QUEUE HANDLER TEXT-NO &rest DATA."
  (lyskom-server-call
   (let ((text-stat (cache-get-text-stat text-no)))
     (cond
      ((null text-stat)			;Cached info?
       (lyskom-call kom-queue		;No, ask the server.
                    lyskom-ref-no
                    handler data
                    (if (lyskom-have-call 90)
                        'lyskom-parse-text-stat
                      'lyskom-parse-text-stat-old)
                    text-no)
       ;;(princ text-no (get-buffer-create "text-stat"))+++
       ;;(terpri (get-buffer-create "text-stat"))
       (lyskom-send-packet kom-queue
                           (lyskom-format-objects
                            (if (lyskom-have-call 90) 90 26)
                            text-no)))
      (t
                                        ;Cached info. 
       (lyskom-call-add kom-queue 'PARSED (lyskom-ref-no) text-stat handler data)
       (lyskom-check-call kom-queue))))))


(defun initiate-mark-as-read (kom-queue handler conf-no text-list &rest data)
  "Mark all texts in TEXT-LIST as read in CONF-NO. Args: CONF-NO TEXT-LIST."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 27 conf-no 
                                               (cons 'LIST text-list)))))


(defun initiate-create-text (kom-queue 
                             handler
                             message
                             misc-list
                             aux-items
                             &rest data)
  "Create a new text.
Args: KOM-QUEUE HANDLER MESSAGE MISC-LIST AUX-ITEMS &rest DATA.
MESSAGE is a string. MISC-LIST should be created by lyskom-create-misc-list."
  (lyskom-server-call
   (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
   (if (lyskom-have-call 86)
       (lyskom-send-packet kom-queue
                           (lyskom-format-objects 86
                                                  (cons 'STRING message)
                                                  misc-list
                                                  (cons 'LIST aux-items)))
     (lyskom-send-packet kom-queue
                         (lyskom-format-objects 28 
						(cons 'STRING message) 
						misc-list)))))

				     
(defun initiate-delete-text (kom-queue handler text-no &rest data)
  "Delete a text.
Args: KOM-QUEUE HANDLER TEXT-NO &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 29 text-no))))


(defun initiate-add-recipient (kom-queue handler
					 text-no conf-no type
					 &rest data)
  "Add a recipient to a text.
Args: KOM-QUEUE HANDLER TEXT-NO CONF-NO TYPE &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet 
     kom-queue
     (lyskom-format-objects 30 text-no conf-no
                            (cond ((eq type 'RECPT) 0)
                                  ((eq type 'CC-RECPT) 1)
                                  ((eq type 'BCC-RECPT) 
                                   (if (lyskom-have-feature bcc-misc)
                                       15 1)))))))

(defun initiate-sub-recipient (kom-queue handler
					 text-no conf-no 
					 &rest data)
  "Subtract a recipient from a text.
Args: KOM-QUEUE HANDLER TEXT-NO CONF-NO &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 31 text-no conf-no))))


(defun initiate-add-comment (kom-queue handler
                                       comment-text-no text-no
                                       &rest data)
  "Add a comment to a text.
Args: KOM-QUEUE HANDLER COMMENT-TEXT-NO TEXT-NO &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 32 comment-text-no text-no))))

				       

(defun initiate-sub-comment (kom-queue handler
                                       comment-text-no text-no
                                       &rest data)
  "Subtract a comment from a text.
Args: KOM-QUEUE HANDLER COMMENT-TEXT-NO TEXT-NO &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 33 comment-text-no text-no))))


(defun initiate-get-map (kom-queue handler conf-no first-local
				   no-of-texts &rest data-list)
  "Get mapping from local to global text-nos for CONF-NO from server.
Args: KOM-QUEUE HANDLER CONF-NO FIRST-LOCAL NO-OF-TEXTS DATA-LIST.
Use z-initiate-get-map instead. This function has severe performance losses
with big maps."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data-list 'lyskom-parse-map)
    (lyskom-send-packet kom-queue 
                        (lyskom-format-objects 34 conf-no
                                               first-local no-of-texts))))

(defun z-initiate-get-map  (kom-queue handler conf-no first-local
                                      no-of-texts &rest data)
  "Get mapping from local to global text-nos for CONF-NO from server.
Args: KOM-QUEUE HANDLER CONF-NO FIRST-LOCAL NO-OF-TEXTS &rest DATA.
This function will automatically split fetching of big maps to small
chunks of lyskom-fetch-map-nos texts/chunk if KOM-QUEUE is not already
used to collect a result. This currently gives a big performance gain.
Unfortunately it is impossible (or at least very hard) to do the same
thing when a collect is in progress. This will of course be fixed in
protocol B."
  (lyskom-server-call
    (cond
     ((kom-queue->collect-flag (cdr-safe (assq kom-queue lyskom-call-data)))
      ;; Use oldstyle single big map. Sorry.
      (apply 'initiate-get-map
             kom-queue handler conf-no first-local no-of-texts data))
     (t
      ;; You win.
      (initiate-get-map kom-queue 'lyskom-receive-partial-map conf-no
                        first-local lyskom-fetch-map-nos
                        (+ lyskom-fetch-map-nos first-local)
                        (- no-of-texts lyskom-fetch-map-nos)
                        conf-no nil kom-queue data handler)))))

(defun lyskom-receive-partial-map (map first-local no-of-texts
				       conf-no map-so-far kom-queue
				       data-list handler)
  "Receive a partial map and start fetching a new chunk."
  (lyskom-server-call
    (let ((map-list (nconc map-so-far (list map))))
      (if (<= no-of-texts 0)
          (apply handler (apply 'lyskom-map-concat map-list) data-list)
        (initiate-get-map kom-queue 'lyskom-receive-partial-map conf-no
                          first-local lyskom-fetch-map-nos
                          (+ lyskom-fetch-map-nos first-local)
                          (- no-of-texts lyskom-fetch-map-nos)
                          conf-no map-list kom-queue data-list handler)))))


(defun initiate-get-time (kom-queue handler &rest data)
  "Get time from server.
Args: KOM-QUEUE HANDLER &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data
                 'lyskom-parse-time)
    (lyskom-send-packet kom-queue (lyskom-format-objects 35))))


(defun initiate-get-server-info (kom-queue handler &rest data)
  "Get info about the server"
  (lyskom-server-call
   (lyskom-call kom-queue lyskom-ref-no
                handler
                data
                (if (lyskom-have-call 94)
                    'lyskom-parse-server-info
                  'lyskom-parse-server-info-old))
   (lyskom-send-packet kom-queue
                       (lyskom-format-objects
                        (if (lyskom-have-call 94) 94 36)))))


(defun initiate-add-footnote (kom-queue handler
					footnote-text-no text-no
					&rest data)
  "Add a footnote to a text.
Args: KOM-QUEUE HANDLER FOOTNOTE-TEXT-NO TEXT-NO &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 37 footnote-text-no text-no))))


(defun initiate-sub-footnote (kom-queue handler
					footnote-text-no text-no
					&rest data)
  "Subtract a footnote from a text.
Args: KOM-QUEUE HANDLER FOOTNOTE-TEXT-NO TEXT-NO &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 38 footnote-text-no text-no))))


;;; Call 39, who-is-on-old, is obsoleted by call 63.


(defun initiate-set-unread (kom-queue handler conf-no no-of-unread &rest data)
  "Set number of unread texts in a certain conference.
Args: KOM-QUEUE HANDLER CONF-NO NO-OF-UNREAD &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 40 conf-no no-of-unread))))


(defun initiate-set-motd-of-lyskom (kom-queue handler text-no &rest data)
  "Set message of the day of LysKOM.
Args: KOM-QUEUE HANDLER TEXT-NO &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 41 text-no))))


(defun initiate-enable (kom-queue handler level &rest data)
  "Set security level.
Args: KOM-QUEUE HANDLER LEVEL &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 42 level))))

;;; Call 43 is sync. Starting with version 1.9 of lyskomd it is a
;;; privileged operation, so there is no harm in having the function
;;; easily available any more.

(defun initiate-sync (kom-queue handler &rest data)
  "Sync the LysKOM datbase. This is a prioritized call."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 43))))

;;; Call 44 is shutdown. Use 'kill -HUP' instead.

(defun initiate-shutdown (kom-queue handler parameter &rest data)
  "Shutdown the server.
Args: KOM-QUEUE HANDLER PARAMETER &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 44 parameter))))


(defun initiate-broadcast (kom-queue handler message &rest data)
  "Send a broadcast message to all logged in users.
Args: KOM-QUEUE HANDLER MESSAGE &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 45 message))))



(defun initiate-get-membership (kom-queue handler pers-no &rest data)
  "Get membership-list for PERS-NO from server.
Args: KOM-QUEUE HANDLER PERS-NO &rest DATA."
  (lyskom-server-call
    (cond ((lyskom-have-call 108)
           (lyskom-call kom-queue lyskom-ref-no handler data
                        'lyskom-parse-membership-list-11)
           (lyskom-send-packet 
            kom-queue 
            (lyskom-format-objects 108 pers-no 0 lyskom-max-int
                                   1 lyskom-max-int)))
          ((lyskom-have-call 99)
           (lyskom-call kom-queue lyskom-ref-no handler data
                        'lyskom-parse-membership-list-10)
           (lyskom-send-packet 
            kom-queue 
            (lyskom-format-objects 99 pers-no 0 lyskom-max-int 1)))
          (t (lyskom-call kom-queue lyskom-ref-no handler data
                          'lyskom-parse-membership-list-old)
             (lyskom-send-packet 
              kom-queue 
              (lyskom-format-objects 46 pers-no 0 lyskom-max-int 1))))))


(defun initiate-get-part-of-membership (kom-queue handler pers-no first length
						  &rest data)
  "Get membership-list for PERS-NO from server.
Args: KOM-QUEUE HANDLER PERS-NO FIRST-IN-LIST LENGHT &rest DATA."
  (lyskom-server-call
    (cond ((lyskom-have-call 108)
           (lyskom-call kom-queue lyskom-ref-no handler data
                        'lyskom-parse-membership-list-11)
           (lyskom-send-packet 
            kom-queue 
            (lyskom-format-objects 108 pers-no first length
                                   1 1)))
          ((lyskom-have-call 99)
           (lyskom-call kom-queue lyskom-ref-no handler data
                        'lyskom-parse-membership-list-10)
           (lyskom-send-packet 
            kom-queue 
            (lyskom-format-objects 99 pers-no first length 0)))
          (t (lyskom-call kom-queue lyskom-ref-no handler data
                          'lyskom-parse-membership-list-old)
             (lyskom-send-packet 
              kom-queue 
              (lyskom-format-objects 46 pers-no first length 0))))))


(defun initiate-get-created-texts (kom-queue handler pers-no first-local
					     no-of-texts &rest data)
  "Get a part of the list of created texts for a person.
Args: KOM-QUEUE HANDLER PERS-NO FIRST-LOCAL NO-OF-TEXTS &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data
                 'lyskom-parse-map)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 47 pers-no
                                               first-local no-of-texts))))


(defun initiate-get-members (kom-queue handler conf-no first-local
				       no-of-members &rest data)
  "Get a part of the list of members in a conference.
Args: KOM-QUEUE HANDLER CONF-NO FIRST-LOCAL NO-OF-MEMBERS &rest DATA.
Returns a conf-no-list."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data
                 (if (lyskom-have-call 101)
                     'lyskom-parse-member-list
                   'lyskom-parse-member-list-old))
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 
                         (if (lyskom-have-call 101) 101 48)
                         conf-no
                         first-local no-of-members))))


(defun initiate-get-pers-stat (kom-queue handler pers-no &rest data)
  "Get status for person PERS-NO.
Args: KOM-QUEUE HANDLER PERS-NO &rest DATA."
  (lyskom-server-call
    (let ((pers-stat (cache-get-pers-stat pers-no)))
      (cond
       ((null pers-stat)                ;Cached info?
        (lyskom-call kom-queue		;No, ask the server.
                     lyskom-ref-no
                     handler data
                     'lyskom-parse-pers-stat pers-no)
                                        ;(princ pers-no (get-buffer-create "pers-stat")) +++
                                        ;(terpri (get-buffer-create "pers-stat"))
        (lyskom-send-packet kom-queue (lyskom-format-objects 49 pers-no)))
       (t
                                        ;Cached info. 
        (lyskom-call-add kom-queue 'PARSED (lyskom-ref-no) pers-stat handler data)
        (lyskom-check-call kom-queue))))))

(defun initiate-get-conf-stat (kom-queue handler conf-no &rest data)
  "Get conf-stat from LysKOM server.
Args: KOM-QUEUE HANDLER CONF-NO &rest DATA."
  (lyskom-server-call
   (let ((conf-stat (cache-get-conf-stat conf-no)))
     (cond
      ((zerop conf-no)			;No real user.
       (lyskom-call-add kom-queue 'PARSED (lyskom-ref-no) nil handler data)
       (lyskom-check-call kom-queue))
      ((null conf-stat)			;Cached info?
       (lyskom-call kom-queue		;No, ask the server.
                    lyskom-ref-no
                    handler data
                    (if (lyskom-have-call 91)
                        'lyskom-parse-conf-stat
                      'lyskom-parse-conf-stat-old)
                    conf-no)
       ;;(princ conf-no (get-buffer-create "conf-stat")) +++
       ;;(terpri (get-buffer-create "conf-stat"))
       (lyskom-send-packet kom-queue 
                           (lyskom-format-objects
                            (if (lyskom-have-call 91) 91 50)
                            conf-no)))
      (t
                                        ;Cached info. 
       (lyskom-call-add kom-queue 'PARSED (lyskom-ref-no) conf-stat handler data)
       (lyskom-check-call kom-queue))))))


;; who-is-on is obsoleted by who-is-on-dynamic (83) i protocol version 9
(defun initiate-who-is-on (kom-queue handler &rest data)
  "Ask server who is on.
Args: KOM-QUEUE HANDLER &rest DATA"
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data
                 'lyskom-parse-who-info-list)
    (lyskom-send-packet kom-queue (lyskom-format-objects 51))))


(defun initiate-get-unread-confs (kom-queue handler pers-no &rest data)
  "Return a list of confs that may have unread texts.
Args: KOM-QUEUE HANDLER PERS-NO &rest DATA.
PERS-NO is the number of the person whos confs we are checking."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data
                 'lyskom-parse-conf-no-list)
    (lyskom-send-packet kom-queue (lyskom-format-objects 52 pers-no))))


(defun initiate-send-message (kom-queue handler recipient message &rest data)
  "Send a message to one or all logged in users.
Args: KOM-QUEUE HANDLER RECIPIENT MESSAGE &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 53 recipient message))))


(defun initiate-get-session-info (kom-queue handler session-no &rest data)
  "Ask server for info about a session.
Args: KOM-QUEUE HANDLER SESSION-NO &rest DATA"
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data
                 'lyskom-parse-session-info)
    (lyskom-send-packet kom-queue (lyskom-format-objects 54 session-no))))


(defun initiate-disconnect (kom-queue handler session-no &rest data)
  "Disconnect a session.
Args: KOM-QUEUE HANDLER SESSION &rest DATA"
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data
                 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 55 session-no))))


(defun initiate-who-am-i (kom-queue handler &rest data)
  "Ask the server which connection we are using.
Args: KOM-QUEUE HANDLER &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data
                 'lyskom-parse-num)
    (lyskom-send-packet kom-queue (lyskom-format-objects 56))))

(defun initiate-set-user-area (kom-queue handler pers-no text-no &rest data)
  "Set user-area of a person.
Args: KOM-QUEUE HANDLER PERS-NO TEXT-NO &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 57 pers-no text-no))))


;; WARNING: If you start using this you have to figure out a way to
;; convert local time (before) to UTC. Since this doesn't work at the
;; moment, I have commented this functoun into oblivion.
;;
;; (defun initiate-get-last-text (kom-queue handler before &rest data)
;;   "Get text created before BEFORE.
;; Args: KOM-QUEUE HANDLER BEFORE &rest DATA"
;;   (lyskom-server-call
;;     (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
;;     (lyskom-send-packet kom-queue (lyskom-format-objects 58 before))))


(defun initiate-create-anonymous-text (kom-queue 
                                       handler
                                       message 
                                       misc-list
                                       aux-items
                                       &rest data)
  "Create a new anonymous text.
Args: KOM-QUEUE HANDLER MESSAGE MISC-LIST AUX-ITEMS &rest DATA.
MESSAGE is a string. MISC-LIST should be created by lyskom-create-misc-list."
  (lyskom-server-call
   (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
   (if (lyskom-have-call 87)
       (lyskom-send-packet kom-queue
                           (lyskom-format-objects 87
                                                  (cons 'STRING message)
                                                  misc-list
                                                  (cons 'LIST aux-items)))
     (lyskom-send-packet kom-queue
                         (lyskom-format-objects 59 
                                                (cons 'STRING message)
                                                misc-list)))))


(defun initiate-find-next-text-no (kom-queue handler text-no &rest data)
  "Find the text following the text TEXT-NO"
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
    (lyskom-send-packet kom-queue (lyskom-format-objects 60 text-no))))

(defun initiate-find-previous-text-no (kom-queue handler text-no &rest data)
  "Find the text preceding the text TEXT-NO"
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
    (lyskom-send-packet kom-queue (lyskom-format-objects 61 text-no))))

;; Call 62 is above

(defun initiate-who-is-on-ident (kom-queue handler &rest data)
  "Who is logged on. Obsolete."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 
                 'lyskom-parse-who-info-ident-list)
    (lyskom-send-packet kom-queue (lyskom-format-objects 63))))

(defun initiate-get-session-info-ident (kom-queue handler 
                                                  session-no &rest data)
  "Get session info. Obsolete."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data
                 'lyskom-parse-session-info-ident)
    (lyskom-send-packet kom-queue (lyskom-format-objects 64 session-no))))

(defun initiate-re-lookup-person (kom-queue handler regexp &rest data)
  "Look up person based on regexp. Obsolete."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 
                 'lyskom-parse-number-array)
    (lyskom-send-packet kom-queue (lyskom-format-objects 65 regexp))))

(defun initiate-re-lookup-conf (kom-queue handler regexp &rest data)
  "Look up conference based on regexp. Obsolete."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 
                 'lyskom-parse-number-array)
    (lyskom-send-packet kom-queue (lyskom-format-objects 66 regexp))))

(defun initiate-lookup-person (kom-queue handler regexp &rest data)
  "Look up person based on abbreviated name. Obsolete."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 
                 'lyskom-parse-number-array)
    (lyskom-send-packet kom-queue (lyskom-format-objects 67 regexp))))

(defun initiate-lookup-conf (kom-queue handler regexp &rest data)
  "Look up conference based on abbreviated name. Obsolete."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 
                 'lyskom-parse-number-array)
    (lyskom-send-packet kom-queue (lyskom-format-objects 68 regexp))))

(defun initiate-set-client-version (kom-queue handler name version &rest data)
  "Tell the server to set the client name and version of this session."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 69 name version))))


(defun initiate-get-client-name (kom-queue handler session &rest data)
  "Tell the server to set the highest unread article in conference CONF-NO
to TEXT-NO
Args: KOM-QUEUE HANDLER CONF-NO TEXT-NO &rest DATA"
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-string)
    (lyskom-send-packet kom-queue (lyskom-format-objects 70 session))))


(defun initiate-get-client-version (kom-queue handler session &rest data)
  "Tell the server to set the highest unread article in conference CONF-NO
to TEXT-NO
Args: KOM-QUEUE HANDLER CONF-NO TEXT-NO &rest DATA"
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-string)
    (lyskom-send-packet kom-queue (lyskom-format-objects 71 session))))

(defun initiate-re-z-lookup (kom-queue handler regexp want-persons want-confs
				       &rest data)
  "Perform a regexp lookup.
Args: KOM-QUEUE HANDLER REGEXP WANT-PERSONS WANT-CONFS &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data
                 'lyskom-parse-conf-z-info-list)
    (lyskom-send-packet kom-queue (lyskom-format-objects 74 regexp want-persons
                                                         want-confs))))

(defun initiate-get-version-info (kom-queue handler &rest data)
  "Perform a get-version-info vall.
Args: KOM-QUEUE HANDLER &rest DATA"
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-version-info)
    (lyskom-send-packet kom-queue (lyskom-format-objects 75))))


(defun initiate-lookup-z-name (kom-queue handler name want-persons want-confs
                                         &rest data)
  "Perform a z-lookup.
Args: KOM-QUEUE HANDLER NAME WANT-PERSONS WANT-CONFS &rest DATA"
  (lyskom-server-call
    (if (lyskom-have-call 76)
        (progn
          (lyskom-call kom-queue lyskom-ref-no handler data 
                       'lyskom-parse-conf-z-info-list)
          (lyskom-send-packet kom-queue (lyskom-format-objects 76 name
                                                               want-persons
                                                               want-confs)))
      (let ((ref-no lyskom-ref-no))
        (lyskom-fake-call kom-queue ref-no handler data)
        (++ lyskom-ref-no)
        (initiate-lookup-name 'compat
                              'initiate-compat-lookup-z-name-2
                              name
                              kom-queue
                              ref-no
                              want-persons
                              want-confs)))))

(defun initiate-compat-lookup-z-name-2 (result kom-queue
                                               ref-no
                                               want-persons
                                               want-confs)
  (lyskom-server-call
    (if (null result)
        (lyskom-complete-call kom-queue ref-no nil))

    (let ((conf-nos (listify-vector (conf-list->conf-nos result)))
          (conf-types (listify-vector (conf-list->conf-types result))))
      (lyskom-collect 'follow)
      (while conf-nos
        (if (or (and want-persons
                     (conf-type->letterbox (car conf-types)))
                (and want-confs
                     (not (conf-type->letterbox (car conf-types)))))
            (initiate-get-conf-stat 'follow nil (car conf-nos)))
        (setq conf-nos (cdr conf-nos))
        (setq conf-types (cdr conf-types)))
      (lyskom-list-use 'follow
                       'initiate-compat-lookup-z-name-3
                       kom-queue
                       ref-no))))



(defun initiate-compat-lookup-z-name-3 (conf-list kom-queue 
                                                  ref-no)
  (lyskom-server-call
    (lyskom-complete-call kom-queue ref-no
                          (lyskom-create-conf-z-info-list
                           (mapcar (function
                                    (lambda (conf-stat)
                                      (lyskom-create-conf-z-info
                                       (conf-stat->name conf-stat)
                                       (conf-stat->conf-type conf-stat)
                                       (conf-stat->conf-no conf-stat))))
                                   conf-list)))))
    

(defun initiate-set-last-read (kom-queue handler conf-no text-no &rest data)
  "Tell the server to set the highest unread article in conference CONF-NO
to TEXT-NO
Args: KOM-QUEUE HANDLER CONF-NO TEXT-NO &rest DATA"
  (lyskom-server-call
    (if (lyskom-have-call 77)
        (progn
          (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
          (lyskom-send-packet kom-queue (lyskom-format-objects 77
                                                               conf-no text-no)))
      (initiate-get-conf-stat kom-queue 
                              'initiate-set-last-read-2 
                              conf-no
                              kom-queue
                              handler 
                              conf-no
                              text-no
                              data))))

(defun initiate-set-last-read-2 (conf-stat 
                                 kom-queue
                                 handler
                                 conf-no
                                 text-no
                                 data)
  (lyskom-server-call
    (let ((no-of-unread (- (1- (+ (conf-stat->first-local-no conf-stat)
                                  (conf-stat->no-of-texts conf-stat)))
                           text-no)))
      (if (< no-of-unread 0)
          (setq no-of-unread 0))

      (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
      (lyskom-send-packet kom-queue
                          (lyskom-format-objects 40 conf-no no-of-unread)))))

(defun initiate-get-uconf-stat (kom-queue handler conf-no &rest data)
  "Get an uconf-stat from LysKOM server.
Args: KOM-QUEUE HANDLER CONF-NO &rest DATA."
  (lyskom-server-call
    (let ((conf-stat (cache-get-uconf-stat conf-no)))
      (cond ((zerop conf-no)
             (lyskom-call-add kom-queue 'PARSED (lyskom-ref-no) nil handler data)
             (lyskom-check-call kom-queue))
            ((null conf-stat)
             (lyskom-call kom-queue
                          lyskom-ref-no
                          handler data
                          'lyskom-parse-uconf-stat conf-no)
             (lyskom-send-packet kom-queue (lyskom-format-objects 78 conf-no)))
            (t
             (lyskom-call-add kom-queue 'PARSED (lyskom-ref-no) conf-stat handler data)
             (lyskom-check-call kom-queue))))))

(defun initiate-set-info (kom-queue handler 
                                    conf-pres-conf
                                    pers-pres-conf
                                    motd-conf
                                    kom-news-conf
                                    motd-of-lyskom &rest data)
  "Set server info."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 79
                                                         0
                                                         conf-pres-conf
                                                         pers-pres-conf
                                                         motd-conf
                                                         kom-news-conf
                                                         motd-of-lyskom))))
                                                         

(defun initiate-accept-async (kom-queue handler list &rest data)
  "Request asynchronous messages in LIST
Args: KOM-QUEUE HANDLER LIST &rest DATA"
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 80
                                                         (cons 'LIST
                                                               list)))))

(defun initiate-query-async (kom-queue handler &rest data)
  "Request information on which async messages are being sent.
Args: KOM-QUEUE HANDLER &rest DATA"
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 
                 'lyskom-parse-number-array)
    (lyskom-send-packet kom-queue (lyskom-format-objects 81))))
                                 


(defun initiate-user-active (kom-queue handler &rest data)
  "Notify the server that the user is active
Args: KOM-QUEUE HANDLER &rest DATA."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 82))))


(defun initiate-who-is-on-dynamic (kom-queue handler want-visible
					     want-invisible active-last
					     &rest data)
  "Ask server who is on.
Args: KOM-QUEUE HANDLER WANT-VISIBLE WANT-INVISIBLE ACTIVE_LAST &rest DATA"
  (lyskom-server-call
  (lyskom-call kom-queue lyskom-ref-no handler data
	       'lyskom-parse-dynamic-session-info-list)
  (lyskom-send-packet kom-queue (lyskom-format-objects
				 83 want-visible want-invisible active-last))))


(defun initiate-get-static-session-info (kom-queue handler session-no
						   &rest data)
  "Ask server for info about a session.
Args: KOM-QUEUE HANDLER SESSION-NO &rest DATA"
  (lyskom-server-call
   (let ((info (cache-get-static-session-info session-no)))
     (cond
      ((null info)			; Not cached
       (lyskom-call kom-queue lyskom-ref-no handler data
                    'lyskom-parse-static-session-info session-no)
       (lyskom-send-packet kom-queue (lyskom-format-objects
                                      84 session-no)))
      (t                                ; Cached
       (lyskom-call-add kom-queue 'PARSED (lyskom-ref-no) info handler data)
       (lyskom-check-call kom-queue)))))) ;This might call the handler.


(defun initiate-get-collate-table (kom-queue handler &rest data)
  "Get the collate table from the server."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-string)
    (lyskom-send-packet kom-queue (lyskom-format-objects 85))))

(defun initiate-modify-text-info (kom-queue handler 
                                            text-no
                                            delete-items
                                            add-items
                                            &rest data)
  (lyskom-server-call
   (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
   (lyskom-send-packet kom-queue
                       (lyskom-format-objects 92
                                              text-no
                                              (cons 'LIST delete-items)
                                              (cons 'LIST add-items)))))

(defun initiate-modify-conf-info (kom-queue handler
                                            conf-no
                                            delete-items
                                            add-items
                                            &rest data)
  (lyskom-server-call
   (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
   (lyskom-send-packet kom-queue 
                       (lyskom-format-objects 93
                                              conf-no
                                              (cons 'LIST delete-items)
                                              (cons 'LIST add-items)))))

(defun initiate-modify-server-info (kom-queue handler 
                                              delete-items
                                              add-items
                                              &rest data)
  (lyskom-server-call
   (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
   (lyskom-send-packet kom-queue
                       (lyskom-format-objects 95
                                              (cons 'LIST delete-items)
                                              (cons 'LIST add-items)))))

(defun initiate-query-predefined-aux-items (kom-queue handler &rest data)
  "Send query-predefined-aux-items to the server"
  (lyskom-server-call
   (lyskom-call kom-queue lyskom-ref-no handler data
                'lyskom-parse-number-array)
   (lyskom-send-packet kom-queue (lyskom-format-objects 96))))

(defun initiate-set-expire (kom-queue handler conf-no expire &rest data)
  "Send set-expire to server."
  (lyskom-server-call
   (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
   (lyskom-send-packet kom-queue (lyskom-format-objects 97 conf-no expire))
   (cache-del-conf-stat conf-no)))

(defun initiate-set-membership-type (kom-queue handler pers-no conf-no type &rest data)
  "Send set-membership-type to the server."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue 
                        (lyskom-format-objects 102 pers-no conf-no type))))

(defun initiate-local-to-global (kom-queue handler 
                                           conf-no
                                           first-local-no
                                           no-of-texts &rest data)
  "Send local-to-global to server."
  (lyskom-server-call
    (cond ((lyskom-have-call 103)
           (lyskom-call kom-queue lyskom-ref-no handler data
                        'lyskom-parse-text-mapping no-of-texts)
           (lyskom-send-packet kom-queue
                               (lyskom-format-objects 103 
                                                      conf-no
                                                      first-local-no
                                                      no-of-texts)))
          (t (lyskom-call kom-queue lyskom-ref-no 
                          'lyskom-l2g-fake-callback
                          (cons handler data)
                          'lyskom-parse-map)
                 (lyskom-send-packet kom-queue 
                                     (lyskom-format-objects 34 conf-no
                                                            first-local-no
                                                            no-of-texts))))))

(defun lyskom-l2g-fake-callback (map handler &rest data)
  (let ((mapping 
         (and map (lyskom-create-text-mapping (map->first-local map)
                                              (+ (map->first-local map)
                                                 (length (map->text-nos map)))
                                              (length (map->text-nos map))
                                              t
                                              'dense
                                              map))))
    (apply handler mapping data)))

(defun initiate-map-created-texts (kom-queue handler
                                             author
                                             first-local-no
                                             no-of-texts
                                             &rest data)
  "Send map-created-texts to the server."
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 
                 'lyskom-parse-text-mappinng no-of-texts)
    (lyskom-send-packet kom-queue
                        (lyskom-format-objects 104
                                               author
                                               first-local-no
                                               no-of-texts))))



(defun initiate-set-keep-commented (kom-queue handler conf-no keep &rest data)
  (lyskom-server-call
   (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
   (lyskom-send-packet kom-queue (lyskom-format-objects 105 conf-no keep))
   (cache-del-conf-stat conf-no)))

(defun initiate-set-pers-flags (kom-queue handler pers-no flags &rest data)
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 106 pers-no flags))
    (cache-del-pers-stat pers-no)))

(defun initiate-mark-as-unread (kom-queue handler conf-no text &rest data)
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 109 conf-no text))))

(defun initiate-get-stats-description (kom-queue handler &rest data)
  (lyskom-server-call
    (if lyskom-stats-description
        (progn (lyskom-call-add kom-queue 'PARSED (lyskom-ref-no) lyskom-stats-description
                                handler data)
          (lyskom-check-call kom-queue))
      (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-stats-description)
      (lyskom-send-packet kom-queue (lyskom-format-objects 111)))))

(defun initiate-get-stats (kom-queue handler what &rest data)
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-stats-array)
    (lyskom-send-packet kom-queue (lyskom-format-objects 112 what))))

(defun initiate-get-boottime-info (kom-queue handler &rest data)
  (lyskom-server-call
    (if lyskom-static-server-info
        (progn (lyskom-call-add kom-queue 'PARSED (lyskom-ref-no) lyskom-static-server-info
                                handler data)
          (lyskom-check-call kom-queue))
      (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-static-server-info)
      (lyskom-send-packet kom-queue (lyskom-format-objects 113)))))

(defun initiate-first-unused-conf-no (kom-queue handler &rest data)
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
    (lyskom-send-packet kom-queue (lyskom-format-objects 114))))

(defun initiate-first-unused-text-no (kom-queue handler &rest data)
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
    (lyskom-send-packet kom-queue (lyskom-format-objects 115))))

(defun initiate-find-next-conf-no (kom-queue handler conf-no &rest data)
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
    (lyskom-send-packet kom-queue (lyskom-format-objects 116 conf-no))))

(defun initiate-find-previous-conf-no (kom-queue handler conf-no &rest data)
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-num)
    (lyskom-send-packet kom-queue (lyskom-format-objects 117 conf-no))))

(defun initiate-get-scheduling (kom-queue handler session-no &rest data)
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-scheduling-info)
    (lyskom-send-packet kom-queue (lyskom-format-objects 118 (or session-no 0)))))

(defun initiate-set-scheduling (kom-queue handler session-no priority weight &rest data)
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 119 (or session-no 0) priority weight))))

(defun initiate-set-connection-time-format (kom-queue handler use-utc &rest data)
  (lyskom-server-call
    (lyskom-call kom-queue lyskom-ref-no handler data 'lyskom-parse-void)
    (lyskom-send-packet kom-queue (lyskom-format-objects 120 use-utc))))


;;; ================================================================


;; Blocking reading from server:

(defvar lyskom-blocking-return nil
  "Return from blocking-do.")

(defun blocking-return (retval)
  "Sets blocking variable."
  (setq lyskom-blocking-return retval
	lyskom-ok-to-send-new-calls nil))

(defun blocking-do (command &rest data)
  "Does the COMMAND agains the lyskom-server and returns the result.
COMMAND is one lyskom-command \(like the initiate-* but the initiate- is
stripped. DATA is the args to command.
The cache is consulted when command is get-conf-stat, get-pers-stat
or get-text-stat."
  ;; Here we could check if lyskom-blocking-return is non-nil, in
  ;; which case there is a bug in the code

  (save-excursion
    (set-buffer (or lyskom-buffer
                    (process-buffer lyskom-proc)))
    ;; If this happens, we're in trouble
    (if lyskom-is-parsing
	(lyskom-really-serious-bug))

    (let ((lyskom-blocking-return 'not-yet-gotten))
      ;; This should not be necessary, but for robustness sake...
      ;; There are occasions when it is needed.
      (setq lyskom-ok-to-send-new-calls t)

      (apply (intern-soft (concat "initiate-"
				  (symbol-name command)))
	     'blocking 'blocking-return
	     data)
      (unwind-protect
	  (while (and (eq lyskom-blocking-return 'not-yet-gotten)
		      (memq (process-status lyskom-proc) '(open run))
		      ;; The following test should probably be removed
		      (not lyskom-quit-flag))
	    (lyskom-accept-process-output))
	
	;; OK to continue with prefetch and stuff again
	(setq lyskom-ok-to-send-new-calls t)
	(lyskom-check-output-queues))
      
      (if (or lyskom-quit-flag quit-flag)
          (signal 'quit nil))
      (setq lyskom-quit-flag nil)
      lyskom-blocking-return)))



(defun lyskom-wait-queue (queue)
  "Waits until all data on QUEUE has been processed"
  (save-excursion
    (set-buffer (or lyskom-buffer
                    (process-buffer lyskom-proc)))
    (let ((collector (make-collector)))
      (lyskom-run queue (lambda (c) (set-collector->value c t)) collector)
      (unwind-protect
	  (while (and (null (collector->value collector))
		      (not lyskom-quit-flag))
	    (lyskom-accept-process-output))
	(setq lyskom-ok-to-send-new-calls t)
	(lyskom-check-output-queues))
      (if (or lyskom-quit-flag quit-flag)
	  (progn
	    (lyskom-insert-before-prompt (lyskom-get-string 'interrupted))
	    (signal 'quit nil)))
      (setq lyskom-quit-flag nil)
      (collector->value collector))))


(defvar lyskom-multiple-blocking-return nil
  "Return from blocking-do-multiple")

(defun lyskom-blocking-do-multiple (call-list)
  (save-excursion
    (set-buffer (or lyskom-buffer
                    (process-buffer lyskom-proc)))
    ;; If this happens, we're in trouble
    (if lyskom-is-parsing
	(lyskom-really-serious-bug))
    
    (let ((lyskom-multiple-blocking-return 'not-yet-gotten))
      (setq lyskom-ok-to-send-new-calls t)
      (lyskom-collect 'blocking)
      (while call-list
	(apply (intern-soft (concat "initiate-"
				    (symbol-name (car (car call-list)))))
	       'blocking nil
	       (cdr (car call-list)))
	(setq call-list (cdr call-list)))
      (lyskom-use 'blocking 'lyskom-blocking-do-multiple-1)
      (unwind-protect
	  (while (and (eq lyskom-multiple-blocking-return 'not-yet-gotten)
		      (memq (process-status lyskom-proc) '(open run))
		      (not lyskom-quit-flag))
	    (lyskom-accept-process-output))
	;; OK to continue with prefetch and stuff again
	(setq lyskom-ok-to-send-new-calls t)
	(lyskom-check-output-queues))
      (if lyskom-quit-flag
	  (progn
	    (setq lyskom-quit-flag nil)
	    (lyskom-insert-before-prompt (lyskom-get-string 'interrupted))
	    (signal 'quit nil)))
      lyskom-multiple-blocking-return)))

(defun lyskom-blocking-do-multiple-1 (&rest data)
  (setq lyskom-multiple-blocking-return data
	lyskom-ok-to-send-new-calls nil))

(defun lyskom-cancel-call (queue-name ref-nos)
  "Attempt to cancel calls in queue QUEUE-NAME with ref-no in REF-NOS. 
There is no guarantee that the call will be canceled. In particular, if
the call is not on QUEUE-NAME or has been sent to the server, it will
probably not be canceled."
  (let ((found nil))

    ;; Delete the call from the pending queue

    (let* ((queue (cdr (assq queue-name lyskom-call-data)))
           (calls (and queue (lyskom-queue->all-entries (kom-queue->pending queue)))))
      (when queue
        (lyskom-queue-make-empty (kom-queue->pending queue))
        (lyskom-traverse el calls
          (if (memq (elt el 1) ref-nos)
              (setq found t)
            (lyskom-queue-enter (kom-queue->pending queue) el)))))
    
    ;; Delete the call from the output queue

    (let* ((queue (aref lyskom-output-queues (lyskom-queue-priority queue-name))))
      (when (and queue found)
        (let ((calls (lyskom-queue->all-entries queue)))
          (lyskom-queue-make-empty queue)
          (lyskom-traverse el calls
            (unless (memq (car el) ref-nos)
              (lyskom-queue-enter queue el))))))))

(put 'blocking-do-multiple 'edebug-form-spec '(sexp body))



(eval-and-compile (provide 'lyskom-services))

;;; services.el ends here
