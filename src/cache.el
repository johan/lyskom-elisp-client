;;;;;
;;;;; $Id: cache.el,v 39.0 1996-03-14 18:16:50 davidk Exp $
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
;;;; File: cache.el
;;;;
;;;; This file contains all functions which have to do with
;;;; caching various data types in the LysKOM client.
;;;;
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: cache.el,v 39.0 1996-03-14 18:16:50 davidk Exp $\n"))


;;; ================================================================
;;;			UConf-stat cache

(defun cache-get-uconf-stat (conf-no)
  "Get uconf-stat for conference CONF-NO, or nil if nothing is cached.
If full conf-stat is cached, construct an uconf-stat from that data and
cache it."
  (or (cache-assoc conf-no lyskom-uconf-cache)
      (cache-construct-uconf-stat (cache-get-conf-stat conf-no))))

(defun cache-construct-uconf-stat (conf)
  "If conf is non-nil, create an uconf-stat from conf and cache it.
Return the new uconf-stat or nil"
  (and conf
       (cache-add-uconf-stat 
	(lyskom-create-uconf-stat (conf-stat->conf-no conf)
				  (conf-stat->name conf)
				  (conf-stat->conf-type conf)
				  (+
				   (conf-stat->first-local-no conf)
				   (conf-stat->no-of-texts conf))
				  (conf-stat->garb-nice conf)))))

(defun cache-add-uconf-stat (uconf-stat)
  "Insert a UCONF-STAT in the cache."
  (cache-add (uconf-stat->conf-no uconf-stat)
	     uconf-stat
	     'lyskom-uconf-cache))

(defun cache-del-uconf-stat (conf-no)
  "Delete a conf-stat from the cache. Args: CONF-NO."
  (cache-del conf-no 'lyskom-uconf-cache)
  (cache-del conf-no 'lyskom-conf-cache))


;;; ================================================================
;;;                     Conf-stat cache.


(defun cache-get-conf-stat (conf-no)
  "Get conf-stat for conference CONF-NO, or nil if nothing is cached."
  (cache-assoc conf-no lyskom-conf-cache))

(defun cache-add-conf-stat (conf-stat)
  "Insert a CONF-STAT in the cache."
  (cache-add (conf-stat->conf-no conf-stat) conf-stat 'lyskom-conf-cache))


(defun cache-del-conf-stat (conf-no)
  "Delete a conf-stat from the cache. Args: CONF-NO."
  (cache-del conf-no 'lyskom-conf-cache)
  (cache-del conf-no 'lyskom-uconf-cache))



;;; ================================================================
;;;                       Pers-stat cache.


(defun cache-get-pers-stat (pers-no)
  "Get pers-stat for person PERS-NO, or nil if nothing is cached."
  (cache-assoc pers-no lyskom-pers-cache))

  
(defun cache-add-pers-stat (pers-stat)
  "Insert a PERS-STAT in the cache."
  (cache-add (pers-stat->pers-no pers-stat) pers-stat 'lyskom-pers-cache))


(defun cache-del-pers-stat (pers-no)
  "Delete a pers-stat from the cache. Args: PERS-NO."
  (cache-del pers-no 'lyskom-pers-cache))
  


;;; ================================================================
;;;                       Text-stat cache.


(defun cache-get-text-stat (text-no)
  "Get text-stat for texton TEXT-NO, or nil if nothing is cached."
  (cache-assoc text-no lyskom-text-cache))

  
(defun cache-add-text-stat (text-stat)
  "Insert a TEXT-STAT in the cache."
  (cache-add (text-stat->text-no text-stat) text-stat 'lyskom-text-cache))


(defun cache-del-text-stat (text-no)
  "Delete a text-stat from the cache. Args: TEXT-NO."
  (cache-del text-no 'lyskom-text-cache))


  
;;; ================================================================
;;;                 Text cache (the text strings).


(defun cache-get-text (text-no)
  "Get text for textno TEXT-NO, or nil if nothing is cached."
  (let ((tx (cache-assoc text-no lyskom-text-mass-cache)))
    (cond
     ((lyskom-text-p tx) tx))))

  
(defun cache-add-text (text)
  "Insert a TEXT in the cache."
  (cache-add (text->text-no text) text 'lyskom-text-mass-cache))


(defun cache-del-text (text-no)
  "Delete a text from the cache. Args: TEXT-NO."
  (cache-del text-no 'lyskom-text-mass-cache))



;;; ================================================================
;;;                      Marked texts cache.


(defun cache-get-marked-texts ()
  "Return the a list of marks for all marked texts."
  lyskom-marked-text-cache)


(defun cache-set-marked-texts (mark-array)
  "Sets the marks for all marked texts. A mark-list is an array, but we
want a list in the cache, so this remakes it into a list."
  (setq lyskom-marked-text-cache (listify-vector mark-array)))


(defun cache-text-is-marked (text-no)
  "Return the mark if the text text-no is marked by the current user,
otherwise return nil"
  (let ((marks lyskom-marked-text-cache)
	(mark nil))
    (while (and (not (null marks))
		(null mark))
      (if (equal text-no (mark->text-no (car marks)))
	  (setq mark (car marks))
	(setq marks (cdr marks))))
    mark))


(defun cache-add-marked-text (text-no mark-type)
  "Insert a mark into the cache. If it is already there, replace it."
  (let ((marks lyskom-marked-text-cache)
	(found nil)
	(mark (lyskom-create-mark text-no mark-type)))
    (while (and (not found)
		(not (null marks)))
      (if (and (car marks)
	       (equal text-no (mark->text-no (car marks))))
	  (progn
	    (setcar marks mark)
	    (setq found t))
	(setq marks (cdr marks))))
    (if (not found)
	(setq lyskom-marked-text-cache
	      (cons mark lyskom-marked-text-cache)))))


(defun cache-del-marked-text (text-no)
  "Remove the mark from the cache of marked texts if it is there.
+++BUG: A mark is replaced with a nil and not removed."
  (let ((marks lyskom-marked-text-cache)
	(found nil))
    (while (and (not found)
		(not (null marks)))
      (if (equal text-no (mark->text-no (car marks)))
	  (progn
	    (setcar marks nil)
	    (setq found t))
	(setq marks (cdr marks))))))


;;; ================================================================
;;;                         who-info cache


(defun cache-initiate-who-info-buffer (who-info-arr kombuf)
  "Sets the cache of who-info items."
  (setq lyskom-who-info-cache (list 'WHO-INFO-LIST))
  (lyskom-save-excursion
   (setq lyskom-who-info-buffer 
	 (get-buffer-create (concat (buffer-name) "-who")))
   (set-buffer lyskom-who-info-buffer)
   (make-local-variable 'kom-buffer)
   (setq lyskom-buffer kombuf)
   (local-set-key [mouse-2] 'kom-mouse-2)
   (erase-buffer))
  (mapcar 'cache-add-who-info
	  (sort (listify-vector who-info-arr)
		(function (lambda (who1 who2)
			    (< (who-info->connection who1)
			       (who-info->connection who2)))))))


(defun cache-add-who-info (who-info)
  "Adds another entry to the lyskom-who-info-cache. Updating the buffer."
  (if lyskom-who-info-buffer-is-on
      (progn
	(lyskom-collect 'who-buffer)
	(initiate-get-conf-stat
	 'who-buffer nil (who-info->pers-no who-info))
	(initiate-get-conf-stat 
	 'who-buffer nil (who-info->working-conf who-info))
	(lyskom-use 'who-buffer 'lyskom-set-who-info-buffer-2
		    who-info))))


(defun cache-add-session-info (session-info)
  "Adds another entry to the lyskom-who-info-cache. Updating the buffer.
ARG: session-info"
  (if (null session-info)
      nil				;+++ Annan felhantering
    (lyskom-halt 'who-buffer)
    (lyskom-collect 'who-buffer-2)
    (initiate-get-conf-stat
     'who-buffer-2 nil (session-info->pers-no session-info))
    (initiate-get-conf-stat 
     'who-buffer-2 nil (session-info->working-conf session-info))
    (lyskom-use 'who-buffer-2 'lyskom-set-session-info
		session-info)
    (lyskom-run 'who-buffer-2 'lyskom-resume 'who-buffer)))


(defun cache-del-who-info (session-no)
  "Delete the session SESSION-NO from the lyskom-who-info-cache. Updating buffer."
  (if lyskom-who-info-buffer-is-on
      (let ((where (cache-assoc session-no lyskom-who-info-cache)))
	(if where
	    (progn
	      (lyskom-save-excursion
	       (set-buffer lyskom-who-info-buffer)
	       (delete-region (marker-position
			       (who-buffer-info->start-marker where))
			      (marker-position
			       (who-buffer-info->end-marker where))))
	      (set-marker (who-buffer-info->start-marker where) nil)
	      (set-marker (who-buffer-info->end-marker where) nil)
	      (cache-del session-no 'lyskom-who-info-cache))))))


(defun lyskom-set-who-info-buffer-2 (pers-conf-stat conf-conf-stat who-info)
  "Inserts a who-buffer-info into lyskom-who-info-cache"
  ;We can use lyskom-insert (not beautiful)
  ;we insert everything at the end of the buffer.

  ;defensive programming and it will work:
  (if (and lyskom-who-info-buffer-is-on
	   lyskom-who-info-buffer)
      (let ((sesno lyskom-session-no)
	    min max
	    (where (cache-assoc (who-info->connection who-info) 
				lyskom-who-info-cache)))
	(lyskom-save-excursion
	 (set-buffer lyskom-who-info-buffer)
	 (save-restriction
	   (if where
	       (progn
		 (narrow-to-region (marker-position 
				    (who-buffer-info->start-marker where))
				   (1- (marker-position 
					(who-buffer-info->end-marker where))))
		 (delete-region (point-min) (point-max)))
	     (goto-char (point-max))
	     (insert " ")
	     (narrow-to-region (point-min) (1- (point-max))))
	   (setq min (point-max-marker))
	   (lyskom-print-who-info pers-conf-stat conf-conf-stat who-info sesno
				  (function
				   (lambda (string)
				     (insert string))))
	   (setq max (point-max-marker))
	   (goto-char (point-max)))
	 (delete-char 1))
	(cache-add (who-info->connection who-info) 
		   (lyskom-create-who-buffer-info who-info min max)
		   'lyskom-who-info-cache)
	(run-hooks 'lyskom-who-info-has-changed-hook))))


(defun lyskom-set-session-info (pers-conf-stat conf-conf-stat session-info)
  "Inserts a session-info into lyskom-who-info-cache"
  (lyskom-set-who-info-buffer-2 pers-conf-stat conf-conf-stat
				(lyskom-create-who-info
				 (session-info->pers-no session-info)
				 (session-info->working-conf session-info)
				 (session-info->connection session-info)
				 (session-info->doing session-info)
				 (session-info->username session-info))))


;;; ================================================================
;;;                     Generic cache routines

;;; BUGS: The cache is never emptied. +++


(defun cache-assoc (key cache)
  "Get data for item with key KEY from CACHE.
CACHE is an assoc-list in this implementation."
  (cdr-safe (assoc key cache)))


(defun cache-add (__key__ __data__ __cache__)
  "Add DATA to CACHE under the key KEY.
Args: KEY DATA CACHE.
CACHE is a (the only one) quoted variable pointing to the cache (an alist).
The variable might be changed."
  (cond
   ((null (eval __cache__))
    (set __cache__ (list (cons __key__ __data__))))
   (t (let ((oldval (assoc __key__ (eval __cache__))))
	(cond
	 ((null oldval)
	  (set __cache__
	       (cons (cons __key__ __data__)

		     (eval __cache__))))
	 (t
	  (setcdr oldval __data__)))))))


(defun cache-del (__key__ __cache__)
  "Delete item with key KEY from CACHE.
CACHE is the name of the variable that points to the cache."
  (let ((oldval (assoc __key__ (eval __cache__))))
    (if oldval
	(setcdr oldval nil))))		;A pair (key . nil) will remain.
					;Fix this bug someday. +++


;;; ================================================================
;;;         lyskom-tell-server (this is also a sort of cache)


(defvar lyskom-what-i-am-doing nil
  "What the client thinks the server thinks the user is doing.")


(defun lyskom-tell-server (string)
  "Tell the server what the user is doing. Args: STRING."
  (cond
   ((equal string lyskom-what-i-am-doing))
   (t
    (setq lyskom-what-i-am-doing string)
    (initiate-change-what-i-am-doing 'background nil string))))
