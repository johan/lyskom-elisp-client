;;;; ================================================================
;;;; ================================================================
;;;;
;;;; File: cache.el
;;;;
;;;; This file contains all functions which have to do with
;;;; caching various data types in the LysKOM client.
;;;;
;;;;



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
  (cache-del conf-no 'lyskom-conf-cache))



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
  "Get text for textno TEXT-NO, or nil if nothing is cached.
This is a special case:
The entry could be a text or a textpointers"
  (let ((tx (cache-assoc text-no lyskom-text-mass-cache)))
    (cond
     ((lyskom-text-p tx) tx)
     ((lyskom-textpointers-p tx)
      (lyskom-create-text 
       (textpointers->text-no tx)
       (concat (buffer-substring (textpointers->start-subject tx)
				 (textpointers->end-subject tx))
	       (buffer-substring (textpointers->start-text tx)
				 (textpointers->end-text tx))))))))

  
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
  (setq lyskom-marked-text-cache (lyskom-array-to-list mark-array)))


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


(defun cache-set-who-info-list (who-info-arr)
  "Sets the cache of who-info items."
  (setq lyskom-who-info-cache (list 'WHO-INFO-LIST))
;+++ Detta {r fel:
;  (setq lyskom-session-no (who-info->connection
;			   (elt who-info-arr 0)))
  (lyskom-save-excursion
   (set-buffer lyskom-who-info-buffer)
   (erase-buffer))
  (mapcar 'cache-add-who-info
	  (sort (lyskom-array-to-list who-info-arr)
		(function (lambda (who1 who2)
			    (< (who-info->connection who1)
			       (who-info->connection who2)))))))


(defun cache-add-who-info (who-info)
  "Adds another entry to the lyskom-who-info-cache. Updating the buffer."
  (lyskom-collect 'who-buffer)
  (initiate-get-conf-stat
   'who-buffer nil (who-info->pers-no who-info))
  (initiate-get-conf-stat 
   'who-buffer nil (who-info->working-conf who-info))
  (lyskom-use 'who-buffer 'lyskom-set-who-info-buffer-2
	      who-info))


(defun cache-add-session-info (session-info)
  "Adds another entry to the lyskom-who-info-cache. Updating the buffer.
ARG: session-info"
  (if (null session-info)
      nil				;+++ Annan felhantering
    (lyskom-collect 'who-buffer)
    (initiate-get-conf-stat
     'who-buffer nil (session-info->pers-no session-info))
    (initiate-get-conf-stat 
     'who-buffer nil (session-info->working-conf session-info))
    (lyskom-use 'who-buffer 'lyskom-set-session-info
		session-info)))


(defun cache-del-who-info (session-no)
  "Delete the session SESSION-NO from the lyskom-who-info-cache. Updating buffer."
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
	  (cache-del session-no 'lyskom-who-info-cache)))))


(defun lyskom-set-who-info-buffer-2 (pers-conf-stat conf-conf-stat who-info)
  "Inserts a who-buffer-info into lyskom-who-info-cache"
  ;We can use lyskom-insert (not beautiful)
  ;we insert everything at the end of the buffer.
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
       (lyskom-print-who-info pers-conf-stat conf-conf-stat who-info sesno)
       (setq max (point-max-marker))
       (goto-char (point-max)))
     (delete-char 1))
    (cache-add (who-info->connection who-info) 
	       (lyskom-create-who-buffer-info who-info min max)
	       'lyskom-who-info-cache)
    (run-hooks 'lyskom-who-info-has-changed-hook)))


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
