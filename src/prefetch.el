;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: prefetch.el,v 44.23 2002-02-24 20:23:27 joel Exp $
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
;;;;
;;;; File: prefetch.el
;;;;
;;;; This file contains the functions that make up the prefetch
;;;; system.
;;;;
;;;; Author: Inge Wallin
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: prefetch.el,v 44.23 2002-02-24 20:23:27 joel Exp $\n"))


;;; ================================================================
;;;                          Variables.


(def-kom-var lyskom-prefetch-stack nil
  "A stack where all prefetch requests are entered. New items are entered
first and when an item is to be prefetched, it is taken from the front of 
this list. 

Each entry is either the atom 'DONE, a cons cell as described below or a 
lyskom-queue.

\('CONFSTAT . number\) - The conf stat of Conference NUMBER.
\('PERSSTAT . number\) - The pers stat of person NUMBER.
\('TEXTSTAT . number\) - The text stat of text NUMBER.
\('TEXTMASS . number\) - The text mass of text NUMBER.
\('TEXTAUTH . number\) - The text stat of the text NUMBER 
                       and the conf-stat of the author of it.
\('TEXT-ALL . number\) - The text stat and mass of text NUMBER, but also
                       all information that will be used when writing
                       this text for the user to see, such as conf-stat
                       for the author, text stats for commented texts,
                       comments, a.s.o.
\('TEXTTREE . number\) - The text stat, author, textauth of comments to 
		       and texttree of all comments and footnotes.
\('CONFSTATFORMAP conf-no first-local\) - The conf-stat of the conference
		       number CONF-NO is fetched and then we continue
		       to fetch the map.
\('MAP conf-stat first-local\) -
                       The next part of the map in conference CONF-STAT.
		       The length fetched per revolution is according to
		       the value of lyskom-fetch-map-nos.
\('MARKS\) -             The whole list of marked texts and then every info about
		       these texts.
\('WHOBUFFER\) -         The who-is-on-info to construct the who-buffer.
\('MEMBERSHIP . pers-no\) -
		       The next part of the membership for person PERS-NO
		       is fetched. How long we already have fetched is
		       kept in the variable lyskom-membership-is-read. If 
		       lyskom-membership-is-read is not a number then we
		       are done.
		       For every membership-part we fetch the conf-stats
		       before continuing with the next part.
\('MEMBERSHIPISREAD\) -  Just sets the lyskom-membership-is-read variable to t.
\('CANCELED . rest\)   Whatever it was, it has been canceled.


See further documentation in the source code."
  local)

;;;
;;; The four requests CONFSTAT, PERSSTAT, TEXTSTAT and TEXTMASS are 
;;; called simple requests and are handled immediately and removed.
;;; The others are called complex requests.  These will each generate
;;; further requests when they return.  
;;; 
;;; When one of the simple requests are sent to the server, the atom
;;; DONE is swapped for the request.
;;;
;;; When one of the complex requests are sent to the server, a
;;; lyskom-queue is swapped for the request and a pointer to the queue
;;; is sent to the handler.  When the call returns the new requests this 
;;; call generates will all be put on the queue.  This process can be
;;; repeated and a queue might contain other queues and so on.  
;;; 
;;; When the prefetch code is searching for a new request to process it
;;; always starts searching at the beginning of the variable 
;;; lyskom-prefetch-stack.  If an empty queue is encountered, it is simply
;;; skipped since this significates a complex request that has already 
;;; been sent and is awaiting its result.  A non-empty queue is recursively
;;; searched and treated in the same way as the original stack.
;;; 


(def-kom-var lyskom-prefetch-in-action nil
  "t when the prefetch-process is started and going."
  local)


(defvar lyskom-inhibit-prefetch nil
  "Set this to a non-nil value locally to inhibit the prefetch.
This is used to prevent the prefetch code to reenter itself.")


(def-kom-var lyskom-pending-prefetch 0
  "Variable counting the number of unfinished prefetch requests."
  local)


;;; ================================================================
;;;                 Functions callable from the outside


(defun lyskom-setup-prefetch ()
  "Sets up the prefetch process in lyskom."
  (setq lyskom-prefetch-stack (lyskom-stack-create))
  (setq lyskom-pending-prefetch 0)
  (setq lyskom-membership-is-read 0))


;;; =================================================================
;;; Functions to cancel some prefetches

(defun lyskom-prefetch-cancel-prefetch-map (conf-no &optional queue)
;;;  (let ((prefetch-list (if queue
;;;                           (lyskom-queue->all-entries queue)
;;;                         (lyskom-stack->all-entries lyskom-prefetch-stack))))
;;;    (lyskom-traverse el prefetch-list
;;;      (cond
;;;       ((lyskom-queue-p el)
;;;        (lyskom-prefetch-cancel-prefetch-map conf-no el))
;;;       ((not (consp el)) nil)
;;;       ((or (and (eq (car el) 'MAP) 
;;;                 (eq (conf-stat->conf-no (elt el 1)) conf-no))
;;;            (and (eq (car el) 'CONFSTATFORMAP)
;;;                 (eq (elt el 1) conf-no)))
;;;        (setcar el 'CANCELED))
;;;       )))
)


;;;; ================================================================
;;; +++ lyskom-reset-prefetch  to be called on client reset.
;;;                            must restart everything.
;;; +++ THIS DOES NOT WORK CURRENTLY
(defun lyskom-reset-prefetch ()
  "Reset the prefetch system."
  ;; (lyskom-wait-queue 'prefetch)
  (lyskom-setup-prefetch))


(defsubst lyskom-membership-is-read ()
  "Return t if the while membership list has been fetched, and nil otherwise."
  (eq lyskom-membership-is-read 't))

(defun lyskom-fetch-start-of-map (conf-stat membership)
  "Block fetching map for MEMBERSHIP until we see a text.
Start the prefetch for the remainder of the map."
  (let ((first-local (1+ (membership->last-text-read membership)))
        (last-local (1- (+ (conf-stat->no-of-texts conf-stat)
                           (conf-stat->first-local-no conf-stat))))
        (done nil))
    (while (not done)
      (let ((map (blocking-do 'get-map 
                              (membership->conf-no membership)
                              first-local
                              lyskom-fetch-map-nos)))
        (setq first-local (+ first-local lyskom-fetch-map-nos))
        (lyskom-enter-map-in-to-do-list map conf-stat membership)
        (cond ((and (map->text-nos map)
                    (< first-local last-local)
                    (> (length (map->text-nos map)) 0))
               (setq done t)
               (lyskom-prefetch-map-using-conf-stat conf-stat
                                                    first-local
                                                    membership)
               )

              ((< first-local last-local))
              (t (setq done t)))))))

(defun lyskom-prefetch-conf (conf-no &optional queue)
  "Prefetch the conf-stat for the conference with number CONF-NO.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (if conf-no
      (if queue
	  (lyskom-queue-enter queue (cons 'CONFSTAT conf-no))
	(lyskom-stack-push lyskom-prefetch-stack (cons 'CONFSTAT conf-no)))
    (signal 'lyskom-internal-error "No argument to lyskom-prefetch-conf"))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-pers (pers-no &optional queue)
  "Prefetch the pers-stat for person with number PERS-NO.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (if queue
      (lyskom-queue-enter queue (cons 'PERSSTAT pers-no))
    (lyskom-stack-push lyskom-prefetch-stack (cons 'PERSSTAT pers-no)))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-text (text-no &optional queue)
  "Prefetch the text-stat for the text with number TEXT-NO.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (if queue
      (lyskom-queue-enter queue (cons 'TEXTSTAT text-no))
    (lyskom-stack-push lyskom-prefetch-stack (cons 'TEXTSTAT text-no)))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-textmass (text-no &optional queue)
  "Prefetch the text mass for the text with number TEXT-NO.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (if queue
      (lyskom-queue-enter queue (cons 'TEXTMASS text-no))
    (lyskom-stack-push lyskom-prefetch-stack (cons 'TEXTMASS text-no)))
  (lyskom-continue-prefetch))
  

(defun lyskom-prefetch-textauth (text-no &optional queue)
  "Prefetch the text stat and the author of text number TEXT-NO.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (if queue
      (lyskom-queue-enter queue (cons 'TEXTAUTH text-no))
    (lyskom-stack-push lyskom-prefetch-stack (cons 'TEXTAUTH text-no)))
  (lyskom-continue-prefetch))
  

(defun lyskom-prefetch-text-all (text-no &optional queue)
  "Prefetch all info about the text with number TEXT-NO.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (if queue
      (lyskom-queue-enter queue (cons 'TEXT-ALL text-no))
    (lyskom-stack-push lyskom-prefetch-stack (cons 'TEXT-ALL text-no)))
  (lyskom-continue-prefetch))

(defun lyskom-prefetch-text-stat-all (text-stat &optional queue)
  "Prefetch all info about the text with text stat TEXT-STAT.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (when (null queue)
    (setq queue (lyskom-queue-create))
    (lyskom-stack-push lyskom-prefetch-stack queue))
  (lyskom-prefetch-text-all-handler text-stat queue))


(defun lyskom-prefetch-texttree (text-no &optional queue only-new)
  "Prefetch all info about the text with number TEXT-NO and descends recursively.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack.

If ONLY-NEW is non-nil and the text-stat in question is already
prefetched the prefetch is not done."
  (if (and only-new
	   (cache-get-text-stat text-no))
      nil
    (if queue
	(lyskom-queue-enter queue (cons 'TEXTTREE text-no))
      (lyskom-stack-push lyskom-prefetch-stack (cons 'TEXTTREE text-no))))
  (lyskom-continue-prefetch))
  

(defun lyskom-prefetch-membership (pers-no &optional queue)
  "h{mtar medlemsskapet i sm} delar och d{refter conf-stat f|r m|tena"
  (if queue
      (lyskom-queue-enter queue (cons 'MEMBERSHIP pers-no))
    (lyskom-stack-push lyskom-prefetch-stack (cons 'MEMBERSHIP pers-no)))
  (lyskom-continue-prefetch))

(defun lyskom-prefetch-one-membership (conf-no pers-no &optional queue)
  (if queue
      (lyskom-queue-enter queue (list 'ONE-MEMBERSHIP conf-no pers-no))
    (lyskom-stack-push lyskom-prefetch-stack (list 'ONE-MEMBERSHIP
                                                   conf-no pers-no)))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-map (conf-no membership &optional queue)
  "Prefetches a map for conf CONFNO."
  (lyskom-prefetch-map-from conf-no
			    (1+ (membership->last-text-read membership))
			    membership
			    queue))


(defun lyskom-prefetch-map-from (conf-no first-local membership
					 &optional queue)
  "Prefetches a map for conf CONFNO starting att FIRST-LOCAL."
  (if queue
      (lyskom-queue-enter queue (list 'CONFSTATFORMAP
				      conf-no first-local membership))
    (lyskom-stack-push lyskom-prefetch-stack
		       (list 'CONFSTATFORMAP
			     conf-no first-local membership)))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-map-using-conf-stat (conf-stat first-local membership
						      &optional queue)
  "Prefetches a map for conf CONFSTAT starting att FIRST-LOCAL."
  (if queue
      (lyskom-queue-enter queue (list 'MAP conf-stat first-local membership))
    (lyskom-stack-push lyskom-prefetch-stack (list 'MAP
						   conf-stat first-local
						   membership)))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-all-conf-stats (&optional queue)
  "+++"
  nil)


(defun lyskom-prefetch-marks (&optional queue)
  "Prefetches the list of marked texts. Then all texts are fetched."
  (if queue
      (lyskom-queue-enter queue (list 'MARKS))
    (lyskom-stack-push lyskom-prefetch-stack (list 'MARKS)))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-who-is-on (&optional queue)
  "Prefetches the list of persons on the system."
  (if queue
      (lyskom-queue-enter queue (list 'WHOBUFFER))
    (lyskom-stack-push lyskom-prefetch-stack (list 'WHOBUFFER)))
  (lyskom-continue-prefetch))


;;(defun lyskom-prefetch-all-conf-texts (&optional queue)
;;  "Prefetches the texts in all conferences."
;;  (if queue
;;      (lyskom-queue-enter queue (list 'ALL-CONF-TEXTS))
;;    (lyskom-stack-push lyskom-prefetch-stack (list 'ALL-CONF-TEXTS)))
;;  (lyskom-continue-prefetch))


;;(defun lyskom-prefetch-conf-texts (text-list &optional queue)
;;  "Prefetches the texts in all conferences."
;;  (if (null (text-list->texts text-list))
;;      nil
;;    (if queue
;;	(lyskom-queue-enter queue (list 'CONF-TEXTS text-list))
;;      (lyskom-stack-push lyskom-prefetch-stack (list 'CONF-TEXTS text-list))))
;;  (lyskom-continue-prefetch))

(defun lyskom-prefetch-texts (texts &optional queue)
  "Prefetches a list of texts."
  (if (null texts)
      nil
    (if queue
	(lyskom-queue-enter queue
			    (list 'TEXTS texts))
      (lyskom-stack-push lyskom-prefetch-stack
			 (list 'TEXTS texts))))
  (lyskom-continue-prefetch))

;; (defun lyskom-prefetch-text-list-continue (texts &optional queue)
;;   "Prefetches a list of texts."
;;   (if (null texts)
;; 	 nil
;;     (if queue
;; 	   (lyskom-queue-enter queue (list 'TEXT-LIST-CONT texts))
;; 	 (lyskom-stack-push lyskom-prefetch-stack
;; 			    (list 'TEXT-LIST-CONT texts))))
;;   (lyskom-continue-prefetch))


;;; ================================================================
;;;           Functions internal to the prefetch package


(defun lyskom-stop-prefetch ()
  "Stop the prefetch process temporarily."
  (setq lyskom-prefetch-in-action nil))


(defun lyskom-start-prefetch ()
  "Start the whole prefetch process"
  (setq lyskom-prefetch-in-action t)
  (lyskom-continue-prefetch))


(defun lyskom-continue-prefetch ()
  "Called after each prefetch is finished and also when the whole prefetch
process is started. Used to keep prefetch going."
  (if (not lyskom-inhibit-prefetch)
      (let ((lyskom-inhibit-prefetch t)) ; Make sure we don't call this 
					; recursively
	(while (and (< lyskom-pending-prefetch
		       lyskom-prefetch-limit)
		    (lyskom-prefetch-one-item)
		    ;; Only increase lyskom-pending-prefetch if a server
		    ;; call was made.
		    ;;
		    ;; The return value from lyskom-prefetch-one-item
		    ;; is whether it has sent a server call, but it
		    ;; should really be if the prefetch-stack has been
		    ;; altered. See the comment in
		    ;; lyskom-prefetch-one-item.
		    (++ lyskom-pending-prefetch))))))


(defun lyskom-skip-finished-in-queue (queue)
  "Remove all 'DONE entries and queues who's only entry is 'FINISHED."
  (let ((element nil)
	(done nil))
    (while (not done)
      (setq element (lyskom-queue->first queue))
      (if (or (eq element 'DONE)
              (and (consp element) 
                   (eq (car element) 'CANCELED))
	      (and (lyskom-queue-p element)
		   (eq (lyskom-queue->first element) 'FINISHED)))
	  (lyskom-queue-delete-first queue)
	(setq done t)))))


(defun lyskom-prefetch-one-item ()
  "Get the first element of the prefetch data structure and fetch it.
Return t if an element was prefetched, otherwise return nil."
  (let ((result nil)
	element
	(prefetch-list (lyskom-stack->all-entries lyskom-prefetch-stack))
	(list-stack (lyskom-stack-create))
    	(done nil))

    ; Remove all finished entries at the top of lyskom-prefetch-stack
    (while (not done)
      (setq element (lyskom-stack->top lyskom-prefetch-stack))
      (if (or (eq element 'DONE)
              (and (consp element)
                   (eq (car element) 'CANCELED))
	      (and (lyskom-queue-p element)
		   (eq (lyskom-queue->first element) 'FINISHED)))
	  (lyskom-stack-pop lyskom-prefetch-stack)
	(setq done t)))

    (while (and (not result)
		prefetch-list)
      (let ((element (car prefetch-list))
	    (rest-list (cdr prefetch-list)))
	(cond
	 ((eq element 'DONE) nil)
	 ((eq element 'FINISHED) nil)
         ((and (consp element)
               (eq (car element) 'CANCELED)) nil)

	 ;; A queue ==> check it out first.
	 ((lyskom-queue-p element)
	  (lyskom-skip-finished-in-queue element)
	  (if (lyskom-queue-isempty element)
	      nil
	    (lyskom-stack-push list-stack rest-list)
	    (setq rest-list (lyskom-queue->all-entries element))))
       
	 ;; A simple request?
	 ((and (listp element)
	       (memq (car element)
		     '(CONFSTAT PERSSTAT TEXTSTAT TEXTMASS)))
	  (setcar prefetch-list 'DONE)
	  (lyskom-prefetch-one-request element nil)
	  (setq result t))

	 ;; A complex request?
	 ((and (listp element)
	       (memq (car element)
		     '(TEXTAUTH TEXT-ALL TEXTTREE ONE-MEMBERSHIP
				CONFSTATFORMAP MAP MARKS
				MEMBERSHIP WHOBUFFER TEXTS)))
	  (let ((queue (lyskom-queue-create)))
	    (setcar prefetch-list queue)
	    (lyskom-prefetch-one-request element queue)
	    (setq result t)))

	 ;; Special requests
	 ((and (listp element)
	       (memq (car element) '(MEMBERSHIPISREAD ALL-CONF-TEXTS)))
	  (if (eq (car element) 'MEMBERSHIPISREAD)
	      (setq lyskom-membership-is-read t)
	    ;; Temporarily disabled
	    (let ((queue (lyskom-queue-create)))
	      (setcar prefetch-list queue)
	      (mapcar
	       (lambda (read-info)
		 (mapcar
		  (lambda (text-no)
		    (lyskom-prefetch-text-all text-no queue))
		  (text-list->texts (read-info->text-list read-info))))
	       (read-list->all-entries lyskom-to-do-list))
	      (lyskom-queue-enter queue 'FINISHED)))

	  ;; This is an ugly hack. If this function returns a non-nil
	  ;; value, lyskom-prefetch-continue will assume that a server
	  ;; call was made and increase lyskom-pending-prefetch. But
	  ;; no server call has been made, so we decrease
	  ;; lyskom-pending-prefetch "in advance". The reason that
	  ;; this sets result to t is that we want the loop in
	  ;; lyskom-continue-prefetch to keep running, as there is no
	  ;; server response that will wake the prefetch up in the
	  ;; future.
	  ;;(-- lyskom-pending-prefetch)
	  ;;(setq result t)
	  )

	 (t (signal 'lyskom-internal-error 
		    '(lyskom-prefetch-one-item ": unknown key"))))
	
	(setq prefetch-list rest-list)
	(if (not (or prefetch-list
		     (lyskom-stack-isempty list-stack)))
	    (setq prefetch-list (lyskom-stack-pop list-stack)))))

    result))


(defun lyskom-prefetch-one-request (request queue)
  "Prefetch REQUEST. If the request is complex, put the resulting requests on QUEUE."
;  (message "Prefetch: %s" request)
  (cond
   ((eq (car request) 'CONFSTAT)
    (initiate-get-conf-stat 'prefetch
			    'lyskom-prefetch-handler (cdr request)))
   ((eq (car request) 'PERSSTAT)
    (initiate-get-pers-stat 'prefetch
			    'lyskom-prefetch-handler (cdr request)))
   ((eq (car request) 'TEXTSTAT)
    (initiate-get-text-stat 'prefetch
			    'lyskom-prefetch-handler (cdr request)))
   ((eq (car request) 'TEXTMASS)
    (initiate-get-text 'prefetch 'lyskom-prefetch-handler (cdr request)))
   ((eq (car request) 'TEXTAUTH)
    (initiate-get-text-stat 'prefetch 'lyskom-prefetch-textauth-handler 
			    (cdr request) queue))
   ((eq (car request) 'TEXT-ALL)
    (initiate-get-text-stat 'prefetch 'lyskom-prefetch-text-all-handler
			    (cdr request) queue))
   ((eq (car request) 'TEXTTREE)
    (initiate-get-text-stat 'prefetch 'lyskom-prefetch-texttree-handler
			    (cdr request) queue))

   ((eq (car request) 'ONE-MEMBERSHIP)
    (initiate-query-read-texts 'prefetch
                               'lyskom-prefetch-read-texts-handler
                               (elt request 2)
                               (elt request 1)
                               (elt request 1)
                               queue))

   ((eq (car request) 'MEMBERSHIP)
    (if (numberp lyskom-membership-is-read) ; Are we done?
	(initiate-get-part-of-membership 
	 'prefetch 'lyskom-prefetch-membership-handler
	 (cdr request)
	 lyskom-membership-is-read lyskom-fetch-membership-length 
	 (cdr request)
	 queue)
      ; We are done
      (lyskom-prefetch-handler)))
   ((eq (car request) 'CONFSTATFORMAP)
    (initiate-get-conf-stat 'prefetch 'lyskom-prefetch-confstatformap-handler
			    (nth 1 request) (nth 2 request) (nth 3 request)
			    queue))
   ((eq (car request) 'MAP)
    (initiate-get-map 'prefetch 'lyskom-prefetch-map-handler
		      (conf-stat->conf-no
		       (nth 1 request))	; conf-stat
		      (nth 2 request)	; first-local
		      lyskom-fetch-map-nos
		      (nth 1 request)	; conf-stat
		      (nth 2 request)	; first-local
		      (nth 3 request)	; membership
		      queue))
   ((eq (car request) 'MARKS)
    (initiate-get-marks 'prefetch 'lyskom-prefetch-marks-handler queue))
   ((eq (car request) 'WHOBUFFER)
    (initiate-who-is-on 'prefetch 'lyskom-prefetch-whobuffer-handler queue))
   ((eq (car request) 'TEXTS)
    (initiate-get-text-stat 'prefetch 'lyskom-prefetch-texts-handler
			    (car (nth 1 request))
			    (cdr (nth 1 request))
			    queue))
   
   (t (signal 'lyskom-internal-error
	      (list "lyskom-prefetch-one-request - unknown key:"
		    (car request))))))


;;; ================================================================
;;;      Functions which handle the results of complex requests.


(defun lyskom-prefetch-textauth-handler (text-stat queue)
  "Prefetch the conf-stat of the author of the text TEXT-STAT.
Put the request on QUEUE."
  (lyskom-stop-prefetch)
  (if (not text-stat)
      nil
    (lyskom-prefetch-conf (text-stat->author text-stat) queue)
    (lyskom-queue-enter queue 'FINISHED))
  (-- lyskom-pending-prefetch)
  (lyskom-start-prefetch))


(defun lyskom-prefetch-text-all-handler (text-stat queue)
  "Prefetch all info neccessary to write the text with text-stat TEXT-STAT.
Put the requests on QUEUE."
  (lyskom-stop-prefetch)
  (lyskom-prefetch-conf (text-stat->author text-stat) queue)
  (lyskom-prefetch-textmass (text-stat->text-no text-stat) queue)
  (lyskom-traverse
   misc
   (text-stat->misc-info-list text-stat)
   (let ((type (misc-info->type misc)))
     (cond
      ((memq type '(RECPT BCC-RECPT CC-RECPT))
       (lyskom-prefetch-conf (misc-info->recipient-no misc) queue))
      ((eq type 'COMM-IN)
       (lyskom-prefetch-textauth (misc-info->comm-in misc) queue))
      ((eq type 'FOOTN-IN)
       (lyskom-prefetch-textauth (misc-info->footn-in misc) queue))
      ((eq type 'COMM-TO)
       (lyskom-prefetch-textauth (misc-info->comm-to misc) queue))
      ((eq type 'FOOTN-TO)
       (lyskom-prefetch-textauth (misc-info->footn-to misc) queue))
      (t nil))))
  (lyskom-queue-enter queue 'FINISHED)
  (-- lyskom-pending-prefetch)
  (lyskom-start-prefetch))


(defun lyskom-prefetch-texttree-handler (text-stat queue)
  "Prefetch all info neccessary to write the text with text-stat TEXT-STAT.
Then prefetch all info (texttree) of comments.
Put the requests on QUEUE."
  (if (not text-stat)
      nil				; We did not get anything
    (lyskom-stop-prefetch)
    (lyskom-prefetch-conf (text-stat->author text-stat) queue)
    (lyskom-prefetch-textmass (text-stat->text-no text-stat) queue)
    (lyskom-traverse
     misc
     (text-stat->misc-info-list text-stat)
     (let ((type (misc-info->type misc)))
       (cond
	((memq type '(RECPT BCC-RECPT CC-RECPT))
	 (lyskom-prefetch-conf (misc-info->recipient-no misc) queue))
	((eq type 'COMM-IN)
	 (lyskom-prefetch-texttree (misc-info->comm-in misc) queue t))
	((eq type 'FOOTN-IN)
	 (lyskom-prefetch-texttree (misc-info->footn-in misc) queue t))
	((eq type 'COMM-TO)
	 (lyskom-prefetch-textauth (misc-info->comm-to misc) queue))
	((eq type 'FOOTN-TO)
	 (lyskom-prefetch-textauth (misc-info->footn-to misc) queue))
	(t nil))))
    (lyskom-queue-enter queue 'FINISHED))
  (-- lyskom-pending-prefetch)
  (lyskom-start-prefetch))


(defun lyskom-prefetch-read-texts-handler (membership pers-no queue)
  (lyskom-stop-prefetch)
  (-- lyskom-pending-prefetch)
  (when membership
    (unless (lyskom-try-get-membership (membership->conf-no membership) t)
      (lyskom-add-memberships-to-membership (list membership))
      (when (and (lyskom-visible-membership membership)
                 (lyskom-prefetch-map (membership->conf-no membership)
                                      membership
                                      queue)))))
  (lyskom-queue-enter queue 'FINISHED)
  (lyskom-start-prefetch))


(defun lyskom-prefetch-membership-handler (memberships pers-no queue)
  "Handle the return of the membership prefetch call."
  (lyskom-stop-prefetch)
  (let ((size (length memberships))
	(i 0)
;;; Commented out 1999-06-28 byers
;;; Used by removed code below
;;;        (old-mships (mapcar (lambda (mship)
;;;                              (and (lyskom-try-get-membership
;;;                                    (membership->conf-no mship))
;;;                                   (membership->conf-no mship)))
;;;                            memberships))
        )
    (lyskom-insert-memberships-in-membership memberships)
    (while (< i size)
      (let ((membership (aref memberships i)))
;;; Commented out 1999-06-26 byers
;;; This should not be necessary since we know that all of these
;;; maps were empty when we started the client. Texts created after
;;; the client was started should end up in the reading list anyway
;;; 'cuase they generate asynchronous messages.
;;;	(if (and (lyskom-visible-membership membership)
;;;                 (not (memq (membership->conf-no membership) old-mships)))
;;;	    (lyskom-prefetch-map (membership->conf-no membership)
;;;				 membership
;;;				 queue))
)
      (++ i))
    (if (and (numberp lyskom-membership-is-read)
	     (< (length memberships) lyskom-fetch-membership-length))
	(progn
	  (setq lyskom-membership-is-read 'almost)
	  (lyskom-queue-enter queue (list 'MEMBERSHIPISREAD)))
      (setq lyskom-membership-is-read (+ lyskom-membership-is-read
					 lyskom-fetch-membership-length))
      (lyskom-prefetch-membership pers-no queue)
      ))
  (lyskom-queue-enter queue 'FINISHED)
  (-- lyskom-pending-prefetch)
  (lyskom-start-prefetch))


(defun lyskom-prefetch-confstatformap-handler (conf-stat first-local
							 membership queue)
  "Now that we have the conf-stat we can fetch the map."
  (lyskom-stop-prefetch)
  (lyskom-prefetch-map-using-conf-stat conf-stat first-local membership queue)
  (lyskom-queue-enter queue 'FINISHED)
  (-- lyskom-pending-prefetch)
  (lyskom-start-prefetch))


(defun lyskom-prefetch-map-handler (map conf-stat first-local membership queue)
  "Handle the return of the membership prefetch call.
Maps are `cached' in lyskom-to-do-list."
  (lyskom-stop-prefetch)
  (let ((next-start (+ first-local lyskom-fetch-map-nos))
	(last-local (1- (+ (conf-stat->no-of-texts conf-stat)
		  (conf-stat->first-local-no conf-stat)))))
    (when map
      ;; An old version of this function tester if the map contained no
      ;; texts. That is not a correct termination condition.
      (when (< next-start last-local)
	(lyskom-prefetch-map-using-conf-stat conf-stat
					     next-start
					     membership
					     queue))
      (lyskom-enter-map-in-to-do-list map conf-stat membership)))
  (lyskom-queue-enter queue 'FINISHED)
  (-- lyskom-pending-prefetch)
  (lyskom-start-prefetch)
  (lyskom-update-prompt)
  (lyskom-set-mode-line))

(defun lyskom-prefetch-marks-handler (marks queue)
  "Handle the list of marked texts."
  (cache-set-marked-texts marks)
  (lyskom-stop-prefetch)
  (let ((list (cache-get-marked-texts)))
    (while list
      (lyskom-prefetch-text-all (mark->text-no (car list)) queue)
      (setq list (cdr list))))
  (-- lyskom-pending-prefetch)
  (lyskom-start-prefetch))


(defun lyskom-prefetch-whobuffer-handler (who-is-on queue)
  "Handle the who-is-on info. The goal here is to get an updated who-buffer."
  ;+++ should be done later
  (cache-initiate-who-info-buffer who-is-on lyskom-buffer)
  (-- lyskom-pending-prefetch)
  )

(defun lyskom-prefetch-texts-handler (text-stat texts queue)
  "Prefetch all info neccessary to write the text with text-stat TEXT-STAT.
Put the requests on QUEUE."
  (lyskom-stop-prefetch)
  (lyskom-prefetch-conf (text-stat->author text-stat) queue)
  (lyskom-prefetch-textmass (text-stat->text-no text-stat) queue)
  (lyskom-traverse
      misc
      (text-stat->misc-info-list text-stat)
    (let ((type (misc-info->type misc)))
      (cond
       ((memq type '(RECPT BCC-RECPT CC-RECPT))
	(lyskom-prefetch-conf (misc-info->recipient-no misc) queue))
       ((eq type 'COMM-IN)
	(lyskom-prefetch-textauth (misc-info->comm-in misc) queue))
       ((eq type 'FOOTN-IN)
	(lyskom-prefetch-textauth (misc-info->footn-in misc) queue))
       ((eq type 'COMM-TO)
	(lyskom-prefetch-textauth (misc-info->comm-to misc) queue))
       ((eq type 'FOOTN-TO)
	(lyskom-prefetch-textauth (misc-info->footn-to misc) queue))
       (t nil))))
  (lyskom-queue-enter queue 'FINISHED)
  ;; The queue is now used up.
  (lyskom-prefetch-texts texts)
  (-- lyskom-pending-prefetch)
  (lyskom-start-prefetch))

(defun lyskom-prefetch-handler (&rest data)
  "Handler called after each simple prefetch request is done."
  (-- lyskom-pending-prefetch)
  (lyskom-continue-prefetch))
