;;;; ================================================================
;;;; ================================================================
;;;;
;;;; File: prefetch.el
;;;;
;;;; This file contains the functions that make up the prefetch
;;;; system.
;;;;
;;;; Author: Inge Wallin
;;;;


;;; ================================================================
;;;                          Variables.


(defvar lyskom-prefetch-stack nil
  "A stack where all prefetch requests are entered. New items are entered
first and when an item is to be prefetched, it is taken from the front of 
this list. 

Each entry is either the atom 'DONE, a cons cell as described below or a 
lyskom-queue.

('CONFSTAT . number) - The conf stat of Conference NUMBER.
('PERSSTAT . number) - The pers stat of person NUMBER.
('TEXTSTAT . number) - The text stat of text NUMBER.
('TEXTMASS . number) - The text mass of text NUMBER.
('TEXTAUTH . number) - The text stat of the text NUMBER 
                       and the conf-stat of the author of it.
('TEXT-ALL . number) - The text stat and mass of text NUMBER, but also
                       all information that will be used when writing
                       this text for the user to see, such as conf-stat
                       for the author, text stats for commented texts,
                       comments, a.s.o.
('TEXTTREE . number) - The text stat, author, textauth of comments to 
		       and texttree of all comments and footnotes.
('MAP-PART . number) - The next part of the map in conference NUMBER.
                       Info about the local text-no:s can be found in the 
                       read-info for the conference in lyskom-to-do-list or 
                       lyskom-reading-list.
		       +++ Not yet implemented.

See further documentation in the source code.")

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


(defvar lyskom-prefetch-in-action nil
  "t when the prefetch-process is started and going.")


(defvar lyskom-pending-prefetch 0
  "Variable counting the number of unfinished prefetch requests.")


;+++These should really be in vars.el:
(defvar lyskom-prefetch-limit 2
  "Number of prefetch requests the client will try to keep going
at a time.")

(defvar lyskom-prefetch-map-size 25
  "Number of local text-no:s that are fetched at a time in a map.")


;;; ================================================================
;;;                 Functions callable from the outside


(defun lyskom-setup-prefetch ()
  "Sets up the prefetch process in lyskom."
;+++ Much more could be done here.
  (setq lyskom-prefetch-stack (lyskom-stack-create)))


(defun lyskom-prefetch-conf (conf-no &optional queue)
  "Prefetch the conf-stat for the conference with number CONF-NO.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (if queue
      (lyskom-queue-enter queue (cons 'CONFSTAT conf-no))
    (lyskom-stack-push lyskom-prefetch-stack (cons 'CONFSTAT conf-no)))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-pers (pers-no &optional queue)
  "Prefetch the pers-stat for person with number PERS-NO.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (if queue
      (lyskom-queue-enter (cons 'PERSSTAT conf-no))
    (lyskom-stack-push lyskom-prefetch-stack (cons 'PERSSTAT conf-no)))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-text (text-no &optional queue)
  "Prefetch the text-stat for the text with number TEXT-NO.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (if queue
      (lyskom-queue-enter(cons 'TEXTSTAT conf-no))
    (lyskom-stack-push lyskom-prefetch-stack (cons 'TEXTSTAT conf-no)))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-textmass (text-no &optional queue)
  "Prefetch the text mass for the text with number TEXT-NO.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (if queue
      (lyskom-queue-enter (cons 'TEXTMASS conf-no))
    (lyskom-stack-push lyskom-prefetch-stack (cons 'TEXTMASS conf-no)))
  (lyskom-continue-prefetch))
  

(defun lyskom-prefetch-textauth (text-no &optional queue)
  "Prefetch the text stat and the author of text number TEXT-NO.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (if queue
      (lyskom-queue-enter (cons 'TEXTAUTH conf-no))
    (lyskom-stack-push lyskom-prefetch-stack (cons 'TEXTAUTH conf-no)))
  (lyskom-continue-prefetch))
  

(defun lyskom-prefetch-text-all (text-no &optional queue)
  "Prefetch all info about the text with number TEXT-NO.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (if queue
      (lyskom-queue-enter (cons 'TEXT-ALL conf-no))
    (lyskom-stack-push lyskom-prefetch-stack (cons 'TEXT-ALL conf-no)))
  (lyskom-continue-prefetch))


(defun lyskom-prefetch-texttree (text-no &optional queue)
  "Prefetch all info about the text with number TEXT-NO and descends recursively.
If QUEUE is non-nil, put the request on it, otherwise put it on 
lyskom-prefetch-stack."
  (if queue
      (lyskom-queue-enter (cons 'TEXTTREE conf-no))
    (lyskom-stack-push lyskom-prefetch-stack (cons 'TEXTTREE conf-no)))
  (lyskom-continue-prefetch))
  

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
  (if lyskom-prefetch-in-action
      (while (< lyskom-pending-prefetch
		lyskom-prefetch-limit)
	; Only increase lyskom-pending-prefetch if there was something to get.
	(if (lyskom-prefetch-one-item)
	    (++ lyskom-pending-prefetch)))))


(defun lyskom-skip-finished-in-queue (queue)
  "Remove all 'DONE entries and queues who's only entry is 'FINISHED."
  (let ((element nil)
	(done nil))
    (while (not done)
      (setq element (lyskom-queue->first queue))
      (if (or (eq element 'DONE)
	      (and (lyskom-queue-p element)
		   (eq (lyskom-queue->first element 'FINISHED))))
	  (lyskom-queue-delete-first queue)
	(setq done t)))))


(defun lyskom-prefetch-one-item ()
  "Get the first element of the prefetch data structure and fetch it.
Return t if an element was prefetched, otherwise return nil."
  (let ((result nil)
	(element nil)
	(prefetch-list (lyskom-stack->all-entries lyskom-prefetch-stack))
	(list-stack (lyskom-stack-create))
    	(done nil))

    ; Remove all finished entries from lyskom-prefetch-stack
    (while (not done)
      (setq element (lyskom-stack->top lyskom-prefetch-stack))
      (if (or (eq element 'DONE)
	      (and (lyskom-queue-p element)
		   (eq (lyskom-queue->first element 'FINISHED))))
	  (lyskom-stack-pop lyskom-prefetch-stack)
	(setq done t)))

    (while (and (not result)
		prefetch-list)
      (setq element (car prefetch-list))
      (cond
       ((eq element 'DONE) nil)
       ((eq element 'FINISHED) nil)

       ; A queue ==> check it out first.
       ((lyskom-queue-p element)
	(lyskom-skip-finished-in-queue element)
	(if (lyskom-queue-isempty element)
	    nil
	  (lyskom-stack-push list-stack prefetch-list)
	  (setq prefetch-list (lyskom-queue->all-entries element))))
       
       ; A simple request?
       ((and (listp element)
	     (memq (car element)
		   '(CONFSTAT PERSSTAT TEXTSTAT TEXTMASS)))
	(setcar prefetch-list 'DONE)
	(lyskom-prefetch-one-request element nil)
	(setq result t))

       ; A complex request?
       ((and (listp element)
	     (memq (car element)
		   '(TEXTAUTH TEXT-ALL TEXTTREE MAP-PART)))
	(let ((queue (lyskom-queue-create)))
	  (setcar prefetch-list queue)
	  (lyskom-prefetch-one-request element queue)
	  (setq result t)))
       
       (t (signal 'lyskom-internal-error 
		  '(lyskom-prefetch-one-item ": unknown key"))))

      (setq prefetch-list (cdr prefetch-list))
      (if (not (or prefetch-list
		   (lyskom-stack-isempty list-stack)))
	  (setq prefetch-list (lyskom-stack-pop list-stack))))))



(defun lyskom-prefetch-one-request (request queue)
  "Prefetch REQUEST. If the request is complex, put the resulting requests on QUEUE."
  (cond
   ((eq (car request) 'CONFSTAT)
    (if (cache-get-conf-stat (cdr request))
	nil
      (initiate-get-conf-stat 'prefetch
			      'lyskom-prefetch-handler (cdr request))))
   ((eq (car request) 'PERSSTAT)
    (if (cache-get-pers-stat (cdr request))
	nil
      (initiate-get-pers-stat 'prefetch
			      'lyskom-prefetch-handler (cdr request))))
   ((eq (car request) 'TEXTSTAT)
    (if (cache-get-text-stat (cdr request))
	nil
      (initiate-get-text-stat 'prefetch
			      'lyskom-prefetch-handler (cdr request))))
   ((eq (car request) 'TEXTMASS)
    (if (cache-get-text (cdr request))
	nil
      (initiate-get-text 'prefetch 'lyskom-prefetch-handler (cdr request))))
   ((eq (car request) 'TEXTAUTH)
    (let ((text-stat (cache-get-text (cdr request))))
      (if text-stat
	  (lyskom-prefetch-conf (text-stat->author text-stat))
	(initiate-get-text 'prefetch 'lyskom-prefetch-textauth-handler 
			   (cdr request) queue))))
   ((eq (car request) 'TEXT-ALL)
    (initiate-get-text-stat 'prefetch 'lyskom-prefetch-text-all-handler
			    (cdr request) queue)
    (initiate-get-text 'prefetch
		       'lyskom-prefetch-handler (cdr request)))
   ((eq (car request) 'TEXTTREE)
    (initiate-get-text-stat 'prefetch 'lyskom-prefetch-texttree-handler
			    (cdr request) queue)
    (initiate-get-text 'prefetch 'lyskom-prefetch-handler (cdr request)))
   (t (signal 'lyskom-internal-error
	      (list "lyskom-prefetch-one-request - unknown key:"
		    (car request))))))


;;; ================================================================
;;;      Functions which handle the results of complex requests.


(defun lyskom-prefetch-textauth-handler (text-stat queue)
  "Prefetch the conf-stat of the author of the text TEXT-STAT.
Put the request on QUEUE."
  (lyskom-stop-prefetch)
  (lyskom-prefetch-conf (text-stat->author text-stat) queue)
  (lyskom-queue-enter queue 'FINISHED)
  (-- lyskom-pending-prefetch)
  (lyskom-start-prefetch))


(defun lyskom-prefetch-text-all-handler (text-stat queue)
  "Prefetch all info neccessary to write the text with text-stat TEXT-STAT.
Put the requests on QUEUE."
  (lyskom-stop-prefetch)
  (lyskom-traverse
   misc
   (text-stat->misc-info-list text-stat)
   (let ((type (misc-info->type misc)))
     (cond
      ((or (eq type 'RECPT)
	   (eq type 'CC-RECPT))
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
  (lyskom-prefetch-conf (text-stat->author text-stat) queue)
  (lyskom-queue-enter queue 'FINISHED)
  (-- lyskom-pending-prefetch)
  (lyskom-start-prefetch))


(defun lyskom-prefetch-texttree-handler (text-stat queue)
  "Prefetch all info neccessary to write the text with text-stat TEXT-STAT.
Then prefetch all info (texttree) of comments.
Put the requests on QUEUE."
  (lyskom-stop-prefetch)
  (lyskom-traverse
   misc
   (text-stat->misc-info-list text-stat)
   (let ((type (misc-info->type misc)))
     (cond
      ((or (eq type 'RECPT)
	   (eq type 'CC-RECPT))
       (lyskom-prefetch-conf (misc-info->recipient-no misc) queue))
      ((eq type 'COMM-IN)
       (lyskom-prefetch-texttree (misc-info->comm-in misc) queue))
      ((eq type 'FOOTN-IN)
       (lyskom-prefetch-texttree (misc-info->footn-in misc) queue))
      ((eq type 'COMM-TO)
       (lyskom-prefetch-textauth (misc-info->comm-to misc) queue))
      ((eq type 'FOOTN-TO)
       (lyskom-prefetch-textauth (misc-info->footn-to misc) queue))
      (t nil))))
  (lyskom-prefetch-conf (text-stat->author text-stat) queue)
  (lyskom-queue-enter queue 'FINISHED)
  (-- lyskom-pending-prefetch)
  (lyskom-start-prefetch))


(defun lyskom-prefetch-handler (&rest data)
  "Handler called after each simple prefetch request is done."
  (-- lyskom-pending-prefetch)
  (lyskom-continue-prefetch))
