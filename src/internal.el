;;;;;
;;;;; $Id: internal.el,v 44.0 1996-08-30 14:46:42 davidk Exp $
;;;;; Copyright (C) 1991, 1996  Lysator Academic Computer Association.
;;;;;
;;;;; This file is part of the LysKOM server.
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
;;;; File: internal.el
;;;;
;;;; Here are internal functions that handles the different kom-queue
;;;; calls. Add a call, apply the handler when a call is done.
;;;;
;;;; Originally written:    ceder
;;;; Completely rewritten:  Inge Wallin
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: internal.el,v 44.0 1996-08-30 14:46:42 davidk Exp $\n"))


;;;; ================================================================
;;;;                          Variables.


(defvar lyskom-call-data nil
  "This is an assoc-list of data for the kom-queues.
Each element on the list has the following format:

(NAME . KOM-QUEUE)

NAME    is an atom, the name of the kom-queue.	A kom-queue is a way
        to send questions to the LysKOM server and deal with the replies 
        in a controlled way.

KOM-QUEUE is a kom-queue.")

(make-variable-buffer-local 'lyskom-call-data)


;;; ================================================================
;;;                   Data type kom-queue

;;;
;;; Each kom-queue consists of the following 4 fields:
;;;     PENDING -
;;;         a list where each element represents a call to a service on 
;;;         the server, or a call to lyskom-collect, lyskom-use or 
;;;         lyskom-run. The elements are lists. The first element on each 
;;;         list is a key to what it represents, as described below.
;;;     COLLECT-FLAG - 
;;;          t means this queue is collecting results for a future 
;;;          lyskom-use of lyskom-list-use. The results are collected
;;;          on collect-queue
;;;     COLLECT-QUEUE -
;;;          This is where the results described above are collected.
;;;     HALTED -
;;;          An integer counting the number of times this queue is halted.
;;;          A call to lyskom-halt increments this counter, a call to
;;;          lyskom-resume decrements it.
;;;
;;;
;;; The items on PENDING is one of the following:
;;;
;;; 	('CALL REF-NO PARSER PARSER-DATA HANDLER HANDLER-DATA)
;;; 		A call that has not yet returned.
;;; 
;;; 	('PARSED RESULT HANDLER HANDLER-DATA)
;;; 		A call that has returned, but the result can not be
;;; 		handled until all previous calls has returned.
;;; 
;;; 	('COLLECT)
;;; 		Marks the start of a sequence of results that will be
;;; 		handled by a multi-handler or multi-list-handler.
;;; 
;;; 	('COLLECT-IGNORE)
;;; 		Marks the start of a sequence of results that will be 
;;; 		handled by a multi-handler of multi-list-handler with
;;; 		errors stripped away.
;;; 
;;; 	('USE MULTI-HANDLER MULTI-HANDLER-DATA)
;;; 		Marks the end of a sequence. MULTI-HANDLER is called
;;; 		when all calls before this element have been handled.
;;; 		MULTI-HANDLER is a function whose first arguments are
;;; 		the results from calls between previous COLLECT and
;;; 		this USE.  MULTI-HANDLER-DATA is optional.  If it
;;; 		exists it is a list of more arguments to send to
;;; 		MULTI-HANDLER.
;;; 
;;; 	('LIST-USE MULTI-HANDLER MULTI-HANDLER-DATA)
;;; 		Marks the end of a sequence. MULTI-HANDLER is called
;;; 		when all calls before this element have been handled.
;;; 		MULTI-HANDLER is a function whose first argument is a
;;; 		list of all results from calls between previous
;;; 		COLLECT and this LIST-USE. MULTI-HANDLER-DATA is
;;; 		optional. If it exists it is a list of more arguments
;;; 		to send to MULTI-HANDLER.
;;; 
;;; 	('RUN FUNCTION FUNCTION-ARGS)
;;; 		Run FUNCTION when all calls before this have been handled.
;;; 


(defun kom-queue-create ()
  "Creates a new instance of an empty kom-queue."
  (vector (lyskom-queue-create)
	  nil
	  (lyskom-queue-create)
	  0))


(defun kom-queue->pending (queue)
  "Returns the pending field of the kom-queue QUEUE."
  (elt queue 0))


(defun kom-queue->collect-flag (queue)
  "Returns the collect-flag field of the kom-queue QUEUE."
  (elt queue 1))


(defun kom-queue->collect-queue (queue)
  "Returns the collect-queue field of the kom-queue QUEUE."
  (elt queue 2))


(defun set-kom-queue-collect-flag (queue new-val)
  "Set the collect-flag field of the kom-queue QUEUE to NEW-VAL."
  (aset queue 1 new-val))


(defun kom-queue-halt (queue)
  "Halts the kom-queue QUEUE."
  (aset queue 3 (1+ (elt queue 3))))


(defun kom-queue-resume (queue)
  "Resume execution on the kom-queue QUEUE."
  (if (eq (elt queue 3) 0)
      (signal 'lyskom-internal-error
	      (list "kom-queue-resume called on an unhalted queue: "
		    queue))
    (aset queue 3 (1- (elt queue 3)))))


(defun kom-queue-is-halted (queue)
  "Return t if the kom-queue QUEUE is halted at least once."
  (> (elt queue 3) 0))



;;; ================================================================
;;;               Entry points to this communication packet.


(defun lyskom-collect (kom-queue)
  "Collect the results of future calls via KOM-QUEUE.
The results of the calls will be available to the multi-handler.
See lyskom-use and lyskom-list-use."
  (lyskom-call-add kom-queue 'COLLECT))


(defun lyskom-collect-ignore-err (kom-queue)
  "Collect the result of future calls via KOM-QUEUE.
The result of the calls will be available to the multi-handler with the calls
producing errors stripped."
  (lyskom-call-add kom-queue 'COLLECT-IGNORE))


(defun lyskom-use (kom-queue multi-handler &rest multi-handler-data)
  "Use the previously collected results from calls to the server.
Args: KOM-QUEUE MULTI-HANDLER &rest MULTI-HANDLER-DATA

MULTI-HANDLER is a function that is called when all previous results
have been handled. MULTI-HANDLER-DATA is a list of additional
arguments the multi-handler wants.

See also lyskom-list-use."
  (lyskom-call-add kom-queue 'USE multi-handler multi-handler-data)
  (lyskom-check-call kom-queue))


(defun lyskom-list-use (kom-queue multi-handler &rest multi-handler-data)
  "Use the previously collected results from calls to the server.
Args: KOM-QUEUE MULTI-HANDLER &rest MULTI-HANDLER-DATA

MULTI-HANDLER is a function that is called when all previous
results have been handled. The first argument is a list of
the results. MULTI-HANDLER-DATA is a list of additional
arguments the multi-handler wants.

The difference between lyskom-use and lyskom-list-use is the way
the MULTI-HANDLER receives the data. lyskom-list-use sends them as
a list, lyskom-use as different parameters."
  (lyskom-call-add kom-queue 'LIST-USE multi-handler multi-handler-data)
  (lyskom-check-call kom-queue))


(defun lyskom-run (kom-queue function &rest function-args)
  "Call a function when all calls have been handled.
Args: KOM-QUEUE FUNCTION &rest FUNCTION-ARGS
Register a FUNCTION that shall be called with FUNCTION-ARGS when
all previous calls to the server via KOM-QUEUE have been handled."
  (lyskom-call-add kom-queue 'RUN function function-args)
  (lyskom-check-call kom-queue))


(defun lyskom-halt (queue-name)
  "Prohibit execution of handlers on QUEUE-NAME.
The execution will resume when (lyskom-resume KOM-QUEUE) is called."
  (let ((queue-pair (assoc queue-name lyskom-call-data)))
    (if (null queue-pair)
	(setq queue-pair (lyskom-add-new-queue queue-name)))
    (kom-queue-halt (cdr queue-pair))))
    

(defun lyskom-resume (kom-queue)
  "Resume execution of waiting handlers on KOM-QUEUE.
See documentation for lyskom-halt."
  (let ((queue (assoc kom-queue lyskom-call-data)))
    (cond
     ((null queue)			;A new kom-queue?
      (signal 'lyskom-internal-error
	      (list "lyskom-resume called on an unused queue:"
		    kom-queue)))
     ((kom-queue-is-halted (cdr queue)) ;A halted queue?
      (kom-queue-resume (cdr queue))    ;Resume execution on the queue.
      (lyskom-check-call kom-queue))	;Run any pending handlers.
     (t					;The queue was not halted. This
      (signal 'lyskom-internal-error	;is an error.
	      (list "lyskom-resume:"
		    kom-queue "(not halted)"))))))


(defun lyskom-call (kom-queue ref-no handler handler-data
			       parser &rest parser-data)
  "Add information about a call that has not yet returned to kom-queue.
Arguments: KOM-QUEUE REF-NO HANDLER HANDLER-DATA
	   PARSER &rest PARSER-DATA."
  (lyskom-call-add kom-queue 'CALL ref-no 
		   parser parser-data handler handler-data))

(defun lyskom-fake-call (kom-queue ref-no handler handler-data)
  "Add information about a call that will not return from the server,
but will be filled in by some other function."
  (lyskom-call-add kom-queue 'CALL ref-no
                   nil nil handler handler-data))

(defun lyskom-complete-call (kom-queue ref-no result)
  "Force a call placed on KOM-QUEUE with reference number REF-NO to return 
RESULT. This should only be used to complete calls placed on the queue using
lyskom-fake-call, or the parser might get confused."
  (let ((call-info (lyskom-locate-ref-no kom-queue ref-no)))
    (if call-info
        (progn
          (lyskom-tr-call-to-parsed call-info result)
          (lyskom-check-call kom-queue)))))


;;; This is used by z-initiate-get-map, which is not used.
;;; This was a temporary solution.

;;(defun lyskom-kom-queue-collect-p (queue-name)
;;  "Return t if the kom-queue QUEUE-NAME has an unmatched 'COLLECT item.
;;It is illegal to call lyskom-collect or lyskom-collect-ignore-err on
;;the kom-queue if and only if this function returns t."
;;  (let* ((queue (cdr-safe (assoc queue-name lyskom-call-data)))
;;	 (pending (lyskom-queue->all-entries (kom-queue->pending queue)))
;;	 (collect-flg nil)
;;	 (type nil))
;;    (while (and queue pending)
;;      (setq type (car (car pending)))
;;      (setq pending (cdr pending))
;;      (cond
;;       ((eq type 'COLLECT)
;;	(setq collect-flg t))
;;       ((eq type 'COLLECT-IGNORE)
;;	(setq collect-flg t))
;;       ((eq type 'USE)
;;	(setq collect-flg nil))
;;       ((eq type 'LIST-USE)
;;	(setq collect-flg nil))))
;;    collect-flg))


;;;; ================================================================
;;;;                     Internal functions.



(defun lyskom-add-new-queue (queue-name)
  "Add QUEUE-NAME to lyskom-call-data as an empty queue.
Return a dotted pair consisting of the QUEUE-NAME and the new queue."
  (let ((new-queue-list (list (cons queue-name (kom-queue-create)))))
    (if (null lyskom-call-data)
	(setq lyskom-call-data new-queue-list)
      (nconc lyskom-call-data new-queue-list))
    (car new-queue-list)))

(defun lyskom-set-queue-priority (queue-name priority)
  (put queue-name 'lyskom-queue-priority priority))

(defun lyskom-queue-priority (queue-name)
  (or (get queue-name 'lyskom-queue-priority)
      0))

(defun lyskom-call-add (queue-name type &rest data)
  "Add an entry to the kom-queue QUEUE-NAME. The entry is of type TYPE
and third argument DATA contains the rest of the necessary data."
  (let ((queue (assoc queue-name lyskom-call-data)))
    (if (null queue)
	(setq queue (lyskom-add-new-queue queue-name)))
    (lyskom-queue-enter (kom-queue->pending (cdr queue))
			(cons type data))))


(defun lyskom-send-packet (kom-queue string)
  "Send a packet to the server.
Add info on lyskom-pending-calls. Update lyskom-ref-no.
Args: KOM-QUEUE STRING."

  ;; Queue it
  (lyskom-queue-enter (aref lyskom-output-queues
			    (lyskom-queue-priority kom-queue))
		      (cons lyskom-ref-no string))
  (setq lyskom-pending-calls
	(cons (cons lyskom-ref-no kom-queue)
	      lyskom-pending-calls))
  (++ lyskom-ref-no)
  
  ;; Send something from the output queues
  (lyskom-check-output-queues))

(defun lyskom-check-output-queues ()
  "Check for pending calls to the server.
Send calls from queues with higher priority first, and make sure that at
most lyskom-max-pending-calls are sent to the server at the same time."
  (catch 'done
    (let ((i 9))
      (while (< lyskom-number-of-pending-calls
		lyskom-max-pending-calls)
	(while (lyskom-queue-isempty (aref lyskom-output-queues i))
	  (-- i)
	  (if (< i 0) (throw 'done nil)))
	(let ((entry (lyskom-queue-delete-first
		      (aref lyskom-output-queues i))))
	  (++ lyskom-number-of-pending-calls)
	  (lyskom-process-send-string
	   lyskom-proc
	   (concat (car entry) (cdr entry) "\n")))))))

(defun lyskom-decrease-pending-calls ()
  "A reply has come.
Send a pending call or decrease lyskom-number-of-pending-calls."
  (-- lyskom-number-of-pending-calls)
  (if (< lyskom-number-of-pending-calls 0)
      (setq lyskom-number-of-pending-calls 0))
  (lyskom-check-output-queues))
	

(defun lyskom-process-send-string (process string)
  "Send PROCESS the contents of STRING.
STRING is split in several parts if the operating system can't deal
with big strings."
  (let ((tries 0))
  (while
      (condition-case err
	  (progn (lyskom-process-send-string-2 process string)
		 nil)
	(file-error
	 (if lyskom-debug-communications-to-buffer
	     (lyskom-debug-insert process "Error: " (format "%s" err))
;;;	     (save-excursion
;;;	       (set-buffer (get-buffer-create
;;;			    lyskom-debug-communications-to-buffer-buffer))
;;;	       (save-excursion
;;;		 (goto-char (point-max))
;;;		 (insert "\n" 
;;;			 (format "%s" process)
;;;			 (concat "Error: " (format "%s" err))))
;;;	       (set-buffer (process-buffer process)))
	   )
	 (cond
	  ((and (string= "writing to process" (car (cdr err)))
		(or (string= "message too long" (car (cdr (cdr err))))
		    (string= "no more processes" (car (cdr (cdr err)))))
		(> lyskom-max-packet-size 1))
	   ;; Seems to be impossible to write too long strings to TCP/IP.
	   ;; This happens on a Sequence Balance with packets longer than
	   ;; 2048 bytes. Decrease lyskom-max-packet-size and retry.
	   (setq lyskom-max-packet-size (/ lyskom-max-packet-size 2))
	   t)

	  ((and (string= "writing to process" (car (cdr err)))
		(string= "host is unreachable" (car (cdr (cdr err)))))
	   ;; The net is currently shaky. We try again in a while.
	   (lyskom-message "%s" (lyskom-format 'shaky-tcp
					       (make-string (++ tries) ?.)))
	   (sit-for 4)
	   (lyskom-message "%s" (lyskom-get-string 'retrying-tcp))
	   t)
	  
	  (t
	   ;; It was some unknown file-error. Pass it down.
	   (signal (car err) (cdr err)))))))))


(defun lyskom-process-send-string-2 (process string)
  "Send PROCESS the contents of STRING as input.
PROCESS may be a process name. At most lyskom-max-packet-size bytes
is sent with each packet. If STRING is longer it is splitted."
  (cond
   ((<= (length string) lyskom-max-packet-size)
    (process-send-string
     process
     (progn
       (if lyskom-debug-communications-to-buffer
	   (lyskom-debug-insert process ">>>>>> " string))
       string)))
   (t
    (let ((i 0))
      (while (< i (length string))
	(process-send-string
	 process
	 (let ((string (substring string
				  i
				  (min (length string)
				       (+ i lyskom-max-packet-size)))))
	   (if lyskom-debug-communications-to-buffer
	       (save-excursion
		 (set-buffer (get-buffer-create 
			      lyskom-debug-communications-to-buffer-buffer))
		 (save-excursion
		   (goto-char (point-max))
		   (insert "\n"
			   (format "%s" process)
			   (concat ">>>>>> " string)))
		 (set-buffer (process-buffer process))))
	   string))
	(setq i (+ i lyskom-max-packet-size)))))))


(defun lyskom-check-call (queue-name)
  "Check lyskom-call-data and call handlers, multi-handlers and functions.
Args: QUEUE-NAME.
HALTED -> stop
CALL -> stop
PARSED -> handle ->
       -> collect-flag? 'COLLECT -> add on temporary resultlist.
	 	        'COLLECT-IGNORE -> add on temporary resultlist if not 
			 		   error.
		        no  -> delete from lyskom-call-data.
COLLECT -> collect-flag? yes -> error!
			 no  -> set collect-flag.
COLLECT-IGNORE -> collect-flag? yes -> error!
				no -> set collect-flag.
USE ->      call handler. Delete previous parts.
LIST-USE -> call handler. Delete previous parts.
RUN ->      call function. Delete. Not allowed inside COLLECT/USE."
  (let* ((queue (cdr-safe (assoc queue-name lyskom-call-data)))
         (type nil)
         (first-pending (lyskom-queue->first (kom-queue->pending queue))))
    (while (and queue
                (not (or (kom-queue-is-halted queue)
                         (lyskom-queue-isempty (kom-queue->pending queue))
                         (eq (car first-pending) 'CALL))))
      (setq type (car first-pending))

      (cond

       ((eq type 'PARSED)
	(kom-queue-halt queue)
	(unwind-protect
	    (lyskom-apply-handler first-pending)
	  (kom-queue-resume queue))
        (if (or (eq (kom-queue->collect-flag queue) 'COLLECT)
                (and (eq (kom-queue->collect-flag queue) 'COLLECT-IGNORE)
                     (car (cdr first-pending))))
            (lyskom-queue-enter (kom-queue->collect-queue queue)
                                (car (cdr first-pending)))))

       ((eq type 'COLLECT)
        (if (kom-queue->collect-flag queue)
            (signal 'lyskom-internal-error
                    '("lyskom-check-call COLLECT."))
          (set-kom-queue-collect-flag queue 'COLLECT)
          (lyskom-queue-make-empty (kom-queue->collect-queue queue))))

       ((eq type 'COLLECT-IGNORE)
        (if (kom-queue->collect-flag queue)
            (signal 'lyskom-internal-error
                    '("lyskom-check-call COLLECT-IGNORE."))
          (set-kom-queue-collect-flag queue 'COLLECT-IGNORE)
          (lyskom-queue-make-empty (kom-queue->collect-queue queue))))

       ((eq type 'USE)
        (if (not (kom-queue->collect-flag queue))
            (signal 'lyskom-internal-error
                    '("lyskom-check-call USE.")))
        (kom-queue-halt queue)
        (unwind-protect
            (lyskom-apply-multi-handler
             first-pending (lyskom-queue->all-entries
                            (kom-queue->collect-queue queue)))
          (kom-queue-resume queue))
        (set-kom-queue-collect-flag queue nil))

       ((eq type 'LIST-USE)
        (if (not (kom-queue->collect-flag queue))
            (signal 'lyskom-internal-error
                    '("lyskom-check-call LIST-USE.")))
        (kom-queue-halt queue)
        (unwind-protect
            (lyskom-apply-multi-list-handler
             first-pending (lyskom-queue->all-entries
                            (kom-queue->collect-queue queue)))
          (kom-queue-resume queue))
        (set-kom-queue-collect-flag queue nil))

       ((eq type 'RUN)
        (if (kom-queue->collect-flag queue)
            (signal 'lyskom-internal-error
                    '("lyskom-check-call RUN.")))
        (kom-queue-halt queue)
        (unwind-protect
            (lyskom-apply-function first-pending)
          (kom-queue-resume queue)))
       
       (t (signal 'lyskom-internal-error
                  (list 'lyskom-check-call
                        "unknown key:"
                        (car first-pending)))))

      (lyskom-queue-delete-first (kom-queue->pending queue))
      (setq first-pending (lyskom-queue->first (kom-queue->pending queue))))))

;;; Quit is ok to press while the handler is running but we should catch it 
;;; here
(defun lyskom-apply-handler (pending)
  "Apply a handler. Args: PENDING.
PENDING is an entry of the list as described in documentation for the variable
lyskom-call-data. The car on the list must be a PARSED:
	('PARSED RESULT HANDLER HANDLER-DATA)"
  (let ((inhibit-quit nil))
    (condition-case error
        (if (car (cdr (cdr pending)))
            (apply (car (cdr (cdr pending))) ;Handler
                   (car (cdr pending))	;Result
                   (car (cdr (cdr (cdr pending)))))) ;Handler-data
      (quit (beep)))))

(defun lyskom-apply-multi-handler (pending result-list)
  "Apply a handler for a lyskom-collect - lyskom-use construct."
  (let ((inhibit-quit nil))
    (condition-case error
        (apply (car (cdr pending))	;Multi-handler
               (nconc result-list
                      (car (cdr (cdr pending))))) ;Multi-handler-data
      (quit (beep)))))


(defun lyskom-apply-multi-list-handler (pending result-list)
  "Apply a handler for a lyskom-collect - lyskom-list-use construct."
  (let ((inhibit-quit nil))
    (condition-case error
        (apply (car (cdr pending))	;Multi-handler
               (cons result-list
                     (car (cdr (cdr pending))))) ;Multi-handler-data
      (quit (beep)))))
    
(defun lyskom-apply-function (pending)
  (setcar pending 'HALTED)
  (apply (car (cdr pending))	    ;Function.
	 (car (cdr (cdr pending)))))  ;Function-args.
