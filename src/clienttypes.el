;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: clienttypes.el,v 44.10 2000-08-14 15:56:25 byers Exp $
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
;;;; File: clienttypes.el
;;;;
;;;; This file contains primitives for the different data types
;;;; in the lyskom elisp client. The types in here are only used
;;;; within this lyskom client.
;;;;
;;;; Authors: Linus Tolke and Inge Wallin
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: clienttypes.el,v 44.10 2000-08-14 15:56:25 byers Exp $\n"))


;;; ================================================================
;;;                            read-info

;;; A read-info is used by to tell the client which text to 
;;; show next and in which order to step through the conferences 
;;; with unread texts. New read-infos are created and deleted
;;; all the time within a session.
;;;
;;; The type is one of the following:
;;;   REVIEW      - Default review type, created by the kom-review-by-to
;;;		    and the kom-review-comments
;;;   REVIEW-TREE - List of texts created by one of the tree-reading 
;;;		    commands: kom-find-root-review, kom-review-tree
;;;   REVIEW-MARK - List of texts created by the review-mark command
;;;   COMM-IN     - Type containing the list of comments to a text
;;;   FOOTN-IN    - Type containing the list of footnotes to a text
;;;   CONF        - Basic type of unread in a conf.
;;;   RE-EDIT-TEXT - Re-edit a failed submission. Buffer is in misc.
;;;   PRI-SESSION - Go to prioritized session.
;;;
;;;   The types REVIEW-TREE, COMM-IN and FOOTN-IN are created for new for
;;;   every text read (recursively) when appropriate.
;;;
;;;   The difference between REVIEW and REVIEW-MARK is just that there
;;;   generate different prompts and different text from kom-review-stack.
;;;


;;; read-info

(defsubst lyskom-create-read-info (type
				conf-stat
				priority
				text-list
				&optional comm-to forward misc)
  "Create a read-info from all parameters."
  ;; The last nil is for the unfetched-texts pair (first . last)
  ;; This field is only applicable in read-infos of type CONF where
  ;; it shows which part of the map for this conference that has not
  ;; yet been fetched.
  (cons
   'READ-INFO
   (vector type conf-stat priority text-list comm-to forward nil misc)))

(defsubst read-info->type (read-info)
  "Get type from read-info."
  (elt (cdr read-info) 0))

(defsubst read-info->conf-stat (read-info)
  "Get conf-stat from read-info."
  (elt (cdr read-info) 1))

(defsubst read-info->priority (read-info)
  "Get priority from read-info."
  (elt (cdr read-info) 2))

(defsubst read-info->text-list (read-info)
  "Get text-list from read-info."
  (elt (cdr read-info) 3))

(defsubst read-info->comm-to (read-info)
  "Get comm-to from read-info."
  (elt (cdr read-info) 4))

(defsubst read-info->forward (read-info)
  "Get forward from read-info."
  (elt (cdr read-info) 5))

(defsubst read-info->unfetched-texts (read-info)
  "Get forward from read-info."
  (elt (cdr read-info) 6))

(defsubst read-info->misc (read-info)
  "Get forward from read-info."
  (elt (cdr read-info) 7))

(defsubst set-read-info->type (read-info newval)
  "Set type in read-info to NEWVAL."
  (aset (cdr read-info) 0 newval))

(defsubst set-read-info->conf-stat (read-info newval)
  "Set conf-stat in read-info to NEWVAL."
  (aset (cdr read-info) 1 newval))

(defsubst set-read-info->priority (read-info newval)
  "Set priority in read-info to NEWVAL."
  (aset (cdr read-info) 2 newval))

(defsubst set-read-info->text-list (read-info newval)
  "Set text-list in read-info to NEWVAL."
  (aset (cdr read-info) 3 newval))

(defsubst set-read-info->comm-to (read-info newval)
  "Set comm-to in read-info to NEWVAL."
  (aset (cdr read-info) 4 newval))

(defsubst set-read-info->forward (read-info newval)
  "Set forward in read-info to NEWVAL."
  (aset (cdr read-info) 5 newval))

(defsubst set-read-info->unfetched-texts (read-info newval)
  "Set forward in read-info to NEWVAL."
  (aset (cdr read-info) 6 newval))

(defsubst set-read-info->misc (read-info newval)
  "Set forward in read-info to NEWVAL."
  (aset (cdr read-info) 7 newval))


(defsubst lyskom-read-info-p (object)
  "Return t if OBJECT is a read-info."
  (eq (car-safe object) 'READ-INFO))


(defsubst read-info-append-text-list (read-info texts)
  (text-list->append (read-info->text-list read-info) texts))

(defsubst read-info-enter-text-last (read-info text-no)
  (read-info-append-text-list read-info (list text-no)))

				   
;;; ================================================================
;;;                          read-list


;;; Constructor:

(defsubst lyskom-create-read-list ()
  "Create an empty read-list."
  (cons 'READ-LIST nil))


;;; Predicates:

(defsubst read-list-isempty (read-list)
  "Return t if READ-LIST is empty, otherwise return nil."
  (null (cdr read-list)))


;;; Selectors:

(defsubst read-list->first (read-list)
  "Return the first entry in READ-LIST, or nil if empty."
  (car-safe (cdr read-list)))


(defsubst read-list->nth (read-list n)
  "Args: READ-LIST N
Return element N in READ-LIST or nil if outside the range.
The range of valid values for N is [0, num-entries - 1]."
  (elt (cdr read-list) n))


(defsubst read-list->all-entries (read-list)
  "Return a list of all entries in READ-LIST."
  (cdr read-list))


;;; Other functions:

(defsubst read-list-length (read-list)
  "Return the number of entries in READ-LIST."
  (1- (length read-list)))


;;; Modifiers:

(defsubst set-read-list-empty (read-list)
  "Empty READ-LIST destructively."
  (setcdr read-list nil))


(defsubst set-read-list-del-first (read-list)
  "Delete the first entry of READ-LIST if there is one."
  (if (cdr read-list)
      (setcdr read-list (cdr (cdr read-list)))))


(defsubst read-list-enter-first (read-info read-list)
  "Enter READ-INFO first into READ-LIST."
  (setcdr read-list (cons read-info (cdr read-list))))


(defun read-list-enter-text (text-no recipient rlist)
  "Args: TEXT-NO RECIPIENT RLIST.
Add the new text TEXT-NO to any RECIPIENT found in RLIST.
RECIPIENT is a conf-stat.
Returns t if there was a conference to insert this text into."
  (let ((inserted nil))
    (lyskom-traverse
     read-info (cdr rlist)
     (cond
      ((and (eq 'CONF (read-info->type read-info))
	    (= (conf-stat->conf-no recipient)
	       (conf-stat->conf-no (read-info->conf-stat read-info))))
       (read-info-enter-text-last read-info text-no)
       (setq inserted t))))
    inserted))


(defun read-list-delete-text (text-no rlist)
  "Destructively delete all occurances of TEXT-NO from RLIST.
RLIST is a list of read-info.
Entries of the type REVIEW, REVIEW-TREE or REVIEW-MARK are not changed
except if they were empty in which case they are removed.
Returns the modified RLIST.
TEXT-NO may be nil, in which case only empty read-infos on RLIST are removed."
  (let* ((prev rlist)			;"Previous" cons-celll
	 (curr (cdr rlist)))		;Current cons-cell
    (while curr
      (if text-no
	  (cond
	   ((let ((type (read-info->type (car curr)))) 
	      (or (eq type 'REVIEW)	; Don't change REVIEW et c.
		  (eq type 'REVIEW-TREE)
		  (eq type 'REVIEW-MARK))))
	   (t				; Do change all other entries.
	    (let ((tl  (read-info->text-list (car curr))))
	      (text-list->delq tl text-no)))))
      
      ;; Delete this element from RLIST if the text-list became or was empty.
      
      (if (text-list->empty (read-info->text-list (car curr)))
	  (setcdr prev (cdr curr))
	(setq prev curr))
      (setq curr (cdr curr))))
  rlist)


(defun read-list-enter-read-info (read-info rlist &optional before)
  "Destructively insert READ-INFO in RLIST, sorted by priority.
RLIST is a list of read-info.
Args: READ-INFO RLIST &optional BEFORE.
A new item with the same priority as an item that is alreay on the list
will nomally be inserted after the old one, but if BEFORE is non-nil it
will be inserted before it."
  (let ((pri (+ (if before 0 -1)
		(read-info->priority read-info)))
	(continue t)
	(conf-stat (read-info->conf-stat read-info))
	(type (read-info->type read-info)))
    (while continue
      (cond
       ;; This case was added by davidk 960925. It is not used from
       ;; everywhere, but at least lyskom-enter-map-in-to-do-list
       ;; should become more efficient.
       ((and (eq type 'CONF)
	     (eq (read-info->type (car (cdr rlist))) 'CONF)
	     (eq conf-stat (read-info->conf-stat (car (cdr rlist)))))
	(read-info-append-text-list
	 (car (cdr rlist))
	 (text-list->texts (read-info->text-list read-info)))
	(setq continue nil)) 
       
       ((null (cdr rlist)) 
	(setcdr rlist (list read-info))
	(setq continue nil))
       ((>= pri (read-info->priority (car (cdr rlist))))
	(setcdr rlist (cons read-info (cdr rlist)))
	(setq continue nil))
       (t 
	(setq rlist (cdr rlist)))))))

(defun read-list-delete-read-info (conf-no rlist)
  "Destructively removes all the entries for the conf CONF-NO in RLIST.
RLIST is a list of read-info."
  (while (cdr rlist)
    (if (eq (conf-stat->conf-no (read-info->conf-stat
				      (car (cdr rlist))))
	    conf-no)
	(setcdr rlist (cdr (cdr rlist)))
      (setq rlist (cdr rlist)))))
 
	  
(defun read-list-rotate (read-list)
  "Put the first element of READ-LIST last in the same list. The second
element will be the new first element."
  (if (> (length read-list) 2)
      (let ((first (cdr read-list))
	    (last  (cdr read-list)))
	(while (cdr last)
	  (setq last (cdr last)))
	(setcdr last first)
	(setcdr read-list (cdr first))
	(setcdr first nil)))
  read-list)    
	  

;;; ================================================================
;;;                      A simple queue

;;;
;;; This is a simple implementation of a queue.  The only thing you
;;; can do with it is stuff things at the back of it and remove
;;; things from the front of it.  (...and of course do a few tests)
;;;

;;; Author: Inge Wallin


(defun lyskom-queue-create ()
  "Create an empty queue."
  (cons 'QUEUE (cons nil nil)))


(defsubst lyskom-queue-enter (queue element)
  "Enter last into the queue QUEUE the ELEMENT."
  (let ((elementcell (cons element nil)))
    (if (null (car (cdr queue)))
	; QUEUE is empty
	(setcar (cdr queue)
		(setcdr (cdr queue) 
			elementcell))
      (setcdr (cdr (cdr queue))
	      elementcell)
      (setcdr (cdr queue)
	      elementcell))))


(defsubst lyskom-queue-p (queue)
  "Return t if QUEUE is a queue, otherwise return nil."
  (eq (car-safe queue) 'QUEUE))


(defsubst lyskom-queue-isempty (queue)
  "Return t if QUEUE is empty, otherwise return nil."
  (null (car (cdr queue))))


(defsubst lyskom-queue->first (queue)
  "Return the first element of QUEUE or nil if it is empty."
  (car-safe (car (cdr queue))))


(defsubst lyskom-queue->all-entries (queue)
  "Return all elements of QUEUE (nil if it is empty)."
  (car-safe (cdr queue)))


(defsubst lyskom-queue->last (queue)
  "Return the lastelement of QUEUE or nil if it is empty."
  (car-safe (cdr (cdr queue))))


(defsubst lyskom-queue-make-empty (queue)
  "Make the queue QUEUE empty."
  (setcdr queue (cons nil nil)))

(defsubst lyskom-queue-set-data (queue data)
  (lyskom-queue-make-empty queue)
  (setcdr queue (cons data (last data))))


(defsubst lyskom-queue-delete-first (queue)
  "Delete the first element of QUEUE and return it. If QUEUE is empty
return nil and do nothing."
  (if (lyskom-queue-isempty queue)
      nil
    (prog1
	(lyskom-queue->first queue)
      (setcar (cdr queue)
	      (cdr (car (cdr queue))))
      (if (null (car (cdr queue)))
	  (setcdr (cdr queue) nil)))))


;;; ================================================================
;;;                           A simple stack

;;; 
;;; This is an implementation of a simple stack.
;;;

;;; Author: Inge Wallin


(defun lyskom-stack-create ()
  "Create an empty stack"
  (cons 'STACK nil))


(defun lyskom-stack-p (stack)
  "Return t if STACK is a lyskom-stack, otherwise return nil."
  (eq (car-safe stack) 'STACK))


(defun lyskom-stack-push (stack element)
  "Push the second arg ELEMENT onto the first arg STACK"
  (setcdr stack (cons element (cdr stack))))


(defun lyskom-stack-pop (stack)
  "Remove the topmost element from STACK and return it. If the stack
is empty, return nil"
  (prog1
      (car-safe (cdr stack))
    (setcdr stack (cdr-safe (cdr stack)))))


(defun lyskom-stack->top (stack)
  "Return the topmost element of STACK or nil if it is empty."
  (car-safe (cdr stack)))


(defun lyskom-stack->length (stack)
  "Return the number of elements on STACK."
  (length (cdr stack)))


(defun lyskom-stack->nth (stack n)
  "Return element no (second arg) N of the stack (first arg) STACK.
N counts from zero. If the length of STACK is less than N, nil is returned."
  (nth n (cdr stack)))


(defun lyskom-stack->all-entries (stack)
  "Return a list of all entries in STACK.
The element last pushed is first in the list."
  (cdr stack))


(defsubst lyskom-stack-isempty (stack)
  "Returns non-nil if the STACK is empty."
  (not (cdr stack)))


;;; ================================================================
;;;                          who-buffer-info

;;; Author: Inge Wallin


;;; Constructor:

(defun lyskom-create-who-buffer-info (info
				      start-marker
				      end-marker)
  "Create a who-buffer-info from all parameters."
  (cons
   'WHO-BUFFER-INFO
   (vector info start-marker end-marker )))


;;; Selectors:

(defun who-buffer-info->info (who-buffer-info)
  "Get info from who-buffer-info."
  (elt (cdr who-buffer-info) 0))

(defun who-buffer-info->start-marker (who-buffer-info)
  "Get start-marker from who-buffer-info."
  (elt (cdr who-buffer-info) 1))

(defun who-buffer-info->end-marker (who-buffer-info)
  "Get end-marker from who-buffer-info."
  (elt (cdr who-buffer-info) 2))


;;; Modifiers:

(defun set-who-buffer-info->info (who-buffer-info newval)
  "Set info in who-buffer-info to NEWVAL."
  (aset (cdr who-buffer-info) 0 newval))

(defun set-who-buffer-info->start-marker (who-buffer-info newval)
  "Set start-marker in who-buffer-info to NEWVAL."
  (aset (cdr who-buffer-info) 1 newval))

(defun set-who-buffer-info->end-marker (who-buffer-info newval)
  "Set end-marker in who-buffer-info to NEWVAL."
  (aset (cdr who-buffer-info) 2 newval))



;;; Predicate:

(defun lyskom-who-buffer-info-p (object)
  "Return t if OBJECT is a who-buffer-info."
  (eq (car-safe object) 'WHO-BUFFER-INFO))


;;; ================================================================
;;;			format-props

(defun make-format-props (arg propl)
  (cons 'format-props (vector arg propl)))

(defsubst format-props-p (arg)
  (eq 'format-props (car-safe arg)))

(defsubst format-props->arg (arg)
  (elt (cdr arg) 0))

(defsubst format-props->propl (arg)
  (elt (cdr arg) 1))

;;;
;;;	Help functions
;;;



;;; ================================================================
;;;			format-state

(defun make-format-state (format-string
			  start
			  argl
			  result)
  (cons 'format-state
	(vector format-string start argl (length argl) result nil nil)))

(defsubst format-state-p (arg)
  (eq 'format-state (car-safe arg)))

(defsubst format-state->format-string (arg)
  (elt (cdr arg) 0))

(defsubst set-format-state->format-string (arg str)
  (aset (cdr arg) 0 str))

(defsubst format-state->start (arg)
  (elt (cdr arg) 1))

(defsubst set-format-state->start (arg pos)
  (aset (cdr arg) 1 pos))

(defsubst format-state->args (arg)
  (elt (cdr arg) 2))

(defsubst set-format-state->args (arg argl)
  (aset (cdr arg) 2 argl)
  (aset (cdr arg) 3 (length argl)))

(defsubst format-state->args-length (arg)
  (elt (cdr arg) 3))

(defsubst format-state->result (arg)
  (elt (cdr arg) 4))

(defsubst set-format-state->result (arg output-list)
  (aset (cdr arg) 4 output-list))

(defsubst format-state->delayed-propl (arg)
  (elt (cdr arg) 5))

(defsubst set-format-state->delayed-propl (arg propl)
  (aset (cdr arg) 5 propl))

(defsubst format-state->delayed-content (arg)
  (elt (cdr arg) 6))

(defsubst set-format-state->delayed-content (arg string)
  (aset (cdr arg) 6 string))

;;; ================================================================


;;; ======================================================================
;;;
;;; collector
;;;

;;; A collector is used when a handler needs to pass information
;;; back to a function using asynchronous calls. You could use
;;; dynamically scoped variables, but that causes problems when
;;; the user quits before all handlers have been called since the
;;; result variable will be out of scope for the remaining handlers.
;;; The best-case scenario will be a crash. The worst-case scenario
;;; is when the handler clobbers another variable with the same name
;;; that has come into scope.
;;;
;;; So, so collect a number of results, do the following:
;;;
;;;    (setq <result> (make-collector))
;;;    (<loop-function>
;;;        (initiate-<någonting> '<queue> <function> <argl> <result>))
;;;    (lyskom-wait-queue '<queue>)
;;;    (<use> (collector->value <result>))
;;;
;;; where function is something like this:
;;;
;;;    (defun <funktion> (data-från-servern collector)
;;;        (set-collector->value collector
;;;                              (cons (<behandla> data-från-servern)
;;;                                    (collector->value collector))))
;;;
;;; or shorter,
;;;
;;;    (defun <funktion> (data-från-servern collector)
;;;        (collector-push (<behandla> data-från-servern)))
;;;
;;; This sidestepping protects the handler from scope changes.
;;;


(defun make-collector ()
  "Create a data type for collecting asynchronous results safely"
  (cons 'COLLECTOR nil))

(defsubst collector->value (collector)
  "Get the current value of a collector"
  (cdr collector))

(defsubst set-collector->value (collector value)
  "Set the calue of a collector"
  (setcdr collector value))

(defun set-value-of-collector (value collector)
  "For use with lyskom handlers. In other cases, use set-collector->value"
  (set-collector->value collector value))

(defun collector-push (value collector)
  "Push VALUE onto the front of COLLECTOR's value"
  (setcdr collector (cons value (cdr collector))))


(eval-and-compile (provide 'lyskom-clienttypes))

;;; clienttypes.el ends here
