;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: clienttypes.el,v 44.27 2004-07-21 11:14:38 byers Exp $
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
	      "$Id: clienttypes.el,v 44.27 2004-07-21 11:14:38 byers Exp $\n"))


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
;;;   REVIEW-FAQ  - List of texts created by kom-review-faq or similar
;;;   REVIEW-FAQ-TREE  - Like REVIEW-TREE but for FAQs
;;;   COMM-IN     - Type containing the list of comments to a text
;;;   FOOTN-IN    - Type containing the list of footnotes to a text
;;;   CONF        - Basic type of unread in a conf.
;;;   RE-EDIT-TEXT - Re-edit a failed submission. Buffer is in misc.
;;;   PRI-SESSION - Go to prioritized session.
;;;
;;;   The types REVIEW-TREE, COMM-IN and FOOTN-IN are created for new for
;;;   every text read (recursively) when appropriate.
;;;
;;;   The difference between REVIEW, REVIEW-MARK and REVIEW-FAQ is just
;;;   that there generate different prompts and different text from
;;;   kom-review-stack.
;;;


;;; read-info

(def-komtype read-info 
  ((type                :read-only t)
   (conf-stat           :read-only t)
   priority 
   (text-list           :read-only t)
   &optional 
   (comm-to             :read-only t)
   forward 
   (unfetched-texts     :read-only t :automatic nil)
   (misc                :read-only t))
  :nil-safe)


(defsubst read-info-append-text-list (read-info texts)
  (text-list->append (read-info->text-list read-info) texts))

(defsubst read-info-enter-text-last (read-info text-no)
  (read-info-append-text-list read-info (list text-no)))


;;; ================================================================
;;;                          read-list
;;;
;;; We don't use def-komtype here because the code below relies on 
;;; read-lists being a cons of READ-LIST and the data.
;;;

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
  (length (read-list->all-entries read-list)))


;;; Modifiers:

(defsubst set-read-list-empty (read-list)
  "Empty READ-LIST destructively."
  (let ((tmp (cdr read-list)))
    (setcdr read-list nil)
    (lyskom-traverse read-info tmp
      (when (eq 'CONF (read-info->type read-info))
        (lp--maybe-update-unreads (conf-stat->conf-no
                                   (read-info->conf-stat read-info)))))))

(defsubst set-read-list-del-first (read-list)
  "Delete the first entry of READ-LIST if there is one."
  (let ((el (car (cdr read-list))))
    (if (cdr read-list)
        (setcdr read-list (cdr (cdr read-list))))
    (when (and el (eq (read-info->type el) 'CONF))
      (lp--maybe-update-unreads (conf-stat->conf-no
                                 (read-info->conf-stat el))))))


(defsubst read-list-enter-first (read-info read-list)
  "Enter READ-INFO first into READ-LIST."
  (setcdr read-list (cons read-info (cdr read-list)))
  (when (eq 'CONF (read-info->type read-info))
    (lp--maybe-update-unreads (conf-stat->conf-no
                               (read-info->conf-stat read-info))))
  )


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
    (when inserted (lp--maybe-update-unreads (conf-stat->conf-no recipient)))
    inserted))


(defun read-list-delete-text (text-no rlist)
  "Destructively delete all occurances of TEXT-NO from RLIST.
RLIST is a list of read-info.
Entries of the type REVIEW, REVIEW-FAQ, REVIEW-TREE or REVIEW-MARK are
not changed except if they were empty in which case they are removed.
Returns the modified RLIST.
TEXT-NO may be nil, in which case only empty read-infos on RLIST are removed."
  (let* ((prev rlist)			;"Previous" cons-celll
	 (curr (cdr rlist)))		;Current cons-cell
    (while curr
      (when text-no
        (cond
         ((let ((type (read-info->type (car curr)))) 
					; Don't change REVIEW et c.
            (memq type lyskom-review-types-list)))
         (t				; Do change all other entries.
          (let ((tl  (read-info->text-list (car curr))))
            (when (text-list->memq tl text-no)
              (text-list->delq tl text-no)
              (lp--maybe-update-unreads
               (conf-stat->conf-no (read-info->conf-stat (car curr)))))))))
      
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
       ((and (eq type 'CONF)
	     (eq (read-info->type (car (cdr rlist))) 'CONF)
	     (eq (conf-stat->conf-no conf-stat)
                 (conf-stat->conf-no (read-info->conf-stat
                                      (car (cdr rlist))))))
	(read-info-append-text-list
	 (car (cdr rlist))
	 (text-list->texts (read-info->text-list read-info)))
        (lp--maybe-update-unreads (conf-stat->conf-no conf-stat))
	(setq continue nil)) 

       ((null (cdr rlist)) 
	(setcdr rlist (list read-info))
	(setq continue nil))
       ((>= pri (read-info->priority (car (cdr rlist))))
	(setcdr rlist (cons read-info (cdr rlist)))
	(setq continue nil))
       (t 
	(setq rlist (cdr rlist)))))

    (when (eq type 'CONF)
      (lp--maybe-update-unreads (conf-stat->conf-no conf-stat)) )))


(defun read-list-delete-read-info (conf-no rlist)
  "Destructively removes all the entries for the conf CONF-NO in RLIST.
RLIST is a list of read-info."
  (while (cdr rlist)
    (if (eq (conf-stat->conf-no (read-info->conf-stat
				      (car (cdr rlist))))
	    conf-no)
	(setcdr rlist (cdr (cdr rlist)))
      (setq rlist (cdr rlist))))
  (lp--maybe-update-unreads conf-no))
 
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


;;UNUSED: lyskom-queue->last
(defsubst lyskom-queue->last (queue)
  "Return the lastelement of QUEUE or nil if it is empty."
  (car-safe (cdr (cdr queue))))


(defun lyskom-queue-remove-matching (queue pred)
  "Remove all elements from QUEUE that satisfy PRED."
  (let ((prev nil)
        (ptr (car (cdr queue))))
    (while ptr
      (if (funcall pred (car ptr))
        (progn (if prev
                   (setcdr prev (cdr ptr))
                 (setcar (cdr queue) (cdr ptr)))
               (unless (cdr ptr)
                 (setcdr (cdr queue) prev)))
        (setq prev ptr))
      (setq ptr (cdr ptr)))))


(defsubst lyskom-queue-make-empty (queue)
  "Make the queue QUEUE empty."
  (setcdr queue (cons nil nil)))

;;UNUSED: lyskom-queue-set-data
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


;;UNUSED: lyskom-stack->length
(defun lyskom-stack->length (stack)
  "Return the number of elements on STACK."
  (length (cdr stack)))

;;UNUSED: lyskom-stack->nth
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
;;;			format-state

(def-komtype format-state
  (format-string
   start
   argl
   length
   result
   delayed-propl
   delayed-overlays
   delayed-content
   depth)
  :nil-safe)

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

(defun collector-push (value collector)
  "Push VALUE onto the front of COLLECTOR's value"
  (setcdr collector (cons value (cdr collector))))


;;; ================================================================
;;; Specification for text links (cached)
;;;
;;; We don't use def-komtype since the accessors should be able to
;;; work on user settings too.

(defun lyskom-create-lyskom-text-link (pattern replacement 
                                               highlight ignore-case)
  (cons 'LYSKOM-TEXT-LINK (vector pattern replacement highlight ignore-case)))

(defsubst lyskom-text-link->pattern (link)
  (if (symbolp (car link)) (elt (cdr link) 0) (elt link 0)))

(defsubst lyskom-text-link->replacement (link)
  (if (symbolp (car link)) (elt (cdr link) 1) (elt link 1)))

(defsubst lyskom-text-link->highlight (link)
  (if (symbolp (car link)) (elt (cdr link) 2) (elt link 2)))

(defsubst lyskom-text-link->ignore-case (link)
  (if (symbolp (car link)) (elt (cdr link) 3) (elt link 3)))





(eval-and-compile (provide 'lyskom-clienttypes))

;;; clienttypes.el ends here
