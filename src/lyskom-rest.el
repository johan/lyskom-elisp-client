;;;;;
;;;;; $Id: lyskom-rest.el,v 41.12 1996-07-09 08:28:32 byers Exp $
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
;;;; File: lyskom-rest.el
;;;;
;;;; This is the rest of what was once the entire source code of the
;;;; client. Now most of the source resides in other files. Eventually
;;;; the functions in here will be distributed out to other files.
;;;; Below follows the original header of the LysKOM elisp client:
;;;;
;;;; ================================================================
;;;;
;;;; Simple LysKOM elisp client.
;;;;
;;;; Originally written by Per Cederqvist.
;;;;
;;;; Heavily hacked by:
;;;;    Thomas Bellman
;;;;    Linus Tolke
;;;;    Inge Wallin
;;;;   and others.
;;;;
;;;; Some ideas stolen from lpmud.el written by Lars Willf|r and Thomas Bellman
;;;;
;;;; Conventions:
;;;;
;;;;    All functions, variables etc follow one of these conventions:
;;;;		lyskom-		general functions.
;;;;		cache-		cache-routines.
;;;;		initiate-	call a service. (Might look up in cache)
;;;;		kom-		user kommands.
;;;;		lyskom-parse-	parse (part of) a reply from the server.
;;;;				these are found in parse.el
;;;;		lyskom-edit-	Things related to the editing of texts.
;;;;		{lyskom,kom}-edit-options-
;;;;				things related to editing options
;;;;				(found in flags.el)
;;;;		{lyskom,kom}-prioritize-
;;;;				things related to the prioritize
;;;;				buffer. The file is prioritize.el
;;;;		{lyskom,kom}-{completing-read,completing,complete}-
;;;;				things related to
;;;;				lyskom-completing-read. Found in
;;;;				completing-read.el 
;;;;		lyskom-*-hook   hooks.
;;;;
;;;;    Three plus-signs (+++) in the code marks a place where more
;;;;    work is needed.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: lyskom-rest.el,v 41.12 1996-07-09 08:28:32 byers Exp $\n"))


;;;; ================================================================
;;;;                             New errors.


(put 'lyskom-protocol-error 'error-conditions
     '(error lyskom-error lyskom-protocol-error))
(put 'lyskom-protocol-error 'error-message
     "LysKOM protocol error:")

(put 'lyskom-internal-error 'error-conditions
     '(error lyskom-error lyskom-internal-error))
(put 'lyskom-internal-error 'error-message
     "Internal LysKOM error.")

(put 'lyskom-format-error 'error-conditions
     '(error lyskom-error lyskom-format-error))
(put 'lyskom-internal-error 'error-message
     "Internal LysKOM format error.")


;;; ================================================================
;;;             Global variables
;;;

(defvar lyskom-unread-mode-line nil)

;;; ================================================================
;;;             Error reporting from a number of commands.
;;;

;;; Author: Inge Wallin



(defun lyskom-get-error-text (errno)
  "Get a string which is the error ERRNO in plain text."
  (let ((pair (assoc errno lyskom-error-texts)))
    (if pair 
	(cdr pair)
      (lyskom-format 'error-not-found errno))))


(defun lyskom-handle-command-answer (answer)
  "Handles a void return from call to the server."
  (lyskom-report-command-answer answer)
  (lyskom-end-of-command))


(defun lyskom-report-command-answer (answer)
  "Handles a void return from call to the server."
  (if answer 
      (lyskom-insert-string 'done)
    (lyskom-insert-string 'nope)
    (lyskom-format-insert 'error-code
			  (lyskom-get-error-text lyskom-errno)
			  lyskom-errno)))


;;; ----------------------------------------------------------------
;;; Author: Aronsson

(defun lyskom-tell-string (key)
  "Retrieve the phrase indexed by the key from the kom-tell-phrases
assoc list."
  (car (cdr (assoc key kom-tell-phrases))))


(defun lyskom-tell-internat (key)
  "Same as lyskom-tell-server, but use a key to a list of phrases."
  ;; The server at Lysator produces a lot of network traffic. The
  ;; solution should to be less verbose. This is very interesting
  ;; anymore, anyway.
  (lyskom-tell-server (lyskom-tell-string key)))


;;;; ================================================================
;;;; Aronsson was here 5 MAY 1991
					; kom-dict-*
(defconst kom-dict
  (append
   '(
     (kom-dict-vfsh		"VFSH") ; sample assoc entry
     )
   kom-tell-phrases			; Really?
   lyskom-commands)			; Really?

  "These texts are used all over the client. They were collected into
one huge assoc list to facilitate internationalization and personal
profiling. Users are encouraged to change this dictionary for fun.

Related variables are kom-tell-phrases and lyskom-commands.")

(defun lyskom-dict-string (key)
  "Retrieve the string indexed by the key from the kom-dict assoc list."
  (car (cdr (assoc key kom-dict))))


;;;; ================================================================
;;;;                User-level commands and functions.


(defun lyskom-command-name (command)
  "Get the command name for the command COMMAND"
  (car (cdr (assoc command 
		   (if kom-emacs-knows-iso-8859-1
		       lyskom-commands
		     lyskom-swascii-commands)))))


(defun lyskom-ok-command (alternative administrator)
  "Returns non-nil if it is ok to do such a command right now."
  (if administrator
      (not (memq (car (cdr alternative)) lyskom-admin-removed-commands))
    (not (memq (car (cdr alternative)) lyskom-noadmin-removed-commands))))


(defun kom-extended-command ()
  "Read a LysKOM function name and call the function."
  (interactive)
  (let* ((completion-ignore-case t)
	 (alternatives (mapcar (function reverse)
			       (if kom-emacs-knows-iso-8859-1
				   lyskom-commands
				 lyskom-swascii-commands)))
	 (name (completing-read (lyskom-get-string 'extended-command)
				alternatives 
				;; lyskom-is-administrator is buffer-local and
				;; must be evalled before the call to 
				;; completing-read
				;; Yes, this is not beautiful
				(list 'lambda '(alternative)
				      (list 'lyskom-ok-command 'alternative
					    lyskom-is-administrator))
				t nil))
	 (fnc (reverse-assoc (car (all-completions name alternatives)) 
			     (if kom-emacs-knows-iso-8859-1
				 lyskom-commands
			       lyskom-swascii-commands))))
    (cond
     (fnc (call-interactively (car fnc)))
     (t (kom-next-command)))))


;;; Resume operation after a crash.

(defun kom-recover (&optional refetch)
  "Try to recover from an error.
If the optional argument REFETCH is non-nil, `lyskom-refetch' is called."
  (interactive "p")
  (lyskom-init-parse lyskom-buffer)
  (setq lyskom-call-data nil)
  (setq lyskom-pending-calls nil)
  (setq lyskom-output-queue (lyskom-queue-create))
  (setq lyskom-number-of-pending-calls 0)
  (setq lyskom-is-parsing nil)
  (if refetch (lyskom-refetch))
  (lyskom-tell-internat 'kom-tell-recover)
  (lyskom-end-of-command))


;;; ================================================================
;;;                      Run default command.


;;; Author: Tommy Persson

(defun kom-page-next-command ()
  "Scroll the bottom to the top and run the next command if it should be run"
  (interactive)
  (if (pos-visible-in-window-p (point-max))
      (progn
	(recenter 0)
	(lyskom-next-command))
    (recenter 0)
    (move-to-window-line -1)
    (lyskom-set-last-viewed)))


;;; Author: Lars Willf|r

(defun kom-next-command ()
  "Run next command or scroll one page."
  (interactive)
  (if (pos-visible-in-window-p (point-max))
      (lyskom-next-command)
    (move-to-window-line -1)
    (lyskom-set-last-viewed)
    (lyskom-scroll)))

  
(defun kom-line-next-command ()
  "Run next command or scroll one line."
  (interactive)
  (if (pos-visible-in-window-p (point-max))
      (lyskom-next-command)
    (move-to-window-line 1)
    (lyskom-set-last-viewed)
    (lyskom-scroll)))


;;; Author: Per Cederqvist

(defun lyskom-next-command ()
  "Run next command."
  (let ((lyskom-doing-default-command t))
    (cond
     ((eq lyskom-command-to-do 'next-pri-text)
      (lyskom-view-priority-text))
     ((eq lyskom-command-to-do 'next-text)
      (kom-view-next-text))
     ((eq lyskom-command-to-do 'next-conf)
      (kom-go-to-next-conf))
     ((eq lyskom-command-to-do 'next-pri-conf)
      (lyskom-go-to-pri-conf))
     ((eq lyskom-command-to-do 'when-done)
      (let ((command (lyskom-what-to-do-when-done)))
	(cond
	 ((stringp command)
	  (execute-kbd-macro command))
	 (t (call-interactively command)))))
     ((eq lyskom-command-to-do 'unknown)
      (lyskom-insert
       (lyskom-get-string 'wait-for-server)))
     (t (signal 'lyskom-internal-error '(kom-next-command))))))


;;; ================================================================
;;;                              View text.


(defun kom-initial-digit-view ()
  (interactive)
  (setq unread-command-events (cons last-command-event
                                    unread-command-events))
  (call-interactively 'kom-view nil))


(defun kom-view (text-no)
  "View text number TEXT-NO."
  (interactive "P")
  (unwind-protect
      (progn
	(let ((kom-page-before-command nil))
	  (lyskom-start-of-command 'kom-view)
	  (lyskom-tell-internat 'kom-tell-review)
	  )
	(if (setq text-no (cond ((null text-no) nil)
				((listp text-no) (car text-no))
				(t text-no)))
	    nil
	  (setq text-no (lyskom-read-number (lyskom-get-string 'review-text-q)
					    lyskom-current-text)))
	(if (or (not (listp kom-page-before-command))
		(memq 'kom-view kom-page-before-command))
	    (recenter 1))
	(lyskom-tell-internat 'kom-tell-review)
	(lyskom-format-insert 'review-text-no text-no)
	(lyskom-view-text text-no))
    (lyskom-end-of-command)))


;;;; ================================================================
;;;;                         View next text.


(defun kom-view-next-new-text ()
  "Display next new text (putting all review constructs at the end of this conf)."
  (interactive)
  (let ((len (read-list-length lyskom-reading-list))
	(finished nil))
    (while (and (not finished)
		(> len 1))
      (let ((type (read-info->type (read-list->first lyskom-reading-list))))
	(cond 
	 ((or (eq type 'REVIEW)
	      (eq type 'REVIEW-TREE)
	      (eq type 'REVIEW-MARK))
	  (read-list-rotate lyskom-reading-list))
	 ((or (eq type 'COMM-IN)
	      (eq type 'FOOTN-IN))
	  (set-read-list-del-first lyskom-reading-list))
	 (t 
	  (setq finished t))))
      (-- len)))
  (kom-view-next-text))


;;; Modified to handle filters

(def-kom-command kom-view-next-text ()
  "Display next text (from lyskom-reading-list)."
  (interactive)
  (lyskom-tell-internat 'kom-tell-read)
  (let ((action 'next-text))
    (while (eq action 'next-text)
      (if (read-list-isempty lyskom-reading-list)
	  (progn
	    (if (/= 0 lyskom-current-conf)
		(lyskom-insert-string 'completely-read-conf)
	      (lyskom-insert-string 'not-in-any-conf))
	    (setq action nil))
	      
	(progn
	  (let* ((tri (read-list->first lyskom-reading-list))
		 (text-no (car (cdr (read-info->text-list tri))))
		 (type (read-info->type tri))
		 (priority (read-info->priority
			    (read-list->first lyskom-reading-list)))
		 (is-review-tree (eq type 'REVIEW-TREE))
		 (is-review (or (eq type 'REVIEW)
				(eq type 'REVIEW-MARK)
				is-review-tree))
		 (mark-as-read (not is-review)))
	    (if is-review
		(delq text-no 
		      (read-info->text-list tri))) ;First entry only
	    (setq action
		  (lyskom-view-text text-no mark-as-read 
				    (and kom-read-depth-first
					 (not is-review))
				    (read-info->conf-stat
				     (read-list->first
				      lyskom-reading-list))
				    priority
				    is-review-tree
				    (not is-review)))
	    (if mark-as-read
		(lyskom-is-read text-no)
	      (read-list-delete-text nil lyskom-reading-list)
	      (read-list-delete-text nil lyskom-to-do-list))))))))
  

;;; Modified to handle filters

;; This is horribly ugly. It acts like a user command, but it isn't.
(defun lyskom-view-priority-text ()
  "Display the first text from the next conference on the lyskom-to-do-list."
  (lyskom-start-of-command 'kom-view-next-text)
  (unwind-protect
      (progn
	(lyskom-tell-internat 'kom-tell-read)
	(let* ((tri (read-list->first lyskom-to-do-list))
	       (priority (read-info->priority
			  (read-list->first lyskom-reading-list)))
	       (text-no (car (text-list->texts (read-info->text-list tri)))))
	  (lyskom-is-read text-no)
	  (lyskom-view-text text-no t nil (read-info->conf-stat tri) 
			    priority nil t))
	(lyskom-wait-queue 'main))
    (lyskom-end-of-command)))


(defun lyskom-is-read (text-no)
  "Remove TEXT-NO from the list of texts to read.
Deletes TEXT-NO from lyskom-reading-list and lyskom-to-do-list.
Adds info in lyskom-membership.

This function only modifies the internal state of the client. It does
not mark the text as read in the server. That function is performed by
lyskom-mark-as-read."
  (read-list-delete-text text-no lyskom-reading-list)
  (read-list-delete-text text-no lyskom-to-do-list)
  (initiate-get-text-stat 'main 'lyskom-is-read-handler text-no)
  (setq lyskom-normally-read-texts (cons text-no lyskom-normally-read-texts)))


(defun lyskom-is-read-handler (text-stat)
  "Update lyskom-membership for all recipients to TEXT-STAT."
  (lyskom-traverse
      misc (text-stat->misc-info-list text-stat)
    (if (or (eq 'RECPT (misc-info->type misc))
	    (eq 'CC-RECPT (misc-info->type misc)))
	(let ((membership (lyskom-member-p (misc-info->recipient-no misc))))
	  (if membership
	      (set-membership->read-texts
	       membership
	       (vconcat (vector (misc-info->local-no misc))
			(membership->read-texts membership))))))))


;;; ================================================================
;;;                        Go to next conf.


(def-kom-command kom-go-to-next-conf ()
  "Go to next conf.
Take first conf from lyskom-to-do-list and copy it to lyskom-reading-list.
Tell server what the user is doing. If the user is reading a conf it is
moved last on lyskom-to-do-list, with priority 0."
  (interactive)
  (lyskom-maybe-move-unread t)
  (lyskom-go-to-next-conf))


(defun lyskom-go-to-pri-conf ()
  "Go to the conf with higher priority.
This differs from kom-go-to-next-conf only in the place where the yet unread 
in the current conf is placed."
  (lyskom-start-of-command 'kom-go-to-next-conf)
  (lyskom-maybe-move-unread nil)
  (lyskom-go-to-next-conf)
  (lyskom-end-of-command))


(defun lyskom-go-to-next-conf ()
  "Actually go to the next conference on the lyskom-to-do-list"
  ;; Copy first element on lyskom-to-do-list to lyskom-reading-list.
  (if (not (read-list-isempty lyskom-to-do-list))
      (progn
	(read-list-enter-first
	   (read-list->first lyskom-to-do-list)
	   lyskom-reading-list)

	;; Tell server which conf the user is reading.
	(let ((conf-stat (read-info->conf-stat
			  (read-list->first lyskom-reading-list))))
	  (when conf-stat
	    (lyskom-enter-conf
	     conf-stat 
	     (read-list->first lyskom-reading-list))
	    (lyskom-set-mode-line conf-stat))))
    (lyskom-insert-string 'all-conf-unread-r)
    (lyskom-set-mode-line (lyskom-get-string 'all-conf-unread-s))))


(defun lyskom-maybe-move-unread (bury)
  "Empty the reading list.
If the argument BURY is non-nil and there are unread artilces left in the
reading list then the conf is inserted last in the to do list."
  (if (not (read-list-isempty lyskom-reading-list))
      (progn
	(if bury
	    (let ((conf-no nil)
		  (r 0))
	      (while (and (not conf-no)
			  (< r (read-list-length lyskom-reading-list)))
		(if (eq (read-info->type (read-list->nth lyskom-reading-list 
							 r))
			'CONF)
		    (setq conf-no (conf-stat->conf-no 
				   (read-info->conf-stat
				    (read-list->nth lyskom-reading-list r))))
		  (++ r)))
	      (if conf-no
		  (progn
		    (read-list-delete-read-info conf-no lyskom-to-do-list)
		    (read-list-enter-first (read-list->nth lyskom-reading-list
							   r)
					   lyskom-to-do-list)
		    (read-list-rotate lyskom-to-do-list)))))
	(set-read-list-empty lyskom-reading-list))))


(defun lyskom-enter-conf (conf-stat read-info)
  "Tell server which conf the user is reading.
Prints the name and amount of unread in the conference we just went to 
according to the value of kom-print-number-of-unread-on-entrance.
Args: CONF-STAT READ-INFO"
  (initiate-pepsi 'main nil (conf-stat->conf-no conf-stat))
  (setq lyskom-current-conf (conf-stat->conf-no conf-stat))
  (let ((num-unread (length (text-list->texts
			     (read-info->text-list
			      read-info)))))
    (lyskom-format-insert (if (not kom-print-number-of-unread-on-entrance)
			      'enter-conf
			    (if (= num-unread 1)
				'one-unread
			      'several-unread))
			  conf-stat
			  num-unread)))
  

(defun lyskom-put-back-on-to-do-list (read-list low-priority)
  "Add the conferences on READ-LIST on lyskom-to-do-list.
Alter the priority to kom-low-priority if LOW-PRIORITY is non-nil."
  (lyskom-traverse
      to-read (read-list->all-entries read-list)
    (if (or (eq 'CONF (read-info->type to-read))
	    (eq 'REVIEW-MARK (read-info->type to-read)))
	;; Only unread conferences and viewings of marked texts
	;; are put back on the to-do-list.
	;; E. g. unread comments to a text are not put back since they
	;; are anyhow present on a 'CONF-item's text-list.
	(progn
	  (if (and kom-low-priority
		   low-priority)
	      (set-read-info->priority to-read kom-low-priority))
	  (read-list-enter-read-info to-read
				     lyskom-to-do-list
				     (not low-priority))))))


;;;================================================================

;;; in vars.el:
;(defvar lyskom-sessions-with-unread nil
;  "Global variable. List of lyskom-sessions with unread articles.")
;;;Must be called after lyskom-get-string is defined. Also after running 
;;;load hooks.
;;;(or (assq 'lyskom-sessions-with-unread minor-mode-alist)
;;;    (setq minor-mode-alist (cons (list 'lyskom-sessions-with-unread 
;;;			       		  (lyskom-get-string 'mode-line-unread))
;;;				 minor-mode-alist)))


(defun lyskom-set-mode-line (&optional conf)
  "Sets mode-line-conf-name to the name of the optional argument conf CONF.
CONF can be a a conf-stat or a string."
  (let ((name (cond
	       ((null conf) 
		(if (zerop lyskom-current-conf)
		    (lyskom-get-string 'not-present-anywhere)
		  (initiate-get-conf-stat 'modeline 'lyskom-set-mode-line
					  lyskom-current-conf)
		  nil))
	       ((stringp conf) conf)
	       ((lyskom-conf-stat-p conf)
		(conf-stat->name conf))
	       (t "")))
	(unread -1)
	(total-unread 0)
	(letters 0)
	(len 0)
	(read-info-list nil))

    (if (null name)
	nil				; We didn't have the name.

      ;; Set unread to the number of unread texts in CONF.
      (if (lyskom-conf-stat-p conf)
	  (progn
	    (setq read-info-list 
		  (read-list->all-entries lyskom-to-do-list))

	    ;; This was weird. Someone had begun to write an if, but
	    ;; this was all there was: (if 
	    (while read-info-list
	      (if (read-info->conf-stat (car read-info-list))
		  (progn
		    (setq len (length (text-list->texts 
				       (read-info->text-list 
					(car read-info-list)))))
		    (if (= (conf-stat->conf-no conf)
			   (conf-stat->conf-no 
			    (read-info->conf-stat (car read-info-list))))
			(setq unread len))
		    (if (= lyskom-pers-no
			   (conf-stat->conf-no 
			    (read-info->conf-stat (car read-info-list))))
			(setq letters len))
		    (setq total-unread (+ total-unread len))))
	      (setq read-info-list (cdr read-info-list))))))
      (if (= unread -1)
	  (setq unread 0))

      (if (null name)
	  nil
	(setq mode-line-conf-name 
	      (substring (concat
			  (if (lyskom-conf-stat-p conf)
			      (if (> lyskom-session-priority 0)
				  (format "(%d/%d:%d) "
					  unread total-unread
					  lyskom-session-priority)
				(format "(%d/%d) " unread total-unread))
			    "")
			  name
			  (make-string 27 ? ))
			 0 27))
	(if (not kom-emacs-knows-iso-8859-1)
	    (setq mode-line-conf-name
		  (iso-8859-1-to-swascii mode-line-conf-name))))

      (if (zerop total-unread)
	  (setq lyskom-sessions-with-unread
		(delq lyskom-proc lyskom-sessions-with-unread))
	(or (assq lyskom-proc lyskom-sessions-with-unread)
	    (setq lyskom-sessions-with-unread
		  (cons lyskom-proc lyskom-sessions-with-unread))))
      (if (zerop letters)
	  (setq lyskom-sessions-with-unread-letters
		(delq lyskom-proc lyskom-sessions-with-unread-letters))
	(or (assq lyskom-proc lyskom-sessions-with-unread-letters)
	    (setq lyskom-sessions-with-unread-letters
		  (cons lyskom-proc lyskom-sessions-with-unread-letters)))))
  (force-mode-line-update))


;;; ================================================================

;;; +++Where should this be moved???


(defun lyskom-member-p (conf-no)
  "Returns non-nil if conference CONF-NO is present on lyskom-membership.
The value is actually the membership for the conference."
  (let ((list lyskom-membership)
	(found nil))
    (while (and (not found) (not (null list)))
      (if (= conf-no (membership->conf-no (car list)))
	  (setq found (car list)))
      (setq list (cdr list)))
    found))


;;; ================================================================
;;; +++ Where shall this function move?

(defun lyskom-return-time (time)
  "Return a string containing the time TIME."
  (lyskom-format 'time-yyyy-mm-dd-hh-mm
	  (+ (time->year time) 1900)
	  (1+ (time->mon  time))
	  (time->mday time)
	  (time->hour time)
	  (time->min  time)))


;;;; ================================================================
;;;;                   Scrolling and text insertion.


(defun lyskom-trim-buffer ()
  "Trim the size of a lyskom buffer to lyskom-max-buffer-size"
  (save-excursion
    (if (and kom-max-buffer-size
	     (> (buffer-size) kom-max-buffer-size))
	(let ((delchars (- (buffer-size) kom-max-buffer-size)))
	  (goto-char (point-min))
	  (while (< (point) delchars)
	    (forward-line 1))
	  (delete-region (point-min) (point))))))

(defun lyskom-scroll ()
  "Scroll screen if necessary.
The position lyskom-last-viewed will always remain visible."
  ;; Find a window to scroll. Scroll the selected window if that shows LysKOM.
  ;; Otherwise scroll any window currently showing LysKOM.
  (let ((win
	 (cond
	  ((eq (window-buffer (selected-window)) (current-buffer))
	   (selected-window))
	  (t				;(get-buffer-window (current-buffer))
	   nil))))
    (if (and win			;Do nothing if no window showed LysKOM.
	     (not (pos-visible-in-window-p (point-max))))
	(progn
	  (goto-char (point-max))
	  (recenter -1)
	  (if (not (pos-visible-in-window-p lyskom-last-viewed))
	      (progn
		(set-window-start win lyskom-last-viewed)
		(move-to-window-line -1)
		(vertical-motion 1)
		(if (not (pos-visible-in-window-p))
		    (forward-char -1))))))))
	

(defun lyskom-insert (string)
  "Insert STRING last in current buffer.
Leaves the point at the end of the buffer if possible without
scrolling past lyskom-last-viewed (generally the most recent prompt.)
Leaves the point at the end of the window if not possible. If buffer
is not on screen then doesn't move point.
The text is converted according to the value of kom-emacs-knows-iso-8859-1."
  (let ((was-at-max (= (save-excursion (end-of-line) (point)) (point-max))))
    (save-excursion
      (goto-char (point-max))
      (let ((buffer-read-only nil))
	(insert (if kom-emacs-knows-iso-8859-1
		    string
		  (iso-8859-1-to-swascii string))))
      (lyskom-trim-buffer))
    (let ((window (get-buffer-window (current-buffer))))
      (if (and window was-at-max)
	  (if (pos-visible-in-window-p (point-max) window)
	      (goto-char (point-max))
	    (and kom-continuous-scrolling (lyskom-scroll)))))))


(defun lyskom-insert-at-point (string)
  "Insert STRING in the current buffer at point.
The text is converted according to the value of
kom-emacs-knows-iso-8859-1."
  (let ((buffer-read-only nil))
    (insert (if kom-emacs-knows-iso-8859-1
                string
              (iso-8859-1-to-swascii string))))
  (lyskom-trim-buffer))  


(defun lyskom-insert-before-prompt (string)
  "Insert STRING just before the prompt of if no prompt then just buffers.
If prompt on screen then do the scroll if necessary.
The strings buffered are printed before the prompt by lyskom-print-prompt."
  (lyskom-do-insert-before-prompt string))

(defun lyskom-do-insert-before-prompt (string)
  (cond
   ((and lyskom-executing-command
         (not (eq lyskom-is-waiting t)))
    ;; Don't insert the string until the current command is finished.
    (if (null lyskom-to-be-printed-before-prompt)
        (setq lyskom-to-be-printed-before-prompt (lyskom-queue-create)))
    (lyskom-queue-enter lyskom-to-be-printed-before-prompt 
                        (list string)))
   (t (save-excursion
    (let ((oldpoint (if (/= (point) (point-max)) (point) nil)))
      (goto-char (point-max))
      (let* ((window (get-buffer-window (current-buffer)))
             (pv (and window
                      (pos-visible-in-window-p (point) window))))
        (beginning-of-line)
        (let ((buffer-read-only nil)
              (start (point)))
          (insert (if kom-emacs-knows-iso-8859-1
                      string
                    (iso-8859-1-to-swascii string))))
        (goto-char (point-max))
        (if (and pv
                 (eq window (selected-window))
                 (not (pos-visible-in-window-p (point) window)))
              (recenter -1))

        (if window
            (if (pos-visible-in-window-p (point) window)
                nil
              (goto-char (window-start window))
              (end-of-line (1- (window-height window)))))))))))


(defun lyskom-message (format-string &rest args)
  "Like message, but converts iso-8859/1 texts to swascii if necessary.
Args: FORMAT-STRING &rest ARGS"
  (let ((str (apply 'format format-string args)))
    (if kom-emacs-knows-iso-8859-1
	(message "%s" str)
      (message "%s" (iso-8859-1-to-swascii str)))))


(defun lyskom-error (format-string &rest args)
  "Like error, but converts iso-8859/1 texts to swascii if necessary.
Args: FORMAT-STRING &rest ARGS"
  (let ((str (apply 'format format-string args)))
    (if kom-emacs-knows-iso-8859-1
	(error "%s" str)
      (error "%s" (iso-8859-1-to-swascii str)))))


(defun lyskom-set-last-viewed ()
  (save-excursion
    (vertical-motion 0)
    (setq lyskom-last-viewed (point))))


;;; ================================================================
;;;                  Extended string formatting

;;; Author: David Byers
;;; Original code: Inge Wallin


(defvar lyskom-format-format
  "%\\(=\\)?\\(-?[0-9]+\\)?\\(#\\([0-9]+\\)\\)?\\(:\\)?\\([][@MmPpnrtsdoxc]\\)"
  "regexp matching format string parts.")

(defun lyskom-insert-string (atom)
 "Find the string corresponding to ATOM and insert it into the LysKOM buffer." 
  (lyskom-insert (lyskom-get-string atom)))

(defun lyskom-get-string (atom)
  "Get the string corresponding to ATOM and return it."
  (if (stringp atom)
      atom
    (let ((format-pair (assoc atom lyskom-strings)))
      (if (null format-pair)
	  (signal 'lyskom-internal-error 
		  (list 'lyskom-get-string
			(list atom ": string not found")))
	(cdr format-pair)))))


(defun lyskom-format (format-string &rest argl)
  (format-state->result (lyskom-do-format format-string argl)))

(defun lyskom-format-insert (format-string &rest argl)
  "Format and insert a string according to FORMAT-STRING.
The string is inserted at the end of the buffer with `lyskom-insert'."
  (let* ((state (lyskom-do-format format-string argl t))
	 (start (point-max))
	 (deferred (format-state->delayed-content state)))
    (lyskom-insert (format-state->result state))
    (while deferred
      (let ((defer-info (car deferred))
	    (m (make-marker)))
	(set-marker m (+ start (defer-info->pos defer-info)))
	(set-defer-info->pos defer-info m)			     
	(lyskom-defer-insertion defer-info)
	(setq deferred (cdr deferred))))))

(defun lyskom-format-insert-at-point (format-string &rest argl)
  "Format and insert a string according to FORMAT-STRING.
The string is inserted at point."
  (let* ((state (lyskom-do-format format-string argl t))
	 (start (point))
	 (deferred (format-state->delayed-content state)))
    (lyskom-insert-at-point (format-state->result state))
    (while deferred
      (let ((defer-info (car deferred))
	    (m (make-marker)))
	(set-marker m (+ start (defer-info->pos defer-info)))
	(set-defer-info->pos defer-info m)			     
	(lyskom-defer-insertion defer-info)
	(setq deferred (cdr deferred))))))

(defun lyskom-format-insert-before-prompt (format-string &rest argl)
  (lyskom-insert-before-prompt (format-state->result (lyskom-do-format format-string argl))))


(defun lyskom-do-format (format-string &optional argl allow-defer)
  "Do the actual formatting and return the resulting format-state."
  (let ((fmt (cond ((stringp format-string) format-string)
		   ((symbolp format-string) (lyskom-get-string 
					     format-string))))
	(state nil))
    (if (null fmt)
        (signal 'lyskom-internal-error (list 'lyskom-format-insert
                                             ": bad format string"))
      (save-excursion
        (if (and (boundp 'lyskom-buffer)
		 lyskom-buffer)
            (set-buffer lyskom-buffer))
	(condition-case error
	    (setq state (lyskom-format-aux (make-format-state
					    fmt
					    0
					    argl
					    "")
					   allow-defer))
	  (lyskom-format-error
	   (error "LysKOM internal error formatting %s: %s%s"
		  format-string (nth 1 error) (nth 2 error))))))
    state))





(defun lyskom-format-aux (format-state allow-defer)
  (let ((format-length (length (format-state->format-string format-state)))
        (arg-no nil)
        (pad-length nil)
        (format-letter nil)
        (colon-flag nil)
        (equals-flag nil)
        (abort-format nil))

    ;;
    ;;  Eat the format string bit by bit
    ;;

    (while (and (not abort-format)
                (< (format-state->start format-state)
                   format-length))

      ;;
      ;;    Look for a format letter. If there is none, finish up,
      ;;    otherwise handle each letter separately
      ;;

      (if (null (string-match lyskom-format-format
                              (format-state->format-string format-state)
                              (format-state->start format-state)))
          (progn
            (set-format-state->result 
             format-state
             (concat (format-state->result format-state)
                     (substring (format-state->format-string format-state)
                                (format-state->start format-state))))
            (set-format-state->start format-state
                                     (length (format-state->format-string
                                              format-state))))

        ;;
        ;; A format letter has been found
        ;;

        (set-format-state->result 
         format-state
         (concat (format-state->result format-state)
                 (substring (format-state->format-string format-state)
                            (format-state->start format-state)
                            (match-beginning 0))))

        (set-format-state->start format-state
                                 (match-end 0))

	(setq equals-flag (match-beginning 1)
	      pad-length (if (match-beginning 2)
			     (string-to-int (substring 
					     (format-state->format-string
					      format-state)
					     (match-beginning 2)
					     (match-end 2)))
			   nil)
	      arg-no (if (match-beginning 4)
			 (string-to-int (substring (format-state->format-string
						    format-state)
						   (match-beginning 4)
						   (match-end 4)))
		       nil)
	      colon-flag (match-beginning 5)
	      format-letter (if (match-beginning 6)
				(aref (format-state->format-string 
				       format-state)
				      (match-beginning 6))
			      (signal 'lyskom-internal-error
				      (list 'lyskom-format-aux 
					    (format-state->format-string
					     format-state)))))

    ;;
    ;;  If the format letter is an end-of-group letter, abort
    ;;  formatting and return to the caller.
    ;;

	(if (= ?\] format-letter)
	    (progn 
	      (setq abort-format t)
	      (set-format-state->start format-state (match-end 0)))

	  (setq format-state 
            (lyskom-format-aux-help 
             format-state
             pad-length
             arg-no
             format-letter
             equals-flag
             colon-flag
             (if (and (match-beginning 2)
                      (eq (aref (format-state->format-string format-state)
                                (match-beginning 2))
                          ?0))
                 ?0
               ? )
	     allow-defer))))))
  (lyskom-tweak-format-state format-state))


(defun lyskom-format-aux-help (format-state
                               pad-length
                               arg-no
                               format-letter
                               equals-flag
                               colon-flag
                               pad-letter
			       allow-defer)
  (let ((arg nil)
        (result nil)
        (propl nil)
        (prop-adjust-start 0)
        (prop-adjust-end 0)
        (oldpos (length (format-state->result format-state)))
        (abs-length (cond ((null pad-length) nil)
                          ((< pad-length 0) (- 0 pad-length))
                          (t pad-length))))
    (if (and arg-no 
             (< (format-state->args-length format-state) arg-no))
        (signal 'lyskom-format-error (list 'lyskom-format
                                             ": too few arguments")))
    (if arg-no
        (setq arg (nth (1- arg-no) (format-state->args format-state))))
    
    (if (format-props-p arg)        
        (setq propl (format-props->propl arg)
              arg (format-props->arg arg)))
    
    
    (cond
     ;;
     ;;  Format a string or symbol by simply inserting it into the
     ;;  result list
     ;;
     ((= format-letter ?s)
      (setq result (cond ((stringp arg) arg)
                         ((symbolp arg) (symbol-name arg))
                         (t (signal 'lyskom-format-error
                                    (list 'lyskom-format
                                          ": argument error"))))))
     ;;
     ;;  Format a number by conferting it to a string and inserting
     ;;  it into the result list
     ;;
     ((or (= format-letter ?d)
          (= format-letter ?o)
          (= format-letter ?x))
      (setq result (if (integerp arg)
                       (format (format "%%%c" format-letter)
                               arg)
                     (signal 'lyskom-internal-error
                             (list 'lyskom-format
                                   ": argument error")))))
     ;;
     ;;  Format a character by converting it to a string and inserting
     ;;  it into the result list
     ;;
     ((= format-letter ?c)
      (setq result (if (integerp arg)
                       (char-to-string arg)
                     (signal 'lyskom-internal-error
                             (list 'lyskom-format
                                   ": argument error")))))
     ;;
     ;;  Format a literal percent character by inserting a string
     ;;  containing it into the result list
     ;;
     ((= format-letter ?%)
      (setq result "%")) 
     
     ;;
     ;;  Format a text property array indicator by retreiving the
     ;;  properties from the argument list and adding a start of 
     ;;  new properties to the format state
     ;;
     ((= format-letter ?@)
      (set-format-state->delayed-propl
       format-state
       (cons (cons (length (format-state->result format-state))
                   arg)
             (format-state->delayed-propl format-state))))
     ;;
     ;;  Format a subformat list by recursively formatting the contents
     ;;  of the list, augmenting the result and format state
     ;;
     ;;  Idea: If this code used lyskom-do-format instead, we could
     ;;  use it to truncate a complex format by using a format string
     ;;  such as:  "%17[ %#1s will be truncated %]"
     ;;
     ;;  This could be useful for faster response when deferring
     ;;  printing. But this function would become more complex and
     ;;  slower.
     ;;
     ((= format-letter ?\[)
      (setq format-state (lyskom-format-aux format-state allow-defer)
            result nil))
     ;;
     ;;  Format a conference or person name by retreiving information
     ;;  about the conference or person and inserting it as a button
     ;;  (unless the colon flag is set)
     ;;
     ((or (= format-letter ?M)
          (= format-letter ?P))
      (setq result
            (cond
             ;; The string is already supplied
             ((stringp arg) arg)

             ;; Conference 0 does not exist, and person 0 means anonymous
             ((and (integerp arg)
                   (zerop arg))
	      (setq colon-flag t)
              (lyskom-format (cond (lyskom-default-conf-string
				    lyskom-default-conf-string)
				   ((= format-letter ?P) 'person-is-anonymous)
				   (t 'conference-does-not-exist))
			     arg)
	      )

             ;; Delay the printing
             ((and allow-defer
                   kom-deferred-printing
                   (integerp arg))
              (let ((tmp (cache-get-conf-stat arg)))
                (if (null tmp)
                    (let* ((format-element
			    (concat "%#2@%"
				    (if equals-flag "=" "")
				    (if pad-length (int-to-string pad-length))
				    "#1"
				    (if colon-flag ":" "")
				    (char-to-string format-letter)))
			   (defer-info (lyskom-create-defer-info
					'get-conf-stat
					arg
					'lyskom-deferred-insert-conf
					oldpos
					(if pad-length
					    (if equals-flag
						(abs pad-length)
					      (max (length
						    lyskom-defer-indicator)
						   (abs pad-length)))
					  (length lyskom-defer-indicator))
					format-element
					lyskom-default-conf-string)))
		      (set-format-state->delayed-content
                       format-state
                       (cons defer-info
                             (format-state->delayed-content
                              format-state)))
                      lyskom-defer-indicator)
                  (setq arg tmp)
                  (conf-stat->name arg))))
	     
             ;; Find the name and return it
             ((integerp arg)
              (let ((conf-stat (blocking-do 'get-conf-stat arg)))
                (if (null conf-stat)
                    (lyskom-format (if (= format-letter ?P)
                                       'person-does-not-exist
                                     'conference-does-not-exist)
                                   arg)
                  (conf-stat->name conf-stat))))
	     
             ;; We got a conf-stat, and can use it directly
             ((lyskom-conf-stat-p arg) (conf-stat->name arg))

             ;; Something went wrong
             (t (signal 'lyskom-internal-error
                        (list 'lyskom-format
                              ": argument error")))))
      (if (and (not colon-flag)
               (or (lyskom-conf-stat-p arg)
                   (numberp arg)))
          (setq propl 
                (append
                 (lyskom-default-button (if (= format-letter ?P) 'pers 'conf)
                                        arg)
                 propl))))
     ;;
     ;;  Format a conference or person number the same way as names,
     ;;  but insert the number rather than the name
     ;;
     ((or (= format-letter ?m)
          (= format-letter ?p))
      (setq result
            (cond ((integerp arg) 
		   (int-to-string arg))
                  ((lyskom-conf-stat-p arg) 
                   (int-to-string (conf-stat->conf-no arg)))
                  (t (signal 'lyskom-internal-error
                             (list 'lyskom-format
                                   ": argument error")))))
      (if (not colon-flag)
          (setq propl 
                (append 
                 (lyskom-default-button (if (= format-letter ?p) 'pers 'conf)
                                        arg)
                 propl))))
     ;;
     ;;  Format an integer or text-stat as a text number by adding the
     ;;  by inserting a button (unless the colon flag is set)
     ;;
     ((= format-letter ?n)
      (setq result
            (cond ((integerp arg) (int-to-string arg))
                  ((lyskom-text-stat-p arg) (int-to-string
                                             (text-stat->text-no arg)))
                  (t (signal 'lyskom-internal-error
                             (list 'lyskom-format
                                   ": argument error")))))
      (if (not colon-flag)
          (setq propl
                (append (lyskom-default-button 'text arg) propl))))
     ;;
     ;;  Format a subject line by adding the subject face to the text
     ;;  properties and the subject to the result list
     ;;
     ((= format-letter ?r)
      (setq result (cond ((stringp arg) (lyskom-button-transform-text arg))
                         (t (signal 'lyskom-internal-error
                                    (list 'lyskom-format
                                          ": argument error")))))
      (if (and (not colon-flag)
               (not (lyskom-face-default-p 'kom-subject-face)))
          (setq propl (append (list 'face 'kom-subject-face) propl))))
     ;;
     ;;  Format a LysKOM text body. Currently this does nothing. It
     ;;  should parse the text for buttons
     ;;
     ((= format-letter ?t)

      ;; +++ One would want to do this before or after, but then
      ;; buttons will not be visible and other highlighting will
      ;; disappear.

      ;; (if (not colon-flag)
      ;;     (setq propl (append (list 'face 'kom-text-face) propl)))

      (setq result
            (cond ((stringp arg) (lyskom-format-text-body arg))
                  ((lyskom-text-p arg) 
                   (lyskom-format-text-body (text->text-mass arg)))
                  (t (signal 'lyskom-internal-error
                             (list 'lyskom-format
                                   ": argument error"))))))


     ;;
     ;; Insert some deferred text
     ;;
     ((= format-letter ?D)
      (setq result
	    (cond
	     ((stringp arg) arg)
	     (t
	      (let ((format-element
		     (concat "%"
			     (if equals-flag "=" "")
			     (if pad-length (int-to-string pad-length))
			     "#1"
			     (if colon-flag ":" "")
			     "s")))
		(set-defer-info->pos arg oldpos)
		(set-defer-info->del-chars
		 arg (if pad-length
			 (if equals-flag
			     (abs pad-length)
			   (max (length
				 lyskom-defer-indicator)
				(abs pad-length)))
		       (length lyskom-defer-indicator)))
		(set-defer-info->format arg format-element))
	      (set-format-state->delayed-content
	       format-state
	       (cons arg
		     (format-state->delayed-content
		      format-state)))
	      lyskom-defer-indicator))))

     ;;
     ;; The format letter was unknown
     ;;
     
     (t (signal 'lyskom-internal-error
		(list 'lyskom-format-help format-letter))))
    
    ;;
    ;; Pad the result to the appropriate length
    ;; Fix flags so text props go in the right places anyway
    ;;
    
    (cond ((or (null pad-length)
               (null result)) nil)
          ((> abs-length (length result))
           (let ((padstring (make-string (- abs-length (length result))
                                         pad-letter)))
             (if (< pad-length 0)       ; LEFT justify
                 (progn
                   (setq prop-adjust-end (- (- abs-length (length result))))
                   (setq result (concat result padstring)))
               (progn
                 (setq prop-adjust-start (- abs-length (length result)))
                 (setq result (concat padstring result))))))
          ((and equals-flag
                (< abs-length (length result)))
           (setq result (substring result 0 abs-length))))
    
    (if result
        (progn
          (set-format-state->result 
           format-state
           (concat (format-state->result format-state)
                   result))))
    (if (and propl kom-text-properties)
        (add-text-properties
         (+ oldpos prop-adjust-start)
         (+ (length (format-state->result format-state)) prop-adjust-end)
         propl
         (format-state->result format-state))))
  format-state)




(defun lyskom-tweak-format-state (format-state) 
  (let ((dp (format-state->delayed-propl format-state)))
    (while dp
      (add-text-properties (car (car dp))
                           (length (format-state->result format-state))
                           (cdr (car dp))
                           (format-state->result format-state))
      (setq dp (cdr dp)))
    (set-format-state->delayed-propl format-state nil))
  format-state)


;;; ================================================================
;;;			 Text body formatting

;;; Author: David K}gedal
;;; This should be considered an experiment

(defvar lyskom-format-experimental nil)

(defun lyskom-format-text-body (text)
  "Format a text for insertion. Does parsing of special markers in the text."
  ;; This function is probably not written as it should
  (if lyskom-format-experimental
      (cond
       ((and (string-match "\\`html:" text)
             (condition-case e (require 'w3) (error nil)))
        (let ((tmpbuf (generate-new-buffer "lyskom-html")))
          (unwind-protect
              (save-excursion
                (set-buffer tmpbuf)
                (insert (substring text 5))
                (w3-preview-this-buffer))
            (kill-buffer tmpbuf))))
       ((and (fboundp 'format-decode-buffer)
             (string-match "\\`enriched:" text))
        (let ((tmpbuf (generate-new-buffer "lyskom-enriched")))
          (unwind-protect
              (save-excursion
                (set-buffer tmpbuf)
                (insert (substring text 10))
                (format-decode-buffer)
                (lyskom-button-transform-text (buffer-string))
                ;; (substring (buffer-string) 0 -1) ; Remove the \n
                )
            (kill-buffer tmpbuf))))
       (t (lyskom-button-transform-text text)
	))
    (if kom-text-properties
	(lyskom-button-transform-text text)
      text)))
    


;;; ============================================================
;;;                     Beeping and feeping
;;;                     Faces and colors
;;;

(defun lyskom-beep (arg)
  "Beep. ARG is how to beep. 
nil means don't beep.
t means beep once.
A number means beep that number of times (.1 second delay between beeps).
A string means start the command kom-audio-player with the string as argument.
A symbol other than t means call it as a function."
  (cond ((null arg))
        ((eq t arg) (ding t))
        ((numberp arg) (while (> arg 0)
                         (ding t)
                         (sit-for 0.1)
                         (setq arg (1- arg))))
        ((stringp arg)
         (start-process "audio"
                        nil
                        kom-audio-player
                        arg))
        ((and (symbolp arg)
              (fboundp arg))
         (condition-case err
             (funcall arg)
           (error (message "Error in beep function")
                  (beep))))
        (t (beep))))

(defun lyskom-face-default-p (f1)
  "Return t if f1 is undefined or the default face."
  (or (not (facep f1))
      (face-equal f1 'default)))
		  

;;; ================================================================
;;;                      Iso-8859-1 converting

;;; Author: Linus Tolke Y


(defvar iso-8859-1-table 
  " 	

 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ 	

  !c#$Y|$\"c+?!-R~C+23'u$-,10?????AAAA[][CE@EEIIIIDNOOOO\\*\\UUU^YTBaaaa{}{ce`eeiiiidnoooo|/|uuu~yty"
  "*This is a table of the chars corresponding value in SWASCII.
Used by the function iso-8859-1-to-swascii function.")


(defun iso-8859-1-to-swascii (string)
  "Returns a string without characters with code > 127.
What chars are converted to is controlled by the iso-8859-1-table."
  (let ((tmp (copy-sequence string))
        (i 0)
        (len (length string)))
    (while (< i len)
      (aset tmp i (aref iso-8859-1-table
                        (aref tmp i)))
      (setq i (1+ i)))
    tmp))


;;;; ================================================================
;;;;                         Running in buffer 

;;; Author: Linus


(defun backward-text (&optional arg)
  "Searches backwards for a text start and recenters with that text at the top."
  (interactive "p")
  (let ((paragraph-start lyskom-text-start))
    (backward-paragraph arg))
  (beginning-of-line))


(defun forward-text (&optional arg)
  "Searches forward for a text start and recenters with that text at the top."
  (interactive "p")
  (let ((paragraph-start lyskom-text-start))
    (forward-paragraph arg)))


(defun kom-save-text (&optional arg)
  "Saves/appends the article before point to a file.
The article is determined by a search-backward the same as backward-text 
and then a forward-text.
With an argument ARG the search is done over that number of texts.
The name of the file is read using the minibuffer and the default is kom-text."
  (interactive "p")
  (save-excursion
    (backward-text arg)
    (let ((p1 (point))
	  (p2 (progn
		(forward-text arg)
		(beginning-of-line)
		(point)))
	  (name (read-file-name 
		 (if kom-emacs-knows-iso-8859-1
		     (lyskom-format 'save-on-file-q
				    (file-name-nondirectory
				     lyskom-saved-file-name))
		   (iso-8859-1-to-swascii 
		    (lyskom-format 'save-on-file-q
				   (file-name-nondirectory
				    lyskom-saved-file-name))))
		 (file-name-directory lyskom-saved-file-name)
		 lyskom-saved-file-name
		 nil)))
      (if (file-directory-p name)
	  (setq name (concat (file-name-as-directory name)
			     (file-name-nondirectory lyskom-saved-file-name))))
      (append-to-file p1 p2
		      (expand-file-name name))
      (setq lyskom-saved-file-name name))))


;;; ================================================================
;;;                Some useful abstractions

(defsubst lyskom-membership-highest-index ()
  "Return the number of conferences the user is a member of minus 1.
This is the highest index in lyskom-membership that contains data, if
positions are counted from 0, as they are."
  (1- (length lyskom-membership)))


;;; ================================================================
;;;                             To-do


(defun lyskom-start-of-command (function &optional may-interrupt)
  "This function is run at the beginning of every LysKOM command.
It moves the cursor one line down, and +++ later it will tell the server
that the previous text has been read.

Argument FUNCTION is a string the string will be written in the buffer
on start of the command. If it is a symbol it searches for the corresponding
command name in lyskom-commands and writes this in the message buffer.

If optional argument MAY-INTERRUPT is present and non-nil,
don't signal an error if this call is interrupting another command.

Special: if lyskom-is-waiting then we are allowed to break if we set 
lyskom-is-waiting nil.

This function checks if lyskom-doing-default-command and
lyskom-first-time-around are bound. The text entered in the buffer is
chosen according to this"

  (if (and lyskom-is-waiting
           (listp lyskom-is-waiting))
      (progn
        (setq lyskom-is-waiting nil)
        (lyskom-end-of-command)))

  (setq lyskom-is-waiting nil)
  (if (and lyskom-executing-command (not may-interrupt))
      (lyskom-error "%s" (lyskom-get-string 'wait-for-prompt)))
  (if (not (and (boundp 'lyskom-doing-default-command)
                lyskom-doing-default-command))
      (cond
       (lyskom-first-time-around)
       ((stringp function) (lyskom-insert function))
       ((and function (symbolp function))
        (let ((name (lyskom-command-name function)))
          (if name (lyskom-insert name)))))
    (save-excursion
      (if (not lyskom-no-prompt)
          (let ((buffer-read-only nil))
            (goto-char (point-max))
            (delete-char (- (length lyskom-prompt-text))))))
    (lyskom-insert lyskom-prompt-executing-default-command-text))
  (setq mode-line-process (lyskom-get-string 'mode-line-working))
  (if (pos-visible-in-window-p (point-max))
      (save-excursion
        (goto-char (point-max))
        (lyskom-set-last-viewed)))
  (setq lyskom-executing-command t)
  (setq lyskom-current-command function)
  (lyskom-insert "\n")
  (if (and (eq (window-buffer (selected-window))
               (current-buffer))
           ;; (= (point) (point-max))
	   ) 
      (progn
	(if (pos-visible-in-window-p (1- (point-max)))
	    (goto-char (point-max)))
        (sit-for 0))) 
                                        ;  (lyskom-scroll)
  (run-hooks 'lyskom-before-command-hook)
  (if kom-page-before-command           ;Nice with dumb terminals.
      (if (or (not (listp kom-page-before-command))
              (memq function kom-page-before-command))
          (recenter 1))))


(defun lyskom-end-of-command ()
  "Print prompt, maybe scroll, prefetch info."
  (message "")
  (let ((start 0))
    (while (and lyskom-to-be-printed-before-prompt
                (lyskom-queue->first lyskom-to-be-printed-before-prompt))
      (setq start (point-max))
      (lyskom-insert (car (lyskom-queue->first 
                           lyskom-to-be-printed-before-prompt)))
      (lyskom-queue-delete-first lyskom-to-be-printed-before-prompt)))
  (setq lyskom-executing-command nil)
  (setq lyskom-current-command nil)
  (setq lyskom-no-prompt t)
  (lyskom-scroll)
  (setq mode-line-process (lyskom-get-string 'mode-line-waiting))
  (if (pos-visible-in-window-p (point-max) (selected-window))
      (lyskom-set-last-viewed))
  (lyskom-prefetch-and-print-prompt)
  (run-hooks 'lyskom-after-command-hook)
  (if kom-inhibit-typeahead
      (discard-input)))


(defun lyskom-print-prompt ()
  "Print prompt if the client knows which command will be default.
Set lyskom-no-prompt otwherwise. Tell server what I am doing."
  (setq lyskom-no-prompt nil)
  (let ((to-do (lyskom-what-to-do)))
    (setq lyskom-command-to-do to-do)
    (cond
     
     ((eq to-do 'next-pri-conf)
      (lyskom-insert-string (lyskom-modify-prompt 'go-to-pri-conf-prompt))
      (lyskom-beep kom-ding-on-priority-break))

     ((eq to-do 'next-pri-text)
      (lyskom-insert-string (lyskom-modify-prompt 'read-pri-text-conf))
      (lyskom-beep kom-ding-on-priority-break))

     ((eq to-do 'next-text)
      (lyskom-insert
       (lyskom-modify-prompt
        (let ((read-info (read-list->first lyskom-reading-list)))
          (cond
           ((eq 'REVIEW (read-info->type read-info))
            (lyskom-get-string 'review-next-text-prompt))
           ((eq 'REVIEW-TREE (read-info->type read-info))
            (lyskom-get-string 'review-next-comment-prompt))
           ((eq 'REVIEW-MARK (read-info->type read-info))
            (lyskom-get-string 'review-next-marked-prompt))
	   ;; The following is not really correct. The text to be read
	   ;; might be in another conference.
           ((= lyskom-current-conf lyskom-pers-no)
            (lyskom-get-string 'read-next-letter-prompt))
           ((eq 'FOOTN-IN (read-info->type read-info))
            (lyskom-get-string 'read-next-footnote-prompt))
           ((eq 'COMM-IN (read-info->type read-info))
            (lyskom-get-string 'read-next-comment-prompt))
           (t (lyskom-get-string 'read-next-text-prompt)))))))

     ((eq to-do 'next-conf)
      (lyskom-insert
       (lyskom-modify-prompt
        (cond
         ((eq 'REVIEW-MARK 
              (read-info->type (read-list->first lyskom-to-do-list)))
          (lyskom-get-string 'go-to-conf-of-marked-prompt))
         ((/= lyskom-pers-no
              (conf-stat->conf-no
               (read-info->conf-stat (read-list->first
                                      lyskom-to-do-list))))
          (lyskom-get-string 'go-to-next-conf-prompt))
         (t (lyskom-get-string 'go-to-your-mailbox-prompt))))))

     ((eq to-do 'when-done)
      (if (not lyskom-is-writing)
          (lyskom-tell-server kom-mercial))
      (lyskom-insert
       (lyskom-modify-prompt
        (let ((command (lyskom-what-to-do-when-done t)))
          (cond			    
           ((lyskom-command-name command))
           ((and (stringp command)
                 (lyskom-command-name (key-binding command))))
           (t (lyskom-format 'the-command command)))))))

     ((eq to-do 'unknown)		;Pending replies from server.
      (setq lyskom-no-prompt t))

     (t (signal 'lyskom-internal-error '(lyskom-print-prompt)))))

  (if lyskom-no-prompt
      nil
    (lyskom-insert lyskom-prompt-text))

  (lyskom-set-mode-line))


(defun lyskom-modify-prompt (s)
  "Modify the LysKOM prompt to reflect the current state of LysKOM."
  (let ((format-string (or kom-prompt-format "%s")))
    (if (symbolp s) (setq s (lyskom-get-string s)))
    (if lyskom-ansaphone-messages
        (if (> (length lyskom-ansaphone-messages) 0)
            (setq format-string 
                  (format (lyskom-get-string 'prompt-modifier-messages)
                          format-string
                          (length lyskom-ansaphone-messages)))))
    (if kom-ansaphone-on
        (setq format-string
              (format (lyskom-get-string 'prompt-modifier-ansaphone)
                      format-string)))

    (format format-string s)))

(defun lyskom-what-to-do ()
  "Check what is to be done. Return an atom as follows:
	next-pri-text	There is a text with higher priority to be read.
	next-pri-conf	There is a conference with higher priority to be read.
	next-text	There are texts on lyskom-reading-list.
	next-conf	There are texts on lyskom-to-do-list.
	when-done	There are no unread texts.
	unknown	        There are pending replies."
  (cond
   ((and kom-higher-priority-breaks
	 (not (read-list-isempty lyskom-reading-list))
	 (not (read-list-isempty lyskom-to-do-list))
	 (let ((type (read-info->type (read-list->first lyskom-reading-list))))
	   (or (eq kom-higher-priority-breaks 'express)
	       (eq type 'CONF)
	       (eq type 'REVIEW)
	       (eq type 'REVIEW-MARK)))
	 (> (read-info->priority (read-list->first lyskom-to-do-list))
	    (read-info->priority (read-list->first lyskom-reading-list))))
    (if (> (length 
	    (text-list->texts 
	     (read-info->text-list
	      (read-list->first lyskom-to-do-list))))
	   1)
       'next-pri-conf
      'next-pri-text))
   ((not (read-list-isempty lyskom-reading-list))
    'next-text)
   ((not (read-list-isempty lyskom-to-do-list))
    'next-conf)
   ((and lyskom-membership-is-read
	 (= lyskom-last-conf-received
	    (lyskom-membership-highest-index)))
    'when-done)
   (t 'unknown)))


(defun lyskom-what-to-do-when-done (&optional nochange)
  "Returns a command, the next command to do from the kom-do-when-done.
If optional argument NOCHANGE is non-nil then the list wont be altered."
  (let* ((now (cdr lyskom-do-when-done))
	 (all (car lyskom-do-when-done))
	 (next (cond
		((and now (eq now all) (cdr all))
		 (cdr all))
		(t all)))
	 (command
	  (cond
	   ((commandp (car now)) (car now))
	   ((and (listp (car next))
		 (not (eq (car (car next))
			  'lambda)))
	    (car (setq now (car next))))
	   (t (car (setq now next))))))
    (if nochange
	nil
      (setq lyskom-do-when-done (cons next (cdr now))))
    command))


(defun lyskom-prefetch-and-print-prompt ()
  "Prefetch info if needed. Print prompt if not already printed."
  (if (< (lyskom-known-texts)
         lyskom-prefetch-conf-tresh)
      (lyskom-prefetch-conf))
  (lyskom-prefetch-text)
  (if (and lyskom-is-waiting
	   (listp lyskom-is-waiting)
           (eval lyskom-is-waiting))
      (progn
        (setq lyskom-is-waiting nil)
        ;;(beep)
        ;;(lyskom-end-of-command)
        ;;(if (read-list-isempty lyskom-reading-list)
        ;;    (kom-go-to-next-conf))
        ;;(kom-next-command)
	))
  (if lyskom-no-prompt
      (lyskom-print-prompt)))


(defun lyskom-known-texts ()
  "Count how many unread texts the user have, that the client knows about."
  (apply '+ (mapcar '(lambda (x) (1- (length (read-info->text-list x))))
		    (read-list->all-entries lyskom-to-do-list))))


;;
;; Called from among others kom-list-news.
;;

;;(defun lyskom-prefetch-all-confs (num-arg continuation)
;;  "Gets all conferences using prefetch. Calls itself recursively.
;;When all confs are fetched then the function in the argument
;;CONTINUATION is called."
;;  ;; If all conf-stats are fetched, run the continuation function
;;  (if (>= lyskom-last-conf-fetched
;;	  (1- (length lyskom-membership)))
;;      (lyskom-run 'main 'lyskom-run 'prefetch continuation num-arg)

;;    ;; ...otherwise fetch next conf-stat.
;;    (let ((lyskom-prefetch-conf-tresh lyskom-max-int)
;;	  (lyskom-prefetch-confs lyskom-max-int))
;;      (lyskom-prefetch-conf))
;;    (lyskom-run 'main 'lyskom-prefetch-all-confs num-arg continuation)))

(defun lyskom-prefetch-all-confs ()
  "Gets all conferences using prefetch."
  (while (not (lyskom-prefetch-done))
    (let ((lyskom-prefetch-conf-tresh lyskom-max-int)
	  (lyskom-prefetch-confs lyskom-max-int))
      (lyskom-prefetch-conf))
    (accept-process-output nil lyskom-apo-timeout-s lyskom-apo-timeout-ms)))

;; ---------------------------------------------------------
;; prefetch conf-stats


(defun lyskom-prefetch-conf ()
  "Fetch conf-stats for next few conferences from lyskom-membership.
This is the main prefetch things function.

This is initiated by lyskom-refetch.

The following variables and functions are involved:
lyskom-last-conf-fetched, lyskom-last-conf-received, lyskom-last-conf-done
\(this functions variables).
lyskom-membership, lyskom-unread-confs (set at login).
Functions:
lyskom-prefetch-conf, starts the ball going, later verifies that everything
is done.
lyskom-prefetch-handle-conf, starts the ball for a conf.

Idea:
This functions fetches one conf from lyskom-unread-confs, creates an empty
read-list entry and fires away a lyskom-prefetch-handle-conf. The 
lyskom-prefetch-handle-conf fills the read-list entry with articles to be
read. When all articles are fetched then lyskom-prefetch-handle-conf
will increase lyskom-last-conf-done and call lyskom-prefetch-conf that fetches
the next conf.

If we start reading before everything is fetched then two things will happen.
For every thing we do (every prompt we get) there will be another 
lyskom-prefetch-conf started and possibly another thread of 
lyskom-prefetch-handle-conf. This will not be a problem.

List news and other things that require the correct count of articles will
have to wait. The correct way of waiting is to use lyskom-prefetch-all-confs.

If we just want to know wether we have fetched all info or not we do the test
\(lyskom-prefetch-done)."
  
  ;; Algoritm:
  ;;

  (let ((lyskom-prefetch-confs lyskom-prefetch-confs))
    (while (and (< lyskom-last-conf-fetched
		   (lyskom-membership-highest-index))
		(< (- lyskom-last-conf-fetched lyskom-last-conf-received)
		   lyskom-prefetch-confs))
      (++ lyskom-last-conf-fetched)
      (let ((membership (elt lyskom-membership lyskom-last-conf-fetched)))
	(if (lyskom-conf-no-list-member (membership->conf-no membership)
					lyskom-unread-confs)
	    (initiate-get-conf-stat 'main 'lyskom-prefetch-handle-conf
				    (membership->conf-no membership)
				    membership)
	  (++ lyskom-last-conf-done)
	  (++ lyskom-prefetch-confs)
	  (++ lyskom-last-conf-received))))))


(defun lyskom-prefetch-done ()
  "Returns t if lyskom has fetched all its info."
  (>= lyskom-last-conf-done
      (lyskom-membership-highest-index)))


(defun lyskom-prefetch-handle-conf (conf-stat membership)
  "Check if there is any unread texts in a conference.
Args: CONF-STAT MEMBERSHIP"
  (++ lyskom-last-conf-received)
  (cond
   ((and (lyskom-visible-membership membership)
	 (> (+ (conf-stat->first-local-no conf-stat)
	       (conf-stat->no-of-texts conf-stat)
	       -1)
	    (membership->last-text-read membership)))
    ;; There are (probably) some unread texts in this conf.
    (initiate-get-map 'prefetch 'lyskom-prefetch-handle-map
		      (conf-stat->conf-no conf-stat)
		      (1+ (membership->last-text-read membership))
		      (+ (conf-stat->no-of-texts conf-stat)
			 (conf-stat->first-local-no conf-stat)
			 (- (membership->last-text-read membership)))
		      membership
		      conf-stat))
   (t 
    ;; Consider this conference handled
    (++ lyskom-last-conf-done)
    (lyskom-prefetch-and-print-prompt))))


(defun lyskom-prefetch-handle-map (map membership conf-stat)
  "Add info about unread texts in a conf to the lyskom-to-do-list.
Args: MAP MEMBERSHIP CONF-STAT.
MAP is the mapping from local to global text-nos for (at least) all
texts after membership->last-text-read. MEMBERSHIP is info about the
user's membership in the conference."
  (++ lyskom-last-conf-done)
  (let ((unread (lyskom-list-unread map membership)))
    (cond
     (unread
      (read-list-enter-read-info
         (lyskom-create-read-info
	    'CONF
	    conf-stat
	    (membership->priority membership)
	    (lyskom-create-text-list unread))
	 lyskom-to-do-list))))
  (lyskom-prefetch-and-print-prompt))


(defun lyskom-list-unread (map membership)
  "Args: MAP MEMBERSHIP. Return a list of unread texts.
The list consists of text-nos."
  (let ((res nil)
	(last-read (membership->last-text-read membership))
	(read (membership->read-texts membership))
	(first (map->first-local map))
	(i (length (map->text-nos map)))
	(the-map (map->text-nos map)))
    (while (> i 0)
      (-- i)
      (cond
       ((zerop (elt the-map i)))	;Deleted text - do nothing.
       ((<= (+ first i) last-read))	;Already read - do nothing.
       ((lyskom-vmemq  (+ i first) read)) ;Already read - do nothing.
       (t				;Unread - add to list.
	(setq res (cons
		   (elt the-map i)
		   res)))))
    res))


(defun lyskom-conf-fetched-p (conf-no)
  "Return t if CONF-NO has been prefetched."
  (let ((n lyskom-last-conf-received)
	(result nil))
    (while (and (not result)
		(>= n 0))
      (if (= (membership->conf-no (elt lyskom-membership n))
	     conf-no)
	  (setq result t))
      (-- n))
    result))


;;-------------------------------------------------------
;; prefetch text-stats

(defun lyskom-prefetch-text ()
  "Make sure that at least lyskom-prefetch-texts texts are fetched."
  (lyskom-prefetch-from-rlist
   (lyskom-prefetch-from-rlist lyskom-prefetch-texts
			       (read-list->all-entries lyskom-reading-list))
   (read-list->all-entries lyskom-to-do-list)))


(defun lyskom-prefetch-from-rlist (n-texts rlist)
  "Prefetch first N-TEXTS texts from RLIST.
Returns number of texts that could not be fetched.
RLIST is a list of reading-info."
  (cond
   ((< n-texts 1)
    0)
   ((null rlist)
    n-texts)
   (t
    (lyskom-prefetch-from-rlist
     (lyskom-prefetch-from-list
      n-texts
      (cdr (read-info->text-list (car rlist))))
     (cdr rlist)))))


(defun lyskom-prefetch-from-list (n-texts list)
  "Prefetch first N-TEXTS texts from LIST.
Returns number of texts that could not be fetched.
RLIST is a list of text-nos. Texts whose text-no is present on
lyskom-fetched-texts are not fetched."
  (while (and (not (null list)) (> n-texts 0))
    ;; Fetch this text - but only if we are not already fetching it.
    (if (memq (car list) lyskom-fetched-texts)
	nil				;already fetched (but maybe not yet
					;received).
      (initiate-get-text-stat 'background 'lyskom-prefetch-comment-stats
			      (car list))
      (initiate-get-text 'background nil (car list))
      (setq lyskom-fetched-texts (cons (car list) lyskom-fetched-texts)))
    (setq list (cdr list))
    (-- n-texts))
  n-texts)

(defun lyskom-prefetch-comment-stats (text-stat)
  "Prefetch the text-stats of the comments to this text."
  (let ((misc (text-stat->misc-info-list text-stat)))
    (while misc
      (cond ((eq 'COMM-IN (misc-info->type (car misc)))
	     (initiate-get-text-stat 'background nil
				     (misc-info->comm-in (car misc))))
	    ((eq 'FOOTN-IN (misc-info->type (car misc)))
	     (initiate-get-text-stat 'background nil
				     (misc-info->footn-in (car misc))))
	    ((eq 'COMM-TO (misc-info->type (car misc)))
	     (initiate-get-text-stat 'background nil
				     (misc-info->comm-to (car misc))))
	    ((eq 'FOOTN-TO (misc-info->type (car misc)))
	     (initiate-get-text-stat 'background nil
				     (misc-info->footn-to (car misc))))
	    ((or (eq 'RCPT (misc-info->type (car misc)))
		 (eq 'CC-RCPT (misc-info->type (car misc))))
	     (initiate-get-conf-stat 'background nil
				     (misc-info->recipient-no (car misc))))
	    (t nil))
      (setq misc (cdr misc)))))

;;;; ================================================================


(defun lyskom-vmemq (elt vector)
  "Return t if ELT is a member of (present in) VECTOR."
  (let ((found nil)
	(i (length vector)))
    (while (and (> i 0)
		(not found))
      (-- i)
      (if (eq elt (aref vector i))
	  (setq found t)))
    found))

      
(defun lyskom-read-num-range (low high &optional prompt show-range default)
  "Read a number from the minibuffer.
Args: LOW HIGH &optional PROMPT SHOW-RANGE with default value DEFAULT.
The read number must be within the range [LOW HIGH].
If SHOW-RANGE is non-nil, the prompt will include the range for information
to the user."
  (let ((number (1- low)))
    (while (or (< number low)
	       (> number high))
      (setq number (lyskom-read-number 
		    (concat (if prompt
				prompt
			      (lyskom-get-string 'give-a-number))
			    (if show-range
				(format "(%d-%d) " low high)
			      ""))
		    default)))
    number))


(defun lyskom-read-number (&optional prompt default)
  "Read a number from the minibuffer. Optional arguments: PROMPT DEFAULT
If DEFAULT is non-nil, it is written within parenthesis after the prompt.
DEFAULT could also be of the type which (interactive P) generates.
If quit is typed it executes lyskom-end-of-command."
  (let ((numdefault (cond ((null default) nil)
			  ((integerp default) default)
			  ((listp default) (car default))
			  (t nil)))
	(number nil)
	(numstr nil)
	(quit nil))
    (while (not number)
      (setq quit t)
      (setq numstr
	    (prog1
		(lyskom-read-string
		 (concat (if prompt 
			     prompt
			   (lyskom-get-string 'give-a-number))
			 (if numdefault 
			     (format " (%d) " numdefault)
			   " ")))
	      (setq quit nil)))
      (cond ((and (string= numstr "") 
		  numdefault)
	     (setq number numdefault))
	    ((string-match "\\`[0-9]+\\'" numstr)
	     (setq number (string-to-int numstr)))
	    (t (beep))))
    number))


(defun lyskom-read-string (prompt &optional initial)
  "Read a string from the minibuffer. Arguments: PROMPT INITIAL"
  (read-string (if kom-emacs-knows-iso-8859-1
		   prompt
		 (iso-8859-1-to-swascii prompt))
	       initial))



(defun ja-or-nej-p (prompt &optional initial-input)
  "Same as yes-or-no-p but language-dependent.
Uses lyskom-message, lyskom-read-string to do interaction and
lyskom-get-string to retrieve regexps for answer and string for repeated query."
  (let ((answer "")
	(nagging nil))
    (while (not (or (string-match (lyskom-get-string 'yes-regexp) answer)
		    (string-match (lyskom-get-string 'no-regexp) answer)))
      (if nagging
	  (progn
	    (lyskom-message "%s" (lyskom-get-string 'yes-or-no-nag))
	    (sit-for 2)))
      (setq answer (lyskom-read-string (concat prompt (lyskom-get-string 'yes-or-no))
				       initial-input))
      (setq nagging t))
    (not (string-match (lyskom-get-string 'no-regexp) answer))))


(defun j-or-n-p (prompt)
  "Same as y-or-n-p but language-dependent.
Uses lyskom-message, lyskom-read-string to do interaction and
lyskom-get-string to retrieve regexps for answer and string for repeated query."
  (let ((input-char ?a)
	(cursor-in-echo-area t)
	(nagging nil))
    (while (not (char-in-string input-char (lyskom-get-string 'y-or-n-instring)))
	(lyskom-message "%s" (concat (if nagging 
					 (lyskom-get-string 'j-or-n-nag)
				       "") 
				     prompt
				     (lyskom-get-string 'j-or-n)))
	(if nagging 
	    (beep))
	(setq input-char (read-char))
	(setq nagging t))
    (char-in-string input-char (lyskom-get-string 'y-instring))))

  
;;; lyskom-j-or-n-p, lyskom-ja-or-no-p
;;; These versions perform lyskom-end-of-command if quit is signalled
;; Author: Linus Tolke

(defun lyskom-j-or-n-p (prompt)
  "Same as j-or-n-p but performs lyskom-end-of-command if quit."
  (condition-case error
      (j-or-n-p prompt)
    (quit (lyskom-end-of-command)
	  (signal 'quit "In lyskom-j-or-n-p"))))


(defun lyskom-ja-or-nej-p (prompt &optional initial-input)
  "Same as ja-or-nej-p but performs lyskom-end-of-command if quit."
  (condition-case error
      (ja-or-nej-p prompt initial-input)
    (quit (lyskom-end-of-command)
	  (signal 'quit "In lyskom-j-or-n-p"))))



(defun lyskom-membership-< (a b)
  "Retuns t if A has a higher priority than B. A and B are memberships."
  (> (membership->priority a)
     (membership->priority b)))


(defun impl ()
  (error "Not implemented"))


(defun lyskom-call-hook (hook-name &rest args)
  "Calls a hook with parameters.
If HOOK-NAME is bound, it is either a function name or a list of functions
to apply to the ARGS."

  (if (and (boundp hook-name) (eval hook-name))
      (if (listp (eval hook-name))
	  (mapcar '(lambda (fn)
		     (apply fn args))
		  (eval hook-name))
	(apply (eval hook-name) args))))


;;; Priority filtering

(defun lyskom-visible-membership (membership)
  "Is this conference visible?
Return t is MEMBERSHIPs priority is higher than or equal to
lyskom-session-priority and nil otherwise.

If MEMBERSHIPs prioriy is 0, it always returns nil."
  (let ((priority (membership->priority membership)))
    (and (> priority 0)
	 (>= priority lyskom-session-priority))))

;;; The filter.

(defun lyskom-filter (proc output)
  "Receive replies from LysKOM server."
  (sit-for 0)				; Why?
  (let (; (inhibit-quit t)		;inhibit-quit is automatically set
					;to t in version 18.57, but not in
					;all older versions of emacs.
	(old-match-data (match-data))
	;; lyskom-filter-old-buffer is also changed when starting to edit
	;; in function lyskom-edit-text.
	(lyskom-filter-old-buffer (current-buffer)))
    (unwind-protect
	(condition-case error
	    (progn
	      (setq lyskom-quit-flag nil)

	      (if lyskom-debug-communications-to-buffer
		  (lyskom-debug-insert proc "-----> " output))
	      
	      (set-buffer (process-buffer proc))
	      (princ output lyskom-unparsed-marker) ;+++lyskom-string-skip-whitespace
	      (if quit-flag		; We are allowed to break here.
		  (setq inhibit-quit nil)) ; This will break
					   ; instantly.

	      ;; Keep inhibit-quit set to t
	      (cond

	       ((and (> lyskom-string-bytes-missing 0)
		     (< (length output) lyskom-string-bytes-missing))
		(setq lyskom-string-bytes-missing
		      (- lyskom-string-bytes-missing (length output))))

	       ;; This test makes e.g. startup a lot faster.
	       ((not (string-match "\n" output)))
	       
	       ((null lyskom-is-parsing) ;Parse one reply at a time.
		(setq lyskom-is-parsing t)
		(unwind-protect
		    (condition-case error-type
			(lyskom-parse-unparsed)
		      (lyskom-parse-incomplete)) ;Incomplete answers are normal.
		  (set-buffer (process-buffer proc)) ;In case it was changed by
					;        ;the handler.
		  (setq lyskom-is-parsing nil)))))
	  ;; condition-case handler
	  (quit (setq lyskom-quit-flag t))
	  ;; (lyskom-protocol-error
	  ;;   (lyskom-message "%s" (lyskom-get-string 'protocol-error) err))
	  )
      
      ;; Restore selected buffer and match data.
      (store-match-data old-match-data)
      (set-buffer lyskom-filter-old-buffer))))
      

;;; The sentinel

(defun lyskom-sentinel (proc sentinel)
  "Handles changes in the lyskom-process."
  (setq lyskom-sessions-with-unread
	(delq proc lyskom-sessions-with-unread))
  (set-buffer (process-buffer proc))
  (lyskom-start-of-command (lyskom-get-string 'process-signal) t)
  (lyskom-clear-vars)
  (lyskom-format-insert 'closed-connection sentinel)
  (setq mode-line-process (lyskom-get-string 'mode-line-down))
  (beep)
  (lyskom-scroll))


;;; ================================================================
;;;         Debug buffer

(defun lyskom-debug-insert (proc prefix string)
  (let* ((buf (get-buffer-create
	       lyskom-debug-communications-to-buffer-buffer))
	 (win (get-buffer-window buf 'visible)))
    (if win
	(save-excursion
	  (save-selected-window
	    (select-window win)
	    (let ((move (eobp)))
	      (save-excursion
		(goto-char (point-max))
		(insert "\n"
			(format "%s" proc)
			prefix  string))
	      (if move (goto-char (point-max))))))
      (save-excursion
	(set-buffer buf)
	(goto-char (point-max))
	(insert "\n"
		(format "%s" proc)
		prefix  string)))))


;;; For serious bugs
(defun lyskom-really-serious-bug ()
  (let ((debug-on-error t))
    (error "Congratulations! You found a serious bug in lyskom.el.
Press q to leave this buffer, and please run M-x kom-bug-report afterwards.")))
  


;;; ================================================================
;;;         Formatting functions for different data types


(defun lyskom-format-objects (&rest args)
  "Arguments: (&rest ARGS). Format ARGS to correct format to send to server.
Strings are converted to Hollerith strings.
Other objects are converted correctly."
  (apply 'concat (mapcar 'lyskom-format-object args)))


(defun lyskom-format-object (object)
  (concat " "
	  (cond
	   ((stringp object) (lyskom-format-string object))
	   ((integerp object) (int-to-string object))
	   ((listp object)
	    (cond
	     ((eq (car object) 'MISC-LIST)
	      (lyskom-format-misc-list (cdr object)))
	     ((eq (car object) 'CONF-TYPE)
	      (lyskom-format-conf-type object))
	     ((eq (car object) 'PRIVS)
	      (lyskom-format-privs object))
	     ((eq (car object) 'LIST)
	      (lyskom-format-simple-list (cdr object)))
	     (t
	      (signal 'lyskom-internal-error
		      (list 'lyskom-format-object
			    ": no support for object "
			    object)))))
	   (t (signal 'lyskom-internal-error
		      (list 'lyskom-format-object
			    ": no support for object "
			    object))))))


(defun lyskom-format-conf-type (conf-type)
  "Format a CONF-TYPE for output to the server."
  (concat
   (lyskom-format-bool (conf-type->rd_prot conf-type))
   (lyskom-format-bool (conf-type->original conf-type))
   (lyskom-format-bool (conf-type->secret conf-type))
   (lyskom-format-bool (conf-type->letterbox conf-type))
   (lyskom-format-bool (conf-type->anarchy conf-type))
   (lyskom-format-bool (conf-type->rsv1 conf-type))
   (lyskom-format-bool (conf-type->rsv2 conf-type))
   (lyskom-format-bool (conf-type->rsv3 conf-type))))


(defun lyskom-format-privs (privs)
  "Format PRIVS for output to the server."
  (concat
   (lyskom-format-bool (privs->wheel privs))
   (lyskom-format-bool (privs->admin privs))
   (lyskom-format-bool (privs->statistic privs))
   (lyskom-format-bool (privs->create_pers privs))
   (lyskom-format-bool (privs->create_conf privs))
   (lyskom-format-bool (privs->change_name privs))
   (lyskom-format-bool (privs->flg7 privs))
   (lyskom-format-bool (privs->flg8 privs))
   (lyskom-format-bool (privs->flg9 privs))
   (lyskom-format-bool (privs->flg10 privs))
   (lyskom-format-bool (privs->flg11 privs))
   (lyskom-format-bool (privs->flg12 privs))
   (lyskom-format-bool (privs->flg13 privs))
   (lyskom-format-bool (privs->flg14 privs))
   (lyskom-format-bool (privs->flg15 privs))
   (lyskom-format-bool (privs->flg16 privs))))

(defun lyskom-format-bool (bool)
  "Format a BOOL for output to the server."
  (if bool 1 0))
      

(defun lyskom-format-misc-list (misc-list)
  "Format a misc-list for output to the server."
  (let ((result (format "%d {" (length misc-list))))
    (while (not (null misc-list))
      (setq result (concat result " "
			    (lyskom-format-misc-item (car misc-list))))
      (setq misc-list (cdr misc-list)))
    (setq result (concat result " }\n"))))


(defun lyskom-format-misc-item (misc-item)
  "Format a misc-item for output to the server."
  (format "%d %d"
	  (cond
	   ((eq (car misc-item) 'recpt) 0)
	   ((eq (car misc-item) 'cc-recpt) 1)
	   ((eq (car misc-item) 'comm-to) 2)
	   ((eq (car misc-item) 'footn-to) 4))
	  (cdr misc-item)))


(defun lyskom-format-simple-list (list)
  "Format some kind of list to send to server."
  (apply 'concat (list (format "%d {" (length list))
		       (apply 'lyskom-format-objects list)
		       " }\n")))

  
(defun lyskom-format-string (string)
  (concat (format "%d"(length string))
	  "H" string))


;;;; ================================================================
;;;;                     Utility routines.


;;; silent-read was
;; Written by Lars Willf|r <willfor@lysator.liu.se>
;; Copyright and copyleft Lars Willf|r.
;; Last modified jun 93 by Linus Tolke

(defun silent-read (prompt-str)
  "Read a string in the minibuffer without echoing.
One parameter - the prompt string."
  (interactive "sPrompt string: ")
  (lyskom-message "%s" prompt-str)
  (let ((input-string "")
	(input-char)
	(cursor-in-echo-area t))
    (while (not (or (eq (setq input-char 
			      (condition-case err
				  (read-char)
				(error (if (string= "Non-character input-event"
						    (car (cdr err)))
					   ?\r
					 (signal (car err) (cdr err))))))
			?\r)
		    (eq input-char ?\n)))
      (progn
	(lyskom-message "%s" prompt-str)
	(setq input-string
	      (cond
	       ((eq input-char ?\C-?)
		(if (equal (length input-string)
			   0)
		    ""
		  (substring input-string 0 -1)))
	       ((eq input-char ?\C-u)
		"")
	       (t
		(concat input-string (char-to-string input-char)))))))
    (message "")
    input-string))

;;; This really is a strange thing to do but...
;;
(defun lyskom-mode-name-from-host ()
  "Calculate what to identify the buffer with."
  (let ((server  (process-name (get-buffer-process (current-buffer)))))
    (or (cdr (assoc server kom-server-aliases))
	(format "LysKOM(%s)" server))))


;;; Validation of kom-tell-phrases
;;;
;;; Author: Roger Mikael Adolfsson

(defun lyskom-missing-fields (alist blist)
  "Returns the list of fields from ALIST that are missing in BLIST."
  (let (caralist clist (alist (copy-alist alist)))
    (while alist
      (setq caralist (car (car alist)))
      (if (assq caralist blist)
	  nil
	(setq clist (cons caralist clist)))
      (setq alist (cdr alist)))
    clist))

(defun lyskom-tell-phrases-validate ()
  "Attempts to validate the value of kom-tell-phrases
from the value of kom-tell-phrases-internal."
  (interactive)
  (let (invalid)
    (cond ((setq invalid
		 (lyskom-missing-fields
		  lyskom-tell-phrases-validation-keyword-list
		  kom-tell-phrases))
	   (error "%s must be in kom-tell-phrases" invalid))
	  ((setq invalid
		 (lyskom-missing-fields 
		  kom-tell-phrases
		  lyskom-tell-phrases-validation-keyword-list))
	   (error "%s should not be in kom-tell-phrases" invalid)))))


(or (memq 'lyskom-unread-mode-line global-mode-string)
    (nconc global-mode-string (list 'lyskom-unread-mode-line)))
(setq lyskom-unread-mode-line
      (list (list 'lyskom-sessions-with-unread 
		  (let ((str (lyskom-get-string 'mode-line-unread)))
		    (if kom-emacs-knows-iso-8859-1
			str
		      (iso-8859-1-to-swascii str))))
	    (list 'lyskom-sessions-with-unread-letters
		  (let ((str (lyskom-get-string 'mode-line-letters)))
		    (if kom-emacs-knows-iso-8859-1
			str
		      (iso-8859-1-to-swascii str))))))
		 


(setq lyskom-swascii-commands
      (mapcar 
       (function (lambda (pair)
		   (list (car pair) (iso-8859-1-to-swascii (car (cdr pair))))))
       lyskom-commands))
(setq lyskom-swascii-header-separator 
      (iso-8859-1-to-swascii lyskom-header-separator))
(setq lyskom-swascii-header-subject
      (iso-8859-1-to-swascii lyskom-header-subject))

(setq lyskom-swascii-filter-actions
      (mapcar 
       (function (lambda (pair)
		   (cons (car pair) (iso-8859-1-to-swascii (cdr pair)))))
       lyskom-filter-actions))
(setq lyskom-swascii-filter-what
      (mapcar 
       (function (lambda (pair)
		   (cons (car pair) (iso-8859-1-to-swascii (cdr pair)))))
       lyskom-filter-what))

(setq lyskom-emacs19-p (string-match "^19" emacs-version))


;;; This should be the very last lines of lyskom.el Everything should
;;; be loaded now, so it's time to run the lyskom-after-load-hook.

(run-hooks 'lyskom-after-load-hook)

;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; end: 
