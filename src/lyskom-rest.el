;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: lyskom-rest.el,v 44.64 1999-06-10 13:36:14 byers Exp $
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
;;;;    David K}gedal
;;;;    David Byers
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

(eval-when-compile
  (require 'lyskom-command "command")
  (require 'lyskom-menus "menus")
  (require 'lyskom-clienttypes "clienttypes.el"))


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: lyskom-rest.el,v 44.64 1999-06-10 13:36:14 byers Exp $\n"))

(lyskom-external-function find-face)


;;;; ================================================================
;;;;                             New errors.


(put 'lyskom-protocol-error 'error-conditions
     '(error lyskom-error lyskom-protocol-error))
(put 'lyskom-protocol-error 'error-message
     "LysKOM protocol error")

(put 'lyskom-internal-error 'error-conditions
     '(error lyskom-error lyskom-internal-error))
(put 'lyskom-internal-error 'error-message
     "Internal LysKOM error")

(put 'lyskom-format-error 'error-conditions
     '(error lyskom-error lyskom-format-error))
(put 'lyskom-internal-error 'error-message
     "Internal LysKOM format error")


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
  (or
   (lyskom-get-string-internal (intern (concat "error-"
					       (number-to-string errno)))
			       'lyskom-error-texts)
   (lyskom-format 'error-not-found errno)))

(defun lyskom-report-command-answer (answer)
  "Handles a void return from call to the server."
  (if answer 
      (lyskom-insert-string 'done)
    (lyskom-insert-string 'nope)
    (lyskom-format-insert 'error-code
			  (lyskom-get-error-text lyskom-errno)
			  lyskom-errno))
  answer)


;;; ----------------------------------------------------------------
;;; Author: Aronsson

(defun lyskom-tell-internat (key)
  "Same as lyskom-tell-server, but use a key to a list of phrases."
  ;; The server at Lysator produces a lot of network traffic. The
  ;; solution should to be less verbose. This is very interesting
  ;; anymore, anyway.
  (lyskom-tell-server (lyskom-tell-string key)))



;;; Resume operation after a crash.

(defun kom-recover (&optional refetch)
  "Try to recover from an error.
If the optional argument REFETCH is non-nil, all caches are cleared and
`lyskom-refetch' is called."
  (interactive "p")
  (lyskom-init-parse lyskom-buffer)
  (setq lyskom-call-data nil)
  (setq lyskom-pending-calls nil)
  (setq lyskom-output-queues (make-vector 10 nil))
  (let ((i 0))
    (while (< i 10)
      (aset lyskom-output-queues i (lyskom-queue-create))
      (++ i)))
  (setq lyskom-number-of-pending-calls 0)
  (setq lyskom-is-parsing nil)
  (when refetch
    (clear-all-caches)
    (lyskom-refetch))
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
	(goto-char (point-max))
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
    (lyskom-scroll)
    (if (< (window-start) lyskom-last-viewed)
        (lyskom-xemacs-or-gnu
         (let ((overlay (make-extent 
                         lyskom-last-viewed
                         (save-excursion (goto-char lyskom-last-viewed)
                                         (end-of-line)
                                         (forward-char 1)
                                         (point)))))
           (set-extent-face overlay 'kom-mark-face)
           (add-timeout 2 
                        'delete-extent
                        overlay))
         (let ((overlay (make-overlay 
                         lyskom-last-viewed
                         (save-excursion (goto-char lyskom-last-viewed)
                                         (end-of-line)
                                         (forward-char 1)
                                         (point)))))
           (overlay-put overlay 'face 'kom-mark-face)
           (run-at-time 2 nil
                        'delete-overlay
                        overlay))))))

  
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
	 ((or (stringp command)
              (vectorp command))
	  (execute-kbd-macro command))
         ((commandp command) (call-interactively command))
         (t (lyskom-start-of-command nil) (lyskom-end-of-command)))))
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
				    (not is-review)
				    (or (eq type 'REVIEW)
					(eq type 'REVIEW-MARK))))
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
            (eq 'BCC-RECPT (misc-info->type misc))
	    (eq 'CC-RECPT (misc-info->type misc)))
	(let ((membership (lyskom-try-get-membership
			   (misc-info->recipient-no misc))))
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
  ;;
  ;; Deal with special membership types
  ;;
  (let ((mship (lyskom-get-membership (conf-stat->conf-no conf-stat))))
    (when mship

      ;; Check for invitation

      (when (membership-type->invitation (membership->type mship))
        (lyskom-format-insert 'your-invited
                              conf-stat
                              (membership->created-by mship))
        (when (lyskom-j-or-n-p (lyskom-get-string 'accept-invitation))
          (set-membership-type->invitation (membership->type mship)
                                           nil)
          (initiate-set-membership-type
           'main
           nil
           lyskom-pers-no
           (conf-stat->conf-no conf-stat)
           (membership->type mship))))

      ;; Check for going to passive membership

      (when (membership-type->passive (membership->type mship))
        (lyskom-format-insert 'enter-passive conf-stat))))

  (lyskom-run-hook-with-args 'lyskom-change-conf-hook
                             lyskom-current-conf
                             (conf-stat->conf-no conf-stat))
  (initiate-pepsi 'main nil (conf-stat->conf-no conf-stat))
  (setq lyskom-current-conf (conf-stat->conf-no conf-stat))
  (let ((num-unread (text-list->length (read-info->text-list read-info))))
    (lyskom-format-insert (if (not kom-print-number-of-unread-on-entrance)
			      'enter-conf
			    (if (= num-unread 1)
				'one-unread
			      'several-unread))
			  conf-stat
			  num-unread)))
  



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
		    (setq len (text-list->length
			       (read-info->text-list (car read-info-list))))
		    (if (= (conf-stat->conf-no conf)
			   (conf-stat->conf-no 
			    (read-info->conf-stat (car read-info-list))))
			(setq unread len))
		    (if (= lyskom-pers-no
			   (conf-stat->conf-no 
			    (read-info->conf-stat (car read-info-list))))
			(setq letters len))
		    (setq total-unread (+ total-unread len))))
	      (setq read-info-list (cdr read-info-list)))))
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
			 0 27)))
      
      (if (zerop total-unread)
	  (setq lyskom-sessions-with-unread
		(delq lyskom-buffer lyskom-sessions-with-unread))
	(or (memq lyskom-buffer lyskom-sessions-with-unread)
	    (setq lyskom-sessions-with-unread
		  (cons lyskom-buffer lyskom-sessions-with-unread))))
      (if (zerop letters)
	  (setq lyskom-sessions-with-unread-letters
		(delq lyskom-buffer lyskom-sessions-with-unread-letters))
	(or (memq lyskom-buffer lyskom-sessions-with-unread-letters)
	    (setq lyskom-sessions-with-unread-letters
		  (cons lyskom-buffer lyskom-sessions-with-unread-letters)))))
    (force-mode-line-update)))


;;; ================================================================

;;; +++Where should this be moved???


(defun lyskom-try-get-membership (conf-no)
  "Returns non-nil if conference CONF-NO is present on lyskom-membership.
The value is actually the membership for the conference.

For foreground functions, lyskom-get-membership should probably be used
instead.

This function does not use blocking-do."
  (let ((list lyskom-membership)
	(found nil))
    (while (and (not found) (not (null list)))
      (if (= conf-no (membership->conf-no (car list)))
	  (setq found (car list)))
      (setq list (cdr list)))
    found))


(defun lyskom-get-membership (conf-no)
  "Get the membership for CONF-NO, or nil if the user is not a member of
CONF-NO.

If the membership list is not fully prefetched and the membership can't be
found in lyskom-membership, a blocking call to the server is made."
  (or (lyskom-try-get-membership conf-no)
      (and (not (lyskom-membership-is-read))
	   (let ((membership
		  (blocking-do 'query-read-texts lyskom-pers-no conf-no)))
	     (if (and membership (lyskom-visible-membership membership))
		 (lyskom-add-membership membership conf-no))
	     membership))))


;;;; ================================================================
;;;;                   Scrolling and text insertion.


(defun lyskom-trim-buffer ()
  "Trim the size of a lyskom buffer to lyskom-max-buffer-size"
  (save-excursion
    (if (and kom-max-buffer-size
	     (> (buffer-size) kom-max-buffer-size))
	(let ((delchars (- (buffer-size) kom-max-buffer-size))
	      (inhibit-read-only t))
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
		(when (not (pos-visible-in-window-p))
		    (forward-char -1)
                    (when (> (current-column)
                             (window-width))
                      (backward-char (+ (- (current-column)
                                            (window-width))
                                        2)))
                    )))))))
	
;;;
;;; Thanks to the stupid danish fool who wrote the widget package, we
;;; have to do it this way, because w3 uses widgets, and because
;;; widgets use overlays, and because overlays aren't copied between
;;; buffers. If the idiot danish flaming asshole had used text
;;; properties or something equally sensible instead, we could have
;;; managed without this shit.
;;;
;;; (Me, upset? Why would you think *that*?)
;;;


(defun lyskom-do-insert (string)
  (let ((start (point)))
    (insert string)
    (let ((bounds (next-text-property-bounds 1 (max 1 (1- start))
                                             'special-insert))
          (next (make-marker))
          (fn nil))
      (while bounds
        (set-marker next (cdr bounds))
        (setq fn (get-text-property (car bounds) 'special-insert))
        (remove-text-properties (car bounds) (cdr bounds)
                                '(special-insert))
        (condition-case val
            (funcall fn (car bounds) (cdr bounds))
          (error (apply 'message "%S" val)))
        (setq start next)
        (setq bounds (next-text-property-bounds 1 start
                                                'special-insert))))
    ))



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
      (let ((inhibit-read-only t))
        (lyskom-do-insert string))
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
  (let ((inhibit-read-only t))
    (lyskom-do-insert string))
  (lyskom-trim-buffer))  


(defun lyskom-insert-before-prompt (string)
  "Insert STRING just before the prompt of if no prompt then just buffers.
If prompt on screen then do the scroll if necessary.
The strings buffered are printed before the prompt by lyskom-update-prompt."
  ;;
  ;; This is the policy for moving point:
  ;;
  ;; old-point-max is the point-max before the text is inserted
  ;; new-point-max is the point-max after the text is inserted
  ;;
  ;; If point /= old-point-max, leave the point where it is, and don't
  ;; scroll the window.
  ;;
  ;; If point = old-point-max, set it to new-point-max, and if the
  ;; buffer is in a window, make sure that point is visible.
  ;;
  (cond
   ((and lyskom-executing-command
         (not (eq lyskom-is-waiting t)))
    ;; Don't insert the string until the current command is finished.
    (if (null lyskom-to-be-printed-before-prompt)
        (setq lyskom-to-be-printed-before-prompt (lyskom-queue-create)))
    (lyskom-queue-enter lyskom-to-be-printed-before-prompt 
                        (list string)))
   (t
    ;; For some reaseon save-excursion doesn't work as expected
    (let ((oldpoint (point-marker)))
      (goto-char (point-max))
      (beginning-of-line)
      (let ((inhibit-read-only t))
	(lyskom-do-insert string))
      (goto-char oldpoint))
    (let ((window (get-buffer-window (current-buffer))))
      (if (and window
	       (not (pos-visible-in-window-p (point) window)))
	  ;; This mease that the prompt has been pushed off the bottom
	  (save-selected-window
	    (select-window window)
	    (recenter -1)))))))
      

(defun lyskom-message (format-string &rest args)
  "Like message, but converts iso-8859/1 texts to swascii if necessary.
Args: FORMAT-STRING &rest ARGS"
  (let ((str (apply 'format format-string args)))
    (message "%s" str)))


(defun lyskom-error (format-string &rest args)
  "Like error, but converts iso-8859/1 texts to swascii if necessary.
Args: FORMAT-STRING &rest ARGS"
  (let ((str (apply 'format format-string args)))
    (error "%s" str)))


(defun lyskom-set-last-viewed ()
  (save-excursion
    (vertical-motion 0)
    (setq lyskom-last-viewed (point))))


;;; ================================================================
;;;                  Extended string formatting

;;; Author: David Byers
;;; Original code: Inge Wallin


(defvar lyskom-format-format
  "%\\(=\\)?\\(-?[0-9]+\\)?\\(#\\([0-9]+\\)\\)?\\(:\\)?\\([][@MmPpnrtsdoxcCSD]\\)"
  "regexp matching format string parts.")

(defun lyskom-insert-string (atom)
 "Find the string corresponding to ATOM and insert it into the LysKOM buffer." 
  (lyskom-insert (lyskom-get-string atom)))

;;(defun lyskom-get-string (atom &optional assoc-list)
;;  "Get the string corresponding to ATOM and return it."
;;  (if (stringp atom)
;;	atom
;;    (let ((format-pair (assoc atom (or assoc-list lyskom-strings))))
;;	(if (null format-pair)
;;	    (signal 'lyskom-internal-error 
;;		    (list 'lyskom-get-string
;;			  (list atom ": string not found")))
;;	  (cdr format-pair)))))


(defun lyskom-format (format-string &rest argl)
  (format-state->result (lyskom-do-format format-string argl)))

(defun lyskom-format-insert (format-string &rest argl)
  "Format and insert a string according to FORMAT-STRING.
The string is inserted at the end of the buffer with `lyskom-insert'."
  (let* ((state (lyskom-do-format format-string argl t))
	 ;; We have to use a marker, because lyskom-insert may trim
	 ;; the buffer size.
	 (start (point-max-marker))
	 (deferred (format-state->delayed-content state)))
    (lyskom-insert (format-state->result state))
    (while deferred
      (let ((defer-info (car deferred))
	    (m (make-marker)))
	(set-marker m (+ start (defer-info->pos defer-info)))
	(set-defer-info->pos defer-info m)			     
	(lyskom-defer-insertion defer-info)
	(setq deferred (cdr deferred))))
    (set-marker start nil)))

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
  "Format and insert a string according to FORMAT-STRING.
The string is inserted just before the prompt, and if the prompt is not
currently visible the text is queued to be inserted when the prompt
reappears.

Note that it is not allowed to use deferred insertions in the text."
  (lyskom-insert-before-prompt
   (format-state->result (lyskom-do-format format-string argl))))


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



;;;
;;; If you add a format letter, for goodness' sake, don't forget to
;;; att it to the regexp above too!
;;;



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
     ;;  Format a command name somewhat specially
     ;;
     ((= format-letter ?C)
      (setq result (cond ((stringp arg) arg)
                         ((vectorp arg) 
                          (mapconcat 'single-key-description
                                     (append arg nil) " "))
                         ((and arg (symbolp arg))
                          (if (memq arg lyskom-commands)
			      (lyskom-command-name arg)
			    (prin1-to-string arg t)))
                         (t (format "(%S)" arg)))))

     ;;
     ;;  Format a sexp by princing it. Sort of.
     ;;
     ((= format-letter ?S)
      (setq result (format "%S" arg)))

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
              (lyskom-format (cond ((= format-letter ?P)
                                    (or lyskom-default-pers-string
                                        'person-is-anonymous))
                                   ((= format-letter ?M)
                                    (or lyskom-default-conf-string
                                        'conference-does-not-exist)))
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
                  (let ((aux (conf-stat-find-aux arg
                                                 10 
                                                 lyskom-pers-no))
                        (face (conf-stat-find-aux arg
                                                  9)))
                    (lyskom-maybe-add-face-to-string
                     face
                     (if aux
                         (concat (aux-item->data (car aux)) " *")
                      (conf-stat->name arg))))
                  )))
	     
             ;; Find the name and return it
             ((integerp arg)
              (let ((conf-stat (blocking-do 'get-conf-stat arg)))
                (if (null conf-stat)
                    (lyskom-format (if (= format-letter ?P)
                                       'person-does-not-exist
                                     'conference-does-not-exist)
                                   arg)
                  (let ((aux (conf-stat-find-aux conf-stat 10 lyskom-pers-no)))
                    (if aux
                        (concat (aux-item->data (car aux)) " *")
                      (conf-stat->name conf-stat))))))
	     
             ;; We got a conf-stat, and can use it directly
             ((lyskom-conf-stat-p arg)
                  (let ((aux (conf-stat-find-aux arg 10 lyskom-pers-no)))
                    (if aux
                        (concat (aux-item->data (car aux)) " *")
                      (conf-stat->name arg))))

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
                  ((and (consp arg)
                        (lyskom-text-stat-p (car arg)))
                   (lyskom-format-text-body (cdr arg)
                                            (car arg)))
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

(lyskom-external-function w3-fetch)
(lyskom-external-function w3-region)

(defun lyskom-format-text-body (text &optional text-stat)
  "Format a text for insertion. Does parsing of special markers in the text."
  (let* ((ct-item (car (text-stat-find-aux text-stat 1)))
         (content-type (cond (ct-item (aux-item->data ct-item))
                             ((and (string-match "\\`\\(\\S-+\\):\\s-*$" text)
                                   (match-beginning 1))
                              (match-string 1 text))
                             (t nil)))
         (fn (and content-type
                  (cdr 
                   (let ((tmp lyskom-format-special)
                         (result nil)
                         (case-fold-search t))
                     (while tmp
                       (when (eq 0 (string-match 
                                    (if (stringp (car (car tmp)))
                                        (car (car tmp))
                                      (symbol-name (car (car tmp))))
                                    content-type))
                         (setq result (car tmp))
                         (setq tmp nil))
                       (setq tmp (cdr tmp)))
                     result))))
         (formatted (and fn (funcall fn text))))

;;    (when (eq t lyskom-last-text-format-flags)
;;      (lyskom-signal-reformatted-text
;;       (if content-type
;;           (lyskom-format 'reformat-generic content-type)
;;         nil)))

    (cond (formatted formatted)
          (t (let ((tmp (if kom-text-properties
                            (lyskom-button-transform-text
                             (lyskom-fill-message text))
                          (lyskom-fill-message text))))
               (when (and kom-smileys
                          (fboundp 'smiley-region))
                 (add-text-properties 0 
                                      (length tmp) 
                                      '(special-insert lyskom-postprocess-text)
                                      tmp))
               tmp)))))

(defun lyskom-postprocess-text (start end)
  (condition-case nil
      (smiley-region start (min (point-max) (1+ end)))
    (error nil)))




(defun lyskom-signal-reformatted-text (how)
  "Signal that the last text was reformatted HOW, which should be a string
in lyskom-messages."
  (or (memq how lyskom-last-text-format-flags)
      (setq lyskom-last-text-format-flags (cons how lyskom-last-text-format-flags))))


(defun lyskom-w3-region (start end)
  (unwind-protect
    (condition-case nil
      (progn
        (narrow-to-region start end)
        (when kom-w3-simplify-body
          (save-excursion
            (let ((case-fold-search t))
              (goto-char start)
              (while (re-search-forward "<body[^>]*>" end t)
                (replace-match "<body>")))))
        (w3-region start end)
        (w3-finish-drawing)
        (add-text-properties (point-min) (point-max) '(end-closed nil)))
      (error nil))))


(defun lyskom-format-html (text)
  (when (condition-case e (progn (require 'w3) t) (error nil))
    (add-text-properties 0 (length text) '(special-insert lyskom-w3-region) text)
    (lyskom-signal-reformatted-text 'reformat-html)
    (substring text 5)))

;;;(defun lyskom-format-html (text)
;;;  (condition-case e (require 'w3) (error nil))
;;;  (let ((tmpbuf (lyskom-get-buffer-create 'lyskom-html " lyskom-html" t)))
;;;    (unwind-protect
;;;        (save-excursion
;;;          (set-buffer tmpbuf)
;;;          (insert (substring text 5))
;;;          (insert " ")                  ; So we can adjust the extents
;;;          (w3-region (point-max) (point-min))
;;;          (let ((tmp nil))
;;;            (map-extents
;;;             (lambda (e x)
;;;               (if (zerop (- (extent-start-position e)
;;;                             (extent-end-position e)))
;;;                   (set-extent-endpoints e (extent-start-position e)
;;;                                         (1+ (extent-end-position e))))
;;;               (progn
;;;                 (set-extent-property e 'duplicable t)
;;;                 (set-extent-property e 'replicable t))
;;;               nil))
;;;            (setq tmp (buffer-string))
;;;            (add-text-properties 0 (length tmp) '(end-closed nil) tmp)
;;;            tmp)))))



(defun lyskom-format-enriched (text)
  (if (not (fboundp 'format-decode-buffer))
      nil
    (let ((tmpbuf (lyskom-generate-new-buffer "lyskom-enriched")))
      (unwind-protect
          (save-excursion
            (set-buffer tmpbuf)
            (insert (substring text 10))
            (format-decode-buffer)
            (lyskom-signal-reformatted-text 'reformat-enriched)
            (lyskom-button-transform-text (buffer-string))
            ;; (substring (buffer-string) 0 -1) ; Remove the \n
            )
        (kill-buffer tmpbuf)))))

(defun lyskom-format-ö (text)
  (cond ((string= text "") "")
        (t
         (save-excursion
           (set-buffer (lyskom-get-buffer-create 'lyskom-text 
                                                 " lyskom-text" t))
           (erase-buffer)
           (insert text)
           (goto-char (point-min))
           (while (not (looking-at "\\'"))
             (save-restriction
               (narrow-to-region (point)
                                 (save-excursion (end-of-line) (point)))
               (lyskom-fill-region (point-min) (point-max))
               (goto-char (point-max)))
             (forward-line 1))
           (lyskom-signal-reformatted-text 'reformat-filled)
           (if kom-text-properties
               (lyskom-button-transform-text (buffer-string))
             (buffer-substring (point-min) (1- (point-max))))))))



;;; ============================================================
;;; lyskom-fill-message
;;; Author: David Byers
;;;
;;; Wrap the lines of a message with long lines so they're a little easier
;;; to read. Try to ignore what looks like preformatted text. 
;;;
;;; Scan the text line by line, and decide whether to fill or not on a 
;;; paragraph by paragraph basis. 
;;;
;;; An empty line ends the current paragraph.
;;; 
;;; An indented line followed by an unindented line ends the current
;;; paragraph and starts a new one. An indented line followed by the
;;; end of the buffer is also considered a paragraph if we have
;;; started a new paragraph based on indentation at least once before.
;;;
;;; A line that starts with a minus or plus starts a new paragraph.
;;; 
;;; An indented line followed by another line indented the same way
;;; starts a new paragraph if we're not already scanning a paragraph.
;;;
;;; Any text seen when not scanning a paragraph starts a new
;;; paragraph.
;;;
;;; When a paragraph is started, filling may be enabled or disabled or 
;;; set in a "maybe" state.
;;; 
;;; A line that does not look like it belongs to the current paragraph 
;;; because it is inndented incorrectly or because it starts with a
;;; strange character disables filling for the entire paragraph.
;;;
;;; A line that is wider than the window enables filling for the
;;; paragraph, unless filling has been disabled earlier.
;;;
;;; Any line containing three whitespace characters in a row, a space
;;; followed by a tab, or a tab followed by a space, or two tabs in a
;;; row, or the beginning or end of a C comment or four hyphens
;;; disables filling for the entire paragraph.
;;;
;;; A paragraph is not filled if filling has been disabled, or if the
;;; difference in line lengths from paragraph to paragraph is
;;; constant.
;;;

(defconst lyskom-minimum-triagle-size 3
  "Minimum number of lines in a triangle or suchlike.")

(defconst lyskom-minimum-brick-size 2
  "Minimum number of lines in a brick.")

(defun lyskom-fill-message-initial-wrap (current-line-length pos)
  (cond ((not (aref lyskom-line-start-chars 
                    (char-to-int 
                     (char-after pos))))
         nil)
        ((> current-line-length fill-column)
         t)
        (t 'maybe)))

(defsubst lyskom-fill-message-colon-line ()
  "Return non-nil if the current line starts with a colon-like thing."
  (save-match-data
    (looking-at "\\S-+\\s-*:")))



(defun lyskom-fill-region (start end &optional justify nosqueeze to-eop)
  "Fill a region of text, compensating for bugs in Emacs."
  (save-match-data
    (let ((fill-column (if nosqueeze (1- fill-column) fill-column)))
      (when nosqueeze
        (condition-case nil
            (save-excursion
              (goto-char (match-beginning 0))
              (backward-char 1)
              (delete-horizontal-space))
          (error nil)))
      (fill-region start (min end (point-max)) justify nosqueeze to-eop))))

(defun lyskom-fill-message (text)
  "Try to reformat a message."
  (cond 
   ((null kom-autowrap) text)
   ((and (numberp kom-autowrap) (> (length text) kom-autowrap)) text)
   (t
    (save-excursion
      (set-buffer (lyskom-get-buffer-create 'lyskom-text " lyskom-text" t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min))

      (let ((start (point))
            (in-paragraph nil)
            (wrap-paragraph 'maybe)
            (length-difference nil)
            (constant-length nil)
            (all-lines-colons t)
            (current-line-length nil)
            (last-line-length nil)
            (paragraph-length 0)
            (eol-point nil)
            (have-indented-paragraphs nil)
            (fill-column 
             (cond ((not (integerp fill-column)) (- (window-width) 5))
                   ((> fill-column (- (window-width) 5)) (- (window-width) 5))
                   (t fill-column)))
            (fill-prefix nil)
            (single-line-regexp "\\(\\S-\\)"))

        ;;
        ;; Scan each line
        ;;

        (while (not (eobp))
          (setq current-line-length (lyskom-fill-message-line-length))

          ;;
          ;; Do some work on checking for constant differences
          ;;

          (cond ((null length-difference)
                 (when (and current-line-length last-line-length)
                   (setq length-difference (- current-line-length
                                              last-line-length))))
                ((eq constant-length 'maybe-not)
                 (setq constant-length nil))
                (constant-length
                 (unless (= (- current-line-length last-line-length)
                            length-difference)
                   (setq constant-length 'maybe-not))))

          (cond 

           ;;
           ;; An empty line signifies a new paragraph. If we were scanning
           ;; a paragraph and it was to be filled, fill it. 
           ;;

           ((looking-at "^\\s-*$")
            (when (and in-paragraph 
                       (not all-lines-colons)
                       (eq wrap-paragraph t)
                       (or (null constant-length)
                           (and (eq 0 length-difference)
                                (< paragraph-length lyskom-minimum-brick-size))
                           (and (not (eq 0 length-difference))
                                (< paragraph-length lyskom-minimum-triagle-size))))
              (lyskom-fill-region start (1- (match-beginning 0)) nil t)
              (lyskom-signal-reformatted-text 'reformat-filled))
            (setq start (match-end 0)
                  in-paragraph nil
                  all-lines-colons t
                  wrap-paragraph 'maybe))

           ;;
           ;; We're in a paragraph, but wait! This looks like 
           ;; a LysKOM text!
           ;;

           ((looking-at (concat "^" (regexp-quote (lyskom-get-string 'subject))
                                ".*\n----"))
            (setq wrap-paragraph nil))

           ;;
           ;; We're in a paragraph, but we see indentation, a dash or
           ;; something that looks like the end of a LysKOM text.
           ;; This has to mean something...
           ;;

           ((and in-paragraph
                 (looking-at "^\\s-+\\([^\n]*\\)\\(\n\\S-\\|\\'\\)")
                 (or (not (eq (point-max) (match-beginning 2)))
                     have-indented-paragraphs))
            (setq have-indented-paragraphs t)
            (when (and (eq wrap-paragraph t)
                       (not all-lines-colons)
                       (or (and (eq 0 length-difference)
                                (< paragraph-length lyskom-minimum-brick-size))
                           (and (not (eq 0 length-difference))
                                (< paragraph-length lyskom-minimum-triagle-size))
                           (null constant-length)))
              (lyskom-fill-region start (match-beginning 0) nil t)
              (lyskom-signal-reformatted-text 'reformat-filled))
            (setq start (match-beginning 0)
                  in-paragraph t
                  paragraph-length 0
                  constant-length t
                  length-difference nil
                  last-line-length nil
                  all-lines-colons (lyskom-fill-message-colon-line)
                  single-line-regexp "\\(\\S-\\)"
                  fill-prefix nil
                  start (match-beginning 0)
                  wrap-paragraph (lyskom-fill-message-initial-wrap
                                  current-line-length (match-beginning 1))))

           ((and in-paragraph
                 (looking-at "^\\s-*\\(-+\\|\\++\\)\\s-*\\S-"))
            (when (and (eq wrap-paragraph t)
                       (not all-lines-colons)
                       (or (and (eq 0 length-difference)
                                (< paragraph-length lyskom-minimum-brick-size))
                           (and (not (eq 0 length-difference))
                                (< paragraph-length lyskom-minimum-triagle-size))
                           (null constant-length)))
              (lyskom-fill-region start (match-beginning 0) nil t)
              (lyskom-signal-reformatted-text 'reformat-filled))
            (setq start (match-beginning 0)
                  in-paragraph t
                  paragraph-length 0
                  constant-length t
                  length-difference nil
                  all-lines-colons (lyskom-fill-message-colon-line)
                  last-line-length nil
                  single-line-regexp "\\(\\S-\\)"
                  fill-prefix nil
                  start (match-beginning 0)
                  wrap-paragraph (lyskom-fill-message-initial-wrap
                                  current-line-length (match-beginning 1))))


           ;;
           ;; Here's a tricky one... We're not in a paragraph, and we
           ;; see what looks like an indented paragraph. Take care with
           ;; this one!
           ;;

           ((and (not in-paragraph)
                 (looking-at "\\(\\s-+\\)\\S-")
                 (looking-at (concat "\\(\\s-+\\)[^\n]*\n"
                                     (match-string 1)
                                     "\\(\\S-\\)")))
            (setq in-paragraph t
                  paragraph-length 0
                  constant-length 0
                  length-difference nil
                  last-line-length nil
                  all-lines-colons (lyskom-fill-message-colon-line)
                  start (match-beginning 0)
                  fill-prefix (match-string 1)
                  single-line-regexp (concat (match-string 1) "\\(\\S-\\)")
                  wrap-paragraph (lyskom-fill-message-initial-wrap
                                  current-line-length (match-beginning 2))))

           ;;
           ;; Not in a paragraph, but here comes some text. Let's start
           ;; a paragraph, shall we?
           ;;

           ((and (not in-paragraph)
                 (looking-at "\\s-*\\(\\S-\\)"))
            (setq in-paragraph t
                  paragraph-length 0
                  all-lines-colons (lyskom-fill-message-colon-line)
                  constant-length t
                  length-difference nil
                  last-line-length nil
                  start (match-beginning 0)
                  fill-prefix nil
                  single-line-regexp "\\(\\S-\\)"
                  wrap-paragraph (lyskom-fill-message-initial-wrap
                                  current-line-length (match-beginning 1))))


           ;;
           ;; We're in a paragraph, but the line looks kind of strange
           ;;

           ((and in-paragraph
                 (or (not (looking-at single-line-regexp))
                     (not (aref lyskom-line-start-chars
                                (char-to-int 
                                 (char-after
                                  (match-beginning 1)))))))
            (setq wrap-paragraph nil))

           ;;
           ;; We're in a paragraph, the line looks OK, but is long. That 
           ;; means we should probably be filling the paragraph later
           ;;

           ((and in-paragraph
                 wrap-paragraph
                 (> current-line-length fill-column))
            (setq wrap-paragraph t))
           )

          ;;
          ;; Check if the line starts with Foo:
          ;;

          (when (and in-paragraph 
                     all-lines-colons
                     (not (lyskom-fill-message-colon-line)))
            (setq all-lines-colons nil))

          ;;
          ;; Certain things are guaranteed to disqualify the 
          ;; current paragraph from wrapping, no matter what.
          ;; This is where we look for those.
          ;;

          (when (and in-paragraph
                     wrap-paragraph)
            (setq eol-point (save-excursion (end-of-line) (point)))
            (when (re-search-forward "\
\\(\\S-[ \t][ \t][ \t]+\\S-\
\\|\\S-[ \t]* \t[ \t]*\\S-\
\\|[ \t]*\t [ \t]*\
\\|\\S-\\s-*\t\t\\s-*\\S-\
\\|----\
\\|/\\*\
\\|\\*/\
\\|[^:]//\
\\)"
                       eol-point t)
              (setq wrap-paragraph nil)))


          (setq last-line-length current-line-length)
          (end-of-line)
          (setq paragraph-length (1+ paragraph-length))
          (unless (eobp)
            (forward-line 1)
            (beginning-of-line)))

        ;;
        ;; We've seen the end of buffer. Fill any unfilled junk.
        ;;

        (when (and in-paragraph 
                   (not all-lines-colons)
                   (eq wrap-paragraph t)
                   (or (and (eq 0 length-difference)
                            (< paragraph-length lyskom-minimum-brick-size))
                       (and (not (eq 0 length-difference))
                            (< paragraph-length lyskom-minimum-triagle-size))
                       (not (eq constant-length t))))
          (lyskom-fill-region start (point) nil t)
          (lyskom-signal-reformatted-text 'reformat-filled)))
      
      ;;
      ;; Kill off unwanted whitespace at the end of the message
      ;;

      (let ((tmp (buffer-string)))
        (if (string-match "[ \t\n]+\\'" tmp)
            (substring tmp 0 (match-beginning 0))
          tmp))))))

(defun lyskom-fill-message-line-length ()
  (- (save-excursion (end-of-line)
                     (skip-chars-backward " \t")
                     (point)) (point)))


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
                         (sit-for kom-ding-pause-amount)
                         (setq arg (1- arg))))
        ((stringp arg)
         (condition-case nil
             (start-process "audio" nil kom-audio-player arg)
           (error nil)))
        ((and (symbolp arg)
              (fboundp arg))
         (condition-case nil
             (funcall arg)
           (error (message "Error in beep function")
                  (beep))))
        (t (beep))))

(defun lyskom-face-default-p (f1)
  "Return t if f1 is undefined or the default face."
  (lyskom-xemacs-or-gnu (or (not (find-face f1))
                            (face-equal (find-face f1)
                                        (find-face 'default)))
                        (or (not (facep f1))
                            (face-equal f1 'default))))
		  

;;;; ================================================================
;;;;                         Running in buffer 

;;; Author: Linus


(defun backward-text (&optional arg)
  "Searches backwards for a text start and recenters with that text at the top."
  (interactive "p")
  (let ((paragraph-start lyskom-text-start)
        (paragraph-ignore-fill-prefix t))
    (backward-paragraph arg))
  (beginning-of-line))


(defun forward-text (&optional arg)
  "Searches forward for a text start and recenters with that text at the top."
  (interactive "p")
  (let ((paragraph-start lyskom-text-start)
        (paragraph-ignore-fill-prefix t))
    (forward-paragraph arg)))


(defun kom-save-text (&optional arg)
  "Saves/appends the article before point to a file.
The article is determined by a search-backward the same as backward-text 
and then a forward-text.
With an argument ARG the search is done over that number of texts.
The name of the file is read using the minibuffer and the default is kom-text."
  (interactive "p")
  (let ((buf (lyskom-get-buffer-create 'temp " *kom*-text"))
        (lyskom-print-complex-dates nil)
        (list-of-texts nil)
        (kom-deferred-printing nil)
        (name nil))
    (unwind-protect
        (save-excursion
          (while (> arg 0)
            (backward-text 1)
            (if (looking-at "\\([0-9]+\\)\\s-")
                (setq list-of-texts (cons (string-to-int (match-string 1))
                                          list-of-texts)
                      arg (1- arg))
              (setq arg 0)))
          (set-buffer buf)
          (mapcar (function
                   (lambda (n)
                     (lyskom-view-text n)
                     (goto-char (point-max))
                     (insert "\n")))
                  list-of-texts)
          (setq name (read-file-name
                      (lyskom-format 'save-on-file-q
                                     (file-name-nondirectory
                                      lyskom-saved-file-name))
                      (file-name-directory lyskom-saved-file-name)
                      lyskom-saved-file-name
                      nil))
          (when (file-directory-p name)
            (setq name (concat (file-name-as-directory name)
                               (file-name-nondirectory 
                                lyskom-saved-file-name))))
          (append-to-file (point-min) (point-max) (expand-file-name name))
          (setq lyskom-saved-file-name name))
;      (kill-buffer buf)
)))



;;; ================================================================
;;;                Some useful abstractions

(defsubst lyskom-membership-highest-index ()
  "Return the number of conferences the user is a member of minus 1.
This is the highest index in lyskom-membership that contains data, if
positions are counted from 0, as they are."
  (1- (length lyskom-membership)))


;;; ================================================================
;;;                             To-do


(defun lyskom-update-prompt (&optional force-prompt-update)
  "Print prompt if the client knows which command will be default.
Set lyskom-current-prompt accordingly. Tell server what I am doing."
  (if (or lyskom-executing-command
	  (and lyskom-current-prompt
	       lyskom-dont-change-prompt))
      nil
    (let ((to-do (lyskom-what-to-do))
	  (prompt nil))
      (setq lyskom-command-to-do to-do)
      (cond

       ((eq to-do 'next-pri-conf)
	(setq prompt 'go-to-pri-conf-prompt)
	(or (eq lyskom-current-prompt prompt)
	    (lyskom-beep kom-ding-on-priority-break)))

       ((eq to-do 'next-pri-text)
	(setq prompt 'read-pri-text-conf)
	(or (eq lyskom-current-prompt prompt)
	    (lyskom-beep kom-ding-on-priority-break)))

       ((eq to-do 'next-text)
	(setq prompt
	      (let ((read-info (read-list->first lyskom-reading-list)))
		(cond
		 ((eq 'REVIEW (read-info->type read-info))
		  'review-next-text-prompt)
		 ((eq 'REVIEW-TREE (read-info->type read-info))
		  'review-next-comment-prompt)
		 ((eq 'REVIEW-MARK (read-info->type read-info))
		  'review-next-marked-prompt)
		 ;; The following is not really correct. The text to be
		 ;; read might be in another conference.
		 ((= lyskom-current-conf lyskom-pers-no)
		  'read-next-letter-prompt)
		 ((eq 'FOOTN-IN (read-info->type read-info))
		  'read-next-footnote-prompt)
		 ((eq 'COMM-IN (read-info->type read-info))
		  'read-next-comment-prompt)
		 (t
		  'read-next-text-prompt)))))

       ((eq to-do 'next-conf)
	(setq prompt
	      (cond
	       ((eq 'REVIEW-MARK 
		    (read-info->type (read-list->first lyskom-to-do-list)))
		'go-to-conf-of-marked-prompt)
	       ((/= lyskom-pers-no
		    (conf-stat->conf-no
		     (read-info->conf-stat (read-list->first
					    lyskom-to-do-list))))
		'go-to-next-conf-prompt)
	       (t
		'go-to-your-mailbox-prompt))))
    
       ((eq to-do 'when-done)
	(if (not lyskom-is-writing)
	    (lyskom-tell-server kom-mercial))
	(setq prompt
	      (let ((command (lyskom-what-to-do-when-done t)))
		(cond			    
		 ((lyskom-command-name command))
		 ((and (stringp command)
		       (lyskom-command-name (key-binding command))))
		 (t (lyskom-format 'the-command command))))))
     
       ((eq to-do 'unknown)		;Pending replies from server.
	(setq prompt nil))
     
       (t (signal 'lyskom-internal-error '(lyskom-update-prompt))))

      (when (or force-prompt-update
                (not (equal prompt lyskom-current-prompt)))
	  (let ((inhibit-read-only t)
		(prompt-text
		 (if prompt
                     (lyskom-modify-prompt
                      (cond
                       ((symbolp prompt) (lyskom-get-string prompt))
                       (t prompt)))
		   ""))
		(was-at-max (eq (point) (point-max))))
	    (save-excursion
	      ;; Insert the new prompt
	      (goto-char (point-max))
	      (beginning-of-line)
              (when lyskom-slow-mode
                (add-text-properties 0 (length prompt-text)
                                     '(read-only t) prompt-text))
	      (insert-string prompt-text)
	      ;; Delete the old prompt
	      (if lyskom-current-prompt
		  (delete-region (point) (point-max))))
	    (if was-at-max (goto-char (point-max)))
	  
	    (setq lyskom-current-prompt prompt)
            (setq lyskom-current-prompt-text prompt-text))))
    (lyskom-set-mode-line)))

(defun lyskom-modify-prompt (s &optional executing)
  (lyskom-format-prompt (cond (lyskom-is-administrator
                               (if executing 
                                   kom-enabled-prompt-format-executing
                                 kom-enabled-prompt-format))
                              (lyskom-is-anonymous
                               (if executing
                                   kom-anonymous-prompt-format-executing
                                 kom-anonymous-prompt-format))
                              (t (if executing
                                     kom-user-prompt-format-executing
                                   kom-user-prompt-format)))
                        s))
           


;(defun lyskom-modify-prompt (s)
;  "Modify the LysKOM prompt to reflect the current state of LysKOM."
;  (let ((format-string (or kom-prompt-format "%s")))
;    (if (symbolp s) (setq s (lyskom-get-string s)))
;    (if lyskom-ansaphone-messages
;        (if (> (length lyskom-ansaphone-messages) 0)
;            (setq format-string 
;                  (format (lyskom-get-string 'prompt-modifier-messages)
;                          format-string
;                          (length lyskom-ansaphone-messages)))))
;    (if kom-ansaphone-on
;        (setq format-string
;              (format (lyskom-get-string 'prompt-modifier-ansaphone)
;                      format-string)))

;    (format format-string s)))

(defun lyskom-format-prompt (fmt command)
  (let ((start 0)
        (len (length fmt))
        (result nil)
        (tmp nil)
        (format-letter nil)
        (messages (length lyskom-ansaphone-messages)))
    (while (< start len)
      (setq tmp (string-match "%[][cm Sswp#]" fmt start))
      (if tmp
          (progn
            (if (> tmp start)
                (setq result (cons (substring fmt start tmp) result)))
            (setq format-letter (elt fmt (1- (match-end 0))))
            (setq start (match-end 0))
            (setq result 
                  (cons
                   (cond ((eq format-letter ?\[) (if kom-ansaphone-on "[" ""))
                         ((eq format-letter ?\]) (if kom-ansaphone-on "]" ""))
                         ((eq format-letter ?c)  command)
                         ((eq format-letter ?w) 
                          (or (conf-stat->name 
                               (cache-get-conf-stat lyskom-current-conf))
                              (lyskom-format 'conference-no
                                             lyskom-current-conf)))
                         ((eq format-letter ?S) lyskom-server-name)
                         ((eq format-letter ?s)
                          (or (cdr (assoc lyskom-server-name 
                                          kom-server-aliases))
                              lyskom-server-name))
                         ((eq format-letter ?p)
                          (or (conf-stat->name
                               (cache-get-conf-stat lyskom-pers-no))
                              (lyskom-format 'person-no
                                             lyskom-pers-no)))

                         ((eq format-letter ?#) (number-to-string 
                                                lyskom-session-no))
                         ((eq format-letter ?a)
                          (lyskom-get-string 'anonymous))

                         ((eq format-letter ?A)
                          (lyskom-get-string 'Anonymous))

                         ((eq format-letter ?m)
                          (cond ((< messages 1)
                                 "")
                                ((= messages 1)
                                 (format (lyskom-get-string 
                                          'prompt-single-message) messages))
                                ((> messages 1)
                                 (format (lyskom-get-string
                                          'prompt-several-messages) 
                                         messages))))
                         ((eq format-letter ?%) "%")
                         ((eq format-letter ?\ )
                          'SPC))
                   result)))
        (progn
          (setq result (cons (substring fmt start) result))
          (setq start len))))
    (lyskom-build-prompt (nreverse result))))


(defun lyskom-build-prompt (data)
  (let ((result "")
        (separate nil))
    (while data
      (cond ((stringp (car data))
             (cond ((and separate
                         (string-match "\\S-$" result)
                         (string-match "^\\S-" (car data)))
                    (setq result (concat result " " (car data))))
                   (t (setq result (concat result (car data)))))
             (setq separate nil))

            ((eq (car data) 'SPC)
             (setq separate t)))
      (setq data (cdr data)))
    result))

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
    (if (> (text-list->length (read-info->text-list
			       (read-list->first lyskom-to-do-list)))
	   1)
       'next-pri-conf
      'next-pri-text))
   ((not (read-list-isempty lyskom-reading-list))
    'next-text)
   ((not (read-list-isempty lyskom-to-do-list))
    'next-conf)
   ;; This is not really true. The prefetch may still be fetching the
   ;; membership. One possible way is to test for a non-numeric,
   ;; non-nil value. Or even better, introduce a test function to
   ;; isolate the test.
   (lyskom-membership-is-read
    'when-done)
   (t 'unknown)))


(defun lyskom-what-to-do-when-done (&optional nochange)
  "Returns a command, the next command to do from the kom-do-when-done.
If optional argument NOCHANGE is non-nil then the list wont be altered."
  (condition-case nil
      (let* ((now (cdr lyskom-do-when-done))
	     (all (car lyskom-do-when-done))
	     (next (cond
		    ((and now (eq now all) (cdr all))
		     (cdr all))
		    (t all)))
	     (command
	      (cond
	       ((commandp (car now)) (car now))
	       ((and (car next)
                     (listp (car next))
		     (not (eq (car (car next))
			      'lambda)))
		(car (setq now (car next))))
	       (t (or (car (setq now next))
                      'kom-display-time)))))
	(if nochange
	    nil
	  (setq lyskom-do-when-done (cons next (cdr now))))
	command)
    (error
     (lyskom-insert-before-prompt (lyskom-get-string
				   'error-in-kom-do-when-done))
     (lyskom-beep t)
     (setq lyskom-do-when-done '((kom-customize kom-display-time)
				 . (kom-edit-options kom-display-time)))
     'kom-display-time)))


(defun lyskom-prefetch-and-print-prompt ()
  "Prefetch info if needed. Print prompt if not already printed."
  ;; (if (< (lyskom-known-texts)
  ;; 	    lyskom-prefetch-conf-tresh)
  ;; 	 (lyskom-prefetch-conf))
  ;; (lyskom-prefetch-text)
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
  (lyskom-update-prompt))


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

;; (defun lyskom-prefetch-all-confs ()
;;   "Gets all conferences using prefetch."
;;   (while (not (lyskom-prefetch-done))
;;     (let ((lyskom-prefetch-conf-tresh lyskom-max-int)
;; 	     (lyskom-prefetch-confs lyskom-max-int))
;; 	 (lyskom-prefetch-conf))
;;     (accept-process-output nil lyskom-apo-timeout-s lyskom-apo-timeout-ms)))

;; +++PREFETCH


(defun lyskom-wait-for-membership ()
  "Give a message and wait for it to be prefetched.
If the full membership hase been read do nothing."
  (let ((total (pers-stat->no-of-confs (blocking-do 'get-pers-stat
						    lyskom-pers-no))))
    (while (and (not (lyskom-membership-is-read))
                (numberp lyskom-membership-is-read))
      (lyskom-message (lyskom-get-string 'waiting-for-membership)
		      lyskom-membership-is-read
		      total)
      (sit-for 0)
      (accept-process-output lyskom-proc 1))))


(defun lyskom-prefetch-all-confs ()
  "Gets all conferences using prefetch."
  (lyskom-wait-for-membership))



;; (defun lyskom-list-unread (map membership)
;;   "Args: MAP MEMBERSHIP. Return a list of unread texts.
;; The list consists of text-nos."
;;   (let ((res nil)
;; 	   (last-read (membership->last-text-read membership))
;; 	   (read (membership->read-texts membership))
;; 	   (first (map->first-local map))
;; 	   (i (length (map->text-nos map)))
;; 	   (the-map (map->text-nos map)))
;;     (while (> i 0)
;; 	 (-- i)
;; 	 (cond
;; 	  ((zerop (elt the-map i)))	;Deleted text - do nothing.
;; 	  ((<= (+ first i) last-read))	;Already read - do nothing.
;; 	  ((lyskom-vmemq  (+ i first) read)) ;Already read - do nothing.
;; 	  (t				;Unread - add to list.
;; 	   (setq res (cons
;; 		      (elt the-map i)
;; 		      res)))))
;;     res))

;; (defun lyskom-list-unread (map membership)
;;   "Args: MAP MEMBERSHIP. Return a list of unread texts.
;; The list consists of text-nos."
;;   (let ((read (membership->read-texts membership))
;; 	   (first (map->first-local map))
;; 	   (i (length (map->text-nos map)))
;; 	   (the-map (map->text-nos map)))
;;     (while (> i 0)
;; 	 (-- i)
;; 	 ;; The server always send the read texts in sorted order. This
;; 	 ;; means that we can use binary search to look for read texts.
;; 	 (when (lyskom-vmemq  (+ i first) read)
;; 	   (aset the-map i 0)))
;;     (delq 0 (listify-vector the-map))))

;; (defun lyskom-list-unread (map membership)
;;   "Args: MAP MEMBERSHIP. Return a list of unread texts.
;; The list consists of text-nos."
;;   (let ((the-map (map->text-nos map)))
;;     (delq 0 (listify-vector the-map))))

(defun lyskom-list-unread (map membership)
  "Args: MAP MEMBERSHIP. Return a list of unread texts.
The list consists of text-nos."
  (let ((read (membership->read-texts membership))
	(first (map->first-local map))
	(i (length (map->text-nos map)))
	(the-map (map->text-nos map)))
    (when (not (null read))
      (while (> i 0)
	(-- i)
	;; The server always send the read texts in sorted order. This
	;; means that we can use binary search to look for read texts.

	;; It might be a good idea to check for zero, and not do a
	;; sarch in that case, but it depends on how big holes there
	;; are in the map. In general the extra test is probably a
	;; slowdow, but when reading the initial part of the I]M map
	;; it would most likely help a lot.
	(when (lyskom-binsearch  (+ i first) read)
	  (aset the-map i 0))))
    (delq 0 (listify-vector the-map))))



;; (defun lyskom-conf-fetched-p (conf-no)
;;   "Return t if CONF-NO has been prefetched."
;;   (let ((n lyskom-last-conf-received)
;; 	   (result nil))
;;     (while (and (not result)
;; 		   (>= n 0))
;; 	 (if (= (membership->conf-no (elt lyskom-membership n))
;; 		conf-no)
;; 	     (setq result t))
;; 	 (-- n))
;;     result))


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

      
(defun lyskom-binsearch (num vector &optional first last+1)
  "Return the index if NUM is a member of (present in) VECTOR.
VECTOR has to be sorted with regard to <."
  (lyskom-binsearch-internal num vector
			     (or first 0)
			     (or last+1 (length vector))))

(defun lyskom-binsearch-internal (num vector first last+1)
  "Return the index if ELT is a member of the sorted vector VECTOR."
  (let* ((split (/ (+ first last+1) 2))
	 (splitval (aref vector split)))
    (cond
     ;; Only one element
     ((= (- last+1 first) 1) (if (= num splitval) split nil))
     ;; This is not really necessary, but it _might_ speed it up..
     ((= num splitval) split)
     ;; Search the left subtree
     ((< num splitval)
      (lyskom-binsearch-internal num vector first split))
     ;; Search the left subtree
     (t (lyskom-binsearch-internal num vector split last+1)))))

      
(defun lyskom-read-num-range (low high &optional prompt show-range default history nildefault)
  "Read a number from the minibuffer.
Args: LOW HIGH &optional PROMPT SHOW-RANGE with default value DEFAULT.
The read number must be within the range [LOW HIGH].
If SHOW-RANGE is non-nil, the prompt will include the range for information
to the user.
HISTORY is the history list to use.
If NILDEFAULT is non-nil, return nil if the user enters an empty string"
  (let ((number (1- low)))
    (while (and (not (and nildefault
                          (null number)))
                (or (< number low)
                    (> number high)))
      (setq number (lyskom-read-number 
		    (concat (if prompt
				prompt
			      (lyskom-get-string 'give-a-number))
			    (if show-range
				(format "(%d-%d) " low high)
			      ""))
		    default
                    history
                    nildefault)))
    number))


(defun lyskom-read-number (&optional prompt default history nildefault)
  "Read a number from the minibuffer. Optional arguments: PROMPT DEFAULT
If DEFAULT is non-nil, it is written within parenthesis after the prompt.
DEFAULT could also be of the type which (interactive P) generates.
If NILDEFAULT is non-null then typing return will cause the function 
to return nil."
  (let ((numdefault (cond ((null default) nil)
			  ((integerp default) default)
			  ((listp default) (car default))
			  (t nil)))
	(number nil)
	(numstr nil)
        (done nil))
    (while (not done)
      (setq numstr
	    (prog1
		(lyskom-read-string
		 (concat (if prompt 
			     prompt
			   (lyskom-get-string 'give-a-number))
			 (if numdefault 
			     (format " (%d) " numdefault)
			   " "))
                 nil
                 history)))
      (cond ((and (string= numstr "") numdefault)
	     (setq number numdefault done t))
            ((and (string= numstr "") nildefault)
             (setq number nil done t))
	    ((string-match "\\`[0-9]+\\'" numstr)
	     (setq number (string-to-int numstr))
             (setq done number))
	    (t (beep))))
    number))


(defun lyskom-read-string (prompt &optional initial history)
  "Read a string from the minibuffer. Arguments: PROMPT INITIAL"
  (read-string prompt
	       initial
               history))



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
				       initial-input t))
      (setq nagging t))
    (not (string-match (lyskom-get-string 'no-regexp) answer))))


;;;
;;; j-or-n-p is similar to y-or-n-p. If optional argument QUITTABLE is
;;; non-nil C-g will abort. 
;;;

(defun j-or-n-p (prompt &optional quittable)
  "Same as y-or-n-p but language-dependent.
Uses lyskom-message, lyskom-read-string to do interaction and
lyskom-get-string to retrieve regexps for answer and string for repeated query."
  (let ((input-char 0)
	(cursor-in-echo-area t)
	(nagging nil))
    (while (and (not (char-in-string input-char
                                     (lyskom-get-string 'y-or-n-instring)))
                (not (and (or (eq input-char ?\C-g)
                              (eq 'keyboard-quit
                                  (lyskom-lookup-key (current-local-map)
                                                     input-char
                                                     t)))
                          quittable)))
	(lyskom-message "%s" (concat (if nagging 
					 (lyskom-get-string 'j-or-n-nag)
				       "") 
				     prompt
				     (lyskom-get-string 'j-or-n)))
	(if nagging (beep))

        ;;
        ;; Workaround for Emacs whose read-char does not accept C-g
        ;;

	(setq input-char 
              (let ((inhibit-quit t))
                (prog1 (read-char)
                  (setq quit-flag nil))))

        ;;
        ;; Redisplay prompt on C-l
        ;;

        (if (or (eq input-char ?\C-l)
                (eq 'recenter (lyskom-lookup-key (current-local-map)
                                                 input-char
                                                 t)))
            (setq nagging nil)
          (setq nagging t)))

    (if (and quittable (eq input-char ?\C-g)) (keyboard-quit))
    (char-in-string input-char (lyskom-get-string 'y-instring))))

  
;;; lyskom-j-or-n-p, lyskom-ja-or-no-p
;;; These versions no longer perform lyskom-end-of-command
;; Author: Linus Tolke

(defun lyskom-j-or-n-p (prompt &optional quittable)
  "Same as j-or-n-p but performs lyskom-end-of-command if quit."
  (condition-case nil
      (j-or-n-p prompt quittable)
    (quit (signal 'quit "In lyskom-j-or-n-p"))))


(defun lyskom-ja-or-nej-p (prompt &optional initial-input)
  "Same as ja-or-nej-p but performs lyskom-end-of-command if quit."
  (condition-case nil
      (ja-or-nej-p prompt initial-input)
    (quit (signal 'quit "In lyskom-ja-or-nej-p"))))



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
;  (sit-for 0)				; Why? [Doesn't work in XEmacs 19.14]
;  (setq lyskom-apo-timeout-log
;        (cons lyskom-apo-timeout lyskom-apo-timeout-log))
  (lyskom-reset-apo-timeout)            ; Reset accept-process-output timeout
  (let ((old-match-data (match-data))
	;; lyskom-filter-old-buffer is also changed when starting to edit
	;; in function lyskom-edit-text.
	(lyskom-filter-old-buffer (current-buffer)))
    (unwind-protect
	(condition-case nil
	    (progn
	      (setq lyskom-quit-flag nil)
	      
              (if lyskom-debug-communications-to-buffer
                  (save-excursion
                    (set-buffer (get-buffer-create "*kom*-replies"))
                    (goto-char (point-max))
                    (princ output (current-buffer))))

	      (if lyskom-debug-communications-to-buffer
                  (if (not lyskom-debug-what-i-am-doing)
                      (if (not (and (eq ?: (elt output 0))
                                    (eq ?5 (elt output 1))))
                          (lyskom-debug-insert proc "From " output))
                    (lyskom-debug-insert proc "From " output)))
	      
	      (set-buffer (process-buffer proc))
	      (princ output lyskom-unparsed-marker)
	      ;;+++lyskom-string-skip-whitespace
	      (if quit-flag		; We are allowed to break here.
		  (setq inhibit-quit nil)) ; This will break
					   ; instantly.

	      ;; Keep inhibit-quit set to t
	      (cond

	       ((and (> lyskom-string-bytes-missing 0)
		     (< (length output) lyskom-string-bytes-missing))
		(setq lyskom-string-bytes-missing
		      (- lyskom-string-bytes-missing
			 (length output))))

	       ;; This test makes e.g. startup a lot faster. At least
	       ;; it does when the maps are read in one chunk, which
	       ;; they usually aren't anymore.
	       ((not (string-match "\n" output)))
	       
	       ((null lyskom-is-parsing) ;Parse one reply at a time.
		(let ((lyskom-is-parsing t))
		  (unwind-protect
		      (condition-case nil
			  (lyskom-parse-unparsed)
			;; Incomplete answers are normal.
			(lyskom-parse-incomplete))
		    ;; In case it was changed by the handler.
		    (set-buffer (process-buffer proc)))))))
	  ;; condition-case handler
	  (quit (setq lyskom-quit-flag t))
	  ;; (lyskom-protocol-error
	  ;;   (lyskom-message "%s" (lyskom-get-string 'protocol-error) err))
	  )

      (setq lyskom-quit-flag (or lyskom-quit-flag quit-flag))
      (setq quit-flag nil)
      ;; Restore selected buffer and match data.
      (store-match-data old-match-data)
      (when (buffer-live-p lyskom-filter-old-buffer)
        (set-buffer lyskom-filter-old-buffer)))
    (sit-for 0)))
      

;;; The sentinel

(defun lyskom-sentinel (proc sentinel)
  "Handles changes in the lyskom-process."
  (setq lyskom-sessions-with-unread
	(delq proc lyskom-sessions-with-unread))
  (set-buffer (process-buffer proc))
  (lyskom-start-of-command (lyskom-get-string 'process-signal) t)
  (lyskom-format-insert 'closed-connection sentinel (current-time-string))
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
                        prefix
			(format "%s" proc)
                        ": "
                        string))
	      (if move (goto-char (point-max))))))
      (save-excursion
	(set-buffer buf)
	(goto-char (point-max))
	(insert "\n"
                prefix
		(format "%s" proc)
		": "  string)))))


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
  (concat " " (mapconcat 'lyskom-format-object args " ")))


(defun lyskom-format-object (object)
  (cond
   ((stringp object) (lyskom-prot-a-format-string object))
   ((integerp object) (int-to-string object))
   ((null object) "0")
   ((listp object)
    (cond
     ((eq (car object) 'MISC-LIST)
      (lyskom-prot-a-format-misc-list (cdr object)))
     ((eq (car object) 'AUX-ITEM)
      (lyskom-prot-a-format-aux-item object))
     ((eq (car object) 'AUX-ITEM-FLAGS)
      (lyskom-prot-a-format-aux-item-flags object))
     ((eq (car object) 'CONF-TYPE)
      (lyskom-prot-a-format-conf-type object))
     ((eq (car object) 'PRIVS)
      (lyskom-prot-a-format-privs object))
     ((eq (car object) 'FLAGS)
      (lyskom-prot-a-format-flags object))
     ((eq (car object) 'MEMBERSHIP-TYPE)
      (lyskom-prot-a-format-membership-type object))
     ((eq (car object) 'LIST)
      (lyskom-prot-a-format-simple-list (cdr object)))
     ((eq (car object) 'TIME)
      (lyskom-prot-a-format-time (cdr object)))
     (t
      (signal 'lyskom-internal-error
              (list 'lyskom-format-object
                    ": no support for object "
                    object)))))
   ((eq object t) "1")
   (t (signal 'lyskom-internal-error
              (list 'lyskom-format-object
                    ": no support for object "
                    object)))))


(defun lyskom-prot-a-format-time (time)
  "Format a TIME for output to the server."
  (mapconcat 'int-to-string time " "))

(defun lyskom-prot-a-format-flags (flags)
  "Format personal flags for the server."
  (concat
   (lyskom-prot-a-format-bool (flags->unread_is_secret flags))
   (lyskom-prot-a-format-bool (flags->flg2 flags))
   (lyskom-prot-a-format-bool (flags->flg3 flags))
   (lyskom-prot-a-format-bool (flags->flg4 flags))
   (lyskom-prot-a-format-bool (flags->flg5 flags))
   (lyskom-prot-a-format-bool (flags->flg6 flags))
   (lyskom-prot-a-format-bool (flags->flg7 flags))
   (lyskom-prot-a-format-bool (flags->flg8 flags))))


(defun lyskom-prot-a-format-membership-type (membership-type)
  "Format a MEMBERSHIP-TYPE for output to the server."
  (concat
   (lyskom-prot-a-format-bool (membership-type->invitation membership-type))
   (lyskom-prot-a-format-bool (membership-type->passive membership-type))
   (lyskom-prot-a-format-bool (membership-type->secret membership-type))
   (lyskom-prot-a-format-bool (membership-type->rsv1 membership-type))
   (lyskom-prot-a-format-bool (membership-type->rsv2 membership-type))
   (lyskom-prot-a-format-bool (membership-type->rsv3 membership-type))
   (lyskom-prot-a-format-bool (membership-type->rsv4 membership-type))
   (lyskom-prot-a-format-bool (membership-type->rsv5 membership-type))))

(defun lyskom-prot-a-format-conf-type (conf-type)
  "Format a CONF-TYPE for output to the server."
  (concat
   (lyskom-prot-a-format-bool (conf-type->rd_prot conf-type))
   (lyskom-prot-a-format-bool (conf-type->original conf-type))
   (lyskom-prot-a-format-bool (conf-type->secret conf-type))
   (lyskom-prot-a-format-bool (conf-type->letterbox conf-type))
   (if (lyskom-have-feature long-conf-types)
       (concat
         (lyskom-prot-a-format-bool (conf-type->anarchy conf-type))
         (lyskom-prot-a-format-bool (conf-type->forbid-secret conf-type))
         (lyskom-prot-a-format-bool (conf-type->rsv2 conf-type))
         (lyskom-prot-a-format-bool (conf-type->rsv3 conf-type)))
     "")))

(defun lyskom-prot-a-format-aux-item (item)
  "Format an AUX-ITEM for output to the server."
  (concat
   (int-to-string (aux-item->tag item))                     " "
   (lyskom-prot-a-format-aux-item-flags (aux-item->flags item))    " "
   (int-to-string (aux-item->inherit-limit item))           " "
   (lyskom-prot-a-format-string (aux-item->data item))))

(defun lyskom-prot-a-format-aux-item-flags (flags)
  "Format AUX-ITEM-FLAGS for output to the server."
  (concat
   (lyskom-prot-a-format-bool (aux-item-flags->deleted flags))
   (lyskom-prot-a-format-bool (aux-item-flags->inherit flags))
   (lyskom-prot-a-format-bool (aux-item-flags->secret flags))
   (lyskom-prot-a-format-bool (aux-item-flags->anonymous flags))
   (lyskom-prot-a-format-bool (aux-item-flags->reserved1 flags))
   (lyskom-prot-a-format-bool (aux-item-flags->reserved2 flags))
   (lyskom-prot-a-format-bool (aux-item-flags->reserved3 flags))
   (lyskom-prot-a-format-bool (aux-item-flags->reserved4 flags))))


(defun lyskom-prot-a-format-privs (privs)
  "Format PRIVS for output to the server."
  (concat
   (lyskom-prot-a-format-bool (privs->wheel privs))
   (lyskom-prot-a-format-bool (privs->admin privs))
   (lyskom-prot-a-format-bool (privs->statistic privs))
   (lyskom-prot-a-format-bool (privs->create_pers privs))
   (lyskom-prot-a-format-bool (privs->create_conf privs))
   (lyskom-prot-a-format-bool (privs->change_name privs))
   (lyskom-prot-a-format-bool (privs->flg7 privs))
   (lyskom-prot-a-format-bool (privs->flg8 privs))
   (lyskom-prot-a-format-bool (privs->flg9 privs))
   (lyskom-prot-a-format-bool (privs->flg10 privs))
   (lyskom-prot-a-format-bool (privs->flg11 privs))
   (lyskom-prot-a-format-bool (privs->flg12 privs))
   (lyskom-prot-a-format-bool (privs->flg13 privs))
   (lyskom-prot-a-format-bool (privs->flg14 privs))
   (lyskom-prot-a-format-bool (privs->flg15 privs))
   (lyskom-prot-a-format-bool (privs->flg16 privs))))

(defun lyskom-prot-a-format-bool (bool)
  "Format a BOOL for output to the server."
  (if bool "1" "0"))
      

(defun lyskom-prot-a-format-misc-list (misc-list)
  "Format a misc-list for output to the server."
  (let ((result (format "%d {" (length misc-list))))
    (while (not (null misc-list))
      (setq result (concat result " "
			    (lyskom-prot-a-format-misc-item (car misc-list))))
      (setq misc-list (cdr misc-list)))
    (setq result (concat result " }"))))


(defun lyskom-prot-a-format-misc-item (misc-item)
  "Format a misc-item for output to the server."
  (format "%d %d"
	  (cond
	   ((eq (car misc-item) 'recpt) 0)
	   ((eq (car misc-item) 'cc-recpt) 1)
	   ((eq (car misc-item) 'comm-to) 2)
	   ((eq (car misc-item) 'footn-to) 4)
           ((eq (car misc-item) 'bcc-recpt) 
            (if (lyskom-have-feature bcc-misc) 15 1)))
	  (cdr misc-item)))


(defun lyskom-prot-a-format-simple-list (list)
  "Format some kind of list to send to server."
  (apply 'concat (list (format "%d {" (length list))
		       (apply 'lyskom-format-objects list)
		       " }")))

  
(defun lyskom-prot-a-format-string (string)
  (format "%dH%s" (length string) string))




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
    (set-buffer-multibyte nil)
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

;;; This code removed (lyskom-tell-phrases-validate)


;;
(lyskom-set-language lyskom-language)

;; Build the menus
;; (lyskom-build-menus)


(or (memq 'lyskom-unread-mode-line global-mode-string)
    (setq global-mode-string
	  (append '("" lyskom-unread-mode-line) global-mode-string)))
(setq lyskom-unread-mode-line
      (list (list 'lyskom-sessions-with-unread 
		  (lyskom-get-string 'mode-line-unread))
	    (list 'lyskom-sessions-with-unread-letters
		  (lyskom-get-string 'mode-line-letters))
	    " "))

;;;
;;; Set up lyskom-line-start-chars. The reason we do it here is that
;;; char-to-int may not be defined until compatibility.el has been
;;; loaded.
;;;

(setq lyskom-line-start-chars
;;      (if (fboundp 'string-to-vector)
;;          (string-to-vector lyskom-line-start-chars-string)
        (let ((tmp (make-vector 256 nil)))
          (mapcar 
           (function
            (lambda (x)
              (aset tmp (char-to-int x) t)))
           lyskom-line-start-chars-string)
          tmp))
;;)
		 


;;; Formely lyskom-swascii-commands
;;(lyskom-define-language 'lyskom-command 'swascii
;;  (mapcar 
;;   (function (lambda (pair)
;;		 (cons (car pair) (iso-8859-1-to-swascii (cdr pair)))))
;;   (lyskom-get-strings lyskom-commands 'lyskom-command)))

;;(setq lyskom-swascii-header-separator 
;;	(iso-8859-1-to-swascii lyskom-header-separator))
;;(setq lyskom-swascii-header-subject
;;	(iso-8859-1-to-swascii lyskom-header-subject))

;;(setq lyskom-swascii-filter-actions
;;	(mapcar 
;;	 (function (lambda (pair)
;;		     (cons (car pair) (iso-8859-1-to-swascii (cdr pair)))))
;;	 lyskom-filter-actions))
;;(setq lyskom-swascii-filter-what
;;	(mapcar 
;;	 (function (lambda (pair)
;;		     (cons (car pair) (iso-8859-1-to-swascii (cdr pair)))))
;;	 lyskom-filter-what))


;; Setup the queue priorities
(lyskom-set-queue-priority 'blocking 9)
(lyskom-set-queue-priority 'main 9)
(lyskom-set-queue-priority 'sending 9)
(lyskom-set-queue-priority 'follow 9)
(lyskom-set-queue-priority 'options 9)
(lyskom-set-queue-priority 'deferred 6)
(lyskom-set-queue-priority 'background 6)
(lyskom-set-queue-priority 'modeline 6)
(lyskom-set-queue-priority 'async 3)
(lyskom-set-queue-priority 'prefetch 0)


(provide 'lyskoom-rest)

;;; This should be the very last lines of lyskom.el Everything should
;;; be loaded now, so it's time to run the lyskom-after-load-hook.

(run-hooks 'lyskom-after-load-hook)

(lyskom-end-of-compilation)

;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; end: 
