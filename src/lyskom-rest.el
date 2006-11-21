;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: lyskom-rest.el,v 44.262 2006-11-21 13:13:29 eric Exp $
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
;;;;    Hans Eric Svensson
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
	      "$Id: lyskom-rest.el,v 44.262 2006-11-21 13:13:29 eric Exp $\n"))


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

(put 'lyskom-error 'error-conditions
     '(error))
(put 'lyskom-error 'error-message
     "LysKOM error")


;;; ================================================================
;;;             Global variables
;;;

(defvar lyskom-unread-mode-line nil)
(defvar lyskom-unread-title-format nil)

;;; ================================================================
;;;             Error reporting from a number of commands.
;;;

;;; Author: Inge Wallin



(defun lyskom-get-error-text (errno &optional error-descriptions)
  "Get a string which is the error ERRNO in plain text."
  (let ((custom-error
         (or (cdr (assq errno error-descriptions))
             (cdr (assq (car (rassq errno lyskom-error-codes))
                        error-descriptions)))))
    (if custom-error
        (lyskom-get-string custom-error)
      (or
       (lyskom-try-get-string (intern (concat "error-"
                                              (number-to-string errno)))
                              'lyskom-error-texts)
       (lyskom-format 'error-not-found errno)))))

(defun lyskom-report-command-answer (answer &optional errno error-descriptions)
  "Handles a void return from call to the server."
  (if answer 
      (lyskom-insert-string 'done)
    (lyskom-insert-string 'nope)
    (lyskom-insert-error errno nil error-descriptions))
  answer)

(defun lyskom-insert-error (&optional errno err-stat error-descriptions)
  "Insert an error message describing ERRNO and ERR-STAT.
If ERRNO and ERR-STAT are not supplied, use lyskom-errno and
lyskom-err-stat instead.  If only ERRNO is supplied, use the empty
string for ERR-STAT.

If ERROR-DESCRIPTIONS is supplied, it should be an alist with error numbers
as the cars and error descriptions (symbols) as the cdr. The symbol will
be used to get a description of the corresponding error."
  (lyskom-format-insert 'error-code
                        (lyskom-get-error-text (or errno lyskom-errno)
                                               error-descriptions)
                        (or errno lyskom-errno)
                        (or err-stat (if errno "" lyskom-err-stat))))

(defun lyskom-current-error ()
  "Return a string describing the current error"
  (lyskom-format 'error-code
		 (lyskom-get-error-text lyskom-errno)
		 lyskom-errno
		 lyskom-err-stat))


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
If the optional argument REFETCH is non-nil or the funcion is called
interactively, all caches are cleared and the membership is re-read."
  (interactive (list t))
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
        (sit-for 0)                     ; FIXME: emacs-21.0.94 workaround
	(lyskom-next-command))
    (recenter 0)
    (sit-for 0)                     ; FIXME: emacs-21.0.94 workaround
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
           (set-extent-priority overlay 1000)
           (set-extent-face overlay kom-mark-face)
           (add-timeout 2 
                        'delete-extent
                        overlay))
         (let ((overlay (make-overlay 
                         lyskom-last-viewed
                         (save-excursion (goto-char lyskom-last-viewed)
                                         (end-of-line)
                                         (forward-char 1)
                                         (point)))))
           (overlay-put overlay 'face kom-mark-face)
           (overlay-put overlay 'priority 1000)
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
  (let ((saved-buffer (current-buffer)))
    (unwind-protect 
        (progn
          (setq lyskom-doing-default-command t)
          (cond
           ((eq lyskom-command-to-do 'next-pri-text)
            (lyskom-view-priority-text))
           ((eq lyskom-command-to-do 'next-text)
            (kom-view-next-text))
           ((eq lyskom-command-to-do 'reedit-text)
            (kom-re-edit-next-text))
           ((eq lyskom-command-to-do 'next-pri-session)
            (kom-go-to-pri-session))
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
           (t (signal 'lyskom-internal-error '(kom-next-command)))))
      (condition-case nil
          (save-excursion
            (set-buffer saved-buffer)
            (setq lyskom-doing-default-command nil))
        (error nil)))))


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
	  (lyskom-tell-internat 'kom-tell-review))
	(unless (and (null current-prefix-arg)
		     (lyskom-plusp text-no))
	  (let ((current-prefix-arg text-no))
	    (setq text-no (lyskom-read-text-no-prefix-arg 'review-text-q))))

        (cond (text-no
               (when (or (not (listp kom-page-before-command))
                         (memq 'kom-view kom-page-before-command))
                 (recenter 1)
                 (sit-for 0)                     ; FIXME: emacs-21.0.94 workaround
                 )
               (lyskom-tell-internat 'kom-tell-review)
               (lyskom-format-insert 'review-text-no text-no)
               (unless kom-review-uses-cache
                 (cache-del-text-stat text-no))
               (lyskom-view-text text-no))
              (t (lyskom-insert 'confusion-what-to-view))))
    (lyskom-end-of-command)))

(defun kom-initial-digit-unread ()
  (interactive)
  (setq unread-command-events (cons last-command-event
                                    unread-command-events))
  (call-interactively 'kom-mark-unread nil))

(def-kom-command kom-mark-unread (text-no)
  "Make a single text unread. This command is functional only on servers
that support making texts unread."
  (interactive (list (lyskom-read-text-no-prefix-arg 'unread-text-q)))
  (lyskom-format-insert 'marking-text-unread text-no)
  (lyskom-report-command-answer (lyskom-mark-unread text-no)))

(defun lyskom-mark-unread (text-no)
  "Mark text TEXT-NO in all conferences.
Returns non-nil if successful."
  (let ((text-stat (blocking-do 'get-text-stat text-no))
        (marked-unread nil))
    (and text-stat
         (let ((result
                (lyskom-traverse misc-info (text-stat->misc-info-list text-stat)
                  (when (memq (misc-info->type misc-info) lyskom-recpt-types-list)
                    (let* ((mship (lyskom-get-membership (misc-info->recipient-no misc-info)))
                           (recipient (and mship (blocking-do 'get-conf-stat (misc-info->recipient-no misc-info)))))
                      (when (and mship recipient)
                        (if (not (blocking-do 'mark-as-unread
                                              (misc-info->recipient-no misc-info)
                                              (misc-info->local-no misc-info)))
                            (lyskom-traverse-break 'error)
                          (when (lyskom-visible-membership mship)
                            (setq marked-unread t)
                            (unless (read-list-enter-text text-no 
                                                          recipient
                                                          lyskom-to-do-list)
                              (let ((info (lyskom-create-read-info
                                           'CONF
                                           recipient
                                           (membership->priority mship)
                                           (lyskom-create-text-list (list text-no)))))
                                (read-list-enter-read-info info lyskom-to-do-list)
                                (if (= lyskom-current-conf (conf-stat->conf-no recipient))
                                    (read-list-enter-read-info info lyskom-reading-list))))))))))))
           (cond (result nil)
                 (marked-unread t)
                 (t (setq lyskom-errno -1)
                    nil))))))



;;;; ================================================================
;;;;                          Re-edit text

(def-kom-command kom-re-edit-next-text ()
  "Display a buffer containing a failed text submission. This pseudo
command is only invoked from the prompt after submission of a text
failed."
  (interactive)
  (let ((el (read-list->first lyskom-reading-list)))
    (set-read-list-del-first lyskom-reading-list)
    (if (buffer-live-p (read-info->misc el))
        (lyskom-display-buffer (read-info->misc el))
      (lyskom-format-insert 'text-buffer-missing))))


;;;; ================================================================
;;;;                        Go to pri session

(def-kom-command kom-go-to-pri-session ()
  "Go to a prioritized session with unreads. This pseudo command is
generally invoked frmo the prompt when there is a session with unread
texts that has a higher priority than the current session.

See `kom-server-priority' and `kom-server-priority-breaks' for
settings to control session priorities."
  (interactive)
  (let ((session (lyskom-get-prioritized-session)))
    (if (or (null session)
            (eq session (current-buffer))
            (not (buffer-live-p session)))
        (lyskom-insert 'no-such-kom-session)
      (lyskom-switch-to-kom-buffer session))))


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
	 ((memq type lyskom-review-types-list)
	  (read-list-rotate lyskom-reading-list))
	 ((memq type lyskom-comment-types-list)
	  (set-read-list-del-first lyskom-reading-list))
	 (t 
	  (setq finished t))))
      (-- len)))
  (kom-view-next-text))


;;; Modified to handle filters

;; This is horribly ugly. It acts like a user command, but it isn't.
(defun lyskom-view-priority-text ()
  "Display the first text from the next conference on the lyskom-to-do-list.

Cannot be called from a callback."
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


;;; ========================================================================
;;; Functions to get the "best" text and the "least" text and their
;;; helper functions.

;;; Author: Hans Eric Svensson

(defun lyskom-member-of-at-least-one-p (confs)
  "Returns NIL if user is not a member of any of the conferences in
CONFS (list of conference numbers), otherwise a conference number."
   (lyskom-traverse conf confs
     (when (lyskom-get-membership conf t)
       (lyskom-traverse-break conf))))

(defun lyskom-member-one-priority-p (confs)
  "Find out if the maximum priority of the conferences in CONFS (list
of conference numbers) is greater than or equal to the user's session
priority which is given by `lyskom-session-priority' (i.e. if the user
will read a text with the set of conferences as the only recipients).

Returns NIL if user will not read texts to any of the conferences in
CONFS, given the current session priority, T otherwise."
(>= (or (lyskom-get-max-priority confs) -1) lyskom-session-priority))

(defun lyskom-up-to (list element)
  "Destructively remove elements in LIST from the first ELEMENT."
  (let ((tem list) (prev nil))
    (while tem
      (when (eq element (car tem))
        (if prev (setcdr prev nil) (setq tem nil list nil)))
      (setq prev tem tem (cdr tem)))
    list))

(defun lyskom-find-best-text (text-nos came-from loop-as-accept)
"Find the \"best\" text number to read next starting from the text
numbers TEXT-NOS (and CAME-FROM).

A text T2 is better to read than a text T1 if:

1. T1 is a comment or footnote to T2.
2. T2 is unread.
3. The recipients of T2 contain at least one conference of which the
   user is a member.
4. The recipients of T2 of which the user is a member has at least one
   priority which is higher than or equal to the current session
   priority.

-or-

1. T1 is a comment or footnote to the text T3.
2. T2 is a comment or footnote to T3.
3. T2 is unread.
4. T3 is read.
5. T2 commented T3 before T1 commented T3.
6. The recipients of T2 contain at least one conference of which the
   user is a member.
7. The recipients of T2 of which the user is a member has at least one
   priority which is higher than or equal to the current session
   priority.

We also keep track of which text numbers have previously been
considered to detect loops. If a loop is detected and LOOP-AS-ACCEPT
is NIL, NIL is returned. Otherwise, the text number which was
determined to be a part of the loop is returned. CAME-FROM is the text
number of the text which was used to acquire TEXT-NOS. It is only used
when TEXT-NO is a read text to filter out the comments to TEXT-NO
which were made after CAME-FROM (as well as CAME-FROM)."
  (let ((stack (list
                (list text-nos (list came-from) came-from
                      loop-as-accept came-from)))
        (result nil))
    (while (and (not result) stack)
      (let* ((params (car stack))
             (text-nos (nth 0 params)))
        (if (null text-nos)
            (setq result (nth 4 params)
                  stack (cdr stack))
          (let* ((text-no (car text-nos))
                 (visited (nth 1 params))
                 (loop-as-accept (nth 3 params))
                 (text-stat (blocking-do 'get-text-stat text-no))
                 ; Sometimes is-read is nil even though it has no proof
                 ; (when kom-follow-comments-outside-membership is nil).
                 (is-read (lyskom-text-read-p text-stat))
                 (confs (lyskom-text-recipients text-stat))
                 (but-current (delq lyskom-current-conf confs))
                 (is-member (lyskom-member-of-at-least-one-p but-current)))
            (setcar params (cdr text-nos))
            (cond ((not text-stat)) ; oops!
                  ((memq text-no visited) ; loop detection
                   (and (not is-read) loop-as-accept text-no))
                  (is-read
                   (let* ((came-from (nth 2 params))
                          (more-text-nos (lyskom-up-to
                                          (lyskom-text-comments text-stat)
                                          came-from)))
                     (setq stack (cons (list more-text-nos
                                             (cons text-no visited)
                                             text-no nil nil)
                                       stack))))
                  ((not (or is-read is-member)) ; outside membership
                   (let*
                       ((more-text-nos (lyskom-text-stat-commented-texts
                                        text-stat))
                        (came-from (nth 2 params))
                        (even-more-text-nos (lyskom-up-to
                                             (lyskom-text-comments text-stat)
                                             came-from)))
                     (setq stack (cons (list more-text-nos
                                             (cons text-no visited)
                                             text-no
                                             loop-as-accept
                                             nil)
                                       (cons (list even-more-text-nos
                                                   (cons text-no visited)
                                                   text-no nil nil)
                                             stack)))))
                  (t (let*
                         ((more-text-nos (lyskom-text-stat-commented-texts
                                          text-stat))
                          (text-no-if-fail (and
                                            is-member
                                            (lyskom-member-one-priority-p
                                             but-current)
                                            text-no)))
                     (setq stack (cons (list more-text-nos
                                             (cons text-no visited)
                                             text-no
                                             loop-as-accept
                                             text-no-if-fail)
                                       stack)))))
            (unless kom-review-uses-cache
              (cache-del-text-stat text-no))))))
    result))

(defsubst lyskom-minimum (list)
  "Return the least element in the list LIST."
  (and list (apply #'min list)))

(defun lyskom-find-lowest-text (text-no)
  "Return the \"lowest\" text to read next, starting from the text
number TEXT-NO.

A text T2 is lower than a text T1 if:

1. The text number of T2 is less than the text number of T1.
2. T2 is unread.

We consider all of the texts reachable from TEXT-NO.

We keep track of which text numbers have previously been considered
VISITED to detect loops and save all of the candidates in CANDIDATES."
  (let* ((visited nil)
         (candidates nil)
         (consider (list text-no)))
    (while consider
      (let* ((text-no (car consider))
             (text-stat (blocking-do 'get-text-stat text-no))
             (is-read (lyskom-text-read-p text-stat))
             (confs (lyskom-text-recipients text-stat))
             (is-member (lyskom-member-of-at-least-one-p confs))
             (more-text-nos (lyskom-text-stat-commented-texts text-stat))
             (even-more-text-nos (lyskom-text-comments text-stat)))
        (setq consider (cdr consider))
        (if is-member
            (unless (or is-read (memq text-no visited))
              (setq candidates (cons text-no candidates))
              (if consider
                  (nconc consider more-text-nos even-more-text-nos)
                (setq consider
                      (nconc consider more-text-nos even-more-text-nos)))))
        (setq visited (cons text-no visited))
        (unless kom-review-uses-cache
          (cache-del-text-stat text-no))))
    (lyskom-minimum candidates)))

(defun lyskom-get-max-priority (confs)
"Return the maximum priority of the conferences in CONFS (list of
conference numbers). If the list is empty or if the user is not a
member of any of the conferences NIL is returned."
  (let ((priorities (delq nil (mapcar
                               (lambda (conf-no)
                                 (membership->priority
                                  (lyskom-get-membership conf-no t)))
                               confs))))
    (unless (null priorities) (apply #'max priorities))))

;;;; ================================================================
;;;;                         View next text (cont'd.)

;;; Modified to handle filters

;;; Modified to read commented texts first (Hans Eric Svensson)

(def-kom-command kom-view-next-text ()
  "Display the next unread text. This is the most common default command."
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
		 (type (read-info->type tri))
		 (is-review-tree (memq type '(REVIEW-TREE)))
		 (is-review (or (memq type '(REVIEW REVIEW-MARK REVIEW-FAQ))
				is-review-tree))
                 (is-reading (memq type '(CONF COMM-IN FOOTN-IN)))
		 (text-no-maybe (car (text-list->texts (read-info->text-list
                                                        tri))))
                 (text-stat-maybe (blocking-do 'get-text-stat text-no-maybe))

                 (text-no (if is-reading
                              (if kom-read-depth-first
                                  (lyskom-find-best-text
                                   (lyskom-text-stat-commented-texts
                                    text-stat-maybe)
                                   text-no-maybe
                                   t)
                                (lyskom-find-lowest-text text-no-maybe))
                            text-no-maybe))

                 (text-stat (blocking-do 'get-text-stat text-no))
                 
                 (priority (read-info->priority tri))
                 (conf-stat (read-info->conf-stat tri))
                 
		 (mark-as-read (not is-review)))
	    (when (or is-review (eq type 'REVIEW-FAQ-TREE))
              (set-text-list->texts
               (read-info->text-list tri)
               (delq text-no (text-list->texts 
                              (read-info->text-list tri)))) ;First entry only
              (unless kom-review-uses-cache
                (cache-del-text-stat text-no)))
	    (setq action
		  (lyskom-view-text text-no mark-as-read 
				    (and kom-read-depth-first
					 (not is-review))
				    conf-stat
				    priority
				    is-review-tree
				    (not is-review)
				    (memq type '(REVIEW REVIEW-MARK))))
	    (if mark-as-read
		(lyskom-is-read text-no)
	      (read-list-delete-text nil lyskom-reading-list)
	      (read-list-delete-text nil lyskom-to-do-list))))))))


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
    (if (memq (misc-info->type misc) '(RECPT BCC-RECPT CC-RECPT))
	(let ((membership (lyskom-try-get-membership
			   (misc-info->recipient-no misc))))
	  (when membership
            (set-membership->read-texts
             membership
             (vconcat (vector (misc-info->local-no misc))
                      (membership->read-texts membership))))))))


;;; ================================================================
;;;                        Go to next conf.


(def-kom-command kom-go-to-next-conf (&optional num)
  "Go to next conference with unread texts. The current conference is
moved to the end of the list of conferences with unread texts. A
numeric prefix argument is interpreted similar to in `kom-list-news':
with a positive argument go to the next conference with at least that
many unread. With a negative number, go to a conference with at most
that many unread texts."
  (interactive "P")
  (let ((conf-stat nil)
	(num-arg (cond
		  ((numberp num) num)
		  ((and (listp num)
			(numberp (car num))) (car num))
		  (t nil))))
    (when num-arg
      (let ((read-list (cdr (lyskom-list-news)))
	    (msgs-conf nil)
	    (unreads   nil)
	    (at-least  (if  (< num-arg 0) 1 num-arg))
	    (at-most   (and (< num-arg 0) (- num-arg))))
	(while (and read-list (null conf-stat))
	  (setq msgs-conf (car read-list))
	  (setq read-list (cdr read-list))
	  (setq unreads (car msgs-conf))
	  (when (and (>= unreads at-least) ; unreads within lower bound
		     (or (not at-most)
			 (<= unreads at-most))) ; unreads within upper bound
	    (setq conf-stat (cdr msgs-conf))))))
    (if (and num-arg (null conf-stat))
	(lyskom-insert-string 'no-unreads-shown)
      (when (lyskom-check-go-to-conf conf-stat)
	(lyskom-maybe-move-unread t)
	(if conf-stat
	    (lyskom-go-to-conf conf-stat)
	  (lyskom-go-to-next-conf))))))


(defun lyskom-go-to-pri-conf ()
  "Go to the conf with higher priority.
This differs from kom-go-to-next-conf only in the place where the yet unread 
in the current conf is placed."
  (lyskom-start-of-command 'kom-go-to-next-conf)
  (when (lyskom-check-go-to-conf)
    (lyskom-maybe-move-unread nil)
    (lyskom-go-to-next-conf)
    (lyskom-end-of-command)))


(defun lyskom-go-to-next-conf ()
  "Actually go to the next conference on the lyskom-to-do-list"
  ;; Copy first element on lyskom-to-do-list to lyskom-reading-list.
  (if (not (read-list-isempty lyskom-to-do-list))
      (let ((conf-stat (read-info->conf-stat 
                        (read-list->first lyskom-to-do-list))))
        (if conf-stat
            (progn (lyskom-set-mode-line conf-stat)
                   (read-list-enter-first
                    (read-list->first lyskom-to-do-list)
                    lyskom-reading-list)
                   (lyskom-enter-conf conf-stat 
                                      (read-list->first lyskom-to-do-list)))
          (read-list-enter-first
           (read-list->first lyskom-to-do-list)
           lyskom-reading-list)))
    (lyskom-insert-string 'all-conf-unread-r)
    (lyskom-set-mode-line (lyskom-get-string 'all-conf-unread-s))))


(defun lyskom-maybe-move-unread (bury)
  "Empty the reading list.
If the argument BURY is non-nil and there are unread articles left in the
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


(defun lyskom-check-go-to-conf (&optional conf)
  "Check if it is OK to go to conf CONF (a conf-stat).
It is assumed that lyskom-pers-no is a member of the conference.
This function interacts with the buffer and the user.

If CONF is nil, check the first conf on the to-do list."
  (let* ((conf-stat (or conf
                        (read-info->conf-stat 
                         (read-list->first lyskom-to-do-list))))
         (mship (and conf-stat 
                     (lyskom-get-membership (conf-stat->conf-no conf-stat) t)))
         (type nil)
         (continue t))
    (when (and conf-stat mship)
      (setq type (membership->type mship))

      (when (membership-type->invitation type)
        ;;; State the invitation,give the user a chance to accept it
        ;;; Leave the conf if the user does not want it
        (lyskom-format-insert 'your-invited
                              conf-stat
                              (membership->created-by mship))
        (if (lyskom-j-or-n-p (lyskom-get-string 'accept-invitation))

            (setq continue (lyskom-add-member (blocking-do 'get-conf-stat lyskom-pers-no) conf-stat))
          (let ((kom-unsubscribe-makes-passive nil))
            (lyskom-sub-member (blocking-do 'get-conf-stat lyskom-pers-no)
                               conf-stat)
            (setq continue nil))))

      (when (and continue (membership-type->secret type))
        (lyskom-format-insert-before-prompt 'bug-secret-mship conf-stat))

      (when (and continue (membership-type->passive type))
        ;;; Offer the user a chance to become an active member
        ;;; If the offer is refused, boot the user off the conference
        (lyskom-format-insert 'enter-passive conf-stat)
        (setq continue nil)
        (if (lyskom-j-or-n-p (lyskom-format 'convert-passive conf-stat))
            (setq continue 
                  (lyskom-add-member (blocking-do 'get-conf-stat lyskom-pers-no) conf-stat))
          (setq continue nil)))
      )

    ;;; Return the result
    continue))

(defun lyskom-reject-recommendation (conf-no)
  "Record that we have rejected an invitation to CONF-NO 
by PERS-NO"
  (unless (lyskom-recommendation-already-rejected conf-no)
    (setq lyskom-rejected-recommendations
          (cons conf-no lyskom-rejected-recommendations))
    (initiate-modify-conf-info 
     'background
     nil
     lyskom-pers-no
     nil
     (list (lyskom-create-aux-item 0 10001 nil nil
                                   (lyskom-create-aux-item-flags nil nil t nil
                                                                 nil nil nil nil)
                                   0
                                   (format "%d" conf-no))))))

(defun lyskom-recommendation-already-rejected (conf-no)
  "Return non-nil if we have rejected recommendation for CONF-NO"
  (member conf-no lyskom-rejected-recommendations))

(defun lyskom-update-rejected-recommendations ()
  "Update the list of rejected recommendations"
  (setq lyskom-rejected-recommendations nil)
  (let* ((conf-stat (blocking-do 'get-conf-stat lyskom-pers-no))
         (aux-list (and conf-stat (lyskom-get-aux-item
                                   (conf-stat->aux-items conf-stat) 
                                   10001))))
    (lyskom-traverse item aux-list
      (when (string-match "^\\([0-9]+\\)" (aux-item->data item))
        (setq lyskom-rejected-recommendations
              (cons (string-to-int (match-string 1 (aux-item->data item)))
                    lyskom-rejected-recommendations))))))

(defun lyskom-startup-check-recommended-memberships ()
  "Create invitations for new membership recommendations"
  (let ((recommendations 
         (filter-list
          (lambda (rec)
            (not (lyskom-recommendation-already-rejected (car rec))))
          (mapcar (lambda (aux)
                    (when (string-match "^\\([0-9]+\\)\\( [0-9]+\\)?\\( [01]+\\)?" (aux-item->data aux))
                      (let* ((conf-no (string-to-int (match-string 1 (aux-item->data aux))))
                             (priority (and conf-no (match-string 2) (string-to-int (match-string 2 (aux-item->data aux)))))
                             (mship-type (and priority (match-string 3) (match-string 3 (aux-item->data aux)))))
                        (list conf-no priority mship-type (aux-item->creator aux)))))
                  (lyskom-get-aux-item (server-info->aux-item-list
                                        lyskom-server-info) 29))))
        (collector (make-collector)))
    ;; At this point RECOMMENDATIONS contains a list with one element
    ;; per previously not rejected conference membership recommendations
    ;; (including the ones we are actually members of).

    (lyskom-traverse rec recommendations
      (initiate-query-read-texts 'main
                                 'collector-push
                                 lyskom-pers-no
                                 (car rec)
                                 nil 0
                                 collector))
    (lyskom-wait-queue 'main)

    ;; Now delete recommendations that we are already members of

    (lyskom-traverse mship (collector->value collector)
      (when mship
        (setq recommendations
              (delq (assq (membership->conf-no mship) recommendations)
                    recommendations))))

    ;; At this point recommendations contains only the conferences
    ;; that we have not rejected and that we are not already members
    ;; of.

    (let ((pers-conf-stat (blocking-do 'get-conf-stat lyskom-pers-no)))
      (lyskom-traverse rec recommendations
        (let ((conf-stat (blocking-do 'get-conf-stat (elt rec 0))))
          (when conf-stat
            (lyskom-format-insert 'has-recommended-conf
                                  (elt rec 3)
                                  (elt rec 0))
            (if (lyskom-j-or-n-p (lyskom-format 'accept-recommendation (elt rec 0)))
                (lyskom-add-member pers-conf-stat conf-stat nil nil 
                                   (and (elt rec 1) (list (elt rec 1))) nil)
              (lyskom-reject-recommendation (elt rec 0)))))))))

(defun lyskom-clean-read-faqs (pers-no)
  "Remove all read-faq aux-items from PERS-NO that correspond to texts
that are no longer FAQs or are missing."
  (let ((conf-stat (blocking-do 'get-conf-stat pers-no)))
    (when conf-stat
      (lyskom-traverse item (lyskom-get-aux-item (conf-stat->aux-items conf-stat)
                                                 10000)
        (when (string-match "^\\([0-9]+\\) \\([0-9]+\\)" (aux-item->data item))
          (let ((conf-no (string-to-int (match-string 1 (aux-item->data item))))
                (text-no (string-to-int (match-string 2 (aux-item->data item)))))
            (initiate-get-text-stat 'background
                                    'lyskom-clean-read-faqs-1
                                    text-no
                                    pers-no
                                    conf-no
                                    item)))))))

(defun lyskom-clean-read-faqs-1 (text-stat pers-no conf-no item)
  "Callback for lyskom-clean-read-faqs"
  (let ((cmp (int-to-string conf-no)))
    (when (or (null text-stat)
              (not (lyskom-traverse aux (lyskom-get-aux-item 
                                         (text-stat->aux-items text-stat)
                                         28)
                     (when (string= cmp (aux-item->data aux))
                       (lyskom-traverse-break t)))))
      (initiate-modify-conf-info 'background
                                 nil
                                 pers-no
                                 (list (aux-item->aux-no item))
                                 nil)
      )))


(defun lyskom-enter-conf (conf-stat read-info)
  "Tell server which conf the user is reading.
Prints the name and amount of unread in the conference we just went to 
according to the value of kom-print-number-of-unread-on-entrance.
Args: CONF-STAT READ-INFO"
  (let ((from-conf lyskom-current-conf)
        (to-conf (conf-stat->conf-no conf-stat)))
    (lyskom-run-hook-with-args 'lyskom-change-conf-hook
                               from-conf
                               to-conf)
    (lyskom-run-hook-with-args 'kom-change-conf-hook 
                               lyskom-current-conf
                               (conf-stat->conf-no conf-stat))
    (unless lyskom-is-anonymous (initiate-pepsi 'main nil to-conf))
    (setq lyskom-current-conf to-conf)
    (lp--update-buffer from-conf)
    (lp--update-buffer to-conf)
    (let ((num-unread (text-list->length (read-info->text-list read-info))))
      (lyskom-enter-conf-print-unread conf-stat num-unread)
      (lyskom-run-hook-with-args 'lyskom-after-change-conf-hook
                                 from-conf
                                 to-conf)
      (lyskom-run-hook-with-args 'kom-after-change-conf-hook 
                                 lyskom-current-conf
                                 (conf-stat->conf-no conf-stat)))))

(defun lyskom-enter-conf-print-unread (conf num)
  "Print information about unread (if requested) when entering a conf.
CONF is the conference and NUM is the number of unread in the conference."
  (let ((faq-list (lyskom-get-unread-faqs conf)))
    (lyskom-format-insert "%#1M" conf)
    (when (or (eq num 0) kom-print-number-of-unread-on-entrance faq-list)
      (lyskom-insert " - "))

    (cond ((eq num 0) 
	   (lyskom-format-insert 'conf-all-read conf))
	  (kom-print-number-of-unread-on-entrance
	   (lyskom-format-insert 'enter-conf-unread num)))

    (when faq-list
      (when (or (eq num 0) kom-print-number-of-unread-on-entrance)
	(lyskom-insert ", "))
      (lyskom-format-insert 'enter-conf-unread-faq (length faq-list)))
  
    (lyskom-insert (lyskom-conf-type-marker conf))
    (lyskom-format-insert ".\n")
    (lyskom-present-unread-faqs conf faq-list)))


(defun lyskom-leave-current-conf ()
  "Leave the current conference without going to another one."
  (let ((from-conf lyskom-current-conf))
    (set-read-list-empty lyskom-reading-list)
    (lyskom-run-hook-with-args 'lyskom-change-conf-hook
                               from-conf 0)
    (setq lyskom-current-conf 0)
    (initiate-pepsi 'main nil 0)
    (lyskom-run-hook-with-args 'lyskom-after-change-conf-hook
                               from-conf 0)
    ))


;;;================================================================

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
        (conf-no (if (lyskom-conf-stat-p conf) (conf-stat->conf-no conf) 0))
	(unread -1)
	(total-unread 0)
	(letters 0)
	(len 0)
	(read-info-list nil))

      ;; Set unread to the number of unread texts in CONF.
    (setq read-info-list (read-list->all-entries lyskom-to-do-list))

    ;; This was weird. Someone had begun to write an if, but
    ;; this was all there was: (if 
    (while read-info-list
      (if (read-info->conf-stat (car read-info-list))
          (progn
            (setq len (text-list->length
                       (read-info->text-list (car read-info-list))))
            (if (= conf-no
                   (conf-stat->conf-no 
                    (read-info->conf-stat (car read-info-list))))
                (setq unread len))
            (if (= lyskom-pers-no
                   (conf-stat->conf-no 
                    (read-info->conf-stat (car read-info-list))))
                (setq letters len))
            (setq total-unread (+ total-unread len))))
      (setq read-info-list (cdr read-info-list)))
      (if (= unread -1)
	  (setq unread 0))
      
      (if (null name)
	  nil
	(setq mode-line-conf-name 
	      (substring (concat
			  (if (lyskom-conf-stat-p conf)
			      (if (not (eq lyskom-session-priority 
                                           kom-default-session-priority))
				  (format "(%d/%d:%d) "
					  unread total-unread
					  lyskom-session-priority)
				(format "(%d/%d) " unread total-unread))
			    "")
			  name
			  (make-string 27 ? ))
			 0 27)))

      (if (zerop total-unread)
          (lyskom-remove-unread-buffer lyskom-buffer)
        (lyskom-add-unread-buffer lyskom-buffer))
      (if (zerop letters)
          (lyskom-remove-unread-buffer lyskom-buffer t)
        (lyskom-add-unread-buffer lyskom-buffer t))

      (lyskom-mode-name-from-host)
      (force-mode-line-update)))




;;;; ================================================================
;;;;                   Scrolling and text insertion.


(defvar lyskom-trim-buffer-delete-to)

(defun lyskom-trim-buffer ()
  "Trim the size of a lyskom buffer to lyskom-max-buffer-size
returns t if it trimmed the buffer, nil otherwise."
  (when (and kom-max-buffer-size
	     (> (- (buffer-size) kom-trim-buffer-minimum) kom-max-buffer-size))
    (lyskom-save-excursion
      (let ((lyskom-trim-buffer-delete-to (- (buffer-size)
					     kom-max-buffer-size))
	    (inhibit-read-only t))
	(goto-char (point-min))
	(while (< (point) lyskom-trim-buffer-delete-to)
	  (forward-line 1))
	(setq lyskom-trim-buffer-delete-to (point))
	(run-hooks 'lyskom-trim-buffer-hook)
	(delete-region (point-min) lyskom-trim-buffer-delete-to)))
      t))

;;USER-HOOK: lyskom-garb-lyskom-buffer-to-file
(defun lyskom-garb-lyskom-buffer-to-file ()
  "Appends the deleted initial portions of the buffer to a file.
Put this function in your lyskom-trim-buffer-hook and set
kom-max-buffer-size to something clever, and the trimmed text
will automagically flow into your lyskom log file."
  (append-to-file 1 lyskom-trim-buffer-delete-to
		  (expand-file-name (concat "~/" (buffer-name) "-history"))))


(defun lyskom-scroll ()
  "Scroll screen if necessary.
The position lyskom-last-viewed will always remain visible."
  ;; Find a window to scroll. Scroll the selected window if that shows LysKOM.
  ;; Otherwise scroll any window currently showing LysKOM.
  (let ((win (cond ((eq (window-buffer (selected-window)) (current-buffer))
                    (selected-window))
                   (t nil))))
    (when (and win (not (pos-visible-in-window-p (point-max))))
      (goto-char (point-max))
      (recenter -1)
      (unless (pos-visible-in-window-p lyskom-last-viewed)
        (set-window-start win lyskom-last-viewed)
        (move-to-window-line -1)
        (vertical-motion 1)
        (when (not (pos-visible-in-window-p))
          (forward-char -1)
          (when (> (current-column) (window-width))
            (backward-char (+ (- (current-column) (window-width)) 6)))
          )))))


(defun lyskom-next-property-bounds (from to sym)
  "Sort of like next-text-property-bounds, but limited at the front and
back, and works even if from has the property."
  (let ((result (cond ((get-text-property from sym)
                       (cons from (next-single-property-change from sym)))
                      ((next-single-property-change from sym)
                       (let ((start (next-single-property-change from sym)))
                         (cons start (next-single-property-change start sym)))))))
    (when result
      (cond ((null (cdr result)) (setcdr result (or to (point-max))))
            ((and from (< (car result) from)) (setcar result from))
            ((and to (> (cdr result) to)) (setcar result to)))) 
    result))


(defun lyskom-do-special-inserts (start end sym)
  (condition-case var
      (let ((bounds (lyskom-next-property-bounds start end sym))
            (next (make-marker))
            (fn nil))
        (while bounds
          (set-marker next (cdr bounds))
          (setq fn (get-text-property (car bounds) sym))
          (remove-text-properties (car bounds) (cdr bounds) (list sym))
          (condition-case val
              (funcall (if (listp fn) (car fn) fn)
                       (car bounds)
                       (cdr bounds)
                       (if (listp fn) (cdr fn) nil))
            (error (apply 'message "%S" val)))
          (setq start next)
          (setq bounds (lyskom-next-property-bounds
                        (1- start) (point-max) sym)))
	(set-marker next nil))
    (error (lyskom-ignore var))))

(defun lyskom-do-insert (string)
  (let ((start (point)))
    (insert string)
    (lyskom-do-special-inserts start (point) 'lyskom-fill)
    (lyskom-do-special-inserts start (point) 'lyskom-overlay)
    (lyskom-do-special-inserts start (point) 'special-insert)
))



(defun lyskom-insert (string)
  "Insert STRING last in current buffer.
Leaves the point at the end of the buffer if possible without
scrolling past lyskom-last-viewed (generally the most recent prompt.)
Leaves the point at the end of the window if not possible. If buffer
is not on screen then doesn't move point."
  (when (symbolp string) (setq string (lyskom-get-string string)))
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
  "Insert STRING in the current buffer at point."
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
        ;; Some (all?) xemacsen adds the text-properties on the prompt
        ;; to text inserted before it. We don't want this, so insert a
        ;; blank and remove all text-properties from it, insert the
        ;; text before the blank and finally remove the blank.
        (insert " ")
        (backward-char)
        (remove-text-properties (point) (+ (point) 1) (text-properties-at (point)))
        (lyskom-do-insert string)
        (delete-char 1))
      (goto-char oldpoint)
      (set-marker oldpoint nil))
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
    (setq lyskom-last-viewed (point-marker))))


;;; ================================================================
;;;                  Extended string formatting

;;; Author: David Byers
;;; Original code: Inge Wallin


(defvar lyskom-format-format
  "%\\(=\\|<\\|>\\)?\\(-?[0-9]+\\|-?[0-9]+\\.[0-9]+\\|-?[0-9]+\\.[0-9]+\\.[0-9]+\\)?\\(\\.\\.\\.\\)?\\(:.\\)?\\(#\\([0-9]+\\)\\)?\\(_\\)?\\(:\\)?\\(&\\)?\\([][$@MmPpnrtsdoxlcfgCSDF%?]\\)"
  "regexp matching format string parts.")

(defun lyskom-insert-string (atom)
 "Find the string corresponding to ATOM and insert it into the LysKOM buffer." 
 (if (stringp atom)
     (lyskom-insert atom)
   (lyskom-insert (lyskom-get-string atom))))

(defun lyskom-format-transform-result (state)
  "Convert a format-state to a string."
  (let ((result (format-state->result state)))
    (lyskom-traverse overlay (format-state->delayed-overlays state)
      (add-text-properties (aref overlay 0)
                           (aref overlay 1)
                           (list 'lyskom-overlay 
                             (cons 'lyskom-special-insert-overlay
                                   (aref overlay 2)))
                           result))
    result))

(defun lyskom-limited-make-overlay (start end)
  (let ((val (lyskom-xemacs-or-gnu
              (make-extent start end)
              (make-overlay start end))))
  (when (lyskom-plusp kom-max-overlays)
    (let ((old (nthcdr (1- kom-max-overlays) lyskom-overlay-pool))) ;
      (setq lyskom-overlay-pool 
            (lyskom-nbutlast lyskom-overlay-pool 
                             (- (length lyskom-overlay-pool)
                                kom-max-overlays)))
      (mapcar
       (lambda (o) (lyskom-xemacs-or-gnu (delete-extent o)
					 (delete-overlay o))) old))
    (setq lyskom-overlay-pool (cons val lyskom-overlay-pool)))
  val))


(defun lyskom-special-insert-overlay (start end args)
  (lyskom-xemacs-or-gnu
   (let ((overlay (lyskom-limited-make-overlay start end)))
     (while args
       (set-extent-property overlay (car args) (car (cdr args)))
       (setq args (nthcdr 2 args)))
     (set-extent-priority overlay 1000))
   (let ((overlay (lyskom-limited-make-overlay start end)))
     (while args
       (overlay-put overlay (car args) (car (cdr args)))
       (setq args (nthcdr 2 args))))))

(defun lyskom-format (format-string &rest argl)
  (lyskom-format-transform-result (lyskom-do-format format-string argl)))

(defun lyskom-format-insert (format-string &rest argl)
  "Format and insert a string according to FORMAT-STRING.
The string is inserted at the end of the buffer with `lyskom-insert'."
  (let* ((state (lyskom-do-format format-string argl t))
	 ;; We have to use a marker, because lyskom-insert may trim
	 ;; the buffer size.
	 (start (point-max-marker))
	 (deferred (format-state->delayed-content state)))
    (lyskom-insert (lyskom-format-transform-result state))

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
    (lyskom-insert-at-point (lyskom-format-transform-result state))
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

Deferred insertions are not supported."
  (lyskom-insert-before-prompt
   (lyskom-format-transform-result (lyskom-do-format format-string argl))))


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
	    (setq state (lyskom-format-aux (lyskom-create-format-state
					    fmt
					    0
					    argl
                                            (length argl)
					    ""
                                            nil
                                            nil
                                            nil
                                            0)
					   allow-defer))
	  (lyskom-format-error
	   (error "LysKOM internal error formatting %s: %s%s (arg %S, got %S)"
		  format-string (nth 1 error) (nth 2 error) 
                  (nth 3 error) (nth 4 error))))))
    state))



;;;
;;; If you add a format letter, for goodness' sake, don't forget to
;;; att it to the regexp above too!
;;;



(defun lyskom-format-aux (format-state allow-defer)
    (set-format-state->depth format-state
                             (1+ (format-state->depth format-state)))
    (let ((format-length (length (format-state->format-string format-state)))
        (arg-no nil)
        (pad-length nil)
        (frac-min nil)
        (frac-max nil)
        (pad-string nil)
        (format-letter nil)
        (colon-flag nil)
        (equals-flag nil)
        (downcase-flag nil)
        (face-flag nil)
        (trailer nil)
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

        (if (match-beginning 2)
          (save-match-data
            (let* ((s (match-string 2 (format-state->format-string format-state))))
              (cond ((or (string-match "\\(-?[0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" s)
                         (string-match "\\(-?[0-9]+\\)\\.\\([0-9]+\\)" s))
                     (setq pad-length (string-to-int (match-string 1 s)))
                     (setq frac-min (string-to-int (match-string 2 s)))
                     (setq frac-max (string-to-int (or (match-string 3 s) (match-string 2 s)))))
                    (t (setq pad-length (string-to-int s))))))
          (setq pad-length nil))

	(setq equals-flag (and (match-beginning 1)
                               (aref (match-string 1 
                                                   (format-state->format-string
                                                    format-state)) 0))
	      arg-no (if (match-beginning 6)
			 (string-to-int 
                          (match-string 6 (format-state->format-string
                                           format-state)))
		       nil)
              downcase-flag (match-beginning 7)
	      colon-flag (match-beginning 8)
              face-flag (match-beginning 9)
              trailer (match-beginning 3)
              pad-string (and (match-beginning 4)
                              (substring (format-state->format-string format-state)
                                         (1+ (match-beginning 4))
                                         (match-end 4)))
	      format-letter (if (match-beginning 10)
				(aref (format-state->format-string 
				       format-state)
				      (match-beginning 10))
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
             frac-min
             frac-max
             arg-no
             format-letter
             equals-flag
             trailer
             colon-flag
             face-flag
             downcase-flag
             (or (elt pad-string 0)
                 (if (and (match-beginning 2)
                          (eq (aref (format-state->format-string format-state)
                                    (match-beginning 2))
                              ?0))
                     ?0
                   ?\ ))
	     allow-defer))))))
    (lyskom-tweak-format-state format-state)
    (set-format-state->depth format-state
                             (1- (format-state->depth format-state)))
    format-state)


(defun lyskom-format-aux-help (format-state
                               pad-length
                               frac-min
                               frac-max
                               arg-no
                               format-letter
                               equals-flag
                               trailer
                               colon-flag
                               face-flag
                               downcase-flag
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
             (not (eq format-letter ?\?))
             (< (format-state->length format-state) arg-no))
        (signal 'lyskom-format-error (list 'lyskom-format
                                             ": too few arguments")))
    (if arg-no
        (setq arg (nth (1- arg-no) (format-state->argl format-state))))

    (cond
     ;;
     ;;  Format a string or symbol by simply inserting it into the
     ;;  result list
     ;;
     ((= format-letter ?l)
      (setq result 
            (cond ((or (integerp arg)
                       (lyskom-characterp arg))
                   (make-string pad-length arg))
                  ((stringp arg)
                   (let ((count (/ pad-length (length arg)))) 
                     (apply 'concat 
                            (nreverse
                             (cons (substring 
                                    arg 0 
                                    (- pad-length (* (length arg) count)))
                                   (make-list count arg))))))
                  (t (signal 'lyskom-internal-error
                                          (list 'lyskom-format
                                                ": argument error (expected char or string)"
                                                arg-no
                                                arg))))))

     ((= format-letter ?s)
      (setq result (cond ((stringp arg) arg)
                         ((symbolp arg) (symbol-name arg))
                         (t (signal 'lyskom-format-error
                                    (list 'lyskom-format
                                          ": argument error (expected string)"
                                          arg-no
                                          arg)))))
      (when downcase-flag (setq result (downcase result))))
     ;;
     ;;  Format a number by conferting it to a string and inserting
     ;;  it into the result list
     ;;
     ((or (= format-letter ?d)
          (= format-letter ?o)
          (= format-letter ?x))
      (setq result (cond ((integerp arg)
                          (format (format "%%%c" format-letter) arg))
                         ((and (floatp arg) (= format-letter ?d))
                          (format "%0.0f" arg))
                         (t (signal 'lyskom-internal-error
                                    (list 'lyskom-format
                                          ": argument error (expected int)"
                                          arg-no
                                          arg))))))

     ((or (= format-letter ?g)
          (= format-letter ?f)
          (= format-letter ?e))
      (setq result (cond ((numberp arg) 
                          (format (if frac-max
                                      (format "%%.%d%c" 
                                              frac-max
                                              format-letter)
                                      (format "%%%c" format-letter))
                                  arg))
                         (t (signal 'lyskom-internal-error
                                    (list 'lyskom-format
                                          ": argument error (expected number)"
                                          arg-no
                                          arg)))))
      (save-match-data
        (when (and (string-match "\\." result)
                   (string-match "\\.?0+$" result))
          (setq result (substring result 0 (match-beginning 0))))
        (when (and frac-min (> frac-min 0))
          (if (string-match "\\.\\([0-9]*\\)$" result)
              (setq result 
                    (concat result
                            (make-string 
                             (max 0 
                                  (- frac-min
                                     (length (match-string 1 result))))
                             ?0)))
            (setq result
                  (concat result
                          "."
                          (make-string frac-min ?0)))))))

     ((= format-letter ?f)
      (setq result (cond ((numberp arg) (format "%f" arg))
                         (t (signal 'lyskom-internal-error
                                    (list 'lyskom-format
                                          ": argument error (expected number)"
                                          arg-no
                                          arg))))))


     ;;
     ;;  Format a character by converting it to a string and inserting
     ;;  it into the result list
     ;;
     ((= format-letter ?c)
      (setq result (cond ((integerp arg) (char-to-string (lyskom-int-to-char arg)))
                         ((lyskom-characterp arg) (char-to-string arg))
                         (t (signal 'lyskom-internal-error
                                    (list 'lyskom-format
                                          ": argument error (expected char)"
                                          arg-no
                                          arg))))))
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
      (setq result (format "%S" arg))
      (when downcase-flag (setq result (downcase result))))

     ;;
     ;;  Format a text property array indicator by retrieving the
     ;;  properties from the argument list and adding a start of 
     ;;  new properties to the format state
     ;;
     ((= format-letter ?@)
      (set-format-state->delayed-propl
       format-state
       (cons (vector (length (format-state->result format-state))
                     arg
                     (format-state->depth format-state))
             (format-state->delayed-propl format-state))))

     ;;
     ;;  Format an overlay
     ;;
     ((= format-letter ?$)
      (when arg
        (set-format-state->delayed-overlays
         format-state
         (cons (vector (length (format-state->result format-state))
                       nil
                       arg
                       (format-state->depth format-state))
               (format-state->delayed-overlays format-state)))))
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

     ;; A predicate
     ;; Get the predicate type and then parse the format string
     ;; accordingly

     ((= format-letter ?\?)
      (unless (string-match "[dbz+]" 
                            (format-state->format-string format-state)
                            (format-state->start format-state))
        (lyskom-error "Unknown predicate in format string %s (%d)"
                      (format-state->format-string format-state)
                      (format-state->start format-state)))
      (set-format-state->start format-state (match-end 0))
      (let ((predicate-type (elt (match-string 0
                                 (format-state->format-string format-state))
                                 0)))
        (cond 

         ;; Plural/singular predicate
         ;; arg is an integer. Use the first subformat if it is one
         ;; and the second if it is other than one.
         ((= predicate-type ?d)
          (setq format-state
                (lyskom-format-do-binary-predicate (= arg 1)
                                                   format-state 
                                                   allow-defer)
                result nil))

         ;; True/false predicate
         ((= predicate-type ?b)
          (setq format-state
                (lyskom-format-do-binary-predicate arg
                                                   format-state
                                                   allow-defer)
                result nil))

         ;; Zero/nonzero predicate
         ((= predicate-type ?z)
          (setq format-state
                (lyskom-format-do-binary-predicate (not (zerop arg))
                                                   format-state
                                                   allow-defer)
                result nil))

         ;; Non-negative predicate
         ((= predicate-type ?+)
          (setq format-state
                (lyskom-format-do-binary-predicate (not (lyskom-minusp arg))
                                                   format-state
                                                   allow-defer)
                result nil))
         )))

     ((= format-letter ?F)
      (set-format-state->delayed-propl
       format-state
       (cons (vector (length (format-state->result format-state))
                     `(lyskom-fill ((lambda (start end width)
                                      (let ((fill-column (or width (- (window-width 8)))))
                                        (fill-region start end))) . ,arg))
                     (format-state->depth format-state))
             (format-state->delayed-propl format-state))))

     ;;
     ;;  Format a conference or person name by retrieving information
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
              (let ((tmp (cache-get-uconf-stat arg)))
                (if (null tmp)
                    (let* ((format-element
			    (concat "%#2@%"
				    (if equals-flag 
                                        (make-string 1 equals-flag)
                                      "")
				    (if pad-length (int-to-string pad-length))
				    "#1"
				    (if colon-flag ":" "")
				    (char-to-string format-letter)))
			   (defer-info (lyskom-create-defer-info
					'get-uconf-stat
					arg
					'lyskom-deferred-insert-conf
					oldpos
					(if pad-length
                                            (cond ((eq equals-flag ?=) 
                                                   abs-length)
                                                  ((eq equals-flag ?<)
                                                   (min (length lyskom-defer-indicator)
                                                        abs-length))
                                                  (t (max (length lyskom-defer-indicator)
                                                          abs-length)))
                                          (length lyskom-defer-indicator))
					format-element
					lyskom-default-conf-string)))
		      (set-format-state->delayed-content
                       format-state
                       (cons defer-info
                             (format-state->delayed-content
                              format-state)))
                      lyskom-defer-indicator)

                  ;; The conf-stat was in the cache

                  (setq arg tmp)
                  (uconf-stat->name arg)
                  )))

             ;; The argument is an integer and we do not permit 
             ;; deferred printing
             ((integerp arg)
              (let ((conf-stat (blocking-do 'get-uconf-stat arg)))
                (if (null conf-stat)
                    (lyskom-format (if (= format-letter ?P)
                                       'person-does-not-exist
                                     'conference-does-not-exist)
                                   arg)
                  (uconf-stat->name conf-stat))))

             ;; We got a conf-stat, and can use it directly
             ((lyskom-conf-stat-p arg)
              (if face-flag
                  (let ((face (conf-stat-find-aux arg 9)))
                    (lyskom-maybe-add-face-to-string face
                                                     (conf-stat->name arg)))
                (conf-stat->name arg)))

             ;; Argument is a conf-z-info
             ((lyskom-conf-z-info-p arg)
              (conf-z-info->name arg))


             ;; We have an uconf-stat
             ((lyskom-uconf-stat-p arg)
              (uconf-stat->name arg))

             ;; Something went wrong
             (t (signal 'lyskom-internal-error
                        (list 'lyskom-format
                              ": argument error (expected conf)"
                              arg-no
                              arg)))))
      (if (and (not colon-flag)
               (or (lyskom-conf-stat-p arg)
                   (lyskom-uconf-stat-p arg)
                   (lyskom-conf-z-info-p arg)
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
                  ((lyskom-conf-z-info-p arg)
                   (int-to-string (conf-z-info->conf-no arg)))
                  ((lyskom-conf-stat-p arg) 
                   (int-to-string (conf-stat->conf-no arg)))
                  ((lyskom-uconf-stat-p arg)
                   (int-to-string (uconf-stat->conf-no arg)))
                  (t (signal 'lyskom-internal-error
                             (list 'lyskom-format
                                   ": argument error (expected conf)"
                                   arg-no
                                   arg)))))
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
                  ((numberp arg) 
                   (setq colon-flag t)
                   (format "%.0f" arg))
                  ((lyskom-text-stat-p arg) (int-to-string
                                             (text-stat->text-no arg)))
                  (t (signal 'lyskom-internal-error
                             (list 'lyskom-format
                                   ": argument error (expected text-no)"
                                   arg-no
                                   arg)))))
      (if (not colon-flag)
          (setq propl
                (append (lyskom-default-button 'text arg) propl))))
     ;;
     ;;  Format a subject line by adding the subject face to the text
     ;;  properties and the subject to the result list
     ;;
     ((= format-letter ?r)
      (setq result (cond ((stringp arg) 
                          (lyskom-format-plaintext-fonts arg)
                          (lyskom-button-transform-text arg))
                         ((consp arg) 
                          (lyskom-format-plaintext-fonts (cdr arg))
                          (lyskom-button-transform-text (cdr arg) (car arg)))
                         (t (signal 'lyskom-internal-error
                                    (list 'lyskom-format
                                          ": argument error (expected subject)"
                                          arg-no
                                          arg)))))
      (if (and (not colon-flag)
               (not (lyskom-face-default-p kom-subject-face)))
          (setq propl (append (list 'face kom-subject-face) propl))))
     ;;
     ;;  Format a LysKOM text body. Currently this does nothing. It
     ;;  should parse the text for buttons
     ;;
     ((= format-letter ?t)

      ;; +++ One would want to do this before or after, but then
      ;; buttons will not be visible and other highlighting will
      ;; disappear.

      ;; (if (not colon-flag)
      ;;     (setq propl (append (list 'face kom-text-face) propl)))

      (setq result
            (cond ((stringp arg) (lyskom-format-text-body arg))
                  ((and (consp arg)
                        (lyskom-text-stat-p (car arg)))
                   (lyskom-format-text-body (cdr arg)
                                            (car arg)))
                  (t (signal 'lyskom-internal-error
                             (list 'lyskom-format
                                   ": argument error (expected text)"
                                   arg-no
                                   arg))))))


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
			     (if equals-flag (make-string 1 equals-flag) "")
			     (if pad-length (int-to-string pad-length))
			     "#1"
			     (if colon-flag ":" "")
			     "s")))
		(set-defer-info->pos arg oldpos)

                ;; Note: length is right below. string-width is wrong.
		(set-defer-info->del-chars
		 arg (if pad-length
                         (cond ((eq equals-flag ?=) 
                                abs-length)
                               ((eq equals-flag ?<)
                                (min (length lyskom-defer-indicator)
                                     abs-length))
                               (t (max (length lyskom-defer-indicator)
                                       abs-length)))
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
          ((and (eq equals-flag ?<)
                (> abs-length (lyskom-string-width result))) nil)
          ((> abs-length (lyskom-string-width result))
           (let ((padstring (make-string (- abs-length (lyskom-string-width result))
                                         pad-letter)))
             (if (< pad-length 0)       ; LEFT justify
                 (progn
                   (setq prop-adjust-end (- (- abs-length (lyskom-string-width 
							   result))))
                   (setq result (concat result padstring)))
               (progn
                 (setq prop-adjust-start (- abs-length (lyskom-string-width result)))
                 (setq result (concat padstring result))))))
          ((and (memq equals-flag '(?= ?<))
                (< abs-length (lyskom-string-width result)))
           (setq result (lyskom-truncate-string-to-width result abs-length (and trailer "...")))))

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


(defun lyskom-format-do-binary-predicate (option format-state allow-defer)
  (cond (option
         (setq format-state
               (lyskom-format-enter-subformat format-state allow-defer))
         (lyskom-format-skip-subformat format-state))
        (t
         (lyskom-format-skip-subformat format-state)
         (setq format-state 
               (lyskom-format-enter-subformat format-state allow-defer))
         ))
  format-state)

(defun lyskom-format-enter-subformat (format-state allow-defer)
  "The format string should  be just before a subformat. Enter it."
  (unless (string-match "%\\[" 
                        (format-state->format-string format-state)
                        (format-state->start format-state))
    (lyskom-error "Predicate syntax error in format string %s (%d)"
                  (format-state->format-string format-state)
                  (format-state->start format-state)))
  (set-format-state->start format-state (match-end 0))
  (lyskom-format-aux format-state allow-defer))


(defun lyskom-format-skip-subformat (format-state)
  "Skip the subformat specification at the start of format-state"

  ;; Check that it looks like a subformat in the first place
  (unless (string-match "%\\[" 
                        (format-state->format-string format-state)
                        (format-state->start format-state))
    (lyskom-error "Predicate syntax error in format string %s (%d)"
                  (format-state->format-string format-state)
                  (format-state->start format-state)))
  (set-format-state->start format-state (match-end 0))

  ;; We are now inside the start of the subformat

  (let ((level 1))
    (while (> level 0)
      (if (null (string-match "\\(%\\[\\|%\\]\\)"
                              (format-state->format-string format-state)
                              (format-state->start format-state)))
          (lyskom-error "Bad nesting in format string %s (%d)" 
                        (format-state->format-string format-state)
                        (format-state->start format-state))
        (let ((ch (elt (match-string 
                        1 (format-state->format-string format-state)) 1)))
        (cond ((= ?\[ ch) (setq level (1+ level)))
              (t (setq level (1- level))))
        (set-format-state->start format-state (match-end 0)))))))


(defun lyskom-tweak-format-state (format-state) 
  (let ((dp (format-state->delayed-propl format-state)))
    (while dp
      (when (eq (format-state->depth format-state)
                (aref (car dp) 2))
        (add-text-properties (aref (car dp) 0)
                             (length (format-state->result format-state))
                             (aref (car dp) 1)
                             (format-state->result format-state))
        (set-format-state->delayed-propl 
         format-state
         (delq (car dp) (format-state->delayed-propl format-state))))
      (setq dp (cdr dp))))

  (lyskom-traverse overlay-spec (format-state->delayed-overlays format-state)
    (unless (or (aref overlay-spec 1)
                (not (eq (format-state->depth format-state)
                         (aref overlay-spec 3))))
      (aset overlay-spec 1 (length (format-state->result format-state)))))

  format-state)


;;; ================================================================
;;;			 Text body formatting


(lyskom-try-require 'latin-unity)
(lyskom-with-external-functions (smiley-region latin-unity-remap-region)

  (defun lyskom-format-text-body (text &optional text-stat)
    "Format a text for insertion. Does parsing of special markers in the text."
    (let* ((ct-item (and text-stat (car (text-stat-find-aux text-stat 1))))
           (content-type (or (cond (ct-item (aux-item->data ct-item))
                                   ((and (string-match "\\`\\(\\S-+\\):\\s-*$" text)
                                         (match-beginning 1))
                                    (match-string 1 text))
                                   (t nil))
                             "text/x-kom-basic"))
           (fn (and content-type
                    (cdr (let ((case-fold-search t))
                           (lyskom-traverse el lyskom-format-special
                             (when (eq 0 (string-match 
                                          (if (stringp (car el))
                                              (car el)
                                            (symbol-name (car el)))
                                          content-type))
                               (lyskom-traverse-break el)))))))
           (formatted (cond ((null fn) nil)
                            ((listp fn) 
                             (lyskom-traverse el fn
                               (let ((a (funcall el text text-stat)))
                                 (when a (lyskom-traverse-break a)))))
                            ((functionp fn)
                             (funcall fn text text-stat))
                            (t nil))))
      (or formatted text)))

  (defun lyskom-format-plaintext (text text-stat)
    (lyskom-format-plaintext-fonts text)
    (let ((tmp (if kom-text-properties
                   (lyskom-button-transform-text
                    (lyskom-fill-message text)
                    text-stat)
                 (lyskom-fill-message text))))
      (when (and kom-smileys (fboundp 'smiley-region))
        (add-text-properties 0 (length tmp) 
                             '(special-insert lyskom-postprocess-text) tmp))
      (when (fboundp 'latin-unity-remap-region)
        (add-text-properties 0 (length tmp)
                             '(special-insert lyskom-unity-text) tmp))
      tmp))

  (defun lyskom-postprocess-text (start end &rest args)
    (condition-case nil
        (smiley-region start (min (point-max) (1+ end)))
      (error nil)))

  (defvar latin-unity-preferred-coding-system-list)
  (defvar latin-unity-iso-8859-1-aliases)
  (defvar latin-unity-cset-codesys-alist)

  (defun lyskom-unity-text (start end &rest args)
    (condition-case nil
        (let ((codesys (car latin-unity-preferred-coding-system-list)))
          (when (memq codesys latin-unity-iso-8859-1-aliases)
            (setq codesys 'iso-8859-1))
          (let ((gr (or (car (rassq codesys latin-unity-cset-codesys-alist))
                        (and codesys
                             (eq (lyskom-coding-system-type codesys) 'iso2022)
                             (lyskom-coding-system-property codesys 'charset-g1)))))
            (when gr
              (latin-unity-remap-region start (min (point-max) (1+ end))
                                        gr nil t))))
      (error nil))))


(defmacro lyskom-format-plaintext-fonts-body ()
  "Internal macro for lyskom-format-plaintext-fonts"
  (let* ((re-x 0)
         (re-1 "[^\]\|\}\) ^\n\r\t%s]")
         (re-2 "[^^\|<>=%%$\r\n%s]")
         (re-3 '(("_" (prefix . "") (suffix . "") (face . italic))
                 ("*" (prefix . "") (suffix . "") (face . bold))
                 ("/" (prefix . "\\s-") (suffix . "[\].,;:-_!\"#$%&\(\)\[\{\}=+?' \t\r\n]") (face . italic))))
         (re (mapconcat (lambda (el)
                          (let ((x (regexp-quote (car el))))
                            (format "%s\\(%s%s\\(%s*%s\\)?%s\\)%s"
                                    (cdr (assq 'prefix (cdr el)))
                                    x
                                    (format re-1 x)
                                    (format re-2 x)
                                    (format re-1 x)
                                    x
                                    (cdr (assq 'suffix (cdr el)))
                                    )))
                        re-3
                        "\\|")))
    `(let ((start 0))
       (while (string-match ,re text start)
         (cond ,@(mapcar (lambda (el)
                           (prog1
                               `((match-beginning ,(+ 1 (* re-x 2)))
                                 (add-text-properties (1+ (match-beginning ,(+ 1 (* re-x 2))))
                                                      (1- (match-end ,(+ 1 (* re-x 2))))
                                                      '(face ,(cdr (assq 'face (elt re-3 re-x))))
                                                      text))
                             (setq re-x (1+ re-x))))
                         re-3))
         (setq start (1- (match-end 0)))))))

(defun lyskom-format-plaintext-fonts (text)
  (when kom-fontify-text
    (lyskom-format-plaintext-fonts-body)))


(defun lyskom-signal-reformatted-text (how)
  "Signal that the last text was reformatted HOW, which should be a string
in lyskom-messages."
  (or (memq how lyskom-last-text-format-flags)
      (setq lyskom-last-text-format-flags (cons how lyskom-last-text-format-flags))))


(lyskom-with-external-functions (w3m-region)
  (defun lyskom-w3m-region (start end &rest args)
    (lyskom-render-html-region start end 'w3m-region)))

(lyskom-with-external-functions (w3-region)
  (defun lyskom-w3-region (start end &rest args)
    (lyskom-render-html-region start end 'w3-region)))

(defun lyskom-render-html-region (start end fun)
  (unwind-protect
      (condition-case var
          (save-restriction
            (let ((buffer-read-only nil))
              (setq start (set-marker (make-marker) start))
              (setq end (set-marker (make-marker) end))
              (narrow-to-region start end)
              (when kom-w3-simplify-body
                (save-excursion
                  (let ((case-fold-search t))
                    (goto-char start)
                    (while (re-search-forward "<body[^>]*>" end t)
                      (replace-match "<body>")))))
              (funcall fun start end)
              (add-text-properties (point-min) (point-max) '(end-closed nil))
	      (set-marker start nil)
	      (set-marker end nil)))
        (error (lyskom-ignore var)))))


(defun lyskom-format-html-plaintext (text text-stat)
  (lyskom-button-transform-text text text-stat))

(defun lyskom-format-html (text text-stat package function)
  ;; Find settings for this author
  (let ((author-setting (and text-stat (or (assq (text-stat->author text-stat)
                                                 kom-format-html-authors)
                                           (assq t kom-format-html-authors)))))
    (when (and (cdr author-setting)
               (condition-case e (progn (require package) t) (error nil)))
      (add-text-properties 0 (length text) `(special-insert ,function) text)
      (lyskom-signal-reformatted-text 'reformat-html)
      (if (string-match "^html:" text)
          (substring text 5)
        text))))

(defun lyskom-format-html-w3 (text text-stat)
  (lyskom-format-html text text-stat 'w3 'lyskom-w3-region))

(defun lyskom-format-html-w3m (text text-stat)
  (lyskom-format-html text text-stat 'w3m 'lyskom-w3m-region))

(defun lyskom-format-image (text text-stat)
  (if kom-format-show-images
      (let* ((cti (lyskom-get-aux-item (text-stat->aux-items text-stat) 1))
	     (content-type (and cti (aux-item->data (car cti))))
	     (msg "")
	     (imagetype (intern 
			 ; FIXME: Can the media type contain more than letters and -?
			 (string-replace-match "^.*/\\([-a-zA-Z]*\\)\\(.\\|\n\\)*" 
					       content-type "\\1")))
	     (imagedata text))
	(condition-case nil
	    (lyskom-xemacs-or-gnu
	     (set-extent-end-glyph ; XEmacs
	      (make-extent (point) (point-max))
	      (make-glyph
	       (make-image-instance (vector imagetype :data imagedata))))
	     ; GNU Emacs
	     (unless (and (lyskom-display-images-p)
			  (lyskom-put-image 
			   (lyskom-create-image imagedata imagetype t) 
			   (point-max)))
	       (setq msg (lyskom-get-string 'image-no-show))))
	       ; Errors just marks it as a no show
	  (error (setq msg (lyskom-get-string 'image-no-show)))) 
	(lyskom-signal-reformatted-text 'reformat-image)
	msg)))

(defun lyskom-format-enriched (text text-stat)
  (if (not (fboundp 'format-decode-buffer))
      nil
    (let ((tmpbuf (lyskom-generate-new-buffer "lyskom-enriched")))
      (unwind-protect
          (save-excursion
            (set-buffer tmpbuf)
            (if (lyskom-get-aux-item (text-stat->aux-items text-stat) 10002)
                (insert (aux-item->data (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 10002))))
              (insert "Content-Type: text/enriched\n\n"))
            (if (lyskom-get-aux-item (text-stat->aux-items text-stat) 1)
                (insert text)
              (insert (substring text 10)))
            (format-decode-buffer)
            (lyskom-signal-reformatted-text 'reformat-enriched)
            (lyskom-button-transform-text (buffer-string) text-stat)
            ;; (substring (buffer-string) 0 -1) ; Remove the \n
            )
        (kill-buffer tmpbuf)))))

(defun lyskom-format-ö (text text-stat)
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
               (lyskom-button-transform-text (buffer-string) text-stat)
             (buffer-substring (point-min) (1- (point-max))))))))

(defun lyskom-get-holerith-list (text &optional no-coding)
  "Assume that TEXT is a list of holerith strings. Return those strings."
  (let (result tmp)
    (while (string-match "\\S-" text)
      (setq tmp (lyskom-get-holerith text no-coding))
      (setq result (cons (car tmp) result)
            text (cdr tmp)))
    (nreverse result)))

(defun lyskom-get-holerith-assoc (text &optional no-coding)
  "Assume that TEXT is a list of holerith strings. Return those strings."
  (let (result tmp key value)
    (while (string-match "\\S-" text)
      (setq tmp (lyskom-get-holerith text no-coding))
      (setq key (car tmp) text (cdr tmp))
      (setq tmp (lyskom-get-holerith text no-coding))
      (setq value (car tmp) text (cdr tmp))
      (setq result (cons (cons key value) result)))
    (nreverse result)))

(defun lyskom-split-user-area (text)
  "Return the user area split into components."
  (let ((tmp (lyskom-get-holerith text t)))
    (lyskom-mapcar2 'cons  
             (lyskom-get-holerith-list (car tmp) t)
             (lyskom-get-holerith-list (cdr tmp) t))))

(defun lyskom-format-x-kom/user-area-data (data)
  (let* ((values (lyskom-get-holerith-assoc data t))
         (maxlen (apply 'max (mapcar (lambda (el) (length (car el))) values)))
         (fs (format "%%-%ds%%s" (+ maxlen 2))))
    (mapconcat (lambda (el) (format fs
                                    (concat (car el) ": ")
                                    (cdr el))) values "\n")))


(defun lyskom-format-x-kom/user-area (text text-stat)
;  (condition-case nil
      (let ((result "")
            (user-area (lyskom-split-user-area text)))
        (while user-area
          (let ((key (car (car user-area)))
                (data (cdr (car user-area))))
            (setq user-area (cdr user-area))
            (setq result
                  (concat result
                          key "\n" (make-string (length key) ?=) "\n\n"
                          (cond ((equal key "common")
                                 (lyskom-format-x-kom/user-area-data data))
                                ((equal key "elisp")
                                 (lyskom-format-x-kom/user-area-data data))
                                (t data))
                          (if user-area "\n\n\n" "")))))
        result)


;    (error (concat "Unable to parse user area\n\n" text)))
)



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

(defconst lyskom-minimum-triangle-size 3
  "Minimum number of lines in a triangle or suchlike.")

(defconst lyskom-minimum-brick-size 2
  "Minimum number of lines in a brick.")

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
      (condition-case nil
          (fill-region start (min end (point-max)) justify nosqueeze to-eop)
        (error nil)))))

(defvar lyskom-fill-disqualifying-regexp
  (let ((res '((triple-whitespace "\\S-[ \t][ \t][ \t]+\\S-")
               (space-tab "\\S-[ \t]* \t[ \t]*\\S-")
               (tab-space "[ \t]*\t [ \t]*")
               (tab-tab "\\S-\\s-*\t\t\\s-*\\S-")
               (multi-dash "----")
               (log-line "[a-zA-Z0-9.]+\\[[0-9]+\\]")
               (c-comment-start "/\\*")
               (c-comment-end "\\*/")
               (line-comment "[^:]//"))))
    (concat "\\("
            (mapconcat (lambda (el) (car (cdr el))) res "\\|")
            "\\)")))


(defmacro lyskom-fill-message-next-line ()
  `(setq next-line t))

(defmacro lyskom-fill-message-return ()
  `(throw 'lyskom-fill-message-return t))

(defsubst lyskom-fill-message-line-length ()
  (- (save-excursion (end-of-line)
                     (skip-chars-backward " \t")
                     (current-column)) (current-column)))

(defmacro lyskom-maybe-fill-region (start end &optional justify nosqueeze to-eop)
  "Fill the region from START to END if the prerequisites for doing
so are met. This macro checks on the internal state of 
lyskom-fill-next-paragraph to see if the text scanned should be filled
or not."
  `(unless (or (<= ,end ,start)
               (<= paragraph-width fill-column)
               quoted-paragraph
               special-characters
               (eq all-colon-lines t)
               (and geometric-text
                    (or (and (eq 0 length-difference)
                             (>= paragraph-length lyskom-minimum-brick-size))
                        (and (not (eq 0 length-difference))
                             (>= paragraph-length lyskom-minimum-triangle-size)))))
     (let ((fill-prefix fill-prefix))
       (when indented-paragraph
         (setq fill-prefix
               (concat (or fill-prefix "")
                       indented-paragraph)))
       (when bulleted-paragraph
         (setq fill-prefix
               (concat (or fill-prefix "")
                       (make-string (length bulleted-paragraph) ?\ ))))
       (lyskom-fill-region ,start ,end ,justify ,nosqueeze ,to-eop)
       (lyskom-signal-reformatted-text 'reformat-filled)
       t)))

(defvar lyskom-fill-inline-text-regexp 
  (concat "^\\("
          (mapconcat (lambda (language)
                       (regexp-quote (lyskom-get-string
                                      'subject nil (car language))))
                     lyskom-languages
                     "\\|")
          "\\).*\n----"))


(defvar lyskom-fill-cancel)
(defun lyskom-fill-message (text)
  (cond 
   ((null kom-autowrap) text)
   ((and (numberp kom-autowrap) (> (length text) kom-autowrap)) text)
   (t (save-excursion
        (set-buffer (lyskom-get-buffer-create 'lyskom-text " lyskom-text" t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))

        (let* ((fill-column 
                (cond ((not (integerp fill-column)) (- (window-width) 5))
                      ((> fill-column (- (window-width) 5)) (- (window-width) 5))
                      (t fill-column)))
               (fill-prefix nil)
               (lyskom-fill-cancel (cons nil nil))
               (timer (and kom-autowrap-timeout
                           (run-at-time kom-autowrap-timeout nil
                                        (lambda (obj) (setcdr obj t)) lyskom-fill-cancel))))
          (condition-case nil
              (while (not (eobp)) (lyskom-fill-next-paragraph))
            (quit nil)
            (error nil)))

        (goto-char (point-max))
        (skip-chars-backward " \t\r\n")

        (let* ((pos (1- (point-max))))
          (while (and (> pos 0) 
                      (progn (goto-char pos)
                             (looking-at "[ \t\n\r]")))
            (setq pos (1- pos)))
          (buffer-substring (point-min) (1+ pos)))))))


(defun lyskom-fill-next-paragraph ()
  "Scan and possibly fill the next paragraph in the current buffer."
  (let* ((start (point))
         (next-char nil)
         (paragraph-length 0)
         (paragraph-width 0)
         (eol-point 0)
         (next-line nil)

         ;; Variables used for geometric texts
         (geometric-text t)
         (current-line-length nil)
         (last-line-length nil)
         (length-difference nil)

         ;; Variables used for special characters
         (special-characters nil)

         ;; Variables used for quoted paragraphs
         (quoted-paragraph nil)

         ;; Variables used for bulleted paragraphs
         (bulleted-paragraph nil)

         ;; Variables used for indented paragraphs
         (indented-paragraph nil)

         ;; Variables used for MIME headers
         (all-colon-lines 'undef)
         )

    ;;
    ;; The end of a paragraph is signalled by throwing to
    ;; here. This simplifies the conditions within the code
    ;; quite considerably.
    ;;

    (catch 'lyskom-fill-message-return

      ;;
      ;; Loop until the end of the buffer
      ;;

      (while (not (eobp))
        (setq next-line nil)

        ;;
        ;; Code can throw to here to prevent further processing
        ;; of a line of input. This is used when a line has been
        ;; completely categorized.
        ;;
        ;; FIXME: Paragraph-width is only used when starting to fill a paragraph, so we should
        ;; FIXME: compute it only at that point. 
        ;;
        ;; FIXME: If we can move the special patterns check, we can lose eol-point as well, and
        ;; FIXME: only compute it when it's needed to calculate paragraph-width.

        (setq next-char (char-after (point)))
        (setq eol-point (save-excursion (end-of-line) (point)))
        (setq paragraph-width (max paragraph-width (- eol-point (point))))

        ;;
        ;; Handle unconditional paragraph break (empty line)
        ;;
        ;; An empty line signals the end of the paragraph, always.
        ;;

        (when (looking-at "^\\s-*$")
          (lyskom-maybe-fill-region start (1- (point)) nil t)
          (lyskom-fill-scan-empty-lines)
          (lyskom-fill-message-return))


        ;;
        ;; Detect special patterns
        ;;
        ;; Here, we look for lines that have patterns that disqualify
        ;; the entire paragraph from being filled. This should catch
        ;; source code and many preformatted things.
        ;;
        ;; FIXME: It is probably possible to do this once when starting
        ;; FIXME: to fill the text. Alternately, we could scan forward
        ;; FIXME: for a para break as soon as we spot this.
        ;; FIXME:
        ;; FIXME: I suspect that indented paragraphs may trigger this
        ;; FIXME: rule.

        (when (save-excursion 
                (re-search-forward lyskom-fill-disqualifying-regexp eol-point t))
          (setq special-characters t))

        (when (looking-at lyskom-fill-inline-text-regexp)
          (setq special-characters t))


        ;;
        ;; Handle unconditional paragraph break (bulleted text)
        ;;
        ;; Bullet items end the current paragraph unconditionally.
        ;; If we have seen text before the item, bail out of here,
        ;; causing the same line to be revisited the next time
        ;; the function is called. Otherwise, keep scanning, but
        ;; set a flag.
        ;;

        (when (and (not next-line) 
                   (looking-at "^\\s-*\\([-*+#]\\|[§#]?\\s-*[0-9]+[.):]?\\)\\s-*"))
          (when (lyskom-maybe-fill-region start (1- (point)) nil t)
            (lyskom-fill-message-return))
          (setq bulleted-paragraph (match-string 0))
          (lyskom-fill-message-next-line))

        ;;
        ;; Handle indented paragraph
        ;;
        ;; - It can be a continuation of a bullet point
        ;; - It can be a continuation of an indented para
        ;; - It can terminate a non-indented para
        ;; - It can continue an all-colon line
        ;;

        (when (and (not next-line) (looking-at "^\\(\\s-+\\)\\S-"))
          (cond (bulleted-paragraph (lyskom-fill-message-next-line))
                ((equal (match-string 1) indented-paragraph)
                 (lyskom-fill-message-next-line))
                ((eq all-colon-lines t) (lyskom-fill-message-next-line))
                ((lyskom-maybe-fill-region start (1- (point)) nil t)
                 (lyskom-fill-message-return))
                (t (setq indented-paragraph (match-string 1)))))

        ;;
        ;; If we have seen indentation, but don't see it now,
        ;; then this isn't an indented paragraph. It's just
        ;; a paragraph with first-line indentation.
        ;;

        (when (and (not next-line) indented-paragraph (looking-at "^\\S-"))
          (setq indented-paragraph nil))


        ;;
        ;; Handle unconditional paragraph break (quoted text)
        ;;
        ;; Quoted text also results in an unconditional paragraph
        ;; break. This must come after handling of indented lines
        ;; since whitespace is not considered a legal line start
        ;; character.
        ;;

        (when (and (not next-line)
                   (not (looking-at "^\\s-"))
                   (< (lyskom-char-to-int next-char)
                      (length lyskom-line-start-chars))
                   (not (aref lyskom-line-start-chars 
                              (lyskom-char-to-int next-char))))
          (lyskom-maybe-fill-region start (1- (point)) nil t)
          (lyskom-fill-scan-quoted-paragraph next-char)
          (lyskom-fill-message-return))

        ;;
        ;; Handle header-like stuff
        ;;
        ;; This rule detects mail-header-like stuff
        ;;

        (if (and (not next-line) 
                 all-colon-lines
                 (looking-at "^\\S-+\\s-*:"))
            (setq all-colon-lines t)
          (setq all-colon-lines nil))

        ;; Handle geometric paragraphs
        ;;
        ;; Geometric paragraphs have constant differences in line
        ;; length. There is also a requirement of minimum size,
        ;; which is enforced in lyskom-maybe-fill-region
        ;;

        (when (and (not next-line) geometric-text)
          (setq current-line-length (lyskom-fill-message-line-length))
          (cond ((and (null length-difference)
                      current-line-length
                      last-line-length)
                 (setq length-difference (- current-line-length last-line-length)))

                ((and current-line-length
                      last-line-length
                      (/= (- current-line-length last-line-length)
                          length-difference))
                 (setq geometric-text nil))))





        (setq last-line-length current-line-length)
        (end-of-line)
        (setq paragraph-length (1+ paragraph-length))
        (unless (eobp)
          (forward-line 1)
          (beginning-of-line)
          (when kom-autowrap-timeout 
            (sit-for 0)
            (and (cdr lyskom-fill-cancel) (error "Out of time"))))

        ) ;; while

      (lyskom-maybe-fill-region start (point) nil t)

      ) ;; catch lyskom-fill-message-return

    ))


(defun lyskom-fill-scan-quoted-paragraph (char)
  (while (and (eq (char-after (point)) char) (not (eobp)))
    (end-of-line)
    (unless (eobp)
      (forward-line 1)
      (beginning-of-line))))

(defun lyskom-fill-scan-empty-lines ()
  (while (and (looking-at "^\\s-*$") (not (eobp)))
    (end-of-line)
    (unless (eobp)
      (forward-line 1)
      (beginning-of-line))))


;;; ============================================================
;;;                     Beeping and feeping
;;;                     Faces and colors
;;;

(defun lyskom-beep (arg &optional optarg)
  "Beep. ARG is how to beep. Optional OPTARG is a modifier. 
nil means don't beep.
t means beep once.
A number means beep that number of times (.1 second delay between beeps).
A string means start the command kom-audio-player with the string as argument.
A symbol other than t means call it as a function.
A list of pairs means OPTARG will be used as a key to look up the real
  value of ARG. The special key t is used when OPTARG is not found."
  (setq optarg
	(cond ((lyskom-conf-stat-p optarg) (conf-stat->conf-no optarg))
	      ((lyskom-uconf-stat-p optarg) (uconf-stat->conf-no optarg))
	      (t optarg)))

  (cond ((null arg))
        ((eq t arg) (ding t))
        ((numberp arg) (while (> arg 0)
                         (ding t)
                         (sit-for kom-ding-pause-amount)
                         (setq arg (1- arg))))
        ((stringp arg) (condition-case nil
                           (start-process "audio" nil kom-audio-player arg)
                         (error nil)))
        ((and (symbolp arg) (fboundp arg))
         (condition-case nil
             (funcall arg)
           (error (message "Error in beep function")
                  (beep))))
        ((and (listp arg)
              (or (assq optarg arg)
                  (assq t arg)))
         (lyskom-beep (cdr (or (assq optarg arg)
                               (assq t arg)))
                      optarg))
        (t (beep))))

(defun lyskom-face-default-p (f1)
  "Return t if f1 is undefined or the default face."
  (not (and (lyskom-find-face f1)
            (face-differs-from-default-p f1))))

;;;; ================================================================
;;;;                         Running in buffer 

;;; Author: Linus


(defun lyskom-text-at-point ()
  "Return the text that point is in, or nil it is impossible to determine."
  (save-excursion
    (lyskom-prev-area 1 'lyskom-text-start)
    (get-text-property (point) 'lyskom-text-start)))

(defun kom-next-prompt (num)
  "Move the cursor to the next prompt in the LysKOM buffer"
  (interactive "p")
  (lyskom-next-area num 'lyskom-prompt))


(defun kom-prev-prompt (num)
  "Move the cursor to the previous prompt in the LysKOM buffer"
  (interactive "p")
  (beginning-of-line)
  (lyskom-prev-area num 'lyskom-prompt))
  


(defun backward-text (&optional arg)
  "Searches backwards for a text start and recenters with that text at the top."
  (interactive "p")
  (lyskom-prev-area (or arg 1) 'lyskom-text-start t)
  (beginning-of-line))

(defun forward-text (&optional arg)
  "Searches forward for a text start and recenters with that text at the top."
  (interactive "p")
  (lyskom-next-area (or arg 1) 'lyskom-text-start t))


(def-kom-command kom-save-text-body (text-no &optional filename)
  "Save the body of the selected text to a file. This command saves
the contents of the text, without LysKOM information around it to a
file, overwriting any previous contents.

See `kom-save-text' for an alternative command."
  (interactive (list (lyskom-read-text-no-prefix-arg 'what-save-no)
                     nil))
  (cond (text-no
         (blocking-do-multiple ((text-stat (get-text-stat text-no))
                                (text (get-text text-no)))
           (if (or (null text-stat) (null text))
               (lyskom-format-insert 'no-such-text-no text-no)
             (let* ((mx-filename (lyskom-get-aux-item (text-stat->aux-items text-stat) 10104))
                    (filename  nil))
               (while (null filename)
                 (setq filename
                       (read-file-name (lyskom-format 'save-text-to-file-q text-no)
                                       (and mx-filename
                                            (file-name-directory (aux-item->data (car mx-filename))))
                                       nil
                                       nil
                                       (and mx-filename
                                            (aux-item->data (car mx-filename)))))
                 (if (file-directory-p filename)
                     (setq filename nil)
                   (if (or (not (file-exists-p filename))
                           (prog1 
                               (lyskom-j-or-n-p
                                (lyskom-format 'save-text-confirm filename))
                             (lyskom-message "")))
                       (let ((buf (lyskom-get-buffer-create 'temp " *kom*-text" t))
                             (str (text->decoded-text-mass text text-stat)))
                         (condition-case nil
                             (progn
                               (lyskom-format-insert 'saving-text text-no filename)
                               (when (string-match "\n" str)
                                 (setq str (substring str (match-end 0))))
                               (save-excursion
                                 (set-buffer buf)
                                 (erase-buffer)
                                 (insert str)
				 (let* ((cti (lyskom-get-aux-item (text-stat->aux-items text-stat) 1))
					(content-type (and cti (aux-item->data (car cti))))
					(charset (cdr (assoc 'charset 
							     (cdr (lyskom-mime-decode-content-type 
								   content-type)))))
					(coding-system-for-write (or charset
								     lyskom-server-coding-system)))
                                 (write-region (point-min) (point-max) filename)))

                               (lyskom-insert (lyskom-get-string 'done)))
                           (quit (lyskom-insert (lyskom-get-string 'cancelled)))
                           (error (lyskom-insert (lyskom-get-string 'nope))))))))))))
        (t (lyskom-insert 'confusion-what-to-save))))


(def-kom-command kom-save-text (arg &optional list-of-texts filename)
  "Append the selected texts to a file. The texts to save are
determined by searching backwards in the buffer. A numeric prefix
argument specifies the number of texts to save. Headers are included
in the file.

The file name to save to is read using the minibuffer, and the default
is taken from kom-saved-file-name the first time. Subsequent calls use
the most recently specified file name.

See `kom-save-text-body' for an alternative to this command."
  (interactive "p")
  (let ((name nil))
    (save-excursion
      (while (and arg (> arg 0))
        (backward-text 1)
        (if (looking-at "\\([0-9]+\\)\\s-")
            (setq list-of-texts (cons (string-to-int (match-string 1))
                                      list-of-texts)
                  arg (1- arg))
          (setq arg 0)))
      (setq name (expand-file-name
                  (or filename
                      (read-file-name
                       (if (eq 1 (length list-of-texts))
                           (lyskom-format 'save-one-on-file-q (car list-of-texts))
                         (lyskom-format 'save-many-on-file-q (length list-of-texts)))
                       (file-name-directory (or lyskom-saved-file-name kom-saved-file-name))
                       nil
                       nil
                       (file-name-nondirectory (or lyskom-saved-file-name kom-saved-file-name))))))
      (cond ((file-directory-p name)
             (lyskom-format-insert 'cant-save-to-directory name))
            (t 
             (set-buffer lyskom-buffer)
             (if (eq 1 (length list-of-texts))
                 (lyskom-format-insert-before-prompt 'saving-one-on-file 
                                       (car list-of-texts)
                                       name)
               (lyskom-format-insert-before-prompt 'saving-many-on-file
                                     (length list-of-texts)
                                     name))
             (mapcar (lambda (n)
                       (blocking-do-multiple ((text-stat (get-text-stat n))
                                              (text (get-text n)))
                         (lyskom-save-text text-stat text name)))
                     list-of-texts)
             (setq lyskom-saved-file-name name))
            ))))

(defun lyskom-save-text (text-stat text filename)
  "Save text in TEXT-STAT and TEXT to FILENAME."
  (let* ((kom-print-relative-dates nil)
         (kom-deferred-printing nil)

         ;; This has to come last
         (buf (lyskom-get-buffer-create 'temp "*kom*-text" t)))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (lyskom-view-text (text-stat->text-no text-stat))
      (append-to-file (point-min) (point-max) filename))))



;;; ================================================================
;;;                             To-do


(defvar lyskom-recursive-prompt-update nil)
(defun lyskom-update-all-prompts (&optional force-prompt-update)
  "Update the prompts in all buffers"
  (unless lyskom-recursive-prompt-update
    (let ((lyskom-recursive-prompt-update t))
      (save-excursion
        (lyskom-traverse buffer lyskom-buffer-list
          (set-buffer buffer)
          (lyskom-update-prompt force-prompt-update))
        (lyskom-set-default 'lyskom-need-prompt-update nil)))))

(defun lyskom-update-prompt (&optional force-prompt-update)
  "Print prompt if the client knows which command will be default.
Set lyskom-current-prompt accordingly. Tell server what I am doing."
  (let ((was-at-max (eq (point) (point-max)))
        (saved-suffix (if lyskom-slow-mode
                          (save-excursion (lyskom-get-entered-slow-command))
                        nil)))
    (if (or lyskom-executing-command
            (and lyskom-current-prompt
                 lyskom-dont-change-prompt))
        nil
      (let ((to-do (lyskom-what-to-do))
            (prompt nil)
            (prompt-args nil))
        (setq lyskom-command-to-do to-do)
        (cond

         ((eq to-do 'next-pri-conf)
          (setq prompt
                (cond ((eq lyskom-pers-no
                           (conf-stat->conf-no
                            (read-info->conf-stat (read-list->first
                                                   lyskom-to-do-list))))
                       'go-to-pri-mailbox-prompt)
                      (t 'go-to-pri-conf-prompt)))

          (or (eq lyskom-current-prompt prompt)
              (lyskom-beep kom-ding-on-priority-break)))

         ((eq to-do 'reedit-text)
          (setq prompt 're-edit-text-prompt))

         ((and (eq to-do 'next-pri-session)
               (lyskom-get-prioritized-session))
          (if (and (read-list-isempty lyskom-reading-list)
                   (read-list-isempty lyskom-to-do-list))
              (setq prompt 'next-unread-session-prompt)
            (setq prompt 'next-pri-session-prompt))
          (setq prompt-args 
                (save-excursion
                  (set-buffer (lyskom-get-prioritized-session))
                  (list
		   (lyskom-session-nickname)))))

         ((eq to-do 'next-pri-text)
          (setq prompt
                (cond ((eq (conf-stat->conf-no
                            (read-info->conf-stat (read-list->first
                                                   lyskom-to-do-list)))
                           lyskom-pers-no)
                       'read-pri-letter-prompt)
                      (t 'read-pri-text-conf)))

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
                   ((or (eq 'REVIEW-FAQ (read-info->type read-info))
                        (eq 'REVIEW-FAQ-TREE (read-info->type read-info)))
                    'review-next-faq-prompt)
                   ;; The following is not really correct. The text to be
                   ;; read might be in another conference.
                   ((= lyskom-current-conf lyskom-pers-no)
                    'read-next-letter-prompt)
                   ((lyskom-get-aux-item (text-stat->aux-items
                                          (cache-get-text-stat
                                           (car-safe
                                            (text-list->texts
                                             (read-info->text-list
                                              read-info)))))
                                         10100)
                    'read-next-attachment-prompt)
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
                 ((eq 'REVIEW
                      (read-info->type (read-list->first lyskom-to-do-list)))
                  'go-to-conf-of-review-prompt)
                 ((eq 'REVIEW-FAQ
                      (read-info->type (read-list->first lyskom-to-do-list)))
                  'go-to-conf-of-review-faq-prompt)
                 ((eq 'REVIEW-FAQ-TREE
                      (read-info->type (read-list->first lyskom-to-do-list)))
                  'go-to-conf-of-review-faq-prompt)
                 ((eq 'REVIEW-TREE 
                      (read-info->type (read-list->first lyskom-to-do-list)))
                  'go-to-conf-of-review-tree-prompt)
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
                   ((lyskom-command-name command lyskom-language))
                   ((and (stringp command)
                         (lyskom-command-name (key-binding command) lyskom-language)))
                   (t (lyskom-format 'the-command command))))))
     
         ((eq to-do 'unknown)		;Pending replies from server.
          (setq prompt nil))
     
         (t (message "%s" to-do)
            (setq prompt "???")))

        (when (or force-prompt-update
                  (not (equal prompt lyskom-current-prompt)))
          (let ((inhibit-read-only t)
                (prompt-text
                 (if prompt
                     (lyskom-modify-prompt
                      (apply 'lyskom-format
                             (cond
                              ((symbolp prompt) (lyskom-get-string prompt))
                              (t prompt))
                             prompt-args))
                   "")))
            (save-excursion
              ;; Insert the new prompt
              (goto-char (point-max))
              (beginning-of-line)
              (lyskom-xemacs-or-gnu
               (let ((extent 
                      (make-extent 0 (length prompt-text)
                                   prompt-text)))
                 (set-extent-property extent 'read-only t)
                 (set-extent-property extent 'rear-nonsticky t)
                 (set-extent-property extent 'duplicable nil))

               (add-text-properties 0 (length prompt-text)
                                    '(read-only t rear-nonsticky t)
                                    prompt-text))
              (insert prompt-text)
              ;; Delete the old prompt
              (when lyskom-current-prompt
                (if (and lyskom-slow-mode 
                         (looking-at 
                          (regexp-quote lyskom-current-prompt-text)))
                    (delete-region (point) (match-end 0))
                  (delete-region (point) (point-max))
                  (if saved-suffix (insert saved-suffix)))))
            (when was-at-max
              (goto-char (point-max)))

            (setq lyskom-current-prompt prompt)
            (setq lyskom-current-prompt-args prompt-args)
            (setq lyskom-current-prompt-text prompt-text))))
      (lyskom-set-mode-line))))

(defun lyskom-modify-prompt (s &optional executing)
  (let ((text (lyskom-format-prompt (cond (lyskom-is-administrator
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
                                    s)))
    (add-text-properties 0 (length text)
			 (append
			  (lyskom-default-button
			   'timestamp lyskom-current-prompt-timestamp
			   (list 'timestamp-popup-title
				 (let ((kom-print-relative-dates nil))
                                   (lyskom-format-time
                                    'date-and-time
                                    lyskom-current-prompt-timestamp))))
			  '(lyskom-prompt t))
			 text)
    (when executing (setq lyskom-current-prompt-timestamp (lyskom-current-client-time)))
    text))
           


(defun lyskom-format-prompt (fmt command)
  (let ((start 0)
        (len (length fmt))
        (result nil)
        (tmp nil)
        (format-letter nil)
        (messages (length lyskom-ansaphone-messages)))
    (while (< start len)
      (setq tmp (string-match "%[][cm Sswp#aA]" fmt start))
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
			  (lyskom-session-nickname))
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

(defun lyskom-get-prioritized-session ()
  "Get the session to go to if we are doing an auto-goto-session"
  (let ((session-list (if (memq kom-server-priority-breaks
				  '(express-letters letters
						    after-conf-letters))
                          lyskom-sessions-with-unread-letters
                        lyskom-sessions-with-unread))
        (session nil)
        (saved-priority nil))
    (while session-list
      (condition-case nil
          (save-excursion
            (set-buffer (car session-list))
            (when (or (null saved-priority)
                      (> kom-server-priority saved-priority))
              (setq session (car session-list)
                    saved-priority kom-server-priority)))
        (error nil))
      (setq session-list (cdr session-list)))
    session))


    
    

(defun lyskom-what-to-do ()
  "Check what is to be done. Return an atom as follows:
	next-pri-text	There is a text with higher priority to be read.
	next-pri-conf	There is a conference with higher priority to be read.
	next-text	There are texts on lyskom-reading-list.
	next-conf	There are texts on lyskom-to-do-list.
        reedit-text     There is an edit buffer with an error.
	when-done	There are no unread texts.
        next-pri-session There is a session with unreads.
	unknown	        There are pending replies."
  (cond

   ;; If session breaks are one ... and ...
   ;;   there is a session with higher priority ... and ...
   ;;   it's not the current session ... and ...
   ;;   we either have express breaks or we're at the end of a comment chain
   ;;   the priority of the other session is higher than what we're reading

   ((let* ((pri-session (lyskom-get-prioritized-session))
           (type (unless (read-list-isempty lyskom-reading-list)
                   (read-info->type (read-list->first lyskom-reading-list))))
           (pri (unless (read-list-isempty lyskom-reading-list) 
                  (read-info->priority (read-list->first lyskom-reading-list))))
           (pri-session-pri (save-excursion (when (and pri-session pri)
                                              (set-buffer pri-session)
                                              kom-server-priority))))
      (and (not (eq pri-session (current-buffer)))
           (or (and (memq kom-server-priority-breaks
			    '(express express-letters))
                    pri-session
                    (or (null pri) (> pri-session-pri pri)))
               (and (eq kom-server-priority-breaks 'when-done)
                    pri-session
                    (read-list-isempty lyskom-reading-list)
                    (read-list-isempty lyskom-to-do-list))
               (and (memq kom-server-priority-breaks '(t letters))
                    pri-session
                    (or (null type)
                        (eq type 'CONF)
                        (eq type 'REVIEW)
                        (eq type 'REVIEW-FAQ)
                        (eq type 'REVIEW-MARK))
                    (or (null pri) (> pri-session-pri pri)))
               (and (memq kom-server-priority-breaks
			    '(after-conf after-conf-letters))
                    pri-session
                    (read-list-isempty lyskom-reading-list)
                    (or (null pri) (> pri-session-pri pri)))
               )))
    'next-pri-session)

   ((and kom-higher-priority-breaks
	 (not (read-list-isempty lyskom-reading-list))
	 (not (read-list-isempty lyskom-to-do-list))
	 (let ((type (read-info->type (read-list->first lyskom-reading-list))))
	   (or (eq kom-higher-priority-breaks 'express)
	       (memq type '(CONF REVIEW REVIEW-MARK REVIEW-FAQ))))
	 (> (read-info->priority (read-list->first lyskom-to-do-list))
	    (read-info->priority (read-list->first lyskom-reading-list))))
    (if (> (text-list->length (read-info->text-list
			       (read-list->first lyskom-to-do-list)))
	   1)
       'next-pri-conf
      'next-pri-text))
   ((and (not (read-list-isempty lyskom-reading-list))
         (eq (read-info->type (read-list->first lyskom-reading-list)) 
             'RE-EDIT-TEXT))
    'reedit-text)
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
  (when (and lyskom-is-waiting
             (listp lyskom-is-waiting)
             (eval lyskom-is-waiting))
      (setq lyskom-is-waiting nil))
  (lyskom-update-prompt))


(defun lyskom-known-texts ()
  "Count how many unread texts the user have, that the client knows about."
  (apply '+ (mapcar '(lambda (x) (text-list->length (read-info->text-list x)))
		    (read-list->all-entries lyskom-to-do-list))))


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


;; FIXME: Here we can handle both maps and mappings, but in 
;; FIXME: reality this should *never* get called with a map 
;; FIXME: anymore

(defun lyskom-list-unread (map membership)
  "Args: MAP MEMBERSHIP. Return a list of unread texts.
The list consists of text-nos."
  (if (lyskom-text-mapping-p map)
      (lyskom-list-unread-mapping map membership)
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
      (delq 0 (listify-vector the-map)))))

(defun lyskom-list-unread-mapping (map membership)
  "Args: MAP MEMBERSHIP. Return a list of unread texts.
The list consists of text-nos."
  (let ((read (membership->read-texts membership))
        (iter  (text-mapping->iterator map))
        (el nil))
    (when (not (null read))
      (while (setq el (text-mapping-iterator->next iter))
        (when (lyskom-binsearch (text-pair->local-number el) read)
          (text-mapping->remove-local map (text-pair->local-number el)))))
    (text-mapping->global-numbers map)))



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


(defun lyskom-binsearch (el sequence &optional first last+1 fn)
  "Return the index if EL is a member of (present in) SEQUENCE.
SEQUENCE has to be sorted with regard to the comparison function.

Optional arguments FIRST and LAST+1 should only be used if you know
what they are for.

Optional argument FN is the function to use for comparison. It should
take arguments A and B and return non-nil if A is less than B. If not
supplied, FN defaults to <."
  (lyskom-binsearch-internal el sequence
			     (or first 0)
			     (or last+1 (length sequence))
                             (or fn '<)))

(defun lyskom-binsearch-internal (num vector first last+1 less-than)
  "Return the index if ELT is a member of the sorted vector VECTOR."
  (let* ((split (/ (+ first last+1) 2))
	 (splitval (elt vector split)))
    (cond
     ;; Only one element
     ((= (- last+1 first) 1) 
      (if (not (or (funcall less-than num splitval) 
                   (funcall less-than splitval num)))
          split nil))
     ;; Search the left subtree
     ((funcall less-than num splitval)
      (lyskom-binsearch-internal num vector first split less-than))

     ;; Search the left subtree
     ((funcall less-than splitval num)
      (lyskom-binsearch-internal num vector split last+1 less-than))

     ;; Found
     (t split))))

(defvar lyskom-verified-read-predicate nil)
(defun lyskom-verified-read-enter ()
  (interactive)
  (let* ((val (lyskom-minibuffer-contents))
         (err (funcall lyskom-verified-read-predicate val)))
    (if err
        (lyskom-minibuffer-message (format " [%s]" err))
      (exit-minibuffer))))

(defvar lyskom-verified-read-map nil)
(if lyskom-verified-read-map
    nil
  (setq lyskom-verified-read-map (copy-keymap minibuffer-local-map))
  (define-key lyskom-verified-read-map (kbd "RET") 'lyskom-verified-read-enter)
  (define-key lyskom-verified-read-map (kbd "C-j") 'lyskom-verified-read-enter)
  (define-key lyskom-verified-read-map (kbd "C-m") 'lyskom-verified-read-enter)
  )

(defun lyskom-verified-read-from-minibuffer (prompt initial pred)
  "Read something from minibuffer, verifying that it is valid.
PROMPT is the prompt and INITIAL the initial contents of the minibuffer.
PRED is a predicate to check entered data. It should return nil or a string. If
it returns a string, the data is not valid and the string is used as an error 
message."
  (let ((lyskom-verified-read-predicate pred))
    (lyskom-read-from-minibuffer prompt initial lyskom-verified-read-map)))


(defun lyskom-read-num-range-or-date (low high prompt &optional initial empty default)
  "Read a number or a date from the minibuffer.
Args: LOW HIGH PROMPT.
The result will be a number or a list of (YEAR MONTH DATE)."
  (let ((result nil)
        (break nil)
        (val (and initial (number-to-string initial)))
        (prompt (concat (if (symbolp prompt) (lyskom-get-string prompt) prompt)
                        (format "(%d-%d %s) " low high (lyskom-get-string 'or-date)))))
    (while (and (null result) (null break))
      (setq val (lyskom-verified-read-from-minibuffer 
                 prompt 
                 (and val (cons val 0))
                 (lambda (val)
                   (cond 
                    ((string-match "^\\s-*[0-9]+\\s-*$" val)
                     (let ((num (string-to-int val)))
                       (unless (and (>= num low) (<= num high))
                         (lyskom-get-string 'number-out-of-range))))
                    ((and empty (string-match "^\\s-*$" val)) nil)
                    (t
                     (condition-case nil
                         (progn (lyskom-parse-date val) nil)
                       (lyskom-error (lyskom-get-string 'invalid-date-entry))))))))
      (cond ((string-match "^\\s-*[0-9]+\\s-*$" val)
             (let ((num (string-to-int val)))
               (when (and (>= num low) (<= num high))
                 (setq result num))))
            ((and empty (string-match "^\\s-*$" val))
             (setq break t
                   result default))
            (t (condition-case nil
                   (setq result (lyskom-parse-date val) )
                 (lyskom-error nil)))))
    result))



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
      (setq number 
            (lyskom-read-number 
             (concat (cond ((null prompt) (lyskom-get-string 'give-a-number))
                           ((symbolp prompt) (lyskom-get-string prompt))
                           (t prompt))
                     (if show-range
                         (format "(%d-%d) " low high)
                       ""))
             default
             history
             nildefault)))
    number))


(defun lyskom-read-number (&optional prompt default history nildefault completions)
  "Read a number from the minibuffer. Optional arguments: PROMPT DEFAULT
If DEFAULT is non-nil, it is written within parenthesis after the prompt.
DEFAULT could also be of the type which (interactive P) generates.
If NILDEFAULT is non-null then typing return will cause the function 
to return nil."
  (let* ((numdefault (cond ((null default) nil)
                           ((integerp default) default)
                           ((listp default) (car default))
                           (t nil)))
         (prompt (concat (cond ((null prompt) (lyskom-get-string 'give-a-number))
                               ((symbolp prompt) (lyskom-get-string prompt))
                               (t prompt))
			 (if numdefault 
			     (format " (%d) " numdefault)
			   " ")))
         (number nil)
         (numstr nil)
         (done nil))
    (while (not done)
      (setq numstr
	    (prog1
                (if completions
                    (lyskom-completing-read prompt
                                            (mapcar (lambda (x) (list (number-to-string x) x))
                                                    completions)
                                            nil
                                            t
                                            nil
                                            history
                                            (when default (number-to-string default)))
                  (lyskom-read-string
                   prompt
                   nil
                   history))))
      (cond ((and (string= numstr "") numdefault)
	     (setq number numdefault done t))
            ((and (string= numstr "") nildefault)
             (setq number nil done t))
	    ((and (string-match "\\`\\s-*[0-9]+\\s-*\\'" numstr)
                  (lyskom-string-to-int numstr))
	     (setq number (lyskom-string-to-int numstr))
             (setq done number))
	    (t (beep))))
    number))


(defun lyskom-read-string (prompt &optional initial history)
  "Read a string from the minibuffer. Arguments: PROMPT INITIAL"
  (read-string prompt initial history))



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
;;; j-or-n-p is similar to y-or-n-p.
;;;


(defun j-or-n-p (prompt)
  "Same as y-or-n-p but language-dependent.
Uses lyskom-message, lyskom-read-string to do interaction and
lyskom-get-string to retrieve regexps for answer and string for repeated query."
  (when (symbolp prompt) (setq prompt (lyskom-get-string prompt)))
  (let ((input-char 0)
	(cursor-in-echo-area t)
        (lyskom-inhibit-minibuffer-messages t)
	(nagging nil))

    (while (and (not (char-in-string input-char
                                     (lyskom-get-string 'y-or-n-instring)))
                (not (or (eq input-char ?\C-g)
                              (eq 'keyboard-quit
                                  (lyskom-lookup-key (current-local-map)
                                                     input-char
                                                     t)))))
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
                (prog1 (read-char-exclusive)
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

    (when (or (eq input-char ?\C-g)
	      (eq 'keyboard-quit (lyskom-lookup-key (current-local-map)
						    input-char
						    t)))
      (signal 'quit nil))

    (lyskom-message "%s" (concat prompt
                                 (lyskom-get-string 'j-or-n)
                                 (if (char-in-string input-char (lyskom-get-string 'y-instring))
                                     (lyskom-get-string 'yes-string)
                                   (lyskom-get-string 'no-string))))
    (char-in-string input-char (lyskom-get-string 'y-instring))))

  
;;; lyskom-j-or-n-p, lyskom-ja-or-no-p
;;; These versions no longer perform lyskom-end-of-command
;; Author: Linus Tolke

(defun lyskom-j-or-n-p (prompt)
  "Same as j-or-n-p but performs lyskom-end-of-command if quit."
  (condition-case nil
      (j-or-n-p prompt)
    (quit (signal 'quit nil))))


(defun lyskom-ja-or-nej-p (prompt &optional initial-input)
  "Same as ja-or-nej-p but performs lyskom-end-of-command if quit."
  (condition-case nil
      (ja-or-nej-p prompt initial-input)
    (quit (signal 'quit nil))))

(defun lyskom-a-or-b-or-c-p (prompt alternatives &optional default)
  "Like y-or-n-p but choose among several options.
Display PROMPT and ask user to choose among ALTERNATIVES.

ALTERNATIVES are symbols. Each alternative must be found as a string
in lyskom-messages. The first character of the string is the character
to enter to select the alternative and is not displayed.

If non-nil DEFAULT is the default alternative and is selected when the
user types RET.

Returns the selected alternative (a symbol)"
  (when (symbolp prompt) (setq prompt (lyskom-get-string prompt)))
  (let* ((input-char 0)
         (cursor-in-echo-area t)
         (lyskom-inhibit-minibuffer-messages t)
         (alts (mapcar (lambda (alt)
                         (let ((string (lyskom-get-string alt)))
                           (list (elt string 0)
                                 (substring string 1)
                                 alt)))
                         alternatives))
         (alts-string (format "(%s) " (mapconcat (lambda (c)
                                                   (lyskom-format "%#3@%#1c:%#2s"
                                                           (elt c 0)
                                                           (elt c 1)
                                                           (and (eq (elt c 2) default) '(face bold))
                                                           ))
                                                 alts
                                                 ", ")))
         (nagging nil))

    (while (and (not (assq input-char alts))
                (not (or (eq input-char ?\C-g)
                         (eq 'keyboard-quit
                             (lyskom-lookup-key (current-local-map)
                                                input-char
                                                t))))
                (not (and default (eq input-char ?\C-m))))
      (lyskom-message "%s" (concat (if nagging 
                                       (lyskom-get-string 'a-or-b-or-c-nag)
                                     "")
                                   prompt
                                   alts-string))
      (if nagging (beep))

      ;;
      ;; Workaround for Emacs whose read-char does not accept C-g
      ;;

      (setq input-char 
            (let ((inhibit-quit t))
              (prog1 (read-char-exclusive)
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

    (when (or (eq input-char ?\C-g)
	      (eq 'keyboard-quit (lyskom-lookup-key (current-local-map)
						    input-char
						    t)))
      (signal 'quit nil))

    (setq alts (cons (list ?\C-m
                           (lyskom-traverse alt alts (when (eq default (elt alt 2)) (lyskom-traverse-break (elt alt 1))))
                           default) alts))
    (lyskom-message "%s" (concat prompt (elt (assq input-char alts) 1)))
    (if (eq input-char ?\C-m)
        default
      (elt (assq input-char alts) 2))))


(defun impl ()
  (error "Not implemented"))


;;; Priority filtering

(defun lyskom-visible-membership (membership)
  "Is this conference visible?
Return t is MEMBERSHIPs priority is higher than or equal to
lyskom-session-priority and nil otherwise.

If MEMBERSHIPs prioriy is 0, it always returns nil."
  (let ((priority (membership->priority membership)))
    (and (not (membership-type->passive (membership->type membership)))
	 (>= priority lyskom-session-priority))))

;;; The filter.

(defun lyskom-filter (proc output)
  "Receive replies from LysKOM server."
;  (sit-for 0)				; Why? [Doesn't work in XEmacs 19.14]
;  (setq lyskom-apo-timeout-log
;        (cons (cons (current-time-string) lyskom-apo-timeout) lyskom-apo-timeout-log))
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
                    (princ (lyskom-string-as-unibyte output) (current-buffer))
                    (lyskom-debug-limit-buffer)))

	      (if lyskom-debug-communications-to-buffer
                  (if (not lyskom-debug-what-i-am-doing)
                      (if (not (and (eq ?: (elt output 0))
                                    (eq ?5 (elt output 1))))
                          (lyskom-debug-insert proc "From " output))
                    (lyskom-debug-insert proc "From " output)))

	      (set-buffer (process-buffer proc))
	      (princ (lyskom-string-as-unibyte output) lyskom-unparsed-marker)
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

;;      (unwind-protect
;;          (save-excursion (set-buffer (process-buffer proc))
;;                          (lyskom-check-mship-lists)))

      (setq lyskom-quit-flag (or lyskom-quit-flag quit-flag))
      (setq quit-flag nil)
      (when lyskom-need-prompt-update
        (lyskom-update-all-prompts))
      ;; Restore selected buffer and match data.
      (store-match-data old-match-data)
      (when (buffer-live-p lyskom-filter-old-buffer)
        (set-buffer lyskom-filter-old-buffer)))
    (sit-for 0)))
      

;;; The sentinel

(defun lyskom-sentinel (proc sentinel)
  "Handles changes in the lyskom-process."
  (lyskom-remove-unread-buffer proc)
  (set-buffer (process-buffer proc))
  (lyskom-start-of-command (lyskom-get-string 'process-signal) t t)
  (lyskom-format-insert 'closed-connection
                        sentinel 
                        (lyskom-format-time
                         'timeformat-day-yyyy-mm-dd-hh-mm-ss))
  (setq mode-line-process (lyskom-get-string 'mode-line-down))
  ; Notify?
  (when kom-lost-session-notification    
    (if (eq 'all-buffers kom-lost-session-notification)
	(let ((session-nick (lyskom-session-nickname)))	
	  (save-excursion
	    (lyskom-traverse buf lyskom-buffer-list
	      (when (lyskom-buffer-p buf)
		(set-buffer buf)
		(lyskom-insert-before-prompt
		 (lyskom-format 'closed-connection-other-buf
				      session-nick)))))))
    (beep)
    )
  (lyskom-scroll))


;;; ================================================================
;;;         Debug buffer

(defun lyskom-debug-limit-buffer ()
  (when (and (numberp lyskom-debug-communications-limit)
             (> (point-max) lyskom-debug-communications-limit))
    (delete-region (point-min) (- (point-max) lyskom-debug-communications-limit))))

(defun lyskom-debug-insert (proc prefix string)
  (let* ((buf (get-buffer-create lyskom-debug-communications-to-buffer-buffer))
	 (win (get-buffer-window buf 'visible)))
    (if win
	(save-excursion
	  (save-selected-window
	    (select-window win)
            (set-buffer buf)
	    (let ((move (eobp)))
	      (save-excursion
		(goto-char (point-max))
		(insert "\n"
                        prefix
			(format "%s" proc)
                        ": "
                        string))
              (lyskom-debug-limit-buffer)
	      (if move (goto-char (point-max))))))
      (save-excursion
	(set-buffer buf)
	(goto-char (point-max))
	(insert "\n"
                prefix
		(format "%s" proc)
		": "  string)
        (lyskom-debug-limit-buffer)))))


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
     ((eq (car object) 'STRING)
      (lyskom-prot-a-format-raw-string (cdr object)))
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
   (lyskom-prot-a-format-bool (eq (membership-type->message-flag membership-type)
                                  (membership-type->passive membership-type)))
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
   (lyskom-prot-a-format-raw-string (cons 'raw-text 
                                          (lyskom-aux-item-output-data 
                                           item)))))

(defun lyskom-prot-a-format-aux-item-flags (flags)
  "Format AUX-ITEM-FLAGS for output to the server."
  (concat
   (lyskom-prot-a-format-bool (aux-item-flags->deleted flags))
   (lyskom-prot-a-format-bool (aux-item-flags->inherit flags))
   (lyskom-prot-a-format-bool (aux-item-flags->secret flags))
   (lyskom-prot-a-format-bool (aux-item-flags->anonymous flags))
   (lyskom-prot-a-format-bool (aux-item-flags->dont-garb flags))
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
	   ((eq (car misc-item) 'RECPT) 0)
	   ((eq (car misc-item) 'CC-RECPT) 1)
	   ((eq (car misc-item) 'COMM-TO) 2)
	   ((eq (car misc-item) 'FOOTN-TO) 4)
           ((eq (car misc-item) 'BCC-RECPT) 
            (if (lyskom-have-feature bcc-misc) 15 1)))
	  (cdr misc-item)))


(defun lyskom-prot-a-format-simple-list (list)
  "Format some kind of list to send to server."
  (apply 'concat (list (format "%d {" (length list))
		       (apply 'lyskom-format-objects list)
		       " }")))

  
(defun lyskom-prot-a-format-string (string)
  (let ((tmp (lyskom-encode-coding-string string lyskom-server-coding-system)))
    (lyskom-string-as-unibyte (format "%dH%s" (lyskom-string-bytes tmp) tmp))))

(defun lyskom-prot-a-format-raw-string (string)
  (lyskom-string-as-unibyte (format "%dH%s" (lyskom-string-bytes (cdr string)) 
			     (cdr string))))





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
				  (read-char-exclusive)
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
		(if (equal (length input-string) 0)
		    ""
		  (substring input-string 0 -1)))
	       ((eq input-char ?\C-u)
		"")
	       (t
		(concat input-string (char-to-string input-char)))))))
    (message "")
    input-string))


(defun lyskom-session-nickname (&optional in-modeline)
  "Return the nickname to use for this LysKOM session."
  (if kom-session-nickname
      kom-session-nickname
    (let ((server (process-name (get-buffer-process (current-buffer)))))
      (or (cdr (assoc server
		      (append kom-server-aliases
			      kom-builtin-server-aliases)))
	  (if in-modeline
	    (format "LysKOM(%s)" server)
	    server)))))
   
;;; This really is a strange thing to do but...
;;
(defun lyskom-mode-name-from-host ()
  "Calculate what to identify the buffer with."
  (setq mode-line-server-name 
	(lyskom-session-nickname t)))

(defvar lyskom-modeline-keymap nil)
(if lyskom-modeline-keymap
    nil
  (setq lyskom-modeline-keymap (make-sparse-keymap))
  (lyskom-xemacs-or-gnu
   (progn (define-key lyskom-modeline-keymap (kbd (lyskom-keys 'button2up)) 'kom-modeline-next-unread-kom)
          (define-key lyskom-modeline-keymap (kbd (lyskom-keys 'button3)) 'kom-modeline-select-unread-kom))
  (progn (define-key lyskom-modeline-keymap [mode-line mouse-2] 'kom-modeline-next-unread-kom)
         (define-key lyskom-modeline-keymap [mode-line mouse-3] 'kom-modeline-select-unread-kom))))


(defun lyskom-make-alternate-unread-mode-line ()
  (save-excursion
    (let ((mline ())
	  (current nil)
	  (upcased nil))
      
      (lyskom-traverse session lyskom-sessions-with-unread
	(set-buffer session)
	(setq current (lyskom-session-nickname))
	(when (memq session lyskom-sessions-with-unread-letters)
	  (setq upcased (upcase current))
	  (setq current
		(if (string= upcased current)
		    (concat "*" current "*")
		  upcased)))
	(setq mline (append mline (list (concat current " ")))))
      (setq mline (sort mline 'string<))
      mline)))

(defun lyskom-make-lyskom-unread-mode-line ()
  (if kom-unread-mode-line-type
      (lyskom-make-alternate-unread-mode-line)
    (lyskom-xemacs-or-gnu
     (list (list 'lyskom-sessions-with-unread 
		 (lyskom-get-string 'mode-line-unread))
	   (list 'lyskom-sessions-with-unread-letters
		 (lyskom-get-string 'mode-line-letters))
	   " ")
     `((lyskom-sessions-with-unread 
	,(lyskom-format "%#1@%#2s"
			(list 'local-map lyskom-modeline-keymap)
			(lyskom-get-string 'mode-line-unread)))
       (lyskom-sessions-with-unread-letters
	,(lyskom-format "%#1@%#2s"
			(list 'local-map lyskom-modeline-keymap)
			(lyskom-get-string 'mode-line-letters)))
       " "))))

(defun lyskom-make-lyskom-unread-title-format ()
  `(kom-show-unread-in-frame-title
    (lyskom-session-has-unreads 
     (" (" ((lyskom-session-has-unreads
             ,(lyskom-recode-string-for-title (lyskom-get-string 'frame-title-unread)
                                              'iso-8859-1))
            (lyskom-session-has-unread-letters ,(lyskom-recode-string-for-title
                                                 (lyskom-get-string 'frame-title-letters)
                                                 'iso-8859-1)))
      ")"))))

(defvar icon-title-format)
(defvar frame-icon-title-format)


(if lyskom-is-loaded
    nil

  ;; Fix altered format for kom-default-language
  (when (and kom-default-language (symbolp kom-default-language))
    (setq kom-default-language (list kom-default-language)))

  ;; Set up default language
  (setq lyskom-language (lyskom-default-language))
  (setq lyskom-global-language lyskom-language)
  (unless kom-default-language
    (setq kom-default-language (list lyskom-language)))


  (setq-default lyskom-collate-table lyskom-default-collate-table)
  ;; We should set lyskom-char-classes to
  ;;     (lyskom-compute-char-classes lyskom-collate-table))
  ;; but that currently fails under Emacs 20.7, because
  ;; lyskom-default-collate-table isn't set properly.
  (setq-default lyskom-char-classes nil)
  (lyskom-set-language lyskom-language 'local)
  (lyskom-set-language lyskom-language 'global)

  (unless (or (memq 'lyskom-unread-mode-line global-mode-string)
              (rassq 'lyskom-unread-mode-line global-mode-string))
    (lyskom-xemacs-or-gnu
     (let ((extent (make-extent nil nil nil)))
       (set-extent-keymap extent lyskom-modeline-keymap)
       (setq global-mode-string
             (append (list "" (cons extent 'lyskom-unread-mode-line))
                     global-mode-string)))
     (setq global-mode-string
           (append '("" lyskom-unread-mode-line) global-mode-string))))

  (if (boundp 'frame-title-format)
      (setq frame-title-format (list ""
                                     frame-title-format
                                     'lyskom-unread-title-format)))

  (if (boundp 'icon-title-format)
      (setq icon-title-format (list ""
                                    icon-title-format
                                    'lyskom-unread-title-format)))  
  (if (boundp 'frame-icon-title-format)
      (setq frame-icon-title-format (list ""
                                          frame-icon-title-format
                                          'lyskom-unread-title-format)))
  
  (add-hook 'kill-buffer-hook 'lyskom-remove-buffer-from-lists)


;;;
;;; Set up lyskom-line-start-chars. The reason we do it here is that
;;; char-to-int may not be defined until compatibility.el has been
;;; loaded.
;;;

  (setq lyskom-line-start-chars
        (let ((tmp (make-vector 256 nil)))
          (mapcar 
           (function
            (lambda (x)
              (aset tmp (lyskom-char-to-int x) t)))
           (lyskom-maybe-recode-string lyskom-line-start-chars-string 'iso-8859-1 t))
          tmp))


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

  ;; Seed the randomizer

  (random t)
  (run-hooks 'lyskom-after-load-hook)
  (run-hooks 'kom-after-load-hook)
  (setq lyskom-is-loaded t))



(provide 'lyskom-rest)

;;; This should be the very last lines of lyskom.el Everything should
;;; be loaded now, so it's time to run the kom-after-load-hook.


(lyskom-end-of-compilation)

;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; end: 
