;;;;;
;;;;; $Id: startup.el,v 38.4 1995-03-01 17:56:07 byers Exp $
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
;;;; File: startup.el
;;;;
;;;; This file contains functions that are called only when lyskom
;;;; is loaded, started or when a new user is logged in during a 
;;;; session.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: startup.el,v 38.4 1995-03-01 17:56:07 byers Exp $\n"))


;;; ================================================================
;;;                         Start kom.


(defun lyskom (&optional host username password)
  "Start a LysKOM session.
Optional arguments: HOST, USERNAME and PASSWORD.

See lyskom-mode for details."
  (interactive (list (lyskom-read-string (lyskom-format 'server-q
					  (or (getenv "KOMSERVER")
					      lyskom-default-server)))
		     nil
		     nil))

  (run-hooks 'lyskom-init-hook)
  (setq username
	(if username username (getenv "KOMNAME")))
  (setq password
	(if password password (getenv "KOMPASSWORD")))
  (if (zerop (length host))
      (setq host (or (getenv "KOMSERVER")
		     lyskom-default-server)))
  (let ((port 4894)
	(init-done nil))
    (cond				;Allow "nanny:4892" to use port 4892.
     ((string-match ":" host)
      (setq port (string-to-int (substring host (match-end 0))))
      (cond
       ((zerop (match-beginning 0))
	(setq host lyskom-default-server))
       (t
	(setq host (substring host 0 (match-beginning 0)))))))

    (unwind-protect
	(let* ((buffer (generate-new-buffer host))
	       (name (buffer-name buffer))
	       (proc (open-network-stream name buffer host port)))
	  (switch-to-buffer buffer)
	  (lyskom-mode)			;Clearing lyskom-default...
	  (setq lyskom-default-user-name username)
	  (setq lyskom-default-password password)
	  (setq lyskom-server-name host)
	  (setq lyskom-proc proc)
	  (lyskom-insert
	   (lyskom-format 'try-connect lyskom-clientversion host))
	  (set-process-filter proc 'lyskom-connect-filter)
	  (lyskom-process-send-string proc
				      (concat "A"
					      (lyskom-format-objects
					       (concat (user-login-name)
						       "%" (system-name)))))
	  (while (eq 'lyskom-connect-filter (process-filter proc))
	    (accept-process-output proc))
					; Now we have gotten the correct response.

	  (set-process-sentinel proc 'lyskom-sentinel)

	  (save-excursion
	    (lyskom-init-parse))

	  (setq lyskom-server-info (blocking-do 'get-server-info))
	  (lyskom-format-insert 
	   'connection-done
	   (if (zerop (% (server-info->version lyskom-server-info) 100))
	       (format "%d.%d"
		       (/ (server-info->version lyskom-server-info) 10000)
		       (/ (% (server-info->version lyskom-server-info) 10000) 
			  100))
	     (format "%d.%d.%d"
		     (/ (server-info->version lyskom-server-info) 10000)
		     (/ (% (server-info->version lyskom-server-info) 10000)
			100)
		     (% (server-info->version lyskom-server-info) 100))))
	  (if (not (zerop (server-info->motd-of-lyskom lyskom-server-info)))
	      (lyskom-insert 
	       (text->text-mass 
		(blocking-do 'get-text 
			     (server-info->motd-of-lyskom lyskom-server-info)))))
	  ;; Can't use lyskom-end-of-command here.
	  (setq lyskom-executing-command nil) 
	  ;;; B|rja
	  (kom-start-anew t)
	  (setq init-done t))
      ;; Something went wrong. Lets cleanup everything. :->
      (if init-done
	  nil
	(delete-process (get-buffer-process (current-buffer)))
	(kill-buffer (current-buffer))))))


(defun lyskom-connect-filter (proc output)
  "Receive connection acknowledgement from server."
  (if lyskom-debug-communications-to-buffer
      (save-excursion
	(set-buffer
	 (get-buffer-create lyskom-debug-communications-to-buffer-buffer))
	(goto-char (point-max))
	(insert "\n" (format "%s" proc) "-----> " output)))
  (cond
   ((string-match "^LysKOM\n" output)
    (set-process-filter proc 'lyskom-filter))))





;;; ================================================================
;;;                        Start anew


(defun kom-start-anew (&optional first-time-around)
  "Start as a new person."
  (interactive)
  (lyskom-start-of-command 'kom-start-anew)
  (if first-time-around
      nil
    (lyskom-tell-internat 'kom-tell-login))
  (setq lyskom-pers-no nil)
  (while (not lyskom-pers-no)
    (if (and first-time-around
	     lyskom-default-user-name)
	(setq lyskom-pers-no  ;; This is nil if we can't find a unique match.
	      (lyskom-read-conf-name-internal lyskom-default-user-name
					      'pers 'conf-no)))
    (if lyskom-pers-no
	nil
      (setq lyskom-pers-no
	    (lyskom-read-conf-no (lyskom-get-string 'what-is-your-name)
				 'pers nil "")))
    ;; Now lyskom-pers-no contains a number of a person.
    ;; Lets log him in.
    (let ((conf-stat (blocking-do 'get-conf-stat lyskom-pers-no)))
      (lyskom-insert (concat (conf-stat->name conf-stat) "\n"))
      (setq first-time-around nil)
      (if (blocking-do 'login lyskom-pers-no
		       (if lyskom-default-password
			   (prog1
			       lyskom-default-password
			     (setq lyskom-default-password nil)
			     (set-default 'lyskom-default-password nil))
			 (silent-read (lyskom-get-string 'password))))
	  nil
	(lyskom-insert-string 'wrong-password)
	(setq lyskom-pers-no nil))))

  ;; Now we are logged in.
  (lyskom-insert-string 'are-logged-in)
  (let ((conf-stat (blocking-do 'get-conf-stat lyskom-pers-no)))
    (if (and conf-stat
	     (/= (conf-stat->msg-of-day conf-stat) 0))
	(progn
	  (lyskom-insert-string 'you-have-motd)
	  (lyskom-view-text (conf-stat->msg-of-day conf-stat))))
    (if (and conf-stat
	     (zerop (conf-stat->presentation conf-stat))
	     (not (zerop (conf-stat->no-of-texts conf-stat))))
	(lyskom-insert-string 'presentation-encouragement)))
  (lyskom-read-options) 
  (lyskom-refetch)
  ;; (cache-initiate-who-info-buffer (blocking-do 'who-is-on))
  (cache-set-marked-texts (blocking-do 'get-marks))
  (setq lyskom-who-am-i (blocking-do 'who-am-i))
  (lyskom-end-of-command))


(defun lyskom-refetch ()
  "Resets and fetches all reading info.
This is called at login and after prioritize and set-unread."
  ;; The whole membership!
  (lyskom-set-membership (blocking-do 'get-membership lyskom-pers-no))
  (setq lyskom-membership-is-read t)
  (setq lyskom-unread-confs (blocking-do 'get-unread-confs lyskom-pers-no))
  (setq lyskom-last-conf-fetched -1)
  (setq lyskom-last-conf-received -1)
  (setq lyskom-last-conf-done -1)
  (setq lyskom-to-do-list (lyskom-create-read-list))
  (setq lyskom-reading-list (lyskom-create-read-list)))


(defun lyskom-set-membership (membership)
  "Sets lyskom-membership to a new value.
Args: MEMBERSHIP."
  (setq lyskom-membership (sort (lyskom-array-to-list membership)
				'lyskom-membership-<))
  (setq lyskom-membership-is-read t))


(defun lyskom-print-name (conf-stat)
  "Print the name of the CONF-STAT, with a trailing \n."
  (lyskom-insert (concat (conf-stat->name conf-stat) "\n")))


(defun lyskom-extract-persons (conf-list)
  "Extract persons from a conf-list.
Return a list of pers-nos of all conferences that are persons.
Args: CONF-LIST."
  (lyskom-do-extract-persons-or-confs conf-list t))


(defun lyskom-extract-confs (conf-list)
  "Extract conferences from a conf-list.
Return a list of conf-nos of all conferences that are persons.
Args: CONF-LIST."
  (lyskom-do-extract-persons-or-confs conf-list nil))


(defun lyskom-do-extract-persons-or-confs (conf-list want-persons)
  "Extract persons or conferences from CONF-LIST.
WANT-PERSONS is t for persons, nil for confs."
  (let* ((result nil)
	 (i 0)
	 (nos (conf-list->conf-nos conf-list))
	 (types (conf-list->conf-types conf-list))
	 (len (length nos)))
    (while (< i len)
      (cond
       ((eq (conf-type->letterbox (elt types i))
	    want-persons)
	(setq result (cons (elt nos i)
			   result))))
      (++ i))
    (nreverse result)))


(defun lyskom-start-anew-new-person (name)
  "A new user named NAME (or an old that mis-spelled his name)."
  (lyskom-insert
   (lyskom-format 'first-greeting name))
  (lyskom-scroll)
  (cond
   ((ja-or-nej-p (lyskom-format 'is-name-correct name))
    (let ((password (silent-read (lyskom-get-string 'personal-password))))
      (cond
       ((not (equal password 
		    (silent-read (lyskom-get-string 'repeat-password))))
	(lyskom-insert-string 'repeat-failure)
	(setq lyskom-executing-command nil)
	(kom-start-anew))
       (t
	(setq lyskom-pers-no 0)
	(initiate-create-person 'main 'lyskom-start-anew-create-handler
				name password
				name password)))))
   (t
    (setq lyskom-executing-command nil)
    (kom-start-anew))))


(defun lyskom-start-anew-create-handler (pers-no name password)
  "A new person has been created. Log in as him."
  (cond
   ((null pers-no)
    (lyskom-insert-string 'could-not-create-you)
    (setq lyskom-executing-command nil)
    (kom-start-anew))
   (t
    (initiate-login 'main 'lyskom-add-for-new-person
		    pers-no password pers-no lyskom-pers-no)
    )))

(defun lyskom-add-for-new-person (reply pers-no lyskom-pers-no)
  "Add a news person as member in the default presentation conference."
  (initiate-add-member 'main 'lyskom-start-anew-login-2
		       (server-info->conf-pres-conf lyskom-server-info)
		       pers-no 100 1
		       pers-no lyskom-pers-no))


;;; ================================================================
;;;                        The LysKOM mode.

;; The LysKOM mode should not be inherited if we create a new buffer
;; and default-major-mode is nil.
(put 'lyskom-mode 'mode-class 'special)

(defun lyskom-mode ()
  "\\<lyskom-mode-map>Mode for LysKOM client.
Commands:
\\[kom-next-command]	reads the next text, selects next conference with unread texts
	or whatever the prompt says.
\\[kom-go-to-conf]	asks for a conference and makes you a member (if your not) in
	that conference. (you name the conference).
\\[kom-list-conferences]	writes a list of conferences matching a given string.
\\[kom-list-persons]	writes a list of persons matching a given string.

\\[kom-list-news]	gives a list of the amount of unread texts you have and in 
	what conferences.
\\[kom-go-to-next-conf]	put the current conference in the bottom of the list and read 
	the next one from the list of conferences with unread texts.

\\[kom-membership]	creates a buffer with the list of conferences you are member in.

\\[kom-quit]	leaves this lyskom session.
\\[kom-who-is-on]	gives you a list of all the users of lyskom right now.

\\[kom-extended-command]	reads a command using the minibuffer and executes it.
	This is another way to give commands.

\\[kom-write-text]	asks for subject and start editing of a new text.
\\[kom-write-comment]	starts editing of a comment to the last read article.
\\[kom-private-answer]	starts editing of a personal answer to the author of the last
	read article.
\\[kom-send-letter]	prompts for a name of a person or conference and then starts
	editing a letter to this person or conference.

\\[kom-page-next-command]	starts at the top of a page and then does what \\[kom-next-command] does.
\\[kom-line-next-command]	does what the \\[kom-next-command] does but scrolls at most 1 line.

0 .. 9	gives a numeric argument to certain commands.
\\[describe-mode]	gives you this help.

\\[kom-busy-wait]	put the lyskom-session in wait mode. The next created text with 
	a priotity higher that that of the next conference you are going
	to will be read directly when it is created.
\\[kom-set-unread]	mark a number of texts as unread.
\\[kom-jump]	skip, mark as read, all the comments to this article recursively.
\\[kom-display-time]	show the current date and time.

\\[kom-change-presentation]	change your presentation.
\\[kom-view]	view the specified text.
\\[kom-view-commented-text]	view the text that the current text comments or is a footnote to.
\\[kom-review-presentation]	show the presentation for a person or a conferencce.

\\[kom-review-comments]	view all comments to the current text.
\\[kom-review-tree]	view all comments to the current text and step through the tree
	in depth-first order.

\\[kom-find-root-review]	view the complete comment tree.
\\[kom-find-root]	show the root text of this comment tree.
\\[kom-review-by-to]	view the last (first or all) article written by named author
	in a named conference.

\\[kom-mark-text]	creates a mark on a text. (you specify a number).
\\[kom-unmark-text]	removes the mark on a text.
\\[kom-review-marked-texts]	view all text marked with a certain mark. (you specify it).
\\[kom-review-all-marked-texts]	view all marked text.

\\[kom-view-next-new-text]	push the viewing on the bottom of the stack and read the next new
	text from the list of unread.
\\[kom-review-next]	continue the viewing.
\\[kom-review-stack]	show the stack of things we are viewing.
\\[kom-review-clear]	clear the stack of things we are viewing.
\\[kom-review-backward]	toggles the read order of reviewed texts. This can only be done
	when viewing texts with \\[kom-review-by-to], \\[kom-review-marked-texts] and \\[kom-review-all-marked-texts].

\\[kom-status-conf]	show the status of a conference.
\\[kom-status-person]	show the status of a person
\\[kom-save-text]	save the text you are looking at to a file.

\\[kom-get-abuse]	get an insulting text.
\\[kom-get-appreciation]	get an encouraging text.

\\[kom-list-news-old]	another list of unread texts. Sometimes they differ due to 
	bugs in the client. This will not exist any more when there are no 
	more bugs in these parts of the code.

\\[kom-add-self]	makes you a member of a conference.
\\[kom-sub-self]	removes you as a member of a conference.

All bindings (this is here due to the fact that inconsistensies while 
developping this package are frequent):
\\{lyskom-mode-map}
Entry to this mode runs lyskom-mode-hook.

Functions and variables beginning with kom- are intended for the user to see,
set or call.
Functions and variables beginning with lyskom- are not intended for the user
to see, set of call."
  (interactive)
  (lyskom-clear-vars)
  (setq mode-line-buffer-identification
	(list (concat (lyskom-mode-name-from-host) ": ") 'mode-line-conf-name))

  (setq major-mode 'lyskom-mode)
  (setq mode-name "LysKOM")
  (setq mode-line-process '(": %s"))
  (use-local-map lyskom-mode-map)
  (lyskom-count-down-edits)
  (run-hooks 'lyskom-mode-hook)
  (lyskom-tell-phrases-validate))


(defun lyskom-clear-vars ()
  "Set up buffer-local vars."
  (let ((proc lyskom-proc)
	(pers-no lyskom-pers-no)
	(membership lyskom-membership)
	(membership-is-read lyskom-membership-is-read)
	(last-viewed lyskom-last-viewed)
	(replies-buffer lyskom-unparsed-buffer)
	(replies-marker lyskom-unparsed-marker)
	(server-info lyskom-server-info)
	(server-name lyskom-server-name)
	)
    (kill-all-local-variables)
    (make-local-variable 'lyskom-blocking-return)
    (make-local-variable 'lyskom-unparsed-buffer)
    (make-local-variable 'lyskom-unparsed-marker)
    (make-local-variable 'lyskom-is-writing)
    (make-local-variable 'lyskom-pending-calls)
    (make-local-variable 'lyskom-number-of-pending-calls)
    (make-local-variable 'lyskom-errno)
    (make-local-variable 'lyskom-pers-no)
    (make-local-variable 'lyskom-session-no)
    (make-local-variable 'lyskom-proc)
    (make-local-variable 'lyskom-conf-cache)
    (make-local-variable 'lyskom-pers-cache)
    (make-local-variable 'lyskom-text-cache)
    (make-local-variable 'lyskom-text-mass-cache)
    (make-local-variable 'lyskom-server-info)
    (make-local-variable 'lyskom-server-name)
    (make-local-variable 'lyskom-default-user-name)
    (make-local-variable 'lyskom-default-password)
    (make-local-variable 'lyskom-who-info-cache)
    (make-local-variable 'lyskom-who-info-buffer)
    (make-local-variable 'lyskom-who-info-buffer-is-on)
    (make-local-variable 'lyskom-prioritize-buffer)
    (make-local-variable 'lyskom-marked-text-cache)
    (make-local-variable 'lyskom-is-parsing)
    (make-local-variable 'lyskom-last-viewed)
    (make-local-variable 'lyskom-prefetch-confs)
    (make-local-variable 'lyskom-unread-confs)
    (make-local-variable 'lyskom-last-conf-fetched)
    (make-local-variable 'lyskom-last-conf-received)
    (make-local-variable 'lyskom-last-conf-done)
    (make-local-variable 'lyskom-membership)
    (make-local-variable 'lyskom-prefetch-texts)
    (make-local-variable 'lyskom-prefetch-conf-tresh)
    (make-local-variable 'lyskom-no-prompt)
    (make-local-variable 'lyskom-reading-list)
    (make-local-variable 'lyskom-to-do-list)
    (make-local-variable 'lyskom-fetched-texts)
    (make-local-variable 'lyskom-command-to-do)
    (make-local-variable 'lyskom-to-be-printed-before-prompt)
    (make-local-variable 'lyskom-other-clients-user-areas)
    (make-local-variable 'lyskom-is-waiting)
    (make-local-variable 'lyskom-what-i-am-doing)
    (make-local-variable 'lyskom-current-conf)
    (make-local-variable 'lyskom-previous-text)
    (make-local-variable 'lyskom-current-text)
    (make-local-variable 'lyskom-current-subject)
    (make-local-variable 'lyskom-executing-command)
    (make-local-variable 'lyskom-membership-is-read)
    (make-local-variable 'lyskom-dont-change-prompt)
    (make-local-variable 'lyskom-count-var)
    (make-local-variable 'lyskom-is-administrator)
    (make-local-variable 'lyskom-do-when-starting)
    (make-local-variable 'lyskom-do-when-done)
    (make-local-variable 'mode-line-conf-name)
    (make-local-variable 'lyskom-output-queue)
    (make-local-variable 'lyskom-options-done)
    (make-local-variable 'lyskom-list-of-edit-buffers)
    (make-local-variable 'lyskom-filter-list)
    (setq lyskom-proc proc)
    (setq lyskom-pers-no pers-no)
    (setq lyskom-membership membership)
    (setq lyskom-last-viewed last-viewed)
    (setq lyskom-membership-is-read membership-is-read)
    (setq lyskom-unparsed-buffer replies-buffer)
    (setq lyskom-unparsed-marker replies-marker)
    (setq lyskom-server-info server-info)
    (setq lyskom-server-name server-name)
    (setq lyskom-do-when-done (cons kom-do-when-done kom-do-when-done))
    (setq lyskom-output-queue (lyskom-queue-create))
    (setq lyskom-list-of-edit-buffers nil)
    (lyskom-set-mode-line (lyskom-get-string 'not-present-anywhere))))
