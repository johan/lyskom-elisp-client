;;;;;
;;;;; $Id: startup.el,v 35.14 1992-08-12 04:40:27 linus Exp $
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
	      "$Id: startup.el,v 35.14 1992-08-12 04:40:27 linus Exp $\n"))


;;; ================================================================
;;;                         Start kom.


(defun lyskom (&optional host username password)
  "Start a LysKOM session.
Optional arguments: HOST, USERNAME and PASSWORD."
  (interactive (list (lyskom-read-string (lyskom-format 'server-q
					  (or (getenv "KOMSERVER")
					      lyskom-default-server)))
		     nil
		     nil))

  (setq lyskom-default-user-name
	(if username username (getenv "KOMNAME")))
  (setq lyskom-default-password
	(if password password (getenv "KOMPASSWORD")))
  (if (zerop (length host))
      (setq host (or (getenv "KOMSERVER")
		     lyskom-default-server)))
  (let ((port 4894))
    (cond				;Allow "nanny:4892" to use port 4892.
     ((string-match ":" host)
      (setq port (string-to-int (substring host (match-end 0))))
      (cond
       ((zerop (match-beginning 0))
	(setq host lyskom-default-server))
       (t
	(setq host (substring host 0 (match-beginning 0)))))))

    (let* ((name (buffer-name
		  (generate-new-buffer
		   (concat "*"
			   (substring
			    host
			    (string-match "^[^.]*" host)
			    (match-end 0))
			   "*"))))
	   (proc (open-network-stream name name host port)))
      (setq lyskom-debug-communications-to-buffer-buffer
	    (concat name "-debugs"))
      (switch-to-buffer (process-buffer proc))
      (lyskom-mode)
      (setq lyskom-proc proc)
      (lyskom-insert
       (lyskom-format 'try-connect lyskom-clientversion host))
      (set-process-filter proc 'lyskom-connect-filter)
      (lyskom-process-send-string proc
				  (concat "A"
					  (lyskom-format-objects
					   (concat (user-login-name)
						   "%" (system-name))))))))


(defun lyskom-connect-filter (proc output)
  "Receive connection acknowledgement from server."
  (sit-for 0)
  (let ((cur (current-buffer)))
    (set-buffer (process-buffer proc))
    (cond
     (lyskom-debug-communications-to-buffer
      (set-buffer
       (get-buffer-create lyskom-debug-communications-to-buffer-buffer))
      (save-excursion
	(goto-char (point-max))
	(insert "-----> " output " <-----\n"))
      (set-buffer (process-buffer proc))))
    (cond
      ((string= output "LysKOM\n")
       (lyskom-init-parse)
       (set-process-filter proc 'lyskom-filter)
       (set-process-sentinel proc 'lyskom-sentinel)
       (setq lyskom-executing-command nil)
       (setq lyskom-is-parsing nil)
       (initiate-get-server-info 'main 'lyskom-store-server-info))
      ((string-match "\\`%%" output)
       (lyskom-format-insert 'protocoll-error output)
       (error "Protocol error. Servers says: %s" output))
      (t))
    (set-buffer cur)))


(defun lyskom-store-server-info (server-info)
  "Receive info about the server, store it in the variable lyskom-server-info
and tell the user about it."
  (if (not server-info)
      (kom-start-anew t)
    (setq lyskom-server-info server-info)
    (lyskom-insert
     (lyskom-format 
      'connection-done
      (if (zerop (% (server-info->version server-info) 100))
	  (format "%d.%d"
		  (/ (server-info->version server-info) 10000)
		  (/ (% (server-info->version server-info) 10000) 
		     100))
	(format "%d.%d.%d"
		(/ (server-info->version server-info) 10000)
		(/ (% (server-info->version server-info) 10000)
		   100)
		(% (server-info->version server-info) 100)))))
    (if (not (zerop (server-info->motd-of-lyskom lyskom-server-info)))
	(let ((motd-of-lyskom (server-info->motd-of-lyskom
			       lyskom-server-info)))
;	  (lyskom-collect 'main)
;	  (initiate-get-text-stat 'main nil motd-of-lyskom)
;	  (initiate-get-text 'main nil motd-of-lyskom)
;	  (lyskom-use 'main 'lyskom-print-text nil motd-of-lyskom)

	  ; Instead of the above.
	  (initiate-get-text 'main '(lambda (text)
				      (lyskom-insert
				       (text->text-mass text)))
			     motd-of-lyskom)
	  (lyskom-run 'main 'kom-start-anew t))
      (kom-start-anew t))))


;;; ================================================================
;;;                        Start anew


(defun kom-start-anew (&optional first-time-around)
  "Start as a new person. If the optional argument FIRST is non-nil,
this is the initial connection at startup."
  (interactive)
  (lyskom-start-of-command 'kom-start-anew)
  (if first-time-around
      nil
    (lyskom-tell-internat 'kom-tell-login))
  (if (and first-time-around
	   lyskom-default-user-name)
      (initiate-lookup-name 'main 'lyskom-start-anew-handle-conf-list
			    lyskom-default-user-name
			    lyskom-default-user-name)
    (lyskom-completing-read 'main 'lyskom-start-anew-login
			    (lyskom-get-string 'what-is-your-name)
			    'person t "")))


(defun lyskom-start-anew-login (return)
  "Handle the return of the lyskom-completing-read.
The return could be a name of a pers-no.
If it is a name then we shall start a new character.
If it is a number we shall log that person in."
  (cond
   ((stringp return)
    (lyskom-start-anew-new-person return))
   ((numberp return)
    (initiate-get-conf-stat 'main 'lyskom-start-anew-login-1-5
			    return))
   (t
    (signal 'lyskom-internal-error (list 'lyskom-completing-read
					 " returned " return)))))
			    

(defun lyskom-start-anew-handle-conf-list (conf-list name)
  "Check the number of conf-no:s in CONF-LIST. The name was NAME.
If 0, assume a new user.
If =1, login with this name.
If >1, Retry."
  (let ((len (lyskom-conf-list-length conf-list)))
    (cond ((eq len 0)
	   (lyskom-start-anew-new-person name))
	  ((eq len 1)
	   (initiate-get-conf-stat 'main 'lyskom-start-anew-login-1-5
				   (elt (conf-list->conf-nos conf-list) 0)))
	  (t 
	   (setq lyskom-executing-command nil)
	   (kom-start-anew)))))


(defun lyskom-start-anew-login-1-5 (conf-stat)
  "Echo the users name and log in as the person described by conf-stat.
Prompt user for password."
  (lyskom-insert (concat (conf-stat->name conf-stat) "\n"))
  (let ((curper lyskom-pers-no))
    (setq lyskom-pers-no 0)		; Don't handle asynchronous now.
    (initiate-login 'main 'lyskom-start-anew-login-2 
		    (conf-stat->conf-no conf-stat)
		    (if lyskom-default-password
			(prog1
			    lyskom-default-password
			  (setq lyskom-default-password nil)
			  (set-default 'lyskom-default-password nil))
		      (silent-read (lyskom-get-string 'password)))
		    (conf-stat->conf-no conf-stat)
		    curper
		    conf-stat)))


(defun lyskom-start-anew-login-2 (reply pers-no old-pers-no &optional conf-stat)
  "Handle the REPLY to a try to login on the server as PERS-NO.
If not succeeded then reset the person to OLD-PERS-NO.
Optional argument CONF-STAT is used to check for a msg-of-day on the person."
  (cond
   ((null reply)
    (lyskom-insert-string 'wrong-password)
    (setq lyskom-executing-command nil)
    (setq lyskom-pers-no old-pers-no)
    (kom-start-anew))
   (t
    (lyskom-insert-string 'are-logged-in)
    (lyskom-mode)
    (setq lyskom-membership nil)
    (setq lyskom-membership-is-read nil)
    (setq lyskom-pers-no pers-no)
    (if (and conf-stat
	     (/= (conf-stat->msg-of-day conf-stat) 0))
	(progn
	  (lyskom-insert-string 'you-have-motd)
	  (lyskom-view-text 'main (conf-stat->msg-of-day conf-stat))))
    (if (and conf-stat
	     (zerop (conf-stat->presentation conf-stat))
	     (not (zerop (conf-stat->no-of-texts conf-stat))))
	(lyskom-insert-string 'presentation-encouragement))
    (lyskom-setup-prefetch)
    (lyskom-stop-prefetch)
    (setq lyskom-membership-is-read 0)
    (setq lyskom-membership nil)
    (setq lyskom-command-to-do 'unknown)
    (setq lyskom-unread-confs)
    (setq lyskom-options-done)
    (setq lyskom-to-do-list (lyskom-create-read-list))
    (setq lyskom-reading-list (lyskom-create-read-list))
    (initiate-get-unread-confs 'main 'lyskom-register-unread-confs 
			       lyskom-pers-no)
    (lyskom-read-options)		; Check this to be finished before
					; printing the first prompt
    (initiate-get-part-of-membership 'main 'lyskom-start-anew-login-3 
				     lyskom-pers-no
				     lyskom-membership-is-read
				     lyskom-fetch-membership-length)
    (lyskom-prefetch-all-conf-stats)
    (lyskom-prefetch-marks)
    (lyskom-prefetch-who-is-on)
    (lyskom-prefetch-membership pers-no)
    ;; Don't start prefetching until we have the first prompt.
    )))


(defun lyskom-register-unread-confs (unread-confs)
  "Saves the list of UNREADCONFS in a variable for later use."
  (setq lyskom-unread-confs unread-confs))


(defun lyskom-start-anew-login-3 (part-of-membership)
  "Puts the membersship, or part of it in the lyskom-to-do-list.
If we have got enough information to be able to decide upon the first
prompt the prompt is printed and this function is left. If not, fetch
more membership.

Information required for this:
- lyskom-unread-confs
- lyskom-options-done
- an article in one of the conferences in the membership."
  (if (symbolp part-of-membership)
      nil
    (if (< (length part-of-membership) lyskom-fetch-membership-length) ; all
	(setq lyskom-membership-is-read 'almost)
      (setq lyskom-membership-is-read (+ lyskom-membership-is-read 
					 lyskom-fetch-membership-length)))
    (lyskom-append-to-membership part-of-membership))
  (if (and lyskom-unread-confs
	   lyskom-options-done	; We have the fundamental stuff
	   (or (not (read-list-isempty lyskom-to-do-list))
	       (eq lyskom-membership-is-read t))
	   )
      (lyskom-end-of-command)))
		

; obsolete
;(defun lyskom-set-membership (membership)
;  "Sets lyskom-membership to a new value.
;Args: MEMBERSHIP."
;  (setq lyskom-membership (sort (lyskom-array-to-list membership)
;				'lyskom-membership-<))
;  (setq lyskom-membership-is-read t))

(defun lyskom-append-to-membership (membership)
  "Adds a PART last in the membership-list."
  (let ((list (lyskom-array-to-list membership))
	sent)
    (lyskom-add-membership-to-membership membership)
    (lyskom-fetch-until-we-have-an-unread list)))


(defun lyskom-fetch-until-we-have-an-unread (&optional list)
  "Call to fetch info until we have decided if we have an unread. (or are done."
  (let ((sent))
    (while (and (not sent) list)
      (if (and lyskom-unread-confs
	       (not (memq (membership->conf-no (car list))
			  (lyskom-array-to-list
			   (conf-no-list->conf-nos lyskom-unread-confs)))))
	  nil
	(initiate-get-conf-stat 'main 'lyskom-decide-unread-conf
				(membership->conf-no (car list))
				(car list)
				(cdr list))
	(setq sent t))
      (setq list (cdr list)))
    (if sent
	nil
      (if (numberp lyskom-membership-is-read)
	  (initiate-get-part-of-membership 'main 'lyskom-start-anew-login-3
					   lyskom-pers-no
					   lyskom-membership-is-read
					   lyskom-fetch-membership-length)
	(setq lyskom-membership-is-read t)))
    (let ((reverse (reverse lyskom-membership)))
      (while reverse
	(lyskom-prefetch-conf (membership->conf-no (car reverse)))
	(setq reverse (cdr reverse))))))


(defun lyskom-decide-unread-conf (conf-stat membship rest-memblist)
  "Checks if we have unread in the conf CONF-STAT using MEMBERSHIP.
If not, fetch next conf-stat from REST-MEMBERSHIPLIST."
  (if (cond
       ((not conf-stat)			; error
	nil)
       ((> (+ (conf-stat->first-local-no conf-stat)
	      (conf-stat->no-of-texts conf-stat)
	      -1)
	   (membership->last-text-read membship))
	;; There are (probably) some unread texts in this conf.
	(lyskom-prefetch-map (conf-stat->conf-no conf-stat) 
			     (+ lyskom-fetch-map-nos
				(membership->last-text-read membship)))
	(initiate-get-map 'main 'lyskom-decide-unread-map
			  (conf-stat->conf-no conf-stat)
			  (1+ (membership->last-text-read membship))
			  lyskom-fetch-map-nos
			  conf-stat)
	t))
      nil
    (if rest-memblist
	(progn
	  (initiate-get-conf-stat 'main 'lyskom-decide-unread-conf
				  (membership->conf-no (car rest-memblist))
				  (car rest-memblist)
				  (cdr rest-memblist))
	  (lyskom-run 'main 'lyskom-start-anew-login-3 'cont))
      (if (numberp lyskom-membership-is-read)
	  (initiate-get-part-of-membership 'main 'lyskom-start-anew-login-3
					   lyskom-membership-is-read
					   lyskom-fetch-membership-length)
	(setq lyskom-membership-is-read t)
	(lyskom-run 'main 'lyskom-start-anew-login-3 'cont)))))


(defun lyskom-decide-unread-map (map conf)
  "Enters this map in the lyskom-to-do-list and returns to lyskom-start-anew-login-3."
  (lyskom-enter-map-in-to-do-list map conf)
  (lyskom-fetch-until-we-have-an-unread))


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
    (initiate-login 'main 'lyskom-start-anew-login-2
		    pers-no password pers-no lyskom-pers-no)
    (lyskom-run 'main 'lyskom-edit-text lyskom-proc
		       (lyskom-create-misc-list
		       'recpt
		       (server-info->pers-pres-conf lyskom-server-info))
		      (lyskom-format 'presentation-subject name)
		      (lyskom-format 'presentation-form name)
		      'lyskom-set-presentation pers-no)
    (lyskom-run 'main 'lyskom-tell-internat 'kom-tell-1st-pres))))


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
Entry to this mode runs lyskom-mode-hook."
  (interactive)
  (lyskom-clear-vars)
  (setq mode-line-buffer-identification
	(append
	 (mapcar '(lambda (str)
		    (if (stringp str)
		        (elib-string-replace-match "%[1-9][0-9]+b" str "%b")
		      str))
		 mode-line-buffer-identification)
	 '(" " mode-line-conf-name)))

  (setq major-mode 'lyskom-mode)
  (setq mode-name "LysKOM")
  (setq mode-line-process '(": %s"))
  (use-local-map lyskom-mode-map)
  (run-hooks 'lyskom-mode-hook))


(defun lyskom-clear-vars ()
  "Set up buffer-local vars."
  (let ((proc lyskom-proc)
	(ref-no lyskom-ref-no)
	(pers-no lyskom-pers-no)
	(membership lyskom-membership)
	(membership-is-read lyskom-membership-is-read)
	(last-viewed lyskom-last-viewed)
	(replies-buffer lyskom-unparsed-buffer)
	(replies-marker lyskom-unparsed-marker)
	(server-info lyskom-server-info)
	(default-password lyskom-default-password))
    (kill-all-local-variables)
    (make-local-variable 'lyskom-default-password)
    (make-local-variable 'lyskom-unparsed-buffer)
    (make-local-variable 'lyskom-unparsed-marker)
    (make-local-variable 'lyskom-is-writing)
    (make-local-variable 'lyskom-pending-calls)
    (make-local-variable 'lyskom-number-of-pending-calls)
    (make-local-variable 'lyskom-ref-no)
    (make-local-variable 'lyskom-errno)
    (make-local-variable 'lyskom-pers-no)
    (make-local-variable 'lyskom-session-no)
    (make-local-variable 'lyskom-proc)
    (make-local-variable 'lyskom-conf-cache)
    (make-local-variable 'lyskom-pers-cache)
    (make-local-variable 'lyskom-text-cache)
    (make-local-variable 'lyskom-text-mass-cache)
    (make-local-variable 'lyskom-server-info)
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
    (make-local-variable 'lyskom-debug-communications-to-buffer)
    (make-local-variable 'lyskom-debug-communications-to-buffer-buffer)
    (setq lyskom-default-password default-password)
    (setq lyskom-proc proc)
    (setq lyskom-ref-no ref-no)
    (setq lyskom-pers-no pers-no)
    (setq lyskom-membership membership)
    (setq lyskom-last-viewed last-viewed)
    (setq lyskom-membership-is-read membership-is-read)
    (setq lyskom-unparsed-buffer replies-buffer)
    (setq lyskom-unparsed-marker replies-marker)
    (setq lyskom-server-info server-info)
    (setq lyskom-do-when-done (cons kom-do-when-done kom-do-when-done))
    (setq lyskom-output-queue (lyskom-queue-create))
    (lyskom-set-mode-line (lyskom-get-string 'not-present-anywhere))))
