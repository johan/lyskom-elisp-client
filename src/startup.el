;;;;;
;;;;; $Id: startup.el,v 44.13 1997-01-31 15:46:53 davidk Exp $
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
;;;; File: startup.el
;;;;
;;;; This file contains functions that are called only when lyskom
;;;; is loaded, started or when a new user is logged in during a 
;;;; session.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: startup.el,v 44.13 1997-01-31 15:46:53 davidk Exp $\n"))


;;; ================================================================
;;;                         Start kom.


;;;###autoload
(defun lyskom (&optional host username password session-priority)
  "Start a LysKOM session.
Optional arguments: HOST, USERNAME and PASSWORD.

See lyskom-mode for details."
  (interactive (list (lyskom-read-server-name
		      (lyskom-format 'server-q
				     (or (getenv "KOMSERVER")
					 lyskom-default-server)))
		     nil
		     nil
		     (if current-prefix-arg
			 (prefix-numeric-value current-prefix-arg)
		       nil)))

  (run-hooks 'lyskom-init-hook)
  (setq username
	(or username (getenv "KOMNAME")))
  (setq password
	(or password (getenv "KOMPASSWORD")))
  (if (zerop (length host))
      (let* ((env-kom (getenv "KOMSERVER"))
	     (canon (rassoc env-kom kom-server-aliases)))
	(setq host (or (car canon)
		       env-kom
		       lyskom-default-server))))
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

    (let* ((buffer (get-buffer host))
	   (name nil)
	   (proc nil))
      (if (and buffer
	       (lyskom-buffer-p buffer)
	       (not (prog1
			(j-or-n-p (lyskom-get-string
				   'start-new-session-same-server))
		      (message ""))))
	  (switch-to-buffer buffer)
	(unwind-protect
	    (progn
	      (cond ((and buffer (not (lyskom-buffer-p buffer)))
		     (set-buffer buffer)
		     (goto-char (point-max))
		     (let ((time (decode-time (current-time))))
		       (setcar (cdr (cdr (cdr (cdr time))))
			       (1- (car (cdr (cdr (cdr (cdr time)))))))
		       (setcar (cdr (cdr (cdr (cdr (cdr time)))))
			       (- (car (cdr (cdr (cdr (cdr (cdr time))))))
				  1900))
		       (lyskom-insert
			(format (lyskom-get-string 'new-session-in-buffer)
				(lyskom-format-time
				 (apply 'lyskom-create-time time))))
		       (setq name (buffer-name buffer))))
		    (t
		     (setq buffer (generate-new-buffer host))
		     (setq name (buffer-name buffer))))
	      (setq proc (open-network-stream name buffer host port))
	      (switch-to-buffer buffer)
	      (lyskom-mode)		;Clearing lyskom-default...
	      (if session-priority
		  (setq lyskom-session-priority session-priority))
	      (setq lyskom-buffer buffer)
	      (setq lyskom-default-user-name username)
	      (setq lyskom-default-password password)
	      (setq lyskom-server-name host)
	      (setq lyskom-proc proc)
	      (lyskom-setup-faces)
	      (lyskom-insert
	       (lyskom-format 'try-connect lyskom-clientversion host))
	      (set-process-filter proc 'lyskom-connect-filter)
	      (lyskom-process-send-string
	       proc
	       (concat "A"
		       (lyskom-format-objects
			(concat (user-login-name)
				"%" (system-name)))))
	      (while (eq 'lyskom-connect-filter (process-filter proc))
		(accept-process-output proc))
	      ;; Now we have got the correct response.
	      (set-process-sentinel proc 'lyskom-sentinel)

	      (save-excursion
		(lyskom-init-parse buffer))

	      ;; +++PREFETCH+++
	      (lyskom-setup-prefetch)

	      ;; Tell the server who we are
	      (initiate-set-client-version 'background nil
					   "lyskom.el" lyskom-clientversion)

	      (setq lyskom-server-info (blocking-do 'get-server-info))
	      (setq
	       lyskom-server-version
	       (list (/ (server-info->version lyskom-server-info) 10000)
		     (/ (% (server-info->version lyskom-server-info) 10000)
			100)
		     (% (server-info->version lyskom-server-info) 100)))
	      (lyskom-setup-client-for-server-version)
	      (lyskom-format-insert 
	       'connection-done
	       (if (zerop (elt lyskom-server-version 2))
		   (format "%d.%d"
			   (elt lyskom-server-version 0)
			   (elt lyskom-server-version 1))
		 (format "%d.%d.%d"
                         (elt lyskom-server-version 0)
                         (elt lyskom-server-version 1)
                         (elt lyskom-server-version 2))))
	      (if (not (zerop (server-info->motd-of-lyskom
			       lyskom-server-info)))
		  (let ((text (blocking-do 'get-text 
					   (server-info->motd-of-lyskom
					    lyskom-server-info))))
		    (lyskom-insert 
		     (if text
			 (text->text-mass text)
		       (lyskom-get-string 'lyskom-motd-was-garbed)))
		    (lyskom-insert "\n")))
	      ;; Can't use lyskom-end-of-command here.
	      (setq lyskom-executing-command nil) 
	      ;; Log in
	      (kom-start-anew t)
	      (if (memq lyskom-buffer lyskom-buffer-list)
		  (while (not (eq lyskom-buffer (car lyskom-buffer-list)))
		    (setq lyskom-buffer-list
			  (nconc (cdr lyskom-buffer-list)
				 (list (car lyskom-buffer-list)))))
		(setq lyskom-buffer-list
		      (cons lyskom-buffer lyskom-buffer-list)))
	      (setq init-done t))
	  ;; Something went wrong. Lets cleanup everything. :->
	  (if init-done
	      nil
	    (if proc (delete-process proc))
	    (kill-buffer buffer)))))))


(defun lyskom-setup-client-check-version (spec version)
  (let ((relation (elt spec 0))
        (major (elt spec 1))
        (minor (elt spec 2))
        (revision (elt spec 3)))
    (cond ((eq relation '=)
           (and (or (null major) (= major (elt version 0)))
                (or (null minor) (= minor (elt version 1)))
                (or (null revision) (= revision (elt version 2)))))
          ((eq relation '>=)
           (or (and (= (elt version 0) major)
                    (= (elt version 1) minor)
                    (>= (elt version 2) revision))
               (and (= (elt version 0) major)
                    (> (elt version 1) minor))
               (and (> (elt version 0) major))))
          ((eq relation '<)
           (or (and (= (elt version 0) major)
                    (= (elt version 1) minor)
                    (< (elt version 2) revision))
               (and (= (elt version 0) major)
                    (< (elt version 1) minor))
               (and (< (elt version 0) major)))))))


(defun lyskom-setup-client-for-server-version ()
 "Set up the supports list and flags for the current server. See
variable documentation for lyskom-server-feautres"
  (let ((result nil)
        (flags nil))
    (mapcar (function
             (lambda (spec)
               (let ((spec-version (elt spec 0))
                     (plist (elt spec 1)))
                 (if (and (lyskom-setup-client-check-version 
                           spec-version
                           lyskom-server-version))
                     (mapcar
                      (function
                       (lambda (x)
                         (cond ((consp x) (setq result (cons x result)))
                               ((symbolp x) (setq flags (cons x flags)))
                               (t (setq result (cons (cons x t) 
                                                     result))))))
                      plist)))))
            lyskom-server-features)
    (mapcar (function (lambda (x) (set x t))) flags)
    (setq lyskom-server-supports result)))
                                             
                                                                 

(defun lyskom-connect-filter (proc output)
  "Receive connection acknowledgement from server."
  (if lyskom-debug-communications-to-buffer
      (lyskom-debug-insert proc "-----> " output))
  (cond
   ((string-match "^LysKOM\n" output)
    (set-process-filter proc 'lyskom-filter))))





;;; ================================================================
;;;                        Start anew


(defun kom-start-anew (&optional lyskom-first-time-around)
  "Start as a new person."
  (interactive)
  (lyskom-start-of-command 'kom-start-anew)
  (lyskom-completing-clear-cache)
  (let ((old-me lyskom-pers-no)
	(login-successful nil))
    (unwind-protect
        (progn
          (if lyskom-first-time-around
              nil
            (lyskom-tell-internat 'kom-tell-login))
          (setq lyskom-pers-no nil)
          (while (not lyskom-pers-no)

            (if (and lyskom-first-time-around
                     lyskom-default-user-name)
                ;; This is nil if we can't find a unique match.
                (setq lyskom-pers-no
                      (conf-z-info->conf-no
                       (lyskom-lookup-conf-by-name lyskom-default-user-name
                                                   '(pers)))))
            (if lyskom-pers-no
                nil
              (let ((name (lyskom-read-conf-name
                           (lyskom-get-string 'what-is-your-name)
                           '(pers none) t "" t)))
                (setq lyskom-pers-no
                      (or (conf-z-info->conf-no 
                           (lyskom-lookup-conf-by-name name '(pers)))
                          (lyskom-create-new-person name)))))
            ;; Now lyskom-pers-no contains a number of a person.
            ;; Lets log him in.
            (if lyskom-pers-no
                (let ((conf-stat (blocking-do 'get-conf-stat lyskom-pers-no))
                      (lyskom-inhibit-minibuffer-messages t))

		  ;; DEBUG
		  (if (null conf-stat)
		      (lyskom-insert "You don't exist. Go away.\n"))

                  (lyskom-insert (concat (conf-stat->name conf-stat) "\n"))
                  (setq lyskom-first-time-around nil)
                  (if (blocking-do 'login lyskom-pers-no
                                   (if lyskom-default-password
                                       (prog1
                                           lyskom-default-password
                                         (setq lyskom-default-password nil)
                                         (set-default 'lyskom-default-password
                                                      nil))
                                     ;; Use password read when creating
                                     ;; the person when loggin in new
                                     ;; users
                                     (or lyskom-is-new-user
                                         (silent-read
					  (lyskom-get-string 'password)))))
		      (progn
			(if lyskom-is-new-user
			    (blocking-do 'add-member
					 (server-info->conf-pres-conf lyskom-server-info)
					 lyskom-pers-no 100 1))
			(setq login-successful t))
                    (lyskom-insert-string 'wrong-password)
                    (setq lyskom-pers-no nil))
                  (setq lyskom-is-new-user nil))))
	  
          ;; Now we are logged in.
          (lyskom-insert-string 'are-logged-in)
          (let ((conf-stat (blocking-do 'get-conf-stat lyskom-pers-no)))
            (if (and conf-stat
                     (/= (conf-stat->msg-of-day conf-stat) 0))
                (progn
                  (lyskom-insert-string 'you-have-motd)
                  (let ((lyskom-show-comments ; +++SOJGE
                         (not kom-no-comments-to-motd)))
                    (lyskom-view-text (conf-stat->msg-of-day conf-stat)))))
            (if (and conf-stat
                     (zerop (conf-stat->presentation conf-stat))
                     (not (zerop (conf-stat->no-of-texts conf-stat))))
                (lyskom-insert-string 'presentation-encouragement)))
          (if (not lyskom-dont-read-user-area)
              (lyskom-read-options))
          (setq lyskom-current-conf 0)
          (lyskom-refetch)
          ;; (cache-initiate-who-info-buffer (blocking-do 'who-is-on))
          (cache-set-marked-texts (blocking-do 'get-marks))
          ;; What is this variable? It is never used. It is ust to
          ;; fill the cache?
          (let ((lyskom-who-am-i (blocking-do 'who-am-i)))
            (if lyskom-who-am-i (setq lyskom-session-no lyskom-who-am-i))))
	  
      ;; If something failed, make sure we are someone
      (if login-successful
	  (clear-all-caches)
	(setq lyskom-pers-no old-me))
      (setq lyskom-is-new-user nil)
      (lyskom-end-of-command)))
  ;; Run the hook kom-login-hook. We don't want to hang the
  ;; login, just because something crashed here.
  (condition-case err
      (progn
        (run-hooks 'lyskom-login-hook)
        (run-hooks 'kom-login-hook))
    (error (lyskom-format-insert-before-prompt
            'error-in-login-hook (format "%s" err)))))


(defun lyskom-refetch ()
  "Resets and fetches all reading info.
This is called at login and after prioritize and set-unread."
  ;; +++PREFETCH+++

  ;; The whole membership!
  ;; (lyskom-set-membership (blocking-do 'get-membership lyskom-pers-no))
  ;; (setq lyskom-membership-is-read t)
  ;; (setq lyskom-unread-confs (blocking-do 'get-unread-confs lyskom-pers-no))

  (setq lyskom-membership nil)

  (lyskom-reset-prefetch)
  (lyskom-start-prefetch)
  (lyskom-prefetch-membership lyskom-pers-no)

  (setq lyskom-to-do-list (lyskom-create-read-list))
  (setq lyskom-reading-list (lyskom-create-read-list)))


(defun lyskom-set-membership (membership)
  "Sets lyskom-membership to a new value.
Args: MEMBERSHIP."
  (setq lyskom-membership (sort (listify-vector membership)
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


(defun lyskom-create-new-person (name)
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
	;; Failed to enter the same password twice
	(lyskom-insert-string 'repeat-failure)
	nil)
       (t
	;; Entered the same password twice
	(let ((new-person (blocking-do 'create-person name password)))
	  (if (null new-person)
	      (lyskom-insert-string 'could-not-create-you)
	    ;; Raise a flag so the user will be added to the
	    ;; presentation conference after login
	    (setq lyskom-is-new-user password))
	  new-person)))))
   (t
    ;; Do not create a new person
    nil)))


;;(defun lyskom-start-anew-create-handler (pers-no name password)
;;  "A new person has been created. Log in as him."
;;  (cond
;;   ((null pers-no)
;;    (lyskom-insert-string 'could-not-create-you)
;;    (setq lyskom-executing-command nil)
;;    (kom-start-anew))
;;   (t
;;    (initiate-login 'main 'lyskom-add-for-new-person
;;		    pers-no password pers-no lyskom-pers-no)
;;    )))

;;(defun lyskom-add-for-new-person (reply pers-no lyskom-pers-no)
;;  "Add a news person as member in the default presentation conference."
;;  (initiate-add-member 'main 'lyskom-start-anew-login-2
;;		       (server-info->conf-pres-conf lyskom-server-info)
;;		       pers-no 100 1
;;		       pers-no lyskom-pers-no))


(defun lyskom-read-server-name (prompt)
  "Read the name of a LysKOM server.
Copmpletion is done on the servers i kom-server-aliases. If an
alias name is entered, the corresponding address is returned."
  ;; Create a completion table like
  ;; (("kom.lysator.liu.se" . "kom.lysator.liu.se")
  ;;  ("LysKOM" . "kom.lysator.liu.se"))
  (let ((known-servers
	 (append (mapcar (function (lambda (pair)
				     (cons (car pair) (car pair))))
			 kom-server-aliases)
		 (mapcar (function (lambda (pair)
				     (cons (cdr pair) (car pair))))
			 kom-server-aliases)))
	(completion-ignore-case t)
	server)
    (setq server (completing-read prompt known-servers nil nil))
    (or (cdr (assoc server known-servers))
	server)))
      

;;; ================================================================
;;;                        The LysKOM mode.

;; The LysKOM mode should not be inherited if we create a new buffer
;; and default-major-mode is nil.
(put 'lyskom-mode 'mode-class 'special)

(defun lyskom-mode ()
  "\\<lyskom-mode-map>Mode for LysKOM client.
Commands:
\\[kom-next-command]	Do the default action. This can be to read the next text,select
n	ext conference with unread texts or whatever the prompt says.
\\[kom-go-to-conf]	Go to a conference. LysKOM will ask you for a conference
	and make you a member of it if you are not already.
\\[kom-list-conferences]	List conferences matching a given string.
\\[kom-list-persons]	List persons matching a given string.

\\[kom-list-news]	List the conferences you have unread texts in.
\\[kom-go-to-next-conf]	Go to the next conference with unread texts.

\\[kom-membership]	Display a buffer with the list of conferences you are member in.

\\[kom-quit]	Leave this LysKOM session.
\\[kom-who-is-on]	Show a list of all the users of lyskom right now.

\\[kom-extended-command]	Read a command using the minibuffer and execute it.
	This is another way to give commands.

\\[kom-write-text]	Start writing a new text.
\\[kom-write-comment]	Start writing a comment to the last read article.
\\[kom-private-answer]	Start writing a personal answer to the author of the last
	read article.
\\[kom-send-letter]	Start writing a letter to a person or conference.

\\[kom-page-next-command]	Clear the page and do what \\[kom-next-command] does.
\\[kom-line-next-command]	Do what \\[kom-next-command] does, but scroll at most 1 line.

0 .. 9	Give a numeric argument to the next command.
\\[describe-mode]	Display this help text.

\\[kom-busy-wait]	Put the lyskom-session in wait mode. The next created text with 
	a priotity higher that that of the next conference you are going
	to will be read directly when it is created.
\\[kom-set-unread]	Mark a number of texts as unread.
\\[kom-jump]	Skip (mark as read) all the comments to this article recursively.
\\[kom-display-time]	Show the current date and time.

\\[kom-change-presentation]	Change your presentation.
\\[kom-view]	View the specified text.
\\[kom-view-commented-text]	View the text that the current text comments or is a footnote to.
\\[kom-review-presentation]	Show the presentation for a person or a conferencce.

\\[kom-review-comments]	View all comments to the current text.
\\[kom-review-tree]	View all comments to the current text and step through the tree
	in depth-first order.

\\[kom-find-root-review]	View the complete comment tree.
\\[kom-find-root]	Show the root text of this comment tree.
\\[kom-review-by-to]	View the last (first or all) article written by named author
	in a named conference.

\\[kom-mark-text]	Create a mark on a text.
\\[kom-unmark-text]	Remove the mark on a text.
\\[kom-review-marked-texts]	View all text marked with a certain mark.
\\[kom-review-all-marked-texts]	View all marked text.

\\[kom-view-next-new-text]	Read the next text from the list of unread.
\\[kom-review-next]	Continue the viewing.
\\[kom-review-stack]	Show the stack of things we are viewing.
\\[kom-review-clear]	Clear the stack of things we are viewing.
\\[kom-review-backward]	Toggles the read order of reviewed texts. This can only be done
	when viewing texts with \\[kom-review-by-to], \\[kom-review-marked-texts] and \\[kom-review-all-marked-texts].

\\[kom-status-conf]	Show the status of a conference.
\\[kom-status-person]	Show the status of a person
\\[kom-save-text]	Save the text you are looking at to a file.

\\[kom-get-abuse]	Get an insulting text.
\\[kom-get-appreciation]	Get an encouraging text.

\\[kom-add-self]	Become a member of a conference.
\\[kom-sub-self]	Removes you as a member of a conference.

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
  (setq mode-line-process (lyskom-get-string 'mode-line-working))
  (use-local-map lyskom-mode-map)
  (lyskom-count-down-edits)
  (run-hooks 'lyskom-mode-hook)
  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only t))

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
    (make-local-variable 'kom-ansaphone-show-messages)
    (make-local-variable 'kom-ansaphone-record-messages)
    (make-local-variable 'kom-ansaphone-on)
    (make-local-variable 'kom-ansaphone-default-reply)
    (make-local-variable 'kom-friends)
    (make-local-variable 'kom-login-hook)
    (make-local-variable 'kom-membership-default-priority)
    (make-local-variable 'kom-permanent-filter-list)
    (make-local-variable 'kom-prompt-format)
    (make-local-variable 'kom-remote-control)
    (make-local-variable 'kom-remote-controllers)
    (make-local-variable 'kom-session-filter-list)
    (make-local-variable 'lyskom-accept-async-flag)
    (make-local-variable 'lyskom-blocking-return)
    (make-local-variable 'lyskom-buffer)
    (make-local-variable 'lyskom-command-to-do)
    (make-local-variable 'lyskom-conf-cache)
    (make-local-variable 'lyskom-count-var)
    (make-local-variable 'lyskom-current-conf)
    (make-local-variable 'lyskom-current-subject)
    (make-local-variable 'lyskom-current-text)
    (make-local-variable 'lyskom-default-password)
    (make-local-variable 'lyskom-default-user-name)
    (make-local-variable 'lyskom-do-when-done)
    (make-local-variable 'lyskom-dynamic-session-info-flag)
    (make-local-variable 'lyskom-dont-change-prompt)
    (make-local-variable 'lyskom-errno)
    (make-local-variable 'lyskom-executing-command)
    (make-local-variable 'lyskom-filter-list)
    (make-local-variable 'lyskom-idle-time-flag)
    (make-local-variable 'lyskom-is-administrator)
    (make-local-variable 'lyskom-is-parsing)
    (make-local-variable 'lyskom-is-waiting)
    (make-local-variable 'lyskom-is-writing)
    (make-local-variable 'lyskom-last-group-message-recipient)
    (make-local-variable 'lyskom-last-personal-message-sender)
    (make-local-variable 'lyskom-last-viewed)
    (make-local-variable 'lyskom-list-of-edit-buffers)
    (make-local-variable 'lyskom-long-conf-types-flag)
    (make-local-variable 'lyskom-marked-text-cache)
    (make-local-variable 'lyskom-membership)
    (make-local-variable 'lyskom-membership-is-read)
    (make-local-variable 'lyskom-current-prompt)
    (make-local-variable 'lyskom-normally-read-texts)
    (make-local-variable 'lyskom-number-of-pending-calls)
    (make-local-variable 'lyskom-options-done)
    (make-local-variable 'lyskom-other-clients-user-areas)
    (make-local-variable 'lyskom-output-queues)
    (make-local-variable 'lyskom-pending-calls)
    (make-local-variable 'lyskom-pending-prefetch)
    (make-local-variable 'lyskom-pers-cache)
    (make-local-variable 'lyskom-pers-no)
    (make-local-variable 'lyskom-prefetch-conf-tresh)
    (make-local-variable 'lyskom-prefetch-confs)
    (make-local-variable 'lyskom-prefetch-in-action)
    (make-local-variable 'lyskom-prefetch-pending-prefetch)
    (make-local-variable 'lyskom-prefetch-stack)
    (make-local-variable 'lyskom-prefetch-texts)
    (make-local-variable 'lyskom-previous-text)
    (make-local-variable 'lyskom-prioritize-buffer)
    (make-local-variable 'lyskom-proc)
    (make-local-variable 'lyskom-reading-list)
    (make-local-variable 'lyskom-server-info)
    (make-local-variable 'lyskom-server-name)
    (make-local-variable 'lyskom-server-version)
    (make-local-variable 'lyskom-server-supports)
    (make-local-variable 'lyskom-long-conf-types-flag)
    (make-local-variable 'lyskom-set-last-read-flag)
    (make-local-variable 'lyskom-uconf-stats-flag)
    (make-local-variable 'lyskom-z-lookup-flag)
    (make-local-variable 'lyskom-session-no)
    (make-local-variable 'lyskom-session-priority)
    (make-local-variable 'lyskom-text-cache)
    (make-local-variable 'lyskom-text-mass-cache)
    (make-local-variable 'lyskom-to-be-printed-before-prompt)
    (make-local-variable 'lyskom-to-do-list)
    (make-local-variable 'lyskom-unparsed-buffer)
    (make-local-variable 'lyskom-unparsed-marker)
    (make-local-variable 'lyskom-unread-confs)
    (make-local-variable 'lyskom-what-i-am-doing)
    (make-local-variable 'lyskom-who-info-buffer)
    (make-local-variable 'lyskom-who-info-buffer-is-on)
    (make-local-variable 'lyskom-who-info-cache)
    (make-local-variable 'lyskom-collate-table)
    (make-local-variable 'mode-line-conf-name)
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
    (setq lyskom-output-queues (make-vector 10 nil))
    (setq lyskom-collate-table lyskom-default-collate-table)
    (let ((i 0))
      (while (< i 10)
	(aset lyskom-output-queues i (lyskom-queue-create))
	(++ i)))
    (setq lyskom-list-of-edit-buffers nil)
    (setq lyskom-pending-calls nil)
    (lyskom-set-mode-line (lyskom-get-string 'not-present-anywhere))))
