;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: startup.el,v 44.78 2002-08-01 18:24:08 byers Exp $
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
;;;; File: startup.el
;;;;
;;;; This file contains functions that are called only when lyskom
;;;; is loaded, started or when a new user is logged in during a 
;;;; session.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: startup.el,v 44.78 2002-08-01 18:24:08 byers Exp $\n"))


;;; ================================================================
;;;                         Start kom.


(defvar lyskom-www-proxy-connect-phase 1
  "Phase when reading connection response from www-proxy:
1: (initial phase) waiting for the
   string \"HTTP/1.0 2000 Connection established\"
2: we have seen the connection string. After this, it may come lines stating
   proxy-agents and other things. They all seem to end with an empty line,
   so in this phase we wait for an empty line.")


(eval-and-compile
  (defun lyskom-compilation-in-progress ()
    "Returns non-nil if the client currently is being compiled, else nil."

    ;; This horrid code is inspired from how cl-compiling-file in the
    ;; cl package works.
    (let ((outbuffersym (lyskom-xemacs-or-gnu 'byte-compile-outbuffer
                                              'outbuffer)))
      (and (boundp outbuffersym)
           (bufferp (symbol-value outbuffersym))
           (equal (buffer-name (symbol-value outbuffersym))
                  " *Compiler Output*")))))


(defconst lyskom-is-running-compiled
  (eval-when-compile (lyskom-compilation-in-progress))
  "Non-nil if the client is running compiled, else nil.")


;;;###autoload
(defun lyskom (&optional host username password session-priority invisiblep)
  "Start a LysKOM session.
Optional arguments: HOST, USERNAME, PASSWORD and INVISIBLEP.

A numeric prefix argument sets the session priority. A prefix argument
of `C-u', on the other hand, logs in the session without notifying other
clients of the event. See lyskom-mode for details on lyskom."
  (interactive (list (lyskom-read-server-name
		      (lyskom-format 'server-q
				     (or (getenv "KOMSERVER")
					 lyskom-default-server)))
		     nil
		     nil
		     (if current-prefix-arg
			 (prefix-numeric-value current-prefix-arg)
		       nil)
		     (and current-prefix-arg (listp current-prefix-arg))))

  (run-hooks 'lyskom-init-hook)
  (setq username
	(or username (getenv "KOMNAME")))
  (setq password
	(or password (getenv "KOMPASSWORD")))
  (if (zerop (length host))
      (let* ((env-kom (getenv "KOMSERVER"))
	     (canon (lyskom-string-rassoc env-kom kom-server-aliases)))
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

    (let* ((duplicate-buffers
            (delq nil
                  (mapcar (lambda (buffer)
                            (and (lyskom-buffer-p buffer t)
                                 (buffer-name buffer)
                                 (string-match (regexp-quote host)
                                               (buffer-name buffer))
                                 (save-excursion (set-buffer buffer)
                                                 (and 
                                                  (boundp 'lyskom-server-port)
                                                  (eq port lyskom-server-port)))
                                 buffer))
                          (buffer-list))))
	   (name nil)
	   (proc nil)
           (buffer (car duplicate-buffers))
	   ;; (alive (lyskom-buffer-p buffer))
           (reused-buffer nil))
      

      ;;<<<<<<< startup.el
      ;;      (if (and buffer
      ;;	       alive
      ;;=======
      (if (and (lyskom-buffer-p buffer nil)
	       (not (prog1
			(j-or-n-p (lyskom-get-string
				   'start-new-session-same-server))
		      (message ""))))
	  (progn 
            (switch-to-buffer buffer)
            (setq reused-buffer t))
	(unwind-protect
	    (progn
	      (cond ((and buffer (not (lyskom-buffer-p buffer)))
		     (set-buffer buffer)
                     (setq reused-buffer t)
		     (goto-char (point-max))
                     (lyskom-insert
                      (format (lyskom-get-string 'new-session-in-buffer)
                              (lyskom-format-time
                               'timeformat-day-yyyy-mm-dd-hh-mm-ss)))
                     (setq name (buffer-name buffer)))
		    (t
		     (setq buffer (lyskom-generate-new-buffer host))
		     (setq name (buffer-name buffer))))
              (let* ((www-proxy-host-string
                      (cond ((stringp kom-www-proxy) kom-www-proxy)
                            ((listp kom-www-proxy)
                             (or (cdr (lyskom-string-assoc host kom-www-proxy))
                                 (cdr (assq t kom-www-proxy))
                                 nil))
                            (t nil)))
                     (www-proxy-host nil)
                     (www-proxy-port nil)
                     (match (string-match "\\(.*\\):\\([0-9]+\\)"
                                          (or www-proxy-host-string "")))
                     (headers nil))
                (setq www-proxy-host (or (and match
                                          (match-string 1 www-proxy-host-string))
                                     www-proxy-host-string)
                      www-proxy-port (or (and match
                                          (string-to-int
                                           (match-string 2
                                                         www-proxy-host-string)))
                                     80))
                (cond (www-proxy-host
                       (setq headers 
                             (cond ((stringp kom-www-proxy-headers)
                                    (list kom-www-proxy-headers))
                                   ((and (listp kom-www-proxy-headers)
                                         (stringp (car kom-www-proxy-headers)))
                                    kom-www-proxy-headers)
                                   ((and (listp kom-www-proxy-headers)
                                         (consp (car kom-www-proxy-headers)))
                                    (cdr (or (lyskom-string-assoc www-proxy-host
                                                                  kom-www-proxy-headers)
                                             (assq t kom-www-proxy-headers))))))
                       (setq proc (lyskom-open-network-stream name buffer
                                                              www-proxy-host
                                                              www-proxy-port))
                       ;; We do explicit coding
                       (set-process-coding-system proc 'no-conversion 'no-conversion)

		       ;; Install our filter.
		       ;; Do this before we send the CONNECT command to
		       ;; the proxy, in case the proxy answers fast.
		       (setq lyskom-www-proxy-connect-phase 1)
		       (set-process-filter proc
					   'lyskom-www-proxy-connect-filter)
		       (process-kill-without-query proc nil)

                       (lyskom-process-send-string 
                        proc
                        (format "CONNECT %s:%d HTTP/1.0\r\n"
                                host port))

                       (mapcar (lambda (header)
                                 (lyskom-process-send-string proc header)
                                 (lyskom-process-send-string proc "\r\n"))
                               headers)
                       (lyskom-process-send-string proc "\r\n")


		       ;; Now wait for the answer from the proxy
		       ;;
		       ;; This is because anything we send before the
		       ;; connection ack will be thrown away by the proxy
		       ;; so it is bad to try to start talking with the
		       ;; server before the connection is up.
		       (while (eq 'lyskom-www-proxy-connect-filter
				  (process-filter proc))
			 (accept-process-output proc))
		       ;; Now the proxy has connected to the kom server
		       )
                      (t (setq proc (lyskom-open-network-stream name buffer
                                                                host port))
                         ;; We do explicit coding
                         (set-process-coding-system proc 'no-conversion 'no-conversion))))
	      (switch-to-buffer buffer)
	      (lyskom-mode)		;Clearing lyskom-default...
	      (setq lyskom-buffer buffer)
	      (setq lyskom-default-user-name username)
	      (setq lyskom-default-password password)
	      (setq lyskom-server-name host)
	      (setq lyskom-server-port port)
	      (setq lyskom-proc proc)
	      (lyskom-setup-faces)
	      (lyskom-insert
	       (lyskom-format 'try-connect lyskom-clientversion host))
	      (set-process-filter proc 'lyskom-connect-filter)
              (process-kill-without-query proc nil)
	      (lyskom-process-send-string
	       proc
	       (concat "A"
		       (lyskom-format-objects
			(concat (user-login-name)
				"%" (system-name)))
                       "\n"))
	      (while (eq 'lyskom-connect-filter (process-filter proc))
		(accept-process-output proc))
	      ;; Now we have got the correct response.
	      (set-process-sentinel proc 'lyskom-sentinel)

	      (save-excursion
		(lyskom-init-parse buffer))

              ;; Async messages
              (lyskom-accept-async)

	      ;; +++PREFETCH+++
	      (lyskom-setup-prefetch)

	      ;; Tell the server who we are
	      (initiate-set-client-version 'background nil
					   "lyskom.el" lyskom-clientversion)

              (setq lyskom-server-version-info (blocking-do 'get-version-info))
              (when (or (null lyskom-server-version-info)
                        (<= (version-info->protocol-version 
                             lyskom-server-version-info) 7))
                (lyskom-error 'too-old-server))
	      (lyskom-setup-client-for-server-version)

	      (setq lyskom-server-info (blocking-do 'get-server-info))
	      (lyskom-format-insert 
	       'connection-done
	       (version-info->software-version lyskom-server-version-info))

              (when (lyskom-have-call 85)
                (setq lyskom-collate-table (blocking-do 'get-collate-table))
                (setq lyskom-char-classes nil)
                (lyskom-update-command-completion))
	      (if (not (zerop (server-info->motd-of-lyskom
			       lyskom-server-info)))
		  (blocking-do-multiple ((text (get-text 
                                                (server-info->motd-of-lyskom
                                                 lyskom-server-info)))
                                         (text-stat (get-text-stat
                                                     (server-info->motd-of-lyskom
                                                      lyskom-server-info))))
		    (lyskom-format-insert "%#1t\n"
		     (if (and text text-stat)
			 (text->decoded-text-mass text text-stat)
		       (lyskom-get-string 'lyskom-motd-was-garbed))
                     (when kom-highlight-text-body
                       '(face kom-text-body-face)))))

	      ;; Can't use lyskom-end-of-command here.
	      (setq lyskom-executing-command nil)
	      ;; Log in
	      (kom-start-anew t session-priority invisiblep)
	      (if (memq lyskom-buffer lyskom-buffer-list)
		  (while (not (eq lyskom-buffer (car lyskom-buffer-list)))
		    (setq lyskom-buffer-list
			  (nconc (cdr lyskom-buffer-list)
				 (list (car lyskom-buffer-list)))))
		(setq lyskom-buffer-list
		      (cons lyskom-buffer lyskom-buffer-list)))
              ;; We're done
	      (setq init-done t)
              )
	  ;; Something went wrong. Lets cleanup everything. :->
	  (if init-done
	      nil
	    (if proc (delete-process proc))
	    (unless reused-buffer (kill-buffer buffer))))))))


(defun lyskom-accept-async ()
  (blocking-do 'accept-async '(5 7 8 9 11 12 13 14 15 16 17 18))
  (let* ((ans (blocking-do 'query-async)))
    (unless (memq 15 (listify-vector ans))
      (blocking-do 'accept-async '(0 5 7 8 9 11 12 13 14 16 17 18)))))


(defun lyskom-www-proxy-connect-filter (proc output)
  "Receive connection acknowledgement from proxy."
  (if lyskom-debug-communications-to-buffer
      (lyskom-debug-insert proc "-----> " output))
  (cond
   ((and (= lyskom-www-proxy-connect-phase 1)
	 (string-match "^HTTP/1\\.. 200.*\r\n" output))
    (setq lyskom-www-proxy-connect-phase 2)
    ;; safety check: see if the empty line is already in this output
    (lyskom-www-proxy-connect-filter proc output))

   ((and (= lyskom-www-proxy-connect-phase 2)
	 (string-match "^\r\n" output))
    (set-process-filter proc 'lyskom-connect-filter))))

(defun lyskom-open-network-stream (name buffer host service)
  (let ((relay (lyskom-setup-ssh-relay host service buffer)))
    (if relay
        (open-network-stream name
                             buffer
                             "127.0.0.1"
                             (get relay 'relay-port))
    (open-network-stream name
                         buffer
                         host
                         service))))


(defun lyskom-setup-ssh-relay (server port kom-buffer)
  (when kom-ssh-relay-host
    (let* ((procname (format "ssh<%s:%s:%d>" kom-ssh-relay-host server port))
           (bufname (concat " *" procname "*"))
           (proc (get-process procname))
           (procsym (intern procname))
           (relay-port (and proc
                            (eq (process-status proc) 'run)
                            (get procsym 'relay-port)))
           (msg nil))
      (unwind-protect
          (save-excursion
            (set-buffer (get-buffer-create bufname))
            (if relay-port
                (lyskom-message 
                 "%s" 
                 (lyskom-format (setq msg 'using-ssh-connection)
                                kom-ssh-relay-host))
              (when proc (delete-process proc))
              (setq relay-port (+ 10000 (random 20000)))
              (put procsym 'relay-host 
                   (if (string-match "@" kom-ssh-relay-host)
                       (substring kom-ssh-relay-host (1+ (match-beginning 0)))
                     kom-ssh-relay-host))
              (put procsym 'relay-port relay-port)
              (put procsym 'num-connected 0)
              (lyskom-message
               "%s"
               (lyskom-format (setq msg 'opening-ssh-connection)
                              kom-ssh-relay-host))
              (goto-char (point-max))
              (insert "\n--- new connection ---\n")
              (let ((old-lc-all (getenv "LC_ALL")))
                (unwind-protect
                    (progn
                      (setenv "LC_ALL" "C")
                      (setq proc (start-process
                                  procname
                                  bufname
                                  "ssh" "-n" "-x"
                                  "-L" (format "%d:%s:%d" relay-port server port)
                                  kom-ssh-relay-host
                                  "sh -c \"while :; do echo ok; sleep 600; done\"")))
                  (setenv "LC_ALL" old-lc-all)))
              (process-kill-without-query proc))
            (while (progn
                     (goto-char (point-max))
                     (re-search-backward "^--- .* ---$" nil t)
                     (not (re-search-forward "^ok$" nil t)))
              (when (re-search-forward "\\<\\(Enter passphrase.*$\\|^.*password.*$\\)\\|refused\\|disconnect\\|denied\\|error\\|key not found\\>" nil t)
                (cond ((match-string 1)
                       (process-send-string 
                        proc
                        (concat (silent-read (match-string 1)) "\n"))
                       (delete-region (match-beginning 0) 
                                      (match-end 0)))
                      (t (error (lyskom-get-string 'ssh-cant-connect)
                                (buffer-substring-no-properties
                                 (progn (beginning-of-line) (point))
                                 (progn (skip-chars-forward "^\n\r")
                                        (point)))))))
              (sleep-for 0.5))
            (setq proc nil)
            (lyskom-message 
             "%s"
             (lyskom-format (concat 
                             (lyskom-format msg kom-ssh-relay-host)
                             (lyskom-get-string 'done)))))
        (if proc (delete-process proc)))

      (save-excursion 
        (set-buffer kom-buffer)
        (put procsym 'num-connected (1+ (or (get procsym 'num-connected) 0)))
        (make-local-variable 'lyskom-ssh-proxy)
        (setq lyskom-ssh-proxy procsym))
      )))


(defun lyskom-setup-client-for-server-version ()
  "Setup flags according to protocol versions."
  (lyskom-clear-features)
  (let ((protocol-version 
         (version-info->protocol-version lyskom-server-version-info)))

  (when (>= protocol-version 10)
    (lyskom-set-feature bcc-misc t)
    (lyskom-set-feature aux-items t)
    (lyskom-set-feature highest-call 105)
    (lyskom-set-feature local-to-global t))

  (when (>= protocol-version 9)
    (lyskom-set-feature dynamic-session-info t)
    (lyskom-set-feature idle-time t))

  (when (>= protocol-version 8)
    (lyskom-set-feature long-conf-types t))))


(defun lyskom-connect-filter (proc output)
  "Receive connection acknowledgement from server."
  (if lyskom-debug-communications-to-buffer
      (lyskom-debug-insert proc "-----> " output))
  (cond
   ((string-match "^LysKOM\n" output)
    (set-process-filter proc 'lyskom-filter))))





;;; ================================================================
;;;                        Start anew


(defun kom-start-anew (&optional lyskom-first-time-around session-priority invisiblep)
  "Start/login as a new person. If INVISIBLEP is not nil, the login will not be
shown to other users."
  (interactive)
  (lyskom-start-of-command 'kom-start-anew)
  (lyskom-completing-clear-cache)
  (let ((new-me nil)
	(login-successful nil)
        (ignored-user-area-vars nil))
    (unwind-protect
        (progn
          (if lyskom-first-time-around
              nil
            (lyskom-tell-internat 'kom-tell-login))
          ;; We can't allow the prefetch to go on after the new user
          ;; is logged in, but to shut down the prefetch would be too
          ;; brutal, since the new login might be cancelled. To
          ;; prevent the blocking-do calls below from allowing
          ;; prefetch we set lyskom-inhibit-prefetch locally.
          (let ((lyskom-inhibit-prefetch t))
            (while (not new-me)

              (if (and lyskom-first-time-around
                       lyskom-default-user-name)
                  ;; This is nil if we can't find a unique match.
                  (setq new-me
                        (conf-z-info->conf-no
                         (lyskom-lookup-conf-by-name lyskom-default-user-name
                                                     '(pers)))))
              (if new-me
                  nil
                (let ((name nil))
                  (while (or (null name)
                             (string= name ""))
                    (setq name (lyskom-read-conf-name
                                (lyskom-get-string 'what-is-your-name) 
                                '(pers none) t "" t)))
                  (setq new-me
                        (or (conf-z-info->conf-no 
                             (lyskom-lookup-conf-by-name name '(pers)))
                            (lyskom-create-new-person name)))))
              ;; Now new-me contains a number of a person.
              ;; Lets log him in.
              (if new-me
                  (let ((conf-stat (blocking-do 'get-conf-stat new-me))
                        (lyskom-inhibit-minibuffer-messages t))

                    ;; Previously this code used lyskom-pers-no
                    ;; directly instead of new-me, but that caused
                    ;; problem with asynchrounous code trying to
                    ;; access it.
                    ;;
                    ;; Setting lyskom-pers-no fscks up other things
                    ;; if we do keyboard-quit in the middle, so don't.
                    ;;
                    ;; (setq lyskom-pers-no new-me)

                    ;; DEBUG
                    (if (null conf-stat)
                        (lyskom-insert "You don't exist. Go away.\n"))

                    (lyskom-insert (concat (conf-stat->name conf-stat) "\n"))
                    (setq lyskom-first-time-around nil)
                    (if (blocking-do 'login new-me
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
                                            (lyskom-get-string 'password))))
                                     (if invisiblep 1 0))
                        (progn
                          (if lyskom-is-new-user
                              (blocking-do 'add-member
                                           (server-info->conf-pres-conf lyskom-server-info)
                                           new-me
                                           100 
                                           1
                                           (lyskom-create-membership-type
                                            nil nil nil nil nil nil nil nil)))
                          (setq login-successful t))
                      (lyskom-insert-string 'wrong-password)
                      (when (lyskom-get-aux-item 
                             (server-info->aux-item-list lyskom-server-info)
                             13)        ; e-mail
                        (lyskom-insert 'wrong-password-help)
                        (mapcar (lambda (el)
                                  (lyskom-format-insert 'wrong-password-email
                                                        (aux-item->data el)))
                                (lyskom-get-aux-item 
                                 (server-info->aux-item-list lyskom-server-info)
                                 13)    ; e-mail
                                ))
                      (setq new-me nil))
                    (setq lyskom-is-new-user nil))))

            ;; Now we are logged in.
            (setq lyskom-pers-no new-me)
            (lyskom-insert-string 'are-logged-in)

            (unless lyskom-is-running-compiled
              (lyskom-insert-string 'warning-about-uncompiled-client))

            (unless lyskom-dont-read-user-area
              (setq ignored-user-area-vars (lyskom-read-options)))
            (when (or session-priority kom-default-session-priority)
              (setq lyskom-session-priority
                    (or session-priority kom-default-session-priority)))
            (lyskom-run-hook-with-args 'lyskom-change-conf-hook
                                       lyskom-current-conf
                                       0)
            (lyskom-run-hook-with-args 'kom-change-conf-hook
                                       lyskom-current-conf
                                       0)
            (setq lyskom-current-conf 0)
            ;; (cache-initiate-who-info-buffer (blocking-do 'who-is-on))
            (cache-set-marked-texts (blocking-do 'get-marks))
            ;; What is this variable? It is never used. It is ust to
            ;; fill the cache?
            (let ((lyskom-who-am-i (blocking-do 'who-am-i)))
              (if lyskom-who-am-i (setq lyskom-session-no lyskom-who-am-i))))

          ;; If login succeeded, clear the caches and set the language

          (when login-successful
            (progn (clear-all-caches)
                   (unless (eq lyskom-language kom-default-language)   
                     (when (lyskom-set-language kom-default-language 'local)
                       (unless lyskom-have-one-login
                         (lyskom-set-language kom-default-language 'global)
                         (lyskom-maybe-setq-default kom-default-language kom-default-language)
                         (setq-default lyskom-language kom-default-language))
                       (lyskom-format-insert 
                        'language-set-to
                        (lyskom-language-name kom-default-language))))
                   (setq lyskom-have-one-login t)))

          (when ignored-user-area-vars
            (lyskom-format-insert-before-prompt
             'ignored-user-area-var
             (mapconcat 'symbol-name 
                        ignored-user-area-vars
                        "\n    ")))

          ;; Show motd and encourage writing a presentation

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

          (setq lyskom-is-new-user nil)

          ;; Start the prefetch and update some basic caches
          (lyskom-refetch))
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

  (setq lyskom-to-do-list (lyskom-create-read-list))
  (setq lyskom-reading-list (lyskom-create-read-list))


  (lyskom-reset-prefetch)
  (let ((lyskom-inhibit-prefetch t))
    (lyskom-prefetch-membership lyskom-pers-no)
    (let ((unreads (blocking-do 'get-unread-confs lyskom-pers-no)))
      (lyskom-traverse conf-no (nreverse (conf-no-list->conf-nos unreads))
        (lyskom-prefetch-one-membership conf-no lyskom-pers-no))))
  (lyskom-start-prefetch)

  (condition-case nil
      (progn (lyskom-update-read-faqs)
             (lyskom-update-rejected-recommendations)
             (lyskom-startup-check-faqs)
             (lyskom-startup-check-recommended-memberships))
    (error nil)
    (quit nil)))


(defun lyskom-set-membership (membership)
  "Sets lyskom-membership to a new value.
Args: MEMBERSHIP."
  (setq lyskom-membership (listify-vector membership))
  (lyskom-sort-membership)
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
	(let ((new-person (blocking-do 'create-person name
                                       password
                                       (lyskom-create-flags nil
                                                            nil
                                                            nil
                                                            nil
                                                            nil
                                                            nil
                                                            nil
                                                            nil)
                                       nil)))
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
    (setq server (lyskom-completing-read prompt
                                         (lyskom-maybe-frob-completion-table
                                          known-servers)
                                         nil nil))
    (or (cdr (lyskom-string-assoc server known-servers))
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
	next conference with unread texts or whatever the prompt says.
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
	a priority higher than that of the next conference you are going
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
  (lyskom-set-menus 'lyskom-mode lyskom-mode-map)
  (run-hooks 'lyskom-mode-hook)

  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only t))

(defun lyskom-clear-vars ()
  "Set up buffer-local vars."
  (lyskom-save-variables (lyskom-proc
                          lyskom-pers-no
                          lyskom-membership
                          lyskom-membership-is-read
                          lyskom-last-viewed
                          lyskom-unparsed-buffer
                          lyskom-unparsed-marker
                          lyskom-server-info
                          lyskom-server-name)
      (kill-all-local-variables))
    (lyskom-setup-local-variables)
    (setq lyskom-do-when-done (cons kom-do-when-done kom-do-when-done))
    (setq lyskom-output-queues (make-vector 10 nil))
    (setq lyskom-collate-table lyskom-default-collate-table)
    (setq lyskom-char-classes nil)
    (let ((i 0))
      (while (< i 10)
        (aset lyskom-output-queues i (lyskom-queue-create))
        (setq i (1+ i))))
    (setq lyskom-pending-calls nil)
    (lyskom-set-mode-line
     (lyskom-get-string 'not-present-anywhere)))
