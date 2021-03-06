;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: startup.el,v 44.123 2010-05-13 18:14:12 byers Exp $
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
	      "$Id: startup.el,v 44.123 2010-05-13 18:14:12 byers Exp $\n"))


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
    (let ((buf (car (delq nil
                          (mapcar (lambda (sym)
                                    (and (boundp sym) (symbol-value sym)))
                                  '(bytecomp-outbuffer byte-compile-outbuffer outbuffer))))))
      (and (bufferp buf) (equal (buffer-name buf) " *Compiler Output*")))))

(defun lyskom-is-running-compiled ()
  (condition-case nil
      (or (eval-when-compile (lyskom-compilation-in-progress))
          (byte-code-function-p (symbol-function 'lyskom-is-running-compiled)))
    (error nil)))


;;;###autoload
(defun lyskom (&optional host username password session-priority invisiblep)
  "Start a LysKOM session.
Optional arguments: HOST, USERNAME, PASSWORD and INVISIBLEP.

A numeric prefix argument sets the session priority. A prefix argument
of `C-u', on the other hand, logs in the session without notifying other
clients of the event. See lyskom-mode for details on lyskom."
  (interactive (list (lyskom-read-server-name)
		     nil
		     nil
		     (if current-prefix-arg
			 (prefix-numeric-value current-prefix-arg)
		       nil)
		     (and current-prefix-arg (listp current-prefix-arg))))

  (run-hooks 'lyskom-init-hook)
  (setq username
	(or username 
	    kom-default-user-name
	    (getenv "KOMNAME")))
  (setq password
	(or password
	    (when (and lyskom-default-password ; Got default password?
		       (or (not lyskom-server-name) ; For this host?
			   (string= lyskom-server-name
				    host)))
	      lyskom-default-password)
	    kom-default-password
	    (getenv "KOMPASSWORD")))
  (if (zerop (length host))
      (let* ((env-kom (getenv "KOMSERVER"))
	     (canon (or (lyskom-string-rassoc env-kom kom-server-aliases)
                        (lyskom-string-rassoc env-kom kom-builtin-server-aliases))))
	(setq host (or (car canon)
		       env-kom
		       lyskom-default-server
		       kom-default-server))))
  (let ((port 4894)
	(init-done nil))
    (cond				;Allow "nanny:4892" to use port 4892.
     ((string-match ":" host)
      (setq port (lyskom-string-to-number (substring host (match-end 0))))
      (cond
       ((zerop (match-beginning 0))
	(setq host (or lyskom-default-server kom-default-server)))
       (t
	(setq host (substring host 0 (match-beginning 0)))))))

    (let* ((duplicate-buffers
            (delq nil
                  (mapcar (lambda (buffer)
                            (and (lyskom-buffer-p buffer t)
                                 (buffer-name buffer)
                                 (string-match (regexp-quote host)
                                               (buffer-name buffer))
                                 (save-current-buffer
                                   (set-buffer buffer)
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
                                          (lyskom-string-to-number
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
                       (lyskom-set-process-coding-system proc 'no-conversion 'no-conversion)

		       ;; Install our filter.
		       ;; Do this before we send the CONNECT command to
		       ;; the proxy, in case the proxy answers fast.
		       (setq lyskom-www-proxy-connect-phase 1)
		       (set-process-filter proc
					   'lyskom-www-proxy-connect-filter)
		       (lyskom-set-process-query-on-exit-flag proc nil)

                       (lyskom-process-send-string 
                        proc
                        (format "CONNECT %s:%d HTTP/1.0\r\n"
                                host port))

                       (mapc (lambda (header)
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
                         (lyskom-set-process-coding-system proc 'no-conversion 'no-conversion))))
	      (switch-to-buffer buffer)
	      (lyskom-mode)		;Clearing lyskom-default...
	      (setq lyskom-buffer buffer)
	      (setq lyskom-server-name host)
	      (setq lyskom-server-port port)
	      (setq lyskom-proc proc)
	      (lyskom-setup-faces-for-buffer)
              (lyskom-check-configuration)
	      (lyskom-insert
	       (lyskom-format 'try-connect lyskom-clientversion host))
	      (set-process-filter proc 'lyskom-connect-filter)
              (lyskom-set-process-query-on-exit-flag proc nil)
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

	      (save-current-buffer
		(lyskom-init-parse buffer))

              ;; Set up timestamps and stuff
              (lyskom-set-connection-time-format t)

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
                    (let* ((conf-stat (and text-stat (blocking-do 'get-conf-stat (text-stat->author text-stat))))
                           (str (and text text-stat (text->decoded-text-mass text text-stat))))
                      (if (null text-stat)
                          (lyskom-get-string 'lyskom-motd-was-garbed)
                        (lyskom-insert (lyskom-get-string 'server-has-motd))
                        (when (string-match "\n" str)
                          (setq str (substring str (match-end 0))))
                        (lyskom-format-insert 
                         "%#2$%#1s\n"
                         (if kom-dashed-lines
                             (make-string kom-text-header-dash-length ?-)
                           "")
                         (when kom-highlight-dashed-lines
                           `(face ,(or kom-dashed-lines-face
                                       lyskom-default-dashed-lines-face))))

                        (lyskom-format-insert "%#2$%#1t\n"
                                              str
                                              (when kom-highlight-text-body
                                                `(face ,(or kom-text-body-face
                                                            lyskom-default-text-body-face))))

                        (lyskom-format-insert
                         "%#2$%#1s\n"
                         (lyskom-format-text-footer 
                          text-stat
                          conf-stat
                          (cond ((eq (text-stat->author text-stat) 0)
                                 (lyskom-get-string 'person-is-anonymous))
                                (conf-stat (conf-stat->name conf-stat))
                                (t (lyskom-format 'person-does-not-exist 
                                                  (text-stat->author text-stat))))
                          kom-text-footer-format
                          lyskom-last-text-format-flags)
                         (when kom-highlight-dashed-lines
                           `(face ,(or kom-dashed-lines-face
                                       lyskom-default-dashed-lines-face))))
                        ))))

	      ;; Can't use lyskom-end-of-command here.
	      (setq lyskom-executing-command nil)
	      ;; Log in
	      (kom-start-anew t session-priority invisiblep username password)
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
  (blocking-do 'accept-async '(5 7 8 9 11 12 13 14 15 16 17 18 19 20 21 22))
  (let* ((ans (blocking-do 'query-async)))
    (unless (memq 15 (listify-vector ans))
      ;; async-new-text is not implemented, so use async-new-text-old
      (blocking-do 'accept-async '(0 5 7 8 9 11 12 13 14 16 17 18 19 20 21 22)))))


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
          (save-current-buffer
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
                                  kom-ssh-command "-n" "-x"
                                  "-L" (format "%d:%s:%d" relay-port server port)
                                  kom-ssh-relay-host
                                  "sh -c \"while :; do echo ok; sleep 600; done\"")))
                  (setenv "LC_ALL" old-lc-all)))
              (lyskom-set-process-query-on-exit-flag proc nil))
            (while (progn
                     (goto-char (point-max))
                     (re-search-backward "^--- .* ---$" nil t)
                     (not (re-search-forward "^ok$" nil t)))
              (when (or (re-search-forward kom-ssh-general-errors nil t)
			(not (eq 'run (process-status proc))))
                (cond ((match-string 1)
                       (process-send-string 
                        proc
                        (concat (lyskom-silent-read (match-string 1)) "\n"))
                       (delete-region (match-beginning 0) 
                                      (match-end 0)))
                      (t (error (lyskom-get-string 'ssh-cant-connect)
                                (buffer-substring-no-properties
                                 (progn (beginning-of-line) (point))
                                 (progn (skip-chars-forward "^\n\r")
                                        (point)))))))
              (sleep-for 0.5))

	    ;; Check that local forwarding works.

	    (save-excursion
	      (goto-char (point-max))
	      (re-search-backward "^--- .* ---$" nil t)
	      (when (re-search-forward kom-ssh-forwarding-errors nil t)
		(kill-process proc)
		(error (lyskom-get-string 'ssh-cant-connect)
                                (buffer-substring-no-properties
                                 (progn (beginning-of-line) (point))
                                 (progn (skip-chars-forward "^\n\r")
                                        (point))))))

            (setq proc nil)
            (lyskom-message 
             "%s"
             (lyskom-format (concat 
                             (lyskom-format msg kom-ssh-relay-host)
                             (lyskom-get-string 'done)))))
        (if proc (delete-process proc)))

      (save-current-buffer
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

  (when (>= protocol-version 8)
    (lyskom-set-feature long-conf-types t))

  (when (>= protocol-version 9)
    (lyskom-set-feature dynamic-session-info t)
    (lyskom-set-feature idle-time t))

  (when (>= protocol-version 10)
    (lyskom-set-feature bcc-misc t)
    (lyskom-set-feature aux-items t)
    (lyskom-set-feature highest-call 105)
    (lyskom-set-feature local-to-global t))

  (when (>= protocol-version 11)
    (lyskom-set-feature read-ranges t)
    (lyskom-set-feature highest-call 110))
  ))


(defun lyskom-connect-filter (proc output)
  "Receive connection acknowledgement from server."
  (if lyskom-debug-communications-to-buffer
      (lyskom-debug-insert proc "-----> " output))
  (cond
   ((string-match "^LysKOM\n" output)
    (set-process-filter proc 'lyskom-filter))))





;;; ================================================================
;;;                        Start anew


(defun kom-start-anew (&optional lyskom-first-time-around session-priority invisiblep username password)
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
              (lyskom-init-membership)
            (lyskom-tell-internat 'kom-tell-login))
          ;; We can't allow the prefetch to go on after the new user
          ;; is logged in, but to shut down the prefetch would be too
          ;; brutal, since the new login might be cancelled. To
          ;; prevent the blocking-do calls below from allowing
          ;; prefetch we set lyskom-inhibit-prefetch locally.
          (let ((lyskom-inhibit-prefetch t))
            (while (not new-me)

              (if (and lyskom-first-time-around
                       username)
                  ;; This is nil if we can't find a unique match.
                  (setq new-me
			(if (integerp username)
			    username
			(conf-z-info->conf-no
                         (lyskom-lookup-conf-by-name username
                                                     '(pers))))))
              (if new-me
                  nil
                (let ((name nil))
                  (while (or (null name) (string= name ""))
                    (setq name (lyskom-read-conf-name 'what-is-your-name
                                                      '(pers none) nil nil t)))
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
                                     (setq password
					   (or password
					     ;; Use password read when creating
					     ;; the person when loggin in new
					     ;; users
					     (or lyskom-is-new-user
						 (lyskom-silent-read
						  (lyskom-get-string 'password)))))
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
		      (setq lyskom-default-password nil)
                      (setq password nil)
                      (when (lyskom-get-aux-item 
                             (server-info->aux-item-list lyskom-server-info)
                             13)        ; e-mail
                        (lyskom-insert 'wrong-password-help)
                        (mapc (lambda (el)
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
            (setq lyskom-is-administrator nil)
            (lyskom-insert-string 'are-logged-in)

            (unless (lyskom-is-running-compiled)
              (lyskom-insert-string 'warning-about-uncompiled-client))

            (unless lyskom-dont-read-user-area
              (setq ignored-user-area-vars (lyskom-read-options)))

	    (if kom-remember-password
		(setq lyskom-default-password password))

	    ;Update mode-line string if needed (as early as possible).

	    (lyskom-mode-name-from-host) 
	    
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
            (cache-set-marked-texts (blocking-do 'get-marks))
            ;; What is this variable? It is never used. It is ust to
            ;; fill the cache?
            (let ((lyskom-who-am-i (blocking-do 'who-am-i)))
              (if lyskom-who-am-i (setq lyskom-session-no lyskom-who-am-i))))

          ;; If login succeeded, clear the caches and set the language

          (when login-successful
            (clear-all-caches)
            (lyskom-init-membership)
            (when (lyskom-set-language (lyskom-default-language) 'local)
              (unless lyskom-have-one-login
                (lyskom-set-language (lyskom-default-language) 'global)
                (setq-default lyskom-language lyskom-language)
                (setq-default kom-default-language (list lyskom-language)))
              (lyskom-format-insert-before-prompt
               'language-set-to
               (lyskom-language-name (lyskom-default-language))
               "Help on changing languages" 
                `(face underline
                       mouse-face highlight
                       lyskom-button t
                       lyskom-button-text ""
                       lyskom-button-type func
                       lyskom-buffer ,(current-buffer)
                       lyskom-button-arg 
                       (kom-help ("Changing Languages" . language-help))))

              )
            (setq lyskom-have-one-login t))

          (when ignored-user-area-vars
            (lyskom-format-insert-before-prompt
             'ignored-user-area-var
             (mapconcat (lambda (x) (symbol-name (car x)))
                        ignored-user-area-vars
                        "\n    ")
             (lyskom-string= lyskom-clientversion lyskom-settings-version)
             lyskom-settings-version
             lyskom-clientversion
             72)
            (setq lyskom-saved-unknown-variables ignored-user-area-vars))

          ;; Show motd and encourage writing a presentation

          (let ((conf-stat (blocking-do 'get-conf-stat lyskom-pers-no)))
            (if (and conf-stat
                     (/= (conf-stat->msg-of-day conf-stat) 0))
                (progn
                  (lyskom-insert-string 'you-have-motd)
                  (lyskom-view-text (conf-stat->msg-of-day conf-stat))))
            (lyskom-nag-about-presentation t))

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

  ;; The whole membership!

  (lyskom-init-membership)
  (setq lyskom-to-do-list (lyskom-create-read-list))
  (setq lyskom-reading-list (lyskom-create-read-list))


  (lyskom-reset-prefetch)
  (let ((lyskom-inhibit-prefetch t))
    (let ((unreads (blocking-do 'get-unread-confs lyskom-pers-no)))
      (lyskom-traverse conf-no (conf-no-list->conf-nos unreads)
        (lyskom-prefetch-one-membership conf-no lyskom-pers-no)))
    (lyskom-prefetch-membership lyskom-pers-no))
  (lyskom-start-prefetch)

  (condition-case nil
      (progn (lyskom-update-read-faqs)
             (lyskom-clean-read-faqs lyskom-pers-no)
             (lyskom-update-rejected-recommendations)
             (lyskom-startup-check-faqs)
             (lyskom-startup-check-recommended-memberships))
    (error nil)
    (quit nil)))

(defun lyskom-create-new-person (name)
  "A new user named NAME (or an old that mis-spelled his name)."
  (lyskom-insert
   (lyskom-format 'first-greeting name))
  (lyskom-scroll)
  (cond
   ((ja-or-nej-p (lyskom-format 'is-name-correct name))
    (let ((password (lyskom-silent-read (lyskom-get-string 'personal-password))))
      (cond
       ((not (equal password 
		    (lyskom-silent-read (lyskom-get-string 'repeat-password))))
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


(defvar lyskom-known-servers)
(defun lyskom-complete-server-name (string pred flag)
  "Programmed completion function for lyskom-read-server-name."
  (cond ((null flag)			; try-completion
	 (or (try-completion string lyskom-known-servers) string))
	((eq flag 'lambda)		; test-completion
	 (cond ((assoc string lyskom-known-servers) t)
	       ((try-completion string lyskom-known-servers) nil)
	       (t t)))
	((eq flag 't)			; all-completions
	 (or (all-completions string lyskom-known-servers)
	     (list string)))))

(defun lyskom-read-server-name ()
  "Read the name of a LysKOM server.
Copmpletion is done on the servers i kom-server-aliases and
kom-builtin-server-aliases. If an alias name is entered, the
corresponding address is returned."
  ;; Create a completion table like
  ;; (("kom.lysator.liu.se" . "kom.lysator.liu.se")
  ;;  ("LysKOM" . "kom.lysator.liu.se"))
  (let ((lyskom-known-servers
	 (append (mapcar (lambda (pair)
			   (cons (car pair) (car pair)))
			 (append kom-server-aliases kom-builtin-server-aliases))
		 (mapcar (lambda (pair)
			   (cons (cdr pair) (car pair)))
			 (append kom-server-aliases kom-builtin-server-aliases))))
	(completion-ignore-case t)
	server)
    (setq lyskom-known-servers (lyskom-maybe-frob-completion-table lyskom-known-servers))

    (while (null server)
      (setq server (lyskom-completing-read (lyskom-format 'server-q)
					   'lyskom-complete-server-name
                                           nil t
                                           (cons (or (getenv "KOMSERVER")
                                                     lyskom-default-server
                                                     kom-default-server
                                                     "") 0))))
    (or (cdr (lyskom-string-assoc server lyskom-known-servers))
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
  (lyskom-mode-name-from-host)
  (setq mode-line-buffer-identification
	; Must start with a string to be handled properly, it seems.
	(list "" 'mode-line-server-name  ": " 'mode-line-conf-name))

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
