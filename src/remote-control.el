;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: remote-control.el,v 44.11 2003-08-24 21:12:44 byers Exp $
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
;;;; File: remote-control.el
;;;; Author: David Byers
;;;;
;;;; This file implements the remote control mechanism.
;;;; It must be loaded after messages.el
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: remote-control.el,v 44.11 2003-08-24 21:12:44 byers Exp $\n"))

(eval-when-compile
  (require 'lyskom-command "command"))


;;;============================================================
;;;
;;;     Internal variables.
;;;

(defconst lyskom-remote-commands
  '(("set message" . lyskom-remote-set-message)
    ("list messages" . lyskom-remote-list-messages)
    ("erase messages" . lyskom-remote-erase-messages)
    ("autoreply" . lyskom-remote-autoreply)
    ("quit" . lyskom-remote-quit)))


;;;============================================================
;;;
;;;     Interactive functions
;;;

(def-kom-command kom-remote-autoreply (&optional session-no state)
  "Remotely turn on or off the auto reply facility of another client.
This is similar to `kom-toggle-auto-reply', but operates on another
session.

You will be prompted for which session to control. For remote control
to work the target session must have remote control turned on, and the
user you are logged on as must have permission to manipulate that
session.

See `kom-remote-controllers' and `kom-self-control' for settings that
affect remote control."
  (interactive)
  (setq session-no (or session-no
                       (car (lyskom-read-session-no
                             'remote-control-who
                             nil nil t))))
  (setq state (or state
                  (cdr-safe (assoc
                             (lyskom-completing-read
                              (lyskom-get-string 'remote-control-autoreply)
                              (lyskom-maybe-frob-completion-table
			       lyskom-onoff-table t)
			      nil t nil nil)
                             (lyskom-maybe-frob-completion-table
			      lyskom-onoff-table t)))))
  (let ((info (blocking-do 'get-session-info session-no)))
    (lyskom-send-message (session-info->pers-no info)
                         (format "Remote-command: %d %d\nautoreply\n%s"
                                 lyskom-session-no
                                 session-no
                                 (cond ((eq state 'on) "on")
                                       ((eq state 'off) "off")
                                       (t "")))
			 t)))

(def-kom-command kom-remote-set-message (&optional session-no message)
  "Remotely set the default auto reply message of another client. This
is similar to `kom-change-auto-reply', but operates on another
session.

You will be prompted for which session to control. For remote control
to work the target session must have remote control turned on, and the
user you are logged on as must have permission to manipulate that
session.

See `kom-remote-controllers' and `kom-self-control' for settings that
affect remote control."
  (interactive)
  (setq session-no (or session-no
                       (car (lyskom-read-session-no
                             'remote-control-who
                             nil nil t))))
  (setq message (or message
                    (lyskom-read-string
                     (lyskom-get-string 'message-prompt))))
  (let ((info (blocking-do 'get-session-info session-no)))
    (lyskom-send-message (session-info->pers-no info)
                         (format "Remote-command: %d %d\nset message\n%s"
                                 lyskom-session-no
                                 session-no
                                 message)
			 t)))

(def-kom-command kom-remote-list-messages (&optional session-no)
  "List messages collected from a remote auto reply facility. This is
similar to `kom-list-messages', but operates on another session.

You will be prompted for which session to control. For remote control
to work the target session must have remote control turned on, and the
user you are logged on as must have permission to manipulate that
session.

See `kom-remote-controllers' and `kom-self-control' for settings that
affect remote control."
  (interactive)
  (setq session-no (or session-no
                       (car (lyskom-read-session-no
                             'remote-control-who
                             nil nil t))))
  (let ((info (blocking-do 'get-session-info session-no)))
    (lyskom-send-message (session-info->pers-no info)
                         (format "Remote-command: %d %d\nlist messages\n"
                                 lyskom-session-no
                                 session-no)
			 t)))



(def-kom-command kom-remote-erase-messages (&optional session-no)
  "Erase stored messages on a remote auto reply facility. This is
similar to `kom-erase-messages', but operates on another session.

You will be prompted for which session to control. For remote control
to work the target session must have remote control turned on, and the
user you are logged on as must have permission to manipulate that
session.

See `kom-remote-controllers' and `kom-self-control' for settings that
affect remote control."
  (interactive)
  (setq session-no (or session-no
                       (car (lyskom-read-session-no
                             'remote-control-who
                             nil nil t))))
  (let ((info (blocking-do 'get-session-info session-no)))
    (lyskom-send-message (session-info->pers-no info)
                         (format "Remote-command: %d %d\nerase messages\n"
                                 lyskom-session-no
                                 session-no)
			 t)))


(def-kom-command kom-remote-quit (&optional session-no)
  "Quit a remote client. The difference between this command and
`kom-force-logout' is that the latter uses server facilities to
forcefully disconnect the session, while the former allows the client
to cleanly disconnect from the server.

You will be prompted for which session to control. For remote control
to work the target session must have remote control turned on, and the
user you are logged on as must have permission to manipulate that
session.

See `kom-remote-controllers' and `kom-self-control' for settings that
affect remote control."
  (interactive)
  (setq session-no (or session-no
                       (car (lyskom-read-session-no
                             'remote-control-who
                             nil nil t))))
  (let ((info (blocking-do 'get-session-info session-no)))
    (lyskom-send-message (session-info->pers-no info)
                         (format "Remote-command: %d %d\nquit\n"
                                 lyskom-session-no
                                 session-no)
			 t)))

;;;============================================================
;;;
;;;     Main handler function
;;;


(defun lyskom-remote-handler (message-type sender recipient text)
  "Personal message handler.

Handler to implement remote control of the ansaphone."
  (let* ((error nil)
         (is-remote (eq 0 
                        (string-match "^Remote-command: \\([0-9]+\\) \\([0-9]+\\)\n" text)))
         (is-from-me (and is-remote
                          (= (string-to-number (substring text
                                                          (match-beginning 1)
                                                          (match-end 1)))
                             lyskom-session-no)))
         (is-to-me (and is-remote
                        (= (string-to-number (substring text
                                                        (match-beginning 2)
                                                        (match-end 2)))
                           lyskom-session-no)))
         (is-valid (eq 0 
                       (string-match
                        "^Remote-command: \\([0-9]+\\) \\([0-9]+\\)\n\\(.*\\)\n\\(\\(\n\\|.\\)*\\)$"
                        text)))
         (is-trusted (or (memq (conf-stat->conf-no sender)
                               kom-remote-controllers)
                         (and kom-self-control
                              (eq (conf-stat->conf-no sender)
                                  lyskom-pers-no)))))
    (cond ((not is-remote) nil)
          (is-from-me t)
          ((not is-to-me) t)
          ((not is-trusted)
           (lyskom-ansaphone-send-message
            lyskom-pers-no
            (lyskom-format (lyskom-get-string 'illegal-remote)
                           (lyskom-format-time 'timeformat-yyyy-mm-dd-hh-mm)
                           sender
                           recipient
                           text))
           (lyskom-ansaphone-send-message
            sender
            (lyskom-format (lyskom-get-string 'illegal-remote-reply)
                           (lyskom-get-string 'remote-not-in-list)))
           t)
          ((not is-valid) (setq error 'remote-bad-command))
          (t (let* ((command (substring text (match-beginning 3)
                                        (match-end 3)))
                   (arg (substring text (match-beginning 4) (match-end 4)))
                   (desc (assoc command lyskom-remote-commands)))
               (if (null desc)
                   (setq error 'remote-bad-command)
                 (setq error (funcall (cdr desc)
                                      arg
                                      sender
                                      recipient
                                      text))))))
    (if error
        (progn
          (lyskom-ansaphone-send-message 
           sender
           (lyskom-format (lyskom-get-string 'illegal-remote-reply)
                          (or (lyskom-get-string error) 
                              (lyskom-get-string 'remote-unknown-error))))))
    is-remote))




(defun lyskom-remote-set-message (arg sender recipient text)
  (if arg
      (let ((lyskom-last-text-format-flags nil))
        (setq kom-ansaphone-default-reply arg)
        (setq lyskom-ansaphone-when-set (lyskom-format-time
                                         'timeformat-yyyy-mm-dd-hh-mm))
        (lyskom-ansaphone-send-message sender
                                       (concat 
                                        (lyskom-get-string
                                         'ansaphone-new-message)
                                        arg))
        (lyskom-insert-before-prompt
         (lyskom-format (lyskom-get-string-sol 'remote-set-message)
                        sender
                        (lyskom-format-time 'timeformat-yyyy-mm-dd-hh-mm)
                        arg))
        nil)
    'remote-bad-command))
  


;;;============================================================
;;;
;;;     Command handlers
;;;


(defun lyskom-remote-autoreply (arg sender recipient text)
  (cond ((string= (downcase arg) "on") (setq kom-ansaphone-on t))
        ((string= (downcase arg) "off") (setq kom-ansaphone-on nil))
        (t (setq kom-ansaphone-on (not kom-ansaphone-on))))
  (lyskom-ansaphone-send-message 
   sender (lyskom-format (lyskom-get-string 'ansaphone-state)
                         (lyskom-get-string
                          (if kom-ansaphone-on 
                              'state-on 'state-off))))
  (lyskom-insert-before-prompt
   (lyskom-format (lyskom-get-string 'remote-set-ansaphone)
                  sender
                  (lyskom-format-time 'timeformat-yyyy-mm-dd-hh-mm)
                  (lyskom-get-string (if kom-ansaphone-on 
                                         'state-on 'state-off))))
  nil)



(defun lyskom-remote-list-messages (arg sender recipient text)
  (if (null lyskom-ansaphone-messages)
      (lyskom-ansaphone-send-message 
       sender
       (lyskom-get-string 'ansaphone-no-messages))
    (progn
      (lyskom-collect 'follow)
      (let ((tmp (reverse lyskom-ansaphone-messages)))
        (while tmp
          (initiate-get-conf-stat 'follow nil (elt (car tmp) 0))
          (initiate-get-conf-stat 'follow nil (elt (car tmp) 1))
          (setq tmp (cdr tmp))))
      (lyskom-use 'follow 'lyskom-remote-list-messages-1 sender)))
  nil)

(defun lyskom-remote-list-messages-1 (sender &rest pairs)
  (let ((message "")
        (tmp (reverse lyskom-ansaphone-messages))
        (from nil)
        (to nil))
  (while pairs
    (setq from (car pairs) to (car (cdr pairs)))
    (setq pairs (cdr (cdr pairs)))
    (setq message (concat message
                          (lyskom-format-as-personal-message
                           from
                           to
                           (elt (car tmp) 2)
                           (elt (car tmp) 3))
                          (if pairs "\n\n" "")))
    (setq tmp (cdr tmp)))
  (lyskom-ansaphone-send-message sender message)
  (lyskom-insert-before-prompt
   (lyskom-format (lyskom-get-string 'remote-list-messages)
		  sender
                  (lyskom-format-time 'timeformat-yyyy-mm-dd-hh-mm)))))





(defun lyskom-remote-erase-messages (arg sender recipient text)
  (setq lyskom-ansaphone-messages nil)
  (lyskom-format-insert-before-prompt
    (lyskom-get-string 'remote-erase-messages)
    sender
    (lyskom-format-time 'timeformat-yyyy-mm-dd-hh-mm))
  nil)


(defun lyskom-remote-quit (arg sender recipient text)
  (lyskom-insert-before-prompt
   (lyskom-format (lyskom-get-string 'remote-quit)
                  sender
                  (lyskom-format-time 'timeformat-yyyy-mm-dd-hh-mm)))
   (lyskom-quit))


;;;============================================================
;;;
;;;     Clean-up and installation
;;;

(lyskom-add-personal-message-handler 'lyskom-remote-handler
                                     'before
                                     'lyskom-ansaphone-message-handler
                                     t)
