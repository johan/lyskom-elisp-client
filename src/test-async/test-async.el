;;;;;
;;;;; $Id: test-async.el,v 39.0 1996-03-14 18:18:58 davidk Exp $
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
(defun lyskom-parse-async (tokens buffer)
  "Parse an asynchronous message from the server.
The message consists of TOKENS tokens. Unknown messages are skipped.
Actions are taken to perform the various tasks that is required on reciept of
an asynchronous message.
If variable kom-presence-messages is non-nil or some minibuffer editing is 
going on then nothing is printed on the message area.
This function is called with the lyskom-unparsed-buffer as current-buffer.
All calls using the lyskom-variables have to be made using the buffer BUFFER.

Be careful when editing this. All parsing is done with the buffer this
function is called with as the current-buffer, while all calls from
this function shall be with current-buffer the BUFFER."

  (let ((msg-no (lyskom-parse-num)))
    (cond
     ((eq msg-no 0)			; New text
      (let* ((text-no (lyskom-parse-num))
	     (text-stat (lyskom-parse-text-stat text-no)))
	(lyskom-save-excursion
	  (set-buffer buffer)
	  (lyskom-call-hook 'lyskom-new-text-hook text-stat)
	  (lyskom-call-hook 'kom-new-text-hook text-stat)
	  ;;(lyskom-async-new-text text-stat);;OLD-OLD-OLD
	  ))) ;

     ((eq msg-no 1)			; Logout
      (let ((pno (lyskom-parse-num)))
	(lyskom-save-excursion
	  (set-buffer buffer)
	  (lyskom-call-hook 'lyskom-logout-hook pno)
	  (lyskom-call-hook 'kom-logout-hook pno)
	  ;;(if (and (not (zerop lyskom-pers-no));;OLD-OLD-OLD
	  ;;	   (or kom-presence-messages
	  ;;	       kom-presence-messages-in-buffer))
	  ;;    (initiate-get-conf-stat 'follow 'lyskom-show-logged-out-person
	  ;;			      pno));;OLD-OLD-OLD
	  )))

     ((eq msg-no 2)			; Login, obsolete.
      (lyskom-skip-tokens tokens))

     ((eq msg-no 3)			; Conference deleted
      (lyskom-skip-tokens tokens))

     ((eq msg-no 4)			; Conference created
      (lyskom-skip-tokens tokens))

     ((eq msg-no 5)             ; A person or conference has changed name.
      (let ((conf-no (lyskom-parse-num))
	    (old-name (lyskom-parse-string))
	    (new-name (lyskom-parse-string)))
	(lyskom-save-excursion
	  (set-buffer buffer)
	  (lyskom-call-hook 'lyskom-name-change-hook conf-no old-name new-name)
	  (lyskom-call-hook 'kom-name-change-hook conf-no old-name new-name)
	  ;;(cache-del-conf-stat conf-no) ;+++Borde {ndra i cachen i st{llet.
	  ;;(cond
	  ;; ((lyskom-is-in-minibuffer))
	  ;; (kom-presence-messages
	  ;;  (message (concat old-name " har nu bytt namn till " new-name))))
	  ;;(cond
	  ;; (kom-presence-messages-in-buffer
	  ;;  (lyskom-insert-before-prompt
	  ;;   (concat old-name " har nu bytt namn till " new-name "\n"))))
	  )))

     ((eq msg-no 6)			;i_am_on - something is moving
      (let ((info (lyskom-parse-who-info)))
	(lyskom-save-excursion
	  (set-buffer buffer)
	  (lyskom-call-hook 'lyskom-i-am-on-hook info)
	  (lyskom-call-hook 'kom-i-am-on-hook info)
	  ;;(if (zerop lyskom-pers-no)
	  ;;    nil
	  ;;  (if (and (/= (who-info->pers-no info) 0)
	  ;;	     (/= (who-info->pers-no info) lyskom-pers-no))
	  ;;				;Don't show myself.
	  ;;	(initiate-get-conf-stat 'follow
	  ;;				'lyskom-show-changed-person
	  ;;				(who-info->pers-no info)
	  ;;				(who-info->working-conf info)
	  ;;				(who-info->doing-what info)))
	  ;;  (if (/= (who-info->working-conf info) 0)
	  ;;	(initiate-get-conf-stat 'void nil
	  ;;				(who-info->working-conf info))))
	  )))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
     ((eq msg-no 7)			; Database is syncing.
      (lyskom-save-excursion
	(set-buffer buffer)
	(if (and (not (lyskom-is-in-minibuffer))
		 kom-presence-messages)
	    (message "Databasen synkas. V{nta ett bra tag!"))
	(setq mode-line-process ": saving")
	(set-buffer-modified-p (buffer-modified-p))
	(sit-for 0)
	(if (not lyskom-pending-calls)
	    (initiate-get-time 'async nil))))

     ((eq msg-no 8)			; Forced leave conference
      (lyskom-skip-tokens tokens))

     ((eq msg-no 9)			; A person has logged in
      (let ((pers-no (lyskom-parse-num))
	    (session (lyskom-parse-num)))
	(lyskom-save-excursion
	  (set-buffer buffer)
	  (if (and (/= 0 lyskom-pers-no)
		   (/= pers-no lyskom-pers-no))
					; Don't show myself.
	      (initiate-get-conf-stat 'follow
				      (if kom-presence-messages
					  'lyskom-show-logged-in-person
					nil)
				      pers-no)))))

     ((eq msg-no 10)			; Broadcast message
      (let ((sender (lyskom-parse-num))
	    (message (lyskom-parse-string)))
	(lyskom-save-excursion
	 (set-buffer buffer)
	 (initiate-get-conf-stat 'follow
				 'lyskom-show-broadcast-message sender
				 message))))

     ((eq msg-no 11)
      (lyskom-save-excursion
       (set-buffer buffer)
       (lyskom-insert-before-prompt "\
===========================================================
Meddelande fr}n LysKOM-systemet: N}gon f|rs|kte koppla upp,
men misslyckades eftersom alla tillg{ngliga f|rbindelser {r
upptagna. Logga ut och kom tillbaks senare om du v{ntar nu.
===========================================================\n")))

     ((eq msg-no 12)			; Message to the user (or everybody)
      (let ((recipient (lyskom-parse-num))
	    (sender (lyskom-parse-num))
	    (message (lyskom-parse-string)))
	(lyskom-save-excursion
	 (set-buffer buffer)
	 (cond
	  ((string= message "info")
	   (initiate-send-message 'follow nil sender 
				  (format "emacs-version %s\n%s"
					  emacs-version
					  (save-excursion
					    (goto-char (point-min))
					    (forward-line 3)
					    (buffer-substring (point-min)
							      (point))))))
	  (t
	   (initiate-get-conf-stat 'follow
				   'lyskom-show-personal-message sender
				   recipient
				   message))))))

     (t
      (lyskom-skip-tokens tokens)))))