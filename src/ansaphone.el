;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: ansaphone.el,v 44.4 1998-06-02 12:14:10 byers Exp $
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
;;;; File: ansaphone.el
;;;; Author: David Byers
;;;;
;;;; This file implements the auto-reply facility.
;;;; It must be loaded after messages.el
;;;;

(eval-when-compile
  (require 'lyskom-vars "vars")
  (require 'lyskom-macros "macros")
  (require 'lyskom-command "command")
  (require 'lyskom-language "language")
  (require 'lyskom-messages "messages")
  (require 'lyskom-services "services"))


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: ansaphone.el,v 44.4 1998-06-02 12:14:10 byers Exp $\n"))

(defconst lyskom-ansaphone-tag "Auto-reply:\n")


;;;============================================================
;;;
;;; User functions
;;;

(def-kom-command kom-change-auto-reply (&optional message)
  "Change the default automatic reply message."
  (interactive)
  (let ((message (or message
                    (read-from-minibuffer 
                     (lyskom-get-string 'ansaphone-new-message))))
        (lyskom-last-text-format-flags nil))
    (setq kom-ansaphone-default-reply message)
    (lyskom-format-insert (lyskom-get-string 'ansaphone-message)
                          kom-ansaphone-default-reply)))


(def-kom-command kom-toggle-auto-reply ()
  "Toggle automatic replies to personal messages."
  (interactive)
  (setq kom-ansaphone-on (not kom-ansaphone-on))
  (lyskom-format-insert (lyskom-get-string 'ansaphone-state-r)
                        (lyskom-get-string (if kom-ansaphone-on
                                               'state-on
                                             'state-off)))
  (if kom-ansaphone-on
      (progn
        (setq lyskom-ansaphone-when-set (current-time-string))
        (lyskom-format-insert (lyskom-get-string 'ansaphone-message)
                              kom-ansaphone-default-reply))))


(def-kom-command kom-list-messages ()
  "List collected messages"
  (interactive)
  (if (null lyskom-ansaphone-messages)
      (lyskom-format-insert (lyskom-get-string 'ansaphone-no-messages))
    (progn
      (lyskom-format-insert (lyskom-get-string 'ansaphone-message-list-start))
      (mapcar (function
               (lambda (msg)
                 (lyskom-show-personal-message 
                  (blocking-do 'get-conf-stat (elt msg 0))
                  (blocking-do 'get-conf-stat (elt msg 1))
                  (elt msg 2)
                  (elt msg 3)
                  'nobeep)))
              (reverse lyskom-ansaphone-messages))
      (lyskom-format-insert (lyskom-get-string 'ansaphone-message-list-end)))))


(def-kom-command kom-erase-messages ()
  "Erase collected messages"
  (interactive)
  (lyskom-message "%s" (lyskom-get-string 'ansaphone-messages-gone))
  (setq lyskom-ansaphone-messages nil))



(defun lyskom-ansaphone-send-message (recipient message)
  (initiate-send-message 'async 
                         nil
                         (if (numberp recipient)
                             recipient
                           (conf-stat->conf-no recipient))
                         (concat lyskom-ansaphone-tag
                                 message)))



(defun lyskom-ansaphone-message-handler (message-type sender recipient text)
  "Personal message handler.

Automatically reply to certain personal messages and strip auto-reply
identification from messages.

See kom-ansaphone-on"

  (let ((is-automatic (eq 0 (string-match lyskom-ansaphone-tag text))))
    (if is-automatic
        (progn
          (string-match (concat "^"
                                lyskom-ansaphone-tag 
                                "\\(\\(.\\|\n\\)*\\)") text)
          (lyskom-set-current-message-text (substring text
                                                      (match-beginning 1)
                                                      (match-end 1)))))

    ;;
    ;; See if we want to reply to this message
    ;;

    (if (and kom-ansaphone-on
             sender
	     recipient
             (not is-automatic))
        (let ((reply (lyskom-ansaphone-find-reply 
                      message-type
		      (conf-stat->conf-no sender)
                      (cond ((numberp recipient) recipient)
			    (t (conf-stat->conf-no recipient)))
                      text)))
          (if (and reply (elt reply 4))
              (progn
                (setq reply
                      (concat 
                       (lyskom-format
                        (lyskom-get-string 'ansaphone-message-header)
                        lyskom-ansaphone-when-set)
                       (elt reply 4)))
                (lyskom-ansaphone-send-message sender reply)))))

    ;;
    ;; See if we want to record this message
    ;;

    (if (and kom-ansaphone-on 
             kom-ansaphone-record-messages
             sender)
        (lyskom-ansaphone-record-message sender
                                         recipient
                                         lyskom-message-current-text)))

  ;;
  ;; Perhaps we want to show the message, perhaps not
  ;;
  
  (if kom-ansaphone-on
      (not kom-ansaphone-show-messages)
    nil))



(defun lyskom-ansaphone-find-reply (message-type sender recipient text)
  "Find an automatic reply suitable for messages of type MESSAGE-TYPE from
SENDER to RECIPIENT consisting of TEXT. See the documentation for
kom-ansaphone-default-reply and kom-ansaphone-replies."
  (let ((exprs kom-ansaphone-replies)
        (result nil))
    (while exprs
      (if (and (or (null (elt (car exprs) 0))
                   (eq (elt (car exprs) 0) message-type))
               (or (null (elt (car exprs) 1))
                   (eq (elt (car exprs) 1) sender)
                   (and (listp (elt (car exprs) 1))
                        (memq sender (elt (car exprs) 1))))
               (or (null (elt (car exprs) 2))
                   (eq (elt (car exprs) 2) recipient)
                   (and (listp (elt (car exprs) 2))
                        (memq recipient (elt (car exprs) 2))))
               (or (null (elt (car exprs) 3))
                   (string-match (elt (car exprs) 3) text)))
          (progn
            (setq result (car exprs))
            (setq exprs nil)))
      (setq exprs (cdr-safe exprs)))
    (or result (and (eq message-type 'personal)
                    (list nil nil nil nil kom-ansaphone-default-reply)))))
        

(defun lyskom-ansaphone-record-message (sender recipient text)
  (if (not (numberp sender))
      (setq sender (conf-stat->conf-no sender)))
  (if (not (numberp recipient))
      (setq recipient (conf-stat->conf-no recipient)))
  (setq lyskom-ansaphone-messages (cons (list sender recipient text
                                              (current-time-string))
                                        lyskom-ansaphone-messages)))



(lyskom-add-personal-message-handler 'lyskom-ansaphone-message-handler
                                     'before
                                     nil
                                     t)


(eval-and-compile (provide 'lyskom-ansaphone))

;;; ansaphone.el ends here
