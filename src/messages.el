;;;;;
;;;;; $Id: messages.el,v 38.1 1996-02-01 09:37:07 byers Exp $
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
;;;; File: messages.el
;;;; Author: David Byers
;;;;
;;;; This file implements the personal message handler queue
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: messages.el,v 38.1 1996-02-01 09:37:07 byers Exp $\n"))

(defvar lyskom-personal-message-handlers nil
  "A list of personal message handlers.")

(defvar lyskom-message-current-text "")

(defun lyskom-set-current-message-text (text)
  (setq lyskom-message-current-text text))

(defun lyskom-handle-personal-message (sender recipient text)
  (let ((message-type (cond ((eq recipient 0) 'common)
                            ((= (conf-stat->conf-no recipient)
                                lyskom-pers-no) 'personal)
                            (t 'group)))
        (lyskom-message-current-text text)
        (handlers lyskom-personal-message-handlers)
        (done nil))
    (while (and (not done)
                handlers)
      (setq done
            (funcall (car handlers) message-type sender recipient 
                   lyskom-message-current-text))
      (setq handlers (cdr handlers)))
    (if (not done)
        (lyskom-show-personal-message sender recipient 
                                      lyskom-message-current-text))))


(defun lyskom-add-personal-message-handler (handler 
                                            &optional place relative new)
  "Add HANDLER to the queue of personal message handlers. 
Optional argument PLACE can be one of 'before or 'after. Optional
argument RELATIVE can be another handler in the queue. IF fourth argument
NEW is t, the handler is only added if it does not already exist in the list.

The new handler is placed first in the queue if PLACE is 'before and
RELATIVE is not specified; last if PLACE is 'after and RELATIVE is not 
specified; or before or after the handler RELATIVE in the queue, depending
on the value of PLACE. If PLACE is nil, 'after is assumed."

  (if (or (not new)
          (not (memq handler lyskom-personal-message-handlers)))
      (progn
        (setq place (or (and (eq place 'before) 'before) 'after))
  (setq relative (car-safe (memq relative lyskom-personal-message-handlers)))
  (let ((pos (if relative
                 (- (length lyskom-personal-message-handlers)
                    (length (memq relative 
                                  lyskom-personal-message-handlers))))))
    (cond ((and relative
                (eq place 'after))
           (setcdr (nthcdr pos lyskom-personal-message-handlers)
                   (cons handler (nthcdr (1+ pos)
                                         lyskom-personal-message-handlers)))
           )
          ((and relative 
                (eq place 'before)
                (> pos 0))
           (setcdr (nthcdr (1- pos) lyskom-personal-message-handlers)
                   (cons handler
                         (nthcdr pos lyskom-personal-message-handlers)))
           )
          ((and lyskom-personal-message-handlers (eq place 'after))
           (setcdr (nthcdr (1- (length lyskom-personal-message-handlers))
                           lyskom-personal-message-handlers)
                   (cons handler nil)))
          ((or (null lyskom-personal-message-handlers) (eq place 'before))
           (setq lyskom-personal-message-handlers 
                 (cons handler
                       lyskom-personal-message-handlers)))
          (t (setcdr (nthcdr (1- (length lyskom-personal-message-handlers))
                             lyskom-personal-message-handlers)
                     (cons handlers nil)))))
  lyskom-personal-message-handlers)))
        

(defun lyskom-info-request-handler (message-type sender recipient text)
  (if (string= text "	")
      (progn
        (initiate-send-message 
         'follow 
         nil
         (conf-stat->conf-no sender)
         (format "emacs-version:  %s\nclient-version: %s"
                 (emacs-version)
                 lyskom-clientversion))
        t)
    nil))


    
                             
(lyskom-add-personal-message-handler 'lyskom-info-request-handler 'before)
