;;;;;
;;;;; $Id: command.el,v 44.8 1997-07-29 14:52:57 byers Exp $
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
;;;; File: command.el
;;;;
;;;; This file contains stuff regarding commands.
;;;;


(eval-when-compile
  (require 'lyskom-vars "vars")
  (require 'lyskom-services "services")
  (require 'lyskom-language "language")
  (require 'lyskom-clienttypes "clienttypes"))


;;; ======================================================================
;;; LysKOM user commands
;;; The new, blocking commands have a very similar structure
;;;
;;;  (defun kom-cmd (args)
;;;    "Documentation"
;;;    (interactive "...")
;;;    (lyskom-start-of-command 'kom-cmd)
;;;    (unwind-protect
;;;        (progn ...)
;;;      (lyskom-end-of-command)))
;;;
;;; This can now be written as
;;;
;;; (def-kom-command kom-cmd (args)
;;;   "Documentation"
;;;   (interactive "...")
;;;   ...)

(defmacro def-kom-command (cmd args doc interactive-decl &rest forms)
  (list 'defun cmd args doc interactive-decl
	(list 'lyskom-start-of-command (list 'quote cmd))
	(list 'unwind-protect
	      (list 'condition-case nil
		    (cons 'progn
			  forms)
		    (list 'quit
			  (list 'ding)
			  (list 'lyskom-insert-before-prompt
				(list 'lyskom-get-string
				      (list 'quote 'interrupted)))))
	      (list 'lyskom-end-of-command))))


(put 'def-kom-command 'edebug-form-spec
     '(&define name lambda-list
	       [&optional stringp]	; Match the doc string, if present.
	       ("interactive" interactive)
	       def-body))



;;;; ================================================================
;;;;                User-level commands and functions.


(defsubst lyskom-command-name (command)
  "Get the command name for the command COMMAND"
  (condition-case nil
      (lyskom-get-string command 'lyskom-command)
    (error nil)))

(defun lyskom-ok-command (alternative administrator)
  "Returns non-nil if it is ok to do such a command right now."
  (if administrator
      (not (memq (cdr alternative) lyskom-admin-removed-commands))
    (not (memq (cdr alternative) lyskom-noadmin-removed-commands))))


(defun kom-extended-command ()
  "Read a LysKOM function name and call the function."
  (interactive)
  (let ((fnc (lyskom-read-extended-command current-prefix-arg)))
    (cond
     (fnc (call-interactively fnc))
     (t (kom-next-command)))) )

(defun lyskom-read-extended-command (&optional prefix-arg)
  "Reads and returns a command"
  (let* ((completion-ignore-case t)
	 (minibuffer-setup-hook minibuffer-setup-hook)
	 (alternatives (mapcar (function (lambda (pair)
					   (cons (cdr pair) (car pair))))
			       (lyskom-get-strings lyskom-commands
						   'lyskom-command)))
	 (name nil)
         (prefix-text
          (cond ((eq prefix-arg '-) "- ")
                              ((equal prefix-arg '(4)) "C-u ")
                              ((integerp prefix-arg) 
                               (format "%d " prefix-arg))
                              ((and (consp prefix-arg) 
                                    (integerp (car prefix-arg)))
                               (format "%d " (car prefix-arg)))
                              (t nil)))
         (prompt (if prefix-text
                     (concat prefix-text (lyskom-get-string 'extended-command))
                   (lyskom-get-string 'extended-command))))

    ;; (add-hook 'minibuffer-setup-hook
    ;; 		 (function
    ;; 		  (lambda ()
    ;; 		    (let ((table (make-char-table 'case-table)))
    ;; 		      (set-char-table-parent table (current-case-table))
    ;; 		      (aset table ?\} 345)
    ;; 		      (set-case-table table)))))
    (lyskom-with-lyskom-minibuffer
     (setq name (completing-read prompt
                                 alternatives 
                                 ;; lyskom-is-administrator is buffer-local and
                                 ;; must be evalled before the call to 
                                 ;; completing-read
                                 ;; Yes, this is not beautiful
                                 (list 'lambda '(alternative)	     
                                       (list 'lyskom-ok-command 'alternative
                                             lyskom-is-administrator))
                                 t nil 'lyskom-command-history)))
    (cdr (lyskom-string-assoc name alternatives))))

(defun lyskom-start-of-command (function &optional may-interrupt)
  "This function is run at the beginning of every LysKOM command.
It moves the cursor one line down, and +++ later it will tell the server
that the previous text has been read.

Argument FUNCTION is a string the string will be written in the buffer
on start of the command. If it is a symbol it searches for the corresponding
command name in lyskom-commands and writes this in the message buffer.

If optional argument MAY-INTERRUPT is present and non-nil,
don't signal an error if this call is interrupting another command.

Special: if lyskom-is-waiting then we are allowed to break if we set 
lyskom-is-waiting nil.

This function checks if lyskom-doing-default-command and
lyskom-first-time-around are bound. The text entered in the buffer is
chosen according to this"

  (if (not lyskom-proc)
      (lyskom-error "%s" (lyskom-get-string 'dead-session)))

  (if (and lyskom-is-waiting
           (listp lyskom-is-waiting))
      (progn
        (setq lyskom-is-waiting nil)
        (lyskom-end-of-command)))

  (setq lyskom-is-waiting nil)
  (if (and lyskom-executing-command (not may-interrupt))
      (lyskom-error "%s" (lyskom-get-string 'wait-for-prompt)))
  (if (not (and (boundp 'lyskom-doing-default-command)
                lyskom-doing-default-command))
      (cond
       (lyskom-first-time-around)
       ((stringp function) (lyskom-insert function))
       ((and function (symbolp function))
        (let ((name (lyskom-command-name function)))
          (if name (lyskom-insert name)))))
    (save-excursion
      (if lyskom-current-prompt
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (beginning-of-line)
            (delete-region (point) (point-max)))))
    (lyskom-insert (lyskom-modify-prompt 
                    (cond ((stringp lyskom-current-prompt) 
                           lyskom-current-prompt)
                          ((symbolp lyskom-current-prompt)
                           (lyskom-get-string lyskom-current-prompt))
                          (t (format "%S" lyskom-current-prompt)))
                    t)))
  (setq mode-line-process (lyskom-get-string 'mode-line-working))
  (if (pos-visible-in-window-p (point-max))
      (save-excursion
        (goto-char (point-max))
        (lyskom-set-last-viewed)))
  (setq lyskom-executing-command t)
  (setq lyskom-current-command function)
  (setq lyskom-current-prompt nil)
  (lyskom-insert "\n")
  (if (and (eq (window-buffer (selected-window))
               (current-buffer))
           ;; (= (point) (point-max))
	   ) 
      (progn
	(if (pos-visible-in-window-p (1- (point-max)))
	    (goto-char (point-max)))
        (sit-for 0))) 
                                        ;  (lyskom-scroll)
  (run-hooks 'lyskom-before-command-hook)
  (if kom-page-before-command           ;Nice with dumb terminals.
      (if (or (not (listp kom-page-before-command))
              (memq function kom-page-before-command))
          (recenter 1))))


(defun lyskom-end-of-command ()
  "Print prompt, maybe scroll, prefetch info."
  (message "")
  (while (and lyskom-to-be-printed-before-prompt
	      (lyskom-queue->first lyskom-to-be-printed-before-prompt))
    (if (not (bolp)) (lyskom-insert "\n"))
    (lyskom-insert (car (lyskom-queue->first 
			 lyskom-to-be-printed-before-prompt)))
    (lyskom-queue-delete-first lyskom-to-be-printed-before-prompt))
  (setq lyskom-executing-command nil)
  (setq lyskom-current-command nil)
  (setq lyskom-current-prompt nil)	; Already set in s-o-c really
  (lyskom-scroll)
  (setq mode-line-process (lyskom-get-string 'mode-line-waiting))
  (if (pos-visible-in-window-p (point-max) (selected-window))
      (lyskom-set-last-viewed))
  (lyskom-prefetch-and-print-prompt)
  (run-hooks 'lyskom-after-command-hook)
  (if lyskom-idle-time-flag
      (save-excursion (set-buffer lyskom-buffer)
                      (initiate-user-active 'background nil)))
  (if kom-inhibit-typeahead
      (discard-input))
  ;; lyskom-pending-commands should probably be a queue or a stack.
  (when lyskom-pending-commands
    (let ((command (car lyskom-pending-commands)))
      (setq lyskom-pending-commands (cdr lyskom-pending-commands))
      (if (symbolp command)
	  (call-interactively command)
	(eval command))))
  (when lyskom-slow-mode
    (buffer-enable-undo)))

(provide 'lyskom-command)

;;; command.el ends here
