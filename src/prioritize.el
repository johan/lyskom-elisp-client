;;;;;
;;;;; $Id: prioritize.el,v 40.0 1996-03-26 08:31:54 byers Exp $
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
;;; ================================================================
;;;              Prioritera medlemsskap - Prioritize membership


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: prioritize.el,v 40.0 1996-03-26 08:31:54 byers Exp $\n"))




;;; Author: Per Cederqvist and Linus Tolke (some code from kom-membership)


(defvar lyskom-prioritize-stack nil
  "List of killed conferences.
Each entry consists of a marker.
Only used by lyskom-prioritize-mode.")

(defvar lyskom-prioritize-buffer nil
  "Used by kom-prioritize to remember which buffer the LysKOM session is in.
Only used by lyskom-prioritize-mode.")


(defun lyskom-prioritize-marker (pos)
  "Return a marker that points to the second character on the current line."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (forward-char 2)
    (point-marker)))

(defun kom-prioritize ()
  "Re-prioritize all conferences you are a member in.
Show memberships last visited, priority, unread and name in a buffer.
In that buffer you can use \\[kom-prioritize-kill], \\[kom-prioritize-yank]
\\[kom-prioritize-move-up] and \\[kom-prioritize-move-down]
to move conferences and \\[kom-prioritize-set-priority] to alter the priority."
  (interactive)
  (lyskom-start-of-command 'kom-prioritize)
  (if lyskom-membership-is-read
      nil
    (signal 'lyskom-internal-error '(membership-isnt-read kom-prioritize)))
  (lyskom-collect 'priority)
  (initiate-get-membership 'priority nil lyskom-pers-no)
  (initiate-get-pers-stat 'priority nil lyskom-pers-no)
  (lyskom-use 'priority 'lyskom-prioritize-1))


(defun lyskom-prioritize-1 (membership-list pers-stat)
  "Set up buffer for kom-prioritize. Args: MEMBERSHIP-LIST PERS-STAT."
  (cond
   ((null membership-list)
    (lyskom-insert-string 'cannot-get-membership)
    (lyskom-end-of-command))
   ((null pers-stat)
    (lyskom-insert-string 'cannot-get-pers-stat)
    (lyskom-end-of-command))
   (t
    (let ((buffer (current-buffer))
	  (tmp-buffer (get-buffer-create (concat (buffer-name (current-buffer))
						 "-prioritize")))
	  (pers-no lyskom-pers-no))
      (set-buffer tmp-buffer)

      (make-local-variable 'lyskom-prioritize-buffer)
      (make-local-variable 'lyskom-prioritize-stack)
      (make-local-variable 'lyskom-pers-no)
      (make-local-variable 'kom-buffer)
      (setq kom-buffer buffer)
      (setq lyskom-prioritize-buffer buffer)
      (setq lyskom-prioritize-stack nil)
      (setq lyskom-pers-no pers-no)

      (lyskom-prioritize-mode)

      (let ((buffer-read-only nil))
	(erase-buffer)
	(lyskom-insert-string 'your-membship))
      
      (set-buffer buffer)		;initiate-* must be done in 
					;LysKOM buffer.
      (lyskom-traverse
       memb-ship membership-list
       (initiate-get-conf-stat 'priority 'lyskom-prioritize-2
			       (membership->conf-no memb-ship)
			       tmp-buffer))
      (lyskom-run 'priority 'lyskom-prioritize-3 tmp-buffer)))))


(defun lyskom-prioritize-2 (conf-stat buffer)
  "Insert info about a conference last in prioritize-buffer.
Args: CONF-STAT BUFFER."
   (let ((membership (lyskom-get-membership-in-conf
		      (conf-stat->conf-no conf-stat))))
     (lyskom-save-excursion
      (set-buffer buffer)
      (goto-char (point-max))
      (let ((buffer-read-only nil))
	(lyskom-format-insert 'prio-row
			      (membership->priority membership)
			      conf-stat
			      conf-stat)))))


(defun lyskom-prioritize-3 (buffer)
  "Show lyskom-prioritize-buffer. Args: BUFFER."
  (lyskom-end-of-command)
  (switch-to-buffer buffer)
  (goto-char (point-max))
  (let ((buffer-read-only nil))		;So that you can move a conference
    (insert "  "))			;to the last line.
  (goto-char (point-min))
  (forward-line 2))

  
(defun lyskom-get-membership-in-conf (conf-no)
  "Returns membership for a persons membership in CONF-NO."
  (let ((l lyskom-membership))
    (while (and l (/= (membership->conf-no (car l)) conf-no))
      (setq l (cdr l)))
    (if (not l)
	(signal 'lyskom-internal-error
		'(membership-is-currupt lyskom-get-membership-in-conf)))
    (car l)))


(defun lyskom-prioritize-mode ()
  "\\<lyskom-prioritize-mode-map>Mode for prioritizing conferences in LysKOM.

Commands:
\\[kom-prioritize-kill]\tMove conference on current line to stack.
\\[kom-prioritize-yank]\tInsert conference from stack before then current line.
\\[kom-prioritize-set-priority]\tAlter the priority of the conference.
\\[kom-prioritize-move-up]\tMove conference on current line one line up.
\\[kom-prioritize-move-down]\tMove conference on current line one line down.
\\[kom-prioritize-quit]\tReturn to LysKOM.

All bindings:
\\{lyskom-prioritize-mode-map}
Entry to this mode runs lyskom-prioritize-mode-hook."
  (interactive)
  (setq major-mode 'lyskom-prioritize-mode)
  (setq mode-name "LysKOM prioritize")
  (setq buffer-read-only t)
  (use-local-map lyskom-prioritize-mode-map)
  (run-hooks 'lyskom-prioritize-mode-hook))


(defun kom-prioritize-kill (count)
  "\"Kill\" conference on current line.
If optional prefix argument COUNT is present that many conferences
are killed. A killed conference is marked with a '-' in the first
column, and can be inserted at another line with \\[kom-prioritize-yank]."
  (interactive "P")
  (beginning-of-line)
  (cond
   ((< (count-lines (point-min) (point)) 2) 		;In header?
    (lyskom-error "%s" (lyskom-get-string 'too-high-goto-2)))
   ((< (count-lines (point) (point-max)) 2)
    (lyskom-error "%s" (lyskom-get-string 'too-low-go-up)))
   ((null count)			;Take care of interactive argument.
    (setq count 1))
   ((numberp count))
   ((and (listp count)
	 (numberp (car count))
	 (null (cdr count)))
    (setq count (car count)))
   (t (signal 'lyskom-internal-error '(kom-prioritize-yank))))
  (while (and (> count 0)		;Repeat count times.
	      (re-search-forward
	       "^ "			;Find first line without kill flag.
	       (- (point-max) 6)
	       'foo))		;Stop at end of buffer if none found.
    (beginning-of-line)
    (setq lyskom-prioritize-stack	;Push a marker to this line.
	  (cons (lyskom-prioritize-marker (point))
		lyskom-prioritize-stack))
    (let ((buffer-read-only nil))	;Set kill-flag.
      (delete-char 1)
      (insert "-"))
    (beginning-of-line)
    (-- count)))			;... and repeat.
  
    

(defun kom-prioritize-yank (&optional count)
  "Yank topmost conference from stack before current line.
If optional prefix argument COUNT is present yank that many
conferences (or as many as is on the stack, whichever is less)."
  (interactive "P")
  (cond
   ((null lyskom-prioritize-stack)
    (lyskom-error "%s" (lyskom-get-string 'all-confs-popped)))
   ((< (count-lines (point-min) (point)) 2)
    (lyskom-error "%s" (lyskom-get-string 'too-high-goto-2)))
   ((null count)			;Interactive...
    (setq count 1))
   ((numberp count))
   ((and (listp count)
	 (numberp (car count))
	 (null (cdr count)))
    (setq count (car count)))
   (t (signal 'lyskom-internal-error '(kom-prioritize-yank))))
  (while (and (> count 0)		;Repeat count times...
	      lyskom-prioritize-stack)	;...or until stack is empty.
    (beginning-of-line)
    (lyskom-prioritize-move
     (car lyskom-prioritize-stack)	;Move from top-of-stack
     (lyskom-prioritize-marker (point))) ;to current line.
    (setq lyskom-prioritize-stack	;Pop from stack.
	  (cdr lyskom-prioritize-stack))
    (-- count))) 


(defun lyskom-prioritize-move (from to)
  "Move a line from position FROM to position TO.
FROM and TO should be markers that points to the second characters
on the lines. Leaves point at the beginning of the moved line."
  (cond
   ((not (and (markerp from) (markerp to)))
    (signal 'lyskom-internal-error
	    '("lyskom-prioritize-move called without marker.")))
   (t
    ;;Store line to move in 'line'.

    (let* ((start (progn (goto-char from)
			 (beginning-of-line)
			 (point)))
	   (end (progn (end-of-line)
		       (1+ (point))))
	   (line (prog1 (buffer-substring start end)
		   (let ((buffer-read-only nil))
		     (delete-region start end)))))
      ;;Insert 'line'.
      (goto-char to)
      (beginning-of-line)
      (let ((buffer-read-only nil))
	(insert line)
	(beginning-of-line 0)		;Move back to the newly inserted line.
	(delete-char 1)			;Clear kill-flag.
	(insert " ")))
    (beginning-of-line)

    ;; Tell the server what we have done.
    ;; (Should also tell the cache to avoid refetching everything).

    (save-excursion
      (let ((posnr (- (count-lines (point-min) (point))
		      2))
	    (prio (read (current-buffer)))
	    (conf-no (read (current-buffer))))
	(set-buffer lyskom-prioritize-buffer)
	(initiate-add-member 'priority 'lyskom-prioritize-handler
			     conf-no lyskom-pers-no prio posnr))))))


(defun lyskom-prioritize-handler (res)
  "Arg: RES. Barf if RES is nil."
  (or res (lyskom-error "%s" (lyskom-get-string 'prio-died))))


(defun kom-prioritize-quit ()
  "Reselect LysKOM buffer."
  (interactive)
  (let ((buffer lyskom-prioritize-buffer))
    (bury-buffer)			;Should maybe kill buffer?
    (switch-to-buffer buffer)
    (lyskom-start-of-command "" t)
    (lyskom-insert-string 'wait)
    (lyskom-refetch)
    (lyskom-end-of-command)))			;The internal structures SHOULD be
					;updated when one move around the
					;conferences in the prioritize-buffer,
					;but it is simpler to do it this way...


(defun kom-prioritize-set-priority ()
  "Change priority of conference on current line."
  (interactive)
  (beginning-of-line)
  (cond
   ((< (count-lines (point-min) (point)) 2)
    (lyskom-error "%s" (lyskom-get-string 'too-high-goto-2)))
   ((zerop (count-lines (point) (point-max)))
    (lyskom-error "%s" (lyskom-get-string 'too-low-go-up)))
   (t
    (save-excursion
      (let
	  ((new-prio (lyskom-read-num-range
		      0 255 (lyskom-get-string 'new-priority)))
	   (posnr (- (count-lines (point-min) (point))
		     2))
	   (prio (read (current-buffer)))
	   (conf-no (read (current-buffer))))
	(beginning-of-line)
	(let ((buffer-read-only nil))
	  (delete-char 6)		;Write new priority on display.
	  (insert (lyskom-format 'new-prio new-prio)))
	(beginning-of-line)
	(set-buffer lyskom-prioritize-buffer)
	;; Tell the server the new priority.
	(initiate-add-member 'priority 'lyskom-prioritize-handler
			     conf-no lyskom-pers-no new-prio posnr)
	;; Tell the cache the new priority.
	;; (Should sort the membership-list also).
	(set-membership->priority (lyskom-get-membership-in-conf conf-no)
				  new-prio))))))


(defun kom-prioritize-move-up (&optional count reverse)
  "Move current line up one line.
If optional prefix-arg COUNT move COUNT lines instead.
If optional argument REVERSE is non-nil move line down instead."
  (interactive "P")
  (beginning-of-line)
  (cond
   ((< (count-lines (point-min) (point)) 2)
    (lyskom-error "%s" (lyskom-get-string 'too-high-goto-2)))
   ((zerop (count-lines (point) (point-max)))
    (lyskom-error "%s" (lyskom-get-string 'too-low-go-up)))
   ((null count)
    (setq count 1))
   ((numberp count))
   ((and (listp count)
	 (numberp (car count))
	 (null (cdr count)))
    (setq count (car count))))
  (if reverse (setq count (- count)))
  (if (< count 0) (-- count))
  (let ((from (lyskom-prioritize-marker (point))))
    (forward-line (- count))
    (cond
     ((< (count-lines (point-min) (point)) 2)
      (goto-char (point-min))
      (forward-line 2)))
    (lyskom-prioritize-move from (lyskom-prioritize-marker (point)))))


(defun kom-prioritize-move-down (&optional count)
  "Move current line down one line.
If optional prefix-arg COUNT move COUNT lines instead."
  (interactive "P")
  (kom-prioritize-move-up count t))

