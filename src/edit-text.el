;;;;;
;;;;; $Id: edit-text.el,v 38.0 1994-01-06 01:57:33 linus Exp $
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
;;;; File: edit-text.el
;;;;
;;;; This file contains functions which lets the LysKOM user edit
;;;; a text in a window. It also defines a new mode - lyskom-edit-mode.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: edit-text.el,v 38.0 1994-01-06 01:57:33 linus Exp $\n"))


;;;; ================================================================


(defvar lyskom-edit-mode-mode-map nil
  "Mode map for the mode LysKOM edites in.")

(defvar lyskom-edit-mode-name "LysKOM edit"
  "Name of the mode.")

(defun lyskom-edit-text (proc misc-list subject body
			      &optional handler &rest data)
  "Edit a text in a new buffer.
PROC is the associated process.
MISC-LIST is the default misc-list.
SUBJECT is subject (a string).
BODY is the default text-body (a string, normally empty.)
HANDLER is a function to call when the text has been created.
DATA is extra data to send to the function. HANDLER is called with
	(apply HANDLER text-no DATA)
where text-no is the number of the text.
Does lyskom-end-of-command."
  (setq lyskom-is-writing t)
  (lyskom-end-of-command)
  (let ((buffer (generate-new-buffer
		 (concat (buffer-name (process-buffer proc)) "-edit")))
	(config (current-window-configuration)))
    (setq lyskom-list-of-edit-buffers (cons buffer 
					    lyskom-list-of-edit-buffers))
    (condition-case emacs-18.55
	(process-kill-without-query (get-buffer-process (current-buffer)) t)
      (error
       ;; You loose some...
       (message "Old emacs! Upgrade!")
       (process-kill-without-query (get-buffer-process (current-buffer)))))
    (cond
     ((and (or (bufferp kom-write-texts-in-window)
	       (stringp kom-write-texts-in-window))
	   (get-buffer-window kom-write-texts-in-window))
      (select-window (get-buffer-window kom-write-texts-in-window))
      (switch-to-buffer buffer))
     ((eq kom-write-texts-in-window 'other)
      (switch-to-buffer-other-window buffer))
     ((and (eq kom-write-texts-in-window 'other-frame)
	   (not (eq (selected-frame) (next-frame))))
      (select-frame (next-frame)))
     ((eq kom-write-texts-in-window 'new-frame)
      (make-local-variable 'lyskom-is-dedicated-edit-window)
      (setq lyskom-is-dedicated-edit-window t)
      (switch-to-buffer-other-frame buffer))
     (t
      (switch-to-buffer buffer)))
    (if (boundp 'lyskom-filter-old-buffer)
	(setq lyskom-filter-old-buffer (current-buffer)))
    (lyskom-edit-mode)
    (setq lyskom-proc proc)
    (make-local-variable 'lyskom-edit-handler)
    (make-local-variable 'lyskom-edit-handler-data)
    (make-local-variable 'lyskom-edit-return-to-configuration)
    (setq lyskom-edit-handler handler)
    (setq lyskom-edit-handler-data data)
    (setq lyskom-edit-return-to-configuration config)
    (lyskom-edit-insert-miscs misc-list subject body)
    (lyskom-message "%s" (lyskom-get-string 'press-C-c-C-c)))
  (set-buffer (process-buffer lyskom-proc))
  )


(defun lyskom-edit-insert-miscs (misc-list subject body)
  "Insert MISC-LIST into header of text.
recpt		-> Mottagare: <%d> %s.
cc-recpt	-> Extra kopia: <%d> %s.
comm-to		-> Kommentar till text %d.
footn-to	-> Fotnot till text %d."
  (let ((edit-buffer (current-buffer))
	(where-put-misc (point-min-marker))
	(main-buffer (process-buffer lyskom-proc)))
    (set-buffer main-buffer)
    (setq misc-list (cdr misc-list))
    (while (not (null misc-list))
      (let ((key (car (car misc-list)))
	    (data (cdr (car misc-list))))
	(cond
	 ((eq key 'recpt)
	  (lyskom-edit-insert-misc-conf (blocking-do 'get-conf-stat data)
					(lyskom-get-string 'recipient)
					where-put-misc data))
	 ((eq key 'cc-recpt)
	  (lyskom-edit-insert-misc-conf (blocking-do 'get-conf-stat data)
					(lyskom-get-string 'carbon-copy)
					where-put-misc data))
	 ((eq key 'comm-to)
	  (lyskom-edit-get-commented-author (blocking-do 'get-text-stat data)
					    (lyskom-get-string 'comment)
					    where-put-misc data))
	 ((eq key 'footn-to)
	  (lyskom-edit-get-commented-author (blocking-do 'get-text-stat data)
					    (lyskom-get-string 'footnote)
					    where-put-misc data)))
	(setq misc-list (cdr misc-list))))
    (princ (lyskom-format 'text-mass subject 
			  (if kom-emacs-knows-iso-8859-1
			      lyskom-header-separator
			    lyskom-swascii-header-separator)
			  body 
			  (if kom-emacs-knows-iso-8859-1
			      lyskom-header-subject
			    lyskom-swascii-header-subject))
	   where-put-misc)
    (set-buffer edit-buffer)
    (goto-char where-put-misc)
    ))


(defun lyskom-edit-goto-char (marker)
  "Positions the editing at MARKER."
  (let ((curbuf (current-buffer)))
    (set-buffer (marker-buffer marker))
    (save-window-excursion
      (goto-char marker))
    (set-buffer curbuf)))


(defun lyskom-edit-insert-misc-conf (conf-stat string stream number)
  "Insert Mottagare: or Extra kopia: in edit-buffer.
Args: CONF-STAT STRING STREAM NUMBER
CONF-STAT is the conf-stat of the conference that is about to be put in,
STRING is the string that is inserted.
STREAM is the buffer of a marker telling the position.
NUMBER is the number of the person. Used if the conf-stat is nil."
  (save-excursion
    (let ((buf (cond
		((bufferp stream) stream)
		((markerp stream) (marker-buffer stream)))))
      (if buf
	  (set-buffer buf)))
    (save-excursion
      (princ (concat string
		     (format " <%d> " (if conf-stat
					  (conf-stat->conf-no conf-stat)
					number))
		     (if conf-stat
			 (conf-stat->name conf-stat)
		       "")
		     "\n")
	     stream))))


(defun lyskom-edit-get-commented-author (text-stat string stream number)
  (if text-stat
      (lyskom-edit-insert-commented-author 
       (blocking-do 'get-conf-stat (text-stat->author text-stat))
       string stream number)
    (lyskom-edit-insert-commented-author nil string stream number)))


(defun lyskom-edit-insert-commented-author (conf-stat string stream number)
  (princ (lyskom-format 'comment-to-by
			string number 
			(if conf-stat
			    (lyskom-format 'by (conf-stat->name conf-stat))
			  ""))
	 stream)
  )
			  


(defun lyskom-create-misc-list (&rest misc-pairs)
  "Create a misc-list.
Note that a misc-list is very different from a misc-info-list.
A misc-list is used when creating a new text, and sent to the server.
A misc-info-list is what is received from the server. Although the server
has the same format for the two things, this client uses two quite
different formats.

The arguments to this function is any number of pairs of data. The first
item in each pair should be one of recpt, cc-recpt, comm-to or footn-to.
The second item should be the corresponding conf- or text-no.

The result is a list of dotted pairs:
	('recpt . conf-no)
	('cc-recpt . conf-no)
	('comm-to . text-no)
	('footn-to . text-no).
First element is a type-tag."
  (let ((result (cons 'MISC-LIST nil)))
    (while (not (null misc-pairs))
      (nconc result (cons (cons (car misc-pairs)
				(car (cdr misc-pairs)))
			  nil))
      (setq misc-pairs (cdr (cdr misc-pairs))))
    result))


;;; ================================================================
;;;                   lyskom-edit-mode

(defvar lyskom-edit-mode-hook nil
  "*List of functions to be called when entering lyskom-edit-mode.
Watch out! None of these functions are allowed to do kill-all-local-variables
because kom-edit-send and other functions depend on some variables to be able
to enter the text in the correct lyskom-process.")

(defvar lyskom-edit-mode-mode-hook nil
  "*List of functions to be called when entering lyskom-edit-mode.
Watch out! None of these functions are allowed to do kill-all-local-variables
because kom-edit-send and other functions depend on some variables to be able
to enter the text in the correct lyskom-process.

This one differs from lyskom-edit-mode-hook in that it is called before
the lyskom-special key bindings are added.")


(defun lyskom-edit-mode ()
  "\\<lyskom-edit-mode-map>Mode for editing texts for LysKOM.
Commands:
\\[kom-edit-send]   sends the text when you are ready. The buffer will be
	  deleted if (and only if) the server accepts the text.
\\[kom-edit-quit]   aborts the editing. You will get back to the LysKOM buffer.

\\[kom-edit-show-commented]   shows the commented text in a temporary buffer.

\\[kom-edit-add-recipient]   asks for another recipient and adds him to the header.
\\[kom-edit-add-copy]   as \\[kom-edit-add-recipient] but adds him as copy-recipient.

\\[kom-edit-insert-commented]   inserts the commented of footnoted text.
\\[kom-edit-insert-text]   inserts the shown text, you tell the number.

The mode is based on the mode in the lyskom-edit-mode-mode-hook or fundamental.
Entry to this mode runs lyskom-edit-mode-hook."
  (interactive)
  (lyskom-clear-vars)
  (setq mode-line-buffer-identification '("LysKOM (server: %b)"))
  (text-mode)
  (run-hooks 'lyskom-edit-mode-mode-hook)
  (setq lyskom-edit-mode-mode-map (and (current-local-map)
				       (copy-keymap (current-local-map))))
  (setq major-mode 'lyskom-edit-mode)
  (setq mode-name lyskom-edit-mode-name)
  ;; (setq buffer-offer-save t)
  (use-local-map (overlay-map lyskom-edit-mode-mode-map
			      lyskom-edit-mode-map))
  (auto-save-mode 1)
  (auto-fill-mode 1)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'lyskom-proc)
  (setq paragraph-start (concat "^" 
				(regexp-quote 
				 lyskom-header-separator)
				"$\\|^"
				(regexp-quote
				 lyskom-swascii-header-separator)
				"$\\|" paragraph-start))
  (setq paragraph-separate (concat "^" 
				   (regexp-quote 
				    lyskom-header-separator)
				   "$\\|^"
				   (regexp-quote
				    lyskom-swascii-header-separator)
				   "$\\|" paragraph-separate))
  (run-hooks 'lyskom-edit-mode-hook))


;;; ================================================================
;;;   Functions bound to keyboard seqences in lyskom-edit-mode
;;;


(defun kom-edit-send ()
  "Send the text to the server."
  (interactive)
  (if (or (string= mode-name lyskom-edit-mode-name)
	  (j-or-n-p (lyskom-get-string 'already-sent)))
      (progn 
	(let ((buffer (current-buffer))
	      misc-list subject message)

	  (save-excursion
	    (setq misc-list (lyskom-edit-read-misc-list)
		  subject (lyskom-edit-extract-subject)))
	  (let ((minibuffer-local-map (overlay-map lyskom-edit-mode-mode-map
						   minibuffer-local-map)))
	    (setq subject (lyskom-read-string (lyskom-get-string 'subject)
					      subject)))
	  (setq message (lyskom-edit-extract-text))
	  (setq mode-name "LysKOM sending")
	  (save-excursion
	    (set-buffer (process-buffer lyskom-proc))
	    (setq lyskom-dont-change-prompt t)
	    (setq lyskom-is-writing nil)
	    (lyskom-tell-internat 'kom-tell-send)
	    (initiate-create-text 'sending 'lyskom-create-text-handler
				  (concat subject "\n" message) misc-list
				  buffer)))
	(if kom-dont-restore-window-after-editing
	    (bury-buffer)
	  (save-excursion
	    (if (boundp 'lyskom-is-dedicated-edit-window)
		(condition-case error
		    (delete-frame)
		  (error))))
	  (set-window-configuration lyskom-edit-return-to-configuration)
	  (set-buffer (window-buffer (selected-window))))
	(goto-char (point-max)))))


(defun kom-edit-quit ()
  "Kill the text (if any) written so far and continue reading."
  (interactive)
  (let ((edit-buffer (current-buffer)))
    (if kom-dont-restore-window-after-editing
	(bury-buffer)
      ;; Select the old configuration.
      (save-excursion
	(if (boundp 'lyskom-is-dedicated-edit-window)
	    (condition-case error
		(delete-frame)
	      (error))))
      (set-window-configuration lyskom-edit-return-to-configuration)
      (set-buffer (window-buffer (selected-window))))
    (goto-char (point-max))
    (setq lyskom-is-writing nil)
    (lyskom-tell-internat 'kom-tell-regret)
    (lyskom-save-excursion
     (set-buffer edit-buffer)
     (delete-auto-save-file-if-necessary))
    (kill-buffer edit-buffer)
    (lyskom-count-down-edits))
  (garbage-collect))			;Take care of the garbage.


(defun kom-edit-show-commented ()
  "Show the commented text in another window."
  (interactive)
  (lyskom-edit-get-commented 'lyskom-edit-show-commented))


(defun kom-edit-insert-commented ()
  "Insert the commented text with '>' first on each line"
  (interactive)
  (lyskom-edit-get-commented 'lyskom-edit-insert-commented))


(defun kom-edit-insert-digit-text ()
  (interactive)
  (if (boundp 'unread-command-event)	;Special for lucid-emacs
      (setq unread-command-event last-command-event)
    (setq unread-command-char last-command-char))
  (call-interactively 'kom-edit-insert-text nil))


(defun kom-edit-insert-text (no)
  "Insert the text number NO with '>' first on each line"
  (interactive (list
		(cond
		 ((null current-prefix-arg)
		  (string-to-int 
		   (read-from-minibuffer 
		    (format "%s" (lyskom-get-string 'which-text-include)))))
		 ((prefix-numeric-value current-prefix-arg)))))
  (let ((buffer (current-buffer)))
    (set-buffer (process-buffer lyskom-proc))
    (initiate-get-text 'edit 'lyskom-edit-insert-commented no buffer)
    (set-buffer buffer)
    (sit-for 0)))
    

(defun lyskom-edit-get-commented (thendo)
  "Get the commented text and then do THENDO with it."
  (let ((p (point)))
    (save-excursion
      (let* ((buffer (current-buffer))
	     (endhead (progn
			(goto-char (point-min))
			(or (re-search-forward 
			     (regexp-quote
			      (if kom-emacs-knows-iso-8859-1
				  lyskom-header-separator
				lyskom-swascii-header-separator))
			     nil t)
			    (re-search-forward 
			     (regexp-quote
			      (if kom-emacs-knows-iso-8859-1
				  lyskom-swascii-header-separator
				lyskom-header-separator))))
			(point)))
	     (found (progn
		      (goto-char (point-min))
		      (or (re-search-forward "^K[^0-9]*\\([0-9]+\\)" endhead
					     t)
			  (re-search-forward "^Fot[^0-9]*\\([0-9]+\\)" endhead
					     t))))
	     (no (and found
		      (string-to-int (buffer-substring
				      (match-beginning 1)
				      (match-end 1))))))
	(cond
	 (no
	  (goto-char p)
	  (set-buffer (process-buffer lyskom-proc))
	  (initiate-get-text 'edit thendo no buffer)
	  (set-buffer buffer)
	  (sit-for 0))
	 (t
	  (lyskom-message "%s" (lyskom-get-string 'no-such-text-m))))))))


;;; ================================================================
;;;	  Add recipient, copy-recipient - Addera mottagare 
;;;
;;;  Author: Anders Gertz
;;;  Changed by: Linus Tolke


(defun kom-edit-add-recipient ()
  "Adds a conference as recipient to the text being edited."
  (interactive)
  (lyskom-edit-add-recipient/copy (lyskom-get-string 'added-recipient)
				  (lyskom-get-string 'recipient)))


(defun kom-edit-add-copy ()
  "Adds a conference to which a copy of the edited text will be sent."
  (interactive)
  (lyskom-edit-add-recipient/copy (lyskom-get-string 'added-carbon-copy)
				  (lyskom-get-string 'carbon-copy)))


(defun lyskom-edit-add-recipient/copy (prompt string)
  "Adds a new recipient or a cc-recipient to the text which is being edited."
  (let ((marker (point-min-marker))
	(edit-buffer (current-buffer)))
    (lyskom-save-excursion
     (set-buffer (process-buffer lyskom-proc))
     (lyskom-completing-read-conf-stat 'edit 'lyskom-edit-add-recipient/copy-2
				       prompt nil 
				       nil "" string marker edit-buffer))))

(defun lyskom-edit-add-recipient/copy-2 (conf-stat string stream buffer)
  "Adds the conference with conf-stat CONF-STAT as a recipient of the
text in BUFFER. If the conference has a set motd, then show it."
  (let ((text-no (conf-stat->msg-of-day conf-stat)))
    (if (zerop text-no)
	(lyskom-edit-insert-misc-conf conf-stat string stream nil)
      (lyskom-edit-add-recipient/copy-3 (blocking-do 'get-text text-no)
					conf-stat string stream buffer))))


(defun lyskom-edit-add-recipient/copy-3 (text conf-stat string stream buffer)
  "Shows the motd TEXT in a temporary buffer and confirms that the user
still wants to add the conference with conf-stat CONF-STAT as a recipient
to the text in BUFFER."
  (if (and text (get-buffer-window buffer))
      (let ((win-config (current-window-configuration)))
	(set-buffer buffer)
	(with-output-to-temp-buffer "*Motd*"
	  (princ (lyskom-format 'conf-has-motd-no
				(text->text-no text)
				(text->text-mass text))))
	(and (j-or-n-p (lyskom-get-string 'still-want-to-add))
	     (lyskom-edit-insert-misc-conf conf-stat string stream nil))
	(set-window-configuration win-config))
    (lyskom-edit-insert-misc-conf conf-stat string stream nil)))



;;; ================================================================
;;;   Help functions for the functions bound to keyboard sequences 
;;;                       in lyskom-edit-mode.


(defun lyskom-edit-read-misc-list ()
  "Read misc-list from buffer."
  ; +++ Should use lyskom-create-misc-list.
  (goto-char (point-min))
  (let ((result (cons 'MISC-LIST nil)))
    (while (and (< (point) (point-max))
		(not (or (equal (char-to-string 
				 (elt 
				  (if kom-emacs-knows-iso-8859-1
				      lyskom-header-subject
				    lyskom-swascii-header-subject)
				  0))
				(buffer-substring (point) (1+ (point))))
			 (equal (char-to-string 
				 (elt 
				  (if kom-emacs-knows-iso-8859-1
				      lyskom-swascii-header-subject
				    lyskom-header-subject)
				  0))
				(buffer-substring (point) (1+ (point)))))))
      (let ((char (string-to-char
		   (upcase (buffer-substring (point) (1+ (point)))))))
	(nconc 
	 result
	 (cons
	  (cond
	   ((eq char (elt (lyskom-get-string 'recipient) 0)) ;recpt
	    (re-search-forward "<\\([0-9]+\\)>")
	    (cons 'recpt (string-to-int (buffer-substring
					 (match-beginning 1)
					 (match-end 1)))))
	   ((eq char (elt (lyskom-get-string 'carbon-copy) 0)) ;cc-recpt
	    (re-search-forward "<\\([0-9]+\\)>")
	    (cons 'cc-recpt (string-to-int (buffer-substring
					    (match-beginning 1)
					    (match-end 1)))))
	   ((eq char (elt (lyskom-get-string 'comment) 0)) ;comm-to
	    (re-search-forward "\\([0-9]+\\)")
	    (cons 'comm-to (string-to-int (buffer-substring
					   (match-beginning 1)
					   (match-end 1)))))
	   ((eq char (elt (lyskom-get-string 'footnote) 0)) ;footn-to
	    (re-search-forward "\\([0-9]+\\)")
	    (cons 'footn-to (string-to-int (buffer-substring
					    (match-beginning 1)
					    (match-end 1)))))
	   (t 
	    (signal 'lyskom-internal-error 
		    (list "Unknown header line: "
			  (buffer-substring (point)
					    (progn 
					      (end-of-line)
					      (point)))))))
	  nil)))
      (beginning-of-line 2))
    result))	   


(defun lyskom-edit-extract-subject ()
  "Find the subject.
Point must be located on the line where the subject is."
  (re-search-forward ": \\(.*\\)")
  (buffer-substring (match-beginning 1) (match-end 1)))


(defun lyskom-edit-extract-text ()
  "Get text as a string."
  (save-excursion
    (goto-char (point-min))
    (if (not (or (re-search-forward 
		  (regexp-quote 
		   (if kom-emacs-knows-iso-8859-1
		       lyskom-header-separator
		     lyskom-swascii-header-separator)) 
		  nil (point-max))
		 (not (or (equal (char-to-string 
				  (elt 
				   (if kom-emacs-knows-iso-8859-1
				       lyskom-header-subject
				     lyskom-swascii-header-subject)
				   0))
				 (buffer-substring (point) (1+ (point))))
			  (equal (char-to-string 
				  (elt 
				   (if kom-emacs-knows-iso-8859-1
				       lyskom-swascii-header-subject
				     lyskom-header-subject)
				   0))
				 (buffer-substring (point) (1+ (point))))))))
	(let ((char (string-to-char
		     (upcase (buffer-substring (point) (1+ (point)))))))
	  (nconc 
	   result
	   (cons
	    (cond
	     ((eq char (elt (lyskom-get-string 'recipient) 0)) ;recpt
	      (re-search-forward "<\\([0-9]+\\)>")
	      (cons 'recpt (string-to-int (buffer-substring
					   (match-beginning 1)
					   (match-end 1)))))
	     ((eq char (elt (lyskom-get-string 'carbon-copy) 0)) ;cc-recpt
	      (re-search-forward "<\\([0-9]+\\)>")
	      (cons 'cc-recpt (string-to-int (buffer-substring
					      (match-beginning 1)
					      (match-end 1)))))
	     ((eq char (elt (lyskom-get-string 'comment) 0)) ;comm-to
	      (re-search-forward "\\([0-9]+\\)")
	      (cons 'comm-to (string-to-int (buffer-substring
					     (match-beginning 1)
					     (match-end 1)))))
	     ((eq char (elt (lyskom-get-string 'footnote) 0)) ;footn-to
	      (re-search-forward "\\([0-9]+\\)")
	      (cons 'footn-to (string-to-int (buffer-substring
					      (match-beginning 1)
					      (match-end 1)))))
	     (t 
	      (signal 'lyskom-internal-error 
		      (list "Unknown header line: "
			    (buffer-substring (point)
					      (progn 
						(end-of-line)
						(point)))))))
	    nil)))
      (beginning-of-line 2))
    result))	   


(defun lyskom-edit-extract-subject ()
  "Find the subject.
Point must be located on the line where the subject is."
  (re-search-forward ": \\(.*\\)")
  (buffer-substring (match-beginning 1) (match-end 1)))


(defun lyskom-edit-extract-text ()
  "Get text as a string."
  (save-excursion
    (goto-char (point-min))
    (if (not (or (re-search-forward 
		  (regexp-quote 
		   (if kom-emacs-knows-iso-8859-1
		       lyskom-header-separator
		     lyskom-swascii-header-separator)) 
		  nil (point-max))
		 (re-search-forward 
		  (regexp-quote 
		   (if kom-emacs-knows-iso-8859-1
		       lyskom-swascii-header-separator
		     lyskom-header-separator)) 
		  nil (point-max))))
	(signal 'lyskom-internal-error
		"Altered lyskom-header-separator line.")
      (buffer-substring (1+ (point))
			(progn
			  (goto-char (1- (point-max)))
			  (while (looking-at "\\s-")	; remove trailing
			    (backward-char 1))		; whitespace
			  (forward-char 1)
			  (point))))))

  
(defun lyskom-create-text-handler (text-no edit-buffer)
  "Handle an attempt to write a text."
  (lyskom-tell-server "")
  (message "")
  (cond
   ((null text-no)
    (lyskom-insert-before-prompt
     (lyskom-format 'could-not-create-text lyskom-errno
		    (lyskom-get-error-text lyskom-errno)))
    (beep)
    (lyskom-message "%s" (lyskom-format 'could-not-create-text lyskom-errno
				   (lyskom-get-error-text lyskom-errno)))
    (set-buffer edit-buffer)
    (setq mode-name lyskom-edit-mode-name)
    (sit-for 0))
   (t
    (lyskom-insert-before-prompt
     (lyskom-format 'text-created  text-no))

    ;; Immediately mark the text as read if kom-created-texts-are-read is set.
    
    (cond
     (kom-created-texts-are-read
      (lyskom-is-read text-no)
      (initiate-get-text-stat 'background 'lyskom-mark-as-read
			      text-no)
      (lyskom-run 'background 'set 'lyskom-dont-change-prompt nil)
      (lyskom-run 'background 'lyskom-set-mode-line)))

    (set-buffer edit-buffer)		;Need local variables.

    ;; Select the old configuration.

    (let ((hnd lyskom-edit-handler)
	  (dta lyskom-edit-handler-data))
      (cond
       ((get-buffer-window edit-buffer)
	(set-window-configuration lyskom-edit-return-to-configuration)
	(set-buffer (window-buffer (selected-window)))
	(goto-char (point-max))))
	

      ;; Apply handler.

      (set-buffer (process-buffer lyskom-proc))
      (if hnd
	  (apply hnd text-no dta)))
    
    ;; Kill the edit-buffer.

    (lyskom-save-excursion
     (set-buffer edit-buffer)
     (delete-auto-save-file-if-necessary))
    (kill-buffer edit-buffer)
    (lyskom-count-down-edits))))


(defun lyskom-edit-show-commented (text editing-buffer)
  "Handles the TEXT from the return of the call of the text.
The EDITING-BUFFER is the buffer the editing is done in. If this buffer is 
not displayed nothing is done. If displayed then this buffer is chosen then 
the with-output-to-temp-buffer command is issued to make them both apear."
  (and text
       (get-buffer-window editing-buffer)
       (progn
	 (set-buffer editing-buffer)
	 (with-output-to-temp-buffer "*Commented*"
	   (princ
	    (if kom-emacs-knows-iso-8859-1
		(text->text-mass text)
	      (iso-8859-1-to-swascii (text->text-mass text))))))))


(defun lyskom-edit-insert-commented (text editing-buffer)
  "Handles the TEXT from the return of the call of the text.
The text is inserted in the buffer with '>' first on each line."
  (if text
      (progn
	(set-buffer editing-buffer)
	(and (not (bolp))
	     (insert "\n"))
	(and (not (eolp))
	     (open-line 1))
	(let* ((pb (point))
	       (as (string-match "\n" (text->text-mass text)))
	       (te (substring (text->text-mass text) (1+ as))))
	  (insert te)
	  (while (< pb (point))
	    (beginning-of-line)
	    (insert 62)
	    (backward-char 2))))
    (lyskom-message "%s" (lyskom-get-string 'no-get-text))))


(defun lyskom-count-down-edits ()
  "Counts down the number of edit sessions.
Returns non-nil if there are sessions left.

Can be called from any of the lyskom-associated buffers. At least the main
buffer and edit buffers."
  (save-excursion
    (let ((proc (or (get-buffer-process (current-buffer))
		    (and (boundp 'lyskom-proc)
			 (processp lyskom-proc)
			 lyskom-proc)
		    (signal 'lyskom-internal-error
			    "lyskom-count-down-edits called from "
			    (current-buffer)))))
      (set-buffer (process-buffer proc))
      (while (and lyskom-list-of-edit-buffers
		  (not (memq (car lyskom-list-of-edit-buffers) (buffer-list))))
	(setq lyskom-list-of-edit-buffers (cdr lyskom-list-of-edit-buffers)))
      (condition-case emacs-18.55
	  (if lyskom-list-of-edit-buffers
	      (process-kill-without-query proc t)
	    (process-kill-without-query proc nil))
	(error
	 ;; You loose some if you only have emacs-18.55...
	 (message "Running emacs 18.55 or earlier? Please upgrade!")
	 (process-kill-without-query proc))))
    lyskom-list-of-edit-buffers))
      

;;; ================================================================
;;;        Maphanteringsfunktion - keymap handling.
;;;
;;; Author: Linus Tolke
;;;

(defun overlay-map (oldmap newmap)
  "Returns a map that is the union of OLDMAP and NEWMAP. NEW-MAP has priority.
This function chooses whether the returned map is a list or an array.
Currently always same type as oldmap.
BUG: does not descend in the maps."
  (cond
   ((not (keymapp oldmap))
    newmap)
   ((not (keymapp newmap))
    oldmap)
   (t
    (let ((map (copy-keymap oldmap))
	  (r 0))
      (cond
       ((fboundp 'map-keymap)		;Special for lucid-emacs
	(map-keymap
	 (function
	  (lambda (event function)
	    (define-key map (vector event) function)))
	 newmap))
       ((and (string-match "^19" emacs-version)
	     (arrayp (car (cdr newmap))))
	(while (< r (length (car (cdr newmap))))
	  (if (aref (car (cdr newmap)) r)
	      (define-key map (char-to-string r) (aref (car (cdr newmap)) r)))
	  (setq r (1+ r)))
	(mapcar
	 (function
	  (lambda (ele)
	    (define-key map 
	      (cond
	       ((integerp (car ele))
		(char-to-string (car ele)))
	       ((vector (car ele))))
	      (cdr ele))))
	 (cdr (cdr newmap))))
       ((arrayp newmap)
	(while (< r (length newmap))
	  (if (aref newmap r)
	      (define-key map (char-to-string r) (aref newmap r)))
	  (setq r (1+ r))))
       (t
	(mapcar
	 (function
	  (lambda (ele)
	    (define-key map 
	      (cond
	       ((integerp (car ele))
		(char-to-string (car ele)))
	       ((vector (car ele))))
	      (cdr ele))))
	 (cdr newmap))))
      map))))
