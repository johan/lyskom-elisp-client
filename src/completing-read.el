;;;;;
;;;;; $Id: completing-read.el,v 36.2 1993-05-05 03:12:31 linus Exp $
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
;;;; File: completing-read.el
;;;;
;;;; This file implements functions for reading a conference name
;;;; or a person name with completion and other help.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: completing-read.el,v 36.2 1993-05-05 03:12:31 linus Exp $\n"))


;;; Author: Linus Tolke


;;; ================================================================
;;;      Some entry points into the functions in this file


(defun lyskom-completing-read-conf-stat (queue handler prompt
					       type new initial &rest data)
  "Same as lyskom-completing-read, but give the handler a conf-stat
instead of a conf-no.
ARGS: QUEUE HANDLER PROMPT TYPE NEW INITIAL &rest DATA"
  (apply 'lyskom-completing-read 
	 queue 'lyskom-completing-read-conf-stat-handler
	 prompt type new initial queue handler data))


(defun lyskom-completing-read-conf-stat-handler (conf-no queue handler &rest data)
  "Take CONF-NO (returned by lyskom-completing-read) and send it through QUEUE
to HANDLER using initiate-get-conf-stat. Also send DATA to it.
If we had the empty conf-no could be 0. Then we shall return 0."
  (if (eq conf-no 0)
      (apply handler 0 data)
    (if data
	(apply 'initiate-get-conf-stat queue handler conf-no data)
      (initiate-get-conf-stat queue handler conf-no))))


(defvar lyskom-completing-map nil
  "Keymap to read the conf names with completion.")

(defvar lyskom-initial-completing-map nil
  "Keymap to hack certain lossage wrt scrolling other window in minibuffer.")


(defun lyskom-completing-read (kom-queue handler 
					 prompt type new initial &rest data)
  "Read the name of a person or conf using minibuffer.
Call server for name lookup using KOM-QUEUE. If KOM-QUEUE nil then using 
the kom-queue miniread.
Then call the function HANDLER.
The arguments PROMPT, TYPE, NEW and INITIAL are used to do the read.
PROMPT and INITIAL are strings. TYPE is 'person if only allowed to return a
person, 'conf if only a conference is accepted, nil if you will accept both.
If NEW is nil then only persons and conferences already existing are accepted.
If NEW is t then a name not yet existing and not completing to an existing name
is accepted.
If NEW is 'empty then the empty string is accepted.
The function HANDLER is called with the arguments conf-no DATA.
If the parameter NEW is t and a name not existing is read then HANDLER is 
called with the arguments name DATA.
If the parameter NEW is 'empty and the empty string is chosen then HANDLER is
called with the arguments 0 DATA."
  (if kom-queue
      nil
    (setq kom-queue 'miniread))
  (lyskom-completing-read-2 kom-queue handler
			    prompt type (eq new t) (eq new 'empty) initial
			    data (current-window-configuration)))


;;; ================================================================


(defun lyskom-completing-read-2 (kom-queue handler prompt type new empty
				 initial data curwin
				 &optional completion-buffer)
  "Read the name of person or conf using minibuffer. Use lyskom-completing-read.
This function is called by lyskom-completing-read and calls itself recursively.
It reads a name (or part of it) using the minibuffer and does all the work that
is to be done before lookup-name is called.
When/If it is decided that lookup-name is requested then that is done and the
result is treated by lyskom-completing-read-3.
The args: KOM-QUEUE HANDLER PROMPT TYPE NEW EMPTY INITIAL DATA CURWIN
	  COMPLETION-BUFFER
The TYPE is one of: nil, person and conf. nil means choose both persons and
confs.
The NEW is non-nil then accept existing and non-existing names.
If EMPTY is non-nil then accept existing names or the empty string.
The CURWIN is the window configuration that we should return to when done.
COMPLETION-BUFFER, if given, tells which window the completions are shown in.
When C-G is pressed then lyskom-end-of-command is evaled."
  (let* (window-conf
	 (window (selected-window))
	 (buffer (current-buffer))
	 (return
	  (read-from-minibuffer (if kom-emacs-knows-iso-8859-1
				    prompt
				  (iso-8859-1-to-swascii prompt))
				initial
				lyskom-initial-completing-map))
	 (result (substring return 1))
	 (control (string-to-char return))
	 no)
    (if window-conf
	(progn
	  (set-window-configuration window-conf)
	  (select-window window)))
    (set-buffer buffer)
    (cond
     ((and empty
	   (eq control ?X)
	   (string= result ""))
      (lyskom-complete-return 0 handler data curwin))

     ((and (string= result "")
	   (eq control ?X))
      (lyskom-message "%s" (concat prompt " " result " [Not sole completion]"))
      (sit-for 2)
      (lyskom-completing-read-2 kom-queue handler prompt type new empty result
				data curwin))

     ((and (eq control ?X)
	   (string-match
	    (lyskom-get-string 'person-or-conf-no-regexp)
	    result))
      (setq no (string-to-int (substring result
					 (match-beginning 1)
					 (match-end 1))))
      (initiate-get-conf-stat kom-queue 'lyskom-complete-verify-type no
			      kom-queue handler prompt type new empty 
			      result control data curwin))
     ((eq control ?G)			;QUIT
      (set-window-configuration curwin)
      (lyskom-end-of-command))
     (t
      (initiate-lookup-name kom-queue 'lyskom-completing-read-3 result
			    control kom-queue handler prompt type new empty 
			    result data curwin)))))


(defun lyskom-completing-read-3 (conf-list what
				 kom-queue handler prompt type new empty
				 initial data curwin)
  "Treats the result from the lookup-name. Use lyskom-completing-read.
This function is called by lyskom-completing-read-2 and calls 
lyskom-completing-read-2 again if necessary.
The args: CONF-LIST WHAT KOM-QUEUE HANDLER PROMPT TYPE NEW EMPTY
	  INITIAL DATA CURWIN
CONF-LIST is the result from the lookup-name call.
If WHAT is nil then just reads from the minibuffer with PROMPT and INITIAL.
If WHAT is ?L then build a list of completions using lookup to the server.
If WHAT is ?X and CONF-LIST only contains 1 element of TYPE then calls HANDLER
with arguments conf-no DATA.
If WHAT is ?C then completes the buffer contents from the names of the 
conferences in CONF-LIST.
The TYPE is one of: nil, person and conf. nil means choose both persons and
confs.
The NEW is non-nil then accept existing and non-existing names.
If EMPTY is non-nil then accept existing names or the empty string.
The CURWIN is the window configuration that we should return to when done.
When C-G is pressed then lyskom-end-of-command is evaled."
  (cond
   ((eq what ?L)			;Build completions list
    (lyskom-collect kom-queue)
    (lyskom-traverse
     conf-no (lyskom-get-from-conf-list (conf-list->conf-types conf-list)
					(conf-list->conf-nos conf-list)
					type)
     (initiate-get-conf-stat kom-queue nil conf-no))
    (lyskom-list-use kom-queue 'lyskom-show-completions
		     kom-queue handler prompt type new empty
		     initial data curwin))

   ((or (eq what ?X)			;Complete and exit
	(eq what ?C))			;Complete
    (let ((choice (if type
		      (lyskom-get-from-conf-list
		       (conf-list->conf-types conf-list)
		       (conf-list->conf-nos conf-list)
		       type)
		    (conf-list->conf-nos conf-list))))
      (cond
       ((and (= (length choice) 1)
	     (eq what ?X))
	(lyskom-complete-return (elt choice 0) handler data curwin))
       ((and new
	     (= (length choice) 0)
	     (eq what ?X))
	(lyskom-complete-return initial handler data curwin))
       ((or (eq what ?C)
	    (and (eq what ?X)
		 (> (length choice) 1)))
	(lyskom-collect-ignore-err kom-queue)
	(lyskom-traverse
	 conf-no choice
	 (initiate-get-conf-stat kom-queue nil conf-no))
	(lyskom-list-use kom-queue 'lyskom-complete-and-edit
			 what kom-queue handler prompt type new empty initial
			 data curwin))
       (t
	(lyskom-message "%s" (concat prompt initial
				     (if (= (length choice)
					    0)
					 " [No match]"
				       " [Next char not unique]")))
	(sit-for 2)
	(lyskom-completing-read-2 kom-queue handler prompt type new empty 
				  initial data curwin)))))

   (t
    (signal 'lyskom-internal-error
	    (list 'lyskom-completing-read
		  (char-to-string what))))))


(defun lyskom-show-completions (conf-stat-list
				kom-queue handler prompt type new empty
				initial data curwin)
  "Show all completions collected."
  (let (completion-buffer)
    (with-output-to-temp-buffer " *Completions*"
      (princ "Possible completions are:\n")
      (mapcar '(lambda (conf-stat)
		 (princ (conf-stat->name conf-stat))
		 (terpri))
	      conf-stat-list)
      (setq completion-buffer standard-output))
    (lyskom-run kom-queue 'lyskom-completing-read-2
		kom-queue handler prompt type new empty initial
		data curwin completion-buffer)))


;;; The problem:
;;;    When getting a list of possible completions, C-M-v scrolls
;;;    the wrong window (the LysKOM buffer instead of the
;;;    *Completions* buffer, as from M-x and other commands).

;;; Cause:
;;;    Normally, when getting a completion list from other commands
;;;    using 'completing-read', the minibuffer routines sets the
;;;    variable 'minibuffer-scroll-window' to point to the *Completions*
;;;    window.  This is difficult to do from lyskom-completing-read,
;;;    since the minibuffer is temporary exited when getting completions.

;;; Solution:
;;;    Set minibuffer-scroll-window in the minibuffer.  But this is
;;;    tricky.  What we do, is that we give a special keymap to
;;;    read-minibuffer, where *all* keys (except C-g) are bound to
;;;    lyskom-hack-minibuf.  This function sets minibuffer-scroll-window
;;;    to the correct value, switches keymap to lyskom-completing-map,
;;;    which is the map doing the real job, and push back the given
;;;    character to be read again (and thus interpreted in the new
;;;    keymap context).
;;;
;;;    The window to be scrolled is identified by the buffer in the
;;;    parameter completion-buffer to lyskom-completing-read-2, which
;;;    is set in lyskom-show-completions.

(defun lyskom-hack-minibuf ()
  "Fix lossage due to minibuffer deletion when requesting alternatives."
  (interactive)
  (setq minibuffer-scroll-window
	(and completion-buffer
	     (get-buffer-window completion-buffer)))
  (use-local-map lyskom-completing-map)
  (if (fboundp 'map-keymap)		;Special for lucid-emacs.
      (setq unread-command-event last-command-event)
    (setq unread-command-char last-command-char)))


(defun lyskom-complete-and-edit (conf-stats what
				 kom-queue handler prompt type new empty 
				 initial data curwin)
  "Used to catch the lookup return when typing \\[lyskom-complete].
Or when typing \\[lyskom-complete-and-exit] and there are several alternatives
then completing and trying to read again."
  (let (nos
	(completion-ignore-case t))	;+++ should perhaps be 
					;lyskom-ignore-case-in-names flag
					;instead 
    (cond
     ((null conf-stats)			;+++ Annan felhantering
      (lyskom-completing-read-2 kom-queue handler prompt type new empty initial
				data curwin))
     ((null (cdr conf-stats))		;Sole completion.
      (if (string= initial (conf-stat->name (car conf-stats)))
	  (progn
	    (lyskom-message "%s" (concat prompt initial " [Sole completion]"))
	    (sit-for 2)))
      (lyskom-completing-read-2 kom-queue handler prompt type new empty
				(conf-stat->name (car conf-stats))
				data curwin))
     ((and (eq what ?X)			;Exact match is OK even if other
	   (setq nos (car (apply 'append;matches exist.
				 (mapcar
				  'lyskom-complete-test-exact-name
				  conf-stats)))))
      (lyskom-complete-return nos handler data curwin))
     (t					;Difficult one.
      (let ((newname
	     (lyskom-try-complete-partials initial
					   (mapcar 
					    'lyskom-complete-get-name-in-list
					    conf-stats))))
	(if completion-auto-help	;Build list more often.
					;Easier here, we have the conf-stats
	    (lyskom-show-completions conf-stats kom-queue handler prompt 
				     type new empty
				     newname data curwin)
	  (lyskom-completing-read-2 kom-queue handler prompt type new empty
				    newname data curwin)))))))


(defun lyskom-complete-test-exact-name (stat)
  "Returns a list containing the conf-no if CONF-STAT matches exactly.
The variable that the name is tested against is the locally bound initial."
  (and (string= (upcase (conf-stat->name stat))
		(upcase initial))
       (list (conf-stat->conf-no stat))))


(defun lyskom-complete-get-name-in-list (stat)
  "Returns a list of the name (a string) in CONF-STAT."
  (list (conf-stat->name stat)))

(defun lyskom-try-complete-partials (string alist)
  "Returns the longest string matching STRING.
Where every word matches the corresponding word in the car part of ALIST.
parst matching ([^)]) in string and alist are disgarded."
  (let* ((a-whitespace "\\([ \t]\\|([^)]*)\\)+")
	 (endfirstword (string-match a-whitespace string))
	 (firstword (substring string 0 endfirstword))
	 (reststring (and endfirstword
			  (substring string (match-end 0))))
	 (words (or (try-completion firstword alist)
		    string))		;+++ Buggfix. Inget error om []\->{}|
	 (endfirstwords (string-match a-whitespace words))
	 (firstwords (substring words 0 endfirstwords))
	 (restlist (mapcar
		    (function
		     (lambda (part)
		       (cond
			((string-match a-whitespace
				       (car part))
			 (list (substring (car part) (match-end 0))))
			((list "")))))
		    alist)))
    (if	(= (length reststring) 0)
	words
      (concat (if (> (length firstwords) (length firstword))
		  firstwords
		firstword)
	      " " (lyskom-try-complete-partials reststring
						restlist)))))
	

(defun lyskom-complete-verify-type (conf-stat
				    kom-queue handler prompt type new empty
				    initial what data curwin)
  "Used to allow people to write [mp] <number> also."
  (if (and conf-stat			;+++ Annan felhantering
	   (or (null type)
	       (and (eq type 'person)
		    (conf-type->letterbox (conf-stat->conf-type conf-stat)))
	       (and (eq type 'conf)
		    (not (conf-type->letterbox
			  (conf-stat->conf-type conf-stat))))))
      (lyskom-complete-return (conf-stat->conf-no conf-stat) handler
			      data curwin)
    (lyskom-completing-read-2 kom-queue handler prompt type new empty initial
			      data curwin)))


(if lyskom-completing-map
    nil
  (setq lyskom-completing-map (make-sparse-keymap))
  (define-key lyskom-completing-map "\n" 'lyskom-complete-and-exit)
  (define-key lyskom-completing-map "\r" 'lyskom-complete-and-exit)
  (define-key lyskom-completing-map "\t" 'lyskom-complete)
  (define-key lyskom-completing-map "\C-g" 'lyskom-complete-quit)
;  (define-key lyskom-completing-map "\C-\M-v" 'lyskom-scroll-other-minibuffer)
  (define-key lyskom-completing-map "?" 'lyskom-complete-help))

(if lyskom-initial-completing-map
    nil
  (setq lyskom-initial-completing-map (make-keymap))
  (if (fboundp 'map-keymap)		;lucid-emacs' way of doing things.
      (map-keymap 
       (function (lambda (keydesc binding)
		   (define-key lyskom-initial-completing-map keydesc 
		     'lyskom-hack-minibuf)))
       global-map)
    (let ((i (length lyskom-initial-completing-map)))
      (while (>= (setq i (1- i)) 0)
	(aset lyskom-initial-completing-map i 'lyskom-hack-minibuf))))
  (define-key lyskom-initial-completing-map "\C-g" 'lyskom-complete-quit))
   

(defun lyskom-complete-and-exit ()
  "Exit the minibuffer if the contents matches a single type."
  (interactive)
  (setq window-conf (current-window-configuration))
  (goto-char (point-min))
  (insert "X")
  (exit-minibuffer))


(defun lyskom-complete-quit ()
  "Exit the minibuffer if the contents matches a single type."
  (interactive)
  (setq window-conf (current-window-configuration))
  (goto-char (point-min))
  (insert "G")
  (exit-minibuffer))


(defun lyskom-complete-help ()
  "Build a list of completions and show it."
  (interactive)
  (setq window-conf (current-window-configuration))
  (goto-char (point-min))
  (insert "L")
  (exit-minibuffer))


(defun lyskom-complete ()
  "Insert the completing parts of the name."
  (interactive)
  (setq window-conf (current-window-configuration))
  (goto-char (point-min))
  (insert "C")
  (exit-minibuffer))


(defun lyskom-scroll-other-minibuffer ()
  "Scrolls the buffer with completions if there is one."
  (interactive)
  (setq minibuffer-scroll-window (get-buffer-window " *Completions*"))
  (scroll-other-window))


(defun lyskom-get-from-conf-list (type-list conf-nos type)
  "Returns a list of all conf-nos of a certain type.
The sequence TYPE-LIST and CONF-NOS have to be corresponding. The TYPE is the 
one chosen. (nil person or conf see lyskom-completing-read)."
  (let* ((list nil)
	 (r 0))
    (while (< r (length type-list))
      (let ((conf-type (elt type-list r))
	    (conf-no (elt conf-nos r)))
	(if (or (null type)
		(and (eq type 'person)
		     (conf-type->letterbox conf-type))
		(and (eq type 'conf)
		     (not (conf-type->letterbox conf-type))))
	    (setq list (cons conf-no list))))
      (setq r (1+ r)))
    (nreverse list)))
      
	

(defun lyskom-complete-return (return handler data curwin)
  "Returns from lyskom-completing-read.
Arguments: RETURN HANDLER DATA CURWIN.
Sets the window configuration back to CURWIN. Then applies HANDLER with 
arguments RETURN and DATA."
  (set-window-configuration curwin)
  (apply handler return data))


