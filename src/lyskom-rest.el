;;;; ================================================================
;;;; ================================================================
;;;;
;;;; File: lyskom-rest.el
;;;;
;;;; This is the rest of what was once the entire source code of the
;;;; client. Now most of the source resides in other files. Eventually
;;;; the functions in here will be distributed out to other files.
;;;; Below follows the original header of the LysKOM elisp client:
;;;;
;;;; ================================================================
;;;;
;;;; Simple LysKOM elisp client.
;;;;
;;;; Originally written by Per Cederqvist.
;;;;
;;;; Heavily hacked by:
;;;;    Thomas Bellman
;;;;    Linus Tolke
;;;;    Inge Wallin
;;;;   and others.
;;;;
;;;; Some ideas stolen from lpmud.el written by Lars Willf|r and Thomas Bellman
;;;;
;;;; Conventions:
;;;;
;;;;    All functions, variables etc follow one of these conventions:
;;;;		lyskom-		general functions.
;;;;		cache-		cache-routines.
;;;;		initiate-	call a service. (Might look up in cache)
;;;;		kom-		user kommands.
;;;;		lyskom-parse-	parse (part of) a reply from the server.
;;;;				these are found in parse.el
;;;;		lyskom-edit-	Things related to the editing of texts.
;;;;		{lyskom,kom}-edit-options-
;;;;				things related to editing options
;;;;				(found in flags.el)
;;;;		{lyskom,kom}-prioritize-
;;;;				things related to the prioritize
;;;;				buffer. The file is prioritize.el
;;;;		{lyskom,kom}-{completing-read,completing,complete}-
;;;;				things related to
;;;;				lyskom-completing-read. Found in
;;;;				completing-read.el 
;;;;		lyskom-*-hook   hooks.
;;;;
;;;;    Three plus-signs (+++) in the code marks a place where more
;;;;    work is needed.
;;;;

;;;; ================================================================
;;;;                             New errors.


(put 'lyskom-protocol-error 'error-conditions
     '(error lyskom-error lyskom-protocol-error))
(put 'lyskom-protocol-error 'error-message
     "LysKOM protocol error:")

(put 'lyskom-internal-error 'error-conditions
     '(error lyskom-error lyskom-internal-error))
(put 'lyskom-internal-error 'error-message
     "Internal LysKOM error.")


;;; ================================================================
;;;             Error reporting from a number of commands.
;;;

;;; Author: Inge Wallin



(defun lyskom-get-error-text (errno)
  "Get a string which is the error ERRNO in plain text."
  (let ((pair (assoc errno lyskom-error-texts)))
    (if pair 
	(cdr pair)
      (lyskom-format 'error-not-found errno))))


(defun lyskom-handle-command-answer (answer)
  "Handles a void return from call to the server."
  (if answer 
      (lyskom-insert-string 'done)
    (lyskom-insert-string 'nope)
    (lyskom-format-insert 'error-code
			  (lyskom-get-error-text lyskom-errno)
			  lyskom-errno))
  (lyskom-end-of-command))


;;; ----------------------------------------------------------------
;;; Author: Aronsson

(defun lyskom-tell-string (key)
  "Retrieve the phrase indexed by the key from the kom-tell-phrases
assoc list."
  (car (cdr (assoc key kom-tell-phrases))))


(defun lyskom-tell-internat (key)
  "Same as lyskom-tell-server, but use a key to a list of phrases."
  (lyskom-tell-server (lyskom-tell-string key)))


;;;; ================================================================
;;;; Aronsson was here 5 MAY 1991
					; kom-dict-*
(defconst kom-dict
  (append
   '(
     (kom-dict-vfsh		"VFSH") ; sample assoc entry
     )
   kom-tell-phrases			; Really?
   lyskom-commands)			; Really?

  "These texts are used all over the client. They were collected into
one huge assoc list to facilitate internationalization and personal
profiling. Users are encouraged to change this dictionary for fun.

Related variables are kom-tell-phrases and lyskom-commands.")

(defun lyskom-dict-string (key)
  "Retrieve the string indexed by the key from the kom-dict assoc list."
  (car (cdr (assoc key kom-dict))))


;;;; ================================================================
;;;;                User-level commands and functions.


(defun lyskom-command-name (command)
  "Get the command name for the command COMMAND"
  (car (cdr (assoc command lyskom-commands))))


(defun kom-extended-command ()
  "Read a LysKOM function name and call the function."
  (interactive)
  (let* ((completion-ignore-case t)
	 (alternatives (mapcar (function reverse)
			       lyskom-commands))
	 (name (completing-read (lyskom-get-string 'extended-command)
				alternatives nil t nil))
	 (fnc (reverse-assoc (car (all-completions name alternatives)) 
			     lyskom-commands)))
    (cond
     (fnc (call-interactively (car fnc)))
     (t (kom-next-command)))))


;;; Resume operation after a crash.

(defun kom-recover ()
  "Try to recover from an error."
  (interactive)
  (lyskom-init-parse)
  (setq lyskom-call-data nil)
  (setq lyskom-pending-calls nil)
  (setq lyskom-output-queue (lyskom-queue-create))
  (setq lyskom-number-of-pending-calls 0)
  (setq lyskom-is-parsing nil)
  (lyskom-tell-internat 'kom-tell-recover)
  (lyskom-end-of-command))


;;; ================================================================
;;;                      Run default command.


;;; Author: Tommy Persson

(defun kom-page-next-command ()
  "Scroll the bottom to the top and run the next command if it should be run"
  (interactive)
  (if (pos-visible-in-window-p (point-max))
      (progn
	(recenter 0)
	(kom-next-command))
    (recenter 0)
    (move-to-window-line -1)
    (beginning-of-line)
    (setq lyskom-last-viewed (point))
    (end-of-line)))


;;; Author: Per Cederqvist

(defun kom-next-command ()
  "Run next command or scroll one page."
  (interactive)
  (lyskom-next-command-or-scroll (- (window-height (selected-window)) 2)))

  
(defun kom-line-next-command ()
  "Run next command or scroll one line."
  (interactive)
  (lyskom-next-command-or-scroll 1))


(defun lyskom-next-command-or-scroll (no-of-lines)
  "Run next command or scroll (at most) NO-OF-LINES lines."
  (let ((doing-default-command t))
  (cond
   ((not (pos-visible-in-window-p (point-max)))
    (lyskom-scroll-lines no-of-lines))
   ((eq lyskom-command-to-do 'next-pri-text)
    (lyskom-view-priority-text))
   ((eq lyskom-command-to-do 'next-text)
    (kom-view-next-text))
   ((eq lyskom-command-to-do 'next-conf)
    (kom-go-to-next-conf))
   ((eq lyskom-command-to-do 'next-pri-conf)
    (lyskom-go-to-pri-conf))
   ((eq lyskom-command-to-do 'when-done)
    (let ((command (lyskom-what-to-do-when-done)))
      (cond
       ((stringp command)
	(execute-kbd-macro command))
       (t (call-interactively command)))))
   ((eq lyskom-command-to-do 'unknown)
    (lyskom-insert
     (lyskom-get-string 'wait-for-server)))
   (t (signal 'lyskom-internal-error '(kom-next-command))))))


;;; ================================================================
;;;                              View text.


(defun kom-initial-digit-view ()
  (interactive)
  (setq unread-command-char last-command-char)
  (call-interactively 'kom-view nil))


(defun kom-view (text-no)
  "View text number TEXT-NO."
  (interactive "P")
  (lyskom-start-of-command 'kom-view)
  (if (setq text-no (cond ((null text-no) nil)
			  ((listp text-no) (car text-no))
			  (t text-no)))
      nil
    (setq text-no (lyskom-read-number (lyskom-get-string 'review-text-q)
				      lyskom-current-text)))
  (lyskom-tell-internat 'kom-tell-review)
  (lyskom-format-insert 'review-text-no text-no)
  (lyskom-view-text 'main text-no)
  (lyskom-run 'main 'lyskom-end-of-command))


;;;; ================================================================
;;;;                         View next text.


(defun kom-view-next-new-text ()
  "Display next new text (putting all review constructs at the end of this conf)."
  (interactive)
  (let ((len (read-list-length lyskom-reading-list))
	(finished nil))
    (while (and (not finished)
		(> len 1))
      (let ((type (read-info->type (read-list->first lyskom-reading-list))))
	(cond 
	 ((or (eq type 'REVIEW)
	      (eq type 'REVIEW-TREE)
	      (eq type 'REVIEW-MARK))
	  (read-list-rotate lyskom-reading-list))
	 ((or (eq type 'COMM-IN)
	      (eq type 'FOOTN-IN))
	  (set-read-list-del-first lyskom-reading-list))
	 (t 
	  (setq finished t))))
      (-- len)))
  (kom-view-next-text))


(defun kom-view-next-text ()
  "Display next text (from lyskom-reading-list)."
  (interactive)
  (lyskom-start-of-command 'kom-view-next-text)
  (lyskom-tell-internat 'kom-tell-read)
  (if (read-list-isempty lyskom-reading-list)
      (progn
	(if (/= 0 lyskom-current-conf)
	    (lyskom-insert-string 'completely-read-conf)
	  (lyskom-insert-string 'not-in-any-conf))
	(lyskom-end-of-command))

    (progn
      (let* ((tri             (read-list->first lyskom-reading-list))
	     (text-no         (car (cdr (read-info->text-list tri))))
	     (type            (read-info->type tri))
	     (priority 	      (read-info->priority
			       (read-list->first lyskom-reading-list)))
	     (is-review-tree  (eq type 'REVIEW-TREE))
	     (is-review       (or (eq type 'REVIEW)
				  (eq type 'REVIEW-MARK)
				  is-review-tree))
	     (mark-as-read    (not is-review)))
	(if is-review
	    (delq text-no (read-info->text-list tri)))  ;First entry only
	(if mark-as-read
	    (lyskom-is-read text-no)
	  (read-list-delete-text nil lyskom-reading-list))
	(lyskom-view-text 'main text-no mark-as-read (and kom-read-depth-first
							  (not is-review))
			  (read-info->conf-stat
			   (read-list->first lyskom-reading-list))
			  priority
			  is-review-tree)
	(lyskom-run 'main 'lyskom-end-of-command)))))


(defun lyskom-view-priority-text ()
  "Display the first text from the next conference on the lyskom-to-do-list."
  (lyskom-start-of-command 'kom-view-next-text)
  (lyskom-tell-internat 'kom-tell-read)
  (let* ((tri (read-list->first lyskom-to-do-list))
	 (priority (read-info->priority
		    (read-list->first lyskom-reading-list)))
	 (text-no (car (text-list->texts (read-info->text-list tri)))))
    (lyskom-is-read text-no)
    (lyskom-view-text 'main text-no t nil (read-info->conf-stat tri) 
		      priority nil))
  (lyskom-run 'main 'lyskom-end-of-command))


(defun lyskom-is-read (text-no)
  "Tell lyskom that TEXT-NO is about to be read.
Deletes TEXT-NO from lyskom-reading-list and lyskom-to-do-list.
Adds info in lyskom-membership."
  (read-list-delete-text text-no lyskom-reading-list)
  (read-list-delete-text text-no lyskom-to-do-list)
  (initiate-get-text-stat 'background 'lyskom-is-read-handler
			  text-no))


(defun lyskom-is-read-handler (text-stat)
  "Update lyskom-membership for all recipients to TEXT-STAT."
  (lyskom-traverse
   misc (text-stat->misc-info-list text-stat)
   (if (or (eq 'RECPT (misc-info->type misc))
	   (eq 'CC-RECPT (misc-info->type misc)))
       (let ((membership (lyskom-member-p (misc-info->recipient-no misc))))
	 (if membership
	     (set-membership->read-texts
	      membership
	      (vconcat (vector (misc-info->local-no misc))
		       (membership->read-texts membership))))))))


;;; ================================================================
;;;                        Go to next conf.


(defun kom-go-to-next-conf ()
  "Go to next conf.
Take first conf from lyskom-to-do-list and copy it to lyskom-reading-list.
Tell server what the user is doing. If the user is reading a conf it is
moved last on lyskom-to-do-list, with priority 0."
  (interactive)
  (lyskom-start-of-command 'kom-go-to-next-conf)
  (lyskom-maybe-move-unread t)
  (lyskom-go-to-next-conf))


(defun lyskom-go-to-pri-conf ()
  "Go to the conf with higher priority.
This differs from kom-go-to-next-conf only in the place where the yet unread 
in the current conf is placed."
  (lyskom-start-of-command 'kom-go-to-next-conf)
  (lyskom-maybe-move-unread nil)
  (lyskom-go-to-next-conf))


(defun lyskom-go-to-next-conf ()
  "Actually go to the next conference on the lyskom-to-do-list"
  ;; Copy first element on lyskom-to-do-list to lyskom-reading-list.
  (if (not (read-list-isempty lyskom-to-do-list))
      (progn
	(read-list-enter-first
	   (read-list->first lyskom-to-do-list)
	   lyskom-reading-list)

	;; Tell server which conf the user is reading.
	(if (read-info->conf-stat (read-list->first lyskom-reading-list))
	    (let ((conf-stat (read-info->conf-stat
			      (read-list->first lyskom-reading-list))))
	      (lyskom-enter-conf
	       conf-stat 
	       (read-list->first lyskom-reading-list))
	      (lyskom-set-mode-line conf-stat))))
    (lyskom-insert-string 'all-conf-unread-r)
    (lyskom-set-mode-line (lyskom-get-string 'all-conf-unread-s)))
  (lyskom-end-of-command))


(defun lyskom-maybe-move-unread (burry)
  "If leaving conf with unread put the unread texts list on the to-do-list.
If the argument BURRY is non-nil then the conf is inserted last altered."
  (if (not (read-list-isempty lyskom-reading-list))
      (progn
	(if burry
	    (let ((conf-no nil)
		  (r 0))
	      (while (and (not conf-no)
			  (< r (read-list-length lyskom-reading-list)))
		(if (eq (read-info->type (read-list->nth lyskom-reading-list 
							 r))
			'CONF)
		    (setq conf-no (conf-stat->conf-no 
				   (read-info->conf-stat
				    (read-list->nth lyskom-reading-list r))))
		  (++ r)))
	      (if conf-no
		  (progn
		    (read-list-delete-read-info conf-no lyskom-to-do-list)
		    (read-list-enter-first (read-list->nth lyskom-reading-list
							   r)
					   lyskom-to-do-list)
		    (read-list-rotate lyskom-to-do-list)))))
	(set-read-list-empty lyskom-reading-list))))


(defun lyskom-enter-conf (conf-stat read-info)
  "Tell server which conf the user is reading.
Prints the name and amount of unread in the conference we just went to 
according to the value of kom-print-number-of-unread-on-entrance.
Args: CONF-STAT READ-INFO"
  (initiate-pepsi 'main nil (conf-stat->conf-no conf-stat))
  (setq lyskom-current-conf (conf-stat->conf-no conf-stat))
  (lyskom-insert
   (concat (conf-stat->name conf-stat)
	   (if (not kom-print-number-of-unread-on-entrance)
	       "\n"
	     (let ((num_unread
		    (length (text-list->texts
			     (read-info->text-list
			      read-info)))))
	       (if (= num_unread 1)
		   (lyskom-get-string 'one-unread)
		 (lyskom-format 'several-unread num_unread)))))))

  

(defun lyskom-put-back-on-to-do-list (read-list low-priority)
  "Add the conferences on READ-LIST on lyskom-to-do-list.
Alter the priority to kom-low-priority if LOW-PRIORITY is non-nil."
  (lyskom-traverse
   to-read (read-list->all-entries read-list)
   (if (or (eq 'CONF (read-info->type to-read))
	   (eq 'REVIEW-MARK (read-info->type to-read)))
    ;; Only unread conferences and viewings of marked texts
    ;; are put back on the to-do-list.
    ;; E. g. unread comments to a text are not put back since they
    ;; are anyhow present on a 'CONF-item's text-list.
       (progn
	 (if (and kom-low-priority
		  low-priority)
	     (set-read-info->priority to-read kom-low-priority))
	 (read-list-enter-read-info to-read
				    lyskom-to-do-list
				    (not low-priority))))))


;;;================================================================

;;; Whereto?


(defun lyskom-set-mode-line (&optional conf)
  "Sets mode-line-conf-name to the name of the optional argument conf CONF.
CONF can be a a conf-stat or a string."
  (let ((name (cond
	       ((null conf) 
		(if (zerop lyskom-current-conf)
		    (lyskom-get-string 'not-present-anywhere)
		  (initiate-get-conf-stat 'modeline 'lyskom-set-mode-line
					  lyskom-current-conf)
		  nil))
	       ((stringp conf) conf)
	       ((lyskom-conf-stat-p conf)
		(conf-stat->name conf))
	       (t "")))
	(unread -1)
	(read-info-list nil))

    ; Set unread to the number of unread texts in CONF.
    (if (lyskom-conf-stat-p conf)
	(progn
	  (setq read-info-list (read-list->all-entries lyskom-to-do-list))
	  (while (and (= unread -1)
		     read-info-list)
	    (if (and (read-info->conf-stat (car read-info-list))
		     (= (conf-stat->conf-no conf)
			(conf-stat->conf-no 
			 (read-info->conf-stat (car read-info-list)))))
		(setq unread (length (text-list->texts
				      (read-info->text-list 
				       (car read-info-list)))))
	      (setq read-info-list (cdr read-info-list))))
	  (if (= unread -1)
	      (setq unread 0))))

    (if (null name)
	nil
      (setq mode-line-conf-name 
	    (substring (concat (if (lyskom-conf-stat-p conf)
				   (format "(%d) " unread)
				 "")
			       name
			       (make-string 27 ? ))
		       0 27))
      (if (not kom-emacs-knows-iso-8859-1)
	  (setq mode-line-conf-name
		(iso-8859-1-to-swascii mode-line-conf-name))))))


;;; ================================================================

;;; +++Where should this be moved???


(defun lyskom-member-p (conf-no)
  "Returns non-nil if conference CONF-NO is present on lyskom-membership.
The value is actually the membership for the conference."
  (let ((list lyskom-membership)
	(found nil))
    (while (and (not found) (not (null list)))
      (if (= conf-no (membership->conf-no (car list)))
	  (setq found (car list)))
      (setq list (cdr list)))
    found))


;;; ================================================================
;;; +++ Where shall this function move?

(defun lyskom-return-time (time)
  "Return a string containing the time TIME."
  (lyskom-format 'time-yyyy-mm-dd-hh-mm
	  (+ (time->year time) 1900)
	  (1+ (time->mon  time))
	  (time->mday time)
	  (time->hour time)
	  (time->min  time)))


;;;; ================================================================
;;;;                   Scrolling and text insertion.


(defun lyskom-scroll-lines (no-of-lines)
  "Scroll at most NO-OF-LINES lines, but only as much as needed
to show the end of the buffer."
  (interactive)
  (while (and (not (zerop no-of-lines))
	      (not (pos-visible-in-window-p (point-max))))
    (scroll-up 1)
    (-- no-of-lines))
  (move-to-window-line -1)
  (beginning-of-line)
  (setq lyskom-last-viewed (point))
  (end-of-line))


(defun lyskom-scroll ()
  "Scroll screen if necessary.
The position lyskom-last-viewed will always remain visible."
  ;; Find a window to scroll. Scroll the selected window if that shows LysKOM.
  ;; Otherwise scroll any window currently showing LysKOM.
  (let ((win
	 (cond
	  ((eq (window-buffer (selected-window)) (current-buffer))
	   (selected-window))
	  (t				;(get-buffer-window (current-buffer))
	   nil))))
    (cond
     (win				;Do nothing if no window showed LysKOM.
      (while
	  (and
	   (not (pos-visible-in-window-p (point-max) win))
	   (< (window-start win)
	      lyskom-last-viewed))
	(scroll-up 1)
	(end-of-line 2))))))
	

(defun lyskom-insert (string)
  "Insert STRING last in current buffer.
Never scrolls but leaves the point at the end of the buffer if possible without
scrolling. Leaves the point at the end of the window if not possible.
If buffer is not on screen then doesn't move point."
  (goto-char (point-max))
  (let ((buffer-read-only nil))
    (insert (if kom-emacs-knows-iso-8859-1
		string
	      (iso-8859-1-to-swascii string))))
  (let ((window (get-buffer-window (current-buffer))))
    (if window
	(if (pos-visible-in-window-p (point) window)
	    nil
	  (goto-char (window-start window))
	  (end-of-line (1- (window-height window)))))))


(defun lyskom-insert-before-prompt (string)
  "Insert STRING just before the prompt of if no prompt then just buffers.
If prompt on screen then do the scroll if necessary.
The strings buffered are printed before the prompt by lyskom-print-prompt."
  (cond
   ((and lyskom-executing-command
	 (not lyskom-is-waiting))
    (or lyskom-to-be-printed-before-prompt
	(setq lyskom-to-be-printed-before-prompt (lyskom-queue-create)))
    (lyskom-queue-enter lyskom-to-be-printed-before-prompt string))
   (t
    (goto-char (point-max))
    (let* ((window (get-buffer-window (current-buffer)))
	   (pv (and window
		    (pos-visible-in-window-p (point) window))))
      (beginning-of-line)
      (let ((buffer-read-only nil))
	(insert (if kom-emacs-knows-iso-8859-1
		    string
		  (iso-8859-1-to-swascii string))))
      (goto-char (point-max))
      (if (and pv
	       (eq window (selected-window))
	       (not (pos-visible-in-window-p (point) window)))
	  (recenter -1))
      (if window
	  (if (pos-visible-in-window-p (point) window)
	      nil
	    (goto-char (window-start window))
	    (end-of-line (1- (window-height window)))))))))


(defun lyskom-message (format-string &rest args)
  "Like message, but converts iso-8859/1 texts to swascii if necessary.
Args: FORMAT-STRING &rest ARGS"
  (let ((str (apply 'format format-string args)))
    (if kom-emacs-knows-iso-8859-1
	(message str)
      (message (iso-8859-1-to-swascii str)))))


;;; ================================================================
;;;                  Extended string formatting

;;; Author: Inge Wallin


(defun lyskom-insert-string (atom)
 "Find the string corresponding to ATOM and insert it into the LysKOM buffer." 
  (lyskom-insert (lyskom-get-string atom)))


(defun lyskom-format-insert (format-atom &rest args)
  "Format the string FORMAT-ATOM and insert it into the LysKOM buffer.
Use lyskom-format to format the string.
Args: FORMAT-ATOM &rest ARGS"
  (lyskom-insert (apply 'lyskom-format format-atom args)))


(defun lyskom-format (format-atom &rest args)
  "Find FORMAT-ATOM in the list of strings in LysKOM and call lyskom-do-format.
Args: FORMAT-ATOM &rest ARGS."
  (apply 'lyskom-do-format (lyskom-get-string format-atom) args))


(defun lyskom-get-string (atom)
  "Get the string corresponding to ATOM and return it."
  (let ((format-pair (assoc atom lyskom-strings)))
    (if (null format-pair)
	(signal 'lyskom-internal-error 
		(list 'lyskom-get-string
		      (list atom ": string not found")))
      (cdr format-pair))))


(defun lyskom-do-format (format-string &rest args)
  (let ((format-length (length format-string))
	(start 0)
	(parts nil)
	(length 0)
	(arg-no nil)
	(format-letter nil))
    (while (< start format-length)
      (if (null (string-match "%\\(-?[0-9]+\\)?#\\([0-9]+\\)\\([sdoxc]\\)"
			       format-string start))
	  (setq parts (cons (substring format-string start)
			    parts)
		start format-length)
	(setq parts (cons (substring format-string start (match-beginning 0))
			  parts)
	      length (if (match-beginning 1)
			 (string-to-int (substring format-string
						   (match-beginning 1)
						   (match-end 1)))
		       nil)
	      arg-no (if (match-beginning 2)
			 (string-to-int (substring format-string
						   (match-beginning 2)
						   (match-end 2)))
		       (signal 'lyskom-internal-error
			       (list 'lyskom-format format-string)))
	      format-letter (if (match-beginning 3)
				(aref format-string 
				      (match-beginning 3))
			      (signal 'lyskom-internal-error
				      (list 'lyskom-format format-string)))
	      start (match-end 0))
         (setq parts (lyskom-format-help parts length 
					 arg-no args format-letter
					 (if (and (match-beginning 1)
						  (eq (aref format-string
							    (match-beginning 1))
						      ?0))
					     ?0
					   ? )))))
    (apply 'concat (reverse parts))))


(defun lyskom-format-help (parts length arg-no args format-letter pad-letter)
  (let ((args-length (length args))
	(arg nil)
	(result nil)
	(abs-length (cond ((null length) nil)
			  ((< length 0) (- 0 length))
			  (t length))))
    (if (< args-length arg-no)
	(signal 'lyskom-internal-error (list 'lyskom-format
					     ": too few arguments"))
      (setq arg (nth (1- arg-no) args))
      (cond
       ((eq format-letter ?s)
	(setq result (cond ((stringp arg) arg)
			   ((symbolp arg) (symbol-name arg))
			   (t (signal 'lyskom-internal-error
				      (list 'lyskom-format
					    ": argument error"))))))
       ((or (eq format-letter ?d)
	    (eq format-letter ?o)
	    (eq format-letter ?x))
	(setq result (if (integerp arg)
			 (format (format "%%%c" format-letter)
				 arg)
		       (signal 'lyskom-internal-error
			       (list 'lyskom-format
				     ": argument error")))))
       ((eq format-letter ?c)
	(setq result (if (integerp arg)
			 (char-to-string arg)
		       (signal 'lyskom-internal-error
			       (list 'lyskom-format
				     ": argument error")))))
       ((eq format-letter ?%)
	(setq result "%"))
       (t (signal 'lyskom-internal-error
		  (list 'lyskom-format format-string))))

      ; Pad the result to appropriate length
      (cons (cond ((or (null length)
		       (< abs-length (length result)))
		   result)
		  ((< length 0)		; left align
		   (substring (concat result (make-string abs-length ? ))
			      0 abs-length))
		  (t			; right align
		   (substring (concat (make-string length pad-letter) result)
			      (- 0 length))))
	    parts))))


;;; ================================================================
;;;                      Iso-8859-1 converting

;;; Author: Linus Tolke Y


(defvar iso-8859-1-table 
  " 	
 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ 	
  !c#$Y|$\"c+?!-R~C+23'u$-,10?????AAAA[][CE@EEIIIIDNOOOO\\*\\UUU^YTBaaaa{}{ce`eeiiiidnoooo|/|uuu~yty"
  "*This is a table of the chars corresponding value in SWASCII.
Used by the function iso-8859-1-to-swascii function.")


(defun iso-8859-1-to-swascii (string)
  "Returns a string without characters with code > 127.
What chars are converted to is controlled by the iso-8859-1-table."
  (mapconcat
   (function (lambda (char)
	       (substring iso-8859-1-table char (1+ char))))
   string ""))


;;;; ================================================================
;;;;                         Running in buffer 

;;; Author: Linus


(defun backward-text (&optional arg)
  "Searches backwards for a text start and recenters with that text at the top."
  (interactive "p")
  (let ((paragraph-start lyskom-text-start))
    (backward-paragraph arg))
  (end-of-line))


(defun forward-text (&optional arg)
  "Searches forward for a text start and recenters with that text at the top."
  (interactive "p")
  (let ((paragraph-start lyskom-text-start))
    (forward-paragraph arg))
  (end-of-line))


(defun kom-save-text (&optional arg)
  "Saves/appends the article before point to a file.
The article is determined by a search-backward the same as backward-text 
and then a forward-text.
With an argument ARG the search is done over that number of texts.
The name of the file is read using the minibuffer and the default is kom-text."
  (interactive "p")
  (save-excursion
    (backward-text arg)
    (let ((p1 (point))
	  (p2 (progn
		(forward-text arg)
		(beginning-of-line)
		(point))))
      (append-to-file p1 p2
		      (expand-file-name (read-file-name (lyskom-get-string 'save-on-file-q)
							default-directory
							"kom-text"
							nil))
		      ))))


;;; ================================================================
;;;                             To-do


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
	 This function checks if doing-default-command and first-time-around 
	 are bound. The text entered in the buffer is chosen according to this"
  (if (and lyskom-is-waiting
	   (listp lyskom-is-waiting))
      (progn
	(setq lyskom-is-waiting nil)
	(lyskom-end-of-command)))
  (if (and lyskom-executing-command (not may-interrupt))
      (error (lyskom-get-string 'wait-for-prompt)))
  (if (not (and (boundp 'doing-default-command)
		doing-default-command))
      (cond
       ((and (boundp 'first-time-around)
	     first-time-around))
       ((stringp function) (lyskom-insert function))
       ((and function (symbolp function))
	(let ((name (lyskom-command-name function)))
	  (if name (lyskom-insert name)))))
    (save-excursion
      (if (not lyskom-no-prompt)
	  (let ((buffer-read-only nil))
	    (goto-char (point-max))
	    (delete-char (- (length lyskom-prompt-text ))))))
    (lyskom-insert lyskom-prompt-executing-default-command-text))
  (setq lyskom-executing-command t)
  (lyskom-insert "\n")
  (if (and (eq (window-buffer (selected-window))
	       (current-buffer))
	   (= (point) (point-max)))	;Tell user something is
      (progn
	(beginning-of-line 0)		;about to happen.
	(sit-for 0)))
;  (lyskom-scroll)
  (if kom-page-before-command		;Nice with dumb terminals.
      (recenter 0)))


(defun lyskom-end-of-command ()
  "Print prompt, maybe scroll, prefetch info."
  (message "")
  (while (and lyskom-to-be-printed-before-prompt
	      (lyskom-queue->first lyskom-to-be-printed-before-prompt))
    (lyskom-insert (lyskom-queue->first lyskom-to-be-printed-before-prompt))
    (lyskom-queue-delete-first lyskom-to-be-printed-before-prompt))
  (setq lyskom-executing-command nil)
  (setq lyskom-no-prompt t)
  (lyskom-scroll)
  (if (pos-visible-in-window-p (point-max) (selected-window))
      (save-excursion
	(beginning-of-line)
	(setq lyskom-last-viewed (point))))
  (lyskom-prefetch-and-print-prompt))


(defun lyskom-print-prompt ()
  "Print prompt if the client knows which command will be default.
Set lyskom-no-prompt otwherwise. Tell server what I am doing.
If lyskom-do-when-starting is non-nil then do the first command from this 
list."
  (setq lyskom-no-prompt nil)
  (let ((to-do (lyskom-what-to-do)))
    (setq lyskom-command-to-do to-do)
    (cond
     
     ((eq to-do 'next-pri-conf)
      (lyskom-insert-string 'go-to-pri-conf-prompt)
      (beep))

     ((eq to-do 'next-pri-text)
      (lyskom-insert-string 'read-pri-text-conf)
      (beep))

     ((eq to-do 'next-text)
      (lyskom-insert
       (let ((read-info (read-list->first lyskom-reading-list)))
	 (cond
	  ((eq 'REVIEW (read-info->type read-info))
	   (lyskom-get-string 'review-next-text-prompt))
	  ((eq 'REVIEW-TREE (read-info->type read-info))
	   (lyskom-get-string 'review-next-comment-prompt))
	  ((eq 'REVIEW-MARK (read-info->type read-info))
	   (lyskom-get-string 'review-next-marked-prompt))
	  ((= lyskom-current-conf lyskom-pers-no)
	   (lyskom-get-string 'read-next-letter-prompt))
	  ((eq 'FOOTN-IN (read-info->type read-info))
	   (lyskom-get-string 'read-next-footnote-prompt))
	  ((eq 'COMM-IN (read-info->type read-info))
	   (lyskom-get-string 'read-next-comment-prompt))
	  (t (lyskom-get-string 'read-next-text-prompt))))))

     ((eq to-do 'next-conf)
      (lyskom-insert
       (cond
	((eq 'REVIEW-MARK 
	     (read-info->type (read-list->first lyskom-to-do-list)))
	 (lyskom-get-string 'go-to-conf-of-marked-prompt))
	((/= lyskom-pers-no
	     (conf-stat->conf-no
	      (read-info->conf-stat (read-list->first
				     lyskom-to-do-list))))
	 (lyskom-get-string 'go-to-next-conf-prompt))
	(t (lyskom-get-string 'go-to-your-mailbox-prompt)))))

     ((eq to-do 'when-done)
      (if (not lyskom-is-writing)
	  (lyskom-tell-server kom-mercial))
      (setq lyskom-is-waiting t)
      (lyskom-insert (let ((command (lyskom-what-to-do-when-done t)))
		       (cond			    
			((lyskom-command-name command))
			((and (stringp command)
			      (lyskom-command-name (key-binding command))))
			(t (lyskom-format 'the-command command))))))

     ((eq to-do 'unknown)		;Pending replies from server.
      (setq lyskom-no-prompt t))

     (t (signal 'lyskom-internal-error '(lyskom-print-prompt)))))

  (if lyskom-no-prompt
      nil
    (lyskom-insert lyskom-prompt-text)
    (lyskom-maybe-do-when-starting))

    (lyskom-set-mode-line))


(defun lyskom-maybe-do-when-starting ()
  "Call a command if lyskom-do-when-starting is non-nil."
  (if lyskom-do-when-starting
      (progn
	(goto-char (point-max))
	(let ((command (car lyskom-do-when-starting))
	      (lyskom-executing-command nil))
	  (setq lyskom-do-when-starting (cdr lyskom-do-when-starting))
	  (cond
	   ((stringp command)
	    (execute-kbd-macro command))
	   ((commandp command)
	    (call-interactively command))
	   (t 
	    (lyskom-insert
	     (lyskom-format 'error-in-do-when-starting command))))))))


(defun lyskom-what-to-do ()
  "Check what is to be done. Return an atom as follows:
	next-pri-text	There is a text with higher priority to be read.
	next-pri-conf	There is a conference with higher priority to be read.
	next-text	There are texts on lyskom-reading-list.
	next-conf	There are texts on lyskom-to-do-list.
	when-done	There are no unread texts.
	unknown	        There are pending replies."
  (cond
   ((and kom-higher-priority-breaks
	 (not (read-list-isempty lyskom-reading-list))
	 (not (read-list-isempty lyskom-to-do-list))
	 (let ((type (read-info->type (read-list->first lyskom-reading-list))))
	   (or (eq kom-higher-priority-breaks 'express)
	       (eq type 'CONF)
	       (eq type 'REVIEW)
	       (eq type 'REVIEW-MARK)))
	 (> (read-info->priority (read-list->first lyskom-to-do-list))
	    (read-info->priority (read-list->first lyskom-reading-list))))
    (if (> (length 
	    (text-list->texts 
	     (read-info->text-list
	      (read-list->first lyskom-to-do-list))))
	   1)
       'next-pri-conf
      'next-pri-text))
   ((not (read-list-isempty lyskom-reading-list))
    'next-text)
   ((not (read-list-isempty lyskom-to-do-list))
    'next-conf)
   ((and lyskom-membership-is-read
	 (= (1+ lyskom-last-conf-received)
	    (length lyskom-membership)))
    'when-done)
   (t 'unknown)))


(defun lyskom-what-to-do-when-done (&optional nochange)
  "Returns a command, the next command to do from the kom-do-when-done.
If optional argument NOCHANGE is non-nil then the list wont be altered."
  (let* ((now (cdr lyskom-do-when-done))
	 (all (car lyskom-do-when-done))
	 (next (cond
		((and now (eq now all) (cdr all))
		 (cdr all))
		(t all)))
	 (command
	  (cond
	   ((commandp (car now)) (car now))
	   ((and (listp (car next))
		 (not (eq (car (car next))
			  'lambda)))
	    (car (setq now (car next))))
	   (t (car (setq now next))))))
    (if nochange
	nil
      (setq lyskom-do-when-done (cons next (cdr now))))
    command))


(defun lyskom-prefetch-and-print-prompt ()
  "Prefetch info if needed. Print prompt if not already printed."
  (if (< (lyskom-known-texts)
	 lyskom-prefetch-conf-tresh)
      (lyskom-prefetch-conf))
  (lyskom-prefetch-text)
  (if (and (listp lyskom-is-waiting)
	   (eval lyskom-is-waiting))
      (progn
	(setq lyskom-is-waiting nil)
	(beep)
	(lyskom-end-of-command)
	(if (read-list-isempty lyskom-reading-list)
	    (kom-go-to-next-conf))
	(kom-next-command)))
  (if lyskom-no-prompt
      (lyskom-print-prompt)))


(defun lyskom-known-texts ()
  "Count how many unread texts the user have, that the client knows about."
  (apply '+ (mapcar '(lambda (x) (1- (length (read-info->text-list x))))
		    (read-list->all-entries lyskom-to-do-list))))


;;
;; Called from among others kom-list-news.
;;
(defun lyskom-prefetch-all-confs (continuation)
  "Gets all conferences using prefetch. Calls itself recursively.
When all confs are fetched then the function in the argument
CONTINUATION is called."
  ;; If all conf-stats are fetched, run the continuation function
  (if (>= lyskom-last-conf-fetched
	  (1- (length lyskom-membership)))
      (lyskom-run 'main 'lyskom-run 'prefetch continuation)

    ;; ...otherwise fetch next conf-stat.
    (let ((lyskom-prefetch-conf-tresh lyskom-max-int)
	  (lyskom-prefetch-confs lyskom-max-int))
      (lyskom-prefetch-conf))
    (lyskom-run 'main 'lyskom-prefetch-all-confs continuation)))


;; ---------------------------------------------------------
;; prefetch conf-stats


(defun lyskom-prefetch-conf ()
  "Fetch conf-stats for next few conferences from lyskom-membership."
  (let ((lyskom-prefetch-confs lyskom-prefetch-confs))
    (while (and (< lyskom-last-conf-fetched
		   (1- (length lyskom-membership)))
		(< (- lyskom-last-conf-fetched lyskom-last-conf-received)
		   lyskom-prefetch-confs))
      (++ lyskom-last-conf-fetched)
      (let ((membership (elt lyskom-membership lyskom-last-conf-fetched)))
	(if (lyskom-conf-no-list-member (membership->conf-no membership)
					lyskom-unread-confs)
	    (initiate-get-conf-stat 'main 'lyskom-prefetch-handle-conf
				    (membership->conf-no membership)
				    membership)
	  (++ lyskom-prefetch-confs)
	  (++ lyskom-last-conf-received))))))


(defun lyskom-prefetch-handle-conf (conf-stat membership)
  "Check if there is any unread texts in a conference.
Args: CONF-STAT MEMBERSHIP"
  (++ lyskom-last-conf-received)
  (cond
   ((> (+ (conf-stat->first-local-no conf-stat)
	  (conf-stat->no-of-texts conf-stat)
	  -1)
       (membership->last-text-read membership))
    ;; There are (probably) some unread texts in this conf.
    (initiate-get-map 'prefetch 'lyskom-prefetch-handle-map
		      (conf-stat->conf-no conf-stat)
		      (1+ (membership->last-text-read membership))
		      (+ (conf-stat->no-of-texts conf-stat)
			 (conf-stat->first-local-no conf-stat)
			 (- (membership->last-text-read membership)))
		      membership
		      conf-stat))
   (t 
    (lyskom-prefetch-and-print-prompt))))


(defun lyskom-prefetch-handle-map (map membership conf-stat)
  "Add info about unread texts in a conf to the lyskom-to-do-list.
Args: MAP MEMBERSHIP CONF-STAT.
MAP is the mapping from local to global text-nos for (at least) all
texts after membership->last-text-read. MEMBERSHIP is info about the
user's membership in the conference."
  (let ((unread (lyskom-list-unread map membership)))
    (cond
     (unread
      (read-list-enter-read-info
         (lyskom-create-read-info
	    'CONF
	    conf-stat
	    (membership->priority membership)
	    (lyskom-create-text-list unread))
	 lyskom-to-do-list))))
  (lyskom-prefetch-and-print-prompt))


(defun lyskom-list-unread (map membership)
  "Args: MAP MEMBERSHIP. Return a list of unread texts.
The list consists of text-nos."
  (let ((res nil)
	(last-read (membership->last-text-read membership))
	(read (membership->read-texts membership))
	(first (map->first-local map))
	(i (length (map->text-nos map)))
	(the-map (map->text-nos map)))
    (while (> i 0)
      (-- i)
      (cond
       ((zerop (elt the-map i)))	;Deleted text - do nothing.
       ((<= (+ first i) last-read))	;Already read - do nothing.
       ((lyskom-vmemq  (+ i first) read)) ;Already read - do nothing.
       (t				;Unread - add to list.
	(setq res (cons
		   (elt the-map i)
		   res)))))
    res))


(defun lyskom-conf-fetched-p (conf-no)
  "Return t if CONF-NO has been prefetched."
  (let ((n lyskom-last-conf-received)
	(result nil))
    (while (and (not result)
		(>= n 0))
      (if (= (membership->conf-no (elt lyskom-membership n))
	     conf-no)
	  (setq result t))
      (-- n))
    result))


;;-------------------------------------------------------
;; prefetch text-stats

(defun lyskom-prefetch-text ()
  "Make sure that at least lyskom-prefetch-texts texts are fetched."
  (lyskom-prefetch-from-rlist
   (lyskom-prefetch-from-rlist lyskom-prefetch-texts
			       (read-list->all-entries lyskom-reading-list))
   (read-list->all-entries lyskom-to-do-list)))


(defun lyskom-prefetch-from-rlist (n-texts rlist)
  "Prefetch first N-TEXTS texts from RLIST.
Returns number of texts that could not be fetched.
RLIST is a list of reading-info."
  (cond
   ((< n-texts 1)
    0)
   ((null rlist)
    n-texts)
   (t
    (lyskom-prefetch-from-rlist
     (lyskom-prefetch-from-list
      n-texts
      (cdr (read-info->text-list (car rlist))))
     (cdr rlist)))))


(defun lyskom-prefetch-from-list (n-texts list)
  "Prefetch first N-TEXTS texts from LIST.
Returns number of texts that could not be fetched.
RLIST is a list of text-nos. Texts whose text-no is present on
lyskom-fetched-texts are not fetched."
  (while (and (not (null list)) (> n-texts 0))
    ;; Fetch this text - but only if we are not already fetching it.
    (if (memq (car list) lyskom-fetched-texts)
	nil				;already fetched (but maybe not yet
					;received).
      (initiate-get-text-stat 'background nil (car list))
      (initiate-get-text 'background nil (car list))
      (setq lyskom-fetched-texts (cons (car list) lyskom-fetched-texts)))
    (setq list (cdr list))
    (-- n-texts))
  n-texts)


;;;; ================================================================


(defun lyskom-vmemq (elt vector)
  "Return t if ELT is a member of (present in) VECTOR."
  (let ((found nil)
	(i (length vector)))
    (while (and (> i 0)
		(not found))
      (-- i)
      (if (eq elt (aref vector i))
	  (setq found t)))
    found))

      
(defun lyskom-read-num-range (low high &optional prompt show-range)
  "Read a number from the minibuffer.
Args: LOW HIGH &optional PROMPT SHOW-RANGE.
The read number must be within the range [LOW HIGH].
If SHOW-RANGE is non-nil, the prompt will include the range for information
to the user."
  (let ((number (1- low)))
    (while (or (< number low)
	       (> number high))
      (setq number (lyskom-read-number 
		    (concat (if prompt
				prompt
			      (lyskom-get-string 'give-a-number))
			    (if show-range
				(format "(%d-%d) " low high)
			      "")))))
    number))


(defun lyskom-read-number (&optional prompt default)
  "Read a number from the minibuffer. Optional arguments: PROMPT DEFAULT
If DEFAULT is non-nil, it is written within parenthesis after the prompt.
DEFAULT could also be of the type which (interactive P) generates.
If quit is typed it executes lyskom-end-of-command."
  (let ((numdefault (cond ((null default) nil)
			  ((integerp default) default)
			  ((listp default) (car default))
			  (t nil)))
	(number nil)
	(numstr nil)
	(quit nil))
    (while (not number)
      (setq quit t)
      (setq numstr
	    (unwind-protect
		(prog1
		  (lyskom-read-string
		   (concat (if prompt 
			       prompt
			     (lyskom-get-string 'give-a-number))
			   (if numdefault 
			       (format " (%d) " numdefault))))
		  (setq quit nil))
	      (if quit
		  (lyskom-end-of-command))))
      (cond ((and (string= numstr "") 
		  numdefault)
	     (setq number numdefault))
	    ((string-match "\\`[0-9]+\\'" numstr)
	     (setq number (string-to-int numstr)))
	    (t (beep))))
    number))


(defun lyskom-read-string (prompt &optional initial)
  "Read a string from the minibuffer. Arguments: PROMPT INITIAL
If quit is typed it executes lyskom-end-of-command."
  (let ((quit t))
    (unwind-protect
	(prog1
	    (read-string (if kom-emacs-knows-iso-8859-1
			     prompt
			   (iso-8859-1-to-swascii prompt))
			 initial)
	  (setq quit nil))
      (if quit
	  (lyskom-end-of-command)))))



(defun ja-or-nej-p (prompt &optional initial-input)
  "Same as yes-or-no-p but in swedish."
  (let ((answer "")
	(nagging nil))
    (while (not (or (string-match (lyskom-get-string 'yes-regexp) answer)
		    (string-match (lyskom-get-string 'no-regexp) answer)))
      (if nagging
	  (progn
	    (lyskom-message (lyskom-get-string 'yes-or-no-nag))
	    (sit-for 2)))
      (setq answer (lyskom-read-string (concat prompt (lyskom-get-string 'yes-or-no))
				       initial-input))
      (setq nagging t))
    (not (string-match (lyskom-get-string 'no-regexp) answer))))


(defun j-or-n-p (prompt)
  "Same as y-or-n-p but in swedish."
  (let ((input-char ?a)
	(cursor-in-echo-area t)
	(nagging nil))
    (while (not (char-in-string input-char (lyskom-get-string 'y-or-n-instring)))
	(lyskom-message (concat (if nagging 
			     (lyskom-get-string 'j-or-n-nag)
			   "") 
			 prompt
			 (lyskom-get-string 'j-or-n)))
	(if nagging 
	    (beep))
	(setq input-char (read-char))
	(setq nagging t))
    (char-in-string input-char (lyskom-get-string 'y-instring))))

  
(defun lyskom-membership-< (a b)
  "Retuns t if A has a higher priority than B. A and B are memberships."
  (> (membership->priority a)
     (membership->priority b)))


(defun impl ()
  (error "Not implemented"))


(defun lyskom-call-hook (hook-name &rest args)
  "Calls a hook with parameters.
If HOOK-NAME is bound, it is either a function name or a list of functions
to apply to the ARGS."

  (if (and (boundp hook-name) (eval hook-name))
      (if (listp (eval hook-name))
	  (mapcar '(lambda (fn)
		     (apply fn args))
		  (eval hook-name))
	(apply (eval hook-name) args))))


;;;; ================================================================
;;;; Subroutines - or as near as you can get with this com-packet.
;;;; (I know that this isn't beautiful.)


;;; Print a name.

(defun lyskom-queue-print-name (kom-queue conf-no is-person &optional format)
  "Print the name of CONF-NO. Args: KOM-QUEUE CONF-NO IS-PERSON FORMAT.
IS-PERSON should be non-nil if it is a person.
If FORMAT is non-nil this is the length the printed string should have. If nil
then a newline is printed after the name instead."
  (initiate-get-conf-stat kom-queue 'lyskom-queue-print-name-2
			  conf-no conf-no is-person format))


(defun lyskom-queue-print-name-2 (conf-stat conf-no is-person format)
  (if format
      (cond
       ((null conf-stat)
	(lyskom-insert (lyskom-fix-str format
				       (if (not is-person)
					   (lyskom-format 'conf-does-not-exist
							  conf-no)
					 (lyskom-format 'person-does-not-exist
							conf-no)))))
       (t (lyskom-insert (lyskom-fix-str format
					 (conf-stat->name conf-stat)))))
    (cond
     ((null conf-stat)
      (lyskom-insert (if (not is-person)
			 (lyskom-format 'conf-does-not-exist
					conf-no)
		       (lyskom-format 'person-does-not-exist
				      conf-no)))
      (lyskom-insert "\n"))
     (t (lyskom-print-name conf-stat)))))


;;; The filter.

(defun lyskom-filter (proc output)
  "Receive replies from LysKOM server."
  (let ((inhibit-quit t)		;inhibit-quit is automatically set
					;to t in version 18.57, but not in
					;all older versions of emacs.
	(old-match-data (match-data))
	(old-buffer (current-buffer)))
    (unwind-protect
	(progn

	  (cond
	   (lyskom-debug-communications-to-buffer
	    (set-buffer (get-buffer-create
			 lyskom-debug-communications-to-buffer-buffer))
	    (save-excursion
	      (goto-char (point-max))
	      (insert "-----> "  output " <-----\n"))))

	  (set-buffer (process-buffer proc))
	  (princ output lyskom-unparsed-marker)	;+++lyskom-string-skip-whitespace
	  (setq inhibit-quit nil)	;We are allowed to break here.
	  (setq inhibit-quit t)
	  (cond
	   ((null lyskom-is-parsing)	;Parse one reply at a time.
	    (setq lyskom-is-parsing t)
	    (unwind-protect
		(condition-case
		    error-type
		    (lyskom-parse-unparsed)
		  (lyskom-parse-incomplete)) ;Incomplete answers are normal.
	      (set-buffer (process-buffer proc)) ;In case it was changed by
					;        ;the handler.
	      (setq lyskom-is-parsing nil)))))

      ; Restore selected buffer and match data.

      (store-match-data old-match-data)
      (set-buffer old-buffer))))
      


;;; The sentinel

(defun lyskom-sentinel (proc sentinel)
  "Handles changes in the lyskom-process."
  (set-buffer (process-buffer proc))
  (lyskom-start-of-command (lyskom-get-string 'process-signal) t)
  (lyskom-clear-vars)
  (lyskom-format-insert 'closed-connection sentinel)
  (beep)
  (lyskom-scroll))


;;; ================================================================
;;;         Formatting functions for different data types


(defun lyskom-format-objects (&rest args)
  "Arguments: (&rest ARGS). Format ARGS to correct format to send to server.
Strings are converted to Hollerith strings.
Other objects are converted correctly."
  (apply 'concat (mapcar 'lyskom-format-object args)))


(defun lyskom-format-object (object)
  (concat " "
	  (cond
	   ((stringp object) (lyskom-format-string object))
	   ((integerp object) (int-to-string object))
	   ((listp object)
	    (cond
	     ((eq (car object) 'MISC-LIST)
	      (lyskom-format-misc-list (cdr object)))
	     ((eq (car object) 'CONF-TYPE)
	      (lyskom-format-conf-type object))
	     ((eq (car object) 'LIST)
	      (lyskom-format-simple-list (cdr object)))
	     (t
	      (signal 'lyskom-internal-error
		      (list 'lyskom-format-object
			    ": no support for object "
			    object)))))
	   (t (signal 'lyskom-internal-error
		      (list 'lyskom-format-object
			    ": no support for object "
			    object))))))


(defun lyskom-format-conf-type (conf-type)
  "Format a CONF-TYPE for output to the server."
  (concat
   (lyskom-format-bool (conf-type->rd_prot conf-type))
   (lyskom-format-bool (conf-type->original conf-type))
   (lyskom-format-bool (conf-type->secret conf-type))
   (lyskom-format-bool (conf-type->letterbox conf-type))))


(defun lyskom-format-bool (bool)
  "Format a BOOL for output to the server."
  (if bool 1 0))
      

(defun lyskom-format-misc-list (misc-list)
  "Format a misc-list for output to the server."
  (let ((result (format "%d {" (length misc-list))))
    (while (not (null misc-list))
      (setq result (concat result " "
			    (lyskom-format-misc-item (car misc-list))))
      (setq misc-list (cdr misc-list)))
    (setq result (concat result " }\n"))))


(defun lyskom-format-misc-item (misc-item)
  "Format a misc-item for output to the server."
  (format "%d %d"
	  (cond
	   ((eq (car misc-item) 'recpt) 0)
	   ((eq (car misc-item) 'cc-recpt) 1)
	   ((eq (car misc-item) 'comm-to) 2)
	   ((eq (car misc-item) 'footn-to) 4))
	  (cdr misc-item)))


(defun lyskom-format-simple-list (list)
  "Format some kind of list to send to server."
  (apply 'concat (list (format "%d {" (length list))
		       (apply 'lyskom-format-objects list)
		       " }\n")))

  
(defun lyskom-format-string (string)
  (concat (format "%d"(length string))
	  "H" string))


;;;; ================================================================
;;;;                     Utility routines.


;;; silent-read was
;; Written by Lars Willf|r <willfor@lysator.liu.se>
;; Copyright and copyleft Lars Willf|r.
;; Last modified sep 90.

(defun silent-read (prompt-str)
  "Read a string in the minibuffer without echoing.
One parameter - the prompt string."
  (interactive "sPrompt string: ")
  (lyskom-message prompt-str)
  (let ((input-string "")
	(input-char)
	(cursor-in-echo-area t))
    (while (not (eq (setq input-char (read-char))
		    ?\r))
      (progn
	(lyskom-message prompt-str)
	(setq input-string
	      (cond
	       ((eq input-char ?\C-?)
		(if (equal (length input-string)
			   0)
		    ""
		  (substring input-string 0 -1)))
	       ((eq input-char ?\C-u)
		"")
	       (t
		(concat input-string (char-to-string input-char)))))))
    (message "")
    input-string))


(defun lyskom-array-to-list (array)
  "Return a list whose elements are the elements of ARRAY."
  (append array nil))


(defun reverse-assoc (key cache)
  "Same as assoc, but searches on last element in a list"
  (reverse (assoc key (mapcar (function reverse) cache))))


(run-hooks 'lyskom-init-hook)


(if kom-emacs-knows-iso-8859-1
    nil
  (setq lyskom-commands
	(mapcar 
	 (function (lambda (pair)
		     (list (car pair) (iso-8859-1-to-swascii (car (cdr pair))))))
	 lyskom-commands)))

