;;;;;
;;;;; $Id: edit-text.el,v 44.5 1996-10-24 09:47:45 byers Exp $
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
;;;; File: edit-text.el
;;;;
;;;; This file contains functions which lets the LysKOM user edit
;;;; a text in a window. It also defines a new mode - lyskom-edit-mode.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: edit-text.el,v 44.5 1996-10-24 09:47:45 byers Exp $\n"))


;;;; ================================================================


(defvar lyskom-edit-mode-mode-map nil
  "Mode map for the mode LysKOM edites in.")

(defvar lyskom-edit-mode-name "LysKOM edit"
  "Name of the mode.")

(defvar lyskom-is-dedicated-edit-window nil
  "Status variable for an edit-window.")

(defvar lyskom-edit-handler nil
  "Status variable for an edit-buffer.
See lyskom-edit-handler-data.")

(defvar lyskom-edit-handler-data nil
  "Status variable for an edit-buffer.
See lyskom-edit-handler.")

(defvar lyskom-edit-return-to-configuration nil
  "Status variable for an edit-buffer.")

;;; Error signaled by lyskom-edit-parse-headers
(put 'lyskom-edit-text-abort 'error-conditions
     '(error lyskom-error lyskom-edit-error lyskom-abort-edit))

(put 'lyskom-unknown-header 'error-conditions
     '(error lyskom-error lyskom-edit-error lyskom-unknown-header))

(put 'lyskom-no-subject 'error-conditions
     '(error lyskom-error lyskom-edit-error lyskom-no-subject))

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
  (lyskom-dispatch-edit-text proc misc-list subject body
                             handler data))


(defun lyskom-dispatch-edit-text (proc misc-list subject body
				       &optional handler &rest data)
  "Same as lyskom-edit-text except that it doesn't set lyskom-is-writing."
  (let ((buffer (generate-new-buffer
		 (concat (buffer-name (process-buffer proc)) "-edit")))
	(config (current-window-configuration)))
    (setq lyskom-list-of-edit-buffers (cons buffer 
					    lyskom-list-of-edit-buffers))
    (process-kill-without-query (get-buffer-process (current-buffer)) t)

    (lyskom-associate-buffer buffer)
    (lyskom-display-buffer buffer
                           'kom-write-texts-in-window
                           'kom-dont-restore-window-after-editing)
    

;;;    (cond
;;;     ((and (or (bufferp kom-write-texts-in-window)
;;;	       (stringp kom-write-texts-in-window))
;;;	   (get-buffer-window kom-write-texts-in-window))
;;;      (select-window (get-buffer-window kom-write-texts-in-window))
;;;      (switch-to-buffer buffer))
;;;     ((eq kom-write-texts-in-window 'other)
;;;      (switch-to-buffer-other-window buffer))
;;;     ((and (eq kom-write-texts-in-window 'other-frame)
;;;	   (not (eq (selected-frame) (next-frame))))
;;;      (select-frame (next-frame))
;;;      (switch-to-buffer buffer))
;;;     ((eq kom-write-texts-in-window 'new-frame)
;;;      (switch-to-buffer-other-frame buffer)
;;;      (make-local-variable 'lyskom-is-dedicated-edit-window)
;;;      (setq lyskom-is-dedicated-edit-window t))
;;;     (t
;;;      (switch-to-buffer buffer)))

    (lyskom-protect-environment
     (lyskom-edit-mode))
    (make-local-variable 'lyskom-edit-handler)
    (make-local-variable 'lyskom-edit-handler-data)
    (make-local-variable 'lyskom-edit-return-to-configuration)
    (setq lyskom-edit-handler handler)
    (setq lyskom-edit-handler-data data)
    (setq lyskom-edit-return-to-configuration config)
    (lyskom-edit-insert-miscs misc-list subject body)
    (goto-char (point-min))
    (re-search-forward (regexp-quote (lyskom-get-string
				      'header-subject))
		       (point-max)
		       'end)
    (if (not (looking-at "\\s-*$"))
        (goto-char (point-max)))
    (lyskom-message "%s" (lyskom-get-string 'press-C-c-C-c)))
  (set-buffer lyskom-buffer)
  )


(defun lyskom-edit-insert-miscs (misc-list subject body)
  "Insert MISC-LIST into header of text.
recpt		-> Mottagare: <%d> %s.
cc-recpt	-> Extra kopia: <%d> %s.
comm-to		-> Kommentar till text %d.
footn-to	-> Fotnot till text %d."
  (let ((edit-buffer (current-buffer))
	(where-put-misc (point-min-marker))
	(main-buffer lyskom-buffer))
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
			  (substitute-command-keys
			   (lyskom-get-string 'header-separator))
			  body 
			  (lyskom-get-string 'header-subject))
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
STREAM is the buffer or a marker telling the position.
NUMBER is the number of the person. Used if the conf-stat is nil."
  (save-excursion
    (let ((buf (cond
		((bufferp stream) stream)
		((markerp stream) (marker-buffer stream)))))
      (set-buffer buf)
      (save-excursion
	(if (markerp stream) (goto-char stream))
	(princ (lyskom-format "%#1s <%#2m> %#3M\n" 
                              string
                              (or conf-stat number)
                              (or conf-stat ""))
	       stream)))))


(defun lyskom-edit-get-commented-author (text-stat string stream number)
  (if text-stat
      (lyskom-edit-insert-commented-author 
       (blocking-do 'get-conf-stat (text-stat->author text-stat))
       string stream number)
    (lyskom-edit-insert-commented-author nil string stream number)))


(defun lyskom-edit-insert-commented-author (conf-stat string stream number)
  (princ (lyskom-format 'comment-to-by
			string
                        number
			(if conf-stat
			    (lyskom-format 'by conf-stat)
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
  (setq paragraph-start (concat "^" 
				(regexp-quote 
				 (substitute-command-keys
				  (lyskom-get-string 'header-separator)))
				"$\\|" paragraph-start))
  (setq paragraph-separate (concat "^" 
				   (regexp-quote 
				    (substitute-command-keys
				     (lyskom-get-string 'header-separator)))
				   "$\\|" paragraph-separate))
  (run-hooks 'lyskom-edit-mode-hook))


;;; ================================================================
;;;   Functions bound to keyboard seqences in lyskom-edit-mode
;;;

(defun kom-edit-send-anonymous ()
  "Send the text anonymously to the server."
  (interactive)
  (lyskom-edit-send 'initiate-create-anonymous-text))

(defun kom-edit-send ()
  "Send the text to the server."
  (interactive)
  (lyskom-edit-send 'initiate-create-text))

(defun lyskom-edit-send (send-function)
  "Send the text to the server by calling SEND-FUNCTION."
  (condition-case err
      (if (or (string= mode-name lyskom-edit-mode-name)
	      (j-or-n-p (lyskom-get-string 'already-sent)))
	  (progn 
	    (let ((buffer (current-buffer))
		  headers misc-list subject message)

	      (save-excursion
		(setq headers (lyskom-edit-parse-headers)
		      misc-list (apply 'lyskom-create-misc-list (cdr headers))
		      subject (car headers)))
              ;;
              ;; Check that there is a subject
              ;;

	      (if (string= subject "")
		  (let ((old (point)))
		    (goto-char (point-min))
		    (re-search-forward (lyskom-get-string 'header-subject)
				       nil t)
		    (end-of-line)
		    (if (/= (point) old)
			(signal 'lyskom-no-subject '(enter-subject-idi)))))

              ;;
              ;; Check the recipients
              ;;

              (let ((extra-headers
                     (lyskom-edit-send-check-recipients misc-list
                                                        subject)))
                (if extra-headers
                    (setq misc-list (apply 'lyskom-create-misc-list
                                           (cdr (nconc headers
                                                       extra-headers))))))

              ;;
              ;; Transform the message text
              ;;

	      (setq message
                    (lyskom-send-transform-text (lyskom-edit-extract-text)))

	      (setq mode-name "LysKOM sending")
	      (save-excursion
		(set-buffer lyskom-buffer)
		;; Don't change the prompt if we won't see our own text
		(if kom-created-texts-are-read
		    (setq lyskom-dont-change-prompt t))
		(setq lyskom-is-writing nil)
		(lyskom-tell-internat 'kom-tell-send)
		(funcall send-function
                         'sending
                         'lyskom-create-text-handler
                         (concat subject "\n" message)
                         misc-list
                         buffer)))
            (lyskom-undisplay-buffer)
;;;	    (if kom-dont-restore-window-after-editing
;;;		(bury-buffer)
;;;	      (save-excursion
;;;		(if (and (boundp 'lyskom-is-dedicated-edit-window)
;;;			 lyskom-is-dedicated-edit-window)
;;;		    (condition-case error
;;;			(delete-frame)
;;;		      (error))))
;;;	      (set-window-configuration lyskom-edit-return-to-configuration)
;;;	      (set-buffer (window-buffer (selected-window))))

	    (goto-char (point-max))))
    ;;
    ;; Catch no-subject and other things
    ;;

    (lyskom-abort-edit
     (apply 'lyskom-message (cdr-safe err)))
    (lyskom-no-subject
     (lyskom-beep lyskom-ding-on-no-subject)
     (if (cdr-safe (cdr-safe err))
	 (goto-char (car-safe (cdr-safe (cdr-safe err)))))
     (lyskom-message "%s" (lyskom-get-string (car (cdr err))))
     (condition-case nil
         (let ((text ""))
           (save-excursion
             (set-buffer lyskom-buffer)
             (if (and (string= "kom.lysator.liu.se" lyskom-server-name)
                      (eq lyskom-pers-no 698))
                 (setq text "Ärende, IDI!")))
           (save-excursion (insert text)))
       (error nil)))
    (lyskom-unknown-header
     (lyskom-message "%s" (lyskom-get-string (car (cdr err)))))))


(defun lyskom-edit-send-check-recipients (misc-list subject) 
  "Check that the recipients of this text are OK. Ask the user to
confirm multiple recipients; check that the author of the commented
text is a member of some recipient of this text."
  (let* ((comm-to-list nil)
         (recipient-list nil)
         (author-list nil)
         (author-is-member nil)
         (collector (make-collector))
         (extra-headers nil)
         (me (save-excursion (set-buffer lyskom-buffer)
                             lyskom-pers-no))
         (num-me 0))

    ;;
    ;; List all texts this text is a comment to
    ;;

    (lyskom-traverse misc (cdr misc-list)
      (cond ((eq (car misc) 'comm-to)
             (setq comm-to-list (cons (cdr misc)
                                      comm-to-list)))
            ((or (eq (car misc) 'recpt)
                 (eq (car misc) 'cc-recpt))
             (if (eq (cdr misc) me) (setq num-me (1+ num-me)))
             (setq recipient-list (cons (cdr misc) recipient-list)))))

    ;;
    ;; Confirm multiple recipients
    ;;
    
    (if (and kom-confirm-multiple-recipients
             (not (eq kom-confirm-multiple-recipients 'before))
             (> (- (length recipient-list) num-me) 1))
        (save-excursion
          (goto-char (point-min))
          (if (not 
               (lyskom-j-or-n-p
                (lyskom-format 'comment-all-relevant-p) t))
              (signal 'lyskom-edit-text-abort 
                      (list "%s" 
                            (lyskom-get-string 
                             'please-edit-recipients))))))

    (if (and kom-check-commented-author-membership
             (assq 'comm-to (cdr misc-list)))
        (progn
          (lyskom-message (lyskom-get-string 'checking-rcpt))

          ;;
          ;; For each commented text, get the author
          ;;
        
          (setq author-list (mapcar 
                             (function (lambda (x)
                                         (text-stat->author
                                          (blocking-do 'get-text-stat x))))
                             comm-to-list))

          ;;
          ;; For each author, see if the author is a direct recipient
          ;; of the text. If so, there is no point in continuing.
          ;; (People can unsubscribe from their mailboxes, but if they
          ;; do, this code won't help anyway.)
          ;;

          (lyskom-traverse misc (cdr misc-list)
            (cond ((eq (car misc) 'comm-to)
                   (setq comm-to-list (cons (cdr misc)
                                            comm-to-list)))
                  ((or (eq (car misc) 'recpt)
                       (eq (car misc) 'cc-recpt))
                   (if (or (memq (cdr misc) author-list)
                           (eq (cdr misc) me))
                       (setq author-list (delq (cdr misc) author-list))))))

          ;;
          ;; For each author, get his or her memberships in all
          ;; recipient conferences.
          ;;

          (save-excursion
            (set-buffer lyskom-buffer)
            (mapcar (function
                     (lambda (author-number)
                       (lyskom-collect 'sending)
                       (mapcar
                        (function
                         (lambda (conference-number)
                           (initiate-query-read-texts 
                            'sending
                            nil
                            author-number conference-number)))
                        recipient-list)

                       (lyskom-list-use 'sending
                                        'collector-push
                                        collector)
                       (lyskom-wait-queue 'sending)
                       (setq author-is-member (collector->value collector))

                       (if (and (null author-is-member)
				(not (zerop author-number))
                                (lyskom-j-or-n-p
                                 (let ((kom-deferred-printing nil))
                                   (lyskom-format
                                    'add-recipient-p
                                    author-number)) t))
                           (setq extra-headers
                                 (nconc (list 'recpt 
                                              author-number)
                                        extra-headers)))))
                    author-list))))

    extra-headers))
    


(defun lyskom-send-transform-text (message)
  (if lyskom-format-experimental
      (condition-case nil
          (let ((buf (generate-new-buffer "lyskom-enriched")))
            (unwind-protect
                (save-excursion
                  (set-buffer buf)
                  (insert message)
                  (goto-char (point-min))
                  (format-encode-buffer 'text/enriched)
                  (goto-char (point-min))
                  (search-forward "\n\n")
                  (if (not (string= (buffer-substring (point)
						      (point-max)) message))
                      (concat "enriched:\n" (buffer-string))
                    message))
              (kill-buffer buf)))
        (error message))
    message))


(defun kom-edit-quit ()
  "Kill the text (if any) written so far and continue reading."
  (interactive)
  (let ((edit-buffer (current-buffer)))
    (if kom-dont-restore-window-after-editing
	(bury-buffer)
      ;; Select the old configuration.
      (save-excursion
	(if (and (boundp 'lyskom-is-dedicated-edit-window)
		 lyskom-is-dedicated-edit-window)
	    (condition-case nil
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
  (setq unread-command-events (cons last-command-event unread-command-events))
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
    (set-buffer lyskom-buffer)
    (initiate-get-text 'edit 'lyskom-edit-insert-commented no buffer)
    (set-buffer buffer)
    (sit-for 0)))
    

(defun lyskom-edit-get-commented (thendo)
  "Get the commented text and then do THENDO with it."
  (let ((p (point)))
    (save-excursion
      (let* ((buffer (current-buffer))
             (headers (condition-case nil
                          (cdr (lyskom-edit-parse-headers))
                        (lyskom-edit-error nil))) ; Ignore these errors
             (no nil))
        (while headers
          (if (or (eq (car headers) 'comm-to)
                  (eq (car headers) 'footn-to))
              (setq no (car (cdr headers))
                    headers nil)
            (setq headers (cdr (cdr headers)))))
        (cond
         (no
          (goto-char p)
          (set-buffer lyskom-buffer)
          (initiate-get-text 'edit thendo no buffer)
          (set-buffer buffer))
         (t
          (lyskom-message "%s" (lyskom-get-string 'no-such-text-m))))))
    (sit-for 0)))


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
  (let ((edit-buffer (current-buffer))
	(insert-at (point-min-marker))
	(conf-stat (lyskom-read-conf-stat prompt '(all) nil "" t)))
    (lyskom-save-excursion
     ;;(save-excursion
     (set-buffer lyskom-buffer)
     ;; +++ The information about msg-of-day might be old. We should
     ;; make sure it is up-to-date.
     (let ((text-no (conf-stat->msg-of-day conf-stat)))
       (if (zerop text-no)
	   (lyskom-edit-insert-misc-conf conf-stat string insert-at nil)
	 (let ((text (blocking-do 'get-text text-no)))
	   (if (and text (get-buffer-window edit-buffer))
	       (let ((win-config (current-window-configuration)))
		 ;;(set-buffer buffer)
		 (with-output-to-temp-buffer "*Motd*"
		   (princ (lyskom-format 'conf-has-motd-no
					 (text->text-no text)
					 (text->text-mass text))))
		 (and (j-or-n-p (lyskom-get-string 'still-want-to-add))
		      (lyskom-edit-insert-misc-conf conf-stat string
						    insert-at nil))
		 (set-window-configuration win-config))
	     (lyskom-edit-insert-misc-conf conf-stat string
					   insert-at nil))))))))



;;; ================================================================
;;;   Help functions for the functions bound to keyboard sequences 
;;;                       in lyskom-edit-mode.


(defun lyskom-looking-at-header (header match-number)
  "Check if point is at the beginning of a header of type HEADER.
Return the corresponding number (conf no etc.) if MATCH-NUMBER is
non-nil. If MATCH-NUMBER is 'angled, only match a number inside <>."
  (if (looking-at
       (concat (lyskom-get-string header)
	       (cond ((eq match-number 'angled)
		      "[^0-9]*<\\([0-9]+\\)>")
		     (match-number
		      "[^0-9]*\\([0-9]+\\)")
		     (nil
		      ""))))
      (if match-number
	  (string-to-int (buffer-substring (match-beginning 1)
					   (match-end 1)))
	t)
    nil))

(defun lyskom-edit-parse-headers ()
  "Parse the headers of an article.
They are returned as a list where the first element is the subject,
and the rest is a list (HEADER DATA HEADER DATA ...), where HEADER is
either 'recpt, 'cc-recpt, 'comm-to or 'footn-to. This is to make it
easy to use the result in a call to `lyskom-create-misc-list'."
  (goto-char (point-min))
  (let ((result (cons "" nil)))		; The car will be replaced by
					; the real subject
    (save-restriction
      ;; Narrow to headers
      (search-forward (substitute-command-keys
		       (lyskom-get-string 'header-separator)))
      (beginning-of-line)
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(let ((case-fold-search t)
	      n)
	  (cond
	   ((setq n (lyskom-looking-at-header 'recipient-prefix 'angled))
	    (nconc result (list 'recpt n)))
	   ((setq n (lyskom-looking-at-header 'carbon-copy-prefix 'angled))
	    (nconc result (list 'cc-recpt n)))
	   ((setq n (lyskom-looking-at-header 'comment-prefix t))
	    (nconc result (list 'comm-to n)))
	   ((setq n (lyskom-looking-at-header 'footnote-prefix t))
	    (nconc result (list 'footn-to n)))
	   ((lyskom-looking-at-header 'header-subject nil)
	    (setcar result (lyskom-edit-extract-subject)))
	   (t
	    (signal 'lyskom-unknown-header (list 'unknown-header (point))))))
	(forward-line 1)))
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
		  (substitute-command-keys
		   (lyskom-get-string 'header-separator))
		  nil (point-max))
		 (search-forward 
		  (substitute-command-keys
		   (lyskom-get-string 'header-separator)) 
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
  (lyskom-tell-internat 'kom-tell-silence)
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
      (lyskom-run 'background 'lyskom-set-mode-line))
     (t					; Probably not necessary
      (setq lyskom-dont-change-prompt nil)))
    
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

      (set-buffer lyskom-buffer)
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
	   (princ (text->text-mass text))))))


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
          (while (<= pb (point))
            (beginning-of-line)
            (insert (or kom-cite-string 62))
            (forward-line -1)
            )))
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
	  (if lyskom-list-of-edit-buffers
	      (process-kill-without-query proc t)
	    (process-kill-without-query proc nil)))
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
