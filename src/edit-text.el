;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: edit-text.el,v 44.134 2010-05-13 18:14:10 byers Exp $
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
;;;; File: edit-text.el
;;;;
;;;; This file contains functions which lets the LysKOM user edit
;;;; a text in a window. It also defines a new mode - lyskom-edit-mode.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: edit-text.el,v 44.134 2010-05-13 18:14:10 byers Exp $\n"))


;;;; ================================================================

;;; Set variables to make lyskom-edit-mode a minor mode. This
;;; simplifies some stuff a lot

(defvar lyskom-edit-mode nil
  "Mode variable for lyskom-edit-mode")
(make-variable-buffer-local 'lyskom-edit-mode)

(defvar lyskom-edit-sending-mode nil
  "Mode variable for lyskom-edit-sending-mode")
(make-variable-buffer-local 'lyskom-edit-sending-mode)

(defvar lyskom-edit-sent-mode nil
  "Mode variable for lyskom-edit-sent-mode")
(make-variable-buffer-local 'lyskom-edit-sent-mode)

(put 'lyskom-edit-mode 'permanent-local t)

(or (assq 'lyskom-edit-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(lyskom-edit-mode " LysKOM Edit")
                                 minor-mode-alist)))

(or (assq 'lyskom-edit-sending-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(lyskom-edit-sending-mode " LysKOM Sending")
                                 minor-mode-alist)))

(or (assq 'lyskom-edit-sent-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(lyskom-edit-sent-mode " LysKOM Sent")
                                 minor-mode-alist)))

(or (assq 'lyskom-edit-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'lyskom-edit-mode lyskom-edit-mode-map)
		minor-mode-map-alist)))

(or (assq 'lyskom-edit-sending-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'lyskom-edit-sending-mode lyskom-edit-mode-map)
		minor-mode-map-alist)))

(or (assq 'lyskom-edit-sent-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'lyskom-edit-sending-mode lyskom-edit-mode-map)
		minor-mode-map-alist)))



(defvar lyskom-edit-mode-name "LysKOM edit"
  "Name of the mode.")

(defvar lyskom-edit-text-sent nil
  "Non-nil when a text has been sent")

(defvar lyskom-is-dedicated-edit-window nil
  "Status variable for an edit-window.")

(defvar lyskom-edit-handler nil
  "Status variable for an edit-buffer.
See lyskom-edit-handler-data and lyskom-edit-handler-buffer.")

(defvar lyskom-edit-handler-data nil
  "Status variable for an edit-buffer.
See lyskom-edit-handler and lyskom-edit-handler-buffer.")

(defvar lyskom-edit-handler-buffer nil
  "Buffer to run the edit-handler in.
See lyskom-edit-handler and lyskom-edit-handler-data.")

(defvar lyskom-edit-return-to-configuration nil
  "Status variable for an edit-buffer.")

(def-komtype lyskom-text-headers
  ((subject :read-only t)
   (misc-info :read-only t)
   (aux-items :read-only t)))
   

;;; Error signaled by lyskom-edit-parse-headers
(put 'lyskom-edit-text-abort 'error-conditions
     '(error lyskom-error lyskom-edit-error lyskom-edit-text-abort))

(put 'lyskom-unknown-header 'error-conditions
     '(error lyskom-error lyskom-edit-error lyskom-unknown-header))

(put 'lyskom-no-subject 'error-conditions
     '(error lyskom-error lyskom-edit-error lyskom-no-subject))

(put 'lyskom-edit-error 'error-conditions
     '(error lyskom-error lyskom-edit-error))

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
  (let* ((saved-buffer lyskom-buffer)
         (buffer (lyskom-get-buffer-create 'write-texts
                                           (concat 
                                            (buffer-name (process-buffer proc))
                                            "-edit")))
         (config (current-window-configuration)))

    (lyskom-display-buffer buffer)
    (text-mode)
    (lyskom-ignore-errors
      (run-hooks 'lyskom-edit-mode-mode-hook))
    (lyskom-edit-mode)
    (make-local-variable 'lyskom-edit-handler)
    (make-local-variable 'lyskom-edit-handler-data)
    (make-local-variable 'lyskom-edit-handler-buffer)
    (make-local-variable 'lyskom-edit-return-to-configuration)
    (setq lyskom-edit-handler handler)
    (setq lyskom-edit-handler-data data)
    (setq lyskom-edit-handler-buffer saved-buffer)
    (setq lyskom-edit-return-to-configuration config)
    (buffer-disable-undo)
    (lyskom-edit-insert-miscs misc-list subject body)
    (buffer-enable-undo)
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


(defun lyskom-edit-find-separator (&optional move-point)
  "Return the end position of the separator line.
If MOVE-POINT is non-nil, move point to the start of the separator."
  (prog1
      (save-excursion
        (goto-char (point-min))
        (and (re-search-forward
              (concat "^"
                      (regexp-quote
                       (substitute-command-keys
                        (lyskom-get-string 'header-separator)))
                      "$")
              nil t)))
    (when (and move-point (match-beginning 0))
      (goto-char (match-beginning 0)))))


(defun lyskom-edit-insert-miscs (misc-list subject body &optional aux-list)
  "Insert MISC-LIST into header of text.
RECPT		-> Mottagare: <%d> %s.
CC-RECPT	-> Extra kopia: <%d> %s.
BCC-RECPT       -> Blind kopia: <%d> %s.
COMM-TO		-> Kommentar till text %d.
FOOTN-TO	-> Fotnot till text %d.
nil             -> Ingenting."
  (let ((edit-buffer (current-buffer))
	(where-put-misc (point-min-marker))
	(main-buffer lyskom-buffer))
    (set-buffer main-buffer)
    (setq misc-list (cdr misc-list))
    (while (not (null misc-list))
      (let ((key (car (car misc-list)))
	    (data (cdr (car misc-list))))
	(cond
	 ((eq key 'RECPT)
	  (lyskom-edit-insert-misc-conf (blocking-do 'get-conf-stat data)
					(lyskom-get-string 'recipient)
					where-put-misc data))
	 ((eq key 'CC-RECPT)
	  (lyskom-edit-insert-misc-conf (blocking-do 'get-conf-stat data)
					(lyskom-get-string 'carbon-copy)
					where-put-misc data))
	 ((eq key 'BCC-RECPT)
	  (lyskom-edit-insert-misc-conf (blocking-do 'get-conf-stat data)
					(lyskom-get-string 'blank-carbon-copy)
					where-put-misc data))
	 ((eq key 'COMM-TO)
	  (lyskom-edit-get-commented-author (blocking-do 'get-text-stat data)
					    (lyskom-get-string 'comment)
					    where-put-misc data))
	 ((eq key 'FOOTN-TO)
	  (lyskom-edit-get-commented-author (blocking-do 'get-text-stat data)
					    (lyskom-get-string 'footnote)
					    where-put-misc data)))
	(setq misc-list (cdr misc-list))))
    (mapc (lambda (item)
            (let ((data (lyskom-aux-item-call
                         item '(edit-insert print)
                         item lyskom-pers-no)))
              (when data
                (lyskom-princ
                 (lyskom-format "%#1@%[%#3s%] %#2s\n" 
                                (lyskom-default-button 'aux-edit-menu 
                                                       (cons edit-buffer
                                                             (copy-marker where-put-misc)))
                                data
                                (lyskom-get-string 'aux-item-prefix))
                 where-put-misc))))
          aux-list)
    (unless kom-edit-hide-add-button
      (lyskom-princ (lyskom-format "%[%#1@%#2s%]\n"
                                   (lyskom-default-button 'add-recipient-or-xref
                                                          edit-buffer)
                                   (lyskom-get-string 'add-recpt-button-text))
                    where-put-misc))
    (lyskom-princ (lyskom-format 'text-mass subject 
				 (substitute-command-keys
				  (lyskom-get-string 'header-separator))
				 body 
				 (lyskom-get-string 'header-subject)
                                 '(read-only t)
                                 '(read-only t front-sticky nil start-open t rear-nonsticky nil end-open nil)
                                 '(read-only t rear-nonsticky t end-open t front-sticky t start-open nil)
                                 )
		  where-put-misc)
    (set-buffer edit-buffer)
    (goto-char where-put-misc)
    (set-marker where-put-misc nil)))


(defun lyskom-edit-insert-misc-conf (conf-stat string stream number)
  "Insert Recipient:, Carbon copy: or Blind Carbon copy: in
edit-buffer.
CONF-STAT is the conf-stat of the conference that is about to be put in,
STRING is the string that is inserted.
STREAM is the buffer or a marker telling the position.
NUMBER is the number of the person. Used if the conf-stat is nil."
  (lyskom-princ
   (lyskom-format "%[%#1@%#2s%]: <%#3m> %#4M\n" 
                  (lyskom-default-button
                   'recpt-type
                   (list (or (conf-stat->conf-no conf-stat) number)
                         (marker-buffer stream))
                   (list (lyskom-get-string 'recpt-type-popup-title)
                         string))
                  string
                  (or conf-stat number)
                  (or conf-stat ""))
   stream))

(defun lyskom-edit-get-commented-author (text-stat string stream number)
  (if text-stat
      (let ((mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 17)))
            (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 16))))
        (lyskom-edit-insert-commented-author 
         (if (or mx-from mx-author)
             (lyskom-format-mx-author mx-from mx-author)
           (blocking-do 'get-conf-stat (text-stat->author text-stat)))
         string stream number))
    (lyskom-edit-insert-commented-author nil string stream number)))


(defun lyskom-edit-insert-commented-author (conf-stat string stream number)
  (lyskom-princ (lyskom-format 'comment-to-by
                               string
                               number
                               (if conf-stat
                                   (lyskom-format 'by conf-stat)
                                 ""))
                stream))




(defun lyskom-create-misc-list (&rest misc-pairs)
  "Create a misc-list.
Note that a misc-list is very different from a misc-info-list.
A misc-list is used when creating a new text, and sent to the server.
A misc-info-list is what is received from the server. Although the server
has the same format for the two things, this client uses two quite
different formats.

The arguments to this function is any number of pairs of data. The first
item in each pair should be one of RECPT, CC-RECPT, COMM-TO or FOOTN-TO.
The second item should be the corresponding conf- or text-no.

The result is a list of dotted pairs:
	('RECPT . conf-no)
	('CC-RECPT . conf-no)
	('BCC-RECPT . conf-no)
	('COMM-TO . text-no)
	('FOOTN-TO . text-no).
First element is a type-tag."
   (let ((result (cons 'MISC-LIST nil)))
    (while (not (null misc-pairs))
      (setq result (cons (cons (car misc-pairs)
				(car (cdr misc-pairs)))
			  result))
      (setq misc-pairs (cdr (cdr misc-pairs))))
    (nreverse result)))


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


(defun lyskom-edit-mode (&optional arg)
  "\\<lyskom-edit-mode-map>Mode for editing texts for LysKOM.
Commands:
\\[kom-edit-send]   sends the text when you are ready. The buffer will be
	  deleted if (and only if) the server accepts the text.
\\[kom-edit-quit]   aborts the editing. You will get back to the LysKOM buffer.

\\[kom-edit-show-commented]   shows the commented text in a temporary buffer.

\\[kom-edit-add-recipient]   asks for another recipient and adds him to the header.
\\[kom-edit-add-copy]   as \\[kom-edit-add-recipient] but adds him as copy-recipient.
\\[kom-edit-add-cross-reference]   asks for what to refer to and adds a cross reference to it.

\\[kom-edit-insert-commented]   inserts the commented or footnoted text.
\\[kom-edit-insert-text]   inserts the shown text, you tell the number.
\\[kom-edit-insert-link]   asks for what to link to and inserts a link to it.

Even though this is a minor mode, it's not intended to be turned on and off,
so it's not as clean as it ought to be."
  (interactive "P")
  (setq lyskom-edit-mode
        (if (null arg)
            (not lyskom-edit-mode)
          (> (prefix-numeric-value arg) 0)))

  (lyskom-set-menus 'lyskom-edit-mode lyskom-edit-mode-map)

  (when lyskom-edit-mode
    (lyskom-edit-sending-mode 0)
    (lyskom-edit-sent-mode 0)
    (auto-fill-mode 1)
    (auto-save-mode 1)
    (when (not (local-variable-p 'lyskom-edit-text-sent (current-buffer)))
      (make-local-variable 'lyskom-edit-text-sent)
      (setq lyskom-edit-text-sent nil))
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
    (make-local-variable 'comment-start)
    (make-local-variable 'comment-end)
    (setq comment-start kom-cite-string
	  comment-end "")
    (run-hooks 'lyskom-edit-mode-hook)))

(defun lyskom-edit-sending-mode (arg)
  (interactive "P")
  (setq lyskom-edit-sending-mode 
        (if (null arg)
            (not lyskom-edit-sending-mode)
          (> (prefix-numeric-value arg) 0)))

  (when lyskom-edit-sending-mode
    (lyskom-edit-mode 0)
    (lyskom-edit-sent-mode 0)))

(defun lyskom-edit-sent-mode (arg)
  (interactive "P")
  (setq lyskom-edit-sent-mode 
        (if (null arg)
            (not lyskom-edit-sent-mode)
          (> (prefix-numeric-value arg) 0)))

  (when lyskom-edit-sent-mode
    (lyskom-edit-sending-mode 0)
    (lyskom-edit-mode 0)))


;;; ================================================================
;;;   Functions bound to keyboard seqences in lyskom-edit-mode
;;;

(defvar lyskom-edit-aux-list)
(defvar lyskom-edit-misc-list)
(defvar lyskom-edit-subject)
(defvar lyskom-edit-text)

(defun kom-edit-send-anonymous ()
  "Send the text anonymously to the server. Be aware that although the text 
will be truly anonymous, it is easy to slip up in such a way that the author
is evident anyway."
  (interactive)
  (lyskom-edit-send 'initiate-create-anonymous-text t))

(defun kom-edit-send ()
  "Send the text to the server. This command will attempt to send the text
to the server. If something goes wrong, a prompt will be shown allowing you
to edit the message and try to send it again."
  (interactive)
   (if (and (lyskom-default-value 'lyskom-is-anonymous)
            (lyskom-j-or-n-p 'do-send-anonymous))
       (lyskom-edit-send 'initiate-create-anonymous-text t)
     (lyskom-edit-send 'initiate-create-text nil)))

(defun lyskom-edit-send (send-function &optional is-anonymous)
  "Send the text to the server by calling SEND-FUNCTION.
If optional IS-ANONYMOUS is non-nil, assume that the text is being submitted
anonymously and take actions to avoid revealing the sender.

This runs `kom-send-text-hook' and (for backwards compatibility)
`lyskom-send-text-hook'."
  (condition-case err
      (if (or (not lyskom-edit-text-sent)
	      (j-or-n-p (lyskom-get-string 'already-sent)))
	  (progn 
	    (let ((buffer (current-buffer))
		  (handler lyskom-edit-handler)
		  (handler-data lyskom-edit-handler-data)
		  (handler-buffer lyskom-edit-handler-buffer)
		  (headers nil)
                  (misc-list nil)
                  (subject nil)
		  (subject-start-pos nil)
		  (body-start-pos nil)
                  (message nil)
                  (aux-list nil))

              ;;
              ;; Run user hooks
              ;; ####: ++++: FIXME: We should quit more graciously.

              (if (not (run-hook-with-args-until-failure 'kom-send-text-hook))
                  (signal 'lyskom-edit-text-abort nil))


	      (save-excursion
		(setq headers (lyskom-edit-parse-headers)
		      misc-list (apply 'lyskom-create-misc-list 
                                       (lyskom-text-headers->misc-info headers))
                      aux-list (lyskom-text-headers->aux-items headers)
		      subject (lyskom-text-headers->subject headers)))

              ;;
              ;; Check that there is a subject
              ;;

	      (if (or (null subject)
                      (string= subject ""))
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
                    (setq misc-list
			  (apply 'lyskom-create-misc-list
				 (nconc (lyskom-text-headers->misc-info headers)
					extra-headers)))))

              ;;
              ;; Check that we don't add ourselves to an anon text
              ;;

              (when (and is-anonymous
                         (rassq lyskom-pers-no (cdr misc-list))
                         (lyskom-j-or-n-p 
                          (lyskom-get-string 'remove-self-sending-anonymous)))
                (rplacd misc-list (delq (rassq lyskom-pers-no (cdr misc-list))
                                        (cdr misc-list))))


              ;;
              ;; Transform the message text
              ;;

              (let ((lyskom-edit-aux-list aux-list)
                    (lyskom-edit-misc-list misc-list)
                    (lyskom-edit-subject subject)
                    (lyskom-edit-text (lyskom-edit-extract-text)))
                (run-hook-with-args-until-success
                 'lyskom-send-text-transform-hook)
                (setq aux-list lyskom-edit-aux-list)
                (setq misc-list lyskom-edit-misc-list)
                (setq subject lyskom-edit-subject)
                (setq message lyskom-edit-text))

	      (save-excursion
                (let* ((full-message
                        (cond ((and lyskom-allow-missing-subject
                                    (or (null subject)
                                        (string= subject ""))
                                    (not (string-match ".*\n" message)))
                               message)
                              (t (concat (or subject "") "\n" message))))
                       (content-type (lyskom-get-aux-item aux-list 1))
                       (charset (and content-type
                                     (lyskom-mime-content-type-get
                                      (lyskom-mime-decode-content-type
                                           (aux-item->data (car content-type)))
                                      'charset)))
		       (mime-charset (with-temp-buffer
				       (insert full-message)
				       (lyskom-mime-charset-for-text (point-min) (point-max)))))

                  ;; If the charset isn't already set, encode the string

                  (if (and mime-charset (null charset))
                      (setq full-message
                            (lyskom-mime-encode-string full-message mime-charset))
		    (lyskom-error (lyskom-get-string 'too-many-languages-error)))

                  ;; Add the charset data to the content type

                  (cond ((> (length content-type) 1)
                         (lyskom-error (lyskom-get-string 'too-many-content-types)))
                        ((null content-type)
                         (setq aux-list
                               (cons (lyskom-create-aux-item 0 1 nil nil
                                                             (lyskom-create-aux-item-flags nil nil nil nil nil nil nil nil)
                                                             0
                                                             (format "text/x-kom-basic;charset=%S" mime-charset))
                                     aux-list)))
                        ((null charset)
                         (set-aux-item->data (car content-type)
                                             (format "%s;charset=%S"
                                                     (aux-item->data (car content-type))
                                                     mime-charset))))

                  ;; Send the text

                  (lyskom-edit-sending-mode 1)
                  (set-buffer lyskom-buffer)
                  ;; Don't change the prompt if we won't see our own text
                  (if (and kom-created-texts-are-read
                           (not is-anonymous))
                      (setq lyskom-dont-change-prompt t))
                  (setq lyskom-is-writing nil)
                  (lyskom-tell-internat 'kom-tell-send)
                  (run-hook-with-args 'kom-create-text-hook
                              full-message
                              misc-list
                              (if (not is-anonymous)
                                  (cons (lyskom-create-aux-item
                                         0 15 0 0
                                         (lyskom-create-aux-item-flags
                                          nil nil nil nil nil nil nil nil)
                                         0 (concat "lyskom.el "
                                                   lyskom-clientversion))
                                        aux-list)
                                aux-list)
                              buffer
                              is-anonymous)

                  (funcall send-function
                           'sending
                           'lyskom-create-text-handler
                           full-message
                           misc-list
                           (if (not is-anonymous)
                               (cons (lyskom-create-aux-item
                                      0 15 0 0
                                      (lyskom-create-aux-item-flags
                                       nil nil nil nil nil nil nil nil)
                                      0 (concat "lyskom.el "
                                                lyskom-clientversion))
                                     aux-list)
                             aux-list)
                           buffer
                           is-anonymous
			   handler
			   handler-data
                           handler-buffer))))
            (lyskom-undisplay-buffer)
	    (goto-char (point-max))))
    ;;
    ;; Catch no-subject and other things
    ;;

    (lyskom-edit-text-abort
     (and (cdr err) (apply 'lyskom-message (cdr err))))
    (lyskom-no-subject
     (lyskom-beep kom-ding-on-no-subject)
     (if (cdr-safe (cdr-safe err))
	 (goto-char (car-safe (cdr-safe (cdr-safe err)))))
     (lyskom-message "%s" (lyskom-get-string (car (cdr err)))))
    (lyskom-unknown-header
     (lyskom-message "%s" (lyskom-get-string (car (cdr err)))))))

(eval-when-compile (defvar ispell-dictionary nil))
(eval-when-compile (defvar ispell-message-text-end nil))
(eval-when-compile (defvar ispell-message-start-skip nil))
(eval-when-compile (defvar ispell-message-end-skip nil))


;;USER-HOOK: lyskom-ispell-text
(defun lyskom-ispell-text ()
  "Check spelling of the text body.
Put this in kom-send-text-hook"
  (kom-edit-ispell-message)
  t)


(eval-when-compile
  (defvar ispell-dictionary nil)
  (defvar ispell-message-text-end nil)
  (defvar ispell-message-start-skip nil)
  (defvar ispell-message-end-skip nil))

(defalias 'kom-ispell-message 'kom-edit-ispell-message)
(defun kom-edit-ispell-message ()
  "Check spelling of the text. Spelling is checked using ispell
and the dictionary indicated by `kom-ispell-dictionary'. If you
want to check the spelling of every message before sending it,
read the documentation for `lyskom-ispell-text' and 
`kom-send-text-hook'"
  (interactive)
  (require 'ispell)
  (let ((ispell-dictionary (or kom-ispell-dictionary ispell-dictionary))
        (kill-ispell (or (not (boundp 'ispell-dictionary))
                         (not (lyskom-string= kom-ispell-dictionary
                                       ispell-dictionary))))
        (result nil))
    (when kill-ispell (ispell-kill-ispell t))

    ;; Checking code

    (save-excursion
      (goto-char (point-min))
      (let* ((internal-messagep (lyskom-edit-find-separator t))
             (limit 
              (copy-marker
               (cond ((not ispell-message-text-end) (point-max))
                     ((char-or-string-p ispell-message-text-end)
                      (if (re-search-forward ispell-message-text-end nil t)
                          (match-beginning 0)
                        (point-max)))
                     (t (min (point-max) (funcall ispell-message-text-end))))))
             (cite-regexp 
              (regexp-quote
               (lyskom-default-value 'kom-cite-string)))
             (cite-regexp-start (concat "^[ \t]*$\\|" cite-regexp))
             (cite-regexp-end (concat "^\\(" cite-regexp "\\)"))
             (old-case-fold-search case-fold-search)
             (case-fold-search t)
             (ispell-checking-message t)
             (subject-string 
              (concat "^" (regexp-quote (lyskom-get-string 'subject)))))
        (lyskom-ignore ispell-checking-message)
        (goto-char (point-min))
        (while (if internal-messagep
                   (< (point) internal-messagep)
                 (not (eobp)))
          (if (lyskom-looking-at subject-string)
              (progn (goto-char (match-end 0))
                     (let ((case-fold-search old-case-fold-search))
                       (ispell-region (point)
                                      (progn
                                        (end-of-line)
                                        (point)))))
            (forward-line 1)))

        (while (< (point) limit)
          (while (and (lyskom-looking-at cite-regexp-start)
                      (< (point) limit)
                      (zerop (forward-line 1))))

          (if (< (point) limit)
              (let* ((start (point))
                     (end-c (and (re-search-forward cite-regexp-end limit 'end)
                                 (match-beginning 0)))
                     (end-fwd (and (goto-char start)
                                   (boundp 'ispell-message-start-skip)
                                   (stringp ispell-message-start-skip)
                                   (re-search-forward ispell-message-start-skip
                                                      limit 'end)))
                     (end (or (and end-c end-fwd (min end-c end-fwd))
                              end-c 
                              end-fwd
                              (marker-position limit))))
                (goto-char start)
                (setq result (ispell-region start end))
                  (if (and end-fwd (= end end-fwd))
                      (progn (goto-char end)
                             (re-search-forward ispell-message-end-skip 
                                                limit 'end))
                    (goto-char end)))))
          (set-marker limit nil)
          result))

    (when kill-ispell (ispell-kill-ispell t))
    result))

(defun lyskom-is-permitted-author (conf-stat)
  (and conf-stat
       (or (eq 0 (conf-stat->permitted-submitters conf-stat))
           (lyskom-is-supervisor (conf-stat->conf-no conf-stat)
                                 lyskom-pers-no))))


(defun lyskom-edit-send-check-recipients (misc-list subject) 
  "Check that the recipients of this text are OK. Ask the user to
confirm multiple recipients; check that the author of the commented
text is a member of some recipient of this text.

Cannot be called from a callback."
  (let* ((comm-to-list nil)
         (recipient-list nil)
         (text-stat nil)
         (collector (make-collector))
         (extra-headers nil)
         (buffer (current-buffer))
         (me (lyskom-with-lyskom-buffer lyskom-pers-no))
         (num-me 0)
         (num-real-recpt 0))
    (lyskom-ignore text-stat)       ; Have no idea if its ever used...

    ;;
    ;; List all texts this text is a comment to
    ;; List all recipients of the text
    ;;

    (lyskom-traverse misc (cdr misc-list)
      (cond ((eq (car misc) 'COMM-TO)
             (setq comm-to-list (cons (cdr misc)
                                      comm-to-list)))
            ((memq (car misc) lyskom-recpt-types-list)
             (when (eq (car misc) 'RECPT)
               (setq num-real-recpt (1+ num-real-recpt))
               (when (eq (cdr misc) me) (setq num-me (1+ num-me))))
             (setq recipient-list (cons (cdr misc) recipient-list)))))

    ;;
    ;; Check that there are recipients
    ;;

    (when (null recipient-list)
      (lyskom-error "%s" (lyskom-format 'no-recipients)))

    ;;
    ;; Check for duplicate recipients
    ;;

    (let ((tmp recipient-list))
      (while tmp
        (when (memq (car tmp) (cdr tmp))
          (lyskom-error "%s" (lyskom-format 'duplicate-recipients (car tmp))))
        (setq tmp (cdr tmp))))

    ;;
    ;; Check for new comments
    ;;

    (when (lyskom-with-lyskom-buffer
            (cond ((null kom-check-for-new-comments) nil)
                  ((functionp kom-check-for-new-comments)
                   (funcall kom-check-for-new-comments
                            buffer misc-list subject))
                  (t t)))
      (lyskom-message "%s" (lyskom-format 'checking-comments))
      (lyskom-with-lyskom-buffer
        (set-collector->value collector nil)

        (mapc (lambda (text-stat)
                (cache-del-text-stat text-stat)
                (initiate-get-text-stat 'sending 
                                        'collector-push
                                        text-stat
                                        collector))
                comm-to-list)
        (lyskom-wait-queue 'sending)

	(lyskom-traverse
            text-stat (collector->value collector)
          (when text-stat
            (when (catch 'unread
                    (lyskom-traverse
                        misc-item (text-stat->misc-info-list text-stat)
                      (when (and (eq (misc-info->type misc-item) 'COMM-IN)
                                 (not (lyskom-text-read-at-least-once-p 
                                       (blocking-do
                                        'get-text-stat
                                        (misc-info->comm-in misc-item)))))
                        (throw 'unread t))))
              (unless (lyskom-j-or-n-p
                       (lyskom-format 'have-unread-comment text-stat))
                (signal 'lyskom-edit-text-abort
                        (list "%s"
                              (lyskom-get-string 
                               'please-check-commented-texts))))))))
      (lyskom-message "%s" (lyskom-format 'checking-comments-done)))
    
    
    ;;
    ;; Confirm multiple recipients
    ;;
    

    (if (and (lyskom-default-value 'kom-confirm-multiple-recipients)
             (not (eq (lyskom-default-value 'kom-confirm-multiple-recipients)
                      'before))
             (> (- num-real-recpt num-me) 1))

        (save-excursion
          (goto-char (point-min))
          (if (not 
               (lyskom-j-or-n-p
                (lyskom-format 'comment-all-relevant-p)))
              (signal 'lyskom-edit-text-abort 
                      (list "%s" 
                            (lyskom-get-string 
                             'please-edit-recipients))))))

    ;;
    ;; Check that the authors of all commented texts get to see the new text
    ;;

    (let ((authors-to-ask-about (lyskom-compute-recipients-commented-authors 
				 comm-to-list 
				 (delq nil 
				       (mapcar (lambda (misc) 
						 (and (memq (car misc) lyskom-recpt-types-list)
						      (cdr misc)))
					       (cdr misc-list))))))
      (when authors-to-ask-about

        ;;
        ;; Now authors-to-ask-about contains all authors that we
        ;; want to ask about. So do that.
        ;;

        (lyskom-traverse author authors-to-ask-about
          (let ((send-comments-to
                 (car (lyskom-get-aux-item (conf-stat->aux-items author) 33))))
            (when send-comments-to
              (cond ((string-match "^\\([0-9]+\\)\\s-+\\([0-9]+\\)" (aux-item->data send-comments-to))
                     (setq send-comments-to 
                           (cons (string-to-number
                                  (match-string 1 (aux-item->data send-comments-to)))
                                 (string-to-number
                                  (match-string 2 (aux-item->data send-comments-to))))))
                    ((string-match "^\\([0-9]+\\)"
                                   (aux-item->data send-comments-to))
                     (setq send-comments-to
                           (cons (string-to-number
                                  (match-string 1 (aux-item->data send-comments-to)))
                                 0)))
                    (t (setq send-comments-to nil))))

            (when (lyskom-j-or-n-p 
                   (lyskom-format 'add-recipient-p 
                                  author
                                  (car send-comments-to)))
              (setq extra-headers
                    (nconc
                     (list
                      (cond ((and send-comments-to
                                  (eq (cdr send-comments-to) 0)) 'RECPT)
                            ((and send-comments-to
                                  (eq (cdr send-comments-to) 1)) 'CC-RECPT)
                            ((and send-comments-to
                                  (eq (cdr send-comments-to) 15)) 'BCC-RECPT)
                            (t (if (lyskom-j-or-n-p
                                    (lyskom-format 'really-add-as-recpt-q author))
                                   'RECPT
                                 'CC-RECPT)))
                            (conf-stat->conf-no author))
                     extra-headers)))))))
    extra-headers))


;;UNUSED: lyskom-send-enriched
(defun lyskom-send-enriched ()
  (let ((content-type (lyskom-get-aux-item lyskom-edit-aux-list 1)))
    (when (or (null content-type)
              (and (= (length content-type) 1)
                   (string-match "^text/enriched" (aux-item->data (car content-type)))))
      (condition-case err
          (let ((buf (lyskom-get-buffer-create 'lyskom-enriched 
                                               "lyskom-enriched" 
                                               t)))
            (unwind-protect
                (save-current-buffer
                  (set-buffer buf)
                  (insert lyskom-edit-text)
                  (goto-char (point-min))
                  (format-encode-buffer 'text/enriched)
                  (goto-char (point-min))
                  (search-forward "\n\n")
                  (when (and (not (lyskom-string= 
                                   (replace-in-string (buffer-substring (point) (point-max))
                                                      "<<" "<" t)
                                   lyskom-edit-text))
                             (lyskom-with-lyskom-buffer
                               (lyskom-j-or-n-p 
                                (lyskom-get-string 'send-formatted))))
                    (setq lyskom-edit-text (buffer-substring (point) (point-max)))
                    (unless content-type
                      (setq lyskom-edit-aux-list 
                            (cons (lyskom-create-aux-item 0 1 nil nil
                                                          (lyskom-create-aux-item-flags nil nil nil nil nil nil nil nil)
                                                          0
                                                          "text/enriched")
                                  lyskom-edit-aux-list)))
                    (setq lyskom-edit-aux-list
                          (cons (lyskom-create-aux-item 0 10002 nil nil
                                                        (lyskom-create-aux-item-flags nil nil nil nil nil nil nil nil)
                                                        0
                                                        (buffer-substring (point-min) (point)))
                                lyskom-edit-aux-list)))
                  'stop-transforming-texts)
              (kill-buffer buf)))
        (error (if (lyskom-j-or-n-p
                    (lyskom-format (lyskom-get-string 'transform-error)
                                   (error-message-string err)))
                   nil
                 (signal 'lyskom-edit-text-abort nil)))))))



(defun kom-edit-quit ()
  "Cancel editing the text being written and return to reading LysKOM."
  (interactive)
  (let ((edit-buffer (current-buffer)))
    (goto-char (point-max))
    (setq lyskom-is-writing nil)
    (lyskom-tell-internat 'kom-tell-regret)
    (lyskom-save-excursion (set-buffer edit-buffer)
                           (delete-auto-save-file-if-necessary))
    (lyskom-undisplay-buffer edit-buffer)
    (kill-buffer edit-buffer))
  (garbage-collect))			;Take care of the garbage.


(defun kom-edit-show-commented ()
  "Show the commented text in another window."
  (interactive)
  (lyskom-edit-get-commented 'lyskom-edit-show-commented))


(defun kom-edit-insert-commented (arg)
  "Insert the commented text, prepending each line with the
text in `kom-cite-string' (defaults to \"> \"). Note that citing
the commented text is not common practise in LysKOM (unlike
e-mail and news) since there is a strong link to the commented
text anyway. Use this command sparingly."
  (interactive "P")
  (lyskom-edit-get-commented
   'lyskom-edit-insert-commented
   (list (cond ((and arg (listp arg)) "")
	       (t nil)))))


(defun kom-edit-insert-buglist ()
  "Insert the commented buglist, Roxen Internet Software style. Excluded from manual."
  (interactive)
  (lyskom-edit-get-commented 'lyskom-edit-insert-buglist))


(defun kom-edit-insert-digit-text ()
  "Prompt for a text to insert. Excluded from manual."
  (interactive)
  (setq unread-command-events (cons last-command-event unread-command-events))
  (call-interactively 'kom-edit-insert-text nil))


(defun kom-edit-insert-text (no)
  "Prompt for a text to insert, prefixing each line with the contents
of `kom-cite-string' (defaults to \"> \"). Note that citing texts is not
commonplace in LysKOM (unlike e-mail and news) since it is easy to
refer to specific texts (see `kom-edit-insert-link' and
`kom-edit-add-cross-reference'). Use this command sparingly."
  (interactive (list
		(cond
		 ((null current-prefix-arg)
                  (lyskom-read-number 'which-text-include))
		 ((prefix-numeric-value current-prefix-arg)))))
  (let ((buffer (current-buffer))
        (window (selected-window)))
    (set-buffer lyskom-buffer)
    (lyskom-collect 'edit)
    (initiate-get-text 'edit nil no)
    (initiate-get-text-stat 'edit nil no)
    (lyskom-use 'edit 'lyskom-edit-insert-commented buffer window)
    (set-buffer buffer)
    (sit-for 0)))


(defun lyskom-edit-get-commented (thendo &optional arg-list)
  "Get the commented text and text stat and then do THENDO with it.
This function is called with an argument list TEXT TEXT-STAT BUFFER
WINDOW plus any optional arguments given in ARG-LIST."
  (let ((p (point)))
    (save-excursion
      (let* ((buffer (current-buffer))
             (window (selected-window))
             (headers (condition-case nil
                          (lyskom-text-headers->misc-info (lyskom-edit-parse-headers))
                        (lyskom-edit-error nil))) ; Ignore these errors
             (no nil))
        (while headers
          (if (memq (car headers) '(COMM-TO FOOTN-TO))
              (setq no (car (cdr headers))
                    headers nil)
            (setq headers (cdr (cdr headers)))))
        (cond
         (no
          (goto-char p)
          (set-buffer lyskom-buffer)
          (blocking-do-multiple ((text (get-text no))
                                 (text-stat (get-text-stat no)))
            (set-buffer buffer)
            (apply thendo text text-stat buffer window arg-list)))
         (t
          (lyskom-message "%s" (lyskom-get-string 'no-such-text-m))))))
    (sit-for 0)))


;;; ============================================================
;;; Info node stuff

(defvar Info-current-node)
(defvar Info-current-file)

;; NOTUSED: kom-yank-info-nodename
(defalias 'kom-yank-info-nodename 'kom-edit-yank-info-nodename)
(defun kom-edit-yank-info-nodename ()
  "When browsing info files, this command will place a reference to the
current info node in the kill ring, from where it can be pasted into
another buffer. This command is useful when you want to refer to an 
info node in a LysKOM text."
  (interactive)
  (kill-new (format "*Note %s: (%s)%s,"
		    Info-current-node
		    (file-name-nondirectory Info-current-file)
		    Info-current-node)))


;; NOTUSED: kom-insert-last-info-nodename
(defalias 'kom-insert-last-info-nodename 'kom-edit-insert-last-info-nodename)
(defun kom-edit-insert-last-info-nodename ()
  "Insert a reference to the most recently visited info node."
  (interactive)
  (condition-case nil
      (let ((link nil))
        (save-current-buffer
          (set-buffer (get-buffer "*info*"))
          (setq link (format "*Note %s: (%s)%s,"
                             Info-current-node
                             (file-name-nondirectory Info-current-file)
                             Info-current-node)))
        (insert link))
    (error (lyskom-message "%s" (lyskom-get-string 'cant-find-info-node)))))

;;; ================================================================
;;;	  Add recipient, copy-recipient - Addera mottagare 
;;;
;;;  Author: Anders Gertz
;;;  Changed by: Linus Tolke

(defun kom-edit-add-comment ()
  "Makes this text a comment to another text. Using this command it is
possible to make the text a comment to multiple texts. To remove a 
comment link, simply remove the corresponding line from the headers
in the edit buffer."
  (interactive)
  (let* ((edit-buffer (current-buffer))
         (insert-at (point-min-marker))
         (text-no (lyskom-read-number (lyskom-get-string 'text-to-comment-q)))
         (text-stat (blocking-do 'get-text-stat text-no)))
    (lyskom-ignore edit-buffer)
    (lyskom-save-excursion
     (if text-stat
         (lyskom-edit-get-commented-author 
          (blocking-do 'get-text-stat text-no)
          (lyskom-get-string 'comment)
          insert-at text-no)
       (lyskom-error "%s" (lyskom-get-string 'no-such-text-m)))
     (set-marker insert-at nil))))


(defun kom-edit-add-recipient ()
  "Adds a regular recipient to the text or converts an existing
recipient to a regular recipient. Using this command it is possible to
add any number of regular recipients. To remove a recipient, simply
delete the corresponding header line in the edit buffer."
  (interactive)
  (lyskom-edit-add-recipient/copy 'added-recipient nil 'RECPT))


(defun kom-edit-add-bcc ()
  "Adds a blind carbon copy recipient to the text, or converts an
existing recipient to blind carbon copy. Using this command it is
possible to add any number of regular recipients. To remove a
recipient, simply delete the corresponding header line in the edit
buffer."
  (interactive)
  (lyskom-edit-add-recipient/copy 'added-blank-carbon-copy nil 'BCC-RECPT))


(defun kom-edit-add-copy ()
  "Adds a carbon copy recipient to the text, or converts an existing
recipient to blind carbon copy. Using this command it is possible to
add any number of regular recipients. To remove a recipient, simply
delete the corresponding header line in the edit buffer."
  (interactive)
  (lyskom-edit-add-recipient/copy 'added-carbon-copy nil 'CC-RECPT))

(defun kom-edit-move-text ()
  "Adds a regular recipient to the text, or converts an existing
recipient to blind carbon copy, and converts all other recipients to
carbonn copy recipients. This command is intended for situations where
a commend is being sent to a different recipient than the commented
text was."
  (interactive)
  (lyskom-edit-add-recipient/copy 'edit-who-to-move-to-q
                                  'lyskom-edit-move-recipients))


(defun lyskom-edit-move-recipients (conf-stat insert-at edit-buffer)
  (save-current-buffer
    (set-buffer edit-buffer)
    (let* ((headers (lyskom-edit-parse-headers))
           (subject (lyskom-text-headers->subject headers))
           (miscs (mapcar (lambda (x) (cond ((and (eq (car x) 'RECPT)
						  (not (conf-type->letterbox
							(uconf-stat->conf-type
							 (blocking-do 'get-uconf-stat (cdr x))))))
					     (cons 'CC-RECPT (cdr x)))
					    (t x)))
			  (cdr (lyskom-edit-translate-headers (lyskom-text-headers->misc-info headers)))))
           (aux-list (lyskom-text-headers->aux-items headers))
           (elem nil))


      ;; If the new target is already a recipient, convert it to the right 
      ;; kind. Otherwise insert the new target after the last comm-to

      (setq elem (lyskom-edit-find-misc miscs lyskom-recpt-types-list 
                                        (conf-stat->conf-no conf-stat)))
      (if elem
          (setcar elem 'RECPT)
        (lyskom-insert-in-list
         (cons 'RECPT (conf-stat->conf-no conf-stat))
         miscs
         (car (cdr (memq (lyskom-edit-find-misc miscs '(FOOTN-TO COMM-TO) 
                                                nil t)
                         miscs)))))

      (lyskom-edit-replace-headers subject (cons 'MISC-LIST miscs) aux-list))))



(defun lyskom-edit-do-add-recipient/copy (recpt-type recpt-no edit-buffer)
  (save-current-buffer
    (set-buffer edit-buffer)
    (let* ((headers (lyskom-edit-parse-headers))
           (miscs (lyskom-edit-translate-headers (lyskom-text-headers->misc-info headers)))
           (elem (lyskom-edit-find-misc miscs lyskom-recpt-types-list
                                        recpt-no)))

      (cond (elem (setcar elem recpt-type))
            (t (setq miscs
                     (append miscs (list (cons recpt-type recpt-no))))))
      (lyskom-edit-replace-headers (lyskom-text-headers->subject headers)
                                   miscs
                                   (lyskom-text-headers->aux-items headers)))))


(defun lyskom-edit-add-recipient/copy (prompt &optional what-to-do recpt-type)
  "Adds a new recipient or a cc-recipient to the text which is being edited.
PROMPT is the prompt to use to ask the user for a recipient.
WHAT-TO-DO is a function to call to do the insertion.
RECPT-TYPE is the type of recipient to add."
  (let ((edit-buffer (current-buffer))
	(insert-at (point-min-marker))
	(conf-stat (lyskom-read-conf-stat prompt '(all) nil nil t)))
    (lyskom-save-excursion
     (save-current-buffer
       (set-buffer lyskom-buffer)
       ;; +++ The information about msg-of-day might be old. We should
       ;; make sure it is up-to-date.

       (let ((text-no (conf-stat->msg-of-day conf-stat))
             (win-config nil)
             (text nil)
             (text-stat nil)
             (collector (make-collector)))

         (unless (zerop text-no)
           (initiate-get-text 'edit 'collector-push text-no collector)
           (initiate-get-text-stat 'edit 'collector-push text-no collector)
           (lyskom-wait-queue 'edit)
           (setq text-stat (elt (collector->value collector) 0))
           (setq text (elt (collector->value collector) 1)))

         (when (or (null text)
                   (null text-stat)
                   (null (get-buffer-window edit-buffer))
                   (progn (setq win-config (current-window-configuration))
                          (with-output-to-temp-buffer "*Motd*"
                            (lyskom-princ
                             (lyskom-format 'conf-has-motd-no
                                            (text->text-no text)
                                            (text->decoded-text-mass text 
                                                                     text-stat))))
                          (j-or-n-p (lyskom-get-string 'still-want-to-add))))

           (when (and kom-confirm-add-recipients
                      (eq recpt-type 'RECPT)
                      (not (lyskom-j-or-n-p (lyskom-format
                                             'really-add-as-recpt-q
                                             conf-stat))))
             (setq recpt-type 'CC-RECPT))

           (if what-to-do
               (funcall what-to-do conf-stat insert-at edit-buffer)
             (lyskom-edit-do-add-recipient/copy recpt-type
                                                (conf-stat->conf-no conf-stat)
                                                edit-buffer)))
         (when win-config (set-window-configuration win-config))))
     (set-marker insert-at nil))))

(defun lyskom-edit-sub-recipient/copy (recpt-no edit-buffer)
  "Remove the recipient having RECPT-NO from EDIT-BUFFER"
  (save-current-buffer
    (set-buffer edit-buffer)
    (let* ((headers (lyskom-edit-parse-headers))
           (miscs (lyskom-edit-translate-headers (lyskom-text-headers->misc-info headers)))
           (elem (lyskom-edit-find-misc miscs lyskom-recpt-types-list
                                        recpt-no)))

      (when elem (setcar elem nil))

      (lyskom-edit-replace-headers (lyskom-text-headers->subject headers)
                                   miscs
                                   (lyskom-text-headers->aux-items headers)))))
    

(defun kom-edit-add-cross-reference ()
  "Add a cross reference to this text. This command prompts for the
type and target of the cross reference. Cross references are used
to systematically refer from a text to another text, conference or
person without altering the contents of the text."
  (interactive)
  (let ((item (lyskom-read-cross-reference-and-get-aux-item)))
    (when item
      (lyskom-edit-insert-aux-item item))))

(defun kom-edit-add-read-confirm-request ()
  "Request that others confirm reading this text. Conforming clients
will ask each reader of this text to confirm reading the text. Note
that not all clients understand this request, and that confirmation is
neither automatic nor mandatory. Use very sparingly as this interrupts
the normal flow of reading for many users."
  (interactive)
  (lyskom-edit-insert-aux-item
   (lyskom-create-aux-item 0 6 0 0
                           (lyskom-create-aux-item-flags
                            nil nil nil nil nil nil nil nil)
                           0 "")))

(defun kom-edit-add-no-comments ()
  "Request that nobody comments this text. Conforming clients will
either prevent users from commenting the text or ask for confirmation
before commenting the text. Note that not all clients understand this
request and that it is advisory only."
  (interactive)
  (lyskom-edit-insert-aux-item
   (lyskom-create-aux-item 0 4 0 0
                           (lyskom-create-aux-item-flags
                            nil nil nil nil nil nil nil nil)
                           0 "")))

(defun kom-edit-add-personal-comments ()
  "Request that all replies to this text are in the form of personal
replies. Conforming clients will treat a request to comment this text
as a request to answer privately. Note that not all clients understand
this request and that it is advisory only."
  (interactive)
  (lyskom-edit-insert-aux-item
   (lyskom-create-aux-item 0 5 0 0
                           (lyskom-create-aux-item-flags
                            nil nil nil nil nil nil nil nil)
                           0 "")))

(defun kom-edit-add-world-readable ()
  "Make this text readable to all users, even if they are not logged
in. World readable texts can be read by anyone, regardless of what
recipients the text has. World readable texts can even be read without
logging in to LysKOM."
  (interactive)
  (lyskom-edit-insert-aux-item
   (lyskom-create-aux-item 0 34 0 0
                           (lyskom-create-aux-item-flags
                            nil nil nil nil nil nil nil nil)
                           0 "")))

(defun kom-edit-insert-link ()
  "Insert an in-line link to another text, a conference or a person.
This command prompts for the target of the link and inserts text that
most clients will interpret as a link."
  (interactive)
  (let ((item (lyskom-read-link)))
    (when item
      (insert item))))



(defun lyskom-read-link ()
  "Query user about link type and value, and return the corresponding
link as a string."
  (let* ((type (lyskom-a-or-b-or-c-p 'link-type
                                     '(abc-conference
                                       abc-person
                                       abc-text)
                                     nil))
         (obj nil))
    (cond
     ((eq type 'abc-text)
      (let ((prompt 'which-text-to-link))
        (while (null obj)
          (setq obj (blocking-do 'get-text-stat
                                 (lyskom-read-number prompt)))
          (setq prompt 'which-text-to-link-err )))
      (let* ((text-no (text-stat->text-no obj))
             (text (blocking-do 'get-text text-no))
	     (txt (text->decoded-text-mass text obj))
	     (eos (string-match (regexp-quote "\n") txt))
	     (subject (substring txt 0 eos)))
	(format "<text %d: %s>" text-no subject)))
     
     ((eq type 'abc-conference)
      (while (null obj)
        (setq obj (lyskom-read-conf-stat 'which-conf-to-link '(conf) nil nil t)))
      (format "<m�te %d: %s>" (conf-stat->conf-no obj)
			   (conf-stat->name obj)))

     ((eq type 'abc-person)
      (while (null obj)
        (setq obj (lyskom-read-conf-stat 'which-pers-to-link '(pers) nil nil t)))
      (format "<person %d: %s>" (conf-stat->conf-no obj)
			   (conf-stat->name obj))))))


(defun lyskom-edit-insert-aux-item (item)
  "Insert the aux item ITEM in the current buffer"
  (save-excursion
    (lyskom-edit-find-separator t)
    (forward-line (if kom-edit-hide-add-button -1 -2))
    (insert
     (concat (lyskom-format
              (format "%%#1@%%[%s%%] %%#2s" (lyskom-get-string 'aux-item-prefix))
	      ; FIXME: What happens with marker?
              (lyskom-default-button 'aux-edit-menu (cons (current-buffer)
                                                          (point-marker)))
              (lyskom-aux-item-call item 
                                    '(edit-insert print)
                                    item 
                                    lyskom-pers-no))
              "\n")))
  )


(defun lyskom-edit-toggle-secret-aux (buf arg text)
  (interactive)
  (save-current-buffer
    (set-buffer (car arg))
    (goto-char (cdr arg))
    (lyskom-edit-toggle-aux-item-flag buf arg text 'secret)))

(defun lyskom-edit-toggle-anonymous-aux (buf arg text)
  (interactive)
  (save-current-buffer
    (set-buffer (car arg))
    (goto-char (cdr arg))
    (lyskom-edit-toggle-aux-item-flag buf arg text 'anonymous)))

(defun lyskom-edit-toggle-inherit-aux (buf arg text)
  (interactive)
  (save-current-buffer
    (set-buffer (car arg))
    (goto-char (cdr arg))
    (lyskom-edit-toggle-aux-item-flag buf arg text 'inherit)))

(defun lyskom-edit-delete-aux (buf arg text)
  (interactive)
  (save-current-buffer
    (set-buffer (car arg))
    (goto-char (cdr arg))
    (beginning-of-line)
    (delete-region (point) (save-excursion (forward-line 1) (point)))))


(defun lyskom-edit-generate-aux-item-flags (flags)
  (let ((str (mapconcat 'identity
                          (delq nil
                                (list
                                 (and (aux-item-flags->secret flags)
                                      (lyskom-get-string 'secret-aux-flag))
                                 (and (aux-item-flags->anonymous flags)
                                      (lyskom-get-string 'anonymous-aux-flag))
                                 (and (aux-item-flags->inherit flags)
                                      (lyskom-get-string 'inherit-aux-flag))))
                          ", ")))
      (when (not (string= str ""))
        (format " [%s]" str))))

(defun lyskom-edit-insert-aux-item-flags (flags)
  (end-of-line)
  (insert (lyskom-edit-generate-aux-item-flags flags)))

(defun lyskom-edit-toggle-aux-item-flag (buf arg text flag)
  (beginning-of-line)
  (let ((flags (lyskom-edit-extract-aux-item-flags)))
    (when (re-search-forward "\\s-?\\[[^]]*\\]\\s-*$" 
                             (save-excursion (end-of-line) (point)) t)
      (delete-region (match-beginning 0) (match-end 0)))
    (funcall (intern (format "set-aux-item-flags->%s" flag))
             flags
             (not
              (funcall (intern (format "aux-item-flags->%s" flag)) flags)))
    (lyskom-edit-insert-aux-item-flags flags)))



  

;;; ================================================================
;;;   Help functions for the functions bound to keyboard sequences 
;;;                       in lyskom-edit-mode.


(defun lyskom-edit-find-misc (misc-list type data &optional last)
  "Return the first misc-info pair in MISC-LIST of type TYPE containing DATA.
If TYPE is a list, then any type in TYPE is considered to match. If DATA
is nil, then any DATA is considered to match.
If optional LAST is non-nil, then return the last match instead of the first."
  (when (eq (car misc-list) 'MISC-LIST) (setq misc-list (cdr misc-list)))
  (let ((result nil)
        (elem nil))
    (while (and misc-list (or last (null result)))
      (setq elem (car misc-list))
      (setq misc-list (cdr misc-list))
      (when (cond ((listp type) (and (memq (car elem) type)
                                     (or (null data) (eq data (cdr elem)))))
                  ((symbolp type) (and (eq type (car elem))
                                       (or (null data) (eq data (cdr elem)))))
                  (t (or (null data) (eq data (cdr elem)))))
        (setq result elem)))
    result))

(defun lyskom-edit-translate-headers (misc-list)
  "Translate result of lyskom-edit-parse-header to something we can send
to lyskom-edit-replace-headers"
  (let ((result nil))
    (while misc-list
      (setq result (cons (cons (car misc-list) (car (cdr misc-list)))
                         result))
      (setq misc-list (cdr (cdr misc-list))))
    (cons 'MISC-LIST (nreverse result))))

(defun lyskom-edit-replace-headers (subject misc-list aux-list)
  "Replace all headers with SUBJECT and MISC-LIST"
  (save-excursion
    (let ((start nil)
          (inhibit-read-only t)
          (end nil))
      (goto-char (point-min))
      (setq start (point-marker))
      (set-marker-insertion-type start t)
      (lyskom-edit-find-separator t)
      (end-of-line)
      (setq end (point-marker))
      (goto-char (point-min))
      (lyskom-edit-insert-miscs misc-list subject "" aux-list)
      (delete-region start end)
      (goto-char end)
      (delete-char 1)
      (set-marker start nil)
      (set-marker end nil))))

(defun lyskom-looking-at-header (header match-number)
  "Check if point is at the beginning of a header of type HEADER.
Return the corresponding number (conf no etc.) if MATCH-NUMBER is
non-nil. If MATCH-NUMBER is 'angled, only match a number inside <>."
  (if (lyskom-looking-at
       (concat (lyskom-get-string header)
	       (cond ((eq match-number 'angled)
		      "[^0-9\n\r]*<\\([0-9]+\\)>")
                     ((eq match-number 'empty)
                      "[^:]*:\\(\\s-*\\)$")
		     (match-number
		      "[^0-9]*\\([0-9]+\\)")
		     (nil
		      ""))))
      (if match-number
	  (lyskom-string-to-number (buffer-substring (match-beginning 2)
					   (match-end 2)))
	t)
    nil))

(defun lyskom-edit-parse-headers ()
  "Parse the headers of an article.

The value returned is a lyskom-text-headers structure."
  (save-excursion
    (goto-char (point-min))
    (let ((misc nil)
          (subject nil)
          (aux nil))
      (save-restriction
        ;; Narrow to headers
        (lyskom-edit-find-separator t)
        (narrow-to-region (point-min) (point))
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((case-fold-search t)
                (n nil))
            (cond
             ((setq n (lyskom-looking-at-header 'recipient-prefix 'angled))
              (setq misc (nconc misc (list 'RECPT n))))
             ((setq n (lyskom-looking-at-header 'carbon-copy-prefix 'angled))
              (setq misc (nconc misc (list 'CC-RECPT n))))
             ((setq n (lyskom-looking-at-header 'blank-carbon-copy-prefix
                                                'angled))
              (setq misc (nconc misc (list 'BCC-RECPT n))))
             ((setq n (lyskom-looking-at-header 'comment-prefix t))
              (setq misc (nconc misc (list 'COMM-TO n))))
             ((setq n (lyskom-looking-at-header 'footnote-prefix t))
              (setq misc (nconc misc (list 'FOOTN-TO n))))
             ((lyskom-looking-at-header 'header-subject nil)
              (setq subject (lyskom-edit-extract-subject)))

             ((lyskom-looking-at (lyskom-get-string 'aux-item-prefix-regexp))
              (goto-char (match-end 0))
              (let ((item (lyskom-edit-parse-aux-item)))
                (if item
                    (setq aux (cons item aux))
                  (signal 'lyskom-unknown-header
                          (list 'unknown-header (point))))))

             ((lyskom-looking-at (lyskom-get-string 'comment-item-prefix))
              nil)

             ((or (lyskom-looking-at-header 'blank-carbon-copy-prefix 'empty)
                  (lyskom-looking-at-header 'carbon-copy-prefix 'empty)
                  (lyskom-looking-at-header 'recipient-prefix 'empty))
              nil)
             ((lyskom-looking-at-header 'add-recpt-button-text-regex nil) nil)

             (t (signal 'lyskom-unknown-header (list 'unknown-header (point))))))
          (forward-line 1)))
      (lyskom-create-lyskom-text-headers subject misc aux))))

(defun lyskom-edit-parse-aux-item ()
  (let ((definitions lyskom-aux-item-definitions)
        (data nil))
    (while (and (null data) definitions)
      (setq data (lyskom-aux-item-call (car definitions) 'parse))
      (unless data (setq definitions (cdr definitions))))

    (when data
      (lyskom-create-aux-item 0 (lyskom-aux-item-definition->number
                                 (car definitions))
                              0 0 (lyskom-edit-extract-aux-item-flags) 0
                              data))))

(defun lyskom-edit-extract-aux-item-flags ()
  (if (lyskom-looking-at ".*\\[\\([^]]*\\)\\]\\s-*$")
    (let ((flag-strings (match-string 1))
          (start 0)
          (flag nil)
          (flags (lyskom-create-aux-item-flags 
                  nil nil nil nil nil nil nil nil))
          (case-fold-search t))
      (while (string-match "\\s-*\\(\\w+\\)\\s-*" flag-strings start)
        (setq flag (match-string 1 flag-strings))
        (setq start (match-end 0))
        (cond ((string-match (lyskom-get-string 'secret-aux-flag) flag)
               (set-aux-item-flags->secret flags t))
              ((string-match (lyskom-get-string 'anonymous-aux-flag) flag)
               (set-aux-item-flags->anonymous flags t))
              ((string-match (lyskom-get-string 'inherit-aux-flag) flag)
               (set-aux-item-flags->inherit flags t))
              (t nil)))
      flags)
    (lyskom-create-aux-item-flags nil nil nil nil nil nil nil nil)))


(defun lyskom-edit-extract-subject ()
  "Find the subject.
Point must be located on the line where the subject is."
  (re-search-forward ": \\(.*\\)")
  (buffer-substring (match-beginning 1) (match-end 1)))


(defun lyskom-edit-extract-text ()
  "Get text as a string."
  (save-excursion
    (if (not (lyskom-edit-find-separator t))
	(signal 'lyskom-internal-error
		"Altered lyskom-header-separator line.")
      (end-of-line)
      (buffer-substring (1+ (point))
			(progn
			  (goto-char (1- (point-max)))
			  (while (lyskom-looking-at "\\s-") ; remove trailing
			    (backward-char 1)) ; whitespace
			  (forward-char 1)
			  (point))))))


(defun lyskom-create-text-handler (text-no edit-buffer is-anonymous
					   callback callback-data 
                                           callback-buffer)
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
    (set-buffer lyskom-buffer)
    (read-list-enter-first (lyskom-create-read-info 'RE-EDIT-TEXT
                                                    nil
                                                    256
                                                    nil
                                                    nil
                                                    nil
                                                    edit-buffer)
                           lyskom-reading-list)
    (lyskom-update-prompt)
    (set-buffer edit-buffer)
    (lyskom-edit-mode 1)
    (sit-for 0))
   (t 
    (if is-anonymous
        (lyskom-format-insert-before-prompt 'text-created-anonymous text-no)
      (lyskom-insert-before-prompt (lyskom-format 'text-created  text-no)))

    ;; Save the text

    (when (and kom-created-texts-are-saved (buffer-live-p edit-buffer))
      (initiate-get-conf-stat 'background 
                              'lyskom-edit-fcc-text
                              lyskom-pers-no
                              (save-current-buffer (set-buffer edit-buffer)
                                                   (buffer-string))
                              text-no
                              is-anonymous))

    ;; Immediately mark the text as read if kom-created-texts-are-read is set
    ;; and we are not sending the text anonymously.
    
    (cond
     ((and kom-created-texts-are-read (not is-anonymous))
      (lyskom-is-read text-no)
      (initiate-get-text-stat 'background 'lyskom-mark-as-read
			      text-no)
      (lyskom-run 'background 'set 'lyskom-dont-change-prompt nil)
      (lyskom-run 'background 'lyskom-set-mode-line))
     (t (setq lyskom-dont-change-prompt nil)))

    (save-current-buffer
      (set-buffer edit-buffer)		;Need local variables.
      (lyskom-edit-sent-mode 1))

    ;; Record the text number

    (unless is-anonymous
      (lyskom-setq-default lyskom-last-written text-no)
      (lyskom-setq-default lyskom-last-seen-written text-no))

    ;; Select the old configuration.

    (when (get-buffer-window edit-buffer)
      (set-window-configuration lyskom-edit-return-to-configuration)
      (set-buffer (window-buffer (selected-window)))
      (goto-char (point-max)))

    ;; Apply handler.

    (when (and callback (buffer-live-p callback-buffer))
      (save-current-buffer
        (set-buffer callback-buffer)
        (if callback (apply callback text-no callback-data))))

    ;; Kill the edit-buffer.

    (save-current-buffer
     (set-buffer edit-buffer)
     (delete-auto-save-file-if-necessary))
    (kill-buffer edit-buffer)
)))

(defun lyskom-edit-fcc-text (conf-stat text text-no is-anonymous)
  (condition-case arg
      (let ((start 1) (end 0) (inhibit-read-only t))
        (save-current-buffer
          (set-buffer (lyskom-get-buffer-create 'fcc "*kom*-fcc" t))
          (erase-buffer)
          (insert text)
          (goto-char (point-min))
          (when (lyskom-edit-find-separator t)
            (delete-region (match-beginning 0) (match-end 0))
            (unless kom-edit-hide-add-button
              (save-excursion
                (beginning-of-line)
                (forward-line -2)
                (delete-region (progn (beginning-of-line) (point))
                               (progn (forward-line 1) (point)))))
            (insert (make-string kom-text-header-dash-length ?-))
            (forward-line 1)
            (beginning-of-line)
            (setq start (point)))
          (goto-char (point-min))
          (when (re-search-forward "\\(\\s-\\|[\n\r]\\)+\\'" nil t)
            (delete-region (match-beginning 0) (match-end 0)))
          (goto-char (point-max))
          (setq end (point))
          (insert (lyskom-format "\n(%#1n) %#2s\n\n"
                                 text-no
                                 (make-string (- kom-text-footer-dash-length
                                                 (lyskom-string-width (int-to-string text-no)) 
                                                 3) ?-)))
          (goto-char (point-min))
          (lyskom-format-insert-at-point 'text-no-comment 
                                         text-no
                                         (let ((kom-print-relative-dates nil))
                                           (lyskom-format-time 'date-and-time))
                                         (count-lines start end)
                                         conf-stat
                                         is-anonymous)
          (append-to-file (point-min)
                          (point-max)
                          (expand-file-name kom-created-texts-are-saved))))
    (file-error (message "%S" arg)(lyskom-format-insert-before-prompt 'cant-fcc-text-file-error
                                                    text-no
                                                    kom-created-texts-are-saved
                                                    (elt arg 1)
                                                    (elt arg 2)
                                                    ))
    (error (lyskom-format-insert-before-prompt 'cant-fcc-text 
                                               text-no
                                               kom-created-texts-are-saved
                                               (error-message-string arg)))))
                          


(defun lyskom-edit-show-commented (text text-stat editing-buffer window)
  "Handles the TEXT and TEXT-STAT from the return of the call of the text.
The EDITING-BUFFER is the buffer the editing is done in. If this buffer is 
not displayed nothing is done. If displayed then this buffer is chosen then 
the with-output-to-temp-buffer command is issued to make them both apear."
  (and text
       (get-buffer-window editing-buffer)
       (progn
         (set-buffer editing-buffer)
         (select-window window)
         (let ((buf (lyskom-get-buffer-create 'view-commented "*Commented*"))
               (kom-deferred-printing nil))
           (save-selected-window
             (lyskom-display-buffer buf)
             (unless kom-review-uses-cache
               (cache-del-text-stat (text->text-no text)))
             (save-current-buffer (set-buffer buf)
                                  (erase-buffer)
                                  (lyskom-view-text (text->text-no text))
                                  (set-buffer-modified-p nil)
                                  (lyskom-view-mode)))))))


(defun lyskom-edit-insert-commented (text text-stat editing-buffer window &optional prefix)
  "Handles the TEXT and TEXT-STAT from the return of the call of the text.
When given a PREFIX string, that is prepended to each inserted line.
If PREFIX is nil, use the default prefix (kom-cite-string or \">\"."
  (if (and text text-stat)
      (let ((str (text->decoded-text-mass text text-stat))
	    (prefix (or prefix (lyskom-default-value 'kom-cite-string) ">")))
        (set-buffer editing-buffer)
        (and (not (bolp))
             (insert "\n"))
        (and (not (eolp))
             (open-line 1))
        (let* ((pb (point))
               (as (string-match "\n" str))
               (te (substring str (1+ as))))
          (insert te)
          (while (<= pb (point))
            (beginning-of-line)
	    (when prefix
	      (insert prefix))
            (forward-line -1))))
    (lyskom-message "%s" (lyskom-get-string 'no-get-text))))


;;; ========================================================================
;;;   Treat the commented text as a Roxen Internet Software-style buglist,
;;;   handling the removal of closed subjects, last-message changes et c

;;; Author: Johan Sundstr�m

(defun lyskom-edit-insert-buglist (text text-stat editing-buffer window)
  "Handles the TEXT and TEXT-STAT from the return of the call of the text.
The commented text is inserted in the buffer in the Roxen Internet Software
buglist style, automating the removal of closed subjects and change-marks."
  (if (and text text-stat)
      (let ((str (text->decoded-text-mass text text-stat)))
        (set-buffer editing-buffer)
        (and (not (bolp))
             (insert "\n"))
        (and (not (eolp))
             (open-line 1))
        (let* ((pb (point))
               (as (string-match "\n" str))
               (te (substring str (1+ as))))
	  ;; insert the body of the commented text at point (pb)
          (insert te)
	  ;; traverse the inserted text, replacing a leading !
	  ;; with a space wherever some form of "[ ]" follows
	  ;; (and X:s, since some morons were too inept to
	  ;;  handle these highly advanced syntactic rules)
          (while (re-search-backward "^\\(\\s-*\\)[!X]\\([ \t(]*\\[\\)" nil t)
            (replace-match "\\1 \\2"))
	  (goto-char pb)
	  ;; new pass; leave all [ ], [/] and [\] subjects untouched,
	  ;; but remove the rest, except for category headers, who remain.
          (while
	      ;; Find a closed subject (that does not match any of the above)
	      (re-search-forward "^[ \t*([]*\\[[^\\/ ]\\]" nil t)
	    (beginning-of-line)
	    ;; Delete from here to the next open subject or the
	    ;; next category header, whichever comes first.
	    (let* ((delete-from (point))
		   (delete-to
		    ;; First part matches an open subject, second part finds
		    ;; new headline. A headline is here defined as something
		    ;; with no more than two leading ws characters followed by
		    ;; a non-whitespace, non-[(*! character. The advantage of
		    ;; this over matching against ^\s-*$ is that long subjects
		    ;; may be split into separate paragraphs and still work here.
		    (re-search-forward "^[ \t*(]*\\[[\\/ ]\\]\\|^\\s-\\{0,2\\}[^ \t(*![]" nil t)))
	      (if delete-to
		  ;; There was at least one more subject later on
		  (progn
		    (goto-char delete-to)
		    (beginning-of-line)
		    ;; Are we looking at a headline?
		    (if (lyskom-thing-at-point-looking-at "^\\s-\\{0,2\\}[^ \t(*![]")
			;; yes -- hence we need to move point somewhat
			(progn
			  (beginning-of-line)
			  ;; leave the newline before the headline intact
			  (forward-char -1))))
		;; This was the last subject; kill the rest of the buffer
		(goto-char (point-max)))
	      ;; Drain the closed subject.
	      (delete-region delete-from (point))))))
    (lyskom-message "%s" (lyskom-get-string 'no-get-text))))



;;; ================================================================
;;; Tab between buttons in the header

(defun kom-edit-next-button-or-self-insert (num)
  "Move to the next link or insert a TAB, depending on context. Excluded from manual."
  (interactive "p")
  (let ((header-end (lyskom-edit-find-separator))
        (start (point))
        (next-pos nil))
    (if (> (point) header-end)
        (unless (lyskom-traverse keymap (current-minor-mode-maps)
                  (let ((binding (lookup-key keymap (this-command-keys))))
                    (when (and (commandp binding)
                               (not (eq binding
                                        'kom-edit-next-button-or-self-insert)))
                      (call-interactively binding)
                      (lyskom-traverse-break t))))
          (let ((binding (lookup-key (current-local-map) 
                                     (this-command-keys) t)))
            (if (and (commandp binding)
                       (not (eq binding
                                'kom-edit-next-button-or-self-insert)))
                (call-interactively binding)
              (call-interactively 'self-insert-command))))
      (while (> num 0)
        (lyskom-next-area 1 'lyskom-button)
        (if (eq start (point))
            (setq next-pos nil)
          (setq next-pos (point)))
        (cond ((or (null next-pos)
                   (> next-pos header-end))
               (goto-char start)
               (unless (re-search-forward (regexp-quote (lyskom-get-string
                                                         'header-subject))
                                          header-end
                                          t)
                 (goto-char (point-max))
                 (setq num 0)))
              (t (goto-char next-pos)))
        (setq num (1- num) start (point))))))

(defun kom-edit-prev-button (num)
  "Move to the previous link. Excluded from manual."
  (interactive "p")
  (let ((header-end (lyskom-edit-find-separator)))
    (while (> num 0)
      (cond ((and (> (point) header-end)
                  (progn (goto-char (point-min))
                         (re-search-forward (regexp-quote (lyskom-get-string
                                                           'header-subject))
                                            header-end
                                            t)))
             (setq num (1- num)))
            (t (lyskom-prev-area num 'lyskom-button)
               (setq num 0))))))
