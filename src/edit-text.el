;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: edit-text.el,v 44.59 2000-04-29 07:41:11 jhs Exp $
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
	      "$Id: edit-text.el,v 44.59 2000-04-29 07:41:11 jhs Exp $\n"))


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
See lyskom-edit-handler-data.")

(defvar lyskom-edit-handler-data nil
  "Status variable for an edit-buffer.
See lyskom-edit-handler.")

(defvar lyskom-edit-return-to-configuration nil
  "Status variable for an edit-buffer.")

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
  (let ((buffer (lyskom-get-buffer-create 'write-texts
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
    (make-local-variable 'lyskom-edit-return-to-configuration)
    (setq lyskom-edit-handler handler)
    (setq lyskom-edit-handler-data data)
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



(defun lyskom-edit-insert-miscs (misc-list subject body &optional aux-list)
  "Insert MISC-LIST into header of text.
recpt		-> Mottagare: <%d> %s.
cc-recpt	-> Extra kopia: <%d> %s.
bcc-recpt       -> För kännedom: <%d> %s.
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
	 ((eq key 'bcc-recpt)
	  (lyskom-edit-insert-misc-conf (blocking-do 'get-conf-stat data)
					(lyskom-get-string 'blank-carbon-copy)
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
    (mapcar (function
             (lambda (item)
               (let ((data (lyskom-aux-item-call
                            item '(edit-insert print)
                            item lyskom-pers-no)))
                 (when data
                   (lyskom-princ
                    (concat (lyskom-get-string 'aux-item-prefix) data "\n")
                    where-put-misc)))))
            aux-list)
    (lyskom-princ (lyskom-format 'text-mass subject 
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
  (lyskom-princ  (lyskom-format "%#1s <%#2m> %#3M\n" 
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
item in each pair should be one of recpt, cc-recpt, comm-to or footn-to.
The second item should be the corresponding conf- or text-no.

The result is a list of dotted pairs:
	('recpt . conf-no)
	('cc-recpt . conf-no)
	('bcc-recpt . conf-no)
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


;;;(defun lyskom-edit-mode ()
;;;  "\\<lyskom-edit-mode-map>Mode for editing texts for LysKOM.
;;;Commands:
;;;\\[kom-edit-send]   sends the text when you are ready. The buffer will be
;;;          deleted if (and only if) the server accepts the text.
;;;\\[kom-edit-quit]   aborts the editing. You will get back to the LysKOM buffer.
;;;
;;;\\[kom-edit-show-commented]   shows the commented text in a temporary buffer.
;;;
;;;\\[kom-edit-add-recipient]   asks for another recipient and adds him to the header.
;;;\\[kom-edit-add-copy]   as \\[kom-edit-add-recipient] but adds him as copy-recipient.
;;;
;;;\\[kom-edit-insert-commented]   inserts the commented of footnoted text.
;;;\\[kom-edit-insert-text]   inserts the shown text, you tell the number."
;;;  (interactive)
;;;  (let ((tmp-keymap nil))
;;;    (kill-all-local-variables)
;;;    (text-mode)
;;;
;;;    (run-hooks 'lyskom-edit-mode-mode-hook)
;;;
;;;    (setq tmp-keymap (and (current-local-map)
;;;                          (copy-keymap (current-local-map))))
;;;
;;;    (lyskom-set-menus 'lyskom-edit-mode lyskom-edit-mode-map)
;;;    (setq mode-line-buffer-identification '("LysKOM (server: %b)"))
;;;    (setq major-mode 'lyskom-edit-mode)
;;;    (setq mode-name lyskom-edit-mode-name)
;;;
;;;    (if tmp-keymap
;;;        (let ((new-keymap (make-sparse-keymap)))
;;;          (make-local-variable 'lyskom-edit-mode-map)
;;;          (setq lyskom-edit-mode-map 
;;;                (lyskom-default-value 'lyskom-edit-mode-map))
;;;
;;;          (lyskom-xemacs-or-gnu
;;;           (set-keymap-parents new-keymap
;;;                               (list lyskom-edit-mode-map
;;;                                     tmp-keymap))
;;;           (progn (set-keymap-parent new-keymap lyskom-edit-mode-map)
;;;                  (lyskom-overlay-keymap lyskom-edit-mode-map
;;;                                         tmp-keymap
;;;                                         new-keymap)))
;;;          (use-local-map new-keymap))
;;;
;;;      (lyskom-use-local-map lyskom-edit-mode-map))
;;;    
;;;
;;;    (auto-save-mode 1)
;;;    (auto-fill-mode 1)
;;;    (make-local-variable 'paragraph-start)
;;;    (make-local-variable 'paragraph-separate)
;;;    (setq paragraph-start (concat "^" 
;;;                                  (regexp-quote 
;;;                                   (substitute-command-keys
;;;                                    (lyskom-get-string 'header-separator)))
;;;                                  "$\\|" paragraph-start))
;;;    (setq paragraph-separate (concat "^" 
;;;                                     (regexp-quote 
;;;                                      (substitute-command-keys
;;;                                       (lyskom-get-string 'header-separator)))
;;;                                     "$\\|" paragraph-separate))
;;;    (run-hooks 'lyskom-edit-mode-hook)))


(defun lyskom-edit-mode (&optional arg)
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

(defun kom-edit-send-anonymous ()
  "Send the text anonymously to the server."
  (interactive)
  (lyskom-edit-send 'initiate-create-anonymous-text nil))

(defun kom-edit-send ()
  "Send the text to the server."
  (interactive)
   (if (and (lyskom-default-value 'lyskom-is-anonymous)
            (lyskom-j-or-n-p 'do-send-anonymous t))
       (lyskom-edit-send 'initiate-create-anonymous-text t)
     (lyskom-edit-send 'initiate-create-text nil)))

(defun lyskom-edit-send (send-function &optional is-anonymous)
  "Send the text to the server by calling SEND-FUNCTION.
If optional IS-ANONYMOUS is non-nil, assume that the text is being submitted
anonymously and take actions to avoid revealing the sender."
  (condition-case err
      (if (or (not lyskom-edit-text-sent)
	      (j-or-n-p (lyskom-get-string 'already-sent)))
	  (progn 
	    (let ((buffer (current-buffer))
		  (headers nil)
                  (misc-list nil)
                  (subject nil)
                  (message nil)
                  (aux-list nil))

	      (save-excursion
		(setq headers (lyskom-edit-parse-headers)
		      misc-list (apply 'lyskom-create-misc-list 
                                       (elt headers 1))
                      aux-list (elt headers 2)
		      subject (car headers)))

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
                    (setq misc-list (apply 'lyskom-create-misc-list
                                           (nconc (elt headers 1)
						  extra-headers)))))

              ;;
              ;; Run user hooks
              ;; ####: ++++: FIXME: We should quit more graciously.

              (if (not (run-hook-with-args-until-failure 
                        'lyskom-send-text-hook))
                  (signal 'lyskom-edit-text-abort nil))
                                                
              ;;
              ;; Transform the message text
              ;;

	      (setq message
                    (if (fboundp lyskom-send-text-transform-function)
                        (funcall lyskom-send-text-transform-function
                                 (lyskom-edit-extract-text))
                      (lyskom-edit-extract-text)))

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
                                     (cdr (lyskom-mime-decode-content-type
                                           (aux-item->data (car content-type))))))
                       (mime-charset (lyskom-mime-string-charset full-message)))

                  ;; If the charset isn't already set, encode the string

                  (if (and mime-charset (null charset))
                      (setq full-message
                            (lyskom-mime-encode-string full-message))
                    (when (lyskom-j-or-n-p 'too-many-languages)
                      (keyboard-quit)))

                  ;; Add the charset data to the content type

                  (cond ((> (length content-type) 1)
                         (lyskom-error (lyskom-get-string 'too-many-content-types)))
                        ((null content-type)
                         (setq aux-list
                               (cons (lyskom-create-aux-item 0 1 nil nil
                                                             (lyskom-create-aux-item-flags nil nil nil nil nil nil nil nil)
                                                             0
                                                             (format "x-kom/text;charset=%S" mime-charset))
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
                           is-anonymous))))
            (lyskom-undisplay-buffer)
	    (goto-char (point-max))))
    ;;
    ;; Catch no-subject and other things
    ;;

    (lyskom-edit-text-abort
     (apply 'lyskom-message (cdr-safe err)))
    (lyskom-no-subject
     (lyskom-beep kom-ding-on-no-subject)
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

(eval-when-compile (defvar ispell-dictionary nil))
(eval-when-compile (defvar ispell-message-text-end nil))
(eval-when-compile (defvar ispell-message-start-skip nil))
(eval-when-compile (defvar ispell-message-end-skip nil))


(defun lyskom-ispell-text ()
  "Check spelling of the text body.
Put this in lyskom-send-text-hook"
  (kom-ispell-message)
  t)


(eval-when-compile
  (defvar ispell-dictionary nil)
  (defvar ispell-message-text-end nil)
  (defvar ispell-message-start-skip nil)
  (defvar ispell-message-end-skip nil))

(defun kom-ispell-message ()
  "Check spelling of the text.
kom-ispell-dictionary is the dictionary to use to check spelling.
Based on ispell-message."
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
      (let* ((internal-messagep 
              (save-excursion
                (re-search-forward
                 (concat "^"
                         (regexp-quote
                          (substitute-command-keys
                           (lyskom-get-string 'header-separator)))
                         "$")
                 nil t)))
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
         (author-list nil)
         (author-is-member nil)
         (text-stat nil)
         (collector (make-collector))
         (extra-headers nil)
         (buffer (current-buffer))
         (me (save-excursion (set-buffer lyskom-buffer)
                             lyskom-pers-no))
         (num-me 0)
         (num-real-recpt 0))
    (lyskom-ignore text-stat)              ; Have no idea if its ever used...

    ;;
    ;; List all texts this text is a comment to
    ;; List all recipients of the text
    ;;

    (lyskom-traverse misc (cdr misc-list)
      (cond ((eq (car misc) 'comm-to)
             (setq comm-to-list (cons (cdr misc)
                                      comm-to-list)))
            ((or (eq (car misc) 'recpt)
                 (eq (car misc) 'cc-recpt)
                 (eq (car misc) 'bcc-recpt))
             (when (eq (car misc) 'recpt)
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

    (when (save-excursion (set-buffer lyskom-buffer)
                           (cond ((null kom-check-for-new-comments) nil)
                                 ((functionp kom-check-for-new-comments)
                                  (funcall kom-check-for-new-comments
                                           buffer misc-list subject))
                                 (t t)))
      (lyskom-message "%s" (lyskom-format 'checking-comments))
      (save-excursion
        (set-buffer lyskom-buffer)
        (set-collector->value collector nil)
	
        (mapcar (function (lambda (text-stat)
                            (cache-del-text-stat text-stat)
                            (initiate-get-text-stat 'sending 
                                                    'collector-push
                                                    text-stat
                                                    collector)))
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
    

    (set-collector->value collector nil)

    (if (and (lyskom-default-value 'kom-confirm-multiple-recipients)
             (not (eq (lyskom-default-value 'kom-confirm-multiple-recipients)
                      'before))
             (> (- num-real-recpt num-me) 1))

        (save-excursion
          (goto-char (point-min))
          (if (not 
               (lyskom-j-or-n-p
                (lyskom-format 'comment-all-relevant-p) t))
              (signal 'lyskom-edit-text-abort 
                      (list "%s" 
                            (lyskom-get-string 
                             'please-edit-recipients))))))

    (if (and (lyskom-default-value 'kom-check-commented-author-membership)
             (assq 'comm-to (cdr misc-list)))
        (progn
          (lyskom-message "%s" (lyskom-get-string 'checking-rcpt))

          ;;
          ;; For each commented text, get the author
          ;;
        
          (mapcar 
           (lambda (x)
             (let ((text (blocking-do 'get-text-stat x)))
               (when text
                 (add-to-list 'author-list (text-stat->author text)))))
           comm-to-list)

          ;;
          ;; For each author, see if the author is a direct recipient
          ;; of the text. If so, there is no point in continuing.
          ;; (People can unsubscribe from their mailboxes, but if they
          ;; do, this code won't help anyway.)
          ;;

          (lyskom-traverse misc (cdr misc-list)
            (cond ((or (eq (car misc) 'recpt)
                       (eq (car misc) 'bcc-recpt)
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
                       (mapcar
                        (function
                         (lambda (conference-number)
                           (initiate-query-read-texts 
                            'sending
                            'collector-push
                            author-number conference-number
                            collector)))
                        recipient-list)
                       (lyskom-wait-queue 'sending)

                       ;; Now collector contains all the memberships for
                       ;; author-number in the recipients.

                       (let ((tmp (collector->value collector)))
                         (while tmp
                           (when (and (lyskom-membership-p (car tmp))
                                      (not (membership-type->passive
                                        (membership->type (car tmp)))))
                             (setq author-is-member t
                                   tmp nil))
                           (setq tmp (cdr tmp))))
                             
                       (if (and (not author-is-member)
				(not (zerop author-number))
                                (lyskom-is-permitted-author
                                 (blocking-do 'get-conf-stat author-number))
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
    
(defun lyskom-send-enriched (message)
  (condition-case err
      (let ((buf (lyskom-get-buffer-create 'lyskom-enriched 
                                           "lyskom-enriched" 
                                           t)))
        (unwind-protect
            (save-excursion
              (set-buffer buf)
              (insert message)
              (goto-char (point-min))
              (format-encode-buffer 'text/enriched)
              (goto-char (point-min))
              (search-forward "\n\n")
              (if (and (not (lyskom-string= (buffer-substring (point)
                                                              (point-max)) message))
                       (save-excursion
                         (set-buffer lyskom-buffer)
                         (lyskom-j-or-n-p 
                          (lyskom-get-string 'send-formatted) t)))
                  (concat "enriched:\n" (buffer-string))
                message))
          (kill-buffer buf)))
    (error (if (lyskom-j-or-n-p
                (lyskom-format (lyskom-get-string 'transform-error)
                               (error-message-string err)))
               message
             (signal 'lyskom-edit-text-abort nil)))))



(defun kom-edit-quit ()
  "Kill the text (if any) written so far and continue reading."
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


(defun kom-edit-insert-commented ()
  "Insert the commented text with '>' first on each line."
  (interactive)
  (lyskom-edit-get-commented 'lyskom-edit-insert-commented))


(defun kom-edit-insert-buglist ()
  "Insert the commented buglist, Roxen Internet Software style."
  (interactive)
  (lyskom-edit-get-commented 'lyskom-edit-insert-buglist))


(defun kom-edit-insert-digit-text ()
  (interactive)
  (setq unread-command-events (cons last-command-event unread-command-events))
  (call-interactively 'kom-edit-insert-text nil))


(defun kom-edit-insert-text (no)
  "Insert the text number NO with '>' first on each line."
  (interactive (list
		(cond
		 ((null current-prefix-arg)
		  (string-to-int 
		   (read-from-minibuffer 
		    (format "%s" (lyskom-get-string 'which-text-include)))))
		 ((prefix-numeric-value current-prefix-arg)))))
  (let ((buffer (current-buffer))
        (window (selected-window)))
    (set-buffer lyskom-buffer)
    (lyskom-collect 'edit)
    (initiate-get-text 'edit
                       nil
                       no)
    (initiate-get-text-stat 'edit
                            nil
                            no)
    (lyskom-use 'edit 'lyskom-edit-insert-commented buffer window)
    (set-buffer buffer)
    (sit-for 0)))
    

(defun lyskom-edit-get-commented (thendo)
  "Get the commented text and text stat and then do THENDO with it."
  (let ((p (point)))
    (save-excursion
      (let* ((buffer (current-buffer))
             (window (selected-window))
             (headers (condition-case nil
                          (elt (lyskom-edit-parse-headers) 1)
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
          (blocking-do-multiple ((text (get-text no))
                                 (text-stat (get-text-stat no)))
            (set-buffer buffer)
            (funcall thendo text text-stat buffer window)))
         (t
          (lyskom-message "%s" (lyskom-get-string 'no-such-text-m))))))
    (sit-for 0)))


;;; ============================================================
;;; Info node stuff

(defvar Info-current-node)
(defvar Info-current-file)

(defun kom-yank-info-nodename ()
  "Put the current Info-node on the kill-ring."
  (interactive)
  (kill-new (format "*Note %s: (%s)%s,"
		    Info-current-node
		    (file-name-nondirectory Info-current-file)
		    Info-current-node)))

(defun kom-insert-last-info-nodename ()
  "Insert a reference to the most recently visited info node."
  (interactive)
  (condition-case nil
      (let ((link nil))
        (save-excursion
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
  "Adds a text as commented to the text being edited."
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
       (lyskom-error "%s" (lyskom-get-string 'no-such-text-m))))))


(defun kom-edit-add-recipient ()
  "Adds a conference as recipient to the text being edited."
  (interactive)
  (lyskom-edit-add-recipient/copy (lyskom-get-string 'added-recipient)
                                 nil
                                  'recpt))


(defun kom-edit-add-bcc ()
  "Adds a conference as bcc recipient to the text being edited."
  (interactive)
  (lyskom-edit-add-recipient/copy (lyskom-get-string 'added-blank-carbon-copy)
                                  nil
                                  'bcc-recpt))


(defun kom-edit-add-copy ()
  "Adds a conference to which a copy of the edited text will be sent."
  (interactive)
  (lyskom-edit-add-recipient/copy (lyskom-get-string 'added-carbon-copy)
                                  nil
                                  'cc-recpt))

(defun kom-edit-move-text ()
  "Adds a conference as a recipient, and changes all other recipients to
CC recipients."
  (interactive)
  (lyskom-edit-add-recipient/copy (lyskom-get-string 'who-to-move-to-q)
                                  'lyskom-edit-move-recipients))


(defun lyskom-edit-move-recipients (conf-stat insert-at edit-buffer)
  (save-excursion
    (set-buffer edit-buffer)
    (let* ((tmp (lyskom-edit-parse-headers))
           (subject (car tmp))
           (miscs (mapcar (lambda (x) (if (eq (car x) 'recpt) 
                                          (cons 'cc-recpt (cdr x)) x))
                  (cdr (lyskom-edit-translate-headers (elt tmp 1)))))
           (aux-list (elt tmp 2))
           (elem nil))


      ;; If the new target is already a recipient, convert it to the right 
      ;; kind. Otherwise insert the new target after the last comm-to

      (setq elem (lyskom-edit-find-misc miscs '(recpt cc-recpt bcc-recpt) 
                                        (conf-stat->conf-no conf-stat)))
      (if elem
          (setcar elem 'recpt)
        (lyskom-insert-in-list
         (cons 'recpt (conf-stat->conf-no conf-stat))
         miscs
         (car (cdr (memq (lyskom-edit-find-misc miscs '(footn-to comm-to) 
                                                nil t)
                         miscs)))))

      (lyskom-edit-replace-headers subject (cons 'MISC-LIST miscs) aux-list))))
                                  


(defun lyskom-edit-do-add-recipient/copy (recpt-type recpt-no edit-buffer)
  (lyskom-save-excursion
    (set-buffer edit-buffer)
    (let* ((headers (lyskom-edit-parse-headers))
           (miscs (lyskom-edit-translate-headers (elt headers 1)))
           (elem (lyskom-edit-find-misc miscs '(cc-recpt bcc-recpt recpt)
                                        recpt-no)))

      (cond (elem (setcar elem recpt-type))
            (t (setq miscs
                     (append miscs (list (cons recpt-type recpt-no))))))
      (lyskom-edit-replace-headers (elt headers 0)
                                   miscs
                                   (elt headers 2)))))
           
    


(defun lyskom-edit-add-recipient/copy (prompt 
                                       &optional what-to-do recpt-type)
  "Adds a new recipient or a cc-recipient to the text which is being edited.
PROMPT is the prompt to use to ask the user for a recipient.
WHAT-TO-DO is a function to call to do the insertion.
RECPT-TYPE is the type of recipient to add."
  (let ((edit-buffer (current-buffer))
	(insert-at (point-min-marker))
	(conf-stat (lyskom-read-conf-stat prompt '(all) nil "" t)))
    (lyskom-save-excursion
     (save-excursion
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

         (when (and (eq recpt-type 'recpt)
                    (not (lyskom-j-or-n-p (lyskom-format
                                           'really-add-as-recpt-q
                                           conf-stat) t)))
           (setq recpt-type 'cc-recpt))

         (if what-to-do
           (funcall what-to-do conf-stat insert-at edit-buffer)
           (lyskom-edit-do-add-recipient/copy recpt-type
                                              (conf-stat->conf-no conf-stat)
                                              edit-buffer)))
       (when win-config (set-window-configuration win-config)))))))

(defun kom-edit-add-cross-reference ()
  (interactive)
  (let* ((completions (list (cons (lyskom-get-string 'conference) 'conf)
                            (cons (lyskom-get-string 'person) 'pers)
                            (cons (lyskom-get-string 'text) 'text)))
         (completion-ignore-case t)
         (type (cdr (lyskom-string-assoc
                     (completing-read (lyskom-get-string 'xref-type)
                                      completions nil t)
                     completions)))
         (obj nil)
         (prompt nil)
         (item nil))
    (cond 
     ((eq type 'text)
      (setq prompt (lyskom-get-string 'which-text-to-xref))
      (while (null obj)
        (setq obj (blocking-do 'get-text-stat
                               (lyskom-read-number prompt)))
        (setq prompt (lyskom-get-string 'which-text-to-xref-err )))
      (setq item
            (lyskom-create-aux-item 0 3 0 0 
                                    (lyskom-create-aux-item-flags
                                     nil nil nil nil nil nil nil nil)
                                    0 (format "T%d"
                                              (text-stat->text-no obj)))))

     ((eq type 'conf)
      (setq prompt (lyskom-get-string 'which-conf-to-xref))
      (while (null obj)
        (setq obj (lyskom-read-conf-no prompt '(conf) nil nil t))
        (setq item
              (lyskom-create-aux-item 0 3 0 0
                                      (lyskom-create-aux-item-flags
                                       nil nil nil nil nil nil nil nil)
                                      0 (format "C%d" obj)))))
        
     ((eq type 'pers)
      (setq prompt (lyskom-get-string 'which-pers-to-xref))
      (while (null obj)
        (setq obj (lyskom-read-conf-no prompt '(pers) nil nil t))
        (setq item
              (lyskom-create-aux-item 0 3 0 0
                                      (lyskom-create-aux-item-flags
                                       nil nil nil nil nil nil nil nil)
                                      0 (format "P%d" obj)))))
        
     (t nil))

    (lyskom-edit-insert-aux-item item)))

(defun kom-edit-add-read-confirm-request ()
  (interactive)
  (lyskom-edit-insert-aux-item
   (lyskom-create-aux-item 0 6 0 0
                           (lyskom-create-aux-item-flags
                            nil nil nil nil nil nil nil nil)
                           0 "")))

(defun kom-edit-add-no-comments ()
  (interactive)
  (lyskom-edit-insert-aux-item
   (lyskom-create-aux-item 0 4 0 0
                           (lyskom-create-aux-item-flags
                            nil nil nil nil nil nil nil nil)
                           0 "")))

(defun kom-edit-add-personal-comments ()
  (interactive)
  (lyskom-edit-insert-aux-item
   (lyskom-create-aux-item 0 5 0 0
                           (lyskom-create-aux-item-flags
                            nil nil nil nil nil nil nil nil)
                           0 "")))
  
    



(defun lyskom-edit-insert-aux-item (item)
  "Insert the aux item ITEM in the current buffer"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat "^"
             (regexp-quote
              (substitute-command-keys
               (lyskom-get-string 'header-separator)))
             "$")
     nil t)
    (beginning-of-line)
    (forward-line -1)
    (insert
     (concat (lyskom-format
              (format "%%#1@%%[%s%%]%%#2s" (lyskom-get-string 'aux-item-prefix))
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
  (lyskom-save-excursion
    (set-buffer (car arg))
    (goto-char (cdr arg))
    (lyskom-edit-toggle-aux-item-flag buf arg text 'secret)))

(defun lyskom-edit-toggle-anonymous-aux (buf arg text)
  (interactive)
  (save-excursion
    (set-buffer (car arg))
    (goto-char (cdr arg))
    (lyskom-edit-toggle-aux-item-flag buf arg text 'anonymous)))

(defun lyskom-edit-toggle-inherit-aux (buf arg text)
  (interactive)
  (save-excursion
    (set-buffer (car arg))
    (goto-char (cdr arg))
    (lyskom-edit-toggle-aux-item-flag buf arg text 'inherit)))

(defun lyskom-edit-delete-aux (buf arg text)
  (interactive)
  (save-excursion
    (set-buffer (car arg))
    (goto-char (cdr arg))
    (beginning-of-line)
    (delete-region (point) (save-excursion (forward-line 1) (point)))))


(defun lyskom-edit-insert-aux-item-flags (flags)
  (save-excursion
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
          (end-of-line)
          (insert (format " [%s]" str))))))


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
          (end nil))
      (goto-char (point-min))
      (setq start (point-marker))
      (set-marker-insertion-type start t)
      (search-forward (substitute-command-keys
                       (lyskom-get-string 'header-separator)))
      (end-of-line)
      (setq end (point-marker))
      (goto-char (point-min))
      (lyskom-edit-insert-miscs misc-list subject "" aux-list)
      (delete-region start end)
      (goto-char end)
      (delete-char 1))))

(defun lyskom-looking-at-header (header match-number)
  "Check if point is at the beginning of a header of type HEADER.
Return the corresponding number (conf no etc.) if MATCH-NUMBER is
non-nil. If MATCH-NUMBER is 'angled, only match a number inside <>."
  (if (lyskom-looking-at
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
  (let ((misc nil)
        (subject nil)
        (aux nil))
    (save-restriction
      ;; Narrow to headers
      (search-forward (substitute-command-keys
		       (lyskom-get-string 'header-separator)))
      (beginning-of-line)
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(let ((case-fold-search t)
	      (n nil))
	  (cond
	   ((setq n (lyskom-looking-at-header 'recipient-prefix 'angled))
	    (setq misc (nconc misc (list 'recpt n))))
	   ((setq n (lyskom-looking-at-header 'carbon-copy-prefix 'angled))
	    (setq misc (nconc misc (list 'cc-recpt n))))
	   ((setq n (lyskom-looking-at-header 'blank-carbon-copy-prefix
                                              'angled))
	    (setq misc (nconc misc (list 'bcc-recpt n))))
	   ((setq n (lyskom-looking-at-header 'comment-prefix t))
	    (setq misc (nconc misc (list 'comm-to n))))
	   ((setq n (lyskom-looking-at-header 'footnote-prefix t))
	    (setq misc (nconc misc (list 'footn-to n))))
	   ((lyskom-looking-at-header 'header-subject nil)
	    (setq subject (lyskom-edit-extract-subject)))

           ((lyskom-looking-at (lyskom-get-string 'aux-item-prefix))
            (goto-char (match-end 0))
            (let ((item (lyskom-edit-parse-aux-item)))
              (if item
                  (setq aux (cons item aux))
                (signal 'lyskom-unknown-header
                        (list 'unknown-header (point))))))

	   (t (signal 'lyskom-unknown-header (list 'unknown-header (point))))))
	(forward-line 1)))
    (list subject misc aux)))

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
    (goto-char (point-min))
    (if (not (search-forward 
              (substitute-command-keys
               (lyskom-get-string 'header-separator)) 
              nil (point-max)))
	(signal 'lyskom-internal-error
		"Altered lyskom-header-separator line.")
      (buffer-substring (1+ (point))
			(progn
			  (goto-char (1- (point-max)))
			  (while (lyskom-looking-at "\\s-")	; remove trailing
			    (backward-char 1))		; whitespace
			  (forward-char 1)
			  (point))))))


(defun lyskom-edit-on-aux-item ()
  "Return non-nil if point is on the same line as an aux-item"
  (save-excursion
    (beginning-of-line)
    (and (lyskom-looking-at (lyskom-get-string 'aux-item-prefix))
         (search-forward 
          (substitute-command-keys
           (lyskom-get-string 'header-separator)) 
          nil (point-max)))))


(defun lyskom-create-text-handler (text-no edit-buffer 
                                           &optional is-anonymous)
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
   (is-anonymous
    (lyskom-format-insert-before-prompt 'text-created-anonymous text-no))
   (t
    (lyskom-insert-before-prompt
     (lyskom-format 'text-created  text-no))

    ;; Immediately mark the text as read if kom-created-texts-are-read is set
    ;; and we are not sending the text anonymously.
    
    (cond
     ((and kom-created-texts-are-read
           (not is-anonymous))
      (lyskom-is-read text-no)
      (initiate-get-text-stat 'background 'lyskom-mark-as-read
			      text-no)
      (lyskom-run 'background 'set 'lyskom-dont-change-prompt nil)
      (lyskom-run 'background 'lyskom-set-mode-line))
     (t					; Probably not necessary
      (setq lyskom-dont-change-prompt nil)))
    
    (set-buffer edit-buffer)		;Need local variables.
    (lyskom-edit-sent-mode 1)

    ;; Record the text number

    (lyskom-setq-default lyskom-last-written text-no)


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
)))


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
             (save-excursion (set-buffer buf)
                             (erase-buffer)
                             (lyskom-view-text (text->text-no text))
                             (set-buffer-modified-p nil)
                             (lyskom-view-mode)))))))


(defun lyskom-edit-insert-commented (text text-stat editing-buffer window)
  "Handles the TEXT and TEXT-STAT from the return of the call of the text.
The text is inserted in the buffer with '>' first on each line."
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
          (insert te)
          (while (<= pb (point))
            (beginning-of-line)
            (insert (or (lyskom-default-value 'kom-cite-string) 62))
            (forward-line -1)
            )))
    (lyskom-message "%s" (lyskom-get-string 'no-get-text))))


;;; ========================================================================
;;;   Treat the commented text as a Roxen Internet Software-style buglist,
;;;   handling the removal of finished subjects, last-message changes et c

;;; Author: Johan Sundström

(defun lyskom-edit-insert-buglist (text text-stat editing-buffer window)
  "Handles the TEXT and TEXT-STAT from the return of the call of the text.
The commented text is inserted in the buffer in the Roxen Internet Software
buglist style, automating the removal of finished subjects and change-marks."
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
          (insert te)
          (while (re-search-backward "^[ \t]*[!*X][ \t(]*\\[" nil t)
            (replace-regexp "^\\([ \t]*\\)[!*X]\\([ \t(]*\\[\\)" "\\1 \\2"))
	  (goto-char pb)
          (while (re-search-forward "^[ \t([]*\\[[^\\/ ]\\]" nil t)
	    (beginning-of-line)
	    (let* ((df (point))
		   (dt (re-search-forward "^\\([^ \t][^ \t]\\|[ \t([]*\\[[\\/ ]\\]\\)" 
					  nil t)))
	      (if dt
		  (progn
		    (goto-char dt)
		    (beginning-of-line)
		    (if (thing-at-point-looking-at "^[^ \t]")
			(progn
			  (beginning-of-line)
			  (forward-char -1))))
		(goto-char (point-max)))
	      (delete-region df (point))))))
    (lyskom-message "%s" (lyskom-get-string 'no-get-text))))



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
