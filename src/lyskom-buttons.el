;;;;;
;;;;; $Id: lyskom-buttons.el,v 44.4 1996-10-08 02:57:55 davidk Exp $
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
;;;; File: lyskom-buttons.el
;;;; Author: David Byers
;;;;
;;;;


(defun lyskom-add-button-action (type text func)
  "Add a new action to the popup menu for a class of objects.
Arguments are TYPE, the type of object to adjust, TEXT the menu text
for the action and FUNC, the function to call when the action is
selected. By default TYPE may be any one of text, conf, pers or url 
although users can add other types.

FUNC must be a function with three arguments, BUFFER, ARGUMENT and
TEXT.  BUFFER is the LysKOM buffer that the command should use, TEXT
is the text of the selected button and ARGUMENT is the data argument
associated with the object. For button type text it is a text
number. For types conf and pers it is the conference number for the
object. For URLs it is the text of the URL (a string) or NIL. For
other (user-defined) types, it is a string.

For more information on button types and arguments, see the
documentation for the variable lyskom-text-buttons."
  (nconc (nth 3 (assq type lyskom-button-actions))
	 (list (cons text func))))

(defun lyskom-set-default-button-action (type func)
  "Set the default action for buttons of the type TYPE to FUNC. 

FUNC must be a valid button action function. For more information
on such functions see the documentation for lyskom-add-button-action."
  (let ((el (assq type lyskom-button-actions)))
    (setcdr (nthcdr 1 el)
	    (cons func (nthcdr 3 el)))))
      


(defun kom-previous-link (num)
  "Move the cursor to the previous active area in the LysKOM buffer."
  (interactive "p")
  (while (> num 0)
    (let ((where (previous-single-property-change (point) 
                                                  'lyskom-button-text)))
      (if where
          (progn
            (if (not (get-text-property where 'lyskom-button-text))
                (setq where (previous-single-property-change 
                             where
                             'lyskom-button-text)))
            (if where
                (goto-char where)
              (goto-char (point-min))
              (setq num 1)))))
    (setq num (1- num))))

(defun kom-next-link (num)
  "Move the cursor to the next active area in the LysKOM buffer."
  (interactive "p")
  (while (> num 0)
    (let ((where (next-single-property-change (point) 'lyskom-button-text)))
      (if where
          (progn
            (if (not (get-text-property where 'lyskom-button-text))
                (setq where (next-single-property-change where
                                                         'lyskom-button-text)))
            (if where
                (goto-char where)
              (goto-char (point-max))
              (setq num 1)))))
    (setq num (1- num))))


(defun kom-key-mouse-2 ()
  "Simulate a mouse button press at point."
  (interactive)
  (lyskom-mouse-2 (point)))

(defun kom-mouse-2 (click)
  "Execute the default action of the active area under the mouse."
  (interactive "@e")
  (let ((start (event-start click)))
    (lyskom-mouse-2 (car (cdr start)))))

(defun kom-mouse-3 (event)
  "Pop up a menu of actions to be taken at the active area under the mouse."
  (interactive "@e")
  (let ((pos (posn-point (event-start event))))
    (if (get-text-property pos 'lyskom-button-type)
	(lyskom-button-menu pos event)
      (lyskom-background-menu pos event))))
  
(defun kom-mouse-null (event)
  "Do nothing."
  ;; This is here to pervent unwanted events when clicking mouse-3
  (interactive "e"))

(defun lyskom-make-button-menu (title entries)
  "Create a menu keymap from a list of button actions."
  ;; Use the command as the event for simplicity.
  (append (list 'keymap title)
	  (mapcar '(lambda (entry) (cons (cdr entry) entry))
		  entries)))

(defun lyskom-button-menu (pos event)
  "Internal function used by kom-mouse-3"
  (let* ((type  (get-text-property pos 'lyskom-button-type))
         (arg   (get-text-property pos 'lyskom-button-arg))
         (text  (get-text-property pos 'lyskom-button-text))
         (buf   (get-text-property pos 'lyskom-buffer))
         (data  (assq type lyskom-button-actions))
         (title (lyskom-format 
                 (lyskom-get-string
                  (or (intern-soft (concat (symbol-name type)
                                           "-popup-title"))
                      'generic-popup-title))
                 text))
         (actl  (or (and data (elt data 3)) nil)))
    (cond ((null data) (goto-char pos))
          ((null actl) (goto-char pos))
          ((null buf) (goto-char pos))
          ((null (get-buffer buf))
           (lyskom-message "%s" (lyskom-get-string 'no-such-buffer)))
          (t         
           (if (symbolp title) (setq title (lyskom-get-string title)))
           (set-buffer buf)

	   ;; There is a simple bug in x-popup-menu which causes menus
	   ;; from simple keymaps to be title-less. A list consisting
	   ;; of a single keymap works better. A patch is submittet to
	   ;; the GNU folks. /davidk
           (let* ((menu (lyskom-make-button-menu title actl))
		  (result (x-popup-menu event (list menu)))
		  ;; We trust that the menu is really simple.
		  (command (car result))
		  )
	     ;; If mouse-3 is bound to its default
	     ;; mouse-save-than-kill, we will get an extra mouse event
	     ;; when the user clicks to get a menu that stays up. So
	     ;; we bind mouse-3 to a dummy function. Unfortunately it
	     ;; doesn't work completely.
             (if command
                 (funcall command buf arg text)))))))
         

(defun lyskom-mouse-2 (pos)
  "Execute the default action of the active area at POS if any."
  (let* ((type (get-text-property pos 'lyskom-button-type))
         (arg  (get-text-property pos 'lyskom-button-arg))
         (text (get-text-property pos 'lyskom-button-text))
         (buf  (get-text-property pos 'lyskom-buffer))
         (hint (get-text-property pos 'lyskom-button-hint))
         (data (assq type lyskom-button-actions))
         (act  (or (and kom-use-button-hints hint)
                   (and data (elt data 2)))))
                 
    (cond ((null act) (goto-char pos))
          ((null buf) (goto-char pos))
          ((and buf (null (get-buffer buf))) 
           (lyskom-message "%s" (lyskom-get-string 'no-such-buffer)))
          (t (and buf (set-buffer buf))
             (funcall act
                      buf
                      arg
                      text)))))
                      


(defun lyskom-button-transform-text (text)
  "Add text properties to the string TEXT according to the definition of
lyskom-text-buttons. Returns the modified string."
  (let ((blist lyskom-text-buttons)
        (start 0)
        (el nil))
    (while blist
      (setq el (car blist))
      (setq start 0)
      (while (string-match (elt el 0) text start)
        (add-text-properties 
         (match-beginning (or (elt el 2) 0))
         (match-end (or (elt el 2) 0))
         (cond ((eq (elt el 1) 'text)
                (lyskom-generate-button 'text
                                        (lyskom-button-get-arg el text)
                                        (lyskom-button-get-text el text)
                                        (lyskom-button-get-face el)))
               ((eq (elt el 1) 'conf)
                (lyskom-generate-button 'conf
                                        (lyskom-button-get-arg el text)
                                        (lyskom-button-get-text el text)
                                        (lyskom-button-get-face el)))
               ((eq (elt el 1) 'pers)
                (lyskom-generate-button 'pers
                                        (lyskom-button-get-arg el text)
                                        (lyskom-button-get-text el text)
                                        (lyskom-button-get-face el)))
               ((eq (elt el 1) 'url)
                (lyskom-generate-button 'url
                                        nil
                                        (lyskom-button-get-text el text)
                                        (lyskom-button-get-face el)))
               (t nil))
         text)
        (setq start (match-end 0)))
      (setq blist (cdr blist))))
  text)

(defun lyskom-button-get-arg (el text)
  "Get the button argument for button type EL from TEXT according to
the current match-data."
  (let ((no (or (elt el 3) 0)))
    (substring text (match-beginning no) (match-end no))))

(defun lyskom-button-get-text (el text)
  "Get the button text for button type EL from TEXT according to
the current match-data."
  (let ((no (or (elt el 2) 0)))
    (substring text (match-beginning no) (match-end no))))

(defun lyskom-button-get-face (el)
  "Get the button face for button type EL from TEXT according to
the current match-data."
  (elt el 4))


(defun lyskom-get-button-hint (hints)
  "Get the hint to be used right now (if any) from HINTS"
  (let ((result nil)
        (hint nil))
    (while (and hints (null result))
      (setq hint (car hints))
      (setq hints (cdr hints))
      (cond ((null (car hint)) 
             (if (and (eq lyskom-current-function
                          (elt hint 1))
                      (or (null (elt hint 2))
                          (eq lyskom-current-function-phase
                              (elt hint 2))))
                 (setq result (elt hint 3))))
              ((listp (car hint))
               (if (and lyskom-executing-command
                        (memq lyskom-current-command (car hint)))
                   (setq result (cdr hint))))
              ((symbolp (car hint))
               (if (and lyskom-executing-command
                        lyskom-current-command
                        (eq lyskom-current-command (car hint)))
                   (setq result (cdr hint))))))
    result))


	
(defun lyskom-generate-button (type arg &optional text face)
  "Generate the properties for a button of type TYPE with argument ARG.
Optional argument TEXT is the button text to be saved as a property and
FACE is the default text face for the button."
  (let* ((persno (cond ((boundp 'lyskom-pers-no) lyskom-pers-no)
                       ((and (boundp 'lyskom-buffer) lyskom-buffer)
                        (save-excursion
                          (set-buffer lyskom-buffer)
                          lyskom-pers-no))
                       (t -1)))
         (numarg (cond ((numberp arg) arg)
                       ((stringp arg) (string-to-number arg))
                       (t nil)))
         (data (assq type lyskom-button-actions))
         (hints (and data (elt data 4)))
         (the-hint (lyskom-get-button-hint hints))
         (props 
          (cond ((and (or (eq type 'conf) 
                          (eq type 'pers))
                      numarg)
                 (list 'face 
                       (or face
                           (cond ((eq persno numarg) 'kom-me-face)
                                 ((memq numarg kom-friends) 'kom-friends-face)
                                 (t 'kom-active-face)))
                       'mouse-face 'kom-highlight-face
                       'lyskom-button-text text
                       'lyskom-button-type type
                       'lyskom-button-arg numarg
                       'lyskom-buffer lyskom-buffer))
                ((and (eq type 'text) numarg)
                 (list 'face (or face 'kom-text-no-face)
                       'mouse-face 'kom-highlight-face
                       'lyskom-button-text text
                       'lyskom-button-type type
                       'lyskom-button-arg numarg
                       'lyskom-buffer lyskom-buffer))
                ((eq type 'url)
                 (list 'face (or face 'kom-active-face)
                       'mouse-face 'kom-highlight-face
                       'lyskom-button-text text
                       'lyskom-button-type type
                       'lyskom-button-arg arg
                       'lyskom-buffer lyskom-buffer))
                (t
		 (list 'face (or face 'kom-active-face)
		       'mouse-face 'kom-highlight-face
		       'lyskom-button-text text
		       'lyskom-button-type type
		       'lyskom-button-arg arg
		       'lyskom-buffer lyskom-buffer)))))

    (append (list 'rear-nonsticky t)
	    (if the-hint
		(cons 'lyskom-button-hint
		      (cons the-hint props))
	      props))))
      
        
           

(defun lyskom-default-button (type arg)
  "Generate a button of type TYPE from data in ARG. ARG can be almost any
type of data and is converted to the proper argument type for buttons of
type TYPE before being send to lyskom-generate-button."
  (and kom-text-properties
       (let (xarg text)
	 (cond ((eq type 'conf)
		(cond ((lyskom-conf-stat-p arg)
		       (if (conf-type->letterbox (conf-stat->conf-type arg))
			   (setq type 'pers))
		       (setq xarg (conf-stat->conf-no arg)
			     text (conf-stat->name arg)))
		      ((numberp arg) (setq text "" xarg arg))
		      (t (setq text "" xarg 0))))
	       ((eq type 'pers)
		(cond ((lyskom-conf-stat-p arg)
		       (setq xarg (conf-stat->conf-no arg)
			     text (conf-stat->name arg)))
		      ((lyskom-pers-stat-p arg)
		       (setq xarg (pers-stat->pers-no arg)
			     text ""))
		      ((numberp arg) (setq text "" xarg arg))
		      (t (setq text "" xarg 0))))
	       ((eq type 'text)
		(cond ((stringp arg) (setq xarg (string-to-number arg)
					   text arg))
		      ((numberp arg) (setq xarg arg
					   text (number-to-string arg)))
		      ((lyskom-text-stat-p arg)
		       (setq xarg (text-stat->text-no arg)
			     text (number-to-string (text-stat->text-no arg))))
		      (t (setq xarg 0 text ""))))
	       ((eq type 'url)
		(cond ((stringp arg) (setq xarg nil text arg))
		      (t (setq xarg nil text ""))))
	       (t (setq xarg arg text "")))
	 (lyskom-generate-button type xarg text nil))))
                  

           


;;;========================================
;;; Button actions
;;;

(defun lyskom-button-view-text (buf arg text)
  "In the LysKOM buffer BUF, view the text ARG. Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-view arg))))


(defun lyskom-button-review-tree (buf arg text)
  "In the LysKOM buffer BUF, view the text ARG. Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
	(t (pop-to-buffer buf)
	   (kom-review-tree arg))))

(defun lyskom-button-find-root (buf arg text)
  "In the LysKOM buffer BUF, view the text ARG. Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
	(t (pop-to-buffer buf)
	   (kom-find-root arg))))

(defun lyskom-button-comment-text (buf arg text)
  "In the LysKOM buffer BUF, comment the text ARG. Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-write-comment arg))))

(defun lyskom-button-private-comment-text (buf arg text)
  "In the LysKOM buffer BUF, write a private comment the text ARG. 
Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-private-answer arg))))

(defun lyskom-button-mark-text (buf arg text)
  "In the LysKOM buffer BUF, mark the text ARG. Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (unwind-protect
               (progn
                 (pop-to-buffer buf)
                 (lyskom-start-of-command 'kom-mark-text)
                 (lyskom-mark-text arg nil 1))
             (lyskom-end-of-command)))))

(defun lyskom-button-unmark-text (buf arg text)
  "In the LysKOM buffer BUF, unmark the text ARG. Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (unwind-protect
               (progn
                 (pop-to-buffer buf)
                 (lyskom-start-of-command 'kom-unmark-text)
                 (lyskom-mark-text arg nil 0))
             (lyskom-end-of-command)))))
  

(defun lyskom-button-view-conf-presentation (buf arg text)
  "In the LysKOM buffer BUF, view the presentation of ARG. 
Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-review-presentation arg))))

(defun lyskom-button-view-conf-status (buf arg text)
  "In the LysKOM buffer BUF, view the status of conference ARG. Last argument
TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-status-conf arg))))

(defun lyskom-button-goto-conf (buf arg text)
  "In the LysKOM buffer BUF, go to the conference ARG. Last argument TEXT is
ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-go-to-conf arg))))

(defun lyskom-button-add-self (buf arg text)
  "In the LysKOM buffer buf, add self to conference ARG."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-add-self arg))))

(defun lyskom-button-sub-self (buf arg text)
  "In the LysKOM buffer buf, sub self from conference ARG."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-sub-self arg))))

(defun lyskom-button-view-pers-presentation (buf arg text)
  "In the LysKOM buffer BUF, view the presentation of person ARG. 
Last argument TEXT is ignored.
This is a LysKOM button action."
  (lyskom-button-view-conf-presentation buf arg text))

(defun lyskom-button-view-pers-status (buf arg text)
  "In the LysKOM buffer BUF, view the status of person ARG. 
Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-status-person arg))))

(defun lyskom-button-mail (buf arg text)
  "In the LysKOM buffer BUF, send mail to the conference ARG.
Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-send-letter arg))))

(defun lyskom-button-send-message (buf arg text)
  "In the LysKOM buffer BUF, send a personal message to person ARG."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-send-message arg nil))))

(defun lyskom-button-copy-url (but arg text)
  "In the LysKOM buffer BUF, ignore ARG and copy TEXT to the kill ring.
This is a LysKOM button action."
  (kill-new text))


(defun lyskom-button-open-url (buf arg text)
  "In the LysKOM buffer BUF, ignore ARG and open TEXT as an URL.
This is a LysKOM button action."
  (let* ((url text)
         protocol
         url-manager)
    (string-match ":" url)
    (setq protocol (substring url 0 (match-beginning 0)))
    (setq url-manager (lyskom-get-url-manager protocol))
    (if (null url-manager)
        (lyskom-error "Can't find URL viewer"))
    (goto-char (point-max))
    (funcall (elt url-manager 3) url url-manager)))



;;;
;;;	LysKOM URL Management
;;;

(defun lyskom-get-url-manager (protocol)
  "Get the URL manager for PROTOCOL (a string). Returns a function."
  (let ((managers kom-url-managers)
	(preferences kom-url-viewer-preferences)
	(result nil))
    (while (and preferences (not result))
      (setq managers kom-url-managers)
      (while (and managers (not result))
	(if (and (string-match (car (car managers))
			       (car preferences))
		 (string-match (car (cdr (car managers)))
			       protocol))
	    (setq result (car managers)))
	(setq managers (cdr managers)))
      (setq preferences (cdr preferences)))
    result))



(defun lyskom-url-manager-starting (manager)
  "Tell the user that the URL manager MANAGER is starting."
  (lyskom-message "%s" (lyskom-format (lyskom-get-string 'starting-program)
				 (elt manager 2))))


(defun lyskom-view-url-w3 (url manager)
  "View the URL URL using W3. Second argument MANAGER is ignored."
  (w3-fetch url))

(defun lyskom-view-url-dired (url manager)
  "View the URL URL using dired. Second argument MANAGER is ignored."
  (if (not (and (string-match
		 "\\(file\\|ftp\\)://\\([^/:]*\\)\\(:[0-9]*\\)?\\(/\\|$\\)"
		 url)
		(match-beginning 0)
		(match-beginning 1)
		(match-beginning 2)))
      (lyskom-error "Bad URL"))
  (let ((host (substring url (match-beginning 2) (match-end 2)))
	(path (substring url (match-end 0)))
	(user "anonymous"))
    (if  (string-match ";type=.$" path)
        (setq path (substring path 0 (match-beginning 0))))
    (if (and (string-match "\\([^@]*\\)@" host)
	     (match-beginning 1))
	(progn
	  (setq user (substring host (match-beginning 1) (match-end 1)))
	  (setq host (substring host (match-end 0)))))
    (cond ((string= host "localhost") (find-file path))
          (t (find-file (concat "/" user "@" host ":/" path))))
    ;;    (message "%s  %s  %s" user host path)
    ))



(defun lyskom-view-url-telnet (url manager)
  "View the URL URL using telnet. Second argument MANAGER is ignored."
  (if (not (and (string-match 
                 "telnet://\\([^@]*@\\)?\\([^/:]*\\)\\(:[0-9]*\\)?"
		 url)
		(match-beginning 0)
		(match-beginning 2)))
      (lyskom-error "Bad URL"))
  (let ((host (substring url (match-beginning 2) (match-end 2)))
	(port (if (match-beginning 3)
		  (substring url (1+ (match-beginning 3)) (match-end 3))
		""))
	(user (if (match-beginning 1)
                  (substring url (match-beginning 1) (1- (match-end 1)))
                nil))
        ;; (password nil)
	)
    (if (and user
             (string-match "^\\([^:]*\\):\\(.*\\)" user))
        (progn
	  ;; (setq password (substring user
	  ;; 			    (match-beginning 2)
	  ;; 			    (match-end 2)))
	  (setq user (substring user 
				(match-beginning 1)
				(match-end 1)))))
    
    (telnet (concat host " " port))
    ;;    (message "u:%s  p:%s  h:%s  #:%s" 
    ;;             (or user "<u>")
    ;;             (or password "<p>")
    ;;             (or host "<h>")
    ;;             (or port "<#>"))

    ))


(defun lyskom-view-url-mailmode (url manager)
  "View the URL URL using mail in Emacs. The second argument MANAGER is ignored."
  (if (not (and (string-match "mailto:\\([^@]+@.*\\)$" url)
		(match-beginning 1)))
      (lyskom-error "Bad URL"))
  (mail nil (substring url (match-beginning 1) (match-end 1))))


(defun lyskom-view-url-netscape (url manager)
  "View the URL URL using Netscape Navigator. The second argument
MANAGER is the URL manager that started Netscape.

This function attempts to load the URL in a running Netscape, but failing
that, starts a new one."
  (let* ((proc (apply 'start-process "netscape"
                      nil
                      (if (listp kom-netscape-command)
                          (car kom-netscape-command)
                        kom-netscape-command)
                      (if (listp kom-netscape-command)
                          (append (cdr kom-netscape-command)
                                  (list "-remote"
                                        (format "openUrl(%s)" url)))
                        (list "-remote"
                              (format "openUrl(%s)" url)))))
         (status 'run)
         (exit nil))
    (lyskom-url-manager-starting manager)
    (while (eq status 'run)
      (accept-process-output)
      (setq status (process-status proc)))
    (setq exit (process-exit-status proc))
    (cond ((and (eq status 'exit) 
                (eq exit 1))
           (apply 'start-process "netscape"
                          nil
                          (if (listp kom-netscape-command)
                              (car kom-netscape-command)
                            kom-netscape-command)
                          (if (listp kom-netscape-command)
                              (append (cdr kom-netscape-command)
                                      (list url))
                            (list url))))
          (t nil))))


(defun lyskom-view-url-mosaic (url manager)
  "View the URL URL using NCSA Mosaic. The attempts to open the URL in an
existing Mosaic process. Failing that, it starts a new Mosaic."
  (let ((pid -1)
        tempbuffer
        (filename "/tmp/Mosaic."))
    (if (file-exists-p (expand-file-name "~/.mosaicpid"))
        (save-excursion
          (set-buffer 
           (setq tempbuffer (get-buffer-create " *kom*-mosaicpid")))
          (insert-file-contents (expand-file-name "~/.mosaicpid"))
          (setq pid (read tempbuffer))
          (delete-region (point-min) (point-max))
          (insert "newwin\n")
          (insert url)
          (insert "\n")
          (setq filename (concat filename (number-to-string pid)))
          (write-region (point-min) (point-max) filename nil 'x)
          (kill-buffer tempbuffer)
          (if (= -1 (signal-process pid 30))
              (apply 'start-process "mosaic"
                     (current-buffer)
                     (if (listp kom-mosaic-command)
                         (car kom-mosaic-command)
                       kom-mosaic-command )
                     (if (listp kom-mosaic-command)
                         (append (cdr kom-mosaic-command)
                                 (list url))
                       (list url)))
	    (lyskom-url-manager-starting manager)))
      (save-excursion
        (apply 'start-process "mosaic"
               (current-buffer)
               (if (listp kom-mosaic-command)
                   (car kom-mosaic-command)
                 kom-mosaic-command )
               (if (listp kom-mosaic-command)
                   (append (cdr kom-mosaic-command)
                           (list url))
                 (list url)))
	(lyskom-url-manager-starting manager)))))


;;;
;;;	email buttons
;;;

(defun lyskom-button-send-mail (to)
  (mail nil to))
