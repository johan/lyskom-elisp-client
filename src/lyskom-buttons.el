;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;; $Id: lyskom-buttons.el,v 44.108 2010-05-13 18:14:11 byers Exp $
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
;;;; File: lyskom-buttons.el
;;;; Author: David Byers
;;;;
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: lyskom-buttons.el,v 44.108 2010-05-13 18:14:11 byers Exp $\n"))

(lyskom-external-function glyph-property)
(lyskom-external-function widget-at)
(lyskom-external-function widget-get)
(lyskom-external-function w3-widget-button-click)
(lyskom-external-function w3-popup-menu)
(lyskom-external-function Info-goto-node)
(lyskom-external-function term-char-mode)
(lyskom-external-function w3m)


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


(defun kom-previous-link (num)
  "Move the cursor to the previous active area in the LysKOM buffer."
  (interactive "p")
  (lyskom-prev-area num 'lyskom-button))

(defun kom-next-link (num)
  "Move the cursor to the next active area in the LysKOM buffer."
  (interactive "p")
  (lyskom-next-area num 'lyskom-button))


(defun kom-button-press-or-self-insert-command ()
  "Simulate a mouse button press at point, if there is a button."
  (interactive)
  (if (get-text-property (point) 'lyskom-button)
      (call-interactively 'kom-button-press)
    (call-interactively 'self-insert-command)))

(defun kom-button-press ()
  "Simulate a mouse button press at point."
  (interactive)
  (let* ((type  (get-text-property (point) 'lyskom-button-type))
         (data  (assq type lyskom-button-actions))
         (hint (get-text-property (point) 'lyskom-button-hint))
         (act  (or (and kom-use-button-hints hint)
                   (and data (elt data 2)))))
    (cond ((null data) (lyskom-button-press (point)))
          ((null act) (lyskom-button-menu (point) 'key))
          (t (lyskom-button-press (point))))))

(defun kom-menu-button-press-or-self-insert-command ()
  "Simulate a menu mouse button press at point, if there is a button."
  (interactive)
  (if (get-text-property (point) 'lyskom-button)
      (call-interactively 'kom-menu-button-press)
    (call-interactively 'self-insert-command)))


(defun kom-menu-button-press ()
  "Simulate a menu mouse button press at point."
  (interactive)
  (lyskom-button-menu (point) 'key))

(defun kom-button-click (event &optional do-default)
  "Execute the default action of the active area under the mouse.
If optional argument do-default is non-nil, call the default binding of
this-command-keys."
  (interactive "@e")
  (let* ((pos (lyskom-event-point event))
         (glyph (lyskom-event-glyph event))
         (widget (and pos 
                      (or (and glyph (glyph-property glyph 'widget))
                          (widget-at pos))))
         (parent (and widget (widget-get widget ':parent)))
         (href (or (and widget (widget-get widget ':href))
                   (and parent (widget-get parent ':href))
                   (and widget (widget-get widget 'href))
                   (and parent (widget-get parent 'href))))
         (w3m-url (and pos (get-text-property pos 'w3m-href-anchor)))
         (type  (and pos (get-text-property pos 'lyskom-button-type)))
         (data  (and type (assq type lyskom-button-actions)))
         (hint (and pos (get-text-property pos 'lyskom-button-hint)))
         (act  (or (and kom-use-button-hints hint)
                   (and data (elt data 2)))))
    (cond (href (require 'w3)
                (w3-widget-button-click event))
          (w3m-url (require 'w3m)
               (w3m w3m-url))
          ((and do-default
                (or (null pos)
                    (null (get-text-property pos 'lyskom-button-type))))
           (let ((fn (lookup-key global-map (this-command-keys))))
             (when (commandp fn)
               (call-interactively fn))))
          ((null act) (kom-popup-menu event))
          (t (lyskom-button-press pos)))))

(defun kom-button-click-or-yank (event)
  "Execute the default action of the active area under the mouse.
If there is no active area, then do something else."
  (interactive "@e")
  (kom-button-click event t))

(defun kom-popup-menu (event)
  "Pop up a menu of actions to be taken at the active area under the mouse."
  (interactive "@e")
  (let* ((pos (lyskom-event-point event))
         (glyph (lyskom-event-glyph event))
         (widget (and pos 
                      (or (and glyph (glyph-property glyph 'widget))
                          (widget-at pos))))
         (parent (and widget (widget-get widget ':parent)))
         (href (or (and widget (widget-get widget ':href))
                   (and parent (widget-get parent ':href))
                   (and widget (widget-get widget 'href))
                   (and parent (widget-get parent 'href)))))
    (cond (href (require 'w3)
                (w3-popup-menu event))
          ((and pos (get-text-property pos 'lyskom-button-type))
           (lyskom-button-menu pos event))
          (t (lyskom-background-menu pos event)))))
  
(defun kom-mouse-null (event)
  "Do nothing."
  (interactive "@e")
  ;; This is here to pervent unwanted events when clicking mouse-3
  (identity 1))

(defun lyskom-make-button-menu (title entries buf arg text filter)
  "Create a menu keymap from a list of button actions."
  ;; Use the command as the event for simplicity.  Note that the menu
  ;; function alters the menu, so we copy the entries to prevent it
  ;; from fiddling with lyskom-button-actions.
  (let* ((lyskom-language lyskom-global-language)
         (title (lyskom-menu-encode title 'item)))
    (when (> (length title) 44) (setq title (concat (substring title 0 40)
                                                    " ...")))
    (cond ((string-match "XEmacs" (emacs-version))
           (cons title
                 (delq nil
                       (mapcar (lambda (entry)
                                 (and (funcall filter (cdr entry) arg)
                                      (vector (lyskom-menu-encode
                                               (lyskom-get-string (car entry))
					       'item)
                                              (list (cdr entry)
                                                    buf
                                                    (if (listp arg)
                                                        (list 'quote arg)
                                                      arg)
                                                    text)
                                              ':active t)))
                               entries))))
          (t (list title
                   (cons title
                         (delq nil
                               (mapcar (lambda (entry)
                                         (when (funcall filter (cdr entry) arg)
                                           (cons 
                                            (lyskom-menu-encode (lyskom-get-string (car entry))
                                                                'item)
                                            `(,(cdr entry) ,buf ,arg ,text))))
                                       entries))))))))


(defun lyskom-button-menu (pos event)
  "Internal function used by kom-popup-menu"
  (let* ((type  (get-text-property pos 'lyskom-button-type))
         (arg   (get-text-property pos 'lyskom-button-arg))
         (text  (get-text-property pos 'lyskom-button-text))
         (buf   (get-text-property pos 'lyskom-buffer))
         (data  (assq type lyskom-button-actions))
         (title (cond 
                 ((get-text-property pos 'lyskom-button-menu-title)
                  (apply 'lyskom-format 
                         (get-text-property pos 'lyskom-button-menu-title)))
                 ((elt data 1)
                  (lyskom-format (lyskom-get-string (elt data 1)) text))
                 (t (lyskom-format (lyskom-get-string 'generic-popup-title) text))))
         (actl  (or (and data (elt data 3)) nil))
         (filter (or (and data (elt data 5)) (lambda (&rest args) t))))
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
	   (if (eq event 'key)
	       (lyskom-keyboard-menu title actl buf arg text filter)
	     (let* ((menu (lyskom-make-button-menu title actl
						   buf arg text filter)))
	       (lyskom-do-popup-menu menu event)))))))


(defun lyskom-keyboard-menu (title entries buf arg text filter)
  "Do a keyboard menu selection."
  (let* ((prompt nil)
         (maxlen 0)
         (entries 
          (delq nil 
                (mapcar (lambda (el)
                          (and (funcall filter (cdr el) arg)
                               (cons (if (stringp (car el)) 
                                         (car el)
                                       (lyskom-get-string (car el)))
                                     (cdr el))))
                        entries)))
         (title (if (stringp title) title (lyskom-get-string title)))
         (completion-ignore-case t))
    (lyskom-traverse e entries
      (if (> (lyskom-string-width (car e)) maxlen)
          (setq maxlen (lyskom-string-width (car e)))))
    (setq prompt (substring title 0
			    (min (lyskom-string-width title)
				 (- (window-width (minibuffer-window))
				    maxlen 3))))

    (let ((choice (lyskom-read-from-menu prompt
                                         (lyskom-maybe-frob-completion-table
                                          entries t))))
      (when choice
        (funcall (cdr choice)
                 buf arg text)))))

       

(defun lyskom-button-press (pos)
  "Execute the default action of the active area at POS if any."
  (when pos
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
                        text))))))
                      

(defun lyskom-fix-pseudo-url (url)
  (save-match-data
    (when (string-match "^google:/*" url)
      (setq url (concat "http://www.google.com/search?q="
                        (substring url (match-end 0)))))
    (if (not (string-match lyskom-url-protocol-regexp url))
        (cond ((string-match "^www\\." url) 
               (concat "http://" url))
              ((string-match "^ftp\\." url) 
               (concat "ftp://" url))
              ((string-match "^gopher\\." url) 
               (concat "gopher://" url))
              ((string-match "^wais\\." url)
               (concat "wais://" url))
              (t (concat "http://" url)))
      url)))



(defun lyskom-button-add-links (text links)
  "Add links to TEXT according to LINKS.
LINKS is a list of lyskom-text-link objects."
  (lyskom-traverse spec links
    (let ((case-fold-search (lyskom-text-link->ignore-case spec)) 
          (start 0))
      (while (string-match (lyskom-text-link->pattern spec) text start)
         ;; Need to do this here since replace-in-string fscks up match-data
        (setq start (match-end 0))
        (add-text-properties
         (or (match-beginning (lyskom-text-link->highlight spec))
             (match-beginning 0))
         (or (match-end (lyskom-text-link->highlight spec))
             (match-end 0))
         (lyskom-generate-button 'url
                                 nil
                                 (replace-in-string 
                                  (match-string 0 text)
                                  (lyskom-text-link->pattern spec)
                                  (lyskom-text-link->replacement spec))
                                 kom-url-face)
         text)))))



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
  (let ((face (elt el 4)))
    (or (and (boundp face) (symbol-value face))
        (and (facep face) face))))

(defsubst lyskom-button-get-pred (el)
  "Return the match predicate for bitton type EL."
  (elt el 5))

(defun lyskom-button-transform-text (text &optional text-stat)
  "Add text properties to the string TEXT according to the definition of
lyskom-text-buttons. Returns the modified string."
  (let ((blist lyskom-text-buttons)
        (rcpts (reverse (and text-stat (lyskom-text-recipients text-stat))))
        (start 0)
        (el nil))

    ;; Do other text buttons
    (while blist
      (setq el (car blist))
      (setq start 0)
      (while (string-match (elt el 0) text start)
        (if (or (null (lyskom-button-get-pred el))
                  (funcall (lyskom-button-get-pred el)
                           (lyskom-button-get-text el text)))
          (progn
            (add-text-properties 
             (match-beginning (or (elt el 2) 0))
             (match-end (or (elt el 2) 0))
             (cond ((and (eq (elt el 1) 'text)
                         (not lyskom-transforming-external-text))
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

                   ((eq (elt el 1) 'pseudo-url)
                    (let ((url (lyskom-fix-pseudo-url
                                (lyskom-button-get-text el text))))
                      (lyskom-generate-button 'url
                                              nil
                                              url
                                              (lyskom-button-get-face el))))

                   ((eq (elt el 1) 'info-node)
                    (lyskom-generate-button 'info-node
                                            (lyskom-button-get-arg el text)
                                            (lyskom-button-get-text el text)
                                            (lyskom-button-get-face el)))


                   ((eq (elt el 1) 'email)
                    (lyskom-generate-button 'email
                                            nil
                                            (lyskom-button-get-text el text)
                                            (lyskom-button-get-face el)))
                   (t nil))
             text)
            (setq start (match-end 0)))
          (setq start (1+ (match-beginning 0)))))
      (setq blist (cdr blist)))

    ;; Do links from kom-text-links last -- they have precedence
    ;; Do the first recipient last so links for that rcpt have prio
    (lyskom-traverse rcpt rcpts
      (lyskom-button-add-links text (cdr (assq rcpt kom-text-links))))

    ;; Do global links from kom-text-links very last
    (lyskom-button-add-links text (cdr (assq t kom-text-links)))
    )
  text)


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


	
(defun lyskom-generate-button (type arg &optional text face menu-title
				    subtle)
  "Generate the properties for a button of type TYPE with argument
ARG. Optional argument TEXT is the button text to be saved as a
property and FACE is the default text face for the button. Optional
argument MENU-TITLE defines the title for the popup menu. See
lyskom-default-button for more information. Optional argument SUBTLE
means don't set the lyskom-button property if non-nil. that means
kom-next- and -previous-link won't notice the button"
  (setq lyskom-dummy-variable-to-fool-the-byte-compiler
        (car menu-title))		; produce error if menu-title not cons
  (let* ((numarg (cond ((numberp arg) arg)
                       ((stringp arg) (string-to-number arg))
                       ((lyskom-conf-stat-p arg) (conf-stat->conf-no arg))
                       ((lyskom-uconf-stat-p arg) (uconf-stat->conf-no arg))
                       ((lyskom-pers-stat-p arg) (pers-stat->pers-no arg))
                       (t nil)))
         (data (assq type lyskom-button-actions))
         (hints (and data (elt data 4)))
         (the-hint (lyskom-get-button-hint hints))
         (props 
          (cond ((and (memq type '(conf pers)) numarg)
                 (list 'face 
                       (or face
                           (or 
			    (let ((val (lyskom-indirect-assq 
					numarg kom-highlight-conferences)))
			      (when val
				(cond ((facep val) val)
				      ((and (symbolp val) (boundp val))
				       (symbol-value val)))))
                            kom-active-face))
                       'mouse-face kom-highlight-face
                       'lyskom-button-text text
                       'lyskom-button-type type
                       'lyskom-button-arg numarg
                       'lyskom-button-menu-title menu-title
                       'lyskom-buffer lyskom-buffer))
                ((and (eq type 'text) numarg)
                 (list 'face (or face kom-text-no-face)
                       'mouse-face kom-highlight-face
                       'lyskom-button-text text
                       'lyskom-button-type type
                       'lyskom-button-arg numarg
                       'lyskom-button-menu-title menu-title
                       'lyskom-buffer lyskom-buffer))
                ((eq type 'url)
                 (list 'face (or face kom-active-face)
                       'mouse-face kom-highlight-face
                       'lyskom-button-text text
                       'lyskom-button-type type
                       'lyskom-button-arg arg
                       'lyskom-button-menu-title menu-title
                       'lyskom-buffer lyskom-buffer))
                (t
		 (list 'face (or face kom-active-face)
		       'mouse-face kom-highlight-face
		       'lyskom-button-text text
		       'lyskom-button-type type
		       'lyskom-button-arg arg
                       'lyskom-button-menu-title menu-title
		       'lyskom-buffer lyskom-buffer)))))

    (append (list 'rear-nonsticky t)
	    (if (not subtle) (list 'lyskom-button t))
	    (if the-hint
		(cons 'lyskom-button-hint
		      (cons the-hint props))
	      props))))




(defun lyskom-default-button (type arg &optional menu-title)
  "Generate a button of type TYPE from data in ARG. ARG can be almost any
type of data and is converted to the proper argument type for buttons of
type TYPE before being sent to lyskom-generate-button. Optional argument
MENU-TITLE is a list consisting of a format string or symbol and arguments
for the format string. The arguments are not when the menu is popped
up."
  (and kom-text-properties
       (let (xarg text face subtle)
	 (cond ((eq type 'conf)
		(cond ((lyskom-conf-stat-p arg)
		       (if (conf-type->letterbox (conf-stat->conf-type arg))
			   (setq type 'pers))
		       (setq xarg arg
			     text (conf-stat->name arg)))
                      ((lyskom-uconf-stat-p arg)
		       (if (conf-type->letterbox (uconf-stat->conf-type arg))
			   (setq type 'pers))
		       (setq xarg (uconf-stat->conf-no arg)
			     text (uconf-stat->name arg)))
                      ((lyskom-conf-z-info-p arg)
                       (if (conf-type->letterbox (conf-z-info->conf-type arg))
                           (setq type 'pers))
                       (setq xarg (conf-z-info->conf-no arg)
                             text (conf-z-info->name arg)))
		      ((numberp arg)
                       (if (setq xarg (cache-get-uconf-stat arg))
                           (progn
                             (if (conf-type->letterbox
                                  (uconf-stat->conf-type xarg))
                                 (setq type 'pers))
                             (setq text (uconf-stat->name xarg))
                             (setq xarg arg))
                         (setq text "" xarg arg)))
		      (t (setq text "" xarg 0))))

	       ((eq type 'pers)
		(cond ((lyskom-conf-stat-p arg)
		       (setq xarg arg
			     text (conf-stat->name arg)))
                      ((lyskom-uconf-stat-p arg)
		       (setq xarg arg
			     text (uconf-stat->name arg)))
                      ((lyskom-conf-z-info-p arg)
                       (setq xarg arg
                             text (conf-z-info->name arg)))
		      ((lyskom-pers-stat-p arg)
		       (setq xarg (pers-stat->pers-no arg)
			     text (or (conf-stat->name
                                       (cache-get-conf-stat
                                        (pers-stat->pers-no arg)))
                                      "")))
		      ((numberp arg) 
                       (if (setq xarg (cache-get-uconf-stat arg))
                           (progn
                             (if (conf-type->letterbox
                                  (uconf-stat->conf-type xarg))
                                 (setq type 'pers))
                             (setq text (uconf-stat->name xarg))
                             (setq xarg arg))
                         (setq text "" xarg arg)))

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
                (setq face kom-url-face)
		(cond ((stringp arg) (setq xarg nil text arg))
		      (t (setq xarg nil text ""))))

               ((eq type 'email)
                (setq face kom-url-face)
                (cond ((stringp arg) (setq xarg nil text arg))
                      (t (setq xarg nil text ""))))

	       ((eq type 'timestamp)
		(setq face 'default
		      subtle t)
                (cond ((null arg) (setq xarg (lyskom-current-client-time)
                                        ;text (format-time-string "%Y-%m-%d %H:%M")
                                        ))
		      (t (setq xarg arg
                               ;text (format-time-string "%Y-%m-%d %H:%M" arg)
                               ))))
	       (t (setq xarg arg
			text "")))
	 (lyskom-generate-button type xarg text face menu-title subtle))))
                  

           


;;;========================================
;;; Button actions
;;;


(defun lyskom-button-view-text (buf arg text)
  "In the LysKOM buffer BUF, view the text ARG. Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (goto-char (point-max))
           (kom-view arg))))

(defun lyskom-button-unread-text (buf arg text)
  "In the LysKOM buffer BUF, unread the text ARG. 
Last argument TEXT is ignored. This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-mark-unread arg))))

(defun lyskom-button-copy-text-no (but arg text)
  "In the LysKOM buffer BUF, ignore ARG and copy TEXT to the kill ring.
This is a LysKOM button action."
  (kill-new text))


(defun lyskom-button-review-noconversion (buf arg text)
  "In the LysKOM buffer BUF, view the text ARG without conversion. 
Last argument TEXT is ignored. This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (goto-char (point-max))
           (kom-review-noconversion arg))))

(defun lyskom-button-review-converted (buf arg text)
  "In the LysKOM buffer BUF, view the text ARG converted. 
Last argument TEXT is ignored. This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (goto-char (point-max))
           (kom-review-converted arg))))

(defun lyskom-button-review-rot13 (buf arg text)
  "In the LysKOM buffer BUF, view the text ARG rot13:ed. 
Last argument TEXT is ignored. This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (goto-char (point-max))
           (kom-review-rot13 arg))))

(defun lyskom-button-find-root-review (buf arg text)
  "In the LysKOM buffer BUF, view the text ARG. Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
	(t (pop-to-buffer buf)
           (goto-char (point-max))
	   (kom-find-root-review arg))))

(defun lyskom-button-find-root (buf arg text)
  "In the LysKOM buffer BUF, view the text ARG. Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
	(t (pop-to-buffer buf)
           (goto-char (point-max))
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
                 (lyskom-mark-text arg))
             (lyskom-end-of-command)))))

(defun lyskom-button-unmark-text (buf arg text)
  "In the LysKOM buffer BUF, unmark the text ARG. Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (unwind-protect
               (progn
                 (pop-to-buffer buf)
                 (lyskom-start-of-command 'kom-unmark-text)
                 (lyskom-unmark-text arg))
             (lyskom-end-of-command)))))

(defun lyskom-button-save-text (buf arg text)
  "In the LysKOM buffer BUF, save the text ARG. Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (unwind-protect
               (progn (pop-to-buffer buf)
                      (kom-save-text nil (list arg)))))))

(defun lyskom-button-save-text-body (buf arg text)
  "In the LysKOM buffer BUF, save the text ARG. Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (unwind-protect
               (progn (pop-to-buffer buf)
                      (kom-save-text-body arg))))))

(defun lyskom-button-review-comments (buf arg text)
  "In the LysKOM buffer BUF, review comments to the the text ARG.
Last argument TEXT is ignored. This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-review-comments arg))))

(defun lyskom-button-review-tree (buf arg text)
  "In the LysKOM buffer BUF, recursively review comments to the the text ARG.
Last argument TEXT is ignored. This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-review-tree arg))))

(defun lyskom-button-write-footnote (buf arg text)
  "In the LysKOM buffer BUF, write a footnote to the the text ARG.
Last argument TEXT is ignored. This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-write-footnote arg))))

(defun lyskom-button-fast-reply (buf arg text)
  "In the LysKOM buffer BUF, make a remark to the the text ARG.
Last argument TEXT is ignored. This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-fast-reply arg))))


(defun lyskom-button-view-conf-presentation (buf arg text)
  "In the LysKOM buffer BUF, view the presentation of ARG. 
Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (goto-char (point-max))
           (kom-review-presentation arg))))

(defun lyskom-button-view-conf-status (buf arg text)
  "In the LysKOM buffer BUF, view the status of conference ARG. Last argument
TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (goto-char (point-max))
           (kom-status-conf arg))))

(defun lyskom-button-goto-conf (buf arg text)
  "In the LysKOM buffer BUF, go to the conference ARG. Last argument TEXT is
ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (goto-char (point-max))
           (kom-go-to-conf arg))))

(defun lyskom-button-add-self (buf arg text)
  "In the LysKOM buffer buf, add self to conference ARG."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (let ((conf (blocking-do 'get-conf-stat arg)))
             (if conf
                 (kom-add-self conf)
               (lyskom-insert-before-prompt 'somebody-deleted-that-conf))))))

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
           (goto-char (point-max))
           (kom-status-person arg))))

(defun lyskom-button-befriend (buf arg text)
  "In the LysKOM buffer BUF, befriend person ARG.
Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (goto-char (point-max))
           (kom-befriend arg))))

(defun lyskom-button-moronify (buf arg text)
  "In the LysKOM buffer BUF, befriend person ARG.
Last argument TEXT is ignored.
This is a LysKOM button action."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (goto-char (point-max))
           (kom-moronify arg))))

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

(defun lyskom-button-view-session-status (buf arg text)
  "In the LysKOM buffer BUF, show session status for person ARG."
  (cond ((not (integerp arg)) nil)
        (t (pop-to-buffer buf)
           (kom-status-session (lyskom-session-from-conf arg)))))

(defun lyskom-button-copy-email (but arg text)
  "In the LysKOM buffer BUF, ignore ARG and copy TEXT to the kill ring.
This is a LysKOM button action."
  (kill-new text))

(lyskom-external-function compose-mail)
(defun lyskom-button-open-email (but arg text)
  "In the LysKOM buffer BUF, ignore ARG and open TEXT as an e-mail address.
This is a LysKOM button action."
  (if (fboundp 'compose-mail)
      (compose-mail text)
    (mail nil text)))


(defun lyskom-button-copy-url (but arg text)
  "In the LysKOM buffer BUF, ignore ARG and copy TEXT to the kill ring.
This is a LysKOM button action."
  (kill-new (replace-in-string text "\\s-+" "")))


(defun lyskom-transform-url (url)
  (let ((rules kom-url-transformation-rules))
    (while rules
      (setq url (replace-in-string url (car (car rules)) (cdr (car rules)))
            rules (cdr rules))))
  url)

(defun lyskom-button-open-url (buf arg text)
  "In the LysKOM buffer BUF, ignore ARG and open TEXT as an URL.
This is a LysKOM button action."
  (let* ((url (lyskom-fix-pseudo-url (replace-in-string text "\\s-+" "")))
         protocol
         url-manager)
    (setq url (lyskom-transform-url url))
    (string-match lyskom-url-protocol-regexp url)
    (setq protocol (match-string 1 url))
    (setq url-manager (lyskom-get-url-manager protocol))
    (if (null url-manager)
        (lyskom-error "Can't find URL viewer"))
    (funcall (elt url-manager 3) url url-manager)))

 
 
;;;
;;;	Info node button
;;;

(defun lyskom-button-goto-info-node (buf arg text)
  "In the LysKOM buffer BUF, open ARG as an Info node, and ignore TEXT.
This is a LysKOM button action."
  (when (not (fboundp 'Info-goto-node))
    (autoload 'Info-goto-node "info"
      "Go to info node named NAME.  Give just NODENAME or (FILENAME)NODENAME."
      t))
  (setq arg (replace-in-string arg "\n" " " t))
  (setq arg (replace-in-string arg " +" " " t))
  (Info-goto-node arg))

;;;
;;;	Timestamp button
;;;

(defun lyskom-button-copy-timestamp (buf arg text)
  "In the LysKOM buffer BUF, ignore TEXT and copy ARG to the kill ring
after formating it as time. This is a LysKOM button action."
  (let ((kom-print-relative-dates nil))
    (kill-new (lyskom-format-time 'date-and-time arg))))


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


(eval-when-compile (defvar browse-url-browser-function nil))

(defun lyskom-view-url-browse-url (url manager)
  (require 'browse-url)
  (funcall browse-url-browser-function url))

(lyskom-with-external-functions (w3-fetch)
  (defun lyskom-view-url-w3 (url manager)
    "View the URL using W3. Second argument MANAGER is ignored."
    (w3-fetch url)))

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


(defun lyskom-view-url-windows (url manager)
  "View the URL URL in Microsoft Windows. MANGER is the URL manager.
Fall back on Netscape if not running in Microsoft Windows.

In theory, opening an URL on Windows should be a matter of doing
\(w32-shell-execute \"open\" URL\), but this sometimes fails, so
we protect ourselves by also trying to start some common defaults
using start-process."
  (cond
   ((memq window-system '(win32 mswindows w32))
    (cond
     ((and (boundp 'kom-windows-browser-command)
           (and (not (null kom-windows-browser-command))
                (not (eq (length kom-windows-browser-command) 0))))
      ;; Explicit given file name of browser, so complain to user if
      ;; the value is bad.
      (if (not (file-executable-p kom-windows-browser-command))
          (error (concat "Not an executable file: %s;"
                         " kom-windows-browser-command has a bad value")
                 kom-windows-browser-command))

      (condition-case nil
          (progn
            (lyskom-url-manager-starting manager)
            (start-process "Browser"
                           nil
                           kom-windows-browser-command
                           url))
        (error (error (concat "Failed starting browser using"
                              " kom-windows-browser-command"
                              " (%s)")
                      kom-windows-browser-command))))
     ((and (not (memq 'w32-shell-execute lyskom-compatibility-definitions))
           (condition-case nil
               (progn (w32-shell-execute "open" url) t)
             (error nil)))
      (lyskom-url-manager-starting manager))
     ;;(lyskom-message "Webb via [%s \"%s\" \"%s\"] ..."
     ;;                "w32-shell-execute" "open" url))
     (t (let ((programs
               '("c:\\Program\\Internet Explorer\\iexplore.exe"
                 "c:\\Programs\\Internet Explorer\\iexplore.exe"
                 "c:\\Program Files\\Internet Explorer\\iexplore.exe"
                 "start"
                 "explorer"
                 "C:\\Program Files\\Netscape\\Communicator\\Program\\netscape.exe"
                 "C:\\Program Files\\Netscape\\Navigator\\Program\\netscape.exe"
                 )))
          (while programs
            (condition-case nil
                (progn
                  (lyskom-url-manager-starting manager)
                  (start-process (car programs)
                                 nil
                                 (car programs)
                                 url)
                  (setq programs nil))
              (error (setq programs (cdr programs)))))))))
   (t (lyskom-view-url-netscape url manager))))


(defun lyskom-view-url-netscape (url manager)
  "View the URL URL using Netscape Navigator. The second argument
MANAGER is the URL manager that started Netscape.

This function attempts to load the URL in a running Netscape, but failing
that, starts a new one."
  (setq url (replace-in-string url "," "%2C"))
  (setq url (replace-in-string url "(" "%28"))
  (setq url (replace-in-string url ")" "%29"))
  (let* ((url-string (if (memq window-system '(win32 mswindows w32))
                         (list url)
                       (list "-remote"
                             (format
                              (cond ((eq kom-netscape-variant 'new-window)
                                     "openUrl(%s, new-window)")
                                    ((eq kom-netscape-variant 'new-tab)
                                     "openUrl(%s, new-tab)")
                                    (t "openUrl(%s)"))
                              url))))
         
         (proc (apply 'start-process "netscape"
                      nil
                      (if (listp kom-netscape-command)
                          (car kom-netscape-command)
                        kom-netscape-command)
                      (if (listp kom-netscape-command)
                          (append (cdr kom-netscape-command)
                                  url-string)
                        url-string)))
         (status 'run)
         (exit nil))
    (lyskom-url-manager-starting manager)
    (while (eq status 'run)
      (lyskom-accept-process-output)
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
        (save-current-buffer
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

(defun lyskom-view-url-galeon (url manager)
  "View the URL URL using Galeon. The second argument MANAGER is the URL
manager that started Galeon.

This function attempts to load the URL in a running Galeon, but failing
that, starts a new one."
  (setq url (replace-in-string url "," "%2C"))
  (setq url (replace-in-string url "(" "%28"))
  (setq url (replace-in-string url ")" "%29"))
  (let* ((url-string (if (memq window-system '(win32 mswindows w32))
                         (list url)
                       (list "-n"
                             (format "%s" url)))))
    (lyskom-url-manager-starting manager)
    (apply 'start-process "galeon"
           nil
           (if (listp kom-galeon-command)
               (car kom-galeon-command)
             kom-galeon-command)
           (if (listp kom-galeon-command)
               (append (cdr kom-galeon-command)
                       url-string)
             url-string))))

;; Added by Peter Liljenberg
(defun lyskom-view-url-lynx (url manager)
  "View the URL URL using Lynx.
Lynx will be run either in an xterm or in Emacs terminal mode,
depending on the value of `kom-lynx-terminal'."
  (cond
   ((eq kom-lynx-terminal 'xterm)
    (apply 'start-process
	   "lynx"
	   nil
	   (car kom-lynx-xterm-command)
	   (append (cdr kom-lynx-xterm-command) (list url)))
    (lyskom-url-manager-starting manager))

   ((eq kom-lynx-terminal 'terminal)
    (let* ((lbuf (get-buffer "*Lynx*"))
	   (lproc (and lbuf (get-buffer-process lbuf))))
      (if lproc
	  ;; Tell existing Lynx to fetch URL
	  (process-send-string lproc (concat "g" url "\n"))

	;; Create a new Lynx
	(switch-to-buffer
	 (apply 'make-term "Lynx"
		(if (listp kom-lynx-terminal-command)
		    (car kom-lynx-terminal-command)
		  kom-lynx-terminal-command)
		nil
		(if (listp kom-lynx-terminal-command)
		    (append (cdr kom-lynx-terminal-command) (list url))
		  (list url))))
	(delete-other-windows)
	(term-char-mode)
	(set-process-sentinel
	 (get-buffer-process (current-buffer))
	 (function (lambda (proc str)
		     (kill-buffer (process-buffer proc))))))
      (lyskom-url-manager-starting manager)))

   (t (lyskom-error "Bad Lynx terminal: %s" kom-lynx-terminal))
   ))


;;;
;;;     aux-item buttons
;;;

(defun lyskom-button-delete-aux (buf arg text)
  (let ((aux nil))
    (cond ((lyskom-aux-item-p arg))
          ((listp arg)
           (let ((items (cond ((eq 'text (car arg))
                               (text-stat->aux-items 
                                (blocking-do 'get-text-stat (elt arg 1))))
                              ((eq 'conf (car arg))
                               (conf-stat->aux-items 
                                (blocking-do 'get-conf-stat (elt arg 1))))
                              ((eq 'server (car arg))
                               (server-info->aux-item-list
                                (blocking-do 'get-server-info)))
                              (t nil))))
             (while items
               (when (eq (aux-item->aux-no (car items)) (elt arg 2))
                 (setq aux (car items))
                 (setq items nil))
               (setq items (cdr items))))))

    (when aux
      (lyskom-start-of-command nil)
      (unwind-protect
          (progn
            (unless (cond ((eq 'text (car arg))
                           (cache-del-text-stat (elt arg 1))
                           (blocking-do 'modify-text-info
                                        (elt arg 1)
                                        (list (aux-item->aux-no aux))
                                        nil))
                          ((eq 'conf (car arg))
                           (cache-del-conf-stat (elt arg 1))
                           (blocking-do 'modify-conf-info
                                        (elt arg 1)
                                        (list (aux-item->aux-no aux))
                                        nil))
                          ((eq 'server (car arg))
                           (prog1 (blocking-do 'modify-server-info
                                               (list (aux-item->aux-no aux))
                                               nil)
                             (lyskom-set-default 'lyskom-server-info (blocking-do 'get-server-info)))))
              (lyskom-report-command-answer nil)))
        (lyskom-end-of-command)))))
          

(defun lyskom-button-info-aux (buf arg text)
  (pop-to-buffer buf)
  (goto-char (point-max))
  (let ((aux nil))
    (cond ((lyskom-aux-item-p arg))
          ((listp arg) 
           (let ((items 
                  (cond ((eq 'text (car arg))
                         (text-stat->aux-items 
                          (blocking-do 'get-text-stat (elt arg 1))))
                        ((eq 'conf (car arg))
                         (conf-stat->aux-items 
                          (blocking-do 'get-conf-stat (elt arg 1))))
                        ((eq 'server (car arg))
                         (server-info->aux-item-list
                          (blocking-do 'get-server-info)))
                        (t nil))))
             (while items
               (when (eq (aux-item->aux-no (car items)) (elt arg 2))
                 (setq aux (car items))
                 (setq items nil))
               (setq items (cdr items))))))
              
    (if aux
        (let ((header (cond ((eq 'text (car arg)) 
                             (lyskom-format 'aux-item-for-text-no (elt arg 1)))
                            ((eq 'conf (car arg))
                             (lyskom-format 'aux-item-for-conference-no
                                            (blocking-do 'get-conf-stat
                                                         (elt arg 1))))
                            ((eq 'server (car arg))
                             (lyskom-format 'aux-item-for-server))
                            (t "????"))))
          (lyskom-start-of-command nil)
          (unwind-protect
              (lyskom-insert (or (lyskom-aux-item-call aux
                                                       'info 
                                                       aux
                                                       header)
                                 (lyskom-aux-item-info aux header)))
            (lyskom-end-of-command)))
      (lyskom-format-insert-before-prompt 'cant-get-aux-item))))
           
(defun lyskom-button-apply (buf arg text)
  (apply (car arg) (cdr arg)))


;;; ================================================================
;;; Functions to put in kom-highlight-conferences

(defun lyskom-highlight-function-get-conf-stat (arg)
  "Try to translate ARG to a conf-stat without making server calls.

ARG may be a conf-stat, pers-stat, uconf-stat, conf-z-info, integer or string."
  (if (lyskom-conf-stat-p arg)
      arg
    (let* ((conf-no (cond ((lyskom-pers-stat-p arg) (pers-stat->pers-no arg))
                          ((lyskom-uconf-stat-p arg) (uconf-stat->conf-no arg))
                          ((numberp arg) arg)
                          ((lyskom-conf-z-info-p arg) (conf-z-info->conf-no arg))
                          ((stringp arg) (lyskom-string-to-number arg)))))
      (when conf-no
        (cond ((cache-get-conf-stat conf-no))
              (conf-no (initiate-get-conf-stat 'background nil conf-no)
                       nil))))))


(defun lyskom-highlight-i-am-supervisor (arg)
  "Returns non-nil if lyskom-pers-no is the supervisor of ARG.

ARG interpreted by `lyskom-highlight-function-get-conf-stat'.

This function never makes server calls, so if the information required
to answer accurately is not cached, this function will return an incorrect
result (nil instead of t)."
  (let ((conf-stat (lyskom-highlight-function-get-conf-stat arg)))
    (lyskom-i-am-supervisor conf-stat nil)))

(defun lyskom-highlight-has-no-presentation (arg)
  "Returns non-nil if lyskom-pers-no is the supervisor of CONF-STAT.

ARG is interpreted by `lyskom-highlight-function-get-conf-stat'.

This function never makes server calls, so if the information required
to answer accurately is not cached, this function will return an incorrect
result (nil instead of t)."
  (let ((conf-stat (lyskom-highlight-function-get-conf-stat arg)))
    (cond ((null conf-stat) nil)
          ((conf-type->secret (conf-stat->conf-type conf-stat)) nil)
          (t (eq 0 (conf-stat->presentation conf-stat))))))
