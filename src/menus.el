;;;;; -*-unibyte: t;-*-
;;;;;
;;;;; $Id: menus.el,v 44.15.2.1 1999-10-13 09:56:07 byers Exp $
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
;;;; File: menus.el
;;;; Author: Niels M�ller
;;;;
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: menus.el,v 44.15.2.1 1999-10-13 09:56:07 byers Exp $\n"))

(lyskom-external-function set-buffer-menubar)
(lyskom-external-function popup-menu)
(lyskom-external-function add-submenu)


(defvar lyskom-current-menu-category nil
  "Category of menus currently used in buffer")
(make-variable-buffer-local 'lyskom-current-menu-category)

(defvar lyskom-menu-template
  '((menu read
	  ((item kom-view-next-text)
	   (item kom-list-news)
	   (hline review-separator)
	   (item kom-view-commented-text)
	   (item kom-view-previous-commented-text)
	   (item kom-review-comments)
	   (item kom-review-tree)
	   (item kom-find-root)
           (item kom-find-root-review)
           (item kom-review-clear)
	   (hline jump-separator)
	   (item kom-jump)
	   (item kom-super-jump)
	   (item kom-set-unread)))
    (menu write
	  ((item kom-write-text)
	   (item kom-send-letter)
	   (item kom-write-comment)
	   (item kom-private-answer)
	   (item kom-comment-previous)
	   (hline send-separator)
	   (item kom-send-message)))
    (menu conference
	  ((item kom-go-to-conf)
	   (item kom-go-to-next-conf)
	   (hline info-separator)
	   (item kom-membership)
	   (item kom-list-conferences)
	   (item kom-status-conf)
	   (item kom-review-presentation)
	   (hline member-separator)
	   (item kom-add-self)
	   (item kom-sub-self)
	   (item kom-prioritize)))
    (menu person
	  ((item kom-who-is-on)
	   (item kom-status-session)
	   (hline info-separator)
	   (item kom-list-persons)
	   (item kom-status-person)
	   (item kom-review-presentation)
	   (hline change-separator)
	   (item kom-change-name)
	   (item kom-change-password)))
    (menu other
          ((item kom-customize))))
  "The menus used in LysKOM.")

(defvar lyskom-popup-menu-template
  (` (menu lyskom ((,@ lyskom-menu-template))))
  "Popup-menu in the backgrouond of the LysKOM window")


(defvar lyskom-edit-menu-template
  '((menu lyskom
	  ((item kom-ispell-message)
           (item kom-edit-send)
;	   (item kom-edit-send-anonymous)
	   (hline reciever-separator)
	   (item kom-edit-add-recipient)
	   (item kom-edit-add-copy)
	   (hline comment-separator)
	   (item kom-edit-show-commented)
;	   (item kom-edit-insert-commented)
	   (hline send-separator)
	   (item kom-edit-quit))))
  "The menus for editing LysKOM messages.")

(defvar lyskom-menu-list '((lyskom-mode . lyskom-menu)
                           (lyskom-edit-mode . lyskom-edit-menu))
  "List of menu sets in LysKOM")

(defvar lyskom-menu nil
  "A keymap describing the LysKOM top menu.")

;(when (not lyskom-menu) 
;  (setq lyskom-menu (make-sparse-keymap)))

(defvar lyskom-edit-menu nil
  "A keymap the LysKOM menu in the edit buffer.")

;(when (not lyskom-edit-menu)
;  (setq lyskom-edit-menu (make-sparse-keymap)))

(defvar lyskom-popup-menu nil
  "A keymap the LysKOM menu in the edit buffer.")

;(when (not lyskom-popup-menu)
;  (setq lyskom-popup-menu (make-sparse-keymap)))

(defun lyskom-build-menus ()
  "Create menus according to LYSKOM-MENUS"
  (lyskom-xemacs-or-gnu (lyskom-build-menus-xemacs)
                        (lyskom-build-menus-gnu)))


(defun lyskom-build-menus-xemacs ()
  (setq lyskom-edit-menu (lyskom-define-menu-xemacs lyskom-edit-menu-template))
  (setq lyskom-popup-menu (lyskom-define-menu-xemacs
                               lyskom-popup-menu-template))
  (setq lyskom-menu (lyskom-define-menu-xemacs lyskom-menu-template)))


(defun lyskom-build-menus-gnu ()
  "Rebuilds the LysKOM menus"
  (setq lyskom-menu (make-sparse-keymap))
  (setq lyskom-edit-menu (make-sparse-keymap))
  (setq lyskom-popup-menu (make-sparse-keymap))
  (lyskom-define-menu-gnu lyskom-menu lyskom-menu-template)
  (lyskom-define-menu-gnu lyskom-edit-menu lyskom-edit-menu-template)
  (lyskom-define-menu-gnu lyskom-popup-menu
                          (list lyskom-popup-menu-template))
  (setq lyskom-popup-menu (lookup-key lyskom-popup-menu [lyskom])))

(defun lyskom-define-menu-xemacs (menus)
  (let ((type nil)
        (parameters nil))
    (ignore type parameters)            ; Are they ever used?
    (cond ((null (car menus)))
          ((listp (car menus))          ; Menu bar
           (mapcar 'lyskom-define-menu-xemacs
                   menus))

          ((eq (car menus) 'menu)       ; A menu
           (let ((menu-title (car (cdr menus)))
                 (menu-items (car (cdr (cdr menus)))))
             (cons (lyskom-get-menu-string menu-title)
                   (mapcar
                    (function
                     (lambda (item)
                       (cond ((eq (car item) 'item)
                              (vector (lyskom-get-menu-string 
                                       (car (cdr item)))
                                       (car (cdr item))
                                       ':active
                                       t))
                             ((eq (car item) 'hline)
                              (vector "--:shadowEtchedIn" nil ':active nil) )
                             ((eq (car item) 'menu)
                              (lyskom-define-menu-xemacs item))
                             (t
                              (error "Bad menu item: %S"
                                     item)))))
                    menu-items))))

          (t nil))))


(defun lyskom-define-menu-gnu (map menus)
  (when menus
    (lyskom-define-menu-gnu map (cdr menus))
    (let ((type (car (car menus)))
	  (symbol (car (cdr (car menus)))))
      (cond ((eq 'hline type)
	     (define-key map (vector symbol) '("--")))
	    ((eq 'menu type)
	     (let* ((name (lyskom-get-menu-string symbol))
		    (submap (make-sparse-keymap name)))
	       (define-key map (vector symbol)
		 (cons name submap))
	       (lyskom-define-menu-gnu submap
				   (car (cdr (cdr (car menus)))))))
	    ((eq 'item type)
	     (define-key map (vector symbol)
	       (cons (lyskom-get-menu-string symbol) symbol)))
	    (t (error "Menu description invalid in lyskom-define-menu"))))))


(defun lyskom-get-menu-category (menu-category)
  (symbol-value (cdr (assq menu-category lyskom-menu-list))))

(defun lyskom-update-menus ()
  (lyskom-build-menus)
  (when (and (boundp 'lyskom-current-menu-category)
             lyskom-current-menu-category)
    (mapcar (function
             (lambda (mc)
               (lyskom-set-menus mc (current-local-map))))
            lyskom-current-menu-category)))

(defun lyskom-set-menus (menu-category  keymap)
  (lyskom-xemacs-or-gnu (lyskom-set-menus-xemacs menu-category)
                        (lyskom-set-menus-gnu menu-category keymap)))

(defun lyskom-set-menus-gnu (menu-category keymap)
  "Update the menus"
  (define-key keymap [menu-bar] (lyskom-get-menu-category menu-category))
  (make-local-variable 'lyskom-current-menu-category)
  (if (not (boundp 'lyskom-current-menu-category))
      (setq lyskom-current-menu-category (list menu-category))
    (add-to-list 'lyskom-current-menu-category menu-category)))

(eval-when-compile (defvar default-menubar nil))

(defun lyskom-set-menus-xemacs (menu-category)
  "Update the menus"
  (make-local-variable 'current-menubar)
  (make-local-variable 'lyskom-current-menu-category)
  (set-buffer-menubar default-menubar)
  (mapcar (function
           (lambda (menu)
             (add-submenu nil menu)))
          (lyskom-get-menu-category menu-category))
  (setq lyskom-current-menu-category (list menu-category)))

;;;
;;; This function would have been completely unnecessary if Gnu Emacs
;;; didn't carry around an ancient version of popup-menu that is
;;; completely incompatible with XEmacs version of the same function. 
;;; Sometimes I hate elisp.
;;;

(defun lyskom-do-popup-menu (menu event)
  "Pop up a menu"
  (lyskom-xemacs-or-gnu 
   (popup-menu menu event)
   (let* ((result (nreverse (x-popup-menu (or event t)
                                          (list menu)))))
     (cond ((null result))
           ((listp (car result)) 
            (apply (car (car result))
                   (cdr (car result))))
           ((commandp (car result))
            (call-interactively (car (nreverse result))))
           ((functionp (car result))
            (funcall (car result)))
           (t nil)))))


(defun lyskom-background-menu (pos event)
  "Pop up a menu with LysKOM commands and execute the selected command."
  (let* ((menu lyskom-popup-menu)
	 (result (lyskom-do-popup-menu menu event)))))




;;;;(�terse) Bakl�nges		   Addera extra kopiemottagare
;;;;Addera kommentar		   Addera medlem
;;;;Addera mottagare		   Automatsvar
;;;;Avmarkera (inl�gg)		   Bli medlem i m�te
;;;;B�rja med nytt namn		   Endast l�sa senaste
;;;;Filtrera f�rfattare		   Filtrera inneh�ll
;;;;Filtrera �rende			   Fj�rrkontrollera automatsvar
;;;;Fj�rrkontrollera avsluta	   Fj�rrkontrollera lista meddelanden
;;;;Fj�rrkontrollera radera meddelanden
;;;;Fj�rrkontrollera �ndra svarsmeddelande
;;;;Flytta inl�gg			   Fotnot till inl�gg
;;;;F� sk�ll			   F� uppmuntran
;;;;G� till m�te			   G� till n�sta m�te
;;;;Hj�lp				   Hoppa �ver alla kommentarer
;;;;Kasta ut en session		   Kommentera f�reg�ende inl�gg
;;;;Kommentera inl�gget		   Ladda ner fil
;;;;Ladda upp fil			   Lista (med) regexpar
;;;;Lista filarean			   Lista filter
;;;;Lista klienter			   Lista meddelanden
;;;;Lista medlemsskap		   Lista m�ten
;;;;Lista nyheter			   Lista personer
;;;;Lista �renden			   L�sa n�sta inl�gg
;;;;L�ngsamma kommandon		   Markera (inl�gg)
;;;;Personligt svar			   Personligt svar p� f�reg�ende inl�gg
;;;;Prioritera m�ten		   Radera inl�gg
;;;;Radera meddelanden		   Se tiden
;;;;Skapa m�te			   Skicka brev
;;;;Skriva ett inl�gg		   Sluta
;;;;Snabba kommandon		   Spara text (p� fil)
;;;;Spara variabler			   Status (f�r) m�te
;;;;Status (f�r) person		   Status (f�r) session
;;;;Subtrahera kommentar		   Subtrahera mottagare
;;;;Superhoppa			   S�nda meddelande
;;;;S�tt lapp p� d�rren		   S�tt l�sniv�
;;;;Ta bort lapp p� d�rren		   Uppskjuta l�sning
;;;;Uteslut medlem			   Utpl�na
;;;;Uttr�da ur m�te			   Var (�r) jag
;;;;Vilka �r inloggade		   Visa user-arean
;;;;V�nta p� ett inl�gg		   �ndra filter
;;;;�ndra livsl�ngd			   �ndra l�senord
;;;;�ndra m�testyp			   �ndra namn
;;;;�ndra organisat�r		   �ndra presentation
;;;;�ndra superm�te			   �ndra svarsmeddelande
;;;;�ndra till�tna f�rfattare	   �ndra variabler
;;;;�terse alla			   �terse alla kommentarer
;;;;�terse alla kommentarer rekursivt  �terse alla markerade
;;;;�terse det f�reg�ende kommenterade
;;;;�terse det kommenterade		   �terse f�rsta
;;;;�terse hoppa			   �terse igen
;;;;�terse inl�gg			   �terse lista
;;;;�terse markerade		   �terse n�sta
;;;;�terse omodifierat		   �terse presentation
;;;;�terse senaste			   �terse tr�d
;;;;�terse urinl�gget		   �terstarta kom
;;;;�verg� till administrat�rsmod

(provide 'lyskom-menus)

;;; menus.el ends here
