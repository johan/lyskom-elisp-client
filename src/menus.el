;;;;;
;;;;; $Id: menus.el,v 44.5 1996-10-08 12:22:37 nisse Exp $
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


(defvar lyskom-menus
  '((menu lyskom
	  ((menu read ((item kom-view-next-text)
		       hline
		       (item kom-view-commented-text)
		       (item kom-view-previous-commented-text)
		       (item kom-review-comments)
		       (item kom-review-tree)
		       (item kom-find-root)))
	   (menu dont-read((item kom-jump)
			   (item kom-set-unread)))
	   (menu write ((item kom-write-text)
			(item kom-send-letter)
			(item kom-write-comment)
			(item kom-comment-previous)
			hline
			(item kom-send-message)))
	   (menu move ((item kom-go-to-conf)
		       (item kom-go-to-next-conf)))
	   (menu info ((item kom-who-is-on)
		       (item kom-list-news)
		       hline
		       (item kom-status-person)
		       (item kom-status-conf)
		       (item kom-review-presentation)
		       hline
		       (item kom-list-conferences)
		       (item kom-list-persons))))))
  "The menus used in LysKOM.")

(defvar lyskom-edit-menus
  '((menu lyskom
	  ((menu send ((item kom-edit-send)
		       (item kom-edit-send-anonymous)))
	   (item kom-edit-quit)
	   hline
	   (menu recievers ((item kom-edit-add-recipient)
			    (item kom-edit-add-copy)))
	   (menu commented ((item kom-edit-show-commented)
			    (item kom-edit-insert-commented))))))
  "The menus for editing LysKOM messages.")

(defvar lyskom-menu-map nil
  "A keymap describing the LysKOM top menu.")

(when (not lyskom-menu-map) 
  (setq lyskom-menu-map (make-sparse-keymap)))

(defvar lyskom-edit-menu-map nil
  "A keymap the LysKOM menu in the edit buffer.")

(when (not lyskom-edit-menu-map)
  (setq lyskom-edit-menu-map (make-sparse-keymap)))

(defun lyskom-build-menus ()
  "Create menus from according to LYSKOM-MENUS"
  (interactive)
  ;; Menu in lyskom-mode
  (define-key lyskom-mode-map [menu-bar]
    lyskom-menu-map)
  (define-key lyskom-mode-map [menu-bar edit]
    'undefined)
  (lyskom-define-menu lyskom-menu-map lyskom-menus)
  ;; Menu in lyskom-edit-mode
  (define-key lyskom-edit-mode-map [menu-bar]
    lyskom-edit-menu-map)
  (lyskom-define-menu lyskom-edit-menu-map lyskom-edit-menus))
  

(defun lyskom-define-menu (map menus)
  (when menus
    (lyskom-define-menu map (cdr menus))
    (cond ((eq 'hline (car menus))
	   (define-key map [separator] '("--")))
	  ((eq 'menu (car (car menus)))
	   (let* ((symbol (car (cdr (car menus))))
		  (name (lyskom-get-string symbol 'menu))
		  (submenu (car (cdr (cdr (car menus)))))
		  (submap (make-sparse-keymap name)))
	     (define-key map (vector symbol)
	       (cons name submap))
	     (lyskom-define-menu submap submenu)))
	  ((eq 'item (car (car menus)))
	   (let* ((symbol (car (cdr (car menus))))
		  (name (lyskom-get-string symbol 'menu)))
	     (define-key map (vector symbol)
	       (cons name symbol))))
	  (t (error "Menu description invalid in lyskom-define-menu")))))

		   
(defun lyskom-background-menu (pos event)
  "Pop up a menu with LysKOM commands and execute the selected command."
  (let* ((menu (lookup-key lyskom-menu-map [lyskom]))
	 (result (x-popup-menu event (list menu)))
	 (command (and result
		       (lookup-key menu
				   (apply 'vector result)))))
    (if command
	(call-interactively command))))




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
