;;;;;
;;;;; $Id: menus.el,v 44.3 1996-10-08 02:58:49 davidk Exp $
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
;;;; Author: Niels Möller
;;;;
;;;;


(setq lyskom-menus
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
			   (item kom-list-persons)))))))

(setq lyskom-menu-names
      '((lyskom . "LysKOM")
	(read . "Läs")
	(dont-read . "Hoppa")
	(write . "Skriv")
	(move . "Gå")
	(info . "Om")))

(setq lyskom-menu-map (make-sparse-keymap))

(defun lyskom-build-menus ()
  (define-key lyskom-mode-map [menu-bar]
    lyskom-menu-map)
  (define-key lyskom-mode-map [menu-bar edit]
    'undefined)
  (lyskom-define-menu lyskom-menu-map lyskom-menus))

(defun lyskom-define-menu (map menus)
  (when menus
    (lyskom-define-menu map (cdr menus))
    (cond ((eq 'hline (car menus))
	   ; ignore
	   t)
	  ((eq 'menu (car (car menus)))
	   (let* ((symbol (car (cdr (car menus))))
		  (name (lyskom-get-string symbol lyskom-menu-names))
		  (submenu (car (cdr (cdr (car menus)))))
		  (submap (make-sparse-keymap name)))
	     (define-key map (vector symbol)
	       (cons name submap))
	     (lyskom-define-menu submap submenu)))
	  ((eq 'item (car (car menus)))
	   (let* ((symbol (car (cdr (car menus))))
		  (name (lyskom-get-string symbol lyskom-commands)))
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




;;;;(Återse) Baklänges		   Addera extra kopiemottagare
;;;;Addera kommentar		   Addera medlem
;;;;Addera mottagare		   Automatsvar
;;;;Avmarkera (inlägg)		   Bli medlem i möte
;;;;Börja med nytt namn		   Endast läsa senaste
;;;;Filtrera författare		   Filtrera innehåll
;;;;Filtrera ärende			   Fjärrkontrollera automatsvar
;;;;Fjärrkontrollera avsluta	   Fjärrkontrollera lista meddelanden
;;;;Fjärrkontrollera radera meddelanden
;;;;Fjärrkontrollera ändra svarsmeddelande
;;;;Flytta inlägg			   Fotnot till inlägg
;;;;Få skäll			   Få uppmuntran
;;;;Gå till möte			   Gå till nästa möte
;;;;Hjälp				   Hoppa över alla kommentarer
;;;;Kasta ut en session		   Kommentera föregående inlägg
;;;;Kommentera inlägget		   Ladda ner fil
;;;;Ladda upp fil			   Lista (med) regexpar
;;;;Lista filarean			   Lista filter
;;;;Lista klienter			   Lista meddelanden
;;;;Lista medlemsskap		   Lista möten
;;;;Lista nyheter			   Lista personer
;;;;Lista ärenden			   Läsa nästa inlägg
;;;;Långsamma kommandon		   Markera (inlägg)
;;;;Personligt svar			   Personligt svar på föregående inlägg
;;;;Prioritera möten		   Radera inlägg
;;;;Radera meddelanden		   Se tiden
;;;;Skapa möte			   Skicka brev
;;;;Skriva ett inlägg		   Sluta
;;;;Snabba kommandon		   Spara text (på fil)
;;;;Spara variabler			   Status (för) möte
;;;;Status (för) person		   Status (för) session
;;;;Subtrahera kommentar		   Subtrahera mottagare
;;;;Superhoppa			   Sända meddelande
;;;;Sätt lapp på dörren		   Sätt läsnivå
;;;;Ta bort lapp på dörren		   Uppskjuta läsning
;;;;Uteslut medlem			   Utplåna
;;;;Utträda ur möte			   Var (är) jag
;;;;Vilka är inloggade		   Visa user-arean
;;;;Vänta på ett inlägg		   Ändra filter
;;;;Ändra livslängd			   Ändra lösenord
;;;;Ändra mötestyp			   Ändra namn
;;;;Ändra organisatör		   Ändra presentation
;;;;Ändra supermöte			   Ändra svarsmeddelande
;;;;Ändra tillåtna författare	   Ändra variabler
;;;;Återse alla			   Återse alla kommentarer
;;;;Återse alla kommentarer rekursivt  Återse alla markerade
;;;;Återse det föregående kommenterade
;;;;Återse det kommenterade		   Återse första
;;;;Återse hoppa			   Återse igen
;;;;Återse inlägg			   Återse lista
;;;;Återse markerade		   Återse nästa
;;;;Återse omodifierat		   Återse presentation
;;;;Återse senaste			   Återse träd
;;;;Återse urinlägget		   Återstarta kom
;;;;Övergå till administratörsmod
