;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: swedish-strings.el,v 44.54 1999-06-11 12:56:16 byers Exp $
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
;;;; File: swedish-strings.el
;;;;
;;;; This file contains all strings in the LysKOM elisp client.
;;;; Language: Swedish.
;;;;
;;;; ================================================================
;;;;

(require 'lyskom-vars "vars")
(require 'lyskom-language "language")

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: swedish-strings.el,v 44.54 1999-06-11 12:56:16 byers Exp $\n"))


;;; ================================================================
;;; The language definition

(lyskom-define-language
 'sv
 "Svenska")


;;; ================================================================
;;; lyskom-edit-mode-map

(defvar lyskom-sv-edit-mode-map nil)
(lyskom-language-keymap lyskom-edit-mode-map sv lyskom-sv-edit-mode-map)

;;; Set the keymap for lyskom-edit-mode

(defvar lyskom-sv-edit-prefix nil)

(if lyskom-sv-edit-mode-map
    nil
  (setq lyskom-sv-edit-mode-map (make-sparse-keymap))
  (define-prefix-command 'lyskom-sv-edit-prefix)
  (define-prefix-command 'lyskom-sv-edit-review-prefix)
  (define-prefix-command 'lyskom-sv-edit-insert-prefix)
  (define-prefix-command 'lyskom-sv-edit-aux-prefix)
  (define-prefix-command 'lyskom-sv-edit-add-prefix)
  (define-key lyskom-sv-edit-mode-map (kbd "C-c")   'lyskom-sv-edit-prefix)
  (define-key lyskom-sv-edit-mode-map (kbd (lyskom-keys 'button2up)) 'kom-button-click-or-yank)
  (define-key lyskom-sv-edit-mode-map (kbd (lyskom-keys 'button2)) 'kom-mouse-null)
  (define-key lyskom-sv-edit-mode-map (kbd (lyskom-keys 'button3)) 'kom-popup-menu)
  (define-key lyskom-sv-edit-mode-map (kbd (lyskom-keys 'button3up))     'kom-mouse-null)
  (define-key lyskom-sv-edit-prefix (kbd "C-x")     'lyskom-sv-edit-aux-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "?")       'lyskom-help)
  (define-key lyskom-sv-edit-prefix (kbd "}")       'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "]")       'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-]")     'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-}")     'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-�")     'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-�")     'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "*")       'kom-button-press)
  (define-key lyskom-sv-edit-prefix (kbd "+")       'kom-menu-button-press)
  (define-key lyskom-sv-edit-prefix (kbd "C-i")     'lyskom-sv-edit-insert-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-c")     'kom-edit-send)
  (define-key lyskom-sv-edit-prefix (kbd "C-s")     'kom-ispell-message)
  (define-key lyskom-sv-edit-prefix (kbd "C-k")     'kom-edit-quit)
  (define-key lyskom-sv-edit-prefix (kbd "} ?")     'lyskom-help)
  (define-key lyskom-sv-edit-prefix (kbd "} C-k")   'kom-edit-show-commented)
  (define-key lyskom-sv-edit-prefix (kbd "} k")     'kom-edit-show-commented)
  (define-key lyskom-sv-edit-prefix (kbd "C-i ?")   'lyskom-help)
  (define-key lyskom-sv-edit-prefix (kbd "C-i C-k") 'kom-edit-insert-commented)
  (define-key lyskom-sv-edit-prefix (kbd "C-y")     'kom-edit-insert-commented)
  (define-key lyskom-sv-edit-prefix (kbd "C-i 1")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-prefix (kbd "C-i 2")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-prefix (kbd "C-i 3")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-prefix (kbd "C-i 4")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-prefix (kbd "C-i 5")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-prefix (kbd "C-i 6")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-prefix (kbd "C-i 7")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-prefix (kbd "C-i 8")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-prefix (kbd "C-i 9")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-prefix (kbd "C-i SPC") 'kom-edit-insert-text)
  (define-key lyskom-sv-edit-prefix (kbd "C-a")     'lyskom-sv-edit-add-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-a C-m") 'kom-edit-add-recipient)
  (define-key lyskom-sv-edit-prefix (kbd "C-a <RET>") 'kom-edit-add-recipient)
  (define-key lyskom-sv-edit-prefix (kbd "C-a C-j") 'kom-edit-add-recipient)
  (define-key lyskom-sv-edit-prefix (kbd "C-a <LFD>") 'kom-edit-add-recipient)
  (define-key lyskom-sv-edit-prefix (kbd "C-a C-k") 'kom-edit-add-comment)
  (define-key lyskom-sv-edit-prefix (kbd "C-a C-e") 'kom-edit-add-copy)
  (define-key lyskom-sv-edit-prefix (kbd "C-a C-f") 'kom-edit-move-text)
  (define-key lyskom-sv-edit-prefix (kbd "C-a C-x") 'kom-edit-add-cross-reference)
  (define-key lyskom-sv-edit-prefix (kbd "C-x C-p") 'kom-edit-add-personal-comments)
  (define-key lyskom-sv-edit-prefix (kbd "C-x C-n") 'kom-edit-add-no-comments)
  (define-key lyskom-sv-edit-prefix (kbd "C-x C-b") 'kom-edit-add-read-confirm-request)
  (define-key lyskom-sv-edit-prefix (kbd "C-a ?")   'lyskom-help))



(defconst lyskom-strings-missing '())



;;; The alist formely known as lyskom-strings
(lyskom-language-strings lyskom-message sv
  '(
    ;; From vars.el: 
    ;; From komtypes.el: nil
    ;; From clienttypes.el: nil
    ;; From startup.el:
    (server-q . "LysKOM-server? (%#1s) ")
    (try-connect . "LysKOM elisp-klient version %#1s.\nF�rs�ker koppla upp mot %#2s.\n")
    (protocoll-error . "Protocol error. Servers says: %#1s")
    (too-old-server . "Servern �r f�r gammal f�r den h�r versionen av klienten.")
    (connection-done . "Uppkopplingen klar. Serverns versionsnummer �r %#1s.\n\n")
    (what-is-your-name . "Vad heter du? ")
    (password . "L�senord? ")
    (wrong-password . "Fel l�sen.\n")
    (are-logged-in . "Du �r nu inloggad. V�nta ett tag.\n")
    (you-have-motd . "\nDu har en lapp p� d�rren:\n\n")
    (lyskom-motd-was-garbed . "\nLoginmeddelandet finns inte!
Det meddelande som ska visas efter inloggning har f�rsvunnit.
Kontakta LysKOM-administrat�ren.\n")
    (presentation-encouragement . 
				"Du har ingen presentation. Det skulle vara trevligt om du skrev en.
Anv�nd kommandot �p. Om du inte vill skriva n�gon presentation tryck fs.\n")

    (first-greeting . "%#1s
Det tycks vara f�rsta g�ngen du anv�nder LysKOM. V�lkommen!
Kontrollera att du stavat ditt namn r�tt.  Anv�nd g�rna ditt fullst�ndiga
namn och organisation, t ex \"Eskil Block, FOA\".  Om du stavat ditt namn
fel, eller vill �ndra ditt namn, svara nej p� fr�gan nedan.

Observera att all information �n s� l�nge sparas s� att vem som helst
kan l�sa den. Dock �r l�senordet krypterat.

Om du �r os�ker p� hur man anv�nder LysKOM kan du h�mta en manual via
anonym ftp fr�n ftp.lysator.liu.se. Fr�ga din systemadministrat�r om
du �r os�ker.\n")

    (is-name-correct . "�r namnet %#1s korrekt? ")
    (personal-password . "Ange ett personligt l�sen: ")
    (repeat-password . "Repetera f�r kontroll: ")
    (repeat-failure . "Du angav inte samma l�senord b�gge g�ngerna.\n")

    (could-not-create-you .  "Det gick inte att skapa personen.\n")
    (presentation-subject . "%#1s")
    (presentation-form . "Namn:\t\nAdress:\t\nStad:\t\nTelefon:
Email-adress:\nWWW:\t\n\nAnnat:\t")
    (presentation-help . "Du skriver just  nu din presentation.\n")
    (not-present-anywhere . "Ej n�rvarande i n�got m�te.")
    (secret-person . "Hemlig person")
    (in-secret-conference . "Hemligt m�te (%#1d).")
    (start-new-session-same-server
     . "Du k�r redan mot den servern. Vill du starta en ny session? ")
    (new-session-in-buffer . "\n\n---- Ny session startad %s ----\n\n")


    ;; From internal.el:
    (shaky-tcp . "Nu n�r jag inte servern. TCP/IP-f�rbindelsen �r skakig%#1s")
    (retrying-tcp . "Jag f�rs�ker igen.")

    ;; From parse.el:
    (protocol-error . "protokollfel: %s")

    ;; From services.el:
    (interrupted . "Kommandot avbrutet\n")

    ;; From cache.el:
    ;; No entries.

    ;; From commands1.el:
    (appreciation . "Du �r mycket vacker och mycket klok. M�nga �lskar dig b�de till kropp
och till sj�l. Du kommer att �ver�sas med rikedom och f� stor lycka i ditt
liv. Var glad att just du �r du. Det har du all anledning att vara.
Fantomen �nskar dig en bra dag.\n\n")
    (abuse . "Du �r mycket ful och mycket dum. M�nga hatar dig b�de till kropp
och till sj�l. Du kommer att �ver�sas med sjukdomar och inte f� n�gon som
helst lycka i ditt liv. Du borde verkligen ta dig samman och g�ra n�got av
ditt liv �ven om det inte �r mycket att starta med.
Guran vill helst s�tta en giftpil i dig.\n\n")

    (what-conf-to-delete . "Vilket m�te/person vill du utpl�na: ")
    (what-conf-to-change . "Vilket m�te vill du �ndra: ")
    (confirm-delete-pers-or-conf . "Ta bort %#1s %#2s? ")
    (the-pers . "personen")
    (the-conf . "m�tet")
    (deletion-not-confirmed . "Utpl�ningen avbruten\n")
    (somebody-else-deleted-that-conf . "N�gon annan tog precis bort m�tet.\n")
    (conf-is-deleted . "Ok, nu �r %#1s utpl�nad.\n")
    (you-could-not-delete . "%#1M kunde inte utpl�nas av dig.\n")
    (you-have-deleted-yourself . "Du har utpl�nat dig sj�lv.\n")

    (what-text-to-delete . "Vilket inl�gg skall tas bort? ")
    (delete-marked-text . "Inl�gget �r %#1s. Ta bort �nd�? ")
    (delete-marked-by-you . "markerat av dig")
    (delete-marked-by-you-and-one . "markerat av dig och n�gon annan")
    (delete-marked-by-you-and-several . "markerat av dig och %#1d andra")
    (delete-marked-by-one . "markerat av 1 person")
    (delete-marked-by-several . "markerat av %#1d personer")
    (deleting-text . "Radering av text %#1:n...")

    (presentation-for-whom . "Vilket m�te/person? ")
    (somebody-deleted-that-conf . "N�gon tog precis bort m�tet.\n")
    (review-presentation-of . "�terse presentation av %#1M.\n")
    (has-no-presentation . "%#1:M har ingen presentation.\n")

    (have-to-read . "Du m�ste l�sa ett inl�gg f�rst.\n")

    (no-comment-to . "Det finns inget kommenterat inl�gg att titta p�.\n")

    (who-letter-to . "Vem vill du skicka brev till? ")
    (who-send-text-to . "Vem vill du skicka inl�gget till? ")
    (has-motd . "%#1P har en lapp p� d�rren:\n\n")
    (motd-persist-q . "Vill du fortfarande skicka brevet? ")

    (who-to-add . "Vem vill du addera? ")
    (where-to-add . "Vilket m�te skall han/hon adderas till? ")
    (where-to-add-self . "Vilket m�te vill du bli medlem i? ")
    (priority-q . "Prioritet p� m�tet? (0 (passivt medlemskap), 1 (l�g) - 255 (h�g)) ")
    (done . "klart.\n")
    (nope . "Det gick inte.\n")

    (cant-find-supervisor . "Hittar inte organisat�ren f�r %#1M.\n")
    (is-read-protected-contact-supervisor . "%#1M �r slutet.
Skicka ett brev till %#2P f�r medlemsskap.\n")

    (conf-does-not-exist . "\nM�tet finns inte.\n")

    (who-to-exclude . "Vem vill du utesluta? ")
    (where-from-exclude . "Vilket m�te skall han/hon uteslutas ifr�n? ")

    (leave-what-conf . "Vilket m�te vill du g� ur? ")

    (error-fetching-person . "Fel i personh�mtningen.\n")
    (error-fetching-conf . "Fel i m�tesh�mtningen.\n")

    (name-of-conf . "Vad ska m�tet heta? ")
    (anyone-member . "F�r vem som helst bli medlem? ")
    (secret-conf . "Hemligt m�te? ")
    (comments-allowed . "F�r man skriva kommentarer? ")
    (anonymous-allowed . "F�r man skriva anonyma texter? ")
    (secret-members-allowed . "F�r man g� med som hemlig medlem? ")
    (what-comment-no . "Kommentera text nummer: ")
    (what-footnote-no . "Fotnotera text nummer: ")
    (confusion-what-to-comment . "Jag f�rst�r inte vilken text du vill kommentera.\n")
    (confusion-what-to-footnote . "Jag f�rst�r inte vilken text du vill skriva en fotnot till.\n")

    (what-private-no . "Personligt svar till text nummer: ")
    (confusion-who-to-reply-to . "Jag f�rst�r inte vems inl�gg du vill
 skriva ett privat svar till.\n")
    (confusion-what-to-answer-to . "Jag f�rst�r inte vilken text du vill besvara.\n")
    (confusion-what-to-view . "Jag f�rst�r inte vilken text du vill �terse.\n")
    (quit-in-spite-of-unsent . "Vill du avsluta sessionen trots os�nt meddelande? ")
    (really-quit . "Vill du verkligen avsluta sessionen? ")
    (session-ended . "
*****************************
Lyskom-sessionen �r avslutad.
*****************************\n")
    (session-auto-ended . "
===========================================================
Kopplar ned fr�n LysKOM eftersom LysKOM �r fullt och
du har l�st klart allting. Kom tillbaks senare.
===========================================================\n\n")
    (what-to-change-pres-you . "Vilket m�te/person vill du �ndra presentationen f�r (dig sj�lv): ")
    (who-to-put-motd-for . "Vilket m�te/person vill du s�tta lapp p� d�rr f�r (dig sj�lv): ")

    (cant-get-conf-stat . "Kan ej h�mta m�tesstatus f�r m�tet.\n")
    (go-to-conf-p . "G� till m�te: ")
    (want-become-member . "Vill du bli medlem? ")
    (no-ok . "Nehej.\n")

    (who-to-remove-motd-for . "Vilket m�te/person vill du ta bort lapp p� d�rr f�r (dig sj�lv): ")

    (conf-all-read . "%#1M - inga ol�sta.\n")
    (no-in-conf . "Du �r inte n�rvarande i n�got m�te.\n")

    (search-for-pers . "Ange s�kvillkor (RETURN f�r alla personer): ")
    (search-for-conf . "Ange s�kvillkor (RETURN f�r alla m�ten): ")
    (search-re . "Ange s�kuttryck (regulj�rt uttryck): ")
    (name-to-be-changed . "Ange det namn som skall �ndras: ")
    (no-such-conf-or-pers . "M�tet eller personen finns ej.\n")
    (new-name . "Nytt namn: ")
    (new-paren . "Ny parentes: ")
    (no-paren-in-name . "Namnet inneh�ller ingen parentes.\n")
    (who-to-change-supervisor-for . "Vem vill du �ndra organisat�r f�r? ")
    (new-supervisor . "Ny organisat�r: ")
    (text-to-mark . "Vilket inl�gg vill du markera? ")
    (text-to-unmark . "Vilket inl�gg vill du avmarkera? ")
    (what-mark . "Vilken markering vill du s�tta? ")
    (unmarking-textno . "Avmarkering av text %#1n...")
    (marking-textno . "Markering av text %#1n...")
    (list-which-mark . "Lista vilken markering (0-255, RET f�r alla)? ")

    (new-passwd-again . "Mata in det nya l�senordet igen f�r kontroll: ")
    (what-mark-to-view . "Vilken markering vill du �terse? ")
    (whos-passwd . "Vem vill du �ndra l�senord f�r? (dig sj�lv) ")
    (old-passwd . "Mata in ditt nuvarande l�senord: ")
    (new-passwd . "Mata in det nya l�senordet: ")
    (changing-passwd . "�ndrar l�senordet...")
    (retype-dont-match . "L�senorden �r inte samma. G�r om.\n")
    (palindrome . "(en palindrom!) ")
    (lyskom-name . "Anv�ndare")
    (is-in-conf . "N�rvarande i m�te")
    (from-machine . "K�r fr�n")
    (is-doing . "G�r")
    (connection-time . "Loggade in")
    (active-last . "Aktiv senast")
    (active . "Aktiv")
    (lyskom-client . "Klient")
    (text-to-add-recipient . "Vilket inl�gg vill du addera mottagare till:")
    (text-to-add-copy . "Vilket inl�gg vill du addera en extra kopia till:")
    (text-to-add-bcc . "Vilket inl�gg vill du skicka f�r k�nnedom:")
    (text-to-delete-recipient . "Vilket inl�gg vill du subtrahera mottagare fr�n:")
    (text-to-move . "Vilket inl�gg vill du flytta: ")
    (text-to-add-comment-to . "Vilket inl�gg vill du addera en kommentar till:")
    (text-to-delete-comment-from . "Vilket inl�gg vill du subtrahera en kommentar fr�n:")

    (where-on-list-q . "Placering p� listan? (0-%#1d) ")
    (member-in-conf . "Bli medlem i %#1M...")
    (add-member-in . "Addera %#1P som medlem i %#2M...")
    (unsubscribe-to . "Uttr�da ur %#1M...")

    (exclude-from . "Utesluta %#1P fr�n %#2M...")

    (unsubscribe-failed . "\nDet gick inte. %#1P var kanske aldrig medlem i %#2M?\n")

    (You . "Du")
    (could-not-create-conf . "M�tet \"%#1s\" kunde ej skapas.\n")
    (created-conf-no-name . "M�te nummer %[%#3@%#1:m %#2:M%], skapat.\n")
    (cant-read-textno . "Du f�r inte l�sa text %#1:n")

    (not-supervisor-for . "Du �r inte organisat�r f�r %#1M.\n")
    (go-to-conf . "G� till %#1M.\n")
    (cant-go-to-his-mailbox . "Du f�r inte g� till %#1Ms brevl�da.\n")
    (not-member-of-conf . "Du �r inte medlem i %#1M.\n")
    (about-to-change-name-from . "%#1M\n")
    (change-name-done . "Klart. Nytt namn: %[%#2@%#1:M%].\n")
    (change-name-nope . "Det gick inte bra att �ndra till %#1s.\nFelkod %#3d. %#2s.\n")
    (change-supervisor-from-to . "�ndra organisat�r f�r %#1M till %#2P...")
    (change-supervisor-nope . 
			    "\nDet gick inte. Kanske du inte f�r �ndra organisat�r f�r %#1M?\n")
    
    (no-marked-texts . "Du har inga markerade inl�gg.\n")
    (no-marked-texts-mark . 
			  "Du har inga markerade inl�gg med markeringen %#1d.\n")

;;; For later
;    (northward . "norrut")
;    (southward . "s�derut")
;    (permanent-sundown . "Solen kommer inte att g� upp. Flytta %#1s!")
;    (permanent-sunup . "Solen kommer inte att g� ned. Fytta %#1s!")
;    (sunup-soon . "Solen g�r snart upp")
;    (sundown-recently . "Solen gick nyligen ned")
;    (after-sunset . "Solen har g�tt ned")
;    (before-sunup . "Solen har inte g�tt upp �nnu")
;    (sun-is-up . "Solen �r uppe")
;;;
    (weekdays . ["s�ndag" "m�ndag" "tisdag" "onsdag" "torsdag"
		 "fredag" "l�rdag" "s�ndag"])
    (time-is . "Det �r %#1s %#2s(enligt servern).")
    (time-format-exact . "%#7s %4#1d-%02#2d-%02#3d %02#4d:%02#5d:%02#6d")

    (xmaseve . "\nJulafton! Har du �ppnat dina julklappar �n?")
    (xmasday . "Juldagen.\nDu har v�l varit i julottan?")
    (newyearday . "Gott nytt %#1d!")
    (newyearevelate . "Mindre �n en timme kvar...")
    (newyeareve . "Gott nytt �r!")
    (cgdag . "Konungens f�delsedag.")
    (sixjune . "Sveriges nationaldag och svenska flaggans dag")
    (holdnose . "H�ll f�r n�san...")
    (lysbday . "
P� denna dag, �r 1973, grundades Lysator, och det var en stor dag
i svensk datorhistoria. L�s mer p� http://www.lysator.liu.se/history/")


    (total-users . "    Sammanlagt %#1d anv�ndare.\n")
    (total-visible-users . "    Sammanlagt %#1d synliga anv�ndare.\n")
    (total-active-users . "    Sammanlagt %#1d aktiva anv�ndare.\n")
    (total-visible-active-users
     . "    Sammanlagt %#1d synliga aktiva anv�ndare.\n")
    (who-to-add-q . "Vilket m�te/person vill du addera som mottagare? ")
    (who-to-add-copy-q . "Vilket m�te/person vill du addera som kopiemottagare? ")
    (who-to-sub-q . "Vilket m�te/person vill du subtrahera som mottagare? ")
    (who-to-move-from-q . "Fr�n vilket m�te vill du flytta texten? ")
    (who-to-move-to-q . "Vart vill du flytta texten? ")

    (adding-name-as-recipient . "Adderar %#1M som mottagare till text %#2n...")
    (adding-name-as-copy . "Adderar %#1M som kopiemottagare till text %#2n...")
    (remove-name-as-recipient .
			      "Subtraherar %#1M som mottagare fr�n text %#2n...")
    (moving-name . "Flyttar text %#3n fr�n %#1M till %#2M...")
    (text-to-add-q . "Vilket inl�gg vill du addera som kommentar? ")
    (text-to-remove-q . "Vilket inl�gg vill du subtrahera som kommentar? ")
    (add-comment-to . "Adderar text %#1n som kommentar till text %#2n...") ;
    (sub-comment-to . "Subtraherar text %#1n som kommentar till text %#2n...")
    (comment-keep-recpt-p ."Ska %#1s vara mottagare? ")
    (comment-all-relevant-p . "Inl�gget har flera mottagare. �r alla relevanta? ")
    (please-edit-recipients . "�ndra mottagarlistan och skicka in inl�gget igen.")
    (checking-rcpt . "Kontrollerar mottagare...")
    (checking-rcpt-done . "Kontrollerar mottagare...klart")
    (checking-comments . "Kontrollerar kommenterade texter...")
    (checking-comments-done . "Kontrollerar kommenterade texter...klart")
    (please-check-commented-texts . "�terse de kommenterade texterna och deras kommentarer.")
    (have-unread-comment . "Skicka in trots ol�sta kommentarer till text %#1n? ")

    (add-recipient-p . "Addera %#1P som mottagare? ")
    (matching-regexp . "M�ten/personer som matchar '%#1s'\n")

    (who-is-active-all . "Visar alla sessioner.\n")
    (who-is-active-last-minutes . "Visar alla sessioner som har varit aktiva de senaste %#1d minuterna.\n")
    (showing-invisibles . "Visar osynliga sessioner.\n")
    (null-who-info . "Det finns inga (aktiva) inloggade.\n")

    (no-other-lyskom-r . "Det finns inga fler aktiva LysKOM-sessioner.\n")
    (no-lyskom-session . "Det finns ingen aktiv LysKOM-session.")
    (no-unread-lyskom-r . "Hittar ingen aktiv LysKOM-session med ol�sta.\n")
    (no-unread-lyskom . "Hittar ingen aktiv LysKOM-session med ol�sta.")

    (who-is-on-in-what-conference . "Vilka i vilket m�te: ")
    (who-is-active-and-member . "Visar endast medlemmar i %#1M.\n")
    
    ;; From commands2.el:

    (your-memberships . "Ditt medlemskap i olika KOM-m�ten:\n")
    (memberships-header . "Senast inne	   Prio	Ol�sta	M�tesnamn\n")
    (memberships-line . "%16#1s  %#2d\t%#3d\t%#4M\n")
    (conf-for-status . "Vilket m�te vill du se statusen f�r? ")
    (no-such-conf . "M�tet finns ej.\n")
    (status-record . "Status f�r m�te %#1M (%#2m) %#3s\n\n")
    (change-type-prompt . "�ndra m�testyp f�r m�te %#1M (%#2m) %#3s\n")
    (Mailbox . "Brevl�da")
    (Protected . "Skyddat")
    (no-comments . "original")
    (closed . "slutet")
    (allow-anon . "anonymt ok")
    (allow-secret . "hemliga medl")

    (created-by . "Skapat av person %25#1p %#3s(%#2P)\n")
    (created-at . "Skapad:%35#1s\n")
    (members .    "Antal medlemmar: %25#1d\n")
    (conf-allows-secret-members . "Hemliga medlemmar:                      %#1s\n")
    (conf-allows-anon-texts .     "Anonyma inl�gg:                         %#1s\n")
    (anon-texts-permitted . "Anonyma inl�gg �r till�tna")
    (anon-texts-not-permitted . "Anonyma inl�gg �r inte till�tna")
    (secret-members-permitted . "Hemliga medlemmar �r till�tna")
    (secret-members-not-permitted . "Hemliga medlemmar �r inte till�tna")
    (garb-nice . "Livsl�ngd p� inl�gg (dagar):%14#1d\n")
    (lowest-local-no . "L�gsta existerande lokala nummer: %8#1d\n")
    (highest-local-no . "H�gsta existerande lokala nummer: %8#1d\n")
    (last-text-time . 
		    "Tid f�r senaste inl�gg: %18#1s (st�r det i din cache)\n")
    (no-of-motd . "Lapp p� d�rren i text nummer: %12#1n\n")
    (superconf-is-no-name . "Superm�te:       %25#1m %#3s(%#2M)\n")
    (permitted-submitters-no-name . "Till�tna f�rfattare:%22#1m %#3s(%#2M)\n")
    (supervisor-is-no-name . "Organisat�r:     %25#1p %#3s(%#2P)\n")
    (presentation-no . "Presentation:    %25#1n\n")
    (conf-has-motd . "\n%#1M har en lapp p� d�rren:\n")

    (Everybody . "Alla")
    (show-members-list-also-q . "Vill du se medlemslistan ocks�? ")
    (show-membership-info-q . "Visa antalet ol�sta? ")
    (conf-has-these-members . "\n%#1M har f�ljande medlemmar:\n")
    (conf-has-no-members . "\n%#1M har inga medlemmar.\n")
    (member-list-header . "Senast inne         Osett  Namn\n\n")
    (secret-membership . "*** Hemlig rad ***\n")
    (conf-membership-line . "%#1s%#2M %#3s\n")
    (conf-membership-line-2 . "                          Adderad %#1s av %#2P\n")
    (pers-for-status . "Vem vill du se statusen f�r? ")
    (no-such-pers . "Det finns ingen s�dan person.\n")
    (pers-status-record . "Status f�r person %#1P (%#2p)\n")
    (created-time .  "Skapad:%34#1s\n\n")
    (created-confs . "Skapade m�ten:%27#1d\n")
    (created-persons . "Skapade personer:%24#1d\n")
    (created-texts . "Skapade texter:%26#1d\n")
    (created-lines . "Skapade rader:%27#1d\n")
    (created-chars . "Antal skapade tecken:%20#1d\n")
    (no-of-sessions . "Antal sessioner:%25#1d\n")
    (present-time-d-h-m-s . "N�rvarotid:%19#1d d %02#2d:%02#3d:%02#4d\n")
    (last-log-in . "Senast inne:%29#1s\n")
    (user-name . "Anv�ndare: %30#1s\n")

    (read-texts . "L�sta texter:%28#1d\n")
    (marked-texts . "Markerade texter:%24#1d\n")
    (time-for-last-letter . "Tid f�r senaste brev:%20#1s (st�r det i din cache)\n")
    (superconf . "Superm�te:%31#1m %#3s(%#2M)\n")
    (supervisor . "Organisat�r:%29#1p %#3s(%#2P)\n")
    (member-of-confs . "Medlem i (antal m�ten):%18#1d\n")
    (presentation . "Presentation:    %24#1n\n")
    (show-membership-list-also-q . "Vill du se vilka m�ten personen �r medlem i ocks�? ")
    (not-allowed-see-confs . "Du f�r inte se vilka m�ten %#1P �r medlem i.\n")
    (is-member-of . "\n%#1P �r medlem i f�ljande m�ten:\n")
    (membership-list-header . "Senast inne         Osett  Namn\n\n")
    (pers-membership-line . "%#1s%#2s%#3M %#4s\n")
    (pers-membership-line-2 . "                          Adderad %#1s av %#2P\n")
    (is-supervisor-mark . "O ")
    (who-to-send-message-to . "Vem vill du skicka meddelandet till? (%s) ")
    (send-empty-message-p . "Meddelandet �r tomt. Vill du �nd� skicka det? ")
    (his-total-unread . "\n%#1M har totalt %#2d ol�sta.\n")
    (message-prompt . "Meddelande: ")
    (message-sent-to-user
     . "================================================================
Ditt meddelande till %#2M:

%#1t
----------------------------------------------------------------
")
    (message-sent-to-all
     . "================================================================
Ditt allm�nna meddelande l�d:

%#1t
----------------------------------------------------------------
")
    (message-use-alarm-instead . "Anv�nd kommandot %#1s f�r att skicka alarmmeddelanden.\n")
    (message-all-info . "S�nd alarmmeddelande\n")
    (message-recipient-info . "S�nd meddelande till %#1M\n")
    (message-nope
     . "Du kunde inte skicka meddelandet. Mottagaren var kanske inte inloggad.
Meddelandet du f�rs�kte s�nda till %#1M var:
%#2t\n")
    (only-last . "Endast l�sa senaste (0 - %#1d) i %#2s: ")
    (only-error . "N�got gick galet. Sorry.\n")

    (you-have-unreads . "Du har %#1d ol�sta inl�gg i %#2M\n")
    (you-have-an-unread . "Du har 1 ol�st inl�gg i %#1M\n")
    (you-have-unreads-special . "Du har %#1d okommenterade inl�gg i %#2M\n")
    (you-have-an-unread-special . "Du har 1 okommenterat inl�gg i %#1M\n")
    (you-have-read-everything . "Du har sett alla nyheter.\n")
    (total-unreads . "\nDu har %#1d ol�sta inl�gg.\n")
    (total-unread . "\nDu har 1 ol�st inl�gg.\n")
    (waiting-for-anything .
			  "Du v�ntar p� ett inl�gg i vilket m�te som helst.\n")
    (waiting-higher-than . 
			 "Du v�ntar p� ett inl�gg i ett m�te med h�gre prioritet �n %#1d.\n")

    (have-to-be-in-conf-with-unread . "Du m�ste g� till ett icketomt m�te f�rst.\n")
    (Texts . "Inl�gg")
    (Date . "Datum")
    (Lines . "Rad.")
    (Author . "F�rfattare")
    (Subject . "�rende")
    (mark-type . "Typ")
    (could-not-read . "Du fick inte l�sa denna text (%#1n).\n")
    (multiple-choice . "Flera alternativ finns.")
    (does-not-exist . "Detta kommando finns inte.")
    (summary-line . "%=-8#1n%#2s%4#3d  %[%#4@%#5:P%]  %[%#6@%#7r%]\n")

    (what-mark-to-list . "Vilken markering vill du lista? ")
    (you-have-marks . "Du har %#1d inl�gg markerade markerade med %#2d.\n")
    (you-have-marks-all . "Du har %#1d markerade inl�gg.\n")


    ;; Only people fixing bugs or recieving bugg-reports should
    ;; change these:
    (buggreport-compilestart . "Skapar buggrapporten...")
    (buggreport-compileend . "Skapar buggrapporten...klart")
    (buggreport-description . "Detta gjorde jag:
\(Fyll i dina kommentarer nedan\)\n================\n\n
================
Bland informationen nedan finns ocks� en lista p� de 100 sist tryckta
tangenterna i din emacs. Om du nyligen loggat in kan den inneh�lla ditt
lyskoml�senord. Titta igenom den och �ndra det som �r ditt l�senord
till * * * eller m i t t l � s e n eller n�got annat l�mpligt.

N�r du skrivit klart skall du skicka in din buggrapport till LysKOMs
elispklientutvecklare. Det sker antingen:
* med email till bug-lyskom@lysator.liu.se
* med vanligt brev till:
\tLysator
\tc/o ISY
\tLink�ping University
\tS-581 83 Linkoping
\tSWEDEN.

M�rk kuvertet \"LysKOM buggrapport f�r elispklienten\".\n\n")
    
    (buggreport-internals . "LysKOMs interna information:\n\n")
    (buggreport-version . "lyskom-version:")
    (buggreport-emacs-version . "emacs-version:")
    (buggreport-system-id . "system-id:")
    (buggreport-ctl-arrow-doc . "ctrl-doc:")
    (buggreport-unparsed . "\nlyskom-unparsed-buffer:")
    (buggreport-command-keys . "Nyss tryckta tangenter:")
    (buggreport-backtrace . "\n*Backtrace*:\n%#1s\n")
    (buggreport-communications . "\nlyskom-debug-communications-to-buffer-buffer:")
    (buggreport-all-kom-variables . "\n\nAndra variabler:\n***** *********")
    (buggreport-instead-of-byte-comp . "byte-code(\"byte-string\"")
    (buggreport-subject . "Bugg-rapport elisp-klienten version %#1s")


    (not-logged-in . "Du �r inte inloggad.  ")


    ;; +++ cant seem to find where these are used:
    (name-is-not-in-conf . "%#1s �r inte n�rvarande i n�got m�te.\n")
    (name-is-in-conf . "%#1s �r n�rvarande i\n%#2s\n")
    (connected-during . "Uppkopplingstid: %#1d sekunder.\n")
    ;; +++


    (conf-to-set-permitted-submitters-q . "Vilket m�te vill du s�tta till�tna f�rfattare f�r? ")
    (conf-to-set-super-conf-q . "Vilket m�te vill du s�tta superm�te f�r? ")
    (new-super-conf-q . "Vilket m�te vill du ha som superm�te? ")
    (new-permitted-submitters-q . "M�te med till�tna f�rfattare till %#1s? (alla) ")
    (super-conf-for-is . "�ndra superm�te f�r %#1M till %#2M...")
    (permitted-submitters-removed-for-conf . "Till�t alla f�rfattare i m�te %#1M...")
    (submitters-conf-for-is . "�ndra till�tna f�rfattare f�r m�te %#1M till\nmedlemmarna i %#2M...")

    (conf-to-set-garb-nice-q . "Vilket m�te vill du s�tta livsl�ngd f�r? ")
    (new-garb-nice-q . "Vilket v�rde vill du s�tta livsl�ngden till? ")
    (garb-nice-for-is . "�ndra livsl�ngden f�r %#1M till %#2d...")

    (really-shutdown . "�r du s�ker p� att du vill st�nga av servern? ")
    (closing-server . "St�nga av servern...")
    (really-sync . "�r du s�ker p� att du vill spara databasen? ")
    (syncing-server . "Spara databasen...")
    (administrator . "administrat�r")
    (no-longer-administrator . "en normal anv�ndare igen")
    (you-are-now . "Ok, du k�r nu som %#1s.\n")
    (setting-motd . "S�tter loginmeddelandet till text %#1n.\n")
    (set-motd-success . "Du har satt ett nytt loginmeddelande.\n")
    (set-motd-failed . "Det gick inte. Du var kanske inte administrat�r.\n")
    (removing-motd . "Tar bort loginmeddelandet.\n")
    (removed-motd . "Du har tagit bort loginmeddelandet.\n")
    (who-to-throw-out . "Vilken session vill du kasta ut? ")
    (throwing-out . "Kastar nu ut session %#1d... ")
    (postpone-prompt . "Hur lite vill du l�sa nu? ")
    (set-session-priority . "S�tt l�sniv�: ")

    ;; From review.el:
    (no-review-done . "Du m�ste �terse innan du kan �terse mer.\n")
    (review-how-many . "�terse hur m�nga?")
    (review-how-many-more . "�terse ytterligare hur m�nga?")
    (latest-n . "senaste %#1d")
    (first-n . "f�rsta %#1d")
    (info-by-whom . "%#1s av vem: ")
    (info-to-conf . "%#1s till m�te: ")
    (all-confs . "alla m�ten")
    ;; +++ not used?
    (info-by-to . "%#1s av %#2P till %#3M fram�t.")
    (no-get-conf . "Du f�r inte h�mta m�tet.\n")
    (no-get-pers . "Du f�r inte h�mta personen.\n")
    (no-review-info . "Ej till�tet �terse %#1s\n")
    ;; +++
    (review-info . "�terse %#1s")
    (review-info-by-to . "�terse %#1s av %#2P till %#3M fram�t.\n")
    (review-more-info-by-to . "�terse %#1s av %#2P till %#3M fram�t.\n")
    (review-rest . "resten")
    (review-more . "n�sta %#1d")
    (you-review . "Du �terser nu %#1s.\n")
    (read-text-first . "Du m�ste l�sa en text f�rst.\n")
    (cannot-read-last-text . "Du kan inte l�sa den senast l�sta texten.\n")
    (review-n-texts . "�terse %#1d inl�gg.\n")
    (review-marked . "�terse %#1d markerade.\n")
    (review-text-no . "�terse text nummer %#1n\n")
    (review-one-comment . "�terse en kommentar till inl�gg %#1n.\n")
    (review-many-comments . "�terse %#2d kommentarer till inl�gg %#1n.\n")
    (read-normally-read . "Hur m�nga vill du se igen? ")

    (review-conf-gone . "M�tet finns inte.\n")
    (review-pers-gone . "Personen finns inte.\n")
    (review-cant-read-conf . "Du kan inte �terse inl�gg till ett slutet m�te du inte �r med i.\n")
    (review-cant-read-letterbox . "Du kan inte �terse inl�gg till n�gon annans brevl�da.\n")
    (review-cant-read-empty . "M�tet �r tomt.\n")
    (cant-review-everything . "Du kan inte �terse alla texter i LysKOM.\n")

    (more-than-one-root . "Inl�gg %#1n har mer �n ett urinl�gg.\n")
    (more-than-one-root-review . 
"Inl�gg %#1n har mer �n ett urinl�gg, men endast ett tr�d kommer att visas.\n")

    ;; From edit-text.el:
    (press-C-c-C-c . "Tryck C-c C-c f�r att skicka in texten.")
    (recipient . "Mottagare:")
    (recipient-prefix . "[Mm]")
    (carbon-copy . "Extra kopia:")
    (blank-carbon-copy . "F�r k�nnedom:")
    (carbon-copy-prefix . "[Ee]")
    (blank-carbon-copy-prefix . "[Ff]")

    (secret-aux-flag . "hemlig")
    (anonymous-aux-flag . "anonym")
    (inherit-aux-flag  . "�rvd")
    (aux-item-prefix . "> ")

    (header-subject . "�rende: ")
    (header-separator . "\\<lyskom-edit-mode-map>\
--- Skriv nedan. \
Skicka in=\\[kom-edit-send], \
Avbryt=\\[kom-edit-quit], \
Annat se \\[describe-mode] ---")
    (text-mass . "%#4s%#1s\n%#2s\n%#3s")
    (comment-to-by . "%#1s till text %#2n%#3s.\n")
    (already-sent . "Du har redan skickat denna text en g�ng. S�nd �nd�? ")
    (subject . "�rende: ")
    (subject-prefix . "[��\\{\\[]")
    (enter-subject-idi . "Skriv ett �rende.")
    (which-text-include . "Vilken text skall vi inkludera? ")
    (added-recipient . "Mottagare som skall adderas: ")
    (added-carbon-copy . "Extra kopia till m�te: ")
    (added-blank-carbon-copy . "F�r k�nnedom till: ")
    (text-to-comment-q . "Vilket inl�gg vill du kommentera? ")
    (conf-has-motd-no . "M�tet har en lapp p� d�rren. (%#1d)\n\n%#2s")
    (still-want-to-add . "Vill du fortfarande addera m�tet? ")
    (could-not-create-text . "\nTexten kunde ej skapas. Felet: %#2s.\n")
    (no-get-text . "Du fick inte h�mta texten.")
    (unknown-header . "Ok�nd information p� raden")
    (transform-error . "Skicka in oformatterat (%#1s)? ")

    ;; From view-text.el:
    (line . " /1 rad/ ")
    (lines ." /%#1d rader/ ")

    (marked-by-you . "Markerad av dig.\n")
    (marked-by-you-and-one . "Markerad av dig och n�gon annan.\n")
    (marked-by-you-and-several . "Markerad av dig och %#1d andra.\n")
    (marked-by-one . "Markerad av 1 person.\n")
    (marked-by-several . "Markerad av %#1d personer.\n")

    ;; The format of this function should coincide with the
    ;; format of the lyskom-text-start variable. DONT change
    ;; one without changing the other.

    (time-yyyy-mm-dd-hh-mm . "%4#1d-%02#2d-%02#3d %02#4d:%02#5d")

    ;; used by lyskom-print-time
    (time-y-m-d-h-m . "%4#1d-%02#2d-%02#3d  %02#4d:%02#5d ")
    (today-time-format-string . "%#6s %02#4d:%02#5d")
    (yesterday-time-format-string . "%#6s %02#4d:%02#5d")
    (today . "idag")
    (yesterday . "ig�r")
    (no-such-text-no . "Det finns inget s�dant inl�gg. (%#1:n)\n")

    (head-Subject . "�rende: ")
    (Recipient . "Mottagare")
    (Extra-recipient . "Extra kopia")
    (Hidden-recipient . "F�r k�nnedom")
    (Strange-recipient . "Underlig mottagare")
    (send-at . "    S�nt:     %#1s\n")
    (sent-by . "    S�nt av %#1P\n")
    (recieved-at . "    Mottaget: %#1s\n")

    (comment-to-text . "Kommentar till text %#1n")
    (footnote-to-text . "Fotnot till text %#1n")
    (comment-in-text . "Kommentar i text %#1n")
    (footnote-in-text . "Fotnot i text %#1n")

    (comment-to-text-by . "Kommentar till text %#1n av %#2P")
    (footnote-to-text-by . "Fotnot till text %#1n av %#2P")
    (comment-in-text-by . "Kommentar i text %#1n av %#2P")
    (footnote-in-text-by . "Fotnot i text %#1n av %#2P")

    (written-by . " av %#1P\n")

    ;; From async.el:

    (name-has-changed-to-name . "%#1:P har nu bytt namn till %#2:P")
    (name-has-changed-to-name-r . "%[%#3@%#1:P%] har nu bytt namn till %[%#3@%#2:P%]\n")
    (you-changed-name-to . "Nu har du bytt namn till %[%#2@%#1:P%].\n")
    (database-sync . "Databasen synkas.")

    (lyskom-is-full . "\
===========================================================
Meddelande fr�n LysKOM-systemet: N�gon f�rs�kte koppla upp,
men misslyckades eftersom alla tillg�ngliga f�rbindelser �r
upptagna. Logga ut och kom tillbaks senare om du v�ntar nu.
===========================================================\n")
    (has-entered . "Nu har %#1:P g�tt in i %#2s.")
    (has-entered-r . "%#2@Nu har %#1P g�tt in i %#3s.\n")
    (has-left . "Nu har %#1:P g�tt ur %#2s.")
    (has-left-r . "%#2@Nu har %#1P g�tt ur %#3s.\n")
    (unknown . "ok�nd")
    (secret-person . "Hemlig person")

    (no-longer-member . "Du �r inte l�ngre medlem i %#1M\n")
    (no-longer-member-n . "Du �r inte l�ngre medlem i m�te %#1m (finns inte.)\n")

    (have-become-member . "Du har blivit medlem i %#1M\n")

    (message-broadcast . 
		       "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Allm�nt meddelande fr�n %#1P (%#3s):

%#2t
----------------------------------------------------------------
")
    (message-from . 
		  "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Personligt meddelande fr�n %#1P (%#3s):

%#2t
----------------------------------------------------------------
")
    (message-from-to .
		     "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Gruppmeddelande till %#3M\nfr�n %#2P (%#4s):

%#1t
----------------------------------------------------------------
")
    (text-is-created . "Text %#1n �r skapad!")

    ;; Used in mode-line-process
    (mode-line-waiting . ": v�ntar")
    (mode-line-working . ": arbetar")
    (mode-line-saving . ": sparar")
    (mode-line-down . ": nerkopplad")

    ;; From completing-read.el:
	   
    (person-or-conf-no-regexp . "\\`[ \t]*[mpMP]\\w*[ \t]+\\([0-9]+\\)\\'")
    (session-no-regexp . "\\`[ \t]*[sS]\\w*[ \t]+\\([0-9]+\\)\\'")

    ;;n From prioritize.el:

    (cant-move-nothing-nowhere . "Kan inte flytta ingenting n�gonstans.")
    (goto-priority-prompt . "Hoppa till prioritet: ")
    (priority-prompt . "Ny prioritet f�r %#1M: ")
    (priority-prompt-marked . "Ny prioritet p� markerade m�ten: ")
    (beginning-of-list . "B�rjan av listan")
    (end-of-list . "Slutet av listan")
    (reprioritize-from . "Prioritera om fr�n: ")
    (reprioritize-to . "Prioritera om till: ")
    (no-selection . "Ingen markerad")
    (selection . "%d markerade")

    (cannot-get-membership . "Kan ej h�mta medlemsskap f�r dig.")
    (cannot-get-pers-stat . "Kan ej h�mta personstatus f�r dig.")
    (prioritize-help .
		     "u,n Flytta m�te, SPC markera, p prioritera markerade, q avsluta, C-h m hj�lp")
    (your-priorities . " Prioritet  M�tesnamn
----------------------------------------------------------------------------
")
    (your-membship . "Ditt medlemsskap i olika KOM-m�ten:
  Prio M�tesnr M�tesnamn\n")
    (prio-row . " %5#1d%5#2m  %#3M\n")
    (too-high-goto-2 . "Du st�r f�r h�gt upp. G� ner till rad 2.")
    (too-low-go-up . "Du kan inte pusha sista raden. G� upp en rad.")
    (all-confs-popped .  "Alla m�ten �r poppade.")
    (prio-died . "Flyttningen misslyckades. Sorry. D�da bufferten.")
    (new-priority . "Ny prioritet? (0 (l�g) - 255 (h�g)) ")
    (new-prio . "%6#1d")

    ;; From flags.el:
    (saving-settings . "Sparar inst�llningarna...")
    (saving-settings-done . "Sparar inst�llningarna...klart")
    (hang-on . "V�nta ett tag...\n")
    (no-changes . "Ingenting beh�vde sparas eftersom inga variabler hade �ndrats.\n")
    (could-not-save-options . "Kunde ej spara inst�llningarna.\n")
    (could-not-create-area . "Kunde ej skapa texten.\n")
    (could-not-set-user-area . "Kunde ej st�lla om user-arean. Servern s�ger felmeddelande: %#1d\n")
    (you-dont-exist . "Du finns inte.\n")
    (error-in-options . "Det fanns ett fel i en av dina variabler (%#1s)
Det stod \"%#2s\" i user-arean. Den s�tts till nil ist�llet.
Skicka en bugrapport.\n")

    ;; From elib-string.el:
    ;; No entries.
		
    ;; From lyskom-rest.el:
		
    (mode-line-unread . " Ol�sta ")
    (mode-line-letters . "brev ")

    (error-code . "Felkod %#2d: %#1s.\n")
    (error-in-kom-do-when-done . "Variabeln kom-do-when-done har ett felaktigt v�.
Du b�r s�tta den till ett b�ttre v�rde.\n")
    (extended-command . "LysKOM: ")
    (wait-for-server . "LysKOM v�ntar p� svar fr�n servern. V�nta tills du f�r en prompt.\n")
    (review-text-q . "�terse text nummer: ")

    (completely-read-conf . "Du har sett alla texter i detta m�te.\n")
    (not-in-any-conf . "Du l�ser inte n�got m�te just nu.\n")

    (all-conf-unread-r . "Du har l�st ut alla m�ten.\n")
    (all-conf-unread-s . "Du har l�st ut alla m�ten. ")
    (one-unread . "%#1M - 1 ol�st\n")
    (several-unread . "%#1M - %#2d ol�sta\n")
    (enter-conf . "%#1M\n")

    (save-on-file-q . "Spara inl�gg p� fil: (%#1s) ")
    (wait-for-prompt . "V�nta p� prompten.")

    (conference-no . "<m�te %#1d>")
    (person-no . "<person %#1d>")
    (prompt-several-messages . "(%d meddelanden)")
    (prompt-single-message   . "(%d meddelande)")

    (go-to-pri-conf-prompt . "G� till n�sta prioriterade m�te")
    (read-pri-text-conf . "L�sa n�sta prioriterade text")
    (review-next-text-prompt . "�terse n�sta text")
    (review-next-comment-prompt . "�terse n�sta kommentar")
    (review-next-marked-prompt . "�terse n�sta markerade")
    (read-next-letter-prompt . "L�sa n�sta brev")
    (read-next-footnote-prompt . "L�sa n�sta fotnot")
    (read-next-comment-prompt . "L�sa n�sta kommentar")
    (read-next-text-prompt . "L�sa n�sta text")
    (go-to-conf-of-marked-prompt . "�terse (n�sta) markerade")
    (go-to-next-conf-prompt . "G� till n�sta m�te")
    (go-to-your-mailbox-prompt . "G� till din brevl�da")
    (the-command . "Kommandot: %#1C")
    (error-in-login-hook . "Det fanns ett fel i din kom-login-hook: %#1s\n")

    (give-a-number . "Mata in ett tal: ")

    (yes-regexp . "\\`[jJ][aA]\\'")
    (no-regexp . "\\`[nN][eE][jJ]\\'")
    (yes-or-no-nag . "Svara bara ja eller nej.")
    (yes-or-no . "(ja eller nej) ")

    (y-or-n-instring . "jJnN")
    (j-or-n-nag . "Svara bara j eller n. ")
    (j-or-n . "(j eller n) ")
    (y-instring . "jJ")

    (person-does-not-exist . "Person %#1d (finns inte).")
    (conference-does-not-exist . "M�te %#1d (finns inte).")
    (person-is-anonymous . "Anonym person")

    (process-signal . "Signal fr�n processen.")
    (closed-connection . "
**************************************************
%#2s
Lyskom-sessionen onormalt st�ngd.
Felmeddelande: %#1s**************************************************")
    (dead-session . "LysKOM-sessionen �r inte aktiv.")
    (error-not-found . "Fel nummer %#1d. Ingen klartextf�rklaring finns.")

    ;; Useful in more place than one:
    (illegal-command . "Otill�tet kommando.\n")
    (no-such-text . "Det finns inget s�dant inl�gg.\n")
    (no-such-text-m . "Det finns inget s�dant inl�gg.")
    (nobody . "ingen")
    (everybody . "alla")
    (everything . "allt")
    (anybody . "vem som helst")
    (forward . "fram�t")
    (backward . "bak�t")
    (wait . "V�nta ett tag...\n")
    (comment . "Kommentar")
    (comment-prefix . "[Kk]")
    (footnote . "Fotnot")
    (footnote-prefix . "[Ff]")

    (by . " av %#1P")
    (text-created .  "Text nummer %#1n �r skapad.\n")

    (resolve-session . "Ange vilken session: ")

    (starting-program . "Startar %#1s...")
    (super-jump . "Filtrerar �rende \"%#1r\" i m�te \"%#2M\"\n")
    (no-recipient . "Inl�gget har ingen mottagare.\n")
    (filtered . "[Filtrerad]")
    (filter-error-specification . "Fel i filterspecifikationen")
    (filter-error-bad-not . "Fel i filterspecifikation efter 'not'")
    (filter-error-unknown-key . "Filternyckeln '%S' �r ok�nd.")
    (filter-error-key-arg . "Fel filterdata (%S %S)")
    (filter-tree . "Hoppar �ver text %#1n \"%#2r\" av %#3P och dess kommentarstr�d.\n")
    (filter-text . "Hoppar �ver text %#1n \"%#2r\" av %#3P.\n")
    (filter-permanent . "Permanent? ")
    (filter-action . "Hur vill du filtrera? ")
    (filter-in-conf . "I vilket m�te? (alla) ")
    (filter-subject . "Filtrera vilket �rende? ")
    (filter-which-text . "Filtrera inl�gg som inneh�ller: ")
    (filter-author . "Filtrera vilken f�rfattare? ")
    (permanent . "(permanent)")
    (temporary . "(tillf�llig)")
    (filter-edit-buffer-name . "*LysKOM Filter Edit*")
    (filter-edit-empty-list . "Listan �r tom")
    (filter-edit-start-of-list . "Listans b�rjan")
    (filter-edit-end-of-list . "Listans slut")
    (filter-edit-filter-how . "Hur vill du filtrera? ")
    (filter-edit-filter-what . "Vad vill du filtrera? ")
    (filter-edit-bad-argument . "Felaktig inmatning: %s")
    (filter-edit-outside-entry . "Kan inte utf�ra kommandot utanf�r ett filter")
    (filter-edit-outside-list . "Kan inte utf�ra operationen utanf�r listan")
    (filter-edit-end-of-pattern . "Filtrets slut")
    (filter-edit-save-p . "Spara f�r�ndringar? ")
    (filter-edit-remove-empty . "Tomma filter g�r att alla texter filtreras. Vill du ta bort dessa? ")
    (filter-edit-restart-p . "Du har gjort �ndringar. Vill du verkligen b�rja om? ")
    (filter-edit-help . 
		      "p,n Upp/ned, i/M-i Ny rad/filter, d/M-d Radera rad/filter, C-h m Mer hj�lp")
    (filter-edit-header . "�ndra filter f�r \"%s\"\n")
    (filter-edit-saving . "Sparar �ndringarna...")
    (filter-edit-saving-done . "Sparar �ndringarna...klart")
    (filter-edit-saving-error . "Kunde inte spara �ndringarna!")
    (filter-edit-insert-pred . "%#1s (=,!=): ")
    (filter-edit-insert-arg . "%#1s %#2s (vad): ")
    (no-filters . "Inga filter har definierats.\n")
    (view-filters-header . "\nAktiva filter:\n\n")
    (view-filters-footer . "")

    (ansaphone-new-message . "Nytt automatsvar: ")
    (ansaphone-message . "Svarsmeddelande:
----------------------------------------------------------------------
%#1t
----------------------------------------------------------------------
")
    (ansaphone-state . "Automatsvar �r nu %#1s.")
    (ansaphone-state-r . "Automatsvar �r nu %#1s.\n")
    (ansaphone-messages-gone . "Sparade meddelanden raderade.")
    (ansaphone-no-messages . "Inga meddelanden.\n")
    (ansaphone-message-list-start . "Sparade meddelanden:\n\n")
    (ansaphone-message-list-end . "\n\n")
    (ansaphone-message-header . "Automatiskt svar (satt %#1s):\n")

    (remote-erase-messages . "Fj�rrstyrning (%#1P %#2s): Sparade meddelanden raderade\n")
    (remote-set-message . "Fj�rrstyrning (%#1P %#2s): Svarsmeddelande:
----------------------------------------------------------------------
%#3t
----------------------------------------------------------------------
")
    (remote-set-ansaphone . "Fj�rrstyrning (%#1P %#2s): Automatsvar �r nu %#3s\n")
    (remote-list-messages . "Fj�rrstyrning (%#1P %#2s): Meddelanden listade\n")
    (remote-quit . "Fj�rrstyrning(%#1P %#2s): Avsluta\n")

    (illegal-remote . 
		    "Otill�ten fj�rrstyrning:
Tid: %#1s
Fr�n: %#2P <%#2p>
Till: %#3P <%#3p>
Text: 
%#4t")
    (illegal-remote-reply . "Fj�rrstyrning inte accepterad: %#1s") 
    (remote-not-in-list . "Otill�ten person")
    (remote-bad-command . "Felaktigt kommando")
    (remote-unknown-error . "Ok�nt fel")

    (remote-control-who . "Kontrollera vilken session? ")
    (remote-control-autoreply . "Automatsvar p� eller av? ")

    (state-on . "p�slaget")
    (state-off . "avslaget")

    (text-popup-title . "Inl�gg %#1s")
    (conf-popup-title . "M�te %#1s")
    (pers-popup-title . "Person %#1s")
    (url-popup-title  . "URL %#1s")
    (aux-popup-title  . "Till�ggsinformation")
    (generic-popup-title . "%#1s")

    (who-i-am-not-present . "%#1P �r inte n�rvarande i n�got m�te\n")
    (who-i-am-present . "%#1P �r n�rvarande i %#2M\n")
    (who-i-am-client . "Programmet heter lyskom.el, version %#1s.\n")
    (who-i-am-server . "Detta �r %#1s, version %#2s.\n")
    (who-i-am-emacs . "Det k�rs under %#1s.\n")

    (no-such-session-r . "Det finns ingen s�dan session. Personen kanske inte �r inloggad.\n")
    (person-not-logged-in-r . "%#1P �r inte inloggad.\n")
    (session-status . "Session %#1d �r %#2P <%#2p>
%#5s %#7s %#4M
K�r %#6D fr�n %#3s\n")
    (session-status-9 . "Session %#1d �r %#2P <%#2p>
%#5s %#7s %#4M
K�r %#6D fr�n %#3s
Uppkopplad sedan %#8s%#9s")
    (session-status-inactive . "\nHar inte varit aktiv p� %#1s\n")
    (one-day . "en dag")
    (one-hour . "en timme")
    (one-minute . "en minut")
    (days . "dagar")
    (hours . "timmar")
    (minutes . "minuter")
    (and . "och")
    (session-is-active . " och �r aktiv.\n")
    (session-is-invisible . "Denna session �r osynlig.\n")
    (status-for-session . "Sessionsstatus f�r vilken person? ")
    (unknown-doing-what . "Existerar")
    (doing-where-conn . "i")
    (doing-nowhere-conn . "men �r")
    (waiting-for-membership . "V�ntar p� att medlemskapslistan ska l�sas in...%d/%d")

    (keyboard-menu-help . "N�sta: SPC  F�reg: DEL  Utf�r: RET  Avbryt: ESC")
    
	   ;; From slow.el
    (no-such-command . "Det finns inget s�dant kommando.\n")
    (command-completions . "Du kan mena n�gon av f�ljande:\n %#1s\n")
    (which-language . "�ndra spr�k till: ")
    (send-formatted . "Skicka in som formatterad text? ")
    (changing-language-to . "Byter till %#1s.\n")
    (language-set-to . "Spr�ket �r %#1s.\n")
    (language-not-loaded . "%#1s finns inte tillg�ngligt.\n")

;;    (reformat-generic . "(%#1s)")
    (reformat-html . "HTML")
    (reformat-enriched . "enriched")
    (reformat-filled . "ombruten")

    (reformat-truncated . "nedkortad")
    
    (need-library . "Paketet \"%#1s\" beh�vs f�r att utf�ra detta kommando.\n")
    (calc-expression . "Uttryck: ")

    (do-send-anonymous . "Skicka in texten anonymt? ")
    (anonymous . "anonym")
    (Anonymous . "Anonym")

    ;; Some Help

    (where-is-doesnt-exist . "Kommandot %#1s finns inte")
    (where-is-on-no-key . "%#1s finns inte p� n�gon tangent")
    (where-is-on-key . "%#1s finns p� %#2s")

    ;; From aux-items.el

    (content-type-aux . "Inneh�llstyp: %#1s")
    (content-type-regexp . "Inneh�llstyp: \\(\\S-+\\)")
    
    (fast-reply-aux . "\"%#1t\" /%#2P/")
    
    (cross-reference-text-aux . "Se �ven inl�gg %#1n /%#2P/")
    (cross-reference-conf-aux . "Se �ven m�te <%#1m> %#1M /%#2P/")
    (cross-reference-pers-aux . "Se �ven person <%#1p> %#1P /%#2P/")
    (cross-reference-text-regexp . "Se �ven inl�gg \\([0-9]+\\)")
    (cross-reference-conf-regexp . "Se �ven m�te <\\([0-9]+\\)>")
    (cross-reference-pers-regexp . "Se �ven person <\\([0-9]+\\)>")
    (strange-cross-reference . "Se �ven %#1s, vad nu det betyder")
    
    (no-comments-aux . "F�rfattaren har beg�rt att inte f� kommentarer")
    (no-comments-edit-aux . "Beg�r att inte f� kommentarer")
    (no-comments-regexp . "Beg�r att inte f� kommentarer")
    
    (personal-comment-aux . "F�rfattaren har beg�rt endast personliga svar")
    (personal-comment-edit-aux . "Beg�r endast personliga svar")
    (personal-comment-regexp . "Beg�r endast personliga svar")
    
    (request-confirmation-aux . "F�rfattaren har beg�rt l�sbekr�ftelse")
    (request-confirmation-edit-aux . "Beg�r bekr�ftelse om l�sning")
    (request-confirmation-regexp . "Beg�r bekr�ftelse om l�sning")
    (confirm-read-q . "Bekr�fta l�sning av inl�gg %#1n? ")
    
    (read-confirm-aux . "L�st: %#1P %#2s")

    (redirect-email-aux . "Skicka sladdpost till %#1s")
    (redirect-lyskom-aux . "Skicka inl�gg till m�te %#1M")

    (label-what-kind . "S�tt personligt namn p� vad (inl�gg, m�te, person)? ")
    (label-what-text . "S�tt personligt namn p� inl�gg nummer: ")
    (label-what-conf . "S�tt personligt namn p� vilket m�te? ")
    (label-what-pers . "S�tt personligt namn p� vilken person? ")
    (label-what-label . "Personligt namn: ")
    (label-secret    . "Skall andra kunna se namnet? ")

    (cant-get-aux-item . "Hittar inte till�ggsinformationen")
    (aux-item-no-info . "Ingen information tillg�nglig\n")
    (aux-item-info . "\
Nummer:       %#1d %#6s
Typ:          %#2d (%#3s)
Skapad av:    %#4P <%#4p>
Skapad:       %#5s
Flaggor:      %#7s
Arvsgr�ns:    %#8s
Inneh�ll:    \"%#9s\"
")
    (secret . "Hemlig")
    (deleted . "Raderad")
    (hide-creator . "Dold skapare")
    (inherit . "�rvs")
    (unlimited-inherit . "Obegr�nsat arv")
    (no-inheritance . "Arv �r avslaget")
    (no-more-inheritance . "Gr�nsen uppn�dd; inget mer arv")
    (inherit-steps . "%#1d steg")

    (aux-item-for . "Till�ggsinformation f�r ")
    (conference-no . "m�te <%#1m> %#1M")
    (text-no . "inl�gg %#1n")

    (what-fast-reply-no . "Snabb replik till vilket inl�gg? ")
    (fast-reply-prompt . "Replik: ")
    (agree-prompt . "Text: ")
    (default-agree-string . "H�ller med")
    (what-agree-no . "H�ll med vilket inl�gg? ")

    ;; Aux-item stuff from edit-text

    (person . "Person")
    (conference . "M�te")
    (text . "Inl�gg")

    (xref-type . "Vad vill du referera till (inl�gg, m�te eller person)? ")
    (which-text-to-xref . "L�gg till referens till text nummer: ")
    (which-text-to-xref-err . "Hittar inte texten. L�gg till referens till text nummer: ")
    (which-pers-to-xref . "L�gg till referens till person: ")
    (which-conf-to-xref . "L�gg till referens till m�te: ")

    (no-comments-q . "F�rfattaren vill inte ha kommentarer. Kommentera �nd�? ")
    (private-answer-q . "F�rfattaren har bett om personliga svar. Skriv personligt svar? ")

    (your-invited . "Du har bjudits in till %#1M av %#2P.\n")
    (accept-invitation . "Vill du acceptera inbjudan nu? ")
    (enter-passive . "Du �r passiv medlem i %#1M.\n")

    (invitation-mt-type . "inbjuden")
    (passive-mt-type . "passiv")
    (secret-mt-type . "hemlig")
))




;;; ================================================================
;;;              The commands and their associated functions



;;; The Alist formely known as lyskom-commands
(lyskom-language-strings lyskom-command sv
  '(
    (describe-mode            . "Hj�lp")
    (kom-slow-mode            . "L�ngsamma kommandon")
    (kom-quick-mode           . "Snabba kommandon")
    (kom-send-message         . "S�nda meddelande")
    (kom-send-alarm           . "S�nda alarmmeddelande")
    (kom-create-conf	      . "Skapa m�te")
    (kom-delete-conf          . "Utpl�na")
    (kom-delete-text          . "Radera inl�gg")
    (kom-display-time	      . "Se tiden")
    (kom-go-to-conf	      . "G� till m�te")
    (kom-go-to-next-conf      . "G� till n�sta m�te")
    (kom-jump		      . "Hoppa �ver alla kommentarer")
    (kom-list-conferences     . "Lista m�ten") 
    (kom-list-persons	      . "Lista personer")
    (kom-list-news            . "Lista nyheter")
    (kom-list-re              . "Lista (med) regexpar")
    (kom-membership	      . "Lista medlemsskap")
    ;; (kom-list-marks		"Lista markeringar") 
    (kom-postpone	      . "Uppskjuta l�sning")
    (kom-set-session-priority . "S�tt l�sniv�")
    (kom-prioritize	      . "Prioritera m�ten")
    (kom-status-person	      . "Status (f�r) person")
    (kom-status-conf	      . "Status (f�r) m�te")
    (kom-add-self	      . "Bli medlem i m�te")
    (kom-list-summary	      . "Lista �renden")
    (kom-sub-self             . "Uttr�da ur m�te")
    (kom-quit		      . "Sluta")
    (kom-recover	      . "�terstarta kom") 
    (kom-start-anew	      . "B�rja med nytt namn")
    (kom-view		      . "�terse inl�gg")
    (kom-find-root-review     . "�terse tr�d")
    (kom-review-comments      . "�terse alla kommentarer")
    (kom-review-tree	      . "�terse alla kommentarer rekursivt")
    (kom-review-clear         . "�terse hoppa")
    (kom-review-last-normally-read
     			      . "�terse igen")
    (kom-review-noconversion  . "�terse omodifierat")
    (kom-review-next          . "�terse n�sta")
    (kom-find-root	      . "�terse urinl�gget")
    (kom-review-by-to         . "�terse senaste")
    (kom-review-more          . "�terse fler inl�gg")
    (kom-review-first         . "�terse f�rsta")
    (kom-review-all           . "�terse alla")
    (kom-view-commented-text  . "�terse det kommenterade")
    (kom-view-previous-commented-text
     			      . "�terse det f�reg�ende kommenterade")
    (kom-review-stack         . "�terse lista")
    (kom-review-presentation  . "�terse presentation")
    (kom-review-backward      . "(�terse) Bakl�nges")
    (kom-view-next-text	      . "L�sa n�sta inl�gg")
    (kom-who-is-on	      . "Vilka �r inloggade") 
    (kom-who-is-on-in-conference
     			      . "Vilka �r inloggade i m�te") 
    (kom-who-am-i             . "Var (�r) jag")
;    (kom-display-who-buffer	"Visa vilkalistan")
    (kom-list-clients	      . "Lista klienter")
    (kom-busy-wait	      . "V�nta p� ett inl�gg")
    (kom-write-comment	      . "Kommentera inl�gget")
    (kom-comment-previous     . "Kommentera f�reg�ende inl�gg")
    (kom-write-footnote       . "Fotnot till inl�gg")
    (kom-private-answer       . "Personligt svar")
    (kom-private-answer-previous
     . "Personligt svar p� f�reg�ende inl�gg")
    (kom-set-unread	      . "Endast l�sa senaste")
    (kom-write-text	      . "Skriva ett inl�gg")
    (kom-send-letter	      . "Skicka brev")
    (kom-change-name	      . "�ndra namn")
    (kom-change-parenthesis   . "�ndra parentes")
    (kom-change-password      . "�ndra l�senord")
    (kom-change-supervisor    . "�ndra organisat�r")
    (kom-change-presentation  . "�ndra presentation")
    (kom-get-appreciation     . "F� uppmuntran")
    (kom-get-abuse            . "F� sk�ll")
    (kom-mark-text            . "Markera (inl�gg)")
    (kom-unmark-text          . "Avmarkera (inl�gg)")
    (kom-review-marked-texts  . "�terse markerade")
    (kom-review-all-marked-texts . "�terse alla markerade")
    (kom-add-recipient        . "Addera mottagare")
    (kom-add-copy	      . "Addera extra kopiemottagare")
    (kom-add-bcc	      . "Addera f�r k�nnedom")
    (kom-sub-recipient        . "Subtrahera mottagare")
    (kom-move-text            . "Flytta inl�gg")
    (kom-add-comment	      . "Addera kommentar")
    (kom-sub-comment	      . "Subtrahera kommentar")
    (kom-add-member	      . "Addera medlem")
    (kom-sub-member           . "Uteslut medlem")
    (kom-change-conf-motd     . "S�tt lapp p� d�rren")
    (kom-set-garb-nice        . "�ndra livsl�ngd")
    (kom-set-super-conf       . "�ndra superm�te")
    (kom-set-permitted-submitters . "�ndra till�tna f�rfattare")
    (kom-unset-conf-motd      . "Ta bort lapp p� d�rren")
    (kom-save-text	      . "Spara text (p� fil)")
    (kom-edit-options	      . "�ndra variabler")
    (kom-save-options         . "Spara variabler")
    (kom-shutdown-server      . "St�ng av servern")
    (kom-sync-database        . "Spara databasen")
    (kom-enable-adm-caps      . "�verg� till administrat�rsmod")
    (kom-disable-adm-caps     . "�verg� till normalmod")
    (kom-set-motd             . "S�tt loginmeddelande")
    (kom-remove-motd          . "Ta bort loginmeddelande")
    (kom-force-logout         . "Kasta ut en session")
    (kom-filter-author        . "Filtrera f�rfattare")
    (kom-filter-subject       . "Filtrera �rende")
    (kom-filter-text          . "Filtrera inneh�ll")
    (kom-super-jump           . "Superhoppa")
    (kom-filter-edit          . "�ndra filter")
    (kom-list-filters         . "Lista filter")
    (kom-show-user-area       . "Visa user-arean")
    (kom-change-conf-type     . "�ndra m�testyp")

    (kom-change-auto-reply    . "�ndra svarsmeddelande")
    (kom-toggle-auto-reply    . "Automatsvar")
    (kom-list-messages        . "Lista meddelanden")
    (kom-erase-messages       . "Radera meddelanden")

    (kom-remote-autoreply     . "Fj�rrkontrollera automatsvar")
    (kom-remote-set-message   . "Fj�rrkontrollera �ndra svarsmeddelande")
    (kom-remote-list-messages . "Fj�rrkontrollera lista meddelanden")
    (kom-remote-erase-messages . "Fj�rrkontrollera radera meddelanden")
    (kom-remote-quit          . "Fj�rrkontrollera avsluta")

    (kom-status-session       . "Status (f�r) session")
    (kom-customize            . "Inst�llningar (f�r) LysKOM")
    (kom-next-kom             . "N�sta LysKOM")
    (kom-previous-kom         . "F�reg�ende LysKOM")
    (kom-next-unread-kom      . "N�sta ol�sta LysKOM")
    (kom-change-language      . "�ndra spr�k")
    (kom-calculate            . "Ber�kna")
    (kom-list-marks           . "Lista markeringar")

    (kom-where-is             . "Var finns kommandot")

    (kom-fast-reply           . "Snabb replik")
    (kom-agree                . "H�lla med")
    
    ))

(lyskom-language-var lyskom-language-codes sv
  '((aa . "Afar")
    (ab . "Abkhasianska")
    (af . "Afrikaans")
    (am . "Amhariska")
    (ar . "Arabiska")
    (as . "Assamesiska")
    (ay . "Aymara")
    (az . "Azerbajanska")
    (ba . "Bashkiriska")
    (be . "Vitryska")
    (bg . "Bulgariska")
    (bh . "Bihariska")
    (bi . "Bislamska")
    (bn . "Bengaliska")
    (bo . "Tibetanska")
    (br . "Bretangneska")
    (ca . "Katalanska")
    (co . "Korsikanska")
    (cs . "Tjeckiska")
    (cy . "Walesiska")
    (da . "Danska")
    (de . "Tyska")
    (dz . "Bhutanska")
    (el . "Grekiska")
    (en . "Engelska")
    (eo . "Esperanto")
    (es . "Spanska")
    (et . "Estniska")
    (eu . "Baskiska")
    (fa . "Persiska")
    (fi . "Finska")
    (fj . "Fiji")
    (fo . "Far�iska")
    (fr . "Franska")
    (fy . "Frisiska")
    (ga . "Irl�ndska")
    (gd . "Skotsk G�liska")
    (gl . "Galiciska")
    (gn . "Guarani")
    (gu . "Gujaratiska")
    (ha . "Hausa")
    (he . "Hebreiska")
    (hi . "Hindi")
    (hr . "Kroatiska")
    (hu . "Ungerska")
    (hy . "Armeniska")
    (ia . "Interlingua")
    (id . "Indonesiska")
    (ie . "Interlingue")
    (ik . "Inupiak")
    (is . "Isl�ndska")
    (it . "Italienska")
    (iu . "Inuktitut")
    (ja . "Japanska")
    (jw . "Javanesiska")
    (ka . "Georgiska")
    (kk . "Kazakhstanska")
    (kl . "Gr�nl�ndska")
    (km . "Kambodianska")
    (kn . "Kannada")
    (ko . "Koreanska")
    (ks . "Kashmiriska")
    (ku . "Kurdiska")
    (ky . "Kirghiz")
    (la . "Latinska")
    (ln . "Lingala")
    (lo . "Laotesiska")
    (lt . "Litauiska")
    (lv . "Lettiska")
    (mg . "Malagasiska")
    (mi . "Maori")
    (mk . "Makedonska")
    (ml . "Malayalam")
    (mn . "Mongolska")
    (mo . "Moldaviska")
    (mr . "Marathi")
    (ms . "Malaysiska")
    (mt . "Maltesiska")
    (my . "Burmesiska")
    (na . "Nauruiska")
    (ne . "Nepalska")
    (nl . "Holl�ndska")
    (no . "Norska")
    (oc . "Occitanska")
    (om . "Oromo")
    (or . "Oriya")
    (pa . "Pundjabiska")
    (pl . "Polska")
    (ps . "Pashtu")
    (pt . "Protugisiska")
    (qu . "Quechua")
    (rm . "Rh�toromanska")
    (rn . "Kirundiska")
    (ro . "Rum�nska")
    (ru . "Ryska")
    (rw . "Kiyarwanda")
    (sa . "Sanskrit")
    (sd . "Sindhi")
    (sg . "Sangho")
    (sh . "Serbokroatiska")
    (si . "Singhalesiska")
    (sk . "Slovakiska")
    (sl . "Slovenska")
    (sm . "Samoanska")
    (sn . "Shoniska")
    (so . "Somaliska")
    (sq . "Albanska")
    (sr . "Serbiska")
    (ss . "Siswatiska")
    (st . "Sesothiska")
    (su . "Sudanesiska")
    (sv . "Svenska")
    (sw . "Swahili")
    (ta . "Tamilska")
    (te . "Telugu")
    (tg . "Tajikiska")
    (th . "Thail�ndska")
    (ti . "Tigrinya")
    (tk . "Turkmenistanska")
    (tl . "Tagalog")
    (tn . "Sichuanska")
    (to . "Tongiska")
    (tr . "Turkiska")
    (ts . "Tsongiska")
    (tt . "Tatariska")
    (tw . "Twi")
    (ug . "Uiguriska")
    (uk . "Ukrainska")
    (ur . "Urdu")
    (uz . "Uzbekistanska")
    (vi . "Vietnamesiska")
    (vo . "Volap�k")
    (wo . "Wolof")
    (xh . "Xhosa")
    (yi . "Yiddish")
    (yo . "Yorouba")
    (za . "Zhuang")
    (zh . "Kinesiska")
    (zu . "Zulu")
    (-- . "Ok�nt spr�k (%#1s)")))


(lyskom-language-strings lyskom-menu sv
  '((lyskom    . "LysKOM")
    (read      . "L�s")
    (dont-read . "Hoppa")
    (write     . "Skriv")
    (conference . "M�te")
    (person    . "Person")
    (other     . "Annat")
    (marks     . "Markeringar")
    (move      . "G�")
    (info      . "Om")
    (send      . "S�nd")
    (recievers . "Mottagare")
    (commented . "Kommenterar")
    (kom-edit-send . "Skicka in")
    (kom-edit-send-anonymous . "Skicka anonymt")
    (kom-edit-quit . "Kasta bort") 
    (kom-ispell-message . "Stavningskontroll")
    (kom-edit-add-recipient . "Addera mottagare")
    (kom-edit-add-copy . "Addera extra kopiemottagare")
    (kom-edit-show-commented . "�terse det kommenterade")
    (kom-edit-insert-commented . "Citera det kommenterade")
    (kom-edit-add-bcc . "Addera f�r k�nnedom")
    (kom-edit-add-cross-reference . "Addera korsreferens")
    (kom-edit-add-no-comments . "Beg�r inga kommentarer")
    (kom-edit-add-personal-comments . "Beg�r personliga svar")
    (kom-edit-add-read-confirm-request . "Beg�r l�sbekr�ftelse")
    (kom-edit-move-text . "Flytta till en ny mottagare")
))

;;(defvar lyskom-swascii-commands nil
;;  "The swascii-versions of lyskom-commands.")

(lyskom-language-var lyskom-onoff-table sv
  '(("p�" . on) ("av" . off)))

(lyskom-language-var lyskom-filter-predicate-list sv
      '(("=" . nil) ("!=" . t)))

(lyskom-language-var lyskom-filter-what sv
      '((author . "F�rfattare")
        (author-no . "F�rfattare (nummer)")
        (author-re . "F�rfattare (regexp)")
        (subject . "�rende")
        (subject-re . "�rende (regexp)")
        (recipient . "Mottagare")
        (recipient-no . "Mottagare (nummer)")
        (recipient-re . "Mottagare (regexp)")
        (text . "Inneh�ll")
        (text . "Inneh�ll (regexp)")))

(lyskom-language-var lyskom-filter-actions sv
      '((skip-text . "Hoppa �ver")
        (dontshow . "Visa inte")
        (skip-tree . "Hoppa �ver kommentarer")))
                               
;;(defvar lyskom-swascii-filter-actions nil
;;  "The swascii-versions of lyskom-filter-actions.")
;;(defvar lyskom-swascii-filter-what nil
;;  "The swascii version of lyskom-filter-what")

(lyskom-language-var lyskom-text-start sv
  "[0-9]+ +\\(199[0-9]-[0-1][0-9]-[0-3][0-9]\\|idag\\|ig�r\\) +[0-2][0-9]:[0-5][0-9] +/[0-9]+ rad\\(er\\)?/ ")


(defconst lyskom-keybindings-missing '(lyskom-previous-prefix))

(defvar lyskom-sv-mode-map nil)
(lyskom-language-keymap lyskom-mode-map sv lyskom-sv-mode-map)

(if lyskom-sv-mode-map
    nil
  (setq lyskom-sv-mode-map (make-keymap))
  (suppress-keymap lyskom-sv-mode-map)
  (define-prefix-command 'lyskom-sv-review-prefix)
  (define-prefix-command 'lyskom-sv-change-prefix)
  (define-prefix-command 'lyskom-sv-next-prefix)
  (define-prefix-command 'lyskom-sv-list-prefix)
  (define-prefix-command 'lyskom-sv-S-prefix)
  (define-prefix-command 'lyskom-sv-fast-reply-prefix)
  (define-prefix-command 'lyskom-sv-filter-get-prefix)

  (define-key lyskom-sv-mode-map (kbd "f") 'lyskom-sv-filter-get-prefix)
  (define-key lyskom-sv-mode-map (kbd "n") 'lyskom-sv-next-prefix)
  (define-key lyskom-sv-mode-map (kbd "l") 'lyskom-sv-list-prefix)
  (define-key lyskom-sv-mode-map (kbd "s") 'lyskom-sv-S-prefix)
  (define-key lyskom-sv-mode-map (kbd "r") 'lyskom-sv-fast-reply-prefix)
  (define-key lyskom-sv-mode-map (kbd "�") 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map (kbd "�") 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map (kbd "�") 'lyskom-sv-review-prefix)
  (define-key lyskom-sv-mode-map (kbd "�") 'lyskom-sv-review-prefix)

  (define-key lyskom-sv-mode-map (kbd (lyskom-keys 'button2up)) 'kom-button-click)
  (define-key lyskom-sv-mode-map (kbd (lyskom-keys 'button2)) 'kom-mouse-null)
  (define-key lyskom-sv-mode-map (kbd (lyskom-keys 'button3))   'kom-popup-menu)
  (define-key lyskom-sv-mode-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lyskom-sv-mode-map (kbd "*")     'kom-button-press)
  (define-key lyskom-sv-mode-map (kbd "+")     'kom-menu-button-press)
  (define-key lyskom-sv-mode-map (kbd "TAB")   'kom-next-link)
  (define-key lyskom-sv-mode-map (kbd "M-TAB") 'kom-previous-link)
  (define-key lyskom-sv-mode-map (kbd "C-i")   'kom-next-link)
  (define-key lyskom-sv-mode-map (kbd "M-C-i") 'kom-previous-link)

  (define-key lyskom-sv-mode-map (kbd "{") 'lyskom-sv-change-prefix) ; krullar
  (define-key lyskom-sv-mode-map (kbd "[") 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map (kbd "}") 'lyskom-sv-review-prefix)
  (define-key lyskom-sv-mode-map (kbd "]") 'lyskom-sv-review-prefix)


  ;;(define-key lyskom-sv-mode-map "vi" 'vilka)
  
  ;; These should be first in order to be last in the menu of alternatives.
  (define-key lyskom-sv-mode-map (kbd "{ ?") 'lyskom-help)
  (define-key lyskom-sv-mode-map (kbd "} ?") 'lyskom-help)
  (define-key lyskom-sv-mode-map (kbd "f ?") 'lyskom-help)
  (define-key lyskom-sv-mode-map (kbd "n ?") 'lyskom-help)
  (define-key lyskom-sv-mode-map (kbd "l ?") 'lyskom-help)
  (define-key lyskom-sv-mode-map (kbd "s ?") 'lyskom-help)
  

  (define-key lyskom-sv-mode-map (kbd "e")   'kom-set-unread)
  (define-key lyskom-sv-mode-map (kbd "a")   'kom-extended-command)
  (define-key lyskom-sv-mode-map (kbd "<SPC>") 'kom-next-command)
  (define-key lyskom-sv-mode-map (kbd "<RET>") 'kom-line-next-command)
  (define-key lyskom-sv-mode-map (kbd "<LFD>") 'kom-page-next-command)
  (define-key lyskom-sv-mode-map (kbd "C-j") 'kom-page-next-command)
  (define-key lyskom-sv-mode-map (kbd "j")   'kom-page-next-command)
  (define-key lyskom-sv-mode-map (kbd "C-m") 'kom-line-next-command)

  (define-key lyskom-sv-mode-map (kbd "?")   'describe-mode)
  (define-key lyskom-sv-mode-map (kbd "b")   'kom-send-letter)
  (define-key lyskom-sv-mode-map (kbd "g")   'kom-go-to-conf)
  (define-key lyskom-sv-mode-map (kbd "i")   'kom-write-text)
  (define-key lyskom-sv-mode-map (kbd "k")   'kom-write-comment)
  (define-key lyskom-sv-mode-map (kbd "K")   'kom-comment-previous)
  (define-key lyskom-sv-mode-map (kbd "F")   'kom-write-footnote)
  (define-key lyskom-sv-mode-map (kbd "p")   'kom-private-answer)
  (define-key lyskom-sv-mode-map (kbd "P")   'kom-private-answer-previous)
  (define-key lyskom-sv-mode-map (kbd "h")   'kom-jump)
  (define-key lyskom-sv-mode-map (kbd "H")   'kom-super-jump)
  (define-key lyskom-sv-mode-map (kbd "l M") 'kom-list-marks)
  (define-key lyskom-sv-mode-map (kbd "l m") 'kom-list-conferences)
  (define-key lyskom-sv-mode-map (kbd "l n") 'kom-list-news)
  (define-key lyskom-sv-mode-map (kbd "l p") 'kom-list-persons)
  (define-key lyskom-sv-mode-map (kbd "l r") 'kom-list-re)
  (define-key lyskom-sv-mode-map (kbd "l s") 'kom-membership)
  (define-key lyskom-sv-mode-map (kbd "l �") 'kom-list-summary)
  (define-key lyskom-sv-mode-map (kbd "l �") 'kom-list-summary)
  (define-key lyskom-sv-mode-map (kbd "l {") 'kom-list-summary)
  (define-key lyskom-sv-mode-map (kbd "l [") 'kom-list-summary)
  (define-key lyskom-sv-mode-map (kbd "l f") 'kom-list-filters)
  (define-key lyskom-sv-mode-map (kbd "l v") 'kom-who-is-on-in-conference)
  (define-key lyskom-sv-mode-map (kbd "m")   'kom-add-self)
  (define-key lyskom-sv-mode-map (kbd "M")   'kom-mark-text)
  (define-key lyskom-sv-mode-map (kbd "A")   'kom-unmark-text)
  (define-key lyskom-sv-mode-map (kbd "n i") 'kom-view-next-new-text)
  (define-key lyskom-sv-mode-map (kbd "n m") 'kom-go-to-next-conf)
  (define-key lyskom-sv-mode-map (kbd "n l") 'kom-next-kom)
  (define-key lyskom-sv-mode-map (kbd "n o") 'kom-next-unread-kom)
  (define-key lyskom-sv-mode-map (kbd "f l") 'kom-previous-kom)
  (define-key lyskom-sv-mode-map (kbd "S")   'kom-quit)
  (define-key lyskom-sv-mode-map (kbd "q")   'kom-quit)
  (define-key lyskom-sv-mode-map (kbd "z")   'kom-bury)
  (define-key lyskom-sv-mode-map (kbd "R")   'kom-recover)
  (define-key lyskom-sv-mode-map (kbd "t")   'kom-display-time)
  (define-key lyskom-sv-mode-map (kbd "f u") 'kom-get-appreciation)
  (define-key lyskom-sv-mode-map (kbd "f s") 'kom-get-abuse)
  (define-key lyskom-sv-mode-map (kbd "f t") 'kom-move-text)
  (define-key lyskom-sv-mode-map (kbd "f �") 'kom-filter-subject)
  (define-key lyskom-sv-mode-map (kbd "f �") 'kom-filter-subject)
  (define-key lyskom-sv-mode-map (kbd "f {") 'kom-filter-subject)
  (define-key lyskom-sv-mode-map (kbd "f [") 'kom-filter-subject)
  (define-key lyskom-sv-mode-map (kbd "f f") 'kom-filter-author)
  (define-key lyskom-sv-mode-map (kbd "f i") 'kom-filter-text)
  (define-key lyskom-sv-mode-map (kbd "v")   'kom-who-is-on)
  (define-key lyskom-sv-mode-map (kbd "J")   'kom-who-am-i)
  (define-key lyskom-sv-mode-map (kbd "V")   'kom-busy-wait)
  (define-key lyskom-sv-mode-map (kbd "{ p") 'kom-change-presentation)
  (define-key lyskom-sv-mode-map (kbd "{ f") 'kom-filter-edit)
  (define-key lyskom-sv-mode-map (kbd "{ m") 'kom-change-auto-reply)
  (define-key lyskom-sv-mode-map (kbd "} SPC") 'kom-view)
  (define-key lyskom-sv-mode-map (kbd "} 0") 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map (kbd "} 1") 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map (kbd "} 2") 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map (kbd "} 3") 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map (kbd "} 4") 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map (kbd "} 5") 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map (kbd "} 6") 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map (kbd "} 7") 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map (kbd "} 8") 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map (kbd "} 9") 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map (kbd "} k") 'kom-view-commented-text)
  (define-key lyskom-sv-mode-map (kbd "} K") 'kom-view-previous-commented-text)
  (define-key lyskom-sv-mode-map (kbd "} a ?") 'lyskom-help)
  (define-key lyskom-sv-mode-map (kbd "} a k") 'kom-review-comments)
  (define-key lyskom-sv-mode-map (kbd "} a r") 'kom-review-tree)
  (define-key lyskom-sv-mode-map (kbd "} h") 'kom-review-clear)
  (define-key lyskom-sv-mode-map (kbd "} i") 'kom-review-last-normally-read)
  (define-key lyskom-sv-mode-map (kbd "} n") 'kom-review-next)
  (define-key lyskom-sv-mode-map (kbd "} o") 'kom-review-noconversion)
  (define-key lyskom-sv-mode-map (kbd "} r") 'kom-find-root)
  (define-key lyskom-sv-mode-map (kbd "} u") 'kom-find-root)
  (define-key lyskom-sv-mode-map (kbd "} s") 'kom-review-by-to)
  (define-key lyskom-sv-mode-map (kbd "} y") 'kom-review-more)
  (define-key lyskom-sv-mode-map (kbd "} A") 'kom-review-all)
  (define-key lyskom-sv-mode-map (kbd "} f") 'kom-review-first)
  (define-key lyskom-sv-mode-map (kbd "} l") 'kom-review-stack)
  (define-key lyskom-sv-mode-map (kbd "} p") 'kom-review-presentation)
  (define-key lyskom-sv-mode-map (kbd "} t") 'kom-find-root-review)
  (define-key lyskom-sv-mode-map (kbd "} m") 'kom-review-marked-texts)
  (define-key lyskom-sv-mode-map (kbd "} a m") 'kom-review-all-marked-texts)
  (define-key lyskom-sv-mode-map (kbd "} a SPC") 'kom-review-all)
  (define-key lyskom-sv-mode-map (kbd "B")   'kom-review-backward)
  (define-key lyskom-sv-mode-map (kbd "s m") 'kom-status-conf)
  (define-key lyskom-sv-mode-map (kbd "s p") 'kom-status-person)
  (define-key lyskom-sv-mode-map (kbd "s s") 'kom-status-session)
  (define-key lyskom-sv-mode-map (kbd "s �") 'kom-send-message)
  (define-key lyskom-sv-mode-map (kbd "s {") 'kom-send-message)
  (define-key lyskom-sv-mode-map (kbd "s [") 'kom-send-message)

  (define-key lyskom-sv-mode-map (kbd "r r") 'kom-fast-reply)
  (define-key lyskom-sv-mode-map (kbd "r h") 'kom-agree)
  
  ;; Running in buffer

  (define-key lyskom-sv-mode-map (kbd "M-p") 'backward-text)
  (define-key lyskom-sv-mode-map (kbd "M-n") 'forward-text)
  (define-key lyskom-sv-mode-map (kbd "s t") 'kom-save-text)

  (define-key lyskom-sv-mode-map (kbd "C-?") 'scroll-down)
  (define-key lyskom-sv-mode-map (kbd "<DEL>") 'scroll-down)
  (define-key lyskom-sv-mode-map (kbd "<BS>") 'scroll-down)
)





;;;==============================================================
;;; Keymap for filter editing
;;;

(defvar lyskom-sv-filter-edit-map nil)
(lyskom-language-keymap lyskom-filter-edit-map sv lyskom-sv-filter-edit-map)

(if lyskom-sv-filter-edit-map ()
  (setq lyskom-sv-filter-edit-map (make-keymap))
  (suppress-keymap lyskom-sv-filter-edit-map)
  (define-key lyskom-sv-filter-edit-map (kbd "p")   'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-sv-filter-edit-map (kbd "P")   'lyskom-filter-edit-prev-entry)
  (define-key lyskom-sv-filter-edit-map (kbd "n")   'lyskom-filter-edit-next-pattern)
  (define-key lyskom-sv-filter-edit-map (kbd "N")   'lyskom-filter-edit-next-entry)
  (define-key lyskom-sv-filter-edit-map (kbd "C-p") 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-sv-filter-edit-map (kbd "C-n") 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-sv-filter-edit-map (kbd "C-b") 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-sv-filter-edit-map (kbd "C-f") 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-sv-filter-edit-map (kbd "M-p") 'lyskom-filter-edit-prev-entry)
  (define-key lyskom-sv-filter-edit-map (kbd "M-n") 'lyskom-filter-edit-next-entry)
  (define-key lyskom-sv-filter-edit-map (kbd "d")   'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-sv-filter-edit-map (kbd "M-d") 'lyskom-filter-edit-delete-entry)
  (define-key lyskom-sv-filter-edit-map (kbd "D")   'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-sv-filter-edit-map (kbd "C-d") 'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-sv-filter-edit-map (kbd "i")   'lyskom-filter-edit-insert-pattern)
  (define-key lyskom-sv-filter-edit-map (kbd "I")   'lyskom-filter-edit-insert-pattern)
  (define-key lyskom-sv-filter-edit-map (kbd "M-i") 'lyskom-filter-edit-insert-entry)
  (define-key lyskom-sv-filter-edit-map (kbd "<")   'lyskom-filter-edit-beginning-of-list)
  (define-key lyskom-sv-filter-edit-map (kbd ">")   'lyskom-filter-edit-end-of-list)
  (define-key lyskom-sv-filter-edit-map (kbd "M-<") 'lyskom-filter-edit-beginning-of-list)
  (define-key lyskom-sv-filter-edit-map (kbd "M->") 'lyskom-filter-edit-end-of-list)
  (define-key lyskom-sv-filter-edit-map (kbd "q")   'lyskom-filter-edit-quit)
  (define-key lyskom-sv-filter-edit-map (kbd "x")   'lyskom-filter-edit-expunge)
  (define-key lyskom-sv-filter-edit-map (kbd "s")   'lyskom-filter-edit-save)
  (define-key lyskom-sv-filter-edit-map (kbd "g")   'lyskom-filter-edit-revert)
  (define-key lyskom-sv-filter-edit-map (kbd "t")   'lyskom-filter-edit-toggle-permanent)
  (define-key lyskom-sv-filter-edit-map (kbd "a")   'lyskom-filter-edit-toggle-action)
  (define-key lyskom-sv-filter-edit-map (kbd "?")   'lyskom-filter-edit-brief-help)
  (define-key lyskom-sv-filter-edit-map (kbd "h")   'lyskom-filter-edit-brief-help)
  )



(defvar lyskom-sv-prioritize-mode-map nil)
(lyskom-language-keymap lyskom-prioritize-mode-map sv 
  lyskom-sv-prioritize-mode-map)

(if lyskom-sv-prioritize-mode-map 
    nil
  (setq lyskom-sv-prioritize-mode-map (make-keymap))
  (suppress-keymap lyskom-sv-prioritize-mode-map)
  (define-key lyskom-sv-prioritize-mode-map (kbd (lyskom-keys 'button2up)) 'kom-button-click)
  (define-key lyskom-sv-prioritize-mode-map (kbd (lyskom-keys 'button2)) 'kom-mouse-null)
  (define-key lyskom-sv-prioritize-mode-map (kbd (lyskom-keys 'button3)) 'kom-popup-menu)
  (define-key lyskom-sv-prioritize-mode-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lyskom-sv-prioritize-mode-map (kbd "*") 'kom-button-press)
  (define-key lyskom-sv-prioritize-mode-map (kbd "+") 'kom-menu-button-press)
  (define-key lyskom-sv-prioritize-mode-map (kbd "?") 'kom-prioritize-help)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-k") 'kom-prioritize-select)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-y") 'kom-prioritize-yank)
  (define-key lyskom-sv-prioritize-mode-map (kbd "SPC") 'kom-prioritize-select)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-m") 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "RET") 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-j") 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "<down>") 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-n") 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-?") 'kom-prioritize-previous-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-C-?") 'kom-prioritize-deselect-all)
  (define-key lyskom-sv-prioritize-mode-map (kbd "<up>") 'kom-prioritize-previous-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-p") 'kom-prioritize-previous-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "p") 'kom-prioritize-previous-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-<up>") 'kom-prioritize-move-up)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-p") 'kom-prioritize-move-up)
  (define-key lyskom-sv-prioritize-mode-map (kbd "u") 'kom-prioritize-move-up)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-<down>") 'kom-prioritize-move-down)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-n") 'kom-prioritize-move-down)
  (define-key lyskom-sv-prioritize-mode-map (kbd "d") 'kom-prioritize-move-down)
  (define-key lyskom-sv-prioritize-mode-map (kbd "n") 'kom-prioritize-move-down)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-<") 'kom-prioritize-beginning)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M->") 'kom-prioritize-end)
  (define-key lyskom-sv-prioritize-mode-map (kbd "r") 'kom-prioritize-reprioritize)
  (define-key lyskom-sv-prioritize-mode-map (kbd "g") 'kom-prioritize-goto-priority)
  (define-key lyskom-sv-prioritize-mode-map (kbd "p") 'kom-prioritize-set-priority)
  (define-key lyskom-sv-prioritize-mode-map (kbd "s") 'kom-prioritize-save)
  (define-key lyskom-sv-prioritize-mode-map (kbd "q") 'kom-prioritize-quit)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-c C-c") 'kom-prioritize-quit)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-i") 'kom-next-link)
  (define-key lyskom-sv-prioritize-mode-map (kbd "TAB") 'kom-next-link)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-TAB") 'kom-previous-link)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-C-i") 'kom-previous-link)
)


(lyskom-language-var lyskom-prioritize-header-lines sv 2)

(lyskom-language-var lyskom-prioritize-header sv
" Prio   M�te
-----------------------------------------------------------------------------
")



;;;; ============================================================
;;;; Strings and things for the customize mode
;;;;

(defvar lyskom-sv-customize-map nil)
(lyskom-language-keymap lyskom-customize-map sv lyskom-sv-customize-map)


(if lyskom-sv-customize-map
    nil
  (setq lyskom-sv-customize-map (make-sparse-keymap))
  (define-key lyskom-sv-customize-map (kbd "TAB") 'widget-forward)
  (define-key lyskom-sv-customize-map (kbd "M-TAB") 'widget-backward)
  (define-key lyskom-sv-customize-map (kbd "C-i") 'widget-forward)
  (define-key lyskom-sv-customize-map (kbd "M-C-i") 'widget-backward)
  (define-key lyskom-sv-customize-map (kbd "C-m") 'widget-button-press)
  (define-key lyskom-sv-customize-map (kbd "C-j") 'widget-button-press)
  (define-key lyskom-sv-customize-map (kbd "<RET>") 'widget-button-press)
  (define-key lyskom-sv-customize-map (kbd "<LFD>") 'widget-button-press)
  (define-key lyskom-sv-customize-map (kbd (lyskom-keys 'button2up)) 'widget-button-click)
  (define-key lyskom-sv-customize-map (kbd (lyskom-keys 'button3up)) 'lyskom-widget-click)
  (define-key lyskom-sv-customize-map (kbd "C-c C-c") 'lyskom-customize-save-and-quit)
  (define-key lyskom-sv-customize-map (kbd "C-c C-k") 'lyskom-customize-quit)
  (define-key lyskom-sv-customize-map (kbd "C-c C-s") 'lyskom-customize-save)
  (define-key lyskom-sv-customize-map (kbd "C-c C-a") 'lyskom-customize-apply)
  (define-key lyskom-sv-customize-map (kbd "?") 'lyskom-customize-help)
)

(lyskom-language-strings lyskom-custom-strings sv
  '(
    ;;
    ;; Widget strings
    ;;

    (which-person . "Ange en person: ")
    (which-conf . "Ange ett m�te: ")
    (which-conf-or-person . "Ange en person eller ett m�te: ")
    (which-name . "Ange ett namn: ")
    (some-person . "Person %#1d")
    (invalid-value . "Otill�tet v�rde (%#1S)")
    (unknown-command . "Ok�nt kommando (%#1s)")

    ;;
    ;; Help messages
    ;;

    (variable-type-help . "Kryssa i f�r att spara inst�llningen i servern")
    (default-help-echo . "�ndra v�rdet p� %#1s.")
    (change-this-name . "�ndra namnet p� denna rad.")
    (show-doc . "Visa hj�lptexten.")
    (hide-doc . "D�lj hj�lptexten.")
    (select-command . "V�lj kommando.")
    (select-what-to-execute . "Exekvera kommando eller tangentbordsmakro.")
    (select-url-viewer . "V�lj en WWW-l�sare.")
    (select-number . "Ange hur m�nga g�nger.")
    (select-audio-file . "Ange en ljudfil.")
    (select-priority . "Ange prioritet.")
    (select-buffer . "Ange buffert.")
    (select-buffer-size . "Ange buffertstorlek.")

    ;;
    ;; Strings that are used in types and so forth
    ;;

    (buffer-name . "%#1s-inst�llningar")

    (other-window     . "N�got annat f�nster            ")
    (other-frame      . "I en annan frame               ")
    (new-frame        . "I en ny frame                  ")
    (lyskom-window    . "LysKOM-buffertens f�nster      ")
    (window-on-buffer . "Ett f�nster som visar bufferten")

    (on . "P�")
    (off . "Av")
    (yes . "Ja ")
    (no . "Nej")
    (max-text-length . "F�r inl�gg kortare �n: ")
    
    (friends . "V�nner och bekanta")

    (turned-off      . "Avslaget            ")
    (number-of-times . "N�gra g�nger")
    (sound-file . "Ljudfil")
    (selected-mark . "Markering")
    (ask . "Fr�ga varje g�ng")
    (before . "F�re texten")
    (after . "Efter texten")
    (depth-first . "I kommentarsordning")
    (time-order . "I tidsordning")

    (express-break . "Omedelbart efter de har skapats")
    (break         . "Efter aktuell kommentarskedja  ")
    (no-break      . "Efter aktuellt m�te            ")

    (command . "Kommando")
    (command-list . "Kommandolista")
    (some-persons . "F�r vissa personer")
    (name . "Namn")

    (page-none . "Aldrig                 ")
    (page-all  . "F�re varje kommando    ")
    (page-some . "F�re f�ljande kommandon")

    (ask-every-time . "Fr�ga varje g�ng      ")
    (fixed-priority . "Fast prioritet")

    (messages-in-lyskom-buffer . "I LysKOM-bufferten ")
    (discard-messages .          "Ingenstans         ")
    (in-named-buffer .           "I namngiven buffert")

    (everybody-rcpt . "Alla")
    (group-rcpt .     "Senaste gruppmeddelandes mottagare")
    (sender-rcpt .    "Senaste meddelandes avs�ndare")

    (viewer-program . "WWW-l�sare")
    (no-viewer            . "(ingenting valt)")
    (default-viewer       . "Browse-URL (alla)")
    (netscape-viewer      . "Netscape Navigator (alla)")
    (emacs-w3-viewer      . "Emacs W3-mode (HTTP, Goper, FTP)")
    (emacs-general-viewer . "Emacs (FTP, Telnet, Mail)")
    (emacs-dired-viewer   . "Emacs Dired (FTP)")
    (emacs-mail-viewer    . "Emacs Mail-mode (Mail)")
    (emacs-telnet-viewer  . "Emacs Telnet-mode (telnet)")
    (mosaic-viewer        . "NCSA Mosaic (alla)")
    (lynx-viewer          . "Lynx (alla)")

    (dont-check . "Ingen bekr�ftelse")
    (check-before-open . "Bekr�fta innan inl�gget skrivs")
    (check-before-send . "Bekr�fta innan inl�gget s�nds")
    (no-size-limit . "Ingen begr�nsning")
    (max-size-in-bytes . "Begr�nsning (i bytes)")
    (execute . "Utf�r")
    (kbd-macro . "Tangentbordmakro")
    (command . "Kommando")
    (enter-kbd-macro . "Mata in tangentbordsmakro. Avsluta med %#1s")

    (long-format  . "Visa hj�lptexter")
    (short-format . "G�m hj�lptexter ")

    (truncate-threshold-lines . "Max rader")
    
    ;;
    ;; Misc doc strings
    ;;

    (lyskom . "Inst�llningar f�r LysKOM")
    (lyskom-doc . "\
\\[lyskom-customize-save-and-quit] f�r att spara och avsluta,
\\[lyskom-customize-save] f�r att spara utan att avsluta
\\[lyskom-customize-quit] f�r att avsluta utan att spara
\\[widget-forward] flyttar till n�sta inst�llning
\\[widget-button-press] �ndrar v�rdet

H�lptexter:  [?] Visa hj�lptext    [!] G�m hj�lptext
Listor mm.:  [INS] L�gg till rad   [DEL] Ta bort rad    [*] �ndra v�rde

Om rutan f�re inst�llningens namn �r ikryssad kommer v�rdet att sparas
i servern. Annars sparas det i din .emacs.")

    (section . "------------------------------------------------------------------------------\n")
    (look-and-feel-misc . "Diverse utseende och beteende\n")
    (window-locations   . "F�nster\n")
    (windows-where      . "Hur skall f�nster skapas:\n")
    (reading .            "Beteende vid l�sning\n")
    (writing .            "Beteende vid skrivning\n")
    (urls .               "Hantering av URLer\n")
    (personal-messages .  "Hantering av personliga meddelanden\n")
    (remote-control .     "Fj�rrstyrning av LysKOM\n")
    (hooks .              "Hook-funktioner\n")

    (audio-cues .         "Ljudsignaler\n")
    (audio-cues-when    . "Ge ljudsignal:\n")
    (automatic-replies .  "Automatiskt svar\n")
    (mail-import .        "Emailimport\n")
    
    (audio-cues-doc . "\
  Efterf�ljande inst�llningar best�mmer vilken ljudsignal LysKOM skall ge 
  i olika situationer. F�ljande alternativ g�r att v�lja:

    Avslaget
        Ingen ljudsignal alls kommer att ges.

    N�gra g�nger
        Emacs kommer att pipa en eller flera g�nger n�r h�ndelsen
        intr�ffar. Antal pip anges ocks�. 

    Ljudfil
        Emacs kommer att spela upp den angivna ljudfilen. Programmet som
        angavs ovan som ljudspelarprogram anv�nds f�r att spela ljudet.")
    

    (sending-doc . "\
  F�ljande inst�llningar sl�r p� eller av vissa kontroller vid inskickning
  av inl�gg. Kontrollerna �r avsedda att hindra att man g�r n�gonting dumt.

    Bekr�fta multipla mottagare
        Om ett inl�gg eller kommentar har flera mottagare s� kan LysKOM
        fr�ga vilka av mottagarna som �r relevanta f�r inl�gget. Antingen
        g�rs detta innan man skriver inl�gget (d� f�r man en fr�ga f�r
        varje defaultmottagare), eller n�r man skickar in inl�gget (d� f�r
        man bekr�fta alla mottagare i klump.) Det g�r ocks� att sl� av
        helt.

    Kontroll av kommenterad f�rfattares medlemsskap
        Kontrollera att f�rfattaren till det kommenterade inl�gget �r
        medlem i n�gon av mottagarna f�r kommentaren. Om s� inte �r fallet,
        erbjuder LysKOM att l�gga till f�rfattaren som mottagare till
        inl�gget. 

    Kontroll av ol�sta kommentarer
        N�r detta �r p�slaget s� kontrollerar LysKOM att det inl�gg man
        kommenterar inte har n�gra kommentarer man inte har l�st. Detta �r
        fr�mst anv�ndbart f�r att undvika att man skriver n�gonting som
        n�gon annan redan har skrivit.")


    (windows-doc . "\
  F�ljande inst�llningar best�mmer hur f�nster skapas i LysKOM. Alternativen
  som finns �r f�ljande:

    N�got annat f�nster 
        I ett annat f�nster i samma frame som LysKOM. Om det bara finns ett
        f�nster s� kommer ett nytt att skapas (och tas bort n�r man �r klar.)

    I en annan frame
        I en annan frame �n LysKOM. Om det bara finns en frame s� kommer en
        ny frame att skapas, och tas bort n�r man �r klar. 

    I en ny frame
        En ny frame skapas f�r �ndam�let, och tas bort n�r man �r klar.

    LysKOM-buffertens f�nster 
        LysKOM-buffertens f�nster kommer att anv�ndas, och LysKOM kommer
        att �terst�llas till f�nstret n�r man �r f�rdig.

    Ett f�nster som visar bufferten 
        Om det finns ett f�nster n�gonstans som visar den angivna bufferten
        s� kommer detta f�nster att anv�ndas. Det kan till exempel vara
        anv�ndbart om man hela tiden har ett f�nster �ppet, men inte be�ver
        det just medan man utf�r n�got visst kommando.")


    ;;
    ;; Doc strings for variables
    ;;

    (kom-emacs-knows-iso-8859-1-doc . "\
  P�slaget betyder att Emacs f�rv�ntas f�rst� ISO-8859-1. Avslaget inneb�r att
  swascii accepteras i kommandonamn med mera. Skall vara p�slaget.")

    (kom-bury-buffers-doc . "\
  Best�mmer hur bufferten hanteras n�r man g�r till ett annat KOM med N�sta
  LysKOM och liknande funktioner. P�slaget inneb�r att den aktuella bufferten
  l�ggs sist i buffertlistan n�r man byter LysKOM.")

    (kom-personal-messages-in-window-doc . "\
  Best�mmer i vilket f�nster som bufferten med personliga meddelanden visas.
  F�r att den h�r inst�llningen skall spela n�gon roll s� m�ste personliga
  meddelanden visas i en separat buffert, och bufferten m�ste visas varje
  g�ng det kommer meddelanden.")

    (kom-write-texts-in-window-doc . "\
  Best�mmer i vilket f�nster nya texter skrivs.")

    (kom-prioritize-in-window-doc . "\
  Best�mmer i vilket f�nster man prioriterar om m�ten.")

    (kom-edit-filters-in-window-doc . "\
  Best�mmer i vilket f�nster man �ndrar filter.")

    (kom-customize-in-window-doc . "\
  Best�mmer i vilket f�nster man g�r dessa inst�llningar.")

    (kom-view-commented-in-window-doc . "\
  Best�mmer i vilket f�nster man visar kommenterarer medan man skriver nya.")

    (kom-list-membership-in-window-doc . "\
  Best�mmer i vilket f�nster man listar medlemsskap.")


    (kom-user-prompt-format-doc . "\
  Format f�r LysKOM-prompten. Vissa teckenkombinationer s�tter in speciella
  texter:

    %c - S�tter in kommandot som k�rs om man trycker p� SPC eller RET.
    %[ - S�tter in '[' om automatsvar �r p�slaget.
    %] - S�tter in ']' om automatsvar �r avst�ngt.     
    %m - S�tter in information om antal inspelade meddelanden.
    %s - S�tter in LysKOM-systemets namn
    %S - S�tter in LysKOM-serverns namn
    %p - S�tter in namnet p� den person som �r inloggad.
    %w - S�tter in namn p� aktuellt m�te.
    %# - S�tter in sessionsnummer.
    %  - S�tter in mellanslag om det ser ut att beh�vas.
    %% - S�tter in ett procenttecken.

  N�gra exempel:

    \"%[%c% %m%] - \"             Standardprompt
    \"%[%s: %c% %m%] - \"         Till exempel \"LysKOM: Se tiden - \"")

    (kom-user-prompt-format-executing-doc . "\
  Format f�r LysKOM-prompten efter man trycker p� SPC eller RET.  Vissa
  teckenkombinationer s�tter in speciella texter:

    %c - S�tter in kommandot som k�rs om man trycker p� SPC eller RET.
    %[ - S�tter in '[' om automatsvar �r p�slaget.
    %] - S�tter in ']' om automatsvar �r avst�ngt.     
    %m - S�tter in information om antal inspelade meddelanden.
    %s - S�tter in LysKOM-systemets namn
    %S - S�tter in LysKOM-serverns namn
    %p - S�tter in namnet p� den person som �r inloggad.
    %w - S�tter in namn p� aktuellt m�te.
    %# - S�tter in sessionsnummer.
    %  - S�tter in mellanslag om det ser ut att beh�vas.
    %% - S�tter in ett procenttecken.

  N�gra exempel:

    \"%[%c% %m%].\"             Standardprompt
    \"%[%s: K�r %c% %m%]...\"   Till exempel \"LysKOM: K�r Se tiden...\"")

    
    (kom-cite-string-doc . "\
  Text som s�tts in f�re varje rad i ett citerat inl�gg. Normalt brukar
  n�gonting i stil med \"> \" anv�ndas. Notera dock att det inte �r brukligt
  att citera KOM-inl�gg som man g�r med sladdpost och News eftersom det
  kommenterade inl�gget oftast finns tillg�ngligt.")


    (kom-created-texts-are-read-doc . "\
  Om detta �r p�slaget kommer alla inl�gg man sj�lv skriver att l�smarkeras
  automatiskt. Om inst�llningen �r avslagen s� f�r man l�sa s�dant man
  sj�lv skriver.")


    (kom-default-mark-doc . "\
  Det markeringsv�rde som anv�nds f�r nya markeringar. Om inget markerings-
  v�rde �r valt fr�gar LysKOM varje g�ng man markerar en text. V�rden mellan
  1 och 255 �r till�tna.")


    (kom-reading-puts-comments-in-pointers-last-doc . "\
  Best�mmer om kommentarsl�nkar visas f�re eller efter en text. Normalt brukar
  man visa kommentarsl�nkar efter texten.

  F�re:

    398331 1996-09-24  13:22  /2 rader/ George Berkeley
    Mottagare: Filosofim�tet <1226>
    Kommentar i text 398374 av John Locke
    �rende: 
    ------------------------------------------------------------
    En abstrakt id� �r en sj�lvmots�gelse. 
    (398331) -----------------------------------

  Efter:

    398331 1996-09-24  13:22  /2 rader/ George Berkeley
    Mottagare: Filosofim�tet <1226>
    �rende: 
    ------------------------------------------------------------
    En abstrakt id� �r en sj�lvmots�gelse. 
    (398331) -----------------------------------
    Kommentar i text 398374 av John Locke
")

    (kom-show-author-at-end-doc . "\
  Om detta �r p�slaget s� visas namnet p� f�rfattaren efter inl�ggstexten.
  Naturligtvis s� visas �ven namnet i inl�ggshuvudet. 

  P�slaget (med streckade linjer ocks� p�slaget):

    892342 1996-09-24  19:21  /2 rader/ Claude Shannon
    Mottagare: Presentation (av nya) Medlemmar
    �rende: Claude Shannon
    ------------------------------------------------------------
    Informationsteoretiker
    (892342) /Claude Shannon/------------------------------

  Avslaget:

    892342 1996-09-24  19:21  /2 rader/ Claude Shannon
    Mottagare: Presentation (av nya) Medlemmar
    �rende: Claude Shannon
    ------------------------------------------------------------
    Informationsteoretiker
    (892342) -----------------------------------

  Om man har streckade linjer avslaget s� visas f�rfattaren p� samma 
  s�tt, men de streckade linjerna finns naturligtvis inte med.")

    (kom-truncate-threshold-doc . "\
  Om detta �r p�slaget kommer inl�gg som �r l�ngre �n maxantalet att kortas
  ner n�r man �terser kommentarer, �terser markerade inl�gg eller �terser de
  senaste eller f�rsta.  Inl�gg kortas inte av d� man �terser inl�gg, �terser
  tr�d eller bara l�ser n�sta inl�gg.")

    (kom-truncate-show-lines-doc . "\
  Antalet rader som visas n�r ett inl�gg avkortats.")
     
    (kom-dashed-lines-doc . "\
  Om detta �r p�slaget visas streckade linjer f�re och efter inl�ggstexten,
  annars visas en tomrad efter �renderaden, och ingenting speciellt i slutet.

  P�slaget:

    892343 1996-09-24  19:21  /2 rader/ Tycho Brahe
    Mottagare: Presentation (av nya) Medlemmar
    �rende: Tycho Brahe
    ------------------------------------------------------------
    Astronom och uppt�ckare av stj�rnor med hemvist p� Ven.
    (892343) -----------------------------------

  Avslaget:

    892343 1996-09-24  19:21  /2 rader/ Tycho Brahe
    Mottagare: Presentation (av nya) Medlemmar
    �rende: Tycho Brahe

    Astronom och uppt�ckare av stj�rnor med hemvist p� Ven.
    (892343)

  De flesta brukar ha detta p�slaget.")

    (kom-autowrap-doc . "\
  Om detta �r p�slaget s� kommer LysKOM att f�rs�ka bryta om raderna i
  stycken vars rader egentligen �r l�ngre �n sk�rmen �r bred. Stycken
  som verkar vara f�rformatterade av f�rfattaren bryts dock aldrig om.")

    (kom-print-number-of-unread-on-entrance-doc . "\
  Om detta �r p�slaget s� visas antalet ol�sta i ett m�te n�r man g�r till 
  m�tet:

  P�slaget:

    G� till n�sta m�te...
    Presentation (av nya) medlemmar - 3 ol�sta
    L�sa n�sta text - 

  Avslaget:

    G� till n�sta m�te...
    Presentation (av nya) medlemmar
    L�sa n�sta text - ")


    (kom-presence-messages-doc . "\
  Om man vill ha meddelanden i minibufferten n�r n�gon loggar in, ut eller
  byter namn i LysKOM b�r man sl� p� den h�r inst�llningen. Meddelanden
  visas i minibufferten. Om man vill ha information om endast vissa
  personer g�r det att ange en lista av personer. Vill man inte ha n�gra
  meddelanden alls �r det bara att ange en tom lista.")

    (kom-presence-messages-in-buffer-doc . "\
  Om man vill ha meddelanden i LysKOM-bufferten n�r n�gon loggar in, ut eller
  byter namn i LysKOM b�r man sl� p� den h�r inst�llningen.  Meddelanden
  visas i minibufferten. Om man vill ha information om endast vissa
  personer g�r det att ange en lista av personer. Vill man inte ha n�gra
  meddelanden alls �r det bara att ange en tom lista.")

    (kom-show-where-and-what-doc . "\
  Information i vilkalistan om varifr�n en person �r inloggad och vad personen
  g�r visas bara om detta �r p�slaget.

  P�slaget:

        Anv�ndare                            N�rvarande i m�te              
        K�r fr�n                             G�r                             
    --------------------------------------------------------------------------
     6810 George Berkeley                      Filosofim�tet                  
          berkeley@emp1.tcd.ie                 (Skriver en kommentar.)        
     7571 John Locke                           Filosofim�tet                  
          eridy@cc.ox.ac.uk                    (V�ntar.)                      

  Avslaget:

        Anv�ndare                            N�rvarande i m�te              
    --------------------------------------------------------------------------
     6810 George Berkeley                      Filosofim�tet                  
     7571 John Locke                           Filosofim�tet")

    (kom-show-since-and-when-doc . "\
  Information i vilkalistan om n�r en person loggade in och hur l�nge sedan
  personen var aktiv.")

    (kom-idle-hide-doc . "\
  I vilkalistan visas per default enbart de sessioner som har varit aktiva
  under den senaste tiden. Denna inst�llning best�mmer hur m�nga minuter
  en session f�r ha varit stilla och �nd� visas i vilkalistan.")


    (kom-show-footnotes-immediately-doc . "\
  Fotnoter visas normalt p� samma s�tt som kommentarer, men om detta �r
  p�slaget s� visas fotnoter direkt efter inl�gget de h�r till.")


    (kom-follow-comments-outside-membership-doc . "\
  Normalt s� visar inte LysKOM kommentarer till ett inl�gg om man inte �r
  medlem i n�got av m�tena kommentaren har som mottagare. Om man vill
  f�lja kommentarskedjor utanf�r de m�ten man �r medlem i kan man sl�
  p� den h�r inst�llningen.")


    (kom-read-depth-first-doc . "\
  LysKOM kan visa inl�gg antingen i den ordning de skapades, eller i den
  ordning de ligger i kommentarstr�det. Ta till exempel f�ljande inl�gg:

  Inl�gg 1002 �r inte en kommentar till n�gonting. Inl�gg 1003 och 1004
  �r kommentarer till inl�gg 1002. Inl�gg 1005 och 1007 �r kommentarer
  till inl�gg 1004 och inl�gg 1006 �r en kommentar till inl�gg 1003. D�
  ser kommentarstr�det ut ungef�r s� h�r:
        
  1002 +-- 1003 --- 1006
       |
       +-- 1004 +-- 1005
                |
                +-- 1007

  Om man l�ser inl�ggen i tidsordning kommer de att visas i ordningen
  1002, 1003, 1004, 1005, 1006 och 1007. I kommentarsordning blir det
  i st�llet 1003, 1003, 1006, 1004, 1005 och 1007.")


    (kom-continuous-scrolling-doc . "\
  P�slaget inneb�r att klienten scrollar bufferten medan ny text s�tts in, s�
  man ser det senaste som har satts in. Detta fungerar bra om man har en
  snabb terminal, men kan vara v�rt att sl� av om terminalen �r l�ngsam, och
  scrollning i Emacs tar tid.")


    (kom-deferred-printing-doc . "\
  F�r att snabba upp LysKOM s� skrivs vissa saker, som person- och m�tesnamn
  inte ut omedelbart, utan skrivs f�rst ut som \"[...]\", vilket sedan byts
  ut mot den r�tta utskriften n�r texten finns tillg�nglig. Detta snabbar
  upp svarstiderna i klienten ganska mycket, men g�r att st�nga av med den
  h�r inst�llningen om man tycker det ser otrevligt ut.")


    (kom-higher-priority-breaks-doc . "\
  N�r texter kommer in till m�ten som har h�gre prioritet �n det man l�ser
  kan klienten l�ta en l�sa dessa texter omedelbart, efter den aktuella
  kommentarskedjan eller efter man har l�st ut m�tet.")
    

    (kom-login-hook-doc . "\
  Kommandon som skall k�ras n�r man loggar in, innan n�gon inmatning fr�n
  tangentbordet tas emot. Till exempel kan man g�ra Lista nyheter h�r f�r
  att f� se en lista av nyheter varje g�ng man loggar in.")


    (kom-do-when-done-doc . "\
  Kommandon som skall k�ras n�r man har l�st ut alla inl�gg. Det �r ganska
  vanligt att man har �terse alla markerade h�r. B�de tangentbordsmakron
  och regelr�tta funktioner g�r bra.")


    (kom-page-before-command-doc . "\
  LysKOM-bufferten kan scrollas f�re alla kommandon s� att text som s�tts in
  av kommandot hamnar �verst i f�nstret. Denna variabel talar om f�re vilka
  kommandon detta skall ske.")


    (kom-permissive-completion-doc . "\
  Om detta �r p�slaget s� kommer TAB bara att fylla ut namn p� inloggade
  personer n�r kommandot bara kan utf�ras p� inloggade personer (till
  exempel Status (f�r) session och S�nda messelande.) Om det �r avslaget
  kommer TAB att fylla ut �ven med namn p� personer som inte �r inloggade.")


    (kom-membership-default-priority-doc . "\
  Detta best�mmer hur prioriteten p� m�ten man g�r med i s�tts. Om det �r
  en siffra mellan 1 och 255 s� kommer nya medlemskap att f� den prioriteten.
  I annat fall kommer klienten att fr�ga f�r varje nytt medlemskap vilken
  prioritet det skall ha.")


    (kom-show-personal-messages-in-buffer-doc . "\
  Denna inst�llning best�mmer var personliga meddelanden, gruppmeddelanden
  och allm�nna meddelanden visas. Meddelanden kan antingen visas i 
  LysKOM-bufferten, kastas bort helt eller visas i en namngiven buffert.")


    (kom-pop-personal-messages-doc . "\
  Om personliga meddelanden skall visas i en egen buffert och denna inst�llning
  ocks� �r p�slagen s� kommer den bufferten att visas s� fort ett meddelande
  anl�nder.")


    (kom-audio-player-doc . "\
  Om man vill att LysKOM skall spela ljudfiler i st�llet f�r att pipa vid 
  olika tillf�llen m�ste denna inst�llning peka ut ett program som klarar
  att spela ljudfilerna. Programmet skall ta ett argument: namnet p� filen
  som skall spelas.")


    (kom-default-message-recipient-doc . "\
  Denna inst�llning best�mmer vem som kommer att vara defaultmottagare f�r
  personliga meddelande man skickar. Alternativen som finns �r att meddelanden
  per default �r allm�nna, att avs�ndaren f�r det senast mottagna meddelandet
  skall vara default eller att mottagaren f�r det senaste gruppmeddelandet
  (eller avs�ndaren av det senaste personliga eller alm�nna meddelandet) skall
  vara mottagare.")


    (lyskom-filter-outgoing-messages-doc . "\
  Om denna inst�llning �r p�slagen s� kommer meddelanden som skickas 
  automatiskt, till exempel automatiska svar och svar p� fj�rkontrollf�rs�k
  �ven att visas som om man hade skickat det manuellt.")

    (kom-friends-doc . "\
  Namnen p� personerna i denna lista kommer att visas med ett speciellt 
  utseende i LysKOM-bufferten.")

    (kom-url-viewer-preferences-doc . "\
  Denna inst�llning best�mmer vilken WWW-l�sare som i f�rsta hand skall 
  anv�ndas f�r att �ppna URLer som f�rekommer i LysKOM. Om den f�rsta l�saren 
  i listan inte klarar den typ av URL som man f�rs�ker �ppna s� anv�nds n�sta
  l�sare och s� vidare.")

    (kom-mosaic-command-doc . "\
  Denna inst�llning talar om vilket kommando som skall anv�ndas f�r att starta
  Mosaic.")

    (kom-netscape-command-doc . "\
  Denna inst�llning anger kommandot f�r att k�ra Netscape.")

    (kom-inhibit-typeahead-doc . "\
  Normalt sparas tangenttryckningar som g�rs medan klienten �r upptagen, och
  utf�rs n�r det blir m�jligt. Om denna inst�llning �r avslagen s� kommer
  LysKOM enbart att utf�ra kommandon som ges n�r klienten inte arbetar.")

    (kom-max-buffer-size-doc . "\
  Det �r m�jligt att begr�nsa LysKOM-buffertens storlek genom att ange hur
  stor den f�r bli i den h�r variabeln. Om bufferten blir f�r stor kommer
  information fr�n buffertens b�rjan att tas bort.")

    (kom-ansaphone-record-messages-doc . "\
  LysKOM kan \"spela in\" meddelanden som kommer n�r funktionen f�r automatiskt
  svar �r p�slagen. Denna inst�llning best�mmer om s� sker eller inte.")

    (kom-ansaphone-show-messages-doc . "\
  Om denna inst�llning �r p�slagen kommer LysKOM att visa inkomna personliga
  meddelanden �ven om automatiskt svar �r p�slaget.")

    (kom-ansaphone-default-reply-doc . "\
  Automatsvararen skickar detta meddelande om inget annat meddelande har
  specificerats via n�gon annan mekanism (och den �r bara till f�r f�rsiktiga
  experter.)")

    (kom-remote-control-doc . "\
  P�slagen inneb�r att det g�r att fj�rrstyra klienten. Fj�rrstyrningen �r
  dock begr�nsad till vissa personer.")

    (kom-remote-controllers-doc . "\
  Personerna i denna lista f�r fj�rrstyra klienten.")

    (kom-self-control-doc . "\
  Om detta �r p�slaget f�r anv�ndaren som �r inloggad styra klienten fr�n
  en annan session. Detta �r ett alternativ till att l�gga in sig sj�lv i
  listan �ver till�tna fj�rrstyrare.")

    (kom-customize-format-doc . "\
  Dokumentation till inst�llningarna kan vara p� eller avslagen n�r 
  inst�llningsf�nstret �ppnas. Dokumentationen kan alltid visas eller
  g�mmas f�r varje enskild inst�llning genom att anv�nda fr�getecknet
  eller utropstecknet som st�r till h�ger om inst�llningen.")

    (kom-default-language-doc . "\
  Spr�k som skall anv�ndas i LysKOM. Om du �ndrar inst�llningen h�r s� kommer
  inte inst�llningen �ndras f�r aktuell session, utan du m�ste ge kommandot
  �ndra spr�k ocks�.")

    (kom-ispell-dictionary-doc . "\
  Ordlista som skall anv�ndas av stavningskontrollen i LysKOM. Om detta
  �r satt till kom-ispell-dictionary s� kommer variabeln ispell-dictionary
  att anv�ndas ist�llet.")

    (kom-show-namedays-doc . "\
  P�slaget inneb�r att dagens namn visas n�r man ser tiden. Antagligen s�
  kommer detta att ers�ttas av ett nytt kommando i n�gon kommande version
  av elispklienten, men det var visst n�gon som ville ha det, s� ...")


    (kom-mail-address-doc . "\
  V�rde f�r From:-headern i skickade email.")
    
    (kom-mail-importer-persons-doc . "\
  Personer som i sj�lva verket �r emailimport�rer.  Inl�gg av dessa
  personer kommer att formatteras som emailtexter ist�llet f�r som
  vanliga inl�gg.")

    (kom-mail-visible-headers-doc . "\
  Visa dessa headers i emailtexter.")
				  
    ;;
    ;; Tags for variables
    ;;
    
    (kom-emacs-knows-iso-8859-1-tag . "Emacs f�rst�r ISO-8859-1:")
    (kom-bury-buffers-tag . "Begrav buffertar n�r man byter LysKOM:")

  (kom-personal-messages-in-window-tag . "Personliga meddelanden:   ")
    (kom-customize-in-window-tag       . "Inst�llningar f�r LysKOM: ")
    (kom-write-texts-in-window-tag     . "Skriv inl�gg:             ")
    (kom-prioritize-in-window-tag      . "Prioritera m�ten:         ")
    (kom-edit-filters-in-window-tag    . "�ndra filter:             ")
    (kom-view-commented-in-window-tag  . "�terse kommenterer:       ")
    (kom-list-membership-in-window-tag . "Lista medlemsskap:        ")

    (kom-user-prompt-format-tag . "Promptformat:")
    (kom-user-prompt-format-executing-tag . "Promptformat vid k�rning:")

    (kom-higher-priority-breaks-tag . 
"L�s prioriterade texter:                  ")
    (kom-created-texts-are-read-tag . 
"L�smarkera skapade texter:                ")
    (kom-default-mark-tag           . 
"Defaultmarkering:                         ")
    (kom-print-number-of-unread-on-entrance-tag . 
"Visa ol�sta n�r man g�r till ett m�te:    ")
    (kom-follow-comments-outside-membership-tag .
"F�lj kommentarskedjor utanf�r medlemskap: ")
    (kom-show-footnotes-immediately-tag .
"Visa fotnoter omedelbart:                 ")
    (kom-membership-default-priority-tag . 
"Prioritet f�r nya medlemskap:             ")
    (kom-dashed-lines-tag . 
"Streckade linjer kring inl�ggstexten:     ")
    (kom-autowrap-tag . 
"Automatisk radbrytning vid l�sning:       ")
    (kom-show-author-at-end-tag .
"Visa f�rfattarens namn efter inl�gget:    ")

    (kom-truncate-threshold-tag .
"Korta av l�nga inl�gg:                    ")
    (kom-truncate-show-lines-tag .
"Rader att visa vid avkortade inl�gg:      ")
    
    (kom-reading-puts-comments-in-pointers-last-tag . "Kommentarsl�nkar visas:")
    (kom-read-depth-first-tag . "L�sordning:")
    (kom-deferred-printing-tag . "F�rdr�jda utskrifter:")
    (kom-continuous-scrolling-tag . "Omedelbar scrollning:")

    (kom-presence-messages-tag . 
"N�rvaromeddelanden p� eller av:       ")
    (kom-presence-messages-in-buffer-tag .
"N�rvaromeddelanden i LysKOM-bufferten:")
    (kom-page-before-command-tag . "Rensa sk�rmen:")

    (kom-idle-hide-tag . 
"Antal minuter en session f�r vara inaktiv och �nd� visas: ")
    (kom-show-where-and-what-tag . 
"Visa varifr�n personer �r inloggade och vad de g�r:         ")
    (kom-show-since-and-when-tag . 
"Visa n�r personer loggade in och n�r de var aktiva:         ")



    (kom-login-hook-tag . "Kommandon som k�rs vid login:")
    (kom-do-when-done-tag . "Kommandon som k�rs efter allt �r utl�st:")
    (kom-permissive-completion-tag . "Petig utfyllnad av namn:")
    (kom-show-personal-messages-in-buffer-tag . 
"Var visas meddelanden:           ")
    (kom-pop-personal-messages-tag . 
"Ploppa upp meddelandebufferten:  ")
    (kom-default-message-recipient-tag . 
"Defaultmottagare f�r meddelanden:")

    (kom-audio-player-tag . "Ljudspelarprogram:")
    (kom-ding-on-new-letter-tag        . "N�r det kommer brev:               ")
    (kom-ding-on-priority-break-tag    . "N�r det kommer prioriterade inl�gg:")
    (kom-ding-on-wait-done-tag         . "N�r man har v�ntat klart:          ")
    (kom-ding-on-common-messages-tag   . "Vid allm�nna meddelanden:          ")
    (kom-ding-on-group-messages-tag    . "Vid gruppmeddelanden:              ")
    (kom-ding-on-personal-messages-tag . "Vid personliga meddelanden:        ")
    (kom-ding-on-no-subject-tag     . "N�r �renderad saknas:              ")

    (lyskom-filter-outgoing-messages-tag . "Visa automatiska meddelanden:")
    (kom-friends-tag . "V�nner och bekanta:")
    (kom-url-viewer-preferences-tag . "�ppna URLer med f�ljande program:")
    (kom-mosaic-command-tag . "Kommando f�r att starta NCSA Mosaic:")
    (kom-netscape-command-tag . "Kommando f�r att starta Netscape Navigator:")

    (kom-cite-string-tag . "Citatmarkering: ")
    (kom-confirm-multiple-recipients-tag . 
"Bekr�fta multipla mottagare:                   ")
    (kom-check-commented-author-membership-tag . 
"Kontrollera kommenterad f�rfattares medlemskap:")
    (kom-check-for-new-comments-tag . 
"Kontrollera ol�sta kommentarer:                ")

    (kom-ansaphone-record-messages-tag . 
"Spara meddelanden d� automatiskt svar �r p�slaget: ")
    (kom-ansaphone-show-messages-tag . 
"Visa meddelanden d� automatiskt svar �r p�slaget:  ")
    (kom-ansaphone-default-reply-tag . "Svarsmeddelande:")


    (kom-inhibit-typeahead-tag . "Buffra tangenttryckningar:")
    (kom-max-buffer-size-tag . "Maximal buffertstorlek:")

    (kom-remote-control-tag .     "Fj�rrstyrning:                     ")
    (kom-self-control-tag .       "Till�t fj�rrstyrning av mig sj�lv: ")
    (kom-remote-controllers-tag . "Till�tna fj�rrstyrare:")

    (kom-customize-format-tag . "Visa hj�lptexterna f�r inst�llningar:")
    (kom-default-language-tag . "Spr�k:           ")
    (kom-show-namedays-tag .        "Visa dagens namn:")
    (kom-ispell-dictionary-tag . "Ordlista:")

    (kom-mail-address-tag .          "Emailadress:       ")
    (kom-mail-importer-persons-tag . "Mailimport�rer:    ")
    (kom-mail-visible-headers-tag .  "Visa dessa headers:")
    )
)

  
;;;; ============================================================
;;;; The default Ansaphone message goes here. The more complex 
;;;; message specification probably should too, but it's not here
;;;; yet. People who know how to use it are smart enough to do it
;;;; right.

(lyskom-language-var kom-ansaphone-default-reply sv
  "Jag l�ser inte LysKOM just nu. Skicka g�rna ett brev i st�llet.")

;;;; ============================================================
;;;; Other language-dependent variables
;;;;

(lyskom-language-var kom-ispell-dictionary sv
  "svenska")


;;;; ============================================================
;;;; Text buttom menuse go here. This will probably be moved back
;;;; to vars.el.in when the strings have been replaced by
;;;; symbols, but for now they'll stay here.


(lyskom-language-var lyskom-button-actions sv
  '((text
     text-text
     lyskom-button-view-text
     (("�terse texten" . lyskom-button-view-text)
      ("�terse omodifierat" . lyskom-button-review-noconversion)
      ("�terse tr�d" . lyskom-button-review-tree)
      ("�terse urinl�gget" . lyskom-button-find-root)
      ("Kommentera texten" . lyskom-button-comment-text)
      ("Personligt svar" . lyskom-button-private-comment-text)
      ("Markera texten" . lyskom-button-mark-text)
      ("Avmarkera texten" . lyskom-button-unmark-text))
     nil
;     ((nil lyskom-print-text footer lyskom-button-comment-text))
     )
    (conf 
     conf-text
     lyskom-button-view-conf-presentation
     (("Visa presentation" . lyskom-button-view-conf-presentation)
      ("Visa m�tesstatus" . lyskom-button-view-conf-status)
      ("G� till m�tet" . lyskom-button-goto-conf)
      ("Skicka gruppmeddelande" . lyskom-button-send-message)
      ("Bli medlem i m�tet" . lyskom-button-add-self)
      ("Uttr�d ur m�tet" . lyskom-button-sub-self))
     ((kom-list-news . lyskom-button-goto-conf)
      (kom-membership . lyskom-button-goto-conf)))
    (pers 
     pers-text
     lyskom-button-view-pers-presentation
     (("Visa presentation" . 
                             lyskom-button-view-pers-presentation)
      ("Visa personstatus" . lyskom-button-view-pers-status)
      ("Skicka brev" . lyskom-button-mail)
      ("S�nd meddelande" . lyskom-button-send-message))
     nil)
    (url 
     url-text
     lyskom-button-open-url
     (("�ppna" . lyskom-button-open-url)
      ("Kopiera" . lyskom-button-copy-url))
     nil)
    (info-node 
     info-node-text
     lyskom-button-goto-info-node
     (("�ppna" . lyskom-button-goto-info-node))
     nil)
    (email
     email-text
     lyskom-button-open-email
     (("Skicka mail" . lyskom-button-open-email)
      ("Kopiera" . lyskom-button-copy-email))
     nil)
    (aux
     aux-text
     lyskom-button-info-aux
     (("Information" . lyskom-button-info-aux)
      ("Radera" . lyskom-button-delete-aux)))
    (aux-edit-menu
     nil
     aux-edit-menu-text
     (("V�xla \"hemlig\"" . lyskom-edit-toggle-secret-aux)
      ("V�xla \"anonym\"" . lyskom-edit-toggle-anonymous-aux)
      ("V�xla \"�rvd\""   . lyskom-edit-toggle-inherit-aux)
      ("Ta bort"          . lyskom-edit-delete-aux)))
))
        

;;;; ================================================================
;;;; Tell phrases should really be configured with the default
;;;; language used at the server and not for person reading if they
;;;; happens to differ. This is of coarse because they are sent to the
;;;; server for everybody else to see.

;;;; Aronsson was here 4 DEC 1990, thus creating version 0.18
					; Created *-tell-*


;;;; kom-tell-phrases 

;;; To coders of the elisp-client:
;;; if you change kom-tell-phrases here, make sure the list of valid
;;; keywords is updated it in vars.el.
;;; lyskom-tell-phrases-validation-keyword-list, lyskom-tell-phrases-validate

(eval-when-compile (defvar kom-tell-phrases))

(lyskom-language-strings 
 kom-tell-phrases sv
 '((kom-tell-silence		. "")   ; Why ?
   (kom-tell-send		. "F�rs�ker l�gga in en text.")
   (kom-tell-login		. "Loggar in.")
   (kom-tell-read		. "L�ser.")
   (kom-tell-1st-pres		. "Skriver den f�rsta presentationen.")
   (kom-tell-write-comment	. "Skriver en kommentar.")
   (kom-tell-write-footnote	. "Skriver en fotnot.")
   (kom-tell-write-letter	. "Skriver ett brev.")
   (kom-tell-write-reply	. "Skriver ett privat svar.")
   (kom-tell-write-text	. "Skriver inl�gg.")
   (kom-tell-conf-pres		. "Skriver presentation f�r ett nytt m�te.")
   (kom-tell-recover		. "�terstartar kom. Suck.")
   (kom-tell-wait		. "V�ntar.")
   (kom-tell-regret		. "�ngrar sig och sl�nger inl�gget.")
   (kom-tell-review		. "�terser.")
   (kom-tell-change-name       . "�ndrar sitt namn till n�got annat.")
   (kom-tell-change-supervisor . "�ndrar organisat�r f�r n�got.")
   (kom-tell-next-lyskom       . "Hoppar till ett annat LysKOM.")
   (kom-tell-is-anonymous      . "�r hemlighetsfull")))

(if (and (boundp 'kom-tell-phrases)
         kom-tell-phrases)
    (lyskom-language-strings kom-tell-phrases sv
      (mapcar (function 
               (lambda (x)
                 (cond ((and (consp x)
                             (symbolp (car x))
                             (stringp (cdr x))) x)
                       ((and (consp x)
                             (symbolp (car x))
                             (consp (cdr x))
                             (stringp (car (cdr x))))
                        (cons (car x) (car (cdr x))))
                       (t nil))))
              kom-tell-phrases)))


;; Placed here because this must NOT be evaluated before 
;; kom-tell-phrases is defined:

(lyskom-language-var kom-mercial sv (lyskom-get-string 'kom-tell-wait 'kom-tell-phrases))

;;;


(lyskom-language-strings lyskom-error-texts sv
  '((error-0 . "Inget fel har intr�ffat")
    (error-2 . "�nnu ej implementerat")
    (error-3 . "Inte l�ngre implementerat")
    (error-4 . "Felaktigt l�senord")
    (error-5 . "Str�ngen f�r l�ng")
    (error-6 . "Du �r inte inloggad")
    (error-7 . "Ingen f�r logga in i LysKOM just nu")
    (error-8 . "Du f�rs�kte anv�nda m�te nummer 0")
    (error-9 . "Odefinierat eller hemligt m�te")
    (error-10 . "Odefinierad eller hemlig person")
    (error-11 . "Ingen skriv- eller l�sr�ttighet")
    (error-12 . "Otill�ten operation")
    (error-13 . "Du �r inte medlem i m�tet")
    (error-14 . "Inget s�dant textnummer")
    (error-15 . "Du kan inte anv�nda globalt textnummer 0")
    (error-16 . "Inget s�dant lokalt textnummer")
    (error-17 . "Du kan inte anv�nda lokalt textnummer 0")
    (error-18 . "Namnet f�r kort eller f�r l�ngt eller inneh�ller felaktiga tecken")
    (error-19 . "Index utanf�r gr�nserna")
    (error-20 . "M�tet existerar redan")
    (error-21 . "Personen existerar redan")
    (error-22 . "Hemligt, men ej l�sskyddat")
    (error-23 . "Du f�r inte �ndra person/m�tesflaggan")
    (error-24 . "Fel i databasen. Attans otur.")
    (error-25 . "Otill�tet misc-f�lt. (Internt fel)")
    (error-26 . "Otill�ten infotyp. (Bug i klienten)")
    (error-27 . "Redan mottagare till denna text")
    (error-28 . "Redan kommentar till denna text")
    (error-29 . "Redan fotnot till denna text")
    (error-30 . "Inte mottagare till denna text")
    (error-31 . "Inte kommentar till denna text")
    (error-32 . "Inte fotnot till denna text")
    (error-33 . "F�r m�nga mottagare")
    (error-34 . "F�r m�nga kommentarer")
    (error-35 . "F�r m�nga fotnoter")
    (error-36 . "F�r m�nga markeringar")
    (error-37 . "Du �r inte f�rfattare till texten")
    (error-38 . "Du kan inte koppla upp dig till servern")
    (error-39 . "Minnet slut")
    (error-40 . "Servern har ballat ur")
    (error-41 . "Klienten tror att servern s�ger att den inte f�rst�r klienten")
    (error-42 . "Ingen s�dan session finns")
    (error-43 . "Ogiltigt regulj�rt uttryck")
    (error-44 . "Texten �r inte markerad")
    (error-45 . "Tillf�lligt fel. F�rs�k senare")
    (error-46 . "Serven v�grade ta emot en array")
    (error-47 . "Mottagaren tar inte emot anonyma texter")
    (error-48 . "Otill�ten till�ggsinformation")
    (error-49 . "Otill�ten �ndring av till�ggsinformation")
    (error-50 . "Ok�nt asynkront meddelande")
    (error-51 . "Internt fel i servern")
    (error-52 . "Funktionen avst�ngd i servern")
    (error-53 . "Meddelandet kunde inte s�ndas")
    (error-54 . "Otill�ten typ p� medlemskap")
))


(provide 'lyskom-language-sv)

;;; swedish-strings.el ends here
