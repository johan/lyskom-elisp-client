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
  (define-key lyskom-sv-edit-prefix (kbd "C-Å")     'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-å")     'lyskom-sv-edit-review-prefix)
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
    (try-connect . "LysKOM elisp-klient version %#1s.\nFörsöker koppla upp mot %#2s.\n")
    (protocoll-error . "Protocol error. Servers says: %#1s")
    (too-old-server . "Servern är för gammal för den här versionen av klienten.")
    (connection-done . "Uppkopplingen klar. Serverns versionsnummer är %#1s.\n\n")
    (what-is-your-name . "Vad heter du? ")
    (password . "Lösenord? ")
    (wrong-password . "Fel lösen.\n")
    (are-logged-in . "Du är nu inloggad. Vänta ett tag.\n")
    (you-have-motd . "\nDu har en lapp på dörren:\n\n")
    (lyskom-motd-was-garbed . "\nLoginmeddelandet finns inte!
Det meddelande som ska visas efter inloggning har försvunnit.
Kontakta LysKOM-administratören.\n")
    (presentation-encouragement . 
				"Du har ingen presentation. Det skulle vara trevligt om du skrev en.
Använd kommandot Äp. Om du inte vill skriva någon presentation tryck fs.\n")

    (first-greeting . "%#1s
Det tycks vara första gången du använder LysKOM. Välkommen!
Kontrollera att du stavat ditt namn rätt.  Använd gärna ditt fullständiga
namn och organisation, t ex \"Eskil Block, FOA\".  Om du stavat ditt namn
fel, eller vill ändra ditt namn, svara nej på frågan nedan.

Observera att all information än så länge sparas så att vem som helst
kan läsa den. Dock är lösenordet krypterat.

Om du är osäker på hur man använder LysKOM kan du hämta en manual via
anonym ftp från ftp.lysator.liu.se. Fråga din systemadministratör om
du är osäker.\n")

    (is-name-correct . "Är namnet %#1s korrekt? ")
    (personal-password . "Ange ett personligt lösen: ")
    (repeat-password . "Repetera för kontroll: ")
    (repeat-failure . "Du angav inte samma lösenord bägge gångerna.\n")

    (could-not-create-you .  "Det gick inte att skapa personen.\n")
    (presentation-subject . "%#1s")
    (presentation-form . "Namn:\t\nAdress:\t\nStad:\t\nTelefon:
Email-adress:\nWWW:\t\n\nAnnat:\t")
    (presentation-help . "Du skriver just  nu din presentation.\n")
    (not-present-anywhere . "Ej närvarande i något möte.")
    (secret-person . "Hemlig person")
    (in-secret-conference . "Hemligt möte (%#1d).")
    (start-new-session-same-server
     . "Du kör redan mot den servern. Vill du starta en ny session? ")
    (new-session-in-buffer . "\n\n---- Ny session startad %s ----\n\n")


    ;; From internal.el:
    (shaky-tcp . "Nu når jag inte servern. TCP/IP-förbindelsen är skakig%#1s")
    (retrying-tcp . "Jag försöker igen.")

    ;; From parse.el:
    (protocol-error . "protokollfel: %s")

    ;; From services.el:
    (interrupted . "Kommandot avbrutet\n")

    ;; From cache.el:
    ;; No entries.

    ;; From commands1.el:
    (appreciation . "Du är mycket vacker och mycket klok. Många älskar dig både till kropp
och till själ. Du kommer att överösas med rikedom och få stor lycka i ditt
liv. Var glad att just du är du. Det har du all anledning att vara.
Fantomen önskar dig en bra dag.\n\n")
    (abuse . "Du är mycket ful och mycket dum. Många hatar dig både till kropp
och till själ. Du kommer att överösas med sjukdomar och inte få någon som
helst lycka i ditt liv. Du borde verkligen ta dig samman och göra något av
ditt liv även om det inte är mycket att starta med.
Guran vill helst sätta en giftpil i dig.\n\n")

    (what-conf-to-delete . "Vilket möte/person vill du utplåna: ")
    (what-conf-to-change . "Vilket möte vill du ändra: ")
    (confirm-delete-pers-or-conf . "Ta bort %#1s %#2s? ")
    (the-pers . "personen")
    (the-conf . "mötet")
    (deletion-not-confirmed . "Utplåningen avbruten\n")
    (somebody-else-deleted-that-conf . "Någon annan tog precis bort mötet.\n")
    (conf-is-deleted . "Ok, nu är %#1s utplånad.\n")
    (you-could-not-delete . "%#1M kunde inte utplånas av dig.\n")
    (you-have-deleted-yourself . "Du har utplånat dig själv.\n")

    (what-text-to-delete . "Vilket inlägg skall tas bort? ")
    (delete-marked-text . "Inlägget är %#1s. Ta bort ändå? ")
    (delete-marked-by-you . "markerat av dig")
    (delete-marked-by-you-and-one . "markerat av dig och någon annan")
    (delete-marked-by-you-and-several . "markerat av dig och %#1d andra")
    (delete-marked-by-one . "markerat av 1 person")
    (delete-marked-by-several . "markerat av %#1d personer")
    (deleting-text . "Radering av text %#1:n...")

    (presentation-for-whom . "Vilket möte/person? ")
    (somebody-deleted-that-conf . "Någon tog precis bort mötet.\n")
    (review-presentation-of . "Återse presentation av %#1M.\n")
    (has-no-presentation . "%#1:M har ingen presentation.\n")

    (have-to-read . "Du måste läsa ett inlägg först.\n")

    (no-comment-to . "Det finns inget kommenterat inlägg att titta på.\n")

    (who-letter-to . "Vem vill du skicka brev till? ")
    (who-send-text-to . "Vem vill du skicka inlägget till? ")
    (has-motd . "%#1P har en lapp på dörren:\n\n")
    (motd-persist-q . "Vill du fortfarande skicka brevet? ")

    (who-to-add . "Vem vill du addera? ")
    (where-to-add . "Vilket möte skall han/hon adderas till? ")
    (where-to-add-self . "Vilket möte vill du bli medlem i? ")
    (priority-q . "Prioritet på mötet? (0 (passivt medlemskap), 1 (låg) - 255 (hög)) ")
    (done . "klart.\n")
    (nope . "Det gick inte.\n")

    (cant-find-supervisor . "Hittar inte organisatören för %#1M.\n")
    (is-read-protected-contact-supervisor . "%#1M är slutet.
Skicka ett brev till %#2P för medlemsskap.\n")

    (conf-does-not-exist . "\nMötet finns inte.\n")

    (who-to-exclude . "Vem vill du utesluta? ")
    (where-from-exclude . "Vilket möte skall han/hon uteslutas ifrån? ")

    (leave-what-conf . "Vilket möte vill du gå ur? ")

    (error-fetching-person . "Fel i personhämtningen.\n")
    (error-fetching-conf . "Fel i möteshämtningen.\n")

    (name-of-conf . "Vad ska mötet heta? ")
    (anyone-member . "Får vem som helst bli medlem? ")
    (secret-conf . "Hemligt möte? ")
    (comments-allowed . "Får man skriva kommentarer? ")
    (anonymous-allowed . "Får man skriva anonyma texter? ")
    (secret-members-allowed . "Får man gå med som hemlig medlem? ")
    (what-comment-no . "Kommentera text nummer: ")
    (what-footnote-no . "Fotnotera text nummer: ")
    (confusion-what-to-comment . "Jag förstår inte vilken text du vill kommentera.\n")
    (confusion-what-to-footnote . "Jag förstår inte vilken text du vill skriva en fotnot till.\n")

    (what-private-no . "Personligt svar till text nummer: ")
    (confusion-who-to-reply-to . "Jag förstår inte vems inlägg du vill
 skriva ett privat svar till.\n")
    (confusion-what-to-answer-to . "Jag förstår inte vilken text du vill besvara.\n")
    (confusion-what-to-view . "Jag förstår inte vilken text du vill återse.\n")
    (quit-in-spite-of-unsent . "Vill du avsluta sessionen trots osänt meddelande? ")
    (really-quit . "Vill du verkligen avsluta sessionen? ")
    (session-ended . "
*****************************
Lyskom-sessionen är avslutad.
*****************************\n")
    (session-auto-ended . "
===========================================================
Kopplar ned från LysKOM eftersom LysKOM är fullt och
du har läst klart allting. Kom tillbaks senare.
===========================================================\n\n")
    (what-to-change-pres-you . "Vilket möte/person vill du ändra presentationen för (dig själv): ")
    (who-to-put-motd-for . "Vilket möte/person vill du sätta lapp på dörr för (dig själv): ")

    (cant-get-conf-stat . "Kan ej hämta mötesstatus för mötet.\n")
    (go-to-conf-p . "Gå till möte: ")
    (want-become-member . "Vill du bli medlem? ")
    (no-ok . "Nehej.\n")

    (who-to-remove-motd-for . "Vilket möte/person vill du ta bort lapp på dörr för (dig själv): ")

    (conf-all-read . "%#1M - inga olästa.\n")
    (no-in-conf . "Du är inte närvarande i något möte.\n")

    (search-for-pers . "Ange sökvillkor (RETURN för alla personer): ")
    (search-for-conf . "Ange sökvillkor (RETURN för alla möten): ")
    (search-re . "Ange sökuttryck (reguljärt uttryck): ")
    (name-to-be-changed . "Ange det namn som skall ändras: ")
    (no-such-conf-or-pers . "Mötet eller personen finns ej.\n")
    (new-name . "Nytt namn: ")
    (new-paren . "Ny parentes: ")
    (no-paren-in-name . "Namnet innehåller ingen parentes.\n")
    (who-to-change-supervisor-for . "Vem vill du ändra organisatör för? ")
    (new-supervisor . "Ny organisatör: ")
    (text-to-mark . "Vilket inlägg vill du markera? ")
    (text-to-unmark . "Vilket inlägg vill du avmarkera? ")
    (what-mark . "Vilken markering vill du sätta? ")
    (unmarking-textno . "Avmarkering av text %#1n...")
    (marking-textno . "Markering av text %#1n...")
    (list-which-mark . "Lista vilken markering (0-255, RET för alla)? ")

    (new-passwd-again . "Mata in det nya lösenordet igen för kontroll: ")
    (what-mark-to-view . "Vilken markering vill du återse? ")
    (whos-passwd . "Vem vill du ändra lösenord för? (dig själv) ")
    (old-passwd . "Mata in ditt nuvarande lösenord: ")
    (new-passwd . "Mata in det nya lösenordet: ")
    (changing-passwd . "Ändrar lösenordet...")
    (retype-dont-match . "Lösenorden är inte samma. Gör om.\n")
    (palindrome . "(en palindrom!) ")
    (lyskom-name . "Användare")
    (is-in-conf . "Närvarande i möte")
    (from-machine . "Kör från")
    (is-doing . "Gör")
    (connection-time . "Loggade in")
    (active-last . "Aktiv senast")
    (active . "Aktiv")
    (lyskom-client . "Klient")
    (text-to-add-recipient . "Vilket inlägg vill du addera mottagare till:")
    (text-to-add-copy . "Vilket inlägg vill du addera en extra kopia till:")
    (text-to-add-bcc . "Vilket inlägg vill du skicka för kännedom:")
    (text-to-delete-recipient . "Vilket inlägg vill du subtrahera mottagare från:")
    (text-to-move . "Vilket inlägg vill du flytta: ")
    (text-to-add-comment-to . "Vilket inlägg vill du addera en kommentar till:")
    (text-to-delete-comment-from . "Vilket inlägg vill du subtrahera en kommentar från:")

    (where-on-list-q . "Placering på listan? (0-%#1d) ")
    (member-in-conf . "Bli medlem i %#1M...")
    (add-member-in . "Addera %#1P som medlem i %#2M...")
    (unsubscribe-to . "Utträda ur %#1M...")

    (exclude-from . "Utesluta %#1P från %#2M...")

    (unsubscribe-failed . "\nDet gick inte. %#1P var kanske aldrig medlem i %#2M?\n")

    (You . "Du")
    (could-not-create-conf . "Mötet \"%#1s\" kunde ej skapas.\n")
    (created-conf-no-name . "Möte nummer %[%#3@%#1:m %#2:M%], skapat.\n")
    (cant-read-textno . "Du får inte läsa text %#1:n")

    (not-supervisor-for . "Du är inte organisatör för %#1M.\n")
    (go-to-conf . "Gå till %#1M.\n")
    (cant-go-to-his-mailbox . "Du får inte gå till %#1Ms brevlåda.\n")
    (not-member-of-conf . "Du är inte medlem i %#1M.\n")
    (about-to-change-name-from . "%#1M\n")
    (change-name-done . "Klart. Nytt namn: %[%#2@%#1:M%].\n")
    (change-name-nope . "Det gick inte bra att ändra till %#1s.\nFelkod %#3d. %#2s.\n")
    (change-supervisor-from-to . "Ändra organisatör för %#1M till %#2P...")
    (change-supervisor-nope . 
			    "\nDet gick inte. Kanske du inte får ändra organisatör för %#1M?\n")
    
    (no-marked-texts . "Du har inga markerade inlägg.\n")
    (no-marked-texts-mark . 
			  "Du har inga markerade inlägg med markeringen %#1d.\n")

;;; For later
;    (northward . "norrut")
;    (southward . "söderut")
;    (permanent-sundown . "Solen kommer inte att gå upp. Flytta %#1s!")
;    (permanent-sunup . "Solen kommer inte att gå ned. Fytta %#1s!")
;    (sunup-soon . "Solen går snart upp")
;    (sundown-recently . "Solen gick nyligen ned")
;    (after-sunset . "Solen har gått ned")
;    (before-sunup . "Solen har inte gått upp ännu")
;    (sun-is-up . "Solen är uppe")
;;;
    (weekdays . ["söndag" "måndag" "tisdag" "onsdag" "torsdag"
		 "fredag" "lördag" "söndag"])
    (time-is . "Det är %#1s %#2s(enligt servern).")
    (time-format-exact . "%#7s %4#1d-%02#2d-%02#3d %02#4d:%02#5d:%02#6d")

    (xmaseve . "\nJulafton! Har du öppnat dina julklappar än?")
    (xmasday . "Juldagen.\nDu har väl varit i julottan?")
    (newyearday . "Gott nytt %#1d!")
    (newyearevelate . "Mindre än en timme kvar...")
    (newyeareve . "Gott nytt år!")
    (cgdag . "Konungens födelsedag.")
    (sixjune . "Sveriges nationaldag och svenska flaggans dag")
    (holdnose . "Håll för näsan...")
    (lysbday . "
På denna dag, år 1973, grundades Lysator, och det var en stor dag
i svensk datorhistoria. Läs mer på http://www.lysator.liu.se/history/")


    (total-users . "    Sammanlagt %#1d användare.\n")
    (total-visible-users . "    Sammanlagt %#1d synliga användare.\n")
    (total-active-users . "    Sammanlagt %#1d aktiva användare.\n")
    (total-visible-active-users
     . "    Sammanlagt %#1d synliga aktiva användare.\n")
    (who-to-add-q . "Vilket möte/person vill du addera som mottagare? ")
    (who-to-add-copy-q . "Vilket möte/person vill du addera som kopiemottagare? ")
    (who-to-sub-q . "Vilket möte/person vill du subtrahera som mottagare? ")
    (who-to-move-from-q . "Från vilket möte vill du flytta texten? ")
    (who-to-move-to-q . "Vart vill du flytta texten? ")

    (adding-name-as-recipient . "Adderar %#1M som mottagare till text %#2n...")
    (adding-name-as-copy . "Adderar %#1M som kopiemottagare till text %#2n...")
    (remove-name-as-recipient .
			      "Subtraherar %#1M som mottagare från text %#2n...")
    (moving-name . "Flyttar text %#3n från %#1M till %#2M...")
    (text-to-add-q . "Vilket inlägg vill du addera som kommentar? ")
    (text-to-remove-q . "Vilket inlägg vill du subtrahera som kommentar? ")
    (add-comment-to . "Adderar text %#1n som kommentar till text %#2n...") ;
    (sub-comment-to . "Subtraherar text %#1n som kommentar till text %#2n...")
    (comment-keep-recpt-p ."Ska %#1s vara mottagare? ")
    (comment-all-relevant-p . "Inlägget har flera mottagare. Är alla relevanta? ")
    (please-edit-recipients . "Ändra mottagarlistan och skicka in inlägget igen.")
    (checking-rcpt . "Kontrollerar mottagare...")
    (checking-rcpt-done . "Kontrollerar mottagare...klart")
    (checking-comments . "Kontrollerar kommenterade texter...")
    (checking-comments-done . "Kontrollerar kommenterade texter...klart")
    (please-check-commented-texts . "Återse de kommenterade texterna och deras kommentarer.")
    (have-unread-comment . "Skicka in trots olästa kommentarer till text %#1n? ")

    (add-recipient-p . "Addera %#1P som mottagare? ")
    (matching-regexp . "Möten/personer som matchar '%#1s'\n")

    (who-is-active-all . "Visar alla sessioner.\n")
    (who-is-active-last-minutes . "Visar alla sessioner som har varit aktiva de senaste %#1d minuterna.\n")
    (showing-invisibles . "Visar osynliga sessioner.\n")
    (null-who-info . "Det finns inga (aktiva) inloggade.\n")

    (no-other-lyskom-r . "Det finns inga fler aktiva LysKOM-sessioner.\n")
    (no-lyskom-session . "Det finns ingen aktiv LysKOM-session.")
    (no-unread-lyskom-r . "Hittar ingen aktiv LysKOM-session med olästa.\n")
    (no-unread-lyskom . "Hittar ingen aktiv LysKOM-session med olästa.")

    (who-is-on-in-what-conference . "Vilka i vilket möte: ")
    (who-is-active-and-member . "Visar endast medlemmar i %#1M.\n")
    
    ;; From commands2.el:

    (your-memberships . "Ditt medlemskap i olika KOM-möten:\n")
    (memberships-header . "Senast inne	   Prio	Olästa	Mötesnamn\n")
    (memberships-line . "%16#1s  %#2d\t%#3d\t%#4M\n")
    (conf-for-status . "Vilket möte vill du se statusen för? ")
    (no-such-conf . "Mötet finns ej.\n")
    (status-record . "Status för möte %#1M (%#2m) %#3s\n\n")
    (change-type-prompt . "Ändra mötestyp för möte %#1M (%#2m) %#3s\n")
    (Mailbox . "Brevlåda")
    (Protected . "Skyddat")
    (no-comments . "original")
    (closed . "slutet")
    (allow-anon . "anonymt ok")
    (allow-secret . "hemliga medl")

    (created-by . "Skapat av person %25#1p %#3s(%#2P)\n")
    (created-at . "Skapad:%35#1s\n")
    (members .    "Antal medlemmar: %25#1d\n")
    (conf-allows-secret-members . "Hemliga medlemmar:                      %#1s\n")
    (conf-allows-anon-texts .     "Anonyma inlägg:                         %#1s\n")
    (anon-texts-permitted . "Anonyma inlägg är tillåtna")
    (anon-texts-not-permitted . "Anonyma inlägg är inte tillåtna")
    (secret-members-permitted . "Hemliga medlemmar är tillåtna")
    (secret-members-not-permitted . "Hemliga medlemmar är inte tillåtna")
    (garb-nice . "Livslängd på inlägg (dagar):%14#1d\n")
    (lowest-local-no . "Lägsta existerande lokala nummer: %8#1d\n")
    (highest-local-no . "Högsta existerande lokala nummer: %8#1d\n")
    (last-text-time . 
		    "Tid för senaste inlägg: %18#1s (står det i din cache)\n")
    (no-of-motd . "Lapp på dörren i text nummer: %12#1n\n")
    (superconf-is-no-name . "Supermöte:       %25#1m %#3s(%#2M)\n")
    (permitted-submitters-no-name . "Tillåtna författare:%22#1m %#3s(%#2M)\n")
    (supervisor-is-no-name . "Organisatör:     %25#1p %#3s(%#2P)\n")
    (presentation-no . "Presentation:    %25#1n\n")
    (conf-has-motd . "\n%#1M har en lapp på dörren:\n")

    (Everybody . "Alla")
    (show-members-list-also-q . "Vill du se medlemslistan också? ")
    (show-membership-info-q . "Visa antalet olästa? ")
    (conf-has-these-members . "\n%#1M har följande medlemmar:\n")
    (conf-has-no-members . "\n%#1M har inga medlemmar.\n")
    (member-list-header . "Senast inne         Osett  Namn\n\n")
    (secret-membership . "*** Hemlig rad ***\n")
    (conf-membership-line . "%#1s%#2M %#3s\n")
    (conf-membership-line-2 . "                          Adderad %#1s av %#2P\n")
    (pers-for-status . "Vem vill du se statusen för? ")
    (no-such-pers . "Det finns ingen sådan person.\n")
    (pers-status-record . "Status för person %#1P (%#2p)\n")
    (created-time .  "Skapad:%34#1s\n\n")
    (created-confs . "Skapade möten:%27#1d\n")
    (created-persons . "Skapade personer:%24#1d\n")
    (created-texts . "Skapade texter:%26#1d\n")
    (created-lines . "Skapade rader:%27#1d\n")
    (created-chars . "Antal skapade tecken:%20#1d\n")
    (no-of-sessions . "Antal sessioner:%25#1d\n")
    (present-time-d-h-m-s . "Närvarotid:%19#1d d %02#2d:%02#3d:%02#4d\n")
    (last-log-in . "Senast inne:%29#1s\n")
    (user-name . "Användare: %30#1s\n")

    (read-texts . "Lästa texter:%28#1d\n")
    (marked-texts . "Markerade texter:%24#1d\n")
    (time-for-last-letter . "Tid för senaste brev:%20#1s (står det i din cache)\n")
    (superconf . "Supermöte:%31#1m %#3s(%#2M)\n")
    (supervisor . "Organisatör:%29#1p %#3s(%#2P)\n")
    (member-of-confs . "Medlem i (antal möten):%18#1d\n")
    (presentation . "Presentation:    %24#1n\n")
    (show-membership-list-also-q . "Vill du se vilka möten personen är medlem i också? ")
    (not-allowed-see-confs . "Du får inte se vilka möten %#1P är medlem i.\n")
    (is-member-of . "\n%#1P är medlem i följande möten:\n")
    (membership-list-header . "Senast inne         Osett  Namn\n\n")
    (pers-membership-line . "%#1s%#2s%#3M %#4s\n")
    (pers-membership-line-2 . "                          Adderad %#1s av %#2P\n")
    (is-supervisor-mark . "O ")
    (who-to-send-message-to . "Vem vill du skicka meddelandet till? (%s) ")
    (send-empty-message-p . "Meddelandet är tomt. Vill du ändå skicka det? ")
    (his-total-unread . "\n%#1M har totalt %#2d olästa.\n")
    (message-prompt . "Meddelande: ")
    (message-sent-to-user
     . "================================================================
Ditt meddelande till %#2M:

%#1t
----------------------------------------------------------------
")
    (message-sent-to-all
     . "================================================================
Ditt allmänna meddelande löd:

%#1t
----------------------------------------------------------------
")
    (message-use-alarm-instead . "Använd kommandot %#1s för att skicka alarmmeddelanden.\n")
    (message-all-info . "Sänd alarmmeddelande\n")
    (message-recipient-info . "Sänd meddelande till %#1M\n")
    (message-nope
     . "Du kunde inte skicka meddelandet. Mottagaren var kanske inte inloggad.
Meddelandet du försökte sända till %#1M var:
%#2t\n")
    (only-last . "Endast läsa senaste (0 - %#1d) i %#2s: ")
    (only-error . "Något gick galet. Sorry.\n")

    (you-have-unreads . "Du har %#1d olästa inlägg i %#2M\n")
    (you-have-an-unread . "Du har 1 oläst inlägg i %#1M\n")
    (you-have-unreads-special . "Du har %#1d okommenterade inlägg i %#2M\n")
    (you-have-an-unread-special . "Du har 1 okommenterat inlägg i %#1M\n")
    (you-have-read-everything . "Du har sett alla nyheter.\n")
    (total-unreads . "\nDu har %#1d olästa inlägg.\n")
    (total-unread . "\nDu har 1 oläst inlägg.\n")
    (waiting-for-anything .
			  "Du väntar på ett inlägg i vilket möte som helst.\n")
    (waiting-higher-than . 
			 "Du väntar på ett inlägg i ett möte med högre prioritet än %#1d.\n")

    (have-to-be-in-conf-with-unread . "Du måste gå till ett icketomt möte först.\n")
    (Texts . "Inlägg")
    (Date . "Datum")
    (Lines . "Rad.")
    (Author . "Författare")
    (Subject . "Ärende")
    (mark-type . "Typ")
    (could-not-read . "Du fick inte läsa denna text (%#1n).\n")
    (multiple-choice . "Flera alternativ finns.")
    (does-not-exist . "Detta kommando finns inte.")
    (summary-line . "%=-8#1n%#2s%4#3d  %[%#4@%#5:P%]  %[%#6@%#7r%]\n")

    (what-mark-to-list . "Vilken markering vill du lista? ")
    (you-have-marks . "Du har %#1d inlägg markerade markerade med %#2d.\n")
    (you-have-marks-all . "Du har %#1d markerade inlägg.\n")


    ;; Only people fixing bugs or recieving bugg-reports should
    ;; change these:
    (buggreport-compilestart . "Skapar buggrapporten...")
    (buggreport-compileend . "Skapar buggrapporten...klart")
    (buggreport-description . "Detta gjorde jag:
\(Fyll i dina kommentarer nedan\)\n================\n\n
================
Bland informationen nedan finns också en lista på de 100 sist tryckta
tangenterna i din emacs. Om du nyligen loggat in kan den innehålla ditt
lyskomlösenord. Titta igenom den och ändra det som är ditt lösenord
till * * * eller m i t t l ö s e n eller något annat lämpligt.

När du skrivit klart skall du skicka in din buggrapport till LysKOMs
elispklientutvecklare. Det sker antingen:
* med email till bug-lyskom@lysator.liu.se
* med vanligt brev till:
\tLysator
\tc/o ISY
\tLinköping University
\tS-581 83 Linkoping
\tSWEDEN.

Märk kuvertet \"LysKOM buggrapport för elispklienten\".\n\n")
    
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


    (not-logged-in . "Du är inte inloggad.  ")


    ;; +++ cant seem to find where these are used:
    (name-is-not-in-conf . "%#1s är inte närvarande i något möte.\n")
    (name-is-in-conf . "%#1s är närvarande i\n%#2s\n")
    (connected-during . "Uppkopplingstid: %#1d sekunder.\n")
    ;; +++


    (conf-to-set-permitted-submitters-q . "Vilket möte vill du sätta tillåtna författare för? ")
    (conf-to-set-super-conf-q . "Vilket möte vill du sätta supermöte för? ")
    (new-super-conf-q . "Vilket möte vill du ha som supermöte? ")
    (new-permitted-submitters-q . "Möte med tillåtna författare till %#1s? (alla) ")
    (super-conf-for-is . "Ändra supermöte för %#1M till %#2M...")
    (permitted-submitters-removed-for-conf . "Tillåt alla författare i möte %#1M...")
    (submitters-conf-for-is . "Ändra tillåtna författare för möte %#1M till\nmedlemmarna i %#2M...")

    (conf-to-set-garb-nice-q . "Vilket möte vill du sätta livslängd för? ")
    (new-garb-nice-q . "Vilket värde vill du sätta livslängden till? ")
    (garb-nice-for-is . "Ändra livslängden för %#1M till %#2d...")

    (really-shutdown . "Är du säker på att du vill stänga av servern? ")
    (closing-server . "Stänga av servern...")
    (really-sync . "Är du säker på att du vill spara databasen? ")
    (syncing-server . "Spara databasen...")
    (administrator . "administratör")
    (no-longer-administrator . "en normal användare igen")
    (you-are-now . "Ok, du kör nu som %#1s.\n")
    (setting-motd . "Sätter loginmeddelandet till text %#1n.\n")
    (set-motd-success . "Du har satt ett nytt loginmeddelande.\n")
    (set-motd-failed . "Det gick inte. Du var kanske inte administratör.\n")
    (removing-motd . "Tar bort loginmeddelandet.\n")
    (removed-motd . "Du har tagit bort loginmeddelandet.\n")
    (who-to-throw-out . "Vilken session vill du kasta ut? ")
    (throwing-out . "Kastar nu ut session %#1d... ")
    (postpone-prompt . "Hur lite vill du läsa nu? ")
    (set-session-priority . "Sätt läsnivå: ")

    ;; From review.el:
    (no-review-done . "Du måste återse innan du kan återse mer.\n")
    (review-how-many . "Återse hur många?")
    (review-how-many-more . "Återse ytterligare hur många?")
    (latest-n . "senaste %#1d")
    (first-n . "första %#1d")
    (info-by-whom . "%#1s av vem: ")
    (info-to-conf . "%#1s till möte: ")
    (all-confs . "alla möten")
    ;; +++ not used?
    (info-by-to . "%#1s av %#2P till %#3M framåt.")
    (no-get-conf . "Du får inte hämta mötet.\n")
    (no-get-pers . "Du får inte hämta personen.\n")
    (no-review-info . "Ej tillåtet återse %#1s\n")
    ;; +++
    (review-info . "Återse %#1s")
    (review-info-by-to . "Återse %#1s av %#2P till %#3M framåt.\n")
    (review-more-info-by-to . "Återse %#1s av %#2P till %#3M framåt.\n")
    (review-rest . "resten")
    (review-more . "nästa %#1d")
    (you-review . "Du återser nu %#1s.\n")
    (read-text-first . "Du måste läsa en text först.\n")
    (cannot-read-last-text . "Du kan inte läsa den senast lästa texten.\n")
    (review-n-texts . "Återse %#1d inlägg.\n")
    (review-marked . "Återse %#1d markerade.\n")
    (review-text-no . "Återse text nummer %#1n\n")
    (review-one-comment . "Återse en kommentar till inlägg %#1n.\n")
    (review-many-comments . "Återse %#2d kommentarer till inlägg %#1n.\n")
    (read-normally-read . "Hur många vill du se igen? ")

    (review-conf-gone . "Mötet finns inte.\n")
    (review-pers-gone . "Personen finns inte.\n")
    (review-cant-read-conf . "Du kan inte återse inlägg till ett slutet möte du inte är med i.\n")
    (review-cant-read-letterbox . "Du kan inte återse inlägg till någon annans brevlåda.\n")
    (review-cant-read-empty . "Mötet är tomt.\n")
    (cant-review-everything . "Du kan inte återse alla texter i LysKOM.\n")

    (more-than-one-root . "Inlägg %#1n har mer än ett urinlägg.\n")
    (more-than-one-root-review . 
"Inlägg %#1n har mer än ett urinlägg, men endast ett träd kommer att visas.\n")

    ;; From edit-text.el:
    (press-C-c-C-c . "Tryck C-c C-c för att skicka in texten.")
    (recipient . "Mottagare:")
    (recipient-prefix . "[Mm]")
    (carbon-copy . "Extra kopia:")
    (blank-carbon-copy . "För kännedom:")
    (carbon-copy-prefix . "[Ee]")
    (blank-carbon-copy-prefix . "[Ff]")

    (secret-aux-flag . "hemlig")
    (anonymous-aux-flag . "anonym")
    (inherit-aux-flag  . "ärvd")
    (aux-item-prefix . "> ")

    (header-subject . "Ärende: ")
    (header-separator . "\\<lyskom-edit-mode-map>\
--- Skriv nedan. \
Skicka in=\\[kom-edit-send], \
Avbryt=\\[kom-edit-quit], \
Annat se \\[describe-mode] ---")
    (text-mass . "%#4s%#1s\n%#2s\n%#3s")
    (comment-to-by . "%#1s till text %#2n%#3s.\n")
    (already-sent . "Du har redan skickat denna text en gång. Sänd ändå? ")
    (subject . "Ärende: ")
    (subject-prefix . "[Ää\\{\\[]")
    (enter-subject-idi . "Skriv ett ärende.")
    (which-text-include . "Vilken text skall vi inkludera? ")
    (added-recipient . "Mottagare som skall adderas: ")
    (added-carbon-copy . "Extra kopia till möte: ")
    (added-blank-carbon-copy . "För kännedom till: ")
    (text-to-comment-q . "Vilket inlägg vill du kommentera? ")
    (conf-has-motd-no . "Mötet har en lapp på dörren. (%#1d)\n\n%#2s")
    (still-want-to-add . "Vill du fortfarande addera mötet? ")
    (could-not-create-text . "\nTexten kunde ej skapas. Felet: %#2s.\n")
    (no-get-text . "Du fick inte hämta texten.")
    (unknown-header . "Okänd information på raden")
    (transform-error . "Skicka in oformatterat (%#1s)? ")

    ;; From view-text.el:
    (line . " /1 rad/ ")
    (lines ." /%#1d rader/ ")

    (marked-by-you . "Markerad av dig.\n")
    (marked-by-you-and-one . "Markerad av dig och någon annan.\n")
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
    (yesterday . "igår")
    (no-such-text-no . "Det finns inget sådant inlägg. (%#1:n)\n")

    (head-Subject . "Ärende: ")
    (Recipient . "Mottagare")
    (Extra-recipient . "Extra kopia")
    (Hidden-recipient . "För kännedom")
    (Strange-recipient . "Underlig mottagare")
    (send-at . "    Sänt:     %#1s\n")
    (sent-by . "    Sänt av %#1P\n")
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
Meddelande från LysKOM-systemet: Någon försökte koppla upp,
men misslyckades eftersom alla tillgängliga förbindelser är
upptagna. Logga ut och kom tillbaks senare om du väntar nu.
===========================================================\n")
    (has-entered . "Nu har %#1:P gått in i %#2s.")
    (has-entered-r . "%#2@Nu har %#1P gått in i %#3s.\n")
    (has-left . "Nu har %#1:P gått ur %#2s.")
    (has-left-r . "%#2@Nu har %#1P gått ur %#3s.\n")
    (unknown . "okänd")
    (secret-person . "Hemlig person")

    (no-longer-member . "Du är inte längre medlem i %#1M\n")
    (no-longer-member-n . "Du är inte längre medlem i möte %#1m (finns inte.)\n")

    (have-become-member . "Du har blivit medlem i %#1M\n")

    (message-broadcast . 
		       "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Allmänt meddelande från %#1P (%#3s):

%#2t
----------------------------------------------------------------
")
    (message-from . 
		  "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Personligt meddelande från %#1P (%#3s):

%#2t
----------------------------------------------------------------
")
    (message-from-to .
		     "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Gruppmeddelande till %#3M\nfrån %#2P (%#4s):

%#1t
----------------------------------------------------------------
")
    (text-is-created . "Text %#1n är skapad!")

    ;; Used in mode-line-process
    (mode-line-waiting . ": väntar")
    (mode-line-working . ": arbetar")
    (mode-line-saving . ": sparar")
    (mode-line-down . ": nerkopplad")

    ;; From completing-read.el:
	   
    (person-or-conf-no-regexp . "\\`[ \t]*[mpMP]\\w*[ \t]+\\([0-9]+\\)\\'")
    (session-no-regexp . "\\`[ \t]*[sS]\\w*[ \t]+\\([0-9]+\\)\\'")

    ;;n From prioritize.el:

    (cant-move-nothing-nowhere . "Kan inte flytta ingenting någonstans.")
    (goto-priority-prompt . "Hoppa till prioritet: ")
    (priority-prompt . "Ny prioritet för %#1M: ")
    (priority-prompt-marked . "Ny prioritet på markerade möten: ")
    (beginning-of-list . "Början av listan")
    (end-of-list . "Slutet av listan")
    (reprioritize-from . "Prioritera om från: ")
    (reprioritize-to . "Prioritera om till: ")
    (no-selection . "Ingen markerad")
    (selection . "%d markerade")

    (cannot-get-membership . "Kan ej hämta medlemsskap för dig.")
    (cannot-get-pers-stat . "Kan ej hämta personstatus för dig.")
    (prioritize-help .
		     "u,n Flytta möte, SPC markera, p prioritera markerade, q avsluta, C-h m hjälp")
    (your-priorities . " Prioritet  Mötesnamn
----------------------------------------------------------------------------
")
    (your-membship . "Ditt medlemsskap i olika KOM-möten:
  Prio Mötesnr Mötesnamn\n")
    (prio-row . " %5#1d%5#2m  %#3M\n")
    (too-high-goto-2 . "Du står för högt upp. Gå ner till rad 2.")
    (too-low-go-up . "Du kan inte pusha sista raden. Gå upp en rad.")
    (all-confs-popped .  "Alla möten är poppade.")
    (prio-died . "Flyttningen misslyckades. Sorry. Döda bufferten.")
    (new-priority . "Ny prioritet? (0 (låg) - 255 (hög)) ")
    (new-prio . "%6#1d")

    ;; From flags.el:
    (saving-settings . "Sparar inställningarna...")
    (saving-settings-done . "Sparar inställningarna...klart")
    (hang-on . "Vänta ett tag...\n")
    (no-changes . "Ingenting behövde sparas eftersom inga variabler hade ändrats.\n")
    (could-not-save-options . "Kunde ej spara inställningarna.\n")
    (could-not-create-area . "Kunde ej skapa texten.\n")
    (could-not-set-user-area . "Kunde ej ställa om user-arean. Servern säger felmeddelande: %#1d\n")
    (you-dont-exist . "Du finns inte.\n")
    (error-in-options . "Det fanns ett fel i en av dina variabler (%#1s)
Det stod \"%#2s\" i user-arean. Den sätts till nil istället.
Skicka en bugrapport.\n")

    ;; From elib-string.el:
    ;; No entries.
		
    ;; From lyskom-rest.el:
		
    (mode-line-unread . " Olästa ")
    (mode-line-letters . "brev ")

    (error-code . "Felkod %#2d: %#1s.\n")
    (error-in-kom-do-when-done . "Variabeln kom-do-when-done har ett felaktigt vä.
Du bör sätta den till ett bättre värde.\n")
    (extended-command . "LysKOM: ")
    (wait-for-server . "LysKOM väntar på svar från servern. Vänta tills du får en prompt.\n")
    (review-text-q . "Återse text nummer: ")

    (completely-read-conf . "Du har sett alla texter i detta möte.\n")
    (not-in-any-conf . "Du läser inte något möte just nu.\n")

    (all-conf-unread-r . "Du har läst ut alla möten.\n")
    (all-conf-unread-s . "Du har läst ut alla möten. ")
    (one-unread . "%#1M - 1 oläst\n")
    (several-unread . "%#1M - %#2d olästa\n")
    (enter-conf . "%#1M\n")

    (save-on-file-q . "Spara inlägg på fil: (%#1s) ")
    (wait-for-prompt . "Vänta på prompten.")

    (conference-no . "<möte %#1d>")
    (person-no . "<person %#1d>")
    (prompt-several-messages . "(%d meddelanden)")
    (prompt-single-message   . "(%d meddelande)")

    (go-to-pri-conf-prompt . "Gå till nästa prioriterade möte")
    (read-pri-text-conf . "Läsa nästa prioriterade text")
    (review-next-text-prompt . "Återse nästa text")
    (review-next-comment-prompt . "Återse nästa kommentar")
    (review-next-marked-prompt . "Återse nästa markerade")
    (read-next-letter-prompt . "Läsa nästa brev")
    (read-next-footnote-prompt . "Läsa nästa fotnot")
    (read-next-comment-prompt . "Läsa nästa kommentar")
    (read-next-text-prompt . "Läsa nästa text")
    (go-to-conf-of-marked-prompt . "Återse (nästa) markerade")
    (go-to-next-conf-prompt . "Gå till nästa möte")
    (go-to-your-mailbox-prompt . "Gå till din brevlåda")
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
    (conference-does-not-exist . "Möte %#1d (finns inte).")
    (person-is-anonymous . "Anonym person")

    (process-signal . "Signal från processen.")
    (closed-connection . "
**************************************************
%#2s
Lyskom-sessionen onormalt stängd.
Felmeddelande: %#1s**************************************************")
    (dead-session . "LysKOM-sessionen är inte aktiv.")
    (error-not-found . "Fel nummer %#1d. Ingen klartextförklaring finns.")

    ;; Useful in more place than one:
    (illegal-command . "Otillåtet kommando.\n")
    (no-such-text . "Det finns inget sådant inlägg.\n")
    (no-such-text-m . "Det finns inget sådant inlägg.")
    (nobody . "ingen")
    (everybody . "alla")
    (everything . "allt")
    (anybody . "vem som helst")
    (forward . "framåt")
    (backward . "bakåt")
    (wait . "Vänta ett tag...\n")
    (comment . "Kommentar")
    (comment-prefix . "[Kk]")
    (footnote . "Fotnot")
    (footnote-prefix . "[Ff]")

    (by . " av %#1P")
    (text-created .  "Text nummer %#1n är skapad.\n")

    (resolve-session . "Ange vilken session: ")

    (starting-program . "Startar %#1s...")
    (super-jump . "Filtrerar ärende \"%#1r\" i möte \"%#2M\"\n")
    (no-recipient . "Inlägget har ingen mottagare.\n")
    (filtered . "[Filtrerad]")
    (filter-error-specification . "Fel i filterspecifikationen")
    (filter-error-bad-not . "Fel i filterspecifikation efter 'not'")
    (filter-error-unknown-key . "Filternyckeln '%S' är okänd.")
    (filter-error-key-arg . "Fel filterdata (%S %S)")
    (filter-tree . "Hoppar över text %#1n \"%#2r\" av %#3P och dess kommentarsträd.\n")
    (filter-text . "Hoppar över text %#1n \"%#2r\" av %#3P.\n")
    (filter-permanent . "Permanent? ")
    (filter-action . "Hur vill du filtrera? ")
    (filter-in-conf . "I vilket möte? (alla) ")
    (filter-subject . "Filtrera vilket ärende? ")
    (filter-which-text . "Filtrera inlägg som innehåller: ")
    (filter-author . "Filtrera vilken författare? ")
    (permanent . "(permanent)")
    (temporary . "(tillfällig)")
    (filter-edit-buffer-name . "*LysKOM Filter Edit*")
    (filter-edit-empty-list . "Listan är tom")
    (filter-edit-start-of-list . "Listans början")
    (filter-edit-end-of-list . "Listans slut")
    (filter-edit-filter-how . "Hur vill du filtrera? ")
    (filter-edit-filter-what . "Vad vill du filtrera? ")
    (filter-edit-bad-argument . "Felaktig inmatning: %s")
    (filter-edit-outside-entry . "Kan inte utföra kommandot utanför ett filter")
    (filter-edit-outside-list . "Kan inte utföra operationen utanför listan")
    (filter-edit-end-of-pattern . "Filtrets slut")
    (filter-edit-save-p . "Spara förändringar? ")
    (filter-edit-remove-empty . "Tomma filter gör att alla texter filtreras. Vill du ta bort dessa? ")
    (filter-edit-restart-p . "Du har gjort ändringar. Vill du verkligen börja om? ")
    (filter-edit-help . 
		      "p,n Upp/ned, i/M-i Ny rad/filter, d/M-d Radera rad/filter, C-h m Mer hjälp")
    (filter-edit-header . "Ändra filter för \"%s\"\n")
    (filter-edit-saving . "Sparar ändringarna...")
    (filter-edit-saving-done . "Sparar ändringarna...klart")
    (filter-edit-saving-error . "Kunde inte spara ändringarna!")
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
    (ansaphone-state . "Automatsvar är nu %#1s.")
    (ansaphone-state-r . "Automatsvar är nu %#1s.\n")
    (ansaphone-messages-gone . "Sparade meddelanden raderade.")
    (ansaphone-no-messages . "Inga meddelanden.\n")
    (ansaphone-message-list-start . "Sparade meddelanden:\n\n")
    (ansaphone-message-list-end . "\n\n")
    (ansaphone-message-header . "Automatiskt svar (satt %#1s):\n")

    (remote-erase-messages . "Fjärrstyrning (%#1P %#2s): Sparade meddelanden raderade\n")
    (remote-set-message . "Fjärrstyrning (%#1P %#2s): Svarsmeddelande:
----------------------------------------------------------------------
%#3t
----------------------------------------------------------------------
")
    (remote-set-ansaphone . "Fjärrstyrning (%#1P %#2s): Automatsvar är nu %#3s\n")
    (remote-list-messages . "Fjärrstyrning (%#1P %#2s): Meddelanden listade\n")
    (remote-quit . "Fjärrstyrning(%#1P %#2s): Avsluta\n")

    (illegal-remote . 
		    "Otillåten fjärrstyrning:
Tid: %#1s
Från: %#2P <%#2p>
Till: %#3P <%#3p>
Text: 
%#4t")
    (illegal-remote-reply . "Fjärrstyrning inte accepterad: %#1s") 
    (remote-not-in-list . "Otillåten person")
    (remote-bad-command . "Felaktigt kommando")
    (remote-unknown-error . "Okänt fel")

    (remote-control-who . "Kontrollera vilken session? ")
    (remote-control-autoreply . "Automatsvar på eller av? ")

    (state-on . "påslaget")
    (state-off . "avslaget")

    (text-popup-title . "Inlägg %#1s")
    (conf-popup-title . "Möte %#1s")
    (pers-popup-title . "Person %#1s")
    (url-popup-title  . "URL %#1s")
    (aux-popup-title  . "Tilläggsinformation")
    (generic-popup-title . "%#1s")

    (who-i-am-not-present . "%#1P är inte närvarande i något möte\n")
    (who-i-am-present . "%#1P är närvarande i %#2M\n")
    (who-i-am-client . "Programmet heter lyskom.el, version %#1s.\n")
    (who-i-am-server . "Detta är %#1s, version %#2s.\n")
    (who-i-am-emacs . "Det körs under %#1s.\n")

    (no-such-session-r . "Det finns ingen sådan session. Personen kanske inte är inloggad.\n")
    (person-not-logged-in-r . "%#1P är inte inloggad.\n")
    (session-status . "Session %#1d är %#2P <%#2p>
%#5s %#7s %#4M
Kör %#6D från %#3s\n")
    (session-status-9 . "Session %#1d är %#2P <%#2p>
%#5s %#7s %#4M
Kör %#6D från %#3s
Uppkopplad sedan %#8s%#9s")
    (session-status-inactive . "\nHar inte varit aktiv på %#1s\n")
    (one-day . "en dag")
    (one-hour . "en timme")
    (one-minute . "en minut")
    (days . "dagar")
    (hours . "timmar")
    (minutes . "minuter")
    (and . "och")
    (session-is-active . " och är aktiv.\n")
    (session-is-invisible . "Denna session är osynlig.\n")
    (status-for-session . "Sessionsstatus för vilken person? ")
    (unknown-doing-what . "Existerar")
    (doing-where-conn . "i")
    (doing-nowhere-conn . "men är")
    (waiting-for-membership . "Väntar på att medlemskapslistan ska läsas in...%d/%d")

    (keyboard-menu-help . "Nästa: SPC  Föreg: DEL  Utför: RET  Avbryt: ESC")
    
	   ;; From slow.el
    (no-such-command . "Det finns inget sådant kommando.\n")
    (command-completions . "Du kan mena någon av följande:\n %#1s\n")
    (which-language . "Ändra språk till: ")
    (send-formatted . "Skicka in som formatterad text? ")
    (changing-language-to . "Byter till %#1s.\n")
    (language-set-to . "Språket är %#1s.\n")
    (language-not-loaded . "%#1s finns inte tillgängligt.\n")

;;    (reformat-generic . "(%#1s)")
    (reformat-html . "HTML")
    (reformat-enriched . "enriched")
    (reformat-filled . "ombruten")

    (reformat-truncated . "nedkortad")
    
    (need-library . "Paketet \"%#1s\" behövs för att utföra detta kommando.\n")
    (calc-expression . "Uttryck: ")

    (do-send-anonymous . "Skicka in texten anonymt? ")
    (anonymous . "anonym")
    (Anonymous . "Anonym")

    ;; Some Help

    (where-is-doesnt-exist . "Kommandot %#1s finns inte")
    (where-is-on-no-key . "%#1s finns inte på någon tangent")
    (where-is-on-key . "%#1s finns på %#2s")

    ;; From aux-items.el

    (content-type-aux . "Innehållstyp: %#1s")
    (content-type-regexp . "Innehållstyp: \\(\\S-+\\)")
    
    (fast-reply-aux . "\"%#1t\" /%#2P/")
    
    (cross-reference-text-aux . "Se även inlägg %#1n /%#2P/")
    (cross-reference-conf-aux . "Se även möte <%#1m> %#1M /%#2P/")
    (cross-reference-pers-aux . "Se även person <%#1p> %#1P /%#2P/")
    (cross-reference-text-regexp . "Se även inlägg \\([0-9]+\\)")
    (cross-reference-conf-regexp . "Se även möte <\\([0-9]+\\)>")
    (cross-reference-pers-regexp . "Se även person <\\([0-9]+\\)>")
    (strange-cross-reference . "Se även %#1s, vad nu det betyder")
    
    (no-comments-aux . "Författaren har begärt att inte få kommentarer")
    (no-comments-edit-aux . "Begär att inte få kommentarer")
    (no-comments-regexp . "Begär att inte få kommentarer")
    
    (personal-comment-aux . "Författaren har begärt endast personliga svar")
    (personal-comment-edit-aux . "Begär endast personliga svar")
    (personal-comment-regexp . "Begär endast personliga svar")
    
    (request-confirmation-aux . "Författaren har begärt läsbekräftelse")
    (request-confirmation-edit-aux . "Begär bekräftelse om läsning")
    (request-confirmation-regexp . "Begär bekräftelse om läsning")
    (confirm-read-q . "Bekräfta läsning av inlägg %#1n? ")
    
    (read-confirm-aux . "Läst: %#1P %#2s")

    (redirect-email-aux . "Skicka sladdpost till %#1s")
    (redirect-lyskom-aux . "Skicka inlägg till möte %#1M")

    (label-what-kind . "Sätt personligt namn på vad (inlägg, möte, person)? ")
    (label-what-text . "Sätt personligt namn på inlägg nummer: ")
    (label-what-conf . "Sätt personligt namn på vilket möte? ")
    (label-what-pers . "Sätt personligt namn på vilken person? ")
    (label-what-label . "Personligt namn: ")
    (label-secret    . "Skall andra kunna se namnet? ")

    (cant-get-aux-item . "Hittar inte tilläggsinformationen")
    (aux-item-no-info . "Ingen information tillgänglig\n")
    (aux-item-info . "\
Nummer:       %#1d %#6s
Typ:          %#2d (%#3s)
Skapad av:    %#4P <%#4p>
Skapad:       %#5s
Flaggor:      %#7s
Arvsgräns:    %#8s
Innehåll:    \"%#9s\"
")
    (secret . "Hemlig")
    (deleted . "Raderad")
    (hide-creator . "Dold skapare")
    (inherit . "Ärvs")
    (unlimited-inherit . "Obegränsat arv")
    (no-inheritance . "Arv är avslaget")
    (no-more-inheritance . "Gränsen uppnådd; inget mer arv")
    (inherit-steps . "%#1d steg")

    (aux-item-for . "Tilläggsinformation för ")
    (conference-no . "möte <%#1m> %#1M")
    (text-no . "inlägg %#1n")

    (what-fast-reply-no . "Snabb replik till vilket inlägg? ")
    (fast-reply-prompt . "Replik: ")
    (agree-prompt . "Text: ")
    (default-agree-string . "Håller med")
    (what-agree-no . "Håll med vilket inlägg? ")

    ;; Aux-item stuff from edit-text

    (person . "Person")
    (conference . "Möte")
    (text . "Inlägg")

    (xref-type . "Vad vill du referera till (inlägg, möte eller person)? ")
    (which-text-to-xref . "Lägg till referens till text nummer: ")
    (which-text-to-xref-err . "Hittar inte texten. Lägg till referens till text nummer: ")
    (which-pers-to-xref . "Lägg till referens till person: ")
    (which-conf-to-xref . "Lägg till referens till möte: ")

    (no-comments-q . "Författaren vill inte ha kommentarer. Kommentera ändå? ")
    (private-answer-q . "Författaren har bett om personliga svar. Skriv personligt svar? ")

    (your-invited . "Du har bjudits in till %#1M av %#2P.\n")
    (accept-invitation . "Vill du acceptera inbjudan nu? ")
    (enter-passive . "Du är passiv medlem i %#1M.\n")

    (invitation-mt-type . "inbjuden")
    (passive-mt-type . "passiv")
    (secret-mt-type . "hemlig")
))




;;; ================================================================
;;;              The commands and their associated functions



;;; The Alist formely known as lyskom-commands
(lyskom-language-strings lyskom-command sv
  '(
    (describe-mode            . "Hjälp")
    (kom-slow-mode            . "Långsamma kommandon")
    (kom-quick-mode           . "Snabba kommandon")
    (kom-send-message         . "Sända meddelande")
    (kom-send-alarm           . "Sända alarmmeddelande")
    (kom-create-conf	      . "Skapa möte")
    (kom-delete-conf          . "Utplåna")
    (kom-delete-text          . "Radera inlägg")
    (kom-display-time	      . "Se tiden")
    (kom-go-to-conf	      . "Gå till möte")
    (kom-go-to-next-conf      . "Gå till nästa möte")
    (kom-jump		      . "Hoppa över alla kommentarer")
    (kom-list-conferences     . "Lista möten") 
    (kom-list-persons	      . "Lista personer")
    (kom-list-news            . "Lista nyheter")
    (kom-list-re              . "Lista (med) regexpar")
    (kom-membership	      . "Lista medlemsskap")
    ;; (kom-list-marks		"Lista markeringar") 
    (kom-postpone	      . "Uppskjuta läsning")
    (kom-set-session-priority . "Sätt läsnivå")
    (kom-prioritize	      . "Prioritera möten")
    (kom-status-person	      . "Status (för) person")
    (kom-status-conf	      . "Status (för) möte")
    (kom-add-self	      . "Bli medlem i möte")
    (kom-list-summary	      . "Lista ärenden")
    (kom-sub-self             . "Utträda ur möte")
    (kom-quit		      . "Sluta")
    (kom-recover	      . "Återstarta kom") 
    (kom-start-anew	      . "Börja med nytt namn")
    (kom-view		      . "Återse inlägg")
    (kom-find-root-review     . "Återse träd")
    (kom-review-comments      . "Återse alla kommentarer")
    (kom-review-tree	      . "Återse alla kommentarer rekursivt")
    (kom-review-clear         . "Återse hoppa")
    (kom-review-last-normally-read
     			      . "Återse igen")
    (kom-review-noconversion  . "Återse omodifierat")
    (kom-review-next          . "Återse nästa")
    (kom-find-root	      . "Återse urinlägget")
    (kom-review-by-to         . "Återse senaste")
    (kom-review-more          . "Återse fler inlägg")
    (kom-review-first         . "Återse första")
    (kom-review-all           . "Återse alla")
    (kom-view-commented-text  . "Återse det kommenterade")
    (kom-view-previous-commented-text
     			      . "Återse det föregående kommenterade")
    (kom-review-stack         . "Återse lista")
    (kom-review-presentation  . "Återse presentation")
    (kom-review-backward      . "(Återse) Baklänges")
    (kom-view-next-text	      . "Läsa nästa inlägg")
    (kom-who-is-on	      . "Vilka är inloggade") 
    (kom-who-is-on-in-conference
     			      . "Vilka är inloggade i möte") 
    (kom-who-am-i             . "Var (är) jag")
;    (kom-display-who-buffer	"Visa vilkalistan")
    (kom-list-clients	      . "Lista klienter")
    (kom-busy-wait	      . "Vänta på ett inlägg")
    (kom-write-comment	      . "Kommentera inlägget")
    (kom-comment-previous     . "Kommentera föregående inlägg")
    (kom-write-footnote       . "Fotnot till inlägg")
    (kom-private-answer       . "Personligt svar")
    (kom-private-answer-previous
     . "Personligt svar på föregående inlägg")
    (kom-set-unread	      . "Endast läsa senaste")
    (kom-write-text	      . "Skriva ett inlägg")
    (kom-send-letter	      . "Skicka brev")
    (kom-change-name	      . "Ändra namn")
    (kom-change-parenthesis   . "Ändra parentes")
    (kom-change-password      . "Ändra lösenord")
    (kom-change-supervisor    . "Ändra organisatör")
    (kom-change-presentation  . "Ändra presentation")
    (kom-get-appreciation     . "Få uppmuntran")
    (kom-get-abuse            . "Få skäll")
    (kom-mark-text            . "Markera (inlägg)")
    (kom-unmark-text          . "Avmarkera (inlägg)")
    (kom-review-marked-texts  . "Återse markerade")
    (kom-review-all-marked-texts . "Återse alla markerade")
    (kom-add-recipient        . "Addera mottagare")
    (kom-add-copy	      . "Addera extra kopiemottagare")
    (kom-add-bcc	      . "Addera för kännedom")
    (kom-sub-recipient        . "Subtrahera mottagare")
    (kom-move-text            . "Flytta inlägg")
    (kom-add-comment	      . "Addera kommentar")
    (kom-sub-comment	      . "Subtrahera kommentar")
    (kom-add-member	      . "Addera medlem")
    (kom-sub-member           . "Uteslut medlem")
    (kom-change-conf-motd     . "Sätt lapp på dörren")
    (kom-set-garb-nice        . "Ändra livslängd")
    (kom-set-super-conf       . "Ändra supermöte")
    (kom-set-permitted-submitters . "Ändra tillåtna författare")
    (kom-unset-conf-motd      . "Ta bort lapp på dörren")
    (kom-save-text	      . "Spara text (på fil)")
    (kom-edit-options	      . "Ändra variabler")
    (kom-save-options         . "Spara variabler")
    (kom-shutdown-server      . "Stäng av servern")
    (kom-sync-database        . "Spara databasen")
    (kom-enable-adm-caps      . "Övergå till administratörsmod")
    (kom-disable-adm-caps     . "Övergå till normalmod")
    (kom-set-motd             . "Sätt loginmeddelande")
    (kom-remove-motd          . "Ta bort loginmeddelande")
    (kom-force-logout         . "Kasta ut en session")
    (kom-filter-author        . "Filtrera författare")
    (kom-filter-subject       . "Filtrera ärende")
    (kom-filter-text          . "Filtrera innehåll")
    (kom-super-jump           . "Superhoppa")
    (kom-filter-edit          . "Ändra filter")
    (kom-list-filters         . "Lista filter")
    (kom-show-user-area       . "Visa user-arean")
    (kom-change-conf-type     . "Ändra mötestyp")

    (kom-change-auto-reply    . "Ändra svarsmeddelande")
    (kom-toggle-auto-reply    . "Automatsvar")
    (kom-list-messages        . "Lista meddelanden")
    (kom-erase-messages       . "Radera meddelanden")

    (kom-remote-autoreply     . "Fjärrkontrollera automatsvar")
    (kom-remote-set-message   . "Fjärrkontrollera ändra svarsmeddelande")
    (kom-remote-list-messages . "Fjärrkontrollera lista meddelanden")
    (kom-remote-erase-messages . "Fjärrkontrollera radera meddelanden")
    (kom-remote-quit          . "Fjärrkontrollera avsluta")

    (kom-status-session       . "Status (för) session")
    (kom-customize            . "Inställningar (för) LysKOM")
    (kom-next-kom             . "Nästa LysKOM")
    (kom-previous-kom         . "Föregående LysKOM")
    (kom-next-unread-kom      . "Nästa olästa LysKOM")
    (kom-change-language      . "Ändra språk")
    (kom-calculate            . "Beräkna")
    (kom-list-marks           . "Lista markeringar")

    (kom-where-is             . "Var finns kommandot")

    (kom-fast-reply           . "Snabb replik")
    (kom-agree                . "Hålla med")
    
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
    (fo . "Faröiska")
    (fr . "Franska")
    (fy . "Frisiska")
    (ga . "Irländska")
    (gd . "Skotsk Gäliska")
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
    (is . "Isländska")
    (it . "Italienska")
    (iu . "Inuktitut")
    (ja . "Japanska")
    (jw . "Javanesiska")
    (ka . "Georgiska")
    (kk . "Kazakhstanska")
    (kl . "Grönländska")
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
    (nl . "Holländska")
    (no . "Norska")
    (oc . "Occitanska")
    (om . "Oromo")
    (or . "Oriya")
    (pa . "Pundjabiska")
    (pl . "Polska")
    (ps . "Pashtu")
    (pt . "Protugisiska")
    (qu . "Quechua")
    (rm . "Rhätoromanska")
    (rn . "Kirundiska")
    (ro . "Rumänska")
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
    (th . "Thailändska")
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
    (vo . "Volapük")
    (wo . "Wolof")
    (xh . "Xhosa")
    (yi . "Yiddish")
    (yo . "Yorouba")
    (za . "Zhuang")
    (zh . "Kinesiska")
    (zu . "Zulu")
    (-- . "Okänt språk (%#1s)")))


(lyskom-language-strings lyskom-menu sv
  '((lyskom    . "LysKOM")
    (read      . "Läs")
    (dont-read . "Hoppa")
    (write     . "Skriv")
    (conference . "Möte")
    (person    . "Person")
    (other     . "Annat")
    (marks     . "Markeringar")
    (move      . "Gå")
    (info      . "Om")
    (send      . "Sänd")
    (recievers . "Mottagare")
    (commented . "Kommenterar")
    (kom-edit-send . "Skicka in")
    (kom-edit-send-anonymous . "Skicka anonymt")
    (kom-edit-quit . "Kasta bort") 
    (kom-ispell-message . "Stavningskontroll")
    (kom-edit-add-recipient . "Addera mottagare")
    (kom-edit-add-copy . "Addera extra kopiemottagare")
    (kom-edit-show-commented . "Återse det kommenterade")
    (kom-edit-insert-commented . "Citera det kommenterade")
    (kom-edit-add-bcc . "Addera för kännedom")
    (kom-edit-add-cross-reference . "Addera korsreferens")
    (kom-edit-add-no-comments . "Begär inga kommentarer")
    (kom-edit-add-personal-comments . "Begär personliga svar")
    (kom-edit-add-read-confirm-request . "Begär läsbekräftelse")
    (kom-edit-move-text . "Flytta till en ny mottagare")
))

;;(defvar lyskom-swascii-commands nil
;;  "The swascii-versions of lyskom-commands.")

(lyskom-language-var lyskom-onoff-table sv
  '(("på" . on) ("av" . off)))

(lyskom-language-var lyskom-filter-predicate-list sv
      '(("=" . nil) ("!=" . t)))

(lyskom-language-var lyskom-filter-what sv
      '((author . "Författare")
        (author-no . "Författare (nummer)")
        (author-re . "Författare (regexp)")
        (subject . "Ärende")
        (subject-re . "Ärende (regexp)")
        (recipient . "Mottagare")
        (recipient-no . "Mottagare (nummer)")
        (recipient-re . "Mottagare (regexp)")
        (text . "Innehåll")
        (text . "Innehåll (regexp)")))

(lyskom-language-var lyskom-filter-actions sv
      '((skip-text . "Hoppa över")
        (dontshow . "Visa inte")
        (skip-tree . "Hoppa över kommentarer")))
                               
;;(defvar lyskom-swascii-filter-actions nil
;;  "The swascii-versions of lyskom-filter-actions.")
;;(defvar lyskom-swascii-filter-what nil
;;  "The swascii version of lyskom-filter-what")

(lyskom-language-var lyskom-text-start sv
  "[0-9]+ +\\(199[0-9]-[0-1][0-9]-[0-3][0-9]\\|idag\\|igår\\) +[0-2][0-9]:[0-5][0-9] +/[0-9]+ rad\\(er\\)?/ ")


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
  (define-key lyskom-sv-mode-map (kbd "ä") 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map (kbd "Ä") 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map (kbd "Å") 'lyskom-sv-review-prefix)
  (define-key lyskom-sv-mode-map (kbd "å") 'lyskom-sv-review-prefix)

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
  (define-key lyskom-sv-mode-map (kbd "l ä") 'kom-list-summary)
  (define-key lyskom-sv-mode-map (kbd "l Ä") 'kom-list-summary)
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
  (define-key lyskom-sv-mode-map (kbd "f ä") 'kom-filter-subject)
  (define-key lyskom-sv-mode-map (kbd "f Ä") 'kom-filter-subject)
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
  (define-key lyskom-sv-mode-map (kbd "s ä") 'kom-send-message)
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
" Prio   Möte
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
    (which-conf . "Ange ett möte: ")
    (which-conf-or-person . "Ange en person eller ett möte: ")
    (which-name . "Ange ett namn: ")
    (some-person . "Person %#1d")
    (invalid-value . "Otillåtet värde (%#1S)")
    (unknown-command . "Okänt kommando (%#1s)")

    ;;
    ;; Help messages
    ;;

    (variable-type-help . "Kryssa i för att spara inställningen i servern")
    (default-help-echo . "Ändra värdet på %#1s.")
    (change-this-name . "Ändra namnet på denna rad.")
    (show-doc . "Visa hjälptexten.")
    (hide-doc . "Dölj hjälptexten.")
    (select-command . "Välj kommando.")
    (select-what-to-execute . "Exekvera kommando eller tangentbordsmakro.")
    (select-url-viewer . "Välj en WWW-läsare.")
    (select-number . "Ange hur många gånger.")
    (select-audio-file . "Ange en ljudfil.")
    (select-priority . "Ange prioritet.")
    (select-buffer . "Ange buffert.")
    (select-buffer-size . "Ange buffertstorlek.")

    ;;
    ;; Strings that are used in types and so forth
    ;;

    (buffer-name . "%#1s-inställningar")

    (other-window     . "Något annat fönster            ")
    (other-frame      . "I en annan frame               ")
    (new-frame        . "I en ny frame                  ")
    (lyskom-window    . "LysKOM-buffertens fönster      ")
    (window-on-buffer . "Ett fönster som visar bufferten")

    (on . "På")
    (off . "Av")
    (yes . "Ja ")
    (no . "Nej")
    (max-text-length . "För inlägg kortare än: ")
    
    (friends . "Vänner och bekanta")

    (turned-off      . "Avslaget            ")
    (number-of-times . "Några gånger")
    (sound-file . "Ljudfil")
    (selected-mark . "Markering")
    (ask . "Fråga varje gång")
    (before . "Före texten")
    (after . "Efter texten")
    (depth-first . "I kommentarsordning")
    (time-order . "I tidsordning")

    (express-break . "Omedelbart efter de har skapats")
    (break         . "Efter aktuell kommentarskedja  ")
    (no-break      . "Efter aktuellt möte            ")

    (command . "Kommando")
    (command-list . "Kommandolista")
    (some-persons . "För vissa personer")
    (name . "Namn")

    (page-none . "Aldrig                 ")
    (page-all  . "Före varje kommando    ")
    (page-some . "Före följande kommandon")

    (ask-every-time . "Fråga varje gång      ")
    (fixed-priority . "Fast prioritet")

    (messages-in-lyskom-buffer . "I LysKOM-bufferten ")
    (discard-messages .          "Ingenstans         ")
    (in-named-buffer .           "I namngiven buffert")

    (everybody-rcpt . "Alla")
    (group-rcpt .     "Senaste gruppmeddelandes mottagare")
    (sender-rcpt .    "Senaste meddelandes avsändare")

    (viewer-program . "WWW-läsare")
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

    (dont-check . "Ingen bekräftelse")
    (check-before-open . "Bekräfta innan inlägget skrivs")
    (check-before-send . "Bekräfta innan inlägget sänds")
    (no-size-limit . "Ingen begränsning")
    (max-size-in-bytes . "Begränsning (i bytes)")
    (execute . "Utför")
    (kbd-macro . "Tangentbordmakro")
    (command . "Kommando")
    (enter-kbd-macro . "Mata in tangentbordsmakro. Avsluta med %#1s")

    (long-format  . "Visa hjälptexter")
    (short-format . "Göm hjälptexter ")

    (truncate-threshold-lines . "Max rader")
    
    ;;
    ;; Misc doc strings
    ;;

    (lyskom . "Inställningar för LysKOM")
    (lyskom-doc . "\
\\[lyskom-customize-save-and-quit] för att spara och avsluta,
\\[lyskom-customize-save] för att spara utan att avsluta
\\[lyskom-customize-quit] för att avsluta utan att spara
\\[widget-forward] flyttar till nästa inställning
\\[widget-button-press] ändrar värdet

Hälptexter:  [?] Visa hjälptext    [!] Göm hjälptext
Listor mm.:  [INS] Lägg till rad   [DEL] Ta bort rad    [*] Ändra värde

Om rutan före inställningens namn är ikryssad kommer värdet att sparas
i servern. Annars sparas det i din .emacs.")

    (section . "------------------------------------------------------------------------------\n")
    (look-and-feel-misc . "Diverse utseende och beteende\n")
    (window-locations   . "Fönster\n")
    (windows-where      . "Hur skall fönster skapas:\n")
    (reading .            "Beteende vid läsning\n")
    (writing .            "Beteende vid skrivning\n")
    (urls .               "Hantering av URLer\n")
    (personal-messages .  "Hantering av personliga meddelanden\n")
    (remote-control .     "Fjärrstyrning av LysKOM\n")
    (hooks .              "Hook-funktioner\n")

    (audio-cues .         "Ljudsignaler\n")
    (audio-cues-when    . "Ge ljudsignal:\n")
    (automatic-replies .  "Automatiskt svar\n")
    (mail-import .        "Emailimport\n")
    
    (audio-cues-doc . "\
  Efterföljande inställningar bestämmer vilken ljudsignal LysKOM skall ge 
  i olika situationer. Följande alternativ går att välja:

    Avslaget
        Ingen ljudsignal alls kommer att ges.

    Några gånger
        Emacs kommer att pipa en eller flera gånger när händelsen
        inträffar. Antal pip anges också. 

    Ljudfil
        Emacs kommer att spela upp den angivna ljudfilen. Programmet som
        angavs ovan som ljudspelarprogram används för att spela ljudet.")
    

    (sending-doc . "\
  Följande inställningar slår på eller av vissa kontroller vid inskickning
  av inlägg. Kontrollerna är avsedda att hindra att man gör någonting dumt.

    Bekräfta multipla mottagare
        Om ett inlägg eller kommentar har flera mottagare så kan LysKOM
        fråga vilka av mottagarna som är relevanta för inlägget. Antingen
        görs detta innan man skriver inlägget (då får man en fråga för
        varje defaultmottagare), eller när man skickar in inlägget (då får
        man bekräfta alla mottagare i klump.) Det går också att slå av
        helt.

    Kontroll av kommenterad författares medlemsskap
        Kontrollera att författaren till det kommenterade inlägget är
        medlem i någon av mottagarna för kommentaren. Om så inte är fallet,
        erbjuder LysKOM att lägga till författaren som mottagare till
        inlägget. 

    Kontroll av olästa kommentarer
        När detta är påslaget så kontrollerar LysKOM att det inlägg man
        kommenterar inte har några kommentarer man inte har läst. Detta är
        främst användbart för att undvika att man skriver någonting som
        någon annan redan har skrivit.")


    (windows-doc . "\
  Följande inställningar bestämmer hur fönster skapas i LysKOM. Alternativen
  som finns är följande:

    Något annat fönster 
        I ett annat fönster i samma frame som LysKOM. Om det bara finns ett
        fönster så kommer ett nytt att skapas (och tas bort när man är klar.)

    I en annan frame
        I en annan frame än LysKOM. Om det bara finns en frame så kommer en
        ny frame att skapas, och tas bort när man är klar. 

    I en ny frame
        En ny frame skapas för ändamålet, och tas bort när man är klar.

    LysKOM-buffertens fönster 
        LysKOM-buffertens fönster kommer att användas, och LysKOM kommer
        att återställas till fönstret när man är färdig.

    Ett fönster som visar bufferten 
        Om det finns ett fönster någonstans som visar den angivna bufferten
        så kommer detta fönster att användas. Det kan till exempel vara
        användbart om man hela tiden har ett fönster öppet, men inte beöver
        det just medan man utför något visst kommando.")


    ;;
    ;; Doc strings for variables
    ;;

    (kom-emacs-knows-iso-8859-1-doc . "\
  Påslaget betyder att Emacs förväntas förstå ISO-8859-1. Avslaget innebär att
  swascii accepteras i kommandonamn med mera. Skall vara påslaget.")

    (kom-bury-buffers-doc . "\
  Bestämmer hur bufferten hanteras när man går till ett annat KOM med Nästa
  LysKOM och liknande funktioner. Påslaget innebär att den aktuella bufferten
  läggs sist i buffertlistan när man byter LysKOM.")

    (kom-personal-messages-in-window-doc . "\
  Bestämmer i vilket fönster som bufferten med personliga meddelanden visas.
  För att den här inställningen skall spela någon roll så måste personliga
  meddelanden visas i en separat buffert, och bufferten måste visas varje
  gång det kommer meddelanden.")

    (kom-write-texts-in-window-doc . "\
  Bestämmer i vilket fönster nya texter skrivs.")

    (kom-prioritize-in-window-doc . "\
  Bestämmer i vilket fönster man prioriterar om möten.")

    (kom-edit-filters-in-window-doc . "\
  Bestämmer i vilket fönster man ändrar filter.")

    (kom-customize-in-window-doc . "\
  Bestämmer i vilket fönster man gör dessa inställningar.")

    (kom-view-commented-in-window-doc . "\
  Bestämmer i vilket fönster man visar kommenterarer medan man skriver nya.")

    (kom-list-membership-in-window-doc . "\
  Bestämmer i vilket fönster man listar medlemsskap.")


    (kom-user-prompt-format-doc . "\
  Format för LysKOM-prompten. Vissa teckenkombinationer sätter in speciella
  texter:

    %c - Sätter in kommandot som körs om man trycker på SPC eller RET.
    %[ - Sätter in '[' om automatsvar är påslaget.
    %] - Sätter in ']' om automatsvar är avstängt.     
    %m - Sätter in information om antal inspelade meddelanden.
    %s - Sätter in LysKOM-systemets namn
    %S - Sätter in LysKOM-serverns namn
    %p - Sätter in namnet på den person som är inloggad.
    %w - Sätter in namn på aktuellt möte.
    %# - Sätter in sessionsnummer.
    %  - Sätter in mellanslag om det ser ut att behövas.
    %% - Sätter in ett procenttecken.

  Nägra exempel:

    \"%[%c% %m%] - \"             Standardprompt
    \"%[%s: %c% %m%] - \"         Till exempel \"LysKOM: Se tiden - \"")

    (kom-user-prompt-format-executing-doc . "\
  Format för LysKOM-prompten efter man trycker på SPC eller RET.  Vissa
  teckenkombinationer sätter in speciella texter:

    %c - Sätter in kommandot som körs om man trycker på SPC eller RET.
    %[ - Sätter in '[' om automatsvar är påslaget.
    %] - Sätter in ']' om automatsvar är avstängt.     
    %m - Sätter in information om antal inspelade meddelanden.
    %s - Sätter in LysKOM-systemets namn
    %S - Sätter in LysKOM-serverns namn
    %p - Sätter in namnet på den person som är inloggad.
    %w - Sätter in namn på aktuellt möte.
    %# - Sätter in sessionsnummer.
    %  - Sätter in mellanslag om det ser ut att behövas.
    %% - Sätter in ett procenttecken.

  Nägra exempel:

    \"%[%c% %m%].\"             Standardprompt
    \"%[%s: Kör %c% %m%]...\"   Till exempel \"LysKOM: Kör Se tiden...\"")

    
    (kom-cite-string-doc . "\
  Text som sätts in före varje rad i ett citerat inlägg. Normalt brukar
  någonting i stil med \"> \" användas. Notera dock att det inte är brukligt
  att citera KOM-inlägg som man gör med sladdpost och News eftersom det
  kommenterade inlägget oftast finns tillgängligt.")


    (kom-created-texts-are-read-doc . "\
  Om detta är påslaget kommer alla inlägg man själv skriver att läsmarkeras
  automatiskt. Om inställningen är avslagen så får man läsa sådant man
  själv skriver.")


    (kom-default-mark-doc . "\
  Det markeringsvärde som används för nya markeringar. Om inget markerings-
  värde är valt frågar LysKOM varje gång man markerar en text. Värden mellan
  1 och 255 är tillåtna.")


    (kom-reading-puts-comments-in-pointers-last-doc . "\
  Bestämmer om kommentarslänkar visas före eller efter en text. Normalt brukar
  man visa kommentarslänkar efter texten.

  Före:

    398331 1996-09-24  13:22  /2 rader/ George Berkeley
    Mottagare: Filosofimötet <1226>
    Kommentar i text 398374 av John Locke
    Ärende: 
    ------------------------------------------------------------
    En abstrakt idé är en självmotsägelse. 
    (398331) -----------------------------------

  Efter:

    398331 1996-09-24  13:22  /2 rader/ George Berkeley
    Mottagare: Filosofimötet <1226>
    Ärende: 
    ------------------------------------------------------------
    En abstrakt idé är en självmotsägelse. 
    (398331) -----------------------------------
    Kommentar i text 398374 av John Locke
")

    (kom-show-author-at-end-doc . "\
  Om detta är påslaget så visas namnet på författaren efter inläggstexten.
  Naturligtvis så visas även namnet i inläggshuvudet. 

  Påslaget (med streckade linjer också påslaget):

    892342 1996-09-24  19:21  /2 rader/ Claude Shannon
    Mottagare: Presentation (av nya) Medlemmar
    Ärende: Claude Shannon
    ------------------------------------------------------------
    Informationsteoretiker
    (892342) /Claude Shannon/------------------------------

  Avslaget:

    892342 1996-09-24  19:21  /2 rader/ Claude Shannon
    Mottagare: Presentation (av nya) Medlemmar
    Ärende: Claude Shannon
    ------------------------------------------------------------
    Informationsteoretiker
    (892342) -----------------------------------

  Om man har streckade linjer avslaget så visas författaren på samma 
  sätt, men de streckade linjerna finns naturligtvis inte med.")

    (kom-truncate-threshold-doc . "\
  Om detta är påslaget kommer inlägg som är längre än maxantalet att kortas
  ner när man återser kommentarer, återser markerade inlägg eller återser de
  senaste eller första.  Inlägg kortas inte av då man återser inlägg, återser
  träd eller bara läser nästa inlägg.")

    (kom-truncate-show-lines-doc . "\
  Antalet rader som visas när ett inlägg avkortats.")
     
    (kom-dashed-lines-doc . "\
  Om detta är påslaget visas streckade linjer före och efter inläggstexten,
  annars visas en tomrad efter ärenderaden, och ingenting speciellt i slutet.

  Påslaget:

    892343 1996-09-24  19:21  /2 rader/ Tycho Brahe
    Mottagare: Presentation (av nya) Medlemmar
    Ärende: Tycho Brahe
    ------------------------------------------------------------
    Astronom och upptäckare av stjärnor med hemvist på Ven.
    (892343) -----------------------------------

  Avslaget:

    892343 1996-09-24  19:21  /2 rader/ Tycho Brahe
    Mottagare: Presentation (av nya) Medlemmar
    Ärende: Tycho Brahe

    Astronom och upptäckare av stjärnor med hemvist på Ven.
    (892343)

  De flesta brukar ha detta påslaget.")

    (kom-autowrap-doc . "\
  Om detta är påslaget så kommer LysKOM att försöka bryta om raderna i
  stycken vars rader egentligen är längre än skärmen är bred. Stycken
  som verkar vara förformatterade av författaren bryts dock aldrig om.")

    (kom-print-number-of-unread-on-entrance-doc . "\
  Om detta är påslaget så visas antalet olästa i ett möte när man går till 
  mötet:

  Påslaget:

    Gå till nästa möte...
    Presentation (av nya) medlemmar - 3 olästa
    Läsa nästa text - 

  Avslaget:

    Gå till nästa möte...
    Presentation (av nya) medlemmar
    Läsa nästa text - ")


    (kom-presence-messages-doc . "\
  Om man vill ha meddelanden i minibufferten när någon loggar in, ut eller
  byter namn i LysKOM bör man slå på den här inställningen. Meddelanden
  visas i minibufferten. Om man vill ha information om endast vissa
  personer går det att ange en lista av personer. Vill man inte ha några
  meddelanden alls är det bara att ange en tom lista.")

    (kom-presence-messages-in-buffer-doc . "\
  Om man vill ha meddelanden i LysKOM-bufferten när någon loggar in, ut eller
  byter namn i LysKOM bör man slå på den här inställningen.  Meddelanden
  visas i minibufferten. Om man vill ha information om endast vissa
  personer går det att ange en lista av personer. Vill man inte ha några
  meddelanden alls är det bara att ange en tom lista.")

    (kom-show-where-and-what-doc . "\
  Information i vilkalistan om varifrån en person är inloggad och vad personen
  gör visas bara om detta är påslaget.

  Påslaget:

        Användare                            Närvarande i möte              
        Kör från                             Gör                             
    --------------------------------------------------------------------------
     6810 George Berkeley                      Filosofimötet                  
          berkeley@emp1.tcd.ie                 (Skriver en kommentar.)        
     7571 John Locke                           Filosofimötet                  
          eridy@cc.ox.ac.uk                    (Väntar.)                      

  Avslaget:

        Användare                            Närvarande i möte              
    --------------------------------------------------------------------------
     6810 George Berkeley                      Filosofimötet                  
     7571 John Locke                           Filosofimötet")

    (kom-show-since-and-when-doc . "\
  Information i vilkalistan om när en person loggade in och hur länge sedan
  personen var aktiv.")

    (kom-idle-hide-doc . "\
  I vilkalistan visas per default enbart de sessioner som har varit aktiva
  under den senaste tiden. Denna inställning bestämmer hur många minuter
  en session får ha varit stilla och ändå visas i vilkalistan.")


    (kom-show-footnotes-immediately-doc . "\
  Fotnoter visas normalt på samma sätt som kommentarer, men om detta är
  påslaget så visas fotnoter direkt efter inlägget de hör till.")


    (kom-follow-comments-outside-membership-doc . "\
  Normalt så visar inte LysKOM kommentarer till ett inlägg om man inte är
  medlem i något av mötena kommentaren har som mottagare. Om man vill
  följa kommentarskedjor utanför de möten man är medlem i kan man slå
  på den här inställningen.")


    (kom-read-depth-first-doc . "\
  LysKOM kan visa inlägg antingen i den ordning de skapades, eller i den
  ordning de ligger i kommentarsträdet. Ta till exempel följande inlägg:

  Inlägg 1002 är inte en kommentar till någonting. Inlägg 1003 och 1004
  är kommentarer till inlägg 1002. Inlägg 1005 och 1007 är kommentarer
  till inlägg 1004 och inlägg 1006 är en kommentar till inlägg 1003. Då
  ser kommentarsträdet ut ungefär så här:
        
  1002 +-- 1003 --- 1006
       |
       +-- 1004 +-- 1005
                |
                +-- 1007

  Om man läser inläggen i tidsordning kommer de att visas i ordningen
  1002, 1003, 1004, 1005, 1006 och 1007. I kommentarsordning blir det
  i stället 1003, 1003, 1006, 1004, 1005 och 1007.")


    (kom-continuous-scrolling-doc . "\
  Påslaget innebär att klienten scrollar bufferten medan ny text sätts in, så
  man ser det senaste som har satts in. Detta fungerar bra om man har en
  snabb terminal, men kan vara värt att slå av om terminalen är långsam, och
  scrollning i Emacs tar tid.")


    (kom-deferred-printing-doc . "\
  För att snabba upp LysKOM så skrivs vissa saker, som person- och mötesnamn
  inte ut omedelbart, utan skrivs först ut som \"[...]\", vilket sedan byts
  ut mot den rätta utskriften när texten finns tillgänglig. Detta snabbar
  upp svarstiderna i klienten ganska mycket, men går att stänga av med den
  här inställningen om man tycker det ser otrevligt ut.")


    (kom-higher-priority-breaks-doc . "\
  När texter kommer in till möten som har högre prioritet än det man läser
  kan klienten låta en läsa dessa texter omedelbart, efter den aktuella
  kommentarskedjan eller efter man har läst ut mötet.")
    

    (kom-login-hook-doc . "\
  Kommandon som skall köras när man loggar in, innan någon inmatning från
  tangentbordet tas emot. Till exempel kan man göra Lista nyheter här för
  att få se en lista av nyheter varje gång man loggar in.")


    (kom-do-when-done-doc . "\
  Kommandon som skall köras när man har läst ut alla inlägg. Det är ganska
  vanligt att man har Återse alla markerade här. Både tangentbordsmakron
  och regelrätta funktioner går bra.")


    (kom-page-before-command-doc . "\
  LysKOM-bufferten kan scrollas före alla kommandon så att text som sätts in
  av kommandot hamnar överst i fönstret. Denna variabel talar om före vilka
  kommandon detta skall ske.")


    (kom-permissive-completion-doc . "\
  Om detta är påslaget så kommer TAB bara att fylla ut namn på inloggade
  personer när kommandot bara kan utföras på inloggade personer (till
  exempel Status (för) session och Sända messelande.) Om det är avslaget
  kommer TAB att fylla ut även med namn på personer som inte är inloggade.")


    (kom-membership-default-priority-doc . "\
  Detta bestämmer hur prioriteten på möten man går med i sätts. Om det är
  en siffra mellan 1 och 255 så kommer nya medlemskap att få den prioriteten.
  I annat fall kommer klienten att fråga för varje nytt medlemskap vilken
  prioritet det skall ha.")


    (kom-show-personal-messages-in-buffer-doc . "\
  Denna inställning bestämmer var personliga meddelanden, gruppmeddelanden
  och allmänna meddelanden visas. Meddelanden kan antingen visas i 
  LysKOM-bufferten, kastas bort helt eller visas i en namngiven buffert.")


    (kom-pop-personal-messages-doc . "\
  Om personliga meddelanden skall visas i en egen buffert och denna inställning
  också är påslagen så kommer den bufferten att visas så fort ett meddelande
  anländer.")


    (kom-audio-player-doc . "\
  Om man vill att LysKOM skall spela ljudfiler i stället för att pipa vid 
  olika tillfällen måste denna inställning peka ut ett program som klarar
  att spela ljudfilerna. Programmet skall ta ett argument: namnet på filen
  som skall spelas.")


    (kom-default-message-recipient-doc . "\
  Denna inställning bestämmer vem som kommer att vara defaultmottagare för
  personliga meddelande man skickar. Alternativen som finns är att meddelanden
  per default är allmänna, att avsändaren för det senast mottagna meddelandet
  skall vara default eller att mottagaren för det senaste gruppmeddelandet
  (eller avsändaren av det senaste personliga eller almänna meddelandet) skall
  vara mottagare.")


    (lyskom-filter-outgoing-messages-doc . "\
  Om denna inställning är påslagen så kommer meddelanden som skickas 
  automatiskt, till exempel automatiska svar och svar på fjärkontrollförsök
  även att visas som om man hade skickat det manuellt.")

    (kom-friends-doc . "\
  Namnen på personerna i denna lista kommer att visas med ett speciellt 
  utseende i LysKOM-bufferten.")

    (kom-url-viewer-preferences-doc . "\
  Denna inställning bestämmer vilken WWW-läsare som i första hand skall 
  användas för att öppna URLer som förekommer i LysKOM. Om den första läsaren 
  i listan inte klarar den typ av URL som man försöker öppna så används nästa
  läsare och så vidare.")

    (kom-mosaic-command-doc . "\
  Denna inställning talar om vilket kommando som skall användas för att starta
  Mosaic.")

    (kom-netscape-command-doc . "\
  Denna inställning anger kommandot för att köra Netscape.")

    (kom-inhibit-typeahead-doc . "\
  Normalt sparas tangenttryckningar som görs medan klienten är upptagen, och
  utförs när det blir möjligt. Om denna inställning är avslagen så kommer
  LysKOM enbart att utföra kommandon som ges när klienten inte arbetar.")

    (kom-max-buffer-size-doc . "\
  Det är möjligt att begränsa LysKOM-buffertens storlek genom att ange hur
  stor den får bli i den här variabeln. Om bufferten blir för stor kommer
  information från buffertens början att tas bort.")

    (kom-ansaphone-record-messages-doc . "\
  LysKOM kan \"spela in\" meddelanden som kommer när funktionen för automatiskt
  svar är påslagen. Denna inställning bestämmer om så sker eller inte.")

    (kom-ansaphone-show-messages-doc . "\
  Om denna inställning är påslagen kommer LysKOM att visa inkomna personliga
  meddelanden även om automatiskt svar är påslaget.")

    (kom-ansaphone-default-reply-doc . "\
  Automatsvararen skickar detta meddelande om inget annat meddelande har
  specificerats via någon annan mekanism (och den är bara till för försiktiga
  experter.)")

    (kom-remote-control-doc . "\
  Påslagen innebär att det går att fjärrstyra klienten. Fjärrstyrningen är
  dock begränsad till vissa personer.")

    (kom-remote-controllers-doc . "\
  Personerna i denna lista får fjärrstyra klienten.")

    (kom-self-control-doc . "\
  Om detta är påslaget får användaren som är inloggad styra klienten från
  en annan session. Detta är ett alternativ till att lägga in sig själv i
  listan över tillåtna fjärrstyrare.")

    (kom-customize-format-doc . "\
  Dokumentation till inställningarna kan vara på eller avslagen när 
  inställningsfönstret öppnas. Dokumentationen kan alltid visas eller
  gömmas för varje enskild inställning genom att använda frågetecknet
  eller utropstecknet som står till höger om inställningen.")

    (kom-default-language-doc . "\
  Språk som skall användas i LysKOM. Om du ändrar inställningen här så kommer
  inte inställningen ändras för aktuell session, utan du måste ge kommandot
  Ändra språk också.")

    (kom-ispell-dictionary-doc . "\
  Ordlista som skall användas av stavningskontrollen i LysKOM. Om detta
  är satt till kom-ispell-dictionary så kommer variabeln ispell-dictionary
  att användas istället.")

    (kom-show-namedays-doc . "\
  Påslaget innebär att dagens namn visas när man ser tiden. Antagligen så
  kommer detta att ersättas av ett nytt kommando i någon kommande version
  av elispklienten, men det var visst någon som ville ha det, så ...")


    (kom-mail-address-doc . "\
  Värde för From:-headern i skickade email.")
    
    (kom-mail-importer-persons-doc . "\
  Personer som i själva verket är emailimportörer.  Inlägg av dessa
  personer kommer att formatteras som emailtexter istället för som
  vanliga inlägg.")

    (kom-mail-visible-headers-doc . "\
  Visa dessa headers i emailtexter.")
				  
    ;;
    ;; Tags for variables
    ;;
    
    (kom-emacs-knows-iso-8859-1-tag . "Emacs förstår ISO-8859-1:")
    (kom-bury-buffers-tag . "Begrav buffertar när man byter LysKOM:")

  (kom-personal-messages-in-window-tag . "Personliga meddelanden:   ")
    (kom-customize-in-window-tag       . "Inställningar för LysKOM: ")
    (kom-write-texts-in-window-tag     . "Skriv inlägg:             ")
    (kom-prioritize-in-window-tag      . "Prioritera möten:         ")
    (kom-edit-filters-in-window-tag    . "Ändra filter:             ")
    (kom-view-commented-in-window-tag  . "Återse kommenterer:       ")
    (kom-list-membership-in-window-tag . "Lista medlemsskap:        ")

    (kom-user-prompt-format-tag . "Promptformat:")
    (kom-user-prompt-format-executing-tag . "Promptformat vid körning:")

    (kom-higher-priority-breaks-tag . 
"Läs prioriterade texter:                  ")
    (kom-created-texts-are-read-tag . 
"Läsmarkera skapade texter:                ")
    (kom-default-mark-tag           . 
"Defaultmarkering:                         ")
    (kom-print-number-of-unread-on-entrance-tag . 
"Visa olästa när man går till ett möte:    ")
    (kom-follow-comments-outside-membership-tag .
"Följ kommentarskedjor utanför medlemskap: ")
    (kom-show-footnotes-immediately-tag .
"Visa fotnoter omedelbart:                 ")
    (kom-membership-default-priority-tag . 
"Prioritet för nya medlemskap:             ")
    (kom-dashed-lines-tag . 
"Streckade linjer kring inläggstexten:     ")
    (kom-autowrap-tag . 
"Automatisk radbrytning vid läsning:       ")
    (kom-show-author-at-end-tag .
"Visa författarens namn efter inlägget:    ")

    (kom-truncate-threshold-tag .
"Korta av långa inlägg:                    ")
    (kom-truncate-show-lines-tag .
"Rader att visa vid avkortade inlägg:      ")
    
    (kom-reading-puts-comments-in-pointers-last-tag . "Kommentarslänkar visas:")
    (kom-read-depth-first-tag . "Läsordning:")
    (kom-deferred-printing-tag . "Fördröjda utskrifter:")
    (kom-continuous-scrolling-tag . "Omedelbar scrollning:")

    (kom-presence-messages-tag . 
"Närvaromeddelanden på eller av:       ")
    (kom-presence-messages-in-buffer-tag .
"Närvaromeddelanden i LysKOM-bufferten:")
    (kom-page-before-command-tag . "Rensa skärmen:")

    (kom-idle-hide-tag . 
"Antal minuter en session får vara inaktiv och ändå visas: ")
    (kom-show-where-and-what-tag . 
"Visa varifrån personer är inloggade och vad de gör:         ")
    (kom-show-since-and-when-tag . 
"Visa när personer loggade in och när de var aktiva:         ")



    (kom-login-hook-tag . "Kommandon som körs vid login:")
    (kom-do-when-done-tag . "Kommandon som körs efter allt är utläst:")
    (kom-permissive-completion-tag . "Petig utfyllnad av namn:")
    (kom-show-personal-messages-in-buffer-tag . 
"Var visas meddelanden:           ")
    (kom-pop-personal-messages-tag . 
"Ploppa upp meddelandebufferten:  ")
    (kom-default-message-recipient-tag . 
"Defaultmottagare för meddelanden:")

    (kom-audio-player-tag . "Ljudspelarprogram:")
    (kom-ding-on-new-letter-tag        . "När det kommer brev:               ")
    (kom-ding-on-priority-break-tag    . "När det kommer prioriterade inlägg:")
    (kom-ding-on-wait-done-tag         . "När man har väntat klart:          ")
    (kom-ding-on-common-messages-tag   . "Vid allmänna meddelanden:          ")
    (kom-ding-on-group-messages-tag    . "Vid gruppmeddelanden:              ")
    (kom-ding-on-personal-messages-tag . "Vid personliga meddelanden:        ")
    (kom-ding-on-no-subject-tag     . "När ärenderad saknas:              ")

    (lyskom-filter-outgoing-messages-tag . "Visa automatiska meddelanden:")
    (kom-friends-tag . "Vänner och bekanta:")
    (kom-url-viewer-preferences-tag . "Öppna URLer med följande program:")
    (kom-mosaic-command-tag . "Kommando för att starta NCSA Mosaic:")
    (kom-netscape-command-tag . "Kommando för att starta Netscape Navigator:")

    (kom-cite-string-tag . "Citatmarkering: ")
    (kom-confirm-multiple-recipients-tag . 
"Bekräfta multipla mottagare:                   ")
    (kom-check-commented-author-membership-tag . 
"Kontrollera kommenterad författares medlemskap:")
    (kom-check-for-new-comments-tag . 
"Kontrollera olästa kommentarer:                ")

    (kom-ansaphone-record-messages-tag . 
"Spara meddelanden då automatiskt svar är påslaget: ")
    (kom-ansaphone-show-messages-tag . 
"Visa meddelanden då automatiskt svar är påslaget:  ")
    (kom-ansaphone-default-reply-tag . "Svarsmeddelande:")


    (kom-inhibit-typeahead-tag . "Buffra tangenttryckningar:")
    (kom-max-buffer-size-tag . "Maximal buffertstorlek:")

    (kom-remote-control-tag .     "Fjärrstyrning:                     ")
    (kom-self-control-tag .       "Tillåt fjärrstyrning av mig själv: ")
    (kom-remote-controllers-tag . "Tillåtna fjärrstyrare:")

    (kom-customize-format-tag . "Visa hjälptexterna för inställningar:")
    (kom-default-language-tag . "Språk:           ")
    (kom-show-namedays-tag .        "Visa dagens namn:")
    (kom-ispell-dictionary-tag . "Ordlista:")

    (kom-mail-address-tag .          "Emailadress:       ")
    (kom-mail-importer-persons-tag . "Mailimportörer:    ")
    (kom-mail-visible-headers-tag .  "Visa dessa headers:")
    )
)

  
;;;; ============================================================
;;;; The default Ansaphone message goes here. The more complex 
;;;; message specification probably should too, but it's not here
;;;; yet. People who know how to use it are smart enough to do it
;;;; right.

(lyskom-language-var kom-ansaphone-default-reply sv
  "Jag läser inte LysKOM just nu. Skicka gärna ett brev i stället.")

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
     (("Återse texten" . lyskom-button-view-text)
      ("Återse omodifierat" . lyskom-button-review-noconversion)
      ("Återse träd" . lyskom-button-review-tree)
      ("Återse urinlägget" . lyskom-button-find-root)
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
      ("Visa mötesstatus" . lyskom-button-view-conf-status)
      ("Gå till mötet" . lyskom-button-goto-conf)
      ("Skicka gruppmeddelande" . lyskom-button-send-message)
      ("Bli medlem i mötet" . lyskom-button-add-self)
      ("Utträd ur mötet" . lyskom-button-sub-self))
     ((kom-list-news . lyskom-button-goto-conf)
      (kom-membership . lyskom-button-goto-conf)))
    (pers 
     pers-text
     lyskom-button-view-pers-presentation
     (("Visa presentation" . 
                             lyskom-button-view-pers-presentation)
      ("Visa personstatus" . lyskom-button-view-pers-status)
      ("Skicka brev" . lyskom-button-mail)
      ("Sänd meddelande" . lyskom-button-send-message))
     nil)
    (url 
     url-text
     lyskom-button-open-url
     (("Öppna" . lyskom-button-open-url)
      ("Kopiera" . lyskom-button-copy-url))
     nil)
    (info-node 
     info-node-text
     lyskom-button-goto-info-node
     (("Öppna" . lyskom-button-goto-info-node))
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
     (("Växla \"hemlig\"" . lyskom-edit-toggle-secret-aux)
      ("Växla \"anonym\"" . lyskom-edit-toggle-anonymous-aux)
      ("Växla \"ärvd\""   . lyskom-edit-toggle-inherit-aux)
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
   (kom-tell-send		. "Försöker lägga in en text.")
   (kom-tell-login		. "Loggar in.")
   (kom-tell-read		. "Läser.")
   (kom-tell-1st-pres		. "Skriver den första presentationen.")
   (kom-tell-write-comment	. "Skriver en kommentar.")
   (kom-tell-write-footnote	. "Skriver en fotnot.")
   (kom-tell-write-letter	. "Skriver ett brev.")
   (kom-tell-write-reply	. "Skriver ett privat svar.")
   (kom-tell-write-text	. "Skriver inlägg.")
   (kom-tell-conf-pres		. "Skriver presentation för ett nytt möte.")
   (kom-tell-recover		. "Återstartar kom. Suck.")
   (kom-tell-wait		. "Väntar.")
   (kom-tell-regret		. "Ångrar sig och slänger inlägget.")
   (kom-tell-review		. "Återser.")
   (kom-tell-change-name       . "Ändrar sitt namn till något annat.")
   (kom-tell-change-supervisor . "Ändrar organisatör för något.")
   (kom-tell-next-lyskom       . "Hoppar till ett annat LysKOM.")
   (kom-tell-is-anonymous      . "Är hemlighetsfull")))

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
  '((error-0 . "Inget fel har inträffat")
    (error-2 . "Ännu ej implementerat")
    (error-3 . "Inte längre implementerat")
    (error-4 . "Felaktigt lösenord")
    (error-5 . "Strängen för lång")
    (error-6 . "Du är inte inloggad")
    (error-7 . "Ingen får logga in i LysKOM just nu")
    (error-8 . "Du försökte använda möte nummer 0")
    (error-9 . "Odefinierat eller hemligt möte")
    (error-10 . "Odefinierad eller hemlig person")
    (error-11 . "Ingen skriv- eller läsrättighet")
    (error-12 . "Otillåten operation")
    (error-13 . "Du är inte medlem i mötet")
    (error-14 . "Inget sådant textnummer")
    (error-15 . "Du kan inte använda globalt textnummer 0")
    (error-16 . "Inget sådant lokalt textnummer")
    (error-17 . "Du kan inte använda lokalt textnummer 0")
    (error-18 . "Namnet för kort eller för långt eller innehåller felaktiga tecken")
    (error-19 . "Index utanför gränserna")
    (error-20 . "Mötet existerar redan")
    (error-21 . "Personen existerar redan")
    (error-22 . "Hemligt, men ej lässkyddat")
    (error-23 . "Du får inte ändra person/mötesflaggan")
    (error-24 . "Fel i databasen. Attans otur.")
    (error-25 . "Otillåtet misc-fält. (Internt fel)")
    (error-26 . "Otillåten infotyp. (Bug i klienten)")
    (error-27 . "Redan mottagare till denna text")
    (error-28 . "Redan kommentar till denna text")
    (error-29 . "Redan fotnot till denna text")
    (error-30 . "Inte mottagare till denna text")
    (error-31 . "Inte kommentar till denna text")
    (error-32 . "Inte fotnot till denna text")
    (error-33 . "För många mottagare")
    (error-34 . "För många kommentarer")
    (error-35 . "För många fotnoter")
    (error-36 . "För många markeringar")
    (error-37 . "Du är inte författare till texten")
    (error-38 . "Du kan inte koppla upp dig till servern")
    (error-39 . "Minnet slut")
    (error-40 . "Servern har ballat ur")
    (error-41 . "Klienten tror att servern säger att den inte förstår klienten")
    (error-42 . "Ingen sådan session finns")
    (error-43 . "Ogiltigt reguljärt uttryck")
    (error-44 . "Texten är inte markerad")
    (error-45 . "Tillfälligt fel. Försök senare")
    (error-46 . "Serven vägrade ta emot en array")
    (error-47 . "Mottagaren tar inte emot anonyma texter")
    (error-48 . "Otillåten tilläggsinformation")
    (error-49 . "Otillåten ändring av tilläggsinformation")
    (error-50 . "Okänt asynkront meddelande")
    (error-51 . "Internt fel i servern")
    (error-52 . "Funktionen avstängd i servern")
    (error-53 . "Meddelandet kunde inte sändas")
    (error-54 . "Otillåten typ på medlemskap")
))


(provide 'lyskom-language-sv)

;;; swedish-strings.el ends here
