;;;;; -*-unibyte: t;-*-
;;;;;
;;;;; $Id: swedish-strings.el,v 44.33.2.1 1999-10-13 09:56:17 byers Exp $
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
	      "$Id: swedish-strings.el,v 44.33.2.1 1999-10-13 09:56:17 byers Exp $\n"))


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
  (define-key lyskom-sv-edit-mode-map "\C-c"	'lyskom-sv-edit-prefix)
  (define-key lyskom-sv-edit-mode-map "\C-c?"	'lyskom-help)
  (define-key lyskom-sv-edit-mode-map "\C-c}"	'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-mode-map "\C-c]"	'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-]" 'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-mode-map [(control c) (control })] 'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (lyskom-keys [C-Å])
    'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (lyskom-keys [C-å])
    'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-mode-map (lyskom-keys [mouse-2])
    'kom-button-click-or-yank)
  (define-key lyskom-sv-edit-mode-map (lyskom-keys [down-mouse-3]) 'kom-popup-menu)
  (define-key lyskom-sv-edit-mode-map [mouse-3] 'kom-mouse-null)
  (define-key lyskom-sv-edit-mode-map "\C-c*" 'kom-button-press)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-i" 'lyskom-sv-edit-insert-prefix)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-c" 'kom-edit-send)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-s" 'kom-ispell-message)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-k" 'kom-edit-quit)
  (define-key lyskom-sv-edit-mode-map "\C-c}?"	'lyskom-help)
  (define-key lyskom-sv-edit-mode-map "\C-c}\C-k" 'kom-edit-show-commented)
  (define-key lyskom-sv-edit-mode-map "\C-c}k" 'kom-edit-show-commented)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-i?" 'lyskom-help)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-i\C-k" 'kom-edit-insert-commented)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-y" 'kom-edit-insert-commented)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-i1" 'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-i2" 'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-i3" 'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-i4" 'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-i5" 'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-i6" 'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-i7" 'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-i8" 'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-i9" 'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-i " 'kom-edit-insert-text)
  (define-prefix-command 'lyskom-sv-edit-add-prefix)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-a" 'lyskom-sv-edit-add-prefix)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-a\C-m" 'kom-edit-add-recipient)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-a\C-k" 'kom-edit-add-comment)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-a\C-e" 'kom-edit-add-copy)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-a\C-f" 'kom-edit-move-text)
  (define-key lyskom-sv-edit-mode-map "\C-c\C-a?" 'lyskom-help))


;;(defvar lyskom-header-separator
;;  (substitute-command-keys
;;   "\\<lyskom-edit-mode-map>\
;;--- Skriv nedan. \
;;Skicka in=\\[kom-edit-send], \
;;Avbryt=\\[kom-edit-quit], \
;;Annat se \\[describe-mode] ---")
;;  "*String to separate headers from text body.")
;;
;;(defvar lyskom-swascii-header-separator nil
;;  "The swascii version of lyskom-header-separator.")


;;(defvar lyskom-header-subject "\304rende: "
;;  "*String to prompt for subject in the edit buffer.")
;;
;;(defvar lyskom-swascii-header-subject nil
;;  "The swascii version of lyskom-header-subject.")

(defconst lyskom-strings-missing '())

;;; ================================================================
;;;                      Iso-8859-1 converting

;;; Author: Linus Tolke Y


;;(defvar iso-8859-1-table 
;;  "	
;;
;; !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~	
;;
;;  !c#$Y|$\"c+?!-R~C+23'u$-,10?????AAAA[][CE@EEIIIIDNOOOO\\*\\UUU^YTBaaaa{}{ce`eeiiiidnoooo|/|uuu~yty"
;;  "*This is a table of the chars corresponding value in SWASCII.
;;Used by the function iso-8859-1-to-swascii function.")
;;
;;
;;(defun iso-8859-1-to-swascii (string)
;;  "Returns a string without characters with code > 127.
;;What chars are converted to is controlled by the iso-8859-1-table."
;;  ;; If argument is a vector of strings
;;  (if (vectorp string)
;;	(apply 'vector (mapcar 'iso-8859-1-to-swascii string))
;;    (let ((tmp (copy-sequence string))
;;	    (i 0)
;;	    (len (length string)))
;;	(while (< i len)
;;	(aset tmp i (aref iso-8859-1-table
;;			  (aref tmp i)))
;;	(setq i (1+ i)))
;;	tmp)))


;;; The alist formely known as lyskom-strings
(lyskom-language-strings lyskom-message sv
  '(
    ;; From vars.el: 
    ;; From komtypes.el: nil
    ;; From clienttypes.el: nil
    ;; From startup.el:
    (server-q . "LysKOM-server? (%#1s) ")
    (try-connect . "LysKOM elisp-klient version %#1s.\nF\366rs\366ker koppla upp mot %#2s.\n")
    (protocoll-error . "Protocol error. Servers says: %#1s")
    (connection-done . "Uppkopplingen klar. Serverns versionsnummer \344r %#1s.\n\n")
    (what-is-your-name . "Vad heter du? ")
    (password . "L\366senord? ")
    (wrong-password . "Fel l\366sen.\n")
    (are-logged-in . "Du \344r nu inloggad. V\344nta ett tag.\n")
    (you-have-motd . "\nDu har en lapp p\345 d\366rren:\n\n")
    (lyskom-motd-was-garbed . "\nLoginmeddelandet finns inte!
Det meddelande som ska visas efter inloggning har f\366rsvunnit.
Kontakta LysKOM-administrat\366ren.\n")
    (presentation-encouragement . 
				"Du har ingen presentation. Det skulle vara trevligt om du skrev en.
Anv\344nd kommandot \304p. Om du inte vill skriva n\345gon presentation tryck fs.\n")

    (first-greeting . "%#1s
Det tycks vara f\366rsta g\345ngen du anv\344nder LysKOM. V\344lkommen!
Kontrollera att du stavat ditt namn r\344tt.  Anv\344nd g\344rna ditt fullst\344ndiga
namn och organisation, t ex \"Eskil Block, FOA\".  Om du stavat ditt namn
fel, eller vill \344ndra ditt namn, svara nej p\345 fr\345gan nedan.

Observera att all information \344n s\345 l\344nge sparas s\345 att vem som helst
kan l\344sa den. Dock \344r l\366senordet krypterat.

Om du \344r os\344ker p\345 hur man anv\344nder LysKOM kan du h\344mta en manual via
anonym ftp fr\345n ftp.lysator.liu.se. Fr\345ga din systemadministrat\366r om
du \344r os\344ker.\n")

    (is-name-correct . "\304r namnet %#1s korrekt? ")
    (personal-password . "Ange ett personligt l\366sen: ")
    (repeat-password . "Repetera f\366r kontroll: ")
    (repeat-failure . "Du angav inte samma l\366senord b\344gge g\345ngerna.\n")

    (could-not-create-you .  "Det gick inte att skapa personen.\n")
    (presentation-subject . "%#1s")
    (presentation-form . "Namn:\t\nAdress:\t\nStad:\t\nTelefon:
Email-adress:\nWWW:\t\n\nAnnat:\t")
    (presentation-help . "Du skriver just  nu din presentation.\n")
    (not-present-anywhere . "Ej n\344rvarande i n\345got m\366te.")
    (secret-person . "Hemlig person")
    (in-secret-conference . "Hemligt m\366te (%#1d).")
    (start-new-session-same-server
     . "Du k\366r redan mot den servern. Vill du starta en ny session? ")
    (new-session-in-buffer . "\n\n---- Ny session startad %s ----\n\n")


    ;; From internal.el:
    (shaky-tcp . "Nu n\345r jag inte servern. TCP/IP-f\366rbindelsen \344r skakig%#1s")
    (retrying-tcp . "Jag f\366rs\366ker igen.")

    ;; From parse.el:
    (protocol-error . "protokollfel: %s")

    ;; From services.el:
    (interrupted . "Kommandot avbrutet\n")

    ;; From cache.el:
    ;; No entries.

    ;; From commands1.el:
    (appreciation . "Du \344r mycket vacker och mycket klok. M\345nga \344lskar dig b\345de till kropp
och till sj\344l. Du kommer att \366ver\366sas med rikedom och f\345 stor lycka i ditt
liv. Var glad att just du \344r du. Det har du all anledning att vara.
Fantomen \366nskar dig en bra dag.\n\n")
    (abuse . "Du \344r mycket ful och mycket dum. M\345nga hatar dig b\345de till kropp
och till sj\344l. Du kommer att \366ver\366sas med sjukdomar och inte f\345 n\345gon som
helst lycka i ditt liv. Du borde verkligen ta dig samman och g\366ra n\345got av
ditt liv \344ven om det inte \344r mycket att starta med.
Guran vill helst s\344tta en giftpil i dig.\n\n")

    (what-conf-to-delete . "Vilket m\366te/person vill du utpl\345na: ")
    (what-conf-to-change . "Vilket m\366te vill du \344ndra: ")
    (confirm-delete-pers-or-conf . "Ta bort %#1s %#2s? ")
    (the-pers . "personen")
    (the-conf . "m\366tet")
    (deletion-not-confirmed . "Utpl\345ningen avbruten\n")
    (somebody-else-deleted-that-conf . "N\345gon annan tog precis bort m\366tet.\n")
    (conf-is-deleted . "Ok, nu \344r %#1s utpl\345nad.\n")
    (you-could-not-delete . "%#1M kunde inte utpl\345nas av dig.\n")
    (you-have-deleted-yourself . "Du har utpl\345nat dig sj\344lv.\n")

    (what-text-to-delete . "Vilket inl\344gg skall tas bort? ")
    (deleting-text . "Radering av text %#1:n...")

    (presentation-for-whom . "Vilket m\366te/person? ")
    (somebody-deleted-that-conf . "N\345gon tog precis bort m\366tet.\n")
    (review-presentation-of . "\305terse presentation av %#1M.\n")
    (has-no-presentation . "%#1:M har ingen presentation.\n")

    (have-to-read . "Du m\345ste l\344sa ett inl\344gg f\366rst.\n")

    (no-comment-to . "Det finns inget kommenterat inl\344gg att titta p\345.\n")

    (who-letter-to . "Vem vill du skicka brev till? ")
    (has-motd . "%#1P har en lapp p\345 d\366rren:\n\n")
    (motd-persist-q . "Vill du fortfarande skicka brevet? ")

    (who-to-add . "Vem vill du addera? ")
    (where-to-add . "Vilket m\366te skall han/hon adderas till? ")
    (where-to-add-self . "Vilket m\366te vill du bli medlem i? ")
    (priority-q . "Prioritet p\345 m\366tet? (0 (passivt medlemskap), 1 (l\345g) - 255 (h\366g)) ")
    (done . "klart.\n")
    (nope . "Det gick inte.\n")

    (cant-find-supervisor . "Hittar inte organisat\366ren f\366r %#1M.\n")
    (is-read-protected-contact-supervisor . "%#1M \344r slutet.
Skicka ett brev till %#2P f\366r medlemsskap.\n")

    (conf-does-not-exist . "\nM\366tet finns inte.\n")

    (who-to-exclude . "Vem vill du utesluta? ")
    (where-from-exclude . "Vilket m\366te skall han/hon uteslutas ifr\345n? ")

    (leave-what-conf . "Vilket m\366te vill du g\345 ur? ")

    (error-fetching-person . "Fel i personh\344mtningen.\n")
    (error-fetching-conf . "Fel i m\366tesh\344mtningen.\n")

    (name-of-conf . "Vad ska m\366tet heta? ")
    (anyone-member . "F\345r vem som helst bli medlem? ")
    (secret-conf . "Hemligt m\366te? ")
    (comments-allowed . "F\345r man skriva kommentarer? ")
    (anonymous-allowed . "F\345r man skriva anonyma texter? ")
    (what-comment-no . "Kommentera text nummer: ")
    (what-footnote-no . "Fotnotera text nummer: ")
    (confusion-what-to-comment . "Jag f\366rst\345r inte vilken text du vill kommentera.\n")
    (confusion-what-to-footnote . "Jag f\366rst\345r inte vilken text du vill skriva en fotnot till.\n")

    (what-private-no . "Personligt svar till text nummer: ")
    (confusion-who-to-reply-to . "Jag f\366rst\345r inte vems inl\344gg du vill
 skriva ett privat svar till.\n")
    (confusion-what-to-answer-to . "Jag f\366rst\345r inte vilken text du vill besvara.\n")
    (confusion-what-to-view . "Jag f\366rst\345r inte vilken text du vill \345terse.\n")
    (quit-in-spite-of-unsent . "Vill du avsluta sessionen trots os\344nt meddelande? ")
    (really-quit . "Vill du verkligen avsluta sessionen? ")
    (session-ended . "
*****************************
Lyskom-sessionen \344r avslutad.
*****************************\n")
    (session-auto-ended . "
===========================================================
Kopplar ned fr\345n LysKOM eftersom LysKOM \344r fullt och
du har l\344st klart allting. Kom tillbaks senare.
===========================================================\n\n")
    (what-to-change-pres-you . "Vilket m\366te/person vill du \344ndra presentationen f\366r (dig sj\344lv): ")
    (who-to-put-motd-for . "Vilket m\366te/person vill du s\344tta lapp p\345 d\366rr f\366r (dig sj\344lv): ")

    (cant-get-conf-stat . "Kan ej h\344mta m\366tesstatus f\366r m\366tet.\n")
    (go-to-conf-p . "G\345 till m\366te: ")
    (want-become-member . "Vill du bli medlem? ")
    (no-ok . "Nehej.\n")

    (who-to-remove-motd-for . "Vilket m\366te/person vill du ta bort lapp p\345 d\366rr f\366r (dig sj\344lv): ")

    (conf-all-read . "%#1M - inga ol\344sta.\n")
    (no-in-conf . "Du \344r inte n\344rvarande i n\345got m\366te.\n")

    (search-for-pers . "Ange s\366kvillkor (RETURN f\366r alla personer): ")
    (search-for-conf . "Ange s\366kvillkor (RETURN f\366r alla m\366ten): ")
    (search-re . "Ange s\366kuttryck (regulj\344rt uttryck): ")
    (name-to-be-changed . "Ange det namn som skall \344ndras: ")
    (no-such-conf-or-pers . "M\366tet eller personen finns ej.\n")
    (new-name . "Nytt namn: ")
    (who-to-change-supervisor-for . "Vem vill du \344ndra organisat\366r f\366r? ")
    (new-supervisor . "Ny organisat\366r: ")
    (text-to-mark . "Vilket inl\344gg vill du markera? ")
    (text-to-unmark . "Vilket inl\344gg vill du avmarkera? ")
    (what-mark . "Vilken markering vill du s\344tta? ")
    (unmarking-textno . "Avmarkering av text %#1n...")
    (marking-textno . "Markering av text %#1n...")

    (new-passwd-again . "Mata in det nya l\366senordet igen f\366r kontroll: ")
    (what-mark-to-view . "Vilken markering vill du \345terse? ")
    (whos-passwd . "Vem vill du \344ndra l\366senord f\366r? (dig sj\344lv) ")
    (old-passwd . "Mata in ditt l\366senord: ")
    (new-passwd . "Mata in det nya l\366senordet: ")
    (changing-passwd . "\304ndrar l\366senordet...")
    (retype-dont-match . "L\366senorden \344r inte samma. G\366r om.\n")
    (palindrome . "(ett palindrom!) ")
    (lyskom-name . "Anv\344ndare")
    (is-in-conf . "N\344rvarande i m\366te")
    (from-machine . "K\366r fr\345n")
    (is-doing . "G\366r")
    (lyskom-client . "Klient")
    (text-to-add-recipient . "Vilket inl\344gg vill du addera mottagare till:")
    (text-to-add-copy . "Vilket inl\344gg vill du addera en extra kopia till:")
    (text-to-add-bcc . "Vilket inl\344gg vill du skicka för kännedom:")
    (text-to-delete-recipient . "Vilket inl\344gg vill du subtrahera mottagare fr\345n:")
    (text-to-move . "Vilket inl\344gg vill du flytta: ")
    (text-to-add-comment-to . "Vilket inl\344gg vill du addera en kommentar till:")
    (text-to-delete-comment-from . "Vilket inl\344gg vill du subtrahera en kommentar fr\345n:")

    (where-on-list-q . "Placering p\345 listan? (0-%#1d) ")
    (member-in-conf . "Bli medlem i %#1M...")
    (add-member-in . "Addera %#1P som medlem i %#2M...")
    (unsubscribe-to . "Uttr\344da ur %#1M...")

    (exclude-from . "Utesluta %#1P fr\345n %#2M...")

    (unsubscribe-failed . "\nDet gick inte. %#1P var kanske aldrig medlem i %#2M?\n")

    (You . "Du")
    (could-not-create-conf . "M\366tet \"%#1s\" kunde ej skapas.\n")
    (created-conf-no-name . "M\366te nummer %[%#3@%#1:m %#2:M%], skapat.\n")
    (cant-read-textno . "Du f\345r inte l\344sa text %#1:n")

    (not-supervisor-for . "Du \344r inte organisat\366r f\366r %#1M.\n")
    (go-to-conf . "G\345 till %#1M.\n")
    (cant-go-to-his-mailbox . "Du f\345r inte g\345 till %#1Ms brevl\345da.\n")
    (not-member-of-conf . "Du \344r inte medlem i %#1M.\n")
    (about-to-change-name-from . "%#1M\n")
    (change-name-done . "Klart. Nytt namn: %[%#2@%#1:M%].\n")
    (change-name-nope . "Det gick inte bra att \344ndra till %#1s.\nFelkod %#3d. %#2s.\n")
    (change-supervisor-from-to . "\304ndra organisat\366r f\366r %#1M till %#2P...")
    (change-supervisor-nope . 
			    "\nDet gick inte. Kanske du inte f\345r \344ndra organisat\366r f\366r %#1M?\n")
    
    (no-marked-texts . "Du har inga markerade inl\344gg.\n")
    (no-marked-texts-mark . 
			  "Du har inga markerade inl\344gg med markeringen %#1d.\n")

;;; For later
;    (northward . "norrut")
;    (southward . "s\366derut")
;    (permanent-sundown . "Solen kommer inte att g\345 upp. Flytta %#1s!")
;    (permanent-sunup . "Solen kommer inte att g\345 ned. Fytta %#1s!")
;    (sunup-soon . "Solen g\345r snart upp")
;    (sundown-recently . "Solen gick nyligen ned")
;    (after-sunset . "Solen har g\345tt ned")
;    (before-sunup . "Solen har inte g\345tt upp \344nnu")
;    (sun-is-up . "Solen \344r uppe")
;;;
    (weekdays . ["s\366ndag" "m\345ndag" "tisdag" "onsdag" "torsdag"
		 "fredag" "l\366rdag" "s\366ndag"])
    (time-is . "Det \344r %#1s %#2s(enligt servern).")
    (time-format-exact . "%#7s %4#1d-%02#2d-%02#3d %02#4d:%02#5d:%02#6d")

    (xmaseve . "\nJulafton! Har du \366ppnat dina julklappar \344n?")
    (xmasday . "Juldagen.\nDu har väl varit i julottan?")
    (newyearday . "Gott nytt %#1d!")
    (newyearevelate . "Mindre \344n en timme kvar...")
    (newyeareve . "Gott nytt \345r!")
    (cgdag . "Konungens födelsedag.")
    (sixjune . "Sveriges nationaldag och svenska flaggans dag")
    (holdnose . "H\345ll f\366r n\344san...")
    (lysbday . "
P\345 denna dag, \345r 1973, grundades Lysator, och det var en stor dag
i svensk datorhistoria. L\344s mer p\345 http://www.lysator.liu.se/history/")


    (total-users . "    Sammanlagt %#1d anv\344ndare.\n")
    (total-visible-users . "    Sammanlagt %#1d synliga anv\344ndare.\n")
    (total-active-users . "    Sammanlagt %#1d aktiva anv\344ndare.\n")
    (total-visible-active-users
     . "    Sammanlagt %#1d synliga aktiva anv\344ndare.\n")
    (who-to-add-q . "Vilket m\366te/person vill du addera som mottagare? ")
    (who-to-add-copy-q . "Vilket m\366te/person vill du addera som kopiemottagare? ")
    (who-to-sub-q . "Vilket m\366te/person vill du subtrahera som mottagare? ")
    (who-to-move-from-q . "Fr\345n vilket m\366te vill du flytta texten? ")
    (who-to-move-to-q . "Vart vill du flytta texten? ")

    (adding-name-as-recipient . "Adderar %#1M som mottagare till text %#2n...")
    (adding-name-as-copy . "Adderar %#1M som kopiemottagare till text %#2n...")
    (remove-name-as-recipient .
			      "Subtraherar %#1M som mottagare fr\345n text %#2n...")
    (moving-name . "Flyttar text %#3n fr\345n %#1M till %#2M...")
    (text-to-add-q . "Vilket inl\344gg vill du addera som kommentar? ")
    (text-to-remove-q . "Vilket inl\344gg vill du subtrahera som kommentar? ")
    (add-comment-to . "Adderar text %#1n som kommentar till text %#2n...") ;
    (sub-comment-to . "Subtraherar text %#1n som kommentar till text %#2n...")
    (comment-keep-recpt-p ."Ska %#1s vara mottagare? ")
    (comment-all-relevant-p . "Inl\344gget har flera mottagare. \304r alla relevanta? ")
    (please-edit-recipients . "\304ndra mottagarlistan och skicka in inl\344gget igen.")
    (checking-rcpt . "Kontrollerar mottagare...")
    (checking-rcpt-done . "Kontrollerar mottagare...klart")
    (checking-comments . "Kontrollerar kommenterade texter...")
    (checking-comments-done . "Kontrollerar kommenterade texter...klart")
    (please-check-commented-texts . "Återse de kommenterade texterna och deras kommentarer.")
    (have-unread-comment . "Skicka in trots olästa kommentarer till text %#1n? ")

    (add-recipient-p . "Addera %#1P som mottagare? ")
    (matching-regexp . "M\366ten/personer som matchar '%#1s'\n")

    (who-is-active-all . "Visar alla sessioner.\n")
    (who-is-active-last-minutes . "Visar alla sessioner som har varit aktiva de senaste %#1d minuterna.\n")
    (showing-invisibles . "Visar osynliga sessioner.\n")
    (null-who-info . "Det finns inga (aktiva) inloggade.\n")

    (no-other-lyskom-r . "Det finns inga fler aktiva LysKOM-sessioner.\n")
    (no-lyskom-session . "Det finns ingen aktiv LysKOM-session.")
    (no-unread-lyskom-r . "Hittar ingen aktiv LysKOM-session med olästa.\n")
    (no-unread-lyskom . "Hittar ingen aktiv LysKOM-session med olästa.")

    ;; From commands2.el:

    (your-memberships . "Ditt medlemskap i olika KOM-m\366ten:\n")
    (memberships-header . "Senast inne	   Prio	Ol\344sta	M\366tesnamn\n")
    (memberships-line . "%16#1s  %#2d\t%#3d\t%#4M\n")
    (conf-for-status . "Vilket m\366te vill du se statusen f\366r? ")
    (no-such-conf . "M\366tet finns ej.\n")
    (status-record . "Status f\366r m\366te %#1M (%#2m) %#3s\n\n")
    (change-type-prompt . "\304ndra m\366testyp f\366r m\366te %#1M (%#2m) %#3s\n")
    (Mailbox . "Brevl\345da")
    (Protected . "Skyddat")
    (no-comments . "original")
    (closed . "slutet")

    (created-by . "Skapat av person %25#1p %#3s(%#2P)\n")
    (created-at . "Skapad:%35#1s\n")
    (members .    "Antal medlemmar: %25#1d\n")
    (garb-nice . "Livsl\344ngd p\345 inl\344gg (dagar):%14#1d\n")
    (lowest-local-no . "L\344gsta existerande lokala nummer: %8#1d\n")
    (highest-local-no . "H\366gsta existerande lokala nummer: %8#1d\n")
    (last-text-time . 
		    "Tid f\366r senaste inl\344gg: %18#1s (st\345r det i din cache)\n")
    (no-of-motd . "Lapp p\345 d\366rren i text nummer: %12#1n\n")
    (superconf-is-no-name . "Superm\366te:       %25#1m %#3s(%#2M)\n")
    (permitted-submitters-no-name . "Till\345tna f\366rfattare:%22#1m %#3s(%#2M)\n")
    (supervisor-is-no-name . "Organisat\366r:     %25#1p %#3s(%#2P)\n")
    (presentation-no . "Presentation:    %25#1n\n")
    (conf-has-motd . "\n%#1M har en lapp p\345 d\366rren:\n")

    (Everybody . "Alla")
    (show-members-list-also-q . "Vill du se medlemslistan ocks\345? ")
    (show-membership-info-q . "Visa antalet ol\344sta? ")
    (conf-has-these-members . "\n%#1M har f\366ljande medlemmar:\n")
    (member-list-header . "Senast inne         Osett  Namn\n\n")
    (secret-membership . "*** Hemlig rad ***\n")
    (conf-membership-line . "%#1s%#2M\n")
    (pers-for-status . "Vem vill du se statusen f\366r? ")
    (no-such-pers . "Det finns ingen s\345dan person.\n")
    (pers-status-record . "Status f\366r person %#1P (%#2p)\n")
    (created-time .  "Skapad:%34#1s\n\n")
    (created-confs . "Skapade m\366ten:%27#1d\n")
    (created-persons . "Skapade personer:%24#1d\n")
    (created-texts . "Skapade texter:%26#1d\n")
    (created-lines . "Skapade rader:%27#1d\n")
    (created-chars . "Antal skapade tecken:%20#1d\n")
    (no-of-sessions . "Antal sessioner:%25#1d\n")
    (present-time-d-h-m-s . "N\344rvarotid:%19#1d d %02#2d:%02#3d:%02#4d\n")
    (last-log-in . "Senast inne:%29#1s\n")
    (user-name . "Anv\344ndare: %30#1s\n")

    (read-texts . "L\344sta texter:%28#1d\n")
    (marked-texts . "Markerade texter:%24#1d\n")
    (time-for-last-letter . "Tid f\366r senaste brev:%20#1s (st\345r det i din cache)\n")
    (superconf . "Superm\366te:%31#1m %#3s(%#2M)\n")
    (supervisor . "Organisat\366r:%29#1p %#3s(%#2P)\n")
    (member-of-confs . "Medlem i (antal m\366ten):%18#1d\n")
    (presentation . "Presentation:    %24#1n\n")
    (show-membership-list-also-q . "Vill du se vilka m\366ten personen \344r medlem i ocks\345? ")
    (not-allowed-see-confs . "Du f\345r inte se vilka m\366ten %#1P \344r medlem i.\n")
    (is-member-of . "\n%#1P \344r medlem i f\366ljande m\366ten:\n")
    (membership-list-header . "Senast inne         Osett  Namn\n\n")
    (pers-membership-line . "%#1s%#2s%#3M\n")
    (is-supervisor-mark . "O ")
    (who-to-send-message-to . "Vem vill du skicka meddelandet till? (%s) ")
    (send-empty-message-p . "Meddelandet \344r tomt. Vill du \344nd\345 skicka det? ")
    (his-total-unread . "\n%#1M har totalt %#2d ol\344sta.\n")
    (message-prompt . "Meddelande: ")
    (message-sent-to-user
     . "================================================================
Ditt meddelande till %#2M:

%#1t
----------------------------------------------------------------
")
    (message-sent-to-all
     . "================================================================
Ditt allm\344nna meddelande l\366d:

%#1t
----------------------------------------------------------------
")
    (message-all-info . "S\344nd allm\344nt meddelande\n")
    (message-recipient-info . "S\344nd meddelande till %#1M\n")
    (message-nope
     . "Du kunde inte skicka meddelandet. Mottagaren var kanske inte inloggad.
Meddelandet du f\366rs\366kte s\344nda till %#1M var:
%#2t\n")
    (only-last . "Endast l\344sa senaste (0 - %#1d) i %#2s: ")
    (only-error . "N\345got gick galet. Sorry.\n")

    (you-have-unreads . "Du har %#1d ol\344sta inl\344gg i %#2M\n")
    (you-have-an-unread . "Du har 1 ol\344st inl\344gg i %#1M\n")
    (you-have-unreads-special . "Du har %#1d okommenterade inl\344gg i %#2M\n")
    (you-have-an-unread-special . "Du har 1 okommenterat inl\344gg i %#1M\n")
    (you-have-read-everything . "Du har sett alla nyheter.\n")
    (total-unreads . "\nDu har %#1d ol\344sta inl\344gg.\n")
    (total-unread . "\nDu har 1 ol\344st inl\344gg.\n")
    (waiting-for-anything .
			  "Du v\344ntar p\345 ett inl\344gg i vilket m\366te som helst.\n")
    (waiting-higher-than . 
			 "Du v\344ntar p\345 ett inl\344gg i ett m\366te med h\366gre prioritet \344n %#1d.\n")

    (have-to-be-in-conf-with-unread . "Du m\345ste g\345 till ett icketomt m\366te f\366rst.\n")
    (Texts . "Inl\344gg")
    (Date . "Datum")
    (Lines . "Rad.")
    (Author . "F\366rfattare")
    (Subject . "\304rende")
    (could-not-read . "Du fick inte l\344sa denna text (%#1n).\n")
    (multiple-choice . "Flera alternativ finns.")
    (does-not-exist . "Detta kommando finns inte.")
    (summary-line . "%=-8#1n%#2s%4#3d  %[%#4@%#5:P%]  %[%#6@%#7r%]\n")

    (what-mark-to-list . "Vilken markering vill du lista? ")
    (you-have-marks . "Du har %#1d inl\344gg markerade markerade med %#2d.\n")
    (you-have-marks-all . "Du har %#1d markerade inl\344gg.\n")


    ;; Only people fixing bugs or recieving bugg-reports should
    ;; change these:
    (buggreport-compilestart . "Skapar buggrapporten...")
    (buggreport-compileend . "Skapar buggrapporten...klart")
    (buggreport-description . "Detta gjorde jag:
\(Fyll i dina kommentarer nedan\)\n================\n\n
================
Bland informationen nedan finns ocks\345 en lista p\345 de 100 sist tryckta
tangenterna i din emacs. Om du nyligen loggat in kan den inneh\345lla ditt
lyskoml\366senord. Titta igenom den och \344ndra det som \344r ditt l\366senord
till * * * eller m i t t l \366 s e n eller n\345got annat l\344mpligt.

N\344r du skrivit klart skall du skicka in din buggrapport till LysKOMs
elispklientutvecklare. Det sker antingen:
* med email till bug-lyskom@lysator.liu.se
* med vanligt brev till:
\tLysator
\tc/o ISY
\tLink\366ping University
\tS-581 83 Linkoping
\tSWEDEN.

M\344rk kuvertet \"LysKOM buggrapport f\366r elispklienten\".\n\n")
    
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


    (not-logged-in . "Du \344r inte inloggad.  ")


    ;; +++ cant seem to find where these are used:
    (name-is-not-in-conf . "%#1s \344r inte n\344rvarande i n\345got m\366te.\n")
    (name-is-in-conf . "%#1s \344r n\344rvarande i\n%#2s\n")
    (connected-during . "Uppkopplingstid: %#1d sekunder.\n")
    ;; +++


    (conf-to-set-permitted-submitters-q . "Vilket m\366te vill du s\344tta till\345tna f\366rfattare f\366r? ")
    (conf-to-set-super-conf-q . "Vilket m\366te vill du s\344tta superm\366te f\366r? ")
    (new-super-conf-q . "Vilket m\366te vill du ha som superm\366te? ")
    (new-permitted-submitters-q . "M\366te med till\345tna f\366rfattare till %#1s? (alla) ")
    (super-conf-for-is . "\304ndra superm\366te f\366r %#1M till %#2M...")
    (permitted-submitters-removed-for-conf . "Till\345t alla f\366rfattare i m\366te %#1M...")
    (submitters-conf-for-is . "\304ndra till\345tna f\366rfattare f\366r m\366te %#1M till\nmedlemmarna i %#2M...")

    (conf-to-set-garb-nice-q . "Vilket m\366te vill du s\344tta livsl\344ngd f\366r? ")
    (new-garb-nice-q . "Vilket v\344rde vill du s\344tta livsl\344ngden till? ")
    (garb-nice-for-is . "\304ndra livsl\344ngden f\366r %#1M till %#2d...")

    (really-shutdown . "\304r du s\344ker p\345 att du vill st\344nga av servern? ")
    (closing-server . "St\344nga av servern...")
    (really-sync . "\304r du s\344ker p\345 att du vill spara databasen? ")
    (syncing-server . "Spara databasen...")
    (administrator . "administrat\366r")
    (no-longer-administrator . "en normal anv\344ndare igen")
    (you-are-now . "Ok, du k\366r nu som %#1s.\n")
    (setting-motd . "S\344tter loginmeddelandet till text %#1n.\n")
    (set-motd-success . "Du har satt ett nytt loginmeddelande.\n")
    (set-motd-failed . "Det gick inte. Du var kanske inte administrat\366r.\n")
    (removing-motd . "Tar bort loginmeddelandet.\n")
    (removed-motd . "Du har tagit bort loginmeddelandet.\n")
    (who-to-throw-out . "Vilken session vill du kasta ut? ")
    (throwing-out . "Kastar nu ut session %#1d... ")
    (postpone-prompt . "Hur lite vill du l\344sa nu? ")
    (set-session-priority . "S\344tt l\344sniv\345: ")

    ;; From review.el:
    (no-review-done . "Du m\345ste \345terse innan du han \345terse mer.\n")
    (review-how-many . "\305terse hur m\345nga?")
    (review-how-many-more . "\305terse ytterligare hur m\345nga?")
    (latest-n . "senaste %#1d")
    (first-n . "f\366rsta %#1d")
    (info-by-whom . "%#1s av vem: ")
    (info-to-conf . "%#1s till m\366te: ")
    (all-confs . "alla m\366ten")
    ;; +++ not used?
    (info-by-to . "%#1s av %#2P till %#3M fram\345t.")
    (no-get-conf . "Du f\345r inte h\344mta m\366tet.\n")
    (no-get-pers . "Du f\345r inte h\344mta personen.\n")
    (no-review-info . "Ej till\345tet \345terse %#1s\n")
    ;; +++
    (review-info . "\305terse %#1s")
    (review-info-by-to . "\305terse %#1s av %#2P till %#3M fram\345t.\n")
    (review-more-info-by-to . "\305terse %#1s av %#2P till %#3M fram\345t.\n")
    (review-rest . "resten")
    (review-more . "n\344sta %#1d")
    (you-review . "Du \345terser nu %#1s.\n")
    (read-text-first . "Du m\345ste l\344sa en text f\366rst.\n")
    (cannot-read-last-text . "Du kan inte l\344sa den senast l\344sta texten.\n")
    (review-n-texts . "\305terse %#1d inl\344gg.\n")
    (review-marked . "\305terse %#1d markerade.\n")
    (review-text-no . "\305terse text nummer %#1n\n")
    (review-one-comment . "\305terse en kommentar till inl\344gg %#1n.\n")
    (review-many-comments . "\304terse %#2d kommentarer till inl\344gg %#1n.\n")
    (read-normally-read . "Hur m\345nga vill du se igen? ")

    (review-conf-gone . "M\366tet finns inte.\n")
    (review-pers-gone . "Personen finns inte.\n")
    (review-cant-read-conf . "Du kan inte \345terse inl\344gg till ett slutet m\366te du inte \344r med i.\n")
    (review-cant-read-letterbox . "Du kan inte \345terse inl\344gg till n\345gon annans brevl\345da.\n")
    (review-cant-read-empty . "M\366tet \344r tomt.\n")
    (cant-review-everything . "Du kan inte \345terse alla texter i LysKOM.\n")

    (more-than-one-root . "Inlägg %#1n har mer än ett urinlägg.\n")
    (more-than-one-root-review . 
"Inlägg %#1n har mer än ett urinlägg, men endast ett träd kommer att visas.\n")

    ;; From edit-text.el:
    (press-C-c-C-c . "Tryck C-c C-c f\366r att skicka in texten.")
    (recipient . "Mottagare:")
    (recipient-prefix . "[Mm]")
    (carbon-copy . "Extra kopia:")
    (blank-carbon-copy . "För kännedom:")
    (carbon-copy-prefix . "[Ee]")
    (blank-carbon-copy-prefix . "[Ff]")
    (header-subject . "\304rende: ")
    (header-separator . "\\<lyskom-edit-mode-map>\
--- Skriv nedan. \
Skicka in=\\[kom-edit-send], \
Avbryt=\\[kom-edit-quit], \
Annat se \\[describe-mode] ---")
    (text-mass . "%#4s%#1s\n%#2s\n%#3s")
    (comment-to-by . "%#1s till text %#2n%#3s.\n")
    (already-sent . "Du har redan skickat denna text en g\345ng. S\344nd \344nd\345? ")
    (subject . "\304rende: ")
    (subject-prefix . "[\304\344\\{\\[]")
    (enter-subject-idi . "Skriv ett \344rende.")
    (which-text-include . "Vilken text skall vi inkludera? ")
    (added-recipient . "Mottagare som skall adderas: ")
    (added-carbon-copy . "Extra kopia till m\366te: ")
    (added-blank-carbon-copy . "För kännedom till: ")
    (text-to-comment-q . "Vilket inlägg vill du kommentera? ")
    (conf-has-motd-no . "M\366tet har en lapp p\345 d\366rren. (%#1d)\n\n%#2s")
    (still-want-to-add . "Vill du fortfarande addera m\366tet? ")
    (could-not-create-text . "\nTexten kunde ej skapas. Felet: %#2s.\n")
    (no-get-text . "Du fick inte h\344mta texten.")
    (unknown-header . "Ok\344nd information p\345 raden")
    (transform-error . "Skicka in oformatterat (%#1s)? ")

    ;; From view-text.el:
    (line . " /1 rad/ ")
    (lines ." /%#1d rader/ ")

    (marked-by-you . "Markerad av dig.\n")
    (marked-by-you-and-one . "Markerad av dig och n\345gon annan.\n")
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
    (yesterday . "ig\345r")
    (no-such-text-no . "Det finns inget s\345dant inl\344gg. (%#1:n)\n")

    (head-Subject . "\304rende: ")
    (Recipient . "Mottagare")
    (Extra-recipient . "Extra kopia")
    (Hidden-recipient . "För kännedom")
    (Strange-recipient . "Underlig mottagare")
    (send-at . "    S\344nt:     %#1s\n")
    (sent-by . "    S\344nt av %#1P\n")
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
Meddelande fr\345n LysKOM-systemet: N\345gon f\366rs\366kte koppla upp,
men misslyckades eftersom alla tillg\344ngliga f\366rbindelser \344r
upptagna. Logga ut och kom tillbaks senare om du v\344ntar nu.
===========================================================\n")
    (has-entered . "Nu har %#1:P g\345tt in i LysKOM.")
    (has-entered-r . "%#2@Nu har %#1P g\345tt in i LysKOM.\n")
    (has-left . "Nu har %#1:P g\345tt ur LysKOM.")
    (has-left-r . "%#2@Nu har %#1P g\345tt ur LysKOM.\n")
    (unknown . "ok\344nd")
    (secret-person . "Hemlig person")

    (message-broadcast . 
		       "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Allm\344nt meddelande fr\345n %#1P (%#3s):

%#2t
----------------------------------------------------------------
")
    (message-from . 
		  "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Personligt meddelande fr\345n %#1P (%#3s):

%#2t
----------------------------------------------------------------
")
    (message-from-to .
		     "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Gruppmeddelande till %#3M\nfr\345n %#2P (%#4s):

%#1t
----------------------------------------------------------------
")
    (text-is-created . "Text %#1n \344r skapad!")

    ;; Used in mode-line-process
    (mode-line-waiting . ": v\344ntar")
    (mode-line-working . ": arbetar")
    (mode-line-saving . ": sparar")
    (mode-line-down . ": nerkopplad")

    ;; From completing-read.el:
	   
    (person-or-conf-no-regexp . "\\`[ \t]*[mpMP]\\w*[ \t]+\\([0-9]+\\)\\'")
    (session-no-regexp . "\\`[ \t]*[sS]\\w*[ \t]+\\([0-9]+\\)\\'")

    ;;n From prioritize.el:

    (cant-move-nothing-nowhere . "Kan inte flytta ingenting n\345gonstans.")
    (goto-priority-prompt . "Hoppa till prioritet: ")
    (priority-prompt . "Ny prioritet för %#1M: ")
    (priority-prompt-marked . "Ny prioritet på markerade möten: ")
    (beginning-of-list . "B\366rjan av listan")
    (end-of-list . "Slutet av listan")
    (reprioritize-from . "Prioritera om fr\345n: ")
    (reprioritize-to . "Prioritera om till: ")
    (no-selection . "Ingen markerad")
    (selection . "%d markerade")

    (cannot-get-membership . "Kan ej h\344mta medlemsskap f\366r dig.")
    (cannot-get-pers-stat . "Kan ej h\344mta personstatus f\366r dig.")
    (prioritize-help .
		     "u,n Flytta m\366te, SPC markera, p prioritera markerade, q avsluta, C-h m hj\344lp")
    (your-priorities . " Prioritet  M\366tesnamn
----------------------------------------------------------------------------
")
    (your-membship . "Ditt medlemsskap i olika KOM-m\366ten:
  Prio M\366tesnr M\366tesnamn\n")
    (prio-row . " %5#1d%5#2m  %#3M\n")
    (too-high-goto-2 . "Du st\345r f\366r h\366gt upp. G\345 ner till rad 2.")
    (too-low-go-up . "Du kan inte pusha sista raden. G\345 upp en rad.")
    (all-confs-popped .  "Alla m\366ten \344r poppade.")
    (prio-died . "Flyttningen misslyckades. Sorry. D\366da bufferten.")
    (new-priority . "Ny prioritet? (0 (l\345g) - 255 (h\366g)) ")
    (new-prio . "%6#1d")

    ;; From flags.el:
    (saving-settings . "Sparar inst\344llningarna...")
    (saving-settings-done . "Sparar inst\344llningarna...klart")
    (hang-on . "V\344nta ett tag...\n")
    (no-changes . "Ingenting beh\366vde sparas eftersom inga variabler hade \344ndrats.\n")
    (could-not-save-options . "Kunde ej spara inst\344llningarna.\n")
    (could-not-create-area . "Kunde ej skapa texten.\n")
    (could-not-set-user-area . "Kunde ej st\344lla om user-arean. Servern s\344ger felmeddelande: %#1d\n")
    (you-dont-exist . "Du finns inte.\n")
    (error-in-options . "Det fanns ett fel i en av dina variabler (%#1s)
Det stod \"%#2s\" i user-arean. Den s\344tts till nil ist\344llet.
Skicka en bugrapport.\n")

    ;; From elib-string.el:
    ;; No entries.
		
    ;; From lyskom-rest.el:
		
    (mode-line-unread . " Ol\344sta ")
    (mode-line-letters . "brev ")

    (error-code . "Felkod %#2d: %#1s.\n")
    (error-in-kom-do-when-done . "Variabeln kom-do-when-done har ett felaktigt v\344.
Du b\366r s\344tta den till ett b\344ttre v\344rde.\n")
    (extended-command . "LysKOM: ")
    (wait-for-server . "LysKOM v\344ntar p\345 svar fr\345n servern. V\344nta tills du f\345r en prompt.\n")
    (review-text-q . "\305terse text nummer: ")

    (completely-read-conf . "Du har sett alla texter i detta m\366te.\n")
    (not-in-any-conf . "Du l\344ser inte n\345got m\366te just nu.\n")

    (all-conf-unread-r . "Du har l\344st ut alla m\366ten.\n")
    (all-conf-unread-s . "Du har l\344st ut alla m\366ten. ")
    (one-unread . "%#1M - 1 ol\344st\n")
    (several-unread . "%#1M - %#2d ol\344sta\n")
    (enter-conf . "%#1M\n")

    (save-on-file-q . "Spara inl\344gg p\345 fil: (%#1s) ")
    (wait-for-prompt . "V\344nta p\345 prompten.")

    (conference-no . "<möte %#1d>")
    (person-no . "<person %#1d>")
    (prompt-several-messages . "(%d meddelanden)")
    (prompt-single-message   . "(%d meddelande)")

    (go-to-pri-conf-prompt . "G\345 till n\344sta prioriterade m\366te")
    (read-pri-text-conf . "L\344sa n\344sta prioriterade text")
    (review-next-text-prompt . "\305terse n\344sta text")
    (review-next-comment-prompt . "\305terse n\344sta kommentar")
    (review-next-marked-prompt . "\305terse n\344sta markerade")
    (read-next-letter-prompt . "L\344sa n\344sta brev")
    (read-next-footnote-prompt . "L\344sa n\344sta fotnot")
    (read-next-comment-prompt . "L\344sa n\344sta kommentar")
    (read-next-text-prompt . "L\344sa n\344sta text")
    (go-to-conf-of-marked-prompt . "\305terse (n\344sta) markerade")
    (go-to-next-conf-prompt . "G\345 till n\344sta m\366te")
    (go-to-your-mailbox-prompt . "G\345 till din brevl\345da")
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
    (conference-does-not-exist . "M\366te %#1d (finns inte).")
    (person-is-anonymous . "Anonym person")

    (process-signal . "Signal fr\345n processen.")
    (closed-connection . "
**************************************************
%#2s
Lyskom-sessionen onormalt st\344ngd.
Felmeddelande: %#1s**************************************************")
    (dead-session . "LysKOM-sessionen \344r inte aktiv.")
    (error-not-found . "Fel nummer %#1d. Ingen klartextf\366rklaring finns.")

    ;; Useful in more place than one:
    (illegal-command . "Otill\345tet kommando.\n")
    (no-such-text . "Det finns inget s\345dant inl\344gg.\n")
    (no-such-text-m . "Det finns inget s\345dant inl\344gg.")
    (everybody . "alla")
    (everything . "allt")
    (anybody . "vem som helst")
    (forward . "fram\345t")
    (backward . "bak\345t")
    (wait . "V\344nta ett tag...\n")
    (comment . "Kommentar")
    (comment-prefix . "[Kk]")
    (footnote . "Fotnot")
    (footnote-prefix . "[Ff]")

    (by . " av %#1P")
    (text-created .  "Text nummer %#1n \344r skapad.\n")

    (resolve-session . "Ange vilken session: ")

    (starting-program . "Startar %#1s...")
    (super-jump . "Filtrerar \344rende \"%#1r\" i m\366te \"%#2M\"\n")
    (no-recipient . "Inl\344gget har ingen mottagare.\n")
    (filtered . "[Filtrerad]")
    (filter-error-specification . "Fel i filterspecifikationen")
    (filter-error-bad-not . "Fel i filterspecifikation efter 'not'")
    (filter-error-unknown-key . "Filternyckeln '%S' \344r ok\344nd.")
    (filter-error-key-arg . "Fel filterdata (%S %S)")
    (filter-tree . "Hoppar \366ver text %#1n \"%#2r\" av %#3P och dess kommentarstr\344d.\n")
    (filter-text . "Hoppar \366ver text %#1n \"%#2r\" av %#3P.\n")
    (filter-permanent . "Permanent? ")
    (filter-action . "Hur vill du filtrera? ")
    (filter-in-conf . "I vilket m\366te? (alla) ")
    (filter-subject . "Filtrera vilket \344rende? ")
    (filter-which-text . "Filtrera inl\344gg som inneh\345ller: ")
    (filter-author . "Filtrera vilken f\366rfattare? ")
    (permanent . "(permanent)")
    (temporary . "(tillf\344llig)")
    (filter-edit-buffer-name . "*LysKOM Filter Edit*")
    (filter-edit-empty-list . "Listan \344r tom")
    (filter-edit-start-of-list . "Listans b\366rjan")
    (filter-edit-end-of-list . "Listans slut")
    (filter-edit-filter-how . "Hur vill du filtrera? ")
    (filter-edit-filter-what . "Vad vill du filtrera? ")
    (filter-edit-bad-argument . "Felaktig inmatning: %s")
    (filter-edit-outside-entry . "Kan inte utf\366ra kommandot utanf\366r ett filter")
    (filter-edit-outside-list . "Kan inte utf\366ra operationen utanf\366r listan")
    (filter-edit-end-of-pattern . "Filtrets slut")
    (filter-edit-save-p . "Spara f\366r\344ndringar? ")
    (filter-edit-remove-empty . "Tomma filter g\366r att alla texter filtreras. Vill du ta bort dessa? ")
    (filter-edit-restart-p . "Du har gjort \344ndringar. Vill du verkligen b\366rja om? ")
    (filter-edit-help . 
		      "p,n Upp/ned, i/M-i Ny rad/filter, d/M-d Radera rad/filter, C-h m Mer hj\344lp")
    (filter-edit-header . "\304ndra filter f\366r \"%s\"\n")
    (filter-edit-saving . "Sparar \344ndringarna...")
    (filter-edit-saving-done . "Sparar \344ndringarna...klart")
    (filter-edit-saving-error . "Kunde inte spara \344ndringarna!")
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
    (ansaphone-state . "Automatsvar \344r nu %#1s.")
    (ansaphone-state-r . "Automatsvar \344r nu %#1s.\n")
    (ansaphone-messages-gone . "Sparade meddelanden raderade.")
    (ansaphone-no-messages . "Inga meddelanden.\n")
    (ansaphone-message-list-start . "Sparade meddelanden:\n\n")
    (ansaphone-message-list-end . "\n\n")
    (ansaphone-message-header . "Automatiskt svar (satt %#1s):\n")

    (remote-erase-messages . "Fj\344rrstyrning (%#1P %#2s): Sparade meddelanden raderade\n")
    (remote-set-message . "Fj\344rrstyrning (%#1P %#2s): Svarsmeddelande:
----------------------------------------------------------------------
%#3t
----------------------------------------------------------------------
")
    (remote-set-ansaphone . "Fj\344rrstyrning (%#1P %#2s): Automatsvar \344r nu %#3s\n")
    (remote-list-messages . "Fj\344rrstyrning (%#1P %#2s): Meddelanden listade\n")
    (remote-quit . "Fj\344rrstyrning(%#1P %#2s): Avsluta\n")

    (illegal-remote . 
		    "Otill\345ten fj\344rrstyrning:
Tid: %#1s
Fr\345n: %#2P <%#2p>
Till: %#3P <%#3p>
Text: 
%#4t")
    (illegal-remote-reply . "Fj\344rrstyrning inte accepterad: %#1s") 
    (remote-not-in-list . "Otill\345ten person")
    (remote-bad-command . "Felaktigt kommando")
    (remote-unknown-error . "Ok\344nt fel")

    (remote-control-who . "Kontrollera vilken session? ")
    (remote-control-autoreply . "Automatsvar p\345 eller av? ")

    (state-on . "p\345slaget")
    (state-off . "avslaget")

    (text-popup-title . "Inl\344gg %#1s")
    (conf-popup-title . "M\366te %#1s")
    (pers-popup-title . "Person %#1s")
    (url-popup-title  . "URL %#1s")
    (generic-popup-title . "%#1s")

    (who-i-am-not-present . "%#1P \344r inte n\344rvarande i n\345got m\366te\n")
    (who-i-am-present . "%#1P \344r n\344rvarande i %#2M\n")
    (who-i-am-client . "Programmet heter lyskom.el, version %#1s.\n")
    (who-i-am-server . "Detta \344r %#1s, version %#2s.\n")
    (who-i-am-emacs . "Det k\366rs under %#1s.\n")

    (no-such-session-r . "Det finns ingen s\345dan session. Personen kanske inte \344r inloggad.\n")
    (person-not-logged-in-r . "%#1P \344r inte inloggad.\n")
    (session-status . "Session %#1d \344r %#2P <%#2p>
%#5s %#7s %#4M
K\366r %#6D fr\345n %#3s\n")
    (session-status-9 . "Session %#1d \344r %#2P <%#2p>
%#5s %#7s %#4M
K\366r %#6D fr\345n %#3s
Uppkopplad sedan %#8s%#9s")
    (session-status-inactive . "\nHar inte varit aktiv p\345 %#1s\n")
    (one-day . "en dag")
    (one-hour . "en timme")
    (one-minute . "en minut")
    (days . "dagar")
    (hours . "timmar")
    (minutes . "minuter")
    (and . "och")
    (session-is-active . " och \344r aktiv.\n")
    (session-is-invisible . "Denna session \344r osynlig.\n")
    (status-for-session . "Sessionsstatus f\366r vilken person? ")
    (unknown-doing-what . "Existerar")
    (doing-where-conn . "i")
    (doing-nowhere-conn . "men \344r")
    (waiting-for-membership . "V\344ntar p\345 att medlemskapslistan ska l\344sas in...%d/%d")

	   ;; From slow.el
    (no-such-command . "Det finns inget s\345dant kommando.\n")
    (command-completions . "Du kan mena n\345gon av f\366ljande:\n %#1s\n")
    (which-language . "Ändra språk till: ")
    (send-formatted . "Skicka in som formatterad text? ")
    (changing-language-to . "Byter till %#1s.\n")
    (language-not-loaded . "%#1s finns inte tillgängligt.\n")

    (reformat-html . "(HTML)")
    (reformat-enriched . "(Enriched)")
    (reformat-filled . "(Ombruten)")
    
    (need-library . "Paketet \"%#1s\" behövs för att utföra detta kommando.\n")
    (calc-expression . "Uttryck: ")
))




;;; ================================================================
;;;              The commands and their associated functions



;;; The Alist formely known as lyskom-commands
(lyskom-language-strings lyskom-command sv
  '(
    (describe-mode            . "Hj\344lp")
    (kom-slow-mode            . "L\345ngsamma kommandon")
    (kom-quick-mode           . "Snabba kommandon")
    (kom-send-message         . "S\344nda meddelande")
    (kom-create-conf	      . "Skapa m\366te")
    (kom-delete-conf          . "Utpl\345na")
    (kom-delete-text          . "Radera inl\344gg")
    (kom-display-time	      . "Se tiden")
    (kom-go-to-conf	      . "G\345 till m\366te")
    (kom-go-to-next-conf      . "G\345 till n\344sta m\366te")
    (kom-jump		      . "Hoppa \366ver alla kommentarer")
    (kom-list-conferences     . "Lista m\366ten") 
    (kom-list-persons	      . "Lista personer")
    (kom-list-news            . "Lista nyheter")
    (kom-list-re              . "Lista (med) regexpar")
    (kom-membership	      . "Lista medlemsskap")
    ;; (kom-list-marks		"Lista markeringar") 
    (kom-postpone	      . "Uppskjuta l\344sning")
    (kom-set-session-priority . "S\344tt l\344sniv\345")
    (kom-prioritize	      . "Prioritera m\366ten")
    (kom-status-person	      . "Status (f\366r) person")
    (kom-status-conf	      . "Status (f\366r) m\366te")
    (kom-add-self	      . "Bli medlem i m\366te")
    (kom-list-summary	      . "Lista \344renden")
    (kom-sub-self             . "Uttr\344da ur m\366te")
    (kom-quit		      . "Sluta")
    (kom-recover	      . "\305terstarta kom") 
    (kom-start-anew	      . "B\366rja med nytt namn")
    (kom-view		      . "\305terse inl\344gg")
    (kom-find-root-review     . "\305terse tr\344d")
    (kom-review-comments      . "\305terse alla kommentarer")
    (kom-review-tree	      . "\305terse alla kommentarer rekursivt")
    (kom-review-clear         . "\305terse hoppa")
    (kom-review-last-normally-read
     			      . "\305terse igen")
    (kom-review-noconversion  . "\305terse omodifierat")
    (kom-review-next          . "\305terse n\344sta")
    (kom-find-root	      . "\305terse urinl\344gget")
    (kom-review-by-to         . "\305terse senaste")
    (kom-review-more          . "\305terse fler inl\344gg")
    (kom-review-first         . "\305terse f\366rsta")
    (kom-review-all           . "\305terse alla")
    (kom-view-commented-text  . "\305terse det kommenterade")
    (kom-view-previous-commented-text
     			      . "\305terse det f\366reg\345ende kommenterade")
    (kom-review-stack         . "\305terse lista")
    (kom-review-presentation  . "\305terse presentation")
    (kom-review-backward      . "(\305terse) Bakl\344nges")
    (kom-view-next-text	      . "L\344sa n\344sta inl\344gg")
    (kom-who-is-on	      . "Vilka \344r inloggade") 
    (kom-who-am-i             . "Var (\344r) jag")
;    (kom-display-who-buffer	"Visa vilkalistan")
    (kom-list-clients	      . "Lista klienter")
    (kom-busy-wait	      . "V\344nta p\345 ett inl\344gg")
    (kom-write-comment	      . "Kommentera inl\344gget")
    (kom-comment-previous     . "Kommentera f\366reg\345ende inl\344gg")
    (kom-write-footnote       . "Fotnot till inl\344gg")
    (kom-private-answer       . "Personligt svar")
    (kom-private-answer-previous
     . "Personligt svar p\345 f\366reg\345ende inl\344gg")
    (kom-set-unread	      . "Endast l\344sa senaste")
    (kom-write-text	      . "Skriva ett inl\344gg")
    (kom-send-letter	      . "Skicka brev")
    (kom-change-name	      . "\304ndra namn")
    (kom-change-password      . "\304ndra l\366senord")
    (kom-change-supervisor    . "\304ndra organisat\366r")
    (kom-change-presentation  . "\304ndra presentation")
    (kom-get-appreciation     . "F\345 uppmuntran")
    (kom-get-abuse            . "F\345 sk\344ll")
    (kom-mark-text            . "Markera (inl\344gg)")
    (kom-unmark-text          . "Avmarkera (inl\344gg)")
    (kom-review-marked-texts  . "\305terse markerade")
    (kom-review-all-marked-texts . "\305terse alla markerade")
    (kom-add-recipient        . "Addera mottagare")
    (kom-add-copy	      . "Addera extra kopiemottagare")
    (kom-add-bcc	      . "Addera för kännedom")
    (kom-sub-recipient        . "Subtrahera mottagare")
    (kom-move-text            . "Flytta inl\344gg")
    (kom-add-comment	      . "Addera kommentar")
    (kom-sub-comment	      . "Subtrahera kommentar")
    (kom-add-member	      . "Addera medlem")
    (kom-sub-member           . "Uteslut medlem")
    (kom-change-conf-motd     . "S\344tt lapp p\345 d\366rren")
    (kom-set-garb-nice        . "\304ndra livsl\344ngd")
    (kom-set-super-conf       . "\304ndra superm\366te")
    (kom-set-permitted-submitters . "\304ndra till\345tna f\366rfattare")
    (kom-unset-conf-motd      . "Ta bort lapp p\345 d\366rren")
    (kom-save-text	      . "Spara text (p\345 fil)")
    (kom-edit-options	      . "\304ndra variabler")
    (kom-save-options         . "Spara variabler")
    (kom-shutdown-server      . "St\344ng av servern")
    (kom-sync-database        . "Spara databasen")
    (kom-enable-adm-caps      . "\326verg\345 till administrat\366rsmod")
    (kom-disable-adm-caps     . "\326verg\345 till normalmod")
    (kom-set-motd             . "S\344tt loginmeddelande")
    (kom-remove-motd          . "Ta bort loginmeddelande")
    (kom-force-logout         . "Kasta ut en session")
    (kom-filter-author        . "Filtrera f\366rfattare")
    (kom-filter-subject       . "Filtrera \344rende")
    (kom-filter-text          . "Filtrera inneh\345ll")
    (kom-super-jump           . "Superhoppa")
    (kom-filter-edit          . "\304ndra filter")
    (kom-list-filters         . "Lista filter")
    (kom-show-user-area       . "Visa user-arean")
    (kom-change-conf-type     . "\304ndra m\366testyp")

    (kom-change-auto-reply    . "\304ndra svarsmeddelande")
    (kom-toggle-auto-reply    . "Automatsvar")
    (kom-list-messages        . "Lista meddelanden")
    (kom-erase-messages       . "Radera meddelanden")

    (kom-remote-autoreply     . "Fj\344rrkontrollera automatsvar")
    (kom-remote-set-message   . "Fj\344rrkontrollera \344ndra svarsmeddelande")
    (kom-remote-list-messages . "Fj\344rrkontrollera lista meddelanden")
    (kom-remote-erase-messages . "Fj\344rrkontrollera radera meddelanden")
    (kom-remote-quit          . "Fj\344rrkontrollera avsluta")

    (kom-status-session       . "Status (f\366r) session")
    (kom-customize            . "Inställningar (för) LysKOM")
    (kom-next-kom             . "Nästa LysKOM")
    (kom-previous-kom         . "Föregående LysKOM")
    (kom-next-unread-kom      . "Nästa olästa LysKOM")
    (kom-change-language      . "Ändra språk")
    (kom-calculate            . "Beräkna")
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
    (read      . "L\344s")
    (dont-read . "Hoppa")
    (write     . "Skriv")
    (conference . "M\366te")
    (person    . "Person")
    (other     . "Annat")
    (move      . "G\345")
    (info      . "Om")
    (send      . "S\344nd")
    (recievers . "Mottagare")
    (commented . "Kommenterar")
    (kom-edit-send . "Skicka in")
    (kom-edit-send-anonymous . "Skicka anonymt")
    (kom-edit-quit . "Kasta bort") 
    (kom-ispell-message . "Stavningskontroll")
    (kom-edit-add-recipient . "Addera mottagare")
    (kom-edit-add-copy . "Addera extra kopiemottagare")
    (kom-edit-show-commented . "\305terse det kommenterade")
    (kom-edit-insert-commented . "Citera det kommenterade")))

;;(defvar lyskom-swascii-commands nil
;;  "The swascii-versions of lyskom-commands.")

(lyskom-language-var lyskom-onoff-table sv
  '(("p\345" . on) ("av" . off)))

(lyskom-language-var lyskom-filter-predicate-list sv
      '(("=" . nil) ("!=" . t)))

(lyskom-language-var lyskom-filter-what sv
      '((author . "F\366rfattare")
        (author-no . "F\366rfattare (nummer)")
        (author-re . "F\366rfattare (regexp)")
        (subject . "\304rende")
        (subject-re . "\304rende (regexp)")
        (recipient . "Mottagare")
        (recipient-no . "Mottagare (nummer)")
        (recipient-re . "Mottagare (regexp)")
        (text . "Inneh\345ll")
        (text . "Inneh\345ll (regexp)")))

(lyskom-language-var lyskom-filter-actions sv
      '((skip-text . "Hoppa \366ver")
        (dontshow . "Visa inte")
        (skip-tree . "Hoppa \366ver kommentarer")))
                               
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
  (define-prefix-command 'lyskom-sv-filter-get-prefix)

  (define-key lyskom-sv-mode-map "f" 'lyskom-sv-filter-get-prefix)
  (define-key lyskom-sv-mode-map "n" 'lyskom-sv-next-prefix)
  (define-key lyskom-sv-mode-map "l" 'lyskom-sv-list-prefix)
  (define-key lyskom-sv-mode-map "s" 'lyskom-sv-S-prefix)

  ;; emacs 19

  (define-key lyskom-sv-mode-map (lyskom-keys [ä]) 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map (lyskom-keys [Ä]) 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map (lyskom-keys [Å]) 'lyskom-sv-review-prefix)
  (define-key lyskom-sv-mode-map (lyskom-keys [å]) 'lyskom-sv-review-prefix)
  (define-key lyskom-sv-mode-map (lyskom-keys [f ä]) 'kom-filter-subject)
  (define-key lyskom-sv-mode-map (lyskom-keys [f Ä]) 'kom-filter-subject)
  (define-key lyskom-sv-mode-map (lyskom-keys [l ä]) 'kom-list-summary)
  (define-key lyskom-sv-mode-map (lyskom-keys [l Ä]) 'kom-list-summary)
  (define-key lyskom-sv-mode-map [ä] 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map [Ä] 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map [Å] 'lyskom-sv-review-prefix)
  (define-key lyskom-sv-mode-map [å] 'lyskom-sv-review-prefix)
  (define-key lyskom-sv-mode-map [f ä] 'kom-filter-subject)
  (define-key lyskom-sv-mode-map [f Ä] 'kom-filter-subject)
  (define-key lyskom-sv-mode-map [l ä] 'kom-list-summary)
  (define-key lyskom-sv-mode-map [l Ä] 'kom-list-summary)
  (define-key lyskom-sv-mode-map (lyskom-keys [mouse-2]) 'kom-button-click)
  (define-key lyskom-sv-mode-map (lyskom-keys [down-mouse-3]) 'kom-popup-menu)
  (define-key lyskom-sv-mode-map [mouse-3] 'kom-mouse-null)
  (define-key lyskom-sv-mode-map "*" 'kom-button-press)
  (define-key lyskom-sv-mode-map "\C-i" 'kom-next-link)
  (define-key lyskom-sv-mode-map "\M-\C-i" 'kom-previous-link)

  (define-key lyskom-sv-mode-map "{" 'lyskom-sv-change-prefix) ; krullar
  (define-key lyskom-sv-mode-map "[" 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map "}" 'lyskom-sv-review-prefix)
  (define-key lyskom-sv-mode-map "]" 'lyskom-sv-review-prefix)
  ;; (define-key lyskom-sv-mode-map "\344" 'lyskom-sv-change-prefix) ; 8-bit keymap
  ;; (define-key lyskom-sv-mode-map "\304" 'lyskom-sv-change-prefix)
  ;; (define-key lyskom-sv-mode-map "\345" 'lyskom-sv-review-prefix)
  ;; (define-key lyskom-sv-mode-map "\305" 'lyskom-sv-review-prefix)
  ;; (define-key lyskom-sv-mode-map "\M-{" 'lyskom-sv-change-prefix)
  ;; (define-key lyskom-sv-mode-map "\M-[" 'lyskom-sv-change-prefix) 
  ;; (define-key lyskom-sv-mode-map "\M-}" 'lyskom-sv-review-prefix)
  ;; (define-key lyskom-sv-mode-map "\M-]" 'lyskom-sv-review-prefix) 



  ;;(define-key lyskom-sv-mode-map "vi" 'vilka)
  
  ;; These should be first in order to be last in the menu of alternatives.
  (define-key lyskom-sv-mode-map "{?" 'lyskom-help)
  (define-key lyskom-sv-mode-map "}?" 'lyskom-help)
  (define-key lyskom-sv-mode-map "f?" 'lyskom-help)
  (define-key lyskom-sv-mode-map "n?" 'lyskom-help)
  (define-key lyskom-sv-mode-map "l?" 'lyskom-help)
  (define-key lyskom-sv-mode-map "s?" 'lyskom-help)
  

  (define-key lyskom-sv-mode-map "e" 'kom-set-unread)
  (define-key lyskom-sv-mode-map "a" 'kom-extended-command)
  (define-key lyskom-sv-mode-map " " 'kom-next-command)
  (define-key lyskom-sv-mode-map "\n" 'kom-page-next-command)
  (define-key lyskom-sv-mode-map "\r" 'kom-line-next-command)
  (define-key lyskom-sv-mode-map "j" 'kom-page-next-command)

  (define-key lyskom-sv-mode-map "?"  'describe-mode)
  (define-key lyskom-sv-mode-map "b"  'kom-send-letter)
  (define-key lyskom-sv-mode-map "g"  'kom-go-to-conf)
  (define-key lyskom-sv-mode-map "i"  'kom-write-text)
  (define-key lyskom-sv-mode-map "k"  'kom-write-comment)
  (define-key lyskom-sv-mode-map "K"  'kom-comment-previous)
  (define-key lyskom-sv-mode-map "F"  'kom-write-footnote)
  (define-key lyskom-sv-mode-map "p"  'kom-private-answer)
  (define-key lyskom-sv-mode-map "P"  'kom-private-answer-previous)
  (define-key lyskom-sv-mode-map "h"  'kom-jump)
  (define-key lyskom-sv-mode-map "H"  'kom-super-jump)
  (define-key lyskom-sv-mode-map "lm" 'kom-list-conferences)
  (define-key lyskom-sv-mode-map "ln" 'kom-list-news)
  (define-key lyskom-sv-mode-map "lp" 'kom-list-persons)
  (define-key lyskom-sv-mode-map "lr" 'kom-list-re)
  (define-key lyskom-sv-mode-map "ls" 'kom-membership)
  (define-key lyskom-sv-mode-map "l{" 'kom-list-summary)
  (define-key lyskom-sv-mode-map "l[" 'kom-list-summary)
  ;; (define-key lyskom-sv-mode-map "l\344" 'kom-list-summary) ; 8-bit emacs
  ;; (define-key lyskom-sv-mode-map "l\304" 'kom-list-summary)
  ;; (define-key lyskom-sv-mode-map "l\M-{" 'kom-list-summary) ; 7(8)-bit emacs
  ;; (define-key lyskom-sv-mode-map "l\M-[" 'kom-list-summary)
  (define-key lyskom-sv-mode-map "lf" 'kom-list-filters)
  (define-key lyskom-sv-mode-map "m"  'kom-add-self)
  (define-key lyskom-sv-mode-map "M"  'kom-mark-text)
  (define-key lyskom-sv-mode-map "A"  'kom-unmark-text)
  (define-key lyskom-sv-mode-map "ni" 'kom-view-next-new-text)
  (define-key lyskom-sv-mode-map "nm" 'kom-go-to-next-conf)
  (define-key lyskom-sv-mode-map "nl" 'kom-next-kom)
  (define-key lyskom-sv-mode-map "no" 'kom-next-unread-kom)
  (define-key lyskom-sv-mode-map "fl" 'kom-previous-kom)
  (define-key lyskom-sv-mode-map "S"  'kom-quit)
  (define-key lyskom-sv-mode-map "q"  'kom-quit)
  (define-key lyskom-sv-mode-map "z"  'kom-bury)
  (define-key lyskom-sv-mode-map "R"  'kom-recover)
  (define-key lyskom-sv-mode-map "t"  'kom-display-time)
  (define-key lyskom-sv-mode-map "fu" 'kom-get-appreciation)
  (define-key lyskom-sv-mode-map "fs" 'kom-get-abuse)
  (define-key lyskom-sv-mode-map "ft" 'kom-move-text)
  (define-key lyskom-sv-mode-map "f{" 'kom-filter-subject)
  (define-key lyskom-sv-mode-map "f[" 'kom-filter-subject)
  ;; (define-key lyskom-sv-mode-map "f\344" 'kom-filter-subject)
  ;; (define-key lyskom-sv-mode-map "f\304" 'kom-filter-subject)
  ;; (define-key lyskom-sv-mode-map "f\M-{" 'kom-filter-subject)
  ;; (define-key lyskom-sv-mode-map "f\M-[" 'kom-filter-subject)
  (define-key lyskom-sv-mode-map "ff" 'kom-filter-author)
  (define-key lyskom-sv-mode-map "fi" 'kom-filter-text)
  (define-key lyskom-sv-mode-map "v"  'kom-who-is-on)
  (define-key lyskom-sv-mode-map "J"  'kom-who-am-i)
  (define-key lyskom-sv-mode-map "V"  'kom-busy-wait)
  (define-key lyskom-sv-mode-map "{p" 'kom-change-presentation)
  (define-key lyskom-sv-mode-map "{f" 'kom-filter-edit)
  (define-key lyskom-sv-mode-map "{m" 'kom-change-auto-reply)
  (define-key lyskom-sv-mode-map "} " 'kom-view)
  (define-key lyskom-sv-mode-map "}0" 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map "}1" 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map "}2" 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map "}3" 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map "}4" 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map "}5" 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map "}6" 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map "}7" 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map "}8" 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map "}9" 'kom-initial-digit-view)
  (define-key lyskom-sv-mode-map "}k" 'kom-view-commented-text)
  (define-key lyskom-sv-mode-map "}K" 'kom-view-previous-commented-text)
  (define-key lyskom-sv-mode-map "}a?" 'lyskom-help)
  (define-key lyskom-sv-mode-map "}ak" 'kom-review-comments)
  (define-key lyskom-sv-mode-map "}ar" 'kom-review-tree)
  (define-key lyskom-sv-mode-map "}h" 'kom-review-clear)
  (define-key lyskom-sv-mode-map "}i" 'kom-review-last-normally-read)
  (define-key lyskom-sv-mode-map "}n" 'kom-review-next)
  (define-key lyskom-sv-mode-map "}o" 'kom-review-noconversion)
  (define-key lyskom-sv-mode-map "}r" 'kom-find-root)
  (define-key lyskom-sv-mode-map "}u" 'kom-find-root)
  (define-key lyskom-sv-mode-map "}s" 'kom-review-by-to)
  (define-key lyskom-sv-mode-map "}y" 'kom-review-more)
  (define-key lyskom-sv-mode-map "}A" 'kom-review-all)
  (define-key lyskom-sv-mode-map "}f" 'kom-review-first)
  (define-key lyskom-sv-mode-map "}l" 'kom-review-stack)
  (define-key lyskom-sv-mode-map "}p" 'kom-review-presentation)
  (define-key lyskom-sv-mode-map "}t" 'kom-find-root-review)
  (define-key lyskom-sv-mode-map "}m" 'kom-review-marked-texts)
  (define-key lyskom-sv-mode-map "}am" 'kom-review-all-marked-texts)
  (define-key lyskom-sv-mode-map "}a " 'kom-review-all)
  (define-key lyskom-sv-mode-map "B"  'kom-review-backward)
  (define-key lyskom-sv-mode-map "sm" 'kom-status-conf)
  (define-key lyskom-sv-mode-map "sp" 'kom-status-person)
  (define-key lyskom-sv-mode-map "ss" 'kom-status-session)

  ;; Running in buffer

  (define-key lyskom-sv-mode-map "\M-p" 'backward-text)
  (define-key lyskom-sv-mode-map "\M-n" 'forward-text)
  (define-key lyskom-sv-mode-map "st" 'kom-save-text)

  (define-key lyskom-sv-mode-map "\C-?" 'scroll-down)
)





;;;==============================================================
;;; Keymap for filter editing
;;;

(defvar lyskom-sv-filter-edit-map nil)
(lyskom-language-keymap lyskom-filter-edit-map sv lyskom-sv-filter-edit-map)

(if lyskom-sv-filter-edit-map ()
  (setq lyskom-sv-filter-edit-map (make-keymap))
  (suppress-keymap lyskom-sv-filter-edit-map)
  (define-key lyskom-sv-filter-edit-map "p" 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-sv-filter-edit-map "P" 'lyskom-filter-edit-prev-entry)
  (define-key lyskom-sv-filter-edit-map "n" 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-sv-filter-edit-map "N" 'lyskom-filter-edit-next-entry)
  (define-key lyskom-sv-filter-edit-map "\C-P" 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-sv-filter-edit-map "\C-N" 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-sv-filter-edit-map "\C-B" 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-sv-filter-edit-map "\C-F" 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-sv-filter-edit-map "\M-p" 'lyskom-filter-edit-prev-entry)
  (define-key lyskom-sv-filter-edit-map "\M-n" 'lyskom-filter-edit-next-entry)
  (define-key lyskom-sv-filter-edit-map "d" 'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-sv-filter-edit-map "\M-d" 'lyskom-filter-edit-delete-entry)
  (define-key lyskom-sv-filter-edit-map "D" 'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-sv-filter-edit-map "\C-D" 'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-sv-filter-edit-map "i" 'lyskom-filter-edit-insert-pattern)
  (define-key lyskom-sv-filter-edit-map "I" 'lyskom-filter-edit-insert-pattern)
  (define-key lyskom-sv-filter-edit-map "\M-i" 'lyskom-filter-edit-insert-entry)
  (define-key lyskom-sv-filter-edit-map "<" 'lyskom-filter-edit-beginning-of-list)
  (define-key lyskom-sv-filter-edit-map ">" 'lyskom-filter-edit-end-of-list)
  (define-key lyskom-sv-filter-edit-map "\M-<" 'lyskom-filter-edit-beginning-of-list)
  (define-key lyskom-sv-filter-edit-map "\M->" 'lyskom-filter-edit-end-of-list)
  (define-key lyskom-sv-filter-edit-map "q" 'lyskom-filter-edit-quit)
  (define-key lyskom-sv-filter-edit-map "x" 'lyskom-filter-edit-expunge)
  (define-key lyskom-sv-filter-edit-map "s" 'lyskom-filter-edit-save)
  (define-key lyskom-sv-filter-edit-map "g" 'lyskom-filter-edit-revert)
  (define-key lyskom-sv-filter-edit-map "t" 'lyskom-filter-edit-toggle-permanent)
  (define-key lyskom-sv-filter-edit-map "a" 'lyskom-filter-edit-toggle-action)
  (define-key lyskom-sv-filter-edit-map "?" 'lyskom-filter-edit-brief-help)
  (define-key lyskom-sv-filter-edit-map "h" 'lyskom-filter-edit-brief-help)
  )


;;;(if lyskom-prioritize-mode-map
;;;    nil
;;;  (setq lyskom-prioritize-mode-map (make-keymap))
;;;  (suppress-keymap lyskom-prioritize-mode-map)
;;;  (define-key lyskom-prioritize-mode-map [mouse-2] 'kom-button-click)
;;;  (define-key lyskom-prioritize-mode-map "\C-?" 'previous-line)
;;;  (define-key lyskom-prioritize-mode-map " "    'next-line)
;;;  (define-key lyskom-prioritize-mode-map "\C-k" 'kom-prioritize-kill)
;;;  (define-key lyskom-prioritize-mode-map "\C-y" 'kom-prioritize-yank)
;;;  (define-key lyskom-prioritize-mode-map "p"     'kom-prioritize-set-priority)
;;;  (define-key lyskom-prioritize-mode-map "\C-c\C-c" 'kom-prioritize-quit)
;;;  (define-key lyskom-prioritize-mode-map "q"     'kom-prioritize-quit)
;;;  (define-key lyskom-prioritize-mode-map "S"     'kom-prioritize-quit)
;;;  (define-key lyskom-prioritize-mode-map "u"     'kom-prioritize-move-up)
;;;  (define-key lyskom-prioritize-mode-map "n"     'kom-prioritize-move-down)
;;;)

(defvar lyskom-sv-prioritize-mode-map nil)
(lyskom-language-keymap lyskom-prioritize-mode-map sv 
  lyskom-sv-prioritize-mode-map)

(if lyskom-sv-prioritize-mode-map 
    nil
  (setq lyskom-sv-prioritize-mode-map (make-keymap))
  (suppress-keymap lyskom-sv-prioritize-mode-map)
  (define-key lyskom-sv-prioritize-mode-map (lyskom-keys [mouse-2]) 'kom-button-click)
  (define-key lyskom-sv-prioritize-mode-map (lyskom-keys [down-mouse-3]) 'kom-popup-menu)
  (define-key lyskom-sv-prioritize-mode-map [mouse-3] 'kom-mouse-null)
  (define-key lyskom-sv-prioritize-mode-map "*" 'kom-button-press)
  (define-key lyskom-sv-prioritize-mode-map "?" 'kom-prioritize-help)
  (define-key lyskom-sv-prioritize-mode-map "\C-k" 'kom-prioritize-select)
  (define-key lyskom-sv-prioritize-mode-map "\C-y" 'kom-prioritize-yank)
  (define-key lyskom-sv-prioritize-mode-map " " 'kom-prioritize-select)
  (define-key lyskom-sv-prioritize-mode-map "\C-m" 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map "\C-j" 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map [down] 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map "\C-n" 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map "\C-?" 'kom-prioritize-previous-line)
  (define-key lyskom-sv-prioritize-mode-map "\M-\C-?" 'kom-prioritize-deselect-all)
  (define-key lyskom-sv-prioritize-mode-map [up] 'kom-prioritize-previous-line)
  (define-key lyskom-sv-prioritize-mode-map "\C-p" 'kom-prioritize-previous-line)
  (define-key lyskom-sv-prioritize-mode-map "p" 'kom-prioritize-previous-line)
  (define-key lyskom-sv-prioritize-mode-map [(meta up)] 
    'kom-prioritize-move-up)
  (define-key lyskom-sv-prioritize-mode-map "\M-p" 'kom-prioritize-move-up)
  (define-key lyskom-sv-prioritize-mode-map "u" 'kom-prioritize-move-up)
  (define-key lyskom-sv-prioritize-mode-map [(meta down)]
    'kom-prioritize-move-down)
  (define-key lyskom-sv-prioritize-mode-map "\M-n" 'kom-prioritize-move-down)
  (define-key lyskom-sv-prioritize-mode-map "d" 'kom-prioritize-move-down)
  (define-key lyskom-sv-prioritize-mode-map "n" 'kom-prioritize-move-down)
  (define-key lyskom-sv-prioritize-mode-map "\M-<" 'kom-prioritize-beginning)
  (define-key lyskom-sv-prioritize-mode-map "\M->" 'kom-prioritize-end)
  (define-key lyskom-sv-prioritize-mode-map "r" 'kom-prioritize-reprioritize)
  (define-key lyskom-sv-prioritize-mode-map "g" 'kom-prioritize-goto-priority)
  (define-key lyskom-sv-prioritize-mode-map "p" 'kom-prioritize-set-priority)
  (define-key lyskom-sv-prioritize-mode-map "s" 'kom-prioritize-save)
  (define-key lyskom-sv-prioritize-mode-map "q" 'kom-prioritize-quit)
  (define-key lyskom-sv-prioritize-mode-map "\C-c\C-c" 'kom-prioritize-quit)
  (define-key lyskom-sv-prioritize-mode-map "\t" 'kom-next-link)
  (define-key lyskom-sv-prioritize-mode-map "\M-\C-i" 'kom-previous-link)
)


(lyskom-language-var lyskom-prioritize-header-lines sv 2)

(lyskom-language-var lyskom-prioritize-header sv
" Prio   M\366te
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
  (define-key lyskom-sv-customize-map "\t" 'widget-forward)
  (define-key lyskom-sv-customize-map "\M-\t" 'widget-backward)
  (define-key lyskom-sv-customize-map "\C-m" 'widget-button-press)
  (define-key lyskom-sv-customize-map (lyskom-keys [mouse-2]) 
    'widget-button-click)
  (define-key lyskom-sv-customize-map (lyskom-keys [mouse-3]) 
    'lyskom-widget-click)
  (define-key lyskom-sv-customize-map "\C-c\C-c" 'lyskom-customize-save-and-quit)
  (define-key lyskom-sv-customize-map "\C-c\C-k" 'lyskom-customize-quit)
  (define-key lyskom-sv-customize-map "\C-c\C-s" 'lyskom-customize-save)
  (define-key lyskom-sv-customize-map "\C-c\C-a" 'lyskom-customize-apply)
  (define-key lyskom-sv-customize-map "?" 'lyskom-customize-help)
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
Listor mm.:  [INS] Lägg till rad   [DEL] Ta bort rad    [*] Ändra värde")

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


    ;;
    ;; Tags for variables
    ;;
    
    (kom-emacs-knows-iso-8859-1-tag . "Emacs förstår ISO-8859-1:")
    (kom-bury-buffers-tag . "Begrav buffertar när man byter LysKOM:")

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
    )
)

  
;;;; ============================================================
;;;; The default Ansaphone message goes here. The more complex 
;;;; message specification probably should too, but it's not here
;;;; yet. People who know how to use it are smart enough to do it
;;;; right.

(lyskom-language-var kom-ansaphone-default-reply sv
  "Jag l\344ser inte LysKOM just nu. Skicka g\344rna ett brev i st\344llet.")

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
     (("\305terse texten" . lyskom-button-view-text)
      ("\305terse omodifierat" . lyskom-button-review-noconversion)
      ("\305terse tr\344d" . lyskom-button-review-tree)
      ("\305terse urinl\344gget" . lyskom-button-find-root)
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
      ("Visa m\366tesstatus" . lyskom-button-view-conf-status)
      ("G\345 till m\366tet" . lyskom-button-goto-conf)
      ("Skicka gruppmeddelande" . lyskom-button-send-message)
      ("Bli medlem i m\366tet" . lyskom-button-add-self)
      ("Uttr\344d ur m\366tet" . lyskom-button-sub-self))
     ((kom-list-news . lyskom-button-goto-conf)
      (kom-membership . lyskom-button-goto-conf)))
    (pers 
     pers-text
     lyskom-button-view-pers-presentation
     (("Visa presentation" . 
                             lyskom-button-view-pers-presentation)
      ("Visa personstatus" . lyskom-button-view-pers-status)
      ("Skicka brev" . lyskom-button-mail)
      ("S\344nd meddelande" . lyskom-button-send-message))
     nil)
    (url 
     url-text
     lyskom-button-open-url
     (("\326ppna" . lyskom-button-open-url)
      ("Kopiera" . lyskom-button-copy-url))
     nil)
    (info-node 
     info-node-text
     lyskom-button-goto-info-node
     (("\362 öppna" . lyskom-button-goto-info-node))
     nil)
    (email
     email-text
     lyskom-button-open-email
     (("Skicka mail" . lyskom-button-open-email)
      ("Kopiera" . lyskom-button-copy-email))
     nil)))
        

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
   (kom-tell-send		. "F\366rs\366ker l\344gga in en text.")
   (kom-tell-login		. "Loggar in.")
   (kom-tell-read		. "L\344ser.")
   (kom-tell-1st-pres		. "Skriver den f\366rsta presentationen.")
   (kom-tell-write-comment	. "Skriver en kommentar.")
   (kom-tell-write-footnote	. "Skriver en fotnot.")
   (kom-tell-write-letter	. "Skriver ett brev.")
   (kom-tell-write-reply	. "Skriver ett privat svar.")
   (kom-tell-write-text	. "Skriver inl\344gg.")
   (kom-tell-conf-pres		. "Skriver presentation f\366r ett nytt m\366te.")
   (kom-tell-recover		. "\305terstartar kom. Suck.")
   (kom-tell-wait		. "V\344ntar.")
   (kom-tell-regret		. "\305ngrar sig och sl\344nger inl\344gget.")
   (kom-tell-review		. "\305terser.")
   (kom-tell-change-name       . "\304ndrar sitt namn till n\345got annat.")
   (kom-tell-change-supervisor . "\304ndrar organisat\366r f\366r n\345got.")
   (kom-tell-next-lyskom       . "Hoppar till ett annat LysKOM.")))

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
  '((error-0 . "Inget fel har intr\344ffat")
    (error-2 . "\304nnu ej implementerat")
    (error-3 . "Inte l\344ngre implementerat")
    (error-4 . "Felaktigt l\366senord")
    (error-5 . "Str\344ngen f\366r l\345ng")
    (error-6 . "Du \344r inte inloggad")
    (error-7 . "Ingen f\345r logga in i LysKOM just nu")
    (error-8 . "Du f\366rs\366kte anv\344nda m\366te nummer 0")
    (error-9 . "Odefinierat eller hemligt m\366te")
    (error-10 . "Odefinierad eller hemlig person")
    (error-11 . "Ingen skriv- eller l\344sr\344ttighet")
    (error-12 . "Otill\345ten operation")
    (error-13 . "Du \344r inte medlem i m\366tet")
    (error-14 . "Inget s\345dant textnummer")
    (error-15 . "Du kan inte anv\344nda globalt textnummer 0")
    (error-16 . "Inget s\345dant lokalt textnummer")
    (error-17 . "Du kan inte anv\344nda lokalt textnummer 0")
    (error-18 . "Namnet f\366r kort eller f\366r l\345ngt eller inneh\345ller felaktiga tecken")
    (error-19 . "Index utanf\366r gr\344nserna")
    (error-20 . "M\366tet existerar redan")
    (error-21 . "Personen existerar redan")
    (error-22 . "Hemligt, men ej l\344sskyddat")
    (error-23 . "Du f\345r inte \344ndra person/m\366tesflaggan")
    (error-24 . "Fel i databasen. Attans otur.")
    (error-25 . "Otill\345tet misc-f\344lt. (Internt fel)")
    (error-26 . "Otill\345ten infotyp. (Bug i klienten)")
    (error-27 . "Redan mottagare till denna text")
    (error-28 . "Redan kommentar till denna text")
    (error-29 . "Redan fotnot till denna text")
    (error-30 . "Inte mottagare till denna text")
    (error-31 . "Inte kommentar till denna text")
    (error-32 . "Inte fotnot till denna text")
    (error-33 . "F\366r m\345nga mottagare")
    (error-34 . "F\366r m\345nga kommentarer")
    (error-35 . "F\366r m\345nga fotnoter")
    (error-36 . "F\366r m\345nga markeringar")
    (error-37 . "Du \344r inte f\366rfattare till texten")
    (error-38 . "Du kan inte koppla upp dig till servern")
    (error-39 . "Minnet slut")
    (error-40 . "Servern har ballat ur")
    (error-41 . "Klienten tror att servern s\344ger att den inte f\366rst\345r klienten")
    (error-42 . "Ingen s\345dan session finns")
    (error-43 . "Ogiltigt regulj\344rt uttryck")
    (error-44 . "Texten \344r inte markerad")
    (error-45 . "Tillf\344lligt fel. F\366rs\366k senare")
    (error-46 . "Serven v\344grade ta emot en array")
    (error-47 . "Mottagaren tar inte emot anonyma texter")))


(provide 'lyskom-language-sv)

;;; swedish-strings.el ends here
