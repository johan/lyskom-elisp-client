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
  (define-key lyskom-sv-edit-prefix (lyskom-keys [C-�])
    'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (lyskom-keys [C-�])
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
    (text-to-add-bcc . "Vilket inl\344gg vill du skicka f�r k�nnedom:")
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
    (xmasday . "Juldagen.\nDu har v�l varit i julottan?")
    (newyearday . "Gott nytt %#1d!")
    (newyearevelate . "Mindre \344n en timme kvar...")
    (newyeareve . "Gott nytt \345r!")
    (cgdag . "Konungens f�delsedag.")
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
    (please-check-commented-texts . "�terse de kommenterade texterna och deras kommentarer.")
    (have-unread-comment . "Skicka in trots ol�sta kommentarer till text %#1n? ")

    (add-recipient-p . "Addera %#1P som mottagare? ")
    (matching-regexp . "M\366ten/personer som matchar '%#1s'\n")

    (who-is-active-all . "Visar alla sessioner.\n")
    (who-is-active-last-minutes . "Visar alla sessioner som har varit aktiva de senaste %#1d minuterna.\n")
    (showing-invisibles . "Visar osynliga sessioner.\n")
    (null-who-info . "Det finns inga (aktiva) inloggade.\n")

    (no-other-lyskom-r . "Det finns inga fler aktiva LysKOM-sessioner.\n")
    (no-lyskom-session . "Det finns ingen aktiv LysKOM-session.")
    (no-unread-lyskom-r . "Hittar ingen aktiv LysKOM-session med ol�sta.\n")
    (no-unread-lyskom . "Hittar ingen aktiv LysKOM-session med ol�sta.")

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

    (more-than-one-root . "Inl�gg %#1n har mer �n ett urinl�gg.\n")
    (more-than-one-root-review . 
"Inl�gg %#1n har mer �n ett urinl�gg, men endast ett tr�d kommer att visas.\n")

    ;; From edit-text.el:
    (press-C-c-C-c . "Tryck C-c C-c f\366r att skicka in texten.")
    (recipient . "Mottagare:")
    (recipient-prefix . "[Mm]")
    (carbon-copy . "Extra kopia:")
    (blank-carbon-copy . "F�r k�nnedom:")
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
    (added-blank-carbon-copy . "F�r k�nnedom till: ")
    (text-to-comment-q . "Vilket inl�gg vill du kommentera? ")
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
    (Hidden-recipient . "F�r k�nnedom")
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
    (priority-prompt . "Ny prioritet f�r %#1M: ")
    (priority-prompt-marked . "Ny prioritet p� markerade m�ten: ")
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

    (conference-no . "<m�te %#1d>")
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
    (which-language . "�ndra spr�k till: ")
    (send-formatted . "Skicka in som formatterad text? ")
    (changing-language-to . "Byter till %#1s.\n")
    (language-not-loaded . "%#1s finns inte tillg�ngligt.\n")

    (reformat-html . "(HTML)")
    (reformat-enriched . "(Enriched)")
    (reformat-filled . "(Ombruten)")
    
    (need-library . "Paketet \"%#1s\" beh�vs f�r att utf�ra detta kommando.\n")
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
    (kom-add-bcc	      . "Addera f�r k�nnedom")
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
    (kom-customize            . "Inst�llningar (f�r) LysKOM")
    (kom-next-kom             . "N�sta LysKOM")
    (kom-previous-kom         . "F�reg�ende LysKOM")
    (kom-next-unread-kom      . "N�sta ol�sta LysKOM")
    (kom-change-language      . "�ndra spr�k")
    (kom-calculate            . "Ber�kna")
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
  (define-prefix-command 'lyskom-sv-filter-get-prefix)

  (define-key lyskom-sv-mode-map "f" 'lyskom-sv-filter-get-prefix)
  (define-key lyskom-sv-mode-map "n" 'lyskom-sv-next-prefix)
  (define-key lyskom-sv-mode-map "l" 'lyskom-sv-list-prefix)
  (define-key lyskom-sv-mode-map "s" 'lyskom-sv-S-prefix)

  ;; emacs 19

  (define-key lyskom-sv-mode-map (lyskom-keys [�]) 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map (lyskom-keys [�]) 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map (lyskom-keys [�]) 'lyskom-sv-review-prefix)
  (define-key lyskom-sv-mode-map (lyskom-keys [�]) 'lyskom-sv-review-prefix)
  (define-key lyskom-sv-mode-map (lyskom-keys [f �]) 'kom-filter-subject)
  (define-key lyskom-sv-mode-map (lyskom-keys [f �]) 'kom-filter-subject)
  (define-key lyskom-sv-mode-map (lyskom-keys [l �]) 'kom-list-summary)
  (define-key lyskom-sv-mode-map (lyskom-keys [l �]) 'kom-list-summary)
  (define-key lyskom-sv-mode-map [�] 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map [�] 'lyskom-sv-change-prefix)
  (define-key lyskom-sv-mode-map [�] 'lyskom-sv-review-prefix)
  (define-key lyskom-sv-mode-map [�] 'lyskom-sv-review-prefix)
  (define-key lyskom-sv-mode-map [f �] 'kom-filter-subject)
  (define-key lyskom-sv-mode-map [f �] 'kom-filter-subject)
  (define-key lyskom-sv-mode-map [l �] 'kom-list-summary)
  (define-key lyskom-sv-mode-map [l �] 'kom-list-summary)
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
    (which-conf . "Ange ett m�te: ")
    (which-conf-or-person . "Ange en person eller ett m�te: ")
    (which-name . "Ange ett namn: ")
    (some-person . "Person %#1d")
    (invalid-value . "Otill�tet v�rde (%#1S)")
    (unknown-command . "Ok�nt kommando (%#1s)")

    ;;
    ;; Help messages
    ;;

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
Listor mm.:  [INS] L�gg till rad   [DEL] Ta bort rad    [*] �ndra v�rde")

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


    ;;
    ;; Tags for variables
    ;;
    
    (kom-emacs-knows-iso-8859-1-tag . "Emacs f�rst�r ISO-8859-1:")
    (kom-bury-buffers-tag . "Begrav buffertar n�r man byter LysKOM:")

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
     (("\362 �ppna" . lyskom-button-goto-info-node))
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
