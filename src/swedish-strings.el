;;;;;
;;;;; $Id: swedish-strings.el,v 40.0 1996-03-26 08:32:13 byers Exp $
;;;;; Copyright (C) 1991  Lysator Academic Computer Association.
;;;;;
;;;;; This file is part of the LysKOM server.
;;;;; 
;;;;; LysKOM is free software; you can redistribute it and/or modify it
;;;;; under the terms of the GNU General Public License as published by 
;;;;; the Free Software Foundation; either version 1, or (at your option) 
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


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: swedish-strings.el,v 40.0 1996-03-26 08:32:13 byers Exp $\n"))


;;; ================================================================
;;; lyskom-edit-mode-map

(defvar lyskom-edit-mode-map nil
  "Mode map for LysKOM edit.")

;;; Set the keymap for lyskom-edit-mode

(defvar lyskom-edit-prefix nil
  "Mode-map for lyskom edit mode.")

(if lyskom-edit-mode-map
    nil
  (setq lyskom-edit-mode-map (make-sparse-keymap))
  (define-prefix-command 'lyskom-edit-prefix)
  (define-prefix-command 'lyskom-edit-review-prefix)
  (define-prefix-command 'lyskom-edit-insert-prefix)
  (define-key lyskom-edit-mode-map "\C-c"	'lyskom-edit-prefix)
  (define-key lyskom-edit-mode-map "\C-c?"	'lyskom-help)
  (define-key lyskom-edit-mode-map "\C-c}"	'lyskom-edit-review-prefix)
  (define-key lyskom-edit-mode-map "\C-c]"	'lyskom-edit-review-prefix)
  ;; emacs 19
  (if (string-match "^19" emacs-version)
      (progn
	(define-key lyskom-edit-prefix [?\345] 'lyskom-edit-review-prefix)
	(define-key lyskom-edit-prefix [?\305] 'lyskom-edit-review-prefix)
	))
  (define-key lyskom-edit-mode-map "\C-ci"	'lyskom-edit-insert-prefix)
  (define-key lyskom-edit-mode-map "\C-c\C-c"	'kom-edit-send)
  (define-key lyskom-edit-mode-map "\C-ck"	'kom-edit-quit)
  (define-key lyskom-edit-mode-map "\C-c\C-k"	'kom-edit-quit)
  (define-key lyskom-edit-mode-map "\C-c}?"	'lyskom-help)
  (define-key lyskom-edit-mode-map "\C-c}k"	'kom-edit-show-commented)
  (define-key lyskom-edit-mode-map "\C-ci?"	'lyskom-help)
  (define-key lyskom-edit-mode-map "\C-cik"	'kom-edit-insert-commented)
  (define-key lyskom-edit-mode-map "\C-c\C-y"	'kom-edit-insert-commented)
  (define-key lyskom-edit-mode-map "\C-ci1"	'kom-edit-insert-digit-text)
  (define-key lyskom-edit-mode-map "\C-ci2"	'kom-edit-insert-digit-text)
  (define-key lyskom-edit-mode-map "\C-ci3"	'kom-edit-insert-digit-text)
  (define-key lyskom-edit-mode-map "\C-ci4"	'kom-edit-insert-digit-text)
  (define-key lyskom-edit-mode-map "\C-ci5"	'kom-edit-insert-digit-text)
  (define-key lyskom-edit-mode-map "\C-ci6"	'kom-edit-insert-digit-text)
  (define-key lyskom-edit-mode-map "\C-ci7"	'kom-edit-insert-digit-text)
  (define-key lyskom-edit-mode-map "\C-ci8"	'kom-edit-insert-digit-text)
  (define-key lyskom-edit-mode-map "\C-ci9"	'kom-edit-insert-digit-text)
  (define-key lyskom-edit-mode-map "\C-ci "	'kom-edit-insert-text)
  (define-prefix-command 'lyskom-edit-add-prefix)
  (define-key lyskom-edit-mode-map "\C-ca" 'lyskom-edit-add-prefix)
  (define-key lyskom-edit-mode-map "\C-cam" 'kom-edit-add-recipient)
  (define-key lyskom-edit-mode-map "\C-cak" 'kom-edit-add-copy)
  (define-key lyskom-edit-mode-map "\C-cae" 'kom-edit-add-copy)
  (define-key lyskom-edit-mode-map "\C-ca?" 'lyskom-help))


(defvar lyskom-header-separator
  (substitute-command-keys
   "\\<lyskom-edit-mode-map>\
--- Skriv nedan. \
Skicka in=\\[kom-edit-send], \
Avbryt=\\[kom-edit-quit], \
Annat se \\[describe-mode] ---")
  "*String to separate headers from text body.")

(defvar lyskom-swascii-header-separator nil
  "The swascii version of lyskom-header-separator.")


(defvar lyskom-header-subject "\304rende: "
  "*String to prompt for subject in the edit buffer.")

(defvar lyskom-swascii-header-subject nil
  "The swascii version of lyskom-header-subject.")

(defconst lyskom-strings 
  '(
    ; From vars.el: 
    ; From komtypes.el: nil
    ; From clienttypes.el: nil
    ; From startup.el:
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
Kontakta systemadministratera.\n")
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
Email-adress:\n\nAnnat:\t")
    (presentation-help . "Du skriver just  nu din presentation.\n")
    (not-present-anywhere . "Ej n\344rvarande i n\345got m\366te.")



    ; From internal.el:
    (shaky-tcp . "Nu n\345r jag inte servern. TCP/IP-f\366rbindelsen \344r skakig%#1s")
    (retrying-tcp . "Jag f\366rs\366ker igen.")

    ; From parse.el:
    (protocol-error . "protokollfel: %s")

    ; From services.el:
    (interrupted . "Kommandot avbrutet\n")

    ; From cache.el:
    ; No entries.

    ; From commands1.el:
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

    (what-text-to-delete . "Vilket inl\344gg skall tas bort?")
    (deleting-text . "Radering av text %#1:n...")

    (presentation-for-whom . "Vilket m\366te/person? ")
    (somebody-deleted-that-conf . "N\345gon tog precis bort m\366tet.\n")
    (review-presentation-of . "\305terse presentation av %#1M.\n")
    (has-no-presentation . "%#1:M har ingen presentation.\n")

    (have-to-read . "Du m\345ste l\344sa ett inl\344gg f\366rst.\n")

    (review-text-no . "\305terse text nummer %#1n.\n")
    (no-comment-to . "Det finns inget kommenterat inl\344gg att titta p\345.\n")

    (who-letter-to . "Vem vill du skicka brev till? ")
    (has-motd . "%#1P har en lapp p\345 d\366rren:\n\n")
    (motd-persist-q . "Vill du fortfarande skicka brevet? ")

    (who-to-add . "Vem vill du addera? ")
    (where-to-add . "Vilket m\366te skall han/hon adderas till? ")
    (where-to-add-self . "Vilket m\366te vill du bli medlem i? ")
    (priority-q . "Prioritet p\345 m\366tet? (0 (l\345g) - 255 (h\366g)) ")
    (done . "klart.\n")
    (nope . "Det gick inte.\n")

    (cant-find-supervisor . "Hittar inte organisat\366ren f\366r %#1M.\n")
    (is-read-protected-contact-supervisor . "%#1M \344r slutet.
Skicka ett brev till %#2P f\366r medlemsskap.\n")

    (conf-does-not-exist . "\nM\366tet finns inte.\n")

    (who-to-exclude . "Vem vill du utesluta? ")
    (where-from-exclude . "Vilket m\366te skall han/hon uteslutas ifr\345n? ")

    (leave-what-conf . "Vilket m\366te vill du g\345 ur? ")

    (error-fetching-person . "Fel i personh\344mtningen\n")
    (error-fetching-conf . "Fel i m\366tesh\344mtningen.\n")

    (name-of-conf . "Vad ska m\366tet heta? ")
    (anyone-member . "F\345r vem som helst bli medlem? ")
    (secret-conf . "Hemligt m\366te? ")
    (comments-allowed . "F\345r man skriva kommentarer? ")
    (what-comment-no . "Kommentera text nummer: ")
    (confusion-what-to-comment . "Jag f\366rst\345r inte vilken text du vill kommentera.\n")
    (confusion-what-to-footnote . "Jag f\366rst\345r inte vilken text du vill skriva en fotnot till.\n")

    (what-private-no . "Personligt svar till text nummer: ")
    (confusion-who-to-reply-to . "Jag f\366rst\345r inte vems inl\344gg du vill
 skriva ett privat svar till.\n")
    (confusion-what-to-answer-to . "Jag f\366rst\345r inte vilken text du vill besvara.")
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

    (search-for-pers . "Ange s\366kvillkor (RETURN f\366r alla personer) ")
    (search-for-conf . "Ange s\366kvillkor (RETURN f\366r alla m\366ten) ")
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
    (whos-passwd . "Vem vill du \344ndra l\366senord f\366r (dig sj\344lv)? ")
    (old-passwd . "Mata in ditt l\366senord: ")
    (new-passwd . "Mata in det nya l\366senordet: ")
    (changing-passwd . "\304ndrar l\366senordet...")
    (retype-dont-match . "L\366senorden \344r inte samma. G\366r om.\n")
    (palindrome . "(ett palindrom!) ")
    (lyskom-name . "Anv\344ndare")
    (is-in-conf . "N\344rvarande i m\366te")
    (from-machine . "K\366r fr\345n")
    (is-doing . "G\366r")
    (text-to-add-recipient . "Vilket inl\344gg vill du addera mottagare till")
    (text-to-add-copy . "Vilket inl\344gg vill du addera en extra kopia till")
    (text-to-delete-recipient . "Vilket inl\344gg vill du subtrahera mottagare fr\345n")
    (text-to-add-comment-to . "Vilket inl\344gg vill du addera en kommentar till")
    (text-to-delete-comment-from . "Vilket inl\344gg vill du subtrahera en kommentar fr\345n")

    (where-on-list-q . "Placering p\345 listan? (0-%#1d) ")
    (member-in-conf . "Bli medlem i %#1M...")
    (add-member-in . "Addera %#1P som medlem i %#2M...")
    (unsubscribe-to . "Uttr\344da ur %#1M...")

    (exclude-from . "Utesluta %#1P fr\345n %#2M...")

    (unsubscribe-failed . "\nDet gick inte. %#1P var kanske aldrig medlem i %#2M?\n")

    (You . "Du")
    (could-not-create-conf . "M\366tet \"%#1s\" kunde ej skapas.\nFelkod: %#2d.\n")
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
    (time-is . "Klockan \344r %4#1d-%02#2d-%02#3d %02#4d:%02#5d:%02#6d %#7s(enligt servern).\n")
    (total-users . "    Sammanlagt %#1d anv\344ndare.\n")
    (who-to-add-q . "Vilket m\366te/person vill du addera som mottagare? ")
    (who-to-add-copy-q . "Vilket m\366te/person vill du addera som kopiemottagare? ")
    (who-to-sub-q . "Vilket m\366te/person vill du subtrahera som mottagare? ")

    (adding-name-as-recipient . "Adderar %#1M som mottagare till text %#2n...")
    (adding-name-as-copy . "Adderar %#1M som kopiemottagare till text %#2n...")
    (remove-name-as-recipient .
     "Subtraherar %#1M som mottagare fr\345n text %#2n...")
    (text-to-add-q . "Vilket inl\344gg vill du addera som kommentar? ")
    (text-to-remove-q . "Vilket inl\344gg vill du subtrahera som kommentar? ")
    (add-comment-to . "Adderar text %#1n som kommentar till text %#2n...")
    (sub-comment-to . "Subtraherar text %#1n som kommentar till text %#2n...")
    (comment-keep-recpt-p ."Ska %#1s vara mottagare? ")

    ; From commands2.el:

    (your-memberships . "Ditt medlemskap i olika KOM-m\366ten:\n")
    (memberships-header . "Senast inne	   Prio	Ol\344sta	M\366tesnamn\n")
    (memberships-line . "%16#1s  %#2d\t%#3d\t%#4M\n")
    (conf-for-status . "Vilket m\366te vill du se statusen f\366r? ")
    (no-such-conf . "M\366tet finns ej.")
    (status-record . "Status f\366r m\366te %#1M (%#2m) %#3s\n\n")
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
    (conf-has-these-members . "\n%#1M har f\366ljande medlemmar:\n")
    (member-list-header . "Senast inne         Osett  Namn\n\n")
    (secret-membership . "*** Hemlig rad ***\n")
    (conf-membership-line . "%#1s%#2M\n")
    (pers-for-status . "Vem vill du se statusen f\366r? ")
    (no-such-pers . "Det finns ingen s\345dan person.")
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
    (his-total-unread . "\n%#1M har totalt %#2d ol\344sta.\n")
    (message-prompt . "Meddelande: ")
    (message-sent-to-user . 
"================================================================
Ditt meddelande till %#2M:

%#1t
----------------------------------------------------------------
")
    (message-sent-to-all . 
"================================================================
Ditt allm\344nna meddelande l\366d:

%#1t
----------------------------------------------------------------
")
    (message-nope .
  "Du kunde inte skicka meddelandet. Mottagaren var kanske inte inloggad.
Meddelandet du f\366rs\366kte s\344nda till %#1M var:
%#2t\n")
    (only-last . "Endast l\344sa senaste (0 - %#1d) i %#2s? ")
    (only-error . "N\345t gick galet. Sorry.\n")

    (you-have-unreads . "Du har %#1d ol\344sta inl\344gg i %#2M\n")
    (you-have-an-unread . "Du har 1 ol\344st inl\344gg i %#1M\n")
    (you-have-unreads-special . "Du har %#1d okommenterade inl\344gg i %#2M\n")
    (you-have-an-unread-special . "Du har 1 okommenterat inl\344gg i %#1M\n")
    (you-have-read-everything . "Du har sett alla nyheter\n")
    (total-unreads . "\nDu har %#1d ol\344sta inl\344gg.\n")
    (total-unread . "\nDu har 1 ol\344st inl\344gg.\n")
    (waiting-for-anything .
     "Du v\344ntar p\345 ett inl\344gg i vilket m\366te som helst.\n")
    (waiting-higher-than . 
     "Du v\344ntar p\345 ett inl\344gg i ett m\366te med h\366gre prioritet \344n %#1d.\n")

    (have-to-be-in-conf-with-unread . "Du m\345ste g\345 till ett icketomt m\366te f\366rst.\n")
    (Texts . "Inl\344gg")
    (Date . "Datum")
    (Lines . "Rader")
    (Author . "  F\366rfattare")
    (Subject . "  \304rende")
    (could-not-read . "Du fick inte l\344sa denna text (%#1n).\n")
    (multiple-choice . "Flera alternativ finns.")
    (does-not-exist . "Detta kommando finns inte.")
    (summary-line . "%=-8#1n%#2s%4#3d  %[%#4@%#5:P%]  %[%#6@%#7r%]\n")

    (what-mark-to-list . "Vilken markering vill du lista? ")
    (you-have-marks . "Du har %#1d inl\344gg markerade markerade med %#2d.\n")
    (you-have-marks-all . "Du har %#1d markerade inl\344gg.\n")


    ; Only people fixing bugs or recieving bugg-reports should change these:
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
    (set-session-priority . "S\344tt l\344sniv\345:")

    ; From review.el:
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
    (review-info . "\305terse %#1s\n")
;; +++
    (review-info-by-to . "\305terse %#1s av %#2P till %#3M fram\345t.\n")
    (you-review . "Du \345terser nu %#1s.\n")
    (read-text-first . "Du m\345ste l\344sa en text f\366rst.\n")
    (cannot-read-last-text . "Du kan inte l\344sa den senast l\344sta texten.\n")
    (review-n-texts . "\305terse %#1d inl\344gg.\n")
    (review-marked . "\305terse %#1d markerade.\n")
    (review-text-no . "\305terse text nummer %#1n")
    (review-one-comment . "\305terse en kommentar till inl\344gg %#1n.\n")
    (review-many-comments . "\304terse %#2d kommentarer till inl\344gg %#1n.\n")
    (read-normally-read . "Hur m\345nga vill du se igen?")

    ; From edit-text.el:
    (press-C-c-C-c . "Tryck C-c C-c f\366r att skicka in texten.")
    (recipient . "Mottagare:")
    (carbon-copy . "Extra kopia:")
    (text-mass . "%#4s%#1s\n%#2s\n%#3s")
    (comment-to-by . "%#1s till text %#2d%#3s.\n")
    (already-sent . "Du har redan skickat denna text en g\345ng. S\344nd \344nd\345? ")
    (subject . "\304rende: ")
    (enter-subject-idi . "Skriv ett ärende.")
    (which-text-include . "Vilken text skall vi inkludera? ")
    (added-recipient . "Mottagare som skall adderas: ")
    (added-carbon-copy . "Extra kopia till m\366te: ")
    (conf-has-motd-no . "M\366tet har en lapp p\345 d\366rren. (%#1d)\n\n%#2s")
    (still-want-to-add . "Vill du fortfarande addera m\366tet? ")
    (could-not-create-text . "\nTexten kunde ej skapas. Felet: %#2s.\n")
    (no-get-text . "Du fick inte h\344mta texten.")
    (unknown-header . "Okänd information på raden")

    ; From view-text.el:
    (line . " /1 rad/ ")
    (lines ." /%#1d rader/ ")

    (marked-by-you . "Markerad av dig.\n")
    (marked-by-you-and-one . "Markerad av dig och n\345gon annan.\n")
    (marked-by-you-and-several . "Markerad av dig och %#1d andra.\n")
    (marked-by-one . "Markerad av 1 person.\n")
    (marked-by-several . "Markerad av %#1d personer.\n")

;; The format of this function should coincide with the format of the 
;; lyskom-text-start variable. DONT change one without changing the other.

; used by lyskom-print-time
    (time-y-m-d-h-m . "%4#1d-%02#2d-%02#3d  %02#4d:%02#5d ")
    (today-time-format-string . "%#6s %02#4d:%02#5d")
    (yesterday-time-format-string . "%#6s %02#4d:%02#5d")
    (today . "idag")
    (yesterday . "igår")
; used by lyskom-return-time
    (time-yyyy-mm-dd-hh-mm . "%4#1d-%02#2d-%02#3d %02#4d:%02#5d")
    (no-such-text-no . "Det finns ingen s\345dan text. (%#1n)\n")

    (head-Subject . "\304rende: ")
    (Recipient . "Mottagare")
    (Extra-recipient . "Extra kopia")
    (send-at . "    S\344nt:     %#1s\n")
    (sent-by . "    S\344nt av %#1P\n")
    (recieved-at . "    Mottaget: %#1s\n")
;; +++ unused?
    (written-by . "av %#1P")
;;; +++
    (comment-to-text-by . "Kommentar till text %#1n av %#2P\n")
    (footnote-to-text-by . "Fotnot till text %#1n av %#2P\n")
    (comment-in-text-by . "Kommentar i text %#1n av %#2P\n")
    (footnote-in-text-by . "Fotnot i text %#1n av %#2P\n")

    ; From async.el:

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
Gruppmeddelande till %#3M fr\345n %#2P (%#4s):

%#1t
----------------------------------------------------------------
")
    (text-is-created . "Text %#1n \344r skapad!")

    ; Used in mode-line-process
    (mode-line-waiting . ": v\344ntar")
    (mode-line-working . ": arbetar")
    (mode-line-saving . ": sparar")
    (mode-line-down . ": nerkopplad")

    ; From completing-read.el:

    (person-or-conf-no-regexp . "\\`[ \t]*[mpMP]\\w*[ \t]+\\([0-9]+\\)\\'")

    ; From prioritize.el:
    (cannot-get-membership . "Kan ej h\344mta medlemsskap f\366r dig.")
    (cannot-get-pers-stat . "Kan ej h\344mta personstatus f\366r dig.")
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

    ; From flags.el:
    (saving-settings . "Sparar inst\344llningarna")
    (hang-on . "V\344nta ett tag...\n")
    (could-not-save-options . "Kunde ej spara inst\344llningarna.\n")
    (could-not-create-area . "Kunde ej skapa texten.\n")
    (could-not-set-user-area . "Kunde ej st\344lla om user-arean. Servern s\344ger felmeddelande: %#1d\n")
    (you-dont-exist . "Du finns inte.\n")
    (error-in-options . "Det fanns ett fel i en av dina variabler (%#1s)
Det stod \"%#2s\" i user-arean. Den sätts till nil istället.
Skicka en bugrapport.\n")

    ; From elib-string.el:
    ; No entries.

    ; From lyskom-rest.el:

    (mode-line-unread . " Ol\344sta")
    (mode-line-letters . " brev")

    (error-code . "Felkod %#2d: %#1s.\n")
    (extended-command . "LysKOM: ")
    (wait-for-server . "LysKOM v\344ntar p\345 svar fr\345n servern. V\344nta tills du f\345r en prompt.\n")
    (review-text-q . "\305terse text nummer: ")
    (review-text-no . "\305terse text nummer %#1d.\n")

    (completely-read-conf . "Du har sett alla texter i detta m\366te.\n")
    (not-in-any-conf . "Du l\344ser inte n\345got m\366te just nu.\n")

    (all-conf-unread-r . "Du har l\344st ut alla m\366ten.\n")
    (all-conf-unread-s . "Du har l\344st ut alla m\366ten. ")
    (one-unread . "%#1M - 1 ol\344st\n")
    (several-unread . "%#1M - %#2d ol\344sta\n")
    (enter-conf . "%#1M\n")

    (save-on-file-q . "Spara inl\344gg p\345 fil: (%#1s) ")
    (wait-for-prompt . "V\344nta p\345 prompten!")
    (prompt-modifier-ansaphone . "[%s]")
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
    (the-command . "Kommandot:%#1s")
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
Lyskom-sessionen onormalt st\344ngd.
Felmeddelande: %#1s**************************************************")
    (error-not-found . "Fel nummer %#1d. Ingen klartextf\366rklaring finns.")

    ; Useful in more place than one:
    (illegal-command . "Otill\345tet kommando.\n")
    (no-such-text . "Det finns inget s\345dant inl\344gg.\n")
    (no-such-text-m . "Det finns inget s\345dant inl\344gg.")
    (everybody . "alla")
    (anybody . "vem som helst")
    (forward . "fram\345t")
    (backward . "bak\345t")
    (wait . "V\344nta ett tag...\n")
    (comment . "Kommentar")
    (footnote . "Fotnot")

;;;+++ not used?
    (by . " av %#1s")
    (text-created .  "Text nummer %#1n \344r skapad.\n")

    (resolve-session . "Ange vilken session: ")

    (starting-program . "Startar %#1s...")
    (super-jump . "Filtrerar \344rende \"%#1r\" i m\366te \"%#2M\"\n")
    (filtered . "[Filtrerad]")
    (filter-error-specification . "Fel i filterspecifikationen")
    (filter-error-bad-not . "Fel i filterspecifikation efter 'not'")
    (filter-error-unknown-key . "Filternyckeln '%S' \344r ok\344nd.")
    (filter-error-key-arg . "Fel filterdata (%S %S)")
    (filter-tree . "Hoppar \366ver text %#1n \"%#2r\" av %#3P och dess kommentarstr\344d.\n")
    (filter-text . "Hoppar \366ver text %#1n \"%#2r\" av %#3P.\n")
    (filter-permanent . "Permanent? ")
    (filter-action . "Hur vill du filtrera? ")
    (filter-in-conf . "I vilket m\366te (Alla)? ")
    (filter-subject . "Filtrera vilket \344rende? ")
    (filter-which-text . "Filtrera inl\344gg som inneh\345ller: ")
    (filter-author . "Filtrera vilken författare? ")
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
    (filter-edit-help . "p Upp, n Ned, i Ny rad, M-i Nytt filter, d Radera rad, M-d Radera filter")
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
    (remote-control-autoreply . "Automatsvar på eller av? ")

    (state-on . "p\345slaget")
    (state-off . "avslaget")

    (text-popup-title . "Inl\344gg %#1s")
    (conf-popup-title . "M\366te %#1s")
    (pers-popup-title . "Person %#1s")
    (url-popup-title  . "URL %#1s")
    (generic-popup-title . "%#1s")
    )
  "Assoc list containing pairs of atoms and strings")



;;; ================================================================
;;;              The commands and their associated functions


(defconst lyskom-commands
  '(
    (describe-mode              "Hj\344lp")
    (kom-slow-mode              "L\345ngsamma kommandon")
    (kom-quick-mode             "Snabba kommandon")
    (kom-send-message           "S\344nda meddelande")
    (kom-create-conf		"Skapa m\366te")
    (kom-delete-conf            "Utpl\345na")
    (kom-delete-text            "Radera inl\344gg")
    (kom-display-time		"Se tiden")
    (kom-go-to-conf		"G\345 till m\366te")
    (kom-go-to-next-conf	"G\345 till n\344sta m\366te")
    (kom-jump			"Hoppa \366ver alla kommentarer")
    (kom-list-conferences 	"Lista m\366ten") 
    (kom-list-persons		"Lista personer")
    (kom-list-news              "Lista nyheter")
    (kom-membership		"Lista medlemsskap")
    ;; (kom-list-marks		"Lista markeringar") 
    (kom-postpone		"Uppskjuta l\344sning")
    (kom-set-session-priority	"S\344tt l\344sniv\345")
    (kom-prioritize		"Prioritera m\366ten")
    (kom-status-person		"Status (f\366r) person")
    (kom-status-conf		"Status (f\366r) m\366te")
    (kom-add-self		"Bli medlem i m\366te")
    (kom-list-summary		"Lista \344renden")
    (kom-sub-self               "Uttr\344da ur m\366te")
    (kom-quit			"Sluta")
    (kom-recover		"\305terstarta kom") 
    (kom-start-anew		"B\366rja med nytt namn")
    (kom-view			"\305terse inl\344gg")
    (kom-find-root-review	"\305terse tr\344d")
    (kom-review-comments	"\305terse alla kommentarer")
    (kom-review-tree		"\305terse alla kommentarer rekursivt")
    (kom-review-clear           "\305terse hoppa")
    (kom-review-last-normally-read
     				"\305terse igen")
    (kom-review-noconversion    "\305terse omodifierat")
    (kom-review-next            "\305terse n\344sta")
    (kom-find-root		"\305terse urinl\344gget")
    (kom-review-by-to           "\305terse senaste")
    (kom-view-commented-text    "\305terse det kommenterade")
    (kom-review-stack           "\305terse lista")
    (kom-review-presentation    "\305terse presentation")
    (kom-review-backward        "(\305terse) Bakl\344nges")
    (kom-view-next-text		"L\344sa n\344sta inl\344gg")
    (kom-who-is-on		"Vilka \344r inloggade") 
;    (kom-display-who-buffer	"Visa vilkalistan")
    (kom-busy-wait		"V\344nta p\345 ett inl\344gg")
    (kom-write-comment		"Kommentera inl\344gget")
    (kom-comment-previous	"Kommentera f\366reg\345ende inl\344gg")
    (kom-write-footnote         "Fotnot till inl\344gg")
    (kom-private-answer         "Personligt svar")
    (kom-private-answer-previous
     "Personligt svar p\345 f\366reg\345ende inl\344gg")
    (kom-set-unread		"Endast l\344sa senaste")
    (kom-write-text		"Skriva ett inl\344gg")
    (kom-send-letter		"Skicka brev")
    (kom-change-name		"\304ndra namn")
    (kom-change-password        "\304ndra l\366senord")
    (kom-change-supervisor	"\304ndra organisat\366r")
    (kom-change-presentation	"\304ndra presentation")
    (kom-get-appreciation       "F\345 uppmuntran")
    (kom-get-abuse              "F\345 sk\344ll")
    (kom-mark-text              "Markera (inl\344gg)")
    (kom-unmark-text            "Avmarkera (inl\344gg)")
    (kom-review-marked-texts    "\305terse markerade")
    (kom-review-all-marked-texts "\305terse alla markerade")
    (kom-add-recipient          "Addera mottagare")
    (kom-add-copy		"Addera extra kopiemottagare")
    (kom-sub-recipient          "Subtrahera mottagare")
    (kom-add-comment		"Addera kommentar")
    (kom-sub-comment		"Subtrahera kommentar")
    (kom-add-member		"Addera medlem")
    (kom-sub-member             "Uteslut medlem")
    (kom-change-conf-motd	"S\344tt lapp p\345 d\366rren")
    (kom-set-garb-nice          "\304ndra livsl\344ngd")
    (kom-set-super-conf         "\304ndra superm\366te")
    (kom-set-permitted-submitters "\304ndra till\345tna f\366rfattare")
    (kom-unset-conf-motd	"Ta bort lapp p\345 d\366rren")
    (kom-save-text		"Spara text (p\345 fil)")
    (kom-edit-options		"\304ndra variabler")
    (kom-shutdown-server        "St\344ng av servern")
    (kom-enable-adm-caps        "\326verg\345 till administrat\366rsmod")
    (kom-disable-adm-caps       "\326verg\345 till normalmod")
    (kom-set-motd               "S\344tt loginmeddelande")
    (kom-remove-motd            "Ta bort loginmeddelande")
    (kom-force-logout           "Kasta ut en session")
    (kom-list-files		"Lista filarean")
    (kom-put-file		"Ladda upp fil")
    (kom-get-file		"Ladda ner fil")
    (kom-filter-author  "Filtrera f\366rfattare")
    (kom-filter-subject "Filtrera \344rende")
    (kom-filter-text    "Filtrera inneh\345ll")
    (kom-super-jump     "Superhoppa")
    (kom-filter-edit    "\304ndra filter")
    (kom-list-filters   "Lista filter")
    (kom-show-user-area "Visa user-arean")
    (kom-change-conf-type "\304ndra m\366testyp")

    (kom-change-auto-reply "\304ndra svarsmeddelande")
    (kom-toggle-auto-reply "Automatsvar")
    (kom-list-messages     "Lista meddelanden")
    (kom-erase-messages    "Radera meddelanden")

    (kom-remote-autoreply  "Fj\344rrkontrollera automatsvar")
    (kom-remote-set-message "Fj\344rrkontrollera \344ndra svarsmeddelande")
    (kom-remote-list-messages "Fj\344rrkontrollera lista meddelanden")
    (kom-remote-erase-messages "Fj\344rrkontrollera radera meddelanden")
    (kom-remote-quit "Fj\344rrkontrollera avsluta")
    )
  "A list of LysKOM-commands that the extended parser understands.")

(defvar lyskom-swascii-commands nil
  "The swascii-versions of lyskom-commands.")

(defvar lyskom-onoff-table
  '(("på" . on) ("av" . off))
  "A completion table for on and off selections.")

(defvar lyskom-filter-predicate-list
      '(("=" . nil) ("!=" . t))
      "A list of legal filter comparison predicates.")

(defvar lyskom-filter-what
      '((author . "F\366rfattare")
        (author-no . "F\366rfattare (nummer)")
        (author-re . "F\366rfattare (regexp)")
        (subject . "\304rende")
        (subject-re . "\304rende (regexp)")
        (recipient . "Mottagare")
        (recipient-no . "Mottagare (nummer)")
        (recipient-re . "Mottagare (regexp)")
        (text . "Inneh\345ll")
        (text . "Inneh\345ll (regexp)"))
      "A list of legal filter conditions and their textual representation.")

(defvar lyskom-filter-actions
      '((skip-text . "Hoppa \366ver")
        (dontshow . "Visa inte")
        (skip-tree . "Hoppa \366ver kommentarer"))
      "A list of legal filter actions an their textual representation.")
                               
(defvar lyskom-swascii-filter-actions nil
  "The swascii-versions of lyskom-filter-actions.")
(defvar lyskom-swascii-filter-what nil
  "The swascii version of lyskom-filter-what")

(defvar lyskom-text-start "[0-9]+ +\\(199[0-9]-[0-1][0-9]-[0-3][0-9]\\|idag\\|igår\\) +[0-2][0-9]:[0-5][0-9] +/[0-9]+ rad\\(er\\)?/ "
  "Regexp matching beginning of a text in lyskom buffer.
Cf. paragraph-start.")


(if lyskom-mode-map
    nil
  (setq lyskom-mode-map (make-keymap))
  (suppress-keymap lyskom-mode-map)
  (define-prefix-command 'lyskom-review-prefix)
  (define-prefix-command 'lyskom-change-prefix)
  (define-prefix-command 'lyskom-next-prefix)
  (define-prefix-command 'lyskom-list-prefix)
;  (define-prefix-command 'lyskom-get-prefix)
  (define-prefix-command 'lyskom-S-prefix)
  (define-prefix-command 'lyskom-filter-get-prefix)

  (define-key lyskom-mode-map "{" 'lyskom-change-prefix) ; krullar
  (define-key lyskom-mode-map "[" 'lyskom-change-prefix)
  (define-key lyskom-mode-map "}" 'lyskom-review-prefix)
  (define-key lyskom-mode-map "]" 'lyskom-review-prefix)
  (define-key lyskom-mode-map "\344" 'lyskom-change-prefix) ; 8-bit keymap
  (define-key lyskom-mode-map "\304" 'lyskom-change-prefix)
  (define-key lyskom-mode-map "\345" 'lyskom-review-prefix)
  (define-key lyskom-mode-map "\305" 'lyskom-review-prefix)
  (define-key lyskom-mode-map "\033{" 'lyskom-change-prefix) ; 7(8)-bit emacs.
  (define-key lyskom-mode-map "\033[" 'lyskom-change-prefix) 
  (define-key lyskom-mode-map "\033}" 'lyskom-review-prefix)
  (define-key lyskom-mode-map "\033]" 'lyskom-review-prefix) 

  (define-key lyskom-mode-map "f" 'lyskom-filter-get-prefix)
  (define-key lyskom-mode-map "n" 'lyskom-next-prefix)
  (define-key lyskom-mode-map "l" 'lyskom-list-prefix)
  (define-key lyskom-mode-map "s" 'lyskom-S-prefix)

  ;; emacs 19
  (if (string-match "^19" emacs-version)
      (progn
	(define-key lyskom-mode-map [?\344] 'lyskom-change-prefix)
	(define-key lyskom-mode-map [?\304] 'lyskom-change-prefix)
	(define-key lyskom-mode-map [?\345] 'lyskom-review-prefix)
	(define-key lyskom-mode-map [?\305] 'lyskom-review-prefix)
	
	(define-key lyskom-mode-map [?f ?\344] 'kom-filter-subject)
	(define-key lyskom-mode-map [?f ?\304] 'kom-filter-subject)
	(define-key lyskom-mode-map [?l ?\344] 'kom-list-summary)
	(define-key lyskom-mode-map [?l ?\304] 'kom-list-summary)
	(define-key lyskom-mode-map [mouse-2] 'kom-mouse-2)
	(define-key lyskom-mode-map [down-mouse-3] 'kom-mouse-3)
	(define-key lyskom-mode-map [mouse-3] 'kom-mouse-null)
	(define-key lyskom-mode-map "*" 'kom-key-mouse-2)
	(define-key lyskom-mode-map "\C-i" 'kom-next-link)
	(define-key lyskom-mode-map "\M-\C-i" 'kom-previous-link)
	))
  ;;(define-key lyskom-mode-map "vi" 'vilka)
  
  ;; These should be first in order to be last in the menu of alternatives.
  (define-key lyskom-mode-map "{?" 'lyskom-help)
  (define-key lyskom-mode-map "}?" 'lyskom-help)
  (define-key lyskom-mode-map "f?" 'lyskom-help)
  (define-key lyskom-mode-map "n?" 'lyskom-help)
  (define-key lyskom-mode-map "l?" 'lyskom-help)
  (define-key lyskom-mode-map "s?" 'lyskom-help)
  

  (define-key lyskom-mode-map "e" 'kom-set-unread)
  (define-key lyskom-mode-map "a" 'kom-extended-command)
  (define-key lyskom-mode-map " " 'kom-next-command)
  (define-key lyskom-mode-map "\n" 'kom-page-next-command)
  (define-key lyskom-mode-map "\r" 'kom-line-next-command)
  (define-key lyskom-mode-map "j" 'kom-page-next-command)

  (define-key lyskom-mode-map "?"  'describe-mode)
  (define-key lyskom-mode-map "b"  'kom-send-letter)
  (define-key lyskom-mode-map "g"  'kom-go-to-conf)
  (define-key lyskom-mode-map "i"  'kom-write-text)
  (define-key lyskom-mode-map "k"  'kom-write-comment)
  (define-key lyskom-mode-map "K"  'kom-comment-previous)
  (define-key lyskom-mode-map "F"  'kom-write-footnote)
  (define-key lyskom-mode-map "p"  'kom-private-answer)
  (define-key lyskom-mode-map "P"  'kom-private-answer-previous)
  (define-key lyskom-mode-map "h"  'kom-jump)
  (define-key lyskom-mode-map "H"  'kom-super-jump)
  (define-key lyskom-mode-map "lm" 'kom-list-conferences)
  (define-key lyskom-mode-map "ln" 'kom-list-news)
  (define-key lyskom-mode-map "lp" 'kom-list-persons)
  (define-key lyskom-mode-map "ls" 'kom-membership)
  (define-key lyskom-mode-map "l{" 'kom-list-summary)
  (define-key lyskom-mode-map "l[" 'kom-list-summary)
  (define-key lyskom-mode-map "l\344" 'kom-list-summary) ; 8-bit emacs
  (define-key lyskom-mode-map "l\304" 'kom-list-summary)
  (define-key lyskom-mode-map "l\033{" 'kom-list-summary) ; 7(8)-bit emacs
  (define-key lyskom-mode-map "l\033[" 'kom-list-summary)
  (define-key lyskom-mode-map "lf" 'kom-list-filters)
  (define-key lyskom-mode-map "m"  'kom-add-self)
  (define-key lyskom-mode-map "M"  'kom-mark-text)
  (define-key lyskom-mode-map "A"  'kom-unmark-text)
  (define-key lyskom-mode-map "ni" 'kom-view-next-new-text)
  (define-key lyskom-mode-map "nm" 'kom-go-to-next-conf)
  (define-key lyskom-mode-map "nl" 'kom-next-kom)
  (define-key lyskom-mode-map "S"  'kom-quit)
  (define-key lyskom-mode-map "q"  'kom-quit)
  (define-key lyskom-mode-map "z"  'kom-bury)
  (define-key lyskom-mode-map "R"  'kom-recover)
  (define-key lyskom-mode-map "t"  'kom-display-time)
  (define-key lyskom-mode-map "fu" 'kom-get-appreciation)
  (define-key lyskom-mode-map "fs" 'kom-get-abuse)
  (define-key lyskom-mode-map "f{" 'kom-filter-subject)
  (define-key lyskom-mode-map "f[" 'kom-filter-subject)
  (define-key lyskom-mode-map "f\344" 'kom-filter-subject)
  (define-key lyskom-mode-map "f\304" 'kom-filter-subject)
  (define-key lyskom-mode-map "f\033{" 'kom-filter-subject)
  (define-key lyskom-mode-map "f\033[" 'kom-filter-subject)
  (define-key lyskom-mode-map "ff" 'kom-filter-author)
  (define-key lyskom-mode-map "fi" 'kom-filter-text)
  (define-key lyskom-mode-map "v"  'kom-who-is-on)
  (define-key lyskom-mode-map "V"  'kom-busy-wait)
  (define-key lyskom-mode-map "{p" 'kom-change-presentation)
  (define-key lyskom-mode-map "{f" 'kom-filter-edit)
  (define-key lyskom-mode-map "{m" 'kom-change-auto-reply)
  (define-key lyskom-mode-map "} " 'kom-view)
  (define-key lyskom-mode-map "}0" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "}1" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "}2" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "}3" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "}4" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "}5" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "}6" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "}7" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "}8" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "}9" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "}k" 'kom-view-commented-text)
  (define-key lyskom-mode-map "}a?" 'lyskom-help)
  (define-key lyskom-mode-map "}ak" 'kom-review-comments)
  (define-key lyskom-mode-map "}ar" 'kom-review-tree)
  (define-key lyskom-mode-map "}h" 'kom-review-clear)
  (define-key lyskom-mode-map "}i" 'kom-review-last-normally-read)
  (define-key lyskom-mode-map "}n" 'kom-review-next)
  (define-key lyskom-mode-map "}o" 'kom-review-noconversion)
  (define-key lyskom-mode-map "}r" 'kom-find-root)
  (define-key lyskom-mode-map "}u" 'kom-find-root)
  (define-key lyskom-mode-map "}s" 'kom-review-by-to)
  (define-key lyskom-mode-map "B"  'kom-review-backward)
  (define-key lyskom-mode-map "}l" 'kom-review-stack)
  (define-key lyskom-mode-map "}p" 'kom-review-presentation)
  (define-key lyskom-mode-map "}t" 'kom-find-root-review)
  (define-key lyskom-mode-map "}m" 'kom-review-marked-texts)
  (define-key lyskom-mode-map "}am" 'kom-review-all-marked-texts)
  (define-key lyskom-mode-map "sm" 'kom-status-conf)
  (define-key lyskom-mode-map "sp" 'kom-status-person)

  ;; Running in buffer

  (define-key lyskom-mode-map "\033p" 'backward-text)
  (define-key lyskom-mode-map "\033n" 'forward-text)
  (define-key lyskom-mode-map "st" 'kom-save-text)

  (define-key lyskom-mode-map "\C-?" 'scroll-down)
)

;;;==============================================================
;;; Keymap for filter editing
;;;

(defvar lyskom-filter-edit-map nil
  "Keymap for LysKOM filter edit")

(if lyskom-filter-edit-map ()
  (setq lyskom-filter-edit-map (make-keymap))
  (suppress-keymap lyskom-filter-edit-map)
  (define-key lyskom-filter-edit-map "p" 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-filter-edit-map "P" 'lyskom-filter-edit-prev-entry)
  (define-key lyskom-filter-edit-map "n" 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-filter-edit-map "N" 'lyskom-filter-edit-next-entry)
  (define-key lyskom-filter-edit-map "\C-P" 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-filter-edit-map "\C-N" 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-filter-edit-map "\C-B" 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-filter-edit-map "\C-F" 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-filter-edit-map "\M-p" 'lyskom-filter-edit-prev-entry)
  (define-key lyskom-filter-edit-map "\M-n" 'lyskom-filter-edit-next-entry)
  (define-key lyskom-filter-edit-map "d" 'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-filter-edit-map "\M-d" 'lyskom-filter-edit-delete-entry)
  (define-key lyskom-filter-edit-map "D" 'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-filter-edit-map "\C-D" 'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-filter-edit-map "i" 'lyskom-filter-edit-insert-pattern)
  (define-key lyskom-filter-edit-map "I" 'lyskom-filter-edit-insert-pattern)
  (define-key lyskom-filter-edit-map "\M-i" 'lyskom-filter-edit-insert-entry)
  (define-key lyskom-filter-edit-map "<" 'lyskom-filter-edit-beginning-of-list)
  (define-key lyskom-filter-edit-map ">" 'lyskom-filter-edit-end-of-list)
  (define-key lyskom-filter-edit-map "\M-<" 'lyskom-filter-edit-beginning-of-list)
  (define-key lyskom-filter-edit-map "\M->" 'lyskom-filter-edit-end-of-list)
  (define-key lyskom-filter-edit-map "q" 'lyskom-filter-edit-quit)
  (define-key lyskom-filter-edit-map "x" 'lyskom-filter-edit-expunge)
  (define-key lyskom-filter-edit-map "s" 'lyskom-filter-edit-save)
  (define-key lyskom-filter-edit-map "g" 'lyskom-filter-edit-revert)
  (define-key lyskom-filter-edit-map "t" 'lyskom-filter-edit-toggle-permanent)
  (define-key lyskom-filter-edit-map "a" 'lyskom-filter-edit-toggle-action)
  (define-key lyskom-filter-edit-map "?" 'lyskom-filter-edit-brief-help)
  (define-key lyskom-filter-edit-map "h" 'lyskom-filter-edit-brief-help)
  )


(defvar lyskom-prioritize-mode-map nil
  "Keymap used in lyskom-prioritize-mode.")

(if lyskom-prioritize-mode-map
    nil
  (setq lyskom-prioritize-mode-map (make-keymap))
  (suppress-keymap lyskom-prioritize-mode-map)
  (define-key lyskom-prioritize-mode-map [mouse-2] 'kom-mouse-2)
  (define-key lyskom-prioritize-mode-map "\C-?" 'previous-line)
  (define-key lyskom-prioritize-mode-map " "    'next-line)
  (define-key lyskom-prioritize-mode-map "\C-k" 'kom-prioritize-kill)
  (define-key lyskom-prioritize-mode-map "\C-y" 'kom-prioritize-yank)
  (define-key lyskom-prioritize-mode-map "p"     'kom-prioritize-set-priority)
  (define-key lyskom-prioritize-mode-map "\C-c\C-c" 'kom-prioritize-quit)
  (define-key lyskom-prioritize-mode-map "q"     'kom-prioritize-quit)
  (define-key lyskom-prioritize-mode-map "S"     'kom-prioritize-quit)
  (define-key lyskom-prioritize-mode-map "u"     'kom-prioritize-move-up)
  (define-key lyskom-prioritize-mode-map "n"     'kom-prioritize-move-down)
)


;;; (defvar lyskom-prioritize-mode-map nil
;;;  "Keymap used in lyskom-prioritize-mode.")
;;;
;;;(if lyskom-prioritize-mode-map 
;;;    nil
;;;  (setq lyskom-prioritize-mode-map (make-keymap))
;;;  (suppress-keymap lyskom-prioritize-mode-map)
;;;  (define-key lyskom-prioritize-mode-map [mouse-2] 'kom-mouse-2)
;;;  (define-key lyskom-prioritize-mode-map [down] 'kom-prioritize-next-line)
;;;  (define-key lyskom-prioritize-mode-map "\C-n" 'kom-prioritize-next-line)
;;;  (define-key lyskom-prioritize-mode-map [up] 'kom-prioritize-previous-line)
;;;  (define-key lyskom-prioritize-mode-map "\C-p" 'kom-prioritize-previous-line)
;;;  (define-key lyskom-prioritize-mode-map [M-up] 'kom-prioritize-move-up)
;;;  (define-key lyskom-prioritize-mode-map [M-down] 'kom-prioritize-move-down)
;;;  (define-key lyskom-prioritize-mode-map "\M-p" 'kom-prioritize-move-up)
;;;  (define-key lyskom-prioritize-mode-map "\M-n" 'kom-prioritize-move-down)
;;;  (define-key lyskom-prioritize-mode-map "u" 'kom-prioritize-move-up)
;;;  (define-key lyskom-prioritize-mode-map "d" 'kom-prioritize-move-down)
;;;  (define-key lyskom-prioritize-mode-map "\M-<" 'kom-prioritize-beginning)
;;;  (define-key lyskom-prioritize-mode-map "\M->" 'kom-prioritize-end)
;;;  (define-key lyskom-prioritize-mode-map "r" 'kom-prioritize-reprioritize)
;;;  (define-key lyskom-prioritize-mode-map "g" 'kom-prioritize-goto-priority)
;;;  (define-key lyskom-prioritize-mode-map "s" 'kom-prioritize-set-priority)
;;;  (define-key lyskom-prioritize-mode-map "q" 'kom-prioritize-quit)
;;;  (define-key lyskom-prioritize-mode-map "\C-x\C-x" 'exchange-point-and-mark)
;;;  (define-key lyskom-prioritize-mode-map [C-SPC] 'set-mark-command)
;;;  (define-key lyskom-prioritize-mode-map "*" 'kom-prioritize-mark))

  
;;;; ============================================================
;;;; The default Ansaphone message goes here. The more complex 
;;;; message specification probably should too, but it's not here
;;;; yet. People who know how to use it are smart enough to do it
;;;; right.

(defvar kom-ansaphone-default-reply 
  "Jag l\344ser inte LysKOM just nu. Skicka gärna ett brev i stället."
  "*Default message to send when the ansaphone is on.

This variable is stored in the LysKOM server.")


;;;; ============================================================
;;;; Text buttom menuse go here. This will probably be moved back
;;;; to vars.el.in when the strings have been replaced by
;;;; symbols, but for now they'll stay here.


(defvar lyskom-button-actions
  '((text
     text-text
     lyskom-button-view-text
     (("Återse texten" . lyskom-button-view-text)
      ("Återse träd" . lyskom-button-review-tree)
      ("Återse urinlägget" . lyskom-button-find-root)
      ("Kommentera texten" . lyskom-button-comment-text)
      ("Personligt svar" . lyskom-button-private-comment-text)
      ("Markera texten" . lyskom-button-mark-text)
      ("Avmarkera texten" . lyskom-button-unmark-text))
     nil)
    (conf 
     conf-text
     lyskom-button-view-conf-presentation
     (("Visa presentation" . lyskom-button-view-conf-presentation)
      ("Visa mötesstatus" . lyskom-button-view-conf-status)
      ("Gå till mötet" . lyskom-button-goto-conf)
      ("Skicka gruppmeddelande" . lyskom-button-send-message)
      ("Bli medlem i mötet" . lyskom-button-add-self)
      ("Utträd ur mötet" . lyskom-button-sub-self))
     ((kom-list-news . lyskom-button-goto-conf)))
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
     nil))

"This variable defines valid button types in LysKOM. Each element is a
list consisting of (TYPE LABEL DEFAULT ACTIONS HINTS).
TYPE is the button type the entry defines
LABEL is a textual representation for the button type, used in menu titles. If
      it is a symbol, that symbol will be looked up using lyskom-get-string.
DEFAULT is the default action to take on a click. It must be a function.
ACTIONS are other possible actions. The format of this entry is described 
        below.
HINTS is a list of hints to override the default action. This is described 
      below.

The ACTIONS entry is used to construct a pop-up menu. It is a list consisting 
of lists with the format (STRING . FUNCTION). STRING is the menu label and
FUNCTION is the function to call when the menu item is selected.

The HINTS entry is used to generate hints that the default action should be 
overridden. It is a list containing elements (COMMAND . HINT) where COMMAND is
as interactive LysKOM command and HINT is a function to call. When a button
is generated while the command COMMAND is being executed, HINT is used as a 
hint for a new default action. The user has the option to ignore or used the 
hint.

Also see the function \"kom-add-button-action\".

This variable is not saved in the LysKOM server.")
        

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

(defvar kom-tell-phrases 
  '(
    (kom-tell-silence		"") ; Why ?
    (kom-tell-send		"F\366rs\366ker l\344gga in en text.")
    (kom-tell-login		"Loggar in.")
    (kom-tell-read		"L\344ser.")
    (kom-tell-1st-pres		"Skriver den f\366rsta presentationen.")
    (kom-tell-write-comment	"Skriver en kommentar.")
    (kom-tell-write-footnote	"Skriver en fotnot.")
    (kom-tell-write-letter	"Skriver ett brev.")
    (kom-tell-write-reply	"Skriver ett privat svar.")
    (kom-tell-write-text	"Skriver inl\344gg.")
    (kom-tell-conf-pres		"Skriver presentation f\366r ett nytt m\366te.")
    (kom-tell-recover		"\305terstartar kom. Suck.")
    (kom-tell-wait		"V\344ntar.")
    (kom-tell-regret		"\305ngrar sig och sl\344nger inl\344gget.")
    (kom-tell-review		"\305terser.")
    (kom-tell-change-name       "\304ndrar sitt namn till n\345got annat.")
    (kom-tell-change-supervisor "\304ndrar organisat\366r f\366r n\345got.")
    (kom-tell-next-lyskom       "Hoppar till ett annat LysKOM.")
    )
  "These texts are used as arguments to lyskom-tell-server.
The variable kom-mercial defaults to kom-tell-wait.
Users are encouraged to change this dictionary for fun.")

;; Placed here because this must NOT be evaluated before 
;; kom-tell-phrases is defined:

(defvar kom-mercial (car (cdr (assoc 'kom-tell-wait kom-tell-phrases)))
  "*When the user has seen all texts and has reached the view-time prompt,
this string is used as the argument to lyskom-tell-server.
Users are encouraged to use their best sense of humor.

This variable is stored in the LysKOM server.")

;;;


(defconst lyskom-error-texts
  '((0 . "Inget fel har intr\344ffat")
    (2 . "\304nnu ej implementerat")
    (3 . "Inte l\344ngre implementerat")
    (4 . "Felaktigt l\366senord")
    (5 . "Str\344ngen f\366r l\345ng")
    (6 . "Du \344r inte inloggad")
    (7 . "Ingen f\345r logga in i LysKOM just nu")
    (8 . "Du f\366rs\366kte anv\344nda m\366te nummer 0")
    (9 . "Odefinierat eller hemligt m\366te")
    (10 . "Odefinierad eller hemlig person")
    (11 . "Ingen skriv- eller l\344sr\344ttighet")
    (12 . "Otill\345ten operation")
    (13 . "Du \344r inte medlem i m\366tet")
    (14 . "Inget s\345dant textnummer")
    (15 . "Du kan inte anv\344nda globalt textnummer 0")
    (16 . "Inget s\345dant lokalt textnummer")
    (17 . "Du kan inte anv\344nda lokalt textnummer 0")
    (18 . "Namnet f\366r kort eller f\366r l\345ngt eller inneh\345ller felaktiga tecken")
    (19 . "Index utanf\366r gr\344nserna")
    (20 . "M\366tet existerar redan")
    (21 . "Personen existerar redan")
    (22 . "Hemligt, men ej l\344sskyddat")
    (23 . "Du f\345r inte \344ndra person/m\366tesflaggan")
    (24 . "Fel i databasen. Attans otur.")
    (25 . "Otill\345tet misc-f\344lt. (Internt fel)")
    (26 . "Otill\345ten infotyp. (Bug i klienten)")
    (27 . "Redan mottagare till denna text")
    (28 . "Redan kommentar till denna text")
    (29 . "Redan fotnot till denna text")
    (30 . "Inte mottagare till denna text")
    (31 . "Inte kommentar till denna text")
    (32 . "Inte fotnot till denna text")
    (33 . "F\366r m\345nga mottagare")
    (34 . "F\366r m\345nga kommentarer")
    (35 . "F\366r m\345nga fotnoter")
    (36 . "F\366r m\345nga markeringar")
    (37 . "Du \344r inte f\366rfattare till texten")
    (38 . "Du kan inte koppla upp dig till servern")
    (39 . "Minnet slut")
    (40 . "Servern har ballat ur")
    (41 . "Klienten tror att servern s\344ger att den inte f\366rst\345r klienten")
    (42 . "Ingen s\345dan session finns"))
  "All the errors reported from the server in plain text.")


     
;;;; This file contains the code that makes it possible to run a 
;;;; long-commands mode in the lyskom-buffer.
;;;;

;;; Author: Linus Tolke 

(defvar lyskom-slow-mode-map
  (make-sparse-keymap)
  "Mode map for the `slow' lyskom command mode.")

(define-key lyskom-slow-mode-map "\r" 'lyskom-parse-command-and-execute)

(defun lyskom-parse-command-and-execute ()
  "Reads a command from the last line in the buffer and executes it."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region lyskom-last-viewed (point-max))
    (search-backward lyskom-prompt-text))
  (forward-char (length lyskom-prompt-text))
  (while (looking-at "\\s-")
    (forward-char 1))
  (let* ((text (buffer-substring (point) (point-max)))
	 (completion-ignore-case t)
	 (alternatives (mapcar (function reverse)
			       (if kom-emacs-knows-iso-8859-1
				   lyskom-commands
				 lyskom-swascii-commands)))
	 (completes (all-completions text alternatives)))
    (cond
     ((zerop (length text))
      (kom-next-command))
     ((> (length completes) 1)
      (lyskom-insert "\nDu kan mena n}gon av f|ljande:\n")
      (mapcar (function (lambda (string) 
			  (lyskom-insert string)
			  (lyskom-insert "\n")))
	      completes)
      (lyskom-end-of-command))
     ((= (length completes) 1)
      (delete-region (point) (point-max))
      (call-interactively (car (reverse-assoc (car completes)
					      (if kom-emacs-knows-iso-8859-1
						  lyskom-commands
						lyskom-swascii-commands)))))
     (t
      (lyskom-insert "Det finns inget s}dant kommando.\n")
      (lyskom-end-of-command)))
  ))


(defun kom-slow-mode ()
  "Starts the slow-command-mode."
  (interactive)
  (lyskom-start-of-command 'kom-slow-mode)
  (use-local-map lyskom-slow-mode-map)
  (lyskom-end-of-command))

(defun kom-quick-mode ()
  "Starts the quick-command-mode."
  (interactive)
  (lyskom-start-of-command 'kom-quick-mode)
  (use-local-map lyskom-mode-map)
  (lyskom-end-of-command))


;; Review a non-converted text
;; Author: Linus Tolke
 

(defun kom-review-noconversion (&optional text-no)
  "Displays the last read text without any conversion."
  (interactive (list 
		(cond
		 ((null current-prefix-arg)
		  lyskom-current-text)
		 ((integerp current-prefix-arg)
		  current-prefix-arg)
		 ((and (listp current-prefix-arg) 
		       (integerp (car current-prefix-arg)) 
		       (null (cdr current-prefix-arg)))
		  (car current-prefix-arg))
		 (t
		  (signal 'lyskom-internal-error '(kom-review-noconversion))))))
  (lyskom-start-of-command 'kom-review-noconversion)
  (let ((kom-emacs-knows-iso-8859-1 t))
    (lyskom-view-text text-no))
  (lyskom-end-of-command))


;; Vars}god sojge, en massa j{ttebra funktioner.

(defun kom-put-file (filename)
  (interactive "fVilken fil vill du ladda upp? ")
  (copy-file filename (concat "/ftp@ftp.lysator.liu.se:/open/" 
			      (file-name-nondirectory filename))))

(defun kom-get-file (filename)
  (interactive "sVilken fil vill du ladda ner? ")
  (copy-file (concat "/ftp@ftp.lysator.liu.se:/open/" filename) filename))

(defun kom-list-files ()
  (interactive)
  (list-directory "/ftp@ftp.lysator.liu.se:/open"))
