;;;;;
;;;;; $Id: swedish-strings.el,v 35.11 1991-10-07 15:34:26 linus Exp $
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
	      "$Id: swedish-strings.el,v 35.11 1991-10-07 15:34:26 linus Exp $\n"))


(defvar lyskom-header-separator "--- Skriv nedan. Skicka in=C-c C-c, Avbryt=C-c C-k, Annat se C-h m ---"
  "*String to separate headers from text body.")


(defvar lyskom-header-subject "�rende: "
  "*String to prompt for subject in the edit buffer.")


(defconst lyskom-strings 
  '(
    ; From vars.el: 
    ; From komtypes.el: nil
    ; From clienttypes.el: nil
    ; From startup.el:
    (server-q . "Lyskom-server? (%#1s) ")
    (try-connect . "LysKOM elisp-klient version %#1s.\nF�rs�ker koppla upp mot %#2s.\n")
    (protocoll-error . "Protocol error. Servers says: %#1s")
    (connection-done . "Uppkopplingen klar. Serverns versionsnummer �r %#1s.\n\n")
    (what-is-your-name . "Vad heter du? ")
    (password . "L�senord? ")
    (wrong-password . "Fel l�sen.\n")
    (are-logged-in . "Du �r nu inloggad. V�nta ett tag.\n")
    (you-have-motd . "\nDu har en lapp p� d�rren:\n\n")

    (first-greeting . "%#1s
Det tycks vara f�rsta g�ngen du anv�nder LysKOM. V�lkommen!
Kontrollera att du stavat ditt namn r�tt.  Anv�nd g�rna ditt fullst�ndiga
namn och organisation, t ex \"Eskil Block, FOA\".  Om du stavat ditt namn
fel, eller vill �ndra ditt namn, svara nej p� fr�gan nedan.

Observera att all information �n s� l�nge sparas s� att vem som helst
kan l�sa den. Till exempel s� kan vem som helst se vilket l�senord
du anv�nder.

Om du �r os�ker p� hur man anv�nder LysKOM kan du h�mta en manual via
anonym ftp fr�n ftp.lysator.liu.se. Fr�ga din systemadministrat�r om
du �r os�ker.\n")

    (is-name-correct . "�r namnet %#1s korrekt? ")
    (personal-password . "Ange ett personligt l�sen: ")
    (repeat-password . "Repetera f�r kontroll: ")
    (repeat-failure . "Du angav inte samma l�senord b�gge g�ngerna.\n")

    (could-not-create-you .  "Det gick inte att skapa personen.\n")
    (presentation-subject . "%#1s")
    (presentation-form . "Namn:\t\nAdress:\t\n\t\nTelefon:
Email-adress:\n\nAnnat:\t")
    (not-present-anywhere . "Ej n�rvarande i n�got m�te.")



    ; From internal.el:
    (shaky-tcp . "Nu n�r jag inte servern. TCP/IP-f�rbindelsen �r skakig%#1s")
    (retrying-tcp . "Jag f�rs�ker igen.")

    ; From parse.el:
    ; No entries.

    ; From services.el:
    ; No entries.

    ; From cache.el:
    ; No entries.

    ; From commands1.el:
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
    (confirm-delete-pers-or-conf . "Ta bort %#1s %#2s? ")
    (the-pers . "personen")
    (the-conf . "m�tet")
    (deletion-not-confirmed . "Utpl�ningen avbruten\n")
    (somebody-else-deleted-that-conf . "N�gon annan tog precis bort m�tet.\n")
    (conf-is-deleted . "Ok, nu �r %#1s utpl�nad.\n")
    (you-could-not-delete . "%#1s kunde inte utpl�nas av dig.\n")

    (what-text-to-delete . "Vilket inl�gg skall tas bort?")
    (deleting-text . "Radering av text %#1d...")

    (presentation-for-whom . "Vilket m�te/person? ")
    (somebody-deleted-that-conf . "N�gon tog precis bort m�tet.\n")
    (review-presentation-of . "�terse presentation av %#1s.\n")
    (has-no-presentation . "%#1s har ingen presentation.\n")

    (have-to-read . "Du m�ste l�sa ett inl�gg f�rst.\n")

    (review-text-no . "�terse text nummer %#1d.\n")
    (no-comment-to . "Det finns inget kommenterat inl�gg att titta p�.\n")

    (who-letter-to . "Vem vill du skicka brev till? ")
    (has-motd . "%#1s har en lapp p� d�rren:\n\n")
    (motd-persist-q . "Vill du fortfarande skicka brevet? ")

    (who-to-add . "Vem vill du addera? ")
    (where-to-add . "Vilket m�te skall han/hon adderas till? ")
    (where-to-add-self . "Vilket m�te vill du bli medlem i? ")
    (priority-q . "Prioritet p� m�tet? (0 (l�g) - 255 (h�g)) ")
    (done . "klart.\n")
    (nope . "Det gick inte.\n")

    (conf-does-not-exist . "\nM�tet finns inte.\n")

    (who-to-exclude . "Vem vill du utesluta? ")
    (where-from-exclude . "Vilket m�te skall han/hon uteslutas ifr�n? ")

    (leave-what-conf . "Vilket m�te vill du g� ur? ")

    (error-fetching-person . "Fel i personh�mtningen\n")
    (error-fetching-conf . "Fel i m�tesh�mtningen.\n")

    (name-of-conf . "Vad ska m�tet heta? ")
    (anyone-member . "F�r vem som helst bli medlem? ")
    (secret-conf . "Hemligt m�te? ")
    (comments-allowed . "F�r man skriva kommentarer? ")
    (confusion-what-to-comment . "Jag f�rst�r inte vilken text du vill kommentera.\n")
    (confusion-what-to-footnote . "Jag f�rst�r inte vilken text du vill skriva en fotnot till.\n")

    (confusion-who-to-reply-to . "Jag f�rst�r inte vems inl�gg du vill skriva ett privat svar till.\n")
    (confusion-what-to-answer-to . "Jag f�rst�r inte vilken text du vill besvara.")
    (really-quit . "Vill du verkligen avsluta sessionen? ")
    (session-ended . "
*****************************
Lyskom-sessionen �r avslutad.
*****************************\n")
    (what-to-change-pres-you . "Vilket m�te/person vill du �ndra presentationen f�r (dig sj�lv): ")
    (who-to-put-motd-for . "Vilket m�te/person vill du s�tta lapp p� d�rr f�r (dig sj�lv): ")

    (cant-get-conf-stat . "Kan ej h�mta m�tesstatus f�r m�tet.")
    (go-to-conf . "G� till m�te: ")
    (want-become-member . "Vill du bli medlem? ")
    (no-ok . "Nehej.\n")

    (who-to-remove-motd-for . "Vilket m�te/person vill du ta bort lapp p� d�rr f�r: ")

    (conf-all-read . "%#1s - inga ol{sta.\n")
    (no-in-conf . "Du �r inte n�rvarande i n�got m�te.\n")

    (search-for-pers . "Ange s�kvillkor (RETURN f�r alla personer) ")
    (search-for-conf . "Ange s�kvillkor (RETURN f�r alla m�ten) ")
    (name-to-be-changed . "Ange det namn som skall �ndras: ")
    (no-such-conf-or-pers . "M�tet eller personen finns ej.\n")
    (new-name . "Nytt namn: ")
    (who-to-change-supervisor-for . "Vem vill du �ndra organisat�r f�r? ")
    (new-supervisor . "Ny organisat�r: ")
    (text-to-mark . "Vilket inl�gg vill du markera? ")
    (text-to-unmark . "Vilket inl�gg vill du avmarkera? ")
    (what-mark . "Vilken markering vill du s�tta? ")
    (unmarking-textno . "Avmarkering av text %#1d...")
    (marking-textno . "Markering av text %#1d...")

    (new-passwd-again . "Mata in det nya l�senordet igen f�r kontroll: ")
    (what-mark-to-view . "Vilken markering vill du �terse? ")
    (whos-passwd . "Vem vill du {ndra l�senord f|r (dig sj�lv)? ")
    (old-passwd . "Mata in ditt l�senord: ")
    (new-passwd . "Mata in det nya l�senordet: ")
    (changing-passwd . "�ndrar l�senordet...")
    (retype-dont-match . "L�senorden �r inte samma. G�r om.\n")
    (palindrome . "(ett palindrom!) ")
    (lyskom-name . "Anv�ndare")
    (is-in-conf . "N�rvarande i m�te")
    (from-machine . "K�r fr�n")
    (is-doing . "G�r")
    (text-to-add-recipient . "Vilket inl�gg vill du addera mottagare till")
    (text-to-delete-recipient . "Vilket inl�gg vill du subtrahera mottagare fr�n")
    (text-to-add-comment-to . "Vilket inl�gg vill du addera en kommentar till")
    (text-to-delete-comment-from . "Vilket inl�gg vill du subtrahera en kommentar fr�n")

    (where-on-list-q . "Placering p� listan? (0-%#1d) ")
    (member-in-conf . "Bli medlem i %#1s...")
    (add-member-in . "Addera %#1s som medlem i %#2s...")
    (unsubscribe-to . "Uttr�da ur %#1s...")

    (exclude-from . "Utesluta %#1s fr�n %#2s...")

    (unsubscribe-failed . "\nDet gick inte. %#1s var kanske aldrig medlem i %#2s?\n")

    (You . "Du")
    (could-not-create-conf . "M�tet \"%#1s\" kunde ej skapas.\nFelkod: %#2d.\n")
    (created-conf-no-name . "M�te nummer %#1d %#2s, skapat.\n")
    (cant-read-textno . "Du f�r inte l�sa text %#1d")

    (not-supervisor-for . "Du �r inte organisat�r f�r %#1s.\n")
    (not-supervisor-for . "Du �r inte organisat�r f�r %#1s.\n")
    (go-to-conf . "G� till %#1s.\n")
    (cant-go-to-his-mailbox . "Du f�r inte g� till %#1ss brevl�da.\n")
    (not-member-of-conf . "Du �r inte medlem i %#1s.\n")
    (change-name-done . "\nKlart. Nytt namn: %#1s.\n")
    (change-name-nope . "\nDet gick inte bra. Kanske n�got annat redan heter %#1s?\n")
    (change-supervisor-from-to . "�ndra organisat�r f�r %#1s till %#2s...")
    (change-supervisor-nope . 
     "\nDet gick inte. Kanske du inte f�r �ndra organisat�r f�r %#1s?\n")
    
    (no-marked-texts . "Du har inga markerade inl�gg.\n")
    (no-marked-texts-mark . 
     "Du har inga markerade inl�gg med markeringen %#1d.\n")
    (time-is . "Klockan �r %4#1d-%02#2d-%02#3d %02#4d:%02#5d:%02#6d %#7s(enligt servern).\n")
    (total-users . "    Sammanlagt %#1d anv�ndare.\n")
    (who-to-add-q . "Vilket m�te/person vill du addera som mottagare? ")
    (who-to-sub-q . "Vilket m�te/person vill du subtrahera som mottagare? ")

    (adding-name-as-recipient . "Adderar %#1s som mottagare till text %#2d...")
    (remove-name-as-recipient .
     "Subtraherar %#1s som mottagare fr�n text %#2d...")
    (text-to-add-q . "Vilket inl�gg vill du addera som kommentar? ")
    (text-to-remove-q . "Vilket inl�gg vill du subtrahera som kommentar? ")
    (add-comment-to . "Adderar text %#1d som kommentar till text %#2d...")
    (sub-comment-to . "Subtraherar text %#1d som kommentar till text %#2d...")

    ; From commands2.el:

    (your-memberships . "Ditt medlemskap i olika KOM-m�ten:\n")
    (memberships-header . "Senast inne	   Prio	Ol�sta	M�tesnamn\n")
    
    (conf-for-status . "Vilket m�te vill du se statusen f�r? ")
    (no-such-conf . "M�tet finns ej.")
    (status-record . "Status f�r m�te %#1s (%#2s) %#3s\n\n")
    (Mailbox . "Brevl�da")
    (Protected . "Skyddat")
    (no-comments . "original")
    (closed . "slutet")

    (created-by . "Skapat av person %#1s %#2s\n")
    (created-at . "Skapad:%35#1s\n")
    (members .    "Antal medlemmar: %25#1d\n")
    (garb-nice . "Livsl�ngd p� inl�gg (dagar):%14#1d\n")
    (lowest-local-no . "L�gsta existerande lokala nummer: %8#1d\n")
    (highest-local-no . "H�gsta existerande lokala nummer: %8#1d\n")
    (last-text-time . 
     "Tid f�r senaste inl�gg: %18#1s (st�r det i din cache)\n")
    (no-of-motd . "Lapp p� d�rren i text nummer: %12#1d\n")
    (superconf-is-no-name . "Superm�te:       %25#1d %#2s\n")
    (permitted-submitters-no-name . "Till�tna f�rfattare:%22#1d %#2s\n")
    (supervisor-is-no-name . "Organisat�r:     %25#1d %#2s\n")
    (presentation-no . "Presentation:    %25#1d\n")
    (conf-has-motd . "\n%#1s har en lapp p� d�rren:\n")

    (Everybody . "Alla")
    (show-members-list-also-q . "Vill du se medlemslistan ocks�? ")
    (conf-has-these-members . "\n%#1s har f�ljande medlemmar:\n")
    (member-list-header . "Senast inne         Osett  Namn\n\n")
    (secret-membership . "*** Hemlig rad ***\n")
    (pers-for-status . "Vem vill du se statusen f�r? ")
    (no-such-pers . "Det finns ingen s�dan person.")
    (pers-status-record . "Status f�r person %#1s (%#2d)\n")
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
    (time-for-last-letter . "Tid f�r senaste brev:%20#1s (st�r det i din cache)\n")
    (superconf . "Superm�te:%31#1d %#2s\n")
    (supervisor . "Organisat�r:%29#1d %#2s\n")
    (member-of-confs . "Medlem i (antal m�ten):%18#1d\n")
    (presentation . "Presentation:    %24#1d\n")
    (show-membership-list-also-q . "Vill du se vilka m�ten personen �r medlem i ocks�? ")
    (not-allowed-see-confs . "Du f�r inte se vilka m�ten %#1s �r medlem i.\n")
    (is-member-of . "\n%#1s �r medlem i f�ljande m�ten:\n")
    (membership-list-header . "Senast inne         Osett  Namn\n\n")
    (who-to-send-message-to . "Vem vill du skicka meddelandet till? (Alla) ")
    (his-total-unread . "\n%#1s har totalt %#2d ol�sta.\n")
    (message-prompt . "Meddelande: ")
    (message-sent . "Meddelandet s�nt.\n")
    (message-nope .
  "Du kunde inte skicka meddelandet. Mottagaren var kanske inte inloggad.\n")
    (only-last . "Endast l�sa senaste (0 - %#1d) i %#2s? ")
    (only-error . "N�t gick galet. Sorry.\n")

    (you-have-unreads . "Du har %#1d ol�sta inl�gg i %#2s\n")
    (you-have-an-unread . "Du har 1 ol�st inl�gg i %#1s\n")
    (you-have-read-everything . "Du har sett alla nyheter\n")
    (total-unreads . "\nDu har %#1d ol�sta inl�gg.\n")
    (total-unread . "\nDu har 1 ol�st inl�gg.\n")
    (waiting-for-anything .
     "Du v�ntar p� ett inl�gg i vilket m�te som helst.\n")
    (waiting-higher-than . 
     "Du v�ntar p� ett inl�gg i ett m�te med h�gre prioritet �n %#1d.\n")

    (have-to-be-in-conf . "Du m�ste g� till ett m�te f�rst.\n")
    (Texts . "Inl�gg")
    (Date . "Datum")
    (Lines . "Rader")
    (Author . "  F�rfattare")
    (Subject . "  �rende")
    (could-not-read . "Du fick inte l�sa denna text (%#1d).\n")
    (multiple-choice . "Flera alternativ finns.")
    (does-not-exist . "Detta kommando finns inte.")

    ; Only people fixing bugs or recieving bugg-reports should change these:
    (buggreport-description . "Detta gjorde jag:
(Fyll i dina kommenarer)\n\n\n
N{r du skrivit klart skall du skicka in din buggrapport till
LysKOM-utvecklarna. Det sker antingen med email till 
bug-lyskom@lysator.liu.se eller i Lysators LysKOM till m|tet
\"LysKOM; Elispklientens buggrapporter\" eller om inget annat
fungerar med vanligt brev till:
Lysator, c/o ISY, Linkoping University, S-581 83 Linkoping, SWEDEN.
M{rk kuvertet LysKOM buggrapport.\n\n")
    (buggreport-internals . "LysKOMs interna information:\n\n")
    (buggreport-version . "lyskom-version:")
    (buggreport-pending-calls . "lyskom-pending-calls:")
    (buggreport-call-data . "\nlyskom-call-data:")
    (buggreport-unparsed . "\nlyskom-unparsed-buffer:")
    (buggreport-reading-list . "\nlyskom-reading-list:")
    (buggreport-to-do-list . "\nlyskom-to-do-list:")
    (buggreport-backtrace . "\n*Backtrace*:\n%#1s\n")
    (buggreport-communications . "\nlyskom-debug-communications-to-buffer-buffer:")
    (buggreport-instead-of-byte-comp . "byte-code(\"byte-string\"")
    (buggreport-subject . "Bugg-rapport elisp-klienten version %#1s")


    (not-logged-in . "Du �r inte inloggad.  ")
    (name-is-not-in-conf . "%#1s �r inte n�rvarande i n�got m�te.\n")
    (name-is-in-conf . "%#1s �r n�rvarande i\n%#2s\n")
    (connected-during . "Uppkopplingstid: %#1d sekunder.\n")
    (conf-to-set-garb-nice-q . "Vilket m�te vill du s�tta livsl�ngd f�r? ")
    (new-garb-nice-q . "Vilket v�rde vill du s�tta livsl�ngden till? ")
    (garb-nice-for-is . "�ndra livsl�ngden f�r %#1s till %#2d...")

    (really-shutdown . "�r du s�ker p� att du vill st�nga av servern? ")
    (closing-server . "St�nga av servern...")
    (administrator . "administrat�r")
    (no-longer-administrator . "en normal anv�ndare igen")
    (you-are-now . "Ok, du k�r nu som %#1s.\n")
    (setting-motd . "S�tter loginmeddelandet till text %#1d.\n")
    (set-motd-success . "Du har satt ett nytt loginmeddelande.\n")
    (set-motd-failed . "Det gick inte. Du var kanske inte administrat|r.\n")
    (removing-motd . "Tar bort loginmeddelandet.\n")
    (removed-motd . "Du har tagit bort loginmeddelandet.\n")
    (who-to-throw-out . "Vilken session vill du kasta ut? ")
    (throwing-out . "Kastar nu ut session %#1d... ")
    (postpone-prompt . "Hur lite vill du l�sa nu? ")

    ; From review.el:
    (latest-n . "senaste %#1d")
    (first-n . "f�rsta %#1d")
    (info-by-whom . "%#1s av vem: ")
    (info-to-conf . "%#1s till m�te: ")
    (info-by-to . "%#1s av %#2s till %#3s fram�t.")
    (all-confs . "alla m�ten")
    (no-get-conf . "Du f�r inte h�mta m�tet.\n")
    (no-get-pers . "Du f�r inte h�mta personen.\n")
    (no-review-info . "Ej till�tet �terse %#1s\n")
    (review-info . "�terse %#1s\n")
    (you-review . "Du �terser nu %#1s.\n")
    (read-text-first . "Du m�ste l�sa en text f�rst.\n")
    (cannot-read-last-text . "Du kan inte l�sa den senast l�sta texten.\n")
    (review-n-texts . "�terse %#1d inl�gg.\n")
    (review-marked . "�terse %#1d markerade.\n")
    (review-text-no . "�terse text nummer %#1d")
    (review-one-comment . "�terse en kommentar till inl�gg %#1d.\n")
    (review-many-comments . "�terse %#2d kommentarer till inl�gg %#1d.\n")

    ; From edit-text.el:
    (press-C-c-C-c . "Tryck C-c C-c f�r att skicka in texten.")
    (recipient . "Mottagare:")
    (carbon-copy . "Extra kopia:")
    (text-mass . "%#4s%#1s\n%#2s\n%#3s")
    (comment-to-by . "%#1s till text %#2d%#3s.\n")
    (already-sent . "Du har redan skickat denna text en g�ng. S�nd �nd�? ")
    (subject . "�rende: ")
    (which-text-include . "Vilken text skall vi inkludera? ")
    (added-recipient . "Mottagare som skall adderas: ")
    (added-carbon-copy . "Extra kopia till m�te: ")
    (conf-has-motd-no . "M�tet har en lapp p� d�rren. (%#1d)\n\n%#2s")
    (still-want-to-add . "Vill du fortfarande addera m�tet? ")
    (could-not-create-text . "\nTexten kunde ej skapas. Felkod: %#1d.\n")
    (no-get-text . "Du fick inte h�mta texten.")


    ; From view-text.el:
    (line . " /1 rad/ ")
    (lines ." /%#1d rader/ ")

    (marked-by-you . "Markerad av dig.\n")
    (marked-by-you-and-one . "Markerad av dig och n�gon annan.\n")
    (marked-by-you-and-several . "Markerad av dig och %#1d andra.\n")
    (marked-by-one . "Markerad av 1 person.\n")
    (marked-by-several . "Markerad av %#1d personer.\n")

;; The format of this function should coincide with the format of the 
;; lyskom-text-start variable. DONT change one without changing the other.

; used by lyskom-print-time
    (time-y-m-d-h-m . "%4#1d-%02#2d-%02#3d  %02#4d:%02#5d ")
; used by lyskom-return-time
    (time-yyyy-mm-dd-hh-mm . "%4#1d-%02#2d-%02#3d %02#4d:%02#5d")
    (no-such-text-no . "Det finns ingen s�dan text. (%#1d)\n")

    (head-Subject . "�rende: ")
    (Recipient . "Mottagare")
    (Extra-recipient . "Extra kopia")
    (send-at . "    S�nt:     %#1s\n")
    (sent-by . "    S�nt av ")
    (recieved-at . "    Mottaget: %#1s\n")
    (written-by . "av %#1s")
    (comment-to-text-by . "Kommentar till text %#1d %#2s\n")
    (footnote-to-text-by . "Fotnot till text %#1d %#2s\n")
    (comment-in-text-by . "Kommentar i text %#1d %#2s\n")
    (footnote-in-text-by . "Fotnot i text %#1d %#2s\n")

    ; From async.el:

    (name-has-changed-to-name . "%#1s har nu bytt namn till %#2s")
    (name-has-changed-to-name-r . "%#1s har nu bytt namn till %#2s\n")
    (database-sync . "Databasen synkas.")
    (lyskom-is-full . "\
===========================================================
Meddelande fr�n LysKOM-systemet: N�gon f�rs�kte koppla upp,
men misslyckades eftersom alla tillg�ngliga f�rbindelser �r
upptagna. Logga ut och kom tillbaks senare om du v�ntar nu.
===========================================================\n")
    (has-entered . "Nu har %#1s g�tt in i LysKOM.")
    (has-entered-r . "Nu har %#1s g�tt in i LysKOM.\n")
    (has-left . "Nu har %#1s g�tt ur LysKOM.")
    (has-left-r . "Nu har %#1s g�tt ur LysKOM.\n")
    (unknown . "ok�nd")

    (message-broadcast . "Allm�nt meddelande fr�n %#1s (%#3s):\n\n%#2s\n")
    (message-from . "Personligt meddelande fr�n %#1s (%#3s):\n\n%#2s\n")
    (text-is-created . "Text %#1d �r skapad!")


    ; From completing-read.el:

    (person-or-conf-no-regexp . "\\`[ \t]*[mpMP]\\w*[ \t]+\\([0-9]+\\)\\'")

    ; From prioritize.el:
    (cannot-get-membership . "Kan ej h�mta medlemsskap f�r dig.")
    (cannot-get-pers-stat . "Kan ej h�mta personstatus f�r dig.")
    (your-membship . "Ditt medlemsskap i olika KOM-m�ten:
  Prio M�tesnr M�tesnamn\n")
    (prio-row . " %5#1d%5#2d  %#3s\n")
    (too-high-goto-2 . "Du st�r f�r h�gt upp. G� ner till rad 2.")
    (too-low-go-up . "Du kan inte pusha sista raden. G� upp en rad.")
    (all-confs-popped .  "Alla m�ten �r poppade.")
    (prio-died . "Flyttningen misslyckades. Sorry. D�da bufferten.")
    (new-priority . "Ny prioritet? (0 (l�g) - 255 (h�g)) ")
    (new-prio . "%6#1d")

    ; From flags.el:
    (saving-settings . "Sparar flaggorna")
    (hang-on . "V�nta ett tag...\n")
    (could-not-create-area . "Kunde ej skapa texten.\n")
    (could-not-set-user-area . "Kunde ej st�lla om user-arean. Servern s�ger felmeddelande: %#1d\n")
    (you-dont-exist . "Du finns inte.\n")

    ; From elib-string.el:
    ; No entries.

    ; From lyskom-rest.el:

    (error-code . "Felkod %#2d: %#1s.\n")
    (extended-command . "LysKOM: ")
    (wait-for-server . "LysKOM v�ntar p� svar fr�n servern. V�nta tills du f�r en prompt.\n")
    (review-text-q . "�terse text nummer: ")
    (review-text-no . "�terse text nummer %#1d.\n")

    (completely-read-conf . "Du har sett alla texter i detta m�te.\n")
    (not-in-any-conf . "Du l�ser inte n�got m�te just nu.\n")

    (all-conf-unread-r . "Du har l�st ut alla m�ten.\n")
    (all-conf-unread-s . "Du har l�st ut alla m�ten. ")
    (one-unread . " - 1 ol�st\n")
    (several-unread . " - %#1d ol�sta\n")

    (save-on-file-q . "Spara inl�gg p� fil: (%#1s) ")
    (wait-for-prompt . "V�nta p� prompten!")
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
    (the-command . "Kommandot:%#1s")
    (error-in-do-when-starting . "Fel i din kom-do-when-starting. %#1s\n")

    (give-a-number . "Mata in ett tal: ")

    (yes-regexp . "\\`[jJ][aA]\\'")
    (no-regexp . "\\`[nN][eE][jJ]\\'")
    (yes-or-no-nag . "Svara bara ja eller nej.")
    (yes-or-no . "(ja eller nej) ")

    (y-or-n-instring . "jJnN ")
    (j-or-n-nag . "Svara bara j eller n. ")
    (j-or-n . "(j eller n) ")
    (y-instring . "jJ ")

    (person-does-not-exist . "Person %#1d (finns inte).")
    (conf-does-not-exist . "M�te %#1d (finns inte).")

    (process-signal . "Signal fr�n processen.")
    (closed-connection . "
**************************************************
Lyskom-sessionen onormalt st�ngd.
Felmeddelande: %#1s**************************************************")
    (error-not-found . "Fel nummer %#1d. Ingen klartextf�rklaring finns.")

    ; Useful in more place than one:
    (illegal-command . "Otill�tet kommando.\n")
    (no-such-text . "Det finns inget s�dant inl�gg.\n")
    (no-such-text-m . "Det finns inget s�dant inl�gg.")
    (everybody . "alla")
    (anybody . "vem som helst")
    (forward . "fram�t")
    (backward . "bak�t")
    (wait . "V�nta ett tag...\n")
    (comment . "Kommentar")
    (footnote . "Fotnot")
    (by . " av %#1s")
    (text-created .  "Text nummer %#1d �r skapad.\n")
    )
  "Assoc list containing pairs of atoms and strings")



;;; ================================================================
;;;              The commands and their associated functions


(defconst lyskom-commands
  '(
    (describe-mode              "Hj�lp")
    (kom-send-message           "S�nda meddelande")
    (kom-create-conf		"Skapa m�te")
    (kom-delete-conf            "Utpl�na")
    (kom-delete-text            "Radera inl�gg")
    (kom-display-time		"Se tiden")
    (kom-go-to-conf		"G� till m�te")
    (kom-go-to-next-conf	"G� till n�sta m�te")
    (kom-jump			"Hoppa �ver alla kommentarer")
    (kom-list-conferences 	"Lista m�ten") 
    (kom-list-persons		"Lista personer")
    (kom-list-news              "Lista nyheter")
    (kom-membership		"Lista medlemsskap")
    (kom-postpone		"Uppskjuta l�sning")
    (kom-prioritize		"Prioritera m�ten")
    (kom-status-person		"Status (f�r) person")
    (kom-status-conf		"Status (f�r) m�te")
    (kom-add-self		"Bli medlem i m�te")
    (kom-list-summary		"Lista �renden")
    (kom-sub-self               "Uttr�da ur m�te")
    (kom-quit			"Sluta")
    (kom-recover		"�terstarta kom") 
    (kom-start-anew		"B�rja med nytt namn")
    (kom-view			"�terse inl�gg")
    (kom-find-root-review	"�terse tr�d")
    (kom-review-comments	"�terse alla kommentarer")
    (kom-review-tree		"�terse alla kommentarer rekursivt")
    (kom-review-clear           "�terse hoppa")
    (kom-review-next            "�terse n�sta")
    (kom-find-root		"�terse urinl�gget")
    (kom-review-by-to           "�terse senaste")
    (kom-view-commented-text    "�terse det kommenterade")
    (kom-review-stack           "�terse lista")
    (kom-review-presentation    "�terse presentation")
    (kom-review-backward        "(�terse) Bakl�nges")
    (kom-view-next-text		"L�sa n�sta inl�gg")
    (kom-who-is-on		"Vilka �r inloggade") 
    (kom-display-who-buffer	"Visa vilkalistan")
    (kom-busy-wait		"V�nta p� ett inl�gg")
    (kom-write-comment		"Kommentera inl�gget")
    (kom-comment-previous	"Kommentera f�reg�ende inl�gg")
    (kom-write-footnote         "Fotnot till inl�gg")
    (kom-private-answer         "Personligt svar")
    (kom-set-unread		"Endast l�sa senaste")
    (kom-write-text		"Skriva ett inl�gg")
    (kom-send-letter		"Skicka brev")
    (kom-change-name		"�ndra namn")
    (kom-change-password        "�ndra l�senord")
    (kom-change-supervisor	"�ndra organisat�r")
    (kom-change-presentation	"�ndra presentation")
    (kom-get-appreciation       "F� uppmuntran")
    (kom-get-abuse              "F� sk�ll")
    (kom-mark-text              "Markera (inl�gg)")
    (kom-unmark-text            "Avmarkera (inl�gg)")
    (kom-review-marked-texts    "�terse markerade")
    (kom-review-all-marked-texts "�terse alla markerade")
    (kom-add-recipient          "Addera mottagare")
    (kom-sub-recipient          "Subtrahera mottagare")
    (kom-add-comment		"Addera kommentar")
    (kom-sub-comment		"Subtrahera kommentar")
    (kom-add-member		"Addera medlem")
    (kom-sub-member             "Uteslut medlem")
    (kom-change-conf-motd	"S�tt lapp p� d�rren")
    (kom-set-garb-nice          "�ndra livsl�ngd")
    (kom-unset-conf-motd	"Ta bort lapp p� d�rren")
    (kom-save-text		"Spara text (p� fil)")
    (kom-edit-options		"�ndra variabler")
    (kom-shutdown-server        "St�ng av servern")
    (kom-enable-adm-caps        "�verg� till administrat�rsmod")
    (kom-disable-adm-caps       "�verg� till normalmod")
    (kom-set-motd               "S�tt loginmeddelande")
    (kom-remove-motd            "Ta bort loginmeddelande")
    (kom-force-logout           "Kasta ut en session")
    )
  "A list of LysKOM-commands that the extended parser understands.")


(defvar lyskom-text-start "
[0-9]+ 199[0-9]-[0-1][0-9]-[0-3][0-9] +[0-2][0-9]:[0-5][0-9]  /[0-9]+ rad\\(er\\)?/ "
  "Regexp matching beginning of a text in lyskom buffer.
Cf. paragraph-start.")


(if lyskom-mode-map
    nil
  (setq lyskom-mode-map (make-keymap))
  (suppress-keymap lyskom-mode-map)
  (fset 'lyskom-review-prefix (make-keymap))
  (fset 'lyskom-change-prefix (make-keymap))
  (fset 'lyskom-next-prefix (make-keymap))
  (fset 'lyskom-list-prefix (make-keymap))
  (fset 'lyskom-get-prefix (make-keymap))
  (fset 'lyskom-S-prefix (make-keymap))
  (define-key lyskom-mode-map "{" 'lyskom-change-prefix)
  (define-key lyskom-mode-map "[" 'lyskom-change-prefix)
  (define-key lyskom-mode-map "\344" 'lyskom-change-prefix) ; 8-bit emacs.
  (define-key lyskom-mode-map "\304" 'lyskom-change-prefix)
  (define-key lyskom-mode-map "\033{" 'lyskom-change-prefix) ; 7(8)-bit emacs.
  (define-key lyskom-mode-map "\033[" 'lyskom-change-prefix) 
  (define-key lyskom-mode-map "}" 'lyskom-review-prefix)
  (define-key lyskom-mode-map "]" 'lyskom-review-prefix)
  (define-key lyskom-mode-map "\345" 'lyskom-review-prefix)
  (define-key lyskom-mode-map "\305" 'lyskom-review-prefix)
  (define-key lyskom-mode-map "\033}" 'lyskom-review-prefix) ; 7(8)-bit emacs.
  (define-key lyskom-mode-map "\033]" 'lyskom-review-prefix) 
  (define-key lyskom-mode-map "f" 'lyskom-get-prefix)
  (define-key lyskom-mode-map "n" 'lyskom-next-prefix)
  (define-key lyskom-mode-map "l" 'lyskom-list-prefix)
  (define-key lyskom-mode-map "s" 'lyskom-S-prefix)

	;(define-key lyskom-mode-map "vi" 'vilka)

  ; These should be first in order to be last in the menu of alternatives.
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

  (define-key lyskom-mode-map "?"  'describe-mode)
  (define-key lyskom-mode-map "b"  'kom-send-letter)
  (define-key lyskom-mode-map "g"  'kom-go-to-conf)
  (define-key lyskom-mode-map "i"  'kom-write-text)
  (define-key lyskom-mode-map "k"  'kom-write-comment)
  (define-key lyskom-mode-map "K"  'kom-comment-previous)
  (define-key lyskom-mode-map "F"  'kom-write-footnote)
  (define-key lyskom-mode-map "p"  'kom-private-answer)
  (define-key lyskom-mode-map "h"  'kom-jump)
  (define-key lyskom-mode-map "lm" 'kom-list-conferences)
  (define-key lyskom-mode-map "ln" 'kom-list-news)
  (define-key lyskom-mode-map "lN" 'kom-list-news-old)
  (define-key lyskom-mode-map "lp" 'kom-list-persons)
  (define-key lyskom-mode-map "ls" 'kom-membership)
  (define-key lyskom-mode-map "l{" 'kom-list-summary)
  (define-key lyskom-mode-map "l[" 'kom-list-summary)
  (define-key lyskom-mode-map "l\344" 'kom-list-summary) ; 8-bit emacs
  (define-key lyskom-mode-map "l\304" 'kom-list-summary)
  (define-key lyskom-mode-map "l\033{" 'kom-list-summary) ; 7(8)-bit emacs
  (define-key lyskom-mode-map "l\033[" 'kom-list-summary)
  (define-key lyskom-mode-map "m"  'kom-add-self)
  (define-key lyskom-mode-map "M"  'kom-mark-text)
  (define-key lyskom-mode-map "A"  'kom-unmark-text)
  (define-key lyskom-mode-map "ni" 'kom-view-next-new-text)
  (define-key lyskom-mode-map "nm" 'kom-go-to-next-conf)
  (define-key lyskom-mode-map "S"  'kom-quit)
  (define-key lyskom-mode-map "q"  'kom-quit)
  (define-key lyskom-mode-map "r"  'kom-recover)
  (define-key lyskom-mode-map "t"  'kom-display-time)
  (define-key lyskom-mode-map "fu" 'kom-get-appreciation)
  (define-key lyskom-mode-map "fs" 'kom-get-abuse)
  (define-key lyskom-mode-map "v"  'kom-who-is-on)
  (define-key lyskom-mode-map "V"  'kom-busy-wait)
  (define-key lyskom-mode-map "{p" 'kom-change-presentation)
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
  (define-key lyskom-mode-map "}n" 'kom-review-next)
  (define-key lyskom-mode-map "}r" 'kom-find-root)
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



;;;; ================================================================
;;;; Tell phrases should be configured with the default language used
;;;; at the server and not for person reading if they happens to
;;;; differ. This is of coarse because they are sent to the server for
;;;; everybody else to see.

;;;; Aronsson was here 4 DEC 1990, thus creating version 0.18
					; Created *-tell-*


(defconst kom-tell-phrases
  '(
    (kom-tell-silence		"") ; Why ?
    (kom-tell-send		"F�rs�ker l�gga in en text.")
    (kom-tell-login		"Loggar in.")
    (kom-tell-read		"L�ser.")
    (kom-tell-1st-pres		"Skriver den f�rsta presentationen.")
    (kom-tell-write-comment	"Skriver en kommentar.")
    (kom-tell-write-footnote	"Skriver en fotnot.")
    (kom-tell-write-letter	"Skriver ett brev.")
    (kom-tell-write-reply	"Skriver ett privat svar.")
    (kom-tell-write-text	"Skriver inl�gg.")
    (kom-tell-conf-pres		"Skriver presentation f�r ett nytt m�te.")
    (kom-tell-recover		"Trycker 'r'. Suck.")
    (kom-tell-wait		"V�ntar.")
    (kom-tell-regret		"�ngrar sig och sl�nger inl�gget.")
    (kom-tell-review		"�terser.")
    (kom-tell-change-name       "�ndrar sitt namn till n�got annat.")
    (kom-tell-change-supervisor "�ndrar organisat�r f�r n�got.")
    )
  "These texts are used as arguments to lyskom-tell-server.
The variable kom-mercial defaults to kom-tell-wait.
Users are encouraged to change this dictionary for fun.")


;; Placed here because this must NOT be evaluated before 
;; kom-tell-phrases is defined:

(defvar kom-mercial (car (cdr (assoc 'kom-tell-wait kom-tell-phrases)))
  "*When the user has seen all texts and has reached the view-time prompt,
this string is used as the argument to lyskom-tell-server.
Users are encouraged to use their best sense of humor.")


(defconst lyskom-error-texts
  '((0 . "Inget fel har intr�ffat")
    (2 . "�nnu ej implementerat")
    (3 . "Inte l�ngre implementerat")
    (4 . "Felaktigt l�senord")
    (5 . "Str�ngen f�r l�ng")
    (6 . "Du �r inte inloggad")
    (7 . "Ingen f�r logga in i LysKOM just nu")
    (8 . "Du f�rs�kte anv�nda m�te nummer 0")
    (9 . "Odefinierat eller hemligt m�te")
    (10 . "Odefinierad eller hemlig person")
    (11 . "Ingen skriv- eller l�sr�ttighet")
    (12 . "Otill�ten operation")
    (13 . "Du �r inte medlem i m�tet")
    (14 . "Inget s�dant textnummer")
    (15 . "Du kan inte anv�nda globalt text nummer 0")
    (16 . "Inget s�dant lokalt textnummer")
    (17 . "Du kan inte anv�nda lokalt text nummer 0")
    (18 . "Felaktiga tecken i namnet")
    (19 . "Index utanf�r gr�nserna")
    (20 . "M�tet existerar redan")
    (21 . "Personen existerar redan")
    (22 . "Hemligt, men ej l�sskyddat")
    (23 . "Du f�r inte �ndra person/m�tesflaggan")
    (24 . "Fel i databasen. Attans otur.")
    (25 . "Otill�tet misc-f�lt. (Internt fel)")
    (26 . "Otill�ten infotyp. (Bug i klienten)")
    (27 . "Redan mottagare till denna text")
    (28 . "Redan kommentar till denna text")
    (29 . "Redan fotnot till denna text")
    (30 . "Inte mottagare till denna text")
    (31 . "Inte kommentar till denna text")
    (32 . "Inte fotnot till denna text")
    (33 . "F�r m�nga mottagare")
    (34 . "F�r m�nga kommentarer")
    (35 . "F�r m�nga fotnoter")
    (36 . "F�r m�nga markeringar")
    (37 . "Du �r inte f�rfattare till texten")
    (38 . "Du kan inte koppla upp dig till servern")
    (39 . "Minnet slut")
    (40 . "Servern har ballat ur")
    (41 . "Klienten tror att servern s�ger att den inte f�rst�r klienten")
    (42 . "Ingen s�dan session finns"))
  "All the errors reported from the server in plain text.")
