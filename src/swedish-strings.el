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


(defvar lyskom-header-subject "Ärende: "
  "*String to prompt for subject in the edit buffer.")


(defconst lyskom-strings 
  '(
    ; From vars.el: 
    ; From komtypes.el: nil
    ; From clienttypes.el: nil
    ; From startup.el:
    (server-q . "Lyskom-server? (%#1s) ")
    (try-connect . "LysKOM elisp-klient version %#1s.\nFörsöker koppla upp mot %#2s.\n")
    (protocoll-error . "Protocol error. Servers says: %#1s")
    (connection-done . "Uppkopplingen klar. Serverns versionsnummer är %#1s.\n\n")
    (what-is-your-name . "Vad heter du? ")
    (password . "Lösenord? ")
    (wrong-password . "Fel lösen.\n")
    (are-logged-in . "Du är nu inloggad. Vänta ett tag.\n")
    (you-have-motd . "\nDu har en lapp på dörren:\n\n")

    (first-greeting . "%#1s
Det tycks vara första gången du använder LysKOM. Välkommen!
Kontrollera att du stavat ditt namn rätt.  Använd gärna ditt fullständiga
namn och organisation, t ex \"Eskil Block, FOA\".  Om du stavat ditt namn
fel, eller vill ändra ditt namn, svara nej på frågan nedan.

Observera att all information än så länge sparas så att vem som helst
kan läsa den. Till exempel så kan vem som helst se vilket lösenord
du använder.

Om du är osäker på hur man använder LysKOM kan du hämta en manual via
anonym ftp från ftp.lysator.liu.se. Fråga din systemadministratör om
du är osäker.\n")

    (is-name-correct . "Är namnet %#1s korrekt? ")
    (personal-password . "Ange ett personligt lösen: ")
    (repeat-password . "Repetera för kontroll: ")
    (repeat-failure . "Du angav inte samma lösenord bägge gångerna.\n")

    (could-not-create-you .  "Det gick inte att skapa personen.\n")
    (presentation-subject . "%#1s")
    (presentation-form . "Namn:\t\nAdress:\t\n\t\nTelefon:
Email-adress:\n\nAnnat:\t")
    (not-present-anywhere . "Ej närvarande i något möte.")



    ; From internal.el:
    (shaky-tcp . "Nu når jag inte servern. TCP/IP-förbindelsen är skakig%#1s")
    (retrying-tcp . "Jag försöker igen.")

    ; From parse.el:
    ; No entries.

    ; From services.el:
    ; No entries.

    ; From cache.el:
    ; No entries.

    ; From commands1.el:
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
    (confirm-delete-pers-or-conf . "Ta bort %#1s %#2s? ")
    (the-pers . "personen")
    (the-conf . "mötet")
    (deletion-not-confirmed . "Utplåningen avbruten\n")
    (somebody-else-deleted-that-conf . "Någon annan tog precis bort mötet.\n")
    (conf-is-deleted . "Ok, nu är %#1s utplånad.\n")
    (you-could-not-delete . "%#1s kunde inte utplånas av dig.\n")

    (what-text-to-delete . "Vilket inlägg skall tas bort?")
    (deleting-text . "Radering av text %#1d...")

    (presentation-for-whom . "Vilket möte/person? ")
    (somebody-deleted-that-conf . "Någon tog precis bort mötet.\n")
    (review-presentation-of . "Återse presentation av %#1s.\n")
    (has-no-presentation . "%#1s har ingen presentation.\n")

    (have-to-read . "Du måste läsa ett inlägg först.\n")

    (review-text-no . "Återse text nummer %#1d.\n")
    (no-comment-to . "Det finns inget kommenterat inlägg att titta på.\n")

    (who-letter-to . "Vem vill du skicka brev till? ")
    (has-motd . "%#1s har en lapp på dörren:\n\n")
    (motd-persist-q . "Vill du fortfarande skicka brevet? ")

    (who-to-add . "Vem vill du addera? ")
    (where-to-add . "Vilket möte skall han/hon adderas till? ")
    (where-to-add-self . "Vilket möte vill du bli medlem i? ")
    (priority-q . "Prioritet på mötet? (0 (låg) - 255 (hög)) ")
    (done . "klart.\n")
    (nope . "Det gick inte.\n")

    (conf-does-not-exist . "\nMötet finns inte.\n")

    (who-to-exclude . "Vem vill du utesluta? ")
    (where-from-exclude . "Vilket möte skall han/hon uteslutas ifrån? ")

    (leave-what-conf . "Vilket möte vill du gå ur? ")

    (error-fetching-person . "Fel i personhämtningen\n")
    (error-fetching-conf . "Fel i möteshämtningen.\n")

    (name-of-conf . "Vad ska mötet heta? ")
    (anyone-member . "Får vem som helst bli medlem? ")
    (secret-conf . "Hemligt möte? ")
    (comments-allowed . "Får man skriva kommentarer? ")
    (confusion-what-to-comment . "Jag förstår inte vilken text du vill kommentera.\n")
    (confusion-what-to-footnote . "Jag förstår inte vilken text du vill skriva en fotnot till.\n")

    (confusion-who-to-reply-to . "Jag förstår inte vems inlägg du vill skriva ett privat svar till.\n")
    (confusion-what-to-answer-to . "Jag förstår inte vilken text du vill besvara.")
    (really-quit . "Vill du verkligen avsluta sessionen? ")
    (session-ended . "
*****************************
Lyskom-sessionen är avslutad.
*****************************\n")
    (what-to-change-pres-you . "Vilket möte/person vill du ändra presentationen för (dig själv): ")
    (who-to-put-motd-for . "Vilket möte/person vill du sätta lapp på dörr för (dig själv): ")

    (cant-get-conf-stat . "Kan ej hämta mötesstatus för mötet.")
    (go-to-conf . "Gå till möte: ")
    (want-become-member . "Vill du bli medlem? ")
    (no-ok . "Nehej.\n")

    (who-to-remove-motd-for . "Vilket möte/person vill du ta bort lapp på dörr för: ")

    (conf-all-read . "%#1s - inga ol{sta.\n")
    (no-in-conf . "Du är inte närvarande i något möte.\n")

    (search-for-pers . "Ange sökvillkor (RETURN för alla personer) ")
    (search-for-conf . "Ange sökvillkor (RETURN för alla möten) ")
    (name-to-be-changed . "Ange det namn som skall ändras: ")
    (no-such-conf-or-pers . "Mötet eller personen finns ej.\n")
    (new-name . "Nytt namn: ")
    (who-to-change-supervisor-for . "Vem vill du ändra organisatör för? ")
    (new-supervisor . "Ny organisatör: ")
    (text-to-mark . "Vilket inlägg vill du markera? ")
    (text-to-unmark . "Vilket inlägg vill du avmarkera? ")
    (what-mark . "Vilken markering vill du sätta? ")
    (unmarking-textno . "Avmarkering av text %#1d...")
    (marking-textno . "Markering av text %#1d...")

    (new-passwd-again . "Mata in det nya lösenordet igen för kontroll: ")
    (what-mark-to-view . "Vilken markering vill du återse? ")
    (whos-passwd . "Vem vill du {ndra lösenord f|r (dig själv)? ")
    (old-passwd . "Mata in ditt lösenord: ")
    (new-passwd . "Mata in det nya lösenordet: ")
    (changing-passwd . "Ändrar lösenordet...")
    (retype-dont-match . "Lösenorden är inte samma. Gör om.\n")
    (palindrome . "(ett palindrom!) ")
    (lyskom-name . "Användare")
    (is-in-conf . "Närvarande i möte")
    (from-machine . "Kör från")
    (is-doing . "Gör")
    (text-to-add-recipient . "Vilket inlägg vill du addera mottagare till")
    (text-to-delete-recipient . "Vilket inlägg vill du subtrahera mottagare från")
    (text-to-add-comment-to . "Vilket inlägg vill du addera en kommentar till")
    (text-to-delete-comment-from . "Vilket inlägg vill du subtrahera en kommentar från")

    (where-on-list-q . "Placering på listan? (0-%#1d) ")
    (member-in-conf . "Bli medlem i %#1s...")
    (add-member-in . "Addera %#1s som medlem i %#2s...")
    (unsubscribe-to . "Utträda ur %#1s...")

    (exclude-from . "Utesluta %#1s från %#2s...")

    (unsubscribe-failed . "\nDet gick inte. %#1s var kanske aldrig medlem i %#2s?\n")

    (You . "Du")
    (could-not-create-conf . "Mötet \"%#1s\" kunde ej skapas.\nFelkod: %#2d.\n")
    (created-conf-no-name . "Möte nummer %#1d %#2s, skapat.\n")
    (cant-read-textno . "Du får inte läsa text %#1d")

    (not-supervisor-for . "Du är inte organisatör för %#1s.\n")
    (not-supervisor-for . "Du är inte organisatör för %#1s.\n")
    (go-to-conf . "Gå till %#1s.\n")
    (cant-go-to-his-mailbox . "Du får inte gå till %#1ss brevlåda.\n")
    (not-member-of-conf . "Du är inte medlem i %#1s.\n")
    (change-name-done . "\nKlart. Nytt namn: %#1s.\n")
    (change-name-nope . "\nDet gick inte bra. Kanske något annat redan heter %#1s?\n")
    (change-supervisor-from-to . "Ändra organisatör för %#1s till %#2s...")
    (change-supervisor-nope . 
     "\nDet gick inte. Kanske du inte får ändra organisatör för %#1s?\n")
    
    (no-marked-texts . "Du har inga markerade inlägg.\n")
    (no-marked-texts-mark . 
     "Du har inga markerade inlägg med markeringen %#1d.\n")
    (time-is . "Klockan är %4#1d-%02#2d-%02#3d %02#4d:%02#5d:%02#6d %#7s(enligt servern).\n")
    (total-users . "    Sammanlagt %#1d användare.\n")
    (who-to-add-q . "Vilket möte/person vill du addera som mottagare? ")
    (who-to-sub-q . "Vilket möte/person vill du subtrahera som mottagare? ")

    (adding-name-as-recipient . "Adderar %#1s som mottagare till text %#2d...")
    (remove-name-as-recipient .
     "Subtraherar %#1s som mottagare från text %#2d...")
    (text-to-add-q . "Vilket inlägg vill du addera som kommentar? ")
    (text-to-remove-q . "Vilket inlägg vill du subtrahera som kommentar? ")
    (add-comment-to . "Adderar text %#1d som kommentar till text %#2d...")
    (sub-comment-to . "Subtraherar text %#1d som kommentar till text %#2d...")

    ; From commands2.el:

    (your-memberships . "Ditt medlemskap i olika KOM-möten:\n")
    (memberships-header . "Senast inne	   Prio	Olästa	Mötesnamn\n")
    
    (conf-for-status . "Vilket möte vill du se statusen för? ")
    (no-such-conf . "Mötet finns ej.")
    (status-record . "Status för möte %#1s (%#2s) %#3s\n\n")
    (Mailbox . "Brevlåda")
    (Protected . "Skyddat")
    (no-comments . "original")
    (closed . "slutet")

    (created-by . "Skapat av person %#1s %#2s\n")
    (created-at . "Skapad:%35#1s\n")
    (members .    "Antal medlemmar: %25#1d\n")
    (garb-nice . "Livslängd på inlägg (dagar):%14#1d\n")
    (lowest-local-no . "Lägsta existerande lokala nummer: %8#1d\n")
    (highest-local-no . "Högsta existerande lokala nummer: %8#1d\n")
    (last-text-time . 
     "Tid för senaste inlägg: %18#1s (står det i din cache)\n")
    (no-of-motd . "Lapp på dörren i text nummer: %12#1d\n")
    (superconf-is-no-name . "Supermöte:       %25#1d %#2s\n")
    (permitted-submitters-no-name . "Tillåtna författare:%22#1d %#2s\n")
    (supervisor-is-no-name . "Organisatör:     %25#1d %#2s\n")
    (presentation-no . "Presentation:    %25#1d\n")
    (conf-has-motd . "\n%#1s har en lapp på dörren:\n")

    (Everybody . "Alla")
    (show-members-list-also-q . "Vill du se medlemslistan också? ")
    (conf-has-these-members . "\n%#1s har följande medlemmar:\n")
    (member-list-header . "Senast inne         Osett  Namn\n\n")
    (secret-membership . "*** Hemlig rad ***\n")
    (pers-for-status . "Vem vill du se statusen för? ")
    (no-such-pers . "Det finns ingen sådan person.")
    (pers-status-record . "Status för person %#1s (%#2d)\n")
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
    (time-for-last-letter . "Tid för senaste brev:%20#1s (står det i din cache)\n")
    (superconf . "Supermöte:%31#1d %#2s\n")
    (supervisor . "Organisatör:%29#1d %#2s\n")
    (member-of-confs . "Medlem i (antal möten):%18#1d\n")
    (presentation . "Presentation:    %24#1d\n")
    (show-membership-list-also-q . "Vill du se vilka möten personen är medlem i också? ")
    (not-allowed-see-confs . "Du får inte se vilka möten %#1s är medlem i.\n")
    (is-member-of . "\n%#1s är medlem i följande möten:\n")
    (membership-list-header . "Senast inne         Osett  Namn\n\n")
    (who-to-send-message-to . "Vem vill du skicka meddelandet till? (Alla) ")
    (his-total-unread . "\n%#1s har totalt %#2d olästa.\n")
    (message-prompt . "Meddelande: ")
    (message-sent . "Meddelandet sänt.\n")
    (message-nope .
  "Du kunde inte skicka meddelandet. Mottagaren var kanske inte inloggad.\n")
    (only-last . "Endast läsa senaste (0 - %#1d) i %#2s? ")
    (only-error . "Nåt gick galet. Sorry.\n")

    (you-have-unreads . "Du har %#1d olästa inlägg i %#2s\n")
    (you-have-an-unread . "Du har 1 oläst inlägg i %#1s\n")
    (you-have-read-everything . "Du har sett alla nyheter\n")
    (total-unreads . "\nDu har %#1d olästa inlägg.\n")
    (total-unread . "\nDu har 1 oläst inlägg.\n")
    (waiting-for-anything .
     "Du väntar på ett inlägg i vilket möte som helst.\n")
    (waiting-higher-than . 
     "Du väntar på ett inlägg i ett möte med högre prioritet än %#1d.\n")

    (have-to-be-in-conf . "Du måste gå till ett möte först.\n")
    (Texts . "Inlägg")
    (Date . "Datum")
    (Lines . "Rader")
    (Author . "  Författare")
    (Subject . "  Ärende")
    (could-not-read . "Du fick inte läsa denna text (%#1d).\n")
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


    (not-logged-in . "Du är inte inloggad.  ")
    (name-is-not-in-conf . "%#1s är inte närvarande i något möte.\n")
    (name-is-in-conf . "%#1s är närvarande i\n%#2s\n")
    (connected-during . "Uppkopplingstid: %#1d sekunder.\n")
    (conf-to-set-garb-nice-q . "Vilket möte vill du sätta livslängd för? ")
    (new-garb-nice-q . "Vilket värde vill du sätta livslängden till? ")
    (garb-nice-for-is . "Ändra livslängden för %#1s till %#2d...")

    (really-shutdown . "Är du säker på att du vill stänga av servern? ")
    (closing-server . "Stänga av servern...")
    (administrator . "administratör")
    (no-longer-administrator . "en normal användare igen")
    (you-are-now . "Ok, du kör nu som %#1s.\n")
    (setting-motd . "Sätter loginmeddelandet till text %#1d.\n")
    (set-motd-success . "Du har satt ett nytt loginmeddelande.\n")
    (set-motd-failed . "Det gick inte. Du var kanske inte administrat|r.\n")
    (removing-motd . "Tar bort loginmeddelandet.\n")
    (removed-motd . "Du har tagit bort loginmeddelandet.\n")
    (who-to-throw-out . "Vilken session vill du kasta ut? ")
    (throwing-out . "Kastar nu ut session %#1d... ")
    (postpone-prompt . "Hur lite vill du läsa nu? ")

    ; From review.el:
    (latest-n . "senaste %#1d")
    (first-n . "första %#1d")
    (info-by-whom . "%#1s av vem: ")
    (info-to-conf . "%#1s till möte: ")
    (info-by-to . "%#1s av %#2s till %#3s framåt.")
    (all-confs . "alla möten")
    (no-get-conf . "Du får inte hämta mötet.\n")
    (no-get-pers . "Du får inte hämta personen.\n")
    (no-review-info . "Ej tillåtet återse %#1s\n")
    (review-info . "Återse %#1s\n")
    (you-review . "Du återser nu %#1s.\n")
    (read-text-first . "Du måste läsa en text först.\n")
    (cannot-read-last-text . "Du kan inte läsa den senast lästa texten.\n")
    (review-n-texts . "Återse %#1d inlägg.\n")
    (review-marked . "Återse %#1d markerade.\n")
    (review-text-no . "Återse text nummer %#1d")
    (review-one-comment . "Återse en kommentar till inlägg %#1d.\n")
    (review-many-comments . "Äterse %#2d kommentarer till inlägg %#1d.\n")

    ; From edit-text.el:
    (press-C-c-C-c . "Tryck C-c C-c för att skicka in texten.")
    (recipient . "Mottagare:")
    (carbon-copy . "Extra kopia:")
    (text-mass . "%#4s%#1s\n%#2s\n%#3s")
    (comment-to-by . "%#1s till text %#2d%#3s.\n")
    (already-sent . "Du har redan skickat denna text en gång. Sänd ändå? ")
    (subject . "Ärende: ")
    (which-text-include . "Vilken text skall vi inkludera? ")
    (added-recipient . "Mottagare som skall adderas: ")
    (added-carbon-copy . "Extra kopia till möte: ")
    (conf-has-motd-no . "Mötet har en lapp på dörren. (%#1d)\n\n%#2s")
    (still-want-to-add . "Vill du fortfarande addera mötet? ")
    (could-not-create-text . "\nTexten kunde ej skapas. Felkod: %#1d.\n")
    (no-get-text . "Du fick inte hämta texten.")


    ; From view-text.el:
    (line . " /1 rad/ ")
    (lines ." /%#1d rader/ ")

    (marked-by-you . "Markerad av dig.\n")
    (marked-by-you-and-one . "Markerad av dig och någon annan.\n")
    (marked-by-you-and-several . "Markerad av dig och %#1d andra.\n")
    (marked-by-one . "Markerad av 1 person.\n")
    (marked-by-several . "Markerad av %#1d personer.\n")

;; The format of this function should coincide with the format of the 
;; lyskom-text-start variable. DONT change one without changing the other.

; used by lyskom-print-time
    (time-y-m-d-h-m . "%4#1d-%02#2d-%02#3d  %02#4d:%02#5d ")
; used by lyskom-return-time
    (time-yyyy-mm-dd-hh-mm . "%4#1d-%02#2d-%02#3d %02#4d:%02#5d")
    (no-such-text-no . "Det finns ingen sådan text. (%#1d)\n")

    (head-Subject . "Ärende: ")
    (Recipient . "Mottagare")
    (Extra-recipient . "Extra kopia")
    (send-at . "    Sänt:     %#1s\n")
    (sent-by . "    Sänt av ")
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
Meddelande från LysKOM-systemet: Någon försökte koppla upp,
men misslyckades eftersom alla tillgängliga förbindelser är
upptagna. Logga ut och kom tillbaks senare om du väntar nu.
===========================================================\n")
    (has-entered . "Nu har %#1s gått in i LysKOM.")
    (has-entered-r . "Nu har %#1s gått in i LysKOM.\n")
    (has-left . "Nu har %#1s gått ur LysKOM.")
    (has-left-r . "Nu har %#1s gått ur LysKOM.\n")
    (unknown . "okänd")

    (message-broadcast . "Allmänt meddelande från %#1s (%#3s):\n\n%#2s\n")
    (message-from . "Personligt meddelande från %#1s (%#3s):\n\n%#2s\n")
    (text-is-created . "Text %#1d är skapad!")


    ; From completing-read.el:

    (person-or-conf-no-regexp . "\\`[ \t]*[mpMP]\\w*[ \t]+\\([0-9]+\\)\\'")

    ; From prioritize.el:
    (cannot-get-membership . "Kan ej hämta medlemsskap för dig.")
    (cannot-get-pers-stat . "Kan ej hämta personstatus för dig.")
    (your-membship . "Ditt medlemsskap i olika KOM-möten:
  Prio Mötesnr Mötesnamn\n")
    (prio-row . " %5#1d%5#2d  %#3s\n")
    (too-high-goto-2 . "Du står för högt upp. Gå ner till rad 2.")
    (too-low-go-up . "Du kan inte pusha sista raden. Gå upp en rad.")
    (all-confs-popped .  "Alla möten är poppade.")
    (prio-died . "Flyttningen misslyckades. Sorry. Döda bufferten.")
    (new-priority . "Ny prioritet? (0 (låg) - 255 (hög)) ")
    (new-prio . "%6#1d")

    ; From flags.el:
    (saving-settings . "Sparar flaggorna")
    (hang-on . "Vänta ett tag...\n")
    (could-not-create-area . "Kunde ej skapa texten.\n")
    (could-not-set-user-area . "Kunde ej ställa om user-arean. Servern säger felmeddelande: %#1d\n")
    (you-dont-exist . "Du finns inte.\n")

    ; From elib-string.el:
    ; No entries.

    ; From lyskom-rest.el:

    (error-code . "Felkod %#2d: %#1s.\n")
    (extended-command . "LysKOM: ")
    (wait-for-server . "LysKOM väntar på svar från servern. Vänta tills du får en prompt.\n")
    (review-text-q . "Återse text nummer: ")
    (review-text-no . "Återse text nummer %#1d.\n")

    (completely-read-conf . "Du har sett alla texter i detta möte.\n")
    (not-in-any-conf . "Du läser inte något möte just nu.\n")

    (all-conf-unread-r . "Du har läst ut alla möten.\n")
    (all-conf-unread-s . "Du har läst ut alla möten. ")
    (one-unread . " - 1 oläst\n")
    (several-unread . " - %#1d olästa\n")

    (save-on-file-q . "Spara inlägg på fil: (%#1s) ")
    (wait-for-prompt . "Vänta på prompten!")
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
    (conf-does-not-exist . "Möte %#1d (finns inte).")

    (process-signal . "Signal från processen.")
    (closed-connection . "
**************************************************
Lyskom-sessionen onormalt stängd.
Felmeddelande: %#1s**************************************************")
    (error-not-found . "Fel nummer %#1d. Ingen klartextförklaring finns.")

    ; Useful in more place than one:
    (illegal-command . "Otillåtet kommando.\n")
    (no-such-text . "Det finns inget sådant inlägg.\n")
    (no-such-text-m . "Det finns inget sådant inlägg.")
    (everybody . "alla")
    (anybody . "vem som helst")
    (forward . "framåt")
    (backward . "bakåt")
    (wait . "Vänta ett tag...\n")
    (comment . "Kommentar")
    (footnote . "Fotnot")
    (by . " av %#1s")
    (text-created .  "Text nummer %#1d är skapad.\n")
    )
  "Assoc list containing pairs of atoms and strings")



;;; ================================================================
;;;              The commands and their associated functions


(defconst lyskom-commands
  '(
    (describe-mode              "Hjälp")
    (kom-send-message           "Sända meddelande")
    (kom-create-conf		"Skapa möte")
    (kom-delete-conf            "Utplåna")
    (kom-delete-text            "Radera inlägg")
    (kom-display-time		"Se tiden")
    (kom-go-to-conf		"Gå till möte")
    (kom-go-to-next-conf	"Gå till nästa möte")
    (kom-jump			"Hoppa över alla kommentarer")
    (kom-list-conferences 	"Lista möten") 
    (kom-list-persons		"Lista personer")
    (kom-list-news              "Lista nyheter")
    (kom-membership		"Lista medlemsskap")
    (kom-postpone		"Uppskjuta läsning")
    (kom-prioritize		"Prioritera möten")
    (kom-status-person		"Status (för) person")
    (kom-status-conf		"Status (för) möte")
    (kom-add-self		"Bli medlem i möte")
    (kom-list-summary		"Lista ärenden")
    (kom-sub-self               "Utträda ur möte")
    (kom-quit			"Sluta")
    (kom-recover		"Återstarta kom") 
    (kom-start-anew		"Börja med nytt namn")
    (kom-view			"Återse inlägg")
    (kom-find-root-review	"Återse träd")
    (kom-review-comments	"Återse alla kommentarer")
    (kom-review-tree		"Återse alla kommentarer rekursivt")
    (kom-review-clear           "Återse hoppa")
    (kom-review-next            "Återse nästa")
    (kom-find-root		"Återse urinlägget")
    (kom-review-by-to           "Återse senaste")
    (kom-view-commented-text    "Återse det kommenterade")
    (kom-review-stack           "Återse lista")
    (kom-review-presentation    "Återse presentation")
    (kom-review-backward        "(Återse) Baklänges")
    (kom-view-next-text		"Läsa nästa inlägg")
    (kom-who-is-on		"Vilka är inloggade") 
    (kom-display-who-buffer	"Visa vilkalistan")
    (kom-busy-wait		"Vänta på ett inlägg")
    (kom-write-comment		"Kommentera inlägget")
    (kom-comment-previous	"Kommentera föregående inlägg")
    (kom-write-footnote         "Fotnot till inlägg")
    (kom-private-answer         "Personligt svar")
    (kom-set-unread		"Endast läsa senaste")
    (kom-write-text		"Skriva ett inlägg")
    (kom-send-letter		"Skicka brev")
    (kom-change-name		"Ändra namn")
    (kom-change-password        "Ändra lösenord")
    (kom-change-supervisor	"Ändra organisatör")
    (kom-change-presentation	"Ändra presentation")
    (kom-get-appreciation       "Få uppmuntran")
    (kom-get-abuse              "Få skäll")
    (kom-mark-text              "Markera (inlägg)")
    (kom-unmark-text            "Avmarkera (inlägg)")
    (kom-review-marked-texts    "Återse markerade")
    (kom-review-all-marked-texts "Återse alla markerade")
    (kom-add-recipient          "Addera mottagare")
    (kom-sub-recipient          "Subtrahera mottagare")
    (kom-add-comment		"Addera kommentar")
    (kom-sub-comment		"Subtrahera kommentar")
    (kom-add-member		"Addera medlem")
    (kom-sub-member             "Uteslut medlem")
    (kom-change-conf-motd	"Sätt lapp på dörren")
    (kom-set-garb-nice          "Ändra livslängd")
    (kom-unset-conf-motd	"Ta bort lapp på dörren")
    (kom-save-text		"Spara text (på fil)")
    (kom-edit-options		"Ändra variabler")
    (kom-shutdown-server        "Stäng av servern")
    (kom-enable-adm-caps        "Övergå till administratörsmod")
    (kom-disable-adm-caps       "Övergå till normalmod")
    (kom-set-motd               "Sätt loginmeddelande")
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
    (kom-tell-send		"Försöker lägga in en text.")
    (kom-tell-login		"Loggar in.")
    (kom-tell-read		"Läser.")
    (kom-tell-1st-pres		"Skriver den första presentationen.")
    (kom-tell-write-comment	"Skriver en kommentar.")
    (kom-tell-write-footnote	"Skriver en fotnot.")
    (kom-tell-write-letter	"Skriver ett brev.")
    (kom-tell-write-reply	"Skriver ett privat svar.")
    (kom-tell-write-text	"Skriver inlägg.")
    (kom-tell-conf-pres		"Skriver presentation för ett nytt möte.")
    (kom-tell-recover		"Trycker 'r'. Suck.")
    (kom-tell-wait		"Väntar.")
    (kom-tell-regret		"Ångrar sig och slänger inlägget.")
    (kom-tell-review		"Återser.")
    (kom-tell-change-name       "Ändrar sitt namn till något annat.")
    (kom-tell-change-supervisor "Ändrar organisatör för något.")
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
  '((0 . "Inget fel har inträffat")
    (2 . "Ännu ej implementerat")
    (3 . "Inte längre implementerat")
    (4 . "Felaktigt lösenord")
    (5 . "Strängen för lång")
    (6 . "Du är inte inloggad")
    (7 . "Ingen får logga in i LysKOM just nu")
    (8 . "Du försökte använda möte nummer 0")
    (9 . "Odefinierat eller hemligt möte")
    (10 . "Odefinierad eller hemlig person")
    (11 . "Ingen skriv- eller läsrättighet")
    (12 . "Otillåten operation")
    (13 . "Du är inte medlem i mötet")
    (14 . "Inget sådant textnummer")
    (15 . "Du kan inte använda globalt text nummer 0")
    (16 . "Inget sådant lokalt textnummer")
    (17 . "Du kan inte använda lokalt text nummer 0")
    (18 . "Felaktiga tecken i namnet")
    (19 . "Index utanför gränserna")
    (20 . "Mötet existerar redan")
    (21 . "Personen existerar redan")
    (22 . "Hemligt, men ej lässkyddat")
    (23 . "Du får inte ändra person/mötesflaggan")
    (24 . "Fel i databasen. Attans otur.")
    (25 . "Otillåtet misc-fält. (Internt fel)")
    (26 . "Otillåten infotyp. (Bug i klienten)")
    (27 . "Redan mottagare till denna text")
    (28 . "Redan kommentar till denna text")
    (29 . "Redan fotnot till denna text")
    (30 . "Inte mottagare till denna text")
    (31 . "Inte kommentar till denna text")
    (32 . "Inte fotnot till denna text")
    (33 . "För många mottagare")
    (34 . "För många kommentarer")
    (35 . "För många fotnoter")
    (36 . "För många markeringar")
    (37 . "Du är inte författare till texten")
    (38 . "Du kan inte koppla upp dig till servern")
    (39 . "Minnet slut")
    (40 . "Servern har ballat ur")
    (41 . "Klienten tror att servern säger att den inte förstår klienten")
    (42 . "Ingen sådan session finns"))
  "All the errors reported from the server in plain text.")
