;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: swedish-strings.el,v 44.322 2003-05-17 15:09:20 byers Exp $
;;;;; Copyright (C) 1991-2002  Lysator Academic Computer Association.
;;;;;
;;;;; This file is part of the LysKOM Emacs LISP client.
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
	      "$Id: swedish-strings.el,v 44.322 2003-05-17 15:09:20 byers Exp $\n"))


;;; ================================================================
;;; The language definition

(lyskom-define-language 'sv
                        'iso-8859-1
                        "Svenska"
                        )


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
  (define-key lyskom-sv-edit-mode-map (kbd "*")     'kom-button-press-or-self-insert-command)
  (define-key lyskom-sv-edit-mode-map (kbd "=")     'kom-menu-button-press-or-self-insert-command)
  (define-key lyskom-sv-edit-mode-map (kbd "TAB")   'kom-edit-next-button-or-self-insert)
  (define-key lyskom-sv-edit-mode-map (kbd "M-TAB") 'kom-edit-prev-button)
  (define-key lyskom-sv-edit-mode-map (kbd "<S-tab>") 'kom-edit-prev-button)
  (define-key lyskom-sv-edit-mode-map (kbd "C-c")   'lyskom-sv-edit-prefix)
  (define-key lyskom-sv-edit-mode-map (kbd (lyskom-keys (lyskom-xemacs-or-gnu 'button2 'button2up))) 'kom-button-click-or-yank)
  (define-key lyskom-sv-edit-mode-map (kbd (lyskom-keys (lyskom-xemacs-or-gnu 'button2up 'button2))) 'kom-mouse-null)
  (define-key lyskom-sv-edit-mode-map (kbd (lyskom-keys 'button3)) 'kom-popup-menu)
  (define-key lyskom-sv-edit-mode-map (kbd (lyskom-keys 'button3up))     'kom-mouse-null)
  (define-key lyskom-sv-edit-prefix (kbd "C-x")     'lyskom-sv-edit-aux-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "?")       'lyskom-help)
  (define-key lyskom-sv-edit-prefix (kbd "}")       'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "]")       'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-]")     'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-}")     'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-å")     'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-Å")     'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix [(control aring)]     'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix [(control Aring)]     'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix [(control 229)]     'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix [(control 197)]     'lyskom-sv-edit-review-prefix)
  (define-key lyskom-sv-edit-prefix "\205"     'lyskom-sv-edit-review-prefix) ;For windows


  (define-key lyskom-sv-edit-prefix (kbd "*")       'kom-button-press)
  (define-key lyskom-sv-edit-prefix (kbd "=")       'kom-menu-button-press)
  (define-key lyskom-sv-edit-prefix (kbd "C-i")     'lyskom-sv-edit-insert-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "TAB")     'lyskom-sv-edit-insert-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-c")     'kom-edit-send)
  (define-key lyskom-sv-edit-prefix (kbd "C-s")     'kom-ispell-message)
  (define-key lyskom-sv-edit-prefix (kbd "C-k")     'kom-edit-quit)
  (define-key lyskom-sv-edit-prefix (kbd "} ?")     'lyskom-help)
  (define-key lyskom-sv-edit-prefix (kbd "} C-k")   'kom-edit-show-commented)
  (define-key lyskom-sv-edit-prefix (kbd "} k")     'kom-edit-show-commented)
  (define-key lyskom-sv-edit-prefix (kbd "C-i ?")   'lyskom-help)
  (define-key lyskom-sv-edit-prefix (kbd "C-i C-k") 'kom-edit-insert-commented)
  (define-key lyskom-sv-edit-prefix (kbd "C-y")     'kom-edit-insert-commented)
  (define-key lyskom-sv-edit-prefix (kbd "C-i C-y")     'kom-edit-insert-commented)
  (define-key lyskom-sv-edit-prefix (kbd "C-b")     'kom-edit-insert-buglist)
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
  (define-key lyskom-sv-edit-prefix (kbd "C-i C-l") 'kom-edit-insert-link)
  (define-key lyskom-sv-edit-prefix (kbd "C-i C-i") 'kom-insert-last-info-nodename)
  (define-key lyskom-sv-edit-prefix (kbd "C-a")     'lyskom-sv-edit-add-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-a C-m") 'kom-edit-add-recipient)
  (define-key lyskom-sv-edit-prefix (kbd "C-a <RET>") 'kom-edit-add-recipient)
  (define-key lyskom-sv-edit-prefix (kbd "C-a C-e") 'kom-edit-add-copy)
  (define-key lyskom-sv-edit-prefix (kbd "C-a C-b") 'kom-edit-add-bcc)
  (define-key lyskom-sv-edit-prefix (kbd "C-a C-f") 'kom-edit-move-text)
  (define-key lyskom-sv-edit-prefix (kbd "C-a C-j") 'kom-edit-add-recipient)
  (define-key lyskom-sv-edit-prefix (kbd "C-a <LFD>") 'kom-edit-add-recipient)
  (define-key lyskom-sv-edit-prefix (kbd "C-a C-k") 'kom-edit-add-comment)
  (define-key lyskom-sv-edit-prefix (kbd "C-a C-x") 'kom-edit-add-cross-reference)
  (define-key lyskom-sv-edit-prefix (kbd "C-x C-p") 'kom-edit-add-personal-comments)
  (define-key lyskom-sv-edit-prefix (kbd "C-x C-n") 'kom-edit-add-no-comments)
  (define-key lyskom-sv-edit-prefix (kbd "C-x C-l") 'kom-edit-add-world-readable)
  (define-key lyskom-sv-edit-prefix (kbd "C-x C-b") 'kom-edit-add-read-confirm-request)
  (define-key lyskom-sv-edit-prefix (kbd "C-a ?")   'lyskom-help))



(defconst lyskom-strings-missing '())



;;; The alist formely known as lyskom-strings
(lyskom-language-strings local lyskom-message sv
  '(
    ;; From vars.el: 
    ;; From komtypes.el: nil
    ;; From clienttypes.el: nil
    ;; From startup.el:
    (server-q . "LysKOM-server? (%#1s) ")
    (try-connect . "LysKOM elisp-klient version %#1s.\nFörsöker koppla upp mot %#2s.\n")
    (too-old-server . "Servern är för gammal för den här versionen av klienten.")
    (connection-done . "Uppkopplingen klar. Serverns versionsnummer är %#1s.\n\n")
    (what-is-your-name . "Vad heter du? ")
    (password . "Lösenord? ")
    (wrong-password . "Fel lösen.\n")
    (wrong-password-help . "
Du kan skicka e-mail till någon av nedanstående adresser för att få
ett nytt lösenord eller för att få din gamla person raderad så att du
kan skapa en ny:\n")
    (wrong-password-email . "* %#1t\n")
    (are-logged-in . "Du är nu inloggad. Vänta ett tag.\n")
    (you-have-motd . "\nDu har en lapp på dörren:\n\n")
    (server-has-motd . "Servern har ett loginmeddelande:\n")
    (lyskom-motd-was-garbed . "\nLoginmeddelandet finns inte!
Det meddelande som ska visas efter inloggning har försvunnit.
Kontakta LysKOM-administratören.\n")
    (presentation-encouragement . 
				"Du har ingen presentation. Det skulle vara trevligt om du skrev en.
Använd kommandot Äp. Om du inte vill skriva någon presentation tryck fs.\n")

    (first-greeting . "%#1s
Det tycks vara första gången du använder LysKOM. Välkommen!
Kontrollera att du stavat ditt namn rätt. Använd gärna ditt fullständiga
namn och organisation, t ex \"Eskil Block, FOA\". Om du stavat ditt namn
fel, eller vill ändra ditt namn, svara nej på frågan nedan.

Observera att all information än så länge sparas så att vem som helst
kan läsa den. Dock är lösenordet krypterat.

Om du är osäker på hur man använder LysKOM kan du hämta en manual via
anonym ftp från ftp.lysator.liu.se. Fråga din systemadministratör om
du är osäker. Du kan även skriva \"?\" för kommandot \"Hjälp\".
")

    (is-name-correct . "Är namnet %#1s korrekt? ")
    (personal-password . "Ange ett personligt lösen: ")
    (repeat-password . "Repetera för kontroll: ")
    (repeat-failure . "Du angav inte samma lösenord bägge gångerna.\n")

    (could-not-create-you .  "Det gick inte att skapa personen.\n")
    (presentation-subject . "%#1s")
    (presentation-form . "\
Namn:    
Adress:  
Stad:    
Telefon: 
E-post:  
WWW:     

Annat:   ")
    (presentation-help . "Du skriver just  nu din presentation.\n")
    (not-present-anywhere . "Ej närvarande i något möte.")
    (in-secret-conference . "Hemligt möte (%#1d).")
    (start-new-session-same-server
     . "Du kör redan mot den servern. Vill du starta en ny session? ")
    (new-session-in-buffer . "\n\n---- Ny session startad %s ----\n\n")
    (warning-about-uncompiled-client . "
OBS: LysKOM-klienten är inte kompilerad. För att öka prestanda bör du köra en
kompilerad version av klienten. Läs i den medföljande README-filen hur man
gör det.
")

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
    (delete-marked-by-you-and-others . "markerad av dig och %#1?d%[någon annan%]%[%#1d andra%]")
    (delete-marked-by-several . "markerat av %#1d person%#1?d%[%]%[er%]")
    (delete-commented-text-help . "\
Inlägget du håller på att radera har kommentarer. Om du tar bort inlägget
så kommer dessa kommentarer att förlora sitt sammanhang. Detta irriterar
många läsare. Överväg att flytta inlägget till ett annat möte istället
för att radera det.")
    (delete-commented-text . "Inlägget har kommentarer. Radera ändå? ")
    (really-delete-commented-text . "Att radera kommenterade inlägg irriterar många läsare. Radera ändå?")
    (deleting-text . "Radering av text %#1:n...")

    (presentation-for-whom . "Se presentation för vilket möte/person? ")
    (unread-presentation-for-whom . "Oläsmarkera presentation för vilket möte/person? ")
    (text-to-see-author-of . "Se presentation för vilket inläggs författare? ")
    (text-to-unread-author-of . "Oläsmarkera presentation för vilket inläggs författare? ")
    (somebody-deleted-that-conf . "Någon tog precis bort mötet.\n")
    (review-presentation-of . "Återse presentation av %#1M.\n")
    (has-no-presentation . "%#1:M har ingen presentation.\n")

    (have-to-read . "Du måste läsa ett inlägg först.\n")

    (no-comment-to . "Det finns inget kommenterat inlägg att titta på.\n")
    (no-text-at-point . "Det finns inget inlägg vid markören.\n")
    (what-ancestor . "Vilken av de kommenterade texterna önskas? ")

    (who-letter-to . "Vem vill du skicka brev till? ")
    (who-send-text-to . "Vart vill du skicka inlägget? ")
    (has-motd . "%#1P har en lapp på dörren:\n\n")
    (motd-persist-q . "Vill du fortfarande skriva texten? ")

    (who-to-add . "Vem vill du addera? ")
    (where-to-add . "Vilket möte skall han/hon adderas till? ")
    (where-to-add-self . "Vilket möte vill du bli medlem i? ")
    (priority-q . "Prioritet på ditt medlemskap i mötet? (0 (låg) - 255 (hög)) ")
    (other-priority-q . "Prioritet på den adderades medlemskap i mötet? (0 (låg) - 255 (hög)) ")
    (done . "klart.\n")
    (cancelled . "avbrutet.\n")
    (nope . "det gick inte.\n")
    (add-already-member . "%#1P är redan medlem i %#2M.\n")
    (you-already-member . "Du är redan medlem i %#1M.\n")

    (cant-find-supervisor . "Hittar inte organisatören för %#1M.\n")
    (is-read-protected-contact-supervisor . "%#1M är slutet.
Skicka ett brev till %#2P för medlemskap.\n")

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
    (what-private-no . "Personligt svar till text nummer: ")
    (quit-in-spite-of-unsent . "Vill du avsluta sessionen trots oinskickat inlägg? ")
    (really-quit . "Vill du verkligen avsluta sessionen? ")
    (session-ended . "
*****************************
LysKOM-sessionen är avslutad.
*****************************\n")
    (session-auto-ended . "
===========================================================
Kopplar ned från LysKOM eftersom LysKOM är fullt och
du har läst klart allting. Kom tillbaks senare.
===========================================================\n\n")
    (session-ended-long . "
*******************************************************************************
LysKOM-sessionen är avslutad.
*******************************************************************************
")
    (session-auto-ended-long . "
===============================================================================
Kopplar ned från LysKOM eftersom LysKOM är fullt och
du har läst klart allting. Kom tillbaks senare.
===============================================================================
\n")
    (what-to-change-pres-you . "Vilket möte/person vill du ändra presentationen för (dig själv): ")
    (what-to-change-faq-you . "Vilket möte vill du ändra FAQ för: ")
    (who-to-put-motd-for . "Vilket möte/person vill du sätta lapp på dörr för (dig själv): ")

    (what-to-set-pres-you . "Vilket möte/person vill du sätta presentationen för (dig själv): ")
    (what-text-to-set-as-pres-no . "Vilket inlägg vill du ska vara den nya presentationen: ")
    (what-to-set-motd-you . "Vilket möte/person vill du sätta lapp på dörr för (dig själv): ")
    (what-text-to-set-as-motd-no . "Vilket inlägg vill du ska vara den nya lappen: ")
    (conf-already-has-pres . "Mötet/personen har redan en presentation. Fortsätt ändå? ")
    (conf-already-has-motd . "Mötet/personen har redan en lapp på dörren. Fortsätt ändå? ")
    (setting-conf-pres . "Sätter presentation för %#1M till text %#2n...")
    (setting-conf-motd . "Sätter lapp på dörren för %#1M till text %#2n...")

    (who-to-remove-pres-for . "Vilket möte/person vill du ta bort presentationen från (dig själv): ")
    (removing-pres-for-conf . "Tar bort presentation (text %#2n) från %#1M...")

    (cant-get-conf-stat . "Kan ej hämta mötesstatus för mötet.\n")
    (go-to-conf-p . "Gå till möte: ")
    (want-become-member . "Vill du bli medlem? ")
    (no-ok . "Nehej.\n")

    (who-to-remove-motd-for . "Vilket möte/person vill du ta bort lapp på dörr för (dig själv): ")

    (conf-all-read . "inga olästa")
    (no-in-conf . "Du är inte närvarande i något möte.\n")

    (search-for-pers . "Ange sökvillkor (RETURN för alla personer): ")
    (search-for-conf . "Ange sökvillkor (RETURN för alla möten): ")
    (search-re . "Ange sökuttryck (reguljärt uttryck): ")
    (search-re-for-what . "Vad vill du söka? ")
    (search-re-persons . "pSök personer")
    (search-re-confs . "mSök möten")
    (search-re-all . "bSök alla")

    (no-matching-confs . "Inga möten uppfyller sökvillkoret \"%#1s\".\n")
    (no-matching-perss . "Inga personer uppfyller sökvillkoret \"%#1s\".\n")
    (no-matching-anys . "Inga personer eller möten uppfyller uttrycket \"%#1s\".\n")
    (no-confs-exist . "Det finns inga möten i databasen.\n")
    (no-pers-confs-exist . "Det finns inga möten eller personer i databasen.\n")
    (list-confs-created-by . "Lista ägda möten för: ")
    (listing-confs-created-by . "Ägda möten för %#1P\n\
    S=Skapat, O=Organisatör, M=Brevlåda som supermöte; S=Slutet, H=Hemligt\n")
    (list-pers-confs-created-by . "Lista ägda möten (med brevlådor) för: ")
    (getting-all-confs . "Hämtar en lista av alla möten från servern...")
    (getting-all-pers-confs . "Hämtar en lista av alla personer och möten från servern...")
    (getting-all-confs-done . "Hämtar en lista av alla möten från servern...klart")
    (getting-all-pers-confs-done . "Hämtar en lista av alla personer och möten från servern...klart")
    (finding-created-confs . "Söker ägda möten (%#1d av %#2d klart)")
    (finding-created-pers-confs . "Söker ägda möten (med brevlådor) (%#1d av %#2d klart)")
    (no-created-confs . "%#1P är inte skapare, organisatör eller supermöte för något möte.\n")

    (name-to-be-changed . "Ange det namn som skall ändras: ")
    (no-such-conf-or-pers . "Mötet eller personen finns ej.\n")
    (new-name . "Nytt namn: ")
    (new-paren . "Ny parentes: ")
    (too-many-parens-in-name . "Namnet innehåller mer än en parentes. Använd \"Ändra namn\" istället.\n")
    (no-paren-in-name . "Namnet innehåller ingen parentes.\n")
    (who-to-change-supervisor-for . "Vem vill du ändra organisatör för? ")
    (new-supervisor . "Ny organisatör: ")
    (text-to-mark . "Vilket inlägg vill du markera? ")
    (text-to-unmark . "Vilket inlägg vill du avmarkera? ")
    (what-mark . "Vilken typ av markering vill du sätta (namn eller 0-255)? ")
    (erroneous-mark . "Felaktig markeringstyp.\n")
    (want-to-create-symbolic-mark . "Markeringstypen \"%#1s\" finns inte. Vill du skapa den? ")
    (creating-symbolic-mark-type . "Skapar markeringstypen \"%#1s\" (%#2d).\n")
    (no-mark-types-left . "Ledsen, det finns inga lediga markeringstyper. Rensa bland de gamla först.")
    (unmarking-textno . "Avmarkering av text %#1n...")
    (marking-textno . "Markering av text %#1n...")
    (list-which-mark . "Lista vilken markeringstyp (namn eller 0-255, RET för alla)? ")

    (new-passwd-again . "Mata in det nya lösenordet igen för kontroll: ")
    (what-mark-to-unread . "Oläsmarkera vilken markeringstyp (namn eller 0-255, RET för alla)? ")
    (what-mark-to-view . "Återse vilken markeringstyp (namn eller 0-255, RET för alla)? ")
    (whos-passwd . "Vem vill du ändra lösenord för? (dig själv) ")
    (old-passwd . "Mata in ditt nuvarande lösenord: ")
    (new-passwd . "Mata in det nya lösenordet: ")
    (changing-passwd . "Ändrar lösenordet...")
    (retype-dont-match . "Lösenorden är inte samma. Gör om.\n")
    (palindrome . " (en palindrom!)")
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
    (text-to-add-bcc . "Vilket inlägg vill du skicka som dold kopia:")
    (text-to-delete-recipient . "Vilket inlägg vill du subtrahera mottagare från:")
    (text-to-move . "Vilket inlägg vill du flytta:")
    (text-tree-to-move . "Vilket inlägg är rot till trädet du vill flytta:")
    (text-to-add-comment-to . "Vilket inlägg vill du addera en kommentar till:")
    (text-to-delete-comment-from . "Vilket inlägg vill du subtrahera en kommentar från:")
    (text-to-add-footnote-to . "Vilket inlägg vill du addera en fotnot till:")
    (text-to-delete-footnote-from . "Vilket inlägg vill du subtrahera en fotnot från:")
    (text-to-add-cross-reference-to . "Vilket inlägg vill du addera en referens till:")
    (text-has-no-recipients-r . "Inlägg %#1n har inga mottagare\n")

    (where-on-list-q . "Placering på listan? (0-%#1d) ")
    (member-in-conf . "Bli medlem i %#1M%#2?b%[ %#2s%]%[%]...")
    (member-in-conf-with-low-priority . "%#1M har lägre prioritet än din läsnivå och kommer därför inte att visas.\n")
    (member-in-conf-with-unread-date . "med olästa från %#3d %#2s %#1d")
    (member-in-conf-with-unread . "med %#1d olästa")
    (add-member-in . "Addera %#1P som medlem i %#2M...")
    (change-priority-for-q . "Ändra prioritet för möte: ")
    (change-priority-for . "Ändra prioritet på %#2M...")
    (unsubscribe-to . "Utträda ur %#1M...")
    (unsubscribe-to-2 . "Utträda ur %#1M%#2s...")
    (confirm-each-join . "Vill du bekräfta medlemskap i varje enskilt möte? ")
    (no-confirm-each-sure . "Är du helt säker på att du vill gå med i %#1d möten automatiskt? ")
    (confirm-join . "Gå med i %#1M? ")
    (getting-all-confs . "Hämtar en lista med alla möten...")
    (getting-all-confs-progress . "Hämtar en lista med alla möten (%#1d/%#2d)...")
    (getting-all-confs-done . "Hämtar en lista med alla möten...klart")
    (unsub-all-skipping-letterbox . "nix\nMötet är din brevlåda.\n")
    (unsub-all-skipping-supervised . "nix\nDu är organisatör för mötet.\n")
    (unsub-secret-conf-q . "%#1M är hemligt. Vill du gå ur mötet ändå? ")
    (unsub-all-secret . "aGå ur alla hemliga möten")
    (unsub-closed-conf-q . "%#1M är slutet. Vill du gå ur mötet ändå? ")
    (unsub-all-closed . "aGå ur alla slutna möten")
    (unsub-open-conf-q . "Vill du gå ur %#1M? ")
    (unsub-all-open . "aGå ur alla öppna möten")
    (abc-yes . "jJa")
    (abc-no . "nNej")

    (exclude-from . "Utesluta %#1P från %#2M...")

    (unsubscribe-failed . "\nDet gick inte. %#1P var kanske aldrig medlem i %#2M?\n")
    (passivate-done . "Du är nu passiv medlem i %#1M.
Utträd ur mötet en gång till för gå ur helt.\n")

    (You . "Du")
    (could-not-create-conf . "Mötet \"%#1s\" kunde ej skapas.\n")
    (created-conf-no-name . "Möte nummer %[%#3@%#1:m %#2:M%], skapat.\n")
    (cant-read-textno . "Du får inte läsa text %#1:n.\n")

    (not-supervisor-for . "Du är inte organisatör för %#1M.\n")
    (not-supervisor-for-server . "Du är inte administratör för LysKOM-servern.\n")
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
			  "Du har inga markerade inlägg med markeringstypen \"%#1s\".\n")

;;; For later
;    (northward . "norrut")
;    (southward . "söderut")
;    (permanent-sundown . "Solen kommer inte att gå upp. Flytta %#1s!")
;    (permanent-sunup . "Solen kommer inte att gå ned. Flytta %#1s!")
;    (sunup-soon . "Solen går snart upp")
;    (sundown-recently . "Solen gick nyligen ned")
;    (after-sunset . "Solen har gått ned")
;    (before-sunup . "Solen har inte gått upp ännu")
;    (sun-is-up . "Solen är uppe")
;;;
    (weekdays . ["söndag" "måndag" "tisdag" "onsdag" "torsdag"
		 "fredag" "lördag" "söndag"])
    (weekdays-short . ["sön" "mån" "tis" "ons" "tor" "fre" "lör" "sön"])
    (time-is . "Det är %#1s%#2s (enligt servern).")
    (time-is-week . "Det är %#1s%#2s, vecka %#3d (enligt servern).")

    (xmaseve . "\nJulafton! Har du öppnat dina julklappar än?")
    (xmasday . "Juldagen.\nDu har väl varit i julottan?")
    (newyearday . "Nyårsdagen. Gott nytt %#1d!")
    (newyearevelate . "Mindre än en timme kvar av %#1d...")
    (newyeareve . "Nyårsafton.")
    (cgdag . "Valborgsmässoafton.\nKonungens födelsedag.")
    (sixjune . "Sveriges nationaldag och svenska flaggans dag.")
    (holdnose . "Håll för näsan...")
    (lysbday . "
På denna dag, år 1973, grundades Lysator, och det var en stor dag
i svensk datorhistoria. Läs mer på http://www.lysator.liu.se/history/")
    (13dayxmas . "Trettondedag jul (\"Trettondagen\").")
    (20dayxmas . "Tjugondedag jul (\"Tjugondag Knut\").")
    (kyndeldag . "Kyndelsmässodagen.")
    (skottdag . "Skottdagen.")
    (intwomday . "Internationella kvinnodagen.")
    (mariebdag . "Marie bebådelsedag.\nFram med våffeljärnet!")
    (johannesddag . "Johannes Döparens dag.")
    (fnday . "FN-dagen.")
    (allhelgonadag . "Allhelgonadagen.")
    (varnlosdag . "Värnlösa barns dag.")
    (luciaday . "\nLuciadagen! Glöm inte att lussa för dina kollegor.")

    (total-users . "    Sammanlagt %#1d användare (%#2s).\n")
    (total-users-sans-date . "    Sammanlagt %#1d användare\n")
    (total-visible-users . "    Sammanlagt %#1d synlig%#1?d%[%]%[a%] användare (%#2s).\n")
    (total-active-users . "    Sammanlagt %#1d aktiv%#1?d%[%]%[a%] användare (%#2s).\n")
    (total-visible-active-users
     . "    Sammanlagt %#1d synlig%#1?d%[%]%[a%] aktiv%#1?d%[%]%[a%] användare (%#2s).\n")
    (client-statistics . "    Klienternas fördelning:\n")
    (client-statistics-line . "    %4#2d %#1s")
    (who-to-add-q . "Vilket möte/person vill du addera som mottagare? ")
    (who-to-add-copy-q . "Vilket möte/person vill du addera som kopiemottagare? ")
    (who-to-add-bcc-q . "Vilket möte/person vill du addera som mottagare av en dold kopia? ")
    (really-add-as-recpt-q . "Skall framtida kommentarer till inlägget också skickas till %#1M? ") 
    (who-to-sub-q . "Vilket möte/person vill du subtrahera som mottagare? ")
    (who-to-move-from-q . "Från vilket möte vill du flytta texten? ")
    (who-to-move-to-q . "Vart vill du flytta texten? ")
    (who-to-move-to-or-sub-q . "Vart vill du flytta texten (tomt för att bara subtrahera)? ")

    (adding-name-as-recipient . "Adderar %#1M som mottagare till text %#2n...")
    (adding-name-as-copy . "Adderar %#1M som kopiemottagare till text %#2n...")
    (remove-name-as-recipient .
			      "Subtraherar %#1M som mottagare från text %#2n...")
    (adding-cross-reference . "Adderar referens...")

    (error-recipient-limit . "Text %#1n har för många mottagare.\n")
    (error-already-recipient . "%#2M är redan mottagare till inlägg %#1n.\n")
    (error-permission-denied-add-recpt . "Bara författaren till %#1n eller organisatören för %#2M kan ändra mottagartyp.\n")
    (error-permission-denied-sub-recpt . "Bara författaren till %#1n eller organisatören för %#2M kan subtrahera mottagare.\n")
    (error-access-denied-add-recpt . "Du får inte addera inlägg till %#2M.\n")
    (error-not-recipient . "%#2M är inte mottagare till inlägg %#1n.\n")

    (moving-tree-what-action-q . "Vad vill du göra? (flytta) ")
    (moving-name . "Flyttar text %#3n från %#1M till %#2M...")
    (moving-already-moved . "Inlägg %#1n har inte %#2M som mottagare.\nFlyttar inte inlägget eller dess kommentarsträd.\n")
    (text-to-add-q . "Vilket inlägg vill du addera som kommentar till %#1n? ")
    (text-to-remove-q . "Vilket inlägg vill du subtrahera som kommentar från %#1n? ")
    (text-to-add-footn-q . "Vilket inlägg vill du addera som fotnot till %#1n? ")
    (text-to-remove-footn-q . "Vilket inlägg vill du subtrahera som fotnot från %#1n? ")
    (add-comment-to . "Adderar text %#1n som kommentar till text %#2n...") ;
    (sub-comment-to . "Subtraherar text %#1n som kommentar till text %#2n...")
    (add-footnote-to . "Adderar text %#1n som fotnot till text %#2n...") ;
    (sub-footnote-to . "Subtraherar text %#1n som fotnot till text %#2n...")
    (comment-keep-recpt-p ."Ska %#1s vara mottagare? ")
    (comment-all-relevant-p . "Inlägget har flera mottagare. Är alla relevanta? ")
    (please-edit-recipients . "Ändra mottagarlistan och skicka in inlägget igen.")
    (checking-rcpt . "Kontrollerar mottagare...")
    (checking-rcpt-done . "Kontrollerar mottagare...klart")
    (checking-comments . "Kontrollerar kommenterade texter...")
    (checking-comments-done . "Kontrollerar kommenterade texter...klart")
    (please-check-commented-texts . "Återse de kommenterade texterna och deras kommentarer.")
    (have-unread-comment . "Skicka in trots olästa kommentarer till text %#1n? ")
    (duplicate-recipients . "Kan inte skicka in texten. En mottagare förekommer flera gånger (%#1M)")
    (no-recipients . "Kan inte skicka in texten. Inga mottagare har angivits.")

    (add-recipient-p . "Addera mottagare %#2?b%[ %#2M för att nå%]%[%] %#1P? ")
    (matching-regexp . "Möten/personer som matchar '%#1s'\n")
    (matching-regexp-perss . "Personer som matchar '%#1s'\n")
    (matching-regexp-confs . "Möten som matchar '%#1s'\n")

    (who-is-active-all . "Visar alla sessioner.\n")
    (who-is-active-last-minutes . "Visar alla sessioner som har varit aktiva de senaste %#1d minuterna.\n")
    (showing-invisibles . "Visar osynliga sessioner.\n")
    (null-who-info . "Det finns inga (aktiva) inloggade.\n")

    (no-other-lyskom-r . "Det finns inga fler aktiva LysKOM-sessioner.\n")
    (no-lyskom-session . "Det finns ingen aktiv LysKOM-session.")
    (no-other-unread-lyskom-r . "Hittar ingen annan LysKOM-session med olästa.\n")
    (no-unread-lyskom-r . "Hittar ingen aktiv LysKOM-session med olästa.\n")
    (no-unread-lyskom . "Hittar ingen aktiv LysKOM-session med olästa.")

    (who-is-on-in-what-conference . "Vilka i vilket möte: ")
    (who-is-present-in-what-conference . "Vilka närvarande i vilket möte: ")
    (who-is-friend . "Visar endast vänner.\n")
    (who-is-active-and-member . "Visar endast medlemmar i %#1M.\n")
    (who-is-active-and-present . "Visar endast medlemmar närvarande i %#1M.\n")

    (personal-comment-to-anonymous . "Det går inte att skriva ett personligt svar på ett anonymt inlägg.\n")

    ;; From commands2.el:

    (your-memberships . "Ditt medlemskap i olika KOM-möten:\n")
    (memberships-header . "Senast inne	   Prio	Olästa	Mötesnamn\n")
    (memberships-line . "%16#1s  %#2d\t%#3d\t%#4M\n")
    (conf-for-status . "Vilket möte vill du se statusen för? ")
    (no-such-conf . "Mötet finns ej.\n")
    (status-record . "Status för möte %#1M (%#1m)%#2s\n\n")
    (change-type-prompt . "Ändra mötestyp för möte %#1M (%#1m) (%#2s)...")
    (Mailbox . "Brevlåda")
    (Protected . "Hemligt")
    (no-comments . "original")
    (closed . "slutet")
    (allow-anon . "anonymt ok")
    (allow-secret . "hemliga medl")

    (created-by . "Skapat av person %25#1p %#3s(%#2P)\n")
    (created-at . "Skapad:%35#1s\n")
    (members .    "Antal medlemmar: %25#1d\n")
    (conf-allows-secret-members . "Hemliga medlemmar:                       %#1s\n")
    (conf-allows-anon-texts .     "Anonyma inlägg:                          %#1s\n")
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
    (conf-mship-priority . "Prioritet:       %25#1n%#2?b%[ %#2s%]%[%]\n")
    (conf-has-motd . "\n%#1M har en lapp på dörren:\n")
    (status-conf-generic . "%-40#1s %#2s\n")
    (status-aux-item .   "Okänd tilläggsinformation: %15#1s%#3s (skapad av %#2M)\n")
    (conf-mx-list-name . "Importerad mailinglista:                 %#1s %#2s\n")
    (conf-mx-refuse-import . "Vägra import av:                         %#1s %#2s\n")
    (recommended-conf-aux . "Rekommenderat möte:                      %#1M <%#1m> %#2s\n")
    (status-read-faq-aux-item . "Läst FAQ:                 %15#2n för %#1?z%[%#1M <%#1m>%]%[servern%] %#3s\n")
    (status-rejected-recommendation-aux-item . "Avvisad mötesrekommendation:            %#1M %#2s\n")
    (status-send-comments-to . "Dirigera om kommentarer till:           %#1M <%#1m> %#2s\n")

    (Everybody . "Alla")
    (show-members-list-also-q . "Vill du se medlemslistan också? ")
    (show-membership-info-q . "Visa medlemskapsinformation och antalet olästa? ")
    (conf-has-these-members . "\n%#1M har följande medlemmar:\n")
    (conf-has-no-members . "\n%#1M har inga medlemmar.\n")
    (member-list-header . "\nSenast inne         Osett   Namn\n")
    (secret-membership . "*** Hemlig rad ***\n")
    (conf-membership-line . "%#4@%#1s%#2M %#3s\n")
    (conf-membership-line-2 . "                            Adderad %#1s av %#2P\n")
    (pers-for-status . "Vem vill du se statusen för? ")
    (text-to-see-author-status-of . "Vilken texts författare vill du statusen för? ")
    (no-such-pers . "Det finns ingen sådan person.\n")
    (pers-status-record . "Status för person %#1P (%#1p)\n")
    (created-time .  "Skapad:%34#1s\n\n")
    (created-confs . "Skapade möten:%27#1d\n")
    (created-persons . "Skapade personer:%24#1d\n")
    (created-texts . "Skapade texter:%26#1d\n")
    (created-lines . "Skapade rader:%27#1d\n")
    (created-chars . "Antal skapade tecken:%20#1d\n")
    (no-of-sessions . "Antal sessioner:%25#1d\n")
    (present-time-d-h-m-s . "Närvarotid:%19#1d d %02#2d:%02#3d:%02#4d\n")
    (last-log-in . "Senaste in-/utloggning:%18#1s\n")
    (user-name . "Användare: %30#1s\n")

    (read-texts . "Lästa texter:%28#1d\n")
    (marked-texts . "Markerade texter:%24#1d\n")
    (time-for-last-letter . "Tid för senaste brev:%20#1s (står det i din cache)\n")
    (superconf . "Supermöte:%31#1m %#3s(%#2M)\n")
    (supervisor . "Organisatör:%29#1p %#3s(%#2P)\n")
    (member-of-confs . "Medlem i (antal möten):%18#1d\n")
    (presentation . "Presentation:    %24#1n\n")
    (show-membership-list-also-q . "Vill du se vilka möten personen är medlem i också? ")
    (not-allowed-see-confs . "%#1P är inte medlem i några möten.\n")
    (is-member-of . "\n%#1P är medlem i följande möten:\n")
    (membership-list-header . "\nSenast inne           Osett  Namn\n")
    (pers-membership-line . "%#5@%#1s%#2s%#3M %#4s\n")
    (pers-membership-line-2 . "                             Adderad %#1s av %#2P\n")
    (is-supervisor-mark . "O ")
    (who-to-send-message-to . "Vem vill du skicka meddelandet till? (%s) ")
    (send-empty-message-p . "Meddelandet är tomt. Vill du ändå skicka det? ")
    (his-total-unread . "\n%#1M har totalt %#2d olästa (plus %#3d i passiva medlemskap).\n")
    (message-prompt . "Meddelande: ")
    (message-sent-to-user . "\
%[%#3$\
================================================================
%]%[%#4$\
Ditt meddelande till %#2M:

%#1t
%]%[%#3$\
----------------------------------------------------------------
%]")
    (message-sent-to-all . "\
%[%#3$\
================================================================
%]%[%#4$\
Ditt alarmmeddelande löd:

%#1t
%]%[%#3$\
----------------------------------------------------------------
%]")
    (message-sent-to-user-long . "\
%[%#3$\
===============================================================================
%]%[%#4$\
Ditt meddelande till %#2M:

%#1t
%]%[%#3$\
-------------------------------------------------------------------------------
%]")
    (message-sent-to-all-long . "\
%[%#3$\
===============================================================================
%]%[%#4$\
Ditt alarmmeddelande löd:

%#1t
%]%[%#3$\
-------------------------------------------------------------------------------
%]")
    (message-use-alarm-instead . "Använd kommandot %#1s för att skicka alarmmeddelanden.\n")
    (message-all-info . "Sänd alarmmeddelande\n")
    (message-recipient-info . "Sänd meddelande till %#1M\n")
    (message-nope
     . "Det gick inte. %#3s\
Meddelandet du försökte sända till %#1M var:
%#2t\n")
    (only-last . "Endast läsa senaste i %#1s: ")
    (initial-unread . "Initialt antal olästa (tomt för alla): ")
    (only-error . "Något gick galet. Ledsen.\n")

    (you-have-unreads . "Du har %#1d oläst%#1?d%[%]%[a%] inlägg i %#2M\n")
    (you-have-unreads-special . "Du har %#1d okommentera%#1?d%[t%]%[de%] inlägg i %#2M\n")
    (you-have-no-unreads . "Du har inget oläst i %#1M\n")
    (you-have-read-everything . "Du har sett alla nyheter.\n")
    (no-unreads-shown . "Hittade inga möten som matchade det kriteriet.\n")
    (total-unreads . "Du har totalt %#1d oläst%#1?d%[%]%[a%] inlägg i %#2d möte%#2?d%[%]%[n%].\n")
    (shown-unreads . "Visade %#1d oläst%#1?d%[%]%[a%] inlägg i %#2d möte%#2?d%[%]%[n%].\n")
    (list-unread-with-n-unread . "Visar endast möten med minst %#1d olästa.\n")
    (list-unread-with-at-most-n-unread . "Visar endast möten med som mest %#1d olästa.\n")
    (waiting-for-anything .
			  "Du väntar på ett inlägg i vilket möte som helst.\n")
    (waiting-higher-than . 
			 "Du väntar på ett inlägg i ett möte med högre prioritet än %#1d.\n")

    (have-to-be-in-conf-with-unread . "Du måste gå till ett icketomt möte först.\n")
    (Texts . "Inlägg")
    (Written . "Skrivet")
    (Lines . "Rader")
    (Author . "Författare")
    (Subject . "Ärende")
    (Comments . "Ko")
    (mark-type . "Typ")
    (could-not-read . "Du fick inte läsa denna text (%#1n).\n")
    (multiple-choice . "Flera alternativ finns.")
    (does-not-exist . "Detta kommando finns inte.")
    (summary-line . "%=-8#1n%#2s%4#3d  %[%#4@%#5:P%]  %[%#6@%#7r%]\n")
    (diff-what-text-old . "Gammal text att jämföra: ")
    (diff-what-text-new . "Ny text att jämföra: ")


    ;; Only people fixing bugs or receiving bug reports should
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

    ;; Used for kom-is-person-member-of-conference:
    (pers-to-check-mship-for . "Vems medlemskap vill du undersöka? ") ;-)
    (conf-to-check-mship-of . "...i vilket möte? ")
    (conf-is-empty . "Mötet %#1M är tomt.\n")
    (pers-is-member-of-conf . "Ja, %#1P är medlem i mötet %#2M.\n")
    (pers-is-passive-member-of-conf . "Nja, %#1P är bara passiv medlem i mötet %#2M.\n")
    (pers-is-not-member-of-conf . "Nej, %#1P är inte medlem i mötet %#2M.\n")
    (pers-is-member-of-conf-2 . "Antal olästa: %#2D (senast inne %#1s)\n")
    (Unknown-number . "Okänt antal")
    (text-to-check-will-read-for . "...för medlemskap i mottagare till inlägg: ")
    (pers-to-check-will-read-for . "Vilken person vill du kontrollera: ")
    (pers-is-member-of-rcpt . "Ja, %#1P är medlem i minst en mottagare till inlågg %#2n.\n")
    (pers-is-passive-member-of-rcpt . "Nja, %#1P är bara passiv medlem någon mottagare till %#2n.\n")
    (pers-is-not-member-of-rcpt . "Nej, %#1P är inte medlem i någon mottagare till %#2n.\n")

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
    (new-garb-nice-q . "Efter hur många dagar ska inlägg utplånas? ")
    (garb-nice-for-is . "Ändrar livslängden för %#1M till %#2d dagar...")

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
    (who-to-throw-out . "Vems session vill du kasta ut? ")
    (throwing-out . "Kastar nu ut session %#1d... ")
    (postpone-prompt . "Hur lite vill du läsa nu? ")
    (set-session-priority . "Sätt läsnivå: ")

    ;; From review.el:
    (no-unread-done . "Du måste markera inlägg olästa innan du kan markera fler olästa.\n")
    (no-review-done . "Du måste återse innan du kan återse mer.\n")
    (not-reviewing . "Du håller inte på att återse något nu.\n")
    (unread-how-many . "Oläsmarkera hur många?")
    (review-how-many . "Återse hur många?")
    (unread-how-many-more . "Oläsmarkera ytterligare hur många?")
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
    (unread-info . "Oläsmarkera %#1s")
    (unread-info-by-to . "Oläsmarkera  %#1s av %#2P till %#3M.\n")
    (unread-more-info-by-to . "Oläsmarkera %#1s av %#2P till %#3M.\n")
    (unread-rest . "resten")
    (unread-more . "nästa %#1d")

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
    (review-many-comments . "Återse %#2?d%[en%]%[%#2d%] kommentar%#2?d%[%]%[er%] till inlägg %#1n.\n")
    (view-many-comments . "Läs %#2?d%[en%]%[%#2d%] kommentar%#2?d%[%]%[er%] till %#1n.\n")
    (view-texts-in-conf . "Totalt %#1d inlägg att läsa i %#2M.\n")
    (not-reading-anywhere . "Du läser inte i något möte.\n")
    (read-normally-read . "Hur många vill du se igen? ")

    (review-conf-gone . "Mötet finns inte.\n")
    (review-pers-gone . "Personen finns inte.\n")
    (review-cant-read-conf . "Du kan inte återse inlägg till ett slutet möte du inte är med i.\n")
    (review-cant-read-letterbox . "Du kan inte återse inlägg till någon annans brevlåda.\n")
    (review-cant-read-empty . "Mötet är tomt.\n")
    (cant-review-everything . "Du kan inte återse alla texter i LysKOM.\n")
    (cant-unread-everything . "Du kan inte oläsmarkera alla texter i LysKOM.\n")

    (more-than-one-root . "Inlägg %#1n har mer än ett urinlägg.\n")
    (more-than-one-root-review . 
"Inlägg %#1n har mer än ett urinlägg, men endast ett träd kommer att visas.\n")

    ;; From edit-text.el:
    (press-C-c-C-c . "Tryck C-c C-c för att skicka in texten.")
    (recipient . "Mottagare")
    (carbon-copy . "Extra kopia")
    (blank-carbon-copy . "Dold kopia")
    (recipient-prefix . "\\([Mm]\\)")
    (carbon-copy-prefix . "\\([Ee]\\)")
    (blank-carbon-copy-prefix . "\\([Dd]\\)")
    (add-recipient . "Addera mottagare")
    (add-recipient-or-xref . "Addera...")

    (secret-aux-flag . "hemlig")
    (anonymous-aux-flag . "anonym")
    (inherit-aux-flag  . "ärvd")
    (aux-item-prefix . "[*]")
    (aux-item-prefix-regexp . "\\[\\*\\]\\s-*")
    (comment-item-prefix . "#\\s-*")
    (text-no-comment . "%#1d %#2s /%#3d rad%#3?d%[%]%[er%]/ %#4P %#5?b%[ [anonymt]%]%[%]\n")
    (cant-fcc-text-file-error . "Kan inte spara inlägg %#1n till \"%#2s\" (%#3s: %#4s).\n")
    (cant-fcc-text . "Kan inte spara inlägg %#1n till \"%#2s\" (%#3s).\n")

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
    (added-blank-carbon-copy . "Dold kopia till: ")
    (text-to-comment-q . "Vilket inlägg vill du kommentera? ")
    (conf-has-motd-no . "Mötet har en lapp på dörren. (%#1d)\n\n%#2s")
    (still-want-to-add . "Vill du fortfarande addera mötet? ")
    (could-not-create-text . "\nTexten kunde ej skapas. %#2s.\n")
    (no-get-text . "Du fick inte hämta texten.")
    (unknown-header . "Okänd information på raden")
    (transform-error . "Skicka in oformatterat (%#1s)? ")
    (cant-find-info-node . "Hittar inte info-bufferten")

    (link-type . "Vad vill du länka till (inlägg, möte eller person)? ")
    (which-text-to-link . "Lägg in länk till text nummer: ")
    (which-text-to-link-err . "Hittar inte texten. Lägg in länk till text nummer: ")
    (which-pers-to-link . "Lägg in länk till person: ")
    (which-conf-to-link . "Lägg in länk till möte: ")

    ;; From view-text.el:
    (view-text-first-line . "%#7$%#2@%#1n %#3s /%#4d rad%#4?d%[%]%[er%]/ %#5P%#6?b%[%#6s%]%[%]\n")

    (marked-by-you . "Markerad av dig (typ: %#1s).\n")
    (marked-by-you-and-others . "Markerad av dig (typ: %#2s) och %#1?d%[någon annan%]%[%#1d andra%].\n")
    (marked-by-several . "Markerad av %#1d person%#1?d%[%]%[er%].\n")

    (timeformat-day-yyyy-mm-dd-hh-mm-ss . "%#7s %4#1d-%02#2d-%02#3d %02#4d:%02#5d:%02#6d")
    (timeformat-yyyy-mm-dd-hh-mm-ss . "%4#1d-%02#2d-%02#3d %02#4d:%02#5d:%02#6d")
    (timeformat-yyyy-mm-dd-hh-mm . "%4#1d-%02#2d-%02#3d %02#4d:%02#5d")
    (timeformat-yyyy-mm-dd . "%4#1d-%02#2d-%02#3d")
    (timeformat-hh-mm-ss . "%02#4d:%02#5d:%02#6d")
    (timeformat-hh-mm . "%02#4d:%02#5d")

    (format-time-date-and-time . "%#1s %#2s")
    (format-time-just-date . "%#1s")
    (format-time-just-time . "%#2s")
    (today . "idag")
    (yesterday . "igår")

    (no-such-text-no . "Det finns inget sådant inlägg. (%#1:n)\n")
    (text-created-at . "Skapad: %#1s\n")
    (text-imported-at . "Importerad: %#1s\n")
    (text-imported-at-by . "Importerad: %#1s av %#2P\n")

    (head-Subject . "Ärende: ")
    (Recipient . "Mottagare")
    (Extra-recipient . "Extra kopia")
    (Hidden-recipient . "Dold kopia")
    (mx-Recipient . "Extern mottagare")
    (mx-Extra-recipient . "Extern kopiemottagare")
    (mx-Extern-reply-to . "Externa svar till")
    (Strange-recipient . "Underlig mottagare")
    (send-at . "    Sänt:     %#1s\n")
    (sent-by . "    Sänt av %#1P\n")
    (received-at . "    Mottaget: %#1s\n")

    (comment-to-text . "Kommentar till text %#1n")
    (footnote-to-text . "Fotnot till text %#1n")
    (comment-in-text . "Kommentar i text %#1n")
    (footnote-in-text . "Fotnot i text %#1n")

    (comment-to-text-by . "Kommentar till text %#1n av %#2P")
    (footnote-to-text-by . "Fotnot till text %#1n av %#2P")
    (comment-in-text-by . "Kommentar i text %#1n av %#2P")
    (footnote-in-text-by . "Fotnot i text %#1n av %#2P")

    (attachment-to-text . "Bilaga %#3still text %#1n")
    (attachment-in-text . "Bilaga %#3si text %#1n")
    
    (envelope-sender . "Sänt av: %#1s\n")
    (attachment-filename . "Bilagans filnamn: \"%#1s\"\n")

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
    (lyskom-is-full-long . "\
===============================================================================
Meddelande från LysKOM-systemet: Någon försökte koppla upp,
men misslyckades eftersom alla tillgängliga förbindelser är
upptagna. Logga ut och kom tillbaks senare om du väntar nu.
===============================================================================
")
    (has-entered . "Nu har %#1:P gått in i %#2s.")
    (has-entered-r . "%#2@Nu har %#1P gått in i %#3s.\n")
    (has-left . "Nu har %#1:P gått ur %#2s.")
    (has-left-r . "%#2@Nu har %#1P gått ur %#3s.\n")
    (Unknown . "Okänd")
    (unknown . "okänd")
    (Unknown2 . "Okänt")
    (unknown-person . "Okänd person")

    (no-longer-member . "Du är inte längre medlem i %#1M.\n")
    (no-longer-member-n . "Du är inte längre medlem i möte %#1m (finns inte).\n")

    (have-become-member . "Du har blivit medlem i %#1M.\n")
    (have-become-invited-member . "Du har bjudits in till %#1M.
Gå till mötet för att acceptera eller tacka nej till inbjudan. Du kan även
använda kommandot 'Bli medlem i möte' för att acceptera inbjudan.\n")
    (have-become-passive-member . "Du har blivit passiv medlem i %#1M.\n")

    (message-broadcast . "\
%[%#4$\
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#5$\
Alarmmeddelande från %#1P (%#3s):

%#2t
%]%[%#4$\
----------------------------------------------------------------
%]")
    (message-broadcast-long . "\
%[%#4$\
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#5$\
Alarmmeddelande från %#1P (%#3s):

%#2t
%]%[%#4$\
-------------------------------------------------------------------------------
%]")
    (message-from . "\
%[%#4$\
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#5$\
Personligt meddelande från %#1P (%#3s):

%#2t
%]%[%#4$\
----------------------------------------------------------------
%]")
    (message-from-long . "\
%[%#4$\
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#5$\
Personligt meddelande från %#1P (%#3s):

%#2t
%]%[%#4$\
-------------------------------------------------------------------------------
%]")
    (message-from-to . "\
%[%#5$\
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#6$\
Gruppmeddelande till %#3M\nfrån %#2P (%#4s):

%#1t
%]%[%#5$\
----------------------------------------------------------------
%]")
    (message-from-to-long . "\
%[%#5$\
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#6$\
Gruppmeddelande till %#3M\nfrån %#2P (%#4s):

%#1t
%]%[%#5$\
-------------------------------------------------------------------------------
%]")
    (text-is-created . "Text %#1n är skapad!")

    ;; Used in mode-line-process
    (mode-line-waiting . ": väntar")
    (mode-line-working . ": arbetar")
    (mode-line-saving . ": sparar")
    (mode-line-down . ": nerkopplad")

    ;; From completing-read.el:
	   
    (person-or-conf-no-regexp . "\\`[ \t]*[mpMP]\\w*[ \t]+\\([0-9]+\\)\\'")
    (session-no-regexp . "\\`[ \t]*[sS]\\w*[ \t]+\\([0-9]+\\)\\'")
    (conf-prompt . "Vilket möte/person? ")

    ;; From prioritize.el:

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

    (cannot-get-membership . "Kan ej hämta medlemskap för dig.")
    (cannot-get-pers-stat . "Kan ej hämta personstatus för dig.")
    (prioritize-help .
		     "u,n Flytta möte, SPC markera, p prioritera markerade, q avsluta, C-h m hjälp")
    (your-priorities . " Prioritet  Mötesnamn
-------------------------------------------------------------------------------
")
    (your-membship . "Ditt medlemskap i olika KOM-möten:
  Prio Mötesnr Mötesnamn\n")
    (prio-row . " %5#1d%5#2m  %#3M\n")
    (too-high-goto-2 . "Du står för högt upp. Gå ner till rad 2.")
    (too-low-go-up . "Du kan inte pusha sista raden. Gå upp en rad.")
    (all-confs-popped .  "Alla möten är poppade.")
    (prio-died . "Flyttningen misslyckades. Ledsen. Döda bufferten.")
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
    (error-in-options-short . "Det fanns ett fel i dina inställningar. Vissa inställningar har inte lästs in.")

    ;; From elib-string.el:
    ;; No entries.
		
    ;; From lyskom-rest.el:
		
    (mode-line-unread . " Olästa ")
    (mode-line-letters . "brev ")
    (frame-title-unread . "Olästa")
    (frame-title-letters . " brev")
    (sessions-with-unreads . "Sessioner med olästa")
    (unread-letters . "olästa brev")

    (bad-text-no-prefix . "Kan inte översätta prefix `%s' till inläggsnummer")
    (prefix-arg-try-again . "Ange en annan text eller tryck control-g för att avbryta.\n")
    (error-code . "%#2?+%[Felkod %#2d/%#3S: %]%[%]%#1s.\n")
    (error-in-kom-do-when-done . "Variabeln kom-do-when-done har ett felaktigt värde.
Du bör sätta den till ett bättre värde.\n")
    (extended-command . "LysKOM: ")
    (wait-for-server . "LysKOM väntar på svar från servern. Vänta tills du får en prompt.\n")
    (review-text-q . "Återse text nummer: ")

    (completely-read-conf . "Du har sett alla texter i detta möte.\n")
    (not-in-any-conf . "Du läser inte något möte just nu.\n")

    (all-conf-unread-r . "Du har läst ut alla möten.\n")
    (all-conf-unread-s . "Du har läst ut alla möten. ")
    (enter-conf-unread . "%#1d oläst%#1?d%[%]%[a%]")
    (enter-conf-unread-faq . "%#1d oläst%#1?d%[%]%[a%] FAQ")

    (save-one-on-file-q . "Arkivera inlägg %#1n till fil: ")
    (save-many-on-file-q . "Arkivera %#1d inlägg till fil: ")
    (saving-one-on-file . "Arkiverar inlägg %#1n i %#2s.\n")
    (saving-many-on-file . "Arkivera %#1d inlägg i %#2s.\n")
    (save-text-to-file-q . "Spara inläggstext %#1n på fil: ")
    (save-text-confirm . "Filen %#1s finns redan. Vill du skriva över den? ")
    (saving-text . "Sparar inlägg %#1n som %#2s...")
    (what-save-no . "Vilket inlägg vill du spara? ")
    (wait-for-prompt . "Vänta på prompten.")

    (conference-no . "<möte %#1d>")
    (person-no . "<person %#1d>")
    (prompt-several-messages . "(%d meddelanden)")
    (prompt-single-message   . "(%d meddelande)")

    (text-buffer-missing . "Inläggsbufferten existerar inte längre.\n")

    (re-edit-text-prompt . "Redigera texten som inte kunde skapas")
    (go-to-pri-conf-prompt . "Gå till nästa prioriterade möte")
    (read-pri-text-conf . "Läsa nästa prioriterade text")
    (review-next-text-prompt . "Återse nästa text")
    (review-next-comment-prompt . "Återse nästa kommentar")
    (review-next-marked-prompt . "Återse nästa markerade")
    (review-next-faq-prompt . "Återse nästa FAQ")
    (read-next-letter-prompt . "Läsa nästa brev")
    (read-next-footnote-prompt . "Läsa nästa fotnot")
    (read-next-comment-prompt . "Läsa nästa kommentar")
    (read-next-text-prompt . "Läsa nästa text")
    (read-next-attachment-prompt . "Läsa nästa bilaga")
    (go-to-conf-of-marked-prompt . "Återuppta återse markerade")
    (go-to-conf-of-review-tree-prompt . "Återuppta återse kommentarer")
    (go-to-conf-of-review-prompt . "Återuppta återse")
    (go-to-conf-of-review-faq-prompt . "Återuppta återse FAQ")
    (go-to-next-conf-prompt . "Gå till nästa möte")
    (go-to-your-mailbox-prompt . "Gå till din brevlåda")
    (next-pri-session-prompt . "Gå till prioriterat LysKOM \"%#1s\"")
    (next-unread-session-prompt . "Gå till LysKOM \"%#1s\"")

    (no-such-kom-session . "Det finns ingen sådan session med olästa.\n")
    (the-command . "Kommandot: %#1C")
    (error-in-login-hook . "Det fanns ett fel i din kom-login-hook: %#1s\n")

    (give-a-number . "Mata in ett tal: ")

    (yes-regexp . "\\`[jJ][aA]\\'")
    (no-regexp . "\\`[nN][eE][jJ]\\'")
    (yes-string . "Ja")
    (no-string . "Nej")
    (yes-or-no-nag . "Svara bara ja eller nej.")
    (yes-or-no . "(ja eller nej) ")

    (y-or-n-instring . "jJnN ")
    (j-or-n-nag . "Svara bara j eller n. ")
    (j-or-n . "(j eller n) ")
    (y-instring . "jJ ")

    (a-or-b-or-c-nag . "Ge ett giltigt alternativ. ")

    (person-does-not-exist . "Person %#1d (finns inte).")
    (conference-does-not-exist . "Möte %#1d (finns inte).")
    (conf-no-does-not-exist-r . "Möte %#1d finns inte.")
    (person-is-anonymous . "Anonym person")

    (process-signal . "Signal från processen.")
    (closed-connection . "
**************************************************
%#2s
LysKOM-sessionen onormalt stängd.
Felmeddelande: %#1s**************************************************")
    (dead-session . "LysKOM-sessionen är inte aktiv.")
    (not-lyskom-buffer . "Detta är inte en aktiv LysKOM-session.")
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
    (comment-prefix . "\\([Kk]\\)")
    (footnote . "Fotnot")
    (footnote-prefix . "\\([Ff]\\)")

    (by . " av %#1P")
    (text-created .  "Text nummer %#1n är skapad.\n")
    (text-created-anonymous .  "\
Text nummer %#1n är skapad (anonymt). För att göra det svårare för 
andra att ta reda på att du skapade inlägget bör du vänta ett tag med
att läsa det.\n")

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
    (filter-recipient . "Vilken mottagare vill du filtrera? ")
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
    (ansaphone-message-long . "Svarsmeddelande:
-------------------------------------------------------------------------------
%#1t
-------------------------------------------------------------------------------
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
    (remote-set-message-long . "Fjärrstyrning (%#1P %#2s): Svarsmeddelande:
-------------------------------------------------------------------------------
%#3t
-------------------------------------------------------------------------------
")    (remote-set-ansaphone . "Fjärrstyrning (%#1P %#2s): Automatsvar är nu %#3s\n")
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
    (timestamp-popup-title . "Tidsstämpel %#1s")
    (recpt-type-popup-title . "Mottagartyp: %#1s")
    (add-recpt-button-text . "[Addera...]")
    (add-recpt-button-text-regex . "\\[Addera\\.\\.\\.\\]")
    (generic-popup-title . "%#1s")

    (who-i-am-not-present . "%#1P är inte närvarande i något möte\n")
    (who-i-am-present . "%#1P är närvarande i %#2M\n")
    (who-i-am-client . "Programmet heter lyskom.el, version %#1s%#2?b%[ (MULE)%]%[%].\n")
    (who-i-am-server . "Detta är %#1s, version %#2s.\n")
    (who-i-am-emacs . "Det körs under %#1s%#2?b%[ (MULE)%]%[%].\n")

    (no-such-session-r . "Det finns ingen sådan session. Personen kanske inte är inloggad.\n")
    (person-not-logged-in-r . "%#1P %#2?b%[har inte varit inloggad sedan %#2s%]%[är inte inloggad%].\n")
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
    (years . "år")
    (year . "år")
    (months . "månader")
    (month . "månad")
    (days . "dagar")
    (day . "dag")
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

    ;; From slow.el
    (no-such-command . "Det finns inget sådant kommando.\n")
    (command-completions . "Du kan mena någon av följande:\n %#1s\n")
    (which-language . "Ändra språk till: ")
    (send-formatted . "Skicka in som formatterad text? ")
    (changing-language-to . "Byter till %#1_s.\n")
    (language-set-to . "Språket är %#1_s.\n")
    (language-not-loaded . "%#1s finns inte tillgängligt.\n")

    (reformat-html . "HTML")
    (reformat-enriched . "enriched")
    (reformat-filled . "ombruten")

    (reformat-truncated . "nedkortad")

    (reformat-signature . "avsignerad")

    (reformat-deswascii . "var swascii")

    (need-library . "Paketet \"%#1s\" behövs för att utföra detta kommando.\n")
    (calc-expression . "Uttryck: ")

    (do-send-anonymous . "Skicka in texten anonymt? ")
    (remove-self-sending-anonymous . "Vill du ta bort dig själv som mottagare från inlägget? ")
    (anonymous . "anonym")
    (Anonymous . "Anonym")

    (secret-conf-letter . "H")
    (protected-conf-letter . "S")
    (created-conf-letter . "S")
    (superconf-conf-letter . "M")
    (supervisor-conf-letter . "O")

    ;; Some Help

    (where-is-doesnt-exist . "Kommandot %#1s finns inte")
    (where-is-on-no-key . "%#1s finns inte på någon tangent")
    (where-is-on-key . "%#1s finns på %#2s")

    ;; From aux-items.el

    (content-type-aux . "Innehållstyp: %#1s")
    (content-type-regexp . "Innehållstyp: \\(\\S-+\\)")
    
    (agreeing . "Håller med om inlägg %#1n...")
    (fast-replying . "Anmärkning till inlägg %#1n...")
    (author-fast-replies . "Anmärkningar av författaren:")
    (author-fast-reply-aux . "  \"%#1t\"")
    (other-fast-replies . "Anmärkningar:")
    (other-fast-reply-aux . "  \"%#1t\" /%#2P/")
    (fast-reply-too-long . "Kan inte skapa anmärkningar som består av mer än en rad.\n")
    
    (faq-for-conf-aux . "Texten är FAQ för %#1M <%#1m>") 
    (faq-for-server-aux . "Texten är FAQ för LysKOM-servern") 
    (faq-in-text-aux . "FAQ i inlägg:                   %10#1n %#2D")
    (conf-to-add-faq . "Vilket möte vill du lägga till en FAQ för? ")
    (text-to-add-as-faq . "Vilken text vill du lägga till som FAQ? ")
    (text-to-change-as-faq . "Vilken FAQ vill du ändra? ")
    (adding-faq . "Lägger till inlägg %#1n som FAQ för %#2?b%[%#2M%]%[servern%]...")
    (conf-to-del-faq . "Vilket möte vill du ta bort en FAQ från? ")
    (text-to-del-as-faq . "Vilken text vill du ta bort som FAQ? ")
    (deleting-faq . "Tar bort inlägg %#1n som FAQ för %#2?b%[%#2M%]%[servern%]...")
    (conf-has-no-faq . "%#1?b%[%#1M%]%[Servern%] har ingen FAQ\n")
    (unread-which-faq . "Återse FAQ för vilket möte? ")
    (view-which-faq . "Återse FAQ för vilket möte? ")
    (review-faq-for-r . "Återse FAQ för %#1?b%[%#1M%]%[servern%].\n")
    (set-faq-for-conf-done . "Text %#3n är nu FAQ för %#1?b%[%#1M%]%[servern%].\n")
    (set-faq-for-conf-failed . "Kunde inte sätta FAQ för %#1?b%[%#1M%]%[servern%] till text %#3n.\n")
    (changed-faq-for-conf-done . "FAQ i text %#2n för %#1?b%[%#1M%]%[servern%] ändrad till text %#3n.\n")
    (changed-faq-for-conf-failed . "Kunde inte ändra FAQ för %#1?b%[%#1M%]%[servern%] till text %#3n.
%#2?b%[Text %#2n är fortfarande FAQ.
%]%[%]%#4s")
    (faq-in-text . "FAQ i text %#1n %#3s%#4s")
    (faq-in-text-by . "FAQ i text %#1n %#5s %#3sav %#2P %#4s")
    (server-has-new-faq . "\nDet finns %#1d ny%#1?d%[%]%[a%] FAQ till servern.\n")
    (unread-faqs-header . "\nOläst%#1?d%[%]%[a%] FAQ till %#2?b%[%#2M%]%[servern%]:\n")

    (too-many-languages . "För många teckenuppsättningar för att koda. Skicka in okodat? ")
    (too-many-content-types . "Kan inte avgöra innehållstyp. Förenkla inlägget.")

    (cross-reference-text-status-aux . "Se även inlägg:                 %10#1n /%#2P/")
    (cross-reference-conf-status-aux . "Se även möte:                           <%#1m> %#1M /%#2P/")
    (cross-reference-pers-status-aux . "Se även person:                         <%#1p> %#1P /%#2P/")
    (strange-cross-reference-status .  "Se även:                                 %#1s (vad nu det betyder)")
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
    
    (request-confirmation-aux . "%#1P har begärt läsbekräftelse")
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

    (creating-software-aux . "Skapad med %#1s")
    (world-readable-text-aux . "Inlägget kan läsas utan inloggning")
    (world-readable-text-edit-aux . "Gör inlägget läsbart utan inloggning")

    (cant-get-aux-item . "Hittar inte tilläggsinformationen\n")
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
    (aux-item-for-conference-no . "möte <%#1m> %#1M")
    (aux-item-for-text-no . "inlägg %#1n")
    (aux-item-for-server . "servern")

    (what-fast-reply-no . "Anmärkning till vilket inlägg? ")
    (fast-reply-prompt . "Anmärkning: ")
    (agree-prompt . "Text: ")
    (default-agree-string . "Håller med")
    (what-agree-no . "Håll med vilket inlägg? ")

    (what-kind-to-add-aux-to . "Koppla tilläggsinformation till vad? ")
    (which-conf-to-add-aux-to . "Vilket möte vill du koppla till? ")
    (which-text-to-add-aux-to . "Vilket inlägg vill du koppla till? ")
    (which-aux-item-tag . "Aux-item tag: ")
    (which-aux-item-inherit . "Sätt inherit-flaggan? ")
    (which-aux-item-secret . "Sätt secret-flaggan? ")
    (which-aux-item-anonymous . "Sätt anonymous-flaggan? ")
    (which-aux-item-rsv1 . "Sätt dont-garb-flaggan? ")
    (which-aux-item-rsv2 . "Sätt reserved2-flaggan? ")
    (which-aux-item-rsv3 . "Sätt reserved3-flaggan? ")
    (which-aux-item-rsv4 . "Sätt reserved4-flaggan? ")
    (which-aux-item-inherit-limit . "Arvsgräns: ")
    (which-aux-item-data . "Data: ")

    ;; Cross-reference stuff

    (person . "Person")
    (conference . "Möte")
    (text . "Inlägg")
    (server . "Server")

    (xref-type . "Vad vill du referera till (inlägg, möte eller person)? ")
    (which-text-to-xref . "Lägg till referens till text nummer: ")
    (which-text-to-xref-err . "Hittar inte texten. Lägg till referens till text nummer: ")
    (which-pers-to-xref . "Lägg till referens till person: ")
    (which-conf-to-xref . "Lägg till referens till möte: ")

    ;; Aux-item stuff from edit-text

    (no-comments-q . "Författaren vill inte ha kommentarer. Kommentera ändå? ")
    (private-answer-q . "Författaren har bett om personliga svar. Skriv personligt svar? ")

    (your-invited . "%#2P har bjudit in dig till %#1M.
Genom att acceptera inbjudan fortsätter du vara medlem i %#1M.
Du kan även tacka nej, och då kommer du automatiskt att gå ur mötet.
")

    (accept-invitation . "Vill du acceptera inbjudan och fortsätta vara medlem? ")
    (enter-passive . "Du är passiv medlem i %#1M.
Du måste bli aktiv medlem för att gå till mötet.\n")
    (convert-passive . "Vill du bli aktiv medlem i %#1M? ")
    (bug-secret-mship . "Du är hemlig medlem i %#1M\n")

    (invitation-mt-type . "inbjuden")
    (passive-mt-type . "passiv")
    (secret-mt-type . "hemlig")

    (Invitation-mt-type . "Inbjuden")
    (Passive-mt-type . "Passiv")
    (Secret-mt-type . "Hemlig")

    (not-author-try-anyway-p . "Du är inte författare till inlägget. Vill du försöka ändå? ")
    (what-no-comments-no . "Vilken text vill du inte ha kommentarer till: ")
    (adding-no-comments . "Begär att inte få kommentarer till inlägg %#1n...")
    (already-no-comments . "Inlägg %#1n har redan begäran om inga kommentarer.\n")

    (what-private-answer-no . "Vilken text vill du ha personligt svar till: ")
    (adding-private-answer . "Begär att endast få personligt svar till inlägg %#1n...")
    (already-private-answer . "Inlägg %#1n har redan begäran om enbart personliga svar.\n")

    (what-request-confirm-no . "Vilken text vill du ha läsbekräftelse till: ")
    (adding-request-confirm . "Begär läsbekräftelse till inlägg %#1n...")
    (already-request-confirm . "Inlägg %#1n har redan begäran om läsbekräftelse.\n")

    (review-mail-headers-to-what . "Vilken texts brevhuvud vill du se? ")
    (no-mail-headers . "Inlägg %#1n har inget brevhuvud\n")
    (mail-headers-for . "Brevhuvud för inlägg %#1n:\n")
    (email-name-prefix . "")
    (email-name-suffix . "")

    (you-are-anonymous . "Du är nu någorlunda anonym.\n")
    (you-are-nonanonymous . "Du är inte längre anonym.\n")
    (you-are-already-anonymous . "Du är redan någorlunda anonym.\n")
    (you-are-already-nonanonymous . "Du är redan icke-anonym.\n")

    (start-keep-alive . "Sänder data med %#1d sekunders mellanrum för att hålla förbindelsen aktiv.")
    (stop-keep-alive . "Data sänds inte längre för att hålla förbindelsen aktiv.")

    (review-converted-q . "Återse vilket inlägg konverterat?")
    (review-noconversion-q . "Återse vilket inlägg omodifierat?")
    (unread-commented-q . "Oläsmarkera vilket inläggs kommenterade?")
    (review-commented-q . "Återse vilket inlägg kommenterade?")
    (unread-tree-q . "Oläsmarkera alla kommentarer rekursivt för vilket inlägg?")
    (review-tree-q . "Återse alla kommentarer rekursivt för vilket inlägg?")
    (unread-root-q . "Oläsmarkera urinlägget för vilken text?")
    (find-root-q . "Återse urinlägget för vilken text?")
    (unread-root-review-q . "Oläsmarkera träd för vilket inlägg?")
    (find-root-review-q . "Återse träd för vilket inlägg?")
    (unread-comments-q . "Oläsmarkera alla kommentarer till vilket inlägg?")
    (review-comments-q . "Återse alla kommentarer till vilket inlägg?")

    (confusion-who-to-reply-to . "Jag förstår inte vems inlägg du vill skriva ett privat svar till.\n")
    (confusion-what-to-answer-to . "Jag förstår inte vilken text du vill besvara.\n")
    (confusion-what-to-view . "Jag förstår inte vilken text du vill återse.\n")

    (confusion-what-to-reply-to . "Jag förstår inte vilken text du vill skriva en anmärkning till.\n")
    (confusion-what-to-agree-to . "Jag förstår inte vilken text du vill hålla med om.\n")
    (confusion-what-to-request-confirmation . "Jag förstår inte vilken text du vill ha läsbekräftelser på.\n")
    (confusion-what-to-comment . "Jag förstår inte vilken text du vill kommentera.\n")
    (confusion-what-to-footnote . "Jag förstår inte vilken text du vill skriva en fotnot till.\n")

    (confusion-what-to-delete . "Jag förstår inte vilken text du vill radera.\n")
    (confusion-what-to-add-sub-recipient . "Jag förstår inte vilken text du vill %#1s.\n")
    (add-rcpt-action-name . "addera mottagare till")
    (add-copy-action-name . "addera extra-kopiemottagare till")
    (add-bcc-action-name . "addera dold kopia till")
    (sub-action-name . "subtrahera mottagare från")
    (move-action-name . "flytta")

    (confusion-what-to-add-comment-to . "Jag förstår inte vilken text du vill addera en kommentar till.\n")
    (confusion-what-to-sub-comment-from . "Jag förstår inte vilken text du vill subtrahera en kommentar från.\n")
    (confusion-what-to-add-footnote-to . "Jag förstår inte vilken text du vill addera en fotnot till.\n")
    (confusion-what-to-sub-footnote-from . "Jag förstår inte vilken text du vill subtrahera en fotnot från.\n")
    
    (confusion-what-to-mark . "Jag förstår inte vilket inlägg du vill markera.\n")
    (confusion-what-to-unmark . "Jag förstår inte vilket inlägg du vill avmarkera.\n")
    (confusion-what-to-save . "Jag förstår inte vilket inlägg du vill spara.\n")

    (confusion-what-to-review-mail-headers . "Jag förstår inte vilket inläggs brevhuvud du vill se.\n")
    (confusion-what-to-unread-root . "Jag förstår inte vilket inläggs urinlägg du vill markera oläst.\n")
    (confusion-what-to-find-root . "Jag förstår inte vilket inläggs urinlägg du vill återse.\n")
    (confusion-what-to-unread-root-review . "Jag förstår inte vilket inläggsträd du vill markera oläst.\n")
    (confusion-what-to-find-root-review . "Jag förstår inte vilket inläggsträd du vill återse.\n")


    ;; Help

    (help-with-what . "Vad vill du ha hjälp med? ")
    (help-for . "Hjälp för \"%#1s\"\n")

    ;; Button actions

    (lyskom-button-view-text-action . "Återse texten")
    (lyskom-button-copy-text-no-action . "Kopiera inläggsnumret")
    (lyskom-button-review-converted-action . "Återse konverterat")
    (lyskom-button-review-noconversion-action . "Återse omodifierat")
    (lyskom-button-find-root-review-action . "Återse träd")
    (lyskom-button-find-root-action . "Återse urinlägget")
    (lyskom-button-comment-text-action . "Kommentera texten")
    (lyskom-button-private-comment-text-action . "Personligt svar")
    (lyskom-button-mark-text-action . "Markera texten")
    (lyskom-button-unmark-text-action . "Avmarkera texten")
    (lyskom-button-save-text-action . "Arkivera inlägg")
    (lyskom-button-save-text-body-action . "Spara inläggstext")
    (lyskom-button-review-comments-action . "Återse alla kommentarer")
    (lyskom-button-review-tree-action . "Återse alla kommentarer rekursivt")
    (lyskom-button-write-footnote-action . "Fotnotera")
    (lyskom-button-fast-reply-action . "Anmärk")
    (lyskom-button-view-conf-presentation-action . "Visa presentation")
    (lyskom-button-view-conf-status-action . "Visa mötesstatus")
    (lyskom-button-goto-conf-action . "Gå till mötet")
    (lyskom-button-send-message-action . "Skicka gruppmeddelande")
    (lyskom-button-add-self-action . "Bli medlem i mötet")
    (lyskom-button-sub-self-action . "Utträd ur mötet")
    (lyskom-button-view-pers-presentation-action . "Visa presentation")
    (lyskom-button-view-pers-status-action . "Visa personstatus")
    (lyskom-button-view-session-status-action . "Visa sessionsstatus")
    (lyskom-button-mail-action . "Skicka brev")
    (lyskom-button-send-message-action . "Sänd meddelande")
    (lyskom-button-open-url-action . "Öppna")
    (lyskom-button-copy-url-action . "Kopiera")
    (lyskom-button-goto-info-node-action . "Öppna")
    (lyskom-button-open-email-action . "Skicka mail")
    (lyskom-button-copy-email-action . "Kopiera")
    (lyskom-button-info-aux-action . "Information")
    (lyskom-button-delete-aux-action . "Radera")
    (lyskom-button-copy-timestamp-action . "Kopiera")
    (lyskom-button-recpt-type-recipient . "Mottagare")
    (lyskom-button-recpt-type-copy . "Extra kopia")
    (lyskom-button-recpt-type-bcc . "Dold kopia")
    (lyskom-button-recpt-type-sub . "Subtrahera")
    (lyskom-button-recpt-add-recipient . "Mottagare")
    (lyskom-button-recpt-add-copy . "Extra-kopiemottagare")
    (lyskom-button-recpt-add-bcc . "Dold-kopiamottagare")
    (lyskom-button-aux-type-xref . "Referens till möte/person/inlägg")
    (lyskom-button-aux-type-no-comments . "Begäran om inga kommentarer")
    (lyskom-button-aux-type-personal-comments . "Begäran om personligt svar")

    (lyskom-edit-toggle-secret-aux-action . "Växla \"hemlig\"")
    (lyskom-edit-toggle-anonymous-aux-action . "Växla \"anonym\"")
    (lyskom-edit-toggle-inherit-aux-action   . "Växla \"ärvd\"")
    (lyskom-edit-delete-aux-action          . "Ta bort")
    (lyskom-prioritize-flag-toggle-action . "Växla")
    (lyskom-prioritize-flag-set-action . "Sätt på")
    (lyskom-prioritize-flag-clear-action . "Stäng av")

    (server-status-header   . "Status för LysKOM-server %#1s%#2?b%[ (%#2s:%#3d)%]%[%]\n\n")
    (server-status-server   . "Kanonisk server:                         %#1s%#2?b%[:%#2s%]%[%]\n")
    (server-status-version  . "Programversion:                          %#1s %#2s\n")
    (server-status-protocol . "Protokollversion:          %15#1d\n")
    (server-status-sessions . "\
Antal sessioner:     %21#1d (totalt)
                     %21#2d aktiva under de senaste %#7d minuterna
                     %21#3d inaktiva sessioner
                     %21#4d okänd aktivitet
                     %21#5d osynliga sessioner
                     %21#6d ej inloggade/hemliga/zombies\n")
    (server-status-first-text . "Äldsta befintliga text:    %15#1n\n")
    (server-status-last-text  . "Yngsta befintliga text:    %15#1n\n")
    (server-status-has-motd . "\nServern har en lapp på dörren:\n")
    (server-status-time . "Serverns tid:                   %#1s\n")

    (mship-type-invitation-q . "Inbjudan till medlemskap? ")
    (mship-type-passive-q . "Passivt medlemskap? ")
    (mship-type-secret-q . "Hemligt medlemskap? ")
    (recommend-which-conf . "Vilket möte vill du rekommendera? ")
    (recommend-set-priority-q . "Rekommendera prioritet? ")
    (recommend-set-mship-type-q . "Rekommendera mötestyp? ")
    (recommending-conf . "Rekommenderar %#1M%#2?b%[ (prioritet %#2d)%]%[%]%#3?b%[ %#3s%]%[%]...")
    (has-recommended-conf . "%#1P har rekommenderat möte %#2M. Vill du bli medlem?\n")
    (accept-recommendation . "Gå med i %#1M? ")
    (accepting-recommendataion . "Gå med i rekommenderat möte %#2M...")

    (redirect-for-whom . "Dirigera om kommentarer för: ")
    (redirect-to-which-conf . "Dirigera om kommentarer till: ")
    (redirecting-comments-to . "Dirigerar om kommentarer för %#1P till %#2M%#3?b%[ (förr %#1M)%]%[%]...")

    (kom-redirect-comments-e49 . "Du har inte rätt att ändra denna omdirigering")
    (kom-redirect-comments-e48 . "Omdirigering av kommentarer stöds inte av servern")

    (external-program-missing . "Hittar inte det externa programmet \"%#1s\" som krävs för att genomföra kommandot.")
    (ignored-user-area-var . "\

Ignorerar följande variabler i dina inställningar:

    %#1s

Variablerna är inte registrerade LysKOM-variabler och kan därför inte
läsas från servern. Du kan ta bort variablerna från servern genom att
ge kommandot \"Spara inställningar\".\n\n")

    (unknown-aux-item . "Okänd tilläggsinformation")
    (text-header-aux-item . "%#1s: <%#2d> \"%#3s\" %#4s")

    (aux-content-type-name . "Innehållstyp")
    (aux-fast-reply-name . "Anmärkning")
    (aux-cross-reference-name . "Referens")
    (aux-no-comments-name . "Begäran om inga kommentarer")
    (aux-personal-comment-name . "Begäran om personliga svar")
    (aux-request-confirmation-name . "Begäran om läsbekräftelse")
    (aux-read-confirm-name . "Läsbekräftelse")
    (aux-redirect-name . "Omdirigering")
    (aux-x-face-name . "Bild")
    (aux-alternate-name-name . "Alternativt namn")
    (aux-pgp-signature-name . "PGP-signatur")
    (aux-pgp-public-key-name . "Publik PGP-nyckel")
    (aux-e-mail-address-name . "E-postadress")
    (aux-faq-text-name . "FAQ i text")
    (aux-creating-software-name . "Skapande klient")
    (aux-mx-author-name . "E-mailförfattare")
    (aux-mx-from-name . "E-mail från")
    (aux-mx-reply-to-name . "E-mail svar sänds till")
    (aux-mx-to-name . "E-mail till")
    (aux-mx-cc-name . "E-mail CC")
    (aux-mx-date-name . "E-maildatum")
    (aux-mx-message-id-name . "E-mail-meddelande-ID")
    (aux-mx-in-reply-to-name . "E-mail svar till")
    (aux-mx-misc-name . "E-mailheader")
    (aux-mx-allow-filter-name . "E-mail allow-filter")
    (aux-mx-reject-forward-name . "E-mail reject-forward")
    (aux-notify-comments-name . "Meddela kommentarer")
    (aux-faq-for-conf-name . "FAQ för möte")
    (aux-recommended-conf-name . "Mötesrekommendation")
    (aux-allowed-content-type-name . "Tillåten innehållstyp")
    (aux-canonical-name-name . "Kanoniskt namn")
    (aux-mx-list-name-name . "E-maillistnamn")
    (aux-send-comments-to-name . "Sänd kommentarer till")
    (aux-world-readable-name . "Läsbart utan inloggning")
    (aux-mx-refuse-import . "Vägra inläggsimport")
    (aux-elisp-client-read-faq-name . "Läst FAQ")
    (aux-elisp-client-rejected-invitation-name . "Avvisad inbjudan")

    (review-marking-as-read . "Återse läsmarkerar")
    (review-not-marking-as-read . "Återse läsmarkerar inte")

    (using-ssh-connection . "Använder ssh-uppkoppling till %#1s...")
    (opening-ssh-connection . "Öppnar ssh-uppkoppling till %#1s...")
    (ssh-cant-connect . "Kan inte göra ssh-uppkopplingen: %s")
    (ssh-closing . "Stänger ssh-uppkopplingen till %#1s")
    (ssh-unknown-host . "okänd värd")

    (keyboard-cancel . "Avbryt")
    (keyboard-menu-help . "(välj: C-n, C-p; bekräfta: RET)")
    (customize-help . "Se början av bufferten för mer information")

    (no-mule-warning . "\

%#1@Du har stödet för multibytetecken (MULE) avslaget i emacs. Detta beror
antagligen på att du har ett anrop till standard-display-european
eller motsvarande i din .emacs, eller i någon av systemets startfiler.

Elispklienten är gjord för att köras med stödet för multibytetecken
påslaget. Utan detta stöd kan inlägg visas fel, och du kan få problem
med att skapa inlägg. Du bör därför slå på stödet för multibytetecken
i emacs.

")
    (coding-system-mismatch-warning . "\

%#3@Din emacs är inställd på att i första hand koda tecken enligt
\"%#1s\", men den LysKOM-server du kör mot rekommenderar
\"%#2s\". Det innebär att kommentarer du skriver till
andras inlägg, och kommentarer till dina inlägg kan få en blandning av
tecken som inte går att visa ordentligt. Du bör byta språkomgivning i
emacs till en som använder \"%#2s\" som förstahandsval för
teckenkodning.

")

    (has-nameday-1 . "%#1s har namnsdag i dag%#2?b%[ (%#2s)%]%[%].")
    (has-nameday-2 . "%#1s och %#2s har namnsdag i dag%#3?b%[ (%#3s)%]%[%].")

    (invalid-date-entry . "Ogiltigt datum")
    (number-out-of-range . "Otillåtet tal")
    (or-date . "eller datum")

    (set-unread-n . "Endast läsa %#1?d%[det senaste inlägget%]%[de senaste %#1d inläggen%].\n")
    (set-unread-date . "Endast läsa inlägg sedan %#3d %#2s %#1d.\n")

    (jump-from-text . "Hoppa över kommentarer till vilken text? ")
    (jumping-from-text . "Hoppar över kommentarer till inlägg %#1n.\n")
    (jumping-from-text-review . "Hoppar över återsedda kommentarer.\n")
    
    (session-to-copy-options-from . "Kopiera inställningar från session: ")
    (reading-settings-from . "Läser inställningar från %s...")
    (reading-settings-from-done . "Läser inställningar från %s...klart")

    (super-jump-q . "Superhoppa vilket inlägg? ")
    (conf-list-legend . "* Ej medlem; - Passiv medlem\n")

    (unread-text-q . "Gör vilket inlägg oläst? ")
    (marking-text-unread . "Markerar inlägg %#1n oläst...")
    (cant-mark-text-unread . "Kunde inte markera inlägg %#1n som oläst (%#2s)\n")
    (confusion-what-to-mark-unread . "Jag förstår inte vilken text du vill markera oläst.\n")
    (no-comment-to-to-unread . "Det finns inget kommenterat inlägg att markera som oläst.\n")

    (mx-refuse-import-spam . "Spam")
    (mx-refuse-import-all . "Alla texter")
    (abc-spam . "sSpam")
    (abc-everything . "aAlla texter")
    (limit-import-to-conf . "Begränsa import till vilket möte? ")
    (limit-import-of-what . "Begränsa import av vad? ")
    (limit-import-not-super . "Du verkar inte vara organisatör för mötet. Försök ändå? ")
    (limiting-import . "Begränsar import av %#1s i %#2M...")
    ))

(lyskom-language-var local lyskom-month-names sv
  '(("januari" . 1)   ("jan" . 1)
    ("februari" . 2)  ("feb" . 2)
    ("mars" . 3)      ("mar" . 3)
    ("april" . 4)     ("apr" . 4)
    ("maj" . 5)       ("maj" . 5)
    ("juni" . 6)      ("jun" . 6)
    ("juli" . 7)      ("jul" . 7)
    ("augusti" . 8)   ("aug" . 8)
    ("september" . 9) ("sep" . 9)
    ("oktober" . 10)  ("okt" . 10)
    ("november" . 11) ("nov" . 11)
    ("december" . 12) ("dec" . 12)))


;;; ================================================================
;;;              The commands and their associated functions



;;; The Alist formerly known as lyskom-commands
(lyskom-language-strings global lyskom-command sv
  '(
    (kom-help                 . "Hjälp")
    (kom-slow-mode            . "Långsamma kommandon")
    (kom-quick-mode           . "Snabba kommandon")
    (kom-send-message         . "Sända meddelande")
    (kom-send-alarm           . "Skicka alarmmeddelande")
    (kom-create-conf	      . "Skapa möte")
    (kom-delete-conf          . "Utplåna (möte/person)")
    (kom-delete-text          . "Radera inlägg")
    (kom-display-time	      . "Se tiden")
    (kom-go-to-conf	      . "Gå till möte")
    (kom-go-to-next-conf      . "Gå till nästa möte")
    (kom-jump		      . "Hoppa över alla kommentarer")
    (kom-list-created-conferences . "Lista ägda möten")
    (kom-list-conferences     . "Lista möten") 
    (kom-list-persons	      . "Lista personer")
    (kom-list-news            . "Lista nyheter")
    (kom-list-re              . "Lista (med) regexpar")
    (kom-membership	      . "Lista medlemskap")
    (kom-postpone	      . "Uppskjuta läsning")
    (kom-set-session-priority . "Sätt läsnivå")
    (kom-prioritize	      . "Prioritera möten")
    (kom-status-person	      . "Status (för) person")
    (kom-status-conf	      . "Status (för) möte")
    (kom-add-self	      . "Bli medlem i möte")
    (kom-change-priority      . "Ändra prioritet")
    (kom-list-summary	      . "Lista ärenden")
    (kom-sub-self             . "Utträda ur möte")
    (kom-quit		      . "Sluta")
    (kom-recover	      . "Starta om klienten") 
    (kom-start-anew	      . "Börja med nytt namn")
    (kom-view		      . "Återse inlägg")
    (kom-find-root-review     . "Återse träd")
    (kom-review-comments      . "Återse alla kommentarer")
    (kom-review-tree	      . "Återse alla kommentarer rekursivt")
    (kom-review-clear         . "Återse hoppa")
    (kom-review-last-normally-read
     			      . "Återse igen")
    (kom-review-converted     . "Återse konverterat")
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
    (kom-who-is-on	      . "Vilka (är) inloggade") 
    (kom-who-is-on-in-conference
     			      . "Vilka (är inloggade i) möte") 
    (kom-who-is-present-in-conference
     			      . "Vilka (är) närvarande (i möte)")
    (kom-who-is-on-and-friend . "Vilka vänner (är inloggade)")
    (kom-who-am-i             . "Var (är) jag")
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
    (kom-add-bcc	      . "Addera dold kopia")
    (kom-sub-recipient        . "Subtrahera mottagare")
    (kom-move-text            . "Flytta inlägg")
    (kom-add-comment	      . "Addera kommentar")
    (kom-sub-comment	      . "Subtrahera kommentar")
    (kom-add-cross-reference  . "Addera referens")
    (kom-add-member	      . "Addera medlem")
    (kom-sub-member           . "Uteslut medlem")
    (kom-change-conf-motd     . "Sätt lapp på dörren")
    (kom-set-garb-nice        . "Ändra livslängd")
    (kom-set-super-conf       . "Ändra supermöte")
    (kom-set-permitted-submitters . "Ändra tillåtna författare")
    (kom-unset-conf-motd      . "Ta bort lapp på dörren")
    (kom-save-text	      . "Arkivera inlägg (till fil)")
    (kom-save-text-body	      . "Spara inläggstext (på fil)")
    (kom-save-options         . "Spara inställningar")
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
    (kom-filter-recipient     . "Filtrera mottagare")
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
    (kom-fast-reply           . "Anmärkning (till inlägg)")
    (kom-agree                . "Hålla med")
    (kom-add-faq              . "Addera FAQ")
    (kom-del-faq              . "Ta bort FAQ")
    (kom-review-faq           . "Återse FAQ")

    (kom-add-footnote	      . "Addera fotnot")
    (kom-sub-footnote	      . "Subtrahera fotnot")

    (kom-add-no-comments      . "Förhindra kommentarer")
    (kom-add-private-answer   . "Begär personligt svar")
    (kom-add-request-confirm  . "Begär läsbekräftelse")

    (kom-review-mail-headers  . "Återse brevhuvud")
    
    (kom-compare-texts        . "Jämför två texter")
    (kom-diff-texts           . "Se diff")

    (kom-become-anonymous     . "Bli anonym")
    (kom-become-nonanonymous  . "Bli icke-anonym (träd fram ur skuggorna)")

    (kom-keep-alive           . "Håll förbindelsen aktiv")
    (kom-stop-keep-alive      . "Avbryt håll förbindelsen aktiv")

    (kom-is-person-member-of-conference . "Kontrollera (om person är) medlem (i möte)")
    (kom-will-person-read-text . "Kontrollera (om person kommer att) läsa (inlägg)")

    (kom-change-conf-faq      . "Ändra FAQ")

    (kom-make-review-mark-as-read . "Återse läsmarkerar")
    (kom-make-review-not-mark-as-read . "Återse läsmarkerar inte")

    (kom-set-presentation     . "Addera presentation")
    (kom-remove-presentation  . "Ta bort presentation")
    (kom-set-motd-text        . "Addera lapp på dörren")
    (kom-create-aux-item      . "Skapa tilläggsinformation")
    (kom-status-server        . "Status (för) servern")
    (kom-add-server-faq       . "Addera server-FAQ")
    (kom-del-server-faq       . "Ta bort server-FAQ")
    (kom-review-server-faq    . "Återse server-FAQ")
    (kom-change-server-faq    . "Ändra server-FAQ")
    (kom-recommend-conference . "Rekommendera möte")
    (kom-redirect-comments    . "Dirigera om kommentarer")
    (kom-move-text-tree       . "Flytta träd")

    (kom-copy-options         . "Kopiera inställningar")
    (kom-mark-unread          . "Oläsmarkera inlägg")
    (kom-unread-by-to         . "Oläsmarkera senaste")
    (kom-unread-more          . "Oläsmarkera fler inlägg")
    (kom-unread-commented-text . "Oläsmarkera det kommenterade")
    (kom-unread-previous-commented-text . "Oläsmarkera det föregående kommenterade")
    (kom-unread-comments      . "Oläsmarkera alla kommentarer")
    (kom-unread-tree          . "Oläsmarkera alla kommentarer rekursivt")
    (kom-unread-root          . "Oläsmarkera urinlägget")
    (kom-unread-root-review   . "Oläsmarkera träd")
    (kom-unread-last-normally-read
     			      . "Oläsmarkera igen")
    (kom-unread-all           . "Oläsmarkera alla")
    (kom-unread-first         . "Oläsmarkera första")
    (kom-unread-presentation  . "Oläsmarkera presentation")
    (kom-unread-server-faq    . "Oläsmarkera server-FAQ")
    (kom-unread-faq           . "Oläsmarkera FAQ")
    (kom-unread-marked-texts  . "Oläsmarkera markerade")
    (kom-unread-all-marked-texts . "Oläsmarkera alla markerade")

    (kom-join-all-conferences . "Bli medlem i alla möten")
    (kom-leave-all-conferences . "Utträd ur (nästan) alla möten")
    (kom-limit-import         . "Begränsa import av texter")
    ))

(lyskom-language-var local lyskom-language-codes sv
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


(lyskom-language-strings global lyskom-menu sv
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
    (receivers . "Mottagare")
    (commented . "Kommenterar")
    (kom-edit-send . "Skicka in")
    (kom-edit-send-anonymous . "Skicka anonymt")
    (kom-edit-quit . "Kasta bort") 
    (kom-ispell-message . "Stavningskontroll")
    (kom-edit-add-recipient . "Addera mottagare")
    (kom-edit-add-copy . "Addera extra kopiemottagare")
    (kom-edit-show-commented . "Återse det kommenterade")
    (kom-edit-insert-commented . "Citera det kommenterade")
    (kom-edit-insert-buglist . "Klistra in kommenterad buglista")
    (kom-edit-add-bcc . "Addera dold kopia")
    (kom-edit-add-cross-reference . "Addera korsreferens")
    (kom-edit-add-no-comments . "Begär inga kommentarer")
    (kom-edit-add-personal-comments . "Begär personliga svar")
    (kom-edit-add-read-confirm-request . "Begär läsbekräftelse")
    (kom-edit-move-text . "Flytta till en ny mottagare")
))

(lyskom-language-var local lyskom-move-tree-actions sv
                     '(("Flytta eller subtrahera mottagare" . move)
                       ("Lämna kvar och fortsätt med kommentarerna" . skip)
                       ("Avsluta helt" . quit)
                       ("Hoppa över inlägget och dess kommentarsträd" . jump)))

(lyskom-language-var local lyskom-onoff-table sv
  '(("på" . on) ("av" . off)))

(lyskom-language-var local lyskom-filter-predicate-list sv
      '(("=" . nil) ("!=" . t)))

(lyskom-language-var local lyskom-filter-what sv
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

(lyskom-language-var local lyskom-filter-actions sv
      '((skip-text . "Hoppa över")
        (dontshow . "Visa inte")
        (read . "Undanta från filtrering")
        (skip-tree . "Hoppa över kommentarer")
	(obliterate . "Utplåna")))

(defconst lyskom-keybindings-missing nil)

(defvar lyskom-sv-mode-map nil)
(lyskom-language-keymap lyskom-mode-map sv lyskom-sv-mode-map)

(defvar lyskom-sv-review-prefix)
(defvar lyskom-sv-change-prefix)
(defvar lyskom-sv-next-prefix)
(defvar lyskom-sv-list-prefix)
(defvar lyskom-sv-S-prefix)
(defvar lyskom-sv-fast-reply-prefix)
(defvar lyskom-sv-filter-get-prefix)
(defvar lyskom-sv-who-prefix)
(defvar lyskom-sv-A-prefix)
(defvar lyskom-sv-unread-prefix)

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
  (define-prefix-command 'lyskom-sv-who-prefix)
  (define-prefix-command 'lyskom-sv-A-prefix)
  (define-prefix-command 'lyskom-sv-unread-prefix)

  (define-key lyskom-sv-mode-map (kbd "f") 'lyskom-sv-filter-get-prefix)
  (define-key lyskom-sv-mode-map (kbd "n") 'lyskom-sv-next-prefix)
  (define-key lyskom-sv-mode-map (kbd "l") 'lyskom-sv-list-prefix)
  (define-key lyskom-sv-mode-map (kbd "s") 'lyskom-sv-S-prefix)
  (define-key lyskom-sv-mode-map (kbd "r") 'lyskom-sv-fast-reply-prefix)
  (define-key lyskom-sv-mode-map (kbd "v") 'lyskom-sv-who-prefix)
  (define-key lyskom-sv-mode-map (kbd "a") 'lyskom-sv-A-prefix)
  (define-key lyskom-sv-mode-map (kbd "o") 'lyskom-sv-unread-prefix)

  (lyskom-try-define-key lyskom-sv-mode-map (kbd "ä")    'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map (kbd "{")    'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map (kbd "[")    'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [adiaeresis] 'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [Adiaeresis] 'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [ä]          'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [Ä]          'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [228]        'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [196]        'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [2276]       'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [2244]       'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [3908]       'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [3940]       'lyskom-sv-change-prefix)

  (lyskom-try-define-key lyskom-sv-mode-map (kbd "å") 'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map (kbd "}") 'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map (kbd "]") 'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [aring]     'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [Aring]     'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [å]       'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [Å]       'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [229]     'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [197]     'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [2277]    'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [2245]    'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [3909]    'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [3941]    'lyskom-sv-review-prefix)

  (define-key lyskom-sv-mode-map (kbd "M-m") 'kom-toggle-mark-as-read-prefix)

  (define-key lyskom-sv-mode-map (kbd (lyskom-keys 'button2)) 'kom-button-click)
  (define-key lyskom-sv-mode-map (kbd (lyskom-keys 'button3))   'kom-popup-menu)
  (define-key lyskom-sv-mode-map (kbd (lyskom-keys 'button2up)) 'kom-mouse-null)
  (define-key lyskom-sv-mode-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lyskom-sv-mode-map (kbd "*")     'kom-button-press)
  (define-key lyskom-sv-mode-map (kbd "=")     'kom-menu-button-press)
  (define-key lyskom-sv-mode-map (kbd "TAB")   'kom-next-link)
  (define-key lyskom-sv-mode-map (kbd "M-TAB") 'kom-previous-link)
  (define-key lyskom-sv-mode-map (kbd "<S-tab>") 'kom-previous-link)
  (define-key lyskom-sv-mode-map (kbd "C-i")   'kom-next-link)
  (define-key lyskom-sv-mode-map (kbd "M-C-i") 'kom-previous-link)

  ;; These should be first in order to be last in the menu of alternatives.
  (define-key lyskom-sv-change-prefix     (kbd "?") 'lyskom-help)
  (define-key lyskom-sv-review-prefix     (kbd "?") 'lyskom-help)
  (define-key lyskom-sv-unread-prefix     (kbd "?") 'lyskom-help)
  (define-key lyskom-sv-filter-get-prefix (kbd "?") 'lyskom-help)
  (define-key lyskom-sv-next-prefix       (kbd "?") 'lyskom-help)
  (define-key lyskom-sv-list-prefix       (kbd "?") 'lyskom-help)
  (define-key lyskom-sv-S-prefix          (kbd "?") 'lyskom-help)
  (define-key lyskom-sv-who-prefix        (kbd "?") 'lyskom-help)
  (define-key lyskom-sv-fast-reply-prefix (kbd "?") 'lyskom-help)
  (define-key lyskom-sv-A-prefix          (kbd "?") 'lyskom-help)
  

  (define-key lyskom-sv-mode-map (kbd "e")   'kom-set-unread)
;;  (define-key lyskom-sv-mode-map (kbd "a")   'kom-obsolete-extended-command-binding)
;;  (define-key lyskom-sv-mode-map (kbd "l v") 'kom-obsolete-who-is-on-in-conference)
  (define-key lyskom-sv-mode-map (kbd "x")   'kom-extended-command)
  (define-key lyskom-sv-mode-map (kbd "<SPC>") 'kom-next-command)
  (define-key lyskom-sv-mode-map (kbd "<RET>") 'kom-line-next-command)
  (define-key lyskom-sv-mode-map (kbd "<LFD>") 'kom-page-next-command)
  (define-key lyskom-sv-mode-map (kbd "C-j") 'kom-page-next-command)
  (define-key lyskom-sv-mode-map (kbd "j")   'kom-page-next-command)
  (define-key lyskom-sv-mode-map (kbd "C-m") 'kom-line-next-command)

  (define-key lyskom-sv-mode-map (kbd "?")   'kom-help)
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
  (define-key lyskom-sv-mode-map (kbd "m")   'kom-add-self)
  (define-key lyskom-sv-mode-map (kbd "M")   'kom-mark-text)
  (define-key lyskom-sv-mode-map (kbd "A")   'kom-unmark-text)
  (define-key lyskom-sv-mode-map (kbd "S")   'kom-quit)
  (define-key lyskom-sv-mode-map (kbd "q")   'kom-quit)
  (define-key lyskom-sv-mode-map (kbd "z")   'kom-bury)
  (define-key lyskom-sv-mode-map (kbd "R")   'kom-recover)
  (define-key lyskom-sv-mode-map (kbd "t")   'kom-display-time)
  (define-key lyskom-sv-mode-map (kbd "J")   'kom-who-am-i)
  (define-key lyskom-sv-mode-map (kbd "V")   'kom-busy-wait)

  (define-key lyskom-sv-next-prefix (kbd "i") 'kom-view-next-new-text)
  (define-key lyskom-sv-next-prefix (kbd "m") 'kom-go-to-next-conf)
  (define-key lyskom-sv-next-prefix (kbd "l") 'kom-next-kom)
  (define-key lyskom-sv-next-prefix (kbd "o") 'kom-next-unread-kom)

  (define-key lyskom-sv-list-prefix (kbd "M") 'kom-list-marks)
  (define-key lyskom-sv-list-prefix (kbd "m") 'kom-list-conferences)
  (define-key lyskom-sv-list-prefix (kbd "n") 'kom-list-news)
  (define-key lyskom-sv-list-prefix (kbd "p") 'kom-list-persons)
  (define-key lyskom-sv-list-prefix (kbd "r") 'kom-list-re)
  (define-key lyskom-sv-list-prefix (kbd "s") 'kom-membership)
  (define-key lyskom-sv-list-prefix (kbd "f") 'kom-list-filters)
  (lyskom-try-define-key lyskom-sv-list-prefix (kbd "ä") 'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix (kbd "{") 'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix (kbd "[") 'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [adiaeresis]     'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [Adiaeresis]     'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [ä]       'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [Ä]       'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [228]     'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [196]     'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [2276]    'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [2244]    'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [3908]    'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [3940]    'kom-list-summary)

  (define-key lyskom-sv-filter-get-prefix (kbd "u") 'kom-get-appreciation)
  (define-key lyskom-sv-filter-get-prefix (kbd "s") 'kom-get-abuse)
  (define-key lyskom-sv-filter-get-prefix (kbd "t") 'kom-move-text)
  (define-key lyskom-sv-filter-get-prefix (kbd "T") 'kom-move-text-tree)
  (define-key lyskom-sv-filter-get-prefix (kbd "f") 'kom-filter-author)
  (define-key lyskom-sv-filter-get-prefix (kbd "i") 'kom-filter-text)
  (define-key lyskom-sv-filter-get-prefix (kbd "m") 'kom-filter-recipient)
  (define-key lyskom-sv-filter-get-prefix (kbd "l") 'kom-previous-kom)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix (kbd "ä")    'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix (kbd "{")    'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix (kbd "[")    'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [adiaeresis] 'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [Adiaeresis] 'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [ä]          'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [Ä]          'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [228]        'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [196]        'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [2276]       'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [2244]       'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [3908]       'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [3940]       'kom-filter-subject)

  (define-key lyskom-sv-who-prefix (kbd "i") 'kom-who-is-on)
  (define-key lyskom-sv-who-prefix (kbd "m") 'kom-who-is-on-in-conference)
  (define-key lyskom-sv-who-prefix (kbd "n") 'kom-who-is-present-in-conference)
  (define-key lyskom-sv-who-prefix (kbd "v") 'kom-who-is-on-and-friend)

  (define-key lyskom-sv-change-prefix (kbd "p") 'kom-change-presentation)
  (define-key lyskom-sv-change-prefix (kbd "q") 'kom-change-conf-faq)
  (define-key lyskom-sv-change-prefix (kbd "f") 'kom-filter-edit)
  (define-key lyskom-sv-change-prefix (kbd "m") 'kom-change-auto-reply)

  (define-key lyskom-sv-unread-prefix (kbd "SPC") 'kom-mark-unread)
  (define-key lyskom-sv-unread-prefix (kbd "0") 'kom-initial-digit-unread)
  (define-key lyskom-sv-unread-prefix (kbd "1") 'kom-initial-digit-unread)
  (define-key lyskom-sv-unread-prefix (kbd "2") 'kom-initial-digit-unread)
  (define-key lyskom-sv-unread-prefix (kbd "3") 'kom-initial-digit-unread)
  (define-key lyskom-sv-unread-prefix (kbd "4") 'kom-initial-digit-unread)
  (define-key lyskom-sv-unread-prefix (kbd "5") 'kom-initial-digit-unread)
  (define-key lyskom-sv-unread-prefix (kbd "6") 'kom-initial-digit-unread)
  (define-key lyskom-sv-unread-prefix (kbd "7") 'kom-initial-digit-unread)
  (define-key lyskom-sv-unread-prefix (kbd "8") 'kom-initial-digit-unread)
  (define-key lyskom-sv-unread-prefix (kbd "9") 'kom-initial-digit-unread)
  (define-key lyskom-sv-unread-prefix (kbd "k") 'kom-unread-commented-text)
  (define-key lyskom-sv-unread-prefix (kbd "K") 'kom-unread-previous-commented-text)
  (define-key lyskom-sv-unread-prefix (kbd "a ?") 'lyskom-help)
  (define-key lyskom-sv-unread-prefix (kbd "a k") 'kom-unread-comments)
  (define-key lyskom-sv-unread-prefix (kbd "a r") 'kom-unread-tree)
  (define-key lyskom-sv-unread-prefix (kbd "i") 'kom-unread-last-normally-read)
  (define-key lyskom-sv-unread-prefix (kbd "r") 'kom-unread-root)
  (define-key lyskom-sv-unread-prefix (kbd "u") 'kom-unread-root)
  (define-key lyskom-sv-unread-prefix (kbd "s") 'kom-unread-by-to)
  (define-key lyskom-sv-unread-prefix (kbd "y") 'kom-unread-more)
  (define-key lyskom-sv-unread-prefix (kbd "t") 'kom-unread-root-review)
  (define-key lyskom-sv-unread-prefix (kbd "A") 'kom-unread-all)
  (define-key lyskom-sv-unread-prefix (kbd "f") 'kom-unread-first)
  (define-key lyskom-sv-unread-prefix (kbd "p") 'kom-unread-presentation)
  (define-key lyskom-sv-unread-prefix (kbd "q") 'kom-unread-faq)
  (define-key lyskom-sv-unread-prefix (kbd "Q") 'kom-unread-server-faq)
  (define-key lyskom-sv-unread-prefix (kbd "m") 'kom-unread-marked-texts)
  (define-key lyskom-sv-unread-prefix (kbd "a m") 'kom-unread-all-marked-texts)
  (define-key lyskom-sv-unread-prefix (kbd "a SPC") 'kom-unread-all)

  (define-key lyskom-sv-review-prefix (kbd "SPC") 'kom-view)
  (define-key lyskom-sv-review-prefix (kbd "0") 'kom-initial-digit-view)
  (define-key lyskom-sv-review-prefix (kbd "1") 'kom-initial-digit-view)
  (define-key lyskom-sv-review-prefix (kbd "2") 'kom-initial-digit-view)
  (define-key lyskom-sv-review-prefix (kbd "3") 'kom-initial-digit-view)
  (define-key lyskom-sv-review-prefix (kbd "4") 'kom-initial-digit-view)
  (define-key lyskom-sv-review-prefix (kbd "5") 'kom-initial-digit-view)
  (define-key lyskom-sv-review-prefix (kbd "6") 'kom-initial-digit-view)
  (define-key lyskom-sv-review-prefix (kbd "7") 'kom-initial-digit-view)
  (define-key lyskom-sv-review-prefix (kbd "8") 'kom-initial-digit-view)
  (define-key lyskom-sv-review-prefix (kbd "9") 'kom-initial-digit-view)
  (define-key lyskom-sv-review-prefix (kbd "k") 'kom-view-commented-text)
  (define-key lyskom-sv-review-prefix (kbd "K") 'kom-view-previous-commented-text)
  (define-key lyskom-sv-review-prefix (kbd "a ?") 'lyskom-help)
  (define-key lyskom-sv-review-prefix (kbd "a k") 'kom-review-comments)
  (define-key lyskom-sv-review-prefix (kbd "a r") 'kom-review-tree)
  (define-key lyskom-sv-review-prefix (kbd "h") 'kom-review-clear)
  (define-key lyskom-sv-review-prefix (kbd "i") 'kom-review-last-normally-read)
  (define-key lyskom-sv-review-prefix (kbd "n") 'kom-review-next)
  (define-key lyskom-sv-review-prefix (kbd "o") 'kom-review-noconversion)
  (define-key lyskom-sv-review-prefix (kbd "v") 'kom-review-converted)
  (define-key lyskom-sv-review-prefix (kbd "r") 'kom-find-root)
  (define-key lyskom-sv-review-prefix (kbd "u") 'kom-find-root)
  (define-key lyskom-sv-review-prefix (kbd "s") 'kom-review-by-to)
  (define-key lyskom-sv-review-prefix (kbd "y") 'kom-review-more)
  (define-key lyskom-sv-review-prefix (kbd "A") 'kom-review-all)
  (define-key lyskom-sv-review-prefix (kbd "f") 'kom-review-first)
  (define-key lyskom-sv-review-prefix (kbd "l") 'kom-review-stack)
  (define-key lyskom-sv-review-prefix (kbd "p") 'kom-review-presentation)
  (define-key lyskom-sv-review-prefix (kbd "t") 'kom-find-root-review)
  (define-key lyskom-sv-review-prefix (kbd "m") 'kom-review-marked-texts)
  (define-key lyskom-sv-review-prefix (kbd "q") 'kom-review-faq)
  (define-key lyskom-sv-review-prefix (kbd "Q") 'kom-review-server-faq)
  (define-key lyskom-sv-review-prefix (kbd "a m") 'kom-review-all-marked-texts)
  (define-key lyskom-sv-review-prefix (kbd "a SPC") 'kom-review-all)
  (define-key lyskom-sv-review-prefix (kbd "b") 'kom-review-mail-headers)
  (define-key lyskom-sv-review-prefix (kbd "B")   'kom-review-backward)

  (define-key lyskom-sv-S-prefix (kbd "m") 'kom-status-conf)
  (define-key lyskom-sv-S-prefix (kbd "p") 'kom-status-person)
  (define-key lyskom-sv-S-prefix (kbd "s") 'kom-status-session)
  (define-key lyskom-sv-S-prefix (kbd "k") 'kom-status-server)
  (define-key lyskom-sv-S-prefix (kbd "u ?") 'lyskom-help)
  (define-key lyskom-sv-S-prefix (kbd "u m") 'kom-sub-recipient)
  (define-key lyskom-sv-S-prefix (kbd "u k") 'kom-sub-comment)
  (define-key lyskom-sv-S-prefix (kbd "u q") 'kom-del-faq)
  (define-key lyskom-sv-S-prefix (kbd "u f") 'kom-sub-footnote)
  (define-key lyskom-sv-S-prefix (kbd "t") 'kom-save-text)
  (lyskom-try-define-key lyskom-sv-S-prefix (kbd "ä")    'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix (kbd "{")    'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix (kbd "[")    'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [adiaeresis] 'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [Adiaeresis] 'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [ä]          'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [Ä]          'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [228]        'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [196]        'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [2276]       'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [2244]       'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [3908]       'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [3940]       'kom-send-message)

  (define-key lyskom-sv-A-prefix (kbd "m") 'kom-add-recipient)
  (define-key lyskom-sv-A-prefix (kbd "e") 'kom-add-copy)
  (define-key lyskom-sv-A-prefix (kbd "d") 'kom-add-bcc)
  (define-key lyskom-sv-A-prefix (kbd "k") 'kom-add-comment)
  (define-key lyskom-sv-A-prefix (kbd "r") 'kom-add-cross-reference)
  (define-key lyskom-sv-A-prefix (kbd "q") 'kom-add-faq)
  (define-key lyskom-sv-A-prefix (kbd "f") 'kom-add-footnote)
  (define-key lyskom-sv-A-prefix (kbd "Q") 'kom-add-server-faq)

  (define-key lyskom-sv-fast-reply-prefix (kbd "a") 'kom-fast-reply)
  (define-key lyskom-sv-fast-reply-prefix (kbd "r") 'kom-fast-reply)
  (define-key lyskom-sv-fast-reply-prefix (kbd "h") 'kom-agree)

  ;; Running in buffer

  (define-key lyskom-sv-mode-map (kbd "M-p") 'backward-text)
  (define-key lyskom-sv-mode-map (kbd "M-n") 'forward-text)
  (define-key lyskom-sv-mode-map (kbd "C-M-p") 'kom-prev-prompt)
  (define-key lyskom-sv-mode-map (kbd "C-M-n") 'kom-next-prompt)

  (define-key lyskom-sv-mode-map (kbd "C-?") 'scroll-down)
  (define-key lyskom-sv-mode-map (kbd "<DEL>") 'scroll-down)
  (define-key lyskom-sv-mode-map (kbd "<delete>") 'scroll-down)
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
  (define-prefix-command 'lyskom-sv-filter-edit-prefix)
  (define-key lyskom-sv-filter-edit-map (kbd "C-c") 'lyskom-sv-filter-edit-prefix)
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
  (define-key lyskom-sv-filter-edit-map (kbd "C-c C-c")   'lyskom-filter-edit-quit)
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
  (define-key lyskom-sv-prioritize-mode-map (kbd (lyskom-keys 'button2)) 'kom-button-click)
  (define-key lyskom-sv-prioritize-mode-map (kbd (lyskom-keys 'button3)) 'kom-popup-menu)
  (define-key lyskom-sv-prioritize-mode-map (kbd (lyskom-keys 'button2up)) 'kom-mouse-null)
  (define-key lyskom-sv-prioritize-mode-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lyskom-sv-prioritize-mode-map (kbd "*") 'kom-button-press)
  (define-key lyskom-sv-prioritize-mode-map (kbd "=") 'kom-menu-button-press)
  (define-key lyskom-sv-prioritize-mode-map (kbd "?") 'kom-prioritize-help)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-k") 'kom-prioritize-select)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-y") 'kom-prioritize-yank)
  (define-key lyskom-sv-prioritize-mode-map (kbd "SPC") 'kom-prioritize-select)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-m") 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "RET") 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-j") 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "<down>") 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-n") 'kom-prioritize-next-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "DEL") 'kom-prioritize-previous-line)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-DEL") 'kom-prioritize-deselect-all)
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
  (define-key lyskom-sv-prioritize-mode-map (kbd "<S-tab>") 'kom-previous-link)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-C-i") 'kom-previous-link)
)


(lyskom-language-var local lyskom-prioritize-header-lines sv 2)

(lyskom-language-var local lyskom-prioritize-header sv
" Prio   Möte
-------------------------------------------------------------------------------
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
  (define-key lyskom-sv-customize-map (kbd "<S-tab>") 'widget-backward)
  (define-key lyskom-sv-customize-map (kbd "C-i") 'widget-forward)
  (define-key lyskom-sv-customize-map (kbd "M-C-i") 'widget-backward)
  (define-key lyskom-sv-customize-map (kbd "C-m") 'widget-button-press)
  (define-key lyskom-sv-customize-map (kbd "C-j") 'widget-button-press)
  (define-key lyskom-sv-customize-map (kbd "<RET>") 'widget-button-press)
  (define-key lyskom-sv-customize-map (kbd "<LFD>") 'widget-button-press)
  (define-key lyskom-sv-customize-map (kbd (lyskom-keys 'button2)) 'widget-button-click)
  (define-key lyskom-sv-customize-map (kbd (lyskom-keys 'button3)) 'lyskom-widget-click)
  (define-key lyskom-sv-customize-map (kbd (lyskom-keys 'button2up)) 'kom-mouse-null)
  (define-key lyskom-sv-customize-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lyskom-sv-customize-map (kbd "C-c C-c") 'lyskom-customize-save-and-quit)
  (define-key lyskom-sv-customize-map (kbd "C-c C-k") 'lyskom-customize-quit)
  (define-key lyskom-sv-customize-map (kbd "C-c C-s") 'lyskom-customize-save)
  (define-key lyskom-sv-customize-map (kbd "C-c C-a") 'lyskom-customize-apply)
  (define-key lyskom-sv-customize-map (kbd "?") 'lyskom-customize-help)
)

(lyskom-language-strings local lyskom-custom-strings sv
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
    (ding . "Ljudsignal")

    ;;
    ;; Help messages
    ;;

    (variable-type-help . "Kryssa i för att spara inställningen i servern.")
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
    (to-file . "Till fil:")
    (max-text-length . "För inlägg kortare än: ")
    
    (friends . "Vänner och bekanta")

    (turned-off      . "Avslaget            ")
    (number-of-times . "Några gånger")
    (specific-spec . "Per mottagare/avsändare")
    (sound-file . "Ljudfil")
    (other-persons . "Alla andra")
    (selected-mark . "Markering")
    (conf-or-person . "Person eller möte")
    (ask . "Fråga varje gång")
    (before . "Före texten")
    (after . "Efter texten")
    (first . "Först")
    (last . "Sist ")
    (depth-first . "I kommentarsordning")
    (time-order . "I tidsordning")
    (limited-number . "Ett begränsat antal")
    (unlimited-number . "Obegränsat")

    (ar-message-type . "Meddelandetyp")
    (ar-personal     . "Personligt meddelande")
    (ar-group        . "Gruppmeddelande")
    (ar-alarm        . "Alarmmeddelande")
    (ar-any-type     . "Alla meddelandetyper")
    (ar-sender       . "Avsändare")
    (ar-any-sender   . "Alla avsändare")
    (ar-specified-sender . "Specifika avsändare")
    (ar-pers-or-conf . "Person eller möte")
    (ar-recipient    . "Mottagare")
    (ar-any-recipient . "Alla mottagare")
    (ar-specified-recipient . "Specifika mottagare")
    (ar-person       . "Person")
    (ar-message-text . "Meddelandetext")
    (ar-any-message  . "Alla meddelanden")
    (ar-matching-regexp . "Meddelanden som matchar")
    (ar-reply        . "Automatsvar")
    (ar-no-reply     . "Inget svar")
    (ar-reply-text   . "Svarstext")

    (express-break . "Omedelbart efter de har skapats")
    (break         . "Efter aktuell kommentarskedja")
    (no-break      . "Efter aktuellt möte")

    (express-server-break . "Omedelbart")
    (express-server-letters-break . "Omedelbart om det kommer brev")
    (server-break . "Efter aktuell kommentarskedja")
    (letters-server-break . "Efter aktuell kommentarskedja om det kommer brev")
    (after-conf-server-break . "Efter aktuellt möte")
    (after-conf-letters-server-break . "Efter aktuellt möte när det kommer brev")
    (when-done-server-break . "När allt är utläst")
    (no-server-break . "Aldrig")

    (command . "Kommando")
    (command-list . "Kommandolista")
    (some-persons . "För vissa personer")
    (name . "Namn")

    (page-none . "Aldrig                 ")
    (page-all  . "Före varje kommando    ")
    (page-some . "Före följande kommandon")

    (ask-every-time . "Fråga varje gång      ")
    (fixed-priority . "Fast prioritet")

    (messages-in-lyskom-buffer . "I LysKOM-bufferten  ")
    (discard-messages .          "Ingenstans          ")
    (in-named-buffer .           "I namngiven buffert:")

    (everybody-rcpt .       "Alla                              ")
    (group-rcpt .           "Senaste gruppmeddelandes mottagare")
    (sender-rcpt .          "Senaste meddelandes avsändare     ")
    (last-recipient-recpt . "Senaste mottagaren                ")

    (viewer-program . "WWW-läsare")
    (no-viewer            . "(ingenting valt)")
    (default-viewer       . "Browse-URL (alla)")
    (netscape-viewer      . "Netscape/Mozilla (alla)")
    (windows-viewer       . "Windows standard eller Netscape/Mozilla (alla)")
    (emacs-w3-viewer      . "Emacs W3-mode (HTTP, Goper, FTP)")
    (emacs-general-viewer . "Emacs (FTP, Telnet, Mail)")
    (emacs-dired-viewer   . "Emacs Dired (FTP)")
    (emacs-mail-viewer    . "Emacs Mail-mode (Mail)")
    (emacs-telnet-viewer  . "Emacs Telnet-mode (telnet)")
    (mosaic-viewer        . "NCSA Mosaic (alla)")
    (lynx-viewer          . "Lynx (alla)")
    (galeon-viewer        . "Galeon (alla)")

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

    (first . "Först")
    (last . "Sist")
    (specific-placement . "Specifik plats")
    (priority . "Prioritet")
    (same-as-conf . "Samma som aktuellt möte")
    (custom-format . "Eget format:")
    (default-format . "Standardformat")
    (a-string . "En text:")
    (some-string . "En slumpmässig text")
    (unspecified . "Ospecificerat")

    (symbolic-mark-association . "Associering")
    (symbolic-mark-name . "Symboliskt namn: ")
    (mark-type-to-assoc . "Markeringstyp att associera till: ")

    (prompt-for-text-no . "Fråga efter textnummer")

    (all-others . "Alla andra")
    (some-person . "Person")
    (format-html . "Formattera HTML:")

    (url-transform-regexp . "Från (regexp):")
    (url-transform-newtext . " Till:")

    (default-namedays . "Namnsdagar för aktuellt språk")
    (specific-namedays . "Specifik namnlängd")
    (several-name-lists . "Flera namnlängder")

    (link-pattern-for . "Mönster för")
    (all-conferences . "Alla mottagare")
    (link-specific-rcpt . "Specifik mottagare")
    (link-pattern . "Länkmönster")
    (link-replace . "Länksubstitution")
    (link-highlight-match . "Visa grupp")
    (link-fold-case . "Versaler och gemener lika")

    (generated-face . "Automatgenererat teckensnitt")
    (predefined-fonts . "Teckensnitt avsedda för specifika tillämpningar\n")
    (other-fonts . "Teckensnitt utan specifika tillämpningar\n")
    (other-face . "Annat")

    (ext-simple-command . "Enkelt kommando")
    (ext-complex-command . "Kommando med argument")
    (ext-complex-component . "Komanndo/argument")

    (lynx-xterm . "Lynx i xterm")
    (lynx-emacs . "Lynx i emacs terminal-mode")

    (morons . "Idioter och knäppskallar")
    (friends . "Vänner och bekanta")
    (me . "Mig själv och ingen annan")
    (i-am-supervisor . "Möten jag organiserar")
    (has-no-presentation . "Möten utan presentationer")
    (highlight-conferences . "Möten:")
    (highlight-face . "Utseende:")
    (highlight-conflist . "Möteslista")

    (yes-dont-pop . "Ja, men avikonifiera inte fönstret")

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

Hjälptexter: [?] Visa hjälptext    [!] Göm hjälptext
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

    (new-window-netscape . "Öppna URLer i nytt fönster")
    (new-tab-netscape . "Öppna URLer i ny tab")
    (default-netscape . "Öppna URLer där de råkar hamna")
    
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
        man bekräfta alla mottagare i klump). Det går också att slå av
        helt.

    Kontroll av kommenterad författares medlemskap
        Kontrollera att författaren till det kommenterade inlägget är
        medlem i någon av mottagarna för kommentaren. Om så inte är fallet,
        erbjuder LysKOM att lägga till författaren som mottagare till
        inlägget. 

    Författare som inte kontrolleras
        En lista av författare som inte skall kontrolleras även om
        kontroll av kommenterad författares medlemskap är påslaget.

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
        fönster så kommer ett nytt att skapas (och tas bort när man är klar).

    I en annan frame
        I en annan frame än LysKOM. Om det bara finns en frame så kommer en
        ny frame att skapas, och (tas bort när man är klar).

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

    (kom-ansaphone-replies-doc . "\
  Den här inställningen styr automatsvar i detalj. Det går att ställa in 
  svar för specifika avsändare, mottagare och till och med meddelanden. Om
  meddelandet som skall besvaras inte stämmer in på något i den här listan
  så används svarsmeddelandet ovan istället.

  För varje mönster går det att ställa in meddelandetyp, avsändare som 
  meddelandet skall ha, mottagare som meddelandet skall ha, ett mönster
  som meddelandet skall innehålla och vilket svar som skall skickas.

  Det är lämpligt att ställa in att inget svar skall skickas på något
  gruppmeddelande eller alarmmeddelande.")

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
  Bestämmer i vilket fönster man listar medlemskap.")


    (kom-user-prompt-format-doc . "\
  Format för LysKOM-prompten. Vissa teckenkombinationer sätter in speciella
  texter:

    %c - Sätter in kommandot som körs om man trycker på SPC eller RET.
    %[ - Sätter in '[' om automatsvar är påslaget.
    %] - Sätter in ']' om automatsvar är påslaget.     
    %m - Sätter in information om antal inspelade meddelanden.
    %s - Sätter in LysKOM-systemets namn
    %S - Sätter in LysKOM-serverns namn
    %p - Sätter in namnet på den person som är inloggad.
    %w - Sätter in namn på aktuellt möte.
    %a - Sätter in \"anonym\"
    %A - Sätter in \"Anonym\"
    %# - Sätter in sessionsnummer.
    %  - Sätter in mellanslag om det ser ut att behövas.
    %% - Sätter in ett procenttecken.

  Några exempel:

    \"%[%c% %m%] - \"             Standardprompt
    \"%[%s: %c% %m%] - \"         Till exempel \"LysKOM: Se tiden - \"")

    (kom-user-prompt-format-executing-doc . "\
  Format för LysKOM-prompten efter man trycker på SPC eller RET.  Vissa
  teckenkombinationer sätter in speciella texter:

    %c - Sätter in kommandot som körs om man trycker på SPC eller RET.
    %[ - Sätter in '[' om automatsvar är påslaget.
    %] - Sätter in ']' om automatsvar är påslaget.     
    %m - Sätter in information om antal inspelade meddelanden.
    %s - Sätter in LysKOM-systemets namn
    %S - Sätter in LysKOM-serverns namn
    %p - Sätter in namnet på den person som är inloggad.
    %w - Sätter in namn på aktuellt möte.
    %a - Sätter in \"anonym\"
    %A - Sätter in \"Anonym\"
    %# - Sätter in sessionsnummer.
    %  - Sätter in mellanslag om det ser ut att behövas.
    %% - Sätter in ett procenttecken.

  Några exempel:

    \"%[%c% %m%].\"             Standardprompt
    \"%[%s: Kör %c% %m%]...\"   Till exempel \"LysKOM: Kör Se tiden...\"")

    
    (kom-anonymous-prompt-format-doc . "\
  Format för LysKOM-prompten när man är anonym. Vissa teckenkombinationer 
  sätter in speciella texter:

    %c - Sätter in kommandot som körs om man trycker på SPC eller RET.
    %[ - Sätter in '[' om automatsvar är påslaget.
    %] - Sätter in ']' om automatsvar är påslaget.     
    %m - Sätter in information om antal inspelade meddelanden.
    %s - Sätter in LysKOM-systemets namn
    %S - Sätter in LysKOM-serverns namn
    %p - Sätter in namnet på den person som är inloggad.
    %w - Sätter in namn på aktuellt möte.
    %a - Sätter in \"anonym\"
    %A - Sätter in \"Anonym\"
    %# - Sätter in sessionsnummer.
    %  - Sätter in mellanslag om det ser ut att behövas.
    %% - Sätter in ett procenttecken.

  Några exempel:

    \"%[%c% %m%] - \"             Standardprompt
    \"%[%s: %c% %m%] - \"         Till exempel \"LysKOM: Se tiden - \"")

    (kom-anonymous-prompt-format-executing-doc . "\
  Format för LysKOM-prompten efter man trycker på SPC eller RET och är
  anonym.  Vissa teckenkombinationer sätter in speciella texter:

    %c - Sätter in kommandot som körs om man trycker på SPC eller RET.
    %[ - Sätter in '[' om automatsvar är påslaget.
    %] - Sätter in ']' om automatsvar är påslaget.     
    %m - Sätter in information om antal inspelade meddelanden.
    %s - Sätter in LysKOM-systemets namn
    %S - Sätter in LysKOM-serverns namn
    %p - Sätter in namnet på den person som är inloggad.
    %w - Sätter in namn på aktuellt möte.
    %a - Sätter in \"anonym\"
    %A - Sätter in \"Anonym\"
    %# - Sätter in sessionsnummer.
    %  - Sätter in mellanslag om det ser ut att behövas.
    %% - Sätter in ett procenttecken.

  Några exempel:

    \"%[%c% %m%].\"             Standardprompt
    \"%[%s: Kör %c% %m%]...\"   Till exempel \"LysKOM: Kör Se tiden...\"")

    (kom-enabled-prompt-format-doc . "\
  Format för LysKOM-prompten när man är i administratörsmod. Vissa
  teckenkombinationer sätter in speciella texter:

    %c - Sätter in kommandot som körs om man trycker på SPC eller RET.
    %[ - Sätter in '[' om automatsvar är påslaget.
    %] - Sätter in ']' om automatsvar är påslaget.     
    %m - Sätter in information om antal inspelade meddelanden.
    %s - Sätter in LysKOM-systemets namn
    %S - Sätter in LysKOM-serverns namn
    %p - Sätter in namnet på den person som är inloggad.
    %w - Sätter in namn på aktuellt möte.
    %a - Sätter in \"anonym\"
    %A - Sätter in \"Anonym\"
    %# - Sätter in sessionsnummer.
    %  - Sätter in mellanslag om det ser ut att behövas.
    %% - Sätter in ett procenttecken.

  Några exempel:

    \"%[%c% %m%] - \"             Standardprompt
    \"%[%s: %c% %m%] - \"         Till exempel \"LysKOM: Se tiden - \"")

    (kom-enabled-prompt-format-executing-doc . "\
  Format för LysKOM-prompten efter man trycker på SPC eller RET och är i
  administratörsmod.  Vissa teckenkombinationer sätter in speciella texter:

    %c - Sätter in kommandot som körs om man trycker på SPC eller RET.
    %[ - Sätter in '[' om automatsvar är påslaget.
    %] - Sätter in ']' om automatsvar är påslaget.     
    %m - Sätter in information om antal inspelade meddelanden.
    %s - Sätter in LysKOM-systemets namn
    %S - Sätter in LysKOM-serverns namn
    %p - Sätter in namnet på den person som är inloggad.
    %w - Sätter in namn på aktuellt möte.
    %a - Sätter in \"anonym\"
    %A - Sätter in \"Anonym\"
    %# - Sätter in sessionsnummer.
    %  - Sätter in mellanslag om det ser ut att behövas.
    %% - Sätter in ett procenttecken.

  Några exempel:

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
  Den markeringstyp som används för nya markeringar. En markeringstyp är ett
  heltal mellan 0 och 255 (inklusive). Om ingen defaultmarkeringstyp är vald
  frågar LysKOM efter markeringstyp varje gång man markerar en text. Detta
  kan användas för att hålla isär olika sorters markeringar. Till exempel
  kan man markera inlägg som innehåller intressant information med ett visst
  tal och inlägg som man ska komma ihåg att kommentera vid ett senare
  tillfälle med ett annat tal.")

    (kom-symbolic-marks-alist-doc . "\
  För att slippa behöva memorera vad man tänkte sig att de heltaliga
  markeringstyperna ska innebära kan man definiera en lista med symboliska
  markeringstyper. En symbolisk markeringstyp är en beskrivande sträng som
  associeras med en heltalig markeringstyp.")

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


    (kom-presence-messages-in-echo-area-doc . "\

  Om man vill ha meddelanden i ekoarean när någon loggar in, ut eller
  byter namn i LysKOM bör man slå på den här inställningen. Meddelanden
  visas i ekoarean. Om man vill ha information om endast vissa personer
  går det att ange en lista av personer. Vill man inte ha några
  meddelanden alls är det bara att ange en tom lista.")

    (kom-presence-messages-in-buffer-doc . "\
  Om man vill ha meddelanden i LysKOM-bufferten när någon loggar in, ut eller
  byter namn i LysKOM bör man slå på den här inställningen.  Meddelanden
  visas i LysKOM-bufferten. Om man vill ha information om endast vissa 
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
  I vilkalistan visas normalt enbart de sessioner som har varit aktiva
  under den senaste tiden. Denna inställning bestämmer hur många minuter
  en session får ha varit inaktiv och ändå visas i vilkalistan.")


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
  i stället 1002, 1003, 1006, 1004, 1005 och 1007.")


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
  exempel Status (för) session och Sända meddelande). Om det är avslaget
  kommer TAB att fylla ut även med namn på personer som inte är inloggade.")


    (kom-membership-default-priority-doc . "\
  Detta bestämmer hur prioriteten på möten man går med i sätts. Om det är
  en siffra mellan 1 och 255 så kommer nya medlemskap att få den prioriteten.
  I annat fall kommer klienten att fråga för varje nytt medlemskap vilken
  prioritet det skall ha.")


    (kom-show-personal-messages-in-buffer-doc . "\
  Denna inställning bestämmer var personliga meddelanden, gruppmeddelanden
  och alarmmeddelanden visas. Meddelanden kan antingen visas i 
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
  Denna inställning bestämmer vem som kommer att vara defaultmottagare
  för personliga meddelanden man skickar. Alternativen som finns är
  att meddelanden normalt är allmänna, att avsändaren för det senast
  mottagna meddelandet skall vara default eller att mottagaren för det
  senaste gruppmeddelandet (eller avsändaren av det senaste personliga
  eller allmänna meddelandet) skall vara mottagare.")


    (kom-filter-outgoing-messages-doc . "\
  Om denna inställning är påslagen så kommer meddelanden som skickas 
  automatiskt, till exempel automatiska svar och svar på fjärrkontrollförsök
  även att visas som om man hade skickat det manuellt.")

    (kom-friends-doc . "\
  Namnen på personerna i denna lista kommer att visas med ett speciellt 
  utseende i LysKOM-bufferten. Kommandot Vilka vänner är inloggade (och
  andra liknande kommandon) använder också denna lista.")

    (kom-morons-doc . "\
  Namnen på personerna i denna lista kommer att visas med ett varnande 
  utseende i LysKOM-bufferten.")

    (kom-url-viewer-preferences-doc . "\
  Denna inställning bestämmer vilken WWW-läsare som i första hand skall 
  användas för att öppna URLer som förekommer i LysKOM. Om den första läsaren 
  i listan inte klarar den typ av URL som man försöker öppna så används nästa
  läsare och så vidare.")

    (kom-windows-browser-command-doc . "\
  Denna inställning talar om vilket kommando som skall användas för att starta
  en WWW-läsare i Windows. Om fältet är tomt testas ett antal kommandon som
  typiskt brukar fungera i Windows.")

    (kom-mosaic-command-doc . "\
  Denna inställning talar om vilket kommando som skall användas för att starta
  Mosaic.")

    (kom-netscape-command-doc . "\
  Denna inställning anger kommandot för att köra Netscape eller Mozilla.")

    (kom-galeon-command-doc . "\
  Denna inställning anger kommandot för att köra Galeon.")

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
  experter).")

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
  Påslaget innebär att dagens namn visas när man ser tiden. Det går att
  välja att se namnsdagarna från namnsdagslängden som hör till det språk
  man för närvarande använder eller att använda en specifik lista med
  namn.")

    (kom-show-week-number-doc . "\
  Se tiden visar veckonummer om detta är påslaget. Annars visas inte 
  veckonummer.")

    (kom-membership-default-placement-doc . "\
  Denna inställning styr var nya medlemskap hamnar. Först betyder att nya
  medlemskap hamnar före gamla med samma prioritet. Sist betyder att nya 
  medlemskap hamnar efter gamla med samma prioritet. En siffra är en fix
  placering i medlemskapslistan (men medlemskapet kommer att flyttas
  såsmåningom).")

    (kom-show-imported-importer-doc . "\
  Påslaget betyder visa vilken person som importerade ett importerat e-mail.")

    (kom-show-imported-envelope-sender-doc . "\
  Visa den verkliga avsändaren till ett importerat e-mail om detta är 
  påslaget.")

    (kom-show-imported-external-recipients-doc . "\
  Visa externa mottagare och extra-kopiemottagare om detta är påslaget.")

    (kom-agree-text-doc . "\
  Text som används av kommandot Hålla med. Det kan antingen vara en text
  eller en lista av texter. Om det är en lista så väljs ett alternativ
  slumpmässigt.")

    (kom-silent-ansaphone-doc . "\
  On detta är avslaget så piper inte klienten när det kommer meddelanden och
  automatsvar är påslaget.")

    (kom-default-session-priority-doc . "\
  Läsnivå för nya sessioner. Detta är den läsnivå som nya sessioner får
  automatiskt.")

    (kom-unsubscribe-makes-passive-doc . "\
  Om detta är påslaget så gör kommandot Utträda ur möte att man först blir
  passiv medlem i mötet. Om man utträder ur mötet igen så går man ur på
  riktigt. Om detta är avslaget så går man ur mötet första gången man försöker
  utträda.")

    (kom-review-priority-doc . "\
  Prioritet för återsekommandon. Detta är den prioritet som inläggen man ser
  när man gör återse får. Det används för att avgöra om nya inlägg skall
  avbryta återsefunktionen. Sätt till 256 eller högre om du inte vill att nya
  inlägg någonsin skall avbryta en återsefunktion.")

    (kom-show-creating-software-doc . "\
  Om detta är påslaget så visas vilken klient som skapade inlägget tillsammans
  med varje inlägg, förutsatt att informationen finns överhuvudtaget.")

    (kom-text-footer-format-doc . "\
  Format för inläggsfoten. I texten ersätts %n med inläggsnumret, %P med
  författarens namn, %p med författarens nummer, %f med information om
  inlägget (ombruten, HTML mm.) och %- med en lagom lång streckad linje.
  En siffra efter procenttecknet är minsta bredd på texten. Ett minustecken
  före siffran betyder att texten skall vänsterjusteras. Ett likhetstecken
  betyder att fältlängden är exakt och texten kanske kortas av.")

    (kom-long-lines-doc . "\
  Om detta är påslaget så görs de flesta streckade linjer i klienten mycket
  längre än normalt.")

    (kom-postpone-default-doc . "\
  Antalet inlägg som inte skall uppskjutas med Uppskjuta läsning.")

    (kom-allow-incompleteness-doc . "\
  Påslaget betyder att klienten inte väntar på att information om alla
  olästa har kommit fram innan den tillåter kommandon som Lista nyheter.
  Avslaget innebär att klienten väntar på information om olästa inlägg om
  den upptäcker att informationen behövs och inte har kommit fram.")

    (kom-smileys-doc . "\
  Gladmän, som :-), visas grafiskt när denna inställning är på, förutsatt
  att Emacs klarar det och paketet smiley.el (en del av Gnus) är installerat.")

    (kom-ignore-message-senders-doc . "\
  Visa inte personliga, grupp- och alarmmeddelanden från dessa personer.")

    (kom-ignore-message-recipients-doc . "\
  Visa inte gruppmeddelanden riktade till dessa möten.")

    (kom-text-footer-dash-length-doc . "\
  Den totala längden på inläggsfoten när streckade linjer, men inte extra
  långa linjer, och inte ett eget format används.")

    (kom-text-header-dash-length-doc . "\
  Den totala längden på den streckade linjen ovanför inläggstexten när
  extra långa linjer inte används.")

    (kom-show-personal-message-date-doc . "\
  När detta är påslaget så visas datum och klockslag för alla personliga,
  grupp- och alarmmeddelanden.")

    (kom-w3-simplify-body-doc . "\
  När detta är påslaget så visas HTML utan de färger som anges i HTML-koden.")

    (kom-mercial-doc . "\
  Denna text visas i vilkalistan då du har läst färdigt alla möten.")

    (kom-server-priority-doc . "\
  Prioritet för sessionen. Sessioner med hög prioritet kan avbryta andra
  sessioner, beroende på inställningen av YYY.")

    (kom-server-priority-breaks-doc . "\
  Denna inställning styr hur prioriterade sessioner avbryter andra
  sessioner. En session är högre prioriterad än en annan om dess
  sessionsprioritet är högre än den andra sessionens.

  Omedelbart            Så fort der kommer ett inlägg i en prioriterad 
                        session så får man prompt om att gå till den
                        sessionen.

  Omedelbart om det kommer brev     Så fort det kommer ett personligt
                        brev i en prioriterad session så får man prompt 
                        om att gå till den sessionen.

  Efter aktuell kommentarskedja     Om det kommer inlägg i en 
                        prioriterad session så får man prompt om att
                        gå till sessionen när man har läst klart den
                        aktuella kommentarskedjan.

  Efter aktuell kommentarskedja om det kommer brev      Om det kommer
                        personliga brev i en prioriterad session så 
                        får man prompt om att gå till sessionen när
                        man har läst klart den aktuella kommentarskedjan.

  Efter aktuellt möte   Om det kommer inlägg i en prioriterad session 
                        så får man prompt om att gå till sessionen när
                        man har läst klart det aktuella mötet.

  Efter aktuellt möte när det kommer brev       Om det kommer brev i en
                        prioriterad session så får man prompt om att gå
                        till sessionen när man har läst klart det 
                        aktuella mötet.

  När allt är utläst    Ge prompt om att byta till nästa LysKOM med 
                        olästa inlägg när allt är klart, oberoende av
                        sessionernas prioritet.

  Aldrig                Ge aldrig prompt om att byta session.")

    (kom-ding-on-no-subject-doc . "")
    (kom-ding-on-personal-messages-doc . "")
    (kom-ding-on-group-messages-doc . "")
    (kom-ding-on-common-messages-doc . "")
    (kom-ding-on-no-subject-doc . "")
    (kom-ding-on-wait-done-doc . "")
    (kom-ding-on-priority-break-doc . "")
    (kom-ding-on-new-letter-doc . "")

    (kom-check-for-new-comments-doc . "")
    (kom-check-commented-author-membership . "")
    (kom-confirm-multiple-recipients-doc . "")
    (kom-check-commented-author-membership-doc . "")
    (kom-complete-numbers-before-names-doc . "\
  När detta är påslaget och man matar in en text som kan tolkas både som
  ett mötesnamn eller ett mötesnummer vid inläsning av mötesnamn, så
  tolkar klienten det i första hand som ett nummer. När detta är avslaget
  så tolkas det i första hand som ett mötesnamn.")
    (kom-keep-alive-interval-doc . "\
  Antalet sekunder som skall gå mellan de meddelanden som används för att
  hålla nätverkskopplingen till LysKOM-servern aktiv. Om din förbindelse
  kopplar ned efter en stunds inaktivitet så kan du sätta detta till ungefär 
  hälften och sedan ge kommandot \"Håll förbindelsen aktiv\".")
    (kom-text-no-prompts-doc . "\
  Den här inställningen styr vilka kommandon som alltid frågar efter 
  textnummer (om man inte anger ett explicit som prefixargument). För 
  kommandon som inte står med här används inbyggda standardinställningar.")
    (kom-saved-file-name-doc . "\
  Anger vilken fil som inlägg skall arkiveras till. Klienten kommer ändå
  att fråga, men detta blir det förifyllda svaret.")
    (kom-follow-attachments-doc . "\
  Anger om bilagor till importerade brev skall läsas som kommentarer. 
  Om detta är avstängt så läsmarkeras bilagor automatiskt, utan att
  man läser dem.")
    (kom-show-unread-in-frame-title-doc . "\
  Om detta är påslaget så visas \"(Olästa)\" i titelraden för de
  fönster vars valda buffert hör till en LysKOM-session med olästa. 
  Det är inte säkert att titelraden ändras om fönstret inte är öppet.")
    (kom-created-texts-are-saved-doc . "\
  Om detta är ett filnamn så sparas alla inlägg man skriver till den
  filen i ungefär samma format som de har när man läser dem. Inlägg
  som inte kan skapas sparas inte.")
    (kom-confirm-add-recipients-doc . "\
  Om detta är påslaget och man adderar en mottagare till ett inlägg så
  frågar klienten om man vill att kommentarer till inlägget också
  skall skickas till den nya mottagaren. Om detta är avslaget så
  kommer kommentarer alltid att skickas även till den nya mottagaren. 
  Det är oftast bra att ha påslaget.")
    (kom-trim-buffer-minimum-doc . "\
  Om du begränsar LysKOM-buffertens storlek så måste den bli så här mycket
  för stor innan text i bufferten raderas. Variablen ska vara ett heltal,
  default är 4096.")
    (kom-dont-check-commented-authors-doc . "")
    (kom-print-relative-dates-doc . "\
  Tidsangivelsen för inlägg (och en del andra saker) kommer att bli
  \"idag\" alternativt \"igår\" om denna inställning är påslagen och
  inlägget skrevs idag alternativt igår. Om den är avslagen kommer ett
  absolut datum alltid att visas.")
    (kom-print-seconds-in-time-strings-doc . "\
  Om denna inställning är påslagen kommer en del tidsangivelser (till
  exempel för visade inlägg och sända meddelanden) att inkludera
  sekunder.")
    (kom-review-uses-cache-doc . "\
  Om detta är påslaget så kommer kommandon som återser inlägg att 
  använda sparade kopior av inläggen om de finns. Det gör att kommandona
  går lite snabbare, men det finns en viss risk för att ett inlägg kan
  ha ändrats sedan kopian sparades.")
    (kom-review-marks-texts-as-read-doc . "\
  Om detta är påslaget så kommer kommandon som återser inlägg att 
  läsmarkera de inlägg som visas. I annat fall lämnar återsekommandona
  din läshistorik orörd, precis som vanligt.")
    (kom-auto-review-faqs-doc . "\
  Om detta är påslaget så kommer olästa FAQer inte att återses automatiskt
  när du loggar in eller går till ett möte med en oläst FAQ.")
    (kom-auto-list-faqs-doc . "\
  Om detta är påslaget så kommer olästa FAQer att listas automatiskt när
  du loggar in eller går till ett möte med en oläst FAQ.")
    (kom-extended-status-information-doc . "\
  Om detta är påslaget så visas extra statusinformation i kommandona
  Status (för) person, Status (för) möte och Status (för) servern 
  (om sådan information finns tillgänglig).")
    (kom-highlight-first-line-doc . "\
  Om detta är påslaget så visas första raden i varje inlägg (den med
  inläggsnummer, datum och författare) med ett annorlunda utseende än
  normal text.")
    (kom-highlight-dashed-lines-doc . "\
  Om detta är påslaget så visas raderna före och efter inläggstexten med
  ett annorlunda utseende än normal text.")
    (kom-highlight-text-body-doc . "\
  Om detta är påslaget så visas inläggstexter med ett annorlunda utseende
  än normal text.")
    (kom-async-highlight-dashed-lines-doc . "\
  Om detta är påslaget så visas raderna före och efter personliga, grupp-
  och alarmmeddelanden med ett annorlunda utseende än normal text.")
    (kom-async-highlight-text-body-doc . "\
  Om detta är påslaget så visas personliga, grupp- och alarmmeddelanden 
  med ett annorlunda utseende än normal text.")
    (kom-edit-hide-add-button-doc . "\
  Om detta är påslaget så kommer knappen [Addera...] att visas efter
  mottagare och tilläggsinformation när man skriver inlägg.")
    (kom-format-html-authors-doc . "\
  Den här inställningen styr vilka författares HTML-meddelanden som
  elispklienten kommer att försöka formattera.")
    (kom-keyboard-menu-immediate-selection-doc . "\
  Den här inställningen styr huruvida snabbtangenter automatiskt bekräftar
  val i textbaserade menyer eller inte. Om detta är påslaget så måste man
  alltid trycka retur för att välja i textbaserade menyer.")
    (kom-max-overlays-doc . "\
  Med denna inställning kan man begränsa antalet färgmarkeringar (som
  används bland annat för färgade inläggsbakgrunder). Genom att begränsa
  antalet markeringar kan scrollning gå mycket fortare. Varje inlägg
  behöver mellan 1 och 4 markeringar beroende på andra inställningar.")
    (kom-mark-read-texts-as-read-in-new-recipient-doc . "\
  Om detta är påslaget så läsmarkeras redan lästa inlägg automatiskt i
  möten de adderas till medan man är inloggad. Om man inte är inloggad
  så sker inte denna läsmarkering. Inlägg som adderas till ens brevlåda
  läsmarkeras dock aldrig automatiskt.")
    (kom-url-transformation-rules-doc . "\
  Regler för att transformera URLer innan de skickas till en webbläsare.
  \"Från\" är ett mönster (en regexp) som skall översättas till texten
  i \"Till\". Dokumentation om regexpar finns i Emacs-manualen. \"Till\" 
  kan innehålla följande specialtecken:

      \\& ersätts med den text som matchar regexpen
      \\N ersätts med den text som matchar \(...\)-uttryck N i regexpen
      \\\\ ersätts med en \\.")
    (kom-text-links-doc . "\
  Definition av textlänkar. Varje rad definierar en länk för inlägg i ett
  visst möte. Alla förekomster av länkmönstret blir URL-länkar genom att
  texten som länkmönstret matchar substitueras enligt länksubstitutionen.
  Den text som faktist visas som länk i LysKOM-bufferten bestäms av 
  inställningen \"Visa grupp\". Använd 0 för att visa hela den matchande
  texten eller en siffta från 1 till 9 för att visa den N-te gruppen i
  länkmönstret (använd 0 om du inte förstår vad som avses med detta). 
  Om inställningen \"Versaler och gemener lika\" är på så räknas versaker
  och gemener av samma bokstav som lika.

  Länksubstitutionen kan innehålla följande specialtecken:

      \\& ersätts med den text som matchar regexpen
      \\N ersätts med den text som matchar \(...\)-uttryck N i regexpen
      \\\\ ersätts med en \\.")

    (kom-active-face-doc . "\
  Teckensnitt för klickbara texter förutom URLer, möten, vänner och
  bekanta, idioter och knäppskallar, och inläggsnummer. Värdet skall 
  vara namnet på en så kallad `face'.")
    (kom-active-highlight-face-doc . "\ 
  Teckensnitt avsett för klickbara texter som skall markeras extra
  tydligt. Värdet skall vara namnet på en så kallad `face'.")    
    (kom-active-strikethrough-face-doc . "\ 
  Teckensnitt avsett för klickbara texter som skall markeras extra
  tydligt. Värdet skall vara namnet på en så kallad `face'.")
    (kom-url-face-doc . "\
  Teckensnitt för klickbara URLer.")
    (kom-me-face-doc . "\
  Teckensnitt för ens eget namn.")
    (kom-highlight-face-doc . "\
  Teckensnitt för markering av klickbara ytor när muspekaren förs över
  dem.")
    (kom-subject-face-doc . "\
  Teckensnitt för ärenderader.")
    (kom-text-no-face-doc . "\
  Teckensnitt för inläggsnummer.")
    (kom-friends-face-doc . "\
  Teckensnitt för vänner och bekanta. De personer som räknas upp i
  listan \"vänner och bekanta\" kommer att markeras med detta
  teckensnitt.")
    (kom-morons-face-doc . "\
  Teckensnitt för idioter och knäppskallar. De personer som räknas upp
  i listan \"Idioter och knäppskallar\" kommer att markeras med detta
  teckensnitt.")
    (kom-presence-face-doc . "\
  Teckensnitt för närvaromeddelanden.")
    (kom-first-line-face-doc . "\
  Teckensnitt för första raden i inläggshuvudet. Detta teckensnitt
  överlagras på standardteckensnitten i första raden, så de egenskaper
  som inte sätts av detta teckensnitt kommer inte att påverkas.")
    (kom-warning-face-doc . "\
  Teckensnitt för viktiga varningar.")
    (kom-mark-face-doc . "\
  Teckensnitt för diverse markeringar. Bland annat används detta
  teckensnitt för markeringen när elispklienten bläddrar mindre än en
  hel skärmsida. Detta teckensnitt överlagras på standardteckensnitten
  där det används, så de egenskaper som inte sätts av detta
  teckensnitt kommer inte att påverkas.")
    (kom-dim-face-doc . "\
  Teckensnitt för diverse diskreta texter. Detta teckensnitt används
  bland annat för att visa passiva medlemskap.")
    (kom-text-body-face-doc . "\
  Teckensnitt för inläggstexter. Detta teckensnitt överlagras på
  standardteckensnitten i inläggstexten, så de egenskaper som inte
  sätts av detta teckensnitt kommer inte att påverkas.")
    (kom-dashed-lines-face-doc . "\
  Teckensnitt för streckrader runt inlägg. Detta teckensnitt
  överlagras på standardteckensnitten i streckraden, så de egenskaper
  som inte sätts av detta teckensnitt kommer inte att påverkas.")
    (kom-async-text-body-face-doc . "\
  Teckensnitt för meddelanden. Detta teckensnitt överlagras på
  standardteckensnitten i meddelandet, så de egenskaper som inte
  sätts av detta teckensnitt kommer inte att påverkas.")
    (kom-async-dashed-lines-face-doc . "\
  Teckensnitt för streckrader runt meddelanden. Detta teckensnitt
  överlagras på standardteckensnitten i streckraderna, så de
  egenskaper som inte sätts av detta teckensnitt kommer inte att
  påverkas.") 
    (kom-lynx-terminal-doc . "\
  Lynx kan köras inuti Emacs eller i en xterm (eller motsvarande). Här
  väljer du vilket.")
    (kom-lynx-terminal-command-doc . "\
  Kommandot som används för att starta Lynx i Emacs terminal-mode. 
  Ska normalt vara så enkelt som \"lynx\".")
    (kom-lynx-xterm-command-doc . "\
  Kommandot som används för att starta Lynx i en xterm. Kommandot ska
  starta både xterm och lynx.")
    (kom-show-sync-messages-doc . "\
  Om denna inställning är påslagen så kommer klienten att visa ett
  meddelande i ekoarean nät servern signallerar att den håller på
  spara databasen.")
    (kom-highlight-conferences-doc . "\
  Denna inställning styr hur mötesnamn färgläggs. För varje val
  kan ett utseende anges. Utseendena är namn på faces i Emacs.")
    (kom-netscape-variant-doc . "\
  Denna inställning styr var elispklienten försöker öppna URLer. Hur
  bra de olika valen fungerar beror på vilken variant av Netscape eller
  Mozilla man använder, och till och med vilket operativsystem man
  använder.")

    ;;
    ;; Tags for variables
    ;;

    (kom-ansaphone-replies-tag . "Detaljerade automatsvar")
    (kom-bury-buffers-tag . "Begrav buffertar när man byter LysKOM:")

    (kom-personal-messages-in-window-tag . "Personliga meddelanden:   ")
    (kom-customize-in-window-tag       . "Inställningar för LysKOM: ")
    (kom-write-texts-in-window-tag     . "Skriv inlägg:             ")
    (kom-prioritize-in-window-tag      . "Prioritera möten:         ")
    (kom-edit-filters-in-window-tag    . "Ändra filter:             ")
    (kom-view-commented-in-window-tag  . "Återse kommentarer:       ")
    (kom-list-membership-in-window-tag . "Lista medlemskap:         ")

    (kom-user-prompt-format-tag . "Promptformat:")
    (kom-user-prompt-format-executing-tag . "Promptformat vid körning:")
    (kom-anonymous-prompt-format-tag . "Promptformat (anonym):")
    (kom-anonymous-prompt-format-executing-tag . "Promptformat vid körning (anonym):")
    (kom-enabled-prompt-format-tag . "Promptformat (admin):")
    (kom-enabled-prompt-format-executing-tag . "Promptformat vid körning (admin):")

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

    (kom-presence-messages-in-echo-area-tag . 
"Närvaromeddelanden i ekoarean:        ")
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
    (kom-ding-on-new-letter-tag        . "När det kommer brev:                ")
    (kom-ding-on-priority-break-tag    . "När det kommer prioriterade inlägg: ")
    (kom-ding-on-wait-done-tag         . "När man har väntat klart:           ")
    (kom-ding-on-common-messages-tag   . "Vid alarmmeddelanden:               ")
    (kom-ding-on-group-messages-tag    . "Vid gruppmeddelanden:               ")
    (kom-ding-on-personal-messages-tag . "Vid personliga meddelanden:         ")
    (kom-ding-on-no-subject-tag        . "När ärenderad saknas:               ")

    (kom-filter-outgoing-messages-tag . "Visa automatiska meddelanden:")
    (kom-friends-tag . "Vänner och bekanta:")
    (kom-morons-tag . "Idioter och knäppskallar:")
    (kom-url-viewer-preferences-tag . "Öppna URLer med följande program:")
    (kom-windows-browser-command-tag . "Kommando för att starta en WWW-läsare i Windows:")
    (kom-mosaic-command-tag . "Kommando för att starta NCSA Mosaic:")
    (kom-netscape-command-tag . "Kommando för att starta Netscape eller Mozilla:")
    (kom-galeon-command-tag . "Kommando för att starta Galeon:")

    (kom-symbolic-marks-alist-tag . "Symboliska markeringstyper:")

    (kom-cite-string-tag . "Citatmarkering:")
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
    (kom-default-language-tag . "Språk:")
    (kom-ispell-dictionary-tag . "Ordlista:")
    (kom-show-namedays-tag .    "Visa dagens namn:")

    (kom-show-week-number-tag . "Visa veckonummer:")
    (kom-membership-default-placement-tag . "Placering av nya medlemskap:")
    (kom-show-imported-importer-tag . "Visa importör av importerade inlägg:")
    (kom-show-imported-envelope-sender-tag . "Visa avsändare av importerade inlägg:")
    (kom-show-imported-external-recipients-tag . "Visa externa mottagare till importerade inlägg:")
    (kom-agree-text-tag . "Text för Hålla med:")
    (kom-silent-ansaphone-tag . "Pip när automatsvar är påslaget:")
    (kom-default-session-priority-tag . "Läsnivå för nya sessioner:")
    (kom-unsubscribe-makes-passive-tag . "Utträda ur möte gör om medlemskap till passiva:")
    (kom-review-priority-tag . "Prioritet för återsekommandon:")
    (kom-show-creating-software-tag . "Visa skapande klient:")
    (kom-text-footer-format-tag . "Format för inläggsfot:")
    (kom-long-lines-tag . "Långa streckade linjer:")
    (kom-postpone-default-tag . "Skönsvärde för uppskjuta läsning:")
    (kom-allow-incompleteness-tag . "Tillåt ofullständig information om olästa:")
    (kom-smileys-tag . "Visa gladmän grafiskt:")
    (kom-ignore-message-senders-tag . "Visa inte meddelanden från:")
    (kom-ignore-message-recipients-tag . "Visa inte meddelanden till:")
    (kom-text-footer-dash-length-tag . "Inläggsfotens längd:")
    (kom-text-header-dash-length-tag . "Längden på linjen ovanför inläggstexten:")
    (kom-show-personal-message-date-tag . "Visa datum för alla meddelanden:")
    (kom-w3-simplify-body-tag . "Visa HTML utan standardfärger:")
    (kom-mercial-tag . "Text när allt är utläst:")
    (kom-server-priority-tag . "Sessionsprioritet:")
    (kom-server-priority-breaks-tag . "Byt till LysKOM med olästa:")
    (kom-complete-numbers-before-names-tag . "Läs mötesnummer före mötesnamn:")
    (kom-keep-alive-interval-tag . "Intervall för håll förbindelsen igång:")
    (kom-text-no-prompts-tag . "Hur kommandon frågar efter textnummer:")
    (kom-saved-file-name-tag . "Fil att arkivera inlägg i:")
    (kom-follow-attachments-tag . "Läs importerade bilagor som vanligt:")
    (kom-show-unread-in-frame-title-tag . "Visa olästamarkering i titelraden:")
    (kom-created-texts-are-saved-tag . "Spara författade inlägg:")
    (kom-confirm-add-recipients-tag . "Fråga om kommentarer skall sändas till nya mottagare:")
    (kom-trim-buffer-minimum-tag . "Minsta storlek av LysKOM-bufferten som tas bort:")
    (kom-dont-check-commented-authors-tag . "Författare som inte kontrolleras:")
    (kom-print-relative-dates-tag . "Visa relativa datumangivelser:")
    (kom-print-seconds-in-time-strings-tag . "Inkludera sekunder i tidsangivelser:")
    (kom-review-uses-cache-tag . "Återsekommandon använder sparade inlägg:")
    (kom-review-marks-texts-as-read-tag . "Återsekommandon läsmarkerar visade texter:")
    (kom-auto-review-faqs-tag . "Visa nya FAQer automatiskt:")
    (kom-auto-list-faqs-tag . "Lista nya FAQer automatiskt:")
    (kom-extended-status-information-tag . "Utökad statusinformation:")
    (kom-highlight-first-line-tag . "Annorlunda bakgrundsfärg på första raden:")
    (kom-highlight-dashed-lines-tag . "Annorlunda bakgrundsfärg före och efter inläggstexten:")
    (kom-highlight-text-body-tag . "Annorlunda bakgrundsfärg på inläggstexten:")
    (kom-async-highlight-dashed-lines-tag . "Annorlunda bakgrundsfärg före och efter meddelanden:")
    (kom-async-highlight-text-body-tag . "Annorlunda bakgrundsfärg på meddelanden:")
    (kom-edit-hide-add-button-tag . "Visa Addera-knappen när man skriver inlägg:")
    (kom-format-html-authors-tag . "Formattera HTML-meddelanden per författare:")
    (kom-keyboard-menu-immediate-selection-tag . "Snabbval i textmenyer kräver bekräftelse:")
    (kom-max-overlays-tag . "Högsta antal färgmarkeringar:")
    (kom-mark-read-texts-as-read-in-new-recipient-tag . "Markera lästa inlägg lästa även i nya mottagare:")
    (kom-url-transformation-rules-tag . "Transformation av URLer:")
    (kom-text-links-tag . "Textlänkar:")
    (kom-active-face-tag . "Övriga klickbara texter:")
    (kom-active-highlight-face-tag . "Tydligare markerade klickbara texter:")
    (kom-active-strikethrough-face-tag . "Överstrukna klickbara texter:")
    (kom-url-face-tag . "Klickbara URLer:")
    (kom-me-face-tag . "Mitt eget namn:")
    (kom-highlight-face-tag . "Markering av klickbara ytor:")
    (kom-subject-face-tag . "Ärenderader:")
    (kom-text-no-face-tag . "Inläggsnummer:")
    (kom-friends-face-tag . "Vänner och bekanta:")
    (kom-morons-face-tag . "Idioter och knäppskallar:")
    (kom-presence-face-tag . "Närvaromeddelanden:")
    (kom-first-line-face-tag . "Första raden i inläggshuvudet:")
    (kom-warning-face-tag . "Viktiga varningar:")
    (kom-mark-face-tag . "Diverse markeringar:")
    (kom-dim-face-tag . "Diverse diskreta texter:")
    (kom-text-body-face-tag . "Inläggstexter:")
    (kom-dashed-lines-face-tag . "Streckrader runt inlägg:")
    (kom-async-text-body-face-tag . "Meddelanden:")
    (kom-async-dashed-lines-face-tag . "Streckrader runt meddelanden:")
    (kom-lynx-terminal-command-tag . "Kommando för Lynx i Emacs:")
    (kom-lynx-terminal-tag . "Var ska Lynx köra:")
    (kom-lynx-xterm-command-tag . "Kommando för Lynx i xterm:")
    (kom-show-sync-messages-tag . "Visa databasen-sparar-meddelanden:")
    (kom-highlight-conferences-tag . "Färgläggning av namn:")
    (kom-netscape-variant-tag . "Var ska Netscape/Mozilla öppna URLer:")
    )
)

  
;;;; ============================================================
;;;; The default Ansaphone message goes here. The more complex 
;;;; message specification probably should too, but it's not here
;;;; yet. People who know how to use it are smart enough to do it
;;;; right.

(lyskom-language-var local kom-ansaphone-default-reply sv
  "Jag läser inte LysKOM just nu. Skicka gärna ett brev i stället.")

;;;; ============================================================
;;;; Other language-dependent variables
;;;;

(lyskom-language-var local kom-ispell-dictionary sv
  "svenska")


;;;; ================================================================
;;;; Tell phrases should really be configured with the default
;;;; language used at the server and not for person reading if they
;;;; happen to differ. This is of coarse because they are sent to the
;;;; server for everybody else to see.

;;;; Aronsson was here 4 DEC 1990, thus creating version 0.18
					; Created *-tell-*


;;;; kom-tell-phrases 

;;; To coders of the elisp-client:
;;; if you change kom-tell-phrases here, make sure the list of valid
;;; keywords is updated it in vars.el.
;;; lyskom-tell-phrases-validation-keyword-list, lyskom-tell-phrases-validate

(eval-when-compile (defvar kom-tell-phrases))

(lyskom-language-strings local kom-tell-phrases sv
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
   (kom-tell-next-lyskom       . "Hoppar till ett annat LysKOM.")))

(if (and (boundp 'kom-tell-phrases)
         kom-tell-phrases)
    (lyskom-language-strings local kom-tell-phrases sv
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

(lyskom-language-var local kom-mercial sv
                     (lyskom-get-string 'kom-tell-wait 'kom-tell-phrases))

;;;


(lyskom-language-strings local lyskom-error-texts sv
  '((error-0 . "Inget fel har inträffat")
    (error-2 . "Ännu ej implementerat i servern")
    (error-3 . "Inte längre implementerat i servern")
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
    (error-18 . "Namnet för kort, för långt eller innehåller felaktiga tecken")
    (error-19 . "Index utanför gränserna")
    (error-20 . "Mötet existerar redan")
    (error-21 . "Personen existerar redan")
    (error-22 . "Hemligt, men ej lässkyddat")
    (error-23 . "Du får inte ändra person/mötesflaggan")
    (error-24 . "Fel i databasen. Attans otur.")
    (error-25 . "Otillåten mottagartyp eller kommentarslänk")
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
    (error-53 . "Mottagaren var kanske inte inloggad")
    (error-54 . "Otillåten typ på medlemskap")

    (error--1 . "Du är inte medlem i något mottagarmöte")
))

(lyskom-language-var global lyskom-unread-mode-line sv
  (lyskom-make-lyskom-unread-mode-line))

(lyskom-language-var global lyskom-unread-title-format sv
  (lyskom-make-lyskom-unread-title-format))

(lyskom-language-var local lyskom-help-data sv lyskom-sv-help-data)

(provide 'lyskom-language-sv)

;;; swedish-strings.el ends here
