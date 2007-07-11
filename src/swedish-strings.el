;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: swedish-strings.el,v 44.417 2007-07-11 20:48:43 byers Exp $
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
	      "$Id: swedish-strings.el,v 44.417 2007-07-11 20:48:43 byers Exp $\n"))


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

(defvar lyskom-sv-edit-prefix)
(defvar lyskom-sv-edit-review-prefix)
(defvar lyskom-sv-edit-insert-prefix)
(defvar lyskom-sv-edit-aux-prefix)
(defvar lyskom-sv-edit-add-prefix)

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
  (define-key lyskom-sv-edit-prefix (kbd "C-a")     'lyskom-sv-edit-add-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "C-i")     'lyskom-sv-edit-insert-prefix)
  (define-key lyskom-sv-edit-prefix (kbd "TAB")     'lyskom-sv-edit-insert-prefix)

  (lyskom-try-define-key lyskom-sv-edit-prefix (kbd "C-]")       'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix (kbd "C-}")       'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix (kbd "C-�")       'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix (kbd "C-�")       'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [(control aring)] 'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [(control Aring)] 'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix "\205"            'lyskom-sv-edit-review-prefix) ; For windows and Emacs 19

  (lyskom-try-define-key lyskom-sv-edit-prefix (kbd "�") 'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix (kbd "}") 'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix (kbd "]") 'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [aring]   'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [Aring]   'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [�]       'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [�]       'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [229]     'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [197]     'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [2277]    'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [2245]    'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [3909]    'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [3941]    'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [195 165] 'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [195 165] 'lyskom-sv-edit-review-prefix)
  (lyskom-try-define-key lyskom-sv-edit-prefix [(�)]     (let ((k (make-sparse-keymap)))
							   (define-key k (vector (intern "\205")) 'lyskom-sv-edit-review-prefix)
							   (define-key k [�] 'lyskom-sv-edit-review-prefix)
							   k))

  (define-key lyskom-sv-edit-prefix (kbd "?")       'lyskom-help)
  (define-key lyskom-sv-edit-prefix (kbd "*")       'kom-button-press)
  (define-key lyskom-sv-edit-prefix (kbd "=")       'kom-menu-button-press)
  (define-key lyskom-sv-edit-prefix (kbd "C-c")     'kom-edit-send)
  (define-key lyskom-sv-edit-prefix (kbd "C-s")     'kom-edit-ispell-message)
  (define-key lyskom-sv-edit-prefix (kbd "C-k")     'kom-edit-quit)
  (define-key lyskom-sv-edit-prefix (kbd "C-y")     'kom-edit-insert-commented)
  (define-key lyskom-sv-edit-prefix (kbd "C-b")     'kom-edit-insert-buglist)

  (define-key lyskom-sv-edit-insert-prefix (kbd "?")   'lyskom-help)
  (define-key lyskom-sv-edit-insert-prefix (kbd "C-k") 'kom-edit-insert-commented)
  (define-key lyskom-sv-edit-insert-prefix (kbd "C-y")     'kom-edit-insert-commented)
  (define-key lyskom-sv-edit-insert-prefix (kbd "1")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-insert-prefix (kbd "2")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-insert-prefix (kbd "3")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-insert-prefix (kbd "4")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-insert-prefix (kbd "5")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-insert-prefix (kbd "6")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-insert-prefix (kbd "7")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-insert-prefix (kbd "8")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-insert-prefix (kbd "9")   'kom-edit-insert-digit-text)
  (define-key lyskom-sv-edit-insert-prefix (kbd "SPC") 'kom-edit-insert-text)
  (define-key lyskom-sv-edit-insert-prefix (kbd "C-l") 'kom-edit-insert-link)
  (define-key lyskom-sv-edit-insert-prefix (kbd "C-i") 'kom-edit-insert-last-info-nodename)

  (define-key lyskom-sv-edit-add-prefix (kbd "C-m") 'kom-edit-add-recipient)
  (define-key lyskom-sv-edit-add-prefix (kbd "<RET>") 'kom-edit-add-recipient)
  (define-key lyskom-sv-edit-add-prefix (kbd "C-e") 'kom-edit-add-copy)
  (define-key lyskom-sv-edit-add-prefix (kbd "C-b") 'kom-edit-add-bcc)
  (define-key lyskom-sv-edit-add-prefix (kbd "C-f") 'kom-edit-move-text)
  (define-key lyskom-sv-edit-add-prefix (kbd "C-j") 'kom-edit-add-recipient)
  (define-key lyskom-sv-edit-add-prefix (kbd "<LFD>") 'kom-edit-add-recipient)
  (define-key lyskom-sv-edit-add-prefix (kbd "C-k") 'kom-edit-add-comment)
  (define-key lyskom-sv-edit-add-prefix (kbd "C-x") 'kom-edit-add-cross-reference)
  (define-key lyskom-sv-edit-add-prefix (kbd "?")   'lyskom-help))

  (define-key lyskom-sv-edit-aux-prefix (kbd "C-p") 'kom-edit-add-personal-comments)
  (define-key lyskom-sv-edit-aux-prefix (kbd "C-n") 'kom-edit-add-no-comments)
  (define-key lyskom-sv-edit-aux-prefix (kbd "C-l") 'kom-edit-add-world-readable)
  (define-key lyskom-sv-edit-aux-prefix (kbd "C-b") 'kom-edit-add-read-confirm-request)

  (define-key lyskom-sv-edit-review-prefix (kbd "?")     'lyskom-help)
  (define-key lyskom-sv-edit-review-prefix (kbd "C-k")   'kom-edit-show-commented)
  (define-key lyskom-sv-edit-review-prefix (kbd "k")     'kom-edit-show-commented)



(defconst lyskom-strings-missing '())



;;; The alist formely known as lyskom-strings
(lyskom-language-strings local lyskom-message sv
  '(
    ;; From vars.el: 
    ;; From komtypes.el: nil
    ;; From clienttypes.el: nil
    ;; From startup.el:
    (server-q . "LysKOM-server? ")
    (try-connect . "LysKOM elisp-klient version %#1s.\nF�rs�ker koppla upp mot %#2s.\n")
    (too-old-server . "Servern �r f�r gammal f�r den h�r versionen av klienten.")
    (connection-done . "Uppkopplingen klar. Serverns versionsnummer �r %#1s.\n\n")
    (what-is-your-name . "Vad heter du? ")
    (password . "L�senord? ")
    (wrong-password . "Fel l�sen.\n")
    (wrong-password-help . "
Du kan skicka e-mail till n�gon av nedanst�ende adresser f�r att f�
ett nytt l�senord eller f�r att f� din gamla person raderad s� att du
kan skapa en ny:\n")
    (wrong-password-email . "* %#1t\n")
    (are-logged-in . "Du �r nu inloggad. V�nta ett tag.\n")
    (you-have-motd . "\nDu har en lapp p� d�rren:\n\n")
    (server-has-motd . "Servern har ett loginmeddelande:\n")
    (lyskom-motd-was-garbed . "\nLoginmeddelandet finns inte!
Det meddelande som ska visas efter inloggning har f�rsvunnit.
Kontakta LysKOM-administrat�ren.\n")
    (first-greeting . "%#1s
Det tycks vara f�rsta g�ngen du anv�nder LysKOM. V�lkommen!
Kontrollera att du stavat ditt namn r�tt. Anv�nd g�rna ditt fullst�ndiga
namn och organisation, t ex \"Eskil Block, FOA\". Om du stavat ditt namn
fel, eller vill �ndra ditt namn, svara nej p� fr�gan nedan.

Observera att all information �n s� l�nge sparas s� att vem som helst
kan l�sa den. Dock �r l�senordet krypterat.

Om du �r os�ker p� hur man anv�nder LysKOM kan du h�mta en manual via
anonym ftp fr�n ftp.lysator.liu.se. Fr�ga din systemadministrat�r om
du �r os�ker. Du kan �ven skriva \"?\" f�r kommandot \"Hj�lp\".
")

    (is-name-correct . "�r namnet %#1s korrekt? ")
    (personal-password . "Ange ett personligt l�sen: ")
    (repeat-password . "Repetera f�r kontroll: ")
    (repeat-failure . "Du angav inte samma l�senord b�gge g�ngerna.\n")

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
    (not-present-anywhere . "Ej n�rvarande i n�got m�te.")
    (in-secret-conference . "Hemligt m�te (%#1d).")
    (start-new-session-same-server
     . "Du k�r redan mot den servern. Vill du starta en ny session? ")
    (new-session-in-buffer . "\n\n---- Ny session startad %s ----\n\n")
    (warning-about-uncompiled-client . "
OBS: LysKOM-klienten �r inte kompilerad. F�r att �ka prestanda b�r du k�ra en
kompilerad version av klienten. L�s i den medf�ljande README-filen hur man
g�r det.
")

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
    (delete-marked-by-you-and-others . "markerad av dig och %#1?d%[n�gon annan%]%[%#1d andra%]")
    (delete-marked-by-several . "markerat av %#1d person%#1?d%[%]%[er%]")
    (delete-commented-text-help . "\
Inl�gget du h�ller p� att radera har kommentarer. Om du tar bort inl�gget
s� kommer dessa kommentarer att f�rlora sitt sammanhang. Detta irriterar
m�nga l�sare. �verv�g att flytta inl�gget till ett annat m�te ist�llet
f�r att radera det.")
    (delete-commented-text . "Inl�gget har kommentarer. Radera �nd�? ")
    (really-delete-commented-text . "Att radera kommenterade inl�gg irriterar m�nga l�sare. Radera �nd�?")
    (deleting-text . "Radering av text %#1:n...")

    (presentation-for-whom . "Se presentation f�r vilket m�te/person? ")
    (unread-presentation-for-whom . "Ol�smarkera presentation f�r vilket m�te/person? ")
    (text-to-see-author-of . "Se presentation f�r vilket inl�ggs f�rfattare? ")
    (text-to-unread-author-of . "Ol�smarkera presentation f�r vilket inl�ggs f�rfattare? ")
    (somebody-deleted-that-conf . "N�gon tog precis bort m�tet.\n")
    (review-presentation-of . "�terse presentation av %#1M.\n")
    (has-no-presentation . "%#1:M har ingen presentation.\n")

    (have-to-read . "Du m�ste l�sa ett inl�gg f�rst.\n")

    (no-comment-to . "Det finns inget kommenterat inl�gg att titta p�.\n")
    (no-text-at-point . "Det finns inget inl�gg vid mark�ren.\n")
    (what-ancestor . "Vilken av de kommenterade texterna �nskas? ")

    (who-letter-to . "Vem vill du skicka brev till? ")
    (who-send-text-to . "Vart vill du skicka inl�gget? ")
    (has-motd . "%#1P har en lapp p� d�rren:\n\n")
    (motd-persist-q . "Vill du fortfarande skriva texten? ")

    (who-to-add . "Vem vill du addera? ")
    (where-to-add . "Vilket m�te skall han/hon adderas till? ")
    (where-to-add-self . "Vilket m�te vill du bli medlem i? ")
    (priority-q . "Prioritet p� ditt medlemskap i m�tet? (0 (l�g) - 255 (h�g)) ")
    (other-priority-q . "Prioritet p� den adderades medlemskap i m�tet? (0 (l�g) - 255 (h�g)) ")
    (done . "klart.\n")
    (cancelled . "avbrutet.\n")
    (nope . "det gick inte.\n")
    (add-already-member . "%#2P �r redan medlem i %#1M.\n")
    (you-already-member . "Du �r redan medlem i %#1M.\n")

    (cant-find-supervisor . "Hittar inte organisat�ren f�r %#1M.\n")
    (is-read-protected-contact-supervisor . "%#1M �r slutet.
Skicka ett brev till %#2P f�r medlemskap.\n")

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
    (what-private-no . "Personligt svar till text nummer: ")
    (quit-in-spite-of-unsent . "Vill du avsluta sessionen trots oinskickat inl�gg? ")
    (really-quit . "Vill du verkligen avsluta sessionen? ")
    (session-ended . "
*****************************
LysKOM-sessionen �r avslutad.
*****************************\n")
    (session-auto-ended . "
===========================================================
Kopplar ned fr�n LysKOM eftersom LysKOM �r fullt och
du har l�st klart allting. Kom tillbaks senare.
===========================================================\n\n")
    (session-ended-long . "
*******************************************************************************
LysKOM-sessionen �r avslutad.
*******************************************************************************
")
    (session-auto-ended-long . "
===============================================================================
Kopplar ned fr�n LysKOM eftersom LysKOM �r fullt och
du har l�st klart allting. Kom tillbaks senare.
===============================================================================
\n")
    (what-to-change-pres-you . "Vilket m�te/person vill du �ndra presentationen f�r: ")
    (what-to-change-faq-you . "Vilket m�te vill du �ndra FAQ f�r: ")
    (who-to-put-motd-for . "Vilket m�te/person vill du s�tta lapp p� d�rr f�r: ")

    (what-to-set-pres-you . "Vilket m�te/person vill du s�tta presentationen f�r: ")
    (what-text-to-set-as-pres-no . "Vilket inl�gg vill du ska vara den nya presentationen: ")
    (what-to-set-motd-you . "Vilket m�te/person vill du s�tta lapp p� d�rr f�r: ")
    (what-text-to-set-as-motd-no . "Vilket inl�gg vill du ska vara den nya lappen: ")
    (conf-already-has-pres . "M�tet/personen har redan en presentation. Forts�tt �nd�? ")
    (conf-already-has-motd . "M�tet/personen har redan en lapp p� d�rren. Forts�tt �nd�? ")
    (setting-conf-pres . "S�tter presentation f�r %#1M till text %#2n...")
    (setting-conf-motd . "S�tter lapp p� d�rren f�r %#1M till text %#2n...")

    (who-to-remove-pres-for . "Vilket m�te/person vill du ta bort presentationen fr�n: ")
    (removing-pres-for-conf . "Tar bort presentation (text %#2n) fr�n %#1M...")

    (cant-get-conf-stat . "Kan ej h�mta m�tesstatus f�r m�tet.\n")
    (go-to-conf-p . "G� till m�te: ")
    (want-become-member . "Vill du bli medlem? ")
    (no-ok . "Nehej.\n")

    (who-to-remove-motd-for . "Vilket m�te/person vill du ta bort lapp p� d�rr f�r: ")

    (conf-all-read . "inga ol�sta")
    (no-in-conf . "Du �r inte n�rvarande i n�got m�te.\n")

    (search-for-pers . "Ange s�kvillkor (RETURN f�r alla personer): ")
    (search-for-conf . "Ange s�kvillkor (RETURN f�r alla m�ten): ")
    (search-re . "Ange s�kuttryck (regulj�rt uttryck): ")
    (search-re-for-what . "Vad vill du s�ka? ")
    (search-re-persons . "pS�k personer")
    (search-re-confs . "mS�k m�ten")
    (search-re-all . "bS�k alla")

    (no-matching-confs . "Inga m�ten uppfyller s�kvillkoret \"%#1s\".\n")
    (no-matching-perss . "Inga personer uppfyller s�kvillkoret \"%#1s\".\n")
    (no-matching-anys . "Inga personer eller m�ten uppfyller uttrycket \"%#1s\".\n")
    (no-confs-exist . "Det finns inga m�ten i databasen.\n")
    (no-pers-confs-exist . "Det finns inga m�ten eller personer i databasen.\n")
    (list-confs-created-by . "Lista �gda m�ten f�r: ")
    (listing-confs-created-by . "�gda m�ten f�r %#1P\n\
    S=Skapat, O=Organisat�r, M=Brevl�da som superm�te; S=Slutet, H=Hemligt\n")
    (list-pers-confs-created-by . "Lista �gda m�ten (med brevl�dor) f�r: ")
    (getting-all-confs . "H�mtar en lista av alla m�ten fr�n servern...")
    (getting-all-pers-confs . "H�mtar en lista av alla personer och m�ten fr�n servern...")
    (getting-all-confs-done . "H�mtar en lista av alla m�ten fr�n servern...klart")
    (getting-all-pers-confs-done . "H�mtar en lista av alla personer och m�ten fr�n servern...klart")
    (finding-created-confs . "S�ker �gda m�ten (%#1d av %#2d klart)")
    (finding-created-pers-confs . "S�ker �gda m�ten (med brevl�dor) (%#1d av %#2d klart)")
    (no-created-confs . "%#1P �r inte skapare, organisat�r eller superm�te f�r n�got m�te.\n")

    (name-to-be-changed . "Ange det namn som skall �ndras: ")
    (no-such-conf-or-pers . "M�tet eller personen finns ej.\n")
    (new-name . "Nytt namn: ")
    (new-paren . "Ny parentes: ")
    (too-many-parens-in-name . "Namnet inneh�ller mer �n en parentes. Anv�nd \"�ndra namn\" ist�llet.\n")
    (no-paren-in-name . "Namnet inneh�ller ingen parentes.\n")
    (who-to-change-supervisor-for . "Vem vill du �ndra organisat�r f�r? ")
    (new-supervisor . "Ny organisat�r: ")
    (text-to-mark . "Vilket inl�gg vill du markera? ")
    (text-to-unmark . "Vilket inl�gg vill du avmarkera? ")
    (what-mark . "Vilken typ av markering vill du s�tta (namn eller 0-255)? ")
    (erroneous-mark . "Felaktig markeringstyp.\n")
    (want-to-create-symbolic-mark . "Markeringstypen \"%#1s\" finns inte. Vill du skapa den? ")
    (creating-symbolic-mark-type . "Skapar markeringstypen \"%#1s\" (%#2d).\n")
    (no-mark-types-left . "Ledsen, det finns inga lediga markeringstyper. Rensa bland de gamla f�rst.")
    (unmarking-textno . "Avmarkering av text %#1n...")
    (marking-textno . "Markering av text %#1n med markering %#2?b%[\"%#2s\" (%#3d)%]%[%#3d%]...")
    (list-which-mark . "Lista vilken markeringstyp (namn eller 0-255, RET f�r alla)? ")

    (new-passwd-again . "Mata in det nya l�senordet igen f�r kontroll: ")
    (what-mark-to-unread . "Ol�smarkera vilken markeringstyp (namn eller 0-255, RET f�r alla)? ")
    (what-mark-to-view . "�terse vilken markeringstyp (namn eller 0-255, RET f�r alla)? ")
    (whos-passwd . "Vem vill du �ndra l�senord f�r? ")
    (old-passwd . "Mata in ditt nuvarande l�senord: ")
    (new-passwd . "Mata in det nya l�senordet: ")
    (changing-passwd . "�ndrar l�senordet...")
    (retype-dont-match . "L�senorden �r inte samma. G�r om.\n")
    (palindrome . " (en palindrom!)")
    (lyskom-name . "Anv�ndare")
    (is-in-conf . "N�rvarande i m�te")
    (from-machine . "K�r fr�n")
    (is-doing . "G�r")
    (connection-time . "Loggade in")
    (active-last . "Aktiv senast")
    (active . "Aktiv")
    (lyskom-client . "Klient")
    (move-roots-q . "Inl�gg %#1n �r en fotnot/bilaga. Flytta %#2n%#3?b%[ mfl.%]%[%] ist�llet? ")
    (add-roots-q . "Inl�gg %#1n �r en fotnot/bilaga. Addera till %#2n%#3?b%[ mfl.%]%[%] ist�llet? ")
    (sub-roots-q . "Inl�gg %#1n �r en fotnot/bilaga. Subtrahera fr�n %#2n%#3?b%[ mfl.%]%[%] ist�llet? ")
    (text-to-add-recipient . "Vilket inl�gg vill du addera mottagare till:")
    (text-to-add-copy . "Vilket inl�gg vill du addera en extra kopia till:")
    (text-to-add-bcc . "Vilket inl�gg vill du skicka som dold kopia:")
    (text-to-delete-recipient . "Vilket inl�gg vill du subtrahera mottagare fr�n:")
    (text-to-move . "Vilket inl�gg vill du flytta:")
    (text-tree-to-move . "Vilket inl�gg �r rot till tr�det du vill flytta:")
    (text-to-add-comment-to . "Vilket inl�gg vill du addera en kommentar till:")
    (text-to-delete-comment-from . "Vilket inl�gg vill du subtrahera en kommentar fr�n:")
    (text-to-add-footnote-to . "Vilket inl�gg vill du addera en fotnot till:")
    (text-to-delete-footnote-from . "Vilket inl�gg vill du subtrahera en fotnot fr�n:")
    (comment-to-move-q . "Vilken kommentar vill du flytta:")
    (comment-move-from-q . "Fr�n vilken text vill du flytta %#1n:")
    (comment-move-to-q . "Till vilken text vill du flytta %#1n:")
    (text-is-not-a-comment . "Inl�gg %#1n �r inte en kommentar till n�got inl�gg\n")
    (cant-move-footnotes . "Kan inte flytta fotnoter.\n")
    (moving-comment . "Flyttar %#1n fr�n %#2n till %#3n...\n")

    (text-to-add-cross-reference-to . "Vilket inl�gg vill du addera en referens till:")
    (text-has-no-recipients-r . "Inl�gg %#1n har inga mottagare\n")
    (cant-move-from-to-same . "M�tet du flyttar till kan inte vara samma som du flyttar fr�n.\n")

    (where-on-list-q . "Placering p� listan? (0-%#1d) ")
    (accepting-invitation . "Accepterar inbjudan till %#2M%#3?b%[ %#3s%]%[%]...")
    (member-in-conf . "Bli medlem i %#2M%#3?b%[ %#3s%]%[%]...")
    (member-in-conf-with-low-priority . "%#1M har l�gre prioritet �n din l�sniv� och kommer d�rf�r inte att visas.\n")
    (member-in-conf-with-unread-date . "med ol�sta fr�n %#3d %#2s %#1d")
    (member-in-conf-with-unread . "med %#1d ol�sta")
    (add-member-in . "Addera %#1P som medlem i %#2M...")
    (change-priority-for-q . "�ndra prioritet f�r m�te: ")
    (cant-change-priority-of-passive . "Kan inte �ndra prioritet p� passiva medlemskap.\n")
    (change-priority-for . "�ndra prioritet p� %#2M...")
    (unsubscribe-to . "Uttr�da ur %#1M...")
    (unsubscribe-to-2 . "Uttr�da ur %#1M%#2s...")
    (confirm-each-join . "Vill du bekr�fta medlemskap i enskila m�ten? ")
    (confirm-each-msg . "Vill du bekr�fta mottagande av gruppmeddelande i enskila m�ten? ")
    (receive-each-msg . "Vill du ta emot gruppmeddelande i alla m�ten? ")
    (no-confirm-each-sure . "�r du helt s�ker p� att du vill g� med i %#1d m�ten automatiskt? ")
    (confirm-join . "G� med i %#1M? ")
    (getting-all-confs . "H�mtar en lista med alla m�ten...")
    (getting-all-confs-progress . "H�mtar en lista med alla m�ten (%#1d/%#2d)...")
    (getting-all-confs-done . "H�mtar en lista med alla m�ten...klart")
    (unsub-all-skipping-letterbox . "nix\nM�tet �r din brevl�da.\n")
    (unsub-all-skipping-supervised . "nix\nDu �r organisat�r f�r m�tet.\n")
    (unsub-secret-conf-q . "%#1M �r hemligt. Vill du g� ur m�tet �nd�? ")
    (unsub-all-secret . "aG� ur alla hemliga m�ten")
    (unsub-closed-conf-q . "%#1M �r slutet. Vill du g� ur m�tet �nd�? ")
    (unsub-all-closed . "aG� ur alla slutna m�ten")
    (unsub-open-conf-q . "Vill du g� ur %#1M? ")
    (unsub-all-open . "aG� ur alla �ppna m�ten")
    (abc-yes . "jJa")
    (abc-no . "nNej")
    (abc-yes-all . "aJa till alla")
    (abc-no-all . "xNej till alla")

    (move-tree-move . "fFlytta")
    (move-tree-sub  . "sSubtrahera")
    (move-tree-add  . "aAddera")
    (move-tree-ign  . "iIngenting")
    (move-tree-jump . "hHoppa")
    (move-tree-quit . "qAvbryt")

    (move-tree-add-rcpt-type . "Addera som: ")
    (move-tree-rcpt . "mMottagare")
    (move-tree-cc . "eExtra kopia")
    (move-tree-bcc . "dDold kopia")

    (move-tree-leave-cc-q . "L�mna ursprungsmottagare som extrakopiemottagare? ")

    (exclude-from . "Utesluta %#1P fr�n %#2M...")

    (unsubscribe-failed . "\nDet gick inte. %#1P var kanske aldrig medlem i %#2M?\n")
    (activate-mship . "Bli aktiv medlem i %#1M...")
    (activate-mship-done . "Du �r nu �ter aktiv medlem i %#1M.\n")
    (passivate-done . "Du �r nu passiv medlem i %#1M.
Uttr�d ur m�tet en g�ng till f�r g� ur helt.\n")

    (You . "Du")
    (could-not-create-conf . "M�tet \"%#1s\" kunde ej skapas.\n")
    (created-conf-no-name . "M�te nummer %[%#3@%#1:m %#2:M%], skapat.\n")
    (cant-read-textno . "Du f�r inte l�sa text %#1:n.\n")

    (not-supervisor-for . "Du �r inte organisat�r f�r %#1M.\n")
    (not-supervisor-for-server . "Du �r inte administrat�r f�r LysKOM-servern.\n")
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
			  "Du har inga markerade inl�gg med markeringstypen \"%#1s\".\n")

;;; For later
;    (northward . "norrut")
;    (southward . "s�derut")
;    (permanent-sundown . "Solen kommer inte att g� upp. Flytta %#1s!")
;    (permanent-sunup . "Solen kommer inte att g� ned. Flytta %#1s!")
;    (sunup-soon . "Solen g�r snart upp")
;    (sundown-recently . "Solen gick nyligen ned")
;    (after-sunset . "Solen har g�tt ned")
;    (before-sunup . "Solen har inte g�tt upp �nnu")
;    (sun-is-up . "Solen �r uppe")
;;;
    (weekdays . ["s�ndag" "m�ndag" "tisdag" "onsdag" "torsdag"
		 "fredag" "l�rdag" "s�ndag"])
    (weekdays-short . ["s�n" "m�n" "tis" "ons" "tor" "fre" "l�r" "s�n"])
    (time-is . "Det �r %#1s%#2s (enligt servern).")
    (time-is-week . "Det �r %#1s%#2s, vecka %#3d (enligt servern).")

    (xmaseve . "\nJulafton! Har du �ppnat dina julklappar �n?")
    (xmasday . "Juldagen.\nDu har v�l varit i julottan?")
    (newyearday . "Ny�rsdagen. Gott nytt %#1d!")
    (newyearevelate . "Mindre �n en timme kvar av %#1d...")
    (newyeareve . "Ny�rsafton.")
    (cgdag . "Valborgsm�ssoafton.\nKonungens f�delsedag.")
    (sixjune . "Sveriges nationaldag och svenska flaggans dag.")
    (holdnose . "H�ll f�r n�san...")
    (lysbday . "
P� denna dag, �r 1973, grundades Lysator, och det var en stor dag
i svensk datorhistoria. L�s mer p� http://www.lysator.liu.se/history/")
    (13dayxmas . "Trettondedag jul (\"Trettondagen\").")
    (20dayxmas . "Tjugondedag jul (\"Tjugondag Knut\").")
    (kyndeldag . "Kyndelsm�ssodagen.")
    (skottdag . "Skottdagen.")
    (intwomday . "Internationella kvinnodagen.")
    (mariebdag . "Marie beb�delsedag.\nFram med v�ffelj�rnet!")
    (johannesddag . "Johannes D�parens dag.")
    (fnday . "FN-dagen.")
    (allhelgonadag . "Allhelgonadagen.")
    (varnlosdag . "V�rnl�sa barns dag.")
    (luciaday . "\nLuciadagen! Gl�m inte att lussa f�r dina kollegor.")

    (total-users . "    Sammanlagt %#1d anv�ndare (%#2s).\n")
    (total-users-sans-date . "    Sammanlagt %#1d anv�ndare\n")
    (total-visible-users . "    Sammanlagt %#1d synlig%#1?d%[%]%[a%] anv�ndare (%#2s).\n")
    (total-active-users . "    Sammanlagt %#1d aktiv%#1?d%[%]%[a%] anv�ndare (%#2s).\n")
    (total-visible-active-users
     . "    Sammanlagt %#1d synlig%#1?d%[%]%[a%] aktiv%#1?d%[%]%[a%] anv�ndare (%#2s).\n")
    (client-statistics . "    Klienternas f�rdelning:\n")
    (client-statistics-line . "    %4#2d %#1s%#3?d%[%]%[ [%#3d varianter]%]")
    (who-to-add-q . "Vilket m�te/person vill du addera som mottagare till text %#1n? ")
    (who-to-add-copy-q . "Vilket m�te/person vill du addera som kopiemottagare till text %#1n? ")
    (who-to-add-bcc-q . "Vilket m�te/person vill du addera som mottagare av en dold kopia till text %#1n? ")
    (add-footnotes-too-q . "F�rs�k addera mottagare till fotnoter ocks�? ")
    (add-attachments-too-q . "F�rs�k addera mottagare till bilagor ocks�? ")

    (really-add-as-recpt-q . "Skall framtida kommentarer till inl�gget ocks� skickas till %#1M? ") 
    (who-to-sub-q . "Vilket m�te/person vill du subtrahera som mottagare till text %#1n? ")
    (sub-footnotes-too-q . "F�rs�k subtrahera mottagare fr�n fotnoter ocks�? ")
    (sub-attachments-too-q . "F�rs�k subtrahera mottagare fr�n bilagor ocks�? ")

    (who-to-move-from-q . "Fr�n vilket m�te vill du flytta text %#1n? ")
    (who-to-move-to-q . "Vart vill du flytta text %#1n? ")
    (edit-who-to-move-to-q . "Vart vill du flytta texten? ")
    (who-to-move-to-or-sub-q . "Vart vill du flytta texten (tomt f�r att bara subtrahera)? ")
    (move-footnotes-too-q . "F�rs�k flytta fotnoter ocks�? ")
    (move-attachments-too-q . "F�rs�k flytta bilagor ocks�? ")
    (move-text-not-recipient . "har ej r�tt mottagare.\n")

    (adding-name-as-recipient . "Adderar %#1M som mottagare till text %#2n...")
    (adding-name-as-copy . "Adderar %#1M som kopiemottagare till text %#2n...")
    (adding-name-as-bcc . "Adderar %#1M som dold kopiemottagare till text %#2n...")
    (remove-name-as-recipient . "Subtraherar %#1M som mottagare fr�n text %#2n...")


    (unknown-recpt-text . "ok�nd mottagare")
    (cc-recpt-text . "kopiemottagare")
    (bcc-recpt-text . "dold kopiemottagare")
    (recpt-text . "mottagare")
    (add-conf-as-rcpt-q . "Vill du g�ra %#1M %#2s till text %#3n? ")
    (del-conf-as-rcpt-q . "Vill ta bort %#1M som %#2s till text %#3n? ")
    (change-conf-as-rcpt-q . "Vill du �ndra %#2s %#1M till en %#3s till text %#4n? ")
    (move-conf-as-rcpt-q . "Vill du flytta %#1n fr�n %#2M till %#3M? ")

    (adding-cross-reference . "Adderar referens...")

    (error-recipient-limit . "Text %#1n har f�r m�nga mottagare.\n")
    (error-already-recipient . "%#2M �r redan mottagare till inl�gg %#1n.\n")
    (error-permission-denied-add-recpt . "Bara f�rfattaren till %#1n eller organisat�ren f�r %#2M kan �ndra mottagartyp.\n")
    (error-permission-denied-sub-recpt . "Bara f�rfattaren till %#1n eller organisat�ren f�r %#2M kan subtrahera mottagare.\n")
    (error-access-denied-add-recpt . "Du f�r inte addera inl�gg till %#2M.\n")
    (error-not-recipient . "%#2M �r inte mottagare till inl�gg %#1n.\n")

    (moving-tree-what-action-q . "Vad vill du g�ra? ")
    (moving-cant-read . "Det gick inte att inte flytta text %#1n (kan inte l�sa texten).\n")
    (moving-name . "Flyttar text %#3n fr�n %#1M till %#2M...")
    (moving-already-moved . "Inl�gg %#1n har inte %#2M som mottagare.\nFlyttar inte inl�gget eller dess kommentarstr�d.\n")
    (text-to-add-q . "Vilket inl�gg vill du addera som kommentar till %#1n? ")
    (text-to-remove-q . "Vilket inl�gg vill du subtrahera som kommentar fr�n %#1n? ")
    (text-to-add-footn-q . "Vilket inl�gg vill du addera som fotnot till %#1n? ")
    (text-to-remove-footn-q . "Vilket inl�gg vill du subtrahera som fotnot fr�n %#1n? ")
    (add-comment-to . "Adderar text %#1n som kommentar till text %#2n...") ;
    (sub-comment-to . "Subtraherar text %#1n som kommentar till text %#2n...")
    (add-footnote-to . "Adderar text %#1n som fotnot till text %#2n...") ;
    (sub-footnote-to . "Subtraherar text %#1n som fotnot till text %#2n...")
    (comment-keep-recpt-p ."Ska %#1s vara mottagare? ")
    (comment-all-relevant-p . "Inl�gget har flera mottagare. �r alla relevanta? ")
    (please-edit-recipients . "�ndra mottagarlistan och skicka in inl�gget igen.")
    (checking-rcpt . "Kontrollerar mottagare...")
    (checking-rcpt-done . "Kontrollerar mottagare...klart")
    (checking-comments . "Kontrollerar kommenterade texter...")
    (checking-comments-done . "Kontrollerar kommenterade texter...klart")
    (please-check-commented-texts . "�terse de kommenterade texterna och deras kommentarer.")
    (have-unread-comment . "Skicka in trots ol�sta kommentarer till text %#1n? ")
    (duplicate-recipients . "Kan inte skicka in texten. En mottagare f�rekommer flera g�nger (%#1M)")
    (no-recipients . "Kan inte skicka in texten. Inga mottagare har angivits.")

    (add-recipient-p . "Addera mottagare %#2?b%[ %#2M f�r att n�%]%[%] %#1P? ")
    (matching-regexp . "M�ten/personer som matchar '%#1s'\n")
    (matching-regexp-perss . "Personer som matchar '%#1s'\n")
    (matching-regexp-confs . "M�ten som matchar '%#1s'\n")

    (who-is-active-all . "Visar alla sessioner.\n")
    (who-is-active-last-minutes . "Visar alla sessioner som har varit aktiva de senaste %#1d minuterna.\n")
    (showing-invisibles . "Visar osynliga sessioner.\n")
    (null-who-info . "Det finns inga (aktiva) inloggade.\n")

    (no-other-lyskom-r . "Det finns inga fler aktiva LysKOM-sessioner.\n")
    (no-lyskom-session . "Det finns ingen aktiv LysKOM-session.")
    (no-other-unread-lyskom-r . "Hittar ingen annan LysKOM-session med ol�sta.\n")
    (no-unread-lyskom-r . "Hittar ingen aktiv LysKOM-session med ol�sta.\n")
    (no-unread-lyskom . "Hittar ingen aktiv LysKOM-session med ol�sta.")

    (who-is-on-in-what-conference . "Vilka i vilket m�te: ")
    (who-is-present-in-what-conference . "Vilka n�rvarande i vilket m�te: ")
    (who-is-friend . "Visar endast v�nner.\n")
    (who-is-active-and-member . "Visar endast medlemmar i %#1M.\n")
    (who-is-active-and-present . "Visar endast medlemmar n�rvarande i %#1M.\n")

    (personal-comment-to-anonymous . "Det g�r inte att skriva ett personligt svar p� ett anonymt inl�gg.\n")

    ;; From commands2.el:

    (your-memberships . "Ditt medlemskap i olika KOM-m�ten:\n")
    (memberships-header . "Senast inne	   Prio	Ol�sta	M�tesnamn\n")
    (memberships-line . "%16#1s  %#2d\t%#3d\t%#4M\n")
    (conf-for-status . "Vilket m�te vill du se statusen f�r? ")
    (no-such-conf . "M�tet finns ej.\n")
    (status-record . "Status f�r m�te %#1M (%#1m)%#2s\n\n")
    (change-type-prompt . "�ndra m�testyp f�r m�te %#1M (%#1m) (%#2s)...")
    (Mailbox . "Brevl�da")
    (Protected . "Hemligt")
    (no-comments . "original")
    (closed . "slutet")
    (allow-anon . "anonymt ok")
    (allow-secret . "hemliga medl")

    (created-by . "Skapat av person %25#1p %#3s(%#2P)\n")
    (created-at . "Skapad:%35#1s\n")
    (members .    "Antal medlemmar: %25#1d\n")
    (conf-allows-secret-members . "Hemliga medlemmar:                       %#1s\n")
    (conf-allows-anon-texts .     "Anonyma inl�gg:                          %#1s\n")
    (anon-texts-permitted . "Anonyma inl�gg �r till�tna")
    (anon-texts-not-permitted . "Anonyma inl�gg �r inte till�tna")
    (secret-members-permitted . "Hemliga medlemmar �r till�tna")
    (secret-members-not-permitted . "Hemliga medlemmar �r inte till�tna")
    (garb-nice .      "Livsl�ngd p� inl�gg:        %14#1d dag%#1?d%[%]%[ar%]\n")
    (keep-commented . "Minsta livsl�ngd f�r kommenterade inl�gg: %#1d dag%#1?d%[%]%[ar%]\n")
    (lowest-local-no ."L�gsta existerande lokala nummer: %8#1d\n")
    (highest-local-no . "H�gsta existerande lokala nummer: %8#1d\n")
    (last-text-time . "Tid f�r senaste inl�gg: %18#1s (st�r det i din cache)\n")
    (no-of-motd . "Lapp p� d�rren i text nummer: %12#1n\n")
    (pers-has-privileges .  "R�ttigheter: %#1s\n")
    (pers-has-privileges-2 .  "%28#1s")
    (superconf-is-no-name . "Superm�te:       %25#1m %#3s(%#2M)\n")
    (permitted-submitters-no-name . "Till�tna f�rfattare:%22#1m %#3s(%#2M)\n")
    (supervisor-is-no-name . "Organisat�r:     %25#1p %#3s(%#2P)\n")
    (presentation-no . "Presentation:    %25#1n\n")
    (conf-mship-priority . "Prioritet:       %25#1n%#2?b%[ %#2s%]%[%]\n")
    (conf-has-motd . "\n%#1M har en lapp p� d�rren:\n")
    (status-conf-generic . "%-40#1s %#2s\n")
    (status-aux-item .   "Ok�nd till�ggsinformation: %15#1s%#3s (skapad av %#2M)\n")
    (conf-mx-list-name . "Importerad mailinglista:                 %#1s %#2s\n")
    (conf-mx-refuse-import . "V�gra import av:                         %#1s %#2s\n")
    (recommended-conf-aux . "Rekommenderat m�te:                      %#1M <%#1m> %#2s\n")
    (bad-recommended-conf-aux . "Rekommenderat m�te:                     \"%<20...#1s\" %[%#2@[ogiltigt]%] %#3s\n")
    (status-read-faq-aux-item . "L�st FAQ:                 %15#2n f�r %#1?z%[%#1M <%#1m>%]%[servern%] %#3s\n")
    (bad-status-read-faq-aux-item . "L�st FAQ:                              \"%<20...#1s\" %[%#2@[ogiltigt]%] %#3s\n")
    (status-rejected-recommendation-aux-item . "Avvisad m�tesrekommendation:            %#1M %#2s\n")
    (bad-status-rejected-recommendation-aux-item . "Avvisad m�tesrekommendation:           \"%<20...#1s\" %[%#2@[ogiltigt]%] %#3s\n")
    (status-send-comments-to . "Dirigera om kommentarer till:           %#1M <%#1m> %#2s\n")
    (bad-status-send-comments-to . "Dirigera om kommentarer till:          \"%<20...#1s\" %[%#2@[ogiltigt]%] %#3s\n")

    (Everybody . "Alla")
    (show-members-list-also-q . "Vill du se medlemslistan ocks�? ")
    (show-membership-info-q . "Visa medlemskapsinformation och antalet ol�sta? ")
    (conf-has-these-members . "\n%#1M har f�ljande medlemmar:\n")
    (conf-has-no-members . "\n%#1M har inga medlemmar.\n")
    (member-list-header . "\nSenast inne         Osett   Namn\n")
    (secret-membership . "*** Hemlig rad ***\n")
    (conf-membership-line . "%#4@%#1s%#2M %#3s\n")
    (conf-membership-line-2 . "                            Adderad %#1s av %#2P\n")
    (pers-for-status . "Vem vill du se statusen f�r? ")
    (text-to-see-author-status-of . "Vilken texts f�rfattare vill du statusen f�r? ")
    (no-such-pers . "Det finns ingen s�dan person.\n")
    (pers-status-record . "Status f�r person %#1P (%#1p)\n")
    (created-time .  "Skapad:%34#1s\n\n")
    (created-confs . "Skapade m�ten:%27#1d\n")
    (created-persons . "Skapade personer:%24#1d\n")
    (created-texts . "Skapade texter:%26#1d\n")
    (created-lines . "Skapade rader:%27#1d\n")
    (created-chars . "Antal skapade tecken:%20#1d\n")
    (no-of-sessions . "Antal sessioner:%25#1d\n")
    (present-time-d-h-m-s . "N�rvarotid:%19#1d d %02#2d:%02#3d:%02#4d\n")
    (last-log-in . "Senaste in-/utloggning:%18#1s\n")
    (user-name . "Anv�ndare: %30#1s\n")

    (read-texts . "L�sta texter:%28#1d\n")
    (marked-texts . "Markerade texter:%24#1d\n")
    (time-for-last-letter . "Tid f�r senaste brev:%20#1s (st�r det i din cache)\n")
    (superconf . "Superm�te:%31#1m %#3s(%#2M)\n")
    (supervisor . "Organisat�r:%29#1p %#3s(%#2P)\n")
    (member-of-confs . "Medlem i (antal m�ten):%18#1d\n")
    (presentation . "Presentation:    %24#1n\n")
    (show-membership-list-also-q . "Vill du se vilka m�ten personen �r medlem i ocks�? ")
    (not-allowed-see-confs . "%#1P �r inte medlem i n�gra m�ten.\n")
    (is-member-of . "\n%#1P �r medlem i f�ljande m�ten:\n")
    (membership-list-header . "\nSenast inne           Osett  Namn\n")
    (pers-membership-line . "%#5@%#1s%#2s%#3M %#4s\n")
    (pers-membership-line-2 . "                             Adderad %#1s av %#2P\n")
    (is-supervisor-mark . "O ")
    (moronify-saving . "Sparar �ndringen...")
    (moronify-saving-done . "Sparar �ndringen...klart")
    (moronify-saving-error . "Kunde inte spara �ndringen!")
    (befriend-saving . "Sparar �ndringen...")
    (befriend-saving-done . "Sparar �ndringen...klart")
    (befriend-saving-error . "Kunde inte spara �ndringen!")
    (moronify-whom . "L�gg vem p� minuslistan? ")
    (befriend-whom . "L�gg vem p� pluslistan? ")
    (who-to-send-message-to . "Vem vill du skicka meddelandet till? (%#1s) ")
    (send-empty-message-p . "Meddelandet �r tomt. Vill du �nd� skicka det? ")
    (his-total-unread . "\n%#1M har totalt %#2d ol�sta (plus %#3d i passiva medlemskap).\n")
    (message-prompt . "Meddelande%#1?z%[%]%[ (till alla inloggade)%]: ")
    (message-sent-to-user . "\
%[%#3$\
================================================================
%]%[%#4$\
Ditt meddelande till %#2M (%#5s):

%#1t
%]%[%#3$\
----------------------------------------------------------------
%]")
    (message-sent-to-all . "\
%[%#3$\
================================================================
%]%[%#4$\
Ditt alarmmeddelande l�d (%#5s):

%#1t
%]%[%#3$\
----------------------------------------------------------------
%]")
    (message-sent-to-user-long . "\
%[%#3$\
===============================================================================
%]%[%#4$\
Ditt meddelande till %#2M (%#5s):

%#1t
%]%[%#3$\
-------------------------------------------------------------------------------
%]")
    (message-sent-to-all-long . "\
%[%#3$\
===============================================================================
%]%[%#4$\
Ditt alarmmeddelande l�d (%#5s):

%#1t
%]%[%#3$\
-------------------------------------------------------------------------------
%]")
    (message-use-alarm-instead . "Anv�nd kommandot %#1s f�r att skicka alarmmeddelanden.\n")
    (message-all-info . "\
%#1@S�nd alarmmeddelande till alla inloggade.

%[%#3F�verv�g att anv�nda \"%#2C\" f�r att skicka meddelandet till ett
specifikt m�te eller en enskild anv�ndare.%]
")
    (message-recipient-info . "S�nd meddelande till %#1M\n")
    (message-nope
     . "Det gick inte. %#3s\
Meddelandet du f�rs�kte s�nda till %#1M var:
%#2t\n")
    (only-last . "Endast l�sa senaste i %#1s: ")
    (initial-unread . "Initialt antal ol�sta (tomt f�r alla): ")
    (only-error . "N�got gick galet. Ledsen.\n")

    (lp--only-last . "Antal texter att l�sa: ")

    (session-list-unreads-in-confs . 
			      "Du har %#2?z%[%#2d ol�st%#2?d%[%]%[a%] brev och totalt %]%[%]%#3d ol�st%#3?d%[%]%[a%] i %#4d m�te%#4?d%[%]%[n%] i %#1s.\n")
    (session-list-no-unread-in . "Du har l�st ut %#1s.\n")

    (you-have-unreads . "Du har %#1d ol�st%#1?d%[%]%[a%] inl�gg i %#2M\n")
    (you-have-unreads-special . "Du har %#1d okommentera%#1?d%[t%]%[de%] inl�gg i %#2M\n")
    (you-have-no-unreads . "Du har inget ol�st i %#1M\n")
    (you-have-read-everything . "Du har sett alla nyheter.\n")
    (no-unreads-shown . "Hittade inga m�ten som matchade det kriteriet.\n")
    (total-unreads . "Du har totalt %#1d ol�st%#1?d%[%]%[a%] inl�gg i %#2d m�te%#2?d%[%]%[n%].\n")
    (shown-unreads . "Visade %#1d ol�st%#1?d%[%]%[a%] inl�gg i %#2d m�te%#2?d%[%]%[n%].\n")
    (list-unread-with-n-unread . "Visar endast m�ten med minst %#1d ol�sta.\n")
    (list-unread-with-at-most-n-unread . "Visar endast m�ten med som mest %#1d ol�sta.\n")
    (waiting-for-anything .
			  "Du v�ntar p� ett inl�gg i vilket m�te som helst.\n")
    (waiting-higher-than . 
			 "Du v�ntar p� ett inl�gg i ett m�te med h�gre prioritet �n %#1d.\n")

    (have-to-be-in-conf-with-unread . "Du m�ste g� till ett icketomt m�te f�rst.\n")
    (Texts . "Inl�gg")
    (Written . "Skrivet")
    (Lines . "Rader")
    (Author . "F�rfattare")
    (Subject . "�rende")
    (Comments . "Ko")
    (Num-marks . "Mk")
    (mark-type . "Markering")
    (mark-no . "Mrk")
    (could-not-read . "Du fick inte l�sa denna text (%#1n).\n")
    (multiple-choice . "Flera alternativ finns.")
    (does-not-exist . "Detta kommando finns inte.")
    (summary-line . "%=-8#1n%#2s%4#3d  %[%#4@%#5:P%]  %[%#6@%#7r%]\n")
    (diff-what-text-old . "Gammal text att j�mf�ra: ")
    (diff-what-text-new . "Ny text att j�mf�ra: ")


    ;; Only people fixing bugs or receiving bug reports should
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

    ;; Used for kom-is-person-member-of-conference:
    (pers-to-check-mship-for . "Vems medlemskap vill du unders�ka? ") ;-)
    (conf-to-check-mship-of . "...i vilket m�te? ")
    (conf-is-empty . "M�tet %#1M �r tomt.\n")
    (pers-is-member-of-conf . "Ja, %#1P �r medlem i m�tet %#2M.\n")
    (pers-is-passive-member-of-conf . "Nja, %#1P �r bara passiv medlem i m�tet %#2M.\n")
    (pers-is-not-member-of-conf . "Nej, %#1P �r inte medlem i m�tet %#2M.\n")
    (pers-is-member-of-conf-2 . "Antal ol�sta: %#2D (senast inne %#1s)\n")
    (pers-will-receive-async . "Tar %#1?b%[%]%[inte %]emot gruppmeddelanden till m�tet\n")
    (pers-mship-priority . "Medlemskapets prioritet: %#1d\n")
    (Unknown-number . "Ok�nt antal")
    (text-to-check-will-read-for . "...f�r medlemskap i mottagare till inl�gg: ")
    (pers-to-check-will-read-for . "Vilken person vill du kontrollera: ")
    (pers-is-member-of-rcpt . "Ja, %#1P �r medlem i minst en mottagare till inl�gg %#2n.\n")
    (pers-is-passive-member-of-rcpt . "Nja, %#1P �r bara passiv medlem n�gon mottagare till %#2n.\n")
    (pers-is-not-member-of-rcpt . "Nej, %#1P �r inte medlem i n�gon mottagare till %#2n.\n")

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
    (new-garb-nice-q . "Efter hur m�nga dagar ska inl�gg utpl�nas? ")
    (new-keep-commented-q . "Hur m�nga dagar ska nya kommentarer skydda det kommenterade? ")
    (garb-nice-for-is . "�ndrar livsl�ngden f�r %#1M till %#2d dag%#2?d%[%]%[ar%]...")
    (keep-commented-for-is . "�ndrar s� kommentarer skyddar fr�n utpl�ning i %#2d dag%#2?d%[%]%[ar%] f�r %#1M...")

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
    (who-to-throw-out . "Vems session vill du kasta ut? ")
    (throwing-out . "Kastar nu ut session %#1d... ")
    (postpone-prompt . "Hur lite vill du l�sa nu? ")
    (set-session-priority . "S�tt l�sniv�: ")

    ;; From review.el:
    (no-unread-done . "Du m�ste markera inl�gg ol�sta innan du kan markera fler ol�sta.\n")
    (no-review-done . "Du m�ste �terse innan du kan �terse mer.\n")
    (not-reviewing . "Du h�ller inte p� att �terse n�got nu.\n")
    (unread-how-many . "Ol�smarkera hur m�nga?")
    (unread-how-many-roots . "Ol�smarkera hur m�nga urinl�gg?")
    (review-how-many . "�terse hur m�nga?")
    (review-how-many-roots . "�terse hur m�nga urinl�gg?")
    (unread-how-many-more . "Ol�smarkera ytterligare hur m�nga?")
    (review-how-many-more . "�terse ytterligare hur m�nga?")
    (latest-n . "senaste %#1d")
    (first-n . "f�rsta %#1d")

    (review-by-whom . "�terse %#1s %#2?b%[%#2s %]%[%]av vem: ")
    (review-to-conf . "�terse %#1s %#2?b%[%#2s %]%[%]till m�te: ")
    (unread-by-whom . "Ol�smarkera %#1s %#2?b%[%#2s %]%[%]av vem: ")
    (unread-to-conf . "Ol�smarkera %#1s %#2?b%[%#2s %]%[%]till m�te: ")
    (all-confs . "alla m�ten")

    (unread-info-by-to . "Ol�smarkera %#1s %#4?b%[%#4s %]%[%]av %#2P till %#3M.\n")
    (unread-more-info-by-to . "Ol�smarkera %#1s %#4?b%[%#4s %]%[%]av %#2P till %#3M.\n")
    (unread-rest . "resten")
    (unread-more . "n�sta %#1d")

    (review-info-by-to . "�terse %#1s  %#4?b%[%#4s %]%[%]av %#2P till %#3M.\n")
    (review-more-info-by-to . "�terse %#1s %#4?b%[%#4s %]%[%]av %#2P till %#3M.\n")
    (review-filter-roots . "urinl�gg")

    (review-rest . "resten")
    (review-more . "n�sta %#1d")
    (you-review . "Du �terser nu %#1s.\n")
    (read-text-first . "Du m�ste l�sa en text f�rst.\n")
    (cannot-read-last-text . "Du kan inte l�sa den senast l�sta texten.\n")
    (review-n-texts . "�terse %#1d inl�gg.\n")
    (review-marked . "�terse %#1d markerade.\n")
    (review-text-no . "�terse text nummer %#1n\n")
    (review-many-comments . "�terse %#2?d%[en%]%[%#2d%] kommentar%#2?d%[%]%[er%] till inl�gg %#1n.\n")
    (view-many-comments . "L�s %#2?d%[en%]%[%#2d%] kommentar%#2?d%[%]%[er%] till %#1n.\n")
    (view-texts-in-conf . "Totalt %#1d inl�gg att l�sa i %#2M.\n")
    (not-reading-anywhere . "Du l�ser inte i n�got m�te.\n")
    (read-normally-read . "Hur m�nga vill du se igen? ")

    (review-conf-gone . "M�tet finns inte.\n")
    (review-pers-gone . "Personen finns inte.\n")
    (review-cant-read-conf . "Du kan inte �terse inl�gg till ett slutet m�te du inte �r med i.\n")
    (review-cant-read-letterbox . "Du kan inte �terse inl�gg till n�gon annans brevl�da.\n")
    (review-cant-read-empty . "M�tet �r tomt.\n")
    (cant-review-everything . "Du kan inte �terse alla texter i LysKOM.\n")
    (cant-unread-everything . "Du kan inte ol�smarkera alla texter i LysKOM.\n")

    (more-than-one-root . "Inl�gg %#1n har mer �n ett urinl�gg.\n")
    (more-than-one-root-review . 
"Inl�gg %#1n har mer �n ett urinl�gg, men endast ett tr�d kommer att visas.\n")

    ;; From edit-text.el:
    (press-C-c-C-c . "Tryck C-c C-c f�r att skicka in texten.")
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
    (inherit-aux-flag  . "�rvd")
    (aux-item-prefix . "[*]")
    (aux-item-prefix-regexp . "\\[\\*\\]\\s-*")
    (comment-item-prefix . "#\\s-*")
    (text-no-comment . "%#1d %#2s /%#3d rad%#3?d%[%]%[er%]/ %#4P %#5?b%[ [anonymt]%]%[%]\n")
    (cant-fcc-text-file-error . "Kan inte spara inl�gg %#1n till \"%#2s\" (%#3s: %#4s).\n")
    (cant-fcc-text . "Kan inte spara inl�gg %#1n till \"%#2s\" (%#3s).\n")

    (header-subject . "�rende: ")
    (header-separator . "\\<lyskom-edit-mode-map>\
--- Skriv nedan. \
Skicka in=\\[kom-edit-send], \
Avbryt=\\[kom-edit-quit], \
Annat se \\[describe-mode] ---")
    (text-mass . "%#4s%#1s%[%#6@\n%]%[%#5@%#2s%]%[%#7@\n%]%#3s")
    (comment-to-by . "%#1s till text %#2n%#3s.\n")
    (already-sent . "Du har redan skickat denna text en g�ng. S�nd �nd�? ")
    (subject . "�rende: ")
    (subject-prefix . "[��\\{\\[]")
    (enter-subject-idi . "Skriv ett �rende.")
    (which-text-include . "Vilken text skall vi inkludera? ")
    (added-recipient . "Mottagare som skall adderas: ")
    (added-carbon-copy . "Extra kopia till m�te: ")
    (added-blank-carbon-copy . "Dold kopia till: ")
    (text-to-comment-q . "Vilket inl�gg vill du kommentera? ")
    (conf-has-motd-no . "M�tet har en lapp p� d�rren. (%#1d)\n\n%#2s")
    (still-want-to-add . "Vill du fortfarande addera m�tet? ")
    (could-not-create-text . "\nTexten kunde ej skapas. %#2s.\n")
    (no-get-text . "Du fick inte h�mta texten.")
    (unknown-header . "Ok�nd information p� raden")
    (transform-error . "Skicka in oformatterat (%#1s)? ")
    (cant-find-info-node . "Hittar inte info-bufferten")

    (link-type . "Vad vill du l�nka till? ")
    (which-text-to-link . "L�gg in l�nk till text nummer: ")
    (which-text-to-link-err . "Hittar inte texten. L�gg in l�nk till text nummer: ")
    (which-pers-to-link . "L�gg in l�nk till person: ")
    (which-conf-to-link . "L�gg in l�nk till m�te: ")

    ;; From view-text.el:
    (view-text-first-line . "%#7$%#2@%#1n %#3s /%#4d rad%#4?d%[%]%[er%]/ %#5P%#6?b%[%#6s%]%[%]\n")

    (marked-by-you . "Markerad av dig (typ: %#1s).\n")
    (marked-by-you-and-others . "Markerad av dig (typ: %#2s) och %#1?d%[n�gon annan%]%[%#1d andra%].\n")
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
    (yesterday . "ig�r")

    (no-such-text-no . "Det finns inget s�dant inl�gg. (%#1:n)\n")
    (text-created-at . "Skapad: %#1s\n")
    (text-imported-at . "Importerad: %#1s\n")
    (text-imported-at-by . "Importerad: %#1s av %#2P\n")

    (head-Subject . "�rende: ")
    (Recipient . "Mottagare")
    (Extra-recipient . "Extra kopia")
    (Hidden-recipient . "Dold kopia")
    (mx-Recipient . "Extern mottagare")
    (mx-Extra-recipient . "Extern kopiemottagare")
    (mx-Extern-reply-to . "Externa svar till")
    (Strange-recipient . "Underlig mottagare")
    (send-at . "    S�nt:     %#1s\n")
    (sent-by . "    S�nt av %#1P\n")
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
    
    (envelope-sender . "S�nt av: %#1s\n")
    (attachment-filename . "Bilagans filnamn: \"%#1s\"\n")
    (message-id . "Meddelande-ID: %#1s\n")

    (written-by . " av %#1P\n")

    ;; From async.el:

    (name-has-changed-to-name . "%#1:P har nu bytt namn till %#2:P")
    (name-has-changed-to-name-r . "%#4@%[%#3@%#1:P%] har nu bytt namn till %[%#3@%#2:P%]\n")
    (you-changed-name-to . "Nu har du bytt namn till %[%#2@%#1:P%].\n")
    (database-sync . "Databasen synkas.")

    (lyskom-is-full . "\
===========================================================
Meddelande fr�n LysKOM-systemet: N�gon f�rs�kte koppla upp,
men misslyckades eftersom alla tillg�ngliga f�rbindelser �r
upptagna. Logga ut och kom tillbaks senare om du v�ntar nu.
===========================================================\n")
    (lyskom-is-full-long . "\
===============================================================================
Meddelande fr�n LysKOM-systemet: N�gon f�rs�kte koppla upp,
men misslyckades eftersom alla tillg�ngliga f�rbindelser �r
upptagna. Logga ut och kom tillbaks senare om du v�ntar nu.
===============================================================================
")
    (has-entered . "Nu har %#1:P g�tt in i %#2s.")
    (has-entered-r . "%#2@Nu har %#1P g�tt in i %#3s%#6?b%[ fr�n %#6s%]%[%]%#4?b%[ (%#4s %#5s)%]%[%].\n")
    (has-left . "Nu har %#1:P g�tt ur %#2s.")
    (has-left-r . "%#2@Nu har %#1P g�tt ur %#3s (%#4s %#5s).\n")
    (Unknown . "Ok�nd")
    (unknown . "ok�nd")
    (Unknown2 . "Ok�nt")
    (unknown-person . "Ok�nd person")

    (no-longer-member . "Du �r inte l�ngre medlem i %#1M.\n")
    (no-longer-member-n . "Du �r inte l�ngre medlem i m�te %#1m (finns inte).\n")

    (have-become-member . "Du har blivit medlem i %#1M.\n")
    (have-become-invited-member . "Du har bjudits in till %#1M.
G� till m�tet f�r att acceptera eller tacka nej till inbjudan. Du kan �ven
anv�nda kommandot 'Bli medlem i m�te' f�r att acceptera inbjudan.\n")
    (have-become-passive-member . "Du har blivit passiv medlem i %#1M.\n")

    (message-broadcast . "\
%[%#4$\
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#5$\
Alarmmeddelande fr�n %#1P (%#3s):

%#2t
%]%[%#4$\
----------------------------------------------------------------
%]")
    (message-broadcast-long . "\
%[%#4$\
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#5$\
Alarmmeddelande fr�n %#1P (%#3s):

%#2t
%]%[%#4$\
-------------------------------------------------------------------------------
%]")
    (message-from . "\
%[%#4$\
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#5$\
Personligt meddelande fr�n %#1P (%#3s):

%#2t
%]%[%#4$\
----------------------------------------------------------------
%]")
    (message-from-long . "\
%[%#4$\
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#5$\
Personligt meddelande fr�n %#1P (%#3s):

%#2t
%]%[%#4$\
-------------------------------------------------------------------------------
%]")
    (message-from-to . "\
%[%#5$\
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#6$\
Gruppmeddelande till %#3M\nfr�n %#2P (%#4s):

%#1t
%]%[%#5$\
----------------------------------------------------------------
%]")
    (message-from-to-long . "\
%[%#5$\
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#6$\
Gruppmeddelande till %#3M\nfr�n %#2P (%#4s):

%#1t
%]%[%#5$\
-------------------------------------------------------------------------------
%]")
    (text-is-created . "Text %#1n �r skapad!")

    ;; Used in mode-line-process
    (mode-line-waiting . ": v�ntar")
    (mode-line-working . ": arbetar")
    (mode-line-saving . ": sparar")
    (mode-line-down . ": nerkopplad")

    ;; From completing-read.el:
	   
    (person-or-conf-no-regexp . "\\`[ \t]*[mpMP]\\w*[ \t]+\\([0-9]+\\)\\'")
    (session-no-regexp . "\\`[ \t]*[sS]\\w*[ \t]+\\([0-9]+\\)\\'")
    (conf-prompt . "Vilket m�te/person? ")

    ;; From flags.el:
    (saving-settings . "Sparar inst�llningarna...")
    (saving-settings-done . "Sparar inst�llningarna...klart")
    (save-options-failed-internal . "\
Det gick inte att spara inst�llningarna p� grund av ett internt fel.

Globala inst�llningar:         %#1?b%[Kodningen misslyckades%]%[OK%]
Elispklientens inst�llningar:  %#2?b%[Kodningen misslyckades%]%[OK%]
Andra klienters inst�llningar: %#3?b%[Kodningen misslyckades%]%[OK%]

Rapportera g�rna detta fel till elispklientens utvecklare.
")
    (hang-on . "V�nta ett tag...\n")
    (no-changes . "Ingenting beh�vde sparas eftersom inga variabler hade �ndrats.\n")
    (could-not-save-options . "Kunde ej spara inst�llningarna.\n")
    (could-not-create-area . "Kunde ej skapa texten.\n")
    (could-not-set-user-area . "Kunde ej st�lla om user-arean. Servern s�ger felmeddelande: %#1d\n")
    (you-dont-exist . "Du finns inte.\n")
    (error-in-options . "Det fanns ett fel i en av dina variabler (%#1s)
Det stod \"%#2s\" i user-arean. Den s�tts till nil ist�llet.
Skicka en bugrapport.\n")
    (error-in-options-short . "Det fanns ett fel i dina inst�llningar. Vissa inst�llningar har inte l�sts in.")
    (reading-settings-from-server . "Dina sparade inst�llningar har �ndrats p� servern. L�ser om inst�llningarna.\n")

    ;; From elib-string.el:
    ;; No entries.
		
    ;; From lyskom-rest.el:
		
    (mode-line-unread . " Ol�sta ")
    (mode-line-letters . "brev ")
    (frame-title-unread . "Ol�sta")
    (frame-title-letters . " brev")
    (sessions-with-unreads . "Sessioner med ol�sta")
    (unread-letters . "ol�sta brev")

    (bad-text-no-prefix . "Kan inte �vers�tta prefix `%s' till inl�ggsnummer")
    (prefix-arg-try-again . "Ange en annan text eller tryck control-g f�r att avbryta.\n")
    (error-code . "%#2?+%[Felkod %#2d/%#3S: %]%[%]%#1s.\n")
    (error-in-kom-do-when-done . "Variabeln kom-do-when-done har ett felaktigt v�rde.
Du b�r s�tta den till ett b�ttre v�rde.\n")
    (extended-command . "LysKOM: ")
    (wait-for-server . "LysKOM v�ntar p� svar fr�n servern. V�nta tills du f�r en prompt.\n")
    (review-text-q . "�terse text nummer: ")

    (completely-read-conf . "Du har sett alla texter i detta m�te.\n")
    (not-in-any-conf . "Du l�ser inte n�got m�te just nu.\n")

    (all-conf-unread-r . "Du har l�st ut alla m�ten.\n")
    (all-conf-unread-s . "Du har l�st ut alla m�ten. ")
    (enter-conf-unread . "%#1d ol�st%#1?d%[%]%[a%]")
    (enter-conf-unread-faq . "%#1d ol�st%#1?d%[%]%[a%] FAQ")

    (save-one-on-file-q . "Arkivera inl�gg %#1n till fil: ")
    (save-many-on-file-q . "Arkivera %#1d inl�gg till fil: ")
    (saving-one-on-file . "Arkiverar inl�gg %#1n i %#2s.\n")
    (saving-many-on-file . "Arkivera %#1d inl�gg i %#2s.\n")
    (save-text-to-file-q . "Spara inl�ggstext %#1n p� fil: ")
    (save-text-confirm . "Filen %#1s finns redan. Vill du skriva �ver den? ")
    (saving-text . "Sparar inl�gg %#1n som %#2s...")
    (what-save-no . "Vilket inl�gg vill du spara? ")
    (wait-for-prompt . "V�nta p� prompten.")

    (conference-no . "<m�te %#1d>")
    (person-no . "<person %#1d>")
    (prompt-several-messages . "(%d meddelanden)")
    (prompt-single-message   . "(%d meddelande)")

    (text-buffer-missing . "Inl�ggsbufferten existerar inte l�ngre.\n")

    (re-edit-text-prompt . "Redigera texten som inte kunde skapas")
    (go-to-pri-conf-prompt . "G� till n�sta prioriterade m�te")
    (go-to-pri-mailbox-prompt . "G� till din brevl�da (prioriterad)")
    (read-pri-text-conf . "L�sa n�sta prioriterade text")
    (read-pri-letter-prompt . "L�sa n�sta brev (prioriterat)")
    (review-next-text-prompt . "�terse n�sta text")
    (review-next-comment-prompt . "�terse n�sta kommentar")
    (review-next-marked-prompt . "�terse n�sta markerade")
    (review-next-faq-prompt . "�terse n�sta FAQ")
    (read-next-letter-prompt . "L�sa n�sta brev")
    (read-next-footnote-prompt . "L�sa n�sta fotnot")
    (read-next-comment-prompt . "L�sa n�sta kommentar")
    (read-next-text-prompt . "L�sa n�sta text")
    (read-next-attachment-prompt . "L�sa n�sta bilaga")
    (go-to-conf-of-marked-prompt . "�teruppta �terse markerade")
    (go-to-conf-of-review-tree-prompt . "�teruppta �terse kommentarer")
    (go-to-conf-of-review-prompt . "�teruppta �terse")
    (go-to-conf-of-review-faq-prompt . "�teruppta �terse FAQ")
    (go-to-next-conf-prompt . "G� till n�sta m�te")
    (go-to-your-mailbox-prompt . "G� till din brevl�da")
    (next-pri-session-prompt . "G� till prioriterat LysKOM \"%#1s\"")
    (next-unread-session-prompt . "G� till LysKOM \"%#1s\"")

    (no-such-kom-session . "Det finns ingen s�dan session med ol�sta.\n")
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
    (conference-does-not-exist . "M�te %#1d (finns inte).")
    (conf-no-does-not-exist-r . "M�te %#1d finns inte.")
    (person-is-anonymous . "Anonym person")

    (process-signal . "Signal fr�n processen.")
    (closed-connection . "
**************************************************
%#2s
LysKOM-sessionen onormalt st�ngd.
Felmeddelande: %#1s**************************************************")
    (closed-connection-other-buf . "\
**************************************************
LysKOM-sessionen %#1s st�ngdes onormalt.
**************************************************
")
    (dead-session . "LysKOM-sessionen �r inte aktiv.")
    (resurrect-session . "LysKOM-sessionen har kopplats ner, vill du ansluta igen? ")
    (not-lyskom-buffer . "Detta �r inte en aktiv LysKOM-session.")
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
    (comment-prefix . "\\([Kk]\\)")
    (footnote . "Fotnot")
    (footnote-prefix . "\\([Ff]\\)")

    (by . " av %#1P")
    (text-created .  "Text nummer %#1n �r skapad.\n")
    (text-created-anonymous .  "\
Text nummer %#1n �r skapad (anonymt). F�r att g�ra det sv�rare f�r 
andra att ta reda p� att du skapade inl�gget b�r du v�nta ett tag med
att l�sa det.\n")

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
    (filter-recipient . "Vilken mottagare vill du filtrera? ")
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
    (filter-edit-footer . "
 Upp rad:    p  Ny rad:          i    Ta bort rad:     d
 Upp filter: P  Nytt filter:     M-i  Ta bort filter:  M-d
 Ned rad:    n  Spara �ndringar: s    �ndra handling:  a
 Ned filter: N  �ngra �ndringar: g    �ndra permanens: t
 Avsluta:    q                        Mer hj�lp:       C-h m
")
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
    (ansaphone-message-long . "Svarsmeddelande:
-------------------------------------------------------------------------------
%#1t
-------------------------------------------------------------------------------
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
    (remote-set-message-long . "Fj�rrstyrning (%#1P %#2s): Svarsmeddelande:
-------------------------------------------------------------------------------
%#3t
-------------------------------------------------------------------------------
")    (remote-set-ansaphone . "Fj�rrstyrning (%#1P %#2s): Automatsvar �r nu %#3s\n")
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
    (timestamp-popup-title . "Tidsst�mpel %#1s")
    (recpt-type-popup-title . "Mottagartyp: %#1s")
    (add-recpt-button-text . "[Addera...]")
    (add-recpt-button-text-regex . "\\[Addera\\.\\.\\.\\]")
    (generic-popup-title . "%#1s")

    (who-i-am-not-present . "%#1P �r inte n�rvarande i n�got m�te\n")
    (who-i-am-present . "%#1P �r n�rvarande i %#2M\n")
    (who-i-am-client . "Programmet heter lyskom.el, version %#1s%#2?b%[ (MULE)%]%[%].\n")
    (who-i-am-server . "Detta �r %#1s, version %#2s.\n")
    (who-i-am-emacs . "Det k�rs under %#1s%#2?b%[ (MULE)%]%[%].\n")

    (no-such-session-r . "Det finns ingen s�dan session. Personen kanske inte �r inloggad.\n")
    (person-not-logged-in-r . "%#1P %#2?b%[har inte varit inloggad sedan %#2s%]%[�r inte inloggad%].\n")
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
    (years . "�r")
    (year . "�r")
    (months . "m�nader")
    (month . "m�nad")
    (days . "dagar")
    (day . "dag")
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

    ;; From slow.el
    (no-such-command . "Det finns inget s�dant kommando.\n")
    (command-completions . "Du kan mena n�gon av f�ljande:\n %#1s\n")
    (which-language . "�ndra spr�k till (change language to): ")
    (send-formatted . "Skicka in som formatterad text? ")
    (changing-language-to . "Byter till %#1_s.\n")
    (language-set-to . "Spr�ket �r %#1_s%#2?b%[ %#3@[%#2s]%]%[%].\n")
    (language-not-loaded . "%#1s finns inte tillg�ngligt.\n")

    (reformat-html . "HTML")
    (reformat-enriched . "enriched")
    (reformat-filled . "ombruten")
    (reformat-image . "bild")
    (image-no-show . "< Bilden (%#1s) kan inte visas. >")
    (reformat-video . "video")
    (video-no-show . "< Video (%#1s) st�ds inte. >")
    (reformat-audio . "ljud")
    (audio-no-show . "< Ljud (%#1s) st�ds inte. >")
    (reformat-multipart . "flerdelat")
    (multipart-no-show . "< Flerdelade inl�gg (%#1s) st�ds inte. >")
    (reformat-message . "meddelande")
    (message-no-show . "< Meddelandetyp (%#1s) st�ds inte. >")
    (reformat-model . "modell")
    (model-no-show . "< Modeller (%#1s) st�ds inte. >")
    (reformat-application . "till�mpningsdata")
    (application-no-show . "< Till�mpningsdata (%#1s) st�ds inte. >")
    (reformat-unknown . "ok�nd typ")
    (unknown-no-show . "< Ok�nd typ (%#1s) st�ds inte. >")

    (reformat-truncated . "nedkortad")

    (reformat-signature . "avsignerad")

    (reformat-deswascii . "var swascii")

    (reformat-rot13 . "rot13")

    (reformat-utf-8 . "var UTF-8")

    (need-library . "Paketet \"%#1s\" beh�vs f�r att utf�ra detta kommando.\n")
    (calc-expression . "Uttryck: ")

    (do-send-anonymous . "Skicka in texten anonymt? ")
    (remove-self-sending-anonymous . "Vill du ta bort dig sj�lv som mottagare fr�n inl�gget? ")
    (anonymous . "anonym")
    (Anonymous . "Anonym")

    (secret-conf-letter . "H")
    (protected-conf-letter . "S")
    (created-conf-letter . "S")
    (superconf-conf-letter . "M")
    (supervisor-conf-letter . "O")

    ;; Some Help

    (where-is-doesnt-exist . "Kommandot %#1s finns inte")
    (where-is-on-no-key . "%#1s finns inte p� n�gon tangent")
    (where-is-on-key . "%#1s finns p� %#2s")

    ;; From aux-items.el

    (content-type-aux . "Inneh�llstyp: %#1s")
    (content-type-regexp . "Inneh�llstyp: \\(\\S-+\\)")
    
    (agreeing . "H�ller med om inl�gg %#1n...")
    (fast-replying . "Anm�rkning till inl�gg %#1n...")
    (author-fast-replies . "Anm�rkningar av f�rfattaren:")
    (author-fast-reply-aux . "  \"%#1t\"")
    (other-fast-replies . "Anm�rkningar:")
    (other-fast-reply-aux . "  \"%#1t\" /%#2P/")
    (fast-reply-too-long . "Kan inte skapa anm�rkningar som best�r av mer �n en rad.\n")
    
    (conf-to-list-faqs . "Lista FAQ f�r vilket m�te: ")
    (faq-for-conf-aux . "Texten �r FAQ f�r %#1M <%#1m>")
    (bad-faq-for-conf-aux . "Texten �r FAQ: \"%<20...#1s\" %[%#2@[ogiltigt]%] %#3s")
    (faq-for-server-aux . "Texten �r FAQ f�r LysKOM-servern") 
    (faq-in-text-aux . "FAQ i inl�gg:                   %10#1n %#2D")
    (bad-faq-in-text-aux . "FAQ i inl�gg:                            \"%<20...#1s\" %[%#2@[ogiltigt]%] %#3s")
    (conf-to-add-faq . "Vilket m�te vill du l�gga till en FAQ f�r? ")
    (text-to-add-as-faq . "Vilken text vill du l�gga till som FAQ? ")
    (text-to-change-as-faq . "Vilken FAQ vill du �ndra? ")
    (adding-faq . "L�gger till inl�gg %#1n som FAQ f�r %#2?b%[%#2M%]%[servern%]...")
    (conf-to-del-faq . "Vilket m�te vill du ta bort en FAQ fr�n? ")
    (text-to-del-as-faq . "Vilken text vill du ta bort som FAQ? ")
    (deleting-faq . "Tar bort inl�gg %#1n som FAQ f�r %#2?b%[%#2M%]%[servern%]...")
    (conf-has-no-faq . "%#1?b%[%#1M%]%[Servern%] har ingen FAQ\n")
    (unread-which-faq . "Ol�smarkera FAQ f�r vilket m�te? ")
    (view-which-faq . "�terse FAQ f�r vilket m�te? ")
    (review-faq-for-r . "�terse FAQ f�r %#1?b%[%#1M%]%[servern%].\n")
    (set-faq-for-conf-done . "Text %#3n �r nu FAQ f�r %#1?b%[%#1M%]%[servern%].\n")
    (set-faq-for-conf-failed . "Kunde inte s�tta FAQ f�r %#1?b%[%#1M%]%[servern%] till text %#3n.\n")
    (changed-faq-for-conf-done . "FAQ i text %#2n f�r %#1?b%[%#1M%]%[servern%] �ndrad till text %#3n.\n")
    (changed-faq-for-conf-failed . "Kunde inte �ndra FAQ f�r %#1?b%[%#1M%]%[servern%] till text %#3n.
%#2?b%[Text %#2n �r fortfarande FAQ.
%]%[%]%#4s")
    (faq-in-text . "FAQ i text %#1n %#3s%#4s")
    (faq-in-text-by . "FAQ i text %#1n %#5s %#3sav %#2P %#4s")
    (server-has-new-faq . "\nDet finns %#1d ny%#1?d%[%]%[a%] FAQ till servern.\n")
    (unread-faqs-header . "\nOl�st%#1?d%[%]%[a%] FAQ till %#2?b%[%#2M%]%[servern%]:\n")
    (all-faqs-header . "Alla FAQ till %#1?b%[%#1M%]%[servern%]:\n")

    (too-many-languages . "F�r m�nga teckenupps�ttningar f�r att koda. Skicka in okodat? ")
    (too-many-content-types . "Kan inte avg�ra inneh�llstyp. F�renkla inl�gget.")

    (cross-reference-text-status-aux . "Se �ven inl�gg:                 %10#1n /%#2P/")
    (cross-reference-conf-status-aux . "Se �ven m�te:                           <%#1m> %#1M /%#2P/")
    (cross-reference-pers-status-aux . "Se �ven person:                         <%#1p> %#1P /%#2P/")
    (strange-cross-reference-status .  "Se �ven:                                 %#1s (vad nu det betyder)")
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
    
    (request-confirmation-aux . "%#1P har beg�rt l�sbekr�ftelse")
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

    (creating-software-aux . "Skapad med %#1s")
    (world-readable-text-aux . "Inl�gget kan l�sas utan inloggning")
    (world-readable-text-edit-aux . "G�r inl�gget l�sbart utan inloggning")

    (cant-get-aux-item . "Hittar inte till�ggsinformationen\n")
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
    (aux-item-for-conference-no . "m�te <%#1m> %#1M")
    (aux-item-for-text-no . "inl�gg %#1n")
    (aux-item-for-server . "servern")

    (what-fast-reply-no . "Anm�rkning till vilket inl�gg? ")
    (fast-reply-prompt . "Anm�rkning: ")
    (agree-prompt . "Text: ")
    (default-agree-string . "H�ller med")
    (what-agree-no . "H�ll med vilket inl�gg? ")

    (what-kind-to-add-aux-to . "Koppla till�ggsinformation till vad? ")
    (which-conf-to-add-aux-to . "Vilket m�te vill du koppla till? ")
    (which-text-to-add-aux-to . "Vilket inl�gg vill du koppla till? ")
    (which-aux-item-tag . "Aux-item tag: ")
    (which-aux-item-inherit . "S�tt inherit-flaggan? ")
    (which-aux-item-secret . "S�tt secret-flaggan? ")
    (which-aux-item-anonymous . "S�tt anonymous-flaggan? ")
    (which-aux-item-rsv1 . "S�tt dont-garb-flaggan? ")
    (which-aux-item-rsv2 . "S�tt reserved2-flaggan? ")
    (which-aux-item-rsv3 . "S�tt reserved3-flaggan? ")
    (which-aux-item-rsv4 . "S�tt reserved4-flaggan? ")
    (which-aux-item-inherit-limit . "Arvsgr�ns: ")
    (which-aux-item-data . "Data: ")

    ;; Cross-reference stuff

    (Person . "Person")
    (person . "person")
    (persons . "personer")
    (Conference . "M�te")
    (conference . "m�te")
    (conferences . "m�ten")
    (Text . "Inl�gg")
    (text . "inl�gg")
    (texts . "inl�gg")
    (Server . "Server")
    (server . "server")
    (servers . "servrar")
    (abc-conference . "mM�te")
    (abc-person . "pPerson")
    (abc-text . "iInl�gg")

    (xref-type . "Vad vill du referera till? ")
    (which-text-to-xref . "L�gg till referens till text nummer: ")
    (which-text-to-xref-err . "Hittar inte texten. L�gg till referens till text nummer: ")
    (which-pers-to-xref . "L�gg till referens till person: ")
    (which-conf-to-xref . "L�gg till referens till m�te: ")

    ;; Aux-item stuff from edit-text

    (no-comments-q . "F�rfattaren vill inte ha kommentarer. Kommentera �nd�? ")
    (private-answer-q . "F�rfattaren har bett om personliga svar. Skriv personligt svar? ")

    (your-invited . "%#2P har bjudit in dig till %#1M.
Genom att acceptera inbjudan forts�tter du vara medlem i %#1M.
Du kan �ven tacka nej, och d� kommer du automatiskt att g� ur m�tet.
")

    (accept-invitation . "Vill du acceptera inbjudan och forts�tta vara medlem? ")
    (enter-passive . "Du �r passiv medlem i %#1M.
Du m�ste bli aktiv medlem f�r att g� till m�tet.\n")
    (convert-passive . "Vill du bli aktiv medlem i %#1M? ")
    (bug-secret-mship . "Du �r hemlig medlem i %#1M\n")

    (invitation-mt-type . "inbjuden")
    (passive-mt-type . "passiv")
    (secret-mt-type . "hemlig")
    (message-flag-off-mt-type . "ej gruppmeddelanden")
    (message-flag-on-mt-type . "gruppmeddelanden")

    (Invitation-mt-type . "Inbjuden")
    (Passive-mt-type . "Passiv")
    (Secret-mt-type . "Hemlig")
    (Message-flag-mt-type . "Gruppmeddelanden")

    (not-author . "Du �r inte f�rfattare till text %#1n.")
    (not-author-try-anyway-p . "Du �r inte f�rfattare till inl�gget. Vill du f�rs�ka �nd�? ")
    (what-no-comments-no . "Vilken text vill du inte ha kommentarer till: ")
    (adding-no-comments . "Beg�r att inte f� kommentarer till inl�gg %#1n...")
    (already-no-comments . "Inl�gg %#1n har redan beg�ran om inga kommentarer.\n")

    (what-private-answer-no . "Vilken text vill du ha personligt svar till: ")
    (adding-private-answer . "Beg�r att endast f� personligt svar till inl�gg %#1n...")
    (already-private-answer . "Inl�gg %#1n har redan beg�ran om enbart personliga svar.\n")

    (what-request-confirm-no . "Vilken text vill du ha l�sbekr�ftelse till: ")
    (adding-request-confirm . "Beg�r l�sbekr�ftelse till inl�gg %#1n...")
    (already-request-confirm . "Inl�gg %#1n har redan beg�ran om l�sbekr�ftelse.\n")

    (review-mail-headers-to-what . "Vilken texts brevhuvud vill du se? ")
    (no-mail-headers . "Inl�gg %#1n har inget brevhuvud\n")
    (mail-headers-for . "Brevhuvud f�r inl�gg %#1n:\n")
    (email-name-prefix . "")
    (email-name-suffix . "")

    (you-are-anonymous . "Du �r nu n�gorlunda anonym.\n")
    (you-are-nonanonymous . "Du �r inte l�ngre anonym.\n")
    (you-are-already-anonymous . "Du �r redan n�gorlunda anonym.\n")
    (you-are-already-nonanonymous . "Du �r redan icke-anonym.\n")

    (start-keep-alive . "S�nder data med %#1d sekunders mellanrum f�r att h�lla f�rbindelsen aktiv.")
    (stop-keep-alive . "Data s�nds inte l�ngre f�r att h�lla f�rbindelsen aktiv.")

    (review-converted-q . "�terse vilket inl�gg konverterat?")
    (review-noconversion-q . "�terse vilket inl�gg omodifierat?")
    (review-rot13-q . "�terse vilket inl�gg i rot13?")
    (unread-commented-q . "Ol�smarkera vilket inl�ggs kommenterade?")
    (review-commented-q . "�terse vilket inl�gg kommenterade?")
    (unread-tree-q . "Ol�smarkera alla kommentarer rekursivt f�r vilket inl�gg?")
    (review-tree-q . "�terse alla kommentarer rekursivt f�r vilket inl�gg?")
    (unread-root-q . "Ol�smarkera urinl�gget f�r vilken text?")
    (find-root-q . "�terse urinl�gget f�r vilken text?")
    (unread-root-review-q . "Ol�smarkera tr�d f�r vilket inl�gg?")
    (find-root-review-q . "�terse tr�d f�r vilket inl�gg?")
    (unread-comments-q . "Ol�smarkera alla kommentarer till vilket inl�gg?")
    (review-comments-q . "�terse alla kommentarer till vilket inl�gg?")
    (review-xrefs-q . "�terse alla korsreferenser fr�n vilket inl�gg?")

    (confusion-who-to-reply-to . "Jag f�rst�r inte vems inl�gg du vill skriva ett privat svar till.\n")
    (confusion-what-to-answer-to . "Jag f�rst�r inte vilken text du vill besvara.\n")
    (confusion-what-to-view . "Jag f�rst�r inte vilken text du vill �terse.\n")

    (confusion-what-to-reply-to . "Jag f�rst�r inte vilken text du vill skriva en anm�rkning till.\n")
    (confusion-what-to-agree-to . "Jag f�rst�r inte vilken text du vill h�lla med om.\n")
    (confusion-what-to-request-confirmation . "Jag f�rst�r inte vilken text du vill ha l�sbekr�ftelser p�.\n")
    (confusion-what-to-comment . "Jag f�rst�r inte vilken text du vill kommentera.\n")
    (confusion-what-to-footnote . "Jag f�rst�r inte vilken text du vill skriva en fotnot till.\n")

    (confusion-what-to-delete . "Jag f�rst�r inte vilken text du vill radera.\n")
    (confusion-what-to-add-sub-recipient . "Jag f�rst�r inte vilken text du vill %#1s.\n")
    (add-rcpt-action-name . "addera mottagare till")
    (add-copy-action-name . "addera extra-kopiemottagare till")
    (add-bcc-action-name . "addera dold kopia till")
    (sub-action-name . "subtrahera mottagare fr�n")
    (move-action-name . "flytta")

    (confusion-what-to-add-comment-to . "Jag f�rst�r inte vilken text du vill addera en kommentar till.\n")
    (confusion-what-to-sub-comment-from . "Jag f�rst�r inte vilken text du vill subtrahera en kommentar fr�n.\n")
    (confusion-what-to-add-footnote-to . "Jag f�rst�r inte vilken text du vill addera en fotnot till.\n")
    (confusion-what-to-sub-footnote-from . "Jag f�rst�r inte vilken text du vill subtrahera en fotnot fr�n.\n")
    
    (confusion-what-to-mark . "Jag f�rst�r inte vilket inl�gg du vill markera.\n")
    (confusion-what-to-unmark . "Jag f�rst�r inte vilket inl�gg du vill avmarkera.\n")
    (confusion-what-to-save . "Jag f�rst�r inte vilket inl�gg du vill spara.\n")

    (confusion-what-to-review-mail-headers . "Jag f�rst�r inte vilket inl�ggs brevhuvud du vill se.\n")
    (confusion-what-to-unread-root . "Jag f�rst�r inte vilket inl�ggs urinl�gg du vill markera ol�st.\n")
    (confusion-what-to-find-root . "Jag f�rst�r inte vilket inl�ggs urinl�gg du vill �terse.\n")
    (could-not-find-root . "Kan inte identifiera urinl�gget. Kommentarsstrukturen kanske �r cirkul�r.\n")
    (confusion-what-to-unread-root-review . "Jag f�rst�r inte vilket inl�ggstr�d du vill markera ol�st.\n")
    (confusion-what-to-find-root-review . "Jag f�rst�r inte vilket inl�ggstr�d du vill �terse.\n")


    ;; Help

    (help-with-what . "Vad vill du ha hj�lp med? ")
    (help-for . "Hj�lp f�r \"%#1s\"\n")

    ;; Button actions

    (lyskom-button-view-text-action . "�terse texten")
    (lyskom-button-unread-text-action . "Ol�smarkera texten")
    (lyskom-button-copy-text-no-action . "Kopiera inl�ggsnumret")
    (lyskom-button-review-converted-action . "�terse konverterat")
    (lyskom-button-review-noconversion-action . "�terse omodifierat")
    (lyskom-button-review-rot13-action . "�terse rot13")
    (lyskom-button-find-root-review-action . "�terse tr�d")
    (lyskom-button-find-root-action . "�terse urinl�gget")
    (lyskom-button-comment-text-action . "Kommentera texten")
    (lyskom-button-private-comment-text-action . "Personligt svar")
    (lyskom-button-mark-text-action . "Markera texten")
    (lyskom-button-unmark-text-action . "Avmarkera texten")
    (lyskom-button-save-text-action . "Arkivera inl�gg")
    (lyskom-button-save-text-body-action . "Spara inl�ggstext")
    (lyskom-button-review-comments-action . "�terse alla kommentarer")
    (lyskom-button-review-tree-action . "�terse alla kommentarer rekursivt")
    (lyskom-button-write-footnote-action . "Fotnotera")
    (lyskom-button-fast-reply-action . "Anm�rk")
    (lyskom-button-view-conf-presentation-action . "Visa presentation")
    (lyskom-button-view-conf-status-action . "Visa m�tesstatus")
    (lyskom-button-goto-conf-action . "G� till m�tet")
    (lyskom-button-send-message-action . "Skicka gruppmeddelande")
    (lyskom-button-add-self-action . "Bli medlem i m�tet")
    (lyskom-button-sub-self-action . "Uttr�d ur m�tet")
    (lyskom-button-view-pers-presentation-action . "Visa presentation")
    (lyskom-button-view-pers-status-action . "Visa personstatus")
    (lyskom-button-view-session-status-action . "Visa sessionsstatus")
    (lyskom-button-mail-action . "Skicka brev")
    (lyskom-button-send-message-action . "S�nd meddelande")
    (lyskom-button-moronify-action . "L�gg p� idiot/kn�ppskalle-listan")
    (lyskom-button-befriend-action . "L�gg p� v�nner/bekanta-listan")
    (lyskom-button-open-url-action . "�ppna")
    (lyskom-button-copy-url-action . "Kopiera")
    (lyskom-button-goto-info-node-action . "�ppna")
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
    (lyskom-button-aux-type-xref . "Referens till m�te/person/inl�gg")
    (lyskom-button-aux-type-no-comments . "Beg�ran om inga kommentarer")
    (lyskom-button-aux-type-personal-comments . "Beg�ran om personligt svar")

    (lyskom-edit-toggle-secret-aux-action . "V�xla \"hemlig\"")
    (lyskom-edit-toggle-anonymous-aux-action . "V�xla \"anonym\"")
    (lyskom-edit-toggle-inherit-aux-action   . "V�xla \"�rvd\"")
    (lyskom-edit-delete-aux-action          . "Ta bort")
    (lyskom-prioritize-flag-toggle-action . "V�xla")
    (lyskom-prioritize-flag-set-action . "S�tt p�")
    (lyskom-prioritize-flag-clear-action . "St�ng av")

    (server-status-header   . "Status f�r LysKOM-server %#1s%#2?b%[ (%#2s:%#3d)%]%[%]\n\n")
    (server-status-server   . "Kanonisk server:                         %#1s%#2?b%[:%#2s%]%[%]\n")
    (server-status-version  . "Programversion:                          %#1s %#2s\n")
    (server-status-protocol . "Protokollversion:          %15#1d\n")
    (server-status-sessions . "\
Antal sessioner:     %21#1d (totalt)
                     %21#2d aktiva under de senaste %#7d minuterna
                     %21#3d inaktiva sessioner
                     %21#4d ok�nd aktivitet
                     %21#5d osynliga sessioner
                     %21#6d ej inloggade/hemliga/zombies\n")
    (server-status-first-text . "�ldsta befintliga text:     %14#1n\n")
    (server-status-last-text  . "Yngsta befintliga text:     %14#1n%#2?b%[ (%#2n vid start)%]%[%]\n")
    (server-status-texts .      "Antal existerande inl�gg:   %14#1d (%#2s just nu, %#3d vid start)\n")
    (server-status-confs .      "Antal existerande m�ten:    %14#1d (%#2s just nu, %#3d vid start)\n")
    (server-status-pers .       "Antal existerande personer: %14#1d (%#2s just nu, %#3d vid start)\n")
    (server-status-has-motd . "\nServern har en lapp p� d�rren:\n")
    (server-status-time . "Serverns tid:                   %#1s\n")
    (server-status-boot-time . "Servern startades:              %#1s\n")
    (server-status-save-time . "Initial databas sparades:       %#1s%#2?b%[ (%#2s)%]%[%]\n")

    (mship-type-invitation-q . "Inbjudan till medlemskap? ")
    (mship-type-passive-q . "Passivt medlemskap? ")
    (mship-type-secret-q . "Hemligt medlemskap? ")
    (recommend-which-conf . "Vilket m�te vill du rekommendera? ")
    (recommend-set-priority-q . "Rekommendera prioritet? ")
    (recommend-set-mship-type-q . "Rekommendera m�testyp? ")
    (recommending-conf . "Rekommenderar %#1M%#2?b%[ (prioritet %#2d)%]%[%]%#3?b%[ %#3s%]%[%]...")
    (has-recommended-conf . "%#1P har rekommenderat m�te %#2M. Vill du bli medlem?\n")
    (accept-recommendation . "G� med i %#1M? ")
    (accepting-recommendation . "G� med i rekommenderat m�te %#2M...")

    (redirect-for-whom . "Dirigera om kommentarer f�r: ")
    (redirect-to-which-conf . "Dirigera om kommentarer till: ")
    (redirecting-comments-to . "Dirigerar om kommentarer f�r %#1P till %#2M%#3?b%[ (f�rr %#1M)%]%[%]...")

    (kom-redirect-comments-e49 . "Du har inte r�tt att �ndra denna omdirigering")
    (kom-redirect-comments-e48 . "Omdirigering av kommentarer st�ds inte av servern")

    (external-program-missing . "Hittar inte det externa programmet \"%#1s\" som kr�vs f�r att genomf�ra kommandot.")
    (ignored-user-area-var . "\

Ignorerar f�ljande variabler i dina inst�llningar:

    %#1s

Variablerna �r inte registrerade LysKOM-variabler och kan d�rf�r inte
l�sas fr�n servern. Du kan ta bort variablerna fr�n servern genom att
ge kommandot \"Spara inst�llningar\" med prefixargument.

%#2?b%[%#5F\
Ok�nda variabler kan bero p� att dina inst�llningar sparades med
version %#3s av elispklienten, medan detta �r version %#4s.

%]%[%]")

    (unknown-aux-item . "Ok�nd till�ggsinformation")
    (text-header-aux-item . "%#1s: <%#2d> \"%#3s\" %#4s")

    (aux-content-type-name . "Inneh�llstyp")
    (aux-fast-reply-name . "Anm�rkning")
    (aux-cross-reference-name . "Referens")
    (aux-no-comments-name . "Beg�ran om inga kommentarer")
    (aux-personal-comment-name . "Beg�ran om personliga svar")
    (aux-request-confirmation-name . "Beg�ran om l�sbekr�ftelse")
    (aux-read-confirm-name . "L�sbekr�ftelse")
    (aux-redirect-name . "Omdirigering")
    (aux-x-face-name . "Bild")
    (aux-alternate-name-name . "Alternativt namn")
    (aux-pgp-signature-name . "PGP-signatur")
    (aux-pgp-public-key-name . "Publik PGP-nyckel")
    (aux-e-mail-address-name . "E-postadress")
    (aux-faq-text-name . "FAQ i text")
    (aux-creating-software-name . "Skapande klient")
    (aux-mx-author-name . "E-mailf�rfattare")
    (aux-mx-from-name . "E-mail fr�n")
    (aux-mx-reply-to-name . "E-mail svar s�nds till")
    (aux-mx-to-name . "E-mail till")
    (aux-mx-cc-name . "E-mail CC")
    (aux-mx-date-name . "E-maildatum")
    (aux-mx-message-id-name . "E-mail-meddelande-ID")
    (aux-mx-in-reply-to-name . "E-mail svar till")
    (aux-mx-misc-name . "E-mailheader")
    (aux-mx-allow-filter-name . "E-mail allow-filter")
    (aux-mx-reject-forward-name . "E-mail reject-forward")
    (aux-notify-comments-name . "Meddela kommentarer")
    (aux-faq-for-conf-name . "FAQ f�r m�te")
    (aux-recommended-conf-name . "M�tesrekommendation")
    (aux-allowed-content-type-name . "Till�ten inneh�llstyp")
    (aux-canonical-name-name . "Kanoniskt namn")
    (aux-mx-list-name-name . "E-maillistnamn")
    (aux-send-comments-to-name . "S�nd kommentarer till")
    (aux-world-readable-name . "L�sbart utan inloggning")
    (aux-mx-refuse-import . "V�gra inl�ggsimport")
    (aux-elisp-client-read-faq-name . "L�st FAQ")
    (aux-elisp-client-rejected-invitation-name . "Avvisad inbjudan")

    (review-marking-as-read . "�terse l�smarkerar")
    (review-not-marking-as-read . "�terse l�smarkerar inte")

    (review-using-cache . "�terse anv�nder cache")
    (review-not-using-cache . "�terse anv�nder inte cache")

    (using-ssh-connection . "Anv�nder ssh-uppkoppling till %#1s...")
    (opening-ssh-connection . "�ppnar ssh-uppkoppling till %#1s...")
    (ssh-cant-connect . "Kan inte g�ra ssh-uppkopplingen: %s")
    (ssh-closing . "St�nger ssh-uppkopplingen till %#1s")
    (ssh-unknown-host . "ok�nd v�rd")

    (keyboard-cancel . "Avbryt")
    (keyboard-menu-help . "(v�lj: C-n, C-p; bekr�fta: RET)")
    (customize-help . "Se b�rjan av bufferten f�r mer information")

    (no-mule-warning . "\

%#1@Du har st�det f�r multibytetecken (MULE) avslaget i emacs. Detta beror
antagligen p� att du har ett anrop till standard-display-european
eller motsvarande i din .emacs, eller i n�gon av systemets startfiler.

Elispklienten �r gjord f�r att k�ras med st�det f�r multibytetecken
p�slaget. Utan detta st�d kan inl�gg visas fel, och du kan f� problem
med att skapa inl�gg. Du b�r d�rf�r sl� p� st�det f�r multibytetecken
i emacs.

")
    (coding-system-mismatch-warning . "\

%#3@Din emacs �r inst�lld p� att i f�rsta hand koda tecken enligt
\"%#1s\", men den LysKOM-server du k�r mot rekommenderar
\"%#2s\". Det inneb�r att kommentarer du skriver till
andras inl�gg, och kommentarer till dina inl�gg kan f� en blandning av
tecken som inte g�r att visa ordentligt. Du b�r byta spr�komgivning i
emacs till en som anv�nder \"%#2s\" som f�rstahandsval f�r
teckenkodning (se Emacs-manualen och kommandot set-language-environment
f�r mer information).

")

    (has-nameday-1 . "%#1s har namnsdag i dag%#2?b%[ (%#2s)%]%[%].")
    (has-nameday-2 . "%#1s och %#2s har namnsdag i dag%#3?b%[ (%#3s)%]%[%].")
    (no-nameday . "Ingen namnsdag i dag%#2?b%[ (%#2s)%]%[%]")

    (invalid-date-entry . "Ogiltigt datum")
    (number-out-of-range . "Otill�tet tal")
    (or-date . "eller datum")

    (set-unread-n . "Endast l�sa %#1?d%[det senaste inl�gget%]%[de senaste %#1d inl�ggen%].\n")
    (set-unread-date . "Endast l�sa inl�gg sedan %#3d %#2s %#1d.\n")

    (jump-from-text . "Hoppa �ver kommentarer till vilken text? ")
    (jumping-from-text . "Hoppar �ver kommentarer till inl�gg %#1n.\n")
    (jumping-from-text-review . "Hoppar �ver �tersedda kommentarer.\n")
    
    (session-to-copy-options-from . "Kopiera inst�llningar fr�n session: ")
    (reading-settings-from . "L�ser inst�llningar fr�n %s...")
    (reading-settings-from-done . "L�ser inst�llningar fr�n %s...klart")

    (super-jump-q . "Superhoppa vilket inl�gg? ")
    (conf-list-legend . "* Ej medlem; - Passiv medlem\n")

    (unread-text-q . "G�r vilket inl�gg ol�st? ")
    (marking-text-unread . "Markerar inl�gg %#1n ol�st...")
    (cant-mark-text-unread . "Kunde inte markera inl�gg %#1n som ol�st (%#2s)\n")
    (confusion-what-to-mark-unread . "Jag f�rst�r inte vilken text du vill markera ol�st.\n")
    (no-comment-to-to-unread . "Det finns inget kommenterat inl�gg att markera som ol�st.\n")

    (mx-refuse-import-html . "HTML")
    (mx-refuse-import-spam . "Spam")
    (mx-refuse-import-all . "Alla texter")
    (abc-html . "hHTML")
    (abc-spam . "sSpam")
    (abc-everything . "aAlla texter")
    (limit-import-to-conf . "Begr�nsa import till vilket m�te? ")
    (limit-import-of-what . "Begr�nsa import av vad? ")
    (limit-import-not-super . "Du verkar inte vara organisat�r f�r m�tet. F�rs�k �nd�? ")
    (limiting-import . "Begr�nsar import av %#1s i %#2M...")

    (set-message-flag-for-conf . "�ndra meddelandeflagga f�r vilket m�te? ")
    (set-message-flag-to-what . "Vill du ta emot gruppmeddelanden till %#1M? ")
    (setting-message-flag . "%#1?b%[Sl�r p�%]%[St�nger av%] mottagning av gruppmeddelanden till %#2M...")

    (text-has-no-comments . "Inl�gg %#1n har inga kommentarer\n")
    (text-has-no-footnotes . "Inl�gg %#1n har inga fotnoter\n")
    (set-message-flag-q . "Ta emot gruppmeddelanden till %#1M? ")
    (why-you-got-no-presentation . "\

%#1@\
========================================================================
Du har ingen presentation%#2?z%[ men har skrivit %#2d inl�gg%]%[%]

Anv�nd kommandot \"�ndra presentation\" f�r att skriva eller �ndra din
presentation.

%[%#4F\
Din presentation l�ter andra i %#3s veta lite om vem du �r, och det
bidrar till k�nslan av samh�righet i %#3s. Det h�r meddelandet kommer
att upprepas d� och d� tills du har skrivit en presentation.%]
========================================================================

")

    (delete-user-area-warning . "\
%#1@\
========================================================================
Om du raderar user-arean s� kommer alla dina sparade inst�llningar att
f�rsvinna. Du kan naturligtvis spara dina aktuella inst�llningar, och 
d� kommer en ny user-area att skapas med elispklientens (men inga andra
klienters) inst�llningar.

�r du helt s�ker p� att du vet var du h�ller p� med och att du verkligen
vill radera din user-area?
========================================================================
")
    (delete-user-area-confirm . "Radera din user-area och alla sparade inst�llningar? ")
    (removing-user-area . "Tar bort h�nvisningen till user-arean fr�n din personstatus...")
    (deleting-user-area . "Raderar din user-area...")

    (status-server-stats . "Statistik om servern:\n\n")
    (current-average . "nu")

    (unit-second . "sekund")
    (unit-minute . "minut")
    (unit-hour . "timme")
    (unit-day . "dag")
    (unit-mongth . "m�nad")
    (unit-year . "�r")

    (db-status-backup . "informationsf�rlust kan ha f�rekommit")
    (no-support-in-server . "Servern saknar n�dv�ndig funktionalitet f�r detta kommando.\n")
    (mark-confs-as-known . "Markera dessa  %#2d %#1s som k�nda? ")
    (no-new-conferences . "Inga nya %#2s%#1?b%[ sedan %#1s%]%[%].\n")
    (new-conferences-since . "Nya %#2s%#1?b%[ sedan %#1s%]%[%]:\n")
    (list-confs-from-date . "Lista %#1s skapade sedan vilket datum (tomt f�r alla)? ")

    (privs-wheel . "operat�r")
    (privs-admin . "administrat�r")
    (privs-statistic . "statistik")
    (privs-create-conf . "skapa m�ten")
    (privs-create-pers . "skapa personer")
    (privs-change-name . "�ndra namn")
    (privs-flg7 . "ok�nd (7)")
    (privs-flg8 . "ok�nd (8)")
    (privs-flg9 . "ok�nd (9)")
    (privs-flg10 . "ok�nd (10)")
    (privs-flg11 . "ok�nd (11)")
    (privs-flg12 . "ok�nd (12)")
    (privs-flg13 . "ok�nd (13)")
    (privs-flg14 . "ok�nd (14)")
    (privs-flg15 . "ok�nd (15)")
    (privs-flg16 . "ok�nd (16)")
    (lyskom-no-privileges . "inga r�ttigheter")

    (what-pers-privs-to-change . "Vems r�ttigheter vill du �ndra? ")
    (change-pers-privs-prompt . "\
Nuvarande r�ttigheter f�r %#1P (%#1p):
    %#2s
�ndra r�ttigheter f�r %#1P (%#1p)...")

    (set-wheel-priv-q . "Aktivera operat�rsr�ttigheter? ")
    (set-admin-priv-q . "Aktivera administrat�rsr�ttigheter? ")
    (set-statistic-priv-q . "Aktivera statistikr�ttigheter? ")
    (set-create-conf-priv-q . "Aktivera r�tt att skapa m�ten? ")
    (set-create-pers-priv-q . "Aktivera r�tt att skapa personer? ")
    (set-change-name-priv-q . "Aktivera r�tt att �ndra namn? ")
    (set-flg7-priv-q . "Aktivera ok�nd r�ttighet 7? ")
    (set-flg8-priv-q . "Aktivera ok�nd r�ttighet 8? ")
    (set-flg9-priv-q . "Aktivera ok�nd r�ttighet 9? ")
    (set-flg10-priv-q . "Aktivera ok�nd r�ttighet 10? ")
    (set-flg11-priv-q . "Aktivera ok�nd r�ttighet 11? ")
    (set-flg12-priv-q . "Aktivera ok�nd r�ttighet 12? ")
    (set-flg13-priv-q . "Aktivera ok�nd r�ttighet 13? ")
    (set-flg14-priv-q . "Aktivera ok�nd r�ttighet 14? ")
    (set-flg15-priv-q . "Aktivera ok�nd r�ttighet 15? ")
    (set-flg16-priv-q . "Aktivera ok�nd r�ttighet 16? ")

    (canceling-command . "Abryter kommandot...")

    (no-selection . "Ingen markerad")
    (selection . "%#1d markerad%#1?d%[%]%[e%]")
    (lp-no-hidden . "")
    (lp-hidden . "(%#1d dold%#1?d%[%]%[a%])")
    (priority-prompt . "Ny prioritet f�r %#1M: ")
    (priority-prompt-marked . "Ny prioritet p� markerade m�ten: ")
    (lp-no-creation-info . "Ingen information om n�r medlemskapet skapades")
    (lp-invited . "Inbjuden")
    (lp-added . "Adderad")
    (lp-nope . "Det gick inte: %#1s")
    (lp-no-entry . "Det finns inget medlemskap h�r")
    (lp-no-active-filter . "(inga filter aktiva)")
    (lp-active-filters . "Aktiva filter: %#1s")
    (lp-mark-mship-with-prio . "Markera medlemskap med prioritet: ")
    (lp-unmark-mship-with-prio . "Avmarkera medlemskap med prioritet: ")
    (lp-no-selection . "Inga medlemskap �r markerade")
    (lp-at-min-prio . "Medlemskapet har redan l�gsta m�jliga prioritet")
    (lp-at-max-prio . "Medlemskapet har redan h�gsta m�jliga prioritet")
    (lp-beginning-of-list . "Listans b�rjan")
    (lp-end-of-list . "Listans slut")
    (lp-goto-priority . "G� till prioritet: ")
    (lp-mode-name . "Medlemskap")
    (lp-header-main . "Medlemskap f�r %#1M p� %#2s")
    (lp-list-header . " Prio   %#1s  Senast inne  Ol�st  IHPM\n")
    (lp-help-footer . "
 Markera medlemskap: SPC      Markera omr�de: C-w      Flytta markerade:   C-y
 S�tt prioritet:     p        �ka prioritet:  +        Minska prioritet:   -
 Flytta upp:         M-p      Flytta ned:     M-n      �ndra flaggor:  I,H,P,M
 Uppskjuta l�sning:  u        Endast:         e
 Avsluta:            C-c C-c                           Mer hj�lp:        C-h m
")
    (lp-hide-read-after . "D�lj medlemskap l�sta efter: ")
    (lp-hide-read-since . "D�lj medlemskap ej l�sta sedan: ")
    (lp-skipping-missing-meeting . "M�tet %#1M finns inte l�ngre, hoppar �ver.")
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
    (kom-help                 . "Hj�lp")
    (kom-slow-mode            . "L�ngsamma kommandon")
    (kom-quick-mode           . "Snabba kommandon")
    (kom-moronify             . "L�gg p� idiot/kn�ppskalle-listan")
    (kom-befriend             . "L�gg p� v�nner/bekanta-listan")
    (kom-send-message         . "S�nda meddelande")
    (kom-send-alarm           . "Skicka alarmmeddelande")
    (kom-create-conf	      . "Skapa m�te")
    (kom-delete-conf          . "Utpl�na (m�te/person)")
    (kom-delete-text          . "Radera inl�gg")
    (kom-display-time	      . "Se tiden")
    (kom-go-to-conf	      . "G� till m�te")
    (kom-go-to-next-conf      . "G� till n�sta m�te")
    (kom-jump		      . "Hoppa �ver alla kommentarer")
    (kom-list-created-conferences . "Lista �gda m�ten")
    (kom-list-conferences     . "Lista m�ten") 
    (kom-list-persons	      . "Lista personer")
    (kom-list-news            . "Lista nyheter")
    (kom-list-sessions        . "Lista sessioner")
    (kom-list-re              . "Lista (med) regexpar")
    (kom-membership	      . "Lista medlemskap")
    (kom-postpone	      . "Uppskjuta l�sning")
    (kom-set-session-priority . "S�tt l�sniv�")
    (kom-prioritize	      . "Prioritera m�ten")
    (kom-status-person	      . "Status (f�r) person")
    (kom-status-conf	      . "Status (f�r) m�te")
    (kom-add-self	      . "Bli medlem i m�te")
    (kom-change-priority      . "�ndra prioritet")
    (kom-list-summary	      . "Lista �renden")
    (kom-sub-self             . "Uttr�da ur m�te")
    (kom-quit		      . "Sluta")
    (kom-recover	      . "Starta om klienten") 
    (kom-start-anew	      . "B�rja med nytt namn")
    (kom-view		      . "�terse inl�gg")
    (kom-find-root-review     . "�terse tr�d")
    (kom-review-comments      . "�terse alla kommentarer")
    (kom-review-more-comments . "�terse fler kommentarer")
    (kom-review-tree	      . "�terse alla kommentarer rekursivt")
    (kom-review-clear         . "�terse hoppa")
    (kom-review-last-normally-read
     			      . "�terse igen")
    (kom-review-cross-references
     			      . "�terse alla korsreferenser")
    (kom-review-converted     . "�terse konverterat")
    (kom-review-noconversion  . "�terse omodifierat")
    (kom-review-rot13         . "�terse rot13")
    (kom-review-next          . "�terse n�sta")
    (kom-find-root	      . "�terse urinl�gget")
    (kom-review-by-to         . "�terse senaste")
    (kom-review-roots-by-to   . "�terse senaste urinl�gg")
    (kom-review-more          . "�terse fler inl�gg")
    (kom-review-first         . "�terse f�rsta")
    (kom-review-first-roots   . "�terse f�rsta urinl�gg")
    (kom-review-all           . "�terse alla")
    (kom-review-all-roots     . "�terse alla urinl�gg")
    (kom-view-commented-text  . "�terse det kommenterade")
    (kom-view-previous-commented-text
     			      . "�terse det f�reg�ende kommenterade")
    (kom-review-stack         . "�terse lista")
    (kom-review-presentation  . "�terse presentation")
    (kom-review-backward      . "(�terse) Bakl�nges")
    (kom-view-next-text	      . "L�sa n�sta inl�gg")
    (kom-who-is-on	      . "Vilka (�r) inloggade") 
    (kom-who-is-on-in-conference
     			      . "Vilka (�r inloggade i) m�te") 
    (kom-who-is-present-in-conference
     			      . "Vilka (�r) n�rvarande (i m�te)")
    (kom-who-is-on-and-friend . "Vilka v�nner (�r inloggade)")
    (kom-who-am-i             . "Var (�r) jag")
    (kom-list-clients	      . "Lista klienter")
    (kom-busy-wait	      . "V�nta p� ett inl�gg")
    (kom-write-comment	      . "Kommentera inl�gg")
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
    (kom-add-bcc	      . "Addera dold kopia")
    (kom-sub-recipient        . "Subtrahera mottagare")
    (kom-move-text            . "Flytta inl�gg")
    (kom-add-comment	      . "Addera kommentar")
    (kom-sub-comment	      . "Subtrahera kommentar")
    (kom-move-comment	      . "Flytta kommentar")
    (kom-add-cross-reference  . "Addera referens")
    (kom-add-member	      . "Addera medlem")
    (kom-sub-member           . "Uteslut medlem")
    (kom-change-conf-motd     . "S�tt lapp p� d�rren")
    (kom-set-garb-nice        . "�ndra livsl�ngd")
    (kom-set-super-conf       . "�ndra superm�te")
    (kom-set-permitted-submitters . "�ndra till�tna f�rfattare")
    (kom-unset-conf-motd      . "Ta bort lapp p� d�rren")
    (kom-save-text	      . "Arkivera inl�gg (till fil)")
    (kom-save-text-body	      . "Spara inl�ggstext (p� fil)")
    (kom-save-options         . "Spara inst�llningar")
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
    (kom-filter-recipient     . "Filtrera mottagare")
    (kom-super-jump           . "Superhoppa")
    (kom-filter-edit          . "�ndra filter")
    (kom-list-filters         . "Lista filter")
    (kom-show-user-area       . "Visa user-arean")
    (kom-delete-user-area     . "Ta bort user-arean")
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
    (kom-fast-reply           . "Anm�rkning (till inl�gg)")
    (kom-agree                . "H�lla med")
    (kom-add-faq              . "Addera FAQ")
    (kom-del-faq              . "Ta bort FAQ")
    (kom-review-faq           . "�terse FAQ")

    (kom-add-footnote	      . "Addera fotnot")
    (kom-sub-footnote	      . "Subtrahera fotnot")

    (kom-add-no-comments      . "F�rhindra kommentarer")
    (kom-add-private-answer   . "Beg�r personligt svar")
    (kom-add-request-confirm  . "Beg�r l�sbekr�ftelse")

    (kom-review-mail-headers  . "�terse brevhuvud")
    
    (kom-compare-texts        . "J�mf�r tv� texter")
    (kom-diff-texts           . "Se diff")

    (kom-become-anonymous     . "Bli anonym")
    (kom-become-nonanonymous  . "Bli icke-anonym (tr�d fram ur skuggorna)")

    (kom-keep-alive           . "H�ll f�rbindelsen aktiv")
    (kom-stop-keep-alive      . "Avbryt h�ll f�rbindelsen aktiv")

    (kom-is-person-member-of-conference . "Kontrollera (om person �r) medlem (i m�te)")
    (kom-will-person-read-text . "Kontrollera (om person kommer att) l�sa (inl�gg)")

    (kom-change-conf-faq      . "�ndra FAQ")

    (kom-make-review-mark-as-read . "�terse l�smarkerar")
    (kom-make-review-not-mark-as-read . "�terse l�smarkerar inte")

    (kom-set-presentation     . "Addera presentation")
    (kom-remove-presentation  . "Ta bort presentation")
    (kom-set-motd-text        . "Addera lapp p� d�rren")
    (kom-create-aux-item      . "Skapa till�ggsinformation")
    (kom-status-server        . "Status (f�r) servern")
    (kom-add-server-faq       . "Addera server-FAQ")
    (kom-del-server-faq       . "Ta bort server-FAQ")
    (kom-review-server-faq    . "�terse server-FAQ")
    (kom-change-server-faq    . "�ndra server-FAQ")
    (kom-recommend-conference . "Rekommendera m�te")
    (kom-redirect-comments    . "Dirigera om kommentarer")
    (kom-move-text-tree       . "Flytta tr�d")

    (kom-copy-options         . "Kopiera inst�llningar")
    (kom-mark-unread          . "Ol�smarkera inl�gg")
    (kom-unread-by-to         . "Ol�smarkera senaste")
    (kom-unread-roots-by-to   . "Ol�smarkera senaste urinl�gg")
    (kom-unread-more          . "Ol�smarkera fler inl�gg")
    (kom-unread-commented-text . "Ol�smarkera det kommenterade")
    (kom-unread-previous-commented-text . "Ol�smarkera det f�reg�ende kommenterade")
    (kom-unread-comments      . "Ol�smarkera alla kommentarer")
    (kom-unread-tree          . "Ol�smarkera alla kommentarer rekursivt")
    (kom-unread-root          . "Ol�smarkera urinl�gget")
    (kom-unread-root-review   . "Ol�smarkera tr�d")
    (kom-unread-last-normally-read
     			      . "Ol�smarkera igen")
    (kom-unread-all           . "Ol�smarkera alla")
    (kom-unread-all-roots     . "Ol�smarkera alla urinl�gg")
    (kom-unread-first         . "Ol�smarkera f�rsta")
    (kom-unread-first-roots   . "Ol�smarkera f�rsta urinl�gg")
    (kom-unread-presentation  . "Ol�smarkera presentation")
    (kom-unread-server-faq    . "Ol�smarkera server-FAQ")
    (kom-unread-faq           . "Ol�smarkera FAQ")
    (kom-unread-marked-texts  . "Ol�smarkera markerade")
    (kom-unread-all-marked-texts . "Ol�smarkera alla markerade")

    (kom-join-all-conferences . "Bli medlem i alla m�ten")
    (kom-leave-all-conferences . "Uttr�d ur (n�stan) alla m�ten")
    (kom-limit-import         . "Begr�nsa import av texter")
    (kom-change-message-flag  . "�ndra mottagning (av) gruppmeddelanden")
    (kom-list-faqs            . "Lista FAQ")
    (kom-list-server-faqs     . "Lista server-FAQ")
    (kom-list-new-conferences . "Lista nya m�ten")
    (kom-list-new-persons     . "Lista nya personer")
    (kom-change-privileges    . "�ndra r�ttigheter")
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


(lyskom-language-ending-mismatch lyskom-menu language sv en)
(lyskom-language-ending-mismatch lyskom-menu kom-change-language sv en)
(lyskom-language-ending-mismatch lyskom-menu kom-change-global-language sv en)
(lyskom-language-ending-mismatch lyskom-menu kom-change-local-language sv en)

(lyskom-language-strings global lyskom-menu sv
  '((lyskom    . "LysKOM")
    (read      . "L�s")
    (dont-read . "Hoppa")
    (write     . "Skriv")
    (conference . "M�te")
    (person    . "Person")
    (server    . "Server")
    (other     . "Annat")
    (move      . "G�")
    (info      . "Om")
    (send      . "S�nd")
    (unread    . "Markera ol�st")
    (review    . "�terse inl�gg")
    (marks     . "Markerade inl�gg")
    (filters   . "Filter")
    (receivers . "Mottagare")
    (commented . "Kommenterar")
    (aux-items . "Till�ggsinformation")
    (conf-admin . "M�tesadministration")
    (server-admin . "Serveradministration")
    (membership . "Medlemskap")
    (pers-admin . "Personadministration")
    (autoreply . "Automatsvar")
    (remote-control . "Fj�rrstyrning")
    (language . "�ndra spr�k (Change Language)")
    (kom-help . "Hj�lp (Help)")
    (kom-change-language . "�ndra spr�k (Change language)")
    (kom-change-global-language . "Visning och kommandon (display and commands)")
    (kom-change-local-language . "Endast visning (display language only)")
    (kom-edit-send . "Skicka in")
    (kom-edit-send-anonymous . "Skicka anonymt")
    (kom-edit-quit . "Kasta bort") 
    (kom-ispell-message . "Stavningskontroll")
    (kom-edit-add-recipient . "Addera mottagare")
    (kom-edit-add-copy . "Addera extra kopiemottagare")
    (kom-edit-show-commented . "�terse det kommenterade")
    (kom-edit-insert-commented . "Citera det kommenterade")
    (kom-edit-insert-buglist . "Klistra in kommenterad buglista")
    (kom-edit-add-bcc . "Addera dold kopia")
    (kom-edit-add-cross-reference . "Addera korsreferens")
    (kom-edit-add-no-comments . "Beg�r inga kommentarer")
    (kom-edit-add-personal-comments . "Beg�r personliga svar")
    (kom-edit-add-read-confirm-request . "Beg�r l�sbekr�ftelse")
    (kom-edit-move-text . "Flytta till en ny mottagare")

    (lp--toggle-membership-selection . "V�xla markering p� medlemskap")
    (lp--select-region . "Markera omr�de")
    (lp--select-priority . "Markera prioritet")
    (lp--deselect-priority . "Avmarkera prioritet")
    (lp--deselect-all . "Avmarkera alla")
    (lp--membership-expansion . "Expandera medlemskap")
    (lp--toggle-entry-expansion . "V�xla expansion")
    (lp--expand-entry . "Expandera medlemskap")
    (lp--contract-entry . "Avexpandera medlemskap")
    (lp--membership-priority . "Prioritet")
    (lp--set-priority . "�ndra prioritet")
    (lp--move-up . "Flytta upp�t")
    (lp--move-down . "Flytta ned�t")
    (lp--increase-priority . "�ka prioritet")
    (lp--decrease-priority . "Minska prioritet")
    (lp--yank . "Flytta markerade")
    (lp--filter . "Urval")
    (lp--show-hide-memberships-read-before . "V�xla visning av medlemskap l�sta f�re...")
    (lp--show-hide-memberships-read-after . "V�xla visning av medlemskap l�sta efter...")
    (lp--show-hide-read-memberships . "V�xla visning av medlemskap utan ol�sta")
    (lp--show-hide-passive-memberships . "V�xla visning av passiva medlemskap")
    (lp--show-all . "Visa alla medlemskap")
    (lp--membership-type . "Medlemskapstyp")
    (lp--toggle-invitation . "V�xla inbjudan")
    (lp--toggle-passive . "V�xla passiv")
    (lp--toggle-message-flag . "V�xla meddelanden")
    (lp--toggle-secret . "V�xla hemlig")
    (lp--set-unread . "Endast")
    (lp--quit . "Avsluta")
))

(lyskom-language-var local lyskom-onoff-table sv
  '(("p�" . on) ("av" . off)))

(lyskom-language-var local lyskom-filter-predicate-list sv
      '(("=" . nil) ("!=" . t)))

(lyskom-language-var local lyskom-filter-what sv
      '((author . "F�rfattare")
        (author-no . "F�rfattare (nummer)")
        (author-re . "F�rfattare (regexp)")
        (subject . "�rende")
        (subject-re . "�rende (regexp)")
        (recipient . "Mottagare")
        (recipient-no . "Mottagare (nummer)")
        (recipient-re . "Mottagare (regexp)")
        (text . "Inneh�ll")
        (text-re . "Inneh�ll (regexp)")))

(lyskom-language-var local lyskom-filter-actions sv
      '((skip-text . "Hoppa �ver")
        (dontshow . "Visa inte")
        (read . "Undanta fr�n filtrering")
        (skip-tree . "Hoppa �ver kommentarer")
	(obliterate . "Utpl�na")))

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

  (lyskom-try-define-key lyskom-sv-mode-map (kbd "�")    'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map (kbd "{")    'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map (kbd "[")    'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [adiaeresis] 'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [Adiaeresis] 'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [�]          'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [�]          'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [228]        'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [196]        'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [2276]       'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [2244]       'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [3908]       'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [3940]       'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [195 164]    'lyskom-sv-change-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [195 132]    'lyskom-sv-change-prefix)

  (lyskom-try-define-key lyskom-sv-mode-map (kbd "�") 'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map (kbd "}") 'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map (kbd "]") 'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [aring]     'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [Aring]     'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [�]       'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [�]       'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [229]     'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [197]     'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [2277]    'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [2245]    'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [3909]    'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [3941]    'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [195 165] 'lyskom-sv-review-prefix)
  (lyskom-try-define-key lyskom-sv-mode-map [195 133] 'lyskom-sv-review-prefix)

  ; XEmacs 21.4 UTF-8 on terminal compatibility
  (lyskom-try-define-key lyskom-sv-mode-map [(�)]      (let ((k (make-sparse-keymap)))
                                                           (define-key k (vector (intern "\205")) 'lyskom-sv-review-prefix)
                                                           (define-key k [�] 'lyskom-sv-review-prefix)
                                                           (define-key k (vector (intern "\204")) 'lyskom-sv-change-prefix)
                                                           (define-key k [(�)] 'lyskom-sv-change-prefix)
							   k))

  (define-key lyskom-sv-mode-map (kbd "M-m") 'kom-toggle-mark-as-read-prefix)
  (define-key lyskom-sv-mode-map (kbd "M-c") 'kom-toggle-cache-prefix)
  (define-key lyskom-sv-mode-map (kbd "M-u") 'kom-toggle-topic-prefix)

  (define-key lyskom-sv-mode-map (kbd (lyskom-keys 'button2)) 'kom-mouse-null)
  (define-key lyskom-sv-mode-map (kbd (lyskom-keys 'button2up)) 'kom-button-click)
  (define-key lyskom-sv-mode-map (kbd (lyskom-keys 'button3))   'kom-popup-menu)
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

  (define-key lyskom-sv-list-prefix (kbd "k") 'kom-list-sessions)
  (define-key lyskom-sv-list-prefix (kbd "M") 'kom-list-marks)
  (define-key lyskom-sv-list-prefix (kbd "m") 'kom-list-conferences)
  (define-key lyskom-sv-list-prefix (kbd "n") 'kom-list-news)
  (define-key lyskom-sv-list-prefix (kbd "p") 'kom-list-persons)
  (define-key lyskom-sv-list-prefix (kbd "r") 'kom-list-re)
  (define-key lyskom-sv-list-prefix (kbd "s") 'kom-membership)
  (define-key lyskom-sv-list-prefix (kbd "f") 'kom-list-filters)
  (define-key lyskom-sv-list-prefix (kbd "q") 'kom-list-faqs)
  (define-key lyskom-sv-list-prefix (kbd "Q") 'kom-list-server-faqs)

  (lyskom-try-define-key lyskom-sv-list-prefix (kbd "�") 'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix (kbd "{") 'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix (kbd "[") 'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [adiaeresis]     'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [Adiaeresis]     'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [�]       'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [�]       'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [228]     'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [196]     'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [2276]    'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [2244]    'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [3908]    'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [3940]    'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [195 164] 'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [195 132] 'kom-list-summary)
  (lyskom-try-define-key lyskom-sv-list-prefix [(�)]        (let ((k (make-sparse-keymap)))
                                                           (define-key k (vector (intern "\204")) 'kom-list-summary)
                                                           (define-key k [�] 'kom-list-summary)
							   k))

  (define-key lyskom-sv-filter-get-prefix (kbd "u") 'kom-get-appreciation)
  (define-key lyskom-sv-filter-get-prefix (kbd "s") 'kom-get-abuse)
  (define-key lyskom-sv-filter-get-prefix (kbd "t") 'kom-move-text)
  (define-key lyskom-sv-filter-get-prefix (kbd "T") 'kom-move-text-tree)
  (define-key lyskom-sv-filter-get-prefix (kbd "f") 'kom-filter-author)
  (define-key lyskom-sv-filter-get-prefix (kbd "i") 'kom-filter-text)
  (define-key lyskom-sv-filter-get-prefix (kbd "m") 'kom-filter-recipient)
  (define-key lyskom-sv-filter-get-prefix (kbd "l") 'kom-previous-kom)
  (define-key lyskom-sv-filter-get-prefix (kbd "k") 'kom-move-comment)

  (lyskom-try-define-key lyskom-sv-filter-get-prefix (kbd "�")    'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix (kbd "{")    'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix (kbd "[")    'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [adiaeresis] 'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [Adiaeresis] 'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [�]          'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [�]          'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [228]        'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [196]        'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [2276]       'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [2244]       'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [3908]       'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [3940]       'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [195 164]    'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [195 132]    'kom-filter-subject)
  (lyskom-try-define-key lyskom-sv-filter-get-prefix [(�)]        (let ((k (make-sparse-keymap)))
                                                           (define-key k (vector (intern "\204")) 'kom-filter-subject)
                                                           (define-key k [�] 'kom-filter-subject)
							   k))

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
  (define-key lyskom-sv-unread-prefix (kbd "S") 'kom-unread-roots-by-to)
  (define-key lyskom-sv-unread-prefix (kbd "y") 'kom-unread-more)
  (define-key lyskom-sv-unread-prefix (kbd "t") 'kom-unread-root-review)
  (define-key lyskom-sv-unread-prefix (kbd "A") 'kom-unread-all-roots)
  (define-key lyskom-sv-unread-prefix (kbd "f") 'kom-unread-first)
  (define-key lyskom-sv-unread-prefix (kbd "F") 'kom-unread-first-roots)
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
  (define-key lyskom-sv-review-prefix (kbd "a y") 'kom-review-more-comments)
  (define-key lyskom-sv-review-prefix (kbd "a r") 'kom-review-tree)
  (define-key lyskom-sv-review-prefix (kbd "a x") 'kom-review-cross-references)
  (define-key lyskom-sv-review-prefix (kbd "h") 'kom-review-clear)
  (define-key lyskom-sv-review-prefix (kbd "i") 'kom-review-last-normally-read)
  (define-key lyskom-sv-review-prefix (kbd "n") 'kom-review-next)
  (define-key lyskom-sv-review-prefix (kbd "o") 'kom-review-noconversion)
  (define-key lyskom-sv-review-prefix (kbd "v") 'kom-review-converted)
  (define-key lyskom-sv-review-prefix (kbd "3") 'kom-review-rot13)
  (define-key lyskom-sv-review-prefix (kbd "r") 'kom-find-root)
  (define-key lyskom-sv-review-prefix (kbd "u") 'kom-find-root)
  (define-key lyskom-sv-review-prefix (kbd "s") 'kom-review-by-to)
  (define-key lyskom-sv-review-prefix (kbd "S") 'kom-review-roots-by-to)
  (define-key lyskom-sv-review-prefix (kbd "y") 'kom-review-more)
  (define-key lyskom-sv-review-prefix (kbd "A") 'kom-review-all-roots)
  (define-key lyskom-sv-review-prefix (kbd "f") 'kom-review-first)
  (define-key lyskom-sv-review-prefix (kbd "F") 'kom-review-first-roots)
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

  (lyskom-try-define-key lyskom-sv-S-prefix (kbd "�")    'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix (kbd "{")    'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix (kbd "[")    'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [adiaeresis] 'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [Adiaeresis] 'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [�]          'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [�]          'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [228]        'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [196]        'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [2276]       'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [2244]       'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [3908]       'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [3940]       'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [195 164]    'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [195 132]    'kom-send-message)
  (lyskom-try-define-key lyskom-sv-S-prefix [(�)]        (let ((k (make-sparse-keymap)))
                                                           (define-key k (vector (intern "\204")) 'kom-send-message)
                                                           (define-key k [�] 'kom-send-message)
							   k))

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

  (define-key lyskom-sv-mode-map (kbd "M-p") 'kom-backward-text)
  (define-key lyskom-sv-mode-map (kbd "M-n") 'kom-forward-text)
  (define-key lyskom-sv-mode-map (kbd "C-M-p") 'kom-prev-prompt)
  (define-key lyskom-sv-mode-map (kbd "C-M-n") 'kom-next-prompt)

  (define-key lyskom-sv-mode-map (kbd "C-?") 'scroll-down)
  (define-key lyskom-sv-mode-map (kbd "<DEL>") 'scroll-down)
  (define-key lyskom-sv-mode-map (kbd "<delete>") 'scroll-down)
  (define-key lyskom-sv-mode-map (kbd "<BS>") 'scroll-down)

  (define-key lyskom-sv-mode-map (kbd "<") 'beginning-of-buffer)
  (define-key lyskom-sv-mode-map (kbd ">") 'end-of-buffer)
  )


;;; ================================================================
;;; Prioritization

(defvar lyskom-sv-prioritize-mode-map nil "Keymap used in lyskom-prioritize-mode")
(lyskom-language-keymap lyskom-prioritize-mode-map sv lyskom-sv-prioritize-mode-map)

(if lyskom-sv-prioritize-mode-map
    nil
  (setq lyskom-sv-prioritize-mode-map (make-keymap))
  (suppress-keymap lyskom-sv-prioritize-mode-map)
  (define-key lyskom-sv-prioritize-mode-map (kbd "SPC") 'lp--toggle-membership-selection)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-k") 'lp--toggle-membership-selection)
  (define-key lyskom-sv-prioritize-mode-map (kbd "p")   'lp--set-priority)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-w") 'lp--select-region)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-w") 'lp--select-region)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-y") 'lp--yank)
  (define-key lyskom-sv-prioritize-mode-map (kbd "#")   'lp--select-priority)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-#")   'lp--deselect-priority)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-DEL") 'lp--deselect-all)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-p") 'lp--previous-entry)
  (define-key lyskom-sv-prioritize-mode-map (kbd "<up>") 'lp--previous-entry)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-n") 'lp--next-entry)
  (define-key lyskom-sv-prioritize-mode-map (kbd "<down>") 'lp--next-entry)
  (define-key lyskom-sv-prioritize-mode-map (kbd "<home>") 'lp--first-entry)
  (define-key lyskom-sv-prioritize-mode-map (kbd "<end>") 'lp--last-entry)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-<") 'lp--first-entry)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M->") 'lp--last-entry)
  (define-key lyskom-sv-prioritize-mode-map (kbd "g") 'lp--goto-priority)
  (define-key lyskom-sv-prioritize-mode-map (kbd "RET") 'lp--toggle-entry-expansion)
  (define-key lyskom-sv-prioritize-mode-map (kbd "+") 'lp--increase-priority)
  (define-key lyskom-sv-prioritize-mode-map (kbd "-") 'lp--decrease-priority)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-p") 'lp--move-up)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-n") 'lp--move-down)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-<up>") 'lp--move-up)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-<down>") 'lp--move-down)
  (define-key lyskom-sv-prioritize-mode-map (kbd "I") 'lp--toggle-invitation)
  (define-key lyskom-sv-prioritize-mode-map (kbd "H") 'lp--toggle-secret)
  (define-key lyskom-sv-prioritize-mode-map (kbd "P") 'lp--toggle-passive)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M") 'lp--toggle-message-flag)
  (define-key lyskom-sv-prioritize-mode-map (kbd "e") 'lp--set-unread)
  (define-key lyskom-sv-prioritize-mode-map (kbd "u") 'lp--postpone)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-c C-c") 'lp--quit)
  (define-key lyskom-sv-prioritize-mode-map (kbd "q") 'lp--quit)
  (define-key lyskom-sv-prioritize-mode-map (kbd "(") 'lp--expand-entry)
  (define-key lyskom-sv-prioritize-mode-map (kbd ")") 'lp--contract-entry)

  (define-key lyskom-sv-prioritize-mode-map (kbd (lyskom-keys 'button2up)) 'kom-button-click)
  (define-key lyskom-sv-prioritize-mode-map (kbd (lyskom-keys 'button2)) 'kom-mouse-null)
  (define-key lyskom-sv-prioritize-mode-map (kbd (lyskom-keys 'button3)) 'kom-popup-menu)
  (define-key lyskom-sv-prioritize-mode-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lyskom-sv-prioritize-mode-map (kbd "*") 'kom-button-press)
  (define-key lyskom-sv-prioritize-mode-map (kbd "=") 'kom-menu-button-press)
  (define-key lyskom-sv-prioritize-mode-map (kbd "TAB") 'kom-next-link)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-TAB") 'kom-previous-link)
  (define-key lyskom-sv-prioritize-mode-map (kbd "C-i") 'kom-next-link)
  (define-key lyskom-sv-prioritize-mode-map (kbd "M-C-i") 'kom-previous-link)
  )



;;; ==============================================================
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
  (define-key lyskom-sv-customize-map (kbd (lyskom-keys 'button1)) 'widget-button-click)
  (define-key lyskom-sv-customize-map (kbd (lyskom-keys 'button2)) 'widget-button-click)
  (define-key lyskom-sv-customize-map (kbd (lyskom-keys 'button3)) 'lyskom-widget-click)
  (define-key lyskom-sv-customize-map (kbd (lyskom-keys 'button1up)) 'kom-mouse-null)
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
    (which-conf . "Ange ett m�te: ")
    (which-conf-or-person . "Ange en person eller ett m�te: ")
    (which-name . "Ange ett namn: ")
    (some-person . "Person %#1d")
    (invalid-value . "Otill�tet v�rde (%#1S)")
    (unknown-command . "Ok�nt kommando (%#1s)")
    (ding . "Ljudsignal")

    ;;
    ;; Help messages
    ;;

    (variable-type-help . "Kryssa i f�r att spara inst�llningen i servern.")
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
    (to-file . "Till fil:")
    (max-text-length . "F�r inl�gg kortare �n: ")
    (autowrap-timeout . "Antal sekunder: ")
    (no-timeout . "Ingen tidsbegr�nsning")

    (session-lost-all-buffers . "Visa notifiering i alla aktiva LysKOM buffrar")
    (session-lost-beep . "Pip")

    (friends . "V�nner och bekanta")

    (turned-off      . "Avslaget            ")
    (number-of-times . "N�gra g�nger")
    (specific-spec . "Per mottagare/avs�ndare")
    (sound-file . "Ljudfil")
    (other-persons . "Alla andra")
    (selected-mark . "Markering")
    (conf-or-person . "Person eller m�te")
    (ask . "Fr�ga varje g�ng")
    (before . "F�re texten")
    (after . "Efter texten")
    (first . "F�rst")
    (last . "Sist ")
    (depth-first . "I kommentarsordning")
    (time-order . "I tidsordning")
    (limited-number . "Ett begr�nsat antal")
    (unlimited-number . "Obegr�nsat")

    (ar-message-type . "Meddelandetyp")
    (ar-personal     . "Personligt meddelande")
    (ar-group        . "Gruppmeddelande")
    (ar-alarm        . "Alarmmeddelande")
    (ar-any-type     . "Alla meddelandetyper")
    (ar-sender       . "Avs�ndare")
    (ar-any-sender   . "Alla avs�ndare")
    (ar-specified-sender . "Specifika avs�ndare")
    (ar-pers-or-conf . "Person eller m�te")
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
    (no-break      . "Efter aktuellt m�te")

    (express-server-break . "Omedelbart")
    (express-server-letters-break . "Omedelbart om det kommer brev")
    (server-break . "Efter aktuell kommentarskedja")
    (letters-server-break . "Efter aktuell kommentarskedja om det kommer brev")
    (after-conf-server-break . "Efter aktuellt m�te")
    (after-conf-letters-server-break . "Efter aktuellt m�te n�r det kommer brev")
    (when-done-server-break . "N�r allt �r utl�st")
    (no-server-break . "Aldrig")

    (command . "Kommando")
    (command-list . "Kommandolista")
    (some-persons . "F�r vissa personer:")
    (name . "Namn")

    (page-none . "Aldrig                 ")
    (page-all  . "F�re varje kommando    ")
    (page-some . "F�re f�ljande kommandon")

    (ask-every-time . "Fr�ga varje g�ng      ")
    (fixed-priority . "Fast prioritet")

    (messages-in-lyskom-buffer . "I LysKOM-bufferten  ")
    (discard-messages .          "Ingenstans          ")
    (in-named-buffer .           "I namngiven buffert:")

    (automatic-session-name . "Automatiskt")
    (given-session-name . "Anv�nd angivet:")

    (simple-unread-mode-line . "Enkel")
    (explicit-unread-mode-line . "Visa alla sessioner med ol�sta")

    (everybody-rcpt .       "Alla                              ")
    (group-rcpt .           "Senaste gruppmeddelandes mottagare")
    (sender-rcpt .          "Senaste meddelandes avs�ndare     ")
    (last-recipient-recpt . "Senaste mottagaren                ")

    (viewer-program . "WWW-l�sare")
    (no-viewer            . "(ingenting valt)")
    (default-viewer       . "Browse-URL (alla)")
    (netscape-viewer      . "Firefox/Mozilla/Opera/Netscape (alla)")
    (windows-viewer       . "Windows standard eller Netscape-liknande (alla)")
    (emacs-w3-viewer      . "Emacs W3-mode (HTTP, Gopher, FTP)")
    (emacs-general-viewer . "Emacs (FTP, Telnet, Mail)")
    (emacs-dired-viewer   . "Emacs Dired (FTP)")
    (emacs-mail-viewer    . "Emacs Mail-mode (Mail)")
    (emacs-telnet-viewer  . "Emacs Telnet-mode (telnet)")
    (mosaic-viewer        . "NCSA Mosaic (alla)")
    (lynx-viewer          . "Lynx (alla)")
    (galeon-viewer        . "Galeon (alla)")

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

    (first . "F�rst")
    (last . "Sist")
    (specific-placement . "Specifik plats")
    (priority . "Prioritet")
    (same-as-conf . "Samma som aktuellt m�te")
    (custom-format . "Eget format:")
    (default-format . "Standardformat")
    (a-string . "En text:")
    (some-string . "En slumpm�ssig text")
    (unspecified . "Ospecificerat")

    (symbolic-mark-association . "Associering")
    (symbolic-mark-name . "Symboliskt namn: ")
    (mark-type-to-assoc . "Markeringstyp att associera till: ")

    (prompt-for-text-no . "Fr�ga efter textnummer")

    (all-others . "Alla andra")
    (some-person . "Person")
    (format-html . "Formattera HTML:")

    (url-transform-regexp . "Fr�n (regexp):")
    (url-transform-newtext . "Till:")

    (default-namedays . "Namnsdagar f�r aktuellt spr�k")
    (specific-namedays . "Specifik namnl�ngd")
    (several-name-lists . "Flera namnl�ngder")

    (link-pattern-for . "M�nster f�r")
    (all-conferences . "Alla mottagare")
    (link-specific-rcpt . "Specifik mottagare")
    (link-pattern . "L�nkm�nster")
    (link-replace . "L�nksubstitution")
    (link-highlight-match . "Visa grupp")
    (link-fold-case . "Versaler och gemener lika")

    (generated-face . "Automatgenererat teckensnitt")
    (predefined-fonts . "Teckensnitt avsedda f�r specifika till�mpningar\n")
    (other-fonts . "Teckensnitt utan specifika till�mpningar\n")
    (other-face . "Annat")

    (ext-simple-command . "Enkelt kommando")
    (ext-complex-command . "Kommando med argument")
    (ext-complex-component . "Komanndo/argument")

    (lynx-xterm . "Lynx i xterm")
    (lynx-emacs . "Lynx i emacs terminal-mode")

    (morons . "Idioter och kn�ppskallar")
    (friends . "V�nner och bekanta")
    (me . "Mig sj�lv och ingen annan")
    (i-am-supervisor . "M�ten jag organiserar")
    (has-no-presentation . "M�ten utan presentationer")
    (highlight-conferences . "M�ten:")
    (highlight-face . "Utseende:")
    (highlight-conflist . "M�teslista")

    (yes-dont-pop . "Ja, men avikonifiera inte f�nstret")
    (language . "Spr�k")

    (specific-extended-info . "Enbart vald information")
    (extended-info-show . "Visa:")
    (extended-info-type . "Informationstyp:")

    (extended-server-info . "All ut�kad serverinformation")
    (extended-conf-info . "All ut�kad m�tesinformation")
    (extended-pers-info . "All ut�kad personinformation")
    (extended-read-faq-info . "L�sta FAQ")
    (extended-statistics-info . "Fullst�ndig serverstatistik")
    (extended-all-other-info . "Annan information")

    (show-presence . "Visa n�rvaromeddelanden")
    (conflist . "M�tes/Personlista")
    (pers-list-1 . "M�tes/personlista 1")
    (pers-list-2 . "M�tes/personlista 2")
    (pers-list-3 . "M�tes/personlista 3")
    (pers-list-4 . "M�tes/personlista 4")
    (other-list . "Annan lista")
    (always-show-presence . "Visa alla n�rvaromeddelanden")
    (never-show-presence . "Visa inga n�rvaromeddelanden")
    (obsolete-setting . "F�r�ldrad inst�llning")
    (detailed-setting . "Detaljerad inst�llning")

    ;;
    ;; Misc doc strings
    ;;

    (lyskom . "Inst�llningar f�r LysKOM")
    (lyskom-doc . "\
Dessa tangenter fungerar utanf�r redigerbara textf�lt.
\\[lyskom-customize-save-and-quit] f�r att spara och avsluta,
\\[lyskom-customize-save] f�r att spara utan att avsluta
\\[lyskom-customize-quit] f�r att avsluta utan att spara
\\[widget-forward] flyttar till n�sta inst�llning
\\[widget-button-press] �ndrar v�rdet
Tryck C-h m f�r en fullst�ndig lista av kommandon.

Hj�lptexter: [?] Visa hj�lptext    [!] G�m hj�lptext
Listor mm.:  [INS] L�gg till rad   [DEL] Ta bort rad    [*] �ndra v�rde

Om rutan f�re inst�llningens namn �r ikryssad kommer v�rdet att sparas
i servern. Annars sparas det i din .emacs.

Klicka med musens mittenknapp p� ledtexterna f�r att �ndra v�rde eller
ta fram menyer.")

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

    (new-window-netscape . "�ppna URLer i nytt f�nster")
    (new-tab-netscape . "�ppna URLer i ny tab")
    (default-netscape . "�ppna URLer d�r de r�kar hamna")
    
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
        man bekr�fta alla mottagare i klump). Det g�r ocks� att sl� av
        helt.

    Kontroll av kommenterad f�rfattares medlemskap
        Kontrollera att f�rfattaren till det kommenterade inl�gget �r
        medlem i n�gon av mottagarna f�r kommentaren. Om s� inte �r fallet,
        erbjuder LysKOM att l�gga till f�rfattaren som mottagare till
        inl�gget. 

    F�rfattare som inte kontrolleras
        En lista av f�rfattare som inte skall kontrolleras �ven om
        kontroll av kommenterad f�rfattares medlemskap �r p�slaget.

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
        f�nster s� kommer ett nytt att skapas (och tas bort n�r man �r klar).

    I en annan frame
        I en annan frame �n LysKOM. Om det bara finns en frame s� kommer en
        ny frame att skapas, och (tas bort n�r man �r klar).

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

    (kom-ansaphone-replies-doc . "\
  Den h�r inst�llningen styr automatsvar i detalj. Det g�r att st�lla in 
  svar f�r specifika avs�ndare, mottagare och till och med meddelanden. Om
  meddelandet som skall besvaras inte st�mmer in p� n�got i den h�r listan
  s� anv�nds svarsmeddelandet ovan ist�llet.

  F�r varje m�nster g�r det att st�lla in meddelandetyp, avs�ndare som 
  meddelandet skall ha, mottagare som meddelandet skall ha, ett m�nster
  som meddelandet skall inneh�lla och vilket svar som skall skickas.

  Det �r l�mpligt att st�lla in att inget svar skall skickas p� n�got
  gruppmeddelande eller alarmmeddelande.")

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
  Best�mmer i vilket f�nster man listar medlemskap.")


    (kom-user-prompt-format-doc . "\
  Format f�r LysKOM-prompten. Vissa teckenkombinationer s�tter in speciella
  texter:

    %c - S�tter in kommandot som k�rs om man trycker p� SPC eller RET.
    %[ - S�tter in '[' om automatsvar �r p�slaget.
    %] - S�tter in ']' om automatsvar �r p�slaget.     
    %m - S�tter in information om antal inspelade meddelanden.
    %s - S�tter in LysKOM-systemets namn
    %S - S�tter in LysKOM-serverns namn
    %p - S�tter in namnet p� den person som �r inloggad.
    %w - S�tter in namn p� aktuellt m�te.
    %a - S�tter in \"anonym\"
    %A - S�tter in \"Anonym\"
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
    %] - S�tter in ']' om automatsvar �r p�slaget.     
    %m - S�tter in information om antal inspelade meddelanden.
    %s - S�tter in LysKOM-systemets namn
    %S - S�tter in LysKOM-serverns namn
    %p - S�tter in namnet p� den person som �r inloggad.
    %w - S�tter in namn p� aktuellt m�te.
    %a - S�tter in \"anonym\"
    %A - S�tter in \"Anonym\"
    %# - S�tter in sessionsnummer.
    %  - S�tter in mellanslag om det ser ut att beh�vas.
    %% - S�tter in ett procenttecken.

  N�gra exempel:

    \"%[%c% %m%].\"             Standardprompt
    \"%[%s: K�r %c% %m%]...\"   Till exempel \"LysKOM: K�r Se tiden...\"")

    
    (kom-anonymous-prompt-format-doc . "\
  Format f�r LysKOM-prompten n�r man �r anonym. Vissa teckenkombinationer 
  s�tter in speciella texter:

    %c - S�tter in kommandot som k�rs om man trycker p� SPC eller RET.
    %[ - S�tter in '[' om automatsvar �r p�slaget.
    %] - S�tter in ']' om automatsvar �r p�slaget.     
    %m - S�tter in information om antal inspelade meddelanden.
    %s - S�tter in LysKOM-systemets namn
    %S - S�tter in LysKOM-serverns namn
    %p - S�tter in namnet p� den person som �r inloggad.
    %w - S�tter in namn p� aktuellt m�te.
    %a - S�tter in \"anonym\"
    %A - S�tter in \"Anonym\"
    %# - S�tter in sessionsnummer.
    %  - S�tter in mellanslag om det ser ut att beh�vas.
    %% - S�tter in ett procenttecken.

  N�gra exempel:

    \"%[%c% %m%] - \"             Standardprompt
    \"%[%s: %c% %m%] - \"         Till exempel \"LysKOM: Se tiden - \"")

    (kom-anonymous-prompt-format-executing-doc . "\
  Format f�r LysKOM-prompten efter man trycker p� SPC eller RET och �r
  anonym.  Vissa teckenkombinationer s�tter in speciella texter:

    %c - S�tter in kommandot som k�rs om man trycker p� SPC eller RET.
    %[ - S�tter in '[' om automatsvar �r p�slaget.
    %] - S�tter in ']' om automatsvar �r p�slaget.     
    %m - S�tter in information om antal inspelade meddelanden.
    %s - S�tter in LysKOM-systemets namn
    %S - S�tter in LysKOM-serverns namn
    %p - S�tter in namnet p� den person som �r inloggad.
    %w - S�tter in namn p� aktuellt m�te.
    %a - S�tter in \"anonym\"
    %A - S�tter in \"Anonym\"
    %# - S�tter in sessionsnummer.
    %  - S�tter in mellanslag om det ser ut att beh�vas.
    %% - S�tter in ett procenttecken.

  N�gra exempel:

    \"%[%c% %m%].\"             Standardprompt
    \"%[%s: K�r %c% %m%]...\"   Till exempel \"LysKOM: K�r Se tiden...\"")

    (kom-enabled-prompt-format-doc . "\
  Format f�r LysKOM-prompten n�r man �r i administrat�rsmod. Vissa
  teckenkombinationer s�tter in speciella texter:

    %c - S�tter in kommandot som k�rs om man trycker p� SPC eller RET.
    %[ - S�tter in '[' om automatsvar �r p�slaget.
    %] - S�tter in ']' om automatsvar �r p�slaget.     
    %m - S�tter in information om antal inspelade meddelanden.
    %s - S�tter in LysKOM-systemets namn
    %S - S�tter in LysKOM-serverns namn
    %p - S�tter in namnet p� den person som �r inloggad.
    %w - S�tter in namn p� aktuellt m�te.
    %a - S�tter in \"anonym\"
    %A - S�tter in \"Anonym\"
    %# - S�tter in sessionsnummer.
    %  - S�tter in mellanslag om det ser ut att beh�vas.
    %% - S�tter in ett procenttecken.

  N�gra exempel:

    \"%[%c% %m%] - \"             Standardprompt
    \"%[%s: %c% %m%] - \"         Till exempel \"LysKOM: Se tiden - \"")

    (kom-enabled-prompt-format-executing-doc . "\
  Format f�r LysKOM-prompten efter man trycker p� SPC eller RET och �r i
  administrat�rsmod.  Vissa teckenkombinationer s�tter in speciella texter:

    %c - S�tter in kommandot som k�rs om man trycker p� SPC eller RET.
    %[ - S�tter in '[' om automatsvar �r p�slaget.
    %] - S�tter in ']' om automatsvar �r p�slaget.     
    %m - S�tter in information om antal inspelade meddelanden.
    %s - S�tter in LysKOM-systemets namn
    %S - S�tter in LysKOM-serverns namn
    %p - S�tter in namnet p� den person som �r inloggad.
    %w - S�tter in namn p� aktuellt m�te.
    %a - S�tter in \"anonym\"
    %A - S�tter in \"Anonym\"
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
  Den markeringstyp som anv�nds f�r nya markeringar. En markeringstyp �r ett
  heltal mellan 0 och 255 (inklusive). Om ingen defaultmarkeringstyp �r vald
  fr�gar LysKOM efter markeringstyp varje g�ng man markerar en text. Detta
  kan anv�ndas f�r att h�lla is�r olika sorters markeringar. Till exempel
  kan man markera inl�gg som inneh�ller intressant information med ett visst
  tal och inl�gg som man ska komma ih�g att kommentera vid ett senare
  tillf�lle med ett annat tal.")

    (kom-symbolic-marks-alist-doc . "\
  F�r att slippa beh�va memorera vad man t�nkte sig att de heltaliga
  markeringstyperna ska inneb�ra kan man definiera en lista med symboliska
  markeringstyper. En symbolisk markeringstyp �r en beskrivande str�ng som
  associeras med en heltalig markeringstyp.")

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
  stycken vars rader �r f�r l�nga. Stycken som verkar vara
  f�rformatterade av f�rfattaren bryts dock aldrig om. Det g�r att ange
  en maximal l�ngd i tecken p� inl�gg som f�r brytas om (vilket kan vara
  anv�ndbart f�r att undvika mycket l�nga inl�gg).")

    (kom-autowrap-timeout-doc . "\
  Denna inst�llning begr�nsar hur l�ng tid automatisk ombrytning av
  raderna i ett inl�gg f�r ta (i sekunder). En begr�nsning �r s�rskilt
  anv�ndbar n�r man l�ser mycket l�nga inl�gg. Notera att tiden �r
  h�gst ungef�rlig. Ombrytningen kan p�g� i flera sekunder l�ngre �n
  detta v�rde. Begr�nsningen g�r ocks� att all inl�ggsvisning g�r
  aningen l�ngsammare. Om begr�nsningen �r avst�ngd kan man �nd�
  avbryta ombrytningen genom att trycka C-g.") 

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


    (kom-presence-messages-in-echo-area-doc . "\

  Om man vill ha meddelanden i ekoarean n�r n�gon loggar in, ut eller
  byter namn i LysKOM b�r man sl� p� den h�r inst�llningen. Meddelanden
  visas i ekoarean. Om man vill ha information om endast vissa personer
  g�r det att ange en lista av personer. Vill man inte ha n�gra
  meddelanden alls �r det bara att ange en tom lista.")

    (kom-presence-messages-in-buffer-doc . "\
  Om man vill ha meddelanden i LysKOM-bufferten n�r n�gon loggar in, ut eller
  byter namn i LysKOM b�r man sl� p� den h�r inst�llningen.  Meddelanden
  visas i LysKOM-bufferten. Om man vill ha information om endast vissa 
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

    (kom-remember-password-doc . "\
  Om denna inst�llning �r p�slagen sparas l�senordet till LysKOM-servern
  som en lokal variabel i sessionsbufferten och kan p� s� vis anv�ndas
  vid �terinloggning ifall sessionen kopplas ner. OBS: L�senordet sparas
  i klartext, detta g�r att om n�gon f�r tillg�ng till din LysKOM-
  sessionsbuffer (�ven nedkopplad s�dan) kan han/hon f� reda p� ditt
  l�senord ifall du aktiverar detta val.")

    (kom-idle-hide-doc . "\
  I vilkalistan visas normalt enbart de sessioner som har varit aktiva
  under den senaste tiden. Denna inst�llning best�mmer hur m�nga minuter
  en session f�r ha varit inaktiv och �nd� visas i vilkalistan.")


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
  i st�llet 1002, 1003, 1006, 1004, 1005 och 1007.")


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
  exempel Status (f�r) session och S�nda meddelande). Om det �r avslaget
  kommer TAB att fylla ut �ven med namn p� personer som inte �r inloggade.")


    (kom-membership-default-priority-doc . "\
  Detta best�mmer hur prioriteten p� m�ten man g�r med i s�tts. Om det �r
  en siffra mellan 1 och 255 s� kommer nya medlemskap att f� den prioriteten.
  I annat fall kommer klienten att fr�ga f�r varje nytt medlemskap vilken
  prioritet det skall ha.")

    (kom-membership-default-message-flag-doc . "\
  Detta best�mmer om gruppmeddelanden tas emot i m�ten man g�r med i. `Ja'  
  betyder att gruppmeddelanden tas emot, `Nej' att meddelanden inte tas
  emot. `Fr�ga varje g�ng' att klienten fr�gar n�r du g�r med i m�ten.")

    (kom-show-personal-messages-in-buffer-doc . "\
  Denna inst�llning best�mmer var personliga meddelanden, gruppmeddelanden
  och alarmmeddelanden visas. Meddelanden kan antingen visas i 
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
  Denna inst�llning best�mmer vem som kommer att vara defaultmottagare
  f�r personliga meddelanden man skickar. Alternativen som finns �r
  att meddelanden normalt �r allm�nna, att avs�ndaren f�r det senast
  mottagna meddelandet skall vara default eller att mottagaren f�r det
  senaste gruppmeddelandet (eller avs�ndaren av det senaste personliga
  eller allm�nna meddelandet) skall vara mottagare.")


    (kom-filter-outgoing-messages-doc . "\
  Om denna inst�llning �r p�slagen s� kommer meddelanden som skickas 
  automatiskt, till exempel automatiska svar och svar p� fj�rrkontrollf�rs�k
  �ven att visas som om man hade skickat det manuellt.")

    (kom-friends-doc . "\
  Namnen p� personerna i denna lista kan visas med ett speciellt utseende
  i LysKOM-bufferten. Kommandot Vilka v�nner �r inloggade (och andra
  liknande kommandon) anv�nder ocks� denna lista.")

    (kom-morons-doc . "\
  Namnen p� personerna i denna lista kan visas med ett varnande 
  utseende i LysKOM-bufferten.")

    (kom-person-list-1-doc . "\
  Den h�r listan har ingen f�rutbest�md betydelse, utan kan anv�ndas f�r
  att st�lla in att personerna ska visas med ett speciellt utseende (se
  kom-highlight-conferences) eller f�r att konfigurera in- och utloggningar
  (se kom-presence-messages-in-buffer) med mera.")

    (kom-person-list-2-doc . "\
  Den h�r listan har ingen f�rutbest�md betydelse, utan kan anv�ndas f�r
  att st�lla in att personerna ska visas med ett speciellt utseende (se
  kom-highlight-conferences) eller f�r att konfigurera in- och utloggningar
  (se kom-presence-messages-in-buffer) med mera.")

    (kom-person-list-3-doc . "\
  Den h�r listan har ingen f�rutbest�md betydelse, utan kan anv�ndas f�r
  att st�lla in att personerna ska visas med ett speciellt utseende (se
  kom-highlight-conferences) eller f�r att konfigurera in- och utloggningar
  (se kom-presence-messages-in-buffer) med mera.")

    (kom-person-list-4-doc . "\
  Den h�r listan har ingen f�rutbest�md betydelse, utan kan anv�ndas f�r
  att st�lla in att personerna ska visas med ett speciellt utseende (se
  kom-highlight-conferences) eller f�r att konfigurera in- och utloggningar
  (se kom-presence-messages-in-buffer) med mera.")

    (kom-url-viewer-preferences-doc . "\
  Denna inst�llning best�mmer vilken WWW-l�sare som i f�rsta hand skall 
  anv�ndas f�r att �ppna URLer som f�rekommer i LysKOM. Om den f�rsta l�saren 
  i listan inte klarar den typ av URL som man f�rs�ker �ppna s� anv�nds n�sta
  l�sare och s� vidare.")

    (kom-windows-browser-command-doc . "\
  Denna inst�llning talar om vilket kommando som skall anv�ndas f�r att starta
  en WWW-l�sare i Windows. Om f�ltet �r tomt testas ett antal kommandon som
  typiskt brukar fungera i Windows.")

    (kom-mosaic-command-doc . "\
  Denna inst�llning talar om vilket kommando som skall anv�ndas f�r att starta
  Mosaic.")

    (kom-netscape-command-doc . "\
  Denna inst�llning anger kommandot f�r att k�ra Firefox, Mozilla, Opera 
  eller Netscape.")

    (kom-galeon-command-doc . "\
  Denna inst�llning anger kommandot f�r att k�ra Galeon.")

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
  experter).")

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
  �ndra spr�k ocks�. Det f�rsta spr�k p� listan som �r tillg�ngligt kommer
  att v�ljas av alla klienter med st�d f�r funktionen.")

    (kom-ispell-dictionary-doc . "\
  Ordlista som skall anv�ndas av stavningskontrollen i LysKOM. Om detta
  �r satt till kom-ispell-dictionary s� kommer variabeln ispell-dictionary
  att anv�ndas ist�llet.")

    (kom-show-namedays-doc . "\
  P�slaget inneb�r att dagens namn visas n�r man ser tiden. Det g�r att
  v�lja att se namnsdagarna fr�n namnsdagsl�ngden som h�r till det spr�k
  man f�r n�rvarande anv�nder eller att anv�nda en specifik lista med
  namn.")

    (kom-show-week-number-doc . "\
  Se tiden visar veckonummer om detta �r p�slaget. Annars visas inte 
  veckonummer.")

    (kom-relogin-behaviour-doc . "\
  Denna inst�llning styr om klienten ska f�rs�ka �teransluta om ett 
  kommando ges i en nedkopplad session. P� betyder att det sker 
  automatiskt, av att klienten inte fr�gar om man vill �teransluta.")

    (kom-relogin-inhibit-commands-doc . "\
  Kommandon som inte skall orsaka n�gon �teranslutning. Dessa kommandon
  kommer inte att orsaka att en nedkopplad klient �teransluter.")

    (kom-membership-default-placement-doc . "\
  Denna inst�llning styr var nya medlemskap hamnar. F�rst betyder att nya
  medlemskap hamnar f�re gamla med samma prioritet. Sist betyder att nya 
  medlemskap hamnar efter gamla med samma prioritet. En siffra �r en fix
  placering i medlemskapslistan (men medlemskapet kommer att flyttas
  s�sm�ningom).")

    (kom-show-imported-importer-doc . "\
  P�slaget betyder visa vilken person som importerade ett importerat e-mail.")

    (kom-show-imported-envelope-sender-doc . "\
  Visa den verkliga avs�ndaren till ett importerat e-mail om detta �r 
  p�slaget.")

    (kom-show-imported-external-recipients-doc . "\
  Visa externa mottagare och extra-kopiemottagare om detta �r p�slaget.")

    (kom-show-imported-message-id-doc . "\
  P�slaget betyder visa meddelande-id.")

    (kom-agree-text-doc . "\
  Text som anv�nds av kommandot H�lla med. Det kan antingen vara en text
  eller en lista av texter. Om det �r en lista s� v�ljs ett alternativ
  slumpm�ssigt.")

    (kom-silent-ansaphone-doc . "\
  On detta �r avslaget s� piper inte klienten n�r det kommer meddelanden och
  automatsvar �r p�slaget.")

    (kom-default-session-priority-doc . "\
  L�sniv� f�r nya sessioner. Detta �r den l�sniv� som nya sessioner f�r
  automatiskt.")

    (kom-unsubscribe-makes-passive-doc . "\
  Om detta �r p�slaget s� g�r kommandot Uttr�da ur m�te att man f�rst blir
  passiv medlem i m�tet. Om man uttr�der ur m�tet igen s� g�r man ur p�
  riktigt. Om detta �r avslaget s� g�r man ur m�tet f�rsta g�ngen man f�rs�ker
  uttr�da.")

    (kom-review-priority-doc . "\
  Prioritet f�r �tersekommandon. Detta �r den prioritet som inl�ggen man ser
  n�r man g�r �terse f�r. Det anv�nds f�r att avg�ra om nya inl�gg skall
  avbryta �tersefunktionen. S�tt till 256 eller h�gre om du inte vill att nya
  inl�gg n�gonsin skall avbryta en �tersefunktion.")

    (kom-show-creating-software-doc . "\
  Om detta �r p�slaget s� visas vilken klient som skapade inl�gget tillsammans
  med varje inl�gg, f�rutsatt att informationen finns �verhuvudtaget.")

    (kom-text-footer-format-doc . "\
  Format f�r inl�ggsfoten. I texten ers�tts %n med inl�ggsnumret, %P med
  f�rfattarens namn, %p med f�rfattarens nummer, %f med information om
  inl�gget (ombruten, HTML mm.) och %- med en lagom l�ng streckad linje.
  En siffra efter procenttecknet �r minsta bredd p� texten. Ett minustecken
  f�re siffran betyder att texten skall v�nsterjusteras. Ett likhetstecken
  betyder att f�ltl�ngden �r exakt och texten kanske kortas av.")

    (kom-long-lines-doc . "\
  Om detta �r p�slaget s� g�rs de flesta streckade linjer i klienten mycket
  l�ngre �n normalt.")

    (kom-postpone-default-doc . "\
  Antalet inl�gg som inte skall uppskjutas med Uppskjuta l�sning.")

    (kom-allow-incompleteness-doc . "\
  P�slaget betyder att klienten inte v�ntar p� att information om alla
  ol�sta har kommit fram innan den till�ter kommandon som Lista nyheter.
  Avslaget inneb�r att klienten v�ntar p� information om ol�sta inl�gg om
  den uppt�cker att informationen beh�vs och inte har kommit fram.")

    (kom-smileys-doc . "\
  Gladm�n, som :-), visas grafiskt n�r denna inst�llning �r p�, f�rutsatt
  att Emacs klarar det och paketet smiley.el (en del av Gnus) �r installerat.")

    (kom-ignore-message-senders-doc . "\
  Visa inte personliga, grupp- och alarmmeddelanden fr�n dessa personer.")

    (kom-ignore-message-recipients-doc . "\
  Visa inte gruppmeddelanden riktade till dessa m�ten.")

    (kom-text-footer-dash-length-doc . "\
  Den totala l�ngden p� inl�ggsfoten n�r streckade linjer, men inte extra
  l�nga linjer, och inte ett eget format anv�nds.")

    (kom-text-header-dash-length-doc . "\
  Den totala l�ngden p� den streckade linjen ovanf�r inl�ggstexten n�r
  extra l�nga linjer inte anv�nds.")

    (kom-show-personal-message-date-doc . "\
  N�r detta �r p�slaget s� visas datum och klockslag f�r alla personliga,
  grupp- och alarmmeddelanden.")

    (kom-w3-simplify-body-doc . "\
  N�r detta �r p�slaget s� visas HTML utan de f�rger som anges i HTML-koden.")

    (kom-mercial-doc . "\
  Denna text visas i vilkalistan d� du har l�st f�rdigt alla m�ten.")

    (kom-session-nickname-doc . "\
  Hur sessionsnamn best�ms. Detta �r det namn som visas i mode-raden och
  av Lista sessioner.")

    (kom-server-priority-doc . "\
  Prioritet f�r sessionen. Sessioner med h�g prioritet kan avbryta andra
  sessioner, beroende p� inst�llningen av YYY.")

    (kom-server-priority-breaks-doc . "\
  Denna inst�llning styr hur prioriterade sessioner avbryter andra
  sessioner. En session �r h�gre prioriterad �n en annan om dess
  sessionsprioritet �r h�gre �n den andra sessionens.

  Omedelbart            S� fort der kommer ett inl�gg i en prioriterad 
                        session s� f�r man prompt om att g� till den
                        sessionen.

  Omedelbart om det kommer brev     S� fort det kommer ett personligt
                        brev i en prioriterad session s� f�r man prompt 
                        om att g� till den sessionen.

  Efter aktuell kommentarskedja     Om det kommer inl�gg i en 
                        prioriterad session s� f�r man prompt om att
                        g� till sessionen n�r man har l�st klart den
                        aktuella kommentarskedjan.

  Efter aktuell kommentarskedja om det kommer brev      Om det kommer
                        personliga brev i en prioriterad session s� 
                        f�r man prompt om att g� till sessionen n�r
                        man har l�st klart den aktuella kommentarskedjan.

  Efter aktuellt m�te   Om det kommer inl�gg i en prioriterad session 
                        s� f�r man prompt om att g� till sessionen n�r
                        man har l�st klart det aktuella m�tet.

  Efter aktuellt m�te n�r det kommer brev       Om det kommer brev i en
                        prioriterad session s� f�r man prompt om att g�
                        till sessionen n�r man har l�st klart det 
                        aktuella m�tet.

  N�r allt �r utl�st    Ge prompt om att byta till n�sta LysKOM med 
                        ol�sta inl�gg n�r allt �r klart, oberoende av
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
  N�r detta �r p�slaget och man matar in en text som kan tolkas b�de som
  ett m�tesnamn eller ett m�tesnummer vid inl�sning av m�tesnamn, s�
  tolkar klienten det i f�rsta hand som ett nummer. N�r detta �r avslaget
  s� tolkas det i f�rsta hand som ett m�tesnamn.")
    (kom-keep-alive-interval-doc . "\
  Antalet sekunder som skall g� mellan de meddelanden som anv�nds f�r att
  h�lla n�tverkskopplingen till LysKOM-servern aktiv. Om din f�rbindelse
  kopplar ned efter en stunds inaktivitet s� kan du s�tta detta till ungef�r 
  h�lften och sedan ge kommandot \"H�ll f�rbindelsen aktiv\".")
    (kom-lost-session-notification-doc . "\
  Hur notifiering av onormalt avslutade session sker. Ett meddelande skrivs
  alltid ut i den avslutade sessionens LysKOM-buffert. Om denna inst�llning
  �r satt till \"Pip\" kommer klienten dessutom att pipa. Om du har valt \"Visa
  notifiering i alla aktiva LysKOM-buffrar\" kommer klienten att pipa och
  skriva ut ett kort meddelande i alla aktiva LysKOM-buffrar.")
    (kom-text-no-prompts-doc . "\
  Den h�r inst�llningen styr vilka kommandon som alltid fr�gar efter 
  textnummer (om man inte anger ett explicit som prefixargument). F�r 
  kommandon som inte st�r med h�r anv�nds inbyggda standardinst�llningar.")
    (kom-saved-file-name-doc . "\
  Anger vilken fil som inl�gg skall arkiveras till. Klienten kommer �nd�
  att fr�ga, men detta blir det f�rifyllda svaret.")
    (kom-follow-attachments-doc . "\
  Anger om bilagor till importerade brev skall l�sas som kommentarer. 
  Om detta �r avst�ngt s� l�smarkeras bilagor automatiskt, utan att
  man l�ser dem.")
    (kom-show-unread-in-frame-title-doc . "\
  Om detta �r p�slaget s� visas \"(Ol�sta)\" i titelraden f�r de
  f�nster vars valda buffert h�r till en LysKOM-session med ol�sta. 
  Det �r inte s�kert att titelraden �ndras om f�nstret inte �r �ppet.")
    (kom-unread-mode-line-type-doc . "\
  Denna inst�llning best�mmer hur ol�sta indikeras i mode-raden. `Enkel'
  betyder en gemensam indikering f�r samtliga sessioner ifall du har 
  ol�sta texter eller brev. 

  `Visa alla sessioner med ol�sta' betyder att alla sessioner med ol�sta 
  texter indikeras separat. F�rekomst med brev indikeras genom att 
  sessionsnamnet visas med stora bokst�ver (till exempel LYSLYS ist�llet 
  f�r LysLys). Om sessionsnamnet bara inneh�ller versaler (exempelvis 
  LYSLYS) visas det mellan asterisker (*LYSLYS*).

  Notera att denna inst�llning b�r vara samma f�r alla aktiva 
  LysKom-sessioner varf�r det troligen �r b�st att spara den i
  din .emacs.")

   (kom-created-texts-are-saved-doc . "\
  Om detta �r ett filnamn s� sparas alla inl�gg man skriver till den
  filen i ungef�r samma format som de har n�r man l�ser dem. Inl�gg
  som inte kan skapas sparas inte.")
    (kom-confirm-add-recipients-doc . "\
  Om detta �r p�slaget och man adderar en mottagare till ett inl�gg s�
  fr�gar klienten om man vill att kommentarer till inl�gget ocks�
  skall skickas till den nya mottagaren. Om detta �r avslaget s�
  kommer kommentarer alltid att skickas �ven till den nya mottagaren. 
  Det �r oftast bra att ha p�slaget.")
    (kom-trim-buffer-minimum-doc . "\
  Om du begr�nsar LysKOM-buffertens storlek s� m�ste den bli s� h�r mycket
  f�r stor innan text i bufferten raderas. Variablen ska vara ett heltal,
  default �r 4096.")
    (kom-dont-check-commented-authors-doc . "")
    (kom-print-relative-dates-doc . "\
  Tidsangivelsen f�r inl�gg (och en del andra saker) kommer att bli
  \"idag\" alternativt \"ig�r\" om denna inst�llning �r p�slagen och
  inl�gget skrevs idag alternativt ig�r. Om den �r avslagen kommer ett
  absolut datum alltid att visas.")
    (kom-print-seconds-in-time-strings-doc . "\
  Om denna inst�llning �r p�slagen kommer en del tidsangivelser (till
  exempel f�r visade inl�gg och s�nda meddelanden) att inkludera
  sekunder.")
    (kom-review-uses-cache-doc . "\
  Om detta �r p�slaget s� kommer kommandon som �terser inl�gg att 
  anv�nda sparade kopior av inl�ggen om de finns. Det g�r att kommandona
  g�r lite snabbare, men det finns en viss risk f�r att ett inl�gg kan
  ha �ndrats sedan kopian sparades.")
    (kom-review-marks-texts-as-read-doc . "\
  Om detta �r p�slaget s� kommer kommandon som �terser inl�gg att 
  l�smarkera de inl�gg som visas. I annat fall l�mnar �tersekommandona
  din l�shistorik or�rd, precis som vanligt.")
    (kom-auto-review-faqs-doc . "\
  Om detta �r p�slaget s� kommer ol�sta FAQer inte att �terses automatiskt
  n�r du loggar in eller g�r till ett m�te med en ol�st FAQ.")
    (kom-auto-list-faqs-doc . "\
  Om detta �r p�slaget s� kommer ol�sta FAQer att listas automatiskt n�r
  du loggar in eller g�r till ett m�te med en ol�st FAQ.")
    (kom-extended-status-information-doc . "\
  Om detta �r p�slaget s� visas extra statusinformation i kommandona
  Status (f�r) person, Status (f�r) m�te och Status (f�r) servern 
  (om s�dan information finns tillg�nglig).")
    (kom-highlight-first-line-doc . "\
  Om detta �r p�slaget s� visas f�rsta raden i varje inl�gg (den med
  inl�ggsnummer, datum och f�rfattare) med ett annorlunda utseende �n
  normal text.")
    (kom-highlight-dashed-lines-doc . "\
  Om detta �r p�slaget s� visas raderna f�re och efter inl�ggstexten med
  ett annorlunda utseende �n normal text.")
    (kom-highlight-text-body-doc . "\
  Om detta �r p�slaget s� visas inl�ggstexter med ett annorlunda utseende
  �n normal text.")
    (kom-async-highlight-dashed-lines-doc . "\
  Om detta �r p�slaget s� visas raderna f�re och efter personliga, grupp-
  och alarmmeddelanden med ett annorlunda utseende �n normal text.")
    (kom-async-highlight-text-body-doc . "\
  Om detta �r p�slaget s� visas personliga, grupp- och alarmmeddelanden 
  med ett annorlunda utseende �n normal text.")
    (kom-edit-hide-add-button-doc . "\
  Om detta �r p�slaget s� kommer knappen [Addera...] att visas efter
  mottagare och till�ggsinformation n�r man skriver inl�gg.")
    (kom-format-html-authors-doc . "\
  Den h�r inst�llningen styr vilka f�rfattares HTML-meddelanden som
  elispklienten kommer att f�rs�ka formattera.")
    (kom-format-show-images-doc . "\
  Den h�r inst�llningen styr om inl�gg uppm�rkta som bilder specialhanteras
  eller visas som traditionella inl�gg.")
    (kom-keyboard-menu-immediate-selection-doc . "\
  Den h�r inst�llningen styr huruvida snabbtangenter automatiskt bekr�ftar
  val i textbaserade menyer eller inte. Om detta �r p�slaget s� m�ste man
  alltid trycka retur f�r att v�lja i textbaserade menyer.")
    (kom-max-overlays-doc . "\
  Med denna inst�llning kan man begr�nsa antalet f�rgmarkeringar (som
  anv�nds bland annat f�r f�rgade inl�ggsbakgrunder). Genom att begr�nsa
  antalet markeringar kan scrollning g� mycket fortare. Varje inl�gg
  beh�ver mellan 1 och 4 markeringar beroende p� andra inst�llningar.")
    (kom-mark-read-texts-as-read-in-new-recipient-doc . "\
  Om detta �r p�slaget s� l�smarkeras redan l�sta inl�gg automatiskt i
  m�ten de adderas till medan man �r inloggad. Om man inte �r inloggad
  s� sker inte denna l�smarkering. Inl�gg som adderas till ens brevl�da
  l�smarkeras dock aldrig automatiskt.")
    (kom-url-transformation-rules-doc . "\
  Regler f�r att transformera URLer innan de skickas till en webbl�sare.
  \"Fr�n\" �r ett m�nster (en regexp) som skall �vers�ttas till texten
  i \"Till\". Dokumentation om regexpar finns i Emacs-manualen. \"Till\" 
  kan inneh�lla f�ljande specialtecken:

      \\& ers�tts med den text som matchar regexpen
      \\N ers�tts med den text som matchar \(...\)-uttryck N i regexpen
      \\\\ ers�tts med en \\.")
    (kom-text-links-doc . "\
  Definition av textl�nkar. Varje rad definierar en l�nk f�r inl�gg i ett
  visst m�te. Alla f�rekomster av l�nkm�nstret blir URL-l�nkar genom att
  texten som l�nkm�nstret matchar substitueras enligt l�nksubstitutionen.
  Den text som faktist visas som l�nk i LysKOM-bufferten best�ms av 
  inst�llningen \"Visa grupp\". Anv�nd 0 f�r att visa hela den matchande
  texten eller en siffta fr�n 1 till 9 f�r att visa den N-te gruppen i
  l�nkm�nstret (anv�nd 0 om du inte f�rst�r vad som avses med detta). 
  Om inst�llningen \"Versaler och gemener lika\" �r p� s� r�knas versaker
  och gemener av samma bokstav som lika.

  L�nksubstitutionen kan inneh�lla f�ljande specialtecken:

      \\& ers�tts med den text som matchar regexpen
      \\N ers�tts med den text som matchar \(...\)-uttryck N i regexpen
      \\\\ ers�tts med en \\.")

    (kom-active-face-doc . "\
  Teckensnitt f�r klickbara texter f�rutom URLer, m�ten, v�nner och
  bekanta, idioter och kn�ppskallar, och inl�ggsnummer. V�rdet skall 
  vara namnet p� en s� kallad `face'.")
    (kom-active-highlight-face-doc . "\ 
  Teckensnitt avsett f�r klickbara texter som skall markeras extra
  tydligt. V�rdet skall vara namnet p� en s� kallad `face'.")    
    (kom-active-strikethrough-face-doc . "\ 
  Teckensnitt avsett f�r klickbara texter som skall markeras extra
  tydligt. V�rdet skall vara namnet p� en s� kallad `face'.")
    (kom-url-face-doc . "\
  Teckensnitt f�r klickbara URLer.")
    (kom-me-face-doc . "\
  Teckensnitt f�r ens eget namn.")
    (kom-highlight-face-doc . "\
  Teckensnitt f�r markering av klickbara ytor n�r muspekaren f�rs �ver
  dem.")
    (kom-subject-face-doc . "\
  Teckensnitt f�r �renderader.")
    (kom-text-no-face-doc . "\
  Teckensnitt f�r inl�ggsnummer.")
    (kom-friends-face-doc . "\
  Teckensnitt f�r v�nner och bekanta. De personer som r�knas upp i
  listan \"v�nner och bekanta\" kommer att markeras med detta
  teckensnitt.")
    (kom-morons-face-doc . "\
  Teckensnitt f�r idioter och kn�ppskallar. De personer som r�knas upp
  i listan \"Idioter och kn�ppskallar\" kommer att markeras med detta
  teckensnitt.")
    (kom-presence-face-doc . "\
  Teckensnitt f�r n�rvaromeddelanden.")
    (kom-first-line-face-doc . "\
  Teckensnitt f�r f�rsta raden i inl�ggshuvudet. Detta teckensnitt
  �verlagras p� standardteckensnitten i f�rsta raden, s� de egenskaper
  som inte s�tts av detta teckensnitt kommer inte att p�verkas.")
    (kom-warning-face-doc . "\
  Teckensnitt f�r viktiga varningar.")
    (kom-mark-face-doc . "\
  Teckensnitt f�r diverse markeringar. Bland annat anv�nds detta
  teckensnitt f�r markeringen n�r elispklienten bl�ddrar mindre �n en
  hel sk�rmsida. Detta teckensnitt �verlagras p� standardteckensnitten
  d�r det anv�nds, s� de egenskaper som inte s�tts av detta
  teckensnitt kommer inte att p�verkas.")
    (kom-dim-face-doc . "\
  Teckensnitt f�r diverse diskreta texter. Detta teckensnitt anv�nds
  bland annat f�r att visa passiva medlemskap.")
    (kom-text-body-face-doc . "\
  Teckensnitt f�r inl�ggstexter. Detta teckensnitt �verlagras p�
  standardteckensnitten i inl�ggstexten, s� de egenskaper som inte
  s�tts av detta teckensnitt kommer inte att p�verkas.")
    (kom-dashed-lines-face-doc . "\
  Teckensnitt f�r streckrader runt inl�gg. Detta teckensnitt
  �verlagras p� standardteckensnitten i streckraden, s� de egenskaper
  som inte s�tts av detta teckensnitt kommer inte att p�verkas.")
    (kom-async-text-body-face-doc . "\
  Teckensnitt f�r meddelanden. Detta teckensnitt �verlagras p�
  standardteckensnitten i meddelandet, s� de egenskaper som inte
  s�tts av detta teckensnitt kommer inte att p�verkas.")
    (kom-async-dashed-lines-face-doc . "\
  Teckensnitt f�r streckrader runt meddelanden. Detta teckensnitt
  �verlagras p� standardteckensnitten i streckraderna, s� de
  egenskaper som inte s�tts av detta teckensnitt kommer inte att
  p�verkas.") 
    (kom-lynx-terminal-doc . "\
  Lynx kan k�ras inuti Emacs eller i en xterm (eller motsvarande). H�r
  v�ljer du vilket.")
    (kom-lynx-terminal-command-doc . "\
  Kommandot som anv�nds f�r att starta Lynx i Emacs terminal-mode. 
  Ska normalt vara s� enkelt som \"lynx\".")
    (kom-lynx-xterm-command-doc . "\
  Kommandot som anv�nds f�r att starta Lynx i en xterm. Kommandot ska
  starta b�de xterm och lynx.")
    (kom-show-sync-messages-doc . "\
  Om denna inst�llning �r p�slagen s� kommer klienten att visa ett
  meddelande i ekoarean n�t servern signallerar att den h�ller p�
  spara databasen.")
    (kom-highlight-conferences-doc . "\
  Denna inst�llning styr hur m�tesnamn f�rgl�ggs. F�r varje val
  kan ett utseende anges. Utseendena �r namn p� faces i Emacs.")
    (kom-netscape-variant-doc . "\
  Denna inst�llning styr var elispklienten f�rs�ker �ppna URLer. Hur
  bra de olika valen fungerar beror p� vilken variant av Firefox, 
  Mozilla, Netscape eller Opera man anv�nder, och till och med vilket 
  operativsystem man anv�nder.")
    (kom-auto-confirm-new-conferences-doc . "\
  Om den h�r inst�llningen �r p�slagen s� kommer kommandona \"Lista
  nya m�ten\" och \"Lista nya personer\" automatiskt markera nya
  m�ten och personer som k�nda.")
    (kom-text-properties-doc . "\
  Om den h�r inst�llningen �r p� s� kommer klienten att anv�nda olika
  typsnitt och f�rger till olika saker. Till exempel kommer textnummer
  och annat man kan klicka p� att visas med en avvikande f�rg.")
    (kom-fontify-text-doc . "\
  Om den h�r inst�llningen �r p� s� kommer fet och kursiv stil att
  anv�ndas i inl�ggstext. Till exempel s� kommer ord och fraser
  omgivna av understrykningstecken att visas med kursiv text.")

    ;;
    ;; Tags for variables
    ;;

    (kom-ansaphone-replies-tag . "Detaljerade automatsvar")
    (kom-bury-buffers-tag . "Begrav buffertar n�r man byter LysKOM:")

    (kom-personal-messages-in-window-tag . "Personliga meddelanden:   ")
    (kom-customize-in-window-tag       . "Inst�llningar f�r LysKOM: ")
    (kom-write-texts-in-window-tag     . "Skriv inl�gg:             ")
    (kom-prioritize-in-window-tag      . "Prioritera m�ten:         ")
    (kom-edit-filters-in-window-tag    . "�ndra filter:             ")
    (kom-view-commented-in-window-tag  . "�terse kommentarer:       ")
    (kom-list-membership-in-window-tag . "Lista medlemskap:         ")

    (kom-user-prompt-format-tag . "Promptformat:")
    (kom-user-prompt-format-executing-tag . "Promptformat vid k�rning:")
    (kom-anonymous-prompt-format-tag . "Promptformat (anonym):")
    (kom-anonymous-prompt-format-executing-tag . "Promptformat vid k�rning (anonym):")
    (kom-enabled-prompt-format-tag . "Promptformat (admin):")
    (kom-enabled-prompt-format-executing-tag . "Promptformat vid k�rning (admin):")

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
    (kom-membership-default-message-flag-tag . 
"Mottag gruppmeddelanden f�r nya medlemskap: ")
    (kom-dashed-lines-tag . 
"Streckade linjer kring inl�ggstexten:     ")
    (kom-autowrap-tag . 
"Automatisk radbrytning vid l�sning:       ")
    (kom-autowrap-timeout-tag . 
"Tidsbegr�nsning av automatisk radbrytning: ")
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

    (kom-presence-messages-in-echo-area-tag . 
"N�rvaromeddelanden i ekoarean:        ")
    (kom-presence-messages-in-buffer-tag .
"N�rvaromeddelanden i LysKOM-bufferten:")
    (kom-page-before-command-tag . "Rensa sk�rmen:")

    (kom-remember-password-tag . "Kom ih�g LysKOM-sessionsl�senordet:")

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
    (kom-ding-on-new-letter-tag        . "N�r det kommer brev:                ")
    (kom-ding-on-priority-break-tag    . "N�r det kommer prioriterade inl�gg: ")
    (kom-ding-on-wait-done-tag         . "N�r man har v�ntat klart:           ")
    (kom-ding-on-common-messages-tag   . "Vid alarmmeddelanden:               ")
    (kom-ding-on-group-messages-tag    . "Vid gruppmeddelanden:               ")
    (kom-ding-on-personal-messages-tag . "Vid personliga meddelanden:         ")
    (kom-ding-on-no-subject-tag        . "N�r �renderad saknas:               ")

    (kom-filter-outgoing-messages-tag . "Visa automatiska meddelanden:")
    (kom-friends-tag . "V�nner och bekanta:")
    (kom-morons-tag . "Idioter och kn�ppskallar:")
    (kom-person-list-1-tag . "M�tes/personlista (1):")
    (kom-person-list-2-tag . "M�tes/personlista (2):")
    (kom-person-list-3-tag . "M�tes/personlista (3):")
    (kom-person-list-4-tag . "M�tes/personlista (4):")
    (kom-url-viewer-preferences-tag . "�ppna URLer med f�ljande program:")
    (kom-windows-browser-command-tag . "Kommando f�r att starta en WWW-l�sare i Windows:")
    (kom-mosaic-command-tag . "Kommando f�r att starta NCSA Mosaic:")
    (kom-netscape-command-tag . "Kommando f�r att starta Netscape-liknande WWW-l�sare:")
    (kom-galeon-command-tag . "Kommando f�r att starta Galeon:")

    (kom-symbolic-marks-alist-tag . "Symboliska markeringstyper:")

    (kom-cite-string-tag . "Citatmarkering:")
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
    (kom-default-language-tag . "Spr�klista:")
    (kom-ispell-dictionary-tag . "Ordlista:")
    (kom-show-namedays-tag .    "Visa dagens namn:")
    (kom-show-week-number-tag . "Visa veckonummer:")

    (kom-relogin-behaviour-tag . "�teranslut automatiskt:")
    (kom-relogin-inhibit-commands-tag . "Kommandon f�r vilka �teranslutning aldrig sker automatiskt:")
    (kom-membership-default-placement-tag . "Placering av nya medlemskap:")
    (kom-show-imported-importer-tag . "Visa import�r av importerade inl�gg:")
    (kom-show-imported-envelope-sender-tag . "Visa avs�ndare av importerade inl�gg:")
    (kom-show-imported-external-recipients-tag . "Visa externa mottagare till importerade inl�gg:")
    (kom-show-imported-message-id-tag . "Visa meddelande-id f�r importerade inl�gg:")
    (kom-agree-text-tag . "Text f�r H�lla med:")
    (kom-silent-ansaphone-tag . "Pip n�r automatsvar �r p�slaget:")
    (kom-default-session-priority-tag . "L�sniv� f�r nya sessioner:")
    (kom-unsubscribe-makes-passive-tag . "Uttr�da ur m�te g�r om medlemskap till passiva:")
    (kom-review-priority-tag . "Prioritet f�r �tersekommandon:")
    (kom-show-creating-software-tag . "Visa skapande klient:")
    (kom-text-footer-format-tag . "Format f�r inl�ggsfot:")
    (kom-long-lines-tag . "L�nga streckade linjer:")
    (kom-postpone-default-tag . "Sk�nsv�rde f�r uppskjuta l�sning:")
    (kom-allow-incompleteness-tag . "Till�t ofullst�ndig information om ol�sta:")
    (kom-smileys-tag . "Visa gladm�n grafiskt:")
    (kom-ignore-message-senders-tag . "Visa inte meddelanden fr�n:")
    (kom-ignore-message-recipients-tag . "Visa inte meddelanden till:")
    (kom-text-footer-dash-length-tag . "Inl�ggsfotens l�ngd:")
    (kom-text-header-dash-length-tag . "L�ngden p� linjen ovanf�r inl�ggstexten:")
    (kom-show-personal-message-date-tag . "Visa datum f�r alla meddelanden:")
    (kom-w3-simplify-body-tag . "Visa HTML utan standardf�rger:")
    (kom-mercial-tag . "Text n�r allt �r utl�st:")
    (kom-session-nickname-tag . "Hur best�ms sessionsnamn:")
    (kom-server-priority-tag . "Sessionsprioritet:")   
    (kom-server-priority-breaks-tag . "Byt till LysKOM med ol�sta:")
    (kom-complete-numbers-before-names-tag . "L�s m�tesnummer f�re m�tesnamn:")
    (kom-keep-alive-interval-tag . "Intervall f�r h�ll f�rbindelsen ig�ng:")
    (kom-lost-session-notification-tag . "Notifiering av onormalt avslutade sessioner:")
    (kom-text-no-prompts-tag . "Hur kommandon fr�gar efter textnummer:")
    (kom-saved-file-name-tag . "Fil att arkivera inl�gg i:")
    (kom-follow-attachments-tag . "L�s importerade bilagor som vanligt:")
    (kom-show-unread-in-frame-title-tag . "Visa ol�stamarkering i titelraden:")
    (kom-unread-mode-line-type-tag . "Typ av ol�staindikering i mode-raden:")
    (kom-created-texts-are-saved-tag . "Spara f�rfattade inl�gg:")
    (kom-confirm-add-recipients-tag . "Fr�ga om kommentarer skall s�ndas till nya mottagare:")
    (kom-trim-buffer-minimum-tag . "Minsta storlek av LysKOM-bufferten som tas bort:")
    (kom-dont-check-commented-authors-tag . "F�rfattare som inte kontrolleras:")
    (kom-print-relative-dates-tag . "Visa relativa datumangivelser:")
    (kom-print-seconds-in-time-strings-tag . "Inkludera sekunder i tidsangivelser:")
    (kom-review-uses-cache-tag . "�tersekommandon anv�nder sparade inl�gg:")
    (kom-review-marks-texts-as-read-tag . "�tersekommandon l�smarkerar visade texter:")
    (kom-auto-review-faqs-tag . "Visa nya FAQer automatiskt:")
    (kom-auto-list-faqs-tag . "Lista nya FAQer automatiskt:")
    (kom-extended-status-information-tag . "Ut�kad statusinformation:")
    (kom-highlight-first-line-tag . "Annorlunda bakgrundsf�rg p� f�rsta raden:")
    (kom-highlight-dashed-lines-tag . "Annorlunda bakgrundsf�rg f�re och efter inl�ggstexten:")
    (kom-highlight-text-body-tag . "Annorlunda bakgrundsf�rg p� inl�ggstexten:")
    (kom-async-highlight-dashed-lines-tag . "Annorlunda bakgrundsf�rg f�re och efter meddelanden:")
    (kom-async-highlight-text-body-tag . "Annorlunda bakgrundsf�rg p� meddelanden:")
    (kom-edit-hide-add-button-tag . "Visa Addera-knappen n�r man skriver inl�gg:")
    (kom-format-html-authors-tag . "Formattera HTML-meddelanden per f�rfattare:")
    (kom-format-show-images-tag . "Visa bildinl�gg som bilder:")
    (kom-keyboard-menu-immediate-selection-tag . "Snabbval i textmenyer kr�ver bekr�ftelse:")
    (kom-max-overlays-tag . "H�gsta antal f�rgmarkeringar:")
    (kom-mark-read-texts-as-read-in-new-recipient-tag . "Markera l�sta inl�gg l�sta �ven i nya mottagare:")
    (kom-url-transformation-rules-tag . "Transformation av URLer:")
    (kom-text-links-tag . "Textl�nkar:")
    (kom-active-face-tag . "�vriga klickbara texter:")
    (kom-active-highlight-face-tag . "Tydligare markerade klickbara texter:")
    (kom-active-strikethrough-face-tag . "�verstrukna klickbara texter:")
    (kom-url-face-tag . "Klickbara URLer:")
    (kom-me-face-tag . "Mitt eget namn:")
    (kom-highlight-face-tag . "Markering av klickbara ytor:")
    (kom-subject-face-tag . "�renderader:")
    (kom-text-no-face-tag . "Inl�ggsnummer:")
    (kom-friends-face-tag . "V�nner och bekanta:")
    (kom-morons-face-tag . "Idioter och kn�ppskallar:")
    (kom-presence-face-tag . "N�rvaromeddelanden:")
    (kom-first-line-face-tag . "F�rsta raden i inl�ggshuvudet:")
    (kom-warning-face-tag . "Viktiga varningar:")
    (kom-mark-face-tag . "Diverse markeringar:")
    (kom-dim-face-tag . "Diverse diskreta texter:")
    (kom-text-body-face-tag . "Inl�ggstexter:")
    (kom-dashed-lines-face-tag . "Streckrader runt inl�gg:")
    (kom-async-text-body-face-tag . "Meddelanden:")
    (kom-async-dashed-lines-face-tag . "Streckrader runt meddelanden:")
    (kom-lynx-terminal-command-tag . "Kommando f�r Lynx i Emacs:")
    (kom-lynx-terminal-tag . "Var ska Lynx k�ra:")
    (kom-lynx-xterm-command-tag . "Kommando f�r Lynx i xterm:")
    (kom-show-sync-messages-tag . "Visa databasen-sparar-meddelanden:")
    (kom-highlight-conferences-tag . "F�rgl�ggning av namn:")
    (kom-netscape-variant-tag . "Var ska Netscape-liknande WWW-l�sare �ppna URLer:")
    (kom-auto-confirm-new-conferences-tag . "Markera alltid nya m�ten och personer som k�nda:")
    (kom-text-properties-tag . "Anv�nd typsnitt och f�rger:")
    (kom-fontify-text-tag . "Visa fetstil och kursiv text i inl�gg:")
    )
)

  
;;;; ============================================================
;;;; The default Ansaphone message goes here. The more complex 
;;;; message specification probably should too, but it's not here
;;;; yet. People who know how to use it are smart enough to do it
;;;; right.

(lyskom-language-var local kom-ansaphone-default-reply sv
  "Jag l�ser inte LysKOM just nu. Skicka g�rna ett brev i st�llet.")

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
  '((error-0 . "Inget fel har intr�ffat")
    (error-2 . "�nnu ej implementerat i servern")
    (error-3 . "Inte l�ngre implementerat i servern")
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
    (error-18 . "Namnet f�r kort, f�r l�ngt eller inneh�ller felaktiga tecken")
    (error-19 . "Index utanf�r gr�nserna")
    (error-20 . "M�tet existerar redan")
    (error-21 . "Personen existerar redan")
    (error-22 . "Hemligt, men ej l�sskyddat")
    (error-23 . "Du f�r inte �ndra person/m�tesflaggan")
    (error-24 . "Fel i databasen. Attans otur.")
    (error-25 . "Otill�ten mottagartyp eller kommentarsl�nk")
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
    (error-53 . "Mottagaren var kanske inte inloggad")
    (error-54 . "Otill�ten typ p� medlemskap")
    (error-55 . "Ogiltigt intervall")
    (error-56 . "Ogiltig kombination av intervall")
    (error-57 . "Odefinierad m�tning")
    (error-58 . "Otill�ten prioritet")
    (error-59 . "Otill�ten viktning")
    (error-60 . "Viktning noll ej giltig")
    (error-61 . "Ogiltigt sanningsv�rde")

    (error--1 . "Du �r inte medlem i n�got mottagarm�te")
))

(lyskom-language-var global lyskom-unread-mode-line sv
  (lyskom-make-lyskom-unread-mode-line))

(lyskom-language-var global lyskom-unread-title-format sv
  (lyskom-make-lyskom-unread-title-format))

(lyskom-language-var local lyskom-help-data sv lyskom-sv-help-data)

(provide 'lyskom-language-sv)

;;; swedish-strings.el ends here
