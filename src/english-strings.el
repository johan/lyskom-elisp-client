;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: english-strings.el,v 44.281 2003-04-21 16:15:17 byers Exp $
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
;;;; File: english-strings.el
;;;;
;;;; This file contains all strings in the LysKOM elisp client.
;;;; Language:     English.
;;;; ================================================================
;;;;
;;;; Translation from swedish-strings.el: David Byers
;;;;

(require 'lyskom-vars "vars")
(require 'lyskom-language "language")

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
              "$Id: english-strings.el,v 44.281 2003-04-21 16:15:17 byers Exp $"))


;;; ================================================================
;;; The language definition

(lyskom-define-language 'en
                        'iso-8859-1
                        "English"
                        "Engelska")


;;; ================================================================
;;; lyskom-edit-mode-map                             English version

(defvar lyskom-en-edit-mode-map nil)
(lyskom-language-keymap lyskom-edit-mode-map en lyskom-en-edit-mode-map)

;;; Set the keymap for lyskom-edit-mode

(defvar lyskom-en-edit-prefix nil)

(if lyskom-en-edit-mode-map
    nil
  (setq lyskom-en-edit-mode-map (make-sparse-keymap))
  (define-prefix-command 'lyskom-en-edit-prefix)
  (define-prefix-command 'lyskom-en-edit-review-prefix)
  (define-prefix-command 'lyskom-en-edit-insert-prefix)
  (define-prefix-command 'lyskom-en-edit-aux-prefix)
  (define-prefix-command 'lyskom-en-edit-add-prefix)
  (define-key lyskom-en-edit-mode-map (kbd (lyskom-keys (lyskom-xemacs-or-gnu 'button2 'button2up))) 'kom-button-click-or-yank)
  (define-key lyskom-en-edit-mode-map (kbd (lyskom-keys (lyskom-xemacs-or-gnu 'button2up 'button2))) 'kom-mouse-null)
  (define-key lyskom-en-edit-mode-map (kbd (lyskom-keys 'button3)) 'kom-popup-menu)
  (define-key lyskom-en-edit-mode-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lyskom-en-edit-mode-map (kbd "*")     'kom-button-press-or-self-insert-command)
  (define-key lyskom-en-edit-mode-map (kbd "=")     'kom-menu-button-press-or-self-insert-command)
  (define-key lyskom-en-edit-mode-map (kbd "TAB")   'kom-edit-next-button-or-self-insert)
  (define-key lyskom-en-edit-mode-map (kbd "M-TAB") 'kom-edit-prev-button)
  (define-key lyskom-en-edit-mode-map (kbd "<S-tab>") 'kom-edit-prev-button)
  (define-key lyskom-en-edit-mode-map (kbd "C-c")	'lyskom-en-edit-prefix)
  (define-key lyskom-en-edit-prefix (kbd "C-x") 'lyskom-en-edit-aux-prefix)
  (define-key lyskom-en-edit-prefix (kbd "?")	'lyskom-help)
  (define-key lyskom-en-edit-prefix (kbd "C-r") 'lyskom-en-edit-review-prefix)
  (define-key lyskom-en-edit-prefix (kbd "C-i") 'lyskom-en-edit-insert-prefix)
  (define-key lyskom-en-edit-prefix (kbd "TAB") 'lyskom-en-edit-insert-prefix)
  (define-key lyskom-en-edit-prefix (kbd "*") 'kom-button-press)
  (define-key lyskom-en-edit-prefix (kbd "=") 'kom-menu-button-press)
  (define-key lyskom-en-edit-prefix (kbd "C-c")	'kom-edit-send)
  (define-key lyskom-en-edit-prefix (kbd "C-s") 'kom-ispell-message)
  (define-key lyskom-en-edit-prefix (kbd "C-k")	'kom-edit-quit)
  (define-key lyskom-en-edit-prefix (kbd "r ?")	'lyskom-help)
  (define-key lyskom-en-edit-prefix (kbd "C-r C-c") 'kom-edit-show-commented)
  (define-key lyskom-en-edit-prefix (kbd "C-i ?") 'lyskom-help)
  (define-key lyskom-en-edit-prefix (kbd "C-i C-c") 'kom-edit-insert-commented)
  (define-key lyskom-en-edit-prefix (kbd "C-y") 'kom-edit-insert-commented)
  (define-key lyskom-en-edit-prefix (kbd "C-i C-y") 'kom-edit-insert-commented)
  (define-key lyskom-en-edit-prefix (kbd "C-b")     'kom-edit-insert-buglist)
  (define-key lyskom-en-edit-prefix (kbd "C-i 1") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 2") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 3") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 4") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 5") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 6") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 7") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 8") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 9") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i SPC") 'kom-edit-insert-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i C-l") 'kom-edit-insert-link)
  (define-key lyskom-en-edit-prefix (kbd "C-a") 'lyskom-en-edit-add-prefix)
  (define-key lyskom-en-edit-prefix (kbd "C-a C-r") 'kom-edit-add-recipient)
  (define-key lyskom-en-edit-prefix (kbd "C-a C-c") 'kom-edit-add-copy)
  (define-key lyskom-en-edit-prefix (kbd "C-a <RET>") 'kom-edit-move-text)
  (define-key lyskom-en-edit-prefix (kbd "C-a C-x") 'kom-edit-add-cross-reference)
  (define-key lyskom-en-edit-prefix (kbd "C-x C-p") 'kom-edit-add-personal-comments)
  (define-key lyskom-en-edit-prefix (kbd "C-x C-n") 'kom-edit-add-no-comments)
  (define-key lyskom-en-edit-prefix (kbd "C-x C-r") 'kom-edit-add-read-confirm-request)
  (define-key lyskom-en-edit-prefix (kbd "C-a ?") 'lyskom-help))



(lyskom-language-missing-string lyskom-message cgdag en)
(lyskom-language-missing-string lyskom-message sixjune en)
(lyskom-language-missing-string lyskom-message holdnose en)
(lyskom-language-missing-string lyskom-message 13dayxmas en)
(lyskom-language-missing-string lyskom-message 20dayxmas en)
(lyskom-language-missing-string lyskom-message kyndeldag en)
(lyskom-language-missing-string lyskom-message skottdag en)
(lyskom-language-missing-string lyskom-message intwomday en)
(lyskom-language-missing-string lyskom-message mariebdag en)
(lyskom-language-missing-string lyskom-message johannesddag en)
(lyskom-language-missing-string lyskom-message fnday en)
(lyskom-language-missing-string lyskom-message allhelgonadag en)
(lyskom-language-missing-string lyskom-message varnlosdag en)
(lyskom-language-missing-string lyskom-message luciaday en)

(lyskom-language-ending-mismatch lyskom-message carbon-copy-prefix en sv)
(lyskom-language-ending-mismatch lyskom-message blank-carbon-copy-prefix en sv)
(lyskom-language-ending-mismatch lyskom-message newyeareve en sv)

;;; Formely known as lyskom-strings
(lyskom-language-strings local lyskom-message en 
  '(
    ; From vars.el: 
    ; From komtypes.el: nil
    ; From clienttypes.el: nil
    ; From startup.el:
    (server-q . "LysKOM server? (%#1s) ")
    (try-connect . "LysKOM elisp client version %#1s.\nAttempting to connect to %#2s.\n")
    (too-old-server . "The server is too old for this version of the client.")
    (connection-done . "Connection established. Server version is %#1s.\n\n")
    (what-is-your-name . "What is your name? ")
    (password . "Your password? ")
    (wrong-password . "Incorrect password.\n")
    (wrong-password-help . "
If you have forgotten your password you may be able to get a new one
by e-mailing the site administrators at one of the addresses listed
below:\n")
    (wrong-password-email . "* %#1t\n")
    (are-logged-in . "You have entered LysKOM. Please wait...\n")
    (you-have-motd . "\nYou have a notice on your mailbox:\n\n")
    (server-has-motd . "There is a notice on the server:\n")
    (lyskom-motd-was-garbed . "\nThe login message does not exist!
The message that was supposed to be shown after login has disappeared.
Please contact the LysKOM administrator.\n")
    (presentation-encouragement .
"You have not written a presentation. Please write a presentation by using
the command Ap. If you do not want to write a presentations, please type fk.\n")

    (first-greeting . "%#1s
This appears to be the first time you use LysKOM. Welcome!
Please make sure you have spelled your name correctly. You should use your
full name and organisation, eg. \"Joe Hacker, MIT\". If your name is spelled
incorrectly, or you wish to change it, answer 'no' to the question below.

At present the LysKOM server stores most of the information so that
anybody can read it. Only passwords are encrypted.

If you are uncertain about how to use LysKOM, you can retrieve a manual by
anonymous ftp to ftp.lysator.liu.se. Ask your system administrator for help
on how to do this. You can also type \"?\" for the command \"Help\".
")

    (is-name-correct . "Is the name %#1s correct? ")
    (personal-password . "Enter a personal password: ")
    (repeat-password . "Repeat for confirmation: ")
    (repeat-failure . "The passwords were not the same.\n")

    (could-not-create-you .  "LysKOM couldn't create that user.\n")
    (presentation-subject . "%#1s")
    (presentation-form . "\
Name:      
Address:   
           
Telephone: 
E-mail:    
WWW:       

Other:     ")
    (presentation-help . "You are writing your presentation.\n")
    (not-present-anywhere . "Not in any conference.")
    (unknown-person . "Unknown user")
    (in-secret-conference . "Secret conference (%#1d).")
    (start-new-session-same-server
     . "You are already connected to that server. Do you want a new session? ")
    (new-session-in-buffer . "\n\n---- New session at %s ----\n\n")
    (warning-about-uncompiled-client . "
NOTE: The LysKOM client is not compiled. It is recommended to run a compiled
client for performance reasons. Read the accompanying README to learn how to
do that.
")

    ; From internal.el:
    (shaky-tcp . "At the moment I can't reach the server. The TCP/IP connection is shaky%#1s")
    (retrying-tcp . "Retrying.")

    ; From parse.el:
    (protocol-error . "protocol error: %s")

    ; From services.el:
    (interrupted . "Interrupted\n")

    ; From cache.el:
    ; No entries.

    ; From commands1.el:
    (appreciation . 
"You are a very special person, beautiful and wise, respected by
everybody around you. You are doing a splendid job. Many people love
you, body and soul. You make life easier for others. You are a very
warm and sensitive person.

Be proud of being You! You have a very good reason.\n\n")
    (abuse . 
"You are a nuisance, ugly and stupid, disrespected by everybody around
you. You are doing a worthless job. Many people hate you, body and
soul. You make life harder for others. You are a very cold and
unfeeling person.

Be ashamed of being You! You have a very good reason.\n\n")


    (what-conf-to-delete . "Conference/user to delete: ")
    (what-conf-to-change . "Conference to modify: ")
    (confirm-delete-pers-or-conf . "Really delete %#1s %#2s? ")
    (the-pers . "the user")
    (the-conf . "the conference")
    (deletion-not-confirmed . "Deletion aborted\n")
    (somebody-else-deleted-that-conf . "Somebody else just deleted the conference.\n")
    (conf-is-deleted . "OK, %#1s is now deleted.\n")
    (you-could-not-delete . "%#1M can't be deleted by you.\n")
    (you-have-deleted-yourself . "You have deleted yourself.\n")

    (what-text-to-delete . "Remove which text? ")
    (delete-marked-text . "The text is %#1s. Delete anyway? ")
    (delete-marked-by-you . "marked by you")
    (delete-marked-by-you-and-others . "marked by you and %#1?d%[someone else%]%[%#1d others%]")
    (delete-marked-by-several . "marked by %#1d user%#1?d%[%]%[s%]")
    (delete-commented-text-help . "\
The text you are about to remove has comments. If you remove it the 
comments will lose their context, and this annoys and even angers
many readers. You should consider moving it to another conference
instead of removing it.")
    (delete-commented-text . "The text has comments. Remove anyway? ")
    (really-delete-commented-text . "Removing a commented text may annoy many readers. Remove anyway?")
    (deleting-text . "Removing text %#1:n...")

    (presentation-for-whom . "View presentation for which conference/user? ")
    (text-to-see-author-of . "Review presentation of author of which text? ")
    (unread-presentation-for-whom . "Unread presentation for which conference/user? ")
    (text-to-unread-author-of . "Mark presentation of author of which text as unread? ")
    (somebody-deleted-that-conf . "Somebody just deleted that conference.\n")
    (review-presentation-of . "Review presentation of %#1M.\n")
    (has-no-presentation . "%#1:M has no presentation.\n")

    (have-to-read . "You must read a text first.\n")

    (no-comment-to . "There is no commented text.\n")
    (no-text-at-point . "There is no text at point.\n")
    (what-ancestor . "Which of the commented texts do you want? ")

    (who-letter-to . "Send a letter to whom? ")
    (who-send-text-to . "Send text to which conference? ")
    (has-motd . "%#1P has a notice on his/her mailbox:\n\n")
    (motd-persist-q . "Do you still want to write the text? ")

    (who-to-add . "Whom do you want to add? ")
    (where-to-add . "To which conference? ")
    (where-to-add-self . "Join which conference? ")
    (priority-q . "Priority of your membership? (0 (low) - 255 (high)) ")
    (other-priority-q . "Priority of the membership? (0 (low) - 255 (high)) ")
    (done . "done.\n")
    (cancelled . "cancelled.\n")
    (nope . "didn't work.\n")
    (add-already-member . "%#1P is already a member of %#2M.\n")
    (you-already-member . "You are already a member of %#1M.\n")

    (cant-find-supervisor . "Can't find supervisor of %#1M.\n")
    (is-read-protected-contact-supervisor . "%#1M is closed.
Send a letter to  %#2P to apply for membership.\n")

    (conf-does-not-exist . "\nThe conference doesn't exist.\n")

    (who-to-exclude . "Who do you want to remove? ")
    (where-from-exclude . "From which conference? ")

    (leave-what-conf . "Leave which conference? ")

    (error-fetching-person . "Error retrieving user.\n")
    (error-fetching-conf . "Error retrieving conference.\n")

    (name-of-conf . "Conference name? ")
    (anyone-member . "May anyone join? ")
    (secret-conf . "Secret conference? ")
    (comments-allowed . "Are comments allowed? ")
    (anonymous-allowed . "Are anonymous texts allowed? ")
    (secret-members-allowed . "Are secret members permitted? ")
    (what-comment-no . "Comment text number: ")
    (what-footnote-no . "Footnote text number: ")
    (what-private-no . "Private reply to text number: ")

    (quit-in-spite-of-unsent . "You have an unsent text. Do you really want to quit? ")
    (really-quit . "Do you really want to quit LysKOM? ")
    (session-ended . "
--------------------------------------------
  LysKOM session finished
  You are now disconnected from the server
--------------------------------------------
")
    (session-ended-long . "
-------------------------------------------------------------------------------
  LysKOM session finished
  You are now disconnected from the server
-------------------------------------------------------------------------------
")
    (session-auto-ended . "
============================================================
Disconnecting from LysKOM since all connections are in use
and you have finished reading. Please come back later.
============================================================\n\n")
    (session-auto-ended-long . "
===============================================================================
Disconnecting from LysKOM since all connections are in use
and you have finished reading. Please come back later.
===============================================================================
\n")
    (what-to-change-pres-you . "Change presentation of whom/what (yourself): ")
    (what-to-change-faq-you . "Change FAQ for which conference: ")
    (who-to-put-motd-for . "Post notice on whom/what (yourself): ")

    (what-to-set-pres-you . "Set presentation for whom/what (yourself): ")
    (what-text-to-set-as-pres-no . "Which text do you want as the new presentation: ")
    (what-to-set-motd-you . "Set notice on whom/what (yourself): ")
    (what-text-to-set-as-motd-no . "Which text do you want as the new notice: ")
    (conf-already-has-pres . "The conference/person already has a presentation. Continue anyway? ")
    (conf-already-has-motd . "The conference/person already has a notice. Continue anyway? ")
    (setting-conf-pres . "Setting presentation of %#1M to text %#2n...")
    (setting-conf-motd . "Setting notice for %#1M to text %#2n...")

    (who-to-remove-pres-for . "Vilket möte/person vill du ta bort presentationen från (dig själv): ")
    (removing-pres-for-conf . "Tar bort presentation (text %#2n) från %#1M...")

    (cant-get-conf-stat . "Cannot get the status of that conference.\n")
    (go-to-conf-p . "Go to conference: ")
    (want-become-member . "Do you want to join? ")
    (no-ok . "Okiedokie, whatever you say.\n")

    (who-to-remove-motd-for . "Remove notice from whom/what: ")

    (conf-all-read . "no unread texts")
    (no-in-conf . "You are not present in any conference.\n")

    (search-for-pers . "Enter search key (RETURN for all users): ")
    (search-for-conf . "Enter search key (RETURN for all conferences): ")
    (search-re . "Enter search regexp: ")
    (search-re-for-what . "What do you want to search for? ")
    (search-re-persons . "ppersons")
    (search-re-confs . "cconferences")
    (search-re-all . "bboth")

    (no-matching-confs . "No conferences match \"%#1s\".\n")
    (no-matching-perss . "No persons match \"%#1s\".\n")
    (no-matching-anys . "No persons or conferences match \"%#1s\".\n")
    (no-confs-exist . "There are no conferences in the database.\n")
    (no-pers-confs-exist . "There are no persons or conferences in the database.\n")
    (list-confs-created-by . "List owned conferences for: ")
    (listing-confs-created-by . "Listing owned conferences for %#1P\
    C=Created, O=Supervisor, S=Letterbox is super conf; S=Secret, P=Protected\n")
    (list-pers-confs-created-by . "List owned conferences and persons for: ")
    (getting-all-confs . "Retrieving a list of all conferences from the server...")
    (getting-all-pers-confs . "Retrieving a list of all persons and conferences from the server...")
    (getting-all-confs-done . "Retrieving a list of all conferences from the server...done")
    (getting-all-pers-confs-done . "Retrieving a list of all persons and conferences from the server...done")
    (finding-created-confs . "Finding owned conferences (%#1d of %#2d done)")
    (finding-created-pers-confs . "Finding owned persons and conferences (%#1d of %#2d done)")
    (no-created-confs . "%#1P is not creator, supervisor or super conference for any conference.\n")

    (name-to-be-changed . "Name to change: ")
    (no-such-conf-or-pers . "The conference or user doesn't exist.\n")
    (new-name . "New name: ")
    (new-paren . "New parenthesis: ")
    (too-many-parens-in-name . "Multiple parentheses in name. Use \"Change name\" instead.\n")
    (no-paren-in-name . "The name doesn't contain a parenthesis.\n")
    (who-to-change-supervisor-for . "Change supervisor of whom/what? ")
    (new-supervisor . "New supervisor: ")
    (text-to-mark . "Mark which text? ")
    (text-to-unmark . "Unmark which text? ")
    (what-mark . "Set which mark type (name or 0-255)? ")
    (erroneous-mark . "Erroneous mark type.\n")
    (want-to-create-symbolic-mark . "The mark type \"%#1s\" does not exist. Do you want to create it? ")
    (creating-symbolic-mark-type . "Creating mark type \"%#1s\" (%#2d).\n")
    (no-mark-types-left . "Sorry, there are no free mark types. Remove an old one first.")
    (unmarking-textno . "Unmarking text %#1n...")
    (marking-textno . "Marking text %#1n...")
    (list-which-mark . "List texts with which mark type (name or 0-255, RET for all)? ")

    (new-passwd-again . "Repeat the new password for confirmation: ")
    (what-mark-to-unread . "Mark which mark type as unread (name or 0-255, RET for all)? ")
    (what-mark-to-view . "Review which mark type (name or 0-255, RET for all)? ")
    (whos-passwd . "Change password for whom? (yourself) ")
    (old-passwd . "Your current password: ")
    (new-passwd . "The new password: ")
    (changing-passwd . "Changing password...")
    (retype-dont-match . "You didn't reenter the same passwrod. Try again.\n")
    (palindrome . " (a palindrome!)")
    (lyskom-name . "User")
    (is-in-conf . "In conference")
    (from-machine . "At")
    (is-doing . "Activity")
    (connection-time . "Connected")
    (active-last . "Active last")
    (active . "Active")
    (lyskom-client . "Client")
    (text-to-add-recipient . "Add recipient to which text:")
    (text-to-add-copy . "Add recipient of carbon copy to which text:")
    (text-to-add-bcc . "Add recipient of blind carbon copy to which text:")
    (text-to-delete-recipient . "Remove recipient from which text:")
    (text-to-move . "Which text do you want to move:")
    (text-tree-to-move . "Which text is the root of the tree you want to move:")

    (text-to-add-comment-to . "Add comment to which text:")
    (text-to-delete-comment-from . "Remove comment from which text:")
    (text-to-add-footnote-to . "Add footnote to which text:")
    (text-to-delete-footnote-from . "Remove footnote from which text:")
    (text-to-add-cross-reference-to . "Add cross reference to which text:")
    (text-has-no-recipients-r . "Text %#1n has no recipients\n")

    (where-on-list-q . "Placement in your list? (0-%#1d) ")
    (member-in-conf . "Joining to %#1M%#2?b%[ %#2s%]%[%]...")
    (member-in-conf-with-low-priority . "%#1M has lower priority than your session priority, and will not be shown.\n")
    (member-in-conf-with-unread-date . "with unread from %#2s %#3d, %#1d")
    (member-in-conf-with-unread . "unread %#1d unread")
    (add-member-in . "Adding %#1P as a member of %#2M...")
    (change-priority-for-q . "Change priority of conference: ")
    (change-priority-for . "Changing priority of %#2M...")
    (unsubscribe-to . "Leaving %#1M...")
    (unsubscribe-to-2 . "Leaving %#1M%#2s...")
    (confirm-each-join . "Confirm joining each individual conference? ")
    (no-confirm-each-sure . "Are you sure you want to join %#1d conferences automatically? ")
    (confirm-join . "Join %#1M? ")
    (getting-all-confs . "Getting a list of all conferences...")
    (getting-all-confs-progress . "Getting a list of all conferences (%#1d/%#2d)...")
    (getting-all-confs-done . "Getting a list of all conferences...done")
    (unsub-all-skipping-letterbox . "nix\nThis is your letterbox.\n")
    (unsub-all-skipping-supervised . "nix\nYou are the supervisor of the conference.\n")
    (unsub-secret-conf-q . "%#1M is secret. Leave anyway? ")
    (unsub-all-secret . "aLeave all secret conferences")
    (unsub-closed-conf-q . "%#1M är close. Leave anyway? ")
    (unsub-all-closed . "aLeave all closed conferences")
    (unsub-open-conf-q . "Leave %#1M? ")
    (unsub-all-open . "aLeave all open conferences")
    (abc-yes . "yYes")
    (abc-no . "nNo")

    (exclude-from . "Removing %#1P from %#2M...")

    (unsubscribe-failed . "\nDidn't work. Perhaps %#1P isn't a member of %#2M?\n")
    (passivate-done . "You are now a passive member of %#1M.
Leave the conference again to unsubscribe completely.\n")

    (You . "You")
    (could-not-create-conf . "Couldn't create the conference \"%#1s\".\n")
    (created-conf-no-name . "Conference number %[%#3@%#1:m %#2:M%] has been created.\n")
    (cant-read-textno . "You are not allowed to read text %#1:n.\n")

    (not-supervisor-for . "You are not the supervisor of %#1M.\n")
    (not-supervisor-for-server . "You do not have administrative rights on this LysKOM-server.\n")
    (go-to-conf . "Go to conference %#1M.\n")
    (cant-go-to-his-mailbox . "You are not allowed to go to %#1M's mailbox.\n")
    (not-member-of-conf . "You are not a member of %#1M.\n")
    (about-to-change-name-from . "%#1M\n")
    (change-name-done . "Done. New name: %[%#2@%#1:M%].\n")
    (change-name-nope . "Couldn't change name to %#1s. Error code %#3d. %#2s.\n")
    (change-supervisor-from-to . "Change supervisor of %#1M to %#2P...")
    (change-supervisor-nope . 
     "\nDidn't work. Perhaps you are not allowed to change the supervisor of %#1M?\n")
    
    (no-marked-texts . "You have not marked any texts.\n")
    (no-marked-texts-mark . 
     "You have not marked any texts with mark type \"%#1s\".\n")

    (weekdays . ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday"
		 "Friday" "Saturday" "Sunday"])
    (weekdays-short . ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"])
    (time-is . "The time is %#1s%#2s (according to the server).")
    (time-is-week . "The time is %#1s%#2s, week %#3d (according to the server).")

    (xmaseve . "Christmas eve!\nYou didn't open any gifts early, did you?")
    (xmasday . "Christmas day.\nDid you get any nice gifts this year?")
    (newyearday . "\nMay %#1d be a prosperous and good year for you!")
    (newyeareve . "Happy New Year!")
    (newyearevelate . "Less than an hour left of %#1d...")
    (lysbday . "
On this day, in 1973, the Lysator Academic Computer Society was
formed, and it was a great day in the history of computing in Sweden.
Read all about it at http://www.lysator.liu.se/history/")

    (total-users . "    A total of %#1d users (%#2s).\n")
    (total-users-sans-date . "    A total of %#1d users\n")
    (total-visible-users . "    A total of %#1d visible user%#1?d%[%]%[s%] (%#2s).\n")
    (total-active-users . "    A total of %#1d active user%#1?d%[%]%[s%] (%#2s).\n")
    (total-visible-active-users
     . "    A total of %#1d visible%#1?d%[%]%[%] active user%#1?d%[%]%[s%] (%#2s).\n")
    (client-statistics . "    Distribution of clients:\n")
    (client-statistics-line . "    %4#2d %#1s")
    (who-to-add-q . "Add whom/what as a recipient? ")
    (who-to-add-copy-q . "Add which conference/user as carbon copy recipient? ")
    (who-to-add-bcc-q . "Add which conference/user as blind carbon copy recipient? ")
    (really-add-as-recpt-q . "Send future comments to the text to %#1M too? ") 
    (who-to-sub-q . "Remove whom/what as a recipient? ")
    (who-to-move-from-q . "Move from where? ")
    (who-to-move-to-q . "Move to where? ")
    (who-to-move-to-or-sub-q . "Move to where (empty to just remove recipient)? ")

    (adding-name-as-recipient . "Adding %#1M as recipient of text %#2n...")
    (adding-name-as-copy . "%#1M will receive a carbon copy of text %#2n...")
    (remove-name-as-recipient .
     "Removing %#1M as recipient of text %#2n...")
    (adding-cross-reference . "Adding cross reference...")

    (error-recipient-limit . "Text %#1n has too many recipients.\n")
    (error-already-recipient . "%#2M is already a recipient of text %#1n.\n")
    (error-permission-denied-add-recpt . "Only the author of %#1n or supervisor for %#2M can change recipient types.\n")
    (error-permission-denied-sub-recpt . "Only the author of %#1n or supervisor for %#2M can remove the recipient.\n")
    (error-access-denied-add-recpt . "You are not allowed to add texts to %#2M.\n")
    (error-not-recipient . "%#2M is not a recipient of text %#1n.\n")

    (moving-tree-what-action-q . "What do you want to do? (move) ")
    (moving-name . "Moving text %#3n from %#1M to %#2M...")
    (moving-already-moved . "Text %#1n does not have %#2M as a recipient.\nSkipping this text and its comments.\n")
    (text-to-add-q . "Add which text as a comment to %#1n? ")
    (text-to-remove-q . "Remove which text as a comment from %#1n? ")
    (text-to-add-footn-q . "Add which text as a footnote to %#1n? ")
    (text-to-remove-footn-q . "Remove which text as a footnote from %#1n? ")
    (add-comment-to . "Adding text %#1n as a comment to text %#2n...")
    (sub-comment-to . "Removing text %#1n as a comment to text %#2n...")
    (add-footnote-to . "Adding text %#1n as a footnote to text %#2n...")
    (sub-footnote-to . "Removing text %#1n as a footnote to text %#2n...")
    (comment-keep-recpt-p ."Should %#1s remain a recipient? ")
    (comment-all-relevant-p . "There are multiple recipients. Are they all relevant? ")
    (please-edit-recipients . "Modify the recipient list and send the text again.")
    (add-recipient-p . "Add recipient%#2?b%[ %#2M to reach%]%[%] %#1P? ")
    (checking-rcpt . "Checking recipients...")
    (checking-rcpt-done . "Checking recipients...done")
    (checking-comments . "Checking commented texts...")
    (checking-comments-done . "Checking commented texts...done")
    (please-check-commented-texts . "Review the commented text and its comments.")
    (have-unread-comment . "Send despite unread comments to text %#1n? ")
    (duplicate-recipients . "Unable to post. A recipient is duplicated (%#1M)")
    (no-recipients . "Unable to post text. No recipients were specified.")

    (matching-regexp . "Conferences/users matching `%#1s'\n")
    (matching-regexp-perss . "Users matching `%#1s'\n")
    (matching-regexp-confs . "Conferences matching `%#1s'\n")

    (who-is-active-all . "Showing all sessions.\n")
    (who-is-active-last-minutes . "Showing all sessions active the last %#1d minutes.\n")
    (showing-invisibles . "Showing invisible sessions.\n")
    (null-who-info . "No one (active) is logged on.\n")

    (no-other-lyskom-r . "There are no other active LysKOM sessions.\n")
    (no-lyskom-session . "There are no active LysKOM sessions.")
    (no-other-unread-lyskom-r . "There is no another LysKOM session with unread texts.\n")
    (no-unread-lyskom-r . "There is no active LysKOM session with unread texts.\n")
    (no-unread-lyskom . "There is no active LysKOM session with unread texts.")

    (who-is-on-in-what-conference . "Who in which conference: ")
    (who-is-present-in-what-conference . "Who present in which conference: ")
    (who-is-friend . "Only friends are shown.\n")
    (who-is-active-and-member . "Only members in %#1M are shown.\n")
    (who-is-active-and-present . "Only members present in %#1M are shown.\n")

    (personal-comment-to-anonymous . "It is not possible to write a personal comment to an anonymous text.\n")

    ;; Used for kom-is-person-member-of-conference:
    (pers-to-check-mship-for . "Whose membership do you want to check? ") ;-)
    (conf-to-check-mship-of . "...for what conference? ")
    (conf-is-empty . "The conference %#1M is empty.\n")
    (pers-is-member-of-conf . "Yes, %#1P is a member of the conference %#2M.\n")
    (pers-is-passive-member-of-conf . "Well, %#1P is only a passive member of the conference %#2M.\n")
    (pers-is-not-member-of-conf . "No, %#1P is not a member of the conference %#2M.\n")
    (pers-is-member-of-conf-2 . "Number of unread: %#2D (last access %#1s)\n")
    (Unknown-number . "Unknown number")
    (text-to-check-will-read-for . "...for membership in recipients of text: ")
    (pers-to-check-will-read-for . "Person to check: ")
    (pers-is-member-of-rcpt . "Yes, %#1P is a member of at least one recipient to text %#2n.\n")
    (pers-is-passive-member-of-rcpt . "Well, %#1P is only a passive member of at least one recipient of %#2n.\n")
    (pers-is-not-member-of-rcpt . "No, %#1P is not a member of any recipient of %#2n.\n")

    ; From commands2.el:

    (your-memberships . "Your LysKOM conference memberships:\n")
    (memberships-header . "Last access	   Prio Unread	Conference\n")
    (memberships-line . "%16#1s  %#2d\t%#3d\t%#4M\n")
    
    (conf-for-status . "Get status of which conference? ")
    (no-such-conf . "The conference doesn't exist.\n")
    (status-record . "Status of conference %#1M (%#1m)%#2s\n\n")
    (change-type-prompt . "Change conference type for %#1M (%#1m) (%#2s)...")
    (Mailbox . "Mailbox")
    (Protected . "Protected")
    (no-comments . "No comments")
    (closed . "Closed")
    (allow-anon . "anonymomus ok")
    (allow-secret . "secret members")
                   
    (created-by . "Created by person %24#1p %#3s(%#2P)\n")
    (created-at . "Created:%34#1s\n")
    (members .    "Number of members: %19#1d\n")
    (conf-allows-secret-members . "Secret members:                          %#1s\n")
    (conf-allows-anon-texts .     "Anonymous texts:                         %#1s\n")
    (anon-texts-permitted . "Anonymous texts are permitted")
    (anon-texts-not-permitted . "Anonymous texts are not permitted")
    (secret-members-permitted . "Secret members are permitted")
    (secret-members-not-permitted . "Secret members are not permitted")
    (garb-nice . "Expiration time (in days):%16#1d\n")
    (lowest-local-no . "Lowest local number: %21#1d\n")
    (highest-local-no . "Highest local number: %20#1d\n")
    (last-text-time . 
     "Time of last text:    %20#1s (according to your cache)\n")
    (no-of-motd . "Notice in text:    %13#1n\n")
    (superconf-is-no-name . "Superconference: %25#1m %#3s(%#2M)\n")
    (permitted-submitters-no-name . "Allowed authors: %25#1m %#3s(%#2M)\n")
    (supervisor-is-no-name . "Supervisor: %30#1p %#3s(%#2P)\n")
    (presentation-no . "Presentation:    %25#1n\n")
    (conf-has-motd . "\n%#1M has a notice on his/her mailbox:\n")
    (conf-mship-priority . "Prioritet:       %25#1n%#2?b%[ %#2s%]%[%]\n")
    (status-conf-generic . "%-40#1s %#2s\n")
    (status-aux-item . "Unknown auxiliary information: %11#1s%#3s (created by %#2M)\n")
    (conf-mx-list-name . "Imported mailing list:                   %#1s %#2s\n")
    (conf-mx-refuse-import . "Refuse import of:                        %#1s %#2s\n")
    (recommended-conf-aux . "Recommended conference:                  %#1M <%#1m> %#2s\n")
    (status-read-faq-aux-item . "Read FAQ:                  %15#2n for %#1?z%[%#1M <%#1m>%]%[the server%] %#3s\n")
    (status-rejected-recommendation-aux-item . "Rejected recommendation for:             %#1M %#2s\n")
    (status-send-comments-to . "Redirect comments to:                    %#1M <%#1m> %#2s\n")

    (Everybody . "Everyone")
    (show-members-list-also-q . "List members? ")
    (show-membership-info-q . "Show membership info number of unreads? ")
    (conf-has-these-members . "\n%#1M has the following members:\n")
    (conf-has-no-members . "\n%#1M has no members.\n")
    (member-list-header . "\nLast entered        Unread  Name\n")
    (secret-membership . "--- Secret line ---\n")
    (conf-membership-line . "%#4@%#1s%#2M %#3s\n")
    (conf-membership-line-2 . "                            Added %#1s by %#2P\n")
    (pers-for-status . "Get status of which user? ")
    (text-to-see-author-status-of . "Get status of the author of which text? ")
    (no-such-pers . "The user doesn't exist.\n")
    (pers-status-record . "Status of user %#1P (%#1p)\n")
    (created-time .  "Created:%34#1s\n\n")
    (created-confs . "Conferences created:%22#1d\n")
    (created-persons . "Users created:%28#1d\n")
    (created-texts . "Texts created:%28#1d\n")
    (created-lines . "Lines created:%28#1d\n")
    (created-chars . "Characters created:%23#1d\n")
    (no-of-sessions . "Session count:%28#1d\n")
    (present-time-d-h-m-s . "Total presence:%16#1d d %02#2d:%02#3d:%02#4d\n")
    (last-log-in . "Last login or logout:%21#1s\n")
    (user-name . "User: %36#1s\n")

    (read-texts . "Texts read:%31#1d\n")
    (marked-texts . "Texts marked:%29#1d\n")
    (time-for-last-letter . "Time of last letter:%22#1s (according to your cache)\n")
    (superconf . "Superconference: %25#1m %#3s(%#2M)\n")
    (supervisor . "Supervisor: %30#1p %#3s(%#2P)\n")
    (member-of-confs . "Member of (conferences):%18#1d\n")
    (presentation . "Presentation:    %25#1n\n")
    (show-membership-list-also-q . "List memberships? ")
    (not-allowed-see-confs . "%#1P is not a member of any conferences.\n")
    (is-member-of . "\n%#1P is a member of the following conferences:\n")
    (membership-list-header . "\nLast access         Unread   Conference\n")
    (pers-membership-line . "%#5@%#1s%#2s%#3M %#4s\n")
    (pers-membership-line-2 . "                             Added %#1s by %#2P\n")
    (is-supervisor-mark . "O ")
    (who-to-send-message-to . "Send message to whom? (%s) ")
    (send-empty-message-p . "The message is empty. Send it anyway? ")
    (his-total-unread . "\n%#1M has %#2d unread texts (plus %#3d in passive memberships).\n")
    (message-prompt . "Message: ")
    (message-sent-to-user . "\
%[%#3$================================================================
%]%[%#4$Your message for %#2M:

%#1t
%]%[%#3$----------------------------------------------------------------
%]")
    (message-sent-to-all . "\
%[%#3$================================================================
%]%[%#4$Your public message

%#1t
%]%[%#3$----------------------------------------------------------------
%]")
    (message-sent-to-user-long . "\
%[%#3$\
===============================================================================
%]%[%#4$Your message for %#2M:

%#1t
%]%[%#3$\
-------------------------------------------------------------------------------
%]")
    (message-sent-to-all-long . "\
%[%#3$\
===============================================================================
%]%[%#4$Your public message

%#1t
%]%[%#3$\
-------------------------------------------------------------------------------
%]")
    (message-use-alarm-instead . "Use %#1s to send alarm messages.\n")
    (message-all-info . "Send alarm\n")
    (message-recipient-info . "Send message to %#1M\n")
    (message-nope .
     "Unable to send message. %#3s\
The message you were sending to %#1M was:
%#2t\n")
    (only-last . "Last texts in %#1s: ")
    (initial-unread . "Initial number of unread (empty for all texts): ")
    (only-error . "Something went wrong. Sorry.\n")
    
    (you-have-unreads . "You have %#1d unread text%#1?d%[%]%[s%] in %#2M\n")
    (you-have-unreads-special . "You have %#1d uncommented text%#1?d%[%]%[s%] in %#2M\n")
    (you-have-no-unreads . "You have read everything in %#1M\n")
    (you-have-read-everything . "No news (is bad news).\n")
    (no-unreads-shown . "Found no conferences meeting that criterion.\n")
    (total-unreads .
     "You have %#1d unread text%#1?d%[%]%[s%] texts in %#2d conference%#2?d%[%]%[s%].\n")
    (shown-unreads . "Showed %#1d unread text%#1?d%[%]%[s%] in %#2d conference%#2?d%[%]%[s%].\n")
    (list-unread-with-n-unread . "Listing conferences with at least %#1d unread.\n")
    (list-unread-with-at-most-n-unread . "Listing conferences with at most %#1d unread.\n")
    (waiting-for-anything .
     "You are waiting for a text in any conference.\n")
    (waiting-higher-than . 
     "You are waiting for a text in any conference with a priority higher than %#1d.\n")
    
    (have-to-be-in-conf-with-unread . "You must go to a non-empty conference first.\n")
    (Texts . "Text")
    (Written . "Written")
    (Lines . "Lines")
    (Author . "Author")
    (Subject . "Subject")
    (Comments . "Co")
    (mark-type . "Type")

    (could-not-read . "You couldn't read the text (%#1n).\n")
    (multiple-choice . "There are several alternatives.")

    (does-not-exist . "Unknown command.") ; Only people fixing bugs or receiving bug reports should change these:
    (summary-line . "%=-8#1n%#2s%4#3d  %[%#4@%#5:P%]  %[%#6@%#7r%]\n")
    (diff-what-text-old . "Old text to compare: ")
    (diff-what-text-new . "New text to compare: ")

    ; Only people fixing bugs or receiving bug reports should change these:
    (buggreport-compilestart . "Creating bug report...")
    (buggreport-compileend . "Creating bug report...done")
    (buggreport-description . "This is what I was doing:
\(Fill in your comments below\)\n================\n\n
================
In the information below are the 100 most recently pressed keys from
your emacs. If you recently logged on, you password may be contained in
this list. If that is the case, change the characters corresponding to
your password to asterisks.

When you have finished writing this, send your bug report to the LysKOM
developers. You can do this either by email to bug-lyskom@lysator.liu.se or
by mailing a hardcopy of your bug report to:
Lysator, c/0 ISY, Linkoping Univerity, S-581 83 Linkoping, SWEDEN.
Mark the envelope with \"LysKOM bug report\"\n\n")
    (buggreport-internals . "LysKOM's internal information:\n\n")
    (buggreport-version . "lyskom-version:")
    (buggreport-emacs-version . "emacs-version:")
    (buggreport-system-id . "system-id:")
    (buggreport-ctl-arrow-doc . "ctrl-doc:")
    (buggreport-unparsed . "\nlyskom-unparsed-buffer:")
    (buggreport-command-keys . "Recently pressed keys:")
    (buggreport-backtrace . "\n*Backtrace*:\n%#1s\n")
    (buggreport-communications . "\nlyskom-debug-communications-to-buffer-buffer:")
    (buggreport-all-kom-variables . "\n\nOther variables:\n***** *********")
    (buggreport-instead-of-byte-comp . "byte-code(\"byte-string\"")
    (buggreport-subject . "Bugreport elisp-client version %#1s")


    (not-logged-in . "You are not logged on. ")
    (name-is-not-in-conf . "%#1s is not in any conference.\n")
    (name-is-in-conf . "%#1s is in\n%#2s\n")
    (connected-during . "Connect time: %#1d seconds.\n")

    (conf-to-set-permitted-submitters-q . "For which conference do you want to set the allowed authors? ")
    (conf-to-set-super-conf-q . "Set superconference of which conference? ")
    (new-super-conf-q . "Which conference do you want as superconference? ")
    (new-permitted-submitters-q . "Allow members of which conference as authors in %#1s? (all) ")
    (super-conf-for-is . "Changing superconference of %#1M to %#2M...")
    (permitted-submitters-removed-for-conf . "Allowing all authors to conference %#1M...")
    (submitters-conf-for-is . "Changing authors admitted to conference %#1M to the members of %#2M...") 
   
    (conf-to-set-garb-nice-q . "Set expiration time for which conference? ")
    (new-garb-nice-q . "After how many days shall texts be removed? ")
    (garb-nice-for-is . "Changing expiration for %#1M to %#2d days...")

    (really-shutdown . "Are you sure you want to shut down the server? ")
    (closing-server . "Shutting down the server...")
    (really-sync . "Are you sure you want to save the database? ")
    (syncing-server . "Saving the database...")
    (administrator . "administrator")
    (no-longer-administrator . "a regular user again")
    (you-are-now . "Ok, you are now running as %#1s.\n")
    (setting-motd . "Changing login message to text %#1n.\n")
    (set-motd-success . "You have set a new login message.\n")
    (set-motd-failed . "Didn't work. Perhaps you were not an administrator.\n")
    (removing-motd . "Removing the login message.\n")
    (removed-motd . "You have removed the login message.\n")
    (who-to-throw-out . "Whose session do you want to kill? ")
    (throwing-out . "Killing session %#1d... ")
    (postpone-prompt . "How much do you want to read now? ")
    (set-session-priority . "Set reading level: ")

    ; From review.el:
    (no-unread-done . "You need to unread something before you can unread more.\n")
    (no-review-done . "You need to review something before you can review more.\n")
    (not-reviewing . "You are currently not reviewing anything.\n")
    (unread-how-many . "Unread how many?")
    (unread-how-many-more . "Unread how many more?")
    (review-how-many . "Review how many?")
    (review-how-many-more . "Review how many more?")
    (latest-n . "last %#1d")
    (first-n . "first %#1d")
    (info-by-whom . "%#1s by whom: ")
    (info-to-conf . "%#1s to conference: ")
    (info-by-to . "%#1s by %#2P to %#3M forward.")
    (all-confs . "all conferences")
    (no-get-conf . "You are not allowed to access that conference.\n")
    (no-get-pers . "You are not allowed to access that user.\n")
    (no-review-info . "You are not allowed to review %#1s\n")

    (unread-info . "Unread %#1s")
    (unread-info-by-to . "Unread %#1s by %#2P to %#3M.\n")
    (unread-more-info-by-to . "Unread %#1s by %#2P to %#3M.\n")
    (unread-rest . "the rest")
    (unread-more . "%#1d more")

    (review-info . "Review %#1s")
    (review-info-by-to . "Review %#1s by %#2P to %#3M forwards.\n")
    (review-more-info-by-to . "Review %#1s by %#2P to %#3M forwards.\n")
    (review-rest . "the rest")
    (review-more . "%#1d more")
    (you-review . "You are now reviewing %#1s.\n")
    (read-text-first . "You must read a text first.\n")
    (cannot-read-last-text . "You cannot review the last read text.\n")
    (review-n-texts . "Review %#1d texts.\n")
    (review-marked . "Review %#1d marked texts.\n")
    (review-text-no . "Review text %#1n\n")
    (review-many-comments . "Review %#2?d%[one%]%[%#2d%] comment%#2?d%[%]%[s%] to text %#1n.\n")
    (view-many-comments . "Read %#2?d%[one%]%[%#2d%] comment%#2?d%[%]%[s%] to %#1n.\n")
    (view-texts-in-conf . "Grand total of %#1d texts to read in %#2M.\n")

    (not-reading-anywhere . "You are not reading in any conference.\n")
    (read-normally-read . "How many texts to you want to read again? ")

    (review-conf-gone . "The conference does not exist.\n")
    (review-pers-gone . "The user does not exist.\n")
    (review-cant-read-conf . "You can't review texts to a closed conference you are not a member of.\n")
    (review-cant-read-letterbox . "You can't review texts to somebody else's mailbox.\n")
    (review-cant-read-empty . "The conference is empty.\n")
    (cant-review-everything . "You cannot review every text in LysKOM.\n")
    (cant-unread-everything . "You cannot unread every text in LysKOM.\n")

    (more-than-one-root . "Text %#1n has more than one root.\n")
    (more-than-one-root-review . "\
Text %#1n has more than one root but only one of the trees will be shown.\n")

    ; From edit-text.el:
    (press-C-c-C-c . "Enter C-c C-c to post the text.")
    (recipient . "Recipient")
    (carbon-copy . "Carbon copy")
    (blank-carbon-copy . "Blind Carbon copy")
    (recipient-prefix . "\\([Rr]\\)")
    (carbon-copy-prefix . "[Cc]\\([Aa]\\|[Cc]\\)")
    (blank-carbon-copy-prefix . "[Bb]\\([Ll]\\|[Cc][Cc]\\)")
    (add-recipient . "Add a recipient")
    (add-recipient-or-xref . "Add...")

    (secret-aux-flag . "secret")
    (anonymous-aux-flag . "anonymous")
    (inherit-aux-flag  . "inherited")
    (aux-item-prefix . "[*]")
    (aux-item-prefix-regexp . "\\[\\*\\]\\s-*")
    (comment-item-prefix . "#\\s-*")
    (text-no-comment . "%#1d %#2s /%#3d line%#3?d%[%]%[s%]/ %#4P %#5?b%[ [anonymous]%]%[%]\n")
    (cant-fcc-text-file-error . "Unable to save text %#1n to \"%#2s\" (%#3s: %#4s).\n")
    (cant-fcc-text . "Unable to save text %#1n to \"%#2s\" (%#3s).\n")

    (header-subject . "Subject: ")
    (header-separator . "\\<lyskom-edit-mode-map>\
--- Write below. \
Post: \\[kom-edit-send], \
Kill: \\[kom-edit-quit], \
Help: \\[describe-mode] ---")
    (text-mass . "%#4s%#1s\n%#2s\n%#3s")
    (comment-to-by . "%#1s to text %#2n%#3s.\n")
    (already-sent . "You have already posted this text. Post it anyway? ")
    (subject . "Subject: ")
    (subject-prefix . "[Ss]")
    (enter-subject-idi . "Enter a subject.")
    (which-text-include . "Include which text? ")
    (added-recipient . "Recipient: ")
    (added-carbon-copy . "Carbon copy to conference: ")
    (added-blank-carbon-copy . "Blind carbon copy to conference: ")
    (text-to-comment-q . "Which text do you want to comment? ")
    (conf-has-motd-no . "The conference has a notice. (%#1d)\n\n%#2s")
    (still-want-to-add . "Do you still want to add the conference as a recipient? ")
    (could-not-create-text . "\nCouldn't create the text. Error: %#2s.\n")
    (no-get-text . "You were not allowed to retrieve the text.")
    (unknown-header . "Unknown header")
    (transform-error . "Send unformatted (%#1s)? ")
    (cant-find-info-node . "Can't find the info buffer")

    (link-type . "What to you want to link to (text, conference or person)? ")
    (which-text-to-link . "Add link to text: ")
    (which-text-to-link-err . "Text not found. Add link to text: ")
    (which-pers-to-link . "Add link to user: ")
    (which-conf-to-link . "Add link to conference: ")

    ; From view-text.el:
    (view-text-first-line . "%#7$%#2@%#1n %#3s /%#4d line%#4?d%[%]%[s%]/ %#5P%#6?b%[%#6s%]%[%]\n")

    (marked-by-you . "Marked by you (type: %#1s).\n")
    (marked-by-you-and-others . "Marked by you (type: %#2s) and %#1?d%[someone else%]%[%#1d others%].\n")
    (marked-by-several . "Marked by %#1d user%#1?d%[%]%[s%].\n")

    (timeformat-day-yyyy-mm-dd-hh-mm-ss . "%#7s %4#1d-%02#2d-%02#3d %02#4d:%02#5d:%02#6d")
    (timeformat-yyyy-mm-dd-hh-mm-ss . "%4#1d-%02#2d-%02#3d %02#4d:%02#5d:%02#6d")
    (timeformat-yyyy-mm-dd-hh-mm . "%4#1d-%02#2d-%02#3d %02#4d:%02#5d")
    (timeformat-yyyy-mm-dd . "%4#1d-%02#2d-%02#3d")
    (timeformat-hh-mm-ss . "%02#4d:%02#5d:%02#6d")
    (timeformat-hh-mm . "%02#4d:%02#5d")

    (format-time-date-and-time . "%#1s %#2s")
    (format-time-just-date . "%#1s")
    (format-time-just-time . "%#2s")
    (today . "today")
    (yesterday . "yesterday")

    (no-such-text-no . "The text doesn't exist. (%#1:n)\n")
    (text-created-at . "Created: %#1s\n")
    (text-imported-at . "Imported: %#1s\n")
    (text-imported-at-by . "Imported: %#1s by %#2P\n")

    (head-Subject . "Subject: ")
    (Recipient . "Recipient")
    (Extra-recipient . "CC")
    (Hidden-recipient . "BCC")
    (mx-Recipient . "External recipient")
    (mx-Extra-recipient . "External CC")
    (mx-Extern-reply-to . "External replies to")
    (Strange-recipient . "Also to")
    (send-at . "   Posted:     %#1s\n")
    (sent-by . "   Posted by %#1P\n")
    (received-at . "    Received: %#1s\n")

    (comment-to-text . "Comment to text %#1n")
    (footnote-to-text . "Footnote to text %#1n")
    (comment-in-text . "Comment in text %#1n")
    (footnote-in-text . "Footnote in text %#1n")

    (attachment-to-text . "Attachment %#3sto text %#1n")
    (attachment-in-text . "Attachment %#3sin text %#1n")
    
    (envelope-sender . "Sent by: %#1s\n")
    (attachment-filename . "Attachment file name: \"%#1s\"\n")

    (comment-to-text-by . "Comment to text %#1n by %#2P")
    (footnote-to-text-by . "Footnote to text %#1n by %#2P")
    (comment-in-text-by . "Footnote in text %#1n by %#2P")
    (footnote-in-text-by . "Footnote in text %#1n by %#2P")

    (written-by . " by %#1P\n")

    ; From async.el:

    (name-has-changed-to-name . "%#1:P has changed name to %#2:P")
    (name-has-changed-to-name-r . "%[%#3@%#1:P%] has changed name to %[%#3@%#2:P%]\n")
    (you-changed-name-to . "You have now changed your name to %[%#2@%#1:P%].\n")
    (database-sync . "Synching database.")

    (lyskom-is-full . "\
===========================================================
Message from the LysKOM system: Somebody tried to connect,
but failed since all connections available to LysKOM are in
use. Please leave and return later if you are just waiting
for a text.
===========================================================\n")
    (lyskom-is-full-long . "\
===============================================================================
Message from the LysKOM system: Somebody tried to connect,
but failed since all connections available to LysKOM are in
use. Please leave and return later if you are just waiting
for a text.
===============================================================================
")
    (has-entered . "%#1:P has entered %#2s.")
    (has-entered-r . "%#2@%#1P has entered %#3s.\n")
    (has-left . "%#1:P has left %#2s.")
    (has-left-r . "%#2@%#1P has left %#3s.\n")
    (Unknown . "Unknown")
    (unknown . "unknown")
    (Unknown2 . "Unknown")

    (no-longer-member . "You are no longer a member of %#1M.\n")
    (no-longer-member-n . "You are no longer a member of %#1m (conference is gone).\n")
    (have-become-member . "You have become a member of %#1M.\n")
    (have-become-invited-member . "You have been invited to %#1M.
Go to the conference to accept or decline the invitation. You can also use
the command 'Join conference' to accept the invitation.\n")
    (have-become-passive-member . "You have become a passive member of %#1M.\n")


    (message-broadcast . "\
%[%#4$\
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#5$\
Alarm from %#1P (%#3s):

%#2t
%]%[%#4$\
----------------------------------------------------------------
%]")
    (message-broadcast-long . "\
%[%#4$\
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#5$Alarm from %#1P (%#3s):

%#2t
%]%[%#4$\
-------------------------------------------------------------------------------
%]")
    (message-from . "\
%[%#4$\
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#5$Personal message from %#1P (%#3s):

%#2t
%]%[%#4$\
----------------------------------------------------------------
%]")
    (message-from-long . "\
%[%#4$\
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#5$Personal message from %#1P (%#3s):

%#2t
%]%[%#4$\
-------------------------------------------------------------------------------
%]")
    (message-from-to ."\
%[%#5$\
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#6$Group message to %#3M\nfrom %#2P (%#4s):

%#1t
%]%[%#5$\
----------------------------------------------------------------
%]")
    (message-from-to-long ."\
%[%#5$\
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%]%[%#6$Group message to %#3M\nfrom %#2P (%#4s):

%#1t
%]%[%#5$\
-------------------------------------------------------------------------------
%]")


    (text-is-created . "Text %#1n has been created!")

    ; Used in mode-line-process
    (mode-line-waiting . ": waiting")
    (mode-line-working . ": working")
    (mode-line-saving . ": saving")
    (mode-line-down . ": down")

    ; From completing-read.el:

    (person-or-conf-no-regexp . "\\`[ \t]*[mpMP]\\w*[ \t]+\\([0-9]+\\)\\'")
    (session-no-regexp . "\\`[ \t]*[sS]\\w*[ \t]+\\([0-9]+\\)\\'")
    (conf-prompt . "Which conference/person? ")

    ; From prioritize.el:

    (cant-move-nothing-nowhere . "Can't move nothing anywhere.")
    (goto-priority-prompt . "Go to priority: ")
    (priority-prompt . "New priority for %#1M: ")
    (priority-prompt-marked . "New priority for selected conferences: ")
    (beginning-of-list . "Beginning of list")
    (end-of-list . "End of list")
    (reprioritize-from . "Reprioritize from: ")
    (reprioritize-to . "Reprioritize to: ")
    (no-selection . "No selection")
    (selection . "%d selected")

    (cannot-get-membership . "Cannot retrieve your membership list.")
    (cannot-get-pers-stat . "Cannot retrieve your personal status.")
    (prioritize-help .
"u,n Move conference, SPC select, p prioritize selection, q quit, C-h m help")
    (your-priorities . " Priority   Conference
-------------------------------------------------------------------------------
")
    (your-membship . "Your memberships:
  Prio Conf# Conference\n")
    (prio-row . " %5#1d%5#2m  %#3M\n")
    (too-high-goto-2 . "You are too high up. Move down to line two.")
    (too-low-go-up . "You can't push the last line. Move up one line.")
    (all-confs-popped .  "All conferences have been popped.")
    (prio-died . "Couldn't complete the move. Sorry. Kill the buffer.")
    (new-priority . "New priority? (0 (low) - 255 (high)) ")
    (new-prio . "%6#1d")

    ; From flags.el:
    (saving-settings . "Saving options...")
    (saving-settings-done . "Saving options...done")
    (hang-on . "Wait a moment...\n")
    (no-changes . "Nothing was saved since no options had been changed.\n")
    (could-not-save-options . "Couldn't save options.\n")
    (could-not-create-area . "Couldn't create the text.\n")
    (could-not-set-user-area . "Couldn't alter the user area. The server says error: %#1d\n")
    (you-dont-exist . "You don't exist. Go away.\n")
    (error-in-options . "There was an error in one of your variables (%#1s)
It was set to \"%#2s\" in the user area. It is set to nil instead.
Send a bug report.\n")
    (error-in-options-short . "There was an error in your saved settings. Some settings have not been read.")

    ; From elib-string.el:
    ; No entries.

    ; From lyskom-rest.el:

    (mode-line-unread . " Unread ")
    (mode-line-letters . "letters ")
    (frame-title-unread . "Unread")
    (frame-title-letters . " letters")
    (sessions-with-unreads . "Sessions with unread texts")
    (unread-letters . "unread letters")

    (bad-text-no-prefix . "Unable to translate prefix `%s' to a text number")
    (prefix-arg-try-again . "Specify another text or press control-g to abort.\n")
    (error-code . "%#2?+%[Error code %#2d/%#3S: %]%[%]%#1s.\n")
    (error-in-kom-do-when-done . "The variable kom-do-when-done has an erroneous value.
You should set it to a better value.\n")
    (extended-command . "LysKOM: ")
    (wait-for-server . "LysKOM is waiting for a response from the server. Wait for the prompt.\n")
    (review-text-q . "Review text: ")

    (completely-read-conf . "You have seen all the texts in this conference.\n")
    (not-in-any-conf . "You are not in a conference now.\n")

    (all-conf-unread-r . "You have nothing unread.\n")
    (all-conf-unread-s . "You have nothing unread. ")
    (enter-conf-unread . "%#1d unread text%#1?d%[%]%[s%]")
    (enter-conf-unread-faq . "%#1d unread FAQ%#1?d%[%]%[s%]")

    (save-one-on-file-q . "Save copy of text %#1n in file: ")
    (save-many-on-file-q . "Save copies of %#1d texts in file: ")
    (saving-one-on-file . "Saving copy of text %#1n in %#2s.\n")
    (saving-many-on-file . "Saving %#1d texts in %#2s.\n")
    (save-text-to-file-q . "Save body of text %#1n in which file: ")
    (save-text-confirm . "File %#1s exists. Overwrite? ")
    (saving-text . "Saving text %#1n as %#2s...")
    (what-save-no . "Which text do you want to save? ")
    (wait-for-prompt . "Wait for the prompt.")

    (conference-no . "<conference %#1d>")
    (person-no . "<person %#1d>")
    (prompt-several-messages . "(%d messages)")
    (prompt-single-message . "(%d message)")

    (text-buffer-missing . "The text buffer no longer exists.\n")
    (re-edit-text-prompt . "Edit the text that couldn't be created")
    (go-to-pri-conf-prompt . "Go to next prioritized conference")
    (read-pri-text-conf . "Read next prioritized text")
    (review-next-text-prompt . "Review next text")
    (review-next-comment-prompt . "Review next comment")
    (review-next-marked-prompt . "Review next marked text")
    (review-next-faq-prompt . "Review next FAQ")
    (read-next-letter-prompt . "Read next letter")
    (read-next-footnote-prompt . "Read next footnote")
    (read-next-comment-prompt . "Read next comment")
    (read-next-text-prompt . "Read next text")
    (read-next-attachment-prompt . "Read next attachment")
    (go-to-conf-of-marked-prompt . "Resume reviewing marked")
    (go-to-conf-of-review-tree-prompt . "Resume reviewing comments")
    (go-to-conf-of-review-prompt . "Resume reviewing texts")
    (go-to-conf-of-review-faq-prompt . "Resume reviewing FAQs")
    (go-to-next-conf-prompt . "Go to next conference")
    (go-to-your-mailbox-prompt . "Go to your mailbox")
    (next-pri-session-prompt . "Go to prioritized LysKOM \"%#1s\"")
    (next-unread-session-prompt . "Go to LysKOM \"%#1s\"")
    (no-such-kom-session . "No such session with unread texts.\n")
    (the-command . "Command: %#1C")
    (error-in-login-hook . "There was an error in your kom-login-hook: %#1s\n")

    (give-a-number . "Enter a number: ")

    (yes-regexp . "\\`[yY][eE][sS]\\'")
    (no-regexp . "\\`[nN][oO]\\'")
    (yes-string . "Yes")
    (no-string . "No")
    (yes-or-no-nag . "Answer yes or no.")
    (yes-or-no . "(yes or no) ")

    (y-or-n-instring . "yYnN ")
    (j-or-n-nag . "Answer y or n. ")
    (j-or-n . "(y or n) ")
    (y-instring . "yY ")

    (a-or-b-or-c-nag . "Pleas press a valid key. ")

    (person-does-not-exist . "User %#1d (does not exist).")
    (conference-does-not-exist . "Conference %#1d (does not exist).")
    (conf-no-does-not-exist-r . "Conference %#1d does not exist.")
    (person-is-anonymous . "Anonymous user")

    (process-signal . "Signal from the process.")
    (dead-session . "No active LysKOM session.")
    (not-lyskom-buffer . "This is not an active LysKOM session.")
    (closed-connection . "
**************************************************
%#2s
LysKOM session killed abnormally
Error message: %#1s**************************************************")
    (error-not-found . "Error code %#1d. No explanation available.")

    ; Useful in more places than one:
    (illegal-command . "Illegal command.\n")
    (no-such-text . "The text doesn't exist.\n")
    (no-such-text-m . "The text doesn't exist.")
    (nobody . "nobody")
    (everybody . "everyone")
    (everything . "everything")
    (anybody . "anyone")
    (forward . "forward")
    (backward . "backward")
    (wait . "Wait a moment...\n")
    (comment . "Comment")
    (comment-prefix . "\\([Cc][Oo]\\)")
    (footnote . "Footnote")
    (footnote-prefix . "\\([Ff]\\)")
    (by . " by %#1P")
    (text-created .  "Text %#1n has been created.\n")
    (text-created-anonymous .  "\
Text %#1n has been created (anonymously). To make it more difficult
for others to figure out that you wrote the text, you should wait a while
before reading it.\n")

    (resolve-session . "Which session: ")

    (starting-program . "Starting %#1s...")
    (super-jump . "Filtering subject \"%#1r\" in conference \"%#2M\"\n")
    (no-recipient . "There are no recipients for this text.\n")
    (filtered . "[Filtered]")
    (filter-error-specification . "Error in the filter specification")
    (filter-error-bad-not . "Error in the filter specification after 'not'")
    (filter-error-unknown-key . "The filter key '%S' is unknown.")
    (filter-error-key-arg . "Bad filter data (%S %S)")
    (filter-tree . "Skipping text %#1n \"%#2r\" by %#3P and all its comments.\n")
    (filter-text . "Skipping text %#1n \"%#2r\" by %#3P.\n")
    (filter-permanent . "Permanent? ")
    (filter-action . "Filter how? ")
    (filter-in-conf . "In which conference? (all) ")
    (filter-subject . "Filter which subject? ")
    (filter-recipient . "Filter which recipient? ")
    (filter-which-text . "Filter texts containing: ")
    (filter-author . "Filter which author? ")
    (permanent . "(permanent)")
    (temporary . "(temporary)")
    (filter-edit-buffer-name . "*LysKOM Filter Edit*")
    (filter-edit-empty-list . "Empty list")
    (filter-edit-start-of-list . "Beginning of list")
    (filter-edit-end-of-list . "End of list")
    (filter-edit-filter-how . "Filter how? ")
    (filter-edit-filter-what . "What do you want to filter? ")
    (filter-edit-bad-argument . "Bad input: %s")
    (filter-edit-outside-entry . "Can't do that outside a filter")
    (filter-edit-outside-list . "Can't do that outside the list")
    (filter-edit-end-of-pattern . "End of filter")
    (filter-edit-save-p . "Save changes? ")
    (filter-edit-remove-empty . "Empty filters cause all texts to be filtered. Do you want to remove these? ")
    (filter-edit-restart-p . "You have made changes. Really revert? ")
    (filter-edit-help . "p Up, n Down, i New line, M-i New filter, d Delete line, M-d Delete filter")
    (filter-edit-header . "Edit filters on \"%s\"\n")
    (filter-edit-saving . "Saving changes...")
    (filter-edit-saving-done . "Saving changes...done")
    (filter-edit-saving-error . "Couldn't save changes!")
    (filter-edit-insert-pred . "%#1s (=,!=): ")
    (filter-edit-insert-arg . "%#1s %#2s (what): ")
    (no-filters . "No filters are defined.\n")
    (view-filters-header . "\nActive filters:\n\n")
    (view-filters-footer . "")

    (ansaphone-new-message . "New Ansaphone message: ")
    (ansaphone-message . "Ansaphone message:
----------------------------------------------------------------------
%#1t
----------------------------------------------------------------------
")
    (ansaphone-message-long . "Ansaphone message:
-------------------------------------------------------------------------------
%#1t
-------------------------------------------------------------------------------
")
    (ansaphone-state . "The Ansaphone is %#1s.")
    (ansaphone-state-r . "The Ansaphone is now %#1s.\n")
    (ansaphone-messages-gone . "Recorded messages have been erased.")
    (ansaphone-no-messages . "No recorded messages.\n")
    (ansaphone-message-list-start . "Recorded messages:\n\n")
    (ansaphone-message-list-end . "\n\n")
    (ansaphone-message-header . "Automatic reply (set %#1s):\n")

    (remote-erase-messages . "Remote control (%#1P %#2s): Erased recorded messages\n")
    (remote-set-message . "Remote control (%#1P %#2s): Ansaphone message:
----------------------------------------------------------------------
%#3t
----------------------------------------------------------------------
")
    (remote-set-message-long . "Remote control (%#1P %#2s): Ansaphone message:
-------------------------------------------------------------------------------
%#3t
-------------------------------------------------------------------------------
")    (remote-set-ansaphone . "Remote control (%#1P %#2s): The ansaphone is %#3s\n")
    (remote-list-messages . "Remote control (%#1P %#2s): Listed recorded messages\n")
    (remote-quit . "Remote control (%#1P %#2s): Quit\n")

    (illegal-remote . 
"Illegal remote control attempt:
Time: %#1s
From: %#2P <%#2p>
To  : %#3P <%#3p>
Text: 
%#4t")
    (illegal-remote-reply . "Remote control rejected: %#1s") 
    (remote-not-in-list . "Unauthorised person")
    (remote-bad-command . "Unknown or malformed command")
    (remote-unknown-error . "Unknown error")

    (remote-control-who . "Remotely control which session? ")
    (remote-control-autoreply . "Ansaphone on or off? ")

    (state-on . "on")
    (state-off . "off")
    

    (text-popup-title . "Text %#1s")
    (conf-popup-title . "Conference %#1s")
    (pers-popup-title . "User %#1s")
    (url-popup-title  . "URL %#1s")
    (aux-popup-title  . "Auxiliary information")
    (timestamp-popup-title . "Timestamp %#1s")
    (recpt-type-popup-title . "Recipient type: %#1s")
    (add-recpt-button-text . "[Add...]")
    (add-recpt-button-text-regex . "\\[Add\\.\\.\\.\\]")
    (generic-popup-title . "%#1s")

    (who-i-am-not-present . "%#1P (not in any conference) \n")
    (who-i-am-present . "%#1P is present in %#2M\n")
    (who-i-am-client . "The program is lyskom.el, version %#1s%#2?b%[ (MULE)%]%[%].\n")
    (who-i-am-server . "This is %#1s, version %#2s.\n")
    (who-i-am-emacs . "Running under %#1s%#2?b%[ (MULE)%]%[%].\n")

    (no-such-session-r . "That session does not exist. Perhaps the user is not logged on.\n")
    (person-not-logged-in-r . "%#1P %#2?b%[has not been logged on since %#2s%]%[is not logged on%].\n")

    (session-status . "Session %#1d is %#2P <%#2p>
%#5s %#7s %#4M
Using %#6D from %#3s\n")
    (session-status-9 . "Session %#1d is %#2P <%#2p>
%#5s %#7s %#4M
Using %#6D from %#3s
On since %#8s%#9s")
    (session-status-inactive . "\nHas been inactive for %#1s\n")
    (one-day . "one day")
    (one-hour . "one hour")
    (one-minute . "one minute")
    (years . "years")
    (year . "år")
    (month . "month")
    (months . "months")
    (days . "days")
    (day . "day")
    (hours . "hours")
    (minutes . "minutes")
    (and . "and")
    (session-is-active . " and is currently active.\n")
    (session-is-invisible . "This session is invisible.\n")
    (status-for-session . "Session status for whom? ")
    (unknown-doing-what . "Exists")
    (doing-where-conn . "in")
    (doing-nowhere-conn . "but is")
    (waiting-for-membership . "Waiting for the membership list to be fetched...%d/%d")

    ;; From slow.el
    (no-such-command . "There is no such command.\n")
    (command-completions . "You may mean one of the following:\n %#1s\n")

    (which-language . "Change language to: ")
    (send-formatted . "Send as formatted text? ")
    (changing-language-to . "Changing to %#1s.\n")
    (language-set-to . "Language set to %#1s.\n")
    (language-not-loaded . "%#1s is unavailable.\n")

    (reformat-html . "HTML")
    (reformat-enriched . "enriched")
    (reformat-filled . "filled")

    (reformat-truncated . "truncated")

    (reformat-signature . "designed")

    (reformat-deswascii . "was swascii")

    (need-library . "The \"%#1s\" package is required for this command.\n")
    (calc-expression . "Expression: ")

    (do-send-anonymous . "Send the text anonymously? ")
    (remove-self-sending-anonymous . "Remove yourself as a recipient of the text? ")
    (anonymous . "anonymous")
    (Anonymous . "Anonymous")

    (secret-conf-letter . "S")
    (protected-conf-letter . "P")
    (created-conf-letter . "C")
    (superconf-conf-letter . "S")
    (supervisor-conf-letter . "O")

    ;; Some Help

    (where-is-doesnt-exist . "The command %#1s does not exist")
    (where-is-on-no-key . "%#1s is not on any key")
    (where-is-on-key . "%#1s is on %#2s")

    ;; From aux-items.el

    (content-type-aux . "Content type: %#1s")
    (content-type-regexp . "Content type: \\(\\S-+\\)")

    (agreeing . "Agreeing with text %#1n...")
    (fast-replying . "Remark to text %#1n...")
    (author-fast-replies . "Remarks by the author:")
    (author-fast-reply-aux . "  \"%#1t\"")
    (other-fast-replies . "Remarks:")
    (other-fast-reply-aux . "  \"%#1t\" /%#2P/")
    (fast-reply-too-long . "Can't create remarks that consist of more than one line.\n")

    (faq-for-conf-aux . "FAQ for %#1M <%#1m>") 
    (faq-for-server-aux . "FAQ for the LysKOM-server") 
    (faq-in-text-aux . "FAQ in text:                 %10#1n %#2D")
    (conf-to-add-faq . "Which conference do you want to add an FAQ for? ")
    (text-to-add-as-faq . "Which text is the new FAQ? ")
    (text-to-change-as-faq . "Which FAQ do you want to change? ")
    (adding-faq . "Adding text %#1n as an FAQ for %#2?b%[%#2M%]%[the server%]...")
    (conf-to-del-faq . "Which conference do you want to remove an FAQ from? ")
    (text-to-del-as-faq . "Which text do you want to remove as FAQ? ")
    (deleting-faq . "Removing text %#1n as FAQ for %#2?b%[%#2M%]%[the server%]...")
    (conf-has-no-faq . "%#1?b%[%#1M%]%[The server%] has no FAQ\n")
    (unread-which-faq . "Mark FAQ for which conference as unread? ")
    (view-which-faq . "View FAQ for which conference? ")
    (review-faq-for-r . "View FAQ for %#1?b%[%#1M%]%[the server%].\n")
    (set-faq-for-conf-done . "FAQ for %#1?b%[%#1M%]%[the server%] set to text %#3n.\n")
    (set-faq-for-conf-failed . "Unable to set the FAQ for %#1?b%[%#1M%]%[the server%] to text %#3n.\n")
    (changed-faq-for-conf-done . "FAQ in text %#2n for %#1?b%[%#1M%]%[the server%] changed to text %#3n.\n")
    (changed-faq-for-conf-failed . "Unable to change FAQ for %#1?b%[%#1M%]%[the server%] to text %#3n.
%#2?b%[Text %#2n is still the FAQ.
%]%[%]%#4s")
    (faq-in-text . "FAQ in text %#1n %#3s%#4s")
    (faq-in-text-by . "FAQ in text %#1n %#5s %#3sby %#2P %#4s")
    (server-has-new-faq . "\nThere %#1?d%[is a new FAQ%]%[are %#1d new FAQs%] for the server.\n")
    (unread-faqs-header . "\nUnread FAQ%#1?d%[%]%[s%] for %#2?b%[%#2M%]%[the server%]::\n")

    (too-many-languages . "Cannot code that many character sets. Send uncoded? ")
    (too-many-content-types . "Cannot figure out what content type you want. Simplify the text.")

    (cross-reference-text-status-aux . "See text:              %10#1n /%#2P/")
    (cross-reference-conf-status-aux . "See conference:                         <%#1m> %#1M /%#2P/")
    (cross-reference-pers-status-aux . "See person:                             <%#1p> %#1P /%#2P/")
    (strange-cross-reference-status .  "See also:                                %#1s (whatever that means)")
    (cross-reference-text-aux . "See text %#1n /%#2P/")
    (cross-reference-conf-aux . "See conference <%#1m> %#1M /%#2P/")
    (cross-reference-pers-aux . "See person <%#1p> %#1P /%#2P/")
    (cross-reference-text-regexp . "See text \\([0-9]+\\)")
    (cross-reference-conf-regexp . "See conference <\\([0-9]+\\)>")
    (cross-reference-pers-regexp . "See person <\\([0-9]+\\)>")
    (strange-cross-reference . "See %#1s, whatever that might mean")

    (no-comments-aux . "The author has requested others not to comment this text")
    (no-comments-edit-aux . "Request that others do not comment")
    (no-comments-regexp . "Request that others do not comment")
    
    (personal-comment-aux . "The author has requested private replies only")
    (personal-comment-edit-aux . "Request private replies only")
    (personal-comment-regexp . "Request private replies only")
    
    (request-confirmation-aux . "%#1P has requested confirmation of reading")
    (request-confirmation-edit-aux . "Request confirmation of reading")
    (request-confirmation-regexp . "Request confirmation of reading")
    (confirm-read-q . "Confirm reading text %#1n? ")
    
    (read-confirm-aux . "Confirmed reading: %#1P %#2s")

    (redirect-email-aux . "Send e-mail to %#1s")
    (redirect-lyskom-aux . "Send texts to conference %#1M")

    (label-what-kind . "Set personal label on what (text, conference, user)? ")
    (label-what-text . "Set personal label on text: ")
    (label-what-conf . "Set personal label on which conference? ")
    (label-what-pers . "Set personal label on which person? ")
    (label-what-label . "Personal label: ")
    (label-secret    . "Should others to be able to see the label? ")

    (creating-software-aux . "Created with %#1s")
    (world-readable-text-aux . "The text can be read without logging on")
    (world-readable-text-edit-aux . "Make the text readable without logging on")

    (cant-get-aux-item . "Can't find auxiliary information\n")
    (aux-item-no-info . "No information available\n")
    (aux-item-info . "\
Number:        %#1d %#6s
Type:          %#2d (%#3s)
Created by:    %#4P <%#4p>
Created:       %#5s
Flags:         %#7s
Inherit limit: %#8s
Contents:     \"%#9s\"
")
    (secret . "Secret")
    (deleted . "Deleted")
    (hide-creator . "Secret creator")
    (inherit . "Inherited")
    (unlimited-inherit . "Unlimited inheritance")
    (no-inheritance . "Inheritance is off")
    (no-more-inheritance . "Inherit limit reached; will not be further inherited")
    (inherit-steps . "%#1d steps")

    (aux-item-for . "Auxiliary information for ")
    (aux-item-for-conference-no . "conference <%#1m> %#1M")
    (aux-item-for-text-no . "text %#1n")
    (aux-item-for-server . "the server")

    (what-fast-reply-no . "Remark to which text? ")
    (fast-reply-prompt . "Remark: ")
    (agree-prompt . "Text: ")
    (default-agree-string . "I agree")
    (what-agree-no . "Agree with which text? ")

    (what-kind-to-add-aux-to . "Attach information to what? ")
    (which-conf-to-add-aux-to . "Which conference to you want to attach information to? ")
    (which-text-to-add-aux-to . "Which text to you want to attach information to? ")
    (which-aux-item-tag . "Information tag: ")
    (which-aux-item-inherit . "Set the  inherit-flag? ")
    (which-aux-item-secret . "Set the secret-flag? ")
    (which-aux-item-anonymous . "Set the anonymous-flag? ")
    (which-aux-item-rsv1 . "Set the dont-garb-flag? ")
    (which-aux-item-rsv2 . "Set the reserved2-flag? ")
    (which-aux-item-rsv3 . "Set the reserved3-flag? ")
    (which-aux-item-rsv4 . "Set the reserved4-flag? ")
    (which-aux-item-inherit-limit . "Inherit limit: ")
    (which-aux-item-data . "Data: ")

    ;; Cross-reference stuff

    (person . "User")
    (conference . "Conference")
    (text . "Text")
    (server . "Server")

    (xref-type . "What to you want to refer to (text, conference or person)? ")
    (which-text-to-xref . "Add reference to text: ")
    (which-text-to-xref-err . "Text not found. Add reference to text: ")
    (which-pers-to-xref . "Add reference to user: ")
    (which-conf-to-xref . "Add reference to conference: ")

    ;; Aux-item stuff from edit-text

    (no-comments-q . "The author doesn't want comments. Comment anyway? ")
    (private-answer-q . "The author wants private replies. Write private reply? ")

    (your-invited . "You have been invited to %#1M by %#2P.
By accepting the invitation you will continue to be a member of %#1M.
If you decline the invitation you will not be a member of the conference.
")
    (accept-invitation . "Do you want to accept the invitation now? ")
    (enter-passive . "You are a passive member of %#1M.
You must become an active member of the conference to enter it.\n")
    (convert-passive . "Vill du bli aktiv medlem i %#1M? ")
    (bug-secret-mship . "You are a secret member of %#1M\n")

    (invitation-mt-type . "invited")
    (passive-mt-type . "passive")
    (secret-mt-type . "secret")

    (Invitation-mt-type . "Invited")
    (Passive-mt-type . "Passive")
    (Secret-mt-type . "Secret")

    (not-author-try-anyway-p . "You are not the author of the text. Try anyway? ")
    (what-no-comments-no . "Which text do you want to prevent comments to: ")
    (adding-no-comments . "Preventing comments to text %#1n...")
    (already-no-comments . "Text %#1n already prevents comments.\n")

    (what-private-answer-no . "Which text do you want private replies to: ")
    (adding-private-answer . "Requesting only private replies to text %#1n...")
    (already-private-answer . "Text %#1n already has a request for private replies only.\n")

    (what-request-confirm-no . "Which text do you want read confirmations for: ")
    (adding-request-confirm . "Requesting read confirmations for text %#1n...")
    (already-request-confirm . "Text %#1n already has a request for read confirmations.\n")

    (review-mail-headers-to-what . "Which text's mail headers do you want to see? ")
    (no-mail-headers . "Text %#1n has no mail headers\n")
    (mail-headers-for . "Mail headers for text %#1n:\n")
    (email-name-prefix . "")
    (email-name-suffix . "")

    (you-are-anonymous . "You are now somewhat anonymous.\n")
    (you-are-nonanonymous . "You are no longer anonymous.\n")
    (you-are-already-anonymous . "You are already somewhat anonymous.\n")
    (you-are-already-nonanonymous . "You are already non-anonymous.\n")

    (start-keep-alive . "Sending data at %#1d second intervals to keep the connection active.")
    (stop-keep-alive . "No longer keeping connection active by sending extra data.")

    (review-converted-q . "Review which text converted?")
    (review-noconversion-q . "Review which text unconverted?")
    (unread-commented-q . "Mark the commented for which text as unread?")
    (review-commented-q . "Review the commented for which text?")

    (review-tree-q . "Review all comments recursively for which text?")
    (find-root-q . "Review original text for text?")
    (find-root-review-q . "Review tree of which text?")
    (review-comments-q . "Review all comments to which text?")
    (unread-tree-q . "Unread all comments recursively for which text?")
    (unread-root-q . "Unread original text for text?")
    (unread-root-review-q . "Unread tree of which text?")
    (unread-comments-q . "Unread all comments to which text?")

    (confusion-who-to-reply-to . "I can't figure out which text you want to write a private reply to.\n")
    (confusion-what-to-answer-to . "I can't figure out which text you want to write a reply to.\n")
    (confusion-what-to-view . "I can't figure out which text you want to view.\n")

    (confusion-what-to-reply-to . "I can't figure out which text you want to write a remark to.\n")
    (confusion-what-to-agree-to . "I can't figure out which text you want to agree with.\n")
    (confusion-what-to-request-confirmation . "I can't figure out which text you want read confirmations for.\n")
    (confusion-what-to-comment . "I can't figure out which text you want to comment.\n")
    (confusion-what-to-footnote . "I can't figure out to which text you want to write a footnote to.\n")

    (confusion-what-to-delete . "I can't figure out which text you want to delete.\n")
    (confusion-what-to-add-sub-recipient . "I can't figure out which text you want to %#1s.\n")
    (add-rcpt-action-name . "add a recipient to")
    (add-copy-action-name . "add a carbon copy recipient to")
    (add-bcc-action-name . "add a blind carbon copy recipient to")
    (sub-action-name . "remove a recipient from")
    (move-action-name . "move")

    (confusion-what-to-add-comment-to . "I can't figure out which text you want to add a comment to.\n")
    (confusion-what-to-sub-comment-from . "I can't figure out which text you want to remove a comment from.\n")
    (confusion-what-to-add-footnote-to . "I can't figure out which text you want to add a footnote to.\n")
    (confusion-what-to-sub-footnote-from . "I can't figure out which text you want to remove a footnote from.\n")
    
    (confusion-what-to-mark . "I can't figure out which text you want to mark.\n")
    (confusion-what-to-unmark . "I can't figure out which text you want to unmark.\n")
    (confusion-what-to-save . "I can't figure out which text you want to save.\n")

    (confusion-what-to-review-mail-headers . "I can't figure out which text's mail headers you want to see.\n")
    (confusion-what-to-unread-root . "I can't figure out which text's root you want to mark as unread.\n")
    (confusion-what-to-unread-root-review . "I can't figure out which tree you want to mark as unread.\n")
    (confusion-what-to-find-root . "I can't figure out which text's root you want to see.\n")
    (confusion-what-to-find-root-review . "I can't figure out which tree you want to review.\n")

    ;; Help stuff

    (help-with-what . "What do you want help with? ")
    (help-for . "Help for \"%#1s\"\n")

    ;; Button actions

    (lyskom-button-view-text-action . "View text")
    (lyskom-button-copy-text-no-action . "Copy text number")
    (lyskom-button-review-converted-action . "Review converted")
    (lyskom-button-review-noconversion-action . "Review unconverted")
    (lyskom-button-find-root-review-action . "Review tree")
    (lyskom-button-find-root-action . "Review root text")
    (lyskom-button-comment-text-action . "Write comment")
    (lyskom-button-private-comment-text-action . "Write personal reply")
    (lyskom-button-mark-text-action . "Mark text")
    (lyskom-button-unmark-text-action . "Unmark text")
    (lyskom-button-save-text-action . "Save archive copy")
    (lyskom-button-save-text-body-action . "Save text body")
    (lyskom-button-review-comments-action . "Review all comments")
    (lyskom-button-review-tree-action . "Review all comments recursively")
    (lyskom-button-write-footnote-action . "Write footnote")
    (lyskom-button-fast-reply-action . "Remark")
    (lyskom-button-view-conf-presentation-action . "View presentation")
    (lyskom-button-view-conf-status-action . "View conference status")
    (lyskom-button-goto-conf-action . "Go to conference")
    (lyskom-button-send-message-action . "Send group message")
    (lyskom-button-add-self-action . "Join conference")
    (lyskom-button-sub-self-action . "Leave conference")
    (lyskom-button-view-pers-presentation-action . "View presentation")
    (lyskom-button-view-pers-status-action . "View user status")
    (lyskom-button-view-session-status-action . "View session status")
    (lyskom-button-mail-action . "Send letter")
    (lyskom-button-send-message-action . "Send personal message")
    (lyskom-button-open-url-action . "Open")
    (lyskom-button-copy-url-action . "Copy")
    (lyskom-button-goto-info-node-action . "Open")
    (lyskom-button-open-email-action . "Send mail")
    (lyskom-button-copy-email-action . "Copy")
    (lyskom-button-info-aux-action . "Information")
    (lyskom-button-delete-aux-action . "Delete")
    (lyskom-button-copy-timestamp-action . "Copy")
    (lyskom-button-recpt-type-recipient . "Recipient")
    (lyskom-button-recpt-type-copy . "Carbon copy")
    (lyskom-button-recpt-type-bcc . "Blind Carbon copy")
    (lyskom-button-recpt-type-sub . "Remove")
    (lyskom-button-recpt-add-recipient . "Recipient")
    (lyskom-button-recpt-add-copy . "Carbon copy recipient")
    (lyskom-button-recpt-add-bcc . "Blind carbon copy recipient")
    (lyskom-button-aux-type-xref . "Reference to conference/person/text")
    (lyskom-button-aux-type-no-comments . "Request no comments")
    (lyskom-button-aux-type-personal-comments . "Request personal reply")

    (lyskom-edit-toggle-secret-aux-action . "Toggle \"secret\"")
    (lyskom-edit-toggle-anonymous-aux-action . "Toggle \"anonymous\"")
    (lyskom-edit-toggle-inherit-aux-action . "Toggle \"inherited\"")
    (lyskom-edit-delete-aux-action . "Delete")
    (lyskom-prioritize-flag-toggle-action . "Toggle")
    (lyskom-prioritize-flag-set-action . "Set")
    (lyskom-prioritize-flag-clear-action . "Clear")

    (server-status-header   . "Status for LysKOM-server %#1s%#2?b%[ (%#2s:%#3d)%]%[%]\n\n")
    (server-status-server   . "Canonical server name:                   %#1s%#2?b%[:%#2s%]%[%]\n")
    (server-status-version  . "Software version:                        %#1s %#2s\n")
    (server-status-protocol . "Protocol version:          %15#1d\n")
    (server-status-sessions . "\
Number of sessions:  %21#1d (total)
                     %21#2d active in the last %#7d minutes
                     %21#3d inactive sessions
                     %21#4d unknown activity
                     %21#5d invisible sessions
                     %21#6d not logged on/secret/zombies\n")
    (server-status-first-text . "Oldest existing text:      %15#1n\n")
    (server-status-last-text  . "Youngest existing text:    %15#1n\n")
    (server-status-has-motd . "\nThe server has a notice:\n")
    (server-status-time . "Serverns tid:                   %#1s\n")

    (mship-type-invitation-q . "Membership invitation? ")
    (mship-type-passive-q . "Passive membership? ")
    (mship-type-secret-q . "Secret membership? ")
    (recommend-which-conf . "Which conference do you want to recommend? ")
    (recommend-set-priority-q . "Recommend a priority? ")
    (recommend-set-mship-type-q . "Recommend a membership type? ")
    (recommending-conf . "Recommending %#1M%#2?b%[ (priority %#2d)%]%[%]%#3?b%[ %#3s%]%[%]...")
    (has-recommended-conf . "%#1P has recommended conference %#2M. Do you want to become a member?\n")
    (accept-recommendation . "Join conference %#1M? ")
    (accepting-recommendataion . "Joining recommended conference %#1M...")

    (redirect-for-whom . "Redirect comments for whom: ")
    (redirect-to-which-conf . "Redirect comments to: ")
    (redirecting-comments-to . "Redirecting comments for %#1P to %#2M%#3?b%[ (old %#1M)%]%[%]...")

    (kom-redirect-comments-e49 . "You do not have permission to perform this redirection")
    (kom-redirect-comments-e48 . "Comment redirection is not supported by the server")

    (external-program-missing . "Can't find the external program \"%#1s\" that is required for this command.")
    (ignored-user-area-var . "\

Ignoring the following variables in your settings:

    %#1s

These variables are not registered as LysKOM variables and cannot be
read from the server. You can remove the variables from the server by
giving the command \"Save options\".\n\n")


    (unknown-aux-item . "Unknown auxiliary information")
    (text-header-aux-item . "%#1s: <%#2d> \"%#3s\" %#4s")

    (aux-content-type-name . "Content type")
    (aux-fast-reply-name . "Remark")
    (aux-cross-reference-name . "Reference")
    (aux-no-comments-name . "Request for no comments")
    (aux-personal-comment-name . "Request for personal replies")
    (aux-request-confirmation-name . "Request for read confirmation")
    (aux-read-confirm-name . "Read confirmation")
    (aux-redirect-name . "Redirect")
    (aux-x-face-name . "Picture")
    (aux-alternate-name-name . "Alternate name")
    (aux-pgp-signature-name . "PGP signature")
    (aux-pgp-public-key-name . "PGP public key")
    (aux-e-mail-address-name . "E-mail address")
    (aux-faq-text-name . "FAQ in text")
    (aux-creating-software-name . "Creating software")
    (aux-mx-author-name . "E-mail author")
    (aux-mx-from-name . "E-mail from")
    (aux-mx-reply-to-name . "E-mail reply to")
    (aux-mx-to-name . "E-mail to")
    (aux-mx-cc-name . "E-mail CC")
    (aux-mx-date-name . "E-mail date")
    (aux-mx-message-id-name . "E-mail message ID")
    (aux-mx-in-reply-to-name . "E-mail in reply to")
    (aux-mx-misc-name . "E-mail headers")
    (aux-mx-allow-filter-name . "E-mail allow-filter")
    (aux-mx-reject-forward-name . "E-mail reject-forward")
    (aux-notify-comments-name . "Notify me about comments")
    (aux-faq-for-conf-name . "FAQ for conference")
    (aux-recommended-conf-name . "Conference recommendation")
    (aux-allowed-content-type-name . "Allowed content type")
    (aux-canonical-name-name . "Canonical name")
    (aux-mx-list-name-name . "Mailing list name")
    (aux-send-comments-to-name . "Send comments to")
    (aux-world-readable-name . "World-readable")
    (aux-mx-refuse-import . "Refuse text import")
    (aux-elisp-client-read-faq-name . "Read FAQ")
    (aux-elisp-client-rejected-invitation-name . "Rejected invitation")

    (review-marking-as-read . "Review marks as read")
    (review-not-marking-as-read . "Review does not mark as read")

    (using-ssh-connection . "Using ssh connection to %#1s...")
    (opening-ssh-connection . "Opening ssh connection to %#1s...")
    (ssh-cant-connect . "Unable to open ssh connection: %s")
    (ssh-closing . "Closing ssh connection to %#1s")
    (ssh-unknown-host . "unknown host")

    (keyboard-cancel . "Cancel")
    (keyboard-menu-help . "(choose: C-n, C-p; confirm: RET)")
    (customize-help . "See the beginning of the buffer for more information")

    (no-mule-warning . "\

%#1@You have disabled support for multibyte characters (MULE) in emacs.
This may be due to a call to standard-display-european in your .emacs
or in one of the shared emacs startup files.

This software is designed to run with support for multibyte characters
enabled. Without this support texts may be displayed incorrectly, and
you may encounter problems writing texts. Consider enabling sypport
for multibyte characters in emacs.

")
    (coding-system-mismatch-warning . "\

%#3@Your emacs is configured to encode characters using \"%#1s\",
but the LysKOM server you are using recommends \"%#2s\".

You may encounter problems when writing comments, and comments to your
texts may contain a mix of characters that cannot be encoded or
displayed correctly. Please consider changing your language
environment to one that uses \"%#2s\" to encode text.

")


    (has-nameday-1 . "Today's name is %#1s%#2?b%[ (%#2s)%]%[%].")
    (has-nameday-2 . "Today's names are %#1s and %#2s%#3?b%[ (%#3s)%]%[%].")

    (invalid-date-entry . "Invalid date")
    (number-out-of-range . "Invalid number")
    (or-date . "or date")

    (set-unread-n . "Only read the most recent %#1?d%[text%]%[%#1d texts%].\n")
    (set-unread-date . "Only read texts since %#2s %#3d, %#1d.\n")

    (jump-from-text . "Skip comments to which text? ")
    (jumping-from-text . "Skipping comments to text %#1n.\n")
    (jumping-from-text-review . "Skipping reviewed comments.\n")

    (session-to-copy-options-from . "Session to copy options from: ")
    (reading-settings-from . "Reeading options from %s...")
    (reading-settings-from-done . "Reading options from %s...done")

    (super-jump-q . "Super jump which text? ")
    (conf-list-legend . "* Not member; - Passive member\n")

    (unread-text-q . "Make which text unread? ")
    (marking-text-unread . "Marking text %#1n as unread...")
    (cant-mark-text-unread . "Couldn't mark %#1n as unread (%#2s)\n")
    (confusion-what-to-mark-unread . "I can't figure out what text you want to mark as unread.\n")
    (no-comment-to-to-unread . "There is no commented text to mark as unread.\n")

    (mx-refuse-import-spam . "Spam")
    (mx-refuse-import-all . "All imported texts")
    ))


(lyskom-language-var local lyskom-month-names en
  '(("january" . 1)   ("jan" . 1)
    ("february" . 2)  ("feb" . 2)
    ("march" . 3)     ("mar" . 3)
    ("april" . 4)     ("apr" . 4)
    ("may" . 5)       ("may" . 5)
    ("june" . 6)      ("jun" . 6)
    ("july" . 7)      ("jul" . 7)
    ("august" . 8)    ("aug" . 8)
    ("september" . 9) ("sep" . 9)
    ("october" . 10)  ("oct" . 10)
    ("november" . 11) ("nov" . 11)
    ("december" . 12) ("dec" . 12)))


;;; ================================================================
;;;              The commands and their associated functions


;;; Formely known as lyskom-commands
(lyskom-language-strings local lyskom-command en
  '(
    (kom-help                 . "Help")
    (kom-slow-mode            . "Long commands")
    (kom-quick-mode           . "Short commands")
    (kom-send-message         . "Send message")
    (kom-send-alarm           . "Send alarm")
    (kom-create-conf          . "Create conference")
    (kom-delete-conf          . "Delete conference/user")
    (kom-delete-text          . "Remove text")
    (kom-display-time         . "Time")
    (kom-go-to-conf           . "Go (to) conference")
    (kom-go-to-next-conf      . "(Go to) next conference")
    (kom-jump                 . "Skip (all) comments")
    (kom-list-created-conferences     . "List owned conferences") 
    (kom-list-conferences     . "List conferences") 
    (kom-list-persons         . "List users")
    (kom-list-news            . "List news")
    (kom-list-re              . "List (using) regexps")
    (kom-membership           . "List memberships")
    ;; (kom-list-marks	      . "List marks")
    (kom-postpone             . "Postpone reading")
    (kom-set-session-priority . "Set reading level")
    (kom-prioritize           . "Prioritize conferences")
    (kom-status-person        . "Status (of) person")
    (kom-status-conf          . "Status (of) conference")
    (kom-add-self             . "Join conference")
    (kom-change-priority      . "Change priority")
    (kom-list-summary         . "List text (subjects)")
    (kom-sub-self             . "Leave (a) conference")
    (kom-quit                 . "Quit")
    (kom-recover              . "Recover") 
    (kom-start-anew           . "New User")
    (kom-view                 . "Review text")
    (kom-find-root-review     . "Review tree")
    (kom-review-comments      . "Review all comments")
    (kom-review-tree          . "Review all comments recursively")
    (kom-review-clear         . "Review and skip")
    (kom-review-last-normally-read
     			      . "Review again")
    (kom-review-converted     . "Review converted")
    (kom-review-noconversion  . "Review unconverted")
    (kom-review-next          . "Review next")
    (kom-find-root            . "Review original (text)")
    (kom-review-by-to         . "Review last")
    (kom-review-first         . "Review first")
    (kom-review-all           . "Review all")
    (kom-review-more          . "Review more")
    (kom-view-commented-text  . "Review (the) commented (text)")
    (kom-view-previous-commented-text
     			      . "Review (the) previously commented (text)")
    (kom-review-stack         . "Review stack")
    (kom-review-presentation  . "Review presentation")
    (kom-review-backward      . "(Review) Backwards")
    (kom-view-next-text       . "(Read) next text")
    (kom-who-is-on            . "Who (is on)")
    (kom-who-is-on-in-conference
                              . "Who (is on in) conference")
    (kom-who-is-present-in-conference
                              . "Who (is) present (in) conference")
    (kom-who-is-on-and-friend . "Which friends (are on)")
    (kom-who-am-i             . "Where (am) i")
    (kom-list-clients	      . "List clients")
    (kom-busy-wait            . "Wait (for news)")
    (kom-write-comment        . "(Write) comment")
    (kom-comment-previous     . "(Write) comment (to) previous text")
    (kom-write-footnote       . "(Write) footnote")
    (kom-private-answer       . "(Write) personal reply (by letter)")
    (kom-private-answer-previous
   . "(Write) personal (reply to) previous text (by letter)")
    (kom-set-unread     	      . "Only (the) last")
    (kom-write-text           . "Write (a) text")
    (kom-send-letter          . "Write (a) letter")
    (kom-change-name          . "Change name")
    (kom-change-parenthesis   . "Change parenthesis")
    (kom-change-password      . "Change password")
    (kom-change-supervisor    . "Change supervisor")
    (kom-change-presentation  . "Change presentation")
    (kom-get-appreciation     . "(Please) pat my head")
    (kom-get-abuse            . "(Please) kick my butt")
    (kom-mark-text            . "Mark (text)")
    (kom-unmark-text          . "Unmark (text)")
    (kom-review-marked-texts  . "Review marked (texts)")
    (kom-review-all-marked-texts . "Review all marked (texts)")
    (kom-add-recipient        . "Add recipient")
    (kom-add-copy             . "Add (recipient of) carbon copy")
    (kom-add-bcc	      . "Add (recipient of) blind carbon copy")
    (kom-sub-recipient        . "Remove recipient")
    (kom-move-text            . "Move text")
    (kom-move-text-tree       . "Move tree")
    (kom-add-comment          . "Add comment")
    (kom-sub-comment          . "Remove comment")
    (kom-add-cross-reference  . "Add cross reference")
    (kom-add-member           . "Add (a) member")
    (kom-sub-member           . "Remove (a) member")
    (kom-change-conf-motd     . "(Post) notice (on the) conference")
    (kom-set-garb-nice        . "Change expiration")
    (kom-set-super-conf       . "Change superconference")
    (kom-set-permitted-submitters . "Change allowed authors")
    (kom-unset-conf-motd      . "Remove notice")
    (kom-save-text            . "Archive (copy of) text (in file)")
    (kom-save-text-body       . "Save text body (in file)")
    (kom-save-options         . "Save options")
    (kom-shutdown-server      . "Shut down (server)")
    (kom-sync-database        . "Save (the) database")
    (kom-enable-adm-caps      . "Become administrator")
    (kom-disable-adm-caps     . "Become (normal) user")
    (kom-set-motd             . "Change login message")
    (kom-remove-motd          . "Remove login message")
    (kom-force-logout         . "Kill session")
    (kom-filter-author        . "Filter author")
    (kom-filter-subject       . "Filter subject")
    (kom-filter-recipient     . "Filter recipient")
    (kom-super-jump           . "Super jump")
    (kom-filter-edit          . "Edit filters")
    (kom-filter-text          . "Filter contents")
    (kom-list-filters         . "List filters")
    (kom-show-user-area       . "Show user area")
    (kom-change-conf-type     . "Change conference type")

    (kom-change-auto-reply    . "Change ansaphone message")
    (kom-toggle-auto-reply    . "Ansaphone")
    (kom-list-messages        . "List messages")
    (kom-erase-messages       . "Erase messages")

    (kom-remote-autoreply     . "Remote control ansaphone")
    (kom-remote-set-message   . "Remote control change ansaphone message")
    (kom-remote-list-messages . "Remote control list messages")
    (kom-remote-erase-messages . "Remote control erase messages")
    (kom-remote-quit          . "Remote control quit")

    (kom-status-session       . "Status (of) session")
    (kom-customize            . "Customize LysKOM")
    (kom-next-kom             . "Next LysKOM")
    (kom-previous-kom         . "Previous LysKOM")
    (kom-next-unread-kom      . "Next unread LysKOM")
    (kom-change-language      . "Change language")
    (kom-calculate            . "Calculate")
    (kom-list-marks           . "List marked texts")
    (kom-where-is             . "Where (is the) command")
    (kom-fast-reply           . "Remark (to text)")
    (kom-agree                . "Agree")
    (kom-add-faq              . "Add FAQ")
    (kom-del-faq              . "Remove FAQ")
    (kom-review-faq           . "View FAQ")

    (kom-sub-footnote         . "Remove footnote")
    (kom-add-footnote         . "Add footnote")

    (kom-add-no-comments      . "Prevent comments")
    (kom-add-private-answer   . "Request private reply")
    (kom-add-request-confirm  . "Request confirmation of reading")

    (kom-review-mail-headers  . "Review mail headers")

    (kom-compare-texts        . "Compare two texts")
    (kom-diff-texts           . "View diff")

    (kom-become-anonymous     . "Become anonymous")
    (kom-become-nonanonymous  . "Become non-anonymous (come forth into the light)")

    (kom-keep-alive           . "Keep connection alive")
    (kom-stop-keep-alive      . "Stop keeping connection alive")

    (kom-is-person-member-of-conference . "Check (if person is a) member (of conference)")
    (kom-will-person-read-text . "Check (if person will) read (text)")

    (kom-change-conf-faq      . "Change FAQ")

    (kom-make-review-mark-as-read . "Review marks as read")
    (kom-make-review-not-mark-as-read . "Review does not mark as read")

    (kom-set-presentation     . "Add presentation")
    (kom-remove-presentation  . "Remove presentation")
    (kom-set-motd-text        . "Add notice")
    (kom-create-aux-item      . "Create auxiliary information")
    (kom-status-server        . "Status (of) server")
    (kom-add-server-faq       . "Add server FAQ")
    (kom-del-server-faq       . "Remove server FAQ")
    (kom-review-server-faq    . "Review server FAQ")
    (kom-change-server-faq    . "Change server FAQ")
    (kom-recommend-conference . "Recommend conference")
    (kom-redirect-comments    . "Redirect comments")
    (kom-copy-options         . "Copy options")
    (kom-mark-unread          . "Unread text")
    (kom-unread-by-to         . "Unread last")
    (kom-unread-more          . "Unread more")
    (kom-unread-commented-text . "Unread (the) commented (text)")
    (kom-unread-previous-commented-text . "Unread (the) previously commented (text)")
    (kom-unread-comments      . "Unread all comments")
    (kom-unread-tree          . "Unread all comments recursively")
    (kom-unread-root          . "Unread original (text)")
    (kom-unread-root-review   . "Unread tree")
    (kom-unread-last-normally-read
     			      . "Unread again")
    (kom-unread-first         . "Unread first")
    (kom-unread-all           . "Unread all")

    (kom-unread-presentation  . "Unread presentation")
    (kom-unread-server-faq    . "Unread server FAQ")
    (kom-unread-faq           . "Unread FAQ")
    (kom-unread-marked-texts  . "Unread marked (texts)")
    (kom-unread-all-marked-texts . "Unread all markeed (texts)")

    (kom-join-all-conferences . "Join all conferences")
    (kom-leave-all-conferences . "Leave (almost) all conferences")
    ))

(lyskom-language-var global lyskom-language-codes en
  '((aa . "Afar")
    (ab . "Abkhazian")
    (af . "Afrikaans")
    (am . "Amharic")
    (ar . "Arabic")
    (as . "Assamese")
    (ay . "Aymara")
    (az . "Azerbaijani")
    (ba . "Bashkir")
    (be . "Byelorussian")
    (bg . "Bulgarian")
    (bh . "Bihari")
    (bi . "Bislama")
    (bn . "Bengali")
    (bo . "Tibetan")
    (br . "Breton")
    (ca . "Catalan")
    (co . "Corsican")
    (cs . "Czech")
    (cy . "Welsh")
    (da . "Danish")
    (de . "German")
    (dz . "Bhutani")
    (el . "Greek")
    (en . "English")
    (eo . "Esperanto")
    (es . "Spanish")
    (et . "Estonian")
    (eu . "Basque")
    (fa . "Persian")
    (fi . "Finnish")
    (fj . "Fiji")
    (fo . "Faroese")
    (fr . "French")
    (fy . "Frisian")
    (ga . "Irish")
    (gd . "Scots Gaelic")
    (gl . "Galician")
    (gn . "Guarani")
    (gu . "Gujarati")
    (ha . "Hausa")
    (he . "Hebrew")
    (hi . "Hindi")
    (hr . "Croatian")
    (hu . "Hungarian")
    (hy . "Armenian")
    (ia . "Interlingua")
    (id . "Indonesian")
    (ie . "Interlingue")
    (ik . "Inupiak")
    (is . "Icelandic")
    (it . "Italian")
    (iu . "Inuktitut")
    (ja . "Japanese")
    (jw . "Javanese")
    (ka . "Georgian")
    (kk . "Kazakh")
    (kl . "Greenlandic")
    (km . "Cambodian")
    (kn . "Kannada")
    (ko . "Korean")
    (ks . "Kashmiri")
    (ku . "Kurdish")
    (ky . "Kirghiz")
    (la . "Latin")
    (ln . "Lingala")
    (lo . "Laotian")
    (lt . "Lithuanian")
    (lv . "Latvian Lettish")
    (mg . "Malagasy")
    (mi . "Maori")
    (mk . "Macedonian")
    (ml . "Malayalam")
    (mn . "Mongolian")
    (mo . "Moldavian")
    (mr . "Marathi")
    (ms . "Malay")
    (mt . "Maltese")
    (my . "Burmese")
    (na . "Nauru")
    (ne . "Nepali")
    (nl . "Dutch")
    (no . "Norwegian")
    (oc . "Occitan")
    (om . "Oromo")
    (or . "Oriya")
    (pa . "Pundjabi")
    (pl . "Polish")
    (ps . "Pashto")
    (pt . "Portuguese")
    (qu . "Quechua")
    (rm . "Rhaeto-Romance")
    (rn . "Kirundi")
    (ro . "Romanian")
    (ru . "Russian")
    (rw . "Kiyarwanda")
    (sa . "Sanskrit")
    (sd . "Sindhi")
    (sg . "Sangho")
    (sh . "Serbo-Croatian")
    (si . "Singhalese")
    (sk . "Slovak")
    (sl . "Slovenian")
    (sm . "Samoan")
    (sn . "Shona")
    (so . "Somali")
    (sq . "Albanian")
    (sr . "Serbian")
    (ss . "Siswati")
    (st . "Sesotho")
    (su . "Sudanese")
    (sv . "Swedish")
    (sw . "Swahili")
    (ta . "Tamil")
    (te . "Telugu")
    (tg . "Tajik")
    (th . "Thai")
    (ti . "Tigrinya")
    (tk . "Turkmen")
    (tl . "Tagalog")
    (tn . "Setswana")
    (to . "Tonga")
    (tr . "Turkish")
    (ts . "Tsonga")
    (tt . "Tatar")
    (tw . "Twi")
    (ug . "Uigur")
    (uk . "Ukrainian")
    (ur . "Urdu")
    (uz . "Uzbek")
    (vi . "Vietnamese")
    (vo . "Volapük")
    (wo . "Wolof")
    (xh . "Xhosa")
    (yi . "Yiddish")
    (yo . "Yorouba")
    (za . "Zhuang")
    (zh . "Chinese")
    (zu . "Zulu")
    (-- . "Unknown language (%#1s)")))

(lyskom-language-strings global lyskom-menu en
  '((lyskom    . "LysKOM")
    (read      . "Read")
    (dont-read . "Jump")
    (write     . "Write")
    (conference . "Conference")
    (other     . "Other")
    (person    . "User")
    (marks     . "Marks")
    (move      . "Go")
    (info      . "About")
    (send      . "Send message")
    (receivers . "Receivers")
    (commented . "Commented")
    (kom-edit-send . "Send")
    (kom-edit-send-anonymous . "Send anonymously")
    (kom-edit-quit . "Throw away") 
    (kom-ispell-message . "Check spelling")
    (kom-edit-add-recipient . "Add recipient")
    (kom-edit-add-copy . "Add carbon copy")
    (kom-edit-show-commented . "Review commented")
    (kom-edit-insert-commented . "Cite commented")
    (kom-edit-insert-buglist . "Paste commented bug list")
    (kom-edit-add-bcc . "Add blind carbon copy")
    (kom-edit-add-cross-reference . "Add cross reference")
    (kom-edit-add-no-comments . "Request no comments")
    (kom-edit-add-personal-comments . "Request personal replies")
    (kom-edit-add-read-confirm-request . "Request read confirmation")
    (kom-edit-move-text . "Move to new recipient")
))

(lyskom-language-var local lyskom-move-tree-actions en
                     '(("Move text or subtract recipient" . move)
                       ("Leave text and continue with comments" . skip)
                       ("Abort this command" . quit)
                       ("Skip this command and its comment tree" . jump)))

(lyskom-language-var local lyskom-onoff-table en
  '(("on" . on) ("off" . off)))

(lyskom-language-var local lyskom-filter-predicate-list en
      '(("=" . nil) ("!=" . t)))

(lyskom-language-var local lyskom-filter-what en
      '((author . "Author")
        (author-no . "Author (number)")
        (author-re . "Author (regexp)")
        (subject . "Subject")
        (subject-re . "Subject (regexp)")
        (recipient . "Recipient")
        (recipient-no . "Recipient (number)")
        (recipient-re . "Recipient (regexp)")
        (text . "Contents")
        (text . "Contents (regexp)")))

(lyskom-language-var local lyskom-filter-actions en
      '((skip-text . "Skip")
        (dontshow . "Don't show")
        (read . "Exclude from filter")
        (skip-tree . "Skip comments")
	(obliterate . "Obliterate")))

(defconst lyskom-keybindings-missing nil)

(defvar lyskom-en-mode-map nil)
(lyskom-language-keymap lyskom-mode-map en lyskom-en-mode-map)

(defvar lyskom-en-review-prefix)
(defvar lyskom-en-change-prefix)
(defvar lyskom-en-next-prefix)
(defvar lyskom-en-list-prefix)
(defvar lyskom-en-filter-get-prefix)
(defvar lyskom-en-S-prefix)
(defvar lyskom-en-who-prefix)
(defvar lyskom-en-A-prefix)
(defvar lyskom-en-B-prefix)
(defvar lyskom-en-unread-prefix)

(if lyskom-en-mode-map
    nil
  (setq lyskom-en-mode-map (make-keymap))
  (suppress-keymap lyskom-en-mode-map)
  (define-prefix-command 'lyskom-en-review-prefix)
  (define-prefix-command 'lyskom-en-change-prefix)
  (define-prefix-command 'lyskom-en-next-prefix)
  (define-prefix-command 'lyskom-en-list-prefix)
  (define-prefix-command 'lyskom-en-filter-get-prefix)
  (define-prefix-command 'lyskom-en-S-prefix)
  (define-prefix-command 'lyskom-en-who-prefix)
  (define-prefix-command 'lyskom-en-A-prefix)
  (define-prefix-command 'lyskom-en-B-prefix)
  (define-prefix-command 'lyskom-en-unread-prefix)

  (define-key lyskom-en-mode-map (kbd "h") 'lyskom-en-change-prefix)
  (define-key lyskom-en-mode-map (kbd "r") 'lyskom-en-review-prefix)
  (define-key lyskom-en-mode-map (kbd "f") 'lyskom-en-filter-get-prefix)
  (define-key lyskom-en-mode-map (kbd "n") 'lyskom-en-next-prefix)
  (define-key lyskom-en-mode-map (kbd "l") 'lyskom-en-list-prefix)
  (define-key lyskom-en-mode-map (kbd "s") 'lyskom-en-S-prefix)
  (define-key lyskom-en-mode-map (kbd "w") 'lyskom-en-who-prefix)
  (define-key lyskom-en-mode-map (kbd "a") 'lyskom-en-A-prefix)
  (define-key lyskom-en-mode-map (kbd "b") 'lyskom-en-B-prefix)
  (define-key lyskom-en-mode-map (kbd "u") 'lyskom-en-unread-prefix)

  (define-key lyskom-en-mode-map (kbd "M-m") 'kom-toggle-mark-as-read-prefix)

  (define-key lyskom-en-mode-map (kbd (lyskom-keys 'button2)) 'kom-button-click)
  (define-key lyskom-en-mode-map (kbd (lyskom-keys 'button3)) 'kom-popup-menu)
  (define-key lyskom-en-mode-map (kbd (lyskom-keys 'button2up)) 'kom-mouse-null)
  (define-key lyskom-en-mode-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lyskom-en-mode-map (kbd "*") 'kom-button-press)
  (define-key lyskom-en-mode-map (kbd "=") 'kom-menu-button-press)
  (define-key lyskom-en-mode-map (kbd "TAB") 'kom-next-link)
  (define-key lyskom-en-mode-map (kbd "M-TAB") 'kom-previous-link)
  (define-key lyskom-en-mode-map (kbd "<S-tab>") 'kom-previous-link)

  ;; These should be first in order to be last in the menu of alternatives.
  (define-key lyskom-en-review-prefix     (kbd "?") 'lyskom-help)
  (define-key lyskom-en-unread-prefix     (kbd "?") 'lyskom-help)
  (define-key lyskom-en-change-prefix     (kbd "?") 'lyskom-help)
  (define-key lyskom-en-next-prefix       (kbd "?") 'lyskom-help)
  (define-key lyskom-en-list-prefix       (kbd "?") 'lyskom-help)
  (define-key lyskom-en-filter-get-prefix (kbd "?") 'lyskom-help)
  (define-key lyskom-en-S-prefix          (kbd "?") 'lyskom-help)
  (define-key lyskom-en-who-prefix        (kbd "?") 'lyskom-help)
  (define-key lyskom-en-A-prefix          (kbd "?") 'lyskom-help)
  
  (define-key lyskom-en-mode-map (kbd "o")     'kom-set-unread)
  (define-key lyskom-en-mode-map (kbd "x")     'kom-extended-command)
  (define-key lyskom-en-mode-map (kbd "<SPC>") 'kom-next-command)
  (define-key lyskom-en-mode-map (kbd "<LFD>")  'kom-page-next-command)
  (define-key lyskom-en-mode-map (kbd "<RET>")  'kom-line-next-command)

  (define-key lyskom-en-mode-map (kbd "?")  'kom-help)
  (define-key lyskom-en-mode-map (kbd "e")  'kom-send-letter)
  (define-key lyskom-en-mode-map (kbd "g")  'kom-go-to-conf)
  (define-key lyskom-en-mode-map (kbd "t")  'kom-write-text)
  (define-key lyskom-en-mode-map (kbd "c")  'kom-write-comment)
  (define-key lyskom-en-mode-map (kbd "C")  'kom-comment-previous)
  (define-key lyskom-en-mode-map (kbd "F")  'kom-write-footnote)
  (define-key lyskom-en-mode-map (kbd "p")  'kom-private-answer)
  (define-key lyskom-en-mode-map (kbd "P")  'kom-private-answer-previous)
  (define-key lyskom-en-mode-map (kbd "j")  'kom-jump)
  (define-key lyskom-en-mode-map (kbd "J")  'kom-super-jump)
  (define-key lyskom-en-mode-map (kbd "I")  'kom-who-am-i)
  (define-key lyskom-en-mode-map (kbd "W")  'kom-busy-wait)
  (define-key lyskom-en-mode-map (kbd "m")  'kom-add-self)
  (define-key lyskom-en-mode-map (kbd "M")  'kom-mark-text)
  (define-key lyskom-en-mode-map (kbd "U")  'kom-unmark-text)
  (define-key lyskom-en-mode-map (kbd "q")  'kom-quit)
  (define-key lyskom-en-mode-map (kbd "z")  'kom-bury)
  (define-key lyskom-en-mode-map (kbd "R")  'kom-recover)
  (define-key lyskom-en-mode-map (kbd "T")  'kom-display-time)

  (define-key lyskom-en-list-prefix (kbd "m") 'kom-list-marks)
  (define-key lyskom-en-list-prefix (kbd "c") 'kom-list-conferences)
  (define-key lyskom-en-list-prefix (kbd "n") 'kom-list-news)
  (define-key lyskom-en-list-prefix (kbd "u") 'kom-list-persons)
  (define-key lyskom-en-list-prefix (kbd "r") 'kom-list-re)
  (define-key lyskom-en-list-prefix (kbd "M") 'kom-membership)
  (define-key lyskom-en-list-prefix (kbd "s") 'kom-list-summary)
  (define-key lyskom-en-list-prefix (kbd "f") 'kom-list-filters)

  (define-key lyskom-en-next-prefix (kbd "t") 'kom-view-next-new-text)
  (define-key lyskom-en-next-prefix (kbd "c") 'kom-go-to-next-conf)
  (define-key lyskom-en-next-prefix (kbd "l") 'kom-next-kom)
  (define-key lyskom-en-next-prefix (kbd "u") 'kom-next-unread-kom)

  (define-key lyskom-en-filter-get-prefix (kbd "p") 'kom-get-appreciation)
  (define-key lyskom-en-filter-get-prefix (kbd "k") 'kom-get-abuse)
  (define-key lyskom-en-filter-get-prefix (kbd "s") 'kom-filter-subject)
  (define-key lyskom-en-filter-get-prefix (kbd "a") 'kom-filter-author)
  (define-key lyskom-en-filter-get-prefix (kbd "c") 'kom-filter-text)
  (define-key lyskom-en-filter-get-prefix (kbd "r") 'kom-filter-recipient)

  (define-key lyskom-en-who-prefix (kbd "w") 'kom-who-is-on)
  (define-key lyskom-en-who-prefix (kbd "c") 'kom-who-is-on-in-conference)
  (define-key lyskom-en-who-prefix (kbd "p") 'kom-who-is-present-in-conference)
  (define-key lyskom-en-who-prefix (kbd "f") 'kom-who-is-on-and-friend)

  (define-key lyskom-en-change-prefix (kbd "p") 'kom-change-presentation)
  (define-key lyskom-en-change-prefix (kbd "q") 'kom-change-conf-faq)
  (define-key lyskom-en-change-prefix (kbd "f") 'kom-filter-edit)
  (define-key lyskom-en-change-prefix (kbd "m") 'kom-change-auto-reply)
  (define-key lyskom-en-change-prefix (kbd "t") 'kom-move-text)
  (define-key lyskom-en-change-prefix (kbd "T") 'kom-move-text-tree)

  (define-key lyskom-en-unread-prefix (kbd "SPC") 'kom-mark-unread)
  (define-key lyskom-en-unread-prefix (kbd "0") 'kom-initial-digit-unread)
  (define-key lyskom-en-unread-prefix (kbd "1") 'kom-initial-digit-unread)
  (define-key lyskom-en-unread-prefix (kbd "2") 'kom-initial-digit-unread)
  (define-key lyskom-en-unread-prefix (kbd "3") 'kom-initial-digit-unread)
  (define-key lyskom-en-unread-prefix (kbd "4") 'kom-initial-digit-unread)
  (define-key lyskom-en-unread-prefix (kbd "5") 'kom-initial-digit-unread)
  (define-key lyskom-en-unread-prefix (kbd "6") 'kom-initial-digit-unread)
  (define-key lyskom-en-unread-prefix (kbd "7") 'kom-initial-digit-unread)
  (define-key lyskom-en-unread-prefix (kbd "8") 'kom-initial-digit-unread)
  (define-key lyskom-en-unread-prefix (kbd "9") 'kom-initial-digit-unread)
  (define-key lyskom-en-unread-prefix (kbd "c") 'kom-unread-commented-text)
  (define-key lyskom-en-unread-prefix (kbd "C") 'kom-unread-previous-commented-text)
  (define-key lyskom-en-unread-prefix (kbd "a ?") 'lyskom-help)
  (define-key lyskom-en-unread-prefix (kbd "a c") 'kom-unread-comments)
  (define-key lyskom-en-unread-prefix (kbd "a r") 'kom-unread-tree)
  (define-key lyskom-en-unread-prefix (kbd "g") 'kom-unread-last-normally-read)
  (define-key lyskom-en-unread-prefix (kbd "o") 'kom-unread-root)
  (define-key lyskom-en-unread-prefix (kbd "l") 'kom-unread-by-to)
  (define-key lyskom-en-unread-prefix (kbd "M") 'kom-unread-more)
  (define-key lyskom-en-unread-prefix (kbd "r") 'kom-unread-root-review)
  (define-key lyskom-en-unread-prefix (kbd "f") 'kom-unread-first)
  (define-key lyskom-en-unread-prefix (kbd "A") 'kom-unread-all)
  (define-key lyskom-en-unread-prefix (kbd "f") 'kom-unread-first)
  (define-key lyskom-en-unread-prefix (kbd "p") 'kom-unread-presentation)
  (define-key lyskom-en-unread-prefix (kbd "q") 'kom-unread-faq)
  (define-key lyskom-en-unread-prefix (kbd "Q") 'kom-unread-server-faq)
  (define-key lyskom-en-unread-prefix (kbd "m") 'kom-unread-marked-texts)
  (define-key lyskom-en-unread-prefix (kbd "a m") 'kom-unread-all-marked-texts)
  (define-key lyskom-en-unread-prefix (kbd "a SPC") 'kom-unread-all)

  (define-key lyskom-en-review-prefix (kbd "SPC") 'kom-view)
  (define-key lyskom-en-review-prefix (kbd "0") 'kom-initial-digit-view)
  (define-key lyskom-en-review-prefix (kbd "1") 'kom-initial-digit-view)
  (define-key lyskom-en-review-prefix (kbd "2") 'kom-initial-digit-view)
  (define-key lyskom-en-review-prefix (kbd "3") 'kom-initial-digit-view)
  (define-key lyskom-en-review-prefix (kbd "4") 'kom-initial-digit-view)
  (define-key lyskom-en-review-prefix (kbd "5") 'kom-initial-digit-view)
  (define-key lyskom-en-review-prefix (kbd "6") 'kom-initial-digit-view)
  (define-key lyskom-en-review-prefix (kbd "7") 'kom-initial-digit-view)
  (define-key lyskom-en-review-prefix (kbd "8") 'kom-initial-digit-view)
  (define-key lyskom-en-review-prefix (kbd "9") 'kom-initial-digit-view)
  (define-key lyskom-en-review-prefix (kbd "b") 'kom-review-backward)
  (define-key lyskom-en-review-prefix (kbd "c") 'kom-view-commented-text)
  (define-key lyskom-en-review-prefix (kbd "C") 'kom-view-previous-commented-text)
  (define-key lyskom-en-review-prefix (kbd "a ?") 'lyskom-help)
  (define-key lyskom-en-review-prefix (kbd "a c") 'kom-review-comments)
  (define-key lyskom-en-review-prefix (kbd "a r") 'kom-review-tree)
  (define-key lyskom-en-review-prefix (kbd "j") 'kom-review-clear)
  (define-key lyskom-en-review-prefix (kbd "n") 'kom-review-next)
  (define-key lyskom-en-review-prefix (kbd "u") 'kom-review-noconversion)
  (define-key lyskom-en-review-prefix (kbd "v") 'kom-review-converted)
  (define-key lyskom-en-review-prefix (kbd "o") 'kom-find-root)
  (define-key lyskom-en-review-prefix (kbd "l") 'kom-review-by-to)
  (define-key lyskom-en-review-prefix (kbd "f") 'kom-review-first)
  (define-key lyskom-en-review-prefix (kbd "A") 'kom-review-all)
  (define-key lyskom-en-review-prefix (kbd "M") 'kom-review-more)
  (define-key lyskom-en-review-prefix (kbd "g") 'kom-review-last-normally-read)
  (define-key lyskom-en-review-prefix (kbd "q") 'kom-review-faq)
  (define-key lyskom-en-review-prefix (kbd "Q") 'kom-review-server-faq)
  (define-key lyskom-en-review-prefix (kbd "s") 'kom-review-stack)
  (define-key lyskom-en-review-prefix (kbd "p") 'kom-review-presentation)
  (define-key lyskom-en-review-prefix (kbd "r") 'kom-find-root-review)
  (define-key lyskom-en-review-prefix (kbd "m") 'kom-review-marked-texts)
  (define-key lyskom-en-review-prefix (kbd "a m") 'kom-review-all-marked-texts)
  (define-key lyskom-en-review-prefix (kbd "a SPC") 'kom-review-all)
  (define-key lyskom-en-review-prefix (kbd "x a") 'kom-agree)
  (define-key lyskom-en-review-prefix (kbd "x q") 'kom-fast-reply)
  (define-key lyskom-en-review-prefix (kbd "h") 'kom-review-mail-headers)

  (define-key lyskom-en-S-prefix (kbd "c") 'kom-status-conf)
  (define-key lyskom-en-S-prefix (kbd "p") 'kom-status-person)
  (define-key lyskom-en-S-prefix (kbd "s") 'kom-status-session)
  (define-key lyskom-en-S-prefix (kbd "k") 'kom-status-server)
  (define-key lyskom-en-S-prefix (kbd "m") 'kom-send-message)
  (define-key lyskom-en-S-prefix (kbd "u m") 'kom-sub-recipient)
  (define-key lyskom-en-S-prefix (kbd "u k") 'kom-sub-comment)
  (define-key lyskom-en-S-prefix (kbd "u q") 'kom-del-faq)
  (define-key lyskom-en-S-prefix (kbd "u f") 'kom-sub-footnote)
  (define-key lyskom-en-S-prefix (kbd "u ?") 'lyskom-help)

  (define-key lyskom-en-A-prefix (kbd "?") 'lyskom-help)
  (define-key lyskom-en-A-prefix (kbd "m") 'kom-add-recipient)
  (define-key lyskom-en-A-prefix (kbd "e") 'kom-add-copy)
  (define-key lyskom-en-A-prefix (kbd "d") 'kom-add-bcc)
  (define-key lyskom-en-A-prefix (kbd "k") 'kom-add-comment)
  (define-key lyskom-en-A-prefix (kbd "r") 'kom-add-cross-reference)
  (define-key lyskom-en-A-prefix (kbd "q") 'kom-add-faq)
  (define-key lyskom-en-A-prefix (kbd "f") 'kom-add-footnote)
  (define-key lyskom-en-A-prefix (kbd "Q") 'kom-add-server-faq)

  (define-key lyskom-en-B-prefix (kbd "l") 'kom-previous-kom)


  ;; Running in) buffer

  (define-key lyskom-en-mode-map (kbd "M-p") 'backward-text)
  (define-key lyskom-en-mode-map (kbd "M-n") 'forward-text)
  (define-key lyskom-en-mode-map (kbd "C-M-p") 'kom-prev-prompt)
  (define-key lyskom-en-mode-map (kbd "C-M-n") 'kom-next-prompt)
  (define-key lyskom-en-mode-map (kbd "s a") 'kom-save-text)

  (define-key lyskom-en-mode-map (kbd "C-?") 'scroll-down)
  (define-key lyskom-en-mode-map (kbd "<DEL>") 'scroll-down)
  (define-key lyskom-en-mode-map (kbd "<delete>") 'scroll-down)
  (define-key lyskom-en-mode-map (kbd "<BS>") 'scroll-down)
  )


;;;==============================================================
;;; Keymap for filter editing
;;;

(defvar lyskom-en-filter-edit-map nil)
(lyskom-language-keymap lyskom-filter-edit-map en lyskom-en-filter-edit-map)

(if lyskom-en-filter-edit-map ()
  (setq lyskom-en-filter-edit-map (make-keymap))
  (suppress-keymap lyskom-en-filter-edit-map)
  (define-prefix-command 'lyskom-en-filter-edit-prefix)
  (define-key lyskom-en-filter-edit-map (kbd "C-c")  'lyskom-en-filter-edit-prefix)
  (define-key lyskom-en-filter-edit-map (kbd "p") 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-en-filter-edit-map (kbd "P") 'lyskom-filter-edit-prev-entry)
  (define-key lyskom-en-filter-edit-map (kbd "n") 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-en-filter-edit-map (kbd "N") 'lyskom-filter-edit-next-entry)
  (define-key lyskom-en-filter-edit-map (kbd "C-p") 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-en-filter-edit-map (kbd "C-n") 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-en-filter-edit-map (kbd "C-b") 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-en-filter-edit-map (kbd "C-f") 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-en-filter-edit-map (kbd "M-p") 'lyskom-filter-edit-prev-entry)
  (define-key lyskom-en-filter-edit-map (kbd "M-n") 'lyskom-filter-edit-next-entry)
  (define-key lyskom-en-filter-edit-map (kbd "d") 'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-en-filter-edit-map (kbd "M-d") 'lyskom-filter-edit-delete-entry)
  (define-key lyskom-en-filter-edit-map (kbd "D") 'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-en-filter-edit-map (kbd "C-d") 'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-en-filter-edit-map (kbd "i") 'lyskom-filter-edit-insert-pattern)
  (define-key lyskom-en-filter-edit-map (kbd "I") 'lyskom-filter-edit-insert-pattern)
  (define-key lyskom-en-filter-edit-map (kbd "M-i") 'lyskom-filter-edit-insert-entry)
  (define-key lyskom-en-filter-edit-map (kbd "<") 'lyskom-filter-edit-beginning-of-list)
  (define-key lyskom-en-filter-edit-map (kbd ">") 'lyskom-filter-edit-end-of-list)
  (define-key lyskom-en-filter-edit-map (kbd "M-<") 'lyskom-filter-edit-beginning-of-list)
  (define-key lyskom-en-filter-edit-map (kbd "M->") 'lyskom-filter-edit-end-of-list)
  (define-key lyskom-en-filter-edit-map (kbd "q") 'lyskom-filter-edit-quit)
  (define-key lyskom-en-filter-edit-map (kbd "C-c C-c")   'lyskom-filter-edit-quit)
  (define-key lyskom-en-filter-edit-map (kbd "x") 'lyskom-filter-edit-expunge)
  (define-key lyskom-en-filter-edit-map (kbd "s") 'lyskom-filter-edit-save)
  (define-key lyskom-en-filter-edit-map (kbd "g") 'lyskom-filter-edit-revert)
  (define-key lyskom-en-filter-edit-map (kbd "t") 'lyskom-filter-edit-toggle-permanent)
  (define-key lyskom-en-filter-edit-map (kbd "a") 'lyskom-filter-edit-toggle-action)
  (define-key lyskom-en-filter-edit-map (kbd "?") 'lyskom-filter-edit-brief-help)
  (define-key lyskom-en-filter-edit-map (kbd "h") 'lyskom-filter-edit-brief-help)
  )


(defvar lyskom-en-prioritize-mode-map nil)
(lyskom-language-keymap lyskom-prioritize-mode-map en lyskom-en-prioritize-mode-map)

(if lyskom-en-prioritize-mode-map 
    nil
  (setq lyskom-en-prioritize-mode-map (make-keymap))
  (suppress-keymap lyskom-en-prioritize-mode-map)
  (define-key lyskom-en-prioritize-mode-map (kbd (lyskom-keys 'button2)) 'kom-button-click)
  (define-key lyskom-en-prioritize-mode-map (kbd (lyskom-keys 'button3)) 'kom-popup-menu)
  (define-key lyskom-en-prioritize-mode-map (kbd (lyskom-keys 'button2up)) 'kom-mouse-null)
  (define-key lyskom-en-prioritize-mode-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lyskom-en-prioritize-mode-map (kbd "*") 'kom-button-press)
  (define-key lyskom-en-prioritize-mode-map (kbd "+") 'kom-menu-button-press)
  (define-key lyskom-en-prioritize-mode-map (kbd "?") 'kom-prioritize-help)
  (define-key lyskom-en-prioritize-mode-map (kbd "C-k") 'kom-prioritize-select)
  (define-key lyskom-en-prioritize-mode-map (kbd "C-y") 'kom-prioritize-yank)
  (define-key lyskom-en-prioritize-mode-map (kbd "SPC") 'kom-prioritize-select)
  (define-key lyskom-en-prioritize-mode-map (kbd "C-m") 'kom-prioritize-next-line)
  (define-key lyskom-en-prioritize-mode-map (kbd "C-j") 'kom-prioritize-next-line)
  (define-key lyskom-en-prioritize-mode-map (kbd "DEL") 'kom-prioritize-previous-line)
  (define-key lyskom-en-prioritize-mode-map (kbd "M-DEL") 'kom-prioritize-deselect-all)
  (define-key lyskom-en-prioritize-mode-map (kbd "<down>") 'kom-prioritize-next-line)
  (define-key lyskom-en-prioritize-mode-map (kbd "C-n") 'kom-prioritize-next-line)
  (define-key lyskom-en-prioritize-mode-map (kbd "<up>") 'kom-prioritize-previous-line)
  (define-key lyskom-en-prioritize-mode-map (kbd "C-p") 'kom-prioritize-previous-line)
  (define-key lyskom-en-prioritize-mode-map (kbd "p") 'kom-prioritize-previous-line)
  (define-key lyskom-en-prioritize-mode-map (kbd "M-<up>") 'kom-prioritize-move-up)
  (define-key lyskom-en-prioritize-mode-map (kbd "M-p") 'kom-prioritize-move-up)
  (define-key lyskom-en-prioritize-mode-map (kbd "u") 'kom-prioritize-move-up)
  (define-key lyskom-en-prioritize-mode-map (kbd "M-<down>") 'kom-prioritize-move-down)
  (define-key lyskom-en-prioritize-mode-map (kbd "M-n") 'kom-prioritize-move-down)
  (define-key lyskom-en-prioritize-mode-map (kbd "d") 'kom-prioritize-move-down)
  (define-key lyskom-en-prioritize-mode-map (kbd "M-<") 'kom-prioritize-beginning)
  (define-key lyskom-en-prioritize-mode-map (kbd "M->") 'kom-prioritize-end)
  (define-key lyskom-en-prioritize-mode-map (kbd "r") 'kom-prioritize-reprioritize)
  (define-key lyskom-en-prioritize-mode-map (kbd "g") 'kom-prioritize-goto-priority)
  (define-key lyskom-en-prioritize-mode-map (kbd "p") 'kom-prioritize-set-priority)
  (define-key lyskom-en-prioritize-mode-map (kbd "s") 'kom-prioritize-save)
  (define-key lyskom-en-prioritize-mode-map (kbd "q") 'kom-prioritize-quit)
  (define-key lyskom-en-prioritize-mode-map (kbd "C-c C-c") 'kom-prioritize-quit)
  (define-key lyskom-en-prioritize-mode-map (kbd "TAB") 'kom-next-link)
  (define-key lyskom-en-prioritize-mode-map (kbd "M-C-i") 'kom-previous-link)
  (define-key lyskom-en-prioritize-mode-map (kbd "M-TAB") 'kom-previous-link)
  (define-key lyskom-en-prioritize-mode-map (kbd "<S-tab>") 'kom-previous-link)
)

(lyskom-language-var local lyskom-prioritize-header-lines en 2)

(lyskom-language-var local lyskom-prioritize-header en 
" Prio   Conference
-----------------------------------------------------------------------------
")



;;;; ============================================================
;;;; Strings and things for the customize mode
;;;;

(defvar lyskom-en-customize-map nil)
(lyskom-language-keymap lyskom-customize-map en lyskom-en-customize-map)


(if lyskom-en-customize-map
    nil
  (setq lyskom-en-customize-map (make-sparse-keymap))
  (define-key lyskom-en-customize-map (kbd "TAB") 'widget-forward)
  (define-key lyskom-en-customize-map (kbd "M-TAB") 'widget-backward)
  (define-key lyskom-en-customize-map (kbd "<S-tab>") 'widget-backward)
  (define-key lyskom-en-customize-map (kbd "C-i") 'widget-forward)
  (define-key lyskom-en-customize-map (kbd "M-C-i") 'widget-backward)
  (define-key lyskom-en-customize-map (kbd "C-m") 'widget-button-press)
  (define-key lyskom-en-customize-map (kbd "C-j") 'widget-button-press)
  (define-key lyskom-en-customize-map (kbd "<RET>") 'widget-button-press)
  (define-key lyskom-en-customize-map (kbd "<LFD>") 'widget-button-press)
  (define-key lyskom-en-customize-map (kbd (lyskom-keys 'button2up)) 'kom-mouse-null)
  (define-key lyskom-en-customize-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lyskom-en-customize-map (kbd (lyskom-keys 'button2)) 'widget-button-click)
  (define-key lyskom-en-customize-map (kbd (lyskom-keys 'button3)) 'lyskom-widget-click)
  (define-key lyskom-en-customize-map (kbd "C-c C-c") 'lyskom-customize-save-and-quit)
  (define-key lyskom-en-customize-map (kbd "C-c C-k") 'lyskom-customize-quit)
  (define-key lyskom-en-customize-map (kbd "C-c C-s") 'lyskom-customize-save)
  (define-key lyskom-en-customize-map (kbd "C-c C-a") 'lyskom-customize-apply)
  (define-key lyskom-en-customize-map (kbd "?") 'lyskom-customize-help)
)

(lyskom-language-strings local lyskom-custom-strings en
  '(
    ;;
    ;; Widget strings
    ;;

    (which-person . "Specify person: ")
    (which-conf . "Specify a conference: ")
    (which-conf-or-person . "Specify a person or conference: ")
    (which-name . "Specify a name: ")
    (some-person . "Person %#1d")
    (invalid-value . "Invalid value (%#1S)")
    (unknown-command . "Unknown command (%#1s)")
    (ding . "Audio cue")

    ;;
    ;; Help messages
    ;;

    (variable-type-help . "Select to save the setting in the server.")
    (default-help-echo . "Change the value of  %#1s.")
    (change-this-name . "Change the name in this entry.")
    (show-doc . "Show documentation.")
    (hide-doc . "Hide documentation.")
    (select-command . "Select a command.")
    (select-what-to-execute . "\
Select whether to execute command or keyboard macro.")
    (select-url-viewer . "Select a web browser.")
    (select-number . "Select number of times.")
    (select-audio-file . "Select an audio file.")
    (select-priority . "Select a priority.")
    (select-buffer . "Select a buffer name.")
    (select-buffer-size . "Select maximum buffer size.")

    ;;
    ;; Strings that are used in types and so forth
    ;;

    (buffer-name . "%#1s-configure")

    (other-window .     "Some other window             ")
    (other-frame .      "Some other frame              ")
    (new-frame .        "In a new frame                ")
    (lyskom-window .    "The LysKOM buffer's window    ")
    (window-on-buffer . "A window displaying the buffer")

    (on . "On ")
    (off . "Off")
    (yes . "Yes")
    (no . "No ")
    (to-file . "To file:")
    (max-text-length . "For texts shorter than: ")

    (friends . "Friends and other special people")

    (turned-off .      "Turned off           ")
    (number-of-times . "A few times")
    (specific-spec . "Per recipient/sender")
    (sound-file . "Audio file")
    (other-persons . "All others")

    (selected-mark . "Mark          ")
    (conf-or-person . "Person or conference")
    (ask .           "Ask every time         ")
    (limited-number . "A limited number")
    (unlimited-number . "Unlimited")

    (before . "Before the text")
    (after . "After the text")
    (depth-first . "In comment order")
    (time-order . "In time order")

    (ar-message-type . "Message type")
    (ar-personal     . "Personal")
    (ar-group        . "Group")
    (ar-alarm        . "Alarm")
    (ar-any-type     . "Any message type")
    (ar-sender       . "Sender")
    (ar-any-sender   . "Any sender")
    (ar-specified-sender . "Specified senders")
    (ar-pers-or-conf . "Person or conference")
    (ar-recipient    . "Recipient")
    (ar-any-recipient . "Any recipient")
    (ar-specified-recipient . "Specified recipients")
    (ar-person       . "Person")
    (ar-message-text . "Message text")
    (ar-any-message  . "Any message")
    (ar-matching-regexp . "Matching regexp")
    (ar-reply        . "Reply")
    (ar-no-reply     . "No reply")
    (ar-reply-text   . "Reply text")

    (express-break . "Immediately upon creation  ")
    (break .         "After current comment chain")
    (no-break .      "After current conference   ")

    (express-server-break . "Immediately")
    (express-server-letters-break . "Immediately if letters arrive")
    (server-break . "After current comment chain")
    (letters-server-break . "After current comment chain if letters arrive")
    (after-conf-server-break . "After current conference")
    (after-conf-letters-server-break . "After current conference if letters arrive")
    (when-done-server-break . "After everything has been read")
    (no-server-break . "Never")

    (command . "Command")
    (command-list . "Command list")
    (some-persons . "For some persons")
    (name . "Name")

    (page-none . "Never                        ")
    (page-all .  "Before every command         ")
    (page-some . "Before the following commands")

    (ask-every-time . "Ask every time         ")
    (fixed-priority . "Fixed priority")

    (messages-in-lyskom-buffer . "In the LysKOM buffer")
    (discard-messages .          "Nowhere - discard them")
    (in-named-buffer .           "In a named buffer:")

    (everybody-rcpt . "Everybody                                ")
    (group-rcpt .     "The recipient of the last group message  ")
    (sender-rcpt .    "The sender of the most recent message    ")
    (last-recipient-recpt . "The last person I sent a message to")

    (viewer-program . "Web browser")
    (no-viewer            . "(ingenting valt)")
    (default-viewer       . "Browse-URL (all)")
    (netscape-viewer . "Netscape/Mozilla (all)")
    (windows-viewer       . "Windows default or Netscape/Mozilla (all)")
    (emacs-w3-viewer . "Emacs W3 mode (HTTP, Goper, FTP)")
    (emacs-general-viewer . "Emacs (FTP, Telnet, Mail)")
    (emacs-dired-viewer . "Emacs Dired (FTP)")
    (emacs-mail-viewer . "Emacs Mail mode (Mail)")
    (emacs-telnet-viewer . "Emacs Telnet mode (telnet)")
    (mosaic-viewer . "NCSA Mosaic (all)")
    (lynx-viewer          . "Lynx (all)")
    (galeon-viewer        . "Galeon (all)")

    (dont-check . "Don't ask for confirmation")
    (check-before-open . "Confirm before writing the text")
    (check-before-send . "Confirm before sending the text")
    (no-size-limit . "No limit")
    (max-size-in-bytes . "Fixed limit (in bytes)")
    (execute . "Execute")
    (kbd-macro . "Keyboard macro")
    (command . "Command")
    (enter-kbd-macro . "Enter a keybard macro. Finish with %#1s")
    (long-format .  "Show help texts")
    (short-format . "Hide help texts")

    (truncate-threshold-lines . "Max lines")

    (first . "First")
    (last . "Last")
    (specific-placement . "Specific location")
    (priority . "Priority")
    (same-as-conf . "Same as current conference")
    (custom-format . "Custom format:")
    (default-format . "Standard format")
    (a-string . "A string:")
    (some-string . "A random string")
    (unspecified . "Unspecified")

    (symbolic-mark-association . "Association")
    (symbolic-mark-name . "Symbolic name: ")
    (mark-type-to-assoc . "Mark type to associate with: ")

    (prompt-for-text-no . "Prompt for text number")

    (all-others . "All others")
    (some-person . "Person")
    (format-html . "Format HTML:")

    (url-transform-regexp . "From (regexp):")
    (url-transform-newtext . " To:")

    (default-namedays . "Namedays for current language")
    (specific-namedays . "Specific name list")
    (several-name-lists . "Several name lists")

    (link-pattern-for . "Link patterns for")
    (all-conferences . "All recipients")
    (link-specific-rcpt . "Specific recipient")
    (link-pattern . "Link pattern")
    (link-replace . "Link substitution")
    (link-highlight-match . "Highlight group")
    (link-fold-case . "Case insensitive")

    (generated-face . "Automatic")
    (predefined-fonts . "Fonts intended for specific uses\n")
    (other-fonts . "Fonts with no specific uses\n")
    (other-face . "Other")

    (ext-simple-command . "Simple command")
    (ext-complex-command . "Command with arguments")
    (ext-complex-component . "Command/argument")

    (lynx-xterm . "Lynx in xterm")
    (lynx-emacs . "Lynx in emacs terminal-mode")

    (morons . "Morons and other \"special\" people")
    (friends . "Friends and other special people")
    (me . "Mig, myself and I")
    (i-am-supervisor . "Conferences i supervise")
    (has-no-presentation . "Conferences without presentations")
    (highlight-conferences . "Conferences:")
    (highlight-face . "Face:")
    (highlight-conflist . "Conference list")

    (yes-dont-pop . "Yes, but don't uniconify frames")

    ;;
    ;; Misc doc strings
    ;;

    (lyskom . "Configuration of LysKOM")
    (lyskom-doc . "\\[lyskom-customize-save-and-quit] to save and quit,
\\[lyskom-customize-save-and-quit] to save and quit,
\\[lyskom-customize-save] to save without quitting,
\\[lyskom-customize-quit] to quit without saving.
\\[widget-forward] moves to the next setting
\\[widget-button-press] changes the value

Documentation:  [?] Show documentation    [!] Hide documentation
Lists etc.   :  [INS] Add a line   [DEL] Remove a line   [*] Modify

If the box before the name of the setting is selected, the setting will
be saved in the server. Otherwise it will be saved in your .emacs.")




    (section . "------------------------------------------------------------------------------\n")
    (look-and-feel-misc . "Look and feel\n")
    (window-locations   . "Windows\n")
    (windows-where      . "How are windows created:\n")
    (reading .            "Reading\n")
    (writing .            "Writing\n")
    (urls .               "URL management\n")
    (personal-messages .  "Personal messages\n")
    (remote-control .     "Remote control of LysKOM\n")
    (hooks .              "Hook functions\n")

    (audio-cues .         "Audio cues\n")
    (audio-cues-when    . "Issue audio cues when:\n")
    (automatic-replies .  "Automatic replies\n")


    (audio-cues-doc . "\
  The following group of settings controls how LysKOM issues audio cues in
  various situations. The following options are available for each setting:

    Off
        No audio cue is issued

    A Few Times
        Emacs will beep one or more times. You have to specify how many
        times Emacs is to beep.

    Audio File
        Emacs will attempt to play the specified audio file. The program
        used to play the file is specified by another setting.")
    

    (sending-doc . "\
  The following settings turn on or off certain checks that can be performed
  before sending a text to the server. The checks are designed to keep
  you from doing something stupid.

    Confirm multiple recipients
        If a text or comment has more than one recipient, LysKOM 
        can ask which of the recipients are relevant. This can either
        be done before you start writing the text, in which case 
        LysKOM will post a question for each recipient, or before sending
        the text to the server, in which case you may confirm all the
        recipients at once. It is also possible to turn this check off
        entirely.

    Check membership of commented author
        When on, LysKOM will check that the author of the comment you are
        writing is a member of at least one of the recipients of your comment.
        If not, LysKOM will offer to add the commented author as a recipient
        to the comment you are writing.

    Authors not to check
        A list of authors not to check even if check membership of
        commented author is on.

    Check for unread comments
        When this is on, LysKOM will check that you have read all the other
        comments to the text you are commenting before sending your
        comment to the server. This is supposed to help you avoid duplicating
        someone else's comment.")

    (windows-doc . "\
  The following settings control how windows are created in LysKOM. The
  available options are:


    Some other window
        In another window, but in the same frame as LysKOM. If there only
        is one window in the frame, a new window will be created (and will
        be removed when you are finished).

    Some other frame
        In a different frame than the one LysKOM is in. If there only is one
        frame, a new one will be created (and will be removed when you are
        finished).

    In a new frame
        A new frame is created (and will be removed when you are finished).

    The LysKOM buffer's window
        The LysKOM buffer's window will be used. LysKOM will be restored to
        the window when you're finished.

    A window displaying the buffer
        If there is a window anywhere displaying the named buffer, that 
        window will be used. This might be useful if you always have a
        particular buffer showing, but don't need it when executing some
        command.")


    ;;
    ;; Doc strings for variables
    ;;

    (kom-ansaphone-replies-doc . "\
  This setting controls automatic replies in detail. You can set replies
  for specific senders, recipients and even message contents. If the message
  being replied to does not match any element in this list, the default
  auto reply message above is sent instead.

  For each pattern in the list you can set the required message type, 
  sender the message must have, recipient the message must have, a regular
  expression the message must match and the reply to send.

  It is advisable to configure the client not to send a message to any
  group or alarm message.")

    (kom-bury-buffers-doc . "\
  Controls how the LysKOM buffer is handled when moving to another KOM buffer
  with the Next and Previous LysKOM commands. When turned on the current
  buffer is buried.")

    (kom-personal-messages-in-window-doc . "\
  Controls which window is used to display the buffer with personal messages.
  For this setting to have any effect, personal messages must be displayed
  in a separate buffer, and that buffer must be displayed automatically
  every time a message is received.")

    (kom-write-texts-in-window-doc . "\
  Controls which window is used to write new texts.")

    (kom-prioritize-in-window-doc . "\
  Controls which window is used to prioritize conferences.")

    (kom-edit-filters-in-window-doc . "\
  Controls which window is used for editing filters.")

    (kom-customize-in-window-doc . "\
  Controls which window is used to configure LysKOM.")

    (kom-view-commented-in-window-doc . "\
  Controls which window is used to show the commented text when commenting.")

    (kom-list-membership-in-window-doc . "\
  Controls in which window your membership list is shown.")


    (kom-user-prompt-format-doc . "\
  The format of the LysKOM prompt. Certain control sequences cause special
  text to be inserted:

    %c - Inserts the current default command.
    %[ - Inserts `[' if the ansaphone is on.
    %] - Inserts `]' is the ansaphone is on.
    %m - Inserts information about recorded messages.
    %s - Inserts the name of the LysKOM system
    %S - Inserts the server name.
    %p - Inserts the name of the user currently logged on.
    %w - Inserts the name of the current conference.
    %a - Inserts `anonymous'
    %A - Inserts `Anonymous'
    %# - Inserts the current session number.
    %  - Inserts a space if it seems necessary.
    %% - Inserts a percent sign.

  Here are a few examples:

    \"%[%c% %m%] - \"             The default prompt
    \"%[%s: %c% %m%] - \"         Could display \"LysKOM: Time - \"")

    (kom-user-prompt-format-executing-doc . "\
  The format of the LysKOM prompt when the default command is executing.
  Certain control sequences cause special text to be inserted:

    %c - Inserts the current default command.
    %[ - Inserts `[' if the ansaphone is on.
    %] - Inserts `]' is the ansaphone is on.
    %m - Inserts information about recorded messages.
    %s - Inserts the name of the LysKOM system
    %S - Inserts the server name.
    %p - Inserts the name of the user currently logged on.
    %w - Inserts the name of the current conference.
    %a - Inserts `anonymous'
    %A - Inserts `Anonymous'
    %# - Inserts the current session number.
    %  - Inserts a space if it seems necessary.
    %% - Inserts a percent sign.

  Here are a few examples:

    \"%[%c% %m%] - \"             The default prompt
    \"%[%s: %c% %m%] - \"         Could display \"LysKOM: Time - \"")

    (kom-anonymous-prompt-format-doc . "\
  The format of the LysKOM prompt used when anonymous. Certain control
  sequences cause special text to be inserted:

    %c - Inserts the current default command.
    %[ - Inserts `[' if the ansaphone is on.
    %] - Inserts `]' is the ansaphone is on.
    %m - Inserts information about recorded messages.
    %s - Inserts the name of the LysKOM system
    %S - Inserts the server name.
    %p - Inserts the name of the user currently logged on.
    %w - Inserts the name of the current conference.
    %a - Inserts `anonymous'
    %A - Inserts `Anonymous'
    %# - Inserts the current session number.
    %  - Inserts a space if it seems necessary.
    %% - Inserts a percent sign.

  Here are a few examples:

    \"%[%c% %m%] - \"             The default prompt
    \"%[%s: %c% %m%] - \"         Could display \"LysKOM: Time - \"")

    (kom-anonymous-prompt-format-executing-doc . "\
  The format of the LysKOM prompt when the default command is executing
  and the session is anonymous. Certain control sequences cause special 
  text to be inserted:

    %c - Inserts the current default command.
    %[ - Inserts `[' if the ansaphone is on.
    %] - Inserts `]' is the ansaphone is on.
    %m - Inserts information about recorded messages.
    %s - Inserts the name of the LysKOM system
    %S - Inserts the server name.
    %p - Inserts the name of the user currently logged on.
    %w - Inserts the name of the current conference.
    %a - Inserts `anonymous'
    %A - Inserts `Anonymous'
    %# - Inserts the current session number.
    %  - Inserts a space if it seems necessary.
    %% - Inserts a percent sign.

  Here are a few examples:

    \"%[%c% %m%] - \"             The default prompt
    \"%[%s: %c% %m%] - \"         Could display \"LysKOM: Time - \"")

    (kom-enabled-prompt-format-doc . "\
  The format of the LysKOM prompt when in administrator mode. Certain
  control sequences cause special text to be inserted:

    %c - Inserts the current default command.
    %[ - Inserts `[' if the ansaphone is on.
    %] - Inserts `]' is the ansaphone is on.
    %m - Inserts information about recorded messages.
    %s - Inserts the name of the LysKOM system
    %S - Inserts the server name.
    %p - Inserts the name of the user currently logged on.
    %w - Inserts the name of the current conference.
    %a - Inserts `anonymous'
    %A - Inserts `Anonymous'
    %# - Inserts the current session number.
    %  - Inserts a space if it seems necessary.
    %% - Inserts a percent sign.

  Here are a few examples:

    \"%[%c% %m%] - \"             The default prompt
    \"%[%s: %c% %m%] - \"         Could display \"LysKOM: Time - \"")

    (kom-enabled-prompt-format-executing-doc . "\
  The format of the LysKOM prompt when the default command is executing
  and the session is in administrator mode. Certain control sequences 
  cause special text to be inserted:

    %c - Inserts the current default command.
    %[ - Inserts `[' if the ansaphone is on.
    %] - Inserts `]' is the ansaphone is on.
    %m - Inserts information about recorded messages.
    %s - Inserts the name of the LysKOM system
    %S - Inserts the server name.
    %p - Inserts the name of the user currently logged on.
    %w - Inserts the name of the current conference.
    %a - Inserts `anonymous'
    %A - Inserts `Anonymous'
    %# - Inserts the current session number.
    %  - Inserts a space if it seems necessary.
    %% - Inserts a percent sign.

  Here are a few examples:

    \"%[%c% %m%] - \"             The default prompt
    \"%[%s: %c% %m%] - \"         Could display \"LysKOM: Time - \"")
    
    (kom-cite-string-doc . "\
  A string that is inserted before each line in a cited text. Normally this
  is set to something like \"> \". Note that it is not customary to cite
  commented texts the way it is done in e-mail or Usenet News since the
  commented text is always available in LysKOM.")


    (kom-created-texts-are-read-doc . "\
  When this is on, self-created texts are automatically marked as read. Turned
  off, those texts are presented as any other texts.")

    (kom-default-mark-doc . "\
  The default mark type used for marking texts. A mark type is an integer
  between 0 and 255 (inclusive). If no default mark type is selected, LysKOM
  asks for a mark type every time a text is to be marked. This can be used
  to separate different types of marks. You can, for example, mark texts
  which contain valuable information with a certain integer and texts which
  you should remember to comment some time with another integer.")

    (kom-symbolic-marks-alist-doc . "\
  To avoid having to memorize what the integer mark types are supposed to
  mean, you can define a list of symbolic mark types. A symbolic mark type
  is a descriptive string that is associated with an integer mark type.")

    (kom-reading-puts-comments-in-pointers-last-doc . "\
  Controls if comment links are listed before or after the body of a text.
  Normally comment links are listed after the text.

  Before:

    398331 1996-09-24  13:22  /2 lines/ George Berkeley
    Recipient: Philosophy <1226>
    Comment in text 398374 by John Locke
    Subject: 
    ------------------------------------------------------------
    An abstract idea is a contradiction in terms.
    (398331) -----------------------------------

  After:

    398331 1996-09-24  13:22  /2 lines/ George Berkeley
    Recipient: Philosophy <1226>
    Subject: 
    ------------------------------------------------------------
    An abstract idea is a contradiction in terms.
    (398331) -----------------------------------
    Comment in text 398374 by John Locke
")

    (kom-dashed-lines-doc . "\
  When this setting is turned on, dashed lines are displayed before and after
  the text body. When turned off, blank lines are used instead.

  On:

    892343 1996-09-24  19:21  /2 lines/ Tycho Brahe
    Recipien: Presentation (of new) Members
    Subject: Tycho Brahe
    ------------------------------------------------------------
    Astronomer and discoverer of stars resident on the island of Ven.
    (892343) -----------------------------------

  Off:

    892343 1996-09-24  19:21  /2 lines/ Tycho Brahe
    Recipien: Presentation (of new) Members
    Subject: Tycho Brahe

    Astronomer and discoverer of stars resident on the island of Ven.
    (892343)

  Most people have this turned on.")

    (kom-autowrap-doc . "\
  With this setting turned on, LysKOM will attempt to fill any paragraphs
  containing lines that are wider than the screen. Paragraphs that appear
  to have been preformatted by the author are exempted from this treatment.")

    (kom-show-author-at-end-doc . "\
  When this is turned on, the name of the author will be shown at the end
  of the text. The name is also shown before the text as usual.

  On (with dashed lines on):

    892342 1996-09-24  19:21  /2 lines/ Claude Shannon
    Mottagare: Presentation (of new) Members
    Ärende: Claude Shannon
    ------------------------------------------------------------
    Information theoretician
    (892342) /Claude Shannon/------------------------------

  Off:

    892342 1996-09-24  19:21  /2 lines/ Claude Shannon
    Recipient: Presentation (of new) Members
    Subject: Claude Shannon
    ------------------------------------------------------------
    Information theoretician
    (892342) -----------------------------------

  If dashed lines are off, the author's name will be shown as in this
  example, but the dashed lines are naturally not displayed.")

    (kom-truncate-threshold-doc . "\
  If this is activated, texts longer than the maximum number of lines will
  be truncated when you review comments, review marked texts or review the
  first or last texts in a conference.  Texts will not be truncated when
  you review a text, review a tree or read the next text.")

    (kom-truncate-show-lines-doc . "\
  The number of lines to show of truncated texts.")

    (kom-print-number-of-unread-on-entrance-doc . "\
  Determines whether the number of unread texts is shown when entering
  a conference:

  On:

    Go to next conference...
    Presentation (of new) Members - 3 unread
    Read next text - 

  Off:

    Go to next conference...
    Presentation (of new) Members
    Read next text - ")



    (kom-presence-messages-in-echo-area-doc . "\
  If you want messages in the echo area when somebody logs in, logs out or
  changes name, turn this setting on. Messages are shown in the minibuffer.
  If you want messages about a limited number of users, specify which ones.
  To not get any messages at all, just specify an empty list.")

    (kom-presence-messages-in-buffer-doc . "\
  If you want messages in the LysKOM buffer when somebody logs in, logs out
  or changes name, turn this setting on. Messages are shown in the LysKOM
  buffer. If you want messages about a limited number of users, specify which 
  ones. To not get any messages at all, just specify an empty list.")

    (kom-show-where-and-what-doc . "\
  If this is on, the list of active users will include the machine and user
  they are logged in from (if known) and what they are currently doing.

  On:

        User                                   Is in conference             
        At                                     Activity                        
    --------------------------------------------------------------------------
     6810 George Berkeley                      Philosophy                     
          berkeley@emp1.tcd.ie                 (Writing a comment.)           
     7571 John Locke                           Philosophy                     
          eridy@cc.ox.ac.uk                    (Waiting.)                     

  Off:

        User                                   Is in conference             
    --------------------------------------------------------------------------
     6810 George Berkeley                      Philosophy                     
     7571 John Locke                           Philosophy")

    (kom-show-since-and-when-doc . "\
  If this is on, the list of active users will include the connection time
  and the inactivity period for each user.")

    (kom-idle-hide-doc . "\
  The listing of active users normally only shows those users who have been
  active recently. This setting determines how many minutes a user may be
  inactive without being excluded from the list of active users.")


    (kom-show-footnotes-immediately-doc . "\
  Footnotes can be shown either as comments or immediately when the text
  they are footnotes to is displayed. This setting controls which behavior
  is used.")


    (kom-follow-comments-outside-membership-doc . "\
  LysKOM will normally not follow chains of comments into conferences you are
  not a member of. If you do want to follow comment chains into other
  conferences, turn this setting on.")


    (kom-read-depth-first-doc . "\
  LysKOM can display texts either in the order they were written or in
  the order defined by the comment tree. For example, if texts 1003 and
  1004 are comments to text 1002, text 1006 is a comment to 1003 and 
  texts 1005 and 1007 are comments to 1004, the comment tree looks 
  something like this:
        
  1002 +-- 1003 --- 1006
       |
       +-- 1004 +-- 1005
                |
                +-- 1007

  Reading in order of creation will cause the texts to be displayed
  in numerical order: 1002, 1003, 1004, 1005, 1006 and finally 1007.
  Reading in comment order will give the order 1002, 1003, 1006, 1004,
  1005 and finally 1007.")


    (kom-continuous-scrolling-doc . "\
  Turned on means that LysKOM will scroll the buffer while new text is 
  being inserted, not just at the end of a command. This works well with
  faster terminals, but may be worth turning off if the terminal is so slow
  that scrolling Emacs buffers takes a long time.")


    (kom-deferred-printing-doc . "\
  In order to improve speed, LysKOM will not print certain things, such as
  the names of users and conferences immediately, but will delay printing
  to make time for other tasks. This improves response time in the client
  considerably, and should only be turned off if it causes problems.")


    (kom-higher-priority-breaks-doc . "\
  When texts are created in conferences that have a higher priority than
  the one currently being read, LysKOM will attempt to break the normal
  reading order to show these. This setting controls whether the reading
  order is broken immediately, after the current comment chain is read or
  when everything in the current conference has been read.")


    (kom-login-hook-doc . "\
  This hook lists commands to be run when logging in, before any input is
  accepted from the keyboard.")


    (kom-do-when-done-doc . "\
  This hook lists commands and keyboard macros that are to be executed when
  all texts have been read.")


    (kom-page-before-command-doc . "\
  The LysKOM buffer can be cleared before all commands, so text that is 
  inserted always appears at the top of the buffer's window. This variable
  controls before which commands the buffer is to be scrolled.")


    (kom-permissive-completion-doc . "\
  When this is on, TAB will only complete to the names of users that are 
  logged on when the command being invoked is only applicable to people
  that are logged on. When off, TAB will complete to names of everyone.")


    (kom-membership-default-priority-doc . "\
  This specifies the how the initial priority of a conference is set when
  you first become a member. If it is a number between 1 and 255, that is
  the priority assigned. If it is something else, LysKOM will ask for a 
  priority every time you become a member of a conference.")

    (kom-show-personal-messages-in-buffer-doc . "\
  This setting specifies how personal, group and public messages are shown.
  The messages can be displayed in the LysKOM buffer, simply thrown away or
  be shown in a named buffer.")

    (kom-pop-personal-messages-doc . "\
  If messages are shown in a named buffer and this setting is also on, then
  LysKOM will display that buffer whenever a message arrives.")

    (kom-audio-player-doc . "\
  If you want LysKOM to play audio files instead of simply beeping, this
  setting must specify the name of a program that can play the audio files.
  The program must take a single argument, the name of the file to play.")

    (kom-default-message-recipient-doc . "\
  This setting controls who will be the default recipient of messages. The
  default recipient may either be everyone, i.e. a public message; the
  sender of the most recently received message; the recipient of the most
  recently received group message; or the sender of the most recently
  received personal message.")

    (kom-filter-outgoing-messages-doc . "\
  If this is on, messages that are sent automatically, such as automatic
  replies and replies to remote control commands, will be shown as if you
  had sent them manually.")

    (kom-friends-doc . "\
  The users named in this list will be displayed using a special face in
  the LysKOM buffer. These users are also listed with the command Which 
  friends (and may be used in other similar commands).")

    (kom-morons-doc . "\
  The users named in this list will be displayed using a warning face in
  the LysKOM buffer.")

    (kom-url-viewer-preferences-doc . "\
  This setting controls which WWW browser that will be used to open URLs
  found in LysKOM. If the first browser in the list cannot handle the type
  of URL being opened, then the next browser is tried, and so on.")

    (kom-windows-browser-command-doc . "\
  This setting specifies the command to use to start a web browser
  under Windows. If empty, a couple of commands that are likely to
  work on Windows will be tried.")

    (kom-mosaic-command-doc . "\
  This setting specifies the command to use to start NCSA Mosaic.")

    (kom-netscape-command-doc . "\
  This setting specifies the command to use to start Netscape or Mozilla.")

    (kom-galeon-command-doc . "\
  This setting specifies the command to use to start Galeon.")

    (kom-inhibit-typeahead-doc . "\
  Key presses are usually buffered while LysKOM is busy, and are executed
  as soon as possible. With this setting off, LysKOM discards any key presses
  received while the client was busy.")

    (kom-max-buffer-size-doc . "\
  It is possible to limit the size of the LysKOM buffer by specifying a
  maximum number of characters in this setting. When the buffer grows
  beyond this limit, text from the beginning of the buffer is removed.")

    (kom-ansaphone-record-messages-doc . "\
  LysKOM can record messages that arrive when the autoreply feature is on.
  This setting controls whether messages are recorded or not.")

    (kom-ansaphone-show-messages-doc . "\
  When this setting is on, LysKOM will display incoming messages even if
  the autoreply feature is turned on.")

    (kom-ansaphone-default-reply-doc . "\
  This is the message sent by the autoreply feature unless a different
  message has been specified using some other means (and other means are
  only for careful experts).")

    (kom-remote-control-doc . "\
  When turned on, it is possible to control the session using remote control
  commands. Only those users listed below may issue the commands.")

    (kom-remote-controllers-doc . "\
  The users listed here are permitted to issue remove control commands
  to your LysKOM session.")

    (kom-self-control-doc . "\
  When this is on, the user who is logged on may issue remote control
  commands. This is an alternative to adding yourself to the list of 
  permitted controllers.")

    (kom-customize-format-doc . "\
  The documentation for the various settings can be visible or hidden when
  you open the settings buffer. No matter if it starts hidden or visible, the
  documentation for individual settings can be shown and hidden by using the
  question mark/exclamation mark to the right of the setting.")

    (kom-default-language-doc . "\
  Default language to use in LysKOM. If you change this setting the new
  language will not be applied to the current setting. Use the Change
  language command to do that.")

    (kom-ispell-dictionary-doc . "\
  This specifies the dictionary ispell is to use for spell checking. If set
  to ispell-dictionary, then the variable ispell-dictionary will be used
  instead.")

    (kom-show-namedays-doc . "\
  When this is on, display the name of the day. You can choose to display
  names from a list appropriate to the currently selected language. You
  can also choose a specific list of names.")

    (kom-show-week-number-doc . "\
  The Time command shows week numbers if this is on.")

    (kom-membership-default-placement-doc . "\
  This controls where new memberships are placed. First means before
  all existing memberships of the same priority. Last means after all
  existing memberships of the same priority. A number indicates a
  fixed placement (although the client will eventually sort the membership
  list in order of priority).")

    (kom-show-imported-importer-doc . "\
  The importer of an imported e-mail message is shown if this is on.
  This only works with compliant e-mail importers.")

    (kom-show-imported-envelope-sender-doc . "\
  The actual sender (the envelope sender) of imported e-mails is shown 
  if this is on. This only works with compliant e-mail importers.")

    (kom-show-imported-external-recipients-doc . "\
  Show external recipients and CC recipients of imported e-mails if this
  is on. This only works with compilant e-mail importers.")

    (kom-agree-text-doc . "\
  Default text for the Agree command. It can either be a text or a list
  of texts. If it is a list, one of the texts will be chosen at random.")

    (kom-silent-ansaphone-doc . "\
  If this is off, the client will not beep when a personal, group or
  public message is received when the auto reply feature is on.")

    (kom-default-session-priority-doc . "\
  The session priority of new sessions. Conferences with a lower priority
  will not be read.")

    (kom-unsubscribe-makes-passive-doc . "\
  When this is on, the first time you leave a conference you become a
  passive member. Leaving the conference again unsubscribes you completely.
  When this is off, leaving a conference unsubscribes you immediately.")

    (kom-review-priority-doc . "\
  Priority for review commands. This setting can be used to give review
  commands a higher priority than normal. The default priority is the
  same as the conference currently being read. Set this to 256 or higher
  if you want review commands to take precedence over all conferences.")

    (kom-show-creating-software-doc . "\
  Show the name of the client used to create a text if the information
  is present and this setting is on.")

    (kom-text-footer-format-doc . "\
  Format for the text footer. In the format string, %n is replaced by the
  text number, %P with the author's name, %p with the author's number, %f
  with information about the text (HTML, filled, etc), and %- with a
  suitable dashed line. A number after the percent sign is the minimum
  number of characters to use. A minus sign before the number causes text
  to be left justified within the field. An equals sign causes text
  longer than the indicated width to be truncated.")
 
    (kom-long-lines-doc . "\
  When this is on, most of the dashed lines are made longer than normal.")

    (kom-postpone-default-doc . "\
  Number of texts to postpone with the Postpone reading command.")

    (kom-allow-incompleteness-doc . "\
  When this is on, the client will not wait for information about all
  unread texts, even if it is needed. The result is that List news and
  some other commands do not yield accurate results shortly after logging
  in. When this is off, the client will wait for information on all
  unread texts when it is needed.")

    (kom-smileys-doc . "\
  When this is on, Emacs supports it, and the appropriate package 
  (smiley.el, which is part of Gnus) is installed, smileys will be
  shown graphically.")
  
    (kom-ignore-message-senders-doc . "\
  Don't show personal, group or alarm messages from these senders.")

    (kom-ignore-message-recipients-doc . "\
  Don't show group messages to these recipients.")

    (kom-text-footer-dash-length-doc . "\
  The total length of the text footer when long dashed lines are not
  in effect and no custom format is being used.")

    (kom-text-header-dash-length-doc . "\
  The total length of the text header when long dashed lines are not
  in effect.")

    (kom-show-personal-message-date-doc . "\
  When this is on, the date and time is shown on all personal, group and
  alarm messages.")

    (kom-w3-simplify-body-doc . "\
  When this is on, the client will ignore colors set in the HTML body when
  displaying formatted HTML.")

    (kom-mercial-doc . "\
  This text is shown in the list of users when you have finished reading
  everything.")

    (kom-server-priority-doc . "\
  Priority of this LysKOM session. The client can prompt you to go to
  a session with unread texts when a text arrives in a session with a
  higher priority than the one currently being read.")

    (kom-server-priority-breaks-doc . "\
   This setting controls when a prompt to go to the next LysKOM session
   with unread texts is presented.

   Immediately                  Present a prompt if any text arrives in
                                a session with a higher priority.

   Immediately if letters arrive    Present a prompt is a letter arrives
                                in a session with a higher priority.

   After current comment chain  Present a prompt after the current chain
                                of comments has been read if a text
                                arrives in a session with a higher 
                                priority.

   After current comment chain if letters arrive    Present a prompt
                                after the current chain of comments has
                                been read if a letter arrives in a session
                                with a higher priority.

   After current conference     Present a prompt after all texts in the
                                current conference have been read if a
                                text arrives in a session with a higher
                                priority.

   After current conference if letters arrive       Present a prompt 
                                after all texts in the current conference
                                have been read if a letter arrives in a 
                                session with a higher priority.

   After everything has been read   Present a prompt to go to the next
                                session with unreads after everything
                                has been read. The prompt is presented
                                regardless of session priorities.

   Never                        Never prompt to go to another LysKOM
                                session.")

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
   When this is on and you enter a text that can be either a conference
   number or a conference name, the client will accept it as a conference 
   number, if possible. When this is off, the text will be accepted as
   a conference name first and number second.")
    (kom-keep-alive-interval-doc . "\
  The number of seconds between calls to the server used to keep the
  network connection to the LysKOM server active. If your network connection
  shuts down after a period of inactivity, set this to approximately half
  that period and use the command `Keep connection alive'.")
    (kom-text-no-prompts-doc . "\
  Specifies which commands will always prompt for text numbers and which
  will not. Commands that are not listed here will use default behavior.")
    (kom-saved-file-name-doc . "\
  Specifies the default file to archive texts in. The client will always
  prompt for the file name, using this as the default.")
    (kom-follow-attachments-doc . "\
  Specifies whether to read imported e-mail attachments as regular
  comments. When off, imported attachments are marked as read when the
  e-mail they are attached to is read.")
    (kom-show-unread-in-frame-title-doc . "\
  When this is on, the indicator \"(Unread)\" is shown in the title
  bar of all windows whose active buffer belongs to a LysKOM session
  with unread texts. The title bar may not be updated unless the
  window is open.")
    (kom-created-texts-are-saved-doc . "\
  If this variable is set to a file name, all texts you write will be 
  saved to that file. Texts that could not be created are not saved.")
    (kom-confirm-add-recipients-doc . "\
  When this is on and you add a recipient to a text, the client will
  ask if comments to the text should also be sent to the new
  recipient. When this is off, comments will always be sent to the new
  recipient. For most people it is a good idea to leave this on.")

    (kom-trim-buffer-minimum-doc . "\
  If you have limited the LysKOM buffer size, this variable determines
  how much larger than the limit the buffer has to be before it is
  trimmed back to the maximum size. The variable must be an integer;
  the default is 4096.")

    (kom-dont-check-commented-authors-doc . "")
    (kom-print-relative-dates-doc . "\
  The time fields of texts (and a couple of other things) will be
  \"today\" or \"yesterday\" if this setting is on and the text was
  written today or yesterday, respectively. If it is off, absolute
  dates will always be shown.")
    (kom-print-seconds-in-time-strings-doc . "\
  If this setting is on, some time strings (for example the creation
  time of texts and sent messages) will include seconds.")
    (kom-review-uses-cache-doc . "\
  If this is turned on, commands that review texts will use saved copies
  of the text rather than get new copies from the server. This makes the
  commands run significantly faster, but in some cases the texts may 
  have changed since they were copied, and you will not see those 
  changes.")
    (kom-review-marks-texts-as-read-doc . "\
  If this is turned on, commands that review texts will also mark them
  as read. Otherwise, the review commands will leave your reading history
  untouched, as usual.")
    (kom-auto-review-faqs-doc . "\
  If this is turned on, FAQs that are not marked as read will be reviewed
  automatically when you log on or go to a conference with a new FAQ.")
    (kom-auto-list-faqs-doc . "\
  If this is turned on, new FAQs will be listed automatically when you
  log on or go to a conference with a new FAQ.")
    (kom-extended-status-information-doc . "\
  When this is on, additional information may be shown by commands that
  display person, conference and server status.")
    (kom-highlight-first-line-doc . "\
  When this is on, the first line (with text number, date and author) is
  shown with a different format than normal text.")
    (kom-highlight-dashed-lines-doc . "\
  When this is on, the lines before and after the text body is shown with
  a different format than normal text.")
    (kom-highlight-text-body-doc . "\
  When this is on, the text body is shown with a different format than
  normal text.")
    (kom-async-highlight-dashed-lines-doc . "\
  When this is on, the lines before and after personal, group and alarm
  messages are shown with a different format than normal text.")
    (kom-async-highlight-text-body-doc . "\
  When this is on, the body of personal, group and alarm messages are
  shown with a different format than normal text.")
    (kom-edit-hide-add-button-doc . "\
  When this is on, an [Add...] button will be shown after the list of
  recipients and auxiliary information when writing a new text.")
    (kom-format-html-authors-doc . "\
  This setting controls which authors' HTML messages that the client will
  attempt to format.")
    (kom-keyboard-menu-immediate-selection-doc . "\
  This setting controls whether keyboard shortcuts in text-based context
  menus require confirmation with return or not. With this on you have
  to confirm all selections with return.")
    (kom-max-overlays-doc . "\
  Use this setting to limit the number of overlays in the LysKOM buffer
  \(overlays are used to create the colored backgrounds for texts\). By
  limiting the number of overlays it is possible to speed up operations
  such as scrolling significantly. Each text requires 1-4 overlays for
  highlighting, depending on other settings.")
    (kom-mark-read-texts-as-read-in-new-recipient-doc . "\
  When this is on, texts that have been read will automatically be
  marked as read in any conferences they are added to. This only works
  as long as the LysKOM session is logged in. Texts are never 
  automatically marked as read in your mailbox.")
    (kom-url-transformation-rules-doc . "\
  Rules for transformation of URLs before they are sent to a web browser.
  \"From\" is a regexp that is replaced by \"To\". Regexps are documented
  in the Emacs manual. The string in \"To\" can contain the following
  special sequences:

      \\& means substitute original matched text,
      \\N means substitute match for (...) number N,
      \\\\ means insert one \\.")

    (kom-text-links-doc . "\
  Text link definitions. Each line defines one link for texts with a
  particular recipient. All occurrences of the link pattern will be shown
  as URL links. The URL pointed to by the link is generated from the text
  matching the link pattern, by substituting according to the link
  substitution. The text that is highlighted is determined by the 
  \"Highlight group\" setting: zero means highlight the entire match. A
  number 1-9 indicated the parenthesized group to highlight (and if that
  makes no sense to you, stick with zero). If \"Case insensitive\" is
  on, text will be matched without recard for character case.

  The link substitution can contain the following special sequences:

      \\& means substitute original matched text,
      \\N means substitute match for (...) number N,
      \\\\ means insert one \\.")

    (kom-active-face-doc . "\
  Face used for clickable areas other than URLs, conference and person
  names, friends, morons and text numbers.")
    (kom-active-highlight-face-doc . "\
  Face intended for clickable areas that are to be highlighted.")
    (kom-active-strikethrough-face-doc . "\
  Face intended for clickable areas that are to be indicated with
  strikethrough.")
    (kom-url-face-doc . "\
  Face used for clickable URLs.")
    (kom-me-face-doc . "\
  Face used for my own name.")
    (kom-highlight-face-doc . "\
  Face used for highlighting clickable areas as the mouse is moved
  over them.")
    (kom-subject-face-doc . "\
  Face used for subject lines.")
    (kom-text-no-face-doc . "\
  Face used for text numbers.")
    (kom-friends-face-doc . "\
  Face used for names of people listed in \"Friends and other 
  special people\".")
    (kom-morons-face-doc . "\
  Face used for names of people listed in \"Morons and other
  \"special\" people.")
    (kom-presence-face-doc . "\
  Face used for presence messages.")
    (kom-first-line-face-doc . "\
  Face used for first header line. This face is overlayed on other
  text. Properties not defined by this face will be retained. For
  example, if you only set the weight property, the colors normally
  used will not be changed.")
    (kom-warning-face-doc . "\
  Face used for important warnings.")
    (kom-mark-face-doc . "\
  Face used for various marks, including the indicator shown when the
  client scrolls less than a full page. This face is overlayed on other
  text. Properties not defined by this face will be retained. For
  example, if you only set the weight property, the colors normally
  used will not be changed.")
    (kom-dim-face-doc . "\
  Face used for various discreet texts.")
    (kom-text-body-face-doc . "\
  Face used for text bodies. This face is overlayed on other
  text. Properties not defined by this face will be retained. For
  example, if you only set the weight property, the colors normally
  used will not be changed.")
    (kom-dashed-lines-face-doc . "\
  Face used for dashed lines around texts.  This face is overlayed
  on other text. Properties not defined by this face will be retained. 
  For example, if you only set the weight property, the colors 
  normally used will not be changed.")
    (kom-async-text-body-face-doc . "\
  Face used for messages.  This face is overlayed on other
  text. Properties not defined by this face will be retained. For
  example, if you only set the weight property, the colors normally
  used will not be changed.")
    (kom-async-dashed-lines-face-doc . "\
  Face used for dashed lines around messages.  This face is overlayed
  on other text. Properties not defined by this face will be retained. 
  For example, if you only set the weight property, the colors normally
  used will not be changed.")
    (kom-lynx-terminal-doc . "\
  Lynx can run inside emacs or in an xterm (or similar). This setting
  decides which.")
    (kom-lynx-terminal-command-doc . "\
  This is the command to use to start Lynx in emacs terminal-mode.
  It should be something simple, like \"lynx\".")
    (kom-lynx-xterm-command-doc . "\
  This is the command to use to start Lynx in an xterm. The command
  neets to start both the terminal emulator and lynx.")
    (kom-show-sync-messages-doc . "\
  When this is on, the client will display a message when the LysKOM
  server signals that it is saving the database.")
    (kom-highlight-conferences-doc . "\
  This setting controls display of conference names. For each choice
  it is possible to select a face. The value must be either a face or
  the name of a variable that contains a face.")

    ;;
    ;; Tags for variables
    ;;
    
    (kom-ansaphone-replies-tag . "Automatic replies")
    (kom-bury-buffers-tag . "Bury buffers when changing LysKOM:")

  (kom-personal-messages-in-window-tag . "Personal messages:      ")
    (kom-customize-in-window-tag       . "LysKOM customization:   ")
    (kom-write-texts-in-window-tag     . "Author new texts:       ")
    (kom-prioritize-in-window-tag      . "Prioritize conferences: ")
    (kom-edit-filters-in-window-tag    . "Modify filters:         ")
    (kom-view-commented-in-window-tag  . "Review comments:        ")
    (kom-list-membership-in-window-tag . "List membership:        ")

    (kom-user-prompt-format-tag . "Prompt format:")
    (kom-user-prompt-format-executing-tag . "Prompt format when executing:")
    (kom-anonymous-prompt-format-tag . "Prompt format (anonymous):")
    (kom-anonymous-prompt-format-executing-tag . "Prompt format when executing (anonymous):")
    (kom-enabled-prompt-format-tag . "Prompt format (admin):")
    (kom-enabled-prompt-format-executing-tag . "Prompt format when executing (admin):")

    (kom-higher-priority-breaks-tag . 
"Read prioritized texts:                           ")
    (kom-created-texts-are-read-tag . 
"Automatically read created texts:                 ")
    (kom-default-mark-tag           . 
"Default mark:                                     ")
    (kom-print-number-of-unread-on-entrance-tag . 
"Show number of unread when entering a conference: ")
    (kom-follow-comments-outside-membership-tag .
"Follow comment chains outside membership:         ")
    (kom-show-footnotes-immediately-tag .
"Show footnotes immediately:                       ")
    (kom-membership-default-priority-tag . 
"Default priority for new memberships:             ")
    (kom-dashed-lines-tag . 
"Dashed lines around the text body:                ")
    (kom-autowrap-tag . 
"Fill wide paragraphs before displaying:           ")
    (kom-show-author-at-end-tag .
"Show the name of the author after the body:       ")

    (kom-truncate-threshold-tag .
"Truncate long texts:                              ")
    (kom-truncate-show-lines-tag .
"Lines to show of truncated texts:                 ")

    (kom-reading-puts-comments-in-pointers-last-tag . "Comment links are shown:")
    (kom-read-depth-first-tag . "Read order:")
    (kom-deferred-printing-tag . "Delayed display:")
    (kom-continuous-scrolling-tag . "Continuous scrolling:")

    (kom-presence-messages-in-echo-area-tag . 
"Presence messages in echo area:        ")
    (kom-presence-messages-in-buffer-tag .
"Presence messages in the LysKOM buffer:")
    (kom-page-before-command-tag . "Clear the screen:")

    (kom-idle-hide-tag . 
"Number of minutes of inactivity before session is hidden:      ")
    (kom-show-where-and-what-tag . 
"Show where sessions are logged on from and what they are doing:    ")
    (kom-show-since-and-when-tag . 
"Show when sessions connected and how long they have been inactive: ")



    (kom-login-hook-tag . "Commands executed after logging on:")
    (kom-do-when-done-tag . "Commands to execute after reading everything:")
    (kom-permissive-completion-tag . "Fussy name completion:")
    (kom-show-personal-messages-in-buffer-tag . 
"Where are messages shown: ")
    (kom-pop-personal-messages-tag . 
"Pop up message buffer:    ")
    (kom-default-message-recipient-tag . 
"Default message recipient:")

    (kom-audio-player-tag . "Audio player program:")
    (kom-ding-on-new-letter-tag        . "When a letter arrives:            ")
    (kom-ding-on-priority-break-tag    . "When a prioritized text arrives:  ")
    (kom-ding-on-wait-done-tag         . "When done waiting:                ")
    (kom-ding-on-common-messages-tag   . "When a public message arrives:    ")
    (kom-ding-on-group-messages-tag    . "When a group message arrives:     ")
    (kom-ding-on-personal-messages-tag . "When a personal message arrives:  ")
    (kom-ding-on-no-subject-tag     .    "When you forget the subject line: ")

    (kom-filter-outgoing-messages-tag . "Show automatic messages:")
    (kom-friends-tag . "Friends and other special people:")
    (kom-morons-tag . "Morons and other \"special\" people:")
    (kom-url-viewer-preferences-tag . "Open URLs using the following program:")
    (kom-windows-browser-command-tag . "Command to start a web browser on Windows:")
    (kom-mosaic-command-tag . "Command to start NCSA Mosaic:")
    (kom-netscape-command-tag . "Command to start Netscape/Mozilla:")
    (kom-galeon-command-tag . "Command to start Galeon:")

    (kom-symbolic-marks-alist-tag . "Symbolic mark types:")

    (kom-cite-string-tag . "Quotation indicator:")
    (kom-confirm-multiple-recipients-tag . 
"Confirm multiple recipients:         ")
    (kom-check-commented-author-membership-tag . 
"Check membership of commented author:")
    (kom-check-for-new-comments-tag . 
"Check for unread comments:           ")

    (kom-ansaphone-record-messages-tag . 
"Save messages when auto reply is on:    ")
    (kom-ansaphone-show-messages-tag . 
"Display messages when auto reply is on: ")
    (kom-ansaphone-default-reply-tag . "Auto reply message:")


    (kom-inhibit-typeahead-tag . "Buffer keypresses:")
    (kom-max-buffer-size-tag . "Maximum buffer size:")

    (kom-remote-control-tag .     "Remote commands on or off:             ")
    (kom-self-control-tag .       "Allow me to use remote commands:       ")
    (kom-remote-controllers-tag . "People allowed to use remove commands:")

    (kom-customize-format-tag . "Show documentation for all settings:")
    (kom-default-language-tag . "Default language:")
    (kom-show-namedays-tag    . "Show today's names:")
    (kom-ispell-dictionary-tag . "Spelling dictionary:")

    (kom-show-week-number-tag . "Show week number:")
    (kom-membership-default-placement-tag . "Placement of new memberships:")
    (kom-show-imported-importer-tag . "Show importer of imported messages:")
    (kom-show-imported-envelope-sender-tag . "Show sender of imported messages:")
    (kom-show-imported-external-recipients-tag . "Show external recipients of imported messages:")
    (kom-agree-text-tag . "Default text for Agree:")
    (kom-silent-ansaphone-tag . "Beep when auto reply is on:")
    (kom-default-session-priority-tag . "Default session priority:")
    (kom-unsubscribe-makes-passive-tag . "Leaving a conference converts membership to passive:")
    (kom-review-priority-tag . "Priority for review commands:")
    (kom-show-creating-software-tag . "Show creating software:")
    (kom-text-footer-format-tag . "Text footer format:")
    (kom-long-lines-tag . "Long dashed lines:")
    (kom-postpone-default-tag . "Default number of texts to postpone:")
    (kom-allow-incompleteness-tag . "Allow incomplete information about unread texts:")
    (kom-smileys-tag . "Show smileys graphically:")
    (kom-ignore-message-senders-tag . "Don't show messages from:")
    (kom-ignore-message-recipients-tag . "Don't show messages to:")
    (kom-text-footer-dash-length-tag . "Text footer length:")
    (kom-text-header-dash-length-tag . "Length of dashed line before text:")
    (kom-show-personal-message-date-tag . "Show date and time of messages:")
    (kom-w3-simplify-body-tag . "Display HTML without document colors:")
    (kom-mercial-tag . "Text to display when all is read:")
    (kom-server-priority-tag . "Session priority:")
    (kom-server-priority-breaks-tag . "Prompt to go to next LysKOM:")
    (kom-complete-numbers-before-names-tag . "Read conference numbers before names:")
    (kom-keep-alive-interval-tag . "Keep connection alive interval:")
    (kom-text-no-prompts-tag . "How commands prompt for text numbers:")
    (kom-saved-file-name-tag . "File to archive texts in:")
    (kom-follow-attachments-tag . "Read imported attachments as texts:")
    (kom-show-unread-in-frame-title-tag . "Show unread indicator in title bar:")
    (kom-created-texts-are-saved-tag . "Save created texts:")
    (kom-confirm-add-recipients-tag . "Ask if comments should be sent to new recipients:")
    (kom-trim-buffer-minimum-tag . "How small parts of the LysKOM buffer are trimmed:")
    (kom-dont-check-commented-authors-tag . "Authors not to check:")
    (kom-print-relative-dates-tag . "Show relative dates:")
    (kom-print-seconds-in-time-strings-tag . "Include seconds in time strings:")
    (kom-review-uses-cache-tag . "Review commands use cached texts:")
    (kom-review-marks-texts-as-read-tag . "Review commands mark texts as read:")
    (kom-auto-review-faqs-tag . "Review new FAQs automatically:")
    (kom-auto-list-faqs-tag . "List new FAQs automatically:")
    (kom-extended-status-information-tag . "Extended status information:")
    (kom-highlight-first-line-tag . "Color first line:")
    (kom-highlight-dashed-lines-tag . "Color dashed lines:")
    (kom-highlight-text-body-tag . "Color text backgrounds:")
    (kom-async-highlight-dashed-lines-tag . "Color dashed lines around messages:")
    (kom-async-highlight-text-body-tag . "Color message backgrounds:")
    (kom-edit-hide-add-button-tag . "Show add button when writing texts:")
    (kom-format-html-authors-tag . "Format HTML-messages per author:")
    (kom-keyboard-menu-immediate-selection-tag . "Shortcuts in text menus require confirmation:")
    (kom-max-overlays-tag . "Maximum number of overlays:")
    (kom-mark-read-texts-as-read-in-new-recipient-tag . "Mark read texts read when added to new recipients:")
    (kom-url-transformation-rules-tag . "Transformation of URLs:")
    (kom-text-links-tag . "Text links:")

    (kom-active-face-tag . "Other clickable areas:")
    (kom-active-highlight-face-tag . "Highlighted clickable areas:")
    (kom-active-strikethrough-face-tag . "Struck-through clickable areas:")
    (kom-url-face-tag . "Clickable URLs:")
    (kom-me-face-tag . "My own name:")
    (kom-highlight-face-tag . "Mouseover highlight:")
    (kom-subject-face-tag . "Subject lines:")
    (kom-text-no-face-tag . "Text numbers:")
    (kom-friends-face-tag . "Friends and other special people:")
    (kom-morons-face-tag . "Morons and other \"special\" people:")
    (kom-presence-face-tag . "Presence messages:")
    (kom-first-line-face-tag . "First header line:")
    (kom-warning-face-tag . "Important warnings:")
    (kom-mark-face-tag . "Various marks:")
    (kom-dim-face-tag . "Various discreet texts:")
    (kom-text-body-face-tag . "Text bodies:")
    (kom-dashed-lines-face-tag . "Dashed lines around texts:")
    (kom-async-text-body-face-tag . "Messages:")
    (kom-async-dashed-lines-face-tag . "Dashed lines around messages:")
    (kom-lynx-terminal-command-tag . "Command to run lynx in emacs:")
    (kom-lynx-terminal-tag . "Where to run lynx:")
    (kom-lynx-xterm-command-tag . "Command to run lynx in xterm:")
    (kom-show-sync-messages-tag . "Show database-saving message:")
    (kom-highlight-conferences-tag . "Display of names::")
    )
)






;;;; ============================================================
;;;; The default Ansaphone message goes here. The more complex 
;;;; message specification probably should too, but it's not here
;;;; yet. People who know how to use it are smart enough to do it
;;;; right.

(lyskom-language-var local kom-ansaphone-default-reply en 
  "I am not reading LysKOM right now. Please write a letter instead.")
        
;;;; ============================================================
;;;; Other language-dependent variables
;;;;

(lyskom-language-var local kom-ispell-dictionary en
  "english")



;;;; ================================================================
;;;; Tell phrases should be configured with the default language used
;;;; at the server and not for person reading if they happens to
;;;; differ. This is of course because they are sent to the server for
;;;; everybody else to see.

;;;; Aronsson was here 4 DEC 1990, thus creating version 0.18
					; Created *-tell-*


;;; To coders of the elisp-client:
;;; You not only have to change the text here, you also have to modify
;;; kom-tell-phrases-validation-list in vars.el if you add or remove
;;; one of these.

(eval-when-compile (defvar kom-tell-phrases))

(lyskom-language-strings local kom-tell-phrases en
  '((kom-tell-silence		. "") ; Why ?
    (kom-tell-send		. "Is trying to post a text.")
    (kom-tell-login		. "Is entering LysKOM.")
    (kom-tell-read		. "Is reading.")
    (kom-tell-1st-pres		. "Is writing the first presentation.")
    (kom-tell-write-comment	. "Is writing a comment.")
    (kom-tell-write-footnote	. "Is writing a footnote.")
    (kom-tell-write-letter	. "Is writing a letter.")
    (kom-tell-write-reply	. "Is writing a personal reply.")
    (kom-tell-write-text	. "Is writing a text.")
    (kom-tell-conf-pres		. "Is writing the presentation for a new conference.")
    (kom-tell-recover		. "Is restarting KOM. Sigh.")
    (kom-tell-wait		. "Is waiting.")
    (kom-tell-regret		. "Decides to throw away the text.")
    (kom-tell-review		. "Is reviewing.")
    (kom-tell-change-name       . "Takes on a new name.")
    (kom-tell-change-supervisor . "Changes the supervisor of something.")
    (kom-tell-next-lyskom	. "Moves to a different LysKOM.")))

(if (and (boundp 'kom-tell-phrases)
         kom-tell-phrases)
    (lyskom-language-strings local kom-tell-phrases en
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

(lyskom-language-var local kom-mercial en (lyskom-get-string 'kom-tell-wait 
                                                       'kom-tell-phrases))


(lyskom-language-strings local lyskom-error-texts en
  '((error-0 . "No error")
    (error-2 . "Not yet implemented")
    (error-3 . "No longer implemented")
    (error-4 . "Wrong password")
    (error-5 . "String too long")
    (error-6 . "You have not logged on")
    (error-7 . "Nobody may enter LysKOM at this time")
    (error-8 . "You attempted to use conference number 0")
    (error-9 . "Undefined or secret conference")
    (error-10 . "Undefined or secret user")
    (error-11 . "No read or write permission")
    (error-12 . "Illegal operation")
    (error-13 . "You are not a member of that conference")
    (error-14 . "There is no text with that number")
    (error-15 . "You cannot use global text number 0")
    (error-16 . "There is no text with that local number")
    (error-17 . "You cannot use local text number 0")
    (error-18 . "Name too short, too long or containing illegal characters")
    (error-19 . "Index out of bounds")
    (error-20 . "The conference already exists")
    (error-21 . "The user already exists")
    (error-22 . "Secret but not read-protected")
    (error-23 . "You are not allowed to change the person/conference flag")
    (error-24 . "Error in the database. Tough luck.")
    (error-25 . "Invalid recipient or comment type")
    (error-26 . "Illegal info type. (Bug in the client)")
    (error-27 . "Already recipient of this text")
    (error-28 . "Already comment to this text")
    (error-29 . "Already footnote to this text")
    (error-30 . "Not a recipient of this text")
    (error-31 . "Not a comment to this text")
    (error-32 . "Not a footnote to this text")
    (error-33 . "Too many recipients")
    (error-34 . "Too many commentsp")
    (error-35 . "Too many footnotes")
    (error-36 . "Too many marks")
    (error-37 . "You are not the author of that text")
    (error-38 . "You cannot connect to the server")
    (error-39 . "Out of memory")
    (error-40 . "The server is gone crazy")
    (error-41 . "The client thinks that the server says that it does not understand the client")
    (error-42 . "No such session")
    (error-43 . "Invalid regular expression")
    (error-44 . "Can't unmark a text that was not marked")
    (error-45 . "Temorary lossage. Please try again later")
    (error-46 . "Sending huge messages to the server is not a nice thing to do")
    (error-47 . "Anonymous texts are not accepted by all recipients")
    (error-48 . "Invalid auxiliary information")
    (error-49 . "Change of auxiliary information not permitted")
    (error-50 . "Unknown asynchronous message")
    (error-51 . "Internal server error")
    (error-52 . "Feature disabled in the server")
    (error-53 . "Unable to send message")
    (error-54 . "Invalid membership type")

    (error--1 . "You are not a member of any recipient")
))

(lyskom-language-var global lyskom-unread-mode-line en
  (lyskom-make-lyskom-unread-mode-line))

(lyskom-language-var global lyskom-unread-title-format en
  (lyskom-make-lyskom-unread-title-format))

(lyskom-language-var local lyskom-help-data en lyskom-en-help-data)


(provide 'lyskom-strings)

;;; english-strings ends here
