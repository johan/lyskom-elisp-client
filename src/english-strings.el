;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: english-strings.el,v 44.54 1999-06-30 09:01:02 ceder Exp $
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
;;;; File: english-strings.el
;;;;
;;;; This file contains all strings in the LysKOM elisp client.
;;;; Language:     English.
;;;; Matches version 36.11 of swedish-strings.el
;;;; ================================================================
;;;;
;;;; Translation from swedish-strings.el: David Byers
;;;;

(require 'lyskom-vars "vars")
(require 'lyskom-language "language")

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
              "$Id: english-strings.el,v 44.54 1999-06-30 09:01:02 ceder Exp $"))


;;; ================================================================
;;; The language definition

(lyskom-define-language 'en "English" "Engelska")


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
  (define-key lyskom-en-edit-mode-map (kbd (lyskom-keys 'button2up)) 'kom-button-click-or-yank)
  (define-key lyskom-en-edit-mode-map (kbd (lyskom-keys 'button3)) 'kom-popup-menu)
  (define-key lyskom-en-edit-mode-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lyskom-en-edit-mode-map (kbd "C-c")	'lyskom-en-edit-prefix)
  (define-key lyskom-en-edit-prefix (kbd "C-x") 'lyskom-en-edit-aux-prefix)
  (define-key lyskom-en-edit-prefix (kbd "?")	'lyskom-help)
  (define-key lyskom-en-edit-prefix (kbd "C-r") 'lyskom-en-edit-review-prefix)
  (define-key lyskom-en-edit-prefix (kbd "C-i") 'lyskom-en-edit-insert-prefix)
  (define-key lyskom-en-edit-prefix (kbd "*") 'kom-button-press)
  (define-key lyskom-en-edit-prefix (kbd "+") 'kom-menu-button-press)
  (define-key lyskom-en-edit-prefix (kbd "C-c")	'kom-edit-send)
  (define-key lyskom-en-edit-prefix (kbd "C-s") 'kom-ispell-message)
  (define-key lyskom-en-edit-prefix (kbd "C-k")	'kom-edit-quit)
  (define-key lyskom-en-edit-prefix (kbd "r ?")	'lyskom-help)
  (define-key lyskom-en-edit-prefix (kbd "C-r C-c") 'kom-edit-show-commented)
  (define-key lyskom-en-edit-prefix (kbd "C-i ?") 'lyskom-help)
  (define-key lyskom-en-edit-prefix (kbd "C-i C-c") 'kom-edit-insert-commented)
  (define-key lyskom-en-edit-prefix (kbd "C-i C-y") 'kom-edit-insert-commented)
  (define-key lyskom-en-edit-prefix (kbd "C-i 1") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 2") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 3") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 4") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 5") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 6") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 7") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 8") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i 9") 'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-prefix (kbd "C-i ") 'kom-edit-insert-text)
  (define-key lyskom-en-edit-prefix (kbd "C-a") 'lyskom-en-edit-add-prefix)
  (define-key lyskom-en-edit-prefix (kbd "C-a C-r") 'kom-edit-add-recipient)
  (define-key lyskom-en-edit-prefix (kbd "C-a C-c") 'kom-edit-add-copy)
  (define-key lyskom-en-edit-prefix (kbd "C-a <RET>") 'kom-edit-move-text)
  (define-key lyskom-en-edit-prefix (kbd "C-a C-x") 'kom-edit-add-cross-reference)
  (define-key lyskom-en-edit-prefix (kbd "C-x C-p") 'kom-edit-add-personal-comments)
  (define-key lyskom-en-edit-prefix (kbd "C-x C-n") 'kom-edit-add-no-comments)
  (define-key lyskom-en-edit-prefix (kbd "C-x C-r") 'kom-edit-add-read-confirm-request)
  (define-key lyskom-en-edit-prefix (kbd "C-a ?") 'lyskom-help))


;;(defvar lyskom-header-separator
;;  (substitute-command-keys
;;   "\\<lyskom-edit-mode-map>\
;;--- Write below. \
;;Post: \\[kom-edit-send], \
;;Kill: \\[kom-edit-quit], \
;;Help: \\[describe-mode] ---")
;;  "*String to separate headers from text body.")
;;
;;(defvar lyskom-swascii-header-separator nil
;;  "The swascii version of lyskom-header-separator.")
;;
;;(defvar lyskom-header-subject "Subject: "
;;  "*String to prompt for subject in the edit buffer.")
;;
;;
;;(defvar lyskom-swascii-header-subject nil
;;  "The swascii version of lyskom-header-subject.")

(defconst lyskom-strings-missing
  '(cgdag sixjune holdnose))

;;; Formely known as lyskom-strings
(lyskom-language-strings lyskom-message en 
  '(
    ; From vars.el: 
    ; From komtypes.el: nil
    ; From clienttypes.el: nil
    ; From startup.el:
    (server-q . "LysKOM server? (%#1s) ")
    (try-connect . "LysKOM elisp client version %#1s.\nAttempting to connect to %#2s.\n")
    (protocoll-error . "Protocol error. Servers says: %#1s")
    (too-old-server . "The server is too old for this version of the client.")
    (connection-done . "Connection established. Server version is %#1s.\n\n")
    (what-is-your-name . "What is your name? ")
    (password . "Your Password? ")
    (wrong-password . "Incorrect password.\n")
    (are-logged-in . "You have entered LysKOM. Please wait...\n")
    (you-have-motd . "\nYou have a note on your door:\n\n")
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

If you ar uncertain about how to use LysKOM, you can retrieve a manual by
anonymous ftp to ftp.lysator.liu.se. Ask your system administrator for help
on how to do this.
")

    (is-name-correct . "Is the name %#1s correct? ")
    (personal-password . "Enter a personal password: ")
    (repeat-password . "Repeat for confirmation: ")
    (repeat-failure . "The passwords were not the same.\n")

    (could-not-create-you .  "LysKOM couldn't create that user.\n")
    (presentation-subject . "%#1s")
    (presentation-form . "Name:\t\nAddress:\t\n\t\nTelephone:\t
Email-address:\t\nWWW:\t\n\nOther:\t")
    (presentation-help . "You are writing your presentation.\n")
    (not-present-anywhere . "Not in any conference.")
    (unknown-person . "Unknown user")
    (in-secret-conference . "Secret conference (%#1d).")
    (start-new-session-same-server
     . "You are already connected to that server. Do you want a new session? ")
    (new-session-in-buffer . "\n\n---- New session at %s ----\n\n")



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

    (what-text-to-delete . "Remove which article? ")
    (delete-marked-text . "The article is %#1s. Delete anyway? ")
    (delete-marked-by-you . "marked by you")
    (delete-marked-by-you-and-others . "marked by you and %#1?d%[someone else%]%[%#1d others%]")
    (delete-marked-by-several . "marked by %#1d user%#1?d%[%]%[s%]")
    (deleting-text . "Removing article %#1:n...")

    (presentation-for-whom . "Which conference/user? ")
    (somebody-deleted-that-conf . "Somebody just deleted that conference.\n")
    (review-presentation-of . "Review presentation of %#1M.\n")
    (has-no-presentation . "%#1:M has no presentation.\n")

    (have-to-read . "You must read an article first.\n")

    (no-comment-to . "There is no commented article.\n")

    (who-letter-to . "Send a letter to whom? ")
    (who-send-text-to . "Send article to which conference? ")
    (has-motd . "%#1P has a note on the door:\n\n")
    (motd-persist-q . "Send the letter? ")

    (who-to-add . "Whom do you want to add? ")
    (where-to-add . "To which conference? ")
    (where-to-add-self . "Join which conference? ")
    (priority-q . "Conference priority? (0 (passive membership), 1 (low) - 255 (high)) ")
    (done . "done.\n")
    (nope . "didn't work.\n")

    (cant-find-supervisor . "Can't find supervisor of %#1M.\n")
    (is-read-protected-contact-supervisor . "%#1M is closed.
Send a letter to  %#2P to apply for membership.\n")

    (conf-does-not-exist . "\nThe conference doesn't exist.\n")

    (who-to-exclude . "Who do you want to remove? ")
    (where-from-exclude . "From which conference? ")

    (leave-what-conf . "Leave which conference? ")

    (error-fetching-person . "Error retreiving user.\n")
    (error-fetching-conf . "Error retreiving conference.\n")

    (name-of-conf . "Conference name? ")
    (anyone-member . "May anyone join? ")
    (secret-conf . "Secret conference? ")
    (comments-allowed . "Are comments allowed? ")
    (anonymous-allowed . "Are anonymous articles allowed? ")
    (secret-members-allowed . "Are secret members permitted? ")
    (what-comment-no . "Comment article number: ")
    (what-footnote-no . "Footnote article number: ")
    (confusion-what-to-comment . "I can't figure out which article you want to comment.\n")
    (confusion-what-to-footnote . "I can't figure out to which article you want to write a footnote to.\n")
    (what-private-no . "Private reply to article number: ")

    (confusion-who-to-reply-to . "I can't figure out which article you want to write a private reply to.\n")
    (confusion-what-to-answer-to . "I can't figure out which article you want to write a reply to.\n")
    (quit-in-spite-of-unsent . "You have an unsent article. Do you really want to quit? ")
    (confusion-what-to-view . "I can't figure out which article you want to view.\n")
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
    (what-to-change-pres-you . "Change presentation of who/what (yourself): ")
    (who-to-put-motd-for . "Post note on the door of who/what (yourself): ")

    (cant-get-conf-stat . "Cannot get the status of that conference.\n")
    (go-to-conf-p . "Go to conference: ")
    (want-become-member . "Do you want to join? ")
    (no-ok . "Okiedokie, whatever you say.\n")

    (who-to-remove-motd-for . "Remove note from the door of who/what: ")

    (conf-all-read . "%#1M - no unread articles.\n")
    (no-in-conf . "You are not present in any conference.\n")

    (search-for-pers . "Enter search key (RETURN for all users): ")
    (search-for-conf . "Enter search key (RETURN for all conferences): ")
    (search-re . "Enter search regexp: ")
    (name-to-be-changed . "Name to change: ")
    (no-such-conf-or-pers . "The conference or user doesn't exist.\n")
    (new-name . "New name: ")
    (new-paren . "New parenthesis: ")
    (no-paren-in-name . "The name doesn't contain a parenthesis.\n")
    (who-to-change-supervisor-for . "Change supervisor of who/what? ")
    (new-supervisor . "New supervisor: ")
    (text-to-mark . "Mark which article? ")
    (text-to-unmark . "Unmark which article? ")
    (what-mark . "Set which mark? ")
    (unmarking-textno . "Unmarking article %#1n...")
    (marking-textno . "Marking article %#1n...")
    (list-which-mark . "List articles with which mark (0-255, RET for all)? ")

    (new-passwd-again . "Repeat the new password for confirmation: ")
    (what-mark-to-view . "Review which mark? ")
    (whos-passwd . "Change password for whom? (yourself) ")
    (old-passwd . "Your current password: ")
    (new-passwd . "The new password: ")
    (changing-passwd . "Changing password...")
    (retype-dont-match . "You didn't reenter the same passwrod. Try again.\n")
    (palindrome . "(a palindrome!) ")
    (lyskom-name . "User")
    (is-in-conf . "In conference")
    (from-machine . "At")
    (is-doing . "Activity")
    (connection-time . "Connected")
    (active-last . "Active last")
    (active . "Active")
    (lyskom-client . "Client")
    (text-to-add-recipient . "Add recipient to which article:")
    (text-to-add-copy . "Add recipient of carbon copy of which article:")
    (text-to-add-bcc . "Add recipient of blind carbon copy of which article:")
    (text-to-delete-recipient . "Remove recipient from which article:")
    (text-to-move . "Which text do you want to move: ")
    (text-to-add-comment-to . "Add comment to which article:")
    (text-to-delete-comment-from . "Remove comment from which article:")
    (text-to-add-footnote-to . "Add footnote to which article:")
    (text-to-delete-footnote-from . "Remove footnote from which article:")

    (where-on-list-q . "Placement in your list? (0-%#1d) ")
    (member-in-conf . "Joining to %#1M...")
    (add-member-in . "Adding %#1P as a member of %#2M...")
    (unsubscribe-to . "Leaving %#1M...")
    (passivate-done . "You are now a passive member of %#1M.\n")

    (exclude-from . "Removing %#1P from %#2M...")

    (unsubscribe-failed . "\nDidn't work. Perhaps %#1P isn't a member of %#2M?\n")

    (You . "You")
    (could-not-create-conf . "Couldn't create the conference \"%#1s\".\n")
    (created-conf-no-name . "Conference number %[%#3@%#1:m %#2:M%] has been created.\n")
    (cant-read-textno . "You are not allowed to read article %#1:n")

    (not-supervisor-for . "You are not the supervisor of %#1M.\n")
    (go-to-conf . "Go to conference %#1M.\n")
    (cant-go-to-his-mailbox . "You are not allowed to go to %#1M's mailbox.\n")
    (not-member-of-conf . "You are not a member of %#1M.\n")
    (about-to-change-name-from . "%#1M\n")
    (change-name-done . "Done. New name: %[%#2@%#1:M%].\n")
    (change-name-nope . "Couldn't change name to %#1s. Error code %#3d. %#2s.\n")
    (change-supervisor-from-to . "Change supervisor of %#1M to %#2P...")
    (change-supervisor-nope . 
     "\nDidn't work. Perhaps you are not allowed to change the supervisor of %#1M?\n")
    
    (no-marked-texts . "You have not marked any articles.\n")
    (no-marked-texts-mark . 
     "You have not marked any articles with mark %#1d.\n")

    (weekdays . ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday"
		 "Friday" "Saturday" "Sunday"])
    (weekdays-short . ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"])
    (time-is . "The time is %#1s %#2s(according to the server).")
    (time-format-exact . "%#7s %4#1d-%02#2d-%02#3d %02#4d:%02#5d:%02#6d")

    (xmaseve . "Christmas eve!\nYou didn't open any gifts early, did you?")
    (xmasday . "Christmas day.\nDid you get any nice gifts this year?")
    (newyearday . "\nMay %#1d be a prosperous and good year for you!")
    (newyeareve . "Happy New Year!")
    (newyearevelate . "Less than an hour to go...")
    (lysbday . "
On this day, in 1973, the Lysator Academic Computer Society was
formed, and it was a great day in the history of computing in Sweden.
Read all about it at http://www.lysator.liu.se/history/")

    (total-users . "    A total of %#1d users (%#2s.)\n")
    (total-visible-users . "    A total of %#1d visible user%#1?d%[%]%[s%] (%#2s.)\n")
    (total-active-users . "    A total of %#1d active user%#1?d%[%]%[s%] (%#2s.)\n")
    (total-visible-active-users
     . "    A total of %#1d visible active user%#1?d%[%]%[s%] (%#2s.)\n")
    (who-to-add-q . "Add who/what as a recipient? ")
    (who-to-add-copy-q . "Add which conference/user as recipient of a carbon copy? ")
    (who-to-sub-q . "Remove who/what as a recipient? ")
    (who-to-move-from-q . "Move from where? ")
    (who-to-move-to-q . "Move to where? ")

    (adding-name-as-recipient . "Adding %#1M as recipient of article %#2n...")
    (adding-name-as-copy . "%#1M will receive a carbon copy of article %#2n...")
    (remove-name-as-recipient .
     "Removing %#1M as recipient of article %#2n...")
    (moving-name . "Moving text %#3n from %#1M to %#2M...")
    (text-to-add-q . "Add which article as a comment? ")
    (text-to-remove-q . "Remove which article as a comment? ")
    (text-to-add-footn-q . "Add which article as a footnote? ")
    (text-to-remove-footn-q . "Remove which article as a footnote? ")
    (add-comment-to . "Adding article %#1n as a comment to text %#2n...")
    (sub-comment-to . "Removing article %#1n as a comment to article %#2n...")
    (add-footnote-to . "Adding article %#1n as a footnote to text %#2n...")
    (sub-footnote-to . "Removing article %#1n as a footnote to article %#2n...")
    (comment-keep-recpt-p ."Should %#1s remain a recipient? ")
    (comment-all-relevant-p . "There are multiple recipients. Are they all relevant? ")
    (please-edit-recipients . "Modify the recipient list and send the article again.")
    (add-recipient-p . "Add recipient %#1P? ")
    (checking-rcpt . "Checking recipients...")
    (checking-rcpt-done . "Checking recipients...done")
    (checking-comments . "Checking commented articles...")
    (checking-comments-done . "Checking commented articles...done")
    (please-check-commented-texts . "Review the commented text and its comments.")
    (have-unread-comment . "Send despite unread comments to text %#1n? ")

    (matching-regexp . "Conferences/users matching `%#1s'\n")

    (who-is-active-all . "Showing all sessions.\n")
    (who-is-active-last-minutes . "Showing all sessions active tha last %#1d minutes.\n")
    (showing-invisibles . "Showing invisible sessions.\n")
    (null-who-info . "No one (active) is logged on.\n")

    (no-other-lyskom-r . "There are not other active LysKOM sessions.\n")
    (no-lyskom-session . "There are no active LysKOM-sessions.")
    (no-unread-lyskom-r . "There is no active LysKOM session with unread texts.\n")
    (no-unread-lyskom . "There is no active LysKOM session with unread texts.")

    (who-is-on-in-what-conference . "Who in which conference: ")
    (who-is-active-and-member . "Only members in %#1M are shown.\n")

    ; From commands2.el:

    (your-memberships . "Your LysKOM conference memberships:\n")
    (memberships-header . "Last access	   Prio	Unread	Conference\n")
    (memberships-line . "%16#1s  %#2d\t%#3d\t%#4M\n")
    
    (conf-for-status . "Get status of which conference? ")
    (no-such-conf . "The conference doesn't exist.\n")
    (status-record . "Status of conference %#1M (%#2m) %#3s\n\n")
    (change-type-prompt . "Change conference type for %#1M (%#2m) %#3s\n")
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
    (conf-allows-anon-texts .     "Anonymous articles:                      %#1s\n")
    (anon-texts-permitted . "Anonymous articles are permitted")
    (anon-texts-not-permitted . "Anonymous articles are not permitted")
    (secret-members-permitted . "Secret members are permitted")
    (secret-members-not-permitted . "Secret members are not permitted")
    (garb-nice . "Expiration time (in days):%16#1d\n")
    (lowest-local-no . "Lowest local number: %21#1d\n")
    (highest-local-no . "Highest local number: %20#1d\n")
    (last-text-time . 
     "Time of last article: %20#1s (accordning to your cache)\n")
    (no-of-motd . "Note on the door in article: %13#1n\n")
    (superconf-is-no-name . "Superconference: %25#1m %#3s(%#2M)\n")
    (permitted-submitters-no-name . "Allowed authors: %25#1m %#3s(%#2M)\n")
    (supervisor-is-no-name . "Supervisor: %30#1p %#3s(%#2P)\n")
    (presentation-no . "Presentation:    %25#1n\n")
    (conf-has-motd . "\n%#1M has a note on the door:\n")
    (status-conf-generic . "%-40#1s %#2s\n")

    (Everybody . "Everyone")
    (show-members-list-also-q . "List members? ")
    (show-membership-info-q . "Show number of unreads? ")
    (conf-has-these-members . "\n%#1M has the following members:\n")
    (conf-has-no-members . "\n%#1M has no members.\n")
    (member-list-header . "Last entered       Unread  Name\n\n")
    (secret-membership . "--- Secret line ---\n")
    (conf-membership-line . "%#1s%#2M %#3s\n")
    (conf-membership-line-2 . "                          Added %#1s by %#2P\n")
    (pers-for-status . "Get status of which user? ")
    (no-such-pers . "The user doesn't exist.\n")
    (pers-status-record . "Status of user %#1P (%#2p)\n")
    (created-time .  "Created:%34#1s\n\n")
    (created-confs . "Conferences created:%22#1d\n")
    (created-persons . "Users created:%28#1d\n")
    (created-texts . "Articles created:%25#1d\n")
    (created-lines . "Lines created:%28#1d\n")
    (created-chars . "Characters created:%23#1d\n")
    (no-of-sessions . "Session count:%28#1d\n")
    (present-time-d-h-m-s . "Total presence:%16#1d d %02#2d:%02#3d:%02#4d\n")
    (last-log-in . "Last session:%29#1s\n")
    (user-name . "User: %36#1s\n")

    (read-texts . "Articles read:%28#1d\n")
    (marked-texts . "Articles marked:%26#1d\n")
    (time-for-last-letter . "Time of last letter:%22#1s (accordning to your cache)\n")
    (superconf . "Superconference: %25#1m %#3s(%#2M)\n")
    (supervisor . "Supervisor: %30#1p %#3s(%#2P)\n")
    (member-of-confs . "Member of (conferences):%18#1d\n")
    (presentation . "Presentation:    %25#1n\n")
    (show-membership-list-also-q . "List memberships? ")
    (not-allowed-see-confs . "You are not premitted to see which conferences %#1P is a member of.\n")
    (is-member-of . "\n%#1P is a member of the following conferences:\n")
    (membership-list-header . "Last access       Unread Conference\n\n")
    (pers-membership-line . "%#1s%#2s%#3M %#4s\n")
    (pers-membership-line-2 . "                          Added %#1s by %#2P\n")
    (is-supervisor-mark . "O ")
    (who-to-send-message-to . "Send message to whom? (%s) ")
    (send-empty-message-p . "The message is empty. Send it anyway? ")
    (his-total-unread . "\n%#1M has a total of %#2d unread articles.\n")
    (message-prompt . "Message: ")
    (message-sent-to-user . "\
================================================================
Your message for %#2M:

%#1t
----------------------------------------------------------------
")
    (message-sent-to-all . "\
================================================================
Your public message:

%#1t
----------------------------------------------------------------
")
    (message-sent-to-user-long . "\
===============================================================================
Your message for %#2M:

%#1t
-------------------------------------------------------------------------------
")
    (message-sent-to-all-long . "\
===============================================================================
Your public message:

%#1t
-------------------------------------------------------------------------------
")
    (message-use-alarm-instead . "Use %#1s to send alarm messages.\n")
    (message-all-info . "Send alarm\n")
    (message-recipient-info . "Send message to %#1M\n")
    (message-nope .
     "Unable to send the message. Perhaps the recipient isn't logged on.
The message you were sending to %#1M was:
%#2t\n")
    (only-last . "Last (0 - %#1d) articles in %#2s: ")
    (only-error . "Something went wrong. Sorry.\n")
    
    (you-have-unreads . "You have %#1d unread article%#1?d%[%]%[s%] in %#2M\n")
    (you-have-unreads-special . "You have %#1d uncommented article%#1?d%[%]%[s%] in %#2M\n")
    (you-have-no-unreads . "You have read everything in %#1M\n")
    (you-have-read-everything . "No news (is bad news).\n")
    (total-unreads . "\nYou have %#1d unread article%#1?d%[%]%[s%] articles in %#2M.\n")
    (list-unread-with-n-unread . "Listing conferences with at least %#1d unread.\n")
    (waiting-for-anything .
     "You are waiting for an article in any conference.\n")
    (waiting-higher-than . 
     "You are waiting for an article in any conference with a priority higher than %#1d.\n")
    
    (have-to-be-in-conf-with-unread . "You must go to a non-empty conference first.\n")
    (Texts . "Article")
    (Date . "Date")
    (Lines . "Len.")
    (Author . "  Author")
    (Subject . "  Subject")
    (mark-type . "Mrk")

    (could-not-read . "You couldn't read the article (%#1n).\n")
    (multiple-choice . "There are several alternatives.")

    (what-mark-to-list . "List which mark? ")
    (you-have-marks . "You have %#1d texts marked with %#2d.\n")
    (you-have-marks-all . "You have %#1d marked texts.\n")

    (does-not-exist . "Unknown command.") ; Only people fixing bugs or recieving bug-reports should change these:
    (summary-line . "%=-8#1n%#2s%4#3d  %[%#4@%#5:P%]  %[%#6@%#7r%]\n")

    ; Only people fixing bugs or recieving bugg-reports should change these:
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
    (new-super-conf-q . "Which conferece do you want as superconference? ")
    (new-permitted-submitters-q . "Allow members of which conference as authors in %#1s? (all) ")
    (super-conf-for-is . "Changing superconference of %#1M to %#2M...")
    (permitted-submitters-removed-for-conf . "Allowing all authors to conference %#1M...")
    (submitters-conf-for-is . "Changing authors admitted to conference %#1M to the members of %#2M...") 
   
    (conf-to-set-garb-nice-q . "Set expiration time for which conference? ")
    (new-garb-nice-q . "After how many days shall articles be removed? ")
    (garb-nice-for-is . "Changing expiration for %#1M to %#2d...")

    (really-shutdown . "Are you sure you want to shut down the server? ")
    (closing-server . "Shutting down the server...")
    (really-sync . "Are you sure you want to save the database? ")
    (syncing-server . "Saving the database...")
    (administrator . "administrator")
    (no-longer-administrator . "a regular user again")
    (you-are-now . "Ok, you are now running as %#1s.\n")
    (setting-motd . "Changing login message to article %#1n.\n")
    (set-motd-success . "You have set a new login message.\n")
    (set-motd-failed . "Didn't work. Perhaps you were not an administrator.\n")
    (removing-motd . "Removing the login message.\n")
    (removed-motd . "You have removed the login message.\n")
    (who-to-throw-out . "Which session do you want to kill? ")
    (throwing-out . "Killing session %#1d... ")
    (postpone-prompt . "How much do you want to read now? ")
    (set-session-priority . "Set reading level: ")

    ; From review.el:
    (no-review-done . "You need to review something before you can review more.\n")
    (review-how-many . "Review how many?")
    (review-how-many-more . "Review how many more?")
    (latest-n . "last %#1d")
    (first-n . "first %#1d")
    (info-by-whom . "%#1s by whom: ")
    (info-to-conf . "%#1s to conference: ")
    (info-by-to . "%#1s by %#2P to %#3M forward.")
    (all-confs . "all conferences")
    (no-get-conf . "You are not allowed to access that conferene.\n")
    (no-get-pers . "You are not allowed to access that user.\n")
    (no-review-info . "You are not allowed to review %#1s\n")
    (review-info . "Review %#1s")
    (review-info-by-to . "Review %#1s by %#2P to %#3M forwards.\n")
    (review-more-info-by-to . "Review %#1s by %#2P to %#3M forwards.\n")
    (review-rest . "the rest")
    (review-more . "%#1d more")
    (you-review . "You are now reviewing %#1s.\n")
    (read-text-first . "You must read a article first.\n")
    (cannot-read-last-text . "You cannot review the last read article.\n")
    (review-n-texts . "Review %#1d articles.\n")
    (review-marked . "Review %#1d marked articles.\n")
    (review-text-no . "Review article %#1n\n")
    (review-many-comments . "Review %#2?d%[one%]%[%#2d%] comment%#2?d%[%]%[s%] to article %#1n.\n")
    (view-many-comments . "Read %#2?d%[one%]%[%#2d%] comment%#2?d%[%]%[s%] to %#1n.\n")
    (view-texts-in-conf . "Grand total of %#1d articles to read in %#2M.\n")

    (not-reading-anywhere . "You are not reading in any conference.\n")
    (read-normally-read . "How many articles to you want to read again? ")

    (review-conf-gone . "The conference does not exist.\n")
    (review-pers-gone . "The user does not exist.\n")
    (review-cant-read-conf . "You can't review articles to a closed conference you are not a member of.\n")
    (review-cant-read-letterbox . "You can't review articles to somebody else's mailbox.\n")
    (review-cant-read-empty . "The conference is empty.\n")
    (cant-review-everything . "You cannot review every artible in LysKOM.\n")

    (more-than-one-root . "Article %#1n has more than one root.\n")
    (more-than-one-root-review . "\
Article %#1n has more than one root but only one of the trees will be shown.\n")

    ; From edit-text.el:
    (press-C-c-C-c . "Enter C-c C-c to post the article.")
    (recipient . "Recipient:")
    (recipient-prefix . "[Rr]")
    (carbon-copy . "Carbon copy:")
    (blank-carbon-copy . "Blind Carbon copy:")
    (carbon-copy-prefix . "[Cc]\\([Aa]\\|[Cc]\\)")
    (blank-carbon-copy-prefix . "[Bb]\\([Ll]\\|[Cc][Cc]\\)")

    (secret-aux-flag . "secret")
    (anonymous-aux-flag . "anonymous")
    (inherit-aux-flag  . "inherited")
    (aux-item-prefix . "> ")

    (header-subject . "Subject: ")
    (header-separator . "\\<lyskom-edit-mode-map>\
--- Write below. \
Post: \\[kom-edit-send], \
Kill: \\[kom-edit-quit], \
Help: \\[describe-mode] ---")
    (text-mass . "%#4s%#1s\n%#2s\n%#3s")
    (comment-to-by . "%#1s to article %#2n%#3s.\n")
    (already-sent . "You have already posted this article. Post it anyway? ")
    (subject . "Subject: ")
    (subject-prefix . "[Ss]")
    (enter-subject-idi . "Enter a subject.")
    (which-text-include . "Include which article? ")
    (added-recipient . "Recipient: ")
    (added-carbon-copy . "Carbon copy to conference: ")
    (added-blank-carbon-copy . "Blind carbon copy to conference: ")
    (text-to-comment-q . "Which article to you want to comment? ")
    (conf-has-motd-no . "The conference has a note on the door. (%#1d)\n\n%#2s")
    (still-want-to-add . "Do you still want to add the conference as a recipient? ")
    (could-not-create-text . "\nCouldn't create the article. Error: %#2s.\n")
    (no-get-text . "You were not allowed to retrieve the article.")
    (unknown-header . "Unknown header")
    (transform-error . "Skicka in oformatterat (%#1s)? ")

    ; From view-text.el:
    (line . " /1 line/ ")
    (lines ." /%#1d lines/ ")

    (marked-by-you . "Marked by you.\n")
    (marked-by-you-and-others . "Marked by you and %#1?d%[someone else%]%[%#1d others%].\n")
    (marked-by-several . "Marked by %#1d user%#1?d%[%]%[s%].\n")

;; The format of this function should coincide with the format of the 
;; lyskom-text-start variable. DONT change one without changing the other.

    (time-yyyy-mm-dd-hh-mm . "%4#1d-%02#2d-%02#3d %02#4d:%02#5d")

; used by lyskom-print-time
    (time-y-m-d-h-m . "%4#1d %02#3d/%02#2d  %02#4d:%02#5d ")
    (today-time-format-string . "%#6s %02#4d:%02#5d")
    (yesterday-time-format-string . "%#6s %02#4d:%02#5d")
    (today . "today")
    (yesterday . "yesterday")
    (no-such-text-no . "The article doesn't exist. (%#1:n)\n")
    (time-yyyy-mm-dd . "%4#1d-%02#2d-%02#3d")

    (head-Subject . "Subject: ")
    (Recipient . "Recipient")
    (Extra-recipient . "CC")
    (Hidden-recipient . "BCC")
    (Strange-recipient . "Also to")
    (send-at . "   Posted:     %#1s\n")
    (sent-by . "   Posted by %#1P\n")
    (recieved-at . "    Received: %#1s\n")

    (comment-to-text . "Comment to article %#1n")
    (footnote-to-text . "Footnote to article %#1n")
    (comment-in-text . "Comment in article %#1n")
    (footnote-in-text . "Footnote in article %#1n")

    (comment-to-text-by . "Comment to article %#1n by %#2P")
    (footnote-to-text-by . "Footnote to article %#1n by %#2P")
    (comment-in-text-by . "Footnote in article %#1n by %#2P")
    (footnote-in-text-by . "Footnote in article %#1n by %#2P")

    (written-by . " by %#1P\n")

    ; From async.el:

    (name-has-changed-to-name . "%#1:P has changed name to %#2:P")
    (name-has-changed-to-name-r . "%[%#3@%#1:P%] has changed name to %[%#3@%#2:P%]\n")
    (you-changed-name-to . "You have now changed your name to %[%#2@%#1:P%].\n")
    (database-sync . "Synching database.")

    (lyskom-is-full . "\
===========================================================
Message from the LysKOM-system: Somebody tried to connect,
but failed since all connections available to LysKOM are in
use. Please leave and return later if you are just waiting
for an article.
===========================================================\n")
    (lyskom-is-full-long . "\
===============================================================================
Message from the LysKOM-system: Somebody tried to connect,
but failed since all connections available to LysKOM are in
use. Please leave and return later if you are just waiting
for an article.
===============================================================================
")
    (has-entered . "%#1:P has entered %#2s.")
    (has-entered-r . "%#2@%#1P has entered %#3s.\n")
    (has-left . "%#1:P has left %#2s.")
    (has-left-r . "%#2@%#1P has left %#3s.\n")
    (unknown . "unknown")

    (no-longer-member . "You are no longer a member of %#1M.\n")
    (no-longer-member-n . "You are no longer a member of %#1m (conference is gone.)\n")
    (have-become-member . "You have become a member of %#1M.\n")
    (have-become-passive-member . "You have become a passive member of %#1M.\n")


    (message-broadcast . "\
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Alarm from %#1P (%#3s):

%#2t
----------------------------------------------------------------
")
    (message-broadcast-long . "\
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Alarm from %#1P (%#3s):

%#2t
-------------------------------------------------------------------------------
")
    (message-from . "\
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Personal message from %#1P (%#3s):

%#2t
----------------------------------------------------------------
")
    (message-from-long . "\
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Personal message from %#1P (%#3s):

%#2t
-------------------------------------------------------------------------------
")
    (message-from-to ."\
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Group message to %#3M\nfrom %#2P (%#4s):

%#1t
----------------------------------------------------------------
")
    (message-from-to-long ."\
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Group message to %#3M\nfrom %#2P (%#4s):

%#1t
-------------------------------------------------------------------------------
")
    (text-is-created . "Article %#1n has been created!")

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
    (goto-priority-prompt . "Got to priority: ")
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
    (too-high-goto-2 . "You are too high up, move down to line two.")
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
    (could-not-create-area . "Couldn't create the article.\n")
    (could-not-set-user-area . "Couldn't alter the user-area. The server says error: %#1d\n")
    (you-dont-exist . "You don't exist. Go away.\n")
    (error-in-options . "There was an error in one of your variables (%#1s)
It was set to \"%#2s\" in the user area. It is set to nil instead.
Send a bug report.\n")

    ; From elib-string.el:
    ; No entries.

    ; From lyskom-rest.el:

    (mode-line-unread . " Unread ")
    (mode-line-letters . "letters ")

    (error-code . "Error code %#2d: %#1s.\n")
    (error-in-kom-do-when-done . "The variable kom-do-when-done has an erroneous value.
You should set it to a better value.\n")
    (extended-command . "LysKOM: ")
    (wait-for-server . "LysKOM is waiting for a response from the server. Wait for the prompt.\n")
    (review-text-q . "Review article: ")

    (completely-read-conf . "You have seen all the articles in this conference.\n")
    (not-in-any-conf . "You are not in a conference now.\n")

    (all-conf-unread-r . "You have nothing unread.\n")
    (all-conf-unread-s . "You have nothing unread. ")
    (one-unread . "%#1M - one unread article\n")
    (several-unread . "%#1M - %#2d unread articles\n")
    (enter-conf . "%#1M\n")

    (save-on-file-q . "Save which article in file: (%#1s) ")
    (wait-for-prompt . "Wait for the prompt.")

    (conference-no . "<möte %#1d>")
    (person-no . "<person %#1d>")
    (prompt-several-messages . "(%d messages)")
    (prompt-single-message . "(%d message)")

    (go-to-pri-conf-prompt . "Go to next prioritized conference")
    (read-pri-text-conf . "Read next prioritized article")
    (review-next-text-prompt . "Review next article")
    (review-next-comment-prompt . "Review next comment")
    (review-next-marked-prompt . "Review next marked article")
    (read-next-letter-prompt . "Read next letter")
    (read-next-footnote-prompt . "Read next footnote")
    (read-next-comment-prompt . "Read next comment")
    (read-next-text-prompt . "Read next article")
    (go-to-conf-of-marked-prompt . "Resume reviewing marked")
    (go-to-conf-of-review-tree-prompt . "Resume reviewing comments")
    (go-to-conf-of-review . "Resume reviewing texts")
    (go-to-next-conf-prompt . "Go to next conference")
    (go-to-your-mailbox-prompt . "Go to your mailbox")
    (the-command . "Command: %#1C")
    (error-in-login-hook . "There was an error in your kom-login-hook: %#1s\n")

    (give-a-number . "Enter a number: ")

    (yes-regexp . "\\`[yY][eE][sS]\\'")
    (no-regexp . "\\`[nN][oO]\\'")
    (yes-or-no-nag . "Answer yes or no.")
    (yes-or-no . "(yes or no) ")

    (y-or-n-instring . "yYnN")
    (j-or-n-nag . "Answer y or n. ")
    (j-or-n . "(y or n) ")
    (y-instring . "yY")

    (person-does-not-exist . "User %#1d (does not exist).")
    (conference-does-not-exist . "Conference %#1d (does not exist).")
    (conf-no-does-not-exist-r . "Conference %#1d does not exist.")
    (person-is-anonymous . "Anonymous user")

    (process-signal . "Signal from the process.")
    (dead-session . "No active LysKOM session.")
    (closed-connection . "
**************************************************
%#2s
LysKOM session killed abnormally
Error message: %#1s**************************************************")
    (error-not-found . "Error code %#1d. No explanation available.")

    ; Useful in more place than one:
    (illegal-command . "Illegal command.\n")
    (no-such-text . "The article doesn't exist.\n")
    (no-such-text-m . "The article doesn't exist.")
    (nobody . "nobody")
    (everybody . "everyone")
    (everything . "everything")
    (anybody . "anyone")
    (forward . "forward")
    (backward . "backward")
    (wait . "Wait a moment...\n")
    (comment . "Comment")
    (comment-prefix . "[Cc][Oo]")
    (footnote . "Footnote")
    (footnote-prefix . "[Ff]")
    (by . " by %#1P")
    (text-created .  "Article %#1n has been created.\n")

    (resolve-session . "Which session: ")

    (starting-program . "Starting %#1s...")
    (super-jump . "Filtering subject \"%#1r\" in conference \"%#2M\"\n")
    (no-recipient . "There are no recipients for this article.\n")
    (filtered . "[Filtered]")
    (filter-error-specification . "Error in the filter specification")
    (filter-error-bad-not . "Error in the filter specification after 'not'")
    (filter-error-unknown-key . "The filter key '%S' is unknown.")
    (filter-error-key-arg . "Bad filter data (%S %S)")
    (filter-tree . "Skipping article %#1n \"%#2r\" by %#3P and all its comments.\n")
    (filter-text . "Skipping article %#1n \"%#2r\" by %#3P.\n")
    (filter-permanent . "Permanent? ")
    (filter-action . "Filter how? ")
    (filter-in-conf . "In which conference? (all) ")
    (filter-subject . "Filter which subject? ")
    (filter-which-text . "Filter articles containing: ")
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
    (filter-edit-remove-empty . "Empty filters cause all articles to be filtered. Do you want to remove these? ")
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
    

    (text-popup-title . "Article %#1s")
    (conf-popup-title . "Conference %#1s")
    (pers-popup-title . "User %#1s")
    (url-popup-title  . "URL %#1s")
    (aux-popup-title  . "Extra information")
    (generic-popup-title . "%#1s")

    (who-i-am-not-present . "%#1P (not in any conference) \n")
    (who-i-am-present . "%#1P is present in %#2M\n")
    (who-i-am-client . "The programs is lyskom.el, version %#1s.\n")
    (who-i-am-server . "This is %#1s, version %#2s.\n")
    (who-i-am-emacs . "Running under %#1s.\n")

    (no-such-session-r . "That session does not exist. Perhaps the user is not logged on.\n")
    (person-not-logged-in-r . "%#1P is not logged on.\n")
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
    (days . "days")
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

    (keyboard-menu-help . "Next: SPC  Prev: DEL  Select: RET  Cancel: ESC")

    ;; From slow.el
    (no-such-command . "There is no such command.\n")
    (command-completions . "You may mean one of the following:\n %#1s\n")

    (which-language . "Change language to: ")
    (send-formatted . "Send as formatted text? ")
    (changing-language-to . "Changing to %#1s.\n")
    (language-set-to . "Language set to %#1s.\n")
    (language-not-loaded . "%#1s is unavailable.\n")

;;    (reformat-generic . "(%#1s)")
    (reformat-html . "HTML")
    (reformat-enriched . "enriched")
    (reformat-filled . "filled")

    (reformat-truncated . "truncated")

    (need-library . "The \"%#1s\" package is required for this command.\n")
    (calc-expression . "Expression: ")

    (do-send-anonymous . "Send the article anonymously? ")
    (anonymous . "anonymous")
    (Anonymous . "Anonymous")

    ;; Some Help

    (where-is-doesnt-exist . "The command %#1s does not exist")
    (where-is-on-no-key . "%#1s is not on any key")
    (where-is-on-key . "%#1s is on %#2s")

    ;; From aux-items.el

    (content-type-aux . "Content type: %#1s")
    (content-type-regexp . "Content type: \\(\\S-+\\)")
    
    (agreeing . "Agreeing with article %#1n...")
    (fast-replying . "Quick reply to article %#1n...")
    (fast-reply-aux . "\"%#1t\" /%#2P/")
    
    (faq-for-conf-aux . "FAQ for %#1M <%#1m>") 
    (faq-in-text-aux . "FAQ in article:                 %10#1n")
    (conf-to-add-faq . "Which conference to you want to add a FAQ for? ")
    (text-to-add-as-faq . "Which article is the new FAQ? ")
    (adding-faq . "Adding article %#1n as a FAQ for %#2M...")
    (conf-to-del-faq . "Which conference do you want to remove a FAQ from? ")
    (text-to-del-as-faq . "Which article do you want to remove as FAQ? ")
    (deleting-faq . "Removinb article %#1n as FAQ for %#2M...")
    (conf-has-no-faq . "%#1M has no FAQ\n")
    (view-which-faq . "View FAQ for which conference? ")
    (review-faq-for-r . "View FAQ for %#1M.\n")
    
    (cross-reference-text-status-aux . "See article:              %10#1n /%#2P/")
    (cross-reference-conf-status-aux . "See conference:                         <%#1m> %#1M /%#2P/")
    (cross-reference-pers-status-aux . "See person:                             <%#1p> %#1P /%#2P/")
    (strange-cross-reference-status .  "See also:                                %#1s (whatever that means)")
    (cross-reference-text-aux . "See article %#1n /%#2P/")
    (cross-reference-conf-aux . "See conference <%#1m> %#1M /%#2P/")
    (cross-reference-pers-aux . "See person <%#1p> %#1P /%#2P/")
    (cross-reference-text-regexp . "See article \\([0-9]+\\)")
    (cross-reference-conf-regexp . "See conference <\\([0-9]+\\)>")
    (cross-reference-pers-regexp . "See person <\\([0-9]+\\)>")
    (strange-cross-reference . "See %#1s, whatever that might mean")

    (no-comments-aux . "The author has requested others not to comment this article")
    (no-comments-edit-aux . "Request that others do not comment")
    (no-comments-regexp . "Request that others do not comment")
    
    (personal-comment-aux . "The author requests private replies only")
    (personal-comment-edit-aux . "Request private replies only")
    (personal-comment-regexp . "Request private replies only")
    
    (request-confirmation-aux . "%#1P requests confirmation of reading")
    (request-confirmation-edit-aux . "Request confirmation of reading")
    (request-confirmation-regexp . "Request confirmation of reading")
    (confirm-read-q . "Confirm reading article %#1n? ")
    
    (read-confirm-aux . "Confirmed reading: %#1P %#2s")

    (redirect-email-aux . "Send e-mail to %#1s")
    (redirect-lyskom-aux . "Send articles to conference %#1M")

    (label-what-kind . "Set personal label on what (article, conference, user)? ")
    (label-what-text . "Set personal label on text: ")
    (label-what-conf . "Set personal label on which conference? ")
    (label-what-pers . "Set personal label on which person? ")
    (label-what-label . "Personal label: ")
    (label-secret    . "Should others to be able to see the label? ")

    (creating-software-aux . "Created with %#1s")

    (cant-get-aux-item . "Can't find extra information")
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

    (aux-item-for . "Extra information for ")
    (conference-no . "conference <%#1m> %#1M")
    (text-no . "article %#1n")

    (what-fast-reply-no . "Quick reply to which article? ")
    (fast-reply-prompt . "Reply: ")
    (agree-prompt . "Text: ")
    (default-agree-string . "I agree")
    (what-agree-no . "Agree with which article? ")


    ;; Aux-item stuff from edit-text

    (person . "User")
    (conference . "Conference")
    (text . "Article")

    (xref-type . "What to you want to refer to (article, conference or person)? ")
    (which-text-to-xref . "Add reference to text: ")
    (which-text-to-xref-err . "Text not found. Add reference to text: ")
    (which-pers-to-xref . "Add reference to user: ")
    (which-conf-to-xref . "Add reference to conference: ")

    (no-comments-q . "Author requestes no comments. Comment anyway? ")
    (private-answer-q . "Author requests private replies. Write private reply? ")

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
    ))


;;; ================================================================
;;;              The commands and their associated functions


;;; Formely known as lyskom-commands
(lyskom-language-strings lyskom-command en
  '(
    (describe-mode            . "Help")
    (kom-slow-mode            . "Long commands")
    (kom-quick-mode           . "Short commands")
    (kom-send-message         . "Send message")
    (kom-send-alarm           . "Send alam")
    (kom-create-conf          . "Create conference")
    (kom-delete-conf          . "Delete conference")
    (kom-delete-text          . "Remove article")
    (kom-display-time         . "Time")
    (kom-go-to-conf           . "Go (to) conference")
    (kom-go-to-next-conf      . "(Go to) next conference")
    (kom-jump                 . "Skip (all) comments")
    (kom-list-conferences     . "List conferences") 
    (kom-list-persons         . "List users")
    (kom-list-news            . "List news")
    (kom-list-re              . "List (using) regexps")
    (kom-membership           . "List memberships")
    ;; (kom-list-marks	      . "List marks")
    (kom-postpone             . "Postpone reading")
    (kom-set-session-priority . "Set reading level")
    (kom-prioritize           . "Prioritize conferences")
    (kom-status-person        . "Status (of) user")
    (kom-status-conf          . "Status (of) conference")
    (kom-add-self             . "Join (a) conference")
    (kom-list-summary         . "List article (subjects)")
    (kom-sub-self             . "Leave (a) conference")
    (kom-quit                 . "Quit")
    (kom-recover              . "Recover") 
    (kom-start-anew           . "New User")
    (kom-view                 . "Review article")
    (kom-find-root-review     . "Review tree")
    (kom-review-comments      . "Review all comments")
    (kom-review-tree          . "Review all comments recursively")
    (kom-review-clear         . "Review and skip")
    (kom-review-last-normally-read
     			      . "Review again")
    (kom-review-noconversion  . "Review unconverted")
    (kom-review-next          . "Review next")
    (kom-find-root            . "Review original (article)")
    (kom-review-by-to         . "Review last")
    (kom-review-first         . "Review first")
    (kom-review-all           . "Review all")
    (kom-review-more          . "Review more")
    (kom-view-commented-text  . "Review (the) commented (article)")
    (kom-view-previous-commented-text
     			      . "Review (the) previously commented (article)")
    (kom-review-stack         . "Review stack")
    (kom-review-presentation  . "Review presentation")
    (kom-review-backward      . "(Review) Backwards")
    (kom-view-next-text       . "(Read) next article")
    (kom-who-is-on            . "Who is on") 
    (kom-who-is-on-in-conference
                              . "Who is on in conference") 
    (kom-who-am-i             . "Where (am) i")
    (kom-list-clients	      . "List clients")
    (kom-busy-wait            . "Wait (for news)")
    (kom-write-comment        . "(Write) comment")
    (kom-comment-previous     . "(Write) comment (to) previous article")
    (kom-write-footnote       . "(Write) footnote")
    (kom-private-answer       . "(Write) personal reply (by letter)")
    (kom-private-answer-previous
   . "(Write) personal (reply to) previous article (by letter)")
    (kom-set-unread     	      . "Only (the) last")
    (kom-write-text           . "Write (an) article")
    (kom-send-letter          . "Write (a) letter")
    (kom-change-name          . "Change name")
    (kom-change-parenthesis   . "Change parenthesis")
    (kom-change-password      . "Change password")
    (kom-change-supervisor    . "Change supervisor")
    (kom-change-presentation  . "Change presentation")
    (kom-get-appreciation     . "(Please) pat my head")
    (kom-get-abuse            . "(Please) kick my butt")
    (kom-mark-text            . "Mark (article)")
    (kom-unmark-text          . "Unmark (article)")
    (kom-review-marked-texts  . "Review marked (articles)")
    (kom-review-all-marked-texts . "Review all marked (articles)")
    (kom-add-recipient        . "Add recipient")
    (kom-add-copy             . "Add (recipient of) carbon copy")
    (kom-add-bcc	      . "Addera (recipient of) blind carbon copy")
    (kom-sub-recipient        . "Remove recipient")
    (kom-move-text            . "Move text")
    (kom-add-comment          . "Add comment")
    (kom-sub-comment          . "Remove comment")
    (kom-add-member           . "Add (a) member")
    (kom-sub-member           . "Remove (a) member")
    (kom-change-conf-motd     . "(Post) note (on the) door")
    (kom-set-garb-nice        . "Change expiration")
    (kom-set-super-conf       . "Change superconference")
    (kom-set-permitted-submitters . "Change allowed authors")
    (kom-unset-conf-motd      . "Remove note (from the door)")
    (kom-save-text            . "Save article (in file)")
    (kom-edit-options         . "Change options")
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

    (kom-status-session       . "Status (of a) session")
    (kom-customize            . "Customize LysKOM")
    (kom-next-kom             . "Next LysKOM")
    (kom-previous-kom         . "Previous LysKOM")
    (kom-next-unread-kom      . "Next unread LysKOM")
    (kom-change-language      . "Change language")
    (kom-calculate            . "Calculate")
    (kom-list-marks           . "List marked articles")
    (kom-where-is             . "Where (is the) command")
    (kom-fast-reply           . "Quick reply")
    (kom-agree                . "Agree")
    (kom-add-faq              . "Add FAQ")
    (kom-del-faq              . "Remove FAQ")
    (kom-review-faq           . "View FAQ")

    (kom-sub-footnote         . "Remove footnote")
    (kom-add-footnote         . "Add footnote")
    ))

(lyskom-language-var lyskom-language-codes en
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

(lyskom-language-strings lyskom-menu en
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
    (recievers . "Recievers")
    (commented . "Commented")
    (kom-edit-send . "Send")
    (kom-edit-send-anonymous . "Send anonymously")
    (kom-edit-quit . "Throw away") 
    (kom-ispell-message . "Check spelling")
    (kom-edit-add-recipient . "Add recipient")
    (kom-edit-add-copy . "Add carbon copy")
    (kom-edit-show-commented . "Review commented")
    (kom-edit-insert-commented . "Cite commented")
    (kom-edit-add-bcc . "Add blind carbon copy")
    (kom-edit-add-cross-reference . "Add cross reference")
    (kom-edit-add-no-comments . "Request no comments")
    (kom-edit-add-personal-comments . "Request personal replies")
    (kom-edit-add-read-confirm-request . "Request read confirmation")
    (kom-edit-move-text . "Move to new recipient")
))

;;(defvar lyskom-swascii-commands nil
;;  "The swascii-versions of lyskom-commands.")

(lyskom-language-var lyskom-onoff-table en
  '(("on" . on) ("off" . off)))

(lyskom-language-var lyskom-filter-predicate-list en
      '(("=" . nil) ("!=" . t)))

(lyskom-language-var lyskom-filter-what en
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

(lyskom-language-var lyskom-filter-actions en
      '((skip-text . "Skip")
        (dontshow . "Don't show")
        (skip-tree . "Skip comments")))
                               
;;(defvar lyskom-swascii-filter-actions nil
;;  "The swascii-versions of lyskom-filter-actions.")
;;(defvar lyskom-swascii-filter-what nil
;;  "The swascii version of lyskom-filter-what")


(lyskom-language-var lyskom-text-start en
  "[0-9]+ +\\(199[0-9]-[0-1][0-9]-[0-3][0-9]\\|today\\|yesterday\\) +[0-2][0-9]:[0-5][0-9] +/[0-9]+ line\\(s\\)?/ ")


(defconst lyskom-keybindings-missing nil)

(defvar lyskom-en-mode-map nil)
(lyskom-language-keymap lyskom-mode-map en lyskom-en-mode-map)

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
  (define-prefix-command 'lyskom-en-previous-prefix)
  (define-key lyskom-en-mode-map (kbd "A") 'lyskom-en-change-prefix)
  (define-key lyskom-en-mode-map (kbd "r") 'lyskom-en-review-prefix)
  (define-key lyskom-en-mode-map (kbd "f") 'lyskom-en-filter-get-prefix)
  (define-key lyskom-en-mode-map (kbd "n") 'lyskom-en-next-prefix)
  (define-key lyskom-en-mode-map (kbd "l") 'lyskom-en-list-prefix)
  (define-key lyskom-en-mode-map (kbd "s") 'lyskom-en-S-prefix)
  (define-key lyskom-en-mode-map (kbd "b") 'lyskom-en-previous-prefix)

  (define-key lyskom-en-mode-map (kbd (lyskom-keys 'button2up)) 'kom-button-click)
  (define-key lyskom-en-mode-map (kbd (lyskom-keys 'button3)) 'kom-popup-menu)
  (define-key lyskom-en-mode-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lyskom-en-mode-map (kbd "*") 'kom-button-press)
  (define-key lyskom-en-mode-map (kbd "+") 'kom-menu-button-press)
  (define-key lyskom-en-mode-map (kbd "TAB") 'kom-next-link)
  (define-key lyskom-en-mode-map (kbd "M-TAB") 'kom-previous-link)

  ;; These should be first in order to be last in the menu of alternatives.
  (define-key lyskom-en-mode-map (kbd "A ?") 'lyskom-help)
  (define-key lyskom-en-mode-map (kbd "r ?") 'lyskom-help)
  (define-key lyskom-en-mode-map (kbd "f ?") 'lyskom-help)
  (define-key lyskom-en-mode-map (kbd "n ?") 'lyskom-help)
  (define-key lyskom-en-mode-map (kbd "l ?") 'lyskom-help)
  (define-key lyskom-en-mode-map (kbd "s ?") 'lyskom-help)
  

  (define-key lyskom-en-mode-map (kbd "o")     'kom-set-unread)
  (define-key lyskom-en-mode-map (kbd "x")     'kom-extended-command)
  (define-key lyskom-en-mode-map (kbd "<SPC>") 'kom-next-command)
  (define-key lyskom-en-mode-map (kbd "<LFD>")  'kom-page-next-command)
  (define-key lyskom-en-mode-map (kbd "<RET>")  'kom-line-next-command)

  (define-key lyskom-en-mode-map (kbd "?")  'describe-mode)
  (define-key lyskom-en-mode-map (kbd "m")  'kom-send-letter)
  (define-key lyskom-en-mode-map (kbd "g")  'kom-go-to-conf)
  (define-key lyskom-en-mode-map (kbd "a")  'kom-write-text)
  (define-key lyskom-en-mode-map (kbd "c")  'kom-write-comment)
  (define-key lyskom-en-mode-map (kbd "C")  'kom-comment-previous)
  (define-key lyskom-en-mode-map (kbd "F")  'kom-write-footnote)
  (define-key lyskom-en-mode-map (kbd "p")  'kom-private-answer)
  (define-key lyskom-en-mode-map (kbd "P")  'kom-private-answer-previous)
  (define-key lyskom-en-mode-map (kbd "j")  'kom-jump)
  (define-key lyskom-en-mode-map (kbd "J")  'kom-super-jump)
  (define-key lyskom-en-mode-map (kbd "l M") 'kom-list-marks)
  (define-key lyskom-en-mode-map (kbd "l c") 'kom-list-conferences)
  (define-key lyskom-en-mode-map (kbd "l n") 'kom-list-news)
  (define-key lyskom-en-mode-map (kbd "l u") 'kom-list-persons)
  (define-key lyskom-en-mode-map (kbd "l r") 'kom-list-re)
  (define-key lyskom-en-mode-map (kbd "l s") 'kom-membership)
  (define-key lyskom-en-mode-map (kbd "l a") 'kom-list-summary)
  (define-key lyskom-en-mode-map (kbd "l f") 'kom-list-filters)
  (define-key lyskom-en-mode-map (kbd "l w") 'kom-who-is-on-in-conference)
  (define-key lyskom-en-mode-map (kbd "S")  'kom-add-self)
  (define-key lyskom-en-mode-map (kbd "M")  'kom-mark-text)
  (define-key lyskom-en-mode-map (kbd "U")  'kom-unmark-text)
  (define-key lyskom-en-mode-map (kbd "n a") 'kom-view-next-new-text)
  (define-key lyskom-en-mode-map (kbd "n c") 'kom-go-to-next-conf)
  (define-key lyskom-en-mode-map (kbd "n l") 'kom-next-kom)
  (define-key lyskom-en-mode-map (kbd "n u") 'kom-next-unread-kom)
  (define-key lyskom-en-mode-map (kbd "b l") 'kom-previous-kom)
  (define-key lyskom-en-mode-map (kbd "q")  'kom-quit)
  (define-key lyskom-en-mode-map (kbd "z")  'kom-bury)
  (define-key lyskom-en-mode-map (kbd "R")  'kom-recover)
  (define-key lyskom-en-mode-map (kbd "t")  'kom-display-time)
  (define-key lyskom-en-mode-map (kbd "f p") 'kom-get-appreciation)
  (define-key lyskom-en-mode-map (kbd "f k") 'kom-get-abuse)
  (define-key lyskom-en-mode-map (kbd "f s") 'kom-filter-subject)
  (define-key lyskom-en-mode-map (kbd "f a") 'kom-filter-author)
  (define-key lyskom-en-mode-map (kbd "f c") 'kom-filter-text)
  (define-key lyskom-en-mode-map (kbd "w")  'kom-who-is-on)
  (define-key lyskom-en-mode-map (kbd "I")  'kom-who-am-i)
  (define-key lyskom-en-mode-map (kbd "W")  'kom-busy-wait)
  (define-key lyskom-en-mode-map (kbd "A p") 'kom-change-presentation)
  (define-key lyskom-en-mode-map (kbd "A f") 'kom-filter-edit)
  (define-key lyskom-en-mode-map (kbd "A m") 'kom-change-auto-reply)
  (define-key lyskom-en-mode-map (kbd "A t") 'kom-move-text)
  (define-key lyskom-en-mode-map (kbd "r SPC") 'kom-view)
  (define-key lyskom-en-mode-map (kbd "r 0") 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map (kbd "r 1") 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map (kbd "r 2") 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map (kbd "r 3") 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map (kbd "r 4") 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map (kbd "r 5") 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map (kbd "r 6") 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map (kbd "r 7") 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map (kbd "r 8") 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map (kbd "r 9") 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map (kbd "r c") 'kom-view-commented-text)
  (define-key lyskom-en-mode-map (kbd "r C") 'kom-view-previous-commented-text)
  (define-key lyskom-en-mode-map (kbd "r a ?") 'lyskom-help)
  (define-key lyskom-en-mode-map (kbd "r a c") 'kom-review-comments)
  (define-key lyskom-en-mode-map (kbd "r a r") 'kom-review-tree)
  (define-key lyskom-en-mode-map (kbd "r j") 'kom-review-clear)
  (define-key lyskom-en-mode-map (kbd "r n") 'kom-review-next)
  (define-key lyskom-en-mode-map (kbd "r u") 'kom-review-noconversion)
  (define-key lyskom-en-mode-map (kbd "r o") 'kom-find-root)
  (define-key lyskom-en-mode-map (kbd "r l") 'kom-review-by-to)
  (define-key lyskom-en-mode-map (kbd "r f") 'kom-review-first)
  (define-key lyskom-en-mode-map (kbd "r A") 'kom-review-all)
  (define-key lyskom-en-mode-map (kbd "r M") 'kom-review-more)
  (define-key lyskom-en-mode-map (kbd "r g") 'kom-review-last-normally-read)
  (define-key lyskom-en-mode-map (kbd "r q") 'kom-review-faq)
  (define-key lyskom-en-mode-map (kbd "B")  'kom-review-backward)
  (define-key lyskom-en-mode-map (kbd "r s") 'kom-review-stack)
  (define-key lyskom-en-mode-map (kbd "r p") 'kom-review-presentation)
  (define-key lyskom-en-mode-map (kbd "r r") 'kom-find-root-review)
  (define-key lyskom-en-mode-map (kbd "r m") 'kom-review-marked-texts)
  (define-key lyskom-en-mode-map (kbd "r a m") 'kom-review-all-marked-texts)
  (define-key lyskom-en-mode-map (kbd "r a SPC") 'kom-review-all)
  (define-key lyskom-en-mode-map (kbd "r x a") 'kom-agree)
  (define-key lyskom-en-mode-map (kbd "r x q") 'kom-fast-reply)
  (define-key lyskom-en-mode-map (kbd "s c") 'kom-status-conf)
  (define-key lyskom-en-mode-map (kbd "s u") 'kom-status-person)
  (define-key lyskom-en-mode-map (kbd "s s") 'kom-status-session)
  (define-key lyskom-en-mode-map (kbd "s m") 'kom-send-message)

  ;; Running in) buffer

  (define-key lyskom-en-mode-map (kbd "M-p") 'backward-text)
  (define-key lyskom-en-mode-map (kbd "M-n") 'forward-text)
  (define-key lyskom-en-mode-map (kbd "s a") 'kom-save-text)

  (define-key lyskom-en-mode-map (kbd "C-?") 'scroll-down)
  (define-key lyskom-en-mode-map (kbd "<DEL>") 'scroll-down)
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
  (define-key lyskom-en-filter-edit-map "p" 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-en-filter-edit-map "P" 'lyskom-filter-edit-prev-entry)
  (define-key lyskom-en-filter-edit-map "n" 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-en-filter-edit-map "N" 'lyskom-filter-edit-next-entry)
  (define-key lyskom-en-filter-edit-map "\C-P" 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-en-filter-edit-map "\C-N" 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-en-filter-edit-map "\C-B" 'lyskom-filter-edit-prev-pattern)
  (define-key lyskom-en-filter-edit-map "\C-F" 'lyskom-filter-edit-next-pattern)
  (define-key lyskom-en-filter-edit-map "\M-p" 'lyskom-filter-edit-prev-entry)
  (define-key lyskom-en-filter-edit-map "\M-n" 'lyskom-filter-edit-next-entry)
  (define-key lyskom-en-filter-edit-map "d" 'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-en-filter-edit-map "\M-d" 'lyskom-filter-edit-delete-entry)
  (define-key lyskom-en-filter-edit-map "D" 'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-en-filter-edit-map "\C-D" 'lyskom-filter-edit-delete-pattern)
  (define-key lyskom-en-filter-edit-map "i" 'lyskom-filter-edit-insert-pattern)
  (define-key lyskom-en-filter-edit-map "I" 'lyskom-filter-edit-insert-pattern)
  (define-key lyskom-en-filter-edit-map "\M-i" 'lyskom-filter-edit-insert-entry)
  (define-key lyskom-en-filter-edit-map "<" 'lyskom-filter-edit-beginning-of-list)
  (define-key lyskom-en-filter-edit-map ">" 'lyskom-filter-edit-end-of-list)
  (define-key lyskom-en-filter-edit-map "\M-<" 'lyskom-filter-edit-beginning-of-list)
  (define-key lyskom-en-filter-edit-map "\M->" 'lyskom-filter-edit-end-of-list)
  (define-key lyskom-en-filter-edit-map "q" 'lyskom-filter-edit-quit)
  (define-key lyskom-en-filter-edit-map "x" 'lyskom-filter-edit-expunge)
  (define-key lyskom-en-filter-edit-map "s" 'lyskom-filter-edit-save)
  (define-key lyskom-en-filter-edit-map "g" 'lyskom-filter-edit-revert)
  (define-key lyskom-en-filter-edit-map "t" 'lyskom-filter-edit-toggle-permanent)
  (define-key lyskom-en-filter-edit-map "a" 'lyskom-filter-edit-toggle-action)
  (define-key lyskom-en-filter-edit-map "?" 'lyskom-filter-edit-brief-help)
  (define-key lyskom-en-filter-edit-map "h" 'lyskom-filter-edit-brief-help)
  )


;;;(if lyskom-prioritize-mode-map
;;;    nil
;;;  (setq lyskom-prioritize-mode-map (make-keymap))
;;;  (suppress-keymap lyskom-prioritize-mode-map)
;;;  (define-key lyskom-prioritize-mode-map "\C-?" 'previous-line)
;;;  (define-key lyskom-prioritize-mode-map " "    'next-line)
;;;  (define-key lyskom-prioritize-mode-map "\C-k" 'kom-prioritize-kill)
;;;  (define-key lyskom-prioritize-mode-map "\C-y" 'kom-prioritize-yank)
;;;  (define-key lyskom-prioritize-mode-map "p"     'kom-prioritize-set-priority)
;;;  (define-key lyskom-prioritize-mode-map "\C-c\C-c" 'kom-prioritize-quit)
;;;  (define-key lyskom-prioritize-mode-map "q"     'kom-prioritize-quit)
;;;  (define-key lyskom-prioritize-mode-map "Q"     'kom-prioritize-quit)
;;;  (define-key lyskom-prioritize-mode-map "u"     'kom-prioritize-move-up)
;;;  (define-key lyskom-prioritize-mode-map "d"     'kom-prioritize-move-down)
;;;)

(defvar lyskom-en-prioritize-mode-map nil)
(lyskom-language-keymap lyskom-prioritize-mode-map en lyskom-en-prioritize-mode-map)

(if lyskom-en-prioritize-mode-map 
    nil
  (setq lyskom-en-prioritize-mode-map (make-keymap))
  (suppress-keymap lyskom-en-prioritize-mode-map)
  (define-key lyskom-en-prioritize-mode-map (kbd (lyskom-keys 'button2up)) 'kom-button-click)
  (define-key lyskom-en-prioritize-mode-map (kbd (lyskom-keys 'button3)) 'kom-popup-menu)
  (define-key lyskom-en-prioritize-mode-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lyskom-en-prioritize-mode-map "*" 'kom-button-press)
  (define-key lyskom-en-prioritize-mode-map "+" 'kom-menu-button-press)
  (define-key lyskom-en-prioritize-mode-map "?" 'kom-prioritize-help)
  (define-key lyskom-en-prioritize-mode-map "\C-k" 'kom-prioritize-select)
  (define-key lyskom-en-prioritize-mode-map "\C-y" 'kom-prioritize-yank)
  (define-key lyskom-en-prioritize-mode-map " " 'kom-prioritize-select)
  (define-key lyskom-en-prioritize-mode-map "\C-m" 'kom-prioritize-next-line)
  (define-key lyskom-en-prioritize-mode-map "\C-j" 'kom-prioritize-next-line)
  (define-key lyskom-en-prioritize-mode-map "\C-?" 'kom-prioritize-previous-line)
  (define-key lyskom-en-prioritize-mode-map "\M-\C-?" 'kom-prioritize-deselect-all)
  (define-key lyskom-en-prioritize-mode-map [down] 'kom-prioritize-next-line)
  (define-key lyskom-en-prioritize-mode-map "\C-n" 'kom-prioritize-next-line)
  (define-key lyskom-en-prioritize-mode-map [up] 'kom-prioritize-previous-line)
  (define-key lyskom-en-prioritize-mode-map "\C-p" 'kom-prioritize-previous-line)
  (define-key lyskom-en-prioritize-mode-map "p" 'kom-prioritize-previous-line)
  (define-key lyskom-en-prioritize-mode-map [(meta up)] 'kom-prioritize-move-up)
  (define-key lyskom-en-prioritize-mode-map "\M-p" 'kom-prioritize-move-up)
  (define-key lyskom-en-prioritize-mode-map "u" 'kom-prioritize-move-up)
  (define-key lyskom-en-prioritize-mode-map [(meta down)] 'kom-prioritize-move-down)
  (define-key lyskom-en-prioritize-mode-map "\M-n" 'kom-prioritize-move-down)
  (define-key lyskom-en-prioritize-mode-map "d" 'kom-prioritize-move-down)
  (define-key lyskom-en-prioritize-mode-map "\M-<" 'kom-prioritize-beginning)
  (define-key lyskom-en-prioritize-mode-map "\M->" 'kom-prioritize-end)
  (define-key lyskom-en-prioritize-mode-map "r" 'kom-prioritize-reprioritize)
  (define-key lyskom-en-prioritize-mode-map "g" 'kom-prioritize-goto-priority)
  (define-key lyskom-en-prioritize-mode-map "p" 'kom-prioritize-set-priority)
  (define-key lyskom-en-prioritize-mode-map "s" 'kom-prioritize-save)
  (define-key lyskom-en-prioritize-mode-map "q" 'kom-prioritize-quit)
  (define-key lyskom-en-prioritize-mode-map "\C-c\C-c" 'kom-prioritize-quit)
  (define-key lyskom-en-prioritize-mode-map "\t" 'kom-next-link)
  (define-key lyskom-en-prioritize-mode-map "\M-\C-i" 'kom-previous-link)
)

(lyskom-language-var lyskom-prioritize-header-lines en 2)

(lyskom-language-var lyskom-prioritize-header en 
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
  (define-key lyskom-en-customize-map "\t" 'widget-forward)
  (define-key lyskom-en-customize-map "\M-\t" 'widget-backward)
  (define-key lyskom-en-customize-map "\C-m" 'widget-button-press)
  (define-key lyskom-en-customize-map (kbd (lyskom-keys 'button2up)) 'widget-button-click)
  (define-key lyskom-en-customize-map "\C-c\C-c" 'lyskom-customize-save-and-quit)
  (define-key lyskom-en-customize-map "\C-c\C-k" 'lyskom-customize-quit)
  (define-key lyskom-en-customize-map "\C-c\C-s" 'lyskom-customize-save)
  (define-key lyskom-en-customize-map "\C-c\C-a" 'lyskom-customize-apply)
)

(lyskom-language-strings lyskom-custom-strings en
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

    ;;
    ;; Help messages
    ;;

    (variable-type-help . "Select to save setting in the server.")
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
    (max-text-length . "For articles shorter than: ")

    (friends . "Friends and other special people")

    (turned-off .      "Turned off           ")
    (number-of-times . "A few times")
    (sound-file . "Audio file")

    (selected-mark . "Mark          ")
    (ask .           "Ask every time         ")

    (before . "Before the text")
    (after . "After the text")
    (depth-first . "In comment order")
    (time-order . "I time order")

    (express-break . "Immediately upon creation  ")
    (break .         "After current comment chain")
    (no-break .      "Efter current conference   ")

    (command . "Command")
    (command-list . "Command list")
    (some-persons . "For some persons")
    (name . "Name")

    (page-none . "Never                        ")
    (page-all .  "Before every command         ")
    (page-some . "Before the following commands")

    (ask-every-time . "Ask every time         ")
    (fixed-priority . "Fixed priority")

    (messages-in-lyskom-buffer . "In the LysKOM buffer  ")
    (discard-messages .          "Nowhere - discard them")
    (in-named-buffer .           "In a named buffer     ")

    (everybody-rcpt . "Everybody                              ")
    (group-rcpt .     "The recipient of the last group message")
    (sender-rcpt .    "The sender of the most recent message  ")

    (viewer-program . "Web browser")
    (no-viewer            . "(ingenting valt)")
    (default-viewer       . "Browse-URL (all)")
    (netscape-viewer . "Netscape Navigator (all)")
    (emacs-w3-viewer . "Emacs W3-mode (HTTP, Goper, FTP)")
    (emacs-general-viewer . "Emacs (FTP, Telnet, Mail)")
    (emacs-dired-viewer . "Emacs Dired (FTP)")
    (emacs-mail-viewer . "Emacs Mail-mode (Mail)")
    (emacs-telnet-viewer . "Emacs Telnet-mode (telnet)")
    (mosaic-viewer . "NCSA Mosaic (all)")
    (lynx-viewer          . "Lynx (alla)")

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
    (urls .               "URL Management\n")
    (personal-messages .  "Personal messages\n")
    (remote-control .     "Remote control of LysKOM\n")
    (hooks .              "Hook functions\n")

    (audio-cues .         "Audio cues\n")
    (audio-cues-when    . "Issue audio cues when:\n")
    (automatic-replies .  "Automatic replies\n")


    (audio-cues-doc . "\
  The following group of settings control how LysKOM issues audio cues in
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
  before sending an article to the server. The checks are designed to keep
  you from doing something stupid.

    Confirm multiple recipients
        If an article or comment has more than one recipient, LysKOM 
        can ask which of the recipients are relevant. This can either
        be done before you start writing the article, in which case 
        LysKOM will post a question for each recipient, or before sending
        the article to the server, in which case you may confirm all the
        recipients at once. It is also possible to turn this check off
        entirely.

    Check membership of commented author
        When on, LysKOM will check that the author of the comment you are
        writing is a member of at least one of the recipients of your comment.
        If not, LysKOM will offer to add the commented authos as a recipient
        to the comment you are writing.

    Check for unread comments
        When this is on, LysKOM will check that you have read all the other
        comments to the article you are commenting before sending your
        comment to the server. This is supposed to help you avoid duplicating
        someone else's comment.")

    (windows-doc . "\
  The following settings control how windows are created in LysKOM. The
  available options are:


    Some other window
        In another window, but in the same frame as LysKOM. If there only
        is one window in the frame, a new window will be created (and will
        be removed when you are finished.)

    Some other frame
        In a different frame than the one LysKOM is in. If there only is one
        frame, a new one will be created (and removed when you are finished.)

    In a new frame
        A new frame is created (and removed when you are finished.)

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

    (kom-emacs-knows-iso-8859-1-doc . "\
  Turned on means that LysKOM expects Emacs to understand ISO-8859-1. There
  is no point in turning this off.")

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
    %# - Inserts the current session number.
    %  - Inserts a space if it seems necessary.
    %% - Inserts a percent sign.

  Here are a few examples:

    \"%[%c% %m%] - \"             The default prompt
    \"%[%s: %c% %m%] - \"         Could display \"LysKOM: Se tiden - \"")

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
    %# - Inserts the current session number.
    %  - Inserts a space if it seems necessary.
    %% - Inserts a percent sign.

  Here are a few examples:

    \"%[%c% %m%] - \"             The default prompt
    \"%[%s: %c% %m%] - \"         Could display \"LysKOM: Se tiden - \"")
    
    (kom-cite-string-doc . "\
  A string that is inserted before each line in a cited text. Normally this
  is set to something like \"> \". Note that it is not customary to cite
  commented texts the way it is done in e-mail or Usenet News since the
  commented text is always available in LysKOM.")


    (kom-created-texts-are-read-doc . "\
  When this is on, self-created texts are autmatically marked as read. Turned
  off, those texts are presented as any other texts.")

    (kom-default-mark-doc . "\
  The default value used for marking texts. If no value is selected, LysKOM
  asks for a mark value every time. Values are between 1 and 255 are allowed.")

    (kom-reading-puts-comments-in-pointers-last-doc . "\
  Controls if comment links are listed before or after the body of a text.
  Normally comment links are listed after the text.

  Before:

    398331 1996-09-24  13:22  /2 lines/ George Berkeley
    Recipient: Philosophy <1226>
    Comment in article 398374 by John Locke
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
    Comment in article 398374 by John Locke
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
  to have been preformatted by the author are exempt from this treatment.")

    (kom-show-author-at-end-doc . "\
  When this is turned on the name of the author will be shown at the end
  of the article text. The name is also shown before the text as usual.

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

  If dashed lines are off the author's name will be shown as in this
  example, but the dashed lines are natually not displayed.")

    (kom-truncate-threshold-doc . "\
  If this is activated texts longer than the maximum number of lines will
  be truncated when you review comments, review marked texts or review the
  first or last texts in a conference.  Texts will not be truncated when
  you review a text, review a tree or read the next text.")

    (kom-truncate-show-lines-doc . "\
  The number of lines to show of truncated texts.")

    (kom-print-number-of-unread-on-entrance-doc . "\
  Determines whether the number of unread articles is shown when entering
  a conference:

  On:

    Go to next conference...
    Presentation (of nya) Members - 3 unread
    Read next article - 

  Off:

    Go to next conference...
    Presentation (of nya) Members
    Read next article - ")



    (kom-presence-messages-doc . "\
  If you want messages in the minibuffer when somebody logs in, logs out or
  changes name, turn this setting on. Messages are shown in the minibuffer.
  If you want messages about a limited number of users, specify which ones.
  To not get any messages at all, just specify an empty list.")

    (kom-presence-messages-in-buffer-doc . "\
  If you want messages in the  LysKOM buffer when somebody logs in, logs out
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
  Footnotes can be shown either as comments or immediately when the article
  they are footnotes to is displayed. This setting controls which behavior
  is used.")


    (kom-follow-comments-outside-membership-doc . "\
  LysKOM will normally not follow chains of comments into conference you are
  not a member of. If you do want to follow comment chains into other
  conferences, turn this setting on.")


    (kom-read-depth-first-doc . "\
  LysKOM can display articles either in the order they were written or in
  the order defined by the comment tree. For example, if articles 1003 and
  1004 are comments to article 1002, article 1006 is a comment to 1003 and 
  articles 1005 and 1007 are comments to 1004, the comment tree looks 
  something like this:
        
  1002 +-- 1003 --- 1006
       |
       +-- 1004 +-- 1005
                |
                +-- 1007

  Reading in order of creation will cause the articles to be displayed
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
  the names of users and conferences immeduately, but will delay printing
  to make time for other tasks. This improves response time in the client
  considerably, and should only be turned off if it causes problems.")


    (kom-higher-priority-breaks-doc . "\
  When articles are created in conferences that have a higher priority than
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
  The program must takea single argument, the name of the file to play.")

    (kom-default-message-recipient-doc . "\
  This setting controls who will be the default recipient of messages. The
  default recipient may either be everyone, i.e. a public message; the
  sender of the most recently received message; or the recipient of the most
  recently received group message or the sender of the most recently
  received personal message.")

    (lyskom-filter-outgoing-messages-doc . "\
  If this is on, message that are sent automatically, such as automatic
  replies and replies to remote control commands will be shown as if you
  had sent them manually.")

    (kom-friends-doc . "\
  The users named in this list will be displayed using a special face in
  the LysKOM buffer.")

    (kom-url-viewer-preferences-doc . "\
  This setting controls which WWW browser that will be used to open URLs
  found in LysKOM. If the first browser in the list cannor handle the type
  of URL being opened, then the next browser is tried, and so on.")

    (kom-mosaic-command-doc . "\
  This setting specifies the command to use to start NCSA Mosaic.")

    (kom-netscape-command-doc . "\
  This setting specifies the command to use to start Netscape.")

    (kom-inhibit-typeahead-doc . "\
  Key presses are usually buffered while LysKOM is busy, and are executed
  as soon as possible. With this setting off, LysKOM discard any key presses
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
  only for careful experts.)")

    (kom-remote-control-doc . "\
  When turned on, it is possible to control the session using remote control
  commands. Only those users listed below may issue the commands.")

    (kom-remote-controllers-doc . "\
  The users listed here are premitted to issue remove control commands
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
  This only works in Swedish. If you're running LysKOM in Swedish, turning
  this on causes today's names to be shown when you ask for the time.")

    ;;
    ;; Tags for variables
    ;;
    
    (kom-emacs-knows-iso-8859-1-tag . "Emacs can display ISO-8859-1:")
    (kom-bury-buffers-tag . "Bury buffers when changing LysKOM:")

  (kom-personal-messages-in-window-tag . "Personal messages:      ")
    (kom-customize-in-window-tag       . "LysKOM customization:   ")
    (kom-write-texts-in-window-tag     . "Author new articles:    ")
    (kom-prioritize-in-window-tag      . "Prioritize conferences: ")
    (kom-edit-filters-in-window-tag    . "Modify filters:         ")
    (kom-view-commented-in-window-tag  . "Review comments:        ")
    (kom-list-membership-in-window-tag . "List membership:        ")

    (kom-user-prompt-format-tag . "Prompt format:")
    (kom-user-prompt-format-executing-tag . "Prompt format when executing:")

    (kom-higher-priority-breaks-tag . 
"Read prioritized articles:                        ")
    (kom-created-texts-are-read-tag . 
"Automatically read created texts:                 ")
    (kom-default-mark-tag           . 
"Default mark:                                     ")
    (kom-print-number-of-unread-on-entrance-tag . 
"Show number of unread when entering a conference: ")
    (kom-follow-comments-outside-membership-tag .
"Follow comment chais outside membership:          ")
    (kom-show-footnotes-immediately-tag .
"Show footnotes immediately:                       ")
    (kom-membership-default-priority-tag . 
"Default priority for new memberships:             ")
    (kom-dashed-lines-tag . 
"Dashed lines around the article body:             ")
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

    (kom-presence-messages-tag . 
"Presence messages on or off:           ")
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
    (kom-ding-on-new-letter-tag        . "When a letter arrives:             ")
    (kom-ding-on-priority-break-tag    . "When a prioritized article arrives:")
    (kom-ding-on-wait-done-tag         . "When done waiting:                 ")
    (kom-ding-on-common-messages-tag   . "When a public message arrives:     ")
    (kom-ding-on-group-messages-tag    . "When a group message arrives:      ")
    (kom-ding-on-personal-messages-tag . "When a personal message arrives:   ")
    (kom-ding-on-no-subject-tag     .    "When you forget the subject line:  ")

    (lyskom-filter-outgoing-messages-tag . "Show automatic messages:")
    (kom-friends-tag . "Friends and other special people:")
    (kom-url-viewer-preferences-tag . "Open URLs using the following program:")
    (kom-mosaic-command-tag . "Command to start NCSA Mosaic:")
    (kom-netscape-command-tag . "Command to start Netscape Navigator:")

    (kom-cite-string-tag . "Quotation indicator: ")
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
    (kom-default-language-tag . "Default language:  ")
    (kom-show-namedays-tag    . "Show today's names:")
    (kom-ispell-dictionary-tag . "Spelling dictionary:")
    )
)






;;;; ============================================================
;;;; The default Ansaphone message goes here. The more complex 
;;;; message specification probably should too, but it's not here
;;;; yet. People who know how to use it are smart enough to do it
;;;; right.

(lyskom-language-var kom-ansaphone-default-reply en 
  "I am not reading LysKOM right not. Please write a letter instead.")
        
;;;; ============================================================
;;;; Other language-dependent variables
;;;;

(lyskom-language-var kom-ispell-dictionary sv
  "english")

;;;; ============================================================
;;;; Text buttom menuse go here. This will probably be moved back
;;;; to vars.el.in when the strings have been replaced by
;;;; symbols, but for now they'll stay here.


(lyskom-language-var lyskom-button-actions en
  '((text
     text-text
     lyskom-button-view-text
     (("View article" . lyskom-button-view-text)
      ("Review unconverted" . lyskom-button-review-noconversion)
      ("Review tree" . lyskom-button-review-tree)
      ("Review root article" . lyskom-button-find-root)
      ("Write commend" . lyskom-button-comment-text)
      ("Write personal reply" . lyskom-button-private-comment-text)
      ("Mark article" . lyskom-button-mark-text)
      ("Unmark article" . lyskom-button-unmark-text))
     nil
;     ((nil lyskom-print-text footer lyskom-button-comment-text))
     )
    (conf 
     conf-text
     lyskom-button-view-conf-presentation
     (("View presentation" . lyskom-button-view-conf-presentation)
      ("View conference status" . lyskom-button-view-conf-status)
      ("Go to conference" . lyskom-button-goto-conf)
      ("Send group message" . lyskom-button-send-message)
      ("Join conference" . lyskom-button-add-self)
      ("Leave conference" . lyskom-button-sub-self))
     ((kom-list-news . lyskom-button-goto-conf)))
    (pers 
     pers-text
     lyskom-button-view-pers-presentation
     (("View presentation" . lyskom-button-view-pers-presentation)
      ("View user status" . lyskom-button-view-pers-status)
      ("Send letter" . lyskom-button-mail)
      ("Send personal message" . lyskom-button-send-message))
     nil)
    (url 
     url-text
     lyskom-button-open-url
     (("Open" . lyskom-button-open-url)
      ("Copy" . lyskom-button-copy-url))
     nil)
    (info-node 
     info-node-text
     lyskom-button-goto-info-node
     (("Open" . lyskom-button-goto-info-node))
     nil)
    (email
     email-text
     lyskom-button-open-email
     (("Send mail" . lyskom-button-open-email)
      ("Copy" . lyskom-button-copy-email))
     nil)
    (aux
     aux-text
     lyskom-button-info-aux
     (("Information" . lyskom-button-info-aux)
      ("Delete" . lyskom-button-delete-aux)))
    (aux-edit-menu
     nil
     nil
     (("Toggle \"secret\"" . lyskom-edit-toggle-secret-aux)
      ("Toggle \"anonymous\"" . lyskom-edit-toggle-anonymous-aux)
      ("Toggle \"inherited\""   . lyskom-edit-toggle-inherit-aux)
      ("Delete"          . lyskom-edit-delete-aux)))
    (prioritize-flag-menu
     nil
     lyskom-prioritize-flag-toggle
     (("Toggle" . lyskom-prioritize-flag-toggle)
      ("Set"    . lyskom-prioritize-flag-set)
      ("Clear"  . lyskom-prioritize-flag-clear)))
))


;;;; ================================================================
;;;; Tell phrases should be configured with the default language used
;;;; at the server and not for person reading if they happens to
;;;; differ. This is of coarse because they are sent to the server for
;;;; everybody else to see.

;;;; Aronsson was here 4 DEC 1990, thus creating version 0.18
					; Created *-tell-*


;;; To coders of the elisp-client:
;;; You not only have to change the text here, you also have to modify
;;; kom-tell-phrases-validation-list in vars.el if you add or remove
;;; one of these.

(eval-when-compile (defvar kom-tell-phrases))

(lyskom-language-strings kom-tell-phrases en
  '((kom-tell-silence		. "") ; Why ?
    (kom-tell-send		. "Is trying to post an article.")
    (kom-tell-login		. "Is entering LysKOM.")
    (kom-tell-read		. "Is reading.")
    (kom-tell-1st-pres		. "Is writing the first presentation.")
    (kom-tell-write-comment	. "Is writing a comment.")
    (kom-tell-write-footnote	. "Is writing a footnote.")
    (kom-tell-write-letter	. "Is writing a letter.")
    (kom-tell-write-reply	. "Is writing a personal reply.")
    (kom-tell-write-text	. "Is writing an article.")
    (kom-tell-conf-pres		. "Is writing the presentation for a new conference.")
    (kom-tell-recover		. "Is restarting KOM. Sigh.")
    (kom-tell-wait		. "Is waiting.")
    (kom-tell-regret		. "Decides to throw away the article.")
    (kom-tell-review		. "Is reviewing.")
    (kom-tell-change-name       . "Takes on a new name.")
    (kom-tell-change-supervisor . "Changes the supervisor of something.")
    (kom-tell-next-lyskom	. "Moves to a different LysKOM.")
    (kom-tell-is-anonymous      . "Being secretive")))

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

(lyskom-language-var kom-mercial en (lyskom-get-string 'kom-tell-wait 
                                                       'kom-tell-phrases))


(lyskom-language-strings lyskom-error-texts en
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
    (error-14 . "There is no article with that number")
    (error-15 . "You cannot use global article number 0")
    (error-16 . "There is no article with that local number")
    (error-17 . "You cannot use local article number 0")
    (error-18 . "Name too short, to long or containing illegal characters")
    (error-19 . "Index out of bounds")
    (error-20 . "The conference already exists")
    (error-21 . "The user already exists")
    (error-22 . "Secret but not read-protected")
    (error-23 . "You are not allowed to change the erson/conference flag")
    (error-24 . "Error in the dtabase. Tough luck.")
    (error-25 . "Illegal misc-field. (Internal error)")
    (error-26 . "Illegal info type. (Bug in the client)")
    (error-27 . "Already recipient of this article")
    (error-28 . "Already comment to this article")
    (error-29 . "Already footnote to this article")
    (error-30 . "Not a recipient of this article")
    (error-31 . "Not a comment to this article")
    (error-32 . "Not a footnote to this article")
    (error-33 . "Too many recipients")
    (error-34 . "Too many commentsp")
    (error-35 . "Too many footnotes")
    (error-36 . "Too many marks")
    (error-37 . "You are not the author of that article")
    (error-38 . "You cannot connect to the server")
    (error-39 . "Out of memory")
    (error-40 . "The server is gone crazy")
    (error-41 . "The client thinks that the server says that it does not understand the client")
    (error-42 . "No such session")
    (error-43 . "Invalid regular expression")
    (error-44 . "Can't unmark an article that was not marked")
    (error-45 . "Temorary lossage. Please try again later")
    (error-46 . "Sending huge messages to the server is not a nice thing to do")
    (error-47 . "Anonymous texts are not accepted by all recipients")
    (error-48 . "Invalid extra information")
    (error-49 . "Change of extra information not permitted")
    (error-50 . "Unknown asynchronous message")
    (error-51 . "Internal server error")
    (error-52 . "Feature disabled in the server")
    (error-53 . "Unable to send message")
    (error-54 . "Invalid membership type")
))


     
(provide 'lyskom-strings)

;;; english-strings ends here
