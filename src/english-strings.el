;;;;;
;;;;; $Id: english-strings.el,v 44.11 1996-10-24 09:47:48 byers Exp $
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
              "$Id: english-strings.el,v 44.11 1996-10-24 09:47:48 byers Exp $"))



;;; ================================================================
;;; lyskom-edit-mode-map                             English version

(defvar lyskom-en-edit-mode-map nil)
(lyskom-language-keymap lyskom-edit-mode-map en lyskom-en-edit-mode-map)

;;; Set the keymap for lyskom-edit-mode

(defvar lyskom-sv-edit-prefix nil)

(if lyskom-en-edit-mode-map
    nil
  (setq lyskom-en-edit-mode-map (make-sparse-keymap))
  (define-prefix-command 'lyskom-edit-prefix)
  (define-prefix-command 'lyskom-edit-review-prefix)
  (define-prefix-command 'lyskom-edit-insert-prefix)
  (define-key lyskom-en-edit-mode-map "\C-c"	'lyskom-edit-prefix)
  (define-key lyskom-en-edit-mode-map "\C-c?"	'lyskom-help)
  (define-key lyskom-en-edit-mode-map "\C-cr"	'lyskom-edit-review-prefix)
  (define-key lyskom-en-edit-mode-map "\C-ci"	'lyskom-edit-insert-prefix)
  (define-key lyskom-sv-edit-mode-map [mouse-2] 'kom-mouse-2)
  (define-key lyskom-sv-edit-mode-map [down-mouse-3] 'kom-mouse-3)
  (define-key lyskom-sv-edit-mode-map [mouse-3] 'kom-mouse-null)
  (define-key lyskom-sv-edit-mode-map "\C-c*" 'kom-key-mouse-2)
  (define-key lyskom-en-edit-mode-map "\C-c\C-c"	'kom-edit-send)
  (define-key lyskom-en-edit-mode-map "\C-ck"	'kom-edit-quit)
  (define-key lyskom-en-edit-mode-map "\C-c\C-k"	'kom-edit-quit)
  (define-key lyskom-en-edit-mode-map "\C-cr?"	'lyskom-help)
  (define-key lyskom-en-edit-mode-map "\C-crc"	'kom-edit-show-commented)
  (define-key lyskom-en-edit-mode-map "\C-ci?"	'lyskom-help)
  (define-key lyskom-en-edit-mode-map "\C-cic"	'kom-edit-insert-commented)
  (define-key lyskom-en-edit-mode-map "\C-ci\C-y"	'kom-edit-insert-commented)
  (define-key lyskom-en-edit-mode-map "\C-ci1"	'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-mode-map "\C-ci2"	'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-mode-map "\C-ci3"	'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-mode-map "\C-ci4"	'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-mode-map "\C-ci5"	'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-mode-map "\C-ci6"	'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-mode-map "\C-ci7"	'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-mode-map "\C-ci8"	'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-mode-map "\C-ci9"	'kom-edit-insert-digit-text)
  (define-key lyskom-en-edit-mode-map "\C-ci "	'kom-edit-insert-text)
  (define-prefix-command 'lyskom-edit-add-prefix)
  (define-key lyskom-en-edit-mode-map "\C-ca" 'lyskom-edit-add-prefix)
  (define-key lyskom-en-edit-mode-map "\C-car" 'kom-edit-add-recipient)
  (define-key lyskom-en-edit-mode-map "\C-cac" 'kom-edit-add-copy)
  (define-key lyskom-en-edit-mode-map "\C-ca?" 'lyskom-help))


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
    (connection-done . "Connection established. Server version is %#1s.\n\n")
    (what-is-your-name . "What is your name? ")
    (password . "Your Password? ")
    (wrong-password . "Incorrect password\n")
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
on how to do this.")

    (is-name-correct . "Is the name %#1s correct? ")
    (personal-password . "Enter a personal password: ")
    (repeat-password . "Repeat for confirmation: ")
    (repeat-failure . "The passwords were not the same.\n")

    (could-not-create-you .  "LysKOM couldn't create that user.\n")
    (presentation-subject . "%#1s")
    (presentation-form . "Name:\t\nAddress:\t\n\t\nTelephone:
Email-address:\n\nOther:\t")
    (presentation-help . "You are writing your presentation.\n")
    (not-present-anywhere . "Not in any conference.")
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

    (what-text-to-delete . "Article to remove: ")
    (deleting-text . "Removing article %#1:n...")

    (presentation-for-whom . "Which conference/user: ")
    (somebody-deleted-that-conf . "Somebody just deleted that conference.\n")
    (review-presentation-of . "Review presentation of %#1M.\n")
    (has-no-presentation . "%#1:M has no presentation.\n")

    (have-to-read . "You must read an article first.\n")

    (no-comment-to . "There is no commented article.\n")

    (who-letter-to . "Send a letter to whom: ")
    (has-motd . "%#1P has a note on the door:\n\n")
    (motd-persist-q . "Send the letter? ")

    (who-to-add . "Whom do you want to add: ")
    (where-to-add . "To which conference: ")
    (where-to-add-self . "Join which conference: ")
    (priority-q . "Conference priority? (1 (low) - 255 (high)) ")
    (done . "done.\n")
    (nope . "didn't work.\n")

    (cant-find-supervisor . "Can't find supervisor of %#1M.\n")
    (is-read-protected-contact-supervisor . "%#1M is closed.
Send a letter to  %#2P to apply for membership.")

    (conf-does-not-exist . "\nThe conference doesn't exist.\n")

    (who-to-exclude . "Who do you want to remove: ")
    (where-from-exclude . "From which conference: ")

    (leave-what-conf . "Leave which conference: ")

    (error-fetching-person . "Error retreiving user.\n")
    (error-fetching-conf . "Error retreiving conference.\n")

    (name-of-conf . "Conference name: ")
    (anyone-member . "May anyone join? ")
    (secret-conf . "Secret conference? ")
    (comments-allowed . "Are comments allowed? ")
    (anonymous-allowed . "Are anonymous articles allowed? ")
    (what-comment-no . "Comment article number: ")
    (confusion-what-to-comment . "I can't figure out which article you want to comment.\n")
    (confusion-what-to-footnote . "I can't figure out to which article you want to write a footnote to.\n")
    (what-private-no . "Private reply to article number: ")

    (confusion-who-to-reply-to . "I can't figure out which article you want to write a private reply to.\n")
    (confusion-what-to-answer-to . "I can't figure out which article you want write a reply to.")
    (quit-in-spite-of-unsent . "You have an unsent article. Do you really want to quit? ")
    (confusion-what-to-view . "I can't figure out which article you want to view.\n")
    (really-quit . "Do you really want to quit LysKOM? ")
    (session-ended . "
--------------------------------------------
  LysKOM session finished
  You are now disconnected from the server
--------------------------------------------\n")
    (session-auto-ended . "
============================================================
Disconnecting from LysKOM since all connections are in use
and you have finished reading. Please come back later.
============================================================\n\n")
    (what-to-change-pres-you . "Change presentation of who/what (yourself): ")
    (who-to-put-motd-for . "Post note on the door of who/what (yourself): ")

    (cant-get-conf-stat . "Cannot get the status of that conference.")
    (go-to-conf-p . "Go to conference: ")
    (want-become-member . "Do you want to join? ")
    (no-ok . "Okiedokie, whatever you say.\n")

    (who-to-remove-motd-for . "Remove note from the door of who/what: ")

    (conf-all-read . "%#1M - no unread articles.\n")
    (no-in-conf . "You are not present in any conference.\n")

    (search-for-pers . "Enter search key (RETURN for all users) ")
    (search-for-conf . "Enter search key (RETURN for all conferences) ")
    (search-re . "Enter search regexp: ")
    (name-to-be-changed . "Name to change: ")
    (no-such-conf-or-pers . "The conference or user doesn't exist.\n")
    (new-name . "New name: ")
    (who-to-change-supervisor-for . "Change supervisor of who/what: ")
    (new-supervisor . "New supervisor: ")
    (text-to-mark . "Mark which article: ")
    (text-to-unmark . "Unmark which article: ")
    (what-mark . "Set which mark: ")
    (unmarking-textno . "Unmarking article %#1n...")
    (marking-textno . "Marking article %#1n...")

    (new-passwd-again . "Repeat the new password for confirmation: ")
    (what-mark-to-view . "Review which mark: ")
    (whos-passwd . "Change password for whom (yourself): ")
    (old-passwd . "Your old password: ")
    (new-passwd . "Your new password: ")
    (changing-passwd . "Changing password...")
    (retype-dont-match . "You didn't reenter the same passwrod. Try again.\n")
    (palindrome . "(a palindrome!) ")
    (lyskom-name . "User")
    (is-in-conf . "In conference")
    (from-machine . "At")
    (is-doing . "Activity")
    (lyskom-client . "Client")
    (text-to-add-recipient . "Add recipient to which article:")
    (text-to-add-copy . "Add recipient of carbon copy of which article:")
    (text-to-delete-recipient . "Remove recipient from which article:")
    (text-to-move . "Which text do you want to move: ")
    (text-to-add-comment-to . "Add comment to which article:")
    (text-to-delete-comment-from . "Remove comment from which article:")

    (where-on-list-q . "Placement in your list? (0-%#1d) ")
    (member-in-conf . "Joining to %#1M...")
    (add-member-in . "Adding %#1P as a member of %#2M...")
    (unsubscribe-to . "Leaving %#1M...")

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
    (time-is . "The time is %#1s %#2s(according to the server).")
    (time-format-exact . "%#7s %4#1d-%02#2d-%02#3d %02#4d:%02#5d:%02#6d")

    (xmaseve . "Christmas eve!")
    (xmasday . "Christmas day.\nDid you get any nice gifts this year?")
    (newyearday . "\nMay %#1d be a prosperous and good year for you!")
    (newyeareve . "Happy New Year!")
    (newyearevelate . "Less than an hour to go...")
    (lysbday . "
On this day, in 1973, the Lysator Academic Computer Society was
formed, and it was a great day in the history of computing in Sweden.
Read all about it at http://www.lysator.liu.se/history/")

    (total-users . "    A total of %#1d users.\n")
    (total-visible-users . "    A total of %#1d visible users.\n")
    (total-active-users . "    A total of %#1d active users.\n")
    (total-visible-active-users
     . "    A total of %#1d visible active users.\n")
    (who-to-add-q . "Add who/what as a recipient: ")
    (who-to-add-copy-q . "Add which conference/user as recipient of a carbon copy: ")
    (who-to-sub-q . "Remove who/what as a recipient: ")
    (who-to-move-from-q . "Move from where: ")
    (who-to-move-to-q . "Move to where: ")

    (adding-name-as-recipient . "Adding %#1M as recipient of article %#2n...")
    (adding-name-as-copy . "%#1M will receive a carbon copy of article %#2n...")
    (remove-name-as-recipient .
     "Removing %#1M as recipient of article %#2n...")
    (moving-name . "Moving text %#3n from %#1M to %#2M...")
    (text-to-add-q . "Add which article as a comment: ")
    (text-to-remove-q . "Remove which article as a comment: ")
    (add-comment-to . "Adding article %#1n as a comment to text %#2n...")
    (sub-comment-to . "Removing article %#1n as a comment to article %#2n...")
    (comment-keep-recpt-p ."Should %#1s remain a recipient? ")
    (comment-all-relevant-p . "There are multiple recipients. Are they all relevant? ")
    (please-edit-recipients . "Modify the recipient list and send the article again.")
    (add-recipient-p . "Add recipient %#1P? ")
    (checking-rcpt . "Checking recipients...")
    (checking-rcpt-done . "Checking recipients...done")

    (matching-regexp . "Conferences/users matching `%#1s'\n")

    (who-is-active-all . "Showing all sessions.\n")
    (who-is-active-last-minutes . "Showing all sessions active tha last %#1d minutes.\n")
    (showing-invisibles . "Showing invisible sessions.\n")
    (null-who-info . "No one (active) is logged on.\n")

    ; From commands2.el:

    (your-memberships . "Your LysKOM conference memberships:\n")
    (memberships-header . "Last access	   Prio	Unread	Conference\n")
    (memberships-line . "%16#1s  %#2d\t%#3d\t%#4M\n")
    
    (conf-for-status . "Get status of which conference: ")
    (no-such-conf . "The conference doesn't exist.\n")
    (status-record . "Status of conference %#1M (%#2m) %#3s\n\n")
    (change-type-prompt . "Change conference type for %#1M (%#2m) %#3s\n")
    (Mailbox . "Mailbox")
    (Protected . "Protected")
    (no-comments . "No comments")
    (closed . "Closed")
                   
    (created-by . "Created by person %24#1p %#3s(%#2P)\n")
    (created-at . "Created:%34#1s\n")
    (members .    "Number of members: %19#1d\n")
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

    (Everybody . "Everyone")
    (show-members-list-also-q . "List members? ")
    (show-membership-info-q . "Show number of unreads? ")
    (conf-has-these-members . "\n%#1M has the following members:\n")
    (member-list-header . "Last entered       Unread  Name\n\n")
    (secret-membership . "--- Secret line ---\n")
    (conf-membership-line . "%#1s%#2M\n")
    (pers-for-status . "Get status of which user: ")
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
    (marked-texts . "Articles marked:%26#1d")
    (time-for-last-letter . "Time of last letter:%22#1s (accordning to your cache)\n")
    (superconf . "Superconference: %25#1m %#3s(%#2M)\n")
    (supervisor . "Supervisor: %30#1p %#3s(%#2P)\n")
    (member-of-confs . "Member of (conferences):%18#1d\n")
    (presentation . "Presentation:    %25#1n\n")
    (show-membership-list-also-q . "List memberships? ")
    (not-allowed-see-confs . "You are not premitted to see which conferences %#1P is a member of.\n")
    (is-member-of . "\n%#1P is a member of the following conferences:\n")
    (membership-list-header . "Last access       Unread Conference\n\n")
    (pers-membership-line . "%#1s%#2s%#3M\n")
    (is-supervisor-mark . "O ")
    (who-to-send-message-to . "Send message to whom? (%s) ")
    (send-empty-message-p . "The message is empty. Send it anyway? ")
    (his-total-unread . "\n%#1M has a total of %#2d unread articles.\n")
    (message-prompt . "Message: ")
    (message-sent-to-user .
     "================================================================
Your message for %#2M:

%#1t
----------------------------------------------------------------
")
    (message-sent-to-all . 
     "================================================================
Your public message:

%#1t
----------------------------------------------------------------
")
    (message-all-info . "Send public message\n")
    (message-recipient-info . "Send message to %#1M\n")
    (message-nope .
     "Unable to send the message. Perhaps the recipient isn't logged on.
The message you were sending to %#1M was:
%#2t\n")
    (only-last . "Last (0 - %#1d) articles in %#2s: ")
    (only-error . "Something went wrong. Sorry.\n")
    
    (you-have-unreads . "You have %#1d unread articles in %#2M\n")
    (you-have-an-unread . "You have 1 unread article in %#1M\n")
    (you-have-unreads-special . "You have %#1d uncommented articles in %#2M\n")
    (you-have-an-unread-special . "You have 1 uncommented article in %#1M\n")
    (you-have-read-everything . "No news (is bad news).\n")
    (total-unreads . "\nYou have %#1d unread articles.\n")
    (total-unread . "\nYou have 1 unread article.\n")
    (waiting-for-anything .
     "You are waiting for an article in any conference.\n")
    (waiting-higher-than . 
     "You are waiting for an article in any conference with a priority higher than %#1d.\n")
    
    (have-to-be-in-conf-with-unread . "You must go to a non-empty conference first.\n")
    (Texts . "Article")
    (Date . "Date")
    (Lines . "Lines")
    (Author . "  Author")
    (Subject . "  Subject")
    (could-not-read . "You couldn't read the article (%#1n).\n")
    (multiple-choice . "There are several alternatives.")

    (what-mark-to-list . "List which mark: ")
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

    (conf-to-set-permitted-submitters-q . "For which conference do you want to set the allowed authors: ")
    (conf-to-set-super-conf-q . "Set superconference of which conference: ")
    (new-super-conf-q . "Which conferece do you want as superconference: ")
    (new-permitted-submitters-q . "Allow members of which conference as authors in %#1s (all): ")
    (super-conf-for-is . "Changing superconference of %#1M to %#2M...")
    (permitted-submitters-removed-for-conf . "Allowing all authors to conference %#1M...")
    (submitters-conf-for-is . "Changing authors admitted to conference %#1M to the members of %#2M...") 
   
    (conf-to-set-garb-nice-q . "Set expiration time for which conference: ")
    (new-garb-nice-q . "What do you want to set the expiration time to: ")
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
    (who-to-throw-out . "Which session do you want to kill: ")
    (throwing-out . "Killing session %#1d... ")
    (postpone-prompt . "How much do you want to read now: ")
    (set-session-priority . "Set reading level:")

    ; From review.el:
    (no-review-done . "You need to review something before you can review more.\n")
    (review-how-many . "Review how many: ")
    (review-how-many-more . "Review how many more: ")
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
    (review-one-comment . "Review one comment to article %#1n.\n")
    (review-many-comments . "Review %#2d comments to article %#1n.\n")
    (read-normally-read . "How many articles to you want to review: ")
    (review-conf-gone . "The conference does not exist.\n")
    (review-pers-gone . "The user does not exist.\n")
    (review-cant-read-conf . "You can't review articles to a closed conference you are not a member of.\n")
    (review-cant-read-letterbox . "You can't review articles to somebody else's mailbox.")
    (review-cant-read-empty . "The conference is empty.\n")
    (cant-review-everything . "You cannot review every artible in LysKOM.\n")

    ; From edit-text.el:
    (press-C-c-C-c . "Enter C-c C-c to post the article.")
    (recipient . "Recipient:")
    (recipient-prefix . "[Rr]")
    (carbon-copy . "Carbon copy:")
    (carbon-copy-prefix . "[Cc]\\([Aa]\\|[Cc]\\)")
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
    (which-text-include . "Include which article: ")
    (added-recipient . "Recipient: ")
    (added-carbon-copy . "Carbon copy to conference: ")
    (conf-has-motd-no . "The conference has a note on the door. (%#1d)\n\n%#2s")
    (still-want-to-add . "Do you still want to add the conference as a recipient? ")
    (could-not-create-text . "\nCouldn't create the article. Error: %#2s.\n")
    (no-get-text . "You were not allowed to retrieve the article.")
    (unknown-header . "Unknown header")


    ; From view-text.el:
    (line . " /1 line/ ")
    (lines ." /%#1d lines/ ")

    (marked-by-you . "Marked by you.\n")
    (marked-by-you-and-one . "Marked by dig and someone else.\n")
    (marked-by-you-and-several . "Marked by you and %#1d other users.\n")
    (marked-by-one . "Marked by 1 user.\n")
    (marked-by-several . "Marked by %#1d users.\n")

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

    (head-Subject . "Subject: ")
    (Recipient . "Recipient")
    (Extra-recipient . "CC")
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
    (has-entered . "%#1:P has entered LysKOM.")
    (has-entered-r . "%#2@%#1P has entered LysKOM.\n")
    (has-left . "%#1:P has left LysKOM.")
    (has-left-r . "%#2@%#1P has left LysKOM.\n")
    (unknown . "unknown")
    (secret-person . "Secret user")

    (message-broadcast .
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Public message from %#1P (%#3s):

%#2t
----------------------------------------------------------------
")
    (message-from . 
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Personal message from %#1P (%#3s):

%#2t
----------------------------------------------------------------
")
    (message-from-to .
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Group message to %#3M\nfrom %#2P (%#4s):

%#1t
----------------------------------------------------------------
")    (text-is-created . "Article %#1n has been created!")

    ; Used in mode-line-process
    (mode-line-waiting . ": waiting")
    (mode-line-working . ": working")
    (mode-line-saving . ": saving")
    (mode-line-down . ": down")

    ; From completing-read.el:

    (person-or-conf-no-regexp . "\\`[ \t]*[mpMP]\\w*[ \t]+\\([0-9]+\\)\\'")
    (session-no-regexp . "\\`[ \t]*[sS]\\w*[ \t]+\\([0-9]+\\)\\'")

    ; From prioritize.el:

    (cant-move-nothing-nowhere . "Can't move nothing anywhere.")
    (priority-prompt . "New priority: ")
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
----------------------------------------------------------------------------
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
    (no-changes . "Nothing was saved since no options had been changed\n")
    (could-not-save-options . "Couldn't save options.")
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
    (prompt-modifier-ansaphone . "[%s]")
    (prompt-modifier-messages . "%s (%d messages)")
    (go-to-pri-conf-prompt . "Go to next prioritized conference")
    (read-pri-text-conf . "Read next prioritized article")
    (review-next-text-prompt . "Review next article")
    (review-next-comment-prompt . "Review next comment")
    (review-next-marked-prompt . "Review next marked article")
    (read-next-letter-prompt . "Read next letter")
    (read-next-footnote-prompt . "Read next footnote")
    (read-next-comment-prompt . "Read next comment")
    (read-next-text-prompt . "Read next article")
    (go-to-conf-of-marked-prompt . "Review next marked")
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
    (person-is-anonymous . "Anonymous user")

    (process-signal . "Signal from the process.")
    (dead-session . "No active LysKOM session.")
    (closed-connection . "
**************************************************
%#2s
LysKOM session killed abnormallly
Error message: %#1s**************************************************")
    (error-not-found . "Error code %#1d. No explanation available.")

    ; Useful in more place than one:
    (illegal-command . "Illegal command.\n")
    (no-such-text . "The article doesn't exist.\n")
    (no-such-text-m . "The article doesn't exist.")
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
    (filter-action . "Filter how: ")
    (filter-in-conf . "In which conference (all): ")
    (filter-subject . "Filter which subject: ")
    (filter-which-text . "Filter articles containing: ")
    (filter-author . "Filter which author: ")
    (permanent . "(permanent)")
    (temporary . "(temporary)")
    (filter-edit-buffer-name . "*LysKOM Filter Edit*")
    (filter-edit-empty-list . "Empty list")
    (filter-edit-start-of-list . "Beginning of list")
    (filter-edit-end-of-list . "End of list")
    (filter-edit-filter-how . "Filter how? ")
    (filter-edit-filter-what . "What do you want to filter: ")
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
    (no-filters . "No filters are defined.")
    (view-filters-header . "\nActive filters:\n\n")
    (view-filters-footer . "")

    (ansaphone-new-message . "New Ansaphone message: ")
    (ansaphone-message . "Ansaphone message:
----------------------------------------------------------------------
%#1t
----------------------------------------------------------------------
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
    (remote-set-ansaphone . "Remote control (%#1P %#2s): The ansaphone is %#3s\n")
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
    (generic-popup-title . "%#1s")

    (who-i-am-not-present . "%#1P (not in any conference)\n")
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

    ;; From slow.el
    (no-such-command . "There is no such command.\n")
    (command-completions . "You may mean one of the following:\n %#1s\n")
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
     			      . "Review (the) previouls commented (article)")
    (kom-review-stack         . "Review stack")
    (kom-review-presentation  . "Review presentation")
    (kom-review-backward      . "(Review) Backwards")
    (kom-view-next-text       . "(Read) next article")
    (kom-who-is-on            . "Who is on") 
    (kom-who-am-i             . "Where (am) i")
;    (kom-display-who-buffer   .  "Display who (list)")
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

    (kom-status-session . "Status (of a) session")
    ))

(lyskom-language-strings lyskom-menu en
  '((lyskom    . "LysKOM")
    (read      . "Read")
    (dont-read . "Jump")
    (write     . "Write")
    (conference . "Conference")
    (person    . "User")
    (move      . "Go")
    (info      . "About")
    (send      . "Send message")
    (recievers . "Recievers")
    (commented . "Commented")
    (kom-edit-send . "Send")
    (kom-edit-send-anonymous . "Send anonymously")
    (kom-edit-quit . "Throw away") 
    (kom-edit-add-recipient . "Add recipient")
    (kom-edit-add-copy . "Add carbon copy")
    (kom-edit-show-commented . "Review commented")
    (kom-edit-insert-commented . "Cite commented")))

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
  (define-prefix-command 'lyskom-review-prefix)
  (define-prefix-command 'lyskom-change-prefix)
  (define-prefix-command 'lyskom-next-prefix)
  (define-prefix-command 'lyskom-list-prefix)
  (define-prefix-command 'lyskom-filter-get-prefix)
  (define-prefix-command 'lyskom-S-prefix)
  (define-prefix-command 'lyskom-previous-prefix)
  (define-key lyskom-en-mode-map "A" 'lyskom-change-prefix)
  (define-key lyskom-en-mode-map "r" 'lyskom-review-prefix)
  (define-key lyskom-en-mode-map "f" 'lyskom-filter-get-prefix)
  (define-key lyskom-en-mode-map "n" 'lyskom-next-prefix)
  (define-key lyskom-en-mode-map "l" 'lyskom-list-prefix)
  (define-key lyskom-en-mode-map "s" 'lyskom-S-prefix)
  (define-key lyskom-en-mode-map "b" 'lyskom-previous-prefix)

  (define-key lyskom-en-mode-map [mouse-2] 'kom-mouse-2)
  (define-key lyskom-en-mode-map [down-mouse-3] 'kom-mouse-3)
  (define-key lyskom-en-mode-map [mouse-3] 'kom-mouse-null)
  (define-key lyskom-en-mode-map "*" 'kom-key-mouse-2)
  (define-key lyskom-en-mode-map "\C-i" 'kom-next-link)
  (define-key lyskom-en-mode-map "\M-\C-i" 'kom-previous-link)

  ;; These should be first in order to be last in the menu of alternatives.
  (define-key lyskom-en-mode-map "A?" 'lyskom-help)
  (define-key lyskom-en-mode-map "r?" 'lyskom-help)
  (define-key lyskom-en-mode-map "f?" 'lyskom-help)
  (define-key lyskom-en-mode-map "n?" 'lyskom-help)
  (define-key lyskom-en-mode-map "l?" 'lyskom-help)
  (define-key lyskom-en-mode-map "s?" 'lyskom-help)
  

  (define-key lyskom-en-mode-map "o" 'kom-set-unread)
  (define-key lyskom-en-mode-map "x" 'kom-extended-command)
  (define-key lyskom-en-mode-map " " 'kom-next-command)
  (define-key lyskom-en-mode-map "\n" 'kom-page-next-command)
  (define-key lyskom-en-mode-map "\r" 'kom-line-next-command)

  (define-key lyskom-en-mode-map "?"  'describe-mode)
  (define-key lyskom-en-mode-map "m"  'kom-send-letter)
  (define-key lyskom-en-mode-map "g"  'kom-go-to-conf)
  (define-key lyskom-en-mode-map "a"  'kom-write-text)
  (define-key lyskom-en-mode-map "c"  'kom-write-comment)
  (define-key lyskom-en-mode-map "C"  'kom-comment-previous)
  (define-key lyskom-en-mode-map "F"  'kom-write-footnote)
  (define-key lyskom-en-mode-map "p"  'kom-private-answer)
  (define-key lyskom-en-mode-map "P"  'kom-private-answer-previous)
  (define-key lyskom-en-mode-map "j"  'kom-jump)
  (define-key lyskom-en-mode-map "J"  'kom-super-jump)
  (define-key lyskom-en-mode-map "lc" 'kom-list-conferences)
  (define-key lyskom-en-mode-map "ln" 'kom-list-news)
  (define-key lyskom-en-mode-map "lu" 'kom-list-persons)
  (define-key lyskom-en-mode-map "lr" 'kom-list-re)
  (define-key lyskom-en-mode-map "ls" 'kom-membership)
  (define-key lyskom-en-mode-map "la" 'kom-list-summary)
  (define-key lyskom-en-mode-map "lf" 'kom-list-filters)
  (define-key lyskom-en-mode-map "S"  'kom-add-self)
  (define-key lyskom-en-mode-map "M"  'kom-mark-text)
  (define-key lyskom-en-mode-map "U"  'kom-unmark-text)
  (define-key lyskom-en-mode-map "na" 'kom-view-next-new-text)
  (define-key lyskom-en-mode-map "nc" 'kom-go-to-next-conf)
  (define-key lyskom-en-mode-map "nl" 'kom-next-kom)
  (define-key lyskom-en-mode-map "nu" 'kom-next-unread-kom)
  (define-key lyskom-en-mode-map "bl" 'kom-previous-kom)
  (define-key lyskom-en-mode-map "q"  'kom-quit)
  (define-key lyskom-en-mode-map "z"  'kom-bury)
  (define-key lyskom-en-mode-map "R"  'kom-recover)
  (define-key lyskom-en-mode-map "t"  'kom-display-time)
  (define-key lyskom-en-mode-map "fp" 'kom-get-appreciation)
  (define-key lyskom-en-mode-map "fk" 'kom-get-abuse)
  (define-key lyskom-en-mode-map "fs" 'kom-filter-subject)
  (define-key lyskom-en-mode-map "fa" 'kom-filter-author)
  (define-key lyskom-en-mode-map "fc" 'kom-filter-text)
  (define-key lyskom-en-mode-map "w"  'kom-who-is-on)
  (define-key lyskom-en-mode-map "I"  'kom-who-am-i)
  (define-key lyskom-en-mode-map "W"  'kom-busy-wait)
  (define-key lyskom-en-mode-map "Ap" 'kom-change-presentation)
  (define-key lyskom-en-mode-map "Af" 'kom-filter-edit)
  (define-key lyskom-en-mode-map "Am" 'kom-change-auto-reply)
  (define-key lyskom-en-mode-map "r " 'kom-view)
  (define-key lyskom-en-mode-map "r0" 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map "r1" 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map "r2" 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map "r3" 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map "r4" 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map "r5" 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map "r6" 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map "r7" 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map "r8" 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map "r9" 'kom-initial-digit-view)
  (define-key lyskom-en-mode-map "rc" 'kom-view-commented-text)
  (define-key lyskom-en-mode-map "rC" 'kom-view-previous-commented-text)
  (define-key lyskom-en-mode-map "ra?" 'lyskom-help)
  (define-key lyskom-en-mode-map "rac" 'kom-review-comments)
  (define-key lyskom-en-mode-map "rar" 'kom-review-tree)
  (define-key lyskom-en-mode-map "rj" 'kom-review-clear)
  (define-key lyskom-en-mode-map "rn" 'kom-review-next)
  (define-key lyskom-en-mode-map "ru" 'kom-review-noconversion)
  (define-key lyskom-en-mode-map "ro" 'kom-find-root)
  (define-key lyskom-en-mode-map "rl" 'kom-review-by-to)
  (define-key lyskom-en-mode-map "rf" 'kom-review-first)
  (define-key lyskom-en-mode-map "rA" 'kom-review-all)
  (define-key lyskom-en-mode-map "rM" 'kom-review-more)
  (define-key lyskom-en-mode-map "rg" 'kom-review-last-normally-read)
  (define-key lyskom-en-mode-map "B"  'kom-review-backward)
  (define-key lyskom-en-mode-map "rs" 'kom-review-stack)
  (define-key lyskom-en-mode-map "rp" 'kom-review-presentation)
  (define-key lyskom-en-mode-map "rr" 'kom-find-root-review)
  (define-key lyskom-en-mode-map "rm" 'kom-review-marked-texts)
  (define-key lyskom-en-mode-map "ram" 'kom-review-all-marked-texts)
  (define-key lyskom-en-mode-map "ra " 'kom-review-all)
  (define-key lyskom-en-mode-map "sc" 'kom-status-conf)
  (define-key lyskom-en-mode-map "su" 'kom-status-person)
  (define-key lyskom-en-mode-map "ss" 'kom-status-session)

  ;; Running in buffer

  (define-key lyskom-en-mode-map "\033p" 'backward-text)
  (define-key lyskom-en-mode-map "\033n" 'forward-text)
  (define-key lyskom-en-mode-map "sa" 'kom-save-text)

  (define-key lyskom-en-mode-map "\C-?" 'scroll-down)
  )


;;;==============================================================
;;; Keymap for filter editing
;;;

(lyskom-language-var lyskom-filter-edit-map en nil)

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
(lyskom-language-keymap lyskom-filter-edit-map en lyskom-en-filter-edit-map)

(if lyskom-en-prioritize-mode-map 
    nil
  (setq lyskom-en-prioritize-mode-map (make-keymap))
  (suppress-keymap lyskom-en-prioritize-mode-map)
  (define-key lyskom-en-prioritize-mode-map [mouse-2] 'kom-mouse-2)
  (define-key lyskom-en-prioritize-mode-map [down-mouse-3] 'kom-mouse-3)
  (define-key lyskom-en-prioritize-mode-map [mouse-3] 'kom-mouse-null)
  (define-key lyskom-en-prioritize-mode-map "*" 'kom-key-mouse-2)
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
  (define-key lyskom-en-prioritize-mode-map [M-up] 'kom-prioritize-move-up)
  (define-key lyskom-en-prioritize-mode-map "\M-p" 'kom-prioritize-move-up)
  (define-key lyskom-en-prioritize-mode-map "u" 'kom-prioritize-move-up)
  (define-key lyskom-en-prioritize-mode-map [M-down] 'kom-prioritize-move-down)
  (define-key lyskom-en-prioritize-mode-map "\M-n" 'kom-prioritize-move-down)
  (define-key lyskom-en-prioritize-mode-map "d" 'kom-prioritize-move-down)
  (define-key lyskom-en-prioritize-mode-map "\M-<" 'kom-prioritize-beginning)
  (define-key lyskom-en-prioritize-mode-map "\M->" 'kom-prioritize-end)
  (define-key lyskom-en-prioritize-mode-map "r" 'kom-prioritize-reprioritize)
  (define-key lyskom-en-prioritize-mode-map "g" 'kom-prioritize-goto-priority)
  (define-key lyskom-en-prioritize-mode-map "p" 'kom-prioritize-set-priority)
  (define-key lyskom-en-prioritize-mode-map "s" 'kom-prioritize-save)
  (define-key lyskom-en-prioritize-mode-map "q" 'kom-prioritize-quit)
)

(lyskom-language-var lyskom-prioritize-header-lines en 2)

(lyskom-language-var lyskom-prioritize-header en 
" Prio   Conference
-----------------------------------------------------------------------------
")



;;;; ============================================================
;;;; Strings and things for the customize mode
;;;;

(defvar lyskom-customize-map nil
  "Keymap for the customize buffer")

(if lyskom-customize-map
    nil
  (setq lyskom-customize-map (make-sparse-keymap))
  (set-keymap-parent lyskom-customize-map global-map)
  (define-key lyskom-customize-map "\t" 'widget-forward)
  (define-key lyskom-customize-map "\M-\t" 'widget-backward)
  (define-key lyskom-customize-map "\C-m" 'widget-button-press)
  (define-key lyskom-customize-map [mouse-2] 'widget-button-click)
  (define-key lyskom-customize-map "\C-c\C-c" 'lyskom-customize-save-and-quit)
  (define-key lyskom-customize-map "\C-c\C-k" 'lyskom-customize-quit)
  (define-key lyskom-customize-map "\C-c\C-s" 'lyskom-customize-save)
  (define-key lyskom-customize-map "\C-c\C-a" 'lyskom-customize-apply)
)

(lyskom-language-strings 'lyskom-custom-strings 'sv
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
    ;; Strings that are used in types and so forth
    ;;

    (buffer-name . "%#1s-configure")
    (other-window . "Some other window")
    (other-frame . "Some other frame")
    (new-frame . "In a new frame")
    (lyskom-window . "The LysKOM buffer's window")
    (window-on-buffer . "A window displaying the buffer")
    (on . "On")
    (off . "Off")
    (yes . "Yes ")
    (no . "No")
    (turned-off . "Turned off")
    (number-of-times . "A few times")
    (sound-file . "Audio file")
    (selected-mark . "Mark")
    (ask . "Ask every time")
    (before . "Before the text")
    (after . "After the text")
    (depth-first . "In comment order")
    (time-order . "I time order")
    (express-break . "Immediately when created")
    (break . "After the current comment chain")
    (no-break . "Efter the current conference")
    (command . "Command")
    (command-list . "Command list")
    (some-persons . "For some persons")
    (name . "Name")
    (page-none . "Never")
    (page-all . "Before every command")
    (page-some . "Before the following commands")
    (ask-every-time . "Ask every time")
    (fixed-priority . "Fixed priority")
    (messages-in-lyskom-buffer . "In the LysKOM buffer")
    (discard-messages . "Nowhere - discard them!")
    (in-named-buffer . "In a named buffer")
    (everybody-rcpt . "Everybody")
    (group-rcpt . "The recipient of the last group message")
    (sender-rcpt . "The sender of the most recent message")
    (viewer-program . "Web browser")
    (netscape-viewer . "Netscape Navigator (all)")
    (emacs-w3-viewer . "Emacs W3-mode (HTTP, Goper, FTP)")
    (emacs-general-viewer . "Emacs (FTP, Telnet, Mail)")
    (emacs-dired-viewer . "Emacs Dired (FTP)")
    (emacs-mail-viewer . "Emacs Mail-mode (Mail)")
    (emacs-telnet-viewer . "Emacs Telnet-mode (telnet)")
    (mosaic-viewer . "NCSA Mosaic (all)")
    (dont-check . "Don't ask for confirmation")
    (check-before-open . "Confirm before writing the text")
    (check-before-send . "Confirm before sending the text")
    (no-size-limit . "No limit")
    (max-size-in-bytes . "Fixed limit (in bytes)")
    (execute . "Execute")
    (kbd-macro . "Keyboard macro")
    (command . "Command")
    (enter-kbd-macro . "Enter a keybard macro. Finish with %#1s")
    (long-format . "Show help texts")
    (short-format . "Hide help texts")

    ;;
    ;; Misc doc strings
    ;;

    (lyskom . "Configuration of LysKOM")
    (lyskom-doc . "\\[lyskom-customize-save-and-quit] to save and quit,
\\[lyskom-customize-save] to save without quitting,
\\[lyskom-customize-quit] to quit without saving.")

    (section . "------------------------------------------------------------------------------\n")
    (look-and-feel-misc . "Look and feel\n")
    (reading .            "Reading\n")
    (writing .            "Writing\n")
    (urls .               "URL Management\n")
    (personal-messages .  "Personal messages\n")
    (remote-control .     "Remote control of LysKOM\n")
    (hooks .              "Hook functions\n")

    (audio-cues .         "Audio cues\n")
    (automatic-replies .  "Automatic replies\n")


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

    (kom-write-texts-in-window-doc . "\
  Controls which window is used to write new texts. The possible choices are:
  the LysKOM buffer's window, which means that new texts are written in the
  same window in which the LysKOM buffer appears; some other window, which
  means that a window other than the LysKOM buffer's window is selected (a
  new window may be created); some other frame, which means that new texts
  are written in some other already existing frame; and in a new frame, which
  means that a new frame is created in which to write the text.")

    (kom-prioritize-in-window-doc . "\
  Controls which window is used to prioritize conferences. The possible
  choices are: the LysKOM buffer's window, which means that new texts are 
  written in the same window in which the LysKOM buffer appears; some other 
  window, which means that a window other than the LysKOM buffer's window is 
  selected (a new window may be created); some other frame, which means that 
  new texts are written in some other already existing frame; and in a new 
  frame, which means that a new frame is created in which to write the text.")

    (kom-customize-in-window-doc . "\
  Controls which window is used to configure LysKOM. The possible choices
  are: the LysKOM buffer's window, which means that new texts are written
  in the same window in which the LysKOM buffer appears; some other window,
  which means that a window other than the LysKOM buffer's window is selected
  (a new window may be created); some other frame, which means that new texts
  are written in some other already existing frame; and in a new frame, which
  means that a new frame is created in which to write the text.")

    (kom-prompt-format-doc . "\
  The format of the LysKOM prompt. The characters \"%s\" in this string are
  replaces by the real LysKOM prompt. Some examples:

  `kom.lysator.liu.se %s'   gives the prompt    kom.lysator.liu.se Time -
  `%s, John'                gives the prompt    Time, John -")

    
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
  asks for a mark value every time. Allowed values are between 0 and 255.")

    (kom-reading-puts-comments-in-pointers-last-doc . "\
  Controls if comment links are listed before or after the body of a text.
  Normally comment links are listed after the text.

  Before:

    398331 1996-09-24  13:22  /2 rader/ George Berkeley
    Recipient: Philosophy <1226>
    Comment in article 398374 by John Locke
    Subject: 
    ------------------------------------------------------------
    An abstract idea is a contradiction in terms.
    (398331) -----------------------------------

  After:

    398331 1996-09-24  13:22  /2 rader/ George Berkeley
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

    892343 1996-09-24  19:21  /2 rader/ Tycho Brahe
    Recipien: Presentation (of nya) Members
    Subject: Tycho Brahe
    ------------------------------------------------------------
    Astronomer and discoverer of stars resident on the island of Ven.
    (892343) -----------------------------------

  Off:

    892343 1996-09-24  19:21  /2 rader/ Tycho Brahe
    Recipien: Presentation (of nya) Members
    Subject: Tycho Brahe

    Astronomer and discoverer of stars resident on the island of Ven.
    (892343)

  Most people have this turned on.")


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
  Om detta är påslaget så kommer TAB bara att fylla ut namn på inloggade
  personer när kommandot bara kan utföras på inloggade personer (till
  exempel Status (för) session och Sända messelande.) Om det är avslaget
  kommer TAB att fylla ut även med namn på personer som inte är inloggade.")


    (kom-membership-default-priority-doc . "\
  Detta bestämmer hur prioriteten på möten man går med i sätts. Om det är
  en siffra mellan 0 och 255 så kommer nya medlemskap att få den prioriteten.
  I annat fall kommer klienten att fråga för varje nytt medlemskap vilken
  prioritet det skall ha.")


    (lyskom-prompt-text-doc . "\
  Denna text sätts efter den aktuella prompten, men byts ut om man trycker
  SPC för att köra kommandot i prompten. Det absolut vanligaste är \" - \".")


    (lyskom-prompt-executing-default-command-text-doc . "\
  Denna text sätts efter prompten om man kör kommandot i prompten. Det 
  vanligaste här är en enda punkt: \".\"")


    (kom-show-personal-messages-in-buffer-doc . "\
  Denna inställning bestämmer var personliga, gruppmeddelanden och allmänna
  meddelanden visas. Meddelanden kan antingen visas i LysKOM-bufferten, 
  kastas bort helt eller visas i en namngiven buffert.")


    (kom-pop-personal-messages-doc . "\
  Om personliga meddelanden skall visas i en egen buffert och denna inställning
  också är påslagen så kommer den bufferten att visas så fort ett meddelande
  anländer.")


    (kom-ding-on-new-letter-doc . "\
  LysKOM kan signalera när det kommer inlägg till ens brevlåda som någon
  annan har skrivit. Denna inställning bestämmer hur det signaleras.")

    (kom-ding-on-priority-break-doc . "\
  När det kommer ett inlägg med högre prioritet än det man för tillfället
  läser, och LysKOM vill bryta läsordningen för att visa detta inlägg så
  bestämmer denna inställning hur detta signaleras med ljud.")


    (kom-ding-on-wait-done-doc . "\
  När man har gjort Vänta och det kommer ett inlägg bestämmer denna inställning
  hur händelsen signaleras med ljud.")


    (kom-ding-on-common-messages-doc . "\
  Denna intällning bestämmer hur ljud används för att signalera att det har
  kommit ett allmänt meddelande.")


    (kom-ding-on-group-messages-doc . "\
  Denna intällning bestämmer hur ljud används för att signalera att det har
  kommit ett gruppmeddelande.")


    (kom-ding-on-personal-messages-doc . "\
  Denna intällning bestämmer hur ljud används för att signalera att det har
  kommit ett personligt meddelande.")


    (lyskom-ding-on-no-subject-doc . "\
  Denna inställning bestämmer hur ljud används för att varna att man har
  glömt att skriva en ärenderad.")


    (kom-audio-player-doc . "\
  Om man vill att LysKOM skall spela ljudfiler i stället för att pipa vid 
  olika tillfällen måste denna inställning peka ut ett program som klarar
  att spela ljudfilerna. Programmet skall ta ett argument: namnet på filen
  som skall spelas.")


    (kom-default-message-recipient-doc . "\
  Denna inställning bestämmer vem som kommer att vara defaultmottagare för
  personliga meddelande man skickar. Alternativen som finns är att meddelanden
  per default är almänna, att avsändaren för det senast mottagna meddelandet
  skall vara default eller att mottagaren för det senaste gruppmeddelandet
  (eller avsändaren av det senaste personliga eller almänna meddelandet) skall
  vara mottagare.")


    (lyskom-filter-outgoing-messages-doc . "\
  Om denna inställning är påslagen så kommer meddelanden som skickas 
  automatiskt, till exempel automatiska svar och svar på fjärkotrollförsök
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

    (kom-confirm-multiple-recipients-doc . "\
  När man försöker skriva ett inlägg med mer än en mottagare kan LysKOM be om
  en bekräftelse att alla mottagare skall vara med. Detta för att man skall
  undvika att skicka inlägg till mottagare som inte längre är relevanta för
  inläggets innehåll. Kontrollen kan antingen inte ske alls, ske innan man
  börjar skriva inlägget eller precis när man skall skicka in det.")

    (kom-check-commented-author-membership-doc . "\
  Om detta är påslaget kommer LysKOM att kontrollera att författaren av
  inlägg man kommenterar är medlem i någon av kommentarens mottagare. Om
  så inte är fallet får man frågan om denne skall läggas till som mottagare.")

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
  Autmatsvararen skickar detta meddelande om inget annat meddelande har
  specificerats via någon annan mekanism.")

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
  eller utropstecknet som står till höger om inställningen")


    ;;
    ;; Tags for variables
    ;;
    
    (kom-emacs-knows-iso-8859-1-tag . "Emacs förstår ISO-8859-1")
    (kom-bury-buffers-tag . "Begrav buffertar när man byter LysKOM")
    (kom-write-texts-in-window-tag . "Skriv texter i")
    (kom-prioritize-in-window-tag . "Prioritera möten i")
    (kom-customize-in-window-tag . "Ställ in LysKOM i")
    (kom-prompt-format-tag . "Promptformat")
    (kom-cite-string-tag . "Citatmarkering")
    (kom-created-texts-are-read-tag . "Läsmarkera skapade texter")
    (kom-dont-restore-window-after-editing-tag . 
       "Återställ fönster efter editering")
    (kom-default-mark-tag . "Defaultmarkering")
    (kom-reading-puts-comments-in-pointers-last-tag . "Kommentarslänkar visas")
    (kom-dashed-lines-tag . "Streckade linjer kring inläggstexten")
    (kom-print-number-of-unread-on-entrance-tag . 
       "Visa antalet olästa när man går till ett möte")
    (kom-presence-messages-tag . "Närvaromeddelanden på eller av")
    (kom-presence-messages-in-buffer-tag . "Närvaromeddelanden i LysKOM-bufferten")
    (kom-show-where-and-what-tag . 
       "Visa varifrån personer är inloggade och vad de gör")
    (kom-idle-hide-tag . 
       "Antal minuter en session får vara inaktiv och ändå visas")
    (kom-show-footnotes-immediately-tag .
       "Visa fotnoter omedelbart")
    (kom-follow-comments-outside-membership-tag .
       "Följ kommentarskedjor utanför medlemskap")
    (kom-read-depth-first-tag . "Läsordning")
    (kom-continuous-scrolling-tag . "Omedelbar scrollning")
    (kom-deferred-printing-tag . "Fördröjda utskrifter")
    (kom-higher-priority-breaks-tag . "Läs prioriterade texter")
    (kom-login-hook-tag . "Kommandon som körs vid login")
    (kom-do-when-done-tag . "Kommandon som körs efter allt är utläst")
    (kom-page-before-command-tag . "Rensa skärmen")
    (kom-permissive-completion-tag . "Petig utfyllnad av namn")
    (kom-membership-default-priority-tag . "Prioritet för nya medlemskap")
    (lyskom-prompt-text-tag . "Text som avslutar aktuell prompt")
    (lyskom-prompt-executing-default-command-text-tag . "Text att avsluta prompten med när man trycker SPC")
    (kom-show-personal-messages-in-buffer-tag . "Personliga, grupp och allmänna meddelanden visas")
    (kom-pop-personal-messages-tag . "Ploppa upp en buffert med personliga meddelanden när de kommer")
    (kom-ding-on-new-letter-tag . "Pip när det kommer brev")
    (kom-ding-on-priority-break-tag . "Pip när det kommer prioriterade inlägg")
    (kom-ding-on-wait-done-tag . "Pip när man har väntat klart")
    (kom-ding-on-common-messages-tag . "Pip när det kommer allmänna meddelanden")
    (kom-ding-on-group-messages-tag . "Pip när det kommer gruppmeddelanden")
    (kom-ding-on-personal-messages-tag . "Pip när det kommer personliga meddelanden")
    (lyskom-ding-on-no-subject-tag . "Pip när man glömmer att skriva en ärenderad")
    (kom-audio-player-tag . "Ljudspelarprogram")
    (kom-default-message-recipient-tag . "Defaultmottagare för meddelanden")
    (lyskom-filter-outgoing-messages-tag . "Visa automatiska meddelanden")
    (kom-friends-tag . "Vänner och bekanta")
    (kom-url-viewer-preferences-tag . "Öppna URLer med följande program")
    (kom-mosaic-command-tag . "Kommando för att starta NCSA Mosaic")
    (kom-netscape-command-tag . "Kommando för att starta Netscape Navigator")
    (kom-confirm-multiple-recipients-tag . "Bekräfta multipla mottagare")
    (kom-check-commented-author-membership-tag . "Kontrollerade den kommenterade författarens medlemskap")
    (kom-inhibit-typeahead-tag . "Buffra tangenttryckningar")
    (kom-max-buffer-size-tag . "Maximal buffertstorlek")
    (kom-ansaphone-record-messages-tag . "Spela in meddelanden i automatsvareren")
    (kom-ansaphone-show-messages-tag . "Visa meddelanden medan automatsvararen är påslagen")
    (kom-ansaphone-default-reply-tag . "Svarsmeddelande")
    (kom-remote-control-tag . "Fjärrstyrning")
    (kom-remote-controllers-tag . "Tillåtna fjärrstyrare")
    (kom-self-control-tag . "Tillät fjärrstyrning av mig själv")
    (kom-customize-format-tag . "Visa hjälptextern för inställningar")

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
;;;; Text buttom menuse go here. This will probably be moved back
;;;; to vars.el.in when the strings have been replaced by
;;;; symbols, but for now they'll stay here.


(lyskom-language-var lyskom-button-actions en
  '((text
     text-text
     lyskom-button-view-text
     (("View article" . lyskom-button-view-text)
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
    (email
     email-text
     lyskom-button-open-email
     (("Skicka mail" . lyskom-button-open-email)
      ("Kopiera" . lyskom-button-copy-email))
     nil)))


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
    (kom-tell-next-lyskom	. "Moves to a different LysKOM.")))


;; Placed here because this must NOT be evaluated before 
;; kom-tell-phrases is defined:

(lyskom-language-var kom-mercial en (car (cdr (assoc 'kom-tell-wait kom-tell-phrases))))


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
    (error-47 . "Anonymous texts are not accepted by all recipients")))


     
(provide 'lyskom-strings)

;;; english-strings ends here
