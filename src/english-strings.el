;;;;;
;;;;; $Id: english-strings.el,v 35.14 1992-01-06 18:50:27 linus Exp $
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
;;;; File: english-strings.el
;;;;
;;;; This file contains all strings in the LysKOM elisp client.
;;;; Language:     English.
;;;;
;;;; ================================================================
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: english-strings.el,v 35.14 1992-01-06 18:50:27 linus Exp $\n"))


;;; ================================================================
;;; lyskom-edit-mode-map                             English version

(defvar lyskom-edit-mode-map nil
  "Mode map for LysKOM edit.")

;;; Set the keymap for lyskom-edit-mode

(if lyskom-edit-mode-map
    nil
  (setq lyskom-edit-mode-map (make-sparse-keymap))
  (fset 'lyskom-edit-prefix (make-keymap))
  (fset 'lyskom-edit-review-prefix (make-keymap))
  (fset 'lyskom-edit-insert-prefix (make-keymap))
  (define-key lyskom-edit-mode-map "\C-c"	'lyskom-edit-prefix)
  (define-key lyskom-edit-mode-map "\C-c?"	'lyskom-help)
  (define-key lyskom-edit-mode-map "\C-cr"	'lyskom-edit-review-prefix)
  (define-key lyskom-edit-mode-map "\C-ci"	'lyskom-edit-insert-prefix)
  (define-key lyskom-edit-mode-map "\C-c\C-c"	'kom-edit-send)
  (define-key lyskom-edit-mode-map "\C-ck"	'kom-edit-quit)
  (define-key lyskom-edit-mode-map "\C-c\C-k"	'kom-edit-quit)
  (define-key lyskom-edit-mode-map "\C-cr?"	'lyskom-help)
  (define-key lyskom-edit-mode-map "\C-crc"	'kom-edit-show-commented)
  (define-key lyskom-edit-mode-map "\C-ci?"	'lyskom-help)
  (define-key lyskom-edit-mode-map "\C-cic"	'kom-edit-insert-commented)
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
  (fset 'lyskom-edit-add-prefix (make-keymap))
  (define-key lyskom-edit-mode-map "\C-ca" 'lyskom-edit-add-prefix)
  (define-key lyskom-edit-mode-map "\C-car" 'kom-edit-add-recipient)
  (define-key lyskom-edit-mode-map "\C-cac" 'kom-edit-add-copy)
  (define-key lyskom-edit-mode-map "\C-ca?" 'lyskom-help))


(defvar lyskom-header-separator
  (substitute-command-keys
   "\\<lyskom-edit-mode-map>\
--- Write below. \
Post: \\[kom-edit-send], \
Kill: \\[kom-edit-quit], \
Help: \\[describe-mode] ---")
  "*String to separate headers from text body.")


(defvar lyskom-header-subject "Subject: "
  "*String to prompt for subject in the edit buffer.")


(defconst lyskom-strings 
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
    (are-logged-in . "You are now logged on. Please wait...\n")
    (you-have-motd . "\nYou have a note on the door:\n\n")

    (first-greeting . "%#1s
This appears to be the first time you use LysKOM. Welcome!
Please make sure you have spelled your name correctly. You should use your
full name and organization, eg. \"Joe Hacker, MIT\". If you spelled your name
incorrectly, or you wish to change it, answer 'no' to the question below.

For the time being the LysKOM server stores most of the information so that
anybody can read it. Only passwords are encrypted.

If you ar uncertain about how to use LysKOM, you can retreive a manual by
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



    ; From internal.el:
    (shaky-tcp . "At the moment I can't reach the server. The TCP/IP connection is shaky%#1s")
    (retrying-tcp . "Retrying.")

    ; From parse.el:
    ; No entries.

    ; From services.el:
    ; No entries.

    ; From cache.el:
    ; No entries.

    ; From commands1.el:
    (appreciation . "Tap, tap.\n\n")
    (abuse . "Thomp! Ouch!")


    (what-conf-to-delete . "Conference/user to delete: ")
    (confirm-delete-pers-or-conf . "Really delete %#1s %#2s? ")
    (the-pers . "the user")
    (the-conf . "the conference")
    (deletion-not-confirmed . "Delete canceled\n")
    (somebody-else-deleted-that-conf . "Somebody else just deleted the conference.\n")
    (conf-is-deleted . "Ok, %#1s has now been deleted.\n")
    (you-could-not-delete . "%#1s can't be deleted by you.\n")

    (what-text-to-delete . "Article to remove?")
    (deleting-text . "Removing article %#1d...")

    (presentation-for-whom . "Which conference/user: ")
    (somebody-deleted-that-conf . "Somebody just deleted that conference.\n")
    (review-presentation-of . "Review presentation of %#1s.\n")
    (has-no-presentation . "%#1s has no presentation.\n")

    (have-to-read . "You must read an article first.\n")

    (review-text-no . "Review article %#1d.\n")
    (no-comment-to . "There is no commented article.\n")

    (who-letter-to . "Send a letter to whom: ")
    (has-motd . "%#1s has a note on the door:\n\n")
    (motd-persist-q . "Send the letter? ")

    (who-to-add . "Whom do you want to add: ")
    (where-to-add . "To which conference: ")
    (where-to-add-self . "Subscribe to which conference: ")
    (priority-q . "Conference priority? (0 (low) - 255 (high)) ")
    (done . "done.\n")
    (nope . "didn't work.\n")

    (conf-does-not-exist . "\nThe conference doesn't exist.\n")

    (who-to-exclude . "Whom do you want to unsubscribe: ")

    (where-from-exclude . "From which conference: ")

    (leave-what-conf . "Unsubscribe to which conference: ")

    (error-fetching-person . "Error fetching user.\n")
    (error-fetching-conf . "Error fetching conference.\n")

    (name-of-conf . "Conference name: ")
    (anyone-member . "May anyone subscribe? ")
    (secret-conf . "Secret conference? ")
    (comments-allowed . "Are comments permitted? ")
    (confusion-what-to-comment . "I can't figure out which article you want to comment.\n")
    (confusion-what-to-footnote . "I can't figure out to which article you want to write a footnote to.\n")

    (confusion-who-to-reply-to . "I can't figure out which article you want to write a private reply to.\n")
    (confusion-what-to-answer-to . "I can't figure out which article you want write a reply to.")
    (really-quit . "Do you really want to quit LysKOM? ")
    (session-ended . "
--------------------------------------------
  LysKOM session finished
  You are now disconnected from the server
--------------------------------------------\n")
    (what-to-change-pres-you . "Change presentation of what/who (yourself): ")
    (who-to-put-motd-for . "Post note on the door of who/what (yourself): ")

    (cant-get-conf-stat . "Cannot get the status for that conference.")
    (go-to-conf-p . "Go to conference: ")
    (want-become-member . "Subscribe? ")
    (no-ok . "Okiedokie, whatever you say.\n")

    (who-to-remove-motd-for . "Remove note from the door of who/what: ")

    (conf-all-read . "%#1s - no unread articles.\n")
    (no-in-conf . "You are not present in any conference.\n")

    (search-for-pers . "Enter search key (RETURN for all users) ")
    (search-for-conf . "Enter search key (RETURN for all conferences) ")
    (name-to-be-changed . "Name to change: ")
    (no-such-conf-or-pers . "The conference or user doesn't exist.\n")
    (new-name . "New name: ")
    (who-to-change-supervisor-for . "Change organizer of who/what: ")
    (new-supervisor . "New organizer: ")
    (text-to-mark . "Mark which article: ")
    (text-to-unmark . "Unmark which article: ")
    (what-mark . "Set which mark: ")
    (unmarking-textno . "Unmarking article %#1d...")
    (marking-textno . "Marking article %#1d...")

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
    (text-to-add-recipient . "Add recipient to which article: ")
    (text-to-add-copy . "Add recipient of carbon copy of which article: ")
    (text-to-delete-recipient . "Remove recipient from which article: ")
    (text-to-add-comment-to . "Add comment to which article: ")
    (text-to-delete-comment-from . "Remove comment from which article: ")

    (where-on-list-q . "Placement on your list? (0-%#1d) ")
    (member-in-conf . "Subscribe to %#1s...")
    (add-member-in . "Adding %#1s as a subscriber to %#2s...")
    (unsubscribe-to . "Unsubscribe from %#1s...")

    (exclude-from . "Unsubscribing %#1s from %#2s...")

    (unsubscribe-failed . "\nDidn't work. Perhaps %#1s didn't subscribe to %#2s?\n")

    (You . "You")
    (could-not-create-conf . "Couldn't create the conference \"%#1s\".\nError: %#2d.\n")
    (created-conf-no-name . "Conference number %#1d %#2s has been created.\n")
    (cant-read-textno . "You are not permitted to read article %#1d")

    (not-supervisor-for . "You are not the organizer of %#1s.\n")
    (not-supervisor-for . "You are not the organizer of %#1s.\n")
    (go-to-conf . "Go to conference %#1s.\n")
    (cant-go-to-his-mailbox . "You are not permitted to go to %#1ss mailbox.\n")
    (not-member-of-conf . "You don't subscribe to %#1s.\n")
    (change-name-done . "\nDone. New name: %#1s.\n")
    (change-name-nope . "\nDidn't work. Perhaps somebody else is already named %#1s?\n")
    (change-supervisor-from-to . "Change organizer for %#1s to %#2s...")
    (change-supervisor-nope . 
     "\nDidn't work. Perhaps you are not permitted to change the organizer for %#1s?\n")
    
    (no-marked-texts . "You have no marked articles.\n")
    (no-marked-texts-mark . 
     "You have no marked any articles with mark %#1d.\n")
    (time-is . "The time is %4#1d-%02#2d-%02#3d %02#4d:%02#5d:%02#6d %#7s(according to the server).\n")
    (total-users . "    A total of %#1d users.\n")
    (who-to-add-q . "Add who/what as a recipient: ")
    (who-to-add-copy-q . "Add which conference/user as recipient of a carbon copy: ")
    (who-to-sub-q . "Remove who/what as a recipient: ")
    (adding-name-as-recipient . "Adding %#1s as recipient of article %#2d...")
    (adding-name-as-copy . "%#1s will receive a carbon copy of article %#2d...")
    (remove-name-as-recipient .
     "Removing %#1s as recipient of article %#2d...")
    (text-to-add-q . "Add which article as a comment: ")
    (text-to-remove-q . "Remove which article as a comment: ")
    (add-comment-to . "Adding article %#1d as a comment to text %#2d...")
    (sub-comment-to . "Removing article %#1d as a comment to article %#2d...")
    (comment-keep-recpt-p ."Shall %#1s remain a recipient? ")

    ; From commands2.el:

    (your-memberships . "Your LysKOM conference subscriptions:\n")
    (memberships-header . "Last access	   Prio	Unread	Conference\n")
    
    (conf-for-status . "Get status of which conference: ")
    (no-such-conf . "The conference doesn't exist.")
    (status-record . "Status of conference %#1s (%#2s) %#3s\n\n")
    (Mailbox . "Mailbox")
    (Protected . "Protected")
    (no-comments . "No comments")
    (closed . "Closed")
                   
    (created-by . "Created by %31#1s %#2s\n")
    (created-at . "Created:%34#1s\n")
    (members .    "Number of subscribers: %19#1d\n")
    (garb-nice . "Expiration time (in days):%16#1d\n")
    (lowest-local-no . "Lowest local number: %21#1d\n")
    (highest-local-no . "Highest local number: %20#1d\n")
    (last-text-time . 
     "Time of last article: %20#1s (accordning to your cache)\n")
    (no-of-motd . "Note on the door in article: %13#1d\n")
    (superconf-is-no-name . "Superconference:       %19#1d %#2s\n")
    (permitted-submitters-no-name . "Admitted authors:%25#1d %#2s\n")
    (supervisor-is-no-name . "Organizer:     %27#1d %#2s\n")
    (presentation-no . "Presentation:    %25#1d\n")
    (conf-has-motd . "\n%#1s has a note on the door:\n")

    (Everybody . "Everyone")
    (show-members-list-also-q . "List subscribers? ")
    (conf-has-these-members . "\n%#1s has the following subscribers:\n")
    (member-list-header . "Last entered       Unread  Name\n\n")
    (secret-membership . "*** Secret line ***\n")
    (pers-for-status . "Get status of which user: ")
    (no-such-pers . "The user doesn't exist.")
    (pers-status-record . "Status of user %#1s (%#2d)\n")
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
    (time-for-last-letter . "Time of last letter:%22#1s (accordning to your cache)\n")
    (superconf . "Superconference:%26#1d %#2s\n")
    (supervisor . "Organizer:%32#1d %#2s\n")
    (member-of-confs . "Subscribed to (conferences):%14#1d\n")
    (presentation . "Presentation:    %25#1d\n")
    (show-membership-list-also-q . "List subscriptions? ")
    (not-allowed-see-confs . "You are not premitted to see which conferences %#1s subscribes to.\n")
    (is-member-of . "\n%#1s subscribes to the following conferences:\n")
    (membership-list-header . "Last access       Unread Conference\n\n")
    (who-to-send-message-to . "Send message to whom? (Everyone) ")
    (his-total-unread . "\n%#1s has a total of %#2d unread articles.\n")
    (message-prompt . "Message: ")
    (message-sent . "Message sent.\n")
    (message-nope .
  "Unable to send the message. Perhaps the recipient isn't logged on.\n")
    (only-last . "Last (0 - %#1d) by %#2s: ")
    (only-error . "Something went wrong. Sorry.\n")

    (you-have-unreads . "You have %#1d unread articles in %#2s\n")
    (you-have-an-unread . "You have 1 unread article in %#1s\n")
    (you-have-read-everything . "No news (is bad news).\n")
    (total-unreads . "\nYou have %#1d unread articles.\n")
    (total-unread . "\nYou have 1 unread article.\n")
    (waiting-for-anything .
     "You are waiting for an article in any conference.\n")
    (waiting-higher-than . 
     "You are waiting for an article in any conference with a priority higher than %#1d.\n")

    (have-to-be-in-conf . "You must go to the conference first.\n")
    (Texts . "Article")
    (Date . "Date")
    (Lines . "Lines")
    (Author . "  Author")
    (Subject . "  Subject")
    (could-not-read . "You couldn't read the article (%#1d).\n")
    (multiple-choice . "There are several alternatives.")
    (does-not-exist . "Unknown command.")

    ; Only people fixing bugs or recieving bugg-reports should change these:
    (buggreport-compilestart . "Compiling...")
    (buggreport-compileend . "Compiling...done")
    (buggreport-description . "This is what I was doing:
(Fill in your comments below)\n\n\n
When you have completed writing this, send you r bug report to the LysKOM
developers. You can do this either by email to bug-lyskom@lysator.liu.se or
through Lysator's LysKOM in the conference \"LysKOM; Elispkilentens
buggrapporter\", or if nothing else works, you can send a normal letter to
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


    (not-logged-in . "You are not logged on.  ")
    (name-is-not-in-conf . "%#1s is in any conference.\n")
    (name-is-in-conf . "%#1s is in\n%#2s\n")
    (connected-during . "Connect time: %#1d seconds.\n")

    (conf-to-set-super-conf-q . "Set superconference of which conference: ")
    (new-super-conf-q . "Which converece do you want as superconference: ")
    (super-conf-for-is . "Changing superconference of %#1s to %#2s...")

    (conf-to-set-garb-nice-q . "Set expiration time for which conference: ")
    (new-garb-nice-q . "What do you want to set the expiration time to: ")
    (garb-nice-for-is . "Changing expiration for %#1s to %#2d...")

    (really-shutdown . "Are you sure you want to shut down the server? ")
    (closing-server . "Shutting down the server...")
    (administrator . "administrator")
    (no-longer-administrator . "a regular user again")
    (you-are-now . "Ok, you are now running as %#1s.\n")
    (setting-motd . "Changing login message to article %#1d.\n")
    (set-motd-success . "You have set a new login message.\n")
    (set-motd-failed . "Didn't work. Perhaps you were not an administrator.\n")
    (removing-motd . "Removing the login message.\n")
    (removed-motd . "You have removed the login message.\n")
    (who-to-throw-out . "Which session do you want to kill: ")
    (throwing-out . "Killing session %#1d... ")
    (postpone-prompt . "How much do you want to read now: ")

    ; From review.el:
    (latest-n . "last %#1d")
    (first-n . "first %#1d")
    (info-by-whom . "%#1s by whom: ")
    (info-to-conf . "%#1s to conference: ")
    (info-by-to . "%#1s by %#2s to %#3s forward.")
    (all-confs . "all conferences")
    (no-get-conf . "You are not permitted to access that conferene.\n")
    (no-get-pers . "You are not permitted to access that user.\n")
    (no-review-info . "You are not permitted to review %#1s\n")
    (review-info . "Review %#1s\n")
    (you-review . "You are now reviewing %#1s.\n")
    (read-text-first . "You must read a article first.\n")
    (cannot-read-last-text . "You cannot review the last read article.\n")
    (review-n-texts . "Review %#1d articles.\n")
    (review-marked . "Review %#1d marked articles.\n")
    (review-text-no . "Review article %#1d")
    (review-one-comment . "Review one comment to article %#1d.\n")
    (review-many-comments . "Review %#2d comments to article %#1d.\n")
    (read-normally-read . "How many articles to you want to review: ")

    ; From edit-text.el:
    (press-C-c-C-c . "Enter C-c C-c to post the article.")
    (recipient . "Recipient:")
    (carbon-copy . "Carbon copy:")
    (text-mass . "%#4s%#1s\n%#2s\n%#3s")
    (comment-to-by . "%#1s to article %#2d%#3s.\n")
    (already-sent . "You have already posted this article. Post it anyway? ")
    (subject . "Subject: ")
    (which-text-include . "Include which article: ")
    (added-recipient . "Recipient: ")
    (added-carbon-copy . "Carbon copy to conference: ")
    (conf-has-motd-no . "The conference has a note on the door. (%#1d)\n\n%#2s")
    (still-want-to-add . "Do you still want to add the conference as a recipient? ")
    (could-not-create-text . "\nCouldn't create the article. Error: %#1d.\n")
    (no-get-text . "You were not permitted to retreive the article.")


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

; used by lyskom-print-time
    (time-y-m-d-h-m . "%4#1d-%02#2d-%02#3d  %02#4d:%02#5d ")
; used by lyskom-return-time
    (time-yyyy-mm-dd-hh-mm . "%4#1d-%02#2d-%02#3d %02#4d:%02#5d")
    (no-such-text-no . "The article doesn't exist. (%#1d)\n")

    (head-Subject . "Subject: ")
    (Recipient . "Recipient")
    (Extra-recipient . "CC")
    (send-at . "   Posted:     %#1s\n")
    (sent-by . "   Posted by ")
    (recieved-at . "    Received: %#1s\n")
    (written-by . "by %#1s")
    (comment-to-text-by . "Comment to article %#1d %#2s\n")
    (footnote-to-text-by . "Footnote to article %#1d %#2s\n")
    (comment-in-text-by . "Comment in article %#1d %#2s\n")
    (footnote-in-text-by . "Footnote in article %#1d %#2s\n")

    ; From async.el:

    (name-has-changed-to-name . "%#1s has changed name to %#2s")
    (name-has-changed-to-name-r . "%#1s has changed name to %#2s\n")
    (database-sync . "Synching database.")
    (lyskom-is-full . "\
===========================================================
Message from the LysKOM-system: Somebody tried to connect,
but failed since all connections available to LysKOM are in
use. Please try later.
===========================================================\n")
    (has-entered . "%#1s has logged onto LysKOM.")
    (has-entered-r . "%#1s has logged onto LysKOM.\n")
    (has-left . "%#1s has left LysKOM.")
    (has-left-r . "%#1s has left LysKOM.\n")
    (unknown . "unknown")

    (message-broadcast . "Public message from %#1s (%#3s):\n\n%#2s\n")
    (message-from . "Personal message from %#1s (%#3s):\n\n%#2s\n")
    (text-is-created . "Article %#1d has been created!")


    ; From completing-read.el:

    (person-or-conf-no-regexp . "\\`[ \t]*[mpMP]\\w*[ \t]+\\([0-9]+\\)\\'")

    ; From prioritize.el:
    (cannot-get-membership . "Cannot retreive your subscription list.")
    (cannot-get-pers-stat . "Cannot retreive your personal status.")
    (your-membship . "Your subscriptions:
  Prio Conf# Conference\n")
    (prio-row . " %5#1d%5#2d  %#3s\n")
    (too-high-goto-2 . "You are too high,  move down to line two.")
    (too-low-go-up . "You can't push the last line. Move up one line.")
    (all-confs-popped .  "All conferences have been popped.")
    (prio-died . "Couldn't complete the move. Sorry. Kill the buffer.")
    (new-priority . "New priority? (0 (low) - 255 (high)) ")
    (new-prio . "%6#1d")

    ; From flags.el:
    (saving-settings . "Saving options")
    (hang-on . "Wait a moment...\n")
    (could-not-create-area . "Couldn't create the article.\n")
    (could-not-set-user-area . "Couldn't alter the user-area. The server says error: %#1d\n")
    (you-dont-exist . "You don't exist. Go away.\n")

    ; From elib-string.el:
    ; No entries.

    ; From lyskom-rest.el:

    (error-code . "Error code %#2d: %#1s.\n")
    (extended-command . "LysKOM: ")
    (wait-for-server . "LysKOM is waiting for a response from the server. Wait for the prompt.\n")
    (review-text-q . "Review article: ")
    (review-text-no . "Review article %#1d.\n")

    (completely-read-conf . "You have seen all the articles in this conference.\n")
    (not-in-any-conf . "You are not in a conference now.\n")

    (all-conf-unread-r . "You have nothing unread.\n")
    (all-conf-unread-s . "You have nothing unread. ")
    (one-unread . " - one unread article\n")
    (several-unread . " - %#1d unread articles\n")

    (save-on-file-q . "Save which article in file: (%#1s) ")
    (wait-for-prompt . "Wait for the prompt!")
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
    (the-command . "Command:%#1s")
    (error-in-do-when-starting . "Error in your kom-do-when-starting. %#1s\n")

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
    (conf-does-not-exist . "Conference %#1d (does not exist).")

    (process-signal . "Signal from the process.")
    (closed-connection . "
**************************************************
LysKOM session killed abnormallly
Error message: %#1s**************************************************")
    (error-not-found . "Error code %#1d. No explanation available.")

    ; Useful in more place than one:
    (illegal-command . "Illegal command.\n")
    (no-such-text . "The article doesn't exist.\n")
    (no-such-text-m . "The article doesn't exist.")
    (everybody . "everyone")
    (anybody . "anyone")
    (forward . "forward")
    (backward . "backward")
    (wait . "Wait a moment...\n")
    (comment . "Comment")
    (footnote . "Footnote")
    (by . " by %#1s")
    (text-created .  "Article %#1d has been created.\n")
    )
  "Assoc list containing pairs of atoms and strings")



;;; ================================================================
;;;              The commands and their associated functions


(defconst lyskom-commands
  '(
    (describe-mode              "Help")
    (kom-send-message           "Send message")
    (kom-create-conf		"Create conference")
    (kom-delete-conf            "Delete conference")
    (kom-delete-text            "Remove article")
    (kom-display-time		"Time")
    (kom-go-to-conf		"Go (to) conference")
    (kom-go-to-next-conf	"(Go to) next conference")
    (kom-jump			"Skip (all) comments")
    (kom-list-conferences 	"List conferences") 
    (kom-list-persons		"List users")
    (kom-list-news              "List news")
    (kom-membership		"List subscriptions")
    (kom-postpone		"Postpone reading")
    (kom-prioritize		"Prioritize conferences")
    (kom-status-person		"Status (of) user")
    (kom-status-conf		"Status (of) conference")
    (kom-add-self		"Subscribe (to) conference")
    (kom-list-summary		"List article (subjects)")
    (kom-sub-self               "Unsubscribe (to) conference")
    (kom-quit			"Quit")
    (kom-recover		"Recover") 
    (kom-start-anew		"New User")
    (kom-view			"Review article")
    (kom-find-root-review	"Review tree")
    (kom-review-comments	"Review all comments")
    (kom-review-tree		"Review all comments recursively")
    (kom-review-clear           "Review and skip")
    (kom-review-last-normally-read
     				"Review again")
    (kom-review-next            "Review next")
    (kom-find-root		"Review original (article)")
    (kom-review-by-to           "Review last")
    (kom-view-commented-text    "Review (the) commented (article)")
    (kom-review-stack           "Review stack")
    (kom-review-presentation    "Review presentation")
    (kom-review-backward        "(Review) Backwards")
    (kom-view-next-text		"(Read) next article")
    (kom-who-is-on		"Who (is on)") 
    (kom-display-who-buffer	"Display who (list)")
    (kom-busy-wait		"Wait (for news)")
    (kom-write-comment		"(Write) comment")
    (kom-comment-previous	"(Write) comment (to) previous article")
    (kom-write-footnote         "(Write) footnote")
    (kom-private-answer         "(Write) personal reply (by letter)")
    (kom-set-unread		"Only (the) last")
    (kom-write-text		"Write (an) article")
    (kom-send-letter		"Write (a) letter")
    (kom-change-name		"Change name")
    (kom-change-password        "Change password")
    (kom-change-supervisor	"Change organizer")
    (kom-change-presentation	"Change presentation")
    (kom-get-appreciation       "(Please) pat my head")
    (kom-get-abuse              "(Please) kick my butt")
    (kom-mark-text              "Mark (article)")
    (kom-unmark-text            "Unmark (article)")
    (kom-review-marked-texts    "Review marked (articles)")
    (kom-review-all-marked-texts "Review all marked (articles)")
    (kom-add-recipient          "Add recipient")
    (kom-add-copy		"Add (recipient of) carbon copy")
    (kom-sub-recipient          "Remove recipient")
    (kom-add-comment		"Add comment")
    (kom-sub-comment		"Remove comment")
    (kom-add-member		"Add subscriber")
    (kom-sub-member             "Remove subscriber")
    (kom-change-conf-motd	"(Post) note (on the) door")
    (kom-set-garb-nice          "Change expiration")
    (kom-set-super-conf         "Change superconference")
    (kom-unset-conf-motd	"Remove note (from the door)")
    (kom-save-text		"Save article (in file)")
    (kom-edit-options		"Change options")
    (kom-shutdown-server        "Shut down (server)")
    (kom-enable-adm-caps        "Become administrator")
    (kom-disable-adm-caps       "Become (normal) user")
    (kom-set-motd               "Change login message")
    (kom-remove-motd            "Remove login message")
    (kom-force-logout           "Kill session")
    )
  "A list of LysKOM-commands recognized by the extended parser.")


(defvar lyskom-text-start "
[0-9]+ 199[0-9]-[0-1][0-9]-[0-3][0-9] +[0-2][0-9]:[0-5][0-9]  /[0-9]+ line\\(s\\)?/ "
  "Regexp matching beginning of a article in LysKOM buffer.
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
  (define-key lyskom-mode-map "A" 'lyskom-change-prefix)
  (define-key lyskom-mode-map "r" 'lyskom-review-prefix)
  (define-key lyskom-mode-map "f" 'lyskom-get-prefix)
  (define-key lyskom-mode-map "n" 'lyskom-next-prefix)
  (define-key lyskom-mode-map "l" 'lyskom-list-prefix)
  (define-key lyskom-mode-map "s" 'lyskom-S-prefix)

	;(define-key lyskom-mode-map "vi" 'vilka)

  ; These should be first in order to be last in the menu of alternatives.
  (define-key lyskom-mode-map "A?" 'lyskom-help)
  (define-key lyskom-mode-map "r?" 'lyskom-help)
  (define-key lyskom-mode-map "f?" 'lyskom-help)
  (define-key lyskom-mode-map "n?" 'lyskom-help)
  (define-key lyskom-mode-map "l?" 'lyskom-help)
  (define-key lyskom-mode-map "s?" 'lyskom-help)
  

  (define-key lyskom-mode-map "o" 'kom-set-unread)
  (define-key lyskom-mode-map "x" 'kom-extended-command)
  (define-key lyskom-mode-map " " 'kom-next-command)
  (define-key lyskom-mode-map "\n" 'kom-page-next-command)
  (define-key lyskom-mode-map "\r" 'kom-line-next-command)

  (define-key lyskom-mode-map "?"  'describe-mode)
  (define-key lyskom-mode-map "m"  'kom-send-letter)
  (define-key lyskom-mode-map "g"  'kom-go-to-conf)
  (define-key lyskom-mode-map "a"  'kom-write-text)
  (define-key lyskom-mode-map "c"  'kom-write-comment)
  (define-key lyskom-mode-map "C"  'kom-comment-previous)
  (define-key lyskom-mode-map "F"  'kom-write-footnote)
  (define-key lyskom-mode-map "p"  'kom-private-answer)
  (define-key lyskom-mode-map "j"  'kom-jump)
  (define-key lyskom-mode-map "lc" 'kom-list-conferences)
  (define-key lyskom-mode-map "ln" 'kom-list-news)
  (define-key lyskom-mode-map "lN" 'kom-list-news-old)
  (define-key lyskom-mode-map "lu" 'kom-list-persons)
  (define-key lyskom-mode-map "ls" 'kom-membership)
  (define-key lyskom-mode-map "la" 'kom-list-summary)
  (define-key lyskom-mode-map "S"  'kom-add-self)
  (define-key lyskom-mode-map "M"  'kom-mark-text)
  (define-key lyskom-mode-map "U"  'kom-unmark-text)
  (define-key lyskom-mode-map "na" 'kom-view-next-new-text)
  (define-key lyskom-mode-map "nc" 'kom-go-to-next-conf)
  (define-key lyskom-mode-map "q"  'kom-quit)
  (define-key lyskom-mode-map "R"  'kom-recover)
  (define-key lyskom-mode-map "t"  'kom-display-time)
  (define-key lyskom-mode-map "fp" 'kom-get-appreciation)
  (define-key lyskom-mode-map "fk" 'kom-get-abuse)
  (define-key lyskom-mode-map "w"  'kom-who-is-on)
  (define-key lyskom-mode-map "W"  'kom-busy-wait)
  (define-key lyskom-mode-map "Ap" 'kom-change-presentation)
  (define-key lyskom-mode-map "r " 'kom-view)
  (define-key lyskom-mode-map "r0" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "r1" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "r2" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "r3" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "r4" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "r5" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "r6" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "r7" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "r8" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "r9" 'kom-initial-digit-view)
  (define-key lyskom-mode-map "rc" 'kom-view-commented-text)
  (define-key lyskom-mode-map "ra?" 'lyskom-help)
  (define-key lyskom-mode-map "rac" 'kom-review-comments)
  (define-key lyskom-mode-map "rar" 'kom-review-tree)
  (define-key lyskom-mode-map "rj" 'kom-review-clear)
  (define-key lyskom-mode-map "rn" 'kom-review-next)
  (define-key lyskom-mode-map "ro" 'kom-find-root)
  (define-key lyskom-mode-map "rl" 'kom-review-by-to)
  (define-key lyskom-mode-map "rg" 'kom-review-last-normally-read)
  (define-key lyskom-mode-map "B"  'kom-review-backward)
  (define-key lyskom-mode-map "rs" 'kom-review-stack)
  (define-key lyskom-mode-map "rp" 'kom-review-presentation)
  (define-key lyskom-mode-map "rr" 'kom-find-root-review)
  (define-key lyskom-mode-map "rm" 'kom-review-marked-texts)
  (define-key lyskom-mode-map "ram" 'kom-review-all-marked-texts)
  (define-key lyskom-mode-map "sc" 'kom-status-conf)
  (define-key lyskom-mode-map "su" 'kom-status-person)

  ;; Running in buffer

  (define-key lyskom-mode-map "\033p" 'backward-text)
  (define-key lyskom-mode-map "\033n" 'forward-text)
  (define-key lyskom-mode-map "sa" 'kom-save-text)

  (define-key lyskom-mode-map "\C-?" 'scroll-down)
)



(defvar lyskom-prioritize-mode-map nil
  "Keymap used in lyskom-prioritize-mode.")


(if lyskom-prioritize-mode-map
    nil
  (setq lyskom-prioritize-mode-map (make-keymap))
  (suppress-keymap lyskom-prioritize-mode-map)
  (define-key lyskom-prioritize-mode-map "\C-?" 'previous-line)
  (define-key lyskom-prioritize-mode-map " "    'next-line)
  (define-key lyskom-prioritize-mode-map "\C-k" 'kom-prioritize-kill)
  (define-key lyskom-prioritize-mode-map "\C-y" 'kom-prioritize-yank)
  (define-key lyskom-prioritize-mode-map "p"     'kom-prioritize-set-priority)
  (define-key lyskom-prioritize-mode-map "\C-c\C-c" 'kom-prioritize-quit)
  (define-key lyskom-prioritize-mode-map "q"     'kom-prioritize-quit)
  (define-key lyskom-prioritize-mode-map "Q"     'kom-prioritize-quit)
  (define-key lyskom-prioritize-mode-map "u"     'kom-prioritize-move-up)
  (define-key lyskom-prioritize-mode-map "d"     'kom-prioritize-move-down)
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
    (kom-tell-send		"Is trying to post an article.")
    (kom-tell-login		"Is logging in.")
    (kom-tell-read		"Is reading.")
    (kom-tell-1st-pres		"Is writing the first presentation.")
    (kom-tell-write-comment	"Is writing a comment.")
    (kom-tell-write-footnote	"Is writing a footnote.")
    (kom-tell-write-letter	"Is writing a letter.")
    (kom-tell-write-reply	"Is writing a personal reply.")
    (kom-tell-write-text	"Is writing an article.")
    (kom-tell-conf-pres		"Is writing the presentation for a new conference.")
    (kom-tell-recover		"Performs a reset. Sigh.")
    (kom-tell-wait		"Is waiting.")
    (kom-tell-regret		"Decides to throw away the article.")
    (kom-tell-review		"Is reviewing.")
    (kom-tell-change-name       "Takes on a new name.")
    (kom-tell-change-supervisor "Changes the organizer for something.")
    )
  "These texts are used as arguments to lyskom-tell-server.
The variable kom-mercial defaults to kom-tell-wait.
Users are encouraged to change this dictionary for fun.")


;; Placed here because this must NOT be evaluated before 
;; kom-tell-phrases is defined:

(defvar kom-mercial (car (cdr (assoc 'kom-tell-wait kom-tell-phrases)))
  "*When the user has seen all articles and has reached the view-time prompt,
this string is used as the argument to lyskom-tell-server.
Users are encouraged to use their best sense of humor.")


(defconst lyskom-error-texts
  '((0 . "No error")
    (2 . "Not yet implemented")
    (3 . "No longer implemented")
    (4 . "Wrong password")
    (5 . "String too long")
    (6 . "You have not logged in")
    (7 . "Nobody may enter LysKOM at this time")
    (8 . "You attempted to use conference number 0")
    (9 . "Undefined or secret conference")
    (10 . "Undefined or secret user")
    (11 . "No read or write permission")
    (12 . "Illegal operation")
    (13 . "You do not subscribe to that conference")
    (14 . "There is no article with that number")
    (15 . "You cannot use global article number 0")
    (16 . "There is no article with that local number")
    (17 . "You cannot user local article number 0")
    (18 . "Illegal characters in name")
    (19 . "Index out of bounds")
    (20 . "The conference already exists")
    (21 . "The user already exists")
    (22 . "Secret, but not read-protected")
    (23 . "You are not permitted to change the erson/conference flag")
    (24 . "Error in the dtabase. Tough luck.")
    (25 . "Illegal misc-field. (Internal error)")
    (26 . "Illegal info type. (Bug in the client)")
    (27 . "Already recipient of this article")
    (28 . "Already comment to this article")
    (29 . "Already footnote to this article")
    (30 . "Not a recipient of this article")
    (31 . "Not a comment to this article")
    (32 . "Not a footnote to this article")
    (33 . "Too many recipients")
    (34 . "Too many commentsp")
    (35 . "Too many footnotes")
    (36 . "Too many marks")
    (37 . "You are not the author of this article")
    (38 . "You cannot connect to the server")
    (39 . "Out of memory")
    (40 . "The server is gone crazy")
    (41 . "The client thinks that the server says that it does not understand the client")
    (42 . "No such session"))
  "All the errors reported from the server in plain text.")
