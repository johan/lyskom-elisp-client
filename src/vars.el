;;;;;
;;;;; $Id: vars.el,v 36.2 1993-04-27 00:01:47 linus Exp $
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
;;;; File: vars.el
;;;;
;;;; This file contains almost all the variables used in lyskom.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: vars.el,v 36.2 1993-04-27 00:01:47 linus Exp $\n"))



(provide 'lyskom)


;;;; ================================================================
;;;;                    Emacs dependant variables

(defvar kom-emacs-knows-iso-8859-1 nil
  "*If non-nil then dont convert texts and other things to swascii.
This variable is not saved. It should be set in your .emacs")


;;;; ================================================================
;;;;                     Variables and constants.


;;; User flags

(defvar kom-write-texts-in-window nil
  "*Nil means edit texts in the same window that is used for reading texts.
'other means edit in another window. Creating that window if necessary.
A buffer or buffer name means use the first window displaying that buffer.
If no window is displaying that buffer or the buffer does not exits then it
uses the same window.")

(defvar kom-created-texts-are-read t
  "*Non-nil means automatically mark texts that you create as read.")

(defvar kom-default-mark 100
  "*If non-nil (must be an integer) then the user is not asked for type of mark.")

(defvar kom-reading-puts-comments-in-pointers-last t
  "*If Non-nil the texts are shown with comments references at the end.")

(defvar kom-dashed-lines t
  "*If Non-nil then all texts will be surrounded by lines of dashes.")

(defvar kom-print-number-of-unread-on-entrance t
  "*If Non-nil then print automatically the number of unread articles when
entering a conference.")

(defvar kom-presence-messages t
  "*If non-nil, lyskom prints continuous info about what other people are doing.
Info is printed on the message line and never in the buffer. If minibuffer is
used then no message is printed.")

(defvar kom-presence-messages-in-buffer nil
  "*If t, lyskom prints information about what other people are doing in buffer.
All printing is done just before the prompt.
If nil no messages are printed.
If 'presence then messages about people logging in, out and people 
changing name is printed.")

(defvar kom-show-where-and-what t
  "*Non-nil means kom-who-is-on shows from which machine the user is running
and what he is doing.")

(defvar kom-who-buffer-size-when-displaying 10
  "*Size of window to display the who-buffer.
This is used when executing the kom-display-who-buffer command.")

(defvar kom-read-depth-first t
  "*Non-nil means read comments and footnotes to a text before other texts.")

(defvar kom-higher-priority-breaks nil
  "*Non-nil means allow texts from conferences with highter priority to break in.
If the value is 'express then texts are allowed to break in the middle of a
comment chain.  Otherwise we dont let them in until the end of the comment
tree.")

(defvar kom-do-when-starting nil
  "*What to do when logged in.
This is a list of commands that are executed after we have logged in but before
any command is accepted from the keyboard.
The syntax is the same as for the kom-do-when-starting variable except that the
last element is not remembered forever. These are executed as soon as we get a
new prompt i.e. non-stop at login.")

(defvar kom-do-when-done '(kom-review-all-marked-texts kom-display-time)
  "*What to do when all texts are read.
This is a list of commands and lists of commands that are prompted for
and executed when there are no more new texts.  The last element in
the list is the one that will never be removed from the list.
A command can be one of:
type			prompt
lyskom-function		from the lyskom-command-name function.
command			\"Kommandot:\" name of function or definition of
			lambda expression
keyboard macro		\"Kommandot:\" keyboard macro definition.")

(defvar kom-page-before-command nil
  "*This is a list of all commands before which the screen is cleared.
If it isn't a list and isn't nil the screen is cleared before all commands.")

(defvar kom-membership-default-priority 100
  "*Default priority when joining a new conference.
If a valid priority then new conferences are read with this priority. 
Otherwise ask the user for a priority.

Valid priorities are only the range 0-255.")

(defvar kom-membership-default-placement 'last
  "*Tells the system where to put new conferences.
The value can be one of the following:
'first => before all other conferences.
'last => after all other conferences.
a number => at that position
otherwise => the new conf is entered after all conferences.")

(defvar lyskom-prompt-text " - "
  "*Text to end the prompt.")

(defvar lyskom-prompt-executing-default-command-text ". "
  "*Text to end the prompt while the default-commands is executing.")

(defvar kom-show-personal-messages-in-buffer t
  "*Buffer to show personal messages in.
If nil, discard them.
If t, insert them in the *kom* buffert.
If non-nil and non-t this should be a buffer or a name of a (possibly
nonexistent) buffer in which the message is inserted.")

(defvar kom-pop-personal-messages nil
  "*Non-nil means pop up a buffer with personal messages as they arrive.
kom-show-personal-messages-in-buffer decides which buffer to pop.")

(defvar kom-ding-on-personal-messages t
  "*Non-nil means ding as personal messages arrive.
Non-nil and non-t means only ding when they really are personal.")

(defvar kom-confirm-multiple-recipients t
  "*Non-nil means ask the user for confirmation about recipients.
When the user writes a comment to a text with more than one recipient
he gets a y-or-n-p question for all recipients.")

(defconst lyskom-elisp-variables '(
  kom-do-when-done
  kom-do-when-starting
  kom-mercial 
  kom-higher-priority-breaks
  kom-membership-default-placement
  kom-membership-default-priority
  kom-page-before-command 
  kom-presence-messages-in-buffer
  kom-show-where-and-what
  kom-who-buffer-size-when-displaying
  kom-write-texts-in-window
  kom-quit-hook
  lyskom-fetch-map-nos
  lyskom-new-text-hook
  lyskom-prefetch-conf-tresh 
  lyskom-prefetch-confs
  lyskom-prefetch-texts
  lyskom-who-info-has-changed-hook
  lyskom-prompt-text
  lyskom-prompt-executing-default-command-text
  lyskom-login-hook
  )
  "Tells the client what flags and hooks that are to be saved in the server.
These are the flags that are saved in the elisp-client part of the server.")

(defconst lyskom-global-boolean-variables '(
  kom-created-texts-are-read
  kom-dashed-lines
  kom-presence-messages
  kom-print-number-of-unread-on-entrance
  kom-read-depth-first
  kom-reading-puts-comments-in-pointers-last
  kom-confirm-multiple-recipients
)
  "List of flags that are to be saved as booleans in the common block.

Dont change this. These are defined by the protocol.")
				  
(defconst lyskom-global-non-boolean-variables '(
  kom-default-mark
)
  "List of flags that are to be saved in the common block.
These are the non-boolean ones. See: lyskom-global-boolean-variables.

Dont change these. These are defined by the protocol.")


;;; ================================================================
;;;                  Internal variables and constants


(defconst lyskom-clientversion "0.36"
  "Version of the LysKOM elisp client.")

(defconst lyskom-max-int 8388607
  "The largest int emacs, and thus this LysKOM client, can handle.")

(defvar lyskom-max-packet-size lyskom-max-int
  "The largest possible packet size that can be transmitted to a
TCP/IP connection. This should be unlimited, but in practise there
are systems that limits this. This variable is automatically adjusted
if any problems are detected.")

(defvar lyskom-unparsed-buffer nil
  "Buffer containing unparsed information from the server.")

(defvar lyskom-unparsed-marker nil
  "Here are we inserting now.")

(defvar lyskom-to-be-printed-before-prompt nil
  "Contains the strings to be printed out before the next prompt.")

(defvar lyskom-other-clients-user-areas nil
  "Contains the parts of the user areas of unknown klients.
The area is a pair: name . info (both strings).")

(defvar lyskom-pending-calls nil
  "Assoc-list of calls to LysKOM server that have not yet completed.
Each element on the list has the format
	(REF-NO . KOM-QUEUE)
REF-NO	   unique number assigned by lyskom-send-packet.
KOM-QUEUE is a kom-queue. (See lyskom-call-data).")

(defvar lyskom-output-queue nil
  "Pending output to the server.
At most lyskom-max-pending-calls calls are sent at once.")

(defvar lyskom-max-pending-calls 20
  "*Max number of calls that are transmitted to the server at once.
Extra calls are queued in lyskom-output-queue and sent when the replies
returns.")

(defvar lyskom-number-of-pending-calls 0
  "Number of pending calls that are transmitted to the server.")

(defvar lyskom-ref-no 0
  "Next ref-no to use. These ref-nos are used to keep track of the
different packets.")

(defvar lyskom-pers-no 0
  "The pers-no of the current user.")

(defvar lyskom-session-no 0
  "Session number in the server for this connection")

(defvar lyskom-proc nil
  "The process (network connection) that is associated with this buffer.")

(defvar lyskom-server-info nil
  "Info about the server")


(defvar lyskom-conf-cache nil
  "Cache of conference statuses.")

(defvar lyskom-pers-cache nil
  "Cache of person statuses.")

(defvar lyskom-text-cache nil
  "Cache of text statuses.")

(defvar lyskom-text-mass-cache nil
  "Cache of texts.")

(defvar lyskom-marked-text-cache nil
  "Cache of marks of all texts the current user has marked. ")

(defvar lyskom-who-info-cache nil
  "Cache of people presently logged in in LysKOM.")

(defvar lyskom-who-info-buffer nil
  "Buffer for the who info presentation.")

(defvar lyskom-who-info-buffer-is-on t
  "Says wether we are collecting who-information or not.")


(defvar lyskom-is-parsing t
  "True when parsing a result.
This is used to prevent parallell parsing since the parser is not reentrant.")

(defvar lyskom-string-bytes-missing 0
  "Number of bytes missing in the unparsed buffer when parsing a string.
Set when parsing a string and there were not enough bytes in the buffer
with the unparsed bytes. This variable is used to prevent reparsing before 
the string is complete.
This variable is buffer-local in the unparsed-buffer.")

(defvar lyskom-last-viewed 0
  "Postition of the first char of the last line that the user has had
time to view. This is normally the pos of the first char of the prompt.")

(defvar lyskom-mode-map nil
  "Keymap used in LysKOM mode.")


(defvar lyskom-reading-list nil
  "List of articles to read in the current conference.
Each element is a read-info. Only one of the elements is of the type CONF.
This one is located last in the list (except for the elements of the type 
REVIEW, REVIEW-TREE or REVIEW-MARK).
When reading an article with comments a list of the comments is built
recursively if the flag kom-read-depth-first is non-nil.
This is to keep track of the reading order.
Articles can exist in several of the read-info elements. All unread articles
in the conference are always present in the CONF type entry in this list even
if also in other entries. (COMM-IN, FOOTN-IN)

Some powerful reviewing commands requires to construct a list of articles that
should be read. These use the type REVIEW. When reviewing trees and when
every viewed article is supposed to be followed by all its comments then the
type REVIEW-TREE is used.

The first element is a dummy.")

(defvar lyskom-to-do-list nil
  "List of conferences with unread texts.
Each element is a read-info. All have the type 'CONF and there is one for
every conference with unread articles that have been prefetched already.
The list is sorted in falling priority. 
When going to a conference the first element (the one with the highest
priority) is copied from this list to lyskom-reading-list.

The first element is a dummy.")


;;; These variables control prefetch of conf-stats, text-stats and texts:

(defvar lyskom-prefetch-texts 3
  "*Number of texts to prefetch.")

(defvar lyskom-prefetch-conf-tresh 50
  "*If fewer than lyskom-prefetch-conf-tresh texts are known, ask for more
conf-stats from server.")

(defvar lyskom-prefetch-confs 10
  "*Number of confs to ask about at once when checking for unread texts.")

(defvar lyskom-fetch-map-nos 50
  "*Number of text-nos lyskom will fetch when fetching maps.
+++ This is not yet used everywhere.")

(defvar lyskom-fetch-membership-length 6
  "*Number of entries in the membership-list that is fetched at a time.
This should be optimized depending on how often you read lyskom and
the activity in the first groups in you membership list.

Best performance is achieved if you, when logging in, always have an unread
article in one of the first lyskom-fetch-membership-length conferences.")

(defvar lyskom-last-conf-fetched -1
  "Last conf asked about. Number on the list lyskom-membership.")

(defvar lyskom-last-conf-received -1
  "Last conf known about. See lyskom-last-conf-fetched.")



(defvar lyskom-membership nil
  "Sorted membership-list of the logged in person.")

(defvar lyskom-unread-confs nil
  "List containing all unread confs.")

(defvar lyskom-dont-change-prompt nil
  "Non-nil during the entry of a text.")

(defvar lyskom-fetched-texts nil
  "List of texts that are prefetched.")

(defvar lyskom-command-to-do 'unknown
  "Atom describing what command to do. See the function lyskom-what-to-do.")

(defvar lyskom-is-waiting nil
  "If non-nil then this is the condition for the waiting to be stopped.
If t however just meaning user is waiting for a text with prompt.
It is a form that will be evaluated (using eval) every time the asynchronous
message \"new text\" is received.

This is used by the command kom-busy-wait.")

(defvar lyskom-current-conf 0
  "Current conference. 0 means user is not reading any conf.")

(defvar lyskom-current-text nil
  "Text-no of current text. nil means no text is current.")

(defvar lyskom-previous-text nil
  "Text-no of previous text.  Nil means no text.")

(defvar lyskom-normally-read-texts nil
  "Stack of texts that are read normally. Used for kom-review-last-normally-read.")

(defvar lyskom-current-subject ""
  "Current subject.")

(defvar lyskom-last-added-rcpt 0
  "The default conference when adding a recipient.")

(defvar lyskom-last-added-ccrcpt 0
  "The default conference when adding a ccrecipient.")

(defvar lyskom-saved-file-name (concat default-directory "kom-text")
  "The default file name when saving a lyskom-text.")

(defvar lyskom-mode-hook nil
  "Hook to run when lyskom-mode is entered.")

(defvar kom-quit-hook nil
  "Hook to run when the LysKOM session is correctly ended.")

(defvar lyskom-new-text-hook nil
  "*Hook to run when a new text is created.
This hook is run after the prompt is removed if it shall be changed but before
the text Text 4711 {r skapad! is printed in the message area. And before the
new prompt is printed.
If the text: Text 4711 {r skapad! should not be printed then the hook should 
set the local variable no-message non-nil.")

(defvar lyskom-who-info-has-changed-hook nil
  "*Hook to run every time the who-info-buffer has changed.
The hook is run with current-buffer the lyskom buffer, not the who-info-buffer.")

(defvar lyskom-login-hook nil
  "*Hook to run when we have logged in. Other purposes than kom-do-when-starting.
This could be set in the user-area.")

(defvar lyskom-personal-message-hook nil
  "*Hook to run when a personal message is received.
When the hook is run 'sender' is bound to the pers-stat of the sender
of the message (or possibly nil), 'recipient' is 0 if the message is a
public message and otherwise the pers-no of the user, and 'message' is
a string that holds the message.")


(defvar lyskom-executing-command t
  "Non-nil means the client is executing a command.
Most commands can't be interrupted by another command.")

(defvar kom-low-priority -1
  "*Priority that the current conference are set to when they are
aborted. nil means don't alter priority. (That means that
kom-go-to-next-conf might go to the same conference again.)")

(defvar lyskom-membership-is-read nil
  "T when the membership has been read.")

(defvar lyskom-is-writing nil
  "t when the user is writing a text.")

(defvar lyskom-debug-communications-to-buffer nil
  "Non-nil means all communications with the server is stored in a buffer.
The name is stored in lyskom-debug-communications-to-buffer-buffer.")
  
(defvar lyskom-debug-communications-to-buffer-buffer nil
  "Name of the buffer to insert the communications with the server into if
lyskom-debug-communications-to-buffer is non-nil.")


(defvar lyskom-count-var 0
  "This variable is used for counting things in the client, such as
unread texts in list-unread.")


(defvar lyskom-is-administrator nil
  "This variable is t if the user is in administrator mode and nil otherwise.")


;;; ================================================================
;;;          Externally defined variables (environment)


(defvar lyskom-default-server "kom.lysator.liu.se"
  "*Default LysKOM server.")

(defvar lyskom-default-user-name nil
  "*Default LysKOM user name.")

(defvar lyskom-default-password nil
  "Default LysKOM PASSWORD.")
