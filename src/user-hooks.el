;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: user-hooks.el,v 44.1 2002-05-07 20:12:13 byers Exp $
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
;;;; File: user-hooks.el
;;;;
;;;; This file contains hook functions that users might be interested in.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: user-hooks.el,v 44.1 2002-05-07 20:12:13 byers Exp $\n"))

(def-kom-var kom-list-summary-hook-conf-list nil
  "This variable controls the behavior of kom-list-summary-hook.
When kom-list-summary-hook is used in kom-after-change-conf-hook, 
this variable specifies which conferences to automatically to list
summary for. It is a list of conference numbers"
  server)

(def-kom-var kom-read-last-text-hook-conf-list nil
  "This variable controls the behavior of kom-read-last-text-hook.
When kom-read-last-text-hook is used in kom-change-conf-hook, 
this variable specifies which conferences the hook applies to. Each
element is either a conference number of a cons cell consisting of
a conference number and an integer.

If a conference number appears in the list alone, the most recent
text in that conference will be marked as unread upon entering the 
conference.

If a cons cell (CONF-NO . NUMBER) is in the list, the NUMBER most 
recent texts (approximately) will be marked as unread on entering
conference CONF-NO."
  server)

(defun kom-list-summary-hook (from-conf to-conf)
  "Automatically list text summary when entering a conference. This
function can be added to kom-after-change-conf-hook. Which conferences to
list subjects in is controlled by
kom-list-summary-hook-conf-list"
  (when (or (eq kom-list-summary-hook-conf-list t)
            (memq to-conf kom-list-summary-hook-conf-list))
    (lyskom-list-summary to-conf)))

(defun kom-read-last-text-hook (from-conf to-conf)
  "Automatically read only the most recent N texts in a conference.
This function can be added to kom-change-conf-hook. Which conferences
to perform this in is controlled by kom-read-last-text-hook-conf-list.

Not that even unread texts may be read again when usign this function."
  (let* ((spec (or (and (memq to-conf kom-read-last-text-hook-conf-list) t)
                   (assq to-conf kom-read-last-text-hook-conf-list)))
         (arg (and spec (if (consp spec) (cdr spec) 1))))
    (when (and arg (blocking-do 'set-unread to-conf arg))
      (let ((membership (blocking-do 'query-read-texts lyskom-pers-no 
                                     to-conf))
            (read-info nil))
        (lyskom-replace-membership membership)
        (read-list-delete-read-info to-conf lyskom-to-do-list)
        (read-list-delete-read-info to-conf lyskom-reading-list)

        (setq read-info
              (lyskom-create-read-info 'CONF
                                       (blocking-do 'get-conf-stat to-conf)
                                       (lyskom-get-current-priority)
                                       (listify-vector
                                        (map->text-nos 
                                         (blocking-do 'get-map 
                                                      to-conf
                                                      (membership->last-text-read membership)
                                                      lyskom-max-int)))))
        (read-list-enter-first read-info lyskom-to-do-list)
        (read-list-enter-first read-info lyskom-reading-list)))))
