;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: mship-edit.el,v 44.50 2004-10-29 10:41:09 _cvs_pont_lyskomelisp Exp $
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
;;;; File: mship-edit.el
;;;;
;;;; Mode for editing the membership. Replaced prioritize.el
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: mship-edit.el,v 44.50 2004-10-29 10:41:09 _cvs_pont_lyskomelisp Exp $\n"))

;; KNOWN BUGS AND TO DO
;; --------------------
;;
;; Messed up redraw of an entry
;;
;; Sometimes an entry will be redrawn and appear twice, with one of
;; the old entries on the same line as one of the duplicates. This 
;; seems to happen if two redraws using delayed printing are initiated
;; at once (the second starts before the first one finishes).
;;
;; Remembers that lyskom-insert-membership and lyskom-replace-membership
;; call lp--update-buffer, so it's rarely necessary to call it from 
;; anywhere else.
;;
;; Endast-function to all marked memberships
;;
;; Uppskjut-function to all marked memberships
;;
;; Remove and add membership functions.
;;


(require 'advice)

;;; Code:

(def-komtype lp--entry
  (start-marker                         ; Where the entry is in the buffer
   end-marker                           ; Where it ends in the buffer
   priority                             ; The saved priority of the membership
   membership                           ; The membership
   selected                             ; Selected or not
   state                                ; Expanded display or not
   visible                              ; Non-nil when visible
   extents                              ; Alist of extents/overlays
   ))

(defvar lp--format-string nil "The format string for entries.")

;;; Local variables in the prioritize buffer
;;; There should be no reason to use these at all. There are functional
;;; abstractions that let you access their contents.

(defvar lp--entry-list nil)
(defvar lp--headers nil)
(defvar lp--header-end-marker nil)
(defvar lp--list-start-marker nil)
(defvar lp--list-end-marker nil)
(defvar lp--selected-entry-list nil)
(defvar lp--buffer-done nil)
(defvar lp--conf-name-width nil)
(defvar lp--inhibit-update nil)
(defvar lp--hidden-entries nil)
(defvar lp--entry-filter nil)



;;; ============================================================
;;; Utility functions and really basic stuff

(defun lp--entry-update-extents (entry)
  "Update the start and end positions for extents of ENTRY.
Update the start and end positions of all extents or overlays listed
in the extent list of ENTRY to match the start and end markers. If the
start or end markers point nowhere, detatch the extents. If overlays
are used, set the start and end positions to zero."
  (let ((extents (lp--entry->extents entry)))
    (while extents
      (lyskom-xemacs-or-gnu 
       (progn (set-extent-property (cdr (car extents)) 'end-open t)
              (set-extent-property (cdr (car extents)) 'start-open t)
              (set-extent-endpoints (cdr (car extents))
                                    (lp--entry->start-marker entry)
                                    (lp--entry->end-marker entry)))
       (progn (move-overlay (cdr (car extents)) 
                            (or (lp--entry->start-marker entry) 0)
                            (or (lp--entry->end-marker entry) 0))))
      (setq extents (cdr extents)))))

(defun lyskom-change-membership-priority (conf-no new-priority)
  "Change the priority of memberhip for CONF-NO to NEW-POSITION.
This function does not tell the server about the change."
  (let* ((mship (lyskom-get-membership conf-no t))
         (old-priority (and mship (membership->priority mship))))
    (when mship
      (set-membership->priority mship new-priority)
      (lyskom-replace-membership mship)

      (cond
       ((and (>= old-priority lyskom-session-priority)
             (>= new-priority lyskom-session-priority))
        ;; Do nothing
        )

       ((and (< old-priority lyskom-session-priority)
             (>= new-priority lyskom-session-priority))
        (let ((mship (lyskom-get-membership conf-no t)))
          (when mship (lyskom-prefetch-map conf-no mship))))

       ((and (>= old-priority lyskom-session-priority)
             (< new-priority lyskom-session-priority))
        (read-list-delete-read-info conf-no lyskom-to-do-list)
        (when (eq conf-no lyskom-current-conf)
          (lyskom-leave-current-conf))
        (lyskom-update-prompt t))
      ))))

(defun lyskom-change-membership-position (conf-no new-position)
  "Change the position of memberhip for CONF-NO to NEW-POSITION.
This function does not tell the server about the change."
  (let* ((mship (lyskom-get-membership conf-no t))
         (old-position (membership->position mship)))
    (when mship
      (set-membership->position mship new-position)
       (lyskom-replace-membership mship))))


;;; ============================================================
;; Entry drawing

(defun lp--entry-set-background (entry color)
  "Use extents or overlays to set the background of ENTRY to COLOR."
  (save-excursion
    (set-buffer (marker-buffer (lp--entry->start-marker entry)))
    (if (null color)
        (let* ((extent (assq 'color (lp--entry->extents entry))))
          (when extent
            (lyskom-xemacs-or-gnu
             (delete-extent (cdr extent))
             (delete-overlay (cdr extent)))
            (set-lp--entry->extents entry 
                                    (delq extent (lp--entry->extents entry)))))

      (let* ((extent (cdr (assq 'color (lp--entry->extents entry))))
             (facename (intern (format "lyskom-%s-background" 
				       (lyskom-safe-color-name color))))
             (face (or (lyskom-find-face facename) (lyskom-make-face facename t))))
        (unless extent
          (lyskom-xemacs-or-gnu
           (setq extent (make-extent (lp--entry->start-marker entry)
                                     (lp--entry->end-marker entry)))
           (setq extent (make-overlay (lp--entry->start-marker entry)
                                      (lp--entry->end-marker entry)
                                      nil t)))
          (set-lp--entry->extents entry (cons (cons 'color extent)
                                              (lp--entry->extents entry))))
        (set-face-background face color)
        (lyskom-xemacs-or-gnu (progn (set-extent-property extent 'end-open t)
                                     (set-extent-property extent 'start-open t)
                                     (set-extent-property extent 'priority 1000)
                                     (set-extent-property extent 'face face))
                              (progn (overlay-put extent 'priority 1000)
                                     (overlay-put extent 'face face)))))))

(defun lp--entry-set-foreground (entry color)
  "Use extents or overlays to set the foreground of ENTRY to COLOR."
  (save-excursion 
    (set-buffer (marker-buffer (lp--entry->start-marker entry)))
    (if (null color)
        (let* ((extent (assq 'fcolor (lp--entry->extents entry))))
          (when extent
            (lyskom-xemacs-or-gnu
             (delete-extent (cdr extent))
             (delete-overlay (cdr extent)))
            (set-lp--entry->extents entry 
                                    (delq extent (lp--entry->extents entry)))))

      (let* ((extent (cdr (assq 'fcolor (lp--entry->extents entry))))
             (facename (intern (format "lyskom-%s-foreground" 
				       (lyskom-safe-color-name color))))
             (face (or (lyskom-find-face facename) (lyskom-make-face facename t))))
        (unless extent
          (lyskom-xemacs-or-gnu
           (setq extent (make-extent (lp--entry->start-marker entry)
                                     (lp--entry->end-marker entry)))
           (setq extent (make-overlay (lp--entry->start-marker entry)
                                      (lp--entry->end-marker entry) 
                                      nil t)))
          (set-lp--entry->extents entry (cons (cons 'fcolor extent)
                                              (lp--entry->extents entry))))
        (set-face-foreground face color)
        (lyskom-xemacs-or-gnu (progn (set-extent-property extent 'end-open t)
                                     (set-extent-property extent 'start-open t)
                                     (set-extent-property extent 'priority 1000)
                                     (set-extent-property extent 'face face))
                              (progn (overlay-put extent 'priority 1000)
                                     (overlay-put extent 'face face)))))))



(defmacro lp--save-excursion (&rest body)
  "Evecute BODY and restore the current location of point.
The location of point is in relation to the entry it is in.  Point
will move as the entry moves.  The location of point is restored
even if the character at point is deleted."
  `(let* ((lp--saved-entry  (lp--entry-at (point)))
          (lp--saved-column (and lp--saved-entry
                                 (- (point)
                                    (lp--entry->start-marker
                                     lp--saved-entry)))))
     (save-excursion ,@body)
     (if (and lp--saved-entry
              (lp--entry->start-marker lp--saved-entry))
         (goto-char (+ lp--saved-column (lp--entry->start-marker
                                         lp--saved-entry))))))


(defun lp--compute-format-string ()
  "Compute the format string for an entry in the buffer.
To save time, the format string is cached in `lp--format-string'.  It is
only recomputed if the window width changes."
  (let ((total (- (window-width) 1 3 3 2 12 2 5 2 4 1)))
    (setq lp--conf-name-width total)
    (setq lp--format-string
          (concat "%#1c %=3#2s %#10c %=-" (number-to-string total)
                  "#3M  %=-12#4s %[%#15@%=5#5s%]  %[%#11@%#6c%]%[%#12@%#7c%]%[%#13@%#8c%]%[%#14@%#9c%]"))))

(defun lp--format-insert-entry (entry)
  "Format ENTRY and insert it into the current buffer at point."
  (let* ((mship-conf-no (membership->conf-no (lp--entry->membership entry)))
         (adder-conf-no (membership->created-by (lp--entry->membership entry)))
         (mship-conf-stat (cache-get-conf-stat mship-conf-no))
         (adder-conf-stat (cache-get-conf-stat adder-conf-no)))

    (if (null mship-conf-stat)
        (lyskom-format-insert-at-point 
         "%#1D"
         (lyskom-create-defer-info 'get-conf-stat
                                   mship-conf-no
                                   'lp--format-entry
                                   (point-marker)
                                   (length lyskom-defer-indicator)
                                   "%#1s"
                                   entry))
      (lp--format-entry mship-conf-stat entry))

    (when (eq (lp--entry->state entry) 'expanded)
      (lyskom-insert-at-point "\n        ")
      (if (null adder-conf-stat)
          (lyskom-format-insert-at-point 
           "%#1D"
           (lyskom-create-defer-info 'get-conf-stat
                                     adder-conf-no
                                     'lp--format-entry-expansion
                                     (point-marker)
                                     (length lyskom-defer-indicator)
                                     "%#1s"
                                     entry))
        (lp--format-entry-expansion adder-conf-stat entry)))))


(defun lp--format-entry (conf-stat defer-info)
  (let ((entry nil))
    (if (lyskom-lp--entry-p defer-info)
        (progn (setq entry defer-info)
               (setq defer-info nil))
      (setq entry (defer-info->data defer-info)))
    (let* ((un (lyskom-find-unread (membership->conf-no
                                    (lp--entry->membership entry))))
           (string (lyskom-format
                    lp--format-string
                    (if (lp--entry->selected entry) ?* ?\ )
                    (if (zerop (membership->priority
                                (lp--entry->membership entry)))
                        "-"
                      (int-to-string (membership->priority
                                      (lp--entry->membership entry))))
                    (if (null conf-stat)
                        (lyskom-format 'conference-does-not-exist 
                                       (membership->conf-no 
                                        (lp--entry->membership entry)))
                      conf-stat)
                    (lyskom-format-time
                     'date
                     (membership->last-time-read (lp--entry->membership entry)))
                    (if un (int-to-string un) "")

                    (if (membership-type->invitation (membership->type (lp--entry->membership entry))) ?I ?.)
                    (if (membership-type->secret (membership->type (lp--entry->membership entry))) ?H ?.)
                    (if (membership-type->passive (membership->type (lp--entry->membership entry))) ?P ?.)
                    (if (membership-type->message-flag (membership->type (lp--entry->membership entry))) ?M ?.)
                    (if (and conf-stat (eq lyskom-pers-no (conf-stat->supervisor conf-stat))) ?O ?\ )
                    (lyskom-default-button 'prioritize-flag-menu
                                           (list entry 'invitation)
                                           (list "%#1s (%=#2M)"
                                                 (lyskom-get-string 'Invitation-mt-type)
                                                 (membership->conf-no
                                                  (lp--entry->membership entry))))
                    (lyskom-default-button 'prioritize-flag-menu
                                           (list entry 'secret)
                                           (list "%#1s (%=#2M)"
                                                 (lyskom-get-string 'Secret-mt-type)
                                                 (membership->conf-no
                                                  (lp--entry->membership entry))))
                    (lyskom-default-button 'prioritize-flag-menu
                                           (list entry 'passive)
                                           (list "%#1s (%=#2M)"
                                                 (lyskom-get-string 'Passive-mt-type)
                                                 (membership->conf-no
                                                  (lp--entry->membership entry))))
                    (lyskom-default-button 'prioritize-flag-menu
                                           (list entry 'message-flag)
                                           (list "%#1s (%=#2M)"
                                                 (lyskom-get-string 'Message-flag-mt-type)
                                                 (membership->conf-no
                                                  (lp--entry->membership entry))))
                    '(lp--unread t)
                    )))
      (if defer-info
          (lyskom-replace-deferred defer-info string)
        (lyskom-insert-at-point string))

      (if (membership-type->passive
           (membership->type (lp--entry->membership entry)))
          (lp--entry-set-foreground entry (lyskom-face-foreground kom-dim-face))
        (lp--entry-set-foreground entry nil))

      (cond ((lp--entry->selected entry)
             (lp--entry-set-background entry
                                       (lyskom-face-background kom-mark-face)))

            (t (lp--entry-set-background entry nil)))

      (cond ((eq (membership->conf-no (lp--entry->membership entry))
                 (lyskom-default-value 'lyskom-current-conf))
             (lp--entry-set-foreground entry (lyskom-face-foreground
                                              kom-url-face)))

            (t (lp--entry-set-foreground entry nil)))
)))


(defun lp--format-entry-expansion (conf-stat defer-info)
  (let ((entry nil))
    (if (lyskom-lp--entry-p defer-info)
        (progn (setq entry defer-info)
               (setq defer-info nil))
      (setq entry (defer-info->data defer-info)))

  (let* ((membership (lp--entry->membership entry))
         (string 
          (if (and (null conf-stat)
                   (eq 0 (time->sec (membership->created-at membership)))
                   (eq 0 (time->min (membership->created-at membership)))
                   (eq 1 (time->mon (membership->created-at membership)))
                   (eq 1 (time->mday (membership->created-at membership)))
                   (eq 1970 (time->year (membership->created-at membership))))
              (lyskom-get-string 'lp-no-creation-info)
            (lyskom-format "%#1s %#2s av %#3P"
                           (lyskom-get-string
                            (if (membership-type->invitation (membership->type (lp--entry->membership entry)))
                                'lp-invited 'lp-added))
                           (lyskom-format-time
                            'date-and-time
                            (membership->created-at (lp--entry->membership entry)))
                           (if (null conf-stat)
                               (lyskom-format 'person-does-not-exist
                                              (membership->created-by (lp--entry->membership entry)))
                             conf-stat)))))
    (if defer-info
        (lyskom-replace-deferred defer-info string)
      (lyskom-insert-at-point string)))))


(defun lp--print-entry (entry)
  "Print the entry ENTRY at the current position in the buffer.
The start and end markers of the entry are adjusted"
  (if (lp--entry-set-visible entry (lp--entry-compute-visible entry))
      (let ((buffer-read-only nil))
        (insert-before-markers "\n")
	(if (lp--entry->end-marker entry)
	    (set-marker (lp--entry->end-marker entry) nil))
        (set-lp--entry->end-marker entry (point-marker))
        (forward-char -1)
	(if (lp--entry->start-marker entry)
	    (set-marker (lp--entry->start-marker entry) nil))
        (set-lp--entry->start-marker entry (point-marker))
        (lp--format-insert-entry entry)

	; End marker should be before \n.
	(set-marker (lp--entry->end-marker entry)
		    (1- (lp--entry->end-marker entry)))

        (lp--entry-update-extents entry)
        (forward-char 1))
    (lp--erase-entry entry)))

(defun lp--erase-entry (entry)
  "Erase the printed representation of the entry ENTRY in the buffer."
  (let ((buffer-read-only nil))
    (when (and (lp--entry->start-marker entry)
               (lp--entry->end-marker entry))
      (delete-region (lp--entry->start-marker entry)
                     (1+ (lp--entry->end-marker entry))))
    (if (lp--entry->start-marker entry)
	(set-marker (lp--entry->start-marker entry) nil))
    (set-lp--entry->start-marker entry nil)
    (if (lp--entry->end-marker entry)
	(set-marker (lp--entry->end-marker entry) nil))
    (set-lp--entry->end-marker entry nil)
    (lp--entry-update-extents entry)))

(defun lp--redraw-entry-mark (entry)
  "Redraw the mark for ENTRY."
  (let ((buffer-read-only nil))
    (lp--save-excursion
     (goto-char (lp--entry->start-marker entry))
     (insert (if (lp--entry->selected entry) ?* ?\ ))
     (if (lp--entry->selected entry)
         (lp--entry-set-background entry (lyskom-face-background kom-mark-face))
       (lp--entry-set-background entry nil))
     (delete-char 1)
     (lp--entry-update-extents entry))))

(defun lp--redraw-entry (entry)
  "Redraw the entry ENTRY."
  (lp--save-excursion
   (if (null (lp--entry->start-marker entry))
       (goto-char
        (let ((ne (lp--get-entry
                   (lp--next-visible-entry 
                    (membership->position (lp--entry->membership entry))))))
          (if ne
              (lp--entry->start-marker ne)
            lp--list-end-marker)))
     (goto-char (lp--entry->start-marker entry))
     (lp--erase-entry entry))

   (lp--print-entry entry)))

(defun lp--perform-in-all-buffers (fn &rest args)
  "Perform FN in all prioritization buffers. ARGS are arguments for FN.
Normally there should only be one buffer, but who knows..."
  (when lyskom-buffer
    (save-excursion
      (set-buffer lyskom-buffer)
      (let ((prioritize-buffers (lyskom-buffers-of-category 'prioritize)))
        (lyskom-traverse buffer prioritize-buffers
          (when (buffer-live-p buffer)
            (set-buffer buffer)
            (apply fn args)))))))


(defun lp--maybe-update-entry-for-conf (conf-no)
  (lp--perform-in-all-buffers
   (lambda (conf-no)
     (let ((entry (lp--conf-no-entry conf-no)))
       (when entry (lp--redraw-entry entry))))
   conf-no))

(defun lp--maybe-update-unreads (conf-no)
    (lp--perform-in-all-buffers
     (lambda (conf-no unread)
       (let ((entry (lp--conf-no-entry conf-no)))
         (when entry
           (let ((vis (lp--entry-compute-visible entry)))
             (cond ((not (eq vis (lp--entry->visible entry)))
                    (lp--redraw-entry entry))

                   ((lp--entry->visible entry)
                    (let ((bounds (lyskom-next-property-bounds 
                                   (lp--entry->start-marker entry) 
                                   (lp--entry->end-marker entry) 'lp--unread)))
                      (when bounds
                        (lp--save-excursion
                         (let ((buffer-read-only nil))
                           (delete-region (car bounds) (cdr bounds))
                           (goto-char (car bounds))
                           (insert (lyskom-format "%#2@%=5#1s"
                                                  (if (and unread (> unread 0))
                                                      (int-to-string unread) "")
                                                  '(lp--unread t)))))))))))))
     conf-no (lyskom-find-unread conf-no)))


;;; ============================================================
;; List management

(defun lp--set-entry-list (entries)
  "Set the list of entries to ENTRIES."
  (setq lp--entry-list entries))

(defun lp--all-entries ()
  "Return a list of all entries."
  lp--entry-list)


(defun lp--conf-no-entry (conf-no)
  "Find the entry for a membership in CONF-NO."
  (let ((entries (lp--all-entries))
        (found nil))
    (while (and entries (null found))
      (when (eq conf-no (membership->conf-no (lp--entry->membership
                                              (car entries))))
        (setq found (car entries)))
      (setq entries (cdr entries)))
    found))

(defun lp--find-new-position (entry priority &optional pos)
  "Find the new position for ENTRY it is were given priority PRIORITY.
If priority is lower than the entry priority this is the last position
currently occupied by an entry with a higher priority.  If priority is
higher, then it is the first position with a priority less than the
entry priority"
  (let ((entries (lp--all-entries))
        (result nil)
        (tmp nil))
    (cond

     ;; Moving down. Return the last entry spotted with a higher
     ;; than requested priority
     ((if priority 
          (> (lp--entry->priority entry) priority)
        (< (lp--entry-position entry) pos))
      (while (and entries (null result))
        (when (if priority
                  (<= (lp--entry->priority (car entries)) priority)
                (>= (lp--entry-position (car entries)) pos))
          (setq result tmp))
        (setq tmp (car entries))
        (setq entries (cdr entries)))
      (unless result (setq result tmp)))

     ;; Moving up. Return the first entry with a priority strictly
     ;; less than the requested one
     ((if priority
          (< (lp--entry->priority entry) priority)
        (> (lp--entry-position entry) pos))
      (while (and entries (null result))
        (when (if priority
                  (< (lp--entry->priority (car entries)) priority)
                (>= (lp--entry-position (car entries)) pos))
          (setq result (car entries)))
        (setq entries (cdr entries))))
     (t (setq result entry)))
    result))
  

(defun lp--get-last-visible-entry ()
  "Return the last visible entry in the list."
  (let* ((pos (1- (length (lp--all-entries))))
         (entry (lp--get-entry pos)))
    (while (and (> pos 0) entry (not (lp--entry->visible entry)))
      (setq pos (1- pos))
      (setq entry (lp--get-entry pos)))
    (if (and entry (lp--entry->visible entry)) entry nil)))

(defun lp--get-entry (pos)
  "Return the entry at position POS in the list."
  (elt lp--entry-list pos))

(defun lp--entry-position (entry)
  "Return the position in the list for ENTRY."
  (- (length (lp--all-entries))
     (length (memq entry (lp--all-entries)))))


;;(defun lp--entry-at (where)
;;  "Return the entry at WHERE."
;;  (let* ((pos (save-excursion (goto-char where)
;;                              (beginning-of-line)
;;                              (point)))
;;         (idx (lyskom-binsearch 
;;               pos (lp--all-entries) nil nil
;;               (lambda (a b)
;;                 (cond ((numberp a)
;;                        (and (lp--entry->start-marker b)
;;                             (lp--entry->end-marker b)
;;                             (< a (lp--entry->start-marker b))))
;;                       (t
;;                        (and (lp--entry->start-marker a)
;;                             (lp--entry->end-marker a)
;;                             (< (lp--entry->end-marker a) b))))))))
;;    (and idx (lp--get-entry idx))))

(defun lp--entry-at (where)
  "Return the entry at WHERE."
  (let ((pos (save-excursion (goto-char where)
                             (beginning-of-line)
                             (point))))
    (lyskom-traverse entry (lp--all-entries)
      (when (and (lp--entry->visible entry)
                 (>= pos (lp--entry->start-marker entry))
                 (<= pos (lp--entry->end-marker entry)))
        (lyskom-traverse-break entry)))))


(defun lp--move-entry (entry to)
  "Move the entry ENTRY to position TO in the list."
  (when (or (< to 0) (> to (length (lp--all-entries))))
    (error "Position out of range: %d" to))
  (lp--erase-entry entry)
  (lp--set-entry-list (lp--remove-from-list entry (lp--all-entries)))
  (let ((cur (lp--get-entry (lp--next-visible-entry to))))
    (if cur
        (goto-char (lp--entry->start-marker cur))
      (goto-char lp--list-end-marker))
    (lp--set-entry-list (lp--add-to-list to entry (lp--all-entries)))
    (lp--print-entry entry)))


(defun lp--remove-from-list (elem l)
  "Destructively remove ELEM from the list L."
  (let* ((n (- (length l) (length (memq elem l)))))
    (cond ((= 0 n) (cdr l))
          ((= n (length l)) l)
          (t (setcdr (nthcdr (1- n) l) (nthcdr (1+ n) l))
             l))))

(defun lp--add-to-list (elem data l)
  "At the position ELEM, add DATA to the list L using side effects."
  (if (> elem (length l))
      (error "Args out of range: %S, %d" l elem))
  (if (= 0 elem)
      (cons data l)
    (setcdr (nthcdr (1- elem) l)
            (cons data (nthcdr elem l)))
    l))

(defun lp--list-move-element (el to list)
  "Move element EL by side effects so it appears at position TO in LIST."
  (lp--add-to-list to el (lp--remove-from-list el list)))

(defun lp--update-buffer (conf-no)
  "Update the entry for CONF-NO in the buffer.
If optional NEW-MSHIP is non-nil, then get the membership again."
  (unless lp--inhibit-update
    (lp--perform-in-all-buffers
     (lambda ()
       (lp--save-excursion
        (let ((entry (lp--conf-no-entry conf-no))
              (mship (lyskom-get-membership conf-no t)))

          (cond 
           ((and (null entry) mship)    ; New membership
            (let* ((pos (membership->position mship))
                   (elem (and pos (lp--get-entry 
                                   (lp--next-visible-entry pos))))
                   (entry (lyskom-create-lp--entry 
                           nil
                           nil
                           (membership->priority mship)
                           mship
                           nil
                           (if (memq (membership->created-by mship)
                                     (list lyskom-pers-no 0))
                               'contracted
                             'expanded)
                           t
                           nil)))
              (when pos
                (save-excursion
                  (goto-char (if elem
                                 (lp--entry->start-marker elem)
                               lp--list-end-marker))
                  (lp--set-entry-list
                   (lp--add-to-list pos entry (lp--all-entries)))
                  (lp--print-entry entry)))))

           ;; We have unsubscribed for good

           ((null mship)
            (when entry
              (lp--set-entry-list
               (lp--remove-from-list entry
                                     (lp--all-entries)))
              (lp--erase-entry entry)))

           ;; The priority or position of a membership has changed

           ((or (/= (lp--entry->priority entry)
                    (membership->priority mship))
                (/= (lp--entry-position entry)
                    (membership->position mship)))
            (let ((new-pos (or (membership->position mship)
                               (lp--entry-position
                                (lp--find-new-position 
                                 entry (membership->priority mship))))))
              (lp--set-entry-pri-and-pos
               entry (membership->priority mship) new-pos)
              (set-lp--entry->membership entry mship)))

           (t (set-lp--entry->membership entry mship)
              (lp--redraw-entry entry)))))))))



(defun lp--map-region (start end function &rest args)
  "For each element from START to END, apply FUNCTION.
Apply FUNCTION to each element in the region from START to END, returning
a list of results.  ARGS will be passed as additional arguments to FUNCTION.

Args: START END FUNCTION ARGS."
  (let ((results nil)
        (entry nil))
    (save-excursion
      (goto-char (lp--entry->start-marker (lp--entry-at start)))
      (setq start (set-marker (make-marker) (point)))
      (goto-char (lp--entry->end-marker (lp--entry-at end)))
      (setq end (set-marker (make-marker) (point)))
      (goto-char start)

      (while (<= (point) end)
        (setq entry (lp--entry-at (point)))
        (setq results (cons (apply function entry args) results))
        (goto-char (lp--entry->end-marker entry))
        (forward-char 2)))
    results))



;;; ============================================================
;; Menu and button functions

;;; ------------------------------------------------------------
;; Flag menu
;; Argument is a list of entry and flag

(defun lp--flag-menu-get (entry flag)
  "For the membership in ENTRY, return the value of flag FLAG.
FLAG must be one of 'invitation, 'secret or 'passive."
  (funcall
   (cond ((eq flag 'invitation) 'membership-type->invitation)
         ((eq flag 'secret) 'membership-type->secret)
         ((eq flag 'passive) 'membership-type->passive)
         ((eq flag 'message-flag) 'membership-type->message-flag))
   (membership->type (lp--entry->membership entry))))

(defun lp--flag-menu-set (entry flag value)
  "For the membership in ENTRY, set FLAG to VALUE.
FLAG must be one of 'invitation, 'secret or 'passive."
  (funcall
   (cond ((eq flag 'invitation) 'set-membership-type->invitation)
         ((eq flag 'secret) 'set-membership-type->secret)
         ((eq flag 'passive) 'set-membership-type->passive)
         ((eq flag 'message-flag) 'set-membership-type->message-flag))
   (membership->type (lp--entry->membership entry))
   value))

(defun lyskom-prioritize-flag-clear (buf arg text)
  "Clear the membership flag the user clicked on.
This function should not be called directly.  It is called in response to
a mouse click.

BUF is the buffer in which the mouse click took place.  ARG is a list
of (ENTRY FLAG), where ENTRY is the entry the flag belongs to and FLAG
is one of invitation, secret or passive.  TEXT is the text that the user
clicked on."
  (interactive)
  (let ((entry (elt arg 0))
        (flag (elt arg 1)))
    (when (lp--flag-menu-get entry flag)
      (lyskom-prioritize-flag-toggle buf arg text))))

(defun lyskom-prioritize-flag-set (buf arg text)
  "Set the membership flag the user clicked on.
This function should not be called directly.  It is called in response to
a mouse click.

BUF is the buffer in which the mouse click took place.  ARG is a list
of (ENTRY FLAG), where ENTRY is the entry the flag belongs to and FLAG
is one of invitation, secret or passive.  TEXT is the text that the user
clicked on."
  (interactive)
  (let ((entry (elt arg 0))
        (flag (elt arg 1)))
    (unless (lp--flag-menu-get entry flag)
      (lyskom-prioritize-flag-toggle buf arg text))))

(defun lyskom-prioritize-flag-toggle (buf arg text)
  "Toggle the membership flag the user clicked on.
This function should not be called directly.  It is called in response to
a mouse click.

BUF is the buffer in which the mouse click took place.  ARG is a list
of (ENTRY FLAG), where ENTRY is the entry the flag belongs to and FLAG
is one of invitation, secret or passive.  TEXT is the text that the user
clicked on."
  (interactive)
  (let ((entry (elt arg 0))
        (flag (elt arg 1)))
    (save-excursion
      (set-buffer (marker-buffer (lp--entry->start-marker entry)))
      (lp--flag-menu-set entry flag (not (lp--flag-menu-get entry flag)))

      ;; Attempt to perform the change
      (save-excursion
        (set-buffer lyskom-buffer)
        (let ((result (blocking-do 'set-membership-type
                                   lyskom-pers-no
                                   (membership->conf-no (lp--entry->membership entry))
                                   (membership->type (lp--entry->membership entry)))))

          (unless result
            (lyskom-message 
             (lyskom-format 'lp-nope (lyskom-get-error-text lyskom-errno)))))

        ;; Update the display
        (let ((mship
               (blocking-do 'query-read-texts
                            lyskom-pers-no
                            (membership->conf-no (lp--entry->membership entry))
                            t 0)))
          (lyskom-replace-membership mship)
          (set-lp--entry->membership entry mship)
          (when (eq flag 'passive)
            (cond ((membership-type->passive (membership->type mship))
                   (save-excursion
                     (set-buffer lyskom-buffer)
                     (when (eq (membership->conf-no mship) lyskom-current-conf)
                       (lyskom-leave-current-conf))
                     (read-list-delete-read-info (membership->conf-no mship)
                                                 lyskom-to-do-list)
                     (lyskom-update-prompt t)))
                  (t (lyskom-prefetch-map (membership->conf-no mship) mship))))
          (blocking-do 'get-conf-stat (membership->conf-no mship))))
      (lp--redraw-entry entry))))

(defun lp--toggle-invitation ()
  "Toggle the invitation bit of the current entry"
  (interactive)
  (lp--save-excursion
   (let ((cur (lp--entry-at (point))))
     (cond ((null cur) (error (lyskom-get-string 'lp-no-entry)))
           (t (lyskom-prioritize-flag-toggle (current-buffer)
                                             (list cur 'invitation)
                                             ""))))))

(defun lp--toggle-passive ()
  "Toggle the passive bit of the current entry"
  (interactive)
  (lp--save-excursion
   (let ((cur (lp--entry-at (point))))
     (cond ((null cur) (error (lyskom-get-string 'lp-no-entry)))
           (t (lyskom-prioritize-flag-toggle (current-buffer)
                                             (list cur 'passive)
                                             ""))))))

(defun lp--toggle-message-flag ()
  "Toggle the passive bit of the current entry"
  (interactive)
  (lp--save-excursion
   (let ((cur (lp--entry-at (point))))
     (cond ((null cur) (error (lyskom-get-string 'lp-no-entry)))
           (t (lyskom-prioritize-flag-toggle (current-buffer)
                                             (list cur 'message-flag)
                                             ""))))))

(defun lp--toggle-secret ()
  "Toggle the secret bit of the current entry"
  (interactive)
  (lp--save-excursion
   (let ((cur (lp--entry-at (point))))
     (cond ((null cur) (error (lyskom-get-string 'lp-no-entry)))
           (t (lyskom-prioritize-flag-toggle (current-buffer)
                                             (list cur 'secret)
                                             ""))))))



;;; ============================================================
;; Marking and unmarking memberships
;; 

(defun lp--select-entries (entry-list state)
  "Set the selection value of all entries in ENTRY-LIST to STATE.
Forces a mode line update"
  (lp--do-select-entries entry-list state)
  (lp--update-mode-line))

(defun lp--do-select-entries (entry-list state)
  "Set the selection value of all entries in ENTRY-LIST to STATE."
  (mapcar (lambda (entry)
            (when entry
              (if (and state (lp--entry->visible entry))
                  (add-to-list 'lp--selected-entry-list entry)
                (setq lp--selected-entry-list
                      (delq entry lp--selected-entry-list)))
              (set-lp--entry->selected entry state)
              (lp--redraw-entry-mark entry))) entry-list))

(defun lp--all-selected-entries ()
  "Return a list of all selected entries."
  lp--selected-entry-list)

(defun lp--set-selected-entries (entry-list)
  "Set the selected entries to exactly the entries in ENTRY-LIST.
Forces a mode line update"
  (lp--do-select-entries (lp--all-selected-entries) nil)
  (lp--do-select-entries entry-list t)
  (lp--update-mode-line))

;;; ============================================================
;;; Hiding and unhiding entries
;;; FIXME: This doesn't work yet

(defun lp--entry-compute-visible (entry)
  "Compute the visibility of ENTRY."
  (not (lyskom-traverse filter lp--entry-filter
         (unless (apply (car filter) entry (cdr filter))
           (lyskom-traverse-break t)))))

(defun lp--entry-set-visible (entry state)
  "Set the visibility of ENTRY to STATE"
  (let ((old-state (lp--entry->visible entry)))
    (unless (eq state old-state)
      (set-lp--entry->visible entry state)
      (if state
          (setq lp--hidden-entries (delq entry lp--hidden-entries))
        (setq lp--hidden-entries (cons entry lp--hidden-entries)))
      (lp--update-mode-line)))
  state)

(defun lp--show-all ()
  (interactive)
  (setq lp--entry-filter nil)
  (lyskom-traverse entry lp--hidden-entries
    (lp--redraw-entry entry))
  (setq lp--hidden-entries nil))




;;; ------------------------------------------------------------
;; Server update functions

(defun lp--update-membership (entry old-pri old-pos)
  "Update the server and local versions of membership in ENTRY."
  (save-excursion
    (let ((saved-pos (lp--entry-position entry)))
      (set-buffer lyskom-buffer)
      (let ((mship (lp--entry->membership entry)))
        (unless (eq old-pos saved-pos)
          (lyskom-change-membership-position (membership->conf-no mship)
                                             saved-pos))
        (unless (eq old-pri (lp--entry->priority entry))
          (lyskom-change-membership-priority (membership->conf-no mship)
                                             (lp--entry->priority entry)))
        (cond ((and (>= old-pri lyskom-session-priority)
                    (< (membership->priority mship) lyskom-session-priority))
               (when (eq lyskom-current-conf (membership->conf-no mship))
                 (lyskom-leave-current-conf))
               (read-list-delete-read-info (membership->conf-no mship)
                                           lyskom-to-do-list)
               (lyskom-update-prompt t))
              ((and (< old-pri lyskom-session-priority)
                    (>= (membership->priority mship) lyskom-session-priority))
               (lyskom-prefetch-map (membership->conf-no mship) mship)))

        (initiate-add-member 'background nil
                             (membership->conf-no mship)
                             lyskom-pers-no
                             (membership->priority mship)
                             (membership->position mship)
                             (membership->type mship))))))


;;; ============================================================
;;; Support

(defun lp--next-visible-entry (pos)
  "Return the position of the first visible entry following POS."
  (let ((max (length (lp--all-entries)))
        (result nil))
  (while (and (< pos max) (null result))
    (if (lp--entry->visible (lp--get-entry pos))
        (setq result pos
              pos max)
      (setq pos (1+ pos))))
  (or result max)))
  

(defun lp--calculate-distance (pos delta)
  "Return one more than the number of entries between POS and POS+DELTA.
This includes visible and invisible entries. The sign of the
returned value has the same sign as DELTA. The result is clipped to the
size of the list."
  (let ((step (lyskom-signum delta))
        (num (abs delta))
        (result 0))
    (while (> num 0)
      (setq pos (+ step pos))
      (if (< pos 0)
          (setq num 0)
        (setq result (1+ result))
        (let ((entry (lp--get-entry pos)))
          (cond ((null entry) (setq result (1+ result) num 0))
                ((lp--entry->visible (lp--get-entry pos)) (setq num (1- num)))))))
    (* result step)))


;;; ----------------------------------------------------------------
;;; Filtration

(defun lp--entry-filter-active (filter)
  "Return non-nil if FILTER is an active entry filter."
  (assq filter lp--entry-filter))

(defun lp--add-entry-filter (filter &rest filter-args)
  "Add entry filter FILTER.
FILTER is a function that should take one argument, an lp--entry,
and return non-nil if the entry should be visible.

If optional arguments FILTER-ARGS are supplied, these are also
passed to the filter function."
  (unless (symbolp filter) (error "entry filter must be a symbol"))
  (unless (assq filter lp--entry-filter)
    (setq lp--entry-filter (cons (cons filter filter-args) lp--entry-filter))
    (lp--apply-entry-filter)))

(defun lp--replace-entry-filter (filter &rest filter-args)
  "Replace existing entry filter FILTER.

See `lp--add-entry-filter' for more information."
  (unless (symbolp filter) (error "entry filter must be a symbol"))
  (if (assq filter lp--entry-filter)
      (setcdr (assq filter lp--entry-filter) filter-args)
    (setq lp--entry-filter (cons (cons filter filter-args) lp--entry-filter)))
  (lp--apply-entry-filter))


(defun lp--del-entry-filter (filter)
  "Remove the entry filter FILTER."
  (let ((el (assq filter lp--entry-filter)))
    (when el
      (setq lp--entry-filter (delq el lp--entry-filter))
      (lp--apply-entry-filter))))

(defun lp--apply-entry-filter ()
  "Apply the current filter list to all entries."
  (lp--save-excursion
   (lp--update-filter-description)
   (lyskom-traverse entry (lp--all-entries)
     (let ((vis (lp--entry-compute-visible entry)))
       (unless (eq vis (lp--entry->visible entry))
         (lp--redraw-entry entry))))))

(defun lp--entry-filter-description ()
  "Return a string representing the current entry filters."
  (let (res)
    (lyskom-traverse filter lp--entry-filter
      (let ((name (car filter)))
        (when name (setq res (cons name res)))))
    (or (and res (mapconcat 'symbol-name (nreverse res) ", "))
        (lyskom-get-string 'lp-no-active-filter))))

(defun lp--update-filter-description ()
  "Update the filter description shown in the buffer."
  (lp--set-header 'filter (lyskom-format 'lp-active-filters
                                         (lp--entry-filter-description))))


;; Filter functions should return the desired visibility state

(defun lp--entry-filter-read (entry)
  "Entry filter that displays only conferences with unread texts."
  (let ((n (lyskom-find-unread 
            (membership->conf-no (lp--entry->membership entry)))))
    (or (eq (membership->conf-no (lp--entry->membership entry))
            (lyskom-default-value 'lyskom-current-conf))
        (and n (> n 0)))))

(defun lp--entry-filter-passive (entry)
  "Entry filter that displays only active memberships."
  (not (membership-type->passive 
        (membership->type (lp--entry->membership entry)))))

(defun lp--entry-filter-after (entry time)
  "Hide entries read after a certain time"
  (not (lyskom-time-greater (membership->last-time-read
                             (lp--entry->membership entry))
                            time)))

(defun lp--entry-filter-before (entry time)
  "Hide entries read before a certain time"
  (lyskom-time-greater (membership->last-time-read
                        (lp--entry->membership entry))
                       time))


;;; ================================================================
;;; User-level functions


(defun lp--select-membership ()
  "Select the membership that point is on."
  (interactive)
  (let ((entry (lp--entry-at (point))))
    (when entry
      (lp--select-entries (list entry) t))))

(defun lp--deselect-membership ()
  "Deselect the membership that point is on."
  (interactive)
  (let ((entry (lp--entry-at (point))))
    (when entry
      (lp--select-entries (list entry) nil))))

(defun lp--toggle-membership-selection (where)
  "Toggle selection of the membership at WHERE."
  (interactive "d")
  (let ((entry (lp--entry-at where)))
    (when entry
      (lp--select-entries (list entry) (not (lp--entry->selected entry))))))

(defun lp--select-region (start end)
  "Select all entries in the region. With prefix arg, deselect.
START and END are the starting and ending points of the region."
  (interactive "r")
  (let ((entry-list (lp--map-region start end 'identity)))
    (lp--select-entries entry-list (not current-prefix-arg))))

(defun lp--select-priority (priority)
  "Select all entries with a priority PRIORITY.
With numeric prefix argument select entries with that priority."
  (interactive "P")
  (lp--do-select-priority priority t))

(defun lp--deselect-priority (priority)
  "Deselect all entries with a priority PRIORITY.
With numeric prefix argument deselect entries with that priority."
  (interactive "P")
  (lp--do-select-priority priority nil))

(defun lp--do-select-priority (priority select)
  "Select or deselect all entries with priority PRIORITY.
SELECT specifies new select."
  (when (not (numberp priority))
    (let ((entry (lp--entry-at (point))))
      (setq priority
            (lyskom-read-num-range 0 255 
                                   (lyskom-get-string
                                    (if select
                                        'lp-mark-mship-with-prio
                                      'lp-unmark-mship-with-prio))
                                   nil
                                   (and entry
                                        (membership->priority
                                         (lp--entry->membership entry)))))))

  (lp--select-entries
   (mapcar (lambda (entry)
             (and (eq priority (membership->priority
                                (lp--entry->membership entry)))
                  entry))
           (lp--all-entries))
   select))

(defun lp--deselect-all ()
  "Deselect all memberships."
  (interactive)
  (lp--set-selected-entries nil))



;;; ============================================================
;;; Reprioritization functions

(defun lp--set-entry-pri-and-pos (entry priority position)
  "Set the priority of ENTRY to PRIORITY and the position to POSITION.
If PRIORITY or POSITION is nil, the parameter is ignored.
If the position changes, lp--move-entry is called.
lp--update-membership is called automatically before this function exits."
  (let ((old-pri (lp--entry->priority entry))
        (old-pos (lp--entry-position entry))
        (lp--inhibit-update t)
        (need-redraw nil))
    (when (and priority (not (eq priority old-pri)))
      (set-lp--entry->priority entry priority)
      (set-membership->priority (lp--entry->membership entry) priority)
      (setq need-redraw t))
    (when (and position (not (eq position old-pos)))
      (lp--move-entry entry position)
      (set-membership->position (lp--entry->membership entry) position)
      (setq need-redraw nil))
    (lyskom-replace-membership (lp--entry->membership entry))
    (sit-for 0)
    (lp--update-membership entry old-pri old-pos)
    (when need-redraw (lp--redraw-entry entry))))

(defun lp--yank ()
  "Insert all the selected memberships before the entry at point."
  (interactive)
  (lp--save-excursion
   (let* ((cur (lp--entry-at (point)))
          (pos (and cur (lp--entry-position cur)))
          (priority (and cur (lp--entry->priority cur)))
          (entries (lp--all-selected-entries)))
     (cond ((null cur) (error (lyskom-get-string 'lp-no-entry)))
           ((null entries) (error (lyskom-get-string 'lp-no-selection)))
           (t (mapcar 
               (lambda (entry)
                 (lp--set-entry-pri-and-pos
                  entry priority
                  (lp--entry-position (lp--find-new-position entry nil pos))))
               entries))))))
          


(defun lp--set-priority (priority)
  "Set the priority of selected memberships to PRIORITY.
Memberships that must be moved will be moved the shortest distance
possible in the list."
  (interactive "P")
  (let* ((entries (or (lp--all-selected-entries)
                      (list (lp--entry-at (point))))))
    (unless entries
      (error (lyskom-get-string 'lp-no-selection)))
    (unless (numberp priority)
      (cond ((> (length entries) 1)
             (setq priority
                   (lyskom-read-num-range
                    0 255 (lyskom-get-string 'priority-prompt-marked) t)))
            (t
             (setq priority
                   (lyskom-read-num-range
                    0 255 (lyskom-format 'priority-prompt
                                         (membership->conf-no
                                          (lp--entry->membership
                                           (car entries)))) t)))))
    (lp--save-excursion
     (mapcar (lambda (entry)
               (let ((new-pos (lp--entry-position
                               (lp--find-new-position entry priority))))
                 (lp--set-entry-pri-and-pos entry priority new-pos)))
             entries))))

(defun lp--bump-priority (amount)
  "Increase the priority of the current entry by one"
  (lp--save-excursion
   (let* ((cur (lp--entry-at (point)))
          (pri (and cur (lp--entry->priority cur)))
          (pos (lp--entry-position cur))
          (new-pri (+ pri amount)))
     (when (> new-pri 255) (setq new-pri 255))
     (when (< new-pri 0) (setq new-pri 0))

     (when (and cur
                (eq pri new-pri)
                (eq pri 0)
                (eq pos (1- (length (lp--all-entries)))))
       (error (lyskom-get-string 'lp-at-min-prio)))

     (when (and cur
                (eq pri new-pri)
                (eq pri 255)
                (eq pos 0))
       (error (lyskom-get-string 'lp-at-max-prio)))

     (cond ((null cur) (error (lyskom-get-string 'lp-no-entry)))
           (t (let ((new-pos (lp--entry-position 
                              (lp--find-new-position cur (+ pri amount)))))
                (lp--set-entry-pri-and-pos cur new-pri new-pos)))))))


(defun lp--increase-priority (arg)
  (interactive "p")
  (lp--bump-priority arg))

(defun lp--decrease-priority (arg)
  (interactive "p")
  (lp--bump-priority (- arg)))



(defun lp--move-up ()
  "Move the current entry up one notch."
  (interactive)
  (lp--save-excursion
   (let* ((cur (lp--entry-at (point)))
          (pos (and cur (lp--entry-position cur)))
          (place (and cur (> pos 0) (+ pos (lp--calculate-distance pos -1))))
          (prev (and place (lp--get-entry place))))
     (cond ((null cur) (error (lyskom-get-string 'lp-no-entry)))
           ((null prev) (error (lyskom-get-string 'lp-beginning-of-list)))
           (t (if (/= (lp--entry->priority cur)
                      (lp--entry->priority prev))
                  (progn
                    (lp--set-entry-pri-and-pos cur
                                               (lp--entry->priority prev)
                                               (if (= pos (1+ place))
                                                   nil
                                                 (1+ place)))
                    (lp--redraw-entry cur))
                (lp--set-entry-pri-and-pos cur nil place)))))))

(defun lp--move-down ()
  "Move the current entry up down notch."
  (interactive)
  (lp--save-excursion
   (let* ((cur (lp--entry-at (point)))
          (pos (and cur (lp--entry-position cur)))
          (place (and cur (+ pos (lp--calculate-distance pos 1))))
          (prev (and place (lp--get-entry place))))
     (cond ((null cur) (error (lyskom-get-string 'lp-no-entry)))
           ((null prev) (error (lyskom-get-string 'lp-end-of-list)))
           (t (if (/= (lp--entry->priority cur)
                      (lp--entry->priority prev))
                  (progn
                    (lp--set-entry-pri-and-pos cur
                                               (lp--entry->priority prev)
                                               (if (= pos (1- place))
                                                   nil
                                                 (1- place)))
                    (lp--redraw-entry cur))
                (lp--set-entry-pri-and-pos cur nil place)))))))



;;; ============================================================
;;; Motion commands

(defmacro lp--save-column (&rest body)
  `(let ((lp--saved-column (current-column)))
     ,@body
     (end-of-line)
     (if (> (current-column) lp--saved-column)
         (progn (beginning-of-line)
                (forward-char lp--saved-column)))))

;;; FIXME: Deal with invisible entries

(defun lp--previous-entry (count)
  "Move the cursor up COUNT lines.
The cursor will always move to the start of the target entry."
  (interactive "p")
  (let* ((entry (lp--entry-at (point)))
         (last-entry (lp--get-last-visible-entry))
         (first-entry (lp--next-visible-entry 0))
         (pos (lp--entry-position entry))
         (new-pos (cond ((null last-entry) nil)
                        ((and (null entry) 
                              (< (point)
                                 (lp--entry->start-marker 
                                  (lp--get-entry first-entry))))
                         (lp--get-entry first-entry))
                        ((and (null entry)
                              (> (point) 
                                 (lp--entry->end-marker last-entry)))
                         last-entry)
                        (t (lp--get-entry
                            (max 0 (+ pos (lp--calculate-distance
                                           pos (- count)))))))))
    (condition-case nil
        (goto-char (lp--entry->start-marker new-pos))
      (error nil))))

(defun lp--next-entry (count)
  "Move the cursor down COUNT lines.
The cursor will always move to the start of the target entry."
  (interactive "p")
  (let* ((entry (lp--entry-at (point)))
         (last-entry (lp--get-last-visible-entry))
         (first-entry (lp--next-visible-entry 0))
         (pos (lp--entry-position entry))
         (new-pos (cond ((null last-entry) nil)
                    ((and (null entry) (< (point) 
                                          (lp--entry->start-marker 
                                           (lp--get-entry
                                            first-entry))))
                     (lp--get-entry first-entry))
                    ((and (null entry) (> (point) 
                                          (lp--entry->end-marker 
                                           last-entry)))
                     last-entry)
                    (t (lp--get-entry 
                        (min (1- (length (lp--all-entries)))
                             (+ pos (lp--calculate-distance pos count))))))))
    (condition-case nil
        (goto-char (lp--entry->start-marker new-pos))
      (error nil))))

(defun lp--first-entry ()
  "Move point to the first entry in the membership list."
  (interactive)
  (condition-case nil
      (let ((entry (lp--get-entry (lp--next-visible-entry 0))))
        (cond ((null entry) (goto-char lp--list-end-marker))
              (t (goto-char (lp--entry->start-marker 
                             (lp--get-entry (lp--next-visible-entry 0)))))))
    (error nil)))

(defun lp--last-entry ()
  "Move point to the last entry in the membership list."
  (interactive)
  (condition-case nil
      (let ((entry (lp--get-last-visible-entry)))
        (cond ((null entry) (goto-char lp--list-end-marker))
              (t (goto-char (lp--entry->start-marker entry)))))
    (error nil)))

(defun lp--goto-priority (priority)
  "Move to the closest entry with priority PRIORITY.
If there is no entry with the specified priority, move to the nearest
entry with an adjacent priority."
  (interactive "P")
  (let* ((entry (lp--entry-at (point)))
         (seen-me nil)
         (done nil)
         (entry-list (lp--all-entries)))

    ;; Get the priority to move to

    (unless (numberp priority)
      (setq priority
            (lyskom-read-num-range 
             0 255 (lyskom-get-string 'lp-goto-priority) t)))

    ;; Figure out where to move
    ;; Loop over all entries

    (while (and (not done) (cdr entry-list))
      (when (eq (car entry-list) entry) (setq seen-me t))
      (if (or (< (lp--entry->priority (car entry-list))
                 priority)
              (and seen-me (= (lp--entry->priority (car entry-list))
                              priority)))
          (setq done t)
        (setq entry-list (cdr entry-list))))

    (when entry-list
      (goto-char (lp--entry->start-marker (car entry-list))))))
    
(defun lp--toggle-entry-expansion ()
  "Toggle the expanded state of the current entry."
  (interactive)
  (let ((entry (lp--entry-at (point))))
    (when entry
      (set-lp--entry->state
       entry
       (if (eq (lp--entry->state entry) 'expanded) 'contracted 'expanded))
      (lp--redraw-entry entry))))

(defun lp--expand-entry (arg)
  "Expand selected entries. 
With prefix arg, expand only those that were created by someone else."
  (interactive "P")
  (let ((entries (or (lp--all-selected-entries)
                      (list (lp--entry-at (point)))))
        (hidden-list (list lyskom-pers-no 0)))
    (mapcar (lambda (entry)
              (when (and (or (null arg)
                             (not (memq (membership->created-by
                                         (lp--entry->membership entry))
                                        hidden-list)))
                         (not (eq (lp--entry->state entry) 'expanded)))
                (set-lp--entry->state entry 'expanded)
                (lp--redraw-entry entry)))
            entries)))

(defun lp--contract-entry (arg)
  "Contract selected entries.
With prefix arg, contract only those that were created by self."
  (interactive "P")
  (let ((entries (or (lp--all-selected-entries)
                      (list (lp--entry-at (point)))))
        (hidden-list (list lyskom-pers-no 0)))
    (mapcar (lambda (entry)
              (when (and (or (null arg)
                             (memq (membership->created-by
                                    (lp--entry->membership entry))
                                   hidden-list))
                         (not (eq (lp--entry->state entry) 'contracted)))
                (set-lp--entry->state entry 'contracted)
                (lp--redraw-entry entry)))
            entries)))

(defun lp--quit ()
  "Remove the membership buffer and quit"
  (interactive)
  (lyskom-undisplay-buffer))



(defun lp--scroll-advice (fn)
  (let ((cur (current-column)))
    (funcall fn)
    (when (and (boundp 'lyskom-buffer-category)
               (eq lyskom-buffer-category 'prioritize))
      (let ((last-entry (lp--get-last-visible-entry ))
            (first-entry (lp--next-visible-entry 0)))
        (cond ((null last-entry)
               (goto-char lp--list-end-marker))
              ((> (point) (lp--entry->end-marker last-entry))
               (goto-char (lp--entry->end-marker last-entry))
               (when (> (current-column) cur)
                 (beginning-of-line)
                 (forward-char cur)))

              ((< (point) (lp--entry->start-marker
                           (lp--get-entry first-entry)))
               (goto-char (lp--entry->start-marker 
                           (lp--get-entry first-entry)))
               (end-of-line)
               (when (> (current-column) cur)
                 (beginning-of-line)
                 (forward-char cur))))))))

(defadvice scroll-up-command (around lp--scroll-up-advice activate)
  (lp--scroll-advice (lambda () ad-do-it)))

(defadvice scroll-down-command (around lp--scroll-up-advice activate)
  (lp--scroll-advice (lambda () ad-do-it)))


;;; ============================================================
;; The mode

(defvar lp--mode-line '("--" 
                        mode-line-buffer-identification
                        "   "
                        global-mode-string
                        "   %[("
                        mode-name
                        mode-line-process
                        minor-mode-alist
                        ")%] "
                        lp--mode-line-selected
                        "--"
                        lp--mode-line-hidden
                        "--"
                        (-3 . "%p")
                        "-%-"))

(defvar lp--mode-line-selected ""
  "String showing number of selected entries.")

(defvar lp--mode-line-hidden ""
  "String showing number of hidden entries.")

(defun lp--update-mode-line ()
  (setq lp--mode-line-selected
        (cond ((= (length (lp--all-selected-entries)) 0)
               (lyskom-get-string 'no-selection))
               (t (lyskom-format 'selection
                                 (length (lp--all-selected-entries))))))
  (setq lp--mode-line-hidden
        (cond ((= (length lp--hidden-entries) 0)
               (lyskom-get-string 'lp-no-hidden))
               (t (lyskom-format 'lp-hidden
                                 (length lp--hidden-entries)))))
  (force-mode-line-update))


(def-kom-command kom-prioritize ()
  "Display a list of all memberships with the option to change order,
priority, flags, and a number of other things.

See `kom-priotitize-in-window'."
  (interactive)
  (lyskom-prioritize))

(defun lyskom-prioritize ()
  (let ((kom-deferred-printing nil))
    (set-buffer (lp--create-buffer)))
  (lp--mode)
  (lp--first-entry))

(defun lp--mode ()
  "\\<lyskom-prioritize-mode-map>Mode for prioritizing conferences in LysKOM.

\[lp--move-up] moves a single conference up in the list.
\[lp--mode-down] moves a single conference down in the list.

SPC toggles the selection of a single membership.
\[lp--select-region] selects all conferences between point and mark.
\[lp--select-priority] selects all conferences with a certain priority.
\[lp--deselect-region] clears the selection.

\[lp--yank] moves all selected conferences to where point is in the list.
\[lp--set-priority] changes the priority of all selected conferences.

\[lp--toggle-passive] makes passive memberships active and vice versa.
\[lp--toggle-invitation] toggles the invitation flag of a membership.
\[lp--toggle-secret] makes secret memberships non-secret and vice versa.
\[lp--toggle-message-flag] toggles reception of group messages for that conference.

Changes take effect immediately.

This mode is still under development and is subject to problems. In
particular, it is unlikely to handle all error conditions gracefully,
which may lead to inconsistencies between the buffer and reality.
Should a problem occur, killing the buffer and reopening it usually
clears the problem.

All bindings:
\\{lyskom-prioritize-mode-map}

Entry to this mode runs lp--mode-hook, but since the name of this hook
is subject to change, be careful when using it."
  (interactive)
  (setq major-mode 'lp--mode)
  (setq mode-name (lyskom-get-string 'lp-mode-name))
  (make-local-variable 'lp--format-string)
  (make-local-variable 'lp--conf-name-width)
  (make-local-variable 'lp--entry-list)
  (make-local-variable 'lp--list-start-marker)
  (make-local-variable 'lp--list-end-marker)
  (make-local-variable 'lp--selected-entry-list)
  (make-local-variable 'lp--mode-line-selected)
  (make-local-variable 'lp--hidden-entries)
  (make-local-variable 'lp--entry-filter)

  (setq lp--hidden-entries nil)
  (setq lp--entry-filter nil)

  (lyskom-set-menus 'lp--mode lyskom-prioritize-mode-map)

  (setq lp--mode-line-selected "")
  (setq mode-line-format lp--mode-line)
  (lp--update-mode-line)
  (setq buffer-read-only t)
  (lyskom-use-local-map lyskom-prioritize-mode-map)
  (run-hooks 'lp--mode-hook))


(defun lp--set-header (id string)
  "Set header ID of the current membership buffer to STRING."
  (save-excursion
    (let ((inhibit-read-only t)
          (pos (assq id lp--headers)))
      (setq lp--headers (delq pos lp--headers))

      (if (not pos)
          (goto-char lp--header-end-marker)
        (goto-char (elt pos 1))
        (delete-region (elt pos 1) (elt pos 2)))

      (if (null string)
          (delete-char 1)
        (let ((start (point-marker)) (end nil))
          (set-marker-insertion-type start nil)
          (insert string)
          (setq end (point-marker))
          (unless pos (insert "\n"))
          (setq lp--headers (cons (list id start end) lp--headers)))))))



(defun lp--create-buffer ()
  "Create a buffer for managing memberships."
  (let ((buf (lyskom-get-buffer-create 'prioritize
                                       (concat (buffer-name) "-membership")
                                       t))
        (entry-list nil))

    (lyskom-save-excursion
      (set-buffer buf)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (make-local-variable 'lp--entry-list)
        (make-local-variable 'lp--list-start-marker)
        (make-local-variable 'lp--list-end-marker)
        (make-local-variable 'lp--buffer-done)
        (make-local-variable 'lp--end-marker)
        (make-local-variable 'lp--header-end-marker)
        (make-local-variable 'lp--headers)
        (setq lp--entry-list nil)
        (setq lp--buffer-done nil)
        (lp--compute-format-string)

        (setq lp--header-end-marker (point-marker))
        (set-marker-insertion-type lp--header-end-marker nil)
        (lyskom-insert (make-string (1- (window-width)) ?=))
        (lyskom-insert "\n")
        (set-marker-insertion-type lp--header-end-marker t)

        (lp--set-header 'main
                        (lyskom-format 'lp-header-main
                                       lyskom-pers-no lyskom-server-name))

        (lyskom-format-insert 'lp-list-header
                              (concat (lyskom-get-string 'conference)
                                      (make-string (- lp--conf-name-width
                                                      (length (lyskom-get-string 'conference)))
                                                   ?\ )))

        (lyskom-insert (make-string (1- (window-width)) ?-))
        (lyskom-insert "\n")

        (setq lp--list-start-marker (point-marker))
        (goto-char (point-max))
        (lyskom-display-buffer buf)
        (lyskom-traverse-membership mship
          (let ((entry (lyskom-create-lp--entry nil ; Start
                                                nil ; End
                                                (membership->priority mship)
                                                mship
                                                nil
                                                (if (memq (membership->created-by mship)
                                                          (list lyskom-pers-no 0))
                                                    'contracted
                                                  'expanded)
                                                t
                                                nil)))
            (lp--print-entry entry)
            (setq entry-list (cons entry entry-list))))
        (lp--set-entry-list (nreverse entry-list))
        (setq lp--list-end-marker (point-marker))
        (lyskom-insert (make-string (1- (window-width)) ?=))
        (lyskom-insert (lyskom-get-string 'lp-help-footer))
        ))
    (lyskom-wait-queue 'deferred)
    buf
    ))




;;; ================================================================
;;; User functions for filtering


(defun lp--show-hide-memberships-read-before (arg)
  "Hide memberships that have not been read since a specific date.
With prefix arg, removes any such filter that has been applied."
  (interactive "P")
  (if arg
      (lp--del-entry-filter 'lp--entry-filter-before)

    (let* ((n (lyskom-read-date (lyskom-get-string 'lp-hide-read-sice)))
           (date (and n (lyskom-create-time 0 0 0 (elt n 2)
                                            (elt n 1) (elt n 0)
                                            0 0 nil))))
      (lp--replace-entry-filter 'lp--entry-filter-before date))))


(defun lp--show-hide-memberships-read-after (arg)
  "Hide memberships that have been read since a specific date.
With prefix arg, removes any such filter that has been applied."
  (interactive "P")
  (if arg
      (lp--del-entry-filter 'lp--entry-filter-after)

    (let* ((n (lyskom-read-date (lyskom-get-string 'lp-hide-read-after)))
           (date (and n (lyskom-create-time 0 0 0 (elt n 2)
                                            (elt n 1) (elt n 0)
                                            0 0 nil))))
      (lp--replace-entry-filter 'lp--entry-filter-after date))))


(defun lp--show-hide-read-memberships (arg)
  "Hide memberships that contain only read texts.
With prefix argument, remove any such filters."
  (interactive "P")
  (if arg
      (lp--del-entry-filter 'lp--entry-filter-read)
  (unless (lp--entry-filter-active 'lp--entry-filter-read)
    (lp--add-entry-filter 'lp--entry-filter-read))))

(defun lp--show-hide-passive-memberships (arg)
  "Hide passive memberships.
With prefix argument, remove any such filters."
  (interactive "P")
  (if arg
      (lp--del-entry-filter 'lp--entry-filter-passive)
    (unless (lp--entry-filter-active 'lp--entry-filter-passive)
      (lp--add-entry-filter 'lp--entry-filter-passive))))





(provide 'mship-edit)

;;; mship-edit.el ends here
;;; Local Variables:
;;; eval: (put 'lp--save-excursion 'edebug-form-spec t)
;;; end:


