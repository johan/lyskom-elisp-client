;;; mship-edit.el --- Summary
;; TO DO
;; see tmp.el
;;
;; When showing hidden entries they are not put in the right position
;; in the buffer. lp--set-entry-visible needs to goto-char to the right
;; place in the buffer.
;;
;; Maybe move setting the membership priority to 
;; lyskom-change-membership-priority. 
;;
;; -------------------------------------------------------------------------
;; When prioritizing an entry we need to sort the read lists to put
;; the entries in the proper order. It's possible that we'll have to
;; change the prompt.
;;
;; Do this under lyskom-update-membership
;;
;; Test cases:
;;
;; Change the priority of the current conf to lower than one we have
;; unreads in. Should trigger prompt change.
;;
;; Change the priority of the current conf to higher than one that
;; was higher previously and had unreads. This should trigger a
;; prompt change.
;;
;; Re-order two confs we are not in by changing their priority. Check
;; that we get to read them in the correct order.
;;
;; Re-order two confs without changing their priority. Check that we
;; get to read them in the correct order.
;; -------------------------------------------------------------------------
;;
;; -------------------------------------------------------------------------
;; Changing priority might put the conference above or below the
;; current session priority. We need to fetch or delete maps.
;;
;; Do this under lyskom-update-membership. Done.
;;
;; Test cases:
;;
;; Prioritize a conf under the session priority to above the session
;; priority. Should give us more unreads. Might trigger prompt change.
;;
;; Prioritize a conf with unreads to under the session priority.
;; Should give us less unreads. Might trigger prompt change.
;;
;; While prefetching a conf, change its priority to below the session
;; priority. The prefetched maps should be discarded automatically.
;; -------------------------------------------------------------------------
;;          
;; What we need is a general function we use to change the priority of a
;; membership. Since we do all the updates in the background it's OK to
;; send an update off as soon as we change the priority of a membership.
;; 
;; This function should...
;; - Change the priority and placement of the membership in the server
;; - See if the priority has changed re the session priority and if so
;;   either start a prefetch for the conference or remove the unreads
;;   for the conference from all read lists.
;; - Remove and reinsert the unreads in the reading lists, then update
;;   the prompt.
;; - Sort the membership list.
;;
;; Done in lp--set-entry-pri-and-pos and lp--update-membership.
;;
;; TODO UNSORTED
;; Keep the unread counter updated. Hook in lyskom-mark-as-read
;; Keep the membership list update. Perhaps in lyskom-add-membership
;;      and associated functions to add, lyskom-remove-membership to
;;      remove.
;; Endast-function to all marked memberships
;; Uppskjut-function to all marked memberships
;; Remove and add membership functions.
;; 


;;; Commentary:
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

(defvar lp--last-format-string nil
  "The cached format string for entries. 
Use lp--compute-format-string when you need the format string. Do not
access this variable directly.")
(defvar lp--last-window-width -1)

;;; Local variables in the prioritize buffer
;;; There should be no reason to use these at all. There are functional
;;; abstractions that let you access their contents.

(defvar lp--entry-list nil)
(defvar lp--list-start-marker nil)
(defvar lp--list-end-marker nil)
(defvar lp--selected-entry-list nil)
(defvar lp--buffer-done nil)


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
      (lyskom-sort-membership)
      (cond
       ((and (>= old-priority lyskom-session-priority)
             (>= new-priority lyskom-session-priority))
        ; Don't (lyskom-sort-to-do-list) since lyskom-sort-membership will.
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

  ;; FIXME: We have to update all positions in the membership 
  ;; FIXME: list, not just the one we changed.

  (let ((mship (lyskom-get-membership conf-no t)))
    (when mship
      (set-membership->position mship new-position)
      (setq lyskom-membership (lyskom-move-in-list mship lyskom-membership new-position))
      (lyskom-update-membership-positions))))


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
             (facename (intern (format "lyskom-%s-background" color)))
             (face (or (find-face facename) (lyskom-make-face facename t))))
        (unless extent
          (lyskom-xemacs-or-gnu
           (setq extent (make-extent (or (lp--entry->start-marker entry) 0)
                                     (or (lp--entry->end-marker entry) 0)))
           (setq extent (make-overlay (or (lp--entry->start-marker entry) 0)
                                      (or (lp--entry->end-marker entry) 0)
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
             (facename (intern (format "lyskom-%s-foreground" color)))
             (face (or (find-face facename) (lyskom-make-face facename t))))
        (unless extent
          (lyskom-xemacs-or-gnu
           (setq extent (make-extent (or (lp--entry->start-marker entry) 0)
                                     (or (lp--entry->end-marker entry) 0)))
           (setq extent (make-overlay (or (lp--entry->start-marker entry) 0)
                                      (or (lp--entry->end-marker entry) 0)
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
To save time, the format string is cached in `lp--last-format-string'.  It is
only recomputed if the window width changes."
  (if (and lp--last-format-string
           (eq (window-width) lp--last-window-width))
      lp--last-format-string
    (let ((total (- (window-width) 1 3 3 2 12 2 5 2 3 1)))
      (setq lp--last-window-width (window-width))
      (setq lp--last-format-string
            (concat "%#1c %=3#2s %#9c %=-" (number-to-string total)
                    "#3M  %=-12#4s %[%#13@%=5#5s%]  %[%#10@%#6c%]%[%#11@%#7c%]%[%#12@%#8c%]")))))

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
                    (lp--compute-format-string)
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
                    '(lp--unread t)
                    )))
      (if defer-info
          (lyskom-replace-deferred defer-info string)
        (lyskom-insert-at-point string))

      (if (membership-type->passive
           (membership->type (lp--entry->membership entry)))
          (lp--entry-set-foreground entry (lyskom-face-foreground-name kom-dim-face))
        (lp--entry-set-foreground entry nil))

      (if (lp--entry->selected entry)
          (lp--entry-set-background entry (lyskom-face-background-name kom-mark-face))
        (lp--entry-set-background entry nil)))
))
      

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
              (lyskom-format "Ingen information om när medlemskapet skapades")
            (lyskom-format "%#1s %#2s av %#3P"
                           (if (membership-type->invitation (membership->type (lp--entry->membership entry)))
                               "Inbjuden" "Adderad")
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
  (let ((buffer-read-only nil))
    (insert-before-markers "\n")
    (forward-char -1)
    (set-lp--entry->start-marker entry (point-marker))
    (lp--format-insert-entry entry)
    (set-lp--entry->end-marker entry (point-marker))
    (lp--entry-update-extents entry)
    (forward-char 1)))

(defun lp--erase-entry (entry)
  "Erase the printed representation of the entry ENTRY in the buffer."
  (let ((buffer-read-only nil))
    (delete-region (lp--entry->start-marker entry)
                   (1+ (lp--entry->end-marker entry)))
    (set-lp--entry->start-marker entry nil)
    (set-lp--entry->end-marker entry nil)
    (lp--entry-update-extents entry)))

(defun lp--redraw-entry-mark (entry)
  "Redraw the mark for ENTRY."
  (let ((buffer-read-only nil))
    (lp--save-excursion
     (goto-char (lp--entry->start-marker entry))
     (insert (if (lp--entry->selected entry) ?* ?\ ))
     (if (lp--entry->selected entry)
         (lp--entry-set-background entry (lyskom-face-background-name kom-mark-face))
       (lp--entry-set-background entry nil))
     (delete-char 1)
     (lp--entry-update-extents entry))))

(defun lp--redraw-entry (entry)
  "Redraw the entry ENTRY."
  (lp--save-excursion
   (when (lp--entry->start-marker entry)
     (goto-char (lp--entry->start-marker entry))
     (lp--erase-entry entry))
   (when (lp--entry->visible entry)
     (lp--print-entry entry))))

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
           (let ((bounds (next-text-property-bounds 
                          1 (lp--entry->start-marker entry) 'lp--unread)))
             (when bounds
               (lp--save-excursion
                (let ((buffer-read-only nil))
                  (delete-region (car bounds) (cdr bounds))
                  (goto-char (car bounds))
                  (insert (lyskom-format "%#2@%=5#1s"
                                         (if unread (int-to-string unread) "")
                                         '(lp--unread t))))))))))
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
    (if (lp--entry->visible entry) entry nil)))

(defun lp--get-entry (pos)
  "Return the entry at position POS in the list."
  (elt lp--entry-list pos))

(defun lp--entry-position (entry)
  "Return the position in the list for ENTRY."
  (- (length (lp--all-entries))
     (length (memq entry (lp--all-entries)))))


(defun lp--entry-at (where)
  "Return the entry at WHERE."
  (let ((entry-list (lp--all-entries))
        (found nil)
        (pos (save-excursion (goto-char where)
                             (beginning-of-line)
                             (point))))
    (while (and (not found) entry-list)
      (when (and (lp--entry->start-marker (car entry-list))
                 (lp--entry->end-marker (car entry-list))
                 (<= (lp--entry->start-marker (car entry-list)) pos)
                 (>= (lp--entry->end-marker (car entry-list)) pos))
        (setq found (car entry-list)))
      (setq entry-list (cdr entry-list)))
    found))

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
  "Destructively emove the element at index ELEM from the list L."
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

(defun lp--add-membership-callback (membership)
  (lp--update-buffer (membership->conf-no membership)))


(defun lp--update-buffer (conf-no)
  "Update the entry for CONF-NO in the buffer."
  (lp--save-excursion
    (let ((buffers (lyskom-buffers-of-category 'prioritize)))
      (mapcar (lambda (buffer)
                (set-buffer buffer)
                (let ((entry (lp--conf-no-entry conf-no))
                      (mship (lyskom-get-membership conf-no t)))

                  ;; A new membership

                  (cond 
                   ((null entry)
                    (let* ((pos (or (membership->position mship)
                                    (- (length lyskom-membership)
                                       (length (memq mship lyskom-membership)))))
                           (elem (and pos (lp--get-entry pos)))
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
                          (goto-char (lp--entry->start-marker elem))
                          (lp--set-entry-list
                           (lp--add-to-list pos entry (lp--all-entries)))
                          (lp--print-entry entry)))))

                   ;; We have unsubscribed for good

                   ((null mship)
                    (when entry
                      (lp--set-entry-list
                       (lp--remove-from-list (lp--entry-position entry)
                                             (lp--all-entries)))
                      (lp--erase-entry entry)))

                   ;; The priority or position of a membership has changed

                   ((or (/= (lp--entry->priority entry)
                            (membership->priority mship))
                        (/= (lp--entry-position entry)
                            (membership->position mship)))
                    ;; FIXME: Move the entry
                    )

                   (t (set-lp--entry->membership entry mship)
                      (lp--redraw-entry entry)))))
              buffers))))



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
         ((eq flag 'passive) 'membership-type->passive))
   (membership->type (lp--entry->membership entry))))

(defun lp--flag-menu-set (entry flag value)
  "For the membership in ENTRY, set FLAG to VALUE.
FLAG must be one of 'invitation, 'secret or 'passive."
  (funcall
   (cond ((eq flag 'invitation) 'set-membership-type->invitation)
         ((eq flag 'secret) 'set-membership-type->secret)
         ((eq flag 'passive) 'set-membership-type->passive))
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
            (message "Det gick inte: %s"
                     (lyskom-get-error-text lyskom-errno))))

        ;; Update the display
        (let ((mship
               (blocking-do 'query-read-texts
                            lyskom-pers-no
                            (membership->conf-no (lp--entry->membership entry)))))
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
     (cond ((null cur) (error "No entry at point"))
           (t (lyskom-prioritize-flag-toggle (current-buffer)
                                             (list cur 'invitation)
                                             ""))))))

(defun lp--toggle-passive ()
  "Toggle the passive bit of the current entry"
  (interactive)
  (lp--save-excursion
   (let ((cur (lp--entry-at (point))))
     (cond ((null cur) (error "No entry at point"))
           (t (lyskom-prioritize-flag-toggle (current-buffer)
                                             (list cur 'passive)
                                             ""))))))

(defun lp--toggle-secret ()
  "Toggle the secret bit of the current entry"
  (interactive)
  (lp--save-excursion
   (let ((cur (lp--entry-at (point))))
     (cond ((null cur) (error "No entry at point"))
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

(defun lp--entry-set-visible (entry-list state)
  "Set the visibility of all entries in ENTRY-LIST to STATE."
  (when (null state)
    (lp--select-entries entry-list nil))
  (mapcar (lambda (entry) 
            (unless (eq (lp--entry->visible entry) state)
              (let ((pos (lp--get-entry 
                          (lp--next-visible-entry
                           (lp--entry-position entry)))))
                (if pos
                    (goto-char (lp--entry->start-marker pos))
                  (goto-char lp--list-end-marker))
              (set-lp--entry->visible entry state)
              (lp--redraw-entry entry))))
          entry-list))



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
        (unless (eq old-pri (lp--entry->priority mship))
          (lyskom-change-membership-priority (membership->conf-no mship)
                                             (lp--entry->priority mship)))
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
  (let ((step (signum delta))
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


;;; ------------------------------------------------------------
;; User-level functions


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
      (unless entry (error "No membership at point"))
      (setq priority (membership->priority (lp--entry->membership entry)))))

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
;; Reprioritization functions

(defun lp--set-entry-pri-and-pos (entry priority position)
  "Set the priority of ENTRY to PRIORITY and the position to POSITION.
If PRIORITY or POSITION is nil, the parameter is ignored.
If the position changes, lp--move-entry is called.
lp--update-membership is called automatically before this function exits."
  (let ((old-pri (lp--entry->priority entry))
        (old-pos (lp--entry-position entry))
        (need-redraw nil))
    (when (and priority (not (eq priority old-pri)))
      (set-lp--entry->priority entry priority)
      (set-membership->priority (lp--entry->membership entry) priority)
      (setq need-redraw t))
    (when (and position (not (eq position old-pos)))
      (lp--move-entry entry position)
      (setq need-redraw nil))
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
     (cond ((null cur) (error "No entry at point"))
           ((null entries) (error "No entries selected"))
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
      (error "No entries selected"))
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
       (error "Already at minimum priority"))

     (when (and cur
                (eq pri new-pri)
                (eq pri 255)
                (eq pos 0))
       (error "Already at maximum priority"))

     (cond ((null cur) (error "Nor on an entry"))
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
     (cond ((null cur) (error "Not on an entry"))
           ((null prev) (error "Beginning of list"))
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
     (cond ((null cur) (error "Not on an entry"))
           ((null prev) (error "End of list"))
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
;; Motion commands

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
                        ((and (null entry) (< (point)
                                              (lp--entry->start-marker 
                                               (lp--get-entry first-entry)))) 0)
                        ((and (null entry) (> (point) 
                                              (lp--entry->end-marker last-entry)))
                         (1- (length (lp--all-entries))))
                        (t (max 0 (+ pos (lp--calculate-distance pos (- count))))))))
    (condition-case nil
        (goto-char (lp--entry->start-marker (lp--get-entry new-pos)))
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
                     0)
                    ((and (null entry) (> (point) 
                                          (lp--entry->end-marker 
                                           last-entry)))
                     (1- (length (lp--all-entries))))
                    (t (min (1- (length (lp--all-entries)))
                            (+ pos (lp--calculate-distance pos count)))))))
    (condition-case nil
        (goto-char (lp--entry->start-marker (lp--get-entry new-pos)))
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
            (lyskom-read-num-range 0 255 "Gå till prioritet: " t)))

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

(defun lp--expand-all (arg)
  "Expand all entries. 
With prefix arg, expand only those that were created by someone else."
  (interactive "P")
  (let ((hidden-list (list lyskom-pers-no 0)))
    (mapcar (lambda (entry)
              (when (and (or (null arg)
                             (not (memq (membership->created-by
                                         (lp--entry->membership entry))
                                        hidden-list)))
                         (not (eq (lp--entry->state entry) 'expanded)))
                (set-lp--entry->state entry 'expanded)
                (lp--redraw-entry entry)))
            (lp--all-entries))))

(defun lp--contract-all (arg)
  "Contract all entries.
With prefix arg, contract only those that were created by self."
  (interactive "P")
  (let ((hidden-list (list lyskom-pers-no 0)))
    (mapcar (lambda (entry)
              (when (and (or (null arg)
                             (memq (membership->created-by
                                    (lp--entry->membership entry))
                                   hidden-list))
                         (not (eq (lp--entry->state entry) 'contracted)))
                (set-lp--entry->state entry 'contracted)
                (lp--redraw-entry entry)))
            (lp--all-entries))))

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





(defvar lp--mode-map nil
  "Keymap used in lp--mode")

(if lp--mode-map
    nil
  (setq lp--mode-map (make-keymap))
  (suppress-keymap lp--mode-map)
  (define-key lp--mode-map (kbd "SPC") 'lp--toggle-membership-selection)
  (define-key lp--mode-map (kbd "p")   'lp--set-priority)
  (define-key lp--mode-map (kbd "C-w") 'lp--select-region)
  (define-key lp--mode-map (kbd "C-y") 'lp--yank)
  (define-key lp--mode-map (kbd "#")   'lp--select-priority)
  (define-key lp--mode-map (kbd "M-#")   'lp--select-priority)
  (define-key lp--mode-map (kbd "M-DEL") 'lp--deselect-all)
  (define-key lp--mode-map (kbd "C-p") 'lp--previous-entry)
  (define-key lp--mode-map (kbd "<up>") 'lp--previous-entry)
  (define-key lp--mode-map (kbd "C-n") 'lp--next-entry)
  (define-key lp--mode-map (kbd "<down>") 'lp--next-entry)
  (define-key lp--mode-map (kbd "M-<") 'lp--first-entry)
  (define-key lp--mode-map (kbd "M->") 'lp--last-entry)
  (define-key lp--mode-map (kbd "g") 'lp--goto-priority)
  (define-key lp--mode-map (kbd "RET") 'lp--toggle-entry-expansion)
  (define-key lp--mode-map (kbd "+") 'lp--increase-priority)
  (define-key lp--mode-map (kbd "-") 'lp--decrease-priority)
  (define-key lp--mode-map (kbd "M-p") 'lp--move-up)
  (define-key lp--mode-map (kbd "M-n") 'lp--move-down)
  (define-key lp--mode-map (kbd "I") 'lp--toggle-invitation)
  (define-key lp--mode-map (kbd "H") 'lp--toggle-secret)
  (define-key lp--mode-map (kbd "P") 'lp--toggle-passive)
  (define-key lp--mode-map (kbd "C-c C-c") 'lp--quit)
  (define-key lp--mode-map (kbd "q") 'lp--quit)

  (define-key lp--mode-map (kbd (lyskom-keys 'button2up)) 'kom-button-click)
  (define-key lp--mode-map (kbd (lyskom-keys 'button2)) 'kom-mouse-null)
  (define-key lp--mode-map (kbd (lyskom-keys 'button3)) 'kom-popup-menu)
  (define-key lp--mode-map (kbd (lyskom-keys 'button3up)) 'kom-mouse-null)
  (define-key lp--mode-map (kbd "*") 'kom-button-press)
  (define-key lp--mode-map (kbd "=") 'kom-menu-button-press)
  (define-key lp--mode-map (kbd "TAB") 'kom-next-link)
  (define-key lp--mode-map (kbd "M-TAB") 'kom-previous-link)
  (define-key lp--mode-map (kbd "C-i") 'kom-next-link)
  (define-key lp--mode-map (kbd "M-C-i") 'kom-previous-link)
  )


;;; ============================================================
;; The mode

(defvar lp--mode-line '("" 
                        mode-line-modified
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
                        (-3 . "%p")
                        "-%-"))

(defvar lp--mode-line-selected ""
  "String showing number of selected entries.")

(defun lp--update-mode-line ()
  (setq lp--mode-line-selected
        (cond ((= (length (lp--all-selected-entries)) 0)
               (lyskom-get-string 'no-selection))
               (t (format (lyskom-get-string 'selection)
                          (length (lp--all-selected-entries))))))
  (force-mode-line-update))


(def-kom-command kom-handle-membership ()
  "Pop up a buffer to manage memberships in"
  (interactive)
  (let ((kom-deferred-printing nil))
    (set-buffer (lp--create-buffer)))
  (lp--mode)
  (lp--first-entry))

(defun lp--mode ()
  "\\<lyskom-prioritize-mode-map>Mode for prioritizing conferences in LysKOM.

Commands:
TBD.

All bindings:
\\{lyskom-prioritize-mode-map}
Entry to this mode runs lyskom-prioritize-mode-hook."
  (interactive)
  (setq major-mode 'lp--mode)
  (setq mode-name "Prioritize")
  (make-local-variable 'lp--last-format-string)
  (make-local-variable 'lp--last-window-width)
  (make-local-variable 'lp--entry-list)
  (make-local-variable 'lp--list-start-marker)
  (make-local-variable 'lp--list-end-marker)
  (make-local-variable 'lp--selected-entry-list)
  (make-local-variable 'lp--mode-line-selected)
  (setq lp--mode-line-selected "")

  (setq mode-line-format lp--mode-line)
  (lp--update-mode-line)
  (setq buffer-read-only t)
  (lyskom-use-local-map lp--mode-map)
  (lyskom-add-hook 'lyskom-add-membership-hook
                   'lp--add-membership-callback
                    t)
;;  (lyskom-add-hook 'lyskom-remove-membership-hook 
;;                   'lyskom-prioritize-remove-membership
;;                   t)
;;  (lyskom-add-hook 'lyskom-replace-membership-hook 
;;                   'lyskom-prioritize-replace-membership
;;                   t)
  (run-hooks 'lp--mode-hook))


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
        (setq lp--entry-list nil)
        (setq lp--buffer-done nil)
        (lyskom-format-insert "\
Medlemskap för %#1M på %#2s

===============================================================================
 Prio   Möte                                            Senast inne  Oläst  IHP
-------------------------------------------------------------------------------
" lyskom-pers-no lyskom-server-name)
        (setq lp--list-start-marker (point-marker))
        (goto-char (point-max))
        (lyskom-sort-membership)
        (lyskom-display-buffer buf)
        (lyskom-traverse mship (lyskom-default-value 'lyskom-membership)
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
        (insert "\
===============================================================================
 Markera medlemskap: SPC      Markera område: C-w      Flytta markerade:   C-y
 Sätt prioritet:     p        Öka prioritet:  +        Minska prioritet:   -
 Flytta upp:         M-p      Flytta ned:     M-n      Ändra flaggor:    I,H,P
 Avsluta:            C-c C-c                           Mer hjälp:        C-h m
")
        buf
        ))
    (lyskom-wait-queue 'deferred)
    ))


(defun lp--hide-memberships-by-date (arg)
  (interactive "P")
  (let ((old-entries nil)
        (old-time (lyskom-read-date (if arg
                                        "Hide memberships read after: "
                                      "Hide memberships not read since: "))))
    (mapcar (lambda (entry) 
              (cond ((and arg
                          (not (lyskom-time-greater
                                old-time 
                                (membership->last-time-read
                                 (lp--entry->membership entry)))))
                     (setq old-entries (cons entry old-entries)))
                    ((and (not arg) 
                          (lyskom-time-greater old-time 
                                               (membership->last-time-read
                                                (lp--entry->membership entry))))
                     (setq old-entries (cons entry old-entries)))
                    ))
            (lp--all-entries))
    (lp--entry-set-visible old-entries nil)))


(defun lp--hide-entry ()
  (interactive)
  (let ((entry (lp--entry-at (point))))
    (lp--entry-set-visible (list entry) nil)))

(defun lp--show-all ()
  (interactive)
  (let ((elements nil))
    (mapcar (lambda (entry) (unless (lp--entry->visible entry)
                              (setq elements (cons entry elements))))
            (lp--all-entries))
    (lp--entry-set-visible elements t)))

(provide 'mship-edit)

;;; mship-edit.el ends here
;;; Local Variables:
;;; eval: (put 'lp--save-excursion 'edebug-form-spec t)
;;; end:


