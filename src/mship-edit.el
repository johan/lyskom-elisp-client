;;; mship-edit.el --- Summary
;; TO DO
;; see tmp.el
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
;; Do this under lyskom-update-membership
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
;; Do this under lyskom-update-membership
;;


;;; Commentary:
;; 

;;; Code:
(def-komtype lp--entry
  start-marker                          ; Where the entry is in the buffer
  end-marker                            ; Where it ends in the buffer
  priority                              ; The saved priority of the membership
  membership                            ; The membership
  selected                              ; Selected or not
  state                                 ; Expanded display or not
  )

(defvar lp--last-format-string nil)
(defvar lp--last-window-width -1)

;;; Local variables in the prioritize buffer

(defvar lp--entry-list nil)
(defvar lp--list-start-marker nil)
(defvar lp--list-end-marker nil)
(defvar lp--selected-entry-list nil)




;;; ============================================================
;; Entry drawing

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
                    "#3M  %=-12#4s %=5#5s  %[%#10@%#6c%]%[%#11@%#7c%]%[%#12@%#8c%]")))))

(defun lp--format-entry (entry)
  "Format ENTRY for insertion in a buffer.
Returns a string suitable for insertion in a membership list."
  (let ((un (lyskom-find-unread (membership->conf-no
                              (lp--entry->membership entry))))
        (conf-stat (blocking-do 'get-conf-stat
                                (membership->conf-no
                                 (lp--entry->membership entry)))))
    (concat
     (lyskom-format (lp--compute-format-string)
                    (if (lp--entry->selected entry) ?* ?\ )
                    (if (zerop (membership->priority
                                (lp--entry->membership entry)))
                        "-"
                      (int-to-string (membership->priority
                                (lp--entry->membership entry))))
                    conf-stat
                    (lyskom-return-date-and-time
                     (membership->last-time-read (lp--entry->membership entry))
                     'time-yyyy-mm-dd)
                    (if un (int-to-string un) "")
                    
                    (if (membership-type->invitation (membership->type (lp--entry->membership entry))) ?I ?.)
                    (if (membership-type->secret (membership->type (lp--entry->membership entry))) ?H ?.)
                    (if (membership-type->passive (membership->type (lp--entry->membership entry))) ?P ?.)
                    (if (eq lyskom-pers-no (conf-stat->supervisor conf-stat)) ?O ?\ )
                    (lyskom-default-button 'prioritize-flag-menu
                                           (list entry 'invitation)
                                           (list "%#1s (%=#2M)"
                                                 "Inbjuden"
                                                 (membership->conf-no
                                                  (lp--entry->membership entry))))
                    (lyskom-default-button 'prioritize-flag-menu
                                           (list entry 'secret)
                                           (list "%#1s (%=#2M)"
                                                 "Hemlig"
                                                 (membership->conf-no
                                                  (lp--entry->membership entry))))
                    (lyskom-default-button 'prioritize-flag-menu
                                           (list entry 'passive)
                                           (list "%#1s (%=#2M)"
                                                 "Passiv"
                                                 (membership->conf-no
                                                  (lp--entry->membership entry)))))
     (if (or (eq (lp--entry->state entry) 'expanded)
             (and (not (eq (membership->created-by (lp--entry->membership entry))
                           lyskom-pers-no))
                  (not (eq (membership->created-by (lp--entry->membership entry)) 0))
                  (not (eq (lp--entry->state entry) 'contracted))))
         (lyskom-format "\n        %#1s %#2s av %#3P"
                        (if (membership-type->invitation (membership->type (lp--entry->membership entry)))
                            "Inbjuden" "Adderad")
                        (lyskom-return-date-and-time
                         (membership->created-at (lp--entry->membership entry)))
                        (membership->created-by (lp--entry->membership entry)))
       ""))))

(defun lp--print-entry (entry)
  "Print the entry ENTRY at the current position in the buffer.
The start and end markers of the entry are adjusted"
  (let ((buffer-read-only nil))
    (insert-before-markers "\n")
    (forward-char -1)
    (set-lp--entry->start-marker entry (point-marker))
    (insert (lp--format-entry entry))
    (set-lp--entry->end-marker entry (point-marker))
    (forward-char 1)))

(defun lp--erase-entry (entry)
  "Erase the printed representation of the entry ENTRY in the buffer."
  (let ((buffer-read-only nil))
    (delete-region (lp--entry->start-marker entry)
                   (1+ (lp--entry->end-marker entry)))
    (set-lp--entry->start-marker entry nil)
    (set-lp--entry->end-marker entry nil)))

(defun lp--redraw-entry-mark (entry)
  "Redraw the mark for ENTRY."
  (let ((buffer-read-only nil))
    (lp--save-excursion
     (goto-char (lp--entry->start-marker entry))
     (insert (if (lp--entry->selected entry) ?* ?\ ))
     (delete-char 1))))

(defun lp--redraw-entry (entry)
  "Redraw the entry ENTRY."
  (lp--save-excursion
   (goto-char (lp--entry->start-marker entry))
   (lp--erase-entry entry)
   (lp--print-entry entry)))



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
  (let ((cur (lp--get-entry to)))
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


(defun lyskom-prioritize-update-buffer (conf-no)
  "Update the entry for CONF-NO in the buffer."
  (lp--save-excursion
    (let ((buffers (lyskom-buffers-of-category 'prioritize)))
      (mapcar (lambda (buffer)
                (set-buffer buffer)
                (let ((entry (lp--conf-no-entry conf-no))
                      (mship (lyskom-get-membership conf-no t)))
                  (cond ((null entry)
                         ;; FIXME: Insert a new entry
                         )
                        ((null mship)
                         ;; FIXME: Delete an entry
                         )
                        ((/= (lp--entry->priority entry)
                             (membership->priority mship))
                         ;; FIXME: Move the entry
                         )
                        (t (set-lp--entry->membership mship)
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
          (lyskom-replace-membership mship lyskom-membership)
          (set-lp--entry->membership entry mship)
          (when (eq flag 'passive)
            (cond ((membership-type->passive (membership->type mship))
                   (save-excursion
                     (set-buffer lyskom-buffer)
                     (when (eq (membership->conf-no mship) lyskom-current-conf)
                       (set-read-list-empty lyskom-reading-list)
                       (lyskom-run-hook-with-args 'lyskom-change-conf-hook
                                                  lyskom-current-conf 0)
                       (setq lyskom-current-conf 0))
                     (read-list-delete-read-info (membership->conf-no mship)
                                                 lyskom-to-do-list)
                     (lyskom-update-prompt t)))
                  (t (lyskom-prefetch-map (membership->conf-no mship)
                                          mship))))
          (blocking-do 'get-conf-stat (membership->conf-no mship))))
      (lp--redraw-entry entry))))



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
              (if state
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


;;; ------------------------------------------------------------
;; Server update functions

(defun lp--update-membership (entry)
  "Update the server and local versions of membership in ENTRY."
  (save-excursion
    (set-buffer lyskom-buffer)
    (let ((mship (lp--entry->membership entry)))
      (lyskom-change-membership-position (membership->conf-no mship)
                                         (lp--entry-position entry))
      (lyskom-change-membership-priority (membership->conf-no mship)
                                         (lp--entry->priority mship))
      (initiate-add-member 'background nil
                           (membership->conf-no mship)
                           lyskom-pers-no
                           (membership->priority mship)
                           (membership->position mship)
                           (membership->type mship)))))


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
  "Select all entries in the region.
START and END are the starting and ending points of the region."
  (interactive "r")
  (let ((entry-list (lp--map-region start end 'identity)))
    (lp--select-entries entry-list t)))

(defun lp--select-prioriy (priority)
  "Select all entries with a priority PRIORITY.
With numeric prefix argument select entries with that priority."
  (interactive "P")
  (lp--do-select-priority priority t))

(defun lp--deselect-prioriy (priority)
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

(defun lp--yank ()
  "Insert all the selected memberships before the entry at point."
  (interactive)
  (let* ((cur (lp--entry-at (point)))
         (pos (and cur (lp--entry-position cur)))
         (priority (and cur (lp--entry->priority cur)))
         (entries (lp--all-selected-entries)))
    (cond ((null cur) (error "No entry at point"))
          ((null entries) (error "No entries selected"))
          (t (mapcar 
              (lambda (entry)
                (set-lp--entry->priority entry priority)
                (set-membership->priority
                 (lp--entry->membership entry) priority)
                (lp--move-entry entry (lp--entry-position 
                                       (lp--find-new-position entry nil pos)))
                (lp--update-membership entry))
              entries)))))
          


(defun lp--set-priority (priority)
  "Set the priority of selected memberships to PRIORITY.
Memberships that must be moved will be moved the shortest distance
possible in the list."
  (interactive "P")
  (let* ((cur (lp--entry-at (point)))
         (entries (or (lp--all-selected-entries)
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
                 (set-lp--entry->priority entry priority)
                 (set-membership->priority
                  (lp--entry->membership entry) priority)
                 (lp--move-entry entry new-pos)))
             entries)
     (mapcar 'lp--update-membership entries))))

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
           (t (let ((new-pos (lp--entry-position (lp--find-new-position cur (+ pri amount)))))
                (set-lp--entry->priority cur new-pri)
                (set-membership->priority
                 (lp--entry->membership cur) new-pri)
                (lp--move-entry cur new-pos)
                (lp--update-membership cur)))))))

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
          (prev (and cur (> pos 1) (lp--get-entry (1- pos)))))
     (cond ((null cur) (error "Not on an entry"))
           ((null prev) (error "Beginning of list"))
           (t (if (/= (lp--entry->priority cur)
                      (lp--entry->priority prev))
                  (progn (set-lp--entry->priority cur (lp--entry->priority prev))
                         (set-membership->priority (lp--entry->membership cur) (lp--entry->priority prev))
                         (lp--redraw-entry cur))
                (lp--move-entry cur (1- pos)))
              (lp--update-membership cur))))))

(defun lp--move-down ()
  "Move the current entry up down notch."
  (interactive)
  (lp--save-excursion
   (let* ((cur (lp--entry-at (point)))
          (pos (and cur (lp--entry-position cur)))
          (prev (and cur (lp--get-entry (1+ pos)))))
     (cond ((null cur) (error "Not on an entry"))
           ((null prev) (error "End of list"))
           (t (if (/= (lp--entry->priority cur)
                      (lp--entry->priority prev))
                  (progn (set-lp--entry->priority cur (lp--entry->priority prev))
                         (set-membership->priority (lp--entry->membership cur) (lp--entry->priority prev))
                         (lp--redraw-entry cur))
                (lp--move-entry cur (1+ pos)))
              (lp--update-membership cur))))))




;;; ============================================================
;; Motion commands

(defun lp--previous-entry (count)
  "Move the cursor up COUNT lines.
The cursor will always move to the start of the target entry."
  (interactive "p")
  (let* ((entry (lp--entry-at (point)))
         (pos (max 0 (- (lp--entry-position entry) count))))
    (goto-char (lp--entry->start-marker (lp--get-entry pos)))))

(defun lp--next-entry (count)
  "Move the cursor down COUNT lines.
The cursor will always move to the start of the target entry."
  (interactive "p")
  (let* ((entry (lp--entry-at (point)))
         (pos (min (1- (length (lp--all-entries)))
                   (+ (lp--entry-position entry) count))))
    (condition-case nil
        (goto-char (lp--entry->start-marker (lp--get-entry pos)))
      (error nil))))

(defun lp--first-entry ()
  "Move point to the first entry in the membership list."
  (interactive)
  (condition-case nil
      (goto-char (lp--entry->start-marker (lp--get-entry 0)))
    (error nil)))

(defun lp--last-entry ()
  "Move point to the last entry in the membership list."
  (interactive)
  (condition-case nil
      (goto-char
       (lp--entry->start-marker (lp--get-entry (1- (length (lp--all-entries))))))
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
            (lyskom-read-num-range 0 255 "G� till prioritet: " t)))

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


(def-kom-command kom-handle-membership ()
  "Pop up a buffer to manage memberships in"
  (interactive)
  (set-buffer (lp--create-buffer))
  (lp--mode))

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
;;  (lyskom-add-hook 'lyskom-add-membership-hook
;;                   'lyskom-prioritize-add-membership
;;                   t)
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
                                       (concat (buffer-name) "-prioritize")
                                       t))
        (entry-list nil))

    ;;; First cache all the conf stats
    (lyskom-save-excursion
      (set-buffer buf)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (make-local-variable 'lp--entry-list)
        (make-local-variable 'lp--list-start-marker)
        (make-local-variable 'lp--list-end-marker)
        (setq lp--entry-list nil)
        (lyskom-format-insert "\
Medlemskap f�r %#1M p� %#2s

===============================================================================
 Prio   M�te                                            Senast inne  Ol�st  IHP
-------------------------------------------------------------------------------
" lyskom-pers-no lyskom-server-name)
        (setq lp--list-start-marker (point-marker))
        (goto-char (point-max))
        (lyskom-sort-membership)
        (lyskom-display-buffer buf)
        (lyskom-traverse mship (lyskom-default-value 'lyskom-membership)
          (blocking-do 'get-conf-stat (membership->conf-no mship))
          (let ((entry (lyskom-create-lp--entry nil ; Start
                                                nil ; End
                                                (membership->priority mship)
                                                mship
                                                nil
                                                'normal)))
            (lp--print-entry entry)
            (setq entry-list (cons entry entry-list))))
        (lp--set-entry-list (nreverse entry-list))
        (setq lp--list-end-marker (point-marker))
        (insert "\
===============================================================================
 Markera medlemskap: SPC      Markera omr�de: C-w      Flytta markerade:   C-y
 S�tt prioritet:     p        �ka prioritet:  +        Minska prioritet:   -
 Flytta upp:         M-p      Flytta ned:     M-n      �ndra flaggor:    I,H,P
 Avsluta och spara:  C-c C-c                           Mer hj�lp:        C-h m
")
        buf
        ))))



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



(defun lp--update-mode-line ()
  (setq lp--mode-line-selected
        (cond ((= (length (lp--all-selected-entries)) 0)
               (lyskom-get-string 'no-selection))
               (t (format (lyskom-get-string 'selection)
                          (length (lp--all-selected-entries))))))
  (force-mode-line-update))

(provide 'mship-edit)

;;; mship-edit.el ends here
