(def-komtype lp--entry 
  start-marker                          ; Where the entry is in the buffer
  end-marker                            ; Where it ends in the buffer
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

(defun lp--compute-format-string ()
  (if (and lp--last-format-string
           (eq (window-width) lp--last-window-width))
      lp--last-format-string
    (let ((total (- (window-width) 1 3 3 2 12 2 5 2 3 1)))
      (setq lp--last-window-width (window-width))
      (setq lp--last-format-string
            (concat "%#1c %=3#2d %#9c %=-" (number-to-string total) 
                    "#3M  %=-12#4s %=5#5s  %[%#10@%#6c%]%[%#11@%#7c%]%[%#12@%#8c%]")))))
          

(defun lp--find-unread (conf-no)
  (save-excursion
   (set-buffer lyskom-buffer)
   (let ((rlist (read-list->all-entries lyskom-to-do-list))
         (found nil))
     (while (and (not found) rlist)
       (when (eq conf-no (conf-stat->conf-no 
                          (read-info->conf-stat (car rlist))))
         (setq found (length (cdr (read-info->text-list (car rlist))))))
       (setq rlist (cdr rlist)))
     found)))

(defun lp--format-entry (entry)
  (let ((un (lp--find-unread (membership->conf-no
                              (lp--entry->membership entry))))
        (conf-stat (blocking-do 'get-conf-stat
                                (membership->conf-no 
                                 (lp--entry->membership entry)))))
    (concat 
     (lyskom-format (lp--compute-format-string)
                    (if (lp--entry->selected entry) ?* ?\ )
                    (membership->priority (lp--entry->membership entry))
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
  (insert-before-markers "\n")
  (forward-char -1)
  (set-lp--entry->start-marker entry (point-marker))
  (insert (lp--format-entry entry))
  (set-lp--entry->end-marker entry (point-marker))
  (forward-char 1))

(defun lp--erase-entry (entry)
  "Erase the printed representation of the entry ENTRY in the buffer"
  (delete-region (lp--entry->start-marker entry)
                 (1+ (lp--entry->end-marker entry)))
  (set-lp--entry->start-marker entry nil)
  (set-lp--entry->end-marker entry nil))

(defun lp--redraw-entry-mark (entry)
  "Redraw the mark for ENTRY."
  (save-excursion
    (goto-char (lp--entry->start-marker entry))
    (insert (if (lp--entry->selected entry) ?* ?\ ))
    (delete-char 1)))

(defun lp--redraw-entry (entry)
  "Redraw the entry ENTRY."
  (let ((col (- (point) (lp--entry->start-marker entry)))
        (set (eq (lp--entry-at (point)) entry)))
    (save-excursion
      (goto-char (lp--entry->start-marker entry))
      (lp--erase-entry entry)
      (lp--print-entry entry))
    (when set (goto-char (+ (lp--entry->start-marker entry) col)))))


(defun lp--create-buffer ()
  (interactive)
  (let ((buf (lyskom-get-buffer-create 'prioritize
                                       (concat (buffer-name) "-prioritize")
                                       t))
        (entry-list nil))

    ;;; First cache all the conf stats
    (lyskom-save-excursion
      (set-buffer buf)
      (erase-buffer)
      (make-local-variable 'lp--entry-list)
      (make-local-variable 'lp--list-start-marker)
      (make-local-variable 'lp--list-end-marker)
      (setq lp--entry-list nil)
      (lyskom-format-insert "\
Medlemskap för %#1M på %#2s

===============================================================================
 Prio   Möte                                            Senast inne  Oläst  IHP
-------------------------------------------------------------------------------
" lyskom-pers-no lyskom-server-name)
      (setq lp--list-start-marker (point-marker))
      (goto-char (point-max))
      (lyskom-display-buffer buf)
      (lyskom-traverse mship (lyskom-default-value 'lyskom-membership)
        (blocking-do 'get-conf-stat (membership->conf-no mship))
        (let ((entry (lyskom-create-lp--entry nil ; Start
                                              nil ; End
                                              mship
                                              nil
                                              'normal)))
          (lp--print-entry entry)
          (setq entry-list (cons entry entry-list))))
      (lp--set-entry-list (nreverse entry-list))
      (setq lp--list-end-marker (point-marker))
      (insert "\
===============================================================================
 Markera medlemskap: SPC      Markera område: C-w      Flytta markerade:   C-y
 Sätt prioritet:     p        Öka prioritet:  +        Minska prioritet:   -
 Flytta upp:         M-p      Flytta ned:     M-n      Ändra flaggor:    I,H,P
 Avsluta och spara:  C-c C-c                           Mer hjälp:        C-h m
"))))

(defun lp--set-entry-list (entries)
  "Set the list of entries to ENTRIES"
  (setq lp--entry-list entries))

(defun lp--all-entries ()
  "Return a list of all entries."
  lp--entry-list)

(defun lp--get-entry (pos)
  "Return the entry at position POS in the list."
  (elt lp--entry-list pos))

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
      (goto-char (lp--list-end-marker)))
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

(defun lp--list-move-element (elem to l)
  "Move element from position FROM to position TO in list L using side-fx."
  (lp--add-to-list to elem (lp--remove-from-list elem l)))

(defun lp--map-region (start end function &rest args)
  "For each element from START to END, apply FUNCTION.
Apply FUNCTION to each element in the region from START to END, returning
a list of results. ARGS will be passed as additional arguments to FUNCTION.

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
;;; Menu and button functions

;;; ------------------------------------------------------------
;;; Flag menu
;;; Argument is a list of entry and flag

(defun lp--flag-menu-get (entry flag)
  (funcall
   (cond ((eq flag 'invitation) 'membership-type->invitation)
         ((eq flag 'secret) 'membership-type->secret)
         ((eq flag 'passive) 'membership-type->passive))
   (membership->type (lp--entry->membership entry))))

(defun lp--flag-menu-set (entry flag value)
  (funcall
   (cond ((eq flag 'invitation) 'set-membership-type->invitation)
         ((eq flag 'secret) 'set-membership-type->secret)
         ((eq flag 'passive) 'set-membership-type->passive))
   (membership->type (lp--entry->membership entry))
   value))

(defun lyskom-prioritize-flag-clear (buf arg text)
  (interactive)
  (let ((entry (elt arg 0))
        (flag (elt arg 1)))
    (when (lp--flag-menu-get entry flag)
      (lyskom-prioritize-flag-toggle buf arg text))))

(defun lyskom-prioritize-flag-set (buf arg text)
  (interactive)
  (let ((entry (elt arg 0))
        (flag (elt arg 1)))
    (unless (lp--flag-menu-get entry flag)
      (lyskom-prioritize-flag-toggle buf arg text))))

(defun lyskom-prioritize-flag-toggle (buf arg text)
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
;;; Marking and unmarking memberships
;;; 

(defun lp--select-entries (entry-list state)
  "Set the selection value of all entries in ENTRY-LIST to STATE.
Forces a mode line update"
  (lp--do-select-entries entry-list state)
  (force-mode-line-update))

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
  "Return a list of all selected entries"
  lp--selected-entry-list)

(defun lp--set-selected-entries (entry-list)
  "Set the selected entries to exactly the entries in entry-list.
Forces a mode line update"
  (lp--do-select-entries (lp--all-selected-entries) nil)
  (lp--do-select-entries entry-list t)
  (force-mode-line-update))


;;; ------------------------------------------------------------
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
  "Toggle selection of the membership that point is on."
  (interactive)
  (let ((entry (lp--entry-at (point))))
    (when entry
      (lp--select-entries (list entry) (not (lp--entry->selected entry))))))

(defun lp--select-region (start end)
  "Select all entries in the region"
  (interactive "r")
  (let ((entry-list (lp--map-region start end 'identity)))
    (lp--select-entries entry-list t)))

(defun lp--select-prioriy (priority)
  "Select all entries with a certain priority.
With numeric prefix argument select entries with that priority."
  (interactive "P")
  (lp--do-select-priority priority t))

(defun lp--deselect-prioriy (priority)
  "Deselect all entries with a certain priority.
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
  "Deselect all memberships"
  (interactive)
  (lp--set-selected-entries nil))



;;; ============================================================
;;; Motion commands




