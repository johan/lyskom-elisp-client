;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: prioritize.el,v 44.9 1998-06-02 12:15:09 byers Exp $
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
;;;;
;;;; File: prioritize.el
;;;; Author: David Byers
;;;;
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: prioritize.el,v 44.9 1998-06-02 12:15:09 byers Exp $\n"))




;;; ================================================================
;;; Global variables
;;;



(defvar lyskom-prioritize-mode-line '("" 
                                      mode-line-modified
                                      mode-line-buffer-identification
                                      "   "
                                      global-mode-string
                                      "   %[("
                                      mode-name
                                      mode-line-process
                                      minor-mode-alist
                                      ")%] "
                                      lyskom-prioritize-mode-line-selected
                                      "--"
                                      (-3 . "%p")
                                      "-%-"))

(defvar lyskom-prioritize-entry-list nil
  "List of entries to be prioritized.")

(defvar lyskom-prioritize-selection nil
  "List of selected entries in the prioritization list.")

(defvar lyskom-prioritize-mode-line-selected ""
  "String showing number of selected entries.")



;;; ================================================================
;;; Data types
;;;

(defun make-prioritize-entry (prio conf-stat)
  (vector prio conf-stat nil nil))

(defun prioritize-entry->priority (el)
  (aref el 0))

(defun prioritize-entry->name (el)
  (conf-stat->name (aref el 1)))

(defun prioritize-entry->conf-stat (el)
  (aref el 1))

(defun prioritize-entry->selected (el)
  (aref el 2))

(defun set-prioritize-entry->priority (el prio)
  (aset el 0 prio))

(defun set-prioritize-entry->conf-stat (el conf)
  (aset el 1 conf))

(defun set-prioritize-entry->selected (el marks)
  (aset el 2 marks))



;;; ================================================================
;;; Utility functions
;;;

(defun lyskom-prioritize-remove-from-list (elem l)
  "Destructively emove the element at index ELEM from the list L."
  (if (> elem (length l))
      (error "Args out of range: %S, %d" l elem))
  (if (= 0 elem) 
      (cdr l)
    (setcdr (nthcdr (1- elem) l)
            (nthcdr (1+ elem) l))
    l))

(defun lyskom-prioritize-add-to-list (elem data l)
  "At the position ELEM, add DATA to the list L using side effects."
  (if (> elem (length l))
      (error "Args out of range: %S, %d" l elem))
  (if (= 0 elem)
      (cons data l)
    (setcdr (nthcdr (1- elem) l)
            (cons data (nthcdr elem l)))
    l))

(defun lyskom-prioritize-move-element (from to l)
  "Move element from position FROM to position TO in list L using side-fx."
  (setq from (1- from))
  (setq to (1- to))
  (let ((elem (if (< from 0) nil (elt l from))))
    (lyskom-prioritize-add-to-list 
     to
     elem
     (lyskom-prioritize-remove-from-list from l))))


(defun lyskom-prioritize-current-entry ()
  "Get the entry on the line containing point."
  (save-excursion
    (beginning-of-line)
    (lyskom-prioritize-get-entry-from-no
     (- (1+ (count-lines 1 (point)))
        lyskom-prioritize-header-lines))))

(defun lyskom-prioritize-get-entry-from-no (no)
  "Get entry number NO from the prioritize list."
  (elt lyskom-prioritize-entry-list (1- no)))


(defun lyskom-prioritize-get-no-from-entry (entry)
  "Get the index of entry ENTRY in the prioritize list."
  (1+ (- (length lyskom-prioritize-entry-list)
         (length (memq entry lyskom-prioritize-entry-list)))))


(defun lyskom-prioritize-get-selected ()
  "Get a list of all selected entries."
  lyskom-prioritize-selection)

(defun lyskom-prioritize-find-entry-from-conf (conf-no)
  "Get the entry for conference conf-no"
  (let ((result nil))
    (lyskom-traverse entry lyskom-prioritize-entry-list
      (when (eq (conf-stat->conf-no
                 (prioritize-entry->conf-stat entry))
                conf-no)
        (setq result entry)))
    result))

(defun lyskom-prioritize-get-entry-from-priority (priority first)
  (let ((result nil))
    (lyskom-traverse entry lyskom-prioritize-entry-list
      (cond ((= (prioritize-entry->priority entry)
                priority)
             (when (or (not first)
                       (null result))
               (setq result entry)))
            ((< (prioritize-entry->priority entry)
                priority)
             (when (null result)
               (setq result entry)))))
    result))


(defun lyskom-prioritize-goto-entry (entry)
  "Go to the line containing ENTRY."
  (goto-line (+ lyskom-prioritize-header-lines
                (lyskom-prioritize-get-no-from-entry entry)))
  (beginning-of-line))

(defun lyskom-prioritize-redraw-entry (entry)
  "Redraw the prioritize entry ENTRY in the buffer"
  (save-excursion
    (let ((lineno (+ lyskom-prioritize-header-lines
                     (lyskom-prioritize-get-no-from-entry entry)))
          (buffer-read-only nil))
      (goto-line lineno)
      (delete-region (save-excursion (beginning-of-line) (point))
                     (save-excursion (end-of-line) (point)))
      (insert (lyskom-prioritize-format-entry entry)))))


(defun lyskom-prioritize-format-entry (entry)
  "Return a string representation of the prioritize entry ENTRY"
  (lyskom-format "%#1s %3#2s   %#3M"
                 (if (prioritize-entry->selected entry) "*" " ")
                 (if (= 0 (prioritize-entry->priority entry))
                     "-" (format "%d" (prioritize-entry->priority entry)))
                 (prioritize-entry->conf-stat entry)))


(defun lyskom-prioritize-redraw-buffer ()
  "Update the entire buffer contents"
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert lyskom-prioritize-header)
      (mapcar (function 
               (lambda (el)
                 (insert (concat (lyskom-prioritize-format-entry el)
                                 "\n"))))
              lyskom-prioritize-entry-list))))

(defun lyskom-prioritize-add-membership (membership)
  (let ((buffer (car (lyskom-buffers-of-category 'prioritize))))
    (if (buffer-live-p  buffer)
        (save-excursion
          (set-buffer buffer)
          (let ((tmp (lyskom-prioritize-get-entry-from-no
                      (membership->conf-no membership))))
            (if tmp (lyskom-prioritize-replace-membership 
                     membership
                     (lyskom-default-value 'lyskom-membership))
              (let* ((entry (lyskom-prioritize-get-entry-from-priority 
                             (membership->priority membership) t))
                     (no (lyskom-prioritize-get-no-from-entry entry)))
                (setq lyskom-prioritize-entry-list
                      (lyskom-prioritize-add-to-list 
                       (1- no )
                       (make-prioritize-entry
                        (membership->priority membership)
                        (blocking-do 'get-conf-stat
                                     (membership->conf-no membership)))
                       lyskom-prioritize-entry-list))
                (goto-line (+ no lyskom-prioritize-header-lines))
                (let ((buffer-read-only nil))
                  (open-line 1)
                  (lyskom-prioritize-redraw-entry 
                   (lyskom-prioritize-get-entry-from-no no)))))))
      (lyskom-remove-hook 'lyskom-add-membership-hook
                          'lyskom-prioritize-add-membership))))

(defun lyskom-prioritize-remove-membership (conf-no membership-list)
  (let ((buffer (car (lyskom-buffers-of-category 'prioritize))))
    (if (buffer-live-p buffer)
        (save-excursion
          (set-buffer buffer)
          (let ((entry (lyskom-prioritize-find-entry-from-conf conf-no)))
            (lyskom-prioritize-goto-entry entry)
            (let ((buffer-read-only nil))
              (delete-region (save-excursion (beginning-of-line) (point))
                             (save-excursion (end-of-line) (point)))
              (delete-char 1))
            (setq lyskom-prioritize-entry-list
                  (lyskom-prioritize-remove-from-list 
                   (1- (lyskom-prioritize-get-no-from-entry entry))
                   lyskom-prioritize-entry-list))))
      (lyskom-remove-hook 'lyskom-remove-membership-hook
                          'lyskom-prioritize-remove-membership))))

(defun lyskom-prioritize-replace-membership (membership membership-list)
  (let ((buffer (car (lyskom-buffers-of-category 'prioritize))))
    (if (buffer-live-p buffer)
        (save-excursion
          (set-buffer buffer)
          (let* ((entry (lyskom-prioritize-find-entry-from-conf
                         (membership->conf-no membership)))
                 (target-priority (membership->priority membership))
                 (entry-priority (prioritize-entry->priority entry))
                 (move-up (> target-priority entry-priority))
                 (target-entry
                  (lyskom-prioritize-get-entry-from-priority
                   target-priority
                   (not move-up))))
            (when (not (eq target-priority entry-priority))
              (set-prioritize-entry->priority entry target-priority)
              (lyskom-prioritize-move-entry 
               (lyskom-prioritize-get-no-from-entry entry)
               (+ (lyskom-prioritize-get-no-from-entry target-entry)
                  (cond ((and move-up (= (prioritize-entry->priority 
                                          target-entry)
                                         target-priority)) 1)
                        ((not move-up) -1)
                        (t 0)))
               t t))))
      (lyskom-remove-hook 'lyskom-replace-membership-hook
                          'lyskom-prioritize-replace-membership))))



(defun lyskom-prioritize-move-entry (from to &optional dontset forceup)
  "Move entry from position FROM to position TO.
Non-nil optional DONTSET means don't change priority.
Non-nil optional FORCEUP means force update of entry."
  (let ((inhibit-read-only t))
    (if (/= from to)
        (let ((entry (lyskom-prioritize-get-entry-from-no from))
              (after nil)
              (before nil)
              (start (1+ (count-lines 1 (point)))))
          (if (null entry) (error (lyskom-get-string 
                                   'cant-move-nothing-nowhere)))

          ;;
          ;; Move the entry in the prioritize list
          ;;

          (setq lyskom-prioritize-entry-list
                (lyskom-prioritize-move-element from to
                                                lyskom-prioritize-entry-list))

          (if (not dontset)
              (progn
                (setq after (lyskom-prioritize-get-entry-from-no (1+ to))
                      before (lyskom-prioritize-get-entry-from-no (1- to)))

                (if (> from to)
                    (if (and after
                             (/= (prioritize-entry->priority after)
                                 (prioritize-entry->priority entry)))
                        (set-prioritize-entry->priority 
                         entry
                         (prioritize-entry->priority after)))
                  (if (and before
                           (/= (prioritize-entry->priority before)
                               (prioritize-entry->priority entry)))
                      (set-prioritize-entry->priority 
                       entry
                       (prioritize-entry->priority before))))))

          ;;
          ;; Update the buffer
          ;;

          (goto-line (+ from lyskom-prioritize-header-lines))
          (delete-region (save-excursion (beginning-of-line) (point))
                         (1+ (save-excursion (end-of-line) (point))))
          (goto-line (+ to lyskom-prioritize-header-lines))
          (insert (concat (lyskom-prioritize-format-entry entry)
                          "\n"))
          (goto-line start))
      (if forceup
          (lyskom-prioritize-redraw-entry 
           (lyskom-prioritize-get-entry-from-no from))))))


(defun lyskom-prioritize-update-selection (entry arg)
  (setq lyskom-prioritize-selection
        (let (result)
          (mapcar (function (lambda (x)
                              (if (prioritize-entry->selected x)
                                  (setq result (cons x result)))))
                  lyskom-prioritize-entry-list)
          (nreverse result)))
  (lyskom-prioritize-update-mode-line))


(defun lyskom-prioritize-select (entry arg)
  (set-prioritize-entry->selected entry arg)
  (lyskom-prioritize-update-selection entry arg))



;;; ================================================================
;;; User commands
;;;

(defun kom-prioritize-help ()
  "Get brief help on prioritize mode."
  (interactive)
  (lyskom-message "%s" (lyskom-get-string 'prioritize-help)))

(defun kom-prioritize-deselect-all ()
  "Deselect all selected entries"
  (interactive)
  (let ((entry nil))
    (while lyskom-prioritize-selection
      (setq entry (car lyskom-prioritize-selection))
      (lyskom-prioritize-select entry nil)
      (lyskom-prioritize-redraw-entry entry))))




(defun kom-prioritize-select (&optional arg)
  "Select the record on the line containing point.
If ARG is null, toggle selection. Positive arg means always select and
negative arg means always deselect"
  (interactive "P")
  (let ((entry (lyskom-prioritize-current-entry))
        (start (point)))
    (cond ((or (not (integerp arg))
               (= arg 0))
           (if (prioritize-entry->selected entry)
               (lyskom-prioritize-select entry nil)
             (lyskom-prioritize-select entry t))
           (lyskom-prioritize-redraw-entry entry))
          
          ((> arg 0)
           (if (not (prioritize-entry->selected entry))
               (progn
                 (lyskom-prioritize-select entry t)
                 (lyskom-prioritize-redraw-entry entry))))
          ((< arg 0)
           (if (prioritize-entry->selected entry)
               (progn
                 (lyskom-prioritize-select entry nil)
                 (lyskom-prioritize-redraw-entry entry)))))
    (goto-char start)))



(defun kom-prioritize-next-line (arg)
  "Move forward ARG lines."
  (interactive "p")
  (forward-line arg)
  (let* ((entry (lyskom-prioritize-current-entry)))
    
    (if (< (count-lines 1 (point)) lyskom-prioritize-header-lines)
        (goto-line (1+ lyskom-prioritize-header-lines)))

    (if (> (lyskom-prioritize-get-no-from-entry entry)
           (length lyskom-prioritize-entry-list))
        (goto-line (+ lyskom-prioritize-header-lines
                      (length lyskom-prioritize-entry-list))))

    (setq entry (lyskom-prioritize-current-entry))))


(defun kom-prioritize-previous-line (arg)
  "Move backward ARG lines."
  (interactive "p")
  (kom-prioritize-next-line (- arg)))


(defun kom-prioritize-beginning ()
  "Move to the beginning of the entry list."
  (interactive)
  (set-mark (point))
  (goto-line (1+ lyskom-prioritize-header-lines)))


(defun kom-prioritize-end ()
  "Move to the end of the entry list."
  (interactive)
  (set-mark (point))
  (goto-line (+ lyskom-prioritize-header-lines
                (length lyskom-prioritize-entry-list))))


(defun kom-prioritize-goto-priority (arg)
  "Move to the first entry with priority ARG.
Asks for a priority if no prefix argument is given."
  (interactive "P")
  (let ((prio (or arg (lyskom-read-num-range 
                       0 255 
                       (lyskom-get-string 'goto-priority-prompt))))
        (where 0)
        (entry nil)
        (lineno nil))
    (while (< where (length lyskom-prioritize-entry-list))
      (setq entry (lyskom-prioritize-get-entry-from-no where))
      (if (<= (prioritize-entry->priority entry) prio)
          (setq lineno where
                where (length lyskom-prioritize-entry-list))
        (setq where (1+ where))))
    (if lineno
        (goto-line (+ lineno lyskom-prioritize-header-lines))
      (goto-line (+ lyskom-prioritize-header-lines where)))))


(defun kom-prioritize-move-up (arg)
  "Move current entry up ARG steps.
If the entry is at the top of a priority group, change its priority to
the same as the entry above it, but to not move it."
  (interactive "p")
  (let ((entry (lyskom-prioritize-current-entry)))
    (beginning-of-line)
    (while (> arg 0)
      (let* ((start (lyskom-prioritize-get-no-from-entry entry))
             (target (1- start))
             (before (if (>= target 1)
                         (lyskom-prioritize-get-entry-from-no target)
                       nil)))
        (cond ((null before) (error (lyskom-get-string 'beginning-of-list)))
              ((> (prioritize-entry->priority before)
                  (prioritize-entry->priority entry))
               (set-prioritize-entry->priority 
                entry
                (prioritize-entry->priority before))
               (lyskom-prioritize-redraw-entry entry))
              (t (lyskom-prioritize-move-entry start target)
                 (forward-line -1)))
        (setq arg (1- arg))))))


(defun kom-prioritize-move-down (arg)
  "Move current-entry down ARG steps.
If the entry is at the top of a priority group, change its priority to
the same as the entry above it, but to not move it."
  (interactive "p")
  (let ((entry (lyskom-prioritize-current-entry)))
    (beginning-of-line)
    (while (> arg 0)
      (let* ((start (lyskom-prioritize-get-no-from-entry entry))
             (target (1+ start))
             (after (if (<= target (length lyskom-prioritize-entry-list))
                        (lyskom-prioritize-get-entry-from-no target)
                      nil)))
        (cond ((null after) (error (lyskom-get-string 'end-of-list)))
              ((< (prioritize-entry->priority after)
                  (prioritize-entry->priority entry))
               (set-prioritize-entry->priority 
                entry
                (prioritize-entry->priority after))
               (lyskom-prioritize-redraw-entry entry))
              (t (lyskom-prioritize-move-entry start target)
                 (forward-line 1)))
        (setq arg (1- arg))))))


(defun kom-prioritize-set-priority (arg)
  "Set priority of all selected conferences."
  (interactive "P")
  (set-mark-command nil)
  (let* ((entry (lyskom-prioritize-current-entry))
         (selected (lyskom-prioritize-get-selected))
         (priority (or (and (integerp arg) arg)
                       (lyskom-read-num-range 
                        0 255 
                        (lyskom-format
                         (lyskom-get-string
                          (if selected 'priority-prompt-marked
                            'priority-prompt))
                          (prioritize-entry->conf-stat entry))
                         t))))
    (setq selected (or selected (list entry)))
    (while selected
      (lyskom-prioritize-set-priority (car selected) priority)
      (setq selected (cdr selected)))
    (lyskom-prioritize-goto-entry entry)))



(defun lyskom-prioritize-set-priority (entry priority)
  (let ((list lyskom-prioritize-entry-list)
        (target nil))
    
    (cond ((= (prioritize-entry->priority entry) priority) nil)

          ;;
          ;; Moving up
          ;; Find the LAST ENTRY with EQUAL or HIGHER priority
          ;;

          ((> priority (prioritize-entry->priority entry))
           (while list
             (cond ((>= (prioritize-entry->priority (car list))
                        priority)
                    (setq list (cdr list)))
                   (t (setq target (1+
                                    (- (length lyskom-prioritize-entry-list)
                                       (length list))))
                      (setq list nil))))
           (if (null target)
               (setq target 1))
           (set-prioritize-entry->priority entry priority)
           (lyskom-prioritize-move-entry
            (lyskom-prioritize-get-no-from-entry entry)
            target t t))

          ;;
          ;; Moving down
          ;; Find the FIRST ENTRY with EQUAL or LOWER priority
          ;;
          
          ((< priority (prioritize-entry->priority entry))
           (while list
             (cond ((> (prioritize-entry->priority (car list)) priority)
                    (setq list (cdr list)))
                   (t (setq target (- (length lyskom-prioritize-entry-list)
                                       (length list)))
                      (setq list nil))))
           (if (null target)
               (setq target (length lyskom-prioritize-entry-list)))
           (set-prioritize-entry->priority entry priority)
           (lyskom-prioritize-move-entry
            (lyskom-prioritize-get-no-from-entry entry)
            target t t)))))

            

(defun kom-prioritize-reprioritize ()
  "Reprioritize all entries with a given priority."
  (interactive)
  (let* ((tmp (lyskom-prioritize-current-entry))
         (default (if tmp (prioritize-entry->priority tmp) nil))
         (prio-from (lyskom-read-num-range
                     0 255 
                     (lyskom-get-string 'reprioritize-from)
                     t default))
         (prio-to (lyskom-read-num-range
                   0 255
                   (lyskom-get-string 'reprioritize-to)
                   t))
         (inhibit-read-only t)
         (where 1)
         (elem nil))

    (if (not (eq default prio-from))
        (setq tmp nil))

    (if (and prio-from prio-to)
        (progn
          (while (<= where (length lyskom-prioritize-entry-list))
            (setq elem (lyskom-prioritize-get-entry-from-no where))
            (if (= (prioritize-entry->priority elem) prio-from)
                (progn
                  (set-prioritize-entry->priority elem prio-to)
                  (if (null tmp) (setq tmp elem))))
            (setq where (1+ where)))
          (lyskom-prioritize-sort-entries)
          (lyskom-prioritize-redraw-buffer)
          (lyskom-prioritize-goto-entry tmp)))))


(defun kom-prioritize-yank ()
  "Move all marked entries to before point."
  (interactive)
  (let* ((old-entry (lyskom-prioritize-current-entry))
        (entry-list (lyskom-prioritize-get-selected))
        (prio (prioritize-entry->priority old-entry))
        (from nil)
        (to nil)
        (start (car entry-list)))
    (cond ((null entry-list) nil)
          (t
           (while entry-list
             (setq from (lyskom-prioritize-get-no-from-entry (car entry-list)))
             (setq to (lyskom-prioritize-get-no-from-entry 
                       (lyskom-prioritize-current-entry)))
             (if (< from to)
                 (setq to (1- to)))
             (set-prioritize-entry->priority (car entry-list) prio)
             (lyskom-prioritize-move-entry from to t t)
             (setq entry-list (cdr entry-list))
             (lyskom-prioritize-goto-entry old-entry))
           (lyskom-prioritize-goto-entry start)))))

(defun kom-prioritize-save ()
  "Save changes in the prioritization buffer."
  (interactive)
  (lyskom-prioritize-tell-server))

(defun kom-prioritize-quit ()
  "Quit from the prioritization mode."
  (interactive)
  (lyskom-prioritize-tell-server)
  (lyskom-undisplay-buffer))



;;; ======================================================================
;;; LysKOM User command
;;; LysKOM Prioritize mode and related functions
;;;


(defun kom-prioritize ()
  "Re-prioritize all conferences you are a member in.
Show memberships last visited, priority, unread and name in a buffer.
In that buffer you can use various commands to chande ordering and priorities 
of conferences you are a member of."
  (interactive)
  (lyskom-start-of-command 'kom-prioritize)
  (let* ((buffer (current-buffer))
         (tmp-buffer (lyskom-get-buffer-create 'prioritize
                                               (concat (buffer-name buffer)
                                                       "-prioritize")
                                               t))
         (collector (make-collector)))
    (unwind-protect
        (progn
          (if lyskom-membership-is-read
              nil
            (signal 'lyskom-internal-error
                    '(membership-isnt-read kom-prioritize)))
          (let ((pers-stat (blocking-do 'get-pers-stat lyskom-pers-no))
                (membership-list (blocking-do 'get-membership lyskom-pers-no)))
            (cond ((null membership-list)
                   (lyskom-insert (lyskom-get-string 'cannot-get-membership)))
                  ((null pers-stat)
                   (lyskom-insert (lyskom-get-string 'cannot-get-pers-stat)))
                  (t
                   (let* ((pers-no lyskom-pers-no)
                          (string (concat (lyskom-mode-name-from-host)
                                          " prioritize: "
                                          lyskom-server-name)))
                     (set-buffer tmp-buffer)
                     (make-local-variable 'lyskom-pers-no)
                     (make-local-variable 'lyskom-prioritize-entry-list)
                     (setq lyskom-prioritize-entry-list nil)
                     (setq lyskom-pers-no pers-no)

                     (setq mode-line-buffer-identification string)

                     (lyskom-prioritize-mode)
                     (set-buffer buffer)

                     (lyskom-traverse memb-ship membership-list
                                      (initiate-get-conf-stat
                                       'prioritize
                                       'lyskom-prioritize-handle-get-conf-stat
                                       (membership->conf-no memb-ship)
                                       collector))

                     (lyskom-wait-queue 'prioritize)
                     
                     (lyskom-save-excursion
                      (lyskom-display-buffer 
                       tmp-buffer)
                      (setq lyskom-prioritize-entry-list
                            (nreverse (collector->value
                                       collector)))
                      (lyskom-prioritize-sort-entries)
                      (lyskom-prioritize-redraw-buffer)
                      (goto-char (point-max))
                      (let ((inhibit-read-only t))
                        (insert "  "))
                      (lyskom-prioritize-goto-entry
                       (lyskom-prioritize-get-entry-from-no 1))))))))
      (save-excursion
        (set-buffer buffer)
        (lyskom-end-of-command)))))


(defun lyskom-prioritize-handle-get-conf-stat (conf-stat collector)
  (let ((tmp (make-prioritize-entry
              (membership->priority 
               (lyskom-get-membership 
                (conf-stat->conf-no 
                 conf-stat)))
              conf-stat)))
    (collector-push tmp collector)))


(defun lyskom-prioritize-sort-entries ()
  "Sort the prioritization entry list."
  (setq lyskom-prioritize-entry-list
        (sort lyskom-prioritize-entry-list
              (function (lambda (x y)
                          (> (prioritize-entry->priority x)
                             (prioritize-entry->priority y))))))
  (save-excursion
    (set-buffer lyskom-buffer)
    (setq lyskom-membership (sort lyskom-membership
                                  'lyskom-membership-<))))



(defun lyskom-prioritize-mode ()
  "\\<lyskom-prioritize-mode-map>Mode for prioritizing conferences in LysKOM.

Commands:
\\[kom-prioritize-move-up]\tMove conference on current line up one line.
\\[kom-prioritize-move-down]\tMove conference on current line down one line.
\\[kom-prioritize-yank]\tMove all selected conferences to near the current line.
\\[kom-prioritize-select]\tToggle selection of the conference on the current line.
\\[kom-prioritize-goto-priority]\tMove cursor to an entry with a certain priority.
\\[kom-prioritize-set-priority]\tAlter the priority of the selected conferences.
\\[kom-prioritize-reprioritize]\tChange one priority to another.
\\[kom-prioritize-save]\tSave changes to priorities.
\\[kom-prioritize-quit]\tSave changes and return to LysKOM.

All bindings:
\\{lyskom-prioritize-mode-map}
Entry to this mode runs lyskom-prioritize-mode-hook."
  (interactive)
  (setq major-mode 'lyskom-prioritize-mode)
  (setq mode-name "Prioritize")
  (make-local-variable 'lyskom-prioritize-entry-list)
  (make-local-variable 'lyskom-prioritize-mode-line-selected)
  (make-local-variable 'lyskom-prioritize-selection)
  (setq lyskom-prioritize-mode-line-selected "")
  (setq lyskom-prioritize-selection nil)
  (setq lyskom-prioritize-entry-list nil)
  (setq mode-line-format lyskom-prioritize-mode-line)
  (lyskom-prioritize-update-mode-line)
  (setq buffer-read-only t)
  (lyskom-use-local-map lyskom-prioritize-mode-map)
  (lyskom-add-hook 'lyskom-add-membership-hook
                   'lyskom-prioritize-add-membership
                   t)
  (lyskom-add-hook 'lyskom-remove-membership-hook 
                   'lyskom-prioritize-remove-membership
                   t)
  (lyskom-add-hook 'lyskom-replace-membership-hook 
                   'lyskom-prioritize-replace-membership
                   t)
  (run-hooks 'lyskom-prioritize-mode-hook))



(defun lyskom-prioritize-update-mode-line ()
  (setq lyskom-prioritize-mode-line-selected
        (cond ((= (length (lyskom-prioritize-get-selected)) 0)
               (lyskom-get-string 'no-selection))
               (t (format (lyskom-get-string 'selection)
                          (length (lyskom-prioritize-get-selected))))))
  (force-mode-line-update))




;;; ================================================================
;;; Saving changes
;;;

(defun lyskom-prioritize-tell-server (&optional entry)
  "Tell the server about the changes. If optional arg ENTRY is given, 
only tell server about that entry." 
  (cond ((null entry)
         (mapcar (function 
                  (lambda (x)
                    (if x
                        (lyskom-prioritize-tell-server x))))
                 lyskom-prioritize-entry-list))
        
        (t (let* ((conf-stat (prioritize-entry->conf-stat entry))
                  (conf-no (conf-stat->conf-no conf-stat))
                  (entry-number 
                   (1- (lyskom-prioritize-get-no-from-entry entry))))
             (save-excursion
               (set-buffer lyskom-buffer)
               (set-membership->priority 
                (lyskom-get-membership conf-no)
                (prioritize-entry->priority entry))
               (initiate-add-member 'priority
                                    'lyskom-prioritize-handler
                                    conf-no
                                    lyskom-pers-no
                                    (prioritize-entry->priority entry)
                                    entry-number))))))
                 
      

(defun lyskom-prioritize-handler (res)
  "Arg: RES. Barf if RES is nil."
  (or res (lyskom-error "%s" (lyskom-get-string 'prio-died))))
