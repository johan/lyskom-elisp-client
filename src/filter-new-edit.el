;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: filter-new-edit.el,v 44.3 2002-02-24 20:23:27 joel Exp $
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
;;;; File: filter-new-edit.el
;;;;
;;;; Filter editor for new style filters.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: filter-new-edit.el,v 44.3 2002-02-24 20:23:27 joel Exp $\n"))



(defvar lyskom-filter-edit-all-entries nil
  "List of all filter edit entries in a filter edit buffer.")

(defvar lyskom-filter-edit-start-marker nil)
(defvar lyskom-filter-edit-end-marker nil)


;;; ============================================================
;;; Datatypes

(def-komtype filter-edit-entry
  name
  permanent
  action
  start
  end
  header-end
  collapsed
  pattern
  tree
  )

(defun lyskom-filter-edit-entry-from-filter (filter)
  )

;;; ============================================================
;;; Transforming to and from the tree editor
;;;

(defun lyskom-filter-edit-create-tree-data (pattern &optional negated)
  "Create a list representation of filter pattern PATTERN.
The list is suitable as input to tree-edit-compile-tree."
  (cond ((null pattern) nil)
        (t (let* ((pred (lyskom-filter-get-predicate (car pattern)))
                  (type (and pred (lyskom-filter-get-datatype 
                                   (filter-predicate->datatype pred))))
                  (print-function
                   (and type (filter-datatype->print-function type))))
             (if (and (eq (filter-datatype->name type) 'negation)
                      (null (lyskom-filter-predicate-comment pattern))
                      (eq 1 (length (lyskom-filter-predicate-data pattern))))
                 (lyskom-filter-edit-create-tree-data (car (lyskom-filter-predicate-data pattern))
                                               (not negated))
               (let ((string 
                      (cond ((filter-datatype->is-function type)
                             (if print-function
                                 (funcall print-function pattern negated)
                               (format "%S" (car pattern))))
                            (t (lyskom-format "%#1S %#2s"
                                              (car pattern)
                                              (if print-function
                                                  (funcall print-function
                                                           pattern
                                                           negated)
                                                ""))))))
                 (when (lyskom-filter-predicate-comment pattern)
                   (setq string
                         (concat string "\""
                                 (lyskom-filter-predicate-comment pattern)
                                 "\"")))

               (list
                ;; String representation
                string
                ;; Attributes
                (unless (filter-datatype->is-function type)
                  (list 'leaf t))
                ;; Data
                (vector negated
                        (car pattern)
                        (lyskom-filter-predicate-comment pattern)
                        (unless (filter-datatype->is-function type)
                          (lyskom-filter-predicate-data pattern)))
                ;; Subtrees
                (if (filter-datatype->is-function type)
                    (mapcar (lambda (x) 
                              (lyskom-filter-edit-create-tree-data x))
                            (lyskom-filter-predicate-data pattern))
                  nil))))))))

(defun lyskom-filter-edit-extract-tree-data (tree)
  "Extract a filter pattern from the tree TREE."
  (lyskom-filter-edit-transform-extracted-data (tree-edit-collect-data tree)))

(defun lyskom-filter-edit-transform-extracted-data (pattern)
  "Create a filter pattern from data in PATTERN collected from a tree."
  (cond ((null pattern) nil)
        (t (let* ((node (if (vectorp pattern) pattern (car pattern)))
                  (negated (aref node 0))
                  (pred (aref node 1))
                  (comment (aref node 2))
                  (args (aref node 3))
                  (type (lyskom-filter-get-datatype 
                         (filter-predicate->datatype
                          (lyskom-filter-get-predicate pred))))
                  (form
                   `(,pred ,comment ,@(if (filter-datatype->is-function type)
                                          (mapcar 'lyskom-filter-edit-transform-extracted-data
                                                  (cdr pattern))
                                        args))))
             (if negated
                 `(not nil ,form)
               form)))))



;;; ============================================================
;;; Drawing entries in the filter editor

(defun lyskom-filter-edit-draw-header (entry)
  "Insert the header of filter ENTRY at point."
    (insert (format "-- %s --" (or (filter-edit-entry->name entry)
                                   "Unnamed filter")))
    (if (filter-edit-entry->collapsed entry)
        (insert " [...]\n")
      (insert "\n")
      (insert (lyskom-format "Filter type:  %#1s\n" 
                             (filter-edit-entry->action entry)))
      (insert (lyskom-format "Active until: %#1s\n"
                             (if (filter-edit-entry->permanent entry)
                                 "There's ice-skating in hell"
                               "End of current session"))))
    (set-filter-edit-entry->header-end entry (point-marker))
    (set-marker-insertion-type (filter-edit-entry->header-end entry) nil))


(defun lyskom-filter-edit-draw-entry (entry)
  "Insert text representation of ENTRY at point in the current buffer"
  (let ((start (point-marker)))
    (set-marker-insertion-type start nil)
    (lyskom-filter-edit-draw-header entry)
    (unless (filter-edit-entry->collapsed entry)
      (unless (filter-edit-entry->tree entry)
        (set-filter-edit-entry->tree  
         entry
         (tree-edit-compile-tree 
          (lyskom-filter-edit-create-tree-data
           (filter-edit-entry->pattern entry)))))
      (tree-edit-draw (filter-edit-entry->tree entry))
      (insert "----------------------------------------\n"))
    (set-filter-edit-entry->end entry (point-marker))
    (set-filter-edit-entry->start entry start)
    (set-marker-insertion-type (filter-edit-entry->end entry) t)))

(defun lyskom-filter-edit-delete-entry (entry)
  (save-excursion
    (delete-region (filter-edit-entry->start entry)
                   (filter-edit-entry->end entry))
    (set-filter-edit-entry->start nil)
    (set-filter-edit-entry->end nil))

(defun lyskom-filter-edit-redraw-header (entry)
  (save-excursion
    (let ((end (filter-edit-entry->header-end entry)))
      (goto-char (filter-edit-entry->start entry))
      (lyskom-filter-edit-draw-header entry)
      (delete-region (point) end))


;;; ============================================================
;;; Utility functions

(defun lyskom-filter-edit-all-entries ()
  "Return a list of all filter edit entries in the current buffer"
  lyskom-filter-edit-entry-list))

(defun lyskom-filter-edit-set-entries (entries)
  "Set the list of filter edit entries in buffer to ENTRIES.
Do not redraw anything"
  (setq lyskom-filter-edit-entry-list entries))

(defun lyskom-filter-edit-entry-at (pos)
  "Return the filter entry at POS"
  (lyskom-traverse entry (lyskom-filter-edit-all-entries)
    (cond ((and (<= (filter-edit-entry->start entry) pos)
                (>= (filter-edit-entry->end entry)) pos)
           (lyskom-traverse-break entry))
          ((<= (filter-edit-entry->start entry) pos)
           (lyskom-traverse-break nil)))))

(defun lyskom-filter-edit-entry-position (entry)
  "Reuturn the numeric position of filter edit entry ENTRY."
  (- (length (lyskom-filter-edit-all-entries))
     (length (memq entry (lyskom-filter-edit-all-entries)))))

(defun lyskom-filter-edit-entry-in-header (pos entry)
  "Return non-nil if POS is in the header area of ENTRY."
  (and (>= pos (filter-edit-entry->start entry))
       (< pos (filter-edit-entry->header-end entry))))

(defun lyskom-filter-edit-entry-mode (entry to)
  "Move filter entry ENTRY to before entry TO.
If TO is nil, move to end of list."
  (lyskom-filter-edit-delete-entry entry)
  (if to
      (goto-char (filter-edit-entry->start to))
    (goto-char (point-max)))
  (lyskom-filter-edit-draw-entry entry)
  (lyskom-filter-edit-set-entries 
   (lyskom-move-in-list entry 
                        (lyskom-filter-edit-all-entries)
                        to)


;;; ============================================================
;;; Interactive commands

(defun lyskom-filter-edit-collapse-entry ()
  "Collapse both filters and lines"
  )

(defun lyskom-filter-edit-expand-entry ()
  "Collapse both filters and lines"
  )

(defun lyskom-filter-edit-kill ()
  "Kill both filters and lines in trees"
  )

(defun lyskom-filter-edit-yank ()
  "Yank both filters and lines in trees"
  )

(defun lyskom-filter-edit-move-up (arg)
  "Move filters and lines up"
  (interactive "p")
  )

(defun lyskom-filter-edit-move-down (arg)
  "Move filters and lines down"
  (interactive "p")
  )

(defun lyskom-filter-edit-previous-line (arg)
  "Move cursor up ARG lines"
  (interactive "p")
  )

(defun lyskom-filter-edit-next-line (arg)
  "Move cursor down ARG lines"
  (interactive "p")
  )

(defun lyskom-filter-edit-set-action ()
  )

(defun lyskom-filter-edit-set-permanent ()
  )

(defun lyskom-filter-edit-set-name ()
  )

(defun lyskom-filter-edit-set-predicate-comment ()
  )

(defun lyskom-filter-edit-negate-predicate ()
  )

(defun lyskom-filter-edit-add-predicate ()
  )

(defun lyskom-filter-edit-save-and-quit ()
  )

(defun lyskom-filter-edit-save ()
  )

(defun lyskom-filter-edit-quit ()
  )


(defun lyskom-filter-edit-mode ()
  "\\<lyskom-filter-edit-mode-map>Mode for editing LysKOM filters.

All bindings:
\\{lyskom-filter-edit-mode-map}
Entry to this mode runs lyskom-filter-edit-mode-hook."
  (interactive)
  (setq major-mode 'lyskom-filter-edit-mode)
  (setq mode-name "Filter Edit")
  (make-local-variable 'lyskom-filter-edit-all-entries)
  (make-local-variable 'lyskom-filter-edit-start-marker)
  (make-local-variable 'lyskom-filter-edit-end-marker)
  (setq buffer-read-only t)
  (run-hooks 'lyskom-filter-edit-mode-hook))

(defun lyskom-filter-edit (lyskom-buffer)
  "Edit filters for LysKOM buffer lyskom-buffer."
  (let (edit-buffer)
    (save-excursion
      (set-buffer lyskom-buffer)
      (setq edit-buffer
            (lyskom-get-buffer-create 'filter-edit
                                      (concat (buffer-name) "-Filter Edit")
                                      t)))
    (set-buffer edit-buffer)
    (lyskom-filter-edit-mode)
    (lyskom-filter-edit-set-entries
     (mapcar 'lyskom-filter-edit-entry-from-filter (lyskom-default-value 
                                                    'lyskom-filter-list)))
    (lyskom-filter-edit-update-buffer)))

(defun lyskom-filter-edit-update-buffer ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (lyskom-format-insert "\
Filter för %#1M på %#2s

===============================================================================
" lyskom-pers-no lyskom-server-name)
    (setq lyskom-filter-edit-start-marker (point-marker))
    (set-marker-insertion-type lyskom-filter-edit-start-marker nil)
    (goto-char (point-max))
    (let ((entries (lyskom-filter-edit-all-entries)))
      (while entries
        (lyskom-filter-edit-draw-entry (car entries))
        (insert "\n")
        (setq entries (cdr entries))))
    (setq lyskom-filter-edit-end-marker (point-marker))
    (set-marker-insertion-type lyskom-filter-edit-end-marker t)
    (insert "\
===============================================================================
Nytt filter: M-i    Flytta upp:  M-p    Flytta ut:  M-f 
Ny rad:      i      Flytta ner:  M-n    Flytta in:  M-b
Klipp ut:    C-k    Ändra:       RET    Mer hjälp:  C-h m
Klistra in:  C-y    Kommentar:   k      Avsluta:    C-c C-c
")
    ))



;;; Local Variables:
;;; mode: lisp-interaction
;;; end:

;;; (setq foo (lyskom-filter-edit-create-tree-data
;;;  '(and nil
;;;        (or nil
;;;            (recipient nil "Test")
;;;            (recipient nil "Tset"))
;;;        (not nil
;;;             (or nil
;;;                 (subject nil "Foo")
;;;                 (and nil
;;;                      (text nil "Fjuk")
;;;                      (recipient nil "Fjuk"))))
;;;        (not nil (author-re nil "[Dd]x")))))
;;; 
;;; (lyskom-filter-edit-extract-tree-data tmp)

