;;;;; -*-unibyte: t;-*-
;;;;;
;;;;; $Id: filter-edit.el,v 44.4.2.1 1999-10-13 09:55:58 byers Exp $
;;;;; Copyright (C) 1994, 1996  Lysator Academic Computer Association.
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
;;;;============================================================
;;;;============================================================
;;;;
;;;; File: filter-edit.el
;;;;
;;;; This file contains the filter editor
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: filter-edit.el,v 44.4.2.1 1999-10-13 09:55:58 byers Exp $\n"))


(defvar filter-edit-currently-edited-filter-entry-list nil
  "List of filters currently being edited in a filter editor")
(defvar filter-edit-change-flag nil)
(defvar filter-edit-filter-list nil)
(defvar filter-edit-list-start nil)
(defvar filter-edit-list-end nil)

(defun copy-filter-list (l)
  "Copy the filter list L"
  (cond ((null l) nil)
        (t (cons (copy-filter (car l))
                 (copy-filter-list (cdr l))))))

(defun lyskom-reverse-pairs (l)
  "Reverse the pairs in the assoc list L"
  (mapcar (function
           (lambda (e) (cons (cdr e) (car e)))) l))

;;;========================================
;;; Data types
;;;
;;; filter-entry
;;; Entry in the filter-entry list
;;;
;;; A filter entry is a vector consisting of
;;;
;;; [START END LINES PATTERN]
;;;
;;; START is the starting position (mark) in the buffer
;;; END is the ending position (mark) in the buffer
;;; LINES is a list of starting positions (marks) of the lines
;;; PATTERN is the filter displayed (type filter)
;;;

(defun filter-entry->start (e)
  "Get starting position of entry E."
  (aref e 0))

(defun set-filter-entry->start (e s)
  "Set starting position of entry E to S."
  (aset e 0 s))

(defun filter-entry->end (e)
  "Get end position of entry E."
  (aref e 1))

(defun set-filter-entry->end (e s)
  "Set end position of entry E to S."
  (aset e 1 s))

(defun filter-entry->lines (e)
  "Get line start list of entry E."
  (aref e 2))

(defun set-filter-entry->lines (e p)
  "Set line start list of entry E to P."
  (aset e 2 p))

(defun filter-entry->filter (e)
  "Get filter of entry E."
  (aref e 3))

(defun set-filter-entry->filter (e p)
  "Set filter of entry E to P."
  (aset e 3 p))

(defun make-filter-entry (start end lines pattern)
  "Create an filter entry with START, END, LINES and PATTERN as values."
  (vector start end lines pattern))



;;;============================================================
;;; Verification functions
;;;

(defun lyskom-verify-filter-list (filter)
  "Return non-nil if FILTER is a valid filter list"
  (cond ((null filter) t)
        ((not (listp filter)) nil)
        ((not (filter-p (car filter))) nil)
        (t (and (lyskom-verify-filter-pattern 
                 (filter->pattern (car filter)))
                (lyskom-verify-filter-list (cdr filter))))))

(defun lyskom-verify-filter-pattern (filter)
  "Return non-nil if FILTER is a valid filter pattern"
  (cond ((null filter) nil)
        ((and (= (length filter) 1)
              (symbolp (car filter))) t)
        ((not (listp (car filter))) nil)
        ((eq 'not (car (car filter)))
         (lyskom-verify-filter-pattern
          (cons (cdr (car filter)) (cdr filter))))
        ((or (eq 'author-re (car (car filter)))
             (eq 'recipient-re (car (car filter)))
             (eq 'subject-re (car (car filter)))
             (eq 'text-re (car (car filter))))
         (and
          (lyskom-filter-verify-regexp (cdr (car filter)))
          (lyskom-verify-filter-pattern (cdr filter))))
         
        ((or (eq 'author (car (car filter)))
             (eq 'subject (car (car filter)))
             (eq 'recipient (car (car filter)))
             (eq 'text (car (car filter))))
         (and
          (stringp (cdr (car filter)))
          (lyskom-verify-filter-pattern (cdr filter))))
        ((or (eq 'author-no (car (car filter)))
             (eq 'recipient-no (car (car filter))))
         (and
          (integerp (cdr (car filter)))
          (lyskom-verify-filter-pattern (cdr filter))))
        (t nil)))

(defun lyskom-filter-verify-regexp (re)
  "Return t if RE is a valid regexp."
  (condition-case nil
      (progn
        (string-match re "teststring")
        t)
    (error nil)))

;;;========================================
;;; Locator functions
;;;

(defun lyskom-filter-edit-lineno (where entry)
  "Return the line number at WHERE in the entry ENTRY.
-1 means the header. 0 is the first line. Any number higher than the number of lines means outside the pattern."
  (let* ((lines (append (filter-entry->lines entry)
                        (cons (filter-entry->end entry) nil)))
         (line -1)
         (current-line (length lines)))
    (if (>= where (filter-entry->start entry))
        (while lines
          (if (< where (car lines))
              (progn
                (setq current-line line)
                (setq lines nil))
            (progn
              (setq lines (cdr lines))
              (setq line (1+ line))))))
    current-line))


(defun lyskom-filter-edit-locate (where)
  "Locate the entry at character position WHERE.
nil means WHERE is not in any entry."
  (let ((entry-no (lyskom-filter-edit-locate-no where)))
    (if (= -1 entry-no)
        nil
      (elt filter-edit-currently-edited-filter-entry-list entry-no))))


(defun lyskom-filter-edit-locate-no (where)
  "Find the filter-edit-currently-edited-filter-entry-list index of the entry
covering the character position WHERE. -1 means WHERE is not covered y any
entry."
  (let ((i 0)
        (entry-no -1)
        (l filter-edit-currently-edited-filter-entry-list))
    (while l
      (if (and (>= where (filter-entry->start (car l)))
               (<= where (filter-entry->end (car l))))
          (progn (setq entry-no i)
                 (setq l nil))
        (progn (setq i (1+ i))
               (setq l (cdr l)))))
    entry-no))


;;;========================================
;;; Formatting functions
;;;

(defun lyskom-format-filter-list (filters)
  "Format the filter list FILTERS and insert the result into the
current buffer. The buffer variable 
FILTER-EDIT-CURRENTLY-EDITED-FILTER-ENTRY-LIST is also updated."
  (let ((inhibit-read-only t))
    (setq filter-edit-currently-edited-filter-entry-list nil)
    (lyskom-format-filter-list-2 filters)
    (setq filter-edit-currently-edited-filter-entry-list
	  (nreverse filter-edit-currently-edited-filter-entry-list))))


(defun lyskom-format-filter-list-2 (filters)
  "Format the filter list FILTERS and insert the result into the
current buffer."
  (cond ((null filters) nil)
        (t (setq filter-edit-currently-edited-filter-entry-list
                 (cons (lyskom-format-filter-pattern (car filters))
                       filter-edit-currently-edited-filter-entry-list))
           (lyskom-format-filter-list-2 (cdr filters)))))

  

(defun lyskom-format-filter-pattern (pat)
  "Format and insert the filter pattern PAT into the current buffer.
Returns an filter-entry structure representing the entry."
  (let ((inhibit-read-only t)
        start end lines)
   
    ;;
    ;; Insert text representation
    ;;

    (setq start (point-marker))
    (lyskom-filter-format-entry-header pat t)
    (setq lines (lyskom-format-filter-pattern-2 (filter->pattern pat) nil))
    (setq end (point-marker))
    (insert "\n")
    (make-filter-entry start end lines pat)))


(defun lyskom-format-filter-pattern-2 (pat lines &optional neg)
  "Format and insert the body of the filter pattern PAT into the 
current buffer. Accumulate line starts in LINES.
If NEG is non-nil, the first line will be negated."
  (cond ((or (null pat)) (nreverse lines))
        ((eq (car (car pat)) 'not)
         (lyskom-format-filter-pattern-2 (cons (cdr (car pat))
                                               (cdr pat))
                                         lines
                                         t))
        (t
         (setq lines (cons (point-marker) lines))
         (lyskom-format-filter-pattern-insert pat neg)
         (lyskom-format-filter-pattern-2 (cdr pat) lines))))



(defun lyskom-format-filter-pattern-insert (pat neg)
  "Format and insert the first pattern in PAT.
If NEG is non-nil, format the negation."
  (let (tmp)
    (insert
     (format "    %s %s %S"
	     (cdr (assoc (car (car pat)) lyskom-filter-what))
	     (if neg
		 " != "
	       " = ")
	     (cdr (car pat))))
    (cond ((or (eq (car (car pat)) 'recipient-no)
	       (eq (car (car pat)) 'author-no))
	   (save-excursion
	     (if (boundp 'lyskom-buffer)
		 (set-buffer lyskom-buffer))
	     (setq tmp (blocking-do 'get-conf-stat (cdr (car pat)))))
	   (insert (format "   <%s>" (conf-stat->name tmp)))))
    (insert "\n")))
	


(defun lyskom-filter-format-entry-header (filter &optional newline action)
  "Format the header of a filter entry. FILTER is the filter to format. 
If NEWLINE is non-nil, insert a newline after the header."
  (let (permanent)
    (setq action (filter->attribute filter 'action)
	  permanent (null (filter->attribute filter 'expire)))
    (insert 
     (format "--- %s %s"
	     (cdr 
	      (assq action lyskom-filter-actions		    ))
	     (lyskom-get-string
	      (if permanent 'permanent 'temporary))))
    (if newline (insert "\n"))))


;;;========================================
;;; User commands
;;;

(defun lyskom-filter-edit-next-pattern (arg)
  "Move to the ARGth next pattern line in the current buffer."
  (interactive "p")
  (if (null arg) (setq arg 1))
  (if (< arg 0) 
      (lyskom-filter-edit-prev-entry (- arg))
    (while (/= arg 0)
      (let ((entry-no (lyskom-filter-edit-locate-no (point)))
            tmp)
        (if (= -1 entry-no)
            (if (car filter-edit-currently-edited-filter-entry-list)
                (goto-char 
		 (filter-entry->start 
		  (car filter-edit-currently-edited-filter-entry-list)))
              (lyskom-error (lyskom-get-string 'filter-edit-empty-list)))
          (let* ((entry 
		  (elt filter-edit-currently-edited-filter-entry-list
		       entry-no))
                 (lineno (lyskom-filter-edit-lineno (point) entry)))
            (cond 
             ((= lineno -1)
              (if (filter-entry->lines entry)
                  (setq tmp (car (filter-entry->lines entry)))
                (progn
                  (setq tmp 
                        (elt filter-edit-currently-edited-filter-entry-list
			     (1+ entry-no)))
                  (if tmp 
                      (setq tmp (filter-entry->start tmp))
                    (lyskom-error (lyskom-get-string 'filter-edit-end-of-list))))))

              ((= lineno (1- (length (filter-entry->lines entry))))
               (setq tmp (elt filter-edit-currently-edited-filter-entry-list
			      (1+ entry-no)))
               (if tmp 
                   (setq tmp (filter-entry->start tmp))
                 (lyskom-error (lyskom-get-string 'filter-edit-end-of-list))))
              (t (setq tmp (elt (filter-entry->lines entry) (1+ lineno)))))
        (goto-char tmp)
        (setq arg (1- arg))))))))


(defun lyskom-filter-edit-prev-pattern (arg)
  "Move to the ARGth previous pattern line in the current buffer."
  (interactive "p")
  (if (null arg) (setq arg 1))
  (if (< arg 0) 
      (lyskom-filter-edit-next-entry (- arg))
    (while (/= arg 0)
      (let ((entry-no (lyskom-filter-edit-locate-no (point)))
            tmp)
        (if (= -1 entry-no)
            (if (car filter-edit-currently-edited-filter-entry-list)
                (goto-char
		 (filter-entry->start
		  (car filter-edit-currently-edited-filter-entry-list)))
              (lyskom-error (lyskom-get-string 'filter-edit-empty-list)))
          (let* ((entry 
		  (elt filter-edit-currently-edited-filter-entry-list
		       entry-no))
                 (lineno (lyskom-filter-edit-lineno (point) entry)))
            (cond 
             ((= lineno -1)
              (if (= 0 entry-no)
                  (lyskom-error (lyskom-get-string 'filter-edit-start-of-list))
                (progn
                  (setq entry 
			(elt filter-edit-currently-edited-filter-entry-list
			     (1- entry-no)))
                  (setq tmp (car 
                             (nthcdr (1- (length (filter-entry->lines entry)))
                                     (filter-entry->lines entry))))
                  (if (null tmp)
                      (setq tmp (filter-entry->start entry))))))

              ((= lineno 0)
               (setq tmp (filter-entry->start entry)))

              (t (setq tmp (elt (filter-entry->lines entry) (1- lineno)))))
        (goto-char tmp)
        (setq arg (1- arg))))))))



(defun lyskom-filter-edit-next-entry (arg)
  "Move to the ARGth next entry in the current buffer."
  (interactive "p")
  (if (null arg) (setq arg 1))
  (if (< arg 0) 
      (lyskom-filter-edit-prev-entry (- arg))
    (while (/= 0 arg)
      (let ((entry-no (lyskom-filter-edit-locate-no (point))))
        (if (= -1 entry-no)
            (if (car filter-edit-currently-edited-filter-entry-list)
                (goto-char 
		 (filter-entry->start
		  (car filter-edit-currently-edited-filter-entry-list)))
              (lyskom-error (lyskom-get-string 'filter-edit-empty-list)))
          (let ((tmp (elt filter-edit-currently-edited-filter-entry-list (1+ entry-no))))
            (if tmp
                (progn
                  (goto-char (filter-entry->start tmp))
                  (setq arg (1- arg)))
              (lyskom-error (lyskom-get-string 'filter-edit-end-of-list)))))))))


(defun lyskom-filter-edit-prev-entry (arg &optional noerror)
  "Move to the ARGth previous entry in the current buffer."
  (interactive "p")
  (not
   (catch 'fail
     (if (null arg) (setq arg 1))
     (if (< arg 0) 
         (lyskom-filter-edit-next-entry (- arg))
       (while (/= 0 arg)
         (let ((entry-no (lyskom-filter-edit-locate-no (point))))
           (if (= -1 entry-no)
               (if (car filter-edit-currently-edited-filter-entry-list)
                   (goto-char (filter-entry->start (car filter-edit-currently-edited-filter-entry-list)))
                 (if noerror (throw 'fail t) 
                   (lyskom-error (lyskom-get-string 'filter-edit-empty-list)))))
           (if (= 0 entry-no)
               (if noerror (throw 'fail t) 
                 (lyskom-error (lyskom-get-string 'filter-edit-start-of-list)))
             (let ((tmp (elt filter-edit-currently-edited-filter-entry-list (1- entry-no))))
               (setq arg (1- arg))
               (goto-char (filter-entry->start tmp))))))))))


(defun lyskom-filter-edit-beginning-of-list ()
  "Move to the first entry in the list"
  (interactive)
  (if (null filter-edit-currently-edited-filter-entry-list)
      (lyskom-error (lyskom-get-string 'filter-edit-empty-list))
    (progn
      (push-mark)
      (goto-char (filter-entry->start (car filter-edit-currently-edited-filter-entry-list))))))


(defun lyskom-filter-edit-end-of-list ()
  "Move to the last entry in the list"
  (interactive)
  (if (null filter-edit-currently-edited-filter-entry-list)
      (lyskom-error (lyskom-get-string 'filter-edit-empty-list))
    (let ((entry (elt filter-edit-currently-edited-filter-entry-list (1- (length filter-edit-currently-edited-filter-entry-list)))))
      (push-mark)
      (goto-char (or (elt (filter-entry->lines entry)
                          (1- (length (filter-entry->lines entry))))
                     (filter-entry->start entry))))))



(defun lyskom-filter-edit-insert-entry ()
  "Add an entry to the end of the list"
  (interactive)
  (let ((inhibit-read-only t)
        (completion-ignore-case t)
        (rev-actions 
         (lyskom-reverse-pairs lyskom-filter-actions))
        action permanent filter start end entry)
    (if filter-edit-currently-edited-filter-entry-list
        (progn
          (goto-char (filter-entry->end (elt filter-edit-currently-edited-filter-entry-list
                                             (1- (length filter-edit-currently-edited-filter-entry-list)))))
          (insert "\n"))
      (goto-char filter-edit-list-end))
      
      (setq action (completing-read (lyskom-get-string 'filter-edit-filter-how)
                                    rev-actions
                                    nil
                                    t))
      (setq permanent
            (lyskom-j-or-n-p (lyskom-get-string 'filter-permanent)))
      (setq filter (make-filter nil
                                (list (cons 'action 
                                            (cdr (assoc action rev-actions)))
                                      (cons 'expire (not permanent)))))
      (setq start (point-marker))
      (lyskom-filter-format-entry-header filter t)
      (setq end (point-marker))
      (setq entry
            (make-filter-entry start end nil filter))
      (if filter-edit-currently-edited-filter-entry-list
          (setcdr (nthcdr (1- (length filter-edit-currently-edited-filter-entry-list)) filter-edit-currently-edited-filter-entry-list)
                  (cons entry nil))
        (setq filter-edit-currently-edited-filter-entry-list (cons entry nil)))
      (setq filter-edit-change-flag t)
      (goto-char start)))


(defun lyskom-filter-edit-insert-pattern ()
  "Add a pattern line to the current list entry."
  (interactive)
  (let ((entry-no (lyskom-filter-edit-locate-no (point))))
    (if (= -1 entry-no) 
        (lyskom-error (lyskom-get-string 'filter-edit-outside-entry))))
  (let ((what nil)
        (pred nil)
        (arg nil)
        (argstring nil)
        (lineno nil)
        (entry nil)
        (filter nil)
        (pat nil)
        (inhibit-read-only t)
        (completion-ignore-case t)
        (rev-what (lyskom-reverse-pairs lyskom-filter-what)))
    (setq what (completing-read
                (lyskom-get-string 'filter-edit-filter-what)
                rev-what
                nil t))
    (setq pred 
          (completing-read (lyskom-format 'filter-edit-insert-pred what)
                           lyskom-filter-predicate-list
                           nil t))
    (setq argstring (read-from-minibuffer
                     (lyskom-format 'filter-edit-insert-arg what pred)))
    (setq what (cdr (assoc what rev-what)))

    (if (not
         (cond ((or (eq what 'author)
                    (eq what 'subject)
                    (eq what 'text)
                    (eq what 'recipient)) (setq arg argstring))
               ((or (eq what 'author-no)
                    (eq what 'recipient-no)) (setq arg
                                                   (string-to-int argstring)))
               ((or (eq what 'author-re) 
                    (eq what 'subject-re)
                    (eq what 'text-re)
                    (eq what 'recipient-re)) 
                (setq arg argstring)
                (lyskom-filter-verify-regexp arg))))
        (lyskom-error (lyskom-get-string 'filter-edit-bad-argument)
                       argstring))

    ;;
    ;; Build pattern
    ;;

    (setq pat 
          (cons what arg))
    (if (cdr (assoc pred lyskom-filter-predicate-list))
        (setq pat (cons 'not pat)))

    ;;
    ;; Locate current entry
    ;; Locate current line
    ;;
    
    (setq entry (lyskom-filter-edit-locate (point)))
    (setq lineno (lyskom-filter-edit-lineno (point) entry))
    (setq filter (filter-entry->filter entry))

    ;;
    ;; Splice the new pattern into the old pattern at the
    ;; proper position. If lineno is -1 (the cursor is not in
    ;; the pattern area, append the pattern.
    ;;

    (if (= -1 lineno)
        (setq lineno (length (filter->pattern filter))))
    
    (cond ((= lineno 0) 
           (set-filter->pattern
            filter
            (cons pat (filter->pattern filter))))
          (t (if (filter->pattern filter)
                 (setcdr (nthcdr (1- lineno)
                             (filter->pattern filter))
                     (cons pat (nthcdr lineno
                                       (filter->pattern filter))))
               (set-filter->pattern filter pat))))
    
    ;;
    ;; Update display
    ;;
    
    (if (= lineno (length (filter-entry->lines entry)))
        (goto-char (filter-entry->end entry))
      (goto-char (elt (filter-entry->lines entry) lineno)))
    
    ;;
    ;; OK, right now, POINT is on top os a marker that represents the
    ;; NEXT line. This marker must be puched forward to avoid duplicating
    ;; it.
    ;;
    
    (forward-line -1)
    (end-of-line)
    (insert "\n")
    (lyskom-format-filter-pattern-insert
     (cons
      (if (eq (car pat) 'not)
          (cdr pat) 
        pat) nil)
     (eq (car pat) 'not))
    (forward-char -1) 
    (delete-char 1)
    (beginning-of-line)
    
    ;;
    ;; Update lines list in the entry
    ;;

    (cond ((= lineno 0)
           (set-filter-entry->lines
            entry
            (cons (point-marker) (filter-entry->lines entry))))
          (t (setcdr (nthcdr (1- lineno)
                                 (filter-entry->lines entry))
                     (cons (point-marker)
                           (nthcdr lineno 
                                   (filter-entry->lines entry))))))
    (setq filter-edit-change-flag t)))



(defun lyskom-filter-edit-delete-pattern (arg)
  "Delete ARG pattern lines, starting with the one at point.
Only lines in the current entry will be deleted."
  (interactive "p")
  (let* ((inhibit-read-only t)
         (entry (lyskom-filter-edit-locate (point)))
         (filter (filter-entry->filter entry)))
    (if (null entry)
        (lyskom-error (lyskom-get-string 'filter-edit-outside-list))
      (progn
        (while (/= 0 arg)
          (let ((lineno (lyskom-filter-edit-lineno (point) entry)))
            (if (null arg) (setq arg 1))
            (if (and (> lineno -1)
                     (< lineno (length (filter-entry->lines entry))))
                (progn 
                  (setq arg (1- arg))
                  (setq lineno (lyskom-filter-edit-lineno (point) entry))
                  (delete-region
                   (save-excursion (beginning-of-line) (point))
                   (1+ (save-excursion (end-of-line) (point))))
                  (if (= lineno (1- (length (filter-entry->lines entry))))
                      (forward-line -1))
                  (if (= lineno 0)
                      (progn
                        (set-filter-entry->lines 
                         entry 
                         (cdr (filter-entry->lines entry)))
                        (set-filter->pattern 
                         filter
                         (cdr (filter->pattern filter)))
                        (setq filter-edit-change-flag t))
                    (progn
                      (setcdr (nthcdr (1- lineno) 
                                      (filter-entry->lines entry))
                              (nthcdr (1+ lineno)
                                      (filter-entry->lines entry)))
                      (setcdr (nthcdr (1- lineno)
                                      (filter->pattern
                                       (filter-entry->filter entry)))
                              (nthcdr (1+ lineno)
                                      (filter->pattern
                                       (filter-entry->filter entry))))
                      (setq filter-edit-change-flag t))))
              (lyskom-error (lyskom-get-string 'filter-edit-end-of-pattern)))))))))


(defun lyskom-filter-edit-delete-entry (arg &optional which noerror)
  "Delete ARG entries, starting with the one covering point.
If optional WHICH is non-nil, start with entry number WHICH.
If NOERROR is non-nil, return nil instead of signaling an error."
  (interactive "p")
  (let ((inhibit-read-only t))
  (not
   (catch 'fail
     (if (null arg) (setq arg 1))
     (while (/= 0 arg)
       (let* ((entry-no (or which
                            (lyskom-filter-edit-locate-no (point))))
              (entry nil))
         (if (= -1 entry-no) 
             (if noerror
                 (throw 'fail t)
               (lyskom-error (lyskom-get-string 'filter-edit-end-of-list))))
         (setq entry (elt filter-edit-currently-edited-filter-entry-list
			  entry-no))
         (delete-region (filter-entry->start entry)
                        (1+ (filter-entry->end entry)))
         (cond ((= 0 entry-no)
                (setq filter-edit-currently-edited-filter-entry-list
		      (cdr filter-edit-currently-edited-filter-entry-list))
                (setq filter-edit-change-flag t))
               (t (setcdr
		   (nthcdr (1- entry-no)
			   filter-edit-currently-edited-filter-entry-list)
		   (nthcdr (1+ entry-no)
			   filter-edit-currently-edited-filter-entry-list))
                  (setq filter-edit-change-flag t)))
         (if (= entry-no
		(length filter-edit-currently-edited-filter-entry-list))
             (progn (if (lyskom-filter-edit-prev-entry 1 t)
                        (setq arg 0)
                      (throw 'fail t)))
           (setq arg (1- arg)))))))))
  
(defun lyskom-filter-edit-quit ()
  "Quit filter edit mode and ask to save changes (if any)"
  (interactive)
  (let ((save nil))
    (if filter-edit-change-flag
        (setq save (lyskom-j-or-n-p 
                    (lyskom-get-string 'filter-edit-save-p))))
    (if save
        (lyskom-filter-edit-save))
    (set-window-configuration lyskom-edit-return-to-configuration)))

(defun lyskom-filter-edit-expunge ()
  "Delete all entries that are completely empty"
  (interactive)
  (let ((e filter-edit-currently-edited-filter-entry-list)
        (index 0))
    (while e
      (if (null (filter-entry->lines (car e)))
          (lyskom-filter-edit-delete-entry 1 index t)
        (setq index (1+ index)))
      (setq e (cdr e)))))

(defun lyskom-filter-empty-patterns-p ()
  "Return non-nil if the entry list contains empty entries."
  (let ((e filter-edit-currently-edited-filter-entry-list)
        (result nil))
    (while e
      (if (null (filter-entry->lines (car e)))
          (progn (setq e nil) (setq result t))
        (setq e (cdr e))))
    result))


(defun lyskom-filter-edit-save ()
  "Save changes in filter edit mode"
  (interactive)
  (if (and 
       (lyskom-filter-empty-patterns-p)
       (lyskom-j-or-n-p
        (lyskom-get-string 'filter-edit-remove-empty)))
      (lyskom-filter-edit-expunge))

  (let ((e filter-edit-currently-edited-filter-entry-list)
        (xpermanent-list nil)
        (xtemporary-list nil))
    (while e
      (set-filter->function 
       (filter-entry->filter (car e))
       (lyskom-create-compile-filter-function
	(filter->pattern (filter-entry->filter (car e)))))
      (set (if (filter->attribute (filter-entry->filter (car e))
                                  'expire)
               'xtemporary-list
             'xpermanent-list)
           (cons (filter-entry->filter (car e))
                 (if (filter->attribute (filter-entry->filter (car e))
                                        'expire)
                     xtemporary-list
                   xpermanent-list)))
      (setq e (cdr e)))
    (save-excursion
      (set-buffer lyskom-buffer)
      (setq lyskom-filter-list 
            (append
             (setq kom-permanent-filter-list (nreverse xpermanent-list))
             (setq kom-session-filter-list (nreverse xtemporary-list))))
      (setq filter-edit-change-flag nil)
      (lyskom-message "%s" (lyskom-get-string 'filter-edit-saving))
      (lyskom-save-options lyskom-buffer
                           (lyskom-get-string 'filter-edit-saving)
                           (lyskom-get-string 'filter-edit-saving-done)
                           (lyskom-get-string 'filter-edit-saving-error)))))


(defun lyskom-filter-edit-revert ()
  "Discard changes and restart editing"
  (interactive)
  (if (or (not filter-edit-change-flag)
          (and filter-edit-change-flag
               (lyskom-j-or-n-p (lyskom-get-string 'filter-edit-restart-p))))
      (let ((inhibit-read-only t))
        (setq filter-edit-currently-edited-filter-entry-list nil)
        (delete-region filter-edit-list-start filter-edit-list-end)
        (goto-char filter-edit-list-start)
        (insert "\n")
        (lyskom-format-filter-list (copy-filter-list lyskom-filter-list))
        (setq filter-edit-change-flag nil)
        (lyskom-filter-edit-beginning-of-list))))


(defun lyskom-filter-edit-toggle-permanent ()
  "Toggle the permanent flag of the current entry"
  (interactive)
  (let ((entry-no (lyskom-filter-edit-locate-no (point)))
        (inhibit-read-only t))
    (if (= -1 entry-no)
        (lyskom-error (lyskom-get-string 'filter-edit-outside-entry)))
    (let ((entry (elt filter-edit-currently-edited-filter-entry-list
		      entry-no)))
      (set-filter->attribute (filter-entry->filter entry)
                             'expire
                             (not (filter->attribute 
                                   (filter-entry->filter entry)
                                   'expire)))
      (setq filter-edit-change-flag t)
      (save-excursion
        (goto-char (filter-entry->start entry))
        (delete-region (point) (save-excursion (end-of-line) (point)))
        (lyskom-filter-format-entry-header (filter-entry->filter entry))))))


(defun lyskom-filter-edit-toggle-action ()
  "Toggle the filter action of the current entry."
  (interactive)
  (let ((entry-no (lyskom-filter-edit-locate-no (point)))
        (inhibit-read-only t))
    (if (= -1 entry-no)
        (lyskom-error (lyskom-get-string 'filter-edit-outside-entry)))
    (let* ((entry (elt filter-edit-currently-edited-filter-entry-list
		       entry-no))
           (action-list lyskom-filter-actions)
           (action (car
                    (cdr (memq 
                          (assq
                            (filter->attribute (filter-entry->filter entry)
                                               'action)
                           action-list)
                          action-list))))
           (new-action (or action (car action-list))))
      (setq filter-edit-change-flag t)
      (set-filter->attribute (filter-entry->filter entry)
                             'action
                             (car new-action))
      (save-excursion
        (goto-char (filter-entry->start entry))
        (delete-region (point) (save-excursion (end-of-line) (point)))
        (lyskom-filter-format-entry-header (filter-entry->filter entry))))))


(defun lyskom-filter-edit-brief-help ()
  "Display a help message in the minibuffer."
  (interactive)
  (lyskom-message "%s" (lyskom-get-string 'filter-edit-help)))


(defun lyskom-filter-edit-mode ()
  "\\<lyskom-filter-edit-map>Mode for editing LysKOM filters.
Entry to this mode runs lyskom-filter-edit-hook

Commands:
\\[lyskom-filter-edit-prev-pattern] Moves up in the pattern list.
\\[lyskom-filter-edit-next-pattern] Moves down in the pattern list.
\\[lyskom-filter-edit-prev-entry]   Moves up one filter in the list.
\\[lyskom-filter-edit-next-entry]   Moves down one filter in the list.
\\[lyskom-filter-edit-beginning-of-list]    Moves to the beginning of the list.
\\[lyskom-filter-edit-end-of-list]    Moves to the end of the list.
\\[lyskom-filter-edit-delete-pattern]   Deletes the pattern line at point.
\\[lyskom-filter-edit-delete-entry] Deletes the filter containing point.
\\[lyskom-filter-edit-insert-pattern]   Inserts a pattern line at point.
\\[lyskom-filter-edit-insert-entry] Inserts a new filter into the list.
\\[lyskom-filter-edit-expunge]  Deletes all empty filters from the list.
\\[lyskom-filter-edit-revert]   Discards all changes to the list.
\\[lyskom-filter-edit-toggle-action]    Toggles the filter action of the filter containing point.
\\[lyskom-filter-edit-toggle-permanent] Toggles between permanent and temporary.
\\[lyskom-filter-edit-save] Saves the filters.
\\[lyskom-filter-edit-quit] Quit and return to LysKOM.

All key bindings:
\\{lyskom-filter-edit-map}
"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'filter-edit-currently-edited-filter-entry-list)
  (make-local-variable 'filter-edit-change-flag)
  (make-local-variable 'filter-edit-filter-list)
  (make-local-variable 'filter-edit-list-start)
  (make-local-variable 'filter-edit-list-end)
  (make-local-variable 'lyskom-buffer)
  (make-local-variable 'lyskom-edit-return-to-configuration)
  (setq buffer-read-only t)
  (setq filter-edit-change-flag nil)
  (buffer-disable-undo (current-buffer))
  (lyskom-use-local-map lyskom-filter-edit-map)
  (setq mode-name "LysKOM Filter Edit")
  (setq major-mode 'lyskom-filter-edit-mode)
  (setq local-abbrev-table 'lyskom-filter-edit-abbrev-table)
  (run-hooks 'lyskom-filter-edit-hook))


;;;========================================
;;; LysKOM command to start editing
;;;

(defun kom-filter-edit ()
  (interactive)
  (let ((buf (current-buffer))
        (filters lyskom-filter-list)
        (server-name lyskom-server-name)
        (curwin (current-window-configuration)))
    (lyskom-display-buffer
     (lyskom-get-buffer-create 'edit-filters
                               (concat (buffer-name buf)
                                       "-"
                                       (lyskom-get-string 
                                        'filter-edit-buffer-name))
                               t))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (lyskom-filter-edit-mode)
    (setq lyskom-buffer buf)
    (let ((inhibit-read-only t))
      (setq filter-edit-filter-list filters)
      (insert (format (lyskom-get-string 'filter-edit-header)
                      server-name))
      (setq filter-edit-list-start (point-marker))
      (insert "\n")
      (lyskom-format-filter-list (copy-filter-list filter-edit-filter-list))
      (setq filter-edit-list-end (point-max-marker)))
    (setq lyskom-edit-return-to-configuration curwin)
    (lyskom-filter-edit-beginning-of-list)))


