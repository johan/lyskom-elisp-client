;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: filter.el,v 44.25 2003-01-09 00:43:26 byers Exp $
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
;;;; File: filter.el
;;;;
;;;; Contains the support functions for text filtering.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: filter.el,v 44.25 2003-01-09 00:43:26 byers Exp $\n"))

(eval-when-compile
  (require 'lyskom-command "command"))

;;;============================================================
;;;
;;; Filter pattern accessors
;;;
;;; Filters are lists with the following structure
;;;
;;;   (filter [ PATTERNS ATTRIBUTES FUNCTION])
;;;
;;; Where PATTERNS is a list of filter patterns and ATTRIBUTES is
;;; an association list. FUNCTION is a lisp function implementing
;;; the filter.
;;;
;;; The following attributes are reserved
;;;
;;; action -- What action to take when the pattern matches. Currently
;;;           one of dontshow, skip or skip-comments.
;;; expire -- When the filter expires. Not used.
;;;

(defun make-filter (&optional p a)
  "Creates and returns a filter structure.
Optional P and A initialize pattern and attributes, respectively."
  (lyskom-create-filter p a (lyskom-create-compile-filter-function p)))

(defun lyskom-create-filter (pattern attribute-list function)
  (list 'filter (vector pattern attribute-list function)))

(defun lyskom-recompile-filter (filter)
  "Re-compile the filter FILTER"
  (set-filter->function filter
                        (lyskom-create-compile-filter-function
                         (filter->pattern filter))))

(defun copy-filter (f)
  "Create a copy of the filter F"
  (make-filter (copy-tree (filter->pattern f))
               (copy-tree (filter->attribute-list f))))

(defun filter-p (f)
  "Returns T if f looks like a filter"
  (and (listp f)
       (eq 'filter (car f))))

(defun filter->pattern (f)
  "Extract the patterns part of a filter F"
  (elt (elt f 1) 0))

(defun filter->attribute-list (f)
  "Extract the attribute list of F."
  (elt (elt f 1) 1))

(defun filter->attribute (f a)
  "From filter F, extract the value of attribute A. 
Returns nil if no such attribute is present."
  (cdr (assq a (filter->attribute-list f))))

(defun set-filter->pattern (f p)
  "Set the patterns part of F to P."
  (aset (elt f 1) 0 p))

(defun set-filter->attribute-list (f l)
  "Set the attribute list of filter F to L"
  (aset (elt f 1) 1 l))

(defun set-filter->attribute (f a v)
  "Set the value in filter F of attribute A to V."
  (let ((x (assq a (filter->attribute-list f))))
    (if x
        (setcdr x v)
      (set-filter->attribute-list f
       (cons (cons a v) (filter->attribute-list f))))))

(defun filter->function (f)
  "Get the function for filter F"
  (elt (elt f 1) 2))

(defun set-filter->function (f fn)
  "Set the function for filter F to FN"
  (aset (elt f 1) 2 fn))


;;;============================================================
;;;
;;;


(defvar lyskom-filter-hack nil
  "Variable to busy-wait on to get the filter action. Set to
invalid-value until a filter action has been selected.")

(defun lyskom-filter-text-p (text-no)
  (if (null lyskom-filter-list)
      nil
    (progn
      (setq lyskom-filter-hack 'invalid-value)
      (initiate-get-text-stat 'filter 'lyskom-filter-text-p-2 text-no)
      ;;
      ;; Block until done
      ;;
      (while (eq lyskom-filter-hack 'invalid-value)
        (lyskom-accept-process-output))
      lyskom-filter-hack)))
    
(defun lyskom-filter-text-p-2 (text-stat)
  (if (null text-stat)
      (setq lyskom-filter-hack nil)
    (progn
      ;;
      ;; Collect information from the server
      ;;
      (lyskom-collect 'filter)
      
      ;;
      ;; Get the conf-stat of the author of the text
      ;; Get the text body
      ;;
      
      (initiate-get-conf-stat 'filter nil (text-stat->author text-stat))
      (initiate-get-text 'filter nil (text-stat->text-no text-stat))
      
      ;;
      ;; Get the conf-stat of the recipients
      ;;
      
      (lyskom-traverse
          misc
          (text-stat->misc-info-list text-stat)
        (let ((type (misc-info->type misc)))
          (if (memq type lyskom-recpt-types-list)
              (initiate-get-conf-stat 'filter 
                                      nil
                                      (misc-info->recipient-no misc)))))
      
      ;;
      ;; Use the results
      ;;
      
      (lyskom-use 'filter 'lyskom-filter-text-p-3 text-stat))))
    
(defun lyskom-filter-text-p-3 (author text &rest data)
  (if (or (null text)
	  (null author))
      (setq lyskom-filter-hack nil)
    (let (subject text-stat)
      
      ;;
      ;; Extract the text-stat which is the last element of data
      ;; Next shorten the list in data to exclude the text-stat.
      ;;
      
      (setq text-stat (elt data (- (length data) 1)))
      (if (= (length data) 1)
	  (setq data nil)
	(rplacd (nthcdr (- (length data) 2) data) nil))
      
      ;;
      ;; Extract the subject
      ;;
      
      (let ((str (text->decoded-text-mass text text-stat)))
        (cond ((string-match "\n" str)
               (setq subject (substring str 0 (match-beginning 0))))
              (t (setq subject ""))))
      
      ;;
      ;; Do the checking
      ;;
      
      (setq lyskom-filter-hack
	    (lyskom-check-filter-list text-stat
				      author
				      data
				      subject
                                      (text->decoded-text-mass text text-stat)
				      lyskom-filter-list)))))

(defun lyskom-check-filter-list (text-stat
                                 author
                                 recipient-list
                                 subject
                                 text
                                 filter-list)
  (let (tmp)
    (while filter-list
      (condition-case nil
          (if (functionp (filter->function (car filter-list)))
              (setq tmp (funcall (filter->function (car filter-list))
                                 (car filter-list)
                                 author
                                 recipient-list
                                 subject
                                 text-stat
                                 text)))
        (error nil))
      (if tmp
          (setq filter-list nil)
        (setq filter-list (cdr filter-list))))
    tmp))




;;;========================================
;;;  The filter compiler.
;;;


(defmacro lyskom-filter-is-member (testfn arg list selector)
  (` (let (found
           (objlist (, list)))
       (while (and objlist (not found))
         (and ((, testfn) (, arg) ((, selector) (car objlist)))
           (setq found t))
      (setq objlist (cdr objlist)))
    found)))


(defun lyskom-create-compile-filter-function (pattern)
  (if (null pattern)
      (byte-compile 
       '(lambda (filter author recipient-list subject text-stat text) nil))
    (byte-compile (lyskom-create-filter-function pattern))))


(defun lyskom-create-filter-function (pattern)
  (` (lambda (filter author recipient-list subject text-stat text)
       (, (cons 'and (lyskom-create-filter-function-body pattern))))))

(defun lyskom-create-filter-function-body (pattern)
  (let (inverse)
    (cond
     ;;
     ;; End of pattern
     ;; 
     
     ((null pattern) '((filter->attribute filter 'action)))

     ;;
     ;; Bad pattern
     ;;
     
     ((or (not (listp pattern))
          (not (listp (car pattern)))) 
      (lyskom-error 
       "%s"
       (lyskom-get-string 'filter-error-specification)))
     
     ;;
     ;; Assume valid pattern
     ;;
     
     (t
      (let ((key (car (car pattern)))
            (args (cdr (car pattern)))
            (form nil))
        (if (eq key 'not)
            (if (not (listp args))
                (lyskom-error "%s" (lyskom-get-string 'filter-error-bad-not))
              (setq key (car args) args (cdr args) inverse t)))

        (setq form
              (cond 
               ((eq key 'author)
                (lyskom-filter-check-args 'stringp args)
                (` (string-match (, (regexp-quote args))
                                 (conf-stat->name author))))

               ((eq key 'author-re)
                (lyskom-filter-check-args 'regexpp args)
                (` (string-match (, args)
                                 (conf-stat->name author))))

               ((eq key 'author-no)
                (lyskom-filter-check-args 'integerp args)
                (` (= (, args) (conf-stat->conf-no author))))

               ((eq key 'recipient)
                (lyskom-filter-check-args 'stringp args)
                (` (lyskom-filter-is-member
                    string-match
                    (, (regexp-quote args))
                    recipient-list
                    conf-stat->name)))
               ((eq key 'recipient-re)
                (lyskom-filter-check-args 'regexpp args)
                (` (lyskom-filter-is-member
                    string-match
                    (,  args)
                    recipient-list
                    conf-stat->name)))
               ((eq key 'recipient-no)
                (lyskom-filter-check-args 'integerp args)
                (` (lyskom-filter-is-member
                    =
                    (,  args)
                    recipient-list
                    conf-stat->conf-no)))

               ((eq key 'subject)
                (lyskom-filter-check-args 'stringp args)
                (` (string-match (, (regexp-quote args))
                                 subject)))
               ((eq key 'subject-re)
                (lyskom-filter-check-args 'regexpp args)
                (` (string-match (, args)
                                 subject)))

               ((eq key 'text)
                (lyskom-filter-check-args 'stringp args)
                (` (string-match (, (regexp-quote args))
                                 text)))
               ((eq key 'text-re)
                (lyskom-filter-check-args 'regexpp args)
                (` (string-match (, args)
                                 text)))
               (t (lyskom-error 
                   (lyskom-get-string 'filter-error-unknown-key)
                   key
                   ))))
        
        (if inverse (setq form (list 'not form)))
        (cons form (lyskom-create-filter-function-body (cdr pattern))))))))

      

(defun lyskom-filter-check-args (fn arg)
  (if (not (funcall fn arg))
      (lyskom-error (lyskom-get-string 'filter-error-key-arg)
                    fn arg)))




;;; ============================================================
;;; lyskom-filter-prompt
;;;
;;; Print a notice that a text has been filtered. 
;;;

(defun lyskom-filter-prompt (text-no prompt)
  (setq lyskom-filter-hack t)
  (let ((text-stat (blocking-do 'get-text-stat text-no))
	(subject nil))
    (if text-stat
	(blocking-do-multiple 
            ((text (get-text text-no))
             (conf-stat (get-conf-stat 
                         (text-stat->author text-stat))))
          (if text
              (let ((str (text->decoded-text-mass text text-stat)))
                (setq subject
                      (if (string-match "\n" str)
                          (substring str 0 (match-beginning 0))
                        str))
                (lyskom-format-insert prompt
                                      text-stat
                                      subject
                                      (or conf-stat
                                          (text-stat->author text-stat)))
                (lyskom-scroll))))))
  (setq lyskom-filter-hack nil))



;;;========================================
;;; User functions and support functions
;;;

(defun lyskom-add-filter (filter)
  "Add the filter FILTER to the LysKOM filtering mechanism."
  (if (filter->attribute filter 'expire)
      (setq kom-session-filter-list (cons filter
                                          kom-session-filter-list))
    (progn
      (setq kom-permanent-filter-list (cons filter kom-permanent-filter-list))
      (lyskom-save-options (current-buffer)
                           (lyskom-get-string 'filter-edit-saving)
                           (lyskom-get-string 'filter-edit-saving-done)
                           (lyskom-get-string 'filter-edit-saving-error))))
  (setq lyskom-filter-list
        (cons filter lyskom-filter-list)))


(defun lyskom-filter-read-action ()
  "Read a filter action from the minibuffer, returning its symbol"
  (let ((completion-ignore-case t))
    (car
     (rassoc
      (lyskom-completing-read (lyskom-get-string 'filter-action)
                              (lyskom-maybe-frob-completion-table
                               (lyskom-reverse-pairs 
                                lyskom-filter-actions))
                              nil
                              nil
                              (cdr (car lyskom-filter-actions))
                              t)
      lyskom-filter-actions))))


(defun lyskom-filter-read-permanent ()
  "Ask the user is a filter is permanent and return t in this case.
Otherwise return nil."
  (lyskom-j-or-n-p (lyskom-get-string 'filter-permanent)))



;;;========================================
;;; Filtrera ärende --- Filter subject
;;; 

(def-kom-command kom-filter-subject (&optional subject)
  "Interactively filter a subject. This creates a permanent or temporary
filter for a single subject in one conference or all conferences.

An alternative to this is `kom-super-jump'.

To change existing filters, use `kom-filter-edit'."
  (interactive)
  (if (/= 0 lyskom-current-conf)
	  (let ((conf-stat (blocking-do 'get-conf-stat lyskom-current-conf)))
	    (let (conf perm filter action)
	      (if (null subject)
              (setq subject lyskom-current-subject))
	      (setq subject 
                (lyskom-read-from-minibuffer 
		 (lyskom-get-string 'filter-subject)
		 subject))
	      (setq filter (cons (cons 'subject subject) filter))
	      (setq conf (lyskom-read-conf-no
                      (lyskom-get-string 'filter-in-conf)
                      '(all)
                      t
                      (or (and (conf-stat->conf-no conf-stat)
                               (cons (conf-stat->name conf-stat) 0))
                          "")
                      t))
	      (if (/= conf 0)
              (setq filter (cons (cons 'recipient-no conf) filter)))
	      (setq action (lyskom-filter-read-action))
	      (setq perm (lyskom-filter-read-permanent))
	      
	      (lyskom-add-filter
	       (make-filter filter
                        (list (cons 'action action)
                              (cons 'expire (not perm)))))))))
  

     
;;;========================================
;;; Filtrera författare --- Filter author
;;;


(def-kom-command kom-filter-author ()
  "Interactively filter an author. This creates a permanent or
temporary filter on a single author in one conference or all
conferences.

To change existing filters, use `kom-filter-edit'."
  (interactive)
  (let (auth-stat author conf filter action permanent)
	(blocking-do-multiple ((text-stat (get-text-stat 
                                       (or lyskom-current-text 0)))
                           (conf-stat (get-conf-stat
                                       lyskom-current-conf)))
      (if text-stat
          (setq auth-stat (blocking-do 'get-conf-stat
                                       (text-stat->author text-stat))))
      (setq author 
            (lyskom-read-conf-no (lyskom-get-string 'filter-author)
                                 '(pers)
                                 t
                                 (or (and auth-stat
                                          (cons (conf-stat->name auth-stat) 0))
                                     "")
                                 t))
      (if (/= author 0)
          (setq filter (cons (cons 'author-no author) filter)))
      (setq conf (lyskom-read-conf-no
                  (lyskom-get-string 'filter-in-conf)
                  '(all)
                  t
                  (or 
                   (and conf-stat
                        (cons (conf-stat->name conf-stat) 0))
                   "")
                  t))
      (if (/= conf 0)
          (setq filter (cons (cons 'recipient-no conf) filter)))
      (setq action (lyskom-filter-read-action))
      (setq permanent (lyskom-filter-read-permanent))
      (lyskom-add-filter
       (make-filter filter
                    (list (cons 'action action)
                          (cons 'expire (not permanent))))))))

(def-kom-command kom-filter-recipient ()
  "Interactively filter a recipient. This creates a permanent or
temporary filter on a particular recipient.

To change existing filters, use `kom-filter-edit'."
  (interactive)
  (let ((conf-no (lyskom-read-conf-no 'filter-recipient
                                      '(all)
                                      nil
                                      nil
                                      t))
        (action (lyskom-filter-read-action))
        (permanent (lyskom-filter-read-permanent)))
    (cond ((and conf-no (not (zerop conf-no)))
           (lyskom-add-filter
            (make-filter (list (cons 'recipient-no conf-no))
                         (list (cons 'action action)
                               (cons 'expire (not permanent)))))
           ))))




;;;============================================================
;;;
;;; Superhoppa
;;;

(def-kom-command kom-super-jump (text-no)
  "Skip all texts and comments that share the subject and recipient of
the selected text. This creates a temporary filter on the subject of
the selected text in one of its recipients. The recipient is selected
automatically: if the current conference is a recipient, then filter
in that conference. Otherwise filter in the first recipient that the
user is a member of.

To change existing filters, use `kom-filter-edit'.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'super-jump-q)))
  (if (or (null lyskom-current-text)
          (zerop lyskom-current-text))
      (lyskom-insert-string 'have-to-read)
    (let ((text-stat (blocking-do 'get-text-stat lyskom-current-text))
          (all-recipients nil)
          (recipients-member nil)
          (cc-recipients-member nil)
          (bcc-recipients-member nil)
          (recipients-nonmember nil)
          (cc-recipients-nonmember nil)
          (bcc-recipients-nonmember nil)
          (filter-recipient nil))

      ;; Extract recipients into different lists
      (lyskom-traverse misc
                       (text-stat->misc-info-list text-stat)
        (when (memq (misc-info->type misc) lyskom-recpt-types-list)
          (let* ((conf-no (misc-info->recipient-no misc))
                 (symbol
                  (if (lyskom-get-membership conf-no)
                      (cond ((eq (misc-info->type misc) 'RECPT)
                             'recipients-member)
                            ((eq (misc-info->type misc) 'CC-RECPT)
                             'cc-recipients-member)
                            ((eq (misc-info->type misc) 'BCC-RECPT)
                             'bcc-recipients-member))
                    (cond ((eq (misc-info->type misc) 'RECPT)
                           'recipients-nonmember)
                          ((eq (misc-info->type misc) 'CC-RECPT)
                           'cc-recipients-nonmember)
                          ((eq (misc-info->type misc) 'BCC-RECPT)
                           'bcc-recipients-nonmember)))))
            (set symbol (cons conf-no (symbol-value symbol)))
            (setq all-recipients (cons conf-no all-recipients)))))

      ;; Get the recipient to filter in
      (setq filter-recipient 
            (or (and (memq lyskom-current-conf all-recipients) lyskom-current-conf)
                (car (nreverse recipients-member))
                (car (nreverse cc-recipients-member))
                (car (nreverse bcc-recipients-member))
                (car (nreverse recipients-nonmember))
                (car (nreverse cc-recipients-nonmember))
                (car (nreverse bcc-recipients-nonmember))))

      (if (null filter-recipient)
          (lyskom-insert-string 'no-recipient)

        (let ((text lyskom-current-subject))
          (when (string-match "^\\s-*$" lyskom-current-subject) (setq text ""))

          (lyskom-add-filter
           (make-filter (list 
                         (cons 'subject-re 
                               (concat (if (string-equal text "") "^" "")
                                       "\\([rR][eE]: *\\|[Ff][Ww][Dd]: *\\)*"
                                       (replace-in-string
                                        (regexp-quote text)
                                        "[ \t]+" "[ \t]+")
                                       (if (string-equal text "") "$" "")))
                         (cons 'recipient-no filter-recipient))
                        (list (cons 'action 'skip-tree)
                              (cons 'expire t))))
          (lyskom-format-insert 'super-jump
                                (copy-sequence lyskom-current-subject)
                                filter-recipient))))))




;;;============================================================
;;;
;;; Filtrera text
;;;

(def-kom-command kom-filter-text (&optional text)
  "Interactively filter on text contents. This creates a permanent or
temporary filter on the contents of texts in all conferences or in a
single conference.

To change existing filters, use `kom-filter-edit'."
  (interactive)
  (if (/= 0 lyskom-current-conf)
	  (let ((conf-stat (blocking-do 'get-conf-stat lyskom-current-conf))
            (conf nil)
            (action nil)
            (perm nil)
            (filter nil))
	    (if conf-stat
            (progn
              (setq text 
                    (lyskom-read-from-minibuffer (lyskom-get-string 
						  'filter-which-text)
						 (or text "")))
              (setq filter (cons (cons 'text text) filter))
              (setq conf (lyskom-read-conf-no
                          (lyskom-get-string 'filter-in-conf)
                          '(all) t
                          (or (and (conf-stat->conf-no conf-stat)
                                   (cons (conf-stat->name conf-stat) 0))
                              "")
                          t))
              (if (/= conf 0)
                  (setq filter (cons (cons 'recipient-no conf) filter)))
              (setq action (lyskom-filter-read-action))
              (setq perm (lyskom-filter-read-permanent))
              (lyskom-add-filter
               (make-filter filter
                            (list (cons 'action action)
                                  (cons 'expire (not perm))))))))))


;;;============================================================
;;; Lista filter                         (kom-list-filters)
;;;
;;; Author: David Byers
;;; Calls internal functions in filter-edit mode. This may or
;;; may not be a good idea, but it works...

(def-kom-command kom-list-filters ()
  "Display all filters. This lists all filters in the LysKOM buffer.

To change existing filters, use `kom-filter-edit'."
  (interactive)
  (let ((filters lyskom-filter-list))
    (goto-char (point-max))
    (if (null filters)
        (lyskom-insert (lyskom-get-string 'no-filters))
      (progn
        (lyskom-insert (lyskom-get-string 'view-filters-header))
        (while filters
          (goto-char (point-max))
          (lyskom-format-filter-pattern (car filters))
          (setq filters (cdr filters)))
        (lyskom-insert (lyskom-get-string 'view-filters-footer))))))

