;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: filter-new.el,v 44.3 2002-02-24 20:23:27 joel Exp $
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
;;;; File: filter-new.el
;;;;
;;;; New-style extensible filters.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: filter-new.el,v 44.3 2002-02-24 20:23:27 joel Exp $\n"))

;;; ================================================================
;;; Sections
;;;
;;; Filter compilation
;;;     Compilation of filters
;;;
;;; Filter predicates
;;;     Functions defining what can be included in filters
;;;
;;; Filters
;;;     Functions pertaining to running and evaluating filters.



;;; ================================================================
;;; Filter compilation

(defvar lyskom-filter-compile-need-data)

(defun lyskom-filter-compile (pattern)
  (let* ((lyskom-filter-compile-need-data nil)
         (result (lyskom-filter-compile-internal pattern))
         (bindings
          (mapcar (lambda (sym)
                          `(,sym (lyskom-filter-get-text-data ',sym)))
                  (lyskom-filter-expand-required-data lyskom-filter-compile-need-data))))
    `(lambda () (let ,bindings ,result))))

(defun lyskom-filter-compile-internal (pattern)
  (let* ((key (car pattern))
         (data (cdr (cdr pattern)))
         (predicate (lyskom-filter-get-predicate key))
         (datatype (and predicate (lyskom-filter-get-datatype
                                   (filter-predicate->datatype predicate)))))
    (when (filter-datatype->data-predicate datatype)
      (apply (filter-datatype->data-predicate datatype) predicate data))
    (setq lyskom-filter-compile-need-data
          (append (filter-predicate->prerequisite-data predicate)
                  lyskom-filter-compile-need-data))
    (apply (filter-datatype->compile-function datatype)
           (if (filter-predicate->compile-args predicate)
               (append (filter-predicate->compile-args predicate)
                       data)
             data))))

(defun lyskom-filter-compile-list (pattern)
  (let ((result nil))
    (while pattern
      (setq result (cons (lyskom-filter-compile-internal (car pattern)) result))
      (setq pattern (cdr pattern)))
    (nreverse result)))

(defun lyskom-filter-check-one-arg (check predicate args)
  (unless (= 1 (length args))
    (signal 'wrong-number-of-arguments 
            (list (filter-predicate->name predicate)
                  (length args))))
  (unless (funcall check (car args))
    (signal 'wrong-type-argument (list check (car args)))))

(defun lyskom-filter-check-string (predicate &rest args)
  (lyskom-filter-check-one-arg 'stringp predicate args))

(defun lyskom-filter-check-regexp (predicate &rest args)
  (lyskom-filter-check-one-arg 'regexpp predicate args))

(defun lyskom-filter-check-integer (predicate &rest args)
  (lyskom-filter-check-one-arg 'integerp predicate args))

(defun lyskom-filter-compile-conjunction (&rest pattern)
  (cons 'and (lyskom-filter-compile-list pattern)))

(defun lyskom-filter-compile-disjunction (&rest pattern)
  (cons 'or (lyskom-filter-compile-list pattern)))

(defun lyskom-filter-compile-negation (&rest pattern)
  (if (> 1 (length pattern))
      (cons 'not (list (cons 'or (lyskom-filter-compile-list pattern))))
    (cons 'not (lyskom-filter-compile-list pattern))))

(defun lyskom-filter-compile-string-check (accessor arg)
  `(string-match ,(regexp-quote arg) ,accessor))

(defun lyskom-filter-compile-regexp-check (accessor arg)
  `(string-match ,arg ,accessor))

(defun lyskom-filter-compile-conf-no-check (accessor arg)
  `(= ,accessor ,arg))

(defun lyskom-filter-compile-string-list-check (list-source accessor arg)
  `(lyskom-filter-is-member string-match
                            ,(regexp-quote arg)
                            ,list-source
                            ,accessor))

(defun lyskom-filter-compile-regexp-list-check (list-source accessor arg)
  `(lyskom-filter-is-member string-match
                            ,arg
                            ,list-source
                            ,accessor))

(defun lyskom-filter-compile-conf-no-list-check (list-source accessor arg)
  `(lyskom-filter-is-member =
                            ,arg
                            ,list-source
                            ,accessor))


;; (fset 'foo  (lyskom-filter-compile
;;   '(and "" 
;;         (author nil "David")
;;         (recipient nil "Jim")
;;         (subject nil "Foo"))))
;; 
;; (byte-compile 'foo)

;; (lyskom-filter-compile 
;;  '(and (or (recipient = "Test")
;;            (recipient = "Tset"))
;;        (not (or (subject = "Foo")
;;                 (and (text = "Fjuk")
;;                      (recipient = "Fjuk"))))
;;        (not (author-no = 119))))






;;; ================================================================
;;; Filter predicates
;;;
;;; FIXME: Predicates to add:
;;; FIXME:     Filter by who added a text to a conference
;;; FIXME:     Filter mx-author, mx-from etc.
;;; FIXME:     Filter on aux-items in general

(defvar lyskom-filter-datatypes nil)
(defvar lyskom-filter-predicates nil)

(def-komtype filter-datatype 
  name
  compile-function
  read-function
  print-function
  data-predicate
  is-function
  )

(def-komtype filter-predicate
  name
  datatype
  prerequisite-data
  compile-args
  read-function-args
  print-function
  )

(defmacro define-lyskom-filter-datatype (name &rest args)
  `(setq lyskom-filter-datatypes
         (cons (cons ',name
                     (lyskom-create-filter-datatype
                      ',name
                      ,(plist-get args :compile-function)
                      ,(plist-get args :read-function)
                      ,(plist-get args :print-function)
                      ,(plist-get args :data-predicate)
                      ,(plist-get args :is-function)
                      ))
               lyskom-filter-datatypes)))

(defmacro define-lyskom-filter-predicate (name &rest args)
  `(setq lyskom-filter-predicates
         (cons (cons ',name (lyskom-create-filter-predicate
                             ',name
                             ,(plist-get args :type)
                             ,(plist-get args :data)
                             ,(plist-get args :compile-args)
                             ,(plist-get args :read-function-args)
                             ,(plist-get args :print-function)
                             ))
               lyskom-filter-predicates)))

(defun lyskom-filter-get-predicate (name)
  "Return the filter predicate NAME."
  (cdr (assq name lyskom-filter-predicates)))

(defun lyskom-filter-get-datatype (name)
  "Return the filter datatype NAME."
  (cdr (assq name lyskom-filter-datatypes)))


(define-lyskom-filter-datatype 
  string
  :compile-function 'lyskom-filter-compile-string-check
  :read-function 'lyskom-read-from-minibuffer
  :print-function 'lyskom-filter-edit-print-string
  :data-predicate 'lyskom-filter-check-string
  )

(define-lyskom-filter-datatype
  string-list
  :compile-function 'lyskom-filter-compile-string-list-check
  :read-function 'lyskom-read-from-minibuffer
  :print-function 'lyskom-filter-edit-print-string
  :data-predicate 'lyskom-filter-check-string
  )

(define-lyskom-filter-datatype
  regexp
  :compile-function 'lyskom-filter-compile-regexp-check
  :read-function 'lyskom-filter-edit-read-regexp
  :print-function 'lyskom-filter-edit-print-regexp
  :data-predicate 'lyskom-filter-check-regexp
  )

(define-lyskom-filter-datatype
  regexp-list
  :compile-function 'lyskom-filter-compile-regexp-list-check
  :read-function 'lyskom-filter-edit-read-regexp
  :print-function 'lyskom-filter-edit-print-regexp
  :data-predicate 'lyskom-filter-check-regexp
  )



(define-lyskom-filter-datatype
  conf-no
  :compile-function 'lyskom-filter-compile-conf-no-check
  :read-function 'lyskom-read-conf-no
  :print-function 'lyskom-filter-edit-print-conf-no
  :data-predicate 'lyskom-filter-check-integer
  )

(define-lyskom-filter-datatype
  conf-no-list
  :compile-function 'lyskom-filter-compile-conf-no-list-check
  :read-function 'lyskom-read-conf-no
  :print-function 'lyskom-filter-edit-print-conf-no
  :data-predicate 'lyskom-filter-check-integer
  )

(define-lyskom-filter-datatype
  conjunction
  :compile-function 'lyskom-filter-compile-conjunction
  :print-function 'lyskom-filter-edit-print-builtin
  :is-function t
  )

(define-lyskom-filter-datatype
  disjunction
  :compile-function 'lyskom-filter-compile-disjunction
  :print-function 'lyskom-filter-edit-print-builtin
  :is-function t
  )

(define-lyskom-filter-datatype
  negation
  :print-function 'lyskom-filter-edit-print-builtin
  :compile-function 'lyskom-filter-compile-negation
  :is-function t
  )


(define-lyskom-filter-predicate
  and
  :type 'conjunction
  )

(define-lyskom-filter-predicate
  or
  :type 'disjunction
  )

(define-lyskom-filter-predicate
  not
  :type 'negation
  )

(define-lyskom-filter-predicate 
  author
  :type 'string
  :data '(author)
  :compile-args '((conf-stat->name author))
  :read-function-args 'read-author-name
  )

(define-lyskom-filter-predicate
  author-re
  :type 'regexp
  :data '(author)
  :compile-args '((conf-stat->name author))
  :read-function-args 'read-author-name
  )

(define-lyskom-filter-predicate
  author-no
  :type 'conf-no
  :data '(author)
  :compile-args '((conf-stat->conf-no author))
  :read-function-args '(read-author-name)
  )

(define-lyskom-filter-predicate
  recipient
  :type 'string-list
  :data '(recipient)
  :compile-args '(recipient conf-stat->name)
  :read-function-args '(read-recipient-name)
  )

(define-lyskom-filter-predicate
  recipient-re
  :type 'regexp-list
  :data '(recipient)
  :compile-args '(recipient conf-stat->name)
  :read-function-args '(read-recipient-name)
  )

(define-lyskom-filter-predicate
  recipient-no
  :type 'conf-no-list
  :data '(recipient)
  :compile-args '(recipient conf-stat->conf-no)
  :read-function-args '(read-recipient-name)
  )

(define-lyskom-filter-predicate
  subject
  :type 'string
  :data '(subject)
  :compile-args '(subject)
  :read-function-args '(read-subject)
  )

(define-lyskom-filter-predicate
  subject-re
  :type 'regexp
  :data '(subject)
  :compile-args '(subject)
  :read-function-args '(read-subject)
  )

(define-lyskom-filter-predicate
  text
  :type 'string
  :data '(text)
  :compile-args '((text-mass->text text))
  :read-function-args '(read-text)
  )

(define-lyskom-filter-predicate
  text-re
  :type 'regexp
  :data '(text)
  :compile-args '((text-mass->text text))
  :read-function-args '(read-text)
  )



(defun lyskom-filter-edit-print-string (filter &optional negated)
  (lyskom-format "%#1s \"%#2s\""
                 (if negated "does not contain" "contains")
                 (car (lyskom-filter-predicate-data filter))))

(defun lyskom-filter-edit-print-regexp (filter &optional negated)
  (lyskom-format "%#1s \"%#2s\""
                 (if negated "does not match" "matches")
                 (car (lyskom-filter-predicate-data filter))))

(defun lyskom-filter-edit-print-conf-no (filter &optional negated)
  (lyskom-format "%#1s %#2M <%#2m>"
                 (if negated "is" "is not")))

(defun lyskom-filter-edit-print-builtin (filter &optional negated)
  (cond ((eq (car filter) 'and) (if negated "Not all of" "All of"))
        ((eq (car filter) 'or) (if negated "None of" "At least one of"))
        ((eq (car filter) 'not) (if negated "At least one of" "None of"))))

(defun lyskom-filter-edit-read-regexp (prompt &rest args)
  (let ((result nil)
        (contents (car args))
        (args (cdr args)))
    (while (not (regexpp result))
      (setq result (apply 'lyskom-read-from-minibuffer prompt contents args))
      (unless (regexpp result)
        (message "Not a valid regular expression.")
        (sit-for 2)
        (setq contents result)))
    result))



;;; ============================================================
;;; Filters


(def-komtype lyskom-filter 
  pattern
  attributes
  compiled
  )


(defun set-lyskom-filter-attribute (filter attribute value)
  (set-lyskom-filter->attributes 
   filter
   (plist-put (lyskom-filter->attributes filter) attribute value)))

(defun lyskom-filter-attribute (filter attribute)
  (plist-get (lyskom-filter->attributes filter) attribute))

(defun lyskom-filter-predicate-data (filter)
  (cdr (cdr filter)))

(defun lyskom-filter-predicate-comment (filter)
  (car (cdr filter)))



(defvar lyskom-filter-text-data)

(defvar lyskom-filter-text-data-types
  '((text nil lyskom-filter-generate-text-data)
    (text-stat nil lyskom-filter-generate-text-stat-data)
    (subject (text) lyskom-filter-generate-subject-data)
    (author (text-stat) lyskom-filter-generate-author-data)
    (recipient (text-stat) lyskom-filter-generate-recipient-data))
  "A list of attributes that a text can have that are relevant to filtering.
Each element in the list is a triple (ITEM PREREQ FUNCTION)
ITEM is the name of the item, a symbol
PREREQ is a list of prerequisite items for this item
FUNCTION is a function to generate the data item")

(defun lyskom-filter-expand-required-data (data)
  "Return a list of text data items that are required according to DATA.
DATA is a list of text data items. This function adds those items that
are required to satisfy all requirements in DATA and orders the list
in a way that is safe (i.e. all prerequisites are computed before anything
that needs them"
  (let ((result nil)
        (item nil))
    (while data
      (unless (memq (car data) result)
        (setq item (assq (car data) lyskom-filter-text-data-types))
        (when item
          (mapcar (lambda (prerequisite)
                    (unless (memq prerequisite result)
                      (setq result (cons prerequisite result))))
                  (lyskom-filter-expand-required-data (elt item 1))))
        (setq result (cons (car data) result)))
      (setq data (cdr data)))
    (nreverse result)))

;;; ============================================================
;;; Utility functions to work with recorded text data
;;;

(defun lyskom-filter-record-text-data (type value)
  (setq lyskom-filter-text-data
        (cons (cons type value) lyskom-filter-text-data)))

(defsubst lyskom-filter-get-text-data (type)
  (cdr (or (assq type lyskom-filter-text-data)
           (progn (lyskom-filter-generate-text-data type)
                  (assq type lyskom-filter-text-data)))))

(defun lyskom-filter-generate-text-data (type)
  (let ((item (assq type lyskom-filter-text-data-types)))
    (when item (funcall (elt item 3) type))))


;;; ============================================================
;;; Functions to generate text data
;;;

(defun lyskom-filter-generate-text-data (type)
  (let ((text-no (lyskom-filter-get-text-data 'text-no)))
    (lyskom-filter-record-text-data type (blocking-do 'get-text text-no))))

(defun lyskom-filter-generate-text-stat-data (type)
  (let ((text-no (lyskom-filter-get-text-data 'text-no)))
    (lyskom-filter-record-text-data type
                                    (blocking-do 'get-text-stat text-no))))

(defun lyskom-filter-generate-subject-data (type)
  (let ((text (lyskom-filter-get-text-data 'text)))
    (lyskom-filter-record-text-data type
                                    (save-match-data
                                      (if (string-match "\n" text)
                                          (substring text 0 (match-beginning 0))
                                        "")))))


(defun lyskom-filter-generate-author-data (type)
  (let ((text-stat (lyskom-filter-get-text-data 'text-stat)))
    (lyskom-filter-record-text-data 
     type
     (blocking-do 'get-conf-stat
                  (text-stat->author text-stat)))))

(defun lyskom-filter-generate-recipient-data (type)
  (let ((text-stat (lyskom-filter-get-text-stat 'text-stat))
        (collector (make-collector)))
    (lyskom-traverse recipient (text-stat->misc-info-list text-stat)
        (when (memq (misc-info->type item)
                    lyskom-recpt-types-list)
          (initiate-get-conf-stat 'filter
                                  'collector-push
                                  (misc-info->recipient-no item)
                                  collector)))
    (lyskom-wait-queue 'filter)
    (lyskom-filter-record-text-data type
                                    (collector->value collector))))


;;; ============================================================
;;; Top-level functions
;;;

(defun lyskom-filter-text-p (text-no)
  (let ((lyskom-filter-text-data (cons 'text-no text-no))
        (filter-result nil)
        (filters lyskom-filter-list)
        (result nil))
    (while filters
      (setq result (lyskom-filter-run (car filters)))
      (if result
          (setq filters nil)
        (setq filters (cdr filters))))
    result))


(defun lyskom-filter-run (filter)
  (let ((fn (lyskom-filter->compiled filter)))
    (when (null fn)
      (setq fn (lyskom-filter-compile 
                (lyskom-filter->pattern filter)))
      (set-lyskom-filter->compiled filter fn))

    (if (functionp fn)
        (funcall fn)
      nil)))

