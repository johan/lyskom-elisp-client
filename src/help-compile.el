;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: help-compile.el,v 44.6 2006-03-31 11:48:17 byers Exp $
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
;;;; File: help-compile.el
;;;;
;;;; Compiler for elisp-client help files. This file is only used for
;;;; compilation and is currently not linked into the runtime.
;;;;


(defvar lyskom-help-parse-debugging (getenv "LYSKOM_HELP_DEBUG"))

;;; Note that these are duplicated in help.el. Change one and change both!

(defsubst lyskom-help-create-data (tag attr data)
  (list tag attr data))

(defsubst lyskom-help-data-get-tag (data)
  (elt data 0))

(defsubst lyskom-help-data-get-attr (attr data)
  (cdr (assq attr (elt data 1))))

(defsubst lyskom-help-data-get-attrs (data)
  (elt data 1))

(defsubst lyskom-help-data-get-data (data)
  (elt data 2))

(defmacro lyskom-help-traverse (atom sequence &rest body)
  "Bind ATOM to each element in SEQUENCE and execute BODY.
Value returned is always nil."
  `(let* ((__i__ 0)
          (__sequence__ ,sequence)
          (__len__ (or (listp __sequence__ )
                       (length __sequence__)))
          (,atom nil)
          (__result__ nil))
     (setq __result__ __result__)       ; Get rid of compiler warnings
     (if (listp __sequence__)
         (while __sequence__
           (setq ,atom (car __sequence__))
           ,@body
           (setq __sequence__ (cdr __sequence__)))
       (while (< __i__ __len__)
         (setq ,atom (aref __sequence__ __i__))
         ,@body
         (setq __i__ (1+ __i__))))
     __result__))

(defvar lyskom-help-syntax
  '((help (language) nil (section) lyskom-help-parse-process-finish)
    (section (id prompt) nil (p list h1 h2 h3 inline refer keymap) lyskom-help-parse-process-section)
    (h1 nil nil (cref TEXT) nil)
    (h2 nil nil (cref TEXT) nil)
    (h3 nil nil (cref TEXT) nil)
    (p nil nil (b i cref list refer TEXT) nil)
    (b nil nil (cref TEXT) nil)
    (i nil nil (cref TEXT) nil)
    (list nil (header) (item) nil)
    (item nil nil (refer b i cref TEXT) nil)
    (keymap nil nil nil)
    (inline (id) nil nil lyskom-help-parse-process-section-ref)
    (refer (id) nil nil lyskom-help-parse-process-section-ref)
    (cref (id) nil nil lyskom-help-parse-process-command-ref)
    )
  "Syntax of the help language.
Each element is the syntax for a single tag. Each list consists of
TAG ATTRS CONTENTS")

(defun lyskom-help-syntax-required-attrs (syntax)
  "Return the required attributes of a syntax element"
  (elt syntax 1))

(defun lyskom-help-syntax-optional-attrs (syntax)
  "Return the required attributes of a syntax element"
  (elt syntax 2))

(defun lyskom-help-syntax-contents (syntax)
  "Return the allowed content types of a syntax element"
  (elt syntax 3))

(defun lyskom-help-syntax-postprocess (syntax)
  "Return the allowed content types of a syntax element"
  (elt syntax 4))


(defvar lyskom-help-parse-section-list nil)
(defvar lyskom-help-parse-section-ref-list nil)
(defvar lyskom-help-parse-command-ref-list nil)
(defvar lyskom-help-parse-unresolved-section-references nil)


(defun lyskom-help-parse-process-section (parse)
  (setq lyskom-help-parse-section-list
        (cons (intern (lyskom-help-data-get-attr 'id parse))
              lyskom-help-parse-section-list))
  )

(defun lyskom-help-parse-process-section-ref (parse)
  (setq lyskom-help-parse-section-ref-list
        (cons (intern (lyskom-help-data-get-attr 'id parse))
              lyskom-help-parse-section-ref-list))
  )

(defun lyskom-help-parse-process-command-ref (parse)
  (setq lyskom-help-parse-command-ref-list
        (cons (intern (lyskom-help-data-get-attr 'id parse))
              lyskom-help-parse-command-ref-list))
  )

(defun lyskom-help-parse-process-finish (parse)
  (lyskom-help-traverse id lyskom-help-parse-section-list
    (setq lyskom-help-parse-section-ref-list 
          (delq id lyskom-help-parse-section-ref-list)))
  (when lyskom-help-parse-section-ref-list
    (setq lyskom-help-parse-unresolved-section-references
          lyskom-help-parse-section-ref-list)
    (error "Unresolved help section references: %s"
           lyskom-help-parse-section-ref-list))
  )





(defun lyskom-non-matching-regexp (string)
  "Compute a regexp that will match anything but STRING."
  (let ((alternatives nil)
        (i 0))
    (while (< i (length string))
      (setq alternatives (cons (concat (substring string 0 i)
                                       "[^" 
                                       (substring string i (1+ i))
                                       "]")
                               alternatives)
            i (1+ i)))
    (concat "\\("
            (mapconcat 'identity (nreverse alternatives) "\\|")
            "\\)*")))


(defun lyskom-help-parse-attrs (string)
  (save-match-data 
    (let ((result nil)
          (start 0))
      (while (string-match "\\s-*\\([a-z]+\\)=\"\\([^\"]*\\)\"\\s-*" string start)
        (setq result (cons (cons (intern (match-string 1 string))
                                 (match-string 2 string))
                           result)
              start (match-end 0)))
      (nreverse result))))

(defun lyskom-help-debug (fmt &rest args)
  (and lyskom-help-parse-debugging
       (apply 'message fmt args)))

(defun lyskom-help-parse-string (string &optional containing-syntax)
  "Convert a help string STRING to a tree, checking it as we go.
CONTAINING-SYNTAX is the syntax specification for the containing tag."
  (let ((result nil)
        (collect-data (memq 'TEXT
                            (lyskom-help-syntax-contents containing-syntax)))
        (start 0))

    ;; Loop over the string, looking for tags

    (while (string-match
            "<\\([a-z0-9]+\\)\\(\\(\\s-+[a-z0-9]+=\"[^\"]*\"\\)*\\)\\s-*\\(/*\\)>"
            string start)
      (let* ((tag (match-string 1 string))
             (tag-symbol (intern tag))
             (attrs (lyskom-help-parse-attrs (match-string 2 string)))
             (tag-start (match-beginning 0))
             (tag-end (match-end 0))
             (end (string= "/" (match-string 4 string)))
             (syntax (assq tag-symbol lyskom-help-syntax))
             (contents nil))

        (lyskom-help-debug "%s %S" tag attrs)

        ;; Check that the tag is known

        (unless syntax (error "Unknown tag %s" tag))

        ;; Check that the tag is permitted in this context

        (unless (memq tag-symbol 
                      (lyskom-help-syntax-contents containing-syntax))
          (error "Tag %s not permitted in %s" tag (car containing-syntax)))

        ;; Check that all required attributes are present

        (lyskom-help-traverse required-attribute 
                              (lyskom-help-syntax-required-attrs syntax)
          (unless (assq required-attribute attrs)
            (error "Missing required attribute %s to tag %s" 
                   required-attribute tag)))

        ;; Check that all attributes are either required or optional

        (lyskom-help-traverse attribute attrs
           (unless (or (memq (car attribute) (lyskom-help-syntax-required-attrs syntax))
                       (memq (car attribute) (lyskom-help-syntax-optional-attrs syntax)))
             (error "Invalid attribute %s to tag %s" (car attribute) tag)))

        ;; Found a tag, but there is intervening text
        ;; If we were called with instructions to save this data,
        ;; save it. If not, ensure that the intervening text only
        ;; consists of whitespace.

        (when (> tag-start start)
          (let ((text (substring string start tag-start)))
            (cond (collect-data (setq result (cons (cons 'TEXT text) result)))
                  ((string-match "\\`\\s-*\\'" text))
                  (t (error "Text data \"%s\" inside %s tag" text tag)))))

        ;; Find the position of the end tag

        (let ((end-tag-start nil)
              (end-tag-end nil))
          (if end
              (setq end-tag-start tag-end end-tag-end tag-end)
            (let ((re (concat (lyskom-non-matching-regexp
                               (concat "</" tag ">"))
                              "\\(</" tag ">\\)")))
              (if (string-match (concat "</" tag ">") string tag-end)
                  (setq end-tag-start (match-beginning 0)
                        end-tag-end (match-end 0))
                (error "Unterminated tag: %s" tag tag-start))))

          ;; Have the position of the end tag
          ;; Put the element we found into result

          (setq result
                (cons (lyskom-help-create-data
                       tag-symbol
                       attrs
                       (if (eq end-tag-start tag-end)
                           nil
                         (lyskom-help-parse-string
                          (substring string tag-end end-tag-start)
                          syntax)))
                      result)
                start end-tag-end)

          ;; Postprocess the parsed element
          (when (lyskom-help-syntax-postprocess syntax)
            (lyskom-help-debug "  > postprocess")
            (funcall (lyskom-help-syntax-postprocess syntax) (car result))
            (lyskom-help-debug "  < postprocess")
            ))))

    ;; No more tags
    ;; There's just text left. Record it.

    (unless (eq start (length string))
      (let ((text (substring string start)))
        (cond (collect-data (setq result (cons (cons 'TEXT text) result)))
              ((string-match "\\`\\s-*\\'" text))
              (t (error "Unexpected text \"%s\" at %d in document %s" 
                        text start string)))))

    ;; Done. The result is, as usual, in reverse order.

    (nreverse result)))

(defun lyskom-help-replace-regexp (regexp to-string)
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-string nil nil)))

(defun lyskom-help-parse-help-file (filename)
  (save-excursion
    (let ((buffer (get-buffer-create "*Help*-parse"))
          (lyskom-help-parse-section-list nil)
          (lyskom-help-parse-section-ref-list nil)
          (lyskom-help-parse-command-ref-list nil))
      (set-buffer buffer)
      (erase-buffer)
      (insert-file-contents filename)
      (lyskom-help-parse-buffer))))

(defun lyskom-help-parse-buffer ()
  (lyskom-help-replace-regexp "[\r\n]" "")
  (lyskom-help-replace-regexp "<!--[^>]*-->" "")
  (lyskom-help-replace-regexp "\\s-+" " ")
  (let ((parse (lyskom-help-parse-string (buffer-string)
                                         '(toplevel nil nil (help)))))
    parse))

(defun lyskom-help-parse-extract (parse tag)
  (let ((result nil))
    (lyskom-help-traverse toplevel parse
      (when (eq (lyskom-help-data-get-tag toplevel) tag)
        (setq result
              (cons (list (intern (lyskom-help-data-get-attr 'id toplevel))
                          (lyskom-help-data-get-attr 'prompt toplevel)
                          toplevel)
                    result))))
    (nreverse result)))

(defmacro lyskom-help-compile (language)
  `(quote ,(lyskom-help-parse-extract
            (lyskom-help-data-get-data
             (car (lyskom-help-parse-help-file (format "help-%s.xml" language))))
            'section)))

(defvar lyskom-help-language-mapping '((sv . "swedish") (en . "english")))

(defun lyskom-help-compile-to-el ()
  (let* ((parse (lyskom-help-parse-buffer))
         (result (lyskom-help-parse-extract (lyskom-help-data-get-data (car parse)) 'section))
         (language-code (intern (lyskom-help-data-get-attr 'language (car parse))))
         (language (cdr (assq language-code lyskom-help-language-mapping)))
         (sym (intern (format "lyskom-%S-help-data" language-code))))
    (if (null language)
        (error "Unknown language code `%S'" language-code))
    (set-buffer (get-buffer-create "*LysKOM Help Output*"))
    (erase-buffer)
    (let ((standard-output (current-buffer)))
      (print `(defvar ,sym ',result))
      (write-file (format "%s-help.el" language)))))


