;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: docgen.el,v 1.1 2003-01-17 00:43:17 byers Exp $
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
;;;; File: docgen.el
;;;;
;;;; Generate documentation data for inclusion in the user manual
;;;;


;;; TO DO
;;;
;;; Variable documentation requires considerably more flexibility
;;; than command documentation:
;;;
;;; * Generate segmentedlist elements. Look for two-column
;;;   indented parts in the documentation.
;;;
;;; * Highlight code (e.g. `t', `nil', `other' usw. Handle 
;;;   references to elisp functions and elisp variables.
;;;
;;; * Format examples. I don't think we can do this consistently
;;;   without any markup. MAybe by looking for indented blocks
;;;   where some line in a block does not match a segmentedlist 
;;;   format.
;;;
;;; In variables, warn for undocumented (no **) def-kom-vared
;;; variables that start with kom-
;;;
;;; Generate DTD fragments so we can use entities for command and
;;; variable references (e.g. &fn:kom-mark-text;) in XML.
;;;
;;; Read templates from a file, so we don't hard-code the
;;; format here.
;;;
;;; Specify output file on the command line.
;;;
;;; Specify lyskom.elc on the command line.
;;;
;;; Read a list of external references from a file. Ideally we'd read
;;; and parse the actual XML, but that is probably too much work. We
;;; need to map references like lyskom-read-text-no-prefix-arg to the
;;; actual ID to link to. Specify this file on the command line too.
;;;
;;; Do variable documentation.
;;;
;;; Write or find a stylesheed that doesn't suck. Mainly paragraph
;;; spacing is way too big, and the period after section numbers is
;;; just plain wrong. Maybe this can be fixed by setting parameters to
;;; the DSSSL processor -- see
;;; /usr/share/sgml/docbook/stylesheet/dsssl/modular/print/dbparam.dsl
;;; for all available parameters. %para-sep% is interesting.
;;;

(require 'cl)


(defvar lyskom-docgen-external-refs
  '((lyskom-read-text-no-prefix-arg . lyskom-read-text-no-prefix-arg)))

(defvar lyskom-docgen-errors nil)

(defvar lyskom-docgen-found-variables nil)
(defvar lyskom-docgen-found-functions nil)

(defvar lyskom-docgen-xrefd-variables nil)
(defvar lyskom-docgen-xrefd-functions nil)


(defun lyskom-docgen-error (fmt &rest args)
  (apply 'message fmt args)
  (setq lyskom-docgen-errors (cons (apply 'format fmt args)
                                     lyskom-docgen-errors)))

(defun lyskom-docgen-report-errors (stage)
  (when nil
    (lyskom-traverse msg (nreverse lyskom-docgen-errors)
      (message "%s: %s" stage msg))))

(defun lyskom-docgen-princ (str)
  (insert str))

(defun lyskom-docgen-fixup-sgml (str)
  (when str
    (let ((s str))
      (setq s (replace-in-string s "&" "&amp;"))
      (setq s (replace-in-string s "<" "&lt;"))
      (setq s (replace-in-string s ">" "&gt;"))
      s)))

(defun lyskom-docgen-get-name (fn lang)
  (let ((lyskom-language lang))
    (lyskom-command-name fn)))

(defun lyskom-docgen-get-doc (fn)
  (let ((doc (documentation fn)))
    (when (and doc (string-match "Lisp documentation:" doc))
      (setq doc (substring doc 0 (match-beginning 0))))
    (lyskom-docgen-transform-refs fn (lyskom-docgen-fixup-sgml doc))))

(defun lyskom-docgen-transform-refs (fn doc)
  (if doc
      (let ((result "")
            (start 0))
        (while (string-match "`\\(.*?\\)'" doc start)
          (setq result (concat result (substring doc start (match-beginning 0))))
          (let ((sym (intern (match-string 1 doc))))
            (cond 
             ((assq sym lyskom-docgen-external-refs)
              (setq result
                    (concat result
                            (format "<xref linkend=\"%s\" />" 
                                    (cdr (assq sym lyskom-docgen-external-refs))))))
             ((fboundp sym)
              (unless (memq sym lyskom-commands)
                (lyskom-docgen-error "%s: reference to non-command %s" fn sym))
              (setq result
                    (concat result
                            (format "<link linkend=\"fn:%s\"><command>%s</command></link> [<link linkend=\"fn:%s\"><function>%s</function></link>]"
                                    sym
                                    (lyskom-docgen-get-name fn 'en)
                                    sym
                                    sym)))
              (setq lyskom-docgen-xrefd-functions 
                    (add-to-list 'lyskom-docgen-xrefd-functions sym)))
             ((boundp sym)
              (setq result
                    (concat result
                            (format "<link linkend=\"var:%s\">%s</link>"
                                    sym sym)))
              (setq lyskom-docgen-xrefd-variables 
                    (add-to-list 'lyskom-docgen-xrefd-variables sym)))
             (t (lyskom-docgen-error "%s: undefined reference to %s" fn sym)
                (setq result (concat result 
                                     "`"
                                     (match-string 1 doc)
                                     "'"))))
            (setq start (match-end 0))))
        (setq result (concat result (substring doc start)))
        (concat "      <para>\n"
                (replace-in-string result "\n\n+" "\n      </para>\n      <para>\n")
                "\n      </para>\n")
        )
    (lyskom-docgen-error "%s: No documentation!" fn)
    "<para>Undocumented.</para>"))

(defun lyskom-docgen-format-key-sequence (binding)
  (mapconcat
   (lambda (x) 
     (format "<keycap>%s</keycap>" 
             (lyskom-docgen-fixup-sgml (key-description x))))
   binding "; "))


(defun lyskom-docgen-generate-commands-chapter-2 ()
  (lyskom-traverse command lyskom-commands
    (let* ((name-sv (lyskom-docgen-get-name command 'sv))
           (name-en (lyskom-docgen-get-name command 'en))
           (bind-sv (where-is-internal command lyskom-sv-mode-map))
           (bind-en (where-is-internal command lyskom-en-mode-map))
           (doc-en (lyskom-docgen-get-doc command)))
      (lyskom-docgen-princ
       (format "\
    <section id=\"fn:%s\"><title>%s [%s]</title>

    <informaltable frame=\"topbot\">
      <tgroup cols=\"2\">
        <tbody valign=\"top\">
          <row>
            <entry>English name</entry>
            <entry>%s</entry>
          </row>
          <row>
            <entry>English binding</entry>
            <entry>%s</entry>
          </row>
          <row>
            <entry>Swedish name</entry>
            <entry>%s</entry>
          </row>
          <row>
            <entry>Swedish binding</entry>
            <entry>%s</entry>
          </row>
        </tbody>
      </tgroup>
    </informaltable>
%s
  </section>\n\n" 
               command 
               name-en
               command 
               name-en
               (lyskom-docgen-format-key-sequence bind-en)
               name-sv
               (lyskom-docgen-format-key-sequence bind-sv)
               doc-en))
      (setq lyskom-docgen-found-functions
            (cons command lyskom-docgen-found-functions)))))

(defun lyskom-docgen-generate-missing-anchors ()
  (lyskom-docgen-princ "<section><title>Dummy anchors</title><para>\n")
  (lyskom-traverse fn (set-difference lyskom-docgen-xrefd-functions
                                      lyskom-docgen-found-functions)
    (lyskom-docgen-princ
     (format "<anchor id=\"fn:%s\" />Missing target: %s; \n" fn fn)))

  (lyskom-traverse var (set-difference lyskom-docgen-xrefd-variables
                                       lyskom-docgen-found-variables)
        (lyskom-docgen-princ
         (format "<anchor id=\"var:%s\" />Missing target: %s; \n" var var)))
  (lyskom-docgen-princ "</para></section>\n"))



(defun lyskom-docgen-open-file (fname)
  (set-buffer (get-buffer-create (format "*LysKOM DOCGEN* %s" fname)))
  (set-language-environment 'latin-1)
  (set-buffer-multibyte nil)
  (erase-buffer)
  (make-local-variable 'lyskom-docgen-fname)
  (make-local-variable 'lyskom-docgen-errors)
  (setq lyskom-docgen-errors nil)
  (setq lyskom-docgen-fname fname))

(defun lyskom-docgen-close-file ()
  (write-file lyskom-docgen-fname))


(defun lyskom-docgen-generate-fnc-ent ()
  (message "Generating fnc.ent")
  (lyskom-docgen-open-file "fnc.ent")
  (mapatoms
   (lambda (x)
     (when (and (fboundp x)
                (commandp x)
                (or (string-match "^kom-" (symbol-name x))
                    (string-match "^lyskom-" (symbol-name x))))
       (lyskom-princ
        (format "<!ENTITY fn:%s \"<function>%s</function>\">\n" x x)))))
  (lyskom-docgen-close-file))

(defun lyskom-docgen-generate-var-ent ()
  (message "Generating var.ent")
  (lyskom-docgen-open-file "var.ent")
  (mapatoms
   (lambda (x)
     (when (and (boundp x)
                (or (string-match "^kom-" (symbol-name x))
                    (string-match "^lyskom-" (symbol-name x)))
                (string-match "^\\*\\*"
                              (or (documentation-property x 'variable-documentation) "")))
       (lyskom-princ
        (format "<!ENTITY var:%s \"<varname>%s</varname>\">\n" x x)))))
  (lyskom-docgen-close-file))



(defun lyskom-docgen-generate-commands-chapter ()
  (message "Generating commands.xml")
  (lyskom-docgen-open-file "commands.xml")
  (lyskom-docgen-princ "
<?xml version='1.0' encoding='iso-8859-1' ?>
<!DOCTYPE book PUBLIC \"-//OASIS//DTD DocBook XML V4.2//EN\"
               \"docbook/dtd/xml/4.2/docbookx.dtd\">
<book lang=\"en\">
  <chapter>
    <title>Command Reference</title>

    <para>
      This chapter describes all commands in LysKOM. Commands are
      listed in alphabetical order by english name.
    </para>
")

  (lyskom-docgen-generate-commands-chapter-2)
  (lyskom-docgen-generate-missing-anchors)

  (lyskom-docgen-princ "\
<section id=\"lyskom-read-text-no-prefix-arg\">
  <title>Dummy</title>
  <para>Dummy</para>
</section>\n")
  (lyskom-docgen-princ "</chapter>
</book>")
  (lyskom-docgen-close-file))


(defmacro lyskom-docgen-run (stage fn)
  `(unwind-protect
       (funcall ,fn)
     (lyskom-docgen-report-errors ,stage)))

(defun lyskom-docgen ()
  (lyskom-docgen-run "commands.xml" 'lyskom-docgen-generate-commands-chapter)
  (lyskom-docgen-run "fnc.ent" 'lyskom-docgen-generate-fnc-ent)
  (lyskom-docgen-run "var.ent" 'lyskom-docgen-generate-var-ent)
  )


