;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: docgen.el,v 44.3 2003-01-09 01:45:01 byers Exp $
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

(defun lyskom-docgen-report-errors ()
  (when nil
    (lyskom-traverse msg (nreverse lyskom-docgen-errors)
      (message msg))))

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


(defun lyskom-docgen-2 ()
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



(defun lyskom-docgen ()
  (let ((buf (get-buffer-create "*LysKOM DOCGEN*")))
    (set-buffer buf)
    (set-language-environment 'latin-1)
    (set-buffer-multibyte nil)
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
    (let ((lyskom-docgen-errors nil))
      (unwind-protect
          (lyskom-docgen-2)
        (lyskom-docgen-report-errors)))
    (lyskom-docgen-generate-missing-anchors)
    (lyskom-docgen-princ "\
<section id=\"lyskom-read-text-no-prefix-arg\">
  <title>Dummy</title>
  <para>Dummy</para>
</section>\n")
    (lyskom-docgen-princ "</chapter></book>")
    (write-file "DOC.xml")))



(load-file "./lyskom.elc")


