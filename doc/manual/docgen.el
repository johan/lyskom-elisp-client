;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: docgen.el,v 1.2 2005-01-09 01:17:59 byers Exp $
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
;;;; This should be run in Gnu Emacs 21.4. It works OK in 21.3 too,
;;;; but fails to generate all the key bindings.


;;; TO DO
;;;

;;; Extract documentation from edit-text. All commands with the prefix
;;; kom-edit- whose docstrings do not end in "Excluded from manual."
;;; should be extracted and put into entities like the regular
;;; commands. Then create another entity that we can use to insert
;;; them into the texts.xml file.

;;; Something like this should be done for mship-edit too.

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

;;; Read templates from a file, so we don't hard-code the
;;; format here.

;;; Specify output file on the command line.

;;; Specify lyskom.elc on the command line.

;;; Read a list of external references from a file. Ideally we'd read
;;; and parse the actual XML, but that is probably too much work. We
;;; need to map references like lyskom-read-text-no-prefix-arg to the
;;; actual ID to link to. Specify this file on the command line too.

;;; Do variable documentation.



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
    (lyskom-command-name fn lang)))

(defun lyskom-docgen-function-doc (fn)
  (let ((doc (documentation fn)))
    (when (and doc (string-match "\\`\\*" doc))
      (setq doc (substring doc 1)))
    (when (and doc (string-match "Lisp documentation:" doc))
      (setq doc (substring doc 0 (match-beginning 0))))
    doc))

(defun lyskom-docgen-variable-doc (var)
  (let ((doc (documentation-property var 'variable-documentation t)))
    (and (stringp doc) (string-match "\\`\\*\\*" doc) (substring doc 2))))


(defun lyskom-docgen-where-is-internal (command lang &optional map)
  (let* ((map (or map (symbol-value (intern (format "lyskom-%s-mode-map" lang)))))
         (trans (when (eq lang 'sv) `((?\] . ,(kbd "å")) 
                                      (?\} . ,(kbd "å")) 
                                      (Å . ,(kbd "å"))
                                      (å . ,(kbd "å"))
                                      (3909 . ,(kbd "å"))
                                      (3941 . ,(kbd "å"))
                                      (2277 . ,(kbd "å"))
                                      (2245 . ,(kbd "å"))
                                      (aring . ,(kbd "å"))
                                      (Aring . ,(kbd "å"))
                                      (229 . ,(kbd "å"))
                                      (197 . ,(kbd "å"))

                                      (134217733 . ,(kbd "C-å"))
                                      (C-Aring . ,(kbd "C-å"))
                                      (C-aring . ,(kbd "C-å"))
                                      (67111109 . ,(kbd "C-å"))
                                      (67111141 . ,(kbd "C-å"))
                                      (67108989 . ,(kbd "C-å"))
                                      (,(kbd "C-}") . ,(kbd "C-å"))

                                      (?\[ . ,(kbd "ä")) 
                                      (?\{ . ,(kbd "ä"))
                                      (Ä . ,(kbd "ä"))
                                      (ä . ,(kbd "ä"))
                                      (228 . ,(kbd "ä"))
                                      (196 . ,(kbd "ä"))
                                      (3908 . ,(kbd "ä"))
                                      (3940 . ,(kbd "ä")) 
                                      (2276 . ,(kbd "ä"))
                                      (2244 . ,(kbd "ä"))
                                      (Adiaeresis . ,(kbd "ä"))
                                      (adiaeresis . ,(kbd "ä"))))))

         (delete-duplicates (mapcar (lambda (binding)
                                      (apply 'vector 
                                             (apply 'append
                                                    (mapcar (lambda (x) 
							      (cond ((listp x) x)
								    ((vectorp x) (append x nil))
								    (t (list x))))
                                                            (mapcar (lambda (key) 
                                                                      (or (cdr (assoc key trans)) key))
                                                                    binding)))))
                                    (where-is-internal command map))
                            :test 'equal)))

(defun lyskom-docgen-fixup-key-description (desc)
  (replace-in-string desc "\\(C-[A-Za-z0-9]+\\) TAB " "\\1 C-i "))

(defun lyskom-docgen-format-key-sequence (binding)
  (if binding
      (mapconcat
       (lambda (x) 
         (format "<keycap>%s</keycap>" 
                 (lyskom-docgen-fixup-sgml
		  (lyskom-docgen-fixup-key-description
		   (key-description x)))))
       binding "; ")
    "<emphasis>unbound</emphasis>"))

(defmacro lyskom-docgen-parse-docstring-error ()
  `(let ((msg (mapconcat 'identity (cons str lines) "\n")))
     (error "Parse error in docstring. State: %s. Near: %s" 
            state (substring msg 0 (min (length msg) 79)))))

(defun lyskom-docgen-parse-docstring (str)
  (when (string-match "^\\s-*\\**" str)
    (setq str (substring str (match-end 0))))
  (let* ((result nil)
         (state 'initial)
         (lines (split-string str "\n"))
         (saved-lines lines)
         (str nil)
         (current nil))
    (while lines
      (setq str (car lines) lines (cdr lines))
      (cond
       ((eq state 'initial)
        (setq current nil)
        (cond
         ((string-match "^\\*" str) (setq state 'ul lines (cons str lines)))
         ((string-match "^\\s-*<\\s-*$" str) (setq state 'pre))
         ((string-match "^\\S-" str) (setq state 'para lines (cons str lines)))
         ((string-match "^\\s-+\\S-" str) (setq state 'table lines (cons str lines)))
         ((string-match "^\\s-*$" str))
         (t (lyskom-docgen-parse-docstring-error))))

       ;; An ul block is
       ;; (ul item item item item)
       ;; the items are in reverse order

       ((eq state 'ul)
        (unless (eq 'ul (car current)) 
          (setq current (cons 'ul nil))
          (setq result (cons current result)))
        (cond
         ((string-match "^\\s-*$" str))
         ((string-match "^\\*\\s-*" str) 
          (setq state 'li lines (cons str lines)))
         (t (setq state 'initial lines (cons str lines)))))

       ((eq state 'li)
        (cond
         ((string-match "^\\s-*$" str) (setq state 'ul))
         ((string-match "^\\*\\s-*" str)
          (setcdr current (cons (substring str (match-end 0)) (cdr current))))
         (t (setcar (cdr current) (concat (car (cdr current)) " " str)))))

       ;; a para block is (para . string)

       ((eq state 'para)
        (unless (eq 'para (car current))
          (setq current (cons 'para nil))
          (setq result (cons current result)))
        (cond
         ((string-match "^\\s-*$" str) (setq state 'initial))
         ((string-match "^\\*" str) (setq state 'initial))
         (t (setcdr current (cons str (cdr current))))))

       ;; a pre block is (pre . lines)

       ((eq state 'pre)
        (unless (eq 'pre (car current))
          (setq current (cons 'pre nil))
          (setq result (cons current result)))
        (cond ((string-match "^\\s-*>\\s-*$" str) (setq state 'initial))
              (t (setcdr current (cons str (cdr current))))))

       ;; a table block is (table rows head)

       ((eq state 'table)
        (unless (eq 'table (car current))
          (setq current (cons 'table nil))
          (setq result (cons current result)))
        (setq lines (cons str lines) state 'thead))

       ((eq state 'thead)
        (setcdr current (cons (cdr (string-split "\\s-\\s-+" str))
                              (cdr current)))
        (setq state 'thsep))

       ((eq state 'thsep)
        (unless (string-match "\\s-*-+\\s-*$" str)
          (lyskom-docgen-parse-docstring-error))
        (setq state 'tbody))

       ((eq state 'tbody)
        (cond
         ((string-match "^\\s-*$" str))
         ((string-match "^\\s-*-+\\s-*$" str) (setq state 'initial))
         (t (setq state 'trow lines (cons str lines)))))

       ((eq state 'trow)
        (setcdr current (cons (cdr (string-split "\\s-\\s-+" str))
                              (cdr current)))
        (setq state 'trow2))

       ((eq state 'trow2)
        (cond 
         ((string-match "^\\s-*$" str) (setq state 'tbody))
         ((string-match "^\\s-*-+\\s-*$" str) (setq state 'initial))
         (t (let ((cells (nreverse (cdr (string-split "\\s-\\s-+" str)))))
              (setcar (cdr current)
                      (nreverse
                       (mapcar (lambda (cell)
                                 (prog1
                                     (concat cell " " (or (car cells) ""))
                                   (setq cells (cdr cells))))
                               (nreverse (car (cdr current))))))))))

       (t (error "Unknown state in docstring parser: %s" state))


       ))

    result

   (mapcar (lambda (el)
              (cond ((eq (car el) 'pre)
                     (setcdr el (mapconcat 'identity (nreverse (cdr el)) "\n")))
                    ((eq (car el) 'para)
                     (setcdr el (mapconcat 'identity (nreverse (cdr el)) " ")))
                    ((listp (cdr el))
                     (setcdr el (nreverse (cdr el)))))
              el)
            (nreverse result))

))

(defun lyskom-docgen-docstring-to-docbook (sym str)
  (if str
      (let ((parse (lyskom-docgen-parse-docstring str)))
        (mapconcat 'identity
                   (mapcar
                    (lambda (el)
                      (cond ((eq (car el) 'para)
                             (concat "<para>\n"
                                     (lyskom-docgen-markup-paragraph
                                      sym (lyskom-docgen-fixup-sgml (cdr el)))
                                     "\n</para>"))

                            ((eq (car el) 'ul)
                             (concat "<itemizedlist mark=\"bullet\">\n"
                                     (mapconcat (lambda (item)
                                                  (concat "<listitem>\n"
                                                          "  <para>\n"
                                                          (lyskom-docgen-markup-paragraph
                                                           sym (lyskom-docgen-fixup-sgml item))
                                                          "\n  </para>"
                                                          "\n</listitem>"))
                                                (cdr el)
                                                "\n")
                                     "\n</itemizedlist>"))

                            ((eq (car el) 'table)
                             (concat "<informaltable>\n"
                                     (format "  <tgroup cols=\"%d\">\n" (length (car (cdr el))))
                                     "    <thead>\n"
                                     "      <row>\n"
                                     (mapconcat (lambda (x)
                                                  (concat "        <entry>"
                                                          (lyskom-docgen-markup-paragraph
                                                           sym (lyskom-docgen-fixup-sgml x))
                                                          "</entry>"))
                                                (car (cdr el))
                                                "\n")
                                     "\n      </row>"
                                     "\n    </thead>\n"
                                     "    <tbody>\n"
                                     (mapconcat (lambda (row)
                                                  (concat "      <row>\n"
                                                          (mapconcat (lambda (x)
                                                                       (concat "        <entry>"
                                                                               (lyskom-docgen-markup-paragraph
                                                                                sym (lyskom-docgen-fixup-sgml x))
                                                                               "</entry>"))
                                                                     row "\n")
                                                          "\n    </row>"))
                                                (cdr (cdr el))
                                                "\n")
                                     "\n    </tbody>"
                                     "\n  </tgroup>"
                                     "\n</informaltable>")
                             )

                            ((eq (car el) 'pre)
                             (concat "<screen>\n"
                                     (lyskom-docgen-fixup-sgml (cdr el))
                                     "\n</screen>"))

                            (t (error "Unknown docstring parse element: %s" el))))
                    parse)
                   "\n"))

    (lyskom-docgen-error "%s: No documentation!" fn)
    "<para>Undocumented</para>"))


(defun lyskom-docgen-markup-paragraph (docsym doc)
  (let ((result "")
        (start 0)
        (end nil))
    (while (string-match "`\\(.*?\\)'" doc start)
      (setq result (concat result (substring doc start (match-beginning 0)))
            end (match-end 0))
      (let* ((matched-string (match-string 1 doc))
             (sym (intern matched-string)))
        (cond 

         ;; External reference

         ((assq sym lyskom-docgen-external-refs)
          (setq result
                (concat result
                        (format "<xref linkend=\"%s\" />" 
                                (cdr (assq sym lyskom-docgen-external-refs))))))

         ;; Reference to a LysKOM function or variable

         ((string-match "^\\(lys\\)?kom-" matched-string)
          (cond ((fboundp sym)
                 (setq lyskom-docgen-xrefd-functions (add-to-list 'lyskom-docgen-xrefd-functions sym))
                 (setq result
                       (concat result
                               (format "<link linkend=\"fn:%s\"><command>%s</command></link>" sym sym)
                               (if (memq sym lyskom-commands)
                                   (format " (<link linkend=\"fn:%s\"><command>%s</command></link>)"
                                           sym (lyskom-docgen-get-name sym 'en))
                                 ""))))
                ((boundp sym)
                 (setq lyskom-docgen-xrefd-variables (add-to-list 'lyskom-docgen-xrefd-variables sym))
                 (setq result
                       (concat result
                               (format "<link linkend=\"var:%s\"><varname>%s</varname></link>" sym sym))))

                (t
                 (lyskom-docgen-error "%s: reference to undefined name %s" docsym sym)
                 (setq result
                       (concat result
                               (format "<literal>%s</literal>" sym))))))

         ;; All uppercase means a replaceable string

         ((string-match "^[-_:A-Z]+$" matched-string)
          (setq result (concat result (format "<replaceable>%s</replaceable>" matched-string))))

         ;; Everything else is a literal

         (t (setq result (concat result (format "<literal>%s</literal>" matched-string))))

         )

        (setq start end)))

    (setq result (concat result (substring doc start)))))


(defun lyskom-docgen-generate-missing-anchors ()
  (message "Generating dummy.xml")
  (lyskom-docgen-open-file "dummy.xml")
  (lyskom-docgen-princ "<book>
<chapter id=\"dummy\"><title>Dummy anchors</title><para>\n")
  (lyskom-traverse fn (set-difference lyskom-docgen-xrefd-functions
                                      lyskom-docgen-found-functions)
    (lyskom-docgen-princ
     (format "<anchor id=\"fn:%s\" />Missing target: %s; \n" fn fn)))

  (lyskom-traverse var (set-difference lyskom-docgen-xrefd-variables
                                       lyskom-docgen-found-variables)
        (lyskom-docgen-princ
         (format "<anchor id=\"var:%s\" />Missing target: %s; \n" var var)))
  (lyskom-docgen-princ "</para></chapter>
</book>\n")
  (lyskom-docgen-close-file))



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
       (lyskom-princ (format "<!ENTITY fn:%s \"<function>%s</function>\">\n" x x)))))
  (lyskom-docgen-close-file))

(defun lyskom-docgen-generate-var-ent ()
  (message "Generating var.ent")
  (lyskom-docgen-open-file "var.ent")
  (mapatoms
   (lambda (x)
     (when (and (boundp x)
                (or (string-match "^kom-" (symbol-name x))
                    (string-match "^lyskom-" (symbol-name x)))
                (lyskom-docgen-variable-doc x))
       (lyskom-princ (format "<!ENTITY var:%s \"<varname>%s</varname>\">\n" x x)))))
  (lyskom-docgen-close-file))


(defun lyskom-docgen-generate-editcmd-ent ()
  (message "Generating editcmd.ent")
  (lyskom-docgen-open-file "editcmd.ent")
  (lyskom-docgen-princ "\
<variablelist>
  <title>Edit mode commands</title>
    ")
  (mapatoms
   (lambda (x)
     (let (doc)
       (when (and (fboundp x)
                  (commandp x)
                  (string-match "^kom-edit-" (symbol-name x))
                  (setq doc (lyskom-docgen-function-doc x))
                  (not (string-match "Excluded from manual" doc)))
         (lyskom-docgen-princ
          (format "\
  <varlistentry id=\"fn:%s\">
    <term><command>%s</command></term>
    <listitem>
%s
      <formalpara>
        <title>Bindings</title>
        <para>Swedish: %s; English: %s</para>
      </formalpara>
    </listitem>
  </varlistentry>
    "
                  x x
                  (lyskom-docgen-docstring-to-docbook x doc)
                  (lyskom-docgen-format-key-sequence
                   (lyskom-docgen-where-is-internal x 'sv lyskom-sv-edit-mode-map))
                  (lyskom-docgen-format-key-sequence
                   (lyskom-docgen-where-is-internal x 'en lyskom-en-edit-mode-map))))
      (setq lyskom-docgen-found-functions
            (cons x lyskom-docgen-found-functions))))))
  (lyskom-docgen-princ "</variablelist>")
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

  (lyskom-docgen-princ "\
<section id=\"lyskom-read-text-no-prefix-arg\">
  <title>Dummy</title>
  <para>Dummy</para>
</section>\n")
  (lyskom-docgen-princ "</chapter>
</book>")
  (lyskom-docgen-close-file))

(defun lyskom-docgen-generate-commands-chapter-2 ()
  (lyskom-traverse command lyskom-commands
    (let* ((name-sv (lyskom-docgen-get-name command 'sv))
           (name-en (lyskom-docgen-get-name command 'en))
           (bind-sv (lyskom-docgen-where-is-internal command 'sv))
           (bind-en (lyskom-docgen-where-is-internal command 'en))
           (doc-en (lyskom-docgen-function-doc command)))
      (lyskom-docgen-princ
       (format "\
    <section id=\"fn:%s\"><title>%s [%s]</title>

    <informaltable frame=\"topbot\">
      <tgroup cols=\"3\">
        <tbody valign=\"top\">
          <row>
            <entry>English:</entry>
            <entry>%s</entry>
            <entry>%s</entry>
          </row>
          <row>
            <entry>Swedish:</entry>
            <entry>%s</entry>
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
               (lyskom-docgen-docstring-to-docbook command doc-en)))
      (setq lyskom-docgen-found-functions
            (cons command lyskom-docgen-found-functions)))))



(defun lyskom-docgen-generate-variables-chapter ()
  (message "Generating variables.xml")
  (lyskom-docgen-open-file "variables.xml")
  (lyskom-docgen-princ "
<?xml version='1.0' encoding='iso-8859-1' ?>
<!DOCTYPE book PUBLIC \"-//OASIS//DTD DocBook XML V4.2//EN\"
               \"docbook/dtd/xml/4.2/docbookx.dtd\">
<book lang=\"en\">
  <chapter>
    <title>Variable Reference</title>

    <para>
      This chapter describes all user-settable variables in LysKOM. 
      Variables are listed in alphabetical order name.
    </para>
")

  (lyskom-docgen-generate-variables-chapter-2)

  (lyskom-docgen-princ "</chapter>\n</book>")
  (lyskom-docgen-close-file))

(defun lyskom-docgen-generate-variables-chapter-2 ()
  (let (data)
    (mapatoms (lambda (x)
                (let ((doc (lyskom-docgen-variable-doc x)))
                  (when doc (setq data (cons (cons x doc) data))))))
    (lyskom-traverse el (nreverse data)
      (let ((var (car el))
            (doc (cdr el)))
        (lyskom-docgen-princ 
         (format "\
    <section id=\"var:%s\"><title>%s</title>

%s
    </section>\n\n"
                 var var
                 (lyskom-docgen-docstring-to-docbook var doc)))
        (setq lyskom-docgen-found-variables 
              (cons var lyskom-docgen-found-variables))))))



(defmacro lyskom-docgen-run (stage fn)
  `(unwind-protect
       (funcall ,fn)
     (lyskom-docgen-report-errors ,stage)))

(defun lyskom-docgen ()
  (lyskom-docgen-run "variables.xml" 'lyskom-docgen-generate-variables-chapter)
  (lyskom-docgen-run "commands.xml" 'lyskom-docgen-generate-commands-chapter)
  (lyskom-docgen-run "editcmd.ent" 'lyskom-docgen-generate-editcmd-ent)
  (lyskom-docgen-run "fnc.ent" 'lyskom-docgen-generate-fnc-ent)
  (lyskom-docgen-run "var.ent" 'lyskom-docgen-generate-var-ent)
  (lyskom-docgen-run "dummy.xml" 'lyskom-docgen-generate-missing-anchors)
  )


