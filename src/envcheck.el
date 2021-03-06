;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: envcheck.el,v 1.8 2002-02-24 20:23:27 joel Exp $
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
;;;; File: envcheck.el
;;;;
;;;; Check that the compile-time environment for LysKOM looks OK
;;;;


(if (not (fboundp 'when))
    (defmacro when (cond &rest body)
      (list 'if cond (cons 'progn body))))

(if (not (fboundp 'unless))
    (defmacro unless (cond &rest body)
      (append (list 'if cond nil) body)))

(defun lyskom-locate-library (library &optional nosuffix path interactive-call)
  "Show the precise file name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY.

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'."
  (interactive (list (read-string "Locate library: ")
                     nil nil
                     t))
  (let (result)
    (catch 'answer
      (mapcar
       (lambda (dir)
         (mapcar
          (lambda (suf)
            (let ((try (expand-file-name (concat library suf) dir)))
              (and (file-readable-p try)
                   (null (file-directory-p try))
                   (progn
                     (setq result try)
                     (throw 'answer try)))))
          (if nosuffix
              '("")
            (let ((basic '(".elc" ".el" ""))
                  (compressed '(".Z" ".gz" "")))
              ;; If autocompression mode is on,
              ;; consider all combinations of library suffixes
              ;; and compression suffixes.
              (if (rassq 'jka-compr-handler file-name-handler-alist)
                  (apply 'nconc
                         (mapcar (lambda (compelt)
                                   (mapcar (lambda (baselt)
                                             (concat baselt compelt))
                                           basic))
                                 compressed))
                basic)))))
       (or path load-path)))
    (and interactive-call
         (if result
             (message "Library is file %s" result)
           (message "No library %s in search path" library)))
    result))

(defun lyskom-check-environment ()
  (let ((have-custom (lyskom-locate-library "custom"))
        (have-widget (lyskom-locate-library "widget"))
        (have-wid-edit (lyskom-locate-library "wid-edit"))
        (have-wid-browse (lyskom-locate-library "wid-browse"))
        (have-cus-edit (lyskom-locate-library "cus-edit"))
        (have-cus-face (lyskom-locate-library "cus-face"))
        (have-w3 (lyskom-locate-library "w3"))
        (have-calc (lyskom-locate-library "calc"))
        (fatal nil))

    (message "\

===============================================================================
                Checking the Emacs environment for LysKOM.
")

    ;;
    ;; Check the custom library
    ;;

    (catch 'terminate
      (when have-custom
        (load-library "custom")
        (when (not (fboundp 'custom-declare-group))
          (message "Checking custom and widget...error

--- Antique version the custom library detected (FATAL)

You have an old version of the custom library in your load path that is
incompatible with LysKOM, and with recent versions of Gnus and w3. The
reason for this is probably that you have a version of Gnus in the load
path that used an earlier version of the library. To rectify this situation,
you could install a new version of Gnus with the new custom libraries, or
make sure that the Gnus installation is not in the default load path,
or that all users of LysKOM prepend a directory containing the new version
of custom to their load path.

You can get a copy of the most recent version of the custom library from
http://www.dina.kvl.dk/~abraham/custom/
")

          (setq fatal t)
          (throw 'terminate nil))
        (when (not (and have-widget
                        have-wid-edit
                        have-wid-browse
                        have-cus-edit
                        have-cus-face))
          (message "Checking custom and widget...error

--- Parts of the custom library appear to be missing (FATAL)

It looks like you have a recent version of custom, but some of the
files are missing (see the report below.) It is possible that you have
a fairly recent version of custom, but with other file names (for
example, wid-edit.el used to be widget-edit.el.) If that is the case,
you should get a new copy of the library. You can download the custom
library from http://www.dina.kvl.dk/~abraham/custom/
")
          (message "  custom %s"  (if have-custom "found" "not found"))
          (message "  cus-edit %s" (if have-cus-edit "found"
                                            "not found"))
          (message "  cus-face %s" (if have-cus-face "found" 
p                                            "not found"))
          (message "  widget %s" (if have-widget "found" "not found"))
          (message "  wid-edit %s" (if have-wid-edit "found"
                                          "not found"))
          (message "  wid-browse %s" (if have-wid-browse "found"
                                          "not found"))
          (message "")
          (setq fatal t)
          (throw 'terminate nil))

        (message "Checking custom and widget...OK"))
      (unless have-custom
        (message "Checking custom and widget...error

--- The custom library is not in the default load path (FATAL)

For LysKOM to work you need to install the custom library, a library
that implements a reasonably user-friendly configuration interface to
Emacs. Note that modern versions of custom are incompatible with the
early version distributed with some versions of Gnus. Recent versions
of Gnus use the new library. You can download a copy of the custom
library from http://www.dina.kvl.dk/~abraham/custom/
")
        (setq fatal t)))

    ;;
    ;; Check W3
    ;;

    (catch 'terminate
      (if have-w3
          (progn
            (load-library "w3")
            (if (not (fboundp 'w3-region))
                (message "Checking w3...error

--- Antique version of w3 detected (WARNING)

You have a version of w3 installed that is of no use to LysKOM. Although
LysKOM will work, it will be unable to format articles containing HTML.
You may want to upgrade your w3 to version 3.something.
")

              (message "Checking w3...OK")))
        (message "Checking w3...error

--- Didn't find w3 at all (WARNING)

Without w3 LysKOM will be unable to format articles containing HTML code.
You may want to install w3 too.
")))

    ;;
    ;; Check calc
    ;;

    (catch 'terminate
      (if have-calc
          (progn
            (load-library "calc")
            (if (not (fboundp 'calc-eval))
                (message "Checking calc...error

--- Antique version of calc (WARNING)

You have a version of calc that does not include the function calc-eval, 
which LysKOM uses. LysKOM will still work, but the Calculate command 
won't. 
")
              (message "Checking calc...OK")))
        (message "Checking calc...error

--- Didn't find calc at all (WARNING)

Without calc LysKOM's Calculate command will not work. You may want to
get and install calc.
")))



    ;;
    ;; Summary report
    ;;

    (if fatal
        (message "

FATAL ERRORS DETECTED!

Some of the errors detected are fatal. If you don't fix them LysKOM will
not work and might not even compile.
===============================================================================

")
      (message "
You should be able to compile and install LysKOM without too much trouble
===============================================================================
"))))

