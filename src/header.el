;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: header.el,v 44.2 2002-02-24 20:23:27 joel Exp $
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
;;;; File: header.el
;;;;
;;;; Code that needs to go before everything else
;;;;


;;; Check the version of Emacs when compiling
;;;
;;; If the version of Emacs is too old, signal an error.
;;; We require Gnu Emacs 19.34 or 20.3 or XEmacs 21.1.

(eval-and-compile
  (if (or (not (boundp 'emacs-major-version))
          (not (boundp 'emacs-minor-version))
          (and (string-match "XEmacs" (emacs-version))
               (or (< emacs-major-version 20)
                   (and (= emacs-major-version 20)
                        (< emacs-minor-version 4))
                   (and (= emacs-major-version 21)
                        (< emacs-minor-version 1))))
          (and (not (string-match "XEmacs" (emacs-version)))
               (or (< emacs-major-version 19)
                   (and (= emacs-major-version 19)
                        (< emacs-minor-version 34))
                   (and (= emacs-major-version 20)
                        (< emacs-minor-version 2)))))
      (error "Emacs 19.34, XEmacs 21.1 or later, or Emacs 20.2 or later is required")))

(cond ((or (not (boundp 'emacs-major-version))
           (not (boundp 'emacs-minor-version))
           (< emacs-major-version (eval-when-compile emacs-major-version)))
       (error "LysKOM was compiled for a newer version of Emacs"))
      ((and (eq (eval-when-compile emacs-major-version) 19)
            (> emacs-major-version 19))
       (error "A LysKOM client compiled for Emacs 19 will not run in newer versions of Emacs.")))
