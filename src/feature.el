;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: feature.el,v 1.6 2003-01-05 21:37:06 byers Exp $
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
;;;; File: feature.el
;;;;
;;;; Functions for determining if we have a particular feature or not
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: feature.el,v 1.6 2003-01-05 21:37:06 byers Exp $\n"))

(def-kom-var lyskom-feature-defaults nil
  "Alist with default values of all feature variables")

(eval-and-compile
  (defun lyskom-feature-variable (name)
    (intern (format "lyskom-%s-feature-flag" name))))

(defmacro lyskom-define-feature (name default)
  "Define a feature variable NAME with default value DEFAULT"
  `(progn (setq lyskom-feature-defaults 
                (cons (cons ',name ,default) lyskom-feature-defaults))
          (def-kom-var ,(lyskom-feature-variable name) ,default
            "LysKOM feature flag variable."
            inherited minibuffer local)))

(defmacro lyskom-set-feature (name value)
  "Set the value of feature NAME to VALUE"
  `(setq ,(lyskom-feature-variable name) ,value))

(defmacro lyskom-have-feature (name)
  "Return non-nil if feature NAME is non-nil"
  (lyskom-feature-variable name))

(defmacro lyskom-feature-value (name)
  "Return the value of feature NAME"
  (lyskom-feature-variable name))

(defun lyskom-clear-features ()
  "Set the feature variables to their defaults"
  (mapcar (lambda (feature)
            (set (lyskom-feature-variable (car feature)) (cdr feature)))
          lyskom-feature-defaults))

;;UNUSED: lyskom-list-features
(defun lyskom-list-features ()
  "List all feature values. mapcar ROCKS!"
  (mapcar 'lyskom-insert-before-prompt
          (mapcar (lambda (x) (apply 'format "%S: %S\n" x))
                  (mapcar2 'list
                           (mapcar 'car lyskom-feature-defaults)
                           (mapcar 'symbol-value 
                                   (mapcar 'lyskom-feature-variable
                                           (mapcar 'car lyskom-feature-defaults)))))))

;;; ======================================================================
;;; ======================================================================

;;; Utility functions

(defmacro lyskom-have-call (call-no)
  "Non-nil if call CALL-NO is supported by the server"
  `(<= ,call-no (lyskom-feature-value highest-call)))


;;; ======================================================================
;;; ======================================================================

;;; The features

(lyskom-define-feature bcc-misc nil)
(lyskom-define-feature aux-items nil)
(lyskom-define-feature highest-call 0)
(lyskom-define-feature local-to-global nil)
(lyskom-define-feature dynamic-session-info nil)
(lyskom-define-feature idle-time nil)
(lyskom-define-feature long-conf-types nil)


(provide 'lyskom-feature)
