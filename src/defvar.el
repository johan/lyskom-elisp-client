;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: defvar.el,v 44.9 1998-06-02 12:14:33 byers Exp $
;;;;; Copyright (C) 1991, 1996  Lysator Academic Computer Association.
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
;;;; ================================================================
;;;; ================================================================
;;;;
;;;; File: defvar.el
;;;; Authos: David Byers
;;;;
;;;; This file contains definitions used to define variables
;;;;


(defconst lyskom-clientversion-long 
  "$Id: defvar.el,v 44.9 1998-06-02 12:14:33 byers Exp $\n"
  "Version for every file in the client.")


(provide 'lyskom)

;; Just to get rid of a compiler warning
(defvar kom-dont-read-saved-variables)

(defvar lyskom-local-variables nil
  "List of variables to make local in a LysKOM buffer")

(defvar lyskom-local-hooks nil
  "List of hooks to make local in a LysKOM buffer.")

(defvar lyskom-protected-variables nil
  "List of variables that are protected from kill-buffer")

(defvar lyskom-inherited-variables nil
  "List of variables inherited from the LysKOM buffer")

(defvar lyskom-elisp-variables nil
  "Tells the client what flags and hooks that are to be saved in the server.
These are the flags that are saved in the elisp-client part of the server.")

(defvar lyskom-minibuffer-variables nil
  "These are variables that should be set in the minibuffer by 
lyskom-with-lyskom-minibuffer.")

(defvar lyskom-minibuffer-values nil
  "Dynamic binding of values that minibuffer variables are to take on")


(defmacro lyskom-save-variables (var-list &rest forms)
  "Save the values and property list of symbols in VAR-LIST and execute FORMS
The symbol value, property list and buffer-local property of all variables
is saved before executing FORMS and restored when FORMS have finished."
  (let ((sym1 (make-symbol "lyskom-saved-variables"))
        (sym2 (make-symbol "lyskom-saved-symbols"))
        (sym3 (make-symbol "lyskom-saved-local"))
        (sym4 (make-symbol "lyskom-saved-plist")))
    (` (let* (((, sym2) (quote (, var-list)))
              ((, sym1) (mapcar 'symbol-value (, sym2)))
              ((, sym4) (mapcar 'symbol-plist (, sym2)))
              ((, sym3) (mapcar (function
                                 (lambda (v)
                                   (local-variable-p v (current-buffer))))
                                 (, sym2))))
         (unwind-protect 
             (progn (,@ forms))
           (while (, sym1)
             (if (car (, sym3)) (make-local-variable (car (, sym2))))
             (set (car (, sym2)) (car (, sym1)))
             (setplist (car (, sym2)) (car (, sym4)))
             (setq (, sym1) (cdr (, sym1))
                   (, sym2) (cdr (, sym2))
                   (, sym3) (cdr (, sym3))
                   (, sym4) (cdr (, sym4)))))))))

(put 'lyskom-save-variables 'edebug-form-spec
     '(sexp body))

(defmacro lyskom-with-lyskom-minibuffer (&rest forms)
  "Run FORMS after ensuring that LysKOM minibuffer variables will be set."
  (` (let* ((lyskom-minibuffer-values
             (mapcar 'symbol-value lyskom-minibuffer-variables)))
       (unwind-protect
           (progn
             (add-hook 'minibuffer-setup-hook
                       'lyskom-setup-minibuffer-variables)
             (,@ forms))
         (remove-hook 'minibuffer-setup-hook
                      'lyskom-setup-minibuffer-variables)))))

(put 'lyskom-with-lyskom-minibuffer 'edebug-form-spec
     '(body))


(defun lyskom-setup-minibuffer-variables ()
  (let ((syms lyskom-minibuffer-variables)
        (vals lyskom-minibuffer-values))
    (while syms
      (make-local-variable (car syms))
      (set (car syms) (car vals))
      (setq syms (cdr syms)
            vals (cdr vals)))
    (set-buffer-multibyte nil)))

         

(defmacro def-kom-var (name value &rest args)
    "Define a variable with name NAME and initial value VALUE.
Remaining args, ARGS may be
A string        Used as the documentation string for the variable
A symbol        A predefined property of the variable
A list          A widget specification for the variable

Predefined properties are the following
server          Save the variable in the elisp block. Implies local.
local           Make the variable buffer-local.
inherited       The variable is inherited from parent buffer. Implies protected
protected       The variable is marked as permanent local. Implies local.
minibuffer      Inherit the variable as a local variable in the minibuffer.
server-hook     A hook stored in the server.
local-hook      A hook variable that is made local in LysKOM buffers."
          
    (let ((inherited nil)
          (protected nil)
          (elisp-block nil)
          (buffer-local nil)
          (widget-spec nil)
          (doc-string nil)
          (minibuffer nil)
          (arglist args))
      (while arglist
        (cond ((stringp (car arglist)) (setq doc-string (car arglist)))
              ((consp (car arglist))
               (setq widget-spec 
                     (` ((setq lyskom-custom-variables
                               (cons  (quote (, (list name
                                                      (car arglist))))
                                      lyskom-custom-variables))))))
              ((symbolp (car arglist))
               (cond ((eq (car arglist) 'server)
                      (setq elisp-block
                            (` ((if (and (not (memq (quote (, name))
                                                      lyskom-global-boolean-variables))
                                           (not (memq (quote (, name))
                                                      lyskom-global-non-boolean-variables)))
                                           (add-to-list 'lyskom-elisp-variables
                                                        (quote (, name))))
                                      (add-to-list 'lyskom-local-variables
                                                   (quote (, name)))))))

                     ((eq (car arglist) 'server-hook)
                      (setq elisp-block
                            (` ((add-to-list 'lyskom-elisp-variables
                                             (quote (, name)))
                                (add-to-list 'lyskom-local-hooks
                                             (quote (, name)))))))

                     ((eq (car arglist) 'protected)
                      (setq protected
                            (` ((put (quote (, name)) 'permanent-local t)
                                (add-to-list 'lyskom-protected-variables
                                             (quote (, name)))
                                (add-to-list 'lyskom-local-variables
                                             (quote (, name)))))))

                     ((eq (car arglist) 'inherited)
                      (setq inherited
                            (` ((add-to-list 'lyskom-inherited-variables
                                             (quote (, name)))
                                (put (quote (, name)) 'permanent-local t)
                                (add-to-list 'lyskom-protected-variables
                                             (quote (, name)))
                                (add-to-list 'lyskom-local-variables
                                             (quote (, name)))))))

                     ((eq (car arglist) 'local)
                      (setq buffer-local
                            (` ((add-to-list 'lyskom-local-variables
                                             (quote (, name)))))))

                     ((eq (car arglist) 'local-hook)
                      (setq buffer-local
                            (` ((add-to-list 'lyskom-local-hooks
                                             (quote (, name)))))))

                     ((eq (car arglist) 'minibuffer)
                      (setq minibuffer
                            (` ((add-to-list 'lyskom-minibuffer-variables
                                             (quote (, name)))))))

                     (t (error "LysKOM: Unknown variable property: %S"
                               (car arglist)))))
              (t (error "LysKOM: Strange variable argument type: %S" 
                        (car arglist))))
        (setq arglist (cdr arglist)))

      (` (progn (dont-compile (if (and (boundp (quote (, name)))
                                       (listp kom-dont-read-saved-variables))
                                  (add-to-list 'kom-dont-read-saved-variables
                                               (quote (, name)))))
                (defvar (, name) (, value) (, doc-string))
                (,@ (apply 'append
                           (list inherited
                                 protected
                                 elisp-block
                                 buffer-local
                                 minibuffer
                                 widget-spec
                                 )))))))


(put 'def-kom-var 'edebug-form-spec
     '(&define name form &rest sexp))



(eval-and-compile (provide 'lyskom-defvar))



