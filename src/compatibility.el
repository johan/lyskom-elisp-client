;;;;; -*- emacs-lisp -*-
;;;;; $Id: compatibility.el,v 44.3 1997-03-08 02:46:08 davidk Exp $
;;;;; Copyright (C) 1996  Lysator Academic Computer Association.
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
;;;; File: compatibility.el
;;;;
;;;; This file contains functions that may not exist in all supported
;;;; versions of Gnu Emacs. XEmacs-specific and Emacs 18-specific code
;;;; should go in some other file.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: compatibility.el,v 44.3 1997-03-08 02:46:08 davidk Exp $\n"))


;;; ======================================================================
;;; Use lyskom-provide to supply a definition that is only to be used
;;; if no definition already exists. The definition will be evaluated at
;;; both compile and run time.
;;;
;;; lyskom-provide-macros behaves like defmacro
;;; lyskom-provide-function behaves like defun
;;; lyskom-provide-subst behaves like defsubst
;;;

(eval-and-compile
  (defvar lyskom-compatibility-definitions nil
    "Functions defined or redefined because they are incompatible with
LysKOM"))

;;; ============================================================
;;; lyskom-compatibility-forms
;;; lyskom-compatibility-definition
;;;


(defmacro lyskom-compatibility-forms (predicate &rest forms)
  "If PREDICATE is nil, evaluate FORMS at compile and run time"
  (` (eval-and-compile
       (if (not (, predicate))
           (progn (,@ forms))))))


(defmacro lyskom-compatibility-definition (predicate definition)
  "If PREDICATE is nil, evaluate DEFINITION at compile and run time.
Definition should be a function definition of some kind, with syntax 
similar to defun or defmacro.

To simply define a function if it is not already defined, used one
of the lyskom-provide-* functions instead."
  (` (progn (eval-when-compile
              (if (not (, predicate))
                  (message "Compatibility %S for %S"
                           (quote (, (car definition)))
                           (quote (, (car (cdr definition))))))
              (eval-and-compile
                (if (not (, predicate))
                    (progn
                      (, definition)
                      (setq lyskom-compatibility-definitions
                            (cons (quote (, (car (cdr definition))))
                                  lyskom-compatibility-definitions)))))))))


;;; ============================================================
;;; lyskom-provide
;;; lyskom-provide-macro
;;; lyskom-provide-function
;;; lyskom-provide-subst
;;;
;;; Define functions if they are not already defined
;;;

(defmacro lyskom-provide (definer name rest)
  (` (progn (eval-when-compile 
              (if (not (fboundp (quote (, name))))
                  (message "Compatibility %S for %S"
                           (quote (, definer))
                           (quote (, name)))))
            (eval-and-compile 
              (if (not (fboundp (quote (, name))))
                  (progn
                    (setq lyskom-compatibility-definitions
                          (cons (quote (, name)) 
                                lyskom-compatibility-definitions))
                    ((, definer) (, name) (,@ rest))))))))

(defmacro lyskom-provide-macro (name &rest rest)
  "If NAME is not already defined, define it as a macro."
  (` (lyskom-provide defmacro (, name) (, rest))))

(defmacro lyskom-provide-function (name &rest rest)
  "If NAME is not already defined, define it as a function."
  (` (lyskom-provide defun (, name) (, rest))))

(defmacro lyskom-provide-subst (name &rest rest)
  "If NAME is not already defined, define it as a defsubst."
  (` (lyskom-provide defsubst (, name) (, rest))))


;;; ============================================================
;;; lyskom-xemacs-or-gnu
;;;

(defmacro lyskom-xemacs-or-gnu (xemacs-form gnu-form)
  "Eval XEMACS-FORM in XEmacs and GNU-FORM in Gnu Emacs."
  (` (if (string-match "XEmacs" (emacs-version))
         (, xemacs-form)
       (, gnu-form))))


;;; ======================================================================
;;; ======================================================================
;;; ======================================================================

;;;


(lyskom-provide-macro byte-code-function-p (obj)
  (` (compiled-function-p (, obj))))

(lyskom-compatibility-forms (fboundp 'frame-width)
    (fset 'frame-width 'screen-width))


;;; ======================================================================
;;; Definition of map-keymap that hopefully works like the one in XEmacs
;;; except that the sort-first argument is ignored.
;;;

(lyskom-provide-function map-keymap (fn keymap &optional sort-first)
  (let ((lis nil)
        (r 0))
    (cond ((vectorp keymap)
           (while (< r (length keymap))
             (if (aref keymap r)
                 (funcall fn r (aref keymap r)))
             (setq r (1+ r))))
          (t (mapcar (function 
                      (lambda (x)
                        (funcall fn (car x) (cdr x))))
                     (cdr keymap))))))


(lyskom-provide-function set-keymap-parent (keymap new-parent)
   (let ((tail keymap))
     (while (and tail (cdr tail) (not (eq (car (cdr tail)) 'keymap)))
       (setq tail (cdr tail)))
     (if tail
         (setcdr tail new-parent))))

(defconst lyskom-xemacs-keysym 
  '((mouse-1 . (button1))
    (mouse-2 . (button2))
    (mouse-3 . (button3))
    (down-mouse-3 . (button3))
    (å       . aring)
    (Å       . Aring)
    (ä       . adiearesis)
    (Ä       . Adiearesis)))

(defconst lyskom-gnu-keysym
  '((å       . ?\å)
    (Å       . ?\Å)
    (ä       . ?\Ä)
    (Ä       . ?\Ä)
    (ö       . ?\ö)
    (Ö       . ?\Ö)))


(defun lyskom-keys (binding)
  (cond ((vectorp binding) (apply 'vector (mapcar 'lyskom-keysym binding)))
        (t binding)))

(defun lyskom-keysym (sym)
  "Look up the proper symbol to bind sym to"
  (or (cdr (assq sym (lyskom-xemacs-or-gnu lyskom-xemacs-keysym
                                           lyskom-gnu-keysym)))
      sym))


;;; ======================================================================
;;; Event stuff

(lyskom-provide-function event-point (e)
  "Return the character position of the given mouse event.
If the event did not occur over a window, or did not occur over text,
then this returns nil.  Otherwise, it returns an index into the buffer
visible in the event's window."
  (car (cdr (event-start e))))

(lyskom-provide-function popup-menu (menu-desc &optional event)
  (let* ((result (x-popup-menu (or event t)
                               (list menu-desc)))
         (command (car result)))
    (if command
        (apply (car command)
               (cdr command)))))

(defun lyskom-get-buffer-window-list (buffer &optional minibuf frame)
  "Return windows currently displaying BUFFER, or nil if none.
See `walk-windows' for the meaning of MINIBUF and FRAME."
  (let ((buffer (if (bufferp buffer) buffer (get-buffer buffer))) windows)
    (walk-windows (function (lambda (window)
			      (if (eq (window-buffer window) buffer)
				  (setq windows (cons window windows)))))
		  minibuf frame)
    windows))

(lyskom-provide-function window-list (&optional frame minibuf window)
  "Return a list of windows on FRAME, beginning with WINDOW.
FRAME and WINDOW default to the selected ones.  
Optional second arg MINIBUF t means count the minibuffer window
even if not active.  If MINIBUF is neither t nor nil it means
not to count the minibuffer even if it is active."
  (setq window (or window (selected-window))
	frame (or frame (selected-frame)))
  (if (not (eq (window-frame window) frame))
      (error "Window must be on frame."))
  (let ((current-frame (selected-frame))
	list)
    (unwind-protect
	(save-window-excursion
	  (select-frame frame)
	  (walk-windows
	   (function (lambda (cur-window)
		       (if (not (eq window cur-window))
			   (setq list (cons cur-window list)))))
	   minibuf)
	  (setq list (cons window list)))
      (select-frame current-frame))))



(lyskom-provide-function 
 replace-in-string (str regexp newtext &optional literal)
  "Replaces all matches in STR for REGEXP with NEWTEXT string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat \\ in NEWTEXT string as special:
  \\& means substitute original matched text,
  \\N means substitute match for \(...\) number N,
  \\\\ means insert one \\."
  (if (not (stringp str))
      (error "(replace-in-string): First argument must be a string: %s" str))
  (if (stringp newtext)
      nil
    (error "(replace-in-string): 3rd arg must be a string: %s"
	   newtext))
  (let ((rtn-str "")
	(start 0)
	(special)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat
	      rtn-str
	      (substring str prev-start match)
	      (cond (literal newtext)
		    (t (mapconcat
			 (function
			   (lambda (c)
			     (if special
				 (progn
				   (setq special nil)
				   (cond ((eq c ?\\) "\\")
					 ((eq c ?&)
					  (substring str
						     (match-beginning 0)
						     (match-end 0)))
					 ((and (>= c ?0) (<= c ?9))
					  (if (> c (+ ?0 (length
							   (match-data))))
					      ;; Invalid match num
					      (error "(replace-in-string) Invalid match num: %c" c)
					    (setq c (- c ?0))
					    (substring str
						       (match-beginning c)
						       (match-end c))))
					 (t (char-to-string c))))
			       (if (eq c ?\\) (progn (setq special t) nil)
				 (char-to-string c)))))
			 newtext ""))))))
    (concat rtn-str (substring str start))))

;;; Local Variables:
;;; eval: (put 'lyskom-provide-macro 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-provide-function 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-provide-subst 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-compatibility-forms 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-compatibility-definition 'lisp-indent-hook 2)
;;; end:
