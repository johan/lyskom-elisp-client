;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: flags.el,v 44.21 2000-01-10 13:33:45 byers Exp $
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
;;;; File: flags.el
;;;;
;;;; This file contains code for editing the user variables and saving
;;;; them in the user area.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: flags.el,v 44.21 2000-01-10 13:33:45 byers Exp $\n"))

(eval-when-compile
  (require 'lyskom-command "command"))


(defvar lyskom-options-text nil
  "Text mass when reading options.")

(def-kom-var lyskom-options-done nil
  "When we have read all options this is turned non-nil."
  local)

;;;============================================================
;;;  lyskom-save-options
;;;
;;;  Save user-area without feedback in the KOM buffer. This
;;;  is for use by all functions but edit-options.
;;;
;;;  Messages are given in the minibuffer

(defun lyskom-save-options (kombuf start-message done-message error-message)
  (let* ((print-readably t)
         (common-block 
          (concat
           (mapconcat (function
                       (lambda (var)
                         (lyskom-format-objects
                          (substring (symbol-name var) 4) 
                          (if (symbol-value var) "1" "0"))))
                      lyskom-global-boolean-variables
                      "\n")
           "\n"
           (mapconcat (function
                       (lambda (var)
                         (lyskom-format-objects
                          (substring (symbol-name var) 4)
                          (prin1-to-string (symbol-value var)))))
                      lyskom-global-non-boolean-variables
                      "\n")
           ))
         (elisp-block
          (mapconcat
	   (lambda (var)
	     (concat (format "%dH%s"
			     (length (symbol-name var))
			     (symbol-name var))
		     " "
		     (let* ((data (prin1-to-string (symbol-value var)))
			    (coding 
			     (lyskom-mime-charset-coding-system
			      (lyskom-mime-string-charset data)))
			    (val (condition-case nil
				     (encode-coding-string data coding)
				   (error nil))))
		       (if (and val nil)
			   (format "%dC%s%dH%s"
				   (string-bytes (symbol-name coding))
				   (symbol-name coding)
				   (string-bytes val)
				   val)
			 (setq data
			       (encode-coding-string 
				data
				(or lyskom-server-coding-system 'raw-text)))
			 (format "%dH%s"
				 (string-bytes data)
				 data)))))
	   lyskom-elisp-variables
	   "\n")))
    (save-excursion
      (set-buffer kombuf)
      (lyskom-message "%s" start-message)
      (initiate-create-text
       'options 'lyskom-save-options-2
       (cons 'raw-text
	     (apply 'lyskom-format-objects
		    (apply 'lyskom-format-objects
			   "common"
			   "elisp"
			   (mapcar 
			    (lambda (el)
			      (cons 'STRING (cons 'raw-text (car el))))
			    lyskom-other-clients-user-areas))
		    (cons 'STRING (cons 'raw-text common-block))
		    (cons 'STRING (cons 'raw-text elisp-block))
		    (mapcar (lambda (el)
			     (cons 'STRING (cons 'raw-text (cdr el))))
			    lyskom-other-clients-user-areas)))
                            (lyskom-create-misc-list) 
                            nil
                            kombuf
                            done-message
                            error-message))))

(defun lyskom-save-options-2 (text-no kombuf done-message error-message)
  (if text-no
      (initiate-set-user-area 'options 'lyskom-save-options-3
                              lyskom-pers-no text-no kombuf 
                              done-message error-message)
    (save-excursion
     (set-buffer kombuf)
     (lyskom-insert-string 'could-not-save-options)
     (lyskom-message "%s" (lyskom-get-string 'could-not-save-options)))))

(defun lyskom-save-options-3 (success kombuf done-message error-message)
  (save-excursion
    (set-buffer kombuf)
    (if success
        (progn
          (cache-del-pers-stat lyskom-pers-no)
          (lyskom-message "%s" done-message))
      (lyskom-format-insert 'could-not-set-user-area lyskom-errno)
      (lyskom-message "%s" error-message))))


(defun lyskom-read-options ()
  "Reads the user-area and sets the variables according to the choises."
  (if (and lyskom-pers-no
	   (not (zerop lyskom-pers-no)))
      (let ((pers-stat (blocking-do 'get-pers-stat lyskom-pers-no)))
	(if (not pers-stat)  ;+++ Other error handler.
	    (lyskom-insert-string 'you-dont-exist)
	  (setq lyskom-other-clients-user-areas nil)
	  (if (zerop (pers-stat->user-area pers-stat))
	      (progn
		;; (lyskom-tell-phrases-validate)
		(setq lyskom-options-done t))
	    (lyskom-read-options-eval 
	     (blocking-do 'get-text
			  (pers-stat->user-area pers-stat))))))))


(defun lyskom-read-options-eval (text)
  "Handles the call from where we have the text."
  (condition-case nil
      (if text				;+++ Other error handler
	  (let* ((lyskom-options-text (text->text-mass text))
		 (pointers (lyskom-read-options-eval-get-holerith t))
		 common-no elisp-no
		 (rest lyskom-options-text)
		 working
		 (r 1))
	    (let* ((lyskom-options-text pointers)
		   word
		   (r 1))
	      (while (> (length lyskom-options-text) 2)
		(setq word (lyskom-read-options-eval-get-holerith t))
		(cond
		 ((lyskom-string= word "common")
		  (setq common-no r))
		 ((lyskom-string= word "elisp")
		  (setq elisp-no r))
		 (t
		  ;; Build up lyskom-other-clients-user-areas so that it
		  ;; contains a list of pairs: (name . number). (string, int).
		  (setq lyskom-other-clients-user-areas
			(cons (cons word r) lyskom-other-clients-user-areas))))
		(++ r)))
	    (setq lyskom-other-clients-user-areas 
		  (nreverse lyskom-other-clients-user-areas))
	    (setq lyskom-options-text rest)
	    (while (> (length lyskom-options-text) 2)
	      (setq working (lyskom-read-options-eval-get-holerith t))
	      (cond
	       ;; Note that common-no may be nil here, so the comparison
	       ;; cannot be performed with '=.
	       ((equal r common-no)
		(let ((lyskom-options-text working)
		      name gname value)
		  (while (> (length lyskom-options-text) 2)
		    (setq gname (lyskom-read-options-eval-get-holerith))
		    (setq value (lyskom-read-options-eval-get-holerith))
		    (setq name (concat "kom-" gname))
		    (if (memq (intern-soft name) lyskom-global-boolean-variables)
			(if (string= value "1")
			    (setq value "t")
			  (setq value "nil"))
		      (if (memq (intern-soft name) 
				lyskom-global-non-boolean-variables)
			  nil
			(setq name (concat "UNK-" gname))
			(setq lyskom-global-non-boolean-variables
			      (cons name lyskom-global-non-boolean-variables))))
		    (lyskom-maybe-set-var-from-string name value))))
	       ;; Note that elisp-no may be nil here, so the comparison
	       ;; cannot be performed with '=.
	       ((equal r elisp-no)
		(let ((lyskom-options-text working)
		      name value)
		  (while (> (length lyskom-options-text) 2)
		    (setq name (lyskom-read-options-eval-get-holerith))
		    (setq value (lyskom-read-options-eval-get-holerith))
		    (lyskom-maybe-set-var-from-string name value))))
	       (t
		(let ((pos lyskom-other-clients-user-areas))
		  (while (and pos
			      (not (equal
				    (cdr (car pos)) ;The position or the string.
				    r)))
		    (setq pos (cdr pos)))
		  (if pos
		      (setcdr (car pos) working))))) ;Insert the string
					;where the position
					;was stored.
	      (++ r))

	    (mapcar 'lyskom-recompile-filter kom-permanent-filter-list)
	    (mapcar 'lyskom-recompile-filter kom-session-filter-list)

	    (setq lyskom-filter-list (append kom-permanent-filter-list
					     kom-session-filter-list))
	    (setq lyskom-do-when-done (cons kom-do-when-done kom-do-when-done))
	    ;; Remove not found user-areas
	    (let ((pos lyskom-other-clients-user-areas))
	      (if pos
		  (progn
		    (while (stringp (cdr (car (cdr pos))))
		      (setq pos (cdr pos)))
		    (setcdr pos nil))))))
    (error (lyskom-message "%s" (lyskom-get-string 'error-in-options-short))))
  (setq lyskom-options-done t))


(defun lyskom-read-options-eval-get-holerith (&optional no-coding)
  (let ((coding (if no-coding 'raw-text lyskom-server-coding-system)))
    (while (string-match "\\(\\s-\\|[\n\r]\\)" (substring lyskom-options-text 0 1))
      (setq lyskom-options-text (substring lyskom-options-text 1)))

    ;; Read the explicit coding, if any

    (when (string-match "^[0-9]+C" lyskom-options-text)
      (let ((len (string-to-int lyskom-options-text)))
        (setq coding (intern
                      (substring lyskom-options-text 
                                 (match-end 0)
                                 (+ (match-end 0) len))))
        (setq lyskom-options-text (substring lyskom-options-text
                                             (+ (match-end 0) len))))) 

    ;; Read the string

    (let ((len (string-to-int lyskom-options-text))
          (start (progn (string-match "[0-9]+H" lyskom-options-text)
                        (match-end 0))))
      (let ((name (substring lyskom-options-text start (+ start len))))
        (setq lyskom-options-text (substring lyskom-options-text
                                             (+ start len)))
        (condition-case nil
            (decode-coding-string name coding)
          (error name))))))

(defun lyskom-maybe-set-var-from-string (var string)
  "This is a wrapper around lyskom-set-var-from-string that does nothing
if the variable is in kom-dont-read-saved-variables."
  (cond ((eq kom-dont-read-saved-variables t) nil)
        ((memq (intern var) kom-dont-read-saved-variables) nil)
        (t (lyskom-set-var-from-string var string))))

(defun lyskom-set-var-from-string (var string)
  "This is a wrapper aroud read-from-string.
It returns nil, and writes a message when an error occurs."
  (set (intern var)
       (car
	(condition-case nil
	    (read-from-string string)
	  (invalid-read-syntax
	   (lyskom-format-insert (lyskom-get-string 'error-in-options)
				 var string)
	   nil)))))
