;;;;;
;;;;; $Id: flags.el,v 38.5 1996-01-19 18:49:51 byers Exp $
;;;;; Copyright (C) 1991  Lysator Academic Computer Association.
;;;;;
;;;;; This file is part of the LysKOM server.
;;;;; 
;;;;; LysKOM is free software; you can redistribute it and/or modify it
;;;;; under the terms of the GNU General Public License as published by 
;;;;; the Free Software Foundation; either version 1, or (at your option) 
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
	      "$Id: flags.el,v 38.5 1996-01-19 18:49:51 byers Exp $\n"))


;;; Author: Linus Tolke


(fset 'original-user-variable-p
      (symbol-function 'user-variable-p))


(defun lyskom-user-variable-p (symbol)
  (and (original-user-variable-p symbol)
       (or (string-match "^kom-"
			 (symbol-name symbol))
	   (string-match "^lyskom-"
			 (symbol-name symbol)))))


(defun kom-edit-options ()
  "Edit options for the lyskom client."
  (interactive)
  (fset 'user-variable-p
	(symbol-function 'lyskom-user-variable-p))
  (let ((buf (current-buffer))
	(curwin (current-window-configuration)))
    (edit-options)
    (make-local-variable 'lyskom-buffer)
    (make-local-variable 'lyskom-edit-return-to-configuration)
    (setq lyskom-buffer buf)
    (setq lyskom-edit-return-to-configuration curwin)
    (local-set-key "\C-c\C-c" 'kom-edit-options-send)
    (local-set-key "\C-c\C-k" 'kom-edit-quit)
    )
  (fset 'user-variable-p
	(symbol-function 'original-user-variable-p)))

;;;============================================================
;;;  kom-edit-options-send
;;;
;;;  Finish an edit options session.
;;;  This function must be kept in sync with lyskom-save-options
;;;  below.
;;;

(defun kom-edit-options-send ()
  "Finishes the edit options and sends the new settings to the server."
  (interactive)
  ; The check for changes is not a very good one.
  (cond
   ((not (eq major-mode 'Edit-options-mode))
    (error "You are not in the correct buffer. (Couldn't fool me this time."))
   ((buffer-modified-p (current-buffer))
    ;lets do it.
    ;lyskom-global-variables is a list of variables in the common block.
    ;lyskom-elisp-variables is a list of varibles in the elisp block.
    (let ((buf lyskom-buffer)
	  (optbuf (current-buffer))
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
	   (mapconcat (function
		       (lambda (var)
			 (lyskom-format-objects (symbol-name var) 
						(prin1-to-string
						 (symbol-value var)))))
		      lyskom-elisp-variables
		      "\n")))
      (set-buffer buf)
      (lyskom-start-of-command (lyskom-get-string 'saving-settings) t)
      (lyskom-insert-string 'hang-on)
      (initiate-create-text 'options 'lyskom-edit-options-send
			    ;;; This is a cludge awaiting prot-B
			    (apply 'lyskom-format-objects 
				   (apply 'lyskom-format-objects 
					  "common"
					  "elisp"
					  (mapcar 
					   (function car)
					   lyskom-other-clients-user-areas))
				   common-block
				   elisp-block
				   (mapcar (function cdr) 
					   lyskom-other-clients-user-areas))
;			    (concat common-block "----------\n" elisp-block)
			    (lyskom-create-misc-list) optbuf)))))


(defun lyskom-edit-options-send (text-no optbuf)
  "Handles the call after the options text has been sent to the buffer."
  (if text-no
      (initiate-set-user-area 'options 'lyskom-edit-options-done
			      lyskom-pers-no text-no optbuf)
    (lyskom-insert-string 'could-not-create-area)
    (lyskom-end-of-command)))


(defun lyskom-edit-options-done (success optbuf)
  "Handles the return from the set user area call.
If successful then set the buffer not-modified. Else print a warning."
  (if success
      (save-excursion
	;;;+++ This should be done with the asynchronous call instead.
	(cache-del-pers-stat lyskom-pers-no)
	(set-buffer optbuf)
	(not-modified)
	(set-window-configuration lyskom-edit-return-to-configuration))
    (lyskom-format-insert 'could-not-set-user-area lyskom-errno))
  (lyskom-end-of-command))


(defvar lyskom-options-done nil
  "When we have read all options this is turned non-nil.")

;;;============================================================
;;;  lyskom-save-options
;;;
;;;  Save user-area without feedback in the KOM buffer. This
;;;  is for use by all functions but edit-options.
;;;
;;;  Messages are given in the minibuffer

(defun lyskom-save-options (kombuf start-message done-message error-message)
  (let ((common-block 
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
         (mapconcat (function
                     (lambda (var)
                       (lyskom-format-objects (symbol-name var) 
                                              (prin1-to-string
                                               (symbol-value var)))))
                    lyskom-elisp-variables
                    "\n")))
    (save-excursion
      (set-buffer kombuf)
      (lyskom-message start-message)
      (initiate-create-text 'options 'lyskom-save-options-2
                            (apply 'lyskom-format-objects
                                   (apply 'lyskom-format-objects
                                          "common"
                                          "elisp"
                                          (mapcar 
                                           (function car)
                                           lyskom-other-clients-user-areas))
                                   common-block
                                   elisp-block
                                   (mapcar (function cdr)
                                           lyskom-other-clients-user-areas))
                            (lyskom-create-misc-list) 
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
     (lyskom-message (lyskom-get-string 'could-not-save-options)))))

(defun lyskom-save-options-3 (success kombuf done-message error-message)
  (save-excursion
    (set-buffer kombuf)
    (if success
        (progn
          (cache-del-pers-stat lyskom-pers-no)
          (lyskom-message done-message))
      (lyskom-format-insert 'could-not-set-user-area lyskom-errno)
      (lyskom-message error-message))))


(defun lyskom-read-options ()
  "Reads the user-area and sets the variables according to the choises."
  (if (and lyskom-pers-no
	   (not (zerop lyskom-pers-no)))
      (let ((pers-stat (blocking-do 'get-pers-stat lyskom-pers-no)))
	(if (not pers-stat)  ;+++ Other error handler.
	    (lyskom-insert-string 'you-dont-exist)
	  (setq lyskom-other-clients-user-areas)
	  (if (zerop (pers-stat->user-area pers-stat))
	      (progn
		(lyskom-tell-phrases-validate)
		(setq lyskom-options-done t))
	    (lyskom-read-options-eval 
	     (blocking-do 'get-text
			  (pers-stat->user-area pers-stat))))))))


(defun lyskom-read-options-eval (text)
  "Handles the call from where we have the text."
  (if text				;+++ Other error handler
      (let* ((txt (text->text-mass text))
	     (pointers (lyskom-read-options-eval-get-holerith))
	     common-no elisp-no
	     (rest txt)
	     working
	     (r 1))
	(let* ((txt pointers)
	       word no
	       (r 1))
	  (while (> (length txt) 2)
	    (setq word (lyskom-read-options-eval-get-holerith))
	    (cond
	     ((string= word "common")
	      (setq common-no r))
	     ((string= word "elisp")
	      (setq elisp-no r))
	     (t
	      (setq lyskom-other-clients-user-areas
		    (cons (cons word r) lyskom-other-clients-user-areas))))
	    (++ r)))
	(setq lyskom-other-clients-user-areas 
	      (nreverse lyskom-other-clients-user-areas))
	(setq txt rest)
	(while (> (length txt) 2)
	  (setq working (lyskom-read-options-eval-get-holerith))
	  (cond
	   ((= r common-no)
	    (let ((txt working)
		  name gname value)
	      (while (> (length txt) 2)
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
		(set (intern name) (car (read-from-string value))))))
	   ((= r elisp-no)
	    (let ((txt working)
		  name value)
	      (while (> (length txt) 2)
		(setq name (lyskom-read-options-eval-get-holerith))
		(setq value (lyskom-read-options-eval-get-holerith))
		(set (intern name) (car (read-from-string value))))))
	   (t
	    (let ((pos lyskom-other-clients-user-areas))
	      (while (and pos
			  (not (= (cdr (car pos)) r)))
		(setq pos (cdr pos)))
	      (if pos
		  (setcdr (car pos) working)))))
	  (++ r))

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
  (setq lyskom-options-done t))


(defun lyskom-read-options-eval-get-holerith ()
  (while (string-match "\\s-" (substring txt 0 1))
    (setq txt (substring txt 1)))
  (let ((len (string-to-int txt))
	(start (progn (string-match "[0-9]+H" txt)
		      (match-end 0))))
    (prog1
	(substring txt start (+ start len))
      (setq txt (substring txt (+ start len))))))
