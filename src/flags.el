;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: flags.el,v 44.39 2003-08-14 15:59:24 byers Exp $
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
;;;; File: flags.el
;;;;
;;;; This file contains code for editing the user variables and saving
;;;; them in the user area.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: flags.el,v 44.39 2003-08-14 15:59:24 byers Exp $\n"))

(eval-when-compile
  (require 'lyskom-command "command"))

(put 'lyskom-invalid-flag-type 'error-conditions
     '(error lyskom-error lyskom-invalid-flag-type))

(defvar lyskom-options-text nil
  "Text mass when reading options.")

(def-kom-var lyskom-options-done nil
  "When we have read all options this is turned non-nil."
  local)

(def-kom-command kom-save-options ()
  "Save all LysKOM settings. This will save settings in the server but
not save anything to your Emacs init file (usually .emacs).

To edit settings, use `kom-customize'. This command can also save
settings to your emacs init file."
  (interactive)
  (lyskom-save-options (or lyskom-buffer 
                           (current-buffer))
                       (lyskom-get-string 'saving-settings)
                       (lyskom-get-string 'saving-settings-done)
                       (lyskom-get-string 'could-not-save-options)))

(def-kom-command kom-copy-options ()
  "Copy settings from one LysKOM session to another. This is useful if
you have configured on session to your liking and want ths settings to
be used on other servers as well.

This command will save settings to the LysKOM server, but will not
save anything to your Emacs init file. Use `kom-customize' to edit
settings and save them to your emacs init file."
  (interactive)
  (let* ((completions
          (mapcar (lambda (x)
                    (cons (save-excursion
                            (set-buffer x)
                            (buffer-name))
                          x))
                  (delq (or lyskom-buffer (current-buffer))
                        (copy-sequence lyskom-buffer-list))))
         (from-session (completing-read 
                        (lyskom-get-string 'session-to-copy-options-from)
                        completions
                        nil t)))
    (lyskom-message (lyskom-get-string 'reading-settings-from) from-session)
    (lyskom-read-options from-session)
    (lyskom-message (lyskom-get-string 'reading-settings-from-done) from-session)

    ;; Inline kom-save-options
    (lyskom-save-options (or lyskom-buffer (current-buffer))
                         (lyskom-get-string 'saving-settings)
                         (lyskom-get-string 'saving-settings-done)
                         (lyskom-get-string 'could-not-save-options))
))


;;;============================================================
;;;  lyskom-save-options
;;;
;;;  Save user-area without feedback in the KOM buffer. This
;;;  is for use by all functions but edit-options.
;;;
;;;  Messages are given in the minibuffer

(defun lyskom-save-options (kombuf start-message done-message error-message)
  ;; `kom-presence-messages' isn't used directly by the emacs lisp client.
  ;; Since there is much finer control of what presence messages to give
  ;; it's hard to encode this is a single boolean, so usually don't try.
  ;; Ony if the user sees all or no presence messages is it rather clear
  ;; what the common block value should be:
  (cond ((or (eq kom-presence-messages-in-buffer t)
 	     (eq kom-presence-messages-in-echo-area t))
 	 (setq kom-presence-messages t))
 	((not (or kom-presence-messages-in-buffer
 		  kom-presence-messages-in-echo-area))
 	 (setq kom-presence-messages nil)))

  (let* ((print-readably t)
         (common-block 
          (concat
           (mapconcat (lambda (var)
                        (condition-case nil
                            (let ((common-name (elt var 0))
                                  (elisp-name (elt var 1))
                                  (type (elt var 2)))
                              (lyskom-format-objects
                               (symbol-name common-name)
                               (funcall 
                                (cdr (assq 'write
                                           (cdr (assq (or type t)
                                                      lyskom-global-variable-types))))
                                (symbol-value elisp-name))))
                          (lyskom-invalid-flag-type "")))
                      lyskom-global-variables
                      "\n")
           ))
         (elisp-block
          (mapconcat
	   (lambda (var)
	     (concat (format "%dH%s"
			     (length (symbol-name var))
			     (symbol-name var))
		     " "
		     (let* ((data (lyskom-flag-value-to-string var))
			    (coding 
			     (lyskom-mime-charset-coding-system
			      (lyskom-mime-string-charset data)))
			    (val (condition-case nil
				     (encode-coding-string data coding)
				   (error nil))))
                       (lyskom-ignore val)
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
      (when start-message (lyskom-message "%s" start-message))
      (initiate-create-text
       'options 
       'lyskom-save-options-2
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
                            (list
                             (lyskom-create-aux-item 
                              0 1 nil nil
                              (lyskom-create-aux-item-flags nil nil nil nil nil nil nil nil)
                              0
                              "x-kom/user-area"))
                            kombuf
                            done-message
                            error-message))))

(defun lyskom-save-options-2 (text-no kombuf done-message error-message)
  (if text-no
      (initiate-set-user-area 'options 'lyskom-save-options-3
                              lyskom-pers-no text-no kombuf 
                              done-message error-message text-no)
    (save-excursion
     (set-buffer kombuf)
     (lyskom-insert-string 'could-not-save-options)
     (lyskom-message "%s" (lyskom-get-string 'could-not-save-options)))))

(defun lyskom-save-options-3 (success kombuf done-message
                                      error-message text-no)
  (save-excursion
    (set-buffer kombuf)
    (if success
        (let ((pers-stat (cache-get-pers-stat lyskom-pers-no)))
          (when pers-stat
            (set-pers-stat->user-area pers-stat text-no))
          (setq lyskom-current-user-area text-no)
          (when done-message (lyskom-message "%s" done-message)))
      (lyskom-format-insert 'could-not-set-user-area lyskom-errno)
      (when error-message (lyskom-message "%s" error-message)))))

(defun lyskom-read-options (&optional buffer)
  "Reads the user-area and sets the variables according to the choises.
Returns a list of variables that were ignored. If optional BUFFER is
non-nil, read settings in that buffer."
  (if (and lyskom-pers-no
	   (not (zerop lyskom-pers-no)))
      (let ((pers-stat 
             (save-excursion
               (when buffer (set-buffer buffer))
               (blocking-do 'get-pers-stat lyskom-pers-no))))
	(if (not pers-stat)             ;+++ Other error handler.
	    (progn (lyskom-insert-string 'you-dont-exist)
                   nil)
	  (setq lyskom-other-clients-user-areas nil)
	  (if (zerop (pers-stat->user-area pers-stat))
	      (progn
		;; (lyskom-tell-phrases-validate)
		(setq lyskom-options-done t)
                nil)
	    (lyskom-read-options-eval 
             (save-excursion
               (when buffer (set-buffer buffer))
               (blocking-do 'get-text
                            (pers-stat->user-area pers-stat)))))))
    nil))

(defun lyskom-read-options-eval (text)
  "Handles the call from where we have the text.
Returns a list of variables that were ignored."
  (let ((ignored-user-area-vars nil))
    (condition-case nil
        (if text                        ;+++ Other error handler
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
                        spec name value)
                    (while (> (length lyskom-options-text) 2)
                      (setq name (intern (lyskom-read-options-eval-get-holerith)))
                      (setq value (lyskom-read-options-eval-get-holerith))
                      (setq spec
                            (cond ((lyskom-flag-global-variable-from-common name))
                                  (t (let ((tmp (vector name
                                                        (intern (format "lyskom-UNK-%S" name))
                                                        nil)))
                                       (setq lyskom-global-variables
                                             (cons tmp lyskom-global-variables))
                                       tmp))))

                      (unless (lyskom-maybe-set-var-from-string (elt spec 1) value (elt spec 2))
                        (setq ignored-user-area-vars
                              (cons (elt spec 1) ignored-user-area-vars))))))
                 ;; Note that elisp-no may be nil here, so the comparison
                 ;; cannot be performed with '=.
                 ((equal r elisp-no)
                  (let ((lyskom-options-text working)
                        name value)
                    (while (> (length lyskom-options-text) 2)
                      (setq name (intern (lyskom-read-options-eval-get-holerith)))
                      (setq value (lyskom-read-options-eval-get-holerith))
                      (if (lyskom-maybe-set-var-from-string name value)
                          (when (functionp (cdr (assq name lyskom-transition-variables)))
                            (set name (funcall (cdr (assq name lyskom-transition-variables))
                                               (symbol-value name))))
                        (setq ignored-user-area-vars
                              (cons name ignored-user-area-vars))))))
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
    (setq lyskom-options-done t)
    (setq lyskom-current-user-area (if text (text->text-no text) 0))
    ignored-user-area-vars))


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

(defun lyskom-get-holerith (string &optional no-coding)
  "Get the first holerith string in STRING.
If optional NO-CODING is set, assume the string has internal coding."
  (let ((coding (if no-coding 'raw-text lyskom-server-coding-system)))

    ;; Strip initial whitespace

    (when (string-match "^\\(\\s-\\|[\n\r]\\)+" string)
      (setq string (substring string (match-end 0))))

    ;; Read the explicit coding, if any

    (when (string-match "^[0-9]+C" string)
      (let ((len (string-to-int string)))
        (setq coding (intern
                      (substring string 
                                 (match-end 0)
                                 (+ (match-end 0) len))))
        (setq string (substring string (+ (match-end 0) len)))))

    ;; Read the string

    (let ((len (string-to-int string))
          (start (progn (string-match "[0-9]+H" string)
                        (match-end 0))))
      (let ((name (substring string start (+ start len))))
        (setq string (substring string (+ start len)))
        (condition-case nil
            (cons (decode-coding-string name coding) string)
          (error (cons name string)))))))

(defun lyskom-maybe-set-var-from-string (var string &optional type)
  "This is a wrapper around lyskom-set-var-from-string that does nothing
if the variable is in kom-dont-read-saved-variables.

Return non-nil if the variable shouldn't have been set in the first place."
  (cond ((eq kom-dont-read-saved-variables t) t)
        ((memq var kom-dont-read-saved-variables) t)
        ((not (or (memq var lyskom-elisp-variables)
                  (assq var lyskom-transition-variables)
                  (lyskom-flag-global-variable-from-elisp var))) nil)
        (t (lyskom-set-var-from-string var string type) t)))

(defun lyskom-set-var-from-string (var string &optional type)
  "This is a wrapper aroud read-from-string.
It returns nil, and writes a message when an error occurs."
  (set var (condition-case nil
               (funcall 
                (cdr (assq 'read
                           (cdr (assq (or type t)
                                      lyskom-global-variable-types))))
                string)
             (error
              (lyskom-format-insert (lyskom-get-string 'error-in-options)
                                    var string)
              nil))))


(defun lyskom-flag-value-to-string (symbol)
  "Convert value of SYMBOL to a string."
  (cond ((eq symbol 'kom-permanent-filter-list)
         (prin1-to-string 
          (mapcar (lambda (filter)
                    (lyskom-create-filter (filter->pattern filter)
                                          (filter->attribute-list filter)
                                          nil))
                  kom-permanent-filter-list)))
        (t (prin1-to-string (symbol-value symbol)))))


(defun lyskom-flag-global-variable-from-common (var)
  "Return the element from lyskom-global-variables corresponding to VAR."
  (lyskom-traverse el lyskom-global-variables
    (when (eq (aref el 0) var)
      (lyskom-traverse-break el))))

(defun lyskom-flag-global-variable-from-elisp (var)
  "Return the element from lyskom-global-variables corresponding to
elisp variable VAR."
  (lyskom-traverse el lyskom-global-variables
    (when (eq (aref el 1) var)
      (lyskom-traverse-break el))))

(defun lyskom-flag-write-boolean (val)
  (if val "1" "0"))

(defun lyskom-flag-read-boolean (str)
  (string= "1" str))

(defun lyskom-flag-write-symbol-list (val)
  (cond ((symbolp val)
         (lyskom-format-objects (symbol-name val)))
        (t (mapconcat (lambda (x) (lyskom-format-objects (symbol-name x)))
                      val " "))))

(defun lyskom-flag-read-symbol-list (str)
  (mapcar 'intern (lyskom-get-holerith-list str)))

(defun lyskom-flag-read-from-string (str)
  (car (read-from-string str)))

(defun lyskom-flag-read-integer (str)
  (cond ((equal str "nil") nil)
        (t (string-to-int str))))

(defun lyskom-flag-write-integer (val)
  (cond ((integerp val) (prin1-to-string val))
        (t (signal 'lyskom-invalid-flag-type nil))))
