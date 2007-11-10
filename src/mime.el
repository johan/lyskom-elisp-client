;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: mime.el,v 44.15 2007-11-10 09:09:32 byers Exp $
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
;;;; File: mime.el
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: mime.el,v 44.15 2007-11-10 09:09:32 byers Exp $\n"))

(lyskom-external-function mm-find-mime-charset-region)
(defun lyskom-mime-charset-for-text-xemacs (start end)
  (lyskom-try-require 'un-define)
  (if (lyskom-try-require 'mm-util)
      (let ((cs (mm-find-mime-charset-region start end)))
	(cond ((null cs) lyskom-server-coding-system)
	      ((null (cdr cs)) (car cs))
	      (t nil)))
    (lyskom-error "Unable to load required mm-util from gnus")))

(defun lyskom-mime-charset-for-text-gnu (start end)
  (let ((codings (delq nil 
		       (mapcar (lambda (cs)
				 (cond ((eq cs 'undecided) lyskom-server-coding-system)
				       ((eq cs 'compound-text) nil)
				       ((or (coding-system-get cs 'mime-charset)
					    (coding-system-get cs ':mime-charset)))))
			       (find-coding-systems-region start end)))))
    (cond ((memq lyskom-server-coding-system codings) lyskom-server-coding-system)
	  ((lyskom-traverse cs kom-preferred-charsets
	     (when (memq cs codings)
	       (lyskom-traverse-break cs))))
	  (t (car codings)))))
      

(defun lyskom-mime-charset-for-text (start end)
  "Determine which MIME charset to use for region START..END.
If the server coding system is valid for the text, it will be
returned. Otherwise charsets are returned based on Emcas idea
of coding system priority.

In XEmacs, this function will only work if mm-util from Gnus
is present and follows the expected conventions."
  (lyskom-xemacs-or-gnu
   (lyskom-mime-charset-for-text-xemacs start end)
   (lyskom-mime-charset-for-text-gnu start end)))

(defun lyskom-mime-charset-coding-system (charset)
  (condition-case nil
      (and (lyskom-check-coding-system charset)
           charset)
    (error 'raw-text)))

(defun lyskom-mime-encode-string (data mime-charset)
  (let* ((coding-system (lyskom-mime-charset-coding-system mime-charset)))
    (when (and mime-charset coding-system)
      (cons mime-charset (lyskom-encode-coding-string data coding-system)))))

(defun lyskom-mime-decode-string (data charset)
  (let* ((coding-system (lyskom-mime-charset-coding-system charset)))
    (if coding-system
        (lyskom-decode-coding-string data coding-system)
      data)))

(defvar lyskom-mime-type-regexp)
(defvar lyskom-mime-parameter-regexp)
(defvar lyskom-mime-wrap-regexp)
(defvar lyskom-mime-comment-regexp)
(defconst lyskom-mime-parameter-name-match 1)
(defconst lyskom-mime-parameter-value-match 2)

(let* ((whitespace "[\000-\040]*")
       (tspecials "][()<>@,;:\\\"/?=\000-\040")
       (qstring "\"\\([^\"\\]\\|\\\\[\000-\377]\\)*\"")
       (notspecials (format "[^%s]+" tspecials))
       (comment-base "\\((\\([^()]*%s\\)*[^()]*)\\)"))

  (setq lyskom-mime-parameter-regexp 
        (concat whitespace ";" whitespace
                "\\(" notspecials "\\)"
                whitespace "=" whitespace
                "\\(" notspecials
                "\\|" qstring
                "\\)"
                "[^;]*"))
  (setq lyskom-mime-type-regexp
        (concat whitespace "\\(" notspecials "/" notspecials "\\)"))
  (setq lyskom-mime-wrap-regexp (concat "\\\\\\r?\n" whitespace))

  ;; We cheat with comments, and only allow limited nesting
  (setq lyskom-mime-comment-regexp comment-base)
  (let ((nesting 2))
    (while (> nesting 0)
      (setq lyskom-mime-comment-regexp
            (format lyskom-mime-comment-regexp comment-base))
      (setq nesting (1- nesting))))
      (setq lyskom-mime-comment-regexp
            (concat whitespace
                    (format lyskom-mime-comment-regexp "")
                    whitespace))
  )


;;; Parse content-type header according to RFC2045, with some
;;; exceptions. We do not allow comments everywhere, which is
;;; wrong, but I don't care. We do allow comments in many
;;; places, and hopefully those places people will use.

(defun lyskom-mime-decode-content-type (data)
  (let ((content-type nil)
        (params nil)
        (start 0))

    (when (and (string-match lyskom-mime-comment-regexp data start)
               (eq (match-beginning 0) start))
      (setq start (match-end 0)))
    (when (string-match lyskom-mime-type-regexp data)
      (setq content-type (downcase (match-string 1 data)))
      (setq start (match-end 1)))

    (when (and (string-match lyskom-mime-comment-regexp data start)
               (eq (match-beginning 0) start))
      (setq start (match-end 0)))
    (while (string-match lyskom-mime-parameter-regexp data start)
      (let ((param-name (intern (downcase (match-string lyskom-mime-parameter-name-match data))))
            (param-value (downcase (match-string lyskom-mime-parameter-value-match data))))
        (setq start (match-end 0))

        (when (string-match "^\"[\000-\377]*\"$" param-value)
          (setq param-value (substring param-value 1 -1)))
        (setq param-value (replace-in-string param-value
                                             lyskom-mime-wrap-regexp ""))
	(when (eq param-name 'charset)
	  (setq param-value (intern param-value))
	  (when (eq param-value 'us-ascii) (setq param-value 'iso-8859-1)))

	(when (eq param-name 'format)
	  (setq param-value (intern param-value)))

        (when (and (string-match lyskom-mime-comment-regexp data start)
                   (eq (match-beginning 0) start))
          (setq start (match-end 0)))

        (setq params (cons (cons param-name (or param-value t)) params)))
      )

    (cons content-type params)))


(provide 'lyskom-mime)
