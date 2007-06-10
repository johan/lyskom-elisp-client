;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: mime.el,v 44.13 2007-06-10 11:08:20 byers Exp $
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
	      "$Id: mime.el,v 44.13 2007-06-10 11:08:20 byers Exp $\n"))

(defvar lyskom-charset-alist
  `(((ascii)						. iso-8859-1)
    ((ascii latin-iso8859-1)				. iso-8859-1)
    ,@(condition-case nil
	  (when (lyskom-coding-system-get 'mule-utf-8 'safe-charsets)
	    (list (cons (lyskom-coding-system-get 'mule-utf-8 
						  'safe-charsets)
			'utf-8)))
	(error nil))
    ((ascii latin-iso8859-2)				. iso-8859-2)
    ((ascii latin-iso8859-3)				. iso-8859-3)
    ((ascii latin-iso8859-4)				. iso-8859-4)
    ((ascii cyrillic-iso8859-5)				. iso-8859-5)
    ((ascii arabic-iso8859-6)				. iso-8859-6)
    ((ascii greek-iso8859-7)				. iso-8859-7)
    ((ascii hebrew-iso8859-8)				. iso-8859-8)
    ((ascii latin-iso8859-9)				. iso-8859-9)
    ((ascii latin-iso8859-15)				. iso-8859-15)
    ((ascii latin-jisx0201
	    japanese-jisx0208-1978 japanese-jisx0208)	. iso-2022-jp)
    ((ascii latin-jisx0201
	    katakana-jisx0201 japanese-jisx0208)	. shift_jis)
    ((ascii korean-ksc5601)				. euc-kr)
    ((ascii chinese-gb2312)				. gb2312)
    ((ascii chinese-big5-1 chinese-big5-2)		. big5)
    ((ascii latin-iso8859-1 greek-iso8859-7
	    latin-jisx0201 japanese-jisx0208-1978
	    chinese-gb2312 japanese-jisx0208
	    korean-ksc5601 japanese-jisx0212)		. iso-2022-jp-2)
    ))


(defun lyskom-mime-string-charset (data)
  (let* ((cs (lyskom-find-charset-string data))
         (the-cs (and (eq (length cs) 1) (car cs)))
         (tmp lyskom-charset-alist)
         (best-guess (let ((system nil))
                       (while (and tmp cs)
                         (if (or (lyskom-subset-p cs (car (car tmp)))
                                 (eq the-cs (cdr (car tmp))))
                             (setq system (cdr (car tmp)) tmp nil)
                           (setq tmp (cdr tmp))))
                       system)))
    (or
     best-guess
     (lyskom-xemacs-or-gnu
      lyskom-server-coding-system
      (let ((coding (lyskom-find-coding-systems-for-charsets cs)))
        (while (and (car coding)
                    (null (or (lyskom-coding-system-get (car coding) 'mime-charset)
                              (lyskom-coding-system-get (car coding) :mime-charset))))
          (setq coding (cdr coding)))
        (and (car coding)
             (or (lyskom-coding-system-get (car coding) 'mime-charset)
                 (lyskom-coding-system-get (car coding) :mime-charset)))))
    lyskom-server-coding-system)))

(defun lyskom-mime-charset-coding-system (charset)
  (condition-case nil
      (and (lyskom-check-coding-system charset)
           charset)
    (error 'raw-text)))

(defun lyskom-mime-encode-string (data)
  (let* ((mime-charset (lyskom-mime-string-charset data))
         (coding-system (lyskom-mime-charset-coding-system mime-charset)))
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
