;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: mime.el,v 44.8 2002-09-10 22:04:53 byers Exp $
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
	      "$Id: mime.el,v 44.8 2002-09-10 22:04:53 byers Exp $\n"))

(defvar lyskom-charset-alist
  '(((ascii)						. us-ascii)
    ((ascii latin-iso8859-1)				. iso-8859-1)
    ((ascii latin-iso8859-2)				. iso-8859-2)
    ((ascii latin-iso8859-3)				. iso-8859-3)
    ((ascii latin-iso8859-4)				. iso-8859-4)
    ((ascii cyrillic-iso8859-5)				. iso-8859-5)
;;; ((ascii cyrillic-iso8859-5)				. koi8-r)
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
    ;; ((ascii latin-iso8859-1 greek-iso8859-7
    ;;         latin-jisx0201 japanese-jisx0208-1978
    ;;         chinese-gb2312 japanese-jisx0208
    ;;         korean-ksc5601 japanese-jisx0212
    ;;         chinese-cns11643-1 chinese-cns11643-2)      . iso-2022-int-1)
    ))


(defun lyskom-mime-string-charset (data)
  (let* ((cs (find-charset-string data))
         (tmp lyskom-charset-alist)
         (best-guess (let ((system nil))
                       (while (and tmp cs)
                         (if (lyskom-subset-p cs (car (car tmp)))
                             (setq system (cdr (car tmp)) tmp nil)
                           (setq tmp (cdr tmp))))
                       system)))
    (or
     best-guess
     (lyskom-xemacs-or-gnu
      lyskom-server-coding-system
      (let ((coding (find-coding-systems-for-charsets cs)))
        (while (and (car coding)
                    (null (coding-system-get (car coding) 'mime-charset)))
          (setq coding (cdr coding)))
        (and (car coding)
             (coding-system-get (car coding) 'mime-charset))))
    lyskom-server-coding-system)))

(defun lyskom-mime-charset-coding-system (charset)
  (condition-case nil
      (and (check-coding-system charset)
           charset)
    (error 'raw-text)))

(defun lyskom-mime-encode-string (data)
  (let* ((mime-charset (lyskom-mime-string-charset data))
         (coding-system (lyskom-mime-charset-coding-system mime-charset)))
    (when (and mime-charset coding-system)
      (cons mime-charset (encode-coding-string data coding-system)))))

(defun lyskom-mime-decode-string (data charset)
  (let* ((coding-system (lyskom-mime-charset-coding-system charset)))
    (if coding-system
        (decode-coding-string data coding-system)
      data)))

(defun lyskom-mime-decode-content-type (data)
  (string-match "^\\([^;]*\\)\\(;.*charset=\\([^;]*\\)\\)?" data)
  (cons (match-string 1 data)
        (if (match-string 3 data)
            (intern (match-string 3 data))
          nil)))

(provide 'lyskom-mime)
