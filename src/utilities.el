;;;;; -*- emacs-lisp -*-
;;;;; $Id: utilities.el,v 43.4 1996-08-30 11:44:53 byers Exp $
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
;;;; File: utilities.el
;;;;
;;;; This file contains general lisp utility functions and
;;;; lyskom-specific utility functions (such as date formatting and
;;;; minibuffer reading)
;;;;


(setq lyskom-clientversion-long
      (concat lyskom-clientversion-long
	      "$Id: utilities.el,v 43.4 1996-08-30 11:44:53 byers Exp $\n"))


;;;
;;; Lisp utility functions
;;;

(lyskom-provide-function rassoc (key list)
  "Return non-nil if KEY is `equal' to the cdr of an element of LIST.
The value is actually the element of LIST whose cdr is KEY."
  (catch 'rassoc
    (while list
      (if (equal key (cdr (car list)))
          (throw 'rassoc (car list))
        (setq list (cdr list))))
    (throw 'rassoc nil)))


(lyskom-provide-function rassq (key list)
  "Return non-nil if KEY is `eq' to the cdr of an element of LIST.
The value is actually the element of LIST whose cdr is KEY."
  (catch 'rassq
    (while list
      (if (eq key (cdr (car list)))
          (throw 'rassq (car list))
        (setq list (cdr list))))
    (throw 'rassq nil)))

(defsubst listify-vector (vector)
  "Turn VECTOR into a list"
  (append vector nil))

(defun reverse-assoc (key cache)
  "Same as assoc, but searches on last element in a list"
  (reverse (assoc key (mapcar (function reverse) cache))))


(defun nfirst (n list)
  "Return a list of the N first elements of LIST."
  (if (or (<= n 0) (not list))
      nil
    (cons (car list) (nfirst (1- n) (cdr list)))))


(defun skip-first-zeros (list)
  (while (and list (zerop (car list)))
    (setq list (cdr list)))
  list)


(defun filter-list (test list)
  (cond ((null list) '())
	((apply test (car list) nil)
	 (cons (car list) (filter-list test (cdr list))))
	(t (filter-list test (cdr list)))))

;;;============================================================
;;;
;;; Utility functions.
;;;
;;; These should be shared in LysKOM
;;;

(defun copy-tree (l)
  "Recursively copy the list L"
  (cond ((atom l) l)
        (t (cons (copy-tree (car l))
                 (copy-tree (cdr l))))))

(defun functionp (fn)
  "Return t if fn is callable"
  (or (byte-code-function-p fn)
      (and (listp fn)
           (eq 'lambda (car fn)))
      (and (symbolp fn)
           (symbol-function fn))))


(defun regexpp (re)
  "Return non-nil if RE looks like a valid regexp."
  (let ((result t))
    (save-match-data
      (condition-case nil
          (string-match re "")
        (error (setq result nil))))
    result))


(defun mapcar2 (fn seq1 seq2)
  (let (result)
    (while (and seq1 seq2)
      (setq result (cons (funcall fn (car seq1) (car seq2)) result))
      (setq seq1 (cdr seq1)
            seq2 (cdr seq2)))
    (nreverse result)))


(defun lyskom-maxint ()
  (let ((n 1) (l nil))
    (while (> n 0)
      (setq l (cons n l))
      (setq n (* 2 n)))
    (apply '+ l)))


;;;
;;; LysKOM utility functions
;;;

(defvar lyskom-default-collate-table
" 	

 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\][^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ\\][~���������������������������������!������������������������������AAAA\\[\\CEEEEIIII�NOOOO]*]UUUYY��AAAA\\[\\CEEEEIIII�NOOOO]/]UUUYY��")



(defsubst lyskom-unicase-char (c)
  "Smash case and diacritical marks on c." 
  (aref lyskom-collate-table c))

(defun lyskom-unicase (s)
  "Smash case and diacritical marks of all chars in s." 
  (lyskom-save-excursion
   (set-buffer (process-buffer lyskom-blocking-process))
   (let ((l (length s))
	 (s2 (copy-sequence s)))
     (while (> l 0)
       (setq l (1- l))
       (aset s2 l (lyskom-unicase-char (aref s2 l))))
     s2)))
