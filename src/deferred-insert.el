;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: deferred-insert.el,v 44.1 1998-06-02 12:14:32 byers Exp $
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
;;;; File: deferred-insert.el
;;;;
;;;; This file includes functions for deffering insertion of
;;;; information into the LysKOM buffer.
;;;;

;;;
;;; How to defer a peice of text:
;;; =============================
;;;
;;;  1. Insert some temporary text. Use the variable
;;;     `lyskom-defer-indicator' as a placeholder.
;;; 
;;;  2. Create a defer-info-structure by calling
;;;     `lyskom-create-defer-info' with the following parameters
;;;
;;;        SERVER-CALL - the call to get the data (initate-get-*)
;;;        CALL-ARG    - the argument for the server call. This is
;;;                      limited to a single argument, which is enough
;;;                      for get-conf-stat and friends.
;;;        HANDLER     - the function to be called to insert the
;;;                      "real" data.
;;;        POS         - a marker indicating where the insertion
;;;                      should be placed.
;;;        DEL-CHARS   - an integer indicating how many characters
;;;                      from POS on should be replaced by the "real"
;;;                      text.
;;;        FORMAT      - a format string for inserting the real
;;;                      data. This should normally only contain a
;;;                      single format atom using argument 1
;;;                      (i.e. "%#1P").
;;;        DATA        - any data that you might want to use in
;;;                      HANDLER.
;;;
;;;  3. Call `lyskom-defer-insertion' with the defer-info as argument.
;;;
;;;  4. Write a handler function that takes two arguments, the server
;;;     reply and the defer-info. This function should replace the
;;;     temporary text. A convenient way to do this is to use
;;;     `lyskom-replace-deferred'.
;;;
;;;  5. Sit back and watch it work.
;;;  
;;;  Notes:
;;;  
;;;  POS must be located before the temporary text. DEL-CHARS should
;;;  usually be set to (length lyskom-defer-indicator). Don't count on
;;;  it to be 5.
;;;
;;;  Steps 1 and 2 are often implemented the other way around, or at
;;;  least the POS parameter is determined before any text is
;;;  inserted.
;;;
;;;  You should only defer text if kom-deferred-printing is non-nil.
;;;



;;; Type: defer-info

(defun lyskom-create-defer-info (server-call call-par handler pos
					     del-chars format
					     &optional data)
  (cons 'DEFER-INFO
	(vector server-call call-par handler pos del-chars format data
		lyskom-last-viewed)))

(defun lyskom-defer-info-p (obj)
  (and (consp obj)
       (eq (car obj) 'DEFER-INFO)))

(defun defer-info->server-call (di) (aref (cdr di) 0))
(defun defer-info->call-par (di)    (aref (cdr di) 1))
(defun defer-info->handler (di)     (aref (cdr di) 2))
(defun defer-info->pos (di)         (aref (cdr di) 3))
(defun defer-info->del-chars (di)   (aref (cdr di) 4))
(defun defer-info->format (di)      (aref (cdr di) 5))
(defun defer-info->data (di)        (aref (cdr di) 6))
(defun defer-info->last-viewed (di) (aref (cdr di) 7))

(defun set-defer-info->server-call (di x) (aset (cdr di) 0 x))
(defun set-defer-info->call-par (di x)    (aset (cdr di) 1 x))
(defun set-defer-info->handler (di x)     (aset (cdr di) 2 x))
(defun set-defer-info->pos (di x)         (aset (cdr di) 3 x))
(defun set-defer-info->del-chars (di x)   (aset (cdr di) 4 x))
(defun set-defer-info->format (di x)      (aset (cdr di) 5 x))
(defun set-defer-info->data (di x)        (aset (cdr di) 6 x))
(defun set-defer-info->last-viewed (di x) (aset (cdr di) 7 x))


(defun lyskom-defer-insertion (defer-info)
  "Defer insertion of something.
The insertion will be at (point)."
  (set-defer-info->last-viewed defer-info lyskom-last-viewed)
  ;; (goto-char (defer-info->pos defer-info))
  (funcall (intern-soft (concat "initiate-"
				(symbol-name (defer-info->server-call
					       defer-info)))) 
	   'deferred
	   (defer-info->handler defer-info)
	   (defer-info->call-par defer-info)
	   defer-info))

(defun lyskom-replace-deferred (defer-info &rest replacement-data)
  "Replace some defered text."
  (save-excursion
    (goto-char (defer-info->pos defer-info))
    (apply 'lyskom-format-insert-at-point
	   (defer-info->format defer-info)
	   replacement-data)
    (let ((inhibit-read-only t))
      (delete-char (defer-info->del-chars defer-info)))
    (set-marker (defer-info->pos defer-info) nil))
  (if lyskom-executing-command
      nil
    (let ((window (get-buffer-window lyskom-buffer)))
      (if window
	  (if (pos-visible-in-window-p (point-max) window)
	      nil
	    ;; This means that this insertion moved point out of the
	    ;; window. The scrolling becomes tricky. One big problem is
	    ;; that we can't use lyskom-last-viewed, because it has been
	    ;; updated to the new prompt. Until that is solved we make
	    ;; sure that we never scroll.
	    ;;
	    ;; The solution is to save lyskom-last-viewed in the defer-info
	    (save-selected-window
	      (select-window window)
	      (lyskom-scroll))
	    ;; (move-to-window-line -1)
	    ;; (vertical-motion 1)
	    ;; (if (not (pos-visible-in-window-p))
	    ;;     (forward-char -1))
	    )))))


(defun lyskom-deferred-insert-conf (conf-stat defer-info)
  "Insert the name of a conference at a previously reserved place."
  (lyskom-replace-deferred
   defer-info
   (if (null conf-stat)
       (lyskom-format 
	(or (defer-info->data defer-info)
	    (if (= (aref (defer-info->format defer-info)
			 (1- (length (defer-info->format defer-info))))
		   ?P)
		(if (= (defer-info->call-par defer-info) 0)
		    'person-is-anonymous
		  'person-does-not-exist)
	      'conference-does-not-exist))
	(defer-info->call-par defer-info))
     conf-stat)
   (text-properties-at (defer-info->pos defer-info))))


