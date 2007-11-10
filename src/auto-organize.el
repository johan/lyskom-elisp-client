;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: auto-organize.el,v 44.3 2007-11-10 09:53:50 byers Exp $
;;;;; Copyright (C) 2007 David Byers <davby@ida.liu.se>
;;;;;
;;;;; This file is part of the LysKOM Emacs LISP client.
;;;;; 
;;;;; This is free software; you can redistribute it and/or modify it
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
;;;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, 
;;;;; MA 02139, USA.
;;;;;


(defvar kom-auto-organize-list nil
  "This variable defines how to auto-organize LysKOM conference.

Each element of the list configures auto-organize for a given
conference. Each element of kom-auto-organize-list has the 
format (CONF . AUTHORS), where CONF is the conference number to
auto-organize, or t to auto-organize all supervised conferences.

AUTHORS is also a list, where each element has the format
\(AUTHOR . ACTIONS), where AUTHOR is either a number indicating
for which author the ACTIONS apply, or t, to indicate that the
actions apply to all authors.

ACTIONS is also a list, where each element has the format
\(REGEXP . FUNC). REGEXP is matched against every text viewed in
CONF, and if a match is found, then FUNC is called with a list
containing the conf-stat of the matched resipient (or conf-stats
of all recipients if the default match was used). If the function
returns non-nil, no more actions will be applied.")

(defun kom-auto-organize ()
  (let* ((text-stat lyskom-view-text-text-stat)
	 (text-mass lyskom-view-text-text)
	 (rcpt-confs (make-collector))
	 (authors nil))

    ;; Get the conf-stat for every recipient we supervise

    (lyskom-traverse misc (text-stat->misc-info-list text-stat)
      (when (memq (misc-info->type misc) '(RECPT BCC-RECPT CC-RECPT))
	(initiate-get-conf-stat 'follow 
				'collector-push 
				(misc-info->recipient-no misc)
				rcpt-confs)))
    (lyskom-wait-queue 'follow)
    (setq rcpt-confs 
	  (nreverse 
	   (cons t (delq nil (mapcar
			       (lambda (conf-stat) 
				 (and (or t (lyskom-i-am-supervisor conf-stat t))
				      conf-stat))
			       (collector->value rcpt-confs))))))

    ;; Traverse all recipients, then all authors

    (setq authors (list (text-stat->author text-stat) t))

    (when (> (length rcpt-confs) 1)

      (catch 'kom-auto-organize-done

	;; Look for a match for each recuipient

	(lyskom-traverse rcpt rcpt-confs
	  (let ((rcpt-data (assq (if (atom rcpt)
				     rcpt
				   (conf-stat->conf-no rcpt))
				 kom-auto-organize-list)))

	    ;; If a match for the recipient is found, look for 
	    ;; a match for the author

	    (when rcpt-data
	      (lyskom-traverse author authors
		(let ((author-data (assq author (cdr rcpt-data))))

		  ;; If a match for the author is found, look for
		  ;; a match for the action regexp

		  (when author-data
		    (lyskom-traverse action (cdr author-data)
		      (when (and (or (eq t (car action))
				     (let ((case-fold-search t))
				       (string-match 
					(car action) 
					(text->decoded-text-mass text-mass
								 text-stat))))
				 (funcall (cdr action) 
					  (if (eq rcpt t)
                                              (butlast rcpt-confs 1)
                                            (list rcpt))
					  text-stat
					  text-mass))
			(throw 'kom-auto-organize-done t)))))))))))))


(defun kom-auto-organize-sub-recipient (confs text-stat text-mass)
  "Automatically subtract recipient from text (for kom-auto-organize)"
  (let ((to-sub (cons (text-stat->text-no text-stat)
		      (lyskom-get-text-attachments text-stat))))
    (lyskom-traverse misc (text-stat->misc-info-list text-stat)
      (when (eq (misc-info->type misc) 'FOOTN-IN)
	(setq to-sub (cons (misc-info->footn-in misc) footnotes))))

    (lyskom-traverse text-no to-sub
      (lyskom-traverse conf-stat confs
        (lyskom-format-insert-before-prompt 
         "Subtraherar text %#1n från %#2M i bakgrunden...%#3D\n"
         text-no
         conf-stat
         (lyskom-create-defer-info
          'sub-recipient
          (list text-no (conf-stat->conf-no conf-stat))
          (lambda (res defer-info)
            (if res
                (lyskom-replace-deferred defer-info "klart")
              (lyskom-replace-deferred defer-info "det gick inte")))
          nil nil "%#1s"))))))

(defun kom-auto-organize-debug (confs text-stat text-mass) 
  (let ((to-sub (cons (text-stat->text-no text-stat)
		      (lyskom-get-text-attachments text-stat))))
    (lyskom-traverse misc (text-stat->misc-info-list text-stat)
      (when (eq (misc-info->type misc) 'FOOTN-IN)
	(setq to-sub (cons (misc-info->footn-in misc) footnotes))))

    (lyskom-traverse text-no to-sub
      (lyskom-traverse conf-stat confs
        (lyskom-format-insert-before-prompt 
         "Subtraherar text %#1n från %#2M i bakgrunden...%#3D\n"
         text-no
         conf-stat
         (lyskom-create-defer-info
          'user-active
          nil
          (lambda (res defer-info)
            (if res
                (lyskom-replace-deferred defer-info "klart")
              (lyskom-replace-deferred defer-info "det gick inte")))
          nil nil "%#1s"))))))

; (setq kom-auto-organize-list '((3715 (7871 ("See attachment" . kom-auto-organize-sub-recipient)))))
