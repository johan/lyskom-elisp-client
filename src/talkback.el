;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: talkback.el,v 44.5 2009-03-08 12:20:14 byers Exp $
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
;;;; File: talkback.el
;;;;
;;;; Functions for handing bug reports
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: talkback.el,v 44.5 2009-03-08 12:20:14 byers Exp $\n"))

(defvar kom-bug-report-address "kom@lysator.liu.se")

(defvar lyskom-bug-report-special-vars 
  '((lyskom-conf-cache . (lambda (n v) (format "%d items" (length v))))
    (lyskom-uconf-cache . (lambda (n v) (format "%d items" (length v))))
    (lyskom-text-cache . (lambda (n v) (format "%d items" (length v))))
    (lyskom-text-mass-cache . (lambda (n v) (format "%d items" (length v))))
    (lyskom-default-password . "********")))


(defun lyskom-save-backtrace (&rest data)
  "Save the current backtrace"
  (lyskom-message "Saving debugging information...")
  (setq lyskom-backtrace-list (cons (list (current-time-string)
                                          (let* ((buffer (generate-new-buffer " *Backtrace*"))
                                                 (standard-output buffer))
                                            (backtrace)
                                            (prog1 (save-excursion
                                                     (set-buffer buffer)
                                                     (buffer-string))
                                              (kill-buffer buffer)))
                                          data)
                                    lyskom-backtrace-list))
  (lyskom-message "Saving debugging information...done"))

(defun lyskom-bug-insert-recent-messages (n)
  (let ((message-buf (get-buffer (lyskom-xemacs-or-gnu " *Message-Log*"
                                                       "*Messages*"))))
    (insert "\n\nRecent messages:\n")
    (if message-buf
        (insert-buffer-substring message-buf
                                 (save-excursion
                                   (set-buffer message-buf)
                                   (goto-char (point-max))
                                   (forward-line -10)
                                   (point))
                                  (save-excursion
                                    (set-buffer message-buf)
                                    (point-max))))))


(defun lyskom-insert-bug-report ()
  (insert 
   (save-excursion 
     (set-buffer lyskom-buffer)
     (let ((v lyskom-server-version-info))
       (format "\
This bug report will be sent to the LysKOM development team,
not to your local site managers.

Please write in english or swedish, because the LysKOM maintainers (if
there are any) are too lazy to learn other languages just so they can
read your bug reports.

Client time:    %s
Emacs version:  %s (%s)
Client version: %s (%s)
LysKOM server:  %s:%S
Server version: %s %s %S
Current person: %S (%s)
Command line:   %S

Please describe the precise symptoms of the bug and exactly what
actions triggered the bug. Be as detailed as possible. Do not try to
make any conclusions of your own.

"
               (current-time-string)
               (emacs-version)
               (if enable-multibyte-characters "MULE" "no MULE")
               lyskom-clientversion
               (if lyskom-mule-compiled "MULE" "no MULE")
               lyskom-server-name
               lyskom-server-port
               (if v (version-info->server-software v) "unknown")
               (if v (version-info->software-version v) "unknown")
               (if v (version-info->protocol-version v) "unknown")
               lyskom-pers-no
               (condition-case nil
                   (uconf-stat->name (cache-get-uconf-stat lyskom-pers-no))
                 (error ""))
               command-line-args
               ))))
  (let ((saved-place (point)))
    (insert "\n\n")
    (lyskom-bug-insert-recent-messages 20)
    (insert "\n\nSaved backtraces:\n\n")

    (mapc (lambda (el)
              (insert (elt el 0))        ; Time
              (insert "\n")
              (insert (elt el 1))        ; Backtrace
              (insert "\n")
              (mapcar (lambda (data)
                        (insert "Data: ")
                        (prin1 data)
                        (insert "\n"))
                      (elt el 2)))
            lyskom-backtrace-list)

    (insert "\n\nAll variables:\n\n")
   
    (mapatoms 
     (lambda (var)
       (when (and (save-excursion (set-buffer lyskom-buffer) (boundp var))
                  (or (string-match "^\\(lys\\)?kom" (symbol-name var))))
         (message "Collecting system information...%s" var)
         (insert (format "(%s " var))
         (cond ((assq var lyskom-bug-report-special-vars)
                (let ((fn (cdr (assq var lyskom-bug-report-special-vars))))
                  (cond ((functionp fn) (lyskom-bug-report-insert-value (funcall fn var (lyskom-default-value var))))
                        (t (lyskom-bug-report-insert-value fn)))))
               (t (lyskom-bug-report-insert-value (lyskom-default-value var))))
         (insert ")\n"))))

    (goto-char saved-place)
    ))
                                 
(defun lyskom-bug-report-insert-value (v)
  (cond ((eq t v) (insert "t"))
        ((eq nil v) (insert "nil"))
        ((symbolp v) (insert "'") (princ v))
        (t (prin1 v)))) 
                          
(defun kom-bug-report (topic)
  "Report a LysKOM bug"
  (interactive "sBug Subject: ")
  (let ((buf lyskom-buffer))
    (unless buf
      (error "Switch to a LysKOM buffer to report bugs."))


    (when (lyskom-xemacs-or-gnu
           (progn (compose-mail kom-bug-report-address topic) t)
           (mail nil kom-bug-report-address topic))
      (goto-char (point-min))
      (re-search-forward (concat "^" (regexp-quote mail-header-separator) "\n\
"))

      (let ((standard-output (current-buffer))) 
        (message "Collecting system information...")
        (make-local-variable 'lyskom-buffer)
        (setq lyskom-buffer buf)
        (lyskom-insert-bug-report)
        (message "Collecting system information...done")))))


  
