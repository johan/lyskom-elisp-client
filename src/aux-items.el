;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: aux-items.el,v 44.13 1999-07-14 07:34:49 davidk Exp $
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
;;;; File: aux-items.el
;;;;
;;;; Implementation of aux-item specific stuff.
;;;; Hopefulle more of this will be made more general in the future.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: aux-items.el,v 44.13 1999-07-14 07:34:49 davidk Exp $\n"))

;;; (eval-when-compile
;;;   (require 'lyskom-defvar "defvar.el")
;;;   (require 'lyskom-vars "vars.el")
;;;   (require 'lyskom-types "komtypes.el"))


(def-kom-var lyskom-aux-item-definitions nil
  "List of aux item definitions.")

(defmacro def-aux-item (name number &rest data)
  (` (setq lyskom-aux-item-definitions 
           (add-to-list 'lyskom-aux-item-definitions
                        (cons (, number)
                              (cons (cons 'name (quote (, name)))
                                    (quote (, data))))))))

(defun lyskom-aux-item-definition->name (def)
  (cdr (assq 'name def)))

(defun lyskom-aux-item-definition->number (def)
  (car def))

(defun lyskom-map-aux-items (fn text-stat)
  "Apply FN to every aux item in TEXT-STAT."
  (let ((items (text-stat->aux-items text-stat)))
    (while items
      (when (and (car items) 
                 (not (aux-item-flags->deleted (aux-item->flags items))))
        (funcall fn (car items)))
      (setq items (cdr items)))))



(defun lyskom-aux-item-definition-call (def method &rest args)
  (when (lyskom-aux-item-p def)
    (setq def (lyskom-find-aux-item-definition def)))
  (cond ((listp method)
         (let ((result nil))
           (while method
             (when (lyskom-aux-item-definition-field def (car method))
               (setq result
                     (apply (lyskom-aux-item-definition-field def
                                                              (car method))
                            args))
               (setq method nil))
             (setq method (cdr method)))
           result))
        (t (when (lyskom-aux-item-definition-field def method)
             (apply (lyskom-aux-item-definition-field def method) args)))))


(defun lyskom-aux-item-definition-field (def method)
  (when (lyskom-aux-item-p def)
    (setq def (lyskom-find-aux-item-definition def)))
  (cdr (assq method def)))


(defun lyskom-find-aux-item-definition (aux)
  (assq (aux-item->tag aux) lyskom-aux-item-definitions))

(defun lyskom-aux-item-terminating-button (item obj)
  (if obj
       (lyskom-format " %#1@%[[*]%]" 
                      (lyskom-default-button 'aux 
                                             (cond ((lyskom-text-stat-p obj)
                                                    (list 'text
                                                          (text-stat->text-no
                                                           obj)
                                                          (aux-item->aux-no
                                                           item)))
                                                   ((lyskom-conf-stat-p obj)
                                                    (list 'conf
                                                          (conf-stat->conf-no
                                                           obj)
                                                          (aux-item->aux-no
                                                           item)))
                                                   (t item))))
     ""))

;;; ======================================================================

(def-aux-item content-type 1
  (text-print-when . never) 
  (parse . lyskom-parse-content-type)
  (text-print . lyskom-print-content-type)
  (info  . lyskom-aux-item-info))

(def-aux-item fast-reply 2
  (text-print-when . footer)
  (parse . nil)
  (text-print . lyskom-print-fast-reply)
  (info . lyskom-aux-item-info))

(def-aux-item cross-reference 3
  (status-print . lyskom-status-print-cross-reference)
  (text-print-when . comment)
  (parse . lyskom-parse-cross-reference)
  (text-print . lyskom-print-cross-reference)
  (edit-insert . lyskom-edit-insert-cross-reference)
  (info  . lyskom-aux-item-info))

(def-aux-item no-comments 4
  (text-print-when . footer)
  (parse . lyskom-parse-no-comments)
  (text-print . lyskom-print-no-comments)
  (edit-insert . lyskom-edit-insert-no-comments)
  (info  . lyskom-aux-item-info))

(def-aux-item personal-comment 5
  (text-print-when . footer)
  (parse . lyskom-parse-personal-comments)
  (text-print . lyskom-print-personal-comments)
  (edit-insert . lyskom-edit-insert-personal-comments)
  (info  . lyskom-aux-item-info))

(def-aux-item request-confirmation 6
  (text-print-when . header)
  (parse . lyskom-parse-request-confirmation)
  (text-print . lyskom-print-request-confirmation)
  (edit-insert . lyskom-edit-insert-request-confirmation)
  (info  . lyskom-aux-item-info)
  (read-action . lyskom-request-confirmation-action))

(def-aux-item read-confirm 7
  (text-print-when . header)
  (text-print . lyskom-print-read-confirm)
  (info  . lyskom-aux-item-info))

(def-aux-item redirect 8
  (print . lyskom-print-redirect)
  (info  . lyskom-aux-item-info))

(def-aux-item x-face 9
  (print . lyskom-print-xface)
  (info  . lyskom-aux-item-info))

(def-aux-item alternate-name 10
  (text-print-when . header)
  (text-print . lyskom-print-alternate-name)
  (info  . lyskom-aux-item-info))

(def-aux-item pgp-signature 11
  (info  . lyskom-aux-item-info))

(def-aux-item pgp-public-key 12
  (info  . lyskom-aux-item-info))

(def-aux-item e-mail-address 13
  (info  . lyskom-aux-item-info))

(def-aux-item faq-text 14
  (info . lyskom-aux-item-info)
  (status-print . lyskom-status-print-faq-text))

(def-aux-item creating-software 15
  (info . lyskom-aux-item-info)
  (text-print-when . header)
  (text-print . lyskom-print-creating-software))

(def-aux-item mx-author 16
  (info . lyskom-aux-item-info))

(def-aux-item mx-from 17
  (info . lyskom-aux-item-info))

(def-aux-item mx-reply-to 18
  (info . lyskom-aux-item-info))

(def-aux-item mx-to 19
  (info . lyskom-aux-item-info))

(def-aux-item mx-cc 20
  (info . lyskom-aux-item-info))

(def-aux-item mx-date 21
  (info . lyskom-aux-item-info))

(def-aux-item mx-message-id 22
  (info . lyskom-aux-item-info))

(def-aux-item mx-in-reply-to 23
  (info . lyskom-aux-item-info))

(def-aux-item mx-misc 24
  (info . lyskom-aux-item-info))

(def-aux-item mx-allow-filter 25
  (info . lyskom-aux-item-info))

(def-aux-item mx-reject-forward 26
  (info . lyskom-aux-item-info))

(def-aux-item notify-comments 27
  (info . lyskom-aux-item-info))

(def-aux-item faq-for-conf 28
  (text-print . lyskom-print-faq-for-conf)
  (text-print-when . header)
  (info . lyskom-aux-item-info))




;;; ================================================================



(defun lyskom-aux-item-info (item header)
  (let ((def (lyskom-find-aux-item-definition item)))
    
    (concat 
     (lyskom-get-string 'aux-item-for)
     header
     "\n"
     (lyskom-format
      'aux-item-info
      (aux-item->aux-no item)
      (aux-item->tag item)
      (if def
          (lyskom-aux-item-definition->name def)
        "unknown")
      (aux-item->creator item)
      (lyskom-return-date-and-time (aux-item->sent-at item))
      (if (aux-item-flags->deleted (aux-item->flags item))
          (format "(%s)" (lyskom-get-string 'deleted))
        "")
      (mapconcat 'identity
                 (delq nil
                       (list
                        (and (aux-item-flags->secret (aux-item->flags item))
                             (lyskom-get-string 'secret))
                        (and (aux-item-flags->anonymous (aux-item->flags item))
                             (lyskom-get-string 'hide-creator))
                        (and (aux-item-flags->inherit (aux-item->flags item))
                             (lyskom-get-string 'inherit))))
                 ", ")
      (cond ((not (aux-item-flags->inherit (aux-item->flags item)))
             (lyskom-get-string 'no-inheritance))
            ((zerop (aux-item->inherit-limit item))
             (lyskom-get-string 'unlimited-inherit))
            ((eq 1  (aux-item->inherit-limit item))
             (lyskom-get-string 'no-more-inherit))
            (t (lyskom-format 'inherit-steps
                              (1- (aux-item->inherit-limit item)))))
      (aux-item->data item)))))

                 

(defun lyskom-print-content-type (item &optional obj)
  (concat (lyskom-format 'content-type-aux (aux-item->data item))
          (lyskom-aux-item-terminating-button item obj)))

(defun lyskom-parse-content-type ()
  (and (looking-at (lyskom-get-string 'content-type-regexp))
       (match-string 1)))

(defun lyskom-print-fast-reply (item &optional obj)
  (concat (lyskom-format 'fast-reply-aux
                         (aux-item->data item)
                         (aux-item->creator item))
          (lyskom-aux-item-terminating-button item obj)))

(defun lyskom-parse-cross-reference ()
  (or (and (looking-at (lyskom-get-string 'cross-reference-text-regexp))
           (concat "T" (match-string 1)))
      (and (looking-at (lyskom-get-string 'cross-reference-conf-regexp))
           (concat "C" (match-string 1)))
      (and (looking-at (lyskom-get-string 'cross-reference-pers-regexp))
           (concat "P" (match-string 1)))))

(defun lyskom-edit-insert-cross-reference (item pers)
  (lyskom-print-cross-reference item nil pers))

(defun lyskom-print-cross-reference (item &optional obj pers)
  (let ((pers (if (and (zerop (aux-item->creator item))
                       pers)
                  pers
                (aux-item->creator item))))
    (concat
     (cond ((string-match "^P\\([0-9]+\\)" (aux-item->data item))
            (lyskom-format 'cross-reference-pers-aux 
                           (string-to-int 
                            (match-string 1 (aux-item->data item)))
                           pers
                           ))
           ((string-match "^C\\([0-9]+\\)" (aux-item->data item))
            (lyskom-format 'cross-reference-conf-aux 
                           (string-to-int 
                            (match-string 1 (aux-item->data item)))
                           pers
                           ))
           ((string-match "^T\\([0-9]+\\)" (aux-item->data item))
            (lyskom-format 'cross-reference-text-aux 
                           (string-to-int 
                            (match-string 1 (aux-item->data item)))
                           pers
                           ))
           (t (lyskom-format 'strange-cross-reference 
                             (aux-item->data item)
                             pers
                             )))
     (lyskom-aux-item-terminating-button item obj)
     )))

(defun lyskom-status-print-cross-reference (item &optional obj pers)
  (lyskom-insert 
   (concat
    (cond ((string-match "^P\\([0-9]+\\)" (aux-item->data item))
           (lyskom-format 'cross-reference-pers-status-aux 
                          (string-to-int 
                           (match-string 1 (aux-item->data item)))
                          (aux-item->creator pers)
                          ))
          ((string-match "^C\\([0-9]+\\)" (aux-item->data item))
           (lyskom-format 'cross-reference-conf-status-aux 
                          (string-to-int 
                           (match-string 1 (aux-item->data item)))
                          pers
                          ))
          ((string-match "^T\\([0-9]+\\)" (aux-item->data item))
           (lyskom-format 'cross-reference-text-status-aux 
                          (string-to-int 
                           (match-string 1 (aux-item->data item)))
                          (aux-item->creator pers)
                          ))
          (t (lyskom-format 'strange-cross-reference-status 
                            (aux-item->data item)
                            (aux-item->creator pers)
                            )))
    (lyskom-aux-item-terminating-button item obj)
    "\n")))  

(defun lyskom-parse-no-comments ()
  (and (looking-at (lyskom-get-string 'no-comments-regexp))
       ""))

(defun lyskom-print-no-comments (item &optional obj)
  (concat (lyskom-format 'no-comments-aux)
          (lyskom-aux-item-terminating-button item obj)))

(defun lyskom-edit-insert-no-comments (item &optional obj)
  (lyskom-format 'no-comments-edit-aux))

(defun lyskom-parse-personal-comments ()
  (and (looking-at (lyskom-get-string 'personal-comment-regexp))
       ""))

(defun lyskom-print-personal-comments (item &optional obj)
  (concat (lyskom-format 'personal-comment-aux)
          (lyskom-aux-item-terminating-button item obj)))

(defun lyskom-edit-insert-personal-comments (item &optional obj)
  (lyskom-format 'personal-comment-edit-aux))

(defun lyskom-parse-request-confirmation ()
  (and (looking-at (lyskom-get-string 'request-confirmation-regexp))
       ""))

(defun lyskom-print-request-confirmation (item &optional obj)
  (concat (lyskom-format 'request-confirmation-aux
                         (aux-item->creator item))
          (lyskom-aux-item-terminating-button item obj)))

(defun lyskom-edit-insert-request-confirmation (item &optional obj)
  (lyskom-format 'request-confirmation-edit-aux))

(defun lyskom-request-confirmation-action (text-stat)
  (let ((confirmations (text-stat-find-aux text-stat 7))
        (have-confirmation nil))
    (while confirmations
      (when (eq lyskom-pers-no (aux-item->creator (car confirmations)))
        (setq have-confirmation t)
        (setq confirmations nil))
      (setq confirmations (cdr confirmations)))
    (when  (not have-confirmation)
      (lyskom-scroll)
      (when (lyskom-j-or-n-p
             (lyskom-format (lyskom-get-string 'confirm-read-q)
                            text-stat))
        (let ((item (lyskom-create-aux-item 0 7 lyskom-pers-no
                                            nil 
                                            (lyskom-create-aux-item-flags
                                             nil nil nil nil nil nil nil nil)
                                            0 "")))
          (initiate-modify-text-info 'background
                                     nil
                                     (text-stat->text-no text-stat)
                                     nil
                                     (list item))
          (cache-del-text-stat (text-stat->text-no text-stat)))))))

(defun lyskom-print-read-confirm (item &optional obj)
  (concat 
   (lyskom-format 'read-confirm-aux
                  (aux-item->creator item)
                  (lyskom-return-date-and-time (aux-item->sent-at item)))
   (lyskom-aux-item-terminating-button item obj)))



(defun lyskom-print-redirect (item &optional obj)
  (concat
   (cond ((string-match "^E-mail:\\(.*\\)$" (aux-item->data item))
          (lyskom-format 'redirect-email-aux 
                         (match-string 1 (aux-item->data item))))
         ((string-match "^LysKOM:\\(.*\\)$" (aux-item->data item))
          (lyskom-format 'redirect-lyskom-aux 
                         (string-to-int 
                          (match-string 1 (aux-item->data item))))))
   (lyskom-aux-item-terminating-button item obj)))   


(defun lyskom-print-xface (item &optional obj)
  (lyskom-xemacs-or-gnu
   (lyskom-maybe-add-face-to-string item
                                    (make-string 0 ?X))
   nil))

(defun lyskom-maybe-add-face-to-string (item string)
  (lyskom-xemacs-or-gnu
   (if (null item)
       string
     (setq item (car item))
     (unless (find-face 'kom-xface)
       (make-face 'kom-xface))
     (let* ((h (concat "X-Face: " (aux-item->data item)))
            (g (intern h lyskom-xface-cache))
            (e (make-extent 0 (length string) string)))
       (if (boundp g)
           (setq g (symbol-value g))
         (set g (make-glyph
                 (list
                  (list 'global (cons '(tty) [nothing]))
                  (list 'global (cons '(win) 
                                      (vector 'xface ':data h))))))
         (setq g (symbol-value g))
         (set-glyph-face g 'kom-xface))
       (set-extent-begin-glyph e g)
       (set-extent-property e 'end-open nil)
       (set-extent-property e 'start-open nil)
       (set-extent-property e 'duplicable t)
       string))
   string))


(defun lyskom-print-alternate-name (item &optional obj)
  (concat "[" (aux-item->data item) "] "
          (lyskom-aux-item-terminating-button item obj)))

(defun lyskom-status-print-faq-text (item &optional obj)
  (lyskom-insert 
   (concat 
    (lyskom-format 'faq-in-text-aux (string-to-int (aux-item->data item)))
    (lyskom-aux-item-terminating-button item obj)
    "\n")))

(defun lyskom-print-faq-for-conf (item &optional obj)
  (concat 
   (lyskom-format 'faq-for-conf-aux (string-to-int (aux-item->data item)))
   (lyskom-aux-item-terminating-button item obj)))

(defun lyskom-print-creating-software (item &optional obj)
  (concat
   (lyskom-format 'creating-software-aux (aux-item->data item))
   (lyskom-aux-item-terminating-button item obj)))


(provide 'lyskom-aux-items)
