;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: aux-items.el,v 44.47 2009-03-08 12:20:11 byers Exp $
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
;;;; File: aux-items.el
;;;;
;;;; Implementation of aux-item specific stuff.
;;;; Hopefulle more of this will be made more general in the future.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: aux-items.el,v 44.47 2009-03-08 12:20:11 byers Exp $\n"))

(def-kom-var lyskom-aux-item-definitions nil
  "List of aux item definitions.")

(defmacro def-aux-item (name number &rest data)
  ` (setq lyskom-aux-item-definitions 
           (add-to-list 'lyskom-aux-item-definitions
                        (cons ,number
                              (cons (cons 'name (quote ,name))
                                    (quote ,data))))))

(defun lyskom-aux-item-definition->name (def)
  (cdr (assq 'name def)))

(defun lyskom-aux-item-definition->number (def)
  (car def))


(defun lyskom-aux-item-try-call (item method default &rest args)
  (when (lyskom-aux-item-p item)
    (setq item (lyskom-find-aux-item-definition item)))
  (cond ((listp method)
         (let ((result nil)
               (found nil))
           (while method
             (when (lyskom-aux-item-definition-field item (car method))
               (setq result
                     (apply (lyskom-aux-item-definition-field item
                                                              (car method))
                            args)
                     found t
                     method nil))
             (setq method (cdr method)))
           (if found result default)))
        (t (if (lyskom-aux-item-definition-field item method)
             (apply (lyskom-aux-item-definition-field item method) args)
             default))))

(defun lyskom-aux-item-has-call (item method)
  "Return non-nil if aux-item ITEM has a defined method METHOD"
  (when (lyskom-aux-item-p item)
    (setq item (lyskom-find-aux-item-definition item)))
  (lyskom-aux-item-definition-field item method))

(defun lyskom-aux-item-call (def method &rest args)
  (apply 'lyskom-aux-item-try-call def method nil args))


(defun lyskom-aux-item-definition-field (def method)
  (when (lyskom-aux-item-p def)
    (setq def (lyskom-find-aux-item-definition def)))
  (cdr (assq method def)))


(defun lyskom-find-aux-item-definition (aux)
  (assq (aux-item->tag aux) lyskom-aux-item-definitions))

(defun lyskom-match-aux-items (item-list predicate)
  "Return a list of all aux-items in ITEM-LIST that match PREDICATE.
PREDICATE should receives a single aux-item as its argument and should
return non-nil if the item is to be included in the list."
  (let ((result nil))
    (while item-list
      (when (funcall predicate (car item-list))
        (setq result (cons (car item-list) result)))
      (setq item-list (cdr item-list)))
    (nreverse result)))

(defun lyskom-get-aux-item (item-list tag)
  "Return all aux-items in ITEM-LIST with tag TAG."
  (lyskom-match-aux-items item-list
                          (lambda (el) (eq (aux-item->tag el) tag))))


(defun lyskom-aux-item-terminating-button (item obj)
  (if obj
       (lyskom-format 
        " %#1@%[[*]%]" 
        (lyskom-default-button 
         'aux 
         (cond ((lyskom-text-stat-p obj) (list 'text
                                               (text-stat->text-no obj)
                                               (aux-item->aux-no item)))
               ((lyskom-conf-stat-p obj) (list 'conf
                                               (conf-stat->conf-no obj)
                                               (aux-item->aux-no item)))
               ((eq obj 'server) (list 'server nil (aux-item->aux-no item)))
               (t item))))
     ""))

(defun lyskom-aux-item-after-parse (item)
  (lyskom-aux-item-try-call item 'decode-data nil)
  item)

(defun lyskom-aux-item-output-data (item)
  (lyskom-aux-item-try-call item 
                            'encode-data
                            (aux-item->data item)
			    item))

(defun lyskom-aux-item-modify-list (item-list deleted added)
  "Return a copy of ITEM-LIST with items in DELETED removed and items
in ADDED added."
  (let ((new-nos (nconc (mapcar 'aux-item->aux-no deleted)
                        (mapcar 'aux-item->aux-no added))))
    (nconc (lyskom-filter-list (lambda (el) (not (memq (aux-item->aux-no el) new-nos)))
                        item-list)
           added)))

(defun lyskom-aux-item-validate (data &rest tests)
  "Validata aux-item data.
DATA is data to validate. TESTS are the tests to use.
A test can be a function or a regular expression to match.
Invalid tests are silently ignored."
  (not (lyskom-traverse test tests
         (condition-case nil
             (unless
                 (cond ((stringp test) (string-match test data))
                       ((functionp test) (funcall test data)))
               (lyskom-traverse-break t))
           (error (lyskom-traverse-break t))))))


;;; ======================================================================

(def-aux-item content-type 1
  (text-name aux-content-type-name)
  (parse . lyskom-parse-content-type)
  (text-print . lyskom-print-content-type)
  (info  . lyskom-aux-item-info))

(def-aux-item fast-reply 2
  (text-name aux-fast-reply-name)
  (parse . nil)
  (parse-data . lyskom-aux-item-decode-data)
  (encode-data . lyskom-aux-item-encode-data)
  (info . lyskom-aux-item-info))

(def-aux-item cross-reference 3
  (text-name aux-cross-reference-name)
  (status-print . lyskom-status-print-cross-reference)
  (text-print-when . comment)
  (parse . lyskom-parse-cross-reference)
  (parse-data . lyskom-aux-item-decode-data)
  (encode-data . lyskom-aux-item-encode-data)
  (text-print . lyskom-print-cross-reference)
  (edit-insert . lyskom-edit-insert-cross-reference)
  (info  . lyskom-aux-item-info))

(def-aux-item no-comments 4
  (text-name aux-no-comments-name)
  (text-print-when . footer)
  (parse . lyskom-parse-no-comments)
  (text-print . lyskom-print-no-comments)
  (edit-insert . lyskom-edit-insert-no-comments)
  (info  . lyskom-aux-item-info))

(def-aux-item personal-comment 5
  (text-name aux-personal-comment-name)
  (text-print-when . footer)
  (parse . lyskom-parse-personal-comments)
  (text-print . lyskom-print-personal-comments)
  (edit-insert . lyskom-edit-insert-personal-comments)
  (info  . lyskom-aux-item-info))

(def-aux-item request-confirmation 6
  (text-name aux-request-confirmation-name)
  (text-print-when . header)
  (parse . lyskom-parse-request-confirmation)
  (text-print . lyskom-print-request-confirmation)
  (edit-insert . lyskom-edit-insert-request-confirmation)
  (info  . lyskom-aux-item-info)
  (read-action . lyskom-request-confirmation-action))

(def-aux-item read-confirm 7
  (text-name aux-read-confirm-name)
  (text-print-when . header)
  (text-print . lyskom-print-read-confirm)
  (info  . lyskom-aux-item-info))

(def-aux-item redirect 8
  (text-name aux-redirect-name)
  (print . lyskom-print-redirect)
  (info  . lyskom-aux-item-info))

(def-aux-item x-face 9
  (text-name aux-x-face-name)
  (print . lyskom-print-xface)
  (info  . lyskom-aux-item-info))

(def-aux-item alternate-name 10
  (text-name aux-alternate-name-name)
  (text-print-when . header)
  (text-print . lyskom-print-alternate-name)
  (encode-data . lyskom-aux-item-encode-data)
  (parse-data . lyskom-aux-item-decode-data)
  (info  . lyskom-aux-item-info))

(def-aux-item pgp-signature 11
  (text-name aux-pgp-signature-name)
  (info  . lyskom-aux-item-info))

(def-aux-item pgp-public-key 12
  (text-name aux-pgp-public-key-name)
  (info  . lyskom-aux-item-info))

(def-aux-item e-mail-address 13
  (text-name aux-e-mail-address-name)
  (info  . lyskom-aux-item-info))

(def-aux-item faq-text 14
  (text-name aux-faq-text-name)
  (info . lyskom-aux-item-info)
  (text-header-line . (faq-in-text faq-in-text-by))
  (status-print . lyskom-status-print-faq-text))

(def-aux-item creating-software 15
  (text-name aux-creating-software-name)
  (info . lyskom-aux-item-info)
  (text-print-when . header)
  (parse-data . lyskom-aux-item-decode-data)
  (encode-data . lyskom-aux-item-encode-data)
  (text-print . lyskom-print-creating-software))

(def-aux-item mx-author 16
  (text-name aux-mx-author-name)
  (info . lyskom-aux-item-info))

(def-aux-item mx-from 17
  (text-name aux-mx-from-name)
  (info . lyskom-aux-item-info))

(def-aux-item mx-reply-to 18
  (text-name aux-mx-reply-to-name)
  (info . lyskom-aux-item-info))

(def-aux-item mx-to 19
  (text-name aux-mx-to-name)
  (info . lyskom-aux-item-info))

(def-aux-item mx-cc 20
  (text-name aux-mx-cc-name)
  (info . lyskom-aux-item-info))

(def-aux-item mx-date 21
  (text-name aux-mx-date-name)
  (info . lyskom-aux-item-info))

(def-aux-item mx-message-id 22
  (text-name aux-mx-message-id-name)
  (info . lyskom-aux-item-info))

(def-aux-item mx-in-reply-to 23
  (text-name aux-mx-in-reply-to-name)
  (info . lyskom-aux-item-info))

(def-aux-item mx-misc 24
  (text-name aux-mx-misc-name)
  (info . lyskom-aux-item-info))

(def-aux-item mx-allow-filter 25
  (text-name aux-mx-allow-filter-name)
  (info . lyskom-aux-item-info))

(def-aux-item mx-reject-forward 26
  (text-name aux-mx-reject-forward-name)
  (info . lyskom-aux-item-info))

(def-aux-item notify-comments 27
  (text-name aux-notify-comments-name)
  (info . lyskom-aux-item-info))

(def-aux-item faq-for-conf 28
  (text-name aux-faq-for-conf-name)
  (text-print . lyskom-print-faq-for-conf)
  (text-print-when . header)
  (info . lyskom-aux-item-info)
  (read-action . lyskom-faq-for-conf-action))


(def-aux-item recommended-conf 29
  (text-name aux-recommended-conf-name)
  (status-print . lyskom-print-recommended-conf)
  (info . lyskom-aux-item-info))

(def-aux-item allowed-content-type 30
  (text-name aux-allowed-content-type-name)
  (info . lyskom-aux-item-info))

(def-aux-item canonical-name 31
  (text-name aux-canonical-name-name)
  (info . lyskom-aux-item-info))

(def-aux-item mx-list-name 32
  (text-name aux-mx-list-name-name)
  (info . lyskom-aux-item-info)
  (status-print . lyskom-print-mx-list-name))

(def-aux-item send-comments-to 33
  (text-name aux-send-comments-to-name)
  (info . lyskom-aux-item-info)
  (status-print . lyskom-print-send-comments-to))

(def-aux-item world-readable 34
  (text-name aux-world-readable-name)
  (info . lyskom-aux-item-info)
  (text-print . lyskom-print-world-readable)
  (parse . lyskom-parse-world-readable)
  (edit-insert . lyskom-edit-insert-world-readable)
  (text-print-when . header))

(def-aux-item mx-refuse-import 35
  (text-name aux-mx-refuse-import)
  (info . lyskom-aux-item-info)
  (status-print . lyskom-print-mx-refuse-import))

(def-aux-item elisp-client-read-faq 10000
  (text-name aux-elisp-client-read-faq-name)
  (info . lyskom-aux-item-info)
  (status-print . lyskom-print-elisp-client-read-faq))

(def-aux-item elisp-client-rejected-invitation 10001
  (text-name aux-elisp-client-rejected-invitation-name)
  (info . lyskom-aux-item-info)
  (status-print . lyskom-print-elisp-client-rejected-invitation))

(def-aux-item elisp-client-enriched-headers 10002)

(def-aux-item mx-allow-envelope-sender-regexp 10105
  (text-name aux-mx-allow-envelope-sender-regexp)
  (info . lyskom-aux-item-info)
  (status-print . lyskom-print-mx-allow-envelope-sender-regexp))



;;; ================================================================



(defun lyskom-aux-item-decode-data (item)
  (set-aux-item->data 
   item
   (lyskom-decode-coding-string (aux-item->data item) lyskom-server-coding-system)))

(defun lyskom-aux-item-encode-data (item)
  (lyskom-encode-coding-string (aux-item->data item) lyskom-server-coding-system)
  )


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
      (lyskom-format-time 'date-and-time (aux-item->sent-at item))
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

(defun lyskom-parse-cross-reference ()
  (or (and (looking-at (lyskom-get-string 'cross-reference-text-regexp))
           (concat "T" (match-string 1)))
      (and (looking-at (lyskom-get-string 'cross-reference-conf-regexp))
           (concat "C" (match-string 1)))
      (and (looking-at (lyskom-get-string 'cross-reference-pers-regexp))
           (concat "P" (match-string 1)))))

(defun lyskom-edit-insert-cross-reference (item pers)
  (concat
   (lyskom-print-cross-reference item nil pers)
   (lyskom-edit-generate-aux-item-flags (aux-item->flags item))))

(defun lyskom-print-cross-reference (item &optional obj pers)
  (let ((pers (if (and (zerop (aux-item->creator item))
                       pers)
                  pers
                (aux-item->creator item))))

    ;; We use string-to-int here since we handle floats in 
    ;; lyskom-format.

    (concat
     (cond ((string-match "^P\\([0-9]+\\)" (aux-item->data item))
            (lyskom-format 'cross-reference-pers-aux 
                           (lyskom-string-to-number 
                            (match-string 1 (aux-item->data item)))
                           pers
                           ))
           ((string-match "^C\\([0-9]+\\)" (aux-item->data item))
            (lyskom-format 'cross-reference-conf-aux 
                           (lyskom-string-to-number 
                            (match-string 1 (aux-item->data item)))
                           pers
                           ))
           ((string-match "^T\\([0-9]+\\)" (aux-item->data item))
            (lyskom-format 'cross-reference-text-aux 
                           (lyskom-string-to-number 
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
  ;; We use string-to-int here since we handle floats in 
  ;; lyskom-format.
  (lyskom-insert 
   (concat
    (cond ((string-match "^P\\([0-9]+\\)" (aux-item->data item))
           (lyskom-format 'cross-reference-pers-status-aux 
                          (lyskom-string-to-number 
                           (match-string 1 (aux-item->data item)))
                          (aux-item->creator pers)
                          ))
          ((string-match "^C\\([0-9]+\\)" (aux-item->data item))
           (lyskom-format 'cross-reference-conf-status-aux 
                          (lyskom-string-to-number 
                           (match-string 1 (aux-item->data item)))
                          pers
                          ))
          ((string-match "^T\\([0-9]+\\)" (aux-item->data item))
           (lyskom-format 'cross-reference-text-status-aux 
                          (lyskom-string-to-number 
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
  (concat
   (lyskom-format 'no-comments-edit-aux)
   (lyskom-edit-generate-aux-item-flags (aux-item->flags item))))

(defun lyskom-parse-personal-comments ()
  (and (looking-at (lyskom-get-string 'personal-comment-regexp))
       ""))

(defun lyskom-print-personal-comments (item &optional obj)
  (concat (lyskom-format 'personal-comment-aux)
          (lyskom-aux-item-terminating-button item obj)))

(defun lyskom-edit-insert-personal-comments (item &optional obj)
  (concat
   (lyskom-format 'personal-comment-edit-aux)
   (lyskom-edit-generate-aux-item-flags (aux-item->flags item))))

(defun lyskom-parse-request-confirmation ()
  (and (looking-at (lyskom-get-string 'request-confirmation-regexp))
       ""))

(defun lyskom-print-request-confirmation (item &optional obj)
  (concat (lyskom-format 'request-confirmation-aux
                         (aux-item->creator item))
          (lyskom-aux-item-terminating-button item obj)))

(defun lyskom-edit-insert-request-confirmation (item &optional obj)
  (concat
   (lyskom-format 'request-confirmation-edit-aux)
   (lyskom-edit-generate-aux-item-flags (aux-item->flags item))))

(defun lyskom-faq-for-conf-action (text-stat)
  (let ((faqs (text-stat-find-aux text-stat 28)))
    (lyskom-traverse aux faqs
      (condition-case nil
          (lyskom-register-read-faq (lyskom-string-to-int
                                     (aux-item->data aux) t)
                                    (text-stat->text-no text-stat))
       (lyskom-integer-conversion-error nil)))))

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
                  (lyskom-format-time 'date-and-time (aux-item->sent-at item)))
   (lyskom-aux-item-terminating-button item obj)))



(defun lyskom-print-redirect (item &optional obj)
  ;; We use string-to-int here since we handle floats in 
  ;; lyskom-format.
  (concat
   (cond ((string-match "^E-mail:\\(.*\\)$" (aux-item->data item))
          (lyskom-format 'redirect-email-aux 
                         (match-string 1 (aux-item->data item))))
         ((string-match "^LysKOM:\\(.*\\)$" (aux-item->data item))
          (lyskom-format 'redirect-lyskom-aux 
                         (lyskom-string-to-number 
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
     (unless (lyskom-find-face 'kom-xface)
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

(defun lyskom-print-faq-format-subject (text text-stat text-no)
  (if (and text text-stat)
      (concat "\""
	      (cond ((string-match "\n" (text->decoded-text-mass text text-stat))
		     (substring (text->decoded-text-mass text text-stat) 0 (match-beginning 0)))
		    (t ""))
	      "\"")
    (lyskom-format 'no-such-text-m text-no)))

(defun lyskom-deferred-print-faq (text-stat defer-info)
  (if text-stat
      (initiate-get-text 
       'deferred 
       (lambda (text text-stat defer-info)
         (lyskom-replace-deferred defer-info 
                                  (lyskom-format "%#1r" 
                                                 (lyskom-print-faq-format-subject text
                                                                                  text-stat
                                                                                  (defer-info->data defer-info)))))
       (text-stat->text-no text-stat)
       text-stat
       defer-info)
    (lyskom-replace-deferred defer-info
                             (lyskom-print-faq-format-subject nil nil (defer-info->data defer-info)))))

(defun lyskom-status-print-faq-text (item &optional obj)
  (if (lyskom-aux-item-validate (aux-item->data item) 'lyskom-string-to-int)
      (let* ((text-no (lyskom-string-to-number (aux-item->data item)))
             (subject (if kom-deferred-printing
                          (lyskom-create-defer-info 'get-text-stat
                                                    text-no
                                                    'lyskom-deferred-print-faq
                                                    nil nil nil
                                                    text-no)
                        (blocking-do-multiple ((text (get-text text-no))
                                               (text-stat (get-text-stat text-no)))
                          (lyskom-print-faq-format-subject text text-stat text-no)))))
        (lyskom-format-insert 'faq-in-text-aux 
                              text-no
                              subject)
        (lyskom-insert (lyskom-aux-item-terminating-button item obj)))
    (lyskom-format-insert 'bad-faq-in-text-aux
                          (aux-item->data item)
                          `(face ,kom-warning-face)
                          (lyskom-aux-item-terminating-button item obj)))
  (lyskom-insert "\n"))

(defun lyskom-print-faq-for-conf (item &optional obj)
  (if (lyskom-aux-item-validate (aux-item->data item) 'lyskom-string-to-int)
      (let ((conf-no (lyskom-string-to-number (aux-item->data item))))
        (concat 
         (cond ((zerop conf-no) (lyskom-get-string 'faq-for-server-aux))
               (t (lyskom-format 'faq-for-conf-aux conf-no)))
         (lyskom-aux-item-terminating-button item obj)))
    (lyskom-format 'bad-faq-for-conf-aux 
                   (aux-item->data item)
                   `(face ,kom-warning-face)
                   (lyskom-aux-item-terminating-button item obj))))

(defun lyskom-print-creating-software (item &optional obj)
  (when (or kom-show-creating-software
            (lyskom-viewing-noconversion))
    (concat
     (lyskom-format 'creating-software-aux (aux-item->data item))
     (lyskom-aux-item-terminating-button item obj))))

(defun lyskom-print-send-comments-to (item &optional obj)
  (if (lyskom-aux-item-validate (aux-item->data item) 'lyskom-string-to-int)
      (let ((conf-no (lyskom-string-to-number (aux-item->data item))))
        (lyskom-format-insert 'status-send-comments-to
                              conf-no 
                              (lyskom-aux-item-terminating-button item obj)))
    (lyskom-format-insert 'bad-status-send-comments-to
                          (aux-item->data item)
                          `(face ,kom-warning-face)
                          (lyskom-aux-item-terminating-button item obj))))



(defun lyskom-parse-world-readable ()
  (and (looking-at (regexp-quote 
                    (lyskom-get-string 'world-readable-text-edit-aux)))
       ""))

(defun lyskom-edit-insert-world-readable (item &optional obj)
  (concat
   (lyskom-format 'world-readable-text-edit-aux)
   (lyskom-edit-generate-aux-item-flags (aux-item->flags item))))

(defun lyskom-print-world-readable (item &optional obj)
  (concat (lyskom-format 'world-readable-text-aux)
          (lyskom-aux-item-terminating-button item obj)))

(defun lyskom-print-mx-list-name (item &optional obj)
  (lyskom-format-insert 'conf-mx-list-name 
                        (aux-item->data item)
                        (lyskom-aux-item-terminating-button item obj)))

(defun lyskom-print-mx-refuse-import (item &optional obj)
  (let* ((sym (intern (format "mx-refuse-import-%s" (aux-item->data item))))
         (s (or (lyskom-try-get-string sym 'lyskom-message)
                (format "'%s'" (aux-item->data item)))))
    (lyskom-format-insert 'conf-mx-refuse-import s
                          (lyskom-aux-item-terminating-button item obj))))

(defun lyskom-print-recommended-conf (item &optional obj)
  (if (lyskom-aux-item-validate (aux-item->data item) 'lyskom-string-to-int)
      (let ((conf-no (lyskom-string-to-number (aux-item->data item))))
        (lyskom-format-insert 'recommended-conf-aux
                              conf-no
                              (lyskom-aux-item-terminating-button item obj)))
    (lyskom-format-insert 'bad-recommended-conf-aux
                          (aux-item->data item)
                          `(face ,kom-warning-face)
                          (lyskom-aux-item-terminating-button item obj))))

(defun lyskom-print-elisp-client-read-faq (item &optional obj)
  (when (lyskom-extended-status-information 'read-faq)
    (when (string-match "^\\([0-9]+\\) \\([0-9]+\\)" (aux-item->data item))
      (condition-case nil
          (let ((conf-no (lyskom-string-to-int (match-string 1 (aux-item->data item)) t))
                (text-no (lyskom-string-to-int (match-string 2 (aux-item->data item)) t)))
            (lyskom-format-insert 'status-read-faq-aux-item 
                                  conf-no 
                                  text-no
                                  (lyskom-aux-item-terminating-button item obj)))
        (lyskom-integer-conversion-error
         (lyskom-format-insert 'bad-status-read-faq-aux-item
                               (aux-item->data item)
                               `(face ,kom-warning-face)
                               (lyskom-aux-item-terminating-button item obj)))))))

(defun lyskom-print-elisp-client-rejected-invitation (item &optional obj)
  (if (lyskom-aux-item-validate (aux-item->data item) 'lyskom-string-to-int)
      (let ((conf-no (lyskom-string-to-number (aux-item->data item))))
        (lyskom-format-insert 'status-rejected-recommendation-aux-item
                              conf-no 
                              (lyskom-aux-item-terminating-button item obj)))
    (lyskom-format-insert 'bad-status-rejected-recommendation-aux-item
                          (aux-item->data item)
                          `(face ,kom-warning-face)
                          (lyskom-aux-item-terminating-button item obj))))

(defun lyskom-print-mx-allow-envelope-sender-regexp (item &optional obj)
  (lyskom-format-insert 'conf-mx-allow-envelope-sender-regexp
                        (aux-item->data item)
                        (lyskom-aux-item-terminating-button item obj)))


(provide 'lyskom-aux-items)
