;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: faqs.el,v 44.14 2003-08-14 15:59:24 byers Exp $
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
;;;; File: faqs.el
;;;;
;;;; This file contains the code for FAQ handling
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
              "$Id: faqs.el,v 44.14 2003-08-14 15:59:24 byers Exp $\n"))

(defun lyskom-register-read-faq (conf-no text-no)
  (unless conf-no (setq conf-no 0))
  (unless (lyskom-faq-is-read conf-no text-no)
    (setq lyskom-read-faqs (cons (cons conf-no text-no) lyskom-read-faqs))
    (initiate-modify-conf-info 
     'background
     nil
     lyskom-pers-no
     nil
     (list (lyskom-create-aux-item
            0
            10000
            nil
            nil
            (lyskom-create-aux-item-flags nil nil t nil 
                                          nil nil nil nil)
            0
            (format "%d %d" conf-no text-no))))))

(defun lyskom-faq-is-read (conf-no text-no)
  "Return non-nil if lyskom-pers-no has read CONF-NOs FAQ TEXT-NO."
  (member (cons (or conf-no 0) text-no) lyskom-read-faqs))

(defun lyskom-update-read-faqs ()
  "Update the list of read FAQs from the server."
  (setq lyskom-read-faqs nil)
  (let* ((conf-stat (blocking-do 'get-conf-stat lyskom-pers-no))
         (aux-list (and conf-stat (lyskom-get-aux-item
                                   (conf-stat->aux-items conf-stat) 
                                   10000))))
    (lyskom-traverse item aux-list
      (when (string-match "^\\([0-9]+\\) \\([0-9]+\\)" (aux-item->data item))
        (setq lyskom-read-faqs
              (cons (cons (string-to-int (match-string 1 (aux-item->data item)))
                          (string-to-int (match-string 2 (aux-item->data item))))
                    lyskom-read-faqs))))))

;;; ======================================================================
;;; FAQ Management
;;;

(def-kom-command kom-add-faq (&optional conf-no text-no)
  "Make an existing text a FAQ of a conference. To write a new FAQ for
a conference that doesn't have one, or change an existing FAQ, use
`kom-change-conf-faq'.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-conf-no 'conf-to-add-faq
                                          '(conf pers) nil nil t)
                     (lyskom-read-text-no-prefix-arg 'text-to-add-as-faq nil 'last-seen-written)))
  (lyskom-add-faq conf-no text-no))



(def-kom-command kom-add-server-faq (&optional text-no)
  "Make an existing text a FAQ of the server. To write a new FAQ for
a conference that doesn't have one, or change an existing FAQ, use
`kom-change-server-faq'.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'text-to-add-as-faq nil 'last-seen-written)))
  (lyskom-add-faq nil text-no))


(defun lyskom-add-faq (conf-no text-no)
  "Add a FAQ to a conference or the server.
Add to the server if CONF-NO is nil, otherwise add to conference CONF-NO.
The text to add is passed in TEXT-NO"
  (let ((text (blocking-do 'get-text-stat text-no)))
    (if (null text)
        (lyskom-format-insert 'no-such-text-no text-no)
      (lyskom-format-insert 'adding-faq text-no conf-no)
      (cache-del-text-stat text-no)
      (when conf-no
        (cache-del-conf-stat conf-no))
      (let ((aux-item (lyskom-create-aux-item 
                       0 14 0 0 
                       (lyskom-create-aux-item-flags nil nil nil nil
                                                     nil nil nil nil)
                       0
                       (int-to-string text-no))))
        (lyskom-report-command-answer
         (if conf-no
             (blocking-do 'modify-conf-info
                          conf-no
                          nil
                          (list aux-item))
           (blocking-do 'modify-server-info
                        nil
                        (list aux-item))))))))

(def-kom-command kom-del-server-faq ()
  "Remove a FAQ from the server. You need administrative rights to
do this. To add a FAQ, use `kom-add-server-faq'."
  (interactive)
  (lyskom-del-faq nil))


(def-kom-command kom-del-faq ()
  "Remove a FAQ from a conference. To add a FAQ, use
`kom-add-faq'."
  (interactive)
  (let* ((conf-stat (lyskom-read-conf-stat 'conf-to-del-faq
                                           '(conf pers) nil nil t)))
    (lyskom-del-faq conf-stat)))

(defun lyskom-del-faq (conf-stat)
  (let ((faq-list 
         (mapcar (lambda (aux)
                   (cons (aux-item->data aux)
                         (aux-item->aux-no aux)))
                 (lyskom-get-aux-item
                  (if (null conf-stat)
                      (server-info->aux-item-list
                       (blocking-do 'get-server-info))
                    (conf-stat->aux-items conf-stat))
                  14)))
        (text-no nil))

    (cond
     ((null faq-list) 
      (lyskom-format-insert 'conf-has-no-faq conf-stat))
     (t (setq text-no
              (if (eq 1 (length faq-list))
                  (car (car faq-list))
                (lyskom-completing-read (lyskom-get-string 'text-to-del-as-faq)
                                        (lyskom-maybe-frob-completion-table 
                                         faq-list)
                                        nil t)))
        (when text-no
          (lyskom-format-insert 'deleting-faq 
                                (string-to-int text-no)
                                conf-stat)
          (cache-del-text-stat (string-to-int text-no))
          (when conf-stat
            (cache-del-conf-stat (conf-stat->conf-no conf-stat)))
          (lyskom-report-command-answer
           (if conf-stat
               (blocking-do 'modify-conf-info
                            (conf-stat->conf-no conf-stat)
                            (list (cdr (lyskom-string-assoc text-no faq-list)))
                            nil)
             (blocking-do 'modify-server-info
                          (list (cdr (lyskom-string-assoc text-no faq-list)))
                          nil))))))))

(def-kom-command kom-review-server-faq ()
  "View the FAQs for the server."
  (interactive)
  (lyskom-review-faq nil (server-info->aux-item-list
                          (blocking-do 'get-server-info))))

(def-kom-command kom-unread-server-faq ()
  "Mark the FAQs for the server as unread."
  (interactive)
  (lyskom-unread-faq nil (server-info->aux-item-list
                          (blocking-do 'get-server-info))))

(def-kom-command kom-review-faq (&optional conf-no)
  "View the FAQs for a particular conference."
  (interactive 
   (list (lyskom-read-conf-no 'view-which-faq '(conf pers) t nil t)))
  (if (zerop conf-no)
      (lyskom-review-faq nil (server-info->aux-item-list
                              (blocking-do 'get-server-info)))
    (let ((conf-stat (blocking-do 'get-conf-stat conf-no)))
      (if conf-stat
          (lyskom-review-faq conf-stat (conf-stat->aux-items conf-stat))
        (lyskom-format-insert 'conf-no-does-not-exist-r conf-no)))))

(def-kom-command kom-unread-faq (&optional conf-no)
  "Marks the FAQs for a particular conference as unread."
  (interactive 
   (list (lyskom-read-conf-no 'unread-which-faq '(conf pers) t nil t)))
  (if (zerop conf-no)
      (lyskom-unread-faq nil (server-info->aux-item-list
                              (blocking-do 'get-server-info)))
    (let ((conf-stat (blocking-do 'get-conf-stat conf-no)))
      (if conf-stat
          (lyskom-unread-faq conf-stat (conf-stat->aux-items conf-stat))
        (lyskom-format-insert 'conf-no-does-not-exist-r conf-no)))))


(defun lyskom-unread-faq (conf-stat aux-list)
  (let ((faq-list (mapcar (lambda (aux)
                            (string-to-int (aux-item->data aux)))
                          (lyskom-get-aux-item aux-list 14))))
    (if (null faq-list) 
        (lyskom-format-insert 'conf-has-no-faq conf-stat)
      (lyskom-traverse text-no faq-list
        (lyskom-format-insert 'marking-text-unread text-no)
        (lyskom-report-command-answer 
         (lyskom-mark-unread text-no))))))

(defun lyskom-review-faq (conf-stat aux-list)
  (let ((faq-list (mapcar (lambda (aux)
                            (string-to-int (aux-item->data aux)))
                          (lyskom-get-aux-item aux-list 14))))
    (cond 
     ((null faq-list) 
      (lyskom-format-insert 'conf-has-no-faq conf-stat))
     ((eq 1 (length faq-list))
      (lyskom-format-insert 'review-faq-for-r conf-stat)
      (lyskom-view-text (car faq-list) 
                        nil             ;mark-as-read
                        nil             ;follow-comments
                        nil             ;conf-stat
                        nil             ;priority
                        nil             ;build-review-tree
                        t               ;flat-review
                        ))
     (t 
      (lyskom-format-insert 'review-faq-for-r conf-stat)
      (lyskom-do-review-faq faq-list)
      ))))

(defun lyskom-do-review-faq (faq-list &optional review-tree)
  (read-list-enter-read-info
   (lyskom-create-read-info (if review-tree 'REVIEW-FAQ-TREE 'REVIEW-FAQ)
                            nil
                            (lyskom-review-get-priority)
                            (lyskom-create-text-list faq-list)
                            nil t)
   lyskom-reading-list t))

(def-kom-command kom-change-server-faq ()
  "Change a FAQ for the server. If the server doesn't have a FAQ, 
create a new one. You need administrative rights to do this."
  (interactive)
  (lyskom-change-faq nil (lyskom-get-aux-item (server-info->aux-item-list
                                               (blocking-do 'get-server-info))
                                              14)))

(def-kom-command kom-change-conf-faq ()
  "Change a FAQ fo a conference. If the conference doesn't have a FAQ,
create a new FAQ."
  (interactive)
  (let* ((conf-no (lyskom-read-conf-no
                   (lyskom-get-string 'what-to-change-faq-you)
                   '(conf pers) nil nil t))
         (conf-stat (when conf-no       ; Need this to make sure the conf-stat is up-to-date!
                      (cache-del-conf-stat conf-no)
                      (blocking-do 'get-conf-stat conf-no))))
    (if conf-stat
        (lyskom-change-faq conf-stat (lyskom-get-aux-item 
                                      (conf-stat->aux-items conf-stat)
                                      14))
      (lyskom-insert (lyskom-get-string 'conf-does-not-exist)))))


(defun  lyskom-change-faq (conf-stat aux-list)
  "Change a FAQ for a conference."
  ;; Get a list of FAQ texts and corresponding aux-item-numbers
  ;; Get the FAQ to change
  (let* ((faq-list (mapcar (lambda (x)
                             (cons (aux-item->data x)
                                   (aux-item->aux-no x)))
                           aux-list))
         (text-no-aux (cond ((= (length faq-list) 1) (car faq-list))
                            ((null faq-list) nil)
                            (t (lyskom-string-assoc
                                (lyskom-completing-read 
                                 (lyskom-get-string 'text-to-change-as-faq)
                                 (lyskom-maybe-frob-completion-table 
                                  faq-list)
                                 nil t)
                                faq-list))))
         (text-no (and text-no-aux (string-to-int (car text-no-aux))))
         (aux-no (and text-no-aux (cdr text-no-aux))))

    (cond 

     ;; If conf-stat is null we are changing the FAQ for the server.
     ;; Don't do this unless we are running enabled and have the right
     ;; privileges.
     ((and (null conf-stat)
           (or (not lyskom-is-administrator)
               (not (privs->admin (pers-stat->privileges
                                   (blocking-do 'get-pers-stat
                                                lyskom-pers-no))))))
      (lyskom-format-insert 'not-supervisor-for-server))

     ;; If we have a conf-stat and are not administrator and not
     ;; supervisor for the conf, then we are not allowed to change
     ;; the FAQ.
     ((and conf-stat 
           (not lyskom-is-administrator)
           (not (lyskom-is-supervisor (conf-stat->conf-no conf-stat)
                                      lyskom-pers-no)))
      (lyskom-format-insert 'not-supervisor-for conf-stat))

     ;; OK, it looks like we are allowed to change the FAQ.
     (t (let ((text-stat nil)
              (text-mass nil))

          ;; Get the old text (if there is one)
          (blocking-do-multiple ((ts (get-text-stat text-no))
                                 (tm (get-text text-no)))
            (setq text-stat ts text-mass tm))

          (let* ((str (and text-mass (text->decoded-text-mass text-mass
                                                              text-stat)))
                 (subject (if (and str (string-match "\n" str))
                              (substring str 0 (match-beginning 0))
                            ""
                            ))
                 (body (if (and str (string-match "\n" str))
                           (substring str (match-end 0))
                         (or str ""))))
            (lyskom-dispatch-edit-text
             lyskom-proc
             (apply 'lyskom-create-misc-list
                    (if (and text-stat text-mass)
                        (append (lyskom-get-recipients-from-misc-list
                                 (text-stat->misc-info-list text-stat))
                                (list 'COMM-TO (text-stat->text-no text-stat)))
                      (list 'RECPT (conf-stat->conf-no conf-stat))))
             subject
             body
             'lyskom-change-faq-2
             conf-stat
             (and text-stat (text-stat->text-no text-stat))
             aux-no)))))))


(defun lyskom-change-faq-2 (text-no conf-stat old-text-no old-aux-no)
  (let ((aux-item (lyskom-create-aux-item 
                   0 14 0 0
                   (lyskom-create-aux-item-flags nil nil nil nil
                                                 nil nil nil nil)
                   0
                   (int-to-string text-no))))
    (if conf-stat
        (progn
          (cache-del-conf-stat (conf-stat->conf-no conf-stat))
          (initiate-modify-conf-info 'background
                                     'lyskom-change-faq-3
                                     (conf-stat->conf-no conf-stat)
                                     (when old-aux-no (list old-aux-no))
                                     (list aux-item)
                                     conf-stat
                                     old-text-no
                                     text-no))
      (initiate-modify-server-info 'background
                                   'lyskom-change-faq-3
                                   (when old-aux-no (list old-aux-no))
                                   (list aux-item)
                                   conf-stat
                                   old-text-no
                                   text-no))))

(defun lyskom-change-faq-3 (retval conf-stat old-text-no text-no)
  (cache-del-text-stat text-no)
  (if retval
      (lyskom-format-insert-before-prompt 
       (if old-text-no 'changed-faq-for-conf-done 'set-faq-for-conf-done)
       conf-stat
       old-text-no
       text-no)
    (lyskom-format-insert-before-prompt 
     (if old-text-no 'changed-faq-for-conf-failed 'set-faq-for-conf-failed)
     conf-stat 
     old-text-no text-no
     (lyskom-current-error))))



;;; ================================================================
;;; Special have-you-read-the-FAQ-checking stuff

(defun lyskom-startup-check-faqs ()
  (let ((faq-list (lyskom-get-unread-faqs nil)))
    (when faq-list
      (lyskom-format-insert 'server-has-new-faq (length faq-list))
      (lyskom-present-unread-faqs nil faq-list))))

(defun lyskom-get-unread-faqs (conf-stat)
  "Return a list of unread FAQs for conf CONF-STAT."
  (let* ((conf-no (if conf-stat (conf-stat->conf-no conf-stat) 0))
         (aux-list (if conf-stat
                       (conf-stat->aux-items conf-stat)
                     (server-info->aux-item-list lyskom-server-info)))
         (faq-list
          (filter-list (lambda (faq)
                         (not (lyskom-faq-is-read conf-no faq)))
                       (mapcar (lambda (aux)
                                 (string-to-int (aux-item->data aux)))
                               (lyskom-get-aux-item aux-list 14)))))

    ;; Filter out FAQs that don't exist
    (let ((collector (make-collector)))
      (lyskom-traverse faq faq-list
        (initiate-get-text-stat 'background 
                                (lambda (text-stat faq conf-no collector)
                                  (if text-stat
                                      (collector-push faq collector)
                                    (lyskom-register-read-faq conf-no faq)))
                                faq
                                faq
                                conf-no
                                collector))
      (lyskom-wait-queue 'background)
      (setq faq-list (nreverse (collector->value collector))))

    faq-list))

(defun lyskom-present-unread-faqs (conf-stat faq-list)
  "Present a list of unread FAQs in an appropriate manner."
  (when faq-list
    (when kom-auto-list-faqs
      (lyskom-format-insert 'unread-faqs-header (length faq-list) 
                            (and conf-stat (conf-stat->conf-no conf-stat)))
      (lyskom-do-list-summary faq-list))

    (when kom-auto-review-faqs
      (lyskom-do-review-faq faq-list t))))


(def-kom-command kom-list-faqs (conf-stat)
  "List all FAQs for a conference."
  (interactive (list (lyskom-read-conf-stat "Conference: " 
                                            '(conf) nil nil t)))
  (lyskom-do-list-faqs conf-stat
                       (lyskom-get-aux-item 
                        (conf-stat->aux-items conf-stat)
                        14)))

(def-kom-command kom-list-server-faqs ()
  "List all FAQs for the server."
  (interactive)
  (lyskom-do-list-faqs
   nil
   (lyskom-get-aux-item (server-info->aux-item-list
                         (blocking-do 'get-server-info)) 14)))


(defun lyskom-do-list-faqs (conf-stat faq-list)
  (cond (faq-list
         (lyskom-format-insert 'all-faqs-header 
                               (and conf-stat
                                    (conf-stat->conf-no conf-stat)))
         (lyskom-do-list-summary (mapcar (lambda (faq)
                                           (string-to-int (aux-item->data faq)))
                                         faq-list)))
        (t (lyskom-format-insert 'conf-has-no-faq conf-stat))))
