;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: view-text.el,v 44.64 2002-05-01 21:42:40 byers Exp $
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
;;; ================================================================
;;; ================================================================
;;;
;;; File: view-text.el
;;; 
;;; This file contains functions that have to do with putting
;;; a text into the lyskom buffer.
;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: view-text.el,v 44.64 2002-05-01 21:42:40 byers Exp $\n"))


(defvar lyskom-view-text-text)
(defvar lyskom-view-text-text-stat)


(defun lyskom-view-text (text-no &optional mark-as-read
				 follow-comments conf-stat priority
				 build-review-tree filter-active
				 flat-review)
  "Display text number TEXT-NO.
Args: TEXT-NO &optional MARK-AS-READ FOLLOW-COMMENTS CONF-STAT 
PRIORITY BUILD-REVIEW-TREE FLAT-REVIEW.
If MARK-AS-READ (or `kom-review-marks-texts-as-read') is non-nil the text will
be marked as read.
If FOLLOW-COMMENTS is non-nil all comments and footnotes to this text will be
read before the next text. CONF-STAT must be the conference status of the
current conference, and PRIORITY the priority, if FOLLOW-COMMENTS is non-nil.
If BUILD-REVIEW-TREE is non-nil then it fixes a new entry in the 
lyskom-reading-list to read the comments to this.
If FLAT-REVIEW is non-nil this is a review caused by lyskom-view-commented or
kom-view-text viewing REVIEW or REVIEW-MARK text.

Note that this function must not be called asynchronously."

  (let ((inhibit-read-only t)
	(filter (and filter-active
                     (lyskom-filter-text-p text-no)))
        (start nil)
        (todo nil)
        (lyskom-last-text-format-flags nil))
    (cond ((eq filter 'skip-text) (lyskom-filter-prompt text-no 'filter-text)
	   (setq todo 'next-text)
	   (lyskom-mark-as-read (blocking-do 'get-text-stat text-no))
	   'next-text)
	  ((eq filter 'skip-tree)
	   (lyskom-filter-prompt text-no 'filter-tree)

	   ;;(initiate-get-text-stat 'main 'lyskom-jump text-no t)
	   ;; Let's try something else:
	   (lyskom-jump (blocking-do 'get-text-stat text-no) t)
	   (lyskom-wait-queue 'main)

	   (setq todo 'next-text)
	   'next-text)
	  (t
	   (if (not (or (null filter) 
                        (eq filter 'read)
			(eq filter 'dontshow)))
	       (lyskom-message "%s" (lyskom-get-string 'invalid-filter-list)))
	   (if (eq filter 'dontshow)
	       (setq todo 'next-text))
	   (blocking-do-multiple ((text-stat (get-text-stat text-no))
				  (text (get-text text-no)))
             (setq lyskom-view-text-text text
                   lyskom-view-text-text-stat text-stat)
	     (if (and text-stat text)
		 (progn
                   (run-hooks 'lyskom-view-text-hook)
		   ;; Use a marker, because the buffer may lose data
		   ;; at the top if kom-max-buffer-size is set.
		   (setq start (point-max-marker))





                   (let ((mx-date (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 21)))
                         (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 17)))
                         (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 16)))
                         (mx-to (lyskom-get-aux-item (text-stat->aux-items text-stat) 19))
                         (mx-cc (lyskom-get-aux-item (text-stat->aux-items text-stat) 20))
                         (mx-filename (lyskom-get-aux-item (text-stat->aux-items text-stat) 10104))
                         (mx-sender (lyskom-get-aux-item (text-stat->aux-items text-stat) 10103))
			 (mx-headers (lyskom-get-aux-item (text-stat->aux-items text-stat) 24))
                         (mx-reply-to (lyskom-get-aux-item (text-stat->aux-items text-stat) 18)))
                         
                   (lyskom-format-insert
                    'view-text-first-line
                    text-stat
                    (list 'lyskom-text-start (text-stat->text-no text-stat))
                    (if mx-date
                        (let ((date (lyskom-mx-date-to-time mx-date)))
                          (if date
                              (concat
                               (lyskom-format-time 'date-and-time date)
                               (if (time->tzhr date)
                                   (lyskom-format " %#1s%#2s" 
                                                  (time->tzhr date)
                                                  (time->tzmin date))
                                 ""))
                            (aux-item->data mx-date)))
                      (lyskom-format-time 'date-and-time
                                          (text-stat->creation-time text-stat)))
                    (text-stat->no-of-lines text-stat)
                    (or (lyskom-format-mx-author mx-from mx-author)
                        (lyskom-format "%#1P" 
                                       (text-stat->author text-stat)))
                    (when (eq filter 'dontshow)
                      (lyskom-get-string 'filtered))
                    (when kom-highlight-first-line
                      '(face kom-first-line-face)))

		   (set-marker start nil)

                   ;; Insert sender

                   (when (and mx-sender kom-show-imported-envelope-sender)
                     (lyskom-format-insert 'envelope-sender 
                                           (aux-item->data (car mx-sender))))
                   (when mx-filename
                     (lyskom-format-insert 'attachment-filename
                                           (aux-item->data (car mx-filename))))

                   ;; Insert imported at

                   (cond ((and (or mx-from mx-author mx-sender mx-headers)
                               (text-stat->author text-stat)
                               kom-show-imported-importer)
                          (lyskom-format-insert 'text-imported-at-by
                                                (lyskom-format-time
                                                 'date-and-time
                                                 (text-stat->creation-time text-stat))
                                                (text-stat->author text-stat)))
                         (mx-from
                          (lyskom-format-insert 'text-imported-at
                                                (lyskom-format-time
                                                 'date-and-time
                                                 (text-stat->creation-time text-stat))))
                         (mx-date (lyskom-format-insert 'text-created-at
                                                        (lyskom-format-time
                                                         'date-and-time
                                                         (text-stat->creation-time text-stat)))))
                   (when kom-show-imported-external-recipients
                     (mapcar (lambda (el)
                               (lyskom-format-insert "%#1s: %#2s\n"
                                                     (lyskom-get-string 'mx-Recipient)
                                                     (aux-item->data el)))
                             mx-to)
                     (mapcar (lambda (el)
                               (lyskom-format-insert "%#1s: %#2s\n"
                                                     (lyskom-get-string 'mx-Extra-recipient)
                                                     (aux-item->data el)))
                             mx-cc)
                     (mapcar (lambda (el)
                               (lyskom-format-insert "%#1s: %#2s\n"
                                                     (lyskom-get-string 'mx-Extern-reply-to)
                                                     (aux-item->data el)))
                             mx-reply-to)
                     )



                   ;; Insert all recipients and other header lines.

		   (if (eq filter 'dontshow)
		       (lyskom-mark-as-read
			(blocking-do 'get-text-stat text-no))
		     (lyskom-traverse misc
			 (text-stat->misc-info-list text-stat)
		       (let ((type (misc-info->type misc)))
			 (cond
			  ((memq type '(RECPT BCC-RECPT CC-RECPT))
			   (lyskom-print-header-recpt 
			    (misc-info->recipient-no misc)
			    misc))
			  ((eq type 'COMM-IN)
			   (if kom-reading-puts-comments-in-pointers-last
			       nil
                             (lyskom-print-header-comm 
                              (misc-info->comm-in misc)
                              misc
                              text-stat)))
			  ((eq type 'FOOTN-IN)
			   (if kom-reading-puts-comments-in-pointers-last
			       nil
			     (lyskom-print-header-comm
			      (misc-info->footn-in misc)
			      misc
                              text-stat)))
			  ((eq type 'COMM-TO)
			   (lyskom-print-header-comm (misc-info->comm-to misc)
						     misc
                                                     text-stat))
			  ((eq type 'FOOTN-TO)
			   (lyskom-print-header-comm (misc-info->footn-to misc)
						     misc
                                                     text-stat))
			  )))

                     ;; Insert aux-items that go in the header.

                     (let ((text nil))
                       (lyskom-traverse-aux aux
                                            (text-stat->aux-items text-stat)
                         (when (and
                             (or (eq (lyskom-aux-item-definition-field aux
                                                                 'text-print-when)
                                     'header)
                              (and (eq (lyskom-aux-item-definition-field aux
                                                                  'text-print-when)
                                       'comment)
                               (not kom-reading-puts-comments-in-pointers-last)
                               ))
                             (setq text 
                                   (lyskom-aux-item-call 
                                    aux
                                    'text-print
                                    aux
                                    text-stat)))
                           (lyskom-insert text)
                           (lyskom-insert "\n"))))


                     ;; If the text is marked, insert line saying so.

		     (let ((num-marks (text-stat->no-of-marks text-stat))
			   (is-marked-by-me (cache-text-is-marked
					     (text-stat->text-no text-stat))))
		       (when (> num-marks 0)
			 (if is-marked-by-me
			     (if (= num-marks 1)
				 (lyskom-format-insert 'marked-by-you
						       (mark->symbolic-mark-type
							is-marked-by-me))
			       (lyskom-format-insert 'marked-by-you-and-others
						     (1- num-marks)
						     (mark->symbolic-mark-type
						      is-marked-by-me)))
			   (lyskom-format-insert 'marked-by-several num-marks))))

                     ;; Insert the text body.

		     (lyskom-print-text text-stat text
					(or mark-as-read kom-review-marks-texts-as-read)
					text-no flat-review)
		     (if kom-review-marks-texts-as-read (lyskom-is-read text-no)))

                   ;; Insert aux-items that go in the footer.

                   (let ((text nil))
                       (lyskom-traverse-aux aux
                                            (text-stat->aux-items text-stat)
                         (when (and
                             (or (eq (lyskom-aux-item-definition-field aux
                                                                 'text-print-when)
                                     'footer)
                              (and (eq (lyskom-aux-item-definition-field aux
                                                                  'text-print-when)
                                       'comment)
                                   kom-reading-puts-comments-in-pointers-last
                               ))
                             (setq text 
                                   (lyskom-aux-item-call 
                                    aux
                                    'text-print
                                    aux
                                    text-stat)))
                           (lyskom-insert text)
                           (lyskom-insert "\n"))))

                   ;; Insert fast replies.

                   (let ((author-fast-replies nil)
                         (other-fast-replies nil))
                     (lyskom-traverse-aux aux (text-stat->aux-items text-stat)
                      (when (eq (aux-item->tag aux) 2)
                        (if (or (eq lyskom-current-command 'kom-review-noconversion)
                                (not (eq (text-stat->author text-stat)
                                         (aux-item->creator aux))))
                            (setq other-fast-replies
                                  (cons aux other-fast-replies))
                          (setq author-fast-replies
                                (cons aux author-fast-replies)))))
                     (when author-fast-replies
                       (lyskom-insert
                        (concat
                         (lyskom-format 'author-fast-replies)
                         "\n"
                         (mapconcat (lambda (item)
                                      (concat
                                       (lyskom-format 'author-fast-reply-aux
                                                      (aux-item->data item)
                                                      (aux-item->creator item))
                                       (lyskom-aux-item-terminating-button
                                        item
                                        text-stat)
                                       "\n"))
                                    (nreverse author-fast-replies)
                                    ""))))
                     (when other-fast-replies
                       (lyskom-insert
                        (concat
                         (lyskom-format 'other-fast-replies)
                         "\n"
                         (mapconcat (lambda (item)
                                      (concat
                                       (lyskom-format 'other-fast-reply-aux
                                                      (aux-item->data item)
                                                      (aux-item->creator item))
                                       (lyskom-aux-item-terminating-button
                                        item
                                        text-stat)
                                       "\n"))
                                    (nreverse other-fast-replies)
                                    "")))))

                   ;; Insert footnote/comment references.

		   (if kom-reading-puts-comments-in-pointers-last
		       (lyskom-view-text-handle-saved-comments text-stat))

                   ;; Prefetch commented texts.

		   (if (or follow-comments
			   ;; Checking build-review-tree should not be
			   ;; necessary, really /davidk
			   build-review-tree)
		       ;; This shows footnotes also.
		       (progn
			 ;; this is a bit silly, as it prefetches all
			 ;; the texts many times.
                         (unless (and build-review-tree
                                      kom-review-uses-cache)
                           (lyskom-prefetch-texttree text-no))
			 (lyskom-follow-comments text-stat
						 conf-stat
						 (or mark-as-read kom-review-marks-texts-as-read)
						 priority build-review-tree)))
                   (if (lyskom-text-p (cache-get-text text-no))
                       (cache-del-text text-no))
		   ))
	       (lyskom-format-insert 'no-such-text-no text-no))
             (let ((aux-items (text-stat->aux-items text-stat)))
               (while aux-items
                 (lyskom-aux-item-call (car aux-items)
                                       'read-action
                                       text-stat)
                 (setq aux-items (cdr aux-items))))
             )))
    todo))


(defun lyskom-text-is-mail-p (text-stat)
  "Ad-hoc test to determine whether a text is a mail by peeking at aux-items."
  (lyskom-match-aux-items
   (text-stat->aux-items text-stat)
   (lambda (aux-item)
     (let ((tag (aux-item->tag aux-item)))
       (or (and (> tag 15)	; 16:mx-author,	17:mx-from,	18:mx-reply-to,
		(< tag 22))	; 19:mx-to,	20:mx-cc,	21:mx-date,
	   (= 24 tag))))))	; 24:mx-headers

(defun lyskom-filter-signature-hook ()
  "Filter out the signature of imported mail messages. Most useful
when put in your `lyskom-view-text-hook'."
  (unless (eq 'kom-review-noconversion lyskom-current-command)
    (when (lyskom-text-is-mail-p lyskom-view-text-text-stat)
      (let* ((body (text->text-mass lyskom-view-text-text))
	     (sign (string-match "^-- $" body)))
	(when sign
	  (set-text->text-mass lyskom-view-text-text (substring body 0 sign))
	  (lyskom-signal-reformatted-text 'reformat-signature))))))

;(add-hook 'lyskom-view-text-hook 'lyskom-filter-signature-hook)

(defun lyskom-view-text-convert-ISO-646-SE-to-ISO-8859-1 ()
  "Display r{ksm|rg}s as räksmörgås unless the text is an imported mail
or we are reviewing without conversion. In other words, the characters
][\\}{| are converted, however ^~@` are not. Yet another useful function
to put in your `lyskom-view-text-hook'."
  ;; First the hard part - should we patch the text
  ;; in the text object?
  (unless (or (equal lyskom-current-command
		     'kom-review-noconversion)
	      (lyskom-text-is-mail-p lyskom-view-text-text-stat))
    ;; Yes, modify the text from the text-object (stored in mod)
    (let* ((mod (aref (cdr lyskom-view-text-text) 1))
	   (brk (string-match "\n" mod))
	   (sbj (if brk (substring mod 0 brk) mod))
	   (txt (if brk (substring mod (1+ brk)) ""))
	   (tbl '((?\] . ?\305)          ; Å
		  (?\[ . ?\304)          ; Ä
		  (?\\ . ?\326)          ; Ö
		  (?\} . ?\345)          ; å
		  (?\{ . ?\344)          ; ä
		  (?\| . ?\366))))       ; ö
      (when (and (not (string-match "[\200-\377]" txt))
		 (or (string-match "[A-Za-z][][\\}{|]" txt)
		     (string-match "[][\\}{|][A-Za-z]" txt)
		     (string-match "[A-Za-z] +[][\\}{|]" txt)))
	(lyskom-signal-reformatted-text 'reformat-deswascii)
	(while (string-match "\\([][\\}{|]\\)" txt)
	  (let ((old-char (aref (match-string 1 txt) 0)))
	    (aset txt (match-beginning 1) (cdr (assoc old-char tbl)))))
	(aset (cdr lyskom-view-text-text) 1
	      (if brk (concat sbj "\n" txt) sbj))))))

;(add-hook 'lyskom-view-text-hook 'lyskom-view-text-convert-ISO-646-SE-to-ISO-8859-1)




(defun lyskom-follow-comments (text-stat conf-stat
					 mark-as-read priority review-tree)
  "Arrange so that all comments and footnotes to this text will be read.
This will essentially fix the reading list and issue calls for the cache to be
filled.

Arguments are: TEXT-STAT CONF-STAT MARK-AS-READ PRIORITY REVIEW-TREE
TEXT-STAT is the current text.
CONF-STAT the current conference
If MARK-AS-READ is non-nil the texts will be marked as read.
PRIORITY the priority of the reading.
If REVIEW-TREE is non-nil then build an entry of type 'REVIEW-TREE in the 
lyskom-reading-list."
  ;; . Find footnotes and show them.
  ;; . Fix the reading-list
  ;; . Issue cache-filling initiate-calls for everything left comments.
  
  (let ((flist nil)
        (clist nil)
        (mx-attachments-in (lyskom-get-text-attachments text-stat)))
    (lyskom-traverse misc (text-stat->misc-info-list text-stat)
      (cond
       ((and (eq (misc-info->type misc) 'FOOTN-IN)
	     (> (misc-info->footn-in misc) (text-stat->text-no text-stat))
	     kom-show-footnotes-immediately)
	;; Show the footnote
	(lyskom-view-text (misc-info->footn-in misc)
			  mark-as-read t conf-stat priority review-tree)
	(lyskom-is-read (misc-info->footn-in misc)))
       ((eq (misc-info->type misc) 'FOOTN-IN)
	(setq flist (cons (misc-info->footn-in misc) flist)))
       ((eq (misc-info->type misc) 'COMM-IN)
        (setq clist (cons (misc-info->comm-in misc) clist)))))
    
    (let (comments footnotes)
      ;; Find the comments that we should read and enter them into the
      ;; read-list.
      (lyskom-traverse no clist
        (cond ((or review-tree
                   kom-follow-attachments
                   (not (memq no mx-attachments-in)))
               (let ((text-stat (blocking-do 'get-text-stat no)))
                 (if (and text-stat
                          (or review-tree
                              (not (lyskom-text-read-p text-stat))))
                     (setq comments (cons no comments)))))
              ((memq no mx-attachments-in)
               (lyskom-skip-attachments no mark-as-read))))
      (if comments
          (if review-tree
              (lyskom-review-enter-read-info
               (lyskom-create-read-info 'REVIEW-TREE
                                        conf-stat priority
                                        (lyskom-create-text-list comments)
                                        (text-stat->text-no text-stat)) t)
            (read-list-enter-read-info
             (lyskom-create-read-info 'COMM-IN
                                      conf-stat priority
                                      (lyskom-create-text-list comments)
                                      (text-stat->text-no text-stat))
             lyskom-reading-list t)))

      ;; Find the footnotes that we should read and enter them into
      ;; the read-list. A slight trick is to increase the priority so
      ;; that they will be read first.
      (lyskom-traverse no flist
	(let ((text-stat (blocking-do 'get-text-stat no)))
	  (if (or review-tree
		  (and text-stat
		       (not (lyskom-text-read-p text-stat))))
	      (setq footnotes (cons no footnotes)))))
      (if footnotes
	  (read-list-enter-read-info
	   (lyskom-create-read-info (if review-tree 'REVIEW-TREE 'FOOTN-IN)
				    conf-stat (1+ priority)
				    (lyskom-create-text-list footnotes)
				    (text-stat->text-no text-stat))
	   lyskom-reading-list t)))))

(defun lyskom-get-text-attachments (text-stat)
  "Return a list of all attachments to TEXT-STAT."
  (mapcar
   (lambda (el)
     (string-to-number (aux-item->data el)))
   (lyskom-get-aux-item (text-stat->aux-items text-stat) 10101)))

(defun lyskom-skip-attachments (text-no mark-as-read)
  "Skip the hierarchy of attachments starting at text-no"
  (let ((queue (list text-no))
        (done nil)
        (tmp (make-collector))
        (result nil))
    ;; Note: this does block the client, but explicitly checks that
    ;;       it is not being called from under the parser (where it
    ;;       really doesn't make a whole lot of sense anyway), so
    ;;       we ought to be OK anyway.
    (unless lyskom-is-parsing
      (while queue
        (while queue
          (when (not (memq (car queue) done))
            (initiate-get-text-stat 'background
                                    'collector-push
                                    (car queue) 
                                    tmp)
            (setq queue (cdr queue))
            (setq done (cons (car queue) done))))
        (lyskom-wait-queue 'background)

        (setq result (nconc result (collector->value tmp)))
        (lyskom-traverse text-stat (collector->value tmp)
          (setq queue (nconc queue (lyskom-get-text-attachments text-stat)))))

      (lyskom-traverse text-stat result
        (read-list-delete-text (text-stat->text-no text-stat) lyskom-reading-list)
        (read-list-delete-text (text-stat->text-no text-stat) lyskom-to-do-list)
        (when mark-as-read
          (lyskom-mark-as-read text-stat)
          (lyskom-is-read-handler text-stat))))))


(defun lyskom-fetch-text-for-cache (text-stat)
  "Fetches the author and other things of a text and does nothing with it."
  (cond
   (text-stat
    (lyskom-fetch-author-for-cache text-stat)
    (initiate-get-text 'fill-cache nil (text-stat->text-no text-stat))
    (lyskom-traverse misc (text-stat->misc-info-list text-stat)
      (cond
       ((eq (misc-info->type misc) 'FOOTN-IN)
	(initiate-get-text-stat 'fill-cache 'lyskom-fetch-author-for-cache
				(misc-info->footn-in misc)))
       ((eq (misc-info->type misc) 'FOOTN-TO)
	(initiate-get-text-stat 'fill-cache 'lyskom-fetch-author-for-cache
				(misc-info->footn-to misc)))
       ((eq (misc-info->type misc) 'COMM-IN)
	(initiate-get-text-stat 'fill-cache 'lyskom-fetch-author-for-cache
				(misc-info->comm-in misc)))
       ((eq (misc-info->type misc) 'COMM-TO)
	(initiate-get-text-stat 'fill-cache 'lyskom-fetch-author-for-cache
				(misc-info->comm-to misc))))))))



(defun lyskom-fetch-author-for-cache (text-stat)
  "Fetches the author of a text and does nothing with it."
  (if text-stat				;Error check.
      (initiate-get-conf-stat 'fill-cache nil (text-stat->author text-stat))))



(defun lyskom-text-read-p (text-stat)
  "Return t if TEXT-STAT has been marked as read in all
recipients to it that the user is a member in."
  (let* ((misc-info-list (text-stat->misc-info-list text-stat))
	 (i (length misc-info-list))
	 (res 'not-member))
    (while (and res (not (zerop i)))
      (setq i (1- i))
      (let* ((misc-info (elt misc-info-list i))
	     (type (misc-info->type misc-info)))
	(cond
	 ((memq type '(RECPT BCC-RECPT CC-RECPT))
	  ;; Is this function ever called asynchronously? If not, we
	  ;; can use lyskom-get-membership istead.
	  (let ((membership (lyskom-try-get-membership
			     (misc-info->recipient-no misc-info) t))
		(loc-no (misc-info->local-no misc-info)))

	    ;; Make a note that this text really is in a group we are
	    ;; a member of.
	    (if (and res membership) (setq res t))
	    
	    (if (and membership
		     (> loc-no (membership->last-text-read membership))
		     (not (lyskom-vmemq loc-no
					(membership->read-texts membership))))
		(setq res nil)))))))
    (if (eq res 'not-member)
	(not kom-follow-comments-outside-membership)
      res)))


(defun lyskom-text-read-at-least-once-p (text-stat &optional bg)
  "Return t if TEXT-STAT has been marked as read in any of the recipients
the user is a member of. Uses blocking-do. Returns t if TEXT-STAT is nil.

If BG is non-nil, this function is run in the background and must not use
blocking-do."
  (if text-stat
      (let* ((misc-info-list (text-stat->misc-info-list text-stat))
             (misc-item nil)
             (type nil)
             (membership nil)
             (is-member nil)
             (result nil))
        (while misc-info-list
          (setq misc-item (car misc-info-list))
          (setq type (misc-info->type misc-item))
          (setq misc-info-list (cdr misc-info-list))
          (cond ((memq type '(RECPT BCC-RECPT CC-RECPT))
                 (setq membership (if bg
                                      (lyskom-try-get-membership
                                       (misc-info->recipient-no misc-item) t)
                                    (lyskom-get-membership
                                     (misc-info->recipient-no misc-item) t)))
                 (when membership
                   (setq is-member t)
                   (when (or (<= (misc-info->local-no misc-item)
                                 (membership->last-text-read membership))
                             (lyskom-vmemq (misc-info->local-no misc-item)
                                           (membership->read-texts
                                            membership)))
                     (setq result t)
                     (setq misc-info-list nil))))))
        (cond (result result)
              ((not is-member) (not kom-follow-comments-outside-membership))
              (t nil)))
    t))


(defun lyskom-subtract-one-day (x)
  (let ((high-x (1- (car x)))
        (low-x (car (cdr x))))
    (if (> 20864 low-x)
        (setq high-x (1- high-x)
              low-x (+ low-x 65536)))
    (setq low-x (- low-x 20864))
    (list high-x low-x nil)))

(defun lyskom-calculate-day-diff (time)
  (let* ((now (current-time))
         (yesterday (lyskom-subtract-one-day now))
         (decnow (decode-time now))
         (decthen (decode-time yesterday)))
    (cond ((and (= (time->mday time) (elt decnow 3))
                (= (time->mon time)  (elt decnow 4))
                (= (time->year time) (elt decnow 5)))
           'today)
          ((and (= (time->mday time) (elt decthen 3))
                (= (time->mon time)  (elt decthen 4))
                (= (time->year time) (elt decthen 5)))
           'yesterday)
          (t nil))))


(defun lyskom-format-text-footer (text author author-name format format-flags)
  "Format the footer of a text."
  (let* ((result "")
         (start 0)
         (format-letter nil)
         (field-width nil)
         (kom-deferred-printing nil)
         (author-name-len nil)
         (have-author (and format (string-match "%=?[0-9]*P" format))))

    (setq author-name-len (lyskom-string-width (or author-name (conf-stat->name author))))

    ;; No user-specified format, so output dashes and stuff

    (if (null format)
        (let* ((format-flag-string
                (if (and format-flags (listp format-flags))
                    (let ((first-flag t))
                      (concat
                       "("
                       (mapconcat 
                        (lambda (str)
                          (let ((face nil))
                            ;; If it's a cons, the cdr is property list
                            (when (consp str)
                              (setq face (cdr str)
                                    str (car str)))
                            ;; Get the string
                            (setq str (lyskom-get-string str))
                            ;; Upcase the first in the list
                            (when first-flag 
                              (setq first-flag nil str (upcase-initials str)))
                            ;; If we have a plist, apply it
                            (when face
                              (add-text-properties 0 (length str) face str))
                            ;; Return the string
                            str))
                        format-flags ", ")
                       ")"))
                  ""))
               (end-dash-chars
                (- kom-text-footer-dash-length
                   (lyskom-string-width (int-to-string (text-stat->text-no text))) 
                   3                             ; Parens and space
                   (if kom-show-author-at-end
                       (+ author-name-len 2) 
                     0)
                   (string-width format-flag-string)))
               (end-dash (if (> end-dash-chars 0)
                             (make-string end-dash-chars ?-)
                           "")))
          (lyskom-format
           (cond ((and kom-dashed-lines kom-show-author-at-end)
                  "(%#1n) /%#2P/%#3s%#4s")
                 (kom-dashed-lines
                  "(%#1n) %#3s%#4s")
                 (kom-show-author-at-end
                  "(%#1n) /%#2P/")
                 (t
                  "(%#1n)"))
           text
           (or author-name author)
           end-dash
           format-flag-string))

      ;; User-specified format. Do all kinds of weird stuff

      (while (string-match "%\\(=?-?[0-9]+\\)?\\([-nPpf% ]\\)" format start)
        (setq result (concat result (substring format start 
                                               (match-beginning 0))))
        (setq format-letter (aref format (match-beginning 2)))
        (setq field-width (match-string 1 format))
        (when (null field-width) (setq field-width ""))
        (setq start (match-end 0))
        (setq result
              (concat 
               result
               (cond
                ((eq format-letter ?p)
                 (lyskom-format (format "%%%s#1p" field-width) 
                                (or author (text-stat->author text))))

                ((eq format-letter ?P)
                 (lyskom-format (format "%%%s#1P" field-width) 
                                (or author author-name)))

                ((eq format-letter ?n)
                 (lyskom-format (format "%%%s#1n" field-width) 
                                (text-stat->text-no text)))

                ((eq format-letter ?f)
                 (if format-flags
                     (lyskom-format (format "%%%s#1s" field-width) 
                                    (lyskom-get-string format-flags))
                   ""))

                ((eq format-letter ?-)
                 (let ((width 
                        (cond ((null field-width) 42)
                              ((string= "" field-width) 42)
                              ((eq ?= (aref field-width 0))
                               (string-to-int (substring field-width 1)))
                              (t (string-to-int field-width)))))
                 
                   (lyskom-format 
                    "%#1s"
                    (if have-author
                        (if (< author-name-len width)
                            (make-string (- width author-name-len) ?-)
                          "")
                      (make-string width ?-)))))

                ((eq format-letter ?%)
                 "%")
                (t (concat "%" field-width (make-string 1 format-letter))))))
        )
      (setq result (concat result (substring format start)))
      (if (string-match "[ \t]+\\'" result)
          (substring result 0 (match-beginning 0))
        result))))



(defun lyskom-deferred-insert-footer (conf-stat defer-info)
  "Insert the name of a conference at a previously reserved place."
  (let* ((text-stat (elt (defer-info->data defer-info) 0))
         (format-flags (elt (defer-info->data defer-info) 1))
         (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 17)))
         (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 16)))
         (name (cond (conf-stat nil)
                      ((= (defer-info->call-par defer-info) 0)
                       (lyskom-get-string 'person-is-anonymous))
                      (t (lyskom-format 'person-does-not-exist
                                        (text-stat->author text-stat))))))

    (setq name (or (lyskom-format-mx-author mx-from mx-author) name))

    (lyskom-replace-deferred defer-info 
                             (lyskom-format-text-footer text-stat
                                                        conf-stat
                                                        name
                                                        kom-text-footer-format
                                                        format-flags))))


(defun lyskom-print-text (text-stat text mark-as-read text-no flat-review)
  "Print a text. The header must already be printed.
Print an error message if TEXT-STAT or TEXT is nil.
Mark the text as read if (and only if) MARK-AS-READ is non-nil.
Args: TEXT-STAT TEXT MARK-AS-READ TEXT-NO FLAT-REVIEW."
  (let ((lyskom-current-function 'lyskom-print-text))
    (cond
     ((or (null text)
          (null text-stat))
      (lyskom-format-insert 'no-such-text-no text-no)
      (setq lyskom-previous-text lyskom-current-text)
      (setq lyskom-current-text text-no))
     (t
      (when (eq (text-stat->author text-stat) lyskom-pers-no)
        (setq lyskom-last-seen-written (text-stat->text-no text-stat)))
      (let* ((str (text->decoded-text-mass text text-stat))
	     (truncated nil)
             ;; s1 s2 t1 t2
	     body)

        (cond
         ((string-match "\n" str)
          (setq lyskom-current-subject (substring str 0 (match-beginning 0)))
          (setq body (substring str (match-end 0)))
          (lyskom-insert-string 'head-Subject)
          (let ((lyskom-current-function-phase 'subject))
            (lyskom-format-insert "%#1r\n" 
                                  (copy-sequence lyskom-current-subject)))
            (lyskom-format-insert 
             "%#2$%#1s\n"
             (if kom-dashed-lines
                 (make-string kom-text-header-dash-length ?-)
               "")
             (when kom-highlight-dashed-lines
               '(face kom-dashed-lines-face)))

          ;; (setq t1 (point-max))

	  ;; Truncate body if flat-review and long text
	  (if (and flat-review kom-truncate-show-lines kom-truncate-threshold)
	      (let ((lines 0)
		    (pos 0)
		    (show-lines (min kom-truncate-show-lines
				     kom-truncate-threshold))
		    last-line)
		(while (and pos (setq pos (string-match "\n" body pos)))
		  (setq pos (1+ pos))
		  (setq lines (1+ lines))
		  (if (= lines show-lines)
		      (setq last-line (1- pos)))
		  (if (>= lines kom-truncate-threshold)
		      (setq body (substring body 0 last-line)
			    pos nil
			    truncated t)))))

          (let ((lyskom-current-function-phase 'body))
            (lyskom-format-insert "%#2$%#1t\n" 
                                  (cons text-stat body)
                                  (and kom-highlight-text-body
                                       '(face kom-text-body-face)))
            )

	  ;; Indicate that the text was truncated
	  (if truncated
              (lyskom-signal-reformatted-text
               '(reformat-truncated . (face kom-warning-face))))


          ;; (setq t2 (point-max))
	  )
         (t                             ;No \n found. Don't print header.
          (lyskom-format-insert 
           "%#2$%#1s\n"
           (if kom-dashed-lines
               (make-string kom-text-header-dash-length ?-)
             "")
           (when kom-highlight-dashed-lines
             '(face kom-dashed-lines-face)))
          (lyskom-format-insert "%#2$%#1t\n"
                                (cons text-stat str)
                                (and kom-highlight-text-body
                                       '(face kom-text-body-face)))
          (setq lyskom-current-subject "")))
        (sit-for 0)
        (let* ((lyskom-current-function-phase 'footer)
               (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 17)))
               (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 16)))
               (author-name (lyskom-format-mx-author mx-from mx-author)))

          (if (and kom-deferred-printing
                   (or kom-text-footer-format
                       kom-show-author-at-end))
              (progn
                (lyskom-format-insert "%#2$%#1s\n"
                                      lyskom-defer-indicator
                                      (when kom-highlight-dashed-lines
                                        '(face kom-dashed-lines-face)))
                (lyskom-defer-insertion
                 (lyskom-create-defer-info
                  'get-conf-stat
                  (text-stat->author text-stat)
                  'lyskom-deferred-insert-footer
                  (set-marker (make-marker)
                              (- (point-max) 
                                 (length lyskom-defer-indicator)
                                 1))
                  (length lyskom-defer-indicator)
                  "%#1s"
                  (list text-stat lyskom-last-text-format-flags))))
            (let* ((conf-stat (and (or kom-text-footer-format
                                       kom-show-author-at-end)
                                   (not (eq 0 (text-stat->author text-stat)))
                                   (blocking-do 'get-conf-stat 
                                                (text-stat->author text-stat)))))
              (lyskom-format-insert
               "%#2$%#1s\n"
               (lyskom-format-text-footer 
                text-stat
                conf-stat
                (cond (author-name author-name)
                      ((eq (text-stat->author text-stat) 0)
                       (lyskom-get-string 'person-is-anonymous))
                      (conf-stat (conf-stat->name conf-stat))
                      (t (lyskom-format 'person-does-not-exist 
                                        (text-stat->author text-stat))))
                kom-text-footer-format
                lyskom-last-text-format-flags)
               (when kom-highlight-dashed-lines
                 '(face kom-dashed-lines-face))))))
        (if mark-as-read
            (lyskom-mark-as-read text-stat))
        (setq lyskom-previous-text lyskom-current-text)
        (setq lyskom-current-text (text-stat->text-no text-stat)))))))


(defun lyskom-mark-as-read (text-stat)
  "Mark a text as read in all conferences that are recipients.
Tell the server that it is read.

Args: TEXT-STAT

This function does not remove the text from the internal structures of
the client. That is done by lyskom-is-read."
  (let ((misc-info-list (text-stat->misc-info-list text-stat)))
    (lyskom-traverse
     misc-info misc-info-list
     (if (and (memq (misc-info->type misc-info) '(RECPT BCC-RECPT CC-RECPT))
	      ;; The whole membership list might not be fetched
	      ;; yet. So we better mark it as read in all conferences.
	      ;; (lyskom-member-p (misc-info->recipient-no misc-info))
	      )
	 (initiate-mark-as-read 'background 
                                'lyskom-mark-as-read-callback
				(misc-info->recipient-no misc-info)
				(list (misc-info->local-no misc-info))
                                text-stat
                                (misc-info->recipient-no misc-info)
                                (list (misc-info->local-no misc-info)))))))

(defun lyskom-mark-as-read-callback (result text-stat recipient local-nos)
  (lp--maybe-update-unreads recipient))


(defun lyskom-print-header-recpt (conf-no misc)
  "Print a line of info about a recipient (or cc-recipient) of a text."
  (lyskom-format-insert "%#1s: %#2M <%#3d>\n"
			(cond ((eq (misc-info->type misc) 'RECPT)
			       (lyskom-get-string 'Recipient))
                              ((eq (misc-info->type misc) 'BCC-RECPT)
                               (lyskom-get-string 'Hidden-recipient))
                              ((eq (misc-info->type misc) 'CC-RECPT)
                               (lyskom-get-string 'Extra-recipient))
                              (t (lyskom-get-string 'Strange-recipient)))
			conf-no
			(misc-info->local-no misc))
  (if (misc-info->sent-at misc)
      (lyskom-format-insert 'send-at
			    (lyskom-format-time
                             'date-and-time
			     (misc-info->sent-at misc))))
  (if (misc-info->sender misc)
      (lyskom-format-insert 'sent-by (misc-info->sender misc)))
  (if (misc-info->rec-time misc)
      (lyskom-format-insert
       'received-at
       (lyskom-format-time 'date-and-time (misc-info->rec-time misc)))))




(defun lyskom-view-text-handle-saved-comments (text-stat)
  "Ask server for what is needed to print the references to commenting texts 
and prints the references. Used at the bottom of the text.
Args: TEXT-STAT of the text being read."
  (lyskom-traverse misc
      (text-stat->misc-info-list text-stat)
    (let ((type (misc-info->type misc)))
      (cond
       ((eq type 'COMM-IN)
        (lyskom-print-header-comm (misc-info->comm-in misc) misc text-stat))
       ((eq type 'FOOTN-IN)
	(lyskom-print-header-comm (misc-info->footn-in misc) misc text-stat))))))



(defun lyskom-print-header-comm (text misc read-text-stat)
  "Get author of TEXT-NO and print a header line."
  (let ((text-stat (if kom-deferred-printing
		       (cache-get-text-stat text)
		     (blocking-do 'get-text-stat text))))

    ;; Print information about the link
    (cond (text-stat
           (lyskom-insert-header-comm text-stat misc read-text-stat))
          ((not kom-deferred-printing) 
           (lyskom-insert-header-comm text-stat misc read-text-stat))
          (t
           (let ((defer-info (lyskom-create-defer-info
                              'get-text-stat
                              text
                              'lyskom-insert-deferred-header-comm
                              (point-max-marker)
                              (length lyskom-defer-indicator)
                              nil       ; Filled in later
                              (list misc read-text-stat))))
             (lyskom-format-insert "%#1s\n" lyskom-defer-indicator)
             (lyskom-defer-insertion defer-info))))

    ;; Print information about who added the link
    (if (misc-info->sent-at misc)
	(lyskom-format-insert 'send-at (lyskom-format-time
                                        'date-and-time
					(misc-info->sent-at misc))))
    (if (misc-info->sender misc)
	(lyskom-format-insert 'sent-by (misc-info->sender misc)))))


(defun lyskom-insert-deferred-header-comm (text-stat defer-info)
  (let* ((author (if text-stat (text-stat->author text-stat) nil))
	 (misc (elt (defer-info->data defer-info) 0))
	 (read-text-stat (elt (defer-info->data defer-info) 1))
	 (type (misc-info->type misc))
         (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat)
                                            17)))
         (read-mx-from (car (lyskom-get-aux-item (text-stat->aux-items read-text-stat)
                                                 17)))
         (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat)
                                              16)))
         (mx-attachments-in (lyskom-get-text-attachments read-text-stat))
         (mx-belongs-to (mapcar
                         (lambda (el)
                           (string-to-number (aux-item->data el)))
                         (lyskom-get-aux-item (text-stat->aux-items 
                                               read-text-stat) 10100)))
         (content-type "")
         (read-content-type "")
	 fmt data)

    (setq author (or (lyskom-format-mx-author mx-from mx-author) author))
    ;; Extract the content type

    (if (and mx-from
             (setq content-type
                   (car (lyskom-get-aux-item (text-stat->aux-items 
                                              text-stat) 1))))
        (progn (string-match "^\\(\\S-+\\)" (aux-item->data content-type))
               (setq content-type (format "(%s) "
                                          (aux-item->data content-type))))
      (setq content-type ""))
    
    (if (and read-mx-from
             (setq read-content-type
                   (car (lyskom-get-aux-item (text-stat->aux-items 
                                              read-text-stat) 1))))
        (progn (string-match "^\\(\\S-+\\)" (aux-item->data read-content-type))
               (setq read-content-type (format "(%s) "
                                          (aux-item->data read-content-type))))
      (setq read-content-type ""))
    
    (cond
     ((eq type 'COMM-TO)
      (setq fmt (cond ((memq (misc-info->comm-to misc) mx-belongs-to)
                       'attachment-to-text)
                      (author 'comment-to-text-by)
                      (t 'comment-to-text))
            content-type read-content-type
	    data (misc-info->comm-to misc)))
     ((eq type 'FOOTN-TO)
      (setq fmt (cond ((memq (misc-info->footn-to misc) mx-belongs-to)
                       'attachment-to-text)
                      (author 'footnote-to-text-by)
                      (t 'footnote-to-text))
            content-type read-content-type
	    data (misc-info->footn-to misc)))
     ((eq type 'COMM-IN)
      (setq fmt (cond ((memq (misc-info->comm-in misc) mx-attachments-in) 
                       'attachment-in-text)
                      (author 'comment-in-text-by)
                      (t 'comment-in-text))
	    data (misc-info->comm-in misc)))
     ((eq type 'FOOTN-IN)
      (setq fmt (cond ((memq (misc-info->footn-in misc) mx-attachments-in)
                       'attachment-in-text)
                      (author 'footnote-in-text-by)
                      (t 'footnote-in-text))
	    data (misc-info->footn-in misc)))) 
    (set-defer-info->format defer-info fmt)
    ; Note: author is ignored if fmt is not *-by
    (lyskom-replace-deferred defer-info data author content-type)))



(defun lyskom-insert-header-comm (text-stat misc read-text-stat)
  "Get author of TEXT-NO and print a header line."
  ;;+++ error kommer att se annorlunda ut.
  (let ((author (if text-stat
		    (text-stat->author text-stat)
		  nil))
        (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 17)))
        (read-mx-from (car (lyskom-get-aux-item (text-stat->aux-items read-text-stat) 17)))
        (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 16)))
        (mx-attachments-in (lyskom-get-text-attachments read-text-stat))
        (mx-belongs-to (mapcar
                        (lambda (el)
                          (string-to-number (aux-item->data el)))
                        (lyskom-get-aux-item (text-stat->aux-items read-text-stat) 10100)))
        (content-type "")
        (read-content-type "")
        (is-attachment nil)
        (type (misc-info->type misc)))

    (setq author (or (lyskom-format-mx-author mx-from mx-author) author))

    ;; Extract the content type

    (if (and mx-from
             (setq content-type
                   (car (lyskom-get-aux-item (text-stat->aux-items 
                                              text-stat) 1))))
        (progn (string-match "^\\(\\S-+\\)" (aux-item->data content-type))
               (setq content-type (format "(%s) "
                                          (aux-item->data content-type))))
      (setq content-type ""))

    (if (and read-mx-from
             (setq read-content-type
                   (car (lyskom-get-aux-item (text-stat->aux-items 
                                              read-text-stat) 1))))
        (progn (string-match "^\\(\\S-+\\)" (aux-item->data read-content-type))
               (setq read-content-type (format "(%s) "
                                          (aux-item->data read-content-type))))
      (setq read-content-type ""))

    
    (cond
     ((eq type 'COMM-TO)
      (lyskom-format-insert (if (and (memq (misc-info->comm-to misc) mx-belongs-to)
                                     (setq is-attachment t))
                                'attachment-to-text
                              'comment-to-text)
                            (misc-info->comm-to misc)
                            nil
                            read-content-type))
     ((eq type 'FOOTN-TO)
      (lyskom-format-insert (if (and (memq (misc-info->footn-to misc) mx-belongs-to)
                                     (setq is-attachment t))
                                'attachment-to-text
                              'footnote-to-text)
                            (misc-info->footn-to misc)
                            nil
                            read-content-type))
     ((eq type 'COMM-IN)
      (lyskom-format-insert (if (and (memq (misc-info->comm-in misc) mx-attachments-in)
                                     (setq is-attachment t))
                                'attachment-in-text
                              'comment-in-text)
                            (misc-info->comm-in misc)
                            nil
                            content-type))
     ((eq type 'FOOTN-IN)
      (lyskom-format-insert (if (and (memq (misc-info->footn-in misc) mx-attachments-in)
                                     (setq is-attachment t))
                                'attachment-in-text
                              'footnote-in-text)
                            (misc-info->footn-in misc)
                            nil
                            content-type)))
    (if (and author (not is-attachment))
	(lyskom-format-insert 'written-by author)
      (lyskom-insert "\n"))))

(defun lyskom-format-mx-author (mx-from mx-author)
  (let ((author nil)
        (mx-author (if (and mx-author (equal (aux-item->data mx-author) ""))
                       nil
                     mx-author)))
    (when (or mx-from mx-author)
      (setq author (lyskom-get-string 'email-name-prefix))
      (when mx-author (setq author (concat author (aux-item->data mx-author))))
      (when (and mx-from mx-author) (setq author (concat author " ")))
      (when mx-from
        (setq author (concat author
                      (lyskom-format 
                       "<%[%#1@%#2s%]>"
                       (lyskom-default-button 'email (aux-item->data mx-from))
                       (aux-item->data mx-from)))))
      (setq author (concat (lyskom-get-string 'email-name-suffix) author))
      )
    author))

(defun lyskom-mx-date-to-time (mx-date)
 "Attempt to convert MX-DATE to a lyskom time structure.
Returns the time structure if successful, otherwise nil."
  (if (and mx-date
           (condition-case nil
               (progn (require 'calendar)
                      (require 'cal-iso)
                      t)
             (error nil))
           (string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\) \\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\) \\([-+][0-9][0-9]\\)?\\([0-9][0-9]\\)?"
                         (aux-item->data mx-date)))
      (let* ((secs (string-to-number (match-string 6 (aux-item->data mx-date))))
             (mins (string-to-number (match-string 5 (aux-item->data mx-date))))
             (hour (string-to-number (match-string 4 (aux-item->data mx-date))))
             (mday (string-to-number (match-string 3 (aux-item->data mx-date))))
             (mon  (string-to-number (match-string 2 (aux-item->data mx-date))))
             (year (string-to-number (match-string 1 (aux-item->data mx-date))))
             (tzhr (match-string 7 (aux-item->data mx-date)))
             (tzmin (or (match-string 8 (aux-item->data mx-date)) ""))
             (wday (abs
                    (elt 
                     (calendar-iso-from-absolute
                      (calendar-absolute-from-gregorian
                       (list mon mday year)))
                     1))))
        (lyskom-create-time secs mins hour mday mon year wday 0 nil tzhr tzmin))))

;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; end: 
