;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: view-text.el,v 44.24 1999-10-09 16:13:32 byers Exp $
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
	      "$Id: view-text.el,v 44.24 1999-10-09 16:13:32 byers Exp $\n"))


(defun lyskom-view-text (text-no &optional mark-as-read
				 follow-comments conf-stat priority
				 build-review-tree filter-active
				 flat-review)
  "Display text number TEXT-NO.
Args: TEXT-NO &optional MARK-AS-READ FOLLOW-COMMENTS CONF-STAT 
PRIORITY BUILD-REVIEW-TREE FLAT-REVIEW.
If MARK-AS-READ is non-nil the text will be marked as read.
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
        (end nil)
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
			(eq filter 'dontshow)))
	       (lyskom-message "%s" (lyskom-get-string 'invalid-filter-list)))
	   (if (eq filter 'dontshow)
	       (setq todo 'next-text))
	   (blocking-do-multiple ((text-stat (get-text-stat text-no))
				  (text (get-text text-no)))
	     (if (and text-stat text)
		 (progn
                   (run-hooks 'lyskom-view-text-hook)
		   ;; Use a marker, because the buffer may lose data
		   ;; at the top if kom-max-buffer-size is set.
		   (setq start (point-max-marker))
		   (lyskom-format-insert "%#1n " text-stat)
                   (let ((mx-date (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 21)))
                         (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 17)))
                         (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 16)))
                         (mx-to (lyskom-get-aux-item (text-stat->aux-items text-stat) 19))
                         (mx-cc (lyskom-get-aux-item (text-stat->aux-items text-stat) 20))
                         (mx-filename (lyskom-get-aux-item (text-stat->aux-items text-stat) 10104))
                         (mx-sender (lyskom-get-aux-item (text-stat->aux-items text-stat) 10103))
                         (mx-reply-to (lyskom-get-aux-item (text-stat->aux-items text-stat) 28)))
                         

                     ;; Insert date

                     (if mx-date
                         (if (and 
                              (condition-case nil
                                  (progn (require 'calendar)
                                         (require 'cal-iso)
                                         t)
                                nil)
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
                                     (wday (elt 
                                            (calendar-iso-from-absolute
                                             (calendar-absolute-from-gregorian
                                              (list mon mday year)))
                                            1)))
                             (lyskom-print-date-and-time
                              (lyskom-create-time secs mins hour mday mon (- year 1900) wday 0 nil))
                             (when tzhr 
                               (lyskom-format-insert " %#1s%#2s " tzhr tzmin)))
                           (lyskom-format-insert (aux-item->data mx-date)))
                       (lyskom-print-date-and-time (text-stat->creation-time
                                                    text-stat)
                                                   'time-y-m-d-h-m))

                     ;; Insert number of lines

		   (lyskom-insert 
		    (if (= 1 (text-stat->no-of-lines text-stat))
			(lyskom-get-string 'line)
		      (lyskom-format 'lines 
				     (let ((n (text-stat->no-of-lines
					       text-stat)))
				       (if (= n 0) ; Added to increase
					   2 ; compatibility with old KOM. /lw
					 n)))))
		   
                   ;; Insert the author

                   (lyskom-insert (or (lyskom-format-mx-author mx-from mx-author) ""))
                   (unless (or mx-from mx-author)
                     (lyskom-format-insert "%#1P" (text-stat->author text-stat)))

                   ;; Insert filtration prompt

		   (when (eq filter 'dontshow)
                     (lyskom-insert " ")
                     (lyskom-insert (lyskom-get-string 'filtered)))
                   (lyskom-insert "\n")

                   ;; Insert sender

                   (when mx-sender
                     (lyskom-format-insert 'envelope-sender 
                                           (aux-item->data (car mx-sender))))
                   (when mx-filename
                     (lyskom-format-insert 'attachment-filename
                                           (aux-item->data (car mx-filename))))

                   ;; Insert imported at

                   (cond ((and mx-from (text-stat->author text-stat))
                          (lyskom-format-insert 'text-imported-at-by
                                                (lyskom-return-date-and-time (text-stat->creation-time
                                                                              text-stat)
                                                                             'time-y-m-d-h-m)
                                                (text-stat->author text-stat)))
                         (mx-from
                          (lyskom-format-insert 'text-imported-at
                                                (lyskom-return-date-and-time (text-stat->creation-time
                                                                              text-stat)
                                                                             'time-y-m-d-h-m)))
                         (mx-date (lyskom-format-insert 'text-created-at
                                                        (lyskom-return-date-and-time (text-stat->creation-time
                                                                                      text-stat)
                                                                                     'time-y-m-d-h-m))))
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
                                                   (lyskom-get-string 'mx-Reply-to)
                                                   (aux-item->data el)))
                           mx-reply-to)

		   
		   (setq end (point-max))

		   (if (and kom-text-properties
			    (null filter)
			    (not (lyskom-face-default-p 'kom-first-line-face)))
		       (add-text-properties
			start end '(face kom-first-line-face)))
		   (set-marker start nil)

		   ;; All recipients and other header lines.

		   (if (eq filter 'dontshow)
		       (lyskom-mark-as-read
			(blocking-do 'get-text-stat text-no))
		     (lyskom-traverse misc
			 (text-stat->misc-info-list text-stat)
		       (let ((type (misc-info->type misc)))
			 (cond
			  ((or (eq type 'RECPT)
                               (eq type 'BCC-RECPT)
			       (eq type 'CC-RECPT))
			   (lyskom-print-header-recpt 
			    (misc-info->recipient-no misc)
			    misc))
			  ((eq type 'COMM-IN)
			   (if kom-reading-puts-comments-in-pointers-last
			       nil
                             (if lyskom-show-comments ; +++SOJGE
                                 (lyskom-print-header-comm
                                  (misc-info->comm-in misc)
                                  misc
                                  text-stat))))
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

                     ;;
                     ;; Print aux-items that go in the header
                     ;;

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
                                   (lyskom-aux-item-definition-call 
                                    aux
                                    'text-print
                                    aux
                                    text-stat)))
                           (lyskom-insert text)
                           (lyskom-insert "\n"))))


		     (let ((num-marks (text-stat->no-of-marks text-stat))
			   (is-marked-by-me (cache-text-is-marked
					     (text-stat->text-no text-stat))))
		       (if (> num-marks 0)
			   (lyskom-insert 
			    (if is-marked-by-me
				(if (= num-marks 1)
				    (lyskom-get-string 'marked-by-you)
                                  (lyskom-format 'marked-by-you-and-others
                                                 (1- num-marks)))
                              (lyskom-format 'marked-by-several num-marks)))))
		   
		     (lyskom-print-text text-stat text
					mark-as-read text-no flat-review))
		   
		   
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
                                   (lyskom-aux-item-definition-call 
                                    aux
                                    'text-print
                                    aux
                                    text-stat)))
                           (lyskom-insert text)
                           (lyskom-insert "\n"))))


		   (if kom-reading-puts-comments-in-pointers-last
		       (lyskom-view-text-handle-saved-comments text-stat))
		   
		   (if (or follow-comments
			   ;; Checking build-review-tree should not be
			   ;; necessary, really /davidk
			   build-review-tree)
		       ;; This shows footnotes also.
		       (progn
			 ;; this is a bit silly, as it prefetches all
			 ;; the texts many times.
			 (lyskom-prefetch-texttree text-no)
			 (lyskom-follow-comments text-stat
						 conf-stat mark-as-read
						 priority build-review-tree)))
		   ))
	       (lyskom-format-insert 'no-such-text-no text-no))
             (let ((aux-items (text-stat->aux-items text-stat)))
               (while aux-items
                 (lyskom-aux-item-definition-call (car aux-items)
                                                  'read-action
                                                  text-stat)
                 (setq aux-items (cdr aux-items))))
             )))
    todo))
	  
	  


(defun lyskom-follow-comments (text-stat conf-stat 
					 mark-as-read priority review-tree)
  "Arrange so that all comments and footnotes to this text will be read.
This will essentially fix the reading list and issue calls for the cache to be
filled.

Arguments are: TEXT-STAT CONF-STAT PRIORITY REVIEW-TREE
TEXT-STAT is the current text.
CONF-STAT the current conference
PRIORITY the priority of the reading.
If REVIEW-TREE is non-nil then build an entry of type 'REVIEW-TREE in the 
lyskom-reading-list."
  ;; . Find footnotes and show them.
  ;; . Fix the reading-list
  ;; . Issue cache-filling initiate-calls for everything left comments.
  
  (let (flist clist)
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
        (if lyskom-show-comments        ; +++SOJGE
            (setq clist (cons (misc-info->comm-in misc) clist))))))
    
    (let (comments footnotes)
      ;; Find the comments that we should read and enter them into the
      ;; read-list.
      (lyskom-traverse no clist
	(let ((text-stat (blocking-do 'get-text-stat no)))
	  (if (or review-tree
		  (and text-stat
		       (not (lyskom-text-read-p text-stat))))
	      (setq comments (cons no comments)))))
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
	 ((or (eq type 'RECPT) 
              (eq type 'BCC-RECPT)
              (eq type 'CC-RECPT))
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
          (cond ((or (eq type 'RECPT) 
                     (eq type 'BCC-RECPT)
                     (eq type 'CC-RECPT))
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
                (= (1+ (time->mon time)) (elt decnow 4))
                (= (+ (time->year time) 1900) (elt decnow 5)))
           'today)
          ((and (= (time->mday time) (elt decthen 3))
                (= (1+ (time->mon time)) (elt decthen 4))
                (= (+ (time->year time) 1900) (elt decthen 5)))
           'yesterday)
          (t nil))))

(defun lyskom-return-date-and-time (time &optional fmt)
  "Return date and time as a string. Arg: TIME."
  (let* ((diff (and lyskom-print-complex-dates
		    (lyskom-calculate-day-diff time))))
    (lyskom-format
     (if diff
         (intern (concat (symbol-name diff) "-time-format-string"))
       (or fmt 'time-yyyy-mm-dd-hh-mm))
     (+ (time->year time) 1900)
     (1+ (time->mon  time))
     (time->mday time)
     (time->hour time)
     (time->min  time)
     (and diff (lyskom-get-string diff)))))
                  
(defun lyskom-print-date-and-time (time &optional fmt)
  "Print date and time. Arg: TIME"
  (lyskom-insert (lyskom-return-date-and-time time fmt)))

(defun lyskom-format-text-footer (text author author-name format format-flags)
  "Format the footer of a text."
  (let* ((result "")
         (start 0)
         (format-letter nil)
         (field-width nil)
         (kom-deferred-printing nil)
         (have-author (and format (string-match "%=?[0-9]*P" format))))

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
                   (length (int-to-string (text-stat->text-no text))) 
                   3                             ; Parens and space
                   (if kom-show-author-at-end
                       (+ (length author-name) 2) 
                     0)
                   (length format-flag-string)))
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
                        (if (< (length author-name) width)
                            (make-string (- width (length author-name)) ?-)
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
         (name (cond (conf-stat nil)
                     ((= (defer-info->call-par defer-info) 0)
                      (lyskom-get-string 'person-is-anonymous))
                     (t (lyskom-format 'person-does-not-exist
                                       (defer-info->call-par defer-info)))))
         (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 17)))
         (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 16))))

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
      (let* ((str (text->text-mass text))
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
          (when kom-dashed-lines
              (lyskom-insert 
               (make-string kom-text-header-dash-length ?-)))

          (lyskom-insert "\n")
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
            (lyskom-format-insert "%#1t\n" (cons text-stat body)))

	  ;; Indicate that the text was truncated
	  (if truncated
              (lyskom-signal-reformatted-text
               '(reformat-truncated . (face kom-warning-face))))

	  
          ;; (setq t2 (point-max))
	  )
         (t                             ;No \n found. Don't print header.
          (when kom-dashed-lines
            (lyskom-insert 
             (make-string kom-text-header-dash-length ?-)))
          (lyskom-insert "\n")
          (lyskom-format-insert "%#1t\n" (cons text-stat str))
          (setq lyskom-current-subject "")))
        (if (lyskom-text-p (cache-get-text (text->text-no text)))
            (cache-del-text (text->text-no text)))
        (sit-for 0)
        (let ((lyskom-current-function-phase 'footer))
          (if kom-deferred-printing
              (progn
                (lyskom-format-insert "%#1s\n" lyskom-defer-indicator)
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
            (let* ((conf-stat (blocking-do 'get-conf-stat 
                                           (text-stat->author text-stat)))
                   (author-name
                    (or (conf-stat->name conf-stat)
                        (and (eq (text-stat->author text-stat) 0)
                             (lyskom-get-string 'person-is-anonymous))
                        (lyskom-format 'person-does-not-exist
                                       (text-stat->author text-stat)))))
              (lyskom-insert (lyskom-format-text-footer 
                              text-stat
                              conf-stat
                              author-name
                              kom-text-footer-format
                              lyskom-last-text-format-flags)))
            (lyskom-insert "\n")))
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
     (if (and (or (eq (misc-info->type misc-info) 'RECPT)
                  (eq (misc-info->type misc-info) 'BCC-RECPT)
		  (eq (misc-info->type misc-info) 'CC-RECPT))
	      ;; The whole membership list might not be fetched
	      ;; yet. So we better mark it as read in all conferences.
	      ;; (lyskom-member-p (misc-info->recipient-no misc-info))
	      )
	 (initiate-mark-as-read 'background nil
				(misc-info->recipient-no misc-info)
				(list (misc-info->local-no misc-info)))))))


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
			    (lyskom-return-date-and-time 
			     (misc-info->sent-at misc))))
  (if (misc-info->sender misc)
      (lyskom-format-insert 'sent-by (misc-info->sender misc)))
  (if (misc-info->rec-time misc)
      (lyskom-format-insert
       'recieved-at
       (lyskom-return-date-and-time (misc-info->rec-time misc)))))




(defun lyskom-view-text-handle-saved-comments (text-stat)
  "Ask server for what is needed to print the references to commenting texts 
and prints the references. Used at the bottom of the text.
Args: TEXT-STAT of the text being read."
  (lyskom-traverse misc
      (text-stat->misc-info-list text-stat)
    (let ((type (misc-info->type misc)))
      (cond
       ((eq type 'COMM-IN)
        (if lyskom-show-comments        ;+++SOJGE
            (lyskom-print-header-comm (misc-info->comm-in misc) misc text-stat)))
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
	(lyskom-format-insert 'send-at (lyskom-return-date-and-time 
					(misc-info->sent-at misc))))
    (if (misc-info->sender misc)
	(lyskom-format-insert 'sent-by (misc-info->sender misc)))))


(defun lyskom-insert-deferred-header-comm (text-stat defer-info)
  (let* ((author (if text-stat (text-stat->author text-stat) nil))
	 (misc (elt (defer-info->data defer-info) 0))
	 (read-text-stat (elt (defer-info->data defer-info) 1))
	 (type (misc-info->type misc))
         (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 17)))
         (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 16)))
         (mx-attachments-in (mapcar
                             (lambda (el)
                               (string-to-number (aux-item->data el)))
                             (lyskom-get-aux-item (text-stat->aux-items read-text-stat) 10101)))
         (mx-belongs-to (mapcar
                         (lambda (el)
                           (string-to-number (aux-item->data el)))
                         (lyskom-get-aux-item (text-stat->aux-items read-text-stat) 10100)))
	 fmt data)

    (setq author (or (lyskom-format-mx-author mx-from mx-author) author))
    
    (cond
     ((eq type 'COMM-TO)
      (setq fmt (cond ((memq (misc-info->comm-to misc) mx-belongs-to) 'attachment-to-text)
                      (author 'comment-to-text-by)
                      (t 'comment-to-text))
	    data (misc-info->comm-to misc)))
     ((eq type 'FOOTN-TO)
      (setq fmt (cond ((memq (misc-info->footn-to misc) mx-belongs-to) 'attachment-to-text)
                      (author 'footnote-to-text-by)
                      (t 'footnote-to-text))
	    data (misc-info->footn-to misc)))
     ((eq type 'COMM-IN)
      (setq fmt (cond ((memq (misc-info->comm-in misc) mx-attachments-in) 'attachment-in-text)
                      (author 'comment-in-text-by)
                      (t 'comment-in-text))
	    data (misc-info->comm-in misc)))
     ((eq type 'FOOTN-IN)
      (setq fmt (cond ((memq (misc-info->footn-in misc) mx-attachments-in) 'attachment-in-text)
                      (author 'footnote-in-text-by)
                      (t 'footnote-in-text))
	    data(misc-info->footn-in misc)))) 
    (set-defer-info->format defer-info fmt)
    ; Note: author is ignored if fmt is not *-by
    (lyskom-replace-deferred defer-info data author)))



(defun lyskom-insert-header-comm (text-stat misc read-text-stat)
  "Get author of TEXT-NO and print a header line."
  ;;+++ error kommer att se annorlunda ut.
  (let ((author (if text-stat
		    (text-stat->author text-stat)
		  nil))
        (mx-from (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 17)))
        (mx-author (car (lyskom-get-aux-item (text-stat->aux-items text-stat) 16)))
        (mx-attachments-in (mapcar
                            (lambda (el)
                              (string-to-number (aux-item->data el)))
                            (lyskom-get-aux-item (text-stat->aux-items read-text-stat) 10101)))
        (mx-belongs-to (mapcar
                        (lambda (el)
                          (string-to-number (aux-item->data el)))
                        (lyskom-get-aux-item (text-stat->aux-items read-text-stat) 10100)))
        (type (misc-info->type misc)))

    (setq author (or (lyskom-format-mx-author mx-from mx-author) author))
    
    (cond
     ((eq type 'COMM-TO)
      (lyskom-format-insert (if (memq (misc-info->comm-to misc) mx-belongs-to)
                                'attachment-to-text
                              'comment-to-text)
                            (misc-info->comm-to misc)))
     ((eq type 'FOOTN-TO)
      (lyskom-format-insert (if (memq (misc-info->footn-to misc) mx-belongs-to)
                                'attachment-to-text
                              'footnote-to-text)
                            (misc-info->footn-to misc)))
     ((eq type 'COMM-IN)
      (lyskom-format-insert (if (memq (misc-info->comm-in misc) mx-attachments-in)
                                'attachment-in-text
                              'comment-in-text)
                            (misc-info->comm-in misc)))
     ((eq type 'FOOTN-IN)
      (lyskom-format-insert (if (memq (misc-info->footn-in misc) mx-attachments-in)
                                'attachment-in-text
                              'footnote-in-text)
                            (misc-info->footn-in misc))))
    (if author
	(lyskom-format-insert 'written-by author)
      (lyskom-insert "\n"))))

(defun lyskom-format-mx-author (mx-from mx-author)
  (let ((author nil))
    (when (or mx-from mx-author)
      (setq author (lyskom-get-string 'email-name-prefix))
      (when mx-author (setq author (concat author (aux-item->data mx-author))))
      (when (and mx-from mx-author) (setq author (concat author " ")))
      (when mx-from (setq author (concat author "<" (aux-item->data mx-from) ">")))
      (setq author (concat (lyskom-get-string 'email-name-suffix) author))
      )
    author))
    


;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; end: 
