;;;;;
;;;;; $Id: view-text.el,v 40.2 1996-04-14 23:39:24 davidk Exp $
;;;;; Copyright (C) 1991  Lysator Academic Computer Association.
;;;;;
;;;;; This file is part of the LysKOM server.
;;;;; 
;;;;; LysKOM is free software; you can redistribute it and/or modify it
;;;;; under the terms of the GNU General Public License as published by 
;;;;; the Free Software Foundation; either version 1, or (at your option) 
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
	      "$Id: view-text.el,v 40.2 1996-04-14 23:39:24 davidk Exp $\n"))


(defun lyskom-view-text (text-no &optional mark-as-read
				 follow-comments conf-stat priority
				 build-review-tree filter-active)
  "Display text number TEXT-NO.
Args: TEXT-NO &optional MARK-AS-READ FOLLOW-COMMENTS CONF-STAT 
PRIORITY BUILD-REVIEW-TREE.
If MARK-AS-READ is non-nil the text will be marked as read.
If FOLLOW-COMMENTS is non-nil all comments and footnotes to this text will be
read before the next text. CONF-STAT must be the conference status of the
current conference, and PRIORITY the priority, if FOLLOW-COMMENTS is non-nil.
If BUILD-REVIEW-TREE is non-nil then it fixes a new entry in the 
lyskom-reading-list to read the comments to this.

Note that this function must not be called asynchronously."

  (let ((filter (and filter-active
                     (lyskom-filter-text-p text-no)))
        (start nil)
        (end nil)
        (todo nil))
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
	     (if (and text-stat
		      text)
		 (progn
           (setq start (point-max))
		   (lyskom-format-insert "%#1n " 
					 text-stat)
		   (lyskom-print-date-and-time (text-stat->creation-time
						text-stat)
					       'time-y-m-d-h-m)
		   (lyskom-insert 
		    (if (= 1 (text-stat->no-of-lines text-stat))
			(lyskom-get-string 'line)
		      (lyskom-format 'lines 
				     (let ((n (text-stat->no-of-lines
					       text-stat)))
				       (if (= n 0) ; Added to increase
					   2 ; compatibility with old KOM. /lw
					 n)))))
		   
		   (if (eq filter 'dontshow)
		       (lyskom-format-insert "%#1P %#2s\n"
                                     (text-stat->author text-stat)
                                     (lyskom-get-string 'filtered))
		     (lyskom-format-insert "%#1P\n"
                                   (text-stat->author text-stat)))
		   
           (setq end (point-max))

           (if (and (null filter)
                    (not (lyskom-face-default-p 'kom-first-line-face)))
               (add-text-properties start end '(face kom-first-line-face)))

		   ;; All recipients and other header lines.

		   (if (eq filter 'dontshow)
		       (lyskom-mark-as-read
			(blocking-do 'get-text-stat text-no))
		     (lyskom-traverse misc
			 (text-stat->misc-info-list text-stat)
		       (let ((type (misc-info->type misc)))
			 (cond
			  ((or (eq type 'RECPT)
			       (eq type 'CC-RECPT))
			   (lyskom-print-header-recpt 
			    (blocking-do 'get-conf-stat
					 (misc-info->recipient-no
					  misc))
			    misc))
			  ((eq type 'COMM-IN)
			   (if kom-reading-puts-comments-in-pointers-last
			       nil
			     (lyskom-print-header-comm
			      (misc-info->comm-in misc)
			      misc)))
			  ((eq type 'FOOTN-IN)
			   (if kom-reading-puts-comments-in-pointers-last
			       nil
			     (lyskom-print-header-comm
			      (misc-info->footn-in misc)
			      misc)))
			  ((eq type 'COMM-TO)
			   (lyskom-print-header-comm (misc-info->comm-to misc)
						     misc))
			  ((eq type 'FOOTN-TO)
			   (lyskom-print-header-comm (misc-info->footn-to misc)
						     misc))
			  )))
		     (let ((num-marks (text-stat->no-of-marks text-stat))
			   (is-marked-by-me (cache-text-is-marked
					     (text-stat->text-no text-stat))))
		       (if (> num-marks 0)
			   (lyskom-insert 
			    (if is-marked-by-me
				(if (= num-marks 1)
				    (lyskom-get-string 'marked-by-you)
				  (if (= num-marks 2)
				      (lyskom-get-string 
				       'marked-by-you-and-one)
				    (lyskom-format 'marked-by-you-and-several
						   (1- num-marks))))
			      (if (= num-marks 1)
				  (lyskom-get-string 'marked-by-one)
				(lyskom-format 'marked-by-several
					       num-marks))))))
		   
		     (lyskom-print-text text-stat text
					mark-as-read text-no))
		   
		   
		   (if kom-reading-puts-comments-in-pointers-last
		       (lyskom-view-text-handle-saved-comments text-stat))
		   
		   (if (or follow-comments
			   build-review-tree)
		       ;; This shows footnotes also.
		       (lyskom-follow-comments text-stat conf-stat mark-as-read
					       priority build-review-tree))
		   )
	       (lyskom-format-insert 'no-such-text text-no)))))
    todo))
	  
	  
(defun lyskom-insert-person-name (conf-no)
	    "Inserts the name the conf CONF-NO. If CONF-NO is 0 this person does not exist."
  (let ((cs nil))
    (if (zerop conf-no)
	nil
      (setq cs (blocking-do 'get-conf-stat conf-no)))
    (if cs
	(lyskom-insert (conf-stat->name cs))
      (if (eq conf-no 0)
	  (lyskom-format-insert 'person-is-anonymous)
	(lyskom-format-insert 'person-does-not-exist conf-no)))))


(defun lyskom-insert-conf-name (conf-no)
  "Inserts the name the conf CONF-NO. If CONF-NO is 0 this person does not exist."
  (let ((cs nil))
    (if (zerop conf-no)
	nil
      (setq cs (blocking-do 'get-conf-stat conf-no)))
    (if cs
	(lyskom-insert (conf-stat->name cs))
      (lyskom-format-insert 'conf-does-not-exist conf-no))))



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
  
  (let ((list))
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
	(setq list (cons (misc-info->footn-in misc) list)))
       ((eq (misc-info->type misc) 'COMM-IN)
	(setq list (cons (misc-info->comm-in misc) list)))))

    (let ((comments nil))
      (lyskom-traverse no list
	(cond
	 ((or review-tree
	      (not (lyskom-text-read-p (blocking-do 'get-text-stat no))))
	  (setq comments (cons no comments)))))
      (if comments
	  (read-list-enter-read-info
	   (lyskom-create-read-info (if review-tree 'REVIEW-TREE 'COMM-IN)
				    conf-stat priority
				    (lyskom-create-text-list comments)
				    (text-stat->text-no text-stat))
	   lyskom-reading-list t))
      ;; We have most of the text stats in the cache...
      ;; Just a few stray things to fill the cache...
      (lyskom-traverse no comments
	(initiate-get-text-stat 'fill-cache 'lyskom-fetch-text-for-cache no)))))


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
	 (res t))
    (while (and res (not (zerop i)))
      (setq i (1- i))
      (let* ((misc-info (elt misc-info-list i))
	     (type (misc-info->type misc-info)))
	(cond
	 ((or (eq type 'RECPT) (eq type 'CC-RECPT))
	  (let ((membership (lyskom-member-p
			     (misc-info->recipient-no misc-info)))
		(loc-no (misc-info->local-no misc-info)))
	    (if (and membership
		     (> loc-no (membership->last-text-read membership))
		     (not (lyskom-vmemq loc-no
				       (membership->read-texts membership))))
		(setq res nil)))))))
    res))


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


(defun lyskom-print-text (text-stat text mark-as-read text-no)
  "Print a text. The header must already be printed.
Print an error message if TEXT-STAT or TEXT is nil.
Mark the text as read if (and only if) MARK-AS-READ is non-nil.
Args: TEXT-STAT TEXT MARK-AS-READ TEXT-NO."
  (cond
     ((or (null text)
	  (null text-stat))
      (lyskom-format-insert 'no-such-text-no text-no)
      (setq lyskom-previous-text lyskom-current-text)
      (setq lyskom-current-text text-no))
   (t
      (let* ((str (text->text-mass text))
	     s1 s2 t1 t2 body)
	(cond
	 ((string-match "\n" str)
	    (setq lyskom-current-subject (substring str 0 (match-beginning 0)))
	    (setq body (substring str (match-end 0)))
	    (lyskom-insert-string 'head-Subject)
	    (setq s1 (point-max))
	    (lyskom-format-insert "%#1r\n" 
				  (copy-sequence lyskom-current-subject))
	    (setq s2 (point-max))
	    (if kom-dashed-lines
		(lyskom-insert 
		 "------------------------------------------------------------\n")
	      (lyskom-insert "\n"))
	    (setq t1 (point-max))
	    (lyskom-format-insert "%#1t" body)
	    (setq t2 (point-max)))
	 (t				;No \n found. Don't print header.
	  (setq s1 (point-max))
	  (lyskom-format-insert "%#1t" str)
	  (setq s2 (point-max))
	  (setq t1 (point-max)
		t2 (point-max))
	  (setq lyskom-current-subject "")))
	(if (lyskom-text-p (cache-get-text (text->text-no text)))
	    (cache-del-text (text->text-no text)))
	(sit-for 0)
	(lyskom-format-insert 
			(if kom-dashed-lines
			    "\n(%#1n) -----------------------------------\n"
			  "\n(%#1n)\n")
			(text->text-no text))
	(if mark-as-read
	    (lyskom-mark-as-read text-stat))
	(setq lyskom-previous-text lyskom-current-text)
	(setq lyskom-current-text (text-stat->text-no text-stat))))))


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
		  (eq (misc-info->type misc-info) 'CC-RECPT))
	      (lyskom-member-p (misc-info->recipient-no misc-info)))
	 (initiate-mark-as-read 'background nil
				(misc-info->recipient-no misc-info)
				(list (misc-info->local-no misc-info)))))))


(defun lyskom-print-header-recpt (conf-stat misc)
  "Print a line of info about a recipient (or cc-recipient) of a text."
  (lyskom-format-insert "%#1s: %#2M <%#3d>\n"
			(cond ((eq (misc-info->type misc) 'RECPT)
			       (lyskom-get-string 'Recipient))
			      (t (lyskom-get-string 'Extra-recipient)))
			conf-stat
			(misc-info->local-no misc))
  (if (misc-info->sent-at misc)
      (lyskom-format-insert 'send-at
			    (lyskom-return-date-and-time 
			     (misc-info->sent-at misc))))
  (if (misc-info->sender misc)
	(lyskom-insert (lyskom-format 'sent-by (misc-info->sender misc))))
  (if (misc-info->rec-time misc)
      (lyskom-format-insert 'recieved-at
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
	(lyskom-print-header-comm (misc-info->comm-in misc) misc))
       ((eq type 'FOOTN-IN)
	(lyskom-print-header-comm (misc-info->footn-in misc) misc))))))


(defun lyskom-print-header-comm (text misc)
  "Get author of TEXT-NO and print a header line."
  (let ((text-stat (blocking-do 'get-text-stat text)))
    ;;+++ error kommer att se annorlunda ut.
    (if text-stat
	(let* ((author (blocking-do 'get-conf-stat
				    (text-stat->author text-stat)))
	       (type (misc-info->type misc))
	       (tyname (cond ((eq 0 (text-stat->author text-stat))
                          (lyskom-get-string 'person-is-anonymous))
                         ((null author)
                          (lyskom-format 'person-does-not-exist
                                         (text-stat->author text-stat)))
                         (t author))))

	  (cond
	   ((eq type 'COMM-TO)
	    (lyskom-format-insert 'comment-to-text-by 
				  (misc-info->comm-to misc)
				  tyname))
	   ((eq type 'FOOTN-TO)
	    (lyskom-format-insert 'footnote-to-text-by
				  (misc-info->footn-to misc)
				  tyname))
	   ((eq type 'COMM-IN)
	    (lyskom-format-insert 'comment-in-text-by
				  (misc-info->comm-in misc)
				  tyname))
	   ((eq type 'FOOTN-IN)
	    (lyskom-format-insert 'footnote-in-text-by
				  (misc-info->footn-in misc)
				  tyname)))
	  ;; Print information about who added the link
	  (if (misc-info->sent-at misc)
	      (lyskom-format-insert 'send-at
				    (lyskom-return-date-and-time 
				     (misc-info->sent-at misc))))
	  (if (misc-info->sender misc)
	      (lyskom-insert (lyskom-format 'sent-by (misc-info->sender misc)))))
      ;; Client tolerans agains buggy servers...
      ;; We are writing the line about what comments exists and
      ;; the reference text does not exist anymore. Strange.
      nil)))



;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; end: 
