;;;;;
;;;;; $Id: view-text.el,v 44.8 1997-07-12 13:11:46 byers Exp $
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
	      "$Id: view-text.el,v 44.8 1997-07-12 13:11:46 byers Exp $\n"))


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
	     (if (and text-stat
		      text)
		 (progn
                   (run-hooks 'lyskom-view-text-hook)
		   ;; Use a marker, because the buffer may lose data
		   ;; at the top if kom-max-buffer-size is set.
		   (setq start (point-max-marker))
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
                                  misc))))
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
		   )
	       (lyskom-format-insert 'no-such-text-no text-no)))))
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
	  (read-list-enter-read-info
	   (lyskom-create-read-info (if review-tree 'REVIEW-TREE 'COMM-IN)
				    conf-stat priority
				    (lyskom-create-text-list comments)
				    (text-stat->text-no text-stat))
	   lyskom-reading-list t))
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
	 ((or (eq type 'RECPT) (eq type 'CC-RECPT))
	  ;; Is this function ever called asynchronously? If not, we
	  ;; can use lyskom-get-membership istead.
	  (let ((membership (lyskom-try-get-membership
			     (misc-info->recipient-no misc-info)))
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


(defun lyskom-text-read-at-least-once-p (text-stat)
  "Return t if TEXT-STAT has been marked as read in any of the recipients
the user is a member of. Uses blocking-do. Returns t if TEXT-STAT is nil."
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
          (cond ((or (eq type 'RECPT) (eq type 'CC-RECPT))
                 (setq membership (lyskom-get-membership
                                   (misc-info->recipient-no misc-item)))
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

(defun lyskom-deferred-insert-footer (conf-stat defer-info)
  "Insert the name of a conference at a previously reserved place."
  (let* ((name (cond (conf-stat (conf-stat->name conf-stat))
                      ((= (defer-info->call-par defer-info) 0)
                       (lyskom-get-string 'person-is-anonymous))
                      (t (lyskom-format 'person-does-not-exist
                                        (defer-info->call-par defer-info)))))
         (dashes (if (> 42 (length name))
                     (make-string (- 42 (length name)) ?-)
                   ""))
         (text (lyskom-format "/%[%#3@%#1P%]/%#2s%#4s"
                              (or conf-stat name)
                              dashes
                              (text-properties-at
                               (defer-info->pos defer-info))
                              (if (defer-info->data defer-info)
                                  (lyskom-get-string
                                   (defer-info->data defer-info))
                                ""))))
    (lyskom-replace-deferred defer-info text)))

(defun lyskom-print-text (text-stat text mark-as-read text-no)
  "Print a text. The header must already be printed.
Print an error message if TEXT-STAT or TEXT is nil.
Mark the text as read if (and only if) MARK-AS-READ is non-nil.
Args: TEXT-STAT TEXT MARK-AS-READ TEXT-NO."
  (let ((lyskom-current-function 'lyskom-print-text))
    (cond
     ((or (null text)
          (null text-stat))
      (lyskom-format-insert 'no-such-text-no text-no)
      (setq lyskom-previous-text lyskom-current-text)
      (setq lyskom-current-text text-no))
     (t
      (let* ((str (text->text-mass text))
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
          (if kom-dashed-lines
              (lyskom-insert 
               "------------------------------------------------------------\n")
            (lyskom-insert "\n"))
          ;; (setq t1 (point-max))
          (let ((lyskom-current-function-phase 'body))
            (lyskom-format-insert "%#1t" body))
          ;; (setq t2 (point-max))
	  )
         (t                             ;No \n found. Don't print header.
          (if kom-dashed-lines
              (lyskom-insert 
               "------------------------------------------------------------\n")
            (lyskom-insert "\n"))
          (lyskom-format-insert "%#1t" str)
          (setq lyskom-current-subject "")))
        (if (lyskom-text-p (cache-get-text (text->text-no text)))
            (cache-del-text (text->text-no text)))
        (sit-for 0)
        (let ((lyskom-current-function-phase 'footer))
          (cond (kom-dashed-lines
                 (lyskom-format-insert "\n(%#1n) " (text->text-no text))
                 (if (not kom-show-author-at-end)
                     (progn
                       (lyskom-format-insert
                        "-----------------------------------%#1s\n"
                        (if lyskom-last-text-format-flags
                            (lyskom-get-string lyskom-last-text-format-flags)
                          "")))
                   (if kom-deferred-printing
                       (progn
                         (lyskom-format-insert "%#1s\n" lyskom-defer-indicator)
                         (lyskom-defer-insertion
                          (lyskom-create-defer-info 
                           'get-conf-stat
                           (text-stat->author text-stat)
                           'lyskom-deferred-insert-footer
                           (set-marker
                            (make-marker)
                             (- (point-max)
                                (length lyskom-defer-indicator)
                                1))
                           (length lyskom-defer-indicator)
                           "%#1s"
                           lyskom-last-text-format-flags)))
                     (let* ((conf-stat (blocking-do 
                                        'get-conf-stat
                                        (text-stat->author text-stat)))
                            (name (lyskom-format "%#1P" conf-stat)))
                       (lyskom-format-insert 
                        "/%#1P/%#2s\n"
                        conf-stat
                        (if (> 42 (length name))
                            (make-string (- 42 (length name)) ?-)
                          ""))))))
                (t (lyskom-format-insert
                    (if kom-show-author-at-end 
                        "\n(%#1n) /%#2P/\n"
                      "\n(%#1n)\n")
                    (text->text-no text)
                    (text-stat->author text-stat)))))
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
			      (t (lyskom-get-string 'Extra-recipient)))
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
            (lyskom-print-header-comm (misc-info->comm-in misc) misc)))
       ((eq type 'FOOTN-IN)
	(lyskom-print-header-comm (misc-info->footn-in misc) misc))))))



(defun lyskom-print-header-comm (text misc)
  "Get author of TEXT-NO and print a header line."
  (let ((text-stat (if kom-deferred-printing
		       (cache-get-text-stat text)
		     (blocking-do 'get-text-stat text))))

    ;; Print information about the link
    (if text-stat
	(progn
	  (lyskom-insert-header-comm text-stat misc))
      (let ((defer-info (lyskom-create-defer-info
			 'get-text-stat
			 text
			 'lyskom-insert-deferred-header-comm
			 (point-max-marker)
			 (length lyskom-defer-indicator)
			 nil		; Filled in later
			 misc)))
	(lyskom-format-insert "%#1s\n" lyskom-defer-indicator)
	(lyskom-defer-insertion defer-info)))

    ;; Print information about who added the link
    (if (misc-info->sent-at misc)
	(lyskom-format-insert 'send-at (lyskom-return-date-and-time 
					(misc-info->sent-at misc))))
    (if (misc-info->sender misc)
	(lyskom-format-insert 'sent-by (misc-info->sender misc)))))


(defun lyskom-insert-deferred-header-comm (text-stat defer-info)
  (let* ((author (if text-stat (text-stat->author text-stat) nil))
	 (misc (defer-info->data defer-info))
	 (type (misc-info->type misc))
	 fmt data)
    (cond
     ((eq type 'COMM-TO)
      (setq fmt (if author 'comment-to-text-by 'comment-to-text)
	    data (misc-info->comm-to misc)))
     ((eq type 'FOOTN-TO)
      (setq fmt (if author 'footnote-to-text-by 'footnote-to-text)
	    data (misc-info->footn-to misc)))
     ((eq type 'COMM-IN)
      (setq fmt (if author 'comment-in-text-by 'comment-in-text)
	    data (misc-info->comm-in misc)))
     ((eq type 'FOOTN-IN)
      (setq fmt (if author 'footnote-in-text-by 'footnote-in-text)
	    data(misc-info->footn-in misc)))) 
    (set-defer-info->format defer-info fmt)
    ; Note: author is ignored if fmt is not *-by
    (lyskom-replace-deferred defer-info data author)))



(defun lyskom-insert-header-comm (text-stat misc)
  "Get author of TEXT-NO and print a header line."
  ;;+++ error kommer att se annorlunda ut.
  (let ((author (if text-stat
		    (text-stat->author text-stat)
		  nil))
	(type (misc-info->type misc)))
    (cond
     ((eq type 'COMM-TO)
      (lyskom-format-insert 'comment-to-text (misc-info->comm-to misc)))
     ((eq type 'FOOTN-TO)
      (lyskom-format-insert 'footnote-to-text (misc-info->footn-to misc)))
     ((eq type 'COMM-IN)
      (lyskom-format-insert 'comment-in-text (misc-info->comm-in misc)))
     ((eq type 'FOOTN-IN)
      (lyskom-format-insert 'footnote-in-text (misc-info->footn-in misc))))
    (if author
	(lyskom-format-insert 'written-by author)
      (lyskom-insert-at-point "\n"))))



;;; Local Variables: 
;;; eval: (put 'lyskom-traverse 'lisp-indent-hook 2)
;;; end: 
