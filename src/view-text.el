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
	      "$Id: view-text.el,v 35.3 1991-09-08 20:03:16 ceder Exp $\n"))


(defun lyskom-view-text (queue text-no &optional mark-as-read
			       follow-comments conf-stat priority
			       build-review-tree)
  "Display text number TEXT-NO and use the queue QUEUE as the
queue to get the text at.
Args: QUEUE TEXT-NO &optional MARK-AS-READ FOLLOW-COMMENTS CONF-STAT 
PRIORITY BUILD-REVIEW-TREE.
If MARK-AS-READ is non-nil the text will be marked as read.
If FOLLOW-COMMENTS is non-nil all comments and footnotes to this text will be
read before the next text. CONF-STAT must be the conference status of the
current conference, and PRIORITY the priority, if FOLLOW-COMMENTS is non-nil.
If BUILD-REVIEW-TREE is non-nil then it fixes a new entry in the 
lyskom-reading-list to read the comments to this."
  (lyskom-collect queue)
  (initiate-get-text-stat queue 'lyskom-view-text-2 text-no
			  queue follow-comments conf-stat priority
			  (not kom-reading-puts-comments-in-pointers-last)
			  build-review-tree)
  (initiate-get-text queue nil text-no)
  (lyskom-use queue 'lyskom-print-text mark-as-read text-no)
  (if kom-reading-puts-comments-in-pointers-last
      (initiate-get-text-stat queue 'lyskom-view-text-handle-saved-comments
			      text-no queue)))


(defun lyskom-view-text-2 (text-stat queue follow-comments conf-stat priority
				     show-comments-first build-review-tree)
  "Ask server about everything that is needed to print text header.
Print header when the server replies. Do nothing if TEXT-STAT is nil.
Args: TEXT-STAT FOLLOW-COMMENTS CONF-STAT PRIORITY SHOW-COMMENTS-FIRST 
BUILD-REVIEW-TREE.
Stops the kom-queue QUEUE during printing. Uses kom-queue header.
Also puts all comments and footnotes to this text on lyskom-reading-list
if FOLLOW-COMMENTS is non-nil.
If SHOW-COMMENTS-FIRST is non-nil then comments are not shown.
If BUILD-REVIEW-TREE is non-nil then a new REVIEW-TREE entry is done and 
entered in the lyskom-reading-list"
  (if text-stat
      (progn
	(lyskom-commence-print-header text-stat queue show-comments-first)
	(if (or follow-comments
		build-review-tree)
	    (lyskom-follow-comments text-stat conf-stat
				    queue priority build-review-tree)))))


(defun lyskom-commence-print-header (text-stat queue show-comments-first)
  "Ask server about everything that is needed to print text header.
Print header when the server replies. Do nothing if TEXT-STAT is nil.
Args: TEXT-STAT.
Stops the kom-queue QUEUE during printing. Uses kom-queue header."
  (lyskom-halt queue)
  (lyskom-insert (format "%d " (text-stat->text-no text-stat)))
  (lyskom-print-date-and-time (text-stat->creation-time text-stat))
  (lyskom-insert (if (= 1 (text-stat->no-of-lines text-stat))
		     (lyskom-get-string 'line)
		   (lyskom-format 'lines 
				  (let ((n (text-stat->no-of-lines text-stat)))
				    (if (= n 0) ; Added to increase
					2 ; compatibility with old KOM. /lw
				      n)))))
  (lyskom-queue-print-name 'header (text-stat->author text-stat) t)
  (lyskom-traverse
   misc
   (text-stat->misc-info-list text-stat)
   (let ((type (misc-info->type misc)))
     (cond
      ((or (eq type 'RECPT)
	   (eq type 'CC-RECPT))
       (initiate-get-conf-stat 'header 'lyskom-print-header-recpt
			       (misc-info->recipient-no misc)
			       misc))
      ((eq type 'COMM-IN)
       (if show-comments-first
	   (initiate-get-text-stat 'header 'lyskom-print-header-comm-1
				   (misc-info->comm-in misc)
				   misc)))
      ((eq type 'FOOTN-IN)
       (if show-comments-first
	   (initiate-get-text-stat 'header 'lyskom-print-header-comm-1
				   (misc-info->footn-in misc)
				   misc)))
      ((eq type 'COMM-TO)
       (initiate-get-text-stat 'header 'lyskom-print-header-comm-1
			       (misc-info->comm-to misc)
			       misc))
      ((eq type 'FOOTN-TO)
       (initiate-get-text-stat 'header 'lyskom-print-header-comm-1
			       (misc-info->footn-to misc)
			       misc))           
      (t
       (lyskom-run 'header 'lyskom-print-header-comm nil misc)))))
  (let ((num-marks (text-stat->no-of-marks text-stat))
	(is-marked-by-me (cache-text-is-marked
			  (text-stat->text-no text-stat))))
    (if (> num-marks 0)
      (lyskom-run 'header 'lyskom-insert 
		  (if is-marked-by-me
		      (if (= num-marks 1)
			  (lyskom-get-string 'marked-by-you)
			(if (= num-marks 2)
			    (lyskom-get-string 'marked-by-you-and-one)
			  (lyskom-format 'marked-by-you-and-several
					 (1- num-marks))))
		    (if (= num-marks 1)
			(lyskom-get-string 'marked-by-one)
		      (lyskom-format 'marked-by-several
				     num-marks))))))
  (lyskom-run 'header 'lyskom-resume queue))


(defun lyskom-follow-comments (text-stat conf-stat queue priority review-tree)
  "Arrange so that all comments and footnotes to this text will be read.
Arguments are: TEXT-STAT CONF-STAT QUEUE PRIORITY REVIEW-TREE
TEXT-STAT is the current text.
CONF-STAT the current conference
PRIORITY the priority of the reading.
If REVIEW-TREE is non-nil then build an entry of type 'REVIEW-TREE in the 
lyskom-reading-list."
  (lyskom-halt queue)


  ;; Follow comments...
  
  (lyskom-collect-ignore-err 'comments)
  (lyskom-traverse
   misc (text-stat->misc-info-list text-stat)
   (cond
    ((eq (misc-info->type misc) 'COMM-IN)
     (initiate-get-text-stat 'comments nil
			     (misc-info->comm-in misc)))))
  (lyskom-list-use 'comments 'lyskom-maybe-follow-comments
		   (if review-tree
		       'REVIEW-TREE
		     'COMM-IN)
		   conf-stat priority text-stat)

  ;; ...but follow footnotes even tighter.
  ;; The foot-notes are read first since their code below is run after the
  ;; code for the ordinary comments. This explains some of the slow speed
  ;; since this requires these lists to have executed through before allowing
  ;; anyone to read on. But it doesn't explain why only footnotes are read
  ;; before the text itself and not all comments. /Linus
  ;; This is now invalid since we use the priority differences to get the 
  ;; right order.

  (lyskom-collect-ignore-err 'comments)
  (lyskom-traverse
   misc (text-stat->misc-info-list text-stat)
   (cond
    ((eq (misc-info->type misc) 'FOOTN-IN)
     (initiate-get-text-stat 'comments nil
			     (misc-info->footn-in misc)))))
  (lyskom-list-use 'comments 'lyskom-maybe-follow-comments
		   (if review-tree
		       'REVIEW-TREE
		     'FOOTN-IN)
		   conf-stat (1+ (or priority 255))
		   text-stat)
  (lyskom-run 'comments 'lyskom-resume queue))


(defun lyskom-maybe-follow-comments (text-stat-list type conf-stat priority
				     &optional root-text-stat)
  "Arrange so that all unread texts in TEXT-STAT-LIST will be read as comments.
TYPE is either 'COMM-IN, 'FOOTN-IN or 'REVIEW-TREE.
Prefetch the names of the authors of these comments.
If the type is 'REVIEW-TREE even read texts in text-stat-list is entered.
A fifth optional argument ROOT-TEXT-STAT is used when building the 
read-info."
  (let ((comments nil)
	(review-tree (eq type 'REVIEW-TREE)))
    (lyskom-traverse
     text-stat text-stat-list
     (cond
      ((or (not (lyskom-text-read-p text-stat))
	   review-tree)
       (setq comments (nconc comments
			     (list (text-stat->text-no text-stat))))
       (initiate-get-conf-stat 'prefetch nil (text-stat->author text-stat)))))
    (cond
     (comments
      (read-list-enter-read-info
       (lyskom-create-read-info type conf-stat priority
				(lyskom-create-text-list comments)
				(text-stat->text-no root-text-stat))
       lyskom-reading-list
       t)))))


; (defun lyskom-text-not-read (text-stat)
;   "Return t if TEXT-STAT has not been marked as read."
;   (let* ((misc-info-list (text-stat->misc-info-list text-stat))
; 	 (i (length misc-info-list))
; 	 (res t))
;     ;; Loop through the misc-infos and return nil as soon as a recipient
;     ;; in which the user has marked the text as read is found.
;     (while (and res (not (zerop i)))
;       (setq i (1- i))
;       (let* ((misc-info (elt misc-info-list i))
; 	     (type (misc-info->type misc-info)))
; 	(cond
; 	 ((or (eq type 'RECPT) (eq type 'CC-RECPT))
; 	  (let ((membership (lyskom-member-p
; 			     (misc-info->recipient-no misc-info)))
; 		(loc-no (misc-info->local-no misc-info)))
; 	    (if (and membership
; 		     (or (<= loc-no (membership->last-text-read membership))
; 			 (lyskom-vmemq loc-no
; 				       (membership->read-texts membership))))
; 		(setq res nil)))))))
;     res))


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


(defun lyskom-print-date-and-time (time)
  "Print date and time. Arg: TIME."
  (lyskom-format-insert 'time-y-m-d-h-m
			(+ (time->year time) 1900)
			(1+ (time->mon  time))
			(time->mday time)
			(time->hour time)
			(time->min  time)))


(defun lyskom-print-text (text-stat text mark-as-read text-no)
  "Print a text. The header must already be printed.
Print an error message if TEXT-STAT or TEXT is nil.
Mark the text as read if (and only if) MARK-AS-READ is non-nil.
If the text is represented by a text type in the cache then this is substituted
by a textpointers type.
Args: TEXT-STAT TEXT MARK-AS-READ TEXT-NO."
  (cond
     ((or (null text)
	  (null text-stat))
      (lyskom-format-insert 'no-such-text-no text-no)
      (setq lyskom-previous-text lyskom-current-text)
      (setq lyskom-current-text text-no))
   (t
      (let* ((str (text->text-mass text))
	     s1 s2 t1 t2)
	(cond
	 ((string-match "\n" str)
	    (setq lyskom-current-subject (substring str 0 (match-beginning 0)))
	    (lyskom-insert-string 'head-Subject)
	    (setq s1 (point-max))
	    (lyskom-insert (concat lyskom-current-subject "\n"))
	    (setq s2 (point-max))
	    (if kom-dashed-lines
		(lyskom-insert 
		 "------------------------------------------------------------\n")
	      (lyskom-insert "\n"))
	    (setq t1 (point-max))
	    (lyskom-insert (substring str (match-end 0)))
	    (setq t2 (point-max)))
	 (t				;No \n found. Don't print header.
	  (setq s1 (point-max))
	  (lyskom-insert str)
	  (setq s2 (point-max))
	  (setq t1 (point-max)
		t2 (point-max))
	  (setq lyskom-current-subject "")))
	(if (lyskom-text-p (cache-get-text (text->text-no text)))
	    (cache-add-text 
	     (lyskom-create-textpointers (text->text-no text)
					 s1 s2 t1 t2)))
	(sit-for 0)
	(lyskom-insert (format
			(if kom-dashed-lines
			    "\n(%d) -----------------------------------\n"
			  "\n(%d)\n")
			(text->text-no text)))
	(if mark-as-read
	    (lyskom-mark-as-read text-stat))
	(setq lyskom-previous-text lyskom-current-text)
	(setq lyskom-current-text (text-stat->text-no text-stat))))))


(defun lyskom-mark-as-read (text-stat)
  "Mark a text as read in all conferences that are recipients.
Tell the server that it is read. This function does not remove
the text from the internal structures of the client."
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
  (lyskom-insert
   (concat (cond ((eq (misc-info->type misc) 'RECPT)
		  (lyskom-get-string 'Recipient))
		 (t (lyskom-get-string 'Extra-recipient)))
	   ": "
	   (conf-stat->name conf-stat)
	   (format " <%d>\n" (misc-info->local-no misc))))
  (if (misc-info->sent-at misc)
      (lyskom-format-insert 'send-at
			    (lyskom-return-time 
			     (misc-info->sent-at misc))))
  (if (misc-info->sender misc)
      (progn
	(lyskom-halt 'header)
	(lyskom-run 'header-2 'lyskom-insert (lyskom-format 'sent-by))
	(lyskom-queue-print-name 'header-2 (misc-info->sender misc) t)
	(lyskom-run 'header-2 'lyskom-resume 'header)))
  (if (misc-info->rec-time misc)
      (lyskom-format-insert 'recieved-at
			    (lyskom-return-time (misc-info->rec-time misc)))))


(defun lyskom-view-text-handle-saved-comments (text-stat queue)
  "Ask server for what is needed to print the references to commenting texts 
and prints the references. Used at the bottom of the text.
Args: TEXT-STAT of the text being read.
      QUEUE: the queue the kom-queue the text was originally viewed through."
  (lyskom-halt queue)
  (lyskom-traverse
   misc
   (text-stat->misc-info-list text-stat)
   (let ((type (misc-info->type misc)))
     (cond
      ((eq type 'COMM-IN)
       (initiate-get-text-stat 'header 'lyskom-print-header-comm-1
			       (misc-info->comm-in misc)
			       misc))
      ((eq type 'FOOTN-IN)
       (initiate-get-text-stat 'header 'lyskom-print-header-comm-1
			       (misc-info->footn-in misc)
			       misc)))))
  (lyskom-run 'header 'lyskom-resume queue))


(defun lyskom-print-header-comm-1 (text-stat misc)
  "Get author of TEXT-STAT and call lyskom-print-header-comm."
  (if text-stat ;+++ error kommer att se annorlunda ut.
      (progn
	(lyskom-halt 'header)
	(initiate-get-conf-stat 'header-2 'lyskom-print-header-comm
				(text-stat->author text-stat)
				misc)
	(lyskom-run 'header-2 'lyskom-resume 'header))
    (lyskom-print-header-comm nil misc)))


(defun lyskom-print-header-comm (author misc)
  "Print a line of info about a comment/footnote to/from this text."
  (let* ((type (misc-info->type misc))
	 (name (and author (conf-stat->name author)))
	 (tyname (if name (lyskom-format 'written-by
					 name)
		   "")))
    (cond
     ((eq type 'COMM-TO)
        (lyskom-insert
	 (lyskom-format 'comment-to-text-by
		 (misc-info->comm-to misc)
		 tyname)))
     ((eq type 'FOOTN-TO)
        (lyskom-insert
	 (lyskom-format 'footnote-to-text-by
			(misc-info->footn-to misc)
			tyname)))
     ((eq type 'COMM-IN)
        (lyskom-insert
       (lyskom-format 'comment-in-text-by
		      (misc-info->comm-in misc)
		      tyname)))
     ((eq type 'FOOTN-IN)
        (lyskom-insert
	 (lyskom-format 'footnote-in-text-by
			(misc-info->footn-in misc)
			tyname)))
     (t (signal 'lyskom-internal-error '(lyskom-print-header-comm))))))
