;;;;;
;;;;; $Id: parse.el,v 44.12 1997-10-10 13:12:44 byers Exp $
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
;;;; File: parse.el
;;;;
;;;; This file contains functions which parse replies from the
;;;; server. 
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: parse.el,v 44.12 1997-10-10 13:12:44 byers Exp $\n"))


;;; ================================================================
;;;        Errors that are handled of use while parsing


(put 'lyskom-parse-incomplete 'error-conditions
     '(error lyskom-error lyskom-parse-incomplete))
(put 'lyskom-parse-incomplete 'error-message
     "LysKOM internal error: Parse incomplete.")


;;; ================================================================
;;;                  Low-level parsing.


(defun lyskom-string-skip-whitespace (string)
  "Return STRING omitting any leading whitespace."
  (let ((start (string-match "[^ \t\n\r]" string)))
    (cond ((null start) "")
	  (t (substring string start)))))

(defun lyskom-parse-skip-rest-of-token ()
  "Skip to the next whitespace"
  (let ((c (lyskom-parse-char)))
    (while (not (or (= c ?\ ) (= c ?\n)))
      (setq c (lyskom-parse-char)))))

(defun lyskom-parse-nonwhite-char ()
  "Get next character, skipping whitespace, from lyskom-unparsed-buffer and
increase lyskom-parse-pos. Signal lyskom-parse-incomplete if
the buffer lyskom-unparsed-buffer is exhausted."
  (let ((char (lyskom-parse-char)))
    (while (or (= char ?\ )
               (= char ?\n))
      (setq char (lyskom-parse-char)))
    char))

  
(defun lyskom-parse-char ()
  "Get next character from lyskom-unparsed-buffer and increase 
lyskom-parse-pos. Signal lyskom-parse-incomplete if the buffer 
lyskom-unparsed-buffer is exhausted."
  (cond
   ((< lyskom-parse-pos (point-max))
    (prog1 (char-after lyskom-parse-pos)
      (++ lyskom-parse-pos)))
   (t (signal 'lyskom-parse-incomplete nil))))


(defun lyskom-expect-char (char)
  "Read past next non-white character, which must be equal to CHAR.
Return nil, or signal lyskom-protocol-error if the
first non-white character was not equal to CHAR."
  (if (/= char (lyskom-parse-nonwhite-char))
      (signal 'lyskom-protocol-error
	      (list "Expecting " char " but got "
		    (char-after (1- lyskom-parse-pos))))
    nil))


(defun lyskom-char-p (char)
  "Check if next token is CHAR (a character)."
  (string-match (format "\\`%c[ \t\n\r]" char) (lyskom-string-to-parse)))


(defun lyskom-string-to-parse ()
  "Return unparsed data as a string."
  (lyskom-string-skip-whitespace     
   (buffer-substring lyskom-parse-pos (point-max))))

			  
(defun lyskom-parse-num ()
  "Parse the next token as a number.
Signal lyskom-parse-incomplete if the number is not followed by whitespace.
Signal lyskom-protocol-error if the next token is not a number."
  (goto-char lyskom-parse-pos)

  (cond
   ((looking-at "[ \n]*[0-9]+")
    (if (char-after (match-end 0))
        (progn (setq lyskom-parse-pos (goto-char (match-end 0)))
               (string-to-int (match-string 0)))
      (signal 'lyskom-parse-incomplete nil)))
   ((looking-at "[ \n]*\\'") 
    (goto-char (point-max))
    (signal 'lyskom-parse-incomplete nil))
   (t (signal 'lyskom-protocol-error
              (list "Expected number, got " (lyskom-string-to-parse)))))

)


(defun lyskom-parse-string ()
  "Parse next token as a string.
Signal lyskom-parse-incomplete if the string is not complete.
Signal lyskom-protocol-error if the next token is not a string."
  ;; Kludge to deal with leading spaces.
  (lyskom-parse-nonwhite-char)
  (setq lyskom-parse-pos (1- lyskom-parse-pos))
  ;; End kludge.
  (let ((to-parse (lyskom-string-to-parse)))
    (cond
     ((string-match "\\`[0-9]*\\(\\|H\\)\\'" to-parse)
      (signal 'lyskom-parse-incomplete nil))
     ((null (string-match "\\`[0-9]+H" to-parse))
      (signal 'lyskom-protocol-error (list to-parse))) ;Not a legal string.
     (t
      (let ((end (match-end 0))
	    (len (string-to-int to-parse)))
	(setq lyskom-parse-pos (+ lyskom-parse-pos end))
	(cond
	 ((< (point-max) (+ lyskom-parse-pos len))
	  (setq lyskom-string-bytes-missing
		(- (+ lyskom-parse-pos len)
		   (point-max)))
	  (signal 'lyskom-parse-incomplete nil))
	 (t
	  (prog1 (buffer-substring lyskom-parse-pos 
				   (+ lyskom-parse-pos len))
	    (setq lyskom-parse-pos (+ lyskom-parse-pos len))))))))))


(defun lyskom-parse-1-or-0 ()
  "Parse next nonwhite char and return t if it was 1, nil if it was 0.
Signal lyskom-protocol-error if it was neither 1 nor 0.
Signal lyskom-parse-incomplete if there is no nonwhite char to parse."
  (let ((char (lyskom-parse-nonwhite-char)))
    (cond
     ((= char ?0) nil)
     ((= char ?1) t)
     (t (signal 'lyskom-protocol-error
		(list 'lyskom-parse-1-or-0 char
		      lyskom-parse-pos
		      (buffer-string)))))))


(defun lyskom-parse-bitstring (default)
  "Parse a generic bit string"
  (let ((result nil)
        (char (lyskom-parse-nonwhite-char))
	(continue t))
    (while (and continue default)
      (cond ((eq char ?0) (setq result (cons nil result)
			       default (cdr default)
			       char (lyskom-parse-char)))

            ((eq char ?1) (setq result (cons t result)
			       default (cdr default)
			       char (lyskom-parse-char)))
            ((or (= char ?\ )
                 (= char ?\n))
	     ;; This occurs when the received string is shorter than
	     ;; expected.
             (setq continue nil))

            (t (signal 'lyskom-protocol-error
                       (list 'lyskom-parse-bitstring char
                             lyskom-parse-pos
                             (buffer-string))))))
    (if (not (or (eq char ?\ )
                 (eq char ?\n)))
	;; This occurs when the received string is longer than
	;; expected.
        (progn
          (lyskom-parse-skip-rest-of-token)
          (nreverse result))
      (nconc (nreverse result) (copy-sequence default)))))


(defun lyskom-parse-time ()
  "Parse a time from server. Args: none."
  (lyskom-create-time
   (lyskom-parse-num)			;sec
   (lyskom-parse-num)			;min
   (lyskom-parse-num)			;hour
   (lyskom-parse-num)			;mday
   (lyskom-parse-num)			;mon
   (lyskom-parse-num)			;year
   (lyskom-parse-num)			;wday
   (lyskom-parse-num)			;yday
   (lyskom-parse-num)))			;isdst


;;; ================================================================
;;; Skip tokens. (Used e g to skip unknown asynchronous messages.)


(defun lyskom-skip-tokens (to-skip)
  "Skip next TO-SKIP tokens"
  (while (not (zerop to-skip))
    (lyskom-skip-one-token)
    (-- to-skip)))


(defun lyskom-skip-one-token ()
  (let ((to-parse (lyskom-string-to-parse)))
    (cond
     ((string-match "\\`{" to-parse)	;Array/list?
        (lyskom-skip-array))
     ((string-match "\\`*" to-parse)	;Empty array/list?
        (lyskom-parse-nonwhite-char))	;Simply skip it.
     ((string-match "\\`[0-9]+H" to-parse) ;Hollerith string?
        (lyskom-parse-string))
     ((string-match "\\`[0-9]+[ \t\n\r]" to-parse) ;Number?
        (lyskom-parse-num))
     ((string-match "\\`[0-9]\\'" to-parse)	;Incomplete number?
        (signal 'lyskom-parse-incomplete nil))
     (t (signal 'lyskom-protocol-error (list to-parse))))))


(defun lyskom-skip-array ()
  (let ((to-parse (lyskom-string-to-parse)))
    (cond
     ((string-match "\\`}" to-parse))
     (t (lyskom-skip-one-token)
	(lyskom-skip-array)))))
  

;;; ================================================================
;;; Medium level parsing. Parse arrays, misc-info-lists
;;; and other complex LysKOM types.


(defun lyskom-parse-vector (len parser)
  "Parse a vector with LEN elements.
Each element is parsed by PARSER, a function that takes no arguments."
  (cond
   ((zerop len) (if (lyskom-char-p ?*)
                    (lyskom-expect-char ?*)
                  (lyskom-expect-char ?\{)
                  (lyskom-expect-char ?\})))
   ((lyskom-char-p ?*) (lyskom-expect-char ?*))
   (t (lyskom-expect-char ?{)
      (prog1
	  (lyskom-fill-vector (make-vector len nil) parser)
	(lyskom-expect-char ?})))))


(defun lyskom-fill-vector (vector parser)
  "Fill a vector. Args: VECTOR PARSER.
Fills in all elements in VECTOR. PARSER is called for each element and the
result is assigned to the element."
  (let ((index 0)
	(len (length vector)))
    (while (< index len)
      (aset vector index (funcall parser))
      (setq index (1+ index))))
  vector)


(defun lyskom-parse-conf-type ()
  "Parse a conf-type. No args."
  (apply 'lyskom-create-conf-type (lyskom-parse-bitstring 
                                   '(nil nil nil nil t nil nil nil))))
          
(defun lyskom-parse-privs ()
  "Parse privileges. No args."
  (apply 'lyskom-create-privs
         (lyskom-parse-bitstring
          '(nil nil nil t t t nil nil nil nil nil nil nil nil nil nil))))


(defun lyskom-parse-flags ()
  "Parse Personal_flags. No args."
  (apply 'lyskom-create-flags
         (lyskom-parse-bitstring
          '(nil nil nil nil nil nil nil nil))))

	
(defun lyskom-parse-misc-info-list ()
  "Parse a misc-info-list."
  (let ((n (lyskom-parse-num))		;Number of misc-items to parse.
	(char (lyskom-parse-nonwhite-char)))
    (cond
     ((= char ?*)			;Empty list.
      nil)
     ((= char ?{)			;Start of list.
      (prog1
	  (lyskom-parse-misc-info-list-sub n)
	(lyskom-expect-char ?})))
     (t					;Error.
      (signal 'lyskom-protocol-error (list 'lyskom-parse-misc-info-list
					   "Expected * or {, got " char))))))


(defun lyskom-parse-misc-info-list-sub (n)
  "Parse a misc-info list with N items."
  (let* ((result (list 'dummy))
	 (last result)
	 (next-key (lyskom-parse-num))
	 (res))
    (while (> n 0)
      (cond
       ((eq next-key 0)			;recpt
	(setq res (lyskom-parse-misc-recipient 'RECPT last n)))
       ((eq next-key 1)			;cc-recpt
	(setq res (lyskom-parse-misc-recipient 'CC-RECPT last n)))
       ((eq next-key 2)			;comm-to
	(setq res (lyskom-parse-misc-comm-to last n)))
       ((eq next-key 3)			;comm-in
	(setq res (lyskom-parse-misc-comm-in last n)))
       ((eq next-key 4)			;footn-to
	(setq res (lyskom-parse-misc-footn-to last n)))
       ((eq next-key 5)			;footn-in
	(setq res (lyskom-parse-misc-footn-in last n)))
       ((eq next-key 15)                ;bcc-recpt
        (setq res (lyskom-parse-misc-recipient 'BCC-RECPT last n)))
       (t				;error!
	(signal 'lyskom-protocol-error
		(list 'lyskom-parse-misc-info-list-sub
		      "Unknown misc-type " next-key))))
      (setq n (car res))
      (setq next-key (cdr res))
      (setq last (cdr last)))
    (cdr result)))			;Don't return the dummy element.
      
	      
(defun lyskom-parse-misc-recipient (type last n)
  "Parse a recipient. Args: TYPE LAST N.
TYPE is either RECPT, CC-RECPT or BCC-RECPT.
LAST is a pointer to the last element on a misc-info-list.
N is number of misc-items left to parse.
Returns (cons n next-key)."
  (setcdr last (cons (lyskom-create-empty-misc-info) nil))
  (let ((info (car (cdr last)))
	(next-key nil))
      (set-misc-info->type info type)
    (set-misc-info->recipient-no info (lyskom-parse-num))
    (setq n (1- n))
    ;; A loc-no should follow.
    (if (/= 6 (lyskom-next-num n nil))
	(signal 'lyskom-protocol-error '("No loc-no after recipient.")))
    (set-misc-info->local-no info (lyskom-parse-num))
    (setq n (1- n))
    ;; A rec-time might follow.
    (if (= 7 (setq next-key (lyskom-next-num n nil)))
	(progn
	  (set-misc-info->rec-time info (lyskom-parse-time))
	  (setq n (1- n))
	  (setq next-key nil)))
    ;; A sent-by might follow.
    (if (= 8 (setq next-key (lyskom-next-num n next-key)))
	(progn
	  (set-misc-info->sender info (lyskom-parse-num))
	  (setq n (1- n))
	  (setq next-key nil)))
    ;; A sent-at might follow.
    (if (= 9 (setq next-key (lyskom-next-num n next-key)))
	(progn
	  (set-misc-info->sent-at info (lyskom-parse-time))
	  (setq n (1- n))
	  (setq next-key nil)))
    ;; Return n and next-key.
    (cons n (lyskom-next-num n next-key))))


(defun lyskom-parse-misc-comm-to (last n)
  "Parse a comm-to. Args: LAST N.
LAST is a pointer to the last element on a misc-info-list.
N is number of misc-items left to parse.
Returns (cons n next-key)."
  (setcdr last (cons (lyskom-create-empty-misc-info) nil))
  (let ((info (car (cdr last)))
	(next-key nil))
    (set-misc-info->type info 'COMM-TO)
    (set-misc-info->comm-to info (lyskom-parse-num))
    (setq n (1- n))
    ;; A sent-by might follow.
    (if (= 8 (setq next-key (lyskom-next-num n nil)))
	(progn
	  (set-misc-info->sender info (lyskom-parse-num))
	  (setq n (1- n))
	  (setq next-key nil)))
    ;; A sent-at might follow.
    (if (= 9 (setq next-key (lyskom-next-num n next-key)))
	(progn
	  (set-misc-info->sent-at info (lyskom-parse-time))
	  (setq n (1- n))
	  (setq next-key nil)))
    ;; Return n and next-key.
    (cons n (lyskom-next-num n next-key))))


(defun lyskom-parse-misc-footn-to (last n)
  "Parse a footn-to. Args: LAST N.
LAST is a pointer to the last element on a misc-info-list.
N is number of misc-items left to parse.
Returns (cons n next-key)."
  (setcdr last (cons (lyskom-create-empty-misc-info) nil))
  (let ((info (car (cdr last)))
	(next-key nil))
    (set-misc-info->type info 'FOOTN-TO)
    (set-misc-info->footn-to info (lyskom-parse-num))
    (setq n (1- n))
    ;; A sent-at might follow.
    (if (= 9 (setq next-key (lyskom-next-num n nil)))
	(progn
	  (set-misc-info->sent-at info (lyskom-parse-time))
	  (setq n (1- n))
	  (setq next-key nil)))
    ;; Return n and next-key.
    (cons n (lyskom-next-num n next-key))))


(defun lyskom-parse-misc-comm-in (last n)
  "Parse a comm-in. Args: LAST N.
LAST is a pointer to the last element on a misc-info-list.
N is number of misc-items left to parse.
Returns (cons n next-key)."
  (setcdr last (cons (lyskom-create-empty-misc-info) nil))
  (let ((info (car (cdr last))))
    (set-misc-info->type info 'COMM-IN)
    (set-misc-info->comm-in info (lyskom-parse-num))
    (setq n (1- n))
    ;; Return n and next-key.
    (cons n (lyskom-next-num n nil))))


(defun lyskom-parse-misc-footn-in (last n)
  "Parse a footn-in. Args: LAST N.
LAST is a pointer to the last element on a misc-info-list.
N is number of misc-items left to parse.
Returns (cons n next-key)."
  (setcdr last (cons (lyskom-create-empty-misc-info) nil))
  (let ((info (car (cdr last))))
    (set-misc-info->type info 'FOOTN-IN)
    (set-misc-info->footn-in info (lyskom-parse-num))
    (setq n (1- n))
    ;; Return n and next-key.
    (cons n (lyskom-next-num n nil))))

    
(defun lyskom-next-num (items-to-parse pre-fetched)
  "Parse next number if PRE-FETCHED is nil and ITEMS-TO-PARSE is greater
than 0. Args: ITEMS-TO-PARSE PRE-FETCHED. Returns -1 if ITEMS-TO-PARSE is
0."
  (cond
   ((zerop items-to-parse) -1)
   (pre-fetched)
   (t (lyskom-parse-num))))


;;; ================================================================
;;;         High level parsing. Parsing of entire datatypes.


(defun lyskom-parse-number-array ()
  "Parse an array of integers."
  (lyskom-parse-vector (lyskom-parse-num) 'lyskom-parse-num))


(defun lyskom-parse-membership ()
  "Parse a membership."
  (lyskom-create-membership
   (lyskom-parse-time)			;last-time-read
   (lyskom-parse-num)			;conf-no
   (lyskom-parse-num)			;priority
   (lyskom-parse-num)			;last-text-read
   (lyskom-parse-vector			;read-texts
    (lyskom-parse-num) 'lyskom-parse-num)))		


(defun lyskom-parse-version-info ()
  "Parse info about the server and protocol."
  (lyskom-create-version-info
   (lyskom-parse-num)
   (lyskom-parse-string)
   (lyskom-parse-string)))

(defun lyskom-parse-server-info ()
  "Parse info about the server."
  (lyskom-create-server-info
   (lyskom-parse-num)
   (lyskom-parse-num)
   (lyskom-parse-num)
   (lyskom-parse-num)
   (lyskom-parse-num)
   (lyskom-parse-num)))


(defun lyskom-parse-map ()
  "Parse a text-list (also known as map)."
  (lyskom-create-map
   (lyskom-parse-num)			;first-local
   (lyskom-parse-vector			;text-nos
    (lyskom-parse-num) 'lyskom-parse-num)))


(defun lyskom-parse-who-info ()
  "Parse a who-info."
  (lyskom-create-who-info
   (lyskom-parse-num)			;pers-no
   (lyskom-parse-num)			;working-conf
   (lyskom-parse-num)			;connection
   (lyskom-parse-string)		;doing-what
   (lyskom-parse-string)))		;userid@host

(defun lyskom-parse-session-info ()
  "Parse a session-info."
  (lyskom-create-session-info
   (lyskom-parse-num)			;pers-no
   (lyskom-parse-num)			;working-conf
   (lyskom-parse-num)			;connection
   (lyskom-parse-string)		;doing
   (lyskom-parse-string)		;userid@host
   (lyskom-parse-num)			;idletime
   (lyskom-parse-time)))		;connect-time


;; prot-A.txt says that this should allow more or less flags than
;; specified, but I can't figure out how. /davidk

(defun lyskom-parse-session-flags ()
  "Parse session-flags."
  (apply 'lyskom-create-session-flags
         (lyskom-parse-bitstring
          '(nil nil nil nil nil nil nil nil))))

	
(defun lyskom-parse-dynamic-session-info ()
  "Parse a dynamic-session-info."
  (lyskom-create-dynamic-session-info
   (lyskom-parse-num)			;session-no
   (lyskom-parse-num)			;pers-no
   (lyskom-parse-num)			;working-conf
   (lyskom-parse-num)			;idle-time
   (lyskom-parse-session-flags)		;session-flags
   (lyskom-parse-string)))		;doing


;;; High level parsing. Parsing of complete replies.


(defun lyskom-parse-void ()
  "Parse result from functions that only return an OK/FAILURE."
  t)					;Needn't do anything.
			  

(defun lyskom-parse-conf-list ()
  "Parse result from functions that return a conf-list."
  (let* ((list-len (lyskom-parse-num)))
    (lyskom-create-conf-list
     (lyskom-parse-vector list-len 'lyskom-parse-num)
     (lyskom-parse-vector list-len 'lyskom-parse-conf-type))))


(defun lyskom-parse-conf-no-list ()
  "Parse result from functions that return a conf-no-list."
  (lyskom-create-conf-no-list
   (lyskom-parse-vector (lyskom-parse-num) 'lyskom-parse-num)))


(defun lyskom-parse-mark-list ()
  "Parser result from functions that returns a mark-list."
  (lyskom-parse-vector (lyskom-parse-num) 'lyskom-parse-mark))


(defun lyskom-parse-mark ()
  "Parse a marked text."
  (lyskom-create-mark
   (lyskom-parse-num)			;Text-no
   (lyskom-parse-num)))			;Mark-type
  

;;;================================================================
;;; Parsing of datatypes with cache


(defun lyskom-parse-static-session-info (session)
  "Parse a static-session-info and add it to the cache."
  (let ((info (lyskom-create-static-session-info
	       (lyskom-parse-string)	;username
	       (lyskom-parse-string)	;hostname
	       (lyskom-parse-string)	;ident-user
	       (lyskom-parse-time))))	;connection-time
    (lyskom-save-excursion
    	(set-buffer lyskom-buffer)
    	(cache-add-static-session-info session info))
    info))
    

(defun lyskom-parse-conf-stat (conf-no)
  "Parse a conf-stat, add add it in the cache.
Retuns the conf-stat. Args: CONF-NO."
  (let
      ((conf-stat (lyskom-create-conf-stat
		   conf-no		;conf-no (supplied by
					; initiate-get-conf-stat)
		   (lyskom-parse-string) ;name
		   (lyskom-parse-conf-type) ;conf-type
		   (lyskom-parse-time)	;creation-time
		   (lyskom-parse-time)	;last-written
		   (lyskom-parse-num)	;creator
		   (lyskom-parse-num)	;presentation
		   (lyskom-parse-num)	;supervisor
		   (lyskom-parse-num)	;permitted-submitters
		   (lyskom-parse-num)	;super-conf
		   (lyskom-parse-num)	;msg-of-day
		   (lyskom-parse-num)	;garb-nice
		   (lyskom-parse-num)	;no-of-members
		   (lyskom-parse-num)	;first-local-no
		   (lyskom-parse-num)))) ;no-of-texts
    
    (lyskom-save-excursion
     (set-buffer lyskom-buffer)
     (cache-add-conf-stat conf-stat))
    conf-stat))



(defun lyskom-parse-uconf-stat (conf-no)
  "Parse a uconf-stat, and add it to the cache.
Returns the conf-stat. Args CONF-NO."
  (let ((conf-stat (lyskom-create-uconf-stat conf-no
					     (lyskom-parse-string)
					     (lyskom-parse-conf-type)
					     (lyskom-parse-num)
					     (lyskom-parse-num))))
    (lyskom-save-excursion
     (set-buffer lyskom-buffer)
     (cache-add-uconf-stat conf-stat))
    conf-stat))


(defun lyskom-parse-pers-stat (pers-no)
  "Parse a pers-stat, add add it in the cache.
Retuns the pers-stat. Args: PERS-NO."
  (let
      ((pers-stat
	(lyskom-create-pers-stat
	 pers-no			;pers-no
	 (lyskom-parse-string)		;username
	 (lyskom-parse-privs)		;privileges
	 (lyskom-parse-flags)		;flags
	 (lyskom-parse-time)		;last-login
	 (lyskom-parse-num)		;user-area
	 (lyskom-parse-num)		;total-time-present
	 (lyskom-parse-num)		;sessions
	 (lyskom-parse-num)		;created-lines
	 (lyskom-parse-num)		;created-bytes
	 (lyskom-parse-num)		;read-texts
	 (lyskom-parse-num)		;no-of-text-fetches
	 (lyskom-parse-num)		;created-persons
	 (lyskom-parse-num)		;created-confs
	 (lyskom-parse-num)		;first-created-text
	 (lyskom-parse-num)		;no-of-created-texts
	 (lyskom-parse-num)		;no-of-marks
	 (lyskom-parse-num))))		;no-of-confs
    (lyskom-save-excursion
     (set-buffer lyskom-buffer)
     (cache-add-pers-stat pers-stat))
    pers-stat))


(defun lyskom-parse-text-stat (text-no)
  "Parse a text-stat and add it in the cache. 
Args: TEXT-NO. Value: text-stat."
  (let
      ((text-stat (lyskom-create-text-stat
		   text-no
		   (lyskom-parse-time)	;creation-time
		   (lyskom-parse-num)	;author
		   (lyskom-parse-num)	;no-of-lines
		   (lyskom-parse-num)	;no-of-chars
		   (lyskom-parse-num)	;no-of-marks
		   (lyskom-parse-misc-info-list)))) ;misc-info-list
    (lyskom-save-excursion
     (set-buffer lyskom-buffer)
     (cache-add-text-stat text-stat))
    text-stat))


(defun lyskom-parse-text (text-no)
  "Parse a text and add it to the cache.
  Args: TEXT-NO. Result: text-stat."
  (let ((text (lyskom-create-text
	       text-no
	       (lyskom-parse-string))))		;The text.
    (lyskom-save-excursion
     (set-buffer lyskom-buffer)
     (cache-add-text text))
    text))


(defun lyskom-parse-conf-z-info-list ()
  "Parse result from functions that return a conf-z-info-list."
  (let* ((list-len (lyskom-parse-num)))
    (lyskom-create-conf-z-info-list
     (lyskom-parse-vector list-len 'lyskom-parse-conf-z-info))))

(defun lyskom-parse-conf-z-info ()
  "Parse a conf-z-info."
  (lyskom-create-conf-z-info
   (lyskom-parse-string)		;name
   (lyskom-parse-conf-type)		;conf-type
   (lyskom-parse-num)))			;conf-no



;;; ================================================================
;;;          Parsing of complex datatypes without cache.


(defun lyskom-parse-membership-list ()
  "Parse a membership-list. Returns a vector."
  (lyskom-parse-vector (lyskom-parse-num) 'lyskom-parse-membership))


(defun lyskom-parse-who-info-list ()
  "Parse a who-info-list. Returns a vector."
  (lyskom-parse-vector (lyskom-parse-num) 'lyskom-parse-who-info))
  

(defun lyskom-parse-dynamic-session-info-list ()
  "Parse a who-info-list. Returns a vector."
  (lyskom-parse-vector (lyskom-parse-num) 'lyskom-parse-dynamic-session-info))
  

(defun lyskom-init-parse (buffer)
  "Does all initialization of the parsing routines.
i.e creates the buffer, sets all markers and pointers."
  (setq lyskom-is-parsing nil)
  (setq lyskom-unparsed-buffer 
	(lyskom-generate-new-buffer 
	 (concat (if lyskom-debug-communications-to-buffer "" " ")
		 (buffer-name)
		 "-replies")))
  (setq lyskom-unparsed-marker 
	(lyskom-save-excursion
	 (let ((proc lyskom-proc))
	   (set-buffer lyskom-unparsed-buffer)
 	   (make-local-variable 'lyskom-proc)
	   (make-local-variable 'lyskom-string-bytes-missing)
	   (setq lyskom-proc proc)
	   (make-local-variable 'lyskom-buffer)
	   (setq lyskom-buffer buffer)
	   (goto-char (point-max))
	   (point-marker)))))


;;; ================================================================
;;;        Functions that call the other parsing functions. 


(defun lyskom-parse-success (ref-no buffer)
  "Parse the results of a successful call and call the handler."
  (lyskom-save-excursion
   (set-buffer buffer)
   (let* ((kom-queue (cdr (assq ref-no lyskom-pending-calls)))
	  (call-info (lyskom-locate-ref-no kom-queue ref-no)))
     (set-buffer lyskom-unparsed-buffer)
     (if call-info
	 (apply-parser call-info))
     (set-buffer buffer)
     (lyskom-decrease-pending-calls)
     (setq lyskom-pending-calls
	   (lyskom-assoc-dremove ref-no lyskom-pending-calls))
     (lyskom-check-call kom-queue))))


(defun lyskom-locate-ref-no (kom-queue ref-no)
  (let ((pending (lyskom-queue->all-entries
		  (kom-queue->pending
		   (cdr (assoc kom-queue lyskom-call-data)))))
	(result nil))
    (while (and (null result)
		(not (null pending)))
      (cond
       ((and (eq (car (car pending)) 'CALL)
	     (eq (car (cdr (car pending))) ref-no))
	(setq result (car pending)))
       (t (setq pending (cdr pending)))))
    result))

     
(defun lyskom-assoc-dremove (elt list)
  "Args: ELT LIST. Return a copy of LIST, but exclude any elements whose
car is equal to ELT."
  (let* ((head (cons nil list))
	 (tail head))
    (while (not (null (cdr tail)))
      (cond
       ((equal elt (car (car (cdr tail))))
	(setcdr tail (cdr (cdr tail))))
       (t
	(setq tail (cdr tail)))))
    (cdr head)))


(defun apply-parser (call-info)
  "Try to parse a reply from the server.
CALL-INFO looks like this: (See lyskom-call-data). 
	('CALL REF-NO PARSER PARSER-DATA HANDLER HANDLER-DATA)
PARSER is called, and if it succeeds
CALL-INFO is destructively changed to
	('PARSED RESULT HANDLER HANDLER-DATA)"
  (let ((result
	 (apply (car (cdr (cdr call-info)))	     ;Parser
		(car (cdr (cdr (cdr call-info))))))) ;Parser-data
    ;; If some part of the reply has not yet arrived
    ;; the parser will signal lyskom-parse-incomplete
    ;; and execution will not continue here.
    ;; The parse is complete. Change the call-info.
    (lyskom-tr-call-to-parsed call-info result)))


(defun lyskom-tr-call-to-parsed (call-info result)
  "Transform a CALL to a PARSED."
  (setcar call-info 'PARSED)
  (setcar (cdr call-info) result)
  (setcdr (cdr call-info) (cdr (cdr (cdr (cdr call-info))))))


(defun lyskom-parse-error (ref-no buffer)
  "Parse the result of an unsuccessful call and call the handler."
  (lyskom-save-excursion
    (set-buffer buffer)
    (let* ((kom-queue (cdr (assq ref-no lyskom-pending-calls)))
	   (call-info (lyskom-locate-ref-no kom-queue ref-no))
	   errno)
      (set-buffer lyskom-unparsed-buffer)
      (setq errno (lyskom-parse-num))
      (lyskom-parse-num)		;Skip ref_no.
      (set-buffer buffer)
      (setq lyskom-errno errno)
      (setq lyskom-pending-calls
	    (lyskom-assoc-dremove ref-no lyskom-pending-calls))
      (lyskom-decrease-pending-calls)
      (if call-info
	  (lyskom-tr-call-to-parsed call-info nil)
	(lyskom-message "Bug i lyskom-parse-error"))
      (lyskom-check-call kom-queue))))


(defun lyskom-parse-unparsed ()
  "Parse all complete replies in lyskom-unparsed-buffer.
All parsing is to take place in this buffer but calling the functions:
lyskom-parse-success, lyskom-parse-error and lyskom-parse-async calls 
functions and variables that are connected with the lyskom-buffer."
  (let ((lyskom-buffer (current-buffer))
	(match-data (match-data)))
    ;; Was the server saving?
    (if lyskom-is-saving
	(progn
	  (setq mode-line-process (lyskom-get-string
				   (if lyskom-executing-command
				       'mode-line-working
				     'mode-line-waiting))
		lyskom-is-saving nil)
	  ;; Removed check for kom-presence-messages
	  (if (and (not (lyskom-is-in-minibuffer)))
	      (message ""))))
    (lyskom-save-excursion
     (set-buffer lyskom-unparsed-buffer)
     (setq lyskom-string-bytes-missing 0)
     (while (not (zerop (1- (point-max)))) ;Parse while replies.
       (let* ((lyskom-parse-pos 1)
	      (key (lyskom-parse-nonwhite-char)))
	 (condition-case err
	     (let ((inhibit-quit t))	; Used to be nil, but that can
					; cause hard-to-repair
					; problems
	       (cond
		((= key ?=)		;The call succeeded.
		 (lyskom-parse-success (lyskom-parse-num) lyskom-buffer))
		((= key ?%)		;The call was not successful.
		 (lyskom-parse-error (lyskom-parse-num) lyskom-buffer))
		((= key ?:)		;An asynchronous message.
		 (lyskom-parse-async (lyskom-parse-num) lyskom-buffer)))
	       (delete-region (point-min) lyskom-parse-pos))
	   ;; One reply is now parsed.
	   (lyskom-protocol-error
	    (delete-region (point-min) (min (point-max) (1+ lyskom-parse-pos)))
	    (signal 'lyskom-protocol-error err)))
	 (goto-char (point-min))
         (if (looking-at "[ \n]+")
             (delete-region (match-beginning 0) (match-end 0)))
	 )))
    (store-match-data match-data)))

