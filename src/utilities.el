;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: utilities.el,v 44.120 2002-09-15 17:20:40 byers Exp $
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
;;;; File: utilities.el
;;;;
;;;; This file contains general lisp utility functions and
;;;; lyskom-specific utility functions (such as date formatting and
;;;; minibuffer reading)
;;;;


(setq lyskom-clientversion-long
      (concat lyskom-clientversion-long
	      "$Id: utilities.el,v 44.120 2002-09-15 17:20:40 byers Exp $\n"))

;;;
;;; Need Per Abrahamsens widget and custom packages There should be a
;;; better way of doing this, but I'll be darned if I know how. The
;;; various files need to be loaded in a very specific order.
;;;

;;; Define widget wrappers for all the functions in macros.el

;;;
;;; Lisp utility functions
;;;

(defsubst listify-vector (vector)
  "Turn VECTOR into a list"
  (append vector nil))

(defun nfirst (n list)
  "Return a list of the N first elements of LIST."
  (let ((result nil))
    (while (and (> n 0) list)
      (setq result (cons (car list) result)
            list (cdr list)
            n (1- n)))
    (nreverse result)))

(defun lyskom-rotate-list (list el)
  "Destructively rotate LIST so EL becomes the first element.
If EL occurs more than one, the first occurrence is used."
  (let ((tail (memq el list)))
    (if (or (null tail) (eq el (car list)))
        list
      (setcdr (nthcdr (- (length list) (length tail) 1) list) nil)
      (setcdr (nthcdr (1- (length tail)) tail) list)
      tail)))

(defun lyskom-preceding-cons (list el)
  "Return the cons cell of LIST preceding the first cons cell whose car is EL.
Return nil if TAIL is the same as LIST or not a member of LIST."
  (unless (or (eq (car list) el)
              (not (memq el list)))
    (nthcdr (- (length list) (length (memq el list)) 1) list)))

(defun lyskom-insert-in-list (el list before)
  "Destructively insert EL in LIST before element BEFORE.
If BEFORE is not in the list, then insert EL at the end of the list."
  (cond ((eq before (car list))
         (cons el list))
        (t (setcdr (nthcdr (- (length list)
                              (length (memq before list)) 1) list)
                   (cons el (memq before list)))
           list)))

(defun lyskom-move-in-list (el list pos)
  "Destructively move EL within LIST so it appears at position POS."
  (when (memq el list)
    (setq list (delq el list))
    (cond ((eq 0 pos) (setq list (cons el list)))
          (t (setcdr (nthcdr (1- pos) list)
                     (cons el (nthcdr pos list))))))
  list)

;;;
;;; +++ FIXME: If cl.el can be guaranteed, this is pointless.
;;;

(defun lyskom-butlast (x &optional n)
  "Returns a copy of LIST with the last N elements removed."
  (if (and n (<= n 0)) x
    (lyskom-nbutlast (copy-sequence x) n)))

(defun lyskom-nbutlast (x &optional n)
  "Modifies LIST to remove the last N elements."
  (let ((m (length x)))
    (or n (setq n 1))
    (and (< n m)
	 (progn
	   (if (> n 0) (setcdr (nthcdr (- (1- m) n) x) nil))
	   x))))

(defun filter-list (test list)
  (let ((result nil))
    (lyskom-traverse el list
      (when (funcall test el)
        (setq result (cons el result))))
    (nreverse result)))

;;;============================================================
;;;
;;; Utility functions.
;;;
;;; These should be shared in LysKOM
;;;

(eval-and-compile
  (if (eval-when-compile (string-match "XEmacs" emacs-version))
      (defmacro lyskom-compiled-function-p (arg) `(compiled-function-p ,arg))
    (defmacro lyskom-compiled-function-p (arg) `(byte-code-function-p ,arg))))

(lyskom-provide-function copy-tree (l)
  "Recursively copy the list L"
  (cond ((atom l) l)
        (t (cons (copy-tree (car l))
                 (copy-tree (cdr l))))))

(lyskom-provide-function functionp (obj)
  "Returns t if OBJ is a function, nil otherwise."
  (cond
   ((symbolp obj) (fboundp obj))
   ((subrp obj))
   ((lyskom-compiled-function-p obj))
   ((consp obj)
    (if (eq (car obj) 'lambda) (listp (car (cdr obj)))))
   (t nil)))


(defun lyskom-ignore (&rest args)
  "Ignore all arguments"
  )

(defun regexpp (re)
  "Return non-nil if RE looks like a valid regexp."
  (let ((result t))
    (save-match-data
      (condition-case nil
          (string-match re "")
        (error (setq result nil))))
    result))


(defun mapcar2 (fn seq1 seq2)
  (let (result)
    (while (and seq1 seq2)
      (setq result (cons (funcall fn (car seq1) (car seq2)) result))
      (setq seq1 (cdr seq1)
            seq2 (cdr seq2)))
    (nreverse result)))


(defun lyskom-maxint ()
  (let ((n 1) 
        (l nil)
        (i 31))
    (while (and (> n 0) (> i 0))
      (setq l (cons n l))
      (setq n (* 2 n))
      (setq i (1- i)))
    (apply '+ l)))

;; Set lyskom-maxint correctly

(setq lyskom-max-int (lyskom-maxint))


(defun lyskom-try-require (feature &optional message &rest args)
  "Load the feature FEATURE using require. 
If optional MESSAGE is non-nil, use it as a LysKOM format string 
taking one string argument to print an error message. Remaining
arguments are used as arguments for the format string.

Returns t if the feature is loaded or can be loaded, and nil otherwise."
  (or (featurep 'feature)
      (condition-case nil
          (progn (require feature)
                 t)
        (error 
         (when message
           (apply 'lyskom-format-insert-before-prompt message (symbol-name feature) args))
         nil))))
                               
      

(defun lyskom-emacs-version ()
  (cond ((string-match "^XEmacs" (emacs-version)) 'xemacs)
	(t 'emacs)))


(defvar lyskom-apo-timeout 0
  "Current millisecond timeout value for accept-process-output")

(defvar lyskom-apo-timeout-index 0
  "Index in lyskom-apo-timeout-vector-max where last timeout is")

(defconst lyskom-apo-timeout-vector
  [0 1000 1000 2000 3000 5000 8000 13000 21000 34000 55000 89000 144000 233000 377000 610000]
  "Vector of timeout values (usecs) for accept-process-output")

(defconst lyskom-apo-timeout-vector-max (1- (length lyskom-apo-timeout-vector))
  "Maximum index in lyskom-apo-timeout-vector")

(defun lyskom-next-apo-timeout ()
  (if (< lyskom-apo-timeout-index lyskom-apo-timeout-vector-max)
      (setq lyskom-apo-timeout
            (aref lyskom-apo-timeout-vector
                  (setq lyskom-apo-timeout-index
                        (1+ lyskom-apo-timeout-index))))))

(defun lyskom-reset-apo-timeout ()
  (setq lyskom-apo-timeout-index -1)
  (setq lyskom-apo-timeout 0))

(defun lyskom-accept-process-output ()
  "Call accept-process-output with the correct timeout values."
  (lyskom-next-apo-timeout)
  (accept-process-output nil 0 lyskom-apo-timeout))

(defun lyskom-set-alist (alist item value)
  "Modifies (non-destructively) an alist ALIST to set item
ITEM to the value VALUE."
  (let ((pair (assq item alist)))
     (if pair
         (progn (setcdr pair value) alist)
       (cons (cons item value) alist))))

;;;
;;; LysKOM utility functions
;;;

(lyskom-provide-function string-to-sequence (string type)
  "Convert STRING to a sequence of TYPE which contains characters in STRING.
TYPE should be `list' or `vector'."
  (let ((len (length string))
	(i 0)
	val)
    (cond ((eq type 'list)
	   (setq val (make-list len 0))
	   (let ((l val))
	     (while (< i len)
	       (setcar l (aref string i))
	       (setq l (cdr l) i (1+ i)))))
	  ((eq type 'vector)
	   (setq val (make-vector len 0))
	   (while (< i len)
	     (aset val i (aref string i))
	     (setq i (1+ i))))
	  (t
	   (error "Invalid type: %s" type)))
    val))

(lyskom-provide-subst string-to-vector (string)
  "Return a vector of characters in STRING."
  (string-to-sequence string 'vector))

;;;
;;; WARNING!
;;;
;;; The following variable is *important* if you fuck it up in any
;;; way, the functions used to read conference names won't work. So if
;;; you change it, try to work one character at a time, and when
;;; you're done, run through the mappings of all 256 characters to
;;; make sure they look OK.
;;;
;;; Make sure your MULE Emacs doesnt fuck it up for you. It did for me.
;;;

(defvar lyskom-default-collate-table
  "\000\001\002\003\004\005\006\007\010 \012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]~\200\201\202\203\204\205\206\207\210\211\212\213\214\215\216\217\220\221\222\223\224\225\226\227\230\231\232\233\234\235\236\237 !¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿AAAA[]ACEEEEIIIIÐNOOOO\\×OUUUYYÞßAAAA[]ACEEEEIIIIðNOOOO\\÷OUUUYYþÿ"
  "String mapping characters to their collate class.

Lowercase, uppercase and other equivalents are mapped to the same class.
The class number defines a proper sorting order.")


(defsubst lyskom-maybe-recode-string (s &optional coding force)
  "Change the encoding of S for when multibyte characters are not supported.
Optional second argument CODING is the coding system to use. If optional
third argument FORCE is non-nil, always encode multibyte strings, otherwise
only encode when multibyte strings are not supported."
  (if (and (multibyte-string-p s) (or force (not enable-multibyte-characters)))
      (encode-coding-string s (or coding
                                  (and lyskom-language
                                       (lyskom-language-coding lyskom-language))
                                  'raw-text))
    s))

(defun lyskom-recode-string-for-title (s coding)
  "Encode S with CODING for use in frame titles.
Attempts to encode the string only in Emacs versions that do not require
MULE coding for frame titles"
  (cond
   ((memq window-system '(win32 mswindows w32))
    (lyskom-maybe-recode-string s coding t))
   ((> emacs-major-version 20) s)
   (t (lyskom-maybe-recode-string s coding t))))

(defun lyskom-maybe-frob-completion-table (table &optional copy)
  "Recode the cars of alist TABLE to default coding system unless multibyte
characters are enabled. This function is destructive unless optional copy
is non-nil."
  (cond (enable-multibyte-characters table)
	(copy (mapcar (lambda (el)
			(cons (lyskom-maybe-recode-string (car el) nil t)
			      (cdr el)))
                      table))
	(t (lyskom-traverse el table
		(setcar el (lyskom-maybe-recode-string (car el) nil t)))
	   table)))


(defsubst lyskom-unicase-char (c)
  "Smash case and diacritical marks on c." 
  (if (< (char-to-int c) (length lyskom-collate-table))
      (aref lyskom-collate-table (char-to-int c))
    (setq c (lyskom-encode-coding-char c lyskom-server-coding-system))
    (if (and c (< (char-to-int c) (length lyskom-collate-table)))
	(aref lyskom-collate-table (char-to-int c))
      c)))

(defun lyskom-unicase (s)
  "Smash case and diacritical marks of all chars in s." 
  (lyskom-save-excursion
   (and lyskom-buffer (set-buffer lyskom-buffer))
   (let* ((s2 (string-to-vector 
               (encode-coding-string s lyskom-server-coding-system)))
          (l (length s2)))
     (while (> l 0)
       (setq l (1- l))
       (aset s2 l (lyskom-unicase-char (aref s2 l))))
     (concat s2))))

(defun lyskom-looking-at (s)
  "Version of looking-at that will work in Gnu Emacs 20.3"
  (save-excursion
    (let ((start (point)))
      (and (re-search-forward s nil t)
	   (eq (match-beginning 0) start)))))

;; Don't slow things down with our own looking-at unless we have to

(eval-and-compile
 (condition-case nil
     (if (and (eq emacs-major-version 20)
	      (eq emacs-minor-version 3))
	 nil
       (fset 'lyskom-looking-at (symbol-function 'looking-at)))
   (error nil)))


(defun lyskom-compute-char-classes (map)
  "Find out which characters that are equivalent according to MAP.

MAP should be a collate table.

The return value is an assoc list, which characters as keys, and a
list of single-character strings as values.  Only characters that are
equivalent to at least one more character is returned in the assoc
list.

Example: if this function returns

	((97 \"A\" \"a\")
         (65 \"A\" \"a\")
	 (48 \"O\" \"o\" \"0\")
	 (111 \"O\" \"o\" \"0\")
	 (79 \"O\" \"o\" \"0\"))

it means that a and A are equivalent, and o, O and 0 are equivalent.
All other characters are unique."

  (lyskom-save-excursion
    (and lyskom-buffer (set-buffer lyskom-buffer))
    (let ((ix (length map))
	  (cls-to-strings nil)) ;assoc-list from equivalence class to
				;list of chars
      (while (> ix 0)
	(setq ix (1- ix))
	(let* ((cls (aref map ix))
	       (str (decode-coding-string (concat (vector ix))
					  lyskom-server-coding-system))
	       (elem (assoc cls cls-to-strings)))
	  (if elem
	      (rplacd elem (cons str (cdr elem)))
	    (setq cls-to-strings (cons (list cls str) cls-to-strings)))))
      (let ((res nil))
	(while cls-to-strings
	  (let ((lst (cdr (car cls-to-strings))))
	    (if (> (length lst) 1)
		(while lst
		  (setq res (cons (cons (string-to-char (car lst))
					(cdr (car cls-to-strings)))
				  res))
		  (setq lst (cdr lst)))))
	  (setq cls-to-strings (cdr cls-to-strings)))
	res))))
    
;; Stolen from thingatpt.el
;; FIXME: We may not really need this function. Check the callers.

(defun lyskom-thing-at-point-looking-at (regexp)
  "Return non-nil if point is in or just after a match for REGEXP.
Set the match data from the earliest such match ending at or after
point."
  (save-excursion
    (let ((old-point (point)) match)
      (and (looking-at regexp)
	   (>= (match-end 0) old-point)
	   (setq match (point)))
      ;; Search back repeatedly from end of next match.
      ;; This may fail if next match ends before this match does.
      (re-search-forward regexp nil 'limit)
      (while (and (re-search-backward regexp nil t)
		  (or (> (match-beginning 0) old-point)
		      (and (looking-at regexp) ; Extend match-end past search start
			   (>= (match-end 0) old-point)
			   (setq match (point))))))
      (if (not match) nil
	(goto-char match)
	;; Back up a char at a time in case search skipped
	;; intermediate match straddling search start pos.
	(while (and (not (bobp))
		    (progn (backward-char 1) (looking-at regexp))
		    (>= (match-end 0) old-point)
		    (setq match (point))))
	(goto-char match)
	(looking-at regexp)))))


;; Stolen from Gnu Emacs

(defun lyskom-truncate-string-to-width (str end-column &optional start-column padding)
  "Truncate string STR to end at column END-COLUMN.
The optional 2nd arg START-COLUMN, if non-nil, specifies
the starting column; that means to return the characters occupying
columns START-COLUMN ... END-COLUMN of STR.

The optional 3rd arg PADDING, if non-nil, specifies a padding character
to add at the end of the result if STR doesn't reach column END-COLUMN,
or if END-COLUMN comes in the middle of a character in STR.
PADDING is also added at the beginning of the result
if column START-COLUMN appears in the middle of a character in STR.

If PADDING is nil, no padding is added in these cases, so
the resulting string may be narrower than END-COLUMN."
  (or start-column
      (setq start-column 0))
  (let ((len (length str))
	(idx 0)
	(column 0)
	(head-padding "") (tail-padding "")
	ch last-column last-idx from-idx)
    (condition-case nil
	(while (< column start-column)
	  (setq ch (aref str idx)
		column (+ column (char-width ch))
		idx (1+ idx)))
      (args-out-of-range (setq idx len)))
    (if (< column start-column)
	(if padding (make-string end-column padding) "")
      (if (and padding (> column start-column))
	  (setq head-padding (make-string (- column start-column) padding)))
      (setq from-idx idx)
      (if (< end-column column)
	  (setq idx from-idx)
	(condition-case nil
	    (while (< column end-column)
	      (setq last-column column
		    last-idx idx
		    ch (aref str idx)
		    column (+ column (char-width ch))
		    idx (1+ idx)))
	  (args-out-of-range (setq idx len)))
	(if (> column end-column)
	    (setq column last-column idx last-idx))
	(if (and padding (< column end-column))
	    (setq tail-padding (make-string (- end-column column) padding))))
      (setq str (substring str from-idx idx))
      (if padding
	  (concat head-padding str tail-padding)
	str))))


(eval-and-compile
  (lyskom-xemacs-or-gnu
   (fset 'lyskom-string= (symbol-function 'string=))
   (if (< emacs-major-version 20)
       (fset 'lyskom-string= (symbol-function 'string=))
     (defun lyskom-string= (s1 s2)
       (string= (and s1 (if (multibyte-string-p s1)
		    s1
		  (decode-coding-string s1 (lyskom-language-coding
					    lyskom-language))))
		(and s2 (if (multibyte-string-p s2)
		    s2
		  (decode-coding-string s2 (lyskom-language-coding
					    lyskom-language)))))))))

(defun lyskom-string-assoc (key list)
  "Return non-nil if KEY is the same string as the car of an element of LIST.
The value is actually the element of LIST whose car equals KEY."
  (let ((s (and key (downcase key)))
        (result nil))
    (while list
      (when (lyskom-string= s (downcase (car (car list))))
        (setq result (car list))
        (setq list nil))
      (setq list (cdr list)))
    result))

(defun lyskom-string-member (key list)
  "Return non-nil if KEY is the same string as the car of an element of LIST.
The value is actually the element of LIST whose car equals KEY."
  (let ((s (and key (downcase key)))
        (result nil))
    (while list
      (when (lyskom-string= s (downcase (car list)))
        (setq result (car list))
        (setq list nil))
      (setq list (cdr list)))
    result))

(defun lyskom-string-rassoc (key list)
  "Return non-nil if KEY is the same string as the cdr of an element of LIST.
The value is actually the element of LIST whose car equals KEY."
  (let ((s (and key (downcase key)))
        (result nil))
    (while list
      (when (lyskom-string= s (downcase (cdr (car list))))
        (setq result (car list))
        (setq list nil))
      (setq list (cdr list)))
    result))

(defun lyskom-set-default (sym val)
  "Set the value of SYM in the LysKOM buffer to VAL."
  (save-excursion
    (set-buffer (or (and (boundp 'lyskom-buffer)
                         (bufferp lyskom-buffer)
                         (buffer-live-p lyskom-buffer)
                         lyskom-buffer)
                    (current-buffer)))
    (set sym val)))

(defun lyskom-default-value (sym)
  "Get the value of SYM in the LysKOM buffer"
  (save-excursion
    (set-buffer (or (and (boundp 'lyskom-buffer)
                         (bufferp lyskom-buffer)
                         (buffer-live-p lyskom-buffer)
                         lyskom-buffer)
                    (current-buffer)))
    (symbol-value sym)))

(defun lyskom-default-value-safe (sym)
  "Get the value of SYM in the LysKOM buffer"
  (save-excursion
    (set-buffer (or (and (boundp 'lyskom-buffer)
                         (bufferp lyskom-buffer)
                         (buffer-live-p lyskom-buffer)
                         lyskom-buffer)
                    (current-buffer)))
    (and (boundp sym) (symbol-value sym))))


;;; ============================================================
;;; Prefix arguments

(defun lyskom-get-ancestors-of-text (text-no level)
  "Returns a list of all ancestors of TEXT-NO that are LEVEL
comment/foot-note levels up the comment tree. LEVEL is a
non-negative integer and 0 means the given text-no."
  (if (< level 1)
      (list text-no)
    (let* ((text-stat (blocking-do 'get-text-stat text-no))
	   (ancestors (and text-stat
			   (lyskom-text-stat-commented-texts text-stat)))
	   (level (1- level))
	   (result '()))
	(while ancestors
	  (setq result
		(lyskom-union
		 (lyskom-get-ancestors-of-text (car ancestors) level)
		 result
                 ))
	  (setq ancestors (cdr ancestors)))
	result)))

(defun lyskom-get-explicit-text (arg &optional prompt default constraint) arg)

;; NOTUSED: lyskom-get-command-specified-default-text
(defun lyskom-get-command-specified-default-text (a p def &optional c) def)

(defun lyskom-get-last-read-text (&optional arg prompt default constraint)
  (lyskom-default-value 'lyskom-current-text))

(defun lyskom-get-previous-text (&optional arg prompt default constraint)
  lyskom-previous-text)

(defun lyskom-get-text-at-point (&optional arg prompt default constraint)
  (or (lyskom-text-at-point)
       (lyskom-get-string 'no-text-at-point)))

(defun lyskom-get-text-at-point-ancestor (arg &optional p d c)
  (let* ((text (lyskom-text-at-point))
	 (cnos (and text (lyskom-get-ancestors-of-text text arg)))
	 (txts (length cnos)))
    (cond
     ((not text)
      (lyskom-get-string 'no-text-at-point))
     ((eq txts 0)
      (lyskom-get-string 'no-comment-to))
     ((eq txts 1)
      (car cnos))
     (t
      (lyskom-read-number (lyskom-get-string 'what-ancestor) (car cnos))))))

(defun lyskom-prompt-for-text-no (arg prompt default &optional constraint)
  (lyskom-read-number prompt default))

(defun lyskom-get-last-text-written-by-me (&optional a p d c)
  (lyskom-default-value 'lyskom-last-written))

(defun lyskom-get-last-written-or-read-by-me (&optional a p d c)
  (lyskom-default-value 'lyskom-last-seen-written))

(defun lyskom-get-text-above-point (arg &optional prompt default constraint)
  (save-excursion
    (let ((former-point (point)))
      (backward-text (+ 1 arg))
      (if (looking-at "\\([0-9]+\\)\\s-")
	  (string-to-int (match-string 1))
	(progn ;; we probably ended up above the first message in the buffer
	  (forward-text)
	  (if (and (< (point) former-point)
		   (looking-at "\\([0-9]+\\)\\s-"))
	      (string-to-int (match-string 1))
	    (lyskom-error (lyskom-get-string 'bad-text-no-prefix) arg)))))))

(defun lyskom-get-text-below-point (arg &optional prompt default constraint)
  (save-excursion
    (let ((former-point (point)))
      (forward-text arg)
      (if (looking-at "\\([0-9]+\\)\\s-")
	  (string-to-int (match-string 1))
	(progn ;; we probably ended up below the final message in the buffer
	  (backward-text)
	  (if (and (> (point) former-point)
		   (looking-at "\\([0-9]+\\)\\s-"))
	      (string-to-int (match-string 1))
	    (lyskom-error (lyskom-get-string 'bad-text-no-prefix) arg)))))))

(defun lyskom-read-text-no-prompt-p (command)
  "Return non-nil if the COMMAND should prompt for a text number."
  (let ((check (assq command kom-text-no-prompts)))
    (if check (cdr check) (memq command lyskom-text-no-prompts-defaults))))

(defun lyskom-read-text-no-prefix-arg (prompt &optional always-prompt
				       default constraint)
  "Call in interactive list to read text-no for lyskom-commands using
configurable prefix argument heuristics. The strategy used for picking a
text-no is defined by the variable `kom-pick-text-no-strategy-alist'.

The PROMPT will be used to prompt for the number, either if invoked by the
strategy directly, or as a fallback when no strategy rule found a text-no,
or when that text-no did not meet the CONSTRAINT.

If the optional argument ALWAYS-PROMPT is non-nil and the user did not give
a prefix argument, she gets prompted for the text number regardless of the
`kom-pick-text-no-strategy-alist' settings. Another method of overriding
the prefix-less strategies for a command is via `kom-text-no-prompts'.

When DEFAULT is given, it will be the default text-no fall-back shown in the
prompt, when `kom-pick-text-no-strategy-alist' did not specify a working
default. The requirements that must be met are given by CONSTRAINT, when
specified. DEFAULT is either a text-no, a function for returning one or one
of the symbols 'last-seen-written and 'last-written, which are compatibility
aliases for lyskom-get-last-written-or-read-by-me and
lyskom-get-last-text-written-by-me respectively.

If the optional CONSTRAINT function or list is provided, it is called for
the text chosen by the strategy alist functions to validate that the text
really applies to the command. Returning nil means that it does, and
otherwise a helpful lyskom format string should be returned, which explains
to the user why that text did not apply to the command. This string may
refer to the text-no as %#1. The message is then presented to the user and
she gets prompted for a better text number. Failing a second time will
invoke the command anyway (probably to fail miserably, reporting a less
helpful error message).

When CONSTRAINT is a list, its first item is called with the text number as
its first argument and remaining list items appended to the argument list."
;  (lyskom-insert (format "Prefix arg: %s\n" current-prefix-arg))
  (let ((default (cond ((eq default 'last-written)
			(lyskom-get-last-text-written-by-me))
		       ((eq default 'last-seen-written)
			(lyskom-get-last-written-or-read-by-me))
		       ((functionp default) (funcall default))
		       (t default)))
	(constraint-func constraint)
	(constraint-args '())
	(text-no nil))
    (when (listp constraint)
      (setq constraint-func (car constraint))
      (setq constraint-args (cdr constraint)))
    (let* ((strategies kom-pick-text-no-strategy-alist)
           (how (append (cdr (assq lyskom-current-command strategies))
                        (cdr (assq t strategies)))))
      (while (and how (null text-no))
        (let* ((strategy-pred nil) ;; when a predicate to test the prefix
               (compare-value nil) ;; when a value to compare the prefix to
               (applies-p (car (car how))) ;; either one of the above
               (what-text (cdr (car how)))
               (constraint-not-met nil))
          (if (or (eq applies-p '-) (not (functionp applies-p)))
              (setq compare-value applies-p)
            (setq strategy-pred applies-p))

          (cond
           ((eq compare-value t) ;; provided a default value for the prompt
            (let ((new-default (lyskom-evaluate-text-no-strategy
                                what-text prompt default constraint)))
              (when (and (lyskom-plusp new-default))
                (setq default (or default new-default)))))

           ((or (eq compare-value current-prefix-arg) ;; a text-no strategy
                (and (functionp strategy-pred)
                     (funcall strategy-pred current-prefix-arg)))
            (setq text-no (lyskom-evaluate-text-no-strategy
                           what-text prompt default constraint))
;	    (lyskom-insert (format "cmp: %s\npred: %s\ntext-no: %s\n\n"
;				   compare-value strategy-pred text-no))
            (when (and (not (stringp text-no)) text-no constraint)
              (setq constraint-not-met
                    (apply constraint-func text-no constraint-args))
              (when constraint-not-met
                (lyskom-format-insert constraint-not-met text-no)
                (lyskom-format-insert
                 (lyskom-get-string 'prefix-arg-try-again))
                (setq text-no (lyskom-read-number prompt default))))))
          (setq how (cdr how)))))
    (cond
     ((and (null current-prefix-arg)
           (or always-prompt
               (lyskom-read-text-no-prompt-p lyskom-current-command)))
      (lyskom-read-number prompt 
                          (cond ((lyskom-plusp default) default)
                                ((lyskom-plusp text-no) text-no)
                                (t (lyskom-default-value
                                    'lyskom-current-text)))))
     ((stringp text-no) ;; a strategy failure error message
      (lyskom-error text-no))
     ((and (lyskom-plusp text-no)) ;; a proper text-no
      text-no)
     ((null current-prefix-arg) ;; a fall-back when no strategy had kicked in
      (lyskom-read-number prompt default))
     (t
      (lyskom-error (lyskom-get-string 'bad-text-no-prefix)
                    current-prefix-arg)))))

(defun lyskom-evaluate-text-no-strategy (strategy prompt default constraint)
  (let ((prefix current-prefix-arg)
	(strategy-func nil)
	(strategy-args '()))
    (cond
     ((listp strategy)
      (setq strategy-func (car strategy))
      (setq prefix (funcall (car (cdr strategy)) prefix))
      (setq strategy-args (cdr (cdr strategy)))
      (apply strategy-func prefix prompt default constraint
	     strategy-args))
     ((functionp strategy)
      (funcall strategy prefix prompt default constraint)))))

;;; ============================================================
;;; Set functions

(defun lyskom-subset-p (a b)
  "Return non-nil if A is a subset of B"
  (let ((result t))
    (while a
      (if (memq (car a) b)
          (setq a (cdr a))
        (setq result nil a nil)))
    result))

(defun lyskom-intersection (a b)
  "Returns as a list the intersection of list A and list B.
The order of the list a is kept."
  (if (or a b)
      (let ((list nil))
        (while a
          (if (memq (car a) b)
              (setq list (cons (car a) list)))
          (setq a (cdr a)))
        (nreverse list))))

(defun lyskom-union (a b)
  "Returns a list containing the union of list A and list B.
Specifically, concatenate all elements of B that are not in A
to the end of A. This operation is destructive and modifies the
list pointed to by A."
  (let ((result nil))
    (lyskom-traverse x b
      (unless (memq x a) (setq result (cons x result))))
    (nconc a (nreverse result))))

(defun lyskom-delete-duplicates (list &optional key)
  "Removes all but one instance of each element in LIST.
If optional argument KEY is non-nil, apply KEY to each element before
comparison. Comparison is done with eq."
  (let ((clist (mapcar (lambda (el)
                         (if key
                             (cons (funcall key el) el)
                           (cons el el)))
                       list))
        (result nil))
    (lyskom-traverse el clist
      (unless (assq (car el) result)
        (setq result (cons el result))))
    (nreverse (mapcar 'cdr result))))

(defun lyskom-plusp (int)
  "Returns t for integers greater than 0, nil otherwise."
  (and (integerp int) (> int 0)))

(defun lyskom-minusp (int)
  "Returns t for integers smaller than 0, nil otherwise."
  (and (integerp int) (< int 0)))

;;; ======================================================================
;;; Display device management
;;;


;;; Definition of some useful functions from XEmacs

(lyskom-provide-function console-type (&optional console)
  (or window-system 'tty))

(lyskom-external-function display-color-p)
(lyskom-external-function display-grayscale-p)

(eval-and-compile
  (cond ((fboundp 'device-class)	; XEmacsism
	 (defalias 'lyskom-device-class 'device-class))
	((and (fboundp 'display-color-p) (fboundp 'display-grayscale-p))
	 ;; Emacs 21 can use color even when not running under
	 ;; X-windows.  Note that display-grayscale-p can be false when
	 ;; using a color display!  This happens when running on a
	 ;; classic Linux tty console.
	 (defun lyskom-device-class (&optional device)
	   (cond ((display-color-p device) 'color)
		 ((display-grayscale-p device) 'grayscale)
		 (t 'mono))))
	(t ;; This works in Emacs 20 and earlier.
	 (defun lyskom-device-class (&optional device)
	   (condition-case nil
	       (if (x-display-grayscale-p device)
		   (if (x-display-color-p device)
		       'color
		       'grayscale)
		   'mono)
	     (error 'mono))))))

(lyskom-provide-function frame-property (frame property &optional default)
  (or (cdr (assq property (frame-parameters frame)))
      default))


;;; XEmacs doesn't seem to have a background-mode frame property

(defun lyskom-background-mode ()
  (frame-property (selected-frame) 'background-mode 'light))


;;; ======================================================================
;;; LysKOM Hooks
;;;

(defun lyskom-run-hook-with-args (hook &rest args)
  "Run HOOK with the specified arguments ARGS in the LysKOM buffer.
See run-hook-with-args for detailed information."
  (lyskom-save-excursion (set-buffer (or (and (boundp 'lyskom-buffer)
                                              lyskom-buffer)
                                         (current-buffer)))
                  (apply 'run-hook-with-args hook args)))


;;;(defun lyskom-set-variable-hook-wrapper (hook-function args)
;;;  "Wrapper for running HOOK-FUNCTION with ARGS.
;;;If HOOK returns an alist ((S1 . V1) ...), set value of symbol S1 to V1
;;;and similarly for each element.
;;;
;;;The hook may set hook-function-args to change the arguments to subsequent
;;;hook functions.
;;;
;;;Hook functions that rely on this are totally dependent on the current 
;;;implementation of all callers. If you think you want to use this feature,
;;;think again. You probably don't."
;;;  (lyskom-traverse el (apply hook args)
;;;    (set (car el) (cdr el))))
;;;
;;;
;;;(defun lyskom-run-hook-with-wrapper (hook wrapper &rest args)
;;;  "Run HOOK by calling WRAPPER for each function in the LysKOM buffer.
;;;The wrapper will be called with arguments FUNCTION and ARGS. It
;;;calls the actual hook function by applying FUNCTION to ARGS.
;;;
;;;This function emulates the built-in run-hook-with-args function.
;;;It is modeled on the XEmacs 21.2 implementation, so there may be
;;;slight discrepancies with other Emacs versions.
;;;
;;;The variable hook-function-args holds the arguments to the hook 
;;;function (the ARGS parameter for the wrapper)."
;;;  (save-excursion
;;;    (set-buffer (or (and (boundp 'lyskom-buffer) lyskom-buffer)
;;;                    (current-buffer)))
;;;    (let ((hook-function-args args))
;;;      (when (and hook (boundp hook))
;;;        (let ((val (symbol-value hook)))
;;;          (if (and (consp val) (eq (car val 'lambda)))
;;;              (funcall wrapper val args)
;;;            (while (and val (consp val))
;;;              (if (eq (car val) t)
;;;                  (let ((globals (default-value hook)))
;;;                    (if (and (consp globals) (eq (car globals 'lambda)))
;;;                        (funcall wrapper hook args)
;;;                      (while (and globals (consp globals))
;;;                        (unless (eq (car globals t))
;;;                          (funcall wrapper (car globals) args))
;;;                        (setq globals (cdr globals)))))
;;;                (funcall wrapper (car val) args)
;;;                (setq val (cdr val))))))))))

                                                     

(defun lyskom-add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION in the LysKOM buffer.
If optional APPEND is non-nil, add at the end of HOOK."
  (save-excursion (set-buffer (or (and (boundp 'lyskom-buffer)
                                       lyskom-buffer)
                                  (current-buffer)))
                  (add-hook hook function append t)))

(defun lyskom-remove-hook (hook function)
  "From the value of HOOK remove the function FUNCTION in the LysKOM buffer."
  (save-excursion (set-buffer (or (and (boundp 'lyskom-buffer)
                                       lyskom-buffer)
                                  (current-buffer)))
                  (remove-hook hook function t)))




;;; ======================================================================
;;; Printing
;;;
;;; XEmacs princ does not insert text properties. This function is based
;;; on the C code for princ. 
;;;

(defun lyskom-princ (object &optional stream)
  "Output the printed representation of OBJECT, any Lisp OBJECT.
No quoting characters are used; no delimiters are printed around
the contents of strings. Text properties are retained.

Output stream is STREAM, or value of standard-output, and must be a
buffer or a marker. Function or minibuffer streams are not supported
for strings."
  (if (not (stringp object))
      (princ object stream)
    (let ((old-point nil)
          (start-point nil)
          (old-buffer (current-buffer)))
      (unwind-protect
          (progn
            (cond ((bufferp stream) (set-buffer stream))
                  ((markerp stream) 
                   (setq old-point (point))
                   (set-buffer (marker-buffer stream))
                   (goto-char stream)
                   (setq start-point (point)))
                  ((null stream)
                   (cond ((bufferp standard-output) (set-buffer standard-output))
                         ((markerp standard-output) 
                          (setq old-point (point))
                          (set-buffer (marker-buffer standard-output))
                          (goto-char standard-output)
                          (setq start-point (point))))))

            (insert object))
        (cond ((markerp stream) 
               (set-marker stream (point))
               (if (>= old-point start-point)
                   (goto-char (+ old-point (- (point) start-point)))
                 (goto-char old-point))))
        (set-buffer old-buffer)))))


;;; ======================================================================
;;; Faces
;;;

(make-face 'lyskom-weak-highlight-face)
(make-face 'lyskom-strong-highlight-face)

(defun lyskom-set-face-foreground (face color)
  (condition-case nil
      (set-face-foreground face color)
    (error nil)))

(defun lyskom-set-face-background (face color)
  (condition-case nil
      (set-face-background face color)
    (error nil)))

(defun lyskom-copy-face (old new)
  (lyskom-xemacs-or-gnu (copy-face old new nil nil nil 'remove-all)
                        (copy-face old new)))



(defun lyskom-set-face-scheme (scheme)
  "Set the LysKOM color and face scheme to SCHEME. Valid schemes are listed
in lyskom-face-schemes."
  (let ((tmp (assoc scheme lyskom-face-schemes))
        (properties nil)
        (set-faces nil)
        (background (or (face-background 'default)
                        (frame-property (selected-frame) 'background-color))))
    (when (and tmp
               (fboundp 'copy-face)
               (fboundp 'lyskom-set-face-foreground)
               (fboundp 'lyskom-set-face-background))


      ;; If we have a background color, then compute the highlight colors

      (when background
        (lyskom-set-face-background 'lyskom-strong-highlight-face
                                    (lyskom-get-color-highlight (lyskom-color-values background) 0.05))
        (lyskom-set-face-background 'lyskom-weak-highlight-face
                                    (lyskom-get-color-highlight (lyskom-color-values background) 0.025)))

      ;; Traverse face specifications in the face scheme

      (lyskom-traverse spec (cdr tmp)
        (if (eq 'property (car spec))
            (setq properties (cons (cons (elt spec 1) (elt spec 2)) properties))
          (if (elt spec 1) (lyskom-copy-face (elt spec 1) (elt spec 0)) (make-face (elt spec 0)))
          (when (elt spec 2) (lyskom-set-face-foreground (elt spec 0) (elt spec 2)))
          (when (elt spec 3) (lyskom-set-face-background (elt spec 0) (elt spec 3)))
          (setq set-faces (cons (elt spec 0) set-faces))))

      ;; Check that the background color of the default face is what
      ;; the face scheme expects. If not, copy the computed highlight
      ;; faces to the real highlight faces.

      (let ((expected-background
             (or (null background)
                 (null (assq 'expected-background properties))
                 (equal (lyskom-color-values 
                         (cdr (assq 'expected-background properties)))
                        (lyskom-color-values background)))))
         (unless (and (memq 'kom-dashed-lines-face set-faces)
                      expected-background)
           (copy-face 'lyskom-strong-highlight-face 'kom-dashed-lines-face))

         (unless (and (memq 'kom-text-body-face set-faces)
                      expected-background)
           (copy-face 'lyskom-weak-highlight-face 'kom-text-body-face))

         (unless (and (memq 'kom-async-dashed-lines-face set-faces)
                      expected-background)
           (copy-face 'lyskom-strong-highlight-face 'kom-async-dashed-lines-face))

         (unless (and (memq 'kom-async-text-body-face set-faces)
                      expected-background)
           (copy-face 'lyskom-weak-highlight-face 'kom-async-text-body-face))

         (setq set-faces (append set-faces
                                 (list 'kom-dashed-lines-face
                                       'kom-text-body-face
                                       'kom-async-dashed-lines-face
                                       'kom-async-text-body-face))))

        ;; Check that we've set all faces. If not, copy the default face and post a message

        (let ((unset-faces nil))
          (lyskom-traverse face-name lyskom-faces
            (unless (memq face-name set-faces)
              (setq unset-faces (cons face-name unset-faces))
              (copy-face 'default face-name)))

          (when unset-faces
            (lyskom-format-insert-before-prompt 
             'missing-faces
             (symbol-name scheme)
             (mapconcat 'symbol-name
                        unset-faces
                        "\n    ")))))))


(defun lyskom-face-resource (face-name attr type)
  (if (eq (lyskom-emacs-version) 'xemacs)
      ;; XEmacs style
      (let ((val (x-get-resource (concat face-name ".attribute" attr)
				 (concat "Face.Attribute" attr)
				 type)))
	(cond ((eq type 'string) val)
	      ((and (eq type 'boolean) val) (if (car val) 'on 'off))
	      (t val)))
    ;; Emacs style
    (let ((val (x-get-resource (concat face-name ".attribute" attr)
			       (concat "Face.Attribute" attr))))
      (cond ((eq type 'string) val)
	    ((and val
		  (eq type 'boolean)
		  (member (downcase val) '("on" "true"))) 'on)
	    ((and val (eq type 'boolean)) 'off)
	    (t val)))))


(defun lyskom-modify-face (what face)
  (condition-case nil
      (funcall (intern (concat "make-face-" (symbol-name what)))
               face)
    (error nil)))

(defun lyskom-setup-faces ()
  "Initalize the faces in the LysKOM client.
This sets the face scheme according to `kom-default-face-scheme', and
also reads the proper X resources."
  (unless kom-default-face-scheme
    (setq kom-default-face-scheme
	  (condition-case nil
	      (cond ((eq (lyskom-device-class) 'mono) 'monochrome)
		    ((eq (lyskom-background-mode) 'dark)
		     'inverse)
		    (t 'default))
	    (error 'default))))  
  (lyskom-set-face-scheme kom-default-face-scheme)
  (if (eq (console-type) 'x)
      (lyskom-traverse face lyskom-faces
        (let* ((face-name (symbol-name face))
               (fg (lyskom-face-resource face-name "Foreground" 'string))
               (bg (lyskom-face-resource face-name "Background" 'string))
               (bl (lyskom-face-resource face-name "Bold" 'boolean))
               (it (lyskom-face-resource face-name "Italic" 'boolean))
               (ul (lyskom-face-resource face-name "Underline" 'boolean)))
          (if fg (set-face-foreground face fg))
          (if bg (set-face-background face bg))
          (if (eq bl 'on) (lyskom-modify-face 'bold face))
          (if (eq bl 'off) (lyskom-modify-face 'unbold face))
          (if (eq it 'on) (lyskom-modify-face 'italic face))
          (if (eq it 'off) (lyskom-modify-face 'unitalic face))
          (if ul (set-face-underline-p face (eq ul 'on)))))))


;;; ============================================================
;;; Date and time utilities

(defun lyskom-current-client-time ()
  "Return time representing current client time."
  (let ((now (decode-time)))
    (lyskom-create-time (elt now 0)     ;sec
                        (elt now 1)     ;min
                        (elt now 2)     ;hour
                        (elt now 3)     ;mday
                        (elt now 4)     ;mon
                        (elt now 5)     ;year
                        (elt now 6)     ;wday
                        0               ;yday
                        (if (elt now 7) ;isdst
                            1
                          0)
                        )))

(defun lyskom-current-server-time ()
  "Return time representing current server time."
  (blocking-do 'get-time))

(defun lyskom-format-time (format &optional time)
  "Return TIME as a string formatted as FORMAT.

FORMAT may be a string or a symbol.  If it is a symbol, it is
interpreted as follows:

    'date-and-time  Include date and time. [1][2]
    'date           Include just date. [1]
    'time           Include just time. [2]

    [1] If kom-print-seconds-in-time-strings is nil, only hours and
    minutes will be included in the time; if it is non-nil, seconds
    will also be included.
    [2] If kom-print-relative-dates is non-nil and the date is today's
    or yesterday's, the string \"today\" or \"yesterday\" (respectively)
    is used instead of the standard date format.

If FORMAT is a symbol but not one of the symbols listed above, the
format string will be looked up with lyskom-get-string.  The
timeformat-* strings are tailored to be used as formats for this
function.

The arguments to the format string are (in order): year, month number
\(starting with one for January), day-of-month number, hour, minute,
second, full name of the day of the week, abbreviated name of the day
of the week.

TIME defaults to the current client time."
  (let* ((time (or time (lyskom-current-client-time)))
         (fmt (cond
               ((stringp format)
                format)
               ((memq format '(date-and-time date time))
                (lyskom-format (cond ((eq format 'date-and-time)
                                      'format-time-date-and-time)
                                     ((eq format 'date)
                                      'format-time-just-date)
                                     ((eq format 'time)
                                      'format-time-just-time))
                               (lyskom-get-string
                                (or (and kom-print-relative-dates
                                         (lyskom-calculate-day-diff time))
                                    'timeformat-yyyy-mm-dd))
                               (lyskom-get-string
                                (if kom-print-seconds-in-time-strings
                                    'timeformat-hh-mm-ss
                                  'timeformat-hh-mm))))
               ((symbolp format)
                (lyskom-get-string format))
               (t (error "Invalid argument")))))
    (lyskom-format fmt
                   (time->year time)
                   (time->mon  time)
                   (time->mday time)
                   (time->hour time)
                   (time->min  time)
                   (time->sec  time)
                   (elt (lyskom-get-string 'weekdays)
                        (time->wday time))
                   (elt (lyskom-get-string 'weekdays-short)
                        (time->wday time)))))


;;; ============================================================
;;; Keymap utilities

(defun lyskom-lookup-key (keymap event &optional accept-default)
  (if (not (arrayp event))
      (setq event (vector event)))
  (if (null keymap)
      (and accept-default 
           (lookup-key global-map event))
    (or (lookup-key keymap event)
        (lyskom-lookup-key (keymap-parent keymap) event accept-default))))

(defun lyskom-keymap-body (keymap)
  (setq keymap (cdr keymap))
  (cond ((arrayp (car keymap)) (car keymap))
        (t keymap)))

(defun lyskom-keymap-realbinding (binding)
  (while (stringp (car-safe binding))
    (setq binding (cdr binding)))
  binding)

(defun lyskom-overlay-keymap (basemap overlay keymap &optional prefix)
  (let ((keys (make-vector (1+ (length prefix)) nil))
        (index (length prefix))
        (body nil)
        (r 0))

    (while (< r (length prefix))
      (aset keys r (aref prefix r))
      (setq r (1+ r)))

    (cond ((not (keymapp keymap)))
          ((not (keymapp overlay)))
          ((not (keymapp basemap)))

          ((setq body (lyskom-keymap-body overlay))
           (mapcar
            (function
             (lambda (element)
               (cond ((arrayp element)
                      (let ((len (length element)))
                        (setq r 0)
                        (while (< r len)
                          (aset keys index r)
                          (lyskom-overlay-keys keys (aref element r)
                                               basemap overlay keymap)
                          (setq r (1+ r)))))

                     ((consp element)
                      (when (not (eq t (car element)))
                        (aset keys index (car element))
                        (lyskom-overlay-keys keys
                                             (lyskom-keymap-realbinding
                                              (cdr element))
                                             basemap overlay keymap)))

                     (t nil))))
            body)))))


(defun lyskom-overlay-keys (keys binding basemap overlay keymap)
  (let ((base-binding (lyskom-lookup-key basemap keys nil)))

   ;; If the binding is a keymap or prefix and
   ;; the binding in the base is a keymap or prefix 
   ;; then recurse

   (cond ((and (keymapp binding)
               (keymapp base-binding))
          (lyskom-overlay-keymap basemap binding keymap keys))

   ;; If the binding is a keymap or prefix and
   ;; we are bound in the base
   ;; then don't recurse

         ((and (keymapp binding)
               base-binding) nil)

   ;; If we are not bound in the base
   ;; copy the binding

         ((and binding
               (null base-binding)) (define-key keymap keys binding)))))


;;;
;;; Stuff
;;;

(defun lyskom-return-membership-type (mt)
  "Return a text description of membership type mt"
  (let ((tmp
         (mapconcat 
          'identity
          (delete nil
                  (list (if (membership-type->invitation mt) (lyskom-get-string 'invitation-mt-type) nil)
                        (if (membership-type->passive mt) (lyskom-get-string 'passive-mt-type) nil)
                        (if (membership-type->secret mt) (lyskom-get-string 'secret-mt-type) nil)))
          ", ")))
    (if (string= tmp "") 
        tmp
      (concat "[" tmp "]"))))

(defun lyskom-find-unread (conf-no)
  "Return the number of unread texts in CONF-NO.
If this function is unable to calculate the number of unread texts it will
return nil."
  (save-excursion
   (set-buffer lyskom-buffer)
   (let ((rlist (read-list->all-entries lyskom-to-do-list))
         (found nil))
     (while (and (not found) rlist)
       (when (eq conf-no (conf-stat->conf-no
                          (read-info->conf-stat (car rlist))))
         (setq found (length (cdr (read-info->text-list (car rlist))))))
       (setq rlist (cdr rlist)))
     found)))

(defun lyskom-prev-area (num prop &optional goto-point-min)
  (while (> num 0)
    (let ((where (previous-single-property-change (point) prop)))
      (cond (where
             (if (not (get-text-property where prop))
                 (setq where (previous-single-property-change 
                              where prop)))
             (if where
                 (goto-char where)
               (goto-char (point-min))
               (setq num 1)))
            (goto-point-min (goto-char (point-min))
                            (setq num 1))))
    (setq num (1- num))))

(defun lyskom-next-area (num prop &optional goto-point-max)
  "Move the cursor to the next prompt in the LysKOM buffer"
  (interactive "p")
  (while (> num 0)
    (let ((where (next-single-property-change (point) prop)))
      (cond (where
             (if (not (get-text-property where prop))
                 (setq where (next-single-property-change where prop)))
             (if where
                 (goto-char where)
               (goto-char (point-max))
               (setq num 1)))
            (goto-point-max (goto-char (point-max))
                            (setq num 1))))
      (setq num (1- num))))

;;; ============================================================
;;; Database stuff

;; Extracted from edit-text.el
;;(defun lyskom-is-supervisor (conf-stat &optional memo)
;;  "Return non-nil if lyskom-pers-no is a supervisor of CONF-STAT."
;;  (cond ((null conf-stat) nil)
;;        ((memq (conf-stat->conf-no conf-stat) memo) nil)
;;        ((eq lyskom-pers-no (conf-stat->conf-no conf-stat)) t)
;;        ((eq lyskom-pers-no (conf-stat->supervisor conf-stat)) t)
;;        ((eq 0 (conf-stat->supervisor conf-stat)) nil)
;;        ((lyskom-get-membership (conf-stat->conf-no conf-stat) t) t)
;;        ((lyskom-is-supervisor
;;          (blocking-do 'get-conf-stat (conf-stat->supervisor conf-stat))
;;          (cons (conf-stat->conf-no conf-stat) memo)))))

(defun lyskom-is-supervisor (conf-no viewer-no)
  "Return non-nil if the supervisor of CONF-NO is VIEWER-NO."
  (or (eq viewer-no conf-no)
      (lyskom-is-strictly-supervisor conf-no viewer-no)))

(defun lyskom-is-strictly-supervisor (conf-no viewer-no)
  "Return non-nil if VIEWER-NO is strictly a supervisor of CONF-NO

Cannot be called from a callback."
  (let ((collector (make-collector))
        (conf-stat nil))
    (initiate-get-conf-stat 'background 'collector-push conf-no collector)
    (lyskom-wait-queue 'background)
    (setq conf-stat (car (collector->value collector)))

    (cond ((null viewer-no) nil)
          ((eq viewer-no 0) nil)
          ((null conf-stat) nil)
          ((eq viewer-no (conf-stat->supervisor conf-stat)) t)
          ((lyskom-is-member (conf-stat->supervisor conf-stat) viewer-no) t)
          (t nil))))

(defun lyskom-is-member (conf-no pers-no &optional queue)
  "Return the membership in CONF-NO of PERS-NO
Optional argument QUEUE is the queue to send the queries on.

Cannot be called from a callback."
  (or (and (eq pers-no lyskom-pers-no)
           (lyskom-try-get-membership conf-no t))
      (let ((collector (make-collector)))
        (initiate-query-read-texts (or queue 'background)
                                   'collector-push
                                   pers-no
                                   conf-no
                                   collector)
        (lyskom-wait-queue (or queue 'background))
        (car (collector->value collector)))))

(defun lyskom-text-recipients (text-stat &optional want-types)
  "Return the list of recipients for TEXT-STAT.
If WANT-TYPES is non-nil then the result is an assoc list where the 
car of each element is the recipient number and the cdr is the type."
  (let ((result nil))
    (lyskom-traverse misc (text-stat->misc-info-list text-stat)
      (when (memq (misc-info->type misc) lyskom-recpt-types-list)
        (if want-types
            (setq result (cons (cons (misc-info->recipient-no misc)
                                     (misc-info->type misc))
                               result))
          (setq result (cons (misc-info->recipient-no misc)
                             result)))))
    (nreverse result)))

(defun lyskom-text-comments (text-stat)
  "Return the list of comments to TEXT-STAT"
  (let ((result nil))
    (lyskom-traverse misc (text-stat->misc-info-list text-stat)
      (cond ((eq (misc-info->type misc) 'FOOTN-IN)
             (setq result (cons (misc-info->footn-in misc) result)))
            ((eq (misc-info->type misc) 'COMM-IN)
             (setq result (cons (misc-info->comm-in misc) result)))))
    (nreverse result)))


(defun lyskom-find-text-by-date (conf-stat target-date)
  "Search texts in CONF-STAT for a text added on or about TARGET-DATE.
Returns a cons of (LOCAL . GLOBAL)"
  (let* ((lowest (conf-stat->first-local-no conf-stat))
         (highest (+ lowest (conf-stat->no-of-texts conf-stat)))
         (result nil)
         (index (+ lowest (/ (- highest lowest) 2)))
         (last-index (1- index)))
    (while (/= last-index index)
      (let* ((map (blocking-do 'local-to-global 
                               (conf-stat->conf-no conf-stat)
                               index
                               1)))
        (cond ((null map) (setq lowest highest))
              ((null (text-mapping->global-numbers map))
               (setq highest index))
              (t 
               (let* ((text-no (car (text-mapping->global-numbers map)))
                      (text-stat (blocking-do 'get-text-stat text-no))
                      (local-no (text-mapping->global-to-local map text-no))
                      (date (and text-stat
                                 (lyskom-traverse misc
                                     (text-stat->misc-info-list text-stat)
                                   (when (and (memq (misc-info->type misc) lyskom-recpt-types-list)
					      (eq (misc-info->recipient-no misc) text-no))
                                     (lyskom-traverse-break 
                                      (if (misc-info->sent-at misc)
                                          (misc-info->sent-at misc)
                                        (text-stat->creation-time text-stat))))))))
                 (when text-stat
                   (setq index local-no)
                   (if (lyskom-time-greater
			date
                        target-date)
                       (setq highest index)
                     (setq lowest index))
                   (setq result text-stat))))))
      (setq last-index index)
      (setq index (+ lowest (/ (- highest lowest) 2))))
    (cons last-index result)))

(defvar lyskom-year-window-start 80
  "Windowing threshold for YY year specifications.
Years below this are considered in the 21st century. Years above this
in the 20th century")

(defun lyskom-parse-date (arg)
  "Parse ARG as a date."
  (let ((month-regexp (concat "\\("
                              (mapconcat (lambda (el)
                                           (regexp-quote (car el)))
                                         lyskom-month-names
                                         "\\|")
                              "\\)"))
        (dmy-regexp (concat "\\("
                            (mapconcat (lambda (el)
                                         (regexp-quote (lyskom-get-string el)))
                                       '(years year months month days day)
                                       "\\|")
                            "\\)"))
        (test-date (format-time-string "%x" '(20 0)))
        year month day di mi yi)

  ;; Look at test-date to see where dates in ambiguous cases should go

    (cond ((string-match "01.*16.*70" test-date) (setq di 2 mi 1 yi 3))
          ((string-match "01.*70.*16" test-date) (setq di 3 mi 1 yi 2))
          ((string-match "16.*01.*70" test-date) (setq di 1 mi 2 yi 3))
          ((string-match "16.*70.*01" test-date) (setq di 1 mi 3 yi 2))
          ((string-match "70.*01.*16" test-date) (setq di 3 mi 2 yi 1))
          ((string-match "70.*16.*01" test-date) (setq di 2 mi 3 yi 1)))

     (cond ((string-match "\\([0-9][0-9][0-9][0-9]\\)[-./]\\([0-9][0-9]?\\)[-./]\\([0-9][0-9]?\\)" arg)
           ;; YYYY-MM-DD
           (setq year (string-to-int (match-string 1 arg))
                 month (string-to-int (match-string (if (> mi di) 3 2) arg))
                 day (string-to-int (match-string (if (> di mi) 2 3) arg)))
           (when (> month 12) (setq month day day month))
           )
          ((string-match "\\([0-9][0-9]?\\)[-./]\\([0-9][0-9]?\\)[-./]\\([0-9][0-9][0-9][0-9]?\\)" arg)
           ;; YYYY-MM-DD
           (setq year (string-to-int (match-string 3 arg))
                 month (string-to-int (match-string (if (> mi di) 2 1) arg))
                 day (string-to-int (match-string (if (> di mi) 1 2) arg)))
           (when (> month 12) (setq month day day month))
           )
          ((string-match "\\([0-9][0-9]\\)[-./]\\([0-9][0-9]?\\)[-./]\\([0-9][0-9]?\\)" arg)
           (setq year (string-to-int (match-string yi arg))
                 month (string-to-int (match-string mi arg))
                 day (string-to-int (match-string di arg)))
           (when (> month 12) (setq month day day month))
           )
          ((string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" arg)
           ;; Ambiguous:
           ;; MM/DD       Euro
           ;; DD/MM       US
           (let ((a (string-to-int (match-string 1 arg)))
                 (b (string-to-int (match-string 2 arg))))
             (cond ((> a 12) (setq month b day a))
                   ((> b 12) (setq month a day b))
                   ((> di mi) (setq month b day a))
                   (t (setq month a day b))))
           )
          ((string-match (format "%s \\([0-9][0-9]?\\)" month-regexp) arg)
           ;; Ambiguous:
           ;; Month DD
           )
          ((string-match (format "%s,? \\([0-9][0-9][0-9][0-9]\\)" month-regexp) arg)
           ;; Ambiguous:
           ;; Month YYYY
           )
          ((string-match (format "\\([0-9][0-9]?\\) %s" month-regexp) arg)
           ;; DD Month
           )
          ((string-match (format "\\([0-9][0-9]?\\) %s \\([0-9][0-9][0-9][0-9]\\)" month-regexp) arg)
           ;; DD Month YYYY
           )
          ((string-match (format "%s \\([0-9][0-9]?\\), \\([0-9][0-9][0-9]?[0-9]?\\)" month-regexp) arg)
           ;; Month DD, YYYY
           )
          ((string-match (format "\\([0-9][0-9]?\\) %s, \\([0-9][0-9][0-9]?[0-9]?\\)" month-regexp) arg)
           ;; DD Month, YYYY
           )
          ((string-match (format "-\\([0-9]+\\) %s" dmy-regexp) arg)
           ;; -NN days, months, years
           )
          )

    (if (< year lyskom-year-window-start)
        (setq year (+ 2000 year))
      (setq year (+ 1900 year)))

    (list year month day)
    ))


;;; ================================================================
;;; Check noconversion

(defun lyskom-viewing-noconversion ()
  "Return non-nil if we are reviewing in noconversion mode."
  (eq 'kom-review-noconversion lyskom-current-command))


;;; ================================================================
;;; Read membership types

(defun lyskom-read-membership-type ()
  "Interactively read a membership type"
  (let ((invitation (lyskom-j-or-n-p 'mship-type-invitation-q))
        (passive (lyskom-j-or-n-p 'mship-type-passive-q))
        (secret (lyskom-j-or-n-p 'mship-type-secret-q)))
    (lyskom-create-membership-type invitation
                                   passive
                                   secret
                                   nil
                                   nil
                                   nil
                                   nil
                                   nil)))


;;; ================================================================
;;; String truncation
;;;

(defun lyskom-truncate-to-lines (string threshold show-lines &optional width)
  "If STRING is more than THRESHOLD lines on screen, truncate it to  SHOW-LINES.
Optional argument WIDTH is thw window width to use instead of window-width.

This function takes the setting of truncate-lines into account, so
the resulting string may not have SHOW-LINES newline characters.

Result is eq to STRING when no truncation is required.

The result is approximate when truncate-lines is non-nil since different
Emacsen use a different number of characters for the continuation marks
at the end of broken lines. We assume one character continuation marks."
  (let ((line-length (if truncate-lines lyskom-max-int (- (or width (window-width)) 1)))
        (count 0)
        (end nil)
        (pos 0))
    (while (and (< pos (length string)) (< count threshold))
      (setq count (1+ count))
      (let ((next (string-match "\\(\n\\|\\'\\)" string pos)))
        (if (> (- next pos) line-length)
            (setq pos (+ pos line-length))
          (setq pos (match-end 0))))
        (when (= count show-lines)
          (setq end pos)))

    (if (>= count threshold)
        (substring string 0 end)
      string)))


;;; ================================================================
;;; Color model manipulations
;;;
;;; (Of COURSE you need this in a KOM client!)
;;;

(defun lyskom-get-color-highlight (color distance)
  "Create a highlight color for COLOR that is DISTANCE away.
COLOR is a list of R G and B components from 0 to 65535.
DISTANCE is a non-negative integer no larger than 1.0, that in some
way specifies how far away from the original color the new color
should be."
  (when color
    (let* ((hls (lyskom-rgb-to-hls (mapcar (lambda (x) (/ x 65535.0)) color)))
           (l (elt hls 1)))
      (if (> l 0.6)
          (setq l (- l distance))
        (setq l (+ l distance)))
      (cond ((> l 1.0) (setq l 1.0))
            ((< l 0.0) (setq l 0.0)))
      (aset hls 1 l)

      (apply 'format "#%02x%02x%02x"
             (mapcar (lambda (c) (round (* 255 c)))
                     (lyskom-hls-to-rgb hls))))))


(defun lyskom-rgb-to-hls (rgb)
  "Convert a point in RGB color space to a point in HLS color space.

Input value is a vector [R G B], where R, G and B represent red, green
and blue components, respectively. Each value is a non-negative
floating-point value no larger than 1.0.

Output is a vector [H L S], where H, L and S represend hue, lightness
and saturation, respectively. H is in the range 0..360, L and S are
non-negative floating-point numbers no higher than 1.0. If the input
color is a shade of gray (all components are equal), then H in the
output is nil.

Algorithm adapted from Foley, \"Computer Graphics\"."
  (let* ((r (elt rgb 0))
         (g (elt rgb 1))
         (b (elt rgb 2))
         (rgbmin (min r g b))
         (rgbmax (max r g b))
         (h nil)
         (l (/ (+ rgbmax rgbmin) 2.0))
         (s nil)
         (delta (- rgbmax rgbmin)))
    (if (zerop delta)
        (setq s 0.0)
      (if (<= l 0.5)
          (setq s (/ delta (+ rgbmax rgbmin)))
        (setq s (/ delta (- 2.0 rgbmax rgbmin))))
      (cond ((= r rgbmax)
             (setq h (/ (- g b) delta)))
            ((= g rgbmax)
             (setq h (+ 2.0 (/ (- b r) delta))))
            ((= b rgbmax)
             (setq h (+ 4.0 (/ (- r g) delta)))))
      (setq h (* h 60.0))
      (if (< h 0)
          (setq h (+ 360.0 h))))
    (vector h l s)))

(defun lyskom-hls-to-rgb-value (n1 n2 h)
  "Helper function for lyskom-hls-to-rgb"
  (cond ((> h 360) (setq h (- h 360.0)))
        ((< h 0) (setq h (+ h 360.0))))

  (cond ((< h 60)  (+ n1 (* (- n2 n1) (/ h 60.0))))
        ((< h 180) n2)
        ((< h 240) (+ n1 (* (- n2 n1) (/ (- 240.0 h) 60.0))))
        (t n1)))

(defun lyskom-hls-to-rgb (hls)
  "Convert a point in HLS color space to a point in RGB color space.

Input HLS is a vector [H L S], where H, L and S represend hue,
lightness and saturation, respectively. H is in the range 0..360, L
and S are non-negative floating-point numbers no higher than 1.0. If
the input color is a shade of gray (all components are equal), then S
in the input is ignored and may be anything.

Output value is a vector [R G B], where R, G and B represent red,
green and blue components, respectively. Each value is in the range
1..1.0.

This algorithm is adapted from Foley, \"Computer Graphics\" (and
has the bug in that algorithm fixed)."
  (let* ((h (elt hls 0))
         (l (elt hls 1))
         (s (elt hls 2))
         (m2 (if (<= l 0.5)
                 (* l (+ 1.0 s))
               (+ l (* s (- 1 l)))))
         (m1 (- (* 2 l) m2)))
    (if (zerop s)
        (vector l l l)
      (vector (lyskom-hls-to-rgb-value m1 m2 (+ h 120))
              (lyskom-hls-to-rgb-value m1 m2 h)
              (lyskom-hls-to-rgb-value m1 m2 (- h 120))))))


;;; ================================================================
;;; Automatically test that RGB->X and X->RGB color model conversions
;;; really are the inverse of each other.
;;; 
;;; (defun lyskom-test-color-model ()
;;;   (let ((r 0.0)
;;;         (g 0.0)
;;;         (b 0.0)
;;;         (step 0.05))
;;;     (while (<= r 1.0)
;;;       (setq g 0.0)
;;;       (while (<= g 1.0)
;;;         (setq b 0.0)
;;;         (while (<= b 1.0)
;;;           (let ((tmp (lyskom-hls-to-rgb
;;;                       (lyskom-rgb-to-hls (vector r g b)))))
;;;             (unless (and (< (- r (elt tmp 0)) 0.000000001)
;;;                          (< (- g (elt tmp 1)) 0.000000001)
;;;                          (< (- b (elt tmp 2)) 0.000000001))
;;;               (message "Mismatch %1.2f,%1.2f,%1.2f gave %S/%S" r g b (lyskom-rgb-to-hls (vector r g b)) tmp)))
;;;           (setq b (+ b step)))
;;;         (setq g (+ g step)))
;;;       (setq r (+ r step)))))

;;;(defun lyskom-test-auto-colors ()
;;;  (make-face 'test-1)
;;;  (make-face 'test-2)
;;;  (make-face 'test-default)
;;;  (while t
;;;    (let ((foreground (read-from-minibuffer "Foreground: "))
;;;          (background (read-from-minibuffer "Background: ")))
;;;      (pop-to-buffer (get-buffer-create "*kom*-test"))
;;;      (erase-buffer)
;;;      (set-face-foreground 'test-default foreground)
;;;      (set-face-background 'test-default background)
;;;      (set-face-background 'test-1 (lyskom-get-color-highlight (x-color-values background) 0.05))
;;;      (set-face-background 'test-2 (lyskom-get-color-highlight (x-color-values background) 0.025))
;;;      (let ((lyskom-buffer (current-buffer)))
;;;        (lyskom-format-insert "\
;;;%#3@Läsa nästa fotnot...
;;;8408827 idag 00:40 /1 rad/ Lunkwill/CH ( Auf das Universum! )
;;;Fotnot till text 8408825 av Lunkwill/CH ( Auf das Universum! )
;;;Mottagare: Nätverk, Internet, LysNET, Sunet... <30873>
;;;Mottagare: SUBnet (Stångåstadens och LiU:s) studentbostadsnät <12152>
;;;Mottagare: Lunkwill/CH ( Auf das Universum! ) <2092>
;;;Ärende: Vårmötet
;;;%[%#1$------------------------------------------------------------
;;;%]%[%#2$Och tack så mycket för förklaringarna! =)
;;;%]%[%#1$(8408827) /Lunkwill/CH ( Auf das Universum! )/------
;;;%]Gå till nästa möte...
;;;SUN erfarenhetsutbyte - 1 oläst
;;;Läsa nästa text...
;;;8408823 idag 00:39 /4 rader/ Erik Persson, Lysato(r)
;;;Kommentar till text 8407161 av Dejan (något desperat)
;;;Mottagare: SUN erfarenhetsutbyte <23622>
;;;Ärende: Skapa partitionstabell
;;;%[%#1$------------------------------------------------------------
;;;%]%[%#2$Om du nu vill spela Fibre Channel med IDE-RAID så går du över ån efter
;;;vatten. Det finns redan IDE-RAID med FC-interface.  Dock har jag inte
;;;sett någon som stödjer något annat än FC-AL vilket är lite
;;;begränsande.
;;;%]%[%#1$(8408823) ------------------------------------------
;;;%]Gå till nästa möte...
;;;
;;;
;;;
;;;"
;;;                              '(face test-1)
;;;                              '(face test-2)
;;;                              '(face test-default))))))
;;;
