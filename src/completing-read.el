;;;;;
;;;;; $Id: completing-read.el,v 41.2 1996-05-05 22:19:53 davidk Exp $
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
;;;; ================================================================
;;;; ================================================================
;;;;
;;;; File: completing-read.el
;;;;
;;;; This file implements functions for reading a conference name
;;;; or a person name with completion and other help.
;;;;

;; Overview of `exported' functions from this file:
;; lyskom-read-conf-no        returns conf-no
;; lyskom-read-conf-stat      returns conf-stat
;; lyskom-read-conf-name      returns name


(setq lyskom-clientversion-long 
      (concat
       lyskom-clientversion-long
       "$Id: completing-read.el,v 41.2 1996-05-05 22:19:53 davidk Exp $\n"))


;;; Author: Linus Tolke


;;; Completing-function

(defvar lyskom-name-hist nil)

(defvar lyskom-completing-who-info-cache nil
  "Temporary cache of who-info data")

(defvar lyskom-completing-lookup-name-cache nil
  "Temporary cache of server queries")


(defun lyskom-completing-clear-cache ()
  (setq lyskom-completing-who-info-cache nil)
  (setq lyskom-completing-lookup-name-cache nil))

(defun lyskom-completing-who-is-on ()
  "Get information about who is on, first checking the cache. Returns what 
\(blocking-do 'who-is-on\) would, but as a list, not a vector"
  (if lyskom-completing-who-info-cache
      lyskom-completing-who-info-cache
    (setq lyskom-completing-who-info-cache
          (listify-vector (blocking-do 'who-is-on)))))

(defun lyskom-completing-lookup-name (string)
  "Look up STRING as a name. Same as \(blocking-do 'lookup-name ...\)
but first checks a cache."
  (let ((tmp (assoc string lyskom-completing-lookup-name-cache)))
    (if tmp
        (cdr tmp)
      (progn
        (setq tmp (blocking-do 'lookup-name string))
        (setq lyskom-completing-lookup-name-cache
              (cons (cons string tmp)
                    lyskom-completing-lookup-name-cache))
        tmp))))


(defvar lyskom-minibuffer-local-completion-map
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    (define-key map " " nil)
    map)
  "Keymap used for reading LysKOM names.")

(defvar lyskom-minibuffer-local-must-match-map
  (let ((map (copy-keymap minibuffer-local-must-match-map)))
    (define-key map " " nil)
    map)
  "Keymap used for reading LysKOM names.")


(defun lyskom-read-conf-no (prompt type &optional empty initial mustmatch)
  "Returns the conf-no of a conf or person read by lyskom-read-conf-name.
The question is prompted with PROMPT.
Only the conferences of TYPE are allowed.
The TYPE allows for subsets of the entire Lyskom-name space:
* all
* confs only conferences
* pers only persons
* logins only persons that are logged in right now.
* conflogin conferences and persons that are logged in.
If EMPTY is non-nil then the empty string is allowed (returns 0).
INITIAL is the initial contents of the input field."
  (lyskom-completing-clear-cache)
  (let (read)
    (while (and
            (string= 
             (setq read
                   (lyskom-read-conf-name prompt type mustmatch initial))
             "")
            (not empty)))
    (if (string= read "")
        0
      (lyskom-read-conf-name-internal read type 'conf-no))))


(defun lyskom-read-conf-stat (prompt type &optional empty initial)
  "Exactly the same as lyskom-read-conf-no but returns the conf-stat if possible.
Arguments: PROMPT TYPE EMPTY INITIAL
The TYPE allows for subsets of the entire Lyskom-name space:
* all
* confs only conferences
* pers only persons
* logins only persons that are logged in right now.

If EMPTY is non-nil then the empty string is allowed (returns nil)."
  (lyskom-completing-clear-cache)
  (let ((no (lyskom-read-conf-no prompt type empty initial t)))
    (if (or (null no) (zerop no))
	nil
      (blocking-do 'get-conf-stat no))))

(defun lyskom-read-conf-name (prompt type 
                                     &optional mustmatch 
                                     initial)
  "Read a LysKOM name, prompting with PROMPT.
The TYPE allows for subsets of the entire Lyskom-name space:
* all
* confs only conferences
* pers only persons
* logins only persons that are logged in right now.
The third argument MUSTMATCH makes the function always return the conf-no and 
never the read string.
The fourth argument INITIAL is the initial contents of the input-buffer.

Returns the name."
  (lyskom-completing-clear-cache)
  (let* ((completion-ignore-case t)
         ;; When lyskom-read-conf-name-internal is called the current-buffer
         ;; is the minibuffer and the buffer-local variable lyskom-proc is not
         ;; correct. Then the variable lyskom-blocking-process must be set
         ;; instead. It is not buffer-local but scopes the let.
         (lyskom-blocking-process lyskom-proc)
         (minibuffer-local-completion-map 
          lyskom-minibuffer-local-completion-map)
         (minibuffer-local-must-match-map 
          lyskom-minibuffer-local-must-match-map))
    (completing-read prompt 
                     'lyskom-read-conf-name-internal
                     type
                     mustmatch
                     initial
                     'lyskom-name-hist)))


(defun lyskom-read-conf-name-internal-verify-type (cs predicate logins)
  "Returns true if CONF-STAT is of the correct type.

For types se documentation of lyskom-read-conf-name-internal.
Logins is a list of conf-nos (only significant when PREDICATE is logins)."
  (or (eq predicate 'all)
      (and (eq predicate 'confs)
	   (not (conf-type->letterbox 
		 (conf-stat->conf-type cs))))
      (and (eq predicate 'pers)
	   (conf-type->letterbox
	    (conf-stat->conf-type cs)))
      (and (eq predicate 'logins)
	   (memq (conf-stat->conf-no cs) logins))
      (and (eq predicate 'conflogin)
           (or (not (conf-type->letterbox
                     (conf-stat->conf-type cs)))
               (memq (conf-stat->conf-no cs) logins)))))


(defun lyskom-read-conf-name-internal (string predicate all)
  "The \"try-completion\" for the lyskom-read name.
STRING is the string to be matched.
PREDICATE is one of:
* all
* confs only conferences
* pers only persons
* logins only persons that are logged in right now.
If third argument ALL is t then we are called from all-completions.
If third argument ALL is nil then we are called from try-completion.
If third argument ALL is 'conf-no then we are called from lyskom name
to conf-no translator."
  (let* ((alllogins (and (string= string "")
                         (or (eq predicate 'logins)
                             (eq predicate 'conflogin))))
         (list (if (or (not alllogins)
                       (eq predicate 'conflogin))
                   (lyskom-completing-lookup-name string)))
         (nos (listify-vector (conf-list->conf-nos list)))
         (parlist (if (memq predicate '(pers confs conflogin))
                      (let ((nos nos)
                            (typs (listify-vector 
                                   (conf-list->conf-types list)))
                            res)
                        (while nos
                          (setq res (cons (cons (car nos) (car typs)) res))
                          (setq nos (cdr nos)
                                typs (cdr typs)))
                        res)))
         (logins (and (or (eq predicate 'logins)
                          (eq predicate 'conflogin))
                      (mapcar
                       (function (lambda (ele)
                                   (who-info->pers-no ele)))
                       (lyskom-completing-who-is-on))))
         (mappedlist (cond
                      (alllogins logins)
                      ((eq predicate 'all) nos)
                      ((eq predicate 'confs)
                       (apply 'append 
                              (mapcar (function 
                                       (lambda (par)
                                         (and 
                                          (not
                                           (conf-type->letterbox (cdr par)))
                                              (list (car par)))))
                                      parlist)))
                      ((eq predicate 'pers)
                       (apply 'append
                              (mapcar (function
                                       (lambda (par)
                                         (and (conf-type->letterbox (cdr par))
                                              (list (car par)))))
                                      parlist)))
                      ((eq predicate 'logins)
                       (let ((nos (sort nos '<))
                             ;; We need logins later on
                             (lis (sort (copy-sequence logins) '<))
                             res)
                         (while (and nos
                                     lis)
                           (if (= (car nos) (car lis))
                               (setq res (cons (car nos) res)))
                           (if (> (car nos)
                                  (car lis))
                               (setq lis (cdr lis))
                             (setq nos (cdr nos))))
                         res))
                      ((eq predicate 'conflogin)
                       (append
                        (let ((nos (sort nos '<))
                              ;; We need logins later on
                              (lis (sort (copy-sequence logins) '<))
                              res)
                          (while (and nos
                                      lis)
                            (if (= (car nos) (car lis))
                                (setq res (cons (car nos) res)))
                            (if (> (car nos)
                                   (car lis))
                                (setq lis (cdr lis))
                              (setq nos (cdr nos))))
                          res)                        
                        (apply 'append
                               (mapcar 
                                (function
                                 (lambda (par)
                                   (and (not (conf-type->letterbox (cdr par)))
                                        (list (car par)))))
                                parlist))))
                      )))
    (cond
     ;;
     ;; Called from internal name to conf-no translator
     ;;
     ((eq all 'conf-no)
      (cond
       ((= (length mappedlist) 1)
        (car mappedlist))
       (t (let ((found nil))
            (while (and (not found) mappedlist)
              (if (string= string
                           (conf-stat->name (blocking-do 'get-conf-stat
                                                         (car mappedlist))))
                  (setq found (car mappedlist)))
              (setq mappedlist (cdr mappedlist)))
            (cond
             (found)
             ((string-match (lyskom-get-string 'person-or-conf-no-regexp)
                            string)
              (let* ((no (string-to-int (substring string
                                                   (match-beginning 1)
                                                   (match-end 1))))
                     (cs (blocking-do 'get-conf-stat no)))
                (if (lyskom-read-conf-name-internal-verify-type
                     cs predicate logins)
                    no))))))))
     ;;
     ;; FIXME
     ;;
     ;; Called from completing read. Should return t for exact match
     ;; and nil otherwise. This causes a problem when a string is an
     ;; exact match AND a prefix. In this case we return nil even
     ;; though that appears to be contrary to the documentation for
     ;; programmed completion.
     ;;
     ;; The problem is caused by Emacs expanding our string with
     ;; try-completion and then calling us back with all set to lambda
     ;; and the expanded string. What happened was that we'd enter
     ;; something that was a prefix but not a match which would expand
     ;; to a prefix that was also a match, which would be accepted!
     ;;
     ((eq all 'lambda)
      (= (length mappedlist) 1))
     
     ;;
     ;; Called from all-completions. Returns a list of all possible
     ;; completions.
     ;;
     (all
      (let ((names (mapcar (function (lambda (no)
                                       (conf-stat->name 
                                        (blocking-do 'get-conf-stat no))))
                           mappedlist)))
        (if (and (string-match (lyskom-get-string 'person-or-conf-no-regexp)
                               string)
                 (let* ((no (string-to-int (substring string
                                                      (match-beginning 1)
                                                      (match-end 1))))
                        (cs (blocking-do 'get-conf-stat no)))
                   (lyskom-read-conf-name-internal-verify-type cs 
                                                               predicate
                                                               logins)))
            (cons string names)
          names)))
	    
     ;;
     ;; No completions available on the string and we were called from
     ;; try-completion. Try to expand as a person or conference number
     ;; string instead of a real name.
     ;;

     ((= (length mappedlist) 0)
      (if (string-match (lyskom-get-string 'person-or-conf-no-regexp)
                        string)
          (let* ((no (string-to-int (substring string
                                               (match-beginning 1)
                                               (match-end 1))))
                 (cs (blocking-do 'get-conf-stat no)))
            (if (lyskom-read-conf-name-internal-verify-type
                 cs predicate logins)
                t))))

     ;;
     ;; FIXME
     ;;
     ;; Called from try-completion. Should return t if the string is
     ;; an exact match or the completion of the string. We return t if
     ;; the string is an exact match even if it is also a prefix. I'm
     ;; not sure this is the right thing to do, but it seems to
     ;; work...
     ;;
     (t                                 ; Some matches, maybe exact?
      (let ((strings (mapcar (function
                              (lambda (no)
                                (list (conf-stat->name 
                                       (blocking-do 'get-conf-stat no)))))
                             mappedlist))
            (found nil))

        (while (and (not found)
                    mappedlist)
          (if (string= string 
                       (conf-stat->name (blocking-do 'get-conf-stat 
                                                     (car mappedlist))))
              (setq found t))
          (setq mappedlist (cdr mappedlist)))

        (cond (found t)
              ((= (length strings) 1)
               (if (string= string (car (car strings)))
                   t                       ; Exact
                 (car (car strings))))
              (t (lyskom-try-complete-partials string strings))))))))


(defun lyskom-try-complete-partials (string alist)
  "Returns the longest string matching STRING.
Where every word matches the corresponding word in the car part of ALIST.
parst matching ([^)]) in string and alist are disgarded."
  (let* ((a-whitespace "\\([ \t]\\|([^)]*)\\)+")
	 (string (let ((initwhite (concat "\\`" a-whitespace)))
		   (if (string-match initwhite string)
		       (substring string (match-end 0))
		     string)))
	 (endfirstword (string-match a-whitespace string))
	 (firstword (substring string 0 endfirstword))
	 (reststring (and endfirstword
			  (substring string (match-end 0))))
	 (words (let ((res (try-completion firstword alist)))
		  (cond
		   ((eq res t) string)
		   (res)
		   (t string))))	;+++ Buggfix. Inget error om []\->{}|
	 (endfirstwords (string-match a-whitespace words))
	 (firstwords (substring words 0 endfirstwords))
	 (restlist (mapcar
		    (function
		     (lambda (part)
		       (cond
			((string-match a-whitespace
				       (car part))
			 (list (substring (car part) (match-end 0))))
			((list "")))))
		    alist)))
    (if	(= (length reststring) 0)
	words
      (concat (if (> (length firstwords) (length firstword))
		  firstwords
		firstword)
	      " " (lyskom-try-complete-partials reststring
						restlist)))))	 

					  


(defun lyskom-read-session-no (prompt &optional empty initial only-one)
  "Returns a list of session numbers of a session by reading either 
the number of the session or a name. 

The question is prompted with PROMPT.
If EMPTY is non-nil then the empty string is allowed (returns 0).
INITIAL is the initial contents of the input field.
If ONLY-ONE is non-nil only one session number will be returned."
  (lyskom-completing-clear-cache)
  (let (result data done)
    (while (not done)
      (setq data (lyskom-read-session-no-aux prompt t initial))
      (cond ((and (string= data "") (not empty)))
            ((string= data "") (setq done t result nil))
            (t (setq result
                     (lyskom-read-session-internal data 'logins 'session-no)
                     done t))))
    (if (and only-one (> (length result) 1))
        (setq result
              (lyskom-read-session-resolve-ambiguity result)))
    result))
             


(defun lyskom-read-session-resolve-ambiguity (sessions)
  (lyskom-insert "\n")
  (let* ((s-width (1+ (apply 'max (mapcar (function
					   (lambda (x)
					     (length (int-to-string x))))
					  sessions))))
	 (format-string-s (lyskom-info-line-format-string s-width "s" "s"))
	 (format-string-p (lyskom-info-line-format-string s-width "P" "M")))
    (lyskom-format-insert format-string-s
			  ""
			  (lyskom-get-string 'lyskom-name)
			  (lyskom-get-string 'is-in-conf))
    (lyskom-format-insert format-string-s
			  ""
			  (lyskom-get-string 'from-machine)
			  (lyskom-get-string 'is-doing))
    (lyskom-insert
     (concat (make-string (- (lyskom-window-width) 2) ?-)
	     "\n"))
    (let ((result nil)
	  (who-info
	   (mapcar (function
		    (lambda (el)
		      (let* ((info (blocking-do 'get-session-info el))
			     (confconfstat
			      (blocking-do 'get-conf-stat
					   (session-info->working-conf info))))
			(lyskom-format-insert
			 format-string-p
			 (format "%d%s"
				 (session-info->connection info)
				 (if (eq (session-info->connection info)
					 lyskom-session-no)
				     "*" " "))
			 (session-info->pers-no info)
			 (if (conf-stat->name confconfstat)
			     confconfstat
			   (lyskom-get-string 'not-present-anywhere)))
			(lyskom-format-insert
			 format-string-p
			 ""
			 (lyskom-return-username info)
			 (concat "("
				 (session-info->doing info)
				 ")"))
			(cons (number-to-string
			       (session-info->connection info))
			      info))))
		   (sort sessions '<))))
      (lyskom-insert (concat (make-string (- (lyskom-window-width) 2) ?-)
			     "\n"))
      (lyskom-insert (lyskom-format 'total-users (length who-info)))
      (lyskom-scroll)
      (while (string= ""
		      (setq result (completing-read
				    (lyskom-get-string 'resolve-session)
				    who-info
				    nil
				    t
				    (car (car who-info))
				    nil))))
      (list (session-info->connection (cdr (assoc result who-info)))))))
    


(defun lyskom-read-session-no-aux (prompt 
                                   &optional mustmatch 
                                   initial)
  "Read a LysKOM name or session number, prompting with PROMPT.
The third argument MUSTMATCH makes the function always return the conf-no and 
never the read string.
The fourth argument INITIAL is the initial contents of the input-buffer.

Returns the name."
  (lyskom-completing-clear-cache)
  (let* ((completion-ignore-case t)
	 ; When lyskom-read-conf-name-internal is called the current-buffer
	 ; is the minibuffer and the buffer-local variable lyskom-proc is not
	 ; correct. Then the variable lyskom-blocking-process must be set
	 ; instead. It is not buffer-local but scopes the let.
         (lyskom-blocking-process lyskom-proc)
         (minibuffer-local-completion-map 
          lyskom-minibuffer-local-completion-map)
         (minibuffer-local-must-match-map 
          lyskom-minibuffer-local-must-match-map))
    (completing-read prompt 
                     'lyskom-read-session-internal
                     'logins
                     mustmatch
                     initial
                     'lyskom-name-hist)))


(defun lyskom-read-session-internal (string predicate all)
  (let* ((result nil)
         (partial (lyskom-read-conf-name-internal string predicate all))
         (who-list (if (or (null partial)
                           (eq all 'session-no))
                       (mapcar (function 
                                (lambda (el)
                                  (cons 
                                   (number-to-string (who-info->connection el))
                                   el)))
                               (lyskom-completing-who-is-on))))
         (result (cond
                  ((and (null who-list)
                        (not (eq 'session-no all))) nil)
                  ((eq all nil)         ; try-completion
                   (try-completion string who-list nil))
                  ((eq all t)           ; all-completions
                   (all-completions string who-list nil))
                  ((eq all 'lambda)	; exact match
                   (and (assoc string who-list) t))
		  ((eq all 'session-no)	; get number
		   (car-safe (assoc string who-list))))))
    (cond ((eq all 'session-no)
           (if partial
               (let ((output nil)
                     (list who-list)
		     (num (string-to-number string))
                     (conf-no (lyskom-read-conf-name-internal string
                                                              predicate
                                                              'conf-no)))
                 (while list
                   (if (or (eq conf-no (who-info->pers-no (cdr (car list))))
			   (eq num (who-info->connection (cdr (car list)))))
                       (setq output (cons
                                     (who-info->connection (cdr (car list)))
                                     output)))
                   (setq list (cdr list)))
                 output)
             (list (string-to-number result))))
           (t (or partial result)))))
