;;;;;
;;;;; $Id: completing-read.el,v 41.4 1996-07-08 09:46:08 byers Exp $
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
;;;; Author: David Byers
;;;;
;;;; This file implements functions for reading a conference name
;;;; or a person name with completion and other help.
;;;;

(setq lyskom-clientversion-long 
      (concat
       lyskom-clientversion-long
       "$Id: completing-read.el,v 41.4 1996-07-08 09:46:08 byers Exp $\n"))

(defvar lyskom-name-hist nil)



;;; ============================================================
;;;
;;; Name lookup caches
;;;

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

;;; ============================================================
;;;
;;; Keymaps
;;;


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
  "Read a conference name from the minibuffer with completion and
return its number or zero if nothing was matched.

See lyskom-read-conf for a description of the parameters."
  (let ((conf-stat (lyskom-read-conf prompt type empty initial mustmatch)))
    (cond ((null conf-stat) 0)
          ((stringp conf-stat) 0)
          (t (conf-stat->conf-no conf-stat)))))

(defun lyskom-read-conf-stat (prompt type &optional empty initial mustmatch)
  "Read a conference name from the minibuffer with completion and
return its conf-stat or nil if nothing was matched.

See lyskom-read-conf for a description of the parameters."
  (let ((conf-stat (lyskom-read-conf prompt type empty initial mustmatch)))
    (cond ((null conf-stat) nil)
          ((stringp conf-stat) nil)
          (t conf-stat))))

(defun lyskom-read-conf-name (prompt type &optional empty initial mustmatch)
  "Read a conference name from the minibuffer with completion and
return its name.

See lyskom-read-conf for a description of the parameters."
  (let ((conf-stat (lyskom-read-conf prompt type empty initial mustmatch)))
    (cond ((null conf-stat) "")
          ((stringp conf-stat) conf-stat)
          (t (conf-stat->name conf-stat)))))

(defun lyskom-read-conf (prompt type &optional empty initial mustmatch)
  "Completing read a conference or person from the minibuffer. 

PROMPT is the prompt type type.
TYPE   is the type of conferences to return. It is a list of one or
more of the following:
    all     Return any conference,
    conf    Return conferences (not letterboxes),
    pers    Return persons (letterboxes),
    login   Return persons who are also logged-in, and
    none    Return names that do not match anything in the database.
Optional arguments
EMPTY     allow nothing to be entered.
INITIAL   initial contents of the minibuffer
MUSTMATCH if non-nil, the user must enter a valid name.

The return value may be one of
A conf-stat: The conf-stat associated with the name entered,
nil:         Nothing was entered, or
A string:    A name that matched nothing in the database."

  (lyskom-completing-clear-cache)
  (let* ((completion-ignore-case t)
         (lyskom-blocking-process lyskom-proc)
         (minibuffer-local-completion-map 
          lyskom-minibuffer-local-completion-map)
         (minibuffer-local-must-match-map 
          lyskom-minibuffer-local-must-match-map)
         (read-string nil)
         (result nil)
         (keep-going t))

    (while keep-going
      (setq read-string (completing-read prompt
                                         'lyskom-read-conf-internal
                                         type
                                         mustmatch
                                         initial
                                         'lyskom-name-hist))
      (setq result
            (cond ((null read-string) nil)
                  ((string= "" read-string) nil)
                  (t (lyskom-lookup-conf-by-name read-string type))))
      (setq keep-going (and (not empty)
                            (null result))))
    result))


(defun lyskom-read-conf-get-logins ()
  "Used internally by lyskom-read-conf-internal to get a list of
persons who are logged on."
  (mapcar (function (lambda (el) (who-info->pers-no el)))
          (lyskom-completing-who-is-on)))


(defun lyskom-read-conf-expand-specials (string predicate login-list x-list)
  "Used internally by lyskom-read-conf-internal to expand person and
conference number specifications to something useful."
  (cond ((string-match (lyskom-get-string 'person-or-conf-no-regexp) string)
         (let* ((no (string-to-int (match-string 1 string)))
                (cs (blocking-do 'get-conf-stat no)))
           (if (and cs
                    (lyskom-read-conf-internal-verify-type
                     (conf-stat->conf-no cs)
                     (conf-stat->conf-type cs)
                     predicate 
                     login-list
                     x-list))
               (list string))))))

(defun lyskom-read-conf-lookup-specials (string predicate login-list x-list)
  "Used internally by lyskom-read-conf-internal to look up conf-stats
from person and conference number specifications."
  (if (string-match (lyskom-get-string 'person-or-conf-no-regexp)
                    string)
      (progn
        (let* ((no (string-to-int (match-string 1 string)))
               (cs (blocking-do 'get-conf-stat no)))
          (if (and cs
                   (lyskom-read-conf-internal-verify-type
                    (conf-stat->conf-no cs)
                    (conf-stat->conf-type cs)
                    predicate 
                    login-list
                    x-list))
              cs)))))

(defun lyskom-lookup-conf-by-name (string predicate)
  "Return the conf-stat associated with STRING that also satisfies
PREDICATE or nil if no name matches. See lyskom-read-conf-internal for
a documentation of PREDICATE."
  (let ((lyskom-blocking-process (or lyskom-blocking-process
                                     lyskom-proc)))
    (lyskom-read-conf-internal string predicate 'lyskom-lookup)))


(defun lyskom-read-conf-internal (string predicate all)
  "Complete the name STRING according to PREDICATE and ALL.

STRING is a string to complete.
PREDICATE is a list of name types to return. Valid types are
    all     Any existing name may be returned,
    pers    Names of persons may be returned,
    conf    Names of conferences may be returned,
    login   Names of logged-in persons may be returned, and
    none    Names that match nothing may be returned.
ALL is set by try-completion and all-completions. See the Emacs lisp
manual for a description. Special value 'lyskom-lookup makes the
function work as a name-to-conf-stat translator."
  (let* ((login-list (and (memq 'login predicate)
                          (lyskom-read-conf-get-logins)))
         (x-list (lyskom-completing-lookup-name string))
         (candidate-list (and x-list
                              (listify-vector (conf-list->conf-nos x-list))))
         (candidate-type-list 
          (and x-list (listify-vector (conf-list->conf-types x-list))))
         (result-list nil)
         (get-conf-stat-done nil))

    ;;
    ;;  login-list now contains a list of logins, IF the predicate
    ;;  includes 'login
    ;;
    ;;  candidate-list contains a list of conf-nos, with the
    ;;  corresponding conf-types in candidate-type-list.
    ;;
    ;;  Now set result-list to the conf-stats that fulfill the
    ;;  predicate, fetching the conf-stats asynchronously.
    ;;

    (setq get-conf-stat-done nil)

    (save-excursion
      (set-buffer (process-buffer lyskom-blocking-process))
      (while candidate-list
        (if (lyskom-read-conf-internal-verify-type (car candidate-list)
                                                   (car candidate-type-list)
                                                   predicate
                                                   login-list
                                                   x-list)
            (initiate-get-conf-stat 'complete
                                    (function (lambda (x)
                                                (setq result-list
                                                      (cons x result-list)))) 
                                    (car candidate-list)))
        (setq candidate-list (cdr candidate-list)
              candidate-type-list (cdr candidate-type-list)))
      
      ;;
      ;;  Wait for the conf-stats to arrive
      ;;

      (lyskom-wait-queue 'complete))

    ;;
    ;;  Now the matching conf-stats are in result-list
    ;;

    (cond 

     ((eq all 'lyskom-lookup)
      (let ((specials (lyskom-read-conf-expand-specials string
                                                        predicate
                                                        login-list
                                                        x-list)))

        (cond ((>= (length result-list) 1)
               (car result-list))
              (specials (lyskom-read-conf-lookup-specials string
                                                          predicate
                                                          login-list
                                                          x-list))
              ((string-match (lyskom-get-string 'person-or-conf-no-regexp)
                                                string) nil)
              ((lyskom-read-conf-internal-verify-type nil
                                                      nil
                                                      predicate
                                                      login-list
                                                      x-list)
               string))))
     
     ;;
     ;;  Check for exact match. We have an exact match in the server
     ;;  when there was a single match OR when there was no match, and
     ;;  no match is valid according to predicate
     ;;

     ((eq all 'lambda)
      (let ((specials (lyskom-read-conf-expand-specials string
                                                        predicate
                                                        login-list
                                                        x-list)))
        (cond ((= (length result-list) 1) t)
              (result-list nil)
              ((= (length specials) 1) t)
              (specials nil)
              ((string-match (lyskom-get-string 'person-or-conf-no-regexp)
                             string) nil)
              (t (lyskom-read-conf-internal-verify-type nil
                                                        nil
                                                        predicate
                                                        login-list
                                                        x-list)))))


     ;;
     ;;  Called from all-completions. Return a list of all possible
     ;;  completions, in this case all names in the result list plus,
     ;;  if the input string is a person or conf number specification,
     ;;  the input string, PROVIDED, the requested conference matches
     ;;  the predicate. If there were no matches, return the input
     ;;  string if no matches satisfies the predicate.
     ;;
          
     (all
      (let ((names (mapcar 'conf-stat->name result-list))
            (specials (lyskom-read-conf-expand-specials string
                                                        predicate
                                                        login-list
                                                        x-list)))
        (cond (specials (append specials names))
              (names names)
              ((string-match (lyskom-get-string 'person-or-conf-no-regexp)
                                                string) nil)
              ((lyskom-read-conf-internal-verify-type nil
                                                      nil
                                                      predicate
                                                      login-list
                                                      x-list)
               (list string))
              (t nil))))

     ;;
     ;;  Called from try-completion, and there were no matches. Try to
     ;;  expand the input string as a person or conf number
     ;;  specification or return something sensible if the predicate
     ;;  is satisfied by no matches.
     ;;

     ((null result-list)
      (let ((specials (lyskom-read-conf-expand-specials string
                                                        predicate
                                                        login-list
                                                        x-list)))
        (cond (specials specials)
              ((string-match (lyskom-get-string 'person-or-conf-no-regexp)
                             string) nil)
              ((lyskom-read-conf-internal-verify-type nil
                                                      nil
                                                      predicate
                                                      login-list
                                                      x-list)
               (list string))
              (t nil))))

     ;;
     ;;  Called from try-completion, and there were matches in the
     ;;  server. Return t if the string is an exact match to any
     ;;  string returned from the server. Otherwise, expand the string
     ;;  as far as possible and return that
     ;;

     (t
      (let ((name-list (mapcar 'conf-stat->name result-list))
            (specials (lyskom-read-conf-expand-specials string
                                                        predicate
                                                        login-list
                                                        x-list))
            (found nil))
        (if specials (setq name-list (nconc specials name-list)))

        (cond ((member string name-list) t) ; Exact match
              ((= (length name-list) 1) (car name-list))
              ((string-match (lyskom-get-string 'person-or-conf-no-regexp)
                             string) nil)
              (t (or (lyskom-complete-string string name-list)
                     (and (lyskom-read-conf-internal-verify-type nil
                                                                 nil
                                                                 predicate
                                                                 login-list
                                                                 x-list)
                          (list string))))))))))




(defun lyskom-complete-collect (_fn_ _seq_ &optional _initial_)
  "Apply FN to all elements in SEQ in sequence. FN takes two arguments,
a sequence element and the return value of the last invocation. 
lyskom-complete-collect returns the last value returned by FN. Optional
argument INITIAL is the initial value to give FN."
  (let ((_result_ _initial_)
        (_size_ (length _seq_))
        (_position_ 0))
    (while (< _position_ _size_)
      (setq _result_ (funcall _fn_ (elt _seq_ _position_) _result_))
      (setq _position_ (1+ _position_)))
    _result_))


(defun lyskom-read-conf-internal-verify-type (conf-no
                                              conf-type
                                              predicate
                                              logins
                                              x-list)
  (or (and (memq 'all predicate)
           conf-no)
      (and (memq 'conf predicate)
           (not (conf-type->letterbox conf-type)))
      (and (memq 'pers predicate) 
           (conf-type->letterbox conf-type))
      (and (memq 'login predicate)
           (memq conf-no logins))
      (and (memq 'none predicate) 
           (and (null conf-no)
                (null (conf-list->conf-nos x-list))))))



(defun lyskom-complete-string (string string-list)
  "Return the longest string that matches all the strings in
string-list according to the LysKOM Rules of Engagement. Sorry, Name
Comparison. STRING is the string the user entered. It is ignored at
the moment. STRING-LIST is a list of strings that match the user's
string."

  (if (null (cdr-safe string-list))
      (or (car-safe string-list) "")

    (let ((result nil)
          (start 0)
          (paren-depth 0)
          (position 0)
          (data-list (mapcar (function (lambda (el) (vector 0 0 el))) 
                             string-list))
          (last-match nil)
          (match ?\ )
          (keep-going t))

      (while keep-going

        ;;
        ;; Save the last character matched
        ;;

        (setq last-match match)

        ;;
        ;; Check how the next character matches. A nil means one of
        ;; the strings is exhausted and we should stop. A number means
        ;; that the heads of all strings was the same; the number is
        ;; the character code of the head and we can accumulate this
        ;; into the result. 'open-paren means that the heads did not
        ;; match, but at least one of them was an open-paren
        ;; character; we may be able to skip the parenthesized
        ;; expression and get a match. t means that the heads were
        ;; regular characters but did not all match.
        ;;

        (setq match
              (lyskom-complete-collect
               (function (lambda (el data)
                           (prog1
                               (cond ((null data) nil)
                                     ((>= (aref el 1) 
                                          (length (aref el 2))) nil)
                                     ((eq data 'start)
                                      (downcase (elt (aref el 2)
                                                     (aref el 1))))
                                     ((numberp data)
                                      (if (eq (downcase 
                                               (elt (aref el 2)
                                                    (aref el 1))) data)
                                          data
                                        (if (or (eq data ?\()
                                                (eq (elt (aref el 2)
                                                         (aref el 1)) ?\())
                                            'open-paren
                                          t)))
                                     (t data))
                             (aset el  1 (1+ (aref el 1))))))
                  
               data-list
               'start))
        (cond 

         ;;
         ;; One of the strings is exhausted. If we are not in a
         ;; parenthesized expression, output any pending matches and
         ;; stop the loop.
         ;;

         ((null match) 
          (if (or (= paren-depth 0) 
                  (and (eq last-match ?\))
                       (= paren-depth 1)))
              (if (/= (aref (car data-list) 0)
                      (1- (aref (car data-list) 1)))
                  (setq result (cons (substring (aref (car data-list) 2)
                                                (aref (car data-list) 0)
                                                (1- (aref (car data-list) 1)))
                                     result))))
          (setq keep-going nil))

         ;;
         ;; We hit a word separator. If the last match was a
         ;; close-paren, we have exited a parenthesized expression.
         ;; Note that this does NOT happen when we match the
         ;; close-paren itself! If this leaves us at paren depth zero,
         ;; output the matched string. If not, just keep going.
         ;;

         ((and (/= paren-depth 0)
               (memq match '(?\t ?\n ?\r ?\ )))
          (if (eq last-match ?\))
              (setq paren-depth (1- paren-depth)))

          (if (and (= paren-depth 0)
                   (/= (aref (car data-list) 0)
                       (1- (aref (car data-list) 1))))
              (progn
                (setq result (cons (substring (aref (car data-list) 2)
                                              (aref (car data-list) 0)
                                              (1- (aref (car data-list) 1)))
                                   result))
                (setq match (lyskom-complete-string-next-word data-list))
                (lyskom-complete-string-reset-position data-list))))

         ;;
         ;; We hit a word separator outside a parenthesized
         ;; expression. In this case just output the matched word and
         ;; skip all strings forward to the next word start.
         ;;

         ((and (= paren-depth 0)
               (memq match '(?\t ?\n ?\r ?\ )))
          (if (/= (aref (car data-list) 0)
                  (1- (aref (car data-list) 1)))
              (setq result (cons (substring (aref (car data-list) 2)
                                            (aref (car data-list) 0)
                                            (1- (aref (car data-list) 1)))
                                 result)))
          (setq match (lyskom-complete-string-next-word data-list))
          (lyskom-complete-string-reset-position data-list))
          
         ;;
         ;; We hit an open-parenthesis at the start of a word.
         ;; Increase paren-depth and keep going.
         ;;

         ((and (eq match ?\()
               (memq last-match '(?\t ?\n ?\r ?\ )))
          (setq paren-depth (1+ paren-depth)))
       

         ;;
         ;; Characters matched and were not specials. The heads of all
         ;; strings matched. Just keep going.
         ;;

         ((numberp match))

         ;;
         ;; Characters did not match and were in a parenthesized
         ;; expression. In this case, scan forward until we leave all
         ;; the parenthesized expressions we've entered and then scan
         ;; forward to the start of the next word.
         ;;
      
         ((/= paren-depth 0)
          (while (/= paren-depth 0)
            (lyskom-complete-string-up-level data-list)
            (setq paren-depth (1- paren-depth)))
          (setq match (lyskom-complete-string-next-word data-list))
          (lyskom-complete-string-reset-position data-list))

         ;;
         ;; Characters did not match, one of them was a open paren and
         ;; we're at the beginning of a new word. Skip past all
         ;; parenthesized expressions at the heads of the strings and
         ;; try again.
         ;;

         ((and (eq match 'open-paren)
               (memq last-match '(?\t ?\n ?\r ?\ )))
          (lyskom-complete-string-skip-parens-backup-others data-list)
          (setq match (lyskom-complete-string-next-word data-list))
          (lyskom-complete-string-reset-position data-list))

         ;;
         ;; Characters did not match and were not in a parenthesized
         ;; expression. Abort if at the start of a word, otherwise
         ;; skip on to the next word.
         ;;

         (t 
          (if (/= (aref (car data-list) 0)
                  (1- (aref (car data-list) 1)))          
              (setq result (cons (substring (aref (car data-list) 2)
                                            (aref (car data-list) 0)
                                            (1- (aref (car data-list) 1)))
                                 result)))
          (if (memq last-match '(?\t ?\n ?\r ?\ ))
              (setq keep-going nil)
            (progn
              (setq match (lyskom-complete-string-next-word data-list))
              (lyskom-complete-string-reset-position data-list))))))

      (mapconcat 'identity (nreverse result) " "))))


(defun lyskom-complete-string-up-level (data-list)
  "Scan up one parenthesis level in all strings in DATA-LIST."
  (let ((position 0)
        (size (length data-list))
        (el nil))
    (while (< position size)
      (setq el (elt data-list position))
      (aset el 1 (1- (aref el 1)))      ; Fool lyskom-complete-string-skip-parens-2
      (lyskom-complete-string-skip-parens-2 el)
      (setq position (1+ position)))))


(defun lyskom-complete-string-skip-parens-backup-others (data-list)

  "Scan past the leading parenthesized expressions at heads of strings
in DATA-LIST. If the string does not start with a parenthesized
expression, back up one character. This operation is used to skip
parentheses when a mismatch is found where one of the strings starts
with a parenthesis."
  (let ((position 0)
        (size (length data-list))
        (el nil))
    (while (< position size)
      (setq el (elt data-list position))
      (if (eq (elt (aref el 2) (1- (aref el 1))) ?\()
          (lyskom-complete-string-skip-parens-2 el)
        (aset el 1 (max 0 (1- (aref el 1)))))
      (setq position (1+ position)))))

(defun lyskom-complete-string-skip-parens-2 (el)
  "Skip past the next parenthesized expression in data list element
EL. Used internally by lyskom-complete-string-up-level and
lyskom-complete-string-skip-parens-backup-others."
  (let* ((position (aref el 1))
         (string (aref el 2))
         (size (length string))
         (paren-depth 1)
         (cur ?\()
         (last nil))
    (while (and (< position size)
                (> paren-depth 0))
      (setq last cur)
      (setq cur (elt string position))
      (cond ((and (eq cur ?\() (eq last ?\ )) (setq paren-depth 
                                                    (1+ paren-depth)))
            ((and (eq cur ?\ ) (eq last ?\))) (setq paren-depth
                                                    (1- paren-depth)))
            (t nil))
      (setq position (1+ position)))
    (aset el 1 position)))



(defun lyskom-complete-string-next-word (data-list)
  "Skip to the next word in all strings in DATA-LIST unless at the
start of a word already."
  (let ((position 0)
        (size (length data-list))
        (el nil))
    (while (< position size)
      (setq el (elt data-list position))
      (cond ((string-match "\\s-+" (aref el 2) (1- (aref el 1)))
             (aset el 1 (match-end 0)))
            ((= 0 (aref el 1)))
            (t (aset el 1 (length (aref el 2)))))
      (setq position (1+ position))))
  ?\ )

(defun lyskom-complete-string-reset-position (data-list)
  "Set starting positions to current positions in all elements of DATA-LIST"
  (let ((position 0)
        (size (length data-list))
        (el nil))
    (while (< position size)
      (setq el (elt data-list position))
      (aset el 0 (aref el 1))
      (setq position (1+ position)))))




;;; ============================================================
;;;
;;; Session reading
;;;
;;;



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
                     (lyskom-read-session-internal data '(login)
                                                   'lyskom-lookup)
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
                     '(login)
                     mustmatch
                     initial
                     'lyskom-name-hist)))


(defun lyskom-read-session-internal (string predicate all)
  (let* ((result nil)
         (partial (lyskom-read-conf-internal string predicate all))
         (who-list (if (or (null partial)
                           (eq all 'lyskom-lookup))
                       (mapcar (function 
                                (lambda (el)
                                  (cons 
                                   (number-to-string (who-info->connection el))
                                   el)))
                               (lyskom-completing-who-is-on))))
         (result (cond
                  ((and (null who-list)
                        (not (eq 'lyskom-lookup all))) nil)
                  ((eq all nil)         ; try-completion
                   (try-completion string who-list nil))
                  ((eq all t)           ; all-completions
                   (all-completions string who-list nil))
                  ((eq all 'lambda)	; exact match
                   (and (assoc string who-list) t))
		  ((eq all 'lyskom-lookup)	; get number
		   (car-safe (assoc string who-list))))))
    (cond ((eq all 'lyskom-lookup)
           (if partial
               (let* ((output nil)
                      (list who-list)
                      (num (string-to-number string))
                      (conf-no 
                       (if (= 0 num)  ; Dont lookup unless necessary
                           (conf-stat->conf-no 
                            (lyskom-read-conf-internal string
                                                       predicate
                                                       'lyskom-lookup)))))
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
