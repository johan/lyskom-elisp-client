;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: completing-read.el,v 44.58 2009-03-08 12:20:12 byers Exp $
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
;;;; File: completing-read.el
;;;; Author: David Byers
;;;;
;;;; This file implements functions for reading a conference name
;;;; or a person name with completion and other help.
;;;;

(setq lyskom-clientversion-long 
      (concat
       lyskom-clientversion-long
       "$Id: completing-read.el,v 44.58 2009-03-08 12:20:12 byers Exp $\n"))

(defvar lyskom-name-hist nil)


;;; ============================================================
;;;
;;; Name lookup caches
;;;

(defvar lyskom-completing-who-info-cache nil
  "Temporary cache of who-info data")

(defvar lyskom-completing-lookup-name-cache nil
  "Temporary cache of server queries")

(defvar lyskom-completing-use-dynamic-info nil)

(defun lyskom-completing-clear-cache ()
  (setq lyskom-completing-who-info-cache nil)
  (setq lyskom-completing-lookup-name-cache nil))

(defun lyskom-completing-who-is-on ()
  "Get information about who is on, first checking the cache. Returns what 
\(blocking-do 'who-is-on\) would, but as a list, not a vector"
  (if lyskom-completing-who-info-cache
      lyskom-completing-who-info-cache
    (setq lyskom-completing-who-info-cache
          (listify-vector
	   (if (lyskom-have-feature dynamic-session-info)
	       (blocking-do 'who-is-on-dynamic t t 0)
	     (blocking-do 'who-is-on))))))

(defun lyskom-completing-cache-completion (string data)
  (let* ((downs (lyskom-unicase string))
         (tmp (assoc downs lyskom-completing-lookup-name-cache)))
    (if (null tmp)
        (setq lyskom-completing-lookup-name-cache
              (cons (cons downs data) lyskom-completing-lookup-name-cache)))
    string))

(defun lyskom-completing-lookup-z-name (string want-conf want-pers)
  "Look up STRING as a name. Same as \(blocking-do 'lookup-z-name ...\)
but first checks a cache."
  (if (and (eq 0 want-conf)
           (eq 0 want-pers))
      nil
    (let* ((downs (lyskom-unicase string))
           (tmp (assoc downs lyskom-completing-lookup-name-cache)))
      (if tmp
          (cdr tmp)
        (progn
          (setq tmp (blocking-do 'lookup-z-name string want-pers want-conf))
          (setq lyskom-completing-lookup-name-cache
                (cons (cons downs tmp)
                      lyskom-completing-lookup-name-cache))
          tmp)))))

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
    (lyskom-xemacs-or-gnu 
     (lyskom-set-keymap-parent map lyskom-minibuffer-local-completion-map)
     (define-key map " " nil))
    map)
  "Keymap used for reading LysKOM names.")



(defsubst lyskom-completing-match-string-regexp (string)
  (concat "^"
          (replace-in-string (regexp-quote (lyskom-unicase (lyskom-completing-strip-name string)))
                             "\\s-+" "\\\\S-*\\\\s-+")
          "\\s-*"))

(defsubst lyskom-completing-match-string (string name)
  "Return non-nil if STRING matches NAME using LysKOM completion rules."
  (string-match (lyskom-completing-match-string-regexp string)
                (lyskom-completing-strip-name (lyskom-unicase name))))


(defun lyskom-read-conf-no (prompt type &optional empty initial mustmatch)
  "Read a conference name from the minibuffer with completion and
return its number or zero if nothing was matched.

See lyskom-read-conf for a description of the parameters."
  (let ((conf-z-info (lyskom-read-conf prompt type empty initial mustmatch)))
    (cond ((null conf-z-info) 0)
          ((stringp conf-z-info) 0)
	  ((lyskom-conf-stat-p conf-z-info) (conf-stat->conf-no conf-z-info))
	  ((lyskom-uconf-stat-p conf-z-info) (uconf-stat->conf-no conf-z-info))
          (t (conf-z-info->conf-no conf-z-info)))))

(defun lyskom-read-conf-stat (prompt type &optional empty initial mustmatch)
  "Read a conference name from the minibuffer with completion and
return its conf-stat or nil if nothing was matched.

See lyskom-read-conf for a description of the parameters."
  (let ((conf-z-info (lyskom-read-conf prompt type empty initial mustmatch)))
    (cond ((null conf-z-info) nil)
          ((stringp conf-z-info) nil)
	  ((lyskom-conf-stat-p conf-z-info) conf-z-info)
          ((lyskom-uconf-stat-p conf-z-info) 
           (blocking-do 'get-conf-stat (uconf-stat->conf-no conf-z-info)))
          (t (blocking-do 'get-conf-stat 
                          (conf-z-info->conf-no conf-z-info))))))

(defun lyskom-read-uconf-stat (prompt type &optional empty initial mustmatch)
  "Read a conference name from the minibuffer with completion and
return its conf-stat or nil if nothing was matched.

See lyskom-read-conf for a description of the parameters."
  (let ((conf-z-info (lyskom-read-conf prompt type empty initial mustmatch)))
    (cond ((null conf-z-info) nil)
          ((stringp conf-z-info) nil)
	  ((lyskom-uconf-stat-p conf-z-info) conf-z-info)
	  ((lyskom-conf-stat-p conf-z-info)
           (blocking-do 'get-uconf-stat 
                        (conf-stat->conf-no conf-z-info)))
          (t (blocking-do 'get-uconf-stat 
                          (conf-z-info->conf-no conf-z-info))))))

(defun lyskom-read-conf-name (prompt type &optional empty initial mustmatch)
  "Read a conference name from the minibuffer with completion and
return its name.

See lyskom-read-conf for a description of the parameters."
  (let ((conf-z-info (lyskom-read-conf prompt type empty initial mustmatch)))
    (cond ((null conf-z-info) "")
          ((stringp conf-z-info) conf-z-info)
	  ((lyskom-conf-stat-p conf-z-info) (conf-stat->name conf-z-info))
	  ((lyskom-uconf-stat-p conf-z-info) (uconf-stat->name conf-z-info))
	  (t (conf-z-info->name conf-z-info)))))


;;; ================================================================
;;; Code to guess defaults for initial input
;;;

(defun lyskom-default-conference-at-point (&rest args)
  (let* ((pos (or lyskom-command-point (point)))
         (type (and pos (get-text-property pos 'lyskom-button-type))))
    (and (memq type '(conf pers))
         (list (get-text-property pos 'lyskom-button-arg)))))

(defun lyskom-default-conference-current (&rest args)
  (list lyskom-current-conf))

(defun lyskom-default-conference-self (&rest args)
  (list lyskom-pers-no))

(defun lyskom-default-conference-last-author (&rest args)
  (and (lyskom-get-last-read-text)
       (list (text-stat->author
              (blocking-do 'get-text-stat (lyskom-get-last-read-text))))))

(defun lyskom-default-conference-author-of-text-at-point (&rest args)
  (let ((text (lyskom-text-at-point)))
    (and text 
	 (list (text-stat->author (blocking-do 'get-text-stat text))))))

(defun lyskom-default-conference-restriction (predicate &rest args)
  (and (assq 'restrict predicate)
       (cdr (assq 'restrict predicate))))

(defun lyskom-default-conference-empty (&rest args)
  nil)


(defun lyskom-default-conference-saved (sym &rest args)
  (save-excursion
    (when lyskom-buffer (set-buffer lyskom-buffer))
    (and (cdr (assq (car sym) lyskom-read-conf-saved-inputs))
         (list (conf-z-info->conf-no 
                (cdr (assq (car sym) lyskom-read-conf-saved-inputs)))))))

(defun lyskom-default-conference-not-self (uc &rest args)
  (not (eq (uconf-stat->conf-no uc) lyskom-pers-no)))

(defun lyskom-default-conference-not-current (uc &rest args)
  (not (eq (uconf-stat->conf-no uc) lyskom-current-conf)))



(defun lyskom-get-initial-conf-strategy (prompt)
  (when (listp prompt) (setq prompt (car prompt)))
  (let* ((spec-1 (cdr (assq (or lyskom-current-command this-command)
                            lyskom-default-conference-strategy)))
         (default-spec 
           (cdr (assq t (assq t lyskom-default-conference-strategy))))
         (prompt-spec (cdr (assq prompt spec-1)))
         (cmd-spec (cdr (assq t spec-1))))
    (lyskom-debug-forms
     (unless spec-1 
       (save-excursion
         (when lyskom-buffer (set-buffer lyskom-buffer))
         (lyskom-format-insert-before-prompt 
          "%[%#2@%#1s%]\n"
          (format "Warning: no strategy for %S/%S"
                  (or lyskom-current-command this-command)
                  prompt)
          `(face ,kom-warning-face)))))
    (list (or (assq 'default prompt-spec)
              (assq 'default cmd-spec)
              (assq 'default default-spec))
          (or (assq 'filter prompt-spec)
              (assq 'filter cmd-spec)
              (assq 'filter default-spec))
          (or (assq 'save prompt-spec)
              (assq 'save cmd-spec)
              (assq 'save default-spec)))))

(defun lyskom-read-conf-guess-initial (prompt predicate)
  "Return a guess for the initial value for lyskom-read-conf."
  (let* ((strategy (lyskom-get-initial-conf-strategy prompt))
         (default (cdr (assq 'default strategy)))
         (filter (cdr (assq 'filter strategy))))

    (uconf-stat->name
     (car
      (lyskom-filter-list 
       (lambda (uconf-stat)
         (and uconf-stat
              (not (memq nil
                         (mapcar (lambda (fn)
                                   (funcall fn uconf-stat))
                                 filter)))
              (lyskom-read-conf-internal-verify-type
               (uconf-stat->conf-no uconf-stat)
               (uconf-stat->conf-type uconf-stat)
               predicate nil nil)))
       (mapcar (lambda (conf-no)
                 (blocking-do 'get-uconf-stat conf-no)) 
               (apply 'append 
                      (delq nil (mapcar (lambda (fn)
                                          (if (listp fn)
                                              (funcall (car fn) (cdr fn) predicate)
                                            (funcall fn predicate)))
                                        default)))))))))



(defun lyskom-read-conf-save-input (prompt input)
  "Save INPUT as input for the current completing read command."
  (save-excursion
    (when lyskom-buffer (set-buffer lyskom-buffer))
    (lyskom-traverse sym (cdr (assq 'save (lyskom-get-initial-conf-strategy prompt)))
      (if (assq sym lyskom-read-conf-saved-inputs)
          (setcdr (assq sym lyskom-read-conf-saved-inputs) input)
        (setq lyskom-read-conf-saved-inputs 
              (cons (cons sym input) lyskom-read-conf-saved-inputs))
        ))))

(defun lyskom-read-conf (prompt type &optional empty initial mustmatch)
  "Completing read a conference or person from the minibuffer. 

PROMPT is the prompt. It can be a string, symbol or list. If it is a symbol,
       lyskom-get-string will be used to get the string. If it is a list,
       it is used as the argument list to lyskom-format to create the
       prompt. You should use symbols and lists only.
TYPE   is the type of conferences to return. It is a list of one or
       more of the following:

       all     Return any conference.
       conf    Return conferences (not letterboxes).
       pers    Return persons (letterboxes).
       login   Return persons who are also logged-in.
       membership Return only conferences and letterboxes lyskom-pers-no
               is a member of.
       none    Return names that do not match anything in the database.
       (restrict c1 c2 ...) Restrict matching to conference numbers c1, 
               c2 etc. The implementation is inefficient for long lists.

Optional arguments

EMPTY     allow nothing to be entered.

INITIAL   initial contents of the minibuffer. If an integer, use the
          name of that conference. This is normally computed automatically,
          so pass nil whenever possible.

MUSTMATCH if non-nil, the user must enter a valid name.

The return value may be one of
A conf-z-info: The conf-z-info associated with the name entered,
nil:         Nothing was entered, or
A string:    A name that matched nothing in the database."

  ;; We bind lyskom-current-command here so that changes to
  ;; this-command will not confuse our guesses for the initial value
  ;; (particularly at the end of this function, where we sometimes
  ;; save the value the user entered).

  (let ((lyskom-current-command (or lyskom-current-command this-command)))
    (lyskom-completing-clear-cache)
    (setq initial
          (cond ((integerp initial)
                 (uconf-stat->name (blocking-do 'get-uconf-stat initial)))
                ((stringp initial) initial)
                ((lyskom-conf-stat-p initial)
                 (conf-stat->name initial))
                ((lyskom-uconf-stat-p initial)
                 (uconf-stat->name initial))
                ((lyskom-conf-z-info-p initial)
                 (conf-z-info->name initial))
                ((consp initial) initial)
                ((lyskom-read-conf-guess-initial prompt type))
                (t nil)))

    (let* ((completion-ignore-case t)
           (minibuffer-local-completion-map 
            lyskom-minibuffer-local-completion-map)
           (minibuffer-local-must-match-map 
            lyskom-minibuffer-local-must-match-map)
           (read-string nil)
           (old-prompt prompt)
           (result nil)
           (keep-going t))

      (setq prompt (cond ((stringp prompt) prompt)
                         ((symbolp prompt) (lyskom-get-string prompt))
                         ((listp prompt) (apply 'lyskom-format prompt))
                         (t (lyskom-get-string 'conf-prompt))))

      (while keep-going
        (setq read-string (lyskom-completing-read prompt
                                                  'lyskom-read-conf-internal
                                                  type
                                                  mustmatch
                                                  (if (listp initial)
                                                      initial
                                                    (cons initial 0))
                                                  'lyskom-name-hist))
        (setq result
              (cond ((null read-string) nil)
                    ((string= "" read-string) nil)
                    (t (lyskom-lookup-conf-by-name read-string type))))
        (setq keep-going (and (not empty)
                              (null result))))
      (lyskom-read-conf-save-input old-prompt result)
      result)))


(defun lyskom-read-conf-get-logins ()
  "Used internally by lyskom-read-conf-internal to get a list of
persons who are logged on."
  (mapcar (if (lyskom-have-feature dynamic-session-info)
              (function (lambda (el) (dynamic-session-info->person el)))
            (function (lambda (el) (who-info->pers-no el))))
          (lyskom-completing-who-is-on)))


(defun lyskom-read-conf-expand-specials (string
                                         predicate
                                         login-list
                                         x-list
                                         &optional return-cs)
  "Used internally by lyskom-read-conf-internal to expand person and
conference number specifications to something useful."
  (cond ((string-match (lyskom-get-string 'person-or-conf-no-regexp) string)
         (let* ((no (lyskom-string-to-number (match-string 1 string)))
                (cs (blocking-do 'get-uconf-stat no)))
           (if (and cs
                    (lyskom-read-conf-internal-verify-type
                     (uconf-stat->conf-no cs)
                     (uconf-stat->conf-type cs)
                     predicate 
                     login-list
                     x-list))
               (if return-cs
                   cs
                 (list string)))))
        ((string-match (lyskom-get-string 'session-no-regexp) string)
         (let* ((no (lyskom-string-to-number (match-string 1 string)))
                (si (blocking-do 'get-session-info no))
                (cs (and si
                         (blocking-do 'get-uconf-stat
                                      (session-info->pers-no si)))))
           (if (and cs
                    (lyskom-read-conf-internal-verify-type
                     (uconf-stat->conf-no cs)
                     (uconf-stat->conf-type cs)
                     predicate 
                     login-list
                     x-list))
               (if return-cs
                   cs
                 (list string)))))))

(defun lyskom-read-conf-lookup-specials (string predicate login-list x-list)
  "Used internally by lyskom-read-conf-internal to look up conf-stats
from person and conference number specifications."
  (let ((cs (lyskom-read-conf-expand-specials string
                                              predicate
                                              login-list
                                              x-list
                                              t)))
    (lyskom-create-conf-z-info (uconf-stat->name cs)
                               (uconf-stat->conf-type cs)
                               (uconf-stat->conf-no cs))))

(defun lyskom-lookup-conf-by-name (string predicate)
  "Return the conf-z-info associated with STRING that also satisfies
PREDICATE or nil if no name matches. See lyskom-read-conf-internal for
a documentation of PREDICATE."
  (if (string= string "")
      nil
    (lyskom-read-conf-internal string predicate 'lyskom-lookup)))


(defun lyskom-read-conf-internal (string predicate all)
  "Complete the name STRING according to PREDICATE and ALL.

STRING is a string to complete.
PREDICATE is a list of name types to return. See lyskom-read-conf for
details.
ALL is set by try-completion and all-completions. See the Emacs lisp
manual for a description. Special value 'lyskom-lookup makes the
function work as a name-to-conf-stat translator."

  ;;
  ;;  Catch some degenerate cases that can cause...problems. This
  ;;  won't solve all the...problems, but should speed things up a
  ;;  little bit.
  ;;

  (cond 
   ((and (null all)
         (string-match "^\\s-*$" string)) "")
   ((and (eq all 'lyskom-lookup)
         (string-match "^\\s-*$" string)) nil)
   ((and (eq all 'lambda)
         (string-match "^\\s-*$" string)) nil)
   (t

    (let* ((login-list (and (memq 'login predicate)
                            (lyskom-read-conf-get-logins)))
           (x-list (lyskom-completing-lookup-z-name string 
                                                    (if (or (memq 'all predicate)
                                                            (memq 'membership predicate)
                                                            (memq 'conf predicate)
                                                            (memq 'none predicate)) 1 0)
                                                    (if (or (memq 'all predicate)
                                                            (memq 'membership predicate)
                                                            (memq 'pers predicate)
                                                            (memq 'none predicate)
                                                            (memq 'login predicate)) 1 0)))
           (r-list (when (assq 'restrict predicate)
                     (let ((result (make-collector)))
                       (lyskom-traverse conf-no (cdr (assq 'restrict predicate))
                         (initiate-get-uconf-stat 'main 'collector-push 
                                                  conf-no result))
                       (lyskom-wait-queue 'main)
                       (delq nil
                             (mapcar (lambda (conf-stat)
                                       (when (lyskom-completing-match-string string (uconf-stat->name conf-stat))
                                         (lyskom-create-conf-z-info
                                          (uconf-stat->name conf-stat)
                                          (uconf-stat->conf-type conf-stat)
                                          (uconf-stat->conf-no conf-stat))))
                               (collector->value result))))))
           (candidate-list 
            (append r-list
                   (if x-list
                       (conf-z-info-list->conf-z-infos x-list))))
           (result-list nil))

      ;;
      ;;  login-list now contains a list of logins, IF the predicate
      ;;  includes 'login
      ;;
      ;;  candidate-list contains a list of conf-z-infos
      ;;
      ;;  Now set result-list to the conf-z-infos that fulfill the
      ;;  predicate, fetching the conf-stats asynchronously.
      ;;

      (lyskom-traverse el candidate-list
        (if (lyskom-read-conf-internal-verify-type (conf-z-info->conf-no el)
                                                   (conf-z-info->conf-type el)
                                                   predicate
                                                   login-list
                                                   candidate-list)
            (setq result-list (cons el result-list))))
      

      ;;
      ;;  Now the matching conf-z-infos are in result-list
      ;;

      (cond 
       ((eq all 'lyskom-lookup)
        (let ((names (mapcar 'conf-z-info->name 
                             result-list))
              (specials (lyskom-read-conf-expand-specials string
                                                          predicate
                                                          login-list
                                                          candidate-list)))

          (cond ((and kom-complete-numbers-before-names specials)
                 (lyskom-read-conf-lookup-specials string
                                                   predicate
                                                   login-list
                                                   candidate-list))
                ((= (length result-list) 1)
                 (car result-list))

                ((and (> (length result-list) 1)
                      (lyskom-completing-member string names))
                 (elt result-list
                      (- (length result-list)
                         (length (lyskom-completing-member string names)))))

                (specials (lyskom-read-conf-lookup-specials string
                                                            predicate
                                                            login-list
                                                            candidate-list))
                ((string-match (lyskom-get-string 'person-or-conf-no-regexp)
                               string) nil)
                ((string-match (lyskom-get-string 'session-no-regexp)
                               string) nil)
                ((lyskom-read-conf-internal-verify-type nil
                                                        nil
                                                        predicate
                                                        login-list
                                                        candidate-list)
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
                                                          candidate-list)))
          (cond ((= (length result-list) 1) t)
                ((and (> (length result-list) 1)
                      (let ((names (mapcar 'conf-z-info->name
                                           result-list)))
                        (and (lyskom-completing-member string names)
                             t))))
                (result-list nil)
                ((= (length specials) 1) t)
                (specials nil)
                ((string-match (lyskom-get-string 'person-or-conf-no-regexp)
                               string) nil)
                ((string-match (lyskom-get-string 'session-no-regexp)
                               string) nil)

                (t (lyskom-read-conf-internal-verify-type nil
                                                          nil
                                                          predicate
                                                          login-list
                                                          candidate-list)))))


       ;;
       ;;  Called from all-completions. Return a list of all possible
       ;;  completions, in this case all names in the result list plus,
       ;;  if the input string is a person or conf number specification,
       ;;  the input string, PROVIDED, the requested conference matches
       ;;  the predicate. If there were no matches, return the input
       ;;  string if no matches satisfies the predicate.
       ;;
          
       (all
        (let ((names (mapcar 'conf-z-info->name result-list))
              (specials (lyskom-read-conf-expand-specials string
                                                          predicate
                                                          login-list
                                                          candidate-list)))
          (cond (specials (append specials names))
                (names names)
                ((string-match (lyskom-get-string 'person-or-conf-no-regexp)
                               string) nil)
                ((string-match (lyskom-get-string 'session-no-regexp)
                               string) nil)
                ((lyskom-read-conf-internal-verify-type nil
                                                        nil
                                                        predicate
                                                        login-list
                                                        candidate-list)
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
                                                          candidate-list)))
          (cond (specials (car specials))
                ((string-match (lyskom-get-string 'person-or-conf-no-regexp)
                               string) nil)
                ((string-match (lyskom-get-string 'session-no-regexp)
                               string) nil)
                ((lyskom-read-conf-internal-verify-type nil
                                                        nil
                                                        predicate
                                                        login-list
                                                        candidate-list)
                 t)
                (t nil))))

       ;;
       ;;  Called from try-completion, and there were matches in the
       ;;  server. Return t if the string is an exact match to any
       ;;  string returned from the server. Otherwise, expand the string
       ;;  as far as possible and return that
       ;;

       (t
        (let ((name-list (mapcar 'conf-z-info->name result-list))
              (specials (lyskom-read-conf-expand-specials string
                                                          predicate
                                                          login-list
                                                          candidate-list)))
          (if specials (setq name-list (nconc specials name-list)))

          (cond ((= (length name-list) 1) (if (string= (car name-list) string) t (car name-list)))
                ((string-match (lyskom-get-string 'person-or-conf-no-regexp)
                               string) nil)
                ((string-match (lyskom-get-string 'session-no-regexp)
                               string) nil)
                (t (or (lyskom-completing-cache-completion
                        (lyskom-complete-string name-list)
                        (if r-list
                            (lyskom-create-conf-z-info-list
                             (apply 'vector candidate-list))
                          x-list))
                       (and (lyskom-read-conf-internal-verify-type 
                             nil
                             nil
                             predicate
                             login-list
                             candidate-list)
                            (list string))))))))))))
        

(defun lyskom-completing-member (string list)
  (let ((string (lyskom-unicase (lyskom-completing-strip-name string)))
        (result nil))
    (while (and list (not result))
      (if (lyskom-string= string (lyskom-unicase 
                           (lyskom-completing-strip-name (car list))))
          (setq result list)
        (setq list (cdr list))))
    result))


(defun lyskom-completing-strip-name (string)
  "Strip parens and crap from a name."
  (while (string-match "([^()]*)" string)
    (setq string (replace-match " " t t string)))
  (while (string-match "\\s-\\s-+" string)
    (setq string (replace-match " " t t string)))
  (while (string-match "([^()]*$" string)
    (setq string (substring string 0 (match-beginning 0))))
  (if (string-match "^\\s-*\\(.*\\S-\\)\\s-*$" string)
      (match-string 1 string)
    string))


(defun lyskom-read-conf-internal-verify-type (conf-no
                                              conf-type
                                              predicate
                                              logins
                                              x-list)
  (or (memq conf-no (cdr (assq 'restrict predicate)))
      (and (memq 'all predicate)
           conf-no)
      (and (memq 'conf predicate)
           conf-type
           (not (conf-type->letterbox conf-type)))
      (and (memq 'pers predicate) 
           conf-type
           (conf-type->letterbox conf-type))
      (and (memq 'login predicate)
           conf-no
           (memq conf-no logins))
      (and (memq 'membership predicate)
           conf-no
           (lyskom-get-membership conf-no t))
      (and (memq 'none predicate) 
           (and (null conf-no)
                (null x-list)))))

;;; FOR DEBUGGING (DON'T DELETE)     /byers
;;;
;;; (defun lyskom-complete-show-data-list (state data)
;;;   (save-excursion
;;;     (pop-to-buffer (get-buffer-create "*kom*-complete"))
;;;     (erase-buffer)
;;;     (lyskom-set-buffer-multibyte nil)
;;;    (while data
;;;       (insert
;;;        (format "%s\n" (substring (aref (car data) 2)
;;;                                  (aref (car data) 0)
;;;                                  (aref (car data) 1))))
;;;       (setq data (cdr data)))
;;;     (insert (format "%S %S: %S" (symbol-value current-state)
;;;                     (elt state 0)
;;;                     (elt state 1)))
;;;     (sit-for 5)))


(defun lyskom-complete-string (string-list)
  "Find the longest common prefix of all strings in STRING-LIST according to
the LysKOM rules of string matching."
  (let ((main-state 'start-of-string)
        (tmp-state nil)
        (current-state 'main-state)
        (main-accumulator nil)
        (tmp-accumulator nil)
        (current-accumulator 'main-accumulator)
        (done nil)
        (paren-depth 0)
        (have-here nil)
        (last-event-worth-noting nil)
        (data-list (lyskom-complete-string-munge-input string-list))
        (next-char-state (vector nil nil)))

    (while (not done)
      (lyskom-complete-string-next-char next-char-state data-list)
;      (lyskom-complete-show-data-list next-char-state data-list)
      (cond

       ;;
       ;; Case one, a match of two non-special characters.
       ;; Accumulate one character and advance the lists
       ;;

       ((eq (aref next-char-state 0) 'match)
        (if (eq (aref next-char-state 1) ?\ )
            (progn
              (cond ((memq (symbol-value current-state)
			     '(start-of-word start-of-string))
                     nil)
                    ((eq last-event-worth-noting 'mismatch)
                     (lyskom-complete-string-accumulate current-accumulator
                                                        'SPC))
                    (t
                     (lyskom-complete-string-accumulate current-accumulator
                                                        ?\ )))
              (set current-state 'start-of-word)
              (lyskom-complete-string-advance data-list))
          (progn
            (set current-state 'in-a-word)
            (lyskom-complete-string-accumulate current-accumulator
                                               (aref next-char-state 1))
            (lyskom-complete-string-advance data-list)))
        (setq last-event-worth-noting 'match))
       
       ;;
       ;; Case two, a match of two open-paren expressions Increase
       ;; paren depth and accumulate a character. First set
       ;; current-accumulator to the temporary if paren-depth is zero
       ;; to start with.
       ;;

       ((eq (aref next-char-state 0) 'open-paren-match)
        (setq last-event-worth-noting 'match)
        (if (zerop paren-depth)
            (progn
              (setq current-accumulator 'tmp-accumulator)
              (setq current-state 'tmp-state)
              (setq tmp-state main-state)
              (setq tmp-accumulator nil)))
        (setq paren-depth (1+ paren-depth))
        (lyskom-complete-string-accumulate current-accumulator
                                    (aref next-char-state 1))
        (lyskom-complete-string-advance data-list))

       ;;
       ;; Case three, a match of two close-paren expressions
       ;; Accumulate a character. If paren-depth is postitive,
       ;; decrease it. If it ends up zero, add the temporary
       ;; accumulator to the main accumulator and set the current
       ;; accumulator to the main accumulator.
       ;;

       ((eq (aref next-char-state 0) 'close-paren-match)
        (setq last-event-worth-noting 'match)
        (lyskom-complete-string-accumulate current-accumulator
                                    (aref next-char-state 1))
        (if (> paren-depth 0)
            (progn
              (setq paren-depth (1- paren-depth))
              (if (zerop paren-depth)
                  (progn
                    (setq main-accumulator
                          (nconc tmp-accumulator main-accumulator))
                    (setq main-state tmp-state)
                    (setq current-state 'main-state)
                    (setq current-accumulator 'main-accumulator)))))
        (lyskom-complete-string-advance data-list))

       ;;
       ;; Case two, a mismatch of any kind in a paren expression
       ;;

       ((and (> paren-depth 0)
             (memq (aref next-char-state 0)
		     '(mismatch space-mismatch open-paren-mismatch)))
        (setq last-event-worth-noting 'mismatch)
        (setq tmp-accumulator nil)
        (setq tmp-state nil)
        (setq current-state 'main-state)
        (setq current-accumulator 'main-accumulator)
        (lyskom-complete-string-close-parens data-list paren-depth)
        (setq paren-depth 0))

       ;;
       ;; Case two and a half or so, a space mismatch. This is ignored
       ;; if we're still at the start of the string
       ;;
       
       ((and (eq (aref next-char-state 0) 'space-mismatch)
             (memq (symbol-value current-state)
		     '(start-of-string start-of-word)))
        (setq last-event-worth-noting nil)
        (lyskom-complete-string-skip-whitespace data-list))

       ;;
       ;; Case three, a mismatch of regular characters outside a paren
       ;; Advance to the end of the current word
       ;;

       ((and (memq (aref next-char-state 0) '(mismatch space-mismatch))
             (zerop paren-depth))
        (setq last-event-worth-noting 'mismatch)
        (if (memq (symbol-value current-state)
		    '(start-of-word start-of-string))
            (setq done t)
          (progn
            (if (not have-here)
                (progn
                  (lyskom-complete-string-accumulate current-accumulator 
                                                     'HERE)
                  (setq have-here t)))
            (lyskom-complete-string-advance-to-end-of-word data-list)
            (set current-state 'in-a-word))))

       ;;
       ;; Case four, a mistmatch where one character is an open-paren
       ;;

       ((eq (aref next-char-state 0) 'open-paren-mismatch)
        (setq last-event-worth-noting 'mismatch)
        (lyskom-complete-string-skip-parens data-list))


       ;;
       ;; Case five, eof
       ;;

       ((eq (aref next-char-state 0) 'eof)
        (setq done t))

       ;;
       ;; Case six, can't happen
       ;;

       (t (error "This can't happen: %S" next-char-state))))

    ;;
    ;; Build the result by reversing the result list and making a
    ;; string out of it.
    ;;

    (if (eq (car main-accumulator) 'SPC)
        (setq main-accumulator (cdr main-accumulator)))

    (setq main-accumulator (nreverse main-accumulator))

    (if (memq 'HERE main-accumulator)
        (let ((backup (length (memq 'HERE main-accumulator))))
          (if lyskom-experimental-features
              (setq unread-command-events
                    (append (cons ? (make-list (1- backup) 2))
                            unread-command-events)))
          (setq main-accumulator (delq 'HERE main-accumulator))))
    
    (concat (mapcar (lambda (el) (if (eq el 'SPC) ?\  el))
		    main-accumulator))))


(defun lyskom-complete-string-accumulate (accumulator char)
  (set accumulator (cons char (symbol-value accumulator))))

(defun lyskom-complete-string-munge-input (string-list)
  (mapcar (function
           (lambda (x)
             (vector 0 (length x) x)))
          string-list))

;;;
;;; Advance one regular character or multiple whitespaces
;;;

(defun lyskom-complete-string-advance (data-list)
  (lyskom-traverse 
   el data-list
   (string-match "\\([ \t]+\\|[^ \t]\\|$\\)"
                 (aref el 2)
                 (aref el 0))
   (aset el 0 (match-end 0))))

(defun lyskom-complete-string-skip-whitespace (data-list)
  (lyskom-traverse
   el data-list
   (string-match "[ \t]*" (aref el 2) (aref el 0))
   (aset el 0 (match-end 0))))

;;;
;;; Advance to the end of the current word
;;;

(defun lyskom-complete-string-advance-to-end-of-word (data-list)
  (lyskom-traverse
   el data-list
   (aset el 0 (string-match "\\([ \t]\\|$\\)" 
                            (aref el 2)
                            (aref el 0)))))

;;;
;;; Unwind a number of parens
;;;

(defun lyskom-complete-string-skip-parens (data-list)
  (lyskom-traverse
   el data-list
   (if (eq ?\( (aref (aref el 2) (aref el 0)))
       (progn
         (aset el 0 (1+ (aref el 0)))
         (lyskom-complete-string-close-parens-2 el 1)))))

(defun lyskom-complete-string-close-parens (data-list depth)
  (lyskom-traverse
   el data-list
   (lyskom-complete-string-close-parens-2 el depth)))

(defun lyskom-complete-string-close-parens-2 (el depth)
  (let ((string (aref el 2))
        (pos (aref el 0)))
    (while (> depth 0)
      (cond ((>= pos (length string)) 
             (setq depth 0))
            ((eq (aref string pos) ?\))
             (setq depth (1- depth)))
            ((eq (aref string pos) ?\))
             (setq depth (1+ depth))))
      (setq pos (1+ pos)))
    (aset el 0 pos)))


;;;
;;; Check what's happenin' next
;;;

(defun lyskom-complete-string-next-char (state data-list)
  (let ((eofp nil)
        (open-paren-p nil)
        (close-paren-p nil)
        (matchp t)
        (spacep nil)
        (char nil)
        (xchar nil))

    (lyskom-save-excursion
     (set-buffer lyskom-buffer)
     (mapc
      (function 
       (lambda (x)
         (cond ((>= (aref x 0) (aref x 1))
                (setq eofp t)
                (setq matchp nil))
               ((eq (aref (aref x 2) (aref x 0)) ?\()
                (setq open-paren-p t))
               ((eq (aref (aref x 2) (aref x 0)) ?\))
                (setq close-paren-p t))
               ((eq (aref (aref x 2) (aref x 0)) ?\ )
                (setq spacep t)))

         (setq matchp (and matchp
                           (if (null char)
                               (progn
                                 (setq xchar (aref (aref x 2)
                                                   (aref x 0)))
                                 (setq char (lyskom-unicase-char xchar)))
                             (eq char (lyskom-unicase-char
                                       (aref (aref x 2)
                                             (aref x 0)))))))))
      data-list))

    (aset state 1 xchar)
    (cond (eofp (aset state 0 'eof))
          ((and matchp open-paren-p)
           (aset state 0 'open-paren-match))
          ((and matchp close-paren-p)
           (aset state 0 'close-paren-match))
          (matchp
           (aset state 0 'match))
          (spacep
           (aset state 0 'space-mismatch))
          (open-paren-p
           (aset state 0 'open-paren-mismatch))
          (t
           (aset state 0 'mismatch))))
  state)



;;; ============================================================
;;; Session reading
;;;

(defun lyskom-read-session-no (prompt &optional empty initial only-one)
  (let ((possible-matches
         (lyskom-session-from-conf
          (lyskom-read-conf-no prompt
                               (if kom-permissive-completion
                                   '(pers)
                                 '(login))
                               empty
                               initial
                               t))))
    (if (and (> (length possible-matches) 1)
             only-one)
        (lyskom-read-session-resolve-ambiguity possible-matches)
      possible-matches)))


(defun lyskom-session-from-conf (conf-no)
  (let ((who-list (lyskom-completing-who-is-on))
        (sessions nil))
    (if (lyskom-have-feature dynamic-session-info)
	(while who-list
	  (if (eq (dynamic-session-info->person (car who-list)) conf-no)
	      (setq sessions (cons (dynamic-session-info->session
				    (car who-list))
				   sessions)))
	  (setq who-list (cdr who-list)))
      (while who-list
	(if (eq (who-info->pers-no (car who-list))
		conf-no)
	    (setq sessions (cons (who-info->connection (car who-list))
				 sessions)))
	(setq who-list (cdr who-list))))
    (cond ((and (null sessions) kom-permissive-completion) (list (- conf-no)))
          (t sessions))))


(defun lyskom-read-session-resolve-ambiguity (sessions)
  (lyskom-insert "\n")
  (let* ((s-width (1+ (apply 'max (mapcar (function
					   (lambda (x)
					     (lyskom-string-width (int-to-string x))))
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
			      (blocking-do 'get-uconf-stat
					   (session-info->working-conf info))))
			(lyskom-format-insert
			 format-string-p
			 (format "%d%s"
				 (session-info->connection info)
				 (if (eq (session-info->connection info)
					 lyskom-session-no)
				     "*" " "))
			 (session-info->pers-no info)
			 (or confconfstat
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
      (lyskom-insert (lyskom-format 'total-users-sans-date (length who-info)))
      (lyskom-scroll)
      (while (string= ""
                      (setq result (lyskom-completing-read
				    (lyskom-get-string 'resolve-session)
				    (lyskom-maybe-frob-completion-table 
				     who-info)
				    nil
				    t
				    (car (car who-info))
				    nil))))
      (list (session-info->connection (cdr (assoc result who-info)))))))


;;; ================================================================
;;; Reading text data
;;;

(defun lyskom-completing-read-text-no (prompt default text-no misc-types)
  "Read a text number based on some other text number.
PROMPT is the prompt. DEFAULT is the default value. It can be a number,
nil (in which case a default is computed) or a function that is called
with TEXT-NO and the list of possible completions as argument (and
should return a single number). TEXT-NO is the text number to base
reading on. MISC-TYPES are the types of misc-infos in the base text
to use as completion"
  (unless (listp misc-types) (setq misc-types (list misc-types)))
  (let* ((text-stat (blocking-do 'get-text-stat text-no))
	 (completions 
	  (delq nil
		(mapcar (lambda (misc)
			  (and (memq (misc-info->type misc) misc-types)
			       (cond ((eq (misc-info->type misc) 'COMM-IN)
				      (misc-info->comm-in misc))
				     ((eq (misc-info->type misc) 'FOOTN-IN)
				      (misc-info->footn-in misc))
				     ((eq (misc-info->type misc) 'COMM-TO)
				      (misc-info->comm-to misc))
				     ((eq (misc-info->type misc) 'FOOTN-TO)
				      (misc-info->footn-to misc)))))
			(text-stat->misc-info-list text-stat)))))
    (lyskom-read-number prompt
			(cond ((functionp default) (funcall default text-no completions))
			      (default)
			      ((memq lyskom-current-text completions) lyskom-current-text)
			      (t (car completions)))
			nil
			nil
			completions
			)))
