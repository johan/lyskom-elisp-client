;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: command.el,v 44.48 2003-08-02 20:21:45 byers Exp $
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
;;;; File: command.el
;;;;
;;;; This file contains stuff regarding commands.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: command.el,v 44.48 2003-08-02 20:21:45 byers Exp $\n"))


;;; ======================================================================
;;; LysKOM user commands
;;; The new, blocking commands have a very similar structure
;;;
;;;  (defun kom-cmd (args)
;;;    "Documentation"
;;;    (interactive "...")
;;;    (lyskom-start-of-command 'kom-cmd)
;;;    (unwind-protect
;;;        (progn ...)
;;;      (lyskom-end-of-command)))
;;;
;;; This can now be written as
;;;
;;; (def-kom-command kom-cmd (args)
;;;   "Documentation"
;;;   (interactive "...")
;;;   ...)

(eval-and-compile
  (defun lyskom-fix-interactive-decl (decl command)
    (cond ((stringp (car (cdr decl))) decl)
          (t `(interactive (let ((lyskom-current-command ',command))
                             ,@(cdr decl)))))))




(defmacro def-kom-command (cmd args doc interactive-decl &rest forms)
  (if (not (stringp doc))
      (progn (message "!! No docstring for command %S" cmd)
             (setq forms (cons interactive-decl forms))
             (setq interactive-decl doc)
             (setq doc "")))
  (if (not (eq (car interactive-decl) 'interactive))
      (progn (message "!! Missing interactive declaration for %S; assuming \(interactive\)" cmd)
             (setq forms (cons interactive-decl forms))
             (setq interactive-decl '(interactive))))
  (let ((bufsym (intern (format "%S-start-buffer" cmd))))
    `(defun ,cmd ,args
       ,doc
       ,(lyskom-fix-interactive-decl interactive-decl cmd)
       (let ((lyskom-command-point (point)))
         (lyskom-start-of-command ',cmd)
         (let ((,bufsym (current-buffer)))
           (unwind-protect
               (condition-case nil
                   (progn ,@forms)
                 (quit (ding)
                       (lyskom-insert-before-prompt
                        (lyskom-get-string 'interrupted))))
             (lyskom-save-excursion
               (when (buffer-live-p ,bufsym)
                 (set-buffer ,bufsym))
               (lyskom-end-of-command))))))))

;;
;; def-kom-emacs-command works like def-kom-command, but the template 
;; is different. Commands defined this way will run as regular Emacs
;; commands when invoked outside of a LysKOM buffer. 
;;
;; The variable <cmd>-running-as-kom-command is non-nil when running
;; as a LysKOM command and nil otherwise.
;;
;; Note: this function catches *all* errors in lyskom-start-of-command
;;       which may not be what you want, so be careful.
;;
;; 
;; (defun kom-cmd (args)
;;   "Documentation"
;;   (interactive "...")
;;   (let ((kom-cmd-running-as-kom-command nil))
;;     (condition-case nil
;;         (progn (lyskom-start-of-command 'kom-cmd)
;;                (setq kom-cmd-running-as-kom-command t))
;;       (error nil))
;;     (unwind-protect
;;         (condition-case nil
;;             (progn ...)
;;           (quit (ding)
;;                 (lyskom-insert-before-prompt
;;                  (lyskom-get-string 'interrupted))))
;;       (and kom-cmd-running-as-kom-command (lyskom-end-of-command)))))
;; 

(defmacro def-kom-emacs-command (cmd args doc interactive-decl &rest forms)
  (if (not (stringp doc))
      (progn (message "!! No docstring for command %S" cmd)
             (setq forms (cons interactive-decl forms))
             (setq interactive-decl doc)
             (setq doc "")))
  (if (not (eq (car interactive-decl) 'interactive))
      (progn (message "!! Missing interactive declaration for %S; assuming \(interactive\)" cmd)
             (setq forms (cons interactive-decl forms))
             (setq interactive-decl '(interactive))))

  (let ((rsym (intern (format "%S-running-as-kom-command" cmd)))
        (bufsym (intern (format "%S-start-buffer" cmd))))
    `(defun ,cmd ,args
       ,doc
       ,(lyskom-fix-interactive-decl interactive-decl cmd)
       (let ((,rsym nil))
         (condition-case nil
             (progn (lyskom-start-of-command ',cmd)
                    (setq ,rsym t))
           (error nil))
         (let ((,bufsym (current-buffer)))
           (unwind-protect
               (condition-case nil
                   (progn ,@forms)
                 (quit (ding)
                       (lyskom-insert-before-prompt
                        (lyskom-get-string 'interrupted))))
             (and ,rsym
                  (lyskom-save-excursion
                   (when (buffer-live-p ,bufsym)
                     (set-buffer ,bufsym))
                   (lyskom-end-of-command)))))))))



(put 'def-kom-command 'edebug-form-spec
     '(&define name lambda-list
	       [&optional stringp]	; Match the doc string, if present.
	       ("interactive" interactive)
	       def-body))

(put 'def-kom-emacs-command 'edebug-form-spec
     '(&define name lambda-list
	       [&optional stringp]	; Match the doc string, if present.
	       ("interactive" interactive)
	       def-body))



;;;; ================================================================
;;;;                User-level commands and functions.


(defsubst lyskom-command-name (command)
  "Get the command name for the command COMMAND"
  (condition-case nil
      (lyskom-get-string command 'lyskom-command)
    (error nil)))

(defun lyskom-ok-command (alternative administrator)
  "Returns non-nil if it is ok to do such a command right now."
  (when (vectorp alternative) 
    (setq alternative (cons (elt alternative 0) (elt alternative 1))))
  (if administrator
      (not (memq (cdr alternative) lyskom-admin-removed-commands))
    (not (memq (cdr alternative) lyskom-noadmin-removed-commands))))

(defun kom-extended-command ()
  "Read a LysKOM function name and call the function."
  (interactive)
  (let ((fnc (lyskom-read-extended-command current-prefix-arg)))
    (cond
     (fnc (call-interactively fnc))
     (t (kom-next-command)))) )

(defvar lyskom-command-minibuffer-local-completion-map
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    (define-key map " " 'lyskom-command-complete-word)
    map)
  "Keymap used for reading LysKOM names.")

(defvar lyskom-command-minibuffer-local-must-match-map
  (let ((map (copy-keymap minibuffer-local-must-match-map)))
    (lyskom-xemacs-or-gnu 
     (progn (set-keymap-parent map lyskom-minibuffer-local-completion-map)
            (define-key map " " 'lyskom-command-complete-word))
     (define-key map " " 'lyskom-command-complete-word))
    map)
  "Keymap used for reading LysKOM names.")

(defun lyskom-read-extended-command (&optional prefix-arg prompt)
  "Reads and returns a command"
  (let* ((completion-ignore-case t)
	 (minibuffer-setup-hook minibuffer-setup-hook)
         (base-prompt (cond ((null prompt) (lyskom-get-string 'extended-command))
                            ((symbolp prompt) (lyskom-get-string prompt))
                            (t prompt)))
	 (alternatives (mapcar 
			(lambda (pair)
			  (cons 
			   (cdr pair)
			   (car pair)))
			(lyskom-get-strings lyskom-commands
					    'lyskom-command)))
	 (name nil)
         (prefix-text
          (cond ((eq prefix-arg '-) "- ")
                ((equal prefix-arg '(4)) "C-u ")
                ((integerp prefix-arg) 
                 (format "%d " prefix-arg))
                ((and (consp prefix-arg) 
                      (integerp (car prefix-arg)))
                 (format "%d " (car prefix-arg)))
                (t nil)))
         (prompt (if prefix-text
                     (concat prefix-text base-prompt)
                   base-prompt)))

    (let ((minibuffer-local-completion-map 
           lyskom-command-minibuffer-local-completion-map)
          (minibuffer-local-must-match-map 
           lyskom-command-minibuffer-local-must-match-map))
      (while (or (null name)
                 (string= "" name))
      (lyskom-with-lyskom-minibuffer
       (setq name (lyskom-completing-read prompt
                                          'lyskom-complete-command
                                          ;; lyskom-is-administrator is buffer-local and
                                          ;; must be evalled before the call to 
                                          ;; completing-read
                                          ;; Yes, this is not beautiful
                                          (list 'lambda '(alternative) ;
                                                (list 'lyskom-ok-command 'alternative
                                                      lyskom-is-administrator))
                                          t
                                          nil
                                          'lyskom-command-history)))))
    (cdr (lyskom-string-assoc name alternatives))))


(defun lyskom-update-command-completion ()
  "Build a list of alternatives for completion of LysKOM commands.
Each list element is a vector [NAME COMMAND CANONICAL]. NAME is the
command name, COMMAND is the command and CANONICAL is the name 
transformed for matching."
  (setq lyskom-command-alternatives
        (mapcar (lambda (el) 
                  (vector (cdr el)
                          (car el)
                          (lyskom-completing-strip-command
                           (lyskom-unicase (cdr el)))
			  (lyskom-unicase (cdr el))))
                (lyskom-get-strings lyskom-commands 'lyskom-command))))

(defun lyskom-lookup-command-by-name (string &optional predicate)
  "Look up the command that corresponds to a certain string."
  (lyskom-complete-command string predicate 'lyskom-lookup))

(defsubst lyskom-command-match-string-regexp (string)
  (concat 
   "^\\s-*"
   (replace-in-string (regexp-quote
                       (lyskom-unicase
                        (lyskom-completing-strip-command string)))
                      "\\s-+" "\\\\S-*\\\\s-+")
          "\\s-*"))

(defun lyskom-completing-strip-command (string)
  "Strip parens and crap from a name.
If optional DONT-STRIP-SPACES is non-nil, don't strip spaces at front
and back of the string."
  (while (string-match "([^()]*)" string) ; Strip nested parens
    (setq string (replace-match "" t t string)))
  (while (string-match "\\s-\\s-+" string) ; Collapse spaces
    (setq string (replace-match " " t t string)))
  (while (string-match "([^()]*$" string) ; Strip incomplete parens at end
    (setq string (substring string 0 (match-beginning 0))))
  string)

;;; FIXME: Below is an idea on how to do command completion more right.
;;;
;;; Precompute lists of words in all commands. Include the optional words 
;;; and mark them as optional. Possibly allow sublists in the list, and
;;; make lists of words into sublists and mark the entire sublist as
;;; optional.
;;;
;;; When matching, divide the input into a list of words. Start matchin
;;; prefixes against the list of words for a command. It goes something 
;;; like this:
;;;
;;; C = 0, I = 0
;;; while there are more words in the input and command
;;;    A = word I of the input
;;;    B = word C of the command
;;;    if B is an optional word then
;;;       N = index of word following optional group that B is part of
;;;       push N,I onto backtracking stack
;;;    if A is a prefix of B then
;;;       I = I + 1
;;;	  C = C + 1
;;;       next iteration of the loop
;;;    if the backtracking stack is empty then
;;;       return mismatch
;;;    pop X,Y from the backtracking stack
;;;    C = X
;;;    I = Y
;;;    next iteration of the loop
;;; end while
;;; if there are left-over words in C then
;;;    return a mismatch
;;; else
;;;    return a match (I,C)
;;;
;;; When doing a word completion we can let the completion function do the
;;; actual work. It computes the longest possible completion we can have
;;; (i.e. one full word more than what we've got) and hands that over to
;;; lyskom-complete-string. Computing the longest possible completion
;;; goes something like this:
;;;
;;; W = nil
;;; Store I,C for all matches
;;; for all matches M = 1 .. N do
;;;    do something useful
;;;
;;; The idea is to get the last word that matches the input by storing
;;; the results of the match computation and then looking at the
;;; following word in all possible completions. If the following word
;;; is a word in an optional group that is the same in all possible
;;; completions, then that plus the first C words of any of the
;;; possible completions is the maximum possible. If the next word is
;;; a word of an optional group that does *not* match in all
;;; possibles, then ignore the optional group and look at the next
;;; word instead.

(defun lyskom-complete-command (string predicate all)
  "Completion function for LysKOM commands."
  (when (string-match "^\\s-+" string)
    (setq string (substring string (match-end 0))))
  (let ((alternatives nil)
        (m-string (lyskom-command-match-string-regexp string))
        (u-string (lyskom-unicase string))
        (exact nil))
    (lyskom-traverse el lyskom-command-alternatives
      (when (and (string-match m-string (elt el 2))
                 (or (null predicate) (funcall predicate el)))
        (setq alternatives (cons (if (eq all 'lyskom-lookup) el (elt el 0)) alternatives))
	(if (string-equal u-string (elt el 3)) (setq exact el))))
    (cond 
     ((eq all 'lyskom-lookup) (and exact (elt exact 1)))
     ((eq all 'lambda) exact)
     (all alternatives)
     ((null alternatives) nil)
     ((and (= (length alternatives) 1) exact) t)
     (t (let ((tmp (lyskom-complete-string alternatives)))
	  (lyskom-maybe-recode-string
	   (if (string-match (concat (regexp-quote (lyskom-unicase tmp)) "\\s-") u-string)
	       (concat tmp " ")
	     tmp)))))))

(defun lyskom-command-complete-word ()
  (interactive)
  (let ((string (buffer-string)))
    (when (next-single-property-change 0 'read-only string)
      (setq string 
            (substring string 
                       (next-single-property-change 0 'read-only string))))

    (let ((completion (try-completion string
                                      minibuffer-completion-table
                                      minibuffer-completion-predicate)))
      (cond ((null completion) (minibuffer-message " [No match]") nil)
            ((eq completion t) nil)
            (t (let* ((tmp string))
                 (when (and (string-equal (lyskom-unicase completion)
                                          (lyskom-unicase tmp))
                            (not (string-match "\\s-$" completion)))
                   (if (stringp (setq tmp (try-completion 
                                           (concat tmp " ")
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)))
                       (setq completion tmp)))
                 (if (string-equal (lyskom-unicase completion)
                                   (lyskom-unicase string))
                     (progn (minibuffer-completion-help) nil)
                   (delete-region (- (point-max) (length string))
                                  (point-max))

                   ;; Now we have the suggested completion
                   ;; Expand what's in the buffer by one real word.

                   (let* ((count (lyskom-command-complete-count-words string))
                          (result (lyskom-command-complete-word-truncate 
                                   completion count)))
                     (when (string= result string)
                       (setq result (lyskom-command-complete-word-truncate 
                                     completion (1+ count))))
                     (insert result))
                   t)))))))

(defvar lyskom-command-complete-regexp
  "\\(([^\)]*)\\s-+\\)*\\S-+\\(\\s-*([^\)]*)\\)*\\(\\s-+\\|\\'\\)")

(defun lyskom-command-complete-word-truncate (completion count)
  "Truncate completion COMPLETION to COUNT words."
  (let ((start 0))
    (while (and (> count 0)
                (string-match lyskom-command-complete-regexp completion start))
      (setq start (match-end 0)
            count (1- count)))
    (substring completion 0 start)))

(defun lyskom-command-complete-count-words (string)
  "Count number of real words in command name or completion STRING."
  (let ((start 0)
        (count 0))
    (while (string-match lyskom-command-complete-regexp string start)
      (setq start (match-end 0)
            count (1+ count)))
    count))



;;; The code below is an alternative implementation of 
;;; lyskom-command-complete-word that mucks with the
;;; contents of the minibuffer and then calls the regular
;;; minibuffer functions.
;;;
;;;
;;;(defun lyskom-command-complete-word-count-words (string)
;;;  (let ((count 0)
;;;        (start 0))
;;;    (while (string-match lyskom-command-complete-regexp string start)
;;;      (setq start (match-end 0)
;;;            count (1+ count)))
;;;    count))  
;;;
;;;(defun lyskom-command-complete-word ()
;;;  (interactive)
;;;  (let* ((string (buffer-string))
;;;         completions)
;;;    ;; Strip the prompt in Emacs 21
;;;    (when (next-single-property-change 0 'read-only string)
;;;      (setq string 
;;;            (substring string 
;;;                       (next-single-property-change 0 'read-only string))))
;;;
;;;    (setq completions (save-excursion
;;;                        (set-buffer lyskom-buffer)
;;;                        (lyskom-complete-command string 
;;;                                                 nil
;;;                                                 nil)))
;;;    (when (stringp completions)
;;;      (let ((original-count (lyskom-command-complete-word-count-words string))
;;;            (start 0))
;;;        (while (and (> original-count 0)
;;;                    (string-match lyskom-command-complete-regexp completions start))
;;;          (setq start (match-end 0)
;;;                original-count (1- original-count)))
;;;
;;;        (delete-region (- (point-max) (length string)) (point-max))
;;;        (let ((result (substring completions 0 start)))
;;;          (string-match "\\s-*\\'" result)
;;;          (insert (substring result 0 (match-beginning 0)))))))
;;;  (minibuffer-complete-word))


(defun lyskom-start-of-command (function &optional may-interrupt dead-ok)
  "This function is run at the beginning of every LysKOM command.
It moves the cursor one line down, and +++ later it will tell the server
that the previous text has been read.

Argument FUNCTION is a string the string will be written in the buffer
on start of the command. If it is a symbol it searches for the corresponding
command name in lyskom-commands and writes this in the message buffer.

If optional argument MAY-INTERRUPT is present and non-nil,
don't signal an error if this call is interrupting another command.

If optional DEAD-OK is non-nil, don't signal an error if the session 
has been closed.

Special: if lyskom-is-waiting then we are allowed to break if we set 
lyskom-is-waiting nil.

This function checks if lyskom-doing-default-command and
lyskom-first-time-around are bound. The text entered in the buffer is
chosen according to this"

  (cond ((or (not (boundp 'lyskom-proc))
             (not (boundp 'lyskom-buffer))
             (and (null lyskom-proc) (null lyskom-buffer)))
         (lyskom-error "%s" (lyskom-get-string 'not-lyskom-buffer)))

        ((and (not dead-ok)
              (or (not lyskom-proc)
                  (memq (process-status lyskom-proc) '(closed signal exited nil))))
      (lyskom-error "%s" (lyskom-get-string 'dead-session))))

  (if (and lyskom-is-waiting
           (listp lyskom-is-waiting))
      (progn
        (setq lyskom-is-waiting nil)
        (lyskom-end-of-command)))

  (setq lyskom-is-waiting nil)
  (if (and lyskom-executing-command (not may-interrupt))
      (lyskom-error "%s" (lyskom-get-string 'wait-for-prompt)))
  (if (not (and (boundp 'lyskom-doing-default-command)
                lyskom-doing-default-command))
      (cond
       (lyskom-first-time-around)
       ((stringp function) (lyskom-insert function))
       ((and function (symbolp function))
        (let ((name (lyskom-command-name function)))
          (if name (lyskom-insert name)))))
    (save-excursion
      (if lyskom-current-prompt
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (beginning-of-line)
            (delete-region (point) (point-max)))))
    (lyskom-insert (lyskom-modify-prompt 
                    (cond ((stringp lyskom-current-prompt) 
                           (apply 'lyskom-format 
                                  lyskom-current-prompt
                                  lyskom-current-prompt-args))
                          ((symbolp lyskom-current-prompt)
                           (apply 'lyskom-format 
                                  (lyskom-get-string lyskom-current-prompt)
                                  lyskom-current-prompt-args))
                          (t (format "%S" lyskom-current-prompt)))
                    t)))
  (setq mode-line-process (lyskom-get-string 'mode-line-working))
  (if (pos-visible-in-window-p (point-max))
      (save-excursion
        (goto-char (point-max))
        (lyskom-set-last-viewed)))
  (setq lyskom-executing-command t)
  (setq lyskom-current-command function)
  (setq lyskom-current-prompt nil)
  (lyskom-insert "\n")
  (if (and (eq (window-buffer (selected-window))
               (current-buffer))) 
      (progn
	(if (pos-visible-in-window-p (1- (point-max)))
	    (goto-char (point-max)))
	(sit-for 0)))
  (run-hooks 'lyskom-before-command-hook)
  (if kom-page-before-command           ;Nice with dumb terminals.
      (if (or (not (listp kom-page-before-command))
              (memq function kom-page-before-command))
          (recenter 1))))


(defun lyskom-end-of-command ()
  "Print prompt, maybe scroll, prefetch info."
  (lyskom-save-excursion
   (message "")
   (lyskom-clean-all-buffer-lists)
   (while (and lyskom-to-be-printed-before-prompt
               (lyskom-queue->first lyskom-to-be-printed-before-prompt))
     (if (not (bolp)) (lyskom-insert "\n"))
     (lyskom-insert (car (lyskom-queue->first 
                          lyskom-to-be-printed-before-prompt)))
     (lyskom-queue-delete-first lyskom-to-be-printed-before-prompt))
   (setq lyskom-executing-command nil)
   (setq lyskom-current-command nil)
   (setq lyskom-current-prompt nil)	; Already set in s-o-c really
   (lyskom-scroll)
   (setq mode-line-process (lyskom-get-string 'mode-line-waiting))
   (if (pos-visible-in-window-p (point-max) (selected-window))
       (lyskom-set-last-viewed))
   (lyskom-prefetch-and-print-prompt)
   (run-hooks 'lyskom-after-command-hook)
   (when (and (lyskom-have-feature idle-time)
              (not lyskom-is-anonymous))
     (save-excursion (set-buffer lyskom-buffer)
                     (initiate-user-active 'background nil)))
   (if kom-inhibit-typeahead
       (discard-input))
   ;; lyskom-pending-commands should probably be a queue or a stack.
   (when lyskom-pending-commands
      (let ((command (car lyskom-pending-commands)))
       (setq lyskom-pending-commands (cdr lyskom-pending-commands))
       (if (symbolp command)
           (call-interactively command)
         (eval command))))
   (when lyskom-slow-mode
     (buffer-enable-undo))))

(eval-and-compile (provide 'lyskom-command))

;;; command.el ends here
