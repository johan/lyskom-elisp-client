;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: review.el,v 44.75 2010-05-14 14:10:26 byers Exp $
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
;;;; File: review.el
;;;;
;;;; This file contains functions to review articles in different ways.
;;;; Both the review commands themselves, the functions called by them
;;;; and the underlying functions are here.
;;;;
;;;; Most, if not all, of these functions are written by Linus Tolke.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: review.el,v 44.75 2010-05-14 14:10:26 byers Exp $\n"))

(eval-when-compile
  (require 'lyskom-command "command"))

(put 'lyskom-cant-review-error 'error-conditions '(error lyskom-error lyskom-review-error))
(put 'lyskom-cant-review-error 'error-message "Can't review")
(put 'lyskom-review-error 'error-condiions '(error lyskom-error))
(put 'lyskom-review-error 'error-message "Review error")


;;; ======================================================================
;;; State-saving variables
;;;
;;; All these need to be buffer-local

(def-kom-var lyskom-last-review-by nil local)
(def-kom-var lyskom-last-review-to nil local)
(def-kom-var lyskom-last-review-num nil local)
(def-kom-var lyskom-last-review-pmark nil local)
(def-kom-var lyskom-last-review-cmark nil local)
(def-kom-var lyskom-last-review-filter nil local)
(def-kom-var lyskom-last-review-saved-result-list nil local)
(def-kom-var lyskom-last-review-saved-by-list nil local)
(def-kom-var lyskom-last-review-saved-to-list nil local)
(def-kom-var lyskom-last-review-saved-result-size 0 local)
(def-kom-var lyskom-last-review-saved-smallest nil local)
(def-kom-var lyskom-last-review-saved-largest nil local)
(def-kom-var lyskom-last-review-comments nil local)
(def-kom-var lyskom-have-review nil local)

(def-kom-var lyskom-last-unread-by nil local)
(def-kom-var lyskom-last-unread-to nil local)
(def-kom-var lyskom-last-unread-num nil local)
(def-kom-var lyskom-last-unread-pmark nil local)
(def-kom-var lyskom-last-unread-cmark nil local)
(def-kom-var lyskom-last-unread-filter nil local)
(def-kom-var lyskom-last-unread-saved-result-list nil local)
(def-kom-var lyskom-last-unread-saved-by-list nil local)
(def-kom-var lyskom-last-unread-saved-to-list nil local)
(def-kom-var lyskom-last-unread-saved-result-size 0 local)
(def-kom-var lyskom-last-unread-saved-smallest nil local)
(def-kom-var lyskom-last-unread-saved-largest nil local)
(def-kom-var lyskom-have-unread nil local)

(defvar lyskom-default-review-filter nil
  "Default filter for reviewing texts")


(defun lyskom-remove-zeroes (a)
  "Returns a copy of list where all zeroes are removed."
  (delq 0 (copy-sequence a)))


;;; ================================================================
;;; Temoporary change of mark-as-read

(defun lyskom-maybe-run-mouse-2 (command)
  (lyskom-xemacs-or-gnu 
   nil
   (when (and (eq command 'mouse-drag-region)
	      unread-command-events
	      (eventp (car unread-command-events))
	      (eq (lyskom-event-type 
		   (car unread-command-events)) 'mouse-2))
     (let* ((sequence (vector (car unread-command-events)))
	    (command (or (lookup-key (current-local-map) 
				     sequence)
			 (lookup-key global-map sequence))))
       (setq unread-command-events (cdr unread-command-events))
       (when (commandp command) 
	 (call-interactively command))))))

(defun kom-toggle-mark-as-read-prefix ()
  "Read one key sequence and run one command with state of
kom-review-marks-texts-as-read toggled."
  (interactive)
  (let* ((kom-review-marks-texts-as-read (not kom-review-marks-texts-as-read))
	 (sequence (read-key-sequence
		    (format "%s: " 
			    (lyskom-get-string
			     (if kom-review-marks-texts-as-read 
				 'review-marking-as-read
			       'review-not-marking-as-read)))))
	 (command (or (lookup-key (current-local-map) sequence)
		      (lookup-key global-map sequence))))
    (when (or (null command) (eq command 'kom-mouse-null))
      (setq sequence (read-key-sequence ""))
      (setq command (or (lookup-key (current-local-map) sequence)
			(lookup-key global-map sequence))))
    (when (commandp command) 
      (call-interactively command))
    (lyskom-maybe-run-mouse-2 command)))

(defun kom-toggle-cache-prefix ()
  "Read one key sequence and run one command with state of
kom-review-marks-texts-as-read toggled."
  (interactive)
  (let* ((kom-review-uses-cache (not kom-review-uses-cache))
         (sequence (read-key-sequence
                    (format "%s: " 
                            (lyskom-get-string
                             (if kom-review-uses-cache 
                                 'review-using-cache
                               'review-not-using-cache)))))
         (command (lookup-key (current-local-map) sequence)))
    (when (commandp command)
      (call-interactively command))
    (lyskom-maybe-run-mouse-2 command)))

;; (defun kom-toggle-topic-prefix ()
;;   "Read one key sequence and run one command with the review
;; filter set to only show texts that are not comments."
;;   (interactive)
;;   (let* ((lyskom-default-review-filter 'lyskom-review-filter-roots)
;; 	 (sequence (read-key-sequence
;; 		    (format "%s: " "Enbart urinlägg")))
;; 	 (command (lookup-key (current-local-map) sequence)))
;;     (when (commandp command)
;;       (call-interactively command))))



;;; ================================================================
;;;              ]terse av, till - Review by X to Conference Y.

;;; Author: David Byers


(def-kom-command kom-review-all ()
  "Review all texts written by a particular author to a particular
conference. This can also be accomplished by using `kom-review-by-to'
and specifying zero texts.

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive)
  (lyskom-tell-internat 'kom-tell-review)
  (lyskom-review-by-to 0))

(def-kom-command kom-review-all-roots ()
  "Review all texts that are not comments written by a particular
author to a particular conference. This can also be accomplished
by using `kom-review-roots-by-to' and specifying zero texts.

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive)
  (lyskom-tell-internat 'kom-tell-review)
  (lyskom-review-by-to 0 'lyskom-review-filter-roots))

(def-kom-command kom-unread-all ()
  "Mark all texts written by a particular author to a particular
conference as unread. This can also be accomplished by using
`kom-unread-by-to' and specifying zero texts."
  (interactive)
  (lyskom-unread-by-to 0))

(def-kom-command kom-unread-all-roots ()
  "Mark all texts that are not comments written by a particular
author to a particular conference as unread. This can also be
accomplished by using `kom-unread-roots-by-to' and specifying
zero texts."
  (interactive)
  (lyskom-unread-by-to 0 'lyskom-review-filter-roots))

(def-kom-command kom-review-more (count)
  "Review more texts using the same critera as the last review
performed with `kom-review-by-to'. The review will be resumed where
the previous review finished.

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive "P")
  (if (not lyskom-have-review)
      (lyskom-format-insert 'no-review-done)
    (let* ((count (or count
		      (lyskom-read-number
		       (lyskom-get-string 'review-how-many-more)
		       (abs lyskom-last-review-num))))
           (info (progn (if (and (listp count)
                                 (integerp (car count))
                                 (null (cdr count)))
                            (setq count (car count)))
                        (cond ((zerop count) 
                               (setq count nil)
                               (lyskom-get-string 'review-rest))
                              ((> count 0)
                               (lyskom-format (lyskom-get-string 'review-more)
                                              count)))))
           (by lyskom-last-review-by)
           (to lyskom-last-review-to)
	   (filter lyskom-last-review-filter))

      (lyskom-format-insert 'review-more-info-by-to
                            info
                            (if (zerop by)
                                (lyskom-get-string 'anybody)
                              by)
                            (if (zerop to)
                                (lyskom-get-string 'all-confs)
                              to)
			    (when filter
			      (funcall filter 'description)))
    
      (condition-case arg
          (let ((list (lyskom-get-texts-by-to by to count t nil filter)))
            (setq lyskom-last-review-num 
                  (if (< lyskom-last-review-num 0)
                      (- count)
                    count))
            (if list
                (lyskom-review-enter-read-info
                 (lyskom-create-read-info
                  'REVIEW
                  nil
                  (lyskom-review-get-priority)
                  (lyskom-create-text-list list)
                  nil t) t)
              (lyskom-insert-string 'no-such-text)))
        (lyskom-review-error (if arg
                                 nil
                               (lyskom-insert-string 'no-such-text)))))))

(def-kom-command kom-unread-more (count)
  "Mark more texts unread using the same critera as the last 
mark unread performed with `kom-unread-by-to'."
  (interactive "P")
  (if (not lyskom-have-unread)
      (lyskom-format-insert 'no-unread-done)
    (let* ((count (or count
		      (lyskom-read-number
		       (lyskom-get-string 'unread-how-many-more)
		       (abs lyskom-last-unread-num))))
           (info (progn (if (and (listp count)
                                 (integerp (car count))
                                 (null (cdr count)))
                            (setq count (car count)))
                        (cond ((zerop count) 
                               (setq count nil)
                               (lyskom-get-string 'unread-rest))
                              ((> count 0)
                               (lyskom-format (lyskom-get-string 'unread-more)
                                              count)))))
           (by lyskom-last-unread-by)
           (to lyskom-last-unread-to)
	   (filter lyskom-last-review-filter))

      (lyskom-format-insert 'unread-more-info-by-to
                            info
                            (if (zerop by)
                                (lyskom-get-string 'anybody)
                              by)
                            (if (zerop to)
                                (lyskom-get-string 'all-confs)
                              to)
			    (when filter
			      (funcall filter 'description)))
    
      (condition-case arg
          (let* ((lyskom-last-review-num count)
		 (list (lyskom-get-texts-by-to by to count t t filter)))
            (setq lyskom-last-unread-num 
                  (if (< lyskom-last-unread-num 0)
                      (- count)
                    count))
            (if list
              (lyskom-traverse text-no list
                (unless (lyskom-mark-unread text-no)
                  (lyskom-format-insert 'cant-mark-text-unread
                                        text-no
                                        (lyskom-get-error-text lyskom-errno))))
              (lyskom-insert-string 'no-such-text)))
        (lyskom-review-error (if arg
                                 nil
                               (lyskom-insert-string 'no-such-text)))))))



(def-kom-command kom-review-first (&optional count)
  "Review the first N texts written by a particular author to some
conference. With no author specified, review texts by all authors.
With zero texts specified, review all text. With no conference
specified, review texts to all conferences. With a negative number of
texts, review the last N texts instead of the first (you can use
`kom-review-by-to' instead.

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive "P")
  (lyskom-review-by-to (- (or count
                           (lyskom-read-number
                            (lyskom-get-string 'review-how-many) 1)))))

(def-kom-command kom-review-first-roots (&optional count)

  "Review the first N texts that are not comments written by a
particular author to some conference. With no author specified,
review texts by all authors. With zero texts specified, review
all text. With no conference specified, review texts to all
conferences. With a negative number of texts, review the last N
texts instead of the first (you can use `kom-review-roots-by-to'
instead.

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive "P")
  (lyskom-review-by-to (- (or count
                           (lyskom-read-number
                            (lyskom-get-string 'review-how-many-roots) 1)))
		       'lyskom-review-filter-roots))

(def-kom-command kom-unread-first (&optional count)
  "Mark the first N texts written by a particular author to some
conference as unread. With no author specified, review texts by all 
authors. With zero texts specified, review all text. With no conference
specified, review texts to all conferences. With a negative number of
texts, review the last N texts instead of the first (you can use
`kom-unread-by-to' instead."
  (interactive "P")
  (lyskom-unread-by-to (- (or count
                           (lyskom-read-number
                            (lyskom-get-string 'unread-how-many) 1)))))

(def-kom-command kom-unread-first-roots (&optional count)
  "Mark the first N texts that are not comments written by a
particular author to some conference as unread. With no author
specified, review texts by all authors. With zero texts
specified, review all text. With no conference specified, review
texts to all conferences. With a negative number of texts, review
the last N texts instead of the first (you can use
`kom-unread-roots-by-to' instead."
  (interactive "P")
  (lyskom-unread-by-to (- (or count
                           (lyskom-read-number
                            (lyskom-get-string 'unread-how-many-roots) 1)))
		       'lyskom-review-filter-roots))


(def-kom-command kom-review-by-to (&optional count)
  "Review the last N texts written by a particular author to some
conference. With no author specified, review texts by all authors.
With zero texts specified, review all texts. With no conference
specified, review texts to all conferences. With a negative number of
texts, review the last N texts instead of the first (you can use
`kom-review-first' instead.

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive "P")
  (lyskom-review-by-to (or count
                           (lyskom-read-number
                            (lyskom-get-string 'review-how-many) 1))))

(def-kom-command kom-review-roots-by-to (&optional count)
  "Review the last N texts, excluding comments, written by a particular
author to some conference. With no author specified, review texts by all
authors. With zero texts specified, review all texts. With no conference
specified, review texts to all conferences. With a negative number of
texts, review the last N texts instead of the first (you can use
`kom-review-first-roots' instead.

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive "P")
  (lyskom-review-by-to (or count
                           (lyskom-read-number
                            (lyskom-get-string 'review-how-many-roots) 1))
		       'lyskom-review-filter-roots))

(defun lyskom-review-by-to (count &optional filter)
  "Common function for kom-review-by-to and kom-review-first"
  (setq filter (or filter lyskom-default-review-filter))
  (let* ((info (progn (if (and (listp count)
                               (integerp (car count))
                               (null (cdr count)))
                          (setq count (car count)))
                      (cond ((zerop count) 
                             (setq count nil)
                             (lyskom-get-string 'everything))
                            ((> count 0)
                             (lyskom-format 'latest-n count))
                            ((< count 0)
                             (lyskom-format 'first-n
                                            (- count))))))
         (by (lyskom-read-conf-no
	      `(review-by-whom ,info ,(and filter (funcall filter 'description)))
	      '(pers) t nil t))
         (to (lyskom-read-conf-no
	      `(review-to-conf ,info ,(and filter (funcall filter 'description)))
	      '(all) t nil t)))

    (if (not (zerop to))
        (cache-del-conf-stat to))
    (if (not (zerop by)) 
        (cache-del-pers-stat by))

    (lyskom-format-insert 'review-info-by-to
                          info
                          (if (zerop by)
                              (lyskom-get-string 'anybody)
                            by)
                          (if (zerop to)
                              (lyskom-get-string 'all-confs)
                            to)
			  (when filter
			    (funcall filter 'description)))

    (setq lyskom-last-review-by by)
    (setq lyskom-last-review-to to)
    (setq lyskom-last-review-num count)
    (setq lyskom-last-review-pmark nil)
    (setq lyskom-last-review-cmark nil)
    (setq lyskom-last-review-filter filter)
    (setq lyskom-last-review-saved-result-list nil)
    (setq lyskom-last-review-saved-by-list nil)
    (setq lyskom-last-review-saved-to-list nil)
    (setq lyskom-last-review-saved-result-size 0)
    (setq lyskom-last-review-saved-smallest nil)
    (setq lyskom-last-review-saved-largest nil)
    (setq lyskom-have-review 'lyskom-review-by-to)

    (condition-case arg
        (let ((list (lyskom-get-texts-by-to by to count 
					    nil nil 
					    filter)))
          (if list
              (lyskom-review-enter-read-info
               (lyskom-create-read-info
                'REVIEW
                nil
                (lyskom-review-get-priority)
                (lyskom-create-text-list list)
                nil t) t)
            (lyskom-insert-string 'no-such-text)))
      (lyskom-review-error (if arg
                               nil 
                             (lyskom-insert-string 'no-such-text))))))


(def-kom-command kom-unread-by-to (&optional count)
  "Mark the last N texts written by a particular author to some
conference as unread. With no author specified, review texts by all 
authors. With zero texts specified, review all text. With no conference
specified, review texts to all conferences. With a negative number of
texts, review the last N texts instead of the first (you can use
`kom-unread-first' instead)."
  (interactive "P")
  (lyskom-unread-by-to (or count
                           (lyskom-read-number
                            (lyskom-get-string 'unread-how-many) 1))))

(def-kom-command kom-unread-roots-by-to (&optional count)
  "Mark the last N texts, excluding comments, written by a
particular author to some conference as unread. With no author
specified, review texts by all authors. With zero texts
specified, review all texts. With no conference specified, review
texts to all conferences. With a negative number of texts, review
the last N texts instead of the first (you can use
`kom-unread-first-roots' instead)."
  (interactive "P")
  (lyskom-unread-by-to (or count
                           (lyskom-read-number
                            (lyskom-get-string 'unread-how-many-roots) 1))
		       'lyskom-review-filter-roots))
  

  

(defun lyskom-unread-by-to (count &optional filter)
  "Common function for kom-review-by-to and kom-review-first"
  (setq filter (or filter lyskom-default-review-filter))
  (let* ((info (progn (if (and (listp count)
                               (integerp (car count))
                               (null (cdr count)))
                          (setq count (car count)))
                      (cond ((zerop count) 
                             (setq count nil)
                             (lyskom-get-string 'everything))
                            ((> count 0)
                             (lyskom-format 'latest-n count))
                            ((< count 0)
                             (lyskom-format 'first-n
                                            (- count))))))
         (by (lyskom-read-conf-no 
	      `(unread-by-whom ,info ,(and filter (funcall filter 'description)))
	      '(pers) t nil t))
         (to (lyskom-read-conf-no 
	      `(unread-to-conf ,info ,(and filter (funcall filter 'description)))
	      '(all) t nil t)))

    (if (not (zerop to))
        (cache-del-conf-stat to))
    (if (not (zerop by)) 
        (cache-del-pers-stat by))

    (lyskom-format-insert 'unread-info-by-to
                          info
                          (if (zerop by)
                              (lyskom-get-string 'anybody)
                            by)
                          (if (zerop to)
                              (lyskom-get-string 'all-confs)
                            to)
			  (when filter
			    (funcall filter 'description)))

    (setq lyskom-last-unread-by by)
    (setq lyskom-last-unread-to to)
    (setq lyskom-last-unread-num count)
    (setq lyskom-last-unread-pmark nil)
    (setq lyskom-last-unread-cmark nil)
    (setq lyskom-last-unread-filter filter)
    (setq lyskom-last-unread-saved-result-list nil)
    (setq lyskom-last-unread-saved-by-list nil)
    (setq lyskom-last-unread-saved-to-list nil)
    (setq lyskom-last-unread-saved-result-size 0)
    (setq lyskom-last-unread-saved-smallest nil)
    (setq lyskom-last-unread-saved-largest nil)
    (setq lyskom-have-unread t)

    (condition-case arg
        (let ((list (lyskom-get-texts-by-to by to count 
					    nil t
					    filter)))
          (if list
              (lyskom-traverse text-no list
                (unless (lyskom-mark-unread text-no)
                  (lyskom-format-insert 'cant-mark-text-unread
                                        text-no
                                        (lyskom-get-error-text lyskom-errno))))
            (lyskom-insert-string 'no-such-text)))
      (lyskom-review-error (if arg
                               nil 
                             (lyskom-insert-string 'no-such-text))))))






;;; ================================================================
;;; lyskom-get-texts-by-to
;;; Author: David Byers
;;;
;;; Call lyskom-get-texts-by, lyskom-get-texts-to or 
;;; lyskom-get-texts-by-and-to to get NUM texts by person 
;;; BY to conference TO.
;;;


(defun lyskom-get-texts-by-to (by to num &optional again do-unread filter)
  "Get NUM texts written by person number BY in conference number TO

If optional argument AGAIN is non-nil, then repeat the last review.

If optional argument DO-UNREAD is non-nil, this call is for
marking texts unread, not reviewing them. If optional argument
FILTER is non-nil, then only accept those texts for which
FILTER (called with a text-no) returns non-nil.

Args: BY TO NUM AGAIN DO-UNREAD"
  (cond ((and (zerop by) 
              (zerop to)) (lyskom-get-texts-globally num again 
						     do-unread filter))
        ((zerop to) (lyskom-get-texts-by by num again nil filter))
        ((zerop by) (lyskom-get-texts-to to num again nil filter))
        ((and (eq by lyskom-pers-no)
              (not (eq to by))
              (let ((conf (blocking-do 'get-conf-stat to)))
                (and (conf-type->letterbox (conf-stat->conf-type conf))
                     (null (map->text-nos 
                            (blocking-do 'get-map
                                         (conf-stat->conf-no conf)
                                         (conf-stat->first-local-no conf)
                                         1))))))
         (lyskom-get-texts-by-generic 
          by num 
	  (lambda (x to filter)
	    (let ((found nil))
	      (and (or (null filter)
		       (funcall filter x))
		   (lyskom-traverse misc (text-stat->misc-info-list x)
		     (setq found 
			   (or found
			       (and (memq (misc-info->type misc)
					  lyskom-recpt-types-list)
				    (eq (misc-info->recipient-no misc) to))))))
	      found))
          (list to filter)
          again))
         (t (lyskom-get-texts-by-and-to by to num again nil nil filter))))


;;; ============================================================
;;; lyskom-check-review-access
;;; Author: David Byers
;;;
;;; Check that we can access the conference map. If we can't some
;;; review functions are just not much fun
;;;

(defun lyskom-check-review-access (conf pers)
  "Check that we can review texts to CONF by PERS.
CONF is a conf-stat or t if we know we can access that conference.
PERS is a pers-stat or t if we know we can access that person.
This function signals an error if review is impossible"
  (cond ((null conf) 
         (lyskom-format-insert 'review-conf-gone)
         (signal 'lyskom-cant-review-error t))
        ((null pers)
         (lyskom-format-insert 'review-pers-gone)
         (signal 'lyskom-cant-review-error t))
        ((lyskom-conf-stat-p conf)
         (cond ((= 0 (conf-stat->no-of-texts conf))
                (lyskom-format-insert 'review-cant-read-empty)
                (signal 'lyskom-cant-review-error t))
               ((null (map->text-nos 
                       (blocking-do 'get-map
                                    (conf-stat->conf-no conf)
                                    (conf-stat->first-local-no conf)
                                    1)))
                (if (conf-type->letterbox
                     (conf-stat->conf-type conf))
                    (lyskom-format-insert 'review-cant-read-letterbox)
                  (lyskom-format-insert 'review-cant-read-conf))
                (signal 'lyskom-cant-review-error t))))))



;;; ============================================================
;;; lyskom-get-texts-globally
;;; Author: Per Cederquist, David Byers
;;;

(defun lyskom-get-texts-globally (num &optional again do-unread filter)
  "Get the last NUM texts created in LysKOM. If AGAIN is non-nil, keep
going from where we were before."
  (cond ((and again (null num))
         (setq num lyskom-last-review-num))
        ((and again (< lyskom-last-review-num 0)) 
         (setq num (- num)))
        ((null num) 
         (lyskom-format-insert
          (if do-unread 'cant-unread-everything 'cant-review-everything))
         (signal 'lyskom-cant-review-error t)))

  (let ((result nil)
        (textno (cond (again lyskom-last-review-cmark)
                      ((< num 0) 1)
                      (t (lyskom-maxint))))
        (op (if (< num 0)
                'find-next-text-no
              'find-previous-text-no))
	(batch-size (abs num)))

    ;; Get the texts in batches of how many we have left to get
    ;; With the filter, this gets inefficient as we start getting
    ;; close to the number of texts we want. 

    (while (and (not (eq textno 0)) (not (null textno))
		(< (length result) (abs num)))

      ;; Inner loop to get a batch of texts

      (let (batch)
	(while (and (not (eq textno 0))
		    (not (null textno))
		    (< (length batch) batch-size))
	  (setq textno (blocking-do op textno))
	  (when textno (setq batch (cons textno batch))))
	  
	;; Now filter the results.

	(setq batch (lyskom-review-filter-results batch filter))

	;; We now have the batch (sometimes in reverse order), so we
	;; put it onto the front of the result list. It would probably
	;; be better to retain a reasonable batch size and save any
	;; results not used, but the benefit is not obvious, and the
	;; work required is...

	(setq result (nconc batch result))
	(setq batch-size (- (abs num) (length result)))))

    (setq lyskom-last-review-cmark textno)
    (if (< num 0)
        (nreverse result)
      result)))


;;; ================================================================
;;; lyskom-get-letters-to
;;; Author: David Byers
;;; 
;;; Get letters by self that have a specified letterbox as
;;; recipient. Do this by linearly searching selfs letterbox
;;; map and look as every single doggone text-stat. What a drag.
;;;
;;; +++ FIXME: This is just get-texts-by right now. Need to filter stuff.
;;;

(defun lyskom-get-letters-to (persno recipient num &optional again
				     pstart filter)
  "Get NUM texts written by PERSNO. Args: persno num

Cannot be called from a callback."
  (let ((persstat (blocking-do 'get-pers-stat persno)))
    (lyskom-check-review-access t persstat)

    (cond ((and again (null num)) (setq num lyskom-last-review-num))
          ((and again (< lyskom-last-review-num 0)) (setq num (- num))))

    (let* ((plow (or pstart (pers-stat->first-created-text persstat)))
           (phigh (1- (+ plow (pers-stat->no-of-created-texts persstat))))
           (result (if again
                       lyskom-last-review-saved-result-list
                     nil))
           (increment (if num (abs num)))
           (mark (cond (again lyskom-last-review-pmark)
                        ((and num (< num 0)) plow)
                        (t phigh)))
           (found nil)
           (start nil))
    

      (if (null num)
          (setq num (1+ phigh)
                mark phigh
                increment (1+ phigh)))

      (while (and (<= mark phigh)
                  (>= mark plow)
                  (> (abs num) (length result)))

        (setq increment (min lyskom-fetch-map-nos increment))
        (setq start (if (< num 0)
                        mark
                      (- mark (1- increment))))
        (if (< start 0)
            (progn
              (setq increment (- increment start))
              (setq start 0)))
      
        (setq found (lyskom-remove-zeroes
		     (listify-vector
		      (map->text-nos
		       (blocking-do 'get-created-texts
				    persno
				    start
				    increment)))))

        (let ((collector (make-collector)))
	  (lyskom-traverse x found
	    (initiate-get-text-stat 
	     'main 
	     (lambda (x collector pers-no pred)
	       (if (and x (or (null pred) (funcall pred x))
			(lyskom-is-recipient x pers-no))
		   (collector-push (text-stat->text-no x) collector)))
	     x collector recipient filter))

	  (lyskom-wait-queue 'main)
	  (setq found (nreverse (collector->value collector))))

        (if (> num 0)
            (setq result (nconc found result)
                  mark (- mark increment))
          (setq result (nconc result found)
                mark (+ mark increment)))
        (if (null found)
            (setq increment (min lyskom-fetch-map-nos (* increment 2)))
          (setq increment (- (abs num) (length result)))))

      (setq lyskom-last-review-pmark mark)

      (if (> num 0)
          (progn
            (setq lyskom-last-review-saved-result-list 
                  (lyskom-nfirst (- (length result) num) result))
            (nthcdr (max 0 (- (length result) num)) result))
        (progn
          (setq lyskom-last-review-saved-result-list
                (nthcdr (- num) result))
          (lyskom-nfirst (- num)  result))))))






;;; ================================================================
;;; lyskom-get-texts-by-and-to
;;; Author: David Byers
;;;
;;; Note: We can't assume that the conference's map of texts is
;;; sorted. If we could, it would be possible to simplify this
;;; function considerably without making it slower. 
;;;
;;; Problem: Construct part of the intersection between the user's
;;; created texts (the by-list) and the texts in a conference (the
;;; to-list) without bogging down the client, server or network and
;;; do it quickly!
;;;
;;; Idea: Construct the intersection incrementally without doing more
;;; work comparing things than we would if we had the full maps to
;;; start with.
;;;
;;; Solution: Get one segment of the by-list (call the nth segment
;;; by_n) and to-list (call the nth segment to_n) at a time until we
;;; are done or until both are exhausted. 
;;;
;;; In each iteration do the following: Calculate the intersection
;;; between to_n with each of the previous by_i leaving r_n:
;;;         r_n = \prod_{i=1}^{n-1} by_i \cap to_n
;;; where \prod denotes list concatenation. Next calculate the
;;; intersection of by_n with each to_i 0<i<n in turn, concatenating
;;; the result to the corresponding r_i:
;;;         r_i \larrow r_i * (by_n \cap to_i) ; 0<i<n
;;; At this point, the concatenation of all r_n, \prod r_n, is the
;;; result of the intersection between the segments of the by-list and
;;; to-list we have retrieved so far.
;;;
;;; If the total size of the result equals or exceeds the size we
;;; requested, finish the loop and return the results.
;;;
;;;
;;; Althogh the solution may seem a bit complicated, it's really not
;;; that bad once you think about how it's done. The good thing about
;;; it is that it does as little work as possible comparing elements.
;;; The bad part is that it constructs a little too many new cons
;;; cells, although reversing most of the lists does help.
;;;
;;; The following optimisations have been implemented:
;;;
;;; - Exploit the fact that the user's map is sorted by aborting the
;;; search if we have the full conference map and the lowest number we
;;; have from the user's map is lower than the lowest number in the
;;; conference map. Highest numbers apply when we are searching from
;;; the front.

;;; +++ FIXME: For users that have written very little in high-traffic
;;; conferences, and where the request cannot be fulfilled (if num is
;;; too high) this function ends up scanning the entire conference
;;; map. At some point it is probably faster to get all the text-stats
;;; in the user's map (when we've retrieved the map we know how many
;;; there are) and look at the recipients rather than calculate the
;;; intersection. 
;;;

(defun lyskom-get-texts-by-and-to (persno confno num 
                                          &optional again pstart cstart filter)
  "Get NUM texts written by person PERSNO with conference CONFNO as a
recipient. If optional AGAIN is non-nil, continue from where we were.
Args: persno confno num &optional again pstart cstart"
  (blocking-do-multiple ((persstat (get-pers-stat persno))
                         (confstat (get-conf-stat confno)))
    (cond

     ;;
     ;; Special case: reviewing to a letterbox or conference we're not
     ;; a member of and have no access to.
     ;;

     ((and (eq lyskom-pers-no persno)
           confstat
           (or (conf-type->letterbox (conf-stat->conf-type confstat))
               (and (conf-type->rd_prot (conf-stat->conf-type confstat))
                    (null (map->text-nos 
                           (blocking-do 'get-map
                                        (conf-stat->conf-no confstat)
                                        (conf-stat->first-local-no confstat)
                                        1)))))
           (not (eq persno confno)))
      (lyskom-get-letters-to persno confno num again pstart filter))

     ;;
     ;; General case
     ;;

     (t
      (lyskom-check-review-access confstat persstat)
      (cond ((and again (null num)) (setq num lyskom-last-review-num))
            ((and again (< lyskom-last-review-num 0) (setq num (- num)))))
      (let* ((result-list (if again
                              lyskom-last-review-saved-result-list
                            nil))
             (by-list (if again
                          lyskom-last-review-saved-by-list
                        nil))
             (to-list (if again 
                          lyskom-last-review-saved-to-list 
                        nil))
             (result-size (if again
                              lyskom-last-review-saved-result-size
                            0))
             (by nil)
             (to nil)
             (increment lyskom-fetch-map-nos)
             (plow (or pstart (pers-stat->first-created-text persstat)))
             (phigh (1- (+ plow (pers-stat->no-of-created-texts persstat))))
             (pmark (cond (again lyskom-last-review-pmark)
                          ((and num (< num 0)) plow)
                          (t phigh)))
             (clow (or cstart (conf-stat->first-local-no confstat)))
             (chigh (1- (+ clow (conf-stat->no-of-texts confstat))))
             (cmark (cond (again lyskom-last-review-cmark)
                          ((and num (< num 0)) clow)
                          (t chigh)))
             (smallest (if again lyskom-last-review-saved-smallest nil))
             (largest (if again lyskom-last-review-saved-largest nil))
             (abort-loop nil))

        (if (null num)
            (setq num (1+ phigh)))

        (while (and (or (and (<= pmark phigh)
                             (>= pmark plow))
                        (and (<= cmark chigh)
                             (>= cmark clow)))
                    (> (abs num) result-size)
                    (not abort-loop))

          (setq by (and (<= pmark phigh)
                        (>= pmark plow)
                        (lyskom-remove-zeroes
                         (listify-vector
                          (map->text-nos
                           (blocking-do 'get-created-texts
                                        (pers-stat->pers-no persstat)
                                        (if (< num 0)
                                            pmark
                                          (max 0 (- pmark (1- increment))))
                                        increment)))))
                to (and (<= cmark chigh)
                        (>= cmark clow)
                        (lyskom-remove-zeroes
                         (listify-vector
                          (map->text-nos
                           (blocking-do 'get-map
                                        (conf-stat->conf-no confstat)
                                        (if (< num 0)
                                            cmark
                                          (max 0 (- cmark (1- increment))))
                                        increment))))))

          (if (> num 0)
              (if (and smallest by
                       (> smallest (car by)))
                  (setq abort-loop t))
            (if (and largest by
                     (< largest (car (nthcdr (1- (length by)) by))))
                (setq abort-loop t)))

          ;;
          ;;    Add intersection between new TO and old BYs
          ;;    to the results list.
          ;;

          (setq result-list
                (cons (apply 'nconc
                             (mapcar 
			      (lambda (x)
				(lyskom-review-filter-results
				 (lyskom-intersection to x)
				 filter))
                              by-list))
                      result-list))

          ;;
          ;;    Add new BY and TO to the by-list and to-list
          ;;

          (setq by-list (cons by by-list)
                to-list (cons to to-list))
      

          ;;
          ;;    Add intersections between new BY and all TOs
          ;;

          (setq result-list
                (lyskom-mapcar2 (lambda (x y)
				  (lyskom-review-filter-results
				   (lyskom-intersection y
							(nconc x by))
				   filter))
                                result-list
                                to-list))

          (setq result-size (apply '+ (mapcar 'length result-list)))

          ;;
          ;;    Adjust the marks
          ;;

          (if (> num 0)
              (setq pmark (- pmark increment)
                    cmark (- cmark increment))
            (setq pmark (+ pmark increment)
                  cmark (+ cmark increment)))


          ;;
          ;;  If we have exhausted the conference, calculate smallest and
          ;;  largest
          ;;

          (if (and (null smallest)
                   (null largest)
                   (or (> cmark chigh)
                       (< cmark clow)))
              (setq smallest 
                    (apply 'min
                           (mapcar (lambda (x)
				     (if x
					 (apply 'min x)
				       (lyskom-maxint)))
                                   to-list))
                    largest
                    (apply 'max
                           (mapcar (lambda (x)
				     (if x
					 (apply 'max x)
				       -1))
                                   to-list))))


          ;;
          ;;    This is the end of the while loop
          ;;

          )

        (setq lyskom-last-review-pmark pmark)
        (setq lyskom-last-review-cmark cmark)
        (setq lyskom-last-review-saved-by-list by-list)
        (setq lyskom-last-review-saved-to-list to-list)
        (setq lyskom-last-review-saved-smallest smallest)
        (setq lyskom-last-review-saved-largest largest)

        ;;
        ;;  Extract results
        ;;

        (setq result-list
              (apply 'nconc (if (< num 0)
                                (nreverse result-list)
                              result-list)))

        ;;
        ;;  Save discarded results and return retained results
        ;;

        (if (> num 0)
            (progn
              (setq lyskom-last-review-saved-result-list
                    (lyskom-nfirst (- (length result-list) num) result-list))
              (setq lyskom-last-review-saved-result-size
                    (length  lyskom-last-review-saved-result-list))
              (setq lyskom-last-review-saved-result-list
                    (cons lyskom-last-review-saved-result-list
                          (make-list (- (length by-list) 1) nil)))
              (nthcdr (max 0 (- (length result-list) num)) result-list))

          (progn
            (setq lyskom-last-review-saved-result-list 
                  (nthcdr (- num) result-list))
            (setq lyskom-last-review-saved-result-size
                  (length lyskom-last-review-saved-result-list))
            (setq lyskom-last-review-saved-result-list
                  (cons lyskom-last-review-saved-result-list
                        (make-list (- (length by-list) 1) nil)))
            (lyskom-nfirst (- num) result-list))))))))



;;; ===============================================================
;;; lyskom-get-texts-by, lyskom-get-texts-to
;;; Author: David Byers
;;;
;;; These functions get data in chunks, starting with the number of
;;; texts requested. If they come up empty (which is common when
;;; scanning from the beginning of a map), the increment is
;;; exponentially increased up to a maximum of 150. 
;;;
;;; lyskom-get-texts-by is also careful to filter out those texts that
;;; are not readable, hence the added complexity in that function.
;;;

(defun lyskom-get-texts-by (persno num &optional again pstart filter)
  "Get NUM texts written by PERSNO. Args: persno num"
  (let* ((persstat (blocking-do 'get-pers-stat persno)))
    (lyskom-check-review-access t persstat)
    (lyskom-get-texts-by-generic persno num filter nil again pstart)))

(defun lyskom-get-texts-by-generic (persno num pred args 
                                           &optional again pstart)
  "Get NUM texts written by PERSNO. Args: persno num

Cannot be called from a callback."
  (let* ((persstat (blocking-do 'get-pers-stat persno))
         (user-area (pers-stat->user-area persstat)))

    (cond ((and again (null num)) (setq num lyskom-last-review-num))
          ((and again (< lyskom-last-review-num 0)) (setq num (- num))))

    (let* ((plow (or pstart (pers-stat->first-created-text persstat)))
           (phigh (1- (+ plow (pers-stat->no-of-created-texts persstat))))
           (result (if again
                       lyskom-last-review-saved-result-list
                     nil))
           (increment (if num (abs num)))
           (mark (cond (again lyskom-last-review-pmark)
                        ((and num (< num 0)) plow)
                        (t phigh)))
           (collector nil)
           (found nil)
           (start nil))
    

      (if (null num)
          (setq num (1+ phigh)
                mark phigh
                increment (1+ phigh)))

      (while (and (<= mark phigh)
                  (>= mark plow)
                  (> (abs num) (length result)))

        (setq increment (min lyskom-fetch-map-nos increment))
        (setq start (if (< num 0)
                        mark
                      (- mark (1- increment))))
        (if (< start 0)
            (progn
              (setq increment (- increment start))
              (setq start 0)))
      
        (setq found (delq user-area
			  (lyskom-remove-zeroes
			   (listify-vector
			    (map->text-nos
			     (blocking-do 'get-created-texts
					  persno
					  start
					  increment))))))

	(when pred
	  (let ((collector (make-collector)))
	    (lyskom-traverse x found
	      (initiate-get-text-stat 'main 
				      (lambda (x collector pred args)
					(when (and x (apply pred x args))
					  (collector-push 
					   (text-stat->text-no x)
					   collector)))
				      x collector pred args))

	    (lyskom-wait-queue 'main)
	    (setq found (nreverse (collector->value collector)))))

        (if (> num 0)
            (setq result (nconc found result)
                  mark (- mark increment))
          (setq result (nconc result found)
                mark (+ mark increment)))
        (if (null found)
            (setq increment (min lyskom-fetch-map-nos (* increment 2)))
          (setq increment (- (abs num) (length result)))))

      (setq lyskom-last-review-pmark mark)

      (if (> num 0)
          (progn
            (setq lyskom-last-review-saved-result-list 
                  (lyskom-nfirst (- (length result) num) result))
            (nthcdr (max 0 (- (length result) num)) result))
        (progn
          (setq lyskom-last-review-saved-result-list
                (nthcdr (- num) result))
          (lyskom-nfirst (- num)  result))))))

(defun lyskom-get-texts-to (confno num &optional again cstart filter)
  "From CONFNO get NUM texts.

If optional AGAIN is non-nil, repeat last review. If optional CSTART
is non-nil, then start from that local number. If optional FILTER
is non-nil, then only retrieve those texts for which FILTER (called 
with the text-stat) returns non-nil."
  (let ((confstat (blocking-do 'get-conf-stat confno)))

    (lyskom-check-review-access confstat t)

    (cond ((and again (null num)) (setq num lyskom-last-review-num))
          ((and again (< lyskom-last-review-num 0)) (setq num (- num))))

    (let* ((clow (or cstart (conf-stat->first-local-no confstat)))
           (chigh (1- (+ clow (conf-stat->no-of-texts confstat))))
           (result (if again 
                       lyskom-last-review-saved-result-list
                     nil))
           (start nil)
           (increment (and num (abs num)))
           (mark (cond (again lyskom-last-review-cmark)
                       ((and num (< num 0)) clow)
                       (t chigh))))

      (if (null num)
          (setq num (1+ chigh)
                increment (1+ chigh)
                mark chigh))

      (while (and (<= mark chigh)
                  (>= mark clow)
                  (> (abs num) (length result)))

        (setq increment (min lyskom-fetch-map-nos increment))
        (setq start (if (< num 0)
                        mark
                      (- mark (1- increment))))
        (if (< start 0)
            (progn
              (setq increment (- increment start))
              (setq start 0)))

        (let ((found (lyskom-remove-zeroes
                      (listify-vector
                       (map->text-nos
                        (blocking-do 'get-map
                                     confno
                                     start
                                     increment))))))
	  (when filter
	    (let ((collector (make-collector)))
	      (lyskom-traverse x found
		(initiate-get-text-stat 'main
					(lambda (x collector pred)
					  (when (and x (funcall pred x))
					    (collector-push
					     (text-stat->text-no x)
					     collector)))
					x collector filter))
	      (lyskom-wait-queue 'main)
	      (setq found (nreverse (collector->value collector)))))
					

          (if (> num 0)
              (setq result (nconc found result)
                    mark (- mark increment)
                    increment (- (abs num) (length result)))
            (setq result (nconc result found)
                  mark (+ mark increment)))
          (if (null found)
              (setq increment (min lyskom-fetch-map-nos (* increment 2)))
            (setq increment (- (abs num) (length result))))))

      (setq lyskom-last-review-cmark mark)

      (if (> num 0)
          (progn
            (setq lyskom-last-review-saved-result-list 
                  (lyskom-nfirst (- (length result) num) result))
            (nthcdr (max 0 (- (length result) num)) result))
        (progn
          (setq lyskom-last-review-saved-result-list
                (nthcdr (- num) result))
          (lyskom-nfirst (- num)  result))))))




;;; ============================================================
;;;                 Återse baklänges
;;;

(def-kom-command kom-review-backward ()
  "Toggles the reviewing order. If you are currently reviewing texts
oldest to newest, review newest to oldest instead. When reviewing
newest to oldest, change to oldest to newest.

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive)
  (cond
   ((and (not (read-list-isempty lyskom-reading-list))
	 (memq (read-info->type (read-list->first lyskom-reading-list))
		 '(REVIEW REVIEW-MARK)))
    (let* ((info (read-list->first lyskom-reading-list))
	   (list (read-info->text-list info))
	   (texts (text-list->texts list))
	   (forward (read-info->forward info)))
      (set-text-list->texts list (nreverse texts))
      (set-read-info->forward info (not forward))
      (lyskom-format-insert 'you-review 
			    (lyskom-get-string (if (not forward)
						   'forward
						 'backward)))))
   (t
    (lyskom-insert-string 'illegal-command))))


;;; ================================================================
;;;                   ]terse tr{det - review tree

;;; Author: Linus Tolke


(def-kom-command kom-review-tree (&optional text-no)
  "Recursively review all comments to the selected text.
This command will descend recursively in the comment tree, as when
reading texts normally. Unlike when reading normally, filters are
not applied and circular structures are not dealt with gracefully.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg').

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive (list (lyskom-read-text-no-prefix-arg 'review-tree-q)))
  (lyskom-tell-internat 'kom-tell-review)
  (if text-no
      (progn
        (unless kom-review-uses-cache
          (cache-del-text-stat text-no))
        
        (let ((ts (blocking-do 'get-text-stat text-no)))
          (lyskom-follow-comments ts
                                  nil 'review
                                  (lyskom-review-get-priority)
                                  t)))
    (lyskom-insert-string 'read-text-first)))


(def-kom-command kom-unread-tree (text-no)
    "Recursively mark all comments to the selected text as unread.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'unread-tree-q)))
  (lyskom-unread-tree text-no nil))

(defun lyskom-unread-tree (text-no visited-list)
  "Perform the function of kom-unread-tree."
  (let* ((text-stat (blocking-do 'get-text-stat text-no))
         (worklist (and text-stat
                        (lyskom-text-comments text-stat))))
    (if (null (car worklist))
        (lyskom-format-insert 'no-such-text-no text-no)
      (while worklist
        (let ((cur (car worklist)))
          (setq worklist (cdr worklist))
          (unless (memq cur visited-list)
            (setq visited-list (cons cur visited-list)
                  cur (blocking-do 'get-text-stat cur))
            (when cur
              (when (delq nil (mapcar 'lyskom-get-membership
                                      (lyskom-text-recipients cur)))
                (lyskom-format-insert 'marking-text-unread
                                      (text-stat->text-no cur))
                (lyskom-report-command-answer
                 (lyskom-mark-unread (text-stat->text-no cur))))
              (lyskom-traverse text-no (lyskom-text-comments cur)
                (unless (or (memq text-no visited-list)
                            (memq text-no worklist)
                  (setq worklist (cons text-no worklist)))))))))))
  visited-list)



(def-kom-command kom-find-root (text-no)
  "Finds the root text of the tree containing the selected text.
When there is more than one root, all will be included in a review
operation.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg').

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive (list (lyskom-read-text-no-prefix-arg 'find-root-q)))
  (lyskom-tell-internat 'kom-tell-review)
  (cond
   (text-no
    (unless kom-review-uses-cache
      (cache-del-text-stat text-no))
    (let* ((ts (blocking-do 'get-text-stat text-no))
	   (r (lyskom-find-root ts t)))
      (cond ((> (length r) 1)
             (lyskom-format-insert-before-prompt
              (lyskom-get-string 'more-than-one-root) ts)
             (lyskom-review-enter-read-info
              (lyskom-create-read-info
               'REVIEW
               nil
               (lyskom-review-get-priority)
               (lyskom-create-text-list r)
               nil t) t))
            (r (unless kom-review-uses-cache
                 (cache-del-text-stat (car r)))
               (lyskom-view-text (car r)))
            (t (lyskom-insert-string 'could-not-find-root)))
      )
    )
   (t
    (lyskom-insert-string 'confusion-what-to-find-root))))

(def-kom-command kom-unread-root (text-no)
  "Finds the root text of the tree containing the selected text
and marks it as unread.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'unread-root-q)))
  (if text-no
      (let* ((ts (blocking-do 'get-text-stat text-no))
             (r (lyskom-find-root ts t)))
        (if r
            (lyskom-traverse text-no r
              (lyskom-format-insert 'marking-text-unread text-no)
              (lyskom-report-command-answer
               (lyskom-mark-unread text-no)))
          (lyskom-insert-string 'could-not-find-root)))
    (lyskom-insert-string 'confusion-what-to-unread-root)))


(def-kom-command kom-find-root-review (text-no)
  "Finds the root of the comment tree containing the selected texts
and then recursively reviews all its comments. For texts with a single
root, this is equivalent to doing `kom-find-root' followed by
`kom-review-tree'.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg').

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive (list (lyskom-read-text-no-prefix-arg 'find-root-review-q)))
  (lyskom-tell-internat 'kom-tell-review)
  (cond
   (text-no
    (unless kom-review-uses-cache
      (cache-del-text-stat text-no))
    (let* ((ts (blocking-do 'get-text-stat text-no))
           (start (lyskom-find-root ts t)))
      (cond ((> (length start) 1)
             (lyskom-format-insert-before-prompt
              (lyskom-get-string 'more-than-one-root-review) ts)
             (lyskom-review-tree (car start)))
            (start (lyskom-review-tree (car start)))
            (t (lyskom-insert-string 'could-not-find-root)))))
   (t (lyskom-insert-string 'confusion-what-to-find-root-review))))

(def-kom-command kom-unread-root-review (text-no)
  "Finds the root of the comment tree containing the selected texts
and then recursively marks all its comments as unread. For texts with
a single root, this is equivalent to doing `kom-find-root' followed by
`kom-unread-tree'.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'unread-root-review-q)))
  (if text-no
      (let* ((ts (blocking-do 'get-text-stat text-no))
             (start (lyskom-find-root ts t))
             (marked nil))
        (lyskom-traverse text-no start
          (setq marked (lyskom-unread-tree text-no marked))))
    (lyskom-insert-string 'confusion-what-to-unread-root-review)))


(defun lyskom-find-root (text-stat &optional all)
  "Finds the root text of the tree containing TEXT-STAT.
If optional ALL is non-nil, then return a list of all roots, otherwise
just a list of the firt root found."
  (lyskom-find-dag-roots 
   (lambda (text-stat)
     (delq nil
	   (mapcar (lambda (misc)
		     (cond ((eq (misc-info->type misc) 'COMM-TO) 
			    (misc-info->comm-to misc))
			   ((eq (misc-info->type misc) 'FOOTN-TO) 
			    (misc-info->footn-to misc))))
		   (text-stat->misc-info-list text-stat))))
   (lambda (text-no)
     (unless kom-review-uses-cache
       (cache-del-text-stat text-no))
     (or (cache-get-text-stat text-no)
         (blocking-do 'get-text-stat text-no)))
   (text-stat->text-no text-stat)
   (not all)))




(defun lyskom-review-tree (text)
  "Takes a TEXT as an arg, shows the text and the tree of all comments.
Does a lyskom-end-of-command.
Text is a text-no."
  (cond
   ((integerp text)
    (unless kom-review-uses-cache
      (cache-del-text-stat text))
    (lyskom-view-text text nil t nil (lyskom-review-get-priority) t))
   (t
    (signal 'lyskom-internal-error
	    (list 'lyskom-review-tree
		  "Called with incorrect argument."
		  text)))))


;;; ================================================================
;;;                     ]terse n{sta -  review next

;;; Author: Linus Tolke


(def-kom-command kom-review-next ()
  "Resumes an interupted review by moving all suspended review entries
to the front. This command may be less-than-useful depending on how
review priorities are set, and the setting of
`kom-higher-priority-breaks'.

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive)
  (lyskom-tell-internat 'kom-tell-review)
  (let ((len (read-list-length lyskom-reading-list))
	(finished nil))
    (while (and (not finished)
		(> len 1))
      (let ((type (read-info->type (read-list->first lyskom-reading-list))))
	(if (and (not (eq type 'REVIEW))
		 (not (eq type 'REVIEW-TREE))
		 (not (eq type 'REVIEW-MARK)))
	    (read-list-rotate lyskom-reading-list)
	  (setq finished t)))
      (setq len (1- len)))))


;;; ================================================================
;;;                    ]terse stacken - Review stack

;;; Author: Linus


(def-kom-command kom-review-stack ()
  "Displays a list of suspended review commands, and possibly regular
read commands that have been suspended by reviewing.

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive)
  (if (read-list->all-entries lyskom-reading-list)
      (mapcar
       (lambda (info)
	 (let ((un (text-list->length (read-info->text-list info)))
	       (type (read-info->type info))
	       (cto (read-info->comm-to info))
	       (conf (read-info->conf-stat info)))
	   (cond
	    ((eq type 'COMM-IN)
	     (lyskom-format-insert 'view-many-comments cto un))
	    ((eq type 'CONF)
	     (lyskom-format-insert 'view-texts-in-conf un conf))
	    ((eq type 'REVIEW)
	     (lyskom-format-insert 'review-n-texts un))
	    ((eq type 'REVIEW-TREE)
	     (lyskom-format-insert 'review-many-comments cto un))
	    ((eq type 'REVIEW-MARK)
	     (lyskom-format-insert 'review-marked un)))))
       (read-list->all-entries lyskom-reading-list))
    (cond (lyskom-current-conf 
           (lyskom-format-insert 'you-have-no-unreads lyskom-current-conf))
          (t (lyskom-insert 'not-reading-anywhere)))))



;;; ================================================================
;;;                      ]terse hoppa - review clear

;;; Author: Linus Tolke


(def-kom-command kom-review-clear ()
  "Cancel all active and suspended review commands.

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive)
  (let ((found nil))
    (if (not (read-list-isempty lyskom-reading-list))
        (while (memq (read-info->type (read-list->first lyskom-reading-list))
                       lyskom-review-types-list)
          (set-read-list-del-first lyskom-reading-list)
          (setq found t)))
    (if (not (read-list-isempty lyskom-to-do-list))
        (while (memq (read-info->type (read-list->first lyskom-to-do-list))
                       lyskom-review-types-list)
          (set-read-list-del-first lyskom-to-do-list)
          (setq found t)))

    (unless found (lyskom-insert 'not-reviewing))))


;;; ================================================================
;;;          ]terse det kommenterade - View commented text

;;; Author: Inge Wallin


(def-kom-command kom-review-comments (text-no)
  "Review all comments to the selected text. This command only
reviews one level of comments. To see the entire comment tree, use
`kom-review-tree' instead.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg').

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive (list (lyskom-read-text-no-prefix-arg 'review-comments-q)))
  (lyskom-tell-internat 'kom-tell-review)
  (cond (text-no
         (unless kom-review-uses-cache
           (cache-del-text-stat text-no))
         (lyskom-review-comments
          (list (blocking-do 'get-text-stat text-no))))
        (t (lyskom-insert-string 'read-text-first))))

(def-kom-command kom-review-more-comments ()
  "Review all comments to texts recently viewed with `kom-review-comments'. 
This command only reviews one level of comments. To see the entire comment
tree, use `kom-review-tree' instead.

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive)
  (lyskom-tell-internat 'kom-tell-review)
  (cond
   (lyskom-last-review-comments
    (unless kom-review-uses-cache
      (mapc 'cache-del-text-stat lyskom-last-review-comments))
    (lyskom-review-comments
     (let ((collector (make-collector)))
       (mapc (lambda (text-no)
                 (initiate-get-text-stat 'main
                                         'collector-push
                                         text-no
                                         collector))
               lyskom-last-review-comments)
       (lyskom-wait-queue 'main)
       (delq nil (nreverse (collector->value collector))))))
   (t (lyskom-insert-string 'no-review-done))))


(defun lyskom-review-comments (text-stat-list)
  "Handles the return from the initiate-get-text-stat, displays and builds list."
  (let ((text-nos nil))
    (lyskom-traverse text-stat text-stat-list
      (let* ((misc-info-list (and text-stat
                                  (text-stat->misc-info-list text-stat)))
             (misc-infos (and misc-info-list
                              (append (lyskom-misc-infos-from-list 
                                       'FOOTN-IN misc-info-list)
                                      (lyskom-misc-infos-from-list 
                                       'COMM-IN misc-info-list))))
             (all-text-nos (and misc-infos
                                (mapcar
				 (lambda (misc-info)
				   (if (equal (misc-info->type misc-info)
					      'COMM-IN)
				       (misc-info->comm-in misc-info)
				     (misc-info->footn-in misc-info)))
                                 misc-infos))))
        ;; Only try to review texts that we can read.
        (while all-text-nos
          (unless kom-review-uses-cache
            (cache-del-text-stat (car all-text-nos)))
          (if (blocking-do 'get-text-stat (car all-text-nos))
              (setq text-nos (cons (car all-text-nos) text-nos)))
          (setq all-text-nos (cdr all-text-nos)))))

    (setq text-nos (nreverse text-nos))
    (if text-nos
	(progn
	  (lyskom-format-insert 'review-text-no (car text-nos))
	  (when (cdr text-nos)
            (lyskom-review-enter-read-info
             (lyskom-create-read-info
              'REVIEW nil (lyskom-review-get-priority)
              (lyskom-create-text-list (cdr text-nos))
              lyskom-current-text) t))
          (setq lyskom-last-review-comments text-nos)
          ;; Don't check the no-cache thing here since we already
          ;; did earlier. We may end up slightly out of sync with
          ;; the server, but not so anyone will really notice.
	  (lyskom-view-text (car text-nos)))
      (lyskom-insert-string 'no-such-text))))


(def-kom-command kom-review-cross-references (text-no)
  "Review all texts cross-referenced from the selected text.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg').

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive (list (lyskom-read-text-no-prefix-arg 'review-xrefs-q)))
  (lyskom-tell-internat 'kom-tell-review)
  (cond (text-no
         (unless kom-review-uses-cache
	   (cache-del-text-stat text-no))
         (lyskom-review-cross-references
          (blocking-do 'get-text-stat text-no)))
        (t (lyskom-insert-string 'read-text-first))))

(defun lyskom-cross-referenced-text (aux-item-xref)
  (and (string-match "^T\\([0-9]+\\)" (aux-item->data aux-item-xref))
       (lyskom-string-to-number (match-string 1 (aux-item->data aux-item-xref)))))

(defun lyskom-review-cross-references (text-stat)
  "Handles the return from the initiate-get-text-stat,
displays and builds list."
  (let* ((all-xrefs (lyskom-get-aux-item (text-stat->aux-items text-stat) 3))
	 (only-txts (mapcar 'lyskom-cross-referenced-text all-xrefs))
	 (to-review '()))

    ;; Only try to review texts that we can read.
    (while only-txts
      (when (car only-txts)
	(unless kom-review-uses-cache
	  (cache-del-text-stat (car only-txts)))
	(when (blocking-do 'get-text-stat (car only-txts))
	  (setq to-review (cons (car only-txts) to-review))))
      (setq only-txts (cdr only-txts)))
    (setq to-review (nreverse to-review))

    (if to-review
	(progn
	  (lyskom-format-insert 'review-text-no (car to-review))
	  (when (cdr to-review)
	    (lyskom-review-enter-read-info
	     (lyskom-create-read-info
	      'REVIEW nil (lyskom-review-get-priority)
	      (lyskom-create-text-list (cdr to-review))
	      lyskom-current-text) t))
          ;; Don't check the no-cache thing here since we already
          ;; did earlier. We may end up slightly out of sync with
          ;; the server, but not so anyone will really notice.
	  (lyskom-view-text (car to-review)))
      (lyskom-insert-string 'no-such-text))))




(def-kom-command kom-unread-comments (text-no)
  "Mark all comments to the selected text text as unread. This
command only marks one level of comments as unread. To mark the 
entire comment tree unread, use `kom-unread-tree' instead.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'unread-comments-q)))
  (cond (text-no
         (lyskom-unread-comments
          (blocking-do 'get-text-stat text-no)))
        (t (lyskom-insert-string 'read-text-first))))


(defun lyskom-unread-comments (text-stat)
  "Handles the return from the initiate-get-text-stat, displays and builds list."
  (let* ((misc-info-list (and text-stat
			      (text-stat->misc-info-list text-stat)))
	 (misc-infos (and misc-info-list
			  (append (lyskom-misc-infos-from-list 
				   'FOOTN-IN misc-info-list)
				  (lyskom-misc-infos-from-list 
				   'COMM-IN misc-info-list))))
	 (all-text-nos (and misc-infos
                            (mapcar
                             (lambda (misc-info)
                               (if (equal (misc-info->type misc-info)
                                          'COMM-IN)
                                   (misc-info->comm-in misc-info)
                                 (misc-info->footn-in misc-info)))
                             misc-infos)))
         text-nos)
    ;; Only try to review texts that we can read.
    (while all-text-nos
      (if (blocking-do 'get-text-stat (car all-text-nos))
          (setq text-nos (cons (car all-text-nos) text-nos)))
      (setq all-text-nos (cdr all-text-nos)))
    (setq text-nos (nreverse text-nos))

    (if text-nos
        (lyskom-traverse text-no text-nos
          (lyskom-format-insert 'marking-text-unread text-no)
          (lyskom-report-command-answer
           (lyskom-mark-unread text-no)))
      (lyskom-insert-string 'no-such-text))))



;;; ================================================================
;;;          ]terse igen - kom-review-last-normally-read
;;;
;;; Author: Linus Tolke


(def-kom-command kom-review-last-normally-read (no)
  "Reviews the N texts most recently read using normal commands.
After reviewing a number of texts, this will show the N texts
read prior to that.

See `kom-review-uses-cache', `kom-review-priority' and
`kom-review-marks-texts-as-read' for information on settings that affect
all review-related functions."
  (interactive 
   (list 
    (lyskom-read-number (lyskom-get-string 'read-normally-read) 1)))
  (lyskom-tell-internat 'kom-tell-review)
  (let* ((text-nos (nreverse (lyskom-nfirst no lyskom-normally-read-texts))))
    (if text-nos
	(progn
	  (lyskom-format-insert 'review-text-no (car text-nos))
	  (if (cdr text-nos)
	      (lyskom-review-enter-read-info
               (lyskom-create-read-info
                'REVIEW nil (lyskom-review-get-priority)
                (lyskom-create-text-list (cdr text-nos))
                lyskom-current-text) t))
          (unless kom-review-uses-cache
            (cache-del-text-stat (car text-nos)))
	  (lyskom-view-text (car text-nos)))
      (lyskom-format-insert 'no-such-text))))


(def-kom-command kom-unread-last-normally-read (no)
  "Marks the N texts most recently read using normal commands unread.
After marking a number of texts unread, this will mark the N texts
read prior to that."
  (interactive 
   (list (lyskom-read-number (lyskom-get-string 'read-normally-read) 1)))
  (let* ((text-nos (nreverse (lyskom-nfirst no lyskom-normally-read-texts))))
    (if text-nos
        (lyskom-traverse text-no text-nos
          (lyskom-format-insert 'marking-text-unread text-no)
          (lyskom-report-command-answer
           (lyskom-mark-unread text-no)))
      (lyskom-format-insert 'no-such-text))))


;;; ============================================================
;;;        Återse omodifierat - Review a non-converted text

;;; Author: Linus Tolke
;;; Modified by: Johan Sundström

(def-kom-command kom-review-noconversion (text-no)
  "Displays the selected text without any conversions or filtering
\(i.e. no conversion of HTML, no automatic line breaks, all header
lines etc).

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'review-noconversion-q)))
  (if text-no
      (let ((lyskom-format-special nil)
            (kom-smileys nil)
	    (kom-show-footnotes-immediately nil)
            (kom-show-mx-date-as-local nil)
            (kom-print-relative-dates nil)
            (kom-print-seconds-in-time-strings t)
            (kom-autowrap nil))
          (unless kom-review-uses-cache
            (cache-del-text-stat text-no))
        (lyskom-view-text text-no))
    (lyskom-insert 'confusion-what-to-view)))

;;; ============================================================
;;;        Återse konverterat - Review a text converted

(def-kom-command kom-review-converted (text-no)
  "Displays the selected text with all conversions enabled, even if
some or all of them have are disabled by current settings.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'review-converted-q)))
  (if text-no
      (let ((kom-smileys t)
            (kom-autowrap t)
            (kom-format-html-authors '((t . t))))
        (unless kom-review-uses-cache
          (cache-del-text-stat text-no))
        (lyskom-view-text text-no))
    (lyskom-insert 'confusion-what-to-view)))

;;; ============================================================
;;;        Återse rot13 - Review a text rot13:ed

(def-kom-command kom-review-rot13 (text-no)
  "Displays the selected text rot13:ed.

This command accepts text number prefix arguments \(see
`lyskom-read-text-no-prefix-arg')."
  (interactive (list (lyskom-read-text-no-prefix-arg 'review-rot13-q)))
  (if text-no
      (unwind-protect
          (progn (add-hook 'kom-view-text-hook 'lyskom-filter-rot13)
                 (unless kom-review-uses-cache (cache-del-text-stat text-no))
                 (lyskom-view-text text-no))
        (remove-hook 'kom-view-text-hook 'lyskom-filter-rot13)
        (remove-hook 'kom-view-text-hook 'lyskom-filter-rot13 t))
    (lyskom-insert 'confusion-what-to-view)))

(defun lyskom-review-get-priority ()
  "Get the priority to use for reviewing texts."
  (or kom-review-priority (lyskom-get-current-priority)))

(defun lyskom-review-enter-read-info (read-info before)
  "Enter READ-INFO into lyskom-reading-list and lyskom-to-do-list."
  (read-list-enter-read-info read-info lyskom-reading-list before)
  (read-list-enter-read-info read-info lyskom-to-do-list before))


(defun lyskom-review-filter-roots (text-stat)
  "Return non-nil if TEXT-STATE is a comment"
  (cond ((lyskom-text-stat-p text-stat)
	 (not (lyskom-traverse misc (text-stat->misc-info-list text-stat)
		(when (memq (misc-info->type misc)
			    '(COMM-TO FOOTN-TO))
		  (lyskom-traverse-break t)))))
	((eq text-stat 'description)
	 (lyskom-get-string 'review-filter-roots))))

(defun lyskom-review-filter-results (results filter)
  "Filter review RESULTS according to FILTER"
  (if filter
      (let ((collector (make-collector)))
	(lyskom-traverse x results
	  (initiate-get-text-stat 'main
				  (lambda (x collector pred)
				    (when (and x (funcall pred x))
				      (collector-push
				       (text-stat->text-no x)
				       collector)))
				  x collector filter))
	(lyskom-wait-queue 'main)
	(nreverse (collector->value collector)))
    results))
