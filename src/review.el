;;;;;
;;;;; $Id: review.el,v 44.10 1997-10-23 12:19:16 byers Exp $
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
	      "$Id: review.el,v 44.10 1997-10-23 12:19:16 byers Exp $\n"))

(eval-when-compile
  (require 'lyskom-command "command"))

(put 'lyskom-cant-review-error
     'error-conditions
     '(error lyskom-error lyskom-review-error))
(put 'lyskom-review-error 'error-message
     "Review error")


;;; ======================================================================
;;; State-saving variables
;;;

(defvar lyskom-last-review-by nil)
(defvar lyskom-last-review-to nil)
(defvar lyskom-last-review-num nil)
(defvar lyskom-last-review-pmark nil)
(defvar lyskom-last-review-cmark nil)
(defvar lyskom-last-review-saved-result-list nil)
(defvar lyskom-last-review-saved-by-list nil)
(defvar lyskom-last-review-saved-to-list nil)
(defvar lyskom-last-review-saved-result-size 0)
(defvar lyskom-last-review-saved-smallest nil)
(defvar lyskom-last-review-saved-largest nil)
(defvar lyskom-have-review nil)


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

(defun lyskom-remove-zeroes (a)
  "Returns a copy of list where all zeroes are removed."
  (delq 0 (copy-sequence a)))


;;; ================================================================
;;;              ]terse av, till - Review by X to Conference Y.

;;; Author: David Byers


(def-kom-command kom-review-all ()
  "Review every articles of an author written to a conference."
  (interactive)
  (lyskom-tell-internat 'kom-tell-review)
  (lyskom-review-by-to 0))


(def-kom-command kom-review-more (&optional count)
  "Review more articlies using the same critera as the last review."
  (interactive "P")
  (if (not lyskom-have-review)
      (lyskom-format-insert 'no-review-done)
    (let* ((count (lyskom-read-number
                   (lyskom-get-string 'review-how-many-more)
                   (abs lyskom-last-review-num)))
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
           (to lyskom-last-review-to))

      (lyskom-format-insert 'review-more-info-by-to
                            info
                            (if (zerop by)
                                (lyskom-get-string 'anybody)
                              by)
                            (if (zerop to)
                                (lyskom-get-string 'all-confs)
                              to))
    
      (condition-case arg
          (let ((list (lyskom-get-texts-by-to by to count t)))
            (setq lyskom-last-review-num 
                  (if (< lyskom-last-review-num 0)
                      (- count)
                    count))
            (if list
                (read-list-enter-read-info (lyskom-create-read-info
                                            'REVIEW
                                            nil
                                            (lyskom-get-current-priority)
                                            (lyskom-create-text-list list)
                                            nil t)
                                           lyskom-reading-list t)
              (lyskom-insert-string 'no-such-text)))
        (lyskom-review-error (if arg
                                 nil
                               (lyskom-insert-string 'no-such-text)))))))

                       

(def-kom-command kom-review-first (&optional count)
  "Reviews all articles of author that is written to conference recipient.
If return is given instead of an author then all authors to that conference is
shown. If return is given instead of conference then all conferences for that
person is chosen.
If a negative numeric argument is given then only the last COUNT articles are 
chosen. If the argument is positive then the first -COUNT articles are chosen.
If the argument is zero the all articles are chosen.
No argument is equivalent to COUNT 1.
The defaults for this command is the conference that you are in."
  (interactive "P")
  (lyskom-tell-internat 'kom-tell-review)
  (lyskom-review-by-to (- (or count
                           (lyskom-read-number
                            (lyskom-get-string 'review-how-many) 1)))))


(def-kom-command kom-review-by-to (&optional count)
  "Reviews all articles of author that is written to conference recipient.
If return is given instead of an author then all authors to that conference is
shown. If return is given instead of conference then all conferences for that
person is chosen.
If a positive numeric argument is given then only the last COUNT articles are 
chosen. If the argument is negative then the first -COUNT articles are chosen.
If the argument is zero the all articles are chosen.
No argument is equivalent to COUNT 1.
The defaults for this command is the conference that you are in."
  (interactive "P")
  (lyskom-review-by-to (or count
                           (lyskom-read-number
                            (lyskom-get-string 'review-how-many) 1))))
  


(defun lyskom-review-by-to (count)
  "Common function for kom-review-by-to and kom-review-first"
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
              (lyskom-format 'review-info (lyskom-format 'info-by-whom info))
              '(pers) t nil t))
         (to (lyskom-read-conf-no 
              (lyskom-format 'review-info
                             (lyskom-format 'info-to-conf info))
              '(all) 
              t
              ;; If person is not given we must give
              ;; conf  -- Not anymore!
              ;; (not (zerop by))
              (if (zerop lyskom-current-conf)
                  ""
                (cons (conf-stat->name
                         (blocking-do 'get-conf-stat
                                      lyskom-current-conf)) 0))
              t)))

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
                            to))

    (setq lyskom-last-review-by by)
    (setq lyskom-last-review-to to)
    (setq lyskom-last-review-num count)
    (setq lyskom-last-review-pmark nil)
    (setq lyskom-last-review-cmark nil)
    (setq lyskom-last-review-saved-result-list nil)
    (setq lyskom-last-review-saved-by-list nil)
    (setq lyskom-last-review-saved-to-list nil)
    (setq lyskom-last-review-saved-result-size 0)
    (setq lyskom-last-review-saved-smallest nil)
    (setq lyskom-last-review-saved-largest nil)
    (setq lyskom-have-review t)

    (condition-case arg
        (let ((list (lyskom-get-texts-by-to by to count)))
          (if list
              (read-list-enter-read-info (lyskom-create-read-info
                                          'REVIEW
                                          nil
                                          (lyskom-get-current-priority)
                                          (lyskom-create-text-list list)
                                          nil t)
                                         lyskom-reading-list t)
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


(defun lyskom-get-texts-by-to (by to num &optional again)
  "Get NUM texts written by person number BY in conference number TO
Args: BY TO NUM"
  (cond ((and (zerop by) 
              (zerop to)) (lyskom-get-texts-globally num again))
        ((zerop to) (lyskom-get-texts-by by num again))
        ((zerop by) (lyskom-get-texts-to to num again))
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
          (function 
           (lambda (x to)
             (let ((found nil))
               (lyskom-traverse misc (text-stat->misc-info-list x)
                 (setq found 
                       (or found
                           (and (or (eq (misc-info->type misc) 'RECPT)
                                    (eq (misc-info->type misc) 'CC-RECPT)
                                    (eq (misc-info->type misc) 'BCC-RECPT))
                                (eq (misc-info->recipient-no misc) to)))))
               found)))
          (list to)
          again))
         (t (lyskom-get-texts-by-and-to by to num again))))


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

(defun lyskom-get-texts-globally (num &optional again)
  "Get the last NUM texts created in LysKOM. If AGAIN is non-nil, keep
going from where we were before."
  (cond ((and again (null num))
         (setq num lyskom-last-review-num))
        ((and again (< lyskom-last-review-num 0)) 
         (setq num (- num)))
        ((null num) 
         (lyskom-format-insert 'cant-review-everything)
         (signal 'lyskom-cant-review-error t)))

  (let ((result nil)
        (textno (cond (again lyskom-last-review-cmark)
                      ((< num 0) 1)
                      (t (lyskom-maxint))))
        (op (if (< num 0)
                'find-next-text-no
              'find-previous-text-no)))

    (while (and (not (eq textno 0))
                (not (null textno))
                (< (length result) (abs num)))
      (setq textno (blocking-do op textno))
      (if textno
          (setq result (cons textno result))))
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

(defun lyskom-get-letters-to (persno recipient num &optional again pstart)
  "Get NUM texts written by PERSNO. Args: persno num"
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
           (collector nil)
           (found nil)
           (start nil)
           (data nil))
    

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
      
        (setq data (lyskom-remove-zeroes
                    (listify-vector
                     (map->text-nos
                      (blocking-do 'get-created-texts
                                   persno
                                   start
                                   increment)))))

        (setq collector (make-collector))
        (mapcar
         (function
          (lambda (x)
            (initiate-get-text-stat 
             'main 
             (function
              (lambda (x collector pers-no)
                (if (and x
                         (lyskom-is-recipient x pers-no))
                    (collector-push (text-stat->text-no x) collector))))
             x
             collector
             recipient)))
         data)

        (lyskom-wait-queue 'main)
        (setq found (nreverse (collector->value collector)))

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
                  (nfirst (- (length result) num) result))
            (nthcdr (- (length result) num) result))
        (progn
          (setq lyskom-last-review-saved-result-list
                (nthcdr (- num) result))
          (nfirst (- num)  result))))))






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
;;; to-list we have retreived so far.
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
                                          &optional again pstart cstart)
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
           (or (conf-type->letterbox (conf-stat->type confstat))
               (and (conf-type->rd_prot (conf-stat->type confstat))
                    (null (map->text-nos 
                           (blocking-do 'get-map
                                        (conf-stat->conf-no conf)
                                        (conf-stat->first-local-no conf)
                                        1)))))
           (not (eq persno confno)))
      (lyskom-get-letters-to persno confno num again pstart))

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
                              (function
                               (lambda (x)
                                 (lyskom-intersection to x)))
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
                (mapcar2 (function
                          (lambda (x y)
                            (lyskom-intersection y
                                                 (nconc x by))))
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
                           (mapcar (function (lambda (x)
                                               (if x
                                                   (apply 'min x)
                                                 (lyskom-maxint))))
                                   to-list))
                    largest
                    (apply 'max
                           (mapcar (function (lambda (x)
                                               (if x
                                                   (apply 'max x)
                                                 -1)))
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
                    (nfirst (- (length result-list) num) result-list))
              (setq lyskom-last-review-saved-result-size
                    (length  lyskom-last-review-saved-result-list))
              (setq lyskom-last-review-saved-result-list
                    (cons lyskom-last-review-saved-result-list
                          (make-list (- (length by-list) 1) nil)))
              (nthcdr (- (length result-list) num) result-list))

          (progn
            (setq lyskom-last-review-saved-result-list 
                  (nthcdr (- num) result-list))
            (setq lyskom-last-review-saved-result-size
                  (length lyskom-last-review-saved-result-list))
            (setq lyskom-last-review-saved-result-list
                  (cons lyskom-last-review-saved-result-list
                        (make-list (- (length by-list) 1) nil)))
            (nfirst (- num) result-list))))))))



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

(defun lyskom-get-texts-by (persno num &optional again pstart)
  "Get NUM texts written by PERSNO. Args: persno num"
  (let ((persstat (blocking-do 'get-pers-stat persno)))
    (lyskom-check-review-access t persstat)
    (lyskom-get-texts-by-generic persno num nil nil again pstart)))

(defun lyskom-get-texts-by-generic (persno num pred args 
                                           &optional again pstart)
  "Get NUM texts written by PERSNO. Args: persno num"
  (let ((persstat (blocking-do 'get-pers-stat persno)))

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
           (start nil)
           (data nil))
    

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
      
        (setq data (lyskom-remove-zeroes
                    (listify-vector
                     (map->text-nos
                      (blocking-do 'get-created-texts
                                   persno
                                   start
                                   increment)))))

        (setq collector (make-collector))
        (mapcar
         (function
          (lambda (x)
            (initiate-get-text-stat 'main 
                                    (function
                                     (lambda (x collector pred args)
                                       (when (and x
                                                  (or (null pred)
                                                      (apply pred
                                                             x
                                                             args)))
                                           (collector-push 
                                            (text-stat->text-no x)
                                            collector))))
                                    x collector pred args)))
         data)

        (lyskom-wait-queue 'main)
        (setq found (nreverse (collector->value collector)))

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
                  (nfirst (- (length result) num) result))
            (nthcdr (- (length result) num) result))
        (progn
          (setq lyskom-last-review-saved-result-list
                (nthcdr (- num) result))
          (nfirst (- num)  result))))))

(defun lyskom-get-texts-to (confno num &optional again cstart)
  "From CONFNO get NUM texts."
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
                  (nfirst (- (length result) num) result))
            (nthcdr (- (length result) num) result))
        (progn
          (setq lyskom-last-review-saved-result-list
                (nthcdr (- num) result))
          (nfirst (- num)  result))))))




;;; ============================================================
;;;                 Återse baklänges
;;;

(def-kom-command kom-review-backward ()
  "Toggles the reviewing order.
If reading forward then starts reading backward and the other way round."
  (interactive)
  (cond
   ((and (not (read-list-isempty lyskom-reading-list))
	 (or (eq (read-info->type (read-list->first lyskom-reading-list))
		 'REVIEW)
	     (eq (read-info->type (read-list->first lyskom-reading-list))
		 'REVIEW-MARK)))
    (let* ((info (read-list->first lyskom-reading-list))
	   (list (read-info->text-list info))
	   (texts (cdr list))
	   (forward (read-info->forward info)))
      (setcdr list (nreverse texts))
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
  "Review all comments to this text.
Descends recursively in the comment-tree without marking the texts as read.
The tree is forgotten when a kom-go-to-next-conf command is issued.
If optional prefix argument TEXT-NO is present view tree from that text 
instead. In this case the text TEXT-NO is first shown." 
  (interactive (list
		(cond
		 ((null current-prefix-arg)
		  lyskom-current-text)
		 ((integerp current-prefix-arg)
		  current-prefix-arg)
		 (t
		  (signal 'lyskom-internat-error '(kom-review-tree))))))
  (lyskom-tell-internat 'kom-tell-review)
  (if text-no
      (let ((ts (blocking-do 'get-text-stat text-no)))
	(lyskom-follow-comments ts
				nil 'review
				(lyskom-get-current-priority)
				t))
    (lyskom-insert-string 'read-text-first)))


(def-kom-command kom-find-root (&optional text-no)
  "Finds the root text of the tree containing the text in lyskom-current-text."
  (interactive)
  (lyskom-tell-internat 'kom-tell-review)
  (cond
   (lyskom-current-text 
    (let* ((ts (blocking-do 'get-text-stat (or text-no 
					       lyskom-current-text)))
	   (r (lyskom-find-root ts t)))
      (cond ((> (length r) 1)
             (lyskom-format-insert-before-prompt
              (lyskom-get-string 'more-than-one-root)
              ts)
             (read-list-enter-read-info (lyskom-create-read-info
                                         'REVIEW
                                         nil
                                         (lyskom-get-current-priority)
                                         (lyskom-create-text-list r)
                                         nil t)
                                        lyskom-reading-list t))
            (r (lyskom-view-text (car r)))
            (t (signal 'lyskom-internal-error "Could not find root")))
      )
    )
   (t
    (lyskom-insert-string 'read-text-first))))


(def-kom-command kom-find-root-review ()
  "Finds the root text of the tree containing the text in lyskom-current-text and
reviews the whole tree in deep-first order."
  (interactive)
  (lyskom-tell-internat 'kom-tell-review)
  (cond
   (lyskom-current-text
    (let* ((ts (blocking-do 'get-text-stat lyskom-current-text))
           (start (lyskom-find-root ts t)))
      (cond ((> (length start) 1)
             (lyskom-format-insert-before-prompt
              (lyskom-get-string 'more-than-one-root-review) ts)
             (lyskom-review-tree (car start)))
            (start (lyskom-review-tree (car start)))
            (t (signal 'lyskom-internal-error "Could not find root")))))
   (t (lyskom-insert-string 'read-text-first))))


(defun lyskom-find-root (text-stat &optional all)
  "Finds the root text of the tree containing the text TEXT-STAT.
Args: TEXT-STAT &optional ALL
If ALL is set, return a list of all root texts."  
  (cond (text-stat
         (let ((queue (list text-stat))
               (head nil)
               (misclist nil)
               (tmp nil)
               (result nil))
           (while queue
             (setq head (car queue))
             (setq queue (cdr queue))
             (setq tmp nil)
             
             ;;
             ;; For each parent, add it to the queue
             ;;
               
             (setq misclist (text-stat->misc-info-list head))
             (while misclist
               (cond ((eq (misc-info->type (car misclist)) 'COMM-TO)
                      (setq tmp
                            (cons
                             (blocking-do 'get-text-stat
                                          (misc-info->comm-to (car misclist)))
                             tmp)))
                     ((eq (misc-info->type (car misclist)) 'FOOTN-TO)
                      (setq tmp 
                            (cons
                             (blocking-do 'get-text-stat
                                          (misc-info->footn-to (car misclist)))
                             tmp)))) 
               (setq misclist (cdr misclist)))

	     ;;
	     ;; Remove unreadable texts
	     ;;

	     (setq tmp (delq nil tmp))

             ;;
             ;; If no parents were found, this is is a top-level text
             ;;

             (when (null tmp)
               (setq result (cons head result))
               (if (not all) (setq queue nil)))
             (setq queue (nconc tmp queue)))
           (if all (mapcar 'text-stat->text-no result)
             (text-stat->text-no (car result)))))
        (t nil)))




(defun lyskom-review-tree (text)
  "Takes a TEXT as an arg, shows the text and the tree of all comments.
Does a lyskom-end-of-command.
Text is a text-no."
  (cond
   ((integerp text)
    (lyskom-view-text text nil t nil (lyskom-get-current-priority) t))
   (t
    (signal 'lyskom-internal-error
	    (list 'lyskom-review-tree
		  "Called with incorrect argument."
		  text)))))


;;; ================================================================
;;;                     ]terse n{sta -  review next

;;; Author: Linus Tolke


(def-kom-command kom-review-next ()
  "Resumes an interupted review by moving all review and review-tree entries in
the lyskom-reading-list to the beginning. i.e by moving all other types to the
end."
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
  "Displays the review-stack."
  (interactive)
  (mapcar
   (function
    (lambda (info)
      (let ((un (length (cdr (read-info->text-list info))))
	    (type (read-info->type info))
	    (cto (read-info->comm-to info)))
	(cond
	 ((eq type 'REVIEW)
	  (lyskom-format-insert 'review-n-texts un))
	 ((eq type 'REVIEW-TREE)
	  ; +++ Hmmm. Pluralformer. Besv{rligt!
	  (if (= un 1)
	      (lyskom-format-insert 'review-one-comment cto)
	    (lyskom-format-insert 'review-many-comments cto un)))
	 ((eq type 'REVIEW-MARK)
	  (lyskom-format-insert 'review-marked un))))))
   (read-list->all-entries lyskom-reading-list)))


;;; ================================================================
;;;                      ]terse hoppa - review clear

;;; Author: Linus Tolke


(def-kom-command kom-review-clear ()
  "Deletes all review-types from the lyskom-reading-list and lyskom-to-do-list."
  (interactive)
  (if (not (read-list-isempty lyskom-reading-list))
      (while (or (eq (read-info->type (read-list->first lyskom-reading-list))
		     'REVIEW)
		 (eq (read-info->type (read-list->first lyskom-reading-list))
		     'REVIEW-TREE)
		 (eq (read-info->type (read-list->first lyskom-reading-list))
		     'REVIEW-MARK))
	(set-read-list-del-first lyskom-reading-list)))
  (if (not (read-list-isempty lyskom-to-do-list))
      (while (or (eq (read-info->type (read-list->first lyskom-to-do-list))
		     'REVIEW)
		 (eq (read-info->type (read-list->first lyskom-to-do-list))
		     'REVIEW-TREE)
		 (eq (read-info->type (read-list->first lyskom-to-do-list))
		     'REVIEW-MARK))
	(set-read-list-del-first lyskom-to-do-list))))


;;; ================================================================
;;;          ]terse det kommenterade - View commented text

;;; Author: Inge Wallin


(def-kom-command kom-review-comments ()
  "View the comments to this text.
If the current text has comments in (footnotes in) some texts then the first
text is shown and a REVIEW list is built to shown the other ones."
  (interactive)
  (lyskom-tell-internat 'kom-tell-review)
  (lyskom-review-comments
   (blocking-do 'get-text-stat lyskom-current-text)))


(defun lyskom-review-comments (text-stat)
  "Handles the return from the initiate-get-text-stat, displays and builds list."
  (let* ((misc-info-list (and text-stat
			      (text-stat->misc-info-list text-stat)))
	 (misc-infos (and misc-info-list
			  (append (lyskom-misc-infos-from-list 
				   'FOOTN-IN misc-info-list)
				  (lyskom-misc-infos-from-list 
				   'COMM-IN misc-info-list))))
	 (text-nos (and misc-infos
			(mapcar
			 (function
			  (lambda (misc-info)
			    (if (equal (misc-info->type misc-info)
				       'COMM-IN)
				(misc-info->comm-in misc-info)
			      (misc-info->footn-in misc-info))))
			 misc-infos))))
    (if text-nos
	(progn
	  (lyskom-format-insert 'review-text-no (car text-nos))
	  (if (cdr text-nos)
	      (read-list-enter-read-info
	       (lyskom-create-read-info
		'REVIEW nil (lyskom-get-current-priority)
		(lyskom-create-text-list (cdr text-nos))
		lyskom-current-text)
	       lyskom-reading-list t))
	  (lyskom-view-text (car text-nos)))
      (lyskom-insert-string 'no-such-text))))


;;; ================================================================
;;;          ]terse igen - kom-review-last-normally-read
;;;
;;; Author: Linus Tolke


(def-kom-command kom-review-last-normally-read (no)
  "Reviews the NO last normally read texts."
  (interactive 
   (list 
    (lyskom-read-number (lyskom-get-string 'read-normally-read) 1)))
  (lyskom-tell-internat 'kom-tell-review)
  (let* ((text-nos (nreverse (nfirst no lyskom-normally-read-texts))))
    (if text-nos
	(progn
	  (lyskom-format-insert 'review-text-no (car text-nos))
	  (if (cdr text-nos)
	      (read-list-enter-read-info
	       (lyskom-create-read-info
		'REVIEW nil (lyskom-get-current-priority)
		(lyskom-create-text-list (cdr text-nos))
		lyskom-current-text)
	       lyskom-reading-list t))
	  (lyskom-view-text (car text-nos)))
      (lyskom-format-insert 'no-such-text))))


;; Review a non-converted text
;; Author: Linus Tolke
 

(defun kom-review-noconversion (&optional text-no)
  "Displays the last read text without any conversion."
  (interactive (list 
		(cond
		 ((null current-prefix-arg)
		  lyskom-current-text)
		 ((integerp current-prefix-arg)
		  current-prefix-arg)
		 ((and (listp current-prefix-arg) 
		       (integerp (car current-prefix-arg)) 
		       (null (cdr current-prefix-arg)))
		  (car current-prefix-arg))
		 (t
		  (signal 'lyskom-internal-error '(kom-review-noconversion))))))
  (lyskom-start-of-command 'kom-review-noconversion)
  (let ((kom-emacs-knows-iso-8859-1 t)
        (lyskom-format-special nil)
        (kom-autowrap nil))
    (ignore kom-emacs-knows-iso-8859-1)
    (lyskom-view-text text-no))
  (lyskom-end-of-command))



;;; ============================================================
;;;         Återse senaste dagarnas inlägg
;;;
;;; Author: David Byers

;;;
;;; Algorithm:
;;;
;;; Binärsökning i mappen efter inlägg med rätt datum.
;;;
;;; Utilityfunktioner: Beräkna diff i dagar mellan två datum.
;;;                    Subtrahera n dagar från ett datum.
;;;
;;; Användarkommandot: Återse N inlägg av person X till Y från DATE
;;;                    
;;; Hitta index i X och Y där det sökta datumet börjar
;;; Låt de vanliga återsefunktionerna accepter cmin och pmin som
;;; parametrar så de kan söka igenom en del av en map.
;;;
;;; Problem med binärsökning i mapparna är hålen. Det finns risk att
;;; man försöker binärsöka bland en massa nollor, och det lär ta tid.
;;; I de fallen får man nog göra get-next-text eller get-previous-text
;;; för att få ett riktigt textnummer att titta på. Risk: att man
;;; tittar på samma text två gånger och går i loop.
;;;

