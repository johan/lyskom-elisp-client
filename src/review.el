;;;;;
;;;;; $Id: review.el,v 41.2 1996-05-12 12:28:27 byers Exp $
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
	      "$Id: review.el,v 41.2 1996-05-12 12:28:27 byers Exp $\n"))

(put 'lyskom-cant-review-error
     'error-conditions
     '(error lyskom-error lyskom-review-error))
(put 'lyskom-review-error 'error-message
     "Review error")


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

;;; Author: David Byers, David Kågedal, Linus Tolke


(def-kom-command kom-review-all ()
  "Review every articles of an author written to a conference."
  (interactive)
  (lyskom-tell-internat 'kom-tell-review)
  (lyskom-review-by-to 0))


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
              'pers 'empty nil t))
         (to (lyskom-read-conf-no 
              (lyskom-format 'review-info
                             (lyskom-format 'info-to-conf info))
              'all 
              t
              ;; If person is not given we must give
              ;; conf  -- Not anymore!
              ;; (not (zerop by))
              (if (zerop lyskom-current-conf)
                  ""
                (conf-stat->name
                 (blocking-do 'get-conf-stat
                              lyskom-current-conf)))
              t)))

    (if (not (zerop to))
        (cache-del-conf-stat to))
    (if (not (zerop by)) 
        (cache-del-pers-stat by))

    (blocking-do-multiple ((info-by (get-conf-stat by))
                           (info-to (get-conf-stat to)))
      (lyskom-format-insert 'review-info-by-to
                            info
                            (if (zerop by)
                                (lyskom-get-string 'anybody)
                              info-by)
                            (if (zerop to)
                                (lyskom-get-string 'all-confs)
                              info-to)))

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


(defun lyskom-get-texts-by-to (by to num)
  "Get NUM texts writteb by person number BY in conference number TO
Args: BY TO NUM"
  (cond ((and (zerop by) 
              (zerop to)) (lyskom-get-texts-globally num))
        ((zerop to) (lyskom-get-texts-by by num))
        ((zerop by) (lyskom-get-texts-to to num))
        (t (lyskom-get-texts-by-and-to by to num))))


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

(defun lyskom-get-texts-globally (num)
  "Get the last NUM texts created in LysKOM"
  (if (null num)
      (progn
        (lyskom-format-insert 'cant-review-everything)
        (signal 'lyskom-cant-review-error t)))
  (let ((result nil)
        (textno (if (< num 0)
                    1
                  (lyskom-maxint)))
        (op (if (< num 0)
                'find-next-text-no
              'find-previous-text-no)))

    (while (and (not (eq textno 0))
                (< (length result) (abs num)))
      (setq textno (blocking-do op textno))
      (setq result (cons textno result)))
    (if (< num 0)
        (nreverse result)
      result)))


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

(defun lyskom-get-texts-by-and-to (persno confno num)
  "Get NUM texts written by person PERSNO with conference CONFNO as a
recipient. 
Args: persno confno num"
  (blocking-do-multiple ((persstat (get-pers-stat persno))
                         (confstat (get-conf-stat confno)))
    (lyskom-check-review-access confstat persstat)

    (let* ((result-list nil)
           (by-list nil)
           (to-list nil)
           (result-size 0)
           (by nil)
           (to nil)
           (increment lyskom-fetch-map-nos)
           (plow (pers-stat->first-created-text persstat))
           (phigh (1- (+ plow (pers-stat->no-of-created-texts persstat))))
           (pmark (if (and num (< num 0)) plow phigh))
           (clow (conf-stat->first-local-no confstat))
           (chigh (1- (+ clow (conf-stat->no-of-texts confstat))))
           (cmark (if (and num (< num 0)) clow chigh))
           (smallest nil)
           (largest nil)
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

      ;;
      ;;  Extract results
      ;;

      (setq result-list
            (apply 'nconc (if (< num 0)
                              (nreverse result-list)
                            result-list)))

      (if (> num 0)
          (nthcdr (- (length result-list) num) result-list)
        (nfirst (- num)  result-list)))))


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

(defun lyskom-get-texts-by (persno num)
  "Get NUM texts written by PERSNO. Args: persno num"
  (let ((persstat (blocking-do 'get-pers-stat persno)))
    (lyskom-check-review-access t persstat)

    (let* ((plow (pers-stat->first-created-text persstat))
           (phigh (1- (+ plow (pers-stat->no-of-created-texts persstat))))
           (result nil)
           (increment (if num (abs num)))
           (mark (if (and num (< num 0)) plow phigh))
           (found nil)
           (data nil))
    

      (if (null num)
          (setq num (1+ phigh)
                mark phigh
                increment (1+ phigh)))

      (setq increment (min lyskom-fetch-map-nos increment))

      (while (and (<= mark phigh)
                  (>= mark plow)
                  (> (abs num) (length result)))
      
        (setq data (lyskom-remove-zeroes
                    (listify-vector
                     (map->text-nos
                      (blocking-do 'get-created-texts
                                   persno
                                   (if (< num 0)
                                       mark
                                     (- mark (1- increment)))
                                   increment)))))

        (mapcar (function (lambda (x) (initiate-get-text-stat 'main nil x)))
                data)
        (lyskom-wait-queue 'main)
        (setq found nil)
        (mapcar (function (lambda (x) (if (cache-get-text-stat x)
                                          (setq found (cons x found)))))
                data)
        (setq found (nreverse found))

        (if (> num 0)
            (setq result (nconc found result)
                  mark (- mark increment))
          (setq result (nconc result found)
                mark (+ mark increment)))
        (if (null found)
            (setq increment (min lyskom-fetch-map-nos (* increment 2)))
          (setq increment (- (abs num) (length result)))))

      (if (> num 0)
          (nthcdr (- (length result) num) result)
        (nfirst (- num)  result)))))



(defun lyskom-get-texts-to (confno num)
  "From CONFNO get NUM texts."
  (let ((confstat (blocking-do 'get-conf-stat confno)))

    (lyskom-check-review-access confstat t)

    (let* ((clow (conf-stat->first-local-no confstat))
           (chigh (1- (+ clow (conf-stat->no-of-texts confstat))))
           (result nil)
           (increment (and num (abs num)))
           (mark (if (and num (< num 0)) clow chigh)))



      (if (null num)
          (setq num (1+ chigh)
                increment (1+ chigh)
                mark chigh))

      (setq increment (min lyskom-fetch-map-nos increment))

      (while (and (<= mark chigh)
                  (>= mark clow)
                  (> (abs num) (length result)))
        (let ((found (lyskom-remove-zeroes
                      (listify-vector
                       (map->text-nos
                        (blocking-do 'get-map
                                     confno
                                     (if (< num 0)
                                         mark
                                       (- mark (1- increment)))
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

      (if (> num 0)
          (nthcdr (- (length result) num) result)
        (nfirst (- num)  result)))))


(defun kom-review-backward ()
  "Toggles the reviewing order.
If reading forward then starts reading backward and the other way round."
  (interactive)
  (lyskom-start-of-command 'kom-review-backward)
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
      (setcdr list (nreverse (cdr list)))
      (set-read-info->forward info (not forward))
      (lyskom-format-insert 'you-review 
			    (lyskom-get-string (if (not forward)
						   'forward
						 'backward)))))
   (t
    (lyskom-insert-string 'illegal-command)))
  (lyskom-end-of-command)) 


;;; ================================================================
;;;                   ]terse tr{det - review tree

;;; Author: Linus Tolke


(defun kom-review-tree (&optional text-no)
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
  (lyskom-start-of-command 'kom-review-tree)
  (unwind-protect
      (progn
	(lyskom-tell-internat 'kom-tell-review)
	(if text-no
	    (let ((ts (blocking-do 'get-text-stat text-no)))
	      (lyskom-follow-comments ts
				      nil 'review
				      (lyskom-get-current-priority)
				      t))
	  (lyskom-insert-string 'read-text-first)))
    (lyskom-end-of-command)))


(defun kom-find-root (&optional text-no)
  "Finds the root text of the tree containing the text in lyskom-current-text."
  (interactive)
  (lyskom-start-of-command 'kom-find-root)
  (unwind-protect
      (progn
	(lyskom-tell-internat 'kom-tell-review)
	(cond
	 (lyskom-current-text 
	  (let* ((ts (blocking-do 'get-text-stat (or text-no 
						     lyskom-current-text)))
		 (r (lyskom-find-root ts ts)))
	    (if r
		(lyskom-view-text r)
	      (signal 'lyskom-internal-error "Could not find root"))
	    )
	  )
	 (t
	  (lyskom-insert-string 'read-text-first))))
    (lyskom-end-of-command)))


(def-kom-command kom-find-root-review ()
  "Finds the root text of the tree containing the text in lyskom-current-text and
reviews the whole tree in deep-first order."
  (interactive)
  (lyskom-tell-internat 'kom-tell-review)
  (cond
   (lyskom-current-text
    (lyskom-review-tree
     (lyskom-find-root (blocking-do 'get-text-stat lyskom-current-text)
		       nil)))
   (t
    (lyskom-insert-string 'read-text-first))))


(defun lyskom-find-root (text-stat old-text-stat)
  "Finds the root text of the tree containing the text TEXT-STAT.
Args: TEXT-STAT OLD-TEXT-STAT THENDO
If TEXT-STAT is nil and OLD-TEXT-STAT contains a text-stat
then this means that the parent of the text in OLD-TEXT-STAT is not readable
and the text in OLD-TEXT-STAT is to be used instead.
If both TEXT-STAT and OLD-TEXT-STAT is nil then this means we are not allowed
to read the text we are trying to find the root of. This just returns with a
message."
  (let* ((ts text-stat)
	 (misclist (and ts (text-stat->misc-info-list ts)))
	 (res nil))
    (cond
     (ts				;+++ Smartare errorhantering hit.
      (setq res 'noparents)
      (while misclist
	(let* ((type (misc-info->type (car misclist)))
	       (comm (eq type 'COMM-TO))
	       (yes (or comm
			(eq type 'FOOTN-TO)))
	       (parent-no (if comm
			      (misc-info->comm-to (car misclist))
			    (misc-info->footn-to (car misclist)))))
	  (cond
	   (yes
	    (let* ((pts (blocking-do 'get-text-stat parent-no)))
	      (if (setq res (lyskom-find-root pts ts))
		  (setq misclist nil))))
	   (t
	    (setq misclist (cdr misclist))))))
      (if (eq res 'noparents) (text-stat->text-no ts) res))
     (old-text-stat
      (text-stat->text-no old-text-stat))
     (t
      nil))))


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


(defun kom-review-next ()
  "Resumes an interupted review by moving all review and review-tree entries in
the lyskom-reading-list to the beginning. i.e by moving all other types to the
end."
  (interactive)
  (lyskom-start-of-command 'kom-review-next)
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
      (setq len (1- len))))
  (lyskom-end-of-command))


;;; ================================================================
;;;                    ]terse stacken - Review stack

;;; Author: Linus


(defun kom-review-stack ()
  "Displays the review-stack."
  (interactive)
  (lyskom-start-of-command 'kom-review-stack)
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
   (read-list->all-entries lyskom-reading-list))
  (lyskom-end-of-command))


;;; ================================================================
;;;                      ]terse hoppa - review clear

;;; Author: Linus Tolke


(defun kom-review-clear ()
  "Deletes all review-types from the lyskom-reading-list and lyskom-to-do-list."
  (interactive)
  (lyskom-start-of-command 'kom-review-clear)
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
	(set-read-list-del-first lyskom-to-do-list)))
  (lyskom-end-of-command))


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


(defun kom-review-last-normally-read (no)
  "Reviews the NO last normally read texts."
  (interactive 
   (list 
    (lyskom-read-number (lyskom-get-string 'read-normally-read) 1)))
  (lyskom-start-of-command 'kom-review-last-normally-read)
  (lyskom-tell-internat 'kom-tell-review)
  (let* ((text-nos (reverse (nfirst no lyskom-normally-read-texts))))
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
      (lyskom-format-insert 'no-such-text)))
  (lyskom-end-of-command))
      
