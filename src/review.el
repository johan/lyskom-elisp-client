;;;;;
;;;;; $Id: review.el,v 40.4 1996-04-25 15:03:20 davidk Exp $
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
	      "$Id: review.el,v 40.4 1996-04-25 15:03:20 davidk Exp $\n"))



(defun lyskom-intersection (a b)
  "Returns as a list the intersection of list A and list B.
The order of the list a is kept."
  (let ((list nil))
    (while a
      (if (memq (car a) b)
	  (setq list (cons (car a) list)))
      (setq a (cdr a)))
    (nreverse list)))

(defun lyskom-remove-zeroes (a)
  "Returns a copy of list where all zeroes are removed."
  (delq 0 (copy-sequence a)))


;;; ================================================================
;;;              ]terse av, till - Review by X to Conference Y.

;;; Author: Linus Tolke, David Kågedal, David Byers



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
  (interactive)
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
                             (lyskom-get-string 'everybody))
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
              ;; If person is not given we must give
              ;; conf
              (not (zerop by))
              (if (zerop lyskom-current-conf)
                  ""
                (conf-stat->name
                 (blocking-do 'get-conf-stat
                              lyskom-current-conf)))
              t)))

    ;; Since we fetch everything anyway we don't need to do this.  If
    ;; we later choose to fetch all in small chunks we will have to do
    ;; this then.
    (if (not (zerop to))
        (cache-del-conf-stat to))
    (if (not (zerop by)) 
        (cache-del-pers-stat by))
    (let* ((info-by (if (zerop by) 
                        (lyskom-get-string 'anybody)
                      (blocking-do 'get-conf-stat by)))
           (info-to (if (zerop to)
                        (lyskom-get-string 'all-confs)
                      (blocking-do 'get-conf-stat to))))
      (lyskom-format-insert 'review-info-by-to
                            info
                            info-by
                            info-to))

    (let ((list (lyskom-get-texts-by-to by to count)))
      (if list
          (read-list-enter-read-info (lyskom-create-read-info
                                      'REVIEW
                                      nil
                                      (lyskom-get-current-priority)
                                      (lyskom-create-text-list list)
                                      nil t)
                                     lyskom-reading-list t)
      	(lyskom-insert-string 'no-such-text)))))


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
                (zerop to)) nil)
          ((zerop to) (lyskom-get-texts-by by num))
          ((zerop by) (lyskom-get-texts-to to num))
          (t (lyskom-get-texts-by-and-to by to num))))



;;; ================================================================
;;; lyskom-get-texts-by-and-to
;;; Author: David K}gedal

(defmacro lyskom-bat-advance-by-list ()
  (` (if (cdr by-list)
	 (setq by-list (cdr by-list))
       (setq by-list (nreverse
		      (lyskom-remove-zeroes
		       (listify-vector
			(map->text-nos
			 (blocking-do 'get-created-texts
				      (pers-stat->pers-no persstat)
				      (if (< num 0)
					  pmark
					(- pmark (1- increment)))
				      increment))))))
       (if (> num 0)
	   (setq pmark (- pmark increment))
	 (setq pmark (+ pmark increment))))))

(defmacro lyskom-bat-advance-to-list ()
  (` (if (cdr to-list)
	 (setq to-list (cdr to-list))
       (setq to-list (nreverse
		      (lyskom-remove-zeroes
		       (listify-vector
			(map->text-nos
			 (blocking-do 'get-map
				      (conf-stat->conf-no confstat)
				      (if (< num 0)
					  cmark
					(- cmark (1- increment)))
				      increment))))))
       (if (> num 0)
	   (setq cmark (- cmark increment))
	 (setq cmark (+ cmark increment))))))


(defun lyskom-get-texts-by-and-to (persno confno num)
  "Get NUM texts written by person PERSNO with conference CONFNO as a
recipient. 
Args: persno confno num"
  (let* ((persstat (blocking-do 'get-pers-stat persno))
         (confstat (blocking-do 'get-conf-stat confno))
         (result-list nil)
         (by-list nil)
         (to-list nil)
         (result-size 0)
         (by nil)
         (to nil)
         (increment 30)
         (plow (pers-stat->first-created-text persstat))
         (phigh (1- (+ plow (pers-stat->no-of-created-texts persstat))))
         (pmark (if (< num 0) plow phigh))
         (clow (conf-stat->first-local-no confstat))
         (chigh (1- (+ clow (conf-stat->no-of-texts confstat))))
         (cmark (if (< num 0) clow chigh)))
    ;; Initialize by-list and to-list
    (lyskom-bat-advance-to-list)
    (lyskom-bat-advance-by-list)

    ;; The real work below
    (while (and (< result-size num)
		by-list
		to-list)
      (cond (;; We have found a text in both lists. Then we add it to
	     ;; result-list and move on.
	     (= (car by-list) (car to-list))
	     (setq result-list (cons (car by-list) result-list))
	     (lyskom-bat-advance-to-list)
	     (lyskom-bat-advance-by-list)
	     (++ result-size))

	    ;; We know that the first text on to-list can't be on
	    ;; by-list. So we skip it and move on.
	    ((or (and (< num 0) (> (car by-list) (car to-list)))
		 (and (> num 0) (< (car by-list) (car to-list))))
	     (lyskom-bat-advance-to-list))

	    ;; We know that the first text on by-list can't be on
	    ;; to-list. So we skip it and move on.
	    (t
	     (lyskom-bat-advance-by-list))))
    
    ;; If we were searching from lower numbers, the resulting list
    ;; will be reversed.
    (if (< num 0)
	(setq result-list (nreverse result-list)))

    result-list))
      


;;; ===============================================================
;;; lyskom-get-texts-generic
;;; Author: David Byers
;;;
;;; This function gets NUM texts from the start or end of a map.
;;; It works by fetching INCREMENT texts at a time, removing zeroes
;;; and appending to what it already has. INCREMENT starts out as the
;;; number of texts still to go, but is incremented each time no
;;; new texts are returned (when only zeroes are returned)
;;;

(defun lyskom-get-texts-generic (objnum num low high get-operation)
  "From object OBJNUM (a person or conference number) get NUM texts.
  LOW is the lowest local text number and HIGH the highest in the
  conference or person map. GET-OPERATION is the blocking-do operation
  to use to get texts (get-map or get-created-texts)."
  (let* ((result nil)
         (increment (abs num))
         (mark (if (< num 0) low high)))

    (while (and (<= mark high)
                (>= mark low)
                (> (abs num) (length result)))
      (let ((found (lyskom-remove-zeroes
                    (listify-vector
                     (map->text-nos
                      (blocking-do get-operation
                                   objnum
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
            (setq increment (min 150 (* increment 2)))
          (setq increment (- (abs num) (length result))))))

    (if (> num 0)
        (nthcdr (- (length result) num) result)
      (nfirst (- num)  result))))


(defun lyskom-get-texts-by (persno num)
  "Get NUM texts written by PERSNO. Args: persno num"
  (let* ((persstat (blocking-do 'get-pers-stat persno))
         (plow (pers-stat->first-created-text persstat))
         (phigh (1- (+ plow (pers-stat->no-of-created-texts persstat)))))
    (lyskom-get-texts-generic persno num plow phigh 'get-created-texts)))


(defun lyskom-get-texts-to (confno num)
  "From CONFNO get NUM texts."
  (let* ((confstat (blocking-do 'get-conf-stat confno))
         (clow (conf-stat->first-local-no confstat))
         (chigh (1- (+ clow (conf-stat->no-of-texts confstat)))))
    (lyskom-get-texts-generic confno num clow chigh 'get-map)))







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
  (let* ((text-stat (blocking-do 'get-text-stat lyskom-current-text))
	 (misc-info-list (and text-stat
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
      
