;;;;;
;;;;; $Id: mktype.el,v 38.0 1994-01-06 01:49:00 linus Exp $
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
(defun mktype (first last type-name)
  "Make a new type. The region should consist of the names of the
sub-types that makes up the type. Asks for the name of the new type.
Args: FIRST LAST TYPE-NAME."
  (interactive "r\nsName of new type? ")
  (let ((tags (buffer-substring first last)))
    (kill-region first last)
    (insert (format ";;; %s\n\n" type-name))
    (mkconstructor tags type-name)
    (mkselector tags type-name)
    (mkassigner tags type-name)
    (insert "\n")
    (mkp type-name)))

(defun mkconstructor (tags type)
  "Make a constructor for the type TYPE.
Args: TAGS TYPE."
  (insert ";;; Constructor:\n\n")
  (insert (format "(defun lyskom-create-%s (" type))
  (let ((args tags))
    (while (> (length args) 0)
      (string-match "[ \n\t]*\\([^ \n\t]+\\)[ \n\t]*" args)
      (let ((word (substring args (match-beginning 1) (match-end 1))))
	(insert (format "%s\n" word))
	(setq args (substring args (match-end 0)))
	(lisp-indent-line))))
  (forward-line -1)
  (end-of-line)
  (insert (format ")
  \"Create a %s from all parameters.\"
  (cons
   '%s" type type))
  (mkupcase)
  (insert (format "
   (vector "))
  (let ((args tags))
    (while (> (length args) 0)
      (string-match "[ \n\t]*\\([^ \n\t]+\\)[ \n\t]*" args)
      (let ((word (substring args (match-beginning 1) (match-end 1))))
	(insert (format "%s " word))
	(setq args (substring args (match-end 0)))
	(if (> (current-column) 60)
	    (progn
	      (insert (format "\n"))
	      (lisp-indent-line))))))
  (insert ")))\n\n"))
  

(defun mkupcase ()
  "Upcase typename. The name MUST start with an '."
  (let ((pos (point)))			
    (search-backward "'")
    (upcase-region (point) pos)
    (goto-char pos)))
  

(defun mkselector (tags type-name)
  "Make selector routines. TAGS is the names of the sub-types
 that makes up the type. TYPE-NAME is the name of the new type.
Args: TAGS TYPE-NAME."
  (insert "\n;;; Selectors:\n\n")
  (let ((number 0))
    (while (> (length tags) 0)
      (string-match "[ \n\t]*\\([^ \n\t]+\\)[ \n\t]*" tags)
      (let ((word (substring tags (match-beginning 1) (match-end 1))))
	(insert (format
		 "(defun %s->%s (%s)
  \"Get %s from %s.\"
  (elt (cdr %s) %d))\n\n"
		 type-name word type-name
		 word type-name
		 type-name number)))
      (setq tags (substring tags (match-end 0)))
      (setq number (1+ number)))))


(defun mkp (name)
  (insert "\n;;; Predicate:\n\n")
  (insert (format "(defun lyskom-%s-p (object)
  \"Return t if OBJECT is a %s.\"
  (eq (car-safe object) '%s))\n\n" name name name))
  (mkupcase))


(defun mkassigner (tags type-name)
  "Make assigner routines. TAGS is the names of the sub-types
 that makes up the type. TYPE-NAME is the name of the new type.
Args: TAGS TYPE-NAME."
  (insert "\n;;; Modifiers:\n\n")
  (let ((number 0))
    (while (> (length tags) 0)
      (string-match "[ \n\t]*\\([^ \n\t]+\\)[ \n\t]*" tags)
      (let ((word (substring tags (match-beginning 1) (match-end 1))))
	(insert (format
		 "(defun set-%s->%s (%s newval)
  \"Set %s in %s to NEWVAL.\"
  (aset (cdr %s) %d newval))\n\n"
		 type-name word type-name
		 word type-name
		 type-name number)))
      (setq tags (substring tags (match-end 0)))
      (setq number (1+ number)))))
