;;;;;
;;;;; $Id: move-strings.el,v 41.0 1996-05-02 19:23:44 davidk Exp $
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
(defvar move-to-buffer "swedish-strings.el")


(defun query-move-string ()
  "Search for a string and if the answer is yes we move it to buffer.
Strings without argument only."
  (interactive)
  (create-match-list)
  (while (re-search-forward "\"\\([^\\]\\\\\\(\\\\\\\\\\)*\"\\|[^\"]\\)*\"")
    (let* ((char (read-char))
	   (matchb (match-beginning 0))
	   (matche (match-end 0))
	   (string (buffer-substring matchb
				     matche))
	   name)
      (recenter (/ (window-height) 2))
      (sit-for 0)
      (cond
       ((memq char '(?y ?  ?Y))
	(while (eq t (try-completion
		      (symbol-name 
		       (setq name 
			     (read-minibuffer "What to call this one? ")))
		      move-match-list))
	  (message "That is: %s" (cdr (assoc (symbol-name name)
					     move-match-list)))
	  (sit-for 2))
	(delete-region matchb matche)
	(insert (format "(lyskom-get-string '%s)" name))
	(save-excursion
	  (set-buffer move-to-buffer)
	  (indent-according-to-mode)
	  (insert (format "(%s . %s)\n" name string))))
       ((memq char '(?n ?N ? ?)))
       ((memq char '(?r ?R))
	(create-match-list))
       ((memq char '(?a ?A))
	(setq name (completing-read "What was its name? " 
				    move-match-list
				    nil t nil))
	(delete-region matchb matche)
	(insert (format "(lyskom-get-string '%s)" name)))
       ((memq char '(?c ?C))
	(while (eq t (try-completion
		      (symbol-name 
		       (setq name (read-minibuffer
				   "This complex one need a name? ")))
		      move-match-list))
	  (message "That is: %s" (cdr (assoc (symbol-name name)
					     move-match-list)))
	  (sit-for 2))
	(save-restriction
	  (let ((end (progn (up-list 1) (point)))
		(beg (progn (backward-sexp 1) (point)))
		(string "")
		ele (r 1)
		pos)
	    (down-list 1)
	    (narrow-to-region (1+ beg) (1- end))
	    (kill-sexp 1)
	    (insert (format "lyskom-format '%s" name))
	    (condition-case error
		(while t
		  (setq pos (point))
		  (cond
		   ((stringp (setq ele (read (current-buffer))))
		    (setq string (concat string ele))
		    (if (memq (char-after (1- (point))) '(?\n ?\r))
			(backward-char 1))
		    (delete-region pos (point)))
		   (t
		    (setq string (concat string (format "%%#%ds" r)))
		    (setq r (1+ r)))))
	      (end-of-file))
	    (save-excursion
	      (set-buffer move-to-buffer)
	      (indent-according-to-mode)
	      (insert (format "(%s . %s)\n" name (prin1-to-string string)))))))

       ((memq char '(?))
	(recursive-edit))
       (t
	(error "Exit.")))))
  (message "Done."))


(defvar move-match-list nil)

(defun create-match-list ()
  (save-excursion
    (set-buffer move-to-buffer)
    (eval-defun 1))
  (setq move-match-list
	(mapcar 
	 (function
	  (lambda (ele)
	    (cons (symbol-name (car ele))
		  (cdr ele))))
	 lyskom-strings)))
