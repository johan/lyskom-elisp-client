;;;;;
;;;;; $Id: info-nuke.el,v 44.0 1996-08-30 14:42:22 davidk Exp $
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
;;; Author: Linus Tolke linus@lysator.liu.se


(defun nuke-info (file)
  "Takes an info file. (Only works on an single file) and creates a ascii-file."
  (interactive "fName of info file:")
  (let* ((buf (find-file-noselect file))
	 (name (concat (buffer-file-name buf) ".nuked"))
	 (new (find-file-noselect name))
	 pos)
    (set-buffer new)
    (erase-buffer)
    (insert-buffer buf)
    (switch-to-buffer new)

    ; Most of the work
    (delete-matching-lines "^File:.*Node:")

    (goto-char (point-min))
    (while (search-forward "* Menu:" (point-max) 'limit)
      (delete-region (progn 
		       (beginning-of-line 1)
		       (point))
		     (1- (progn 
			   (search-forward "")
			   (point)))))

    ; remove everythin before the first 
    (goto-char (point-min))
    (search-forward "")
    (backward-char 1)
    (delete-region (point-min) (point))

    ; now remove the tags-table
    (if (search-forward "\nTag table:" (point-max) 'limit)
	(progn
	  (beginning-of-line 0)
	  (forward-char 1)
	  (delete-region (point) (point-max))))

    ; Now process every page the same way.
    (goto-char (point-min))
    (setq pos (point))
    (while (search-forward "" (point-max) 'limit)
      (delete-backward-char 1)
      (delete-blank-lines)
      ; short nodes are removed (indexes.)
      (if (< (count-lines pos (point)) 6)
	  (delete-region pos (point)))
      (setq pos (point))
      (newline)
      (newline))

    ; Now fix Notes.
    (goto-char (point-min))
    (while (re-search-forward "(\\*Note .*:" (point-max) 'limit)
      (replace-match "(Se {ven" t t))
    
    (message "You can save the file %s now." new)))
  

(defun batch-info-nuke ()
  "Usage: emacs -l info-nuke -f batch-info-nuke file"
  (if (not noninteractive)
      (error "batch-info-nuke may only be used -batch."))
  (let ((file))
    (while command-line-args-left
      (setq file (expand-file-name (car command-line-args-left)))
      (if (not (file-exists-p file))
	  (message ">> %s does not exist!" file)
	(nuke-info file)
	(write-file (concat file ".txt")))
      (setq command-line-args-left (cdr command-line-args-left)))))

