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

