(require 'iso-syntax)

(defconst subst-tabell
  ; Ja, detta {r konstigt men jag anv{nder bokst{verna till att f} r{tt
  ; ordning. Det konverteras tillbaka sedan. Var lugna.
  '(("ä" "^^e5")
    ("Ä" "^^c5")
    ("Å" "^^c4")
    ("å" "^^e4")
    ("Ö" "^^d6")
    ("ö" "^^f6")
    ))
    
     
(defun find-sort-key (string)
  "Convert a string containing strange dvi-output to iso-chars."
  (setq string (concat " " string))
  (let ((change t)
	(completion-ignore-case nil))
    (while change
      (setq change nil)
      (let ((s subst-tabell))
	(while s
	  (if (string-match (regexp-quote (car (cdr (car s)))) string)
	      (setq string (concat (substring string 0 (match-beginning 0))
				   (car (car s))
				   (substring string (match-end 0)))
		    change t))
	  (setq s (cdr s))))

      (if (string-match "(.*)" string)
	  (setq string (concat (substring string 0 (match-beginning 0))
			       (substring string (match-end 0)))
		change t))
      (if (string-match "\\s-\\s-" string)
	  (setq string (concat (substring string 0 (match-beginning 0))
			       " "
			       (substring string (match-end 0)))
		change t))
      ))
  (substring string 1))


(defun texindex (filename)
  "Sort the texindex file."
  (interactive "fFilnamn: ")
  ;; all-info {r en associationslista med all information.
  ;; Ide, l{s in, sortera skriv ut. Enklare kan det v{l inte bli.
  (save-excursion
    (let ((all-info nil)
	  (case-fold-search nil))
      ;; Read in the info
      (set-buffer (find-file-noselect filename))
      (goto-char (point-min))
      (while (not (eobp))
	(if (looking-at (concat (regexp-quote "\\entry {")
				"\\(.*\\)"
				(regexp-quote "}{")
 				"\\([0-9]+\\)"
				(regexp-quote "}{")
				"\\(.*\\)"
				(regexp-quote "}\n")))
	    (let ((pagenum (buffer-substring (match-beginning 2)
					     (match-end 2)))
		  (textinfo (buffer-substring (match-beginning 3)
					      (match-end 3)))
		  ;; Hmm, denna m}ste vara sist h{r eftersom find-sort-key
		  ;; fular sig med match-data.
		  (sortkey (find-sort-key (buffer-substring 
					   (match-beginning 1)
					   (match-end 1)))))
	      (forward-line 1)
	      (setq all-info (cons (list sortkey pagenum textinfo)
				   all-info)))
	  (error "unparseable")))
      
      ;; sort the info
      (setq all-info (sort (nreverse all-info)
			   (function
			    (lambda (ele1 ele2)
			      (let ((key1 (car ele1))
				    (key2 (car ele2)))
				(string< (downcase key1) (downcase key2)))))))
      

      ;; Generate the output file.
      (let ((current-letter "initial-first-letter")
	    (current-index "")
	    (current-page ""))
	(set-buffer (find-file-noselect
		     (concat filename "s")))
	(erase-buffer)
	(while all-info
	  (if (string= current-index (upcase (car (car all-info))))
	      ; Same index point.
	      (if (string= current-pate (car (cdr (car all-info))))
		  nil ; Same page skip it.
		(goto-char last-marker)
		(insert ",")
		(insert (car (cdr (car all-info))))
		(setq last-marker (point))
		(goto-char (point-max)))
	    (if (string= current-letter
			 (substring (upcase (car (car all-info))) 0 1))
		nil
	      (setq current-letter 
		    (substring (upcase (car (car all-info))) 0 1))
	      (insert "\\initial {" 
		      (if (assoc current-letter subst-tabell)
			  (car (cdr (assoc current-letter subst-tabell)))
			current-letter)
		      "}\n"))
	    (insert "\\entry {"
		    (car (cdr (cdr (car all-info))))
		    "}{"
		    (car (cdr (car all-info))))
	    (setq last-marker (point))
	    (insert "}\n"))

	  (setq current-index (upcase (car (car all-info))))
	  (setq current-pate (car (cdr (car all-info))))
	  (setq all-info (cdr all-info)))

	))
    (save-buffer)))


(defun batch-texindex ()
  "Run texindex on the files remaining on the command line."
  (if (not noninteractive)
      (error "`batch-texindex' is to be used only with -batch"))
  (let ((error 0))
    (while command-line-args-left
      (condition-case error
	  (texindex (expand-file-name (car command-line-args-left)))
	(error (setq error 1)))
      (setq command-line-args-left (cdr command-line-args-left)))
    (kill-emacs error)))


