;;;;;
;;;;; $Id: check-strings.el,v 44.5 1997-07-15 10:22:54 byers Exp $
;;;;; Copyright (C) 1996  Lysator Academic Computer Association.
;;;;;
;;;;
;;;;  This file is used for checking swedish-strings.el and
;;;;  english-strings.el. Run using
;;;;
;;;;  emacs -batch -l check-strings.el [-l <lyskomfiles>.el] -f lyskom-check-strings
;;;;
;;;;  or by M-x lyskom-check-strings
;;;;
;;;;

(require 'lyskom)


(defvar lcs-dont-check-ending-categories '(lyskom-command)
  "String categories where ending mismatches are OK.")

(defvar lcs-message-buffer "*LysKOM string check*")

(defun lyskom-check-strings ()
  "Check the strings in LysKOM for sanity."
  (interactive)
  (or noninteractive
      (lcs-setup-message-buffer))

  (lcs-message t "Languages: %s" (mapcar 'car lyskom-languages))
  (mapcar 'lcs-check-category lyskom-language-categories)

  (or noninteractive
      (display-buffer lcs-message-buffer)))

(defun lcs-check-category (category)
  "Check the strings in CATEGORY."
  (lcs-message t "Checking category %s" category)
  (let ((strings (lcs-all-category-string category)))
    (while strings
      (lcs-check-strings category (car (car strings))
			 (cdr (car strings)))
      (setq strings (cdr strings)))))
  
(defun lcs-all-category-string (category)
  "Returns list of names for CATEGORY, and their strings."
  (mapcar (function
	   (lambda (symbol)
	     (let ((info (get symbol category)))
	       (if info (cons symbol info)))))
	  (get category 'lyskom-language-symbols)))

(defun lcs-check-strings (category name strings)
  "Check the strings in CATEGORY named NAME.
STRINGS is a list of (language . string)."
;;  (lcs-message t "Checking %s:%s" category name)
  (let ((format-list 'uninitialized)
	(first-str nil)
	(langs (mapcar 'car lyskom-languages)))
    (while strings
      (let* ((lang (car (car strings)))
	     (str (cdr (car strings)))
	     (flist (lcs-check-string category name lang str)))
	(if (listp format-list)
	    (progn 
              (or (lcs-check-format-string format-list flist)
                  (lcs-message nil "(%s:%s) Format mismatch\n    %S\n    %S"
                               category name first-str str))
              (and 
               (not (memq category lcs-dont-check-ending-categories))
               (or (lcs-check-string-ending first-str str)
                   (lcs-message nil "(%s:%s) Ending mismatch\n    %S\n    %S"
                               category name first-str str))))
	  (setq format-list flist
		first-str str))

	(setq strings (cdr strings)
	      langs (delq lang langs))))

    (if langs
	(lcs-message nil "(%s:%s) Missing languages %s"
		     category name langs))))

(defun lcs-check-string (category name lang string)
  "Check the string in CATEGORY named NAME in language LANG.
STRING is the string."
  (if (and string (stringp string))
      (lcs-split-format-string string)
    nil))

(defun lcs-split-format-string (string)
  "Extract the formatters from STRING."
  (let ((result nil)
	(start 0))
    (while (and (< start (length string))
		(string-match lyskom-format-format
			      string
			      start)
		(setq start (match-end 0))
		(setq result (cons 
			      (concat
			       (match-string 1 string)
			       (match-string 3 string)
			       (match-string 5 string)
			       (match-string 6 string))
			      result))))
    result))


(defconst lcs-match-endings 
  '("\\." "\\?" ":" "!" "\\.\n" "\\?\n" ":\n" "!\n" "\n" ")" ")\n"
    "\\? +" ": +" "\\? +\n" ": +\n")
  "String endings that should be identical in various languages.")

(defun lcs-check-string-ending (template str)
  (cond ((or (not (stringp template))
             (not (stringp str))) t)
        (t (let ((result
                  (mapcar
                   (function
                    (lambda (x)
                      (let ((pat (concat "\\`\\(.\\|\n\\)*" x "\\'")))
                        (eq (string-match pat template)
                            (string-match pat str)))))
                   lcs-match-endings)))
             (cond ((memq nil result) nil)
                   (t t))))))
    

(defun lcs-check-format-string (template flist)
  "Match the formatters in TEMPLATE to those in FLIST."
  (let* ((result t))
    (setq template (copy-sequence template))
    (while flist
      (if (not (member (car flist) template))
	  (setq result nil flist nil)
	(setq template (delete (car flist) template))
	(setq flist (delete (car flist) flist))))
    (and result (null template))))

(defun lcs-setup-message-buffer ()
  "Inititalize the message buffer for string checking."
  (save-excursion
    (set-buffer (get-buffer-create lcs-message-buffer))
    (erase-buffer)))

(defun lcs-message (echo format &rest args)
  "Display a message during string checking.
If ECHO is non-nil display the message in the echo area, otherwise
only append it to the log buffer.
FORMAT and ARGS are as for `format'."
  (let ((msg (apply 'format format args)))
    (if noninteractive
	(princ (concat msg "\n") t)
      (save-excursion
	(set-buffer lcs-message-buffer)
	(goto-char (point-max))
	(insert msg "\n"))
      (if echo
	  (message msg)))))
	
    
    
