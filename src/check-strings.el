;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: check-strings.el,v 44.9 1999-06-23 12:25:25 byers Exp $
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

  (lcs-message t "Checking variables")
  (lcs-check-lyskom-commands (mapcar 'car
                                     (lcs-all-category-string
                                      'lyskom-command)))

  (lcs-message t "Checking languages: %s" (mapcar 'car lyskom-languages))
  (lcs-check-key-bindings (mapcar 'car (lcs-all-category-string
                                        'lyskom-command))
                          'lyskom-mode-map)
  (mapcar 'lcs-check-category lyskom-language-categories)

  (lcs-message t "Checking customizeable variables")
  (lcs-check-customize-variables)

  (or noninteractive
      (display-buffer lcs-message-buffer)))

(defun lcs-check-lyskom-commands (commands)
  "Check the lyskom-commands variable"
  (let ((tmp (copy-sequence lyskom-commands)))
    (mapcar (lambda (el)
              (if (memq el lyskom-commands)
                  (setq tmp (delq el tmp))
                (lcs-message nil "(%s) Missing in lyskom-commands" el)))
            commands)
    (mapcar (lambda (el)
              (lcs-message nil "(%s) Unknown command in lyskom-commands" el))
            tmp)))

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

(defun lcs-check-key-bindings (commands keymap &optional show-all-missing)
  "Check the commands in CATEGORY named NAME for bindings 
in lyskom-X-mode-map"
  (let* ((langs (mapcar 'car lyskom-languages))
         (keymaps (get keymap 'lyskom-language-keymap))
         (all-missing nil))
    (while commands
      (let ((lang-c (copy-sequence langs)))
        (mapcar (lambda (lang)
                  (when (where-is-internal (car commands)
                                           (symbol-value (cdr (assq lang keymaps))))
                    (setq lang-c (delq lang lang-c))))
                langs)
        (cond ((equal lang-c langs) (setq all-missing
                                          (cons (car commands) all-missing)))
              (lang-c 
               (lcs-message nil "(%s:%s) Missing binding in %s" keymap 
                            (car commands) lang-c)))
        (setq commands (cdr commands))))
    (when show-all-missing
      (mapcar (lambda (el)
                (lcs-message nil "(%s:%s) Missing in %s" keymap el langs))
              all-missing))))


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

(defun lcs-check-customize-variables ()
  "Check customize variables.
Check that all customizeable variables are server-stored.
Check that all server-stored variables are customizeable."
  (let ((cust-vars-in-buffer
         (delq nil (mapcar (lambda (el) (when (vectorp el) (elt el 0))) 
                           lyskom-customize-buffer-format)))
        (cust-vars-widgets (mapcar 'car lyskom-custom-variables))
        (cust-vars-all (append lyskom-elisp-variables 
                               lyskom-global-boolean-variables 
                               lyskom-global-non-boolean-variables)))
    (mapcar 
     (lambda (var)
       (cond ((not (memq var cust-vars-widgets))
              (lcs-message nil "(%s:%s) No widget definition for variable."
                           'lyskom-custom-variables var))
             ((not (memq var cust-vars-in-buffer))
              (lcs-message nil "(%s:%s) Variable not in customize buffer."
                           'lyskom-customize-buffer-format var))
             (t (unless (lyskom-get-string-internal 
                         (intern (format "%s-tag" var))
                         'lyskom-custom-strings)
                  (lcs-message nil "(%s:%s) No tag string"
                               'lyskom-custom-strings var))
                (unless (lyskom-get-string-internal 
                         (intern (format "%s-doc" var))
                         'lyskom-custom-strings)
                  (lcs-message nil "(%s:%s) No doc string"
                               'lyskom-custom-strings var))))
       (setq cust-vars-in-buffer (delq var cust-vars-in-buffer))
       (setq cust-vars-widgets (delq var cust-vars-widgets))
       nil)
     cust-vars-all)

    (mapcar (lambda (var)
              (lcs-message nil "(%s:%s) Not a server-stored variable."
                           'lyskom-custom-variables var))
            cust-vars-widgets)
    (mapcar (lambda (var)
              (lcs-message nil "(%s:%s) Not a server-stored variable."
                           'lyskom-custom-variables var))
            cust-vars-in-buffer)))
             
             
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
	
    
    
