;;;;;
;;;;; $Id: check-strings.el,v 44.1 1996-09-24 16:24:30 byers Exp $
;;;;; Copyright (C) 1996  Lysator Academic Computer Association.
;;;;;
;;;;
;;;;  This file is used for checking swedish-strings.el and
;;;;  english-strings.el. Run using
;;;;
;;;;  emacs -batch -l check-strings.el -f lyskom-check-strings
;;;;
;;;;

(defvar language-files '((english . "english-strings.el")
                         (swedish . "swedish-strings.el")))
(defvar language-values nil)
(defvar language-variables '(lyskom-strings lyskom-commands lyskom-keybindings))

(defun language-value (language sym)
  (cdr (assq sym (cdr (assq language language-values)))))


(defun missing-sym (sym)
  (intern (concat (symbol-name sym) "-missing")))

(defun symbol-value-safe (sym)
  (and (boundp sym)
       (symbol-value sym)))


(defun read-language (language file vars)
  (let ((result (list language)))
    (load-file file)
    (while vars
      (setq result (append result (list (cons (car vars) 
					      (symbol-value-safe
					       (car vars))))))
      (setq result (append result (list (cons (missing-sym (car vars))
					      (symbol-value-safe
					       (missing-sym (car vars)))))))
      (setq vars (cdr vars)))
    (setq language-values (cons result language-values))))


(defvar lyskom-format-format
  "%\\(=\\)?\\(-?[0-9]+\\)?\\(#\\([0-9]+\\)\\)?\\(:\\)?\\([][@MmPpnrtsdoxc]\\)"
  "regexp matching format string parts.")


(defun format-string (string)
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


(defun check-format-string (template string)
  (if (not (stringp string))
      nil
    (let* ((tmp (format-string string))
	   (result t))
      (setq template (copy-sequence template))
      (while tmp
	(if (not (member (car tmp) template))
	    (setq result nil tmp nil)
	  (setq template (delete (car tmp) template))
	  (setq tmp (delete (car tmp) tmp))))
      (and result (null template)))))
	    
      

(defun check-language-value (language sym)
  (message "  Checking %s %S" language sym)
  (let ((tmp (language-value language sym))
        (missing nil)
        (check nil))
    (while tmp
      (setq check (car tmp))
      (if (and (cdr check)
	       (stringp (cdr check)))
	  (setq format-list (format-string (cdr check)))
	(setq format-list nil))
      (setq tmp (cdr tmp))
      (setq missing nil)
      (mapcar '(lambda (lang)
                 (if (and (not (assq (car check)
				     (language-value (car lang) sym)))
			  (not (memq (car check)
				     (language-value 
				      (car lang)
				      (missing-sym sym)))))
                     (setq missing (cons lang missing))
		   (if (and (stringp (cdr check))
			    (assq (car check)
				  (language-value (car lang) sym)))
		       (if (not
			    (check-format-string 
			     format-list
			     (cdr (assq (car check)
					(language-value (car lang) sym)))))
			   (message "    Format mismatch for %S in %s and %s\n    %S\n    %S"
				    (car check)
				    language
				    (cdr lang)
				    format-list
				    (format-string 
				     (cdr (assq (car check)
						(language-value (car lang) sym))))
)))))
              language-files)
      (if missing
          (message "    Missing %S in %s"
                   (car check)
                   (mapconcat
                    '(lambda (x) (format "%s" (cdr x)))
                    missing
                    ", "))))))



(defvar lyskom-keybindings nil)
(defvar keymaps nil)

(defun suppress-keymap (&rest args) nil)

(defmacro define-key (map key cmd)
  (` (progn (setq keymaps (cons (quote (, map)) keymaps))
            (x-define-key (, cmd)))))

(defun define-prefix-command (&rest args) nil)

(defun x-define-key (cmd)
  (if (not (assq cmd lyskom-keybindings))
      (setq lyskom-keybindings (cons (cons cmd nil) lyskom-keybindings))))

(defun reset-keybindings ()
  (setq lyskom-keybindings nil)
  (mapcar (function (lambda (x) (set x nil))) keymaps)
  (setq keymaps nil))




;;; ============================================================
;;; Here are the functions that do the deed
;;;

(defun run ()
  (setq language-values nil)
  (load-file "./vars.el.in")
  (mapcar 'load-language language-files)
  (mapcar 'check-language language-files))
   

(defun load-language (file-spec)
  (message "Loading %S from %s" (car file-spec) (cdr file-spec))
  (read-language (car file-spec) (cdr file-spec)
		 language-variables)
  (reset-keybindings))

(defun check-language (file-spec)
  (message "Checking %S in %s" (car file-spec) (cdr file-spec))
  (mapcar (function
           (lambda (x) (check-language-value (car file-spec)
                                             x)))
	  language-variables))
