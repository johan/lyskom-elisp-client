
(defconst lyskom-category-properties
  '((command . lyskom-command)
    (menu    . lyskom-menu)
    (message . lyskom-message))
  "Property names for saving different categories.")

(defun lyskom-define-language (category language alist)
  "Associates names to symbols.

CATEGORY and LANGUAGE determines what kind of association to
create. ALIST is a mapping from symbols to strings."
  (let ((property (cdr (assq category lyskom-category-properties))))
    (mapcar (function (lambda (pair)
			(let* ((symbol (car pair))
			       (string (cdr pair))
			       (llist (get symbol property))
			       (entry (assq language llist)))
			  (if entry (setcdr entry string)
			    (put symbol property
				 (cons (cons language string) llist))))))
	    alist)))

(put 'lyskom-define-language 'lisp-indent-function 2)

(defun lyskom-get-string (symbol &optional category)
  "Returns string assiciated with SYMBOL"
  (if (not category) (setq category 'message))
  (let ((prop (if (eq category 'menu)
		  (or (get symbol 'lyskom-menu)
		      (get symbol 'lyskom-command))
		(get symbol (cdr (assq category lyskom-category-properties))))))
    (or (cdr (assq lyskom-language prop))
	(signal 'lyskom-internal-error 
		(list 'lyskom-get-string
		      (list symbol category ": string not found"))))))

(defun lyskom-get-strings (symbols &optional category)
  "Returns an alist of (symbol . string) pairs

according to CATEGORY and lyskom-language. Sort of inverse to
lyskom-define-language."
  (mapcar (function (lambda (symbol)
		      (cons symbol (lyskom-get-string symbol category))))
	  symbols))

