
;;(defconst lyskom-category-properties		    
;;  '((command . lyskom-command)			    
;;    (menu    . lyskom-menu)			    
;;    (message . lyskom-message))			    
;;  "Property names for saving different categories.")

(defvar lyskom-language-symbols nil
  "Symbols with language data bound to them")

(defvar lyskom-language-categories nil
  "Categories used")

(defun lyskom-define-language (category language alist)
  "Associates names to symbols.

CATEGORY and LANGUAGE determines what kind of association to
create. ALIST is a mapping from symbols to strings."
  ;; Record category
  (or (memq category lyskom-language-categories)
      (setq lyskom-language-categories
	    (cons category lyskom-language-categories)))
  (mapcar (function (lambda (pair)
		      (let* ((symbol (car pair))
			     (string (cdr pair))
			     (llist (get symbol category))
			     (entry (assq language llist)))
			;; Record symbol
			(or (memq symbol lyskom-language-symbols)
			    (setq lyskom-language-symbols
				  (cons symbol lyskom-language-symbols)))
			(if entry
			    (setcdr entry string)
			  (put symbol category
			       (cons (cons language string) llist))))))
	  alist))

(put 'lyskom-define-language 'lisp-indent-function 2)

(defsubst lyskom-get-string-internal (symbol category)
    (cdr (assq lyskom-language (get symbol category))))

(defsubst lyskom-get-string-error (function symbol category)
  (signal 'lyskom-internal-error
	  (list function (list symbol category ": string not found"))))

(defun lyskom-get-string (symbol &optional category)
  "Returns string assiciated with SYMBOL"
    (or (lyskom-get-string-internal symbol (or category 'lyskom-message))
        (lyskom-get-string-error 'lyskom-get-string
                                 symbol
                                 (or category 'lyskom-message))))


(defun lyskom-get-strings (symbols &optional category)
  "Returns an alist of (symbol . string) pairs

according to CATEGORY and lyskom-language. Kind of inverse to
lyskom-define-language."
  (mapcar (function (lambda (symbol)
		      (cons symbol (lyskom-get-string symbol category))))
	  symbols))

(defun lyskom-get-menu-string (symbol)
  "Returns the name of a menu(item)

Looks for the 'lyskom-menu category, or 'lyskom-command
if 'lyskom-menu is not found."
    (or (lyskom-get-string-internal symbol 'lyskom-menu)
        (lyskom-get-string-internal symbol 'lyskom-command)
        (lyskom-get-string-error 'lyskom-get-menu-string symbol 'lyskom-menu)))

;;(defun nisse-filter (pred list)
;;  (cond ((null list) nil)
;;	  ((funcall pred (car list))
;;	   (cons (car list) (nisse-filter pred (cdr list))))
;;	  (t (nisse-filter pred (cdr list)))))
    
(defun lyskom-string-check-category (category)
  "Returns list of names for the cetegory, and their supported languages"
  (delq nil (mapcar (function
		     (lambda (symbol)
		       (let ((info (get symbol category)))
			 (if info (cons symbol (mapcar 'car info))))))
		    lyskom-language-symbols)))

			      
