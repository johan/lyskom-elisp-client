(defconst lyskom-doc-filename "elisp-client")

(defvar lyskom-fn-doc nil)
(defvar lyskom-var-doc nil)

(defun lyskom-doc-patch ()
  (setq lyskom-fn-doc (lyskom-doc-load-db "lyskom-commands.data"))
  (setq lyskom-var-doc (lyskom-doc-load-db "lyskom-variables.data"))
  (find-file (concat lyskom-doc-filename ".in")
  (lyskom-doc-patch-fn)
  (lyskom-doc-patch-var)
  (write-file (concat lyskom-doc-filename ".texinfo"))
)


(defun lyskom-doc-patch-fn ()
  (lyskom-doc-patch-any "^%@c\\s-*\\(\\S-*\\)\\s-*$"
                        lyskom-fn-doc
                        'lyskom-format-fn))

(defun lyskom-doc-patch-var ()
  (lyskom-doc-patch-any "^%@v\\s-*\\(\\S-*\\)\\s-*$"
                        lyskom-var-doc
                        'lyskom-format-var))

(defun lyskom-doc-patch-any (regexp doclist format-fn)
  (let ((pos 0)
        (doc nil))
    (goto-char (point-min))
    (while (setq pos (re-search-forward regexp nil t))
      (goto-char pos)
      (setq doc (assoc (match-string 1) doclist))
      (if (null doc)
          (progn
            (message "Unknown key %S near line %d"
                     (match-string 0)
                     (count-lines (point-min) pos))
            (sit-for 2))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (apply format-fn doc))))))

(defun lyskom-doc-symbol (string alist)
  (condition-case arg
      (or (cdr (assq (car-safe (read-from-string string))
                     alist))
          "")
      (error "")))


(defun lyskom-format-var (var-name 
                          value-range
                          default-value
                          is-server-variable
                          is-user-variable
                          xref
                          doc-string)
  (format "\n@item @code{%s}\n@vindex %s\n@itemx @code{%s}%s\n\n%s\n"
          var-name
          var-name
          default-value
          (lyskom-doc-symbol is-server-variable '((t . " (sparas i servern)")
                                                  (nil . "")))
          (replace-in-string doc-string "" "\n")))


(defun lyskom-doc-transform-key-list (keybindings)
  (if (string-match "^\\s-*$" keybindings)
      ""
    (let ((kb-result "")
          (ki-result "")
          (start 0))

      (while (string-match "\\s-*\\([^,]+\\)\\(,\\|$\\)" keybindings 
                           start)
        (setq kb-result
              (concat kb-result
                      (format "@kbd{%s}%s"
                              (match-string 1 keybindings)
                              (if (eq (match-end 0) (length keybindings))
                                  "\n" ", ")))
              ki-result
              (concat ki-result
                      (format "@kindex %s\n"
                              (match-string 1 keybindings))))
        (setq start (match-end 0)))
      (concat kb-result ki-result))))

(defun lyskom-format-fn (symbol
                         name
                         keybindings
                         variable-refs
                         command-refs
                         doc-string)
  (format "%s@item @code{%s}\n@findex{%s}\n%s\n"
          (lyskom-doc-transform-key-list keybindings)
          name
          name
          (replace-in-string doc-string "" "\n")
  ))
    



(defun lyskom-bol ()
  (save-excursion (beginning-of-line) (point)))

(defun lyskom-eol ()
  (save-excursion (end-of-line) (point)))

(defun lyskom-doc-load-db (file)
  (find-file file)
  (goto-char (point-min))
  (let ((result nil)
        (lineno 0)
        (string nil)
        (tmp nil)
        (start 0)
        (maxline (count-lines (point-min) (point-max))))
    (while (< lineno maxline)
      (setq lineno (1+ lineno))
      (goto-line lineno)
      (setq string (buffer-substring (lyskom-bol) (lyskom-eol)))
      (setq start 0 
            tmp nil)
      (while (and 
              (< start (length string))
              (eq start (string-match "\\(^\\|\t\\)\\([^\t]*\\)\\(\t\\|$\\)" 
                                          string 
                                          start)))
        (setq start (match-end 2))
        (setq tmp (cons (match-string 2 string) tmp)))
      (setq result (cons (nreverse tmp) result)))
    result))


(defun replace-in-string (string from to)
  (let ((i 0))
    (if (stringp from)
	(setq from (aref from 0)))
    (if (stringp to)
	(setq to (aref to 0)))
    (while (< i (length string))
      (if (= (aref string i) from)
	  (aset string i to))
      (setq i (1+ i)))
    string))

