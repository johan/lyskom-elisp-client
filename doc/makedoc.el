(defvar lyskom-doc-vardefs nil)

(defvar lyskom-doc-keymaps
  '(lyskom-mode-map lyskom-edit-mode-map))

(fset 'tmpfn-1 (symbol-function 'defvar))

(defun lyskom-doc-defvar (var val doc)
  (setq lyskom-doc-vardefs (cons (list var val
                                       (if (and doc
                                                (string-match "^*" doc))
                                           t
                                         nil))
                                 lyskom-doc-vardefs))
  (set var val))


(defmacro lyskom-doc-defvar-macro (var &optional val doc &rest junk)
  (list 'lyskom-doc-defvar (list 'quote var) val doc))


(defun lyskom-update-documentation ()
  (unwind-protect
      (progn
        (fset 'def-kom-var 'lyskom-doc-defvar-macro)
        (load-file "../src/lyskom.el"))
    (fset 'def-kom-var (symbol-function 'tmpfn-1)))
  (find-file "lyskom-commands.data")
  (lyskom-check-cmd-doc)
  (find-file "lyskom-variables.data")
  (lyskom-check-vbl-doc))


(defun lyskom-check-vbl-doc ()
  (let ((doc-list nil)
        (new-list nil))
    (setq doc-list (lyskom-check-doc-find-keys 0 'intern))
    (mapcar (function
             (lambda (x)
               (if (not (memq (car x) doc-list))
                   (setq new-list (cons (car x) new-list)))))
            lyskom-doc-vardefs)
    (if new-list
        (progn
          (mapcar 'lyskom-make-var-doc-outline new-list)
          (save-buffer))
      (message "No new variables to document"))
  ))

(defun lyskom-make-var-doc-outline (var)
  (if (elt (assq var lyskom-doc-vardefs) 2)
      (progn
        (message "New variable: %s" var)
        (insert
         (concat
          (symbol-name var)
          "\t"
          ;; Allowed values
          "\t"
           (replace-in-string 
            (format "%S" (elt (assq var lyskom-doc-vardefs) 1))
            "\n" "")
          "\t"
          (if (or (memq var lyskom-elisp-variables)
                  (memq var lyskom-global-boolean-variables))
              "t"
            "nil")
          "\t"
          (if (elt (assq var lyskom-doc-vardefs) 2)
              "t"
            "nil")
          "\t"
          ;; Xref
          "\t"
          ;; Doc
          "\n")))))
    

(defun lyskom-check-cmd-doc ()
  (let ((doc-list nil)
        (new-list nil))
    (setq doc-list (lyskom-check-doc-find-keys 0 'intern))
    (mapcar (function
             (lambda (x)
               (if (not (memq x doc-list))
                   (setq new-list (cons x new-list)))))
            lyskom-commands)

    (goto-char (point-min))
;    (message "Documented functions: %S" doc-list)
    (if new-list
        (progn
          (mapcar 'lyskom-make-fn-doc-outline new-list)
          (save-buffer))
      (message "No new commands to document"))
    ))

(defun lyskom-make-fn-doc-outline (fn)
  (let ((cmdname (lyskom-command-name fn)))
    (if cmdname
        (progn
          (message "New command: %S (%s)" fn cmdname)
          (insert
           (concat
            (symbol-name fn)
            "\t"
            cmdname
            "\t"
            (lyskom-make-fn-keybindings fn)
            "\t"
            ;; Variable xref
            "\t"
            ;; Command xref
            "\t"
            ;; Documentation
            "\n")))
      (message "Command %S has no name\n"))))

(defun lyskom-make-fn-keybindings (fn)
  (mapconcat (function
              (lambda (x)
                (replace-in-string 
                 (replace-in-string
                  (replace-in-string
                   (replace-in-string
                    (replace-in-string
                     (replace-in-string
                      (key-description x)
                      "}" "å")
                     "{" "ä")
                    "|" "ö")
                   "]" "å")
                  "[" "ä")
                 "\\" "ö")))
             (apply 'append
                    (mapcar 
                     (function
                      (lambda (keymap)
                        (where-is-internal fn (symbol-value keymap))))
                     lyskom-doc-keymaps))
             ", "))

(defun lyskom-check-doc-find-keys (fieldno modfn)
  (let ((numlines (count-lines (point-min) (point-max)))
        (lineno 0)
        (string nil)
        (result nil)
        (regexp (concat "^"
         (let ((tmp "\\([^\t]*\\)"))
           (while (> fieldno 0)
             (setq tmp (concat "[^\t]*\t" tmp))
             (setq fieldno (1- fieldno)))
           tmp))))
             
    (goto-char (point-min))
    (while (< lineno numlines)
      (setq lineno (1+ lineno))
      (goto-line lineno)
      (setq string (buffer-substring (lyskom-bol) (lyskom-eol)))
      (string-match regexp string)
      (setq result (cons 
                    (funcall modfn (match-string 1 string)) result)))
    (nreverse result)))


  

(defun lyskom-bol ()
  (save-excursion (beginning-of-line) (point)))

(defun lyskom-eol ()
  (save-excursion (end-of-line) (point)))


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
