;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: compatibility.el,v 44.69 2004-06-26 13:32:32 byers Exp $
;;;;; Copyright (C) 1991-2002  Lysator Academic Computer Association.
;;;;; Copyright (C) 2001 Free Software Foundation, Inc.
;;;;;
;;;;; This file is part of the LysKOM Emacs LISP client.
;;;;; 
;;;;; LysKOM is free software; you can redistribute it and/or modify it
;;;;; under the terms of the GNU General Public License as published by 
;;;;; the Free Software Foundation; either version 2, or (at your option) 
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
;;;; ================================================================
;;;; ================================================================
;;;;
;;;; File: compatibility.el
;;;;
;;;; This file contains functions that may not exist in all supported
;;;; versions of Gnu Emacs. XEmacs-specific and Emacs 18-specific code
;;;; should go in some other file.
;;;;

(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: compatibility.el,v 44.69 2004-06-26 13:32:32 byers Exp $\n"))


;;; ============================================================
;;; Utility macros

(eval-and-compile
  (defvar lyskom-compatibility-definitions nil
    "Functions defined or redefined because they are incompatible with LysKOM")


(defmacro lyskom-provide-function (name &rest rest)
  "If NAME is not already defined, define it as a function."
  `(eval-and-compile
     (or (fboundp ',name)
         (progn (setq lyskom-compatibility-definitions
                      (cons ',name lyskom-compatibility-definitions))
                (defun ,name ,@rest)))))

(defmacro lyskom-provide-macro (name &rest rest)
  "If NAME is not already defined, define it as a macro."
  `(eval-and-compile
     (or (fboundp ',name)
         (progn (setq lyskom-compatibility-definitions
                      (cons ',name lyskom-compatibility-definitions))
                (defmacro ,name ,@rest)))))

(defmacro lyskom-macro-alias (name args &rest body)
  "Create a LysKOM name alias. If NAME is defined, then
lyskom-NAME will be an alias to it. Otherwise, lyskom-NAME
will be defined to be a macro with ARGS and BODY."
  (let ((alias-name (intern (concat "lyskom-" (symbol-name name)))))
    `(eval-and-compile
       (if (fboundp ',name)
           (defalias ',alias-name ',name)
         (setq lyskom-compatibility-definitions
               (cons ',name lyskom-compatibility-definitions))
         (defmacro ,alias-name ,args ,@body)))))

(defmacro lyskom-function-alias (name args &rest body)
  "Create a LysKOM name alias. If NAME is defined, then
lyskom-NAME will be an alias to it. Otherwise, lyskom-NAME
will be defined to be a function with ARGS and BODY."
  (let ((alias-name (intern (concat "lyskom-" (symbol-name name)))))
    `(eval-and-compile
       (if (fboundp ',name)
           (defalias ',alias-name ',name)
         (setq lyskom-compatibility-definitions
               (cons ',name lyskom-compatibility-definitions))
         (defun ,alias-name ,args ,@body)))))

(defmacro lyskom-xemacs-or-gnu (xemacs-form gnu-form)
  "Eval XEMACS-FORM in XEmacs and GNU-FORM in Gnu Emacs."
  (if (string-match "XEmacs" (emacs-version))
      xemacs-form
    gnu-form))
)



;;; ======================================================================
;;; Various definitions


;; Lots of Emacsen have buggy definitions of kbd (or no definition at
;; all) Although it's crufty to redefine a function from subr.el, I
;; will do so if it appears to be misbehaving. Don't like it? Tough!

(eval-and-compile
  (and (condition-case nil
           (or (equal (kbd (identity "<down-mouse-2>"))
                      [down-mouse-2])
               (error "Bad definition of kbd"))
         (error t))
       (progn (fmakunbound 'kbd)
              (lyskom-provide-macro kbd (keys)
                "Convert KEYS to the internal Emacs key representation.
KEYS should be a string in the format used for saving keyboard macros
\(see `insert-kbd-macro')."
                (if (or (stringp keys)
                        (vectorp keys))
                    (read-kbd-macro keys)
                  `(read-kbd-macro ,keys))))))


;; Functions mostly from XEmacs that we use

(lyskom-macro-alias compiled-function-p (arg) `(byte-code-function-p ,arg))
(lyskom-function-alias characterp (obj) (integerp obj))
(lyskom-function-alias int-to-char (obj) obj)
(lyskom-function-alias char-to-int (c) c)
(lyskom-function-alias signum (num) (cond ((< num 0) -1)
                                          ((> num 0) 1)
                                          (t 0)))

;; If we use lyskom-function-alias here we'll get an obsolecense
;; warning about screen-width. This way the compiler won't notice :-)

(if (fboundp 'frame-width)
    (defalias 'lyskom-frame-width 'frame-width)
  (fset 'lyskom-frame-width 'screen-width))

;; Some minibuffer compatibility stuff

(lyskom-with-external-functions (temp-minibuffer-message)
  (lyskom-function-alias minibuffer-contents () (buffer-string))
  (lyskom-function-alias minibuffer-message (message) 
    (temp-minibuffer-message message)))

;; Definition of map-keymap that hopefully works like the one in XEmacs
;; except that the sort-first argument is ignored.

(lyskom-function-alias map-keymap (fn keymap &optional sort-first)
  "Map FN over all bindings in KEYMAP."
  (let ((r 0))
    (when (keymapp keymap)
      (when (symbolp keymap) (setq keymap (symbol-value keymap)))
      (let ((keymap (cdr keymap))
            (el nil))
        (while (and keymap (not (keymapp keymap)))
          (setq el (car keymap) keymap (cdr keymap))
          (cond ((vectorp el)
                 (while (< r (length el))
                   (if (aref el r)
                       (funcall fn r (aref el r)))
                   (setq r (1+ r))))
                ((char-table-p el)
                 (map-char-table fn el))
                (t (funcall fn (car el) (cdr el)))))))))


;; set-keymap-parent also comes from XEmacs

(lyskom-function-alias set-keymap-parent (keymap new-parent)
  (let ((tail keymap))
    (while (and tail (cdr tail) (not (eq (car (cdr tail)) 'keymap)))
      (setq tail (cdr tail)))
    (if tail
        (setcdr tail new-parent))))


;; XEmacs and Gnu Emacs don't use the same names for mouse events
;; and such. This is to help us deal with that.

(defconst lyskom-gnu-keysym
  '((button1   . "<down-mouse-1>")
    (button2   . "<down-mouse-2>")
    (button3   . "<down-mouse-3>")
    (button1up . "<mouse-1>")
    (button2up . "<mouse-2>")
    (button3up . "<mouse-3>")))

(defconst lyskom-xemacs-keysym
  '((button1   . "<button1>")
    (button2   . "<button2>")
    (button3   . "<button3>")
    (button1up . "<button1up>")
    (button2up . "<button2up>")
    (button3up . "<button3up>")))


(defun lyskom-keys (sym)
  "Look up the key description for key SYM."
  (cdr (assq sym (lyskom-xemacs-or-gnu lyskom-xemacs-keysym
                                       lyskom-gnu-keysym))))



;;; ============================================================
;;; MULE stuff
;;;
;;; We used to define dummy versions of basic MULE functions so
;;; we could code using the standard names. We don't do that any
;;; more. Instead, we define aliases in our own namespace.

(defvar enable-multibyte-characters nil)
(lyskom-function-alias set-buffer-multibyte (arg)
  (put 'enable-multibyte-characters 'permanent-local t)
  (make-local-variable 'enable-multibyte-characters)
  (setq enable-multibyte-characters arg))

(lyskom-function-alias set-process-coding-system (proc &optional encoding decoding)) 
(lyskom-function-alias encode-coding-string (str coding-system) (copy-sequence str))
(lyskom-function-alias decode-coding-string (str coding-system) (copy-sequence str))
(lyskom-function-alias string-bytes (str) (length str))
(lyskom-function-alias check-coding-system (name) (error "No such coding system"))
(lyskom-function-alias find-coding-systems-for-charsets (cs) nil)
(lyskom-function-alias coding-system-get (cs prop) nil)
(lyskom-function-alias char-width (c) 1)
(lyskom-function-alias find-charset-string (str) '(ascii))
(lyskom-function-alias string-as-unibyte (str) str)
(lyskom-function-alias string-make-unibyte (str) str)
(lyskom-function-alias string-make-multibyte (str) str)
(lyskom-function-alias multibyte-string-p (str) nil)
(lyskom-function-alias charsetp (arg) nil)
(lyskom-function-alias coding-system-type (arg) nil)
(lyskom-function-alias coding-system-property (arg sym) (lyskom-coding-system-get arg sym))
(lyskom-function-alias split-string (string &optional separators)
  (string-split (or separators "[ \f\t\n\r\v]+") string))

;; Detect buggy versions of encode-coding-string and
;; decode-coding-string such as those provided by APEL (part of TM and
;; often included in XEmacs)

(defun lyskom-buggy-encode-coding-string (str coding-system) str)
(eval-and-compile
  (if (let ((test "TEM")) (eq (lyskom-encode-coding-string test 'raw-text) test))
      (progn (fset 'lyskom-buggy-encode-coding-string
                   (symbol-function 'lyskom-encode-coding-string))
             (defun lyskom-encode-coding-string (str coding-system)
               (copy-sequence (lyskom-buggy-encode-coding-string str coding-system))))))

(defun lyskom-buggy-decode-coding-string (str coding-system) str)
(eval-and-compile
  (if (let ((test "TEM")) (eq (lyskom-decode-coding-string test 'raw-text) test))
      (progn (fset 'lyskom-buggy-decode-coding-string
                   (symbol-function 'lyskom-decode-coding-string))
             (defun lyskom-decode-coding-string (str coding-system)
               (copy-sequence (lyskom-buggy-decode-coding-string str coding-system))))))


;; defmacro lyskom-encode-coding-char in XEmacs so the compiled code
;; is quicker. In Gnu Emacs define it as a function.
;;
;; The definition is made at compile-time to avoid getting warnings 
;; about encode-coding-char.

(eval-and-compile
  (cond ((eval-when-compile (string-match "XEmacs" (emacs-version)))
         (defmacro lyskom-encode-coding-char (c system) c))
        ((eval-when-compile (fboundp 'encode-coding-char))
         (defun lyskom-encode-coding-char (c system)
           (let ((s (encode-coding-char c system)))
             (if (and s (= (length s) 1))
                 (elt s 0)))))
        (t (defmacro lyskom-encode-coding-char (c system) c))))


;; It seems that string-width is buggy with respect to handling
;; unibyte strings in multibyte environments or vice versa. Or
;; something like that. There has got to be a good explanation
;; for the mess below.

(eval-and-compile
  (if (fboundp 'string-width)
      (defalias 'lyskom-original-string-width 'string-width)
    (defmacro lyskom-original-string-width (s) `(length ,s)))
  (lyskom-xemacs-or-gnu
   (defalias 'lyskom-string-width 'string-width)
   (defun lyskom-string-width (str)
     (cond ((and (lyskom-multibyte-string-p str)
                 (null enable-multibyte-characters))
            (lyskom-original-string-width (lyskom-string-make-unibyte str)))
           ((and (null (lyskom-multibyte-string-p str))
                 enable-multibyte-characters)
            (lyskom-original-string-width (lyskom-string-make-multibyte str)))
           (t (lyskom-original-string-width str))))))


;;; ================================================================
;;; Faces and windows and stuff

(lyskom-function-alias frame-property (frame property &optional default)
  (or (cdr (assq property (frame-parameters frame))) default))
(lyskom-function-alias face-background (face) nil)
(lyskom-function-alias face-foreground (face) nil)
(lyskom-function-alias find-face (face) (and (facep face) face))

(defun lyskom-make-face (name temporary)
  "Like make-face in XEmacs"
  (lyskom-xemacs-or-gnu (make-face name nil temporary)
                        (make-face name)))

(if (not (lyskom-find-face 'strikethrough))
    (progn (make-face 'strikethrough)
           (if (eval-when-compile (fboundp 'set-face-strikethrough-p))
               (set-face-strikethru-p 'strikethrough t)
             (set-face-underline-p 'strikethrough t))))


(defun lyskom-get-buffer-window-list (buffer &optional minibuf frame)
  "Return windows currently displaying BUFFER, or nil if none.
See `walk-windows' for the meaning of MINIBUF and FRAME."
  (let ((buffer (if (bufferp buffer) buffer (get-buffer buffer))) windows)
    (walk-windows (function (lambda (window)
			      (if (eq (window-buffer window) buffer)
				  (setq windows (cons window windows)))))
		  minibuf frame)
    windows))


;;; ================================================================
;;; Color stuff

(lyskom-with-external-functions (color-rgb-components
                                 make-specifier
                                 set-specifier)
  (eval-and-compile
    (cond ((and (fboundp 'color-rgb-components)
                (fboundp 'make-specifier)
                (fboundp 'set-specifier))
           (defun lyskom-color-values (color)
             (when (stringp color)
               (let ((spec nil))
                 (set-specifier (setq spec (make-specifier 'color)) color)
                 (setq color spec)))
             (color-rgb-components color)))
          ((fboundp 'color-values) (fset 'lyskom-color-values 'color-values))
          ((fboundp 'x-color-values) (fset 'lyskom-color-values 'x-color-values)))))



;;; ======================================================================
;;; Event stuff

(lyskom-with-external-functions (event-start)
  (lyskom-function-alias event-glyph (e))
  (lyskom-function-alias event-point (e)
    "Return the character position of the given mouse event.
If the event did not occur over a window, or did not occur over text,
then this returns nil.  Otherwise, it returns an index into the buffer
visible in the event's window."
    (car (cdr (event-start e))))

  (lyskom-function-alias event-closest-point (e)
    "Return the character position closest to the mouse event EVENT."
    (car (cdr (event-start e)))))


;;; ======================================================================
;;; Platform-specific stuff

(lyskom-provide-function w32-shell-execute (&rest args)
  "Dummy function that raises an error."
  (error "w32-shell-execute undefined"))



;;; ================================================================
;;; We provide these functions in the default namespace since there is
;;; no reason to beleive that (a) the definitions suck (they're taken
;;; from either XEmacs or Gnu Emacs) and (b) no package should be
;;; using them to figure out what environment it is running in
;;; (unlike, say, the MULE functions).
;;;
;;; There are also a few functions (regexpp and functionp) that may
;;; appear in some version of Emacs, and if they do we accept those
;;; definitions.
;;;

(lyskom-provide-function last (x &optional n)
  "Returns the last link in the list LIST.
With optional argument N, returns the Nth-to-last link (default 1)."
  "Returns the last link in the list LIST.
With optional argument N, returns Nth-to-last link (default 1)."
  (if n
      (let ((m 0) (p x))
	(while (consp p) (setq m (1+ m)) (setq p (cdr p)))
	(if (<= n 0) p
	  (if (< n m) (nthcdr (- m n) x) x)))
    (while (consp (cdr x)) (setq x (cdr x)))
    x))

(lyskom-provide-function plist-member (plist prop)
  "Return non-nil if PLIST has the property PROP.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2 ...\). PROP is a symbol.
Unlike `plist-get', this allows you to distinguish between a missing
property and a property with the value nil."
      (while (and plist (not (eq (car plist) prop)))
        (setq plist (cdr (cdr plist))))
      (and plist t))

;; The make-temp-file function below is taken verbatim from Emacs 21.2.
(lyskom-provide-function make-temp-file (prefix &optional dir-flag)
  "Create a temporary file.  The returned file name (created by
appending some random characters at the end of PREFIX, and expanding
against `temporary-file-directory' if necessary, is guaranteed to
point to a newly created empty file.  You can then use `write-region'
to write new data into the file.

If DIR-FLAG is non-nil, create a new empty directory instead of a file."
  (let (file)
    (while (condition-case ()
	       (progn
		 (setq file
		       (make-temp-name
			(expand-file-name prefix 
                                          (lyskom-xemacs-or-gnu
                                           (temp-directory)
                                           temporary-file-directory))))
		 (if dir-flag
		     (make-directory file)
		   (write-region "" nil file nil 'silent nil))
		 nil)
             (file-already-exists t))
      ;; the file was somehow created by someone else between
      ;; `make-temp-name' and `write-region', let's try again.
      nil)
    file))

(lyskom-provide-function replace-in-string
    (str regexp newtext &optional literal)
  "Replaces all matches in STR for REGEXP with NEWTEXT string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat \\ in NEWTEXT string as special:
  \\& means substitute original matched text,
  \\N means substitute match for \(...\) number N,
  \\\\ means insert one \\."
  (if (not (stringp str))
      (error "(replace-in-string): First argument must be a string: %s" str))
  (if (stringp newtext)
      nil
    (error "(replace-in-string): 3rd arg must be a string: %s"
	   newtext))
  (let ((rtn-str "")
	(start 0)
	(special)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat
	      rtn-str
	      (substring str prev-start match)
	      (cond (literal newtext)
		    (t (mapconcat
			 (function
			   (lambda (c)
			     (if special
				 (progn
				   (setq special nil)
				   (cond ((eq c ?\\) "\\")
					 ((eq c ?&)
					  (substring str
						     (match-beginning 0)
						     (match-end 0)))
					 ((and (>= c ?0) (<= c ?9))
					  (if (> c (+ ?0 (length
							   (match-data))))
					      ;; Invalid match num
					      (error "(replace-in-string) Invalid match num: %c" c)
					    (setq c (- c ?0))
					    (substring str
						       (match-beginning c)
						       (match-end c))))
					 (t (char-to-string c))))
			       (if (eq c ?\\) (progn (setq special t) nil)
				 (char-to-string c)))))
			 newtext ""))))))
    (concat rtn-str (substring str start))))


(lyskom-provide-function buffer-live-p (object)
  "T of OBJECT is an editor buffer that has not been deleted."
  (and (bufferp object)
       (buffer-name object)))

(lyskom-provide-function string-to-vector (string)
  "Return a vector of characters in STRING."
  (let ((len (length string)) (i 0) val)
    (setq val (make-vector len 0))
    (while (< i len)
      (aset val i (aref string i))
      (setq i (1+ i)))
    val))

(lyskom-provide-function copy-tree (l)
  "Recursively copy the list L"
  (cond ((atom l) l)
        (t (cons (copy-tree (car l))
                 (copy-tree (cdr l))))))

(lyskom-provide-function functionp (obj)
  "Returns t if OBJ is a function, nil otherwise."
  (cond ((symbolp obj) (fboundp obj))
        ((subrp obj))
        ((lyskom-compiled-function-p obj))
        ((consp obj) (and (eq (car obj) 'lambda) (listp (car (cdr obj)))))
        (t nil)))

(lyskom-provide-function regexpp (re)
  "Return non-nil if RE looks like a valid regexp."
  (let ((result t))
    (save-match-data
      (condition-case nil
          (string-match re "")
        (error (setq result nil))))
    result))

(lyskom-macro-alias char-before (&optional pos buffer)
  `(save-excursion
     (save-restriction 
       (widen)
       ,@(if buffer `((set-buffer ,buffer)))
       ,(if pos 
	    `(if (or (> ,pos (point-max))
		     (<= ,pos (point-min)))
		 nil
	       (goto-char ,pos)
	       (preceding-char))
	   `(if (<= (point) (point-min))
		nil
	      (preceding-char))))))



;;; ================================================================
;;; Some symbols are self-evaluating. They're not in all versions
;;; of Emacs that we would like to support. So we make them
;;; self-evaluating here.

;; This code looks the way it does in order to avoid warnings in
;; Emacs 21.

(eval-and-compile
  (defmacro lyskom-make-self-evaluating (var)
    `(condition-case nil
         (setq lyskom-dummy-variable-to-fool-the-byte-compiler
               (symbol-value ',var))
       (error (set ',var ',var))))

  (lyskom-make-self-evaluating :default)
  (lyskom-make-self-evaluating :mime-charset)
  (lyskom-make-self-evaluating :default-help-echo)
  (lyskom-make-self-evaluating :group)
  (lyskom-make-self-evaluating :automatic)
  (lyskom-make-self-evaluating :read-only)
  (lyskom-make-self-evaluating :filter)
  (lyskom-make-self-evaluating :filter-args)
  (lyskom-make-self-evaluating :constraint)
  (lyskom-make-self-evaluating :save)
  (lyskom-make-self-evaluating :refer)
  (lyskom-make-self-evaluating :width)
  (lyskom-make-self-evaluating :prompt)
  (lyskom-make-self-evaluating :align)
  (lyskom-make-self-evaluating :format)
  (lyskom-make-self-evaluating :output)
  (lyskom-make-self-evaluating :subject-mode)
  (lyskom-make-self-evaluating :subject-indent)
  (lyskom-make-self-evaluating :text)
  (lyskom-make-self-evaluating :text-stat)
  (lyskom-make-self-evaluating :text-no)
  (lyskom-make-self-evaluating :subjects)
  (lyskom-make-self-evaluating :subject-last)
  (lyskom-make-self-evaluating :comment-order)
  (lyskom-make-self-evaluating :unique)
  (lyskom-make-self-evaluating :weight)
  (lyskom-make-self-evaluating :prompt-format)
  (lyskom-make-self-evaluating :dead-ok)
  (lyskom-make-self-evaluating :mark)
  (lyskom-make-self-evaluating :may-interrupt)
)


;;; ================================================================
;;; ================================================================
;;; ================================================================


;;; Set up edebug-form-spec and friends. We do this here because
;;; we have to have the eval block at the end, and this at least
;;; keeps everything together (it would have been nicer to put
;;; an eval block with each definition, but I don't think that
;;; would look good, and it might not always work).

(put 'lyskom-provide-function 'lisp-indent-hook 2)
(put 'lyskom-function-alias 'lisp-indent-hook 2)
(put 'lyskom-macro-alias 'lisp-indent-hook 2)
(put 'lyskom-xemacs-or-gnu 'edebug-form-spec '(form form))

;;; Local Variables:
;;; eval: (put 'lyskom-provide-function 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-function-alias 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-macro-alias 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-xemacs-or-gnu 'edebug-form-spec '(form form))
;;; end:
