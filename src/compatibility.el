;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: compatibility.el,v 44.48 2002-02-24 20:23:26 joel Exp $
;;;;; Copyright (C) 1991-2002  Lysator Academic Computer Association.
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
	      "$Id: compatibility.el,v 44.48 2002-02-24 20:23:26 joel Exp $\n"))


;;; ======================================================================
;;; Use lyskom-provide to supply a definition that is only to be used
;;; if no definition already exists. The definition will be evaluated at
;;; both compile and run time.
;;;
;;; lyskom-provide-macros behaves like defmacro
;;; lyskom-provide-function behaves like defun
;;; lyskom-provide-subst behaves like defsubst
;;;

(eval-and-compile
  (defvar lyskom-compatibility-definitions nil
    "Functions defined or redefined because they are incompatible with
LysKOM"))

;;; ============================================================
;;; lyskom-compatibility-forms
;;; lyskom-compatibility-definition
;;;


(defmacro lyskom-compatibility-forms (predicate &rest forms)
  "If PREDICATE is nil, evaluate FORMS at compile and run time"
  (` (eval-and-compile
       (if (not (, predicate))
           (progn (,@ forms))))))

(defmacro lyskom-compatibility-definition (predicate definition)
  "If PREDICATE is nil, evaluate DEFINITION at compile and run time.
Definition should be a function definition of some kind, with syntax 
similar to defun or defmacro.

To simply define a function if it is not already defined, used one
of the lyskom-provide-* functions instead."
  (` (progn ;(eval-when-compile
            ;  (if (not (, predicate))
            ;      (message "Compatibility %S for %S"
            ;               (quote (, (car definition)))
            ;               (quote (, (car (cdr definition)))))))
              (eval-and-compile
                (if (not (, predicate))
                    (progn
                      (, definition)
                      (setq lyskom-compatibility-definitions
                            (cons (quote (, (car (cdr definition))))
                                  lyskom-compatibility-definitions))))))))


;;; ============================================================
;;; lyskom-provide
;;; lyskom-provide-macro
;;; lyskom-provide-function
;;; lyskom-provide-subst
;;;
;;; Define functions if they are not already defined
;;;

(defmacro lyskom-provide (definer name rest)
  `(eval-and-compile
     (if (not (fboundp ',name))
         (progn (setq lyskom-compatibility-definitions
                      (cons ',name lyskom-compatibility-definitions))
                (,definer ,name ,@rest)))))


(defmacro lyskom-provide-macro (name &rest rest)
  "If NAME is not already defined, define it as a macro."
  (` (lyskom-provide defmacro (, name) (, rest))))

(defmacro lyskom-provide-function (name &rest rest)
  "If NAME is not already defined, define it as a function."
  (` (lyskom-provide defun (, name) (, rest))))

(defmacro lyskom-provide-subst (name &rest rest)
  "If NAME is not already defined, define it as a defsubst."
  (` (lyskom-provide defsubst (, name) (, rest))))


;;; ============================================================
;;; lyskom-xemacs-or-gnu
;;;

(eval-and-compile
  (defmacro lyskom-xemacs-or-gnu (xemacs-form gnu-form)
    "Eval XEMACS-FORM in XEmacs and GNU-FORM in Gnu Emacs."
    (if (string-match "XEmacs" (emacs-version))
        xemacs-form
      gnu-form)))

(put 'lyskom-xemacs-or-gnu 'edebug-form-spec '(form form))



;;; ======================================================================
;;; Defining keys
;;;
;;; Lots of Emacsen have buggy definitions of kbd (or no definition at all)
;;; Although it's crufty to redefine a function from subr.el, I will do so
;;; if it appears to be misbehaving. Don't like it? Tough!
;;;

(lyskom-compatibility-definition

    (condition-case nil
        (or (equal (kbd (identity "<down-mouse-2>"))
                   [down-mouse-2])
            (error "Bad definition of kbd"))
      (error nil))

    (defmacro kbd (keys)
      "Convert KEYS to the internal Emacs key representation.
KEYS should be a string in the format used for saving keyboard macros
\(see `insert-kbd-macro')."
      (if (or (stringp keys)
              (vectorp keys))
          (read-kbd-macro keys)
        `(read-kbd-macro ,keys))))


;;; ======================================================================
;;; ======================================================================
;;; ======================================================================

;;;


(lyskom-provide-macro char-before (&optional pos buffer)
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
		       

(lyskom-provide-function characterp (obj)
  (integerp obj))

(lyskom-provide-function int-to-char (obj)
  obj)

(lyskom-compatibility-forms (fboundp 'frame-width)
    (fset 'frame-width 'screen-width))

(lyskom-provide-function signum (num)
  (cond ((< num 0) -1)
        ((> num 0) 1)
        (t 0)))


;;; ======================================================================
;;; Definition of map-keymap that hopefully works like the one in XEmacs
;;; except that the sort-first argument is ignored.
;;;

(lyskom-provide-function map-keymap (fn keymap &optional sort-first)
  (let ((r 0))
    (cond ((vectorp keymap)
           (while (< r (length keymap))
             (if (aref keymap r)
                 (funcall fn r (aref keymap r)))
             (setq r (1+ r))))
          (t (mapcar (function 
                      (lambda (x)
                        (funcall fn (car x) (cdr x))))
                     (cdr keymap))))))


(lyskom-provide-function set-keymap-parent (keymap new-parent)
   (let ((tail keymap))
     (while (and tail (cdr tail) (not (eq (car (cdr tail)) 'keymap)))
       (setq tail (cdr tail)))
     (if tail
         (setcdr tail new-parent))))

(defconst lyskom-gnu-keysym
  '((button1   . "<down-mouse-1>")
    (button2   . "<down-mouse-2>")
    (button3   . "<down-mouse-3>")
    (button1up . "<mouse-1>")
    (button2up . "<mouse-2>")
    (button3up . "<mouse-3>")
    (å	       . [229])
    (Å	       . [197])
    (C-å       . [(control 229)])
    (C-Å       . [(control 197)])
    (ä	       . [228])
    (Ä	       . [196])
    (ö	       . [246])
    (Ö	       . [214])
))

(defconst lyskom-xemacs-keysym
  '((button1   . "<button1>")
    (button2   . "<button2>")
    (button3   . "<button3>")
    (button1up . "<button1up>")
    (button2up . "<button2up>")
    (button3up . "<button3up>")
    (C-å       . [(control aring)])
    (C-Å       . [(control Aring)])
    (å	       . [aring])
    (Å	       . [Aring])
    (ä	       . [adiaeresis])
    (Ä	       . [Adiaeresis])
    (ö	       . [odiaeresis])
    (Ö	       . [Odiaeresis])
))


(defun lyskom-keys (sym)
  "Look up the key description for key SYM."
  (cdr (assq sym (lyskom-xemacs-or-gnu lyskom-xemacs-keysym
                                       lyskom-gnu-keysym))))


;;; ============================================================
;;; Text property and extents stuff
;;;

(lyskom-provide-function map-extents (&rest args))

(lyskom-provide-function next-text-property-bounds 
    (count pos prop &optional object)
  "Return the COUNTth bounded property region of property PROP after POS.
If COUNT is less than zero, search backwards.  This returns a cons
\(START . END) of the COUNTth maximal region of text that begins after POS
\(starts before POS) and has a non-nil value for PROP.  If there aren't
that many regions, nil is returned.  OBJECT specifies the buffer or
string to search in."
  (or object (setq object (current-buffer)))
  (let ((begin (if (stringp object) 0 (point-min)))
	(end (if (stringp object) (length object) (point-max))))
    (catch 'hit-end
      (if (> count 0)
	  (progn
	    (while (> count 0)
	      (if (>= pos end)
		  (throw 'hit-end nil)
		(and (get-char-property pos prop object)
		     (setq pos (next-single-property-change pos prop
							    object end)))
		(setq pos (next-single-property-change pos prop object end)))
	      (setq count (1- count)))
	    (and (< pos end)
		 (cons pos (next-single-property-change pos prop object end))))
	(while (< count 0)
	  (if (<= pos begin)
	      (throw 'hit-end nil)
	    (and (get-char-property (1- pos) prop object)
		 (setq pos (previous-single-property-change pos prop
							    object begin)))
	    (setq pos (previous-single-property-change pos prop object
						       begin)))
	  (setq count (1+ count)))
	(and (> pos begin)
	     (cons (previous-single-property-change pos prop object begin)
		   pos))))))


;;; ============================================================
;;; Basic stuff

(lyskom-provide-function char-to-int (c) c)

(defvar enable-multibyte-characters nil)
(lyskom-provide-function set-buffer-multibyte (arg)
  (put 'enable-multibyte-characters 'permanent-local t)
  (make-local-variable 'enable-multibyte-characters)
  (setq enable-multibyte-characters arg))

(lyskom-provide-function set-process-coding-system (proc &optional encoding decoding)
  ) 

(lyskom-provide-function encode-coding-string (str coding-system) (copy-sequence str))
(lyskom-provide-function decode-coding-string (str coding-system) (copy-sequence str))
(lyskom-provide-function string-bytes (str) (length str))
(lyskom-provide-function check-coding-system (name) (error "No such coding system"))
(lyskom-provide-function string-width (str) (length str))
(lyskom-provide-function char-width (c) 1)
(lyskom-provide-function find-charset-string (str) '(ascii))
(lyskom-provide-function string-as-unibyte (str) str)
(lyskom-provide-function string-make-unibyte (str) str)
(lyskom-provide-function string-make-multibyte (str) str)
(lyskom-provide-function multibyte-string-p (str) nil)


;;; Detect buggy versions of encode-coding-string and decode-coding-string
;;; such as those provided by APEL (part of TM and often included in XEmacs)

(defun lyskom-buggy-encode-coding-string (str coding-system) str)
(eval-and-compile
  (if (let ((test "TEM")) (eq (encode-coding-string test 'raw-text) test))
      (progn (fset 'lyskom-buggy-encode-coding-string
                   (symbol-function 'encode-coding-string))
             (defun encode-coding-string (str coding-system)
               (copy-sequence (lyskom-buggy-encode-coding-string str coding-system))))))

(defun lyskom-buggy-decode-coding-string (str coding-system) str)
(eval-and-compile
  (if (let ((test "TEM")) (eq (decode-coding-string test 'raw-text) test))
      (progn (fset 'lyskom-buggy-decode-coding-string
                   (symbol-function 'decode-coding-string))
             (defun decode-coding-string (str coding-system)
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


(eval-and-compile
  (lyskom-xemacs-or-gnu
   (fset 'lyskom-string-width (symbol-function 'string-width))
   (defun lyskom-string-width (str)
     (cond ((and (multibyte-string-p str)
                 (null enable-multibyte-characters))
            (string-width (string-make-unibyte str)))
           ((and (null (multibyte-string-p str))
                 enable-multibyte-characters)
            (string-width (string-make-multibyte str)))
           (t (string-width str))))))


(eval-and-compile
  (cond ((eval-when-compile (string-match "XEmacs" (emacs-version)))
         (defun lyskom-completing-read (prompt
                                        table 
                                        &optional predicate require-match
                                        init hist def inherit-input-method)
           (completing-read prompt table predicate require-match init hist)))
        ((eval-when-compile (> emacs-major-version 19))
         (fset 'lyskom-completing-read (symbol-function 'completing-read)))
        (t 
         (defun lyskom-completing-read (prompt
                                        table 
                                        &optional predicate require-match
                                        init hist def inherit-input-method)
           (completing-read prompt table predicate require-match init hist)))))

(eval-and-compile
  (cond ((eval-when-compile (string-match "XEmacs" (emacs-version)))
	 (defun lyskom-read-from-minibuffer (prompt 
					     &optional initial-contents
					     keymap read hist default-value
					     inherit-input-method)
	   (read-from-minibuffer prompt
				 initial-contents
				 keymap
				 read
				 hist)))
	((eval-when-compile (> emacs-major-version 19))
	 (fset 'lyskom-read-from-minibuffer 
	       (symbol-function 'read-from-minibuffer)))
	(t (defun lyskom-read-from-minibuffer (prompt 
					       &optional initial-contents
					       keymap read hist default-value
					       inherit-input-method)
	     (read-from-minibuffer prompt
				   initial-contents
				   keymap
				   read
				   hist)))))

(lyskom-external-function temp-minibuffer-message)
(lyskom-provide-function minibuffer-message (message)
  (temp-minibuffer-message message))


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


;;; ================================================================
;;; Faces

(lyskom-provide-function reset-face (face &optional locale tag-set exact-p)
  )

(lyskom-provide-function lyskom-face-background-name (face)
  (face-background face))

(lyskom-provide-function lyskom-face-foreground-name (face)
  (face-foreground face))


(lyskom-provide-function find-face (face)
  (and (facep face) face))

(defun lyskom-make-face (name temporary)
  "Like make-face in XEmacs"
  (lyskom-xemacs-or-gnu (make-face name nil temporary)
                        (make-face name)))

(if (not (find-face 'strikethrough))
    (progn (make-face 'strikethrough)
           (if (eval-when-compile (fboundp 'set-face-strikethrough-p))
               (set-face-strikethru-p 'strikethrough t)
             (set-face-underline-p 'strikethrough t))))


;;; ======================================================================
;;; Event stuff

(lyskom-external-function event-start)
(lyskom-provide-function event-point (e)
  "Return the character position of the given mouse event.
If the event did not occur over a window, or did not occur over text,
then this returns nil.  Otherwise, it returns an index into the buffer
visible in the event's window."
  (car (cdr (event-start e))))

(lyskom-provide-function event-closest-point (e)
  "Return the character position closest to the mouse event EVENT."
  (car (cdr (event-start e))))

(lyskom-provide-function event-glyph (e))


(defun lyskom-get-buffer-window-list (buffer &optional minibuf frame)
  "Return windows currently displaying BUFFER, or nil if none.
See `walk-windows' for the meaning of MINIBUF and FRAME."
  (let ((buffer (if (bufferp buffer) buffer (get-buffer buffer))) windows)
    (walk-windows (function (lambda (window)
			      (if (eq (window-buffer window) buffer)
				  (setq windows (cons window windows)))))
		  minibuf frame)
    windows))

(lyskom-provide-function 
 replace-in-string (str regexp newtext &optional literal)
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

;;; Local Variables:
;;; eval: (put 'lyskom-provide-macro 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-provide-function 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-provide-subst 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-compatibility-forms 'lisp-indent-hook 2)
;;; eval: (put 'lyskom-compatibility-definition 'lisp-indent-hook 2)
;;; end:
