;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: utilities.el,v 44.39 1999-10-18 18:54:36 byers Exp $
;;;;; Copyright (C) 1996  Lysator Academic Computer Association.
;;;;;
;;;;; This file is part of the LysKOM server.
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
;;;; File: utilities.el
;;;;
;;;; This file contains general lisp utility functions and
;;;; lyskom-specific utility functions (such as date formatting and
;;;; minibuffer reading)
;;;;


(setq lyskom-clientversion-long
      (concat lyskom-clientversion-long
	      "$Id: utilities.el,v 44.39 1999-10-18 18:54:36 byers Exp $\n"))

;;;
;;; Need Per Abrahamsens widget and custom packages There should be a
;;; better way of doing this, but I'll be darned if I know how. The
;;; various files need to be loaded in a very specific order.
;;;

;;; Define widget wrappers for all the functions in macros.el

;;;
;;; Lisp utility functions
;;;

(defsubst listify-vector (vector)
  "Turn VECTOR into a list"
  (append vector nil))

(defun reverse-assoc (key cache)
  "Same as assoc, but searches on last element in a list"
  (reverse (assoc key (mapcar (function reverse) cache))))


(defun nfirst (n list)
  "Return a list of the N first elements of LIST."
  (if (or (<= n 0) (not list))
      nil
    (cons (car list) (nfirst (1- n) (cdr list)))))

(defun lyskom-rotate-list (list el)
  "Destructively rotate LIST so EL becomes the first element.
If EL occurs more than one, the first occurrence is used."
  (let ((tail (memq el list)))
    (if (or (null tail) (eq el (car list)))
        list
      (setcdr (nthcdr (- (length list) (length tail) 1) list) nil)
      (setcdr (nthcdr (1- (length tail)) tail) list)
      tail)))

(defun lyskom-preceding-cons (list el)
  "Return the cons cell of LIST preceding the first cons cell whose car is EL.
Return nil if TAIL is the same as LIST or not a member of LIST."
  (unless (or (eq (car list) el)
              (not (memq el list)))
    (nthcdr (- (length list) (length (memq el list)) 1) list)))

(defun lyskom-insert-in-list (el list before)
  "Destructively insert EL in LIST before element BEFORE.
If BEFORE is not in the list, then insert EL at the end of the list."
  (cond ((eq before (car list))
         (cons el list))
        (t (setcdr (nthcdr (- (length list)
                              (length (memq before list)) 1) list)
                   (cons el (memq before list)))
           list)))

(defun lyskom-move-in-list (el list pos)
  "Destructively move EL within LIST so it appears at position POS."
  (when (memq el list)
    (setq list (delq el list))
    (cond ((eq 0 pos) (setq list (cons el list)))
          (t (setcdr (nthcdr (1- pos) list)
                     (cons el (nthcdr pos list))))))
  list)

;;;
;;; +++ FIXME: If cl.el can be guaranteed, this is pointless.
;;;

(defun lyskom-butlast (x &optional n)
  "Returns a copy of LIST with the last N elements removed."
  (if (and n (<= n 0)) x
    (lyskom-nbutlast (copy-sequence x) n)))

(defun lyskom-nbutlast (x &optional n)
  "Modifies LIST to remove the last N elements."
  (let ((m (length x)))
    (or n (setq n 1))
    (and (< n m)
	 (progn
	   (if (> n 0) (setcdr (nthcdr (- (1- m) n) x) nil))
	   x))))

(defun skip-first-zeros (list)
  (while (and list (zerop (car list)))
    (setq list (cdr list)))
  list)


(defun filter-list (test list)
  (cond ((null list) '())
	((apply test (car list) nil)
	 (cons (car list) (filter-list test (cdr list))))
	(t (filter-list test (cdr list)))))

;;;============================================================
;;;
;;; Utility functions.
;;;
;;; These should be shared in LysKOM
;;;

(lyskom-provide-function copy-tree (l)
  "Recursively copy the list L"
  (cond ((atom l) l)
        (t (cons (copy-tree (car l))
                 (copy-tree (cdr l))))))

(lyskom-provide-function functionp (obj)
  "Returns t if OBJ is a function, nil otherwise."
  (cond
   ((symbolp obj) (fboundp obj))
   ((subrp obj))
   ((byte-code-function-p obj))
   ((consp obj)
    (if (eq (car obj) 'lambda) (listp (car (cdr obj)))))
   (t nil)))


(defun lyskom-ignore (&rest args)
  "Ignore all arguments"
  )

(defun regexpp (re)
  "Return non-nil if RE looks like a valid regexp."
  (let ((result t))
    (save-match-data
      (condition-case nil
          (string-match re "")
        (error (setq result nil))))
    result))


(defun mapcar2 (fn seq1 seq2)
  (let (result)
    (while (and seq1 seq2)
      (setq result (cons (funcall fn (car seq1) (car seq2)) result))
      (setq seq1 (cdr seq1)
            seq2 (cdr seq2)))
    (nreverse result)))


(defun lyskom-maxint ()
  (let ((n 1) 
        (l nil)
        (i 31))
    (while (and (> n 0) (> i 0))
      (setq l (cons n l))
      (setq n (* 2 n))
      (setq i (1- i)))
    (apply '+ l)))

;; Set lyskom-maxint correctly

(setq lyskom-max-int (lyskom-maxint))


(defun lyskom-try-require (feature &optional message &rest args)
  "Load the feature FEATURE using require. 
If optional MESSAGE is non-nil, use it as a LysKOM format string 
taking one string argument to print an error message. Remaining
arguments are used as arguments for the format string.

Returns t if the feature is loaded or can be loaded, and nil otherwise."
  (or (featurep 'feature)
      (condition-case nil
          (progn (require feature)
                 t)
        (error 
         (when message
           (apply 'lyskom-format-insert-before-prompt message (symbol-name feature) args))
         nil))))
                               
      

(defun lyskom-emacs-version ()
  (cond ((string-match "^XEmacs" (emacs-version)) 'xemacs)
	(t 'emacs)))


(defvar lyskom-apo-timeout 0
  "Current millisecond timeout value for accept-process-output")

(defvar lyskom-apo-timeout-index 0
  "Index in lyskom-apo-timeout-vector-max where last timeout is")

(defconst lyskom-apo-timeout-vector
  [0 1000 1000 2000 3000 5000 8000 13000 21000 34000 55000 89000 144000 233000 377000 610000]
  "Vector of timeout values (usecs) for accept-process-output")

(defconst lyskom-apo-timeout-vector-max (1- (length lyskom-apo-timeout-vector))
  "Maximum index in lyskom-apo-timeout-vector")

(defun lyskom-next-apo-timeout ()
  (if (< lyskom-apo-timeout-index lyskom-apo-timeout-vector-max)
      (setq lyskom-apo-timeout
            (aref lyskom-apo-timeout-vector
                  (setq lyskom-apo-timeout-index
                        (1+ lyskom-apo-timeout-index))))))

(defun lyskom-reset-apo-timeout ()
  (setq lyskom-apo-timeout-index -1)
  (setq lyskom-apo-timeout 0))

(defun lyskom-accept-process-output ()
  "Call accept-process-output with the correct timeout values."
  (lyskom-next-apo-timeout)
  (accept-process-output nil 0 lyskom-apo-timeout))

(defun lyskom-current-time (&optional secs)
  "Return the time in a format that LysKOM understands.
If optional argument SECS is set, it is used in place of the value
of \(current-time\)."
  (let ((time (decode-time (or secs (current-time)))))
    (setcar (cdr (cdr (cdr (cdr time))))
            (1- (car (cdr (cdr (cdr (cdr time)))))))
    (setcar (cdr (cdr (cdr (cdr (cdr time)))))
            (- (car (cdr (cdr (cdr (cdr (cdr time))))))
               1900))
    time))



;;;
;;; LysKOM utility functions
;;;

;;;
;;; WARNING!
;;;
;;; The following variable is *important* if you fuck it up in any
;;; way, the functions used to read conference names won't work. So if
;;; you change it, try to work one character at a time, and when
;;; you're done, run through the mappings of all 256 characters to
;;; make sure they look OK.
;;;

(defvar lyskom-default-collate-table
  "\000\001\002\003\004\005\006\007\010 \012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]~€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ !¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿AAAA[]ACEEEEIIIIÐNOOOO\\×OUUUYYÞßAAAA[]ACEEEEIIIIðNOOOO\\÷OUUUYYþÿ"
  "String mapping lowercase to uppercase and equivalents to each others.")

(defsubst lyskom-unicase-char (c)
  "Smash case and diacritical marks on c." 
  (aref lyskom-collate-table (char-to-int c)))

(defun lyskom-unicase (s)
  "Smash case and diacritical marks of all chars in s." 
  (lyskom-save-excursion
   (set-buffer lyskom-buffer)
   (let ((l (length s))
	 (s2 (copy-sequence s)))
     (while (> l 0)
       (setq l (1- l))
       (aset s2 l (lyskom-unicase-char (aref s2 l))))
     s2)))

(defun lyskom-string-assoc (key list)
  "Return non-nil if KEY is the same string as the car of an element of LIST.
The value is actually the element of LIST whose car equals KEY."
  (let ((s (downcase key))
        (result nil))
    (while list
      (when (string= s (downcase (car (car list))))
        (setq result (car list))
        (setq list nil))
      (setq list (cdr list)))
    result))

(defun lyskom-set-default (sym val)
  "Set the value of SYM in the LysKOM buffer to VAL."
  (save-excursion
    (set-buffer (or (and (boundp 'lyskom-buffer)
                         (bufferp lyskom-buffer)
                         (buffer-live-p lyskom-buffer)
                         lyskom-buffer)
                    (current-buffer)))
    (set sym val)))

(defun lyskom-default-value (sym)
  "Get the value of SYM in the LysKOM buffer"
  (save-excursion
    (set-buffer (or (and (boundp 'lyskom-buffer)
                         (bufferp lyskom-buffer)
                         (buffer-live-p lyskom-buffer)
                         lyskom-buffer)
                    (current-buffer)))
    (symbol-value sym)))

(defun lyskom-default-value-safe (sym)
  "Get the value of SYM in the LysKOM buffer"
  (save-excursion
    (set-buffer (or (and (boundp 'lyskom-buffer)
                         (bufferp lyskom-buffer)
                         (buffer-live-p lyskom-buffer)
                         lyskom-buffer)
                    (current-buffer)))
    (and (boundp sym) (symbol-value sym))))


;;; ============================================================
;;; Prefix arguments

(defun lyskom-read-text-no-prefix-arg (prompt &optional always-read)
  "Call in interactive list to read text-no.
If optional argument ALWAYS-READ is non-nil the user is prompted if
an explicit prefix argument was not given."
  (cond
   ((null current-prefix-arg) 
    (if always-read 
        (lyskom-read-number prompt lyskom-current-text)
      lyskom-current-text))
   ((integerp current-prefix-arg) current-prefix-arg)
   ((listp current-prefix-arg) 
    (lyskom-read-number prompt (lyskom-text-at-point)))
   (t (lyskom-error (lyskom-get-string 'bad-text-no-prefix)
                    current-prefix-arg))))



;;; ======================================================================
;;; Display device management
;;;


;;; Definition of some useful functions from XEmacs

(lyskom-provide-function console-type (&optional console)
  (or window-system 'tty))

(lyskom-provide-function device-class (&optional device)
  (condition-case nil
      (if (x-display-grayscale-p)
	  (if (x-display-color-p)
	      'color
	    'grayscale)
	'mono)
    (error 'mono)))


(lyskom-provide-function frame-property (frame property &optional default)
  (or (cdr (assq property (frame-parameters frame)))
      default))


;;; XEmacs doesn't seem to have a background-mode frame property

(defun lyskom-background-mode ()
  (frame-property (selected-frame) 'background-mode 'light))


;;; ======================================================================
;;; LysKOM Hooks
;;;

(defun lyskom-run-hook-with-args (hook &rest args)
  "Run HOOK with the specified arguments ARGS in the LysKOM buffer.
See run-hook-with-args for detailed information."
  (save-excursion (set-buffer (or (and (boundp 'lyskom-buffer)
                                       lyskom-buffer)
                                  (current-buffer)))
                  (apply 'run-hook-with-args hook args)))
                              

(defun lyskom-add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION in the LysKOM buffer.
If optional APPEND is non-nil, add at the end of HOOK."
  (save-excursion (set-buffer (or (and (boundp 'lyskom-buffer)
                                       lyskom-buffer)
                                  (current-buffer)))
                  (add-hook hook function append t)))

(defun lyskom-remove-hook (hook function)
  "From the value of HOOK remove the function FUNCTION in the LysKOM buffer."
  (save-excursion (set-buffer (or (and (boundp 'lyskom-buffer)
                                       lyskom-buffer)
                                  (current-buffer)))
                  (remove-hook hook function t)))




;;; ======================================================================
;;; Printing
;;;
;;; XEmacs princ does not insert text properties. This function is based
;;; on the C code for princ. It only works on strings
;;;

(defun lyskom-princ (string &optional stream)
  "Similar to princ but will only print a string. Does not lose text properties
under XEmacs."
  (let ((old-point nil)
        (start-point nil)
        (old-buffer (current-buffer)))
    (unwind-protect
        (progn
          (cond ((bufferp stream) (set-buffer stream))
                ((markerp stream) 
                 (setq old-point (point))
                 (set-buffer (marker-buffer stream))
                 (goto-char stream)
                 (setq start-point (point))))

          (insert string))
      (cond ((markerp stream) 
             (set-marker stream (point))
             (if (>= old-point start-point)
                 (goto-char (+ old-point (- (point) start-point)))
               (goto-char old-point))))
      (set-buffer old-buffer))))


;;; ======================================================================
;;; Faces
;;;

(defun lyskom-set-face-foreground (face color)
  (condition-case nil
      (set-face-foreground face color)
    (error nil)))

(defun lyskom-set-face-background (face color)
  (condition-case nil
      (set-face-background face color)
    (error nil)))

(defun lyskom-copy-face (old new)
  (lyskom-xemacs-or-gnu (copy-face old new nil nil nil 'remove-all)
                        (copy-face old new)))



(defun lyskom-set-face-scheme (scheme)
  "Set the LysKOM color and face scheme to SCHEME. Valid schemes are listed
in lyskom-face-schemes."
  (let ((tmp (assoc scheme lyskom-face-schemes)))
    (when (and tmp
               (fboundp 'copy-face)
               (fboundp 'lyskom-set-face-foreground)
               (fboundp 'lyskom-set-face-background))
      (mapcar 
       (function
        (lambda (spec)
          (lyskom-copy-face (or (elt spec 1) 'default) (elt spec 0))
          (if (elt spec 2)
              (lyskom-set-face-foreground (elt spec 0) (elt spec 2)))
          (if (elt spec 3)
              (lyskom-set-face-background (elt spec 0) (elt spec 3)))))
       (cdr tmp)))))


(defun lyskom-face-resource (face-name attr type)
  (if (eq (lyskom-emacs-version) 'xemacs)
      ;; XEmac style
      (let ((val (x-get-resource (concat face-name ".attribute" attr)
				 (concat "Face.Attribute" attr)
				 type)))
	(cond ((eq type 'string) val)
	      ((and (eq type 'boolean) val) (if (car val) 'on 'off))
	      (t val)))
    ;; Emacs style
    (let ((val (x-get-resource (concat face-name ".attribute" attr)
			       (concat "Face.Attribute" attr))))
      (cond ((eq type 'string) val)
	    ((and val
		  (eq type 'boolean)
		  (member (downcase val) '("on" "true"))) 'on)
	    ((and val (eq type 'boolean)) 'off)
	    (t val)))))


(defun lyskom-modify-face (what face)
  (condition-case nil
      (funcall (intern (concat "make-face-" (symbol-name what)))
               face)
    (error nil)))

(defun lyskom-setup-faces ()
  "Initalize the faces in the LysKOM client.
This sets the face scheme according to `kom-default-face-scheme', and
also reads the proper X resources."
  (unless kom-default-face-scheme
    (setq kom-default-face-scheme
	  (condition-case nil
	      (cond ((eq (device-class) 'mono) 'monochrome)
		    ((eq (lyskom-background-mode) 'dark)
		     'inverse)
		    (t 'default))
	    (error 'default))))  
  (lyskom-set-face-scheme kom-default-face-scheme)
  (if (eq (console-type) 'x)
      (mapcar
       (function
	(lambda (face)
	  (let* ((face-name (symbol-name face))
		 (fg (lyskom-face-resource face-name "Foreground" 'string))
		 (bg (lyskom-face-resource face-name "Background" 'string))
		 (bl (lyskom-face-resource face-name "Bold" 'boolean))
		 (it (lyskom-face-resource face-name "Italic" 'boolean))
		 (ul (lyskom-face-resource face-name "Underline" 'boolean)))
	    (if fg (set-face-foreground face fg))
	    (if bg (set-face-background face bg))
	    (if (eq bl 'on) (lyskom-modify-face 'bold face))
	    (if (eq bl 'off) (lyskom-modify-face 'unbold face))
	    (if (eq it 'on) (lyskom-modify-face 'italic face))
	    (if (eq it 'off) (lyskom-modify-face 'unitalic face))
	    (if ul (set-face-underline-p face (eq ul 'on))))))
       lyskom-faces)))


;;; ============================================================
;;; Date and time utilities

(defun lyskom-client-date-string (&optional fmt)
  "Format the current client time as a string.
The optional format string FMT specifies the format. If no format string
is supplied time-yyyy-mm-dd-hh-mm is used. The arguments to the format
string are the following: the year, the month, the day, the hour, the 
minutes, the seconds, the full name of the day of week, the abbreviated
name of the day of week."
  (let ((now (decode-time)))
    (lyskom-format (or fmt 'time-yyyy-mm-dd-hh-mm)
                   (elt now 5)
                   (elt now 4)
                   (elt now 3)
                   (elt now 2)
                   (elt now 1)
                   (elt now 0)
                   (elt (lyskom-get-string 'weekdays)
                        (elt now 6))
                   (elt (lyskom-get-string 'weekdays-short)
                        (elt now 6)))))

(defun lyskom-client-date ()
  "Format the current client time as a string.
The optional format string FMT specifies the format. If no format string
is supplied time-yyyy-mm-dd-hh-mm is used. The arguments to the format
string are the following: the year, the month, the day, the hour, the 
minutes, the seconds, the full name of the day of week, the abbreviated
name of the day of week."
  (let ((now (decode-time)))
    (lyskom-create-time (elt now 0)     ;sec
                        (elt now 1)     ;min
                        (elt now 2)     ;hour
                        (1- (elt now 3))     ;mday
                        (elt now 4)     ;mon
                        (- (elt now 5) 1900) ;year
                        (elt now 6)     ;wday
                        0               ;yday
                        (elt now 7)
                        )))

;;; ============================================================
;;; Keymap utilities

(defun lyskom-lookup-key (keymap event &optional accept-default)
  (if (null keymap)
      (and accept-default 
           (lookup-key global-map event))
    (if (not (arrayp event))
	(setq event (vector event)))
    (or (lookup-key keymap event)
        (lyskom-lookup-key (keymap-parent keymap) event accept-default))))

(defun lyskom-keymap-body (keymap)
  (setq keymap (cdr keymap))
  (cond ((arrayp (car keymap)) (car keymap))
        (t keymap)))

(defun lyskom-keymap-realbinding (binding)
  (while (stringp (car-safe binding))
    (setq binding (cdr binding)))
  binding)

(defun lyskom-overlay-keymap (basemap overlay keymap &optional prefix)
  (let ((keys (make-vector (1+ (length prefix)) nil))
        (index (length prefix))
        (body nil)
        (r 0))

    (while (< r (length prefix))
      (aset keys r (aref prefix r))
      (setq r (1+ r)))

    (cond ((not (keymapp keymap)))
          ((not (keymapp overlay)))
          ((not (keymapp basemap)))

          ((setq body (lyskom-keymap-body overlay))
           (mapcar
            (function
             (lambda (element)
               (cond ((arrayp element)
                      (let ((len (length element)))
                        (setq r 0)
                        (while (< r len)
                          (aset keys index r)
                          (lyskom-overlay-keys keys (aref element r)
                                               basemap overlay keymap)
                          (setq r (1+ r)))))

                     ((consp element)
                      (when (not (eq t (car element)))
                        (aset keys index (car element))
                        (lyskom-overlay-keys keys
                                             (lyskom-keymap-realbinding
                                              (cdr element))
                                             basemap overlay keymap)))

                     (t nil))))
            body)))))


(defun lyskom-overlay-keys (keys binding basemap overlay keymap)
  (let ((base-binding (lyskom-lookup-key basemap keys nil)))

   ;; If the binding is a keymap or prefix and
   ;; the binding in the base is a keymap or prefix 
   ;; then recurse

   (cond ((and (keymapp binding)
               (keymapp base-binding))
          (lyskom-overlay-keymap basemap binding keymap keys))

   ;; If the binding is a keymap or prefix and
   ;; we are bound in the base
   ;; then don't recurse

         ((and (keymapp binding)
               base-binding) nil)

   ;; If we are not bound in the base
   ;; copy the binding

         ((and binding
               (null base-binding)) (define-key keymap keys binding)))))


;;;
;;; Stuff
;;;

(defun lyskom-return-membership-type (mt)
  "Return a text description of membership type mt"
  (let ((tmp
         (mapconcat 
          'identity
          (delete nil
                  (list (if (membership-type->invitation mt) (lyskom-get-string 'invitation-mt-type) nil)
                        (if (membership-type->passive mt) (lyskom-get-string 'passive-mt-type) nil)
                        (if (membership-type->secret mt) (lyskom-get-string 'secret-mt-type) nil)))
          ", ")))
    (if (string= tmp "") 
        tmp
      (concat "[" tmp "]"))))

(defun lyskom-find-unread (conf-no)
  "Return the number of unread texts in CONF-NO.
If this function is unable to calculate the number of unread texts it will
return nil."
  (save-excursion
   (set-buffer lyskom-buffer)
   (let ((rlist (read-list->all-entries lyskom-to-do-list))
         (found nil))
     (while (and (not found) rlist)
       (when (eq conf-no (conf-stat->conf-no
                          (read-info->conf-stat (car rlist))))
         (setq found (length (cdr (read-info->text-list (car rlist))))))
       (setq rlist (cdr rlist)))
     found)))

(defun lyskom-prev-area (num prop)
  (while (> num 0)
    (let ((where (previous-single-property-change (point) prop)))
      (when where
        (if (not (get-text-property where prop))
            (setq where (previous-single-property-change 
                         where prop)))
        (if where
            (goto-char where)
          (goto-char (point-min))
          (setq num 1))))
    (setq num (1- num))))

(defun lyskom-next-area (num prop)
  "Move the cursor to the next prompt in the LysKOM buffer"
  (interactive "p")
  (while (> num 0)
    (let ((where (next-single-property-change (point) prop)))
      (when where
        (if (not (get-text-property where prop))
            (setq where (next-single-property-change where prop)))
        (if where
            (goto-char where)
          (goto-char (point-max))
          (setq num 1))))
    (setq num (1- num))))
