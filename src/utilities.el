;;;;; -*- emacs-lisp -*-
;;;;; $Id: utilities.el,v 44.9 1997-02-09 10:50:47 byers Exp $
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
	      "$Id: utilities.el,v 44.9 1997-02-09 10:50:47 byers Exp $\n"))


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

(lyskom-provide-function functionp (fn)
  "Return t if fn is callable"
  (or (byte-code-function-p fn)
      (and (listp fn)
           (eq 'lambda (car fn)))
      (and (symbolp fn)
           (symbol-function fn))))


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
  (let ((n 1) (l nil))
    (while (> n 0)
      (setq l (cons n l))
      (setq n (* 2 n)))
    (apply '+ l)))

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

(defsubst lyskom-next-apo-timeout ()
  (if (< lyskom-apo-timeout-index lyskom-apo-timeout-vector-max)
      (setq lyskom-apo-timeout
            (aref lyskom-apo-timeout-vector
                  (setq lyskom-apo-timeout-index
                        (1+ lyskom-apo-timeout-index))))))

(defsubst lyskom-reset-apo-timeout ()
  (setq lyskom-apo-timeout-index -1)
  (setq lyskom-apo-timeout 0))

(defsubst lyskom-accept-process-output ()
  "Call accept-process-output with the correct timeout values."
  (lyskom-next-apo-timeout)
  (accept-process-output nil 0 lyskom-apo-timeout))


;;;
;;; LysKOM utility functions
;;;

(defvar lyskom-default-collate-table
" 	

 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\][^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ\\][~���������������������������������!������������������������������AAAA\\[\\CEEEEIIII�NOOOO]*]UUUYY��AAAA\\[\\CEEEEIIII�NOOOO]/]UUUYY��")



(defsubst lyskom-unicase-char (c)
  "Smash case and diacritical marks on c." 
  (aref lyskom-collate-table c))

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


(defun lyskom-set-face-scheme (scheme)
  "Set the LysKOM color and face scheme to SCHEME. Valid schemes are listed
in lyskom-face-schemes."
  (let ((tmp (assoc scheme lyskom-face-schemes)))
    ;; This test is NOT good
    (if (and tmp (or (eq (console-type) 'x)
		     (eq (console-type) 'win32)
		     (not (eq (device-class) 'monochro))))
        (progn
          (mapcar 
           (function
            (lambda (spec)
	      (copy-face (or (elt spec 1) 'default) (elt spec 0))
              (if (elt spec 2)
                  (lyskom-set-face-foreground (elt spec 0) (elt spec 2)))
              (if (elt spec 3)
                  (lyskom-set-face-background (elt spec 0) (elt spec 3)))))
	   (cdr tmp))))))


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
  (condition-case tmp
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
