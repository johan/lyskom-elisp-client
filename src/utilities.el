;;;;; -*- emacs-lisp -*-
;;;;; $Id: utilities.el,v 44.1 1996-09-25 17:29:58 byers Exp $
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
	      "$Id: utilities.el,v 44.1 1996-09-25 17:29:58 byers Exp $\n"))


;;;
;;; Lisp utility functions
;;;

(lyskom-provide-function rassoc (key list)
  "Return non-nil if KEY is `equal' to the cdr of an element of LIST.
The value is actually the element of LIST whose cdr is KEY."
  (catch 'rassoc
    (while list
      (if (equal key (cdr (car list)))
          (throw 'rassoc (car list))
        (setq list (cdr list))))
    (throw 'rassoc nil)))


(lyskom-provide-function rassq (key list)
  "Return non-nil if KEY is `eq' to the cdr of an element of LIST.
The value is actually the element of LIST whose cdr is KEY."
  (catch 'rassq
    (while list
      (if (eq key (cdr (car list)))
          (throw 'rassq (car list))
        (setq list (cdr list))))
    (throw 'rassq nil)))

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

(defun copy-tree (l)
  "Recursively copy the list L"
  (cond ((atom l) l)
        (t (cons (copy-tree (car l))
                 (copy-tree (cdr l))))))

(defun functionp (fn)
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


;;;
;;; LysKOM utility functions
;;;

(defvar lyskom-default-collate-table
" 	

 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\][^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ\\][~���������������������������������!������������������������������AAAA\\[\\CEEEEIIII�NOOOO]*]UUUYY��AAAA\\[\\CEEEEIIII�NOOOO]/]UUUYY��")



(defsubst lyskom-unicase-char (c)
  "Smash case and diacritical marks on c." 
  (aref lyskom-collate-table c))

(defun lyskom-unicase (s)
  "Smash case and diacritical marks of all chars in s." 
  (lyskom-save-excursion
   (set-buffer (process-buffer lyskom-blocking-process))
   (let ((l (length s))
	 (s2 (copy-sequence s)))
     (while (> l 0)
       (setq l (1- l))
       (aset s2 l (lyskom-unicase-char (aref s2 l))))
     s2)))


;;; ============================================================
;;; LysKOM Buffer management
;;;

;;; To create a new buffer associated with a LysKOM buffer, do
;;;
;;;     (lyskom-associate-buffer (buffer-creation-function args))
;;;
;;; This call can only be made when the current buffer is another
;;; LysKOM-associated buffer.
;;;
;;; When doing things that can kill local variables in an associated
;;; buffer, surround those things by lyskom-protect-environment. This
;;; ensures that vital local variables are remain. For instance, when
;;; setting a mode in a LysKOM-buffer, do
;;;
;;;     (lyskom-protect-environment (lyskom-edit-mode))
;;;
;;; Next, to display the buffer, lyskom-display-buffer comes in
;;; handy. It takes three arguments, the buffer to be displayed,
;;; the name of a variable that controls how it is displayed and the
;;; name of a variable that controls how it is undisplayed.
;;;
;;; The variable that controls how it is displayed must be defined
;;; in the LysKOM buffer. It can have the values 'other 'other-frame
;;; 'new-frame, a string or buffer, or nil.
;;;
;;; The variable that controls undisplay should by nil if the window
;;; configuration should be restored when the buffer is undisplayed
;;; and if any dedicated frames for the buffer should be deleted. It
;;; should be t otherwise (see kom-dont-restore-<something> for an
;;; example.)
;;;
;;; Finally, to undisplay the buffer, use lyskom-undisplay-buffer.
;;; Given no args is undisplays the current buffer, otherwise the
;;; buffer given as its argument.
;;;

(defvar kill-buffer-hook nil
  "Hook to be run (by `run-hooks', which see) when a buffer is killed.
The buffer being killed will be current while the hook is running.
See `kill-buffer'.")

(defvar lyskom-dedicated-frame nil
  "Non-nil if a new frame was created for the current buffer. The value is 
the frame that was created.")

(defvar lyskom-reset-var nil
  "Name of variable that controls how the windows and frames are reset
when the current buffer is undisplayed.")

(defvar lyskom-original-window-configuration nil
  "The window configuration that was current when the current buffer
was created.")
                                    

(defun lyskom-default-symbol-value (sym)
  "Get the value of the symbol SYM in the LysKOM buffer."
  (save-excursion
    (set-buffer lyskom-buffer)
    (symbol-value sym)))


(defun lyskom-delete-frame-hook ()
  "Delete the frame showing the current buffer if doing so makes sense."
  (if (and (boundp 'lyskom-dedicated-frame)
           lyskom-dedicated-frame
           (memq lyskom-dedicated-frame 
                 (mapcar 'window-frame
                         (get-buffer-window-list (current-buffer) nil t)))
           (not (lyskom-default-symbol-value lyskom-reset-var)))
      (progn
        (delete-frame lyskom-dedicated-frame)
        (setq lyskom-dedicated-frame nil)
        (remove-hook 'kill-buffer-hook 'lyskom-delete-frame-hook))))

;;;
;;; OK, so you think the following code is bizarre. It is.
;;;
;;; Set the current buffer to the LysKOM buffer, then save the
;;; excursion. This is to stop set-window-configuration from
;;; "restoring" point and mark all over the place, which is something
;;; we do NOT want. It's not perfekt because it will reset point and
;;; mark everywhere else, which is a Bad Thing.
;;;
;;; his would be more elegant if we had a way of saving the window
;;; configuration WITHOUT point and mark.
;;;

(defun lyskom-reset-window-configuration-hook ()
  "Reset the saved window configuration if doing so makes sense."
  (let ((cur (current-buffer)))
    (set-buffer lyskom-buffer)
    (save-excursion
      (set-buffer cur)
      (if (and (boundp 'lyskom-original-window-configuration)
               lyskom-original-window-configuration
               (not (lyskom-default-symbol-value lyskom-reset-var)))
          (progn (set-window-configuration 
                  lyskom-original-window-configuration)
                 (set-buffer (window-buffer (selected-window))))
        (bury-buffer)))))


(defun lyskom-display-buffer (buffer open-var reset-var)
  "Display BUFFER in a way according to OPEN-VAR. RESET-VAR controls how
the buffer is removed."
  (let ((open-var-value (symbol-value open-var))
        (original-window-configuration 
         (save-excursion
           (if (and (boundp 'lyskom-buffer)
                    lyskom-buffer)
               (set-buffer lyskom-buffer))
           (current-window-configuration)))
        (dedicated-frame nil))

    (cond ((null open-var-value) 
           (switch-to-buffer buffer))

          ((eq open-var-value 'other)
           (switch-to-buffer-other-window buffer)
           (make-local-hook 'kill-buffer-hook)
           (add-hook 'kill-buffer-hook
                     'lyskom-reset-window-configuration-hook nil t))

          ((and (eq open-var-value 'other-frame)
                (not (eq (selected-frame) (next-frame))))
           (select-frame (next-frame))
           (switch-to-buffer buffer))

          ((eq open-var-value 'new-frame)
           (switch-to-buffer-other-frame buffer)
           (setq dedicated-frame (selected-frame))
           (make-local-hook 'kill-buffer-hook)
           (setq original-window-configuration nil)
           (add-hook 'kill-buffer-hook 'lyskom-delete-frame-hook nil t))

          ((and (or (stringp open-var-value)
                    (bufferp open-var-value))
                (get-buffer-window open-var-value))
           (select-window (get-buffer-window open-var-value))
           (switch-to-buffer buffer))

          (t (switch-to-buffer buffer)))
    (make-local-variable 'lyskom-dedicated-frame)
    (make-local-variable 'lyskom-reset-var)
    (make-local-variable 'lyskom-original-window-configuration)
    (setq lyskom-dedicated-frame dedicated-frame)
    (setq lyskom-reset-var reset-var)
    (setq lyskom-original-window-configuration original-window-configuration)))


(defun lyskom-undisplay-buffer (&optional buffer)
  "Undisplay the buffer BUFFER. If BUFFER is not specified, undisplay the
current buffer."
  (save-excursion
    (if buffer
        (set-buffer buffer)
      (setq buffer (current-buffer)))
    (cond ((not (and (boundp 'lyskom-reset-var)
                     lyskom-reset-var))
           (bury-buffer))

          ((and (boundp 'lyskom-dedicated-frame)
                lyskom-dedicated-frame)
           (lyskom-delete-frame-hook))
          (t (lyskom-reset-window-configuration-hook)))
    buffer))
      

(defun lyskom-associate-buffer (buffer)
  "Set up the buffer BUFFER as a basic LysKOM-associated buffer.
Returns BUFFER."
  (let ((inherited (mapcar 'symbol-value lyskom-inherited-variables)))
    (save-excursion
      (set-buffer buffer)
      (mapcar (function
               (lambda (v)
                 (make-local-variable v)
                 (set v (car inherited))
                 (setq inherited (cdr inherited))))
              lyskom-inherited-variables))
    buffer))


(defmacro lyskom-protect-environment (&rest forms)
  "Evaluate FORMS while protecting the inherited variables in the 
current buffer."
  (` (let ((buffer\ save (current-buffer))
           (,@ (mapcar
                (function
                 (lambda (v)
                   (list (intern (concat (symbol-name v) " save"))
                         (list 'if (list 'boundp (list 'quote v)) v nil))))
                lyskom-protected-variables)))
       (unwind-protect
           (progn
             (,@ forms))
         (if (buffer-live-p buffer\ save)
             (save-excursion
               (set-buffer buffer\ save)
               (,@ (mapcar
                    (function
                     (lambda (v)
                       (list 'make-local-variable (list 'quote v))))
                    lyskom-protected-variables))
               (,@ (mapcar
                    (function
                     (lambda (v)
                       (list 'setq v 
                             (intern (concat (symbol-name v) " save")))))
                    lyskom-protected-variables))))))))

