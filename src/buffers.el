;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: buffers.el,v 44.18 2000-09-09 11:59:20 byers Exp $
;;;;; Copyright (C) 1991, 1996  Lysator Academic Computer Association.
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
;;;; File: buffers.el
;;;; Author: David Byers
;;;;
;;;; This file implements special buffer handling used in LysKOM
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: buffers.el,v 44.18 2000-09-09 11:59:20 byers Exp $\n"))


;;;;
;;;; LYSKOM BUFFER MANAGEMENT
;;;;
;;;; Buffers are arranged in a tree rooted in a LysKOM buffer. There
;;;; is one tree for each session.
;;;;
;;;; Use lyskom-get-buffer-create to create new buffers
;;;; Use lyskom-display-buffer to display most buffers
;;;; Use lyskom-undisplay-buffer to undisplay those buffers
;;;;





;;;; ============================================================
;;;; Buffer hierarchy management
;;;;
;;;; This code should not be too dependent on LysKOM
;;;;

(defvar lyskom-buffer-children nil
  "List of buffers that are children to this buffer")

(defvar lyskom-buffer-inherited-variables nil
  "List of variables automatically inherited to sub-buffers when
they are created.")

(defvar lyskom-buffer-parent nil
  "Parent of buffer")

(defvar lyskom-killing-hierarchy nil
  "Non-nil while killing a buffer hierarchy.")

(make-variable-buffer-local 'lyskom-buffer-parent)
(lyskom-protect-variable 'lyskom-buffer-parent)

(make-variable-buffer-local 'lyskom-buffer-children)
(lyskom-protect-variable 'lyskom-buffer-children)

(make-variable-buffer-local 'lyskom-buffer-inherited-variables)
(lyskom-protect-variable 'lyskom-buffer-inherited-variables)




(defun lyskom-set-buffer-parent (buffer parent)
  "Set the parent buffer of BUFFER to PARENT. If buffer is already
a child of some buffer, reparent it."
  (save-excursion (set-buffer buffer)
		  (if (and lyskom-buffer-parent
                           (buffer-live-p lyskom-buffer-parent))
                      (lyskom-remove-buffer-child lyskom-buffer-parent buffer))
		  (setq lyskom-buffer-parent parent)
		  (if parent (lyskom-add-buffer-child parent buffer))))

(defun lyskom-remove-buffer-child (buffer child)
  "Remove CHILD from BUFFER's list of children. Args: BUFFER CHILD"
  (save-excursion (set-buffer buffer)
		  (if (boundp 'lyskom-buffer-children)
		      (setq lyskom-buffer-children 
                            (delq child lyskom-buffer-children)))))

(defun lyskom-add-buffer-child (buffer child)
  "Add CHILD as a child of BUFFER. Args: BUFFER CHILD"
  (save-excursion (set-buffer buffer)
		  (setq lyskom-buffer-children
                        (cons child lyskom-buffer-children))))

(defun lyskom-get-buffer-parent (buffer)
  "Return the parent of BUFFER or nil if it has no parent"
  (save-excursion (set-buffer buffer)
		  (and (boundp 'lyskom-buffer-parent)
		       lyskom-buffer-parent)))

(defun lyskom-get-buffer-children (buffer)
  "Return the list of children of buffer BUFFER or nil if there are none."
  (save-excursion (set-buffer buffer)
		  (and (boundp 'lyskom-buffer-children)
		       lyskom-buffer-children)))

(defun lyskom-buffer-root-ancestor (buffer)
  "Return the ultimate ancestor of buffer BUFFER."
  (let ((buffer-parent buffer))
    (while (setq buffer-parent
		 (lyskom-get-buffer-parent buffer))
      (setq buffer buffer-parent))
    buffer))

(defun lyskom-traverse-buffer-hierarchy (function buffer)
  "Apply FUNCTION to each descendent of BUFFER. Results are discarded."
  (let ((queue (list buffer))
        (done nil)
        (current nil))
    (while queue
      (setq current (car queue))
      (setq queue (cdr queue))
      (unless (or (memq current done)
                  (not (buffer-live-p current)))
        (setq done (cons current done))
        (setq queue (append queue (lyskom-get-buffer-children current)))
        (funcall function current)))))
        

(defun lyskom-map-buffer-children (function buffer)
  "Apply FUNCTION to each child of BUFFER and make a list of the results."
  (cond ((null buffer) nil)
	(t (cons (funcall function buffer)
		 (apply 'nconc 
			(mapcar (function
				 (lambda (x)
                                   (lyskom-map-buffer-children function x)))
				(lyskom-get-buffer-children buffer)))))))


(defun lyskom-buffer-hierarchy-query-kill-function ()
  "When querying if a buffer is to be killed, ensure that none of
the children object"
  (save-excursion
    (not (memq nil
               (mapcar (function
                        (lambda (buffer)
                          (if (buffer-live-p buffer)
                              (progn (set-buffer buffer)
                                     (run-hook-with-args-until-failure
                                      'kill-buffer-query-functions))
                            t)))
                       (lyskom-get-buffer-children (current-buffer)))))))
  

(defun lyskom-buffer-hierarchy-kill-hook ()
  "When killing a buffer, enure that its children also die"
  (let ((kill-buffer-query-functions nil)
        (lyskom-killing-hierarchy (or lyskom-killing-hierarchy
                                      (current-buffer))))
    (lyskom-set-buffer-parent (current-buffer) nil)
    (let ((buflist (lyskom-get-buffer-children (current-buffer))))
      (while buflist
        (kill-buffer (car buflist))
        (setq buflist (cdr buflist))))))


(defun lyskom-buffer-kill-trim-hook ()
  "When killing a buffer, run trimming hooks."
  (when (eq major-mode 'lyskom-mode)
    (let ((lyskom-trim-buffer-delete-to (point-max)))
      (lyskom-ignore lyskom-trim-buffer-delete-to)
      (run-hooks 'lyskom-trim-buffer-hook))))

(add-hook 'kill-buffer-hook 'lyskom-buffer-hierarchy-kill-hook)
(add-hook 'kill-buffer-hook 'lyskom-buffer-kill-trim-hook)
(add-hook 'kill-buffer-query-functions
          'lyskom-buffer-hierarchy-query-kill-function)



;;;; ======================================================================
;;;; ======================================================================

(defun lyskom-clean-all-buffer-lists ()
  "Remove dead buffers from all relevant buffer lists"
  (setq lyskom-sessions-with-unread
        (lyskom-clean-buffer-list lyskom-sessions-with-unread)
        lyskom-sessions-with-unread-letters
        (lyskom-clean-buffer-list lyskom-sessions-with-unread-letters)
        lyskom-buffer-list
        (lyskom-clean-buffer-list lyskom-buffer-list))
  (lyskom-set-default 'lyskom-need-prompt-update t))

(defun lyskom-clean-buffer-list (buffers)
  "Remove all dead buffers from BUFFERS"
  (let ((result nil))
    (while buffers
      (if (lyskom-buffer-p (car buffers))
        (setq result (cons (car buffers) result))
        (save-excursion (set-buffer (car buffers))
                        (setq lyskom-session-has-unread-letters nil)
                        (setq lyskom-session-has-unreads nil)))
      (setq buffers (cdr buffers)))
    (nreverse result)))

(defun lyskom-remove-buffer-from-lists (&optional buffer)
    "Remove BUFFER from all internal lists.
If BUFFER is not specified, assume the current buffer"
  (unless buffer (setq buffer (current-buffer)))
  (lyskom-remove-unread-buffer buffer)  
  (setq lyskom-buffer-list
        (delq buffer lyskom-buffer-list))
  (lyskom-set-default 'lyskom-need-prompt-update t))

(defun lyskom-remove-unread-buffer (buffer &optional letters-only)
  (unless letters-only
    (lyskom-traverse-buffer-hierarchy 
     (lambda (x)
       (save-excursion (set-buffer x)
                       (setq lyskom-session-has-unreads nil)))
     buffer)
    (setq lyskom-sessions-with-unread 
          (delq buffer lyskom-sessions-with-unread)))
  (lyskom-traverse-buffer-hierarchy 
   (lambda (x)
     (save-excursion (set-buffer x)
                     (setq lyskom-session-has-unread-letters nil)))
   buffer)
  (setq lyskom-sessions-with-unread-letters
        (delq buffer lyskom-sessions-with-unread-letters))
  (lyskom-set-default 'lyskom-need-prompt-update t))
  

(defun lyskom-add-unread-buffer (buffer &optional letters)
  (unless (memq buffer lyskom-sessions-with-unread)
    (lyskom-traverse-buffer-hierarchy 
     (lambda (x)
       (save-excursion (set-buffer x)
                       (setq lyskom-session-has-unreads t)))
     buffer)
    (setq lyskom-sessions-with-unread
          (cons buffer lyskom-sessions-with-unread)))
  (unless (or (null letters)
              (memq buffer lyskom-sessions-with-unread-letters))
    (lyskom-traverse-buffer-hierarchy 
     (lambda (x)
       (save-excursion (set-buffer x)
                       (setq lyskom-session-has-unread-letters t)))
     buffer)
    (setq lyskom-sessions-with-unread-letters
          (cons buffer lyskom-sessions-with-unread-letters)))
  (lyskom-set-default 'lyskom-need-prompt-update t))

(defvar lyskom-associated-buffer-list nil
  "List of (CATEGORY . BUFFER-LIST) listing all buffers of various
categories")

(make-variable-buffer-local 'lyskom-associated-buffer-list)
(lyskom-protect-variable 'lyskom-associated-buffer-list)

(defvar lyskom-buffer-category nil
  "Category of this buffer")

(make-variable-buffer-local 'lyskom-buffer-category)
(lyskom-protect-variable 'lyskom-buffer-category)

(def-kom-var lyskom-saved-window-configuration nil
  "The window configuration to return to when closing the window"
  protected
  local)

(def-kom-var lyskom-dedicated-frame nil
  "The frame dedicated to the current buffer"
  protected
  local)

(def-kom-var lyskom-dedicated-window nil
  "The window dedicated to the current buffer"
  protected
  local)

(defvar lyskom-undisplaying-hierarchy nil
  "The top of the buffer hierarchy being undisplayed.")



(defun lyskom-clean-up-buffer-category (cat)
  (let ((buffers (cdr (assq cat (lyskom-default-value 'lyskom-associated-buffer-list))))
        (result nil))
    (while buffers
      (when (buffer-live-p (car buffers))
        (setq result (cons (car buffers) result)))
      (setq buffers (cdr buffers)))
    (lyskom-set-buffers-of-category cat (nreverse result))))


(defun lyskom-set-buffers-of-category (category buflist)
  (let ((tmp (assq category (lyskom-default-value 'lyskom-associated-buffer-list))))
    (cond (tmp (setcdr tmp buflist))
          (t (lyskom-setq-default 
              lyskom-associated-buffer-list
              (cons (cons category buflist)
                    (lyskom-default-value 'lyskom-associated-buffer-list)))))))


(defun lyskom-buffers-of-category (cat)
  "Return all live buffers of catgory CAT"
  (lyskom-clean-up-buffer-category cat)
  (cdr (assq cat (lyskom-default-value 'lyskom-associated-buffer-list))))


(defun lyskom-add-buffer-of-category (buffer category)
  "Add BUFFER as a buffer of category CATEGORY"
  (let ((tmp (assq category (lyskom-default-value 'lyskom-associated-buffer-list))))
    (cond (tmp (setcdr tmp (cons buffer (cdr tmp))))
          (t (lyskom-setq-default 'lyskom-associated-buffer-list
                                  (cons (cons category (list buffer))
                                        (lyskom-default-value
                                         'lyskom-associated-buffer-list)))))))


(defun lyskom-quit-query ()
  (if (and (boundp 'lyskom-buffer)
           (local-variable-p 'lyskom-buffer (current-buffer))
             (eq lyskom-buffer (current-buffer))
             (lyskom-buffers-of-category 'write-texts))
    (unwind-protect
        (progn
          (display-buffer (car (lyskom-buffers-of-category 'write-texts)))
          (lyskom-ja-or-nej-p (lyskom-get-string 'quit-in-spite-of-unsent)))
      nil)
    t))

(add-hook 'kill-buffer-query-functions 'lyskom-quit-query)
(add-hook 'kill-emacs-query-functions 'lyskom-quit-query)

(defun lyskom-recode-buffer-name (name)
  (cond ((and (multibyte-string-p name) (not enable-multibyte-characters))
	 (encode-coding-string name 
			       (or (and lyskom-language
					(lyskom-language-coding
					 lyskom-language))
				   'raw-text)))
	((and (not (multibyte-string-p name)) enable-multibyte-characters)
	 (decode-coding-string name 
			       (or (and lyskom-language
					(lyskom-language-coding
					 lyskom-language))
				   'raw-text)))
	(t name)))

(defun lyskom-generate-new-buffer (name)
  (setq name (lyskom-recode-buffer-name name))
  (let ((buf (generate-new-buffer name)))
    (save-excursion
      (set-buffer buf))
    buf))

(defun lyskom-get-buffer-create (category name &optional unique)
  "Create a new buffer of category CATEGORY with name generated from NAME. 
If UNIQUE is non-nil, re-use the first existing buffer of category
CATEGORY, renaming it and killing its local variables.

The created buffer is made a child of the current buffer."
  (setq name (lyskom-recode-buffer-name name))
  (let ((buffers (lyskom-buffers-of-category category))
        (buffer nil))
    (if (and unique buffers)
        (progn (setq buffer (car buffers))
               (save-excursion (set-buffer buffer)
                               (let ((inhibit-read-only t))
;;; +++ FIXME: This is that erase-buffer works if there are widgets
                                 (setq before-change-functions
                                       (delq 'widget-before-change
                                             before-change-functions))
                                 (erase-buffer))
                               (kill-all-local-variables)
                               (if (equal (buffer-name (current-buffer))
                                          name)
                                   nil
                                 (rename-buffer name t))))
      (progn (setq buffer (generate-new-buffer name))
             (lyskom-add-buffer-of-category buffer category)))
    (lyskom-set-buffer-parent buffer (current-buffer))
    (lyskom-update-inherited-variables buffer)
    (save-excursion (set-buffer buffer)
                    (setq lyskom-buffer-category category))
    buffer))



(defun lyskom-update-inherited-variables (buffer)
  "Update all inherited variables in this buffer and propagate them
to all children"
  (save-excursion 
    (let ((variables nil)
          (tmp lyskom-inherited-variables))
      (set-buffer buffer)
      (when lyskom-buffer-parent
        (set-buffer lyskom-buffer-parent)
        (setq variables (mapcar 'symbol-value lyskom-inherited-variables))
        (set-buffer buffer)
        (while tmp
          (make-local-variable (car tmp))
          (set (car tmp) (car variables))
          (setq tmp (cdr tmp)
                variables (cdr variables))))
      (mapcar 'lyskom-update-inherited-variables
              lyskom-buffer-children))))



(defun lyskom-display-buffer (buffer)
  "Display the buffer BUFFER and select the window displaying it.

If BUFFER is already visible in some window in any frame, iconified or
otherwise, make that window visible. Otherwise display buffer as per 
which category it is in. Selects the window.

Returns the window displaying BUFFER."


  (set-buffer buffer)
  (let ((windows (lyskom-get-buffer-window-list buffer nil 0))
        (iconified-frame nil)
        (visible-frame))

    ;;
    ;; Find out if the buffer is visible somewhere
    ;;

    (while windows
      (cond ((eq (frame-visible-p (window-frame (car windows))) 'icon)
             (setq iconified-frame (car windows)))
            ((frame-visible-p (window-frame (car windows)))
             (setq visible-frame (car windows))
             (setq windows nil)))
      (setq windows (cdr windows)))

    ;;
    ;; Display it
    ;;

    (cond
     (visible-frame (select-window visible-frame)
                    visible-frame)
     (iconified-frame (make-frame-visible iconified-frame)
                      (select-window iconified-frame)
                      iconified-frame)

     (t
      (let ((category lyskom-buffer-category)
            (window nil))
        (if (null category) 
            (progn (select-window (display-buffer buffer))
                   (selected-window))

          (let* ((sym (intern-soft (concat "kom-"
                                           (symbol-name category)
                                           "-in-window")))
                 (open (lyskom-default-value-safe sym))
                 (saved-window-configuration
                  (save-excursion
                    (set-buffer (or (and (boundp 'lyskom-buffer)
                                         lyskom-buffer)
                                    (current-buffer)))
                    (current-window-configuration)))
                 (dedicated-frame nil)
                 (dedicated-window nil))
            
            (cond 

             ;;
             ;; NULL -- Just switch to the buffer
             ;;

             ((null open)
              (switch-to-buffer buffer)
              (setq window (selected-window)))

             ;;
             ;; OTHER, OTHER-WINDOW -- Switch to in another window
             ;;

             ((or (eq open 'other) (eq open 'other-window))
              (switch-to-buffer-other-window buffer)
              (setq dedicated-window (selected-window))
              (setq window (selected-window)))
                  
             ;;
             ;; OTHER-FRAME -- Switch to in another frame
             ;; Create frame if none exist
             ;;

             ((eq open 'other-frame)
              (if (eq (selected-frame) (next-frame))
                  (switch-to-buffer-other-frame buffer)
                (other-frame 1)
                (switch-to-buffer buffer))
              (setq window (selected-window)))
                  
             ;;
             ;; NEW-FRAME
             ;; Create a new frame and display buffer in that frame
             ;;

             ((eq open 'new-frame)
              (switch-to-buffer-other-frame buffer)
              (setq dedicated-frame (selected-frame))
              (setq dedicated-window (selected-window))
              (setq window (selected-window))
              (setq saved-window-configuration nil))

             ;;
             ;; String or buffer
             ;; Switch to buffer in window displaying named buffer
             ;; Prefer windows in selected frame
             ;;

             ((and (or (stringp open)
                       (bufferp open))
                   (lyskom-get-buffer-window-list open nil 'visible))
              (let ((tmp (lyskom-get-buffer-window-list open nil 'visible)))
                (setq window (car tmp))
                (while tmp
                  (if (eq (window-frame (car tmp))
                          (selected-frame))
                      (progn
                        (setq window (car tmp))
                        (setq tmp nil))
                    (setq tmp (cdr tmp))))
                (select-window window)
                (switch-to-buffer buffer)))

             ;;
             ;; Otherwise just switch
             ;;
             
             (t (switch-to-buffer buffer)
                (setq window (selected-window))))


            ;;
            ;; Set up kill-buffer-hooks and similar things
            ;;

            (select-window window)
            (set-buffer buffer)
            (make-local-variable 'lyskom-dedicated-frame)
            (setq lyskom-dedicated-frame dedicated-frame)
            (make-local-variable 'lyskom-dedicated-window)
            (setq lyskom-dedicated-window dedicated-window)
            (make-local-variable 'lyskom-saved-window-configuration)
            (setq lyskom-saved-window-configuration
                  saved-window-configuration)
            (make-local-hook 'kill-buffer-hook)
            (add-hook 'kill-buffer-hook
                      'lyskom-undisplay-buffer-hook nil t)
            (put 'kill-buffer-hook 'permanent-local t)
            window)))))))


(defun lyskom-undisplay-buffer-hook ()
  (save-excursion (lyskom-undisplay-buffer (current-buffer))))

(defun lyskom-undisplay-buffer (&optional buffer)
  "Undisplay BUFFER. If buffer is not specified, undisplay the current
buffer"

  (setq buffer (or buffer (current-buffer)))
  (let ((dedicated-frame (and (boundp 'lyskom-dedicated-frame)
                              lyskom-dedicated-frame))
        (dedicated-window (and (boundp 'lyskom-dedicated-window)
                                       lyskom-dedicated-window))
        (lyskom-undisplaying-hierarchy (or buffer
                                           lyskom-undisplaying-hierarchy))
        (saved-window-configuration
         (and (boundp 'lyskom-saved-window-configuration)
              lyskom-saved-window-configuration)))
         
    (cond 

     ;;
     ;; If buffer has a dedicated frame AND
     ;; buffer is visible in that frame THEN
     ;; delete the frame
     ;;

     (dedicated-frame
      (when (memq dedicated-frame
                  (mapcar 'window-frame
                          (lyskom-get-buffer-window-list buffer nil t)))
        (delete-frame dedicated-frame)
        (setq dedicated-frame nil)))

     ((and lyskom-killing-hierarchy
           (not (eq lyskom-killing-hierarchy buffer))) nil)
     ((and lyskom-undisplaying-hierarchy
           (not (eq lyskom-undisplaying-hierarchy buffer))) nil)

     (dedicated-window
      (when (and (window-live-p dedicated-window)
                 (eq (get-buffer buffer)
                     (window-buffer dedicated-window)))
        (cond ((null (delq dedicated-window
                           (window-list (window-frame dedicated-window))))
               (bury-buffer buffer))
              (t (delete-window dedicated-window)))
          (setq dedicated-window nil))
      (when saved-window-configuration
        (lyskom-set-partial-window-configuration
         saved-window-configuration)))

     (saved-window-configuration
      (lyskom-set-partial-window-configuration
       lyskom-saved-window-configuration)))))

(defun lyskom-set-partial-window-configuration (configuration)
  "Set CONFIGURATION as the current window configuration with the exception
of point mark and window-start in all windows."
  (let* ((info nil)
         (binfo nil))
    (save-excursion
      (setq binfo
            (mapcar (function
                     (lambda (b)
                       (set-buffer b)
                       (list b (point) (mark t))))
                    (buffer-list)))
      (walk-windows (function
                     (lambda (w)
                       (set-buffer (window-buffer w))
                       (setq info
                             (cons
                              (list w
                                    (window-start w)
                                    (window-point w)
                                    (window-buffer w))
                              info))))
                    t t))
    (unwind-protect
        (set-window-configuration configuration)
      (save-selected-window
        (let (buffer window saved-point saved-mark saved-start)

          (while binfo
            (setq buffer (elt (car info) 0)
                  saved-point (elt (car info) 1)
                  saved-mark (elt (car info) 2))
            (when (buffer-live-p buffer)
              (set-buffer buffer)
              (goto-char saved-point)
              (set-mark saved-mark))
            (setq binfo (cdr binfo)))

          (while info
            (setq window (elt (car info) 0)
                  saved-start (elt (car info) 1)
                  saved-point (elt (car info) 2)
                  buffer (elt (car info) 3))

            (when (and (window-live-p window)
                       (eq (window-buffer window)
                           buffer))
              (set-window-start window saved-start)
              (set-window-point window saved-point))
            (setq info (cdr info))))))))
