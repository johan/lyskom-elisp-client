;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: keyboard-menu.el,v 44.1 2002-06-26 20:20:02 byers Exp $
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
;;;; File: keyboard-menu.el
;;;;
;;;; Implements completing-read almost-compatible lyskom-read-from-menu
;;;; used to implement keyboard navigation of LysKOM menus ('cos tmm
;;;; was *so* ugly).
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: keyboard-menu.el,v 44.1 2002-06-26 20:20:02 byers Exp $\n"))

(defvar lyskom-keyboard-menu-menu)
(defvar lyskom-keyboard-menu-buffer)
(defvar lyskom-keyboard-menu-prompt)
(defvar lyskom-keyboard-menu-selection)
(defvar lyskom-keyboard-menu-overlay)

(defun lyskom-keyboard-menu-read-char (prompt)
  (message prompt)
  (sit-for 0)
  (lyskom-xemacs-or-gnu
   (let ((ev (next-command-event nil prompt)))
	    (when (eq 'key-press (event-type ev))
	      (cond ((eq (event-key ev) 'return) 'return)
		    ((event-to-character ev))
		    ((event-key ev))
		    (t nil))))
   (read-event)))


(defun lyskom-keyboard-menu-keys-for-string (string)
  "Return a list of suitable mnemonics for menu item STRING."
  (let ((a-uc nil)
        (a-lc nil)
        (b-uc nil)
        (b-lc nil)
        (c-uc nil)
        (c-lc nil))
    (lyskom-traverse word (string-split " " string)
      (unless (eq (elt word 0) ?\()
        (setq a-lc (cons (downcase (substring word 0 1)) a-lc)
              a-uc (cons (upcase (substring word 0 1)) a-uc))
        (when (> (length word) 1)
          (setq b-lc (cons (downcase (substring word 1 2)) b-lc)
                b-uc (cons (upcase (substring word 1 2)) b-uc)))
        (when (> (length word) 2)
          c-lc (cons (downcase (substring word 2 3)) c-lc)
          c-uc (cons (upcase (substring word 2 3)) c-uc))
        ))
    (mapcar (lambda (s)
              (lyskom-xemacs-or-gnu
               s
               (encode-coding-string s default-keyboard-coding-system)))
            (nconc (nreverse a-lc)
                   (nreverse b-lc)
                   (nreverse c-lc)
                   (nreverse a-uc)
                   (nreverse b-uc)
                   (nreverse c-uc)
                   ))))


(defun lyskom-keyboard-menu-frob-table (table)
  "Convert TABLE to a form suitable for keyboard menus.
TABLE is an alist whose elements' cars are strings. The result will
be an alist whose elements' cars are mnemonics for the string in the
same element's cdr."
  (let ((used-keys nil)
        (default-keys "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (default-keys-index 0))
    (nconc
     (mapcar 
      (lambda (element)
        (let* ((string (car element))
               (keys (lyskom-keyboard-menu-keys-for-string string))
               (mnemonic (lyskom-traverse key keys
                           (unless (member key used-keys)
                             (lyskom-traverse-break key)))))

          (while (and (not mnemonic)
                      (< default-keys-index (length default-keys)))
            (let ((key (substring default-keys 
                                  default-keys-index
                                  (1+ default-keys-index))))
              (unless (member key used-keys)
                (setq mnemonic key))))

          (setq used-keys (cons mnemonic used-keys))
          (list mnemonic string element)))
      table)
     (list (list "C-g" (lyskom-get-string 'keyboard-cancel) nil)))))



(defun lyskom-keyboard-menu-highlight-selected ()
  (set-buffer lyskom-keyboard-menu-buffer)
  (let ((pos (text-property-any (point-min) 
                                (point-max)
                                'lyskom-keyboard-menu-item
                                lyskom-keyboard-menu-selection)))
    (cond ((or (null pos)
               (null lyskom-keyboard-menu-selection))
           (lyskom-xemacs-or-gnu
            (set-extent-face lyskom-keyboard-menu-overlay nil)
            (overlay-put lyskom-keyboard-menu-overlay 'face nil))
           (goto-char (point-min)))
          (t (lyskom-xemacs-or-gnu
              (set-extent-endpoints lyskom-keyboard-menu-overlay
                                    pos
                                    (save-excursion (goto-char pos)
                                                    (end-of-line)
                                                    (1+ (point))))
              (move-overlay lyskom-keyboard-menu-overlay
                            pos
                            (save-excursion (goto-char pos)
                                            (end-of-line)
                                            (1+ (point)))))
             (goto-char pos)
             (lyskom-xemacs-or-gnu
              (set-extent-face lyskom-keyboard-menu-overlay 'kom-mark-face)
              (overlay-put lyskom-keyboard-menu-overlay
                           'face 'kom-mark-face))))))

(defun lyskom-read-from-menu (prompt table)
  "Let the user select one of the values in TABLE.
TABLE is an alist whose elements' cars are strings.
Returns the selected string."
  (let* ((menu (lyskom-keyboard-menu-frob-table table))
         (buffer (get-buffer-create "*Keyboard menu*"))
         (line-length (apply 'max
                             (mapcar (lambda (x)
                                       (+ (length (elt x 0))
                                          (length (elt x 1))
                                          3))
                                     menu))))
    (save-excursion

      ;; Format the buffer

      (set-buffer buffer)
      (erase-buffer)
      (insert (format "%s\n%s\n"
                      prompt
                      (make-string line-length ?-)))
      (lyskom-traverse el menu
        (unless (cdr (memq el menu))
          (insert (make-string line-length ?-))
          (insert "\n"))
        (let ((start (point)))
          (insert (format "%s (%s)\n" (elt el 1) (elt el 0)))
          (add-text-properties start
                               (point)
                               (list 'lyskom-keyboard-menu-item el)
                               )))
      (goto-char (point-min))

      (setq table (cons (cons nil nil) table))

      ;; Display the window and do the thing

      (save-window-excursion
        (split-window (selected-window) (+ 5 (length table)))
        (switch-to-buffer buffer)
        (let* ((lyskom-keyboard-menu-buffer buffer)
               (lyskom-keyboard-menu-menu menu)
               (lyskom-keyboard-menu-selection nil)
               (lyskom-keyboard-menu-overlay 
                (lyskom-xemacs-or-gnu
                 (make-extent 1 1 lyskom-keyboard-menu-buffer)
                 (make-overlay 0 0 lyskom-keyboard-menu-buffer)))
               (result nil))

          (lyskom-xemacs-or-gnu
           (set-extent-face lyskom-keyboard-menu-overlay nil)
           (overlay-put lyskom-keyboard-menu-overlay 'face nil))

          (condition-case nil
              (while (null result)
                (lyskom-keyboard-menu-highlight-selected)
                (let ((c (lyskom-keyboard-menu-read-char 
                          (lyskom-format "%#1s %#2s: %#3s"
                                         prompt
                                         (lyskom-get-string 'keyboard-menu-help)
                                         (or (elt lyskom-keyboard-menu-selection 1)
                                             "")))))
                  (cond ((eq (lookup-key global-map (vector c)) 'keyboard-quit)
                         (keyboard-quit))
                        ((or (eq c 'up)
			     (eq c ?\C-p)
			     (eq c ?\C-b)
			     (eq c 'left))
                         (lyskom-keyboard-menu-up))
                        ((or (eq c 'down)
			     (eq c ?\C-n)
			     (eq c ?\C-f)
			     (eq c 'right))
                         (lyskom-keyboard-menu-down))
                        ((or (eq c 'return)
			     (eq c ?\r)
			     (eq c ?\n))
                         (setq result lyskom-keyboard-menu-selection))
                        ((and (characterp c)
                              (assoc (make-string 1 c) 
				     lyskom-keyboard-menu-menu))
                         (if kom-keyboard-menu-immediate-selection
                             (setq result (assoc (make-string 1 c) 
						 lyskom-keyboard-menu-menu))
                           (setq lyskom-keyboard-menu-selection
                                 (assoc (make-string 1 c)
					lyskom-keyboard-menu-menu))))))
                (when lyskom-keyboard-menu-selection
                  (setq lyskom-keyboard-menu-menu
                        (lyskom-rotate-list lyskom-keyboard-menu-menu
                                            lyskom-keyboard-menu-selection))))
            (quit (setq result nil)))
          (elt result 2)
      )))))


(defun lyskom-keyboard-menu-up ()
  (interactive)
  (setq lyskom-keyboard-menu-selection (car (last lyskom-keyboard-menu-menu))))


(defun lyskom-keyboard-menu-down ()
  (interactive)
  (if lyskom-keyboard-menu-selection
      (setq lyskom-keyboard-menu-selection (car (cdr lyskom-keyboard-menu-menu)))
    (setq lyskom-keyboard-menu-selection (car lyskom-keyboard-menu-menu))))
