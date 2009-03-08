;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: keyboard-menu.el,v 44.8 2009-03-08 12:20:12 byers Exp $
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
	      "$Id: keyboard-menu.el,v 44.8 2009-03-08 12:20:12 byers Exp $\n"))

(defvar lyskom-keyboard-menu-menu)
(defvar lyskom-keyboard-menu-buffer)
(defvar lyskom-keyboard-menu-prompt)
(defvar lyskom-keyboard-menu-selection)
(defvar lyskom-keyboard-menu-overlay)

(defvar lyskom-keyboard-menu-keymap nil)

(defun lyskom-keyboard-menu-read-char (prompt)
  "Read a single keyboard event from the keyboard.
Like read-event in Gnu Emacs or next-command-event in XEmacs."
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
          (setq c-lc (cons (downcase (substring word 2 3)) c-lc)
                c-uc (cons (upcase (substring word 2 3)) c-uc)))
        ))
    (mapcar (lambda (s)
              (lyskom-xemacs-or-gnu
               s
               (lyskom-encode-coding-string 
                s 
                (and (boundp 'default-keyboard-coding-system)
                     default-keyboard-coding-system))))
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
  "Highlight the selected menu item"
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
          (t (let ((start pos)
                   (end (or (next-single-property-change 
                             pos
                             'lyskom-keyboard-menu-item)
                            (point-max))))
               (lyskom-xemacs-or-gnu
                (set-extent-endpoints lyskom-keyboard-menu-overlay
                                      start end)
                (move-overlay lyskom-keyboard-menu-overlay
                              start end)))
             (goto-char pos)
             (lyskom-xemacs-or-gnu
              (set-extent-face lyskom-keyboard-menu-overlay kom-mark-face)
              (overlay-put lyskom-keyboard-menu-overlay
                           'face kom-mark-face))))
    (save-excursion
      (unless (lyskom-pos-visible-in-window-p pos)
        (recenter)))))


(defun lyskom-keyboard-menu-format-item (item &optional pad-len)
  "Format a single menu item as a string"
  (let ((string (format "%s (%s)" (elt item 1) (elt item 0))))
    (when (and pad-len (> pad-len (length string)))
      (setq string (concat string (make-string (- pad-len (length string))
                                               ?\ ))))
    (add-text-properties 0 
                         (length string)
                         (list 'lyskom-keyboard-menu-item item)
                         string)
    string))

(defun lyskom-keyboard-menu-insert (prompt menu)
  "Insert the text-based representation of the menu.
PROMPT is the menu title and MENU is the menu data."
  (let* ((line-length (apply 'max
                             (mapcar (lambda (x)
                                       (+ (length (elt x 0))
                                          (length (elt x 1))
                                          3))
                                     menu)))
         (columns (/ (+ (window-width) 1)
                     (+ line-length 2)))
         (dashes (make-string (+ (* line-length columns)
                                 (* 2 (1- columns))) ?-))
         (quit-item nil))

    (insert (format "%s\n%s\n" prompt dashes))
    (lyskom-traverse el menu
      (if (null (elt el 2))
          (setq quit-item el)
        (let ((string (lyskom-keyboard-menu-format-item el line-length)))
          (when (> (+ (current-column) (length string) 2) (window-width))
            (insert "\n"))
          (unless (bolp) (insert "  "))
          (insert string))))
    (unless (bolp)
      (insert "\n"))
    (insert dashes)
    (insert "\n")
    (when quit-item
      (insert (lyskom-keyboard-menu-format-item quit-item)))))




;;; ================================================================
;;; Interactive commands
;;; 


(defun lyskom-read-from-menu (prompt table)
  "Let the user select one of the values in TABLE.
TABLE is an alist whose elements' cars are strings.
Returns the selected string."
  (let* ((menu (lyskom-keyboard-menu-frob-table table))
         (buffer (get-buffer-create "*Keyboard menu*"))
         (menu-window-height 0))
    (save-excursion

      ;; Format the buffer

      (set-buffer buffer)
      (erase-buffer)
      (lyskom-keyboard-menu-insert prompt menu)

      (goto-char (point-min))
      (setq menu-window-height (+ 2 (count-lines (point-min) (point-max))))

      ;; Display the window and do the thing

      (save-window-excursion
        (if (< (- (window-height (selected-window))
                  menu-window-height) 5)
            (select-window
             (display-buffer buffer t))
          (split-window (selected-window) menu-window-height)
          (switch-to-buffer buffer))
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
                        ((eq c ?\C-a) (lyskom-keyboard-menu-beginning-of-line))
                        ((eq c ?\C-e) (lyskom-keyboard-menu-end-of-line))
                        ((or (eq c 'up)
                             (eq c ?\C-p))
                         (lyskom-keyboard-menu-up))
                        ((or (eq c ?\C-b)
                             (eq c 'left))
                         (lyskom-keyboard-menu-backward))
                        ((or (eq c 'down)
                             (eq c ?\C-n))
                         (lyskom-keyboard-menu-down))
                        ((or (eq c ?\C-f)
                             (eq c 'right))
                         (lyskom-keyboard-menu-forward))
                        ((or (eq c 'return)
                             (eq c ?\r)
                             (eq c ?\n))
                         (setq result lyskom-keyboard-menu-selection))
                        ((and (lyskom-characterp c)
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
  "Move up one menu item"
  (interactive)
  (let ((done nil))
    (while (not done)

      ;; Move up one line
      (let ((current-prefix-arg nil))
        (condition-case nil
            (call-interactively 'previous-line)
          (beginning-of-buffer (setq done t))))

      ;; Scan the items on this line until we find the one
      ;; just before point or just after point or around
      ;; point.

      (let ((selection nil)
            (start (save-excursion (beginning-of-line) (point)))
            (end (save-excursion (end-of-line) (point)))
            (pos (point)))
        (while (and (>= pos start) (null selection))
          (setq selection 
                (get-text-property pos 'lyskom-keyboard-menu-item)
                pos (1- pos)))

        (setq pos (point))
        (while (and (<= pos end) (null selection))
          (setq selection 
                (get-text-property pos 'lyskom-keyboard-menu-item)
                pos (1+ pos)))
        (when selection (setq done t
                              lyskom-keyboard-menu-selection selection))))))

(defun lyskom-keyboard-menu-down ()
  "Move down one menu item."
  (interactive)
  (let ((done nil))
    (while (not done)

      ;; Move up one line
      (let ((current-prefix-arg nil))
        (condition-case nil
            (call-interactively 'next-line)
          (end-of-buffer (setq done t))))

      ;; Scan the items on this line until we find the one
      ;; just before point or just after point or around
      ;; point.

      (let ((selection nil)
            (start (save-excursion (beginning-of-line) (point)))
            (end (save-excursion (end-of-line) (point)))
            (pos (point)))
        (while (and (>= pos start) (null selection))
          (setq selection 
                (get-text-property pos 'lyskom-keyboard-menu-item)
                pos (1- pos)))

        (setq pos (point))
        (while (and (<= pos end) (null selection))
          (setq selection 
                (get-text-property pos 'lyskom-keyboard-menu-item)
                pos (1+ pos)))
        (when selection (setq done t
                              lyskom-keyboard-menu-selection selection))))))




(defun lyskom-keyboard-menu-forward ()
  "Move right one menu item."
  (interactive)
  (let ((pos (point))
        (item nil))
    (while (and pos (or (null item) (eq item lyskom-keyboard-menu-selection)))
      (setq pos (next-single-property-change pos 'lyskom-keyboard-menu-item))
      (when pos
        (setq item (get-text-property pos 'lyskom-keyboard-menu-item))))
    (when item (setq lyskom-keyboard-menu-selection item))))


(defun lyskom-keyboard-menu-backward ()
  "Move left one menu item."
  (interactive)
  (let ((pos (point))
        (item nil))
    (while (and pos (or (null item) (eq item lyskom-keyboard-menu-selection)))
      (setq pos (previous-single-property-change pos 'lyskom-keyboard-menu-item))
      (when pos
        (setq item (get-text-property pos 'lyskom-keyboard-menu-item))))
    (when item (setq lyskom-keyboard-menu-selection item))))

(defun lyskom-keyboard-menu-beginning-of-line ()
  "Move to the first menu item of this line."
  (interactive)
  (beginning-of-line)
  (setq lyskom-keyboard-menu-selection
        (or (get-text-property (point) 'lyskom-keyboard-menu-item)
            (get-text-property (or (next-single-property-change 
                                    (point) 'lyskom-keyboard-menu-item
                                    nil (save-excursion (end-of-line) (point)))
                                   (point))
                               'lyskom-keyboard-menu-item))))

(defun lyskom-keyboard-menu-end-of-line ()
  "Move to the last menu item of this line."
  (interactive)
  (end-of-line)
  (setq lyskom-keyboard-menu-selection
        (if (get-text-property (point) 'lyskom-keyboard-menu-item)
            (get-text-property (point) 'lyskom-keyboard-menu-item)
          (let ((pos (previous-single-property-change 
                      (point) 'lyskom-keyboard-menu-item
                      nil (1- (save-excursion (beginning-of-line) (point))))))
            (when (and pos (not (zerop pos)))
              (or (get-text-property pos 'lyskom-keyboard-menu-item)
                  (get-text-property (1- pos)  'lyskom-keyboard-menu-item)))))))
