;;;;; -*-unibyte: t;-*-
;;;;;
;;;;; $Id: slow.el,v 44.5.4.1 1999-10-13 09:56:15 byers Exp $
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
;;;; File: slow.el
;;;;
;;;; This file contains the code that makes it possible to run a 
;;;; long-commands mode in the lyskom-buffer.
;;;;
;;;; ================================================================
;;;;

;;; Author: Linus Tolke
;;; Modified by: David Kågedal

(defvar lyskom-slow-mode-map
  (make-sparse-keymap)
  "Mode map for the `slow' lyskom command mode.")

(define-key lyskom-slow-mode-map "\r" 'kom-parse-command-and-execute)
(define-key lyskom-slow-mode-map "\t" 'kom-expand-slow-command)
(define-key lyskom-slow-mode-map " " 'kom-expand-slow-or-next-command)

(defun lyskom-get-entered-slow-command ()
  "Get the text that the user has entered after the last prompt.
Note that this function leaves point at the end of the prompt.

If no text is entered, nil is returned."
  (goto-char (point-max))
  (save-restriction
    (when (> lyskom-last-viewed (point-max))
      (setq lyskom-last-viewed (point-max)))
    (narrow-to-region lyskom-last-viewed (point-max))
    (if (search-backward lyskom-current-prompt-text nil t)
        (forward-char (length lyskom-current-prompt-text))
      (goto-char (point-max))
      (beginning-of-line))
    (when (looking-at "\\(\\s-+\\)")
      (goto-char (match-end 0))))
  (if (= (point) (point-max))
      nil
    (buffer-substring (point) (point-max))))

(defun kom-expand-slow-command ()
  "Tries to complete the command at point.
If the completion was exact return a pair `(COMMAND . POINT)' where
COMMAND is the command and POINT is the point where the command text
starts.

If the completion was not exact it returns nil."
  (interactive)
  (let* ((text (lyskom-get-entered-slow-command))
	 (completion-ignore-case t)
	 (alternatives (mapcar (function (lambda (pair)
					   (cons (cdr pair) (car pair))))
			       (lyskom-get-strings
				lyskom-commands 'lyskom-command)))
	 (completes (and text (all-completions text alternatives)))
	 (command nil))
    (cond
     ((null text)
      (lyskom-beep t))
     ((null completes)
      (lyskom-insert-before-prompt (lyskom-get-string 'no-such-command))
      (lyskom-beep t))
     ((= (length completes) 1)
      (setq command (cons (cdr (assq (car completes) alternatives))
			  (point)))
      (delete-region (point) (point-max))
      (insert (car completes)))
     ((> (length completes) 1)
      (let ((longest (try-completion text alternatives)))
	(cond
	 ((eq longest 't)
	  (delete-region (point) (point-max))
	  (insert (car completes)))
	 ((stringp longest)
	  (if (string= (upcase longest) (upcase text))
	      (lyskom-format-insert-before-prompt
	       'command-completions
	       (mapconcat 'identity completes "\n ")))
	  (delete-region (point) (point-max))
	  (insert longest))
	 (t (signal 'lyskom-internal-error '()))))))
    command))


(defun kom-expand-slow-or-next-command ()
  "If any part of a slow command has been entered, call
`kom-expand-slow-command'. Otherwise, do `kom-next-command'."
  (interactive)
  (if (lyskom-get-entered-slow-command)
      (kom-expand-slow-command)
    (buffer-disable-undo)
    (kom-next-command)))


(defun kom-parse-command-and-execute ()
  "Reads a command from the last line in the buffer and executes it."
  (interactive)
  (let* ((text (lyskom-get-entered-slow-command))
	 (command (and text (kom-expand-slow-command))))
    (buffer-disable-undo)
    (cond
     ((null text)
      (call-interactively 'kom-next-command))
     (command
      (delete-region (cdr command) (point-max))
      (call-interactively (car command))))))


(defun kom-slow-mode ()
  "Starts the slow-command-mode."
  (interactive)
  (lyskom-start-of-command 'kom-slow-mode)
  (unless lyskom-slow-mode
    (setq lyskom-saved-read-only buffer-read-only)
    (setq lyskom-slow-mode t)
    (setq buffer-read-only nil)
    (use-local-map lyskom-slow-mode-map))
  (lyskom-end-of-command))

(defun kom-quick-mode ()
  "Starts the quick-command-mode."
  (interactive)
  (lyskom-start-of-command 'kom-quick-mode)
  (when lyskom-slow-mode
    (setq buffer-read-only lyskom-saved-read-only)
    (setq lyskom-slow-mode nil)
    (use-local-map lyskom-mode-map))
  (lyskom-end-of-command))


