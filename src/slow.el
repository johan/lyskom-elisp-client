;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: slow.el,v 44.10 2000-08-31 12:29:50 byers Exp $
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

(define-key lyskom-slow-mode-map (kbd "RET") 'kom-parse-command-and-execute)
(define-key lyskom-slow-mode-map (kbd "TAB") 'kom-expand-slow-command)
(define-key lyskom-slow-mode-map (kbd "SPC") 'kom-expand-slow-or-next-command)
(define-key lyskom-slow-mode-map (kbd "C-a") 'kom-slow-start-of-line)
(define-key lyskom-slow-mode-map (kbd "?")   'kom-slow-list-completions)

(defun lyskom-slow-start-of-line ()
  "Move point to start of command, after LysKOM prompt."
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
      (goto-char (match-end 0)))))


(defun lyskom-get-entered-slow-command ()
  "Get the text that the user has entered after the last prompt.
Note that this function leaves point at the end of the prompt.

If no text is entered, nil is returned."
  (lyskom-slow-start-of-line)
  (if (= (point) (point-max))
      nil
    (buffer-substring (point) (point-max))))

(defun kom-slow-start-of-line ()
  (interactive)
  (lyskom-slow-start-of-line))

(defun lyskom-expand-slow-command (try-exact eager-completion)
  (let* ((saved-point (point-marker))
         (text (lyskom-get-entered-slow-command))
         (exact (and text try-exact (lyskom-complete-command text nil 'lambda)))
         (completes (and text (all-completions text 'lyskom-complete-command)))
	 (command nil))
    (cond
     ((null text)
      (lyskom-beep t))
     ((null completes)
      (goto-char saved-point)
      (lyskom-insert-before-prompt (lyskom-get-string 'no-such-command))
      (lyskom-beep t))
     (exact
      (setq command (cons (lyskom-lookup-command-by-name text) (point))))
     ((= (length completes) 1)
      (setq command (cons (lyskom-lookup-command-by-name text) (point)))
      (delete-region (point) (point-max))
      (insert (car completes)))
     ((> (length completes) 1)
      (let ((longest (try-completion text 'lyskom-complete-command)))
	(cond
	 ((eq longest 't)
	  (delete-region (point) (point-max))
	  (insert (car completes)))
	 ((stringp longest)
	  (delete-region (point) (point-max))
	  (insert longest)

          ;; If the completion is the same as the entered string
          ;; then we either add a space if there was none at the
          ;; end or list all possible completions *and* add a 
          ;; space (since it has been killed).

          (let* ((have-space (string-match "\\s-+$" text))
                 (text (if have-space (substring text 0 (match-beginning 0)) text))
                 (longest (if (string-match "\\s-+$" longest)
                              (substring longest 0 (match-beginning 0))
                            longest)))
	  (when (lyskom-string= (lyskom-unicase longest) (lyskom-unicase text))
            (if (or have-space eager-completion)
                (lyskom-format-insert-before-prompt
                 'command-completions
                 (mapconcat 'identity completes "\n ")))
            (unless (or eager-completion (eq ?\  (char-before (point))) (insert " "))))
          ))
	 (t (signal 'lyskom-internal-error '()))))))
    command))

(defun kom-expand-slow-command (&optional try-exact)
  "Tries to complete the command at point.
If optional TRY-EXACT is non-nil, look for an exact match.

If the completion was exact return a pair `(COMMAND . POINT)' where
COMMAND is the command and POINT is the point where the command text
starts.

If the completion was not exact it returns nil."
  (interactive)
  (lyskom-expand-slow-command try-exact t))

(defun kom-expand-slow-or-next-command (&optional try-exact)
  "If any part of a slow command has been entered, call
`kom-expand-slow-command'. Otherwise, do `kom-next-command'."
  (interactive)
  (if (save-excursion (lyskom-get-entered-slow-command))
      (lyskom-expand-slow-command try-exact nil)
    (buffer-disable-undo)
    (kom-next-command)))

(defun kom-parse-command-and-execute ()
  "Reads a command from the last line in the buffer and executes it."
  (interactive)
  (let* ((text (lyskom-get-entered-slow-command))
	 (command (and text (kom-expand-slow-command t))))
    (buffer-disable-undo)
    (cond
     ((null text)
      (call-interactively 'kom-next-command))
     (command
      (delete-region (cdr command) (point-max))
      (call-interactively (car command))))))

(defun kom-slow-list-completions ()
  (interactive)
  (save-excursion
    (let* ((text (lyskom-get-entered-slow-command))
           (completes (and text (all-completions text 'lyskom-complete-command))))
      (cond (completes (lyskom-format-insert-before-prompt
                        'command-completions
                        (mapconcat 'identity completes "\n ")))
            (t (lyskom-insert-before-prompt (lyskom-get-string 'no-such-command)))))))


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


