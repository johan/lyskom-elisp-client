;;;;; -*-coding: raw-text;-*-
;;;;;
;;;;; $Id: view-mode.el,v 44.6 1999-06-10 13:36:29 byers Exp $
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
;;;; File: view-mode.el
;;;;
;;;; LysKOM view mode, a simple move for viewing buffers.
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: view-mode.el,v 44.6 1999-06-10 13:36:29 byers Exp $\n"))

(defvar lyskom-view-mode-map nil
  "Keymap for LysKOM view mode")

(eval-when-compile (defvar view-mode-map nil))

(lyskom-external-function view-major-mode)

(defun lyskom-view-base-mode ()
  (cond ((fboundp 'view-major-mode) (view-major-mode))
        ((assq 'view-mode minor-mode-alist)
         (let* ((keymap (copy-keymap lyskom-view-mode-map)))
           (make-variable-buffer-local 'minor-mode-map-alist)
           (set-keymap-parent keymap view-mode-map)
           (setq minor-mode-map-alist (cons (cons 'view-mode keymap)
                                            minor-mode-map-alist))
           (view-mode)))
        (t (view-mode))))


(if lyskom-view-mode-map
    nil
  (setq lyskom-view-mode-map (make-sparse-keymap))
  (define-key lyskom-view-mode-map "*" 'kom-button-press)
  (define-key lyskom-view-mode-map "+" 'kom-menu-button-press)
  (define-key lyskom-view-mode-map "\t" 'kom-next-link)
  (define-key lyskom-view-mode-map [(meta tab)] 'kom-previous-link)
  (define-key lyskom-view-mode-map (kbd (lyskom-keys 'button2up)) 'kom-button-click)
  (define-key lyskom-view-mode-map (kbd (lyskom-keys 'button3)) 'kom-popup-menu)
  (define-key lyskom-view-mode-map "q" 'lyskom-view-mode-quit))

(defun lyskom-view-mode-quit ()
  (interactive)
  (let ((buf (current-buffer)))
    (lyskom-undisplay-buffer buf)
    (kill-buffer buf)))

(define-derived-mode lyskom-view-mode 
  lyskom-view-base-mode
  "LysKOM View"
  "Major mode for viewing buffers")

