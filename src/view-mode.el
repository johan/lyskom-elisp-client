;;;;;
;;;;; $Id: view-mode.el,v 44.1 1997-02-07 18:09:19 byers Exp $
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
	      "$Id: view-mode.el,v 44.1 1997-02-07 18:09:19 byers Exp $\n"))

(defun lyskom-view-base-mode ()
  (if (fboundp 'view-major-mode)
      (view-major-mode)
    (view-mode)))

(define-derived-mode lyskom-view-mode 
  lyskom-view-base-mode
  "LysKOM View"
  "Major mode for viewing buffers")


(defvar lyskom-view-mode-map nil
  "Keymap for LysKOM view mode")

(if lyskom-view-mode-map
    nil
  (setq lyskom-view-mode-map (make-sparse-keymap))
  (define-key lyskom-view-mode-map "*" 'kom-button-press)
  (define-key lyskom-view-mode-map "\t" 'kom-next-link)
  (define-key lyskom-view-mode-map [(meta tab)] 'kom-previous-link)
  (define-key lyskom-view-mode-map (lyskom-keys [mouse-2]) 'kom-button-click)
  (define-key lyskom-view-mode-map (lyskom-keys [mouse-3]) 'kom-popup-menu)
  (define-key lyskom-view-mode-map "q" 'lyskom-view-mode-quit))

(defun lyskom-view-mode-quit ()
  (interactive)
  (let ((buf (current-buffer)))
    (lyskom-undisplay-buffer buf)
    (kill-buffer buf)))
