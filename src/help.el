;;;;; -*-coding: iso-8859-1;-*-
;;;;;
;;;;; $Id: help.el,v 44.2 2002-05-26 23:15:53 byers Exp $
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
;;;; File: help.el
;;;;
;;;; Functions for formatting help strings in the client
;;;;


(setq lyskom-clientversion-long 
      (concat lyskom-clientversion-long
	      "$Id: help.el,v 44.2 2002-05-26 23:15:53 byers Exp $\n"))



(defgroup lyskom-faces nil
  "Faces for use in the LysKOM elisp client."
  :group 'lyskom)

(defgroup lyskom nil
  "Faces for use in the LysKOM elisp client."
  :group 'default)

(defface lyskom-help-h1-face '((t (:weight bold :underline t :height 2.0 :family "helvetica")))
  "First level header in LysKOM help."
  :group 'lyskom-faces)

(defface lyskom-help-h2-face '((t (:weight bold :underline t :height 1.5 :family "helvetica")))
  "Second level header in LysKOM help."
  :group 'lyskom-faces)

(defface lyskom-help-h3-face '((t (:weight bold :family "helvetica")))
  "Third level header in LysKOM help."
  :group 'lyskom-faces)

;;; Note that these are duplicated in help-compile.el. Change one and change both!

(defsubst lyskom-help-create-data (tag attr data)
  (list tag attr data))

(defsubst lyskom-help-data-get-tag (data)
  (elt data 0))

(defsubst lyskom-help-data-get-attr (attr data)
  (cdr (assq attr (elt data 1))))

(defsubst lyskom-help-data-get-attrs (data)
  (elt data 1))

(defsubst lyskom-help-data-get-data (data)
  (elt data 2))


(defvar lyskom-help-format-handlers
  '((h1 . lyskom-help-format-h1)
    (h2 . lyskom-help-format-h2)
    (h3 . lyskom-help-format-h3)
    (p  . lyskom-help-format-p)
    (b  . lyskom-help-format-b)
    (i  . lyskom-help-format-i)
    (list . lyskom-help-format-list)
    (item . lyskom-help-format-item)
    (inline . lyskom-help-format-inline)
    (refer . lyskom-help-format-refer)
    (cref . lyskom-help-format-cref)
    (TEXT . lyskom-help-format-TEXT))
  )

(defun lyskom-help-get-section (section)
  (elt (assq section lyskom-help-data) 2))

(defun lyskom-help-format-section (section)
  "Format and insert section SECTION."
  (lyskom-do-help-format (lyskom-help-get-section section)))

(defun lyskom-do-help-format (data)
  "Format ant insert help data DATA."
  (let ((inhibit-read-only t))
    (cond ((symbolp (car data))
           (funcall (cdr (assq (car data) lyskom-help-format-handlers)) data))
          (t (lyskom-traverse el data
                              (let ((tag (lyskom-help-data-get-tag el))
                                    (data (lyskom-help-data-get-data el)))
                                (funcall (cdr (assq tag lyskom-help-format-handlers))
                                         el)))))))

(defun lyskom-help-format-text-properties (data props)
  (let ((start (point-marker)))
    (lyskom-traverse el (lyskom-help-data-get-data data)
      (lyskom-do-help-format el))
    (add-text-properties start (point) props)
    (set-marker start nil)))

(defun lyskom-help-format-h1 (data)
  (lyskom-insert "\n\n")
  (lyskom-help-format-text-properties data '(face lyskom-help-h1-face))
  (lyskom-insert "\n\n"))

(defun lyskom-help-format-h2 (data)
  (lyskom-insert "\n")
  (lyskom-help-format-text-properties data '(face lyskom-help-h2-face))
  (lyskom-insert "\n\n"))

(defun lyskom-help-format-h3 (data)
  (lyskom-help-format-text-properties data '(face lyskom-help-h3-face))
  (lyskom-insert "\n"))

(defun lyskom-help-format-i (data)
  (lyskom-help-format-text-properties data '(face italic)))

(defun lyskom-help-format-b (data)
  (lyskom-help-format-text-properties data '(face bold)))

(defun lyskom-help-format-TEXT (data)
  (lyskom-insert (cdr data)))

(defun lyskom-help-format-list (data)
  (lyskom-insert "\n")
  (lyskom-traverse el data (lyskom-do-help-format el))
  (lyskom-insert "\n"))

(defun lyskom-help-format-item (data)
  (lyskom-insert "  * ")
  (lyskom-traverse el data (lyskom-do-help-format el))
  (lyskom-insert "\n"))

(defun lyskom-help-format-refer (data)
  (let* ((id (intern (lyskom-help-data-get-attr 'id data)))
         (section (lyskom-help-get-section id)))
    (cond (section (lyskom-insert 
                    (lyskom-help-data-get-attr 'prompt section)))
          (t (lyskom-insert (format "%s" id))))))

(defun lyskom-help-format-cref (data)
  (let* ((command (intern (lyskom-help-data-get-attr 'id data)))
         (command-name (lyskom-command-name command))
         (keys (delq nil
                     (mapcar (lambda (x)
                               (if (and (arrayp x)
                                        (eq (elt x 0) 'menu-bar))
                                   nil
                                 x))
                             (where-is-internal command))))
         (heading (lyskom-format "%#1@%[%#2s%]%#3?b%[ (%#3s)%]%[%]"
                                 '(face italic)
                                 command-name
                                 (and keys 
                                      (mapconcat 'key-description
                                                 keys
                                                 "; ")))))
    (lyskom-insert heading)))

(defun lyskom-help-format-inline (data)
  (let* ((id (intern (lyskom-help-data-get-attr 'id data)))
         (section (lyskom-help-get-section id)))
    (cond (section (lyskom-do-help-format section))
          (t (lyskom-insert (format "[%S]" id))))))

(defun lyskom-help-format-p (data)
  (let ((start (point-marker)))
    (lyskom-traverse el (lyskom-help-data-get-data data)
      (lyskom-do-help-format el))
      (save-excursion
        (save-restriction
          (narrow-to-region start (point))
          (goto-char (point-min))
          (while (re-search-forward "^\\s-+" nil t)
            (replace-match "" nil nil))
          (lyskom-fill-region (point-min) (point-max))))
      (lyskom-insert "\n\n")))
