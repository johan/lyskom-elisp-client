;;; widget.el --- a library of user interface components.
;;
;; Copyright (C) 1996, 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, extensions, faces, hypermedia
;; Version: 1.9941
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; If you want to use this code, please visit the URL above.
;;
;; This file only contain the code needed to define new widget types.
;; Everything else is autoloaded from `wid-edit.el'.

;;; Code:

(eval-when-compile (require 'cl))

(defmacro define-widget-keywords (&rest keys)
  (`
   (eval-and-compile
     (let ((keywords (quote (, keys))))
       (while keywords
	 (or (boundp (car keywords))
	     (set (car keywords) (car keywords)))
	 (setq keywords (cdr keywords)))))))

(define-widget-keywords :match-alternatives :documentation-indent
  :complete-function :complete :button-overlay
  :field-overlay  
  :documentation-shown :button-prefix  
  :button-suffix :mouse-down-action :glyph-up :glyph-down :glyph-inactive  
  :prompt-internal :prompt-history :prompt-match
  :prompt-value  :deactivate :active  
  :inactive :activate :sibling-args :delete-button-args
  :insert-button-args :append-button-args :button-args 
  :tag-glyph :off-glyph :on-glyph :valid-regexp
  :secret :sample-face :sample-face-get :case-fold 
  :create :convert-widget :format :value-create :offset :extra-offset
  :tag :doc :from :to :args :value :action
  :value-set :value-delete :match :parent :delete :menu-tag-get
  :value-get :choice :void :menu-tag :on :off :on-type :off-type
  :notify :entry-format :button :children :buttons :insert-before
  :delete-at :format-handler :widget :value-pos :value-to-internal
  :indent :size :value-to-external :validate :error :directory
  :must-match :type-error :value-inline :inline :match-inline :greedy
  :button-face-get :button-face :value-face :keymap :entry-from
  :entry-to :help-echo :documentation-property :tab-order) 

;; These autoloads should be deleted when the file is added to Emacs.
(unless (fboundp 'load-gc)
  (autoload 'widget-apply "wid-edit")
  (autoload 'widget-create "wid-edit")
  (autoload 'widget-insert "wid-edit")
  (autoload 'widget-prompt-value "wid-edit")
  (autoload 'widget-browse "wid-browse" nil t)
  (autoload 'widget-browse-other-window "wid-browse" nil t)
  (autoload 'widget-browse-at "wid-browse" nil t)
  (autoload 'widget-minor-mode "wid-browse" nil t))

(defun define-widget (name class doc &rest args)
  "Define a new widget type named NAME from CLASS.

NAME and CLASS should both be symbols, CLASS should be one of the
existing widget types, or nil to create the widget from scratch.

After the new widget has been defined, the following two calls will
create identical widgets:

* (widget-create NAME)

* (apply 'widget-create CLASS ARGS)

The third argument DOC is a documentation string for the widget."
  (put name 'widget-type (cons class args))
  (put name 'widget-documentation doc)
  name)

;;; The End.

(provide 'widget)

;; widget.el ends here
