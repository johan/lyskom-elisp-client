;;; wid-browse.el --- Functions for browsing widgets.
;;
;; Copyright (C) 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: extensions
;; Version: 1.56
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;;; Commentary:
;;
;; Widget browser.  See `widget.el'.

;;; Code:

(require 'easymenu)
(require 'custom)
(require 'wid-edit)
(require 'cl)

(defgroup widget-browse nil
  "Customization support for browsing widgets."
  :group 'widgets)

;;; The Mode.

(defvar widget-browse-mode-map nil
  "Keymap for `widget-browse-mode'.")
  
(unless widget-browse-mode-map
  (setq widget-browse-mode-map (make-sparse-keymap))
  (set-keymap-parent widget-browse-mode-map widget-keymap))

(easy-menu-define widget-browse-mode-menu 
    widget-browse-mode-map
  "Menu used in widget browser buffers."
  '("Widget"
    ["Browse" widget-browse t]
    ["Browse At" widget-browse-at t]))

(defcustom widget-browse-mode-hook nil
  "Hook called when entering widget-browse-mode."
  :type 'hook
  :group 'widget-browse)

(defun widget-browse-mode ()
  "Major mode for widget browser buffers.

The following commands are available:

\\[widget-forward]		Move to next button or editable field.
\\[widget-backward]		Move to previous button or editable field.
\\[widget-button-click]		Activate button under the mouse pointer.
\\[widget-button-press]		Activate button under point.

Entry to this mode calls the value of `widget-browse-mode-hook'
if that value is non-nil."
  (kill-all-local-variables)
  (setq major-mode 'widget-browse-mode
	mode-name "Widget")
  (use-local-map widget-browse-mode-map)
  (easy-menu-add widget-browse-mode-menu)
  (run-hooks 'widget-browse-mode-hook))

;;; Commands.

;;;###autoload
(defun widget-browse-at (pos)
  "Browse the widget under point."
  (interactive "d")
  (let* ((field (get-text-property pos 'field))
	 (button (get-text-property pos 'button))
	 (doc (get-text-property pos 'widget-doc))
	 (text (cond (field "This is an editable text area.")
		     (button "This is an active area.")
		     (doc "This is documentation text.")
		     (t "This is unidentified text.")))
	 (widget (or field button doc)))
    (when widget
      (widget-browse widget))
    (message text)))

(defvar widget-browse-history nil)

(defun widget-browse (widget)
  "Create a widget browser for WIDGET."
  (interactive (list (completing-read "Widget: " 
				      obarray
				      (lambda (symbol)
					(get symbol 'widget-type))
				      t nil 'widget-browse-history)))
  (if (stringp widget)
      (setq widget (intern widget)))
  (unless (if (symbolp widget)
	      (get widget 'widget-type)
	    (and (consp widget)
		 (get (widget-type widget) 'widget-type)))
    (error "Not a widget."))
  ;; Create the buffer.
  (if (symbolp widget)
      (let ((buffer (format "*Browse %s Widget*" widget)))
	(kill-buffer (get-buffer-create buffer))
	(switch-to-buffer (get-buffer-create buffer)))
    (kill-buffer (get-buffer-create "*Browse Widget*"))
    (switch-to-buffer (get-buffer-create "*Browse Widget*")))
  (widget-browse-mode)
  
  ;; Quick way to get out.
  (widget-create 'push-button
		 :action (lambda (widget &optional event)
			   (bury-buffer))
		 "Quit")
  (widget-insert "\n")

  ;; Top text indicating whether it is a class or object browser.
  (if (listp widget)
      (widget-insert "Widget object browser.\n\nClass: ")
    (widget-insert "Widget class browser.\n\n")
    (widget-create 'widget-browse
		   :format "%[%v%]\n%d"
		   :doc (get widget 'widget-documentation)
		   widget)
    (unless (eq (preceding-char) ?\n)
      (widget-insert "\n"))
    (widget-insert "\nSuper: ")
    (setq widget (get widget 'widget-type)))

  ;; Now show the attributes.
  (let ((name (car widget))
	(items (cdr widget))
	key value printer)
    (widget-create 'widget-browse
		   :format "%[%v%]"
		   name)
    (widget-insert "\n")
    (while items
      (setq key (nth 0 items)
	    value (nth 1 items)
	    printer (or (get key 'widget-keyword-printer)
			'widget-browse-sexp)
	    items (cdr (cdr items)))
      (widget-insert "\n" (symbol-name key) "\n\t")
      (funcall printer widget key value)
      (widget-insert "\n")))
  (widget-setup)
  (goto-char (point-min)))

;;; The `widget-browse' Widget.

(define-widget 'widget-browse 'push-button
  "Button for creating a widget browser.
The :value of the widget shuld be the widget to be browsed."
  :format "%[[%v]%]"
  :value-create 'widget-browse-value-create
  :action 'widget-browse-action)

(defun widget-browse-action (widget &optional event)
  ;; Create widget browser for WIDGET's :value. 
  (widget-browse (widget-get widget :value)))

(defun widget-browse-value-create (widget)
  ;; Insert type name.
  (let ((value (widget-get widget :value)))
    (cond ((symbolp value)
	   (insert (symbol-name value)))
	  ((consp value)
	   (insert (symbol-name (widget-type value))))
	  (t
	   (insert "strange")))))

;;; Keyword Printer Functions.

(defun widget-browse-widget (widget key value)
  "Insert description of WIDGET's KEY VALUE.
VALUE is assumed to be a widget."
  (widget-create 'widget-browse value))

(defun widget-browse-widgets (widget key value)
  "Insert description of WIDGET's KEY VALUE.
VALUE is assumed to be a list of widgets."
  (while value
    (widget-create 'widget-browse
		   (car value))
    (setq value (cdr value))
    (when value
      (widget-insert " "))))

(defun widget-browse-sexp (widget key value)
  "Insert description of WIDGET's KEY VALUE.
Nothing is assumed about value."
  (let ((pp (condition-case signal
		(pp-to-string value)
	      (error (prin1-to-string signal)))))
    (when (string-match "\n\\'" pp)
      (setq pp (substring pp 0 (1- (length pp)))))
    (if (cond ((string-match "\n" pp)
	       nil)
	      ((> (length pp) (- (window-width) (current-column)))
	       nil)
	      (t t))
	(widget-insert pp)
      (widget-create 'push-button
		     :tag "show"
		     :action (lambda (widget &optional event)
			       (with-output-to-temp-buffer
				   "*Pp Eval Output*"
				 (princ (widget-get widget :value))))
		     pp))))

(defun widget-browse-sexps (widget key value)
  "Insert description of WIDGET's KEY VALUE.
VALUE is assumed to be a list of widgets."
  (let ((target (current-column)))
    (while value
      (widget-browse-sexp widget key (car value))
      (setq value (cdr value))
      (when value
	(widget-insert "\n" (make-string target ?\ ))))))

;;; Keyword Printers.

(put :parent 'widget-keyword-printer 'widget-browse-widget)
(put :children 'widget-keyword-printer 'widget-browse-widgets)
(put :buttons 'widget-keyword-printer 'widget-browse-widgets)
(put :button 'widget-keyword-printer 'widget-browse-widget)
(put :args 'widget-keyword-printer 'widget-browse-sexps)

;;; The End:

(provide 'wid-browse)

;; wid-browse.el ends here
